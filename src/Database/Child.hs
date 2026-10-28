{-# LANGUAGE OverloadedStrings #-}

{- | A database divided by a system model, built from a parent held in memory
rather than from the files.

A parent keeps its processes of several products whole, every product row
priced. Its child divides them under its model. Building the child from the
parent as it stands, rather than from the files, is what carries the parent's
edits over: a price changed on the parent changes the child's shares.
-}
module Database.Child (
    childOf,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Database.Allocation (Allocating (..), allocate, describePropertyRefusal, propertyKeyRefusals)
import Database.Loader (getReferenceProductUUID, linkWithinDatabaseByName)
import Database.Rebuild (rebuildFromActivities)
import Database.SystemModel (Resolution (..), Substituted (..), buys, resolveModel, severalProducts)
import Progress (ProgressLevel (..), reportProgress)
import Types (
    Activity (..),
    AllocationKey (..),
    AllocationProperty,
    Database (..),
    Exchange (..),
    ExchangeFormula (..),
    ModelException (..),
    ModelSubstitution (..),
    Parameter (..),
    ParameterDefinition (..),
    ProcessRef (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    SystemModel,
    TechRole (..),
    exchangeAmount,
    exchangeFlowId,
    exchangeFormula,
    exchangeIsProductOutput,
    exchangeUnitId,
    getUnitNameForExchange,
    noProperties,
    propertyName,
    ratioValue,
    readExchangeLocation,
    reasonText,
    scaleExchange,
    toSimpleDatabase,
 )
import UnitConversion (UnitConfig, convertUnit)

{- | The parent's processes of several products divided by the model, each
block under its own property, each piece keyed as a load keys it: the block's
activity, the piece's product.

Only those processes are divided again. A process of one product was already
cut to its declared share when the parent was read, and dividing it a second
time would apply that share twice.

A coproduct the model substitutes leaves its block before the block is
divided: the products left carry the whole block, credited with what the
coproduct displaces, and a line that bought the coproduct buys what it
displaces.

A block its property cannot weigh stays whole and loses its column, and is
named. An exception that can weigh none of the blocks it takes is refused: the
child would carry its reason and nothing it did.

An input the parent linked to a coproduct of a block it kept whole already
names the piece producing it: the parent links it under the block's activity
and the coproduct, the key the piece is given here. The pieces are still
linked by name among themselves, for an input the parent left unlinked. An
input the parent already linked keeps its supplier.
-}
childOf :: UnitConfig -> SystemModel -> Database -> IO (Either Text Database)
childOf unitConfig model parent = case resolveModel model (sdbActivities simple) of
    Left problems -> pure (Left (T.intercalate "; " (NE.toList problems)))
    Right resolution -> case M.traverseWithKey (substitute (rsSubstitutions resolution)) (sdbActivities simple) of
        Left refusal -> pure (Left refusal)
        Right processes ->
            let whys = refused resolution processes
             in case divide resolution processes whys of
                    Left refusal -> pure (Left refusal)
                    Right pieces -> do
                        reportRefusals whys
                        linked <- linkWithinDatabaseByName unitConfig simple{sdbActivities = pieces}
                        pure (rebuildFromActivities unitConfig parent (sdbActivities linked))
  where
    simple :: SimpleDatabase
    simple = toSimpleDatabase parent

    -- The parent's blocks: a substituted one is divided still, down to one product.
    blocks :: S.Set (UUID, UUID)
    blocks = M.keysSet (M.filter severalProducts (sdbActivities simple))

    allocating :: AllocationProperty -> Allocating
    allocating prop = Allocating{alKey = ByProperty prop, alUnitConfig = unitConfig, alUnitDB = sdbUnits simple}

    {- A process with the substitutions applied: a coproduct of its block they
    take becomes the product it displaces, avoided, and a line buying that
    coproduct buys the displaced product. A product left alone in its block
    loses its declared share, its part of a division that no longer happens. -}
    substitute :: [Substituted] -> (UUID, UUID) -> Activity -> Either Text Activity
    substitute sds k@(activityId, _) act = alone <$> traverse line (exchanges act)
      where
        taken :: M.Map UUID Substituted
        taken = M.fromList [(prProduct (subCoproduct (sdRule sd)), sd) | S.member k blocks, sd <- sds, prActivity (subCoproduct (sdRule sd)) == activityId]

        line :: Exchange -> Either Text Exchange
        line ex = case (M.lookup (exchangeFlowId ex) taken, filter (\sd -> buys (subCoproduct (sdRule sd)) ex) sds) of
            (Just sd, _) | exchangeIsProductOutput ex -> Right (avoided sd ex)
            (_, sd : _) -> rerouted act sd ex
            _ -> Right ex

        alone :: [Exchange] -> Activity
        alone lines' =
            act
                { exchanges = if not (M.null taken) && length (filter exchangeIsProductOutput lines') == 1 then map unshared lines' else lines'
                , activityParameters = M.union (activityParameters act) named
                }

        -- A name a substituted row carried, which other formulas of the block read: it keeps the amount the parent stated.
        named :: M.Map Text Parameter
        named =
            M.fromList
                [ (name, Parameter{parameterDefinition = CalculatedParameter text (Just (exchangeAmount ex)), parameterUnit = Just (unitOf ex), parameterDescription = ""})
                | ex <- exchanges act
                , exchangeIsProductOutput ex
                , M.member (exchangeFlowId ex) taken
                , Just ExchangeFormula{efName = Just name, efText = text} <- [exchangeFormula ex]
                ]

        unshared :: Exchange -> Exchange
        unshared ex = case ex of
            TechnosphereExchange{} | exchangeIsProductOutput ex -> ex{techShare = Nothing}
            _ -> ex

    {- The coproduct's row, now the displaced product avoided. Its formula
    loses its name, which the block would otherwise publish over an amount of
    the displaced product: the name stays a parameter of the block. -}
    avoided :: Substituted -> Exchange -> Exchange
    avoided sd ex = case displaced sd (scaleExchange (ratio sd) ex) of
        credited@TechnosphereExchange{} ->
            credited
                { techRole = AvoidedProduct
                , techComment = Just (reasonText (subReason (sdRule sd)))
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                , techFormula = (\f -> f{efName = Nothing}) <$> techFormula credited
                }
        other -> other

    -- A line buying the coproduct, now buying what it displaces: converted to the coproduct's unit first, then by the ratio.
    rerouted :: Activity -> Substituted -> Exchange -> Either Text Exchange
    rerouted act sd ex = case factor of
        Just f -> Right (displaced sd (scaleExchange (f * ratio sd) ex))
        Nothing ->
            Left $
                activityName act
                    <> " buys "
                    <> activityName (sdBlock sd)
                    <> " in "
                    <> unitOf ex
                    <> ", which does not convert to the "
                    <> unitOf (sdCoproduct sd)
                    <> " the substitution \""
                    <> reasonText (subReason (sdRule sd))
                    <> "\" counts in"
      where
        factor
            | exchangeUnitId ex == exchangeUnitId (sdCoproduct sd) = Just 1
            | otherwise = convertUnit unitConfig (unitOf ex) (unitOf (sdCoproduct sd)) 1

    -- A line bought from, or credited against, the displaced process.
    displaced :: Substituted -> Exchange -> Exchange
    displaced sd ex = case ex of
        TechnosphereExchange{} ->
            ex
                { techFlowId = exchangeFlowId (sdReplacedProduct sd)
                , techUnitId = exchangeUnitId (sdReplacedProduct sd)
                , techActivityLinkId = Just replacing
                , techSupplierClaim = ClaimById replacing
                , techLocation = readExchangeLocation (activityLocation (sdReplaced sd))
                }
        _ -> ex
      where
        replacing = prActivity (subReplaces (sdRule sd))

    ratio :: Substituted -> Double
    ratio = ratioValue . subRatio . sdRule

    unitOf :: Exchange -> Text
    unitOf = getUnitNameForExchange (sdbUnits simple)

    divide :: Resolution -> M.Map (UUID, UUID) Activity -> [(UUID, Text)] -> Either Text (M.Map (UUID, UUID) Activity)
    divide resolution processes whys = do
        mapM_ (refuseIdle (M.fromList whys)) (rsTaken resolution)
        mapM_ (refuseWhole (M.fromList whys)) (rsSubstitutions resolution)
        keyed
            [ keyedPiece
            | (k@(activityId, _), act) <- M.toList processes
            , keyedPiece <- case M.lookup activityId (rsKeys resolution) of
                -- Keyed by activity alone: a sibling of one product under the
                -- same activity must not be cut a second time.
                Just prop | S.member k blocks -> [((activityId, getReferenceProductUUID piece), piece) | piece <- NE.toList (allocate (allocating prop) act)]
                _ -> [(k, act)]
            ]

    -- Each block its property could not weigh, and why. A block substituted down to one product has nothing to weigh.
    refused :: Resolution -> M.Map (UUID, UUID) Activity -> [(UUID, Text)]
    refused resolution processes =
        [ (activityId, name <> " has no column -- " <> describePropertyRefusal prop reason)
        | ((activityId, _), act) <- M.toList (M.restrictKeys processes blocks)
        , severalProducts act
        , Just prop <- [M.lookup activityId (rsKeys resolution)]
        , (name, reason) <- propertyKeyRefusals (allocating prop) [act]
        ]

    refuseIdle :: M.Map UUID Text -> (ModelException, [UUID]) -> Either Text ()
    refuseIdle whys (ex, taken)
        | all (`M.member` whys) taken =
            Left $
                "the "
                    <> propertyName (exKey ex)
                    <> " exception \""
                    <> reasonText (exReason ex)
                    <> "\" divides none of the "
                    <> T.pack (show (S.size (S.fromList taken)))
                    <> " blocks it takes: "
                    <> T.intercalate "; " (take 3 (M.elems (M.restrictKeys whys (S.fromList taken))))
        | otherwise = Right ()

    -- A block left whole has no column: a credit against it would be nothing, and say nothing.
    refuseWhole :: M.Map UUID Text -> Substituted -> Either Text ()
    refuseWhole whys sd = case M.lookup (prActivity (subReplaces (sdRule sd))) whys of
        Just why -> Left ("the substitution \"" <> reasonText (subReason (sdRule sd)) <> "\" credits a block left whole: " <> why)
        Nothing -> Right ()

    -- Loud rather than silent: such a block keeps every share its source
    -- declared and still loses its column.
    reportRefusals :: [(UUID, Text)] -> IO ()
    reportRefusals whys =
        if null whys
            then pure ()
            else do
                reportProgress Warning (show (length whys) <> " blocks were not divided: their key names a property they do not carry")
                mapM_ (reportProgress Warning . T.unpack . snd) (take 10 whys)

    -- Two product rows of one block naming the same flow would key two pieces
    -- alike, and a map would keep one inventory in silence.
    keyed :: [((UUID, UUID), Activity)] -> Either Text (M.Map (UUID, UUID) Activity)
    keyed divided = case M.elems (M.filter ((> 1) . NE.length) (M.fromListWith (<>) [(k, pure act) | (k, act) <- divided])) of
        [] -> Right (M.fromList divided)
        clash : _ -> Left (activityName (NE.head clash) <> " names one product on two rows: its pieces cannot be told apart")

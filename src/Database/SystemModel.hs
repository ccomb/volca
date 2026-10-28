{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The file that says how a child is divided from its parent: a property
for every block of several products, exceptions giving some of them another,
and coproducts left out of the division for what they displace, each with its
reason.

@
name = "Agribalyse 4.0, economic allocation"
version = "1.0.0"
description = "Price everywhere, dry matter for cheese making."

[allocation]
default = "price"

[[allocation.exceptions]]
key = "dry mass"
reason = "Dry matter follows the milk, not the price."
classification = { system = "Category", value = "Agricultural\\\\Food\\\\Cheese production" }

[[substitutions]]
coproduct = "\<activity\>_\<product\>"
replaces = "\<activity\>_\<product\>"
ratio = 23.3
reason = "The biogas displaces natural gas of the same heating value."
@

A substitution names the coproduct by its block and its product, and the
process it displaces the same way, since that process must be in the parent:
a child is made from the parent and its model alone.

The file is read strictly: a key it does not know is refused, since a rule
written for a later release and skipped here would be a child its author did
not ask for.
-}
module Database.SystemModel (
    parseSystemModel,
    resolveModel,
    Resolution (..),
    Substituted (..),
    NamedSubstitution (..),
    takenProcesses,
    severalProducts,
    buys,
) where

import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import TOML (Decoder, TOMLError, Table, Value (..), decode, decodeWith, getArrayOf, getField, getFieldWith, renderTOMLError, tomlDecoder)

import Config (KeySpec (..), getFieldOpt, getFieldOptWith, unknownKeys)
import Types (
    Activity (..),
    AllocationProperty,
    ClassificationFilter (..),
    ClassificationMatch (..),
    Database,
    Exchange (..),
    ModelException (..),
    ModelSubstitution (..),
    ProcessRef (..),
    Ratio (..),
    Reason (..),
    Selector (..),
    SimpleDatabase (..),
    SystemModel (..),
    TechnosphereFlow (..),
    classificationMatches,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsInput,
    exchangeIsProductOutput,
    parseProcessRef,
    processRefText,
    propertyName,
    reasonText,
    toSimpleDatabase,
 )
import qualified Types

{- | A system model from the text of its file, or every reason it is not one.
The text is kept as written.
-}
parseSystemModel :: Text -> Either Text SystemModel
parseSystemModel text = do
    table <- either (Left . renderTOMLError) Right (decode text :: Either TOMLError Table)
    case unknownKeys modelKeys table of
        [] -> pure ()
        unknown -> Left ("the system model names keys nothing reads: " <> T.intercalate ", " unknown)
    model <- either (Left . renderTOMLError) Right (decodeWith (modelDecoder text) text)
    case [exReason ex | ex <- smExceptions model, exKey ex == smDefault model] of
        [] -> pure model
        Reason why : _ -> Left ("an exception divides under the default key, so it changes nothing: " <> why)

modelKeys :: KeySpec
modelKeys =
    keys
        [ plain "name"
        , plain "version"
        , plain "description"
        ,
            ( "allocation"
            , keys
                [ plain "default"
                ,
                    ( "exceptions"
                    , keys [plain "key", plain "reason", plain "processes", ("classification", keys [plain "system", plain "value"])]
                    )
                ]
            )
        , ("substitutions", keys [plain "coproduct", plain "replaces", plain "ratio", plain "reason"])
        ]
  where
    keys = Accepts . M.fromList
    plain k = (k, Accepts M.empty)

modelDecoder :: Text -> Decoder SystemModel
modelDecoder text = do
    name <- getField "name"
    version <- getField "version"
    description <- getFieldOpt "description"
    (deflt, exceptions) <- getFieldWith allocation "allocation"
    substitutions <- fromMaybe [] <$> getFieldOptWith (getArrayOf substitution) "substitutions"
    pure
        SystemModel
            { smName = name
            , smVersion = version
            , smDescription = description
            , smDefault = deflt
            , smExceptions = exceptions
            , smSubstitutions = substitutions
            , smText = text
            }
  where
    substitution :: Decoder ModelSubstitution
    substitution = do
        why <- T.strip <$> getField "reason"
        reason <- if T.null why then fail "a substitution needs its reason" else pure (Reason why)
        coproduct <- getFieldWith processRef "coproduct"
        replaces <- getFieldWith processRef "replaces"
        ratio <- getFieldWith number "ratio"
        unless (ratio > 0 && not (isInfinite ratio)) $
            fail ("the substitution \"" <> T.unpack why <> "\" needs a ratio above zero, not " <> show ratio)
        pure ModelSubstitution{subCoproduct = coproduct, subReplaces = replaces, subRatio = Ratio ratio, subReason = reason}

    -- TOML tells @2@ from @2.0@; a ratio is either.
    number :: Decoder Double
    number =
        tomlDecoder >>= \case
            Integer n -> pure (fromInteger n)
            Float d -> pure d
            other -> fail ("a ratio is a number, not " <> show other)

    processRef :: Decoder ProcessRef
    processRef = do
        raw <- tomlDecoder :: Decoder Text
        maybe (fail ("not a process identifier, written <activity>_<product>: " <> T.unpack raw)) pure (parseProcessRef (T.strip raw))

    allocation :: Decoder (AllocationProperty, [ModelException])
    allocation = (,) <$> getFieldWith property "default" <*> (fromMaybe [] <$> getFieldOptWith (getArrayOf exception) "exceptions")

    exception :: Decoder ModelException
    exception = do
        key <- getFieldWith property "key"
        why <- T.strip <$> getField "reason"
        selector <- (,) <$> getFieldOptWith classification "classification" <*> getFieldOpt "processes"
        reason <- if T.null why then fail "an exception needs its reason" else pure (Reason why)
        selects <- case selector of
            (Just c, Nothing) -> pure (ByClassification c)
            (Nothing, Just ids) -> ByProcesses <$> processIds ids
            (Nothing, Nothing) -> fail ("the exception \"" <> T.unpack why <> "\" takes no process: write classification or processes")
            (Just _, Just _) -> fail ("the exception \"" <> T.unpack why <> "\" writes both classification and processes: write one")
        pure ModelException{exKey = key, exReason = reason, exSelects = selects}

    classification :: Decoder ClassificationFilter
    classification = do
        system <- getField "system"
        value <- getField "value"
        pure ClassificationFilter{clfSystem = system, clfValue = value, clfMatch = MatchExact}

    processIds :: [Text] -> Decoder (NonEmpty UUID)
    processIds raw = case partitionEithers [maybe (Left t) Right (UUID.fromText (T.strip t)) | t <- raw] of
        ([], u : us) -> pure (u :| us)
        ([], []) -> fail "processes is empty"
        (bad, _) -> fail ("not process identifiers: " <> T.unpack (T.intercalate ", " bad))

    property :: Decoder AllocationProperty
    property = do
        raw <- tomlDecoder :: Decoder Text
        case Types.parseAllocationKey raw of
            Right (Types.ByProperty prop) -> pure prop
            Right _ -> fail ("a system model divides by price, dry mass or wet mass, not by " <> T.unpack raw)
            Left err -> fail (T.unpack err)

{- | What a model does to one parent: the property each block of several
products is divided under, keyed by the block's activity, the blocks each
exception took, and each substitution with the processes it names.
-}
data Resolution = Resolution
    { rsKeys :: !(M.Map UUID AllocationProperty)
    , rsTaken :: ![(ModelException, [UUID])]
    , rsSubstitutions :: ![Substituted]
    }

-- | A substitution as the parent holds it: the block and its coproduct's row, the process displaced and its product's row.
data Substituted = Substituted
    { sdRule :: !ModelSubstitution
    , sdBlock :: !Activity
    , sdCoproduct :: !Exchange
    , sdReplaced :: !Activity
    , sdReplacedProduct :: !Exchange
    }

{- | The model against the parent's blocks, or every way it does not fit:
a process it names that the parent lacks or that makes one product, an
exception that takes no block, a block two exceptions take, a substitution
that would leave a block nothing to carry or credit a process the child lacks.

The parent is given as its processes keyed by activity and product, as a load
keys them.
-}
resolveModel :: SystemModel -> M.Map (UUID, UUID) Activity -> Either (NonEmpty Text) Resolution
resolveModel model parent = case NE.nonEmpty (concatMap problemsOf taken <> overlaps <> concat subProblems <> twice <> emptied) of
    Just problems -> Left problems
    Nothing ->
        Right
            Resolution
                { rsKeys = M.union (M.fromList [(u, exKey ex) | (ex, us) <- taken, u <- us]) (smDefault model <$ blocks)
                , rsTaken = taken
                , rsSubstitutions = substituted
                }
  where
    allActs :: M.Map UUID Activity
    allActs = M.fromList [(activityId, act) | ((activityId, _), act) <- M.toList parent]

    blocks :: M.Map UUID Activity
    blocks = M.filter severalProducts allActs

    taken :: [(ModelException, [UUID])]
    taken = [(ex, takenBy (exSelects ex)) | ex <- smExceptions model]

    takenBy :: Selector -> [UUID]
    takenBy selector = case selector of
        ByClassification f -> M.keys (M.filter (classificationMatches f . activityClassification) blocks)
        ByProcesses ids -> filter (`M.member` blocks) (S.toList (S.fromList (NE.toList ids)))

    problemsOf :: (ModelException, [UUID]) -> [Text]
    problemsOf (ex, us) =
        [quoted ex <> " names " <> UUID.toText u <> ", which the parent " <> lacking u | ByProcesses ids <- [exSelects ex], u <- NE.toList ids, not (M.member u blocks)]
            <> [quoted ex <> " takes no process of several products" | null us]

    lacking :: UUID -> Text
    lacking u = maybe "does not hold" (const "holds with one product only") (M.lookup u allActs)

    overlaps :: [Text]
    overlaps =
        [ activityName (blocks M.! u) <> " is taken by two exceptions: " <> T.intercalate " and " (map quoted exs)
        | (u, exs) <- M.toList (M.fromListWith (flip (<>)) [(u, [ex]) | (ex, us) <- taken, u <- S.toList (S.fromList us)])
        , length exs > 1
        ]

    quoted :: ModelException -> Text
    quoted ex = "the " <> propertyName (exKey ex) <> " exception \"" <> reasonText (exReason ex) <> "\""

    (subProblems, substituted) = partitionEithers (map resolveOne (smSubstitutions model))

    resolveOne :: ModelSubstitution -> Either [Text] Substituted
    resolveOne s = case (producing (subCoproduct s), producing (subReplaces s)) of
        (Just (block, coproduct), Just (replaced, replacedProduct)) -> case problems of
            [] -> Right Substituted{sdRule = s, sdBlock = block, sdCoproduct = coproduct, sdReplaced = replaced, sdReplacedProduct = replacedProduct}
            _ -> Left problems
          where
            problems =
                [said s <> " takes " <> activityName block <> ", a process of one product: nothing would be left to carry its burden" | not (severalProducts block)]
                    <> [said s <> " takes a coproduct of " <> activityName block <> " whose amount is " <> T.pack (show (exchangeAmount coproduct)) <> ": it displaces nothing" | exchangeAmount coproduct <= 0]
                    <> [said s <> " credits " <> activityName replaced <> ", a product of the same block" | prActivity (subReplaces s) == prActivity (subCoproduct s)]
                    <> [said s <> " credits " <> activityName replaced <> ", whose product another substitution takes away" | subReplaces s `S.member` coproducts]
                    <> [said s <> " credits " <> activityName replaced <> ", which buys that coproduct itself: the line would loop" | any (buys (subCoproduct s)) (exchanges replaced)]
        (co, rep) ->
            Left $
                [said s <> " names " <> processRefText (subCoproduct s) <> " as its coproduct, which the parent does not make" | Nothing <- [co]]
                    <> [said s <> " names " <> processRefText (subReplaces s) <> " as the process it displaces, which the parent does not make" | Nothing <- [rep]]

    -- The product row of a process, by activity and product: a block is one entry, a sibling of one product under its activity another.
    producing :: ProcessRef -> Maybe (Activity, Exchange)
    producing (ProcessRef a p) =
        listToMaybe
            [ (act, ex)
            | act <- M.elems (M.takeWhileAntitone ((== a) . fst) (M.dropWhileAntitone ((< a) . fst) parent))
            , ex <- exchanges act
            , exchangeIsProductOutput ex
            , exchangeFlowId ex == p
            ]

    coproducts :: S.Set ProcessRef
    coproducts = S.fromList (map subCoproduct (smSubstitutions model))

    twice :: [Text]
    twice =
        [ processRefText ref <> " is taken by two substitutions: " <> T.intercalate " and " (map said ss)
        | (ref, ss) <- M.toList (M.fromListWith (flip (<>)) [(subCoproduct s, [s]) | s <- smSubstitutions model])
        , length ss > 1
        ]

    emptied :: [Text]
    emptied =
        [ activityName block <> " has every product substituted: nothing is left to carry its burden"
        | (block, flows) <- M.elems (M.fromListWith (\(b, fs) (_, gs) -> (b, S.union fs gs)) [(prActivity (subCoproduct (sdRule sd)), (sdBlock sd, S.singleton (exchangeFlowId (sdCoproduct sd)))) | sd <- substituted])
        , S.size flows >= length (filter exchangeIsProductOutput (exchanges block))
        ]

    said :: ModelSubstitution -> Text
    said s = "the substitution \"" <> reasonText (subReason s) <> "\""

-- | A line of a process buying this product from this process, as the parent linked it.
buys :: ProcessRef -> Exchange -> Bool
buys (ProcessRef a p) ex = case ex of
    TechnosphereExchange{techActivityLinkId = Just link} -> exchangeIsInput ex && exchangeFlowId ex == p && link == a
    _ -> False

-- | A block: the only kind of process a model divides.
severalProducts :: Activity -> Bool
severalProducts act = length (filter exchangeIsProductOutput (exchanges act)) > 1

{- | A substitution with the names a reader checks it by: the block, its
coproduct, the process displaced and its product.
-}
data NamedSubstitution = NamedSubstitution
    { nsRule :: !ModelSubstitution
    , nsBlock :: !Text
    , nsCoproduct :: !Text
    , nsReplaced :: !Text
    , nsReplacedProduct :: !Text
    }

{- | Each exception of a model with the parent's processes it takes, by
activity and name, and each substitution named: what a reader checks a rule
against its reason with.
-}
takenProcesses :: SystemModel -> Database -> Either (NonEmpty Text) ([(ModelException, [(UUID, Text)])], [NamedSubstitution])
takenProcesses model parent = named <$> resolveModel model (sdbActivities simple)
  where
    simple :: SimpleDatabase
    simple = toSimpleDatabase parent

    named :: Resolution -> ([(ModelException, [(UUID, Text)])], [NamedSubstitution])
    named resolution =
        ( [(ex, [(u, name) | u <- us, Just name <- [M.lookup u names]]) | (ex, us) <- rsTaken resolution]
        , [ NamedSubstitution
                { nsRule = sdRule sd
                , nsBlock = activityName (sdBlock sd)
                , nsCoproduct = flowName (sdCoproduct sd)
                , nsReplaced = activityName (sdReplaced sd)
                , nsReplacedProduct = flowName (sdReplacedProduct sd)
                }
          | sd <- rsSubstitutions resolution
          ]
        )

    names :: M.Map UUID Text
    names = M.fromList [(activityId, activityName act) | ((activityId, _), act) <- M.toList (sdbActivities simple)]

    flowName :: Exchange -> Text
    flowName ex = maybe (UUID.toText (exchangeFlowId ex)) tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows simple))

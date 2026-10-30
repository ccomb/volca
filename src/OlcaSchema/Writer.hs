{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | A database written as an openLCA JSON-LD package, the inverse of
"OlcaSchema.Parser": what the reader computes from the package written here
is the inventory the database computes.

One process per @(activity, product)@, identified as the ILCD writer
identifies its datasets ('ilcdProcessUUID'). One unit group and one flow
property per unit, the unit keeping its own identifier, so a read gives back
the same unit table. A location is identified as openLCA identifies it, from
its code, so the package's locations fall on those of an openLCA database it
is imported into.

The engine knows product outputs only, a waste treatment's reference being a
negative output of the waste; openLCA calls the same line a waste input. So a
flow some treatment takes as its reference is written as a waste flow, and
every line naming it is written on openLCA's side of that convention. A line
sent to a treatment whose column the engine normalises by a positive
reference (an ILCD treatment) changes sign with it, so the product is the
same.

A line in another unit than its flow is converted to the flow's, the one unit
of its unit group here. Location codes differing only by case are one location
to openLCA, written under the code the database uses most.

Parameters and formulas are written as openLCA reads them back
("Database.ExportFormulas"), a line's formula times the factor its amount is
written with. openLCA settles a global formula among the globals, where the
engine reads it with the process's own values: a process that redefines a name
some database formula it reads reaches gets that formula as its own
parameter, so both compute the same.

A process the database keeps whole, a parent's block of several products,
is written whole with an economic allocation factor per product: the shares
the @price@ key gives it, else those its products declare. openLCA divides it
by them, and so does the reader, which writes the pieces back one by one. A
block with neither keeps every product whole, which is said.

The product systems the database was read with are written back around
their reference process, with the processes and links its lines' providers
reach, and their parameter sets carry the saved scenarios too, each once.

What the package cannot say faithfully refuses the export, line by line;
what it says differently is a warning.
-}
module OlcaSchema.Writer (
    serializeOlcaPackage,
    locationId,
) where

import Control.Applicative ((<|>))
import Crypto.Hash (Digest, MD5, hash)
import Data.Aeson (KeyValue ((.=)))
import Data.Aeson.Encoding (Encoding, Series, encodingToLazyByteString, list, pair, pairs)
import qualified Data.Aeson.Key as K
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Containers.ListUtils (nubOrd)
import Data.Either (lefts, partitionEithers, rights)
import Data.Indexing (collisions)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import Numeric (readHex)

import qualified Data.IntMap.Strict as IM
import Database.Allocation (isCoproduct, producerKeyOf, propertyKeyShares)
import qualified Database.ExportFormulas as EF
import qualified Expr
import ILCD.Writer (exportedFlows, ilcdProcessUUID, sharedActivityUUIDs, splitWarnings)
import OlcaSchema.Parser (directionOf, shippedScenarios)
import Parametric (shareFormulas)
import Types
import UnitConversion (UnitConfig, convertUnit)

{- | The package's documents, by path, and what the package says differently
from the database, its product systems carrying the saved scenarios given.
'Left' names every line the format cannot carry.
-}
serializeOlcaPackage :: UnitConfig -> M.Map Text [ScenarioValue] -> SimpleDatabase -> Either Text ([(FilePath, BS.ByteString)], [Text])
serializeOlcaPackage units saved db = case (partitionEithers written, slashes) of
    (([], ok), []) ->
        let systems = productSystems cx saved ok
         in Right (sortOn fst (documents cx ok <> swDocuments systems), warnings cx ok <> swWarnings systems)
    ((bad, _), _) -> Left (describeRefusals (concat bad <> slashes))
  where
    cx = context units db
    written = map (uncurry (processLines cx)) (M.toAscList (sdbActivities db))
    slashes = [SlashInSub (ofName f) | f <- M.elems (cxFlows cx), OutElementary (Just c) <- [ofType f], maybe False (T.isInfixOf "/") (compartmentSub c)]

-- | What every line is written against.
data Context = Context
    { cxDb :: !SimpleDatabase
    , cxFlows :: !(M.Map UUID OutFlow)
    , cxShared :: !(S.Set UUID)
    , cxUnits :: !UnitConfig
    , cxPlaces :: !(M.Map Text Text)
    -- ^ Each location code to the one written for every code differing from it only by case.
    , cxLocations :: ![Text]
    -- ^ The location codes written.
    , cxFormulas :: EF.Exported
    -- ^ The parameters and formulas written, which read the lines' factors back from this context.
    }

data OutType = OutProduct | OutWaste | OutElementary !(Maybe Compartment)

data OutFlow = OutFlow
    { ofId :: !UUID
    , ofName :: !Text
    , ofCas :: !(Maybe Text)
    , ofUnit :: !UUID
    , ofType :: !OutType
    }

context :: UnitConfig -> SimpleDatabase -> Context
context units db = cx
  where
    cx = Context{cxDb = db, cxFlows = flows, cxShared = sharedActivityUUIDs db, cxUnits = units, cxPlaces = places, cxLocations = written, cxFormulas = EF.exportFormulas Expr.OpenLca factorOf db}
    -- A line refused here refuses the export, so its factor is never written.
    factorOf :: (UUID, UUID) -> Exchange -> EF.Scaled
    factorOf key ex = EF.Times (either (const 1) lnFactor (maybe (Left (NoFlow "")) (\act -> lineOf cx act ex) (M.lookup key (sdbActivities db))))
    uses = locationUses db
    written = S.toList (S.fromList [M.findWithDefault code code places | code <- M.keys uses])
    places =
        M.fromList
            [ (code, most)
            | (_, codes) <- collisions [(T.toLower code, (n, code)) | (code, n) <- M.toList uses]
            , let most = snd (NE.last (NE.sort codes))
            , (_, code) <- NE.toList codes
            ]

    -- A treated waste is in the technosphere and the waste tables under one identifier: written once, as the technosphere table describes it.
    flows = M.fromListWith (\_ first' -> first') [(ofId f, f) | f <- map outFlow (exportedFlows db)]

    outFlow (kind, unit) = case kind of
        TechKind f -> OutFlow (tfId f) (tfName f) (tfCAS f) unit (technosphere (tfId f))
        WasteKind f -> OutFlow (wfId f) (wfName f) (wfCAS f) unit (technosphere (wfId f))
        BioKind f -> OutFlow (bfId f) (bfName f) (bfCAS f) unit (OutElementary (bfCompartment f))

    technosphere fid = if S.member fid (wasteFlowIds db) then OutWaste else OutProduct

{- | The flows openLCA must read as waste: the waste table's, those a waste
line names, and those a treatment takes as its reference. Without the last,
an input product marked as reference is no product to openLCA, and the
treatment reads back with none.
-}
wasteFlowIds :: SimpleDatabase -> S.Set UUID
wasteFlowIds db =
    M.keysSet (sdbWasteFlows db)
        <> S.fromList
            [ exchangeFlowId ex
            | act <- M.elems (sdbActivities db)
            , ex <- exchanges act
            , treats ex
            ]
  where
    treats ex = case ex of
        WasteExchange{} -> True
        TechnosphereExchange{techRole = ReferenceInput} -> True
        TechnosphereExchange{techRole = ReferenceProduct, techAmount = a} -> a < 0
        TechnosphereExchange{} -> False
        BiosphereExchange{} -> False

-- | One exchange as openLCA states it.
data Line = Line
    { lnFlow :: !OutFlow
    , lnAmount :: !Double
    , lnFactor :: !Double
    -- ^ What the stored amount is multiplied by to give 'lnAmount', which the line's formula is too.
    , lnInput :: !Bool
    , lnReference :: !Bool
    , lnAvoided :: !Bool
    , lnProvider :: !(Maybe UUID)
    , lnLocation :: !Text
    , lnComment :: !(Maybe Text)
    }

-- | A process as written: its identifier, the activity, its lines, and the economic factor of each product of a block it keeps whole.
data Written = Written
    { wrId :: !UUID
    , wrKey :: !(UUID, UUID)
    , wrActivity :: !Activity
    , wrLines :: ![Line]
    , wrFactors :: ![(OutFlow, Double)]
    }

-- | Why a line cannot be written, each naming it.
data Refusal
    = NoFlow !Text
    | OtherUnit !Text
    | SlashInSub !Text
    | TreatmentAbsent !Text
    | PositiveWasteReference !Text
    | NotFinite !Text
    | SameProductTwice !Text

-- | Every line the package would carry wrong, named and grouped by why.
describeRefusals :: [Refusal] -> Text
describeRefusals refusals =
    T.intercalate "\n" $
        "openLCA package export cannot carry " <> tshow (length refusals) <> " lines:"
            : concat
                [ ("  " <> tshow (length named) <> " " <> heading) : map ("    " <>) (take 20 named) <> ["    and " <> tshow (length named - 20) <> " more" | length named > 20]
                | (heading, named) <- groups
                , not (null named)
                ]
  where
    groups =
        [ ("name a flow the database does not describe:", [n | NoFlow n <- refusals])
        , ("are in a unit that does not convert to their flow's, and a package's unit group here holds one unit:", [n | OtherUnit n <- refusals])
        , ("elementary flows have a sub-compartment containing '/', which openLCA reads as a deeper category:", [n | SlashInSub n <- refusals])
        , ("waste inputs go to a treatment the database does not have, so their sign cannot be decided:", [n | TreatmentAbsent n <- refusals])
        , ("references are a positive output of a waste flow, which openLCA cannot make:", [n | PositiveWasteReference n <- refusals])
        , ("amounts are not finite:", [n | NotFinite n <- refusals])
        , ("processes make the same product twice, which an allocation factor of openLCA cannot tell apart:", [n | SameProductTwice n <- refusals])
        ]

-- | Every location code a process or a line states, with how often.
locationUses :: SimpleDatabase -> M.Map Text Int
locationUses db =
    M.fromListWith (+) [(code, 1) | act <- M.elems (sdbActivities db), code <- activityLocation act : map exchangeLocation (exchanges act), not (T.null code)]

-- | The code written for a location code.
place :: Context -> Text -> Text
place cx code = M.findWithDefault code code (cxPlaces cx)

{- | openLCA's identifier for a location: a version 3 UUID of the lowercased
code's bytes alone, with no namespace before them.
-}
locationId :: Text -> UUID
locationId code = fromMaybe UUID.nil (UUID.fromByteString (BL.fromStrict (BS.pack (versioned (hexBytes (show digest))))))
  where
    digest :: Digest MD5
    digest = hash (TE.encodeUtf8 (T.toLower code))
    hexBytes hex = case hex of
        a : b : rest -> [v | (v, "") <- readHex [a, b]] <> hexBytes rest
        _ -> []
    versioned bytes = [setBits i b | (i, b) <- zip [0 :: Int ..] bytes]
    setBits i b
        | i == 6 = (b .&. 0x0f) .|. 0x30
        | i == 8 = (b .&. 0x3f) .|. 0x80
        | otherwise = b

-- | A process's lines, or why some cannot be written.
processLines :: Context -> (UUID, UUID) -> Activity -> Either [Refusal] Written
processLines cx key act = case partitionEithers lines' of
    ([], ok)
        | any isCoproduct products && length (nubOrd (map exchangeFlowId products)) < length products -> Left [SameProductTwice (activityName act)]
        | otherwise -> Right Written{wrId = ilcdProcessUUID (cxShared cx) key, wrKey = key, wrActivity = act, wrLines = ok, wrFactors = factors}
    (bad, _) -> Left bad
  where
    lines' = map (lineOf cx act) (exchanges act)
    products = filter exchangeIsProductOutput (exchanges act)

    -- A process of one product is no block: openLCA reads it whole, as it should.
    factors
        | any isCoproduct products = maybe [] (\ss -> [(f, share / 100) | (ex, share) <- zip products ss, Just f <- [flowOf ex]]) shares
        | otherwise = []
    shares =
        propertyKeyShares Price (cxUnits cx) (sdbUnits (cxDb cx)) products
            -- A price in euros beside one deduced from a share cannot weigh the block; its declared shares still do.
            <|> traverse (fmap dsPercent . exchangeDeclaredShare) products
    flowOf ex = M.lookup (exchangeFlowId ex) (cxFlows cx)

lineOf :: Context -> Activity -> Exchange -> Either Refusal Line
lineOf cx act ex = do
    flow <- maybe (Left (NoFlow named)) Right (M.lookup (exchangeFlowId ex) (cxFlows cx))
    whenLeft (isNaN (exchangeAmount ex) || isInfinite (exchangeAmount ex)) (NotFinite named)
    conversion <- maybe (Left (OtherUnit named)) Right (inUnitOf flow)
    let amount = conversion * exchangeAmount ex
        -- The line written with the stored amount in the flow's unit, times a sign.
        line input sign = Line{lnFlow = flow, lnAmount = sign * amount, lnFactor = sign * conversion, lnInput = input, lnReference = False, lnAvoided = False, lnProvider = provider, lnLocation = place cx (exchangeLocation ex), lnComment = exchangeComment ex}
        -- A line sent to a treatment, by the demand it puts on it: written as the waste output openLCA reads it as.
        sent sign = pure (line False (sign * treatmentSign))
        sideOf :: Bool -> Double
        sideOf output = if output then 1 else -1
    case (ex, ofType flow) of
        {- The engine signs an elementary amount by the line's kind, openLCA by
        its side, and a reader takes the kind from the compartment. Written on
        the compartment's side, the amount reads back with its sign, and the
        matrix takes the sign alone. -}
        (BiosphereExchange{}, OutElementary comp) ->
            pure $ case directionOf comp of
                Emission -> (line (amount < 0) (sideOf (amount >= 0))){lnProvider = Nothing}
                Resource -> (line (amount >= 0) (sideOf (amount >= 0))){lnProvider = Nothing}
        (BiosphereExchange{}, _) -> Left (NoFlow named)
        (_, OutElementary _) -> Left (NoFlow named)
        (TechnosphereExchange{techRole = role}, OutProduct) -> pure $ case role of
            ReferenceProduct -> (line False 1){lnReference = True, lnProvider = Nothing}
            Coproduct -> (line False 1){lnProvider = Nothing}
            Input -> line True 1
            AvoidedProduct -> (line True 1){lnAvoided = True}
            -- Never: a reference input makes its flow a waste flow.
            ReferenceInput -> (line True 1){lnReference = True, lnProvider = Nothing}
        (TechnosphereExchange{techRole = role}, OutWaste) -> case role of
            ReferenceProduct
                | amount < 0 -> pure (line True (-1)){lnReference = True, lnProvider = Nothing}
                | otherwise -> Left (PositiveWasteReference named)
            -- Read back as a reference output of the opposite amount, which turns the column's sign.
            ReferenceInput -> pure (line True 1){lnReference = True, lnProvider = Nothing}
            Coproduct -> pure (line True (-1)){lnProvider = Nothing}
            Input -> sent 1
            AvoidedProduct -> sent (-1)
        (WasteExchange{waIsInput = True}, _)
            | isJust treatment -> sent 1
            | otherwise -> Left (TreatmentAbsent named)
        (WasteExchange{}, _) -> sent (-1)
  where
    unitNamed u = unitName <$> M.lookup u (sdbUnits (cxDb cx))

    -- What one unit of the line is in the flow's unit, the only one its unit group holds here.
    inUnitOf flow
        | exchangeUnitId ex == ofUnit flow = Just 1
        | otherwise = do
            from <- unitNamed (exchangeUnitId ex)
            to <- unitNamed (ofUnit flow)
            convertUnit (cxUnits cx) from to 1

    named = activityName act <> " · " <> maybe (UUID.toText (exchangeFlowId ex)) ofName (M.lookup (exchangeFlowId ex) (cxFlows cx))

    activities = sdbActivities (cxDb cx)

    supplierKey = (,exchangeFlowId ex) <$> exchangeActivityLinkId ex

    treatment = supplierKey >>= \k -> (,) k <$> M.lookup k activities

    -- The written process of the supplier, else the identifier as the source stated it.
    provider = case supplierKey of
        Just k | Just p <- producerKeyOf activities k -> Just (ilcdProcessUUID (cxShared cx) p)
        Just (l, _) -> Just l
        Nothing -> Nothing

    {- Every treatment reads back normalised by a negative reference, and a
    waste output of y puts a demand of -y on it. One the engine normalises by
    a positive amount (an ILCD reference input) turns its sign there, so the
    demand on it turns too. A line to no treatment of the database is written
    in the EcoSpold 2 convention, as if its treatment's reference were negative. -}
    treatmentSign :: Double
    treatmentSign = case treatment of
        Just (k, t) -> signum (activityNormFactor t k)
        Nothing -> -1

    whenLeft c r = if c then Left r else Right ()

{- | The category path the reader turns back into this compartment. The three
media openLCA names get its own spelling; the others go under an emission
level the reader reads them from.
-}
categoryPath :: Maybe Compartment -> Text
categoryPath comp = T.intercalate "/" ("Elementary flows" : maybe [] levels comp)
  where
    levels (Compartment medium sub) = case medium of
        Air -> "Emission to air" : subLevel sub
        Water -> "Emission to water" : subLevel sub
        Soil -> "Emission to soil" : subLevel sub
        NaturalResource -> "Resource" : subLevel sub
        InventoryIndicator -> "Emission" : mediumText medium : subLevel sub
        Economic -> "Emission" : mediumText medium : subLevel sub
        Waste -> "Emission" : mediumText medium : subLevel sub
        Social -> "Emission" : mediumText medium : subLevel sub
    subLevel = maybe [] pure

{- | What the package says differently from the database: the products of one
activity become processes of their own, a block written without allocation
factors is read with the whole inventory, an input naming no supplier is
linked by openLCA to a producer of the package, and formulas are written as
their values.
-}
warnings :: Context -> [Written] -> [Text]
warnings cx processes = splitWarnings (cxDb cx) <> EF.formulaWarnings (cxFormulas cx) <> mapMaybe counted kinds
  where
    acts = M.elems (sdbActivities (cxDb cx))
    counted (what, named) = case named of
        [] -> Nothing
        _ -> Just (tshow (length named) <> " " <> what <> foldMap ("\n  " <>) (take 10 named))
    kinds =
        [ ("processes keep a product they did not allocate, and no price or share weighs it; openLCA reads it with the whole inventory:", [activityName (wrActivity w) | w <- processes, null (wrFactors w), any isCoproduct (exchanges (wrActivity w))])
        , ("inputs name no supplier, and openLCA will link them to a producer of the package:", unlinkedProduced)
        , ("processes have a share formula, which openLCA has no place for, and are written at the share's value:", [activityName a | a <- acts, not (null (shareFormulas a))])
        , ("processes state an elementary flow against its compartment, written on its side with the same inventory:", S.toList (S.fromList against))
        , ("location codes differ from another only by case, one location to openLCA, and are written as it:", [code <> " as " <> written | (code, written) <- M.toList (cxPlaces cx), code /= written])
        ]
    produced = S.fromList [exchangeFlowId ex | a <- acts, ex <- exchanges a, exchangeIsReference ex]
    unlinkedProduced =
        [ activityName a <> " · " <> maybe "" ofName (M.lookup (exchangeFlowId ex) (cxFlows cx))
        | a <- acts
        , ex <- exchanges a
        , not (isBiosphereExchange ex)
        , not (exchangeIsReference ex)
        , not (isCoproduct ex)
        , isNothing (exchangeActivityLinkId ex)
        , S.member (exchangeFlowId ex) produced
        ]
    against =
        [ activityName a <> " · " <> ofName f
        | a <- acts
        , ex@BiosphereExchange{bioDirection = dir} <- exchanges a
        , Just f <- [M.lookup (exchangeFlowId ex) (cxFlows cx)]
        , OutElementary comp <- [ofType f]
        , dir /= directionOf comp
        ]

-- | Every document of the package.
documents :: Context -> [Written] -> [(FilePath, BS.ByteString)]
documents cx written =
    ("openlca.json", encode (pairs ("schemaVersion" .= (2 :: Int))))
        : [("unit_groups/" <> uuidPath (groupId u), encode (unitGroupDoc u)) | u <- units]
            <> [("flow_properties/" <> uuidPath (propertyId u), encode (propertyDoc u)) | u <- units]
            <> [("flows/" <> uuidPath (ofId f), encode (flowDoc cx f)) | f <- M.elems (cxFlows cx)]
            <> [("locations/" <> uuidPath (locationId c), encode (locationDoc c)) | c <- cxLocations cx]
            <> [("processes/" <> uuidPath (wrId w), encode (processDoc cx w)) | w <- written]
            <> [("parameters/" <> uuidPath (parameterId Global name), encode (parameterDoc Global (name, p))) | (name, p) <- M.toList (EF.exDatabase (cxFormulas cx))]
  where
    units = M.elems (sdbUnits (cxDb cx))
    uuidPath u = UUID.toString u <> ".json"

-- | Where a parameter is defined: the database, or one process.
data Scope = Global | InProcess !UUID

-- | The product systems as written, and what they say differently.
data SystemsWritten = SystemsWritten
    { swDocuments :: ![(FilePath, BS.ByteString)]
    , swWarnings :: ![Text]
    }

-- | A product system written around its reference row, with the parameter sets it carries.
data Carried = Carried
    { caSystem :: !ProductSystem
    , caReference :: !Written
    , caSets :: ![ParameterSet]
    }

-- | A product system whose reference row is not written, and why.
data Unheld = Unheld
    { unSystem :: !ProductSystem
    , unWhy :: !Text
    }

-- | A line of a written process openLCA links to the written process providing it.
data ProcessLink = ProcessLink
    { plLine :: !Int
    -- ^ The line's internal id, as 'processDoc' numbers the lines
    , plFlow :: !OutFlow
    , plProvider :: !Written
    }

-- | The system a saved scenario goes on, and the set it is written as there.
data Home = Home
    { hoSystem :: !ProductSystem
    , hoSet :: !ParameterSet
    }

{- | The product systems whose reference row is written, each with its sets as
read, a saved scenario named after one of them in its place. A saved scenario
named "<system>: <set>" goes on that system as that set, any other on the
first system whose name no other written one has and whose sets hold none of
that name, under its own name: each is written once, and replaces only the set
it is named after. One that would still not read back (another system of that
name holding a set of that name) is said.
-}
productSystems :: Context -> M.Map Text [ScenarioValue] -> [Written] -> SystemsWritten
productSystems cx saved written =
    SystemsWritten
        { swDocuments = [("product_systems/" <> UUID.toString (systemId (caSystem c)) <> ".json", encode (systemDoc c)) | c <- carried]
        , swWarnings =
            catMaybes
                [ counted "product systems reference a process the database does not hold, and are not written:" [systemName (unSystem u) <> ": " <> unWhy u | u <- unheld]
                , counted "product systems have lines linked to no process of the package:" [systemName (caSystem c) <> ": " <> tshow n <> " lines" | c <- carried, let n = unlinked c, n > 0]
                , counted "parameter set values name a process the database does not hold, and are left out:" (concatMap valuesLeftOut carried)
                , counted "saved scenarios are not in the export, no product system being written to carry them:" (M.keys (M.filter isNothing homes))
                , counted "saved scenarios are written but read back as none, another product system of that name holding a set of that name:" unread
                ]
        }
  where
    byKey :: M.Map (UUID, UUID) Written
    byKey = M.fromList [(wrKey w, w) | w <- written]

    byId :: M.Map UUID Written
    byId = M.fromList [(wrId w, w) | w <- written]

    rowsOf :: M.Map UUID [UUID]
    rowsOf = M.fromListWith (flip (<>)) [(fst (wrKey w), [wrId w]) | w <- written]

    held :: [Either Unheld Carried]
    held =
        [ case systemReference s of
            Nothing -> Left (Unheld s "its reference was not read")
            Just (ProcessRef activity productId) ->
                maybe
                    (Left (Unheld s ("process " <> UUID.toText activity <> "_" <> UUID.toText productId)))
                    (\w -> Right Carried{caSystem = s, caReference = w, caSets = systemSets s})
                    (M.lookup (activity, productId) byKey)
        | s <- sdbProductSystems (cxDb cx)
        ]

    unheld :: [Unheld]
    unheld = lefts held

    ordered :: [Carried]
    ordered = sortOn (\c -> (systemName (caSystem c), systemId (caSystem c))) (rights held)

    carried :: [Carried]
    carried = [c{caSets = foldl (flip withSet) (caSets c) (M.findWithDefault [] (systemId (caSystem c)) placed)} | c <- ordered]

    homes :: M.Map Text (Maybe Home)
    homes = M.union (M.map Just namedAfter) (M.mapWithKey elsewhere (M.difference saved namedAfter))

    placed :: M.Map UUID [ParameterSet]
    placed = M.fromListWith (flip (<>)) [(systemId (hoSystem h), [hoSet h]) | Just h <- M.elems homes]

    -- The saved scenarios named after a system, the longest name first.
    namedAfter :: M.Map Text Home
    namedAfter = M.mapMaybeWithKey (\name values -> (\s -> homeOn s (T.drop (T.length (systemName s) + 2) name) values) <$> listToMaybe (sortOn (negate . T.length . systemName) [s | s <- systems, (systemName s <> ": ") `T.isPrefixOf` name])) saved

    -- Any other, on a system whose set names it would not take over.
    elsewhere :: Text -> [ScenarioValue] -> Maybe Home
    elsewhere name values = (\s -> homeOn s name values) <$> listToMaybe [s | s <- systems, length (filter ((== systemName s) . systemName) systems) == 1, name `notElem` setNames s]

    setNames :: ProductSystem -> [Text]
    setNames s = map parameterSetName (systemSets s) <> [parameterSetName (hoSet h) | h <- M.elems namedAfter, systemId (hoSystem h) == systemId s]

    homeOn :: ProductSystem -> Text -> [ScenarioValue] -> Home
    homeOn s setName values = Home{hoSystem = s, hoSet = ParameterSet setName (map redefinitionOf values)}

    -- The saved scenarios the package, read back, would not list.
    unread :: [Text]
    unread =
        let listed = fst (shippedScenarios [(caSystem c){systemSets = caSets c} | c <- carried])
         in [name | (name, Just h) <- M.toList homes, M.notMember (systemName (hoSystem h) <> ": " <> parameterSetName (hoSet h)) listed]

    systems :: [ProductSystem]
    systems = map caSystem ordered

    redefinitionOf :: ScenarioValue -> Redefinition
    redefinitionOf v = Redefinition{redefinedName = svName v, redefinedValue = svValue v, redefinedContext = maybe OfDatabase OfProcess (svProcess v)}

    -- The set in place of the first of its name, or after the others when none has it.
    withSet :: ParameterSet -> [ParameterSet] -> [ParameterSet]
    withSet set sets = case break ((== parameterSetName set) . parameterSetName) sets of
        (before, _ : after) -> before <> [set] <> filter ((/= parameterSetName set) . parameterSetName) after
        (_, []) -> sets <> [set]

    -- The processes reached from the reference through the providers of their linked lines, depth first.
    reached :: Written -> [Written]
    reached start = go S.empty [start]
      where
        go :: S.Set UUID -> [Written] -> [Written]
        go seen = \case
            [] -> []
            w : rest
                | S.member (wrId w) seen -> go seen rest
                | otherwise -> w : go (S.insert (wrId w) seen) (map plProvider (links w) <> rest)

    links :: Written -> [ProcessLink]
    links w = [ProcessLink{plLine = i, plFlow = lnFlow ln, plProvider = p} | (i, ln) <- zip [1 ..] (wrLines w), linkable ln, Just p <- [(`M.lookup` byId) =<< lnProvider ln]]

    unlinked :: Carried -> Int
    unlinked c = length [() | w <- reached (caReference c), ln <- wrLines w, linkable ln, maybe True (`M.notMember` byId) (lnProvider ln)]

    systemDoc :: Carried -> Encoding
    systemDoc c =
        pairs $
            "@type" .= ("ProductSystem" :: Text)
                <> "@id" .= UUID.toText (systemId (caSystem c))
                <> "name" .= systemName (caSystem c)
                <> pair "referenceProcess" (processRef reference)
                <> foldMap (\i -> pair "referenceExchange" (pairs ("@type" .= ("Exchange" :: Text) <> "internalId" .= i))) (listToMaybe [i | (i, ln) <- numbered, lnReference ln])
                <> "targetAmount" .= systemTarget (caSystem c)
                <> foldMap (targetUnit . lnFlow) (listToMaybe [ln | (_, ln) <- numbered, lnReference ln])
                <> pair "processes" (list processRef processes)
                <> pair "processLinks" (list id [linkDoc p l | p <- processes, l <- links p])
                <> pair "parameterSets" (list setDoc (caSets c))
      where
        reference :: Written
        reference = caReference c

        numbered :: [(Int, Line)]
        numbered = zip [1 ..] (wrLines reference)

        processes :: [Written]
        processes = reached reference

        targetUnit :: OutFlow -> Series
        targetUnit flow = case M.lookup (ofUnit flow) (sdbUnits (cxDb cx)) of
            Just u -> pair "targetUnit" (ref "Unit" (unitId u) (unitName u)) <> pair "targetFlowProperty" (ref "FlowProperty" (propertyId u) (unitName u))
            Nothing -> mempty

    processRef :: Written -> Encoding
    processRef w = ref "Process" (wrId w) (activityName (wrActivity w))

    linkDoc :: Written -> ProcessLink -> Encoding
    linkDoc p l =
        pairs $
            pair "provider" (processRef (plProvider l))
                <> pair "flow" (ref "Flow" (ofId (plFlow l)) (ofName (plFlow l)))
                <> pair "process" (processRef p)
                <> pair "exchange" (pairs ("@type" .= ("Exchange" :: Text) <> "internalId" .= plLine l))

    setDoc :: ParameterSet -> Encoding
    setDoc set = pairs ("@type" .= ("ParameterRedefSet" :: Text) <> "name" .= parameterSetName set <> pair "parameters" (list id [redefinitionDoc r owner | r <- parameterSetValues set, owner <- owners r]))

    redefinitionDoc :: Redefinition -> Series -> Encoding
    redefinitionDoc r owner = pairs ("@type" .= ("ParameterRedef" :: Text) <> "name" .= redefinedName r <> "value" .= redefinedValue r <> owner)

    -- The values of a system's sets left out for naming a process the database does not hold.
    valuesLeftOut :: Carried -> [Text]
    valuesLeftOut c = [systemName (caSystem c) <> ": " <> parameterSetName set <> " · " <> redefinedName r | set <- caSets c, r <- parameterSetValues set, null (owners r)]

    -- The context of each redefinition written for a value: one per written row of a process's block, none for a process gone.
    owners :: Redefinition -> [Series]
    owners r = case redefinedContext r of
        OfDatabase -> [mempty]
        OfProcess activity -> [pair "context" (pairs ("@type" .= ("Process" :: Text) <> "@id" .= UUID.toText row)) | row <- M.findWithDefault [] activity rowsOf]
        OfImpactCategory category -> [pair "context" (pairs ("@type" .= ("ImpactCategory" :: Text) <> "@id" .= UUID.toText category))]
        OfOtherContext kind owner -> [pair "context" (pairs ("@type" .= kind <> foldMap (("@id" .=) . UUID.toText) owner))]

    counted :: Text -> [Text] -> Maybe Text
    counted what = \case
        [] -> Nothing
        named -> Just (tshow (length named) <> " " <> what <> foldMap ("\n  " <>) (take 10 named))

{- | A line openLCA links to a provider: a product input, an avoided product
among them, or a waste output sent to its treatment.
-}
linkable :: Line -> Bool
linkable ln
    | lnReference ln = False
    | otherwise = case ofType (lnFlow ln) of
        OutProduct -> lnInput ln
        OutWaste -> not (lnInput ln)
        OutElementary _ -> False

{- | A parameter as openLCA reads it: an input with its value and spread, or a
formula, with the value it computed when the engine holds one.
-}
parameterDoc :: Scope -> (Text, (Parameter, EF.Written)) -> Encoding
parameterDoc scope (name, (p, written)) =
    pairs $
        "@type" .= ("Parameter" :: Text)
            <> "@id" .= UUID.toText (parameterId scope name)
            <> "name" .= name
            <> "parameterScope" .= scopeName
            <> nonEmpty "description" (parameterDescription p)
            <> case written of
                EF.WrittenValue value spread -> "isInputParameter" .= True <> "value" .= value <> maybe mempty (pair "uncertainty" . uncertaintyDoc) spread
                EF.WrittenFormula formula -> "isInputParameter" .= False <> "formula" .= formula <> maybe mempty ("value" .=) (computed (parameterDefinition p))
  where
    scopeName :: Text
    scopeName = case scope of
        Global -> "GLOBAL_SCOPE"
        InProcess _ -> "PROCESS_SCOPE"
    computed :: ParameterDefinition -> Maybe Double
    computed = \case
        CalculatedParameter _ value -> value
        InputParameter{} -> Nothing

-- | A spread in the fields 'OlcaSchema.Package.distribution' reads.
uncertaintyDoc :: Uncertainty -> Encoding
uncertaintyDoc = \case
    Lognormal mean sd -> pairs ("distributionType" .= ("LOG_NORMAL_DISTRIBUTION" :: Text) <> "geomMean" .= mean <> "geomSd" .= sd)
    Normal mean sd -> pairs ("distributionType" .= ("NORMAL_DISTRIBUTION" :: Text) <> "mean" .= mean <> "sd" .= sd)
    Triangular low mode high -> pairs ("distributionType" .= ("TRIANGLE_DISTRIBUTION" :: Text) <> "minimum" .= low <> "mode" .= mode <> "maximum" .= high)
    Uniform low high -> pairs ("distributionType" .= ("UNIFORM_DISTRIBUTION" :: Text) <> "minimum" .= low <> "maximum" .= high)

-- | A parameter identified by its name within its scope, names compared without case.
parameterId :: Scope -> Text -> UUID
parameterId scope name = UUID5.generateNamed namespace (BS.unpack (TE.encodeUtf8 ("parameter:" <> owner <> T.toLower name)))
  where
    owner :: Text
    owner = case scope of
        Global -> ""
        InProcess process -> UUID.toText process <> ":"

encode :: Encoding -> BS.ByteString
encode = BL.toStrict . encodingToLazyByteString

-- | Namespace for the identifiers this writer mints.
namespace :: UUID
namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack (TE.encodeUtf8 "olca-package-export"))

minted :: Text -> UUID -> UUID
minted kind u = UUID5.generateNamed namespace (BS.unpack (TE.encodeUtf8 (kind <> ":" <> UUID.toText u)))

groupId, propertyId :: Unit -> UUID
groupId = minted "unit-group" . unitId
propertyId = minted "flow-property" . unitId

ref :: Text -> UUID -> Text -> Encoding
ref kind u name = pairs ("@type" .= kind <> "@id" .= UUID.toText u <> "name" .= name)

unitGroupDoc :: Unit -> Encoding
unitGroupDoc u =
    pairs $
        "@type" .= ("UnitGroup" :: Text)
            <> "@id" .= UUID.toText (groupId u)
            <> "name" .= unitName u
            <> pair "defaultFlowProperty" (ref "FlowProperty" (propertyId u) (unitName u))
            <> pair "units" (list unitDoc [u])
  where
    unitDoc v =
        pairs $
            "@type" .= ("Unit" :: Text)
                <> "@id" .= UUID.toText (unitId v)
                <> "name" .= unitName v
                <> "conversionFactor" .= (1 :: Double)
                <> "isRefUnit" .= True

propertyDoc :: Unit -> Encoding
propertyDoc u =
    pairs $
        "@type" .= ("FlowProperty" :: Text)
            <> "@id" .= UUID.toText (propertyId u)
            <> "name" .= unitName u
            <> "flowPropertyType" .= ("PHYSICAL_QUANTITY" :: Text)
            <> pair "unitGroup" (ref "UnitGroup" (groupId u) (unitName u))

flowDoc :: Context -> OutFlow -> Encoding
flowDoc cx f =
    pairs $
        "@type" .= ("Flow" :: Text)
            <> "@id" .= UUID.toText (ofId f)
            <> "name" .= ofName f
            <> "flowType" .= flowType
            <> category
            <> maybe mempty ("cas" .=) (ofCas f)
            <> pair "flowProperties" (list factor [ofUnit f])
  where
    (flowType, category) = case ofType f of
        OutProduct -> ("PRODUCT_FLOW" :: Text, mempty)
        OutWaste -> ("WASTE_FLOW", mempty)
        OutElementary comp -> ("ELEMENTARY_FLOW", "category" .= categoryPath comp)
    factor u =
        pairs $
            "@type" .= ("FlowPropertyFactor" :: Text)
                <> pair "flowProperty" (unitRef "FlowProperty" propertyId u)
                <> "conversionFactor" .= (1 :: Double)
                <> "isRefFlowProperty" .= True
    unitRef kind toId u = case M.lookup u (sdbUnits (cxDb cx)) of
        Just unit -> ref kind (toId unit) (unitName unit)
        Nothing -> ref kind u ""

locationDoc :: Text -> Encoding
locationDoc code =
    pairs ("@type" .= ("Location" :: Text) <> "@id" .= UUID.toText (locationId code) <> "name" .= code <> "code" .= code)

processDoc :: Context -> Written -> Encoding
processDoc cx w =
    pairs $
        "@type" .= ("Process" :: Text)
            <> "@id" .= UUID.toText (wrId w)
            <> "name" .= activityName act
            <> maybe mempty ("category" .=) (M.lookup "Category" (activityClassification act))
            <> nonEmpty "description" (T.intercalate "\n\n" (activityDescription act))
            <> "processType" .= ("UNIT_PROCESS" :: Text)
            <> locationRef (place cx (activityLocation act))
            <> pair "exchanges" (list (uncurry (exchangeDoc cx)) numbered)
            <> "lastInternalId" .= length numbered
            <> parametersOf (M.toList (EF.peParameters ownFormulas))
            <> factorsOf (wrFactors w)
  where
    act = wrActivity w
    ownFormulas = EF.processExport (cxFormulas cx) (wrKey w)
    formulas = EF.peLines ownFormulas
    numbered = zip [1 :: Int ..] [(IM.lookup i formulas, ln) | (i, ln) <- zip [0 ..] (wrLines w)]
    factorsOf :: [(OutFlow, Double)] -> Series
    factorsOf = \case
        [] -> mempty
        factors -> "defaultAllocationMethod" .= economic <> pair "allocationFactors" (list factorDoc factors)
    factorDoc (flow, value) = pairs ("@type" .= ("AllocationFactor" :: Text) <> "allocationType" .= economic <> pair "product" (ref "Flow" (ofId flow) (ofName flow)) <> "value" .= value)
    economic = "ECONOMIC_ALLOCATION" :: Text
    parametersOf :: [(Text, (Parameter, EF.Written))] -> Series
    parametersOf = \case
        [] -> mempty
        params -> pair "parameters" (list (parameterDoc (InProcess (wrId w))) params)

exchangeDoc :: Context -> Int -> (Maybe Text, Line) -> Encoding
exchangeDoc cx internalId (formula, ln) =
    pairs $
        "@type" .= ("Exchange" :: Text)
            <> "internalId" .= internalId
            <> pair "flow" (ref "Flow" (ofId flow) (ofName flow))
            <> "amount" .= lnAmount ln
            <> maybe mempty ("amountFormula" .=) formula
            <> unitRefs
            <> flag "isInput" (lnInput ln)
            <> flag "isQuantitativeReference" (lnReference ln)
            <> flag "isAvoidedProduct" (lnAvoided ln)
            <> maybe mempty (\p -> pair "defaultProvider" (pairs ("@type" .= ("Process" :: Text) <> "@id" .= UUID.toText p))) (lnProvider ln)
            <> locationRef (lnLocation ln)
            <> maybe mempty (nonEmpty "description") (lnComment ln)
  where
    flow = lnFlow ln
    unitRefs = case M.lookup (ofUnit flow) (sdbUnits (cxDb cx)) of
        Just u -> pair "unit" (ref "Unit" (unitId u) (unitName u)) <> pair "flowProperty" (ref "FlowProperty" (propertyId u) (unitName u))
        Nothing -> pair "unit" (ref "Unit" (ofUnit flow) "")
    -- openLCA leaves a boolean out when it is false.
    flag key on = if on then key .= True else mempty

locationRef :: Text -> Series
locationRef code
    | T.null code = mempty
    | otherwise = pair "location" (ref "Location" (locationId code) code)

nonEmpty :: Text -> Text -> Series
nonEmpty key value
    | T.null (T.strip value) = mempty
    | otherwise = K.fromText key .= value

tshow :: (Show a) => a -> Text
tshow = T.pack . show

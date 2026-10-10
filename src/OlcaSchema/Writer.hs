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

What the package cannot say faithfully refuses the export, line by line
('checkOlcaPackageExportable'); what it says differently is a warning.
-}
module OlcaSchema.Writer (
    serializeOlcaPackage,
    checkOlcaPackageExportable,
    locationId,
) where

import Crypto.Hash (Digest, MD5, hash)
import Data.Aeson (KeyValue ((.=)))
import Data.Aeson.Encoding (Encoding, Series, encodingToLazyByteString, list, pair, pairs)
import qualified Data.Aeson.Key as K
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (lefts, rights)
import Data.Indexing (collisions)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import Numeric (readHex)

import ILCD.Writer (exportedFlows, ilcdProcessUUID, sharedActivityUUIDs, splitWarnings)
import Types
import UnitConversion (UnitConfig, convertUnit)

{- | The package's documents, by path, and what the package says differently
from the database. 'Left' names every line the format cannot carry.
-}
serializeOlcaPackage :: UnitConfig -> SimpleDatabase -> Either Text ([(FilePath, BS.ByteString)], [Text])
serializeOlcaPackage units db = do
    checkOlcaPackageExportable units db
    let written = map (uncurry (processLines cx)) (M.toAscList (sdbActivities db))
    pure (sortOn fst (documents cx (rights written)), warnings cx)
  where
    cx = context units db

-- | What every line is written against.
data Context = Context
    { cxDb :: !SimpleDatabase
    , cxFlows :: !(M.Map UUID OutFlow)
    , cxShared :: !(S.Set UUID)
    , cxUnits :: !UnitConfig
    , cxPlaces :: !(M.Map Text Text)
    -- ^ Each location code to the one written for every code differing from it only by case.
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
context units db = Context{cxDb = db, cxFlows = flows, cxShared = sharedActivityUUIDs db, cxUnits = units, cxPlaces = places}
  where
    places =
        M.fromList
            [ (code, written)
            | (_, codes) <- collisions [(T.toLower code, (n, code)) | (code, n) <- M.toList (locationUses db)]
            , let written = snd (NE.last (NE.sort codes))
            , (_, code) <- NE.toList codes
            ]

    -- A flow in two tables is written once, as the first names it.
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
    , lnInput :: !Bool
    , lnReference :: !Bool
    , lnAvoided :: !Bool
    , lnProvider :: !(Maybe UUID)
    , lnLocation :: !Text
    , lnComment :: !(Maybe Text)
    }

-- | A process as written: its identifier, the activity, and its lines.
data Written = Written
    { wrId :: !UUID
    , wrActivity :: !Activity
    , wrLines :: ![Line]
    }

-- | Why a line cannot be written, each naming it.
data Refusal
    = NoFlow !Text
    | OtherUnit !Text
    | SlashInSub !Text
    | TreatmentAbsent !Text
    | PositiveWasteReference !Text
    | NotFinite !Text

{- | Refuse a database the package would carry wrong, naming every line and
why, grouped by why.
-}
checkOlcaPackageExportable :: UnitConfig -> SimpleDatabase -> Either Text ()
checkOlcaPackageExportable units db = case refusals of
    [] -> Right ()
    _ -> Left (describeRefusals refusals)
  where
    cx = context units db
    refusals =
        concat (lefts (map (uncurry (processLines cx)) (M.toAscList (sdbActivities db))))
            <> [SlashInSub (ofName f) | f <- M.elems (cxFlows cx), OutElementary (Just c) <- [ofType f], maybe False (T.isInfixOf "/") (compartmentSub c)]

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
processLines cx key act = case (lefts lines', rights lines') of
    ([], ok) -> Right Written{wrId = ilcdProcessUUID (cxShared cx) key, wrActivity = act, wrLines = ok}
    (bad, _) -> Left bad
  where
    lines' = map (lineOf cx act) (exchanges act)

lineOf :: Context -> Activity -> Exchange -> Either Refusal Line
lineOf cx act ex = do
    flow <- maybe (Left (NoFlow named)) Right (M.lookup (exchangeFlowId ex) (cxFlows cx))
    whenLeft (isNaN (exchangeAmount ex) || isInfinite (exchangeAmount ex)) (NotFinite named)
    amount <- maybe (Left (OtherUnit named)) Right (inUnitOf flow)
    let line input a = Line{lnFlow = flow, lnAmount = a, lnInput = input, lnReference = False, lnAvoided = False, lnProvider = provider, lnLocation = place cx (exchangeLocation ex), lnComment = exchangeComment ex}
        -- A line sent to a treatment, by the demand it puts on it: written as the waste output openLCA reads it as.
        sent demand = pure (line False (demand * treatmentSign))
    case (ex, ofType flow) of
        {- The engine signs an elementary amount by the line's kind, openLCA by
        its side, and a reader takes the kind from the compartment. Written on
        the compartment's side, the amount reads back with its sign, and the
        matrix takes the sign alone. -}
        (BiosphereExchange{}, OutElementary comp) ->
            pure $ case directionOf comp of
                Emission -> (line (amount < 0) (abs amount)){lnProvider = Nothing}
                Resource -> (line (amount >= 0) (abs amount)){lnProvider = Nothing}
        (BiosphereExchange{}, _) -> Left (NoFlow named)
        (_, OutElementary _) -> Left (NoFlow named)
        (TechnosphereExchange{techRole = role}, OutProduct) -> pure $ case role of
            ReferenceProduct -> (line False amount){lnReference = True, lnProvider = Nothing}
            Coproduct -> (line False amount){lnProvider = Nothing}
            Input -> line True amount
            AvoidedProduct -> (line True amount){lnAvoided = True}
            -- Never: a reference input makes its flow a waste flow.
            ReferenceInput -> (line True amount){lnReference = True, lnProvider = Nothing}
        (TechnosphereExchange{techRole = role}, OutWaste) -> case role of
            ReferenceProduct
                | amount < 0 -> pure (line True (negate amount)){lnReference = True, lnProvider = Nothing}
                | otherwise -> Left (PositiveWasteReference named)
            -- Read back as a reference output of the opposite amount, which turns the column's sign.
            ReferenceInput -> pure (line True amount){lnReference = True, lnProvider = Nothing}
            Coproduct -> pure (line True (negate amount)){lnProvider = Nothing}
            Input -> sent amount
            AvoidedProduct -> sent (negate amount)
        (WasteExchange{waIsInput = True}, _)
            | isJust treatment -> sent amount
            | otherwise -> Left (TreatmentAbsent named)
        (WasteExchange{}, _) -> sent (negate amount)
  where
    unitNamed u = unitName <$> M.lookup u (sdbUnits (cxDb cx))

    -- The amount in the flow's unit, the only one its unit group holds here.
    inUnitOf flow
        | exchangeUnitId ex == ofUnit flow = Just (exchangeAmount ex)
        | otherwise = do
            from <- unitNamed (exchangeUnitId ex)
            to <- unitNamed (ofUnit flow)
            convertUnit (cxUnits cx) from to (exchangeAmount ex)

    named = activityName act <> " · " <> maybe (UUID.toText (exchangeFlowId ex)) ofName (M.lookup (exchangeFlowId ex) (cxFlows cx))

    activities = sdbActivities (cxDb cx)

    supplierKey = (,exchangeFlowId ex) <$> exchangeActivityLinkId ex

    treatment = supplierKey >>= \k -> (,) k <$> M.lookup k activities

    -- The written process of the supplier, else the identifier as the source stated it.
    provider = case supplierKey of
        Just k | M.member k activities -> Just (ilcdProcessUUID (cxShared cx) k)
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

{- | The direction a reader gives an elementary line, from its flow's
compartment: a resource is taken, anything else emitted.
-}
directionOf :: Maybe Compartment -> BioDirection
directionOf comp = case compartmentName <$> comp of
    Just NaturalResource -> Resource
    Just Air -> Emission
    Just Water -> Emission
    Just Soil -> Emission
    Just InventoryIndicator -> Emission
    Just Economic -> Emission
    Just Waste -> Emission
    Just Social -> Emission
    Nothing -> Emission

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
activity become processes of their own, a coproduct left unallocated is
read with the whole inventory, an input naming no supplier is linked by
openLCA to a producer of the package, and formulas are written as their
values.
-}
warnings :: Context -> [Text]
warnings cx = splitWarnings (cxDb cx) <> mapMaybe counted kinds
  where
    acts = M.elems (sdbActivities (cxDb cx))
    counted (what, named) = case named of
        [] -> Nothing
        _ -> Just (tshow (length named) <> " " <> what <> foldMap ("\n  " <>) (take 10 named))
    kinds =
        [ ("processes keep a product they did not allocate; openLCA reads it with the whole inventory:", [activityName a | a <- acts, any isCoproduct (exchanges a)])
        , ("inputs name no supplier, and openLCA will link them to a producer of the package:", unlinkedProduced)
        , ("processes have formulas, written as the values they computed:", [activityName a | a <- acts, not (M.null (activityParamExprs a))])
        , ("processes state an elementary flow against its compartment, written on its side with the same inventory:", S.toList (S.fromList against))
        , ("location codes differ from another only by case, one location to openLCA, and are written as it:", [code <> " as " <> written | (code, written) <- M.toList (cxPlaces cx), code /= written])
        ]
    isCoproduct ex = case ex of
        TechnosphereExchange{techRole = Coproduct} -> True
        _ -> False
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
            <> [("locations/" <> uuidPath (locationId c), encode (locationDoc c)) | c <- S.toList (S.fromList (map (place cx) (M.keys (locationUses (cxDb cx)))))]
            <> [("processes/" <> uuidPath (wrId w), encode (processDoc cx w)) | w <- written]
  where
    units = M.elems (sdbUnits (cxDb cx))
    uuidPath u = UUID.toString u <> ".json"

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
  where
    act = wrActivity w
    numbered = zip [1 :: Int ..] (wrLines w)

exchangeDoc :: Context -> Int -> Line -> Encoding
exchangeDoc cx internalId ln =
    pairs $
        "@type" .= ("Exchange" :: Text)
            <> "internalId" .= internalId
            <> pair "flow" (ref "Flow" (ofId flow) (ofName flow))
            <> "amount" .= lnAmount ln
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

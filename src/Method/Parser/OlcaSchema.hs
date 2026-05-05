{-# LANGUAGE OverloadedStrings #-}

{- | Parser for the openLCA JSON-LD schema (https://greendelta.github.io/olca-schema)
restricted to 'ImpactCategory' / 'ImpactFactor' — the shape that encodes a
regionalized LCIA method.

Top-level shape:

> {
>   "@context": "http://greendelta.github.io/olca-schema/context.jsonld",
>   "@type": "ImpactCategory",
>   "@id": "3a08605b-...",
>   "name": "Biodiversity loss potential",
>   "referenceUnitName": "m2*year",
>   "impactFactors": [
>     {
>       "@type": "ImpactFactor",
>       "value": 22.15,
>       "flow": {"@type": "Flow", "@id": "0305...", "name": "Occupation, agriculture",
>                "flowType": "ELEMENTARY_FLOW"},
>       "location": {"@type": "Location", "@id": "...", "name": "France", "code": "FR"},
>       "unit": {"@type": "Unit", "@id": "...", "name": "m2*year"}
>     }, ...
>   ]
> }

Each 'ImpactFactor' becomes a single 'MethodCF':

* @flow.\@id@ → 'mcfFlowRef' (UUID — deterministic match against DB flow UUIDs)
* @flow.name@ → 'mcfFlowName' (kept for display + name-based fallback matching)
* @flow.cas@ → 'mcfCAS' (when present)
* @flow.flowType == \"ELEMENTARY_FLOW\"@ controls whether we carry a compartment.
* @location.code@ (preferred) or @location.name@ → 'mcfConsumerLocation'.
  Absent location → 'Nothing' = universal CF (broadcast over all locations).
* @value@ → 'mcfValue'
* @unit.name@ → 'mcfUnit'

Optional fields ignored at this stage but trivial to wire later:

* @formula@ — parametric CFs (we'd need an evaluator)
* @uncertainty@ — distribution → Monte Carlo support
* @flowProperty@ — we currently rely on @unit@ only

Other openLCA top-level types ('Process', 'Flow', 'ImpactMethod', etc.) are not
recognized by 'isOlcaImpactCategoryJson'; the auto-detection in
'Database.Manager' only routes @ImpactCategory@ files into this parser.
-}
module Method.Parser.OlcaSchema (
    parseOlcaImpactCategoryBytes,
    isOlcaImpactCategoryJson,
) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import Data.Word (Word8)

import Method.Types

-- | Quick structural sniff: a JSON object with @\"@type\": \"ImpactCategory\"@.
-- Used by 'Database.Manager' to dispatch @.json@ files to this parser.
isOlcaImpactCategoryJson :: BS.ByteString -> Bool
isOlcaImpactCategoryJson bytes = case eitherDecodeStrict bytes of
    Right (Object o) -> case KM.lookup "@type" o of
        Just (String "ImpactCategory") -> True
        _ -> False
    _ -> False

-- | Parse one openLCA @ImpactCategory@ JSON-LD document into a 'Method'.
parseOlcaImpactCategoryBytes :: BS.ByteString -> Either String Method
parseOlcaImpactCategoryBytes bytes = do
    val <- eitherDecodeStrict bytes
    case val of
        Object o -> do
            assertType o "ImpactCategory"
            name <- requireText o "name"
            let mid = parseUuidField o "@id"
                description = lookupText o "description"
                unit = fromMaybe "" (lookupText o "referenceUnitName")
                factors = case KM.lookup "impactFactors" o of
                    Just (Array v) -> mapMaybe parseImpactFactor (V.toList v)
                    _ -> []
            Right
                Method
                    { methodId = fromMaybe (synthMethodId name) mid
                    , methodName = name
                    , methodDescription = description
                    , methodUnit = unit
                    , methodCategory = name
                    , methodMethodology = Just "openLCA JSON-LD"
                    , methodFactors = factors
                    }
        _ -> Left "openLCA ImpactCategory: expected a top-level JSON object"

parseImpactFactor :: Value -> Maybe MethodCF
parseImpactFactor (Object o) = do
    value <- numberField o "value"
    flow <- objectField o "flow"
    let flowName = fromMaybe "" (lookupText flow "name")
        flowUuid = fromMaybe (synthFlowId flowName) (parseUuidField flow "@id")
        flowCas = lookupText flow "cas"
        unitName = case objectField o "unit" of
            Just u -> fromMaybe "" (lookupText u "name")
            Nothing -> ""
        loc = case objectField o "location" of
            Just l -> case lookupText l "code" of
                Just c -> Just c
                Nothing -> lookupText l "name"
            Nothing -> Nothing
        direction = directionFromFlow o
    -- A factor without a flow name is unmatchable; drop it. (UUID alone could
    -- in theory match by id, but in practice the name carries the matching
    -- signal too and is required by every fallback strategy.)
    if T.null flowName
        then Nothing
        else
            Just
                MethodCF
                    { mcfFlowRef = flowUuid
                    , mcfFlowName = flowName
                    , mcfDirection = direction
                    , mcfValue = value
                    -- Compartment can live on the openLCA Flow object via its
                    -- 'category' field (a Ref[Category] hierarchy). Wiring that
                    -- in is straightforward but not needed yet — UUID-based
                    -- matching against the DB doesn't use the compartment.
                    , mcfCompartment = Nothing
                    , mcfCAS = flowCas
                    , mcfUnit = unitName
                    , mcfConsumerLocation = loc
                    }
parseImpactFactor _ = Nothing

-- | Direction is carried by 'ImpactFactor.direction' (Direction enum) when
-- present, otherwise default to 'Output' since the vast majority of
-- environmental CFs are emissions.
directionFromFlow :: KM.KeyMap Value -> FlowDirection
directionFromFlow o = case lookupText o "direction" of
    Just "INPUT" -> Input
    Just "OUTPUT" -> Output
    _ -> Output

-- ---------------------------------------------------------------------------
-- Aeson helpers
-- ---------------------------------------------------------------------------

assertType :: KM.KeyMap Value -> Text -> Either String ()
assertType o expected = case KM.lookup "@type" o of
    Just (String t)
        | t == expected -> Right ()
        | otherwise -> Left ("openLCA: expected @type='" <> T.unpack expected <> "', got '" <> T.unpack t <> "'")
    _ -> Left ("openLCA: missing @type field (expected '" <> T.unpack expected <> "')")

requireText :: KM.KeyMap Value -> Text -> Either String Text
requireText o k = case KM.lookup (K.fromText k) o of
    Just (String t) -> Right t
    Just _ -> Left ("openLCA: field '" <> T.unpack k <> "' is not a string")
    Nothing -> Left ("openLCA: missing required field '" <> T.unpack k <> "'")

lookupText :: KM.KeyMap Value -> Text -> Maybe Text
lookupText o k = case KM.lookup (K.fromText k) o of
    Just (String t) | not (T.null t) -> Just t
    _ -> Nothing

objectField :: KM.KeyMap Value -> Text -> Maybe (KM.KeyMap Value)
objectField o k = case KM.lookup (K.fromText k) o of
    Just (Object inner) -> Just inner
    _ -> Nothing

numberField :: KM.KeyMap Value -> Text -> Maybe Double
numberField o k = case KM.lookup (K.fromText k) o of
    Just (Number n) -> Just (toRealFloat n)
    _ -> Nothing

parseUuidField :: KM.KeyMap Value -> Text -> Maybe UUID
parseUuidField o k = lookupText o k >>= UUID.fromText

textKey :: Text -> [Word8]
textKey = BS.unpack . TE.encodeUtf8

-- | Stable, deterministic UUID for a method that didn't carry an @\@id@ field.
-- Falls back to a UUIDv5 derived from the method name.
synthMethodId :: Text -> UUID
synthMethodId name =
    UUID5.generateNamed UUID5.namespaceURL (textKey ("volca:olca-method:" <> name))

-- | Stable UUID for an unidentified flow (no @\@id@). Same idea.
synthFlowId :: Text -> UUID
synthFlowId name =
    UUID5.generateNamed UUID5.namespaceURL (textKey ("volca:olca-flow:" <> name))

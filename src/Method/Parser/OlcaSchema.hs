{-# LANGUAGE OverloadedStrings #-}

{- | Parser for the openLCA JSON-LD schema (https://greendelta.github.io/olca-schema)
restricted to 'ImpactCategory' / 'ImpactFactor' – the shape that encodes a
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

The document-level @category@ (the impact category's group label, a plain
string in the olca schema) becomes 'methodCategory', falling back to the
name when absent.

Each 'ImpactFactor' becomes a single 'MethodCF':

* @flow.\@id@ → 'mcfFlowRef' (UUID – deterministic match against DB flow UUIDs)
* @flow.name@ → 'mcfFlowName' (kept for display + name-based fallback matching)
* @flow.cas@ → 'mcfCAS' (when present)
* @flow.flowType == \"ELEMENTARY_FLOW\"@ controls whether we carry a compartment.
* @location.code@ (preferred) or @location.name@ → 'mcfConsumerLocation'.
  Absent location → 'Nothing' = universal CF (broadcast over all locations).
* @value@ → 'mcfValue'
* @unit.name@ → 'mcfUnit'
* 'mcfDirection' comes from the factor's own @direction@ field when present,
  else the flow's category path, else the document's – see 'factorDirection'.

Optional fields ignored at this stage but trivial to wire later:

* @formula@ – parametric CFs (we'd need an evaluator)
* @uncertainty@ – distribution → Monte Carlo support
* @flowProperty@ – we currently rely on @unit@ only

Other openLCA top-level types ('Process', 'Flow', 'ImpactMethod', etc.) are not
recognized by 'isOlcaImpactCategoryJson'; the auto-detection in
'Database.Manager' only routes @ImpactCategory@ files into this parser.
-}
module Method.Parser.OlcaSchema (
    parseOlcaImpactCategoryBytes,
    isOlcaImpactCategoryJson,
    OlcaImpactMethod (..),
    parseOlcaImpactMethodBytes,
    choosePackageMethod,
) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (find, sort)
import qualified Data.Map.Strict as M
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

import Method.SimaProScoring (NormWeightSet (..))
import Method.Types

{- | Quick structural sniff: a JSON object with @\"@type\": \"ImpactCategory\"@.
Used by 'Database.Manager' to dispatch @.json@ files to this parser.
-}
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
                unit = fromMaybe "" (lookupText o "referenceUnitName" <|> lookupText o "refUnit")
                docDirection = parseDirection =<< lookupText o "direction"
                factors = case KM.lookup "impactFactors" o of
                    Just (Array v) -> mapMaybe (parseImpactFactor docDirection) (V.toList v)
                    _ -> []
            Right
                Method
                    { methodId = fromMaybe (synthMethodId name) mid
                    , methodName = name
                    , methodDescription = description
                    , methodUnit = unit
                    , methodCategory = fromMaybe name (lookupText o "category")
                    , methodMethodology = Just "openLCA JSON-LD"
                    , methodFactors = factors
                    }
        _ -> Left "openLCA ImpactCategory: expected a top-level JSON object"

{- | One method of an openLCA package: its name, the categories it groups, by
the @\@id@ each category's file is named after, and its normalization and
weighting sets.
-}
data OlcaImpactMethod = OlcaImpactMethod
    { oimName :: !Text
    , oimCategories :: ![Text]
    , oimNWSets :: ![NormWeightSet]
    }
    deriving (Eq, Show)

{- | Parse one openLCA @ImpactMethod@ document.

openLCA divides a category's result by its normalisation factor, then
multiplies it by its weight. 'NormWeightSet' holds the normalisation as a
multiplier, as SimaPro writes it, so the factor is inverted here; a category
with no factor, or a zero one, is left out of the normalisation rather than
divided by zero.
-}
parseOlcaImpactMethodBytes :: BS.ByteString -> Either String OlcaImpactMethod
parseOlcaImpactMethodBytes bytes = do
    val <- eitherDecodeStrict bytes
    case val of
        Object o -> do
            assertType o "ImpactMethod"
            name <- requireText o "name"
            nwSets <- traverse normWeightSet (objectsIn o "nwSets")
            Right
                OlcaImpactMethod
                    { oimName = name
                    , oimCategories = mapMaybe (`lookupText` "@id") (objectsIn o "impactCategories")
                    , oimNWSets = nwSets
                    }
        _ -> Left "openLCA ImpactMethod: expected a top-level JSON object"
  where
    normWeightSet :: KM.KeyMap Value -> Either String NormWeightSet
    normWeightSet nw = do
        -- Scores are reported by set name, and a package may leave a set unnamed.
        let setName = fromMaybe "" (lookupText nw "name" <|> lookupText nw "@id")
        {- A factor is matched to its category by name; one without a name would
        leave its category out of the single score without a word. -}
        factors <- traverse (factorOf setName) (objectsIn nw "factors")
        Right
            NormWeightSet
                { nwName = setName
                , nwNormalization = M.fromList [(c, 1 / n) | (c, f) <- factors, Just n <- [numberField f "normalisationFactor"], n /= 0]
                , nwWeighting = M.fromList [(c, w) | (c, f) <- factors, Just w <- [numberField f "weightingFactor"]]
                }
    factorOf setName f = case objectField f "impactCategory" >>= (`lookupText` "name") of
        Just c -> Right (c, f)
        Nothing -> Left ("openLCA ImpactMethod: a factor of set \"" <> T.unpack setName <> "\" names no impact category")

{- | The method of a package a collection reads: the one it names, or the
package's only one. 'Left' lists the methods the package holds, which is what
a reader needs to name one.
-}
choosePackageMethod :: Maybe Text -> [OlcaImpactMethod] -> Either Text OlcaImpactMethod
choosePackageMethod wanted methods = case (wanted, methods) of
    (_, []) -> Left "the package holds no method"
    (Nothing, [only]) -> Right only
    (Just name, _) | Just m <- find ((== name) . oimName) methods -> Right m
    (Just name, _) -> Left ("the package holds no method named \"" <> name <> "\"; it holds " <> held)
    (Nothing, _) ->
        Left
            ( "the package holds several methods and none is named (package-method in the configuration, package_method in an upload's meta.toml); it holds "
                <> held
            )
  where
    held = T.intercalate ", " ["\"" <> n <> "\"" | n <- sort (map oimName methods)]

-- | The objects of an array field; anything else in it, or no such field, gives none.
objectsIn :: KM.KeyMap Value -> Text -> [KM.KeyMap Value]
objectsIn o k = case KM.lookup (K.fromText k) o of
    Just (Array v) -> [inner | Object inner <- V.toList v]
    _ -> []

parseImpactFactor :: Maybe FlowDirection -> Value -> Maybe MethodCF
parseImpactFactor docDirection (Object o) = do
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
        direction = factorDirection docDirection o flow
        compartment = parseCompartment flow
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
                    , mcfCompartment = compartment
                    , mcfCAS = flowCas
                    , mcfUnit = unitName
                    , mcfConsumerLocation = loc
                    }
parseImpactFactor _ _ = Nothing

{- | Turn the flow's category path into a 'Compartment'. The path is
slash-separated: after dropping the @Elementary flows@ root that openLCA's
category tree prepends (a tree artifact, not a compartment), the first
segment is the medium ('resource', 'air', 'water', …) and the rest is the
subcompartment ('land', 'urban air close to ground', …). Without this,
VoLCA's matcher cannot disambiguate DB flows that share a name across
compartments (e.g. Agribalyse has 3 flows literally named
@"Occupation, annual crop"@ in @resource@, @resource/land@, and
@resource/biotic@; only the @resource/land@ one is what activities emit).
-}
parseCompartment :: KM.KeyMap Value -> Maybe Compartment
parseCompartment flow = do
    catName <- categoryPath flow
    -- A path that was only the tree root leaves no segments – no compartment.
    case dropTreeRoot (T.splitOn "/" catName) of
        [] -> Nothing
        (med0 : rest) ->
            let med = T.strip med0
                sub = T.strip (T.intercalate "/" rest)
             in if T.null med
                    then Nothing
                    else Just (Compartment med sub "")

-- | Drop the @Elementary flows@ root of openLCA's category tree.
dropTreeRoot :: [Text] -> [Text]
dropTreeRoot (root : rest)
    | T.toCaseFold (T.strip root) == "elementary flows" = rest
dropTreeRoot segs = segs

{- | The flow's category path. The olca schema carries it as a plain string
on the flow Ref (@"Elementary flows/Emission to air/unspecified"@) – the
form a genuine openLCA export uses – while VoLCA's own exports write a
Category Ref object whose @name@ holds the same path.
-}
categoryPath :: KM.KeyMap Value -> Maybe Text
categoryPath flow =
    lookupText flow "category"
        <|> (objectField flow "category" >>= (`lookupText` "name"))

{- | Direction of one factor, most specific signal first:

1. the factor's own @direction@ field – not in the olca schema (openLCA
   ignores it on import) but written by VoLCA's own exporter, so a VoLCA
   archive round-trips its directions exactly;
2. the flow's category path – openLCA files carry no per-factor direction
   at all, but the path names the boundary side: a resource segment
   (@resource/…@, @Raw materials@, @Land use@) means Input – the same
   spellings the columnar-CSV parser reads – and an emission segment
   (@Emission to air/…@) means Output. A CF resolved against the wrong
   synonym view silently loses direction-specific bridges;
3. the document-level @ImpactCategory.direction@ (olca schema v2) – the
   category's overall orientation, for flows whose category path says
   nothing either way;
4. 'Output', since the vast majority of environmental CFs are emissions.
-}
factorDirection :: Maybe FlowDirection -> KM.KeyMap Value -> KM.KeyMap Value -> FlowDirection
factorDirection docDirection factor flow =
    fromMaybe Output (explicit <|> fromCategory <|> docDirection)
  where
    explicit = parseDirection =<< lookupText factor "direction"
    fromCategory = pathDirection =<< categoryPath flow

-- | The direction a category path implies; 'Nothing' when it says nothing.
pathDirection :: Text -> Maybe FlowDirection
pathDirection path
    | any resourceSegment segments = Just Input
    | any emissionSegment segments = Just Output
    | otherwise = Nothing
  where
    segments = map (T.toCaseFold . T.strip) (T.splitOn "/" path)
    resourceSegment s =
        s `elem` ["resource", "resources", "natural resource"]
            || "raw" `T.isPrefixOf` s
            || "resources " `T.isPrefixOf` s
            || "land " `T.isPrefixOf` s
    emissionSegment s = "emission" `T.isPrefixOf` s

-- | The olca @Direction@ enum, case-insensitively; 'Nothing' on anything else.
parseDirection :: Text -> Maybe FlowDirection
parseDirection t = case T.toCaseFold (T.strip t) of
    "input" -> Just Input
    "output" -> Just Output
    _ -> Nothing

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

{- | Stable, deterministic UUID for a method that didn't carry an @\@id@ field.
Falls back to a UUIDv5 derived from the method name.
-}
synthMethodId :: Text -> UUID
synthMethodId name =
    UUID5.generateNamed UUID5.namespaceURL (textKey ("volca:olca-method:" <> name))

-- | Stable UUID for an unidentified flow (no @\@id@). Same idea.
synthFlowId :: Text -> UUID
synthFlowId name =
    UUID5.generateNamed UUID5.namespaceURL (textKey ("volca:olca-flow:" <> name))

{-# LANGUAGE OverloadedStrings #-}

{- | Serialize a 'MethodCollection' to an ILCD LCIA-method package – the inverse
of "Method.Parser" (method XML) and "Method.FlowResolver" (flow XML).

The output is a deterministic ILCD directory tree, packed into a zip by
"Database.Export":

@
  lciamethods\/\<methodId\>.xml   one LCIAMethodDataSet per method
  flows\/\<flowRef\>.xml          one flow dataset per distinct flow UUID
@

"Method.Parser" reads a method's UUID, name, methodology, impact category,
description, reference unit and its factors (flow ref, direction, value,
per-factor location). Each factor's flow name, compartment and CAS are /not/ in
the method file – the reader recovers them from the sibling @flows\/@ directory
via "Method.FlowResolver", exactly as it does for a native EF package. So we
emit both: the method files carry the factor values, and one flow file per
distinct flow UUID carries that flow's name, compartment and CAS. This is why
the collection round-trips its CAS numbers, which have no home in the method
file itself.

Determinism is the contract, matching the other method writers:

* factors within a method are emitted in a fixed key order;
* one flow file per UUID (the export refuses a collection where one UUID carries
  two different flow definitions – 'checkIlcdMethodExportable');
* every 'Double' is formatted through 'ILCD.Writer.formatDouble', the exact
  inverse of the correctly-rounded 'Amount.readAmount' the reader now uses, so
  every finite factor value round-trips and a re-export is byte-identical.

What ILCD's method profile cannot carry, 'lossWarnings' reports rather than
dropping in silence: a per-factor flow unit (the format stores one reference
unit per method), and the collection-level damage categories, normalization /
weighting sets and formula scoring sets. Methodology and description /do/
round-trip natively, unlike the CSV and openLCA writers.

The compartment qualifier folds into the subcompartment, as in the SimaPro and
openLCA writers – ILCD's flow categorization has no separate qualifier axis.
-}
module Method.WriterILCD (
    serializeIlcdMethodEntries,
    checkIlcdMethodExportable,

    -- * Pure helpers (exported for testing)
    methodXml,
    flowXml,
    FlowDef (..),
    compartmentCategories,
    projectCompartment,
    lossWarnings,
) where

import qualified Data.ByteString as BS
import Data.Indexing (collisions)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import ILCD.Writer (escapeXml, formatDouble)
import Method.FlowResolver (parseCompartment)
import Method.Types
import SubstanceRegistry (normalizeCAS)

--------------------------------------------------------------------------------
-- Entry points
--------------------------------------------------------------------------------

{- | The named ILCD files (method + flow XML) for a collection, plus the
representation-loss warnings, or a guard error if some factor would be silently
corrupted on re-import. "Database.Export" packs the entries into a zip.
-}
serializeIlcdMethodEntries :: MethodCollection -> Either Text ([(FilePath, BS.ByteString)], [Text])
serializeIlcdMethodEntries mc = do
    checkIlcdMethodExportable mc
    let flows = flowMap (mcMethods mc)
        methodEntries = [(methodPath m, render (methodXml m)) | m <- mcMethods mc]
        flowEntries = [(flowPath u, render (flowXml u fd)) | (u, fd) <- M.toList flows]
    pure (sortOn fst (methodEntries ++ flowEntries), lossWarnings mc)

methodPath :: Method -> FilePath
methodPath m = "lciamethods/" <> UUID.toString (methodId m) <> ".xml"

flowPath :: UUID -> FilePath
flowPath u = "flows/" <> UUID.toString u <> ".xml"

--------------------------------------------------------------------------------
-- Guards: refuse anything a re-import would silently corrupt
--------------------------------------------------------------------------------

{- | Reject a collection whose ILCD encoding would not round-trip: an empty
collection, two methods sharing a UUID (their files would overwrite each other),
a method with no name (the reader rejects it and drops all its factors), a
method with no reference unit or impact category (the reader fills the blank
with @"unknown"@), a non-finite or nameless factor (dropped or read as 0 on
re-import), a compartment the flow categorization cannot represent exactly, or
one flow UUID carrying two different flow definitions (one flow file cannot
serve both).
-}
checkIlcdMethodExportable :: MethodCollection -> Either Text ()
checkIlcdMethodExportable mc = do
    let ms = mcMethods mc
    if null ms then Left "The method collection has no methods to export." else Right ()
    firstError (map noDuplicateId (duplicateIds ms))
    firstError (concatMap methodErrors ms)
    firstError (map flowConflict (M.toList (flowDefs ms)))
  where
    duplicateIds :: [Method] -> [(UUID, NonEmpty Text)]
    duplicateIds ms = collisions [(methodId m, methodName m) | m <- ms]

    noDuplicateId :: (UUID, NonEmpty Text) -> Maybe Text
    noDuplicateId (u, names) =
        Just $
            "Two methods share the id "
                <> UUID.toText u
                <> " ("
                <> T.intercalate ", " (NE.toList names)
                <> "); their ILCD files would overwrite each other."

methodErrors :: Method -> [Maybe Text]
methodErrors m =
    emptyName : emptyUnit : emptyCategory : concatMap (factorErrors (methodName m)) (methodFactors m)
  where
    emptyName
        | T.null (T.strip (methodName m)) =
            Just $ "A method (" <> UUID.toText (methodId m) <> ") has no name; ILCD import would reject it and drop all its factors."
        | otherwise = Nothing
    emptyUnit
        | T.null (T.strip (methodUnit m)) =
            Just $ "Method '" <> methodName m <> "' has no reference unit; ILCD import would read it back as \"unknown\"."
        | otherwise = Nothing
    emptyCategory
        | T.null (T.strip (methodCategory m)) =
            Just $ "Method '" <> methodName m <> "' has no impact category; ILCD import would read it back as \"unknown\"."
        | otherwise = Nothing

factorErrors :: Text -> MethodCF -> [Maybe Text]
factorErrors cat cf =
    [ nonFinite
    , emptyFlowName
    , badCompartment
    ]
  where
    flow = mcfFlowName cf
    nonFinite
        | isNaN v || isInfinite v =
            Just $ "Non-finite characterization factor for '" <> flow <> "' in '" <> cat <> "'."
        | otherwise = Nothing
      where
        v = mcfValue cf
    emptyFlowName
        | T.null (T.strip flow) =
            Just $ "A characterization factor in '" <> cat <> "' has no flow name; ILCD import would drop it."
        | otherwise = Nothing
    badCompartment = case mcfCompartment cf of
        Nothing -> Nothing
        Just c
            | parseCompartment (compartmentCategories c) == Just (projectCompartment c) -> Nothing
            | otherwise ->
                Just $
                    "Compartment "
                        <> T.pack (show c)
                        <> " (flow '"
                        <> flow
                        <> "') is not representable in the ILCD flow categorization."

flowConflict :: (UUID, [FlowDef]) -> Maybe Text
flowConflict (u, defs) = case dedupe defs of
    (_ : _ : _) ->
        Just $
            "Flow "
                <> UUID.toText u
                <> " appears with different names, compartments or CAS numbers; one ILCD flow file cannot represent them all."
    _ -> Nothing

-- | First 'Left' in a list of possible errors, or '()' if all are 'Nothing'.
firstError :: [Maybe Text] -> Either Text ()
firstError = maybe (Right ()) Left . listToMaybe . catMaybes

--------------------------------------------------------------------------------
-- Flow catalog
--------------------------------------------------------------------------------

-- | The identity one @flows\/\<uuid\>.xml@ file carries: name, compartment, CAS.
data FlowDef = FlowDef
    { fdName :: !Text
    , fdCompartment :: !(Maybe Compartment)
    , fdCAS :: !(Maybe Text)
    }
    deriving (Eq, Show)

flowDefOf :: MethodCF -> FlowDef
flowDefOf cf = FlowDef (mcfFlowName cf) (mcfCompartment cf) (mcfCAS cf)

-- | All distinct flow definitions seen for each flow UUID (for conflict checks).
flowDefs :: [Method] -> M.Map UUID [FlowDef]
flowDefs ms =
    M.map dedupe $
        M.fromListWith (++) [(mcfFlowRef cf, [flowDefOf cf]) | m <- ms, cf <- methodFactors m]

-- | One flow definition per UUID (safe once 'checkIlcdMethodExportable' passed).
flowMap :: [Method] -> M.Map UUID FlowDef
flowMap ms = M.fromList [(mcfFlowRef cf, flowDefOf cf) | m <- ms, cf <- methodFactors m]

dedupe :: (Eq a) => [a] -> [a]
dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

--------------------------------------------------------------------------------
-- Method XML
--------------------------------------------------------------------------------

-- | Render one LCIAMethodDataSet with its factors in a fixed order.
methodXml :: Method -> [Text]
methodXml m =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<LCIAMethodDataSet xmlns=\"http://lca.jrc.it/ILCD/LCIAMethod\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <LCIAMethodInformation>"
    , "    <dataSetInformation>"
    , tag "common:UUID" (UUID.toText (methodId m))
    , tag "common:name" (methodName m)
    ]
        ++ maybeToList (fmap (tag "methodology") (methodMethodology m))
        ++ [ tag "impactCategory" (methodCategory m)
           ]
        ++ maybeToList (fmap (tag "common:generalComment") (methodDescription m))
        ++ [ "    </dataSetInformation>"
           , "    <quantitativeReference>"
           , "      <referenceQuantity>"
           , tag "common:shortDescription" (methodUnit m)
           , "      </referenceQuantity>"
           , "    </quantitativeReference>"
           , "  </LCIAMethodInformation>"
           , "  <characterisationFactors>"
           ]
        ++ concatMap factorXml (sortOn factorKey (methodFactors m))
        ++ [ "  </characterisationFactors>"
           , "</LCIAMethodDataSet>"
           ]

-- | Total order on factors, so a re-export is byte-identical.
factorKey :: MethodCF -> (Text, Text, Bool, Double)
factorKey cf =
    ( UUID.toText (mcfFlowRef cf)
    , fromMaybe "" (mcfConsumerLocation cf)
    , mcfDirection cf == Input
    , mcfValue cf
    )

factorXml :: MethodCF -> [Text]
factorXml cf =
    [ "    <factor>"
    , "      <referenceToFlowDataSet refObjectId=\"" <> UUID.toText (mcfFlowRef cf) <> "\">"
    , tag "common:shortDescription" (mcfFlowName cf)
    , "      </referenceToFlowDataSet>"
    ]
        ++ maybeToList (fmap (tag "location") (mcfConsumerLocation cf))
        ++ [ tag "exchangeDirection" (directionText (mcfDirection cf))
           , tag "meanValue" (formatDouble (mcfValue cf))
           , "    </factor>"
           ]

directionText :: FlowDirection -> Text
directionText Input = "Input"
directionText Output = "Output"

--------------------------------------------------------------------------------
-- Flow XML
--------------------------------------------------------------------------------

{- | Render one flow dataset. "Method.FlowResolver" reads its UUID, baseName,
compartment categories and CAS to enrich the factors that reference it.
-}
flowXml :: UUID -> FlowDef -> [Text]
flowXml u fd =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <flowInformation>"
    , "    <dataSetInformation>"
    , tag "common:UUID" (UUID.toText u)
    , "      <name>"
    , tag "baseName" (fdName fd)
    , "      </name>"
    ]
        ++ compartmentBlock (fdCompartment fd)
        ++ casBlock (fdCAS fd)
        ++ [ "    </dataSetInformation>"
           , "  </flowInformation>"
           , "</flowDataSet>"
           ]

{- | Emit the CAS in the canonical form the reader normalizes to, so a source
with a non-canonical CAS (leading zeros, stray space) re-exports byte-stable.
-}
casBlock :: Maybe Text -> [Text]
casBlock (Just cas) | not (T.null (T.strip cas)) = [tag "CASNumber" (normalizeCAS cas)]
casBlock _ = []

compartmentBlock :: Maybe Compartment -> [Text]
compartmentBlock Nothing = []
compartmentBlock (Just c) =
    [ "      <classificationInformation>"
    , "        <common:elementaryFlowCategorization>"
    ]
        ++ zipWith categoryLine [0 :: Int ..] (compartmentCategories c)
        ++ [ "        </common:elementaryFlowCategorization>"
           , "      </classificationInformation>"
           ]
  where
    categoryLine lvl txt =
        "          <common:category level=\"" <> T.pack (show lvl) <> "\">" <> escapeXml txt <> "</common:category>"

{- | ILCD category levels whose 'Method.FlowResolver.parseCompartment' inverse
is the projected compartment. Level 0/1 name the medium, level 2 (when a
subcompartment exists) carries it. @air@/@water@/@soil@ use the "Emissions to …"
phrasing, @natural resource@ uses "Resources", any other medium is emitted
verbatim (and reads back lower-cased) – 'checkIlcdMethodExportable' refuses a
medium whose inverse would not match.
-}
compartmentCategories :: Compartment -> [Text]
compartmentCategories c =
    case projectCompartment c of
        Compartment medium sub _
            | medium == "natural resource" ->
                ["Resources", "Resources"] ++ ["Resources " <> sub | not (T.null sub)]
            | medium `elem` ["air", "water", "soil"] ->
                ["Emissions", "Emissions to " <> medium]
                    ++ ["Emissions to " <> medium <> ", " <> sub | not (T.null sub)]
            | otherwise ->
                ["Emissions", medium] ++ [sub | not (T.null sub)]

{- | The compartment the ILCD encoding round-trips to: the qualifier folded into
the subcompartment (ILCD has no qualifier axis), matching the SimaPro and
openLCA writers. A no-op for the real data, which carries no qualifiers.
-}
projectCompartment :: Compartment -> Compartment
projectCompartment (Compartment medium sub qual) =
    Compartment medium (if T.null qual then sub else sub <> "/" <> qual) ""

--------------------------------------------------------------------------------
-- Representation-loss warnings
--------------------------------------------------------------------------------

{- | What the ILCD method profile cannot carry, counted rather than dropped in
silence. Methodology and description are omitted – they round-trip natively.
-}
lossWarnings :: MethodCollection -> [Text]
lossWarnings mc =
    mapMaybe
        countWarning
        [ (perFactorUnits, "characterization factors carry a per-factor flow unit that the ILCD method format cannot store; re-import reads each method's reference unit")
        , (length (mcDamageCategories mc), "damage categories are not part of the ILCD method format and are omitted")
        , (length (mcNormWeightSets mc), "normalization/weighting sets are not part of the ILCD method format and are omitted")
        , (length (mcScoringSets mc), "formula scoring sets are not part of the ILCD method format and are omitted")
        ]
  where
    perFactorUnits =
        length [cf | m <- mcMethods mc, cf <- methodFactors m, mcfUnit cf /= methodUnit m]
    countWarning (n, what)
        | n <= 0 = Nothing
        | otherwise = Just $ T.pack (show n) <> " " <> what

--------------------------------------------------------------------------------
-- XML primitives
--------------------------------------------------------------------------------

{- | @\<tag\>escaped\<\/tag\>@ on one line. The text is stripped: the SAX reader
strips every element's text in its accumulator, so emitting the stripped form is
exactly what it reads back, which keeps a re-export byte-stable even when a
source value carried stray surrounding whitespace (a real BAFU CAS does).
Indentation is cosmetic (inter-tag whitespace is ignored); we keep a readable
two-space nesting.
-}
tag :: Text -> Text -> Text
tag name txt = "      <" <> name <> ">" <> escapeXml (T.strip txt) <> "</" <> name <> ">"

render :: [Text] -> BS.ByteString
render = TE.encodeUtf8 . (<> "\n") . T.intercalate "\n"

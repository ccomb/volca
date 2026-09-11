{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Serialize a VoLCA 'Database'/'SimpleDatabase' to an ILCD process-dataset
package — the inverse of "ILCD.Parser".

The output is a canonical, deterministic ILCD directory tree (and, optionally,
a zip of it) with four subdirectories:

@
  processes/      one XML per (activity, product) pair (see 'ilcdProcessUUID')
  flows/          one XML per tech/bio/waste flow
  flowproperties/ one XML per unit group (1:1 with units)
  unitgroups/     one XML per unit
@

Determinism is the contract:

* every Map/Set-derived list is sorted by key before emission;
* every 'Double' is formatted through one fixed formatter ('formatDouble');
* the only volatile field an ILCD reader/writer round-trip could disagree on
  — the export timestamp / generator string — is /omitted/ entirely unless a
  caller passes one explicitly via 'WriteOptions'. We never inject @now@, so
  @write (parse (write d)) == write d@ holds byte-for-byte.

What round-trips: process UUID, name, location, classifications, processType,
every exchange (flow ref, direction, amount, location, per-exchange comment), and the
full flow + unit catalog (names, CAS, biosphere compartment, flow type, the
flow→unit reference). These are exactly the fields "ILCD.Parser" reads back;
fields the parser drops (activity description, synonyms, params, pedigree)
are not representable in this ILCD profile and are not emitted.

The declared allocation fractions are the exception: the parser now reads
them, and this writer does not yet write them back, so a block exported here
loses the shares it was loaded with and its re-import is refused by the
allocation gate. Emitting @<allocations>@ is what closes that.

The flow→unit indirection mirrors the parser's resolution chain
@flow → flowProperty → unitGroup@: we emit one flowProperty and one unitGroup
per VoLCA unit, all sharing the unit's UUID, so the parser reconstructs the
same unit key.
-}
module ILCD.Writer (
    -- * Options
    WriteOptions (..),
    defaultWriteOptions,

    -- * Writing
    writeILCDDatabase,
    writeILCDArchive,
    ilcdFiles,
    checkILCDExportable,

    -- * Pure helpers (exported for testing)
    escapeXml,
    escapeXmlAttr,
    formatDouble,
    ilcdProcessUUID,
    sharedActivityUUIDs,
    splitWarnings,
    processXML,
    flowXML,
    flowPropertyXML,
    unitGroupXML,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (lefts)
import Data.Indexing (repeated)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, splitDirectories, (</>))

import Amount (readAmount)
import EcoSpold.Common (showFFloatTrim)
import Types
import Zip (zipFiles)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

{- | Knobs that pin or omit volatile metadata. Defaults omit everything
volatile, so a write→parse→write round-trip is byte-stable.
-}
data WriteOptions = WriteOptions
    { woTimestamp :: !(Maybe Text)
    {- ^ Pinned export timestamp, emitted as @<common:timeStamp>@. 'Nothing'
    omits the element entirely (the parser ignores it either way).
    -}
    , woGenerator :: !(Maybe Text)
    -- ^ Pinned generator / tool-version string. 'Nothing' omits it.
    }

-- | Omit all volatile metadata. The right default for reproducible exports.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions{woTimestamp = Nothing, woGenerator = Nothing}

--------------------------------------------------------------------------------
-- Process dataset identity
--------------------------------------------------------------------------------

-- | Namespace for the UUIDs this writer has to mint itself.
ilcdExportNamespace :: UUID
ilcdExportNamespace =
    UUID5.generateNamed UUID5.namespaceURL (BS.unpack (TE.encodeUtf8 "ilcd-export"))

-- | The activity UUIDs carried by more than one @(activity, product)@ entry.
sharedActivityUUIDs :: SimpleDatabase -> S.Set UUID
sharedActivityUUIDs db = S.fromList (repeated [actUUID | (actUUID, _) <- M.keys (sdbActivities db)])

{- | The @common:UUID@ — and the filename — of one exported process dataset.

ILCD keys a process by a single dataset UUID, one process per file. An activity
UUID shared by several @(activity, product)@ entries therefore cannot name them
all: a multi-output activity, or a name collision in the source format, would
write every product to the same file. Such an entry gets a UUID derived from its
pair instead; an unshared activity UUID passes through unchanged.

That condition is what makes the mapping a fixed point of @parse . write@: the
parser reads @common:UUID@ back as the activity UUID, and every re-imported
process is single-output, so a second export reproduces the first byte for byte.
Deriving unconditionally would not — @UUID5(UUID5(a,p),p) /= UUID5(a,p)@.
-}
ilcdProcessUUID :: S.Set UUID -> (UUID, UUID) -> UUID
ilcdProcessUUID sharedActUUIDs (actUUID, prodUUID)
    | actUUID `S.member` sharedActUUIDs =
        UUID5.generateNamed ilcdExportNamespace $
            BS.unpack (TE.encodeUtf8 ("process:" <> processRefText (ProcessRef actUUID prodUUID)))
    | otherwise = actUUID

{- | One warning per activity whose products 'ilcdProcessUUID' spreads over
several process datasets. Every product is kept, but ILCD has no way to say
the datasets came from one activity, so a re-import yields independent
single-output activities — the grouping is the one thing the export loses,
and the caller deserves to hear about it rather than discover it on re-import.
Empty when every activity UUID is unique.
-}
splitWarnings :: SimpleDatabase -> [Text]
splitWarnings db =
    [ "multi-output activity \""
        <> activityName act
        <> "\" (UUID "
        <> uuidText actUUID
        <> "): its "
        <> T.pack (show (length acts))
        <> " products export as separate ILCD process datasets; their grouping is lost on re-import"
    | (actUUID, acts@(act : _ : _)) <- M.toAscList byActivity
    ]
  where
    byActivity = M.fromListWith (++) [(actUUID, [act]) | ((actUUID, _), act) <- M.toAscList (sdbActivities db)]

--------------------------------------------------------------------------------
-- Top-level: directory / archive
--------------------------------------------------------------------------------

{- | Produce the full set of @(relativePath, contents)@ pairs for the ILCD
package. Pure and deterministic. The result is sorted by path.

Paths use a forward slash on every OS: the ILCD package layout and the zip
archive both mandate @/@ regardless of host (a backslash from
'System.FilePath.</>' on Windows would yield a non-portable archive and break
the @processes/@ prefix the parser keys on). 'writeILCDDatabase' maps them to
the native separator before touching disk.
-}
ilcdFiles :: WriteOptions -> SimpleDatabase -> [(FilePath, BS.ByteString)]
ilcdFiles opts db = sortOn fst (processes ++ flows ++ flowProps ++ unitGroups)
  where
    shared = sharedActivityUUIDs db

    processes =
        [ ("processes/" <> uuidStr dsUUID <> ".xml", render (processXML opts dsUUID act))
        | (pair, act) <- M.toAscList (sdbActivities db)
        , let dsUUID = ilcdProcessUUID shared pair
        ]

    flows =
        [ ("flows/" <> uuidStr (flowKindId fk) <> ".xml", render (flowXML fk unitRef))
        | (fk, unitRef) <- allFlows db
        ]

    flowProps =
        [ ("flowproperties/" <> uuidStr uid <> ".xml", render (flowPropertyXML u))
        | (uid, u) <- M.toAscList (sdbUnits db)
        ]

    unitGroups =
        [ ("unitgroups/" <> uuidStr uid <> ".xml", render (unitGroupXML u))
        | (uid, u) <- M.toAscList (sdbUnits db)
        ]

    -- Render the list of lines to canonical UTF-8 bytes.
    render = TE.encodeUtf8 . renderLines

{- | Guard an ILCD export against data the parser cannot read back losslessly.

A multi-output activity is /not/ one of those: ILCD keys a process by a single
dataset UUID, but 'ilcdProcessUUID' hands each @(activity, product)@ entry a
distinct one, so each product becomes its own process dataset.

* /Media this classification cannot name./ 'compartmentBlock' emits
  @"Emissions to <medium>"@ for any non-resource medium, but the parser's
  @extractMedium@ only inverts the air\/water\/soil\/natural-resource phrasings.
  A flow of any other medium ('Waste', 'InventoryIndicator', 'Economic')
  re-imports under a different compartment, silently shifting LCIA scores.

* /Empty classification levels./ 'classificationBlock' joins levels with @"/"@
  and the parser splits on it, dropping empty parts. A value with an empty level
  — @""@ (key vanishes), or @"a//b"@ \/ @"a/"@ (collapses to @"a/b"@ \/ @"a"@) —
  therefore does not round-trip.

* /Amounts that do not re-parse./ The written decimal must re-parse to the same
  'Double' through 'Amount.readAmount' (the importer's correctly-rounded
  reader). Every finite amount does, so this rejects only the non-finite
  (@NaN@\/@Infinity@) that would otherwise shift LCIA scores on re-import.

Rather than silently corrupting any of these, report the first offending flow,
activity or exchange so the caller can fail loudly. Databases whose biosphere
media are all canonical, whose classification levels are all non-empty and whose
amounts all re-parse pass unchanged.
-}
checkILCDExportable :: SimpleDatabase -> Either Text ()
checkILCDExportable db =
    case lefts [checkMedia, checkClassifications, checkAmounts] of
        [] -> Right ()
        violations -> Left (T.intercalate "\n\n" violations)
  where
    -- The media the parser's @extractMedium@ inverts back.
    canonicalMedia = [Air, Water, Soil, NaturalResource]
    checkMedia =
        case [f | f <- M.elems (sdbBioFlows db), notInvertible f] of
            [] -> Right ()
            (f : _) ->
                Left $
                    "ILCD export cannot represent biosphere flow \""
                        <> bfName f
                        <> "\" (UUID "
                        <> uuidText (bfId f)
                        <> "): compartment medium \""
                        <> bfCompartmentName f
                        <> "\" is not one the ILCD parser can read back; "
                        <> "only air, water, soil and natural resource round-trip."
    notInvertible f = case bfCompartment f of
        Nothing -> False
        Just c -> compartmentName c `notElem` canonicalMedia

    nameOf actUUID =
        case [activityName act | ((a, _), act) <- M.toList (sdbActivities db), a == actUUID] of
            (nm : _) -> nm
            [] -> "?"

    -- A classification value round-trips only when none of its "/"-joined levels
    -- is empty; the parser splits on "/" and drops empty parts.
    checkClassifications =
        case [ (actUUID, name, value)
             | ((actUUID, _), act) <- M.toList (sdbActivities db)
             , (name, value) <- M.toList (activityClassification act)
             , any T.null (T.splitOn "/" value)
             ] of
            [] -> Right ()
            ((actUUID, name, value) : _) ->
                Left $
                    "ILCD export cannot represent classification \""
                        <> name
                        <> "\" = \""
                        <> value
                        <> "\" on activity \""
                        <> nameOf actUUID
                        <> "\" (UUID "
                        <> uuidText actUUID
                        <> "): an empty classification level does not round-trip "
                        <> "because the ILCD parser drops empty levels."

    -- An amount round-trips when its fixed-point rendering re-parses to the same
    -- value through 'Amount.readAmount' (the importer's correctly-rounded
    -- reader). Every finite amount does, so this rejects only the non-finite.
    checkAmounts =
        case [ (actUUID, exchangeAmount ex)
             | ((actUUID, _), act) <- M.toList (sdbActivities db)
             , ex <- exchanges act
             , not (amountRoundTrips (exchangeAmount ex))
             ] of
            [] -> Right ()
            ((actUUID, amt) : _) ->
                Left $
                    "ILCD export cannot represent amount "
                        <> T.pack (show amt)
                        <> " on activity \""
                        <> nameOf actUUID
                        <> "\" (UUID "
                        <> uuidText actUUID
                        <> "): it does not re-parse to the same value through the "
                        <> "ILCD parser (a non-finite amount)."
    amountRoundTrips amt = readAmount (formatDouble amt) == Just amt

{- | Write the ILCD package as a directory tree rooted at @dir@, or return the
export guard's 'Left' without touching disk.
-}
writeILCDDatabase :: WriteOptions -> FilePath -> SimpleDatabase -> IO (Either Text ())
writeILCDDatabase opts dir db =
    case checkILCDExportable db of
        Left err -> pure (Left err)
        Right () -> do
            createDirectoryIfMissing True (dir </> "processes")
            createDirectoryIfMissing True (dir </> "flows")
            createDirectoryIfMissing True (dir </> "flowproperties")
            createDirectoryIfMissing True (dir </> "unitgroups")
            mapM_ (\(rel, bytes) -> BS.writeFile (dir </> nativePath rel) bytes) (ilcdFiles opts db)
            pure (Right ())
  where
    -- ilcdFiles yields forward-slash package paths; rejoin with the native
    -- separator so the on-disk write is correct on Windows too.
    nativePath = joinPath . splitDirectories

{- | Build a deterministic zip archive of the ILCD package and return its
serialized bytes. Runs 'checkILCDExportable' first and returns its 'Left' on a
database the format cannot represent faithfully, so an unguarded caller cannot
silently emit a corrupt archive.
-}
writeILCDArchive :: WriteOptions -> SimpleDatabase -> Either Text BL.ByteString
writeILCDArchive opts db = checkILCDExportable db >> pure (zipFiles (ilcdFiles opts db))

--------------------------------------------------------------------------------
-- Flow enumeration (tech ∪ bio ∪ waste), tagged with their unit id
--------------------------------------------------------------------------------

-- | All flows in the database as 'FlowKind' + unit id, sorted by flow UUID.
allFlows :: SimpleDatabase -> [(FlowKind, UUID)]
allFlows db =
    [(TechKind f, tfUnitId f) | f <- M.elems (sdbTechFlows db)]
        ++ [(BioKind f, bfUnitId f) | f <- M.elems (sdbBioFlows db)]
        ++ [(WasteKind f, wfUnitId f) | f <- M.elems (sdbWasteFlows db)]

--------------------------------------------------------------------------------
-- Process XML
--------------------------------------------------------------------------------

{- | Render one ILCD process dataset for an activity, under the dataset UUID
'ilcdProcessUUID' assigned it — not necessarily the activity's own UUID, which
several products may share. The reference exchange gets @dataSetInternalID@
matching @referenceToReferenceFlow@; remaining exchanges follow in their list
order, so the parser reads them back in the same order it would re-serialize.
-}
processXML :: WriteOptions -> UUID -> Activity -> [Text]
processXML opts dsUUID act =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <processInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText dsUUID)
    , "      <name>"
    , attrElem "baseName" [("xml:lang", "en")] (activityName act)
    , "      </name>"
    ]
        ++ classificationBlock (activityClassification act)
        ++ [ "    </dataSetInformation>"
           , attrOnly "geography" [("location", activityLocation act)]
           , "    <quantitativeReference>"
           , elem' "referenceToReferenceFlow" (T.pack (show refIdx))
           , "    </quantitativeReference>"
           ]
        ++ timeStampBlock opts
        ++ [ "  </processInformation>"
           ]
        ++ processTypeBlock (activityNativeType act)
        ++ generatorBlock opts
        ++ ["  <exchanges>"]
        ++ concatMap (uncurry exchangeXML) indexedExchanges
        ++ [ "  </exchanges>"
           , "</processDataSet>"
           ]
  where
    -- Exchanges are emitted in list order; reference index is the position of
    -- the (first) reference exchange. When none is marked, emit a sentinel past
    -- the last internalID so the parser's findRefExchange matches no exchange
    -- and preserves the "no reference" state instead of fabricating one at 0.
    indexedExchanges = zip [0 ..] (exchanges act)
    refIdx :: Int
    refIdx = case [i | (i, ex) <- indexedExchanges, exchangeIsReference ex] of
        (i : _) -> i
        [] -> length indexedExchanges

-- | One @<exchange>@ block. @i@ is the @dataSetInternalID@.
exchangeXML :: Int -> Exchange -> [Text]
exchangeXML i ex =
    [ "    <exchange dataSetInternalID=\"" <> T.pack (show i) <> "\">"
    , attrOnly "referenceToFlowDataSet" [("refObjectId", uuidText (exchangeFlowId ex)), ("type", "flow data set")]
    , elem' "exchangeDirection" direction
    , elem' "resultingAmount" (formatDouble (exchangeAmount ex))
    ]
        ++ locationBlock (exchangeLocation ex)
        ++ commentBlock (exchangeComment ex)
        ++ ["    </exchange>"]
  where
    direction = if exchangeIsInput ex then "Input" else "Output"

{- | Per-exchange @<location>@, which the parser reads back into the exchange's
location field. Omitted when empty — the common case, since ILCD geography lives
at the process level — so it never perturbs the byte-stable round-trip.
-}
locationBlock :: Text -> [Text]
locationBlock loc
    | T.null loc = []
    | otherwise = [elem' "location" loc]

-- | Per-exchange comment, English-tagged to match the parser's preference.
commentBlock :: Maybe Text -> [Text]
commentBlock Nothing = []
commentBlock (Just c) = [attrElem "common:generalComment" [("xml:lang", "en")] c]

{- | Classification block. Each classification system becomes one
@<common:classification name="...">@ with one @<common:class>@ per
"/"-joined level, preserving the parser's join semantics. Systems are
sorted by name for determinism.
-}
classificationBlock :: M.Map Text Text -> [Text]
classificationBlock cls
    | M.null cls = []
    | otherwise =
        ["      <classificationInformation>"]
            ++ concatMap system (M.toAscList cls)
            ++ ["      </classificationInformation>"]
  where
    system (name, value) =
        [attrOnlyOpen "common:classification" [("name", name)]]
            ++ [ attrElem "common:class" [("level", T.pack (show lvl))] part
               | (lvl :: Int, part) <- zip [0 ..] (T.splitOn "/" value)
               ]
            ++ ["        </common:classification>"]

{- | ILCD @<processType>@, nested where the parser expects it. @<processType>@
is free text, so a foreign native-type label (SimaPro @Type@, ecospold
@activityType@) is carried through rather than dropped. Omitted when no native
type is set or its label is empty.
-}
processTypeBlock :: Maybe NativeActivityType -> [Text]
processTypeBlock nt = case nativeTypeLabel nt of
    Just label
        | not (T.null label) ->
            [ "  <modellingAndValidation>"
            , "    <LCIMethodAndAllocation>"
            , elem' "processType" label
            , "    </LCIMethodAndAllocation>"
            , "  </modellingAndValidation>"
            ]
    Just _ -> []
    Nothing -> []

{- | Native-type display label, across all source formats (the value carried
into @<processType>@). 'Nothing' when no native type is set.
-}
nativeTypeLabel :: Maybe NativeActivityType -> Maybe Text
nativeTypeLabel nt = case nt of
    Just (ILCDProcessType label) -> Just label
    Just (SimaProProcessType label) -> Just label
    Just (EcoSpoldActivityType{eatLabel = label}) -> Just label
    Nothing -> Nothing

-- | Optional pinned timestamp inside @<processInformation>@ (omitted by default).
timeStampBlock :: WriteOptions -> [Text]
timeStampBlock opts = case woTimestamp opts of
    Nothing -> []
    Just ts -> [elem' "common:timeStamp" ts]

-- | Optional pinned generator string (omitted by default).
generatorBlock :: WriteOptions -> [Text]
generatorBlock opts = case woGenerator opts of
    Nothing -> []
    Just g ->
        [ "  <administrativeInformation>"
        , "    <dataGenerator>"
        , elem' "common:referenceToDataGenerator" g
        , "    </dataGenerator>"
        , "  </administrativeInformation>"
        ]

--------------------------------------------------------------------------------
-- Flow XML
--------------------------------------------------------------------------------

{- | Render one ILCD flow dataset. @typeOfDataSet@ is set so the parser's
'classifyFlowType' re-buckets the flow into the same kind. Biosphere flows
carry their compartment categories; CAS round-trips when present.
-}
flowXML :: FlowKind -> UUID -> [Text]
flowXML fk unitRef =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <flowInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText (flowKindId fk))
    , "      <name>"
    , attrElem "baseName" [("xml:lang", "en")] (flowKindName fk)
    , "      </name>"
    ]
        ++ compartmentBlock (flowKindCompartment fk)
        ++ casBlock (flowKindCAS fk)
        ++ [ "    </dataSetInformation>"
           , "    <quantitativeReference>"
           , elem' "referenceToReferenceFlowProperty" "0"
           , "    </quantitativeReference>"
           , "  </flowInformation>"
           , "  <modellingAndValidation>"
           , "    <LCIMethod>"
           , elem' "typeOfDataSet" (flowTypeText fk)
           , "    </LCIMethod>"
           , "  </modellingAndValidation>"
           , "  <flowProperties>"
           , "    <flowProperty dataSetInternalID=\"0\">"
           , attrOnly "referenceToFlowPropertyDataSet" [("refObjectId", uuidText unitRef), ("type", "flow property data set")]
           , elem' "meanValue" "1"
           , "    </flowProperty>"
           , "  </flowProperties>"
           , "</flowDataSet>"
           ]

-- | CAS accessor across flow kinds (parser reads it for biosphere flows).
flowKindCAS :: FlowKind -> Maybe Text
flowKindCAS (TechKind f) = tfCAS f
flowKindCAS (BioKind f) = bfCAS f
flowKindCAS (WasteKind f) = wfCAS f

-- | @typeOfDataSet@ string mirroring 'ILCD.Parser.classifyFlowType'.
flowTypeText :: FlowKind -> Text
flowTypeText (TechKind _) = "Product flow"
flowTypeText (BioKind _) = "Elementary flow"
flowTypeText (WasteKind _) = "Waste flow"

casBlock :: Maybe Text -> [Text]
casBlock Nothing = []
casBlock (Just cas)
    | T.null cas = []
    | otherwise = [elem' "CASNumber" cas]

{- | Emit the biosphere @elementaryFlowCategorization@. We reverse the parser's
'parseCompartment': level 0 is "Emissions"/"Resources", level 1 names the
medium, level 2 (when a sub-compartment exists) is "<medium-phrase>, <sub>".
-}
compartmentBlock :: Maybe Compartment -> [Text]
compartmentBlock Nothing = []
compartmentBlock (Just (Compartment medium sub)) =
    [ "      <classificationInformation>"
    , "        <common:elementaryFlowCategorization>"
    , attrElem "common:category" [("level", "0")] level0
    , attrElem "common:category" [("level", "1")] level1
    ]
        ++ level2
        ++ [ "        </common:elementaryFlowCategorization>"
           , "      </classificationInformation>"
           ]
  where
    m = mediumText medium
    isResource = medium == NaturalResource
    level0 = if isResource then "Resources" else "Emissions"
    level1
        | isResource = "Resources"
        | otherwise = "Emissions to " <> m
    level2 = case sub of
        Nothing -> []
        Just s
            | T.null s -> []
            | isResource -> [attrElem "common:category" [("level", "2")] ("Resources " <> s)]
            | otherwise -> [attrElem "common:category" [("level", "2")] ("Emissions to " <> m <> ", " <> s)]

--------------------------------------------------------------------------------
-- FlowProperty XML  (one per unit; shares the unit's UUID)
--------------------------------------------------------------------------------

flowPropertyXML :: Unit -> [Text]
flowPropertyXML u =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<flowPropertyDataSet xmlns=\"http://lca.jrc.it/ILCD/FlowProperty\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <flowPropertiesInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText (unitId u))
    , attrElem "common:name" [("xml:lang", "en")] (unitName u)
    , "    </dataSetInformation>"
    , "    <quantitativeReference>"
    , attrOnly "referenceToReferenceUnitGroup" [("refObjectId", uuidText (unitId u)), ("type", "unit group data set")]
    , "    </quantitativeReference>"
    , "  </flowPropertiesInformation>"
    , "</flowPropertyDataSet>"
    ]

--------------------------------------------------------------------------------
-- UnitGroup XML  (one per unit; shares the unit's UUID; single reference unit)
--------------------------------------------------------------------------------

unitGroupXML :: Unit -> [Text]
unitGroupXML u =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<unitGroupDataSet xmlns=\"http://lca.jrc.it/ILCD/UnitGroup\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <unitGroupInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText (unitId u))
    , attrElem "common:name" [("xml:lang", "en")] (unitName u)
    , "    </dataSetInformation>"
    , "    <quantitativeReference>"
    , elem' "referenceToReferenceUnit" "0"
    , "    </quantitativeReference>"
    , "  </unitGroupInformation>"
    , "  <units>"
    , "    <unit dataSetInternalID=\"0\">"
    , elem' "name" (unitName u)
    , elem' "meanValue" "1"
    , "    </unit>"
    , "  </units>"
    , "</unitGroupDataSet>"
    ]

--------------------------------------------------------------------------------
-- XML primitives
--------------------------------------------------------------------------------

-- | Join the rendered lines with newlines and a trailing newline.
renderLines :: [Text] -> Text
renderLines = (<> "\n") . T.intercalate "\n"
{-# INLINE renderLines #-}

-- | @<tag>escaped-text</tag>@ on one indented line.
elem' :: Text -> Text -> Text
elem' tag txt = "      <" <> tag <> ">" <> escapeXml txt <> "</" <> tag <> ">"

-- | @<tag a=\"v\" ...>escaped</tag>@ on one indented line.
attrElem :: Text -> [(Text, Text)] -> Text -> Text
attrElem tag attrs txt =
    "      <" <> tag <> attrsText attrs <> ">" <> escapeXml txt <> "</" <> tag <> ">"

-- | Self-closing @<tag a=\"v\" .../>@.
attrOnly :: Text -> [(Text, Text)] -> Text
attrOnly tag attrs = "      <" <> tag <> attrsText attrs <> "/>"

-- | Open-only @<tag a=\"v\" ...>@ (caller emits the close).
attrOnlyOpen :: Text -> [(Text, Text)] -> Text
attrOnlyOpen tag attrs = "        <" <> tag <> attrsText attrs <> ">"

attrsText :: [(Text, Text)] -> Text
attrsText = T.concat . map (\(k, v) -> " " <> k <> "=\"" <> escapeXmlAttr v <> "\"")

{- | XML text-node escaping. Covers the three entities that matter in element
content (@&@, @<@, @>@). The parser (Xeno SAX) un-escapes these, so escaping
here keeps the round-trip faithful for names/comments containing @&@, @<@, etc.
-}
escapeXml :: Text -> Text
escapeXml =
    T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

{- | Attribute-value escaping: the text entities, the quote characters, and the
newline/carriage-return control characters. A raw @\\n@/@\\r@ inside an attribute
value is normalised to a space by XML parsers, so encode it as a numeric
character reference (matching the EcoSpold2 writer) to keep it verbatim across a
round trip.
-}
escapeXmlAttr :: Text -> Text
escapeXmlAttr =
    T.replace "\r" "&#13;"
        . T.replace "\n" "&#10;"
        . T.replace "'" "&apos;"
        . T.replace "\"" "&quot;"
        . escapeXml

{- | Canonical 'Double' formatting via the shared 'showFFloatTrim' (fixed-point,
never scientific), the exact inverse of 'Amount.readAmount': every finite amount
round-trips through that correctly-rounded reader. 'checkILCDExportable' rejects
any amount that does not re-parse, which now leaves only the non-finite. A
non-finite value renders as its (non-parseable)
@"NaN"@/@"Infinity"@ form so a bad re-import fails loudly rather than silently
reading @0@; the guard rejects non-finite amounts first. Negative zero is
normalised to @0.0@.
-}
formatDouble :: Double -> Text
formatDouble x
    | isNaN x || isInfinite x = T.pack (show x)
    | x == 0 = "0.0"
    | otherwise = T.pack (showFFloatTrim x)

--------------------------------------------------------------------------------
-- UUID rendering
--------------------------------------------------------------------------------

uuidText :: UUID -> Text
uuidText = UUID.toText

uuidStr :: UUID -> FilePath
uuidStr = UUID.toString

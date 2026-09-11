{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Database.Loader
Description : High-performance EcoSpold XML loading with matrix caching

This module provides optimized loading of EcoSpold XML files together with a
single cache storing the fully indexed database and pre-computed sparse
matrices. When the cache is absent or invalidated, the loader reparses all
EcoSpold datasets, builds the in-memory structures, and writes the matrix cache
for subsequent runs.

Key performance features:
- Parallel parsing with controlled concurrency (prevents resource exhaustion)
- Automatic cache invalidation when the schema or the build inputs change
- Memory-efficient chunked processing for large databases
- Hash-based cache filenames for multi-dataset support

Cache performance (Ecoinvent 3.8 with 18K activities):
- Cold start (XML parsing + matrix build): ~45s
- Matrix cache hit: ~0.5s

The cache keeps day-to-day execution fast while preserving reproducibility.
-}
module Database.Loader (
    -- * Main Loading Functions
    loadDatabase,
    LoadOptions (..),
    defaultLoadOptions,
    loadDatabaseWithLocationAliases,
    reportKeyRefusals,
    loadSimaProCSV,
    loadDatabaseWithCrossDBLinking,
    findFilesByExtRecursive,

    -- * Cache Operations
    loadCachedDatabaseWithMatrices,
    saveCachedDatabaseWithMatrices,
    generateMatrixCacheFilename,

    -- * Cross-Database Linking
    fixActivityLinksWithCrossDB,
    relinkSimpleDatabase,
    findAllCrossDBLinks,
    CrossDBLinkingStats (..),
    crossDBLinksCount,
    unresolvedCount,
    crossDBBySource,
    collectUnlinkedProductNames,

    -- * Database Analysis
    countTotalTechInputs,
    countUnlinkedExchanges,
    collectDanglingProductNames,
    collectStagedDanglingProductNames,

    -- * Supplier-gap report
    GapReason (..),
    GapEdge (..),
    GapConsumer (..),
    GapEntry (..),
    GapReport (..),
    gapReportForLoaded,
    gapReportForStaged,

    -- * Internal Linking
    fixSimaProActivityLinks,
    fixEcoSpold1ActivityLinks,

    -- * Reporting
    reportCrossDBLinkingStats,

    -- * Internal (exposed for testing)
    normalizeText,
    mergeTechFlows,
    mergeBioFlows,
    Harvest (..),
    harvestOf,
    generateActivityUUIDFromActivity,
    datasetUUIDFromPath,
    getReferenceProductUUID,
    indexActivities,
    UnlinkedSummary (..),
    LocationOverride (..),
    AmbiguousProducer (..),
    NameOnlyIndex,
    NameProducer (..),
    ecoSpold1LinkContext,
    fixAllActivities,
    buildSupplierIndex,
    buildSupplierIndexByName,
    fixExchangeLinkByName,
) where

import qualified BrightwayExcel.Parser as BrightwayExcel
import qualified Codec.Compression.Zstd as Zstd
import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Either (lefts, partitionEithers, rights)
import Data.Foldable (find)
import Data.List (intercalate, sort, sortBy, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

-- The flow tables are merged through the strict API: one substance now appears
-- in many datasets, so every merge that used to be a no-op (keys were unique
-- per dataset) is real, and the lazy API would stack one unforced merge per
-- occurrence, holding every superseded record until something forced the chain.
import qualified Data.Map.Strict as MS
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Store (decodeEx, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (typeRep, typeRepFingerprint)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Database.Allocation (
    Allocating (..),
    PropertyRefusal,
    allocate,
    allocateAll,
    describePropertyRefusal,
    propertyKeyRefusals,
 )
import Database.CrossLinking (
    AliasMap,
    CrossDBLinkResult (..),
    IndexedDatabase (..),
    LinkWarning (..),
    LinkingContext (..),
    SupplierEntry (..),
    SupplierQuery (..),
    WasteTreatmentMatch (..),
    defaultLinkingThreshold,
    emptyAliasMap,
    extractBracketedLocation,
    findSupplierByActivityProduct,
    findSupplierInIndexedDBs,
    findWasteTreatmentAcrossDatabases,
    findWasteTreatmentByActivity,
    locationHierarchy,
    normalizeUnicode,
 )
import Database.MatrixBuild (findProducer)
import Database.Upload (listDirectoryRecursive)
import EcoSpold.Common (ParsedDataset (..), distributeFiles)
import EcoSpold.Parser1 (streamParseActivityAndFlowsFromFile1, streamParseAllDatasetsFromFile1)
import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import GHC.Fingerprint (Fingerprint (..))
import qualified ILCD.Parser as ILCD
import Method.Types (Location)
import Progress
import qualified SimaPro.Parser as SimaPro
import SynonymDB (SynonymDB)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import Text.Printf (printf)
import Types
import qualified UnitConversion as UC

-- | Magic bytes to identify VoLCA cache files
cacheMagic :: BS.ByteString
cacheMagic = "VOLCACHE"

{- | Merge two technosphere flows with the same UUID, combining their synonyms
and keeping whichever of the two declares a CAS number.
When multiple .spold files reference the same flow each may carry different
synonyms; M.fromListWith mergeTechFlows ensures no synonym is lost.

The CAS is kept the same way because a file that declares it and a file that
omits it describe the same substance, and which one lands first is an accident
of how the loader distributed the files over its workers. Of the export
measured here, 430 flows are declared with a CAS in some datasets and without
in others; losing it there would silently disable the CAS rung of the
characterization cascade for the whole database.
-}
mergeTechFlows :: TechnosphereFlow -> TechnosphereFlow -> TechnosphereFlow
mergeTechFlows a b =
    a
        { tfSynonyms = M.unionWith S.union (tfSynonyms a) (tfSynonyms b)
        , tfCAS = tfCAS a <|> tfCAS b
        }

-- | Biosphere counterpart of 'mergeTechFlows'.
mergeBioFlows :: BiosphereFlow -> BiosphereFlow -> BiosphereFlow
mergeBioFlows a b =
    a
        { bfSynonyms = M.unionWith S.union (bfSynonyms a) (bfSynonyms b)
        , bfCAS = bfCAS a <|> bfCAS b
        }

{- | What one reader harvested from the files it was given: a piece of the
database, the dataset numbers those files carried, and how many flow and unit
declarations it read before deduplication. A load is the sum of its harvests.

Summing is not the same operation as harvesting, and the two must not be
collapsed into one. Inside a harvest the last dataset read wins a duplicate key:
'M.fromList' keeps the last entry, and 'MS.fromListWith' hands the newer row to
'mergeTechFlows' as the base. Between two harvests the earlier reader wins:
'M.union' keeps the leftmost, and 'MS.unionWith' keeps its left argument as the
base. 'LoaderSpec' pins both directions through 'harvestOf' and this instance,
which is why the two are exported. The qualifiers are not interchangeable
either: the flow tables are merged strictly, for the reason the import of
'Data.Map.Strict' gives, and the rest is left as the build sites had it.

'hvDatasetNumbers' is the one table under neither law: a repeated number is the
ordinary shape there, not a collision to arbitrate, so both sides are kept and
the reader picks by product name. See 'DatasetNumberIndex'.
-}
data Harvest = Harvest
    { hvActivities :: !ActivityMap
    , hvTechFlows :: !TechFlowDB
    , hvBioFlows :: !BioFlowDB
    , hvWasteFlows :: !WasteFlowDB
    , hvUnits :: !UnitDB
    , hvDatasetNumbers :: !DatasetNumberIndex
    , hvRawFlows :: !Int
    -- ^ flow declarations read, before deduplication
    , hvRawUnits :: !Int
    -- ^ unit declarations read, before deduplication
    }

instance Semigroup Harvest where
    a <> b =
        Harvest
            { hvActivities = M.union (hvActivities a) (hvActivities b)
            , hvTechFlows = MS.unionWith mergeTechFlows (hvTechFlows a) (hvTechFlows b)
            , hvBioFlows = MS.unionWith mergeBioFlows (hvBioFlows a) (hvBioFlows b)
            , hvWasteFlows = M.union (hvWasteFlows a) (hvWasteFlows b)
            , hvUnits = M.union (hvUnits a) (hvUnits b)
            , hvDatasetNumbers = MS.unionWith (<>) (hvDatasetNumbers a) (hvDatasetNumbers b)
            , hvRawFlows = hvRawFlows a + hvRawFlows b
            , hvRawUnits = hvRawUnits a + hvRawUnits b
            }

instance Monoid Harvest where
    mempty = Harvest M.empty MS.empty MS.empty M.empty M.empty M.empty 0 0

{- | Harvest a batch of parsed datasets, each already keyed by the (activity,
product) pair its source names it under.
-}
harvestOf :: [((UUID, UUID), ParsedDataset)] -> Harvest
harvestOf entries =
    Harvest
        { hvActivities = M.fromList [(key, pdActivity parsed) | (key, parsed) <- entries]
        , hvTechFlows = MS.fromListWith mergeTechFlows [(tfId f, f) | f <- techs]
        , hvBioFlows = MS.fromListWith mergeBioFlows [(bfId f, f) | f <- bios]
        , hvWasteFlows = M.fromList [(wfId f, f) | f <- wastes]
        , hvUnits = M.fromList [(unitId u, u) | u <- units]
        , hvDatasetNumbers = MS.fromListWith (flip (<>)) [(pdDatasetNumber parsed, key NE.:| []) | (key, parsed) <- entries, pdDatasetNumber parsed /= 0]
        , hvRawFlows = length techs + length bios + length wastes
        , hvRawUnits = length units
        }
  where
    techs :: [TechnosphereFlow]
    techs = concatMap (pdTechFlows . snd) entries
    bios :: [BiosphereFlow]
    bios = concatMap (pdBioFlows . snd) entries
    wastes :: [WasteFlow]
    wastes = concatMap (pdWasteFlows . snd) entries
    units :: [Unit]
    units = concatMap (pdUnits . snd) entries

-- | The five tables of a harvest that make a database; the other three describe the reading.
harvestDatabase :: Harvest -> SimpleDatabase
harvestDatabase h =
    SimpleDatabase (hvActivities h) (hvTechFlows h) (hvBioFlows h) (hvWasteFlows h) (hvUnits h)

{- |
Schema signature of the cache payload.

The 'Typeable' fingerprint names the 'Database' type (package, module, name)
and nothing more: it does not move when a field is added or a nested type
changes. Every change to what the cache holds, layout or meaning, therefore
needs a bump of the trailing constant, so existing caches are rebuilt on the
next load instead of being misread or silently returning stale numbers.

History of manual bumps:
- 5: reference-product amounts normalized to canonical base unit at ingest
     (SimaPro CSV parser); matrices built before this bump divided by the
     raw amount regardless of unit, so e.g. a 1-ton reference yielded
     impacts 1000× too large.
- 6: SimaPro CSV parser now extracts location from the older "name//[XX]"
     pattern (ecoinvent 3.9.1 export). Caches built before this bump have
     empty activityLocation for every activity in such databases, which
     breaks geography-aware supplier lookups.
- 7: SimaPro multi-product processes now share one activityUUID across
     coproducts (activityName derived from "Process name" field, not from
     the product name). Activity record gained activityAllocationPercent
     and activityAllocationFormula. Old caches have stale per-product
     UUIDs and miss the allocation fields entirely.
- 8: EcoSpold1 biosphere-flow UUID now includes the subCategory, so an
     emission to two subcompartments (e.g. river + groundwater, long-term)
     no longer collapses to one row scored at a single arbitrary
     subcompartment's CF. Old caches merged those amounts under one flow.
- 9: LinkBlocker gained AliasTargetMissing (geo-aware relink mapping), which
     changes the Store layout of the linking stats embedded in the cache; a
     downgrade reading a newer cache would fail mid-decode, so both directions
     rebuild once instead.
- 10: Activity record gained activityFormulaCheck (mathematicalRelation
     consistency outcome, surfaced by the database quality report). Old
     caches miss the field and would fail mid-decode.
- 11: SimaPro flow CAS now backfilled from the export's own substance
     registry at parse time — a value change with no type change, so the
     fingerprint alone would accept old caches. Caches built before the
     backfill keep every SimaPro biosphere flow CAS-less, and the method
     CAS bridge silently never fires on them.
- 12: EcoSpold1 activity UUID now taken from the dataset file's own name
     when that name carries one, instead of always being minted from
     name and location. Old caches key the same dataset under the minted
     UUID, so a mixed pair would compare as two disjoint databases.
- 13: Activity record gained activityLocationSource (declared, read off the
     dataset name, or neither). Old caches miss the field; the Store layout
     is positional, so decoding them would misread every field after it.
- 14: EcoSpold1 flow UUID no longer carries the dataset a flow was read from,
     so one substance is one flow across the export. Old caches hold one flow
     per (dataset, substance) pair — a value change with no type change, which
     the fingerprint alone would accept.
- 15: Activity record gained activityDocumentation (the provenance a dataset
     states about itself: published source, technology, review). Old caches
     miss the field; the Store layout is positional, so decoding them would
     misread every field after it.
- 16: a dimension's reference unit is now its shortest spelling at factor 1.0,
     and the energy column of the unit table is scaled so that MJ carries it.
     A reference product ingested from SimaPro or Brightway Excel is therefore
     recorded as 3.6 mj where it used to be 3.6e6 j, which divides the
     activity's normalization factor by a million, and a volume or count
     reference is recorded under m3 / p rather than cubic meter /
     dimensionless, which changes the product flow's UUID (it is derived from
     the unit name) and so the activity's process id. Value changes with no
     type change, which the fingerprint alone would accept.
- 17: the flow index now lists the rows that use a flow, not their activity
     UUIDs, and the product and activity indexes list every row a flow or an
     activity was written as instead of one. The fingerprint hashes the
     identity of Database, never the types inside it, so an old cache would
     pass the check and be decoded reading 16-byte UUIDs as 4-byte row
     numbers, or one row number as a list of them.
- 18: a dataset read from SimaPro or Brightway Excel is identified by the
     identifier its file publishes, a flow by its name folded in case and its
     compartment with no unit, and every row is recorded in the reference unit
     of its dimension. Process ids, flow ids and amounts all move, and none of
     it changes a type, so an old cache would pass the fingerprint and answer
     with identifiers no request would name again.
- 19: an avoided product has its own role, a product row carries its declared
     share and its category, and every activity passes the allocation gate
     before the matrix. The cached exchanges of an allocated database say
     less than the loader now reads, so they are read again.
- 20: the payload records the unit table and location aliases the database
     was built with ('dbBuiltWith'), compared before a cache is trusted. Old
     caches end before the field.
- 21: a technosphere line carries the physical properties its source states
     ('techProperties'), the material an allocation key other than the declared
     one is computed from. Cached exchanges end before the field.
- 22: the payload records the allocation key the database was divided under
     ('dbBuiltWith'), so a cache of one key is not served to a load asking for
     another. Old caches end before the field.
- 23: the index of a block's products is gone, the activity UUID index being
     the same map: the payload has one field fewer, and every field after it
     would be read at the wrong offset.
- 24: an EcoSpold 1 activity now carries the number its dataset is published
     under, in a field that already existed and was already stored. Nothing
     changes type, so a cache written just before this would pass the
     fingerprint and go on reporting that those datasets have no identifier.
- 25: an EcoSpold 2 elementary exchange in the "inventory indicator"
     compartment is a biosphere flow again, not a waste one. Nothing changes
     type, so a cache written just before this would pass the fingerprint and
     go on holding those flows on the waste axis - invisible to every method,
     and demanding a supplier no activity can be.
- 26: a SimaPro "Final waste flows" row is an elementary flow of medium
     "waste", not a waste exchange. Nothing changes type, so a cache written
     just before this would pass the fingerprint and keep those flows on an
     axis no method reads.
- 27: the same row read from an EcoSpold 1 file is an elementary flow too, and
     an export writes it as one. Nothing changes type, so a cache written just
     before this would pass the fingerprint and keep those flows on the waste
     axis, still counted as demands the gap report can never close.
- 28: a compartment's medium is a 'Types.Medium', not text. The stored shape
     changes and the resource flows of a SimaPro-sourced database change
     identity, since the medium is hashed into it.
- 29: a technosphere line carries the supplier activity its source names apart
     from the product ('techSupplierActivity'), and the linking stats carry the
     inputs several activities of one dependency answered equally well. Cached
     exchanges end before the field, and every field after it would be read at
     the wrong offset.
- 30: a technosphere or waste line no longer carries a process link. It was a
     matrix row index on a parsed exchange, which no parser can know and none
     ever wrote; the record is one field shorter, so every field after it
     would be read at the wrong offset.
- 31: a waste line carries its source's claim too, and a technosphere claim is
     a 'SupplierClaim' rather than a bare name, so the bytes of both differ
     from 29's.
- 32: the supplier an exchange resolved to is a 'Maybe', not a UUID with nil
     standing for none. The field changes width, so every field after it would
     be read at the wrong offset.
- 33: an EcoSpold 1 elementary exchange whose category states the direction as
     well as the medium ("emissions to air") is placed in that medium, where it
     used to be left with no compartment at all. Nothing changes type, so a
     cache written just before this would pass the fingerprint and go on
     holding every emission of such an export uncharacterizable - a zero score
     under every method, beside resource categories that look right.
- 34: the unit table a database was built against is keyed by the spelling it
     writes rather than by that spelling lower-cased, so 'UnitConfig' loses a
     'Map Text Text' and gains a 'Map Text [(Text, UnitDef)]' in its place.
     'BuildInputs' holds one, so the stored bytes differ and every field after
     it would be read at the wrong offset.
- 35: a SimaPro file's own unit block is read, so an amount written in a unit
     only that file sizes is converted where it used to be left as written.
     Nothing changes type, so a cache written just before this would pass the
     fingerprint and keep the unconverted amount.

The signature is stored inside the cache file and checked on load.
If it doesn't match, the cache is automatically invalidated and rebuilt.
-}
schemaSignature :: Word64
schemaSignature =
    let Fingerprint hi lo = typeRepFingerprint (typeRep (Proxy :: Proxy Database))
     in hi `xor` lo `xor` 35

{- |
Helper function to parse UUID from Text with deterministic UUID generation fallback.
Uses the same namespace as Parser.hs to ensure consistency.
-}
testDataNamespace :: UUID.UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "acvengine.test")

parseUUID :: T.Text -> UUID.UUID
parseUUID txt = fromMaybe (UUID5.generateNamed testDataNamespace (BS.unpack $ T.encodeUtf8 txt)) (UUID.fromText txt)

-- | Namespace for EcoSpold1 UUID generation
ecospold1Namespace :: UUID.UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "ecospold1.ecoinvent.org")

-- | Generate activity UUID from activity name and location (for EcoSpold1)
generateActivityUUIDFromActivity :: Activity -> UUID.UUID
generateActivityUUIDFromActivity act =
    let key = activityName act <> ":" <> activityLocation act
     in UUID5.generateNamed ecospold1Namespace (BS.unpack $ T.encodeUtf8 key)

{- | The identifier an EcoSpold1 dataset publishes in its own file name,
@process_<uuid>.xml@ or plain @<uuid>.xml@.

Publishers that keep this identifier stable across releases (it survives a
rename, which a name-derived UUID does not) let two versions of a database be
compared dataset by dataset. Files named any other way — ecoinvent's EcoSpold1
exports are numbered, not identified — yield 'Nothing' and keep the minted
UUID.
-}
datasetUUIDFromPath :: FilePath -> Maybe UUID.UUID
datasetUUIDFromPath path =
    let base = T.pack (takeBaseName path)
     in UUID.fromText (fromMaybe base (T.stripPrefix "process_" base))

-- | Get reference product UUID from activity exchanges
getReferenceProductUUID :: Activity -> UUID.UUID
getReferenceProductUUID act =
    case filter exchangeIsReference (exchanges act) of
        (ref : _) -> exchangeFlowId ref
        [] -> UUID.nil -- No reference product found

{- | Key the activities of a name-linked file by @(activityUUID, productUUID)@,
naming every key two blocks claimed.

A block with no published identifier is named by its name folded in case and
its location, so two blocks whose names differ only in case claim one key, and
a map keeps one activity: the last read, as 'M.fromList' always did. Keeping it
in silence is what is not acceptable, since the other block's inventory goes
with it, so each contested key is described for the caller to report.
-}
indexActivities :: [Activity] -> (ActivityMap, [T.Text])
indexActivities activities =
    (M.map NE.head grouped, mapMaybe contested (M.elems grouped))
  where
    grouped :: M.Map (UUID.UUID, UUID.UUID) (NE.NonEmpty Activity)
    grouped =
        M.fromListWith
            (<>)
            [ ((SimaPro.generateActivityUUID act, getReferenceProductUUID act), pure act)
            | act <- activities
            ]
    contested :: NE.NonEmpty Activity -> Maybe T.Text
    contested group = describe (NE.head group) . NE.toList <$> snd (NE.uncons group)
    describe :: Activity -> [Activity] -> T.Text
    describe kept dropped =
        "'"
            <> activityName kept
            <> "' and "
            <> T.intercalate ", " ["'" <> activityName act <> "'" | act <- dropped]
            <> " are one activity at '"
            <> activityLocation kept
            <> "': they differ only in case, no process identifier tells them apart, and only the last read keeps its inventory"

-- | Type alias for supplier lookup index (with location)
type SupplierIndex = M.Map (T.Text, T.Text) (UUID.UUID, UUID.UUID)

{- | One activity of this database producing a given product name.

The reference-product unit lets the linker reject a candidate whose unit is
dimensionally incompatible with the consumer exchange (which the matrix builder
could not convert), instead of forming a link that aborts the whole load. The
activity name is what an input naming its supplier is matched against.
-}
data NameProducer = NameProducer
    { npActivityUUID :: !UUID.UUID
    , npProductUUID :: !UUID.UUID
    , npActivityName :: !T.Text
    , npLocation :: !T.Text
    , npObsolete :: !Bool
    , npReferenceUnit :: !T.Text
    }
    deriving (Eq, Show)

{- | Type alias for name-only supplier lookup (for SimaPro and Brightway Excel)
Maps normalizedProductName → every activity producing it, ranked by
'producerOrder' so the head is the one the tie-break picks.
-}
type NameOnlyIndex = M.Map T.Text (NE.NonEmpty NameProducer)

{- | Name-only supplier lookup for EcoSpold1, mapping a normalized product name
to every dataset producing it as @(activityUUID, productUUID, location)@.

Several is the ordinary shape here, not the exception: an EcoSpold1 product name
carries no location, so one name covers every geography the product is made in.
That is why the value is a 'NE.NonEmpty' and why both readers refuse a name that
covers more than one dataset instead of taking whichever it finds.
-}
type SupplierByNameWithLocation = M.Map T.Text (NE.NonEmpty (UUID.UUID, UUID.UUID, T.Text))

{- | Dataset number → the datasets carrying it, for EcoSpold1 Tier 1 linking.

One number names several datasets rather than one whenever the block it came
from declares more than one product: allocation rewrites such a block into one
dataset per coproduct, all of them carrying the number the block was read
under. Keeping one would leave the others unreachable by number, and the
reading order does not say which coproduct an input meant. The product name
does, and that is what Tier 1 already checks.
-}
type DatasetNumberIndex = M.Map Int (NE.NonEmpty (UUID.UUID, UUID.UUID))

-- | Information about an unlinked technosphere exchange
data UnlinkedExchange = UnlinkedExchange
    { ueFlowName :: !T.Text
    , ueLocation :: !T.Text
    }
    deriving (Eq, Ord, Show)

{- | An EcoSpold1 input whose dataset number and declared location name two
different datasets of the product. The number is what the file links, so it
wins; this is the trace that the file contradicted itself.
-}
data LocationOverride = LocationOverride
    { loConsumer :: !T.Text
    , loConsumerLocation :: !T.Text
    , loFlowName :: !T.Text
    , loDeclared :: !T.Text -- the location the exchange declares
    , loLinked :: !T.Text -- the location of the dataset its number names
    , loDatasetNumber :: !Int
    }
    deriving (Eq, Ord, Show)

{- | One input several activities of this database answer equally well.

The winner is then the tie-break's, not the data's: it reads the file's own
ranking (a block filed under an obsolete category supplies nothing, then
activity name, then location) and the identifier breaks the last tie. An input
that names its supplier narrows the field to the activities carrying that name,
and is counted here only when several of them do. Reported so a modeller can
say which one was meant.
-}
data AmbiguousProducer = AmbiguousProducer
    { apProduct :: !T.Text
    -- ^ Product name several activities produce
    , apChosen :: !T.Text
    -- ^ Activity the tie-break returned
    , apCandidates :: !Int
    -- ^ How many activities answered
    }
    deriving (Eq, Ord, Show)

{- | Summary of unlinked exchanges grouped by consumer activity, and of the
linked ones worth a word.
'Monoid' is hand-written: bare 'Int' has no canonical instance, and using
'Sum Int' would force every reader to unwrap.
-}
data UnlinkedSummary = UnlinkedSummary
    { usActivities :: !(M.Map T.Text [UnlinkedExchange]) -- consumer name → list of unlinked exchanges
    , usTotalLinks :: !Int
    , usFoundLinks :: !Int
    , usMissingLinks :: !Int
    , usLocationOverrides :: ![LocationOverride]
    , usAmbiguousProducers :: ![AmbiguousProducer]
    }
    deriving (Show)

instance Semigroup UnlinkedSummary where
    s1 <> s2 =
        UnlinkedSummary
            { usActivities = M.unionWith (++) (usActivities s1) (usActivities s2)
            , usTotalLinks = usTotalLinks s1 + usTotalLinks s2
            , usFoundLinks = usFoundLinks s1 + usFoundLinks s2
            , usMissingLinks = usMissingLinks s1 + usMissingLinks s2
            , usLocationOverrides = usLocationOverrides s1 ++ usLocationOverrides s2
            , usAmbiguousProducers = usAmbiguousProducers s1 ++ usAmbiguousProducers s2
            }

instance Monoid UnlinkedSummary where
    mempty =
        UnlinkedSummary
            { usActivities = M.empty
            , usTotalLinks = 0
            , usFoundLinks = 0
            , usMissingLinks = 0
            , usLocationOverrides = []
            , usAmbiguousProducers = []
            }

-- | Report grouped summary of unlinked exchanges, and of the ties broken blind
reportUnlinkedSummary :: UnlinkedSummary -> IO ()
reportUnlinkedSummary summary = do
    reportUnlinkedActivities summary
    reportAmbiguousProducers (usAmbiguousProducers summary)

{- | Report the inputs whose supplier the ranking picked among several.
Deduplicated on (product, chosen activity): one line per choice made, however
many activities of the database buy that product.
-}
reportAmbiguousProducers :: [AmbiguousProducer] -> IO ()
reportAmbiguousProducers [] = return ()
reportAmbiguousProducers ties = do
    reportProgress Warning $
        printf
            "%d product(s) several activities of this database answer equally well - nothing in the row says which, so the ranking chose"
            (length unique)
    forM_ unique $ \AmbiguousProducer{apProduct, apChosen, apCandidates} ->
        reportProgress Warning $
            printf
                "  - %s - %d producers, linked to %s"
                (T.unpack apProduct)
                apCandidates
                (T.unpack apChosen)
  where
    unique :: [AmbiguousProducer]
    unique = M.elems $ M.fromList [((apProduct t, apChosen t), t) | t <- ties]

-- | Report grouped summary of unlinked exchanges
reportUnlinkedActivities :: UnlinkedSummary -> IO ()
reportUnlinkedActivities summary
    | M.null (usActivities summary) = return () -- Nothing to report
    | otherwise = do
        let activities = usActivities summary
            activityCount = M.size activities
            -- Sort activities by number of unlinked exchanges (descending)
            sortedActivities = take 10 $ reverse $ sortOn' (length . snd) $ M.toList activities
            remainingCount = activityCount - length sortedActivities

        reportProgress Warning $
            printf "Unlinked activities: %d activities affected" activityCount

        -- Report top activities with their missing suppliers
        forM_ sortedActivities $ \(actName, unlinkedExchanges) -> do
            let uniqueExchanges = nub unlinkedExchanges -- Remove duplicates
                flowCount = length uniqueExchanges
                topFlows = take 3 uniqueExchanges
                remainingFlows = flowCount - length topFlows
            reportProgress Warning $
                printf "  - %s: %d missing suppliers" (T.unpack actName) flowCount
            forM_ topFlows $ \ue ->
                if T.null (ueLocation ue)
                    then reportProgress Warning $ printf "      * %s" (T.unpack (ueFlowName ue))
                    else reportProgress Warning $ printf "      * %s [%s]" (T.unpack (ueFlowName ue)) (T.unpack (ueLocation ue))
            when (remainingFlows > 0) $
                reportProgress Warning $
                    printf "      ... and %d more" remainingFlows

        when (remainingCount > 0) $
            reportProgress Warning $
                printf "  ... and %d more activities" remainingCount
  where
    sortOn' f = sortBy (\a b -> compare (f a) (f b))
    nub = map NE.head . NE.group . sort

-- | Normalize text for matching: lowercase, strip whitespace, normalize Unicode
normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.strip . normalizeUnicode

{- | Build supplier index: (normalizedProductName, location) → (activityUUID, productUUID)
For each activity, we index it by its reference product name + activity location
-}
buildSupplierIndex :: ActivityMap -> TechFlowDB -> SupplierIndex
buildSupplierIndex activities techFlowDb =
    M.fromList
        [ ((normalizeText (tfName flow), activityLocation act), (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
        ]

{- | Build name-only supplier index for SimaPro linking, on the reference
product name and nothing else.

When several activities produce one product name they are duplicates of each
other, a block exported twice, most often because one of the two has been
retired. The file says which: a retired block is filed under an obsolete
category, and 'activityIsObsolete' reads it, so the block still in service
supplies and the retired one supplies nothing. On the Agribalyse 4.0 export of
13 May 2026 that settles all ten of its duplicated products.

Two blocks the file gives no way to tell apart are ordered by activity name
then by location, never by identifier: a change in how identity is minted must
not move a supply chain. The identifier breaks the last tie only.

The duplication is a defect in its own right, and 'Database.Quality' reports it
as one, along with an input a retired block supplies.
-}
buildSupplierIndexByName :: UnitDB -> ActivityMap -> TechFlowDB -> NameOnlyIndex
buildSupplierIndexByName unitDB activities techFlowDb =
    M.map (NE.sortWith producerOrder) $
        M.fromListWith
            (<>)
            [ ( normalizeText (tfName flow)
              , NameProducer
                    { npActivityUUID = actUUID
                    , npProductUUID = prodUUID
                    , npActivityName = activityName act
                    , npLocation = activityLocation act
                    , npObsolete = activityIsObsolete act
                    , npReferenceUnit = getUnitNameForExchange unitDB ex
                    }
                    NE.:| []
              )
            | ((actUUID, prodUUID), act) <- M.toList activities
            , ex <- exchanges act
            , exchangeIsReference ex
            , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
            ]

-- | The rank a producer holds among those sharing a product name.
producerOrder :: NameProducer -> (Bool, T.Text, T.Text, UUID.UUID)
producerOrder p = (npObsolete p, npActivityName p, npLocation p, npActivityUUID p)

{- | Build the name-only supplier index for EcoSpold1 linking, keeping every
dataset a name covers rather than the last one seen.
-}
buildSupplierIndexByNameWithLocation :: ActivityMap -> TechFlowDB -> SupplierByNameWithLocation
buildSupplierIndexByNameWithLocation activities techFlowDb =
    M.fromListWith
        (flip (<>))
        [ (normalizeText (tfName flow), (actUUID, prodUUID, activityLocation act) NE.:| [])
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
        ]

{- | Fix EcoSpold1 activity links by resolving supplier references.
An input's dataset number names its supplier first, checked against the
product name; (flowName, location) is the fallback when the number resolves
to nothing, and the name alone when it covers a single dataset. A number that
contradicts the declared location is reported, not overruled.
Unlinked exchanges stay unlinked so that cross-DB linking can resolve them.
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
-}
fixEcoSpold1ActivityLinks :: M.Map T.Text T.Text -> DatasetNumberIndex -> SimpleDatabase -> IO SimpleDatabase
fixEcoSpold1ActivityLinks locationAliases dsIndex db = do
    let ctx = ecoSpold1LinkContext locationAliases dsIndex db
        (fixedActivities, summary) = fixAllActivities ctx (sdbActivities db)
    reportProgress Info $
        printf
            "Built supplier index with %d entries for activity linking (%d location aliases, %d name-only entries, %d dataset-number entries)"
            (M.size (elcSupplierIndex ctx))
            (M.size locationAliases)
            (M.size (elcNameIndex ctx))
            (M.size dsIndex)

    reportProgress Info $
        printf
            "Activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary
    reportLocationOverrides (usLocationOverrides summary)

    return $ db{sdbActivities = fixedActivities}

{- | One line per input whose dataset number and declared location named two
different datasets, sorted so one consumer's lines sit together.
-}
reportLocationOverrides :: [LocationOverride] -> IO ()
reportLocationOverrides [] = pure ()
reportLocationOverrides overrides = do
    reportProgress Warning $
        printf
            "Dataset number overrides the declared location on %d inputs (the number is the supplier the file links; the location is a label)"
            (length overrides)
    forM_ shown $ \o ->
        reportProgress Warning $
            printf
                "  - %s [%s]: %s declares %s, dataset %d is %s"
                (T.unpack (loConsumer o))
                (T.unpack (loConsumerLocation o))
                (T.unpack (loFlowName o))
                (T.unpack (loDeclared o))
                (loDatasetNumber o)
                (T.unpack (loLinked o))
    when (length overrides > length shown) $
        reportProgress Warning $
            printf "  ... and %d more" (length overrides - length shown)
  where
    shown :: [LocationOverride]
    shown = take 20 (sort overrides)

{- | Bundle of lookup tables threaded through EcoSpold1 activity-link resolution.
Previously these fields were passed as positional parameters through
'fixAllActivities' -> 'fixActivityExchanges' -> 'fixExchangeLink', each call
re-forwarding the same values. The record collapses the cascade to a single
argument and makes the dependencies explicit.
-}
data ExchangeLinkContext = ExchangeLinkContext
    { elcLocationAliases :: !(M.Map T.Text T.Text)
    , elcSupplierIndex :: !SupplierIndex
    , elcNameIndex :: !SupplierByNameWithLocation
    , elcDatasetIndex :: !DatasetNumberIndex
    , elcFlowDB :: !TechFlowDB
    , elcActivities :: !ActivityMap
    }

-- | The lookup tables 'fixAllActivities' links an EcoSpold1 database with.
ecoSpold1LinkContext :: M.Map T.Text T.Text -> DatasetNumberIndex -> SimpleDatabase -> ExchangeLinkContext
ecoSpold1LinkContext locationAliases dsIndex db =
    ExchangeLinkContext
        { elcLocationAliases = locationAliases
        , elcSupplierIndex = buildSupplierIndex (sdbActivities db) (sdbTechFlows db)
        , -- Name-only index, with location, for exchanges missing the location attribute
          elcNameIndex = buildSupplierIndexByNameWithLocation (sdbActivities db) (sdbTechFlows db)
        , elcDatasetIndex = dsIndex
        , elcFlowDB = sdbTechFlows db
        , elcActivities = sdbActivities db
        }

-- | Fix all activities and return statistics with unlinked summary
fixAllActivities :: ExchangeLinkContext -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivities ctx activities =
    let results = M.map (fixActivityExchanges ctx) activities
        summaries = map snd $ M.elems results
        combinedSummary = mconcat summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges and return (fixed activity, UnlinkedSummary)
fixActivityExchanges :: ExchangeLinkContext -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchanges ctx act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLink ctx act) (exchanges act)
        combinedSummary = mconcat summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link by (flowName, location) match.

Unlinked exchanges stay unlinked for cross-DB resolution.
Returns (fixed exchange, UnlinkedSummary)
-}
fixExchangeLink :: ExchangeLinkContext -> Activity -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLink ExchangeLinkContext{..} consumer ex@TechnosphereExchange{techFlowId = fid, techRole = role, techSupplierClaim = claim, techLocation = loc}
    | role == Input || role == ReferenceInput =
        let linked overrides actUUID prodUUID =
                ( ex{techFlowId = prodUUID, techActivityLinkId = Just actUUID}
                , mempty{usTotalLinks = 1, usFoundLinks = 1, usLocationOverrides = overrides}
                )
            unlinked flow lookupLoc =
                let ue = UnlinkedExchange (tfName flow) lookupLoc
                 in (ex, mempty{usActivities = M.singleton (activityName consumer) [ue], usTotalLinks = 1, usMissingLinks = 1})
         in case M.lookup fid elcFlowDB of
                Just flow ->
                    -- Tier 1: dataset-number lookup with name validation
                    case claimedNumber claim >>= \dsNum -> (,) dsNum <$> (M.lookup dsNum elcDatasetIndex >>= supplierNamed flow) of
                        Just (dsNum, (actUUID, prodUUID)) ->
                            linked (locationOverride flow dsNum (actUUID, prodUUID)) actUUID prodUUID
                        Nothing ->
                            -- Tier 2: name + location lookup
                            let soleSupplier = M.lookup (normalizeText (tfName flow)) elcNameIndex >>= sole
                                lookupLoc
                                    | T.null declaredLoc = maybe declaredLoc (\(_, _, actLoc) -> actLoc) soleSupplier
                                    | otherwise = declaredLoc
                                key = (normalizeText (tfName flow), lookupLoc)
                             in case M.lookup key elcSupplierIndex of
                                    Just (actUUID, prodUUID) -> linked [] actUUID prodUUID
                                    Nothing ->
                                        -- Tier 3: the name alone, and only when it
                                        -- covers a single dataset. A name shared by
                                        -- several geographies names none of them.
                                        case soleSupplier of
                                            Just (actUUID, prodUUID, _) -> linked [] actUUID prodUUID
                                            Nothing -> unlinked flow lookupLoc
                Nothing ->
                    (ex, mempty{usTotalLinks = 1, usMissingLinks = 1})
    | otherwise = (ex, mempty)
  where
    declaredLoc :: T.Text
    declaredLoc = fromMaybe loc (M.lookup loc elcLocationAliases)

    -- The number names several datasets whenever the block it came from was
    -- allocated into coproducts, and then only the product name says which one
    -- the input asked for. That name was already what Tier 1 checked when the
    -- number named a single dataset; here it also chooses.
    supplierNamed :: TechnosphereFlow -> NE.NonEmpty (UUID.UUID, UUID.UUID) -> Maybe (UUID.UUID, UUID.UUID)
    supplierNamed flow = find (produces (normalizeText (tfName flow)))

    produces :: T.Text -> (UUID.UUID, UUID.UUID) -> Bool
    produces name (_, prodUUID) = Just name == (normalizeText . tfName <$> M.lookup prodUUID elcFlowDB)

    -- The number named a dataset. When the declared location names another
    -- dataset of the same product, the number still wins: it is what the file
    -- links, and the results a database publishes follow it. The override is
    -- recorded so a reader can see the file contradict itself.
    locationOverride :: TechnosphereFlow -> Int -> (UUID.UUID, UUID.UUID) -> [LocationOverride]
    locationOverride flow dsNum supplierKey =
        [ LocationOverride
            { loConsumer = activityName consumer
            , loConsumerLocation = activityLocation consumer
            , loFlowName = tfName flow
            , loDeclared = loc
            , loLinked = activityLocation supplier
            , loDatasetNumber = dsNum
            }
        | not (T.null declaredLoc)
        , Just supplier <- [M.lookup supplierKey elcActivities]
        , activityLocation supplier /= declaredLoc
        , M.member (normalizeText (tfName flow), declaredLoc) elcSupplierIndex
        ]
fixExchangeLink _ _ ex@BiosphereExchange{} = (ex, mempty)
-- A WasteExchange in input direction (consumed by treatment) would benefit
-- from the same supplier-lookup logic as a technosphere Input, but at this
-- stage we leave waste links to the cross-DB linker (see CrossLinking) and
-- the downstream parsers. Pure pass-through here.
fixExchangeLink _ _ ex@WasteExchange{} = (ex, mempty)

{- |
Load all EcoSpold files with optimized parallel processing and deduplication.

This function implements a high-performance loading strategy:
1. **Chunked Processing**: Split files into optimal chunks (500 files/chunk)
2. **Controlled Parallelism**: Limit concurrent file handles (4 max)
3. **Memory Management**: Process chunks sequentially to control memory usage
4. **Deduplication**: Automatic flow and unit deduplication across files

Performance characteristics:
- Memory usage: ~2-4GB peak for Ecoinvent 3.8
- Processing time: ~45s for 18K activities (cold start)
- Parallelism: 4x concurrent file parsing within chunks
- Chunk size: 500 files (optimal for memory vs parallelism trade-off)

Used when no cache exists or caching is disabled.
-}
loadDatabase :: UC.UnitConfig -> FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabase unitConfig = loadDatabaseWithLocationAliases (defaultLoadOptions unitConfig)

{- | Load all EcoSpold files with location aliases
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")

The 'UnitConfig' is passed down to parsers so reference-product amounts can be
normalized to the canonical base unit of their dimension at ingest time.
-}
loadDatabaseWithLocationAliases :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabaseWithLocationAliases opts path = do
    loaded <- readSourceUnder opts path
    either (pure . Left) (\db -> Right db <$ reportKeyRefusals opts db) loaded

-- | The load itself, before anything is said about what the key refused.
readSourceUnder :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
readSourceUnder opts path = do
    -- Check if path is a file (SimaPro CSV) or directory (EcoSpold)
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then case map toLower (takeExtension path) of
            ".csv" -> loadSimaProCSV opts path
            ".xml" -> loadSingleEcoSpold1File opts path
            ".xlsx" -> loadBrightwayExcel opts path
            _ -> return $ Left $ T.pack $ "Unsupported file type: " ++ path
        else
            if isDir
                then do
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then ILCD.parseILCDDirectory (loUnitConfig opts) (loAllocation opts) path
                        else loadEcoSpoldDirectory opts path
                else return $ Left $ T.pack $ "Path does not exist: " ++ path

{- | What a load reads besides the files themselves.

Gathered rather than passed one by one: every format loader needs all three,
and three positional arguments of which two are maps invite a caller to swap
them.
-}
data LoadOptions = LoadOptions
    { loUnitConfig :: !UC.UnitConfig -- The merged unit table amounts are converted through
    , loLocationAliases :: !(M.Map T.Text T.Text) -- Wrong location -> correct location
    , loAllocation :: !AllocationKey -- How a multi-output block is divided
    }

{- | Say what the key this load asked for could not divide.

Loud rather than silent: such a block keeps every share its source declared
and still loses its column, and nothing else in the run says which key refused
it or why. The first ten are named, and a count says how many there were.
-}
reportKeyRefusals :: LoadOptions -> SimpleDatabase -> IO ()
reportKeyRefusals opts db = case loAllocation opts of
    Declared -> pure ()
    ByProperty prop -> unless (null refusals) $ do
        reportProgress Warning $
            show (length refusals) <> " blocks were not divided: the key names a property they do not carry"
        mapM_ (warn prop) (take 10 refusals)
  where
    refusals :: [(T.Text, PropertyRefusal)]
    refusals = propertyKeyRefusals (allocating opts (sdbUnits db)) (M.elems (sdbActivities db))

    warn :: AllocationProperty -> (T.Text, PropertyRefusal) -> IO ()
    warn prop (name, reason) =
        reportProgress Warning . T.unpack $
            name <> " has no column -- " <> describePropertyRefusal prop reason

-- | What 'allocate' reads, for a load of these options over these units.
allocating :: LoadOptions -> UnitDB -> Allocating
allocating opts unitDB =
    Allocating
        { alKey = loAllocation opts
        , alUnitConfig = loUnitConfig opts
        , alUnitDB = unitDB
        }

-- | The options a load with nothing configured runs under.
defaultLoadOptions :: UC.UnitConfig -> LoadOptions
defaultLoadOptions unitConfig =
    LoadOptions
        { loUnitConfig = unitConfig
        , loLocationAliases = M.empty
        , loAllocation = Declared
        }

{- | Say that an EcoSpold 2 dataset divided into several processes will come
back as one.

'buildProcEntry' reads a process's identity off the file name, which carries
one @activityUUID_productUUID@ pair whatever the file holds, so the coproducts
a key just separated all claim it and the registry keeps the last. Loud rather
than silent: the alternative is a database quietly missing the very products
the key was named to produce. Keying an EcoSpold 2 process on its own product
rather than on its file name is what would fix it.
-}
warnSplitKeyedOnFileName :: FilePath -> IO ()
warnSplitKeyedOnFileName file =
    reportProgress Warning $
        "only one product will be kept of "
            <> file
            <> ": an EcoSpold 2 process is identified by its file name, which names a single product"

{- | Allocate a parsed dataset before it is keyed: one entry per process
'allocate' splits it into, each carrying the file's flows and units, so the
entry is then keyed on its own reference product.
-}
allocateParsed :: LoadOptions -> ParsedDataset -> [ParsedDataset]
allocateParsed opts parsed =
    [ parsed{pdActivity = act}
    | act <- NE.toList (allocate (allocating opts unitMap) (pdActivity parsed))
    ]
  where
    unitMap :: UnitDB
    unitMap = M.fromList [(unitId u, u) | u <- pdUnits parsed]

-- | Load SimaPro CSV file
loadSimaProCSV :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSimaProCSV opts csvPath = do
    let unitConfig = loUnitConfig opts
    parsed <- SimaPro.parseSimaProCSV unitConfig csvPath
    case parsed of
        Left err -> return (Left err)
        Right (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB) ->
            if null activities
                then return $ Left "No activities found in SimaPro CSV file."
                else do
                    -- Build ActivityMap with generated ProcessIds
                    -- For SimaPro: use the same UUID for both activity and product (like EcoSpold1)
                    let (procMap, collisions) = indexActivities (allocateAll (allocating opts unitDB) activities)
                    forM_ collisions $ reportProgress Warning . T.unpack

                    -- Build initial database
                    let simpleDb = SimpleDatabase procMap techFlowDB bioFlowDB wasteFlowDB unitDB

                    -- Fix activity links using supplier lookup (same as EcoSpold1)
                    Right <$> fixSimaProActivityLinks unitConfig simpleDb

{- | Load a Brightway Excel (.xlsx) inventory.

Mirrors 'loadSimaProCSV': the parser returns the same 5-tuple, activities are
keyed @(activityUUID, referenceProductUUID)@, and within-file supplier
references are resolved by the shared name-based pass. Cross-database links to a
background database (e.g. ecoinvent) are resolved later by
'fixActivityLinksWithCrossDB', exactly as for SimaPro and EcoSpold.
-}
loadBrightwayExcel :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadBrightwayExcel opts xlsxPath = do
    let unitConfig = loUnitConfig opts
    parsed <- BrightwayExcel.parseBrightwayExcel unitConfig xlsxPath
    case parsed of
        Left err -> return $ Left err
        Right (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB)
            | null activities -> return $ Left "No activities found in Brightway Excel file."
            | otherwise -> do
                let (procMap, collisions) = indexActivities (allocateAll (allocating opts unitDB) activities)
                    simpleDb = SimpleDatabase procMap techFlowDB bioFlowDB wasteFlowDB unitDB
                forM_ collisions $ reportProgress Warning . T.unpack
                Right <$> fixSimaProActivityLinks unitConfig simpleDb

{- | Fix SimaPro activity links by resolving supplier references
Uses name-only matching (no location required) for SimaPro technosphere inputs
-}
fixSimaProActivityLinks :: UC.UnitConfig -> SimpleDatabase -> IO SimpleDatabase
fixSimaProActivityLinks unitConfig db = do
    let nameIndex = buildSupplierIndexByName (sdbUnits db) (sdbActivities db) (sdbTechFlows db)
    reportProgress Info $ printf "Built name-only supplier index with %d entries for SimaPro linking" (M.size nameIndex)

    -- Count and report statistics
    let (fixedActivities, summary) = fixAllActivitiesByName unitConfig (sdbUnits db) nameIndex (sdbTechFlows db) (sdbActivities db)

    reportProgress Info $
        printf
            "SimaPro activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary

    return $ db{sdbActivities = fixedActivities}

-- | Fix all activities using name-only matching
fixAllActivitiesByName :: UC.UnitConfig -> UnitDB -> NameOnlyIndex -> TechFlowDB -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivitiesByName unitConfig unitDB idx techFlowDb activities =
    let results = M.map (fixActivityExchangesByName unitConfig unitDB idx techFlowDb) activities
        summaries = map snd $ M.elems results
        combinedSummary = mconcat summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges using name-only matching
fixActivityExchangesByName :: UC.UnitConfig -> UnitDB -> NameOnlyIndex -> TechFlowDB -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchangesByName unitConfig unitDB idx techFlowDb act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLinkByName unitConfig unitDB idx techFlowDb (activityName act)) (exchanges act)
        combinedSummary = mconcat summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | A name-based supplier link is admissible only when the matrix builder could
later convert the consumer's exchange unit to the supplier's reference-product
unit. This mirrors the builder's own rule exactly (see 'Database.MatrixBuild'):
a conversion is needed only when the two units differ and both are non-empty,
and it must then succeed. So a link is safe when the units are identical, when
either side is empty, or when they are dimensionally compatible. Forming any
other link would abort the whole load — better to leave the input unlinked.
-}
linkUnitsCompatible :: UC.UnitConfig -> T.Text -> T.Text -> Bool
linkUnitsCompatible unitConfig consumerUnit supplierUnit =
    UC.unitKey unitConfig consumerUnit == UC.unitKey unitConfig supplierUnit
        || T.null (T.strip consumerUnit)
        || T.null (T.strip supplierUnit)
        || UC.unitsCompatible unitConfig consumerUnit supplierUnit

{- | Fix a single exchange's activity link using name-only matching.
Inputs and non-reference outputs (coproducts / avoided-production credits)
are eligible for relinking. A candidate is accepted only if its
reference-product unit is dimensionally compatible with the consumer exchange
('linkUnitsCompatible'); an incompatible candidate is skipped rather than
forming a link the matrix builder cannot convert — which would otherwise abort
the whole load.

An input that names the activity it buys from is honoured first: a product name
several activities of the database produce says which one only when the source
also names it, and a Brightway Excel workbook does. The ranked head is the
fallback, and when it decides alone among several the tie is reported.

There is no second guess. An input naming a product no activity of this
database produces stays unlinked, and the cross-database linker gets its turn
on it. Returns (fixed exchange, UnlinkedSummary).
-}
fixExchangeLinkByName :: UC.UnitConfig -> UnitDB -> NameOnlyIndex -> TechFlowDB -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLinkByName unitConfig unitDB idx techFlowDb consumerName ex@TechnosphereExchange{techFlowId = fid, techRole = role, techSupplierClaim = claim, techLocation = loc}
    | role == Input || role == ReferenceInput || role == AvoidedProduct =
        case M.lookup fid techFlowDb of
            Just flow ->
                let key = normalizeText (tfName flow)
                    consumerUnit = getUnitNameForExchange unitDB ex
                    relink p = ex{techFlowId = npProductUUID p, techActivityLinkId = Just (npActivityUUID p)}
                    -- Accept a candidate only when its reference unit can convert.
                    accept p
                        | linkUnitsCompatible unitConfig consumerUnit (npReferenceUnit p) = Just p
                        | otherwise = Nothing
                    unlinked =
                        ( ex
                        , mempty
                            { usActivities = M.singleton consumerName [UnlinkedExchange (tfName flow) loc]
                            , usTotalLinks = 1
                            , usMissingLinks = 1
                            }
                        )
                 in case M.lookup key idx >>= answering (claimedName claim) of
                        Nothing -> unlinked
                        Just answers -> case accept (NE.head answers) of
                            Nothing -> unlinked
                            Just p ->
                                ( relink p
                                , mempty
                                    { usTotalLinks = 1
                                    , usFoundLinks = 1
                                    , usAmbiguousProducers = tiedOn (tfName flow) answers
                                    }
                                )
            Nothing ->
                -- Flow not in technosphere map — shouldn't happen but be safe
                (ex, mempty{usTotalLinks = 1, usMissingLinks = 1})
    | otherwise = (ex, mempty) -- Reference products: nothing to relink
fixExchangeLinkByName _ _ _ _ _ ex@BiosphereExchange{} = (ex, mempty)
-- Waste link resolution is deferred to the cross-DB linker path.
fixExchangeLinkByName _ _ _ _ _ ex@WasteExchange{} = (ex, mempty)

-- | The dataset number a claim names, when it names one.
claimedNumber :: SupplierClaim -> Maybe Int
claimedNumber = \case
    ClaimByDatasetNumber n -> Just n
    ClaimByProduct -> Nothing
    ClaimById _ -> Nothing
    ClaimByName _ -> Nothing

-- | The supplier activity a claim names, when it names one by name.
claimedName :: SupplierClaim -> Maybe T.Text
claimedName = \case
    ClaimByName name -> Just name
    ClaimByProduct -> Nothing
    ClaimById _ -> Nothing
    ClaimByDatasetNumber _ -> Nothing

{- | Whether the source named an activity by an identifier another database
could carry. An input that did and still found no supplier there is a dangling
identity, which the dangling scan names; anything else is an unsupplied
product, which the blocker report names.

A dataset number is not one of those: it numbers a dataset inside its own file
and means nothing in another database, so a row carrying one is judged on its
product like any other.
-}
claimsAnActivityUUID :: SupplierClaim -> Bool
claimsAnActivityUUID = \case
    ClaimById _ -> True
    ClaimByDatasetNumber _ -> False
    ClaimByProduct -> False
    ClaimByName _ -> False

{- | The producers that answer an input, best first.

An input naming no activity is answered by every producer of the product it
asks for. One that names an activity is answered by the producers carrying that
name, and by nothing else: a name this database does not carry is a reference
to another database, so the input stays here unlinked and the cross-database
linker gets its turn on it. The name is matched the way every other name here
is, on 'normalizeText'.
-}
answering :: Maybe T.Text -> NE.NonEmpty NameProducer -> Maybe (NE.NonEmpty NameProducer)
answering Nothing producers = Just producers
answering (Just name) producers = NE.nonEmpty (NE.filter named producers)
  where
    named :: NameProducer -> Bool
    named p = normalizeText name == normalizeText (npActivityName p)

{- | The tie an input left the ranking to break: several producers answered it
equally well, and nothing in the row says which.
-}
tiedOn :: T.Text -> NE.NonEmpty NameProducer -> [AmbiguousProducer]
tiedOn productName answers
    | count <= 1 = []
    | otherwise =
        [ AmbiguousProducer
            { apProduct = productName
            , apChosen = npActivityName (NE.head answers)
            , apCandidates = count
            }
        ]
  where
    count :: Int
    count = NE.length answers

{- | Recursively collect files under @dir@ whose lowercased extension matches
@ext@. Lets an EcoSpold package load from its root even when the .spold datasets
sit in a subdirectory (e.g. ecoinvent's datasets/).
-}
findFilesByExtRecursive :: String -> FilePath -> IO [FilePath]
findFilesByExtRecursive ext =
    fmap (filter ((== ext) . map toLower . takeExtension)) . listDirectoryRecursive

-- | Load EcoSpold files from directory
loadEcoSpoldDirectory :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadEcoSpoldDirectory opts dir = do
    reportProgress Info "Scanning directory for EcoSpold files"
    files <- listDirectory dir
    -- .spold datasets may live in a subdirectory (e.g. ecoinvent's datasets/),
    -- so find them recursively. .xml (EcoSpold1) stays top-level to avoid
    -- sweeping up MasterData/metadata XML that sits beside the datasets.
    spold2Files <- findFilesByExtRecursive ".spold" dir
    let spold1Files = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]

    -- Determine which format to use based on what's found
    case (spold2Files, spold1Files) of
        ([], []) -> return $ Left $ T.pack $ "No EcoSpold files found in directory: " ++ dir
        ([], [singleXml]) -> do
            -- Single XML file: likely a multi-dataset EcoSpold1 file
            reportProgress Info $ "Found single EcoSpold1 file: " ++ singleXml
            loadSingleEcoSpold1File opts singleXml
        ([], xs) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold1 (.XML) files for processing"
            loadWithWorkerParallelism xs True
        (xs, []) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold2 (.spold) files for processing"
            loadWithWorkerParallelism xs False
        (xs, _) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold2 (.spold) files for processing"
            loadWithWorkerParallelism xs False -- Prefer EcoSpold2 if both present
  where
    locationAliases :: MS.Map T.Text T.Text
    locationAliases = loLocationAliases opts
    -- Worker-based parallelism: divide files among N workers, all process in parallel
    loadWithWorkerParallelism :: [FilePath] -> Bool -> IO (Either T.Text SimpleDatabase)
    loadWithWorkerParallelism allFiles isEcoSpold1 = do
        -- Get actual number of CPU capabilities (respects +RTS -N)
        numWorkers <- getNumCapabilities
        let workers = distributeFiles numWorkers allFiles
        reportProgress Info $
            printf
                "Processing %d files with %d parallel workers (%d files per worker)"
                (length allFiles)
                numWorkers
                (length allFiles `div` numWorkers)

        -- Process all workers in parallel
        startTime <- getCurrentTime
        scoped <- inheritLogScope
        results <- mapConcurrently (scoped . processWorker startTime isEcoSpold1) (zip [1 ..] workers)

        -- Check for errors from any worker
        let errors = lefts results
        case errors of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let !harvested = mconcat (rights results)

                endTime <- getCurrentTime
                let totalDuration = realToFrac $ diffUTCTime endTime startTime
                let totalFiles = length allFiles
                let avgFilesPerSec = fromIntegral totalFiles / totalDuration
                let totalRawFlows = hvRawFlows harvested
                let totalRawUnits = hvRawUnits harvested
                let totalFlows = M.size (hvTechFlows harvested) + M.size (hvBioFlows harvested)
                let flowDeduplication = if totalRawFlows > 0 then 100.0 * (1.0 - fromIntegral totalFlows / fromIntegral totalRawFlows) else 0.0 :: Double
                let unitDeduplication = if totalRawUnits > 0 then 100.0 * (1.0 - fromIntegral (M.size (hvUnits harvested)) / fromIntegral totalRawUnits) else 0.0 :: Double

                reportProgress Info $ printf "Parsing completed (%s, %.1f files/sec):" (formatDuration totalDuration) avgFilesPerSec
                reportProgress Info $ printf "  Activities: %d processes" (M.size (hvActivities harvested))
                reportProgress Info $
                    printf
                        "  Flows: %d tech + %d bio (%.1f%% deduplication from %d raw)"
                        (M.size (hvTechFlows harvested))
                        (M.size (hvBioFlows harvested))
                        flowDeduplication
                        totalRawFlows
                reportProgress Info $
                    printf
                        "  Units: %d unique (%.1f%% deduplication from %d raw)"
                        (M.size (hvUnits harvested))
                        unitDeduplication
                        totalRawUnits
                reportMemoryUsage "Final parsing memory usage"

                -- For EcoSpold1: fix activity links using supplier lookup table
                let simpleDb = harvestDatabase harvested
                if isEcoSpold1
                    then Right <$> fixEcoSpold1ActivityLinks locationAliases (hvDatasetNumbers harvested) simpleDb
                    else return $ Right simpleDb

    -- Process one worker's share of files
    processWorker :: UTCTime -> Bool -> (Int, [FilePath]) -> IO (Either T.Text Harvest)
    processWorker _startTime isEcoSpold1 (workerNum, workerFiles) = do
        workerStartTime <- getCurrentTime
        reportProgress Info $ printf "Worker %d started: processing %d files" workerNum (length workerFiles)

        -- Parse all files for this worker using appropriate parser. Both paths
        -- return a 'ParsedDataset'; EcoSpold 2 addresses its suppliers by UUID
        -- rather than by dataset number, so it fills neither of those fields.
        let parseFile =
                if isEcoSpold1
                    then streamParseActivityAndFlowsFromFile1
                    else streamParseActivityAndFlowsFromFile
        workerResults <- mapM parseFile workerFiles
        let paired = zipWith (\f r -> fmap (f,) r) workerFiles workerResults
        let (errs, oks) = partitionEithers paired
        forM_ errs $ \e ->
            reportProgress Warning e
        let split = [(f, allocateParsed opts r) | (f, r) <- oks]
            (okFiles, okResults) = unzip [(f, r') | (f, rs) <- split, r' <- rs]
        unless isEcoSpold1 $
            mapM_ (warnSplitKeyedOnFileName . fst) (filter ((> 1) . length . snd) split)
        let procEntries = zipWith (buildProcEntry isEcoSpold1) okFiles (map pdActivity okResults)

        case lefts procEntries of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let !harvested = harvestOf (zip (map fst (rights procEntries)) okResults)

                workerEndTime <- getCurrentTime
                let workerDuration = realToFrac $ diffUTCTime workerEndTime workerStartTime
                let filesPerSec = fromIntegral (length workerFiles) / workerDuration
                reportProgress Info $
                    printf
                        "Worker %d completed: %d activities, %d tech + %d bio + %d waste flows (%s, %.1f files/sec)"
                        workerNum
                        (M.size (hvActivities harvested))
                        (M.size (hvTechFlows harvested))
                        (M.size (hvBioFlows harvested))
                        (M.size (hvWasteFlows harvested))
                        (formatDuration workerDuration)
                        filesPerSec

                return $ Right harvested

    -- Build a single process entry, returning Either for error handling
    buildProcEntry :: Bool -> FilePath -> Activity -> Either T.Text ((UUID, UUID), Activity)
    buildProcEntry True filepath activity =
        -- EcoSpold1: prefer the identifier the file itself carries, so a
        -- dataset keeps its identity across releases; mint from name and
        -- location only when the file name carries none.
        let actUUID = fromMaybe (generateActivityUUIDFromActivity activity) (datasetUUIDFromPath filepath)
            prodUUID = getReferenceProductUUID activity
         in Right ((actUUID, prodUUID), activity)
    buildProcEntry False filepath activity =
        -- EcoSpold2: Parse UUIDs from filename
        let filename = T.pack $ takeBaseName filepath
         in case T.splitOn "_" filename of
                [actUUIDText, prodUUIDText] ->
                    let actUUID = parseUUID actUUIDText
                        prodUUID = parseUUID prodUUIDText
                     in Right ((actUUID, prodUUID), activity)
                _ -> Left $ T.pack $ "Invalid filename format (expected activityUUID_productUUID.spold): " ++ filepath

{- | Load a single EcoSpold1 file containing multiple datasets
This handles files where <ecoSpold> contains multiple <dataset> elements.

A file holding exactly one dataset is keyed like the per-file directory
path: the identifier its file name carries wins over the minted UUID.
Several datasets share one file name, so none of them can claim it.
-}
loadSingleEcoSpold1File :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSingleEcoSpold1File opts filepath = do
    let locationAliases = loLocationAliases opts
    reportProgress Info "Parsing multi-dataset EcoSpold1 file..."
    parsed <- streamParseAllDatasetsFromFile1 filepath
    reportProgress Info $ "Parsed " ++ show (length parsed) ++ " datasets from file"
    let results = concatMap (allocateParsed opts) parsed

    -- Build activity map from all parsed activities
    let fileUUID = case results of
            [_] -> datasetUUIDFromPath filepath
            _ -> Nothing
        !harvested = harvestOf [(datasetKey fileUUID r, r) | r <- results]
        simpleDb = harvestDatabase harvested

    reportProgress Info $ printf "  Activities: %d processes" (M.size (hvActivities harvested))
    reportProgress Info $ printf "  Flows: %d tech + %d bio + %d waste (from %d raw)" (M.size (hvTechFlows harvested)) (M.size (hvBioFlows harvested)) (M.size (hvWasteFlows harvested)) (hvRawFlows harvested)
    reportProgress Info $ printf "  Units: %d unique (from %d raw)" (M.size (hvUnits harvested)) (hvRawUnits harvested)

    Right <$> fixEcoSpold1ActivityLinks locationAliases (hvDatasetNumbers harvested) simpleDb
  where
    datasetKey :: Maybe UUID.UUID -> ParsedDataset -> (UUID.UUID, UUID.UUID)
    datasetKey fileUUID parsed =
        let activity = pdActivity parsed
            actUUID = fromMaybe (generateActivityUUIDFromActivity activity) fileUUID
         in (actUUID, getReferenceProductUUID activity)

{- |
Generate filename for matrix cache.

Matrix caches store pre-computed sparse matrices (technosphere A,
biosphere B) enabling direct LCA solving without matrix construction.

The cache lives next to the configured source path
(@takeDirectory sourcePath@). For uploaded databases this is the
upload directory; for preloaded/host-mounted databases it is the
mount directory. Either way the cache persists across restarts as
long as the source location does.

Cache invalidation is handled by a schema signature stored inside
the cache file, not by the filename.
-}
generateMatrixCacheFilename :: T.Text -> FilePath -> IO FilePath
generateMatrixCacheFilename dbName sourcePath = do
    let cacheFilename = "volca.cache." ++ T.unpack dbName ++ ".bin"
        cacheDir = takeDirectory sourcePath
    createDirectoryIfMissing True cacheDir
    return $ cacheDir </> cacheFilename

{- |
Load Database with pre-computed matrices from cache (second-tier).

This is the fastest loading method (~0.5s) as it bypasses both
XML parsing and matrix construction. The Database includes:
- All activities, flows, units (from SimpleDatabase)
- Pre-built indexes for fast querying
- Pre-computed sparse matrices (technosphere A, biosphere B)
- Activity and flow UUID mappings for matrix operations

Returns Nothing if no matrix cache exists, or if the one found was built with
other inputs than the ones in force: what it holds would not be what a fresh
read produces.
-}
loadCachedDatabaseWithMatrices :: T.Text -> FilePath -> BuildInputs -> IO (Maybe Database)
loadCachedDatabaseWithMatrices dbName dataDir inputs = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    zstdExists <- doesFileExist zstdFile
    if not zstdExists
        then do
            reportCacheOperation "No matrix cache found"
            return Nothing
        else do
            -- Delegate to the shared reader; a Nothing here means the cache
            -- is corrupted or was written by another schema, and the database
            -- is rebuilt from source, as it is when the cache was built under
            -- other inputs. The file is left alone: a rebuild overwrites it
            -- anyway, and a host that ships only the cache (see
            -- 'Manager.loadDatabaseRawWithCrossDB') has no source to rebuild
            -- from, so deleting it there destroyed the only copy of the data.
            result <- loadCompressedCacheFile zstdFile
            case result of
                Just db | dbBuiltWith db == inputs -> return (Just db)
                Just db -> do
                    reportCacheOperation $ "Cache was built with another " ++ builtWithDifference (dbBuiltWith db) inputs
                    rebuild
                Nothing -> rebuild
  where
    rebuild :: IO (Maybe Database)
    rebuild = Nothing <$ reportCacheOperation "Will rebuild database from source files"

-- | Which of the build inputs a cache disagrees with, for the log line.
builtWithDifference :: BuildInputs -> BuildInputs -> String
builtWithDifference cached current =
    intercalate " and " $
        ["unit table" | biUnitConfig cached /= biUnitConfig current]
            ++ ["location aliases" | biLocationAliases cached /= biLocationAliases current]
            ++ ["allocation key" | biAllocation cached /= biAllocation current]

-- | Load compressed (.bin.zst) cache file with header validation
loadCompressedCacheFile :: FilePath -> IO (Maybe Database)
loadCompressedCacheFile zstdFile = do
    reportCacheInfo zstdFile
    catch
        ( withProgressTiming Cache "Matrix cache load with zstd decompression" $ do
            contents <- BS.readFile zstdFile
            -- Check minimum size for header (16 bytes)
            if BS.length contents < 16
                then do
                    reportCacheOperation "Cache file too small (missing header)"
                    return Nothing
                else do
                    let (header, compressed) = BS.splitAt 16 contents
                        (magic, sigBytes) = BS.splitAt 8 header
                    -- Check magic bytes
                    if magic /= cacheMagic
                        then do
                            reportCacheOperation "Invalid cache file (wrong magic bytes)"
                            return Nothing
                        else do
                            -- Check schema signature
                            let storedSig = decodeEx sigBytes :: Word64
                            if storedSig /= schemaSignature
                                then do
                                    reportCacheOperation $ "Schema mismatch: cache=" ++ show storedSig ++ " current=" ++ show schemaSignature
                                    reportCacheOperation "Cache will be rebuilt with new schema"
                                    return Nothing
                                else do
                                    -- Decompress and decode the payload
                                    case Zstd.decompress compressed of
                                        Zstd.Skip -> do
                                            reportError "Zstd decompression failed: Skip"
                                            return Nothing
                                        Zstd.Error err -> do
                                            reportError $ "Zstd decompression failed: " ++ show err
                                            return Nothing
                                        Zstd.Decompress decompressed -> do
                                            let !db = decodeEx decompressed
                                            -- Force full evaluation to prevent lazy thunk buildup
                                            db' <- evaluate (force db)
                                            reportCacheOperation $
                                                "Matrix cache loaded: "
                                                    ++ show (dbActivityCount db')
                                                    ++ " activities, "
                                                    ++ show (VU.length $ dbTechnosphereTriples db')
                                                    ++ " tech entries, "
                                                    ++ show (VU.length $ dbBiosphereTriples db')
                                                    ++ " bio entries (decompressed)"
                                            return (Just db')
        )
        ( \(e :: SomeException) -> do
            reportError $ "Compressed cache load failed: " ++ show e
            reportCacheOperation "The compressed cache file is corrupted or incompatible"
            return Nothing
        )

{- |
Save Database with pre-computed matrices to cache.

Serializes the complete Database including sparse matrices to enable
ultra-fast startup (~0.5s load time). The cache file includes:
- 8 bytes magic ("VOLCACHE")
- 8 bytes schema signature (auto-generated from type structure)
- Zstd compressed Database binary

Should be called after matrix construction is complete.
-}
saveCachedDatabaseWithMatrices :: T.Text -> FilePath -> Database -> IO ()
saveCachedDatabaseWithMatrices dbName dataDir db = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    reportCacheOperation $ "Saving Database with matrices to compressed cache: " ++ zstdFile
    withProgressTiming Cache "Matrix cache save with zstd compression" $ do
        -- Serialize to ByteString (store returns strict ByteString)
        let serialized = encode db
        -- Compress with zstd (level 1 = fast compression, ~5% larger than level 3)
        let compressed = Zstd.compress 1 serialized
        -- Build header: magic (8 bytes) + schema signature (8 bytes)
        let signatureBytes = encode schemaSignature
        let header = cacheMagic <> signatureBytes
        -- Write header + compressed data
        BS.writeFile zstdFile (header <> compressed)
        reportCacheOperation $
            "Matrix cache saved ("
                ++ show (dbActivityCount db)
                ++ " activities, "
                ++ show (VU.length $ dbTechnosphereTriples db)
                ++ " tech entries, "
                ++ show (VU.length $ dbBiosphereTriples db)
                ++ " bio entries, compressed)"

--------------------------------------------------------------------------------
-- Cross-Database Linking
--------------------------------------------------------------------------------

{- | CrossDBLinkingStats, mempty, (<>),
  crossDBLinksCount, unresolvedCount, crossDBBySource
  are now defined in Types and re-exported from this module.
-}

{- | Load EcoSpold files with cross-database linking support.

This function loads EcoSpold files and then attempts to resolve unlinked
technosphere exchanges by searching across other already-loaded databases.

The loading sequence:
1. Parse XML files into SimpleDatabase
2. Build supplier index for THIS database
3. Attempt linking within THIS database (standard behavior)
4. For remaining unlinked exchanges, search OTHER databases
5. Report linking summary with cross-DB statistics
-}
loadDatabaseWithCrossDBLinking ::
    -- | What this database is read under: units, aliases, allocation key
    LoadOptions ->
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database for name matching
    SynonymDB ->
    -- | Location hierarchy (empty = use built-in)
    M.Map Location [Location] ->
    -- | Geography policy for this database
    GeographyPolicy ->
    -- | Path to load from
    FilePath ->
    IO (Either T.Text (SimpleDatabase, CrossDBLinkingStats))
loadDatabaseWithCrossDBLinking opts otherIndexes synonymDB locationHier policy path = do
    let unitConfig = loUnitConfig opts
    result <- loadDatabaseWithLocationAliases opts path
    case result of
        Left err -> return $ Left err
        Right simpleDb -> do
            -- Read every unit the database declares against the unit table.
            let !verdict = UC.judgeUnits unitConfig (map unitName (M.elems (sdbUnits simpleDb)))
                !unknownUnits = S.fromList (UC.uvUnknown verdict)
            mapM_ (reportProgress Warning . T.unpack . respeltLine) (UC.uvRespelt verdict)
            unless (S.null unknownUnits) $
                reportProgress Warning $
                    printf
                        "%d unknown unit(s): %s — add to the [[units]] CSV file"
                        (S.size unknownUnits)
                        (T.unpack $ T.intercalate ", " $ map (\u -> "\"" <> u <> "\"") $ S.toList unknownUnits)
            case unitRefusals verdict of
                [] -> loadOn simpleDb unknownUnits
                refusals -> return $ Left (T.intercalate "; " refusals)
  where
    {- The table spells it differently, and only one way, so the reading is
    settled. Which side is the misspelling is not for the loader to say: it
    reports the row it read the amount against and lets the reader judge. -}
    respeltLine :: (T.Text, T.Text) -> T.Text
    respeltLine (written, spelling) =
        "unit \"" <> written <> "\" is written \"" <> spelling <> "\" in the unit table; amounts read against that row"

    {- Both refusals say the same thing: the evidence leaves more than one
    reading, and a guess between them is worth whatever separates the
    candidates. Neither ranks them. -}
    unitRefusals :: UC.UnitVerdict -> [T.Text]
    unitRefusals verdict =
        [ "unit \"" <> written <> "\" could be " <> T.intercalate " or " (map quoted candidates)
        | (written, candidates) <- UC.uvAmbiguous verdict
        ]
            <> [ "this database tells apart "
                    <> T.intercalate " and " (map quoted written)
                    <> ", and the unit table has only "
                    <> quoted row
                    <> " for all of them"
               | (row, written) <- UC.uvCollapsed verdict
               ]
            <> [ "correct the spelling in the source, or give the missing unit its own row in the [[units]] CSV file"
               | not (null (UC.uvAmbiguous verdict)) || not (null (UC.uvCollapsed verdict))
               ]

    quoted :: T.Text -> T.Text
    quoted t = "\"" <> t <> "\""

    loadOn :: SimpleDatabase -> S.Set T.Text -> IO (Either T.Text (SimpleDatabase, CrossDBLinkingStats))
    loadOn simpleDb unknownUnits = do
        -- If there are other databases to search, perform cross-DB linking
        let !totalInputs = countTotalTechInputs simpleDb
        if null otherIndexes
            then do
                -- No cross-DB linking needed
                let !stats = mempty{cdlUnknownUnits = unknownUnits, cdlTotalInputs = totalInputs}
                reportCrossDBLinkingStats (M.size (sdbActivities simpleDb)) stats
                return $ Right (simpleDb, stats)
            else do
                -- Perform cross-database linking using pre-built indexes
                (linkedDb, stats) <-
                    fixActivityLinksWithCrossDB
                        otherIndexes
                        synonymDB
                        (loUnitConfig opts)
                        locationHier
                        policy
                        simpleDb
                return $ Right (linkedDb, stats{cdlUnknownUnits = unknownUnits})

{- | Fix activity links using cross-database lookup.

For each unlinked technosphere input (one with no activityLinkId),
search across other loaded databases to find a matching supplier.

Matching criteria:
- Product name must match (exact, synonym, or fuzzy)
- Units must be compatible
- Location scoring with hierarchy fallback

Cross-database links are stored in CrossDBLinkingStats.cdlLinks for use
in chained inventory solving. The exchanges are NOT modified - they
remain "unlinked" from the perspective of the internal matrix, but the
CrossDBLinks provide the information needed to resolve them at solve time.
-}
fixActivityLinksWithCrossDB ::
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database
    SynonymDB ->
    -- | Unit configuration
    UC.UnitConfig ->
    -- | Location hierarchy (code → parent codes)
    M.Map Location [Location] ->
    -- | Geography policy for this database
    GeographyPolicy ->
    -- | Database to fix
    SimpleDatabase ->
    IO (SimpleDatabase, CrossDBLinkingStats)
fixActivityLinksWithCrossDB indexedDbs synonymDB unitConfig locationHier policy db = do
    -- Count unlinked exchanges before
    let unlinkedBefore = countUnlinkedExchanges db
        !totalInputs = countTotalTechInputs db

    -- If no unlinked exchanges, skip
    if unlinkedBefore == 0
        then do
            reportProgress Info "No unlinked exchanges to resolve via cross-DB linking"
            return (db, mempty{cdlTotalInputs = totalInputs})
        else do
            reportProgress Info $
                printf
                    "Cross-database linking: %d unlinked exchanges, searching %d database(s)..."
                    unlinkedBefore
                    (length indexedDbs)

            -- Report index stats
            forM_ indexedDbs $ \idb ->
                reportProgress Info $
                    printf
                        "  - %s: %d products indexed"
                        (T.unpack (idbName idb))
                        (M.size (idbByProductName idb))

            -- Build the linking context with pre-built indexes
            let linkingCtx =
                    LinkingContext
                        { lcIndexedDatabases = indexedDbs
                        , lcSynonymDB = synonymDB
                        , lcUnitConfig = unitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = if M.null locationHier then locationHierarchy else locationHier
                        , lcGeographyPolicy = policy
                        , lcSupplierAliases = emptyAliasMap
                        }

            -- Process all activities to find cross-DB links
            reportProgress Info "Finding cross-database suppliers..."
            let stats =
                    findAllCrossDBLinks
                        linkingCtx
                        (sdbTechFlows db)
                        (sdbWasteFlows db)
                        (sdbUnits db)
                        (sdbActivities db)

            -- Report statistics
            let !stats' = stats{cdlTotalInputs = totalInputs}
            reportCrossDBLinkingStats (M.size (sdbActivities db)) stats'

            -- Return the original database unchanged, along with the cross-DB links
            -- The links will be stored in the Database.dbCrossDBLinks field later
            return (db, stats')

{- | Inputs that demand a supplier — the exact set the matrix builder tries to
resolve in 'Database.MatrixBuild.techTriple'.

Biosphere flows need no supplier. Reference exchanges sit on the diagonal of
@(I-A)@ and are skipped by the matrix builder, so a treatment process's
'ReferenceInput' is a self-edge, not a supplier demand — counting it would drag
completeness below 100% for a perfectly solvable database. Waste *outputs* are
generated, not demanded; only waste/technosphere *inputs* remain.

An input of zero demands nothing either. It reaches no matrix: the builders drop
a zero triple ('Database.MatrixBuild.triplesFor' and 'buildBioTriples'), and
'missingActivityWarning' does not warn about one, so counting it as an unmet
demand would report a gap no solve can encounter.

Whether a row should be *linked* is the other question, and 'namesASupplier'
answers it. A disabled input still points at a producer, and the author who
re-enables it expects the link to be there.
-}
isSupplierDemand :: Exchange -> Bool
isSupplierDemand ex = namesASupplier ex && exchangeAmount ex /= 0

{- | An input that names a supplier, whatever it asks of it. The linker's
question, where 'isSupplierDemand' is the completeness report's.
-}
namesASupplier :: Exchange -> Bool
namesASupplier ex =
    not (isBiosphereExchange ex)
        && exchangeIsInput ex
        && not (exchangeIsReference ex)

{- | True when a staged input resolves to a producer activity present in the
same database — the @(activityLinkId, flowId)@ branch of
'Database.MatrixBuild.findProducer'.

The process-link branch is deliberately omitted: a 'ProcessId' is an interned
index assigned only when matrices are built, so it never exists on a
'SimpleDatabase' (it is always 'Nothing' here). The loaded-database counterpart
'collectDanglingProductNames' has the real lookup and calls 'findProducer'
directly, honouring both branches.

An absent @activityLinkId@ (SimaPro inputs awaiting cross-DB linking, or a genuine
orphan) is never an internal producer. A *non-nil* link to an activity absent
from this database — e.g. a partial EcoSpold2 import that references ecoinvent
background activities it doesn't ship — is unresolved too: the matrix builder
silently drops such an exchange, so it must count as unlinked rather than
masquerade as a resolved internal link.
-}
hasInternalProducer :: SimpleDatabase -> Exchange -> Bool
hasInternalProducer db ex =
    case exchangeActivityLinkId ex of
        Nothing -> False
        Just actUUID -> M.member (actUUID, exchangeFlowId ex) (sdbActivities db)

{- | Re-resolve the cross-DB links of a 'SimpleDatabase' against the given
dependencies, optionally aliasing supplier names. Unlike
'fixActivityLinksWithCrossDB' this always recomputes — a relink must re-resolve
already-linked exchanges, e.g. to apply a new alias map — and threads @aliases@
into 'lcSupplierAliases', so a staged relink behaves exactly like the loaded one.
-}
relinkSimpleDatabase ::
    [IndexedDatabase] ->
    SynonymDB ->
    UC.UnitConfig ->
    M.Map Location [Location] ->
    GeographyPolicy ->
    AliasMap ->
    SimpleDatabase ->
    CrossDBLinkingStats
relinkSimpleDatabase indexedDbs synonymDB unitConfig locationHier policy aliases db =
    let ctx =
            LinkingContext
                { lcIndexedDatabases = indexedDbs
                , lcSynonymDB = synonymDB
                , lcUnitConfig = unitConfig
                , lcThreshold = defaultLinkingThreshold
                , lcLocationHierarchy = if M.null locationHier then locationHierarchy else locationHier
                , lcGeographyPolicy = policy
                , lcSupplierAliases = aliases
                }
        stats = findAllCrossDBLinks ctx (sdbTechFlows db) (sdbWasteFlows db) (sdbUnits db) (sdbActivities db)
     in stats{cdlTotalInputs = countTotalTechInputs db}

{- | Product names of technosphere demands with no resolved internal producer —
the supplier gaps surfaced on the setup page. Covers both nil-link inputs and
non-nil links whose target activity is absent (partial EcoSpold2 imports).
-}
collectUnlinkedProductNames :: SimpleDatabase -> M.Map T.Text Int
collectUnlinkedProductNames db =
    M.fromListWith
        (+)
        [ (tfName flow, 1)
        | act <- M.elems (sdbActivities db)
        , ex@TechnosphereExchange{} <- exchanges act
        , isSupplierDemand ex
        , not (hasInternalProducer db ex)
        , Just flow <- [M.lookup (exchangeFlowId ex) (sdbTechFlows db)]
        ]

-- | Count supplier demands with no resolved internal producer.
countUnlinkedExchanges :: SimpleDatabase -> Int
countUnlinkedExchanges db =
    length
        [ ()
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isSupplierDemand ex
        , not (hasInternalProducer db ex)
        ]

-- | Count total supplier demands — the completeness denominator.
countTotalTechInputs :: SimpleDatabase -> Int
countTotalTechInputs db =
    length
        [ ()
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isSupplierDemand ex
        ]

{- | How many cross-DB links resolve each consumer @(activityUUID, productUUID,
flowId)@ triple. The engine resolves a demand by @(activityLinkId, flowId)@, so
one activity can consume the same product flow from several suppliers; counting
coverage (rather than testing set membership) lets the dangling scan drop exactly
the covered occurrences and still name a genuinely unresolved sibling.
-}
crossDBCoveredCounts :: [CrossDBLink] -> M.Map (UUID.UUID, UUID.UUID, UUID.UUID) Int
crossDBCoveredCounts links =
    M.fromListWith (+) [((cdlConsumerActUUID l, cdlConsumerProdUUID l, cdlConsumerFlowId l), 1) | l <- links]

{- | Tally dangling product names from @(consumer-triple, productName)@ pairs,
dropping per triple as many occurrences as 'crossDBCoveredCounts' already
covers. Inputs sharing a triple share a product name, so the surplus over the
covered count is the real gap.
-}
tallyDangling ::
    M.Map (UUID.UUID, UUID.UUID, UUID.UUID) Int ->
    [((UUID.UUID, UUID.UUID, UUID.UUID), T.Text)] ->
    M.Map T.Text Int
tallyDangling covered inputs =
    M.fromListWith
        (+)
        [ (name, surplus)
        | (triple, (name, n)) <- M.toList byTriple
        , let surplus = n - M.findWithDefault 0 triple covered
        , surplus > 0
        ]
  where
    byTriple =
        M.fromListWith
            (\(nm, a) (_, b) -> (nm, a + b))
            [(triple, (name, 1 :: Int)) | (triple, name) <- inputs]

{- | Product names of a *loaded* database's dangling internal links: non-nil
@activityLinkId@ inputs that 'findProducer' cannot resolve against the
database's own process lookup (the matrix builder silently drops them) *and*
that no cross-DB link supplies. The loaded-path counterpart that names the
supplier gaps a partial EcoSpold2 import leaves behind — distinct from nil-link
inputs, the cross-DB candidates already tracked in the linking stats.

Sharing 'findProducer' keeps this honest with the matrix: an input the matrix
routes is not dangling, however its activity link looks. Subtracting per-triple cross-DB
coverage ('crossDBCoveredCounts') keeps it honest the other way: once the
matching background is loaded, a UUID- or attribute-resolved input is supplied,
not missing.
-}
collectDanglingProductNames :: Database -> M.Map T.Text Int
collectDanglingProductNames db =
    tallyDangling
        (crossDBCoveredCounts (dbCrossDBLinks db))
        [ ((actUUID, prodUUID, exchangeFlowId ex), tfName flow)
        | ((actUUID, prodUUID), act) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db))
        , ex@TechnosphereExchange{} <- exchanges act
        , isSupplierDemand ex
        , isNothing (findProducer (dbProcessIdLookup db) ex)
        , claimsAnActivityUUID (exchangeSupplierClaim ex)
        , Just flow <- [M.lookup (exchangeFlowId ex) (dbTechFlows db)]
        ]

{- | Staged-path counterpart of 'collectDanglingProductNames' on a
'SimpleDatabase' plus its just-computed links. A 'SimpleDatabase' has no process
lookup yet, so internal resolution is checked with 'hasInternalProducer' and
coverage against the supplied links rather than 'dbCrossDBLinks'.
-}
collectStagedDanglingProductNames :: SimpleDatabase -> [CrossDBLink] -> M.Map T.Text Int
collectStagedDanglingProductNames db links =
    tallyDangling
        (crossDBCoveredCounts links)
        [ ((actUUID, prodUUID, exchangeFlowId ex), tfName flow)
        | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
        , ex@TechnosphereExchange{} <- exchanges act
        , isSupplierDemand ex
        , not (hasInternalProducer db ex)
        , claimsAnActivityUUID (exchangeSupplierClaim ex)
        , Just flow <- [M.lookup (exchangeFlowId ex) (sdbTechFlows db)]
        ]

-- ---------------------------------------------------------------------------
-- Supplier-gap report
-- ---------------------------------------------------------------------------

{- | Why one supplier demand is left unsupplied after internal resolution and
cross-DB linking.
-}
data GapReason
    = -- | Nil-link input the attribute matcher could not place, with its blocker.
      GapBlocked !LinkBlocker
    | {- | Non-nil source identity no dependency ships, and no attribute match
      rescued it — a partial import referencing activities it doesn't carry.
      -}
      GapDanglingIdentity
    | {- | Waste input (treatment side): never a cross-DB demand, so an
      internally unlinked one is a genuine gap.
      -}
      GapWasteInput
    deriving (Show, Eq)

-- | One consumer edge left unsupplied — the unit of the supplier-gap report.
data GapEdge = GapEdge
    { gapFlowName :: !T.Text
    , gapLocation :: !T.Text
    -- ^ Effective requested location ("" when the demand names none)
    , gapUnit :: !T.Text
    , gapAmount :: !Double
    , gapConsumerAct :: !UUID.UUID
    , gapConsumerProd :: !UUID.UUID
    , gapReason :: !GapReason
    }
    deriving (Show, Eq)

-- | One consuming process of a gap entry, with how many of its edges hit it.
data GapConsumer = GapConsumer
    { gcActUUID :: !UUID.UUID
    , gcProdUUID :: !UUID.UUID
    , gcActivityName :: !T.Text
    , gcProductName :: !T.Text
    , gcLocation :: !T.Text
    , gcEdges :: !Int
    }
    deriving (Show, Eq)

{- | Aggregate over one (flow name, location, unit) key of the gap report.
Unit is part of the key so 'geDemandSum' never mixes units.
-}
data GapEntry = GapEntry
    { geFlowName :: !T.Text
    , geLocation :: !T.Text
    , geUnit :: !T.Text
    , geReason :: !GapReason
    , geEdges :: !Int
    , geConsumers :: !Int
    , geDemandSum :: !Double
    , geTopConsumers :: ![GapConsumer]
    }
    deriving (Show, Eq)

-- | Supplier-gap report: header arithmetic plus the aggregated gap entries.
data GapReport = GapReport
    { grDbName :: !T.Text
    , grTotalInputs :: !Int
    , grInternalLinks :: !Int
    , grCrossDBLinks :: !Int
    , grUnresolvedEdges :: !Int
    , grUnresolvedProducts :: !Int
    , grCompleteness :: !Double
    , grGaps :: ![GapEntry]
    }
    deriving (Show, Eq)

{- | Shared gap-edge scan: every supplier demand with no internal producer,
minus per-triple cross-DB coverage. The per-triple accounting mirrors
'tallyDangling' (count-based: a partially covered triple drops its covered
occurrences in scan order), so edge counts stay consistent with the dangling
scans.
-}
gapEdgesWith ::
    (Exchange -> Bool) ->
    SimpleDatabase ->
    [CrossDBLink] ->
    CrossDBLinkingStats ->
    [GapEdge]
gapEdgesWith hasProducer db links stats =
    concatMap surplus (M.toList byTriple)
  where
    covered = crossDBCoveredCounts links
    surplus (triple, es) = drop (M.findWithDefault 0 triple covered) es
    byTriple =
        M.fromListWith
            (flip (<>))
            [ ((actUUID, prodUUID, exchangeFlowId ex), [edge])
            | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
            , ex <- exchanges act
            , isSupplierDemand ex
            , not (hasProducer ex)
            , Just edge <- [mkGapEdge db stats actUUID prodUUID ex]
            ]

{- | Describe one unsupplied demand. Biosphere exchanges are never demands
('isSupplierDemand'), hence 'Nothing'. A missing flow entry doesn't hide the
edge: the flow UUID stands in for the name so the report stays countable.

'cdlUnresolvedProducts' records one blocker per flow /name/, while the report
keys entries by (name, location, unit) — two same-named entries at different
locations therefore share that blocker even when the underlying causes differ.
-}
mkGapEdge ::
    SimpleDatabase ->
    CrossDBLinkingStats ->
    UUID.UUID ->
    UUID.UUID ->
    Exchange ->
    Maybe GapEdge
mkGapEdge db stats actUUID prodUUID ex = case ex of
    TechnosphereExchange{} ->
        let name = flowNameOr tfName (sdbTechFlows db)
            reason
                | claimsAnActivityUUID (exchangeSupplierClaim ex) = GapDanglingIdentity
                | otherwise = GapBlocked (maybe NoNameMatch upBlocker (M.lookup name (cdlUnresolvedProducts stats)))
         in Just (edge name reason)
    WasteExchange{} -> Just (edge (flowNameOr wfName (sdbWasteFlows db)) GapWasteInput)
    BiosphereExchange{} -> Nothing
  where
    flowNameOr nameOf flows =
        maybe (UUID.toText (exchangeFlowId ex)) nameOf (M.lookup (exchangeFlowId ex) flows)
    edge name reason =
        GapEdge
            { gapFlowName = name
            , gapLocation =
                let loc = exchangeLocation ex
                 in if T.null loc then extractBracketedLocation name else loc
            , gapUnit = getUnitNameForExchange (sdbUnits db) ex
            , gapAmount = exchangeAmount ex
            , gapConsumerAct = actUUID
            , gapConsumerProd = prodUUID
            , gapReason = reason
            }

-- | Consumers shown per gap entry — the tail is countable via 'geConsumers'.
topConsumerCap :: Int
topConsumerCap = 20

{- | Group gap edges by (flow name, location, unit), sorted by edge count
descending. Within a group the reason with the richest diagnostic wins
('GapBlocked' over 'GapDanglingIdentity' over 'GapWasteInput'). Consumer
processes are named from the database, most-demanding first.
-}
gapEntries :: SimpleDatabase -> [GapEdge] -> [GapEntry]
gapEntries db edges =
    sortOn (Down . geEdges) (map entry (M.toList byKey))
  where
    byKey =
        M.fromListWith
            (flip (<>))
            [((gapFlowName e, gapLocation e, gapUnit e), [e]) | e <- edges]
    reasonRank r = case r of
        GapBlocked _ -> 0 :: Int
        GapDanglingIdentity -> 1
        GapWasteInput -> 2
    strongerReason a b = if reasonRank a <= reasonRank b then a else b
    entry ((name, loc, unit), es) =
        let consumers =
                M.fromListWith (+) [((gapConsumerAct e, gapConsumerProd e), 1 :: Int) | e <- es]
         in GapEntry
                { geFlowName = name
                , geLocation = loc
                , geUnit = unit
                , geReason = foldr (strongerReason . gapReason) GapWasteInput es
                , geEdges = length es
                , geConsumers = M.size consumers
                , geDemandSum = sum (map gapAmount es)
                , geTopConsumers =
                    [ consumerOf a p n
                    | ((a, p), n) <- take topConsumerCap (sortOn (Down . snd) (M.toList consumers))
                    ]
                }
    consumerOf a p n =
        GapConsumer
            { gcActUUID = a
            , gcProdUUID = p
            , gcActivityName = maybe (UUID.toText a) activityName (M.lookup (a, p) (sdbActivities db))
            , gcProductName = maybe (UUID.toText p) tfName (M.lookup p (sdbTechFlows db))
            , gcLocation = maybe "" activityLocation (M.lookup (a, p) (sdbActivities db))
            , gcEdges = n
            }

{- | Assemble the report. Header counts reuse the setup-page predicates
('countTotalTechInputs' / 'countUnlinkedExchanges'); 'grUnresolvedEdges' is the
edge-accurate count (per-triple coverage), so it can sit below the setup page's
coarse @unlinked - crossDBLinks@ difference when waste-output links exist.
-}
buildGapReport :: T.Text -> SimpleDatabase -> Int -> [GapEdge] -> GapReport
buildGapReport dbName db nLinks edges =
    let total = countTotalTechInputs db
        unlinked = countUnlinkedExchanges db
        entries = gapEntries db edges
     in GapReport
            { grDbName = dbName
            , grTotalInputs = total
            , grInternalLinks = max 0 (total - unlinked)
            , grCrossDBLinks = nLinks
            , grUnresolvedEdges = length edges
            , grUnresolvedProducts = length entries
            , grCompleteness =
                if total > 0
                    then 100 * fromIntegral (total - length edges) / fromIntegral total
                    else 100
            , grGaps = entries
            }

-- | Supplier-gap report of a loaded database ('findProducer' honours process links).
gapReportForLoaded :: T.Text -> Database -> GapReport
gapReportForLoaded dbName db =
    let sdb = toSimpleDatabase db
        edges =
            gapEdgesWith
                (isJust . findProducer (dbProcessIdLookup db))
                sdb
                (dbCrossDBLinks db)
                (dbLinkingStats db)
     in buildGapReport dbName sdb (length (dbCrossDBLinks db)) edges

{- | Staged-path counterpart of 'gapReportForLoaded', against the staged
database's just-computed links and stats.
-}
gapReportForStaged :: T.Text -> SimpleDatabase -> CrossDBLinkingStats -> GapReport
gapReportForStaged dbName sdb stats =
    buildGapReport dbName sdb (crossDBLinksCount stats) (gapEdgesWith (hasInternalProducer sdb) sdb (cdlLinks stats) stats)

{- | Per-run linking environment: the cross-DB context, the consumer database's
own activity-key set (for the internal-resolution gate), and its flow tables.
Bundled so the per-activity / per-exchange matchers keep short signatures.

@lsOwnKeys@ lets the per-exchange matcher tell a non-nil link that resolves
*internally* (the matrix builder handles it) from a dangling one that needs a
cross-DB supplier — so we never emit a redundant cross-DB link for an input
already satisfied in place.
-}
data LinkScan = LinkScan
    { lsCtx :: !LinkingContext
    , lsOwnKeys :: !(S.Set (UUID.UUID, UUID.UUID))
    , lsTechFlows :: !TechFlowDB
    , lsWasteFlows :: !WasteFlowDB
    , lsUnits :: !UnitDB
    }

{- | Find all cross-database links without modifying activities
Returns statistics including the CrossDBLinks for chained solving
-}
findAllCrossDBLinks ::
    LinkingContext ->
    TechFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    ActivityMap ->
    CrossDBLinkingStats
findAllCrossDBLinks ctx techFlowDb wasteFlowDb unitDb activities =
    let !scan = LinkScan ctx (M.keysSet activities) techFlowDb wasteFlowDb unitDb
        results = M.mapWithKey (findActivityCrossDBLinks scan) activities
     in mconcat (M.elems results)

-- | Find cross-database links for one activity's exchanges
findActivityCrossDBLinks ::
    LinkScan ->
    -- | Consumer activity key (actUUID, prodUUID)
    (UUID.UUID, UUID.UUID) ->
    Activity ->
    CrossDBLinkingStats
findActivityCrossDBLinks scan (consumerActUUID, consumerProdUUID) act =
    mconcat (map (findExchangeCrossDBLink scan consumerActUUID consumerProdUUID) (exchanges act))

{- | Find cross-database link for a single exchange.

Technosphere inputs that need a supplier (nil-link, or a non-nil
'activityLinkId' to an activity this database does not ship) resolve via a
cascade:

1. __Exact source identity__ — @(activityLinkId, flowId)@ matched verbatim in a
   dependency ('findSupplierByActivityProduct'). The same-release case; the
   dataset author's own disambiguation, no guessing. Nil-link inputs skip this
   tier (they carry no identity).
2. __Attribute matching__ — name / location / unit scoring
   ('findSupplierInIndexedDBs'), the matcher every other cross-link uses. It
   narrows on the supplier activity the source named, where it named one apart
   from the product, before falling back to the product name alone.
   When a *non-nil* input falls through to here its source activity was absent
   from every dependency, so the match is a likely cross-version stitch,
   recorded in 'cdlAttributeFallbacks' for the consumer to verify.

A link whose target resolves in the internal matrix would be double-counted by
a cross-DB link too, so 'resolvesInternally' gates it out — mirroring
'Database.MatrixBuild.findProducer': a populated process link, or a non-nil
@activityLinkId@ whose @(linkId, flowId)@ key is one of this database's own
('lsOwnKeys'). Waste outputs take the same gate, then a strict matcher chosen by
their link — 'findWasteTreatmentByActivity' when they name a treatment,
'findWasteTreatmentAcrossDatabases' when they name none — with no synonym and
no widening in either.
-}
findExchangeCrossDBLink ::
    LinkScan ->
    UUID.UUID ->
    UUID.UUID ->
    Exchange ->
    CrossDBLinkingStats
findExchangeCrossDBLink LinkScan{lsCtx = ctx, lsOwnKeys = ownKeys, lsTechFlows = techFlowDb, lsUnits = unitDb} consumerActUUID consumerProdUUID ex@TechnosphereExchange{techFlowId = fid, techAmount = amt, techActivityLinkId = linkId, techSupplierClaim = claim, techLocation = loc}
    | namesASupplier ex && not resolvesInternally =
        maybe mempty resolveTechInput (M.lookup fid techFlowDb)
    | otherwise = mempty
  where
    resolvesInternally = maybe False (\supplier -> S.member (supplier, fid) ownKeys) linkId
    mkTechLink supAct supProd supName supLoc srcDb tied flowUnitName =
        CrossDBLink
            { cdlConsumerActUUID = consumerActUUID
            , cdlConsumerProdUUID = consumerProdUUID
            , cdlConsumerFlowId = fid
            , cdlSupplierActUUID = supAct
            , cdlSupplierProdUUID = supProd
            , cdlCoefficient = amt
            , cdlExchangeUnit = flowUnitName
            , cdlFlowName = supName
            , cdlLocation = supLoc
            , cdlSourceDatabase = srcDb
            , cdlTiedAlternatives = tied
            }
    resolveTechInput flow =
        let flowUnitName = maybe "" unitName (M.lookup (tfUnitId flow) unitDb)
            identityMatches = case claim of
                ClaimById actUUID -> findSupplierByActivityProduct (lcIndexedDatabases ctx) actUUID fid
                ClaimByProduct -> []
                ClaimByName _ -> []
                ClaimByDatasetNumber _ -> []
         in case identityMatches of
                ((entry, srcDb) : rest) ->
                    let !crossLink =
                            mkTechLink
                                (seActivityUUID entry)
                                (seProductUUID entry)
                                (seProductName entry)
                                (seLocation entry)
                                srcDb
                                (sort (map snd rest))
                                flowUnitName
                     in mempty{cdlLinks = [crossLink]}
                [] -> attributeMatch flow flowUnitName
    supplierQuery :: TechnosphereFlow -> T.Text -> SupplierQuery
    supplierQuery flow flowUnitName =
        SupplierQuery
            { sqProductName = tfName flow
            , sqSupplierActivity = claimedName claim
            , sqLocation = loc
            , sqUnit = flowUnitName
            }
    attributeMatch flow flowUnitName =
        case findSupplierInIndexedDBs ctx (supplierQuery flow flowUnitName) of
            result@CrossDBLinked{} ->
                let !crossLink =
                        mkTechLink
                            (cdlrActivityUUID result)
                            (cdlrProductUUID result)
                            (cdlrProductName result)
                            (cdlrLocation result)
                            (cdlrDatabaseName result)
                            (cdlrTiedDatabases result)
                            flowUnitName
                    locFallbacks =
                        [ LocationFallback
                            { lfProduct = cdlrProductName result
                            , lfRequested = req
                            , lfActual = actLoc
                            , lfKind = kind
                            }
                        | UpperLocationUsed req actLoc kind <- cdlrWarnings result
                        ]
                    -- Non-nil input matched only by attributes: its named source
                    -- activity was in no dependency — flag the cross-version risk.
                    attrFallbacks =
                        [ AttributeFallback
                            { afProduct = tfName flow
                            , afRequested = loc
                            , afMatched = cdlrLocation result
                            , afSourceDatabase = cdlrDatabaseName result
                            }
                        | claimsAnActivityUUID claim
                        ]
                    -- Several activities of the supplier database answered this
                    -- input equally well: the winner is the ranking's, not the
                    -- data's.
                    ambiguities =
                        [ SupplierAmbiguity
                            { saProduct = tfName flow
                            , saRequested = loc
                            , saChosen = chosen
                            , saCandidates = candidates
                            , saSourceDatabase = cdlrDatabaseName result
                            }
                        | AmbiguousSupplier chosen candidates <- cdlrWarnings result
                        ]
                 in mempty
                        { cdlLinks = [crossLink]
                        , cdlLocationFallbacks = locFallbacks
                        , cdlAttributeFallbacks = attrFallbacks
                        , cdlSupplierAmbiguities = ambiguities
                        }
            CrossDBNotLinked blocker
                -- An input designating by its product row reports a rich blocker;
                -- one that named an identity and matched nothing (no identity, no
                -- attribute match) is left for the dangling scan.
                | claimsAnActivityUUID claim -> mempty
                | otherwise -> unresolvedStats flow blocker
    unresolvedStats flow blocker =
        let unresolved = case blocker of
                LocationRejectedByPolicy{lrRequested = req, lrBestCandidate = actLoc, lrBestKind = kind} ->
                    [ LocationUnresolved
                        { luProduct = tfName flow
                        , luRequested = req
                        , luReason = "policy rejected " <> locationKindCode kind <> " candidate " <> actLoc
                        }
                    ]
                LocationUnavailable req ->
                    [ LocationUnresolved
                        { luProduct = tfName flow
                        , luRequested = req
                        , luReason = "no candidate above link threshold"
                        }
                    ]
                NoNameMatch -> []
                UnitIncompatible{} -> []
                AliasTargetMissing{} -> []
         in mempty
                { cdlUnresolvedProducts = M.singleton (tfName flow) UnresolvedProduct{upDemands = 1, upBlocker = blocker}
                , cdlLocationUnresolved = unresolved
                }
findExchangeCrossDBLink _ _ _ BiosphereExchange{} = mempty
-- Cross-DB linking for waste OUTPUTS the internal matrix does not route:
-- strict match only. No synonym, no fuzzy name match, no location widening.
-- Multi-DB matches stay orphan as 'cdlWasteAmbiguous'. Which matcher applies
-- follows the link: an output that names its treatment is matched on that
-- identity, one that names none on the flow itself. Neither falls back on the
-- other — substituting a treatment found by name for the one the author named
-- would link the waste to an activity nobody asked for.
-- Waste inputs (treatment side) are left alone: they have no clean LCA
-- semantic as a cross-DB demand.
findExchangeCrossDBLink LinkScan{lsCtx = ctx, lsOwnKeys = ownKeys, lsWasteFlows = wasteFlowDb} consumerActUUID consumerProdUUID WasteExchange{waFlowId = fid, waAmount = amt, waActivityLinkId = lid, waSupplierClaim = claim, waIsInput = isInp}
    | not isInp && not resolvesInternally =
        case treatmentMatch of
            WasteMatched entry dbN ->
                let
                    -- The dep-demand solve drives the matched treatment in
                    -- its OWN reference convention: an EcoSpold2 treatment
                    -- has a negative-output reference ('seRefSign' = -1),
                    -- an ILCD one a positive 'ReferenceInput' (+1). The
                    -- consumer's waste-output amount is positive, so we
                    -- carry the treatment's sign into the coefficient —
                    -- without it a negative-reference background treatment
                    -- scores the treated waste's burden with a flipped sign.
                    !crossLink =
                        CrossDBLink
                            { cdlConsumerActUUID = consumerActUUID
                            , cdlConsumerProdUUID = consumerProdUUID
                            , cdlConsumerFlowId = fid
                            , cdlSupplierActUUID = seActivityUUID entry
                            , cdlSupplierProdUUID = seProductUUID entry
                            , cdlCoefficient = amt * seRefSign entry
                            , cdlExchangeUnit = seUnit entry
                            , cdlFlowName = seProductName entry
                            , cdlLocation = seLocation entry
                            , cdlSourceDatabase = dbN
                            , cdlTiedAlternatives = []
                            }
                 in
                    mempty{cdlLinks = [crossLink], cdlWasteExactLinks = 1}
            WasteAmbiguous _ -> mempty{cdlWasteAmbiguous = 1}
            WasteNoMatch -> mempty{cdlCutoffWasteCount = 1}
    | otherwise = mempty
  where
    -- Same gate as the technosphere arm: a link the matrix already routes in
    -- place would be counted twice if a cross-DB link were emitted for it too.
    resolvesInternally = maybe False (\supplier -> S.member (supplier, fid) ownKeys) lid
    treatmentMatch
        | ClaimById treatment <- claim = findWasteTreatmentByActivity ctx treatment fid
        | otherwise = findWasteTreatmentAcrossDatabases ctx fid flowName
    flowName = maybe "" wfName (M.lookup fid wasteFlowDb)

-- | Report cross-database linking statistics
reportCrossDBLinkingStats :: Int -> CrossDBLinkingStats -> IO ()
reportCrossDBLinkingStats nActivities stats = do
    let !nInputs = cdlTotalInputs stats
        !nCrossDB = crossDBLinksCount stats
        !nUnresolved = unresolvedCount stats
        !nInternal = max 0 (nInputs - nCrossDB - nUnresolved)
        !nResolved = nInternal + nCrossDB

    -- Summary line (skip "0/0" for databases without technosphere input tracking)
    if nInputs > 0
        then do
            let !completeness = 100.0 * fromIntegral nResolved / fromIntegral nInputs :: Double
            reportProgress Info $
                printf
                    "Supply chain: %.1f%% complete (%d/%d inputs resolved), %d activities"
                    completeness
                    nResolved
                    nInputs
                    nActivities
            reportProgress Info $
                printf "  Internal: %d, Cross-DB: %d, Unresolved: %d" nInternal nCrossDB nUnresolved
        else
            reportProgress Info $
                printf "Supply chain: %d activities (no technosphere inputs)" nActivities

    -- Per-database breakdown
    forM_ (M.toList (crossDBBySource stats)) $ \(srcDb, count) ->
        reportProgress Info $
            printf "  - %s: %d links" (T.unpack srcDb) count

    -- Waste exchange resolution (only printed when this DB has any waste activity)
    let !wExact = cdlWasteExactLinks stats
        !wAmbig = cdlWasteAmbiguous stats
        !wCutoff = cdlCutoffWasteCount stats
    when (wExact + wAmbig + wCutoff > 0) $
        reportProgress Info $
            printf
                "Waste: %d linked (exact), %d ambiguous, %d cut-off (treatment not modelled)"
                wExact
                wAmbig
                wCutoff

    -- Missing suppliers
    let !missing = sortOn (Down . upDemands . snd) $ M.toList (cdlUnresolvedProducts stats)
    unless (null missing) $ do
        reportProgress Warning $
            printf "Missing suppliers: %d products unresolved" (length missing)
        forM_ (take 20 missing) $ \(name, unresolved) ->
            reportProgress Warning $
                printf
                    "  - %s (%d activities) — %s"
                    (T.unpack name)
                    (upDemands unresolved)
                    (showBlocker (upBlocker unresolved))
        when (length missing > 20) $
            reportProgress Warning $
                printf "  ... and %d more" (length missing - 20)

    -- Unknown units
    let !unknowns = S.toList (cdlUnknownUnits stats)
    unless (null unknowns) $
        reportProgress Warning $
            printf "Unknown units: %s" (T.unpack $ T.intercalate ", " unknowns)

    -- Location fallbacks (deduplicated)
    let !uniqueFallbacks = deduplicateFallbacks (cdlLocationFallbacks stats)
        !nFallbacks = length uniqueFallbacks
    when (nFallbacks > 0) $ do
        reportProgress Info $
            printf "Location fallbacks: %d unique products matched with different location" nFallbacks
        forM_ uniqueFallbacks $ \LocationFallback{lfProduct, lfRequested, lfActual, lfKind} ->
            reportProgress Info $
                printf
                    "  - %s: %s → %s (%s)"
                    (T.unpack lfProduct)
                    (T.unpack lfRequested)
                    (T.unpack lfActual)
                    (T.unpack (locationKindCode lfKind))

    -- Inputs rejected by geography_policy (deduplicated)
    let !uniqueUnresolved = deduplicateUnresolved (cdlLocationUnresolved stats)
        !nUnresolved' = length uniqueUnresolved
    when (nUnresolved' > 0) $ do
        reportProgress Warning $
            printf "Location unresolved: %d unique products with no acceptable supplier" nUnresolved'
        forM_ uniqueUnresolved $ \LocationUnresolved{luProduct, luRequested, luReason} ->
            reportProgress Warning $
                printf
                    "  - %s [%s] — %s"
                    (T.unpack luProduct)
                    (T.unpack luRequested)
                    (T.unpack luReason)

    -- Attribute fallbacks: source-identity inputs matched by attributes because
    -- no dependency shipped the exact activity — a likely cross-version stitch.
    let !uniqueAttrFallbacks = deduplicateAttributeFallbacks (cdlAttributeFallbacks stats)
        !nAttrFallbacks = length uniqueAttrFallbacks
    when (nAttrFallbacks > 0) $ do
        reportProgress Warning $
            printf
                "%d background link(s) matched by attributes, not source identity — verify the dependency is the same source release"
                nAttrFallbacks
        forM_ uniqueAttrFallbacks $ \AttributeFallback{afProduct, afRequested, afMatched, afSourceDatabase} ->
            reportProgress Warning $
                printf
                    "  - %s [%s] → %s in %s"
                    (T.unpack afProduct)
                    (T.unpack afRequested)
                    (T.unpack afMatched)
                    (T.unpack afSourceDatabase)

    -- Ambiguous suppliers: several activities of one dependency tied for the
    -- same input, so the winner is the ranking's and not the data's.
    let !uniqueAmbiguities = deduplicateSupplierAmbiguities (cdlSupplierAmbiguities stats)
        !nAmbiguities = length uniqueAmbiguities
    when (nAmbiguities > 0) $ do
        reportProgress Warning $
            printf
                "%d input(s) several activities of one dependency answer equally well - name the supplier meant, by its activity name or a relink mapping"
                nAmbiguities
        forM_ uniqueAmbiguities $ \SupplierAmbiguity{saProduct, saRequested, saChosen, saCandidates, saSourceDatabase} ->
            reportProgress Warning $
                printf
                    "  - %s [%s] - %d candidates in %s, linked to %s"
                    (T.unpack saProduct)
                    (T.unpack saRequested)
                    saCandidates
                    (T.unpack saSourceDatabase)
                    (T.unpack saChosen)

showBlocker :: LinkBlocker -> String
showBlocker NoNameMatch = "Not found"
showBlocker UnitIncompatible{uiQueryUnit = q, uiSupplierUnit = s} = printf "Unit: %s vs %s" (T.unpack q) (T.unpack s)
showBlocker (LocationUnavailable loc) = printf "Location: %s" (T.unpack loc)
showBlocker LocationRejectedByPolicy{lrRequested = req, lrBestCandidate = act, lrBestKind = kind} =
    printf "Rejected by policy: %s → %s (%s)" (T.unpack req) (T.unpack act) (T.unpack (locationKindCode kind))
showBlocker (AliasTargetMissing name mLoc) =
    printf "Mapping target not found: %s%s" (T.unpack name) (maybe "" ((" @ " <>) . T.unpack) mLoc)

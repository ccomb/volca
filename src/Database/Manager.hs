{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Manager (
    -- * Types
    DatabaseManager (..),
    LoadedDatabase (..),
    DatabaseStatus (..),
    DatabaseLoadStatus (..),
    StagedDatabase (..),
    DatabaseSetupInfo (..),
    SetupError (..),
    setupErrorMessage,
    MissingSupplier (..),
    DependencyChoice (..),
    DependencyStatus (..),
    MethodCollectionStatus (..),
    RefDataStatus (..),
    DirectoryFormat (..),

    -- * Format detection
    detectDirectoryFormat,

    -- * Re-exports
    DepLoadResult (..),

    -- * Initialization
    initDatabaseManager,
    readRefDataSource,
    withReservedName,

    -- * Operations
    getDatabase,
    mkDepSolverLookup,
    listDatabases,
    clearMethodMappingCacheForDb,

    -- * Load/Unload
    loadDatabase,
    unloadDatabase,
    relinkDatabase,
    relinkDatabaseWithMapping,
    RelinkResult (..),
    addDatabase,
    removeDatabase,
    editHome,

    -- * Method Operations
    listMethodCollections,
    loadMethodCollection,
    loadMethodCollectionFromConfig,
    unloadMethodCollection,
    getLoadedMethods,
    getMethodCollection,
    addMethodCollection,
    removeMethodCollection,

    -- * Geography
    parseGeographies,
    parseGeographiesCSV,

    -- * Reference Data Operations
    autoCreateFlowSynonyms,
    SynonymOrigin (..),
    listFlowSynonyms,
    loadFlowSynonyms,
    unloadFlowSynonyms,
    addFlowSynonyms,
    removeFlowSynonyms,
    listCompartmentMappings,
    loadCompartmentMappings,
    unloadCompartmentMappings,
    addCompartmentMappings,
    removeCompartmentMappings,
    listUnitDefs,
    loadUnitDefs,
    unloadUnitDefs,
    addUnitDefs,
    removeUnitDefs,
    getFlowSynonymGroups,
    getMergedSynonymDB,
    getMergedCompartmentMap,
    getMergedEnergyDensities,
    getMergedUnitConfig,
    getMergedFlowMetadata,
    hierarchyFromGeographies,
    managerGeographies,

    -- * Staged Database Operations
    getStagedDatabase,
    getDatabaseSetupInfo,
    buildLoadedSetupInfo,
    databaseGapReport,
    databaseQualityReport,
    databaseCoverageReport,
    explainFlowFactor,
    addDependencyToStaged,
    DependencyEdit (..),
    removeDependencyFromStaged,
    setDataPath,
    RelativeDataPath (..),
    finalizeDatabase,

    -- * Cached flow mapping
    CollectionName (..),
    mapMethodToFlowsCached,
    effectiveMethodMappings,
    mapMethodToTablesCached,
    mapMethodSetToTablesCached,
    mapMethodToIndexCached,

    -- * Internal (for tests: lowest-level loader, says where the load came from)
    loadDatabaseRawWithCrossDB,
    RawLoad (..),
    LoadSource (..),
    CachePolicy (..),

    -- * Internal (for tests: pure dependency-list builder)
    buildDependencyChoices,

    -- * Internal (for tests: the names two entries both claim)
    shadowedNames,
    shadowedMethods,

    -- * Installing a database the caller built itself
    solverFor,
    publishLoaded,
) where

import API.JsonOptions (Stripped (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Control.Exception
import Control.Lens ((&), (?~))
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.Char (toLower)
import qualified Data.Csv as Csv
import Data.Either (fromRight, lefts, partitionEithers, rights)
import Data.Indexing (uniqueIndex)
import qualified Data.Indexing as Indexing
import Data.List (intercalate, isPrefixOf, sort, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToSchema (..), enum_, type_)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.Mem (performGC)

import Builtin (builtinContent, builtinGeographies)
import Config
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (diffUTCTime, getCurrentTime)
import Database (Geographies, buildDatabaseWithMatrices, readGeographies)
import qualified Database.Loader as Loader
import qualified Database.Quality as Quality
import Matrix (clearCachedSolver)
import Method.ChemSynonyms (ChemSynonyms, emptyChemSynonyms, loadChemSynonyms)
import qualified Method.Coverage as Coverage
import qualified Method.Explain as Explain
import Method.Mapping (
    CF (..),
    CFUnit (..),
    MatchStrategy,
    MethodIndex,
    MethodSetTables,
    MethodTables,
    ProxyTargets (..),
    RefusalReason,
    RegionalActivityWeights (..),
    SeaWaterCFs (..),
    buildMethodIndex,
    buildMethodSetTables,
    buildMethodTables,
    characterizedFlowIds,
    directionExcludedCFs,
    dropExcludedMappings,
    expandProxyEdges,
    expandSynonymMappings,
    fillBroadcastVector,
    fillRegionalActivityWeights,
    isExclusionCF,
    mapContextFor,
    mapMethodFlows,
    mtExactCF,
    mtFallbackCF,
    mtRegionalActivityWeights,
    mtRegionalizedCF,
    mtSeaWaterCFs,
    projectRegionalResourceFlows,
    zeroedMatchedCFs,
 )
import Method.Types (
    CompartmentMap,
    EnergyDensityMap,
    Location (..),
    Method (..),
    MethodCF (..),
    MethodCollection (..),
    ScoringSet (..),
    buildCompartmentMapFromCSV,
    buildEnergyDensityMapFromCSV,
    cfFamily,
    compartmentMapSize,
    energyDensityMapSize,
 )
import Progress (ProgressLevel (..), reportError, reportProgress, reportProgressWithTiming, withLogScope)
import qualified Search.BM25 as BM25
import SharedSolver (SharedSolver, createSharedSolver)
import qualified SharedSolver
import SubstanceRegistry (CASNumber (..), KeyNormalizers (..), NormName (..), SubstanceEdge, casBindingsFromEdges, normalizeCAS, parseSubstanceEdges)
import SynonymDB (BridgeDirection (..), SynEdge (..), SynonymDB (..), buildFromCSV, emptySynonymDB, excludeJunkSynonyms, excludeOverFrequentSynonyms, loadFromCSVFileWithCache, mergeSynonymDBs, normalizeName, oversizedClasses, reopenedBridges, synonymCount, uncoveredUnitSuffixes)
import Types (
    ActivityMap,
    AllocationKey (..),
    AttributeFallback (..),
    BioFlowDB,
    BiosphereFlow (..),
    BlockerReason (..),
    BuildInputs (..),
    CrossDBLink (..),
    CrossDBLinkingStats (..),
    Database (..),
    FlowClosure (..),
    GeographyPolicy (..),
    LinkBlocker (..),
    LocationFallback (..),
    LocationUnresolved (..),
    Medium (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierAmbiguity (..),
    UUID,
    Unit (..),
    UnitDB,
    UnresolvedProduct (..),
    allocationKeyText,
    bfCompartmentName,
    bfCompartmentSub,
    blockerReason,
    computeMinimalSelectedDeps,
    crossDBBySource,
    crossDBRedundantSources,
    deduplicateAttributeFallbacks,
    deduplicateFallbacks,
    deduplicateSupplierAmbiguities,
    deduplicateUnresolved,
    enrichBioFlowCAS,
    flowClosure,
    initializeRuntimeFields,
    parseAllocationKey,
    toSimpleDatabase,
    unresolvedCount,
    upDemands,
 )
import qualified UnitConversion

-- CrossDBLinkingStats is now in Types, re-exported from Database.Loader

import API.Types (DepLoadResult (..))
import Database.Author (AuthorContext (..))
import Database.CrossLinking (IndexedDatabase (..), LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold)
import qualified Database.CrossLinking as CrossLinking
import qualified Database.Journal as Journal
import Database.Upload (detectMethodFormat, detectedFormatLabel, findMethodDirectory, listDirectoryRecursive)
import qualified Database.Upload as Upload
import qualified Database.UploadedDatabase as UploadedDB
import Method.FlowResolver (ILCDFlowInfo)
import qualified Method.FlowResolver as FlowResolver
import qualified Method.Parser
import qualified Method.Parser.OlcaSchema as OlcaSchema
import Method.ParserCSV (parseMethodCSVBytes, stripBOM)
import Method.ParserSimaPro (isSimaProMethodCSV, parseSimaProMethodCSVBytes)
import qualified Method.Patch
import SynonymDB.Extract (extractFromEcoSpold2, extractFromILCDFlows, synonymPairsToCSV)

-- | A fully loaded database with solver ready for queries
data LoadedDatabase = LoadedDatabase
    { ldDatabase :: !Database
    , ldSharedSolver :: !SharedSolver
    , ldConfig :: !DatabaseConfig
    }

{- | A staged database awaiting dependency configuration
This is the intermediate state before building matrices
-}
data StagedDatabase = StagedDatabase
    { sdSimpleDB :: !SimpleDatabase
    -- ^ Parsed data (activities, flows, units)
    , sdConfig :: !DatabaseConfig
    -- ^ Configuration
    , sdMissingProducts :: ![(Text, UnresolvedProduct)]
    -- ^ Product name, and the demands it left unsupplied
    , sdSelectedDeps :: ![Text]
    -- ^ Selected dependency database names
    , sdCrossDBLinks :: ![CrossDBLink]
    -- ^ Cross-DB links found so far
    , sdLinkingStats :: !CrossDBLinkingStats
    -- ^ Linking statistics
    , sdBuiltWith :: !BuildInputs
    {- ^ What the parse ran under. The finalized database is stamped with it,
    not with whatever is in force at finalize time: the amounts and the
    unknown units were read under this table, so a table that changed in
    between makes the next start read the source again.
    -}
    , sdCachedDB :: !(Maybe Database)
    -- ^ Pre-built DB from cache (skip rebuild)
    }

-- | Information about a missing supplier product
data MissingSupplier = MissingSupplier
    { msProductName :: !Text
    , msCount :: !Int
    -- ^ Number of activities needing this supplier
    , msLocation :: !(Maybe Text)
    -- ^ Most common location requested
    , msReason :: !Text
    -- ^ "unit_incompatible", "location_unavailable", "no_name_match"
    , msDetail :: !(Maybe Text)
    -- ^ e.g. "kg vs ton", "FR not available"
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped MissingSupplier)

{- | Whether a candidate dependency is currently selected, merely available,
or redundant under the minimal cover (matches links but every link it wins
can be re-supplied by another selected DB at the same score).
-}
data DependencyStatus = SelectedDep | AvailableDep | RedundantDep
    deriving (Show, Eq, Generic)

instance ToJSON DependencyStatus where
    toJSON SelectedDep = A.String "selected"
    toJSON AvailableDep = A.String "available"
    toJSON RedundantDep = A.String "redundant"

{- | String-enum schema matching the lowercase wire codes from ToJSON above.
The previous default-Generic schema advertised the raw Haskell constructor
names (SelectedDep / AvailableDep / RedundantDep), which is what the schema
said but never what the wire emitted.
-}
instance ToSchema DependencyStatus where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "DependencyStatus") $
                mempty
                    & type_ ?~ OpenApiString
                    & enum_ ?~ [toJSON (c :: Text) | c <- ["selected", "available", "redundant"]]

-- | A candidate dependency database in one of three states
data DependencyChoice = DependencyChoice
    { dchStatus :: !DependencyStatus
    , dchDatabaseName :: !Text
    , dchDisplayName :: !Text
    , dchMatchCount :: !Int
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped DependencyChoice)

{- | One of the candidate data directories inside an uploaded database's
upload root. Surfaces in @DatabaseSetupInfo.dsiAvailablePaths@ so the UI
can present a picker. The schema is now a proper named object instead
of a positional 3-tuple.
-}
data PathCandidate = PathCandidate
    { pcPath :: !Text
    -- ^ Relative path under the upload root
    , pcFormat :: !Text
    -- ^ Format label (e.g. "EcoSpold 2", "SimaPro CSV", "Unknown")
    , pcFileCount :: !Int
    -- ^ Number of data files detected in this directory
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped PathCandidate)

-- | Setup info for a database (for the setup page)
data DatabaseSetupInfo = DatabaseSetupInfo
    { dsiName :: !Text
    , dsiDisplayName :: !Text
    , dsiActivityCount :: !Int
    , dsiInputCount :: !Int
    -- ^ Total technosphere inputs
    , dsiCompleteness :: !Double
    -- ^ Percentage of resolved links (0-100)
    , dsiInternalLinks :: !Int
    -- ^ Links resolved within this database
    , dsiCrossDBLinks :: !Int
    -- ^ Links resolved via dependencies
    , dsiUnresolvedLinks :: !Int
    -- ^ Still unresolved
    , dsiMissingSuppliers :: ![MissingSupplier]
    -- ^ Top missing suppliers
    , dsiDependencies :: ![DependencyChoice]
    {- ^ Candidate dependencies in one alpha-sorted list, each tagged as
    selected, available, or redundant under the minimal cover.
    -}
    , dsiIsReady :: !Bool
    -- ^ True if can be finalized
    , dsiUnknownUnits :: ![Text]
    -- ^ Unknown units from sdbUnits
    , dsiLocationFallbacks :: ![LocationFallback]
    -- ^ Accepted links with widened geography, tagged with 'LocationKind'
    , dsiLocationUnresolved :: ![LocationUnresolved]
    {- ^ Inputs that could not be linked because the database's
    'GeographyPolicy' rejected every candidate (or no candidate existed)
    -}
    , dsiAttributeFallbacks :: ![AttributeFallback]
    {- ^ Source-identity inputs (non-nil 'activityLinkId') matched by attributes
    because no loaded dependency shipped the exact activity, a likely
    cross-version stitch the consumer should verify against the source release.
    -}
    , dsiSupplierAmbiguities :: ![SupplierAmbiguity]
    {- ^ Inputs several activities of one dependency answered equally well, so
    the supplier linked is the ranking's choice and not the data's. Named here
    as well as in the load report, because the setup page is where the other
    link diagnostics are read.
    -}
    , dsiDataPath :: !Text
    -- ^ Current selected data path (relative)
    , dsiAvailablePaths :: ![PathCandidate]
    -- ^ Candidate data directories within the upload root
    , dsiIsLoaded :: !Bool
    -- ^ True if database is already loaded (read-only info)
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON) via (Stripped DatabaseSetupInfo)

-- | Errors from getDatabaseSetupInfo
data SetupError
    = SetupNotFound Text
    | -- | Configured (non-uploaded) database that must be loaded before setup.
      SetupNotLoaded Text
    | SetupFailed Text
    deriving (Show, Eq)

setupErrorMessage :: SetupError -> Text
setupErrorMessage (SetupNotFound msg) = msg
setupErrorMessage (SetupNotLoaded name) = "Database not loaded: " <> name
setupErrorMessage (SetupFailed msg) = msg

-- | Load status: derivable from TVar membership + linking stats
data DatabaseLoadStatus = Unloaded | PartiallyLinked | Loaded
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseLoadStatus where
    toJSON Unloaded = A.String "unloaded"
    toJSON PartiallyLinked = A.String "partially_linked"
    toJSON Loaded = A.String "loaded"

instance FromJSON DatabaseLoadStatus where
    parseJSON = A.withText "DatabaseLoadStatus" $ \case
        "unloaded" -> pure Unloaded
        "partially_linked" -> pure PartiallyLinked
        "loaded" -> pure Loaded
        other -> fail $ "Unknown DatabaseLoadStatus: " <> T.unpack other

-- | Status of a database for API responses
data DatabaseStatus = DatabaseStatus
    { dsName :: !Text -- Internal identifier (slug)
    , dsDisplayName :: !Text -- Human-readable name for UI
    , dsDescription :: !(Maybe Text)
    , dsLoadAtStartup :: !Bool -- Configured to load at startup
    , dsStatus :: !DatabaseLoadStatus -- Derived from TVar membership + linking stats
    , dsIsUploaded :: !Bool -- True if path starts with "uploads/"
    , dsPath :: !Text -- Data path
    , dsFormat :: !(Maybe Upload.DatabaseFormat) -- Detected format
    , dsActivityCount :: !Int -- Number of activities (0 if unloaded)
    , dsDependsOn :: ![Text] -- Names of databases this one depends on (for cross-DB linking)
    , dsAllocation :: !AllocationKey -- The key its multi-output blocks were divided under
    , dsSource :: !(Maybe Text) -- The database whose files it reads, when it owns none
    }
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseStatus where
    toJSON DatabaseStatus{..} =
        A.object
            [ "dsName" .= dsName
            , "dsDisplayName" .= dsDisplayName
            , "dsDescription" .= dsDescription
            , "dsLoadAtStartup" .= dsLoadAtStartup
            , "dsStatus" .= dsStatus
            , "dsIsUploaded" .= dsIsUploaded
            , "dsPath" .= dsPath
            , "dsFormat" .= dsFormat
            , "dsActivityCount" .= dsActivityCount
            , "dsDependsOn" .= dsDependsOn
            , "dsAllocation" .= allocationKeyText dsAllocation
            , "dsSource" .= dsSource
            ]

instance FromJSON DatabaseStatus where
    parseJSON = A.withObject "DatabaseStatus" $ \v ->
        DatabaseStatus
            <$> v .: "dsName"
            <*> v .: "dsDisplayName"
            <*> v .:? "dsDescription"
            <*> v .: "dsLoadAtStartup"
            <*> v .: "dsStatus"
            <*> v .: "dsIsUploaded"
            <*> v .: "dsPath"
            <*> v .:? "dsFormat"
            <*> v .: "dsActivityCount"
            <*> v .:? "dsDependsOn" A..!= []
            -- A status written before the key was on the wire describes a
            -- database divided the way its source declares, which is what it
            -- was. A word this client cannot read is not that: showing
            -- "declared" beside shares some other key produced is the silent
            -- misreading the field was put on the wire to end.
            <*> (v .:? "dsAllocation" A..!= "declared" >>= either (fail . T.unpack) pure . parseAllocationKey)
            <*> v .:? "dsSource"

-- | Status of a method collection (e.g., EF-3.1) for API responses
data MethodCollectionStatus = MethodCollectionStatus
    { mcsName :: !Text -- Internal identifier
    , mcsDisplayName :: !Text -- Human-readable name
    , mcsDescription :: !(Maybe Text) -- Optional description
    , mcsStatus :: !DatabaseLoadStatus -- Loaded/Unloaded (reuse existing type)
    , mcsIsUploaded :: !Bool -- True if uploaded (vs. configured in TOML)
    , mcsPath :: !Text -- Path to method directory
    , mcsMethodCount :: !Int -- Number of impact categories (0 if unloaded)
    , mcsFormat :: !Text -- "SimaPro CSV", "ILCD", etc.
    }
    deriving (Show, Eq, Generic)

instance ToJSON MethodCollectionStatus where
    toJSON MethodCollectionStatus{..} =
        A.object
            [ "mcsName" .= mcsName
            , "mcsDisplayName" .= mcsDisplayName
            , "mcsDescription" .= mcsDescription
            , "mcsStatus" .= mcsStatus
            , "mcsIsUploaded" .= mcsIsUploaded
            , "mcsPath" .= mcsPath
            , "mcsMethodCount" .= mcsMethodCount
            , "mcsFormat" .= mcsFormat
            ]

instance FromJSON MethodCollectionStatus where
    parseJSON = A.withObject "MethodCollectionStatus" $ \v ->
        MethodCollectionStatus
            <$> v .: "mcsName"
            <*> v .: "mcsDisplayName"
            <*> v .:? "mcsDescription"
            <*> v .: "mcsStatus"
            <*> v .: "mcsIsUploaded"
            <*> v .: "mcsPath"
            <*> v .: "mcsMethodCount"
            <*> v .: "mcsFormat"

{- | The database manager maintains state for multiple databases
Databases with load=true are pre-loaded at startup for instant switching
-}
data DatabaseManager = DatabaseManager
    { dmLoadedDbs :: !(TVar (Map Text LoadedDatabase)) -- All loaded databases
    , dmStagedDbs :: !(TVar (Map Text StagedDatabase)) -- Staged databases (parsed but not finalized)
    , dmStagingDbs :: !(TVar (S.Set Text)) -- Databases currently being staged
    , dmIndexedDbs :: !(TVar (Map Text IndexedDatabase)) -- Pre-built indexes for cross-DB linking
    , dmAvailableDbs :: !(TVar (Map Text DatabaseConfig)) -- All configured databases
    , dmAvailableMethods :: !(TVar (Map Text MethodConfig)) -- All configured method collections
    , dmLoadedMethods :: !(TVar (Map Text MethodCollection)) -- name → parsed methods + NW data
    -- Reference data: flow synonyms
    , dmAvailableFlowSyns :: !(TVar (Map Text RefDataConfig))
    , dmLoadedFlowSyns :: !(TVar (Map Text SynonymDB))
    , -- Reference data: compartment mappings
      dmAvailableCompMaps :: !(TVar (Map Text RefDataConfig))
    , dmLoadedCompMaps :: !(TVar (Map Text CompartmentMap))
    , -- Reference data: unit definitions
      dmAvailableUnitDefs :: !(TVar (Map Text RefDataConfig))
    , dmLoadedUnitDefs :: !(TVar (Map Text UnitConversion.UnitConfig))
    , -- Reference data: energy densities (mass/volume → energy for energy-denominated CFs)
      dmAvailableEnergyDensities :: !(TVar (Map Text RefDataConfig))
    , dmLoadedEnergyDensities :: !(TVar (Map Text EnergyDensityMap))
    , dmCachePolicy :: !CachePolicy
    , dmGeographies :: !(Map Text (Text, [Text])) -- code → (display_name, parent_codes)
    , dmLocationHierarchy :: !(Map Location [Location])
    {- ^ 'dmGeographies' in the shape the regionalized scoring path and the
    linking paths read it, derived once beside it. Sourced from the configured
    geographies file, empty when none is configured or it fails to parse.
    -}
    , dmMethodMappingCache :: !(TVar (Map (Text, CollectionName, UUID) [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]))
    {- ^ Cached flow mappings: (dbName, collection, methodId) → mappings.
    The collection is part of the key because a method UUID is a UUIDv5 of the
    method name alone, so the same name in two collections collides on UUID
    while carrying different CF lists. Invalidated on database/method/synonym
    reload.
    -}
    , dmMethodTablesCache :: !(TVar (Map (Text, CollectionName, UUID) MethodTables))
    {- ^ Cached LCIA-score lookup tables built from mappings.
    These depend only on (db, collection, method), so building them once per
    triple saves O(n log n) Map constructions on every LCIA call.
    -}
    , dmMethodTablesInflight :: !(TVar (Map (Text, CollectionName, UUID) (TMVar (Either SomeException MethodTables))))
    {- ^ Single-flight slots guarding 'dmMethodTablesCache' builds. The first
    caller for a key installs an empty 'TMVar' and runs the (expensive) build;
    concurrent callers, including the load-time warm-up, await that slot
    instead of each rebuilding the same tables. The slot is removed when the
    build finishes, so a failed build is retried rather than cached.
    -}
    , dmMethodSetTablesCache :: !(TVar (Map (Text, CollectionName, [UUID]) MethodSetTables))
    {- ^ Cached stacked CF tables for multi-method scoring.
    Key is (dbName, collection, sortedMethodIds) so subset-arbitrary requests
    share cache entries with named-collection ones whenever the method ids
    match within the same collection. Purged together with
    'dmMethodTablesCache' on any reload that invalidates the per-method cache
    (collection / synonym / DB load).
    -}
    , dmMethodIndexCache :: !(TVar (Map (Text, CollectionName, UUID) MethodIndex))
    {- ^ Cached inverted indices over a method (CF tokens, by-medium, by-CAS).
    Used by the post-scoring suggester to surface candidate matches for
    uncharacterized flows. Keyed identically to the tables cache and
    invalidated on the same conditions.
    -}
    , dmChemSynonyms :: !ChemSynonyms
    {- ^ Vendored PubChem snapshot loaded once at startup. Drives the
    suggester's synonym-expansion signal. Empty when no path is configured
    or when the file is missing: suggester degrades to plain Jaccard.
    -}
    , dmSubstanceEdges :: ![SubstanceEdge]
    {- ^ Typed flow-correspondence edges loaded once at startup from
    @substance_edges.csv@. Empty when no path is configured. @ProxyFor@ edges
    feed the CF cascade ('expandProxyEdges'); @SameAs@ name↔CAS edges feed
    'dmCasBindings'.
    -}
    , dmCasBindings :: !(M.Map NormName CASNumber)
    {- ^ Name→CAS identities distilled from the @SameAs@ edges, applied to
    every database at load ('enrichBioFlowCAS') to fill empty @bfCAS@ so the
    native CAS bridge reaches flows a source left CAS-less (e.g. SimaPro
    exports). Empty when no edges bind a name to a CAS.
    -}
    , dmMergedFlowMetadataCache :: !(TVar (Maybe (BioFlowDB, UnitDB)))
    {- ^ Memoized 'M.unions' of every loaded DB's flows/units.
    Invalidated on any 'dmLoadedDbs' mutation; collision detection
    runs once per rebuild rather than per hot-path call.
    -}
    , dmMergedUnitConfigCache :: !(TVar (Maybe UnitConversion.UnitConfig))
    {- ^ Memoized merge of every loaded unit-definition set.
    Invalidated on 'dmLoadedUnitDefs' mutation.
    -}
    , dmFlowClosureCache :: !(TVar (Map Text FlowClosure))
    {- ^ Memoized 'FlowClosure' per root database: the flows its
    characterization has to reach, its dependencies' included. Invalidated
    with that database's method caches, which are built from it.
    -}
    }

{- | The databases a root reaches, transitively, through 'dbDependsOn'. The
root itself is excluded, and a name already seen is not walked again, so a
dependency cycle terminates instead of looping.
-}
dependencyClosure :: Map Text LoadedDatabase -> Text -> [Database]
dependencyClosure loaded root = go (S.singleton root) (depsOf root)
  where
    depsOf :: Text -> [Text]
    depsOf name = maybe [] (dbDependsOn . ldDatabase) (M.lookup name loaded)

    go :: S.Set Text -> [Text] -> [Database]
    go _ [] = []
    go seen (name : rest)
        | S.member name seen = go seen rest
        | otherwise = case M.lookup name loaded of
            Nothing -> go (S.insert name seen) rest
            Just ld -> ldDatabase ld : go (S.insert name seen) (rest ++ depsOf name)

{- | The flows a database's characterization has to reach, memoized per root.

Scoring reads the merged inventory of the whole cross-database solve, so a
mapping cascade built on the root's own flows alone leaves every dependency
flow to the coarse rungs (no synonym bridge, no proxy edge, no regional
projection) and the score changes without anything reporting a gap.
-}
getFlowClosure :: DatabaseManager -> Text -> Database -> IO FlowClosure
getFlowClosure manager dbName db = atomically $ do
    cached <- readTVar (dmFlowClosureCache manager)
    case M.lookup dbName cached of
        Just closure -> pure closure
        Nothing -> do
            -- The union is built inside the transaction that publishes it, so a
            -- dependency edit committing mid-build invalidates this read of
            -- 'dmLoadedDbs' and the closure is rebuilt rather than cached
            -- against flows that no longer exist.
            loaded <- readTVar (dmLoadedDbs manager)
            let !closure = flowClosure db (dependencyClosure loaded dbName)
            modifyTVar' (dmFlowClosureCache manager) (M.insert dbName closure)
            pure closure

{- | The name of a method collection. A newtype because it travels next to a
database name, of the same type, through every cache lookup below: swapped,
the two would read and fill the wrong cache entry and nothing would say so.
-}
newtype CollectionName = CollectionName {unCollectionName :: Text}
    deriving (Eq, Ord, Show)

{- | Cached flow mapping: avoids re-matching method CFs to database flows on every LCIA call.
The mapping depends only on (database, method), not on the process being evaluated.
-}
mapMethodToFlowsCached :: DatabaseManager -> Text -> CollectionName -> Database -> Method -> IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mapMethodToFlowsCached manager dbName collection db method = do
    let key = (dbName, collection, methodId method)
    cache <- readTVarIO (dmMethodMappingCache manager)
    case M.lookup key cache of
        Just cached -> return cached
        Nothing -> do
            closure <- getFlowClosure manager dbName db
            cmap <- getMergedCompartmentMap manager
            let ctx = mapContextFor closure (fromMaybe emptySynonymDB (dbSynonymDB db)) cmap
            result <- mapMethodFlows ctx method
            atomically $ modifyTVar' (dmMethodMappingCache manager) (M.insert key result)
            return result

{- | The mappings scoring actually uses: the cached cascade result expanded
with the database's synonym fan-out and the configured substance edges.
Diagnostics (flow-mapping endpoints, coverage audits) must read THIS rather
than the raw cascade, or they under-report what the score tables contain.

Uses the database's frozen-at-load-time synonym DB, which holds the curated
registry plus any source the user had explicitly activated at load time
(auto-extracted candidates are persisted but never loaded by the engine).

The method's exclusions are re-applied last: the expansions travel by flow
name and would otherwise hand an excepted flow the factor of a sibling it
shares a synonym group with (see 'dropExcludedMappings').
-}
effectiveMethodMappings :: DatabaseManager -> Text -> CollectionName -> Database -> Method -> IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
effectiveMethodMappings manager dbName collection db method = do
    mappings <- mapMethodToFlowsCached manager dbName collection db method
    closure <- getFlowClosure manager dbName db
    let synDB = fromMaybe emptySynonymDB (dbSynonymDB db)
        proxyTargets = ProxyTargets (clByName closure) (clByCAS closure) (clByUUID closure)
    pure $
        dropExcludedMappings (filter isExclusionCF (methodFactors method)) $
            expandProxyEdges proxyTargets (dmSubstanceEdges manager) $
                projectRegionalResourceFlows synDB (clByUUID closure) $
                    expandSynonymMappings synDB (clByName closure) mappings

-- | Cached prepared CF tables: built once per (db, method), reused across inventories.
mapMethodToTablesCached :: DatabaseManager -> Text -> CollectionName -> Database -> Method -> IO MethodTables
mapMethodToTablesCached manager dbName collection db method = do
    let key = (dbName, collection, methodId method)
    cache <- readTVarIO (dmMethodTablesCache manager)
    case M.lookup key cache of
        Just tables -> pure tables
        Nothing ->
            -- Single-flight: the expensive build runs once per key even under a
            -- concurrent panel request + load-time warm-up racing on it.
            singleFlight
                (dmMethodTablesInflight manager)
                key
                (modifyTVar' (dmMethodTablesCache manager) . M.insert key)
                (buildMethodTablesFor manager dbName collection db method)

{- | Build the LCIA lookup tables for one method against a database: resolve the
CF→flow mappings, stack them into the broadcast/CAS/regional tables, and
precompute the regionalized per-activity weights. The deduplicated regional
coverage gaps are surfaced once here, at build time, rather than per-pid on the
scoring path. Caching and single-flighting are the caller's responsibility.
-}
buildMethodTablesFor ::
    DatabaseManager -> Text -> CollectionName -> Database -> Method -> IO MethodTables
buildMethodTablesFor manager dbName collection db method = do
    let hier = dmLocationHierarchy manager
    expanded <- effectiveMethodMappings manager dbName collection db method
    closure <- getFlowClosure manager dbName db
    cmap <- getMergedCompartmentMap manager
    let dirExcluded =
            directionExcludedCFs cmap (fromMaybe emptySynonymDB (dbSynonymDB db)) (clByName closure) expanded
    mapM_ (reportProgress Warning) (directionWarning dirExcluded)
    energyDensities <- getMergedEnergyDensities manager
    unitConfig <- getMergedUnitConfig manager
    (mFlows, mUnits) <- getMergedFlowMetadata manager
    -- A method listed in its collection's 'global-methods' is scored without
    -- regionalization: drop its located CFs so the broadcast (global) path, the
    -- method's own unlocated default CF, is the single answer, matching a
    -- reference distribution that flattened the spatial factors to a global value.
    -- This assumes the method carries such an unlocated default for the flows in
    -- question; a method whose CFs are all region-tagged would be left with none.
    -- The config loader warns when a 'global-methods' name matches no method.
    globalMethods <-
        maybe [] mcGlobalMethods . M.lookup (unCollectionName collection)
            <$> readTVarIO (dmAvailableMethods manager)
    let !raw0 = buildMethodTables (cfFamily (methodUnit method)) cmap energyDensities expanded
        !raw =
            if methodName method `elem` globalMethods
                then raw0{mtRegionalizedCF = M.empty}
                else raw0
        !withBroadcast = fillBroadcastVector unitConfig mUnits mFlows raw
        -- Precompute per-activity weights for regionalized methods so subsequent
        -- scoring is a dot product instead of one biosphere-triple walk per pid.
        !tables = fillRegionalActivityWeights unitConfig mUnits mFlows db hier withBroadcast
    mapM_ (reportProgress Warning) (regionalGapWarning (mtRegionalActivityWeights tables))
    mapM_ (reportProgress Warning) (seaWaterWarning raw0)
    mapM_
        (reportProgress Warning)
        (zeroedWarning mUnits (zeroedMatchedCFs unitConfig mUnits mFlows withBroadcast))
    pure tables
  where
    -- One "[LCIA <method>] ..." line per warning, or nothing when there is
    -- nothing to say. Each stays at the point in the build where its evidence
    -- becomes available: this function runs concurrently under
    -- 'warmMethodTables', and grouping the lines would reorder the log.
    lcia :: String -> String
    lcia body = "[LCIA " <> T.unpack (methodName method) <> "] " <> body

    -- A CF matchable through the union synonym tables but not through its own
    -- direction's view was excluded by the direction restriction alone - the
    -- usual cause is a method whose parser defaulted the direction (no
    -- metadata). Warn so the loss is distinguishable from a genuinely
    -- uncharacterized flow.
    directionWarning :: [MethodCF] -> Maybe String
    directionWarning [] = Nothing
    directionWarning excluded =
        Just . lcia $
            show (length excluded)
                <> " CF(s) match a synonym bridge only outside their flow direction "
                <> "(direction metadata may be missing from the method). Samples: "
                <> show (take 3 (map mcfFlowName excluded))

    regionalGapWarning :: Maybe RegionalActivityWeights -> Maybe String
    regionalGapWarning Nothing = Nothing
    regionalGapWarning (Just weights) = case rawMissingPairs weights of
        [] -> Nothing
        missing ->
            Just . lcia $
                show (length missing)
                    <> " regionalized (flow, location) pair(s) without CF coverage "
                    <> "(after walking parent regions and universal broadcast). "
                    <> "Samples: "
                    <> show (take 3 [(show fid, T.unpack loc) | (fid, Location loc) <- missing])

    -- Which side of the sea-water gate this method landed on, said out loud.
    -- A method with no sea-water factor of its own has its medium-level factor
    -- applied to sea emissions, and that is only right when the method had
    -- nothing different to say there. When its sea lines were instead lost on
    -- import, the same silence overstates every sea emission it covers - and
    -- the two cases are indistinguishable from the outside. Report the regime
    -- so a method author can tell them apart; only for a method that writes
    -- water factors at all, since the others have no stake in it.
    seaWaterWarning :: MethodTables -> Maybe String
    seaWaterWarning tables = case mtSeaWaterCFs tables of
        MethodDeclaresSeaWater -> Nothing
        MethodSilentOnSeaWater
            | waterCFs == 0 -> Nothing
            | otherwise ->
                Just . lcia $
                    "no sea-water factor among "
                        <> show waterCFs
                        <> " water factor(s): the medium-level factor will be applied to sea "
                        <> "emissions. Right when the method draws no distinction there, wrong "
                        <> "when its sea lines were lost on import."
      where
        waterCFs :: Int
        waterCFs =
            length [() | (_, Just Water, _) <- M.keys (mtExactCF tables)]
                + length [() | (_, Just Water) <- M.keys (mtFallbackCF tables)]

    -- A CF that matched (broadcast or regionalized) but cannot be
    -- unit-converted scores an (intentional) 0 - refusing wrong-dimension data
    -- is right, hiding the refusal is not: unreported, it reads exactly like an
    -- uncharacterized flow and the method silently undercounts. One
    -- deduplicated WARN per (db, method), same channel as the regionalized
    -- coverage gaps above.
    zeroedWarning :: UnitDB -> [(BiosphereFlow, CF, RefusalReason)] -> Maybe String
    zeroedWarning _ [] = Nothing
    zeroedWarning units zeroed =
        Just . lcia $
            show (length zeroed)
                <> " flow(s) matched a CF that cannot be converted from the flow's unit "
                <> "(no unit-conversion path); their contributions score 0. Samples: "
                <> show (take 3 [(T.unpack (bfName f), flowUnitOf f, T.unpack u, show reason) | (f, CF _ (CFUnit u), reason) <- zeroed])
      where
        flowUnitOf :: BiosphereFlow -> String
        flowUnitOf f = maybe "<unknown unit>" (T.unpack . unitName) (M.lookup (bfUnitId f) units)

{- | Run @build@ at most once per @key@ across concurrent callers. The first
caller installs a slot and runs the build; others block on the same result
rather than duplicating the work. @onSuccess@ runs in the slot-clearing
transaction, so a built value lands in its cache atomically with the release. A
failed build clears the slot (the next caller retries) and re-throws.

A cache purge that ran while the build was in flight takes the slot away. The
value was computed from the state that purge invalidated, so the owner returns
it to its callers but does not put it in the cache: publishing it would refill,
behind the purge's back, exactly what the purge emptied.
-}
singleFlight ::
    (Ord k) =>
    TVar (Map k (TMVar (Either SomeException a))) ->
    k ->
    (a -> STM ()) ->
    IO a ->
    IO a
singleFlight inflightVar key onSuccess build = do
    (slot, owner) <- atomically $ do
        inflight <- readTVar inflightVar
        case M.lookup key inflight of
            Just s -> pure (s, False)
            Nothing -> do
                s <- newEmptyTMVar
                writeTVar inflightVar (M.insert key s inflight)
                pure (s, True)
    if not owner
        then either Control.Exception.throwIO pure =<< atomically (readTMVar slot)
        else do
            result <- try build
            atomically $ do
                inflight <- readTVar inflightVar
                when (M.lookup key inflight == Just slot) $ do
                    writeTVar inflightVar (M.delete key inflight)
                    either (const (pure ())) onSuccess result
                putTMVar slot result
            either Control.Exception.throwIO pure result

{- | Kick off a background warm-up of every loaded method's lookup tables
against @dbName@, so the expensive (regional) build is paid once at load time
rather than on the first user score. Single-flighting means a request arriving
mid-warm joins the in-flight build instead of starting a second one.

Methods are built one at a time in a single background thread, deliberately
sequential rather than 'mapMethodSetToTablesCached''s concurrent fan-out: the
heavy regional builds (e.g. AWARE water use) thrash the GC when run in parallel,
so serial warming is both lower-peak-memory and faster wall-clock here. A
failure is logged, not fatal: the on-demand path rebuilds and surfaces it.
-}
warmMethodTables :: DatabaseManager -> Text -> Database -> IO ()
warmMethodTables manager dbName db = void $ forkIO $ withLogScope dbName $ do
    collections <- readTVarIO (dmLoadedMethods manager)
    let methods = [(collName, m) | (collName, mc) <- M.toList collections, m <- mcMethods mc]
    t0 <- getCurrentTime
    reportProgress Info $
        "[warm] " <> T.unpack dbName <> ": warming " <> show (length methods) <> " method table(s) in background…"
    forM_ methods $ \(collName, method) -> do
        r <- try (void (mapMethodToTablesCached manager dbName (CollectionName collName) db method))
        case r of
            Right () -> pure ()
            Left (e :: SomeException) ->
                reportProgress Warning $
                    "[warm] "
                        <> T.unpack dbName
                        <> " / "
                        <> T.unpack collName
                        <> " / "
                        <> T.unpack (methodName method)
                        <> ": "
                        <> show e
    t1 <- getCurrentTime
    reportProgress Info $
        "[warm] "
            <> T.unpack dbName
            <> ": done ("
            <> show (round (realToFrac (diffUTCTime t1 t0) :: Double) :: Int)
            <> "s)"

{- | Cached stacked CF tables for multi-method scoring. Built once per
(dbName, sortedMethodIds), so two requests asking for the same method set
(in any order) share an entry. Per-method 'MethodTables' are sourced via
'mapMethodToTablesCached' and re-used; the only set-level work is stacking
broadcasts into a dense matrix when none of the methods are regionalized.
-}
mapMethodSetToTablesCached :: DatabaseManager -> Text -> CollectionName -> Database -> [Method] -> IO MethodSetTables
mapMethodSetToTablesCached manager dbName collection db methods = do
    -- Canonical key = (dbName, collection, sorted methodIds). Stable regardless
    -- of input ordering so subset-arbitrary requests don't fragment the cache.
    let sortedMethods = sortOn methodId methods
        key = (dbName, collection, map methodId sortedMethods)
    cache <- readTVarIO (dmMethodSetTablesCache manager)
    case M.lookup key cache of
        Just mst -> pure mst
        Nothing -> do
            -- mapConcurrently here parallelizes the per-method 'MethodTables'
            -- build across the whole collection. On first request for a
            -- method set, this concretely parallelizes the expensive
            -- regionalized 'fillRegionalActivityWeights' walks (one per
            -- regio method × biosphere-triple stream). For EF 3.1 (5 regio
            -- methods over agribalyse) this trades a ~110s sequential warm-up
            -- for a ~25-30s parallel one. Concurrent cache writes on the
            -- per-method cache are idempotent under STM (last write wins,
            -- same value).
            tables <- mapConcurrently (mapMethodToTablesCached manager dbName collection db) sortedMethods
            let !mst = buildMethodSetTables (zip sortedMethods tables)
            atomically $ modifyTVar' (dmMethodSetTablesCache manager) (M.insert key mst)
            pure mst

{- | Cached method index (CF tokens, by-medium, by-CAS): built once per
(db, method), reused by the post-scoring suggester. Doesn't depend on the
'Database' itself, only on the method's CF list, but keyed by (dbName,
methodId) to share lifetime semantics with the tables cache.
-}
mapMethodToIndexCached :: DatabaseManager -> Text -> CollectionName -> Method -> IO MethodIndex
mapMethodToIndexCached manager dbName collection method = do
    let key = (dbName, collection, methodId method)
    cache <- readTVarIO (dmMethodIndexCache manager)
    case M.lookup key cache of
        Just idx -> pure idx
        Nothing -> do
            let !idx = buildMethodIndex method
            atomically $ modifyTVar' (dmMethodIndexCache manager) (M.insert key idx)
            pure idx

{- | Hold a database name against concurrent work for the duration of an
action, and release it whether the action returns or throws.

@decide@ runs in the same transaction as the claim, so a caller refuses for
its own reasons (a name already taken, an edit already running) with no
window between finding the name free and taking it. That window is the whole
reason this is one function: two copies of it, written apart, are two chances
to widen it.

The reservation lives in 'dmStagingDbs' whatever the slow work is. Staging,
copying and editing all rewrite the same database under the same name, so
they contend with each other and not only with their own kind.

'getDatabaseSetupInfo' does not come through here on purpose: it waits for
whoever holds the name (STM @retry@) instead of refusing, and it has a third
answer (already staged, nothing to reserve) that this shape has no room for.
-}
withReservedName ::
    DatabaseManager ->
    Text ->
    -- | Refuse with the caller's own error, or carry a value into the action
    STM (Either e a) ->
    (a -> IO (Either e b)) ->
    IO (Either e b)
withReservedName manager dbName decide act = do
    claimed <- atomically $ do
        decided <- decide
        case decided of
            Left err -> pure (Left err)
            Right carried ->
                Right carried <$ modifyTVar' (dmStagingDbs manager) (S.insert dbName)
    case claimed of
        Left err -> pure (Left err)
        Right carried ->
            Control.Exception.finally
                (act carried)
                (atomically $ modifyTVar' (dmStagingDbs manager) (S.delete dbName))

{- | Clear all cached flow mappings (call when databases, methods, or synonyms change).
Also drops the merged flow/unit snapshots: both caches depend on the loaded-DB set.
-}
clearMethodMappingCache :: DatabaseManager -> IO ()
clearMethodMappingCache manager = atomically $ do
    writeTVar (dmMethodMappingCache manager) M.empty
    writeTVar (dmMethodTablesCache manager) M.empty
    writeTVar (dmMethodTablesInflight manager) M.empty
    writeTVar (dmMethodSetTablesCache manager) M.empty
    writeTVar (dmMethodIndexCache manager) M.empty
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing
    writeTVar (dmFlowClosureCache manager) M.empty

{- | Clear cached flow mappings for a specific database, and for every database
that depends on it: a mapping is built over the root's flow closure, so a
dependency that changes invalidates its dependents' tables as much as its own.

The merged flow/unit snapshots span every loaded DB, so a single-DB mutation
still invalidates them fully.
-}
clearMethodMappingCacheForDb :: DatabaseManager -> Text -> IO ()
clearMethodMappingCacheForDb manager dbName = atomically $ do
    loaded <- readTVar (dmLoadedDbs manager)
    let stale = dependentsClosure loaded dbName
        keep (dn, _, _) _ = not (S.member dn stale)
    modifyTVar' (dmMethodMappingCache manager) (M.filterWithKey keep)
    modifyTVar' (dmMethodTablesCache manager) (M.filterWithKey keep)
    modifyTVar' (dmMethodTablesInflight manager) (M.filterWithKey keep)
    modifyTVar' (dmMethodSetTablesCache manager) (M.filterWithKey keep)
    modifyTVar' (dmMethodIndexCache manager) (M.filterWithKey keep)
    modifyTVar' (dmFlowClosureCache manager) (M.filterWithKey (\dn _ -> not (S.member dn stale)))
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

{- | A database and everything that reaches it through 'dbDependsOn',
transitively. The seen set is what makes a dependency cycle terminate.
-}
dependentsClosure :: Map Text LoadedDatabase -> Text -> S.Set Text
dependentsClosure loaded = go S.empty . pure
  where
    go :: S.Set Text -> [Text] -> S.Set Text
    go seen [] = seen
    go seen (name : rest)
        | S.member name seen = go seen rest
        | otherwise = go (S.insert name seen) (rest ++ directDependents name)

    directDependents :: Text -> [Text]
    directDependents name =
        [ other
        | (other, ld) <- M.toList loaded
        , name `elem` dbDependsOn (ldDatabase ld)
        ]

{- | Everything a manager is built out of, gathered by the discovery and
loading steps of 'initDatabaseManager' before any TVar exists.
-}
data ManagerSeed = ManagerSeed
    { msDatabases :: ![DatabaseConfig]
    , msMethods :: ![MethodConfig]
    , msRefData :: !RefDataSources
    , msCachePolicy :: !CachePolicy
    , msGeographies :: !(Map Text (Text, [Text]))
    , msChemSynonyms :: !ChemSynonyms
    , msSubstanceEdges :: ![SubstanceEdge]
    , msCasBindings :: !(Map NormName CASNumber)
    }

-- | The four kinds of reference data, each configured plus whatever was uploaded.
data RefDataSources = RefDataSources
    { rdsFlowSynonyms :: ![RefDataConfig]
    , rdsCompartmentMaps :: ![RefDataConfig]
    , rdsUnitDefs :: ![RefDataConfig]
    , rdsEnergyDensities :: ![RefDataConfig]
    }

{- | Initialize database manager from config
Pre-loads databases with load=true at startup
Also discovers uploaded databases from uploads/ directory
-}
initDatabaseManager :: Config -> CachePolicy -> IO DatabaseManager
initDatabaseManager config cachePolicy = do
    databases <- discoverDatabases config
    methods <- discoverMethods config
    refData <- discoverRefDataSources config
    geographies <- loadGeographies (cfgGeographies config)
    chemSyns <- loadChemSynonymsOrEmpty (cfgChemSynonyms config)
    substanceEdges <- loadSubstanceEdges (cfgSubstanceEdges config)
    casBindings <- bindSubstanceCas substanceEdges
    manager <-
        newManager
            ManagerSeed
                { msDatabases = databases
                , msMethods = methods
                , msRefData = refData
                , msCachePolicy = cachePolicy
                , msGeographies = geographies
                , msChemSynonyms = chemSyns
                , msSubstanceEdges = substanceEdges
                , msCasBindings = casBindings
                }

    autoLoadRefDataSources manager refData

    totalStart <- getCurrentTime
    loadAllDatabases manager databases
    loadConfiguredMethods manager config
    totalEnd <- getCurrentTime
    reportProgressWithTiming
        Info
        "Total startup loading time"
        (realToFrac (diffUTCTime totalEnd totalStart) :: Double)

    return manager

-- | Configured databases with their format detected, plus the uploaded ones.
discoverDatabases :: Config -> IO [DatabaseConfig]
discoverDatabases config = do
    configured <- forM (cfgDatabases config) $ \dbConfig -> do
        {- An archive that will not extract leaves the configuration pointing
        at it. Say so here, at boot, where an operator can act on it; the load
        itself refuses later with the same reason. -}
        resolved <- resolveDataPath (dcPath dbConfig)
        let resolvedPath = fromRight (dcPath dbConfig) resolved
        either (reportError . T.unpack) (const (pure ())) resolved
        format <- Upload.detectDatabaseFormat resolvedPath
        return dbConfig{dcPath = resolvedPath, dcFormat = Just format}
    -- Uploaded databases are self-describing, through their meta.toml
    uploaded <- discoverUploadedDatabases
    let combined = configured ++ uploaded
    mapM_ (reportProgress Warning) (shadowedNames "database" dcName dcPath combined)
    return combined

-- | Configured method collections plus the uploaded ones.
discoverMethods :: Config -> IO [MethodConfig]
discoverMethods config = do
    combined <- (cfgMethods config ++) <$> discoverUploadedMethodConfigs
    mapM_ (reportProgress Warning) (shadowedMethods combined)
    return combined

-- | Configured reference data plus whatever sits under @uploads/<kind>/@.
discoverRefDataSources :: Config -> IO RefDataSources
discoverRefDataSources config =
    RefDataSources
        <$> withUploads "flow synonym" (cfgFlowSynonyms config) "uploads/flow-synonyms"
        <*> withUploads "compartment mapping" (cfgCompartmentMappings config) "uploads/compartment-mappings"
        <*> withUploads "unit" (cfgUnits config) "uploads/units"
        <*> withUploads "energy density" (cfgEnergyDensities config) "uploads/energy-densities"
  where
    withUploads :: String -> [RefDataConfig] -> FilePath -> IO [RefDataConfig]
    withUploads kind configured dir = do
        combined <- (configured ++) <$> discoverUploadedRefData dir
        mapM_ (reportProgress Warning) (shadowedNames (kind <> " source") rdName (describeSource . rdSource) combined)
        pure combined

{- | One warning per name held by more than one entry, naming the one that is
read and the ones that are not.

'newManager' indexes databases and reference sources by name, so a repeated one
keeps the last and drops the rest. The configuration refuses a repeated name,
but it only ever sees the configured half: what reaches these indexes is that
half concatenated with whatever the uploads directory holds, so an uploaded
directory named like a configured entry is all it takes to replace it. Which
one wins then follows from a concatenation order nothing states, and it used to
be said nowhere.
-}
shadowedNames :: String -> (a -> Text) -> (a -> String) -> [a] -> [String]
shadowedNames kind nameOf describe entries =
    [ "More than one "
        <> kind
        <> " named "
        <> T.unpack name
        <> "; reading "
        <> NE.last paths
        <> ", ignoring "
        <> intercalate ", " (NE.init paths)
    | (name, paths) <- Indexing.collisions [(nameOf e, describe e) | e <- entries]
    ]

{- | The same for method collections, which cannot be told which one is read,
because two registries answer and they disagree.

'dmAvailableMethods' is indexed like the others and keeps the last entry, so a
listing describes the uploaded collection. The boot load walks the configured
list instead and takes the active ones, and an uploaded collection is never
active, so what is actually scored with is the configured one. Under a repeated
name those are two different collections, and neither is simply ignored.
-}
shadowedMethods :: [MethodConfig] -> [String]
shadowedMethods mcs =
    [ "More than one method collection named "
        <> T.unpack name
        <> "; listed from "
        <> NE.last paths
        <> " and loaded from "
        <> NE.head paths
        <> ", so the name answers with two different collections"
    | (name, paths) <- Indexing.collisions [(mcName mc, mcPath mc) | mc <- mcs]
    ]

{- | The location hierarchy this run scores against. Falling back to the
built-in hierarchy when a named file cannot be read would change every
regionalized score without anyone asking, so a failure leaves no hierarchy.
-}
loadGeographies :: Maybe FilePath -> IO (Map Text (Text, [Text]))
loadGeographies Nothing =
    -- No file named: the hierarchy the binary carries. It cannot fail to parse
    -- unless the build did (BuiltinSpec compares it with its file), so a
    -- failure here is reported as the defect it is.
    case parseGeographies "the built-in geographies" (BL.toStrict builtinGeographies) of
        Right geos -> do
            reportProgress Info $ "Loaded " <> show (M.size geos) <> " built-in geographies"
            pure geos
        Left err -> do
            reportError $
                "Could not read "
                    <> T.unpack err
                    <> ": this binary was built wrong, running with no hierarchy"
            pure M.empty
loadGeographies (Just path) =
    parseGeographiesCSV path >>= \case
        Right geos -> do
            reportProgress Info $ "Loaded " <> show (M.size geos) <> " geographies from " <> path
            pure geos
        Left err -> do
            reportProgress Warning $
                "Could not load geographies from " <> T.unpack err <> " (running with no hierarchy)"
            pure M.empty

loadChemSynonymsOrEmpty :: Maybe FilePath -> IO ChemSynonyms
loadChemSynonymsOrEmpty Nothing = pure emptyChemSynonyms
loadChemSynonymsOrEmpty (Just path) =
    loadChemSynonyms path >>= \case
        Right cs -> pure cs
        Left err -> do
            putStrLn $ "warning: could not load chem synonyms from " <> path <> ": " <> err
            pure emptyChemSynonyms

loadSubstanceEdges :: Maybe FilePath -> IO [SubstanceEdge]
loadSubstanceEdges Nothing = pure []
loadSubstanceEdges (Just path) = do
    isFile <- doesFileExist path
    if not isFile
        then do
            putStrLn $ "warning: substance edges file not found: " <> path
            pure []
        else do
            raw <- BL.readFile path
            case parseSubstanceEdges (KeyNormalizers (NormName . normalizeName) (CASNumber . normalizeCAS)) raw of
                Right es -> pure es
                Left err -> do
                    putStrLn $ "warning: could not load substance edges from " <> path <> ": " <> T.unpack err
                    pure []

-- | The name-to-CAS bindings the edges imply, announcing every name bound twice.
bindSubstanceCas :: [SubstanceEdge] -> IO (Map NormName CASNumber)
bindSubstanceCas edges = do
    forM_ conflicts $ \(NormName n, (CASNumber kept, CASNumber ignored)) ->
        putStrLn $
            "warning: substance_edges.csv binds flow name '"
                <> T.unpack n
                <> "' to two CAS ("
                <> T.unpack kept
                <> " kept, "
                <> T.unpack ignored
                <> " ignored)"
    pure bindings
  where
    bindings :: Map NormName CASNumber
    conflicts :: [(NormName, (CASNumber, CASNumber))]
    (bindings, conflicts) = casBindingsFromEdges edges

-- | Every TVar a manager owns, empty, around the values it was seeded with.
newManager :: ManagerSeed -> IO DatabaseManager
newManager ManagerSeed{..} = do
    loadedDbsVar <- newTVarIO M.empty
    stagedDbsVar <- newTVarIO M.empty
    stagingDbsVar <- newTVarIO S.empty
    indexedDbsVar <- newTVarIO M.empty
    availableDbsVar <- newTVarIO $ M.fromList [(dcName dc, dc) | dc <- msDatabases]
    availableMethodsVar <- newTVarIO $ M.fromList [(mcName mc, mc) | mc <- msMethods]
    loadedMethodsVar <- newTVarIO M.empty
    availableFlowSynsVar <- byName (rdsFlowSynonyms msRefData)
    loadedFlowSynsVar <- newTVarIO M.empty
    availableCompMapsVar <- byName (rdsCompartmentMaps msRefData)
    loadedCompMapsVar <- newTVarIO M.empty
    availableUnitDefsVar <- byName (rdsUnitDefs msRefData)
    loadedUnitDefsVar <- newTVarIO M.empty
    availableEnergyDensitiesVar <- byName (rdsEnergyDensities msRefData)
    loadedEnergyDensitiesVar <- newTVarIO M.empty
    methodMappingCacheVar <- newTVarIO M.empty
    methodTablesCacheVar <- newTVarIO M.empty
    methodTablesInflightVar <- newTVarIO M.empty
    methodSetTablesCacheVar <- newTVarIO M.empty
    methodIndexCacheVar <- newTVarIO M.empty
    mergedFlowMetadataCacheVar <- newTVarIO Nothing
    mergedUnitConfigCacheVar <- newTVarIO Nothing
    flowClosureCacheVar <- newTVarIO M.empty
    return
        DatabaseManager
            { dmLoadedDbs = loadedDbsVar
            , dmStagedDbs = stagedDbsVar
            , dmStagingDbs = stagingDbsVar
            , dmIndexedDbs = indexedDbsVar
            , dmAvailableDbs = availableDbsVar
            , dmAvailableMethods = availableMethodsVar
            , dmLoadedMethods = loadedMethodsVar
            , dmAvailableFlowSyns = availableFlowSynsVar
            , dmLoadedFlowSyns = loadedFlowSynsVar
            , dmAvailableCompMaps = availableCompMapsVar
            , dmLoadedCompMaps = loadedCompMapsVar
            , dmAvailableUnitDefs = availableUnitDefsVar
            , dmLoadedUnitDefs = loadedUnitDefsVar
            , dmAvailableEnergyDensities = availableEnergyDensitiesVar
            , dmLoadedEnergyDensities = loadedEnergyDensitiesVar
            , dmCachePolicy = msCachePolicy
            , dmGeographies = msGeographies
            , dmLocationHierarchy = hierarchyFromGeographies msGeographies
            , dmMethodMappingCache = methodMappingCacheVar
            , dmMethodTablesCache = methodTablesCacheVar
            , dmMethodTablesInflight = methodTablesInflightVar
            , dmMethodSetTablesCache = methodSetTablesCacheVar
            , dmMethodIndexCache = methodIndexCacheVar
            , dmChemSynonyms = msChemSynonyms
            , dmSubstanceEdges = msSubstanceEdges
            , dmCasBindings = msCasBindings
            , dmMergedFlowMetadataCache = mergedFlowMetadataCacheVar
            , dmMergedUnitConfigCache = mergedUnitConfigCacheVar
            , dmFlowClosureCache = flowClosureCacheVar
            }
  where
    byName :: [RefDataConfig] -> IO (TVar (Map Text RefDataConfig))
    byName rds = newTVarIO (M.fromList [(rdName rd, rd) | rd <- rds])

{- | Load the active reference data into the manager. Flow synonyms go through
their own binary cache, which is what keeps 161K pairs under a second instead
of fifteen.
-}
autoLoadRefDataSources :: DatabaseManager -> RefDataSources -> IO ()
autoLoadRefDataSources manager RefDataSources{..} = do
    reportProgress Info $
        "Loading reference data: "
            ++ show (length rdsUnitDefs)
            ++ " unit config(s), paths: "
            ++ unwords (map (describeSource . rdSource) rdsUnitDefs)
    autoLoadFlowSynonyms (dmLoadedFlowSyns manager) rdsFlowSynonyms
    autoLoadRefData compMapOps (dmLoadedCompMaps manager) rdsCompartmentMaps
    autoLoadRefData unitDefOps (dmLoadedUnitDefs manager) rdsUnitDefs
    autoLoadRefData energyDensityOps (dmLoadedEnergyDensities manager) rdsEnergyDensities

-- | Load every configured database, one dependency level at a time, in parallel.
loadAllDatabases :: DatabaseManager -> [DatabaseConfig] -> IO ()
loadAllDatabases manager allDbConfigs =
    case resolveLoadOrder allDbConfigs of
        Left err -> reportError $ "Dependency resolution failed: " <> T.unpack err
        Right loadOrder -> do
            synonymDB <- getMergedSynonymDB manager
            warnReopenedBridges synonymDB
            unitConfig <- getMergedUnitConfig manager
            let levels = computeDepLevels configMap loadOrder
                dbsToLoad = configsNamed loadOrder
            reportProgress Info $
                "Loading "
                    ++ show (length dbsToLoad)
                    ++ " database(s) in "
                    ++ show (length levels)
                    ++ " dependency levels: "
                    ++ T.unpack (T.intercalate " → " [T.intercalate "," names | names <- levels])
            forM_ (zip [1 :: Int ..] levels) $ \(levelNum, levelNames) -> do
                let levelConfigs = configsNamed levelNames
                reportProgress Info $
                    "  Level "
                        ++ show levelNum
                        ++ ": loading "
                        ++ show (length levelConfigs)
                        ++ " database(s) in parallel"
                currentIndexedDbs <- readTVarIO (dmIndexedDbs manager)
                let level =
                        LoadLevel
                            { llSynonyms = synonymDB
                            , llUnitConfig = unitConfig
                            , llOtherIndexes = M.elems currentIndexedDbs
                            }
                mapConcurrently_ (loadOneDatabase manager level) levelConfigs
            loadedCount <- M.size <$> readTVarIO (dmLoadedDbs manager)
            reportProgress Info $ "Multi-database mode: " ++ show loadedCount ++ " database(s) loaded"
  where
    configMap :: Map Text DatabaseConfig
    configMap = M.fromList [(dcName c, c) | c <- allDbConfigs]

    configsNamed :: [Text] -> [DatabaseConfig]
    configsNamed names = [c | name <- names, Just c <- [M.lookup name configMap]]

-- | Load the method collections the config marks active.
loadConfiguredMethods :: DatabaseManager -> Config -> IO ()
loadConfiguredMethods manager config =
    forM_ (filter mcActive (cfgMethods config)) $ \mc ->
        loadMethodCollectionFromConfig mc >>= \case
            Left err ->
                reportError $
                    "  [FAIL] Failed to load method " <> T.unpack (mcName mc) <> ": " <> T.unpack err
            Right (collection0, flowInfo) -> do
                let (collection, patchStats) = applyMethodConfig mc collection0
                atomically $ modifyTVar' (dmLoadedMethods manager) (M.insert (mcName mc) collection)
                reportProgress Info $
                    "  [OK] Loaded method: "
                        <> T.unpack (mcName mc)
                        <> " ("
                        <> show (length (mcMethods collection))
                        <> " impact categories)"
                warnZeroTouchPatches (mcName mc) patchStats
                warnUnknownGlobalMethods mc collection
                let !pairs = extractFromILCDFlows flowInfo
                autoCreateFlowSynonyms manager (mcName mc) (SynonymOrigin ("Auto-extracted from " <> mcName mc)) pairs

{- | Surface a 'global-methods' entry that matches no loaded method: the
de-regionalization is keyed by method name, so a typo or a renamed method
would otherwise be ignored in silence and the method would stay regionalized,
diverging from the reference.
-}
warnUnknownGlobalMethods :: MethodConfig -> MethodCollection -> IO ()
warnUnknownGlobalMethods mc collection =
    unless (null unknownGlobals) $
        reportProgress Warning $
            "  [global-methods] collection "
                <> T.unpack (mcName mc)
                <> ": no method named "
                <> T.unpack (T.intercalate ", " unknownGlobals)
                <> ". These stay regionalized; check for a typo."
  where
    unknownGlobals :: [Text]
    unknownGlobals = filter (`S.notMember` knownMethodNames) (Config.mcGlobalMethods mc)

    knownMethodNames :: S.Set Text
    knownMethodNames = S.fromList (map methodName (mcMethods collection))

{- | What every database in one dependency level is loaded against. The indexes
are a snapshot taken when the level started, and deliberately so: reading
'dmIndexedDbs' per database instead would let each one see its siblings as they
land, and the cross-database links would differ from one startup to the next.
-}
data LoadLevel = LoadLevel
    { llSynonyms :: !SynonymDB
    , llUnitConfig :: !UnitConversion.UnitConfig
    , llOtherIndexes :: ![IndexedDatabase]
    }

-- | Load a single database with per-database timing, then register it
loadOneDatabase :: DatabaseManager -> LoadLevel -> DatabaseConfig -> IO ()
loadOneDatabase manager LoadLevel{..} dbConfig = withLogScope (dcName dbConfig) $ do
    dbStart <- getCurrentTime
    reportProgress Info $ "[STARTING] Loading database: " <> T.unpack (dcDisplayName dbConfig)
    result <-
        loadDatabaseFromConfigWithCrossDB
            dbConfig
            llSynonyms
            llUnitConfig
            (dmCachePolicy manager)
            llOtherIndexes
            (dmLocationHierarchy manager)
    case result of
        Right (loaded0, _source) -> do
            -- Backfill empty bfCAS from the registry's name↔CAS edges before
            -- indexing, so a CAS-less source (e.g. a SimaPro export) still
            -- reaches the native CAS bridge.
            let loaded = loaded0{ldDatabase = enrichBioFlowCAS (dmCasBindings manager) (ldDatabase loaded0)}
                indexedDb = buildIndexedDatabaseFromDB (dcName dbConfig) llSynonyms (ldDatabase loaded)
            atomically $ publishLoaded manager (dcName dbConfig) loaded indexedDb
            dbEnd <- getCurrentTime
            let !dbDuration = realToFrac (diffUTCTime dbEnd dbStart) :: Double
            reportProgressWithTiming Info ("  [OK] Loaded: " <> T.unpack (dcDisplayName dbConfig)) dbDuration
            -- Auto-extract synonyms from biosphere flows
            let bioFlowDb = dbBioFlows (ldDatabase loaded)
                !pairs = extractFromEcoSpold2 bioFlowDb
            reportProgress Info (extractSummary bioFlowDb pairs)
            {- A biosphere flow whose name carries a "/unit" suffix that
            'normalizeName' does not strip silently misses its CF (the SimaPro
            unit-in-name convention; e.g. a "/MJ" absent from 'unitSuffixes').
            Surface it so the fix, adding the unit to 'unitSuffixes', is
            visible. -}
            mapM_ (reportProgress Warning . unstrippedSuffixWarning) $
                M.toList $
                    uncoveredUnitSuffixes
                        (UnitConversion.isKnownUnit llUnitConfig)
                        (map bfName (M.elems bioFlowDb))
            autoCreateFlowSynonyms
                manager
                (dcName dbConfig)
                (SynonymOrigin ("Auto-extracted from " <> dcDisplayName dbConfig))
                pairs
        Left err ->
            reportError $ "  [FAIL] Failed to load " <> T.unpack (dcName dbConfig) <> ": " <> T.unpack err
  where
    extractSummary :: BioFlowDB -> [(Text, Text)] -> String
    extractSummary bioFlowDb pairs =
        "  [EXTRACT] "
            <> T.unpack (dcName dbConfig)
            <> ": "
            <> show (M.size bioFlowDb)
            <> " bio flows, "
            <> show (length [() | f <- M.elems bioFlowDb, not (M.null (bfSynonyms f))])
            <> " with synonyms, "
            <> show (length pairs)
            <> " pairs"

    unstrippedSuffixWarning :: (Text, [Text]) -> String
    unstrippedSuffixWarning (unit, egs) =
        "  [UNIT] "
            <> T.unpack (dcName dbConfig)
            <> ": flow-name suffix /"
            <> T.unpack unit
            <> " not stripped on "
            <> show (length egs)
            <> " flows (add \"/"
            <> T.unpack (T.toLower unit)
            <> "\" to unitSuffixes); e.g. "
            <> T.unpack (T.intercalate ", " (take 3 egs))

{- | Compute dependency levels from topo-sorted load order for parallel loading.
  Level 0 = no deps, level N = depends only on levels 0..N-1.
-}
computeDepLevels :: Map Text DatabaseConfig -> [Text] -> [[Text]]
computeDepLevels configMap loadOrder =
    let
        -- Compute level for each name: max(levels of deps) + 1, or 0 if no deps
        levelOf :: Map Text Int -> Text -> Int
        levelOf lvls name = case M.lookup name configMap of
            Nothing -> 0
            Just cfg -> case dcDepends cfg of
                [] -> 0
                deps -> 1 + maximum [M.findWithDefault 0 d lvls | d <- deps]
        -- Fold through topo-sorted order to assign levels
        levels' = foldl (\acc name -> M.insert name (levelOf acc name) acc) M.empty loadOrder
        -- Group by level
        maxLevel = if M.null levels' then 0 else maximum (M.elems levels')
     in
        [[name | name <- loadOrder, M.findWithDefault 0 name levels' == lvl] | lvl <- [0 .. maxLevel]]

{- | Discover uploaded databases from uploads/ directory
Reads meta.toml from each subdirectory and converts to DatabaseConfig
-}
discoverUploadedDatabases :: IO [DatabaseConfig]
discoverUploadedDatabases = do
    uploads <- UploadedDB.discoverUploadedDatabases
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded database: " <> T.unpack slug
        -- Always detect format from actual files (old uploads may have "unknown")
        let dataDir = dirPath </> UploadedDB.umDataPath meta
        format <- Upload.detectDatabaseFormat dataDir
        return $ uploadMetaToConfig slug dirPath meta{UploadedDB.umFormat = format}

-- | Convert UploadMeta to DatabaseConfig
uploadMetaToConfig :: Text -> FilePath -> UploadedDB.UploadMeta -> DatabaseConfig
uploadMetaToConfig slug dirPath meta =
    DatabaseConfig
        { dcName = slug
        , dcDisplayName = UploadedDB.umDisplayName meta
        , -- Full path to data. An upload's path is relative to its home; a
          -- copy's is absolute (it names the source's files), and '</>'
          -- returns an absolute right operand as it is.
          dcPath = dirPath </> UploadedDB.umDataPath meta
        , dcDescription = UploadedDB.umDescription meta
        , dcLoad = False -- Never auto-load uploads
        , dcDefault = False
        , dcDepends = UploadedDB.umDepends meta
        , dcLocationAliases = M.empty
        , dcFormat = Just (UploadedDB.umFormat meta)
        , dcIsUploaded = True -- Discovered from uploads/ directory
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal -- Uploads can't yet express policy; default to permissive
        , -- Read, never assumed: a database derived under a property key is
          -- rebuilt from this config at every restart, and a hardcoded
          -- 'Declared' here handed it back divided the way its source
          -- declares under a name promising the opposite.
          dcAllocation = UploadedDB.umAllocation meta
        , dcSource = UploadedDB.umSource meta
        }

{- | Record an uploaded database's dependency pin where a restart can find it.

The pin otherwise lives in the staging registry and inside the binary matrix
cache, so a restart between choosing a dependency and finalizing the database
used to lose it without saying so: the database came back linked to nothing.
Writes 'UploadedDB.umDepends' and keeps the in-memory config in step.

A configured (TOML) database has no meta.toml to write and owns its
dependencies in the config file, so it is left alone.
-}
persistUploadDepends :: DatabaseManager -> Text -> [Text] -> IO ()
persistUploadDepends manager dbName deps = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    case M.lookup dbName availableDbs of
        Nothing -> pure ()
        Just dbConfig
            | not (dcIsUploaded dbConfig) -> pure ()
            | otherwise -> do
                uploadsDir <- UploadedDB.getDatabaseUploadsDir
                let uploadRoot = uploadsDir </> T.unpack dbName
                mMeta <- UploadedDB.readUploadMeta uploadRoot
                case mMeta of
                    Nothing ->
                        -- Losing the pin silently is the very bug this exists
                        -- to fix, so a missing meta.toml at least says so.
                        reportProgress Warning $
                            "No meta.toml under "
                                <> uploadRoot
                                <> "; the dependency pin of "
                                <> T.unpack dbName
                                <> " lives in memory only and is lost at restart"
                    Just meta -> UploadedDB.writeUploadMeta uploadRoot meta{UploadedDB.umDepends = deps}
                atomically $
                    modifyTVar' (dmAvailableDbs manager) (M.adjust (\c -> c{dcDepends = deps}) dbName)

{- | Discover uploaded methods from uploads/methods/ directory
Reads meta.toml from each subdirectory and converts to MethodConfig
| Convert a ScoringSetConfig to a ScoringSet
-}
configToScoringSet :: ScoringSetConfig -> ScoringSet
configToScoringSet ssc =
    ScoringSet
        { ssName = sscName ssc
        , ssUnit = sscUnit ssc
        , ssVariables = sscVariables ssc
        , ssComputed = sscComputed ssc
        , ssLabels = sscLabels ssc
        , ssNormalization = sscNormalization ssc
        , ssWeighting = sscWeighting ssc
        , ssScores = sscScores ssc
        , ssDisplayMultiplier = sscDisplayMultiplier ssc
        }

{- | Fold a 'MethodConfig's post-parse adjustments into a freshly parsed
collection: inject the configured scoring sets, then apply the declarative
CF patches ('Config.mcPatches'). Pure: reapplying the same config to the
same source file always yields the same result, so a reload never
compounds a patch. Also returns, per patch, how many CFs it touched (for
the zero-touch warning at the call site).
-}
applyMethodConfig :: MethodConfig -> MethodCollection -> (MethodCollection, [(Config.MethodPatch, Int)])
applyMethodConfig mc collection0 =
    let scoringSets = map configToScoringSet (Config.mcScoringSets mc)
        withScoring = collection0{Method.Types.mcScoringSets = scoringSets}
     in Method.Patch.applyMethodPatches (Config.mcPatches mc) withScoring

{- | Surface a patch that matched no characterization factor: the selector is
almost certainly wrong (a typo'd category or flow name), and staying silent
would leave the collection scoring as if the patch were never declared.
-}
warnZeroTouchPatches :: Text -> [(Config.MethodPatch, Int)] -> IO ()
warnZeroTouchPatches collName stats =
    forM_ [p | (p, n) <- stats, n == 0] $ \patch ->
        reportProgress Warning $
            "  [patch] collection "
                <> T.unpack collName
                <> ": \""
                <> T.unpack (Method.Patch.describePatch patch)
                <> "\" touched 0 characterization factors. Check the selector."

discoverUploadedMethodConfigs :: IO [MethodConfig]
discoverUploadedMethodConfigs = do
    uploads <- UploadedDB.discoverUploadedMethods
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded method: " <> T.unpack slug
        -- Find the actual method XML directory (e.g., ILCD/lciamethods/)
        methodDir <- findMethodDirectory dirPath
        -- Read the format off the directory rather than meta.toml: the file on
        -- disk may predate method-aware detection, and can't drift this way.
        methodFormat <- detectMethodFormat methodDir
        return
            MethodConfig
                { mcName = UploadedDB.umDisplayName meta
                , mcPath = methodDir
                , mcActive = False -- Never auto-load uploaded methods
                , mcIsUploaded = True
                , mcDescription = UploadedDB.umDescription meta
                , mcFormat = detectedFormatLabel methodFormat
                , mcScoringSets = []
                , mcGlobalMethods = []
                , mcPatches = []
                }

-- | Get a database by name
getDatabase :: DatabaseManager -> Text -> IO (Maybe LoadedDatabase)
getDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    return $ M.lookup dbName loadedDbs

{- | Build a 'DepSolverLookup' backed by the manager's loaded-databases map.
Passed to 'SharedSolver.computeInventoryMatrixBatchWithDepsCached' so it can
recurse into cross-database suppliers.
-}
mkDepSolverLookup :: DatabaseManager -> SharedSolver.DepSolverLookup
mkDepSolverLookup manager depDbName = do
    m <- getDatabase manager depDbName
    pure $ fmap (\ld -> (ldDatabase ld, ldSharedSolver ld)) m

-- | List all databases with their status
listDatabases :: DatabaseManager -> IO [DatabaseStatus]
listDatabases manager = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    forM (M.toList availableDbs) $ \(name, config) -> do
        let mLoaded = M.lookup name loadedDbs
            !status = case mLoaded of
                Nothing -> Unloaded
                Just ld
                    | unresolvedCount (dbLinkingStats (ldDatabase ld)) > 0 -> PartiallyLinked
                    | otherwise -> Loaded
            !actCount = maybe 0 (V.length . dbActivities . ldDatabase) mLoaded
        return
            DatabaseStatus
                { dsName = name
                , dsDisplayName = dcDisplayName config
                , dsDescription = dcDescription config
                , dsLoadAtStartup = dcLoad config
                , dsStatus = status
                , dsIsUploaded = dcIsUploaded config
                , dsPath = T.pack (dcPath config)
                , dsFormat = dcFormat config
                , dsActivityCount = actCount
                , dsDependsOn = dcDepends config
                , dsAllocation = dcAllocation config
                , dsSource = dcSource config
                }

-- | File extensions 'resolveDataPath' knows how to extract as archives.
archiveExtensions :: [String]
archiveExtensions = [".zip", ".7z", ".gz", ".xz"]

{- | Resolve a database path: if it's an archive, extract it first.
Extracts to "{archivePath}.d/" and finds the actual data directory inside.
Plain files/directories pass through unchanged.

'Left' when a path naming an archive could not be extracted. Handing the
archive path back instead, as this used to, sent every caller on to report
whatever it made of a @.zip@: no supported database files found, from the
database loader, and a diagnosis reconstructed from the extension, in the
method loader.
-}
resolveDataPath :: FilePath -> IO (Either Text FilePath)
resolveDataPath path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    -- A directory, a missing path, or a plain file goes through unchanged, and
    -- the caller reports whatever is wrong with it.
    if isDir || not isFile || map toLower (takeExtension path) `notElem` archiveExtensions
        then pure (Right path)
        else extractAndFind path
  where
    extractAndFind :: FilePath -> IO (Either Text FilePath)
    extractAndFind archive = do
        let extractDir = archive ++ ".d"
        alreadyExtracted <- hasContent extractDir
        if alreadyExtracted
            then do
                reportProgress Info $ "Using cached extraction: " <> extractDir
                Right <$> Upload.findDataDirectory extractDir
            else do
                createDirectoryIfMissing True extractDir
                reportProgress Info $ "Extracting archive: " <> archive
                result <- Upload.extractArchiveFile archive extractDir
                case result of
                    Left err ->
                        pure . Left $
                            "Archive could not be extracted: " <> T.pack archive <> ": " <> err
                    Right () -> do
                        reportProgress Info "Extraction complete"
                        Right <$> Upload.findDataDirectory extractDir

    -- A directory that exists and holds at least one entry.
    hasContent :: FilePath -> IO Bool
    hasContent dir = do
        exists <- doesDirectoryExist dir
        if exists then not . null <$> listDirectory dir else return False

-- | Load a database from its configuration with cross-database linking support
loadDatabaseFromConfigWithCrossDB ::
    DatabaseConfig ->
    SynonymDB ->
    UnitConversion.UnitConfig ->
    CachePolicy ->
    [IndexedDatabase] -> -- Pre-built indexes from other databases for cross-DB linking
    M.Map Location [Location] -> -- Location hierarchy (empty = use built-in)
    IO (Either Text (LoadedDatabase, LoadSource))
loadDatabaseFromConfigWithCrossDB dbConfig synonymDB unitConfig cachePolicy otherIndexes locationHier = do
    let sourcePath = dcPath dbConfig
        locationAliases = dcLocationAliases dbConfig
    reportProgress Info $ "Loading database from: " <> sourcePath
    dbResult <-
        loadDatabaseRawWithCrossDB
            RawLoad
                { rlDbName = dcName dbConfig
                , rlLoadOptions =
                    Loader.LoadOptions
                        { Loader.loUnitConfig = unitConfig
                        , Loader.loLocationAliases = locationAliases
                        , Loader.loAllocation = dcAllocation dbConfig
                        }
                , rlSourcePath = sourcePath
                , rlCachePolicy = cachePolicy
                , rlSynonymDB = synonymDB
                , rlOtherIndexes = otherIndexes
                , rlLocationHierarchy = locationHier
                , rlGeographyPolicy = dcGeographyPolicy dbConfig
                }

    case dbResult of
        Left err -> return $ Left err
        Right (dbRaw, source) -> do
            -- Initialize runtime fields (synonym DB and flow name index)
            let database = BM25.addBM25Index (initializeRuntimeFields dbRaw synonymDB)
            sharedSolver <- solverFor (dcName dbConfig) database

            return $
                Right
                    ( LoadedDatabase
                        { ldDatabase = database
                        , ldSharedSolver = sharedSolver
                        , ldConfig = dbConfig
                        }
                    , source
                    )

-- | Detected format of a database directory

{- | What shape of source a path holds, as far as picking the thing to hand
the loader goes. The distinction that matters is whether the loader wants one
file (a CSV export, a workbook) or the directory itself (an EcoSpold package,
an ILCD tree); 'dataFileExtension' is where that is decided.
-}
data DirectoryFormat = FormatSpold | FormatXML | FormatCSV | FormatExcel | FormatILCD | FormatUnknown
    deriving (Show, Eq, Bounded, Enum)

{- | The extension whose file inside a source directory the loader is handed,
for the formats that name one file rather than a tree. 'Nothing' means hand
over the directory.

The file branch of 'detectDirectoryFormat' keeps its own extension literals;
these two lists are not tied together, only the constructor set is.
-}
dataFileExtension :: DirectoryFormat -> Maybe String
dataFileExtension fmt = case fmt of
    FormatCSV -> Just ".csv"
    FormatExcel -> Just ".xlsx"
    FormatSpold -> Nothing
    FormatXML -> Nothing
    FormatILCD -> Nothing
    FormatUnknown -> Nothing

{- | How a refusal names what it would have accepted. Derived from the
detector rather than written out, so a format added there cannot go missing
from the sentence a user reads.
-}
supportedSourceFormats :: Text
supportedSourceFormats =
    T.intercalate ", " (mapMaybe label [minBound .. maxBound])
  where
    label :: DirectoryFormat -> Maybe Text
    label f = case f of
        FormatSpold -> Just "EcoSpold v2 (.spold)"
        FormatXML -> Just "EcoSpold v1 (.xml)"
        FormatCSV -> Just "SimaPro CSV (.csv)"
        FormatExcel -> Just "Brightway Excel (.xlsx)"
        FormatILCD -> Just "ILCD"
        FormatUnknown -> Nothing

-- | Detect the format of files in a directory
detectDirectoryFormat :: FilePath -> IO DirectoryFormat
detectDirectoryFormat path = do
    isFile <- doesFileExist path
    if isFile
        then return (fileFormat (map toLower (takeExtension path)))
        else do
            isDir <- doesDirectoryExist path
            if isDir then directoryFormat else return FormatUnknown
  where
    fileFormat :: String -> DirectoryFormat
    fileFormat ext = case ext of
        ".csv" -> FormatCSV
        ".xlsx" -> FormatExcel
        ".spold" -> FormatSpold
        ".xml" -> FormatXML
        _ -> FormatUnknown

    directoryFormat :: IO DirectoryFormat
    directoryFormat =
        firstMatch
            [ (FormatILCD, doesDirectoryExist (path </> "processes"))
            , -- EcoSpold packages keep their datasets in a subdirectory (e.g.
              -- ecoinvent's datasets/*.spold), so probe for .spold recursively.
              -- Otherwise a sibling FilenameToActivityLookup.csv at the package
              -- root masks them and the database misdetects as SimaPro CSV,
              -- silently loading zero activities.
              (FormatSpold, containsExtensionDeep ".spold" path)
            , -- A workbook is probed the same way and for the same reason:
              -- zipping a folder puts it one level down, and a sheet exported
              -- beside it as CSV would otherwise mask it. Ahead of .csv, which
              -- is the order 'Database.Upload.detectDatabaseFormat' uses - the
              -- two must agree or a source is announced as one format and
              -- parsed as another.
              (FormatExcel, containsExtensionDeep ".xlsx" path)
            , (FormatCSV, hasTopLevelExtension ".csv")
            , (FormatXML, hasTopLevelExtension ".xml")
            ]

    -- One listing per probe rather than one shared listing: the two that reach
    -- here are the last two, and both are cheap next to the recursive walks above.
    hasTopLevelExtension :: String -> IO Bool
    hasTopLevelExtension ext =
        elem ext . map (map toLower . takeExtension) <$> listDirectory path

    -- No fallback guess: a directory matching no probe stays FormatUnknown.
    firstMatch :: [(DirectoryFormat, IO Bool)] -> IO DirectoryFormat
    firstMatch [] = return FormatUnknown
    firstMatch ((fmt, probe) : rest) = probe >>= \b -> if b then return fmt else firstMatch rest

{- | Recursively test whether the directory tree rooted at @path@ contains at
least one file with the given (lowercased) extension. Lets dataset files in a
subdirectory drive format detection even when an unrelated file sits at the
package root (e.g. ecoinvent's datasets/*.spold beside a root CSV).
-}
containsExtensionDeep :: String -> FilePath -> IO Bool
containsExtensionDeep ext =
    fmap (any ((== ext) . map toLower . takeExtension)) . listDirectoryRecursive

-- | Files in a directory carrying the given (lowercased) extension.
findFilesWithExtension :: String -> FilePath -> IO [FilePath]
findFilesWithExtension ext path = do
    entries <- listDirectory path
    let candidates = [path </> f | f <- entries, map toLower (takeExtension f) == ext]
    -- A directory named "exports.csv" carries the extension and is not a file;
    -- handing it to a parser is a crash where a refusal belongs.
    filterM doesFileExist candidates

{- | The path to hand the loader: the file itself when the source already is
one, otherwise the single file of this format inside the directory.

A refusal names the extension it looked for rather than saying "no CSV", so a
workbook source that holds no workbook does not report a missing CSV.
-}
narrowToDataFile :: DirectoryFormat -> FilePath -> IO (Either Text FilePath)
narrowToDataFile fmt path = case dataFileExtension fmt of
    Nothing -> pure (Right path)
    Just ext ->
        doesFileExist path >>= \isFile ->
            if isFile
                then pure (Right path)
                else do
                    found <- findFilesWithExtension ext path
                    case found of
                        [] -> pure $ Left ("No " <> T.pack ext <> " files found in: " <> T.pack path)
                        [f] -> pure (Right f)
                        (f : rest) -> do
                            -- listDirectory is unordered, so which one this is
                            -- can differ between two machines. Say so rather
                            -- than let a reload quietly read a different file.
                            reportProgress Warning $
                                "Several "
                                    <> ext
                                    <> " files in "
                                    <> path
                                    <> "; loading "
                                    <> f
                                    <> " and ignoring "
                                    <> show (length rest)
                                    <> " other(s)"
                            pure (Right f)

{- | Everything one raw load reads. Nine values whose positional signature
needed a comment per parameter to be readable at all.
-}
data RawLoad = RawLoad
    { rlDbName :: !Text
    , rlLoadOptions :: !Loader.LoadOptions
    -- ^ The unit table, the location aliases and the allocation key it reads under.
    , rlSourcePath :: !FilePath
    -- ^ Unresolved: the matrix cache is co-located with it.
    , rlCachePolicy :: !CachePolicy
    , rlSynonymDB :: !SynonymDB
    , rlOtherIndexes :: ![IndexedDatabase]
    -- ^ Pre-built indexes of the databases this one may link against.
    , rlLocationHierarchy :: !(M.Map Location [Location])
    -- ^ Empty means the built-in hierarchy.
    , rlGeographyPolicy :: !GeographyPolicy
    }

{- | What the matrix cache next to the source is worth for this load. A cache
that still records unresolved links while dependencies are now available is
worth rebuilding, and that is the one case the caller announces.
-}
data CacheVerdict
    = Fresh Database
    | Stale
    | Absent

cacheVerdict :: [IndexedDatabase] -> Maybe Database -> CacheVerdict
cacheVerdict _ Nothing = Absent
cacheVerdict otherIndexes (Just db)
    | unresolvedCount (dbLinkingStats db) > 0, not (null otherIndexes) = Stale
    | otherwise = Fresh db

{- | Load raw database from a configured source path, with cross-database linking.

The cache lives next to @sourcePath@ (see 'Loader.generateMatrixCacheFilename').
We probe it first using the unresolved @sourcePath@, so a deployment that ships
only the cache (no source archive on disk) still loads, as long as the cache
was built under the unit table and location aliases in force: one that was not
is refused like a stale one, and with no source to read the load fails. On
cache miss/stale we 'resolveDataPath' and parse, saving a fresh cache on
success.
-}
loadDatabaseRawWithCrossDB ::
    RawLoad ->
    IO (Either Text (Database, LoadSource))
loadDatabaseRawWithCrossDB RawLoad{..} = do
    mCachedDb <- case rlCachePolicy of
        NoCache -> return Nothing
        UseCache -> Loader.loadCachedDatabaseWithMatrices rlDbName rlSourcePath inputs
    case cacheVerdict rlOtherIndexes mCachedDb of
        Fresh db -> do
            Loader.reportCrossDBLinkingStats (fromIntegral (dbActivityCount db)) (dbLinkingStats db)
            return $ Right (db, FromCache)
        Stale -> do
            reportProgress Info "Cache has unresolved links, rebuilding with available dependencies..."
            rebuildFromSource
        Absent -> rebuildFromSource
  where
    -- Cache miss or stale: now we need the source. Resolve the archive if any.
    rebuildFromSource :: IO (Either Text (Database, LoadSource))
    rebuildFromSource = resolveDataPath rlSourcePath >>= either (pure . Left) fromPath

    fromPath :: FilePath -> IO (Either Text (Database, LoadSource))
    fromPath path = do
        isFile <- doesFileExist path
        isDir <- doesDirectoryExist path
        if not isFile && not isDir
            then return $ Left $ "Source path does not exist: " <> T.pack rlSourcePath
            else do
                format <- detectDirectoryFormat path
                case format of
                    FormatCSV -> narrowToDataFile format path >>= either (pure . Left) loadCSV
                    -- A workbook names one file, like a CSV export, but the
                    -- parsing itself is Loader's business.
                    FormatExcel -> narrowToDataFile format path >>= either (pure . Left) loadStructured
                    FormatUnknown ->
                        return $
                            Left $
                                "No supported database files found in: "
                                    <> T.pack path
                                    <> ". Supported formats: "
                                    <> supportedSourceFormats
                    FormatSpold -> loadStructured path
                    FormatXML -> loadStructured path
                    FormatILCD -> loadStructured path

    loadCSV :: FilePath -> IO (Either Text (Database, LoadSource))
    loadCSV csvFile = do
        reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
        loaded <- Loader.loadSimaProCSV rlLoadOptions csvFile
        -- This path reaches the parser directly, so the load's own report of
        -- what the allocation key refused has to be asked for here.
        either (const (pure ())) (Loader.reportKeyRefusals rlLoadOptions) loaded
        case loaded of
            Left err -> return $ Left err
            Right linkedDb -> do
                reportProgress Info $ "Building database from " <> show (M.size (sdbActivities linkedDb)) <> " activities"
                dbResult <- buildDatabaseWithMatrices inputs linkedDb
                case dbResult of
                    Left err -> return $ Left err
                    Right db -> do
                        when (rlCachePolicy == UseCache) $
                            Loader.saveCachedDatabaseWithMatrices rlDbName rlSourcePath db
                        Loader.reportCrossDBLinkingStats (fromIntegral (dbActivityCount db)) (dbLinkingStats db)
                        return $ Right (db, FromSource)

    loadStructured :: FilePath -> IO (Either Text (Database, LoadSource))
    loadStructured path = do
        loadResult <-
            Loader.loadDatabaseWithCrossDBLinking
                rlLoadOptions
                rlOtherIndexes
                rlSynonymDB
                rlLocationHierarchy
                rlGeographyPolicy
                path
        case loadResult of
            Left err -> return $ Left err
            Right (simpleDb, stats) -> do
                dbResult <-
                    buildDatabaseWithMatrices inputs simpleDb
                case dbResult of
                    Left err -> return $ Left err
                    Right db -> do
                        let crossLinks = cdlLinks stats
                            depDbs = M.keys (crossDBBySource stats)
                            dbWithLinks =
                                db
                                    { dbCrossDBLinks = crossLinks
                                    , dbDependsOn = depDbs
                                    , dbLinkingStats = stats
                                    }
                        when (rlCachePolicy == UseCache) $
                            Loader.saveCachedDatabaseWithMatrices rlDbName rlSourcePath dbWithLinks
                        return $ Right (dbWithLinks, FromSource)

    inputs :: BuildInputs
    inputs =
        BuildInputs
            (Loader.loUnitConfig rlLoadOptions)
            (Loader.loLocationAliases rlLoadOptions)
            (Loader.loAllocation rlLoadOptions)

-- | Load a single database without auto-loading dependencies
loadDatabaseSingle :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabaseSingle manager dbName = do
    -- Check if already staged -> try to finalize, or clear stale staged entry
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged
            -- Same readiness gate as finalize itself, so the shortcut and the
            -- gate can't disagree
            | isNothing (notReadyReason (stagedLinkCounts staged)) ->
                finalizeDatabase manager dbName
            | otherwise -> do
                -- Cannot finalize: clear staged entry, reload from config
                -- (loadDatabase pre-loaded deps, so fresh load should resolve links)
                atomically $ modifyTVar' (dmStagedDbs manager) (M.delete dbName)
                loadDatabaseSingleFromConfig manager dbName
        Nothing -> loadDatabaseSingleFromConfig manager dbName

{- | How a database that has just been loaded got here. The three are what the
work after the load turns on, and the fourth combination a cache-hit flag and a
replayed flag could spell together - read from a cache and replayed over - does
not exist: a cache hit already holds its edits.
-}
data LoadOrigin
    = CacheHit
    | Parsed
    | Replayed
    deriving (Eq)

{- | Where the raw loader got the database. 'FromCache' means it came out of
the matrix cache as it stood, so cross-database linking was NOT run against
the indexes the caller passed; 'FromSource' means it was.
-}
data LoadSource = FromCache | FromSource
    deriving (Eq, Show)

{- | Whether a load may read and write the matrix cache. 'NoCache' is what
@--no-cache@ asks for, and what a database whose journal runs ahead of its
cache gets whatever the flag says.
-}
data CachePolicy = UseCache | NoCache
    deriving (Eq, Show)

{- | On a fresh parse 'loadDatabaseRawWithCrossDB' already ran linking against
the current indexes, so a follow-up relink is guaranteed no-op work. A cache
hit carries links computed against a previous dependency set, possibly stale
versions of the same names, so it needs one to converge; and a replay clears
the cross-database links exactly as an edit does, so it needs the same.
-}
needsSelfRelink :: LoadOrigin -> Bool
needsSelfRelink origin = case origin of
    CacheHit -> True
    Parsed -> False
    Replayed -> True

-- | Load a database from config (not staged)
loadDatabaseSingleFromConfig ::
    DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabaseSingleFromConfig manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    -- Already loaded is a success, not work to do.
    maybe (runExceptT loadFromConfig) (pure . Right) (M.lookup dbName loadedDbs)
  where
    loadFromConfig :: ExceptT Text IO LoadedDatabase
    loadFromConfig = do
        availableDbs <- liftIO $ readTVarIO (dmAvailableDbs manager)
        dbConfig <-
            except $ maybe (Left ("Database not found: " <> dbName)) Right (M.lookup dbName availableDbs)
        liftIO $ reportProgress Info $ "[STARTING] Loading database: " <> T.unpack (dcDisplayName dbConfig)
        synonymDB <- liftIO $ getMergedSynonymDB manager
        liftIO $ warnReopenedBridges synonymDB
        (loaded, origin) <- parseOrReplay dbConfig synonymDB
        liftIO $ publish dbConfig synonymDB loaded origin
        pure loaded

    {- A cache saved before the last edit was journalled would come back
    without it, so that case reads the sources again and the journal is
    replayed over them. A cache hit already holds the edits (its stamp says
    so); a fresh parse holds only what the author uploaded. -}
    parseOrReplay :: DatabaseConfig -> SynonymDB -> ExceptT Text IO (LoadedDatabase, LoadOrigin)
    parseOrReplay dbConfig synonymDB = do
        currentIndexedDbs <- liftIO $ readTVarIO (dmIndexedDbs manager)
        unitConfig <- liftIO $ getMergedUnitConfig manager
        journalAhead <- liftIO $ journalAheadOfCache dbConfig
        eitherResult <-
            liftIO $
                try $
                    loadDatabaseFromConfigWithCrossDB
                        dbConfig
                        synonymDB
                        unitConfig
                        (if journalAhead then NoCache else dmCachePolicy manager)
                        (M.elems currentIndexedDbs)
                        (dmLocationHierarchy manager)
        ExceptT $ case eitherResult of
            Left (ex :: SomeException) -> pure $ Left $ "Exception loading database: " <> T.pack (show ex)
            Right (Left err) -> pure (Left err)
            Right (Right (loaded, FromCache)) -> pure (Right (loaded, CacheHit))
            Right (Right (loaded, FromSource)) -> replayEdits manager dbConfig loaded

    publish :: DatabaseConfig -> SynonymDB -> LoadedDatabase -> LoadOrigin -> IO ()
    publish dbConfig synonymDB loaded origin = do
        atomically $
            publishLoaded manager dbName loaded (buildIndexedDatabaseFromDB dbName synonymDB (ldDatabase loaded))
        clearMethodMappingCacheForDb manager dbName
        reportProgress Info $ "  [OK] Loaded:" <> T.unpack (dcDisplayName dbConfig)
        -- Auto-extract synonyms from biosphere flows
        autoCreateFlowSynonyms
            manager
            dbName
            (SynonymOrigin ("Auto-extracted from " <> dcDisplayName dbConfig))
            (extractFromEcoSpold2 (dbBioFlows (ldDatabase loaded)))
        when (needsSelfRelink origin) $
            relinkDatabase manager dbName >>= either (warnSelfRelinkFailed dbName) (const (pure ()))
        relinkDependents manager dbName
        {- Cache what the journal produced, so the next load does not replay
        it, and stamp the cache with the journal it holds. The stamp goes
        last: a cache that is not stamped is read again from source. -}
        when (origin == Replayed) $ recordReplayedCache manager dbConfig

{- | A self-relink that failed wrote nothing, and the caller carries on: what
was loaded is usable, its cross-database links are just not converged.
-}
warnSelfRelinkFailed :: Text -> Text -> IO ()
warnSelfRelinkFailed dbName err =
    reportProgress Warning $
        "Self-relink of " <> T.unpack dbName <> " failed: " <> T.unpack err

{- | Where a database keeps its edits, or 'Nothing' for one the engine only
reads from its configuration and never writes.

An upload and a copy both keep theirs in the upload directory named after
them, beside the @meta.toml@ that describes them. For a copy that directory is
the only thing it owns, since its data is the source's.
-}
editHome :: DatabaseConfig -> IO (Maybe FilePath)
editHome dbConfig
    | not (dcIsUploaded dbConfig) = pure Nothing
    | otherwise = do
        uploadsDir <- UploadedDB.getDatabaseUploadsDir
        pure (Just (uploadsDir </> T.unpack (dcName dbConfig)))

{- | Whether the journal has moved since the matrix cache was saved from it.

The cache holds the database after its edits, which is what keeps every load
from replaying them. It is only trustworthy while it can say which journal it
was built from, so a cache whose stamp does not match the journal is not used
at all: the sources are read again and the journal replayed over them.
-}
journalAheadOfCache :: DatabaseConfig -> IO Bool
journalAheadOfCache dbConfig =
    editHome dbConfig >>= \case
        Nothing -> pure False
        Just home -> do
            current <- Journal.journalStamp home
            applied <- Journal.readAppliedStamp home
            pure (current /= applied)

{- | Replay a database's edits over the sources just parsed, returning whether
there were any.

Everything the author was told had happened lives in the journal, so a failure
here refuses the load rather than handing back a database that quietly
disagrees with its own record. The dependencies are the ones the config pins,
already loaded by 'loadDatabase', because a supplier an edit points at may
live in one of them.
-}
replayEdits :: DatabaseManager -> DatabaseConfig -> LoadedDatabase -> IO (Either Text (LoadedDatabase, LoadOrigin))
replayEdits manager dbConfig loaded =
    editHome dbConfig >>= \case
        Nothing -> pure (Right (loaded, Parsed))
        Just home ->
            Journal.readJournal home >>= \case
                Left err -> pure (Left (dcName dbConfig <> ": " <> err))
                Right [] -> pure (Right (loaded, Parsed))
                Right events -> do
                    loadedDbs <- readTVarIO (dmLoadedDbs manager)
                    unitConfig <- getMergedUnitConfig manager
                    synonymDB <- getMergedSynonymDB manager
                    let deps = [ldDatabase ld | name <- dcDepends dbConfig, Just ld <- [M.lookup name loadedDbs]]
                        ctx =
                            AuthorContext
                                { acDb = ldDatabase loaded
                                , acDeps = deps
                                , acUnitConfig = unitConfig
                                }
                    reportProgress Info $
                        "Replaying " <> show (length events) <> " recorded edit(s) of " <> T.unpack (dcName dbConfig)
                    case Journal.replayJournal ctx events of
                        Left err -> pure (Left (dcName dbConfig <> ": " <> err))
                        Right edited -> do
                            -- The rebuild resets the runtime indexes and moves
                            -- every matrix row, so both are made again here.
                            let withRuntime = BM25.addBM25Index (initializeRuntimeFields edited synonymDB)
                            clearCachedSolver (dcName dbConfig)
                            solver <- solverFor (dcName dbConfig) withRuntime
                            pure (Right (loaded{ldDatabase = withRuntime, ldSharedSolver = solver}, Replayed))

{- | Save the replayed database to its matrix cache and stamp the cache with
the journal it now holds, so the next load reads it instead of replaying.
-}
recordReplayedCache :: DatabaseManager -> DatabaseConfig -> IO ()
recordReplayedCache manager dbConfig =
    editHome dbConfig >>= \case
        Nothing -> pure ()
        Just home -> do
            saved <- getDatabase manager (dcName dbConfig)
            mapM_ (Loader.saveCachedDatabaseWithMatrices (dcName dbConfig) (dcPath dbConfig) . ldDatabase) saved
            Journal.journalStamp home >>= Journal.writeAppliedStamp home

-- | Result of a relink operation (unresolved counts before/after).
data RelinkResult = RelinkResult
    { rresDbName :: !Text
    , rresUnresolvedBefore :: !Int
    , rresUnresolvedAfter :: !Int
    , rresCrossDBLinks :: !Int
    , rresDepsLoaded :: ![Text]
    , rresLinksChanged :: !Bool
    {- ^ True iff the relink actually changed 'dbCrossDBLinks' (as a set)
    versus the in-memory state before the call. Callers use this to skip
    redundant work, e.g. the explicit cache write in 'finalizeDatabase'
    is suppressed when the relink already saved.
    -}
    }
    deriving (Show, Eq)

{- | Order-insensitive equality for lists that are semantically sets
(cross-DB links, dependency names). Avoids spurious cache re-saves when
only the element order differs.
-}
sameSet :: (Ord a) => [a] -> [a] -> Bool
sameSet xs ys = S.fromList xs == S.fromList ys

{- | Everything a relink reads. Gathered because the caller has already pulled
all of it out of TVars, and the computation itself touches none.
-}
data RelinkInputs = RelinkInputs
    { riDbName :: !Text
    , riDatabase :: !Database
    , riContext :: !LinkingContext
    , riPersistedDeps :: !(Maybe [Text])
    -- ^ The dependency set the matrix cache on disk records, when it is known.
    }

{- | What a relink produced: the rewritten database, the report for the caller,
and whether the matrix cache on disk is now behind. Whether the links
themselves changed is 'rresLinksChanged' inside the report, not repeated here.
-}
data RelinkOutcome = RelinkOutcome
    { roDatabase :: !Database
    , roResult :: !RelinkResult
    , roCacheChanged :: !Bool
    }

{- | Recompute a loaded database's cross-database links. Pure, and its caller
forces the outcome before opening its STM transaction: none of this work may
land inside a transaction that can be retried.
-}
relinkPlan :: RelinkInputs -> RelinkOutcome
relinkPlan RelinkInputs{..} =
    RelinkOutcome
        { roDatabase = db'
        , roResult =
            RelinkResult
                { rresDbName = riDbName
                , rresUnresolvedBefore = unresolvedCount (dbLinkingStats riDatabase)
                , rresUnresolvedAfter = unresolvedCount newStats
                , rresCrossDBLinks = length newLinks
                , rresDepsLoaded = newDeps
                , rresLinksChanged = linksChanged
                }
        , roCacheChanged = linksChanged || depsChanged
        }
  where
    activityMap :: ActivityMap
    activityMap =
        M.fromList
            [ (dbProcessIdTable riDatabase V.! i, dbActivities riDatabase V.! i)
            | i <- [0 .. V.length (dbActivities riDatabase) - 1]
            ]

    newStats :: CrossDBLinkingStats
    newStats =
        ( Loader.findAllCrossDBLinks
            riContext
            (dbTechFlows riDatabase)
            (dbWasteFlows riDatabase)
            (dbUnits riDatabase)
            activityMap
        )
            { cdlTotalInputs = Loader.countTotalTechInputs (toSimpleDatabase riDatabase)
            }

    newLinks :: [CrossDBLink]
    newLinks = cdlLinks newStats

    -- Strict pin: the dependency set is the user's selection, unchanged by
    -- relinking. Only the links within it are refreshed.
    newDeps :: [Text]
    newDeps = dbDependsOn riDatabase

    db' :: Database
    db' =
        riDatabase
            { dbCrossDBLinks = newLinks
            , dbDependsOn = newDeps
            , dbLinkingStats = newStats
            }

    -- The pin is invariant under relink (newDeps is the set already recorded),
    -- so a change can only be in the links. Compare as sets: link order is not
    -- significant and must not trigger a redundant cache write.
    linksChanged :: Bool
    linksChanged = not (sameSet newLinks (dbCrossDBLinks riDatabase))

    -- A caller may have pinned a new dependency in-memory before this call.
    -- The cache is the only durable store of 'dbDependsOn', so if the live pin
    -- diverges from what is on disk the cache must be rewritten even when no
    -- new links were discovered.
    depsChanged :: Bool
    depsChanged = maybe False (not . sameSet newDeps) riPersistedDeps

{- | Re-run cross-DB linking for an already-loaded DB against its pinned
dependency set ('dbDependsOn'), not the full set of loaded DBs. Updates
'dbCrossDBLinks' and 'dbLinkingStats' in place in the LoadedDatabase record;
the dependency set itself is left untouched (strict pin: it changes only via
explicit add/remove-dependency). Does NOT rebuild the technosphere matrix or
invalidate the MUMPS factorization: cross-DB links are consumed only at
solve time.

Side-effect: persists the updated 'Database' back to its matrix-cache file
('Loader.saveCachedDatabaseWithMatrices') whenever the relink actually
changed 'dbCrossDBLinks'. Without this, the next startup
would re-load the stale cache and re-run cross-DB linking from scratch
even though we already know the answer. The save is skipped when the
relink is a no-op (no change vs. the in-memory state).
-}
relinkDatabase :: DatabaseManager -> Text -> IO (Either Text RelinkResult)
relinkDatabase manager dbName = relinkDatabaseWith manager dbName CrossLinking.emptyAliasMap Nothing

{- | Re-link a loaded DB across its full pinned dependency set, applying a
curated supplier-alias map. The aliases let a consumer's input flow name that
only matches a target supplier (typically in @depDb@) under the mapping still
link; links to the other pinned dependencies are re-resolved unchanged rather
than dropped. If @depDb@ is loaded but not yet in the database's declared
dependency set, it is pinned in-memory first, so an in-memory pipeline
(copy → delete → relink) composes without restaging (which would unload the
live database). Same persistence/no-op semantics as 'relinkDatabase'. Errors
(DB or dep not loaded) surface as 'Left'.
-}
relinkDatabaseWithMapping ::
    DatabaseManager ->
    -- | database to relink
    Text ->
    -- | dependency database to link against
    Text ->
    -- | consumer-flow → designated-supplier aliases
    CrossLinking.AliasMap ->
    IO (Either Text RelinkResult)
relinkDatabaseWithMapping manager dbName depDb aliases = withLogScope dbName $ do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Nothing -> relinkStaged manager dbName (Just depDb) aliases
        Just loaded
            | not (M.member depDb loadedDbs) ->
                return $ Left $ "Dependency database not loaded: " <> depDb <> " (load it first)"
            | otherwise -> do
                -- Declare the dependency in-memory if it isn't already pinned, so an
                -- in-memory pipeline (copy → delete → relink) composes in one pass
                -- without restaging, which would unload the live database. The pin
                -- set on disk is the current one *before* this in-memory addition;
                -- pass it so 'relinkDatabaseWith' persists the cache when the pin
                -- diverges from disk even if no new links are discovered.
                let persistedDeps = dbDependsOn (ldDatabase loaded)
                unless (depDb `elem` persistedDeps) $
                    atomically $
                        modifyTVar' (dmLoadedDbs manager) (M.adjust (addPinnedDep depDb) dbName)
                relinkDatabaseWith manager dbName aliases (Just persistedDeps)
  where
    -- Idempotent: a concurrent relink may have pinned the dep between the
    -- snapshot above and this transaction, so never prepend a duplicate.
    addPinnedDep :: Text -> LoadedDatabase -> LoadedDatabase
    addPinnedDep dep ld =
        let db = ldDatabase ld
         in if dep `elem` dbDependsOn db
                then ld
                else ld{ldDatabase = db{dbDependsOn = dep : dbDependsOn db}}

{- | Relink a *staged* (parsed-but-not-finalized) database, mirroring
'relinkDatabaseWith' on the staged 'SimpleDatabase' via the shared
'Loader.relinkSimpleDatabase'. This lets the relink endpoint work from the setup
page before a database is finalized. @maybeDepDb@ pins a chosen dependency (a
mapping relink); @aliases@ feeds the supplier-alias map.
-}
relinkStaged :: DatabaseManager -> Text -> Maybe Text -> CrossLinking.AliasMap -> IO (Either Text RelinkResult)
relinkStaged manager dbName maybeDepDb aliases = withLogScope dbName $ runExceptT $ do
    stagedDbs <- liftIO $ readTVarIO (dmStagedDbs manager)
    staged <- except $ maybe (Left ("Database not loaded: " <> dbName)) Right (M.lookup dbName stagedDbs)
    indexedDbs <- liftIO $ readTVarIO (dmIndexedDbs manager)
    forM_ maybeDepDb $ \dep ->
        unless (M.member dep indexedDbs) $
            throwE ("Dependency database not loaded: " <> dep <> " (load it first)")
    synonymDB <- liftIO $ getMergedSynonymDB manager
    unitConfig <- liftIO $ getMergedUnitConfig manager
    let pinnedDeps = withChosenDep (sdSelectedDeps staged)
        newStats =
            Loader.relinkSimpleDatabase
                [idx | (n, idx) <- M.toList indexedDbs, n `elem` pinnedDeps]
                synonymDB
                unitConfig
                (dmLocationHierarchy manager)
                (dcGeographyPolicy (sdConfig staged))
                aliases
                (sdSimpleDB staged)
    liftIO $
        atomically $
            modifyTVar'
                (dmStagedDbs manager)
                (M.insert dbName (withRelinkedDeps pinnedDeps newStats staged))
    pure (relinkResult staged pinnedDeps newStats)
  where
    -- A mapping relink pins the dependency it retargets, once.
    withChosenDep :: [Text] -> [Text]
    withChosenDep selected = case maybeDepDb of
        Just dep | dep `notElem` selected -> dep : selected
        _ -> selected

    relinkResult :: StagedDatabase -> [Text] -> Loader.CrossDBLinkingStats -> RelinkResult
    relinkResult staged pinnedDeps newStats =
        RelinkResult
            { rresDbName = dbName
            , rresUnresolvedBefore = unresolvedCount (sdLinkingStats staged)
            , rresUnresolvedAfter = unresolvedCount newStats
            , rresCrossDBLinks = length (Loader.cdlLinks newStats)
            , rresDepsLoaded = pinnedDeps
            , rresLinksChanged = not (sameSet (Loader.cdlLinks newStats) (sdCrossDBLinks staged))
            }

{- | Shared relink core. Candidates are the database's full declared pin
('dbDependsOn'); relink recomputes the links within it but never grows or
shrinks the set. @aliases@ feeds 'lcSupplierAliases', a mapping relink passes
the user's curated map (which retargets a chosen dependency without dropping
links to the others), a plain relink passes 'emptyAliasMap'. The dependency
set stored on the database is never mutated here.

@persistedDeps@ is the dependency set as it stands in the matrix cache on disk
('Just' when the caller pinned a new dep in-memory before calling). The cache
is the only durable store of 'dbDependsOn', so a pin that yields zero new links
must still be written; comparing the live pin against @persistedDeps@ surfaces
that divergence. 'Nothing' means the pin is unchanged from disk.
-}
relinkDatabaseWith ::
    DatabaseManager ->
    Text ->
    CrossLinking.AliasMap ->
    Maybe [Text] ->
    IO (Either Text RelinkResult)
relinkDatabaseWith manager dbName aliases persistedDeps = withLogScope dbName $ do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    maybe (relinkStaged manager dbName Nothing aliases) relinkLoaded (M.lookup dbName loadedDbs)
  where
    relinkLoaded :: LoadedDatabase -> IO (Either Text RelinkResult)
    relinkLoaded loaded = do
        indexedDbs <- readTVarIO (dmIndexedDbs manager)
        -- Strict pin: candidates are restricted to the database's declared
        -- dependency set ('dbDependsOn'), never the full set of loaded DBs.
        -- This keeps the user's explicit selection authoritative: relink
        -- recomputes links *within* the whole pin (so a mapping relink against
        -- one dependency re-resolves the others unchanged instead of dropping
        -- them) but never expands or shrinks the set.
        let pinnedDeps = dbDependsOn (ldDatabase loaded)
            otherIndexes = [idb | (n, idb) <- M.toList indexedDbs, n /= dbName, n `elem` pinnedDeps]
        synonymDB <- getMergedSynonymDB manager
        unitConfig <- getMergedUnitConfig manager
        -- Forced here on purpose: the whole link computation must be done
        -- before the transaction below opens, never inside it.
        let !outcome = relinkPlan (planInputs loaded otherIndexes synonymDB unitConfig)
            db' = roDatabase outcome
            loaded' = loaded{ldDatabase = db'}
        atomically $
            publishLoaded manager dbName loaded' (buildIndexedDatabaseFromDB dbName synonymDB db')
        clearMethodMappingCacheForDb manager dbName
        {- Nothing more to do when the relink was a verification no-op: links
        and deps already matched the in-memory state, the cache on disk already
        holds them, and the log line would carry no information. This is the
        common case for a warm load. -}
        when (roCacheChanged outcome) $ do
            Loader.saveCachedDatabaseWithMatrices dbName (dcPath (ldConfig loaded')) db'
            reportProgress Info (relinkSummary (roResult outcome))
        return (Right (roResult outcome))

    planInputs :: LoadedDatabase -> [IndexedDatabase] -> SynonymDB -> UnitConversion.UnitConfig -> RelinkInputs
    planInputs loaded otherIndexes synonymDB unitConfig =
        RelinkInputs
            { riDbName = dbName
            , riDatabase = ldDatabase loaded
            , riContext =
                LinkingContext
                    { lcIndexedDatabases = otherIndexes
                    , lcSynonymDB = synonymDB
                    , lcUnitConfig = unitConfig
                    , lcThreshold = defaultLinkingThreshold
                    , lcLocationHierarchy = dmLocationHierarchy manager
                    , lcGeographyPolicy = dcGeographyPolicy (ldConfig loaded)
                    , lcSupplierAliases = aliases
                    }
            , riPersistedDeps = persistedDeps
            }

    relinkSummary :: RelinkResult -> String
    relinkSummary result =
        "Re-linked "
            <> T.unpack dbName
            <> ": "
            <> show (rresUnresolvedBefore result)
            <> " → "
            <> show (rresUnresolvedAfter result)
            <> " unresolved products ("
            <> show (rresCrossDBLinks result)
            <> " cross-DB links)"

{- | After a DB loads (or reloads), re-link every already-loaded DB that
declares it as a dependency. This makes cross-DB linking converge
automatically regardless of load order.
-}
relinkDependents :: DatabaseManager -> Text -> IO ()
relinkDependents manager newlyLoaded = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    let dependents =
            [ name
            | (name, ld) <- M.toList loadedDbs
            , name /= newlyLoaded
            , newlyLoaded `elem` dbDependsOn (ldDatabase ld)
            ]
    forM_ dependents $ \depName -> do
        result <- relinkDatabase manager depName
        case result of
            Right _ -> return ()
            Left err ->
                reportProgress Warning $
                    "Re-link of " <> T.unpack depName <> " failed: " <> T.unpack err

-- | Auto-load unloaded dependencies via loadDatabaseSingle
autoLoadDeps :: DatabaseManager -> [Text] -> IO [DepLoadResult]
autoLoadDeps manager deps =
    fmap catMaybes $ forM deps $ \depName -> do
        isLoaded <- M.member depName <$> readTVarIO (dmLoadedDbs manager)
        if isLoaded
            then return Nothing
            else do
                reportProgress Info $ "Auto-loading dependency: " <> T.unpack depName
                depResult <- loadDatabaseSingle manager depName
                case depResult of
                    Right _ -> do
                        reportProgress Info $ "  [OK] Auto-loaded: " <> T.unpack depName
                        return (Just (DepLoaded depName))
                    Left err -> do
                        reportProgress Error $ "  [FAIL] " <> T.unpack depName <> ": " <> T.unpack err
                        return (Just (DepLoadFailed depName err))

{- | Load a database on demand with automatic dependency loading
Pre-loads declared dependencies (from TOML config) so cross-DB linking works,
then loads the target database.
-}
loadDatabase :: DatabaseManager -> Text -> IO (Either Text (LoadedDatabase, [DepLoadResult]))
loadDatabase manager dbName = fmap flattenLoad (try (withLogScope dbName go))
  where
    -- A fresh load parses/reads from disk and can throw. Fold any exception
    -- into the Left this function already returns, so every surface (REST,
    -- MCP, CLI) gets one total Either to handle instead of each having to
    -- remember its own catch.
    flattenLoad :: Either SomeException (Either Text a) -> Either Text a
    flattenLoad = either (Left . T.pack . show) id
    go = do
        -- Pre-load declared dependencies so they're available for cross-DB linking
        availableDbs <- readTVarIO (dmAvailableDbs manager)
        let configDeps = maybe [] dcDepends (M.lookup dbName availableDbs)
        depResults1 <- autoLoadDeps manager configDeps

        result <- loadDatabaseSingle manager dbName
        case result of
            Left err -> return (Left err)
            Right loaded -> do
                -- Also auto-load any runtime-discovered dependencies
                depResults2 <- autoLoadDeps manager (dbDependsOn (ldDatabase loaded))
                -- Warm the method-table cache off the request path so the first
                -- score doesn't pay the (regional) build cost on demand.
                warmMethodTables manager dbName (ldDatabase loaded)
                return (Right (loaded, depResults1 ++ depResults2))

{- | Stage an uploaded database (parse + cross-DB link, no matrices yet)
When a valid cache exists, reconstructs staged state from the cached Database
without re-parsing, turning a ~90s operation into ~7s.
-}
stageUploadedDatabase :: DatabaseManager -> DatabaseConfig -> IO (Either Text ())
stageUploadedDatabase manager dbConfig = withLogScope dbName $ runExceptT $ do
    liftIO $ reportProgress Info $ "[STARTING] Staging: " <> T.unpack (dcDisplayName dbConfig)
    -- Try cache first: if valid, reconstruct StagedDatabase without re-parsing
    inputs <- liftIO $ currentBuildInputs manager dbConfig
    mCachedDb <- liftIO $ Loader.loadCachedDatabaseWithMatrices dbName (dcPath dbConfig) inputs
    maybe (parseAndLink inputs) fromCache mCachedDb
  where
    dbName :: Text
    dbName = dcName dbConfig

    fromCache :: Database -> ExceptT Text IO ()
    fromCache cachedDb = liftIO $ do
        -- Cache hit: auto-load dependencies so cross-DB solving works
        _ <- autoLoadDeps manager (dbDependsOn cachedDb)
        stage "  [OK] Staged from cache: " $
            StagedDatabase
                { sdSimpleDB = toSimpleDatabase cachedDb
                , sdConfig = dbConfig
                , sdMissingProducts = []
                , sdSelectedDeps = dbDependsOn cachedDb
                , sdCrossDBLinks = dbCrossDBLinks cachedDb
                , sdLinkingStats = dbLinkingStats cachedDb
                , sdBuiltWith = dbBuiltWith cachedDb
                , sdCachedDB = Just cachedDb
                }

    parseAndLink :: BuildInputs -> ExceptT Text IO ()
    parseAndLink inputs = do
        -- Resolve nested directory structure (e.g. ZIP extracts with multiple subdirs)
        path <- liftIO $ Upload.findDataDirectory (dcPath dbConfig)
        indexedDbs <- liftIO $ readTVarIO (dmIndexedDbs manager)
        {- A CSV export or a workbook names one file; the loader is handed that
        rather than the directory holding it. Either way the loader gets a
        path: a source that holds no file of its own format is left to produce
        the error it produces anyway. -}
        format <- liftIO $ detectDirectoryFormat path
        loadPath <- liftIO $ fromRight path <$> narrowToDataFile format path
        synonymDB <- liftIO $ getMergedSynonymDB manager
        let unitConfig = biUnitConfig inputs
        -- Parse and run cross-DB linking (but don't build matrices)
        (simpleDb, stats) <-
            ExceptT $
                Loader.loadDatabaseWithCrossDBLinking
                    Loader.LoadOptions
                        { Loader.loUnitConfig = unitConfig
                        , Loader.loLocationAliases = dcLocationAliases dbConfig
                        , Loader.loAllocation = dcAllocation dbConfig
                        }
                    (M.elems indexedDbs)
                    synonymDB
                    (dmLocationHierarchy manager)
                    (dcGeographyPolicy dbConfig)
                    loadPath
        let minimalDeps = computeMinimalSelectedDeps (Loader.cdlLinks stats)
        (finalStats, finalDB) <-
            liftIO $ minimalCover indexedDbs synonymDB unitConfig minimalDeps simpleDb stats
        liftIO $
            stage "  [OK] Staged: " $
                StagedDatabase
                    { sdSimpleDB = finalDB
                    , sdConfig = dbConfig
                    , sdMissingProducts = stagedMissingProducts finalDB finalStats
                    , sdSelectedDeps = minimalDeps
                    , sdCrossDBLinks = Loader.cdlLinks finalStats
                    , sdLinkingStats = finalStats
                    , sdBuiltWith = inputs
                    , sdCachedDB = Nothing
                    }

    {- Drop the dependencies whose links are all substitutable by another at
    the same score. If that shrinks the set, linking runs again restricted to
    the chosen ones, so 'sdCrossDBLinks' stays consistent with 'sdSelectedDeps'
    and finalize sees no dangling supplier UUID. -}
    minimalCover ::
        Map Text IndexedDatabase ->
        SynonymDB ->
        UnitConversion.UnitConfig ->
        -- the dependencies the cover kept
        [Text] ->
        SimpleDatabase ->
        Loader.CrossDBLinkingStats ->
        IO (Loader.CrossDBLinkingStats, SimpleDatabase)
    minimalCover indexedDbs synonymDB unitConfig minimalDeps simpleDb stats
        | selectedSet == S.fromList contributingDeps = return (stats, simpleDb)
        | otherwise = do
            reportProgress Info $
                "  Minimal cover: dropping redundant deps "
                    <> show (S.toList (S.fromList contributingDeps `S.difference` selectedSet))
                    <> ", re-linking against "
                    <> show minimalDeps
            (simpleDb', stats') <-
                Loader.fixActivityLinksWithCrossDB
                    [idx | (n, idx) <- M.toList indexedDbs, S.member n selectedSet]
                    synonymDB
                    unitConfig
                    (dmLocationHierarchy manager)
                    (dcGeographyPolicy dbConfig)
                    simpleDb
            return (stats', simpleDb')
      where
        selectedSet :: S.Set Text
        selectedSet = S.fromList minimalDeps

        contributingDeps :: [Text]
        contributingDeps = M.keys (Loader.crossDBBySource stats)

    stage :: String -> StagedDatabase -> IO ()
    stage what staged = do
        atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
        reportProgress Info $ what <> T.unpack (dcDisplayName dbConfig)

{- | Unload a database from memory (keeps config for reloading).
Refuses to unload if any currently-loaded database declares this one as a
dependency: unloading would leave the dependent's cross-DB links dangling.
-}
unloadDatabase :: DatabaseManager -> Text -> IO (Either Text ())
unloadDatabase manager dbName = withLogScope dbName $ do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case unloadRefusal dbName loadedDbs of
        Just refusal -> return (Left refusal)
        Nothing -> do
            -- Remove from loaded databases and IndexedDatabases (for cross-DB linking)
            atomically $ do
                modifyTVar' (dmLoadedDbs manager) (M.delete dbName)
                modifyTVar' (dmIndexedDbs manager) (M.delete dbName)

            -- Clear cached solvers and flow mappings
            clearCachedSolver dbName
            clearMethodMappingCacheForDb manager dbName

            -- Force garbage collection to release memory
            performGC

            reportProgress Info $ "Unloaded database: " <> T.unpack dbName
            return $ Right ()

{- | Why an unload cannot go ahead, when it cannot: the database is not loaded,
or another loaded one still declares it as a dependency and unloading would
leave that one's cross-database links dangling.
-}
unloadRefusal :: Text -> Map Text LoadedDatabase -> Maybe Text
unloadRefusal dbName loadedDbs
    | not (M.member dbName loadedDbs) = Just $ "Database not loaded: " <> dbName
    | not (null dependents) =
        Just $
            "Cannot unload "
                <> dbName
                <> ": still required by "
                <> T.intercalate ", " dependents
                <> ". Unload dependents first."
    | otherwise = Nothing
  where
    dependents :: [Text]
    dependents =
        [ name
        | (name, ld) <- M.toList loadedDbs
        , name /= dbName
        , dbName `elem` dbDependsOn (ldDatabase ld)
        ]

-- | Add a new database config to the manager (without loading)
addDatabase :: DatabaseManager -> DatabaseConfig -> IO ()
addDatabase manager dbConfig = do
    atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert (dcName dbConfig) dbConfig)
    reportProgress Info $ "Added database config: " <> T.unpack (dcDisplayName dbConfig)

{- | Remove a database from the manager
Fails if database is loaded
-}
removeDatabase :: DatabaseManager -> Text -> IO (Either Text ())
removeDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    case M.lookup dbName availableDbs of
        Nothing -> return $ Left $ "Database not found: " <> dbName
        Just dbConfig
            -- Honor the per-config deletable policy (defaults to dcIsUploaded).
            | not (dcDeletable dbConfig) ->
                return $ Left "Cannot delete configured database. Edit volca.toml to remove it."
            | M.member dbName loadedDbs ->
                return $ Left "Cannot delete loaded database. Close it first."
            | otherwise -> do
                -- A copy owns no data: it reads this database's files and keeps
                -- only its own edits, so taking the files would leave it unable
                -- to load at all.
                copies <- copiesOf dbName
                case copies of
                    [] -> deleteUpload dbConfig
                    names ->
                        return $
                            Left $
                                "Cannot delete "
                                    <> dbName
                                    <> ": "
                                    <> T.intercalate ", " names
                                    <> " were copied from it and read its files. Delete them first."
  where
    tryIO :: IO a -> IO (Either SomeException a)
    tryIO = Control.Exception.try
    -- The databases copied from this one, which still read its files.
    copiesOf :: Text -> IO [Text]
    copiesOf name = do
        uploads <- UploadedDB.discoverUploadedDatabases
        pure [slug | (slug, _, meta) <- uploads, UploadedDB.umSource meta == Just name]
    deleteUpload :: DatabaseConfig -> IO (Either Text ())
    deleteUpload dbConfig = do
        uploadsDir <- UploadedDB.getDatabaseUploadsDir
        let uploadDir = uploadsDir </> T.unpack dbName
        pathExists <- doesDirectoryExist uploadDir
        if pathExists
            then
                tryIO (removeDirectoryRecursive uploadDir) >>= \case
                    Left (e :: SomeException) -> return $ Left $ "Failed to delete: " <> T.pack (show e)
                    Right () -> do
                        reportProgress Info $ "Deleted: " <> uploadDir
                        deleteCacheFile dbName (dcPath dbConfig)
                        removeFromMemory manager dbName
            else do
                -- Directory already missing, just remove from memory
                reportProgress Info $ "Directory already missing: " <> uploadDir
                removeFromMemory manager dbName
    deleteCacheFile :: Text -> FilePath -> IO ()
    deleteCacheFile name sourcePath = do
        cacheFile <- Loader.generateMatrixCacheFilename name sourcePath
        let zstdFile = cacheFile ++ ".zst"
        cacheExists <- doesFileExist zstdFile
        when cacheExists $ do
            removeFile zstdFile
            reportProgress Info $ "Deleted cache: " ++ zstdFile

{- | The solver for a database, over the technosphere triples it holds.
Factorization is lazy, so this costs nothing until the first query.
-}
solverFor :: Text -> Database -> IO SharedSolver
solverFor dbName db =
    createSharedSolver
        dbName
        [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
        (fromIntegral (dbActivityCount db))

{- | Install a loaded database and the index built from it under one name.

The two maps move together or not at all: a reader that finds the database
without its index gets one that cross-database linking cannot see into, and
nothing would report the gap.
-}
publishLoaded :: DatabaseManager -> Text -> LoadedDatabase -> IndexedDatabase -> STM ()
publishLoaded manager dbName loaded indexedDb = do
    modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
    modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)

-- | Helper to remove database from in-memory maps only
removeFromMemory :: DatabaseManager -> Text -> IO (Either Text ())
removeFromMemory manager dbName = do
    atomically $ do
        modifyTVar' (dmAvailableDbs manager) (M.delete dbName)
        modifyTVar' (dmStagedDbs manager) (M.delete dbName)
        modifyTVar' (dmStagingDbs manager) (S.delete dbName)
    reportProgress Info $ "Removed database: " <> T.unpack dbName
    return $ Right ()

--------------------------------------------------------------------------------
-- Staged Database Operations
--------------------------------------------------------------------------------

-- | Get a staged database by name
getStagedDatabase :: DatabaseManager -> Text -> IO (Maybe StagedDatabase)
getStagedDatabase manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    return $ M.lookup dbName stagedDbs

{- | Supplier-gap report for a loaded or staged database: what is still
missing to fully supply its demands from the pinned dependencies, aggregated
per (product, location, unit) with the consumers that demand it.
-}
databaseGapReport :: DatabaseManager -> Text -> IO (Either Text Loader.GapReport)
databaseGapReport manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    pure $ case (M.lookup dbName loadedDbs, M.lookup dbName stagedDbs) of
        (Just loaded, _) -> Right (Loader.gapReportForLoaded dbName (ldDatabase loaded))
        (Nothing, Just staged) ->
            Right (Loader.gapReportForStaged dbName (sdSimpleDB staged) (sdLinkingStats staged))
        (Nothing, Nothing) -> Left ("Database not loaded: " <> dbName)

{- | Dataset-soundness report for a loaded or staged database: the structural
defects a score can't reveal. Both phases reduce to the same pure scan, so a
maker gets the same answer before and after building the matrices.
-}
databaseQualityReport :: DatabaseManager -> Text -> IO (Either Text Quality.QualityReport)
databaseQualityReport manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    pure $ case (M.lookup dbName loadedDbs, M.lookup dbName stagedDbs) of
        (Just loaded, _) -> Right (Quality.qualityReport dbName (toSimpleDatabase (ldDatabase loaded)))
        (Nothing, Just staged) -> Right (Quality.qualityReport dbName (sdSimpleDB staged))
        (Nothing, Nothing) -> Left ("Database not loaded: " <> dbName)

{- | Characterization-coverage report: the database flows a method collection
scores only through a name bridge (synonym/CAS), which an exact-name consumer
would score as zero. One entry per loaded collection when @mCollection@ is
'Nothing'; a single named collection otherwise (an error if it isn't loaded).

Needs a built database (the coverage probe reads the method tables), so unlike
the quality report it is loaded-only: no staged answer. Computed per request
on top of the per-method table and mapping caches; if it proves slow at
ecoinvent scale, the upgrade path is a @(db, collection)@-keyed cache beside
'mapMethodToTablesCached'.
-}

{- | Replay the CF cascade for one flow under one method, so a caller can say
why that flow scores with the factor it does.

The flow is looked up in the merged metadata, not the one database, because
that is what scoring reads: a flow arriving from a dependency is characterized
by the same tables and must be explainable by them too.
-}
explainFlowFactor ::
    DatabaseManager ->
    Text ->
    CollectionName ->
    Database ->
    Method ->
    UUID ->
    IO (Either Text (BiosphereFlow, Explain.CFExplanation))
explainFlowFactor manager dbName collection db method fid = do
    (mFlows, mUnits) <- getMergedFlowMetadata manager
    unitCfg <- getMergedUnitConfig manager
    tables <- mapMethodToTablesCached manager dbName collection db method
    pure $ case M.lookup fid mFlows of
        Nothing -> Left ("No such flow in " <> dbName <> ": " <> T.pack (show fid))
        Just flow -> Right (flow, Explain.explainFlowCF unitCfg mUnits tables fid flow)

databaseCoverageReport :: DatabaseManager -> Text -> Maybe Text -> IO (Either Text Coverage.CoverageReport)
databaseCoverageReport manager dbName mCollection = do
    mLoaded <- getDatabase manager dbName
    loadedMethods <- readTVarIO (dmLoadedMethods manager)
    case mLoaded of
        Nothing -> pure (Left ("Database not loaded: " <> dbName))
        Just loaded -> do
            let db = ldDatabase loaded
            case collectionsToReport mCollection loadedMethods of
                Left err -> pure (Left err)
                Right cols -> do
                    bridges <- mapM (collectionBridgesFor db) cols
                    pure (Right (Coverage.CoverageReport dbName bridges))
  where
    -- The named collection (must be loaded) or every loaded one, by name.
    collectionsToReport :: Maybe Text -> Map Text MethodCollection -> Either Text [(Text, MethodCollection)]
    collectionsToReport sel loaded = case sel of
        Just name -> case M.lookup name loaded of
            Just mc -> Right [(name, mc)]
            Nothing -> Left ("Method collection not loaded: " <> name)
        Nothing -> Right (M.toList loaded)
    collectionBridgesFor :: Database -> (Text, MethodCollection) -> IO Coverage.CollectionBridges
    collectionBridgesFor db (collName, mc) = do
        let methods = mcMethods mc
        tables <- mapM (mapMethodToTablesCached manager dbName (CollectionName collName) db) methods
        mappings <- mapM (effectiveMethodMappings manager dbName (CollectionName collName) db) methods
        let characterized = S.size (S.unions (map (`characterizedFlowIds` dbBioFlows db) tables))
            total = fromIntegral (dbBiosphereCount db)
        pure (Coverage.collectionBridges collName total characterized mappings)

{- | Outcome of the atomic staging decision; 'NeedToStage' carries the config
read inside the same transaction, so no later (racy) re-lookup is needed.
-}
data StageAction = AlreadyDone | NeedToStage DatabaseConfig

{- | Get setup info for a database (for the setup page)
Works for both staged and loaded databases
Auto-stages uploaded databases if they're not yet staged
Uses STM to prevent concurrent staging of the same database
-}
getDatabaseSetupInfo :: DatabaseManager -> Text -> IO (Either SetupError DatabaseSetupInfo)
getDatabaseSetupInfo manager dbName = do
    action <- atomically decide
    case action of
        Left err -> return $ Left err
        Right AlreadyDone -> buildSetupResult manager dbName
        Right (NeedToStage dbConfig) -> do
            -- Do the slow work, ensuring we always unmark on exception
            stageResult <-
                Control.Exception.finally
                    (stageUploadedDatabase manager dbConfig)
                    (atomically $ modifyTVar' (dmStagingDbs manager) (S.delete dbName))
            case stageResult of
                Left err -> do
                    reportProgress Error $ "Setup staging failed for " <> T.unpack dbName <> ": " <> T.unpack err
                    return $ Left $ SetupFailed err
                Right () -> buildSetupResult manager dbName
  where
    {- Already staged? already staging? need to stage? One transaction, so the
    answer cannot go stale between the reads, and the name is reserved in the
    same breath as the decision to stage it. -}
    decide :: STM (Either SetupError StageAction)
    decide = do
        stagedDbs <- readTVar (dmStagedDbs manager)
        loadedDbs <- readTVar (dmLoadedDbs manager)
        if M.member dbName stagedDbs || M.member dbName loadedDbs
            then pure (Right AlreadyDone)
            else do
                stagingDbs <- readTVar (dmStagingDbs manager)
                -- Another thread holds the name: block until it is done, then
                -- read again and find the database staged.
                when (S.member dbName stagingDbs) retry
                availableDbs <- readTVar (dmAvailableDbs manager)
                maybe notFound reserve (M.lookup dbName availableDbs)

    notFound :: STM (Either SetupError StageAction)
    notFound = pure (Left (SetupNotFound ("Database not found: " <> dbName)))

    -- Only an upload is staged on demand; a configured database is loaded or
    -- it is nothing.
    reserve :: DatabaseConfig -> STM (Either SetupError StageAction)
    reserve dbConfig
        | not (dcIsUploaded dbConfig) = pure (Left (SetupNotLoaded dbName))
        | otherwise = do
            modifyTVar' (dmStagingDbs manager) (S.insert dbName)
            pure (Right (NeedToStage dbConfig))

-- | Read current state and build setup info for a database
buildSetupResult :: DatabaseManager -> Text -> IO (Either SetupError DatabaseSetupInfo)
buildSetupResult manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    indexedDbs <- readTVarIO (dmIndexedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged -> do
            let info = buildStagedSetupInfo staged availableDbs indexedDbs
            -- Populate available paths for uploaded databases
            if dcIsUploaded (sdConfig staged)
                then do
                    candidates <- discoverCandidatePaths (sdConfig staged)
                    return $ Right info{dsiAvailablePaths = candidates}
                else return $ Right info
        Nothing -> case M.lookup dbName loadedDbs of
            Just loaded ->
                return $ Right $ buildLoadedSetupInfo (ldConfig loaded) (ldDatabase loaded) availableDbs indexedDbs
            Nothing -> return $ Left $ SetupFailed $ "Failed to stage database: " <> dbName

{- | Link-resolution tally shared by the setup page and the finalize gate.
Both derive readiness from this one record, so "ready" and "can be finalized"
can never drift apart.
-}
data LinkCounts = LinkCounts
    { lcActivityCount :: !Int
    , lcTotalInputs :: !Int
    , lcUnlinked :: !Int
    , lcCrossDBLinks :: !Int
    }

-- | Inputs resolved inside the database itself.
lcInternalLinks :: LinkCounts -> Int
lcInternalLinks lc = max 0 (lcTotalInputs lc - lcUnlinked lc)

-- | Inputs no link, internal or cross-DB, resolves.
lcUnresolvedLinks :: LinkCounts -> Int
lcUnresolvedLinks lc = max 0 (lcUnlinked lc - lcCrossDBLinks lc)

-- | Tally for a staged database, from its parsed activities and linking stats.
stagedLinkCounts :: StagedDatabase -> LinkCounts
stagedLinkCounts staged =
    LinkCounts
        { lcActivityCount = M.size (sdbActivities sdb)
        , lcTotalInputs = Loader.countTotalTechInputs sdb
        , lcUnlinked = Loader.countUnlinkedExchanges sdb
        , lcCrossDBLinks = Loader.crossDBLinksCount (sdLinkingStats staged)
        }
  where
    sdb :: SimpleDatabase
    sdb = sdSimpleDB staged

{- | Tally for a loaded database. Counts are recomputed from the activity set
via the same predicate as the staged path ('Loader.countUnlinkedExchanges'),
not read from 'dbLinkingStats': the stats only track nil-link / cross-DB
resolution and are blind to dangling internal links (non-nil 'activityLinkId'
pointing at an activity the database doesn't ship). A database bulk-loaded
from config bypasses the finalize gate, so without this it would report a
partial EcoSpold2 import as 100% ready while the matrix silently drops those
inputs.
-}
loadedLinkCounts :: Database -> LinkCounts
loadedLinkCounts db =
    LinkCounts
        { lcActivityCount = fromIntegral (dbActivityCount db)
        , lcTotalInputs = Loader.countTotalTechInputs sdb
        , lcUnlinked = Loader.countUnlinkedExchanges sdb
        , lcCrossDBLinks = length (dbCrossDBLinks db)
        }
  where
    sdb = toSimpleDatabase db

{- | Percentage of resolved inputs (0-100); an inputless database is complete.
Clamped: stats recording more cross-DB links than unlinked inputs must not
report above 100%.
-}
lcCompleteness :: LinkCounts -> Double
lcCompleteness lc
    | lcTotalInputs lc > 0 =
        min 100.0 $ 100.0 * fromIntegral (lcInternalLinks lc + lcCrossDBLinks lc) / fromIntegral (lcTotalInputs lc)
    | otherwise = 100.0

{- | Why a database cannot be finalized: 'Nothing' means ready. The setup
page's 'dsiIsReady' is the 'isNothing' of this, so the ready badge and the
finalize gate always agree.
-}
notReadyReason :: LinkCounts -> Maybe Text
notReadyReason lc
    | lcActivityCount lc == 0 =
        Just "database contains 0 activities. The data file may be corrupted or in an unsupported format."
    | lcUnresolvedLinks lc > 0 =
        Just $
            T.pack (show (lcUnresolvedLinks lc))
                <> " unresolved inputs. Add dependencies to resolve them first."
    | otherwise = Nothing

{- | Rank missing products by demanding-input count, descending. Nil-link gaps
carry the rich blockers the attribute matcher produced; dangling non-nil gaps
are tagged 'NoNameMatch'. The two sets are disjoint (nil vs non-nil), so the
concatenation never duplicates.
-}
rankMissingProducts :: Map Text UnresolvedProduct -> Map Text Int -> [(Text, UnresolvedProduct)]
rankMissingProducts blocked dangling =
    sortOn
        (Down . upDemands . snd)
        ( M.toList blocked
            <> [(name, UnresolvedProduct (M.singleton NoNameMatch cnt)) | (name, cnt) <- M.toList dangling]
        )

{- | Project one ranked missing product onto its wire shape: one row per reason
it was refused for, biggest first, so a product blocked two ways is read as two
and never as one of them carrying the other's demands.
-}
missingSuppliersOf :: (Text, UnresolvedProduct) -> [MissingSupplier]
missingSuppliersOf (name, unresolved) =
    [ MissingSupplier name n Nothing (brReason reason) (brDetail reason)
    | (blocker, n) <- sortOn (Down . snd) (M.toList (upBlockers unresolved))
    , let reason = blockerReason blocker
    ]

{- | Missing-supplier list for a staged database: rich blockers from the
linking stats plus dangling background links a partial import leaves behind
('Loader.collectStagedDanglingProductNames'), ranked by demand.
-}
stagedMissingProducts :: SimpleDatabase -> CrossDBLinkingStats -> [(Text, UnresolvedProduct)]
stagedMissingProducts sdb stats =
    rankMissingProducts
        (cdlUnresolvedProducts stats)
        (Loader.collectStagedDanglingProductNames sdb (cdlLinks stats))

{- | Assemble the wire record from the shared tally, the single place the
completeness, readiness, and linking-stats fields are filled, for both the
staged and the loaded builder. availablePaths is filled in by
'buildSetupResult' for uploaded databases (requires IO).
-}

-- | Which of the two builders below is asking.
data SetupOrigin = FromStaged | FromLoaded

-- | The tally one setup record is assembled from.
data SetupSource = SetupSource
    { ssConfig :: !DatabaseConfig
    , ssCounts :: !LinkCounts
    , ssStats :: !CrossDBLinkingStats
    , ssMissing :: ![(Text, UnresolvedProduct)]
    , ssDependencies :: ![DependencyChoice]
    , ssOrigin :: !SetupOrigin
    }

setupInfoFrom :: SetupSource -> DatabaseSetupInfo
setupInfoFrom SetupSource{..} =
    DatabaseSetupInfo
        { dsiName = dcName ssConfig
        , dsiDisplayName = dcDisplayName ssConfig
        , dsiActivityCount = lcActivityCount ssCounts
        , dsiInputCount = lcTotalInputs ssCounts
        , dsiCompleteness = lcCompleteness ssCounts
        , dsiInternalLinks = lcInternalLinks ssCounts
        , dsiCrossDBLinks = lcCrossDBLinks ssCounts
        , dsiUnresolvedLinks = lcUnresolvedLinks ssCounts
        , -- The ten worst products, every reason each was refused for: capping
          -- the rows instead would drop the later reasons of the last product,
          -- which is the collapse this list exists to avoid.
          dsiMissingSuppliers = concatMap missingSuppliersOf (take 10 ssMissing)
        , dsiDependencies = ssDependencies
        , dsiIsReady = isNothing (notReadyReason ssCounts)
        , dsiUnknownUnits = S.toList (cdlUnknownUnits ssStats)
        , dsiLocationFallbacks = deduplicateFallbacks (cdlLocationFallbacks ssStats)
        , dsiLocationUnresolved = deduplicateUnresolved (cdlLocationUnresolved ssStats)
        , dsiAttributeFallbacks = deduplicateAttributeFallbacks (cdlAttributeFallbacks ssStats)
        , dsiSupplierAmbiguities = deduplicateSupplierAmbiguities (cdlSupplierAmbiguities ssStats)
        , dsiDataPath = T.pack (dcPath ssConfig)
        , dsiAvailablePaths = []
        , dsiIsLoaded = case ssOrigin of
            FromStaged -> False
            FromLoaded -> True
        }

{- | The four fields a relink writes back onto a staged database. One place,
because the staged relink and the two dependency mutators all write exactly
these and would otherwise drift apart.
-}
withRelinkedDeps :: [Text] -> CrossDBLinkingStats -> StagedDatabase -> StagedDatabase
withRelinkedDeps newDeps newStats staged =
    staged
        { sdSelectedDeps = newDeps
        , sdCrossDBLinks = Loader.cdlLinks newStats
        , sdLinkingStats = newStats
        , sdMissingProducts = stagedMissingProducts (sdSimpleDB staged) newStats
        }

-- | Build setup info from a staged database
buildStagedSetupInfo :: StagedDatabase -> Map Text DatabaseConfig -> Map Text IndexedDatabase -> DatabaseSetupInfo
buildStagedSetupInfo staged configs indexedDbs =
    let stats = sdLinkingStats staged
     in setupInfoFrom
            SetupSource
                { ssConfig = sdConfig staged
                , ssCounts = stagedLinkCounts staged
                , ssStats = stats
                , ssMissing = sdMissingProducts staged
                , ssDependencies =
                    buildDependencyChoices
                        (dcName (sdConfig staged))
                        (sdSelectedDeps staged)
                        (crossDBRedundantSources (cdlLinks stats) (sdSelectedDeps staged))
                        configs
                        indexedDbs
                , ssOrigin = FromStaged
                }

{- | Build setup info from a loaded database (already finalized). Counts come
from 'loadedLinkCounts' (see its note on recomputing rather than trusting
'dbLinkingStats'); rich blocker reasons still come from the stats, dangling
links are ranked in with them.
-}
buildLoadedSetupInfo :: DatabaseConfig -> Database -> Map Text DatabaseConfig -> Map Text IndexedDatabase -> DatabaseSetupInfo
buildLoadedSetupInfo config db configs indexedDbs =
    setupInfoFrom
        SetupSource
            { ssConfig = config
            , ssCounts = loadedLinkCounts db
            , ssStats = dbLinkingStats db
            , ssMissing =
                rankMissingProducts
                    (cdlUnresolvedProducts (dbLinkingStats db))
                    (Loader.collectDanglingProductNames db)
            , ssDependencies = buildDependencyChoices (dcName config) (dbDependsOn db) [] configs indexedDbs
            , ssOrigin = FromLoaded
            }

{- | Discover candidate data paths within an uploaded database's root directory.
Returns one 'PathCandidate' per candidate directory.
-}
discoverCandidatePaths :: DatabaseConfig -> IO [PathCandidate]
discoverCandidatePaths dbConfig = do
    uploadsDir <- UploadedDB.getDatabaseUploadsDir
    let uploadRoot = uploadsDir </> T.unpack (dcName dbConfig)
    candidates <- Upload.findAllDataDirectories uploadRoot
    forM candidates $ \dir -> do
        format <- Upload.detectDatabaseFormat dir
        count <- Upload.countDataFilesIn dir
        let rel = makeRelativePath uploadRoot dir
            label = case format of
                Upload.EcoSpold2 -> "EcoSpold 2"
                Upload.EcoSpold1 -> "EcoSpold 1"
                Upload.SimaProCSV -> "SimaPro CSV"
                Upload.ILCDProcess -> "ILCD"
                Upload.OpenLcaJsonLd -> "openLCA JSON-LD"
                Upload.BrightwayExcel -> "Brightway Excel"
                Upload.UnknownFormat -> "Unknown"
        return PathCandidate{pcPath = T.pack rel, pcFormat = label, pcFileCount = count}
  where
    -- Simple relative path: strip upload root prefix
    makeRelativePath :: FilePath -> FilePath -> FilePath
    makeRelativePath base path
        | base `isPrefixOf` path =
            let r = drop (length base + 1) path
             in if null r then "." else r
        | otherwise = path

{- | Where an upload's data sits under its own directory. Its own type
because the only other argument it travels with is a database name, and
'Text' cannot tell a caller which way round they go.
-}
newtype RelativeDataPath = RelativeDataPath {unRelativeDataPath :: Text}

{- | Change the data path for an uploaded (staged) database.
Validates path, updates config + meta.toml, clears staged DB to force re-stage.
-}
setDataPath :: DatabaseManager -> Text -> RelativeDataPath -> IO (Either Text DatabaseSetupInfo)
setDataPath manager dbName (RelativeDataPath newRelPath) = runExceptT $ do
    availableDbs <- liftIO $ readTVarIO (dmAvailableDbs manager)
    dbConfig <-
        except $
            maybe (Left $ "Database not found: " <> dbName) Right (M.lookup dbName availableDbs)
    unless (dcIsUploaded dbConfig) $ throwE "Cannot change data path for configured databases"

    uploadsDir <- liftIO UploadedDB.getDatabaseUploadsDir
    let uploadRoot = uploadsDir </> T.unpack dbName
        newFullPath = uploadRoot </> T.unpack newRelPath

    hasData <- liftIO $ Upload.anyDataFilesIn newFullPath
    unless hasData $ throwE $ "No data files found in: " <> newRelPath

    {- meta.toml carries the same two fields, and it is the only durable copy:
    the in-memory config is rebuilt from it at every start. Read it before
    changing anything, so a missing one refuses the whole call rather than
    leaving a new path that lasts until the next restart. An upload always has
    one, since that is what 'discoverUploadedDatabases' recognises it by. -}
    meta <-
        liftIO (UploadedDB.readUploadMeta uploadRoot)
            >>= maybe (throwE (noMetaMessage uploadRoot)) pure

    newFormat <- liftIO $ Upload.detectDatabaseFormat newFullPath
    liftIO $ do
        UploadedDB.writeUploadMeta
            uploadRoot
            meta
                { UploadedDB.umDataPath = T.unpack newRelPath
                , UploadedDB.umFormat = newFormat
                }
        atomically $ do
            modifyTVar'
                (dmAvailableDbs manager)
                (M.insert dbName dbConfig{dcPath = newFullPath, dcFormat = Just newFormat})
            -- Clear staged DB to force re-staging with new path
            modifyTVar' (dmStagedDbs manager) (M.delete dbName)

    -- Re-stage and return fresh setup info
    withExceptT setupErrorMessage $ ExceptT (getDatabaseSetupInfo manager dbName)

{- | Why a data path cannot be changed when the upload has lost its meta.toml.
'UploadedDB.readUploadMeta' answers the same for a file that is absent, one
that cannot be read and one that does not parse, so the message claims no more
than it knows.
-}
noMetaMessage :: FilePath -> Text
noMetaMessage uploadRoot =
    "No readable meta.toml under "
        <> T.pack uploadRoot
        <> "; the data path would live in memory only and be lost at restart"

{- | Build the combined list of dependency choices.
Excludes the current database, tags each remaining DB as selected,
redundant, or available, and sorts the result alphabetically.
Selected takes precedence over redundant if a name appears in both sets.
-}
buildDependencyChoices ::
    -- | Current database name (excluded from the result)
    Text ->
    -- | Names currently selected as dependencies
    [Text] ->
    -- | Names that match links but are redundant under the minimal cover
    [Text] ->
    Map Text DatabaseConfig ->
    Map Text IndexedDatabase ->
    [DependencyChoice]
buildDependencyChoices currentName selected redundant configs indexedDbs =
    let selectedSet = S.fromList selected
        redundantSet = S.fromList redundant
        statusOf name
            | S.member name selectedSet = SelectedDep
            | S.member name redundantSet = RedundantDep
            | otherwise = AvailableDep
        mkChoice (name, idx) =
            DependencyChoice
                { dchStatus = statusOf name
                , dchDatabaseName = name
                , dchDisplayName = maybe name dcDisplayName (M.lookup name configs)
                , dchMatchCount = M.size (Database.CrossLinking.idbByProductName idx)
                }
     in sortOn
            dchDatabaseName
            [ mkChoice (name, idx)
            | (name, idx) <- M.toList indexedDbs
            , name /= currentName
            ]

{- | Re-stage a loaded database for dependency editing
Moves from dmLoadedDbs → dmStagedDbs, cleans up solver
-}
restageLoadedDatabase :: DatabaseManager -> Text -> LoadedDatabase -> IO StagedDatabase
restageLoadedDatabase manager dbName ld = do
    let db = ldDatabase ld
        stats = dbLinkingStats db
        staged =
            StagedDatabase
                { sdSimpleDB = toSimpleDatabase db
                , sdConfig = ldConfig ld
                , sdMissingProducts = stagedMissingProducts (toSimpleDatabase db) stats
                , sdSelectedDeps = dbDependsOn db
                , sdCrossDBLinks = dbCrossDBLinks db
                , sdLinkingStats = stats
                , sdBuiltWith = dbBuiltWith db
                , sdCachedDB = Nothing
                }
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.delete dbName)
        modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
    clearCachedSolver dbName
    clearMethodMappingCacheForDb manager dbName
    return staged

-- | Get or create staged database (re-stages loaded DBs on the fly)
getOrStageDatabase :: DatabaseManager -> Text -> IO (Either Text StagedDatabase)
getOrStageDatabase manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged -> return $ Right staged
        Nothing -> do
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            case M.lookup dbName loadedDbs of
                Just ld -> Right <$> restageLoadedDatabase manager dbName ld
                Nothing -> return $ Left $ "Database not found: " <> dbName

{- | Which database's dependency set is being changed, and which name is
being added to or removed from it. Both are database names, so no type can
tell them apart: only the field names can.
-}
data DependencyEdit = DependencyEdit
    { deDatabase :: !Text
    , deDependency :: !Text
    }

{- | Add a dependency to a staged (or partially-linked loaded) database
Runs cross-DB linking against the new dependency
-}
addDependencyToStaged :: DatabaseManager -> DependencyEdit -> IO (Either Text DatabaseSetupInfo)
addDependencyToStaged manager DependencyEdit{deDatabase = dbName, deDependency = depName} = do
    stagedResult <- getOrStageDatabase manager dbName
    {- Read after staging, as the removal path does. Staging is the slow step
    here, and a snapshot taken before it judges whether the dependency is
    loaded on state that may be older than the answer. -}
    indexedDbs <- readTVarIO (dmIndexedDbs manager)
    case stagedResult of
        Left err -> return $ Left err
        Right staged
            | not (M.member depName indexedDbs) ->
                return $ Left $ "Dependency database not loaded: " <> depName
            | otherwise ->
                applyStagedDeps
                    manager
                    dbName
                    StagedRelink
                        { srStaged = staged
                        , srIndexedDbs = indexedDbs
                        , srNewDeps =
                            if depName `elem` sdSelectedDeps staged
                                then sdSelectedDeps staged
                                else depName : sdSelectedDeps staged
                        }

-- | Remove a dependency from a staged (or partially-linked loaded) database
removeDependencyFromStaged :: DatabaseManager -> DependencyEdit -> IO (Either Text DatabaseSetupInfo)
removeDependencyFromStaged manager DependencyEdit{deDatabase = dbName, deDependency = depName} = do
    stagedResult <- getOrStageDatabase manager dbName

    case stagedResult of
        Left err -> return $ Left err
        Right staged -> do
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            applyStagedDeps
                manager
                dbName
                StagedRelink
                    { srStaged = staged
                    , srIndexedDbs = indexedDbs
                    , srNewDeps = filter (/= depName) (sdSelectedDeps staged)
                    }

{- | A staged database, the dependency pin it should now carry, and the indexes
available to link within it.
-}
data StagedRelink = StagedRelink
    { srStaged :: !StagedDatabase
    , srIndexedDbs :: !(Map Text IndexedDatabase)
    , srNewDeps :: ![Text]
    }

{- | Relink a staged database within a new dependency pin, write both back, and
report the setup as it now stands. The two mutators above differ only in how
they compute the pin.
-}
applyStagedDeps :: DatabaseManager -> Text -> StagedRelink -> IO (Either Text DatabaseSetupInfo)
applyStagedDeps manager dbName StagedRelink{..} = do
    synonymDB <- getMergedSynonymDB manager
    unitConfig <- getMergedUnitConfig manager
    (_, newStats) <-
        Loader.fixActivityLinksWithCrossDB
            [idx | (name, idx) <- M.toList srIndexedDbs, name `elem` srNewDeps]
            synonymDB
            unitConfig
            (dmLocationHierarchy manager)
            (dcGeographyPolicy (sdConfig srStaged))
            (sdSimpleDB srStaged)
    atomically $
        modifyTVar'
            (dmStagedDbs manager)
            (M.insert dbName (withRelinkedDeps srNewDeps newStats srStaged))
    persistUploadDepends manager dbName srNewDeps
    first setupErrorMessage <$> getDatabaseSetupInfo manager dbName

{- | A database ready to be made live, and whether this finalize introduced
something the matrix cache on disk does not hold yet.
-}
data FinalizedBuild = FinalizedBuild
    { fbDatabase :: !Database
    , fbNeedsSave :: !Bool
    }

-- | Finalize a staged database (build matrices and make it ready for queries)
finalizeDatabase :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
finalizeDatabase manager dbName = withLogScope dbName $ do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Nothing -> finalizeLoaded
        Just staged -> runExceptT (finalizeStaged staged)
  where
    {- Not staged: an already-loaded database finalizes as a no-op, but only
    through the same readiness gate the setup page reports. A partial import
    bulk-loaded from config must not get a success where the setup says not
    ready. -}
    finalizeLoaded :: IO (Either Text LoadedDatabase)
    finalizeLoaded = do
        loadedDbs <- readTVarIO (dmLoadedDbs manager)
        pure $ case M.lookup dbName loadedDbs of
            Nothing -> Left $ "Staged database not found: " <> dbName
            Just loaded -> case notReadyReason (loadedLinkCounts (ldDatabase loaded)) of
                Just reason -> Left ("Cannot finalize: " <> reason)
                Nothing -> Right loaded

    finalizeStaged :: StagedDatabase -> ExceptT Text IO LoadedDatabase
    finalizeStaged staged = do
        maybe (pure ()) (throwE . ("Cannot finalize: " <>)) (notReadyReason (stagedLinkCounts staged))
        liftIO $ reportProgress Info $ "[STARTING] Finalizing database: " <> T.unpack dbName
        synonymDB <- liftIO (getMergedSynonymDB manager)
        build <- buildFinal staged synonymDB
        liftIO (publish staged synonymDB build)

    -- Use the pre-built database from the cache, or build the matrices.
    buildFinal :: StagedDatabase -> SynonymDB -> ExceptT Text IO FinalizedBuild
    buildFinal staged synonymDB = case sdCachedDB staged of
        {- The on-disk cache is cachedDb. Carrying the (possibly edited) staged
        pin onto the loaded database is what makes the pin authoritative, and a
        re-save is due when either half of it diverges from what is on disk. -}
        Just cachedDb ->
            pure
                FinalizedBuild
                    { fbDatabase = readyToServe synonymDB (withStagedPin staged cachedDb)
                    , fbNeedsSave =
                        not (sameSet (dbDependsOn cachedDb) (sdSelectedDeps staged))
                            || not (sameSet (dbCrossDBLinks cachedDb) (sdCrossDBLinks staged))
                    }
        Nothing -> do
            db <-
                ExceptT $
                    buildDatabaseWithMatrices
                        (sdBuiltWith staged)
                        (sdSimpleDB staged)
            -- Freshly built matrices: always persist.
            pure
                FinalizedBuild
                    { fbDatabase = readyToServe synonymDB (withStagedPin staged db)
                    , fbNeedsSave = True
                    }

    -- The staged pin and the links recomputed under it win over whatever the
    -- built or cached database carries.
    withStagedPin :: StagedDatabase -> Database -> Database
    withStagedPin staged db =
        db
            { dbCrossDBLinks = sdCrossDBLinks staged
            , dbDependsOn = sdSelectedDeps staged
            , dbLinkingStats = sdLinkingStats staged
            }

    readyToServe :: SynonymDB -> Database -> Database
    readyToServe synonymDB db = BM25.addBM25Index (initializeRuntimeFields db synonymDB)

    publish :: StagedDatabase -> SynonymDB -> FinalizedBuild -> IO LoadedDatabase
    publish staged synonymDB FinalizedBuild{..} = do
        sharedSolver <- solverFor dbName fbDatabase
        let loaded =
                LoadedDatabase
                    { ldDatabase = fbDatabase
                    , ldSharedSolver = sharedSolver
                    , ldConfig = sdConfig staged
                    }
            indexedDb = buildIndexedDatabaseFromDB dbName synonymDB fbDatabase
        -- Move from staged to loaded
        atomically $ do
            modifyTVar' (dmStagedDbs manager) (M.delete dbName)
            publishLoaded manager dbName loaded indexedDb
        clearMethodMappingCacheForDb manager dbName
        -- Finalizing is the moment the dependency pin becomes the database's
        -- own; record it where a restart reads.
        persistUploadDepends manager dbName (sdSelectedDeps staged)

        {- Self-relink first against the current dep set: a cached or staged
        build can carry cross-DB links that do not match the deps now in
        'dmIndexedDbs'. 'relinkDatabase' rewrites both the in-memory state and
        (when the links changed) the matrix cache. -}
        linksChangedAfter <-
            relinkDatabase manager dbName >>= either relinkFailed (pure . rresLinksChanged)

        {- Persist when this finalize introduced a change (fresh build, or an
        edited pin on a cache hit) that the relink did not already write: relink
        owns the save whenever it actually changed the in-memory links. -}
        when (fbNeedsSave && not linksChangedAfter) $
            Loader.saveCachedDatabaseWithMatrices dbName (dcPath (sdConfig staged)) fbDatabase

        reportProgress Info $ "  [OK] Finalized: " <> T.unpack dbName
        pure loaded

    -- A failed relink wrote nothing, so the explicit save above must still fire.
    relinkFailed :: Text -> IO Bool
    relinkFailed err = warnSelfRelinkFailed dbName err >> pure False

--------------------------------------------------------------------------------
-- Method Collection Management
--------------------------------------------------------------------------------

{- | What a method path turned out to hold: one file carrying its own
factors, or a directory to scan. The two are read differently and only the
directory has flow definitions beside it.
-}
data MethodSource
    = BareMethodFile FilePath
    | MethodDirectory FilePath

{- | The method files found under one directory, split by what parses them.
Three lists of paths, so they are named rather than positional.
-}
data MethodFiles = MethodFiles
    { mfDirectory :: !FilePath
    , mfXml :: ![FilePath]
    , mfCsv :: ![FilePath]
    , mfJson :: ![FilePath]
    }

-- | What came back from parsing them, before the collection is assembled.
data ParsedMethodFiles = ParsedMethodFiles
    { pmfXmlMethods :: ![Method]
    , pmfCsvCollections :: ![MethodCollection]
    -- ^ SimaPro method exports, which are whole collections
    , pmfCsvMethods :: ![Method]
    -- ^ tabular CSVs, which are loose methods
    , pmfCsvFileCount :: !Int
    , pmfJsonMethods :: ![Method]
    , pmfErrors :: ![String]
    }

{- | Load methods from a MethodConfig path (directory or archive).
Handles ZIP/7z archives via resolveDataPath, finds method XMLs,
and enriches CFs from ILCD flow XMLs when available.
-}
loadMethodCollectionFromConfig :: MethodConfig -> IO (Either Text (MethodCollection, M.Map UUID ILCDFlowInfo))
loadMethodCollectionFromConfig mc = runExceptT $ do
    {- Resolve archives (ZIP to extracted directory). Single .json (openLCA
    JSON-LD ImpactCategory) and .csv (SimaPro method export) files are accepted
    directly without a wrapping directory or archive. -}
    resolvedPath <- ExceptT (resolveDataPath (mcPath mc))
    source <- ExceptT $ methodSourceAt resolvedPath
    files <- liftIO $ methodFilesOf source
    when (noMethodFiles files) $
        throwE ("No method files (.xml/.csv/.json) found in: " <> T.pack (mfDirectory files))
    flowInfo <- ExceptT $ flowDefinitionsFor source (mfDirectory files)
    parsed <- liftIO $ parseMethodFiles flowInfo files
    collection <- except (collectionOf parsed)
    liftIO $ reportProgress Info (parseCounts parsed)
    liftIO $ mapM_ (reportProgress Info) (nwCounts collection)
    liftIO $ mapM_ (reportProgress Warning) (parseFailures parsed)
    pure (collection, flowInfo)
  where
    {- What the path turned out to hold. A bare method file carries its own CFs
    and has no ILCD flows/ sibling; only a real directory does. -}
    methodSourceAt :: FilePath -> IO (Either Text MethodSource)
    methodSourceAt resolvedPath = do
        isDir <- doesDirectoryExist resolvedPath
        isFile <- doesFileExist resolvedPath
        let ext = map toLower (takeExtension resolvedPath)
        pure $ case (isDir, isFile, ext) of
            (True, _, _) -> Right (MethodDirectory resolvedPath)
            (_, True, ".json") -> Right (BareMethodFile resolvedPath)
            (_, True, ".csv") -> Right (BareMethodFile resolvedPath)
            _ -> Left (unusableMethodPath isFile)

    {- An archive that would not extract never reaches here: 'resolveDataPath'
    refuses it with the reason, rather than handing back the archive path for
    this to guess at from the extension. -}
    unusableMethodPath :: Bool -> Text
    unusableMethodPath isFile
        | not isFile = "Method path not found: " <> T.pack (mcPath mc)
        | otherwise =
            "Unsupported method file type (expected a directory, archive, .csv, or .json): "
                <> T.pack (mcPath mc)

    methodFilesOf :: MethodSource -> IO MethodFiles
    methodFilesOf (BareMethodFile file) =
        pure
            MethodFiles
                { mfDirectory = takeDirectory file
                , mfXml = []
                , mfCsv = [takeFileName file | isExt ".csv" file]
                , mfJson = [takeFileName file | isExt ".json" file]
                }
    methodFilesOf (MethodDirectory root) = do
        -- Find method directory (handles nested ILCD structures)
        dir <- findMethodDirectory root
        {- listDirectory order is filesystem-dependent; sort so a collection
        loads its methods in the same order on every machine (and a re-export
        of it is byte-stable). -}
        entries <- sort <$> listDirectory dir
        pure
            MethodFiles
                { mfDirectory = dir
                , mfXml = filter (isExt ".xml") entries
                , mfCsv = filter (isExt ".csv") entries
                , mfJson = filter (isExt ".json") entries
                }

    isExt :: String -> FilePath -> Bool
    isExt ext f = map toLower (takeExtension f) == ext

    noMethodFiles :: MethodFiles -> Bool
    noMethodFiles files = null (mfXml files) && null (mfCsv files) && null (mfJson files)

    {- Scanning a coincidental neighbouring flows/ would parse unrelated flow
    XMLs and register foreign synonyms under this collection's name, so only a
    real ILCD directory is looked at. -}
    flowDefinitionsFor :: MethodSource -> FilePath -> IO (Either Text (M.Map UUID ILCDFlowInfo))
    flowDefinitionsFor (BareMethodFile _) _ = pure (Right M.empty)
    flowDefinitionsFor (MethodDirectory _) dir =
        FlowResolver.resolveFlowDirectory dir >>= \case
            Nothing -> do
                reportProgress Info "  No flows/ directory found, using shortDescription fallback"
                pure (Right M.empty)
            Just flowsDir -> do
                reportProgress Info $ "  Loading ILCD flow XMLs from: " <> flowsDir
                outcome <- FlowResolver.parseFlowDirectory flowsDir
                mapM_ (\info -> reportProgress Info $ "  Loaded " <> show (M.size info) <> " flow definitions") outcome
                pure outcome

    parseMethodFiles :: M.Map UUID ILCDFlowInfo -> MethodFiles -> IO ParsedMethodFiles
    parseMethodFiles flowInfo files = do
        let dir = mfDirectory files
        -- Parse method files with flow enrichment
        xmlResults <- forM (mfXml files) $ \f ->
            Method.Parser.parseMethodFileWithFlows flowInfo (dir </> f)
        -- Split CSV files into SimaPro method exports and tabular CSVs
        csvResults <- forM (mfCsv files) $ \f -> do
            bytes <- stripBOM <$> BS.readFile (dir </> f)
            pure $
                if isSimaProMethodCSV bytes
                    then fmap Left (parseSimaProMethodCSVBytes bytes)
                    else fmap Right (parseMethodCSVBytes bytes)
        {- openLCA JSON-LD ImpactCategory files (carries optional regionalized
        CFs). Only files that actually carry @type=ImpactCategory are parsed;
        others are skipped silently since arbitrary .json files can sit
        alongside method data (e.g. metadata or other openLCA entity types). -}
        jsonResults <- forM (mfJson files) $ \f -> do
            bytes <- BS.readFile (dir </> f)
            pure $
                if OlcaSchema.isOlcaImpactCategoryJson bytes
                    then Just (OlcaSchema.parseOlcaImpactCategoryBytes bytes)
                    else Nothing
        let (xmlErrs, xmlMethods) = partitionEithers xmlResults
            (csvErrs, csvOks) = partitionEithers csvResults
            (jsonErrs, jsonMethods) = partitionEithers (catMaybes jsonResults)
        pure
            ParsedMethodFiles
                { pmfXmlMethods = xmlMethods
                , pmfCsvCollections = lefts csvOks
                , pmfCsvMethods = concat (rights csvOks)
                , pmfCsvFileCount = length csvOks
                , pmfJsonMethods = jsonMethods
                , pmfErrors = xmlErrs ++ csvErrs ++ jsonErrs
                }

    -- Merge: SimaPro CSVs are MethodCollections, tabular CSVs are [Method].
    collectionOf :: ParsedMethodFiles -> Either Text MethodCollection
    collectionOf parsed = case (allMethods, pmfErrors parsed) of
        ([], firstErr : _) -> Left ("All method files failed to parse: " <> T.pack firstErr)
        _ ->
            Right $
                MethodCollection
                    allMethods
                    -- Merge NW data from all SimaPro CSV sources
                    (concatMap mcDamageCategories (pmfCsvCollections parsed))
                    (concatMap mcNormWeightSets (pmfCsvCollections parsed))
                    []
      where
        allMethods :: [Method]
        allMethods =
            pmfXmlMethods parsed
                ++ pmfCsvMethods parsed
                ++ pmfJsonMethods parsed
                ++ concatMap mcMethods (pmfCsvCollections parsed)

    parseCounts :: ParsedMethodFiles -> String
    parseCounts parsed =
        "  Parsed "
            <> show (length (pmfXmlMethods parsed))
            <> " XML, "
            <> show (pmfCsvFileCount parsed)
            <> " CSV, "
            <> show (length (pmfJsonMethods parsed))
            <> " JSON file(s)"

    nwCounts :: MethodCollection -> Maybe String
    nwCounts collection
        | null (mcDamageCategories collection) = Nothing
        | otherwise =
            Just $
                "  "
                    <> show (length (mcDamageCategories collection))
                    <> " damage categories, "
                    <> show (length (mcNormWeightSets collection))
                    <> " normalization-weighting set(s)"

    parseFailures :: ParsedMethodFiles -> Maybe String
    parseFailures parsed
        | null (pmfErrors parsed) = Nothing
        | otherwise = Just ("  " <> show (length (pmfErrors parsed)) <> " method file(s) failed to parse")

-- | List all method collections with their status
listMethodCollections :: DatabaseManager -> IO [MethodCollectionStatus]
listMethodCollections manager = do
    available <- readTVarIO (dmAvailableMethods manager)
    loaded <- readTVarIO (dmLoadedMethods manager)
    return
        [ MethodCollectionStatus
            { mcsName = name
            , mcsDisplayName = mcName mc
            , mcsDescription = mcDescription mc
            , mcsStatus = if M.member name loaded then Loaded else Unloaded
            , mcsIsUploaded = mcIsUploaded mc
            , mcsPath = T.pack (mcPath mc)
            , mcsMethodCount = maybe 0 (length . mcMethods) (M.lookup name loaded)
            , mcsFormat = fromMaybe (detectFormatFromPath (mcPath mc)) (mcFormat mc)
            }
        | (name, mc) <- M.toList available
        ]
  where
    detectFormatFromPath :: FilePath -> Text
    detectFormatFromPath p
        | T.isInfixOf ".csv" (T.toLower (T.pack p)) = "SimaPro CSV"
        | T.isInfixOf ".json" (T.toLower (T.pack p)) = "Regionalized LCIA JSON"
        | otherwise = "ILCD"

-- | Load a method collection on demand
loadMethodCollection :: DatabaseManager -> Text -> IO (Either Text ())
loadMethodCollection manager name = do
    available <- readTVarIO (dmAvailableMethods manager)
    case M.lookup name available of
        Nothing -> return $ Left $ "Method collection not found: " <> name
        Just mc -> do
            already <- M.member name <$> readTVarIO (dmLoadedMethods manager)
            if already
                then return $ Right ()
                else do
                    reportProgress Info $ "[STARTING] Loading method: " <> T.unpack name
                    result <- loadMethodCollectionFromConfig mc
                    case result of
                        Left err -> do
                            reportProgress Error $ "  [FAIL] " <> T.unpack name <> ": " <> T.unpack err
                            return $ Left err
                        Right (collection0, flowInfo) -> do
                            -- Inject scoring sets from TOML config, apply declarative CF patches
                            let (collection, patchStats) = applyMethodConfig mc collection0
                            atomically $ modifyTVar' (dmLoadedMethods manager) (M.insert name collection)
                            clearMethodMappingCache manager
                            let methods = mcMethods collection
                                totalCFs = sum $ map (length . methodFactors) methods
                            reportProgress Info $
                                "  [OK] Loaded: "
                                    <> T.unpack name
                                    <> " ("
                                    <> show (length methods)
                                    <> " impact categories, "
                                    <> show totalCFs
                                    <> " characterization factors)"
                            warnZeroTouchPatches name patchStats
                            -- Auto-extract synonyms from ILCD flow definitions
                            let pairs = extractFromILCDFlows flowInfo
                            autoCreateFlowSynonyms
                                manager
                                name
                                (SynonymOrigin ("Auto-extracted from " <> name))
                                pairs
                            return $ Right ()

-- | Unload a method collection from memory
unloadMethodCollection :: DatabaseManager -> Text -> IO (Either Text ())
unloadMethodCollection manager name = do
    loaded <- readTVarIO (dmLoadedMethods manager)
    if M.member name loaded
        then do
            atomically $ modifyTVar' (dmLoadedMethods manager) (M.delete name)
            clearMethodMappingCache manager
            reportProgress Info $ "Unloaded method: " <> T.unpack name
            return $ Right ()
        else return $ Left $ "Method collection not loaded: " <> name

-- | Get all loaded methods (flattened across all collections)
getLoadedMethods :: DatabaseManager -> IO [(Text, Method)]
getLoadedMethods manager = do
    loaded <- readTVarIO (dmLoadedMethods manager)
    return [(collName, m) | (collName, coll) <- M.toList loaded, m <- mcMethods coll]

-- | Look up one loaded method collection by name.
getMethodCollection :: DatabaseManager -> Text -> IO (Maybe MethodCollection)
getMethodCollection manager name = M.lookup name <$> readTVarIO (dmLoadedMethods manager)

-- | Add a new method collection to the available list
addMethodCollection :: DatabaseManager -> MethodConfig -> IO ()
addMethodCollection manager mc =
    atomically $ modifyTVar' (dmAvailableMethods manager) (M.insert (mcName mc) mc)

-- | Remove an uploaded method collection (delete files + remove from memory)
removeMethodCollection :: DatabaseManager -> Text -> IO (Either Text ())
removeMethodCollection manager name = do
    available <- readTVarIO (dmAvailableMethods manager)
    loaded <- readTVarIO (dmLoadedMethods manager)
    case M.lookup name available of
        Nothing -> return $ Left $ "Method collection not found: " <> name
        Just mc
            | not (mcIsUploaded mc) ->
                return $ Left "Cannot delete configured method. Edit volca.toml to remove it."
            | M.member name loaded ->
                return $ Left "Cannot delete loaded method. Close it first."
            | otherwise -> do
                -- Find and delete the upload directory
                methodUploadsDir <- UploadedDB.getMethodUploadsDir
                -- The slug is derived from the directory name; search for it
                let slug = Upload.slugify name
                    uploadDir = methodUploadsDir </> T.unpack slug
                pathExists <- doesDirectoryExist uploadDir
                if pathExists
                    then do
                        result <- Control.Exception.try $ removeDirectoryRecursive uploadDir
                        case result of
                            Left (e :: SomeException) ->
                                return $ Left $ "Failed to delete: " <> T.pack (show e)
                            Right () -> do
                                reportProgress Info $ "Deleted method: " <> uploadDir
                                atomically $ modifyTVar' (dmAvailableMethods manager) (M.delete name)
                                return $ Right ()
                    else do
                        -- Directory already missing, just remove from memory
                        atomically $ modifyTVar' (dmAvailableMethods manager) (M.delete name)
                        return $ Right ()

--------------------------------------------------------------------------------
-- Merged reference data helpers
--------------------------------------------------------------------------------

-- | Get the merged SynonymDB from all loaded synonym databases.
getMergedSynonymDB :: DatabaseManager -> IO SynonymDB
getMergedSynonymDB manager = do
    loaded <- readTVarIO (dmLoadedFlowSyns manager)
    return $
        if M.null loaded
            then emptySynonymDB
            else mergeSynonymDBs (M.elems loaded)

{- | Surface one-way synonym bridges whose direction constraint is void in the
(merged) set, re-linked in the opposite view by an untyped transitive chain or
a contradictory row ('reopenedBridges'). 'demoteDuplicates' only drops the exact
duplicate pair, so this residue would otherwise silently widen a curated
one-way bridge back to both directions. Called where the merged set is about to
drive a database load, not on the request-path getters, so it fires once per
load rather than per query.
-}
warnReopenedBridges :: SynonymDB -> IO ()
warnReopenedBridges synDB =
    forM_ (reopenedBridges synDB) $ \e ->
        reportProgress Warning $
            "Flow synonyms: one-way bridge "
                <> show (seA e)
                <> " = "
                <> show (seB e)
                <> " ("
                <> dirLabel (seDir e)
                <> ") is re-linked in the opposite direction's view by other rows; its direction restriction is void"
  where
    dirLabel :: BridgeDirection -> String
    dirLabel BridgeBoth = "both"
    dirLabel BridgeInput = "input"
    dirLabel BridgeOutput = "output"

-- | Get the merged CompartmentMap from all loaded compartment mappings.
getMergedCompartmentMap :: DatabaseManager -> IO CompartmentMap
getMergedCompartmentMap manager = do
    loaded <- readTVarIO (dmLoadedCompMaps manager)
    return $ M.unions (M.elems loaded)

{- | Get the merged 'EnergyDensityMap' from all loaded energy-density sets.
First-wins union over active CSVs, mirroring 'getMergedCompartmentMap'.
-}
getMergedEnergyDensities :: DatabaseManager -> IO EnergyDensityMap
getMergedEnergyDensities manager = do
    loaded <- readTVarIO (dmLoadedEnergyDensities manager)
    return $ M.unions (M.elems loaded)

{- | Get the merged UnitConfig from all loaded unit definitions.
Memoized: pure over the loaded-unit-def set, invalidated on mutation.
-}
getMergedUnitConfig :: DatabaseManager -> IO UnitConversion.UnitConfig
getMergedUnitConfig manager = do
    cached <- readTVarIO (dmMergedUnitConfigCache manager)
    case cached of
        Just cfg -> pure cfg
        Nothing -> do
            loaded <- readTVarIO (dmLoadedUnitDefs manager)
            let !cfg =
                    if M.null loaded
                        then UnitConversion.defaultUnitConfig
                        else UnitConversion.mergeUnitConfigs (M.elems loaded)
            atomically $ writeTVar (dmMergedUnitConfigCache manager) (Just cfg)
            pure cfg

{- | What a database of this configuration is parsed under right now: the
merged unit table and the location aliases its configuration declares. A
cache is trusted only if it records the same pair.
-}
currentBuildInputs :: DatabaseManager -> DatabaseConfig -> IO BuildInputs
currentBuildInputs manager dbConfig = do
    unitConfig <- getMergedUnitConfig manager
    pure (BuildInputs unitConfig (dcLocationAliases dbConfig) (dcAllocation dbConfig))

{- | Snapshot of flow + unit metadata across every currently-loaded DB.
Used to characterize or display a cross-DB-merged 'Inventory', whose
flow UUIDs can come from any loaded DB. Without the merge, root-DB-only
metadata silently drops every dep-DB flow during LCIA characterization
(CF lookup falls off the end of the fallback chain) and inventory export.

Memoized on 'dmMergedFlowMetadataCache': the merged Maps are pure over
the loaded-DB set, so the expensive 'M.unions' + UUID collision scan
runs once per DB-set mutation instead of per LCIA call (previously the
dominant source of garbage in 27-wide 'mapConcurrently' characterization).

Detects UUID collisions with divergent metadata. 'M.unions' is first-wins;
collisions should never happen (same UUID ⇒ same flow by construction),
but if data drift produces them, surface via log rather than hide.
-}

{- | The parsed geographies in the shape every consumer reads them: a
@Map ChildLocation [ParentLocation]@, walked by the LCIA regionalized scoring
path (see 'Method.Mapping.computeRegionalizedLCIAScore') and by supplier
resolution. Fills 'dmLocationHierarchy'; the spec reads it too, so the shape
under test is the shape the matcher gets.
-}
hierarchyFromGeographies :: M.Map Text (Text, [Text]) -> M.Map Location [Location]
hierarchyFromGeographies = M.map (map Location . snd) . M.mapKeysMonotonic Location

{- | The same table in the shape a geography filter reads it. Read here rather
than at each of the dozen call sites, so a filter and the count beside it cannot
end up reading the same table two different ways.
-}
managerGeographies :: DatabaseManager -> Geographies
managerGeographies = readGeographies . dmLocationHierarchy

{- | Merged biosphere flow metadata + units across all loaded DBs. Technosphere
flows are not merged here because characterization (the only consumer of
this cache) targets biosphere flows exclusively.
-}
getMergedFlowMetadata :: DatabaseManager -> IO (BioFlowDB, UnitDB)
getMergedFlowMetadata manager = do
    cached <- readTVarIO (dmMergedFlowMetadataCache manager)
    case cached of
        Just snap -> pure snap
        Nothing -> do
            loaded <- readTVarIO (dmLoadedDbs manager)
            let dbs = map ldDatabase (M.elems loaded)
                bioMaps = map dbBioFlows dbs
                unitMaps = map dbUnits dbs
                !mergedBios = M.unions bioMaps
                !mergedUnits = M.unions unitMaps
                bioHits = collisions bioFingerprint bioMaps
                unitHits = collisions unitFingerprint unitMaps
            unless (null bioHits) $
                reportProgress Warning $
                    "[merged BioFlowDB] "
                        <> show (length bioHits)
                        <> " UUID collision(s) with divergent biosphere flow metadata; keeping first. Samples: "
                        <> show (take 3 bioHits)
            unless (null unitHits) $
                reportProgress Warning $
                    "[merged UnitDB] "
                        <> show (length unitHits)
                        <> " UUID collision(s) with divergent unit metadata; keeping first. Samples: "
                        <> show (take 3 unitHits)
            let !snap = (mergedBios, mergedUnits)
            atomically $ writeTVar (dmMergedFlowMetadataCache manager) (Just snap)
            pure snap
  where
    bioFingerprint :: BiosphereFlow -> (Text, Text, Maybe Text)
    bioFingerprint f = (bfName f, bfCompartmentName f, bfCompartmentSub f)

    unitFingerprint :: Unit -> Text
    unitFingerprint = unitName

    collisions :: (Ord fp) => (v -> fp) -> [Map UUID v] -> [UUID]
    collisions fp ms =
        let step = M.foldlWithKey' (insertFp fp)
            insertFp f acc k v = M.insertWith S.union k (S.singleton (f v)) acc
            merged = foldl step (M.empty :: Map UUID (S.Set fp)) ms
         in [u | (u, fps) <- M.toList merged, S.size fps > 1]

-- | Status of a reference data resource for API responses
data RefDataStatus = RefDataStatus
    { rdsName :: !Text
    , rdsDisplayName :: !Text
    , rdsDescription :: !(Maybe Text)
    , rdsStatus :: !DatabaseLoadStatus
    , rdsIsUploaded :: !Bool
    , rdsIsAuto :: !Bool
    , rdsEntryCount :: !Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON RefDataStatus where
    toJSON RefDataStatus{..} =
        A.object
            [ "rdsName" .= rdsName
            , "rdsDisplayName" .= rdsDisplayName
            , "rdsDescription" .= rdsDescription
            , "rdsStatus" .= rdsStatus
            , "rdsIsUploaded" .= rdsIsUploaded
            , "rdsIsAuto" .= rdsIsAuto
            , "rdsEntryCount" .= rdsEntryCount
            ]

instance FromJSON RefDataStatus where
    parseJSON = A.withObject "RefDataStatus" $ \v ->
        RefDataStatus
            <$> v .: "rdsName"
            <*> v .: "rdsDisplayName"
            <*> v .:? "rdsDescription"
            <*> v .: "rdsStatus"
            <*> v .: "rdsIsUploaded"
            <*> v .: "rdsIsAuto"
            <*> v .: "rdsEntryCount"

--------------------------------------------------------------------------------
-- Generic ref-data operations (shared by flow synonyms, compartment maps, units)
--------------------------------------------------------------------------------

-- | Operations for a ref-data kind: everything that varies between the three.
data RefDataOps a = RefDataOps
    { rdoAvailableVar :: !(DatabaseManager -> TVar (Map Text RefDataConfig))
    , rdoLoadedVar :: !(DatabaseManager -> TVar (Map Text a))
    , rdoParse :: !(BL.ByteString -> Either Text a)
    , rdoCount :: !(a -> Int)
    , rdoLabel :: !String
    , rdoUploadDir :: !FilePath
    , rdoCanDelete :: !(RefDataConfig -> Bool)
    }

flowSynOps :: RefDataOps SynonymDB
flowSynOps =
    RefDataOps
        { rdoAvailableVar = dmAvailableFlowSyns
        , rdoLoadedVar = dmLoadedFlowSyns
        , rdoParse = first T.pack . buildFromCSV
        , rdoCount = synonymCount
        , rdoLabel = "flow synonyms"
        , rdoUploadDir = "uploads/flow-synonyms"
        , rdoCanDelete = \rd -> rdIsUploaded rd || rdIsAuto rd
        }

compMapOps :: RefDataOps CompartmentMap
compMapOps =
    RefDataOps
        { rdoAvailableVar = dmAvailableCompMaps
        , rdoLoadedVar = dmLoadedCompMaps
        , rdoParse = first T.pack . buildCompartmentMapFromCSV
        , rdoCount = compartmentMapSize
        , rdoLabel = "compartment mapping"
        , rdoUploadDir = "uploads/compartment-mappings"
        , rdoCanDelete = rdIsUploaded
        }

unitDefOps :: RefDataOps UnitConversion.UnitConfig
unitDefOps =
    RefDataOps
        { rdoAvailableVar = dmAvailableUnitDefs
        , rdoLoadedVar = dmLoadedUnitDefs
        , rdoParse = UnitConversion.buildFromCSV
        , rdoCount = UnitConversion.unitCount
        , rdoLabel = "units"
        , rdoUploadDir = "uploads/units"
        , rdoCanDelete = rdIsUploaded
        }

energyDensityOps :: RefDataOps EnergyDensityMap
energyDensityOps =
    RefDataOps
        { rdoAvailableVar = dmAvailableEnergyDensities
        , rdoLoadedVar = dmLoadedEnergyDensities
        , rdoParse = first T.pack . buildEnergyDensityMapFromCSV
        , rdoCount = energyDensityMapSize
        , rdoLabel = "energy densities"
        , rdoUploadDir = "uploads/energy-densities"
        , rdoCanDelete = rdIsUploaded
        }

listRefDataG :: RefDataOps a -> DatabaseManager -> IO [RefDataStatus]
listRefDataG ops manager = do
    available <- readTVarIO (rdoAvailableVar ops manager)
    loaded <- readTVarIO (rdoLoadedVar ops manager)
    return
        [ RefDataStatus
            { rdsName = rdName rd
            , rdsDisplayName = rdName rd
            , rdsDescription = rdDescription rd
            , rdsStatus = if M.member (rdName rd) loaded then Loaded else Unloaded
            , rdsIsUploaded = rdIsUploaded rd
            , rdsIsAuto = rdIsAuto rd
            , rdsEntryCount = maybe 0 (rdoCount ops) (M.lookup (rdName rd) loaded)
            }
        | rd <- M.elems available
        ]

loadRefDataG :: RefDataOps a -> DatabaseManager -> Text -> IO (Either Text ())
loadRefDataG ops manager name = do
    available <- readTVarIO (rdoAvailableVar ops manager)
    case M.lookup name available of
        Nothing -> return $ Left $ T.pack (rdoLabel ops) <> " not found: " <> name
        Just rd -> do
            loaded <- readTVarIO (rdoLoadedVar ops manager)
            if M.member name loaded
                then return $ Right ()
                else do
                    result <- readRefDataSource (rdSource rd)
                    case result of
                        Left err -> return $ Left err
                        Right csvData -> case rdoParse ops csvData of
                            Left err -> return $ Left err
                            Right val -> do
                                atomically $ do
                                    modifyTVar' (rdoLoadedVar ops manager) (M.insert name val)
                                    invalidateMergedRefCaches manager
                                reportProgress Info $ "Loaded " <> rdoLabel ops <> ": " <> T.unpack name
                                return $ Right ()

unloadRefDataG :: RefDataOps a -> DatabaseManager -> Text -> IO (Either Text ())
unloadRefDataG ops manager name = do
    loaded <- readTVarIO (rdoLoadedVar ops manager)
    if M.member name loaded
        then do
            atomically $ do
                modifyTVar' (rdoLoadedVar ops manager) (M.delete name)
                invalidateMergedRefCaches manager
            reportProgress Info $ "Unloaded " <> rdoLabel ops <> ": " <> T.unpack name
            return $ Right ()
        else return $ Left $ T.pack (rdoLabel ops) <> " not loaded: " <> name

{- | Drop the merged-ref-data caches. Conservatively clears both: the
flow-metadata and unit-config snapshots are cheap to rebuild lazily, and
ref-data changes (units, flow synonyms, compartment maps) are rare enough
that per-kind dispatch adds no observable value.
-}
invalidateMergedRefCaches :: DatabaseManager -> STM ()
invalidateMergedRefCaches manager = do
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

addRefDataG :: RefDataOps a -> DatabaseManager -> RefDataConfig -> IO ()
addRefDataG ops manager rd =
    atomically $ modifyTVar' (rdoAvailableVar ops manager) (M.insert (rdName rd) rd)

removeRefDataG :: RefDataOps a -> DatabaseManager -> Text -> IO (Either Text ())
removeRefDataG ops manager name = do
    available <- readTVarIO (rdoAvailableVar ops manager)
    case M.lookup name available of
        Nothing -> return $ Left $ T.pack (rdoLabel ops) <> " not found: " <> name
        Just rd | not (rdoCanDelete ops rd) -> return $ Left $ "Cannot delete preinstalled " <> T.pack (rdoLabel ops)
        Just _ -> do
            loaded <- readTVarIO (rdoLoadedVar ops manager)
            if M.member name loaded
                then return $ Left "Unload before deleting"
                else do
                    removeUploadedRefData (rdoUploadDir ops) name
                    atomically $ modifyTVar' (rdoAvailableVar ops manager) (M.delete name)
                    return $ Right ()

-- | Auto-load active flow synonyms using binary cache for speed
autoLoadFlowSynonyms :: TVar (Map Text SynonymDB) -> [RefDataConfig] -> IO ()
autoLoadFlowSynonyms loadedVar configs =
    forM_ (filter rdActive configs) $ \rd -> do
        -- The binary cache pays for a registry of 161K pairs; the built-in
        -- table holds a thousand and parses in milliseconds.
        result <- case rdSource rd of
            FromFile path -> loadFromCSVFileWithCache path
            BuiltIn t -> pure (buildFromCSV (builtinContent t))
        case result of
            Right synDB -> do
                atomically $ modifyTVar' loadedVar (M.insert (rdName rd) synDB)
                reportProgress Info $
                    "  [OK] Loaded flow synonyms: "
                        <> T.unpack (rdName rd)
                        <> " ("
                        <> show (synonymCount synDB)
                        <> " entries)"
            Left err ->
                reportError $ "  [FAIL] Failed to load flow synonyms " <> T.unpack (rdName rd) <> ": " <> err

-- | Auto-load active reference data at startup
autoLoadRefData :: RefDataOps a -> TVar (Map Text a) -> [RefDataConfig] -> IO ()
autoLoadRefData ops loadedVar configs =
    forM_ (filter rdActive configs) $ \rd -> do
        result <- readRefDataSource (rdSource rd)
        case result of
            Right csvData -> case rdoParse ops csvData of
                Right val -> do
                    atomically $ modifyTVar' loadedVar (M.insert (rdName rd) val)
                    reportProgress Info $
                        "  [OK] Loaded "
                            <> rdoLabel ops
                            <> ": "
                            <> T.unpack (rdName rd)
                            <> " ("
                            <> show (rdoCount ops val)
                            <> " entries)"
                Left err ->
                    reportError $ "  [FAIL] Failed to parse " <> rdoLabel ops <> " " <> T.unpack (rdName rd) <> ": " <> T.unpack err
            Left err -> reportError $ "  [FAIL] Failed to read " <> T.unpack (rdName rd) <> ": " <> T.unpack err

-- Public API: delegates to generic ops

listFlowSynonyms :: DatabaseManager -> IO [RefDataStatus]
listFlowSynonyms = listRefDataG flowSynOps

loadFlowSynonyms :: DatabaseManager -> Text -> IO (Either Text ())
loadFlowSynonyms = loadRefDataG flowSynOps

unloadFlowSynonyms :: DatabaseManager -> Text -> IO (Either Text ())
unloadFlowSynonyms = unloadRefDataG flowSynOps

addFlowSynonyms :: DatabaseManager -> RefDataConfig -> IO ()
addFlowSynonyms = addRefDataG flowSynOps

removeFlowSynonyms :: DatabaseManager -> Text -> IO (Either Text ())
removeFlowSynonyms = removeRefDataG flowSynOps

-- | Get synonym groups for a specific loaded flow synonyms resource.
getFlowSynonymGroups :: DatabaseManager -> Text -> IO (Either Text [[Text]])
getFlowSynonymGroups manager name = do
    loaded <- readTVarIO (dmLoadedFlowSyns manager)
    case M.lookup name loaded of
        Nothing -> return $ Left $ "Flow synonyms not loaded: " <> name
        Just synDB -> return $ Right $ M.elems (synIdToNames synDB)

listCompartmentMappings :: DatabaseManager -> IO [RefDataStatus]
listCompartmentMappings = listRefDataG compMapOps

loadCompartmentMappings :: DatabaseManager -> Text -> IO (Either Text ())
loadCompartmentMappings = loadRefDataG compMapOps

unloadCompartmentMappings :: DatabaseManager -> Text -> IO (Either Text ())
unloadCompartmentMappings = unloadRefDataG compMapOps

addCompartmentMappings :: DatabaseManager -> RefDataConfig -> IO ()
addCompartmentMappings = addRefDataG compMapOps

removeCompartmentMappings :: DatabaseManager -> Text -> IO (Either Text ())
removeCompartmentMappings = removeRefDataG compMapOps

listUnitDefs :: DatabaseManager -> IO [RefDataStatus]
listUnitDefs = listRefDataG unitDefOps

loadUnitDefs :: DatabaseManager -> Text -> IO (Either Text ())
loadUnitDefs = loadRefDataG unitDefOps

unloadUnitDefs :: DatabaseManager -> Text -> IO (Either Text ())
unloadUnitDefs = unloadRefDataG unitDefOps

addUnitDefs :: DatabaseManager -> RefDataConfig -> IO ()
addUnitDefs = addRefDataG unitDefOps

removeUnitDefs :: DatabaseManager -> Text -> IO (Either Text ())
removeUnitDefs = removeRefDataG unitDefOps

--------------------------------------------------------------------------------
-- Reference data helpers
--------------------------------------------------------------------------------

{- | Parse a geographies CSV file (code,display_name,parents) into a lookup map.
Parents field uses '|' as separator. display_name is optional (falls back to code).
Comment lines start with '#', and the header row starts with "code".

Quoting is the reason this goes through a real CSV reader rather than splitting
on commas: several location codes carry one -- @Europe, Western@,
@IAI Area, EU27 & EFTA@ -- and a hand-split would cut them in half, turning a
region into two codes that match nothing.
-}
parseGeographiesCSV :: FilePath -> IO (Either Text (Map Text (Text, [Text])))
parseGeographiesCSV path = do
    exists <- doesFileExist path
    if not exists
        then do
            reportProgress Warning $ "Geographies file not found: " <> path <> " (running with no hierarchy)"
            return (Right M.empty)
        else parseGeographies (T.pack path) <$> BS.readFile path

{- | The hierarchy a geographies table describes. The label names the source in
messages: a path, or the built-in table. Decodes the bytes as UTF-8 itself:
the table carries non-ASCII display names, and a locale-driven read would
give mojibake or a crash on a non-UTF-8 system.
-}
parseGeographies :: Text -> BS.ByteString -> Either Text (Map Text (Text, [Text]))
parseGeographies label bytes = do
    content <- first (\decodeErr -> label <> ": not valid UTF-8 (" <> T.pack (show decodeErr) <> ")") (TE.decodeUtf8' bytes)
    rows <- first (\err -> label <> ": " <> T.pack err) (Csv.decode Csv.NoHeader (BL.fromStrict (TE.encodeUtf8 (T.unlines (filter meaningful (T.lines content))))))
    -- A duplicated code would silently shadow the earlier row in the map;
    -- refuse the table instead.
    first duplicateCodes (uniqueIndex (map entry (V.toList rows)))
  where
    duplicateCodes :: NonEmpty Text -> Text
    duplicateCodes codes = label <> ": duplicate codes: " <> T.intercalate ", " (NE.toList codes)

    meaningful :: Text -> Bool
    meaningful line =
        let stripped = T.strip line
         in not (T.null stripped)
                && not ("#" `T.isPrefixOf` stripped)
                && not ("code," `T.isPrefixOf` stripped)
    entry :: (Text, Text, Text) -> (Text, (Text, [Text]))
    entry (codeRaw, displayRaw, parentsRaw) =
        let code = T.strip codeRaw
            display = T.strip displayRaw
            parents = T.strip parentsRaw
         in ( code
            ,
                ( if T.null display then code else display
                , if T.null parents then [] else T.splitOn "|" parents
                )
            )

-- | Load CSV file content from path.
loadRefDataCSV :: FilePath -> IO (Either Text BL.ByteString)
loadRefDataCSV path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "File not found: " <> T.pack path
        else Right <$> BL.readFile path

-- | The bytes of a reference table, wherever it comes from.
readRefDataSource :: RefDataSource -> IO (Either Text BL.ByteString)
readRefDataSource (BuiltIn t) = pure (Right (builtinContent t))
readRefDataSource (FromFile path) = loadRefDataCSV path

{- | Discover uploaded reference data from a directory.
Each subdirectory should contain a data.csv and optional meta.toml.
-}
discoverUploadedRefData :: FilePath -> IO [RefDataConfig]
discoverUploadedRefData baseDir = do
    exists <- doesDirectoryExist baseDir
    if not exists
        then return []
        else do
            entries <- listDirectory baseDir
            fmap catMaybes $ forM entries $ \entry -> do
                let dirPath = baseDir </> entry
                    csvPath = dirPath </> "data.csv"
                csvExists <- doesFileExist csvPath
                if csvExists
                    then do
                        let name = T.pack entry
                            isAuto = "auto-" `T.isPrefixOf` name
                        reportProgress Info $ "Discovered uploaded ref data: " <> T.unpack name
                        return $
                            Just
                                RefDataConfig
                                    { rdName = name
                                    , rdSource = FromFile csvPath
                                    , rdActive = not isAuto -- Auto-extracted synonyms inactive by default (noisy); curated data/flows.csv preferred
                                    , rdIsUploaded = True
                                    , rdIsAuto = isAuto
                                    , rdDescription = Nothing
                                    }
                    else return Nothing

-- | Remove uploaded reference data directory.
removeUploadedRefData :: FilePath -> Text -> IO ()
removeUploadedRefData baseDir name = do
    let uploadDir = baseDir </> T.unpack name
    exists <- doesDirectoryExist uploadDir
    when exists $ do
        result <- try $ removeDirectoryRecursive uploadDir
        case result of
            Left (e :: SomeException) ->
                reportError $ "Failed to delete " <> uploadDir <> ": " <> show e
            Right () ->
                reportProgress Info $ "Deleted: " <> uploadDir

-- | The sentence a candidate synonym set carries about where it came from.
newtype SynonymOrigin = SynonymOrigin Text

{- | A synonym carried by more distinct flows than this is a classification label
or stop-word (e.g. @"organic"@), not a true synonym: 'excludeOverFrequentSynonyms'
drops it. The bound sits in the gap between the class-label hubs (≥187 flows in
EF 3.1) and the first genuine flow name used as a synonym (~17 flows).
-}
maxSynonymFlowFrequency :: Int
maxSynonymFlowFrequency = 25

{- | Persist auto-extracted synonym pairs as an opt-in candidate set.
Writes CSV to uploads/flow-synonyms/auto-{source}/data.csv and registers it
inactive. The pairs never enter the matching: flow matching trusts only the
curated registry (data/flows.csv) plus sources the user explicitly activates
(activation lasts for the session and only reaches databases loaded after it);
DB-embedded synonyms are a bootstrap input for offline curation, not a runtime
one. To regenerate a stale candidate, remove the source and reload.
-}
autoCreateFlowSynonyms :: DatabaseManager -> Text -> SynonymOrigin -> [(Text, Text)] -> IO ()
autoCreateFlowSynonyms _ _ _ [] = return ()
autoCreateFlowSynonyms manager sourceName (SynonymOrigin description) pairs = do
    let slug = "auto-" <> sourceName
    -- Skip if already registered (persisted candidate from a previous run,
    -- discovered at startup, or extracted earlier this session)
    alreadyExtracted <- atomically $ M.member slug <$> readTVar (dmAvailableFlowSyns manager)
    if alreadyExtracted
        then reportProgress Info $ "  [AUTO] " <> T.unpack slug <> ": candidate already extracted"
        else do
            let (nonJunkPairs, junkTokens) = excludeJunkSynonyms pairs
                (keptPairs, excludedSyns) =
                    excludeOverFrequentSynonyms maxSynonymFlowFrequency nonJunkPairs
            unless (null junkTokens) $
                reportProgress Info $
                    "  [AUTO] "
                        <> T.unpack slug
                        <> ": dropped "
                        <> show (length junkTokens)
                        <> " placeholder/non-substance synonym tokens: "
                        <> T.unpack (T.intercalate ", " (take 8 junkTokens))
            unless (null excludedSyns) $
                reportProgress Info $
                    "  [AUTO] "
                        <> T.unpack slug
                        <> ": excluded "
                        <> show (length excludedSyns)
                        <> " over-frequent synonym tokens (class labels/stop-words): "
                        <> T.unpack
                            ( T.intercalate ", " $
                                map (\(tok, n) -> tok <> "(" <> T.pack (show n) <> ")") (take 8 excludedSyns)
                            )
            let dir = "uploads/flow-synonyms" </> T.unpack slug
                path = dir </> "data.csv"
            createDirectoryIfMissing True dir
            BL.writeFile path (synonymPairsToCSV keptPairs)
            let rd =
                    RefDataConfig
                        { rdName = slug
                        , rdSource = FromFile path
                        , rdActive = False -- auto-extracted synonyms inactive by default (noisy, use curated data/flows.csv)
                        , rdIsUploaded = True
                        , rdIsAuto = True
                        , rdDescription = Just description
                        }
            addFlowSynonyms manager rd
            -- Close the candidate set only to audit its quality: an oversized
            -- class means the transitive closure fused unrelated substances
            -- through an ambiguous bridge (a junk hub): surface it so the
            -- curator sees it before ever activating the source.
            forM_ (oversizedClasses 100 keptPairs) $ \cls ->
                reportProgress Warning $
                    "  [AUTO] "
                        <> T.unpack slug
                        <> ": synonym closure fused "
                        <> show (length cls)
                        <> " names into one class (possible junk hub); e.g. "
                        <> T.unpack (T.intercalate ", " (take 5 cls))
            reportProgress Info $
                "  [AUTO] "
                    <> T.unpack slug
                    <> ": "
                    <> show (length keptPairs)
                    <> " candidate synonym pairs (opt-in, not loaded)"

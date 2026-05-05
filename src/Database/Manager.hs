{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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
    MissingSupplier (..),
    DependencySuggestion (..),
    MethodCollectionStatus (..),
    RefDataStatus (..),

    -- * Re-exports
    DepLoadResult (..),

    -- * Initialization
    initDatabaseManager,

    -- * Operations
    getDatabase,
    mkDepSolverLookup,
    listDatabases,

    -- * Load/Unload
    loadDatabase,
    unloadDatabase,
    relinkDatabase,
    RelinkResult (..),
    addDatabase,
    removeDatabase,

    -- * Method Operations
    listMethodCollections,
    loadMethodCollection,
    unloadMethodCollection,
    getLoadedMethods,
    addMethodCollection,
    removeMethodCollection,

    -- * Reference Data Operations
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
    getMergedUnitConfig,
    getMergedFlowMetadata,
    getLocationHierarchy,

    -- * Staged Database Operations
    getStagedDatabase,
    getDatabaseSetupInfo,
    addDependencyToStaged,
    removeDependencyFromStaged,
    setDataPath,
    finalizeDatabase,

    -- * Cached flow mapping
    mapMethodToFlowsCached,
    mapMethodToTablesCached,

    -- * Internal (for Main.hs to load database)
    loadDatabaseFromConfig,
) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Control.Exception
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (isAbsolute, normalise, takeDirectory, takeExtension, (</>))
import System.Mem (performGC)

import Config
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (diffUTCTime, getCurrentTime)
import Database (buildDatabaseWithMatrices)
import qualified Database.Loader as Loader
import Matrix (clearCachedSolver)
import Method.Mapping (MatchStrategy, MethodTables, buildMethodTables, mapMethodToFlows)
import Method.Types (
    CompartmentMap,
    Method (..),
    MethodCF,
    MethodCollection (..),
    ScoringSet (..),
    buildCompartmentMapFromCSV,
    compartmentMapSize,
 )
import Plugin.Config (buildRegistry)
import Plugin.Types (PluginRegistry (..), TransformContext (..), TransformHandle (..), TransformResult (..))
import Progress (ProgressLevel (..), reportError, reportProgress, reportProgressWithTiming)
import qualified Search.BM25 as BM25
import SharedSolver (SharedSolver, createSharedSolver)
import qualified SharedSolver
import SynonymDB (SynonymDB (..), buildFromCSV, buildFromPairs, emptySynonymDB, loadFromCSVFileWithCache, mergeSynonymDBs, synonymCount)
import Types (Activity (..), CrossDBLink (..), CrossDBLinkingStats (..), Database (..), Flow (..), FlowDB, LinkBlocker (..), SimpleDatabase (..), SparseTriple (..), UUID, Unit (..), UnitDB, crossDBBySource, deduplicateFallbacks, exchangeFlowId, exchangeIsReference, initializeRuntimeFields, toSimpleDatabase, unresolvedCount)
import qualified UnitConversion

-- CrossDBLinkingStats is now in Types, re-exported from Database.Loader

import API.Types (DepLoadResult (..))
import qualified Data.Text.IO as TIO
import Database.CrossLinking (IndexedDatabase (..), LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold)
import Database.Upload (DatabaseFormat (..), findMethodDirectory)
import qualified Database.Upload as Upload
import qualified Database.UploadedDatabase as UploadedDB
import Method.FlowResolver (ILCDFlowInfo)
import qualified Method.FlowResolver as FlowResolver
import qualified Method.Parser
import qualified Method.Parser.OlcaSchema as OlcaSchema
import Method.ParserCSV (parseMethodCSVBytes)
import Method.ParserSimaPro (isSimaProMethodCSV, parseSimaProMethodCSVBytes)
import qualified SimaPro.Parser as SimaPro
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
    , sdUnlinkedCount :: !Int
    -- ^ Total unlinked exchanges
    , sdMissingProducts :: ![(Text, Int, LinkBlocker)]
    -- ^ (product name, count, reason)
    , sdSelectedDeps :: ![Text]
    -- ^ Selected dependency database names
    , sdCrossDBLinks :: ![CrossDBLink]
    -- ^ Cross-DB links found so far
    , sdLinkingStats :: !CrossDBLinkingStats
    -- ^ Linking statistics
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

instance ToJSON MissingSupplier where
    toJSON MissingSupplier{..} =
        A.object
            [ "productName" .= msProductName
            , "count" .= msCount
            , "location" .= msLocation
            , "reason" .= msReason
            , "detail" .= msDetail
            ]

-- | Suggestion for a dependency database
data DependencySuggestion = DependencySuggestion
    { dsgDatabaseName :: !Text
    , dsgDisplayName :: !Text
    , dsgMatchCount :: !Int
    -- ^ How many missing suppliers it can provide
    }
    deriving (Show, Eq, Generic)

instance ToJSON DependencySuggestion where
    toJSON DependencySuggestion{..} =
        A.object
            [ "databaseName" .= dsgDatabaseName
            , "displayName" .= dsgDisplayName
            , "matchCount" .= dsgMatchCount
            ]

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
    , dsiSelectedDeps :: ![Text]
    -- ^ Currently selected dependencies
    , dsiSuggestions :: ![DependencySuggestion]
    -- ^ Suggested dependencies
    , dsiIsReady :: !Bool
    -- ^ True if can be finalized
    , dsiUnknownUnits :: ![Text]
    -- ^ Unknown units from sdbUnits
    , dsiLocationFallbacks :: ![(Text, Text, Text)]
    -- ^ (product, requestedLoc, actualLoc)
    , dsiDataPath :: !Text
    -- ^ Current selected data path (relative)
    , dsiAvailablePaths :: ![(Text, Text, Int)]
    -- ^ (relativePath, formatLabel, fileCount)
    , dsiIsLoaded :: !Bool
    -- ^ True if database is already loaded (read-only info)
    }
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseSetupInfo where
    toJSON DatabaseSetupInfo{..} =
        A.object
            [ "name" .= dsiName
            , "displayName" .= dsiDisplayName
            , "activityCount" .= dsiActivityCount
            , "inputCount" .= dsiInputCount
            , "completeness" .= dsiCompleteness
            , "internalLinks" .= dsiInternalLinks
            , "crossDBLinks" .= dsiCrossDBLinks
            , "unresolvedLinks" .= dsiUnresolvedLinks
            , "missingSuppliers" .= dsiMissingSuppliers
            , "selectedDependencies" .= dsiSelectedDeps
            , "suggestions" .= dsiSuggestions
            , "isReady" .= dsiIsReady
            , "unknownUnits" .= dsiUnknownUnits
            , "locationFallbacks" .= map encodeFallback dsiLocationFallbacks
            , "dataPath" .= dsiDataPath
            , "availablePaths" .= map encodeCandidate dsiAvailablePaths
            , "isLoaded" .= dsiIsLoaded
            ]
      where
        encodeFallback (prod, req, act) =
            A.object
                ["product" .= prod, "requested" .= req, "actual" .= act]
        encodeCandidate (path, fmt, cnt) =
            A.object
                ["path" .= path, "format" .= fmt, "fileCount" .= cnt]

-- | Errors from getDatabaseSetupInfo
data SetupError = SetupNotFound Text | SetupFailed Text
    deriving (Show, Eq)

setupErrorMessage :: SetupError -> Text
setupErrorMessage (SetupNotFound msg) = msg
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
    , dmNoCache :: !Bool -- Caching disabled flag
    , dmPlugins :: !PluginRegistry -- Plugin registry (built-in + external)
    , dmGeographies :: !(Map Text (Text, [Text])) -- code → (display_name, parent_codes)
    , dmMethodMappingCache :: !(TVar (Map (Text, UUID) [(MethodCF, Maybe (Flow, MatchStrategy))]))
    {- ^ Cached flow mappings: (dbName, methodId) → mappings.
    Invalidated on database/method/synonym reload.
    -}
    , dmMethodTablesCache :: !(TVar (Map (Text, UUID) MethodTables))
    {- ^ Cached LCIA-score lookup tables built from mappings.
    These depend only on (db, method), so building them once per pair
    saves O(n log n) Map constructions on every LCIA call.
    -}
    , dmMergedFlowMetadataCache :: !(TVar (Maybe (FlowDB, UnitDB)))
    {- ^ Memoized 'M.unions' of every loaded DB's flows/units.
    Invalidated on any 'dmLoadedDbs' mutation; collision detection
    runs once per rebuild rather than per hot-path call.
    -}
    , dmMergedUnitConfigCache :: !(TVar (Maybe UnitConversion.UnitConfig))
    {- ^ Memoized merge of every loaded unit-definition set.
    Invalidated on 'dmLoadedUnitDefs' mutation.
    -}
    }

{- | Cached flow mapping: avoids re-matching method CFs to database flows on every LCIA call.
The mapping depends only on (database, method), not on the process being evaluated.
-}
mapMethodToFlowsCached :: DatabaseManager -> Text -> Database -> Method -> IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodToFlowsCached manager dbName db method = do
    let key = (dbName, methodId method)
    cache <- readTVarIO (dmMethodMappingCache manager)
    case M.lookup key cache of
        Just cached -> return cached
        Nothing -> do
            result <- mapMethodToFlows (prMappers (dmPlugins manager)) db method
            atomically $ modifyTVar' (dmMethodMappingCache manager) (M.insert key result)
            return result

-- | Cached prepared CF tables: built once per (db, method), reused across inventories.
mapMethodToTablesCached :: DatabaseManager -> Text -> Database -> Method -> IO MethodTables
mapMethodToTablesCached manager dbName db method = do
    let key = (dbName, methodId method)
    cache <- readTVarIO (dmMethodTablesCache manager)
    case M.lookup key cache of
        Just tables -> pure tables
        Nothing -> do
            mappings <- mapMethodToFlowsCached manager dbName db method
            let !tables = buildMethodTables mappings
            atomically $ modifyTVar' (dmMethodTablesCache manager) (M.insert key tables)
            pure tables

{- | Clear all cached flow mappings (call when databases, methods, or synonyms change).
Also drops the merged flow/unit snapshots — both caches depend on the loaded-DB set.
-}
clearMethodMappingCache :: DatabaseManager -> IO ()
clearMethodMappingCache manager = atomically $ do
    writeTVar (dmMethodMappingCache manager) M.empty
    writeTVar (dmMethodTablesCache manager) M.empty
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

{- | Clear cached flow mappings for a specific database.
The merged flow/unit snapshots span every loaded DB, so a single-DB mutation
still invalidates them fully.
-}
clearMethodMappingCacheForDb :: DatabaseManager -> Text -> IO ()
clearMethodMappingCacheForDb manager dbName = atomically $ do
    modifyTVar' (dmMethodMappingCache manager) (M.filterWithKey (\(dn, _) _ -> dn /= dbName))
    modifyTVar' (dmMethodTablesCache manager) (M.filterWithKey (\(dn, _) _ -> dn /= dbName))
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

{- | Initialize database manager from config
Pre-loads databases with load=true at startup
Also discovers uploaded databases from uploads/ directory
-}
initDatabaseManager :: Config -> Bool -> Maybe FilePath -> IO DatabaseManager
initDatabaseManager config noCache configPath = do
    -- Resolve relative paths against the config file's directory
    let configDir = maybe "." takeDirectory configPath
        resolveRelative p = normalise $ if isAbsolute p then p else configDir </> p

    -- Get configured databases and detect their format
    configuredDbs <- forM (cfgDatabases config) $ \dbConfig -> do
        resolvedPath <- resolveDataPath (resolveRelative (dcPath dbConfig))
        format <- Upload.detectDatabaseFormat resolvedPath
        return dbConfig{dcPath = resolvedPath, dcFormat = Just format}

    -- Discover uploaded databases from uploads/ directory (self-describing with meta.toml)
    uploadedDbs <- discoverUploadedDatabases

    -- Merge configured + uploaded
    let allDbs = configuredDbs ++ uploadedDbs

    -- Create TVars
    loadedDbsVar <- newTVarIO M.empty
    stagedDbsVar <- newTVarIO M.empty
    stagingDbsVar <- newTVarIO S.empty
    indexedDbsVar <- newTVarIO M.empty
    availableDbsVar <- newTVarIO $ M.fromList [(dcName dc, dc) | dc <- allDbs]

    -- Discover uploaded methods
    uploadedMethodConfigs <- discoverUploadedMethodConfigs
    let allMethods = cfgMethods config ++ uploadedMethodConfigs
    availableMethodsVar <- newTVarIO $ M.fromList [(mcName mc, mc) | mc <- allMethods]
    loadedMethodsVar <- newTVarIO M.empty

    -- Reference data TVars (flow synonyms, compartment mappings, units)
    -- Discover uploaded reference data from uploads/<type>/ directories
    uploadedFlowSyns <- discoverUploadedRefData "uploads/flow-synonyms"
    uploadedCompMaps <- discoverUploadedRefData "uploads/compartment-mappings"
    uploadedUnitDefs <- discoverUploadedRefData "uploads/units"
    -- Resolve reference data paths relative to config directory
    let resolveRdPath rd = rd{rdPath = resolveRelative (rdPath rd)}
    let allFlowSyns = map resolveRdPath (cfgFlowSynonyms config) ++ uploadedFlowSyns
        allCompMaps = map resolveRdPath (cfgCompartmentMappings config) ++ uploadedCompMaps
        allUnitDefs = map resolveRdPath (cfgUnits config) ++ uploadedUnitDefs
    availableFlowSynsVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allFlowSyns]
    loadedFlowSynsVar <- newTVarIO M.empty
    availableCompMapsVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allCompMaps]
    loadedCompMapsVar <- newTVarIO M.empty
    availableUnitDefsVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allUnitDefs]
    loadedUnitDefsVar <- newTVarIO M.empty

    geographies <- case cfgGeographies config of
        Nothing -> return M.empty
        Just path -> parseGeographiesCSV (resolveRelative path)

    methodMappingCacheVar <- newTVarIO M.empty
    methodTablesCacheVar <- newTVarIO M.empty
    mergedFlowMetadataCacheVar <- newTVarIO Nothing
    mergedUnitConfigCacheVar <- newTVarIO Nothing

    let manager =
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
                , dmNoCache = noCache
                , dmPlugins = buildRegistry (cfgPlugins config)
                , dmGeographies = geographies
                , dmMethodMappingCache = methodMappingCacheVar
                , dmMethodTablesCache = methodTablesCacheVar
                , dmMergedFlowMetadataCache = mergedFlowMetadataCacheVar
                , dmMergedUnitConfigCache = mergedUnitConfigCacheVar
                }

    -- Auto-load active reference data (flow synonyms, compartment mappings, units)
    -- Flow synonyms use binary cache for fast loading (161K pairs → <1s vs 15s)
    reportProgress Info $
        "Loading reference data: "
            ++ show (length allUnitDefs)
            ++ " unit config(s), paths: "
            ++ unwords (map rdPath allUnitDefs)
    autoLoadFlowSynonyms loadedFlowSynsVar allFlowSyns
    autoLoadRefData compMapOps loadedCompMapsVar allCompMaps
    autoLoadRefData unitDefOps loadedUnitDefsVar allUnitDefs

    totalStart <- getCurrentTime

    -- Load databases with level-based parallelism
    let allDbConfigs = allDbs
        configMap = M.fromList [(dcName c, c) | c <- allDbConfigs]
    case resolveLoadOrder allDbConfigs of
        Left err -> reportError $ "Dependency resolution failed: " <> T.unpack err
        Right loadOrder -> do
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            let dbsToLoad = [configMap M.! name | name <- loadOrder, M.member name configMap]
                levels = computeDepLevels configMap loadOrder
            reportProgress Info $
                "Loading "
                    ++ show (length dbsToLoad)
                    ++ " database(s) in "
                    ++ show (length levels)
                    ++ " dependency levels: "
                    ++ T.unpack (T.intercalate " → " [T.intercalate "," names | names <- levels])
            forM_ (zip [1 :: Int ..] levels) $ \(levelNum, levelNames) -> do
                let levelConfigs = [configMap M.! name | name <- levelNames, M.member name configMap]
                reportProgress Info $
                    "  Level "
                        ++ show levelNum
                        ++ ": loading "
                        ++ show (length levelConfigs)
                        ++ " database(s) in parallel"
                currentIndexedDbs <- readTVarIO indexedDbsVar
                let otherIndexes = M.elems currentIndexedDbs
                mapConcurrently_ (loadOneDatabase synonymDB unitConfig noCache otherIndexes loadedDbsVar indexedDbsVar manager) levelConfigs
            loadedCount <- atomically $ M.size <$> readTVar loadedDbsVar
            reportProgress Info $ "Multi-database mode: " ++ show loadedCount ++ " database(s) loaded"

    -- Load method collections
    let activeMethods = filter mcActive (cfgMethods config)
    forM_ activeMethods $ \mc -> do
        result <- loadMethodCollectionFromConfig mc
        case result of
            Right (collection0, flowInfo) -> do
                let scoringSets = map configToScoringSet (Config.mcScoringSets mc)
                    collection = collection0{Method.Types.mcScoringSets = scoringSets}
                atomically $ modifyTVar' loadedMethodsVar (M.insert (mcName mc) collection)
                reportProgress Info $
                    "  [OK] Loaded method: "
                        <> T.unpack (mcName mc)
                        <> " ("
                        <> show (length (mcMethods collection))
                        <> " impact categories)"
                let !pairs = extractFromILCDFlows flowInfo
                autoCreateFlowSynonyms
                    manager
                    (mcName mc)
                    ("Auto-extracted from " <> mcName mc)
                    pairs
            Left err ->
                reportError $ "  [FAIL] Failed to load method " <> T.unpack (mcName mc) <> ": " <> T.unpack err

    totalEnd <- getCurrentTime
    let totalDuration = realToFrac (diffUTCTime totalEnd totalStart) :: Double
    reportProgressWithTiming Info "Total startup loading time" totalDuration

    return manager

-- | Load a single database with per-database timing, then register it
loadOneDatabase ::
    SynonymDB ->
    UnitConversion.UnitConfig ->
    Bool ->
    [IndexedDatabase] ->
    TVar (Map Text LoadedDatabase) ->
    TVar (Map Text IndexedDatabase) ->
    DatabaseManager ->
    DatabaseConfig ->
    IO ()
loadOneDatabase synonymDB unitConfig noCache otherIndexes loadedDbsVar indexedDbsVar manager dbConfig = do
    dbStart <- getCurrentTime
    reportProgress Info $ "[STARTING] Loading database: " <> T.unpack (dcDisplayName dbConfig)
    let transforms = prTransforms (dmPlugins manager)
    result <- loadDatabaseFromConfigWithCrossDBAndTransforms transforms dbConfig synonymDB unitConfig noCache otherIndexes (M.map snd (dmGeographies manager))
    case result of
        Right loaded -> do
            let indexedDb = buildIndexedDatabaseFromDB (dcName dbConfig) synonymDB (ldDatabase loaded)
            atomically $ do
                modifyTVar' loadedDbsVar (M.insert (dcName dbConfig) loaded)
                modifyTVar' indexedDbsVar (M.insert (dcName dbConfig) indexedDb)
            dbEnd <- getCurrentTime
            let !dbDuration = realToFrac (diffUTCTime dbEnd dbStart) :: Double
            reportProgressWithTiming Info ("  [OK] Loaded: " <> T.unpack (dcDisplayName dbConfig)) dbDuration
            -- Auto-extract synonyms from biosphere flows
            let db = ldDatabase loaded
                bioUUIDs = S.fromList (V.toList (dbBiosphereFlows db))
                !pairs = extractFromEcoSpold2 (dbFlows db) bioUUIDs
                !bioFlowsWithSyns =
                    length
                        [ ()
                        | f <- M.elems (dbFlows db)
                        , S.member (flowId f) bioUUIDs
                        , not (M.null (flowSynonyms f))
                        ]
            reportProgress Info $
                "  [EXTRACT] "
                    <> T.unpack (dcName dbConfig)
                    <> ": "
                    <> show (S.size bioUUIDs)
                    <> " bio flows, "
                    <> show bioFlowsWithSyns
                    <> " with synonyms, "
                    <> show (length pairs)
                    <> " pairs"
            autoCreateFlowSynonyms
                manager
                (dcName dbConfig)
                ("Auto-extracted from " <> dcDisplayName dbConfig)
                pairs
        Left err ->
            reportError $ "  [FAIL] Failed to load " <> T.unpack (dcName dbConfig) <> ": " <> T.unpack err

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
        , dcPath = dirPath </> UploadedDB.umDataPath meta -- Full path to data
        , dcDescription = UploadedDB.umDescription meta
        , dcLoad = False -- Never auto-load uploads
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Just (UploadedDB.umFormat meta)
        , dcIsUploaded = True -- Discovered from uploads/ directory
        , dcDeletable = True
        }

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
        , ssNormalization = sscNormalization ssc
        , ssWeighting = sscWeighting ssc
        , ssScores = sscScores ssc
        , ssDisplayMultiplier = sscDisplayMultiplier ssc
        }

-- | Convert a DatabaseFormat to display text for methods
methodFormatText :: DatabaseFormat -> Text
methodFormatText SimaProCSV = "SimaPro CSV"
methodFormatText ILCDProcess = "ILCD"
methodFormatText f = T.pack (show f)

discoverUploadedMethodConfigs :: IO [MethodConfig]
discoverUploadedMethodConfigs = do
    uploads <- UploadedDB.discoverUploadedMethods
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded method: " <> T.unpack slug
        -- Find the actual method XML directory (e.g., ILCD/lciamethods/)
        methodDir <- findMethodDirectory dirPath
        return
            MethodConfig
                { mcName = UploadedDB.umDisplayName meta
                , mcPath = methodDir
                , mcActive = False -- Never auto-load uploaded methods
                , mcIsUploaded = True
                , mcDescription = UploadedDB.umDescription meta
                , mcFormat = Just $ methodFormatText (UploadedDB.umFormat meta)
                , mcScoringSets = []
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
                }

{- | Load a database from its configuration (without cross-DB linking)
This is the original function, kept for backward compatibility
-}
loadDatabaseFromConfig :: DatabaseConfig -> SynonymDB -> Bool -> IO (Either Text LoadedDatabase)
loadDatabaseFromConfig dbConfig synonymDB noCache =
    loadDatabaseFromConfigWithCrossDB dbConfig synonymDB UnitConversion.defaultUnitConfig noCache [] M.empty

{- | Resolve a database path: if it's an archive, extract it first.
Extracts to "{archivePath}.d/" and finds the actual data directory inside.
Plain files/directories pass through unchanged.
-}
resolveDataPath :: FilePath -> IO FilePath
resolveDataPath path = do
    isDir <- doesDirectoryExist path
    if isDir
        then return path
        else do
            isFile <- doesFileExist path
            if not isFile
                then return path -- missing: let caller handle
                else
                    let ext = map toLower (takeExtension path)
                     in if ext `elem` [".zip", ".7z", ".gz", ".xz"]
                            then extractAndFind path
                            else return path
  where
    extractAndFind archive = do
        let extractDir = archive ++ ".d"
        dirExists <- doesDirectoryExist extractDir
        alreadyExtracted <-
            if dirExists
                then not . null <$> listDirectory extractDir
                else return False
        if alreadyExtracted
            then do
                reportProgress Info $ "Using cached extraction: " <> extractDir
                Upload.findDataDirectory extractDir
            else do
                createDirectoryIfMissing True extractDir
                reportProgress Info $ "Extracting archive: " <> archive
                result <- Upload.extractArchiveFile archive extractDir
                case result of
                    Left err -> do
                        reportError $ "Archive extraction failed: " <> T.unpack err
                        return archive -- let caller report the meaningful error
                    Right () -> do
                        reportProgress Info "Extraction complete"
                        Upload.findDataDirectory extractDir

-- | Load a database from its configuration with cross-database linking support
loadDatabaseFromConfigWithCrossDB ::
    DatabaseConfig ->
    SynonymDB ->
    UnitConversion.UnitConfig ->
    Bool -> -- noCache
    [IndexedDatabase] -> -- Pre-built indexes from other databases for cross-DB linking
    M.Map T.Text [T.Text] -> -- Location hierarchy (empty = use built-in)
    IO (Either Text LoadedDatabase)
loadDatabaseFromConfigWithCrossDB = loadDatabaseFromConfigWithCrossDBAndTransforms []

-- | Load with optional transform pipeline
loadDatabaseFromConfigWithCrossDBAndTransforms ::
    [TransformHandle] ->
    DatabaseConfig ->
    SynonymDB ->
    UnitConversion.UnitConfig ->
    Bool -> -- noCache
    [IndexedDatabase] -> -- Pre-built indexes from other databases for cross-DB linking
    M.Map T.Text [T.Text] -> -- Location hierarchy (empty = use built-in)
    IO (Either Text LoadedDatabase)
loadDatabaseFromConfigWithCrossDBAndTransforms transforms dbConfig synonymDB unitConfig noCache otherIndexes locationHier = do
    let sourcePath = dcPath dbConfig
        locationAliases = dcLocationAliases dbConfig
    reportProgress Info $ "Loading database from: " <> sourcePath
    dbResult <- loadDatabaseRawWithCrossDB (dcName dbConfig) locationAliases sourcePath noCache synonymDB unitConfig otherIndexes locationHier

    case dbResult of
        Left err -> return $ Left err
        Right dbRaw -> do
            -- Apply transform plugins (sorted by priority) before runtime init
            transformed <- applyTransforms transforms (toSimpleDatabase dbRaw)
            dbRebuiltResult <-
                if null transforms
                    then pure (Right dbRaw)
                    else buildDatabaseWithMatrices unitConfig (sdbActivities transformed) (sdbFlows transformed) (sdbUnits transformed)

            case dbRebuiltResult of
                Left err -> return $ Left err
                Right dbRebuilt -> do
                    -- Initialize runtime fields (synonym DB and flow name index)
                    let database = BM25.addBM25Index (initializeRuntimeFields dbRebuilt synonymDB)

                    -- Create shared solver with lazy factorization (deferred to first query)
                    let techTriples = dbTechnosphereTriples database
                        activityCount = dbActivityCount database
                        techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                        activityCountInt = fromIntegral activityCount
                    sharedSolver <- createSharedSolver (dcName dbConfig) techTriplesInt activityCountInt

                    return $
                        Right
                            LoadedDatabase
                                { ldDatabase = database
                                , ldSharedSolver = sharedSolver
                                , ldConfig = dbConfig
                                }

-- | Apply transform plugins sequentially (sorted by priority)
applyTransforms :: [TransformHandle] -> SimpleDatabase -> IO SimpleDatabase
applyTransforms [] db = pure db
applyTransforms (t : ts) db = do
    result <- thTransform t (TransformContext db M.empty)
    mapM_ (reportProgress Info . T.unpack) (trLog result)
    applyTransforms ts (trDatabase result)

-- | Detected format of a database directory
data DirectoryFormat = FormatSpold | FormatXML | FormatCSV | FormatILCD | FormatUnknown
    deriving (Show, Eq)

-- | Detect the format of files in a directory
detectDirectoryFormat :: FilePath -> IO DirectoryFormat
detectDirectoryFormat path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isFile
        then do
            -- Direct file: check extension
            let ext = map toLower (takeExtension path)
            return $ case ext of
                ".csv" -> FormatCSV
                ".spold" -> FormatSpold
                ".xml" -> FormatXML
                _ -> FormatUnknown
        else
            if isDir
                then do
                    -- Check for ILCD format first (has processes/ subdirectory)
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then return FormatILCD
                        else do
                            files <- listDirectory path
                            let extensions = map (map toLower . takeExtension) files
                            -- Check for different formats (in order of preference)
                            if elem ".spold" extensions
                                then return FormatSpold
                                else
                                    if elem ".csv" extensions
                                        then return FormatCSV
                                        else
                                            if elem ".xml" extensions
                                                then return FormatXML
                                                else return FormatUnknown
                else return FormatUnknown

-- | Find CSV files in a directory
findCSVFiles :: FilePath -> IO [FilePath]
findCSVFiles path = do
    files <- listDirectory path
    let csvFiles = filter (\f -> map toLower (takeExtension f) == ".csv") files
    return $ map (path </>) csvFiles

{- | Build activity map from list of activities
Creates (activityUUID, productUUID) -> Activity mapping
-}
buildActivityMap :: [Activity] -> M.Map (UUID, UUID) Activity
buildActivityMap activities =
    M.fromList
        [ ((activityUUID, productUUID), activity)
        | activity <- activities
        , let activityUUID = SimaPro.generateActivityUUID activity
        , let refExchanges = filter exchangeIsReference (exchanges activity)
        , refExchange <- take 1 refExchanges -- Take first reference product
        , let productUUID = exchangeFlowId refExchange
        ]

{- | Load raw database from a configured source path, with cross-database linking.

The cache lives next to @sourcePath@ (see 'Loader.generateMatrixCacheFilename').
We probe it first using the unresolved @sourcePath@, so a deployment that ships
only the cache (no source archive on disk) still loads. On cache miss/stale we
'resolveDataPath' and parse, saving a fresh cache on success.
-}
loadDatabaseRawWithCrossDB ::
    -- | Database name
    T.Text ->
    -- | Location aliases
    M.Map T.Text T.Text ->
    -- | Source path (unresolved; cache is co-located with it)
    FilePath ->
    -- | noCache flag
    Bool ->
    -- | Synonym database
    SynonymDB ->
    -- | Unit configuration
    UnitConversion.UnitConfig ->
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Location hierarchy (empty = use built-in)
    M.Map T.Text [T.Text] ->
    IO (Either Text Database)
loadDatabaseRawWithCrossDB dbName locationAliases sourcePath noCache synonymDB unitConfig otherIndexes locationHier = do
    mCachedDb <-
        if noCache
            then return Nothing
            else Loader.loadCachedDatabaseWithMatrices dbName sourcePath
    let cacheUsable = case mCachedDb of
            Just db
                | unresolvedCount (dbLinkingStats db) > 0
                , not (null otherIndexes) ->
                    False -- stale: deps now available
            Just _ -> True
            Nothing -> False
    case (cacheUsable, mCachedDb) of
        (True, Just db) -> do
            Loader.reportCrossDBLinkingStats (fromIntegral (dbActivityCount db)) (dbLinkingStats db)
            return $ Right db
        _ -> do
            when (isJust mCachedDb && not cacheUsable) $
                reportProgress Info "Cache has unresolved links, rebuilding with available dependencies..."
            -- Cache miss/stale: now we need the source. Resolve archive if any.
            path <- resolveDataPath sourcePath
            isFile <- doesFileExist path
            isDir <- doesDirectoryExist path
            if not isFile && not isDir
                then return $ Left $ "Source path does not exist: " <> T.pack sourcePath
                else do
                    format <- detectDirectoryFormat path
                    case format of
                        FormatCSV -> loadCSV path
                        FormatUnknown ->
                            return $
                                Left $
                                    "No supported database files found in: "
                                        <> T.pack path
                                        <> ". Supported formats: EcoSpold v2 (.spold), EcoSpold v1 (.xml), SimaPro CSV (.csv), ILCD"
                        _ -> loadStructured path
  where
    loadCSV path = do
        mCsvFile <-
            doesFileExist path >>= \isFileCheck ->
                if isFileCheck
                    then return (Right path)
                    else do
                        csvFiles <- findCSVFiles path
                        case csvFiles of
                            [] -> return $ Left $ "No CSV files found in: " <> T.pack path
                            (f : _) -> return (Right f)
        case mCsvFile of
            Left err -> return $ Left err
            Right csvFile -> do
                reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV unitConfig csvFile
                reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                let simpleDb = SimpleDatabase (buildActivityMap activities) flowDB unitDB
                linkedDb <- Loader.fixSimaProActivityLinks simpleDb
                dbResult <- buildDatabaseWithMatrices unitConfig (sdbActivities linkedDb) flowDB unitDB
                case dbResult of
                    Left err -> return $ Left err
                    Right db -> do
                        unless noCache $
                            Loader.saveCachedDatabaseWithMatrices dbName sourcePath db
                        Loader.reportCrossDBLinkingStats (fromIntegral (dbActivityCount db)) (dbLinkingStats db)
                        return $ Right db

    loadStructured path = do
        loadResult <-
            Loader.loadDatabaseWithCrossDBLinking
                locationAliases
                otherIndexes
                synonymDB
                unitConfig
                locationHier
                path
        case loadResult of
            Left err -> return $ Left err
            Right (simpleDb, stats) -> do
                dbResult <-
                    buildDatabaseWithMatrices
                        unitConfig
                        (sdbActivities simpleDb)
                        (sdbFlows simpleDb)
                        (sdbUnits simpleDb)
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
                        unless noCache $
                            Loader.saveCachedDatabaseWithMatrices dbName sourcePath dbWithLinks
                        return $ Right dbWithLinks

-- | Load a single database without auto-loading dependencies
loadDatabaseSingle :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabaseSingle manager dbName = do
    -- Check if already staged -> try to finalize, or clear stale staged entry
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged -> do
            -- Check if the staged database can be finalized
            let unlinked = Loader.countUnlinkedExchanges (sdSimpleDB staged)
                crossDBLinks' = Loader.crossDBLinksCount (sdLinkingStats staged)
                unresolvedLinks = max 0 (unlinked - crossDBLinks')
            if unresolvedLinks == 0
                then finalizeDatabase manager dbName
                else do
                    -- Cannot finalize: clear staged entry, reload from config
                    -- (loadDatabase pre-loaded deps, so fresh load should resolve links)
                    atomically $ modifyTVar' (dmStagedDbs manager) (M.delete dbName)
                    loadDatabaseSingleFromConfig manager dbName
        Nothing -> loadDatabaseSingleFromConfig manager dbName

-- | Load a database from config (not staged)
loadDatabaseSingleFromConfig :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabaseSingleFromConfig manager dbName = do
    -- Check if already loaded
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Just loaded -> return $ Right loaded
        Nothing -> do
            -- Check if it's configured
            availableDbs <- readTVarIO (dmAvailableDbs manager)
            case M.lookup dbName availableDbs of
                Nothing -> return $ Left $ "Database not found: " <> dbName
                Just dbConfig -> do
                    reportProgress Info $ "[STARTING] Loading database: " <> T.unpack (dcDisplayName dbConfig)
                    -- Get currently loaded IndexedDatabases for cross-DB linking
                    currentIndexedDbs <- readTVarIO (dmIndexedDbs manager)
                    let otherIndexes = M.elems currentIndexedDbs
                    synonymDB <- getMergedSynonymDB manager
                    unitConfig <- getMergedUnitConfig manager
                    let transforms = prTransforms (dmPlugins manager)
                    eitherResult <-
                        try $
                            loadDatabaseFromConfigWithCrossDBAndTransforms
                                transforms
                                dbConfig
                                synonymDB
                                unitConfig
                                (dmNoCache manager)
                                otherIndexes
                                (M.map snd (dmGeographies manager))
                    case eitherResult of
                        Left (ex :: SomeException) -> return $ Left $ "Exception loading database: " <> T.pack (show ex)
                        Right (Left err) -> return $ Left err
                        Right (Right loaded) -> do
                            let indexedDb = buildIndexedDatabaseFromDB dbName synonymDB (ldDatabase loaded)
                            atomically $ do
                                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                                modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
                            clearMethodMappingCacheForDb manager dbName
                            reportProgress Info $ "  [OK] Loaded:" <> T.unpack (dcDisplayName dbConfig)
                            -- Auto-extract synonyms from biosphere flows
                            let db = ldDatabase loaded
                                bioUUIDs = S.fromList (V.toList (dbBiosphereFlows db))
                                pairs = extractFromEcoSpold2 (dbFlows db) bioUUIDs
                            autoCreateFlowSynonyms
                                manager
                                dbName
                                ("Auto-extracted from " <> dcDisplayName dbConfig)
                                pairs
                            relinkDependents manager dbName
                            return $ Right loaded

-- | Result of a relink operation (unresolved counts before/after).
data RelinkResult = RelinkResult
    { rresDbName :: !Text
    , rresUnresolvedBefore :: !Int
    , rresUnresolvedAfter :: !Int
    , rresCrossDBLinks :: !Int
    , rresDepsLoaded :: ![Text]
    }
    deriving (Show, Eq)

{- | Re-run cross-DB linking for an already-loaded DB against the current set
of loaded dep DBs. Updates 'dbCrossDBLinks' and 'dbLinkingStats' in place
in the LoadedDatabase record. Does NOT rebuild the technosphere matrix or
invalidate the MUMPS factorization — cross-DB links are consumed only at
solve time.
-}
relinkDatabase :: DatabaseManager -> Text -> IO (Either Text RelinkResult)
relinkDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Nothing -> return $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            let otherIndexes = [idb | (n, idb) <- M.toList indexedDbs, n /= dbName]
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            let db = ldDatabase loaded
                beforeUnresolved = unresolvedCount (dbLinkingStats db)
                activityMap =
                    M.fromList
                        [ (dbProcessIdTable db V.! i, dbActivities db V.! i)
                        | i <- [0 .. V.length (dbActivities db) - 1]
                        ]
                ctx =
                    LinkingContext
                        { lcIndexedDatabases = otherIndexes
                        , lcSynonymDB = synonymDB
                        , lcUnitConfig = unitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = M.map snd (dmGeographies manager)
                        }
                !totalInputs = Loader.countTotalTechInputs (toSimpleDatabase db)
                rawStats =
                    Loader.findAllCrossDBLinks
                        ctx
                        (dbFlows db)
                        (dbUnits db)
                        activityMap
                newStats = rawStats{cdlTotalInputs = totalInputs}
                newLinks = cdlLinks newStats
                newDeps = M.keys (crossDBBySource newStats)
                !db' =
                    db
                        { dbCrossDBLinks = newLinks
                        , dbDependsOn = newDeps
                        , dbLinkingStats = newStats
                        }
                !loaded' = loaded{ldDatabase = db'}
                afterUnresolved = unresolvedCount newStats
            atomically $ do
                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded')
                modifyTVar'
                    (dmIndexedDbs manager)
                    (M.insert dbName (buildIndexedDatabaseFromDB dbName synonymDB db'))
            clearMethodMappingCacheForDb manager dbName
            reportProgress Info $
                "Re-linked "
                    <> T.unpack dbName
                    <> ": "
                    <> show beforeUnresolved
                    <> " \8594 "
                    <> show afterUnresolved
                    <> " unresolved products ("
                    <> show (length newLinks)
                    <> " cross-DB links)"
            return $
                Right
                    RelinkResult
                        { rresDbName = dbName
                        , rresUnresolvedBefore = beforeUnresolved
                        , rresUnresolvedAfter = afterUnresolved
                        , rresCrossDBLinks = length newLinks
                        , rresDepsLoaded = newDeps
                        }

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
loadDatabase manager dbName = do
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
            return (Right (loaded, depResults1 ++ depResults2))

{- | Stage an uploaded database (parse + cross-DB link, no matrices yet)
When a valid cache exists, reconstructs staged state from the cached Database
without re-parsing, turning a ~90s operation into ~7s.
-}
stageUploadedDatabase :: DatabaseManager -> DatabaseConfig -> IO (Either Text ())
stageUploadedDatabase manager dbConfig = do
    let dbName = dcName dbConfig
    reportProgress Info $ "[STARTING] Staging: " <> T.unpack (dcDisplayName dbConfig)

    -- Try cache first: if valid, reconstruct StagedDatabase without re-parsing
    mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName (dcPath dbConfig)

    case mCachedDb of
        Just cachedDb -> do
            -- Cache hit: auto-load dependencies so cross-DB solving works
            _ <- autoLoadDeps manager (dbDependsOn cachedDb)
            -- Recompute unknownUnits against the current unitConfig (cache may be stale)
            unitConfig <- getMergedUnitConfig manager
            let simpleDb = toSimpleDatabase cachedDb
                freshUnknownUnits =
                    S.fromList
                        [ unitName u
                        | u <- M.elems (sdbUnits simpleDb)
                        , not (UnitConversion.isKnownUnit unitConfig (unitName u))
                        , not (T.null (unitName u))
                        ]
                freshStats =
                    (dbLinkingStats cachedDb)
                        { cdlUnknownUnits = freshUnknownUnits
                        }
                staged =
                    StagedDatabase
                        { sdSimpleDB = simpleDb
                        , sdConfig = dbConfig
                        , sdUnlinkedCount = 0 -- was finalized successfully
                        , sdMissingProducts = []
                        , sdSelectedDeps = dbDependsOn cachedDb
                        , sdCrossDBLinks = dbCrossDBLinks cachedDb
                        , sdLinkingStats = freshStats
                        , sdCachedDB = Just cachedDb
                        }
            atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
            reportProgress Info $ "  [OK] Staged from cache: " <> T.unpack (dcDisplayName dbConfig)
            return $ Right ()
        Nothing -> do
            -- Cache miss: parse and cross-DB link as before
            let locationAliases = dcLocationAliases dbConfig

            -- Resolve nested directory structure (e.g. ZIP extracts with multiple subdirs)
            path <- Upload.findDataDirectory (dcPath dbConfig)

            -- Look up indexes for cross-DB linking
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            let otherIndexes = M.elems indexedDbs

            -- Detect format to find the correct file path (CSV needs file, not directory)
            format <- detectDirectoryFormat path
            loadPath <- case format of
                FormatCSV -> do
                    isFile <- doesFileExist path
                    if isFile
                        then return path
                        else do
                            csvFiles <- findCSVFiles path
                            case csvFiles of
                                [] -> return path -- let loader produce the error
                                (f : _) -> return f
                _ -> return path

            -- Parse and run cross-DB linking (but don't build matrices)
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            loadResult <-
                Loader.loadDatabaseWithCrossDBLinking
                    locationAliases
                    otherIndexes
                    synonymDB
                    unitConfig
                    (M.map snd (dmGeographies manager))
                    loadPath

            case loadResult of
                Left err -> return $ Left err
                Right (simpleDb, stats) -> do
                    -- Create staged database
                    let fromStats = [(name, cnt, blocker) | (name, (cnt, blocker)) <- M.toList (Loader.cdlUnresolvedProducts stats)]
                        fromScan =
                            if null fromStats && Loader.crossDBLinksCount stats == 0
                                then [(name, cnt, NoNameMatch) | (name, cnt) <- M.toList (Loader.collectUnlinkedProductNames simpleDb)]
                                else fromStats
                        staged =
                            StagedDatabase
                                { sdSimpleDB = simpleDb
                                , sdConfig = dbConfig
                                , sdUnlinkedCount = Loader.unresolvedCount stats
                                , sdMissingProducts = sortOn (\(_, cnt, _) -> Down cnt) fromScan
                                , sdSelectedDeps = nub $ M.keys (Loader.crossDBBySource stats)
                                , sdCrossDBLinks = Loader.cdlLinks stats
                                , sdLinkingStats = stats
                                , sdCachedDB = Nothing
                                }

                    -- Store in staged map
                    atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
                    reportProgress Info $ "  [OK] Staged: " <> T.unpack (dcDisplayName dbConfig)
                    return $ Right ()

{- | Unload a database from memory (keeps config for reloading).
Refuses to unload if any currently-loaded database declares this one as a
dependency — unloading would leave the dependent's cross-DB links dangling.
-}
unloadDatabase :: DatabaseManager -> Text -> IO (Either Text ())
unloadDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    case M.lookup dbName loadedDbs of
        Nothing -> return $ Left $ "Database not loaded: " <> dbName
        Just _ -> do
            let dependents =
                    [ name
                    | (name, ld) <- M.toList loadedDbs
                    , name /= dbName
                    , dbName `elem` dbDependsOn (ldDatabase ld)
                    ]
            if not (null dependents)
                then
                    return $
                        Left $
                            "Cannot unload "
                                <> dbName
                                <> ": still required by "
                                <> T.intercalate ", " dependents
                                <> ". Unload dependents first."
                else do
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
        Just dbConfig -> do
            -- Honor the per-config deletable policy (defaults to dcIsUploaded).
            if not (dcDeletable dbConfig)
                then return $ Left "Cannot delete configured database. Edit volca.toml to remove it."
                else
                    if M.member dbName loadedDbs
                        then return $ Left "Cannot delete loaded database. Close it first."
                        else do
                            -- Get the upload directory (uploads/<slug>/)
                            uploadsDir <- UploadedDB.getDatabaseUploadsDir
                            let uploadDir = uploadsDir </> T.unpack dbName
                            pathExists <- doesDirectoryExist uploadDir
                            if pathExists
                                then do
                                    -- Delete the database directory immediately
                                    result <- tryIO $ removeDirectoryRecursive uploadDir
                                    case result of
                                        Left (e :: SomeException) ->
                                            return $ Left $ "Failed to delete: " <> T.pack (show e)
                                        Right () -> do
                                            reportProgress Info $ "Deleted: " <> uploadDir
                                            deleteCacheFile dbName (dcPath dbConfig)
                                            removeFromMemory manager dbName
                                else do
                                    -- Directory already missing, just remove from memory
                                    reportProgress Info $ "Directory already missing: " <> uploadDir
                                    removeFromMemory manager dbName
  where
    tryIO :: IO a -> IO (Either SomeException a)
    tryIO = Control.Exception.try
    deleteCacheFile name sourcePath = do
        cacheFile <- Loader.generateMatrixCacheFilename name sourcePath
        let zstdFile = cacheFile ++ ".zst"
        cacheExists <- doesFileExist zstdFile
        when cacheExists $ do
            removeFile zstdFile
            reportProgress Info $ "Deleted cache: " ++ zstdFile

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

data StageAction = AlreadyDone | NeedToStage

{- | Get setup info for a database (for the setup page)
Works for both staged and loaded databases
Auto-stages uploaded databases if they're not yet staged
Uses STM to prevent concurrent staging of the same database
-}
getDatabaseSetupInfo :: DatabaseManager -> Text -> IO (Either SetupError DatabaseSetupInfo)
getDatabaseSetupInfo manager dbName = do
    -- Atomic decision: already staged? already staging? need to stage?
    action <- atomically $ do
        stagedDbs <- readTVar (dmStagedDbs manager)
        loadedDbs <- readTVar (dmLoadedDbs manager)
        stagingDbs <- readTVar (dmStagingDbs manager)
        case M.lookup dbName stagedDbs of
            Just _ -> return $ Right AlreadyDone
            Nothing -> case M.lookup dbName loadedDbs of
                Just _ -> return $ Right AlreadyDone
                Nothing ->
                    if S.member dbName stagingDbs
                        then retry -- another thread is staging; STM blocks until done
                        else do
                            availableDbs <- readTVar (dmAvailableDbs manager)
                            case M.lookup dbName availableDbs of
                                Nothing -> return $ Left $ SetupNotFound $ "Database not found: " <> dbName
                                Just dbConfig
                                    | dcIsUploaded dbConfig -> do
                                        modifyTVar' (dmStagingDbs manager) (S.insert dbName)
                                        return $ Right NeedToStage
                                    | otherwise ->
                                        return $ Left $ SetupFailed $ "Database is not loaded. Use the Load button to load it first: " <> dbName

    case action of
        Left err -> return $ Left err
        Right AlreadyDone -> buildSetupResult manager dbName
        Right NeedToStage -> do
            -- Do the slow work, ensuring we always unmark on exception
            availableDbs <- readTVarIO (dmAvailableDbs manager)
            let dbConfig = availableDbs M.! dbName -- safe: checked above
            stageResult <-
                Control.Exception.finally
                    (stageUploadedDatabase manager dbConfig)
                    (atomically $ modifyTVar' (dmStagingDbs manager) (S.delete dbName))
            case stageResult of
                Left err -> do
                    reportProgress Error $ "Setup staging failed for " <> T.unpack dbName <> ": " <> T.unpack err
                    return $ Left $ SetupFailed err
                Right () -> buildSetupResult manager dbName

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
                let info = buildLoadedSetupInfo (ldConfig loaded) (ldDatabase loaded)
                    suggestions = buildDependencySuggestions' availableDbs indexedDbs
                    nUnresolved = unresolvedCount (dbLinkingStats (ldDatabase loaded))
                 in return $
                        Right
                            info
                                { dsiIsLoaded = False
                                , dsiIsReady = nUnresolved == 0
                                , dsiSuggestions = suggestions
                                }
            Nothing -> return $ Left $ SetupFailed $ "Failed to stage database: " <> dbName

{- | Build setup info from a staged database
dataPath and availablePaths are filled in by buildSetupResult (requires IO)
-}
buildStagedSetupInfo :: StagedDatabase -> Map Text DatabaseConfig -> Map Text IndexedDatabase -> DatabaseSetupInfo
buildStagedSetupInfo staged configs indexedDbs =
    let stats = sdLinkingStats staged
        -- Count from the actual database, not from cross-DB linking stats
        totalInputs = Loader.countTotalTechInputs (sdSimpleDB staged)
        unlinked = Loader.countUnlinkedExchanges (sdSimpleDB staged)
        crossDBLinks = Loader.crossDBLinksCount stats
        internalLinks = totalInputs - unlinked
        unresolvedLinks = max 0 (unlinked - crossDBLinks)
        resolved = internalLinks + crossDBLinks
        completeness =
            if totalInputs > 0
                then 100.0 * fromIntegral resolved / fromIntegral totalInputs
                else 100.0
        -- Convert missing products to MissingSupplier with reason/detail
        missingSuppliers = take 10 $ map blockerToMissingSupplier (sdMissingProducts staged)
        blockerToMissingSupplier (name, cnt, blocker) =
            let (reason, detail) = case blocker of
                    NoNameMatch -> ("no_name_match", Nothing)
                    UnitIncompatible q s -> ("unit_incompatible", Just (q <> " vs " <> s))
                    LocationUnavailable loc -> ("location_unavailable", Just loc)
             in MissingSupplier name cnt Nothing reason detail
        -- Build suggestions from available databases
        suggestions = buildDependencySuggestions staged configs indexedDbs
        -- Database is ready only when it has activities and all inputs are resolved
        activityCount = M.size (sdbActivities (sdSimpleDB staged))
        isReady = activityCount > 0 && unresolvedLinks == 0
     in DatabaseSetupInfo
            { dsiName = dcName (sdConfig staged)
            , dsiDisplayName = dcDisplayName (sdConfig staged)
            , dsiActivityCount = activityCount
            , dsiInputCount = totalInputs
            , dsiCompleteness = completeness
            , dsiInternalLinks = internalLinks
            , dsiCrossDBLinks = crossDBLinks
            , dsiUnresolvedLinks = unresolvedLinks
            , dsiMissingSuppliers = missingSuppliers
            , dsiSelectedDeps = sdSelectedDeps staged
            , dsiSuggestions = suggestions
            , dsiIsReady = isReady
            , dsiUnknownUnits = S.toList (cdlUnknownUnits stats)
            , dsiLocationFallbacks = deduplicateFallbacks (cdlLocationFallbacks stats)
            , dsiDataPath = T.pack (dcPath (sdConfig staged))
            , dsiAvailablePaths = [] -- Filled in by buildSetupResult (requires IO)
            , dsiIsLoaded = False
            }

{- | Build setup info from a loaded database (already finalized)
Uses dbLinkingStats for real completeness/fallback data
-}
buildLoadedSetupInfo :: DatabaseConfig -> Database -> DatabaseSetupInfo
buildLoadedSetupInfo config db =
    let stats = dbLinkingStats db
        totalInputs = cdlTotalInputs stats
        nCrossDBLinks = length (dbCrossDBLinks db)
        nUnresolved = unresolvedCount stats
        resolved = totalInputs - nUnresolved
        completeness =
            if totalInputs > 0
                then 100.0 * fromIntegral resolved / fromIntegral totalInputs
                else 100.0
        internalLinks = max 0 (resolved - nCrossDBLinks)
        missingSuppliers = take 10 $ map blockerToMissingSupplier (M.toList (cdlUnresolvedProducts stats))
        blockerToMissingSupplier (name, (cnt, blocker)) =
            let (reason, detail) = case blocker of
                    NoNameMatch -> ("no_name_match", Nothing)
                    UnitIncompatible q s -> ("unit_incompatible", Just (q <> " vs " <> s))
                    LocationUnavailable loc -> ("location_unavailable", Just loc)
             in MissingSupplier name cnt Nothing reason detail
     in DatabaseSetupInfo
            { dsiName = dcName config
            , dsiDisplayName = dcDisplayName config
            , dsiActivityCount = fromIntegral (dbActivityCount db)
            , dsiInputCount = totalInputs
            , dsiCompleteness = completeness
            , dsiInternalLinks = internalLinks
            , dsiCrossDBLinks = nCrossDBLinks
            , dsiUnresolvedLinks = nUnresolved
            , dsiMissingSuppliers = missingSuppliers
            , dsiSelectedDeps = dbDependsOn db
            , dsiSuggestions = [] -- No suggestions for loaded databases
            , dsiIsReady = True
            , dsiUnknownUnits = S.toList (cdlUnknownUnits stats)
            , dsiLocationFallbacks = deduplicateFallbacks (cdlLocationFallbacks stats)
            , dsiDataPath = T.pack (dcPath config)
            , dsiAvailablePaths = [] -- No picker for loaded/configured databases
            , dsiIsLoaded = True
            }

{- | Discover candidate data paths within an uploaded database's root directory.
Returns (relativePath, formatLabel, fileCount) for each candidate.
-}
discoverCandidatePaths :: DatabaseConfig -> IO [(Text, Text, Int)]
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
                Upload.UnknownFormat -> "Unknown"
        return (T.pack rel, label, count)
  where
    -- Simple relative path: strip upload root prefix
    makeRelativePath base path
        | base `isPrefixOf` path =
            let r = drop (length base + 1) path
             in if null r then "." else r
        | otherwise = path

{- | Change the data path for an uploaded (staged) database.
Validates path, updates config + meta.toml, clears staged DB to force re-stage.
-}
setDataPath :: DatabaseManager -> Text -> Text -> IO (Either Text DatabaseSetupInfo)
setDataPath manager dbName newRelPath = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    case M.lookup dbName availableDbs of
        Nothing -> return $ Left $ "Database not found: " <> dbName
        Just dbConfig
            | not (dcIsUploaded dbConfig) ->
                return $ Left "Cannot change data path for configured databases"
            | otherwise -> do
                -- Resolve full path
                uploadsDir <- UploadedDB.getDatabaseUploadsDir
                let uploadRoot = uploadsDir </> T.unpack dbName
                    newFullPath = uploadRoot </> T.unpack newRelPath

                -- Validate that path exists and has data
                hasData <- Upload.anyDataFilesIn newFullPath
                if not hasData
                    then return $ Left $ "No data files found in: " <> newRelPath
                    else do
                        -- Detect format for the new path
                        newFormat <- Upload.detectDatabaseFormat newFullPath

                        -- Update config
                        let updatedConfig =
                                dbConfig
                                    { dcPath = newFullPath
                                    , dcFormat = Just newFormat
                                    }
                        atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert dbName updatedConfig)

                        -- Update meta.toml
                        mMeta <- UploadedDB.readUploadMeta uploadRoot
                        case mMeta of
                            Just meta ->
                                UploadedDB.writeUploadMeta
                                    uploadRoot
                                    meta
                                        { UploadedDB.umDataPath = T.unpack newRelPath
                                        , UploadedDB.umFormat = newFormat
                                        }
                            Nothing -> return ()

                        -- Clear staged DB to force re-staging with new path
                        atomically $ modifyTVar' (dmStagedDbs manager) (M.delete dbName)

                        -- Re-stage and return fresh setup info
                        result <- getDatabaseSetupInfo manager dbName
                        case result of
                            Left err -> return $ Left $ setupErrorMessage err
                            Right info -> return $ Right info

-- | Build dependency suggestions from available indexed databases
buildDependencySuggestions' :: Map Text DatabaseConfig -> Map Text IndexedDatabase -> [DependencySuggestion]
buildDependencySuggestions' configs indexedDbs =
    [ DependencySuggestion
        { dsgDatabaseName = name
        , dsgDisplayName = maybe name dcDisplayName (M.lookup name configs)
        , dsgMatchCount = M.size (Database.CrossLinking.idbByProductName idx)
        }
    | (name, idx) <- M.toList indexedDbs
    ]

-- | Build dependency suggestions for a staged database
buildDependencySuggestions :: StagedDatabase -> Map Text DatabaseConfig -> Map Text IndexedDatabase -> [DependencySuggestion]
buildDependencySuggestions _staged = buildDependencySuggestions'

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
                , sdUnlinkedCount = unresolvedCount stats
                , sdMissingProducts =
                    sortOn
                        (\(_, cnt, _) -> Down cnt)
                        [(n, cnt, blocker) | (n, (cnt, blocker)) <- M.toList (cdlUnresolvedProducts stats)]
                , sdSelectedDeps = dbDependsOn db
                , sdCrossDBLinks = dbCrossDBLinks db
                , sdLinkingStats = stats
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

{- | Add a dependency to a staged (or partially-linked loaded) database
Runs cross-DB linking against the new dependency
-}
addDependencyToStaged :: DatabaseManager -> Text -> Text -> IO (Either Text DatabaseSetupInfo)
addDependencyToStaged manager dbName depName = do
    indexedDbs <- readTVarIO (dmIndexedDbs manager)
    stagedResult <- getOrStageDatabase manager dbName

    case stagedResult of
        Left err -> return $ Left err
        Right staged -> case M.lookup depName indexedDbs of
            Nothing -> return $ Left $ "Dependency database not loaded: " <> depName
            Just _depIdx -> do
                -- Compute new dependency list, then link only against selected deps
                let newDeps =
                        if depName `elem` sdSelectedDeps staged
                            then sdSelectedDeps staged
                            else depName : sdSelectedDeps staged
                    selectedIndexes = [idx | (name, idx) <- M.toList indexedDbs, name `elem` newDeps]
                synonymDB <- getMergedSynonymDB manager
                unitConfig <- getMergedUnitConfig manager
                (_, newStats) <-
                    Loader.fixActivityLinksWithCrossDB
                        selectedIndexes
                        synonymDB
                        unitConfig
                        (M.map snd (dmGeographies manager))
                        (sdSimpleDB staged)

                -- Update staged database with new stats and dependency
                let updatedStaged =
                        staged
                            { sdSelectedDeps = newDeps
                            , sdCrossDBLinks = Loader.cdlLinks newStats
                            , sdLinkingStats = newStats
                            , sdMissingProducts = sortOn (\(_, cnt, _) -> Down cnt) [(name, cnt, blocker) | (name, (cnt, blocker)) <- M.toList (Loader.cdlUnresolvedProducts newStats)]
                            }

                -- Save updated staged database
                atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName updatedStaged)

                -- Return updated setup info
                first setupErrorMessage <$> getDatabaseSetupInfo manager dbName

-- | Remove a dependency from a staged (or partially-linked loaded) database
removeDependencyFromStaged :: DatabaseManager -> Text -> Text -> IO (Either Text DatabaseSetupInfo)
removeDependencyFromStaged manager dbName depName = do
    stagedResult <- getOrStageDatabase manager dbName

    case stagedResult of
        Left err -> return $ Left err
        Right staged -> do
            let newDeps = filter (/= depName) (sdSelectedDeps staged)

            -- Re-run cross-DB linking without the removed dependency
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            let remainingIndexes = [idx | (name, idx) <- M.toList indexedDbs, name `elem` newDeps]
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            (_, newStats) <-
                Loader.fixActivityLinksWithCrossDB
                    remainingIndexes
                    synonymDB
                    unitConfig
                    (M.map snd (dmGeographies manager))
                    (sdSimpleDB staged)

            -- Update staged database
            let updatedStaged =
                    staged
                        { sdSelectedDeps = newDeps
                        , sdCrossDBLinks = Loader.cdlLinks newStats
                        , sdLinkingStats = newStats
                        , sdMissingProducts = sortOn (\(_, cnt, _) -> Down cnt) [(name, cnt, blocker) | (name, (cnt, blocker)) <- M.toList (Loader.cdlUnresolvedProducts newStats)]
                        }

            atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName updatedStaged)
            first setupErrorMessage <$> getDatabaseSetupInfo manager dbName

-- | Finalize a staged database (build matrices and make it ready for queries)
finalizeDatabase :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
finalizeDatabase manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)

    case M.lookup dbName stagedDbs of
        Nothing -> do
            -- Not staged — check if already loaded (no-op finalize)
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            case M.lookup dbName loadedDbs of
                Just loaded -> return $ Right loaded
                Nothing -> return $ Left $ "Staged database not found: " <> dbName
        Just staged -> do
            -- Reject finalization if there are unresolved links
            let unlinked = Loader.countUnlinkedExchanges (sdSimpleDB staged)
                crossDBLinks = Loader.crossDBLinksCount (sdLinkingStats staged)
                unresolvedLinks = max 0 (unlinked - crossDBLinks)
            let activityCount = M.size (sdbActivities (sdSimpleDB staged))
            if activityCount == 0
                then
                    return $
                        Left $
                            "Cannot finalize: database contains 0 activities. "
                                <> "The data file may be corrupted or in an unsupported format."
                else
                    if unresolvedLinks > 0
                        then
                            return $
                                Left $
                                    "Cannot finalize: "
                                        <> T.pack (show unresolvedLinks)
                                        <> " unresolved inputs. Add dependencies to resolve them first."
                        else do
                            reportProgress Info $ "[STARTING] Finalizing database: " <> T.unpack dbName

                            synonymDB <- getMergedSynonymDB manager

                            -- Use pre-built database from cache, or build matrices from scratch
                            buildResult <- case sdCachedDB staged of
                                Just cachedDb -> do
                                    let db = BM25.addBM25Index (initializeRuntimeFields cachedDb synonymDB)
                                    return $ Right (db, True)
                                Nothing -> do
                                    unitConfig <- getMergedUnitConfig manager
                                    dbResult <-
                                        buildDatabaseWithMatrices
                                            unitConfig
                                            (sdbActivities (sdSimpleDB staged))
                                            (sdbFlows (sdSimpleDB staged))
                                            (sdbUnits (sdSimpleDB staged))
                                    case dbResult of
                                        Left err -> return $ Left err
                                        Right db -> do
                                            let dbWithLinks =
                                                    db
                                                        { dbCrossDBLinks = sdCrossDBLinks staged
                                                        , dbDependsOn = sdSelectedDeps staged
                                                        , dbLinkingStats = sdLinkingStats staged
                                                        }
                                            return $ Right (BM25.addBM25Index (initializeRuntimeFields dbWithLinks synonymDB), False)

                            case buildResult of
                                Left err -> return $ Left err
                                Right (dbWithRuntime, fromCache) -> do
                                    -- Create shared solver with lazy factorization (deferred to first query)
                                    let techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples dbWithRuntime)]
                                        activityCountInt = fromIntegral $ dbActivityCount dbWithRuntime
                                    sharedSolver <- createSharedSolver dbName techTriplesInt activityCountInt

                                    let loaded =
                                            LoadedDatabase
                                                { ldDatabase = dbWithRuntime
                                                , ldSharedSolver = sharedSolver
                                                , ldConfig = sdConfig staged
                                                }

                                    -- Move from staged to loaded
                                    let indexedDb = buildIndexedDatabaseFromDB dbName synonymDB dbWithRuntime
                                    atomically $ do
                                        modifyTVar' (dmStagedDbs manager) (M.delete dbName)
                                        modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                                        modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
                                    clearMethodMappingCacheForDb manager dbName

                                    -- Only save to cache when matrices were built fresh
                                    when (not fromCache) $
                                        Loader.saveCachedDatabaseWithMatrices dbName (dcPath (sdConfig staged)) dbWithRuntime

                                    reportProgress Info $ "  [OK] Finalized: " <> T.unpack dbName
                                    return $ Right loaded

--------------------------------------------------------------------------------
-- Method Collection Management
--------------------------------------------------------------------------------

{- | Load methods from a MethodConfig path (directory or archive).
Handles ZIP/7z archives via resolveDataPath, finds method XMLs,
and enriches CFs from ILCD flow XMLs when available.
-}
loadMethodCollectionFromConfig :: MethodConfig -> IO (Either Text (MethodCollection, M.Map UUID ILCDFlowInfo))
loadMethodCollectionFromConfig mc = do
    -- Resolve archives (ZIP → extracted directory)
    resolvedPath <- resolveDataPath (mcPath mc)
    isDir <- doesDirectoryExist resolvedPath
    if not isDir
        then return $ Left $ "Method path not found: " <> T.pack (mcPath mc)
        else do
            -- Find method directory (handles nested ILCD structures)
            dir <- findMethodDirectory resolvedPath
            files <- listDirectory dir
            let xmlFiles = filter (\f -> map toLower (takeExtension f) == ".xml") files
                csvFiles = filter (\f -> map toLower (takeExtension f) == ".csv") files
                jsonFiles = filter (\f -> map toLower (takeExtension f) == ".json") files
            if null xmlFiles && null csvFiles && null jsonFiles
                then return $ Left $ "No method files (.xml/.csv/.json) found in: " <> T.pack dir
                else do
                    -- Try to find sibling flows/ directory for CF enrichment
                    mFlowsDir <- FlowResolver.resolveFlowDirectory dir
                    flowInfo <- case mFlowsDir of
                        Nothing -> do
                            reportProgress Info "  No flows/ directory found, using shortDescription fallback"
                            return M.empty
                        Just flowsDir -> do
                            reportProgress Info $ "  Loading ILCD flow XMLs from: " <> flowsDir
                            info <- FlowResolver.parseFlowDirectory flowsDir
                            reportProgress Info $ "  Loaded " <> show (M.size info) <> " flow definitions"
                            return info
                    -- Parse method files with flow enrichment
                    xmlResults <- forM xmlFiles $ \f ->
                        Method.Parser.parseMethodFileWithFlows flowInfo (dir </> f)
                    -- Split CSV files into SimaPro method exports and tabular CSVs
                    csvParsed <- forM csvFiles $ \f -> do
                        bytes <- stripBOM <$> BS.readFile (dir </> f)
                        if isSimaProMethodCSV bytes
                            then return $ fmap Left (parseSimaProMethodCSVBytes bytes)
                            else return $ fmap Right (parseMethodCSVBytes bytes)
                    -- openLCA JSON-LD ImpactCategory files (carries optional regionalized CFs).
                    -- Only files that actually carry @type=ImpactCategory are parsed; others
                    -- are skipped silently since arbitrary .json files can sit alongside
                    -- method data (e.g. metadata or other openLCA entity types).
                    jsonResults <- forM jsonFiles $ \f -> do
                        bytes <- BS.readFile (dir </> f)
                        if OlcaSchema.isOlcaImpactCategoryJson bytes
                            then return $ Just $ OlcaSchema.parseOlcaImpactCategoryBytes bytes
                            else return Nothing
                    let (xmlErrs, xmlMethods) = partitionEithers xmlResults
                        (csvErrs, csvOks) = partitionEithers csvParsed
                        (jsonErrs, jsonMethods) = partitionEithers [r | Just r <- jsonResults]
                        -- Merge: SimaPro CSVs are MethodCollections, tabular CSVs are [Method]
                        spCollections = [sp | Left sp <- csvOks]
                        tabularMethods = concat [ms | Right ms <- csvOks]
                        allMethods =
                            xmlMethods
                                ++ tabularMethods
                                ++ jsonMethods
                                ++ concatMap mcMethods spCollections
                        -- Merge NW data from all SimaPro CSV sources
                        allDamageCats = concatMap mcDamageCategories spCollections
                        allNWSets = concatMap mcNormWeightSets spCollections
                        collection = MethodCollection allMethods allDamageCats allNWSets []
                        errs = xmlErrs ++ csvErrs ++ jsonErrs
                    if null allMethods && not (null errs)
                        then return $ Left $ "All method files failed to parse: " <> T.pack (head errs)
                        else do
                            let xmlOk = length xmlMethods
                                csvOk = length csvOks
                                jsonOk = length jsonMethods
                            reportProgress Info $ "  Parsed " <> show xmlOk <> " XML, " <> show csvOk <> " CSV, " <> show jsonOk <> " JSON file(s)"
                            when (not (null allDamageCats)) $
                                reportProgress Info $
                                    "  "
                                        <> show (length allDamageCats)
                                        <> " damage categories, "
                                        <> show (length allNWSets)
                                        <> " normalization-weighting set(s)"
                            when (not (null errs)) $
                                reportProgress Warning $
                                    "  " <> show (length errs) <> " method file(s) failed to parse"
                            return $ Right (collection, flowInfo)
  where
    partitionEithers = foldr f ([], [])
      where
        f (Left a) (ls, rs) = (a : ls, rs)
        f (Right b) (ls, rs) = (ls, b : rs)

-- | Strip UTF-8 BOM if present (common in Windows-created CSV files).
stripBOM :: BS.ByteString -> BS.ByteString
stripBOM bs
    | BS.isPrefixOf "\xEF\xBB\xBF" bs = BS.drop 3 bs
    | otherwise = bs

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
                            -- Inject scoring sets from TOML config
                            let scoringSets = map configToScoringSet (Config.mcScoringSets mc)
                                collection = collection0{Method.Types.mcScoringSets = scoringSets}
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
                            -- Auto-extract synonyms from ILCD flow definitions
                            let pairs = extractFromILCDFlows flowInfo
                            autoCreateFlowSynonyms
                                manager
                                name
                                ("Auto-extracted from " <> name)
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

-- | Get the merged CompartmentMap from all loaded compartment mappings.
getMergedCompartmentMap :: DatabaseManager -> IO CompartmentMap
getMergedCompartmentMap manager = do
    loaded <- readTVarIO (dmLoadedCompMaps manager)
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
-- | Location hierarchy as a 'Map ChildLocation [ParentLocation]', sourced from
-- 'data/geographies.csv' (or the hardcoded fallback). Reused across the LCIA
-- regionalized scoring path (see 'Method.Mapping.computeRegionalizedLCIAScore').
getLocationHierarchy :: DatabaseManager -> IO (M.Map Text [Text])
getLocationHierarchy manager = pure (M.map snd (dmGeographies manager))

getMergedFlowMetadata :: DatabaseManager -> IO (FlowDB, UnitDB)
getMergedFlowMetadata manager = do
    cached <- readTVarIO (dmMergedFlowMetadataCache manager)
    case cached of
        Just snap -> pure snap
        Nothing -> do
            loaded <- readTVarIO (dmLoadedDbs manager)
            let dbs = map ldDatabase (M.elems loaded)
                flowMaps = map dbFlows dbs
                unitMaps = map dbUnits dbs
                !mergedFlows = M.unions flowMaps
                !mergedUnits = M.unions unitMaps
                flowHits = collisions flowFingerprint flowMaps
                unitHits = collisions unitFingerprint unitMaps
            unless (null flowHits) $
                reportProgress Warning $
                    "[merged FlowDB] "
                        <> show (length flowHits)
                        <> " UUID collision(s) with divergent flow metadata; keeping first. Samples: "
                        <> show (take 3 flowHits)
            unless (null unitHits) $
                reportProgress Warning $
                    "[merged UnitDB] "
                        <> show (length unitHits)
                        <> " UUID collision(s) with divergent unit metadata; keeping first. Samples: "
                        <> show (take 3 unitHits)
            let !snap = (mergedFlows, mergedUnits)
            atomically $ writeTVar (dmMergedFlowMetadataCache manager) (Just snap)
            pure snap
  where
    flowFingerprint f = (flowName f, flowCategory f, flowSubcompartment f)
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

-- | Operations for a ref-data kind — everything that varies between the three.
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
        dmAvailableFlowSyns
        dmLoadedFlowSyns
        (first T.pack . buildFromCSV)
        synonymCount
        "flow synonyms"
        "uploads/flow-synonyms"
        (\rd -> rdIsUploaded rd || rdIsAuto rd)

compMapOps :: RefDataOps CompartmentMap
compMapOps =
    RefDataOps
        dmAvailableCompMaps
        dmLoadedCompMaps
        (first T.pack . buildCompartmentMapFromCSV)
        compartmentMapSize
        "compartment mapping"
        "uploads/compartment-mappings"
        rdIsUploaded

unitDefOps :: RefDataOps UnitConversion.UnitConfig
unitDefOps =
    RefDataOps
        dmAvailableUnitDefs
        dmLoadedUnitDefs
        UnitConversion.buildFromCSV
        UnitConversion.unitCount
        "units"
        "uploads/units"
        rdIsUploaded

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
                    result <- loadRefDataCSV (rdPath rd)
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

{- | Drop the merged-ref-data caches. Conservatively clears both — the
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
        result <- loadFromCSVFileWithCache (rdPath rd)
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
        result <- loadRefDataCSV (rdPath rd)
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
Lines starting with "code" are treated as headers and skipped.
-}
parseGeographiesCSV :: FilePath -> IO (Map Text (Text, [Text]))
parseGeographiesCSV path = do
    exists <- doesFileExist path
    if not exists
        then do
            reportProgress Info $ "Geographies file not found: " <> path <> " (using built-in hierarchy)"
            return M.empty
        else do
            content <- TIO.readFile path
            let ls = T.lines content
                parsed = concatMap parseLine ls
            reportProgress Info $ "Loaded " <> show (length parsed) <> " geographies from " <> path
            return $ M.fromList parsed
  where
    parseLine line
        | T.null (T.strip line) = []
        | "code" `T.isPrefixOf` line = [] -- header row
        | "#" `T.isPrefixOf` T.strip line = [] -- comment
        | otherwise = case T.splitOn "," line of
            [] -> []
            [_] -> []
            parts ->
                let code = T.strip (head parts)
                    parentsStr = T.strip (last parts)
                    displayRaw = T.intercalate "," (init (tail parts))
                    displayName = let d = T.strip displayRaw in if T.null d then code else d
                    parents = if T.null parentsStr then [] else T.splitOn "|" parentsStr
                 in [(code, (displayName, parents))]

-- | Load CSV file content from path.
loadRefDataCSV :: FilePath -> IO (Either Text BL.ByteString)
loadRefDataCSV path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "File not found: " <> T.pack path
        else Right <$> BL.readFile path

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
                                    , rdPath = csvPath
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

{- | Auto-create flow synonyms from extracted pairs.
Writes CSV to uploads/flow-synonyms/auto-{source}/data.csv,
registers and auto-loads the synonym set.
-}
autoCreateFlowSynonyms :: DatabaseManager -> Text -> Text -> [(Text, Text)] -> IO ()
autoCreateFlowSynonyms _ _ _ [] = return ()
autoCreateFlowSynonyms manager sourceName description pairs = do
    let slug = "auto-" <> sourceName
    -- Skip if already loaded (persisted CSV from previous run, loaded by autoLoadRefData)
    alreadyLoaded <- atomically $ M.member slug <$> readTVar (dmLoadedFlowSyns manager)
    if alreadyLoaded
        then reportProgress Info $ "  [AUTO] " <> T.unpack slug <> ": already loaded (cached)"
        else do
            let dir = "uploads/flow-synonyms" </> T.unpack slug
                path = dir </> "data.csv"
            createDirectoryIfMissing True dir
            BL.writeFile path (synonymPairsToCSV pairs)
            let rd =
                    RefDataConfig
                        { rdName = slug
                        , rdPath = path
                        , rdActive = False -- auto-extracted synonyms inactive by default (noisy, use curated data/flows.csv)
                        , rdIsUploaded = True
                        , rdIsAuto = True
                        , rdDescription = Just description
                        }
            addFlowSynonyms manager rd
            -- Build SynonymDB directly from pairs (skip CSV round-trip)
            let !synDB = buildFromPairs pairs
            atomically $ modifyTVar' (dmLoadedFlowSyns manager) (M.insert slug synDB)
            reportProgress Info $
                "  [AUTO] "
                    <> T.unpack slug
                    <> ": "
                    <> show (length pairs)
                    <> " synonym pairs"

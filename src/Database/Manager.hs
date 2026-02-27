{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Manager
    ( -- * Types
      DatabaseManager(..)
    , LoadedDatabase(..)
    , DatabaseStatus(..)
    , DatabaseLoadStatus(..)
    , StagedDatabase(..)
    , DatabaseSetupInfo(..)
    , SetupError(..)
    , MissingSupplier(..)
    , DependencySuggestion(..)
      -- * Re-exports
    , DepLoadResult(..)
      -- * Initialization
    , initDatabaseManager
    , initSingleDatabaseManager
      -- * Operations
    , getDatabase
    , listDatabases
      -- * Load/Unload
    , loadDatabase
    , unloadDatabase
    , addDatabase
    , removeDatabase
      -- * Staged Database Operations
    , getStagedDatabase
    , getDatabaseSetupInfo
    , addDependencyToStaged
    , removeDependencyFromStaged
    , setDataPath
    , finalizeDatabase
      -- * Internal (for Main.hs to load database)
    , loadDatabaseFromConfig
    ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Control.Exception
import Control.Monad (forM, forM_, when)
import Data.Maybe (catMaybes, isJust)
import System.Mem (performGC)
import Data.Bifunctor (first)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (takeExtension)
import Data.Char (toLower)
import Data.List (isPrefixOf, nub, sortOn)
import Data.Ord (Down(..))

import Config
import Data.Time (diffUTCTime, getCurrentTime)
import Matrix (precomputeMatrixFactorization, addFactorizationToDatabase, clearCachedKspSolver)
import SharedSolver (SharedSolver, createSharedSolver)
import Progress (reportProgress, reportProgressWithTiming, reportError, ProgressLevel(..))
import Database (buildDatabaseWithMatrices)
import SynonymDB (SynonymDB)
import Types (Database(..), SparseTriple(..), SimpleDatabase(..), initializeRuntimeFields, toSimpleDatabase, Activity(..), Exchange(..), UUID, exchangeFlowId, exchangeIsReference, CrossDBLink(..), CrossDBLinkingStats(..), emptyCrossDBLinkingStats, crossDBLinksCount, crossDBBySource, unresolvedCount, LinkBlocker(..))
import qualified UnitConversion as UnitConversion
import qualified Database.Loader as Loader
-- CrossDBLinkingStats is now in Types, re-exported from Database.Loader
import Database.CrossLinking (IndexedDatabase(..), buildIndexedDatabaseFromDB)
import qualified Database.CrossLinking
import qualified Database.Upload as Upload
import Database.Upload (anyDataFilesIn)
import qualified Database.UploadedDatabase as UploadedDB
import qualified SimaPro.Parser as SimaPro
import System.FilePath (takeExtension, dropExtension, (</>))
import System.Directory (removeDirectoryRecursive)
import API.Types (DepLoadResult(..))

-- | A fully loaded database with solver ready for queries
data LoadedDatabase = LoadedDatabase
    { ldDatabase     :: !Database
    , ldSharedSolver :: !SharedSolver
    , ldConfig       :: !DatabaseConfig
    }

-- | A staged database awaiting dependency configuration
-- This is the intermediate state before building matrices
data StagedDatabase = StagedDatabase
    { sdSimpleDB        :: !SimpleDatabase                -- ^ Parsed data (activities, flows, units)
    , sdConfig          :: !DatabaseConfig                -- ^ Configuration
    , sdUnlinkedCount   :: !Int                           -- ^ Total unlinked exchanges
    , sdMissingProducts :: ![(Text, Int, LinkBlocker)]    -- ^ (product name, count, reason)
    , sdSelectedDeps    :: ![Text]                        -- ^ Selected dependency database names
    , sdCrossDBLinks    :: ![CrossDBLink]                 -- ^ Cross-DB links found so far
    , sdLinkingStats    :: !CrossDBLinkingStats           -- ^ Linking statistics
    }

-- | Information about a missing supplier product
data MissingSupplier = MissingSupplier
    { msProductName :: !Text
    , msCount       :: !Int           -- ^ Number of activities needing this supplier
    , msLocation    :: !(Maybe Text)  -- ^ Most common location requested
    , msReason      :: !Text          -- ^ "unit_incompatible", "location_unavailable", "no_name_match"
    , msDetail      :: !(Maybe Text)  -- ^ e.g. "kg vs ton", "FR not available"
    } deriving (Show, Eq, Generic)

instance ToJSON MissingSupplier where
    toJSON MissingSupplier{..} = A.object
        [ "productName" .= msProductName
        , "count" .= msCount
        , "location" .= msLocation
        , "reason" .= msReason
        , "detail" .= msDetail
        ]

-- | Suggestion for a dependency database
data DependencySuggestion = DependencySuggestion
    { dsgDatabaseName :: !Text
    , dsgDisplayName  :: !Text
    , dsgMatchCount   :: !Int    -- ^ How many missing suppliers it can provide
    } deriving (Show, Eq, Generic)

instance ToJSON DependencySuggestion where
    toJSON DependencySuggestion{..} = A.object
        [ "databaseName" .= dsgDatabaseName
        , "displayName" .= dsgDisplayName
        , "matchCount" .= dsgMatchCount
        ]

-- | Setup info for a database (for the setup page)
data DatabaseSetupInfo = DatabaseSetupInfo
    { dsiName               :: !Text
    , dsiDisplayName        :: !Text
    , dsiActivityCount      :: !Int
    , dsiInputCount         :: !Int          -- ^ Total technosphere inputs
    , dsiCompleteness       :: !Double       -- ^ Percentage of resolved links (0-100)
    , dsiInternalLinks      :: !Int          -- ^ Links resolved within this database
    , dsiCrossDBLinks       :: !Int          -- ^ Links resolved via dependencies
    , dsiUnresolvedLinks    :: !Int          -- ^ Still unresolved
    , dsiMissingSuppliers   :: ![MissingSupplier]  -- ^ Top missing suppliers
    , dsiSelectedDeps       :: ![Text]       -- ^ Currently selected dependencies
    , dsiSuggestions        :: ![DependencySuggestion]  -- ^ Suggested dependencies
    , dsiIsReady            :: !Bool         -- ^ True if can be finalized
    , dsiUnknownUnits       :: ![Text]       -- ^ Unknown units from sdbUnits
    , dsiLocationFallbacks  :: ![(Text, Text, Text)]  -- ^ (product, requestedLoc, actualLoc)
    , dsiDataPath           :: !Text                   -- ^ Current selected data path (relative)
    , dsiAvailablePaths     :: ![(Text, Text, Int)]    -- ^ (relativePath, formatLabel, fileCount)
    , dsiIsLoaded           :: !Bool                   -- ^ True if database is already loaded (read-only info)
    } deriving (Show, Eq, Generic)

instance ToJSON DatabaseSetupInfo where
    toJSON DatabaseSetupInfo{..} = A.object
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
        encodeFallback (prod, req, act) = A.object
            [ "product" .= prod, "requested" .= req, "actual" .= act ]
        encodeCandidate (path, fmt, cnt) = A.object
            [ "path" .= path, "format" .= fmt, "fileCount" .= cnt ]

-- | Errors from getDatabaseSetupInfo
data SetupError = SetupNotFound Text | SetupFailed Text
    deriving (Show, Eq)

setupErrorMessage :: SetupError -> Text
setupErrorMessage (SetupNotFound msg) = msg
setupErrorMessage (SetupFailed msg)   = msg

-- | Load status: derivable from TVar membership + linking stats
data DatabaseLoadStatus = Unloaded | PartiallyLinked | Loaded
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseLoadStatus where
    toJSON Unloaded        = A.String "unloaded"
    toJSON PartiallyLinked = A.String "partially_linked"
    toJSON Loaded          = A.String "loaded"

instance FromJSON DatabaseLoadStatus where
    parseJSON = A.withText "DatabaseLoadStatus" $ \case
        "unloaded"         -> pure Unloaded
        "partially_linked" -> pure PartiallyLinked
        "loaded"           -> pure Loaded
        other              -> fail $ "Unknown DatabaseLoadStatus: " <> T.unpack other

-- | Status of a database for API responses
data DatabaseStatus = DatabaseStatus
    { dsName        :: !Text           -- Internal identifier (slug)
    , dsDisplayName :: !Text           -- Human-readable name for UI
    , dsDescription :: !(Maybe Text)
    , dsLoadAtStartup :: !Bool         -- Configured to load at startup
    , dsStatus      :: !DatabaseLoadStatus -- Derived from TVar membership + linking stats
    , dsIsUploaded  :: !Bool           -- True if path starts with "uploads/"
    , dsPath        :: !Text           -- Data path
    , dsFormat      :: !(Maybe Upload.DatabaseFormat)  -- Detected format
    } deriving (Show, Eq, Generic)

instance ToJSON DatabaseStatus where
    toJSON DatabaseStatus{..} = A.object
        [ "dsName" .= dsName
        , "dsDisplayName" .= dsDisplayName
        , "dsDescription" .= dsDescription
        , "dsLoadAtStartup" .= dsLoadAtStartup
        , "dsStatus" .= dsStatus
        , "dsIsUploaded" .= dsIsUploaded
        , "dsPath" .= dsPath
        , "dsFormat" .= fmap formatToDisplayText dsFormat
        ]
      where
        formatToDisplayText Upload.EcoSpold2 = "EcoSpold 2" :: T.Text
        formatToDisplayText Upload.EcoSpold1 = "EcoSpold 1"
        formatToDisplayText Upload.SimaProCSV = "SimaPro CSV"
        formatToDisplayText Upload.UnknownFormat = ""

instance FromJSON DatabaseStatus where
    parseJSON = A.withObject "DatabaseStatus" $ \v -> DatabaseStatus
        <$> v .: "dsName"
        <*> v .: "dsDisplayName"
        <*> v .:? "dsDescription"
        <*> v .: "dsLoadAtStartup"
        <*> v .: "dsStatus"
        <*> v .: "dsIsUploaded"
        <*> v .: "dsPath"
        <*> (parseFormat <$> v .:? "dsFormat")
      where
        parseFormat :: Maybe T.Text -> Maybe Upload.DatabaseFormat
        parseFormat Nothing = Nothing
        parseFormat (Just "EcoSpold 2") = Just Upload.EcoSpold2
        parseFormat (Just "EcoSpold 1") = Just Upload.EcoSpold1
        parseFormat (Just "SimaPro CSV") = Just Upload.SimaProCSV
        parseFormat (Just _) = Just Upload.UnknownFormat

-- | The database manager maintains state for multiple databases
-- Databases with load=true are pre-loaded at startup for instant switching
data DatabaseManager = DatabaseManager
    { dmLoadedDbs     :: !(TVar (Map Text LoadedDatabase))  -- All loaded databases
    , dmStagedDbs     :: !(TVar (Map Text StagedDatabase))  -- Staged databases (parsed but not finalized)
    , dmStagingDbs    :: !(TVar (S.Set Text))               -- Databases currently being staged
    , dmIndexedDbs    :: !(TVar (Map Text IndexedDatabase)) -- Pre-built indexes for cross-DB linking
    , dmAvailableDbs  :: !(TVar (Map Text DatabaseConfig))  -- All configured databases
    , dmSynonymDB     :: !SynonymDB                         -- Shared synonym database
    , dmNoCache       :: !Bool                              -- Caching disabled flag
    , dmUnitConfig    :: !UnitConversion.UnitConfig         -- Unit configuration
    }

-- | Initialize database manager from config
-- Pre-loads databases with load=true at startup
-- Also discovers uploaded databases from uploads/ directory
initDatabaseManager :: Config -> SynonymDB -> Bool -> Maybe FilePath -> IO DatabaseManager
initDatabaseManager config synonymDB noCache _configPath = do
    -- Get configured databases and detect their format
    configuredDbs <- forM (cfgDatabases config) $ \dbConfig -> do
        resolvedPath <- resolveDataPath (dcPath dbConfig)
        format <- Upload.detectDatabaseFormat resolvedPath
        return dbConfig { dcPath = resolvedPath, dcFormat = Just format }

    -- Discover uploaded databases from uploads/ directory (self-describing with meta.toml)
    uploadedDbs <- discoverUploadedDatabases

    -- Merge configured + uploaded
    let allDbs = configuredDbs ++ uploadedDbs

    -- Build UnitConfig from config (or use defaults)
    unitConfig <- case cfgUnits config of
        Nothing -> do
            reportProgress Info "Using default unit configuration"
            return UnitConversion.defaultUnitConfig
        Just unitsToml -> do
            let aliases = M.map (\uac -> (uacDim uac, uacFactor uac)) (ucAliases unitsToml)
            case UnitConversion.buildUnitConfigFromToml (ucDimensions unitsToml) aliases of
                Right cfg -> do
                    reportProgress Info $ "Loaded " ++ show (M.size aliases) ++ " custom unit aliases from config"
                    return cfg
                Left err -> do
                    reportError $ "Invalid [units] config: " <> T.unpack err <> " - using defaults"
                    return UnitConversion.defaultUnitConfig

    -- Create TVars
    loadedDbsVar <- newTVarIO M.empty
    stagedDbsVar <- newTVarIO M.empty  -- Staged databases awaiting finalization
    stagingDbsVar <- newTVarIO S.empty -- Databases currently being staged (race guard)
    indexedDbsVar <- newTVarIO M.empty  -- Pre-built indexes for cross-DB linking
    availableDbsVar <- newTVarIO $ M.fromList [(dcName dc, dc) | dc <- allDbs]

    let manager = DatabaseManager
            { dmLoadedDbs = loadedDbsVar
            , dmStagedDbs = stagedDbsVar
            , dmStagingDbs = stagingDbsVar
            , dmIndexedDbs = indexedDbsVar
            , dmAvailableDbs = availableDbsVar
            , dmSynonymDB = synonymDB
            , dmNoCache = noCache
            , dmUnitConfig = unitConfig
            }

    -- Resolve load order: expand load=true transitively through depends, then topo-sort
    let allDbConfigs = allDbs
        configMap = M.fromList [(dcName c, c) | c <- allDbConfigs]
    case resolveLoadOrder allDbConfigs of
        Left err -> reportError $ "Dependency resolution failed: " <> T.unpack err
        Right loadOrder -> do
            let dbsToLoad = [configMap M.! name | name <- loadOrder, M.member name configMap]
            reportProgress Info $ "Loading " ++ show (length dbsToLoad) ++ " database(s) in dependency order: "
                ++ T.unpack (T.intercalate " → " loadOrder)
            totalStart <- getCurrentTime
            forM_ dbsToLoad $ \dbConfig -> do
                reportProgress Info $ "Loading database: " <> T.unpack (dcDisplayName dbConfig)
                -- Get currently loaded IndexedDatabases for cross-DB linking
                currentIndexedDbs <- readTVarIO indexedDbsVar
                let otherIndexes = M.elems currentIndexedDbs
                result <- loadDatabaseFromConfigWithCrossDB dbConfig synonymDB unitConfig noCache otherIndexes
                case result of
                    Right loaded -> do
                        -- Build index from the loaded Database for future cross-DB linking
                        let indexedDb = buildIndexedDatabaseFromDB (dcName dbConfig) synonymDB (ldDatabase loaded)
                        atomically $ do
                            modifyTVar' loadedDbsVar (M.insert (dcName dbConfig) loaded)
                            modifyTVar' indexedDbsVar (M.insert (dcName dbConfig) indexedDb)
                        reportProgress Info $ "  [OK] Loaded: " <> T.unpack (dcDisplayName dbConfig)
                    Left err ->
                        reportError $ "  [FAIL] Failed to load " <> T.unpack (dcName dbConfig) <> ": " <> T.unpack err
            totalEnd <- getCurrentTime
            let totalDuration = realToFrac (diffUTCTime totalEnd totalStart) :: Double
            reportProgressWithTiming Info "Total database loading time" totalDuration

    -- Report final status
    loadedCount <- atomically $ M.size <$> readTVar loadedDbsVar
    reportProgress Info $ "Multi-database mode: " ++ show loadedCount ++ " database(s) loaded"

    return manager

-- | Discover uploaded databases from uploads/ directory
-- Reads meta.toml from each subdirectory and converts to DatabaseConfig
discoverUploadedDatabases :: IO [DatabaseConfig]
discoverUploadedDatabases = do
    uploads <- UploadedDB.discoverUploadedDatabases
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded database: " <> T.unpack slug
        -- Always detect format from actual files (old uploads may have "unknown")
        let dataDir = dirPath </> UploadedDB.umDataPath meta
        format <- Upload.detectDatabaseFormat dataDir
        return $ uploadMetaToConfig slug dirPath meta { UploadedDB.umFormat = format }

-- | Convert UploadMeta to DatabaseConfig
uploadMetaToConfig :: Text -> FilePath -> UploadedDB.UploadMeta -> DatabaseConfig
uploadMetaToConfig slug dirPath meta = DatabaseConfig
    { dcName = slug
    , dcDisplayName = UploadedDB.umDisplayName meta
    , dcPath = dirPath </> UploadedDB.umDataPath meta  -- Full path to data
    , dcDescription = UploadedDB.umDescription meta
    , dcLoad = False  -- Never auto-load uploads
    , dcDefault = False
    , dcDepends = []
    , dcLocationAliases = M.empty
    , dcFormat = Just (UploadedDB.umFormat meta)
    , dcIsUploaded = True  -- Discovered from uploads/ directory
    }

-- | Initialize a single-database manager (for --data mode)
-- Creates a config with one database and initializes it
initSingleDatabaseManager :: FilePath -> SynonymDB -> Bool -> IO DatabaseManager
initSingleDatabaseManager dataPath synonymDB noCache = do
    -- Resolve archives and detect format
    resolvedPath <- resolveDataPath dataPath
    format <- Upload.detectDatabaseFormat resolvedPath

    let dbConfig = DatabaseConfig
            { dcName = "default"
            , dcDisplayName = T.pack dataPath  -- Use path as display name
            , dcPath = resolvedPath
            , dcDescription = Just "Single database mode (--data)"
            , dcLoad = True
            , dcDefault = True
            , dcDepends = []
            , dcLocationAliases = M.empty
            , dcFormat = Just format
            , dcIsUploaded = False  -- Command-line specified, not uploaded
            }

    let config = Config
            { cfgServer = defaultServerConfig
            , cfgDatabases = [dbConfig]
            , cfgMethods = []
            , cfgUnits = Nothing
            }

    initDatabaseManager config synonymDB noCache Nothing

-- | Get a database by name
getDatabase :: DatabaseManager -> Text -> IO (Maybe LoadedDatabase)
getDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    return $ M.lookup dbName loadedDbs

-- | List all databases with their status
listDatabases :: DatabaseManager -> IO [DatabaseStatus]
listDatabases manager = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    forM (M.toList availableDbs) $ \(name, config) -> do
        let !status = case M.lookup name loadedDbs of
                Nothing -> Unloaded
                Just ld | unresolvedCount (dbLinkingStats (ldDatabase ld)) > 0 -> PartiallyLinked
                        | otherwise -> Loaded
        return DatabaseStatus
            { dsName = name
            , dsDisplayName = dcDisplayName config
            , dsDescription = dcDescription config
            , dsLoadAtStartup = dcLoad config
            , dsStatus = status
            , dsIsUploaded = dcIsUploaded config
            , dsPath = T.pack (dcPath config)
            , dsFormat = dcFormat config
            }

-- | Check if a file path is a cache file
isCacheFile :: FilePath -> Bool
isCacheFile path =
    let ext = takeExtension path
        ext2 = takeExtension (dropExtension path)
    in ext == ".bin" || (ext == ".zst" && ext2 == ".bin")

-- | Load a database from its configuration (without cross-DB linking)
-- This is the original function, kept for backward compatibility
loadDatabaseFromConfig :: DatabaseConfig -> SynonymDB -> Bool -> IO (Either Text LoadedDatabase)
loadDatabaseFromConfig dbConfig synonymDB noCache =
    loadDatabaseFromConfigWithCrossDB dbConfig synonymDB UnitConversion.defaultUnitConfig noCache []

-- | Resolve a database path: if it's an archive, extract it first.
-- Extracts to "{archivePath}.d/" and finds the actual data directory inside.
-- Plain files/directories pass through unchanged.
resolveDataPath :: FilePath -> IO FilePath
resolveDataPath path = do
    isDir <- doesDirectoryExist path
    if isDir then return path
    else do
        isFile <- doesFileExist path
        if not isFile then return path  -- missing: let caller handle
        else
            let ext = map toLower (takeExtension path)
            in if ext `elem` [".zip", ".7z", ".gz", ".xz"]
                then extractAndFind path
                else return path
  where
    extractAndFind archive = do
        let extractDir = archive ++ ".d"
        dirExists <- doesDirectoryExist extractDir
        alreadyExtracted <- if dirExists
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
                        return archive  -- let caller report the meaningful error
                    Right () -> do
                        reportProgress Info "Extraction complete"
                        Upload.findDataDirectory extractDir

-- | Load a database from its configuration with cross-database linking support
loadDatabaseFromConfigWithCrossDB
    :: DatabaseConfig
    -> SynonymDB
    -> UnitConversion.UnitConfig
    -> Bool  -- noCache
    -> [IndexedDatabase]  -- Pre-built indexes from other databases for cross-DB linking
    -> IO (Either Text LoadedDatabase)
loadDatabaseFromConfigWithCrossDB dbConfig synonymDB unitConfig noCache otherIndexes = do
    path <- resolveDataPath (dcPath dbConfig)
    let locationAliases = dcLocationAliases dbConfig

    -- Check if path exists
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if not isFile && not isDir
        then return $ Left $ "Path does not exist: " <> T.pack path
        else do
            -- Load raw database with cross-DB linking
            reportProgress Info $ "Loading database from: " <> path
            dbResult <- loadDatabaseRawWithCrossDB (dcName dbConfig) locationAliases path noCache synonymDB unitConfig otherIndexes

            case dbResult of
                Left err -> return $ Left err
                Right dbRaw -> do
                    -- Initialize runtime fields (synonym DB and flow name index)
                    let database = initializeRuntimeFields dbRaw synonymDB

                    -- Pre-compute matrix factorization
                    reportProgress Info "Pre-computing matrix factorization..."
                    let techTriples = dbTechnosphereTriples database
                        activityCount = dbActivityCount database
                        techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                        activityCountInt = fromIntegral activityCount

                    factorization <- precomputeMatrixFactorization (dcName dbConfig) techTriplesInt activityCountInt
                    let databaseWithFact = addFactorizationToDatabase database factorization

                    -- Create shared solver
                    reportProgress Info "Creating shared solver..."
                    sharedSolver <- createSharedSolver (dbCachedFactorization databaseWithFact) techTriplesInt activityCountInt

                    return $ Right LoadedDatabase
                        { ldDatabase = databaseWithFact
                        , ldSharedSolver = sharedSolver
                        , ldConfig = dbConfig
                        }

-- | Detected format of a database directory
data DirectoryFormat = FormatSpold | FormatXML | FormatCSV | FormatUnknown
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
        else if isDir
            then do
                files <- listDirectory path
                let extensions = map (map toLower . takeExtension) files
                -- Check for different formats (in order of preference)
                if any (== ".spold") extensions
                    then return FormatSpold
                    else if any (== ".csv") extensions
                        then return FormatCSV
                        else if any (== ".xml") extensions
                            then return FormatXML
                            else return FormatUnknown
            else return FormatUnknown

-- | Find CSV files in a directory
findCSVFiles :: FilePath -> IO [FilePath]
findCSVFiles path = do
    files <- listDirectory path
    let csvFiles = filter (\f -> map toLower (takeExtension f) == ".csv") files
    return $ map (path </>) csvFiles

-- | Build activity map from list of activities
-- Creates (activityUUID, productUUID) -> Activity mapping
buildActivityMap :: [Activity] -> M.Map (UUID, UUID) Activity
buildActivityMap activities = M.fromList
    [ ((activityUUID, productUUID), activity)
    | activity <- activities
    , let activityUUID = SimaPro.generateActivityUUID (activityName activity <> "@" <> activityLocation activity)
    , let refExchanges = filter exchangeIsReference (exchanges activity)
    , refExchange <- take 1 refExchanges  -- Take first reference product
    , let productUUID = exchangeFlowId refExchange
    ]

-- | Load raw database from path (file or directory)
-- Location aliases are used for EcoSpold1 supplier linking (wrongLocation → correctLocation)
loadDatabaseRaw :: T.Text -> M.Map T.Text T.Text -> FilePath -> Bool -> IO (Either Text Database)
loadDatabaseRaw dbName locationAliases path noCache = do
    isFile <- doesFileExist path
    if isFile && isCacheFile path
        then do
            -- Direct cache file (aliases don't apply - cache was built with them)
            mDb <- Loader.loadDatabaseFromCacheFile path
            case mDb of
                Just db -> return $ Right db
                Nothing -> return $ Left $ "Failed to load cache file: " <> T.pack path
        else do
            -- Check what format the directory contains
            format <- detectDirectoryFormat path
            case format of
                FormatCSV -> do
                    -- Determine the CSV file path (direct file or find in directory)
                    isFileCheck <- doesFileExist path
                    csvFile <- if isFileCheck
                        then return path
                        else do
                            csvFiles <- findCSVFiles path
                            case csvFiles of
                                [] -> error $ "No CSV files found in: " ++ path
                                (f:_) -> return f
                    -- Try to load from cache first (same as EcoSpold)
                    if noCache
                        then do
                            -- No caching: parse CSV directly
                            reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                            (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvFile
                            reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                            let activityMap = buildActivityMap activities
                            !db <- buildDatabaseWithMatrices activityMap flowDB unitDB
                            return $ Right db
                        else do
                            -- Try cache first
                            mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName path
                            case mCachedDb of
                                Just db -> return $ Right db
                                Nothing -> do
                                    -- Parse SimaPro CSV
                                    reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                                    (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvFile
                                    reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                                    let activityMap = buildActivityMap activities
                                    !db <- buildDatabaseWithMatrices activityMap flowDB unitDB
                                    -- Save to cache for next time
                                    Loader.saveCachedDatabaseWithMatrices dbName path db
                                    return $ Right db
                FormatUnknown ->
                    return $ Left $ "No supported database files found in: " <> T.pack path <>
                                   ". Supported formats: EcoSpold v2 (.spold), EcoSpold v1 (.xml), SimaPro CSV (.csv)"
                -- FormatSpold and FormatXML use the same loader (handles both formats)
                _ ->
                    if noCache
                        then do
                            -- No caching: load and build from scratch
                            loadResult <- Loader.loadDatabaseWithLocationAliases locationAliases path
                            case loadResult of
                                Left err -> return $ Left err
                                Right simpleDb -> do
                                    !db <- buildDatabaseWithMatrices
                                        (sdbActivities simpleDb)
                                        (sdbFlows simpleDb)
                                        (sdbUnits simpleDb)
                                    return $ Right db
                        else do
                            -- Try to load from cache, build if missing
                            mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName path
                            case mCachedDb of
                                Just db -> return $ Right db
                                Nothing -> do
                                    -- Build and cache (with location aliases applied)
                                    loadResult <- Loader.loadDatabaseWithLocationAliases locationAliases path
                                    case loadResult of
                                        Left err -> return $ Left err
                                        Right simpleDb -> do
                                            !db <- buildDatabaseWithMatrices
                                                (sdbActivities simpleDb)
                                                (sdbFlows simpleDb)
                                                (sdbUnits simpleDb)
                                            Loader.saveCachedDatabaseWithMatrices dbName path db
                                            return $ Right db

-- | Load raw database from path with cross-database linking support
loadDatabaseRawWithCrossDB
    :: T.Text                       -- ^ Database name
    -> M.Map T.Text T.Text          -- ^ Location aliases
    -> FilePath                     -- ^ Path to load from
    -> Bool                         -- ^ noCache flag
    -> SynonymDB                    -- ^ Synonym database
    -> UnitConversion.UnitConfig    -- ^ Unit configuration
    -> [IndexedDatabase]            -- ^ Pre-built indexes from other databases
    -> IO (Either Text Database)
loadDatabaseRawWithCrossDB dbName locationAliases path noCache synonymDB unitConfig otherIndexes = do
    isFile <- doesFileExist path
    if isFile && isCacheFile path
        then do
            -- Direct cache file - no cross-DB linking needed (already built)
            mDb <- Loader.loadDatabaseFromCacheFile path
            case mDb of
                Just db -> return $ Right db
                Nothing -> return $ Left $ "Failed to load cache file: " <> T.pack path
        else do
            format <- detectDirectoryFormat path
            case format of
                FormatCSV -> do
                    -- CSV format - no cross-DB linking for CSV (yet)
                    isFileCheck <- doesFileExist path
                    csvFile <- if isFileCheck
                        then return path
                        else do
                            csvFiles <- findCSVFiles path
                            case csvFiles of
                                [] -> error $ "No CSV files found in: " ++ path
                                (f:_) -> return f
                    if noCache
                        then do
                            reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                            (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvFile
                            reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                            let activityMap = buildActivityMap activities
                            !db <- buildDatabaseWithMatrices activityMap flowDB unitDB
                            return $ Right db
                        else do
                            mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName path
                            case mCachedDb of
                                Just db -> return $ Right db
                                Nothing -> do
                                    reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                                    (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvFile
                                    reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                                    let activityMap = buildActivityMap activities
                                    !db <- buildDatabaseWithMatrices activityMap flowDB unitDB
                                    Loader.saveCachedDatabaseWithMatrices dbName path db
                                    return $ Right db
                FormatUnknown ->
                    return $ Left $ "No supported database files found in: " <> T.pack path <>
                                   ". Supported formats: EcoSpold v2 (.spold), EcoSpold v1 (.xml), SimaPro CSV (.csv)"
                -- FormatSpold and FormatXML use the same loader - WITH cross-DB linking
                _ -> do
                    -- Try cache first (skip if stale: has unresolved links but deps are now available)
                    mCachedDb <- if noCache then return Nothing
                                 else Loader.loadCachedDatabaseWithMatrices dbName path
                    let cacheUsable = case mCachedDb of
                            Just db | unresolvedCount (dbLinkingStats db) > 0
                                    , not (null otherIndexes) -> False  -- stale: deps now available
                            Just _  -> True
                            Nothing -> False
                    case (cacheUsable, mCachedDb) of
                        (True, Just db) -> return $ Right db
                        _ -> do
                            when (isJust mCachedDb && not cacheUsable) $
                                reportProgress Info "Cache has unresolved links, rebuilding with available dependencies..."
                            loadResult <- Loader.loadDatabaseWithCrossDBLinking
                                locationAliases otherIndexes synonymDB unitConfig path
                            case loadResult of
                                Left err -> return $ Left err
                                Right (simpleDb, stats) -> do
                                    !db <- buildDatabaseWithMatrices
                                        (sdbActivities simpleDb)
                                        (sdbFlows simpleDb)
                                        (sdbUnits simpleDb)
                                    let crossLinks = cdlLinks stats
                                        depDbs = M.keys (crossDBBySource stats)
                                        dbWithLinks = db
                                            { dbCrossDBLinks = crossLinks
                                            , dbDependsOn = depDbs
                                            , dbLinkingStats = stats
                                            }
                                    when (not noCache) $
                                        Loader.saveCachedDatabaseWithMatrices dbName path dbWithLinks
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
                    reportProgress Info $ "Loading database: " <> T.unpack (dcDisplayName dbConfig)
                    -- Get currently loaded IndexedDatabases for cross-DB linking
                    currentIndexedDbs <- readTVarIO (dmIndexedDbs manager)
                    let otherIndexes = M.elems currentIndexedDbs
                    eitherResult <- try $ loadDatabaseFromConfigWithCrossDB
                        dbConfig
                        (dmSynonymDB manager)
                        (dmUnitConfig manager)
                        (dmNoCache manager)
                        otherIndexes
                    case eitherResult of
                        Left (ex :: SomeException) -> return $ Left $ "Exception loading database: " <> T.pack (show ex)
                        Right (Left err) -> return $ Left err
                        Right (Right loaded) -> do
                            -- Build index from the loaded Database for future cross-DB linking
                            let indexedDb = buildIndexedDatabaseFromDB dbName (dmSynonymDB manager) (ldDatabase loaded)
                            atomically $ do
                                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                                modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
                            reportProgress Info $ "  [OK] Loaded:" <> T.unpack (dcDisplayName dbConfig)
                            return $ Right loaded

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

-- | Load a database on demand with automatic dependency loading
-- Pre-loads declared dependencies (from TOML config) so cross-DB linking works,
-- then loads the target database.
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

-- | Stage an uploaded database (parse + cross-DB link, no matrices yet)
-- When a valid cache exists, reconstructs staged state from the cached Database
-- without re-parsing, turning a ~90s operation into ~7s.
stageUploadedDatabase :: DatabaseManager -> DatabaseConfig -> IO (Either Text ())
stageUploadedDatabase manager dbConfig = do
    let dbName = dcName dbConfig

    -- Try cache first: if valid, reconstruct StagedDatabase without re-parsing
    mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName (dcPath dbConfig)

    case mCachedDb of
        Just cachedDb -> do
            -- Cache hit: auto-load dependencies so cross-DB solving works
            _ <- autoLoadDeps manager (dbDependsOn cachedDb)
            -- Reconstruct staged state from cached Database
            let simpleDb = toSimpleDatabase cachedDb
                staged = StagedDatabase
                    { sdSimpleDB = simpleDb
                    , sdConfig = dbConfig
                    , sdUnlinkedCount = 0  -- was finalized successfully
                    , sdMissingProducts = []
                    , sdSelectedDeps = dbDependsOn cachedDb
                    , sdCrossDBLinks = dbCrossDBLinks cachedDb
                    , sdLinkingStats = dbLinkingStats cachedDb
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
                                []    -> return path  -- let loader produce the error
                                (f:_) -> return f
                _ -> return path

            -- Parse and run cross-DB linking (but don't build matrices)
            loadResult <- Loader.loadDatabaseWithCrossDBLinking
                locationAliases
                otherIndexes
                (dmSynonymDB manager)
                (dmUnitConfig manager)
                loadPath

            case loadResult of
                Left err -> return $ Left err
                Right (simpleDb, stats) -> do
                    -- Create staged database
                    let fromStats = [(name, cnt, blocker) | (name, (cnt, blocker)) <- M.toList (Loader.cdlUnresolvedProducts stats)]
                        fromScan  = if null fromStats && Loader.crossDBLinksCount stats == 0
                                    then [(name, cnt, NoNameMatch) | (name, cnt) <- M.toList (Loader.collectUnlinkedProductNames simpleDb)]
                                    else fromStats
                        staged = StagedDatabase
                            { sdSimpleDB = simpleDb
                            , sdConfig = dbConfig
                            , sdUnlinkedCount = Loader.unresolvedCount stats
                            , sdMissingProducts = sortOn (\(_, cnt, _) -> Down cnt) fromScan
                            , sdSelectedDeps = nub $ M.keys (Loader.crossDBBySource stats)
                            , sdCrossDBLinks = Loader.cdlLinks stats
                            , sdLinkingStats = stats
                            }

                    -- Store in staged map
                    atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
                    reportProgress Info $ "  [OK] Staged: " <> T.unpack (dcDisplayName dbConfig)
                    return $ Right ()

-- | Unload a database from memory (keeps config for reloading)
unloadDatabase :: DatabaseManager -> Text -> IO (Either Text ())
unloadDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    case M.lookup dbName loadedDbs of
        Nothing -> return $ Left $ "Database not loaded: " <> dbName
        Just _ -> do
            -- Remove from loaded databases and IndexedDatabases (for cross-DB linking)
            atomically $ do
                modifyTVar' (dmLoadedDbs manager) (M.delete dbName)
                modifyTVar' (dmIndexedDbs manager) (M.delete dbName)

            -- Clear the cached KSP solver to release PETSc memory
            clearCachedKspSolver dbName

            -- Force garbage collection to release memory
            performGC

            reportProgress Info $ "Unloaded database: " <> T.unpack dbName
            return $ Right ()

-- | Add a new database config to the manager (without loading)
addDatabase :: DatabaseManager -> DatabaseConfig -> IO ()
addDatabase manager dbConfig = do
    atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert (dcName dbConfig) dbConfig)
    reportProgress Info $ "Added database config: " <> T.unpack (dcDisplayName dbConfig)

-- | Remove a database from the manager
-- Fails if database is loaded
removeDatabase :: DatabaseManager -> Text -> IO (Either Text ())
removeDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)

    case M.lookup dbName availableDbs of
        Nothing -> return $ Left $ "Database not found: " <> dbName
        Just dbConfig -> do
            -- Check if it's an uploaded database (only uploaded can be deleted)
            if not (dcIsUploaded dbConfig)
                then return $ Left $ "Cannot delete configured database. Edit fplca.toml to remove it."
                else if M.member dbName loadedDbs
                    then return $ Left $ "Cannot delete loaded database. Close it first."
                    else do
                            -- Get the upload directory (uploads/<slug>/)
                            uploadsDir <- UploadedDB.getUploadsDir
                            let uploadDir = uploadsDir </> T.unpack dbName
                            pathExists <- doesDirectoryExist uploadDir
                            if pathExists
                                then do
                                    -- Delete the database directory immediately
                                    result <- try $ removeDirectoryRecursive uploadDir
                                    case result of
                                        Left (e :: SomeException) ->
                                            return $ Left $ "Failed to delete: " <> T.pack (show e)
                                        Right () -> do
                                            reportProgress Info $ "Deleted: " <> uploadDir
                                            deleteCacheFile dbName
                                            removeFromMemory manager dbName
                                else do
                                    -- Directory already missing, just remove from memory
                                    reportProgress Info $ "Directory already missing: " <> uploadDir
                                    removeFromMemory manager dbName
  where
    try :: IO a -> IO (Either SomeException a)
    try = Control.Exception.try
    deleteCacheFile name = do
        cacheFile <- Loader.generateMatrixCacheFilename name ""
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

-- | Get setup info for a database (for the setup page)
-- Works for both staged and loaded databases
-- Auto-stages uploaded databases if they're not yet staged
-- Uses STM to prevent concurrent staging of the same database
getDatabaseSetupInfo :: DatabaseManager -> Text -> IO (Either SetupError DatabaseSetupInfo)
getDatabaseSetupInfo manager dbName = do
    -- Atomic decision: already staged? already staging? need to stage?
    action <- atomically $ do
        stagedDbs  <- readTVar (dmStagedDbs manager)
        loadedDbs  <- readTVar (dmLoadedDbs manager)
        stagingDbs <- readTVar (dmStagingDbs manager)
        case M.lookup dbName stagedDbs of
            Just _  -> return $ Right AlreadyDone
            Nothing -> case M.lookup dbName loadedDbs of
                Just _  -> return $ Right AlreadyDone
                Nothing -> if S.member dbName stagingDbs
                    then retry  -- another thread is staging; STM blocks until done
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
            let dbConfig = availableDbs M.! dbName  -- safe: checked above
            stageResult <- Control.Exception.finally
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
    stagedDbs    <- readTVarIO (dmStagedDbs manager)
    loadedDbs    <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    indexedDbs   <- readTVarIO (dmIndexedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged -> do
            let info = buildStagedSetupInfo staged availableDbs indexedDbs
            -- Populate available paths for uploaded databases
            if dcIsUploaded (sdConfig staged)
                then do
                    candidates <- discoverCandidatePaths (sdConfig staged)
                    return $ Right info { dsiAvailablePaths = candidates }
                else return $ Right info
        Nothing -> case M.lookup dbName loadedDbs of
            Just loaded ->
                let info = buildLoadedSetupInfo (ldConfig loaded) (ldDatabase loaded)
                    suggestions = buildDependencySuggestions' availableDbs indexedDbs
                    nUnresolved = unresolvedCount (dbLinkingStats (ldDatabase loaded))
                in return $ Right info
                    { dsiIsLoaded = False
                    , dsiIsReady = nUnresolved == 0
                    , dsiSuggestions = suggestions
                    }
            Nothing -> return $ Left $ SetupFailed $ "Failed to stage database: " <> dbName

-- | Build setup info from a staged database
-- dataPath and availablePaths are filled in by buildSetupResult (requires IO)
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
        completeness = if totalInputs > 0
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
        , dsiLocationFallbacks = cdlLocationFallbacks stats
        , dsiDataPath = T.pack (dcPath (sdConfig staged))
        , dsiAvailablePaths = []  -- Filled in by buildSetupResult (requires IO)
        , dsiIsLoaded = False
        }

-- | Build setup info from a loaded database (already finalized)
-- Uses dbLinkingStats for real completeness/fallback data
buildLoadedSetupInfo :: DatabaseConfig -> Database -> DatabaseSetupInfo
buildLoadedSetupInfo config db =
    let stats = dbLinkingStats db
        totalInputs = cdlTotalInputs stats
        nCrossDBLinks = length (dbCrossDBLinks db)
        nUnresolved = unresolvedCount stats
        resolved = totalInputs - nUnresolved
        completeness = if totalInputs > 0
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
        , dsiSuggestions = []  -- No suggestions for loaded databases
        , dsiIsReady = True
        , dsiUnknownUnits = S.toList (cdlUnknownUnits stats)
        , dsiLocationFallbacks = cdlLocationFallbacks stats
        , dsiDataPath = T.pack (dcPath config)
        , dsiAvailablePaths = []  -- No picker for loaded/configured databases
        , dsiIsLoaded = True
        }

-- | Discover candidate data paths within an uploaded database's root directory.
-- Returns (relativePath, formatLabel, fileCount) for each candidate.
discoverCandidatePaths :: DatabaseConfig -> IO [(Text, Text, Int)]
discoverCandidatePaths dbConfig = do
    uploadsDir <- UploadedDB.getUploadsDir
    let uploadRoot = uploadsDir </> T.unpack (dcName dbConfig)
    candidates <- Upload.findAllDataDirectories uploadRoot
    forM candidates $ \dir -> do
        format <- Upload.detectDatabaseFormat dir
        count  <- Upload.countDataFilesIn dir
        let rel = makeRelativePath uploadRoot dir
            label = case format of
                Upload.EcoSpold2     -> "EcoSpold 2"
                Upload.EcoSpold1     -> "EcoSpold 1"
                Upload.SimaProCSV    -> "SimaPro CSV"
                Upload.UnknownFormat -> "Unknown"
        return (T.pack rel, label, count)
  where
    -- Simple relative path: strip upload root prefix
    makeRelativePath base path
        | base `isPrefixOf` path = let r = drop (length base + 1) path
                                   in if null r then "." else r
        | otherwise = path

-- | Change the data path for an uploaded (staged) database.
-- Validates path, updates config + meta.toml, clears staged DB to force re-stage.
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
                uploadsDir <- UploadedDB.getUploadsDir
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
                        let updatedConfig = dbConfig
                                { dcPath = newFullPath
                                , dcFormat = Just newFormat
                                }
                        atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert dbName updatedConfig)

                        -- Update meta.toml
                        mMeta <- UploadedDB.readUploadMeta uploadRoot
                        case mMeta of
                            Just meta -> UploadedDB.writeUploadMeta uploadRoot meta
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

-- | Re-stage a loaded database for dependency editing
-- Moves from dmLoadedDbs → dmStagedDbs, cleans up solver
restageLoadedDatabase :: DatabaseManager -> Text -> LoadedDatabase -> IO StagedDatabase
restageLoadedDatabase manager dbName ld = do
    let db = ldDatabase ld
        stats = dbLinkingStats db
        staged = StagedDatabase
            { sdSimpleDB = toSimpleDatabase db
            , sdConfig = ldConfig ld
            , sdUnlinkedCount = unresolvedCount stats
            , sdMissingProducts = sortOn (\(_, cnt, _) -> Down cnt)
                [(n, cnt, blocker) | (n, (cnt, blocker)) <- M.toList (cdlUnresolvedProducts stats)]
            , sdSelectedDeps = dbDependsOn db
            , sdCrossDBLinks = dbCrossDBLinks db
            , sdLinkingStats = stats
            }
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.delete dbName)
        modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
    clearCachedKspSolver dbName
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

-- | Add a dependency to a staged (or partially-linked loaded) database
-- Runs cross-DB linking against the new dependency
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
                let newDeps = if depName `elem` sdSelectedDeps staged
                        then sdSelectedDeps staged
                        else depName : sdSelectedDeps staged
                    selectedIndexes = [idx | (name, idx) <- M.toList indexedDbs, name `elem` newDeps]
                (_, newStats) <- Loader.fixActivityLinksWithCrossDB
                    selectedIndexes
                    (dmSynonymDB manager)
                    (dmUnitConfig manager)
                    (sdSimpleDB staged)

                -- Update staged database with new stats and dependency
                let updatedStaged = staged
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
            (_, newStats) <- Loader.fixActivityLinksWithCrossDB
                remainingIndexes
                (dmSynonymDB manager)
                (dmUnitConfig manager)
                (sdSimpleDB staged)

            -- Update staged database
            let updatedStaged = staged
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
        Nothing -> return $ Left $ "Staged database not found: " <> dbName
        Just staged -> do
            -- Reject finalization if there are unresolved links
            let unlinked = Loader.countUnlinkedExchanges (sdSimpleDB staged)
                crossDBLinks = Loader.crossDBLinksCount (sdLinkingStats staged)
                unresolvedLinks = max 0 (unlinked - crossDBLinks)
            let activityCount = M.size (sdbActivities (sdSimpleDB staged))
            if activityCount == 0
              then return $ Left $
                "Cannot finalize: database contains 0 activities. "
                <> "The data file may be corrupted or in an unsupported format."
            else if unresolvedLinks > 0
              then return $ Left $
                "Cannot finalize: " <> T.pack (show unresolvedLinks)
                <> " unresolved inputs. Add dependencies to resolve them first."
              else do
                reportProgress Info $ "Finalizing database: " <> T.unpack dbName

                -- Build the database with matrices
                !db <- buildDatabaseWithMatrices
                    (sdbActivities (sdSimpleDB staged))
                    (sdbFlows (sdSimpleDB staged))
                    (sdbUnits (sdSimpleDB staged))

                -- Add cross-DB links, dependency info, and linking stats
                let dbWithLinks = db
                        { dbCrossDBLinks = sdCrossDBLinks staged
                        , dbDependsOn = sdSelectedDeps staged
                        , dbLinkingStats = sdLinkingStats staged
                        }

                -- Initialize runtime fields
                let dbWithRuntime = initializeRuntimeFields dbWithLinks (dmSynonymDB manager)

                -- Precompute factorization and create shared solver
                let techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples dbWithRuntime)]
                    activityCountInt = fromIntegral $ dbActivityCount dbWithRuntime
                factorization <- precomputeMatrixFactorization dbName techTriplesInt activityCountInt
                let dbWithFactorization = addFactorizationToDatabase dbWithRuntime factorization

                -- Create shared solver with the factorization
                sharedSolver <- createSharedSolver (Just factorization) techTriplesInt activityCountInt

                let loaded = LoadedDatabase
                        { ldDatabase = dbWithFactorization
                        , ldSharedSolver = sharedSolver
                        , ldConfig = sdConfig staged
                        }

                -- Move from staged to loaded
                let indexedDb = buildIndexedDatabaseFromDB dbName (dmSynonymDB manager) dbWithFactorization
                atomically $ do
                    modifyTVar' (dmStagedDbs manager) (M.delete dbName)
                    modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                    modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)

                -- Save to cache
                Loader.saveCachedDatabaseWithMatrices dbName (dcPath (sdConfig staged)) dbWithFactorization

                reportProgress Info $ "  [OK] Finalized: " <> T.unpack dbName
                return $ Right loaded

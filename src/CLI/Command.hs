{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Command where

import CLI.Types (CLIConfig (..), Command (..), DatabaseAction (..), DebugMatricesOptions (..), FlowSubCommand (..), GlobalOptions (..), LCIAOptions (..), MappingOptions (..), MethodAction (..), OutputFormat (..), PluginAction (..), SearchActivitiesOptions (..), SearchFlowsOptions (..), UploadArgs (..))
import Config (DatabaseConfig (..), MethodConfig (..))
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), addDatabase, addMethodCollection)
import qualified Database.Manager as DM
import Database.Upload (UploadData (..), UploadResult (..), findMethodDirectory, handleUpload)
import qualified Database.Upload
import qualified Database.UploadedDatabase as UploadedDB
import Method.Mapping (MappingStats (..), MatchStrategy (..), computeMappingStats, mapMethodToFlows)
import Method.Types (MethodCF (..))
import qualified Method.Types
import Plugin.Types (AnalyzeHandle (..), ExportContext (..), ExportHandle (..), ImportHandle (..), MapperHandle (..), PluginBackend (..), PluginRegistry (..), ReportHandle (..), SearchHandle (..), TransformHandle (..), ValidateHandle (..))
import Progress
import qualified Service
import SharedSolver (SharedSolver)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Types (Database)
import qualified Types
import UnitConversion (defaultUnitConfig)

-- | Default output format for different command types
defaultFormat :: Command -> OutputFormat
defaultFormat (Server _) = JSON -- Server always returns JSON
defaultFormat _ = Pretty -- All other commands default to Pretty

-- | Resolve output format using command-specific defaults
resolveOutputFormat :: GlobalOptions -> Command -> OutputFormat
resolveOutputFormat globalOpts cmd = case format globalOpts of
    Just fmt -> fmt -- Explicit --format overrides everything
    Nothing -> defaultFormat cmd -- Use command-specific default

-- | Look up a database from the manager by name, or use the single loaded one
requireDatabase :: DatabaseManager -> Maybe Text -> IO (Database, SharedSolver)
requireDatabase manager mName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case mName of
        Just name ->
            case M.lookup name loadedDbs of
                Just ld -> return (ldDatabase ld, ldSharedSolver ld)
                Nothing -> do
                    let available = map T.unpack (M.keys loadedDbs)
                    reportError $ "Database '" ++ T.unpack name ++ "' not found. Available: " ++ unwords available
                    exitFailure
        Nothing ->
            case M.elems loadedDbs of
                [ld] -> return (ldDatabase ld, ldSharedSolver ld)
                [] -> do
                    reportError "No databases loaded"
                    exitFailure
                _ -> do
                    let available = map T.unpack (M.keys loadedDbs)
                    reportError $ "Multiple databases loaded, use --db to select one: " ++ unwords available
                    exitFailure

-- | Output result using the plugin registry's reporter handles.
outputResult :: PluginRegistry -> OutputFormat -> Value -> IO ()
outputResult registry fmt result =
    case M.lookup (formatToId fmt) (prReporters registry) of
        Just reporter -> rhReport reporter result >>= BSL.putStrLn
        Nothing -> BSL.putStrLn (encode result) -- fallback
  where
    formatToId JSON = "json"
    formatToId CSV = "csv"
    formatToId Table = "table"
    formatToId Pretty = "pretty"

-- | Execute a CLI command with global options
executeCommand :: CLIConfig -> Command -> DatabaseManager -> IO ()
executeCommand (CLIConfig globalOpts _) cmd manager = do
    let outputFormat = resolveOutputFormat globalOpts cmd
        registry = dmPlugins manager
        out = outputResult registry outputFormat

    case cmd of
        -- Manager-level commands (no database required)
        Server _ -> do
            reportError "Server mode should be handled in Main.hs"
            exitFailure
        Database DbList ->
            DM.listDatabases manager >>= out . toJSON
        Database (DbUpload args) ->
            executeDbUpload registry outputFormat manager args
        Database (DbDelete name) ->
            executeDbDelete registry outputFormat manager name
        Method McList ->
            DM.listMethodCollections manager >>= out . toJSON
        Method (McUpload args) ->
            executeMcUpload registry outputFormat manager args
        Method (McDelete name) ->
            executeMcDelete registry outputFormat manager name
        Methods -> do
            pairs <- DM.getLoadedMethods manager
            let val = toJSON [object ["collection" .= col, "method" .= m] | (col, m) <- pairs]
            out val
        Synonyms ->
            DM.listFlowSynonyms manager >>= out . toJSON
        CompartmentMappings ->
            DM.listCompartmentMappings manager >>= out . toJSON
        Units ->
            DM.listUnitDefs manager >>= out . toJSON
        Plugin PluginList ->
            executePluginList registry outputFormat
        FlowMapping opts -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeFlowMappingCommand registry outputFormat database manager opts

        -- Database-level commands
        Activity _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        Inventory _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        Flow _ _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        SearchActivities _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        SearchFlows _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        Impacts _ _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        DebugMatrices _ _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        ExportMatrices _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand registry outputFormat globalOpts database cmd
        Stop -> do
            reportError "Stop command should be handled in Main.hs"
            exitFailure
        Repl -> do
            reportError "Repl command should be handled in Main.hs"
            exitFailure
        DumpOpenApi -> do
            reportError "DumpOpenApi should be handled in Main.hs"
            exitFailure
        DumpMcpTools -> do
            reportError "DumpMcpTools should be handled in Main.hs"
            exitFailure

-- | Execute commands that require a loaded database
executeDbCommand :: PluginRegistry -> OutputFormat -> GlobalOptions -> Database -> Command -> IO ()
executeDbCommand registry fmt _globalOpts database = \case
    Activity uuid ->
        executeActivityCommand registry fmt database uuid
    Inventory uuid ->
        executeActivityInventoryCommand registry fmt database uuid
    Flow flowId Nothing ->
        executeFlowCommand registry fmt database flowId
    Flow flowId (Just FlowActivities) ->
        executeFlowActivitiesCommand registry fmt database flowId
    SearchActivities opts ->
        executeSearchActivitiesCommand registry fmt database opts
    SearchFlows opts ->
        executeSearchFlowsCommand registry fmt database opts
    Impacts uuid lciaOpts ->
        executeImpactsCommand registry fmt database uuid lciaOpts
    DebugMatrices uuid debugOpts ->
        executeDebugMatricesCommand database uuid debugOpts
    ExportMatrices outputDir ->
        executeExportMatricesCommand registry database outputDir
    -- Manager-level commands already handled above; should never be reached here
    Server _ -> pure ()
    Database _ -> pure ()
    Method _ -> pure ()
    Plugin _ -> pure ()
    Methods -> pure ()
    Synonyms -> pure ()
    CompartmentMappings -> pure ()
    Units -> pure ()
    FlowMapping _ -> pure ()
    Stop -> pure ()
    Repl -> pure ()
    DumpOpenApi -> pure ()
    DumpMcpTools -> pure ()

-- | Execute activity info command
executeActivityCommand :: PluginRegistry -> OutputFormat -> Database -> T.Text -> IO ()
executeActivityCommand registry fmt database uuid =
    case Service.getActivityInfo defaultUnitConfig database uuid of
        Left err -> reportServiceError err
        Right result -> outputResult registry fmt result

-- | Execute activity inventory command
executeActivityInventoryCommand :: PluginRegistry -> OutputFormat -> Database -> T.Text -> IO ()
executeActivityInventoryCommand registry fmt database uuid = do
    reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
    result <- Service.getActivityInventory database uuid
    case result of
        Left err -> reportServiceError err
        Right value -> do
            reportProgress Info "Inventory computation completed"
            outputResult registry fmt value

-- | Execute flow info command
executeFlowCommand :: PluginRegistry -> OutputFormat -> Database -> T.Text -> IO ()
executeFlowCommand registry fmt database flowId =
    case Service.getFlowInfo database flowId of
        Left err -> reportServiceError err
        Right result -> outputResult registry fmt result

-- | Execute flow activities command
executeFlowActivitiesCommand :: PluginRegistry -> OutputFormat -> Database -> T.Text -> IO ()
executeFlowActivitiesCommand registry fmt database flowId =
    case Service.getFlowActivities database flowId of
        Left err -> reportServiceError err
        Right result -> outputResult registry fmt result

-- | Execute search activities command
executeSearchActivitiesCommand :: PluginRegistry -> OutputFormat -> Database -> SearchActivitiesOptions -> IO ()
executeSearchActivitiesCommand registry fmt database opts = do
    let sf =
            Service.SearchFilter
                { Service.sfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = searchName opts
                        , Service.afcLocation = searchGeo opts
                        , Service.afcProduct = searchProduct opts
                        , Service.afcClassifications = []
                        , Service.afcLimit = searchLimit opts
                        , Service.afcOffset = searchOffset opts
                        , Service.afcSort = Nothing
                        , Service.afcOrder = Nothing
                        }
                , Service.sfExactMatch = False
                }
    searchResult <- Service.searchActivities database sf
    case searchResult of
        Left err -> reportServiceError err
        Right result -> outputResult registry fmt result

-- | Execute search flows command
executeSearchFlowsCommand :: PluginRegistry -> OutputFormat -> Database -> SearchFlowsOptions -> IO ()
executeSearchFlowsCommand registry fmt database opts =
    case searchQuery opts of
        Nothing -> outputResult registry fmt Service.emptyFlowSearchResults
        Just query -> do
            let ff =
                    Service.FlowFilter
                        { Service.ffQuery = query
                        , Service.ffLang = searchLang opts
                        , Service.ffLimit = searchFlowsLimit opts
                        , Service.ffOffset = searchFlowsOffset opts
                        , Service.ffSort = Nothing
                        , Service.ffOrder = Nothing
                        }
            searchResult <- Service.searchFlows database ff
            case searchResult of
                Left err -> reportServiceError err
                Right result -> outputResult registry fmt result

-- | Impacts (LCIA) is now handled via HTTP client (see CLI.Client)
executeImpactsCommand :: PluginRegistry -> OutputFormat -> Database -> T.Text -> LCIAOptions -> IO ()
executeImpactsCommand _ _ _ _ _ = do
    reportError "impacts is only available via HTTP. Start the server first: volca --config volca.toml server"
    exitFailure

-- | Execute matrix debugging command
executeDebugMatricesCommand :: Database -> T.Text -> DebugMatricesOptions -> IO ()
executeDebugMatricesCommand database uuid opts = do
    reportProgress Info $ "Extracting matrix debug data for activity: " ++ T.unpack uuid
    reportProgress Info $ "Output base: " ++ debugOutput opts

    case debugFlowFilter opts of
        Just flowFilter -> reportProgress Info $ "Flow filter: " ++ T.unpack flowFilter
        Nothing -> reportProgress Info "No flow filter specified (all biosphere flows)"

    result <- Service.exportMatrixDebugData database uuid opts
    case result of
        Left err -> reportServiceError err
        Right _ -> do
            reportProgress Info "Matrix debug export completed"
            reportProgress Info $ "Supply chain data: " ++ debugOutput opts ++ "_supply_chain.csv"
            reportProgress Info $ "Biosphere matrix: " ++ debugOutput opts ++ "_biosphere_matrix.csv"

-- | Execute export matrices command using plugin registry
executeExportMatricesCommand :: PluginRegistry -> Database -> FilePath -> IO ()
executeExportMatricesCommand registry database outputDir = do
    reportProgress Info $ "Exporting matrices to: " ++ outputDir
    case M.lookup "ecoinvent-matrix" (prExporters registry) of
        Just exporter -> do
            let ctx = ExportContext{ecDatabase = database, ecInventory = Nothing, ecMethods = []}
            ehExport exporter ctx outputDir
        Nothing -> Service.exportUniversalMatrixFormat outputDir database
    reportProgress Info "Matrix export completed"
    reportProgress Info $ "  - ie_index.csv (activity index)"
    reportProgress Info $ "  - ee_index.csv (biosphere flow index)"
    reportProgress Info $ "  - A_public.csv (technosphere matrix)"
    reportProgress Info $ "  - B_public.csv (biosphere matrix)"

-- | Execute plugin list command
executePluginList :: PluginRegistry -> OutputFormat -> IO ()
executePluginList registry fmt = do
    let noPriority = Nothing :: Maybe Int
        plugins =
            concat
                [ [pluginEntry (mhName m) "mapper" (mhBackend m) (Just $ mhPriority m) | m <- prMappers registry]
                , [pluginEntry (rhName r) "reporter" (rhBackend r) noPriority | r <- M.elems (prReporters registry)]
                , [pluginEntry (ehName e) "exporter" (ehBackend e) noPriority | e <- M.elems (prExporters registry)]
                , [pluginEntry (ahName a) "analyzer" (ahBackend a) noPriority | a <- M.elems (prAnalyzers registry)]
                , [pluginEntry (thName t) "transform" (thBackend t) (Just $ thPriority t) | t <- prTransforms registry]
                , [pluginEntry (vhName v) "validator" (vhBackend v) noPriority | v <- prValidators registry]
                , [pluginEntry (shName s) "searcher" (shBackend s) (Just $ shPriority s) | s <- prSearchers registry]
                , [pluginEntry (ihName i) "importer" (ihBackend i) noPriority | i <- prImporters registry]
                ]
    outputResult registry fmt (toJSON plugins)
  where
    pluginEntry name typ backend mPriority =
        object $
            concat
                [
                    [ "name" .= name
                    , "type" .= (typ :: T.Text)
                    , "backend" .= backendText backend
                    ]
                , maybe [] (\p -> ["priority" .= p]) mPriority
                ]
    backendText Builtin = "builtin" :: T.Text
    backendText (External p) = "external:" <> T.pack p

-- | Execute database upload command
executeDbUpload :: PluginRegistry -> OutputFormat -> DatabaseManager -> UploadArgs -> IO ()
executeDbUpload registry fmt manager args = do
    reportProgress Info $ "Reading file: " ++ uaFile args
    fileData <- BL.readFile (uaFile args)

    let uploadData =
            UploadData
                { udName = uaName args
                , udDescription = uaDescription args
                , udZipData = fileData
                }

    uploadsDir <- UploadedDB.getDatabaseUploadsDir

    let progress pe = reportProgress Info $ T.unpack (Database.Upload.pePhase pe) ++ ": " ++ T.unpack (Database.Upload.peMessage pe)

    result <- handleUpload uploadsDir uploadData progress
    case result of
        Left err -> do
            reportError $ "Upload failed: " ++ T.unpack err
            exitFailure
        Right uploadResult -> do
            let slug = urSlug uploadResult
                uploadDir = uploadsDir </> T.unpack slug

            let meta =
                    UploadedDB.UploadMeta
                        { UploadedDB.umVersion = 1
                        , UploadedDB.umDisplayName = uaName args
                        , UploadedDB.umDescription = uaDescription args
                        , UploadedDB.umFormat = urFormat uploadResult
                        , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                        }
            UploadedDB.writeUploadMeta uploadDir meta

            let dbConfig =
                    DatabaseConfig
                        { dcName = slug
                        , dcDisplayName = uaName args
                        , dcPath = urPath uploadResult
                        , dcDescription = uaDescription args
                        , dcLoad = False
                        , dcDefault = False
                        , dcDepends = []
                        , dcLocationAliases = M.empty
                        , dcFormat = Just (urFormat uploadResult)
                        , dcIsUploaded = True
                        }
            addDatabase manager dbConfig
            reportProgress Info $ "Database uploaded: " ++ T.unpack slug

            outputResult registry fmt $
                object
                    [ "slug" .= slug
                    , "format" .= urFormat uploadResult
                    , "fileCount" .= urFileCount uploadResult
                    , "path" .= urPath uploadResult
                    ]
  where
    makeRelative base path
        | base `isPrefixOf` path = drop (length base + 1) path
        | otherwise = path

-- | Execute method upload command
executeMcUpload :: PluginRegistry -> OutputFormat -> DatabaseManager -> UploadArgs -> IO ()
executeMcUpload registry fmt manager args = do
    reportProgress Info $ "Reading file: " ++ uaFile args
    fileData <- BL.readFile (uaFile args)

    let uploadData =
            UploadData
                { udName = uaName args
                , udDescription = uaDescription args
                , udZipData = fileData
                }

    uploadsDir <- UploadedDB.getMethodUploadsDir

    let progress pe = reportProgress Info $ T.unpack (Database.Upload.pePhase pe) ++ ": " ++ T.unpack (Database.Upload.peMessage pe)

    result <- handleUpload uploadsDir uploadData progress
    case result of
        Left err -> do
            reportError $ "Upload failed: " ++ T.unpack err
            exitFailure
        Right uploadResult -> do
            let slug = urSlug uploadResult
                uploadDir = uploadsDir </> T.unpack slug

            let meta =
                    UploadedDB.UploadMeta
                        { UploadedDB.umVersion = 1
                        , UploadedDB.umDisplayName = uaName args
                        , UploadedDB.umDescription = uaDescription args
                        , UploadedDB.umFormat = urFormat uploadResult
                        , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                        }
            UploadedDB.writeUploadMeta uploadDir meta

            methodDir <- findMethodDirectory uploadDir
            let mc =
                    MethodConfig
                        { mcName = uaName args
                        , mcPath = methodDir
                        , mcActive = False
                        , mcIsUploaded = True
                        , mcDescription = uaDescription args
                        , mcFormat = Nothing
                        , mcScoringSets = []
                        }
            addMethodCollection manager mc
            reportProgress Info $ "Method uploaded: " ++ T.unpack slug

            outputResult registry fmt $
                object
                    [ "slug" .= slug
                    , "format" .= urFormat uploadResult
                    , "fileCount" .= urFileCount uploadResult
                    , "path" .= urPath uploadResult
                    ]
  where
    makeRelative base path
        | base `isPrefixOf` path = drop (length base + 1) path
        | otherwise = path

-- | Execute database delete command
executeDbDelete :: PluginRegistry -> OutputFormat -> DatabaseManager -> Text -> IO ()
executeDbDelete registry fmt manager name = do
    result <- DM.removeDatabase manager name
    case result of
        Left err -> do
            reportError $ "Delete failed: " ++ T.unpack err
            exitFailure
        Right () -> do
            reportProgress Info $ "Deleted database: " ++ T.unpack name
            outputResult registry fmt $ object ["deleted" .= name]

-- | Execute method delete command
executeMcDelete :: PluginRegistry -> OutputFormat -> DatabaseManager -> Text -> IO ()
executeMcDelete registry fmt manager name = do
    result <- DM.removeMethodCollection manager name
    case result of
        Left err -> do
            reportError $ "Delete failed: " ++ T.unpack err
            exitFailure
        Right () -> do
            reportProgress Info $ "Deleted method: " ++ T.unpack name
            outputResult registry fmt $ object ["deleted" .= name]

-- | Execute mapping command: analyze flow mapping coverage
executeFlowMappingCommand :: PluginRegistry -> OutputFormat -> Types.Database -> DatabaseManager -> MappingOptions -> IO ()
executeFlowMappingCommand registry fmt database manager opts = do
    -- Find method by UUID
    loadedMethods <- DM.getLoadedMethods manager
    let allMethods = map snd loadedMethods
    case UUID.fromText (mappingMethodId opts) of
        Nothing -> do
            reportError $ "Invalid method UUID: " ++ T.unpack (mappingMethodId opts)
            exitFailure
        Just uuid ->
            case filter (\m -> Method.Types.methodId m == uuid) allMethods of
                [] -> do
                    reportError $ "Method not found: " ++ T.unpack (mappingMethodId opts)
                    exitFailure
                (method : _) -> do
                    mappings <- mapMethodToFlows (prMappers registry) database method
                    let stats = computeMappingStats mappings
                        totalMatched = msTotal stats - msUnmatched stats
                        coverage =
                            if msTotal stats > 0
                                then fromIntegral totalMatched / fromIntegral (msTotal stats) * 100 :: Double
                                else 0.0
                        dbBioCount = fromIntegral (Types.dbBiosphereCount database) :: Int
                        characterizedUUIDs =
                            S.fromList
                                [Types.flowId f | (_cf, Just (f, _)) <- mappings]
                        characterizedCount = S.size characterizedUUIDs
                        uncharacterizedCount = dbBioCount - characterizedCount
                        charCoverage =
                            if dbBioCount > 0
                                then fromIntegral characterizedCount / fromIntegral dbBioCount * 100 :: Double
                                else 0.0

                    let prettyOutput = do
                            putStrLn $ "Method: " ++ T.unpack (Method.Types.methodName method)
                            putStrLn $ "Total CFs: " ++ show (msTotal stats)
                            putStrLn $ "Matched:   " ++ show totalMatched ++ " (" ++ showPercent coverage ++ ")"
                            putStrLn $ "  by UUID:    " ++ show (msByUUID stats)
                            putStrLn $ "  by CAS:     " ++ show (msByCAS stats)
                            putStrLn $ "  by Name:    " ++ show (msByName stats)
                            putStrLn $ "  by Synonym: " ++ show (msBySynonym stats)
                            when (msByFuzzy stats > 0) $
                                putStrLn $
                                    "  by Fuzzy:   " ++ show (msByFuzzy stats)
                            putStrLn $ "Unmatched:  " ++ show (msUnmatched stats)
                            putStrLn ""
                            putStrLn $ "DB biosphere flows: " ++ show dbBioCount
                            putStrLn $ "Characterized:      " ++ show characterizedCount ++ " (" ++ showPercent charCoverage ++ ")"
                            putStrLn $ "Uncharacterized:    " ++ show uncharacterizedCount

                            when (mappingShowMatched opts) $ do
                                putStrLn ""
                                putStrLn "--- Matched CFs ---"
                                mapM_
                                    ( \(cf, f, strat) ->
                                        putStrLn $
                                            "  ["
                                                ++ T.unpack (strategyText strat)
                                                ++ "] "
                                                ++ T.unpack (mcfFlowName cf)
                                                ++ " → "
                                                ++ T.unpack (Types.flowName f)
                                    )
                                    [(cf, f, strat) | (cf, Just (f, strat)) <- mappings]

                            when (mappingShowUnmatched opts) $ do
                                putStrLn ""
                                putStrLn "--- Unmatched CFs (no DB flow found) ---"
                                mapM_
                                    ( \(cf, _) ->
                                        putStrLn $
                                            "  "
                                                ++ T.unpack (mcfFlowName cf)
                                                ++ maybe "" (\c -> " [CAS " ++ T.unpack c ++ "]") (mcfCAS cf)
                                    )
                                    [(cf, m) | (cf, m@Nothing) <- mappings]

                            when (mappingShowUncharacterized opts) $ do
                                putStrLn ""
                                putStrLn "--- Uncharacterized DB flows (no CF matched) ---"
                                mapM_
                                    (\name -> putStrLn $ "  " ++ T.unpack name)
                                    (uncharacterizedFlowNames database characterizedUUIDs)

                    case fmt of
                        JSON ->
                            outputResult registry JSON $
                                toJSON $
                                    object
                                        [ "method" .= Method.Types.methodName method
                                        , "totalCFs" .= msTotal stats
                                        , "matched" .= totalMatched
                                        , "matchedPercent" .= coverage
                                        , "byUUID" .= msByUUID stats
                                        , "byCAS" .= msByCAS stats
                                        , "byName" .= msByName stats
                                        , "bySynonym" .= msBySynonym stats
                                        , "byFuzzy" .= msByFuzzy stats
                                        , "unmatched" .= msUnmatched stats
                                        , "dbBiosphereFlows" .= dbBioCount
                                        , "characterized" .= characterizedCount
                                        , "uncharacterized" .= uncharacterizedCount
                                        , "characterizedPercent" .= charCoverage
                                        , "matchedCFs"
                                            .= if mappingShowMatched opts
                                                then
                                                    toJSON
                                                        [ object
                                                            [ "cfName" .= mcfFlowName cf
                                                            , "dbFlowName" .= Types.flowName f
                                                            , "strategy" .= strategyText strat
                                                            , "cfUUID" .= mcfFlowRef cf
                                                            , "dbFlowUUID" .= Types.flowId f
                                                            ]
                                                        | (cf, Just (f, strat)) <- mappings
                                                        ]
                                                else toJSON (Nothing :: Maybe Value)
                                        , "unmatchedCFs"
                                            .= if mappingShowUnmatched opts
                                                then
                                                    toJSON
                                                        [ object ["name" .= mcfFlowName cf, "uuid" .= mcfFlowRef cf, "cas" .= mcfCAS cf]
                                                        | (cf, Nothing) <- mappings
                                                        ]
                                                else toJSON (Nothing :: Maybe Value)
                                        , "uncharacterizedFlows"
                                            .= if mappingShowUncharacterized opts
                                                then toJSON (uncharacterizedFlowNames database characterizedUUIDs)
                                                else toJSON (Nothing :: Maybe Value)
                                        ]
                        Pretty -> prettyOutput
                        Table -> prettyOutput
                        CSV -> prettyOutput
  where
    showPercent :: Double -> String
    showPercent p = show (round (p * 10) `div` 10 :: Int) ++ "." ++ show (round (p * 10) `mod` 10 :: Int) ++ "%"

    when True action = action
    when False _ = pure ()

strategyText :: MatchStrategy -> Text
strategyText ByUUID = "uuid"
strategyText ByCAS = "cas"
strategyText ByName = "name"
strategyText BySynonym = "synonym"
strategyText ByFuzzy = "fuzzy"
strategyText NoMatch = "none"

-- | Get names of biosphere flows not matched by any CF
uncharacterizedFlowNames :: Types.Database -> S.Set UUID.UUID -> [Text]
uncharacterizedFlowNames db characterized =
    [ Types.flowName f
    | uuid <- V.toList (Types.dbBiosphereFlows db)
    , not (S.member uuid characterized)
    , Just f <- [M.lookup uuid (Types.dbFlows db)]
    ]

-- | Report service errors to stderr and exit
reportServiceError :: Service.ServiceError -> IO ()
reportServiceError err = do
    reportError $ "Error: " ++ show err
    exitFailure

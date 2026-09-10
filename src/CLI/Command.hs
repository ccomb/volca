{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Command where

import API.Types (ActivityInput, ActivityWriteRequest (..), ProducerFilter (..), toAuthoredActivities, toExchangeEdits)
import CLI.Client (readJsonFile)
import CLI.Types (CLIConfig (..), Command (..), DatabaseAction (..), DbActivityArgs (..), DbDeleteArgs (..), DbExportArgs (..), DbRelinkArgs (..), DbWriteArgs (..), DebugMatricesOptions (..), FlowSubCommand (..), GlobalOptions (..), LCIAOptions (..), MappingOptions (..), McExportArgs (..), MethodAction (..), OutputFormat (..), SearchActivitiesOptions (..), SearchFlowsOptions (..), UploadArgs (..))
import Config (DatabaseConfig (..), MethodConfig (..))
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (Geographies)
import Database.Edit (
    DeleteOutcome (..),
    DeleteRequest (..),
    EditReport (..),
    WriteReport (..),
    WriteVerb (..),
    copyDatabase,
    deleteActivitiesInDB,
    editExchanges,
    refusalMessage,
    writeActivities,
 )
import Database.Export (exportDatabase, exportMethodCollection, parseExportFormat, parseMethodExportFormat)
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), RelinkResult (..), addDatabase, addMethodCollection, managerGeographies)
import qualified Database.Manager as DM
import Database.RelinkMapping (relinkWithMappingFile)
import Database.Upload (UploadData (..), UploadResult (..), findMethodDirectory, handleUpload)
import qualified Database.Upload
import qualified Database.UploadedDatabase as UploadedDB
import Method.Mapping (MappingStats (..), computeMappingStats, mapMethodToFlows, strategyToText)
import Method.Types (MethodCF (..))
import qualified Method.Types
import Progress
import qualified Service
import SharedSolver (SharedSolver)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Types (Database)
import qualified Types

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

-- | Print a result value in the requested output format.
outputResult :: OutputFormat -> Value -> IO ()
outputResult fmt result = BSL.putStrLn (render result)
  where
    render = case fmt of
        JSON -> encode
        CSV -> encode
        Table -> encodePretty
        Pretty -> encodePretty

-- | Execute a CLI command with global options
executeCommand :: CLIConfig -> Command -> DatabaseManager -> IO ()
executeCommand (CLIConfig globalOpts _) cmd manager = do
    let outputFormat = resolveOutputFormat globalOpts cmd
        out = outputResult outputFormat

    case cmd of
        -- Manager-level commands (no database required)
        Server _ -> do
            reportError "Server mode should be handled in Main.hs"
            exitFailure
        Database DbList ->
            DM.listDatabases manager >>= out . toJSON
        Database (DbLoad name) ->
            executeDbLoad outputFormat manager name
        Database (DbUnload name) ->
            executeDbUnload outputFormat manager name
        Database (DbUpload args) ->
            executeDbUpload outputFormat manager args
        Database (DbDelete name) ->
            executeDbDelete outputFormat manager name
        Database (DbDeleteActivities args) ->
            executeDbDeleteActivities outputFormat manager args
        Database (DbCopy srcName newName) ->
            executeDbCopy outputFormat manager srcName newName
        Database (DbRelinkMapping args) ->
            executeDbRelinkMapping outputFormat manager args
        Database (DbExport args) ->
            executeDbExport outputFormat manager args
        Database (DbCreateActivities args) ->
            executeDbCreateActivities outputFormat manager args
        Database (DbReplaceActivity args) ->
            executeDbReplaceActivity outputFormat manager args
        Database (DbEditExchanges args) ->
            executeDbEditExchanges outputFormat manager args
        Method McList ->
            DM.listMethodCollections manager >>= out . toJSON
        Method (McUpload args) ->
            executeMcUpload outputFormat manager args
        Method (McDelete name) ->
            executeMcDelete outputFormat manager name
        Method (McExport args) ->
            executeMcExport outputFormat manager args
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
        FlowMapping opts -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeFlowMappingCommand outputFormat database manager opts

        -- Database-level commands
        Activity _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        Inventory _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        Flow _ _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        SearchActivities _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        SearchFlows _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        Impacts _ _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        DebugMatrices _ _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        ExportMatrices _ -> do
            (database, _solver) <- requireDatabase manager (dbName globalOpts)
            executeDbCommand outputFormat globalOpts (managerGeographies manager) database cmd
        QualityReport _ -> do
            reportError "quality-report is served over HTTP; Main.hs routes it to the client"
            exitFailure
        ComputedQualityReport _ -> do
            reportError "computed-quality-report is served over HTTP; Main.hs routes it to the client"
            exitFailure
        Stop -> do
            reportError "Stop command should be handled in Main.hs"
            exitFailure
        Repl -> do
            reportError "Repl command should be handled in Main.hs"
            exitFailure
        Dump _ -> reportError "A dump command is answered before a database is loaded."

-- | Execute commands that require a loaded database
executeDbCommand :: OutputFormat -> GlobalOptions -> Geographies -> Database -> Command -> IO ()
executeDbCommand fmt _globalOpts geographies database = \case
    Activity uuid ->
        executeActivityCommand fmt database uuid
    Inventory uuid ->
        executeActivityInventoryCommand fmt database uuid
    Flow flowId Nothing ->
        executeFlowCommand fmt database flowId
    Flow flowId (Just FlowActivities) ->
        executeFlowActivitiesCommand fmt database flowId
    SearchActivities opts ->
        executeSearchActivitiesCommand fmt geographies database opts
    SearchFlows opts ->
        executeSearchFlowsCommand fmt database opts
    Impacts uuid lciaOpts ->
        executeImpactsCommand fmt database uuid lciaOpts
    DebugMatrices uuid debugOpts ->
        executeDebugMatricesCommand database uuid debugOpts
    ExportMatrices outputDir ->
        executeExportMatricesCommand database outputDir
    -- Manager-level commands already handled above; should never be reached here
    Server _ -> pure ()
    Database _ -> pure ()
    Method _ -> pure ()
    Methods -> pure ()
    Synonyms -> pure ()
    CompartmentMappings -> pure ()
    Units -> pure ()
    FlowMapping _ -> pure ()
    QualityReport _ -> pure ()
    ComputedQualityReport _ -> pure ()
    Stop -> pure ()
    Repl -> pure ()
    Dump _ -> pure ()

-- | Execute activity info command
executeActivityCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityCommand fmt database uuid =
    case Service.getActivityInfo database uuid of
        Left err -> reportServiceError err
        Right result -> outputResult fmt result

-- | Execute activity inventory command
executeActivityInventoryCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityInventoryCommand fmt database uuid = do
    reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
    result <- Service.getActivityInventory database uuid
    case result of
        Left err -> reportServiceError err
        Right value -> do
            reportProgress Info "Inventory computation completed"
            outputResult fmt value

-- | Execute flow info command
executeFlowCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeFlowCommand fmt database flowId =
    case Service.getFlowInfo database flowId of
        Left err -> reportServiceError err
        Right result -> outputResult fmt result

-- | Execute flow activities command
executeFlowActivitiesCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeFlowActivitiesCommand fmt database flowId =
    case Service.getFlowActivities database EitherSide flowId of
        Left err -> reportServiceError err
        Right result -> outputResult fmt result

-- | Execute search activities command
executeSearchActivitiesCommand :: OutputFormat -> Geographies -> Database -> SearchActivitiesOptions -> IO ()
executeSearchActivitiesCommand fmt geographies database opts = do
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
    searchResult <- Service.searchActivities geographies database sf
    case searchResult of
        Left err -> reportServiceError err
        Right result -> outputResult fmt result

-- | Execute search flows command
executeSearchFlowsCommand :: OutputFormat -> Database -> SearchFlowsOptions -> IO ()
executeSearchFlowsCommand fmt database opts =
    case searchQuery opts of
        Nothing -> outputResult fmt Service.emptyFlowSearchResults
        Just query -> do
            let ff =
                    Service.FlowFilter
                        { Service.ffQuery = query
                        , Service.ffLang = searchLang opts
                        , Service.ffKind = Types.AnyKind
                        , Service.ffLimit = searchFlowsLimit opts
                        , Service.ffOffset = searchFlowsOffset opts
                        , Service.ffSort = Nothing
                        , Service.ffOrder = Nothing
                        }
            searchResult <- Service.searchFlows database ff
            case searchResult of
                Left err -> reportServiceError err
                Right result -> outputResult fmt result

-- | Impacts (LCIA) is now handled via HTTP client (see CLI.Client)
executeImpactsCommand :: OutputFormat -> Database -> T.Text -> LCIAOptions -> IO ()
executeImpactsCommand _ _ _ _ = do
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

-- | Execute export matrices command
executeExportMatricesCommand :: Database -> FilePath -> IO ()
executeExportMatricesCommand database outputDir = do
    reportProgress Info $ "Exporting matrices to: " ++ outputDir
    Service.exportUniversalMatrixFormat outputDir database
    reportProgress Info "Matrix export completed"
    reportProgress Info "  - ie_index.csv (activity index)"
    reportProgress Info "  - ee_index.csv (biosphere flow index)"
    reportProgress Info "  - A_public.csv (technosphere matrix)"
    reportProgress Info "  - B_public.csv (biosphere matrix)"

-- | Execute database upload command
executeDbUpload :: OutputFormat -> DatabaseManager -> UploadArgs -> IO ()
executeDbUpload fmt manager args = do
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
                        { UploadedDB.umVersion = UploadedDB.metaVersion
                        , UploadedDB.umDisplayName = uaName args
                        , UploadedDB.umDescription = uaDescription args
                        , UploadedDB.umFormat = urFormat uploadResult
                        , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                        , UploadedDB.umDepends = []
                        , UploadedDB.umSource = Nothing
                        , UploadedDB.umAllocation = Types.Declared
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
                        , dcDeletable = True
                        , dcGeographyPolicy = Types.GeoGlobal
                        , dcAllocation = Types.Declared
                        , dcSource = Nothing
                        }
            addDatabase manager dbConfig
            reportProgress Info $ "Database uploaded: " ++ T.unpack slug

            outputResult fmt $
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
executeMcUpload :: OutputFormat -> DatabaseManager -> UploadArgs -> IO ()
executeMcUpload fmt manager args = do
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
                        { UploadedDB.umVersion = UploadedDB.metaVersion
                        , UploadedDB.umDisplayName = uaName args
                        , UploadedDB.umDescription = uaDescription args
                        , UploadedDB.umFormat = urFormat uploadResult
                        , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                        , UploadedDB.umDepends = []
                        , UploadedDB.umSource = Nothing
                        , UploadedDB.umAllocation = Types.Declared
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
                        , mcGlobalMethods = []
                        , mcPatches = []
                        }
            addMethodCollection manager mc
            reportProgress Info $ "Method uploaded: " ++ T.unpack slug

            outputResult fmt $
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
executeDbDelete :: OutputFormat -> DatabaseManager -> Text -> IO ()
executeDbDelete fmt manager name = do
    result <- DM.removeDatabase manager name
    case result of
        Left err -> do
            reportError $ "Delete failed: " ++ T.unpack err
            exitFailure
        Right () -> do
            reportProgress Info $ "Deleted database: " ++ T.unpack name
            outputResult fmt $ object ["deleted" .= name]

{- | Execute delete-by-selection: deletes the whole filtered set (pagination
ignored) plus the explicit @--add@ ProcessIds, sparing @--keep@.
-}
executeDbDeleteActivities :: OutputFormat -> DatabaseManager -> DbDeleteArgs -> IO ()
executeDbDeleteActivities fmt manager args = do
    let classFilters = case (ddaClassSystem args, ddaClassValue args) of
            (Just sys, Just val) -> [(sys, val, ddaExact args)]
            _ -> []
    result <-
        deleteActivitiesInDB
            manager
            (ddaDb args)
            DeleteRequest
                { drName = ddaName args
                , drLocation = ddaLocation args
                , drProduct = ddaProduct args
                , drClassifications = classFilters
                , drExactName = ddaExact args
                , drKeep = ddaKeep args
                , drExtra = ddaExtra args
                , drIds = if null (ddaIds args) then Nothing else Just (ddaIds args)
                }
    case result of
        Left err -> do
            reportError $ "Delete failed: " ++ T.unpack err
            exitFailure
        Right outcome -> do
            reportProgress Info $ "Deleted " ++ show (doRemoved outcome) ++ " activities from " ++ T.unpack (ddaDb args)
            mapM_ (reportProgress Warning . T.unpack) (doWarnings outcome)
            outputResult fmt $
                object
                    [ "database" .= ddaDb args
                    , "deleted" .= doRemoved outcome
                    , "transient" .= not (doPersisted outcome)
                    , "warnings" .= doWarnings outcome
                    ]

{- | Write new activities into a database, read from a JSON file.

The file is the same document the HTTP endpoint accepts
(@{"activities": [...]}@), and the refusals are the same ones — the command
line and the server share the whole of authoring above the primitives, so
neither can allow what the other forbids.
-}
executeDbCreateActivities :: OutputFormat -> DatabaseManager -> DbWriteArgs -> IO ()
executeDbCreateActivities fmt manager args = do
    request <- readJsonFile (dwaFile args)
    case request of
        Left err -> reportError err >> exitFailure
        Right req -> runWrite fmt manager (dwaDb args) CreateActivities (awrActivities req)

-- | Rewrite one activity, addressed by the process id it already has.
executeDbReplaceActivity :: OutputFormat -> DatabaseManager -> DbActivityArgs -> IO ()
executeDbReplaceActivity fmt manager args = do
    activity <- readJsonFile (daFile args)
    case activity of
        Left err -> reportError err >> exitFailure
        Right body -> runWrite fmt manager (daDb args) (ReplaceActivity (daProcessId args)) [body]

{- | Change one activity's inventory, keeping everything else it carries.

Reaches what a rewrite cannot: an activity a database file brought in, whose
identity no description mints. The file is the same document the HTTP endpoint
accepts, and the refusals are the same ones.
-}
executeDbEditExchanges :: OutputFormat -> DatabaseManager -> DbActivityArgs -> IO ()
executeDbEditExchanges fmt manager args = do
    request <- readJsonFile (daFile args)
    case request >>= mapLeft (T.unpack . T.intercalate "\n") . toExchangeEdits of
        Left err -> reportError err >> exitFailure
        Right edits -> do
            outcome <- editExchanges manager (daDb args) (daProcessId args) edits
            case outcome of
                Left refusal -> do
                    reportError (T.unpack (refusalMessage refusal))
                    exitFailure
                Right report -> do
                    mapM_ (reportProgress Warning . T.unpack) (erWarnings report)
                    outputResult fmt $
                        object
                            [ "database" .= daDb args
                            , "activity" .= daProcessId args
                            , "removed" .= erRemoved report
                            , "amountsSet" .= erAmountsSet report
                            , "added" .= erAdded report
                            , "transient" .= not (erPersisted report)
                            , "warnings" .= erWarnings report
                            ]
  where
    mapLeft f = either (Left . f) Right

{- | Shared tail of both write commands: translate, write, and report. A
refusal is printed and exits non-zero, so a script can tell a rejected batch
from a written one without parsing the output.
-}
runWrite :: OutputFormat -> DatabaseManager -> Text -> WriteVerb -> [ActivityInput] -> IO ()
runWrite fmt manager target verb inputs =
    case toAuthoredActivities inputs of
        Left errs -> reportError (T.unpack (T.intercalate "\n" errs)) >> exitFailure
        Right authored -> do
            outcome <- writeActivities manager target verb authored
            case outcome of
                Left refusal -> do
                    reportError (T.unpack (refusalMessage refusal))
                    exitFailure
                Right report -> do
                    mapM_ (reportProgress Warning . T.unpack) (wrWarnings report)
                    outputResult fmt $
                        object
                            [ "database" .= target
                            , "written" .= wrWritten report
                            , "transient" .= not (wrPersisted report)
                            , "warnings" .= wrWarnings report
                            ]

{- | Execute database load: bring a configured database (and its declared
dependencies) into memory. Any failed dependency is surfaced in the output.
-}
executeDbLoad :: OutputFormat -> DatabaseManager -> Text -> IO ()
executeDbLoad fmt manager name = do
    result <- DM.loadDatabase manager name
    case result of
        Left err -> do
            reportError $ "Load failed: " ++ T.unpack err
            exitFailure
        Right (_loaded, deps) -> do
            reportProgress Info $ "Loaded database: " ++ T.unpack name
            outputResult fmt $ object ["loaded" .= name, "dependencies" .= deps]

{- | Execute database unload: drop a database from memory (refused while another
loaded database still depends on it).
-}
executeDbUnload :: OutputFormat -> DatabaseManager -> Text -> IO ()
executeDbUnload fmt manager name = do
    result <- DM.unloadDatabase manager name
    case result of
        Left err -> do
            reportError $ "Unload failed: " ++ T.unpack err
            exitFailure
        Right () -> do
            reportProgress Info $ "Unloaded database: " ++ T.unpack name
            outputResult fmt $ object ["unloaded" .= name]

-- | Execute database copy command
executeDbCopy :: OutputFormat -> DatabaseManager -> Text -> Text -> IO ()
executeDbCopy fmt manager srcName newName = do
    result <- copyDatabase manager srcName newName
    case result of
        Left err -> do
            reportError $ "Copy failed: " ++ T.unpack err
            exitFailure
        Right () -> do
            reportProgress Info $ "Copied database: " ++ T.unpack srcName ++ " -> " ++ T.unpack newName
            outputResult fmt $ object ["source" .= srcName, "copy" .= newName]

-- | Execute relink-with-mapping: relink a DB to a dependency via an alias CSV.
executeDbRelinkMapping :: OutputFormat -> DatabaseManager -> DbRelinkArgs -> IO ()
executeDbRelinkMapping fmt manager args = do
    result <- relinkWithMappingFile manager (draDb args) (draToDep args) (draMappingCsv args)
    case result of
        Left err -> do
            reportError $ "Relink failed: " ++ T.unpack err
            exitFailure
        Right r -> do
            reportProgress Info $
                "Re-linked "
                    ++ T.unpack (rresDbName r)
                    ++ ": "
                    ++ show (rresUnresolvedBefore r)
                    ++ " -> "
                    ++ show (rresUnresolvedAfter r)
                    ++ " unresolved ("
                    ++ show (rresCrossDBLinks r)
                    ++ " cross-DB links)"
            outputResult fmt $
                object
                    [ "database" .= rresDbName r
                    , "unresolved_before" .= rresUnresolvedBefore r
                    , "unresolved_after" .= rresUnresolvedAfter r
                    , "cross_db_links" .= rresCrossDBLinks r
                    , "depends_on" .= rresDepsLoaded r
                    ]

-- | Execute database export: serialize a loaded database to a file.
executeDbExport :: OutputFormat -> DatabaseManager -> DbExportArgs -> IO ()
executeDbExport fmt manager args =
    case parseExportFormat (deaFormat args) of
        Left err -> reportError (T.unpack err) >> exitFailure
        Right dbFmt -> do
            mLoaded <- DM.getDatabase manager (deaDb args)
            case mLoaded of
                Nothing -> do
                    reportError $ "Database '" ++ T.unpack (deaDb args) ++ "' not loaded"
                    exitFailure
                Just ld -> do
                    result <- exportDatabase dbFmt (ldDatabase ld) (deaOut args)
                    case result of
                        Left err -> reportError (T.unpack err) >> exitFailure
                        Right warnings -> do
                            mapM_ (reportProgress Warning . T.unpack) warnings
                            reportProgress Info $ "Exported " ++ T.unpack (deaDb args) ++ " -> " ++ deaOut args
                            outputResult fmt $
                                object
                                    [ "database" .= deaDb args
                                    , "format" .= deaFormat args
                                    , "out" .= deaOut args
                                    ]

-- | Execute method-collection export: serialize a loaded collection to a file.
executeMcExport :: OutputFormat -> DatabaseManager -> McExportArgs -> IO ()
executeMcExport fmt manager args =
    case parseMethodExportFormat (meaFormat args) of
        Left err -> reportError (T.unpack err) >> exitFailure
        Right mcFmt -> do
            mColl <- DM.getMethodCollection manager (meaName args)
            case mColl of
                Nothing -> do
                    reportError $ "Method collection '" ++ T.unpack (meaName args) ++ "' not loaded"
                    exitFailure
                Just coll -> do
                    result <- exportMethodCollection mcFmt (meaName args) coll (meaOut args)
                    case result of
                        Left err -> reportError (T.unpack err) >> exitFailure
                        Right warnings -> do
                            mapM_ (reportProgress Warning . T.unpack) warnings
                            reportProgress Info $ "Exported " ++ T.unpack (meaName args) ++ " -> " ++ meaOut args
                            outputResult fmt $
                                object
                                    [ "collection" .= meaName args
                                    , "format" .= meaFormat args
                                    , "out" .= meaOut args
                                    ]

-- | Execute method delete command
executeMcDelete :: OutputFormat -> DatabaseManager -> Text -> IO ()
executeMcDelete fmt manager name = do
    result <- DM.removeMethodCollection manager name
    case result of
        Left err -> do
            reportError $ "Delete failed: " ++ T.unpack err
            exitFailure
        Right () -> do
            reportProgress Info $ "Deleted method: " ++ T.unpack name
            outputResult fmt $ object ["deleted" .= name]

-- | Execute mapping command: analyze flow mapping coverage
executeFlowMappingCommand :: OutputFormat -> Types.Database -> DatabaseManager -> MappingOptions -> IO ()
executeFlowMappingCommand fmt database manager opts = do
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
                    cmap <- DM.getMergedCompartmentMap manager
                    mappings <- mapMethodToFlows cmap database method
                    let stats = computeMappingStats mappings
                        totalMatched = msTotal stats - msUnmatched stats
                        coverage =
                            if msTotal stats > 0
                                then fromIntegral totalMatched / fromIntegral (msTotal stats) * 100 :: Double
                                else 0.0
                        dbBioCount = fromIntegral (Types.dbBiosphereCount database) :: Int
                        characterizedUUIDs =
                            S.fromList
                                [Types.bfId f | (_cf, Just (f, _)) <- mappings]
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
                            when (msByProxy stats > 0) $
                                putStrLn $
                                    "  by Proxy:   " ++ show (msByProxy stats)
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
                                                ++ T.unpack (strategyToText strat)
                                                ++ "] "
                                                ++ T.unpack (mcfFlowName cf)
                                                ++ " → "
                                                ++ T.unpack (Types.bfName f)
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
                            outputResult JSON $
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
                                        , "byProxy" .= msByProxy stats
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
                                                            , "dbFlowName" .= Types.bfName f
                                                            , "strategy" .= strategyToText strat
                                                            , "cfUUID" .= mcfFlowRef cf
                                                            , "dbFlowUUID" .= Types.bfId f
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

-- | Get names of biosphere flows not matched by any CF
uncharacterizedFlowNames :: Types.Database -> S.Set UUID.UUID -> [Text]
uncharacterizedFlowNames db characterized =
    [ Types.bfName f
    | uuid <- V.toList (Types.dbBiosphereOrder db)
    , not (S.member uuid characterized)
    , Just f <- [M.lookup uuid (Types.dbBioFlows db)]
    ]

-- | Report service errors to stderr and exit
reportServiceError :: Service.ServiceError -> IO ()
reportServiceError err = do
    reportError $ "Error: " ++ show err
    exitFailure

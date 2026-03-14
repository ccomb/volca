{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Command where

import CLI.Types (Command(..), DatabaseAction(..), MethodAction(..), UploadArgs(..), FlowSubCommand(..), GlobalOptions(..), CLIConfig(..), OutputFormat(..), TreeOptions(..), SearchActivitiesOptions(..), SearchFlowsOptions(..), LCIAOptions(..), DebugMatricesOptions(..), MappingOptions(..))
import qualified Database.Manager as DM
import Database.Manager (DatabaseManager(..), LoadedDatabase(..), addDatabase, addMethodCollection)
import Progress
import qualified Service
import qualified Types
import Types (Database)
import UnitConversion (defaultUnitConfig)
import SharedSolver (SharedSolver)
import Data.Aeson (Value, encode, toJSON, (.=), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Config (DatabaseConfig(..), MethodConfig(..))
import qualified Database.Upload
import Database.Upload (UploadData(..), UploadResult(..), handleUpload, findMethodDirectory)
import qualified Database.UploadedDatabase as UploadedDB
import Control.Concurrent.STM (readTVarIO)
import Data.List (isPrefixOf)
import qualified Data.Set as S
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.Exit (exitFailure)
import Method.Mapping (mapMethodToFlows, computeMappingStats, MappingStats(..), MatchStrategy(..))
import Method.Types (MethodCF(..))
import qualified Method.Types

-- | Default output format for different command types
defaultFormat :: Command -> OutputFormat
defaultFormat (Server _) = JSON       -- Server always returns JSON
defaultFormat _ = Pretty               -- All other commands default to Pretty

-- | Format output with optional JSONPath for CSV extraction
formatOutputWithPath :: OutputFormat -> Maybe Text -> Value -> BSL.ByteString
formatOutputWithPath JSON _ value = encode value
formatOutputWithPath CSV _ value = encode value  -- Simple JSON for CSV (CLI users can pipe to jq)
formatOutputWithPath Table _ value = encodePretty value
formatOutputWithPath Pretty _ value = encodePretty value

-- | Resolve output format using command-specific defaults
resolveOutputFormat :: GlobalOptions -> Command -> OutputFormat
resolveOutputFormat globalOpts cmd = case format globalOpts of
  Just fmt -> fmt                    -- Explicit --format overrides everything
  Nothing -> defaultFormat cmd       -- Use command-specific default

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
        []   -> do
          reportError "No databases loaded"
          exitFailure
        _    -> do
          let available = map T.unpack (M.keys loadedDbs)
          reportError $ "Multiple databases loaded, use --db to select one: " ++ unwords available
          exitFailure

-- | Execute a CLI command with global options
executeCommand :: CLIConfig -> Command -> DatabaseManager -> IO ()
executeCommand (CLIConfig globalOpts _) cmd manager = do
  let outputFormat = resolveOutputFormat globalOpts cmd
      jsonPathOpt = jsonPath globalOpts

  case cmd of
    -- Manager-level commands (no database required)
    Server _ -> do
      reportError "Server mode should be handled in Main.hs"
      exitFailure

    Database DbList ->
      DM.listDatabases manager >>= outputResult outputFormat jsonPathOpt . toJSON

    Database (DbUpload args) ->
      executeDbUpload outputFormat jsonPathOpt manager args

    Database (DbDelete name) ->
      executeDbDelete outputFormat jsonPathOpt manager name

    Method McList ->
      DM.listMethodCollections manager >>= outputResult outputFormat jsonPathOpt . toJSON

    Method (McUpload args) ->
      executeMcUpload outputFormat jsonPathOpt manager args

    Method (McDelete name) ->
      executeMcDelete outputFormat jsonPathOpt manager name

    Methods -> do
      pairs <- DM.getLoadedMethods manager
      let val = toJSON [ object ["collection" .= col, "method" .= m] | (col, m) <- pairs ]
      outputResult outputFormat jsonPathOpt val

    Synonyms ->
      DM.listFlowSynonyms manager >>= outputResult outputFormat jsonPathOpt . toJSON

    CompartmentMappings ->
      DM.listCompartmentMappings manager >>= outputResult outputFormat jsonPathOpt . toJSON

    Units ->
      DM.listUnitDefs manager >>= outputResult outputFormat jsonPathOpt . toJSON

    Mapping opts -> do
      (database, _solver) <- requireDatabase manager (dbName globalOpts)
      executeMappingCommand outputFormat jsonPathOpt database manager opts

    -- Database-level commands
    _ -> do
      (database, _solver) <- requireDatabase manager (dbName globalOpts)
      executeDbCommand outputFormat jsonPathOpt globalOpts database cmd

-- | Execute commands that require a loaded database
executeDbCommand :: OutputFormat -> Maybe Text -> GlobalOptions -> Database -> Command -> IO ()
executeDbCommand fmt jp globalOpts database = \case
    Activity uuid ->
      executeActivityCommand fmt jp database uuid

    Tree uuid treeOpts -> do
      let depth = maybe (treeDepth globalOpts) id (treeDepthOverride treeOpts)
      executeActivityTreeCommand fmt jp database uuid depth

    Inventory uuid ->
      executeActivityInventoryCommand fmt jp database uuid

    Flow flowId Nothing ->
      executeFlowCommand fmt jp database flowId

    Flow flowId (Just FlowActivities) ->
      executeFlowActivitiesCommand fmt jp database flowId

    SearchActivities opts ->
      executeSearchActivitiesCommand fmt jp database opts

    SearchFlows opts ->
      executeSearchFlowsCommand fmt jp database opts

    LCIA uuid lciaOpts ->
      executeLCIACommand fmt jp database uuid lciaOpts

    DebugMatrices uuid debugOpts ->
      executeDebugMatricesCommand database uuid debugOpts

    ExportMatrices outputDir ->
      executeExportMatricesCommand database outputDir

    -- Manager-level commands already handled above
    _ -> pure ()

-- | Execute activity info command
executeActivityCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityCommand fmt jsonPathOpt database uuid =
  case Service.getActivityInfo defaultUnitConfig database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity tree command
executeActivityTreeCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> Int -> IO ()
executeActivityTreeCommand fmt jsonPathOpt database uuid depth =
  case Service.getActivityTree database uuid depth of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity inventory command
executeActivityInventoryCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityInventoryCommand fmt jsonPathOpt database uuid = do
  reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
  result <- Service.getActivityInventory database uuid
  case result of
    Left err -> reportServiceError err
    Right value -> do
      reportProgress Info "Inventory computation completed"
      outputResult fmt jsonPathOpt value

-- | Execute flow info command
executeFlowCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeFlowCommand fmt jsonPathOpt database flowId =
  case Service.getFlowInfo database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute flow activities command
executeFlowActivitiesCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeFlowActivitiesCommand fmt jsonPathOpt database flowId =
  case Service.getFlowActivities database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute search activities command
executeSearchActivitiesCommand :: OutputFormat -> Maybe Text -> Database -> SearchActivitiesOptions -> IO ()
executeSearchActivitiesCommand fmt jsonPathOpt database opts = do
  searchResult <- Service.searchActivities database
         (searchName opts) (searchGeo opts) (searchProduct opts)
         (searchLimit opts) (searchOffset opts)
  case searchResult of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute search flows command
executeSearchFlowsCommand :: OutputFormat -> Maybe Text -> Database -> SearchFlowsOptions -> IO ()
executeSearchFlowsCommand fmt jsonPathOpt database opts = do
  searchResult <- Service.searchFlows database
         (searchQuery opts) (searchLang opts)
         (searchFlowsLimit opts) (searchFlowsOffset opts)
  case searchResult of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute LCIA command with method file
executeLCIACommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> LCIAOptions -> IO ()
executeLCIACommand fmt jsonPathOpt database uuid opts = do
  reportProgress Info $ "Computing LCIA for activity: " ++ T.unpack uuid
  reportProgress Info $ "Using method file: " ++ CLI.Types.lciaMethod opts

  case Service.computeLCIA database uuid (CLI.Types.lciaMethod opts) of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "LCIA computation completed"
      outputResult fmt jsonPathOpt result

      case lciaOutput opts of
        Just outputPath ->
          case Service.exportLCIAAsXML result outputPath of
            Left err -> reportError $ "XML export failed: " ++ show err
            Right _ -> reportProgress Info $ "Results exported to XML: " ++ outputPath
        Nothing -> return ()

      case lciaCSV opts of
        Just csvPath ->
          case Service.exportLCIAAsCSV result csvPath of
            Left err -> reportError $ "CSV export failed: " ++ show err
            Right _ -> reportProgress Info $ "Results exported to CSV: " ++ csvPath
        Nothing -> return ()

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
  reportProgress Info $ "  - ie_index.csv (activity index)"
  reportProgress Info $ "  - ee_index.csv (biosphere flow index)"
  reportProgress Info $ "  - A_public.csv (technosphere matrix)"
  reportProgress Info $ "  - B_public.csv (biosphere matrix)"

-- | Output result in the specified format
outputResult :: OutputFormat -> Maybe Text -> Value -> IO ()
outputResult fmt jsonPathOpt result =
  BSL.putStrLn $ formatOutputWithPath fmt jsonPathOpt result

-- | Execute database upload command
executeDbUpload :: OutputFormat -> Maybe Text -> DatabaseManager -> UploadArgs -> IO ()
executeDbUpload fmt jp manager args = do
  reportProgress Info $ "Reading file: " ++ uaFile args
  fileData <- BL.readFile (uaFile args)

  let uploadData = UploadData
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

      let meta = UploadedDB.UploadMeta
            { UploadedDB.umVersion = 1
            , UploadedDB.umDisplayName = uaName args
            , UploadedDB.umDescription = uaDescription args
            , UploadedDB.umFormat = urFormat uploadResult
            , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
            }
      UploadedDB.writeUploadMeta uploadDir meta

      let dbConfig = DatabaseConfig
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

      outputResult fmt jp $ object
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
executeMcUpload :: OutputFormat -> Maybe Text -> DatabaseManager -> UploadArgs -> IO ()
executeMcUpload fmt jp manager args = do
  reportProgress Info $ "Reading file: " ++ uaFile args
  fileData <- BL.readFile (uaFile args)

  let uploadData = UploadData
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

      let meta = UploadedDB.UploadMeta
            { UploadedDB.umVersion = 1
            , UploadedDB.umDisplayName = uaName args
            , UploadedDB.umDescription = uaDescription args
            , UploadedDB.umFormat = urFormat uploadResult
            , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
            }
      UploadedDB.writeUploadMeta uploadDir meta

      methodDir <- findMethodDirectory uploadDir
      let mc = MethodConfig
            { mcName = uaName args
            , mcPath = methodDir
            , mcActive = False
            , mcIsUploaded = True
            , mcDescription = uaDescription args
            }
      addMethodCollection manager mc
      reportProgress Info $ "Method uploaded: " ++ T.unpack slug

      outputResult fmt jp $ object
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
executeDbDelete :: OutputFormat -> Maybe Text -> DatabaseManager -> Text -> IO ()
executeDbDelete fmt jp manager name = do
  result <- DM.removeDatabase manager name
  case result of
    Left err -> do
      reportError $ "Delete failed: " ++ T.unpack err
      exitFailure
    Right () -> do
      reportProgress Info $ "Deleted database: " ++ T.unpack name
      outputResult fmt jp $ object ["deleted" .= name]

-- | Execute method delete command
executeMcDelete :: OutputFormat -> Maybe Text -> DatabaseManager -> Text -> IO ()
executeMcDelete fmt jp manager name = do
  result <- DM.removeMethodCollection manager name
  case result of
    Left err -> do
      reportError $ "Delete failed: " ++ T.unpack err
      exitFailure
    Right () -> do
      reportProgress Info $ "Deleted method: " ++ T.unpack name
      outputResult fmt jp $ object ["deleted" .= name]

-- | Execute mapping command: analyze flow mapping coverage
executeMappingCommand :: OutputFormat -> Maybe Text -> Types.Database -> DatabaseManager -> MappingOptions -> IO ()
executeMappingCommand fmt jp database manager opts = do
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
        (method:_) -> do
          let mappings = mapMethodToFlows database method
              stats = computeMappingStats mappings
              totalMatched = msTotal stats - msUnmatched stats
              coverage = if msTotal stats > 0
                         then fromIntegral totalMatched / fromIntegral (msTotal stats) * 100 :: Double
                         else 0.0
              dbBioCount = fromIntegral (Types.dbBiosphereCount database) :: Int
              -- Compute characterized DB flows (unique DB flows that got a CF)
              characterizedUUIDs = S.fromList
                  [Types.flowId f | (_cf, Just (f, _)) <- mappings]
              characterizedCount = S.size characterizedUUIDs
              uncharacterizedCount = dbBioCount - characterizedCount
              charCoverage = if dbBioCount > 0
                             then fromIntegral characterizedCount / fromIntegral dbBioCount * 100 :: Double
                             else 0.0

          case fmt of
            JSON -> outputResult fmt jp $ toJSON $ object
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
              , "matchedCFs" .= if mappingShowMatched opts
                  then toJSON [object [ "cfName" .= mcfFlowName cf
                                      , "dbFlowName" .= Types.flowName f
                                      , "strategy" .= strategyText strat
                                      , "cfUUID" .= mcfFlowRef cf
                                      , "dbFlowUUID" .= Types.flowId f
                                      ]
                              | (cf, Just (f, strat)) <- mappings]
                  else toJSON (Nothing :: Maybe Value)
              , "unmatchedCFs" .= if mappingShowUnmatched opts
                  then toJSON [object ["name" .= mcfFlowName cf, "uuid" .= mcfFlowRef cf, "cas" .= mcfCAS cf]
                              | (cf, Nothing) <- mappings]
                  else toJSON (Nothing :: Maybe Value)
              , "uncharacterizedFlows" .= if mappingShowUncharacterized opts
                  then toJSON (uncharacterizedFlowNames database characterizedUUIDs)
                  else toJSON (Nothing :: Maybe Value)
              ]
            _ -> do
              -- Pretty/Table output
              putStrLn $ "Method: " ++ T.unpack (Method.Types.methodName method)
              putStrLn $ "Total CFs: " ++ show (msTotal stats)
              putStrLn $ "Matched:   " ++ show totalMatched ++ " (" ++ showPercent coverage ++ ")"
              putStrLn $ "  by UUID:    " ++ show (msByUUID stats)
              putStrLn $ "  by CAS:     " ++ show (msByCAS stats)
              putStrLn $ "  by Name:    " ++ show (msByName stats)
              putStrLn $ "  by Synonym: " ++ show (msBySynonym stats)
              when (msByFuzzy stats > 0) $
                putStrLn $ "  by Fuzzy:   " ++ show (msByFuzzy stats)
              putStrLn $ "Unmatched:  " ++ show (msUnmatched stats)
              putStrLn ""
              putStrLn $ "DB biosphere flows: " ++ show dbBioCount
              putStrLn $ "Characterized:      " ++ show characterizedCount ++ " (" ++ showPercent charCoverage ++ ")"
              putStrLn $ "Uncharacterized:    " ++ show uncharacterizedCount

              when (mappingShowMatched opts) $ do
                putStrLn ""
                putStrLn "--- Matched CFs ---"
                mapM_ (\(cf, f, strat) ->
                    putStrLn $ "  [" ++ T.unpack (strategyText strat) ++ "] "
                        ++ T.unpack (mcfFlowName cf)
                        ++ " → " ++ T.unpack (Types.flowName f))
                    [(cf, f, strat) | (cf, Just (f, strat)) <- mappings]

              when (mappingShowUnmatched opts) $ do
                putStrLn ""
                putStrLn "--- Unmatched CFs (no DB flow found) ---"
                mapM_ (\(cf, _) -> putStrLn $ "  " ++ T.unpack (mcfFlowName cf)
                            ++ maybe "" (\c -> " [CAS " ++ T.unpack c ++ "]") (mcfCAS cf))
                    [(cf, m) | (cf, m@Nothing) <- mappings]

              when (mappingShowUncharacterized opts) $ do
                putStrLn ""
                putStrLn "--- Uncharacterized DB flows (no CF matched) ---"
                mapM_ (\name -> putStrLn $ "  " ++ T.unpack name)
                    (uncharacterizedFlowNames database characterizedUUIDs)
  where
    showPercent :: Double -> String
    showPercent p = show (round (p * 10) `div` 10 :: Int) ++ "." ++ show (round (p * 10) `mod` 10 :: Int) ++ "%"

    when True  action = action
    when False _      = pure ()

strategyText :: MatchStrategy -> Text
strategyText ByUUID   = "uuid"
strategyText ByCAS    = "cas"
strategyText ByName   = "name"
strategyText BySynonym = "synonym"
strategyText ByFuzzy  = "fuzzy"
strategyText NoMatch  = "none"

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

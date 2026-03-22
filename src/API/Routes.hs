{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import qualified Matrix
import Matrix (Inventory)
import SharedSolver (SharedSolver)
import Method.Mapping (computeLCIAScore, mapMethodToFlows, MatchStrategy(..), MappingStats(..), computeMappingStats)
import Plugin.Types (PluginRegistry(..), AnalyzeHandle(..), AnalyzeContext(..))
import qualified Data.Vector as V
import Method.Types (Method(..), MethodCF(..), MethodCollection(..), DamageCategory(..), NormWeightSet(..), FlowDirection(..))
import Database.Manager (DatabaseManager(..), LoadedDatabase(..), DatabaseSetupInfo(..), getDatabase, MethodCollectionStatus(..), getMergedUnitConfig)
import qualified Database.Manager as DM
import API.DatabaseHandlers (simpleAction)
import qualified API.DatabaseHandlers as DBHandlers
import Progress (getLogLines, reportProgress, ProgressLevel(Info))
import Database
import qualified Service
import Tree (buildLoopAwareTree)
import Types
import API.Types (ActivityInfo (..), ActivitySummary (..), ExchangeDetail (..), FlowCFEntry (..), FlowCFMapping (..), FlowDetail (..), FlowSearchResult (..), FlowSummary (..), GraphExport (..), InventoryExport (..), LCIARequest (..), LCIAResult (..), LCIABatchResult(..), MappingStatus (..), MethodDetail (..), MethodFactorAPI (..), MethodSummary (..), MethodCollectionListResponse(..), MethodCollectionStatusAPI(..), RefDataListResponse(..), SynonymGroupsResponse(..), SearchResults (..), SupplyChainResponse(..), VariantRequest(..), VariantResponse(..), TreeExport (..), UnmappedFlowAPI (..), DatabaseListResponse(..), ActivateResponse(..), LoadDatabaseResponse(..), UploadRequest(..), UploadResponse(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import GHC.Generics
import Servant
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Numeric (showFFloat)
import qualified Version

-- | API type definition - RESTful design with focused endpoints
type LCAAPI =
    "api"
        :> "v1"
        :> ( "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> Get '[JSON] ActivityInfo
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "flows" :> Get '[JSON] [FlowSummary]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "reference-product" :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "tree" :> Get '[JSON] TreeExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> Get '[JSON] InventoryExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "graph" :> QueryParam "cutoff" Double :> Get '[JSON] GraphExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> Get '[JSON] SupplyChainResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "variant" :> ReqBody '[JSON] VariantRequest :> Post '[JSON] VariantResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "lcia" :> Capture "methodId" Text :> Get '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "lcia-batch" :> Capture "collection" Text :> Get '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "analyze" :> Capture "analyzerName" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> "activities" :> Get '[JSON] [ActivitySummary]
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> Get '[JSON] [MethodFactorAPI]
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "mapping" :> Get '[JSON] MappingStatus
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "flow-mapping" :> Get '[JSON] FlowCFMapping
                :<|> "db" :> Capture "dbName" Text :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "db" :> Capture "dbName" Text :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "db" :> Capture "dbName" Text :> "lcia" :> Capture "processId" Text :> ReqBody '[JSON] LCIARequest :> Post '[JSON] Value
                -- Database management endpoints
                :<|> "db" :> Get '[JSON] DatabaseListResponse
                -- Load/Unload/Delete endpoints
                :<|> "db" :> Capture "dbName" Text :> "load" :> Post '[JSON] LoadDatabaseResponse
                :<|> "db" :> Capture "dbName" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "db" :> Capture "dbName" Text :> Delete '[JSON] ActivateResponse
                -- Upload endpoint (base64-encoded ZIP in JSON body)
                :<|> "db" :> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
                -- Database setup endpoints (for cross-DB linking configuration)
                :<|> "db" :> Capture "dbName" Text :> "setup" :> Get '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "add-dependency" :> Capture "depName" Text :> Post '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "remove-dependency" :> Capture "depName" Text :> Post '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "set-data-path" :> ReqBody '[JSON] Value :> Post '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "finalize" :> Post '[JSON] ActivateResponse
                -- Method collection endpoints
                :<|> "method-collections" :> Get '[JSON] MethodCollectionListResponse
                :<|> "method-collections" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "method-collections" :> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
                -- Reference data endpoints (flow synonyms, compartment mappings, units)
                :<|> "flow-synonyms" :> Get '[JSON] RefDataListResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "groups" :> Get '[JSON] SynonymGroupsResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BSL.ByteString)
                :<|> "compartment-mappings" :> Get '[JSON] RefDataListResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
                :<|> "units" :> Get '[JSON] RefDataListResponse
                :<|> "units" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "units" :> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
                -- Log endpoint
                :<|> "logs" :> QueryParam "since" Int :> Get '[JSON] Value
                -- Auth endpoint (login)
                :<|> "auth" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" String] Value)
                -- Version endpoint
                :<|> "version" :> Get '[JSON] Value
           )

-- | Get database by name, throw 404 if not loaded
requireDatabaseByName :: DatabaseManager -> Text -> Handler (Database, SharedSolver)
requireDatabaseByName dbManager dbName = do
    maybeLoaded <- liftIO $ getDatabase dbManager dbName
    case maybeLoaded of
        Just loaded -> return (ldDatabase loaded, ldSharedSolver loaded)
        Nothing -> throwError err404{errBody = "Database not loaded: " <> BSL.fromStrict (T.encodeUtf8 dbName)}


-- | Helper function to validate ProcessId and lookup activity
withValidatedActivity :: Database -> Text -> (Activity -> Handler a) -> Handler a
withValidatedActivity db processId action = do
    case Service.resolveActivityByProcessId db processId of
        Left (Service.InvalidProcessId errorMsg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 errorMsg}
        Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
        Left _ -> throwError err400{errBody = "Invalid request"}
        Right activity -> action activity

-- | Helper function to validate UUID and lookup flow
withValidatedFlow :: Database -> Text -> (Flow -> Handler a) -> Handler a
withValidatedFlow db uuid action = do
    case Service.validateUUID uuid of
        Left (Service.InvalidUUID errorMsg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 errorMsg}
        Left _ -> throwError err400{errBody = "Invalid request"}
        Right validUuidText ->
            case UUID.fromText validUuidText of
                Nothing -> throwError err400{errBody = "Invalid UUID format"}
                Just validUuid ->
                    case M.lookup validUuid (dbFlows db) of
                        Nothing -> throwError err404{errBody = "Flow not found"}
                        Just flow -> action flow

-- | Login request body
data LoginRequest = LoginRequest
    { lrCode :: Text
    } deriving (Generic)

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \v ->
        LoginRequest <$> v .: "code"

-- | API server implementation
-- DatabaseManager is used to dynamically fetch current database on each request
lcaServer :: DatabaseManager -> Int -> Maybe String -> Server LCAAPI
lcaServer dbManager maxTreeDepth password =
    getActivityInfo
        :<|> getActivityFlows
        :<|> getActivityInputs
        :<|> getActivityOutputs
        :<|> getActivityReferenceProduct
        :<|> getActivityTree
        :<|> getActivityInventory
        :<|> getActivityGraph
        :<|> getActivitySupplyChain
        :<|> postActivityVariant
        :<|> getActivityLCIA
        :<|> getActivityLCIABatch
        :<|> getActivityAnalyze
        :<|> getFlowDetail
        :<|> getFlowActivities
        :<|> getMethods
        :<|> getMethodDetail
        :<|> getMethodFactors
        :<|> getMethodMapping
        :<|> getFlowCFMapping
        :<|> searchFlows
        :<|> searchActivitiesWithCount
        :<|> postLCIA
        :<|> DBHandlers.getDatabases dbManager
        :<|> DBHandlers.loadDatabaseHandler dbManager
        :<|> DBHandlers.unloadDatabaseHandler dbManager
        :<|> DBHandlers.deleteDatabaseHandler dbManager
        :<|> DBHandlers.uploadDatabaseHandler dbManager
        :<|> DBHandlers.getDatabaseSetupHandler dbManager
        :<|> DBHandlers.addDependencyHandler dbManager
        :<|> DBHandlers.removeDependencyHandler dbManager
        :<|> DBHandlers.setDataPathHandler dbManager
        :<|> DBHandlers.finalizeDatabaseHandler dbManager
        :<|> getMethodCollections
        :<|> loadMethodCollectionHandler
        :<|> unloadMethodCollectionHandler
        :<|> DBHandlers.deleteMethodHandler dbManager
        :<|> DBHandlers.uploadMethodHandler dbManager
        -- Flow synonyms
        :<|> DBHandlers.listRefData DBHandlers.FlowSynonyms dbManager
        :<|> DBHandlers.loadRefData DBHandlers.FlowSynonyms dbManager
        :<|> DBHandlers.unloadRefData DBHandlers.FlowSynonyms dbManager
        :<|> DBHandlers.deleteRefData DBHandlers.FlowSynonyms dbManager
        :<|> DBHandlers.uploadRefData DBHandlers.FlowSynonyms dbManager
        :<|> DBHandlers.getFlowSynonymGroupsHandler dbManager
        :<|> DBHandlers.downloadRefDataHandler DBHandlers.FlowSynonyms dbManager
        -- Compartment mappings
        :<|> DBHandlers.listRefData DBHandlers.CompartmentMappings dbManager
        :<|> DBHandlers.loadRefData DBHandlers.CompartmentMappings dbManager
        :<|> DBHandlers.unloadRefData DBHandlers.CompartmentMappings dbManager
        :<|> DBHandlers.deleteRefData DBHandlers.CompartmentMappings dbManager
        :<|> DBHandlers.uploadRefData DBHandlers.CompartmentMappings dbManager
        -- Units
        :<|> DBHandlers.listRefData DBHandlers.UnitDefs dbManager
        :<|> DBHandlers.loadRefData DBHandlers.UnitDefs dbManager
        :<|> DBHandlers.unloadRefData DBHandlers.UnitDefs dbManager
        :<|> DBHandlers.deleteRefData DBHandlers.UnitDefs dbManager
        :<|> DBHandlers.uploadRefData DBHandlers.UnitDefs dbManager
        :<|> getLogsHandler
        :<|> postAuth
        :<|> getVersion
  where
    getVersion :: Handler Value
    getVersion = return $ object
        [ "version" .= Version.version
        , "gitHash" .= Version.gitHash
        , "gitTag" .= Version.gitTag
        , "buildTarget" .= Version.buildTarget
        ]

    getLogsHandler :: Maybe Int -> Handler Value
    getLogsHandler sinceMaybe = do
        let since = fromMaybe 0 sinceMaybe
        (nextIndex, logLines) <- liftIO $ getLogLines since
        return $ object
            [ "lines" .= logLines
            , "nextIndex" .= nextIndex
            ]

    postAuth :: LoginRequest -> Handler (Headers '[Header "Set-Cookie" String] Value)
    postAuth loginReq =
        case password of
            Nothing ->
                -- No password configured, auth always succeeds
                return $ noHeader $ object ["ok" .= True]
            Just pwd ->
                if T.unpack (lrCode loginReq) == pwd
                then
                    let cookieValue = "volca_session=" ++ pwd ++ "; Path=/; HttpOnly; SameSite=Strict"
                    in return $ addHeader cookieValue $ object ["ok" .= True]
                else
                    throwError err401{errBody = "{\"error\":\"invalid code\"}"}

    -- Core activity endpoint - streamlined data
    getActivityInfo :: Text -> Text -> Handler ActivityInfo
    getActivityInfo dbName processId = do
        (db, _) <- requireDatabaseByName dbManager dbName
        unitCfg <- liftIO $ getMergedUnitConfig dbManager
        case Service.getActivityInfo unitCfg db processId of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right result -> case fromJSON result of
                Success activityInfo -> return activityInfo
                Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

    -- Activity flows sub-resource
    getActivityFlows :: Text -> Text -> Handler [FlowSummary]
    getActivityFlows dbName processId = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedActivity db processId $ \activity ->
            return $ Service.getActivityFlowSummaries db activity

    -- Activity inputs sub-resource
    getActivityInputs :: Text -> Text -> Handler [ExchangeDetail]
    getActivityInputs dbName processId = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedActivity db processId $ \activity ->
            return $ Service.getActivityInputDetails db activity

    -- Activity outputs sub-resource
    getActivityOutputs :: Text -> Text -> Handler [ExchangeDetail]
    getActivityOutputs dbName processId = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedActivity db processId $ \activity ->
            return $ Service.getActivityOutputDetails db activity

    -- Activity reference product sub-resource
    getActivityReferenceProduct :: Text -> Text -> Handler FlowDetail
    getActivityReferenceProduct dbName processId = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedActivity db processId $ \activity -> do
            case Service.getActivityReferenceProductDetail db activity of
                Nothing -> throwError err404{errBody = "No reference product found"}
                Just refProduct -> return refProduct

    -- Activity tree export for visualization (configurable depth)
    getActivityTree :: Text -> Text -> Handler TreeExport
    getActivityTree dbName processId = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedActivity db processId $ \_activity -> do
            -- Use CLI --tree-depth option for configurable depth
            -- Default depth limit prevents DOS attacks via deep tree requests
            -- Extract activity UUID from processId (format: activityUUID_productUUID)
            let activityUuidText = case T.splitOn "_" processId of
                    (uuid:_) -> uuid
                    [] -> processId  -- Fallback
            case UUID.fromText activityUuidText of
                Nothing -> throwError err400{errBody = "Invalid activity UUID format"}
                Just activityUuid -> do
                    unitCfg <- liftIO $ getMergedUnitConfig dbManager
                    let loopAwareTree = buildLoopAwareTree unitCfg db activityUuid maxTreeDepth
                    return $ Service.convertToTreeExport db processId maxTreeDepth loopAwareTree

    -- Activity inventory calculation (full supply chain LCI)
    getActivityInventory :: Text -> Text -> Handler InventoryExport
    getActivityInventory dbName processId = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        let validators = prValidators (dmPlugins dbManager)
        result <- liftIO $ Service.getActivityInventoryWithSharedSolver validators sharedSolver db processId
        case result of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right inventoryExport -> return inventoryExport

    -- Activity graph endpoint for network visualization
    getActivityGraph :: Text -> Text -> Maybe Double -> Handler GraphExport
    getActivityGraph dbName processId maybeCutoff = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        let cutoffPercent = fromMaybe 1.0 maybeCutoff  -- Default to 1% cutoff
        result <- liftIO $ Service.buildActivityGraph db sharedSolver processId cutoffPercent
        case result of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left (Service.MatrixError msg) -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right graphExport -> return graphExport

    -- Activity supply chain endpoint (scaling vector based)
    getActivitySupplyChain :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe Double -> Handler SupplyChainResponse
    getActivitySupplyChain dbName processId nameFilter limitParam minQuantity = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        result <- liftIO $ Service.getSupplyChain db sharedSolver processId nameFilter limitParam minQuantity
        case result of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left (Service.MatrixError msg) -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right supplyChain -> return supplyChain

    -- Activity variant endpoint (Sherman-Morrison substitution)
    postActivityVariant :: Text -> Text -> VariantRequest -> Handler VariantResponse
    postActivityVariant dbName processId variantReq = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        result <- liftIO $ Service.createVariant db sharedSolver processId variantReq
        case result of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId msg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left (Service.MatrixError msg) -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right variantResp -> return variantResp

    -- Activity LCIA endpoint (single method)
    getActivityLCIA :: Text -> Text -> Text -> Handler LCIAResult
    getActivityLCIA dbName processIdText methodIdText = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        case Service.resolveActivityAndProcessId db processIdText of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (actProcessId, _activity) -> do
                inventory <- liftIO $ Matrix.computeInventoryMatrix db actProcessId
                result <- liftIO $ computeCategoryResult db inventory method
                liftIO $ logLCIAResult result method
                return result

    -- Batch LCIA endpoint (all methods in a collection)
    getActivityLCIABatch :: Text -> Text -> Text -> Handler LCIABatchResult
    getActivityLCIABatch dbName processIdText collectionName = do
        (db, _) <- requireDatabaseByName dbManager dbName
        -- Look up collection
        loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
        collection <- case M.lookup collectionName loadedCollections of
            Just mc -> return mc
            Nothing -> throwError err404{errBody = "Collection not loaded: " <> BSL.fromStrict (T.encodeUtf8 collectionName)}
        let methods = mcMethods collection
            damageCats = mcDamageCategories collection
            nwSets = mcNormWeightSets collection
            -- Build damage category lookup: subcategory name → parent name
            dcLookup = M.fromList
                [ (subName, dcName dc)
                | dc <- damageCats, (subName, _) <- dcImpacts dc ]
            -- Use first NW set if available
            mNW = case nwSets of { (nw:_) -> Just nw; [] -> Nothing }
        case Service.resolveActivityAndProcessId db processIdText of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (actProcessId, activity) -> do
                t0 <- liftIO getCurrentTime
                inventory <- liftIO $ Matrix.computeInventoryMatrix db actProcessId
                t1 <- liftIO getCurrentTime
                let !invSize = M.size inventory
                liftIO $ reportProgress Info $ "[LCIA batch] " <> T.unpack collectionName
                    <> " for " <> T.unpack (activityName activity)
                liftIO $ reportProgress Info $ "  Inventory: "
                    <> show invSize <> " flows ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t1 t0) :: Double) "" <> "s)"
                when (invSize == 0) $
                    liftIO $ reportProgress Info "  WARNING: inventory is empty — check matrix computation"
                when (invSize > 0 && invSize <= 5) $
                    liftIO $ reportProgress Info $ "  Inventory UUIDs: "
                        <> intercalate ", " (map UUID.toString $ M.keys inventory)
                rawResults <- liftIO $ mapM (computeCategoryResult db inventory) methods
                -- Enrich with NW data
                let results = map (enrichWithNW dcLookup mNW) rawResults
                    singleScore = case mNW of
                        Just _ -> Just $ sum [s | r <- results, Just s <- [lrWeightedScore r]]
                        Nothing -> Nothing
                t2 <- liftIO getCurrentTime
                liftIO $ mapM_ (logBatchCategory invSize) results
                liftIO $ reportProgress Info $ "  Total: "
                    <> show (length results) <> " categories ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t2 t0) :: Double) "" <> "s)"
                case singleScore of
                    Just ss -> liftIO $ reportProgress Info $ "  Single score: "
                        <> showFFloat (Just 6) ss "" <> " Pt"
                    Nothing -> return ()
                return LCIABatchResult
                    { lbrResults = results
                    , lbrSingleScore = singleScore
                    , lbrSingleScoreUnit = if null nwSets then Nothing else Just "Pt"
                    , lbrNormWeightSetName = nwName <$> mNW
                    , lbrAvailableNWsets = map nwName nwSets
                    }

    -- Log a single category result in the batch
    logBatchCategory :: Int -> LCIAResult -> IO ()
    logBatchCategory _invSize result = do
        let mapped = lrMappedFlows result
            total = mapped + lrUnmappedFlows result
            scoreTxt = showFFloat (Just 4) (lrScore result) ""
        reportProgress Info $ "  " <> T.unpack (lrMethodName result) <> ": "
            <> scoreTxt <> " " <> T.unpack (lrUnit result)
            <> " (" <> show mapped <> "/" <> show total <> " CFs mapped)"

    -- Activity analysis endpoint (dispatches to registered analyzers)
    getActivityAnalyze :: Text -> Text -> Text -> Handler Value
    getActivityAnalyze dbName processIdText analyzerName = do
        (db, _) <- requireDatabaseByName dbManager dbName
        case M.lookup analyzerName (prAnalyzers (dmPlugins dbManager)) of
            Nothing -> throwError err404{errBody = "Analyzer not found: " <> BSL.fromStrict (T.encodeUtf8 analyzerName)}
            Just analyzer -> do
                case Service.resolveActivityAndProcessId db processIdText of
                    Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
                    Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
                    Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
                    Right (actProcessId, _) -> do
                        inventory <- liftIO $ Matrix.computeInventoryMatrix db actProcessId
                        loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
                        let methods = map snd loadedMethods
                            ctx = AnalyzeContext
                                { acDatabase   = db
                                , acInventory  = inventory
                                , acMethods    = methods
                                , acParameters = M.empty
                                }
                        liftIO $ ahAnalyze analyzer ctx

    -- Helper: compute LCIA result for a single method against an inventory
    computeCategoryResult :: Database -> Inventory -> Method -> IO LCIAResult
    computeCategoryResult db inventory method = do
        let mappers = prMappers (dmPlugins dbManager)
        mappings <- mapMethodToFlows mappers db method
        let stats = computeMappingStats mappings
            score = computeLCIAScore inventory mappings
            unmappedNames = take 50 [mcfFlowName cf | (cf, Nothing) <- mappings]
        pure LCIAResult
            { lrMethodId = methodId method
            , lrMethodName = methodName method
            , lrCategory = methodCategory method
            , lrDamageCategory = methodCategory method  -- default, enriched later
            , lrScore = score
            , lrUnit = methodUnit method
            , lrNormalizedScore = Nothing  -- enriched later
            , lrWeightedScore = Nothing    -- enriched later
            , lrMappedFlows = msTotal stats - msUnmatched stats
            , lrUnmappedFlows = msUnmatched stats
            , lrUnmappedNames = unmappedNames
            }

    -- Enrich a raw LCIA result with damage category mapping and NW scores
    enrichWithNW :: M.Map Text Text -> Maybe NormWeightSet -> LCIAResult -> LCIAResult
    enrichWithNW dcLookup mNW result =
        let dmgCat = M.findWithDefault (lrCategory result) (lrCategory result) dcLookup
            (normScore, weightScore) = case mNW of
                Just nw ->
                    let mNorm = M.lookup dmgCat (nwNormalization nw)
                        mWeight = M.lookup dmgCat (nwWeighting nw)
                    in case (mNorm, mWeight) of
                        (Just n, Just w) ->
                            let ns = lrScore result * n
                            in (Just ns, Just (ns * w))
                        _ -> (Nothing, Nothing)
                Nothing -> (Nothing, Nothing)
        in result { lrDamageCategory = dmgCat
                  , lrNormalizedScore = normScore
                  , lrWeightedScore = weightScore
                  }

    logLCIAResult :: LCIAResult -> Method -> IO ()
    logLCIAResult result method = do
        let mapped = lrMappedFlows result
            total = mapped + lrUnmappedFlows result
            pct :: Double
            pct = if total > 0 then fromIntegral mapped / fromIntegral total * 100 else 0
        reportProgress Info $ "[LCIA] " <> T.unpack (methodName method)
            <> ": " <> showFFloat (Just 4) (lrScore result) ""
            <> " " <> T.unpack (methodUnit method)
        reportProgress Info $ "  Flow mapping: " <> show mapped <> "/" <> show total
            <> " (" <> showFFloat (Just 1) pct "" <> "%)"

    -- Flow detail endpoint
    getFlowDetail :: Text -> Text -> Handler FlowDetail
    getFlowDetail dbName flowIdText = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedFlow db flowIdText $ \flow -> do
            let usageCount = Service.getFlowUsageCount db (flowId flow)
            let flowUnitName = getUnitNameForFlow (dbUnits db) flow
            return $ FlowDetail flow flowUnitName usageCount

    -- Activities using a specific flow
    getFlowActivities :: Text -> Text -> Handler [ActivitySummary]
    getFlowActivities dbName flowIdText = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedFlow db flowIdText $ \flow ->
            return $ Service.getActivitiesUsingFlow db (flowId flow)

    -- List all available methods (from loaded collections)
    getMethods :: Handler [MethodSummary]
    getMethods = do
        loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
        return [ MethodSummary
                    { msmId = methodId m
                    , msmName = methodName m
                    , msmCategory = methodCategory m
                    , msmUnit = methodUnit m
                    , msmFactorCount = length (methodFactors m)
                    , msmCollection = collName
                    }
               | (collName, m) <- loadedMethods
               ]

    -- Get method details
    getMethodDetail :: Text -> Handler MethodDetail
    getMethodDetail methodIdText = do
        method <- loadMethodByUUID methodIdText
        return $ MethodDetail
            { mdId = methodId method
            , mdName = methodName method
            , mdDescription = methodDescription method
            , mdUnit = methodUnit method
            , mdCategory = methodCategory method
            , mdMethodology = methodMethodology method
            , mdFactorCount = length (methodFactors method)
            }

    -- Get method characterization factors
    getMethodFactors :: Text -> Handler [MethodFactorAPI]
    getMethodFactors methodIdText = do
        method <- loadMethodByUUID methodIdText
        return $ map cfToAPI (methodFactors method)

    -- Get method flow mapping status
    getMethodMapping :: Text -> Text -> Handler MappingStatus
    getMethodMapping dbName methodIdText = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        let mappers = prMappers (dmPlugins dbManager)
        mappings <- liftIO $ mapMethodToFlows mappers db method
        let stats = computeMappingStats mappings
            totalFactors = length mappings
            coverage = if totalFactors > 0
                       then fromIntegral (totalFactors - msUnmatched stats) / fromIntegral totalFactors * 100
                       else 0.0
            -- Get unmapped flows (limit to first 50 for API response)
            unmappedFlows = take 50 [ UnmappedFlowAPI
                { ufaFlowRef = mcfFlowRef cf
                , ufaFlowName = mcfFlowName cf
                , ufaDirection = case mcfDirection cf of
                    Input -> "Input"
                    Output -> "Output"
                }
                | (cf, Nothing) <- mappings
                ]
            uniqueDbFlows = S.size $ S.fromList [flowId f | (_, Just (f, _)) <- mappings]
        return MappingStatus
            { mstMethodId = methodId method
            , mstMethodName = methodName method
            , mstTotalFactors = msTotal stats
            , mstMappedByUUID = msByUUID stats
            , mstMappedByCAS = msByCAS stats
            , mstMappedByName = msByName stats
            , mstMappedBySynonym = msBySynonym stats
            , mstUnmapped = msUnmatched stats
            , mstCoverage = coverage
            , mstDbBiosphereCount = fromIntegral (dbBiosphereCount db)
            , mstUniqueDbFlowsMatched = uniqueDbFlows
            , mstUnmappedFlows = unmappedFlows
            }

    -- DB-flow-centric mapping: all biosphere flows with their CF assignments
    getFlowCFMapping :: Text -> Text -> Handler FlowCFMapping
    getFlowCFMapping dbName methodIdText = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        let mappers = prMappers (dmPlugins dbManager)
        mappings <- liftIO $ mapMethodToFlows mappers db method
        let
            -- Build reverse index: DB flow UUID → (MethodCF, MatchStrategy)
            reverseIndex = M.fromList
                [(flowId f, (cf, strat)) | (cf, Just (f, strat)) <- mappings]
            -- Build entries for all biosphere flows
            entries = map (buildFlowEntry db reverseIndex) (V.toList (dbBiosphereFlows db))
            matchedCount = length [() | e <- entries, fceCfValue e /= Nothing]
        return FlowCFMapping
            { fcmMethodName = methodName method
            , fcmMethodUnit = methodUnit method
            , fcmTotalFlows = fromIntegral (dbBiosphereCount db)
            , fcmMatchedFlows = matchedCount
            , fcmFlows = entries
            }

    buildFlowEntry :: Database -> M.Map UUID (MethodCF, MatchStrategy) -> UUID -> FlowCFEntry
    buildFlowEntry db reverseIndex uuid =
        let mFlow = M.lookup uuid (dbFlows db)
            mMatch = M.lookup uuid reverseIndex
        in FlowCFEntry
            { fceFlowId = uuid
            , fceFlowName = maybe "" flowName mFlow
            , fceFlowCategory = maybe "" flowCategory mFlow
            , fceCfValue = fmap (mcfValue . fst) mMatch
            , fceCfFlowName = fmap (mcfFlowName . fst) mMatch
            , fceMatchStrategy = fmap (strategyToText . snd) mMatch
            }

    strategyToText :: MatchStrategy -> Text
    strategyToText ByUUID = "uuid"
    strategyToText ByCAS = "cas"
    strategyToText ByName = "name"
    strategyToText BySynonym = "synonym"
    strategyToText ByFuzzy = "fuzzy"
    strategyToText NoMatch = "none"

    -- Helper to load a method by UUID from the loaded collections
    loadMethodByUUID :: Text -> Handler Method
    loadMethodByUUID uuidText = do
        loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
        let allMethods = map snd loadedMethods
        case UUID.fromText uuidText of
            Nothing -> throwError err400{errBody = "Invalid method UUID format"}
            Just uuid ->
                case filter (\m -> methodId m == uuid) allMethods of
                    (m:_) -> return m
                    []    -> throwError err404{errBody = "Method not found"}

    -- Method collection handlers
    getMethodCollections :: Handler MethodCollectionListResponse
    getMethodCollections = do
        statuses <- liftIO $ DM.listMethodCollections dbManager
        return $ MethodCollectionListResponse
            [ MethodCollectionStatusAPI
                { mcaName = mcsName s
                , mcaDisplayName = mcsDisplayName s
                , mcaDescription = mcsDescription s
                , mcaStatus = case mcsStatus s of
                    DM.Loaded -> "loaded"
                    _         -> "unloaded"
                , mcaIsUploaded = mcsIsUploaded s
                , mcaPath = mcsPath s
                , mcaMethodCount = mcsMethodCount s
                , mcaFormat = Just "ILCD"
                }
            | s <- statuses
            ]

    loadMethodCollectionHandler :: Text -> Handler ActivateResponse
    loadMethodCollectionHandler name =
        simpleAction (DM.loadMethodCollection dbManager name) ("Loaded method: " <> name)

    unloadMethodCollectionHandler :: Text -> Handler ActivateResponse
    unloadMethodCollectionHandler name =
        simpleAction (DM.unloadMethodCollection dbManager name) ("Unloaded method: " <> name)

    -- Helper to convert MethodCF to API type
    cfToAPI :: MethodCF -> MethodFactorAPI
    cfToAPI cf = MethodFactorAPI
        { mfaFlowRef = mcfFlowRef cf
        , mfaFlowName = mcfFlowName cf
        , mfaDirection = case mcfDirection cf of
            Input -> "Input"
            Output -> "Output"
        , mfaValue = mcfValue cf
        }

    -- Search flows by name or synonym with optional language filtering and pagination
    searchFlows :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
    searchFlows dbName queryParam langParam limitParam offsetParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        searchFlowsInternal db queryParam langParam limitParam offsetParam

    -- Search activities by specific fields with pagination and count
    searchActivitiesWithCount :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults ActivitySummary)
    searchActivitiesWithCount dbName nameParam geoParam productParam limitParam offsetParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        -- Use Service.searchActivities which paginates BEFORE calling findProcessIdForActivity
        -- This avoids O(n*m) performance issue where n=results, m=total activities
        result <- liftIO $ Service.searchActivities db nameParam geoParam productParam limitParam offsetParam
        case result of
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right jsonValue -> case fromJSON jsonValue of
                Success searchResults -> return searchResults
                Error parseErr -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack parseErr}

    -- LCIA computation
    postLCIA :: Text -> Text -> LCIARequest -> Handler Value
    postLCIA dbName processId lciaReq = do
        (db, _) <- requireDatabaseByName dbManager dbName
        withValidatedActivity db processId $ \_ -> do
            -- This would implement LCIA computation with the provided method
            -- For now, return a placeholder
            return $ object ["status" .= ("not_implemented" :: Text), "processId" .= processId, "method" .= lciaMethod lciaReq]

-- | Helper function to apply pagination to search results
paginateResults :: [a] -> Maybe Int -> Maybe Int -> IO (SearchResults a)
paginateResults results limitParam offsetParam = do
    startTime <- getCurrentTime
    let totalCount = length results
        limit = maybe totalCount id limitParam -- Default: return all results
        offset = maybe 0 id offsetParam -- Default offset: 0
        paginatedResults = take limit $ drop offset results
        hasMore = offset + length paginatedResults < totalCount
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ SearchResults paginatedResults totalCount offset limit hasMore searchTimeMs

-- | Internal helper for flow search with optional language filtering
searchFlowsInternal :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
searchFlowsInternal _ Nothing _ _ _ = return $ SearchResults [] 0 0 50 False 0.0
searchFlowsInternal db (Just query) _langParam limitParam offsetParam = do
    -- Language filtering not yet implemented, search all synonyms
    let flows = findFlowsBySynonym db query
        flowSearchResults = [FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow)) | flow <- flows]
    liftIO $ paginateResults flowSearchResults limitParam offsetParam

-- | Proxy for the API
lcaAPI :: Proxy LCAAPI
lcaAPI = Proxy

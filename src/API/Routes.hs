{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import qualified Matrix
import Matrix (Inventory, computeProcessLCIAContributions)
import SharedSolver (SharedSolver)
import Method.Mapping (computeLCIAScore, mapMethodToFlows, MatchStrategy(..), MappingStats(..), computeMappingStats)
import Plugin.Types (PluginRegistry(..), AnalyzeHandle(..), AnalyzeContext(..))
import Plugin.Builtin (flowContribution)
import qualified Data.Vector as V
import Method.Types (Method(..), MethodCF(..), MethodCollection(..), DamageCategory(..), NormWeightSet(..), FlowDirection(..), ScoringSet(..), computeFormulaScores)
import Database.Manager (DatabaseManager(..), LoadedDatabase(..), DatabaseSetupInfo(..), getDatabase, MethodCollectionStatus(..), getMergedUnitConfig)
import qualified Database.Manager as DM
import API.DatabaseHandlers (simpleAction)
import qualified API.DatabaseHandlers as DBHandlers
import qualified API.OpenApi
import Data.OpenApi (OpenApi, ToSchema)
import Servant.OpenApi (toOpenApi)
import Progress (getLogLines, reportProgress, ProgressLevel(Info, Warning))
import Database
import qualified Service
import Tree (buildLoopAwareTree)
import Types
import API.Types (ActivityInfo (..), ActivitySummary (..), Aggregation(..), BinaryContent(..), ClassificationSystem(..), ConsumerResult (..), ExchangeDetail (..), ClassificationEntryInfo(..), ClassificationPresetInfo(..), FlowCFEntry (..), FlowCFMapping (..), FlowContributionEntry(..), FlowDetail (..), ContributingFlowsResult(..), FlowSearchResult (..), FlowSummary (..), GraphExport (..), InventoryExport (..), LCIARequest (..), LCIAResult (..), LCIABatchResult(..), MappingStatus (..), MethodDetail (..), MethodFactorAPI (..), MethodSummary (..), MethodCollectionListResponse(..), MethodCollectionStatusAPI(..), ActivityContribution(..), ContributingActivitiesResult(..), RefDataListResponse(..), SynonymGroupsResponse(..), SearchResults (..), SubstitutionRequest(..), SupplyChainResponse(..), TreeExport (..), UnmappedFlowAPI (..), CharacterizationResult(..), CharacterizationEntry(..), DatabaseListResponse(..), ActivateResponse(..), LoadDatabaseResponse(..), UploadRequest(..), UploadResponse(..))
import qualified Service.Aggregate as Agg
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import GHC.Generics
import Servant
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, intercalate, sortBy, sortOn)
import Numeric (showFFloat)
import qualified Config
import qualified GHC.Stats
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
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> Get '[JSON] SupplyChainResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "aggregate"
                        :> QueryParam "scope" Text
                        :> QueryParam "is_input" Bool
                        :> QueryParam "max_depth" Int
                        :> QueryParam "filter_name" Text
                        :> QueryParam "filter_name_not" Text
                        :> QueryParam "filter_unit" Text
                        :> QueryParam "preset" Text
                        :> QueryParams "filter_classification" Text
                        :> QueryParam "filter_target_name" Text
                        :> QueryParam "filter_exchange_type" Text
                        :> QueryParam "filter_is_reference" Bool
                        :> QueryParam "group_by" Text
                        :> QueryParam "aggregate" Text
                        :> Get '[JSON] Aggregation
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Get '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "top-flows" Int :> Get '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Capture "methodId" Text :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] InventoryExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] SupplyChainResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "consumers" :> QueryParam "name" Text :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults ConsumerResult)
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "path-to" :> QueryParam "target" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "analyze" :> Capture "analyzerName" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-flows" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "limit" Int :> Get '[JSON] ContributingFlowsResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-activities" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "limit" Int :> Get '[JSON] ContributingActivitiesResult
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> "activities" :> Get '[JSON] [ActivitySummary]
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> Get '[JSON] [MethodFactorAPI]
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "mapping" :> Get '[JSON] MappingStatus
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "flow-mapping" :> Get '[JSON] FlowCFMapping
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "characterization" :> QueryParam "flow" Text :> QueryParam "limit" Int :> Get '[JSON] CharacterizationResult
                :<|> "db" :> Capture "dbName" Text :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "db" :> Capture "dbName" Text :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "exact" Bool :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "db" :> Capture "dbName" Text :> "classifications" :> Get '[JSON] [ClassificationSystem]
                :<|> "db" :> Capture "dbName" Text :> "impacts" :> Capture "processId" Text :> ReqBody '[JSON] LCIARequest :> Post '[JSON] Value
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
                :<|> "flow-synonyms" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
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
                -- Hosting config (for managed instances)
                :<|> "hosting" :> Get '[JSON] Value
                -- Runtime stats (memory usage)
                :<|> "stats" :> Get '[JSON] Value
                -- Classification presets (from TOML config)
                :<|> "classification-presets" :> Get '[JSON] [ClassificationPresetInfo]
                -- OpenAPI spec, enriched with operationId/description from API.Resources.
                -- pyvolca's runtime dispatcher reads this to route operation_id → HTTP.
                :<|> "openapi.json" :> Get '[JSON] Value
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
newtype LoginRequest = LoginRequest
    { lrCode :: Text
    } deriving (Generic)

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \v ->
        LoginRequest <$> v .: "code"

-- ToSchema orphan for the login request — lives here (not in API.OpenApi)
-- to avoid a circular dependency.
instance ToSchema LoginRequest

-- | The complete OpenAPI 3.0 specification for the VoLCA REST API.
--
-- Built in two steps:
--   1. 'toOpenApi' derives the structural spec from the 'LCAAPI' Servant type.
--   2. 'API.OpenApi.enrichWithResources' stamps @operationId@, @summary@, and
--      the long @description@ onto each operation with a matching entry in
--      'API.Resources'. This makes pyvolca's runtime dispatcher able to key
--      on @operationId@ (e.g. @"get_impacts"@).
volcaOpenApi :: OpenApi
volcaOpenApi = API.OpenApi.enrichWithResources (toOpenApi (Proxy :: Proxy LCAAPI))

-- | Expand a named classification preset from config into the
-- (system, value, exact) triples used by the Service/Aggregate layers.
-- An unknown preset name yields the empty list (same behavior as the
-- previous inline implementations in searchActivitiesWithCount and
-- getActivityConsumers).
expandPreset :: [Config.ClassificationPreset] -> Maybe Text -> [(Text, Text, Bool)]
expandPreset _ Nothing = []
expandPreset presets (Just pn) = case find (\p -> Config.cpName p == pn) presets of
    Just p  -> [(Config.ceSystem e, Config.ceValue e, Config.ceMode e == "exact") | e <- Config.cpFilters p]
    Nothing -> []

-- | API server implementation
-- DatabaseManager is used to dynamically fetch current database on each request
lcaServer :: DatabaseManager -> Int -> Maybe String -> Maybe Config.HostingConfig -> [Config.ClassificationPreset] -> Server LCAAPI
lcaServer dbManager maxTreeDepth password hostingConfig classificationPresets =
    getActivityInfo
        :<|> getActivityFlows
        :<|> getActivityInputs
        :<|> getActivityOutputs
        :<|> getActivityReferenceProduct
        :<|> getActivityTree
        :<|> getActivityInventory
        :<|> getActivityGraph
        :<|> getActivitySupplyChain
        :<|> getActivityAggregate
        :<|> getActivityLCIABatch
        :<|> postActivityLCIABatch
        :<|> getActivityLCIA
        :<|> postActivityLCIA
        :<|> postActivityInventory
        :<|> postActivitySupplyChain
        :<|> getActivityConsumers
        :<|> getActivityPathTo
        :<|> getActivityAnalyze
        :<|> getContributingFlows
        :<|> getContributingActivities
        :<|> getFlowDetail
        :<|> getFlowActivities
        :<|> getMethods
        :<|> getMethodDetail
        :<|> getMethodFactors
        :<|> getMethodMapping
        :<|> getFlowCFMapping
        :<|> getCharacterization
        :<|> searchFlows
        :<|> searchActivitiesWithCount
        :<|> getClassifications
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
        :<|> getHosting
        :<|> getStats
        :<|> getClassificationPresets
        :<|> getOpenApiSpec
  where
    getOpenApiSpec :: Handler Value
    getOpenApiSpec = return $ toJSON volcaOpenApi

    getVersion :: Handler Value
    getVersion = return $ object
        [ "version" .= Version.version
        , "gitHash" .= Version.gitHash
        , "gitTag" .= Version.gitTag
        , "buildTarget" .= Version.buildTarget
        ]

    getHosting :: Handler Value
    getHosting = return $ case hostingConfig of
        Just hc -> object
            [ "is_hosted"      .= True
            , "max_uploads"    .= Config.hcMaxUploads hc
            , "api_access"     .= Config.hcApiAccess hc
            , "upgrade_upload" .= Config.hcUpgradeUpload hc
            , "upgrade_api"    .= Config.hcUpgradeApi hc
            , "upgrade_vm_size" .= Config.hcUpgradeVmSize hc
            ]
        Nothing -> object
            [ "is_hosted"      .= False
            , "max_uploads"    .= (-1 :: Int)
            , "api_access"     .= True
            , "upgrade_upload" .= ("" :: Text)
            , "upgrade_api"    .= ("" :: Text)
            , "upgrade_vm_size" .= ("" :: Text)
            ]

    getStats :: Handler Value
    getStats = liftIO $ do
        enabled <- GHC.Stats.getRTSStatsEnabled
        if enabled then do
            stats <- GHC.Stats.getRTSStats
            return $ object
                [ "memory_used_bytes" .= GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc stats)
                , "memory_allocated_bytes" .= GHC.Stats.allocated_bytes stats
                , "gc_count" .= GHC.Stats.gcs stats
                ]
        else return $ object
            [ "error" .= ("RTS stats not enabled. Run with +RTS -T to enable." :: Text) ]

    getClassificationPresets :: Handler [ClassificationPresetInfo]
    getClassificationPresets = return $ map toInfo classificationPresets
      where
        toInfo p = ClassificationPresetInfo
            { cpiName        = Config.cpName p
            , cpiLabel       = Config.cpLabel p
            , cpiDescription = Config.cpDescription p
            , cpiFilters     = map (\e -> ClassificationEntryInfo (Config.ceSystem e) (Config.ceValue e) (Config.ceMode e)) (Config.cpFilters p)
            }

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
    getActivitySupplyChain :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> [Text] -> [Text] -> Maybe Text -> Maybe Text -> Maybe Bool -> Handler SupplyChainResponse
    getActivitySupplyChain dbName processId nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        let includeEdges = fromMaybe False includeEdgesParam
            presetFilters = expandPreset classificationPresets presetParam
            explicitFilters = zipWith3 (\s v m -> (s, v, m == "exact")) classSystems classValues
                               (classModes ++ repeat "contains")
            classFilters = presetFilters ++ explicitFilters
            af = Service.ActivityFilter
                { Service.afName = nameFilter, Service.afLocation = locationFilter, Service.afProduct = productFilter
                , Service.afClassifications = classFilters, Service.afLimit = limitParam, Service.afOffset = offsetParam
                , Service.afMaxDepth = maxDepthParam, Service.afMinQuantity = minQuantity
                , Service.afSort = sortParam, Service.afOrder = orderParam }
        result <- liftIO $ Service.getSupplyChain db sharedSolver processId af includeEdges
        case result of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left (Service.MatrixError msg) -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right supplyChain -> return supplyChain

    -- Activity aggregate endpoint (generic SQL-group-by-style aggregation)
    getActivityAggregate
        :: Text -> Text
        -> Maybe Text  -- scope
        -> Maybe Bool  -- is_input
        -> Maybe Int   -- max_depth
        -> Maybe Text  -- filter_name
        -> Maybe Text  -- filter_name_not
        -> Maybe Text  -- filter_unit
        -> Maybe Text  -- preset
        -> [Text]      -- filter_classification (repeatable: "System=Value[:exact]")
        -> Maybe Text  -- filter_target_name
        -> Maybe Text  -- filter_exchange_type ("technosphere" | "biosphere")
        -> Maybe Bool  -- filter_is_reference
        -> Maybe Text  -- group_by
        -> Maybe Text  -- aggregate fn
        -> Handler Aggregation
    getActivityAggregate dbName processId scopeParam isInputParam maxDepthParam
                        fnameParam fnameNotParam funitParam presetParam fclassParams ftargetParam
                        fexchangeTypeParam freferenceParam groupByParam aggregateParam = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        scope <- case scopeParam of
            Just "direct"       -> return Agg.ScopeDirect
            Just "supply_chain" -> return Agg.ScopeSupplyChain
            Just "biosphere"    -> return Agg.ScopeBiosphere
            _                   -> throwError err400{errBody = "scope must be one of: direct | supply_chain | biosphere"}
        exchangeType <- case fexchangeTypeParam of
            Nothing             -> return Nothing
            Just "technosphere" -> return (Just Technosphere)
            Just "biosphere"    -> return (Just Biosphere)
            Just _              -> throwError err400{errBody = "filter_exchange_type must be one of: technosphere | biosphere"}
        case (exchangeType, scope) of
            (Just _, Agg.ScopeBiosphere) ->
                throwError err400{errBody = "filter_exchange_type is redundant with scope=biosphere"}
            (Just _, Agg.ScopeSupplyChain) ->
                throwError err400{errBody = "filter_exchange_type is not supported with scope=supply_chain (all entries are technosphere)"}
            _ -> return ()
        aggFn <- case aggregateParam of
            Nothing               -> return Agg.AggSum
            Just "sum_quantity"   -> return Agg.AggSum
            Just "count"          -> return Agg.AggCount
            Just "share"          -> return Agg.AggShare
            Just other            -> throwError err400{errBody = "aggregate must be one of: sum_quantity | count | share (got " <> BSL.fromStrict (T.encodeUtf8 other) <> ")"}
        let presetFilters = expandPreset classificationPresets presetParam
            explicitFilters = mapMaybe parseClassFilter fclassParams
            params = Agg.AggregateParams
                { Agg.apScope                 = scope
                , Agg.apIsInput               = isInputParam
                , Agg.apMaxDepth              = maxDepthParam
                , Agg.apFilterName            = fnameParam
                , Agg.apFilterNameNot         = maybe [] (map T.strip . T.splitOn ",") fnameNotParam
                , Agg.apFilterUnit            = funitParam
                , Agg.apFilterClassifications = presetFilters ++ explicitFilters
                , Agg.apFilterTargetName      = ftargetParam
                , Agg.apFilterExchangeType    = exchangeType
                , Agg.apFilterIsReference     = freferenceParam
                , Agg.apGroupBy               = groupByParam
                , Agg.apAggregate             = aggFn
                }
        result <- liftIO $ Agg.aggregate db sharedSolver processId params
        case result of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left (Service.MatrixError msg) -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right agg -> return agg
      where
        -- Parse "System=Value[:exact]" into (system, value, isExact).
        parseClassFilter :: Text -> Maybe (Text, Text, Bool)
        parseClassFilter raw =
            let (sys, rest) = T.breakOn "=" raw
            in if T.null rest
                 then Nothing
                 else
                   let valAndMode = T.drop 1 rest
                       (val, mode) = T.breakOn ":" valAndMode
                       isExact = T.drop 1 mode == "exact"
                   in Just (T.strip sys, T.strip val, isExact)

    -- Activity LCIA endpoint (single method within a collection)
    getActivityLCIA :: Text -> Text -> Text -> Text -> Maybe Int -> Handler LCIAResult
    getActivityLCIA dbName processIdText _collectionName methodIdText topFlowsParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        case Service.resolveActivityAndProcessId db processIdText of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (actProcessId, activity) -> do
                inventory <- liftIO $ Matrix.computeInventoryMatrix db actProcessId
                result <- liftIO $ computeCategoryResult db activity (fromMaybe 5 topFlowsParam) inventory method
                liftIO $ logLCIAResult result method
                return result

    -- POST: LCIA with substitutions
    postActivityLCIA :: Text -> Text -> Text -> Text -> SubstitutionRequest -> Handler LCIAResult
    postActivityLCIA dbName processIdText _collectionName methodIdText subReq = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        (processId, activity) <- resolveOrThrow db processIdText
        scalingResult <- liftIO $ Service.computeScalingVectorWithSubstitutions db sharedSolver processId (srSubstitutions subReq)
        case scalingResult of
            Left err -> throwServiceError err
            Right scalingVec -> do
                let inventory = Matrix.applyBiosphereMatrix db scalingVec
                liftIO $ computeCategoryResult db activity 5 inventory method

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
                rawResults <- liftIO $ mapM (computeCategoryResult db activity 5 inventory) methods
                -- Enrich with NW data
                let results = map (enrichWithNW dcLookup mNW) rawResults
                    singleScore = case mNW of
                        Just _ -> Just $ sum [s | r <- results, Just s <- [lrWeightedScore r]]
                        Nothing -> Nothing
                    -- Compute formula-based scoring sets
                    rawScoreMap = M.fromList
                        [(lrCategory r, lrScore r) | r <- rawResults]
                -- Compute each scoring set, logging errors
                scoringResults <- liftIO $ do
                    results <- forM (mcScoringSets collection) $ \ss -> do
                        case computeFormulaScores ss rawScoreMap of
                            Right scores -> return $ Just (ssName ss, scores)
                            Left err -> do
                                reportProgress Warning $ "  Scoring set '" <> T.unpack (ssName ss)
                                    <> "' failed: " <> err
                                return Nothing
                    return $ M.fromList [(n, s) | Just (n, s) <- results]
                t2 <- liftIO getCurrentTime
                liftIO $ mapM_ (logBatchCategory invSize) results
                liftIO $ reportProgress Info $ "  Total: "
                    <> show (length results) <> " categories ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t2 t0) :: Double) "" <> "s)"
                case singleScore of
                    Just ss -> liftIO $ reportProgress Info $ "  Single score: "
                        <> showFFloat (Just 6) ss "" <> " Pt"
                    Nothing -> return ()
                forM_ (M.toList scoringResults) $ \(name, scores) ->
                    liftIO $ reportProgress Info $ "  Scoring '" <> T.unpack name <> "': "
                        <> intercalate ", " [T.unpack k <> "=" <> showFFloat (Just 6) v "" | (k, v) <- M.toList scores]
                return LCIABatchResult
                    { lbrResults = results
                    , lbrSingleScore = singleScore
                    , lbrSingleScoreUnit = if null nwSets then Nothing else Just "Pt"
                    , lbrNormWeightSetName = nwName <$> mNW
                    , lbrAvailableNWsets = map nwName nwSets
                    , lbrScoringResults = scoringResults
                    }

    -- POST: Batch LCIA with substitutions
    postActivityLCIABatch :: Text -> Text -> Text -> SubstitutionRequest -> Handler LCIABatchResult
    postActivityLCIABatch dbName processIdText collectionName subReq = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        (processId, activity) <- resolveOrThrow db processIdText
        (methods, damageCats, nwSets, scoringSets) <- loadCollection collectionName
        let dcLookup = M.fromList [(subName, dcName dc) | dc <- damageCats, (subName, _) <- dcImpacts dc]
            mNW = case nwSets of { (nw:_) -> Just nw; [] -> Nothing }
        scalingResult <- liftIO $ Service.computeScalingVectorWithSubstitutions db sharedSolver processId (srSubstitutions subReq)
        case scalingResult of
            Left err -> throwServiceError err
            Right scalingVec -> do
                let inventory = Matrix.applyBiosphereMatrix db scalingVec
                rawResults <- liftIO $ mapM (computeCategoryResult db activity 5 inventory) methods
                let results = map (enrichWithNW dcLookup mNW) rawResults
                    singleScore = case mNW of
                        Just _ -> Just $ sum [s | r <- results, Just s <- [lrWeightedScore r]]
                        Nothing -> Nothing
                    rawScoreMap = M.fromList
                        [(lrCategory r, lrScore r) | r <- rawResults]
                    scoringResults = M.fromList
                        [ (ssName ss, scores)
                        | ss <- scoringSets
                        , Right scores <- [computeFormulaScores ss rawScoreMap]
                        ]
                return LCIABatchResult
                    { lbrResults = results
                    , lbrSingleScore = singleScore
                    , lbrSingleScoreUnit = if null nwSets then Nothing else Just "Pt"
                    , lbrNormWeightSetName = nwName <$> mNW
                    , lbrAvailableNWsets = map nwName nwSets
                    , lbrScoringResults = scoringResults
                    }

    -- POST: Inventory with substitutions
    postActivityInventory :: Text -> Text -> SubstitutionRequest -> Handler InventoryExport
    postActivityInventory dbName processIdText subReq = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        (processId, activity) <- resolveOrThrow db processIdText
        scalingResult <- liftIO $ Service.computeScalingVectorWithSubstitutions db sharedSolver processId (srSubstitutions subReq)
        case scalingResult of
            Left err -> throwServiceError err
            Right scalingVec -> do
                let inventory = Matrix.applyBiosphereMatrix db scalingVec
                    inventoryExport = Service.convertToInventoryExport db processId activity inventory
                return inventoryExport

    -- POST: Supply chain with substitutions
    postActivitySupplyChain :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> [Text] -> [Text] -> Maybe Text -> Maybe Text -> Maybe Bool -> SubstitutionRequest -> Handler SupplyChainResponse
    postActivitySupplyChain dbName processIdText nameFilter limitParam minQuantityParam offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam subReq = do
        (db, sharedSolver) <- requireDatabaseByName dbManager dbName
        let includeEdges = fromMaybe False includeEdgesParam
            presetFilters = expandPreset classificationPresets presetParam
            explicitFilters = zipWith3 (\s v m -> (s, v, m == "exact")) classSystems classValues
                               (classModes ++ repeat "contains")
            classFilters = presetFilters ++ explicitFilters
            af = Service.ActivityFilter
                { Service.afName = nameFilter, Service.afLocation = locationFilter, Service.afProduct = productFilter
                , Service.afClassifications = classFilters, Service.afLimit = limitParam, Service.afOffset = offsetParam
                , Service.afMaxDepth = maxDepthParam, Service.afMinQuantity = minQuantityParam
                , Service.afSort = sortParam, Service.afOrder = orderParam }
        (processId, _) <- resolveOrThrow db processIdText
        scalingResult <- liftIO $ Service.computeScalingVectorWithSubstitutions db sharedSolver processId (srSubstitutions subReq)
        case scalingResult of
            Left err -> throwServiceError err
            Right scalingVec -> do
                let result = Service.buildSupplyChainFromScalingVector db processId scalingVec af includeEdges
                return result

    -- Activity consumers endpoint (reverse supply chain)
    getActivityConsumers :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler (SearchResults ConsumerResult)
    getActivityConsumers dbName processIdText nameFilter locationFilter productFilter presetParam classSystems classValues classModes limitParam offsetParam maxDepthParam sortParam orderParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        let presetFilters = expandPreset classificationPresets presetParam
            explicitFilters = zipWith3 (\s v m -> (s, v, m == "exact")) classSystems classValues
                               (classModes ++ repeat "contains")
            classFilters = presetFilters ++ explicitFilters
            af = Service.ActivityFilter
                { Service.afName = nameFilter, Service.afLocation = locationFilter, Service.afProduct = productFilter
                , Service.afClassifications = classFilters, Service.afLimit = limitParam, Service.afOffset = offsetParam
                , Service.afMaxDepth = maxDepthParam, Service.afMinQuantity = Nothing
                , Service.afSort = sortParam, Service.afOrder = orderParam }
        case Service.getConsumers db processIdText af of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId msg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right consumers -> return consumers

    -- Activity path-to endpoint (shortest supply chain path to first matching upstream activity)
    getActivityPathTo :: Text -> Text -> Maybe Text -> Handler Value
    getActivityPathTo dbName processIdText targetParam = do
        (db, solver) <- requireDatabaseByName dbManager dbName
        target <- maybe
            (throwError err400{errBody = "Missing required 'target' query parameter"})
            pure targetParam
        result <- liftIO $ Service.getPathTo db solver processIdText target
        case result of
            Left (Service.ActivityNotFound msg) ->
                throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left (Service.InvalidProcessId msg) ->
                throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left err ->
                throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right val -> return val

    -- Helpers for POST endpoints with substitutions
    resolveOrThrow :: Database -> Text -> Handler (ProcessId, Activity)
    resolveOrThrow db processIdText =
        case Service.resolveActivityAndProcessId db processIdText of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId msg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (pid, act) ->
                case Service.validateProcessIdInMatrixIndex db pid of
                    Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
                    Right () -> return (pid, act)

    throwServiceError :: Service.ServiceError -> Handler a
    throwServiceError (Service.ActivityNotFound _) = throwError err404{errBody = "Activity not found"}
    throwServiceError (Service.InvalidProcessId msg) = throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
    throwServiceError (Service.MatrixError msg) = throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
    throwServiceError _ = throwError err500{errBody = "Internal server error"}

    -- Log a single category result in the batch
    logBatchCategory :: Int -> LCIAResult -> IO ()
    logBatchCategory _invSize result = do
        let scoreTxt = showFFloat (Just 4) (lrScore result) ""
        reportProgress Info $ "  " <> T.unpack (lrMethodName result) <> ": "
            <> scoreTxt <> " " <> T.unpack (lrUnit result)
            <> " (" <> show (lrMappedFlows result) <> " CFs mapped)"

    -- Load a method collection by name
    loadCollection :: Text -> Handler ([Method], [DamageCategory], [NormWeightSet], [ScoringSet])
    loadCollection collectionName = do
        loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
        case M.lookup collectionName loadedCollections of
            Just mc -> return (mcMethods mc, mcDamageCategories mc, mcNormWeightSets mc, mcScoringSets mc)
            Nothing -> throwError err404{errBody = "Collection not loaded: " <> BSL.fromStrict (T.encodeUtf8 collectionName)}

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

    -- Contributing flows: top elementary flows by LCIA contribution for a specific method
    getContributingFlows :: Text -> Text -> Text -> Text -> Maybe Int -> Handler ContributingFlowsResult
    getContributingFlows dbName processIdText _collectionName methodIdText limitParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        case Service.resolveActivityAndProcessId db processIdText of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (actProcessId, _) -> do
                let mappers = prMappers (dmPlugins dbManager)
                    lim = fromMaybe 20 limitParam
                unitCfg <- liftIO $ getMergedUnitConfig dbManager
                scalingVec <- liftIO $ Matrix.computeScalingVector db actProcessId
                let inventory = Matrix.applyBiosphereMatrix db scalingVec
                mappings <- liftIO $ mapMethodToFlows mappers db method
                let score = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
                    contribs = sortOn (\(_,_,c) -> negate (abs c)) (mapMaybe (flowContribution inventory) mappings)
                    topFlows = [ FlowContributionEntry
                        { fcoFlowName    = flowName f
                        , fcoContribution = c
                        , fcoSharePct    = if score /= 0 then c / score * 100 else 0
                        , fcoFlowId      = UUID.toText (flowId f)
                        , fcoCategory    = flowCategory f
                        , fcoCompartment = flowSubcompartment f
                        , fcoCfValue     = mcfValue cf
                        }
                        | (cf, f, c) <- take lim contribs
                        ]
                return ContributingFlowsResult
                    { cfrMethod     = methodName method
                    , cfrUnit       = methodUnit method
                    , cfrTotalScore = score
                    , cfrTopFlows   = topFlows
                    }

    -- Contributing activities: top upstream activities by LCIA contribution for a specific method
    getContributingActivities :: Text -> Text -> Text -> Text -> Maybe Int -> Handler ContributingActivitiesResult
    getContributingActivities dbName processIdText _collectionName methodIdText limitParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        case Service.resolveActivityAndProcessId db processIdText of
            Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (actProcessId, _) -> do
                let mappers = prMappers (dmPlugins dbManager)
                    lim = fromMaybe 10 limitParam
                unitCfg <- liftIO $ getMergedUnitConfig dbManager
                scalingVec <- liftIO $ Matrix.computeScalingVector db actProcessId
                let inventory = Matrix.applyBiosphereMatrix db scalingVec
                mappings <- liftIO $ mapMethodToFlows mappers db method
                let score = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
                    cfMap = M.fromList [(flowId f, mcfValue cf) | (cf, Just (f, _)) <- mappings]
                    contributions = computeProcessLCIAContributions db scalingVec cfMap
                    sorted = sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
                    mkContrib (pid, c) =
                        let mAct    = Service.findActivityByProcessId db pid
                            actName = maybe "" activityName mAct
                            actLoc  = maybe "" activityLocation mAct
                            prodName = case mAct of
                                Just act -> let (pn, _, _) = Service.getReferenceProductInfo (dbFlows db) (dbUnits db) act in pn
                                Nothing  -> ""
                        in ActivityContribution
                            { acProcessId    = processIdToText db pid
                            , acActivityName = actName
                            , acProductName  = prodName
                            , acLocation     = actLoc
                            , acContribution = c
                            , acSharePct     = if score /= 0 then c / score * 100 else 0
                            }
                return ContributingActivitiesResult
                    { carMethod     = methodName method
                    , carUnit       = methodUnit method
                    , carTotalScore = score
                    , carActivities = map mkContrib (take lim sorted)
                    }

    -- Helper: compute LCIA result for a single method against an inventory
    computeCategoryResult :: Database -> Activity -> Int -> Inventory -> Method -> IO LCIAResult
    computeCategoryResult db activity topFlows inventory method = do
        let mappers = prMappers (dmPlugins dbManager)
        unitCfg <- getMergedUnitConfig dbManager
        mappings <- mapMethodToFlows mappers db method
        let stats = computeMappingStats mappings
            score = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
            (prodName, prodAmount, prodUnit) = Service.getReferenceProductInfo (dbFlows db) (dbUnits db) activity
            functionalUnit = T.pack (showFFloat (Just 2) prodAmount "") <> " " <> prodUnit <> " of " <> prodName
            contribs = sortOn (\(_,_,c) -> negate (abs c)) (mapMaybe (flowContribution inventory) mappings)
            topContribs = take topFlows contribs
            topContributors = [ FlowContributionEntry
                { fcoFlowName = flowName f
                , fcoContribution = c
                , fcoSharePct = if score /= 0 then c / score * 100 else 0
                , fcoFlowId      = UUID.toText (flowId f)
                , fcoCategory    = flowCategory f
                , fcoCompartment = flowSubcompartment f
                , fcoCfValue     = mcfValue cf
                }
                | (cf, f, c) <- topContribs
                ]
        pure LCIAResult
            { lrMethodId        = methodId method
            , lrMethodName      = methodName method
            , lrCategory        = methodCategory method
            , lrDamageCategory  = methodCategory method  -- default, enriched later
            , lrScore           = score
            , lrUnit            = methodUnit method
            , lrNormalizedScore = Nothing  -- enriched later
            , lrWeightedScore   = Nothing  -- enriched later
            , lrMappedFlows     = msTotal stats - msUnmatched stats
            , lrFunctionalUnit  = functionalUnit
            , lrTopContributors = topContributors
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
        reportProgress Info $ "[LCIA] " <> T.unpack (methodName method)
            <> ": " <> showFFloat (Just 4) (lrScore result) ""
            <> " " <> T.unpack (methodUnit method)
        reportProgress Info $ "  Flow mapping: " <> show mapped <> " CFs mapped"

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
            matchedCount = length [() | e <- entries, isJust (fceCfValue e)]
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

    -- Characterization: matched CFs for a method, filterable by flow name
    getCharacterization :: Text -> Text -> Maybe Text -> Maybe Int -> Handler CharacterizationResult
    getCharacterization dbName methodIdText flowFilter limitParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        method <- loadMethodByUUID methodIdText
        let mappers = prMappers (dmPlugins dbManager)
            lim = fromMaybe 50 limitParam
            queryLower = fmap T.toLower flowFilter
        mappings <- liftIO $ mapMethodToFlows mappers db method
        let matched = [ (cf, f, strat)
                      | (cf, Just (f, strat)) <- mappings
                      , matchesQuery queryLower (mcfFlowName cf) (flowName f)
                      ]
            sorted = sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
            top = take lim sorted
            mkEntry (cf, f, strat) = CharacterizationEntry
                { cheMethodFlowName = mcfFlowName cf
                , cheCfValue        = mcfValue cf
                , cheCfUnit         = mcfUnit cf
                , cheDirection      = case mcfDirection cf of
                    Input  -> "Input"
                    Output -> "Output"
                , cheDbFlowName     = flowName f
                , cheFlowId         = UUID.toText (flowId f)
                , cheFlowUnit       = getUnitNameForFlow (dbUnits db) f
                , cheCategory       = flowCategory f
                , cheCompartment    = flowSubcompartment f
                , cheMatchStrategy  = strategyToText strat
                }
        return CharacterizationResult
            { chrMethod  = methodName method
            , chrUnit    = methodUnit method
            , chrMatches = length matched
            , chrShown   = length top
            , chrFactors = map mkEntry top
            }

    matchesQuery :: Maybe Text -> Text -> Text -> Bool
    matchesQuery Nothing _ _ = True
    matchesQuery (Just q) cfName dbName = T.isInfixOf q (T.toLower cfName) || T.isInfixOf q (T.toLower dbName)

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
                , mcaFormat = Just (mcsFormat s)
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
    searchFlows :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler (SearchResults FlowSearchResult)
    searchFlows dbName queryParam langParam limitParam offsetParam sortParam orderParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        searchFlowsInternal db queryParam langParam limitParam offsetParam sortParam orderParam

    -- Search activities by specific fields with pagination and count
    searchActivitiesWithCount :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler (SearchResults ActivitySummary)
    searchActivitiesWithCount dbName nameParam geoParam productParam exactParam presetParam classSystems classValues classModes limitParam offsetParam sortParam orderParam = do
        (db, _) <- requireDatabaseByName dbManager dbName
        -- Expand preset filters then merge with explicit classification params
        let exactMatch = fromMaybe False exactParam
            presetFilters = expandPreset classificationPresets presetParam
            explicitFilters = zipWith3 (\s v m -> (s, v, m == "exact")) classSystems classValues
                               (classModes ++ repeat "contains")
            classFilters = presetFilters ++ explicitFilters
        result <- liftIO $ Service.searchActivities db nameParam geoParam productParam classFilters exactMatch limitParam offsetParam sortParam orderParam
        case result of
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right jsonValue -> case fromJSON jsonValue of
                Success searchResults -> return searchResults
                Error parseErr -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack parseErr}

    getClassifications :: Text -> Handler [ClassificationSystem]
    getClassifications dbName = do
        (db, _) <- requireDatabaseByName dbManager dbName
        return $ Service.getClassifications db

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
        limit = fromMaybe totalCount limitParam -- Default: return all results
        offset = fromMaybe 0 offsetParam -- Default offset: 0
        paginatedResults = take limit $ drop offset results
        hasMore = offset + length paginatedResults < totalCount
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ SearchResults paginatedResults totalCount offset limit hasMore searchTimeMs

-- | Internal helper for flow search with optional language filtering
searchFlowsInternal :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Handler (SearchResults FlowSearchResult)
searchFlowsInternal _ Nothing _ _ _ _ _ = return $ SearchResults [] 0 0 50 False 0.0
searchFlowsInternal db (Just query) _langParam limitParam offsetParam sortParam orderParam = do
    -- Language filtering not yet implemented, search all synonyms
    let flows = findFlowsBySynonym db query
        allResults = [FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow)) | flow <- flows]
        isDesc = orderParam == Just "desc"
        fsCmp = case sortParam of
            Just "category" -> \a b -> compare (fsrCategory a) (fsrCategory b)
            Just "unit"     -> \a b -> compare (fsrUnitName a) (fsrUnitName b)
            _               -> \a b -> compare (fsrName a)     (fsrName b)
        sorted = sortBy (if isDesc then flip fsCmp else fsCmp) allResults
    liftIO $ paginateResults sorted limitParam offsetParam

-- | Proxy for the API
lcaAPI :: Proxy LCAAPI
lcaAPI = Proxy

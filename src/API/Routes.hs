{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Csv (CSV)
import API.DatabaseHandlers (explainCFToAPI, simpleAction)
import qualified API.DatabaseHandlers as DBHandlers
import qualified API.OpenApi
import API.Types (ActivateResponse (..), ActivityContribution (..), ActivityInfo (..), ActivityInput (..), ActivitySummary (..), ActivityWriteRequest (..), ActivityWriteResponse (..), Aggregation (..), BatchImpactsEntry (..), BatchImpactsRequest (..), BatchImpactsResponse (..), BinaryContent (..), CharacterizationEntry (..), CharacterizationResult (..), ClassificationEntryInfo (..), ClassificationPresetInfo (..), ClassificationSystem (..), CollectionCoverage (..), ComputedQualityReportAPI (..), ConsumersResponse (..), ContributingActivitiesResult (..), ContributingFlowsResult (..), CoverageReportAPI (..), CutoffWasteFlow (..), DatabaseListResponse (..), DeleteSelectionRequest (..), DeleteSelectionResponse (..), ExchangeDetail (..), ExchangeEditRequest (..), ExchangeEditResponse (..), ExplainCFResult (..), ExportRequest (..), FlowCFEntry (..), FlowCFMapping (..), FlowContributionEntry (..), FlowDetail (..), FlowSearchResult (..), FlowSummary (..), GapReportAPI (..), GraphExport (..), HostingInfo (..), InventoryExport (..), LCIABatchResult (..), LCIAResult (..), LoadDatabaseResponse (..), MappingStatus (..), MethodCollectionListResponse (..), MethodCollectionStatusAPI (..), MethodDetail (..), MethodFactorAPI (..), MethodSummary (..), PerturbedEntry (..), QualityReportAPI (..), RefDataListResponse (..), RelinkRequest (..), RelinkResponse (..), ScoringIndicator (..), SearchCountsAPI (..), SearchResults (..), SensitivityRequest (..), SensitivityResponse (..), SubstitutionRequest (..), SupplyChainResponse (..), SynonymGroupsResponse (..), TreeExport (..), UnmappedFlowAPI (..), UploadChunk (..), UploadResponse (..), apiFlowOfKind, parseProducerFilter)
import App.Env (AppEnv (..), AppM, runApp)
import qualified Config
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, mfilter, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAscii, isControl)
import Data.Foldable (asum)
import Data.List (intercalate, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.OpenApi (OpenApi, ToSchema)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Validation as V
import qualified Data.Vector as V
import Database
import qualified Database.ComputedQuality as CQ
import Database.Manager (DatabaseManager (..), DatabaseSetupInfo (..), LoadedDatabase (..), MethodCollectionStatus (..), getDatabase, getMergedUnitConfig)
import qualified Database.Manager as DM
import qualified Expr
import GHC.Generics
import qualified GHC.Stats
import qualified Impact
import Matrix (Inventory, Vector)
import qualified Method.Explain as Explain
import Method.Mapping (BuildProvenance (..), CF (..), LCIAOutcome (..), LongTermMode (..), MappingStats (..), MethodTables (..), TableEntry (..), applyLongTermMode, characterizedFlowIds, computeLCIAScoreFromTables, computeLCIAScoreSetFromTables, computeMappingStats, inventoryContributions, longTermModeFromExclude, lookupEntryForFlow, provenanceStrategyText, strategyToText)
import qualified Method.Mapping
import Method.Types (DamageCategory (..), Method (..), MethodCF (..), MethodCollection (..), NormWeightSet (..), ScoringEvaluation (..), ScoringSet (..), computeFormulaScores)
import qualified Method.Types as MT
import Numeric (showFFloat)
import Progress (ProgressLevel (Info, Warning), getLogLines, reportProgress)
import qualified Search.Normalize as Normalize
import Servant
import Servant.OpenApi (toOpenApi)
import qualified Service
import qualified Service.Aggregate as Agg
import SharedSolver (SharedSolver)
import qualified SharedSolver
import Tree (buildLoopAwareTree)
import Types
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
                :<|> "db"
                    :> Capture "dbName" Text
                    :> "activity"
                    :> Capture "processId" Text
                    :> "aggregate"
                    :> QueryParam "scope" Text
                    :> QueryParam "is_input" Bool
                    :> QueryParam "max_depth" Int
                    :> QueryParam "filter_name" Text
                    :> QueryParam "filter_name_not" Text
                    :> QueryParam "filter_unit" Text
                    :> QueryParam "preset" Text
                    :> QueryParams "filter_classification" Text
                    :> QueryParam "filter_target_name" Text
                    :> QueryParam "filter_consumer" Text
                    :> QueryParam "filter_consumer_not" Text
                    :> QueryParam "filter_exchange_type" Text
                    :> QueryParam "filter_is_reference" Bool
                    :> QueryParam "group_by" Text
                    :> QueryParam "aggregate" Text
                    :> Get '[JSON] Aggregation
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> QueryParam "exclude-long-term" Bool :> Get '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> QueryParam "exclude-long-term" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "top-flows" Int :> Get '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Capture "methodId" Text :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "sensitivity" :> Capture "collection" Text :> Capture "methodId" Text :> ReqBody '[JSON] SensitivityRequest :> Post '[JSON] SensitivityResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] InventoryExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] SupplyChainResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "consumers" :> QueryParam "name" Text :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> Get '[JSON] ConsumersResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "path-to" :> QueryParam "target" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-flows" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingFlowsResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-activities" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingActivitiesResult
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> "activities" :> QueryParam "role" Text :> Get '[JSON] [ActivitySummary]
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> Get '[JSON] [MethodFactorAPI]
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "mapping" :> Get '[JSON] MappingStatus
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "flow-mapping" :> Get '[JSON] FlowCFMapping
                :<|> "db" :> Capture "dbName" Text :> "method-collection" :> Capture "collection" Text :> "coverage" :> Get '[JSON] CollectionCoverage
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "characterization" :> QueryParam "flow" Text :> QueryParam "limit" Int :> Get '[JSON] CharacterizationResult
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "explain-cf" :> Capture "flowId" Text :> Get '[JSON] ExplainCFResult
                :<|> "db" :> Capture "dbName" Text :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "kind" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "db" :> Capture "dbName" Text :> "search-counts" :> QueryParam "q" Text :> QueryParam "sort" Text :> QueryParam "exact" Bool :> Get '[JSON] SearchCountsAPI
                :<|> "db" :> Capture "dbName" Text :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "exact" Bool :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "db" :> Capture "dbName" Text :> "classifications" :> Get '[JSON] [ClassificationSystem]
                :<|> "db" :> Capture "dbName" Text :> "impacts" :> Capture "collection" Text :> QueryParam "top-flows" Int :> QueryParam "exclude-long-term" Bool :> ReqBody '[JSON] BatchImpactsRequest :> Post '[JSON] BatchImpactsResponse
                -- Database management endpoints
                :<|> "db" :> Get '[JSON] DatabaseListResponse
                -- Load/Unload/Delete endpoints
                :<|> "db" :> Capture "dbName" Text :> "load" :> Post '[JSON] LoadDatabaseResponse
                :<|> "db" :> Capture "dbName" Text :> "unload" :> Post '[JSON] ActivateResponse
                -- Relink: empty {} body = plain relink; {depDb,mappingCsv} = mapping relink
                :<|> "db" :> Capture "dbName" Text :> "relink" :> ReqBody '[JSON] RelinkRequest :> Post '[JSON] RelinkResponse
                -- Supplier-gap report: what is still unsupplied after linking (read-only relink companion)
                :<|> "db" :> Capture "dbName" Text :> "gap-report" :> QueryParam "limit" Int :> Get '[JSON] GapReportAPI
                -- Dataset-soundness report: what is malformed in the database itself
                :<|> "db" :> Capture "dbName" Text :> "quality-report" :> QueryParam "limit" Int :> Get '[JSON] QualityReportAPI
                -- The same report as a file: one row per finding, for a spreadsheet or a shell
                :<|> "db" :> Capture "dbName" Text :> "quality-report.csv" :> QueryParam "limit" Int :> Get '[CSV] (Headers '[Header "Content-Disposition" Text] QualityReportAPI)
                -- Computed checks: what a loaded database computes, judged against its own norms
                :<|> "db" :> Capture "dbName" Text :> "computed-quality-report" :> QueryParam "collection" Text :> QueryParam "limit" Int :> Get '[JSON] ComputedQualityReportAPI
                :<|> "db" :> Capture "dbName" Text :> "computed-quality-report.csv" :> QueryParam "collection" Text :> QueryParam "limit" Int :> Get '[CSV] (Headers '[Header "Content-Disposition" Text] ComputedQualityReportAPI)
                -- Characterization-coverage report: flows a method collection scores only through a name bridge
                :<|> "db" :> Capture "dbName" Text :> "characterization-coverage" :> QueryParam "collection" Text :> QueryParam "limit" Int :> Get '[JSON] CoverageReportAPI
                :<|> "db" :> Capture "dbName" Text :> "copy" :> Capture "newName" Text :> Post '[JSON] ActivateResponse
                -- Read a source again under another allocation key. A load, not a
                -- copy: the key decides the inventory, so it answers as a load does
                :<|> "db" :> Capture "dbName" Text :> "derive" :> Capture "newName" Text :> QueryParam "allocation" Text :> Post '[JSON] LoadDatabaseResponse
                :<|> "db" :> Capture "dbName" Text :> Delete '[JSON] ActivateResponse
                -- Delete the whole filtered set of activities (selection in JSON body)
                :<|> "db" :> Capture "dbName" Text :> "delete" :> ReqBody '[JSON] DeleteSelectionRequest :> Post '[JSON] DeleteSelectionResponse
                -- Write activities. POST adds to the collection, PUT rewrites the one
                -- addressed — never both, so a mistyped identity fails instead of
                -- quietly becoming a duplicate of the row it meant to correct.
                :<|> "db" :> Capture "dbName" Text :> "activities" :> ReqBody '[JSON] ActivityWriteRequest :> Post '[JSON] ActivityWriteResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> ReqBody '[JSON] ActivityInput :> Put '[JSON] ActivityWriteResponse
                -- Change the inventory of one activity, keeping everything else it
                -- carries — the operation a PUT cannot do for a row that came in
                -- from a database file rather than from a description
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "exchanges" :> ReqBody '[JSON] ExchangeEditRequest :> Post '[JSON] ExchangeEditResponse
                -- Export a loaded database as raw bytes in the requested format;
                -- approximation warnings travel percent-encoded in a response header
                :<|> "db" :> Capture "dbName" Text :> "export" :> ReqBody '[JSON] ExportRequest :> Post '[OctetStream] (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
                -- Upload endpoint (streamed octet-stream body; metadata in query params)
                :<|> "db" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
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
                :<|> "method-collections" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                -- Export a loaded method collection as raw bytes (SimaPro CSV);
                -- projection warnings travel percent-encoded in a response header
                :<|> "method-collections" :> Capture "name" Text :> "export" :> ReqBody '[JSON] ExportRequest :> Post '[OctetStream] (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
                -- Reference data endpoints (flow synonyms, compartment mappings, units)
                :<|> "flow-synonyms" :> Get '[JSON] RefDataListResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "groups" :> Get '[JSON] SynonymGroupsResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
                :<|> "compartment-mappings" :> Get '[JSON] RefDataListResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "units" :> Get '[JSON] RefDataListResponse
                :<|> "units" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "units" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
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

{- | Standard prefix for "database not loaded" 404 bodies. Exported so that
'API.BatchImpacts.translateError' can recover a typed 'DatabaseNotLoaded'
from the wire body without the two ends silently drifting apart.
-}
databaseNotLoadedPrefix :: Text
databaseNotLoadedPrefix = "Database not loaded: "

-- | Same idea for "collection not loaded" bodies and messages.
collectionNotLoadedPrefix :: Text
collectionNotLoadedPrefix = "Collection not loaded: "

-- | Build the 404 body for a database that is not loaded.
databaseNotLoadedBody :: Text -> BSL.ByteString
databaseNotLoadedBody name = BSL.fromStrict (T.encodeUtf8 (databaseNotLoadedPrefix <> name))

{- | What every surface says when a collection is not loaded: the name asked
for, then the ones that are, since a caller cannot guess names that come from
the operator's configuration file. The one wording, used by the HTTP body
below and by the MCP messages ('API.MCP.batchErrorMsg',
'API.MCP.callListScoringSets'), so a single refusal does not read three ways.

The names go on a second line because 'API.BatchImpacts.translateError' reads
the first one back as the requested name; 'BatchImpactsSpec' builds a message
here and parses it there, which is what keeps the two halves honest.
-}
collectionNotLoadedMessage :: Text -> [Text] -> Text
collectionNotLoadedMessage name loaded =
    collectionNotLoadedPrefix <> name <> "\nAvailable collections: " <> available
  where
    available
        | null loaded = "none loaded"
        | otherwise = T.intercalate ", " loaded

-- | The same message as a 404 body.
collectionNotLoadedBody :: Text -> [Text] -> BSL.ByteString
collectionNotLoadedBody name = BSL.fromStrict . T.encodeUtf8 . collectionNotLoadedMessage name

-- | Get database by name, throw 404 if not loaded
requireDatabaseByName :: Text -> AppM (Database, SharedSolver)
requireDatabaseByName dbName = do
    dbManager <- asks aeDbManager
    maybeLoaded <- liftIO $ getDatabase dbManager dbName
    case maybeLoaded of
        Just loaded -> return (ldDatabase loaded, ldSharedSolver loaded)
        Nothing -> throwError err404{errBody = databaseNotLoadedBody dbName}

{- | Refuse LCIA when the DB still has unresolved cross-DB products. Forces
the user to load the missing dep DBs (or POST {} to /relink) rather than
silently undercounting impacts.
-}
requireFullyLinked :: Text -> Database -> AppM ()
requireFullyLinked dbName db =
    let n = unresolvedCount (dbLinkingStats db)
     in when (n > 0) $
            throwError
                err422
                    { errBody =
                        BSL.fromStrict $
                            T.encodeUtf8 $
                                "Database \""
                                    <> dbName
                                    <> "\" has "
                                    <> T.pack (show n)
                                    <> " unresolved cross-DB products. Load the missing dependency "
                                    <> "databases (see GET /api/v1/db/"
                                    <> dbName
                                    <> "/setup) then POST {} to /api/v1/db/"
                                    <> dbName
                                    <> "/relink."
                    }

-- | Inventory with cross-DB back-substitution; maps unit-conversion errors to 422.
inventoryWithDeps :: Text -> Database -> SharedSolver -> ProcessId -> AppM Inventory
inventoryWithDeps dbName db solver pid =
    SharedSolver.csInventory <$> solutionWithDeps dbName db solver pid

{- | Cross-DB inventory + per-DB scaling vectors. The scalings are needed by
the regionalized LCIA path (per-DB dot products summed across all DBs
reached at request time); the inventory alone is enough for non-regional
methods.
-}
solutionWithDeps :: Text -> Database -> SharedSolver -> ProcessId -> AppM SharedSolver.CrossDBSolution
solutionWithDeps dbName db solver pid = do
    dbManager <- asks aeDbManager
    requireFullyLinked dbName db
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    res <-
        liftIO $
            SharedSolver.computeInventoryMatrixWithDepsCached
                unitCfg
                (DM.mkDepSolverLookup dbManager)
                db
                dbName
                solver
                pid
    case res of
        Right sol -> pure sol
        Left err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err}

-- | Batch variant of 'solutionWithDeps'.
solutionsWithDeps :: Text -> Database -> SharedSolver -> [ProcessId] -> AppM [SharedSolver.CrossDBSolution]
solutionsWithDeps dbName db solver pids = do
    dbManager <- asks aeDbManager
    requireFullyLinked dbName db
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    res <-
        liftIO $
            SharedSolver.computeInventoryMatrixBatchWithDepsCached
                unitCfg
                (DM.mkDepSolverLookup dbManager)
                db
                dbName
                solver
                pids
    case res of
        Right sols -> pure sols
        Left err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err}

{- | Per-method static context, prepared ONCE per batch request and reused
across every pid in that request. Carries the precomputed mapping stats
and total mapped-flow count; combined with the precomputed broadcast
(and regionalized activity weights) on 'MethodTables', this is enough
to build an LCIAResult per pid in O(1) per method — no TVar reads, no
per-pid cache lookups, no inventoryContributions walk.
-}
data MethodCtx = MethodCtx
    { mctxMethod :: !Method
    , mctxMappedFlows :: !Int
    }

{- | Build an 'ActivityContribution' row from a cross-DB contribution key
@(depDbName, pid)@. For dep-DB rows the process ID is qualified as
@"dbName::actUUID_prodUUID"@ — same convention as
'Service.hs' (activity-detail endpoint) so the UI's existing
cross-DB navigation handles it. Root-DB rows keep the bare @pid@ form.
-}
mkCrossDBContrib ::
    DatabaseManager ->
    -- | root DB name
    Text ->
    -- | merged biosphere flow DB
    BioFlowDB ->
    -- | merged unitDB
    UnitDB ->
    -- | total score (for share %)
    Double ->
    ((Text, ProcessId), Double) ->
    IO ActivityContribution
mkCrossDBContrib dbManager rootDbName _flowDB unitDB score ((depDbName, pid), c) = do
    mLd <- DM.getDatabase dbManager depDbName
    pure $ case mLd of
        Just ld ->
            let d = ldDatabase ld
                mAct = Service.findActivityByProcessId d pid
                pidText =
                    if depDbName == rootDbName
                        then processIdToText d pid
                        else qualifyRef depDbName (processIdToText d pid)
                -- Reference products are technosphere; pull from the dep DB's tech flows.
                (prodName, _, _) = maybe ("", 0, "") (Service.getReferenceProductInfo (dbTechFlows d) unitDB) mAct
             in ActivityContribution
                    { acProcessId = pidText
                    , acActivityName = maybe "" activityName mAct
                    , acProductName = prodName
                    , acLocation = maybe "" activityLocation mAct
                    , acContribution = c
                    , acSharePct = if score /= 0 then c / score * 100 else 0
                    }
        Nothing ->
            ActivityContribution
                { acProcessId = depDbName <> "::<unloaded>"
                , acActivityName = ""
                , acProductName = ""
                , acLocation = ""
                , acContribution = c
                , acSharePct = if score /= 0 then c / score * 100 else 0
                }

-- | Helper function to validate ProcessId and lookup activity
withValidatedActivity :: Database -> Text -> (Activity -> AppM a) -> AppM a
withValidatedActivity db processId action =
    either throwServiceError action (Service.resolveActivityByProcessId db processId)

{- | Helper function to validate UUID and lookup flow. Returns a tagged sum
so callers can dispatch on tech vs bio.
-}
withValidatedFlow :: Database -> Text -> (FlowKind -> AppM a) -> AppM a
withValidatedFlow db uuid action = do
    validUuid <- either throwServiceError pure (Service.validateUUID uuid)
    let lookups =
            [ TechKind <$> M.lookup validUuid (dbTechFlows db)
            , BioKind <$> M.lookup validUuid (dbBioFlows db)
            , WasteKind <$> M.lookup validUuid (dbWasteFlows db)
            ]
    case asum lookups of
        Just flow -> action flow
        Nothing -> throwError err404{errBody = "Flow not found"}

-- | Login request body
newtype LoginRequest = LoginRequest
    { lrCode :: Text
    }
    deriving (Generic)

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \v ->
        LoginRequest <$> v .: "code"

-- ToSchema orphan for the login request — lives here (not in API.OpenApi)
-- to avoid a circular dependency.
instance ToSchema LoginRequest

{- | The complete OpenAPI 3.0 specification for the VoLCA REST API.

Built in two steps:
  1. 'toOpenApi' derives the structural spec from the 'LCAAPI' Servant type.
  2. 'API.OpenApi.enrichWithResources' stamps @operationId@, @summary@, and
     the long @description@ onto each operation with a matching entry in
     'API.Resources'. This makes pyvolca's runtime dispatcher able to key
     on @operationId@ (e.g. @"get_impacts"@).
-}
volcaOpenApi :: OpenApi
volcaOpenApi = API.OpenApi.stampInfo (API.OpenApi.enrichWithResources (toOpenApi (Proxy :: Proxy LCAAPI)))

-- ============================================================================
-- Hoisted helpers — previously in lcaServer's `where`. Lifted to top level so
-- non-Servant callers (notably src/API/BatchImpacts.hs and any client of the
-- LCIA batch pipeline outside the Servant AppM stack) can reuse them.
--
-- Behavior is byte-identical to the original where-bound versions.
-- ============================================================================

{- | The damage category each sub-indicator rolls up into, which is the table
'enrichWithNW' reads.

'M.fromList' is the right answer here and the repeated key is not a mistake to
refuse: a sub-indicator belonging to two damage categories is how an endpoint
method is built - climate change counts towards human health and towards
ecosystems both - so a refusal would reject a valid collection. No method that
loads today declares one twice.
-}
damageCategoryIndex :: [DamageCategory] -> M.Map Text Text
damageCategoryIndex damageCats =
    M.fromList [(subName, dcName dc) | dc <- damageCats, (subName, _) <- dcImpacts dc]

-- | Enrich a raw LCIA result with damage category mapping and NW scores. Pure.
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
     in result
            { lrDamageCategory = dmgCat
            , lrNormalizedScore = normScore
            , lrWeightedScore = weightScore
            }

-- | Assemble an LCIABatchResult from the post-characterization parts. Pure.
mkLCIABatchResult ::
    [LCIAResult] ->
    Maybe NormWeightSet ->
    [NormWeightSet] ->
    M.Map Text (M.Map Text Double) ->
    [ScoringSet] ->
    M.Map Text (M.Map Text ScoringIndicator) ->
    [CutoffWasteFlow] ->
    LCIABatchResult
mkLCIABatchResult results mNW nwSets scoringResults scoringSets scoringIndicators cutoffWaste =
    LCIABatchResult
        { lbrResults = results
        , lbrSingleScore = Nothing
        , lbrSingleScoreUnit = Nothing
        , lbrNormWeightSetName = nwName <$> mNW
        , lbrAvailableNWsets = map nwName nwSets
        , lbrScoringResults = scoringResults
        , lbrScoringUnits = M.fromList [(ssName ss, ssUnit ss) | ss <- scoringSets]
        , lbrScoringIndicators = scoringIndicators
        , lbrCutoffWaste = cutoffWaste
        }

-- | Per-category single-line log within a batch.
logBatchCategory :: Int -> LCIAResult -> IO ()
logBatchCategory _invSize result = do
    let scoreTxt = showFFloat (Just 4) (lrScore result) ""
    reportProgress Info $
        "  "
            <> T.unpack (lrMethodName result)
            <> ": "
            <> scoreTxt
            <> " "
            <> T.unpack (lrUnit result)
            <> " ("
            <> show (lrMappedFlows result)
            <> " CFs mapped)"

-- | Single-method LCIA log line.
logLCIAResult :: LCIAResult -> Method -> IO ()
logLCIAResult result method = do
    let mapped = lrMappedFlows result
    reportProgress Info $
        "[LCIA] "
            <> T.unpack (methodName method)
            <> ": "
            <> showFFloat (Just 4) (lrScore result) ""
            <> " "
            <> T.unpack (methodUnit method)
    reportProgress Info $ "  Flow mapping: " <> show mapped <> " CFs mapped"

{- | Resolve a process_id text against a database, throwing the appropriate
HTTP status. Validates the resolved ProcessId against the technosphere
matrix index too — see Service.validateProcessIdInMatrixIndex.
-}
resolveOrThrow :: Database -> Text -> AppM (ProcessId, Activity)
resolveOrThrow db processIdText = do
    (pid, act) <- either throwServiceError pure (Service.resolveScorable db processIdText)
    either throwServiceError pure (Service.validateProcessIdInMatrixIndex db pid)
    pure (pid, act)

{- | Pure mapping from a domain 'Service.ServiceError' to the HTTP error it
surfaces as. Every constructor is a client-supplied invariant breakage, so all
map to 4xx/422 — never 5xx. Kept pure (and separate from 'throwServiceError')
so the status contract is unit-testable without booting a server.
-}
serviceErrorToServerError :: Service.ServiceError -> ServerError
serviceErrorToServerError = \case
    Service.InvalidUUID msg -> err400{errBody = utf8Body msg}
    Service.InvalidProcessId msg -> err400{errBody = utf8Body msg}
    -- The activity is there; the query does not say which of its products.
    -- A 404 would send the caller looking for data that is present.
    Service.AmbiguousActivity msg -> err400{errBody = utf8Body msg}
    Service.ActivityNotFound _ -> err404{errBody = "Activity not found"}
    Service.FlowNotFound _ -> err404{errBody = "Flow not found"}
    -- The activity exists and reads fine; it has no column to score. The
    -- message says why, so the caller can repair the dataset or pick another.
    Service.NotScorable msg -> err422{errBody = utf8Body msg}
    -- MatrixError covers singular Sherman-Morrison, missing technosphere links,
    -- and cross-DB unit-conversion failures — all client-submitted invariant
    -- breakages. Surface as 422 like the rest of the cross-DB pipeline.
    Service.MatrixError msg -> err422{errBody = utf8Body msg}
  where
    utf8Body = BSL.fromStrict . T.encodeUtf8

throwServiceError :: Service.ServiceError -> AppM a
throwServiceError = throwError . serviceErrorToServerError

-- | Answer 400 with a message the caller can act on, rather than a body-less status.
badRequest :: Text -> AppM a
badRequest msg = throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

-- | Load a method collection by name from the live DatabaseManager state.
loadCollection :: DM.CollectionName -> AppM ([Method], [DamageCategory], [NormWeightSet], [ScoringSet])
loadCollection collectionName = do
    dbManager <- asks aeDbManager
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    case M.lookup (DM.unCollectionName collectionName) loadedCollections of
        Just mc -> return (mcMethods mc, mcDamageCategories mc, mcNormWeightSets mc, mcScoringSets mc)
        Nothing ->
            throwError
                err404
                    { errBody =
                        collectionNotLoadedBody
                            (DM.unCollectionName collectionName)
                            (M.keys loadedCollections)
                    }

{- | Cross-DB inventory solution for an activity. 'Nothing' takes the cached
no-substitution path ('requireFullyLinked' runs inside 'solutionWithDeps');
'Just' applies the substitutions through the uncached path.
-}
crossDBSolutionFor :: Text -> Database -> SharedSolver -> ProcessId -> Maybe SubstitutionRequest -> AppM SharedSolver.CrossDBSolution
crossDBSolutionFor dbName db solver pid mSub = case mSub of
    Nothing -> solutionWithDeps dbName db solver pid
    Just subReq -> do
        dbManager <- asks aeDbManager
        requireFullyLinked dbName db
        unitCfg <- liftIO $ getMergedUnitConfig dbManager
        eSol <-
            liftIO $
                Service.inventoryWithSubsAndDeps
                    unitCfg
                    (DM.mkDepSolverLookup dbManager)
                    db
                    dbName
                    solver
                    pid
                    (srSubstitutions subReq)
        either throwServiceError pure eSol

{- | Apply the request's long-term policy to a freshly solved inventory. Drops
the delayed long-term emission flows when excluding, and is a no-op (with no
extra work) when including. Every downstream scoring path reads @csInventory@,
so filtering here once keeps scores and top-contributor breakdowns consistent.
-}
applyLongTermToSolution :: DatabaseManager -> LongTermMode -> SharedSolver.CrossDBSolution -> IO SharedSolver.CrossDBSolution
applyLongTermToSolution _ IncludeLongTerm sol = pure sol
applyLongTermToSolution dbManager ExcludeLongTerm sol = do
    (mFlows, _) <- DM.getMergedFlowMetadata dbManager
    pure sol{SharedSolver.csInventory = applyLongTermMode mFlows ExcludeLongTerm (SharedSolver.csInventory sol)}

{- | Look up MethodSetTables for every (DB, scaling) pair in the cross-DB
solution. Cache-keyed by (dbName, methods).
-}
buildPerDbSetTables ::
    DatabaseManager ->
    DM.CollectionName ->
    NE.NonEmpty (Text, Database, Vector) ->
    [Method] ->
    IO (NE.NonEmpty (Database, Vector, Method.Mapping.MethodSetTables))
buildPerDbSetTables dbManager collection scalings methods =
    traverse
        ( \(n, d, sv) -> do
            mst <- DM.mapMethodSetToTablesCached dbManager n collection d methods
            pure (d, sv, mst)
        )
        scalings

{- | Score every method in 'methods' against the inventory in 'sol' in a
single dense matvec (non-regional) plus per-method passes (regional).
-}
batchedScoresFor ::
    DatabaseManager ->
    Text ->
    DM.CollectionName ->
    Database ->
    SharedSolver.CrossDBSolution ->
    [Method] ->
    IO (M.Map UUID (Either Text Double))
batchedScoresFor dbManager _dbName collection _db sol methods = do
    perDb <- buildPerDbSetTables dbManager collection (SharedSolver.csScalings sol) methods
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    let hier = DM.dmLocationHierarchy dbManager
    pure $
        M.fromList $
            computeLCIAScoreSetFromTables
                unitCfg
                mUnits
                mFlows
                (SharedSolver.csInventory sol)
                hier
                perDb

{- | Resolve a method's precomputed batched score. A 'Left' here is a scoring
integrity error (mismatched table lengths, absent weights — never a mere
coverage gap, see 'computeRegionalizedLCIAScore'): it must reach the caller
as an error, never collapse to a 0 the consumer cannot tell from a real
score. A method missing from the map is the same kind of error — the map is
built from the very method list being scored.
-}
resolveBatchedScore :: Method -> M.Map UUID (Either Text Double) -> Either Text Double
resolveBatchedScore method scoreMap =
    case M.lookup (methodId method) scoreMap of
        Nothing -> Left ("[LCIA " <> methodName method <> "] missing from the batched score set")
        Just (Left err) -> Left ("[LCIA " <> methodName method <> "] " <> err)
        Just (Right s) -> Right s

{- | Surface a scoring integrity error as a 500: the collection's tables are
inconsistent and the score is not computable — a silent 0 would be worse
than the failure.
-}
scoringError :: Text -> AppM a
scoringError err = throwError err500{errBody = BSL.fromStrict (T.encodeUtf8 err)}

{- | Pre-warm per-(db, method) cached tables so subsequent 'batchedScoresFor'
and 'inventoryContributions' calls hit a warm cache.
-}
prepMethodCtx :: DatabaseManager -> Text -> DM.CollectionName -> Database -> Method -> IO MethodCtx
prepMethodCtx dbManager dbName collection db method = do
    mappings <- DM.mapMethodToFlowsCached dbManager dbName collection db method
    _ <- DM.mapMethodToTablesCached dbManager dbName collection db method
    let stats = computeMappingStats mappings
    pure MethodCtx{mctxMethod = method, mctxMappedFlows = msTotal stats - msUnmatched stats}

{- | Compute LCIA result for a single method against a cross-DB inventory
solution. 'precomputedScore' short-circuits the per-method scoring loop
when a batched matvec result is already available.
-}
computeCategoryResult ::
    DatabaseManager ->
    Text ->
    DM.CollectionName ->
    Database ->
    SharedSolver.CrossDBSolution ->
    Activity ->
    Int ->
    Maybe (Either Text Double) ->
    Method ->
    IO (Either Text LCIAResult)
computeCategoryResult dbManager dbName collection db sol activity topFlows precomputedScore method = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    mappings <- DM.mapMethodToFlowsCached dbManager dbName collection db method
    tables <- DM.mapMethodToTablesCached dbManager dbName collection db method
    let inventory = SharedSolver.csInventory sol
    let stats = computeMappingStats mappings
    -- A Left is a scoring integrity error (see 'resolveBatchedScore') — it
    -- propagates instead of collapsing to a 0 the consumer can't tell from a
    -- real score. A precomputed Left arrives already labeled by
    -- 'resolveBatchedScore'; only the locally computed one is labeled here.
    scoreE <- case precomputedScore of
        Just e -> traverse evaluate e
        Nothing -> Impact.scoreSolution dbManager collection method tables sol inventory
    case scoreE of
        Left err -> pure (Left err)
        Right score -> buildResult unitCfg mFlows mUnits inventory tables stats score
  where
    buildResult unitCfg mFlows mUnits inventory tables stats score = do
        let functionalUnit = Service.functionalUnitOf (dbTechFlows db) mUnits activity
            (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
            contribs = sortOn (\(_, _, c) -> negate (abs c)) rawContribs
            topContribs = take topFlows contribs
            topContributors =
                [ FlowContributionEntry
                    { fcoFlowName = bfName f
                    , fcoContribution = c
                    , fcoSharePct = if score /= 0 then c / score * 100 else 0
                    , fcoFlowId = UUID.toText (bfId f)
                    , fcoCategory = bfCompartmentName f
                    , fcoCompartment = bfCompartmentSub f
                    , fcoCfValue = cfVal
                    , fcoMatchKind = Explain.flowMatchKind tables (bfId f)
                    }
                | (f, cfVal, c) <- topContribs
                ]
        unless (null unknownUuids) $
            reportProgress Warning $
                "[LCIA "
                    <> T.unpack (methodName method)
                    <> "] "
                    <> show (length unknownUuids)
                    <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                    <> show (take 3 unknownUuids)
        pure $
            Right
                LCIAResult
                    { lrMethodId = methodId method
                    , lrMethodName = methodName method
                    , lrCategory = methodCategory method
                    , lrDamageCategory = methodCategory method
                    , lrScore = score
                    , lrUnit = methodUnit method
                    , lrNormalizedScore = Nothing
                    , lrWeightedScore = Nothing
                    , lrMappedFlows = msTotal stats - msUnmatched stats
                    , lrFunctionalUnit = functionalUnit
                    , lrTopContributors = topContributors
                    }

{- | Batch fast path: scores via 'batchedScoresFor', then build LCIAResult
records straight from the precomputed contexts. Skips the per-pid
'mapConcurrently' over methods entirely. When 'topFlows' is 0
(the default) 'lrTopContributors' is left empty and the contribution
walk is skipped; when >0, runs 'inventoryContributions' per method.
-}
buildLCIABatchResultCached ::
    DatabaseManager ->
    Text ->
    DM.CollectionName ->
    Database ->
    ProcessId ->
    Activity ->
    MethodCollection ->
    SharedSolver.CrossDBSolution ->
    [MethodCtx] ->
    Int ->
    IO (Either Text LCIABatchResult)
buildLCIABatchResultCached dbManager dbName collectionName db actPid activity collection sol ctxs topFlows = do
    let damageCats = mcDamageCategories collection
        nwSets = mcNormWeightSets collection
        dcLookup = damageCategoryIndex damageCats
        mNW = case nwSets of (nw : _) -> Just nw; [] -> Nothing
        methods = map mctxMethod ctxs
        inventory = SharedSolver.csInventory sol
    scoreMap <- batchedScoresFor dbManager dbName collectionName db sol methods
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    let unknownUuids =
            [ fid
            | (fid, qty) <- M.toList inventory
            , qty /= 0
            , not (M.member fid mFlows)
            ]
    unless (null unknownUuids) $
        reportProgress Warning $
            "[LCIA batch] pid="
                <> show actPid
                <> ": "
                <> show (length unknownUuids)
                <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                <> show (take 3 unknownUuids)
    mUnitCfg <-
        if topFlows > 0
            then Just <$> getMergedUnitConfig dbManager
            else pure Nothing
    let functionalUnit = Service.functionalUnitOf (dbTechFlows db) mUnits activity
        mkResultIO ctx = do
            let method = mctxMethod ctx
            case resolveBatchedScore method scoreMap of
                Left err -> pure (Left (err <> " (pid=" <> T.pack (show actPid) <> ")"))
                Right score -> Right <$> mkResultForScore ctx method score
        mkResultForScore ctx method score = do
            topContributors <- case mUnitCfg of
                Nothing -> pure []
                Just unitCfg -> do
                    tables <- DM.mapMethodToTablesCached dbManager dbName collectionName db method
                    let (rawContribs, _unknownUuids) =
                            inventoryContributions unitCfg mUnits mFlows inventory tables
                        sorted = sortOn (\(_, _, c) -> negate (abs c)) rawContribs
                        top = take topFlows sorted
                    pure
                        [ FlowContributionEntry
                            { fcoFlowName = bfName f
                            , fcoContribution = c
                            , fcoSharePct = if score /= 0 then c / score * 100 else 0
                            , fcoFlowId = UUID.toText (bfId f)
                            , fcoCategory = bfCompartmentName f
                            , fcoCompartment = bfCompartmentSub f
                            , fcoCfValue = cfVal
                            , fcoMatchKind = Explain.flowMatchKind tables (bfId f)
                            }
                        | (f, cfVal, c) <- top
                        ]
            pure $
                enrichWithNW dcLookup mNW $
                    LCIAResult
                        { lrMethodId = methodId method
                        , lrMethodName = methodName method
                        , lrCategory = methodCategory method
                        , lrDamageCategory = methodCategory method
                        , lrScore = score
                        , lrUnit = methodUnit method
                        , lrNormalizedScore = Nothing
                        , lrWeightedScore = Nothing
                        , lrMappedFlows = mctxMappedFlows ctx
                        , lrFunctionalUnit = functionalUnit
                        , lrTopContributors = topContributors
                        }
    resultsE <- sequence <$> traverse mkResultIO ctxs
    case resultsE of
        Left err -> pure (Left err)
        Right results -> do
            let rawScoreMap = rawScoreMapByName results
            (scoringResults, scoringIndicators) <-
                computeAllScoringSets (mcScoringSets collection) rawScoreMap
            pure (Right (mkLCIABatchResult results mNW nwSets scoringResults (mcScoringSets collection) scoringIndicators (Service.buildCutoffWaste db activity)))

{- | Top-level LCIA batch entry point — AppM-returning. Used by the Servant
routes (via thin where-aliases) and by API.BatchImpacts.
-}
activityLCIABatchH ::
    Text ->
    Text ->
    Text ->
    Maybe SubstitutionRequest ->
    LongTermMode ->
    AppM LCIABatchResult
activityLCIABatchH dbName processIdText collectionNameText mSub ltMode = do
    let collectionName = DM.CollectionName collectionNameText
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    (actProcessId, activity) <- resolveOrThrow db processIdText
    (methods, damageCats, nwSets, scoringSets) <- loadCollection collectionName
    let dcLookup = damageCategoryIndex damageCats
        mNW = case nwSets of (nw : _) -> Just nw; [] -> Nothing
    t0 <- liftIO getCurrentTime
    sol <- crossDBSolutionFor dbName db sharedSolver actProcessId mSub >>= liftIO . applyLongTermToSolution dbManager ltMode
    t1 <- liftIO getCurrentTime
    let inventory = SharedSolver.csInventory sol
        !invSize = M.size inventory
    when (isNothing mSub) $
        liftIO $ do
            reportProgress Info $
                "[LCIA batch] " <> T.unpack collectionNameText <> " for " <> T.unpack (activityName activity)
            reportProgress Info $
                "  Inventory: "
                    <> show invSize
                    <> " flows ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t1 t0) :: Double) ""
                    <> "s)"
            when (invSize == 0) $
                reportProgress Info "  WARNING: inventory is empty — check matrix computation"
            when (invSize > 0 && invSize <= 5) $
                reportProgress Info $
                    "  Inventory UUIDs: " <> intercalate ", " (map UUID.toString $ M.keys inventory)
    scoreMap <- liftIO $ batchedScoresFor dbManager dbName collectionName db sol methods
    rawResultsE <-
        liftIO $
            mapConcurrently
                (\m -> computeCategoryResult dbManager dbName collectionName db sol activity 5 (Just (resolveBatchedScore m scoreMap)) m)
                methods
    rawResults <- either scoringError pure (sequence rawResultsE)
    let results = map (enrichWithNW dcLookup mNW) rawResults
        rawScoreMap = rawScoreMapByName rawResults
    (scoringResults, scoringIndicators) <- liftIO $ computeAllScoringSets scoringSets rawScoreMap
    when (isNothing mSub) $
        liftIO $ do
            t2 <- getCurrentTime
            mapM_ (logBatchCategory invSize) results
            reportProgress Info $
                "  Total: "
                    <> show (length results)
                    <> " categories ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t2 t0) :: Double) ""
                    <> "s)"
            forM_ (M.toList scoringResults) $ \(name, scores) ->
                reportProgress Info $
                    "  Scoring '"
                        <> T.unpack name
                        <> "': "
                        <> intercalate ", " [T.unpack k <> "=" <> showFFloat (Just 6) v "" | (k, v) <- M.toList scores]
    pure (mkLCIABatchResult results mNW nwSets scoringResults scoringSets scoringIndicators (Service.buildCutoffWaste db activity))

{- | Top-level multi-activity batch impacts. One MUMPS multi-RHS solve for all
valid PIDs, parallel characterization. Used by the Servant POST route and
by API.BatchImpacts.
-}
batchImpactsH ::
    Text ->
    Text ->
    Maybe Int ->
    LongTermMode ->
    BatchImpactsRequest ->
    AppM BatchImpactsResponse
batchImpactsH dbName collectionNameText topFlowsParam ltMode req = do
    let collectionName = DM.CollectionName collectionNameText
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    collection <- case M.lookup collectionNameText loadedCollections of
        Just mc -> pure mc
        Nothing ->
            throwError err404{errBody = collectionNotLoadedBody collectionNameText (M.keys loadedCollections)}
    let resolved =
            [ (pidText, Service.resolveScorable db pidText)
            | pidText <- birProcessIds req
            ]
        valid = [(pidText, pidNum, act) | (pidText, Right (pidNum, act)) <- resolved]
        notFound = [pidText | (pidText, Left (Service.ActivityNotFound _)) <- resolved]
        unscorable = [pidText | (pidText, Left (Service.NotScorable _)) <- resolved]
        -- An under-specified id is unusable as sent, like a malformed one, and
        -- belongs in a bucket rather than in neither.
        invalid =
            [ pidText
            | (pidText, Left err) <- resolved
            , case err of
                Service.InvalidProcessId _ -> True
                Service.AmbiguousActivity _ -> True
                Service.InvalidUUID _ -> False
                Service.ActivityNotFound _ -> False
                Service.FlowNotFound _ -> False
                Service.NotScorable _ -> False
                Service.MatrixError _ -> False
            ]
        validPidNums = [pidNum | (_, pidNum, _) <- valid]
    t0 <- liftIO getCurrentTime
    sols0 <- solutionsWithDeps dbName db sharedSolver validPidNums
    sols <- liftIO $ mapM (applyLongTermToSolution dbManager ltMode) sols0
    t1 <- liftIO getCurrentTime
    ctxs <- liftIO $ mapConcurrently (prepMethodCtx dbManager dbName collectionName db) (mcMethods collection)
    let topFlows = max 0 (fromMaybe 0 topFlowsParam)
    let mkEntry ((pidText, pidNum, activity), sol) = do
            impactsE <- buildLCIABatchResultCached dbManager dbName collectionName db pidNum activity collection sol ctxs topFlows
            pure $
                fmap
                    ( \impacts ->
                        BatchImpactsEntry
                            { bieProcessId = pidText
                            , bieActivityName = activityName activity
                            , bieImpacts = impacts
                            }
                    )
                    impactsE
    entriesE <- liftIO $ mapM mkEntry (zip valid sols)
    -- All-or-nothing on purpose: an integrity error is a property of the
    -- (db, method) tables, not of one activity, so every entry would fail
    -- identically. Unresolvable pids stay per-entry (birNotFound/birInvalid).
    entries <- either scoringError pure (sequence entriesE)
    t2 <- liftIO getCurrentTime
    liftIO $
        reportProgress Info $
            "[batch-impacts] "
                <> T.unpack dbName
                <> " / "
                <> T.unpack collectionNameText
                <> ": "
                <> show (length valid)
                <> " activities"
                <> ( if null notFound && null invalid
                        then ""
                        else
                            " ("
                                <> show (length notFound)
                                <> " not_found, "
                                <> show (length invalid)
                                <> " invalid)"
                   )
                <> " — solve "
                <> showFFloat (Just 2) (realToFrac (diffUTCTime t1 t0) :: Double) ""
                <> "s, "
                <> "total "
                <> showFFloat (Just 2) (realToFrac (diffUTCTime t2 t0) :: Double) ""
                <> "s"
    pure
        BatchImpactsResponse
            { birResults = entries
            , birNotFound = notFound
            , birInvalid = invalid
            , birUnscorable = unscorable
            }

{- | Computed quality checks over the whole catalogue: score every entry of a
loaded database against one method collection — chunked multi-RHS solves on
the cached factorization, warm method tables — then judge the numbers with
the pure 'Database.ComputedQuality' checks. The structural quality report
runs on staged data too; this one needs matrices and methods, so it is a
separate report with the same finding shape.
-}
computedQualityReportH :: Text -> Maybe Text -> Maybe Int -> AppM ComputedQualityReportAPI
computedQualityReportH dbName mCollection mLimit = do
    dbManager <- asks aeDbManager
    (db, _solver) <- requireDatabaseByName dbName
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    collection <- case (mCollection, M.keys loadedCollections) of
        (Just c, _) -> pure c -- an unknown name answers 404 in batchImpactsH below
        (Nothing, [only]) -> pure only
        (Nothing, []) ->
            throwError err400{errBody = "No method collection loaded - the computed checks judge scores, so they need one"}
        (Nothing, several) ->
            throwError
                err400
                    { errBody =
                        BSL.fromStrict . T.encodeUtf8 $
                            "Several method collections loaded (" <> T.intercalate ", " several <> ") - pass ?collection= to pick one"
                    }
    let simple = toSimpleDatabase db
        entriesByPid =
            M.fromList
                [ (processRefText (ProcessRef a p), act)
                | ((a, p), act) <- M.toList (sdbActivities simple)
                ]
        refProductName act = case filter exchangeIsReference (exchanges act) of
            [ex] ->
                asum
                    [ tfName <$> M.lookup (exchangeFlowId ex) (sdbTechFlows simple)
                    , wfName <$> M.lookup (exchangeFlowId ex) (sdbWasteFlows simple)
                    ]
            _ -> Nothing
        chunks xs = case splitAt scoringChunk xs of
            (h, []) -> [h | not (null h)]
            (h, t) -> h : chunks t
    responses <-
        mapM
            (\pids -> batchImpactsH dbName collection Nothing IncludeLongTerm BatchImpactsRequest{birProcessIds = pids})
            (chunks (M.keys entriesByPid))
    -- The ids come from the catalogue itself, so nothing should be
    -- unresolvable — but a dropped entry would silently shrink the report,
    -- so any is worth a warning in the log.
    let unresolved = concatMap (\r -> birNotFound r <> birInvalid r <> birUnscorable r) responses
    unless (null unresolved) $
        liftIO . reportProgress Warning $
            "Computed quality report on " <> T.unpack dbName <> ": " <> show (length unresolved) <> " catalogue entries could not be scored and are missing from the report"
    let scored =
            [ CQ.ScoredEntry
                { CQ.seProcessId = bieProcessId e
                , CQ.seActivityName = bieActivityName e
                , CQ.seLocation = activityLocation act
                , CQ.seProductName = refProductName act
                , CQ.seRefUnit = activityUnit act
                , CQ.seScores =
                    [ CQ.CategoryScore (lrMethodName r) (lrUnit r) (lrScore r)
                    | r <- lbrResults (bieImpacts e)
                    ]
                }
            | e <- concatMap birResults responses
            , Just act <- [M.lookup (bieProcessId e) entriesByPid]
            ]
    pure (DBHandlers.computedQualityReportToAPI mLimit (CQ.computedQualityReport dbName collection scored))

{- | The same two reports as a downloadable file. They answer the question the
JSON answers, in the shape the person asking it works in: a spreadsheet, or a
shell. The rendering lives in 'API.Csv', so every client that hands the report
over as a file fetches it rather than reimplementing the columns.

These are a second representation of an operation 'API.Resources' already
names, not operations of their own, so they get no registry entry: one there
would mint a second MCP tool and a second published operation for the same
question.
-}
qualityReportCsvH :: Text -> Maybe Int -> AppM (Headers '[Header "Content-Disposition" Text] QualityReportAPI)
qualityReportCsvH dbName mLimit =
    addHeader (attachment dbName "quality-report") <$> DBHandlers.qualityReportHandler dbName mLimit

computedQualityReportCsvH :: Text -> Maybe Text -> Maybe Int -> AppM (Headers '[Header "Content-Disposition" Text] ComputedQualityReportAPI)
computedQualityReportCsvH dbName mCollection mLimit =
    addHeader (attachment dbName "computed-quality-report") <$> computedQualityReportH dbName mCollection mLimit

{- | @Content-Disposition@ naming the downloaded file after the database it
describes, so two reports don't collide in a download folder. The name keeps
every printable ASCII character but the two a quoted header field cannot
carry, since dropping more would make distinct databases share a filename.
Non-ASCII goes too: a header field is ASCII, and raw UTF-8 there reaches the
client as mojibake or is refused outright. A name left with nothing at all
names the report alone rather than a file opening with a dash.
-}
attachment :: Text -> Text -> Text
attachment dbName report =
    "attachment; filename=\"" <> prefix <> report <> ".csv\""
  where
    prefix = if T.null kept then "" else kept <> "-"
    kept = T.filter headerSafe dbName
    headerSafe c = isAscii c && not (isControl c) && c /= '"' && c /= '\\'

{- | Batch size of the catalogue-wide solve: bounds the dense right-hand-side
block of one multi-RHS solve while every chunk still reuses the one cached
factorization.
-}
scoringChunk :: Int
scoringChunk = 512

-- ---------------------------------------------------------------------------
-- Pure helpers shared by handlers
-- ---------------------------------------------------------------------------

-- | Parse "System=Value[:exact]" into (system, value, isExact).
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

-- | Merge preset-derived and explicit (system, value, exact) classification filters.
mergeClassFilters ::
    [Config.ClassificationPreset] ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Either Text [(Text, Text, Bool)]
mergeClassFilters presets presetParam systems values modes =
    (++ explicit) <$> Config.expandClassificationPreset presets presetParam
  where
    explicit =
        zipWith3
            (\s v m -> (s, v, m == "exact"))
            systems
            values
            (modes ++ repeat "contains")

-- | Build a 'Service.SupplyChainFilter' shared by GET and POST handlers.
buildSupplyChainFilter ::
    [Config.ClassificationPreset] ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Service.Edges ->
    Either Text Service.SupplyChainFilter
buildSupplyChainFilter presets nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam edges = do
    classifications <- mergeClassFilters presets presetParam classSystems classValues classModes
    pure
        Service.SupplyChainFilter
            { Service.scfCore =
                Service.ActivityFilterCore
                    { Service.afcName = nameFilter
                    , Service.afcLocation = locationFilter
                    , Service.afcProduct = productFilter
                    , Service.afcClassifications = classifications
                    , Service.afcLimit = limitParam
                    , Service.afcOffset = offsetParam
                    , Service.afcSort = sortParam
                    , Service.afcOrder = orderParam
                    }
            , Service.scfMaxDepth = maxDepthParam
            , Service.scfMinQuantity = minQuantity
            , Service.scfEdges = edges
            }

buildFlowEntry :: Database -> MethodTables -> UUID -> FlowCFEntry
buildFlowEntry db tables uuid =
    let mFlow = M.lookup uuid (dbBioFlows db)
        -- The entry this flow actually scores with: the read-side lookup, so
        -- a flow reached via a medium-level or CAS-bridge fallback (no
        -- single build-side CF resolved to it) still reports as covered —
        -- exactly what scoring sees. Its provenance names the method line
        -- and strategy even for fallback-covered flows, which the build-side
        -- reverse index this used to read could not.
        mServed = mFlow >>= \f -> lookupEntryForFlow tables uuid (Just f)
        provenance = teProvenance . snd <$> mServed
     in FlowCFEntry
            { fceFlowId = uuid
            , fceFlowName = maybe "" bfName mFlow
            , fceFlowCategory = maybe "" bfCompartmentName mFlow
            , fceCfValue = cfValue . teCF . snd <$> mServed
            , fceCfFlowName = mcfFlowName . bpSource <$> provenance
            , fceMatchStrategy = provenanceStrategyText <$> provenance
            }

matchesQuery :: Maybe Text -> Text -> Text -> Bool
matchesQuery Nothing _ _ = True
matchesQuery (Just q) cfName dbFlowName =
    T.isInfixOf q (T.toLower cfName) || T.isInfixOf q (T.toLower dbFlowName)

cfToAPI :: MethodCF -> MethodFactorAPI
cfToAPI cf =
    MethodFactorAPI
        { mfaFlowRef = mcfFlowRef cf
        , mfaFlowName = mcfFlowName cf
        , mfaDirection = case mcfDirection cf of
            MT.Input -> "Input"
            MT.Output -> "Output"
        , mfaValue = mcfValue cf
        , mfaUnit = mfilter (not . T.null) (Just (mcfUnit cf))
        , mfaCompartment = compartmentPath <$> mcfCompartment cf
        , mfaLocation = mcfConsumerLocation cf
        }

{- | Render a compartment triple as one display path, keeping every non-empty
axis: @"air/urban air"@, @"water/unspecified/long-term"@.
-}
compartmentPath :: MT.Compartment -> Text
compartmentPath (MT.Compartment medium sub qualifier) =
    T.intercalate "/" (filter (not . T.null) [medium, sub, qualifier])

-- ---------------------------------------------------------------------------
-- AppM helpers
-- ---------------------------------------------------------------------------

{- | Lookup a method by UUID across all loaded collections, returning the
collection name it was found in alongside the method. The collection name is
needed to key the per-method CF caches (a UUID alone collides across
collections that share a method name). First-match on ambiguity, mirroring
'API.MCP.resolveMethod'.
-}
loadMethodByUUID :: Text -> AppM (DM.CollectionName, Method)
loadMethodByUUID uuidText = do
    dbManager <- asks aeDbManager
    loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
    case UUID.fromText uuidText of
        Nothing -> throwError err400{errBody = "Invalid method UUID format"}
        Just uuid ->
            case filter (\(_, m) -> methodId m == uuid) loadedMethods of
                ((col, m) : _) -> return (DM.CollectionName col, m)
                [] -> throwError err404{errBody = "Method not found"}

{- | Resolve a method by UUID *within a named collection*. Unlike
'loadMethodByUUID' (first-match across all collections), this guarantees the
method's CFs belong to @collectionName@ — required wherever the result keys a
collection-scoped cache, so a method's factors and its cache slot never disagree.
-}
loadMethodInCollection :: DM.CollectionName -> Text -> AppM Method
loadMethodInCollection collectionName uuidText = do
    (methods, _, _, _) <- loadCollection collectionName
    case UUID.fromText uuidText of
        Nothing -> throwError err400{errBody = "Invalid method UUID format"}
        Just uuid -> case filter ((== uuid) . methodId) methods of
            (m : _) -> pure m
            [] -> throwError err404{errBody = "Method not found in collection"}

-- | Resolve (db, solver, ProcessId, Activity, Method) within @collectionName@ and dispatch.
withActivityAndMethod ::
    Text ->
    DM.CollectionName ->
    Text ->
    Text ->
    (Database -> SharedSolver -> ProcessId -> Activity -> Method -> AppM a) ->
    AppM a
withActivityAndMethod dbName collectionName processIdText methodIdText k = do
    (db, sharedSolver) <- requireDatabaseByName dbName
    method <- loadMethodInCollection collectionName methodIdText
    case Service.resolveScorable db processIdText of
        Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
        Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
        Left (Service.AmbiguousActivity msg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Left err -> throwError (serviceErrorToServerError err)
        Right (actProcessId, activity) -> k db sharedSolver actProcessId activity method

-- ---------------------------------------------------------------------------
-- Servant handlers (top-level AppM actions)
-- ---------------------------------------------------------------------------

getOpenApiSpec :: AppM Value
getOpenApiSpec = return $ toJSON volcaOpenApi

{- | Wire-format revision advertised on /api/v1/version. BUMP this whenever a
breaking change to the JSON wire shape lands (field rename/removal, type
narrowing, newly-required field), or whenever a new route or capability
appears that a client must know about /before/ calling it. Adding a route
does not exempt a change from the bump: an absent route answers 404, and so
does a request naming a database the engine has not loaded, so a client
cannot tell "this engine is too old" from "you asked for the wrong thing"
(revision 23: the @supplierClaim@ an exchange carries, saying how its source
designates its supplier - by the product row, by an activity identifier, by an
activity name, or by a dataset number - and the @supplierAmbiguities@ a
database setup report carries, the inputs several activities of one dependency
answered equally well;
revision 22: the @nativeId@ an activity carries, the identifier its source
file gave the dataset block it was read from;
revision 21: the @block@ an activity summary carries, and the
@blockProducts@ beside it, so a listing can gather the coproducts of one
source block and say when it is showing only part of one;
revision 20: the @derive@ route, which reads a source again under another
allocation key, and the @allocation@ and @source@ a database status carries,
without which a column of shares cannot say whether the source stated them;
revision 19: the @kind@ parameter of a flow search naming several kinds,
comma-separated, so the biosphere-and-waste bucket the search counts report is
listable in one call;
revision 18: the @properties@ a technosphere exchange carries, the dry and
wet mass its source states of that line;
revision 17: the @search-counts@ route and its @count_search_matches@ tool,
answering how many processes, products and flows one query matches;
revision 16: the @producerCount@ a flow search result carries, and the
@role@ parameter that asks a flow's activities for one side of it only;
revision 15: the @massAllocationPercent@ a product of a multi-output block carries
beside its declared share, saying what the mass would allocate;
revision 14: the @AvoidedProduct@ role a technosphere exchange can report,
which a client decoding the role enumeration has to know, the @unallocated@
check of the quality report, and the @share@ and @classification@ fields a
technosphere exchange carries;
revision 13: the @compartment@ an unmapped factor of the mapping report
carries; revision 12: the @MissingNode@ a tree export reports where a link names a
row no loaded database holds, and the @missing:@ prefixed id such a node and
its edge carry in place of a process id;
revision 11: the @dataVersion@ the version route reports; revision 10: the
@wasteRole@ an activity's waste lines report;
revision 9: the @kind@ a flow search reports and filters on;
revision 8: the two quality reports as downloadable CSV;
revision 7: editing the exchanges of an activity the database already holds;
revision 6: the explain-cf route, and the @match_kind@ field flow
contributions gained alongside it; revision 5: writing activities, and the
@transient@ / @warnings@ fields the
delete response gained alongside it; revision 4: the quality-report,
computed-quality-report and characterization-coverage routes; revision 3: the
delete @ids@ selection, which an older engine would ignore and fall back to
the whole filtered set).
Clients compare it to decide compatibility and to gate such capabilities.
-}
currentWireVersion :: Int
currentWireVersion = 23

getVersion :: AppM Value
getVersion = do
    dataVersion <- asks aeDataVersion
    return $
        object
            [ "version" .= Version.version
            , "gitHash" .= Version.gitHash
            , "gitTag" .= Version.gitTag
            , "buildTarget" .= Version.buildTarget
            , "wireVersion" .= currentWireVersion
            , "dataVersion" .= fmap Config.unDataVersion dataVersion
            ]

getHosting :: AppM Value
getHosting = asks (toJSON . hostingInfo . aeHostingConfig)

{- | The wire answer for a hosting config; no section means an unmanaged,
unrestricted instance (local, CLI, desktop).
-}
hostingInfo :: Maybe Config.HostingConfig -> HostingInfo
hostingInfo hostingConfig = case hostingConfig of
    Just hc ->
        HostingInfo
            { hiIsHosted = True
            , hiMaxUploads = Config.hcMaxUploads hc
            , hiMaxUploadMb = Config.hcMaxUploadMb hc
            , hiMaxLoadedUploads = Config.hcMaxLoadedUploads hc
            , hiApiAccess = Config.hcApiAccess hc
            , -- The sentence a refusal will actually carry (operator's words or
              -- the default), not the raw config value: a client explaining the
              -- situation up front must show what a refusal would have said.
              hiReadOnly = Config.hcReadOnly hc
            , hiReadOnlyMessage = if Config.hcReadOnly hc then Config.readOnlyRefusalFor (Just hc) else ""
            , hiUpgradeUpload = Config.hcUpgradeUpload hc
            , hiUpgradeApi = Config.hcUpgradeApi hc
            , hiUpgradeVmSize = Config.hcUpgradeVmSize hc
            }
    Nothing ->
        HostingInfo
            { hiIsHosted = False
            , hiMaxUploads = -1
            , hiMaxUploadMb = -1
            , hiMaxLoadedUploads = -1
            , hiApiAccess = True
            , hiReadOnly = False
            , hiReadOnlyMessage = ""
            , hiUpgradeUpload = ""
            , hiUpgradeApi = ""
            , hiUpgradeVmSize = ""
            }

getStats :: AppM Value
getStats = liftIO $ do
    enabled <- GHC.Stats.getRTSStatsEnabled
    if enabled
        then do
            stats <- GHC.Stats.getRTSStats
            return $
                object
                    [ "memory_used_bytes" .= GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc stats)
                    , "memory_allocated_bytes" .= GHC.Stats.allocated_bytes stats
                    , "gc_count" .= GHC.Stats.gcs stats
                    ]
        else
            return $
                object
                    ["error" .= ("RTS stats not enabled. Run with +RTS -T to enable." :: Text)]

getClassificationPresets :: AppM [ClassificationPresetInfo]
getClassificationPresets = do
    presets <- asks aeClassificationPresets
    return $ map toInfo presets
  where
    toInfo p =
        ClassificationPresetInfo
            { cpiName = Config.cpName p
            , cpiLabel = Config.cpLabel p
            , cpiDescription = Config.cpDescription p
            , cpiFilters = map (\e -> ClassificationEntryInfo (Config.ceSystem e) (Config.ceValue e) (Config.ceMode e)) (Config.cpFilters p)
            }

getLogsHandler :: Maybe Int -> AppM Value
getLogsHandler sinceMaybe = do
    let since = fromMaybe 0 sinceMaybe
    (nextIndex, logLines) <- liftIO $ getLogLines since
    return $
        object
            [ "lines" .= logLines
            , "nextIndex" .= nextIndex
            ]

postAuth :: LoginRequest -> AppM (Headers '[Header "Set-Cookie" String] Value)
postAuth loginReq = do
    password <- asks aePassword
    case password of
        Nothing ->
            return $ noHeader $ object ["ok" .= True]
        Just pwd ->
            if T.unpack (lrCode loginReq) == pwd
                then
                    let cookieValue = "volca_session=" ++ pwd ++ "; Path=/; HttpOnly; SameSite=Strict"
                     in return $ addHeader cookieValue $ object ["ok" .= True]
                else
                    throwError err401{errBody = "{\"error\":\"invalid code\"}"}

getActivityInfo :: Text -> Text -> AppM ActivityInfo
getActivityInfo dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    result <- either throwServiceError pure (Service.getActivityInfo db processId)
    case fromJSON result of
        Success activityInfo -> return activityInfo
        Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

getActivityFlows :: Text -> Text -> AppM [FlowSummary]
getActivityFlows dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityFlowSummaries db activity

getActivityInputs :: Text -> Text -> AppM [ExchangeDetail]
getActivityInputs dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityInputDetails db activity

getActivityOutputs :: Text -> Text -> AppM [ExchangeDetail]
getActivityOutputs dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityOutputDetails db activity

getActivityReferenceProduct :: Text -> Text -> AppM FlowDetail
getActivityReferenceProduct dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        case Service.getActivityReferenceProductDetail db activity of
            Nothing -> throwError err404{errBody = "No reference product found"}
            Just refProduct -> return refProduct

getActivityTree :: Text -> Text -> AppM TreeExport
getActivityTree dbName processId = do
    dbManager <- asks aeDbManager
    maxTreeDepth <- asks aeMaxTreeDepth
    (db, _) <- requireDatabaseByName dbName
    root <- either throwServiceError pure (Service.resolveActivityAndProcessId db processId)
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    return $ Service.convertToTreeExport db maxTreeDepth (buildLoopAwareTree unitCfg db maxTreeDepth root)

{- | Inventory with optional substitutions; goes through the cross-DB
back-substitution path so dep-DB inventories merge into the response.
-}
activityInventoryCore :: Text -> Text -> Maybe SubstitutionRequest -> AppM InventoryExport
activityInventoryCore dbName processIdText mSub = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    (processId, activity) <- resolveOrThrow db processIdText
    sol <- crossDBSolutionFor dbName db sharedSolver processId mSub
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    pure $ Service.convertToInventoryExport db mFlows mUnits processId activity (SharedSolver.csInventory sol)

getActivityInventory :: Text -> Text -> AppM InventoryExport
getActivityInventory dbName processIdText = activityInventoryCore dbName processIdText Nothing

getActivityGraph :: Text -> Text -> Maybe Double -> AppM GraphExport
getActivityGraph dbName processId maybeCutoff = do
    (db, sharedSolver) <- requireDatabaseByName dbName
    let cutoffPercent = fromMaybe 1.0 maybeCutoff
    result <- liftIO $ Service.buildActivityGraph db sharedSolver processId cutoffPercent
    either throwServiceError pure result

{- | Supply-chain core (scaling-vector based). 'Nothing' takes the cached
solve; 'Just' applies substitutions via the cross-DB resolver.
-}
activitySupplyChainCore ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe SubstitutionRequest ->
    AppM SupplyChainResponse
activitySupplyChainCore dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam mSub = do
    dbManager <- asks aeDbManager
    presets <- asks aeClassificationPresets
    (db, sharedSolver) <- requireDatabaseByName dbName
    let edges = if fromMaybe False includeEdgesParam then Service.WithEdges else Service.EntriesOnly
    scf <-
        either badRequest pure $
            buildSupplyChainFilter
                presets
                nameFilter
                limitParam
                minQuantity
                offsetParam
                maxDepthParam
                locationFilter
                productFilter
                presetParam
                classSystems
                classValues
                classModes
                sortParam
                orderParam
                edges
    case mSub of
        Nothing -> do
            unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
            result <- liftIO $ Service.getSupplyChain unitCfg (DM.managerGeographies dbManager) (DM.mkDepSolverLookup dbManager) db dbName sharedSolver processIdText scf
            either throwServiceError pure result
        Just subReq -> do
            unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
            (processId, _) <- resolveOrThrow db processIdText
            scalingResult <-
                liftIO $
                    Service.computeScalingVectorWithSubstitutionsCrossDB
                        unitCfg
                        (DM.mkDepSolverLookup dbManager)
                        db
                        dbName
                        sharedSolver
                        processId
                        (srSubstitutions subReq)
            case scalingResult of
                Left err -> throwServiceError err
                Right (scalingVec, virtualLinks) -> do
                    eResp <-
                        liftIO $
                            Service.buildSupplyChainFromScalingVectorCrossDB
                                unitCfg
                                (DM.managerGeographies dbManager)
                                (DM.mkDepSolverLookup dbManager)
                                db
                                dbName
                                processId
                                scalingVec
                                virtualLinks
                                scf
                    either throwServiceError pure eResp

getActivitySupplyChain ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    AppM SupplyChainResponse
getActivitySupplyChain dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam =
    activitySupplyChainCore dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam Nothing

{- | Aggregate endpoint with accumulating field-level validation (a single
request can report invalid `scope` and invalid `aggregate` together).
-}
getActivityAggregate ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe Text ->
    Maybe Text ->
    AppM Aggregation
getActivityAggregate dbName processId scopeParam isInputParam maxDepthParam fnameParam fnameNotParam funitParam presetParam fclassParams ftargetParam fconsumerParam fconsumerNotParam fexchangeTypeParam freferenceParam groupByParam aggregateParam = do
    dbManager <- asks aeDbManager
    presets <- asks aeClassificationPresets
    (db, sharedSolver) <- requireDatabaseByName dbName
    let parseScope = \case
            Just "direct" -> V.Success Agg.ScopeDirect
            Just "supply_chain" -> V.Success Agg.ScopeSupplyChain
            Just "biosphere" -> V.Success Agg.ScopeBiosphere
            Just "consumption" -> V.Success Agg.ScopeConsumption
            _ -> V.failure "scope must be one of: direct | supply_chain | biosphere | consumption"
        parseExType = \case
            Nothing -> V.Success Nothing
            Just raw -> case parseExchangeKind raw of
                Just k -> V.Success (Just k)
                Nothing -> V.failure ("filter_exchange_type must be one of: " <> exchangeKindChoices)
        parseAgg = \case
            Nothing -> V.Success Agg.AggSum
            Just "sum_quantity" -> V.Success Agg.AggSum
            Just "count" -> V.Success Agg.AggCount
            Just "share" -> V.Success Agg.AggShare
            Just other -> V.failure ("aggregate must be one of: sum_quantity | count | share (got " <> other <> ")")
    (scope, exchangeType, aggFn) <-
        case V.toEither $ (,,) <$> parseScope scopeParam <*> parseExType fexchangeTypeParam <*> parseAgg aggregateParam of
            Left errs -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 (T.intercalate "; " (NE.toList errs)))}
            Right v -> pure v
    case Agg.exchangeTypeScopeError scope exchangeType of
        Just msg -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 msg)}
        Nothing -> return ()
    presetFilters <- either badRequest pure (Config.expandClassificationPreset presets presetParam)
    let explicitFilters = mapMaybe parseClassFilter fclassParams
        params =
            Agg.AggregateParams
                { Agg.apScope = scope
                , Agg.apIsInput = isInputParam
                , Agg.apMaxDepth = maxDepthParam
                , Agg.apFilterName = fnameParam
                , Agg.apFilterNameNot = maybe [] (map T.strip . T.splitOn ",") fnameNotParam
                , Agg.apFilterUnit = funitParam
                , Agg.apFilterClassifications = presetFilters ++ explicitFilters
                , Agg.apFilterTargetName = ftargetParam
                , Agg.apFilterConsumer = fconsumerParam
                , Agg.apFilterConsumerNot = maybe [] (map T.strip . T.splitOn ",") fconsumerNotParam
                , Agg.apFilterExchangeType = exchangeType
                , Agg.apFilterIsReference = freferenceParam
                , Agg.apGroupBy = groupByParam
                , Agg.apAggregate = aggFn
                }
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    result <- liftIO $ Agg.aggregate unitCfg (DM.managerGeographies dbManager) mFlows mUnits db dbName sharedSolver (DM.mkDepSolverLookup dbManager) processId params
    either throwServiceError pure result

{- | LCIA single-method core. GET passes a top-flows param and logs;
POST carries substitutions instead and skips logging.
-}
activityLCIACore :: Text -> Text -> Text -> Text -> Maybe Int -> Maybe SubstitutionRequest -> AppM LCIAResult
activityLCIACore dbName processIdText collectionNameText methodIdText topFlowsParam mSub = do
    let collectionName = DM.CollectionName collectionNameText
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    method <- loadMethodInCollection collectionName methodIdText
    (processId, activity) <- resolveOrThrow db processIdText
    sol <- crossDBSolutionFor dbName db sharedSolver processId mSub
    resultE <- liftIO $ computeCategoryResult dbManager dbName collectionName db sol activity (fromMaybe 5 topFlowsParam) Nothing method
    result <- either scoringError pure resultE
    when (isNothing mSub) $ liftIO $ logLCIAResult result method
    pure result

getActivityLCIA :: Text -> Text -> Text -> Text -> Maybe Int -> AppM LCIAResult
getActivityLCIA dbName processIdText collectionName methodIdText topFlowsParam =
    activityLCIACore dbName processIdText collectionName methodIdText topFlowsParam Nothing

postActivityLCIA :: Text -> Text -> Text -> Text -> SubstitutionRequest -> AppM LCIAResult
postActivityLCIA dbName processIdText collectionName methodIdText subReq =
    activityLCIACore dbName processIdText collectionName methodIdText Nothing (Just subReq)

{- | Sensitivity sweep: rank-1 perturbations on the root scaling, scored
through the cross-DB graph (regional CFs on dep DBs still apply).
-}
postActivitySensitivity :: Text -> Text -> Text -> Text -> SensitivityRequest -> AppM SensitivityResponse
postActivitySensitivity dbName processIdText collectionNameText methodIdText senReq = do
    let collectionName = DM.CollectionName collectionNameText
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    requireFullyLinked dbName db
    method <- loadMethodInCollection collectionName methodIdText
    (processId, activity) <- resolveOrThrow db processIdText
    eRes <- liftIO $ Service.computeSensitivities db sharedSolver processId (srPerturbations senReq)
    (baselineX, perResults) <- either throwServiceError pure eRes
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    let depLookup = DM.mkDepSolverLookup dbManager
        scaleToSolution x = do
            eSol <-
                SharedSolver.goWithDepsFromScalings
                    unitCfg
                    depLookup
                    db
                    dbName
                    []
                    [x]
                    0
            pure $ case eSol of
                Left err -> Left err
                Right (sol : _) -> Right sol
                Right [] -> Left "cross-DB propagation returned empty result"
        buildEntry baselineLcia (p, eitherX) = case eitherX of
            Left err -> pure (PerturbedEntry p (Left err))
            Right x' -> do
                eSol <- scaleToSolution x'
                case eSol of
                    Left err -> pure (PerturbedEntry p (Left err))
                    Right sol -> do
                        eLcia <- computeCategoryResult dbManager dbName collectionName db sol activity 5 Nothing method
                        pure $ case eLcia of
                            Left err -> PerturbedEntry p (Left err)
                            Right lcia -> PerturbedEntry p (Right (lcia, lrScore lcia - lrScore baselineLcia))
    eBaselineSol <- liftIO $ scaleToSolution baselineX
    baselineSol <-
        either
            (\err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err})
            pure
            eBaselineSol
    eBaselineLcia <-
        liftIO $
            computeCategoryResult dbManager dbName collectionName db baselineSol activity 5 Nothing method
    baselineLcia <- either scoringError pure eBaselineLcia
    perturbed <-
        liftIO $
            mapConcurrently (buildEntry baselineLcia) perResults
    pure SensitivityResponse{srBaseline = baselineLcia, srPerturbed = perturbed}

getActivityLCIABatch :: Text -> Text -> Text -> Maybe Bool -> AppM LCIABatchResult
getActivityLCIABatch dbName processIdText collectionName mExcludeLT =
    activityLCIABatchH dbName processIdText collectionName Nothing (longTermModeFromExclude (fromMaybe False mExcludeLT))

postActivityLCIABatch :: Text -> Text -> Text -> Maybe Bool -> SubstitutionRequest -> AppM LCIABatchResult
postActivityLCIABatch dbName processIdText collectionName mExcludeLT subReq =
    activityLCIABatchH dbName processIdText collectionName (Just subReq) (longTermModeFromExclude (fromMaybe False mExcludeLT))

postActivityInventory :: Text -> Text -> SubstitutionRequest -> AppM InventoryExport
postActivityInventory dbName processIdText subReq = activityInventoryCore dbName processIdText (Just subReq)

postActivitySupplyChain ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    SubstitutionRequest ->
    AppM SupplyChainResponse
postActivitySupplyChain dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam subReq =
    activitySupplyChainCore dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam (Just subReq)

getActivityConsumers ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Int ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    AppM ConsumersResponse
getActivityConsumers dbName processIdText nameFilter locationFilter productFilter presetParam classSystems classValues classModes limitParam offsetParam maxDepthParam sortParam orderParam includeEdgesParam = do
    presets <- asks aeClassificationPresets
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    classifications <- either badRequest pure (mergeClassFilters presets presetParam classSystems classValues classModes)
    let cnf =
            Service.ConsumerFilter
                { Service.cnfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = nameFilter
                        , Service.afcLocation = locationFilter
                        , Service.afcProduct = productFilter
                        , Service.afcClassifications = classifications
                        , Service.afcLimit = limitParam
                        , Service.afcOffset = offsetParam
                        , Service.afcSort = sortParam
                        , Service.afcOrder = orderParam
                        }
                , Service.cnfMaxDepth = maxDepthParam
                , Service.cnfEdges = if fromMaybe False includeEdgesParam then Service.WithEdges else Service.EntriesOnly
                }
    either throwServiceError pure (Service.getConsumers (DM.managerGeographies dbManager) db dbName processIdText cnf)

getActivityPathTo :: Text -> Text -> Maybe Text -> AppM Value
getActivityPathTo dbName processIdText targetParam = do
    (db, solver) <- requireDatabaseByName dbName
    target <-
        maybe
            (throwError err400{errBody = "Missing required 'target' query parameter"})
            pure
            targetParam
    result <- liftIO $ Service.getPathTo db solver processIdText (Service.NamePattern target)
    case result of
        Left (Service.ActivityNotFound msg) ->
            throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Left (Service.InvalidProcessId msg) ->
            throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Left (Service.AmbiguousActivity msg) ->
            throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Left err ->
            throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
        Right val -> return val

getContributingFlows :: Text -> Text -> Text -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingFlowsResult
getContributingFlows dbName processIdText collectionNameText methodIdText limitParam mExcludeLT =
    let collectionName = DM.CollectionName collectionNameText
     in withActivityAndMethod dbName collectionName processIdText methodIdText $ \db sharedSolver actProcessId _ method -> do
            dbManager <- asks aeDbManager
            let lim = fromMaybe 20 limitParam
                ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
            unitCfg <- liftIO $ getMergedUnitConfig dbManager
            (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
            inventory <- applyLongTermMode mFlows ltMode <$> inventoryWithDeps dbName db sharedSolver actProcessId
            tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
            let score = loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables)
                (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
                contribs = sortOn (\(_, _, c) -> negate (abs c)) rawContribs
                topFlows =
                    [ FlowContributionEntry
                        { fcoFlowName = bfName f
                        , fcoContribution = c
                        , fcoSharePct = if score /= 0 then c / score * 100 else 0
                        , fcoFlowId = UUID.toText (bfId f)
                        , fcoCategory = bfCompartmentName f
                        , fcoCompartment = bfCompartmentSub f
                        , fcoCfValue = cfVal
                        , fcoMatchKind = Explain.flowMatchKind tables (bfId f)
                        }
                    | (f, cfVal, c) <- take lim contribs
                    ]
            liftIO $
                unless (null unknownUuids) $
                    reportProgress Warning $
                        "[contributing-flows "
                            <> T.unpack (methodName method)
                            <> "] "
                            <> show (length unknownUuids)
                            <> " inventory flow UUID(s) absent from merged FlowDB. Samples: "
                            <> show (take 3 unknownUuids)
            return
                ContributingFlowsResult
                    { cfrMethod = methodName method
                    , cfrUnit = methodUnit method
                    , cfrTotalScore = score
                    , cfrTopFlows = topFlows
                    }

getContributingActivities :: Text -> Text -> Text -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingActivitiesResult
getContributingActivities dbName processIdText collectionNameText methodIdText limitParam mExcludeLT =
    let collectionName = DM.CollectionName collectionNameText
     in withActivityAndMethod dbName collectionName processIdText methodIdText $ \db sharedSolver actProcessId _ method -> do
            dbManager <- asks aeDbManager
            let lim = fromMaybe 10 limitParam
                ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
            requireFullyLinked dbName db
            unitCfg <- liftIO $ getMergedUnitConfig dbManager
            (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
            tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
            eContribs <-
                liftIO $
                    SharedSolver.crossDBProcessContributions
                        unitCfg
                        mUnits
                        mFlows
                        (DM.mkDepSolverLookup dbManager)
                        db
                        dbName
                        sharedSolver
                        actProcessId
                        tables
                        ltMode
            case eContribs of
                Left err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err}
                Right contributions -> do
                    let score = sum (M.elems contributions)
                        sorted = sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
                        top = take lim sorted
                    rows <- liftIO $ mapM (mkCrossDBContrib dbManager dbName mFlows mUnits score) top
                    return
                        ContributingActivitiesResult
                            { carMethod = methodName method
                            , carUnit = methodUnit method
                            , carTotalScore = score
                            , carActivities = rows
                            }

getFlowDetail :: Text -> Text -> AppM FlowDetail
getFlowDetail dbName flowIdText = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedFlow db flowIdText $ \flow -> do
        let fid = flowKindId flow
            unitName' = flowKindUnitName (dbUnits db) flow
            usageCount = Service.getFlowUsageCount db fid
        return $ FlowDetail (apiFlowOfKind flow) unitName' usageCount

{- | The activities on one side of a flow. @role@ picks the side; an
unrecognised value is refused rather than answered with the other question.
-}
getFlowActivities :: Text -> Text -> Maybe Text -> AppM [ActivitySummary]
getFlowActivities dbName flowIdText mRole = do
    (db, _) <- requireDatabaseByName dbName
    side <- maybe (badRequest unknownRole) pure (parseProducerFilter mRole)
    withValidatedFlow db flowIdText $ \flow ->
        return $ Service.getActivitiesUsingFlow db side (flowKindId flow)
  where
    unknownRole :: Text
    unknownRole = "unknown role: use producer, consumer or any"

getMethods :: AppM [MethodSummary]
getMethods = do
    dbManager <- asks aeDbManager
    loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
    return
        [ MethodSummary
            { msmId = methodId m
            , msmName = methodName m
            , msmCategory = methodCategory m
            , msmUnit = methodUnit m
            , msmFactorCount = length (methodFactors m)
            , msmCollection = collName
            }
        | (collName, m) <- loadedMethods
        ]

getMethodDetail :: Text -> AppM MethodDetail
getMethodDetail methodIdText = do
    (_, method) <- loadMethodByUUID methodIdText
    return $
        MethodDetail
            { mdId = methodId method
            , mdName = methodName method
            , mdDescription = methodDescription method
            , mdUnit = methodUnit method
            , mdCategory = methodCategory method
            , mdMethodology = methodMethodology method
            , mdFactorCount = length (methodFactors method)
            }

getMethodFactors :: Text -> AppM [MethodFactorAPI]
getMethodFactors methodIdText = do
    (_, method) <- loadMethodByUUID methodIdText
    return $ map cfToAPI (methodFactors method)

getMethodMapping :: Text -> Text -> AppM MappingStatus
getMethodMapping dbName methodIdText = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName collectionName db method
    tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
    let stats = computeMappingStats mappings
        totalFactors = length mappings
        coverage =
            if totalFactors > 0
                then fromIntegral (totalFactors - msUnmatched stats) / fromIntegral totalFactors * 100
                else 0.0
        unmappedFlows =
            take
                50
                [ UnmappedFlowAPI
                    { ufaFlowRef = mcfFlowRef cf
                    , ufaFlowName = mcfFlowName cf
                    , ufaDirection = case mcfDirection cf of
                        MT.Input -> "Input"
                        MT.Output -> "Output"
                    , ufaCompartment = compartmentPath <$> mcfCompartment cf
                    }
                | (cf, Nothing) <- mappings
                ]
        -- Counted from the read-side tables, not the build-side mappings: a
        -- factor resolves to at most one flow there, so counting resolved
        -- flows would miss every flow reached through a fallback and report a
        -- fraction of the method's real reach.
        uniqueDbFlows = S.size (characterizedFlowIds tables (dbBioFlows db))
    return
        MappingStatus
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

getFlowCFMapping :: Text -> Text -> AppM FlowCFMapping
getFlowCFMapping dbName methodIdText = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
    let entries = map (buildFlowEntry db tables) (V.toList (dbBiosphereOrder db))
        matchedCount = length [() | e <- entries, isJust (fceCfValue e)]
    return
        FlowCFMapping
            { fcmMethodName = methodName method
            , fcmMethodUnit = methodUnit method
            , fcmTotalFlows = fromIntegral (dbBiosphereCount db)
            , fcmMatchedFlows = matchedCount
            , fcmFlows = entries
            }

{- | Coverage of one database by a whole method collection, as distinct flows.
Distinct across methods, because they overlap — every climate-change variant
characterizes the same gases — so this number cannot be recovered from the
per-method mapping statuses.
-}
getCollectionCoverage :: Text -> Text -> AppM CollectionCoverage
getCollectionCoverage dbName collectionNameText = do
    let collectionName = DM.CollectionName collectionNameText
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (methods, _, _, _) <- loadCollection collectionName
    tablesList <- liftIO $ mapM (DM.mapMethodToTablesCached dbManager dbName collectionName db) methods
    return
        CollectionCoverage
            { ccvCollection = collectionNameText
            , ccvDbName = dbName
            , ccvTotalFlows = fromIntegral (dbBiosphereCount db)
            , ccvCharacterizedFlows = S.size (S.unions (map (`characterizedFlowIds` dbBioFlows db) tablesList))
            }

{- | Why one flow scores with the factor it does.

The cascade is pure, so this replays it for the one flow asked about rather
than reading anything scoring had to carry. The response is assembled by
'explainCFToAPI', which the MCP tool serves too, so the two surfaces cannot
tell different stories about the same flow.
-}
explainCFHandler :: Text -> Text -> Text -> AppM ExplainCFResult
explainCFHandler dbName methodIdText flowIdText = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    fid <- case UUID.fromText (T.strip flowIdText) of
        Nothing -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 ("Malformed flow id: " <> flowIdText))}
        Just u -> pure u
    explained <- liftIO $ DM.explainFlowFactor dbManager dbName collectionName db method fid
    case explained of
        Left err -> throwError err404{errBody = BSL.fromStrict (T.encodeUtf8 err)}
        Right (flow, explanation) ->
            pure (explainCFToAPI db method flow explanation)

getCharacterization :: Text -> Text -> Maybe Text -> Maybe Int -> AppM CharacterizationResult
getCharacterization dbName methodIdText flowFilter limitParam = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    let lim = fromMaybe 50 limitParam
        queryLower = fmap T.toLower flowFilter
    mappings <- liftIO $ DM.effectiveMethodMappings dbManager dbName collectionName db method
    let matched =
            [ (cf, f, strat)
            | (cf, Just (f, strat)) <- mappings
            , matchesQuery queryLower (mcfFlowName cf) (bfName f)
            ]
        sorted = sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
        top = take lim sorted
        mkEntry (cf, f, strat) =
            CharacterizationEntry
                { cheMethodFlowName = mcfFlowName cf
                , cheCfValue = mcfValue cf
                , cheCfUnit = mcfUnit cf
                , cheDirection = case mcfDirection cf of
                    MT.Input -> "Input"
                    MT.Output -> "Output"
                , cheDbFlowName = bfName f
                , cheFlowId = UUID.toText (bfId f)
                , cheFlowUnit = getUnitNameForBioFlow (dbUnits db) f
                , cheCategory = bfCompartmentName f
                , cheCompartment = bfCompartmentSub f
                , cheMatchStrategy = strategyToText strat
                }
    return
        CharacterizationResult
            { chrMethod = methodName method
            , chrUnit = methodUnit method
            , chrMatches = length matched
            , chrShown = length top
            , chrFactors = map mkEntry top
            }

getMethodCollections :: AppM MethodCollectionListResponse
getMethodCollections = do
    dbManager <- asks aeDbManager
    statuses <- liftIO $ DM.listMethodCollections dbManager
    return $
        MethodCollectionListResponse
            [ MethodCollectionStatusAPI
                { mcaName = mcsName s
                , mcaDisplayName = mcsDisplayName s
                , mcaDescription = mcsDescription s
                , mcaStatus = case mcsStatus s of
                    DM.Loaded -> "loaded"
                    _ -> "unloaded"
                , mcaIsUploaded = mcsIsUploaded s
                , mcaPath = mcsPath s
                , mcaMethodCount = mcsMethodCount s
                , mcaFormat = Just (mcsFormat s)
                }
            | s <- statuses
            ]

loadMethodCollectionHandler :: Text -> AppM ActivateResponse
loadMethodCollectionHandler name = do
    dbManager <- asks aeDbManager
    simpleAction (DM.loadMethodCollection dbManager name) ("Loaded method: " <> name)

unloadMethodCollectionHandler :: Text -> AppM ActivateResponse
unloadMethodCollectionHandler name = do
    dbManager <- asks aeDbManager
    simpleAction (DM.unloadMethodCollection dbManager name) ("Unloaded method: " <> name)

searchFlows :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> AppM (SearchResults FlowSearchResult)
searchFlows dbName queryParam langParam kindParam limitParam offsetParam sortParam orderParam = do
    (db, _) <- requireDatabaseByName dbName
    kinds <- maybe (pure AnyKind) namedKinds kindParam
    case queryParam of
        Nothing -> return (SearchResults [] 0 0 50 False 0.0)
        Just query -> do
            let ff =
                    Service.FlowFilter
                        { Service.ffQuery = query
                        , Service.ffLang = langParam
                        , Service.ffKind = kinds
                        , Service.ffLimit = limitParam
                        , Service.ffOffset = offsetParam
                        , Service.ffSort = sortParam
                        , Service.ffOrder = orderParam
                        }
            searchFlowsInternal db ff
  where
    -- An absent parameter is the only reading that means every kind; a
    -- present one naming nothing is refused, see 'parseKindNames'.
    namedKinds :: Text -> AppM KindFilter
    namedKinds raw = either badRequest (pure . OnlyKinds) (parseKindNames raw)

searchActivitiesWithCount :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> AppM (SearchResults ActivitySummary)
searchActivitiesWithCount dbName nameParam geoParam productParam exactParam presetParam classSystems classValues classModes limitParam offsetParam sortParam orderParam = do
    presets <- asks aeClassificationPresets
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    classifications <- either badRequest pure (mergeClassFilters presets presetParam classSystems classValues classModes)
    let exactMatch = fromMaybe False exactParam
        sf =
            Service.SearchFilter
                { Service.sfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = nameParam
                        , Service.afcLocation = geoParam
                        , Service.afcProduct = productParam
                        , Service.afcClassifications = classifications
                        , Service.afcLimit = limitParam
                        , Service.afcOffset = offsetParam
                        , Service.afcSort = sortParam
                        , Service.afcOrder = orderParam
                        }
                , Service.sfExactMatch = exactMatch
                }
    result <- liftIO $ Service.searchActivities (DM.managerGeographies dbManager) db sf
    case result of
        Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
        Right jsonValue -> case fromJSON jsonValue of
            Success searchResults -> return searchResults
            Error parseErr -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack parseErr}

getClassifications :: Text -> AppM [ClassificationSystem]
getClassifications dbName = do
    (db, _) <- requireDatabaseByName dbName
    return $ Service.getClassifications db

postImpactsBatch :: Text -> Text -> Maybe Int -> Maybe Bool -> BatchImpactsRequest -> AppM BatchImpactsResponse
postImpactsBatch dbName collectionName topFlowsParam mExcludeLT =
    batchImpactsH dbName collectionName topFlowsParam (longTermModeFromExclude (fromMaybe False mExcludeLT))

-- ---------------------------------------------------------------------------
-- Servant server
-- ---------------------------------------------------------------------------

lcaServer :: AppEnv -> Server LCAAPI
lcaServer env = hoistServer lcaAPI (runApp env) handlers
  where
    handlers =
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
            :<|> postActivitySensitivity
            :<|> postActivityInventory
            :<|> postActivitySupplyChain
            :<|> getActivityConsumers
            :<|> getActivityPathTo
            :<|> getContributingFlows
            :<|> getContributingActivities
            :<|> getFlowDetail
            :<|> getFlowActivities
            :<|> getMethods
            :<|> getMethodDetail
            :<|> getMethodFactors
            :<|> getMethodMapping
            :<|> getFlowCFMapping
            :<|> getCollectionCoverage
            :<|> getCharacterization
            :<|> explainCFHandler
            :<|> searchFlows
            :<|> countSearchMatches
            :<|> searchActivitiesWithCount
            :<|> getClassifications
            :<|> postImpactsBatch
            :<|> DBHandlers.getDatabases
            :<|> DBHandlers.loadDatabaseHandler
            :<|> DBHandlers.unloadDatabaseHandler
            :<|> DBHandlers.relinkDatabaseHandler
            :<|> DBHandlers.gapReportHandler
            :<|> DBHandlers.qualityReportHandler
            :<|> qualityReportCsvH
            :<|> computedQualityReportH
            :<|> computedQualityReportCsvH
            :<|> DBHandlers.coverageReportHandler
            :<|> DBHandlers.copyDatabaseHandler
            :<|> DBHandlers.deriveDatabaseHandler
            :<|> DBHandlers.deleteDatabaseHandler
            :<|> DBHandlers.deleteActivitiesHandler
            :<|> DBHandlers.createActivitiesHandler
            :<|> DBHandlers.replaceActivityHandler
            :<|> DBHandlers.editExchangesHandler
            :<|> DBHandlers.exportDatabaseHandler
            :<|> DBHandlers.uploadDatabaseHandler
            :<|> DBHandlers.getDatabaseSetupHandler
            :<|> DBHandlers.addDependencyHandler
            :<|> DBHandlers.removeDependencyHandler
            :<|> DBHandlers.setDataPathHandler
            :<|> DBHandlers.finalizeDatabaseHandler
            :<|> getMethodCollections
            :<|> loadMethodCollectionHandler
            :<|> unloadMethodCollectionHandler
            :<|> DBHandlers.deleteMethodHandler
            :<|> DBHandlers.uploadMethodHandler
            :<|> DBHandlers.exportMethodHandler
            :<|> DBHandlers.listRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.loadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.unloadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.deleteRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.uploadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.getFlowSynonymGroupsHandler
            :<|> DBHandlers.downloadRefDataHandler DBHandlers.FlowSynonyms
            :<|> DBHandlers.listRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.loadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.unloadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.deleteRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.uploadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.listRefData DBHandlers.UnitDefs
            :<|> DBHandlers.loadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.unloadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.deleteRefData DBHandlers.UnitDefs
            :<|> DBHandlers.uploadRefData DBHandlers.UnitDefs
            :<|> getLogsHandler
            :<|> postAuth
            :<|> getVersion
            :<|> getHosting
            :<|> getStats
            :<|> getClassificationPresets
            :<|> getOpenApiSpec

{- | Build the scoring input map (impact method name → raw score) from LCIA
results. Keyed by method NAME, which is unique per collection — not by
'lrCategory', which for ILCD methods is the coarse damage class (e.g. all four
"Climate change-*" methods share category "Climate change"; the three freshwater
ecotoxicity methods share "Aquatic eco-toxicity"). Keying by category collapses
such methods and breaks single-score resolution ("Unknown variable: …"). For
SimaPro-adapted methods name == category, so their scoring is unchanged.
-}
rawScoreMapByName :: [LCIAResult] -> M.Map Text Double
rawScoreMapByName results = M.fromList [(lrMethodName r, lrScore r) | r <- results]

{- | Evaluate every scoring set against the raw impact score map.
Returns (setName → scoreName → value, setName → varName → ScoringIndicator).
Scoring sets that fail to evaluate are logged as warnings and omitted.
Values are pre-multiplied by each set's displayMultiplier (default 1.0).
-}
computeAllScoringSets ::
    [ScoringSet] ->
    M.Map Text Double ->
    IO (M.Map Text (M.Map Text Double), M.Map Text (M.Map Text ScoringIndicator))
computeAllScoringSets scoringSets rawScoreMap = do
    evaluations <- forM scoringSets $ \ss ->
        case computeFormulaScores ss rawScoreMap of
            Right eval -> pure $ Just (ss, eval)
            Left err -> do
                reportProgress Warning $
                    "  Scoring set '"
                        <> T.unpack (ssName ss)
                        <> "' failed: "
                        <> err
                pure Nothing
    let ok = [(ss, e) | Just (ss, e) <- evaluations]
        scores = M.fromList [(ssName ss, seScores e) | (ss, e) <- ok]
        indicators = M.fromList [(ssName ss, toIndicators ss e) | (ss, e) <- ok]
    pure (scores, indicators)
  where
    -- Only emit rows for variables that actually contribute to a score formula.
    -- Intermediate helpers (consumed by `computed` but not referenced in any
    -- `scores.*` formula) are hidden from the breakdown.
    toIndicators ss e =
        let displayed = S.fromList (concatMap (Expr.collectIdentifiers '.') (M.elems (ssScores ss)))
            names = ssLabels ss <> ssVariables ss
         in M.mapWithKey
                ( \var val ->
                    ScoringIndicator
                        { siCategory = M.findWithDefault var var names
                        , siValue = val
                        }
                )
                (M.filterWithKey (\var _ -> S.member var displayed) (seNwEnv e))

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

{- | Internal helper for flow search with optional language filtering.
The 'ffQuery' is always present (callers short-circuit on the no-query
case); language filtering is not yet implemented.
-}
searchFlowsInternal :: Database -> Service.FlowFilter -> AppM (SearchResults FlowSearchResult)
searchFlowsInternal db ff@Service.FlowFilter{Service.ffQuery = query, Service.ffLimit = limitParam, Service.ffOffset = offsetParam} =
    -- Language filtering not yet implemented, search all synonyms
    liftIO $
        paginateResults
            (Service.flowSearchResults (dbUnits db) (Service.producerCount db) ff (findFlowsBySynonym db query))
            limitParam
            offsetParam

{- | The three tab counts for one query, in a single call.

The query is required: an empty search box has nothing to count, and counting
every row of a database to say so would be the most expensive way to answer
nothing.

@sort@ and @exact@ are the ones the caller is going to list with, because they
decide which matcher runs and therefore how many rows there are.
-}
countSearchMatches :: Text -> Maybe Text -> Maybe Text -> Maybe Bool -> AppM SearchCountsAPI
countSearchMatches dbName mQuery sortParam exactParam = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    query <- maybe (badRequest "q is required: there is nothing to count without a query") pure (nonBlank mQuery)
    let listedAs = Service.CountAs{Service.caSort = sortParam, Service.caExact = fromMaybe False exactParam}
        counts = Service.searchCounts (DM.managerGeographies dbManager) db listedAs query
    pure
        SearchCountsAPI
            { scaProcesses = Service.scProcesses counts
            , scaProducts = Service.scProducts counts
            , scaFlows = Service.scFlows counts
            }
  where
    -- Punctuation alone survives a blank test and then tokenises to nothing,
    -- so it would answer three zeros where an empty box is refused.
    nonBlank :: Maybe Text -> Maybe Text
    nonBlank = mfilter (not . null . Normalize.queryWords) . fmap T.strip

-- | Proxy for the API
lcaAPI :: Proxy LCAAPI
lcaAPI = Proxy

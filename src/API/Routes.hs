{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Csv (CSV)
import API.DatabaseHandlers (explainCFToAPI, explainedFlowAPI, simpleAction)
import qualified API.DatabaseHandlers as DBHandlers
import qualified API.MethodEditHandlers as MethodEdit
import qualified API.OpenApi
import API.Types (ActivateResponse (..), ActivityComparison, ActivityContribution (..), ActivityInfo (..), ActivityInput (..), ActivitySummary (..), ActivityWriteRequest (..), ActivityWriteResponse (..), Aggregation (..), AvailableMethodCollection, BatchImpactsEntry (..), BatchImpactsRequest (..), BatchImpactsResponse (..), BinaryContent (..), CatalogueEntry, CatalogueFingerprint (..), CataloguePage, CategoryEditRequest, ChangesApplied, ChangesPresence, ChangesQuery (..), CharacterizationEntry (..), CharacterizationResult (..), ClassificationEntryInfo (..), ClassificationPresetInfo (..), ClassificationSystem (..), CollectionCoverage (..), CollectionFactors (..), ComputedQualityReportAPI (..), ConsumersResponse (..), ContributingActivitiesResult (..), ContributingFlowsResult (..), CoverageReportAPI (..), CutoffWasteFlow (..), DatabaseComparison, DatabaseExportRequest (..), DatabaseHistoryEntry, DatabaseListResponse, DeleteSelectionRequest (..), DeleteSelectionResponse (..), DeriveRequest (..), ExchangeContribution (..), ExchangeDetail (..), ExchangeEditRequest (..), ExchangeEditResponse (..), ExplainCFResult (..), ExportRequest (..), FactorEditRequest, FactorReading, FlowCFEntry (..), FlowCFMapping (..), FlowContributionEntry (..), FlowDetail (..), FlowFactorsResult (..), FlowSearchResult (..), FlowSummary (..), FormulaCheckRequest, FormulaCheckResponse, FormulaLanguage, FreeParameter (..), GapReportAPI (..), GraphExport (..), HostingInfo (..), InventoryExport (..), LCIABatchResult (..), LCIAResult (..), LoadDatabaseResponse (..), MappingStatus (..), MethodCollectionComparison (..), MethodCollectionListResponse (..), MethodCollectionProfile (..), MethodCollectionStatusAPI (..), MethodDetail (..), MethodEditResponse, MethodFactorAPI (..), MethodFlowAPI, MethodHistoryEntry, MethodSummary (..), ModelBinding (..), ModelEmission (..), ModelInput (..), ModelLine (..), ModelProcess (..), ModelReference (..), ModelScoringSet (..), ModelSupplier (..), ParameterAPI, ParameterName (..), ParameterValue, ParametersEditRequest, ParametersEditResponse, PerturbedEntry (..), ProcessDivisionAPI (..), QualityReportAPI (..), ReducedMethod (..), ReducedModel (..), ReducedModelRequest (..), RefDataListResponse (..), RefDataRowsResponse (..), RelinkRequest (..), RelinkResponse (..), ScenarioAPI, ScenarioSaveRequest, ScoreBasis, ScorePairs, ScoringEditRequest, ScoringIndicator (..), ScoringSetAPI, SearchCountsAPI (..), SearchResults (..), SensitivityRequest (..), SensitivityResponse (..), SubstitutionRequest (..), SupplyChainResponse (..), SynonymGroupsResponse (..), SystemModelAPI (..), TreeExport (..), UnmappedFlowAPI (..), UploadChunk (..), UploadResponse (..), WithheldShare, apiFlowOfKind, formulaLanguage, parseProducerFilter)
import App.Env (AppEnv (..), AppM, counted, countedEach, runApp)
import Config (DatabaseConfig (..))
import qualified Config
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Concurrent.STM (readTVarIO)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, mfilter, unless, when, zipWithM)
import Control.Monad.Catch (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAscii, isControl)
import Data.Foldable (asum)
import Data.Indexing (groupInOrder)
import Data.Int (Int32)
import Data.List (intercalate, nub, partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.OpenApi (OpenApi, ToSchema)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Validation as V
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database
import qualified Database.ComputedQuality as CQ
import Database.Cutoffs (Cutoffs (..))
import Database.Edit (authorContext, editExchanges)
import Database.Journal (noteFrom)
import Database.Manager (DatabaseManager (..), DatabaseSetupInfo (..), LoadedDatabase (..), getDatabase, getMergedUnitConfig)
import qualified Database.Manager as DM
import Database.Requirements (ReleaseSubstitution)
import Database.SystemModel (Traced (..), tracedRule)
import EcoSpold.Common (acrossCapabilities)
import qualified Expr
import GHC.Generics
import qualified GHC.Stats
import qualified Impact
import Impact.Score (ScoreRef (..), Source (..))
import qualified Impact.Score as Score
import Matrix (Inventory, Vector)
import qualified Matrix
import qualified Method.Explain as Explain
import Method.Mapping (BuildProvenance (..), CF (..), FlowContribution (..), LongTermMode (..), MappingStats (..), MethodTables (..), Resolution (..), TableEntry (..), characterizedFlowIds, computeLCIAScoreSetFromTables, computeMappingStats, longTermModeFromExclude, lookupEntryForFlow, provenanceStrategyText, strategyToText)
import qualified Method.Mapping
import Method.Scoring (ScoringRow (..), readingNotice, rowsOf)
import Method.SimaProScoring (LegacyEntry, LegacyReading (..), legacyReading, legacyReadings, legacySet, legacySetNames)
import Method.Types (Method (..), MethodCF (..), MethodCollection (..), ScoringEvaluation (..), ScoringSet (..), computeFormulaScores)
import qualified Method.Types as MT
import Numeric (showFFloat)
import Parametric (Input (..), Moving (..), describeParameterRefusal)
import Progress (ProgressLevel (Info, Warning), getLogLines, reportProgress)
import qualified ReducedModel as RM
import qualified Search.Normalize as Normalize
import Servant
import Servant.OpenApi (toOpenApi)
import qualified Service
import qualified Service.Aggregate as Agg
import Service.Catalogue (PageWindow (..), catalogueDefaultLimit, catalogueEntries, catalogueFingerprint, cataloguePage)
import qualified Service.Compare as Compare
import qualified Service.CompareMethods as CompareMethods
import SharedSolver (Reached (..), SharedSolver)
import qualified SharedSolver
import Tree (buildLoopAwareTree)
import Types
import Usage (BootId, ProcessKey (..), UsageKind (..), UsageLog, UsagePage, forgetUsage, readUsage)
import qualified Version

-- | API type definition - RESTful design with focused endpoints
type LCAAPI =
    "api"
        :> "v1"
        :> ( "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> Get '[JSON] ActivityInfo
                -- Where a process of a child comes from: its row of the delivery sheet, or null
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "division" :> Get '[JSON] (Maybe ProcessDivisionAPI)
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "flows" :> Get '[JSON] [FlowSummary]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "reference-product" :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "tree" :> Get '[JSON] TreeExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> QueryParam "flow" Text :> QueryParam "limit" Int :> Get '[JSON] InventoryExport
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
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" DM.CollectionName :> QueryParam "exclude-long-term" Bool :> QueryParam "exchange-contributions" Bool :> Get '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" DM.CollectionName :> QueryParam "exclude-long-term" Bool :> QueryParam "exchange-contributions" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" DM.CollectionName :> Capture "methodId" Text :> QueryParam "top-flows" Int :> Get '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" DM.CollectionName :> Capture "methodId" Text :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "sensitivity" :> Capture "collection" DM.CollectionName :> Capture "methodId" Text :> ReqBody '[JSON] SensitivityRequest :> Post '[JSON] SensitivityResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "reduced-model" :> Capture "collection" DM.CollectionName :> QueryParam "exclude-long-term" Bool :> ReqBody '[JSON] ReducedModelRequest :> Post '[JSON] ReducedModel
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> QueryParam "flow" Text :> QueryParam "limit" Int :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] InventoryExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] SupplyChainResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "consumers" :> QueryParam "name" Text :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> Get '[JSON] ConsumersResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "path-to" :> QueryParam "target" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "compare" :> QueryParam "other_process_id" Text :> QueryParam "other_database" Text :> Get '[JSON] ActivityComparison
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "changes-present" :> ReqBody '[JSON] ChangesQuery :> Post '[JSON] ChangesPresence
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "apply-changes" :> ReqBody '[JSON] ChangesQuery :> Post '[JSON] ChangesApplied
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-flows" :> Capture "collection" DM.CollectionName :> Capture "methodId" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingFlowsResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-activities" :> Capture "collection" DM.CollectionName :> Capture "methodId" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingActivitiesResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-flows" :> Capture "collection" DM.CollectionName :> "score" :> Capture "scoringSet" Text :> Capture "score" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingFlowsResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-activities" :> Capture "collection" DM.CollectionName :> "score" :> Capture "scoringSet" Text :> Capture "score" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingActivitiesResult
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> "activities" :> QueryParam "role" Text :> Get '[JSON] [ActivitySummary]
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> "factors" :> QueryParam "collection" Text :> Get '[JSON] FlowFactorsResult
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> QueryParam "collection" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> QueryParam "collection" Text :> Get '[JSON] [MethodFactorAPI]
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "mapping" :> QueryParam "collection" Text :> Get '[JSON] MappingStatus
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "flow-mapping" :> QueryParam "collection" Text :> Get '[JSON] FlowCFMapping
                :<|> "db" :> Capture "dbName" Text :> "method-collection" :> Capture "collection" DM.CollectionName :> "coverage" :> Get '[JSON] CollectionCoverage
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "characterization" :> QueryParam "flow" Text :> QueryParam "limit" Int :> QueryParam "collection" Text :> Get '[JSON] CharacterizationResult
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "explain-cf" :> Capture "flowId" Text :> QueryParam "collection" Text :> Get '[JSON] ExplainCFResult
                :<|> "db" :> Capture "dbName" Text :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "kind" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "db" :> Capture "dbName" Text :> "search-counts" :> QueryParam "q" Text :> QueryParam "sort" Text :> QueryParam "exact" Bool :> Get '[JSON] SearchCountsAPI
                :<|> "db" :> Capture "dbName" Text :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "exact" Bool :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParams "process" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "db" :> Capture "dbName" Text :> "classifications" :> Get '[JSON] [ClassificationSystem]
                :<|> "db" :> Capture "dbName" Text :> "catalogue" :> QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] CataloguePage
                :<|> "db" :> Capture "dbName" Text :> "catalogue" :> "fingerprint" :> Get '[JSON] CatalogueFingerprint
                :<|> "db" :> Capture "dbName" Text :> "compare" :> QueryParam "other_database" Text :> QueryParam "limit" Int :> Get '[JSON] DatabaseComparison
                :<|> "db" :> Capture "dbName" Text :> "score-pairs" :> QueryParam "other_database" Text :> Get '[JSON] ScorePairs
                :<|> "db" :> Capture "dbName" Text :> "impacts" :> Capture "collection" DM.CollectionName :> QueryParam "top-flows" Int :> QueryParam "exclude-long-term" Bool :> ReqBody '[JSON] BatchImpactsRequest :> Post '[JSON] BatchImpactsResponse
                :<|> "db" :> Capture "dbName" Text :> "impacts" :> Capture "collection" DM.CollectionName :> Capture "methodId" Text :> QueryParam "top-flows" Int :> QueryParam "exclude-long-term" Bool :> ReqBody '[JSON] BatchImpactsRequest :> Post '[JSON] BatchImpactsResponse
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
                -- A parent of a database read from its files, or a child of a parent
                -- divided by a system model. A load, not a copy: it answers as a load does
                :<|> "db" :> Capture "dbName" Text :> "derive" :> Capture "newName" Text :> ReqBody '[JSON] DeriveRequest :> Post '[JSON] LoadDatabaseResponse
                -- A child's system model, each exception with the parent's processes it takes
                :<|> "db" :> Capture "dbName" Text :> "system-model" :> Get '[JSON] SystemModelAPI
                :<|> "db" :> Capture "dbName" Text :> Delete '[JSON] ActivateResponse
                -- Delete the whole filtered set of activities (selection in JSON body)
                :<|> "db" :> Capture "dbName" Text :> "delete" :> ReqBody '[JSON] DeleteSelectionRequest :> Post '[JSON] DeleteSelectionResponse
                -- Write activities. POST adds to the collection, PUT rewrites the one
                -- addressed – never both, so a mistyped identity fails instead of
                -- quietly becoming a duplicate of the row it meant to correct.
                :<|> "db" :> Capture "dbName" Text :> "activities" :> ReqBody '[JSON] ActivityWriteRequest :> Post '[JSON] ActivityWriteResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> QueryParam "note" Text :> ReqBody '[JSON] ActivityInput :> Put '[JSON] ActivityWriteResponse
                -- Change the inventory of one activity, keeping everything else it
                -- carries – the operation a PUT cannot do for a row that came in
                -- from a database file rather than from a description
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "exchanges" :> ReqBody '[JSON] ExchangeEditRequest :> Post '[JSON] ExchangeEditResponse
                -- Export a loaded database as raw bytes in the requested format;
                -- approximation warnings travel percent-encoded in a response header
                :<|> "db" :> Capture "dbName" Text :> "export" :> ReqBody '[JSON] DatabaseExportRequest :> Post '[OctetStream] (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
                -- A file a database ships with a literature entry, by the path
                -- the entry lists it under
                :<|> "db" :> Capture "dbName" Text :> "files" :> CaptureAll "path" Text :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
                -- The licence an uploaded database is published under; the export
                -- and the files above obey its refusal to be downloaded
                :<|> "db" :> Capture "dbName" Text :> "licence" :> ReqBody '[JSON] Licence :> Put '[JSON] Licence
                -- Which published database an uploaded database is, or null for none
                :<|> "db" :> Capture "dbName" Text :> "release" :> ReqBody '[JSON] (Maybe Release) :> Put '[JSON] (Maybe Release)
                -- The name an uploaded database is shown under; its slug stays
                :<|> "db" :> Capture "dbName" Text :> "display-name" :> ReqBody '[JSON] DisplayName :> Put '[JSON] DisplayName
                -- What an uploaded database is about, or null for nothing
                :<|> "db" :> Capture "dbName" Text :> "description" :> ReqBody '[JSON] (Maybe Text) :> Put '[JSON] (Maybe Text)
                -- A database accepted in place of a release a packaged database requires
                :<|> "db" :> Capture "dbName" Text :> "accept-substitution" :> ReqBody '[JSON] ReleaseSubstitution :> Post '[JSON] DatabaseSetupInfo
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
                :<|> "method-collections" :> "available" :> Get '[JSON] [AvailableMethodCollection]
                :<|> "method-collections" :> "available" :> Capture "name" Text :> "install" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "method-collections" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> QueryParam "flows_from" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                -- Export a loaded method collection as raw bytes (SimaPro CSV);
                -- projection warnings travel percent-encoded in a response header
                :<|> "method-collections" :> Capture "name" Text :> "export" :> ReqBody '[JSON] ExportRequest :> Post '[OctetStream] (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
                :<|> "method-collections" :> Capture "collection" DM.CollectionName :> "compare" :> QueryParam "other_collection" DM.CollectionName :> QueryParams "pairs" Text :> QueryParam "category" Text :> QueryParam "limit" Int :> Get '[JSON] MethodCollectionComparison
                :<|> "method-collections" :> Capture "collection" DM.CollectionName :> "profile" :> Get '[JSON] MethodCollectionProfile
                -- Change a method collection of one's own through its journal; one
                -- the configuration declares is copied first
                :<|> "method-collections" :> Capture "collection" Text :> "copy" :> Capture "newName" Text :> Post '[JSON] MethodCollectionStatusAPI
                :<|> "method-collections" :> Capture "collection" Text :> "factors" :> ReqBody '[JSON] FactorEditRequest :> Post '[JSON] MethodEditResponse
                :<|> "method-collections" :> Capture "collection" Text :> "categories" :> ReqBody '[JSON] CategoryEditRequest :> Post '[JSON] MethodEditResponse
                :<|> "method-collections" :> Capture "collection" Text :> "scoring-sets" :> Get '[JSON] [ScoringSetAPI]
                :<|> "method-collections" :> Capture "collection" Text :> "scoring-sets" :> ReqBody '[JSON] ScoringEditRequest :> Post '[JSON] MethodEditResponse
                :<|> "method-collections" :> Capture "collection" Text :> "undo" :> QueryParam "line" Int :> Post '[JSON] MethodEditResponse
                :<|> "method-collections" :> Capture "collection" Text :> "history" :> Get '[JSON] [MethodHistoryEntry]
                :<|> "method-collections" :> Capture "collection" Text :> "flows" :> QueryParam "q" Text :> QueryParam "limit" Int :> Get '[JSON] [MethodFlowAPI]
                -- Reference data endpoints (flow synonyms, compartment mappings, units)
                :<|> "flow-synonyms" :> Get '[JSON] RefDataListResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "groups" :> Get '[JSON] SynonymGroupsResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
                :<|> "flow-synonyms" :> Capture "name" Text :> "rows" :> Get '[JSON] RefDataRowsResponse
                :<|> "compartment-mappings" :> Get '[JSON] RefDataListResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
                :<|> "compartment-mappings" :> Capture "name" Text :> "rows" :> Get '[JSON] RefDataRowsResponse
                :<|> "units" :> Get '[JSON] RefDataListResponse
                :<|> "units" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "units" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "units" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
                :<|> "units" :> Capture "name" Text :> "rows" :> Get '[JSON] RefDataRowsResponse
                -- Log endpoint
                :<|> "logs" :> QueryParam "since" Int :> Get '[JSON] Value
                -- Usage log: what was computed, for the operator to collect
                :<|> "usage" :> QueryParam "after" Int :> Get '[JSON] UsagePage
                :<|> "usage" :> QueryParam' '[Required, Strict] "boot" BootId :> QueryParam' '[Required, Strict] "through" Int :> Delete '[JSON] NoContent
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
                -- The standard licences a database can be published under, as the engine reads them
                :<|> "licences" :> Get '[JSON] [Licence]
                -- The formula language, from the table the engine reads formulas with
                :<|> "formula-language" :> Get '[JSON] FormulaLanguage
                :<|> "db" :> Capture "dbName" Text :> "formula-check" :> ReqBody '[JSON] FormulaCheckRequest :> Post '[JSON] FormulaCheckResponse
                -- Every change made to the database, and why
                :<|> "db" :> Capture "dbName" Text :> "history" :> Get '[JSON] [DatabaseHistoryEntry]
                -- The database's own parameters, and their edit
                :<|> "db" :> Capture "dbName" Text :> "parameters" :> Get '[JSON] [ParameterAPI]
                :<|> "db" :> Capture "dbName" Text :> "parameters" :> ReqBody '[JSON] ParametersEditRequest :> Post '[JSON] ParametersEditResponse
                -- Scenarios: named sets of parameter values saved on a database
                :<|> "db" :> Capture "dbName" Text :> "scenarios" :> Get '[JSON] [ScenarioAPI]
                :<|> "db" :> Capture "dbName" Text :> "scenarios" :> Capture "name" Text :> ReqBody '[JSON] ScenarioSaveRequest :> Put '[JSON] [ScenarioAPI]
                :<|> "db" :> Capture "dbName" Text :> "scenarios" :> Capture "name" Text :> Delete '[JSON] [ScenarioAPI]
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

{- | Cross-DB inventory + per-DB scaling vectors. The scalings are needed by
the regionalized LCIA path (per-DB dot products summed across all DBs
reached at request time); the inventory alone is enough for non-regional
methods.
-}
solutionWithDeps :: Text -> Database -> SharedSolver -> ProcessId -> AppM SharedSolver.CrossDBSolution
solutionWithDeps dbName db solver pid = do
    dbManager <- asks aeDbManager
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
to build an LCIAResult per pid in O(1) per method – no TVar reads, no
per-pid cache lookups, no inventoryContributions walk.
-}
data MethodCtx = MethodCtx
    { mctxMethod :: !Method
    , mctxMappedFlows :: !Int
    }

{- | Build an 'ActivityContribution' row from a cross-DB contribution key
@(depDbName, pid)@. For dep-DB rows the process ID is qualified as
@"dbName::actUUID_prodUUID"@ – same convention as
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
                prodName = maybe "" Service.rpName (mAct >>= Service.referenceProductOf (dbTechFlows d) unitDB)
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

-- ToSchema orphan for the login request – lives here (not in API.OpenApi)
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
-- Hoisted helpers – previously in lcaServer's `where`. Lifted to top level so
-- non-Servant callers (notably src/API/BatchImpacts.hs and any client of the
-- LCIA batch pipeline outside the Servant AppM stack) can reuse them.
--
-- Behavior is byte-identical to the original where-bound versions.
-- ============================================================================

-- | Fill the fields kept until 0.16.0 from the set read from the method file. Pure.
enrichWithNW :: M.Map Text LegacyEntry -> LCIAResult -> LCIAResult
enrichWithNW index result = case legacyReading index (lrMethodName result) (lrScore result) of
    Nothing -> result
    Just r -> result{lrDamageCategory = lgDamage r, lrNormalizedScore = lgNormalized r, lrWeightedScore = lgWeighted r}

-- | What 'enrichWithNW' reads, built once per response from a collection's sets.
legacyIndex :: [ScoringSet] -> M.Map Text LegacyEntry
legacyIndex = maybe M.empty legacyReadings . legacySet

-- | Assemble an LCIABatchResult from the post-characterization parts. Pure.
mkLCIABatchResult ::
    [LCIAResult] ->
    [(ScoringSet, ScoringEvaluation)] ->
    [ScoringSet] ->
    [CutoffWasteFlow] ->
    Cutoffs ->
    LCIABatchResult
mkLCIABatchResult results evaluated scoringSets cutoffWaste cutoffs =
    LCIABatchResult
        { lbrResults = results
        , lbrSingleScore = Nothing
        , lbrSingleScoreUnit = Nothing
        , lbrNormWeightSetName = listToMaybe (legacySetNames scoringSets)
        , lbrAvailableNWsets = legacySetNames scoringSets
        , lbrScoringResults = scoresOf evaluated
        , lbrScoringUnits = M.fromList [(ssName ss, ssUnit ss) | ss <- scoringSets]
        , lbrScoringIndicators = M.fromList [(ssName ss, scoringIndicators ss e) | (ss, e) <- evaluated]
        , lbrScoringRows = M.fromList [(ssName ss, scoringRows ss e) | (ss, e) <- evaluated]
        , lbrWithheld = []
        , lbrCutoffWaste = cutoffWaste
        , lbrCutoffInputs = cutoffShown cutoffs
        , lbrWithheldCutoffs = cutoffWithheld cutoffs
        , lbrBasis = Nothing
        }

{- | What the score of one activity rests on: the databases its solution
reached, which a substitution may take beyond the dependencies, and what the
request substituted and set.
-}
activityBasis :: DatabaseManager -> DM.CollectionName -> SharedSolver.CrossDBSolution -> Maybe SubstitutionRequest -> IO ScoreBasis
activityBasis dbManager collection sol mSub =
    DM.solutionBasis
        dbManager
        sol
        collection
        (DM.Asked (foldMap srSubstitutions mSub) (filter (not . null) (map srParameters (maybeToList mSub))))

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
matrix index too – see Service.validateProcessIdInMatrixIndex.
-}
resolveOrThrow :: Database -> Text -> AppM (ProcessId, Activity)
resolveOrThrow db processIdText = do
    (pid, act) <- either throwServiceError pure (Service.resolveScorable db processIdText)
    either throwServiceError pure (Service.validateProcessIdInMatrixIndex db pid)
    pure (pid, act)

{- | Pure mapping from a domain 'Service.ServiceError' to the HTTP error it
surfaces as. Every constructor is a client-supplied invariant breakage, so all
map to 4xx/422 – never 5xx. Kept pure (and separate from 'throwServiceError')
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
    -- and cross-DB unit-conversion failures – all client-submitted invariant
    -- breakages. Surface as 422 like the rest of the cross-DB pipeline.
    Service.MatrixError msg -> err422{errBody = utf8Body msg}
    Service.Withheld msg -> err403{errBody = utf8Body msg}
    Service.ParameterRefused msg -> err400{errBody = utf8Body msg}
    Service.NoSuchLine msg -> err400{errBody = utf8Body msg}
  where
    utf8Body = BSL.fromStrict . T.encodeUtf8

throwServiceError :: Service.ServiceError -> AppM a
throwServiceError = throwError . serviceErrorToServerError

-- | Answer 400 with a message the caller can act on, rather than a body-less status.
badRequest :: Text -> AppM a
badRequest msg = throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

-- | Load a method collection by name from the live DatabaseManager state.
loadCollection :: DM.CollectionName -> AppM ([Method], [ScoringSet])
loadCollection collectionName = do
    dbManager <- asks aeDbManager
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    case M.lookup (DM.unCollectionName collectionName) loadedCollections of
        Just mc -> return (mcMethods mc, mcScoringSets mc)
        Nothing ->
            throwError
                err404
                    { errBody =
                        collectionNotLoadedBody
                            (DM.unCollectionName collectionName)
                            (M.keys loadedCollections)
                    }

{- | Cross-DB inventory solution for an activity. No substitution takes the
cached path; substitutions are applied through the uncached one.
-}
crossDBSolutionFor :: Text -> Database -> SharedSolver -> ProcessId -> Maybe SubstitutionRequest -> AppM SharedSolver.CrossDBSolution
crossDBSolutionFor dbName db solver pid mSub = case maybe [] srSubstitutions mSub of
    [] -> solutionWithDeps dbName db solver pid
    subs -> do
        dbManager <- asks aeDbManager
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
                    subs
        either throwServiceError pure eSol

{- | Run with the database a request reads: the loaded one, or, when the request
states parameter values, the loaded one at those values with a solver for it
that lives as long as the run. Only the database asked is read at those
values: a dependency that links back to it reads it at its defaults.
-}
withRequestDatabase :: Text -> Maybe SubstitutionRequest -> (Database -> SharedSolver -> AppM a) -> AppM a
withRequestDatabase dbName mSub run = do
    loaded <- requireDatabaseByName dbName
    withValues id dbName loaded (maybe [] srParameters mSub) run

{- | Run with the loaded database at these values and a solver for it, as
'withRequestDatabase' does. A refusal of the values passes through @refused@,
which lets a batch say which of its sets was refused; what @run@ throws does
not.
-}
withValues :: (ServerError -> ServerError) -> Text -> (Database, SharedSolver) -> [ParameterValue] -> (Database -> SharedSolver -> AppM a) -> AppM a
withValues _ _ (db, solver) [] run = run db solver
withValues refused dbName loaded@(db, _) values run = do
    settings <- either (throwError . refused . serviceErrorToServerError) pure (Service.settingsOf db values)
    env <- asks id
    outcome <- liftIO (DM.withSettings (aeDbManager env) dbName loaded settings (\db' solver' -> runHandler (runApp env (run db' solver'))))
    either (throwError . refused . err400Of) (either throwError pure) outcome
  where
    err400Of :: Text -> ServerError
    err400Of msg = err400{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

{- | Look up MethodSetTables for every (DB, scaling) pair in the cross-DB
solution. Cache-keyed by (dbName, methods).
-}
buildPerDbSetTables ::
    DatabaseManager ->
    DM.CollectionName ->
    NE.NonEmpty Reached ->
    [Method] ->
    IO (NE.NonEmpty (Database, Vector, Method.Mapping.MethodSetTables))
buildPerDbSetTables dbManager collection scalings methods =
    traverse
        ( \(Reached n d sv) -> do
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
                (SharedSolver.csLongTerm sol)
                (SharedSolver.csInventory sol)
                hier
                perDb

{- | Resolve a method's precomputed batched score. A 'Left' here is a scoring
integrity error (mismatched table lengths, absent weights – never a mere
coverage gap, see 'computeRegionalizedLCIAScore'): it must reach the caller
as an error, never collapse to a 0 the consumer cannot tell from a real
score. A method missing from the map is the same kind of error – the map is
built from the very method list being scored.
-}
resolveBatchedScore :: Method -> M.Map UUID (Either Text Double) -> Either Text Double
resolveBatchedScore method scoreMap =
    case M.lookup (methodId method) scoreMap of
        Nothing -> Left ("[LCIA " <> methodName method <> "] missing from the batched score set")
        Just (Left err) -> Left ("[LCIA " <> methodName method <> "] " <> err)
        Just (Right s) -> Right s

{- | Surface a scoring integrity error as a 500: the collection's tables are
inconsistent and the score is not computable – a silent 0 would be worse
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
    Impact.LicencedSolution ->
    -- | the cut-offs that solution met, read once for every method scored on it
    Cutoffs ->
    Activity ->
    Int ->
    Maybe (Either Text Double) ->
    Method ->
    IO (Either Text LCIAResult)
computeCategoryResult dbManager dbName collection db ls cutoffs activity topFlows precomputedScore method = do
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    mappings <- DM.mapMethodToFlowsCached dbManager dbName collection db method
    tables <- DM.mapMethodToTablesCached dbManager dbName collection db method
    let sol = Impact.lsWhole ls
        inventory = SharedSolver.csInventory sol
        stats = computeMappingStats mappings
        functionalUnit = Service.functionalUnitOf (dbTechFlows db) mUnits activity
    warnUnknownInventoryFlows ("LCIA " <> methodName method) mFlows inventory
    -- A Left is a scoring integrity error (see 'resolveBatchedScore') – it
    -- propagates instead of collapsing to a 0 the consumer can't tell from a
    -- real score. A precomputed Left arrives already labeled by
    -- 'resolveBatchedScore'; only the locally computed one is labeled here.
    scoreE <- case precomputedScore of
        Just e -> traverse evaluate e
        Nothing -> Impact.scoreSolution dbManager collection method tables sol
    case scoreE of
        Left err -> pure (Left err)
        Right score -> do
            contribsE <- licencedTopFlows dbManager collection method tables ls score topFlows
            -- A miss carries an amount of the inventory: read from the part the
            -- licences let be detailed, and kept whole by a dependency keeping its amounts.
            inventoryWithheld <- Impact.withheldDatabases dbManager ReadInventory sol
            let misses
                    | null inventoryWithheld = Service.missesShown mUnits score (Method.Mapping.probableMisses mFlows tables (SharedSolver.csInventory (Impact.lsShown ls)))
                    | otherwise = Service.missesWithheld inventoryWithheld
                result :: TopFlows -> LCIAResult
                result TopFlows{tfRows = topContributors, tfWithheld = withheld} =
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
                        , lrWithheld = Nothing
                        , lrWithheldDatabases = withheld
                        , lrCutoffInputs = cutoffShown cutoffs
                        , lrWithheldCutoffs = cutoffWithheld cutoffs
                        , lrExchangeContributions = []
                        , lrProbableMisses = Just misses
                        , lrBasis = Nothing
                        }
            pure (fmap result contribsE)

-- | The flows a score publishes, and the line of each dependency that keeps its detail.
data TopFlows = TopFlows
    { tfRows :: [FlowContributionEntry]
    , tfWithheld :: [WithheldShare]
    }

{- | The biggest flows of a score as the licences of the databases it reads let
them be read. The rows come from the same path as the score, so their shares
sum to it with the dependencies' lines. A regionalized method has no other way
to say which flow made which part of its score: read region-blind from the
merged inventory, the shares add up to something else entirely.

When no flow is asked for, nothing is walked and no line is given either: the
lines stand beside the rows they complete.
-}
licencedTopFlows ::
    DatabaseManager ->
    DM.CollectionName ->
    Method ->
    MethodTables ->
    Impact.LicencedSolution ->
    Double ->
    Int ->
    IO (Either Text TopFlows)
licencedTopFlows dbManager collection method tables ls score topFlows
    | topFlows <= 0 = pure (Right (TopFlows [] []))
    | otherwise = fmap toTop <$> Impact.licencedContributionsOf dbManager collection method tables ls
  where
    toTop :: Impact.LicencedContributions -> TopFlows
    toTop lc =
        TopFlows
            { tfRows = topContributorRows (Explain.flowMatchKind tables) score topFlows (Impact.lcRows lc)
            , tfWithheld = Service.withheldShares score (Impact.lcWithheld lc)
            }

-- | Say which flows of an inventory the merged metadata has no record of.
warnUnknownInventoryFlows :: Text -> BioFlowDB -> Inventory -> IO ()
warnUnknownInventoryFlows label mFlows =
    Impact.warnUnknownFlowIds label . Impact.unknownInventoryFlows mFlows

{- | The rows one method publishes under a score: the biggest contributions
first, cut to the number asked for.

The factor a row carries is the one the score applied to that flow. A
regionalized method applies one per location, so the row carries the effective
factor its contribution divided by its quantity – see
'Method.Mapping.regionalizedContributionsCrossDB'.
-}
topContributorRows :: (UUID -> Maybe Text) -> Double -> Int -> [FlowContribution] -> [FlowContributionEntry]
topContributorRows matchKind score topFlows rawContribs =
    [ FlowContributionEntry
        { fcoFlowName = bfName f
        , fcoContribution = c
        , fcoSharePct = if score /= 0 then c / score * 100 else 0
        , fcoFlowId = UUID.toText (bfId f)
        , fcoCategory = bfCompartmentName f
        , fcoCompartment = bfCompartmentSub f
        , fcoCfValue = cfVal
        , fcoMatchKind = matchKind (bfId f)
        }
    | FlowContribution{fcFlow = f, fcFactor = cfVal, fcContribution = c} <-
        take topFlows (sortOn (negate . abs . fcContribution) rawContribs)
    ]

{- | Batch fast path: scores via 'batchedScoresFor', then build LCIAResult
records straight from the precomputed contexts. Skips the per-pid
'mapConcurrently' over methods entirely. When 'topFlows' is 0
(the default) 'lrTopContributors' is left empty and the contribution
walk is skipped; when >0, asks 'Impact.contributionsOf' per method, which
reads the rows by the path that scored the method.
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
    let index = legacyIndex (mcScoringSets collection)
        methods = map mctxMethod ctxs
        inventory = SharedSolver.csInventory sol
    scoreMap <- batchedScoresFor dbManager dbName collectionName db sol methods
    -- Split once: the licences divide the solution, not each method's score.
    ls <- Impact.licencedSolution dbManager SeeDetailedScores sol
    cutoffs <- Impact.licencedCutoffs dbManager ls
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    warnUnknownInventoryFlows ("LCIA batch pid=" <> T.pack (show actPid)) mFlows inventory
    let functionalUnit = Service.functionalUnitOf (dbTechFlows db) mUnits activity
        mkResultIO ctx = do
            let method = mctxMethod ctx
            case resolveBatchedScore method scoreMap of
                Left err -> pure (Left (err <> " (pid=" <> T.pack (show actPid) <> ")"))
                Right score -> do
                    tables <- DM.mapMethodToTablesCached dbManager dbName collectionName db method
                    rowsE <- licencedTopFlows dbManager collectionName method tables ls score topFlows
                    pure (fmap (mkResultForScore ctx method score) rowsE)
        mkResultForScore ctx method score TopFlows{tfRows = topContributors, tfWithheld = withheld} =
            enrichWithNW index $
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
                    , lrWithheld = Nothing
                    , lrWithheldDatabases = withheld
                    , lrCutoffInputs = cutoffShown cutoffs
                    , lrWithheldCutoffs = cutoffWithheld cutoffs
                    , lrExchangeContributions = []
                    , lrProbableMisses = Nothing
                    , lrBasis = Nothing
                    }
    resultsE <- sequence <$> traverse mkResultIO ctxs
    case resultsE of
        Left err -> pure (Left err)
        Right results -> do
            let rawScoreMap = rawScoreMapByName results
            evaluated <- computeAllScoringSets (mcScoringSets collection) rawScoreMap
            licence <- fromMaybe LicenceUnstated <$> DM.databaseLicence dbManager dbName
            pure (Right (Service.withholdBatch dbName licence (mkLCIABatchResult results evaluated (mcScoringSets collection) (Service.buildCutoffWaste db activity) cutoffs)))

-- | Whether a score also says what each line of the activity adds to it.
data ScoreLines = WithoutLines | WithLines
    deriving (Eq, Show)

{- | Top-level LCIA batch entry point – AppM-returning. Used by the Servant
routes (via thin where-aliases) and by API.BatchImpacts.
-}
activityLCIABatchH ::
    Text ->
    Text ->
    DM.CollectionName ->
    Maybe SubstitutionRequest ->
    LongTermMode ->
    ScoreLines ->
    AppM LCIABatchResult
activityLCIABatchH dbName processIdText collectionName mSub ltMode scoreLines = do
    dbManager <- asks aeDbManager
    when (scoreLines == WithLines && not (all (null . srSubstitutions) mSub)) $
        badRequest "What each line adds to the score is given without substitutions: a substituted supplier answers no line of the activity."
    withRequestDatabase dbName mSub $ \db sharedSolver -> do
        (actProcessId, activity) <- resolveOrThrow db processIdText
        (methods, scoringSets) <- loadCollection collectionName
        t0 <- liftIO getCurrentTime
        sol <- crossDBSolutionFor dbName db sharedSolver actProcessId mSub >>= liftIO . Impact.withLongTermPolicy dbManager ltMode
        t1 <- liftIO getCurrentTime
        let inventory = SharedSolver.csInventory sol
            !invSize = M.size inventory
        when (isNothing mSub) $
            liftIO $ do
                reportProgress Info $
                    "[LCIA batch] " <> T.unpack (DM.unCollectionName collectionName) <> " for " <> T.unpack (activityName activity)
                reportProgress Info $
                    "  Inventory: "
                        <> show invSize
                        <> " flows ("
                        <> showFFloat (Just 2) (realToFrac (diffUTCTime t1 t0) :: Double) ""
                        <> "s)"
                when (invSize == 0) $
                    reportProgress Info "  WARNING: inventory is empty – check matrix computation"
                when (invSize > 0 && invSize <= 5) $
                    reportProgress Info $
                        "  Inventory UUIDs: " <> intercalate ", " (map UUID.toString $ M.keys inventory)
        scoreMap <- liftIO $ batchedScoresFor dbManager dbName collectionName db sol methods
        ls <- liftIO (Impact.licencedSolution dbManager SeeDetailedScores sol)
        cutoffs <- liftIO (Impact.licencedCutoffs dbManager ls)
        rawResultsE <-
            liftIO $
                mapConcurrently
                    (\m -> computeCategoryResult dbManager dbName collectionName db ls cutoffs activity 5 (Just (resolveBatchedScore m scoreMap)) m)
                    methods
        rawResults <- either scoringError pure (sequence rawResultsE)
        licence <- DBHandlers.servedLicence dbName
        lines' <-
            if scoreLines == WithLines && all (granted licence) [SeeDetailedScores, ReadInventory]
                then lineScores SupplierFrame{sfName = dbName, sfDatabase = db, sfSolver = sharedSolver, sfCollection = collectionName, sfMethods = methods, sfLongTerm = ltMode, sfRoot = sol} actProcessId
                else pure (map (const []) methods)
        let results = zipWith withLines lines' (map (enrichWithNW (legacyIndex scoringSets)) rawResults)
            rawScoreMap = rawScoreMapByName rawResults
        evaluated <- liftIO $ computeAllScoringSets scoringSets rawScoreMap
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
                forM_ (M.toList (scoresOf evaluated)) $ \(name, scores) ->
                    reportProgress Info $
                        "  Scoring '"
                            <> T.unpack name
                            <> "': "
                            <> intercalate ", " [T.unpack k <> "=" <> showFFloat (Just 6) v "" | (k, v) <- M.toList scores]
        basis <- liftIO (activityBasis dbManager collectionName sol mSub)
        pure (Service.withholdBatch dbName licence (mkLCIABatchResult results evaluated scoringSets (Service.buildCutoffWaste db activity) cutoffs)){lbrBasis = Just basis}

{- | What each line of a process adds to each method's score, by method, each
line by its position. One more unit of the process's product puts each line's
amount in, divided by the reference amount and signed as the demand is; a
supplier's line is scored as one more unit of that supplier, an emission by the
method's factor on it.
-}
lineScores :: SupplierFrame -> ProcessId -> AppM [[(Int, Double)]]
lineScores frame@SupplierFrame{sfDatabase = db, sfMethods = methods} process = do
    unitCfg <- liftIO . getMergedUnitConfig =<< asks aeDbManager
    column <- either unprocessable pure (RM.column unitCfg db process)
    factorsAt <- emissionFactors frame
    answered <- supplierAnswers frame [] (S.toList (S.fromList (map fst (RM.columnTechnosphere column))))
    let perUnit = Matrix.referenceSign db process / Matrix.activityNormalizationFactor db process
    rows <- either unprocessable pure (lineContributions perUnit column (M.fromList answered) (`factorsAt` process))
    pure (foldr (\(n, cs) -> zipWith (:) (map (n,) cs)) (map (const []) methods) rows)

{- | Each line of a column, by its position, with what it adds to each method's
score: its stored amount times what one unit of it adds to the column, times
@perUnit@, times the score of one unit of what it feeds. A line whose supplier
no loaded database holds adds nothing.
-}
lineContributions :: Double -> RM.Column -> M.Map RM.Supplier SupplierAnswer -> (Int32 -> [Double]) -> Either Text [(Int, [Double])]
lineContributions perUnit column answers factorsOf =
    M.toList . M.fromListWith (zipWith (+)) <$> sequence (technosphere <> biosphere)
  where
    technosphere = [placed e (map (* saConversion a) (saScores a)) | (s, e) <- RM.columnTechnosphere column, Just a <- [M.lookup s answers]]
    biosphere = [placed e (factorsOf row) | (row, e) <- RM.columnBiosphere column]

    placed :: RM.Entry -> [Double] -> Either Text (Int, [Double])
    placed e perAmount = case RM.entryLine e of
        Just n -> Right (n, map (* (perUnit * RM.entryFactor e * RM.entryStored e)) perAmount)
        Nothing -> Left "A link to another database answers no line of the activity."

-- | A score with what each line adds to it, largest first.
withLines :: [(Int, Double)] -> LCIAResult -> LCIAResult
withLines lines' result =
    result
        { lrExchangeContributions =
            sortOn
                (Down . abs . ecContribution)
                [ExchangeContribution{ecPosition = n, ecContribution = c, ecSharePct = if lrScore result /= 0 then c / lrScore result * 100 else 0} | (n, c) <- lines']
        }

{- | Everything one chunk of a batch needs and no chunk changes: the database
being scored, the collection scoring it, and the per-method contexts prepared
once for the whole request.
-}
data BatchScope = BatchScope
    { bsDbName :: !Text
    , bsDb :: !Database
    , bsSolver :: !SharedSolver
    , bsCollectionName :: !DM.CollectionName
    , bsCollection :: !MethodCollection
    , bsContexts :: ![MethodCtx]
    , bsTopFlows :: !Int
    , bsLongTerm :: !LongTermMode
    , bsParameterSet :: !(Maybe Int)
    -- ^ The position of the parameter set the database is read at, when the request gave sets
    }

{- | One activity a batch asks about: the id as the caller wrote it, the id it
resolved to, and the activity that id names.
-}
data BatchTarget = BatchTarget
    { btRequestedId :: !Text
    , btProcessId :: !ProcessId
    , btActivity :: !Activity
    }

{- | Solve one chunk of a batch and turn it into entries, with the time its
solve took.

Chunking is what keeps a large batch from being a large heap. A solution
carries a dense scaling vector the width of the database beside its inventory,
hundreds of kilobytes for a database of any size, where the entry built from it
carries one number per method. Solving the whole request before building the
first entry therefore held every solution at once; solving a chunk at a time
holds that many, and the solutions of a finished chunk are free to go. Every
chunk still reuses the one cached factorization, so this bounds the memory
without changing the speed.
-}
scoreChunk :: BatchScope -> [BatchTarget] -> AppM (NominalDiffTime, [Either Text BatchImpactsEntry])
scoreChunk scope chunk = do
    dbManager <- asks aeDbManager
    t0 <- liftIO getCurrentTime
    sols0 <- solutionsWithDeps (bsDbName scope) (bsDb scope) (bsSolver scope) (map btProcessId chunk)
    sols <- liftIO $ acrossCapabilities (Impact.withLongTermPolicy dbManager (bsLongTerm scope)) sols0
    t1 <- liftIO getCurrentTime
    entries <- liftIO $ acrossCapabilities (entryOf dbManager) (zip chunk sols)
    pure (diffUTCTime t1 t0, entries)
  where
    {- An entry is forced where it is built. Every field of it is lazy, and the
    score of a category is a closure over the solution's inventory, so an
    unforced chunk hands the next one its predecessor's solutions to hold on to
    and the bound above buys nothing. -}
    entryOf :: DatabaseManager -> (BatchTarget, SharedSolver.CrossDBSolution) -> IO (Either Text BatchImpactsEntry)
    entryOf dbManager (target, sol) =
        evaluate . force . fmap (named target)
            =<< buildLCIABatchResultCached
                dbManager
                (bsDbName scope)
                (bsCollectionName scope)
                (bsDb scope)
                (btProcessId target)
                (btActivity target)
                (bsCollection scope)
                sol
                (bsContexts scope)
                (bsTopFlows scope)

    named :: BatchTarget -> LCIABatchResult -> BatchImpactsEntry
    named target impacts =
        BatchImpactsEntry
            { bieProcessId = btRequestedId target
            , bieActivityName = activityName (btActivity target)
            , bieProductAmount = Service.rpAmount <$> reference
            , bieProductUnit = Service.rpUnit <$> reference
            , bieParameterSet = bsParameterSet scope
            , bieImpacts = impacts
            }
      where
        reference :: Maybe Service.ReferenceProductInfo
        reference = Service.referenceProductOf (dbTechFlows (bsDb scope)) (dbUnits (bsDb scope)) (btActivity target)

{- | Run a scoring computation once one of the instance's slots is free.

The solver takes one request at a time already, so past a couple of requests
in flight, more of them finish no sooner: they only hold more of their
solutions in memory while they wait. Enough of them at once exhaust the heap
and take the process down for every caller, where a request that waits its
turn loses nothing. Waiting rather than refusing, because the request is
legitimate and the answer would be the same a few seconds later.
-}
withScoringSlot :: DatabaseManager -> AppM a -> AppM a
withScoringSlot dm = maybe id (\slots -> bracket_ (liftIO (waitQSem slots)) (liftIO (signalQSem slots))) (dmScoringSlots dm)

{- | Top-level multi-activity batch impacts, a chunk of activities per solve,
parallel characterization. Used by the Servant POST routes and by
API.BatchImpacts. Given a method of the collection, only that one is scored:
characterization costs per method, so one category of a large database comes
back several times faster than the whole collection.
-}
batchImpactsH ::
    Text ->
    DM.CollectionName ->
    Maybe Text ->
    Maybe Int ->
    LongTermMode ->
    BatchImpactsRequest ->
    AppM BatchImpactsResponse
batchImpactsH dbName collectionName methodParam topFlowsParam ltMode req = do
    -- Before any lookup, like the read-only guard: a request this instance
    -- will not run gets the same answer whether or not its names resolve.
    hosting <- asks aeHostingConfig
    forM_ (Config.scoringRefusal hosting (length (birProcessIds req) * max 1 (length (birParameterSets req))) topFlowsParam) $ \msg ->
        throwError err403{errBody = BSL.fromStrict (T.encodeUtf8 msg)}
    dbManager <- asks aeDbManager
    loaded@(db, sharedSolver) <- requireDatabaseByName dbName
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    whole <- case M.lookup (DM.unCollectionName collectionName) loadedCollections of
        Just mc -> pure mc
        Nothing ->
            throwError err404{errBody = collectionNotLoadedBody (DM.unCollectionName collectionName) (M.keys loadedCollections)}
    -- A scoring set read off one of its categories would be wrong without
    -- saying so, so a single method comes back without any.
    collection <- case methodParam of
        Nothing -> pure whole
        Just methodText -> do
            method <- loadMethodInCollection collectionName methodText
            pure whole{mcMethods = [method], mcScoringSets = []}
    let resolved =
            [ (pidText, Service.resolveScorable db pidText)
            | pidText <- birProcessIds req
            ]
        valid = [BatchTarget pidText pidNum act | (pidText, Right (pidNum, act)) <- resolved]
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
                Service.Withheld _ -> False
                Service.ParameterRefused _ -> False
                Service.NoSuchLine _ -> False
            ]
    t0 <- liftIO getCurrentTime
    ctxs <- liftIO $ mapConcurrently (prepMethodCtx dbManager dbName collectionName db) (mcMethods collection)
    let scope =
            BatchScope
                { bsDbName = dbName
                , bsDb = db
                , bsSolver = sharedSolver
                , bsCollectionName = collectionName
                , bsCollection = collection
                , bsContexts = ctxs
                , bsTopFlows = max 0 (fromMaybe 0 topFlowsParam)
                , bsLongTerm = ltMode
                , bsParameterSet = Nothing
                }
        -- One set after another: a set's database lives while its rows are built, and no longer.
        atSet :: Int -> [ParameterValue] -> AppM [(NominalDiffTime, [Either Text BatchImpactsEntry])]
        atSet i values =
            withValues (inSet i) dbName loaded values $ \db' solver' ->
                traverse
                    (scoreChunk scope{bsDb = db', bsSolver = solver', bsParameterSet = Just i})
                    (Matrix.chunksOf scoringChunk [target{btActivity = dbActivities db' V.! fromIntegral (btProcessId target)} | target <- valid])
        inSet :: Int -> ServerError -> ServerError
        inSet i e = e{errBody = "parameter set " <> BSL.fromStrict (T.encodeUtf8 (T.pack (show i))) <> ": " <> errBody e}
    solved <- withScoringSlot dbManager $ case birParameterSets req of
        [] -> traverse (scoreChunk scope) (Matrix.chunksOf scoringChunk valid)
        sets -> concat <$> zipWithM atSet [0 ..] sets
    let solveTime = sum (map fst solved)
        entriesE = concatMap snd solved
    -- All-or-nothing on purpose: an integrity error is a property of the
    -- (db, method) tables, not of one activity, so every entry would fail
    -- identically. Unresolvable pids stay per-entry (birNotFound/birInvalid).
    entries <- either scoringError pure (sequence entriesE)
    t2 <- liftIO getCurrentTime
    -- No substitution in a batch: what it may read is the dependencies.
    basis <- liftIO (DM.databasesRead dbManager dbName >>= \names -> DM.scoreBasis dbManager names collectionName (DM.Asked [] (birParameterSets req)))
    liftIO $
        reportProgress Info $
            "[batch-impacts] "
                <> T.unpack dbName
                <> " / "
                <> T.unpack (DM.unCollectionName collectionName)
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
                <> " – solve "
                <> showFFloat (Just 2) (realToFrac solveTime :: Double) ""
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
            , birBasis = basis
            , birReadingNotice = readingNotice (mcScoringSets collection)
            }

{- | Computed quality checks over the whole catalogue: score every entry of a
loaded database against one method collection – chunked multi-RHS solves on
the cached factorization, warm method tables – then judge the numbers with
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
        (Just c, _) -> pure (DM.CollectionName c) -- an unknown name answers 404 in batchImpactsH below
        (Nothing, [only]) -> pure (DM.CollectionName only)
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
    -- The batch below would refuse too, but with advice to split a request
    -- the caller never shaped: the report owes its own sentence.
    hosting <- asks aeHostingConfig
    forM_ (Config.activitiesPastLimit hosting (M.size entriesByPid)) $ \most ->
        throwError
            err403
                { errBody =
                    BSL.fromStrict . T.encodeUtf8 $
                        "The computed quality report scores every activity of the database, "
                            <> T.pack (show (M.size entriesByPid))
                            <> " here, and this engine scores at most "
                            <> T.pack (show most)
                            <> " in one request."
                }
    response <-
        batchImpactsH dbName collection Nothing Nothing IncludeLongTerm BatchImpactsRequest{birProcessIds = M.keys entriesByPid, birParameterSets = []}
    -- The ids come from the catalogue itself, so nothing should be
    -- unresolvable – but a dropped entry would silently shrink the report,
    -- so any is worth a warning in the log.
    let unresolved = birNotFound response <> birInvalid response <> birUnscorable response
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
            | e <- birResults response
            , Just act <- [M.lookup (bieProcessId e) entriesByPid]
            ]
    pure (DBHandlers.computedQualityReportToAPI mLimit (CQ.computedQualityReport dbName (DM.unCollectionName collection) scored))

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

{- | How many activities a batch solves before it builds their entries. It
bounds what the request holds, not what the solver allocates: the dense
right-hand-side block is already bounded inside the solver by
'Matrix.multiRhsChunkSize', and every chunk here reuses the one cached
factorization.
-}
scoringChunk :: Int
scoringChunk = 512

-- ---------------------------------------------------------------------------
-- Pure helpers shared by handlers
-- ---------------------------------------------------------------------------

-- | Parse "System=Value[:exact]" into a filter.
parseClassFilter :: Text -> Maybe ClassificationFilter
parseClassFilter raw =
    let (sys, rest) = T.breakOn "=" raw
     in if T.null rest
            then Nothing
            else
                let valAndMode = T.drop 1 rest
                    (val, mode) = T.breakOn ":" valAndMode
                    match = if T.drop 1 mode == "exact" then MatchExact else MatchContains
                 in Just ClassificationFilter{clfSystem = T.strip sys, clfValue = T.strip val, clfMatch = match}

-- | Merge preset-derived and explicit classification filters.
mergeClassFilters ::
    [Config.ClassificationPreset] ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Either Text [ClassificationFilter]
mergeClassFilters presets presetParam systems values modes =
    (++ explicit) <$> Config.expandClassificationPreset presets presetParam
  where
    explicit =
        zipWith3
            (\s v m -> ClassificationFilter{clfSystem = s, clfValue = v, clfMatch = if m == "exact" then MatchExact else MatchContains})
            systems
            values
            (modes ++ repeat "contains")

-- | Build a 'Service.SupplyChainFilter' shared by GET and POST handlers.
buildSupplyChainFilter ::
    -- | The databases whose licence keeps their amounts
    S.Set Text ->
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
buildSupplyChainFilter refusing presets nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam edges = do
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
            , Service.scfWithheld = refusing
            }

buildFlowEntry :: Database -> MethodTables -> UUID -> FlowCFEntry
buildFlowEntry db tables uuid =
    let mFlow = M.lookup uuid (dbBioFlows db)
        -- The entry this flow actually scores with: the read-side lookup, so
        -- a flow reached via a medium-level or CAS-bridge fallback (no
        -- single build-side CF resolved to it) still reports as covered –
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

cfToAPI :: (MethodCF -> FactorReading) -> MethodCF -> MethodFactorAPI
cfToAPI reading cf =
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
        , mfaReading = reading cf
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

{- | Why a method UUID named no single method. Two cases because they are two
different answers: nothing to find (404), or a choice the caller has to make
(409), which the message names.
-}
data MethodRefusal
    = MethodNotFound Text
    | MethodAmbiguous Text
    deriving (Eq, Show)

methodRefusalMessage :: MethodRefusal -> Text
methodRefusalMessage = \case
    MethodNotFound msg -> msg
    MethodAmbiguous msg -> msg

{- | Choose the (collection, method) for a UUID from the loaded set, optionally
restricted to a named collection. A method's engine UUID is a UUIDv5 of its
name, so the *same* UUID can be loaded under several collections (e.g. two EF
3.1 versions). Resolving must therefore be loud, not first-match:

  * @Just c@   -- resolve within collection @c@; a UUID is unique inside one
                 collection, so this is unambiguous (or a not-found error).
  * @Nothing@  -- infer. One match resolves; more than one is reported as an
                 error listing the collections to choose from, rather than
                 silently picking whichever loaded first.

Pure so the disambiguation is total and testable without a 'DatabaseManager'.
The REST routes and the MCP tools both resolve through it.
-}
selectMethod :: Maybe Text -> UUID -> [(Text, Method)] -> Either MethodRefusal (Text, Method)
selectMethod mCollection uuid loaded =
    case filter keep loaded of
        [] -> Left (MethodNotFound notFound)
        [hit] -> Right hit
        hit : _ -> maybe (Left (MethodAmbiguous ambiguous)) (const (Right hit)) mCollection
  where
    keep :: (Text, Method) -> Bool
    keep (col, m) = methodId m == uuid && maybe True (== col) mCollection
    uuidText :: Text
    uuidText = UUID.toText uuid
    notFound :: Text
    notFound = case mCollection of
        Nothing -> "Method not found: " <> uuidText
        Just c ->
            "Method "
                <> uuidText
                <> " not found in collection '"
                <> c
                <> "'. Loaded collections: "
                <> T.intercalate ", " (nub (map fst loaded))
    ambiguous :: Text
    ambiguous =
        "Method UUID "
            <> uuidText
            <> " is loaded in multiple collections: "
            <> T.intercalate ", " (nub [col | (col, m) <- loaded, methodId m == uuid])
            <> ". Pass 'collection' to disambiguate."

{- | Look a method up by UUID, in the named collection or, without one, in the
only collection that carries it. The collection name comes back with the
method because it keys the per-method CF caches.
-}
loadMethodByUUID :: Maybe Text -> Text -> AppM (DM.CollectionName, Method)
loadMethodByUUID mCollection uuidText = do
    dbManager <- asks aeDbManager
    loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
    uuid <- maybe (throwError err400{errBody = "Invalid method UUID format"}) pure (UUID.fromText uuidText)
    either refuse (pure . first DM.CollectionName) (selectMethod mCollection uuid loadedMethods)
  where
    refuse :: MethodRefusal -> AppM a
    refuse = \case
        MethodNotFound msg -> throwError err404{errBody = BSL.fromStrict (T.encodeUtf8 msg)}
        MethodAmbiguous msg -> throwError err409{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

{- | Resolve a method by UUID *within a named collection*, which the route
requires rather than infers. This guarantees the
method's CFs belong to @collectionName@ – required wherever the result keys a
collection-scoped cache, so a method's factors and its cache slot never disagree.
-}
loadMethodInCollection :: DM.CollectionName -> Text -> AppM Method
loadMethodInCollection collectionName uuidText = do
    (methods, _) <- loadCollection collectionName
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
(revision 105: the @weighs@ of a scoring set, whether its scores rest on
value choices, and the @readingNotice@ of a batch scored under one that does;
revision 104: the @by@ of each line of a database's or a method collection's
history, the public name of the account that made it;
revision 103: the method collections an engine offers to download, at
@method-collections/available@, and the route that installs one of them and
loads it;
revision 102: the @basis@ of a score on its contributing flows and activities
and on the baseline of a sensitivity sweep;
revision 101: the @flows_from@ of an uploaded workbook of factors, the EcoSpold 2
database whose master data gives it its units and CAS numbers;
revision 100: the @database@ each line of a database's history was made on,
its own or the parent a child is divided from, whose changes the child carries;
revision 99: the @basis@ of a score, what it rests on: the engine, the
databases read and how each differs from its release, the method collection,
and what the request substituted and set;
revision 98: the @parameters@ a system model sets, each with its value, its
reason and the processes of the parent it moves, and the method parameters
the row of a child's delivery sheet names in its rule;
revision 97: the @probableMisses@ of a score, the flows it read no factor for
although its method writes their substance elsewhere in their medium;
revision 96: the row of a child's delivery sheet for one process, the
parent's process it was cut from, its share and its rule, at
@GET /db/{db}/activity/{processId}/division@;
revision 95: the @usage@ clause of an own licence and of the terms a reader
accepts, and the @counted@ releases of a usage line, each with what its
publisher sees of the reader;
revision 94: describing an uploaded database, or clearing its description,
at @PUT /db/{db}/description@;
revision 93: the @rows@ of a reference table, and the download of a
compartment mapping;
revision 92: renaming an uploaded database, the name it is shown under and
not its slug, at @PUT /db/{db}/display-name@;
revision 91: the @substitutions@ a system model lists, each coproduct it
credits against a process of the parent rather than divides;
revision 90: the @added@ moment a database, a method collection or a
reference table lists, when the engine made it;
revision 89: the @setWetMasses@ an edit of an activity's exchanges takes,
which states the wet mass of a product output, its density when the product is
in litres or cubic metres, and the @wetMassesSet@ its answer gives;
revision 88: the @SameCutNames@ rung of a comparison of two databases, which
pairs names cut at 80 characters with their geography, as SimaPro cuts them;
revision 87: the @setDryMasses@ an edit of an activity's exchanges takes,
which states the dry mass of a product output, and the @dryMassesSet@ its
answer gives;
revision 86: the @unchanged@ list of a comparison of two databases, every
pair the cascade made that says the same thing on both sides, with its rung;
revision 85: the body of the @derive@ route, which makes a parent (@parent@) or
a child divided by a system model (@systemModel@) and no longer reads a source
under a property, the @system-model@ route, the @systemModel@ a database status
carries, and the @allocationBy@ an activity summary carries;
revision 84: the @setPrices@ an edit of an activity's exchanges takes,
which prices a product output, and the @pricesSet@ its answer gives;
revision 83: the @none@ allocation key, which keeps a block of several
products whole;
revision 82: the @price@ among the @properties@ of a technosphere exchange,
stated or deduced from a declared share, and the @price@ allocation key;
revision 79: the reason a write may give in @note@, the history of a
database's changes, the @modification@ of a database's status, and the refusal
of a modified database's export without its changes report;
revision 78: the terms of use a licence asks its readers to accept, the @accept@ and
@acceptance@ fields of a licence, and the refusal of a database whose terms
are missing;
revision 77: the scenarios a database ships, marked @shipped@ in its scenario listing;
revision 76: the @selector@ of each exchange of an activity, and the selectors
naming an input or a waste output by its flow and location;
revision 75: the scenarios of a database, listed, saved and dropped;
revision 74: the parameters route of a database, read and edited;
revision 73: the @setParameters@, @setFormulas@ and @removeParameters@ an
edit of an activity's exchanges takes, and the @formulasSet@,
@parametersSet@, @parametersRemoved@ and @restated@ its answer gives;
revision 72: the formula-language route, the formula language with its
functions, operators and rules, and the formula-check route, a formula's value
where it would be written;
revision 71: the @ClaimByNameAlone@ supplier claim, a supplier activity
named with no product;
revision 70: the @exchange-contributions@ flag of an activity's scores, what
each line adds to each score, and the @position@ of each exchange of an
activity;
revision 69: the reduced-model route, the scores of an activity as a function
of some of its parameters;
revision 68: the @parameterSets@ of a batch score, and the @parameterSet@ of
each row;
revision 67: the @flow@ and @limit@ of an inventory request, and the
@matchedFlows@ its answer counts;
revision 66: the @parameters@ of a scoring, inventory or supply chain
request, values the database is read at;
revision 65: the @parameters@ and @databaseParameters@ of an activity,
with the @uncertainty@ of an input parameter;
revision 64: the @formula@ an exchange carries, filled from EcoSpold 2;
revision 63: the @waterBalance@ check of the quality report;
revision 61: the export format @openlca@, a database written as an openLCA
package; revision 60: the @apply-changes@ route, which makes an activity say each
change a comparison wrote that it does not say yet;
revision 59: the database format @openLCA package@;
revision 58: results carry @cutoffInputs@, the unsupplied inputs their
calculation counted as zero; a database with unresolved products is computed
instead of refused;
revision 57: the @changes-present@ route, which says whether an activity
already holds each change a comparison wrote, and the @DescriptionChanged@
and @SupplierChanged@ a comparison now reports;
revision 56: the @setName@, @setLocation@ and @setDescription@ an exchange
edit accepts, restating the texts of an activity under the identity it has,
and the @textsSet@ its answer names them in;
revision 55: the @flow/{flowId}/factors@ route and its @get_flow_factors@
tool, every factor the loaded collections give one flow, and the @methodId@
an explain-cf answer carries;
revision 54: the @heldAs@ a usage line carries, the activity name, product
name and location of the process in its database;
revision 53: the usage log, read with @usage?after=@ and forgotten with
@DELETE usage@;
revision 52: the @default@ a method collection list names;
revision 51: the @collection@ and @factorCount@ each method of the
@list_methods@ tool carries, and a failed load, copy or upload answered with
an HTTP error;
revision 50: a package uploaded as one, its licence and the releases it
requires read from its description, the @requiredReleases@ a setup carries,
and the route that accepts a database in place of one;
revision 49: the @package@ a database export accepts, @ro-crate@ to receive
it with the description of its licence and of the releases it links to;
revision 48: the @release@ a database's status and setup carry, the
@systemModels@ its setup lists, and the route that declares it;
revision 47: the edges a supply chain draws from one database to another,
and the @withheldInputs@ that say which listed process buys from a database
whose licence keeps those edges;
revision 46: the @withheldDatabases@ a dependency's licence groups its part in;
revision 45: reading a collection's scoring sets as rows and changing
them, and the @scoringRows@ an activity's score carries;
revision 44: the @withheld@ of an activity, a score and a batch of scores,
and the inventory and contributions a database's licence refuses answered
with a 403;
revision 43: adding, renaming, changing the unit of and removing a method
collection's categories, and the @methodId@ a change answers with;
revision 42: copying a method collection, changing, adding and removing its
factors, undoing a change and reading its history, the flows a collection
characterizes; the @flowRef@ of a compared factor and the @methodId@ of a
compared category; the @source@ of a listed collection;
revision 41: the @licence@ of a database's setup and of its status in the
list, standard, own or unstated, with the permissions it settles; the route
setting it, @db/{name}/licence@, and the one listing the standard licences,
@licences@;
revision 40: the @files@ a literature entry lists and the route serving
them, @db/{name}/files/{path}@;
revision 39: @process@ on the activity search, the rows of the processes a
caller names, in its order;
revision 38: the catalogue route, every process of a database page by page,
and its fingerprint;
revision 37: the single score a SimaPro method's normalization-weighting set
gives in @scoringResults@, and the @units@ of the variables a scoring set lists;
revision 36: the @documentation@ of a database's setup, the export its file
is and the system descriptions and literature references it holds;
revision 35: the @dates@ an activity carries, the days its dataset says it
was created, last revised, or written without saying which, and the
@DatesChanged@ entry a comparison of two activities reports when they differ,
and the @redated@ list of a comparison of two databases, the pairs that
differ by their dates alone;
revision 34: the @reading@ each factor of a method carries, the medium and the
location it is read at;
revision 33: the profile_method_collection route, what each impact category
of a loaded method collection holds;
revision 32: the @category@ a comparison of two method collections can be
narrowed to, comparing that one pair alone;
revision 31: the compare_method_collections route, which pairs the impact
categories and the characterization factors of two loaded method collections;
revision 30: the @max_batch_activities@ and @max_top_flows@ the hosting
route reports, so a client can size a scoring request before it is refused;
revision 29: the @collection@ query parameter the method routes take, and
the 409 they answer, naming the collections, for a method UUID several loaded
collections carry, where they used to answer from whichever loaded first;
revision 28: the contributing flows and activities of a single score, under
@score/{set}/{score}@, whose flow rows carry no match kind: a single score
weighs several factors into one, so there is no one way it was found;
revision 27: the @regional@ match kind a contributing flow can carry, naming a
factor the method states per location, where the cascade recorded no rung and
the absent field used to say no factor reached the flow at all;
revision 26: the @supplierActivity@ a gap entry and a missing supplier
carry, the activity the inputs named, the @locations@ a missing supplier
carries, the locations those inputs stated, and that list naming a product
once per activity and reason; a reference by identifier names neither;
revision 25: the compare_activities and compare_databases routes;
revision 24: the @reasons@ a gap entry carries, every reason its product was
refused a supplier under, where @reason@ and @detail@ name only the first, and
the setup report's missing-supplier list naming a product once per reason;
revision 23: the @supplierClaim@ an exchange carries, saying how its source
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
currentWireVersion = 105

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
            , hiMaxBatchActivities = Config.hcMaxBatchActivities hc
            , hiMaxTopFlows = Config.hcMaxTopFlows hc
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
            , hiMaxBatchActivities = Nothing
            , hiMaxTopFlows = Nothing
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

{- | The usage log after a collector's cursor. 404 on an engine that keeps
none, so a collector can tell "nothing yet" from "never".
-}
getUsageHandler :: Maybe Int -> AppM UsagePage
getUsageHandler after = do
    lg <- requireUsageLog
    liftIO (readUsage lg (fromMaybe 0 after))

{- | Forget the lines a collector kept. Not a change to the engine's data, so
a read-only engine answers it too; 409 when the cursor is from another start.
-}
forgetUsageHandler :: BootId -> Int -> AppM NoContent
forgetUsageHandler boot through = do
    lg <- requireUsageLog
    liftIO (forgetUsage lg boot through)
        >>= either (\msg -> throwError err409{errBody = BSL.fromStrict (T.encodeUtf8 msg)}) (const (pure NoContent))

requireUsageLog :: AppM UsageLog
requireUsageLog =
    asks aeUsageLog
        >>= maybe (throwError err404{errBody = "This engine keeps no usage log: usage_log is off in its [server] configuration"}) pure

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
getActivityInfo dbName processId = counted Reading dbName (ProcessKey processId) $ do
    (db, _) <- requireDatabaseByName dbName
    licence <- DBHandlers.servedLicence dbName
    result <- either throwServiceError pure (Service.getActivityInfo db processId)
    case fromJSON result of
        Success activityInfo -> return (Service.withholdExchangeAmounts dbName licence activityInfo)
        Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

{- | The row of a child's delivery sheet for one process: null for a process
its model left as the parent has it, or a database read from its files. A 400
says why the model cannot be read against the parent as it is now loaded: the
parent unloaded since the child was divided, or edited so that a process the
model names is gone. Refused under a licence that keeps the inventory, since
the share is the ratio of the block's amounts.
-}
getProcessDivision :: Text -> Text -> AppM (Maybe ProcessDivisionAPI)
getProcessDivision dbName processId = counted Reading dbName (ProcessKey processId) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    ld <- liftIO (getDatabase dbManager dbName) >>= maybe (throwError err404{errBody = databaseNotLoadedBody dbName}) pure
    (pid, _) <- either throwServiceError pure (Service.resolveActivityAndProcessId (ldDatabase ld) processId)
    ref <- maybe (throwError err404{errBody = "Process out of range: " <> BSL.fromStrict (T.encodeUtf8 processId)}) pure (processIdToRef (ldDatabase ld) pid)
    traced <- liftIO (DM.processTraced dbManager ld (prActivity ref, prProduct ref))
    either (\err -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 err)}) (pure . fmap division) traced
  where
    division :: (DatabaseConfig, Traced) -> ProcessDivisionAPI
    division (parent, traced) =
        ProcessDivisionAPI
            { pdaParentDatabase = dcName parent
            , pdaParentName = dcDisplayName parent
            , pdaParentProcessId = processRefText (uncurry ProcessRef (trParent traced))
            , pdaParentActivity = trActivity traced
            , pdaShare = trShare traced
            , pdaRule = tracedRule traced
            }

getActivityFlows :: Text -> Text -> AppM [FlowSummary]
getActivityFlows dbName processId = counted Reading dbName (ProcessKey processId) $ do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityFlowSummaries db activity

getActivityInputs :: Text -> Text -> AppM [ExchangeDetail]
getActivityInputs dbName processId = counted Reading dbName (ProcessKey processId) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityInputDetails db activity

getActivityOutputs :: Text -> Text -> AppM [ExchangeDetail]
getActivityOutputs dbName processId = counted Reading dbName (ProcessKey processId) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
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
getActivityTree dbName processId = counted Reading dbName (ProcessKey processId) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    maxTreeDepth <- asks aeMaxTreeDepth
    (db, _) <- requireDatabaseByName dbName
    root <- either throwServiceError pure (Service.resolveActivityAndProcessId db processId)
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    return $ Service.convertToTreeExport db maxTreeDepth (buildLoopAwareTree unitCfg db maxTreeDepth root)

{- | Inventory with optional substitutions; goes through the cross-DB
back-substitution path so dep-DB inventories merge into the response.
@keep@ picks the flows the reader asked for.
-}
activityInventoryCore :: Text -> Text -> (InventoryExport -> InventoryExport) -> Maybe SubstitutionRequest -> AppM InventoryExport
activityInventoryCore dbName processIdText keep mSub = do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    withRequestDatabase dbName mSub $ \db sharedSolver -> do
        (processId, activity) <- resolveOrThrow db processIdText
        sol <- crossDBSolutionFor dbName db sharedSolver processId mSub
        liftIO (Impact.inventoryRefusal dbManager sol) >>= mapM_ (\msg -> throwError err403{errBody = BSL.fromStrict (T.encodeUtf8 msg)})
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        cutoffs <- liftIO (Impact.solutionCutoffs dbManager ReadInventory sol)
        pure (keep (Service.convertToInventoryExport db mFlows mUnits processId activity cutoffs (SharedSolver.csInventory sol)))

-- | The flows of an inventory a request's @flow@ and @limit@ keep, or a refusal of a negative limit.
inventoryKept :: Maybe Text -> Maybe Int -> AppM (InventoryExport -> InventoryExport)
inventoryKept mFlow mLimit
    | maybe False (< 0) mLimit = badRequest "limit must be zero or more"
    | otherwise = pure (Service.keepInventoryFlows mFlow mLimit)

getActivityInventory :: Text -> Text -> Maybe Text -> Maybe Int -> AppM InventoryExport
getActivityInventory dbName processIdText mFlow mLimit = counted Inventorying dbName (ProcessKey processIdText) $ do
    keep <- inventoryKept mFlow mLimit
    activityInventoryCore dbName processIdText keep Nothing

getActivityGraph :: Text -> Text -> Maybe Double -> AppM GraphExport
getActivityGraph dbName processId maybeCutoff = counted Reading dbName (ProcessKey processId) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
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
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    presets <- asks aeClassificationPresets
    withRequestDatabase dbName mSub $ \db sharedSolver -> do
        let edges = if fromMaybe False includeEdgesParam then Service.WithEdges else Service.EntriesOnly
        refusing <- liftIO (DM.refusingDatabases dbManager ReadInventory)
        scf <-
            either badRequest pure $
                buildSupplyChainFilter
                    refusing
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
        case maybe [] srSubstitutions mSub of
            [] -> do
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                result <- liftIO $ Service.getSupplyChain unitCfg (DM.managerGeographies dbManager) (DM.mkDepSolverLookup dbManager) (DM.getGapIndex dbManager) db dbName sharedSolver processIdText scf
                either throwServiceError pure result
            subs -> do
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
                            subs
                case scalingResult of
                    Left err -> throwServiceError err
                    Right (scalingVec, virtualLinks) -> do
                        eResp <-
                            liftIO $
                                Service.buildSupplyChainFromScalingVectorCrossDB
                                    unitCfg
                                    (DM.managerGeographies dbManager)
                                    (DM.mkDepSolverLookup dbManager)
                                    (DM.getGapIndex dbManager)
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
    counted Reading dbName (ProcessKey processIdText) $
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
getActivityAggregate dbName processId scopeParam isInputParam maxDepthParam fnameParam fnameNotParam funitParam presetParam fclassParams ftargetParam fconsumerParam fconsumerNotParam fexchangeTypeParam freferenceParam groupByParam aggregateParam = counted Inventorying dbName (ProcessKey processId) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
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
    refusing <- liftIO (DM.refusingDatabases dbManager ReadInventory)
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
                , Agg.apWithheld = refusing
                }
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    result <- liftIO $ Agg.aggregate unitCfg (DM.managerGeographies dbManager) mFlows mUnits db dbName sharedSolver (DM.mkDepSolverLookup dbManager) (DM.getGapIndex dbManager) processId params
    either throwServiceError pure result

{- | LCIA single-method core. GET passes a top-flows param and logs;
POST carries substitutions or parameter values instead and skips logging.
-}
activityLCIACore :: Text -> Text -> DM.CollectionName -> Text -> Maybe Int -> Maybe SubstitutionRequest -> AppM LCIAResult
activityLCIACore dbName processIdText collectionName methodIdText topFlowsParam mSub = do
    dbManager <- asks aeDbManager
    withRequestDatabase dbName mSub $ \db sharedSolver -> do
        method <- loadMethodInCollection collectionName methodIdText
        (processId, activity) <- resolveOrThrow db processIdText
        sol <- crossDBSolutionFor dbName db sharedSolver processId mSub
        ls <- liftIO (Impact.licencedSolution dbManager SeeDetailedScores sol)
        cutoffs <- liftIO (Impact.licencedCutoffs dbManager ls)
        resultE <- liftIO $ computeCategoryResult dbManager dbName collectionName db ls cutoffs activity (fromMaybe 5 topFlowsParam) Nothing method
        result <- either scoringError pure resultE
        when (isNothing mSub) $ liftIO $ logLCIAResult result method
        licence <- DBHandlers.servedLicence dbName
        basis <- liftIO (activityBasis dbManager collectionName sol mSub)
        pure (Service.withholdMisses dbName licence (Service.withholdContributors dbName licence result)){lrBasis = Just basis}

getActivityLCIA :: Text -> Text -> DM.CollectionName -> Text -> Maybe Int -> AppM LCIAResult
getActivityLCIA dbName processIdText collectionName methodIdText topFlowsParam =
    counted Scoring dbName (ProcessKey processIdText) $
        activityLCIACore dbName processIdText collectionName methodIdText topFlowsParam Nothing

postActivityLCIA :: Text -> Text -> DM.CollectionName -> Text -> SubstitutionRequest -> AppM LCIAResult
postActivityLCIA dbName processIdText collectionName methodIdText subReq =
    counted Scoring dbName (ProcessKey processIdText) $
        activityLCIACore dbName processIdText collectionName methodIdText Nothing (Just subReq)

{- | Sensitivity sweep: rank-1 perturbations on the root scaling, scored
through the cross-DB graph (regional CFs on dep DBs still apply).
-}
postActivitySensitivity :: Text -> Text -> DM.CollectionName -> Text -> SensitivityRequest -> AppM SensitivityResponse
postActivitySensitivity dbName processIdText collectionName methodIdText senReq = counted Scoring dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted SeeDetailedScores dbName
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
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
                        ls <- Impact.licencedSolution dbManager SeeDetailedScores sol
                        cutoffs <- Impact.licencedCutoffs dbManager ls
                        eLcia <- computeCategoryResult dbManager dbName collectionName db ls cutoffs activity 5 Nothing method
                        pure $ case eLcia of
                            Left err -> PerturbedEntry p (Left err)
                            Right lcia -> PerturbedEntry p (Right (lcia, lrScore lcia - lrScore baselineLcia))
    eBaselineSol <- liftIO $ scaleToSolution baselineX
    baselineSol <-
        either
            (\err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err})
            pure
            eBaselineSol
    baselineLs <- liftIO (Impact.licencedSolution dbManager SeeDetailedScores baselineSol)
    baselineCutoffs <- liftIO (Impact.licencedCutoffs dbManager baselineLs)
    eBaselineLcia <-
        liftIO $
            computeCategoryResult dbManager dbName collectionName db baselineLs baselineCutoffs activity 5 Nothing method
    baselineLcia <- either scoringError pure eBaselineLcia
    perturbed <-
        liftIO $
            mapConcurrently (buildEntry baselineLcia) perResults
    basis <- liftIO (activityBasis dbManager collectionName baselineSol Nothing)
    pure
        SensitivityResponse
            { srBaseline = baselineLcia{lrBasis = Just basis}
            , srPerturbed = perturbed
            , srCutoffInputs = cutoffShown baselineCutoffs
            , srWithheldCutoffs = cutoffWithheld baselineCutoffs
            }

{- | A process's scores as a function of the parameters named, for a program to
compute at any of their values without asking again ('ReducedModel' says how).
It shows the amounts and formulas of the processes they move, so their database
must grant reading its inventory.
-}
postReducedModel :: Text -> Text -> DM.CollectionName -> Maybe Bool -> ReducedModelRequest -> AppM ReducedModel
postReducedModel dbName processIdText collectionName mExcludeLT ReducedModelRequest{rmqParameters = names} = counted Scoring dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    when (null names) $ badRequest "Name at least one parameter: the model is a function of those."
    dbManager <- asks aeDbManager
    (db, solver) <- requireDatabaseByName dbName
    (root, _) <- resolveOrThrow db processIdText
    (methods, scoringSets) <- loadCollection collectionName
    unitCfg <- liftIO (getMergedUnitConfig dbManager)
    free <- either throwServiceError pure (traverse (\ParameterName{pnProcess = process, pnName = name} -> (,name) <$> traverse (fmap fst . Service.resolveActivityAndProcessId db) process) names)
    moving <- either (either (badRequest . describeParameterRefusal) unprocessable) pure (RM.columns unitCfg db root free)
    let suppliers = S.toList (S.fromList [s | (_, column) <- moving, (s, _) <- RM.columnTechnosphere column])
    maybe (pure ()) badRequest (beyondModelBounds (length moving) (length suppliers))
    let ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
    atDefaults <- liftIO . Impact.withLongTermPolicy dbManager ltMode =<< solutionWithDeps dbName db solver root
    let frame = SupplierFrame{sfName = dbName, sfDatabase = db, sfSolver = solver, sfCollection = collectionName, sfMethods = methods, sfLongTerm = ltMode, sfRoot = atDefaults}
    baseline <- methodScores frame atDefaults
    factorsAt <- emissionFactors frame
    answered <- supplierAnswers frame (map (movingProcess . fst) moving) suppliers
    let modelFrame = ModelFrame{mfDatabase = db, mfScaling = reachedScaling (NE.head (SharedSolver.csScalings atDefaults)), mfSuppliers = M.fromList [(s, (i, saConversion a)) | (i, (s, a)) <- zip [0 ..] answered], mfFactors = factorsAt}
    either unprocessable pure $ do
        processes <- traverse (modelProcess modelFrame) moving
        sets <- catMaybes <$> traverse (modelScoringSet methods) scoringSets
        parameters <- zipWithM (freeParameter db) names free
        pure
            ReducedModel
                { rdmMethods = [ReducedMethod (methodId m) (methodName m) (methodUnit m) | m <- methods]
                , rdmExcludeLongTerm = ltMode == ExcludeLongTerm
                , rdmBaseline = baseline
                , rdmParameters = parameters
                , rdmProcesses = processes
                , rdmSuppliers = [ModelSupplier (supplierText db s) (supplierDatabase dbName s) (saScores a) (saScalings a) | (s, a) <- answered]
                , rdmScoringSets = sets
                }

postFormulaCheck :: Text -> FormulaCheckRequest -> AppM FormulaCheckResponse
postFormulaCheck dbName request = do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    (db, _) <- requireDatabaseByName dbName
    unitCfg <- liftIO . getMergedUnitConfig =<< asks aeDbManager
    either throwServiceError pure (Service.checkFormulaAt unitCfg db request)

getDatabaseParameters :: Text -> AppM [ParameterAPI]
getDatabaseParameters dbName = do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    (db, _) <- requireDatabaseByName dbName
    pure (Service.parametersForAPI (dbParameters db))

unprocessable :: Text -> AppM a
unprocessable msg = throwError err422{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

-- | What the suppliers of a root's lines are scored against: its database, the methods, the long-term mode, and the root's own solution.
data SupplierFrame = SupplierFrame
    { sfName :: !Text
    , sfDatabase :: !Database
    , sfSolver :: !SharedSolver
    , sfCollection :: !DM.CollectionName
    , sfMethods :: ![Method]
    , sfLongTerm :: !LongTermMode
    , sfRoot :: !SharedSolver.CrossDBSolution
    }

-- | Each method's score of a solution, in the order of the methods.
methodScores :: SupplierFrame -> SharedSolver.CrossDBSolution -> AppM [Double]
methodScores SupplierFrame{sfName = dbName, sfDatabase = db, sfCollection = collectionName, sfMethods = methods} sol = do
    dbManager <- asks aeDbManager
    byId <- liftIO (batchedScoresFor dbManager dbName collectionName db sol methods)
    either scoringError pure (traverse (`resolveBatchedScore` byId) methods)

-- | Each method's factor on an emission of the root's database, by its row and the process emitting it, as the root's score reads it.
emissionFactors :: SupplierFrame -> AppM (Int32 -> ProcessId -> [Double])
emissionFactors SupplierFrame{sfDatabase = db, sfCollection = collectionName, sfMethods = methods, sfLongTerm = ltMode, sfRoot = root} = do
    dbManager <- asks aeDbManager
    unitCfg <- liftIO (getMergedUnitConfig dbManager)
    tables <- liftIO (buildPerDbSetTables dbManager collectionName (SharedSolver.csScalings root) methods)
    (mFlows, mUnits) <- liftIO (DM.getMergedFlowMetadata dbManager)
    either scoringError pure (Method.Mapping.rowFactorsOf unitCfg mUnits mFlows ltMode db ((\(_, _, t) -> t) <$> tables) (map methodId methods))

{- | What one more unit of each supplier gives, with the scaling it solves to of
each of these processes of the root's database. A supplier no loaded database
holds adds nothing, and is left out.

Each supplier is solved 64 at a time, and its solution is given the databases
of the root's, with nothing in those it does not reach: a method is then scored
on the path the root's score takes.
-}
supplierAnswers :: SupplierFrame -> [ProcessId] -> [RM.Supplier] -> AppM [(RM.Supplier, SupplierAnswer)]
supplierAnswers frame@SupplierFrame{sfName = dbName, sfDatabase = db, sfSolver = solver, sfLongTerm = ltMode, sfRoot = root} pids suppliers = do
    dbManager <- asks aeDbManager
    unitCfg <- liftIO (getMergedUnitConfig dbManager)
    let depLookup = DM.mkDepSolverLookup dbManager
        rootShape = SharedSolver.csScalings root

        -- One more unit of each of a database's suppliers: the scores, and the scaling of that database it solves to.
        solved :: Database -> Text -> SharedSolver -> Int -> [ProcessId] -> AppM [([Double], U.Vector Double)]
        solved sdb name ssolver depth supplied = fmap concat . forM (Matrix.chunksOf 64 supplied) $ \chunk -> do
            sols <- either unprocessable pure =<< liftIO (SharedSolver.goWithDeps unitCfg depLookup sdb name ssolver (map (unitDemand sdb) chunk) depth)
            forM sols $ \sol -> do
                scores <- methodScores frame =<< liftIO (Impact.withLongTermPolicy dbManager ltMode (shapedLike rootShape sol))
                pure (scores, reachedScaling (NE.head (SharedSolver.csScalings sol)))

        answersIn :: Text -> [RM.Supplier] -> AppM [(RM.Supplier, SupplierAnswer)]
        answersIn name group
            | name == dbName = do
                let within = [p | RM.Within p <- group]
                got <- solved db dbName solver 0 within
                pure [(RM.Within p, SupplierAnswer 1 scores [Matrix.processScaling db v q | q <- pids]) | (p, (scores, v)) <- zip within got]
            | otherwise =
                liftIO (depLookup name) >>= \case
                    Nothing -> pure []
                    Just (depDb, depSolver) -> do
                        found <-
                            either unprocessable (pure . concat) . sequence $
                                [ fmap (\(p, conversion) -> (s, p, conversion)) . maybeToList <$> Matrix.inSupplierUnit unitCfg name depDb (uncurry ProcessRef key) (StatedAmount unit 1)
                                | s@(RM.Elsewhere _ key unit) <- group
                                ]
                        got <- solved depDb name depSolver 1 [p | (_, p, _) <- found]
                        pure [(s, SupplierAnswer conversion scores (map (const 0) pids)) | ((s, _, conversion), (scores, _)) <- zip found got]
    concat <$> traverse (uncurry answersIn) (M.toList (NE.toList <$> groupInOrder [(supplierDatabase dbName s, s) | s <- suppliers]))

-- | What one more unit of a supplier gives: the factor to the unit its lines state, its scores, and each moving process's scaling.
data SupplierAnswer = SupplierAnswer
    { saConversion :: !Double
    , saScores :: ![Double]
    , saScalings :: ![Double]
    }

-- | What the moving processes are written against: the suppliers by their position and conversion, and each emission's factors.
data ModelFrame = ModelFrame
    { mfDatabase :: !Database
    , mfScaling :: !(U.Vector Double)
    , mfSuppliers :: !(M.Map RM.Supplier (Int, Double))
    , mfFactors :: !(Int32 -> ProcessId -> [Double])
    }

-- | The refusal of a model too large to send, if it is.
beyondModelBounds :: Int -> Int -> Maybe Text
beyondModelBounds processes suppliers
    | processes > RM.maxProcesses || processes * suppliers > RM.maxEntries =
        Just $
            T.concat
                [ "These parameters move "
                , T.pack (show processes)
                , " processes, supplied by "
                , T.pack (show suppliers)
                , ": a model holds at most "
                , T.pack (show RM.maxProcesses)
                , " processes, and "
                , T.pack (show RM.maxEntries)
                , " for the two counts multiplied. Name fewer parameters."
                ]
    | otherwise = Nothing

-- | One unit of a process's product, as a demand on its database.
unitDemand :: Database -> ProcessId -> Matrix.Demand
unitDemand db p = Matrix.Demand (U.generate (fromIntegral (dbActivityCount db)) (\c -> if c == fromIntegral (dbActivityIndex db V.! fromIntegral p) then 1 else 0))

{- | A supplier's solution given the databases of the root's, the root first and
at zero where the supplier does not reach it, so it is scored on the root's path.
-}
shapedLike :: NonEmpty Reached -> SharedSolver.CrossDBSolution -> SharedSolver.CrossDBSolution
shapedLike rootShape sol =
    sol{SharedSolver.csScalings = NE.appendList headed [SharedSolver.atZero r | r <- NE.toList rootShape, S.notMember (reachedName r) seen]}
  where
    root :: Reached
    root = NE.head rootShape

    reached, headed :: NonEmpty Reached
    reached = SharedSolver.csScalings sol
    headed = if reachedName (NE.head reached) == reachedName root then reached else SharedSolver.atZero root NE.<| reached

    seen :: S.Set Text
    seen = S.fromList (map reachedName (NE.toList headed))

supplierDatabase :: Text -> RM.Supplier -> Text
supplierDatabase rootName = \case
    RM.Within _ -> rootName
    RM.Elsewhere name _ _ -> name

supplierText :: Database -> RM.Supplier -> Text
supplierText db = \case
    RM.Within p -> processIdToText db p
    RM.Elsewhere _ (act, prod) _ -> UUID.toText act <> "_" <> UUID.toText prod

formulaTree :: Text -> Either Text Expr.Formula
formulaTree = first Expr.describeRefusal . Expr.readFormula Expr.Volca

modelLine :: Double -> Maybe Text -> Either Text ModelLine
modelLine stored formula = ModelLine stored <$> traverse formulaTree formula

modelBinding :: (Text, Text) -> Either Text ModelBinding
modelBinding (name, formula) = ModelBinding (T.toLower name) <$> formulaTree formula

-- | A moving process's column, its lines written against the suppliers the model answers.
modelProcess :: ModelFrame -> (Moving, RM.Column) -> Either Text ModelProcess
modelProcess ModelFrame{mfDatabase = db, mfScaling = x0, mfSuppliers = suppliers, mfFactors = factorsAt} (process, column) = do
    bindings <- traverse modelBinding (movingBindings process)
    reference <- ModelReference <$> traverse exchangeLine outputs <*> traverse exchangeLine selfLoops <*> traverse exchangeLine inputs
    technosphere <- sequence [ModelInput i (RM.entryFactor e * conversion) <$> entryLine e | (s, e) <- RM.columnTechnosphere column, Just (i, conversion) <- [M.lookup s suppliers]]
    biosphere <- traverse (\(row, e) -> ModelEmission (factorsAt row pid) <$> entryLine e) (RM.columnBiosphere column)
    pure
        ModelProcess
            { mprProcessId = processIdToText db pid
            , mprName = activityName (dbActivities db V.! fromIntegral pid)
            , mprScaling = Matrix.processScaling db x0 pid
            , mprValues = M.fromList [(name, value) | (name, Fixed value) <- M.toList (movingInputs process)]
            , mprParameters = M.fromList [(name, i) | (name, Free i) <- M.toList (movingInputs process)]
            , mprBindings = bindings
            , mprReference = reference
            , mprTechnosphere = technosphere
            , mprBiosphere = biosphere
            }
  where
    pid = movingProcess process
    NormLines{nlOutputs = outputs, nlSelfLoops = selfLoops, nlInputs = inputs} = RM.columnReference column

    exchangeLine :: Exchange -> Either Text ModelLine
    exchangeLine ex = modelLine (exchangeAmount ex) (efText <$> exchangeFormula ex)

    entryLine :: RM.Entry -> Either Text ModelLine
    entryLine e = modelLine (RM.entryStored e) (RM.entryFormula e)

{- | A scoring set as formulas over the category totals, or nothing for a set the
engine cannot compute, which a score leaves out: whether it can does not depend
on the totals, so it is asked with each at one.
-}
modelScoringSet :: [Method] -> ScoringSet -> Either Text (Maybe ModelScoringSet)
modelScoringSet methods ss
    | Left _ <- computeFormulaScores ss (M.fromList [(methodName m, 1) | m <- methods]) = Right Nothing
    | otherwise = do
        computed <- traverse modelBinding (Expr.settledInOrder Expr.Volca (0 <$ ssVariables ss) (M.toList (ssComputed ss)))
        scores <- traverse formulaTree (ssScores ss)
        pure . Just $
            ModelScoringSet
                { mssName = ssName ss
                , mssUnit = ssUnit ss
                , mssVariables = M.fromList [(T.toLower variable, i) | (variable, category) <- M.toList (ssVariables ss), Just i <- [M.lookup category byName]]
                , mssComputed = computed
                , mssNormalization = M.mapKeys T.toLower (ssNormalization ss)
                , mssWeighting = M.mapKeys T.toLower (ssWeighting ss)
                , mssScores = scores
                , mssDisplayMultiplier = fromMaybe 1 (ssDisplayMultiplier ss)
                }
  where
    byName = M.fromList (zip (map methodName methods) [0 ..])

-- | A parameter the model is a function of, as the request named it and as its process or database defines it.
freeParameter :: Database -> ParameterName -> (Maybe ProcessId, Text) -> Either Text FreeParameter
freeParameter db ParameterName{pnProcess = processText} (process, name) =
    case Service.parametersForAPI (M.filterWithKey (\key _ -> T.toLower key == T.toLower name) defined) of
        [shown] -> Right (FreeParameter processText shown)
        _ -> Left ("parameter '" <> name <> "' is not one name")
  where
    defined = maybe (dbParameters db) (activityParameters . (dbActivities db V.!) . fromIntegral) process

getActivityLCIABatch :: Text -> Text -> DM.CollectionName -> Maybe Bool -> Maybe Bool -> AppM LCIABatchResult
getActivityLCIABatch dbName processIdText collectionName mExcludeLT mLines =
    counted Scoring dbName (ProcessKey processIdText) $
        activityLCIABatchH dbName processIdText collectionName Nothing (longTermModeFromExclude (fromMaybe False mExcludeLT)) (scoreLinesFrom mLines)

postActivityLCIABatch :: Text -> Text -> DM.CollectionName -> Maybe Bool -> Maybe Bool -> SubstitutionRequest -> AppM LCIABatchResult
postActivityLCIABatch dbName processIdText collectionName mExcludeLT mLines subReq =
    counted Scoring dbName (ProcessKey processIdText) $
        activityLCIABatchH dbName processIdText collectionName (Just subReq) (longTermModeFromExclude (fromMaybe False mExcludeLT)) (scoreLinesFrom mLines)

scoreLinesFrom :: Maybe Bool -> ScoreLines
scoreLinesFrom = \case
    Just True -> WithLines
    _ -> WithoutLines

postActivityInventory :: Text -> Text -> Maybe Text -> Maybe Int -> SubstitutionRequest -> AppM InventoryExport
postActivityInventory dbName processIdText mFlow mLimit subReq = counted Inventorying dbName (ProcessKey processIdText) $ do
    keep <- inventoryKept mFlow mLimit
    activityInventoryCore dbName processIdText keep (Just subReq)

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
    counted Reading dbName (ProcessKey processIdText) $
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
getActivityConsumers dbName processIdText nameFilter locationFilter productFilter presetParam classSystems classValues classModes limitParam offsetParam maxDepthParam sortParam orderParam includeEdgesParam = counted Reading dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
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
getActivityPathTo dbName processIdText targetParam = counted Reading dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
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

-- | One activity against another, in this database or in another loaded one.
getActivityComparison :: Text -> Text -> Maybe Text -> Maybe Text -> AppM ActivityComparison
getActivityComparison dbName processIdText otherProcessParam otherDbParam =
    counted Comparing dbName (ProcessKey processIdText)
        . maybe id (counted Comparing (fromMaybe dbName otherDbParam) . ProcessKey) otherProcessParam
        $ do
            mapM_ (DBHandlers.refuseUnlessGranted ReadInventory) (dbName : maybeToList otherDbParam)
            otherProcessId <-
                maybe
                    (throwError err400{errBody = "Missing required 'other_process_id' query parameter"})
                    pure
                    otherProcessParam
            (db, _) <- requireDatabaseByName dbName
            (otherDb, _) <- requireDatabaseByName (fromMaybe dbName otherDbParam)
            either throwServiceError (pure . Compare.compareActivities) $
                Compare.Sides
                    <$> Compare.resolveProcess db processIdText
                    <*> Compare.resolveProcess otherDb otherProcessId

{- | Whether one activity already says what each change says, the changes
written as a comparison writes them. Not a use of the activity: it answers a
question about changes, and the usage log counts what people compute.
-}
postChangesPresent :: Text -> Text -> ChangesQuery -> AppM ChangesPresence
postChangesPresent dbName processIdText query = do
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    (db, _) <- requireDatabaseByName dbName
    either throwServiceError (pure . (`Compare.changesPresent` query)) (Compare.resolveProcess db processIdText)

{- | Make one activity say each change it does not say yet, the changes
written as a comparison writes them, and say what came of each. A change the
activity already says, or says otherwise, is left as it is; one no edit can
make is reported with its reason. Every edit lands in one write, refused as a
direct edit of the activity would be.
-}
postApplyChanges :: Text -> Text -> ChangesQuery -> AppM ChangesApplied
postApplyChanges dbName processIdText query = do
    DBHandlers.guardMutation
    DBHandlers.refuseUnlessGranted ReadInventory dbName
    (db, _) <- requireDatabaseByName dbName
    process <- either throwServiceError pure (Compare.resolveProcess db processIdText)
    dbManager <- asks aeDbManager
    by <- asks aeAuthor
    ctx <- liftIO (authorContext dbManager db)
    case Compare.applicableChanges ctx process query of
        (applied, []) -> pure applied
        (applied, edits) ->
            liftIO (editExchanges dbManager dbName by (cqNote query >>= noteFrom) processIdText edits)
                >>= either DBHandlers.refuseWrite (const (pure applied))

getDatabaseComparison :: Text -> Maybe Text -> Maybe Int -> AppM DatabaseComparison
getDatabaseComparison dbName otherDbParam limitParam = do
    mapM_ (DBHandlers.refuseUnlessGranted ReadInventory) (dbName : maybeToList otherDbParam)
    otherName <-
        maybe
            (throwError err400{errBody = "Missing required 'other_database' query parameter"})
            pure
            otherDbParam
    (db, _) <- requireDatabaseByName dbName
    (otherDb, _) <- requireDatabaseByName otherName
    pure $ maybe id Compare.limitComparison limitParam (Compare.compareDatabases (Compare.Sides db otherDb))

{- | The pairs of 'getDatabaseComparison' without what differs inside them, each
with what puts the other database's score per unit of the base one's product.
No inventory is read out, so no more is asked than to score.
-}
getScorePairs :: Text -> Maybe Text -> AppM ScorePairs
getScorePairs dbName otherDbParam = do
    otherName <-
        maybe
            (throwError err400{errBody = "Missing required 'other_database' query parameter"})
            pure
            otherDbParam
    when (otherName == dbName) $
        throwError err400{errBody = "A database compared with itself says nothing; name another database"}
    (db, _) <- requireDatabaseByName dbName
    (otherDb, _) <- requireDatabaseByName otherName
    units <- liftIO . getMergedUnitConfig =<< asks aeDbManager
    pure (Compare.scorePairs units (Compare.Sides db otherDb))

-- | What a caller asks a comparison of two method collections for.
data MethodComparisonAsk = MethodComparisonAsk
    { mcaCollection :: !DM.CollectionName
    , mcaOther :: !DM.CollectionName
    , mcaPairs :: ![Text]
    , mcaCategory :: !(Maybe Text)
    , mcaLimit :: !(Maybe Int)
    }

data MethodComparisonFailure
    = CollectionMissing !Text ![Text]
    | PairsRefused !CompareMethods.CompareMethodsRefusal

-- | The reference data a comparison of method collections reads names, places and units through.
methodComparisonContext :: DatabaseManager -> IO CompareMethods.CompareMethodsContext
methodComparisonContext manager =
    CompareMethods.CompareMethodsContext
        <$> DM.getMergedSynonymDB manager
        <*> DM.getMergedCompartmentMap manager
        <*> DM.getMergedUnitConfig manager
        <*> pure (dmLocationHierarchy manager)

{- | The one reading of a comparison of two method collections, shared by the
route and the assistant tool so the two cannot answer differently.
-}
runMethodComparison :: DatabaseManager -> MethodComparisonAsk -> IO (Either MethodComparisonFailure MethodCollectionComparison)
runMethodComparison manager request = do
    loaded <- readTVarIO (dmLoadedMethods manager)
    ctx <- methodComparisonContext manager
    pure $ do
        let loadedNamed (DM.CollectionName name) = maybe (Left (CollectionMissing name (M.keys loaded))) Right (M.lookup name loaded)
        cols <- Compare.Sides <$> loadedNamed (mcaCollection request) <*> loadedNamed (mcaOther request)
        forced <- first PairsRefused (traverse CompareMethods.parseForcedPair (mcaPairs request))
        comparison <- first PairsRefused (CompareMethods.compareCollections ctx forced (maybe CompareMethods.EveryCategory CompareMethods.OneCategory (mcaCategory request)) cols)
        pure (maybe id CompareMethods.limitMethodComparison (mcaLimit request) comparison)

{- | The one reading of what a loaded method collection holds, shared by the
route and the assistant tool. 'Left' names the loaded collections when it is
not one of them.
-}
runMethodProfile :: DatabaseManager -> DM.CollectionName -> IO (Either Text MethodCollectionProfile)
runMethodProfile manager (DM.CollectionName name) = do
    loaded <- readTVarIO (dmLoadedMethods manager)
    ctx <- methodComparisonContext manager
    pure (maybe (Left (collectionNotLoadedMessage name (M.keys loaded))) (Right . CompareMethods.profileCollection ctx) (M.lookup name loaded))

getMethodCollectionProfile :: DM.CollectionName -> AppM MethodCollectionProfile
getMethodCollectionProfile collection = do
    manager <- asks aeDbManager
    liftIO (runMethodProfile manager collection)
        >>= either (\msg -> throwError err404{errBody = BSL.fromStrict (T.encodeUtf8 msg)}) pure

getMethodCollectionComparison :: DM.CollectionName -> Maybe DM.CollectionName -> [Text] -> Maybe Text -> Maybe Int -> AppM MethodCollectionComparison
getMethodCollectionComparison collection otherParam forcedPairs categoryParam limitParam = do
    other <-
        maybe
            (throwError err400{errBody = "Missing required 'other_collection' query parameter"})
            pure
            otherParam
    manager <- asks aeDbManager
    liftIO (runMethodComparison manager MethodComparisonAsk{mcaCollection = collection, mcaOther = other, mcaPairs = forcedPairs, mcaCategory = categoryParam, mcaLimit = limitParam})
        >>= either failed pure
  where
    failed :: MethodComparisonFailure -> AppM a
    failed (CollectionMissing name loaded) = throwError err404{errBody = collectionNotLoadedBody name loaded}
    failed (PairsRefused r) = throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 (CompareMethods.refusalMessage r))}

getContributingFlows :: Text -> Text -> DM.CollectionName -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingFlowsResult
getContributingFlows dbName processIdText collectionName methodIdText limitParam mExcludeLT = counted Contributing dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted SeeDetailedScores dbName
    withActivityAndMethod dbName collectionName processIdText methodIdText $ \db sharedSolver actProcessId _ method -> do
        dbManager <- asks aeDbManager
        let lim = fromMaybe 20 limitParam
            ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
        (mFlows, _) <- liftIO $ DM.getMergedFlowMetadata dbManager
        sol <-
            solutionWithDeps dbName db sharedSolver actProcessId
                >>= liftIO . Impact.withLongTermPolicy dbManager ltMode
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
        liftIO $ warnUnknownInventoryFlows ("contributing-flows " <> methodName method) mFlows (SharedSolver.csInventory sol)
        score <- liftIO (Impact.scoreSolution dbManager collectionName method tables sol) >>= either scoringError pure
        ls <- liftIO (Impact.licencedSolution dbManager SeeDetailedScores sol)
        cutoffs <- liftIO (Impact.licencedCutoffs dbManager ls)
        top <- liftIO (licencedTopFlows dbManager collectionName method tables ls score lim) >>= either scoringError pure
        basis <- liftIO (activityBasis dbManager collectionName sol Nothing)
        return
            ContributingFlowsResult
                { cfrMethod = methodName method
                , cfrUnit = methodUnit method
                , cfrTotalScore = score
                , cfrTopFlows = tfRows top
                , cfrWithheldDatabases = tfWithheld top
                , cfrCutoffInputs = cutoffShown cutoffs
                , cfrWithheldCutoffs = cutoffWithheld cutoffs
                , cfrBasis = basis
                }

getContributingActivities :: Text -> Text -> DM.CollectionName -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingActivitiesResult
getContributingActivities dbName processIdText collectionName methodIdText limitParam mExcludeLT = counted Contributing dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted SeeDetailedScores dbName
    withActivityAndMethod dbName collectionName processIdText methodIdText $ \db sharedSolver actProcessId _ method -> do
        dbManager <- asks aeDbManager
        let lim = fromMaybe 10 limitParam
            ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
        sol <-
            solutionWithDeps dbName db sharedSolver actProcessId
                >>= liftIO . Impact.withLongTermPolicy dbManager ltMode
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
        contributions <-
            liftIO (Impact.processContributionsOf dbManager collectionName method tables sol)
                >>= either scoringError pure
        withheld <- liftIO (Impact.withheldDatabases dbManager SeeDetailedScores sol)
        cutoffs <- liftIO (Impact.solutionCutoffs dbManager SeeDetailedScores sol)
        basis <- liftIO (activityBasis dbManager collectionName sol Nothing)
        liftIO $ activitiesResult dbManager dbName Heading{hdName = methodName method, hdUnit = methodUnit method, hdCutoffs = cutoffs, hdBasis = basis} lim (Impact.splitProcessParts withheld contributions)

-- | What a list of contributions answers for, as the answer names it.
data Heading = Heading
    { hdName :: Text
    , hdUnit :: Text
    , hdCutoffs :: Cutoffs
    -- ^ The cut-offs of the solution the list is read from
    , hdBasis :: ScoreBasis
    -- ^ What the score the list adds up to rests on
    }

{- | The biggest contributing activities of a score, from every activity's
part of it. The parts are the terms of the score, so their sum is it, with the
line of each dependency that keeps its detail. Those are set aside before the
ranking, so a page is never short of the processes it could show.
-}
activitiesResult ::
    DatabaseManager ->
    -- | root DB name
    Text ->
    Heading ->
    Int ->
    Impact.ProcessParts ->
    IO ContributingActivitiesResult
activitiesResult dbManager dbName heading lim Impact.ProcessParts{Impact.ppShown = contributions, Impact.ppWithheld = withheld} = do
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    rows <- mapM (mkCrossDBContrib dbManager dbName mFlows mUnits score) top
    pure
        ContributingActivitiesResult
            { carMethod = hdName heading
            , carUnit = hdUnit heading
            , carTotalScore = score
            , carActivities = rows
            , carWithheldDatabases = Service.withheldShares score withheld
            , carCutoffInputs = cutoffShown (hdCutoffs heading)
            , carWithheldCutoffs = cutoffWithheld (hdCutoffs heading)
            , carBasis = hdBasis heading
            }
  where
    score :: Double
    score = sum (M.elems contributions) + sum (map Impact.psScore withheld)

    top :: [((Text, ProcessId), Double)]
    top = take lim (sortOn (\(_, c) -> negate (abs c)) (M.toList contributions))

-- | What a single-score route is asked, as its path and query name it.
data ScoreQuery = ScoreQuery
    { sqDbName :: Text
    , sqCollection :: DM.CollectionName
    , sqProcessId :: Text
    , sqRef :: ScoreRef
    , sqExcludeLongTerm :: Maybe Bool
    }

{- | Resolve the activity and the score, solve, then dispatch. A name that
matches nothing is a 404; a score that names something but does not split into
its indicators' parts is a 422. The answer comes back under its heading: the
score's name and unit, and the cut-offs of the solution it was read from.
-}
withActivityAndScore ::
    ScoreQuery ->
    (Score.Source -> Score.ResolvedScore -> SharedSolver.CrossDBSolution -> IO (Either Score.ScoreRefusal a)) ->
    AppM (Heading, a)
withActivityAndScore ScoreQuery{sqDbName = dbName, sqCollection = collectionName, sqProcessId = processIdText, sqRef = ref, sqExcludeLongTerm = mExcludeLT} k = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    (methods, scoringSets) <- loadCollection collectionName
    rs <- either refused pure (Score.resolveScore methods scoringSets ref)
    (pid, _) <- resolveOrThrow db processIdText
    sol <-
        solutionWithDeps dbName db sharedSolver pid
            >>= liftIO . Impact.withLongTermPolicy dbManager (longTermModeFromExclude (fromMaybe False mExcludeLT))
    cutoffs <- liftIO (Impact.solutionCutoffs dbManager SeeDetailedScores sol)
    basis <- liftIO (activityBasis dbManager collectionName sol Nothing)
    liftIO (k Score.Source{srcManager = dbManager, srcDbName = dbName, srcDatabase = db, srcCollection = collectionName} rs sol)
        >>= either refused (pure . (,) (scoreHeading rs cutoffs basis))
  where
    refused :: Score.ScoreRefusal -> AppM b
    refused = \case
        Score.UnknownScore msg -> throwError err404{errBody = utf8Body msg}
        Score.Unsplittable msg -> throwError err422{errBody = utf8Body msg}
        Score.ScoringFailed msg -> scoringError msg

    utf8Body :: Text -> BSL.ByteString
    utf8Body = BSL.fromStrict . T.encodeUtf8

scoreHeading :: Score.ResolvedScore -> Cutoffs -> ScoreBasis -> Heading
scoreHeading rs cutoffs basis = Heading{hdName = Score.scoreTitle rs, hdUnit = Score.scoreUnit rs, hdCutoffs = cutoffs, hdBasis = basis}

getScoreContributingActivities :: Text -> Text -> DM.CollectionName -> Text -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingActivitiesResult
getScoreContributingActivities dbName processIdText collectionName setName scoreName limitParam mExcludeLT = counted Contributing dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted SeeDetailedScores dbName
    dbManager <- asks aeDbManager
    (heading, parts) <-
        withActivityAndScore
            ScoreQuery
                { sqDbName = dbName
                , sqCollection = collectionName
                , sqProcessId = processIdText
                , sqRef = ScoreRef{srSet = setName, srScore = scoreName}
                , sqExcludeLongTerm = mExcludeLT
                }
            ( \src score sol -> do
                withheld <- Impact.withheldDatabases dbManager SeeDetailedScores sol
                fmap (Impact.splitProcessParts withheld) <$> Score.activityParts src score sol
            )
    liftIO $ activitiesResult dbManager dbName heading (fromMaybe 10 limitParam) parts

getScoreContributingFlows :: Text -> Text -> DM.CollectionName -> Text -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingFlowsResult
getScoreContributingFlows dbName processIdText collectionName setName scoreName limitParam mExcludeLT = counted Contributing dbName (ProcessKey processIdText) $ do
    DBHandlers.refuseUnlessGranted SeeDetailedScores dbName
    (heading, Score.ScoreFlows{Score.sfTotal = total, Score.sfRows = rows, Score.sfWithheld = withheld}) <-
        withActivityAndScore
            ScoreQuery
                { sqDbName = dbName
                , sqCollection = collectionName
                , sqProcessId = processIdText
                , sqRef = ScoreRef{srSet = setName, srScore = scoreName}
                , sqExcludeLongTerm = mExcludeLT
                }
            (Score.licencedFlowParts "contributing-flows")
    pure
        ContributingFlowsResult
            { cfrMethod = hdName heading
            , cfrUnit = hdUnit heading
            , cfrTotalScore = total
            , cfrTopFlows = topContributorRows (const Nothing) total (fromMaybe 20 limitParam) rows
            , cfrWithheldDatabases = Service.withheldShares total withheld
            , cfrCutoffInputs = cutoffShown (hdCutoffs heading)
            , cfrWithheldCutoffs = cutoffWithheld (hdCutoffs heading)
            , cfrBasis = hdBasis heading
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
getMethods = map methodSummary <$> (liftIO . DM.getLoadedMethods =<< asks aeDbManager)

{- | One loaded method as both surfaces list it, named with its collection:
the same method loaded twice is two lines a reader has to tell apart.
-}
methodSummary :: (Text, Method) -> MethodSummary
methodSummary (collName, m) =
    MethodSummary
        { msmId = methodId m
        , msmName = methodName m
        , msmCategory = methodCategory m
        , msmUnit = methodUnit m
        , msmFactorCount = length (methodFactors m)
        , msmCollection = collName
        }

getMethodDetail :: Text -> Maybe Text -> AppM MethodDetail
getMethodDetail methodIdText mCollection = do
    (_, method) <- loadMethodByUUID mCollection methodIdText
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

getMethodFactors :: Text -> Maybe Text -> AppM [MethodFactorAPI]
getMethodFactors methodIdText mCollection = do
    (_, method) <- loadMethodByUUID mCollection methodIdText
    manager <- asks aeDbManager
    compartments <- liftIO (DM.getMergedCompartmentMap manager)
    return $ map (cfToAPI (CompareMethods.factorReading compartments (dmLocationHierarchy manager))) (methodFactors method)

getMethodMapping :: Text -> Text -> Maybe Text -> AppM MappingStatus
getMethodMapping dbName methodIdText mCollection = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID mCollection methodIdText
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

getFlowCFMapping :: Text -> Text -> Maybe Text -> AppM FlowCFMapping
getFlowCFMapping dbName methodIdText mCollection = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID mCollection methodIdText
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
Distinct across methods, because they overlap – every climate-change variant
characterizes the same gases – so this number cannot be recovered from the
per-method mapping statuses.
-}
getCollectionCoverage :: Text -> DM.CollectionName -> AppM CollectionCoverage
getCollectionCoverage dbName collectionName = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (methods, _) <- loadCollection collectionName
    tablesList <- liftIO $ mapM (DM.mapMethodToTablesCached dbManager dbName collectionName db) methods
    return
        CollectionCoverage
            { ccvCollection = DM.unCollectionName collectionName
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
explainCFHandler :: Text -> Text -> Text -> Maybe Text -> AppM ExplainCFResult
explainCFHandler dbName methodIdText flowIdText mCollection = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID mCollection methodIdText
    fid <- case UUID.fromText (T.strip flowIdText) of
        Nothing -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 ("Malformed flow id: " <> flowIdText))}
        Just u -> pure u
    explained <- liftIO $ DM.explainFlowFactor dbManager dbName collectionName db method fid
    case explained of
        Left err -> throwError err404{errBody = BSL.fromStrict (T.encodeUtf8 err)}
        Right (flow, explanation) ->
            pure (explainCFToAPI db method flow explanation)

{- | Every factor the loaded collections, or the one named, give one flow.

The same replay as 'explainCFHandler', for each method in turn, so a reader
asking whether a flow counts anywhere does not have to know which method to
ask, nor make one request per method.
-}
flowFactorsHandler :: Text -> Text -> Maybe Text -> AppM FlowFactorsResult
flowFactorsHandler dbName flowIdText mCollection = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    fid <- case UUID.fromText (T.strip flowIdText) of
        Nothing -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 ("Malformed flow id: " <> flowIdText))}
        Just u -> pure u
    either (\err -> throwError err404{errBody = BSL.fromStrict (T.encodeUtf8 err)}) (pure . flowFactorsToAPI db)
        =<< liftIO (DM.flowFactors dbManager dbName db mCollection fid)

{- | Project every collection's verdict on one flow onto the wire. Shared with
the MCP tool, so the two surfaces list the same methods on the same side.
-}
flowFactorsToAPI :: Database -> DM.FlowFactors -> FlowFactorsResult
flowFactorsToAPI db (DM.FlowFactors flow collections) =
    FlowFactorsResult
        { ffrFlow = explainedFlowAPI db flow
        , ffrCollections = map collectionFactors collections
        }
  where
    collectionFactors :: DM.CollectionExplanation -> CollectionFactors
    collectionFactors (DM.CollectionExplanation collection methods) =
        CollectionFactors
            { cfcCollection = DM.unCollectionName collection
            , cfcFactors = [explainCFToAPI db m flow x | DM.MethodExplanation m x <- reached]
            , cfcNoFactor = [methodSummary (DM.unCollectionName collection, m) | DM.MethodExplanation m _ <- missed]
            }
      where
        (reached, missed) = partition (Explain.reachesFlow . DM.mxExplanation) methods

getCharacterization :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe Text -> AppM CharacterizationResult
getCharacterization dbName methodIdText flowFilter limitParam mCollection = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID mCollection methodIdText
    let lim = fromMaybe 50 limitParam
        queryLower = fmap T.toLower flowFilter
    mappings <- liftIO $ DM.effectiveMethodMappings dbManager dbName collectionName db method
    let matched =
            [ (cf, f, strat)
            | (cf, Just (Resolution f strat)) <- mappings
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
    liftIO $
        MethodCollectionListResponse . map MethodEdit.methodCollectionStatusAPI
            <$> DM.listMethodCollections dbManager
            <*> DM.defaultMethodCollection dbManager

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

{- | The activity search, or with @process@ given, the rows of exactly those
processes in the order given, through the same place, product and
classification filters (see 'Service.activitiesNamed').
-}
searchActivitiesWithCount :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> [Text] -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> AppM (SearchResults ActivitySummary)
searchActivitiesWithCount dbName nameParam geoParam productParam exactParam presetParam classSystems classValues classModes processParams limitParam offsetParam sortParam orderParam = do
    presets <- asks aeClassificationPresets
    geographies <- asks (DM.managerGeographies . aeDbManager)
    (db, _) <- requireDatabaseByName dbName
    classifications <- either badRequest pure (mergeClassFilters presets presetParam classSystems classValues classModes)
    let sf =
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
                , Service.sfExactMatch = fromMaybe False exactParam
                }
    if null processParams
        then searched geographies db sf
        else either throwServiceError pure (Service.searchNamed geographies db sf processParams)
  where
    searched :: Geographies -> Database -> Service.SearchFilter -> AppM (SearchResults ActivitySummary)
    searched geographies db sf = do
        result <- liftIO $ Service.searchActivities geographies db sf
        case result of
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right jsonValue -> case fromJSON jsonValue of
                Success searchResults -> return searchResults
                Error parseErr -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack parseErr}

getClassifications :: Text -> AppM [ClassificationSystem]
getClassifications dbName = do
    (db, _) <- requireDatabaseByName dbName
    return $ Service.getClassifications db

getCatalogue :: Text -> Maybe Int -> Maybe Int -> AppM CataloguePage
getCatalogue dbName offsetParam limitParam = do
    entries <- catalogueOf dbName
    either badRequest pure (cataloguePage entries PageWindow{pwOffset = fromMaybe 0 offsetParam, pwLimit = fromMaybe catalogueDefaultLimit limitParam})

getCatalogueFingerprint :: Text -> AppM CatalogueFingerprint
getCatalogueFingerprint dbName = CatalogueFingerprint . catalogueFingerprint <$> catalogueOf dbName

-- | Units are read against the merged table, the one every other unit answer of this server uses.
catalogueOf :: Text -> AppM [CatalogueEntry]
catalogueOf dbName = do
    (db, _) <- requireDatabaseByName dbName
    unitCfg <- liftIO . getMergedUnitConfig =<< asks aeDbManager
    pure (catalogueEntries unitCfg db)

postImpactsBatch :: Text -> DM.CollectionName -> Maybe Int -> Maybe Bool -> BatchImpactsRequest -> AppM BatchImpactsResponse
postImpactsBatch dbName collectionName = scoreBatchUnder dbName collectionName Nothing

postMethodImpactsBatch :: Text -> DM.CollectionName -> Text -> Maybe Int -> Maybe Bool -> BatchImpactsRequest -> AppM BatchImpactsResponse
postMethodImpactsBatch dbName collectionName methodText = scoreBatchUnder dbName collectionName (Just methodText)

scoreBatchUnder :: Text -> DM.CollectionName -> Maybe Text -> Maybe Int -> Maybe Bool -> BatchImpactsRequest -> AppM BatchImpactsResponse
scoreBatchUnder dbName collectionName methodParam topFlowsParam mExcludeLT =
    countedEach Scoring dbName (map (ProcessKey . bieProcessId) . birResults)
        . batchImpactsH dbName collectionName methodParam topFlowsParam (longTermModeFromExclude (fromMaybe False mExcludeLT))

-- ---------------------------------------------------------------------------
-- Servant server
-- ---------------------------------------------------------------------------

lcaServer :: AppEnv -> Server LCAAPI
lcaServer env = hoistServer lcaAPI (runApp env) handlers
  where
    handlers =
        getActivityInfo
            :<|> getProcessDivision
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
            :<|> postReducedModel
            :<|> postActivityInventory
            :<|> postActivitySupplyChain
            :<|> getActivityConsumers
            :<|> getActivityPathTo
            :<|> getActivityComparison
            :<|> postChangesPresent
            :<|> postApplyChanges
            :<|> getContributingFlows
            :<|> getContributingActivities
            :<|> getScoreContributingFlows
            :<|> getScoreContributingActivities
            :<|> getFlowDetail
            :<|> getFlowActivities
            :<|> flowFactorsHandler
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
            :<|> getCatalogue
            :<|> getCatalogueFingerprint
            :<|> getDatabaseComparison
            :<|> getScorePairs
            :<|> postImpactsBatch
            :<|> postMethodImpactsBatch
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
            :<|> DBHandlers.systemModelHandler
            :<|> DBHandlers.deleteDatabaseHandler
            :<|> DBHandlers.deleteActivitiesHandler
            :<|> DBHandlers.createActivitiesHandler
            :<|> DBHandlers.replaceActivityHandler
            :<|> DBHandlers.editExchangesHandler
            :<|> DBHandlers.exportDatabaseHandler
            :<|> DBHandlers.documentFileHandler
            :<|> DBHandlers.setLicenceHandler
            :<|> DBHandlers.setReleaseHandler
            :<|> DBHandlers.setDisplayNameHandler
            :<|> DBHandlers.setDescriptionHandler
            :<|> DBHandlers.acceptSubstitutionHandler
            :<|> DBHandlers.uploadDatabaseHandler
            :<|> DBHandlers.getDatabaseSetupHandler
            :<|> DBHandlers.addDependencyHandler
            :<|> DBHandlers.removeDependencyHandler
            :<|> DBHandlers.setDataPathHandler
            :<|> DBHandlers.finalizeDatabaseHandler
            :<|> getMethodCollections
            :<|> DBHandlers.availableMethodCollectionsHandler
            :<|> DBHandlers.installMethodCollectionHandler
            :<|> loadMethodCollectionHandler
            :<|> unloadMethodCollectionHandler
            :<|> DBHandlers.deleteMethodHandler
            :<|> DBHandlers.uploadMethodHandler
            :<|> DBHandlers.exportMethodHandler
            :<|> getMethodCollectionComparison
            :<|> getMethodCollectionProfile
            :<|> MethodEdit.copyMethodCollectionHandler
            :<|> MethodEdit.editMethodFactorsHandler
            :<|> MethodEdit.editMethodCategoriesHandler
            :<|> MethodEdit.scoringSetsHandler
            :<|> MethodEdit.editScoringSetsHandler
            :<|> MethodEdit.undoMethodEditHandler
            :<|> MethodEdit.methodHistoryHandler
            :<|> MethodEdit.methodFlowsHandler
            :<|> DBHandlers.listRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.loadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.unloadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.deleteRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.uploadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.getFlowSynonymGroupsHandler
            :<|> DBHandlers.downloadRefDataHandler DBHandlers.FlowSynonyms
            :<|> DBHandlers.refDataRowsHandler DBHandlers.FlowSynonyms
            :<|> DBHandlers.listRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.loadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.unloadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.deleteRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.uploadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.downloadRefDataHandler DBHandlers.CompartmentMappings
            :<|> DBHandlers.refDataRowsHandler DBHandlers.CompartmentMappings
            :<|> DBHandlers.listRefData DBHandlers.UnitDefs
            :<|> DBHandlers.loadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.unloadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.deleteRefData DBHandlers.UnitDefs
            :<|> DBHandlers.uploadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.downloadRefDataHandler DBHandlers.UnitDefs
            :<|> DBHandlers.refDataRowsHandler DBHandlers.UnitDefs
            :<|> getLogsHandler
            :<|> getUsageHandler
            :<|> forgetUsageHandler
            :<|> postAuth
            :<|> getVersion
            :<|> getHosting
            :<|> getStats
            :<|> getClassificationPresets
            :<|> pure standardLicences
            :<|> pure formulaLanguage
            :<|> postFormulaCheck
            :<|> DBHandlers.databaseHistoryHandler
            :<|> getDatabaseParameters
            :<|> DBHandlers.editDatabaseParametersHandler
            :<|> DBHandlers.listScenariosHandler
            :<|> DBHandlers.saveScenarioHandler
            :<|> DBHandlers.dropScenarioHandler
            :<|> getOpenApiSpec

{- | Build the scoring input map (impact method name → raw score) from LCIA
results. Keyed by method NAME, which is unique per collection – not by
'lrCategory', which for ILCD methods is the coarse damage class (e.g. all four
"Climate change-*" methods share category "Climate change"; the three freshwater
ecotoxicity methods share "Aquatic eco-toxicity"). Keying by category collapses
such methods and breaks single-score resolution ("unknown variable …"). For
SimaPro-adapted methods name == category, so their scoring is unchanged.
-}
rawScoreMapByName :: [LCIAResult] -> M.Map Text Double
rawScoreMapByName results = M.fromList [(lrMethodName r, lrScore r) | r <- results]

{- | Evaluate every scoring set against the raw impact score map.
Scoring sets that fail to evaluate are logged as warnings and omitted.
Values are pre-multiplied by each set's displayMultiplier (default 1.0).
-}
computeAllScoringSets :: [ScoringSet] -> M.Map Text Double -> IO [(ScoringSet, ScoringEvaluation)]
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
    pure [(ss, e) | Just (ss, e) <- evaluations]

-- | Each set's scores, by the set's name.
scoresOf :: [(ScoringSet, ScoringEvaluation)] -> M.Map Text (M.Map Text Double)
scoresOf evaluated = M.fromList [(ssName ss, seScores e) | (ss, e) <- evaluated]

{- | The variables a score formula reads, each with its value. Intermediate
helpers (consumed by `computed` but not referenced in any `scores.*` formula)
are hidden from the breakdown.
-}
scoringIndicators :: ScoringSet -> ScoringEvaluation -> M.Map Text ScoringIndicator
scoringIndicators ss e =
    M.mapWithKey (\var val -> ScoringIndicator (M.findWithDefault var var names) val) (M.filterWithKey (\var _ -> S.member var displayed) (seNwEnv e))
  where
    displayed :: S.Set Text
    displayed = S.fromList (concatMap (Expr.collectIdentifiers Expr.Volca) (M.elems (ssScores ss)))
    names :: M.Map Text Text
    names = ssLabels ss <> ssVariables ss

-- | Every row of a set with its value, read or not by a score.
scoringRows :: ScoringSet -> ScoringEvaluation -> M.Map Text ScoringIndicator
scoringRows ss e =
    M.fromList [(srVariable r, ScoringIndicator (srLabel r) value) | r <- rowsOf ss, Just value <- [M.lookup (srVariable r) (seNwEnv e)]]

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

@exact@ is the one the caller is going to list with, because it decides which
rows match and therefore how many there are; @sort@ only orders them.
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

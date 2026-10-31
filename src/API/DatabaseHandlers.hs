{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : API.DatabaseHandlers
Description : Database management API handlers

Extracted from API.Routes to reduce module size and improve organization.
Contains handlers for database listing, loading, unloading, uploading, and deletion.
-}
module API.DatabaseHandlers (
    -- * Handlers
    getDatabases,
    loadDatabaseHandler,
    unloadDatabaseHandler,
    relinkDatabaseHandler,
    gapReportHandler,
    gapReportToAPI,
    editReportToAPI,
    qualityReportHandler,
    qualityReportToAPI,
    computedQualityReportToAPI,
    coverageReportHandler,
    coverageReportToAPI,
    explainCFToAPI,
    explainedFlowAPI,
    copyDatabaseHandler,
    deriveDatabaseHandler,
    systemModelHandler,
    deleteDatabaseHandler,
    deleteActivitiesHandler,
    createActivitiesHandler,
    replaceActivityHandler,
    editExchangesHandler,
    editDatabaseParametersHandler,
    databaseHistoryHandler,
    databaseHistoryToAPI,
    parametersReportToAPI,
    listScenariosHandler,
    saveScenarioHandler,
    dropScenarioHandler,
    exportDatabaseHandler,
    documentFileHandler,
    setLicenceHandler,
    setReleaseHandler,
    setDisplayNameHandler,
    setDescriptionHandler,
    acceptSubstitutionHandler,
    licenceRefusal,
    refuseUnlessGranted,
    servedLicence,
    exportMethodHandler,
    encodeExportWarnings,
    uploadDatabaseHandler,
    uploadMethodHandler,
    availableMethodCollectionsHandler,
    installMethodCollectionHandler,
    deleteMethodHandler,

    -- * Setup Page Handlers
    getDatabaseSetupHandler,
    addDependencyHandler,
    removeDependencyHandler,
    setDataPathHandler,
    finalizeDatabaseHandler,

    -- * Reference data handlers
    RefDataKind (..),
    listRefData,
    loadRefData,
    unloadRefData,
    deleteRefData,
    uploadRefData,
    getFlowSynonymGroupsHandler,
    downloadRefDataHandler,
    refDataRowsHandler,
    refDataRows,

    -- * Helpers
    convertDbStatus,
    guardMutation,
    refuseWrite,
    uploadRefusal,
    memoryRefusal,
    loadRefusal,
    copyRefusal,
    quotaCounts,
    loadQuotaRefusal,
    simpleAction,
    formatToText,
    chooseMasterData,
    uploadSizeCap,
    uploadBodyCeiling,
    streamToTempFile,
    UploadRejection (..),
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, mfilter, unless, void, when)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import Data.List (find, isPrefixOf, unsnoc)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Word (Word64)
import Network.HTTP.Types.URI (urlEncode)
import Servant (Header, Headers, ServerError, SourceIO, addHeader, err400, err403, err404, err409, err413, err422, err500, err502, errBody, throwError)
import qualified Servant.Types.SourceT as S
import qualified System.Directory
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose, openBinaryTempFile)

-- Flow synonyms

-- Compartment mappings

-- Unit definitions

import qualified Database.ComputedQuality as CQ

import API.Types (
    ActivateResponse (..),
    ActivityInput (..),
    ActivityWriteRequest (..),
    ActivityWriteResponse (..),
    AvailableMethodCollection (..),
    BinaryContent (..),
    BridgeGroupAPI (..),
    BridgedFlowAPI (..),
    CollectionBridgesAPI (..),
    ComputedQualityReportAPI (..),
    CoverageReportAPI (..),
    DatabaseExportRequest (..),
    DatabaseHistoryEntry (..),
    DatabaseListResponse (..),
    DatabaseStatusAPI (..),
    DeleteClassFilter (..),
    DeleteSelectionRequest (..),
    DeleteSelectionResponse (..),
    DeriveRequest (..),
    DroppedFactors (..),
    ExchangeEditRequest (..),
    ExchangeEditResponse (..),
    ExplainCFResult (..),
    ExplainedFlowAPI (..),
    ExplainedMatchAPI (..),
    ExplainedStepAPI (..),
    ExportRequest (..),
    GapConsumerAPI (..),
    GapEntryAPI (..),
    GapReportAPI (..),
    LoadDatabaseResponse (..),
    ModelExceptionAPI (..),
    ModelParameterAPI (..),
    ModelSubstitutionAPI (..),
    ParametersEditRequest (..),
    ParametersEditResponse (..),
    QualityCheckAPI (..),
    QualityOffenderAPI (..),
    QualityReportAPI (..),
    RefDataListResponse (..),
    RefDataRowsResponse (..),
    RefDataStatusAPI (..),
    RelinkRequest (..),
    RelinkResponse (..),
    RestatedLineAPI (..),
    ScenarioAPI (..),
    ScenarioSaveRequest (..),
    SynonymGroupsResponse (..),
    SystemModelAPI (..),
    TakenProcessAPI (..),
    UploadChunk (..),
    UploadResponse (..),
    UploadedFamily (..),
    toAuthoredActivities,
    toExchangeEdits,
    toParameterEdits,
 )
import App.Env (AppEnv (..), AppM)
import Config (DatabaseConfig (..), HostingConfig (..), MethodConfig (..), MethodOrigin (..), ReadOnly (..), RefDataConfig (..), RefDataSource (..), hostingReadOnly, licenceOf, messageOr, readOnlyRefusalFor)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Indexing (repeated)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.Author (RestatedLine (..))
import Database.Crate (Changes (..), CrateInput (..), Package (..), Packaging (..), openPackage, packageExport, parsePackaging, requiredReleases)
import Database.Edit (
    DeleteOutcome (..),
    DeleteRequest (..),
    EditReport (..),
    ParametersReport (..),
    WriteRefusal (..),
    WriteReport (..),
    WriteVerb (..),
    copyDatabase,
    databaseHistory,
    databaseProvenance,
    deleteActivitiesInDB,
    derivationOf,
    deriveDatabase,
    editDatabaseParameters,
    editExchanges,
    refusalMessage,
    writeActivities,
 )
import Database.Export (parseExportFormat, parseMethodExportFormat, serializeDatabase, serializeMethodCollection)
import Database.ExportSupplier (dependenciesOf)
import Database.History (Carried, HistoryLine (..), Journal (..), Provenance (..), changesReport)
import Database.Journal (Note, noteFrom, noteText)
import qualified Database.Loader as Loader
import Database.Manager (
    DatabaseLoadStatus (..),
    DatabaseManager (..),
    DatabaseSetupInfo (..),
    DatabaseStatus (..),
    DependencyEdit (..),
    LoadedDatabase (..),
    MethodCollectionStatus (..),
    RefDataStatus (..),
    RelativeDataPath (..),
    RelinkResult (..),
    SettingRefusal (..),
    SetupError (..),
    acceptSubstitution,
    addCompartmentMappings,
    addDatabase,
    addDependencyToStaged,
    addFlowSynonyms,
    addMethodCollection,
    addUnitDefs,
    compartmentMappingsUploadsDir,
    databaseCoverageReport,
    databaseGapReport,
    databaseLicence,
    databaseModification,
    databaseQualityReport,
    finalizeDatabase,
    flowSynonymsUploadsDir,
    getDatabase,
    getDatabaseSetupInfo,
    getFlowSynonymGroups,
    getMergedUnitConfig,
    getMethodCollection,
    listCompartmentMappings,
    listDatabases,
    listFlowSynonyms,
    listMethodCollections,
    listUnitDefs,
    loadCompartmentMappings,
    loadDatabase,
    loadFlowSynonyms,
    loadMethodCollection,
    loadOlcaPackageMethod,
    loadUnitDefs,
    madeFrom,
    readDocumentFile,
    readRefDataSource,
    relinkDatabase,
    relinkDatabaseWithMapping,
    removeCompartmentMappings,
    removeDatabase,
    removeDependencyFromStaged,
    removeFlowSynonyms,
    removeMethodCollection,
    removeUnitDefs,
    setDataPath,
    setUploadDescription,
    setUploadDisplayName,
    setUploadLicence,
    setUploadRelease,
    setupErrorMessage,
    systemModelOf,
    unitDefsUploadsDir,
    unloadCompartmentMappings,
    unloadDatabase,
    unloadFlowSynonyms,
    unloadUnitDefs,
 )
import qualified Database.Quality as Quality
import Database.RelinkMapping (parseAliasMap)
import Database.Requirements (ReleaseSubstitution)
import Database.Scenarios (ScenarioName (..), dropScenario, listScenarios, saveScenario, savedForExport)
import Database.SystemModel (ModelOnParent (..), NamedSubstitution (..))
import Database.Upload (
    DatabaseFormat (..),
    UploadData (..),
    UploadResult (..),
    detectMethodFormat,
    detectedFormatLabel,
    findMethodDirectory,
    formatDisplayText,
    handleUpload,
    isOlcaMethodPackage,
    listDirectoryRecursive,
    slugify,
 )
import qualified Database.UploadedDatabase as UploadedDB
import EcoSpold.Parser2 (parseElementaryExchanges)
import Method.Catalog (CatalogEntry (..), FetchFailure (..), fetchEntry, methodCatalog)
import qualified Method.Coverage as Coverage
import qualified Method.Explain as Explain
import Method.Mapping (BuildProvenance (..), CF (..), CFUnit (..), provenanceStrategyText, strategyToText)
import Method.ParserFactorTable (Dropped (..), familyCollections, isFactorTable, readFactorTable)
import Method.Types (Method (..), MethodCF (..), MethodCollection (mcMethods))
import Method.WriterCSV (serializeColumnarMethodCSV)
import Types (
    AddedAt,
    BiosphereFlow (..),
    BlockerReason (..),
    ClassificationFilter (..),
    ClassificationMatch (..),
    Database (..),
    DisplayName,
    FileReading (..),
    GeographyPolicy (..),
    Licence (..),
    ModelException (..),
    ModelParameter (..),
    ModelSubstitution (..),
    Modification,
    Permission (..),
    ProcessRef (..),
    Reading (..),
    Release (..),
    Requirement (..),
    SystemModel (..),
    addedNow,
    addedText,
    allocationKeyText,
    bfCompartmentName,
    bfCompartmentSub,
    getUnitNameForBioFlow,
    granted,
    isModified,
    modelOf,
    modelTitle,
    nonBlankText,
    processRefText,
    ratioValue,
    readingKey,
    reasonText,
    spdxId,
    unresolvedCount,
    withheldSentence,
 )
import Xlsx (Workbook, openWorkbook)

-- | List all databases
getDatabases :: AppM DatabaseListResponse
getDatabases = do
    dbManager <- asks aeDbManager
    dbStatuses <- liftIO $ listDatabases dbManager
    let statusList = map convertDbStatus dbStatuses
    return $ DatabaseListResponse statusList

-- | Load a database on demand
loadDatabaseHandler :: Text -> AppM LoadDatabaseResponse
loadDatabaseHandler dbName = do
    guardMutation
    dbManager <- asks aeDbManager
    hostingConfig <- asks aeHostingConfig
    liftIO (loadQuotaRefusal dbManager hostingConfig dbName) >>= mapM_ (failWith err403)
    (loadedDb, depResults) <- ioEither400 (loadDatabase dbManager dbName)
    status <- liftIO (loadedStatus dbManager loadedDb)
    pure (LoadSucceeded status depResults)

-- | Unload a database from memory
unloadDatabaseHandler :: Text -> AppM ActivateResponse
unloadDatabaseHandler dbName = do
    dbManager <- asks aeDbManager
    simpleAction (unloadDatabase dbManager dbName) ("Unloaded database: " <> dbName)

{- | Re-run cross-DB linking for a loaded database. An empty @{}@ body
re-resolves links within the existing dependency pin (plain relink), letting
the user recover from loads that happened in a suboptimal order without
reloading. A body carrying both @depDb@ and @mappingCsv@ switches to mapping
mode: relink against that one dependency using an inline supplier-alias CSV
(source and target by name or identity, with optional locations and a unit
conversion, see "Database.RelinkMapping"),
so inputs named after one background database resolve against a
differently-named dependency. A loaded-but-undeclared dependency is
auto-pinned in-memory rather than rejected. Supplying exactly one of the two
is a client error. Parse/validation failures surface as 4xx rather than a
silent no-op; the only 404 from this path is an unloaded database or
dependency.
-}
relinkDatabaseHandler :: Text -> RelinkRequest -> AppM RelinkResponse
relinkDatabaseHandler dbName req = do
    guardMutation
    dbManager <- asks aeDbManager
    case (rmrDepDb req, rmrMappingCsv req) of
        (Nothing, Nothing) ->
            runRelink (relinkDatabase dbManager dbName)
        (Just depDb, Just csv) ->
            case parseAliasMap (BSL.fromStrict (T.encodeUtf8 csv)) of
                Left err -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
                Right aliases -> runRelink (relinkDatabaseWithMapping dbManager dbName depDb aliases)
        (Just _, Nothing) -> incomplete
        (Nothing, Just _) -> incomplete
  where
    incomplete =
        throwError err400{errBody = "relink: depDb and mappingCsv must be supplied together"}
    runRelink :: IO (Either Text RelinkResult) -> AppM RelinkResponse
    runRelink act = do
        res <- liftIO act
        case res of
            Left err -> throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
            Right r ->
                return
                    RelinkResponse
                        { rrDbName = rresDbName r
                        , rrUnresolvedBefore = rresUnresolvedBefore r
                        , rrUnresolvedAfter = rresUnresolvedAfter r
                        , rrCrossDBLinks = rresCrossDBLinks r
                        , rrDependsOn = rresDepsLoaded r
                        }

-- | Every change made to a database, oldest first, with its reason.
databaseHistoryHandler :: Text -> AppM [DatabaseHistoryEntry]
databaseHistoryHandler dbName = do
    refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    liftIO (databaseHistory dbManager dbName) >>= \case
        Right carried -> pure (databaseHistoryToAPI carried)
        Left err -> do
            -- A database that is there with a journal the engine cannot read is the engine's failure, not the caller's.
            known <- M.member dbName <$> liftIO (readTVarIO (dmAvailableDbs dbManager))
            throwError (if known then err500 else err404){errBody = BSL.fromStrict (T.encodeUtf8 err)}

-- | Each line of a database's history, with the database it was made on: its parent's, or its own.
databaseHistoryToAPI :: Carried (Journal DatabaseConfig) -> [DatabaseHistoryEntry]
databaseHistoryToAPI carried =
    [ DatabaseHistoryEntry (hlAt line) (dcName (journalOf journal)) (noteText <$> hlNote line) (hlSummary line) (hlDetails line)
    | journal <- toList carried
    , line <- journalLines journal
    ]

{- | Supplier-gap report for a loaded or staged database: everything still
unsupplied after internal resolution and cross-DB linking. The natural
follow-up read after a relink: POST relink, then GET gap-report.
-}
gapReportHandler :: Text -> Maybe Int -> AppM GapReportAPI
gapReportHandler dbName mLimit = do
    refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    res <- liftIO $ databaseGapReport dbManager dbName
    case res of
        Left err -> throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right report -> return (gapReportToAPI mLimit report)

{- | Project the domain gap report onto its wire shape, keeping at most
@limit@ gap entries (they are ranked by demanding edges, so a cap keeps the
biggest gaps). The header counts always cover the full report, so a truncated
list stays countable, never a silent cap.
-}
gapReportToAPI :: Maybe Int -> Loader.GapReport -> GapReportAPI
gapReportToAPI mLimit r =
    GapReportAPI
        { graDbName = Loader.grDbName r
        , graTotalInputs = Loader.grTotalInputs r
        , graInternalLinks = Loader.grInternalLinks r
        , graCrossDBLinks = Loader.grCrossDBLinks r
        , graUnresolvedEdges = Loader.grUnresolvedEdges r
        , graUnresolvedProducts = Loader.grUnresolvedProducts r
        , graCompleteness = Loader.grCompleteness r
        , graGaps = map entryToAPI (maybe id take mLimit (Loader.grGaps r))
        }
  where
    entryToAPI e =
        let first :| others = Loader.gapReasons (Loader.geReason e)
         in GapEntryAPI
                { gaeName = Loader.geFlowName e
                , gaeSupplierActivity = Loader.geSupplierActivity e
                , gaeLocation = Loader.geLocation e
                , gaeUnit = Loader.geUnit e
                , gaeReasons = first : others
                , gaeReason = brReason first
                , gaeDetail = brDetail first
                , gaeEdges = Loader.geEdges e
                , gaeConsumers = Loader.geConsumers e
                , gaeDemandSum = Loader.geDemandSum e
                , gaeTopConsumers = map consumerToAPI (Loader.geTopConsumers e)
                }
    consumerToAPI c =
        GapConsumerAPI
            { gcaProcessId = processRefText (ProcessRef (Loader.gcActUUID c) (Loader.gcProdUUID c))
            , gcaActivityName = Loader.gcActivityName c
            , gcaProductName = Loader.gcProductName c
            , gcaLocation = Loader.gcLocation c
            , gcaEdges = Loader.gcEdges c
            }

{- | Dataset-soundness report for a loaded or staged database: the structural
defects a score can't reveal. The methodological counterpart of the
supplier-gap report: that one says what a database is missing, this one says
what is malformed in it.
-}
qualityReportHandler :: Text -> Maybe Int -> AppM QualityReportAPI
qualityReportHandler dbName mLimit = do
    refuseUnlessGranted ReadInventory dbName
    dbManager <- asks aeDbManager
    res <- liftIO $ databaseQualityReport dbManager dbName
    case res of
        Left err -> throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right report -> return (qualityReportToAPI mLimit report)

{- | Project the domain quality report onto its wire shape, keeping at most
@limit@ findings per check (they are sorted worst-first, so a cap keeps the
worst ones). Each check's @offenderCount@ always covers its full list, so a
truncated list stays countable, never a silent cap.
-}
qualityReportToAPI :: Maybe Int -> Quality.QualityReport -> QualityReportAPI
qualityReportToAPI mLimit r =
    QualityReportAPI
        { qraDbName = Quality.qrDbName r
        , qraProcessCount = Quality.qrProcessCount r
        , qraReferenceProduct = checkToAPI mLimit (Quality.qrReferenceProduct r)
        , qraAllocationSums = checkToAPI mLimit (Quality.qrAllocationSums r)
        , qraDuplicateActivities = checkToAPI mLimit (Quality.qrDuplicateActivities r)
        , qraDuplicateProducts = checkToAPI mLimit (Quality.qrDuplicateProducts r)
        , qraSuspiciousAmounts = checkToAPI mLimit (Quality.qrSuspiciousAmounts r)
        , qraMissingMetadata = checkToAPI mLimit (Quality.qrMissingMetadata r)
        , qraUndeclaredGeography = checkToAPI mLimit (Quality.qrUndeclaredGeography r)
        , qraFormulaConsistency = checkToAPI mLimit (Quality.qrFormulaConsistency r)
        , qraTruncatedNameCollisions = checkToAPI mLimit (Quality.qrTruncatedNameCollisions r)
        , qraMissingPedigree = checkToAPI mLimit (Quality.qrMissingPedigree r)
        , qraUnconsumedProducts = checkToAPI mLimit (Quality.qrUnconsumedProducts r)
        , qraUnsuppliedInputs = checkToAPI mLimit (Quality.qrUnsuppliedInputs r)
        , qraObsoleteInputs = checkToAPI mLimit (Quality.qrObsoleteInputs r)
        , qraLandTransformationBalance = checkToAPI mLimit (Quality.qrLandTransformationBalance r)
        , qraOxygenDemandOrder = checkToAPI mLimit (Quality.qrOxygenDemandOrder r)
        , qraInvalidCas = checkToAPI mLimit (Quality.qrInvalidCas r)
        , qraAllocationOutOfRange = checkToAPI mLimit (Quality.qrAllocationOutOfRange r)
        , qraUnallocated = checkToAPI mLimit (Quality.qrUnallocated r)
        , qraUnmeasurableAmounts = checkToAPI mLimit (Quality.qrUnmeasurableAmounts r)
        , qraWaterBalance = checkToAPI mLimit (Quality.qrWaterBalance r)
        }

{- | Same projection for the computed report: one wire cap, one offender
shape, shared with the structural report via 'checkToAPI'.
-}
computedQualityReportToAPI :: Maybe Int -> CQ.ComputedQualityReport -> ComputedQualityReportAPI
computedQualityReportToAPI mLimit r =
    ComputedQualityReportAPI
        { cqaDbName = CQ.cqDbName r
        , cqaCollection = CQ.cqCollection r
        , cqaProcessCount = CQ.cqProcessCount r
        , cqaScoreOutliers = checkToAPI mLimit (CQ.cqScoreOutliers r)
        , cqaZeroScores = checkToAPI mLimit (CQ.cqZeroScores r)
        , cqaNegativeScores = checkToAPI mLimit (CQ.cqNegativeScores r)
        }

-- | Project one domain check onto the wire, capping its list at @limit@.
checkToAPI :: Maybe Int -> Quality.QualityCheck -> QualityCheckAPI
checkToAPI mLimit c =
    QualityCheckAPI
        { qcaApplicable = Quality.qcApplicable c
        , qcaOffenderCount = length (Quality.qcOffenders c)
        , qcaOffenders = map offenderToAPI (maybe id take mLimit (Quality.qcOffenders c))
        }
  where
    offenderToAPI o =
        QualityOffenderAPI
            { qoaSeverity = Quality.qoSeverity o
            , qoaProcessId = Quality.qoProcessId o
            , qoaActivityName = Quality.qoActivityName o
            , qoaLocation = Quality.qoLocation o
            , qoaProductName = Quality.qoProductName o
            , qoaDetail = Quality.qoDetail o
            }

{- | Characterization-coverage report for a loaded database: the flows each
loaded method collection scores only through a name bridge. Loaded-only (the
coverage probe reads the built method tables), so a missing database or an
unloaded named collection is a 404.
-}
coverageReportHandler :: Text -> Maybe Text -> Maybe Int -> AppM CoverageReportAPI
coverageReportHandler dbName mCollection mLimit = do
    dbManager <- asks aeDbManager
    res <- liftIO $ databaseCoverageReport dbManager dbName mCollection
    case res of
        Left err -> throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right report -> return (coverageReportToAPI mLimit report)

{- | Project the domain coverage report onto its wire shape, keeping at most
@limit@ bridge groups per collection (sorted by rename target, so a cap is
stable). Each collection's @bridgeGroupCount@ always covers the full list, so a
truncated list stays countable, never a silent cap.
-}
coverageReportToAPI :: Maybe Int -> Coverage.CoverageReport -> CoverageReportAPI
coverageReportToAPI mLimit r =
    CoverageReportAPI
        { cvrDbName = Coverage.crDbName r
        , cvrCollections = map collectionToAPI (Coverage.crCollections r)
        }
  where
    collectionToAPI c =
        CollectionBridgesAPI
            { cvcCollection = Coverage.cbCollection c
            , cvcTotalFlows = Coverage.cbTotalFlows c
            , cvcCharacterizedFlows = Coverage.cbCharacterizedFlows c
            , cvcBridgeGroupCount = length (Coverage.cbGroups c)
            , cvcBridgeGroups = map groupToAPI (maybe id take mLimit (Coverage.cbGroups c))
            }
    groupToAPI g =
        BridgeGroupAPI
            { cvgCas = Coverage.bgCas g
            , cvgMethodName = Coverage.bgMethodName g
            , cvgBridgedFlows = map flowToAPI (Coverage.bgBridged g)
            }
    flowToAPI f =
        BridgedFlowAPI
            { cvfFlowName = Coverage.brfFlowName f
            , cvfStrategy = strategyToText (Coverage.brfStrategy f)
            }

{- | Copy a loaded database under a new name. The copy is an independent
in-memory database registered under @newName@; the source is untouched.
-}
copyDatabaseHandler :: Text -> Text -> AppM ActivateResponse
copyDatabaseHandler dbName newName = do
    guardMutation
    dbManager <- asks aeDbManager
    hostingConfig <- asks aeHostingConfig
    -- A copy produces another database of the user's own, already loaded, so
    -- it spends both budgets; without this, the quota is one rename away from
    -- meaningless.
    refusal <- liftIO $ do
        (uploaded, loadedUploads) <- quotaCounts dbManager
        pure (copyRefusal uploaded loadedUploads hostingConfig)
    mapM_ (failWith err403) refusal
    simpleAction (copyDatabase dbManager dbName newName) ("Copied database: " <> dbName <> " -> " <> newName)

{- | Make a parent of a database read from its files, or a child of a parent
divided by a system model, and register it under @newName@.

Answers with what a load answers with, not with what a copy answers with: it
is a load, of tens of seconds on a large source, and the page that asks for it
is the one that already waits on that answer and shows the progress lines
underneath it.

A request asking for both or neither is a 400, as is a system model that does
not read, does not fit the parent or divides nothing, like every other reason
a load produced no database.
-}
deriveDatabaseHandler :: Text -> Text -> DeriveRequest -> AppM LoadDatabaseResponse
deriveDatabaseHandler dbName newName request = do
    guardMutation
    derivation <- either badRequest pure (derivationOf (fromMaybe False (drParent request)) (drSystemModel request))
    dbManager <- asks aeDbManager
    hostingConfig <- asks aeHostingConfig
    -- A derived database is another database of the user's own, already
    -- loaded, so it spends both budgets exactly as a copy does.
    refusal <- liftIO $ do
        (uploaded, loadedUploads) <- quotaCounts dbManager
        pure (copyRefusal uploaded loadedUploads hostingConfig)
    mapM_ (failWith err403) refusal
    (loadedDb, depResults) <- ioEither400 (deriveDatabase dbManager dbName newName derivation)
    status <- liftIO (loadedStatus dbManager loadedDb)
    pure (LoadSucceeded status depResults)
  where
    badRequest :: Text -> AppM a
    badRequest err = throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 err)}

{- | A child's system model, each exception with the processes it takes from
the parent as loaded. A database read from its files has none: a 400 that says
so, like a parent not loaded.
-}
systemModelHandler :: Text -> AppM SystemModelAPI
systemModelHandler dbName = do
    dbManager <- asks aeDbManager
    onParent <- ioEither400 (systemModelOf dbManager dbName)
    pure
        SystemModelAPI
            { smaText = smText (mopModel onParent)
            , smaExceptions =
                [ ModelExceptionAPI
                    { meaKey = exKey ex
                    , meaReason = reasonText (exReason ex)
                    , meaProcesses = [TakenProcessAPI{tpaActivity = UUID.toText u, tpaName = name} | (u, name) <- processes]
                    }
                | (ex, processes) <- mopExceptions onParent
                ]
            , smaSubstitutions =
                [ ModelSubstitutionAPI
                    { msaCoproduct = processRefText (subCoproduct (nsRule ns))
                    , msaBlock = nsBlock ns
                    , msaCoproductName = nsCoproduct ns
                    , msaReplaces = processRefText (subReplaces (nsRule ns))
                    , msaReplacedProcess = nsReplaced ns
                    , msaReplacedProduct = nsReplacedProduct ns
                    , msaRatio = ratioValue (subRatio (nsRule ns))
                    , msaReason = reasonText (subReason (nsRule ns))
                    }
                | ns <- mopSubstitutions onParent
                ]
            , smaParameters =
                [ ModelParameterAPI
                    { mpaName = mpName mp
                    , mpaValue = mpValue mp
                    , mpaReason = reasonText (mpReason mp)
                    , mpaProcesses = [TakenProcessAPI{tpaActivity = processRefText (uncurry ProcessRef k), tpaName = name} | (k, name) <- processes]
                    }
                | (mp, processes) <- mopParameters onParent
                ]
            }

-- | Delete an uploaded database (move to trash)
deleteDatabaseHandler :: Text -> AppM ActivateResponse
deleteDatabaseHandler dbName = do
    dbManager <- asks aeDbManager
    simpleAction (removeDatabase dbManager dbName) ("Deleted database: " <> dbName)

{- | Delete the whole filtered set of activities from a loaded database, in
place. The filter selects the full matching set (pagination ignored); the
keep/extra lists adjust the set individually. Rebuilds matrices and unlinks
surviving references; returns the count removed.
-}
deleteActivitiesHandler :: Text -> DeleteSelectionRequest -> AppM DeleteSelectionResponse
deleteActivitiesHandler dbName req = do
    guardMutation
    dbManager <- asks aeDbManager
    let classFilters =
            [ ClassificationFilter{clfSystem = dcfSystem f, clfValue = dcfValue f, clfMatch = if dcfExact f then MatchExact else MatchContains}
            | f <- dsqClassifications req
            ]
        -- A present-but-blank filter (e.g. JSON "name":"") is no filter at all.
        -- Collapse it to Nothing so name candidates fall back to "all activities"
        -- instead of a BM25/full-scan path that yields zero (index present) or all
        -- (index absent), an index-dependent ALL-vs-NONE divergence.
        nonBlank = mfilter (not . T.null . T.strip)
    result <-
        liftIO $
            deleteActivitiesInDB
                dbManager
                dbName
                DeleteRequest
                    { drName = nonBlank (dsqName req)
                    , drLocation = nonBlank (dsqLocation req)
                    , drProduct = nonBlank (dsqProduct req)
                    , drClassifications = classFilters
                    , drExactName = fromMaybe False (dsqExact req)
                    , drKeep = dsqKeep req
                    , drExtra = dsqExtra req
                    , drIds = dsqIds req
                    , drNote = dsqNote req >>= noteFrom
                    }
    case result of
        -- A failed delete is a client error (bad filter, unknown DB, loaded
        -- dependents). Surface it as 4xx so a raw HTTP client can't read the
        -- 200 envelope as success.
        Left err -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right outcome ->
            return $
                DeleteSelectionResponse
                    True
                    ("Deleted " <> T.pack (show (doRemoved outcome)) <> " activities from " <> dbName)
                    (doRemoved outcome)
                    (not (doPersisted outcome))
                    (doWarnings outcome)

{- | Write new activities into a loaded database.

The domain decides what is allowed ('Database.Edit.writeActivities'); this
turns each refusal into the status a client can act on. A key that already
exists is a 409 (the author is re-describing a row the database holds and
wants the PUT) and a batch a caller can fix is a 400 carrying every complaint
at once, so a ten-line inventory is fixed in one round trip.
-}
createActivitiesHandler :: Text -> ActivityWriteRequest -> AppM ActivityWriteResponse
createActivitiesHandler dbName req = runWrite dbName (awrNote req >>= noteFrom) CreateActivities (awrActivities req)

{- | Rewrite one activity the database already holds, keeping its identity. A
@process_id@ the database does not hold is a 404.
-}
replaceActivityHandler :: Text -> Text -> Maybe Text -> ActivityInput -> AppM ActivityWriteResponse
replaceActivityHandler dbName processId note body =
    runWrite dbName (note >>= noteFrom) (ReplaceActivity processId) [body]

runWrite :: Text -> Maybe Note -> WriteVerb -> [ActivityInput] -> AppM ActivityWriteResponse
runWrite dbName note verb inputs = do
    guardMutation
    dbManager <- asks aeDbManager
    authored <- either (writeErr err400 . T.intercalate "\n") pure (toAuthoredActivities inputs)
    outcome <- liftIO (writeActivities dbManager dbName note verb authored)
    case outcome of
        Left refusal -> writeErr (statusFor refusal) (refusalMessage refusal)
        Right report ->
            pure
                ActivityWriteResponse
                    { awpWritten = wrWritten report
                    , awpTransient = not (wrPersisted report)
                    , awpWarnings = wrWarnings report
                    }

{- | Change the inventory of one activity the database already holds.

The operation a PUT cannot do: the activity keeps its identity and everything
a description would not carry, and only the lines the edit names change. An
activity the database does not hold is a 404; a selector that reaches nothing
is a 400, listing every complaint at once.
-}
editExchangesHandler :: Text -> Text -> ExchangeEditRequest -> AppM ExchangeEditResponse
editExchangesHandler dbName processId req = do
    guardMutation
    dbManager <- asks aeDbManager
    edits <- either (writeErr err400 . T.intercalate "\n") pure (toExchangeEdits req)
    outcome <- liftIO (editExchanges dbManager dbName (eerNote req >>= noteFrom) processId edits)
    either refuseWrite (pure . editReportToAPI) outcome

{- | What an edit answers, on every surface that offers one, so an assistant
and a person reading the API reference are told the same thing.
-}
editReportToAPI :: EditReport -> ExchangeEditResponse
editReportToAPI report =
    ExchangeEditResponse
        { eepRemoved = erRemoved report
        , eepAmountsSet = erAmountsSet report
        , eepAdded = erAdded report
        , eepTextsSet = erTextsSet report
        , eepFormulasSet = erFormulasSet report
        , eepPricesSet = erPricesSet report
        , eepDryMassesSet = erDryMassesSet report
        , eepWetMassesSet = erWetMassesSet report
        , eepParametersSet = erParametersSet report
        , eepParametersRemoved = erParametersRemoved report
        , eepRestated = [RestatedLineAPI{rsPosition = rlPosition line, rsFrom = rlFrom line, rsTo = rlTo line} | line <- erRestated report]
        , eepTransient = not (erPersisted report)
        , eepWarnings = erWarnings report
        }

-- | Set or remove a database's parameters, and restate the processes that read them.
editDatabaseParametersHandler :: Text -> ParametersEditRequest -> AppM ParametersEditResponse
editDatabaseParametersHandler dbName req = do
    guardMutation
    dbManager <- asks aeDbManager
    edits <- either (writeErr err400 . T.intercalate "\n") pure (toParameterEdits req)
    outcome <- liftIO (editDatabaseParameters dbManager dbName (pqNote req >>= noteFrom) edits)
    either refuseWrite (pure . parametersReportToAPI) outcome

parametersReportToAPI :: ParametersReport -> ParametersEditResponse
parametersReportToAPI report =
    ParametersEditResponse
        { prsParametersSet = prSet report
        , prsParametersRemoved = prRemoved report
        , prsRestatedProcesses = prRestated report
        , prsTransient = not (prPersisted report)
        , prsWarnings = prWarnings report
        }

-- | The scenarios saved on a database.
listScenariosHandler :: Text -> AppM [ScenarioAPI]
listScenariosHandler dbName = do
    dbManager <- asks aeDbManager
    either refuseWrite pure =<< liftIO (listScenarios dbManager dbName)

-- | Save a scenario under a name, replacing what it held, and answer the scenarios saved.
saveScenarioHandler :: Text -> Text -> ScenarioSaveRequest -> AppM [ScenarioAPI]
saveScenarioHandler dbName name req = do
    guardMutation
    dbManager <- asks aeDbManager
    either refuseWrite pure =<< liftIO (saveScenario dbManager dbName (ScenarioName name) (ssrParameters req))
    listScenariosHandler dbName

-- | Drop a saved scenario, and answer the scenarios left.
dropScenarioHandler :: Text -> Text -> AppM [ScenarioAPI]
dropScenarioHandler dbName name = do
    guardMutation
    dbManager <- asks aeDbManager
    either refuseWrite pure =<< liftIO (dropScenario dbManager dbName (ScenarioName name))
    listScenariosHandler dbName

-- | A refused write, answered with its status and its message.
refuseWrite :: WriteRefusal -> AppM a
refuseWrite refusal = writeErr (statusFor refusal) (refusalMessage refusal)

-- | One status per refusal, so a client never has to read the message to branch.
statusFor :: WriteRefusal -> ServerError
statusFor = \case
    NotLoaded _ -> err404
    NotWritable _ -> err400
    Malformed _ -> err400
    AlreadyPresent _ -> err409
    NotPresent _ -> err404
    WriteFailed _ -> err400

writeErr :: ServerError -> Text -> AppM a
writeErr status message = throwError status{errBody = BSL.fromStrict (T.encodeUtf8 message)}

{- | Export a loaded database as a raw octet-stream body: the same shape the
upload endpoint reads, and the only response cheap enough for a multi-hundred-MB
archive (a base64 JSON envelope costs +33% and four full copies before the
first byte leaves). EcoSpold 2 / ILCD multi-file trees are zipped; single-file
formats carry their bytes directly. Best-effort approximation warnings ride the
@X-Volca-Export-Warnings@ header, percent-encoded because activity names are
arbitrary Unicode and joined with newlines. With @package@ (@ro-crate@), the
body is a zip holding that export beside its RO-Crate description. Failures
surface as HTTP errors: 400 for an unknown format or package, data the target
format cannot represent, or a package whose dependency declares no release
(refused before the export is built), 404 for a
database that is not loaded, never a 200 with a failure flag.
-}
exportDatabaseHandler :: Text -> DatabaseExportRequest -> AppM (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
exportDatabaseHandler dbName req = do
    refuseUnlessGranted Download dbName
    dbManager <- asks aeDbManager
    fmt <- either (failWith err400) pure (parseExportFormat (derFormat req))
    packaging <- either (failWith err400) pure (parsePackaging (derPackage req))
    mLoaded <- liftIO (getDatabase dbManager dbName)
    ld <- maybe (failWith err404 ("Database not loaded: " <> dbName)) pure mLoaded
    configs <- liftIO (readTVarIO (dmAvailableDbs dbManager))
    modification <- liftIO (databaseModification configs (ldConfig ld))
    package <- case packaging of
        Plain
            | isModified modification -> failWith err400 "This database was modified: export it with package ro-crate, which carries its changes report."
            | otherwise -> pure id
        RoCrate -> packageExport dbName <$> (liftIO (crateInput dbManager ld fmt modification) >>= either (failWith err400) pure)
    units <- liftIO (getMergedUnitConfig dbManager)
    scenarios <- liftIO (listScenarios dbManager dbName)
    let (saved, problems) = either (const (M.empty, [])) (savedForExport (ldDatabase ld)) scenarios
    indexed <- liftIO (readTVarIO (dmIndexedDbs dbManager))
    (bytes, warnings) <- either (failWith err400) pure (serializeDatabase units fmt saved (dependenciesOf indexed (ldDatabase ld)) (ldDatabase ld))
    pure (addHeader (encodeExportWarnings (warnings <> either unread (scenariosLeftOut fmt problems) scenarios)) (BinaryContent (package bytes)))
  where
    unread :: WriteRefusal -> [Text]
    unread refusal = ["the saved scenarios could not be read (" <> refusalMessage refusal <> "): any there are not in the export"]

    -- An openLCA package keeps the scenarios on its product systems, and says itself which it could not; no other format keeps one.
    scenariosLeftOut :: DatabaseFormat -> [Text] -> [ScenarioAPI] -> [Text]
    scenariosLeftOut fmt problems scenarios = case fmt of
        OpenLcaPackage -> [T.pack (show (length problems)) <> " saved scenarios have a problem and are not in the export:" <> foldMap ("\n  " <>) problems | not (null problems)]
        SimaProCSV -> notKept
        EcoSpold1 -> notKept
        EcoSpold2 -> notKept
        ILCDProcess -> notKept
        OpenLcaImpactCategory -> notKept
        BrightwayExcel -> notKept
        ColumnarCSV -> notKept
        UnknownFormat -> notKept
      where
        notKept :: [Text]
        notKept =
            [ T.pack (show n) <> " " <> kind <> " are not in the export"
            | (kind, n) <- [("scenarios shipped with the database", length (filter saShipped scenarios)), ("saved scenarios", length (filter (not . saShipped) scenarios))]
            , n > 0
            ]

{- | What the package of a loaded database says of it: the licence it is served
under, and the releases of the databases it links to.
-}
crateInput :: DatabaseManager -> LoadedDatabase -> DatabaseFormat -> Modification -> IO (Either Text CrateInput)
crateInput manager ld fmt modification = do
    configs <- readTVarIO (dmAvailableDbs manager)
    licence <- fromMaybe LicenceUnstated <$> databaseLicence manager name
    today <- utctDay <$> getCurrentTime
    history <- if isModified modification then fmap Just <$> databaseHistory manager name else pure (Right Nothing)
    made <- madeFrom manager ld
    pure $ do
        requires <- requiredReleases name configs (dbDependsOn (ldDatabase ld))
        changes <- fmap (changesOf (databaseProvenance configs config)) <$> history
        madeFrom' <- made
        Right
            CrateInput
                { ciName = dcDisplayName config
                , ciDescription = dcDescription config
                , ciPublished = today
                , ciLicence = licence
                , ciRequires = requires
                , ciFormat = fmt
                , ciChanges = changes
                , ciMadeFrom = madeFrom'
                }
  where
    config :: DatabaseConfig
    config = ldConfig ld

    changesOf :: Provenance -> Carried (Journal DatabaseConfig) -> Changes
    changesOf provenance carried = Changes (changesReport provenance (fmap dcDisplayName <$> carried)) (pvOriginal provenance) (pvRelease provenance)

    name :: Text
    name = dcName config

{- | A file a database ships with one of its literature entries, as an
attachment named after it. 404 for a database not loaded, a path its
documentation does not list, or one its package does not hold.
-}
documentFileHandler :: Text -> [Text] -> AppM (Headers '[Header "Content-Disposition" Text] BinaryContent)
documentFileHandler dbName segments = do
    refuseUnlessGranted Download dbName
    dbManager <- asks aeDbManager
    bytes <- liftIO (readDocumentFile dbManager dbName path) >>= either (failWith err404) pure
    pure (addHeader (attachment (last' segments)) (BinaryContent (BSL.fromStrict bytes)))
  where
    path :: Text
    path = T.intercalate "/" segments

    last' :: [Text] -> Text
    last' = maybe path snd . unsnoc

    -- RFC 6266: the UTF-8 name, percent-encoded, beside a plain fallback.
    attachment :: Text -> Text
    attachment name =
        "attachment; filename=\"" <> T.filter (\c -> c /= '"' && c >= ' ' && c < '\DEL') name <> "\"; filename*=UTF-8''" <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 name))

{- | 403 when the licence of a database refuses what the handler is about to
serve. A name the engine does not know passes, so the handler answers it with
its own 404.
-}
refuseUnlessGranted :: Permission -> Text -> AppM ()
refuseUnlessGranted p dbName = do
    dbManager <- asks aeDbManager
    liftIO (databaseLicence dbManager dbName) >>= mapM_ (mapM_ (failWith err403) . licenceRefusal p dbName)

{- | The licence a database is served under, for the handlers that trim their
answer to it. Read after the handler has resolved the database, so a name the
engine does not know has already been answered with a 404.
-}
servedLicence :: Text -> AppM Licence
servedLicence dbName = do
    dbManager <- asks aeDbManager
    fromMaybe LicenceUnstated <$> liftIO (databaseLicence dbManager dbName)

-- | Why a database's licence refuses a permission, when it does.
licenceRefusal :: Permission -> Text -> Licence -> Maybe Text
licenceRefusal p dbName licence
    | granted licence p = Nothing
    | otherwise = Just (withheldSentence dbName p)

{- | Replace the licence of an uploaded database. 404 for a name the engine does
not know, 409 for a database whose licence is written elsewhere: in the
configuration file, or on the source a copy reads, 500 for an upload whose
meta.toml cannot be read.
-}
setLicenceHandler :: Text -> Licence -> AppM Licence
setLicenceHandler dbName licence = do
    guardMutation
    dbManager <- asks aeDbManager
    liftIO (setUploadLicence dbManager dbName licence) >>= either settingRefused pure

{- | Declare, or clear with @null@, which published database an uploaded
database is. 404 for a name the engine does not know, 409 for a database the
configuration file declares, 500 for an upload whose meta.toml cannot be read.
-}
setReleaseHandler :: Text -> Maybe Release -> AppM (Maybe Release)
setReleaseHandler dbName release = do
    guardMutation
    dbManager <- asks aeDbManager
    liftIO (setUploadRelease dbManager dbName release) >>= either settingRefused pure

{- | Rename an uploaded database, and answer with the name kept: the one sent
without the spaces around it. 404 for a name the engine does not know, 409
for a database the configuration file declares, 500 for an upload whose
meta.toml cannot be read. A blank name is refused before it gets here.
-}
setDisplayNameHandler :: Text -> DisplayName -> AppM DisplayName
setDisplayNameHandler dbName name = do
    guardMutation
    dbManager <- asks aeDbManager
    liftIO (setUploadDisplayName dbManager dbName name) >>= either settingRefused pure

{- | Describe an uploaded database, or clear its description with @null@ or a
blank text, and answer with the description kept. 404 for a name the engine
does not know, 409 for a database the configuration file declares, 500 for an
upload whose meta.toml cannot be read.
-}
setDescriptionHandler :: Text -> Maybe Text -> AppM (Maybe Text)
setDescriptionHandler dbName description = do
    guardMutation
    dbManager <- asks aeDbManager
    liftIO (setUploadDescription dbManager dbName description) >>= either settingRefused pure

{- | Accept a database in place of a release an uploaded database requires,
and answer with its setup as it now links. 404 for a name the engine does not
know or a release it does not require, 409 for a database loaded with the
links it has, 500 for an upload whose meta.toml cannot be read.
-}
acceptSubstitutionHandler :: Text -> ReleaseSubstitution -> AppM DatabaseSetupInfo
acceptSubstitutionHandler dbName substitution = do
    guardMutation
    dbManager <- asks aeDbManager
    liftIO (acceptSubstitution dbManager dbName substitution) >>= either settingRefused pure
    -- Staged again apart: a failure there is the setup's, answered as the setup route answers it.
    getDatabaseSetupHandler dbName

-- | The status a refused setting answers with.
settingRefused :: SettingRefusal -> AppM a
settingRefused (SettingUnknown msg) = failWith err404 msg
settingRefused (SettingHeldElsewhere msg) = failWith err409 msg
settingRefused (SettingUnrecordable msg) = failWith err500 msg

{- | Export a loaded method collection over the same transport as the database
export: raw octet-stream body, projection warnings percent-encoded in the
@X-Volca-Export-Warnings@ header, 400 for a format without a method writer,
404 for a collection that is not loaded.
-}
exportMethodHandler :: Text -> ExportRequest -> AppM (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
exportMethodHandler name req = do
    dbManager <- asks aeDbManager
    fmt <- either (failWith err400) pure (parseMethodExportFormat (exrFormat req))
    mColl <- liftIO (getMethodCollection dbManager name)
    coll <- maybe (failWith err404 ("Method collection not loaded: " <> name)) pure mColl
    (bytes, warnings) <- either (failWith err400) pure (serializeMethodCollection fmt name coll)
    pure (addHeader (encodeExportWarnings warnings) (BinaryContent bytes))

{- | Join export warnings for the response header, percent-encoded because
flow and activity names are arbitrary Unicode.

A header is not a log. An ILCD export of a full database names every activity it
approximated, which on a twenty-thousand-activity database runs to hundreds of kilobytes,
and a header that long is refused by every proxy and HTTP client on the way back: the
archive itself is lost along with the warnings. So the header carries as many whole
warnings as fit in 'warningHeaderBudget' and then says how many it left out, which is the
one thing a caller cannot work out for itself.
-}
encodeExportWarnings :: [Text] -> Text
encodeExportWarnings warnings = encode (kept ++ [omitted | not (null dropped)])
  where
    encode :: [Text] -> Text
    encode = T.decodeUtf8 . urlEncode False . T.encodeUtf8 . T.intercalate "\n"

    kept :: [Text]
    kept = fitting warningHeaderBudget warnings

    dropped :: [Text]
    dropped = drop (length kept) warnings

    omitted :: Text
    omitted = "and " <> T.pack (show n) <> plural <> ", too many to carry in a header"
      where
        n :: Int
        n = length dropped

        plural :: Text
        plural = if n == 1 then " further warning" else " further warnings"

    -- The longest prefix whose encoding stays inside the budget, measured warning by
    -- warning so the header always ends on a whole one.
    fitting :: Int -> [Text] -> [Text]
    fitting _ [] = []
    fitting left (w : rest)
        | cost > left = []
        | otherwise = w : fitting (left - cost) rest
      where
        cost :: Int
        cost = T.length (encode [w]) + 3 -- the encoded newline that joins it

{- | How much encoded text the warning header may hold. Well under the 4 kB a reverse
proxy typically allows one response header, which is itself well under what an HTTP
client will read.
-}
warningHeaderBudget :: Int
warningHeaderBudget = 3000

-- | Fail with this status, the sentence as the body a client shows.
failWith :: ServerError -> Text -> AppM a
failWith status msg = throwError status{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

{- | The message to refuse with when a hosting quota is already met, or
'Nothing' when the operation is within budget.

A negative limit is unlimited and an absent hosting config is local/CLI use,
where no quota applies. The count is read before the action rather than
transactionally with it, so two concurrent requests can each take a last
remaining slot, an off-by-one acceptable for the single-caller instances
this guards.

Pure so the policy can be tested without a server, and shared so the storage
and memory budgets cannot drift into different rules.
-}
hostingQuotaRefusal ::
    (HostingConfig -> Int) ->
    (HostingConfig -> Text) ->
    Text ->
    Int ->
    Maybe HostingConfig ->
    Maybe Text
hostingQuotaRefusal limitOf messageOf fallback current mHosting = case mHosting of
    Nothing -> Nothing
    Just hc
        | limitOf hc < 0 -> Nothing
        | current < limitOf hc -> Nothing
        | otherwise -> Just (messageOr fallback (messageOf hc))

-- | Refuse a new upload once the plan's stored-database budget is used up.
uploadRefusal :: [Text] -> Maybe HostingConfig -> Maybe Text
uploadRefusal uploaded =
    hostingQuotaRefusal
        hcMaxUploads
        hcUpgradeUpload
        "You have reached the number of databases this plan can store. Delete one to add another."
        (length uploaded)

-- | Refuse a load once the plan's in-memory budget for uploads is used up.
memoryRefusal :: [Text] -> Maybe HostingConfig -> Maybe Text
memoryRefusal loadedUploads =
    hostingQuotaRefusal
        hcMaxLoadedUploads
        hcUpgradeVmSize
        "This plan cannot hold more uploaded databases in memory. Unload one first."
        (length loadedUploads)

{- | Whether loading @dbName@ would overrun the in-memory budget.

Only a database the user uploaded spends that budget: the databases the
config declares are what an uploaded inventory links against, so gating their
loads would make the quota forbid the very thing uploading is for. Re-loading
an uploaded database already in memory is never refused by its own presence.
-}
loadRefusal :: [Text] -> [Text] -> Text -> Maybe HostingConfig -> Maybe Text
loadRefusal uploaded loadedUploads dbName
    | dbName `notElem` uploaded = const Nothing
    | dbName `elem` loadedUploads = const Nothing
    | otherwise = memoryRefusal loadedUploads

{- | Whether a copy would overrun either budget. A copy lands as a new
uploaded database that is already loaded, so it spends both: the stored
budget an upload does, and the memory budget a load does.
-}
copyRefusal :: [Text] -> [Text] -> Maybe HostingConfig -> Maybe Text
copyRefusal uploaded loadedUploads mHosting =
    uploadRefusal uploaded mHosting <|> memoryRefusal loadedUploads mHosting

{- | The databases the user brought (as opposed to those the config declares),
and those of them currently held in memory: the two counts every quota above
is judged against.
-}
quotaCounts :: DatabaseManager -> IO ([Text], [Text])
quotaCounts dbManager = do
    uploaded <- M.keys . M.filter dcIsUploaded <$> readTVarIO (dmAvailableDbs dbManager)
    loaded <- readTVarIO (dmLoadedDbs dbManager)
    pure (uploaded, filter (`M.member` loaded) uploaded)

{- | 'loadRefusal' read off the manager's current state. Every surface that
loads a database, REST and MCP alike, goes through this, so the budget
cannot be sidestepped by picking another door.
-}
loadQuotaRefusal :: DatabaseManager -> Maybe HostingConfig -> Text -> IO (Maybe Text)
loadQuotaRefusal dbManager mHosting dbName = do
    (uploaded, loadedUploads) <- quotaCounts dbManager
    pure (loadRefusal uploaded loadedUploads dbName mHosting)

{- | Resolve the hosting upload-size policy into a streaming byte cap.
Local/CLI mode (no hosting config) is unlimited. A configured limit of 0
disables uploads; a negative limit is unlimited; a positive limit caps the
size in megabytes. 'Left' rejects the upload outright (disabled plan); 'Right
Nothing' means no cap; 'Right (Just n)' caps the streamed body at n bytes.
-}
uploadSizeCap :: Maybe HostingConfig -> Either Text (Maybe Int)
uploadSizeCap Nothing = Right Nothing
uploadSizeCap (Just hc) =
    case hcMaxUploadMb hc of
        0 -> Left "Uploads are disabled on this plan."
        limitMb
            | limitMb < 0 -> Right Nothing
            | otherwise -> Right (Just (limitMb * 1024 * 1024))

{- | The WAI-level request-body ceiling (in bytes) for a request path, or
'Nothing' for no limit. This is the outer, hard backstop for the in-handler
streaming size check ('withStreamedUpload'): only the database and method upload
routes are bounded, and only when the hosting config sets a positive cap. The
body is now a raw octet-stream (no base64 inflation, no JSON envelope), so we
admit the policy limit plus 1 MiB of slack: files between the real limit and
that ceiling still reach the handler, which streams and returns the precise
rejection. Unlimited (-1), disabled (0), and local/CLI (no config) are left
unbounded here: neither unlimited nor disabled is a size bound, and the handler
still rejects disabled uploads.
-}
uploadBodyCeiling :: Maybe HostingConfig -> [Text] -> Maybe Word64
uploadBodyCeiling hostingConfig path
    | not (isPolicyUploadPath path) = Nothing
    | otherwise = case hostingConfig of
        Nothing -> Nothing
        Just hc
            | hcMaxUploadMb hc > 0 -> Just ((fromIntegral (hcMaxUploadMb hc) + 1) * 1024 * 1024)
            | otherwise -> Nothing

{- | The upload routes governed by the size policy. These mirror the @db/upload@
and @method-collections/upload@ endpoints in "API.Routes"; the reference-data CSV
upload routes are intentionally excluded (small files, outside the policy).
-}
isPolicyUploadPath :: [Text] -> Bool
isPolicyUploadPath path =
    path
        `elem` [ ["api", "v1", "db", "upload"]
               , ["api", "v1", "method-collections", "upload"]
               ]

{- | Stream an octet-stream upload body to a temp file, enforcing the hosting
size policy as the bytes arrive, then hand @(name, description, file bytes)@ to
the continuation. Shared by the database and method upload handlers so both
gate on one rule and never buffer the whole payload in memory. The name is
required (it travels as the @?name=@ query parameter).
-}
withStreamedUpload ::
    Maybe Text ->
    Maybe Text ->
    SourceIO UploadChunk ->
    (Text -> Maybe Text -> BSL.ByteString -> AppM UploadResponse) ->
    AppM UploadResponse
withStreamedUpload mName mDesc src k = do
    guardMutation
    name <- maybe (failWith err400 "Missing upload name. Pass it as the ?name= query parameter.") pure (mfilter (not . T.null) (T.strip <$> mName))
    mCap <- asks aeHostingConfig >>= either (failWith err403) pure . uploadSizeCap
    tmpPath <- liftIO (streamToTempFile mCap src) >>= either (uncurry failWith . rejectionAnswer) pure
    -- The read is lazy, so an uncapped (local/desktop) upload is never
    -- buffered whole; 'finally' guarantees the temp file is deleted even
    -- if the extract/detect continuation throws.
    (liftIO (BSL.readFile tmpPath) >>= k name (nonBlankText =<< mDesc))
        `finally` liftIO (removeQuietly tmpPath)

-- | Why a streamed upload body was not kept.
data UploadRejection
    = -- | The body passed the plan's cap, in bytes.
      TooLarge Int
    | StreamFailed Text
    deriving (Eq, Show)

-- | The status and sentence a rejected upload answers with.
rejectionAnswer :: UploadRejection -> (ServerError, Text)
rejectionAnswer = \case
    TooLarge cap ->
        ( err413
        , "File too large. The upload limit on this plan is "
            <> T.pack (show (cap `div` (1024 * 1024)))
            <> " MB."
        )
    StreamFailed reason -> (err400, "Upload stream error: " <> reason)

{- | Fold a streamed octet-stream body into a fresh temp file, aborting with a
rejection if the running byte count exceeds the cap. Returns the temp file
path on success (the caller deletes it). Bytes are written chunk-by-chunk and
never held whole in memory.
-}
streamToTempFile :: Maybe Int -> SourceIO UploadChunk -> IO (Either UploadRejection FilePath)
streamToTempFile mCap src = do
    tmpDir <- System.Directory.getTemporaryDirectory
    (tmpPath, h) <- openBinaryTempFile tmpDir "volca-upload-.bin"
    result <- try (S.unSourceT src (go h 0)) :: IO (Either SomeException (Either UploadRejection ()))
    hClose h
    case result of
        Left e -> removeQuietly tmpPath >> return (Left (StreamFailed (T.pack (show e))))
        Right (Left rejection) -> removeQuietly tmpPath >> return (Left rejection)
        Right (Right ()) -> return (Right tmpPath)
  where
    go h !n step = case step of
        S.Stop -> return (Right ())
        S.Error e -> return (Left (StreamFailed (T.pack e)))
        S.Skip s -> go h n s
        S.Effect ms -> ms >>= go h n
        S.Yield chunk s ->
            let !n' = n + BS.length (unUploadChunk chunk)
             in case mCap of
                    Just cap | n' > cap -> return (Left (TooLarge cap))
                    _ -> BS.hPut h (unUploadChunk chunk) >> go h n' s

-- | Delete a file, swallowing any error: best-effort temp-file cleanup.
removeQuietly :: FilePath -> IO ()
removeQuietly p = void (try (System.Directory.removeFile p) :: IO (Either SomeException ()))

{- | Upload a new database (streamed octet-stream body; metadata in query params).

The stored-database quota is checked before the body is read, like the size
cap: there is no point receiving a hundred megabytes only to refuse them.
-}
uploadDatabaseHandler :: Maybe Text -> Maybe Text -> SourceIO UploadChunk -> AppM UploadResponse
uploadDatabaseHandler mName mDesc src = do
    guardMutation -- read-only outranks any quota message
    dbManager0 <- asks aeDbManager
    hostingConfig <- asks aeHostingConfig
    refusal <- liftIO $ do
        (uploaded, _) <- quotaCounts dbManager0
        pure (uploadRefusal uploaded hostingConfig)
    mapM_ (failWith err403) refusal
    withStreamedUpload mName mDesc src $ \name mDescription zipBytes ->
        either (failWith err400) (uploadUnpacked name mDescription zipBytes) (openPackage zipBytes)
  where
    -- A package brings its export, the licence it was published under and
    -- the releases it was built on; anything else is the export itself.
    uploadUnpacked :: Text -> Maybe Text -> BSL.ByteString -> Maybe Package -> AppM UploadResponse
    uploadUnpacked name mDescription zipBytes package = do
        dbManager <- asks aeDbManager
        let uploadData =
                UploadData
                    { udName = name
                    , udDescription = mDescription
                    , udZipData = maybe zipBytes pkPayload package
                    }
            licence = maybe LicenceUnstated pkLicence package
            requires = maybe [] (map (`Requirement` Nothing) . pkRequires) package
        -- Handle the upload (extract, detect format)
        uploadsDir <- liftIO UploadedDB.getDatabaseUploadsDir
        result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())

        case result of
            Left err -> failWith err400 err
            Right uploadResult -> do
                let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)
                added <- liftIO addedNow

                -- Create meta.toml for self-describing upload
                let meta =
                        UploadedDB.UploadMeta
                            { UploadedDB.umVersion = UploadedDB.metaVersion
                            , UploadedDB.umDisplayName = name
                            , UploadedDB.umDescription = mDescription
                            , UploadedDB.umFormat = urFormat uploadResult -- Types are now unified
                            , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                            , UploadedDB.umDepends = []
                            , UploadedDB.umSource = Nothing
                            , UploadedDB.umReading = FromFiles ReadDeclared
                            , UploadedDB.umBuiltIn = Nothing
                            , UploadedDB.umLicence = licence
                            , UploadedDB.umRelease = Nothing
                            , UploadedDB.umRequires = requires
                            , UploadedDB.umPackageMethod = Nothing
                            , UploadedDB.umAdded = Just added
                            }
                liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                -- Create database config for in-memory manager
                let dbConfig =
                        DatabaseConfig
                            { dcName = urSlug uploadResult
                            , dcDisplayName = name
                            , dcPath = urPath uploadResult
                            , dcDescription = mDescription
                            , dcLoad = False -- Don't auto-load
                            , dcDefault = False
                            , dcDepends = []
                            , dcLocationAliases = M.empty
                            , dcFormat = Just (urFormat uploadResult)
                            , dcIsUploaded = True -- Freshly uploaded database
                            , dcDeletable = True
                            , dcGeographyPolicy = GeoGlobal
                            , dcReading = FromFiles ReadDeclared
                            , dcPatches = []
                            , dcSource = Nothing
                            , dcLicence = licence
                            , dcRelease = Nothing
                            , dcRequires = requires
                            , dcAdded = Just added
                            }

                -- Add to manager
                liftIO $ addDatabase dbManager dbConfig

                return $
                    UploadResponse
                        True
                        "Database uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)
                        []
                        Nothing

-- | Convert DatabaseManager.DatabaseStatus to API.DatabaseStatusAPI
convertDbStatus :: DatabaseStatus -> DatabaseStatusAPI
convertDbStatus ds =
    DatabaseStatusAPI
        { dsaName = dsName ds
        , dsaDisplayName = dsDisplayName ds
        , dsaDescription = dsDescription ds
        , dsaLoadAtStartup = dsLoadAtStartup ds
        , dsaStatus = statusToText (dsStatus ds)
        , dsaIsUploaded = dsIsUploaded ds
        , dsaPath = dsPath ds
        , dsaFormat = formatDisplayText <$> dsFormat ds
        , dsaActivityCount = dsActivityCount ds
        , dsaDependsOn = dsDependsOn ds
        , dsaAllocation = allocationKeyText (dsAllocation ds)
        , dsaSystemModel = dsSystemModel ds
        , dsaSource = dsSource ds
        , dsaLicence = dsLicence ds
        , dsaRelease = dsRelease ds
        , dsaModification = dsModification ds
        , dsaAdded = addedText <$> dsAdded ds
        }
  where
    statusToText Unloaded = "unloaded"
    statusToText PartiallyLinked = "partially_linked"
    statusToText Loaded = "loaded"

-- | The status of a database just loaded, under the terms in force now.
loadedStatus :: DatabaseManager -> LoadedDatabase -> IO DatabaseStatusAPI
loadedStatus manager loaded = do
    configs <- readTVarIO (dmAvailableDbs manager)
    modification <- databaseModification configs (ldConfig loaded)
    pure (makeStatusFromLoadedDb configs modification loaded)

{- | Create DatabaseStatusAPI from a loaded database (derives status from linking
stats). The configurations are there for its terms, which a copy takes from its
source.
-}
makeStatusFromLoadedDb :: M.Map Text DatabaseConfig -> Modification -> LoadedDatabase -> DatabaseStatusAPI
makeStatusFromLoadedDb configs modification loaded =
    let config = ldConfig loaded
        db = ldDatabase loaded
        status =
            if unresolvedCount (dbLinkingStats db) > 0
                then "partially_linked"
                else "loaded"
     in DatabaseStatusAPI
            { dsaName = dcName config
            , dsaDisplayName = dcDisplayName config
            , dsaDescription = dcDescription config
            , dsaLoadAtStartup = dcLoad config
            , dsaStatus = status
            , dsaIsUploaded = dcIsUploaded config
            , dsaPath = T.pack (dcPath config)
            , dsaFormat = formatDisplayText <$> dcFormat config
            , dsaActivityCount = V.length (dbActivities db)
            , dsaDependsOn = dcDepends config
            , dsaAllocation = allocationKeyText (readingKey (dcReading config))
            , dsaSystemModel = modelTitle <$> modelOf (dcReading config)
            , dsaSource = dcSource config
            , dsaLicence = licenceOf configs config
            , dsaRelease = dcRelease config
            , dsaModification = modification
            , dsaAdded = addedText <$> dcAdded config
            }

-- uploadFormatToMeta removed - types are now unified (UploadedDB re-exports from Upload)

-- | Make a path relative to a base directory
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path
    | base `isPrefixOf` path = drop (length base + 1) path -- +1 for separator
    | otherwise = path

-- | Convert DatabaseFormat to API slug text
formatToText :: DatabaseFormat -> Text
formatToText SimaProCSV = "simapro-csv"
formatToText EcoSpold1 = "ecospold1"
formatToText EcoSpold2 = "ecospold2"
formatToText ILCDProcess = "ilcd"
formatToText OpenLcaImpactCategory = "openlca-jsonld"
formatToText OpenLcaPackage = "openlca-package"
formatToText BrightwayExcel = "brightway-excel"
formatToText ColumnarCSV = "columnar-csv"
formatToText UnknownFormat = "unknown"

--------------------------------------------------------------------------------
-- Setup Page Handlers
--------------------------------------------------------------------------------

{- | Get database setup info
Returns completeness, missing suppliers, and dependency suggestions
-}
getDatabaseSetupHandler :: Text -> AppM DatabaseSetupInfo
getDatabaseSetupHandler dbName = do
    dbManager <- asks aeDbManager
    eitherResult <- liftIO $ try $ getDatabaseSetupInfo dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            throwError $ err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ "Setup failed: " <> T.pack (show ex)}
        Right (Left (SetupNotFound msg)) -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        -- Same 404 + "Database not loaded: " body as a read route asked about an
        -- unloaded database, so typed-error recovery on the client keeps working.
        Right (Left e@(SetupNotLoaded _)) -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 (setupErrorMessage e)}
        Right (Left (SetupFailed msg)) -> throwError $ err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Right (Right setupInfo) -> return setupInfo

{- | Add a dependency to a staged database
Runs cross-DB linking and returns updated setup info
-}
addDependencyHandler :: Text -> Text -> AppM DatabaseSetupInfo
addDependencyHandler dbName depName = do
    guardMutation
    dbManager <- asks aeDbManager
    ioEither400 (addDependencyToStaged dbManager (DependencyEdit dbName depName))

{- | Remove a dependency from a staged database
Re-runs cross-DB linking and returns updated setup info
-}
removeDependencyHandler :: Text -> Text -> AppM DatabaseSetupInfo
removeDependencyHandler dbName depName = do
    guardMutation
    dbManager <- asks aeDbManager
    ioEither400 (removeDependencyFromStaged dbManager (DependencyEdit dbName depName))

-- | Change the data path for an uploaded (staged) database
setDataPathHandler :: Text -> Value -> AppM DatabaseSetupInfo
setDataPathHandler dbName body = do
    guardMutation
    dbManager <- asks aeDbManager
    -- Extract "path" from JSON body
    let mPath = case body of
            A.Object obj -> case KM.lookup "path" obj of
                Just (A.String p) -> Just p
                _ -> Nothing
            _ -> Nothing
    case mPath of
        Nothing -> throwError $ err400{errBody = "Missing \"path\" field in request body"}
        Just newPath -> ioEither400 (setDataPath dbManager dbName (RelativeDataPath newPath))

{- | Finalize a staged database
Builds matrices and makes it ready for queries
-}
finalizeDatabaseHandler :: Text -> AppM ActivateResponse
finalizeDatabaseHandler dbName = do
    guardMutation
    dbManager <- asks aeDbManager
    loaded <-
        liftIO (try (finalizeDatabase dbManager dbName)) >>= \case
            Left (ex :: SomeException) -> failWith err500 ("Server exception: " <> T.pack (show ex))
            Right result -> either (failWith err400) pure result
    status <- liftIO (loadedStatus dbManager loaded)
    pure (ActivateResponse True ("Finalized database: " <> dcDisplayName (ldConfig loaded)) (Just status))

{- | Upload a new method collection
Same flow as database upload but creates MethodConfig entry
-}
uploadMethodHandler :: Maybe Text -> Maybe Text -> Maybe Text -> SourceIO UploadChunk -> AppM UploadResponse
uploadMethodHandler mName mDesc flowsFrom src =
    withStreamedUpload mName mDesc src $ \name mDescription zipBytes -> do
        dbManager <- asks aeDbManager
        let uploadData =
                UploadData
                    { udName = name
                    , udDescription = mDescription
                    , udZipData = zipBytes
                    }
        uploadsDir <- liftIO UploadedDB.getMethodUploadsDir
        result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())
        case result of
            Left err -> failWith err400 err
            Right uploadResult -> do
                let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)
                factorTable <- liftIO (findFactorTable uploadDir)
                case factorTable of
                    -- The table is read once, into collections of their own, and then let go.
                    Just workbook -> uploadFactorTable dbManager uploadsDir name mDescription flowsFrom workbook `finally` liftIO (removeTreeQuietly uploadDir)
                    Nothing -> do
                        when (isJust flowsFrom) $ do
                            liftIO (removeTreeQuietly uploadDir)
                            failWith err400 "flows_from applies to a table of characterization factors only, and this upload holds none."
                        uploadMethodFiles dbManager uploadDir name mDescription uploadResult

-- | The method collections the catalogue offers, each saying whether the engine already knows it.
availableMethodCollectionsHandler :: AppM [AvailableMethodCollection]
availableMethodCollectionsHandler = do
    entries <- either (failWith err500) pure methodCatalog
    known <- knownMethodCollections
    pure
        [ AvailableMethodCollection
            { amcName = ceName e
            , amcPublisher = cePublisher e
            , amcVersion = ceVersion e
            , amcLicence = spdxId <$> ceLicence e
            , amcSourceUrl = ceSourceUrl e
            , amcSize = ceSize e
            , amcDescription = ceDescription e
            , amcInstalled = ceName e `elem` known
            }
        | e <- entries
        ]

{- | Download a method collection of the catalogue, keep it only if it is the
file the catalogue states, then store and load it as an upload of that name.
A collection the engine already knows under that name, uploaded or
configured, is not downloaded again. The plan's upload cap applies, as to a
file sent.
-}
installMethodCollectionHandler :: Text -> AppM ActivateResponse
installMethodCollectionHandler name = do
    guardMutation
    entries <- either (failWith err500) pure methodCatalog
    entry <- maybe (failWith err404 ("The catalogue offers no method collection named " <> name <> ".")) pure (find ((== name) . ceName) entries)
    known <- knownMethodCollections
    when (name `elem` known) $ failWith err409 ("A method collection named " <> name <> " is already installed.")
    mCap <- asks aeHostingConfig >>= either (failWith err403) pure . uploadSizeCap
    forM_ mCap $ \cap -> when (ceSize entry > cap) (uncurry failWith (rejectionAnswer (TooLarge cap)))
    dbManager <- asks aeDbManager
    tmpPath <- liftIO $ do
        tmpDir <- System.Directory.getTemporaryDirectory
        (path, h) <- openBinaryTempFile tmpDir "volca-method-.bin"
        hClose h
        pure path
    ( do
            liftIO (fetchEntry entry tmpPath) >>= either (uncurry failWith . fetchAnswer) pure
            bytes <- liftIO (BSL.readFile tmpPath)
            uploadsDir <- liftIO UploadedDB.getMethodUploadsDir
            let description = Just (ceDescription entry)
            uploaded <- liftIO $ handleUpload uploadsDir (UploadData name description bytes) (\_ -> pure ())
            uploadResult <- either (failWith err400) pure uploaded
            void (uploadMethodFiles dbManager (uploadsDir </> T.unpack (urSlug uploadResult)) name description uploadResult)
            ioEither400 (loadMethodCollection dbManager name)
        )
        `finally` liftIO (removeQuietly tmpPath)
    pure (ActivateResponse True ("Installed and loaded method: " <> name) Nothing)
  where
    fetchAnswer :: FetchFailure -> (ServerError, Text)
    fetchAnswer (Unreachable why) = (err502, why)
    fetchAnswer (NotAsCatalogued why) = (err422, why)

knownMethodCollections :: AppM [Text]
knownMethodCollections = asks aeDbManager >>= liftIO . fmap (map mcsName) . listMethodCollections

{- | Upload a method collection as its files: ILCD, openLCA, SimaPro or
columnar CSV, read again at each activation.
-}
uploadMethodFiles :: DatabaseManager -> FilePath -> Text -> Maybe Text -> UploadResult -> AppM UploadResponse
uploadMethodFiles dbManager uploadDir name mDescription uploadResult = do
    -- Find the actual method XML directory (e.g. ILCD/lciamethods/)
    methodDir <- liftIO $ findMethodDirectory uploadDir
    -- Format comes from the method directory, not from the database
    -- detector: companion spreadsheets shipped alongside a method
    -- package would otherwise read as a Brightway Excel inventory.
    methodFormat <- liftIO $ detectMethodFormat methodDir
    -- An upload names no method, so a package of several is refused here rather than at activation.
    package <- liftIO $ isOlcaMethodPackage methodDir
    when package $
        liftIO (loadOlcaPackageMethod Nothing methodDir) >>= \case
            Right _ -> pure ()
            Left err -> do
                liftIO (removeTreeQuietly uploadDir)
                failWith err400 ("An uploaded openLCA package must hold one method: " <> err <> ". Export that method alone from openLCA.")

    added <- liftIO addedNow
    liftIO $ registerUploadedMethod dbManager name mDescription methodFormat uploadDir methodDir added
    return $
        UploadResponse
            True
            "Method uploaded successfully"
            (Just $ urSlug uploadResult)
            (Just $ formatToText $ urFormat uploadResult)
            []
            Nothing

{- | Record a method collection uploaded into a directory of its own: its
meta.toml, so a restart finds it again, and its place among the collections,
inactive.
-}
registerUploadedMethod :: DatabaseManager -> Text -> Maybe Text -> DatabaseFormat -> FilePath -> FilePath -> AddedAt -> IO ()
registerUploadedMethod dbManager name mDescription methodFormat uploadDir methodDir added = do
    UploadedDB.writeUploadMeta
        uploadDir
        UploadedDB.UploadMeta
            { UploadedDB.umVersion = UploadedDB.metaVersion
            , UploadedDB.umDisplayName = name
            , UploadedDB.umDescription = mDescription
            , UploadedDB.umFormat = methodFormat
            , UploadedDB.umDataPath = makeRelative uploadDir methodDir
            , UploadedDB.umDepends = []
            , UploadedDB.umSource = Nothing
            , UploadedDB.umReading = FromFiles ReadDeclared
            , UploadedDB.umBuiltIn = Nothing
            , UploadedDB.umLicence = LicenceUnstated
            , UploadedDB.umRelease = Nothing
            , UploadedDB.umRequires = []
            , UploadedDB.umPackageMethod = Nothing
            , UploadedDB.umAdded = Just added
            }
    addMethodCollection
        dbManager
        MethodConfig
            { mcName = name
            , mcOrigin = MethodFromFile methodDir
            , mcActive = False
            , mcDefault = False
            , mcHome = Just uploadDir
            , mcSource = Nothing
            , mcDescription = mDescription
            , mcAdded = Just added
            , mcFormat = detectedFormatLabel methodFormat
            , mcScoringSets = []
            , mcGlobalMethods = []
            , mcPatches = []
            , mcPackageMethod = Nothing
            }

-- | The first workbook of the upload that is a table of characterization factors.
findFactorTable :: FilePath -> IO (Maybe Workbook)
findFactorTable uploadDir = do
    files <- listDirectoryRecursive uploadDir
    firstFactorTable [f | f <- files, map toLower (takeExtension f) == ".xlsx"]
  where
    firstFactorTable [] = pure Nothing
    firstFactorTable (f : rest) = do
        workbook <- openWorkbook <$> BSL.readFile f
        case workbook of
            Right wb | isFactorTable wb -> pure (Just wb)
            _ -> firstFactorTable rest

{- | Upload a table of characterization factors: each method family it holds
becomes a columnar CSV collection of its own, named "<name> · <family>". The
units and CAS numbers of its flows come from the EcoSpold 2 master data of a
database ('chooseMasterData'). Nothing is written unless every family can be.
-}
uploadFactorTable :: DatabaseManager -> FilePath -> Text -> Maybe Text -> Maybe Text -> Workbook -> AppM UploadResponse
uploadFactorTable dbManager uploadsDir name mDescription flowsFrom workbook = do
    databases <- liftIO (readTVarIO (dmAvailableDbs dbManager))
    candidates <- liftIO $ traverse (\(db, cfg) -> (,) db <$> masterDataOf cfg) (M.toList databases)
    masterPath <- either (failWith err400) pure (chooseMasterData flowsFrom candidates)
    masterFlows <-
        liftIO (BS.readFile masterPath)
            >>= either (\e -> failWith err400 (T.pack masterPath <> ": " <> T.pack e)) pure . parseElementaryExchanges
    (families, dropped) <- either (failWith err400) pure (familyCollections masterFlows =<< readFactorTable workbook)
    let named = [(name <> " · " <> family, collection) | (family, collection) <- families]
        dirOf collectionName = uploadsDir </> T.unpack (slugify collectionName)
    -- Two families whose names differ only in punctuation would share one folder.
    case repeated (map (slugify . fst) named) of
        [] -> pure ()
        clashing -> failWith err400 ("Several families of this table would be stored under the same folder: " <> T.intercalate ", " clashing <> ".")
    available <- liftIO (readTVarIO (dmAvailableMethods dbManager))
    taken <- liftIO $ filterM (\(n, _) -> (M.member n available ||) <$> System.Directory.doesPathExist (dirOf n)) named
    unless (null taken) $
        failWith err409 ("These collections already exist: " <> T.intercalate ", " (map fst taken) <> ". Delete them, or upload under another name.")
    written <- either (failWith err400) pure (traverse (\(n, c) -> (,,) n c . fst <$> serializeColumnarMethodCSV c) named)
    added <- liftIO addedNow
    liftIO $ forM_ written $ \(n, _, bytes) -> do
        let dir = dirOf n
        System.Directory.createDirectoryIfMissing True dir
        BS.writeFile (dir </> "methods.csv") bytes
        registerUploadedMethod dbManager n mDescription ColumnarCSV dir dir added
    pure
        UploadResponse
            { uprSuccess = True
            , uprMessage = "Method families uploaded successfully"
            , uprSlug = Nothing
            , uprFormat = Just (formatToText ColumnarCSV)
            , uprFamilies = [UploadedFamily n (slugify n) (sum (map (length . methodFactors) (mcMethods c))) | (n, c, _) <- written]
            , uprDropped = Just (DroppedFactors (dropCount dropped) (dropExamples dropped))
            }

{- | Where a database keeps its EcoSpold 2 master data, if it does: in a
@MasterData@ directory within the directory it names, or beside it.
-}
masterDataOf :: DatabaseConfig -> IO (Maybe FilePath)
masterDataOf cfg = firstExisting [dir </> "MasterData" </> "ElementaryExchanges.xml" | dir <- [dcPath cfg, takeDirectory (dcPath cfg)]]
  where
    firstExisting [] = pure Nothing
    firstExisting (p : rest) = System.Directory.doesFileExist p >>= \found -> if found then pure (Just p) else firstExisting rest

{- | The master data a table of factors takes its units and CAS numbers from:
that of the database named, or, when none is, of the only database that has
one.
-}
chooseMasterData :: Maybe Text -> [(Text, Maybe FilePath)] -> Either Text FilePath
chooseMasterData flowsFrom candidates = case flowsFrom of
    Just db -> case lookup db candidates of
        Nothing -> Left ("No database is named " <> db <> "." <> offered)
        Just Nothing -> Left (db <> " has no EcoSpold 2 master data (MasterData/ElementaryExchanges.xml)." <> offered)
        Just (Just path) -> Right path
    Nothing -> case withMasterData of
        [(_, path)] -> Right path
        [] -> Left "A table of characterization factors takes the units and CAS numbers of its flows from the master data of an EcoSpold 2 database. Upload the EcoSpold 2 release this table was made for first."
        _ -> Left ("Several databases have EcoSpold 2 master data. Name the one this table was made for with flows_from." <> offered)
  where
    withMasterData = [(db, path) | (db, Just path) <- candidates]
    offered = if null withMasterData then "" else " Databases with EcoSpold 2 master data: " <> T.intercalate ", " (map fst withMasterData) <> "."

-- | Remove a directory and what it holds, whatever is left of it.
removeTreeQuietly :: FilePath -> IO ()
removeTreeQuietly p = void (try (System.Directory.removeDirectoryRecursive p) :: IO (Either SomeException ()))

-- | Delete an uploaded method collection
deleteMethodHandler :: Text -> AppM ActivateResponse
deleteMethodHandler name = do
    dbManager <- asks aeDbManager
    simpleAction (removeMethodCollection dbManager name) ("Deleted method: " <> name)

{- | Refuse the caller when this instance is configured read-only.

Every operation that changes state shared by all callers of the server calls
this first. The refusal is a 403 rather than a silent no-op so a client can
tell the difference between "done" and "not allowed here".
-}
guardMutation :: AppM ()
guardMutation = do
    hosting <- asks aeHostingConfig
    when (isReadOnly (hostingReadOnly hosting)) $
        throwError err403{errBody = BSL.fromStrict (T.encodeUtf8 (readOnlyRefusalFor hosting))}

{- | Common pattern: run an IO action that returns Either Text (), a failure
answered as a 400 with its sentence.

Every caller performs a state change, so the read-only guard lives here rather
than being repeated at each of them.
-}
simpleAction :: IO (Either Text ()) -> Text -> AppM ActivateResponse
simpleAction action successMsg = do
    guardMutation
    ioEither400 action
    pure (ActivateResponse True successMsg Nothing)

{- | @ioEither400 m@ runs an IO action that returns @Either Text a@; on
@Left@ throws a 400 with the message body, on @Right@ propagates. Used
to compress the @do result <- liftIO ...; case result of Left … Right …@
ladder that recurs across every handler returning a typed payload.
-}
ioEither400 :: IO (Either Text a) -> AppM a
ioEither400 action = do
    result <- liftIO action
    case result of
        Left err -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right v -> return v

--------------------------------------------------------------------------------
-- Reference Data Handlers (flow synonyms, compartment mappings, units)
--------------------------------------------------------------------------------

-- | Which kind of reference data we're operating on
data RefDataKind = FlowSynonyms | CompartmentMappings | UnitDefs

-- | Dispatch to the right Manager functions based on kind
rdOps ::
    RefDataKind ->
    ( DatabaseManager -> IO [RefDataStatus]
    , DatabaseManager -> Text -> IO (Either Text ())
    , DatabaseManager -> Text -> IO (Either Text ())
    , DatabaseManager -> RefDataConfig -> IO ()
    , DatabaseManager -> Text -> IO (Either Text ())
    , IO FilePath -- where uploads go
    )
rdOps FlowSynonyms =
    (listFlowSynonyms, loadFlowSynonyms, unloadFlowSynonyms, addFlowSynonyms, removeFlowSynonyms, flowSynonymsUploadsDir)
rdOps CompartmentMappings =
    (listCompartmentMappings, loadCompartmentMappings, unloadCompartmentMappings, addCompartmentMappings, removeCompartmentMappings, compartmentMappingsUploadsDir)
rdOps UnitDefs =
    (listUnitDefs, loadUnitDefs, unloadUnitDefs, addUnitDefs, removeUnitDefs, unitDefsUploadsDir)

convertRefDataStatus :: RefDataStatus -> RefDataStatusAPI
convertRefDataStatus s =
    RefDataStatusAPI
        { rdaName = rdsName s
        , rdaDisplayName = rdsDisplayName s
        , rdaDescription = rdsDescription s
        , rdaStatus = case rdsStatus s of Loaded -> "loaded"; _ -> "unloaded"
        , rdaIsUploaded = rdsIsUploaded s
        , rdaIsAuto = rdsIsAuto s
        , rdaEntryCount = rdsEntryCount s
        , rdaAdded = addedText <$> rdsAdded s
        }

listRefData :: RefDataKind -> AppM RefDataListResponse
listRefData kind = do
    dbManager <- asks aeDbManager
    let (listFn, _, _, _, _, _) = rdOps kind
    statuses <- liftIO $ listFn dbManager
    return $ RefDataListResponse (map convertRefDataStatus statuses)

loadRefData :: RefDataKind -> Text -> AppM ActivateResponse
loadRefData kind name = do
    dbManager <- asks aeDbManager
    let (_, loadFn, _, _, _, _) = rdOps kind
    simpleAction (loadFn dbManager name) ("Loaded: " <> name)

unloadRefData :: RefDataKind -> Text -> AppM ActivateResponse
unloadRefData kind name = do
    dbManager <- asks aeDbManager
    let (_, _, unloadFn, _, _, _) = rdOps kind
    simpleAction (unloadFn dbManager name) ("Unloaded: " <> name)

deleteRefData :: RefDataKind -> Text -> AppM ActivateResponse
deleteRefData kind name = do
    dbManager <- asks aeDbManager
    let (_, _, _, _, removeFn, _) = rdOps kind
    simpleAction (removeFn dbManager name) ("Deleted: " <> name)

uploadRefData :: RefDataKind -> Maybe Text -> Maybe Text -> SourceIO UploadChunk -> AppM UploadResponse
uploadRefData kind mName mDesc src =
    withStreamedUpload mName mDesc src $ \name mDescription csvBytes -> do
        dbManager <- asks aeDbManager
        let (_, _, _, addFn, _, uploadsDirOf) = rdOps kind
        uploadsDir <- liftIO uploadsDirOf
        added <- liftIO addedNow
        let slug = T.toLower $ T.intercalate "-" $ T.words name
            uploadDir = uploadsDir </> T.unpack slug
            csvPath = uploadDir </> "data.csv"
        liftIO $ do
            System.Directory.createDirectoryIfMissing True uploadDir
            BSL.writeFile csvPath csvBytes
            let metaContent =
                    T.intercalate
                        "\n"
                        [ "[meta]"
                        , "version = 1"
                        , "displayName = " <> quote name
                        , maybe "" (\d -> "description = " <> quote d) mDescription
                        , "added = " <> quote (addedText added)
                        , ""
                        ]
            T.writeFile (uploadDir </> "meta.toml") metaContent
        let rd =
                RefDataConfig
                    { rdName = name
                    , rdSource = FromFile csvPath
                    , rdActive = False
                    , rdIsUploaded = True
                    , rdIsAuto = False
                    , rdDescription = mDescription
                    , rdAdded = Just added
                    }
        liftIO $ addFn dbManager rd
        return $ UploadResponse True "Uploaded successfully" (Just slug) Nothing [] Nothing
  where
    quote t = "\"" <> T.replace "\"" "\\\"" t <> "\""

getFlowSynonymGroupsHandler :: Text -> AppM SynonymGroupsResponse
getFlowSynonymGroupsHandler name = do
    dbManager <- asks aeDbManager
    result <- liftIO $ getFlowSynonymGroups dbManager name
    case result of
        Left err -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right groups -> return $ SynonymGroupsResponse groups

downloadRefDataHandler :: RefDataKind -> Text -> AppM (Headers '[Header "Content-Disposition" Text] BinaryContent)
downloadRefDataHandler kind name = do
    content <- refDataSource kind name
    let disposition = "attachment; filename=\"" <> name <> ".csv\""
    return $ addHeader disposition (BinaryContent content)

-- | A reference table's rows as its CSV says them, loaded or not.
refDataRowsHandler :: RefDataKind -> Text -> AppM RefDataRowsResponse
refDataRowsHandler kind name = do
    content <- refDataSource kind name
    either (\err -> throwError err422{errBody = BSL.fromStrict (T.encodeUtf8 err)}) pure (refDataRows content)

{- | The header and rows of a reference CSV, all as wide as the widest of
them: a short row ends in empty cells (the compartments' @kind@ column is
optional), and a row wider than its header widens the header with empty
titles, since the synonym and compartment loaders never compare a row to the
header and take such a table.
-}
refDataRows :: BSL.ByteString -> Either Text RefDataRowsResponse
refDataRows content =
    either (Left . ("CSV parse error: " <>) . T.pack) (Right . table . V.toList) (Csv.decode Csv.NoHeader content)
  where
    table :: [[Text]] -> RefDataRowsResponse
    table [] = RefDataRowsResponse [] []
    table (header : rows) = RefDataRowsResponse (fitTo header) (map fitTo rows)
      where
        width = foldr (max . length) 0 (header : rows)
        fitTo :: [Text] -> [Text]
        fitTo cells = cells <> replicate (width - length cells) ""

-- | The CSV a reference table was read from, the built-in one included.
refDataSource :: RefDataKind -> Text -> AppM BSL.ByteString
refDataSource kind name = do
    dbManager <- asks aeDbManager
    let tvar = case kind of
            FlowSynonyms -> dmAvailableFlowSyns dbManager
            CompartmentMappings -> dmAvailableCompMaps dbManager
            UnitDefs -> dmAvailableUnitDefs dbManager
    available <- liftIO $ readTVarIO tvar
    case M.lookup name available of
        Nothing -> throwError $ err404{errBody = "Not found"}
        Just rd ->
            liftIO (readRefDataSource (rdSource rd))
                >>= either (const (throwError err404{errBody = "CSV file not found"})) pure

-- | The flow an explanation is about, as the cascade sees it.
explainedFlowAPI :: Database -> BiosphereFlow -> ExplainedFlowAPI
explainedFlowAPI db flow =
    ExplainedFlowAPI
        { eflId = UUID.toText (bfId flow)
        , eflName = bfName flow
        , eflUnit = getUnitNameForBioFlow (dbUnits db) flow
        , eflCategory = bfCompartmentName flow
        , eflCompartment = bfCompartmentSub flow
        , eflCas = bfCAS flow
        }

{- | Project an explanation onto the wire. One encoder for both surfaces: the
sentences come from the engine, the structured fields carry the same decision
in a form a client can compare or link on.
-}
explainCFToAPI :: Database -> Method -> BiosphereFlow -> Explain.CFExplanation -> ExplainCFResult
explainCFToAPI db method flow explanation =
    ExplainCFResult
        { ecrMethodId = methodId method
        , ecrMethod = methodName method
        , ecrMethodUnit = methodUnit method
        , ecrFlow = explainedFlowAPI db flow
        , ecrOutcome = Explain.outcomeName resolution
        , ecrExplanation = Explain.renderResolution resolution
        , ecrMatch = matchAPI resolution
        , ecrStepsTried = map stepAPI (Explain.ceTrail explanation)
        , ecrRegionalFactorCount = Explain.ceRegionalCFCount explanation
        }
  where
    resolution = Explain.ceResolution explanation

    matchAPI = \case
        Explain.Uncharacterized -> Nothing
        Explain.Characterized m bridge -> Just (baseMatch m){emaUnitConversion = Just (Explain.bridgeName bridge)}
        Explain.ConversionRefused m reason -> Just (baseMatch m){emaRefusal = Just (Explain.refusalName reason)}

    baseMatch m =
        let CF value (CFUnit unit) = Explain.cmCF m
            provenance = Explain.cmProvenance m
         in ExplainedMatchAPI
                { emaRung = Explain.rungName (Explain.cmRung m)
                , emaCfValue = value
                , emaCfUnit = unit
                , emaMethodFlowName = mcfFlowName (bpSource provenance)
                , emaMethodCas = mcfCAS (bpSource provenance)
                , emaMatchStrategy = provenanceStrategyText provenance
                , emaUnitConversion = Nothing
                , emaRefusal = Nothing
                }

    stepAPI step =
        ExplainedStepAPI
            { estRung = Explain.rungName (Explain.stRung step)
            , estResult = Explain.stepName (Explain.stResult step)
            , estVeto = Nothing
            }

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
    copyDatabaseHandler,
    deriveDatabaseHandler,
    deleteDatabaseHandler,
    deleteActivitiesHandler,
    createActivitiesHandler,
    replaceActivityHandler,
    editExchangesHandler,
    exportDatabaseHandler,
    exportMethodHandler,
    encodeExportWarnings,
    uploadDatabaseHandler,
    uploadMethodHandler,
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

    -- * Helpers
    convertDbStatus,
    guardMutation,
    uploadRefusal,
    memoryRefusal,
    loadRefusal,
    copyRefusal,
    quotaCounts,
    loadQuotaRefusal,
    simpleAction,
    formatToText,
    uploadSizeCap,
    uploadBodyCeiling,
    streamToTempFile,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (mfilter, void, when)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Word (Word64)
import Network.HTTP.Types.URI (urlEncode)
import Servant (Header, Headers, ServerError, SourceIO, addHeader, err400, err403, err404, err409, err500, errBody, throwError)
import qualified Servant.Types.SourceT as S
import qualified System.Directory
import System.FilePath ((</>))
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
    BinaryContent (..),
    BridgeGroupAPI (..),
    BridgedFlowAPI (..),
    CollectionBridgesAPI (..),
    ComputedQualityReportAPI (..),
    CoverageReportAPI (..),
    DatabaseListResponse (..),
    DatabaseStatusAPI (..),
    DeleteClassFilter (..),
    DeleteSelectionRequest (..),
    DeleteSelectionResponse (..),
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
    QualityCheckAPI (..),
    QualityOffenderAPI (..),
    QualityReportAPI (..),
    RefDataListResponse (..),
    RefDataStatusAPI (..),
    RelinkRequest (..),
    RelinkResponse (..),
    SynonymGroupsResponse (..),
    UploadChunk (..),
    UploadResponse (..),
    toAuthoredActivities,
    toExchangeEdits,
 )
import App.Env (AppEnv (..), AppM)
import Config (DatabaseConfig (..), HostingConfig (..), MethodConfig (..), ReadOnly (..), RefDataConfig (..), RefDataSource (..), hostingReadOnly, messageOr, readOnlyRefusalFor)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.Edit (
    DeleteOutcome (..),
    DeleteRequest (..),
    EditReport (..),
    WriteRefusal (..),
    WriteReport (..),
    WriteVerb (..),
    copyDatabase,
    deleteActivitiesInDB,
    deriveDatabase,
    editExchanges,
    refusalMessage,
    writeActivities,
 )
import Database.Export (parseExportFormat, parseMethodExportFormat, serializeDatabase, serializeMethodCollection)
import qualified Database.Loader as Loader
import Database.Manager (
    DatabaseLoadStatus (..),
    DatabaseManager (..),
    DatabaseSetupInfo (..),
    DatabaseStatus (..),
    DependencyEdit (..),
    LoadedDatabase (..),
    RefDataStatus (..),
    RelativeDataPath (..),
    RelinkResult (..),
    SetupError (..),
    addCompartmentMappings,
    addDatabase,
    addDependencyToStaged,
    addFlowSynonyms,
    addMethodCollection,
    addUnitDefs,
    databaseCoverageReport,
    databaseGapReport,
    databaseQualityReport,
    finalizeDatabase,
    getDatabase,
    getDatabaseSetupInfo,
    getFlowSynonymGroups,
    getMethodCollection,
    listCompartmentMappings,
    listDatabases,
    listFlowSynonyms,
    listUnitDefs,
    loadCompartmentMappings,
    loadDatabase,
    loadFlowSynonyms,
    loadUnitDefs,
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
    setupErrorMessage,
    unloadCompartmentMappings,
    unloadDatabase,
    unloadFlowSynonyms,
    unloadUnitDefs,
 )
import qualified Database.Quality as Quality
import Database.RelinkMapping (buildAliasMap, parseAliasCSV, rejectEmpty)
import Database.Upload (
    DatabaseFormat (..),
    UploadData (..),
    UploadResult (..),
    detectMethodFormat,
    detectedFormatLabel,
    findMethodDirectory,
    formatDisplayText,
    handleUpload,
 )
import qualified Database.UploadedDatabase as UploadedDB
import qualified Method.Coverage as Coverage
import qualified Method.Explain as Explain
import Method.Mapping (BuildProvenance (..), CF (..), CFUnit (..), provenanceStrategyText, strategyToText)
import Method.Types (Method (..), MethodCF (..))
import Types (
    AllocationKey (..),
    BiosphereFlow (..),
    BlockerReason (..),
    Database (..),
    GeographyPolicy (..),
    ProcessRef (..),
    allocationKeyText,
    bfCompartmentName,
    bfCompartmentSub,
    blockerReason,
    getUnitNameForBioFlow,
    parseAllocationKey,
    processRefText,
    unresolvedCount,
 )

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
    refusal <- liftIO (loadQuotaRefusal dbManager hostingConfig dbName)
    case refusal of
        Just msg -> return $ LoadFailed msg
        Nothing -> do
            result <- liftIO $ loadDatabase dbManager dbName
            case result of
                Left err -> return $ LoadFailed err
                Right (loadedDb, depResults) ->
                    return $ LoadSucceeded (makeStatusFromLoadedDb loadedDb) depResults

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
(source/target names with optional locations, see "Database.RelinkMapping"),
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
            case parseAliasCSV (BSL.fromStrict (T.encodeUtf8 csv)) >>= buildAliasMap >>= rejectEmpty of
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

{- | Supplier-gap report for a loaded or staged database: everything still
unsupplied after internal resolution and cross-DB linking. The natural
follow-up read after a relink: POST relink, then GET gap-report.
-}
gapReportHandler :: Text -> Maybe Int -> AppM GapReportAPI
gapReportHandler dbName mLimit = do
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
        let first :| others = gapReasons (Loader.geReason e)
         in GapEntryAPI
                { gaeName = Loader.geFlowName e
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
    -- Ordered by the blockers' own 'Ord', which is the declaration order of
    -- 'LinkBlocker': the same gap always lists its reasons the same way.
    gapReasons :: Loader.GapReason -> NonEmpty BlockerReason
    gapReasons gr = case gr of
        Loader.GapBlocked blockers -> fmap blockerReason blockers
        Loader.GapDanglingIdentity -> BlockerReason "dangling_source_identity" Nothing :| []
        Loader.GapWasteInput -> BlockerReason "unlinked_waste_input" Nothing :| []

{- | Dataset-soundness report for a loaded or staged database: the structural
defects a score can't reveal. The methodological counterpart of the
supplier-gap report: that one says what a database is missing, this one says
what is malformed in it.
-}
qualityReportHandler :: Text -> Maybe Int -> AppM QualityReportAPI
qualityReportHandler dbName mLimit = do
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
    case refusal of
        Just msg -> pure (ActivateResponse False msg Nothing)
        Nothing ->
            simpleAction (copyDatabase dbManager dbName newName) ("Copied database: " <> dbName <> " -> " <> newName)

{- | Read a loaded or configured database's own files again under another
allocation key, and register the result under @newName@.

Answers with what a load answers with, not with what a copy answers with: it
is a load, of tens of seconds on a large source, and the page that asks for it
is the one that already waits on that answer and shows the progress lines
underneath it.

An unreadable key is a 400 naming the keys there are. A key that divided
nothing is a refusal from the engine, carried in the same @LoadFailed@ as
every other reason a load produced no database.
-}
deriveDatabaseHandler :: Text -> Text -> Maybe Text -> AppM LoadDatabaseResponse
deriveDatabaseHandler dbName newName mAllocation = do
    guardMutation
    key <- either badKey pure (parseAllocationKey (fromMaybe "declared" mAllocation))
    dbManager <- asks aeDbManager
    hostingConfig <- asks aeHostingConfig
    -- A derived database is another database of the user's own, already
    -- loaded, so it spends both budgets exactly as a copy does.
    refusal <- liftIO $ do
        (uploaded, loadedUploads) <- quotaCounts dbManager
        pure (copyRefusal uploaded loadedUploads hostingConfig)
    case refusal of
        Just msg -> pure (LoadFailed msg)
        Nothing ->
            liftIO (deriveDatabase dbManager dbName newName key) >>= \case
                Left err -> pure (LoadFailed err)
                Right (loadedDb, depResults) -> pure (LoadSucceeded (makeStatusFromLoadedDb loadedDb) depResults)
  where
    badKey :: Text -> AppM a
    badKey err = throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 ("allocation: " <> err))}

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
    let classFilters = [(dcfSystem f, dcfValue f, dcfExact f) | f <- dsqClassifications req]
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
createActivitiesHandler dbName req = runWrite dbName CreateActivities (awrActivities req)

{- | Rewrite one activity the database already holds, keeping its identity. A
@process_id@ the database does not hold is a 404.
-}
replaceActivityHandler :: Text -> Text -> ActivityInput -> AppM ActivityWriteResponse
replaceActivityHandler dbName processId body =
    runWrite dbName (ReplaceActivity processId) [body]

runWrite :: Text -> WriteVerb -> [ActivityInput] -> AppM ActivityWriteResponse
runWrite dbName verb inputs = do
    guardMutation
    dbManager <- asks aeDbManager
    authored <- either (writeErr err400 . T.intercalate "\n") pure (toAuthoredActivities inputs)
    outcome <- liftIO (writeActivities dbManager dbName verb authored)
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
    outcome <- liftIO (editExchanges dbManager dbName processId edits)
    either (\refusal -> writeErr (statusFor refusal) (refusalMessage refusal)) (pure . editReportToAPI) outcome

{- | What an edit answers, on every surface that offers one, so an assistant
and a person reading the API reference are told the same thing.
-}
editReportToAPI :: EditReport -> ExchangeEditResponse
editReportToAPI report =
    ExchangeEditResponse
        { eepRemoved = erRemoved report
        , eepAmountsSet = erAmountsSet report
        , eepAdded = erAdded report
        , eepTransient = not (erPersisted report)
        , eepWarnings = erWarnings report
        }

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
arbitrary Unicode and joined with newlines. Failures surface as HTTP errors:
400 for an unknown format or data the target format cannot represent, 404 for a
database that is not loaded, never a 200 with a failure flag.
-}
exportDatabaseHandler :: Text -> ExportRequest -> AppM (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
exportDatabaseHandler dbName req = do
    dbManager <- asks aeDbManager
    fmt <- either (exportErr err400) pure (parseExportFormat (exrFormat req))
    mLoaded <- liftIO (getDatabase dbManager dbName)
    ld <- maybe (exportErr err404 ("Database not loaded: " <> dbName)) pure mLoaded
    (bytes, warnings) <- either (exportErr err400) pure (serializeDatabase fmt (ldDatabase ld))
    pure (addHeader (encodeExportWarnings warnings) (BinaryContent bytes))

{- | Export a loaded method collection over the same transport as the database
export: raw octet-stream body, projection warnings percent-encoded in the
@X-Volca-Export-Warnings@ header, 400 for a format without a method writer,
404 for a collection that is not loaded.
-}
exportMethodHandler :: Text -> ExportRequest -> AppM (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent)
exportMethodHandler name req = do
    dbManager <- asks aeDbManager
    fmt <- either (exportErr err400) pure (parseMethodExportFormat (exrFormat req))
    mColl <- liftIO (getMethodCollection dbManager name)
    coll <- maybe (exportErr err404 ("Method collection not loaded: " <> name)) pure mColl
    (bytes, warnings) <- either (exportErr err400) pure (serializeMethodCollection fmt name coll)
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

exportErr :: ServerError -> Text -> AppM a
exportErr status msg = throwError status{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

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
    case mfilter (not . T.null) (T.strip <$> mName) of
        Nothing -> return (rejectUpload "Missing upload name. Pass it as the ?name= query parameter.")
        Just name -> do
            hostingConfig <- asks aeHostingConfig
            case uploadSizeCap hostingConfig of
                Left rejection -> return (rejectUpload rejection)
                Right mCap -> do
                    streamed <- liftIO (streamToTempFile mCap src)
                    case streamed of
                        Left rejection -> return (rejectUpload rejection)
                        Right tmpPath ->
                            -- The read is lazy, so an uncapped (local/desktop) upload is never
                            -- buffered whole; 'finally' guarantees the temp file is deleted even
                            -- if the extract/detect continuation throws.
                            (liftIO (BSL.readFile tmpPath) >>= k name mDesc)
                                `finally` liftIO (removeQuietly tmpPath)
  where
    rejectUpload msg = UploadResponse False msg Nothing Nothing

{- | Fold a streamed octet-stream body into a fresh temp file, aborting with a
'Left' rejection if the running byte count exceeds the cap. Returns the temp
file path on success (the caller deletes it). Bytes are written chunk-by-chunk
and never held whole in memory.
-}
streamToTempFile :: Maybe Int -> SourceIO UploadChunk -> IO (Either Text FilePath)
streamToTempFile mCap src = do
    tmpDir <- System.Directory.getTemporaryDirectory
    (tmpPath, h) <- openBinaryTempFile tmpDir "volca-upload-.bin"
    result <- try (S.unSourceT src (go h 0)) :: IO (Either SomeException (Either Text ()))
    hClose h
    case result of
        Left e -> removeQuietly tmpPath >> return (Left ("Upload stream error: " <> T.pack (show e)))
        Right (Left msg) -> removeQuietly tmpPath >> return (Left msg)
        Right (Right ()) -> return (Right tmpPath)
  where
    tooLarge cap =
        "File too large. The upload limit on this plan is "
            <> T.pack (show (cap `div` (1024 * 1024)))
            <> " MB."
    go h !n step = case step of
        S.Stop -> return (Right ())
        S.Error e -> return (Left ("Upload stream error: " <> T.pack e))
        S.Skip s -> go h n s
        S.Effect ms -> ms >>= go h n
        S.Yield chunk s ->
            let !n' = n + BS.length (unUploadChunk chunk)
             in case mCap of
                    Just cap | n' > cap -> return (Left (tooLarge cap))
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
    case refusal of
        Just msg -> pure (UploadResponse False msg Nothing Nothing)
        Nothing -> uploadAccepted
  where
    uploadAccepted = withStreamedUpload mName mDesc src $ \name mDescription zipBytes -> do
        dbManager <- asks aeDbManager
        let uploadData =
                UploadData
                    { udName = name
                    , udDescription = mDescription
                    , udZipData = zipBytes
                    }
        -- Handle the upload (extract, detect format)
        uploadsDir <- liftIO UploadedDB.getDatabaseUploadsDir
        result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())

        case result of
            Left err ->
                return $ UploadResponse False err Nothing Nothing
            Right uploadResult -> do
                let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

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
                            , UploadedDB.umAllocation = Declared
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
                            , dcAllocation = Declared
                            , dcSource = Nothing
                            }

                -- Add to manager
                liftIO $ addDatabase dbManager dbConfig

                return $
                    UploadResponse
                        True
                        "Database uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)

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
        , dsaSource = dsSource ds
        }
  where
    statusToText Unloaded = "unloaded"
    statusToText PartiallyLinked = "partially_linked"
    statusToText Loaded = "loaded"

-- | Create DatabaseStatusAPI from a loaded database (derives status from linking stats)
makeStatusFromLoadedDb :: LoadedDatabase -> DatabaseStatusAPI
makeStatusFromLoadedDb loaded =
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
            , dsaAllocation = allocationKeyText (dcAllocation config)
            , dsaSource = dcSource config
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
formatToText OpenLcaJsonLd = "openlca-jsonld"
formatToText BrightwayExcel = "brightway-excel"
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
        -- Same 404 + "Database not loaded: " body as every other not-loaded
        -- arm, so typed-error recovery on the client keeps working.
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
    eitherResult <- liftIO $ try $ finalizeDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ ActivateResponse False ("Server exception: " <> T.pack (show ex)) Nothing
        Right (Left err) -> return $ ActivateResponse False err Nothing
        Right (Right loaded) -> do
            let status = makeStatusFromLoadedDb loaded
            return $ ActivateResponse True ("Finalized database: " <> dcDisplayName (ldConfig loaded)) (Just status)

{- | Upload a new method collection
Same flow as database upload but creates MethodConfig entry
-}
uploadMethodHandler :: Maybe Text -> Maybe Text -> SourceIO UploadChunk -> AppM UploadResponse
uploadMethodHandler mName mDesc src =
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
            Left err ->
                return $ UploadResponse False err Nothing Nothing
            Right uploadResult -> do
                let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                -- Find the actual method XML directory (e.g. ILCD/lciamethods/)
                methodDir <- liftIO $ findMethodDirectory uploadDir
                -- Format comes from the method directory, not from the database
                -- detector: companion spreadsheets shipped alongside a method
                -- package would otherwise read as a Brightway Excel inventory.
                methodFormat <- liftIO $ detectMethodFormat methodDir

                -- Create meta.toml (store path relative to upload dir)
                let meta =
                        UploadedDB.UploadMeta
                            { UploadedDB.umVersion = UploadedDB.metaVersion
                            , UploadedDB.umDisplayName = name
                            , UploadedDB.umDescription = mDescription
                            , UploadedDB.umFormat = methodFormat
                            , UploadedDB.umDataPath = makeRelative uploadDir methodDir
                            , UploadedDB.umDepends = []
                            , UploadedDB.umSource = Nothing
                            , UploadedDB.umAllocation = Declared
                            }
                liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                -- Create MethodConfig and add to manager
                let mc =
                        MethodConfig
                            { mcName = name
                            , mcPath = methodDir
                            , mcActive = False
                            , mcIsUploaded = True
                            , mcDescription = mDescription
                            , mcFormat = detectedFormatLabel methodFormat
                            , mcScoringSets = []
                            , mcGlobalMethods = []
                            , mcPatches = []
                            }
                liftIO $ addMethodCollection dbManager mc

                return $
                    UploadResponse
                        True
                        "Method uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)

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

{- | Common pattern: run an IO action that returns Either Text (), map to ActivateResponse.

Every caller performs a state change, so the read-only guard lives here rather
than being repeated at each of them.
-}
simpleAction :: IO (Either Text ()) -> Text -> AppM ActivateResponse
simpleAction action successMsg = do
    guardMutation
    result <- liftIO action
    return $ case result of
        Left err -> ActivateResponse False err Nothing
        Right () -> ActivateResponse True successMsg Nothing

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
    , Text -- upload subdir
    )
rdOps FlowSynonyms =
    (listFlowSynonyms, loadFlowSynonyms, unloadFlowSynonyms, addFlowSynonyms, removeFlowSynonyms, "flow-synonyms")
rdOps CompartmentMappings =
    (listCompartmentMappings, loadCompartmentMappings, unloadCompartmentMappings, addCompartmentMappings, removeCompartmentMappings, "compartment-mappings")
rdOps UnitDefs =
    (listUnitDefs, loadUnitDefs, unloadUnitDefs, addUnitDefs, removeUnitDefs, "units")

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
        let (_, _, _, addFn, _, subdir) = rdOps kind
        baseDir <- liftIO UploadedDB.getDataDir
        let slug = T.toLower $ T.intercalate "-" $ T.words name
            uploadDir = baseDir </> "uploads" </> T.unpack subdir </> T.unpack slug
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
                    }
        liftIO $ addFn dbManager rd
        return $ UploadResponse True "Uploaded successfully" (Just slug) Nothing
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
    dbManager <- asks aeDbManager
    let tvar = case kind of
            FlowSynonyms -> dmAvailableFlowSyns dbManager
            CompartmentMappings -> dmAvailableCompMaps dbManager
            UnitDefs -> dmAvailableUnitDefs dbManager
    available <- liftIO $ readTVarIO tvar
    case M.lookup name available of
        Nothing -> throwError $ err404{errBody = "Not found"}
        Just rd -> do
            bytes <- liftIO $ readRefDataSource (rdSource rd)
            case bytes of
                Left _ -> throwError $ err404{errBody = "CSV file not found"}
                Right content -> do
                    let disposition = "attachment; filename=\"" <> name <> ".csv\""
                    return $ addHeader disposition (BinaryContent content)

{- | Project an explanation onto the wire. One encoder for both surfaces: the
sentences come from the engine, the structured fields carry the same decision
in a form a client can compare or link on.
-}
explainCFToAPI :: Database -> Method -> BiosphereFlow -> Explain.CFExplanation -> ExplainCFResult
explainCFToAPI db method flow explanation =
    ExplainCFResult
        { ecrMethod = methodName method
        , ecrMethodUnit = methodUnit method
        , ecrFlow =
            ExplainedFlowAPI
                { eflId = UUID.toText (bfId flow)
                , eflName = bfName flow
                , eflUnit = getUnitNameForBioFlow (dbUnits db) flow
                , eflCategory = bfCompartmentName flow
                , eflCompartment = bfCompartmentSub flow
                , eflCas = bfCAS flow
                }
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
            , estVeto = case Explain.stResult step of
                Explain.StepVetoed reason -> Just (Explain.vetoName reason)
                Explain.StepHit -> Nothing
                Explain.StepMiss -> Nothing
                Explain.StepNotApplicable -> Nothing
                Explain.StepAmbiguous -> Nothing
            }

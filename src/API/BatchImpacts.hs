{- | Pure-IO wrappers around the LCIA batch entry points exposed by
"API.Routes". They let non-Servant callers (notably the MCP tool
layer) invoke the batch and multi-activity solves without going through
the Servant Handler stack, and surface a typed 'BatchError' sum instead
of an opaque 'ServerError'.

The wrappers delegate to 'activityLCIABatchH' and 'batchImpactsH' via
'Servant.runHandler', then translate the resulting 'ServerError' back
to a domain-shaped 'BatchError'. The "Collection not loaded" /
"Database not loaded" body prefixes are imported from "API.Routes"
('collectionNotLoadedPrefix', 'databaseNotLoadedPrefix') so the two
ends share a single source of truth – drift one and the build breaks
here.
-}
module API.BatchImpacts (
    BatchError (..),
    runActivityLCIABatch,
    runBatchImpacts,
    runComputedQuality,
    translateError,
) where

import API.Routes (activityLCIABatchH, batchImpactsH, collectionNotLoadedPrefix, computedQualityReportH, databaseNotLoadedPrefix)
import API.Types (BatchImpactsRequest (..), BatchImpactsResponse, ComputedQualityReportAPI, LCIABatchResult, SubstitutionRequest)
import App.Env (AppEnv (..), AppM, runApp)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Manager (CollectionName (..), DatabaseManager (..))
import Method.Mapping (LongTermMode (..))
import Servant (ServerError (..))
import qualified Servant

{- | Typed error sum for the batch wrappers. Carries enough context for a
caller (CLI / MCP handler) to produce an actionable message without
re-parsing HTTP bodies.
-}
data BatchError
    = {- | Method collection not loaded. Carries the requested name and the
      list of currently-loaded collection names (for a helpful "did you
      mean" hint on the client side).
      -}
      CollectionNotLoaded Text [Text]
    | {- | Database not loaded into the running engine. Carries the
      requested name.
      -}
      DatabaseNotLoaded Text
    | {- | Process ID could not be resolved to an activity, or its
      format was rejected. Carries the verbatim engine message.
      -}
      ActivityResolutionFailed Text
    | {- | Cross-DB linking / matrix invariant breakage (HTTP 422 in the
      Servant layer). Carries the verbatim engine message.
      -}
      LinkingIncomplete Text
    | {- | Catch-all for any other 'ServerError' surfaced by the Handler
      stack. Carries the HTTP status code and the verbatim body so
      nothing is silently swallowed.
      -}
      OtherBatchError Int Text
    deriving (Eq, Show)

{- | Score one activity against every method in a collection. Returns the
full 'LCIABatchResult' (per-method scores, per-scoring-set aggregates,
per-indicator breakdown, units). Substitutions, when supplied, take the
uncached cross-DB path.
-}
runActivityLCIABatch ::
    DatabaseManager ->
    -- | database name
    Text ->
    -- | process_id (activityUUID_productUUID)
    Text ->
    -- | method collection name
    Text ->
    -- | optional what-if supplier substitutions
    Maybe SubstitutionRequest ->
    -- | whether to keep or drop delayed long-term emissions
    LongTermMode ->
    IO (Either BatchError LCIABatchResult)
runActivityLCIABatch dbm dbName pid coll mSub ltMode =
    runBare dbm (activityLCIABatchH dbName pid (CollectionName coll) mSub ltMode)

{- | Score N activities against every method in a collection in one
multi-RHS MUMPS solve plus parallel characterization. Unresolved process
IDs land in 'birNotFound' / 'birInvalid' / 'birUnscorable' of the response,
not in 'BatchError'.
-}
runBatchImpacts ::
    DatabaseManager ->
    -- | database name
    Text ->
    -- | method collection name
    Text ->
    -- | per-(activity, method) top contributors (default 0 – see batchImpactsH)
    Maybe Int ->
    -- | whether to keep or drop delayed long-term emissions
    LongTermMode ->
    -- | process_ids to score
    [Text] ->
    IO (Either BatchError BatchImpactsResponse)
runBatchImpacts dbm dbName coll topFlows ltMode pids =
    runBare dbm (batchImpactsH dbName (CollectionName coll) topFlows ltMode BatchImpactsRequest{birProcessIds = pids})

{- | Computed-checks report over the whole catalogue of a loaded database.
Same wire shape as the REST endpoint ('computedQualityReportH'), so both
surfaces stay in lock-step.
-}
runComputedQuality ::
    DatabaseManager ->
    -- | database name
    Text ->
    -- | method collection; 'Nothing' picks the single loaded one
    Maybe Text ->
    -- | max findings per check, worst first
    Maybe Int ->
    IO (Either BatchError ComputedQualityReportAPI)
runComputedQuality dbm dbName mColl mLimit =
    runBare dbm (computedQualityReportH dbName mColl mLimit)

{- | Run one handler outside the Servant stack: the bare environment every
wrapper shares, then 'ServerError' translated back to a 'BatchError'.
-}
runBare :: DatabaseManager -> AppM a -> IO (Either BatchError a)
runBare dbm action = do
    let env =
            AppEnv
                { aeDbManager = dbm
                , aeMaxTreeDepth = 0
                , aePassword = Nothing
                , aeHostingConfig = Nothing
                , aeClassificationPresets = []
                , aeDataVersion = Nothing
                }
    res <- Servant.runHandler (runApp env action)
    case res of
        Right r -> pure (Right r)
        Left se -> Left <$> translateErrorIO dbm se

{- | IO-flavoured translator: snapshot the loaded-collection names from the
live TVar only when we actually need them (i.e. on a failure path) and
hand them to the pure 'translateError'. On the happy path the TVar is
not read at all.
-}
translateErrorIO :: DatabaseManager -> ServerError -> IO BatchError
translateErrorIO dbm se = do
    avail <- loadedCollectionNames dbm
    pure (translateError avail se)

{- | Snapshot of currently-loaded method collection names. Read lock-free
from the live TVar; used to enrich 'CollectionNotLoaded' messages.
-}
loadedCollectionNames :: DatabaseManager -> IO [Text]
loadedCollectionNames dbm = M.keys <$> readTVarIO (dmLoadedMethods dbm)

{- | Translate a Servant 'ServerError' back to the typed 'BatchError' sum.

The match is by HTTP status + body prefix. The prefix constants are
imported from "API.Routes", so a renamed prefix breaks compilation here
rather than drifting. The rest of the shape is a convention the compiler
cannot hold: the collection body names the loaded collections on a
second line ('API.Routes.collectionNotLoadedMessage'), so only its first
line is the requested name. What actually guards that is 'BatchImpactsSpec',
which writes a message with that function and parses it with this one.
-}
translateError :: [Text] -> ServerError -> BatchError
translateError availableCollections se
    | code == 404
    , Just rest <- T.stripPrefix collectionNotLoadedPrefix body =
        CollectionNotLoaded (T.takeWhile (/= '\n') rest) availableCollections
    | code == 404
    , Just rest <- T.stripPrefix databaseNotLoadedPrefix body =
        DatabaseNotLoaded rest
    | code == 404 = ActivityResolutionFailed body
    | code == 400 = ActivityResolutionFailed body
    | code == 422 = LinkingIncomplete body
    | otherwise = OtherBatchError code body
  where
    code = errHTTPCode se
    body = TE.decodeUtf8 (BSL.toStrict (errBody se))

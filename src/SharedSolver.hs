{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : SharedSolver
Description : Shared MUMPS solver with lazy factorization and thread synchronization

Implements lazy factorization: the MUMPS solver is not built at startup, but
on the first solve request. This eliminates ~3s x N databases of startup
latency while keeping sub-second solves after the first query.
-}
module SharedSolver (
    -- * Shared solver types
    SharedSolver,

    -- * Solver management
    createSharedSolver,

    -- * Concurrent solving
    solveWithSharedSolver,
    solveMultiWithSharedSolver,
    getFactorization,
    ensureFactorization,

    -- * Cached scaling / inventory
    computeScalingVectorCached,
    computeInventoryMatrixCached,
    computeInventoryMatrixBatchCached,

    -- * Cross-database back-substitution
    DepSolverLookup,
    CrossDBSolution (..),
    computeInventoryMatrixWithDepsCached,
    computeInventoryMatrixBatchWithDepsCached,
    goWithDepsFromScalings,
    maxDepsDepth,
    mergeSolutions,
    prepareDepDemandVecs,
    crossDBProcessContributions,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, try)
import Data.Coerce (coerce)
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Text (Text)
import Matrix (
    Demand (..),
    DepDemands,
    Inventory,
    Vector,
    accumulateDepDemands,
    accumulateDepDemandsWith,
    applyBiosphereMatrix,
    buildDemandVectorFromIndex,
    computeInventoryMatrixBatch,
    depDemandsToVector,
    precomputeMatrixFactorization,
    solveSparseLinearSystem,
    solveSparseLinearSystemWithFactorization,
    solveSparseLinearSystemWithFactorizationMulti,
 )
import Method.Mapping (LongTermMode (..), MethodTables, processContributionsFromTables)
import Progress
import Types
import UnitConversion (UnitConfig)

{- | Shared solver with lazy factorization and thread synchronization.
  Factorization happens on first solve, not at startup.
-}
data SharedSolver = SharedSolver
    { solverLock :: MVar ()
    -- ^ Serialize access to solver
    , solverFactorizationVar :: MVar (Maybe MatrixFactorization)
    -- ^ Lazy: Nothing until first solve
    , solverTechTriples :: [(Int, Int, Double)]
    -- ^ Technosphere matrix data
    , solverActivityCount :: Int
    -- ^ Number of activities
    , solverDbName :: Text
    -- ^ Database name (for solver cache key)
    }

-- | Create a shared solver. No factorization happens here — deferred to first solve.
createSharedSolver :: Text -> [(Int, Int, Double)] -> Int -> IO SharedSolver
createSharedSolver dbName techTriples activityCount = do
    reportProgress Info $ "Creating shared solver for '" ++ show dbName ++ "' (factorization deferred)"
    lock <- newMVar ()
    factVar <- newMVar Nothing
    return $ SharedSolver lock factVar techTriples activityCount dbName

{- | Compute the factorization and cache it. Assumes 'solverLock' is held and
the cache is empty (i.e. only called on a miss). Shared by the first-solve
path and 'ensureFactorization'.
-}
computeAndStoreFactorization :: SharedSolver -> IO MatrixFactorization
computeAndStoreFactorization solver = do
    reportProgress Info $ "Factorizing '" ++ show (solverDbName solver) ++ "' on first use"
    fact <-
        precomputeMatrixFactorization
            (solverDbName solver)
            (solverTechTriples solver)
            (solverActivityCount solver)
    modifyMVar_ (solverFactorizationVar solver) (const (pure (Just fact)))
    pure fact

-- | Solve using shared solver. On first call, triggers lazy factorization.
solveWithSharedSolver :: SharedSolver -> Demand -> IO Vector
solveWithSharedSolver solver (Demand demandVector) =
    withMVar (solverLock solver) $ \_ ->
        readMVar (solverFactorizationVar solver) >>= \case
            Just fact -> do
                reportProgress Solver "Using cached factorization for sub-second solve"
                solveSparseLinearSystemWithFactorization fact demandVector
            Nothing ->
                try (computeAndStoreFactorization solver >>= flip solveSparseLinearSystemWithFactorization demandVector)
                    >>= either fallback pure
  where
    fallback e = do
        reportProgress Solver $ "Factorization failed: " ++ show (e :: SomeException) ++ " — using fallback solver"
        solveSparseLinearSystem (solverTechTriples solver) (solverActivityCount solver) demandVector

-- | Read the cached factorization without solving. Returns Nothing until the first solve.
getFactorization :: SharedSolver -> IO (Maybe MatrixFactorization)
getFactorization solver = readMVar (solverFactorizationVar solver)

{- | Force the factorization if not yet computed, then return it.
Safe to call from multiple threads: the solverLock serializes first-time factorization.
-}
ensureFactorization :: SharedSolver -> IO MatrixFactorization
ensureFactorization solver = withMVar (solverLock solver) $ \_ ->
    readMVar (solverFactorizationVar solver) >>= \case
        Just fact -> pure fact
        Nothing -> computeAndStoreFactorization solver

{- | Solve with multiple RHS vectors in one MUMPS call, using the cached factorization.
Forces factorization on first call. Subsequent calls reuse the cached LU.
-}
solveMultiWithSharedSolver :: SharedSolver -> [Demand] -> IO [Vector]
solveMultiWithSharedSolver solver demandVecs = do
    fact <- ensureFactorization solver
    solveSparseLinearSystemWithFactorizationMulti fact (coerce demandVecs)

{- | Compute the scaling vector for @pid@, routing through the shared solver's
lazy factorization cache. Same shape as 'Matrix.computeScalingVector' but
amortizes factorization across every call in a server's lifetime — the
right default for endpoint handlers.
-}
computeScalingVectorCached :: Database -> SharedSolver -> ProcessId -> IO (Either Text Vector)
computeScalingVectorCached db solver pid =
    either
        (pure . Left)
        (fmap Right . solveWithSharedSolver solver)
        (buildDemandVectorFromIndex (dbActivityIndex db) pid)

-- | Inventory for @pid@ using the shared-solver factorization cache.
computeInventoryMatrixCached :: Database -> SharedSolver -> ProcessId -> IO (Either Text Inventory)
computeInventoryMatrixCached db solver pid =
    fmap (applyBiosphereMatrix db) <$> computeScalingVectorCached db solver pid

-- | Batch inventories for many pids using one MUMPS multi-RHS call against the cached factorization.
computeInventoryMatrixBatchCached :: Database -> SharedSolver -> [ProcessId] -> IO (Either Text [Inventory])
computeInventoryMatrixBatchCached _ _ [] = pure (Right [])
computeInventoryMatrixBatchCached db solver pids = do
    fact <- ensureFactorization solver
    computeInventoryMatrixBatch db fact pids

{- | Resolve a dependency database by name into its (Database, SharedSolver) pair.
Returns @Nothing@ if the dep DB is not loaded; the caller treats it as a
zero-contribution supplier and continues.
-}
type DepSolverLookup = Text -> IO (Maybe (Database, SharedSolver))

{- | Per-pid result of a cross-DB inventory solve: the merged biosphere
'Inventory' plus the per-DB scaling vectors that produced it.

The scaling vectors are needed by the regionalized LCIA path, which scores
each DB's biosphere triples against THAT DB's scaling — a sum across all
DBs reached at request time, not a single dot product against the root.
Without per-DB scalings, dep-DB regional CFs are silently invisible
(the merged 'Inventory' has the emissions but lost the per-activity
location context the regional CF lookup needs).

'csScalings' lists every DB visited (root first, then dep DBs in BFS
order — both the order and the recursion depth follow the same dep-graph
walk that built 'csInventory'). The list is non-empty: the root entry
('rootDbName', root 'Database', root scaling) is always added by the
top-level solve; dep entries are appended as the recursion fans out.
A DB whose scaling vector is the zero vector for this pid is still
included; the solver never silently omits a participating DB.
-}
data CrossDBSolution = CrossDBSolution
    { csInventory :: !Inventory
    , csScalings :: !(NonEmpty (Text, Database, Vector))
    }

{- | Combine two cross-DB solutions: sum their inventories and concatenate
their visited-DB scalings. Associative; folding from a base preserves the
base-first, deps-in-order BFS layout 'csScalings' documents.
-}
instance Semigroup CrossDBSolution where
    a <> b =
        CrossDBSolution
            (M.unionWith (+) (csInventory a) (csInventory b))
            (csScalings a <> csScalings b)

{- |
Batch inventory with cross-DB back-substitution. Multi-RHS is preserved at
every level of the dependency DAG:

* Root DB: one multi-RHS solve for the K root demand vectors.
* For each dependency DB reached via 'dbCrossDBLinks', the K supplier-demand
  maps become K dense demand vectors — one multi-RHS solve on that DB.
* Recurse into the dep DB's own cross-DB links (Agribalyse → Ecoinvent, etc.).
* Sum local + all dep contributions per root by 'M.unionWith (+)'.

Returns one 'CrossDBSolution' per pid: the merged inventory plus the per-DB
scaling vectors that produced it. The regional LCIA path uses the scalings
to sum per-DB regional dot products; callers that only need the inventory
extract 'csInventory'.

Depth is capped at 10 as a safety net against pathological data (cyclic links).
-}
computeInventoryMatrixBatchWithDepsCached ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | root DB name (recorded in the head of each 'csScalings')
    Text ->
    SharedSolver ->
    [ProcessId] ->
    IO (Either Text [CrossDBSolution])
computeInventoryMatrixBatchWithDepsCached _ _ _ _ _ [] = pure (Right [])
computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db dbName solver pids =
    either
        (pure . Left)
        (\demands -> goWithDeps unitConfig depLookup db dbName solver demands 0)
        (traverse (buildDemandVectorFromIndex (dbActivityIndex db)) pids)

-- | Single-process convenience wrapper. One-element batch.
computeInventoryMatrixWithDepsCached ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | root DB name (recorded in the head of 'csScalings')
    Text ->
    SharedSolver ->
    ProcessId ->
    IO (Either Text CrossDBSolution)
computeInventoryMatrixWithDepsCached unitConfig depLookup db dbName solver pid = do
    res <- computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db dbName solver [pid]
    pure $ case res of
        Left err -> Left err
        Right (sol : _) -> Right sol
        Right [] ->
            -- unreachable: a single-pid batch always returns a singleton list.
            -- Surface as Left rather than fabricate an empty 'CrossDBSolution'
            -- with no 'csScalings' (the type now forbids it via NonEmpty).
            Left "computeInventoryMatrixWithDepsCached: empty batch result for one pid"

-- | Safety net against cyclic cross-DB links (Ginko → Agribalyse → Ginko).
maxDepsDepth :: Int
maxDepsDepth = 10

goWithDeps ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | THIS DB's name (recorded in the 'csScalings' entry contributed at this level)
    Text ->
    SharedSolver ->
    -- | K demand vectors, length = dbActivityCount db
    [Demand] ->
    -- | recursion depth
    Int ->
    IO (Either Text [CrossDBSolution])
goWithDeps unitConfig depLookup db dbName solver demands depth = do
    scalings <- solveMultiWithSharedSolver solver demands
    goWithDepsFromScalings unitConfig depLookup db dbName [] scalings depth

{- | Propagate pre-computed root scalings into the dep-DB graph. Same body as
the dep-propagation half of 'goWithDeps' but skips the root solve — the
caller supplies the root scaling vectors (e.g. after a Sherman-Morrison
substitution update) and an optional list of synthesized 'CrossDBLink'
entries to fold into 'accumulateDepDemands' at this level only.

Extra links are applied at the root DB only; recursive calls into dep DBs
use their static 'dbCrossDBLinks'. Supporting nested substitutions would
require threading per-DB extras through 'resolveDep' — out of scope.

Each returned 'CrossDBSolution' is built bottom-up: the current DB
contributes its @(dbName, db, scaling)@ entry, then dep DBs append theirs
through 'resolveDep'. Dep entries appear in the BFS order of 'allDepDbs'.
-}
goWithDepsFromScalings ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | THIS DB's name
    Text ->
    -- | virtual links to inject at this level (root only)
    [CrossDBLink] ->
    -- | K pre-computed root scaling vectors
    [Vector] ->
    -- | recursion depth
    Int ->
    IO (Either Text [CrossDBSolution])
goWithDepsFromScalings unitConfig depLookup db dbName extraLinks scalings depth = do
    let localInvs = map (applyBiosphereMatrix db) scalings
        baseSolutions =
            zipWith
                (\inv s -> CrossDBSolution inv (NE.singleton (dbName, db, s)))
                localInvs
                scalings
    if depth >= maxDepsDepth
        then pure (Right baseSolutions)
        else do
            let perRootDepDemands = map (accumulateDepDemandsWith db extraLinks) scalings
                allDepDbs = depDbsOf perRootDepDemands
            if null allDepDbs
                then pure (Right baseSolutions)
                else do
                    depResults <-
                        mapConcurrently
                            (resolveDep unitConfig depLookup perRootDepDemands depth (length scalings))
                            allDepDbs
                    pure $ case sequence depResults of
                        Left err -> Left err
                        Right depSolsByDb ->
                            -- Each dep returns @[Maybe CrossDBSolution]@ of
                            -- length K. Absent-dep entries (depLookup
                            -- returned Nothing) are 'Nothing' and drop out
                            -- of the merge — an absent dep contributes
                            -- nothing to inventory or csScalings.
                            let perRootDepSols = map catMaybes (transpose depSolsByDb)
                             in Right $
                                    zipWith
                                        mergeSolutions
                                        baseSolutions
                                        perRootDepSols

{- | Merge a base 'CrossDBSolution' (this DB's own contribution) with the
list of dep-DB solutions resolved at this level. Inventories are summed
via 'M.unionWith (+)'; scaling vectors are concatenated so the final
'csScalings' lists every visited DB in BFS order.

Exported so the substitution-aware solver ('Service.goWithSubsAndDeps')
reuses the same merge shape as the plain cross-DB solver.
-}
mergeSolutions :: CrossDBSolution -> [CrossDBSolution] -> CrossDBSolution
mergeSolutions = foldl' (<>)

-- | Every dependency DB referenced across a level's per-root demand maps.
depDbsOf :: [DepDemands] -> [Text]
depDbsOf = S.toList . S.unions . map M.keysSet

{- | Turn a level's per-root demand maps into the dep DB's per-root demand
vectors, performing unit conversion. Picks out @depDbName@'s share of each
root's demands. Shared by every dep resolver (inventory, contributions, and
the substitution-aware path in 'Service').
-}
prepareDepDemandVecs :: UnitConfig -> Text -> Database -> [DepDemands] -> Either Text [Demand]
prepareDepDemandVecs unitConfig depDbName depDb =
    traverse (depDemandsToVector unitConfig depDbName depDb . M.findWithDefault M.empty depDbName)

resolveDep ::
    UnitConfig ->
    DepSolverLookup ->
    [DepDemands] ->
    -- | current depth (for recursion)
    Int ->
    -- | K (so absent-dep returns the right number of 'Nothing' padding)
    Int ->
    Text ->
    IO (Either Text [Maybe CrossDBSolution])
resolveDep unitConfig depLookup perRootDepDemands depth k depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            -- Dep DB not loaded: contribute nothing at every root. The
            -- 'Nothing' propagates through 'mergeSolutions' and drops out
            -- before the inventory union / csScalings concat, exactly
            -- equivalent to an empty 'CrossDBSolution' but without
            -- materialising one (which the NonEmpty type forbids).
            pure (Right (replicate k Nothing))
        Just (depDb, depSolver) ->
            case prepareDepDemandVecs unitConfig depDbName depDb perRootDepDemands of
                Left err -> pure (Left err)
                Right depDemandVecs -> do
                    sols <- goWithDeps unitConfig depLookup depDb depDbName depSolver depDemandVecs (depth + 1)
                    pure $ fmap (map Just) sols

{- | Cross-DB per-activity LCIA contributions. Walks the same dep graph as
'goWithDeps' but attributes contributions per @(dbName, localPid)@ instead
of summing a biosphere inventory. At each DB visited we solve its scaling
vector, run 'processContributionsFromTables' against the merged flow/unit
metadata + 'MethodTables', then propagate dep demands via
'accumulateDepDemands' / 'depDemandsToVector' exactly as the inventory
path does. Result keys are qualified by DB so the same local ProcessId in
different DBs never collides; the caller formats them into "dbName::pid"
for the wire when the DB differs from the root.
-}
crossDBProcessContributions ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    DepSolverLookup ->
    -- | root DB
    Database ->
    -- | root DB name
    Text ->
    -- | root solver
    SharedSolver ->
    -- | root functional unit
    ProcessId ->
    MethodTables ->
    -- | long-term emission policy, forwarded to the per-DB attribution
    LongTermMode ->
    IO (Either Text (M.Map (Text, ProcessId) Double))
crossDBProcessContributions unitConfig unitDB flowDB depLookup rootDb rootName rootSolver rootPid tables ltMode =
    either
        (pure . Left)
        (\demand -> go rootDb rootName rootSolver [demand] 0)
        (buildDemandVectorFromIndex (dbActivityIndex rootDb) rootPid)
  where
    go ::
        Database ->
        Text ->
        SharedSolver ->
        [Demand] ->
        Int ->
        IO (Either Text (M.Map (Text, ProcessId) Double))
    go db dbName solver demands depth = do
        scalings <- solveMultiWithSharedSolver solver demands
        -- attribute each root demand's contributions to this DB's activities;
        -- sum across demands (we currently only call with K=1, but keep the
        -- shape aligned with goWithDeps for future batching).
        let localByRoot = map (\s -> processContributionsFromTables unitConfig unitDB flowDB ltMode db s tables) scalings
            localTagged = M.mapKeys (dbName,) (foldr (M.unionWith (+)) M.empty localByRoot)
        if depth >= maxDepsDepth
            then pure (Right localTagged)
            else do
                let perRootDepDemands = map (accumulateDepDemands db) scalings
                    allDepDbs = depDbsOf perRootDepDemands
                if null allDepDbs
                    then pure (Right localTagged)
                    else do
                        depResults <- mapConcurrently (resolveDepContribs perRootDepDemands depth) allDepDbs
                        pure $ case sequence depResults of
                            Left err -> Left err
                            Right depMaps ->
                                Right $ foldr (M.unionWith (+)) localTagged depMaps

    resolveDepContribs ::
        [DepDemands] ->
        Int ->
        Text ->
        IO (Either Text (M.Map (Text, ProcessId) Double))
    resolveDepContribs perRootDepDemands depth depDbName = do
        depM <- depLookup depDbName
        case depM of
            Nothing -> pure (Right M.empty) -- dep DB not loaded; root-level gate should have caught this
            Just (depDb, depSolver) ->
                case prepareDepDemandVecs unitConfig depDbName depDb perRootDepDemands of
                    Left err -> pure (Left err)
                    Right depDemandVecs -> go depDb depDbName depSolver depDemandVecs (depth + 1)

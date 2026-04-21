{-# LANGUAGE OverloadedStrings #-}

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
    computeInventoryMatrixWithDepsCached,
    computeInventoryMatrixBatchWithDepsCached,
    goWithDepsFromScalings,
    crossDBProcessContributions,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, catch)
import Data.List (transpose)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Matrix (
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
import Method.Mapping (MethodTables, processContributionsFromTables)
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

-- | Solve using shared solver. On first call, triggers lazy factorization.
solveWithSharedSolver :: SharedSolver -> Vector -> IO Vector
solveWithSharedSolver solver demandVector = do
    withMVar (solverLock solver) $ \_ -> do
        maybeFact <- readMVar (solverFactorizationVar solver)
        case maybeFact of
            Just factorization -> do
                reportProgress Solver "Using cached factorization for sub-second solve"
                solveSparseLinearSystemWithFactorization factorization demandVector
            Nothing -> do
                -- First solve: factorize, cache, then solve
                reportProgress Info $ "First solve for '" ++ show (solverDbName solver) ++ "' — computing factorization..."
                factResult <-
                    catch
                        ( do
                            factorization <-
                                precomputeMatrixFactorization
                                    (solverDbName solver)
                                    (solverTechTriples solver)
                                    (solverActivityCount solver)
                            -- Store for subsequent solves (replace the MVar contents)
                            _ <- modifyMVar (solverFactorizationVar solver) $ \_ -> return (Just factorization, ())
                            reportProgress Info "Factorization complete — solving"
                            result <- solveSparseLinearSystemWithFactorization factorization demandVector
                            return (Just result)
                        )
                        ( \e -> do
                            reportProgress Solver $ "Factorization failed: " ++ show (e :: SomeException) ++ " — using fallback solver"
                            return Nothing
                        )
                case factResult of
                    Just result -> return result
                    Nothing ->
                        solveSparseLinearSystem (solverTechTriples solver) (solverActivityCount solver) demandVector

-- | Read the cached factorization without solving. Returns Nothing until the first solve.
getFactorization :: SharedSolver -> IO (Maybe MatrixFactorization)
getFactorization solver = readMVar (solverFactorizationVar solver)

{- | Force the factorization if not yet computed, then return it.
Safe to call from multiple threads: the solverLock serializes first-time factorization.
-}
ensureFactorization :: SharedSolver -> IO MatrixFactorization
ensureFactorization solver = withMVar (solverLock solver) $ \_ -> do
    maybeFact <- readMVar (solverFactorizationVar solver)
    case maybeFact of
        Just fact -> pure fact
        Nothing -> do
            reportProgress Info $ "Factorizing '" ++ show (solverDbName solver) ++ "' on first use"
            fact <-
                precomputeMatrixFactorization
                    (solverDbName solver)
                    (solverTechTriples solver)
                    (solverActivityCount solver)
            modifyMVar_ (solverFactorizationVar solver) $ \_ -> pure (Just fact)
            pure fact

{- | Solve with multiple RHS vectors in one MUMPS call, using the cached factorization.
Forces factorization on first call. Subsequent calls reuse the cached LU.
-}
solveMultiWithSharedSolver :: SharedSolver -> [Vector] -> IO [Vector]
solveMultiWithSharedSolver solver demandVecs = do
    fact <- ensureFactorization solver
    solveSparseLinearSystemWithFactorizationMulti fact demandVecs

{- | Compute the scaling vector for @pid@, routing through the shared solver's
lazy factorization cache. Same shape as 'Matrix.computeScalingVector' but
amortizes factorization across every call in a server's lifetime — the
right default for endpoint handlers.
-}
computeScalingVectorCached :: Database -> SharedSolver -> ProcessId -> IO Vector
computeScalingVectorCached db solver pid =
    solveWithSharedSolver solver (buildDemandVectorFromIndex (dbActivityIndex db) pid)

-- | Inventory for @pid@ using the shared-solver factorization cache.
computeInventoryMatrixCached :: Database -> SharedSolver -> ProcessId -> IO Inventory
computeInventoryMatrixCached db solver pid =
    applyBiosphereMatrix db <$> computeScalingVectorCached db solver pid

-- | Batch inventories for many pids using one MUMPS multi-RHS call against the cached factorization.
computeInventoryMatrixBatchCached :: Database -> SharedSolver -> [ProcessId] -> IO [Inventory]
computeInventoryMatrixBatchCached _ _ [] = pure []
computeInventoryMatrixBatchCached db solver pids = do
    fact <- ensureFactorization solver
    computeInventoryMatrixBatch db fact pids

{- | Resolve a dependency database by name into its (Database, SharedSolver) pair.
Returns @Nothing@ if the dep DB is not loaded; the caller treats it as a
zero-contribution supplier and continues.
-}
type DepSolverLookup = Text -> IO (Maybe (Database, SharedSolver))

{- |
Batch inventory with cross-DB back-substitution. Multi-RHS is preserved at
every level of the dependency DAG:

* Root DB: one multi-RHS solve for the K root demand vectors.
* For each dependency DB reached via 'dbCrossDBLinks', the K supplier-demand
  maps become K dense demand vectors — one multi-RHS solve on that DB.
* Recurse into the dep DB's own cross-DB links (Agribalyse → Ecoinvent, etc.).
* Sum local + all dep contributions per root by 'M.unionWith (+)'.

Depth is capped at 10 as a safety net against pathological data (cyclic links).
-}
computeInventoryMatrixBatchWithDepsCached ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    SharedSolver ->
    [ProcessId] ->
    IO (Either Text [Inventory])
computeInventoryMatrixBatchWithDepsCached _ _ _ _ [] = pure (Right [])
computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db solver pids =
    goWithDeps
        unitConfig
        depLookup
        db
        solver
        (map (buildDemandVectorFromIndex (dbActivityIndex db)) pids)
        0

-- | Single-process convenience wrapper. One-element batch.
computeInventoryMatrixWithDepsCached ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    SharedSolver ->
    ProcessId ->
    IO (Either Text Inventory)
computeInventoryMatrixWithDepsCached unitConfig depLookup db solver pid = do
    res <- computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db solver [pid]
    pure $ case res of
        Left err -> Left err
        Right (inv : _) -> Right inv
        Right [] -> Right M.empty -- unreachable: batch is non-empty

-- | Safety net against cyclic cross-DB links (Ginko → Agribalyse → Ginko).
maxDepsDepth :: Int
maxDepsDepth = 10

goWithDeps ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    SharedSolver ->
    -- | K demand vectors, length = dbActivityCount db
    [Vector] ->
    -- | recursion depth
    Int ->
    IO (Either Text [Inventory])
goWithDeps unitConfig depLookup db solver demands depth = do
    scalings <- solveMultiWithSharedSolver solver demands
    goWithDepsFromScalings unitConfig depLookup db [] scalings depth

{- | Propagate pre-computed root scalings into the dep-DB graph. Same body as
the dep-propagation half of 'goWithDeps' but skips the root solve — the
caller supplies the root scaling vectors (e.g. after a Sherman-Morrison
substitution update) and an optional list of synthesized 'CrossDBLink'
entries to fold into 'accumulateDepDemands' at this level only.

Extra links are applied at the root DB only; recursive calls into dep DBs
use their static 'dbCrossDBLinks'. Supporting nested substitutions would
require threading per-DB extras through 'resolveDep' — out of scope.
-}
goWithDepsFromScalings ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | virtual links to inject at this level (root only)
    [CrossDBLink] ->
    -- | K pre-computed root scaling vectors
    [Vector] ->
    -- | recursion depth
    Int ->
    IO (Either Text [Inventory])
goWithDepsFromScalings unitConfig depLookup db extraLinks scalings depth = do
    let localInvs = map (applyBiosphereMatrix db) scalings
    if depth >= maxDepsDepth
        then pure (Right localInvs)
        else do
            let perRootDepDemands = map (accumulateDepDemandsWith db extraLinks) scalings
                allDepDbs = S.toList $ S.unions $ map M.keysSet perRootDepDemands
            if null allDepDbs
                then pure (Right localInvs)
                else do
                    depResults <-
                        mapConcurrently
                            (resolveDep unitConfig depLookup perRootDepDemands depth (length scalings))
                            allDepDbs
                    pure $ case sequence depResults of
                        Left err -> Left err
                        Right depContribsByDb ->
                            let perRootDepInvs = transpose depContribsByDb
                             in Right $
                                    zipWith
                                        (foldr (M.unionWith (+)))
                                        localInvs
                                        perRootDepInvs

resolveDep ::
    UnitConfig ->
    DepSolverLookup ->
    [M.Map Text (M.Map (UUID, UUID) (Double, Text))] ->
    -- | current depth (for recursion)
    Int ->
    -- | K (so we can pad with empty on a missing dep DB)
    Int ->
    Text ->
    IO (Either Text [Inventory])
resolveDep unitConfig depLookup perRootDepDemands depth k depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            pure (Right (replicate k M.empty))
        Just (depDb, depSolver) ->
            let demandsPerRoot = map (M.findWithDefault M.empty depDbName) perRootDepDemands
                depVecsE = traverse (depDemandsToVector unitConfig depDbName depDb) demandsPerRoot
             in case depVecsE of
                    Left err -> pure (Left err)
                    Right depDemandVecs -> goWithDeps unitConfig depLookup depDb depSolver depDemandVecs (depth + 1)

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
    FlowDB ->
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
    IO (Either Text (M.Map (Text, ProcessId) Double))
crossDBProcessContributions unitConfig unitDB flowDB depLookup rootDb rootName rootSolver rootPid tables =
    go
        rootDb
        rootName
        rootSolver
        [buildDemandVectorFromIndex (dbActivityIndex rootDb) rootPid]
        0
  where
    go ::
        Database ->
        Text ->
        SharedSolver ->
        [Vector] ->
        Int ->
        IO (Either Text (M.Map (Text, ProcessId) Double))
    go db dbName solver demands depth = do
        scalings <- solveMultiWithSharedSolver solver demands
        -- attribute each root demand's contributions to this DB's activities;
        -- sum across demands (we currently only call with K=1, but keep the
        -- shape aligned with goWithDeps for future batching).
        let localByRoot = map (\s -> processContributionsFromTables unitConfig unitDB flowDB db s tables) scalings
            localTagged = M.mapKeys ((,) dbName) (foldr (M.unionWith (+)) M.empty localByRoot)
        if depth >= maxDepsDepth
            then pure (Right localTagged)
            else do
                let perRootDepDemands = map (accumulateDepDemands db) scalings
                    allDepDbs = S.toList $ S.unions $ map M.keysSet perRootDepDemands
                if null allDepDbs
                    then pure (Right localTagged)
                    else do
                        depResults <- mapConcurrently (resolveDepContribs perRootDepDemands depth) allDepDbs
                        pure $ case sequence depResults of
                            Left err -> Left err
                            Right depMaps ->
                                Right $ foldr (M.unionWith (+)) localTagged depMaps

    resolveDepContribs ::
        [M.Map Text (M.Map (UUID, UUID) (Double, Text))] ->
        Int ->
        Text ->
        IO (Either Text (M.Map (Text, ProcessId) Double))
    resolveDepContribs perRootDepDemands depth depDbName = do
        depM <- depLookup depDbName
        case depM of
            Nothing -> pure (Right M.empty) -- dep DB not loaded; root-level gate should have caught this
            Just (depDb, depSolver) ->
                let demandsPerRoot = map (M.findWithDefault M.empty depDbName) perRootDepDemands
                    depVecsE = traverse (depDemandsToVector unitConfig depDbName depDb) demandsPerRoot
                 in case depVecsE of
                        Left err -> pure (Left err)
                        Right depDemandVecs -> go depDb depDbName depSolver depDemandVecs (depth + 1)

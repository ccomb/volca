{-# LANGUAGE OverloadedStrings #-}

{-|
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
    computeInventoryMatrixBatchWithDepsCached
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, SomeException)
import Data.List (transpose)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Progress
import Types
import Matrix
    ( Vector
    , Inventory
    , accumulateDepDemands
    , applyBiosphereMatrix
    , buildDemandVectorFromIndex
    , computeInventoryMatrixBatch
    , depDemandsToVector
    , precomputeMatrixFactorization
    , solveSparseLinearSystem
    , solveSparseLinearSystemWithFactorization
    , solveSparseLinearSystemWithFactorizationMulti
    )

-- | Shared solver with lazy factorization and thread synchronization.
--   Factorization happens on first solve, not at startup.
data SharedSolver = SharedSolver
    { solverLock :: MVar ()                                -- ^ Serialize access to solver
    , solverFactorizationVar :: MVar (Maybe MatrixFactorization) -- ^ Lazy: Nothing until first solve
    , solverTechTriples :: [(Int, Int, Double)]             -- ^ Technosphere matrix data
    , solverActivityCount :: Int                            -- ^ Number of activities
    , solverDbName :: Text                                  -- ^ Database name (for solver cache key)
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
                factResult <- catch
                    (do factorization <- precomputeMatrixFactorization
                            (solverDbName solver)
                            (solverTechTriples solver)
                            (solverActivityCount solver)
                        -- Store for subsequent solves (replace the MVar contents)
                        _ <- modifyMVar (solverFactorizationVar solver) $ \_ -> return (Just factorization, ())
                        reportProgress Info "Factorization complete — solving"
                        result <- solveSparseLinearSystemWithFactorization factorization demandVector
                        return (Just result))
                    (\e -> do
                        reportProgress Solver $ "Factorization failed: " ++ show (e :: SomeException) ++ " — using fallback solver"
                        return Nothing)
                case factResult of
                    Just result -> return result
                    Nothing ->
                        solveSparseLinearSystem (solverTechTriples solver) (solverActivityCount solver) demandVector

-- | Read the cached factorization without solving. Returns Nothing until the first solve.
getFactorization :: SharedSolver -> IO (Maybe MatrixFactorization)
getFactorization solver = readMVar (solverFactorizationVar solver)

-- | Force the factorization if not yet computed, then return it.
-- Safe to call from multiple threads: the solverLock serializes first-time factorization.
ensureFactorization :: SharedSolver -> IO MatrixFactorization
ensureFactorization solver = withMVar (solverLock solver) $ \_ -> do
    maybeFact <- readMVar (solverFactorizationVar solver)
    case maybeFact of
        Just fact -> pure fact
        Nothing -> do
            reportProgress Info $ "Factorizing '" ++ show (solverDbName solver) ++ "' on first use"
            fact <- precomputeMatrixFactorization
                (solverDbName solver)
                (solverTechTriples solver)
                (solverActivityCount solver)
            modifyMVar_ (solverFactorizationVar solver) $ \_ -> pure (Just fact)
            pure fact

-- | Solve with multiple RHS vectors in one MUMPS call, using the cached factorization.
-- Forces factorization on first call. Subsequent calls reuse the cached LU.
solveMultiWithSharedSolver :: SharedSolver -> [Vector] -> IO [Vector]
solveMultiWithSharedSolver solver demandVecs = do
    fact <- ensureFactorization solver
    solveSparseLinearSystemWithFactorizationMulti fact demandVecs

-- | Compute the scaling vector for @pid@, routing through the shared solver's
-- lazy factorization cache. Same shape as 'Matrix.computeScalingVector' but
-- amortizes factorization across every call in a server's lifetime — the
-- right default for endpoint handlers.
computeScalingVectorCached :: Database -> SharedSolver -> ProcessId -> IO Vector
computeScalingVectorCached db solver pid =
    solveWithSharedSolver solver (buildDemandVectorFromIndex (dbActivityIndex db) pid)

-- | Inventory for @pid@ using the shared-solver factorization cache.
computeInventoryMatrixCached :: Database -> SharedSolver -> ProcessId -> IO Inventory
computeInventoryMatrixCached db solver pid =
    applyBiosphereMatrix db <$> computeScalingVectorCached db solver pid

-- | Batch inventories for many pids using one MUMPS multi-RHS call against the cached factorization.
computeInventoryMatrixBatchCached :: Database -> SharedSolver -> [ProcessId] -> IO [Inventory]
computeInventoryMatrixBatchCached _  _      []   = pure []
computeInventoryMatrixBatchCached db solver pids = do
    fact <- ensureFactorization solver
    computeInventoryMatrixBatch db fact pids

-- | Resolve a dependency database by name into its (Database, SharedSolver) pair.
-- Returns @Nothing@ if the dep DB is not loaded; the caller treats it as a
-- zero-contribution supplier and continues.
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
computeInventoryMatrixBatchWithDepsCached
    :: DepSolverLookup
    -> Database
    -> SharedSolver
    -> [ProcessId]
    -> IO [Inventory]
computeInventoryMatrixBatchWithDepsCached _         _  _      [] = pure []
computeInventoryMatrixBatchWithDepsCached depLookup db solver pids =
    goWithDeps depLookup db solver
        (map (buildDemandVectorFromIndex (dbActivityIndex db)) pids)
        0

-- | Single-process convenience wrapper. One-element batch.
computeInventoryMatrixWithDepsCached
    :: DepSolverLookup -> Database -> SharedSolver -> ProcessId -> IO Inventory
computeInventoryMatrixWithDepsCached depLookup db solver pid = do
    invs <- computeInventoryMatrixBatchWithDepsCached depLookup db solver [pid]
    case invs of
        (inv : _) -> pure inv
        []        -> pure M.empty  -- unreachable: batch is non-empty

-- | Safety net against cyclic cross-DB links (Ginko → Agribalyse → Ginko).
maxDepsDepth :: Int
maxDepsDepth = 10

goWithDeps
    :: DepSolverLookup
    -> Database
    -> SharedSolver
    -> [Vector]   -- ^ K demand vectors, length = dbActivityCount db
    -> Int        -- ^ recursion depth
    -> IO [Inventory]
goWithDeps depLookup db solver demands depth = do
    scalings <- solveMultiWithSharedSolver solver demands
    let localInvs = map (applyBiosphereMatrix db) scalings
    if depth >= maxDepsDepth
        then pure localInvs
        else do
            let perRootDepDemands = map (accumulateDepDemands db) scalings
                allDepDbs = S.toList $ S.unions $ map M.keysSet perRootDepDemands
            if null allDepDbs
                then pure localInvs
                else do
                    depContribsByDb <- mapConcurrently
                        (resolveDep depLookup perRootDepDemands depth (length demands))
                        allDepDbs
                    let perRootDepInvs = transpose depContribsByDb
                    pure $ zipWith
                        (foldr (M.unionWith (+)))
                        localInvs
                        perRootDepInvs

resolveDep
    :: DepSolverLookup
    -> [M.Map Text (M.Map (UUID, UUID) Double)]
    -> Int  -- ^ current depth (for recursion)
    -> Int  -- ^ K (so we can pad with empty on a missing dep DB)
    -> Text
    -> IO [Inventory]
resolveDep depLookup perRootDepDemands depth k depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            pure (replicate k M.empty)
        Just (depDb, depSolver) -> do
            let demandsPerRoot = map (M.findWithDefault M.empty depDbName) perRootDepDemands
                depDemandVecs  = map (depDemandsToVector depDb) demandsPerRoot
            goWithDeps depLookup depDb depSolver depDemandVecs (depth + 1)

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
    computeInventoryMatrixBatchCached
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, SomeException)
import Data.Text (Text)
import Progress
import Types
import Matrix
    ( Vector
    , Inventory
    , applyBiosphereMatrix
    , buildDemandVectorFromIndex
    , computeInventoryMatrixBatch
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

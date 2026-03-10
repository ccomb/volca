{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SharedSolver
Description : Shared PETSc solver with lazy factorization and thread synchronization

Implements lazy factorization: the PETSc KSP is not built at startup, but
on the first solve request. This eliminates ~3s × N databases of startup
latency while keeping sub-second solves after the first query.
-}

module SharedSolver (
    -- * Shared solver types
    SharedSolver,

    -- * Solver management
    createSharedSolver,

    -- * Concurrent solving
    solveWithSharedSolver
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, readMVar, modifyMVar)
import Control.Exception (catch, SomeException)
import Data.Text (Text)
import Progress
import Types
import Matrix (Vector, solveSparseLinearSystem, solveSparseLinearSystemWithFactorization, precomputeMatrixFactorization)

-- | Shared solver with lazy factorization and thread synchronization.
--   Factorization happens on first solve, not at startup.
data SharedSolver = SharedSolver
    { solverLock :: MVar ()                                -- ^ Serialize access to solver
    , solverFactorizationVar :: MVar (Maybe MatrixFactorization) -- ^ Lazy: Nothing until first solve
    , solverTechTriples :: [(Int, Int, Double)]             -- ^ Technosphere matrix data
    , solverActivityCount :: Int                            -- ^ Number of activities
    , solverDbName :: Text                                  -- ^ Database name (for PETSc cache key)
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

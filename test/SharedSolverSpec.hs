{-# LANGUAGE OverloadedStrings #-}

module SharedSolverSpec (spec) where

import qualified Data.Vector.Unboxed as U
import GoldenData
import Matrix (buildDemandVectorFromIndex, computeScalingVector)
import SharedSolver (SharedSolver, createSharedSolver, getFactorization, solveWithSharedSolver)
import Test.Hspec
import TestHelpers (assertVectorNear, loadSampleDatabase)
import Types

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- Lazy factorization
    -- -----------------------------------------------------------------------
    describe "lazy factorization" $ do
        it "getFactorization returns Nothing before first solve" $ do
            solver <- createSharedSolver "test" [] 0
            result <- getFactorization solver
            case result of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing before first solve"

        it "getFactorization returns Just after first solve (SAMPLE.min3)" $ do
            (solver, _) <- min3Solver
            fact <- getFactorization solver
            case fact of
                Just _ -> return ()
                Nothing -> expectationFailure "Expected factorization to be cached after solve"

    -- -----------------------------------------------------------------------
    -- Correctness: solve result matches golden values
    -- -----------------------------------------------------------------------
    describe "solve correctness (SAMPLE.min3)" $ do
        it "scaling vector matches golden values [1.0, 0.6, 0.24]" $ do
            (solver, db) <- min3Solver
            let demandVec = buildDemandVectorFromIndex (dbActivityIndex db) 0
            result <- solveWithSharedSolver solver demandVec
            assertVectorNear "scaling vector" defaultTolerance result sampleMin3ExpectedSupply

    -- -----------------------------------------------------------------------
    -- Second solve uses cached factorization
    -- -----------------------------------------------------------------------
    describe "caching" $ do
        it "second solve produces the same result" $ do
            (solver, db) <- min3Solver
            let demandVec = buildDemandVectorFromIndex (dbActivityIndex db) 0
            result1 <- solveWithSharedSolver solver demandVec
            result2 <- solveWithSharedSolver solver demandVec
            U.toList result1 `shouldBe` U.toList result2

-- ---------------------------------------------------------------------------
-- Helper: build solver + DB for SAMPLE.min3, with factorization pre-triggered
-- ---------------------------------------------------------------------------

min3Solver :: IO (SharedSolver, Database)
min3Solver = do
    db <- loadSampleDatabase "SAMPLE.min3"
    let techTriples =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
            ]
        actCount = fromIntegral (dbActivityCount db)
    solver <- createSharedSolver "SAMPLE.min3" techTriples actCount
    -- Trigger lazy factorization via first solve
    let demandVec = buildDemandVectorFromIndex (dbActivityIndex db) 0
    _ <- solveWithSharedSolver solver demandVec
    return (solver, db)

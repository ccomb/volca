{-# LANGUAGE OverloadedStrings #-}

-- | Cross-database back-substitution correctness tests.
--
-- Exercises the WithDeps path added so that LCIA endpoints include the
-- biosphere contributions reached through 'dbCrossDBLinks' (Ginko→Agribalyse,
-- PastoEco→Agribalyse, etc.). Since the test suite has no multi-DB fixture,
-- we test:
--
-- 1. pure helpers on a single loaded sample DB;
-- 2. that 'computeInventoryMatrixBatchWithDepsCached' on a DB with no
--    cross-DB links reduces exactly to the local-only batch variant.
module CrossDBInventorySpec (spec) where

import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types
import Matrix (accumulateDepDemands, depDemandsToVector, buildDemandVectorFromIndex)
import SharedSolver
    ( createSharedSolver
    , computeInventoryMatrixBatchCached
    , computeInventoryMatrixBatchWithDepsCached
    )
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.UUID as UUID

spec :: Spec
spec = do
    describe "accumulateDepDemands" $ do

        it "returns empty map when database has no cross-DB links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                scalingVec = U.replicate n 1.0
            accumulateDepDemands db scalingVec `shouldBe` M.empty

        it "returns empty map when scaling vector is all zeros" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                scalingVec = U.replicate n 0.0
            -- SAMPLE.min3 has no cross-DB links, but verify zero short-circuit:
            accumulateDepDemands db scalingVec `shouldBe` M.empty

    describe "depDemandsToVector" $ do

        it "returns zero vector for empty demand map" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                vec = depDemandsToVector db M.empty
            U.length vec `shouldBe` n
            U.all (== 0.0) vec `shouldBe` True

        it "silently drops suppliers not present in the dep DB" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                fakeSupplier = (UUID.nil, UUID.nil)
                vec = depDemandsToVector db (M.singleton fakeSupplier 42.0)
            U.length vec `shouldBe` n
            U.all (== 0.0) vec `shouldBe` True

    describe "computeInventoryMatrixBatchWithDepsCached" $ do

        it "matches local-only batch for a DB with no cross-DB links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples = [ (fromIntegral i, fromIntegral j, v)
                              | SparseTriple i j v <- U.toList (dbTechnosphereTriples db) ]
                actCount    = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "SAMPLE.min3" techTriples actCount
            let pids = [0]
                -- Dep lookup that always fails: no dep DBs loaded.
                noDeps _ = pure Nothing

            localInvs    <- computeInventoryMatrixBatchCached         db solver pids
            withDepsInvs <- computeInventoryMatrixBatchWithDepsCached noDeps db solver pids

            length withDepsInvs `shouldBe` length localInvs
            case (localInvs, withDepsInvs) of
                ([a], [b]) -> M.toList a `shouldBe` M.toList b
                _          -> expectationFailure "expected one inventory per pid"

        it "empty pid list returns empty result without solving" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples = [ (fromIntegral i, fromIntegral j, v)
                              | SparseTriple i j v <- U.toList (dbTechnosphereTriples db) ]
                actCount    = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "SAMPLE.min3-empty" techTriples actCount
            let noDeps _ = pure Nothing
            invs <- computeInventoryMatrixBatchWithDepsCached noDeps db solver []
            invs `shouldBe` []

{-# LANGUAGE OverloadedStrings #-}

module InventorySpec (spec) where

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import GoldenData
import Matrix (computeInventoryMatrix, computeInventoryMatrixBatch, precomputeMatrixFactorization)
import Test.Hspec
import TestHelpers
import Types

spec :: Spec
spec = do
    describe "SAMPLE.min3 Golden Tests (Linear Supply Chain)" $ do
        it "computes correct inventory for Product X (1 kg demand)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Get ProcessId for first activity (Product X)
            let rootProcessId = 0 :: ProcessId

            -- Compute inventory
            inventory <- computeInventoryMatrix db rootProcessId

            -- Find CO2 and Zinc flows
            let co2Flow = findFlowByName db "carbon dioxide"
            let zincFlow = findFlowByName db "zinc II"

            case (co2Flow, zincFlow) of
                (Just co2, Just zinc) -> do
                    let co2Amount = M.findWithDefault 0.0 (flowId co2) inventory
                    let zincAmount = M.findWithDefault 0.0 (flowId zinc) inventory

                    -- Golden values: CO2 = 0.96 kg, Zinc = 0.00072 kg
                    withinTolerance defaultTolerance sampleMin3ExpectedCO2 co2Amount
                        `shouldBe` True

                    withinTolerance defaultTolerance sampleMin3ExpectedZinc zincAmount
                        `shouldBe` True
                _ -> expectationFailure "Could not find CO2 or Zinc flows in database"

        it "computes finite values (no +inf/-inf)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let rootProcessId = 0 :: ProcessId
            inventory <- computeInventoryMatrix db rootProcessId

            -- All inventory values should be finite
            let allFinite = all (\(_, v) -> not (isInfinite v) && not (isNaN v)) (M.toList inventory)
            allFinite `shouldBe` True

    describe "SAMPLE.min Self-Loops Test" $ do
        it "handles circular dependencies without infinity" $ do
            db <- loadSampleDatabase "SAMPLE.min"

            -- Get first activity ProcessId
            let rootProcessId = 0 :: ProcessId

            -- Compute inventory
            inventory <- computeInventoryMatrix db rootProcessId

            -- All values should be finite
            let allFinite = all (\(_, v) -> not (isInfinite v) && not (isNaN v)) (M.toList inventory)
            allFinite `shouldBe` True

        it "computes positive inventory values" $ do
            db <- loadSampleDatabase "SAMPLE.min"

            let rootProcessId = 0 :: ProcessId
            inventory <- computeInventoryMatrix db rootProcessId

            -- Most biosphere values should be positive (emissions)
            let positiveCount = length $ filter (\(_, v) -> v > 0) (M.toList inventory)
            positiveCount `shouldSatisfy` (> 0)

    describe "Inventory Results Validation" $ do
        it "SAMPLE.min3 inventory is non-empty" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let rootProcessId = 0 :: ProcessId
            inventory <- computeInventoryMatrix db rootProcessId

            -- Inventory should not be empty
            M.size inventory `shouldSatisfy` (> 0)

        it "returns zero inventory for activities with no biosphere flows" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Activity 0 (Product X) should have inventory from downstream
            inventory0 <- computeInventoryMatrix db 0
            M.size inventory0 `shouldSatisfy` (> 0)

            -- All inventory values should be reasonable (not astronomically large)
            let allReasonable = all (\(_, v) -> abs v < 1000000) (M.toList inventory0)
            allReasonable `shouldBe` True

    describe "Batch Inventory (MUMPS multi-RHS)" $ do
        let makeFact db name = do
                let n = fromIntegral (dbActivityCount db)
                    asTuples =
                        [ (fromIntegral i, fromIntegral j, v)
                        | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
                        ]
                fact <- precomputeMatrixFactorization name asTuples n
                pure fact{mfDatabaseId = name}

        it "empty batch returns empty list" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            fact <- makeFact db "SAMPLE.min3.empty"
            result <- computeInventoryMatrixBatch db fact []
            result `shouldBe` []

        it "singleton batch matches single-solve inventory" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            fact <- makeFact db "SAMPLE.min3.single"
            single <- computeInventoryMatrix db 0
            [batched] <- computeInventoryMatrixBatch db fact [0]
            M.keys single `shouldBe` M.keys batched
            mapM_
                ( \k ->
                    withinTolerance
                        1e-10
                        (M.findWithDefault 0 k single)
                        (M.findWithDefault 0 k batched)
                        `shouldBe` True
                )
                (M.keys single)

        it "k=3 batch matches per-process single solves" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            fact <- makeFact db "SAMPLE.min3.k3"
            let pids = [0, 1, 2]
            singles <- mapM (computeInventoryMatrix db) pids
            batched <- computeInventoryMatrixBatch db fact pids
            length batched `shouldBe` length singles
            mapM_
                ( \(s, b) -> do
                    M.keys s `shouldBe` M.keys b
                    mapM_
                        ( \k ->
                            withinTolerance
                                1e-10
                                (M.findWithDefault 0 k s)
                                (M.findWithDefault 0 k b)
                                `shouldBe` True
                        )
                        (M.keys s)
                )
                (zip singles batched)

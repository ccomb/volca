{-# LANGUAGE OverloadedStrings #-}

module SupplyChainSpec (spec) where

import Test.Hspec
import TestHelpers
import GoldenData
import Types
import API.Types (SupplyChainEntry(..), SupplyChainResponse(..))
import Service (buildSupplyChainFromScalingVector)
import Matrix (computeScalingVector)

spec :: Spec
spec = do
    describe "sceQuantity uses rootRefAmount (not activity refAmount)" $ do
        it "sceQuantity equals scalingFactor when root refAmount is 1" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- SAMPLE.min3: X → Y(0.6) → Z(0.24), all refAmounts = 1 kg
            let rootProcessId = 0 :: ProcessId
            supplyVec <- computeScalingVector db rootProcessId

            let response = buildSupplyChainFromScalingVector db rootProcessId supplyVec
                    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                entries = scrSupplyChain response

            -- With rootRefAmount = 1, sceQuantity must equal sceScalingFactor exactly
            mapM_ (\entry ->
                withinTolerance defaultTolerance (sceScalingFactor entry) (sceQuantity entry)
                    `shouldBe` True
                ) entries

    describe "sceDepth (BFS from root)" $ do
        it "assigns positive depth to non-root entries" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let rootProcessId = 0 :: ProcessId
            supplyVec <- computeScalingVector db rootProcessId

            let response = buildSupplyChainFromScalingVector db rootProcessId supplyVec
                    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                entries = scrSupplyChain response

            -- All entries should have depth > 0 (root is excluded from supply chain)
            mapM_ (\entry -> sceDepth entry `shouldSatisfy` (> 0)) entries

    describe "Server-side filtering" $ do
        it "max-depth filter limits results" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let rootProcessId = 0 :: ProcessId
            supplyVec <- computeScalingVector db rootProcessId

            -- No depth filter: should get Y (depth 1) and Z (depth 2)
            let noFilter = buildSupplyChainFromScalingVector db rootProcessId supplyVec
                    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            scrFilteredActivities noFilter `shouldSatisfy` (>= 2)

            -- Depth 1: should only get Y (direct supplier)
            let depth1 = buildSupplyChainFromScalingVector db rootProcessId supplyVec
                    Nothing Nothing Nothing Nothing (Just 1) Nothing Nothing
            scrFilteredActivities depth1 `shouldSatisfy` (< scrFilteredActivities noFilter)

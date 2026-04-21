{-# LANGUAGE OverloadedStrings #-}

module HotspotSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import GoldenData
import Matrix (computeProcessLCIAContributions, computeScalingVector)
import Test.Hspec
import TestHelpers
import Types

spec :: Spec
spec = do
    describe "computeProcessLCIAContributions - SAMPLE.min3" $ do
        it "only Z has a non-zero contribution for a CF=1 CO2 method" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let co2Flow = findFlowByName db "carbon dioxide"
            case co2Flow of
                Nothing -> expectationFailure "CO2 flow not found in SAMPLE.min3"
                Just co2 -> do
                    scalingVec <- computeScalingVector db 0
                    let cfMap = M.singleton (flowId co2) 1.0
                    let contribs = computeProcessLCIAContributions db scalingVec cfMap
                    let nonZero = M.filter (\v -> abs v > 1e-15) contribs
                    -- In SAMPLE.min3 only Z emits CO2 directly
                    M.size nonZero `shouldBe` 1

        it "total contribution equals sampleMin3ExpectedCO2 (0.96 kg)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let co2Flow = findFlowByName db "carbon dioxide"
            case co2Flow of
                Nothing -> expectationFailure "CO2 flow not found in SAMPLE.min3"
                Just co2 -> do
                    scalingVec <- computeScalingVector db 0
                    let cfMap = M.singleton (flowId co2) 1.0
                    let contribs = computeProcessLCIAContributions db scalingVec cfMap
                    let total = sum (M.elems contribs)
                    withinTolerance defaultTolerance sampleMin3ExpectedCO2 total
                        `shouldBe` True

        it "contributing processId maps to Z's activity UUID" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let co2Flow = findFlowByName db "carbon dioxide"
            let zUUID = read sampleMin3ActivityZ :: UUID
            case co2Flow of
                Nothing -> expectationFailure "CO2 flow not found in SAMPLE.min3"
                Just co2 -> do
                    scalingVec <- computeScalingVector db 0
                    let cfMap = M.singleton (flowId co2) 1.0
                    let nonZero =
                            M.filter (\v -> abs v > 1e-15) $
                                computeProcessLCIAContributions db scalingVec cfMap
                    case M.keys nonZero of
                        [pid] ->
                            let (actUUID, _) = dbProcessIdTable db V.! fromIntegral pid
                             in actUUID `shouldBe` zUUID
                        _ -> expectationFailure "Expected exactly one contributing process"

        it "zero contribution for a flow not in the database" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            -- A random UUID that doesn't exist in the database
            let ghostUUID = read "00000000-0000-0000-0000-000000000001" :: UUID
            scalingVec <- computeScalingVector db 0
            let cfMap = M.singleton ghostUUID 1.0
            let contribs = computeProcessLCIAContributions db scalingVec cfMap
            sum (M.elems contribs) `shouldBe` 0.0

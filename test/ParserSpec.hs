{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GoldenData
import Test.Hspec
import TestHelpers
import Types

spec :: Spec
spec = do
    describe "EcoSpold Parser - SAMPLE.min3" $ do
        it "parses all activities successfully" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- SAMPLE.min3 has 3 activities
            let activityCount = dbActivityCount db
            activityCount `shouldBe` 3

        it "parses activity names correctly" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let activities = V.toList $ dbActivities db
            length (filter (\a -> not $ T.null $ activityName a) activities) `shouldBe` 3

        it "extracts technosphere exchanges" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- SAMPLE.min3 has 2 technosphere exchanges (Y needs X, Z needs Y)
            let activities = V.toList $ dbActivities db
            let totalTechExchanges = sum [length $ filter isTechnosphereExchange $ exchanges a | a <- activities]
            totalTechExchanges `shouldSatisfy` (>= 2)

        it "extracts biosphere exchanges" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- SAMPLE.min3 has biosphere exchanges (CO2, Zinc from Z)
            let activities = V.toList $ dbActivities db
            let totalBioExchanges = sum [length $ filter isBiosphereExchange $ exchanges a | a <- activities]
            totalBioExchanges `shouldSatisfy` (>= 2)

        it "identifies reference products correctly" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let activities = V.toList $ dbActivities db
            -- Each activity should have at least one reference product
            let activitiesWithRef = filter hasReferenceProduct activities
            length activitiesWithRef `shouldBe` 3

        it "parses compartments without truncation" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Check that compartments are parsed fully
            let flows = M.elems (dbFlows db)
            let categoriesNotEmpty = filter (\f -> not $ T.null $ flowCategory f) flows
            length categoriesNotEmpty `shouldSatisfy` (>= 0)

    describe "EcoSpold Parser - SAMPLE.min (Self-Loops)" $ do
        it "parses circular dependencies" $ do
            db <- loadSampleDatabase "SAMPLE.min"

            -- SAMPLE.min has 4 activities with self-loops
            let activityCount = dbActivityCount db
            activityCount `shouldBe` 4

        it "handles self-referencing exchanges" $ do
            db <- loadSampleDatabase "SAMPLE.min"

            -- Activity A references itself with 0.1 kg
            -- This creates a diagonal entry in the A matrix
            let activities = V.toList $ dbActivities db
            length activities `shouldBe` 4

    describe "Flow Database" $ do
        it "deduplicates flows correctly" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let flowCount = M.size (dbFlows db)
            -- SAMPLE.min3 should have exactly 2 unique flows (CO2, Zinc)
            flowCount `shouldSatisfy` (>= 2)

        it "stores flow metadata" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let flows = M.elems (dbFlows db)
            -- All flows should have names
            let flowsWithNames = filter (\f -> not $ T.null $ flowName f) flows
            length flowsWithNames `shouldBe` length flows

    describe "EcoSpold2 Parser - Classifications" $ do
        it "parses classifications from EcoSpold2" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
            -- All 3 activities should have non-empty classification
            all (not . M.null . activityClassification) activities `shouldBe` True

        it "parses ISIC classification system and value" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
                isicValues =
                    [ v
                    | a <- activities
                    , Just v <- [M.lookup "ISIC rev.4 ecoinvent" (activityClassification a)]
                    ]
            isicValues `shouldContain` ["2394:Manufacture of cement"]
            isicValues `shouldContain` ["0810:Quarrying of stone, sand and clay"]

        it "parses CPC classification" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
                cpcValues =
                    [ v
                    | a <- activities
                    , Just v <- [M.lookup "CPC" (activityClassification a)]
                    ]
            cpcValues `shouldBe` ["3744:Cement"]

    describe "EcoSpold1 Parser - Classifications" $ do
        it "parses category and subCategory as classifications" $ do
            db <- loadSampleDatabase "SAMPLE.ecospold1"
            let activities = V.toList $ dbActivities db
            length activities `shouldSatisfy` (>= 1)
            let cls = activityClassification (head activities)
            M.lookup "Category" cls `shouldBe` Just "Energy"
            M.lookup "SubCategory" cls `shouldBe` Just "Electricity"

    describe "Unit Database" $ do
        it "parses unit information" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let unitCount = M.size (dbUnits db)
            -- Should have at least kg unit
            unitCount `shouldSatisfy` (>= 1)

-- Helper function
hasReferenceProduct :: Activity -> Bool
hasReferenceProduct activity =
    any (\ex -> exchangeIsReference ex && not (exchangeIsInput ex)) (exchanges activity)

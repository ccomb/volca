{-# LANGUAGE OverloadedStrings #-}

{- | Computed-checks report tests: the log-scale outlier norm and its guard
rails (group size, degenerate MAD, per-unit grouping), plus the zero and
negative checks – all on hand-made scored entries.
-}
module ComputedQualitySpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Database.ComputedQuality (
    CategoryScore (..),
    ComputedQualityReport (..),
    ScoredEntry (..),
    computedQualityReport,
 )
import Database.Quality (QualityCheck (..), QualityOffender (..))
import Types (Severity (..))

entry :: Text -> [CategoryScore] -> ScoredEntry
entry name scores =
    ScoredEntry
        { seProcessId = name <> "_pid"
        , seActivityName = name
        , seLocation = "FR"
        , seProductName = Just (name <> " product")
        , seRefUnit = "kg"
        , seScores = scores
        }

cch :: Double -> CategoryScore
cch = CategoryScore "Climate change" "kg CO2 eq"

reportOf :: [ScoredEntry] -> ComputedQualityReport
reportOf = computedQualityReport "testdb" "EF-3.1"

{- | Honest scores spread over 10..34 – wide enough for a non-degenerate MAD,
tight enough on the log scale that none of the crowd flags itself.
-}
crowd :: [ScoredEntry]
crowd = [entry ("crowd " <> T.pack (show i)) [cch (fromIntegral i)] | i <- [10 .. 34 :: Int]]

names :: QualityCheck -> [Text]
names = map qoActivityName . qcOffenders

details :: QualityCheck -> [Text]
details = map qoDetail . qcOffenders

spec :: Spec
spec = do
    describe "score outliers check" $ do
        it "flags the entry a million times above the norm, and only it" $ do
            let check = cqScoreOutliers (reportOf (crowd <> [entry "monster" [cch 1e6]]))
            names check `shouldBe` ["monster"]
            map qoSeverity (qcOffenders check) `shouldBe` [WarningSev]
            case details check of
                [d] -> do
                    d `shouldSatisfy` T.isInfixOf "above the median"
                    d `shouldSatisfy` T.isInfixOf "Climate change"
                    d `shouldSatisfy` T.isInfixOf "kg-referenced"
                other -> expectationFailure ("expected one detail, got " <> show other)

        it "flags the entry a million times below the norm as below" $ do
            let check = cqScoreOutliers (reportOf (crowd <> [entry "dust" [cch 1e-6]]))
            names check `shouldBe` ["dust"]
            case details check of
                [d] -> d `shouldSatisfy` T.isInfixOf "below the median"
                other -> expectationFailure ("expected one detail, got " <> show other)

        it "ranks outliers by deviation, wildest first, not by name" $ do
            let check =
                    cqScoreOutliers
                        (reportOf (crowd <> [entry "a mild outlier" [cch 1e4], entry "z wild outlier" [cch 1e8]]))
            names check `shouldBe` ["z wild outlier", "a mild outlier"]

        it "judges nothing in a group smaller than the minimum" $ do
            let check = cqScoreOutliers (reportOf (take 10 crowd <> [entry "monster" [cch 1e6]]))
            qcOffenders check `shouldBe` []

        it "compares entries only within their reference unit" $ do
            -- The kWh entry is huge next to the kg crowd, but it has no kg
            -- peers to be compared with – one entry is not a norm.
            let power = (entry "power plant" [cch 1e6]){seRefUnit = "kWh"}
                check = cqScoreOutliers (reportOf (crowd <> [power]))
            qcOffenders check `shouldBe` []

        it "judges nothing against a degenerate norm where half the group is identical" $ do
            let same = [entry ("same " <> T.pack (show i)) [cch 1.0] | i <- [1 .. 25 :: Int]]
                check = cqScoreOutliers (reportOf (same <> [entry "monster" [cch 1e6]]))
            qcOffenders check `shouldBe` []

    describe "zero scores check" $ do
        it "flags an entry whose every category score is zero" $ do
            let zeroed = entry "placeholder" [cch 0, CategoryScore "Land use" "Pt" 0]
                check = cqZeroScores (reportOf (crowd <> [zeroed]))
            names check `shouldBe` ["placeholder"]
            map qoSeverity (qcOffenders check) `shouldBe` [WarningSev]

        it "passes an entry with any non-zero score, and an entry with no scores at all" $ do
            let mixed = entry "mixed" [cch 0, CategoryScore "Land use" "Pt" 1]
                bare = entry "bare" []
            qcOffenders (cqZeroScores (reportOf (crowd <> [mixed, bare]))) `shouldBe` []

    describe "negative scores check" $ do
        it "flags negative categories as info, naming the worst" $ do
            let credit = entry "avoided burden" [cch (-5), CategoryScore "Land use" "Pt" (-9)]
                check = cqNegativeScores (reportOf (crowd <> [credit]))
            names check `shouldBe` ["avoided burden"]
            map qoSeverity (qcOffenders check) `shouldBe` [InfoSev]
            case details check of
                [d] -> do
                    d `shouldSatisfy` T.isInfixOf "2 category score(s) below zero"
                    d `shouldSatisfy` T.isInfixOf "Land use"
                other -> expectationFailure ("expected one detail, got " <> show other)

        it "keeps negative scores out of the outlier norm" $ do
            qcOffenders (cqScoreOutliers (reportOf (crowd <> [entry "credit" [cch (-1e6)]])))
                `shouldBe` []

    describe "report header" $
        it "carries the collection and counts every entry handed over" $ do
            let r = reportOf crowd
            cqCollection r `shouldBe` "EF-3.1"
            cqProcessCount r `shouldBe` 25

{-# LANGUAGE OverloadedStrings #-}

{- | The weights that split a single score into the parts of its indicators:
read from the set's own formulas when the score is a weighted sum, refused
when it is not.
-}
module ScoreWeightsSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Method.Types (ScoringSet (..), ScoringSetOrigin (..), scoreWeights)
import Test.Hspec

-- | A computed variable, a normalization, weights and a display multiplier.
scoringSet :: ScoringSet
scoringSet =
    ScoringSet
        { ssName = "Single"
        , ssUnit = "Pts"
        , ssVariables =
            M.fromList
                [ ("cch", "Climate change")
                , ("etfo", "Ecotoxicity, organics")
                , ("etfi", "Ecotoxicity, inorganics")
                ]
        , ssComputed = M.fromList [("etf", "2 * etfo + etfi")]
        , ssLabels = M.empty
        , ssNormalization = M.fromList [("cch", 4.0)]
        , ssWeighting = M.fromList [("cch", 0.5), ("etf", 0.25)]
        , ssScores = M.fromList [("total", "cch + etf")]
        , ssDisplayMultiplier = Just 10
        , ssUnits = M.empty
        , ssOrigin = DeclaredInConfig
        }

rawScores :: M.Map Text Double
rawScores =
    M.fromList
        [ ("Climate change", 8)
        , ("Ecotoxicity, organics", 2)
        , ("Ecotoxicity, inorganics", 4)
        ]

withTotal :: Text -> ScoringSet
withTotal formula = scoringSet{ssScores = M.fromList [("total", formula)]}

spec :: Spec
spec = describe "scoreWeights" $ do
    it "reads one weight per indicator through computed variables, normalization and display scale" $
        scoreWeights scoringSet "total" rawScores
            `shouldBe` Right
                ( M.fromList
                    [ ("Climate change", 0.5 / 4 * 10)
                    , ("Ecotoxicity, organics", 2 * 0.25 * 10)
                    , ("Ecotoxicity, inorganics", 0.25 * 10)
                    ]
                )

    it "gives weights whose parts add up to the score" $
        fmap (sum . M.intersectionWith (*) rawScores) (scoreWeights scoringSet "total" rawScores)
            `shouldBe` Right (8 * 0.5 / 4 * 10 + (2 * 2 + 4) * 0.25 * 10)

    it "refuses a product of indicators" $
        scoreWeights (withTotal "cch * etf") "total" rawScores `shouldSatisfy` isLeft

    it "refuses a constant term" $
        scoreWeights (withTotal "cch + etf + 1") "total" rawScores `shouldSatisfy` isLeft

    it "refuses a score the set does not have" $
        scoreWeights scoringSet "other" rawScores `shouldSatisfy` isLeft

    it "refuses rather than reading a missing indicator as zero" $
        scoreWeights scoringSet "total" (M.delete "Climate change" rawScores) `shouldSatisfy` isLeft

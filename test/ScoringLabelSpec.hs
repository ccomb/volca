{-# LANGUAGE OverloadedStrings #-}

{- | Display names for scoring-set breakdown indicators: primitive variables
take their impact-category name, computed variables take their entry in
@labels@, and only as a last resort does the raw variable key leak out.
-}
module ScoringLabelSpec (spec) where

import API.Routes (computeAllScoringSets)
import API.Types (ScoringIndicator (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Method.Types (ScoringSet (..), ScoringSetOrigin (..))
import Test.Hspec

scoringSet :: ScoringSet
scoringSet =
    ScoringSet
        { ssName = "SingleScore"
        , ssUnit = "Pts"
        , ssVariables =
            M.fromList
                [ ("cch", "Climate change")
                , ("etfo", "Ecotoxicity, freshwater_organics")
                , ("etfi", "Ecotoxicity, freshwater_inorganics")
                ]
        , ssComputed = M.fromList [("etf", "2 * etfo + etfi")]
        , ssLabels = M.fromList [("etf", "Ecotoxicity, freshwater")]
        , ssNormalization = M.empty
        , ssWeighting = M.fromList [("cch", 1.0), ("etf", 1.0)]
        , ssScores = M.fromList [("total", "cch + etf")]
        , ssDisplayMultiplier = Nothing
        , ssUnits = M.empty
        , ssOrigin = DeclaredInConfig
        }

rawScores :: M.Map Text Double
rawScores =
    M.fromList
        [ ("Climate change", 10.0)
        , ("Ecotoxicity, freshwater_organics", 2.0)
        , ("Ecotoxicity, freshwater_inorganics", 3.0)
        ]

-- | Display name of one breakdown indicator, straight from the scoring pass.
categoryOf :: ScoringSet -> Text -> IO (Maybe Text)
categoryOf ss var = do
    (_, indicators) <- computeAllScoringSets [ss] rawScores
    pure (siCategory <$> (M.lookup (ssName ss) indicators >>= M.lookup var))

spec :: Spec
spec = describe "scoring indicator display names" $ do
    it "labels a computed variable via [methods.scoring.labels]" $
        categoryOf scoringSet "etf" >>= (`shouldBe` Just "Ecotoxicity, freshwater")

    it "keeps impact-category names for primitive variables" $
        categoryOf scoringSet "cch" >>= (`shouldBe` Just "Climate change")

    it "falls back to the raw key for a computed variable with no label" $
        categoryOf scoringSet{ssLabels = M.empty} "etf" >>= (`shouldBe` Just "etf")

{-# LANGUAGE OverloadedStrings #-}

module MethodScoringSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Method.Types

-- | A set reading two categories, with the computed variables and scores given.
setWith :: [(Text, Text)] -> [(Text, Text)] -> ScoringSet
setWith computed scores =
    ScoringSet
        { ssName = "Test"
        , ssUnit = "Pt"
        , ssVariables = M.fromList [("cc", "Climate change"), ("ozone_depletion_long_name", "Ozone depletion")]
        , ssComputed = M.fromList computed
        , ssLabels = M.empty
        , ssNormalization = M.empty
        , ssWeighting = M.fromList [(v, 1) | (v, _) <- computed]
        , ssScores = M.fromList scores
        , ssDisplayMultiplier = Nothing
        , ssUnits = M.empty
        , ssOrigin = CreatedInJournal
        }

raw :: M.Map Text Double
raw = M.fromList [("Climate change", 2), ("Ozone depletion", 3)]

spec :: Spec
spec = describe "Computed variables" $ do
    it "computes a variable after the ones it reads, whatever the length of their formulas" $ do
        let set = setWith [("b", "2 * a"), ("a", "cc + ozone_depletion_long_name")] [("Score", "b")]
        fmap (M.lookup "Score" . seScores) (computeFormulaScores set raw) `shouldBe` Right (Just 10)

    it "refuses two computed variables that read each other, naming them" $ do
        let set = setWith [("a", "b + cc"), ("b", "a")] [("Score", "a")]
        computeFormulaScores set raw `shouldSatisfy` either (\e -> all (`T.isInfixOf` T.pack e) ["'a'", "'b'"]) (const False)

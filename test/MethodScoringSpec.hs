{-# LANGUAGE OverloadedStrings #-}

module MethodScoringSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Method.Scoring
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

-- | The change with its two values exchanged: what undoes it.
swapped :: ScoringChange -> ScoringChange
swapped (SetText e k b a) = SetText e k a b
swapped (SetNumber e k b a) = SetNumber e k a b
swapped (RenameSet a b) = RenameSet b a
swapped (SetUnitOfSet a b) = SetUnitOfSet b a
swapped (SetDisplayMultiplier a b) = SetDisplayMultiplier b a

base :: ScoringSet
base = setWith [("a", "cc + ozone_depletion_long_name")] [("Score", "a")]

spec :: Spec
spec = do
  describe "applyChange" $ do
    it "sets, replaces and removes an entry" $ do
        fmap ssWeighting (applyChange (SetNumber WeightOf "a" (Just 1) (Just 0.3)) base) `shouldBe` Right (M.fromList [("a", 0.3)])
        fmap ssLabels (applyChange (SetText LabelOf "a" Nothing (Just "Human health")) base) `shouldBe` Right (M.fromList [("a", "Human health")])
        fmap ssScores (applyChange (SetText ScoreOf "Score" (Just "a") Nothing) base) `shouldBe` Right M.empty

    it "refuses a value other than the one recorded, naming the entry and what it holds" $
        applyChange (SetNumber WeightOf "a" (Just 0.5) (Just 0.3)) base
            `shouldBe` Left "The scoring set 'Test' has the weight 1.0 for 'a', not 0.5 as recorded."

    it "refuses to set an entry the line says is absent when it is there" $
        applyChange (SetText FormulaOf "a" Nothing (Just "cc")) base
            `shouldBe` Left "The scoring set 'Test' has the formula 'cc + ozone_depletion_long_name' for 'a', not none as recorded."

    it "renames the set it is given by its name only" $ do
        fmap ssName (applyChange (RenameSet "Test" "Other") base) `shouldBe` Right "Other"
        applyChange (RenameSet "Elsewhere" "Other") base `shouldBe` Left "The scoring set is named 'Test', not 'Elsewhere' as recorded."

    it "gives back the set it started from when the change is undone, for every form" $
        mapM_
            (\c -> (applyChange c base >>= applyChange (swapped c)) `shouldBe` Right base)
            [ SetText CategoryOf "x" Nothing (Just "Water use")
            , SetText FormulaOf "a" (Just "cc + ozone_depletion_long_name") (Just "cc")
            , SetText LabelOf "a" Nothing (Just "A")
            , SetText VariableUnitOf "a" Nothing (Just "DALY")
            , SetText ScoreOf "Score" (Just "a") (Just "2 * a")
            , SetNumber NormalizationOf "a" Nothing (Just 2)
            , SetNumber WeightOf "a" (Just 1) Nothing
            , RenameSet "Test" "Other"
            , SetUnitOfSet "Pt" "mPt"
            , SetDisplayMultiplier Nothing (Just 1000)
            ]

  describe "checkSet" $ do
    it "accepts a set that scores, an entry naming no variable included" $
        checkSet base{ssWeighting = M.insert "nowhere" 0.5 (ssWeighting base)} `shouldBe` Right ()

    it "refuses a formula naming nothing in the set" $
        checkSet base{ssScores = M.singleton "Score" "a + ghost"} `shouldSatisfy` either ("ghost" `T.isInfixOf`) (const False)

    it "refuses a formula it cannot read" $
        checkSet base{ssScores = M.singleton "Score" "a +"} `shouldSatisfy` either ("Score" `T.isInfixOf`) (const False)

    it "refuses a simple and a computed variable whose names differ only by case" $
        checkSet base{ssComputed = M.insert "CC" "2" (ssComputed base)}
            `shouldBe` Left "The scoring set 'Test' names two variables 'CC' and 'cc', which a formula cannot tell apart."

  describe "Computed variables" $ do
      it "computes a variable after the ones it reads, whatever the length of their formulas" $ do
        let set = setWith [("b", "2 * a"), ("a", "cc + ozone_depletion_long_name")] [("Score", "b")]
        fmap (M.lookup "Score" . seScores) (computeFormulaScores set raw) `shouldBe` Right (Just 10)

      it "refuses two computed variables that read each other, naming them" $ do
        let set = setWith [("a", "b + cc"), ("b", "a")] [("Score", "a")]
        computeFormulaScores set raw `shouldSatisfy` either (\e -> all (`T.isInfixOf` T.pack e) ["'a'", "'b'"]) (const False)

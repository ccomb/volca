{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MethodScoringEditSpec (spec) where

import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Hspec

import Method.EditPlan
import Method.Journal
import Method.ScoringEdit
import Method.SimaProScoring (DamageCategory (..), NormWeightSet (..), translateScoring)
import Method.Types

uuid :: Int -> UUID.UUID
uuid n = fromMaybe UUID.nil (UUID.fromText ("00000000-0000-0000-0000-" <> T.justifyRight 12 '0' (T.pack (show n))))

category :: Int -> Text -> Method
category n name = Method (uuid n) name Nothing "u" name Nothing []

climate, ozone, water :: Method
climate = category 1 "Climate change"
ozone = category 2 "Ozone depletion"
water = category 3 "Water use"

collection :: MethodCollection
collection = MethodCollection [climate, ozone, water] [] []

draft :: Text -> [(Int, Double)] -> Maybe Double -> Maybe Double -> RowDraft
draft label terms = RowDraft label Nothing (termsOf terms)
  where
    termsOf :: [(Int, Double)] -> NonEmpty (UUID.UUID, Double)
    termsOf ((n, c) : rest) = (uuid n, c) :| [(uuid m, d) | (m, d) <- rest]
    termsOf [] = (uuid 0, 0) :| []

-- | The collection a planned line leaves.
leaving :: MethodCollection -> ScoringEdit -> Either Text MethodCollection
leaving start edit = planScoringEdit start edit >>= applyMethodOp start . fst

-- | The one set of a collection.
soleSet :: MethodCollection -> Either Text ScoringSet
soleSet c = case mcScoringSets c of
    [set] -> Right set
    sets -> Left ("expected one set, got " <> T.pack (show (map ssName sets)))

-- | A collection holding a set of two rows, "Health" and "Resources", summed.
twoRows :: MethodCollection
twoRows =
    fromRight collection $
        leaving collection (NewSet "EF" Nothing [draft "Health" [(1, 1), (2, 1)] (Just 2) (Just 0.5), draft "Resources" [(3, 1)] Nothing (Just 0.25)])

-- | Plan a change, apply it, then apply its inverse.
back :: MethodCollection -> ScoringEdit -> Either Text MethodCollection
back start edit = do
    (op, _) <- planScoringEdit start edit
    changed <- applyMethodOp start op
    inverseOf changed op >>= \case
        UndoWith inverse -> applyMethodOp changed inverse
        UndoSelector _ -> Left "a scoring set's change is never undone by a selector"

refusal :: Either Text a -> String
refusal = either T.unpack (const "accepted")

-- | A collection carrying the set a SimaPro file's damages and weighting translate to.
translated :: MethodCollection
translated =
    MethodCollection
        [climate, category 4 "Ecotoxicity - part 1", category 5 "Ecotoxicity - part 2", water]
        (fst (translateScoring [climate, category 4 "Ecotoxicity - part 1", category 5 "Ecotoxicity - part 2", water] damages [nw]))
        []
  where
    damages :: [DamageCategory]
    damages = [DamageCategory "Climate change" "kg" [("Climate change", 1)], DamageCategory "Ecotoxicity" "CTUe" [("Ecotoxicity - part 1", 1), ("Ecotoxicity - part 2", 1)]]
    nw :: NormWeightSet
    nw = NormWeightSet "EF 3.1" (M.fromList [("Climate change", 1.3e-4), ("Ecotoxicity", 1.76e-5)]) (M.fromList [("Climate change", 0.21), ("Ecotoxicity", 0.02), ("Water use", 0.08)])

spec :: Spec
spec = describe "planning a change to a scoring set" $ do
    it "creates a set whose rows group categories under short names, summed into a single score" $
        fmap (\s -> (ssVariables s, ssComputed s, ssScores s)) (soleSet twoRows)
            `shouldBe` Right
                ( M.fromList [("climate_change", "Climate change"), ("ozone_depletion", "Ozone depletion"), ("water_use", "Water use")]
                , M.fromList [("health", "climate_change + ozone_depletion"), ("resources", "water_use")]
                , M.fromList [("Single score", "health")]
                )

    it "adds a row to the single score, reusing the variable of a category already read" $
        fmap (\s -> (ssScores s, M.lookup "both" (ssComputed s), M.size (ssVariables s))) (leaving twoRows (AddRow "EF" (draft "Both" [(1, 2)] (Just 1) (Just 1))) >>= soleSet)
            `shouldBe` Right (M.fromList [("Single score", "both + health")], Just "2.0 * climate_change", 3)

    it "leaves a score written otherwise as it is" $ do
        let written = fromRight twoRows (leaving twoRows (PutScore "EF" "Single score" "2 * health"))
        fmap ssScores (leaving written (AddRow "EF" (draft "Both" [(1, 1)] (Just 1) (Just 1))) >>= soleSet)
            `shouldBe` Right (M.fromList [("Single score", "2 * health")])

    it "refuses a row labelled as another row" $
        refusal (leaving twoRows (AddRow "EF" (draft "Health" [(3, 1)] Nothing Nothing))) `shouldContain` "already has a row labelled 'Health'"

    -- With no normalization left, the set normalizes nothing, so a weight alone counts.
    it "takes a row out with its entries, the variables it alone read, and its place in the sum" $
        fmap (\s -> (M.keys (ssVariables s), M.keys (ssComputed s), ssScores s, ssLabels s)) (leaving twoRows (DeleteRow "EF" "health") >>= soleSet)
            `shouldBe` Right (["water_use"], ["resources"], M.fromList [("Single score", "resources")], M.fromList [("resources", "Resources")])

    it "refuses to take out a row a score reads otherwise, naming the score" $ do
        let written = fromRight twoRows (leaving twoRows (PutScore "EF" "Twice" "2 * health"))
        refusal (leaving written (DeleteRow "EF" "health")) `shouldContain` "'Twice'"

    it "rewrites a row's grouping, and refuses a change that changes nothing" $ do
        fmap (M.lookup "health" . ssComputed) (leaving twoRows (ChangeRow "EF" "health" (draft "Health" [(1, 0.5), (2, 1)] (Just 2) (Just 0.5))) >>= soleSet)
            `shouldBe` Right (Just "0.5 * climate_change + ozone_depletion")
        refusal (leaving twoRows (ChangeRow "EF" "health" (draft "Health" [(1, 1), (2, 1)] (Just 2) (Just 0.5)))) `shouldContain` "as it is"

    it "adds a row given a weight to the single score, and takes it out when the weight goes" $ do
        let weighed = leaving twoRows (ChangeRow "EF" "resources" (draft "Resources" [(3, 1)] (Just 4) (Just 0.25)))
        fmap ssScores (weighed >>= soleSet) `shouldBe` Right (M.fromList [("Single score", "health + resources")])
        fmap ssScores (weighed >>= \w -> leaving w (ChangeRow "EF" "resources" (draft "Resources" [(3, 1)] (Just 4) Nothing)) >>= soleSet)
            `shouldBe` Right (M.fromList [("Single score", "health")])

    it "makes a weighted category of a translated set a computed row once it groups a second category" $ do
        let promoted = leaving translated (ChangeRow "EF 3.1" "water_use" (draft "Water" [(3, 1), (1, 1)] Nothing (Just 0.08)))
        fmap (\s -> (M.lookup "water" (ssComputed s), M.lookup "water" (ssWeighting s), M.member "water_use" (ssWeighting s), M.member "water_use" (ssVariables s))) (promoted >>= soleSet)
            `shouldBe` Right (Just "water_use + climate_change", Just 0.08, False, True)

    it "takes a translated weighted category out of the rows, and its variable once nothing reads it" $
        fmap (\s -> (M.member "water_use" (ssWeighting s), M.member "water_use" (ssVariables s))) (leaving translated (DeleteRow "EF 3.1" "water_use") >>= soleSet)
            `shouldBe` Right (False, False)

    it "refuses a formula for a variable the set does not compute, naming those it does" $
        refusal (leaving twoRows (WriteFormula "EF" "ghost" "1")) `shouldContain` "'health', 'resources'"

    it "refuses a formula naming nothing the set holds" $
        refusal (leaving twoRows (WriteFormula "EF" "health" "climate_change + ghost")) `shouldContain` "ghost"

    it "refuses a normalization of zero in a new set" $
        refusal (leaving collection (NewSet "EF" Nothing [draft "Health" [(1, 1)] (Just 0) (Just 1)])) `shouldContain` "normalization"

    it "undoes every change it plans back to where it started" $
        mapM_
            (\edit -> back twoRows edit `shouldBe` Right twoRows)
            [ NewSet "Other" (Just "mPt") [draft "Climate" [(1, 1)] Nothing (Just 1)]
            , DeleteSet "EF"
            , RenameScoringSet "EF" "EF bis"
            , ChangeSetUnit "EF" "mPt"
            , ChangeMultiplier "EF" (Just 1000)
            , AddRow "EF" (draft "Both" [(1, 2)] (Just 1) (Just 1))
            , ChangeRow "EF" "health" (draft "Human health" [(1, 0.5)] Nothing (Just 1))
            , DeleteRow "EF" "health"
            , WriteFormula "EF" "health" "2 * climate_change"
            , PutScore "EF" "Twice" "2 * health"
            , DeleteScore "EF" "Single score"
            ]

    it "undoes a change to a translated set back to the very set" $
        mapM_
            (\edit -> back translated edit `shouldBe` Right translated)
            [ AddRow "EF 3.1" (draft "Water" [(3, 1)] (Just 1) (Just 1))
            , ChangeRow "EF 3.1" "water_use" (draft "Water" [(3, 1), (1, 1)] Nothing (Just 0.08))
            , DeleteRow "EF 3.1" "ecotoxicity"
            ]

    it "undoes an added row after a category it reads was renamed" $ do
        let oneRow = fromRight collection (leaving collection (NewSet "EF" Nothing [draft "Health" [(1, 1)] Nothing (Just 1)]))
            undone = do
                (added, _) <- planScoringEdit oneRow (AddRow "EF" (draft "Water" [(3, 2)] Nothing (Just 1)))
                withRow <- applyMethodOp oneRow added
                renamed <- applyMethodOp withRow (RenameCategory (uuid 3) "Water use" "Water consumption")
                inverseOf renamed added >>= \case
                    UndoWith inverse -> applyMethodOp renamed inverse
                    UndoSelector _ -> Left "no selector here"
        fmap mcScoringSets undone `shouldBe` fmap mcScoringSets (applyMethodOp oneRow (RenameCategory (uuid 3) "Water use" "Water consumption"))

    it "names the later line that changed an entry an undo would give back, and only such a line" $ do
        let lineOf edit start = either (const (MethodLine (SetGlobalMethods [] []) Change)) (\(op, _) -> MethodLine op Change) (planScoringEdit start edit)
            first = lineOf (AddRow "EF" (draft "Water" [(3, 2)] (Just 1) (Just 1))) twoRows
            withWater = fromRight twoRows (applyMethodOp twoRows (mlOp first))
            reweighed = lineOf (ChangeRow "EF" "water" (draft "Water" [(3, 2)] (Just 1) (Just 3))) withWater
            relabelled = lineOf (ChangeRow "EF" "health" (draft "Human health" [(1, 1), (2, 1)] (Just 2) (Just 0.5))) withWater
        T.unpack (blockedUndo withWater [first, reweighed] 1 "replay failed") `shouldContain` "undo line 2 first"
        blockedUndo withWater [first, relabelled] 1 "replay failed" `shouldBe` "replay failed"

    it "refuses to undo an added row once a later row changed the score it joined" $ do
        let undone = do
                (water, _) <- planScoringEdit twoRows (AddRow "EF" (draft "Water" [(3, 2)] (Just 1) (Just 1)))
                withWater <- applyMethodOp twoRows water
                (air, _) <- planScoringEdit withWater (AddRow "EF" (draft "Air" [(2, 3)] (Just 1) (Just 1)))
                withAir <- applyMethodOp withWater air
                inverseOf withAir water >>= \case
                    UndoWith inverse -> applyMethodOp withAir inverse
                    UndoSelector _ -> Left "no selector here"
        refusal undone `shouldContain` "Single score"

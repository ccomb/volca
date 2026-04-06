{-# LANGUAGE OverloadedStrings #-}

module EcoSpold1Spec (spec) where

import Test.Hspec
import Data.UUID (nil)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Types
import EcoSpold.Parser1

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A minimal Activity with no exchanges
emptyActivity :: Activity
emptyActivity = Activity
    { activityName         = "Test Activity"
    , activityDescription  = []
    , activitySynonyms     = M.empty
    , activityClassification = M.empty
    , activityLocation     = "GLO"
    , activityUnit         = "kg"
    , exchanges            = []
    , activityParams       = M.empty
    , activityParamExprs   = M.empty
    }

-- | A production output exchange (isInput=False, isReference=False by default)
mkOutput :: UUID -> Double -> Exchange
mkOutput fid amt = TechnosphereExchange fid amt nil False False nil Nothing ""

-- | A reference output exchange
mkRefOutput :: UUID -> Double -> Exchange
mkRefOutput fid amt = TechnosphereExchange fid amt nil False True nil Nothing ""

-- | A technosphere input exchange
mkInput :: UUID -> Double -> Exchange
mkInput fid amt = TechnosphereExchange fid amt nil True False nil Nothing ""

-- | A biosphere exchange
mkBio :: UUID -> Double -> Exchange
mkBio fid amt = BiosphereExchange fid amt nil False ""

fid1, fid2, fid3 :: UUID
fid1 = read "11111111-1111-1111-1111-111111111111"
fid2 = read "22222222-2222-2222-2222-222222222222"
fid3 = read "33333333-3333-3333-3333-333333333333"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

    describe "generateFlowUUID" $ do
        it "is deterministic for the same inputs" $
            generateFlowUUID 42 1 "CO2" "air"
                `shouldBe` generateFlowUUID 42 1 "CO2" "air"

        it "differs when dataset number changes" $
            generateFlowUUID 1 1 "CO2" "air"
                `shouldNotBe` generateFlowUUID 2 1 "CO2" "air"

        it "differs when exchange number changes" $
            generateFlowUUID 1 1 "CO2" "air"
                `shouldNotBe` generateFlowUUID 1 2 "CO2" "air"

        it "differs when flow name changes" $
            generateFlowUUID 1 1 "CO2" "air"
                `shouldNotBe` generateFlowUUID 1 1 "methane" "air"

    describe "generateUnitUUID" $ do
        it "is deterministic for the same unit name" $
            generateUnitUUID "kg" `shouldBe` generateUnitUUID "kg"

        it "differs for different unit names" $
            generateUnitUUID "kg" `shouldNotBe` generateUnitUUID "m3"

    describe "isProductionExchange" $ do
        it "is True for a TechnosphereExchange output (isInput=False)" $
            isProductionExchange (mkOutput fid1 1.0) `shouldBe` True

        it "is False for a TechnosphereExchange input (isInput=True)" $
            isProductionExchange (mkInput fid1 1.0) `shouldBe` False

        it "is False for a BiosphereExchange" $
            isProductionExchange (mkBio fid1 1.0) `shouldBe` False

    describe "hasReferenceProduct" $ do
        it "is False for an activity with no exchanges" $
            hasReferenceProduct emptyActivity `shouldBe` False

        it "is False when only non-reference outputs exist" $ do
            let act = emptyActivity { exchanges = [mkOutput fid1 1.0] }
            hasReferenceProduct act `shouldBe` False

        it "is True when at least one reference exchange exists" $ do
            let act = emptyActivity { exchanges = [mkRefOutput fid1 1.0] }
            hasReferenceProduct act `shouldBe` True

    describe "removeZeroAmountCoproducts" $ do
        it "keeps non-zero production outputs" $
            length (removeZeroAmountCoproducts [mkOutput fid1 1.0]) `shouldBe` 1

        it "removes zero-amount non-reference production outputs" $
            length (removeZeroAmountCoproducts [mkOutput fid1 0.0]) `shouldBe` 0

        it "keeps reference outputs even at zero amount" $ do
            let result = removeZeroAmountCoproducts [mkRefOutput fid1 0.0]
            length result `shouldBe` 1

        it "keeps inputs regardless of amount" $
            length (removeZeroAmountCoproducts [mkInput fid1 0.0]) `shouldBe` 1

        it "keeps biosphere exchanges regardless of amount" $
            length (removeZeroAmountCoproducts [mkBio fid1 0.0]) `shouldBe` 1

    describe "assignSingleProductAsReference" $ do
        it "marks the only non-zero production output as reference" $ do
            let act = emptyActivity { exchanges = [mkOutput fid1 1.0, mkBio fid2 0.5] }
                result = assignSingleProductAsReference act
            length (filter exchangeIsReference (exchanges result)) `shouldBe` 1

        it "does not assign when multiple non-zero outputs exist" $ do
            let act = emptyActivity { exchanges = [mkOutput fid1 1.0, mkOutput fid2 2.0] }
                result = assignSingleProductAsReference act
            length (filter exchangeIsReference (exchanges result)) `shouldBe` 0

        it "does not assign when no non-zero outputs exist" $ do
            let act = emptyActivity { exchanges = [mkOutput fid1 0.0] }
                result = assignSingleProductAsReference act
            length (filter exchangeIsReference (exchanges result)) `shouldBe` 0

    describe "applyCutoffStrategy" $ do
        it "returns Right when a reference product exists" $ do
            let act = emptyActivity { exchanges = [mkRefOutput fid1 1.0] }
            case applyCutoffStrategy act of
                Right _  -> return ()
                Left err -> expectationFailure $ "Expected Right: " ++ err

        it "assigns reference product when exactly one non-zero output" $ do
            let act = emptyActivity { exchanges = [mkOutput fid1 1.0] }
            case applyCutoffStrategy act of
                Right result -> length (filter exchangeIsReference (exchanges result)) `shouldBe` 1
                Left err     -> expectationFailure $ "Expected Right: " ++ err

        it "removes zero-amount coproducts before strategy" $ do
            let act = emptyActivity { exchanges = [mkRefOutput fid1 1.0, mkOutput fid2 0.0] }
            case applyCutoffStrategy act of
                Right result -> length (exchanges result) `shouldBe` 1
                Left err     -> expectationFailure $ "Expected Right: " ++ err

        it "returns Left when no reference product can be assigned" $ do
            let act = emptyActivity { exchanges = [] }
            case applyCutoffStrategy act of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left for activity with no exchanges"

        it "returns Left when multiple non-zero outputs and no reference" $ do
            let act = emptyActivity { exchanges = [mkOutput fid1 1.0, mkOutput fid2 2.0] }
            case applyCutoffStrategy act of
                Left _  -> return ()
                Right _ -> expectationFailure "Expected Left for ambiguous coproducts"

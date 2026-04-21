{-# LANGUAGE OverloadedStrings #-}

module FuzzySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import Search.BM25 (buildIndex)
import Search.BM25.Types (BM25Index)
import Search.Fuzzy (expandTokens)
import Types

mkActivity :: Text -> [Exchange] -> Activity
mkActivity name xs =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityUnit = "kg"
        , exchanges = xs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        }

-- Index-builder helper: create a BM25 index over a list of activity names.
indexOfNames :: [Text] -> BM25Index
indexOfNames names =
    buildIndex (V.fromList [mkActivity n [] | n <- names]) M.empty

spec :: Spec
spec = describe "Search.Fuzzy" $ do
    it "passes exact-vocabulary token through at weight 1.0" $ do
        let idx = indexOfNames ["yaourt nature"]
        expandTokens idx ["yaourt"] `shouldBe` [("yaourt", 1.0)]

    it "expands a typo to the nearest vocabulary token at weight 0.5" $ do
        let idx = indexOfNames ["yaourt nature", "lait demi"]
        expandTokens idx ["yaourtt"] `shouldBe` [("yaourt", 0.5)]

    it "expands another typo (viandee → viande)" $ do
        let idx = indexOfNames ["viande de porc", "electricity grid"]
        expandTokens idx ["viandee"] `shouldBe` [("viande", 0.5)]

    it "expands a stem via prefix coverage at weight 0.7" $ do
        let idx = indexOfNames ["electricity grid"]
        expandTokens idx ["electr"] `shouldBe` [("electricity", 0.7)]

    it "expands a longer stem (electri → electricity) at 0.7" $ do
        let idx = indexOfNames ["electricity grid"]
        expandTokens idx ["electri"] `shouldBe` [("electricity", 0.7)]

    it "prefers edit-distance over prefix when both could match" $ do
        -- "electricit" is 1 edit from "electricity" — edit-distance wins.
        let idx = indexOfNames ["electricity grid"]
        expandTokens idx ["electricit"] `shouldBe` [("electricity", 0.5)]

    it "drops unmatchable tokens" $ do
        let idx = indexOfNames ["yaourt nature", "lait demi"]
        expandTokens idx ["xyzqwert"] `shouldBe` []

    it "does not expand tokens shorter than 4 chars via prefix path" $ do
        -- "ele" has only one trigram, and len < 4; edit distance alone can't
        -- reach "electricity" from "ele". Must drop.
        let idx = indexOfNames ["electricity"]
        expandTokens idx ["ele"] `shouldBe` []

    it "applies length ratio guard on prefix matches" $ do
        -- "elect" (5 chars) should not match a 25-char token
        -- (ratio > 3 — 25/5 = 5). Only the shorter candidate passes.
        let idx =
                indexOfNames
                    [ "electroencephalography used in diagnosis"
                    , "election local"
                    ]
        -- "elect" (len 5): max candidate length = 15.
        -- "electroencephalography" (len 22) is blocked by ratio guard.
        -- "election" (len 8) passes.
        expandTokens idx ["elect"] `shouldBe` [("election", 0.7)]

    it "mixed query: exact + typo carry correct weights" $ do
        let idx = indexOfNames ["yaourt nature", "viande de porc"]
        expandTokens idx ["viande", "yaourtt"]
            `shouldBe` [("viande", 1.0), ("yaourt", 0.5)]

    it "returns exact + close variants when both are in vocabulary (trellis/treillis)" $ do
        -- Real Agribalyse case: two activities use the English and French
        -- spellings. A search for either must surface both; exact ranks first
        -- via the 1.0 vs 0.5 weight gap.
        let idx =
                indexOfNames
                    [ "trellis system wooden poles"
                    , "treillis system wooden poles"
                    ]
        expandTokens idx ["trellis"] `shouldBe` [("trellis", 1.0), ("treillis", 0.5)]
        expandTokens idx ["treillis"] `shouldBe` [("treillis", 1.0), ("trellis", 0.5)]

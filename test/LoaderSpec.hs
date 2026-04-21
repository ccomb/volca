{-# LANGUAGE OverloadedStrings #-}

module LoaderSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import Database.Loader
import TestHelpers (loadSampleDatabase)
import Types

-- ---------------------------------------------------------------------------
-- Minimal fixtures
-- ---------------------------------------------------------------------------

flowUUID1, flowUUID2, actUUID1, actUUID2 :: UUID.UUID
flowUUID1 = read "aaaaaaaa-0000-0000-0000-000000000001"
flowUUID2 = read "bbbbbbbb-0000-0000-0000-000000000002"
actUUID1 = read "cccccccc-0000-0000-0000-000000000001"
actUUID2 = read "cccccccc-0000-0000-0000-000000000002"

minimalFlow :: UUID.UUID -> Text -> Flow
minimalFlow fid name =
    Flow
        { flowId = fid
        , flowName = name
        , flowCategory = ""
        , flowSubcompartment = Nothing
        , flowUnitId = UUID.nil
        , flowType = Technosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }

minimalActivity :: Text -> Text -> [Exchange] -> Activity
minimalActivity name loc exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        }

refExchange :: UUID.UUID -> Exchange
refExchange fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techIsInput = False
        , techIsReference = True
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        }

inputExchange :: UUID.UUID -> Text -> Exchange
inputExchange fid loc =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 0.5
        , techUnitId = UUID.nil
        , techIsInput = True
        , techIsReference = False
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = loc
        }

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- normalizeText
    -- -----------------------------------------------------------------------
    describe "normalizeText" $ do
        it "lowercases text" $
            normalizeText "WHEAT Production" `shouldBe` "wheat production"

        it "strips leading and trailing whitespace" $
            normalizeText "  wheat  " `shouldBe` "wheat"

        it "handles empty text" $
            normalizeText "" `shouldBe` ""

    -- -----------------------------------------------------------------------
    -- mergeFlows
    -- -----------------------------------------------------------------------
    describe "mergeFlows" $ do
        it "unions synonyms from both flows" $ do
            let a = (minimalFlow flowUUID1 "CO2"){flowSynonyms = M.singleton "en" (S.fromList ["carbon dioxide"])}
                b = (minimalFlow flowUUID1 "CO2"){flowSynonyms = M.singleton "en" (S.fromList ["CO2"])}
                merged = mergeFlows a b
            M.lookup "en" (flowSynonyms merged) `shouldBe` Just (S.fromList ["carbon dioxide", "CO2"])

        it "keeps all other fields from the first flow" $ do
            let a = minimalFlow flowUUID1 "flow-a"
                b = minimalFlow flowUUID2 "flow-b"
            flowName (mergeFlows a b) `shouldBe` "flow-a"

    -- -----------------------------------------------------------------------
    -- generateActivityUUIDFromActivity
    -- -----------------------------------------------------------------------
    describe "generateActivityUUIDFromActivity" $ do
        it "is deterministic for the same activity" $ do
            let act = minimalActivity "wheat production" "GLO" []
            generateActivityUUIDFromActivity act
                `shouldBe` generateActivityUUIDFromActivity act

        it "differs for different name" $ do
            let a = minimalActivity "wheat production" "GLO" []
                b = minimalActivity "barley production" "GLO" []
            generateActivityUUIDFromActivity a
                `shouldNotBe` generateActivityUUIDFromActivity b

        it "differs for different location" $ do
            let a = minimalActivity "wheat production" "GLO" []
                b = minimalActivity "wheat production" "FR" []
            generateActivityUUIDFromActivity a
                `shouldNotBe` generateActivityUUIDFromActivity b

    -- -----------------------------------------------------------------------
    -- getReferenceProductUUID
    -- -----------------------------------------------------------------------
    describe "getReferenceProductUUID" $ do
        it "returns the flow UUID of the reference exchange" $ do
            let act = minimalActivity "prod" "GLO" [refExchange flowUUID1]
            getReferenceProductUUID act `shouldBe` flowUUID1

        it "returns UUID.nil when there is no reference exchange" $ do
            let act = minimalActivity "prod" "GLO" []
            getReferenceProductUUID act `shouldBe` UUID.nil

    -- -----------------------------------------------------------------------
    -- mergeUnlinkedSummaries
    -- -----------------------------------------------------------------------
    describe "mergeUnlinkedSummaries" $ do
        it "sums all counters" $ do
            let s1 = UnlinkedSummary M.empty 10 8 2
                s2 = UnlinkedSummary M.empty 5 3 2
                m = mergeUnlinkedSummaries s1 s2
            usTotalLinks m `shouldBe` 15
            usFoundLinks m `shouldBe` 11
            usMissingLinks m `shouldBe` 4

        it "unions activity maps" $ do
            let s1 = UnlinkedSummary (M.singleton "actA" []) 1 0 1
                s2 = UnlinkedSummary (M.singleton "actB" []) 1 0 1
                m = mergeUnlinkedSummaries s1 s2
            M.size (usActivities m) `shouldBe` 2

        it "emptyUnlinkedSummary is the identity" $ do
            let s = UnlinkedSummary M.empty 3 2 1
                m = mergeUnlinkedSummaries s emptyUnlinkedSummary
            usTotalLinks m `shouldBe` 3
            usFoundLinks m `shouldBe` 2
            usMissingLinks m `shouldBe` 1

    -- -----------------------------------------------------------------------
    -- buildSupplierIndex (name+location keyed, EcoSpold1 style)
    -- -----------------------------------------------------------------------
    describe "buildSupplierIndex" $ do
        it "indexes reference exchanges by (normalizedName, location)" $ do
            let act =
                    minimalActivity
                        "wheat production"
                        "GLO"
                        [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndex acts flows
            M.lookup ("wheat", "GLO") idx `shouldBe` Just (actUUID1, flowUUID1)

        it "does not index input (non-reference) exchanges" $ do
            let act =
                    minimalActivity
                        "consumer"
                        "DE"
                        [inputExchange flowUUID1 "GLO"]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndex acts flows
            M.null idx `shouldBe` True

    -- -----------------------------------------------------------------------
    -- buildSupplierIndexByName (name-only keyed, SimaPro style)
    -- -----------------------------------------------------------------------
    describe "buildSupplierIndexByName" $ do
        it "indexes reference exchanges by normalized flow name" $ do
            let act =
                    minimalActivity
                        "wheat production"
                        "GLO"
                        [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat Production")]
                idx = buildSupplierIndexByName acts flows
            M.lookup "wheat production" idx `shouldBe` Just (actUUID1, flowUUID1)

        it "does not index non-reference exchanges" $ do
            let act =
                    minimalActivity
                        "consumer"
                        "DE"
                        [inputExchange flowUUID1 "GLO"]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndexByName acts flows
            M.null idx `shouldBe` True

    -- -----------------------------------------------------------------------
    -- fixExchangeLinkByName (SimaPro-style name-only linking)
    -- -----------------------------------------------------------------------
    describe "fixExchangeLinkByName" $ do
        it "resolves input exchange when supplier in index" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.fromList [("wheat", (actUUID1, flowUUID2))]
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` actUUID1
            usFoundLinks summary `shouldBe` 1
            usMissingLinks summary `shouldBe` 0

        it "leaves exchange unlinked when supplier not in index" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.empty
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` UUID.nil
            usMissingLinks summary `shouldBe` 1

        it "leaves exchange unlinked when flow not in flowDB" $ do
            let flows = M.empty
                idx = M.empty
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` UUID.nil
            usMissingLinks summary `shouldBe` 1

        it "does not touch output reference exchanges" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.fromList [("wheat", (actUUID1, flowUUID2))]
                ex = refExchange flowUUID1
                (fixed, summary) = fixExchangeLinkByName idx flows "producer" ex
            techActivityLinkId fixed `shouldBe` UUID.nil -- unchanged
            usTotalLinks summary `shouldBe` 0 -- not counted
        it "does not touch biosphere exchanges" $ do
            let flows = M.empty
                idx = M.empty
                bioEx = BiosphereExchange flowUUID1 1.0 UUID.nil False ""
                (fixed, summary) = fixExchangeLinkByName idx flows "act" bioEx
            -- BiosphereExchange is returned unchanged: verify it is still biosphere
            isBiosphereExchange fixed `shouldBe` True
            usTotalLinks summary `shouldBe` 0

    -- -----------------------------------------------------------------------
    -- countTotalTechInputs / countUnlinkedExchanges / collectUnlinkedProductNames
    -- (integration tests via SAMPLE.min3)
    -- -----------------------------------------------------------------------
    describe "countTotalTechInputs" $ do
        it "counts all technosphere inputs in SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            countTotalTechInputs sdb `shouldSatisfy` (> 0)

    describe "countUnlinkedExchanges" $ do
        it "returns 0 for fully linked SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            countUnlinkedExchanges sdb `shouldBe` 0

    describe "collectUnlinkedProductNames" $ do
        it "returns empty map for fully linked SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            M.null (collectUnlinkedProductNames sdb) `shouldBe` True

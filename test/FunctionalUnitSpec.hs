{-# LANGUAGE OverloadedStrings #-}

{- | What every computed answer says its numbers are for.

Product D of the sample database makes one kilogram of "product D": every
score, contribution, graph and sensitivity read on it is for that, whichever
route or tool asks. Each answer states it under its own key, the same object
everywhere.
-}
module FunctionalUnitSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import API.Routes (
    getActivityGraph,
    getActivityLCIA,
    getActivityLCIABatch,
    getContributingActivities,
    getContributingFlows,
    getScoreContributingActivities,
    getScoreContributingFlows,
    postActivitySensitivity,
    postImpactsBatch,
 )
import API.Types (
    BatchImpactsEntry (..),
    BatchImpactsRequest (..),
    BatchImpactsResponse (..),
    ContributingActivitiesResult (..),
    ContributingFlowsResult (..),
    FunctionalUnit (..),
    GraphExport (..),
    LCIABatchResult (..),
    LCIAResult (..),
    SensitivityRequest (..),
    SensitivityResponse (..),
 )
import Database.Manager (CollectionName (..))
import DependencyLicenceSpec (climate, collection, dependency, managerOn, root, rootPid, runIn)
import FlowContributionSpec (collectionName, mcpPayload, methodIdText, productD, runOk)
import Method.Types (Method (..))
import Types (Licence (..))

productDUnit :: FunctionalUnit
productDUnit = FunctionalUnit "kg" "product D"

spec :: Spec
spec = describe "the functional unit of a computed answer" $ do
    it "is one kilogram of product D on every REST answer" $ do
        let collection = CollectionName collectionName
        single <- runOk (getActivityLCIA "sample" productD collection methodIdText Nothing)
        panel <- runOk (getActivityLCIABatch "sample" productD collection Nothing Nothing)
        batch <- runOk (postImpactsBatch "sample" collection Nothing Nothing (BatchImpactsRequest [productD] []))
        flows <- runOk (getContributingFlows "sample" productD collection methodIdText Nothing Nothing Nothing)
        activities <- runOk (getContributingActivities "sample" productD collection methodIdText Nothing Nothing Nothing)
        scoreFlows <- runOk (getScoreContributingFlows "sample" productD collection "Single" "total" Nothing Nothing)
        scoreActivities <- runOk (getScoreContributingActivities "sample" productD collection "Single" "total" Nothing Nothing)
        graph <- runOk (getActivityGraph "sample" productD Nothing)
        sensitivity <- runOk (postActivitySensitivity "sample" productD collection methodIdText (SensitivityRequest []))
        ( [lrFunctionalUnit single, lbrFunctionalUnit panel]
                ++ map lrFunctionalUnit (lbrResults panel)
                ++ map (lbrFunctionalUnit . bieImpacts) (birResults batch)
                ++ [ cfrFunctionalUnit flows
                   , carFunctionalUnit activities
                   , cfrFunctionalUnit scoreFlows
                   , carFunctionalUnit scoreActivities
                   , geFunctionalUnit graph
                   , srFunctionalUnit sensitivity
                   ]
            )
            `shouldSatisfy` all (== productDUnit)

    it "is the activity's own name and unit for a process whose reference names no product" $ do
        -- The fixture's root makes a product its tech-flow table does not hold.
        manager <- managerOn root dependency LicenceUnstated
        let collection' = CollectionName collection
        alone <- runIn manager (getActivityLCIA "root" rootPid collection' (UUID.toText (methodId climate)) Nothing)
        batch <- runIn manager (postImpactsBatch "root" collection' Nothing Nothing (BatchImpactsRequest [rootPid] []))
        (fmap lrFunctionalUnit alone, fmap (map (lbrFunctionalUnit . bieImpacts) . birResults) batch)
            `shouldBe` (Right (FunctionalUnit "kg" "act-FR"), Right [FunctionalUnit "kg" "act-FR"])

    it "is the object amount, unit, product under each MCP tool's own key" $ do
        let expectedJson = object ["amount" .= (1 :: Int), "unit" .= ("kg" :: Text), "product" .= ("product D" :: Text)]
            keyedBy :: Key -> Text -> [(Key, Value)] -> Expectation
            keyedBy key tool args = do
                o <- mcpPayload Nothing tool args
                KM.lookup key o `shouldBe` Just expectedJson
            theScore = [("scoring_set", String "Single"), ("score", String "total")]
        toJSON productDUnit `shouldBe` expectedJson
        keyedBy "functional_unit" "get_impacts" []
        keyedBy "functional_unit" "get_contributing_flows" []
        keyedBy "functional_unit" "get_contributing_activities" []
        keyedBy "functional_unit" "get_score_contributing_flows" theScore
        keyedBy "functional_unit" "get_score_contributing_activities" theScore
        keyedBy "functional_unit" "compute_sensitivity" [("perturbations", Array V.empty)]
        keyedBy "functionalUnit" "score_activity" []
        keyedBy "functional_unit" "score_activities" [("process_ids", Array (V.singleton (String productD)))]
        keyedBy "functionalUnit" "get_supply_chain" []
        keyedBy "functionalUnit" "aggregate" [("scope", String "direct")]
        -- Product D buys nothing, so the path starts from product C, which buys it.
        pathTo <- mcpPayload Nothing "get_path_to" [("process_id", String "cc000003-0000-0000-0000-000000000000"), ("target", String "product D")]
        KM.lookup "functionalUnit" pathTo
            `shouldBe` Just (object ["amount" .= (1 :: Int), "unit" .= ("kg" :: Text), "product" .= ("product C" :: Text)])

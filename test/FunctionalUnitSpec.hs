{-# LANGUAGE OverloadedStrings #-}

{- | What every computed answer says its numbers are for.

Product D of the sample database makes one kilogram of "product D": every
score, contribution, graph and sensitivity read on it is for that, whichever
route or tool asks. Each answer states it under its own key, the same object
everywhere.
-}
module FunctionalUnitSpec (spec) where

import Control.Monad (void)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
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
import Types (Database (..), Licence (..))

productDUnit :: FunctionalUnit
productDUnit = FunctionalUnit "kg" "product D"

spec :: Spec
spec = describe "the functional unit of a computed answer" $ do
    it "is one kilogram of product D on every REST answer" $ do
        let collection = CollectionName collectionName
        single <- runOk (getActivityLCIA "sample" productD collection methodIdText Nothing)
        panel <- runOk (getActivityLCIABatch "sample" productD collection Nothing Nothing)
        batch <- runOk (postImpactsBatch "sample" collection Nothing Nothing (BatchImpactsRequest [productD] []))
        flows <- runOk (getContributingFlows "sample" productD collection methodIdText Nothing Nothing)
        activities <- runOk (getContributingActivities "sample" productD collection methodIdText Nothing Nothing)
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

    it "refuses a process whose reference names no product, in a batch and alone" $ do
        manager <- managerOn root{dbTechFlows = M.empty} dependency LicenceUnstated
        batch <- runIn manager (postImpactsBatch "root" (CollectionName collection) Nothing Nothing (BatchImpactsRequest [rootPid] []))
        fmap (\b -> (birUnscorable b, length (birResults b))) batch `shouldBe` Right ([rootPid], 0)
        alone <- runIn manager (getActivityLCIA "root" rootPid (CollectionName collection) (UUID.toText (methodId climate)) Nothing)
        void alone `shouldBe` Left 422

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

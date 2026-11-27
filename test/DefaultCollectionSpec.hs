{-# LANGUAGE OverloadedStrings #-}

{- | A scoring tool called without a method collection, while several are
loaded: it scores with the instance's default and its @basis@ names it, so an
assistant need not choose. A tool whose answer names no collection still
refuses, listing them.
-}
module DefaultCollectionSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import API.MCP (callTool, noRequestId)
import Config (MethodConfig (..), MethodOrigin (..))
import Database.Manager (DatabaseManager (..))
import FlowContributionSpec (collectionName, managerUnder, methodIdText, payloadOf, productD)
import Types (Licence (..))
import Usage (anonymous)

-- | The test collection, marked as the instance's default.
markedDefault :: MethodConfig
markedDefault =
    MethodConfig
        { mcName = collectionName
        , mcOrigin = MethodFromFile "unused.csv"
        , mcActive = True
        , mcDefault = True
        , mcHome = Nothing
        , mcAdded = Nothing
        , mcSource = Nothing
        , mcDescription = Nothing
        , mcFormat = Nothing
        , mcScoringSets = []
        , mcGlobalMethods = []
        , mcPatches = []
        , mcPackageMethod = Nothing
        }

-- | Call a tool on product D without a collection; its payload, or the reply shown.
callUnnamed :: DatabaseManager -> Text -> IO (Either String Object)
callUnnamed manager tool = do
    reply <-
        callTool anonymous manager [] Nothing Nothing noRequestId tool $
            KM.fromList [("database", String "sample"), ("process_id", String productD), ("method_id", String methodIdText)]
    pure (maybe (Left (show reply)) Right (payloadOf reply))

-- | The collection a payload's basis names.
basisCollection :: Object -> Maybe Value
basisCollection o = do
    Object basis <- KM.lookup "basis" o
    Object methods <- KM.lookup "methods" basis
    KM.lookup "collection" methods

-- | The collection the tool's basis names, failing with the reply when it has no payload.
scoredWith :: DatabaseManager -> Text -> IO (Maybe Value)
scoredWith manager tool = callUnnamed manager tool >>= either fail (pure . basisCollection)

spec :: Spec
spec = describe "a scoring tool called without a method collection" $ do
    it "refuses while several are loaded and none is the default" $ do
        manager <- managerUnder LicenceUnstated
        loaded <- M.keys <$> readTVarIO (dmLoadedMethods manager)
        length loaded `shouldSatisfy` (> 1)
        refusal <- callUnnamed manager "score_activity"
        either (\reply -> T.pack reply `shouldSatisfy` T.isInfixOf "Several method collections") (const (expectationFailure "expected a refusal")) refusal

    it "scores with the default one, which basis names" $ do
        manager <- managerUnder LicenceUnstated
        atomically $ modifyTVar' (dmAvailableMethods manager) (M.insert collectionName markedDefault)
        let named = Just (String collectionName)
        -- The test category is in one collection only: score_activity is the one that must choose.
        scoredWith manager "score_activity" `shouldReturn` named
        scoredWith manager "get_impacts" `shouldReturn` named

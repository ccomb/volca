{-# LANGUAGE OverloadedStrings #-}

{- | What a database's licence keeps to itself, on every surface that answers.

The sample database is served under two own licences: one keeping what weighs
in its scores (and so its inventory and its files), one keeping its inventory
alone. Product D emits 2 kg of fossil carbon dioxide, which the method weighs
at 27: the score of 54 stays readable under both, its contributors under the
second only, its exchange amounts under neither.
-}
module LicenceEnforcementSpec (spec) where

import Data.Aeson (Object, Value (..), toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Servant (errHTTPCode, runHandler)
import Test.Hspec

import API.MCP (callTool, noRequestId)
import API.Routes (getActivityInventory, getActivityTree)
import App.Env (AppEnv (..), AppM, runApp)
import Config (DatabaseConfig (..))
import Database.Manager (DatabaseManager, addDatabase)
import FlowContributionSpec (collectionName, managerUnder, methodIdText, payloadOf, productD, sampleConfig)
import Types (Attribution (..), Licence (..), OwnLicence (..), Permission (..))

own :: [Permission] -> Licence
own refused = LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.fromList refused, ownAttribution = AttributionRequired}

-- | Keeps what weighs in its scores, and the two levels that would rebuild it.
detailKept :: Licence
detailKept = own [SeeDetailedScores, ReadInventory, Download]

-- | Keeps its inventory and its files; its contributions stay readable.
inventoryKept :: Licence
inventoryKept = own [ReadInventory, Download]

-- | Call an MCP tool on product D of the sample database.
callOn :: DatabaseManager -> Text -> [(Key, Value)] -> IO Value
callOn manager tool extraArgs =
    callTool manager [] Nothing Nothing noRequestId tool $
        KM.fromList $
            [ ("database", String "sample")
            , ("process_id", String productD)
            , ("method_id", String methodIdText)
            , ("collection", String collectionName)
            ]
                ++ extraArgs

-- | The text of a tool error, or nothing when the tool answered.
errorText :: Value -> Maybe Text
errorText reply = do
    Object o <- Just reply
    Object r <- KM.lookup "result" o
    Bool True <- KM.lookup "isError" r
    Array content <- KM.lookup "content" r
    Object c <- listToMaybe (toList content)
    String text <- KM.lookup "text" c
    pure text

payload :: Value -> IO Object
payload reply = maybe (fail ("unexpected reply: " <> show reply)) pure (payloadOf reply)

-- | The HTTP status a REST handler answers with, against the manager given.
statusOf :: DatabaseManager -> AppM a -> IO (Maybe Int)
statusOf manager handler =
    either (Just . errHTTPCode) (const Nothing) <$> runHandler (runApp env handler)
  where
    env :: AppEnv
    env =
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            }

spec :: Spec
spec = do
    describe "MCP, under a licence keeping what weighs in its scores" $ do
        it "refuses the contributing flows, naming the licence" $ do
            manager <- managerUnder detailKept
            reply <- callOn manager "get_contributing_flows" []
            errorText reply `shouldBe` Just "The licence of sample keeps to itself what weighs in its scores."

        it "answers the score, with no flow and the reason" $ do
            manager <- managerUnder detailKept
            o <- payload =<< callOn manager "get_impacts" [("top_flows", Number 5)]
            (KM.lookup "score" o, KM.lookup "top_flows" o) `shouldBe` (Just (Number 54), Just (Array mempty))
            KM.lookup "withheld" o
                `shouldBe` Just (toJSON (["The licence of sample keeps to itself what weighs in its scores.", "The licence of sample keeps the amounts of its exchanges to itself."] :: [Text]))

        it "refuses a comparison whose second database keeps it" $ do
            manager <- managerUnder LicenceUnstated
            addDatabase manager sampleConfig{dcName = "kept", dcLicence = detailKept}
            reply <- callOn manager "compare_impacts" [("database_a", String "sample"), ("database_b", String "kept")]
            errorText reply `shouldBe` Just "The licence of kept keeps to itself what weighs in its scores."

    describe "MCP, under a licence keeping its inventory" $ do
        it "refuses the supply chain" $ do
            manager <- managerUnder inventoryKept
            reply <- callOn manager "get_supply_chain" []
            errorText reply `shouldBe` Just "The licence of sample keeps the amounts of its exchanges to itself."

        it "answers the contributing flows" $ do
            manager <- managerUnder inventoryKept
            reply <- callOn manager "get_contributing_flows" []
            errorText reply `shouldBe` Nothing

        it "names an activity's exchanges without their amounts" $ do
            manager <- managerUnder inventoryKept
            o <- payload =<< callOn manager "get_activity" []
            Just (Object activity) <- pure (KM.lookup "activity" o)
            KM.lookup "exchanges" activity `shouldBe` Just (Array mempty)
            Just (Object withheld) <- pure (KM.lookup "withheld" activity)
            KM.lookup "reason" withheld `shouldBe` Just (String "The licence of sample keeps the amounts of its exchanges to itself.")
            Just (Array lines') <- pure (KM.lookup "lines" withheld)
            lines' `shouldNotBe` mempty
            -- A name says what the process is made of; an amount would be the recipe.
            T.pack (show lines') `shouldSatisfy` (not . ("amount" `T.isInfixOf`))

    describe "REST, under a licence keeping its inventory" $ do
        it "refuses the inventory and the tree" $ do
            manager <- managerUnder inventoryKept
            statusOf manager (getActivityInventory "sample" productD) `shouldReturn` Just 403
            statusOf manager (getActivityTree "sample" productD) `shouldReturn` Just 403

        it "refuses them on a copy too, which is served under its source's licence" $ do
            manager <- managerUnder inventoryKept
            addDatabase manager sampleConfig{dcName = "copy", dcSource = Just "sample"}
            statusOf manager (getActivityTree "copy" productD) `shouldReturn` Just 403

        it "answers them under a licence that keeps nothing" $ do
            manager <- managerUnder LicenceUnstated
            statusOf manager (getActivityTree "sample" productD) `shouldReturn` Nothing

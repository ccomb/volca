{-# LANGUAGE OverloadedStrings #-}

{- | The hosting plan's database quotas, and what they count.

Two separate budgets: how many databases of their own a user may keep
('uploadRefusal'), and how many of those may sit in memory at once
('memoryRefusal'). Both count only the user's own databases, the ones a tier
preloads are what an uploaded inventory links against, so counting them would
make the quota forbid the very thing uploading is for. 'loadRefusal' and
'copyRefusal' are the exact policies the handlers apply, so a budget wired to
the wrong limit or message field fails here.
-}
module UploadQuotaSpec (spec) where

import API.DatabaseHandlers (copyRefusal, loadRefusal, memoryRefusal, uploadRefusal)
import API.MCP (callTool)
import Config (DatabaseConfig (..), HostingConfig (..), defaultConfig)
import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Manager (CachePolicy (..), DatabaseManager (..), initDatabaseManager)
import Test.Hspec
import Types (AllocationKey (..), GeographyPolicy (..))

-- | A plan allowing @stored@ databases and @loaded@ of them in memory.
plan :: Int -> Int -> HostingConfig
plan stored loaded =
    HostingConfig
        { hcMaxUploads = stored
        , hcMaxUploadMb = 100
        , hcMaxLoadedUploads = loaded
        , hcApiAccess = True
        , hcReadOnly = False
        , hcReadOnlyMessage = ""
        , hcUpgradeUpload = ""
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        }

-- | An entry in the manager's registry for a database the user uploaded.
uploadedEntry :: Text -> DatabaseConfig
uploadedEntry name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = "/nonexistent/" <> T.unpack name
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = True
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        }

spec :: Spec
spec = describe "hosting database quotas" $ do
    it "do not apply at all without a hosting config (local / CLI / desktop)" $ do
        uploadRefusal ["a", "b", "c"] Nothing `shouldBe` Nothing
        loadRefusal ["a"] ["a"] "a" Nothing `shouldBe` Nothing
        copyRefusal ["a", "b"] ["a"] Nothing `shouldBe` Nothing

    it "treat a negative limit as unlimited" $ do
        uploadRefusal ["a", "b", "c"] (Just (plan (-1) (-1))) `shouldBe` Nothing
        memoryRefusal ["a", "b", "c"] (Just (plan (-1) (-1))) `shouldBe` Nothing

    it "allow storing up to the limit and refuse at it" $ do
        uploadRefusal [] (Just (plan 2 1)) `shouldBe` Nothing
        uploadRefusal ["a"] (Just (plan 2 1)) `shouldBe` Nothing
        uploadRefusal ["a", "b"] (Just (plan 2 1))
            `shouldBe` Just "You have reached the number of databases this plan can store. Delete one to add another."

    it "refuse every upload when the limit is zero" $
        uploadRefusal [] (Just (plan 0 0))
            `shouldBe` Just "You have reached the number of databases this plan can store. Delete one to add another."

    it "keep the two budgets independent" $ do
        -- room to store another, but no room to hold another in memory
        uploadRefusal ["a"] (Just (plan 2 1)) `shouldBe` Nothing
        memoryRefusal ["a"] (Just (plan 2 1))
            `shouldBe` Just "This plan cannot hold more uploaded databases in memory. Unload one first."

    it "prefer the plan's own wording when it has some" $ do
        let worded = (plan 1 1){hcUpgradeUpload = "Upgrade to store more."}
        uploadRefusal ["a"] (Just worded) `shouldBe` Just "Upgrade to store more."

    describe "loading" $ do
        it "never gates a database the config declares, even at full quota" $
            loadRefusal ["mine"] ["mine"] "agribalyse" (Just (plan 1 1)) `shouldBe` Nothing

        it "never refuses re-loading an upload by its own presence" $
            loadRefusal ["mine"] ["mine"] "mine" (Just (plan 1 1)) `shouldBe` Nothing

        it "refuses a fresh upload load once the memory budget is spent" $ do
            loadRefusal ["mine", "other"] ["mine"] "other" (Just (plan 2 1))
                `shouldBe` Just "This plan cannot hold more uploaded databases in memory. Unload one first."
            loadRefusal ["mine", "other"] [] "other" (Just (plan 2 1)) `shouldBe` Nothing

    describe "copying" $ do
        it "spends the stored budget like an upload" $
            copyRefusal ["a"] [] (Just (plan 1 (-1)))
                `shouldBe` Just "You have reached the number of databases this plan can store. Delete one to add another."

        it "spends the memory budget too, since the copy lands loaded" $
            copyRefusal ["a"] ["a"] (Just (plan 2 1))
                `shouldBe` Just "This plan cannot hold more uploaded databases in memory. Unload one first."

        it "passes when both budgets have room" $
            copyRefusal ["a"] ["a"] (Just (plan 2 2)) `shouldBe` Nothing

    describe "the MCP door" $
        it "refuses load_database by the same budget as REST" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert "mine" (uploadedEntry "mine"))
            resp <-
                callTool manager [] (Just (plan 1 0)) Nothing Null "load_database" $
                    KM.singleton "database" (String "mine")
            isToolError resp `shouldBe` True

-- | Whether an MCP reply is flagged as an error.
isToolError :: Value -> Bool
isToolError (Object o) = case KM.lookup "result" o of
    Just (Object r) -> KM.lookup "isError" r == Just (Bool True)
    _ -> False
isToolError _ = False

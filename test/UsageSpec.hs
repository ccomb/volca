{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The usage log: one line per computation on a process of a database that
declares its release, numbered within a start of the engine, kept until a
collector says it has them.

The fixture is the root and dependency of "DependencyLicenceSpec": the root
buys from the dependency, so a computation on the root reads both.
-}
module UsageSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Aeson (encode, object, (.=))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Internal (ResponseReceived (..))
import Servant (errHTTPCode, runHandler)
import Test.Hspec

import API.MCP (mcpApp)
import qualified API.Resources as R
import API.Routes (getActivityComparison, getActivityInventory)
import App.Env (AppEnv (..), AppM, runApp)
import Config (DatabaseConfig (..))
import qualified Database.Manager as DM
import DependencyLicenceSpec (dependency, managerOn, root, rootPid)
import Types (Licence (..), Release (..), processIdToText)
import Usage

ecoinvent :: Release
ecoinvent = Release{releaseName = "ecoinvent", releaseVersion = "3.12", releaseSystemModel = Just "Allocation, cut-off by classification"}

-- | A line numbered @n@, the rest of no interest to the bookkeeping.
lineNo :: Int -> UsageLine
lineNo n = UsageLine n (UTCTime (fromGregorian 2026 10 5) 0) Scoring "db" (ProcessKey "p") Nothing Nothing (NE.singleton ecoinvent)

-- | A log that has kept @n@ lines.
logOf :: Int -> LogState
logOf n = foldr (const (snd . appendLine 10 lineNo)) emptyLog [1 .. n]

seqs :: LogState -> [Int]
seqs = map ulSeq . foldr (:) [] . lsLines

-- | Both databases loaded, the dependency declaring the release given.
managerDeclaring :: Maybe Release -> IO DM.DatabaseManager
managerDeclaring release = do
    manager <- managerOn root dependency LicenceUnstated
    atomically $ modifyTVar' (DM.dmAvailableDbs manager) (M.adjust (\c -> c{dcRelease = release}) "dep")
    pure manager

runLogged :: DM.DatabaseManager -> UsageLog -> Maybe Text -> AppM a -> IO (Either Int a)
runLogged manager lg reader handler = either (Left . errHTTPCode) Right <$> runHandler (runApp env handler)
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
            , aeUsageLog = Just lg
            , aeReader = reader
            }

-- | One tools/call on @\/mcp@, with the headers given.
callMcp :: DM.DatabaseManager -> UsageLog -> Maybe Text -> Text -> [(Text, Text)] -> IO ()
callMcp manager lg reader tool args = do
    app <- mcpApp manager [] False Nothing Nothing id (Just lg)
    let body =
            encode $
                object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "id" .= (1 :: Int)
                    , "method" .= ("tools/call" :: Text)
                    , "params" .= object ["name" .= tool, "arguments" .= M.fromList args]
                    ]
    chunks <- newIORef (BL.toChunks body)
    let next = atomicModifyIORef' chunks $ \case
            [] -> ([], mempty)
            (c : rest) -> (rest, c)
        req = defaultRequest{requestMethod = "POST", requestHeaders = [(readerHeader, TE.encodeUtf8 r) | Just r <- [reader]]}
    _ <- app (setRequestBodyChunks next req) (const (pure ResponseReceived))
    pure ()

spec :: Spec
spec = do
    describe "the log's bookkeeping" $ do
        it "numbers lines from one and drops the oldest past the cap, saying how many" $ do
            let (d1, s1) = appendLine 2 lineNo emptyLog
                (d2, s2) = appendLine 2 lineNo s1
                (d3, s3) = appendLine 2 lineNo s2
            (d1, d2, d3) `shouldBe` (0, 0, 1)
            seqs s3 `shouldBe` [2, 3]

        it "pages after a cursor and says when more lines wait" $ do
            let full = logOf 5
            first (map ulSeq) (pageAfter 2 1 full) `shouldBe` ([2, 3], True)
            first (map ulSeq) (pageAfter 2 3 full) `shouldBe` ([4, 5], False)

        it "forgets what a collector kept, and keeps numbering after it" $ do
            let full = logOf 3
                kept = forgetThrough 2 full
            seqs kept `shouldBe` [3]
            seqs (snd (appendLine 10 lineNo kept)) `shouldBe` [3, 4]

        it "refuses to forget for a collector that read another start" $ do
            lg <- newUsageLog
            forgetUsage lg (BootId "an earlier start") 10 >>= (`shouldSatisfy` either (const True) (const False))

    describe "a computation through the REST surface" $ do
        it "leaves one line naming the kind, the process and its name, the reader and the releases read" $ do
            manager <- managerDeclaring (Just ecoinvent)
            lg <- newUsageLog
            _ <- runLogged manager lg (Just "account-7") (getActivityInventory "root" rootPid)
            UsagePage{upBoot = boot, upLines = ls} <- readUsage lg 0
            boot `shouldBe` usageBoot lg
            map (\l -> (ulKind l, ulDatabase l, ulProcess l, ulProcessName l, ulReader l, NE.toList (ulReads l))) ls
                `shouldBe` [(Inventorying, "root", ProcessKey rootPid, Just "act-FR (FR)", Just "account-7", [ecoinvent])]

        it "leaves nothing when no database it reads declares a release" $ do
            manager <- managerDeclaring Nothing
            lg <- newUsageLog
            _ <- runLogged manager lg Nothing (getActivityInventory "root" rootPid)
            upLines <$> readUsage lg 0 `shouldReturn` []

        it "counts both sides of a comparison, each with what it reads" $ do
            manager <- managerDeclaring (Just ecoinvent)
            lg <- newUsageLog
            _ <- runLogged manager lg Nothing (getActivityComparison "root" rootPid (Just (processIdToText dependency 0)) (Just "dep"))
            map (\l -> (ulKind l, ulDatabase l)) . upLines <$> readUsage lg 0
                `shouldReturn` [(Comparing, "dep"), (Comparing, "root")]

        it "reads a copy's amounts as its source's release" $ do
            manager <- managerDeclaring Nothing
            atomically $
                modifyTVar' (DM.dmAvailableDbs manager) $
                    M.adjust (\c -> c{dcRelease = Just ecoinvent}) "dep"
                        . M.adjust (\c -> c{dcSource = Just "dep"}) "root"
            DM.releasesRead manager "root" `shouldReturn` [ecoinvent]

        it "leaves nothing for a process the database does not hold" $ do
            manager <- managerDeclaring (Just ecoinvent)
            lg <- newUsageLog
            _ <- runLogged manager lg Nothing (getActivityInventory "root" "not-a-process")
            upLines <$> readUsage lg 0 `shouldReturn` []

    describe "a tool call through MCP" $ do
        it "leaves the same line, the reader read from its header" $ do
            manager <- managerDeclaring (Just ecoinvent)
            lg <- newUsageLog
            callMcp manager lg (Just "account-7") "get_inventory" [("database", "root"), ("process_id", rootPid)]
            map (\l -> (ulKind l, ulProcess l, ulProcessName l, ulReader l)) . upLines <$> readUsage lg 0
                `shouldReturn` [(Inventorying, ProcessKey rootPid, Just "act-FR (FR)", Just "account-7")]

        it "leaves nothing for a tool that reads no process" $ do
            manager <- managerDeclaring (Just ecoinvent)
            lg <- newUsageLog
            callMcp manager lg Nothing "search_activities" [("database", "root"), ("name", "x")]
            upLines <$> readUsage lg 0 `shouldReturn` []

    describe "which operations are a usage" $
        it "classes every operation that computes on a process" $
            [r | r <- R.allResources, R.resourceUsage r == Just Scoring]
                `shouldBe` [R.GetImpacts, R.ComputeSensitivity, R.ScoreActivity, R.ScoreActivities]

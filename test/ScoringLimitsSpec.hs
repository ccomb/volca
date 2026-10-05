{-# LANGUAGE OverloadedStrings #-}

{- | An instance shared by many callers can bound what one scoring request
asks for.

Scoring a whole database in one request holds every core for a minute, and
every other caller of the instance waits behind it. @max_batch_activities@ and
@max_top_flows@ under @[hosting]@ bound the two things that time grows with.
These specs pin the rule itself, its reading from the config, and the refusal
on both surfaces that score a set of activities, REST and MCP.
-}
module ScoringLimitsSpec (spec) where

import qualified Data.Text as T
import qualified TOML
import Test.Hspec

import API.BatchImpacts (BatchError (..), runBatchImpacts)
import API.Routes (batchImpactsH, hostingInfo)
import API.Types (BatchImpactsRequest (..), HostingInfo (..))
import App.Env (AppEnv (..), runApp)
import Config (HostingConfig (..), defaultConfig, scoringRefusal)
import Database.Manager (CachePolicy (..), CollectionName (..), initDatabaseManager)
import Method.Mapping (LongTermMode (..))
import Servant (ServerError (..), runHandler)

-- | Hosting config whose only interesting knobs are the two scoring limits.
limits :: Maybe Int -> Maybe Int -> HostingConfig
limits activities topFlows =
    HostingConfig
        { hcMaxUploads = -1
        , hcMaxUploadMb = -1
        , hcMaxLoadedUploads = -1
        , hcApiAccess = True
        , hcReadOnly = False
        , hcReadOnlyMessage = ""
        , hcUpgradeUpload = ""
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        , hcMaxBatchActivities = activities
        , hcMaxTopFlows = topFlows
        , hcMaxConcurrentScoring = Nothing
        }

spec :: Spec
spec = do
    describe "scoringRefusal" $ do
        it "lets everything through on an instance with no hosting section" $
            scoringRefusal Nothing 100000 (Just 100000) `shouldBe` Nothing

        it "lets a request through at the limit and refuses one past it" $ do
            scoringRefusal (Just (limits (Just 3) Nothing)) 3 Nothing `shouldBe` Nothing
            scoringRefusal (Just (limits (Just 3) Nothing)) 4 Nothing
                `shouldBe` Just "This engine scores at most 3 activities in one request, and this one covers 4. Split it into smaller requests."

        it "bounds the top contributing flows only when the request asks for some" $ do
            scoringRefusal (Just (limits Nothing (Just 5))) 1 Nothing `shouldBe` Nothing
            scoringRefusal (Just (limits Nothing (Just 5))) 1 (Just 5) `shouldBe` Nothing
            scoringRefusal (Just (limits Nothing (Just 5))) 1 (Just 50)
                `shouldBe` Just "This engine returns at most 5 top contributing flows per activity, and this request asks for 50."

    describe "reading the limits from [hosting]" $ do
        let decodeHosting t = TOML.decode t :: Either TOML.TOMLError HostingConfig
        it "reads the three keys" $
            case decodeHosting "max_batch_activities = 500\nmax_top_flows = 5\nmax_concurrent_scoring = 4\n" of
                Right hc -> (hcMaxBatchActivities hc, hcMaxTopFlows hc, hcMaxConcurrentScoring hc) `shouldBe` (Just 500, Just 5, Just 4)
                Left e -> expectationFailure (show e)

        it "reads their absence as no limit" $
            case decodeHosting "read_only = true\n" of
                Right hc -> (hcMaxBatchActivities hc, hcMaxTopFlows hc, hcMaxConcurrentScoring hc) `shouldBe` (Nothing, Nothing, Nothing)
                Left e -> expectationFailure (show e)

        -- Zero activities would refuse every scoring request while reading
        -- like a limit, so it stops the load rather than meaning "none".
        it "refuses a batch limit below one" $
            either (const True) (const False) (decodeHosting "max_batch_activities = 0\n") `shouldBe` True

        -- No slot at all would leave every scoring request waiting forever.
        it "refuses a concurrency bound below one" $
            either (const True) (const False) (decodeHosting "max_concurrent_scoring = 0\n") `shouldBe` True

    -- A client sizes its requests from the hosting route rather than
    -- learning the limit from a refusal.
    describe "the hosting route" $
        it "reports both limits, and none on an unmanaged instance" $ do
            let reported hi = (hiMaxBatchActivities hi, hiMaxTopFlows hi)
            reported (hostingInfo (Just (limits (Just 500) (Just 5)))) `shouldBe` (Just 500, Just 5)
            reported (hostingInfo Nothing) `shouldBe` (Nothing, Nothing)

    describe "the REST batch" $ do
        it "refuses a request past the limit before looking the database up" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            let env =
                    AppEnv
                        { aeDbManager = manager
                        , aeMaxTreeDepth = 5
                        , aePassword = Nothing
                        , aeHostingConfig = Just (limits (Just 2) Nothing)
                        , aeClassificationPresets = []
                        , aeDataVersion = Nothing
                        , aeUsageLog = Nothing
                        , aeReader = Nothing
                        }
            res <-
                runHandler . runApp env $
                    batchImpactsH "no-such-db" (CollectionName "no-coll") Nothing IncludeLongTerm BatchImpactsRequest{birProcessIds = ["a", "b", "c"]}
            -- A 404 here would mean the database was looked up first.
            either (Just . errHTTPCode) (const Nothing) res `shouldBe` Just 403

    -- The assistant tools score through these wrappers, which used to run
    -- with no hosting section at all and so escaped every limit.
    describe "the wrappers the assistant tools score through" $ do
        it "carry the limit to the batch" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            res <- runBatchImpacts manager (Just (limits (Just 2) Nothing)) "no-such-db" "no-coll" Nothing IncludeLongTerm ["a", "b", "c"]
            case res of
                Left (OtherBatchError 403 msg) -> msg `shouldSatisfy` T.isInfixOf "at most 2 activities"
                Left other -> expectationFailure ("expected the 403 refusal, got " <> show other)
                Right _ -> expectationFailure "expected the 403 refusal, got a result"

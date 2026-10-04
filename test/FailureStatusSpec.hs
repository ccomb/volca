{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A request that fails answers with an HTTP error, never with a 200 whose
body says it failed.

A client that reads the status alone (curl -f, a script, an older client)
would otherwise take a load that never happened for one that did. Each handler
below is given a name that exists nowhere, on a writable instance.
-}
module FailureStatusSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Hspec
import TestHelpers (withScratchDataDir)

import API.DatabaseHandlers (
    RefDataKind (..),
    copyDatabaseHandler,
    deleteDatabaseHandler,
    deleteMethodHandler,
    deleteRefData,
    deriveDatabaseHandler,
    finalizeDatabaseHandler,
    loadDatabaseHandler,
    loadRefData,
    unloadDatabaseHandler,
    unloadRefData,
    uploadDatabaseHandler,
    uploadMethodHandler,
 )
import API.Routes (loadMethodCollectionHandler, unloadMethodCollectionHandler)
import App.Env (AppEnv (..), AppM, runApp)
import Config (HostingConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Servant (ServerError (..), runHandler)
import Servant.Types.SourceT (source)

envWith :: Maybe HostingConfig -> IO AppEnv
envWith hc = do
    manager <- initDatabaseManager defaultConfig NoCache
    pure
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = hc
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            }

-- | The status and body a handler failed with, or 'Nothing' when it answered 200.
failure :: AppEnv -> AppM a -> IO (Maybe (Int, String))
failure env h = either (\e -> Just (errHTTPCode e, BSL.unpack (errBody e))) (const Nothing) <$> runHandler (runApp env h)

failing :: [(String, AppEnv -> IO (Maybe (Int, String)))]
failing =
    [ ("load", (`failure` loadDatabaseHandler "nope"))
    , ("unload", (`failure` unloadDatabaseHandler "nope"))
    , ("delete", (`failure` deleteDatabaseHandler "nope"))
    , ("copy", (`failure` copyDatabaseHandler "nope" "nope-copy"))
    , ("derive", (`failure` deriveDatabaseHandler "nope" "nope-derived" Nothing))
    , ("finalize", (`failure` finalizeDatabaseHandler "nope"))
    , ("upload", (`failure` uploadDatabaseHandler (Just "nope") Nothing (source [])))
    , ("upload without a name", (`failure` uploadDatabaseHandler Nothing Nothing (source [])))
    , ("upload-method", (`failure` uploadMethodHandler (Just "nope") Nothing (source [])))
    , ("load-method", (`failure` loadMethodCollectionHandler "nope"))
    , ("unload-method", (`failure` unloadMethodCollectionHandler "nope"))
    , ("delete-method", (`failure` deleteMethodHandler "nope"))
    , ("load-refdata", (`failure` loadRefData FlowSynonyms "nope"))
    , ("unload-refdata", (`failure` unloadRefData FlowSynonyms "nope"))
    , ("delete-refdata", (`failure` deleteRefData FlowSynonyms "nope"))
    ]

-- | Hosting that has already spent its upload budget.
noUploadsLeft :: HostingConfig
noUploadsLeft =
    HostingConfig
        { hcMaxUploads = 0
        , hcMaxUploadMb = -1
        , hcMaxLoadedUploads = -1
        , hcApiAccess = True
        , hcReadOnly = False
        , hcReadOnlyMessage = ""
        , hcUpgradeUpload = "Upgrade to upload."
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        , hcMaxBatchActivities = Nothing
        , hcMaxTopFlows = Nothing
        , hcMaxConcurrentScoring = Nothing
        }

spec :: Spec
spec = describe "A failed request" $ do
    it "answers 400 with the engine's reason, on every route that used to say it in a 200" $
        withScratchDataDir $ do
            env <- envWith Nothing
            results <- mapM (\(name, run) -> (,) name . fmap fst <$> run env) failing
            results `shouldBe` [(name, Just 400) | (name, _) <- failing]

    it "names what was not found in the body" $ do
        env <- envWith Nothing
        failure env (loadDatabaseHandler "nope") >>= \case
            Just (_, body) -> body `shouldContain` "nope"
            Nothing -> expectationFailure "the load of a missing database answered 200"

    it "answers 403 when the hosting quota refuses it" $
        withScratchDataDir $ do
            env <- envWith (Just noUploadsLeft)
            fmap fst <$> failure env (uploadDatabaseHandler (Just "nope") Nothing (source [])) `shouldReturn` Just 403
            fmap fst <$> failure env (copyDatabaseHandler "nope" "nope-copy") `shouldReturn` Just 403

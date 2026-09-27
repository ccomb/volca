{-# LANGUAGE OverloadedStrings #-}

{- | A read-only instance answers every question and changes nothing.

One server can be reached by many unrelated callers. Loading, unloading,
uploading and deleting all act on process-wide state, so any one caller doing
them acts on behalf of all the others. These specs pin the refusal on both
surfaces that reach that state, the REST handlers and the MCP tools, and
check that analysis is left alone.

The middleware guarding @/api/v1/shutdown@ and @/api/v1/idle-timeout@ lives in
the executable, so it is covered end-to-end in "ServerSpec" instead.
-}
module ReadOnlySpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Test.Hspec
import TestHelpers (withScratchDataDir)

import API.DatabaseHandlers (
    RefDataKind (..),
    addDependencyHandler,
    copyDatabaseHandler,
    createActivitiesHandler,
    deleteActivitiesHandler,
    deleteDatabaseHandler,
    deleteMethodHandler,
    deleteRefData,
    editExchangesHandler,
    finalizeDatabaseHandler,
    getDatabases,
    loadDatabaseHandler,
    loadRefData,
    relinkDatabaseHandler,
    removeDependencyHandler,
    replaceActivityHandler,
    setDataPathHandler,
    unloadDatabaseHandler,
    unloadRefData,
    uploadDatabaseHandler,
    uploadMethodHandler,
    uploadRefData,
 )
import API.MCP (callTool, noRequestId, toolDefinitions)
import API.Resources (Resource (..), allResources, resourceMutates)
import API.Routes (getHosting, loadMethodCollectionHandler, unloadMethodCollectionHandler)
import API.Types (
    ActivityInput (..),
    ActivityWriteRequest (..),
    DeleteSelectionRequest (..),
    ExchangeEditRequest (..),
    RelinkRequest (..),
 )
import App.Env (AppEnv (..), runApp)
import Config (HostingConfig (..), ReadOnly (..), defaultConfig, hostingReadOnly, readOnlyRefusal, readOnlyRefusalFor)
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Servant (ServerError (..), runHandler)
import Servant.Types.SourceT (source)

-- | Hosting config differing only in its read-only stance.
hosting :: Bool -> HostingConfig
hosting ro =
    HostingConfig
        { hcMaxUploads = -1
        , hcMaxUploadMb = -1
        , hcMaxLoadedUploads = -1
        , hcApiAccess = True
        , hcReadOnly = ro
        , hcReadOnlyMessage = ""
        , hcUpgradeUpload = ""
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        }

-- | Build an environment whose only interesting knob is the hosting stance.
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

-- | The HTTP status a handler failed with, or 'Nothing' when it succeeded.
statusOf :: Either ServerError a -> Maybe Int
statusOf = either (Just . errHTTPCode) (const Nothing)

{- | Every mutating REST handler, named, applied to arguments that never have
to be valid: the refusal must come before the handler looks at whether the
database exists, so a missing database and a refused mutation stay
distinguishable.
-}
mutatingHandlers :: [(String, AppEnv -> IO (Maybe Int))]
mutatingHandlers =
    [ ("load", run (loadDatabaseHandler "nope"))
    , ("unload", run (unloadDatabaseHandler "nope"))
    , ("delete", run (deleteDatabaseHandler "nope"))
    , ("copy", run (copyDatabaseHandler "nope" "nope-copy"))
    , ("relink", run (relinkDatabaseHandler "nope" (RelinkRequest Nothing Nothing)))
    , ("delete-selection", run (deleteActivitiesHandler "nope" everything))
    , ("add-dependency", run (addDependencyHandler "nope" "dep"))
    , ("remove-dependency", run (removeDependencyHandler "nope" "dep"))
    , ("finalize", run (finalizeDatabaseHandler "nope"))
    , ("upload", run (uploadDatabaseHandler (Just "nope") Nothing (source [])))
    , ("create-activities", run (createActivitiesHandler "nope" (ActivityWriteRequest [])))
    , ("replace-activity", run (replaceActivityHandler "nope" "id" nothingInParticular))
    , ("edit-exchanges", run (editExchangesHandler "nope" "id" noEdits))
    , ("set-data-path", run (setDataPathHandler "nope" Null))
    , ("upload-method", run (uploadMethodHandler (Just "nope") Nothing (source [])))
    , ("delete-method", run (deleteMethodHandler "nope"))
    , ("load-method", run (loadMethodCollectionHandler "nope"))
    , ("unload-method", run (unloadMethodCollectionHandler "nope"))
    , ("load-refdata", run (loadRefData FlowSynonyms "nope"))
    , ("unload-refdata", run (unloadRefData FlowSynonyms "nope"))
    , ("delete-refdata", run (deleteRefData FlowSynonyms "nope"))
    , ("upload-refdata", run (uploadRefData FlowSynonyms (Just "nope") Nothing (source [])))
    ]
  where
    run h env = statusOf <$> runHandler (runApp env h)
    noEdits =
        ExchangeEditRequest
            { eerRemove = []
            , eerSetAmounts = []
            , eerAddInputs = []
            , eerAddBiosphere = []
            , eerAddWasteOutputs = []
            }
    nothingInParticular =
        ActivityInput
            { aiName = ""
            , aiLocation = ""
            , aiDescription = []
            , aiProductName = ""
            , aiProductAmount = 0
            , aiProductUnit = ""
            , aiInputs = []
            , aiBiosphere = []
            , aiWasteOutputs = []
            }
    everything =
        DeleteSelectionRequest
            { dsqName = Nothing
            , dsqLocation = Nothing
            , dsqProduct = Nothing
            , dsqClassifications = []
            , dsqExact = Nothing
            , dsqKeep = []
            , dsqExtra = []
            , dsqIds = Just []
            }

-- | Whether an MCP reply is flagged as an error.
isToolError :: Value -> Bool
isToolError (Object o) = case KM.lookup "result" o of
    Just (Object r) -> KM.lookup "isError" r == Just (Bool True)
    _ -> False
isToolError _ = False

-- | The sentence an MCP reply carries.
toolText :: Value -> Maybe Text
toolText v = do
    Object o <- Just v
    Object r <- KM.lookup "result" o
    Array content <- KM.lookup "content" r
    Object c <- listToMaybe (toList content)
    String t <- KM.lookup "text" c
    pure t

spec :: Spec
spec = do
    describe "Config" $ do
        it "an instance is writable unless hosting says otherwise" $ do
            hostingReadOnly Nothing `shouldBe` ReadOnly False
            hostingReadOnly (Just (hosting False)) `shouldBe` ReadOnly False
            hostingReadOnly (Just (hosting True)) `shouldBe` ReadOnly True

        it "refuses with the operator's words when configured, the default otherwise" $ do
            readOnlyRefusalFor Nothing `shouldBe` readOnlyRefusal
            readOnlyRefusalFor (Just (hosting True)) `shouldBe` readOnlyRefusal
            readOnlyRefusalFor (Just ((hosting True){hcReadOnlyMessage = "Ask the operator."}))
                `shouldBe` "Ask the operator."

        it "treats a whitespace-only message as unset, never refusing blankly" $
            readOnlyRefusalFor (Just ((hosting True){hcReadOnlyMessage = " \n"}))
                `shouldBe` readOnlyRefusal

    describe "Resource registry" $
        it "counts exactly the operations that change shared state as mutations" $
            filter resourceMutates allResources `shouldBe` [LoadDatabase, UnloadDatabase, DeriveDatabase, EditExchanges]

    describe "REST handlers under read_only" $ do
        it "refuse every mutating endpoint with 403" $
            withScratchDataDir $ do
                env <- envWith (Just (hosting True))
                results <- mapM (\(name, run) -> (,) name <$> run env) mutatingHandlers
                results `shouldBe` [(name, Just 403) | (name, _) <- mutatingHandlers]

        it "carry the operator's message in the refusal body when configured" $ do
            env <- envWith (Just ((hosting True){hcReadOnlyMessage = "Ask the operator."}))
            result <- runHandler (runApp env (loadDatabaseHandler "nope"))
            case result of
                Left err -> errBody err `shouldBe` "Ask the operator."
                Right _ -> expectationFailure "the mutation was not refused"

        it "still answer a read-only endpoint" $ do
            env <- envWith (Just (hosting True))
            listed <- runHandler (runApp env getDatabases)
            statusOf listed `shouldBe` Nothing

        it "never answer 403 on a writable instance" $
            -- The scratch dir keeps the upload handlers, which really write,
            -- out of the working tree.
            withScratchDataDir $ do
                env <- envWith (Just (hosting False))
                results <- mapM (\(name, run) -> (,) name <$> run env) mutatingHandlers
                map snd results `shouldNotContain` [Just 403]

    describe "MCP tools under read_only" $ do
        it "refuse the state-changing tools" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            let call name =
                    callTool manager [] (Just (hosting True)) Nothing noRequestId name $
                        KM.singleton "database" (String "nope")
            loadResp <- call "load_database"
            unloadResp <- call "unload_database"
            isToolError loadResp `shouldBe` True
            isToolError unloadResp `shouldBe` True

        it "refuse with the operator's words when configured" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            resp <-
                callTool manager [] (Just ((hosting True){hcReadOnlyMessage = "Ask the operator."})) Nothing noRequestId "load_database" $
                    KM.singleton "database" (String "nope")
            isToolError resp `shouldBe` True
            toolText resp `shouldBe` Just "Ask the operator."

        it "still answer a read-only tool" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            listed <- callTool manager [] (Just (hosting True)) Nothing noRequestId "list_databases" KM.empty
            isToolError listed `shouldBe` False

        it "hide the state-changing tools from tools/list" $ do
            let names ro = [n | Object o <- toolDefinitions ro, Just (String n) <- [KM.lookup "name" o]]
            names (ReadOnly True) `shouldNotContain` ["load_database"]
            names (ReadOnly True) `shouldNotContain` ["unload_database"]
            names (ReadOnly True) `shouldContain` ["list_databases"]
            names (ReadOnly False) `shouldContain` ["load_database"]

    describe "GET /api/v1/hosting" $ do
        let messageReported hc = do
                env <- envWith (Just hc)
                result <- runHandler (runApp env getHosting)
                pure $ case result of
                    Right (Object o) -> KM.lookup "read_only_message" o
                    _ -> Nothing

        it "reports the sentence a refusal will actually carry" $ do
            messageReported (hosting True) `shouldReturn` Just (String readOnlyRefusal)
            messageReported ((hosting True){hcReadOnlyMessage = "Ask the operator."})
                `shouldReturn` Just (String "Ask the operator.")

        it "reports no message on a writable instance, even a leftover one" $
            messageReported ((hosting False){hcReadOnlyMessage = "leftover"})
                `shouldReturn` Just (String "")

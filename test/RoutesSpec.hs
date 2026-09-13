{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Integration tests for the HTTP API surface (Servant routes).

We boot the real volca binary as a subprocess ONCE (via beforeAll +
afterAll), threading the bundled resources into each test rather than
relying on top-level mutable state. The subprocess listens on an
OS-assigned ephemeral port so parallel CI runners do not collide.
-}
module RoutesSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (
    Manager,
    Response,
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseHeaders,
    responseStatus,
 )
import Network.HTTP.Types (statusCode)
import Network.Socket (
    Family (AF_INET),
    SockAddr (SockAddrInet),
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    socket,
    socketPort,
    tupleToHostAddress,
 )
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (Handle, hClose, openTempFile)
import qualified System.Info as Info
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getProcessExitCode,
    interruptProcessGroupOf,
    proc,
    readProcess,
    waitForProcess,
 )
import Test.Hspec

import API.Routes (serviceErrorToServerError)
import Servant.Server (errHTTPCode)
import Service (ServiceError (..))

-- | Resources owned for the lifetime of the spec, threaded via beforeAll.
data Booted = Booted
    { bManager :: Manager
    , bProcess :: ProcessHandle
    , bConfigPath :: FilePath
    , bLogHandle :: Handle
    , bLogPath :: FilePath
    , bBaseUrl :: String
    }

findVolcaExe :: IO FilePath
findVolcaExe = do
    envExe <- lookupEnv "VOLCA_EXE"
    exe <- case envExe of
        Just p | not (null p) -> pure p
        _ -> do
            raw <- readProcess "cabal" ["list-bin", "exe:volca"] ""
            pure (dropWhileEnd isSpace raw)
    exists <- doesFileExist exe
    if exists
        then pure exe
        else error $ "volca executable not found at " <> exe <> ". Run 'cabal build' first."

{- | Ask the OS for a free TCP port on 127.0.0.1. There is a small race
between closing this socket and the child binding to it, but it is
accepted as the cost of avoiding hardcoded ports clashing under
concurrent CI on the same host.
-}
findFreePort :: IO Int
findFreePort =
    bracket
        (socket AF_INET Stream defaultProtocol)
        close
        ( \sock -> do
            bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
            fromIntegral <$> socketPort sock
        )

bootServer :: IO Booted
bootServer = do
    exe <- findVolcaExe
    mgr <- newManager defaultManagerSettings
    port <- findFreePort
    tmp <- getTemporaryDirectory
    (logPath, logHandle) <- openTempFile tmp "volca-routes-test.log"
    let cfg = tmp </> ("volca-routes-test-" <> show port <> ".toml")
    writeFile cfg ("[server]\nport = " <> show port <> "\nhost = \"127.0.0.1\"\n")
    let args = ["--config", cfg, "server", "--port", show port]
        url = "http://127.0.0.1:" <> show port
    (_, _, _, ph) <-
        createProcess
            (proc exe args)
                { std_out = UseHandle logHandle
                , std_err = UseHandle logHandle
                , create_group = True
                }
    ready <- waitForReady mgr url 50
    if ready
        then pure (Booted mgr ph cfg logHandle logPath url)
        else do
            interruptProcessGroupOf ph
            _ <- waitForProcess ph
            hClose logHandle
            _ <- try @SomeException (removeFile logPath)
            _ <- try @SomeException (removeFile cfg)
            error "Server failed to start within timeout (10s)"

teardownServer :: Booted -> IO ()
teardownServer b = do
    mCode <- getProcessExitCode (bProcess b)
    case mCode of
        Nothing -> do
            interruptProcessGroupOf (bProcess b)
            _ <- waitForProcess (bProcess b)
            pure ()
        Just _ -> pure ()
    hClose (bLogHandle b)
    _ <- try @SomeException (removeFile (bConfigPath b))
    _ <- try @SomeException (removeFile (bLogPath b))
    pure ()

waitForReady :: Manager -> String -> Int -> IO Bool
waitForReady _ _ 0 = pure False
waitForReady mgr url remaining = do
    threadDelay 200000
    alive <- isAlive mgr url
    if alive then pure True else waitForReady mgr url (remaining - 1)

isAlive :: Manager -> String -> IO Bool
isAlive mgr url = do
    r <- try $ do
        req <- parseRequest (url <> "/api/v1/db")
        resp <- httpLbs req mgr
        pure (statusCode (responseStatus resp))
    case r of
        Right code -> pure (code < 500)
        Left (_ :: SomeException) -> pure False

doGet :: Booted -> String -> IO (Response BL.ByteString)
doGet b path = do
    req <- parseRequest (bBaseUrl b <> path)
    httpLbs req (bManager b)

doPost :: Booted -> String -> IO (Response BL.ByteString)
doPost b path = do
    req0 <- parseRequest (bBaseUrl b <> path)
    let req = req0{method = "POST", requestHeaders = [("Content-Type", "application/json")]}
    httpLbs req (bManager b)

spec :: Spec
spec = do
    errorMappingSpec
    integrationSpec

{- | Pure regression guard for the ServiceError -> HTTP status contract. Lives
outside the booted-server block so it runs everywhere (incl. Windows) and
needs no loaded database.
-}
errorMappingSpec :: Spec
errorMappingSpec = describe "serviceErrorToServerError (HTTP status contract)" $ do
    it "maps InvalidUUID to 400 (malformed client id, never 5xx)" $
        errHTTPCode (serviceErrorToServerError (InvalidUUID "x")) `shouldBe` 400
    it "maps InvalidProcessId to 400" $
        errHTTPCode (serviceErrorToServerError (InvalidProcessId "x")) `shouldBe` 400
    it "maps AmbiguousActivity to 400, not the 404 of a missing one" $
        errHTTPCode (serviceErrorToServerError (AmbiguousActivity "x")) `shouldBe` 400
    it "maps ActivityNotFound to 404" $
        errHTTPCode (serviceErrorToServerError (ActivityNotFound "x")) `shouldBe` 404
    it "maps FlowNotFound to 404" $
        errHTTPCode (serviceErrorToServerError (FlowNotFound "x")) `shouldBe` 404
    it "maps MatrixError to 422" $
        errHTTPCode (serviceErrorToServerError (MatrixError "x")) `shouldBe` 422

integrationSpec :: Spec
integrationSpec
    | Info.os == "mingw32" =
        describe "Routes integration (skipped on Windows)" $
            it "subprocess teardown deadlocks on this platform" pending
    | otherwise = beforeAll bootServer $ afterAll teardownServer routeSpecs

routeSpecs :: SpecWith Booted
routeSpecs = do
    describe "core listing endpoints" $ do
        let endpoints =
                [ "/api/v1/db"
                , "/api/v1/methods"
                , "/api/v1/units"
                , "/api/v1/compartment-mappings"
                ]
        mapM_
            ( \ep ->
                it ("GET " <> ep <> " returns 200") $ \b -> do
                    resp <- doGet b ep
                    statusCode (responseStatus resp) `shouldBe` 200
            )
            endpoints

    describe "version + tooling dumps" $ do
        it "GET /api/v1/version returns a JSON object carrying a 'version' field" $ \b -> do
            -- Body must be a JSON object with the documented 'version' key -
            -- anything else (HTML error page, empty body, wrong wrapper) is a
            -- regression that 'body length > 0' would silently accept.
            resp <- doGet b "/api/v1/version"
            statusCode (responseStatus resp) `shouldBe` 200
            decode (responseBody resp) `shouldSatisfy` \case
                Just (Object km) -> KM.member "version" km
                _ -> False

        it "GET /api/v1/version advertises an integer 'wireVersion'" $ \b -> do
            -- wireVersion is the field clients read to detect a JSON wire-format
            -- mismatch at connect time; a rename, removal, or retype silently
            -- breaks that check, so assert both presence and that it decodes as
            -- a JSON number rather than just 'body length > 0'.
            resp <- doGet b "/api/v1/version"
            statusCode (responseStatus resp) `shouldBe` 200
            decode (responseBody resp) `shouldSatisfy` \case
                Just (Object km) -> case KM.lookup "wireVersion" km of
                    Just (Number _) -> True
                    _ -> False
                _ -> False

        it "GET /api/v1/version says which data bundle it reads: the built-in one when none is named" $ \b -> do
            -- The test config names no flow registry, so the engine reads the
            -- one it carries and says its number. The key is always there: a
            -- client that read wire 11 tells "no bundle" (null, a registry
            -- with no VERSION beside it) from "an engine too old to say".
            expected <- T.strip <$> TIO.readFile "data/VERSION"
            resp <- doGet b "/api/v1/version"
            decode (responseBody resp) `shouldSatisfy` \case
                Just (Object km) -> KM.lookup "dataVersion" km == Just (String expected)
                _ -> False

        it "GET /api/v1/openapi.json returns an OpenAPI 3 document with 'paths'" $ \b -> do
            -- The OpenAPI spec is served at /api/v1/openapi.json (not /openapi).
            -- We check the actual contract (openapi+paths keys), not the byte
            -- length - Swagger UI would break in the same way a missing 'paths'
            -- would, while a 'body length > 100' assertion would pass on any
            -- random JSON-shaped payload.
            resp <- doGet b "/api/v1/openapi.json"
            statusCode (responseStatus resp) `shouldBe` 200
            decode (responseBody resp) `shouldSatisfy` \case
                Just (Object km) -> KM.member "openapi" km && KM.member "paths" km
                _ -> False

    describe "404 / unknown resource" $ do
        it "GET /api/v1/db/no-such-db/activity/whatever returns 404" $ \b -> do
            -- This hits an unambiguously absent route: even if the DB existed,
            -- 'whatever' is not a real processId, so the handler returns 404.
            resp <- doGet b "/api/v1/db/no-such-db/activity/whatever"
            statusCode (responseStatus resp) `shouldBe` 404

        it "GET /api/v1/db/no-such-db/gap-report returns 404" $ \b -> do
            -- The gap report answers only for a loaded or staged database.
            resp <- doGet b "/api/v1/db/no-such-db/gap-report"
            statusCode (responseStatus resp) `shouldBe` 404

        it "GET /api/v1/db/no-such-db/quality-report returns 404" $ \b -> do
            -- Same contract as the gap report: loaded or staged, or nothing.
            resp <- doGet b "/api/v1/db/no-such-db/quality-report"
            statusCode (responseStatus resp) `shouldBe` 404

        it "GET /api/v1/db/no-such-db/characterization-coverage returns 404" $ \b -> do
            -- Loaded-only: it reads the built method tables, so an absent
            -- database is a 404.
            resp <- doGet b "/api/v1/db/no-such-db/characterization-coverage"
            statusCode (responseStatus resp) `shouldBe` 404

    describe "method-not-allowed" $ do
        it "GET /api/v1/db/X returns 405 (only DELETE is defined on /db/{name})" $ \b -> do
            -- Documents the current API surface: /db/{name} accepts DELETE,
            -- not GET - so Servant rightly answers 405 for GET.
            resp <- doGet b "/api/v1/db/no-such-db"
            statusCode (responseStatus resp) `shouldBe` 405

        it "POST /api/v1/db returns 405 (db listing is read-only)" $ \b -> do
            resp <- doPost b "/api/v1/db"
            statusCode (responseStatus resp) `shouldBe` 405

    describe "response shape" $ do
        it "JSON endpoints carry Content-Type: application/json" $ \b -> do
            resp <- doGet b "/api/v1/db"
            let ct = lookup "Content-Type" (responseHeaders resp)
            ct `shouldSatisfy` maybe False ("application/json" `BS.isPrefixOf`)

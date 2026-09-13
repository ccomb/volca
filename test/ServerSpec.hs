{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openFile)
import qualified System.Info as Info
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getProcessExitCode, interruptProcessGroupOf, proc, readProcess, waitForProcess)
import Test.Hspec

{- | Find the volca executable in the build directory.

Prefer the VOLCA_EXE env var when set – build.sh exports it after
`cabal list-bin` so the test does not have to spawn cabal again. The
fallback to `cabal list-bin exe:volca` keeps `cabal test` from the
project root working without extra setup. The shell-out path is the
one that hangs on Windows: cabal sees the project files as modified
and tries to re-configure under a build lock the parent `cabal test`
already holds, deadlocking until the runner kills the job.
-}
findVolcaExe :: IO FilePath
findVolcaExe = do
    envExe <- lookupEnv "VOLCA_EXE"
    exe <- case envExe of
        Just p | not (null p) -> return p
        _ -> do
            raw <- readProcess "cabal" ["list-bin", "exe:volca"] ""
            return (dropWhileEnd isSpace raw)
    exists <- doesFileExist exe
    if exists
        then return exe
        else error $ "volca executable not found at " ++ exe ++ ". Run 'cabal build' first."

-- | Port for test server (high port to avoid conflicts)
testPort :: Int
testPort = 18199

-- | Write a minimal TOML config (no databases, no auth) to a temp file
withMinimalConfig :: (FilePath -> IO a) -> IO a
withMinimalConfig action = do
    tmpDir <- getTemporaryDirectory
    let cfgPath = tmpDir </> "volca-test-server.toml"
    writeFile cfgPath "[server]\nport = 18199\nhost = \"127.0.0.1\"\n"
    result <- action cfgPath
    removeFile cfgPath
    return result

{- | The same minimal config, declared read-only, with the operator's own
refusal message - so the specs below also prove that message crosses the
whole stack, TOML file to HTTP response.

A read-only instance is one many unrelated callers share, so none of them may
end it: both lifetime endpoints must refuse rather than obey.
-}
withReadOnlyConfig :: (FilePath -> IO a) -> IO a
withReadOnlyConfig action = do
    tmpDir <- getTemporaryDirectory
    let cfgPath = tmpDir </> "volca-test-server-readonly.toml"
    writeFile cfgPath $
        "[server]\nport = 18199\nhost = \"127.0.0.1\"\n\n"
            ++ "[hosting]\nread_only = true\nread_only_message = \""
            ++ T.unpack operatorMessage
            ++ "\"\n"
    result <- action cfgPath
    removeFile cfgPath
    return result

-- | The sentence the operator configured in 'withReadOnlyConfig'.
operatorMessage :: Text
operatorMessage = "Ask the operator."

-- | Start the server, run action, ensure cleanup
withServer :: FilePath -> (ProcessHandle -> Manager -> IO a) -> IO a
withServer cfgPath action = do
    exe <- findVolcaExe
    mgr <- newManager defaultManagerSettings
    tmpDir <- getTemporaryDirectory
    let logFile = tmpDir </> "volca-test-server.log"
    logHandle <- openFile logFile AppendMode
    let args = ["--config", cfgPath, "server", "--port", show testPort]
    (_, _, _, ph) <-
        createProcess
            (proc exe args)
                { std_out = UseHandle logHandle
                , std_err = UseHandle logHandle
                , create_group = True
                }
    -- Wait for server to be ready (poll)
    ready <- waitForReady mgr 30
    if ready
        then bracket (pure ()) (\_ -> cleanup ph logHandle) $ \_ -> action ph mgr
        else do
            cleanup ph logHandle
            error "Server failed to start within timeout"
  where
    cleanup ph logHandle = do
        mCode <- getProcessExitCode ph
        case mCode of
            Nothing -> do
                interruptProcessGroupOf ph
                _ <- waitForProcess ph
                pure ()
            Just _ -> pure ()
        hClose logHandle

-- | Poll until server responds or timeout
waitForReady :: Manager -> Int -> IO Bool
waitForReady _ 0 = return False
waitForReady mgr remaining = do
    threadDelay 200000 -- 200ms
    alive <- isAlive mgr
    if alive then return True else waitForReady mgr (remaining - 1)

{- | Poll 'getProcessExitCode' until the process exits or budget runs out.
Returns the exit code if it exited within budget, 'Nothing' otherwise.
Budget is in 200ms ticks.

Why: shutdown is asynchronous – the endpoint returns immediately and the
RTS finishes teardown some hundreds of ms later. A fixed sleep is racy
on slow CI runners; polling lets the test pass as soon as the process
actually exits and bounds the worst case at a clear upper limit.

How to apply: probe the process, NOT the HTTP socket. Hitting @isAlive@
in a tight loop would count as activity and reset the idle timer the
@idle-timeout@ test depends on.
-}
waitForExit :: ProcessHandle -> Int -> IO (Maybe ExitCode)
waitForExit _ 0 = pure Nothing
waitForExit ph remaining = do
    mCode <- getProcessExitCode ph
    case mCode of
        Just _ -> pure mCode
        Nothing -> threadDelay 200000 >> waitForExit ph (remaining - 1)

-- | Check if server is reachable
isAlive :: Manager -> IO Bool
isAlive mgr = do
    result <- try $ do
        req <- parseRequest $ "http://127.0.0.1:" ++ show testPort ++ "/api/v1/db"
        resp <- httpLbs req mgr
        return $ statusCode (responseStatus resp)
    case result of
        Right code -> return (code < 500)
        Left (_ :: SomeException) -> return False

-- | POST to a server endpoint
postEndpoint :: Manager -> String -> IO Int
postEndpoint mgr path = fst <$> postEndpointWithBody mgr path

-- | POST to a server endpoint, returning the status and the body text
postEndpointWithBody :: Manager -> String -> IO (Int, Text)
postEndpointWithBody mgr path = do
    req0 <- parseRequest $ "http://127.0.0.1:" ++ show testPort ++ path
    let req = req0{method = "POST", requestHeaders = [("Content-Type", "application/json")]}
    resp <- httpLbs req mgr
    return (statusCode (responseStatus resp), TE.decodeUtf8Lenient (BSL.toStrict (responseBody resp)))

{- | These specs spawn volca as a subprocess and tear it down with
interruptProcessGroupOf + waitForProcess. On Windows the terminate
signal does not unblock the running RTS reliably, so cleanup waits
forever and the whole suite stalls until the runner kills the job.
Skip the spawn-based specs there.
-}
spec :: Spec
spec
    | Info.os == "mingw32" =
        describe "Server lifecycle (skipped on Windows)" $
            it "subprocess teardown deadlocks on this platform" pending
    | otherwise = serverSpecs

serverSpecs :: Spec
serverSpecs = do
    describe "Server shutdown endpoint" $ do
        it "POST /api/v1/shutdown stops the server" $ do
            withMinimalConfig $ \cfgPath ->
                withServer cfgPath $ \ph mgr -> do
                    isAlive mgr `shouldReturn` True
                    code <- postEndpoint mgr "/api/v1/shutdown"
                    code `shouldBe` 200
                    -- Poll for process exit, up to 5s (25 * 200ms).
                    mCode <- waitForExit ph 25
                    mCode `shouldBe` Just ExitSuccess
                    isAlive mgr `shouldReturn` False

    describe "Read-only instance" $ do
        it "refuses to be shut down, and survives the attempt" $ do
            withReadOnlyConfig $ \cfgPath ->
                withServer cfgPath $ \ph mgr -> do
                    (code, body) <- postEndpointWithBody mgr "/api/v1/shutdown"
                    code `shouldBe` 403
                    body `shouldSatisfy` T.isInfixOf operatorMessage
                    -- Give a shutdown that wrongly went through time to land.
                    waitForExit ph 10 `shouldReturn` Nothing
                    isAlive mgr `shouldReturn` True

        it "refuses to have an idle timeout armed on it" $ do
            withReadOnlyConfig $ \cfgPath ->
                withServer cfgPath $ \ph mgr -> do
                    code <- postEndpoint mgr "/api/v1/idle-timeout/2"
                    code `shouldBe` 403
                    -- Stay quiet past the timer that was refused, then check
                    -- the process (not the socket, which would reset a timer).
                    threadDelay 2200000
                    waitForExit ph 10 `shouldReturn` Nothing

    describe "Server idle timeout" $ do
        it "POST /api/v1/idle-timeout/N shuts down after N seconds" $ do
            withMinimalConfig $ \cfgPath ->
                withServer cfgPath $ \ph mgr -> do
                    isAlive mgr `shouldReturn` True
                    code <- postEndpoint mgr "/api/v1/idle-timeout/2"
                    code `shouldBe` 200
                    -- The idle timer resets on each HTTP hit, so the next
                    -- check is the last activity before the quiet window.
                    isAlive mgr `shouldReturn` True
                    -- Then stay quiet for the full timer + a small grace,
                    -- then poll the *process* (not the socket) for exit.
                    -- Budget: 6s of polling after a 2s quiet window covers
                    -- generous CI scheduling slop.
                    threadDelay 2200000 -- 2.2s – let the 2s timer fire
                    mCode <- waitForExit ph 30
                    mCode `shouldBe` Just ExitSuccess

        it "POST /api/v1/idle-timeout/0 cancels timeout" $ do
            withMinimalConfig $ \cfgPath ->
                withServer cfgPath $ \_ph mgr -> do
                    -- Activate 2s timeout then immediately cancel
                    _ <- postEndpoint mgr "/api/v1/idle-timeout/2"
                    _ <- postEndpoint mgr "/api/v1/idle-timeout/0"
                    -- Wait longer than the timeout
                    threadDelay 3500000 -- 3.5s
                    -- Server should still be alive
                    isAlive mgr `shouldReturn` True

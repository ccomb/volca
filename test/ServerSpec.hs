{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestHeaders, responseStatus)
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

Prefer the VOLCA_EXE env var when set — build.sh exports it after
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
postEndpoint mgr path = do
    req0 <- parseRequest $ "http://127.0.0.1:" ++ show testPort ++ path
    let req = req0{method = "POST", requestHeaders = [("Content-Type", "application/json")]}
    resp <- httpLbs req mgr
    return $ statusCode (responseStatus resp)

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
                    -- Server should be alive
                    isAlive mgr `shouldReturn` True
                    -- Send shutdown
                    code <- postEndpoint mgr "/api/v1/shutdown"
                    code `shouldBe` 200
                    -- Wait for server to die
                    threadDelay 1000000 -- 1s
                    isAlive mgr `shouldReturn` False
                    -- Process should have exited
                    mCode <- getProcessExitCode ph
                    mCode `shouldBe` Just ExitSuccess

    describe "Server idle timeout" $ do
        it "POST /api/v1/idle-timeout/N shuts down after N seconds" $ do
            withMinimalConfig $ \cfgPath ->
                withServer cfgPath $ \ph mgr -> do
                    -- Server should be alive
                    isAlive mgr `shouldReturn` True
                    -- Activate 2-second idle timeout
                    code <- postEndpoint mgr "/api/v1/idle-timeout/2"
                    code `shouldBe` 200
                    -- Still alive immediately
                    isAlive mgr `shouldReturn` True
                    -- Wait for timeout + buffer
                    threadDelay 3500000 -- 3.5s
                    -- Should be dead
                    isAlive mgr `shouldReturn` False
                    mCode <- getProcessExitCode ph
                    mCode `shouldBe` Just ExitSuccess

        it "POST /api/v1/idle-timeout/0 cancels timeout" $ do
            withMinimalConfig $ \cfgPath ->
                withServer cfgPath $ \ph mgr -> do
                    -- Activate 2s timeout then immediately cancel
                    _ <- postEndpoint mgr "/api/v1/idle-timeout/2"
                    _ <- postEndpoint mgr "/api/v1/idle-timeout/0"
                    -- Wait longer than the timeout
                    threadDelay 3500000 -- 3.5s
                    -- Server should still be alive
                    isAlive mgr `shouldReturn` True

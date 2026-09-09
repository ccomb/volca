{-# LANGUAGE OverloadedStrings #-}

{- | The idle watchdog decides when a server is unused. It must not decide that
while the server is answering.
-}
module IdleSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types (status200)
import Network.Wai (Application, Request (..), defaultRequest, responseLBS)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

import App.Idle (Bearing (..), IdleState (..), bearingOf, idleTrackingMiddleware, idleWatchdog, newIdleState)

spec :: Spec
spec = do
    describe "Idle watchdog" $ do
        it "does not shut down under a request that outlasts the timeout" $ do
            idle <- armed
            fired <- newIORef False
            _ <- forkIO (idleWatchdog idle 1 (writeIORef fired True))
            runOnce (idleTrackingMiddleware idle (slowApp 2500000)) "/api/v1/db"
            readIORef fired `shouldReturn` False

        it "shuts down once nothing is running any more" $ do
            idle <- armed
            fired <- newIORef False
            _ <- forkIO (idleWatchdog idle 1 (writeIORef fired True))
            runOnce (idleTrackingMiddleware idle (slowApp 0)) "/api/v1/db"
            threadDelay 3000000
            readIORef fired `shouldReturn` True

        it "shuts down under a log stream left open" $ do
            idle <- armed
            fired <- newIORef False
            _ <- forkIO (idleWatchdog idle 1 (writeIORef fired True))
            runOnce (idleTrackingMiddleware idle (slowApp 3000000)) "/api/v1/logs/stream"
            readIORef fired `shouldReturn` True

    describe "What a path says about the server being in use" $ do
        it "counts an ordinary request for as long as it runs" $
            bearingOf "/api/v1/db" `shouldBe` Counted

        it "does not let an open log stream stand for someone working" $
            bearingOf "/api/v1/logs/stream" `shouldBe` Stamped

        it "leaves the assistant endpoint to judge its own calls" $
            bearingOf "/mcp" `shouldBe` Ignored

armed :: IO IdleState
armed = do
    idle <- newIdleState
    writeIORef (idleArmed idle) True
    pure idle

-- | An application that takes its time before answering.
slowApp :: Int -> Application
slowApp micros _ respond = do
    threadDelay micros
    respond (responseLBS status200 [] "ok")

{- | Put one request for a path through an application. It returns when the
application has answered, which is what makes the first test a test: the
watchdog gets its chance to fire while this is still waiting.
-}
runOnce :: Application -> ByteString -> IO ()
runOnce app path = do
    ResponseReceived <- app defaultRequest{rawPathInfo = path} (\_ -> pure ResponseReceived)
    pure ()

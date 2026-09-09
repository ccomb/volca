{-# LANGUAGE OverloadedStrings #-}

{- | When a server has gone unused, and what counts as being used.

Three things count, because any two alone would be wrong. An HTTP request
proves someone is there. A matrix solve proves expensive work is under way,
which may well outlast the request that asked for it. And a request still
running is the server being used right now, however long it takes: a load that
reads a gigabyte of source and builds its matrices can outlast the whole
timeout, and shutting down under it hands the caller a closed socket and no
answer at all.
-}
module App.Idle (
    IdleState (..),
    newIdleState,
    stampIdle,
    whileWorking,
    Bearing (..),
    bearingOf,
    idleTrackingMiddleware,
    idleWatchdog,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network.Wai (Application, rawPathInfo)

import qualified Matrix

-- | What the idle watchdog reads to decide the server is unused.
data IdleState = IdleState
    { idleLastRequest :: !(IORef UTCTime)
    -- ^ When the server was last known to be in use.
    , idleInFlight :: !(IORef Int)
    -- ^ How many requests are running right now.
    , idleArmed :: !(IORef Bool)
    -- ^ Whether a watchdog is watching at all.
    }

newIdleState :: IO IdleState
newIdleState = IdleState <$> (newIORef =<< getCurrentTime) <*> newIORef 0 <*> newIORef False

-- | Move the idle deadline to now.
stampIdle :: IdleState -> IO ()
stampIdle idle = getCurrentTime >>= writeIORef (idleLastRequest idle)

-- | What a request on a given path says about the server being in use.
data Bearing
    = {- | The ordinary answer: someone asked, and the server is in use until
      they have their reply, however long that takes.
      -}
      Counted
    | {- | Someone asked, and how long the answer takes says nothing. A log
      stream stays open for as long as a reader leaves a terminal open;
      counting that would keep the server alive, and billing, for ever.
      -}
      Stamped
    | {- | It says nothing at all, because that endpoint answers the question
      itself: a connected assistant polls @\/mcp@ on its own initiative, and
      counting those would keep an idle server alive with nobody there.
      -}
      Ignored
    deriving (Eq, Show)

bearingOf :: ByteString -> Bearing
bearingOf path = case path of
    "/mcp" -> Ignored
    "/api/v1/logs/stream" -> Stamped
    _ -> Counted

{- | Count the server as in use for as long as an action runs.

Both stamps carry their weight. The one before the count rises closes the
window where a watchdog tick lands after the request arrived and before it was
counted; the one before the count falls closes the same window at the other
end.
-}
whileWorking :: IdleState -> IO a -> IO a
whileWorking idle action = do
    stampIdle idle
    bracket_ (bump 1) (stampIdle idle >> bump (-1)) action
  where
    bump :: Int -> IO ()
    bump delta = atomicModifyIORef' (idleInFlight idle) (\n -> (n + delta, ()))

{- | WAI middleware that records that a request is happening, and while it runs.

The timestamp alone said when a request /arrived/, which is not when the server
was last in use: a load that reads a gigabyte of source and builds its matrices
can outlast the whole timeout, and the watchdog would then shut the process down
under the caller, who sees a closed socket and no answer.
-}
idleTrackingMiddleware :: IdleState -> Application -> Application
idleTrackingMiddleware idle app req respond = case bearingOf (rawPathInfo req) of
    Counted -> whileWorking idle (app req respond)
    Stamped -> stampIdle idle >> app req respond
    Ignored -> app req respond

{- | Watch until the server has gone unused for @timeoutSecs@, then run
@onIdle@ - the process exits in production, and a test records that it would
have. Returns as soon as the watchdog is disarmed.

The solve count is read before the deadline is judged: a solve that lands in
the last seconds has to be seen before the clock is.
-}
idleWatchdog :: IdleState -> Int -> IO () -> IO ()
idleWatchdog idle timeoutSecs onIdle = go =<< Matrix.readSolveCounter
  where
    checkInterval :: Int
    checkInterval = min (timeoutSecs * 1000000) (5 * 1000000) -- every 5s, or the timeout when shorter
    go :: Int -> IO ()
    go lastSeen = do
        threadDelay checkInterval
        armed <- readIORef (idleArmed idle)
        when armed $ do
            solves <- Matrix.readSolveCounter
            running <- readIORef (idleInFlight idle)
            when (solves /= lastSeen || running > 0) (stampIdle idle)
            elapsed <- secondsSinceLastRequest
            if elapsed >= fromIntegral timeoutSecs
                then onIdle
                else go solves

    secondsSinceLastRequest :: IO Double
    secondsSinceLastRequest = do
        now <- getCurrentTime
        realToFrac . diffUTCTime now <$> readIORef (idleLastRequest idle)

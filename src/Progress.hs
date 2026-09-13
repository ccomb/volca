{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Progress
Description : Unified progress reporting system for the LCA engine

This module provides a unified system for reporting progress during LCA calculations.
It replaces the scattered trace statements throughout the codebase with clean,
informative progress output that includes timing information.

Key features:
- Consistent formatting for all progress messages
- Automatic timing measurement for operations
- Clean output suitable for production use
- Stderr-based output that doesn't interfere with JSON responses
- In-memory log buffer accessible via API
-}
module Progress (
    -- * Progress reporting functions
    reportProgress,
    reportProgressWithTiming,
    reportError,
    reportCacheOperation,
    reportMatrixOperation,
    reportSolverOperation,

    -- * Timing utilities
    withProgressTiming,
    formatDuration,

    -- * Performance monitoring
    reportMemoryUsage,
    formatBytes,
    reportCacheInfo,

    -- * Log buffer
    getLogLines,
    waitForNewLines,

    -- * Log scoping
    withLogScope,
    inheritLogScope,

    -- * Types
    ProgressLevel (..),
    LogLine (..),
) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Exception (SomeException, bracket_, catch, try)
import Control.Monad (when)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.Directory (doesFileExist, getFileSize)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

-- | Progress reporting levels
data ProgressLevel
    = -- | General information
      Info
    | -- | Warning messages
      Warning
    | -- | Cache operations
      Cache
    | -- | Matrix construction operations
      Matrix
    | -- | Matrix solver operations
      Solver
    | -- | Error messages
      Error
    deriving (Eq, Show)

{- | One buffered log line, tagged with the database whose operation emitted it
when that operation declared one via 'withLogScope'. The tag is what lets a
consumer showing one database's progress drop the lines of a concurrent load
of another.
-}
data LogLine = LogLine
    { llScope :: !(Maybe Text)
    -- ^ Database name the emitting operation was scoped to, if any
    , llText :: !String
    -- ^ Formatted line, exactly as written to stderr
    }
    deriving (Eq, Show)

instance ToJSON LogLine where
    toJSON l = object ["db" .= llScope l, "text" .= llText l]

-- | In-memory log buffer state
data LogBuffer = LogBuffer
    { lbLines :: !(Seq LogLine)
    -- ^ Buffered log lines
    , lbOffset :: !Int
    -- ^ Absolute index of first line in buffer
    , lbMaxSize :: !Int
    -- ^ Maximum number of lines to keep
    }

{- | Global mutex to serialize all progress output operations
This prevents concurrent stderr output from corrupting the console and causing crashes
-}
{-# NOINLINE progressOutputMutex #-}
progressOutputMutex :: MVar ()
progressOutputMutex = unsafePerformIO $ newMVar ()

-- | Global in-memory log buffer (TVar for STM-based blocking reads)
{-# NOINLINE logBufferRef #-}
logBufferRef :: TVar LogBuffer
logBufferRef =
    unsafePerformIO $
        newTVarIO
            LogBuffer
                { lbLines = Seq.empty
                , lbOffset = 0
                , lbMaxSize = 1000
                }

{- | Per-thread log scopes. 'reportProgress' reads the calling thread's entry
to tag the line; threads outside any 'withLogScope' stay untagged.
-}
{-# NOINLINE logScopesRef #-}
logScopesRef :: TVar (M.Map ThreadId Text)
logScopesRef = unsafePerformIO $ newTVarIO M.empty

{- | Tag every log line the current thread emits during the action with the
given scope (a database name). The outermost scope wins: a dependency loaded
inside another database's load keeps reporting under the database the caller
named, because that is the load being watched.
-}
withLogScope :: Text -> IO a -> IO a
withLogScope scope action = do
    tid <- myThreadId
    existing <- M.lookup tid <$> readTVarIO logScopesRef
    case existing of
        Just _ -> action
        Nothing ->
            bracket_
                (atomically $ modifyTVar' logScopesRef (M.insert tid scope))
                (atomically $ modifyTVar' logScopesRef (M.delete tid))
                action

{- | Capture the calling thread's log scope as a wrapper for worker actions.
Scopes are per-thread, so a forked worker starts unscoped - wrapping its action
with what this returns keeps its lines attributed to the same database.
-}
inheritLogScope :: IO (IO a -> IO a)
inheritLogScope = do
    tid <- myThreadId
    scope <- M.lookup tid <$> readTVarIO logScopesRef
    pure (maybe id withLogScope scope)

-- | Append a line to the global log buffer
appendLogLine :: LogLine -> IO ()
appendLogLine line = atomically $ modifyTVar' logBufferRef $ \buf ->
    let newLines = lbLines buf Seq.|> line
        len = Seq.length newLines
        maxSz = lbMaxSize buf
     in if len > maxSz
            then
                let drop' = len - maxSz
                 in buf
                        { lbLines = Seq.drop drop' newLines
                        , lbOffset = lbOffset buf + drop'
                        }
            else buf{lbLines = newLines}

{- | Get log lines since a given absolute index.
Returns (nextIndex, newLines).
-}
getLogLines :: Int -> IO (Int, [LogLine])
getLogLines since = do
    buf <- readTVarIO logBufferRef
    let nextIndex = lbOffset buf + Seq.length (lbLines buf)
        startInSeq = max 0 (since - lbOffset buf)
    return (nextIndex, foldr (:) [] (Seq.drop startInSeq (lbLines buf)))

{- | Block until new lines appear after the given index.
Returns (nextIndex, newLines). Uses STM retry for efficient blocking.
-}
waitForNewLines :: Int -> IO (Int, [LogLine])
waitForNewLines since = atomically $ do
    buf <- readTVar logBufferRef
    let nextIndex = lbOffset buf + Seq.length (lbLines buf)
    if nextIndex <= since
        then retry
        else do
            let startInSeq = max 0 (since - lbOffset buf)
            return (nextIndex, foldr (:) [] (Seq.drop startInSeq (lbLines buf)))

-- | Report progress with consistent formatting and thread-safe output
reportProgress :: ProgressLevel -> String -> IO ()
reportProgress level message = do
    let prefix = case level of
            Info -> ""
            Warning -> "[WARN] "
            Cache -> "[CACHE] "
            Matrix -> "[MATRIX] "
            Solver -> "[SOLVER] "
            Error -> "[ERROR] "
        formatted = prefix ++ message
    -- Append to in-memory buffer, tagged with the thread's scope
    tid <- myThreadId
    scope <- M.lookup tid <$> readTVarIO logScopesRef
    appendLogLine (LogLine scope formatted)
    -- Serialize all output to prevent thread-unsafe stderr corruption
    -- Catch encoding errors (safety net for Windows code page issues)
    withMVar progressOutputMutex $ \_ ->
        catch
            (hPutStrLn stderr formatted >> hFlush stderr)
            (\(_ :: SomeException) -> return ())

-- | Report progress with timing information
reportProgressWithTiming :: ProgressLevel -> String -> Double -> IO ()
reportProgressWithTiming level operation duration = do
    let message = operation ++ " (" ++ formatDuration duration ++ ")"
    reportProgress level message

-- | Report error messages
reportError :: String -> IO ()
reportError = reportProgress Error

-- | Report cache-related operations
reportCacheOperation :: String -> IO ()
reportCacheOperation = reportProgress Cache

-- | Report matrix construction operations
reportMatrixOperation :: String -> IO ()
reportMatrixOperation = reportProgress Matrix

-- | Report matrix solver operations
reportSolverOperation :: String -> IO ()
reportSolverOperation = reportProgress Solver

-- | Execute an operation with automatic timing and progress reporting
withProgressTiming :: ProgressLevel -> String -> IO a -> IO a
withProgressTiming level operationName action = do
    reportProgress level $ operationName ++ "..."
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    reportProgressWithTiming level operationName duration
    return result

-- | Format duration in seconds with appropriate precision
formatDuration :: Double -> String
formatDuration duration
    | duration < 0.001 = printf "%.2fms" (duration * 1000)
    | duration < 1.0 = printf "%.0fms" (duration * 1000)
    | duration < 60.0 = printf "%.2fs" duration
    | otherwise = printf "%.1fmin" (duration / 60)

-- | Report current memory usage (if RTS stats are enabled)
reportMemoryUsage :: String -> IO ()
reportMemoryUsage operation = do
    result <- try getRTSStats :: IO (Either SomeException RTSStats)
    case result of
        Right stats -> do
            let memoryBytes = fromIntegral (allocated_bytes stats) :: Double
            reportProgress Info $ operation ++ " - Memory allocated: " ++ formatBytes memoryBytes
        Left _ ->
            reportProgress Info $ operation ++ " - Memory stats disabled (use +RTS -T to enable)"

-- | Format bytes in human-readable format
formatBytes :: Double -> String
formatBytes bytes
    | bytes < 1024 = printf "%.0f B" bytes
    | bytes < 1024 * 1024 = printf "%.1f KB" (bytes / 1024)
    | bytes < 1024 * 1024 * 1024 = printf "%.1f MB" (bytes / (1024 * 1024))
    | otherwise = printf "%.2f GB" (bytes / (1024 * 1024 * 1024))

-- | Report cache file information
reportCacheInfo :: FilePath -> IO ()
reportCacheInfo cacheFile = do
    exists <- doesFileExist cacheFile
    when exists $ do
        size <- getFileSize cacheFile
        reportCacheOperation $ "Cache file: " ++ cacheFile ++ " (" ++ formatBytes (fromIntegral size) ++ ")"

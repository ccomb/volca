{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay, myThreadId, throwTo, ThreadId)
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure, die, ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- VoLCA imports
import API.Auth (authMiddleware)
import CLI.Client (resolveRemoteConfig, executeRemoteCommand)
import CLI.Repl (runRepl)
import CLI.Command (executeCommand)
import CLI.Parser (cliParserInfo)
import CLI.Types
import Config (loadConfig, Config(..), ServerConfig(..), DatabaseConfig(..))
import Database.Manager (initDatabaseManager, DatabaseManager(..))
import Matrix (initializePetscForServer)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Progress
import Control.Concurrent.STM (readTVarIO)

-- For server mode
import API.Routes (lcaAPI, lcaServer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hCacheControl, hContentType, hPragma)
import Network.Wai (Application, Request (..), Response, ResponseReceived, rawPathInfo, rawQueryString, requestMethod, requestHeaders, pathInfo, mapResponseHeaders, responseStream, responseLBS)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices, staticApp)
import Network.Wai.Handler.Warp (runSettings, setPort, setTimeout, defaultSettings)
import Servant (serve)
import WaiAppStatic.Types (unsafeToPiece, MaxAge(..), ssMaxAge)
import Data.String (fromString)

-- | Main entry point
main :: IO ()
main = do
  cliConfig <- execParser cliParserInfo
  validateCLIConfig cliConfig

  case (CLI.Types.command cliConfig, configFile (globalOptions cliConfig)) of
    (Just (Server serverOpts), Just cfgFile) -> runServerWithConfig cliConfig serverOpts cfgFile
    (Just Repl, Just cfgFile)                -> runReplMode cliConfig cfgFile
    (Just cmd, Just cfgFile) | isLocalCommand cmd -> runCLIWithConfig cliConfig cmd cfgFile
    (Just cmd, Just cfgFile)                 -> runCLIViaAPI cliConfig cmd cfgFile
    (Nothing, Just cfgFile)                  -> runConfigLoadOnly cliConfig cfgFile
    (Just Stop, Nothing)                     -> runStopWithoutConfig cliConfig
    _                                        -> die "--config is required"

-- | Load config or die with error message
loadConfigOrDie :: FilePath -> IO Config
loadConfigOrDie cfgFile = do
  reportProgress Info $ "Loading configuration from: " ++ cfgFile
  configResult <- loadConfig cfgFile
  case configResult of
    Left err -> do
      reportError $ "Failed to load config: " ++ T.unpack err
      exitFailure
    Right config -> return config

-- | Commands that require local database access (not available via HTTP)
isLocalCommand :: Command -> Bool
isLocalCommand (DebugMatrices _ _) = True
isLocalCommand (ExportMatrices _)  = True
isLocalCommand (Plugin _)          = True
isLocalCommand _                   = False

-- | Run local-only CLI commands through DatabaseManager (loads DBs, PETSc)
runCLIWithConfig :: CLIConfig -> Command -> FilePath -> IO ()
runCLIWithConfig cliConfig cmd cfgFile = do
  config <- loadConfigOrDie cfgFile
  initializePetscForServer
  dbManager <- initDatabaseManager config (noCache (globalOptions cliConfig)) (Just cfgFile)
  executeCommand cliConfig cmd dbManager

-- | Run CLI commands via HTTP against a running server (lightweight, no DB loading)
runCLIViaAPI :: CLIConfig -> Command -> FilePath -> IO ()
runCLIViaAPI cliConfig cmd cfgFile = do
  config <- loadConfigOrDie cfgFile
  mgr <- newManager defaultManagerSettings
  rc <- resolveRemoteConfig (globalOptions cliConfig) (Just config)
  executeRemoteCommand mgr rc (globalOptions cliConfig) cmd

-- | Run interactive REPL over HTTP (auto-starts server if needed)
runReplMode :: CLIConfig -> FilePath -> IO ()
runReplMode cliConfig cfgFile = do
  config <- loadConfigOrDie cfgFile
  mgr <- newManager defaultManagerSettings
  rc <- resolveRemoteConfig (globalOptions cliConfig) (Just config)
  runRepl mgr rc (globalOptions cliConfig) cfgFile

-- | Run stop without config — resolveRemoteConfig falls back to env vars / defaults
runStopWithoutConfig :: CLIConfig -> IO ()
runStopWithoutConfig cliConfig = do
    mgr <- newManager defaultManagerSettings
    rc <- resolveRemoteConfig (globalOptions cliConfig) Nothing
    executeRemoteCommand mgr rc (globalOptions cliConfig) Stop

-- | Run server with multi-database configuration file
runServerWithConfig :: CLIConfig -> ServerOptions -> FilePath -> IO ()
runServerWithConfig cliConfig serverOpts cfgFile = do
  config <- loadConfigOrDie cfgFile

  -- Apply --load override if specified
  let effectiveConfig = case serverLoadDbs serverOpts of
        Nothing -> config
        Just dbNames -> config { cfgDatabases = map (overrideLoad dbNames) (cfgDatabases config) }

  -- Initialize DatabaseManager (pre-loads databases with load=true)
  reportProgress Info "Initializing database manager..."
  dbManager <- initDatabaseManager effectiveConfig (noCache (globalOptions cliConfig)) (Just cfgFile)

  -- Log database status (allow starting with no databases for BYOL mode)
  loadedDbs <- readTVarIO (dmLoadedDbs dbManager)
  if M.null loadedDbs
    then reportProgress Info "No databases loaded - upload or load one via the web interface"
    else reportProgress Info $ "Loaded databases: " ++ intercalate ", " (map T.unpack (M.keys loadedDbs))

  let port = serverPort serverOpts

  -- Initialize PETSc/MPI context (also restores SIGPIPE handler on Unix)
  reportProgress Info "Initializing PETSc/MPI for persistent matrix operations"
  initializePetscForServer
  -- Flush stdout after PETSc init: PETSc's C printf may leave data in the
  -- C buffer when stdout is piped (block-buffered), which blocks the Tauri
  -- desktop launcher waiting for the next line.
  hFlush stdout

  -- Get password from CLI, config, or env var
  password <- case CLI.Types.serverPassword (globalOptions cliConfig) of
    Just pwd -> return (Just pwd)
    Nothing -> case scPassword (cfgServer effectiveConfig) of
      Just pwd -> return (Just $ T.unpack pwd)
      Nothing -> lookupEnv "VOLCA_PASSWORD"

  -- Determine static directory (--static-dir or default "web/dist")
  let staticDir = fromMaybe "web/dist" (serverStaticDir serverOpts)
      desktopMode = serverDesktopMode serverOpts

  -- In desktop mode, print machine-readable port for launcher, then minimal logging
  if desktopMode
    then do
      putStrLn $ "VOLCA_PORT=" ++ show port
      hFlush stdout
    else do
      reportProgress Info $ "Starting API server on port " ++ show port
      reportProgress Info $ "Tree depth: " ++ show (treeDepth (globalOptions cliConfig))
      case password of
        Just _ -> reportProgress Info "Authentication: ENABLED"
        Nothing -> reportProgress Info "Authentication: DISABLED (use --password or VOLCA_PASSWORD to enable)"
      reportProgress Info $ "Web interface available at: http://localhost:" ++ show port ++ "/"

  -- Idle timeout: track last request time, watchdog activated on demand via API
  mainTid <- myThreadId
  lastRequestRef <- newIORef =<< getCurrentTime
  idleActiveRef <- newIORef False

  -- If --idle-timeout is set, activate immediately (for scripts)
  let idleTimeout = serverIdleTimeout serverOpts
  when (idleTimeout > 0) $ do
      reportProgress Info $ "Idle timeout: " ++ show idleTimeout ++ "s"
      writeIORef idleActiveRef True
      _ <- forkIO $ idleWatchdog mainTid lastRequestRef idleActiveRef idleTimeout
      pure ()

  -- Create app with DatabaseManager - API handlers fetch current DB dynamically
  let baseApp = Main.createServerApp dbManager (treeDepth (globalOptions cliConfig)) staticDir desktopMode password
      appWithIdleAndShutdown = idleTrackingMiddleware lastRequestRef
          $ shutdownEndpoint mainTid lastRequestRef idleActiveRef baseApp
      finalApp = case password of
        Just pwd -> authMiddleware (C8.pack pwd) appWithIdleAndShutdown
        Nothing -> appWithIdleAndShutdown
      settings = setTimeout 600 $ setPort port defaultSettings
  runSettings settings finalApp

-- | Run config load-only mode (load all databases from config and exit)
-- Useful for cache generation, validation, and benchmarking
runConfigLoadOnly :: CLIConfig -> FilePath -> IO ()
runConfigLoadOnly cliConfig cfgFile = do
  config <- loadConfigOrDie cfgFile

  -- Initialize DatabaseManager (pre-loads databases with load=true)
  reportProgress Info "Loading all databases from config..."
  _dbManager <- initDatabaseManager config (noCache (globalOptions cliConfig)) (Just cfgFile)

  -- Report success
  let loadCount = length $ filter dcLoad (cfgDatabases config)
  reportProgress Info $ "No command specified - " ++ show loadCount ++ " database(s) loaded and cached"
  reportProgress Info "Cache files ready for deployment"

-- | Override load flag for databases based on --load CLI option
overrideLoad :: [T.Text] -> DatabaseConfig -> DatabaseConfig
overrideLoad dbNames dbConfig =
    dbConfig { dcLoad = dcName dbConfig `elem` dbNames }

-- | Create a Wai application with DatabaseManager
createServerApp :: DatabaseManager -> Int -> FilePath -> Bool -> Maybe String -> Application
createServerApp dbManager maxTreeDepth staticDir desktopMode password req respond = do
  let path = rawPathInfo req
      qs = rawQueryString req
      fullUrl = path <> qs

  -- Simple request logging (suppress in desktop mode)
  unless desktopMode $ do
    putStrLn $ C8.unpack (requestMethod req) ++ " " ++ C8.unpack fullUrl
    hFlush stdout

  -- Route requests based on path prefix
  if path == "/api/v1/logs/stream"
    then handleLogStream req respond
    else if C8.pack "/api/" `BS.isPrefixOf` path
    then
      serve lcaAPI (lcaServer dbManager maxTreeDepth password) req respond
    else if C8.pack "/static/" `BS.isPrefixOf` path
      then
        let strippedPath = BS.drop 7 path
            originalPathInfo = pathInfo req
            newPathInfo = case originalPathInfo of
              (segment:rest) | segment == T.pack "static" -> rest
              other -> other
            staticReq = req { rawPathInfo = strippedPath, pathInfo = newPathInfo }
            staticSettings = (defaultWebAppSettings staticDir)
              { ssIndices = [unsafeToPiece (T.pack "index.html")]
              , ssMaxAge = NoMaxAge
              }
         in staticApp staticSettings staticReq respond
      else
        let staticSettings = (defaultWebAppSettings staticDir)
              { ssIndices = [unsafeToPiece (T.pack "index.html")]
              , ssMaxAge = NoMaxAge
              }
            indexReq = req { rawPathInfo = C8.pack "/", pathInfo = [] }
            noCacheRespond res = respond $ mapResponseHeaders
              (\hs -> (hCacheControl, C8.pack "no-cache, no-store, must-revalidate")
                    : (hPragma, C8.pack "no-cache")
                    : hs) res
         in staticApp staticSettings indexReq noCacheRespond

-- | SSE endpoint for real-time log streaming
handleLogStream :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogStream req respond = do
    let lastEventId = lookup "Last-Event-ID" (requestHeaders req)
        since = maybe 0 (\v -> fromMaybe 0 (readMaybe (C8.unpack v))) lastEventId
    respond $ responseStream status200
        [ (hContentType, "text/event-stream")
        , (hCacheControl, "no-cache")
        , ("X-Accel-Buffering", "no")
        ] $ \write flush -> do
            let loop !cursor = do
                    (!nextIdx, newLines) <- waitForNewLines cursor
                    forM_ newLines $ \line ->
                        write (fromString $ "id:" ++ show nextIdx ++ "\ndata:" ++ line ++ "\n\n")
                    flush
                    loop nextIdx
            loop since

-- | Validate CLI configuration for consistency
validateCLIConfig :: CLIConfig -> IO ()
validateCLIConfig (CLIConfig globalOpts _) =
  case (format globalOpts, jsonPath globalOpts) of
    (Just CSV, Nothing) ->
      die "--format csv requires --jsonpath. Examples: --jsonpath 'srResults', --jsonpath 'piActivity.pfaExchanges'"
    (Just fmt, Just _) | fmt /= CSV ->
      die "--jsonpath can only be used with --format csv"
    _ -> pure ()

-- | WAI middleware that updates last-request timestamp on every request
idleTrackingMiddleware :: IORef UTCTime -> Application -> Application
idleTrackingMiddleware ref app req respond = do
    getCurrentTime >>= writeIORef ref
    app req respond

-- | Middleware that handles POST /api/v1/idle-timeout/{seconds} and POST /api/v1/shutdown
-- 0 = cancel timeout, N>0 = activate/restart idle watchdog
shutdownEndpoint :: ThreadId -> IORef UTCTime -> IORef Bool -> Application -> Application
shutdownEndpoint mainTid lastRequestRef idleActiveRef app req respond = do
    let path = rawPathInfo req
        method = requestMethod req
    case (method, BS.stripPrefix "/api/v1/idle-timeout/" path, path) of
        ("POST", _, "/api/v1/shutdown") -> do
            reportProgress Info "Shutdown requested via API"
            _ <- forkIO $ threadDelay 500000 >> throwTo mainTid ExitSuccess
            respond $ responseLBS status200
                [(hContentType, "application/json")]
                "{\"ok\":true}"
        ("POST", Just secondsBS, _) -> do
            let seconds = fromMaybe 30 (readMaybe (C8.unpack secondsBS)) :: Int
            if seconds <= 0
                then do
                    writeIORef idleActiveRef False
                    reportProgress Info "Idle timeout cancelled"
                else do
                    alreadyActive <- readIORef idleActiveRef
                    writeIORef idleActiveRef True
                    getCurrentTime >>= writeIORef lastRequestRef
                    unless alreadyActive $ do
                        _ <- forkIO $ idleWatchdog mainTid lastRequestRef idleActiveRef seconds
                        pure ()
                    reportProgress Info $ "Idle timeout: " ++ show seconds ++ "s"
            respond $ responseLBS status200
                [(hContentType, "application/json")]
                "{\"ok\":true}"
        (_, _, _) -> app req respond

-- | Background thread that exits the server after idle timeout (in seconds)
idleWatchdog :: ThreadId -> IORef UTCTime -> IORef Bool -> Int -> IO ()
idleWatchdog mainTid ref activeRef timeoutSecs = go
  where
    checkInterval = min (timeoutSecs * 1000000) (5 * 1000000)  -- check every 5s or timeout, whichever is shorter
    go = do
        threadDelay checkInterval
        active <- readIORef activeRef
        if not active then pure ()
        else do
            now <- getCurrentTime
            lastReq <- readIORef ref
            let idleSeconds = realToFrac (diffUTCTime now lastReq) :: Double
            if idleSeconds >= fromIntegral timeoutSecs
                then do
                    reportProgress Info $ "Idle for " ++ show timeoutSecs ++ "s, shutting down."
                    throwTo mainTid ExitSuccess
                else go

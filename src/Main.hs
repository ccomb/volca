{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, unless)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure, die)
import System.IO (hFlush, stdout)
#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, Handler(Ignore), sigPIPE)
#endif
import Text.Read (readMaybe)

-- fpLCA imports
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
import Network.Wai (Application, Request (..), Response, ResponseReceived, rawPathInfo, rawQueryString, requestMethod, requestHeaders, pathInfo, mapResponseHeaders, responseStream)
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
  rc <- resolveRemoteConfig (globalOptions cliConfig) config
  executeRemoteCommand mgr rc (globalOptions cliConfig) cmd

-- | Run interactive REPL over HTTP (auto-starts server if needed)
runReplMode :: CLIConfig -> FilePath -> IO ()
runReplMode cliConfig cfgFile = do
  config <- loadConfigOrDie cfgFile
  mgr <- newManager defaultManagerSettings
  rc <- resolveRemoteConfig (globalOptions cliConfig) config
  runRepl mgr rc (globalOptions cliConfig) cfgFile

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

  -- Install SIGPIPE handler (Unix only - not needed on Windows)
#ifndef mingw32_HOST_OS
  reportProgress Info "Installing SIGPIPE handler to prevent client disconnect crashes"
  _ <- installHandler sigPIPE Ignore Nothing
#endif

  -- Initialize PETSc/MPI context
  reportProgress Info "Initializing PETSc/MPI for persistent matrix operations"
  initializePetscForServer

  -- Get password from CLI, config, or env var
  password <- case CLI.Types.serverPassword (globalOptions cliConfig) of
    Just pwd -> return (Just pwd)
    Nothing -> case scPassword (cfgServer effectiveConfig) of
      Just pwd -> return (Just $ T.unpack pwd)
      Nothing -> lookupEnv "FPLCA_PASSWORD"

  -- Determine static directory (--static-dir or default "web/dist")
  let staticDir = fromMaybe "web/dist" (serverStaticDir serverOpts)
      desktopMode = serverDesktopMode serverOpts

  -- In desktop mode, print machine-readable port for launcher, then minimal logging
  if desktopMode
    then do
      putStrLn $ "FPLCA_PORT=" ++ show port
      hFlush stdout
    else do
      reportProgress Info $ "Starting API server on port " ++ show port
      reportProgress Info $ "Tree depth: " ++ show (treeDepth (globalOptions cliConfig))
      case password of
        Just _ -> reportProgress Info "Authentication: ENABLED"
        Nothing -> reportProgress Info "Authentication: DISABLED (use --password or FPLCA_PASSWORD to enable)"
      reportProgress Info $ "Web interface available at: http://localhost:" ++ show port ++ "/"

  -- Create app with DatabaseManager - API handlers fetch current DB dynamically
  let baseApp = Main.createServerApp dbManager (treeDepth (globalOptions cliConfig)) staticDir desktopMode password
      finalApp = case password of
        Just pwd -> authMiddleware (C8.pack pwd) baseApp
        Nothing -> baseApp
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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Foreign.C.Types (CInt (..))
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hFlush, stderr, stdout)
import Text.Read (readMaybe)

-- VoLCA imports
import API.Auth (authMiddleware)
import CLI.Client (executeRemoteCommand, resolveRemoteConfig)
import CLI.Command (executeCommand)
import CLI.Parser (cliParserInfo)
import CLI.Repl (runRepl)
import CLI.Types
import Config (ClassificationPreset, Config (..), DatabaseConfig (..), HostingConfig (..), ServerConfig (..), loadConfig)
import Control.Concurrent.STM (readTVarIO)
import Database.Manager (DatabaseManager (..), initDatabaseManager)
import Matrix (initializeSolverForServer)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Progress

-- For server mode
import API.MCP (mcpApp, toolDefinitions)
import API.Routes (lcaAPI, lcaServer, volcaOpenApi)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.String (fromString)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hCacheControl, hContentType, hPragma)
import Network.Wai (Application, Request (..), Response, ResponseReceived, mapResponseHeaders, pathInfo, rawPathInfo, rawQueryString, requestHeaders, requestMethod, responseLBS, responseStream)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices, staticApp)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Servant (serve)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge, unsafeToPiece)

-- _exit(0) bypasses Haskell RTS teardown — necessary on statically-linked
-- glibc builds (notably aarch64) where the threaded RTS's shutdown calls
-- pthread_cancel, which in turn dlopen()'s libgcc_s.so.1 to find the
-- stack unwinder and SIGILLs when that returns NULL in a static binary.
-- The 500 ms delay before calling this gives Warp time to flush the HTTP
-- response back to the caller; hFlush flushes any buffered log lines.
foreign import ccall "_exit" c_exit :: CInt -> IO ()

hardExit :: IO ()
hardExit = do
    hFlush stdout
    hFlush stderr
    c_exit 0

-- | Main entry point
main :: IO ()
main = do
    cliConfig <- execParser cliParserInfo
    validateCLIConfig cliConfig

    case (CLI.Types.command cliConfig, configFile (globalOptions cliConfig)) of
        (Just DumpOpenApi, _) -> BSL.putStrLn (encode volcaOpenApi)
        (Just DumpMcpTools, _) -> BSL.putStrLn (encode toolDefinitions)
        (Just (Server serverOpts), Just cfgFile) -> runServerWithConfig cliConfig serverOpts cfgFile
        (Just Repl, Just cfgFile) -> runReplMode cliConfig cfgFile
        (Just cmd, Just cfgFile) | isLocalCommand cmd -> runCLIWithConfig cliConfig cmd cfgFile
        (Just cmd, Just cfgFile) -> runCLIViaAPI cliConfig cmd cfgFile
        (Nothing, Just cfgFile) -> runConfigLoadOnly cliConfig cfgFile
        (Just Stop, Nothing) -> runStopWithoutConfig cliConfig
        _ -> die "--config is required"

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
isLocalCommand (ExportMatrices _) = True
isLocalCommand (Plugin _) = True
isLocalCommand _ = False

-- | Run local-only CLI commands through DatabaseManager (loads DBs, matrix solver)
runCLIWithConfig :: CLIConfig -> Command -> FilePath -> IO ()
runCLIWithConfig cliConfig cmd cfgFile = do
    config <- loadConfigOrDie cfgFile
    initializeSolverForServer
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
            Just dbNames -> config{cfgDatabases = map (overrideLoad dbNames) (cfgDatabases config)}

    -- Initialize DatabaseManager (pre-loads databases with load=true)
    reportProgress Info "Initializing database manager..."
    dbManager <- initDatabaseManager effectiveConfig (noCache (globalOptions cliConfig)) (Just cfgFile)

    -- Log database status (allow starting with no databases for BYOL mode)
    loadedDbs <- readTVarIO (dmLoadedDbs dbManager)
    if M.null loadedDbs
        then reportProgress Info "No databases loaded - upload or load one via the web interface"
        else reportProgress Info $ "Loaded databases: " ++ intercalate ", " (map T.unpack (M.keys loadedDbs))

    let port = serverPort serverOpts

    -- Initialize matrix solver (no-op for MUMPS, kept for API compatibility)
    initializeSolverForServer

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
            reportProgress Info $ "Tree depth: " ++ show (serverTreeDepth serverOpts)
            case password of
                Just _ -> reportProgress Info "Authentication: ENABLED"
                Nothing -> reportProgress Info "Authentication: DISABLED (use --password or VOLCA_PASSWORD to enable)"
            reportProgress Info $ "Web interface available at: http://localhost:" ++ show port ++ "/"

    -- Idle timeout: track last request time, watchdog activated on demand via API
    lastRequestRef <- newIORef =<< getCurrentTime
    idleActiveRef <- newIORef False

    -- If --idle-timeout is set, activate immediately (for scripts)
    let idleTimeout = serverIdleTimeout serverOpts
    when (idleTimeout > 0) $ do
        reportProgress Info $ "Idle timeout: " ++ show idleTimeout ++ "s"
        writeIORef idleActiveRef True
        _ <- forkIO $ idleWatchdog lastRequestRef idleActiveRef idleTimeout
        pure ()

    -- Create app with DatabaseManager - API handlers fetch current DB dynamically
    baseApp <- Main.createServerApp dbManager (serverTreeDepth serverOpts) staticDir desktopMode password (cfgHosting effectiveConfig) (cfgClassificationPresets effectiveConfig)
    let appWithIdleAndShutdown =
            idleTrackingMiddleware lastRequestRef $
                shutdownEndpoint lastRequestRef idleActiveRef baseApp
        finalApp = case password of
            Just pwd -> authMiddleware (C8.pack pwd) appWithIdleAndShutdown
            Nothing -> appWithIdleAndShutdown
        settings = setTimeout 600 $ setPort port defaultSettings
    runSettings settings finalApp

{- | Run config load-only mode (load all databases from config and exit)
Useful for cache generation, validation, and benchmarking
-}
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
    dbConfig{dcLoad = dcName dbConfig `elem` dbNames}

-- | Create a Wai application with DatabaseManager
createServerApp :: DatabaseManager -> Int -> FilePath -> Bool -> Maybe String -> Maybe HostingConfig -> [ClassificationPreset] -> IO Application
createServerApp dbManager maxTreeDepth staticDir desktopMode password hostingConfig filterPresets = do
    mcp <- mcpApp dbManager filterPresets
    let openApiJson = encode volcaOpenApi
        swaggerHtml =
            "<!DOCTYPE html><html><head><title>volca API</title>\
            \<meta charset=\"utf-8\"/>\
            \<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist/swagger-ui.css\">\
            \</head><body>\
            \<div id=\"swagger-ui\"></div>\
            \<script src=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist/swagger-ui-bundle.js\"></script>\
            \<script>SwaggerUIBundle({url:\"/api/v1/openapi.json\",dom_id:\"#swagger-ui\"})</script>\
            \</body></html>"
    return $ \req respond -> do
        let path = rawPathInfo req
            qs = rawQueryString req
            fullUrl = path <> qs

        -- Simple request logging (suppress in desktop mode)
        unless desktopMode $ do
            putStrLn $ C8.unpack (requestMethod req) ++ " " ++ C8.unpack fullUrl
            hFlush stdout

        -- Route requests based on path prefix
        if path == "/mcp"
            then mcp req respond
            else
                if path == "/api/v1/openapi.json"
                    then
                        respond $
                            responseLBS
                                status200
                                [(hContentType, "application/json")]
                                openApiJson
                    else
                        if path == "/api/v1/docs"
                            then
                                respond $
                                    responseLBS
                                        status200
                                        [(hContentType, "text/html; charset=utf-8")]
                                        swaggerHtml
                            else
                                if path == "/api/v1/logs/stream"
                                    then handleLogStream req respond
                                    else
                                        if C8.pack "/api/" `BS.isPrefixOf` path
                                            then
                                                serve lcaAPI (lcaServer dbManager maxTreeDepth password hostingConfig filterPresets) req respond
                                            else
                                                if C8.pack "/static/" `BS.isPrefixOf` path
                                                    then
                                                        let strippedPath = BS.drop 7 path
                                                            originalPathInfo = pathInfo req
                                                            newPathInfo = case originalPathInfo of
                                                                (segment : rest) | segment == T.pack "static" -> rest
                                                                other -> other
                                                            staticReq = req{rawPathInfo = strippedPath, pathInfo = newPathInfo}
                                                            staticSettings =
                                                                (defaultWebAppSettings staticDir)
                                                                    { ssIndices = [unsafeToPiece (T.pack "index.html")]
                                                                    , ssMaxAge = NoMaxAge
                                                                    }
                                                         in staticApp staticSettings staticReq respond
                                                    else
                                                        let staticSettings =
                                                                (defaultWebAppSettings staticDir)
                                                                    { ssIndices = [unsafeToPiece (T.pack "index.html")]
                                                                    , ssMaxAge = NoMaxAge
                                                                    }
                                                            indexReq = req{rawPathInfo = C8.pack "/", pathInfo = []}
                                                            noCacheRespond res =
                                                                respond $
                                                                    mapResponseHeaders
                                                                        ( \hs ->
                                                                            (hCacheControl, C8.pack "no-cache, no-store, must-revalidate")
                                                                                : (hPragma, C8.pack "no-cache")
                                                                                : hs
                                                                        )
                                                                        res
                                                         in staticApp staticSettings indexReq noCacheRespond

-- | SSE endpoint for real-time log streaming
handleLogStream :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogStream req respond = do
    let lastEventId = lookup "Last-Event-ID" (requestHeaders req)
        since = maybe 0 (\v -> fromMaybe 0 (readMaybe (C8.unpack v))) lastEventId
    respond
        $ responseStream
            status200
            [ (hContentType, "text/event-stream")
            , (hCacheControl, "no-cache")
            , ("X-Accel-Buffering", "no")
            ]
        $ \write flush -> do
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
        (Just fmt, Just _)
            | fmt /= CSV ->
                die "--jsonpath can only be used with --format csv"
        _ -> pure ()

-- | WAI middleware that updates last-request timestamp on every request
idleTrackingMiddleware :: IORef UTCTime -> Application -> Application
idleTrackingMiddleware ref app req respond = do
    getCurrentTime >>= writeIORef ref
    app req respond

{- | Middleware that handles POST /api/v1/idle-timeout/{seconds} and POST /api/v1/shutdown
0 = cancel timeout, N>0 = activate/restart idle watchdog
-}
shutdownEndpoint :: IORef UTCTime -> IORef Bool -> Application -> Application
shutdownEndpoint lastRequestRef idleActiveRef app req respond = do
    let path = rawPathInfo req
        method = requestMethod req
    case (method, BS.stripPrefix "/api/v1/idle-timeout/" path, path) of
        ("POST", _, "/api/v1/shutdown") -> do
            reportProgress Info "Shutdown requested via API"
            _ <- forkIO $ threadDelay 500000 >> hardExit
            respond $
                responseLBS
                    status200
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
                        _ <- forkIO $ idleWatchdog lastRequestRef idleActiveRef seconds
                        pure ()
                    reportProgress Info $ "Idle timeout: " ++ show seconds ++ "s"
            respond $
                responseLBS
                    status200
                    [(hContentType, "application/json")]
                    "{\"ok\":true}"
        (_, _, _) -> app req respond

-- | Background thread that exits the server after idle timeout (in seconds)
idleWatchdog :: IORef UTCTime -> IORef Bool -> Int -> IO ()
idleWatchdog ref activeRef timeoutSecs = go
  where
    checkInterval = min (timeoutSecs * 1000000) (5 * 1000000) -- check every 5s or timeout, whichever is shorter
    go = do
        threadDelay checkInterval
        active <- readIORef activeRef
        if not active
            then pure ()
            else do
                now <- getCurrentTime
                lastReq <- readIORef ref
                let idleSeconds = realToFrac (diffUTCTime now lastReq) :: Double
                if idleSeconds >= fromIntegral timeoutSecs
                    then do
                        reportProgress Info $ "Idle for " ++ show timeoutSecs ++ "s, shutting down."
                        hardExit
                    else go

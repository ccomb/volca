{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt (..))
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stderr, stdout)
import Text.Read (readMaybe)

-- VoLCA imports
import API.Auth (authMiddleware)
import App.Idle (IdleState (..), idleTrackingMiddleware, idleWatchdog, newIdleState, stampIdle, whileWorking)
import CLI.Client (executeRemoteCommand, resolveRemoteConfig)
import CLI.Command (executeCommand)
import CLI.Parser (cliParserInfo)
import CLI.Repl (runRepl)
import CLI.Types
import Config (Config (..), DatabaseConfig (..), HostingConfig (..), Listen (..), ReadOnly (..), ServerConfig (..), ServerName, clientHost, configKeys, freePortHost, hostingReadOnly, keyPaths, listenOn, loadConfigOrDefault, readDataVersion, readOnlyRefusalFor)
import Control.Concurrent.STM (readTVarIO)
import Database.Manager (DatabaseManager (..), initDatabaseManager)
import qualified Database.Manager as DM
import Network.HTTP.Client (Manager, defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutNone)
import Progress

-- For server mode

import API.DatabaseHandlers (uploadBodyCeiling)
import API.Licenses (licensesResponse)
import API.MCP (WhileWorking, mcpApp, toolDefinitions)
import API.Routes (lcaAPI, lcaServer, volcaOpenApi)
import App.Env (AppEnv (..))
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.String (fromString)
import Network.HTTP.Types (status200, status403)
import Network.HTTP.Types.Header (hCacheControl, hContentType, hPragma)
import Network.Wai (Application, Middleware, Request (..), Response, ResponseReceived, mapResponseHeaders, pathInfo, rawPathInfo, rawQueryString, requestHeaders, requestMethod, responseLBS, responseStream)
import Network.Wai.Application.Static (StaticSettings, defaultWebAppSettings, ssIndices, staticApp)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettings, runSettingsSocket, setHost, setPort, setTimeout)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Servant (serve)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge, unsafeToPiece)

-- _exit(0) bypasses Haskell RTS teardown, necessary on statically-linked
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
        (Just (Dump target), _) -> BSL.putStrLn (dumpDocument target)
        (Just (Server serverOpts), mCfgFile) -> runServerWithConfig cliConfig serverOpts mCfgFile
        (Just Repl, Just cfgFile) -> runReplMode cliConfig cfgFile
        (Just cmd, Just cfgFile) | isLocalCommand cmd -> runCLIWithConfig cliConfig cmd cfgFile
        (Just cmd, Just cfgFile) -> runCLIViaAPI cliConfig cmd cfgFile
        (Nothing, Just cfgFile) -> runConfigLoadOnly cliConfig cfgFile
        (Just Stop, Nothing) -> runStopWithoutConfig cliConfig
        _ -> die "--config is required"

{- | Load config or die with error message. Without a path the effective
config is the built-in defaults (no databases): 'loadConfigOrDefault'
still validates it and honours VOLCA_DATA_DIR.
-}
loadConfigOrDie :: Maybe FilePath -> IO Config
loadConfigOrDie mCfgFile = do
    reportProgress Info $ case mCfgFile of
        Just cfgFile -> "Loading configuration from: " ++ cfgFile
        Nothing -> "No --config given, running on built-in defaults (no databases)"
    configResult <- loadConfigOrDefault mCfgFile
    case configResult of
        Left err -> do
            reportError $ "Failed to load config: " ++ T.unpack err
            exitFailure
        Right config -> return config

-- | Commands that require local database access (not available via HTTP)
isLocalCommand :: Command -> Bool
isLocalCommand (DebugMatrices _ _) = True
isLocalCommand (ExportMatrices _) = True
isLocalCommand _ = False

-- | What @--no-cache@ asks of every load this run makes.
cachePolicyOf :: CLIConfig -> DM.CachePolicy
cachePolicyOf cliConfig
    | noCache (globalOptions cliConfig) = DM.NoCache
    | otherwise = DM.UseCache

-- | Run local-only CLI commands through DatabaseManager (loads DBs, matrix solver)
runCLIWithConfig :: CLIConfig -> Command -> FilePath -> IO ()
runCLIWithConfig cliConfig cmd cfgFile = do
    config <- loadConfigOrDie (Just cfgFile)
    dbManager <- initDatabaseManager config (cachePolicyOf cliConfig)
    executeCommand cliConfig cmd dbManager

{- | HTTP manager for client-mode commands, with the 30 s default response
timeout lifted: the server legitimately computes for minutes before the first
byte of a large database export or batch scoring arrives.
-}
newClientManager :: IO Manager
newClientManager = newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutNone}

-- | Run CLI commands via HTTP against a running server (lightweight, no DB loading)
runCLIViaAPI :: CLIConfig -> Command -> FilePath -> IO ()
runCLIViaAPI cliConfig cmd cfgFile = do
    config <- loadConfigOrDie (Just cfgFile)
    mgr <- newClientManager
    rc <- resolveRemoteConfig (globalOptions cliConfig) (Just config)
    executeRemoteCommand mgr rc (globalOptions cliConfig) cmd

-- | Run interactive REPL over HTTP (auto-starts server if needed)
runReplMode :: CLIConfig -> FilePath -> IO ()
runReplMode cliConfig cfgFile = do
    config <- loadConfigOrDie (Just cfgFile)
    mgr <- newClientManager
    rc <- resolveRemoteConfig (globalOptions cliConfig) (Just config)
    runRepl mgr rc (globalOptions cliConfig) cfgFile

-- | Run stop without config: resolveRemoteConfig falls back to env vars / defaults
runStopWithoutConfig :: CLIConfig -> IO ()
runStopWithoutConfig cliConfig = do
    mgr <- newClientManager
    rc <- resolveRemoteConfig (globalOptions cliConfig) Nothing
    executeRemoteCommand mgr rc (globalOptions cliConfig) Stop

-- | Apply the --load override (if any) to the in-memory config.
applyLoadOverride :: ServerOptions -> Config -> Config
applyLoadOverride serverOpts config = case serverLoadDbs serverOpts of
    Nothing -> config
    Just dbNames -> config{cfgDatabases = map (overrideLoad dbNames) (cfgDatabases config)}

{- | Warn for each --load name that matches no configured database: the
override silently loads nothing for it, guaranteed when running on the
built-in defaults, which configure no databases at all.
-}
warnUnknownLoadNames :: ServerOptions -> Config -> IO ()
warnUnknownLoadNames serverOpts config =
    mapM_ warn unknown
  where
    known = map dcName (cfgDatabases config)
    unknown = concatMap (filter (`notElem` known)) (serverLoadDbs serverOpts)
    warn name =
        reportProgress Warning $
            "--load " ++ T.unpack name ++ " matches no configured database; nothing will be loaded for it"

-- | Log loaded databases (allows starting with none for BYOL mode).
logLoadedDatabases :: DatabaseManager -> IO ()
logLoadedDatabases dbManager = do
    loadedDbs <- readTVarIO (dmLoadedDbs dbManager)
    reportProgress Info $
        if M.null loadedDbs
            then "No databases loaded - upload or load one via the web interface"
            else "Loaded databases: " ++ intercalate ", " (map T.unpack (M.keys loadedDbs))

{- | Resolve the admin password from CLI flag, config file, or env var, in that
order. Returns 'Nothing' when authentication is disabled (no source set).
-}
resolvePassword :: GlobalOptions -> ServerConfig -> IO (Maybe String)
resolvePassword globalOpts serverCfg = case CLI.Types.serverPassword globalOpts of
    Just pwd -> pure (Just pwd)
    Nothing -> case scPassword serverCfg of
        Just pwd -> pure (Just (T.unpack pwd))
        Nothing -> lookupEnv "VOLCA_PASSWORD"

{- | What a hidden @dump-*@ command writes to stdout. Pure, and answered
before any configuration is read, so these documents describe the engine
itself rather than one installation of it.
-}
dumpDocument :: DumpTarget -> BSL.ByteString
dumpDocument = \case
    DumpOpenApi -> encode volcaOpenApi
    DumpMcpTools -> encode (toolDefinitions (ReadOnly False))
    DumpConfigSchema -> encode (keyPaths configKeys)

{- | In desktop mode, print a machine-readable port line for the launcher
to capture and stay quiet. Otherwise emit the human-facing startup banner,
naming the interface as well as the port: a configuration asking for one the
server cannot bind - @0.0.0.0@ under @--port 0@ - shows up here rather than
being discovered from a connection that never arrives.
-}
logServerStartup :: ServerOptions -> Text -> Int -> Maybe String -> IO ()
logServerStartup serverOpts host port password
    | serverDesktopMode serverOpts = do
        putStrLn ("VOLCA_PORT=" ++ show port)
        hFlush stdout
    | otherwise = do
        reportProgress Info ("Starting API server on " ++ T.unpack host ++ " port " ++ show port)
        reportProgress Info ("Tree depth: " ++ show (serverTreeDepth serverOpts))
        reportProgress Info $ case password of
            Just _ -> "Authentication: ENABLED"
            Nothing -> "Authentication: DISABLED (use --password or VOLCA_PASSWORD to enable)"
        reportProgress Info ("Web interface available at: http://" ++ T.unpack (clientHost host) ++ ":" ++ show port ++ "/")

{- | Allocate the idle-tracking state and fork the watchdog when
@--idle-timeout@ is positive. The state is returned for both the
tracking and the shutdown middleware.
-}
setupIdleTimeout :: ServerOptions -> IO IdleState
setupIdleTimeout serverOpts = do
    idle <- newIdleState
    let idleTimeout = serverIdleTimeout serverOpts
    when (idleTimeout > 0) $ do
        reportProgress Info ("Idle timeout: " ++ show idleTimeout ++ "s")
        writeIORef (idleArmed idle) True
        _ <- forkIO (idleWatchdog idle idleTimeout (shutDownIdle idleTimeout))
        pure ()
    pure idle

-- | Stack idle-tracking, shutdown-endpoint and (optionally) auth middleware.
wrapWithMiddleware :: Maybe String -> Maybe HostingConfig -> IdleState -> Application -> Application
wrapWithMiddleware password mHosting idle baseApp =
    let withIdleAndShutdown =
            idleTrackingMiddleware idle $
                shutdownEndpoint mHosting idle baseApp
     in case password of
            Just pwd -> authMiddleware (C8.pack pwd) withIdleAndShutdown
            Nothing -> withIdleAndShutdown

{- | Reject oversized upload requests at the HTTP layer, before the body is
buffered into memory. The per-request ceiling comes from 'uploadBodyCeiling',
so the unlimited tier is never bounded and only the policy-governed upload
routes are capped. This backstops the in-handler 'checkUploadSize'.
-}
uploadSizeLimitMiddleware :: Maybe HostingConfig -> Middleware
uploadSizeLimitMiddleware hostingConfig =
    requestSizeLimitMiddleware $
        setMaxLengthForRequest
            (pure . uploadBodyCeiling hostingConfig . pathInfo)
            defaultRequestSizeLimitSettings

-- | Run the server: on a configuration file when given, else on built-in defaults.
runServerWithConfig :: CLIConfig -> ServerOptions -> Maybe FilePath -> IO ()
runServerWithConfig cliConfig serverOpts mCfgFile = do
    config <- applyLoadOverride serverOpts <$> loadConfigOrDie mCfgFile
    warnUnknownLoadNames serverOpts config
    reportProgress Info "Initializing database manager..."
    dbManager <- initDatabaseManager config (cachePolicyOf cliConfig)
    logLoadedDatabases dbManager
    let staticDir = fromMaybe "web/dist" (serverStaticDir serverOpts)
    password <- resolvePassword (globalOptions cliConfig) (cfgServer config)
    idle <- setupIdleTimeout serverOpts
    dataVersion <- readDataVersion config
    let env =
            AppEnv
                { aeDbManager = dbManager
                , aeMaxTreeDepth = serverTreeDepth serverOpts
                , aePassword = password
                , aeHostingConfig = cfgHosting config
                , aeClassificationPresets = cfgClassificationPresets config
                , aeDataVersion = dataVersion
                }
    baseApp <-
        createServerApp
            env
            staticDir
            (serverDesktopMode serverOpts)
            (scName (cfgServer config))
            (whileWorking idle)
    let finalApp =
            uploadSizeLimitMiddleware (cfgHosting config) $
                wrapWithMiddleware password (cfgHosting config) idle baseApp
        settings = setTimeout 600 defaultSettings
    case listenOn (serverPort serverOpts) (cfgServer config) of
        ListenOnFreeLoopbackPort -> do
            (boundPort, socket) <- openFreePort
            logServerStartup serverOpts freePortHost boundPort password
            runSettingsSocket settings socket finalApp
        ListenOn host port -> do
            logServerStartup serverOpts host port password
            runSettings (setHost (fromString (T.unpack host)) (setPort port settings)) finalApp

{- | Run config load-only mode (load all databases from config and exit)
Useful for cache generation, validation, and benchmarking
-}
runConfigLoadOnly :: CLIConfig -> FilePath -> IO ()
runConfigLoadOnly cliConfig cfgFile = do
    config <- loadConfigOrDie (Just cfgFile)

    -- Initialize DatabaseManager (pre-loads databases with load=true)
    reportProgress Info "Loading all databases from config..."
    _dbManager <- initDatabaseManager config (cachePolicyOf cliConfig)

    -- Report success
    let loadCount = length $ filter dcLoad (cfgDatabases config)
    reportProgress Info $ "No command specified - " ++ show loadCount ++ " database(s) loaded and cached"
    reportProgress Info "Cache files ready for deployment"

-- | Override load flag for databases based on --load CLI option
overrideLoad :: [T.Text] -> DatabaseConfig -> DatabaseConfig
overrideLoad dbNames dbConfig =
    dbConfig{dcLoad = dcName dbConfig `elem` dbNames}

{- | Swagger-UI shell that pulls the OpenAPI spec from our @/api/v1/openapi.json@
endpoint. Served verbatim from @/api/v1/docs@; constant per build.
-}
swaggerHtml :: BSL.ByteString
swaggerHtml =
    "<!DOCTYPE html><html><head><title>volca API</title>\
    \<meta charset=\"utf-8\"/>\
    \<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist/swagger-ui.css\">\
    \</head><body>\
    \<div id=\"swagger-ui\"></div>\
    \<script src=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist/swagger-ui-bundle.js\"></script>\
    \<script>SwaggerUIBundle({url:\"/api/v1/openapi.json\",dom_id:\"#swagger-ui\"})</script>\
    \</body></html>"

{- | Serve the Elm SPA bundle from @staticDir@, with the SPA's @index.html@ as
the directory index and no @max-age@ caching headers.
-}
spaStaticSettings :: FilePath -> StaticSettings
spaStaticSettings staticDir =
    (defaultWebAppSettings staticDir)
        { ssIndices = [unsafeToPiece (T.pack "index.html")]
        , ssMaxAge = NoMaxAge
        }

{- | Serve files under @/static/<rest>@ by stripping the prefix and delegating
to wai-app-static.
-}
serveStripped :: StaticSettings -> Application
serveStripped settings req respond =
    let strippedPath = BS.drop 7 (rawPathInfo req)
        newPathInfo = case pathInfo req of
            (segment : rest) | segment == T.pack "static" -> rest
            other -> other
        staticReq = req{rawPathInfo = strippedPath, pathInfo = newPathInfo}
     in staticApp settings staticReq respond

{- | Serve the SPA shell (@index.html@) for any non-API path, with cache-busting
headers so the browser always re-fetches the latest bundle.
-}
serveSpaIndex :: StaticSettings -> Application
serveSpaIndex settings req respond =
    let indexReq = req{rawPathInfo = C8.pack "/", pathInfo = []}
        noCacheRespond res =
            respond $
                mapResponseHeaders
                    ( \hs ->
                        (hCacheControl, C8.pack "no-cache, no-store, must-revalidate")
                            : (hPragma, C8.pack "no-cache")
                            : hs
                    )
                    res
     in staticApp settings indexReq noCacheRespond

{- | Path-based request dispatcher. The fixed endpoints (@/mcp@,
@/api/v1/{openapi.json,licenses,docs,logs/stream}@) match exactly; anything
under @/api/@ goes through Servant; @/static/@ serves bundled assets; the
catch-all hands back the SPA so client-side routing can handle the URL.
-}
dispatchRequest :: FilePath -> Application -> Application -> Application
dispatchRequest staticDir mcp apiApp req respond =
    let path = rawPathInfo req
        settings = spaStaticSettings staticDir
     in if
            | path == "/mcp" -> mcp req respond
            | path == "/api/v1/openapi.json" ->
                respond $ responseLBS status200 [(hContentType, "application/json")] (encode volcaOpenApi)
            | path == "/api/v1/licenses" -> respond licensesResponse
            | path == "/api/v1/docs" ->
                respond $ responseLBS status200 [(hContentType, "text/html; charset=utf-8")] swaggerHtml
            | path == "/api/v1/logs/stream" -> handleLogStream req respond
            | C8.pack "/api/" `BS.isPrefixOf` path -> apiApp req respond
            | C8.pack "/static/" `BS.isPrefixOf` path -> serveStripped settings req respond
            | otherwise -> serveSpaIndex settings req respond

-- | Per-request log line written to stdout (suppressed in desktop mode).
logRequest :: Request -> IO ()
logRequest req = do
    putStrLn $ C8.unpack (requestMethod req) ++ " " ++ C8.unpack (rawPathInfo req <> rawQueryString req)
    hFlush stdout

-- | Create a Wai application over a ready environment.
createServerApp :: AppEnv -> FilePath -> Bool -> Maybe ServerName -> WhileWorking -> IO Application
createServerApp env staticDir desktopMode serverName whileCalling = do
    -- The MCP @web_url@ deep links point at Elm SPA routes served from
    -- 'staticDir'. When the SPA is not bundled (backend-only image), those
    -- URLs would 404, so we omit 'web_url' from MCP responses entirely.
    hasFrontend <- doesFileExist (staticDir </> "index.html")
    unless (desktopMode || hasFrontend) $
        reportProgress Info "Frontend not bundled. MCP responses will omit 'web_url'"
    mcp <- mcpApp (aeDbManager env) (aeClassificationPresets env) hasFrontend (aeHostingConfig env) serverName whileCalling
    let apiApp = serve lcaAPI (lcaServer env)
    pure $ \req respond -> do
        unless desktopMode (logRequest req)
        dispatchRequest staticDir mcp apiApp req respond

-- | SSE endpoint for real-time log streaming
handleLogStream :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogStream req respond = do
    let lastEventId = lookup "Last-Event-ID" (requestHeaders req)
        since = maybe 0 (fromMaybe 0 . readMaybe . C8.unpack) lastEventId
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
                        write
                            ( fromString ("id:" ++ show nextIdx ++ "\ndata:")
                                <> Builder.lazyByteString (encode line)
                                <> fromString "\n\n"
                            )
                    flush
                    loop nextIdx
            loop since

-- | Validate CLI configuration for consistency
validateCLIConfig :: CLIConfig -> IO ()
validateCLIConfig (CLIConfig globalOpts mCmd) =
    case (format globalOpts, jsonPath globalOpts) of
        -- --jsonpath is not required: a response that is an array, or holds
        -- exactly one, needs no naming. CLI.Render asks for the path only when
        -- the choice is genuinely ambiguous, and says so then.
        (Just CSV, Just _)
            | ownCsv ->
                die "--jsonpath does not apply here: the engine renders this report's CSV itself"
        (Just fmt, Just _)
            | fmt /= CSV ->
                die "--jsonpath can only be used with --format csv"
        _ -> pure ()
  where
    ownCsv = maybe False rendersOwnCsv mCmd

{- | Middleware that handles POST /api/v1/idle-timeout/{seconds} and POST /api/v1/shutdown
0 = cancel timeout, N>0 = activate/restart idle watchdog

Both endpoints decide the lifetime of the whole process, so a read-only
instance refuses them: on a server answering many unrelated callers, one of
them must not be able to shut it down under the others.
-}
shutdownEndpoint :: Maybe HostingConfig -> IdleState -> Application -> Application
shutdownEndpoint mHosting idle app req respond =
    case (requestMethod req, BS.stripPrefix "/api/v1/idle-timeout/" path, path) of
        ("POST", _, "/api/v1/shutdown")
            | isReadOnly readOnly -> refuse
            | otherwise -> do
                reportProgress Info "Shutdown requested via API"
                _ <- forkIO $ threadDelay 500000 >> hardExit
                ok
        ("POST", Just secondsBS, _)
            | isReadOnly readOnly -> refuse
            | otherwise -> do
                let seconds = fromMaybe 30 (readMaybe (C8.unpack secondsBS)) :: Int
                if seconds <= 0
                    then do
                        writeIORef (idleArmed idle) False
                        reportProgress Info "Idle timeout cancelled"
                    else do
                        alreadyActive <- readIORef (idleArmed idle)
                        writeIORef (idleArmed idle) True
                        stampIdle idle
                        unless alreadyActive $ do
                            _ <- forkIO $ idleWatchdog idle seconds (shutDownIdle seconds)
                            pure ()
                        reportProgress Info $ "Idle timeout: " ++ show seconds ++ "s"
                ok
        (_, _, _) -> app req respond
  where
    path = rawPathInfo req
    readOnly = hostingReadOnly mHosting
    ok = respond $ responseLBS status200 [(hContentType, "application/json")] "{\"ok\":true}"
    refuse =
        respond $
            responseLBS
                status403
                [(hContentType, "application/json")]
                (encode (object ["error" .= readOnlyRefusalFor mHosting]))

-- | Say why the process is going, then go: the log line is the only trace left.
shutDownIdle :: Int -> IO ()
shutDownIdle timeoutSecs = do
    reportProgress Info $ "Idle for " ++ show timeoutSecs ++ "s, shutting down."
    hardExit

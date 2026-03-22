{-# LANGUAGE OverloadedStrings #-}

module CLI.Repl (runRepl) where

import CLI.Client (RemoteConfig(..), executeRemoteCommand, apiGet, apiPost)
import CLI.Parser (commandParser)
import CLI.Types
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException, bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson
import Data.Aeson (Value)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import qualified Options.Applicative as OA
import System.Console.Haskeline
import System.Environment (getExecutablePath)
import System.IO (hFlush, stdout, openFile, IOMode(..))
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), ProcessHandle)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

-- | REPL session state
data ReplState = ReplState
    { rsDb       :: Maybe Text          -- current --db selection (switchable via "use <db>")
    , rsFormat   :: Maybe OutputFormat   -- current format override (switchable via ":format")
    , rsServerPH :: Maybe ProcessHandle  -- server process we started (Nothing if pre-existing)
    }

-- | Idle timeout in seconds after REPL exits (keeps server warm for quick reconnect)
replIdleTimeoutSeconds :: Int
replIdleTimeoutSeconds = 10

-- | Run the interactive REPL, auto-starting the server if needed
runRepl :: Manager -> RemoteConfig -> GlobalOptions -> FilePath -> IO ()
runRepl mgr rc globalOpts cfgFile = do
    mServerPH <- ensureServer mgr rc globalOpts cfgFile
    stateRef <- newIORef ReplState
        { rsDb = dbName globalOpts
        , rsFormat = Just Table
        , rsServerPH = mServerPH
        }
    -- Cancel any existing idle timeout (in case we're reconnecting to a server with active timeout)
    case mServerPH of
        Just _  -> pure ()
        Nothing -> cancelIdleTimeout mgr rc
    -- Run REPL, activate idle timeout on exit
    bracket (pure ()) (\_ -> activateIdleTimeout mgr rc >> cleanupServer stateRef) $ \_ -> do
        putStrLn "Type :help for available commands, :quit to exit."
        runInputT (setComplete (completionFunc stateRef) defaultSettings) (loop stateRef)
  where
    loop stateRef = do
        st <- liftIO $ readIORef stateRef
        let prompt = "volca" ++ maybe "" (\db -> "[" ++ T.unpack db ++ "]") (rsDb st) ++ "> "
        minput <- getInputLine prompt
        case minput of
            Nothing    -> return ()  -- Ctrl+D
            Just input -> do
                cont <- dispatch stateRef (words input)
                if cont then loop stateRef else return ()

    dispatch _ []          = return True
    dispatch _ [":quit"]   = return False
    dispatch _ [":q"]      = return False
    dispatch _ [":help"]   = liftIO printHelp >> return True
    dispatch _ (":help":_) = liftIO printHelp >> return True
    dispatch _ ["help"]    = liftIO printHelp >> return True

    dispatch stateRef ["use", dbArg] = liftIO $ do
        modifyIORef stateRef $ \s -> s { rsDb = Just (T.pack dbArg) }
        putStrLn $ "Switched to database: " ++ dbArg
        return True

    dispatch stateRef [":format", fmtArg] = liftIO $ case parseOutputFormat fmtArg of
        Just fmt -> do
            modifyIORef stateRef $ \s -> s { rsFormat = Just fmt }
            putStrLn $ "Format: " ++ fmtArg
            return True
        Nothing -> putStrLn "Valid formats: json, pretty, table, csv" >> return True

    dispatch stateRef [":server", "stop"] = liftIO $ do
        alive <- isServerAlive mgr rc
        if alive
            then do
                _ <- try (apiPost mgr rc "/api/v1/shutdown" (Data.Aeson.object [])) :: IO (Either SomeException (Either String Value))
                modifyIORef stateRef $ \s -> s { rsServerPH = Nothing }
                putStrLn "Server stopped."
            else putStrLn "Server is not running."
        return True

    dispatch stateRef [":server", "start"] = liftIO $ do
        alive <- isServerAlive mgr rc
        if alive
            then putStrLn $ "Server already running at " ++ rcBaseUrl rc
            else do
                putStr $ "Starting server at " ++ rcBaseUrl rc ++ "..."
                hFlush stdout
                ph <- startServerProcess globalOpts rc cfgFile
                waitForServer mgr rc 120
                putStrLn " ready."
                modifyIORef stateRef $ \s -> s { rsServerPH = Just ph }
        return True

    dispatch _ [":server", "status"] = liftIO $ do
        alive <- isServerAlive mgr rc
        putStrLn $ if alive
            then "Server running at " ++ rcBaseUrl rc
            else "Server not reachable at " ++ rcBaseUrl rc
        return True

    dispatch stateRef tokens = liftIO $ do
        st <- readIORef stateRef
        let opts = globalOpts { dbName = rsDb st, format = rsFormat st }
        case parseCommand tokens of
            Just cmd -> executeRemoteCommand mgr rc opts cmd
            Nothing  -> putStrLn "Unknown command. Type :help for usage."
        return True

    parseCommand tokens =
        OA.getParseResult $
            OA.execParserPure OA.defaultPrefs (OA.info (commandParser OA.<**> OA.helper) mempty) tokens

    cleanupServer _stateRef = pure ()
    -- Server cleanup is handled by idle timeout — the server shuts itself down
    -- after replIdleTimeoutSeconds of inactivity. This keeps the server warm
    -- if the user opens another REPL session quickly.

-- | Check if the server is reachable; if not, start it and wait.
-- Returns the ProcessHandle if we started it, Nothing if it was already running.
ensureServer :: Manager -> RemoteConfig -> GlobalOptions -> FilePath -> IO (Maybe ProcessHandle)
ensureServer mgr rc globalOpts cfgFile = do
    alive <- isServerAlive mgr rc
    if alive
        then do
            putStrLn $ "Connected to " ++ rcBaseUrl rc
            return Nothing
        else do
            putStr $ "Starting server at " ++ rcBaseUrl rc ++ "..."
            hFlush stdout
            ph <- startServerProcess globalOpts rc cfgFile
            waitForServer mgr rc 120
            putStrLn " ready."
            return (Just ph)

-- | Ping the server (try without auth — any HTTP response means it's up)
isServerAlive :: Manager -> RemoteConfig -> IO Bool
isServerAlive mgr rc = do
    let noAuth = rc { rcAuth = Nothing }
    result <- try (apiGet mgr noAuth "/api/v1/db") :: IO (Either SomeException (Either String Value))
    return $ case result of
        Right (Right _) -> True   -- 2xx response
        Right (Left e)  -> not ("Cannot connect" `isPrefixOf` e)  -- 401/404 = alive, connection error = not
        Left _          -> False  -- unexpected exception

-- | Spawn the server as a background process, logging to a temp file.
-- No --idle-timeout here; the REPL activates it via API on exit.
startServerProcess :: GlobalOptions -> RemoteConfig -> FilePath -> IO ProcessHandle
startServerProcess globalOpts rc cfgFile = do
    exe <- getExecutablePath
    let port = extractPort (rcBaseUrl rc)
        args = ["--config", cfgFile, "server", "--port", show port]
            ++ maybe [] (\p -> ["--password", p]) (serverPassword globalOpts)
    tmpDir <- getTemporaryDirectory
    let logFile = tmpDir </> "volca-server.log"
    logHandle <- openFile logFile AppendMode
    (_, _, _, ph) <- createProcess (proc exe args)
        { std_out = UseHandle logHandle
        , std_err = UseHandle logHandle
        , create_group = True    -- detach from REPL's process group
        , delegate_ctlc = False
        }
    putStr (" log: " ++ logFile ++ " ")
    hFlush stdout
    return ph

-- | Extract port from URL like "http://host:port"
extractPort :: String -> Int
extractPort url =
    case reverse $ takeWhile (/= ':') $ reverse url of
        portStr | all (`elem` ("0123456789" :: String)) portStr
                , not (null portStr) -> read portStr
        _ -> 8081  -- fallback to config default

-- | Poll until the server responds, with dot progress
waitForServer :: Manager -> RemoteConfig -> Int -> IO ()
waitForServer mgr rc remaining
    | remaining <= 0 = putStrLn "\nServer failed to start within timeout."
    | otherwise = do
        threadDelay 500000  -- 0.5s
        alive <- isServerAlive mgr rc
        if alive
            then return ()
            else do
                putChar '.'
                hFlush stdout
                waitForServer mgr rc (remaining - 1)

-- | Tell the server to shut down after idle timeout (called on REPL exit)
activateIdleTimeout :: Manager -> RemoteConfig -> IO ()
activateIdleTimeout mgr rc = do
    let url = "/api/v1/idle-timeout/" ++ show replIdleTimeoutSeconds
    _ <- try (apiPost mgr rc url (Data.Aeson.object [])) :: IO (Either SomeException (Either String Value))
    pure ()

-- | Cancel idle timeout on the server (called on REPL connect)
cancelIdleTimeout :: Manager -> RemoteConfig -> IO ()
cancelIdleTimeout mgr rc = do
    _ <- try (apiPost mgr rc "/api/v1/idle-timeout/0" (Data.Aeson.object [])) :: IO (Either SomeException (Either String Value))
    pure ()

-- | Tab completion for command names and flags
completionFunc :: IORef ReplState -> CompletionFunc IO
completionFunc _stateRef = completeWord Nothing " \t" $ \prefix ->
    return [ simpleCompletion c | c <- allCompletions, prefix `isPrefixOf` c ]

allCompletions :: [String]
allCompletions = commands ++ flags
  where
    commands =
        [ "activity", "tree", "inventory", "flow", "activities", "flows"
        , "lcia", "database", "method", "methods", "synonyms"
        , "compartment-mappings", "units", "mapping"
        , "use", ":format", ":help", ":quit"
        , ":server", "stop", "start", "status"
        ]
    flags =
        [ "--name", "--geo", "--product", "--limit", "--offset"
        , "--query", "--lang", "--method", "--format", "--db"
        , "--matched", "--unmatched", "--uncharacterized"
        , "--depth", "json", "pretty", "table", "csv"
        , "list", "upload", "delete", "activities"
        ]

printHelp :: IO ()
printHelp = do
    putStrLn "Commands:"
    putStrLn "  activity UUID              Activity info"
    putStrLn "  tree UUID [--depth N]      Supply chain tree"
    putStrLn "  inventory UUID             Life cycle inventory"
    putStrLn "  flow FLOW_ID [activities]  Flow info"
    putStrLn "  activities [--name X]      Search activities"
    putStrLn "  flows [--query X]          Search flows"
    putStrLn "  lcia UUID --method M_UUID  LCIA computation"
    putStrLn "  mapping METHOD_UUID        Flow mapping coverage"
    putStrLn "  database [list|upload|delete]"
    putStrLn "  method [list|upload|delete]"
    putStrLn "  methods                    List loaded methods"
    putStrLn "  synonyms                   List synonym sources"
    putStrLn "  units                      List unit definitions"
    putStrLn ""
    putStrLn "Session:"
    putStrLn "  use <db-name>              Switch database"
    putStrLn "  :format json|pretty|table  Switch output format"
    putStrLn "  :server stop               Stop the server"
    putStrLn "  :server start              Start the server"
    putStrLn "  :server status             Check server status"
    putStrLn "  :help                      This help"
    putStrLn "  :quit / Ctrl+D             Exit (stops server)"
    hFlush stdout

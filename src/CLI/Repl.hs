{-# LANGUAGE OverloadedStrings #-}

module CLI.Repl (
    runRepl,

    -- * What a leaving session leaves behind
    ServerOwner (..),
    idleOnExit,
    replIdleTimeoutSeconds,
) where

import CLI.Client (RemoteConfig (..), apiGet, apiPost, executeRemoteCommand)
import CLI.Parser (commandParser)
import CLI.Types
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, withObject, (.:))
import qualified Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import qualified Options.Applicative as OA
import System.Console.Haskeline
import System.Directory (getTemporaryDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), hFlush, openFile, stdout)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc)
import Text.Read (readMaybe)

-- | REPL session state
data ReplState = ReplState
    { rsDb :: Maybe Text -- current --db selection (switchable via "use <db>")
    , rsFormat :: Maybe OutputFormat -- current format override (switchable via ":format")
    , rsServerPH :: Maybe ProcessHandle -- server process we started (Nothing if pre-existing)
    }

-- | Idle timeout in seconds after REPL exits (keeps server warm for quick reconnect)
replIdleTimeoutSeconds :: Int
replIdleTimeoutSeconds = 10

-- | Whether the server a session talked to is the one that session started.
data ServerOwner = StartedByThisRepl | FoundRunning
    deriving (Eq, Show)

{- | Who owns the server, read off the session as it ends rather than as it began:
@:server start@ and @:server stop@ both change the answer mid-session.
-}
ownerOf :: ReplState -> ServerOwner
ownerOf = maybe FoundRunning (const StartedByThisRepl) . rsServerPH

{- | The countdown a leaving session leaves running.

A server this REPL started is its own to end, and gets the grace period: long
enough to reconnect to, short enough not to outlive the session. Any other
server keeps exactly the countdown it was found running, which is none at all
for one started by hand, and that is what leaves it standing.
-}
idleOnExit :: ServerOwner -> Maybe Int -> Maybe Int
idleOnExit StartedByThisRepl _ = Just replIdleTimeoutSeconds
idleOnExit FoundRunning found = found

-- | Run the interactive REPL, auto-starting the server if needed
runRepl :: Manager -> RemoteConfig -> GlobalOptions -> FilePath -> IO ()
runRepl mgr rc globalOpts cfgFile = do
    mServerPH <- ensureServer mgr rc globalOpts cfgFile
    stateRef <-
        newIORef
            ReplState
                { rsDb = dbName globalOpts
                , rsFormat = Just Table
                , rsServerPH = mServerPH
                }
    -- Any countdown is held off while the REPL is connected, since a prompt
    -- nobody is typing at reads as inactivity, and 'idleOnExit' says what to
    -- leave running in its place.
    found <- cancelIdleTimeout mgr rc
    bracket (pure ()) (\_ -> restoreIdleTimeout stateRef found) $ \_ -> do
        putStrLn "Type :help for available commands, :quit to exit."
        runInputT (setComplete (completionFunc stateRef) defaultSettings) (loop stateRef)
  where
    -- \| Leave the server counting down whatever 'idleOnExit' names, or nothing.
    restoreIdleTimeout :: IORef ReplState -> Maybe Int -> IO ()
    restoreIdleTimeout stateRef found = do
        owner <- ownerOf <$> readIORef stateRef
        mapM_ (activateIdleTimeout mgr rc) (idleOnExit owner found)

    loop stateRef = do
        st <- liftIO $ readIORef stateRef
        let prompt = "volca" ++ maybe "" (\db -> "[" ++ T.unpack db ++ "]") (rsDb st) ++ "> "
        minput <- getInputLine prompt
        case minput of
            Nothing -> return () -- Ctrl+D
            Just input -> do
                cont <- dispatch stateRef (words input)
                when cont $ loop stateRef

    dispatch _ [] = return True
    dispatch _ [":quit"] = return False
    dispatch _ [":q"] = return False
    dispatch _ [":help"] = liftIO printHelp >> return True
    dispatch _ (":help" : _) = liftIO printHelp >> return True
    dispatch _ ["help"] = liftIO printHelp >> return True
    dispatch stateRef ["use", dbArg] = liftIO $ do
        modifyIORef stateRef $ \s -> s{rsDb = Just (T.pack dbArg)}
        putStrLn $ "Switched to database: " ++ dbArg
        return True
    dispatch stateRef [":format", fmtArg] = liftIO $ case parseOutputFormat fmtArg of
        Just fmt -> do
            modifyIORef stateRef $ \s -> s{rsFormat = Just fmt}
            putStrLn $ "Format: " ++ fmtArg
            return True
        Nothing -> putStrLn "Valid formats: json, pretty, table, csv" >> return True
    dispatch stateRef [":server", "stop"] = liftIO $ do
        alive <- isServerAlive mgr rc
        if alive
            then do
                _ <- try (apiPost mgr rc "/api/v1/shutdown" (Data.Aeson.object [])) :: IO (Either SomeException (Either String Value))
                modifyIORef stateRef $ \s -> s{rsServerPH = Nothing}
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
                modifyIORef stateRef $ \s -> s{rsServerPH = Just ph}
        return True
    dispatch _ [":server", "status"] = liftIO $ do
        alive <- isServerAlive mgr rc
        putStrLn $
            if alive
                then "Server running at " ++ rcBaseUrl rc
                else "Server not reachable at " ++ rcBaseUrl rc
        return True
    dispatch stateRef tokens = liftIO $ do
        st <- readIORef stateRef
        let opts = globalOpts{dbName = rsDb st, format = rsFormat st}
        case OA.execParserPure OA.defaultPrefs (OA.info (commandParser OA.<**> OA.helper) mempty) tokens of
            OA.Success cmd -> executeRemoteCommand mgr rc opts cmd
            OA.CompletionInvoked _ -> putStrLn unknownCommand
            -- A parser answers --help by failing with the help text and an
            -- exit code of zero. Reading only the success case, as
            -- getParseResult does, made every --help here read as a command
            -- nobody knows - in the one place a user types it by reflex.
            OA.Failure failure -> case OA.renderFailure failure "volca" of
                (helpText, ExitSuccess) -> putStrLn helpText
                (_, ExitFailure _) -> putStrLn unknownCommand
        return True

    unknownCommand = "Unknown command. Type :help for usage."

{- | Check if the server is reachable; if not, start it and wait.
Returns the ProcessHandle if we started it, Nothing if it was already running.
-}
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

-- | Ping the server (try without auth – any HTTP response means it's up)
isServerAlive :: Manager -> RemoteConfig -> IO Bool
isServerAlive mgr rc = do
    let noAuth = rc{rcAuth = Nothing}
    result <- try (apiGet mgr noAuth "/api/v1/db") :: IO (Either SomeException (Either String Value))
    return $ case result of
        Right (Right _) -> True -- 2xx response
        Right (Left e) -> not ("Cannot connect" `isPrefixOf` e) -- 401/404 = alive, connection error = not
        Left _ -> False -- unexpected exception

{- | Spawn the server as a background process, logging to a temp file.
No --idle-timeout here; the REPL activates it via API on exit.
-}
startServerProcess :: GlobalOptions -> RemoteConfig -> FilePath -> IO ProcessHandle
startServerProcess globalOpts rc cfgFile = do
    exe <- getExecutablePath
    let port = extractPort (rcBaseUrl rc)
        args =
            ["--config", cfgFile, "server", "--port", show port]
                ++ maybe [] (\p -> ["--password", p]) (serverPassword globalOpts)
    tmpDir <- getTemporaryDirectory
    let logFile = tmpDir </> "volca-server.log"
    logHandle <- openFile logFile AppendMode
    (_, _, _, ph) <-
        createProcess
            (proc exe args)
                { std_out = UseHandle logHandle
                , std_err = UseHandle logHandle
                , create_group = True -- detach from REPL's process group
                , delegate_ctlc = False
                }
    putStr (" log: " ++ logFile ++ " ")
    hFlush stdout
    return ph

-- | Extract port from URL like "http://host:port"
extractPort :: String -> Int
extractPort url =
    case reverse $ takeWhile (/= ':') $ reverse url of
        portStr
            | all (`elem` ("0123456789" :: String)) portStr
            , Just port <- readMaybe portStr ->
                port
        _ -> 8080 -- fallback to config default

-- | Poll until the server responds, with dot progress
waitForServer :: Manager -> RemoteConfig -> Int -> IO ()
waitForServer mgr rc remaining
    | remaining <= 0 = putStrLn "\nServer failed to start within timeout."
    | otherwise = do
        threadDelay 500000 -- 0.5s
        alive <- isServerAlive mgr rc
        if alive
            then return ()
            else do
                putChar '.'
                hFlush stdout
                waitForServer mgr rc (remaining - 1)

-- | Ask the server to shut down after @seconds@ of inactivity.
activateIdleTimeout :: Manager -> RemoteConfig -> Int -> IO ()
activateIdleTimeout mgr rc seconds = do
    let url = "/api/v1/idle-timeout/" ++ show seconds
    _ <- try (apiPost mgr rc url (Data.Aeson.object [])) :: IO (Either SomeException (Either String Value))
    pure ()

{- | Hold off any idle shutdown while the REPL is connected, and report the one
it cancelled so that the exit can put it back. A prompt nobody is typing at
counts as inactivity, so a session left open would otherwise be cut off under
its user.
-}
cancelIdleTimeout :: Manager -> RemoteConfig -> IO (Maybe Int)
cancelIdleTimeout mgr rc = do
    answer <- try (apiPost mgr rc "/api/v1/idle-timeout/0" (Data.Aeson.object [])) :: IO (Either SomeException (Either String Value))
    pure $ case answer of
        Right (Right value) -> cancelledSeconds value
        _ -> Nothing

-- | The timeout a cancellation says it took away, when it took one away.
cancelledSeconds :: Value -> Maybe Int
cancelledSeconds value = case parseMaybe (withObject "cancel" (.: "cancelled")) value of
    Just seconds | seconds > 0 -> Just seconds
    _ -> Nothing

-- | Tab completion for command names and flags
completionFunc :: IORef ReplState -> CompletionFunc IO
completionFunc _stateRef = completeWord Nothing " \t" $ \prefix ->
    return [simpleCompletion c | c <- allCompletions, prefix `isPrefixOf` c]

allCompletions :: [String]
allCompletions = commands ++ flags
  where
    commands =
        [ "activity"
        , "inventory"
        , "flow"
        , "activities"
        , "flows"
        , "impacts"
        , "database"
        , "method"
        , "methods"
        , "synonyms"
        , "compartment-mappings"
        , "units"
        , "flow-mapping"
        , "quality-report"
        , "computed-quality-report"
        , "use"
        , ":format"
        , ":help"
        , ":quit"
        , ":server"
        , "stop"
        , "start"
        , "status"
        ]
    flags =
        [ "--name"
        , "--geo"
        , "--product"
        , "--limit"
        , "--offset"
        , "--query"
        , "--lang"
        , "--method"
        , "--format"
        , "--db"
        , "--matched"
        , "--unmatched"
        , "--uncharacterized"
        , "--collection"
        , "--depth"
        , "json"
        , "pretty"
        , "table"
        , "csv"
        , "list"
        , "upload"
        , "delete"
        , "activities"
        ]

printHelp :: IO ()
printHelp = do
    putStrLn "Commands:"
    putStrLn "  activity UUID               Activity info"
    putStrLn "  inventory UUID              Life cycle inventory"
    putStrLn "  flow FLOW_ID [activities]   Flow info"
    putStrLn "  activities [--name X]       Search activities"
    putStrLn "  flows [--query X]           Search flows"
    putStrLn "  impacts UUID --method M     Impact assessment (LCIA)"
    putStrLn "  flow-mapping METHOD_UUID    Flow mapping coverage"
    putStrLn "  quality-report              What is malformed in the database"
    putStrLn "  computed-quality-report     What the database computes, judged against its own norms"
    putStrLn "  database [list|upload|delete]"
    putStrLn "  method [list|upload|delete]"
    putStrLn "  methods                    List loaded methods"
    putStrLn "  synonyms                   List synonym sources"
    putStrLn "  units                      List unit definitions"
    putStrLn ""
    putStrLn "Session:"
    putStrLn "  use <db-name>              Switch database"
    putStrLn "  :format json|pretty|table|csv  Switch output format"
    putStrLn "  :server stop               Stop the server"
    putStrLn "  :server start              Start the server"
    putStrLn "  :server status             Check server status"
    putStrLn "  :help                      This help"
    putStrLn "  :quit / Ctrl+D             Exit (stops server)"
    hFlush stdout

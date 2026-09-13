{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Client (
    RemoteConfig (..),
    resolveRemoteConfig,
    executeRemoteCommand,
    apiGet,
    apiPost,
    deleteSelectionBody,
    readJsonFile,
) where

import CLI.Render (renderResult)
import CLI.Types
import Config (Config (..), ServerConfig (..), clientHost)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, Value (..), decode, eitherDecode, encode, object, (.:), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither, parseMaybe, withArray, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (..),
    Response,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    setQueryString,
 )
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlDecode)
import Progress (ProgressLevel (Warning), reportError, reportProgress)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hFileSize, withBinaryFile)

-- | Configuration for connecting to a remote VoLCA server
data RemoteConfig = RemoteConfig
    { rcBaseUrl :: String
    , rcAuth :: Maybe String
    }

-- | Resolve server URL and auth from CLI flags, env vars, and config (optional)
resolveRemoteConfig :: GlobalOptions -> Maybe Config -> IO RemoteConfig
resolveRemoteConfig globalOpts mbConfig = do
    url <- case serverUrl globalOpts of
        Just u -> return u
        Nothing -> do
            envUrl <- lookupEnv "VOLCA_URL"
            case envUrl of
                Just u -> return u
                Nothing -> case cfgServer <$> mbConfig of
                    -- clientHost, not scHost: that setting names the
                    -- interfaces to accept on, and "every interface" is not
                    -- somewhere a client can connect to.
                    Just sc -> return $ "http://" ++ T.unpack (clientHost (scHost sc)) ++ ":" ++ show (scPort sc)
                    Nothing -> do
                        reportError "No server URL: use --config, --url, or VOLCA_URL"
                        exitFailure
    pwd <- case serverPassword globalOpts of
        Just p -> return (Just p)
        Nothing -> case mbConfig >>= scPassword . cfgServer of
            Just p -> return (Just $ T.unpack p)
            Nothing -> lookupEnv "VOLCA_PASSWORD"
    return RemoteConfig{rcBaseUrl = url, rcAuth = pwd}

-- | Execute a CLI command via HTTP against a running server
executeRemoteCommand :: Manager -> RemoteConfig -> GlobalOptions -> Command -> IO ()
executeRemoteCommand mgr rc globalOpts cmd = do
    let fmt = fromMaybe Pretty (format globalOpts)
        jp = jsonPath globalOpts
    case cmd of
        Database DbList ->
            apiGet mgr rc "/api/v1/db" >>= output fmt jp
        Database (DbLoad name) ->
            -- Load endpoint takes no body; the empty object is ignored server-side.
            apiPost mgr rc ("/api/v1/db/" ++ T.unpack name ++ "/load") (object [])
                >>= outputLoad fmt jp
        Database (DbUnload name) ->
            apiPost mgr rc ("/api/v1/db/" ++ T.unpack name ++ "/unload") (object [])
                >>= outputStatus fmt jp "unload"
        Database (DbUpload args) ->
            executeUpload mgr rc fmt jp "/api/v1/db/upload" args
        Database (DbDelete name) ->
            apiDelete mgr rc ("/api/v1/db/" ++ T.unpack name) >>= output fmt jp
        Database (DbDeleteActivities args) ->
            apiPost mgr rc ("/api/v1/db/" ++ T.unpack (ddaDb args) ++ "/delete") (deleteSelectionBody args)
                >>= outputStatus fmt jp "delete"
        Database (DbCopy srcName newName) ->
            -- The copy endpoint takes no body (newName is a path capture); the
            -- empty object is ignored server-side.
            apiPost
                mgr
                rc
                ("/api/v1/db/" ++ T.unpack srcName ++ "/copy/" ++ T.unpack newName)
                (object [])
                >>= outputStatus fmt jp "copy"
        Database (DbRelinkMapping args) -> do
            readResult <- try (TIO.readFile (draMappingCsv args)) :: IO (Either IOException Text)
            case readResult of
                Left _ -> do
                    reportError $ "cannot read mapping file " ++ draMappingCsv args
                    exitFailure
                Right csv ->
                    apiPost
                        mgr
                        rc
                        ("/api/v1/db/" ++ T.unpack (draDb args) ++ "/relink")
                        (relinkMappingBody (draToDep args) csv)
                        >>= output fmt jp
        Database (DbExport args) ->
            executeRemoteExport mgr rc fmt jp args
        Database (DbCreateActivities args) ->
            -- The file is the request body; the server owns validation, so the
            -- client forwards it rather than judging it twice.
            postJsonFile mgr rc fmt jp ("/api/v1/db/" ++ T.unpack (dwaDb args) ++ "/activities") (dwaFile args)
        Database (DbReplaceActivity args) ->
            putJsonFile
                mgr
                rc
                fmt
                jp
                ("/api/v1/db/" ++ T.unpack (daDb args) ++ "/activity/" ++ T.unpack (daProcessId args))
                (daFile args)
        Database (DbEditExchanges args) ->
            postJsonFile
                mgr
                rc
                fmt
                jp
                ("/api/v1/db/" ++ T.unpack (daDb args) ++ "/activity/" ++ T.unpack (daProcessId args) ++ "/exchanges")
                (daFile args)
        Method McList ->
            apiGet mgr rc "/api/v1/method-collections" >>= output fmt jp
        Method (McUpload args) ->
            executeUpload mgr rc fmt jp "/api/v1/method-collections/upload" args
        Method (McDelete name) ->
            apiDelete mgr rc ("/api/v1/method-collections/" ++ T.unpack name) >>= output fmt jp
        Method (McExport args) ->
            executeRemoteMethodExport mgr rc fmt jp args
        Methods ->
            apiGet mgr rc "/api/v1/methods" >>= output fmt jp
        Synonyms ->
            apiGet mgr rc "/api/v1/flow-synonyms" >>= output fmt jp
        CompartmentMappings ->
            apiGet mgr rc "/api/v1/compartment-mappings" >>= output fmt jp
        Units ->
            apiGet mgr rc "/api/v1/units" >>= output fmt jp
        Activity uuid -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/activity/" ++ T.unpack uuid) >>= output fmt jp
        Flow flowId Nothing -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/flow/" ++ T.unpack flowId) >>= output fmt jp
        Flow flowId (Just FlowActivities) -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/flow/" ++ T.unpack flowId ++ "/activities") >>= output fmt jp
        Inventory uuid -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/activity/" ++ T.unpack uuid ++ "/inventory") >>= output fmt jp
        SearchActivities opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let qs =
                    buildQuery
                        [ ("name", T.unpack <$> searchName opts)
                        , ("geo", T.unpack <$> searchGeo opts)
                        , ("product", T.unpack <$> searchProduct opts)
                        , ("limit", show <$> searchLimit opts)
                        , ("offset", show <$> searchOffset opts)
                        ]
            apiGet mgr rc (dbPath db ++ "/activities" ++ qs) >>= output fmt jp
        SearchFlows opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let qs =
                    buildQuery
                        [ ("q", T.unpack <$> searchQuery opts)
                        , ("lang", T.unpack <$> searchLang opts)
                        , ("limit", show <$> searchFlowsLimit opts)
                        , ("offset", show <$> searchFlowsOffset opts)
                        ]
            apiGet mgr rc (dbPath db ++ "/flows" ++ qs) >>= output fmt jp
        Impacts uuid lciaOpts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let methodIdText = lciaMethodId lciaOpts
            mCollection <- lookupMethodCollection mgr rc methodIdText
            case mCollection of
                Nothing -> reportError "Method not found in loaded collections" >> exitFailure
                Just col ->
                    apiGet
                        mgr
                        rc
                        ( dbPath db
                            ++ "/activity/"
                            ++ T.unpack uuid
                            ++ "/impacts/"
                            ++ T.unpack col
                            ++ "/"
                            ++ T.unpack methodIdText
                        )
                        >>= output fmt jp
        FlowMapping opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let methodId = T.unpack (mappingMethodId opts)
            apiGet mgr rc (dbPath db ++ "/method/" ++ methodId ++ "/mapping") >>= output fmt jp
        QualityReport mLimit -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            fetchReport mgr rc fmt jp (dbPath db ++ "/quality-report") (buildQuery [("limit", show <$> mLimit)])
        ComputedQualityReport opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            fetchReport
                mgr
                rc
                fmt
                jp
                (dbPath db ++ "/computed-quality-report")
                (buildQuery [("collection", T.unpack <$> cqoCollection opts), ("limit", show <$> cqoLimit opts)])
        Stop -> do
            result <- apiPost mgr rc "/api/v1/shutdown" (object [])
            case result of
                Right _ -> putStrLn $ "Server at " ++ rcBaseUrl rc ++ " stopped"
                Left err -> reportError err >> exitFailure

        -- Local-only commands should never reach here
        Server _ -> reportError "Server command is local-only" >> exitFailure
        DebugMatrices{} -> reportError "debug-matrices is local-only" >> exitFailure
        ExportMatrices{} -> reportError "export-matrices is local-only" >> exitFailure
        Repl -> reportError "repl should be handled in Main" >> exitFailure
        -- Answered before a server is contacted, so reaching here means the
        -- REPL, where the line is simply not one of its commands. Saying so
        -- and carrying on, rather than ending the session over it.
        Dump _ -> reportError "A dump command writes to stdout; run it outside the REPL."

-- | Look up the collection name for a given method UUID via /api/v1/methods
lookupMethodCollection :: Manager -> RemoteConfig -> Text -> IO (Maybe Text)
lookupMethodCollection mgr rc methodId = do
    result <- apiGet mgr rc "/api/v1/methods"
    return $ either (const Nothing) (parseMaybe go) result
  where
    go :: Value -> Parser Text
    go = withArray "methods" $ \arr ->
        case mapMaybe (parseMaybe matchOne) (V.toList arr) of
            (c : _) -> pure c
            [] -> fail "method not found"
    matchOne :: Value -> Parser Text
    matchOne = withObject "method" $ \obj -> do
        uuid <- obj .: "id"
        col <- obj .: "collection"
        if (uuid :: Text) == methodId then pure col else fail "no match"

-- | Auto-detect the single loaded database, or use the specified one
resolveDbName :: Manager -> RemoteConfig -> Maybe Text -> IO Text
resolveDbName _ _ (Just name) = return name
resolveDbName mgr rc Nothing = do
    result <- apiGet mgr rc "/api/v1/db"
    case result of
        Right val -> case extractLoadedDbNames val of
            Right [name] -> return name
            Right [] -> reportError "No databases loaded on the server" >> exitFailure
            Right names -> do
                reportError $
                    "Multiple databases loaded, use --db to select one: "
                        ++ unwords (map T.unpack names)
                exitFailure
            Left err -> do
                reportError $ "Cannot read the database list from " ++ rcBaseUrl rc ++ ": " ++ err
                exitFailure
        Left err -> reportError err >> exitFailure

{- | Names of the databases in memory, read from the database list. The keys
are the wire's, not the Haskell record's: a list this cannot read is an engine
whose shape moved, which is why it says so rather than answering "none".

A database whose cross-database links did not all resolve is in memory and
answers queries, so it counts: leaving it out reported "no databases loaded"
for a server holding one.
-}
extractLoadedDbNames :: Value -> Either String [Text]
extractLoadedDbNames = parseEither go
  where
    go :: Value -> Parser [Text]
    go = withObject "resp" $ \obj -> do
        dbs <- obj .: "databases"
        catMaybes <$> mapM getName dbs
    getName :: Value -> Parser (Maybe Text)
    getName = withObject "db" $ \db -> do
        status <- db .: "status"
        name <- db .: "name"
        return $ if (status :: Text) `elem` ["loaded", "partially_linked"] then Just name else Nothing

-- | Build a database-scoped API path
dbPath :: Text -> String
dbPath name = "/api/v1/db/" ++ T.unpack name

-- | JSON body for the delete-by-selection endpoint (field names match the API).
deleteSelectionBody :: DbDeleteArgs -> Value
deleteSelectionBody args =
    object
        [ "name" .= ddaName args
        , "location" .= ddaLocation args
        , "product" .= ddaProduct args
        , "classifications" .= classifications
        , "exact" .= ddaExact args
        , "keep" .= ddaKeep args
        , "extra" .= ddaExtra args
        , "ids" .= (if null (ddaIds args) then Nothing else Just (ddaIds args))
        ]
  where
    classifications = case (ddaClassSystem args, ddaClassValue args) of
        (Just sys, Just val) ->
            [object ["system" .= sys, "value" .= val, "exact" .= ddaExact args]]
        _ -> [] :: [Value]

{- | Body for POST /api/v1/db/{db}/relink with an inline alias mapping.
Keys mirror 'API.Types.RelinkRequest' after the @Stripped@ prefix transform
(@rmrDepDb@ → @depDb@, @rmrMappingCsv@ → @mappingCsv@); both are populated here,
which the handler reads as mapping mode (an empty @{}@ body is a plain relink).
-}
relinkMappingBody :: Text -> Text -> Value
relinkMappingBody depDb csv =
    object
        [ "depDb" .= depDb
        , "mappingCsv" .= csv
        ]

{- | Remote export: POST the target format and write the raw bytes the server
returns (serialization happens server-side) to @--out@. Approximation warnings
arrive percent-encoded in the @X-Volca-Export-Warnings@ header and are
reported, not fatal. Failures (bad format, db not loaded) surface loudly
rather than writing a partial/empty file.
-}
executeRemoteExport :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> DbExportArgs -> IO ()
executeRemoteExport mgr rc fmt jp args = do
    resp <- apiPostRaw mgr rc ("/api/v1/db/" ++ T.unpack (deaDb args) ++ "/export") (object ["format" .= deaFormat args])
    case resp of
        -- A failed export (bad format, db not loaded, unexportable data) arrives
        -- as a non-2xx HTTP status, surfaced here as 'Left'.
        Left err -> reportError err >> exitFailure
        Right r -> do
            mapM_ (reportProgress Warning . T.unpack) (exportWarnings r)
            BL.writeFile (deaOut args) (responseBody r)
            output fmt jp (Right (object ["database" .= deaDb args, "format" .= deaFormat args, "out" .= deaOut args]))

{- | Remote counterpart of @method export@: same transport as the database
export, against the method-collections endpoint.
-}
executeRemoteMethodExport :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> McExportArgs -> IO ()
executeRemoteMethodExport mgr rc fmt jp args = do
    resp <- apiPostRaw mgr rc ("/api/v1/method-collections/" ++ T.unpack (meaName args) ++ "/export") (object ["format" .= meaFormat args])
    case resp of
        Left err -> reportError err >> exitFailure
        Right r -> do
            mapM_ (reportProgress Warning . T.unpack) (exportWarnings r)
            BL.writeFile (meaOut args) (responseBody r)
            output fmt jp (Right (object ["collection" .= meaName args, "format" .= meaFormat args, "out" .= meaOut args]))

{- | Decode the @X-Volca-Export-Warnings@ response header: percent-decode, then
split on the newlines the server joined with. Absent or empty header = no
warnings.
-}
exportWarnings :: Response BL.ByteString -> [Text]
exportWarnings r =
    [ w
    | Just raw <- [lookup "X-Volca-Export-Warnings" (responseHeaders r)]
    , w <- T.splitOn "\n" (T.decodeUtf8 (urlDecode False raw))
    , not (T.null w)
    ]

-- | Build query string from optional parameters
buildQuery :: [(String, Maybe String)] -> String
buildQuery params =
    case [(k, v) | (k, Just v) <- params] of
        [] -> ""
        pairs -> "?" ++ intercalate "&" [k ++ "=" ++ urlEncode v | (k, v) <- pairs]
  where
    urlEncode = concatMap encodeChar
    encodeChar c
        | isAsciiUpper c = [c]
        | isAsciiLower c = [c]
        | isDigit c = [c]
        | c `elem` ("-_.~" :: String) = [c]
        | otherwise = '%' : showHex2 (fromEnum c)
    showHex2 n = [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
    hexDigit n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'A' + n - 10)

-- | Execute an upload command (database or method collection)
executeUpload :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> String -> UploadArgs -> IO ()
executeUpload mgr rc fmt jp path args = apiUploadFile mgr rc path args >>= output fmt jp

{- | Stream a file to an upload endpoint as a raw octet-stream body, carrying the
display name and optional description as query parameters. The body is streamed
from disk in constant memory (no base64, no whole-file buffering).
-}
apiUploadFile :: Manager -> RemoteConfig -> String -> UploadArgs -> IO (Either String Value)
apiUploadFile mgr rc path args = do
    body <- fileRequestBody (uaFile args)
    let url = rcBaseUrl rc ++ path
        query =
            ("name", Just (T.encodeUtf8 (uaName args)))
                : [("description", Just (T.encodeUtf8 d)) | Just d <- [uaDescription args]]
    result <- try $ do
        req0 <- parseRequest url
        let req1 =
                setQueryString query $
                    req0
                        { Network.HTTP.Client.method = "POST"
                        , requestHeaders =
                            authHeaders rc ++ [("Content-Type", "application/octet-stream")] ++ requestHeaders req0
                        , requestBody = body
                        }
        httpLbs req1 mgr
    case result of
        Left e -> return $ Left (formatHttpError (rcBaseUrl rc) e)
        Right resp ->
            let status = statusCode (responseStatus resp)
                respBody = responseBody resp
             in if status >= 200 && status < 300
                    then return $ Right $ fromMaybe (object []) (decode respBody)
                    else return $ Left $ formatApiError status respBody

-- | Build a constant-memory streaming request body from a file on disk.
fileRequestBody :: FilePath -> IO RequestBody
fileRequestBody fp = do
    size <- withBinaryFile fp ReadMode hFileSize
    return $ RequestBodyStream (fromIntegral size) $ \needsPopper ->
        withBinaryFile fp ReadMode $ \h -> needsPopper (BS.hGetSome h 65536)

{- | Output a response whose body carries an in-band @{"success",..,"message"}@
status (the handlers return HTTP 200 even on failure, so a bare 'output' would
exit 0 on a failed copy/delete). Inspect the @success@ field and fail loudly
when it is false; otherwise render normally. Covers both 'ActivateResponse' and
'DeleteSelectionResponse', which share these keys after the @Stripped@ transform.
-}
outputStatus :: OutputFormat -> Maybe Text -> String -> Either String Value -> IO ()
outputStatus _ _ _ (Left err) = reportError err >> exitFailure
outputStatus fmt jp action (Right val) = case parseMaybe parseStatus val of
    Nothing -> reportError (action ++ ": malformed server response") >> exitFailure
    Just (False, msg) -> reportError (action ++ " failed: " ++ T.unpack msg) >> exitFailure
    Just (True, _) -> output fmt jp (Right val)
  where
    parseStatus :: Value -> Parser (Bool, Text)
    parseStatus = withObject "StatusResponse" $ \o -> (,) <$> o .: "success" <*> o .: "message"

{- | @database load@ returns HTTP 200 even on failure, with a bare
@{"error": …}@ body ('LoadDatabaseResponse'\'s @LoadFailed@) and no @success@
field for 'outputStatus' to check. Mirror 'outputStatus': fail loudly when that
key is present, so a failed remote load exits non-zero instead of printing the
error and returning success.
-}
outputLoad :: OutputFormat -> Maybe Text -> Either String Value -> IO ()
outputLoad _ _ (Left err) = reportError err >> exitFailure
outputLoad fmt jp (Right val) = case val of
    Object o
        | Just (String err) <- KM.lookup "error" o ->
            reportError ("load failed: " ++ T.unpack err) >> exitFailure
    _ -> output fmt jp (Right val)

-- | Format and output a result
output :: OutputFormat -> Maybe Text -> Either String Value -> IO ()
output _ _ (Left err) = reportError err >> exitFailure
output fmt jp (Right val) = case renderResult fmt jp val of
    Left err -> reportError (T.unpack err) >> exitFailure
    Right rendered -> BSL.putStr rendered

authHeaders :: RemoteConfig -> [(HeaderName, BS.ByteString)]
authHeaders rc = case rcAuth rc of
    Just pwd -> [("Authorization", "Bearer " <> C8.pack pwd)]
    Nothing -> []

-- | HTTP GET, POST, DELETE helpers
apiGet :: Manager -> RemoteConfig -> String -> IO (Either String Value)
apiGet mgr rc path = apiRequest mgr rc "GET" path Nothing

apiPost :: Manager -> RemoteConfig -> String -> Value -> IO (Either String Value)
apiPost mgr rc path body = apiRequest mgr rc "POST" path (Just body)

apiDelete :: Manager -> RemoteConfig -> String -> IO (Either String Value)
apiDelete mgr rc path = apiRequest mgr rc "DELETE" path Nothing

apiPut :: Manager -> RemoteConfig -> String -> Value -> IO (Either String Value)
apiPut mgr rc path body = apiRequest mgr rc "PUT" path (Just body)

{- | Send a JSON file as the request body. The activities a user writes live in
a file, not on the command line, and the server owns what is valid - so the
client forwards the document rather than judging it twice. A file that is not
JSON at all is caught here, where the path can be named.
-}
postJsonFile :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> String -> FilePath -> IO ()
postJsonFile mgr rc fmt jp path = sendJsonFile (apiPost mgr rc path) fmt jp

putJsonFile :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> String -> FilePath -> IO ()
putJsonFile mgr rc fmt jp path = sendJsonFile (apiPut mgr rc path) fmt jp

sendJsonFile :: (Value -> IO (Either String Value)) -> OutputFormat -> Maybe Text -> FilePath -> IO ()
sendJsonFile send fmt jp file =
    readJsonFile file >>= \case
        Left err -> output fmt jp (Left err)
        Right body -> send body >>= output fmt jp

{- | Read and decode a JSON file, naming the file in any complaint about it.
Shared by the remote and local write commands, which read the same document.
-}
readJsonFile :: (FromJSON a) => FilePath -> IO (Either String a)
readJsonFile path = do
    bytes <- try (BL.readFile path)
    pure $ case bytes of
        Left (e :: IOException) -> Left (path <> ": " <> show e)
        Right raw -> either (\err -> Left (path <> ": " <> err)) Right (eitherDecode raw)

{- | Fetch a quality report in the representation the caller asked for.
@--format csv@ takes the engine's own CSV rendering, so the file the CLI
writes down a pipe is the file the web UI downloads; every other format takes
the JSON and renders it like any other command.
-}
fetchReport :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> String -> String -> IO ()
fetchReport mgr rc fmt jp path query = case fmt of
    CSV -> apiGetRaw mgr rc (path ++ ".csv" ++ query) >>= either fail' (BSL.putStr . responseBody)
    JSON -> asJson
    Pretty -> asJson
    Table -> asJson
  where
    asJson = apiGet mgr rc (path ++ query) >>= output fmt jp
    fail' err = reportError err >> exitFailure

{- | POST a JSON body and return the raw response (bytes + headers), for
octet-stream endpoints like database export. Shares the error formatting of
'apiRequest' but skips its JSON decoding.
-}
apiPostRaw :: Manager -> RemoteConfig -> String -> Value -> IO (Either String (Response BL.ByteString))
apiPostRaw mgr rc path body =
    rawOutcome rc <$> try request
  where
    request = do
        req0 <- parseRequest (rcBaseUrl rc ++ path)
        httpLbs
            req0
                { Network.HTTP.Client.method = "POST"
                , requestHeaders = authHeaders rc ++ [("Content-Type", "application/json")] ++ requestHeaders req0
                , requestBody = RequestBodyLBS (encode body)
                }
            mgr

-- | GET a raw response, for the endpoints that answer something other than JSON.
apiGetRaw :: Manager -> RemoteConfig -> String -> IO (Either String (Response BL.ByteString))
apiGetRaw mgr rc path =
    rawOutcome rc <$> try request
  where
    request = do
        req0 <- parseRequest (rcBaseUrl rc ++ path)
        httpLbs req0{requestHeaders = authHeaders rc ++ requestHeaders req0} mgr

-- | Read a raw HTTP outcome: 2xx is the response, anything else the formatted error.
rawOutcome :: RemoteConfig -> Either HttpException (Response BL.ByteString) -> Either String (Response BL.ByteString)
rawOutcome rc (Left e) = Left (formatHttpError (rcBaseUrl rc) e)
rawOutcome _ (Right resp)
    | status >= 200 && status < 300 = Right resp
    | otherwise = Left (formatApiError status (responseBody resp))
  where
    status = statusCode (responseStatus resp)

-- | Core HTTP request helper with error handling
apiRequest :: Manager -> RemoteConfig -> String -> String -> Maybe Value -> IO (Either String Value)
apiRequest mgr rc reqMethod path mBody = do
    let url = rcBaseUrl rc ++ path
    result <- try $ do
        req0 <- parseRequest url
        let req1 =
                req0
                    { Network.HTTP.Client.method = C8.pack reqMethod
                    , requestHeaders = authHeaders rc ++ contentHeaders ++ requestHeaders req0
                    }
            req2 = case mBody of
                Just body -> req1{requestBody = RequestBodyLBS (encode body)}
                Nothing -> req1
        httpLbs req2 mgr
    case result of
        Left e -> return $ Left (formatHttpError (rcBaseUrl rc) e)
        Right resp ->
            let status = statusCode (responseStatus resp)
                body = responseBody resp
             in if status >= 200 && status < 300
                    then return $ Right $ fromMaybe (object []) (decode body)
                    else return $ Left $ formatApiError status body
  where
    contentHeaders = case mBody of
        Just _ -> [("Content-Type", "application/json")]
        Nothing -> []

-- | Format HTTP exceptions into user-friendly messages
formatHttpError :: String -> HttpException -> String
formatHttpError baseUrl (HttpExceptionRequest _ (ConnectionFailure _)) =
    "Cannot connect to VoLCA server at "
        ++ baseUrl
        ++ "\nStart it with: volca --config volca.toml server"
formatHttpError _ (HttpExceptionRequest _ content) =
    "HTTP error: " ++ show content
formatHttpError _ (InvalidUrlException url reason) =
    "Invalid URL '" ++ url ++ "': " ++ reason

-- | Format API error responses
formatApiError :: Int -> BL.ByteString -> String
formatApiError 401 _ = "Authentication failed. Check --password or VOLCA_PASSWORD"
formatApiError 404 body = "Not found" ++ bodyDetail body
formatApiError status body = "Server error (HTTP " ++ show status ++ ")" ++ bodyDetail body

bodyDetail :: BL.ByteString -> String
bodyDetail body
    | BL.null body = ""
    | otherwise = ": " ++ BSL.unpack (BL.take 200 body)

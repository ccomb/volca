{-# LANGUAGE OverloadedStrings #-}

module CLI.Client (
    RemoteConfig (..),
    resolveRemoteConfig,
    executeRemoteCommand,
    apiGet,
    apiPost,
) where

import CLI.Types
import Config (Config (..), ServerConfig (..))
import Control.Exception (try)
import Data.Aeson (Value (..), decode, encode, object, (.:), (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseMaybe, withArray, withObject)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate, transpose)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (..),
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Types.Status (statusCode)
import Progress (reportError)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

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
                    Just sc -> return $ "http://" ++ T.unpack (scHost sc) ++ ":" ++ show (scPort sc)
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
        Database (DbUpload args) ->
            executeUpload mgr rc fmt jp "/api/v1/db/upload" args
        Database (DbDelete name) ->
            apiDelete mgr rc ("/api/v1/db/" ++ T.unpack name) >>= output fmt jp
        Method McList ->
            apiGet mgr rc "/api/v1/method-collections" >>= output fmt jp
        Method (McUpload args) ->
            executeUpload mgr rc fmt jp "/api/v1/method-collections/upload" args
        Method (McDelete name) ->
            apiDelete mgr rc ("/api/v1/method-collections/" ++ T.unpack name) >>= output fmt jp
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
        Stop -> do
            result <- apiPost mgr rc "/api/v1/shutdown" (object [])
            case result of
                Right _ -> putStrLn $ "Server at " ++ rcBaseUrl rc ++ " stopped"
                Left err -> reportError err >> exitFailure

        -- Local-only commands should never reach here
        Server _ -> reportError "Server command is local-only" >> exitFailure
        Plugin{} -> reportError "plugin command is local-only" >> exitFailure
        DebugMatrices{} -> reportError "debug-matrices is local-only" >> exitFailure
        ExportMatrices{} -> reportError "export-matrices is local-only" >> exitFailure
        Repl -> reportError "repl should be handled in Main" >> exitFailure
        DumpOpenApi -> reportError "dump-openapi should be handled in Main" >> exitFailure
        DumpMcpTools -> reportError "dump-mcp-tools should be handled in Main" >> exitFailure

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
        uuid <- obj .: "msmId"
        col <- obj .: "msmCollection"
        if (uuid :: Text) == methodId then pure col else fail "no match"

-- | Auto-detect the single loaded database, or use the specified one
resolveDbName :: Manager -> RemoteConfig -> Maybe Text -> IO Text
resolveDbName _ _ (Just name) = return name
resolveDbName mgr rc Nothing = do
    result <- apiGet mgr rc "/api/v1/db"
    case result of
        Right val -> case extractLoadedDbNames val of
            [name] -> return name
            [] -> reportError "No databases loaded on the server" >> exitFailure
            names -> do
                reportError $
                    "Multiple databases loaded, use --db to select one: "
                        ++ unwords (map T.unpack names)
                exitFailure
        Left err -> reportError err >> exitFailure

-- | Extract names of loaded databases from the database list JSON
extractLoadedDbNames :: Value -> [Text]
extractLoadedDbNames = fromMaybe [] . parseMaybe go
  where
    go :: Value -> Parser [Text]
    go = withObject "resp" $ \obj -> do
        dbs <- obj .: "dlrDatabases"
        fmap catMaybes $ mapM getName dbs
    getName :: Value -> Parser (Maybe Text)
    getName = withObject "db" $ \db -> do
        status <- db .: "dsaStatus"
        name <- db .: "dsaName"
        return $ if (status :: Text) == "loaded" then Just name else Nothing

-- | Build a database-scoped API path
dbPath :: Text -> String
dbPath name = "/api/v1/db/" ++ T.unpack name

-- | Build query string from optional parameters
buildQuery :: [(String, Maybe String)] -> String
buildQuery params =
    case [(k, v) | (k, Just v) <- params] of
        [] -> ""
        pairs -> "?" ++ intercalate "&" [k ++ "=" ++ urlEncode v | (k, v) <- pairs]
  where
    urlEncode = concatMap encodeChar
    encodeChar c
        | c >= 'A' && c <= 'Z' = [c]
        | c >= 'a' && c <= 'z' = [c]
        | c >= '0' && c <= '9' = [c]
        | c `elem` ("-_.~" :: String) = [c]
        | otherwise = '%' : showHex2 (fromEnum c)
    showHex2 n = [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
    hexDigit n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'A' + n - 10)

-- | Execute an upload command (database or method collection)
executeUpload :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> String -> UploadArgs -> IO ()
executeUpload mgr rc fmt jp path args = do
    fileData <- BL.readFile (uaFile args)
    let encoded = T.decodeLatin1 $ B64.encode (BL.toStrict fileData)
        body =
            object
                [ "urName" .= uaName args
                , "urDescription" .= uaDescription args
                , "urFileData" .= encoded
                ]
    apiPost mgr rc path body >>= output fmt jp

-- | Format and output a result
output :: OutputFormat -> Maybe Text -> Either String Value -> IO ()
output _ _ (Left err) = reportError err >> exitFailure
output fmt _jp (Right val) = case fmt of
    JSON -> BSL.putStrLn $ encode val
    Pretty -> BSL.putStrLn $ encodePretty val
    Table -> putStr $ renderTable val
    CSV -> putStr $ renderCSV val

-- | Render a JSON value as an aligned text table
renderTable :: Value -> String
renderTable val =
    case findArray val of
        Just rows -> formatTable (extractTable rows)
        Nothing -> BSL.unpack (encodePretty val) ++ "\n" -- fallback for non-array

-- | Render a JSON value as CSV
renderCSV :: Value -> String
renderCSV val =
    case findArray val of
        Just rows ->
            let (headers, dataRows) = extractTable rows
             in unlines $ intercalate "," (map quote headers) : map (intercalate "," . map quote) dataRows
        Nothing -> BSL.unpack (encode val) ++ "\n"
  where
    quote s = "\"" ++ concatMap (\c -> if c == '"' then "\"\"" else [c]) s ++ "\""

-- | Find the first array in a JSON value (top-level or one level deep)
findArray :: Value -> Maybe [Value]
findArray (Array arr) = Just (V.toList arr)
findArray (Object obj) =
    -- Look for a single array field (e.g., databases, results, methods, items)
    case mapMaybe extractArr (KM.toList obj) of
        [(_, arr)] -> Just arr
        _ -> Nothing
  where
    extractArr (_, Array arr) = Just ((), V.toList arr)
    extractArr _ = Nothing
findArray _ = Nothing

-- | Extract headers and rows from a list of JSON objects
extractTable :: [Value] -> ([String], [[String]])
extractTable [] = ([], [])
extractTable rows@(Object first : _) =
    let keys = map fst (KM.toList first)
        headers = map (Key.toString) keys
        dataRows = map (rowValues keys) rows
     in (headers, dataRows)
extractTable rows = (["value"], map (\v -> [cellValue v]) rows)

rowValues :: [KM.Key] -> Value -> [String]
rowValues keys (Object obj) = map (\k -> cellValue (fromMaybe Null (KM.lookup k obj))) keys
rowValues _ v = [cellValue v]

-- | Convert a JSON value to a display string for table cells
cellValue :: Value -> String
cellValue (String t) = T.unpack t
cellValue (Number n) = let s = show n in if ".0" `isSuffixOf` s then take (length s - 2) s else s
cellValue (Bool True) = "yes"
cellValue (Bool False) = ""
cellValue Null = ""
cellValue v = BSL.unpack (encode v)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Format headers + rows as an aligned table with separators
formatTable :: ([String], [[String]]) -> String
formatTable ([], _) = ""
formatTable (headers, rows) =
    let allRows = headers : rows
        widths = map (maximum . map length) (transpose (map (map (take maxColWidth)) allRows))
        padRow = zipWith (\w c -> take maxColWidth c ++ replicate (w - length (take maxColWidth c)) ' ') widths
        sep = intercalate "+" (map (\w -> replicate (w + 2) '-') widths)
        fmtRow r = "  " ++ intercalate " | " (padRow r)
     in unlines $ fmtRow headers : ("--" ++ sep) : map fmtRow rows
  where
    maxColWidth = 60

-- | HTTP GET, POST, DELETE helpers
apiGet :: Manager -> RemoteConfig -> String -> IO (Either String Value)
apiGet mgr rc path = apiRequest mgr rc "GET" path Nothing

apiPost :: Manager -> RemoteConfig -> String -> Value -> IO (Either String Value)
apiPost mgr rc path body = apiRequest mgr rc "POST" path (Just body)

apiDelete :: Manager -> RemoteConfig -> String -> IO (Either String Value)
apiDelete mgr rc path = apiRequest mgr rc "DELETE" path Nothing

-- | Core HTTP request helper with error handling
apiRequest :: Manager -> RemoteConfig -> String -> String -> Maybe Value -> IO (Either String Value)
apiRequest mgr rc reqMethod path mBody = do
    let url = rcBaseUrl rc ++ path
    result <- try $ do
        req0 <- parseRequest url
        let req1 =
                req0
                    { Network.HTTP.Client.method = C8.pack reqMethod
                    , requestHeaders = authHeaders ++ contentHeaders ++ requestHeaders req0
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
    authHeaders = case rcAuth rc of
        Just pwd -> [("Authorization", "Bearer " <> C8.pack pwd)]
        Nothing -> []
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

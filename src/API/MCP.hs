{-# LANGUAGE OverloadedStrings #-}

-- | MCP (Model Context Protocol) server endpoint.
-- Exposes VoLCA's LCA capabilities as MCP tools over HTTP (JSON-RPC 2.0).
-- Single POST /mcp endpoint handles initialize, tools/list, tools/call.
module API.MCP (mcpApp) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Network.HTTP.Types (status200, status202, hContentType)
import System.Random (randomIO)
import Network.Wai (Application, responseLBS, strictRequestBody)

import Database.Manager (DatabaseManager(..), LoadedDatabase(..), getDatabase)
import qualified Database.Manager as DM
import Matrix (computeInventoryMatrix, computeScalingVector, applyBiosphereMatrix, computeProcessLCIAContributions)
import Method.Mapping (computeLCIAScore, mapMethodToFlows, computeMappingStats, MappingStats(..))
import Method.Types (Method(..), MethodCF(..))
import Plugin.Types (PluginRegistry(..))
import Plugin.Builtin (flowContribution)
import SharedSolver (SharedSolver)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Service
import Types (Database(..), Activity(..), activityName, activityLocation, flowName, flowId, processIdToText)
import API.Types (InventoryExport(..), InventoryFlowDetail(..))
import UnitConversion (defaultUnitConfig)
import Network.Wai (requestHeaders)
import Network.HTTP.Types.Header (hHost)
import Numeric (showFFloat)

-- ---------------------------------------------------------------------------
-- JSON-RPC 2.0 types
-- ---------------------------------------------------------------------------

data RpcRequest = RpcRequest
    { rpcId     :: Maybe Value  -- Nothing = notification
    , rpcMethod :: Text
    , rpcParams :: Maybe Value
    } deriving (Show)

instance FromJSON RpcRequest where
    parseJSON = withObject "RpcRequest" $ \v -> RpcRequest
        <$> v .:? "id"
        <*> v .: "method"
        <*> v .:? "params"

rpcResult :: Value -> Value -> Value
rpcResult rid res = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= rid
    , "result"  .= res
    ]

rpcError :: Value -> Int -> Text -> Value
rpcError rid code msg = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= rid
    , "error"   .= object [ "code" .= code, "message" .= msg ]
    ]

toolError :: Value -> Text -> Value
toolError rid msg = rpcResult rid $ object
    [ "content" .= [ object [ "type" .= ("text" :: Text), "text" .= msg ] ]
    , "isError" .= True
    ]

toolSuccessJson :: Value -> Value -> Value
toolSuccessJson rid val = rpcResult rid $ object
    [ "content" .= [ object [ "type" .= ("text" :: Text), "text" .= encodeAsText val ] ]
    , "isError" .= False
    ]
  where
    encodeAsText (String t) = t
    encodeAsText v = TE.decodeUtf8 $ BSL.toStrict $ encode v

-- ---------------------------------------------------------------------------
-- MCP Application
-- ---------------------------------------------------------------------------

newtype McpState = McpState
    { mcpSessionId :: Text
    }

mcpApp :: DatabaseManager -> IO Application
mcpApp dbManager = do
    (a, b) <- (,) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)
    let sessionId = T.pack $ show (abs a) ++ "-" ++ show (abs b)
    stateRef <- newIORef McpState { mcpSessionId = sessionId }
    return $ \req respond -> do
        body <- strictRequestBody req
        let hostHeader = fromMaybe "localhost" $ lookup hHost (requestHeaders req)
            baseUrl = "http://" <> TE.decodeUtf8 hostHeader
        case eitherDecode body of
            Left err ->
                respond $ jsonResponse $ rpcError Null (-32700) (T.pack $ "Parse error: " ++ err)
            Right rpcReq -> do
                st <- readIORef stateRef
                resp <- handleRpc dbManager baseUrl st rpcReq
                case resp of
                    Nothing -> respond $ responseLBS status202 [(hContentType, "application/json")] ""
                    Just val -> respond $ jsonResponse val
  where
    jsonResponse v = responseLBS status200
        [ (hContentType, "application/json")
        , ("X-Content-Type-Options", "nosniff")
        ] (encode v)

-- ---------------------------------------------------------------------------
-- RPC dispatch
-- ---------------------------------------------------------------------------

handleRpc :: DatabaseManager -> Text -> McpState -> RpcRequest -> IO (Maybe Value)
handleRpc dbManager baseUrl _st req = case rpcMethod req of
    "initialize"                -> Just <$> handleInitialize req
    "notifications/initialized" -> return Nothing  -- notification, no response
    "tools/list"                -> return $ Just $ handleToolsList req
    "tools/call"                -> Just <$> handleToolsCall dbManager baseUrl req
    "ping"                      -> return $ Just $ rpcResult (rid req) (object [])
    other                       -> return $ Just $ rpcError (rid req) (-32601)
                                     ("Method not found: " <> other)
  where
    rid r = fromMaybe Null (rpcId r)

-- ---------------------------------------------------------------------------
-- initialize
-- ---------------------------------------------------------------------------

handleInitialize :: RpcRequest -> IO Value
handleInitialize req = return $ rpcResult (fromMaybe Null $ rpcId req) $ object
    [ "protocolVersion" .= ("2025-11-25" :: Text)
    , "capabilities"    .= object [ "tools" .= object [] ]
    , "serverInfo"      .= object
        [ "name"    .= ("volca" :: Text)
        , "version" .= ("0.6.0" :: Text)
        ]
    , "instructions"    .= T.unlines
        [ "VoLCA is a Life Cycle Assessment engine."
        , "Use search_activities to find processes, then get_activity, get_tree, get_inventory, or compute_lcia to analyze them."
        , "All tools that operate on activities require a 'database' parameter (name of a loaded DB) and a 'process_id' (format: activityUUID_productUUID)."
        , "Use list_databases to see available databases, and list_methods for available LCIA methods."
        ]
    ]

-- ---------------------------------------------------------------------------
-- tools/list
-- ---------------------------------------------------------------------------

handleToolsList :: RpcRequest -> Value
handleToolsList req = rpcResult (fromMaybe Null $ rpcId req) $ object
    [ "tools" .= toolDefinitions ]

toolDefinitions :: [Value]
toolDefinitions =
    [ mkTool "list_databases" "List all loaded LCA databases"
        (props [] [])
    , mkTool "search_activities" "Search for activities (processes) by name, geography, or product. Returns a paginated list of matching activities with their process IDs."
        (props
            [ ("database", "string", "Database name")
            , ("name", "string", "Name substring to search for")
            ]
            [ ("geo", "string", "Geography/location filter (e.g. 'FR', 'DE', 'GLO')")
            , ("product", "string", "Product name filter")
            , ("limit", "integer", "Max results (default 20)")
            ])
    , mkTool "search_flows" "Search for biosphere flows (emissions, resources) by name"
        (props
            [ ("database", "string", "Database name")
            , ("query", "string", "Flow name to search for")
            ]
            [ ("limit", "integer", "Max results (default 20)")
            ])
    , mkTool "get_activity" "Get detailed information about an activity: name, location, exchanges, reference product, metadata"
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            ] [])
    , mkTool "get_tree" "Get the supply chain tree of an activity, showing recursive inputs with loop detection"
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID")
            ]
            [ ("depth", "integer", "Max tree depth (default 3)")
            ])
    , mkTool "get_supply_chain" "Get a flat list of all upstream activities in the supply chain with their scaled quantities"
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID")
            ]
            [ ("name", "string", "Filter by activity name")
            , ("location", "string", "Filter by location")
            , ("limit", "integer", "Max results (default 100)")
            , ("min_quantity", "number", "Min scaled quantity threshold")
            ])
    , mkTool "get_inventory" "Compute the Life Cycle Inventory (LCI) — biosphere flows (emissions and resource extractions) for an activity's full supply chain. Returns statistics and top flows by quantity."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID")
            ]
            [ ("flow", "string", "Filter flows by name (case-insensitive substring)")
            , ("limit", "integer", "Max flows to return, sorted by absolute quantity (default 50)")
            ])
    , mkTool "compute_lcia" "Compute Life Cycle Impact Assessment (LCIA) score for an activity. Returns the score, functional unit, and top contributing elementary flows."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID")
            , ("method_id", "string", "Method UUID")
            ]
            [ ("top_flows", "integer", "Number of top contributing flows to return (default 5)")
            ])
    , mkTool "list_methods" "List all loaded LCIA methods (impact assessment methods like climate change, acidification, etc.)"
        (props [] [])
    , mkTool "get_flow_mapping" "Get the mapping between a method's characterization factors and database flows, showing match coverage"
        (props
            [ ("database", "string", "Database name")
            , ("method_id", "string", "Method UUID")
            ] [])
    , mkTool "analyze_flow_hotspots" "Identify which elementary flows (emissions/resources) contribute most to a specific impact category. Answers 'which emissions drive my climate change score?'"
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID")
            , ("method_id", "string", "Method UUID for the impact category")
            ]
            [ ("limit", "integer", "Max flows to return, sorted by contribution (default 20)")
            ])
    , mkTool "analyze_process_hotspots" "Identify which upstream processes contribute most to a specific impact category. Answers 'which suppliers drive my climate change score?' Uses exact matrix-based computation, valid even for cyclic supply chains."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID")
            , ("method_id", "string", "Method UUID for the impact category")
            ]
            [ ("limit", "integer", "Max processes to return, sorted by contribution (default 10)")
            ])
    ]

mkTool :: Text -> Text -> Value -> Value
mkTool name desc schema = object
    [ "name"        .= name
    , "description" .= desc
    , "inputSchema" .= schema
    ]

-- Build a JSON Schema object with required + optional properties
props :: [(Text, Text, Text)] -> [(Text, Text, Text)] -> Value
props required optional = object $
    [ "type"       .= ("object" :: Text)
    , "properties" .= object
        (map propEntry (required ++ optional))
    ] ++
    [ "required" .= map (\(n,_,_) -> n) required | not (null required) ]
  where
    propEntry (n, ty, desc) = fromText n .= object
        [ "type" .= ty, "description" .= desc ]

-- ---------------------------------------------------------------------------
-- tools/call dispatch
-- ---------------------------------------------------------------------------

handleToolsCall :: DatabaseManager -> Text -> RpcRequest -> IO Value
handleToolsCall dbManager baseUrl req = do
    let rid = fromMaybe Null (rpcId req)
    case rpcParams req >>= parseCallParams of
        Nothing -> return $ rpcError rid (-32602) "Invalid params: expected {name, arguments}"
        Just (toolName, args) -> callTool dbManager baseUrl rid toolName args

parseCallParams :: Value -> Maybe (Text, KeyMap Value)
parseCallParams (Object o) = do
    String name <- KM.lookup "name" o
    let args = case KM.lookup "arguments" o of
            Just (Object a) -> a
            _               -> KM.empty
    return (name, args)
parseCallParams _ = Nothing

callTool :: DatabaseManager -> Text -> Value -> Text -> KeyMap Value -> IO Value
callTool dbManager baseUrl rid name args = case name of
    "list_databases"          -> callListDatabases dbManager rid
    "search_activities"       -> withDb dbManager rid args $ callSearchActivities rid args
    "search_flows"            -> withDb dbManager rid args $ callSearchFlows rid args
    "get_activity"            -> withDb dbManager rid args $ callGetActivity rid args
    "get_tree"                -> withDb dbManager rid args $ callGetTree rid args
    "get_supply_chain"        -> withDb dbManager rid args $ callGetSupplyChain rid args
    "get_inventory"           -> withDb dbManager rid args $ callGetInventory rid args
    "compute_lcia"            -> callComputeLCIA dbManager baseUrl rid args
    "list_methods"            -> callListMethods dbManager rid
    "get_flow_mapping"        -> callGetFlowMapping dbManager rid args
    "analyze_flow_hotspots"   -> callAnalyzeFlowHotspots dbManager baseUrl rid args
    "analyze_process_hotspots"-> callAnalyzeProcessHotspots dbManager baseUrl rid args
    _                         -> return $ toolError rid ("Unknown tool: " <> name)

-- Helper: extract database, then run action
withDb :: DatabaseManager -> Value -> KeyMap Value
       -> ((Database, SharedSolver) -> IO Value) -> IO Value
withDb dbManager rid args action =
    case textArg "database" args of
        Nothing -> return $ toolError rid "Missing required parameter: database"
        Just dbName -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> action (ldDatabase ld, ldSharedSolver ld)

textArg :: Text -> KeyMap Value -> Maybe Text
textArg key args = case KM.lookup (fromText key) args of
    Just (String t) -> Just t
    _               -> Nothing

intArg :: Text -> KeyMap Value -> Maybe Int
intArg key args = case KM.lookup (fromText key) args of
    Just (Number n) -> Just (round n)
    _               -> Nothing

doubleArg :: Text -> KeyMap Value -> Maybe Double
doubleArg key args = case KM.lookup (fromText key) args of
    Just (Number n) -> Just (realToFrac n)
    _               -> Nothing

-- ---------------------------------------------------------------------------
-- Tool implementations
-- ---------------------------------------------------------------------------

callListDatabases :: DatabaseManager -> Value -> IO Value
callListDatabases dbManager rid = do
    loaded <- readTVarIO (dmLoadedDbs dbManager)
    let names = M.keys loaded
    return $ toolSuccessJson rid $ object [ "databases" .= names ]

callSearchActivities :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchActivities rid args (db, _) = do
    let name    = textArg "name" args
        geo     = textArg "geo" args
        product' = textArg "product" args
        limit   = intArg "limit" args
    result <- Service.searchActivities db name geo product' Nothing Nothing (limit <|> Just 20) Nothing
    case result of
        Left err  -> return $ toolError rid (T.pack $ show err)
        Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a       <|> _ = a

callSearchFlows :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchFlows rid args (db, _) = do
    let query = textArg "query" args
        limit = intArg "limit" args
    result <- Service.searchFlows db query Nothing (limit <|> Just 20) Nothing
    case result of
        Left err  -> return $ toolError rid (T.pack $ show err)
        Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a       <|> _ = a

callGetActivity :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetActivity rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            case Service.getActivityInfo defaultUnitConfig db pid of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val

callGetTree :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetTree rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            let depth = fromMaybe 3 (intArg "depth" args)
            in case Service.getActivityTree db pid depth of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val

callGetSupplyChain :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetSupplyChain rid args (db, solver) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid -> do
            let nameF = textArg "name" args
                locF  = textArg "location" args
                limit = intArg "limit" args
                minQ  = doubleArg "min_quantity" args
            result <- Service.getSupplyChain db solver pid nameF limit minQ
                        Nothing Nothing locF Nothing Nothing Nothing False
            case result of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid (toJSON val)

callGetInventory :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetInventory rid args (db, solver) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid -> do
            let limit = fromMaybe 50 (intArg "limit" args)
                nameFilter = textArg "flow" args
            result <- Service.getActivityInventoryWithSharedSolver [] solver db pid
            case result of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right inv ->
                    let flows = ieFlows inv
                        filtered = case nameFilter of
                            Nothing -> flows
                            Just q  -> filter (T.isInfixOf (T.toLower q) . T.toLower . flowName . ifdFlow) flows
                        sorted  = L.sortBy (\a b -> compare (abs $ ifdQuantity b) (abs $ ifdQuantity a)) filtered
                        topN    = take limit sorted
                        slim f  = object
                            [ "flow"     .= flowName (ifdFlow f)
                            , "quantity" .= ifdQuantity f
                            , "unit"     .= ifdUnitName f
                            , "category" .= ifdCategory f
                            , "isEmission" .= ifdIsEmission f
                            ]
                    in return $ toolSuccessJson rid $ object
                        [ "statistics" .= toJSON (ieStatistics inv)
                        , "total_flows" .= length flows
                        , "shown_flows" .= length topN
                        , "flows"       .= map slim topN
                        ]

callComputeLCIA :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callComputeLCIA dbManager baseUrl rid args = do
    case (textArg "database" args, textArg "process_id" args, textArg "method_id" args) of
        (Nothing, _, _) -> return $ toolError rid "Missing required parameter: database"
        (_, Nothing, _) -> return $ toolError rid "Missing required parameter: process_id"
        (_, _, Nothing) -> return $ toolError rid "Missing required parameter: method_id"
        (Just dbName, Just pidText, Just methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                        topN = fromMaybe 5 (intArg "top_flows" args)
                    loadedMethods <- DM.getLoadedMethods dbManager
                    let allMethods = map snd loadedMethods
                    case UUID.fromText methodIdText of
                        Nothing -> return $ toolError rid "Invalid method UUID format"
                        Just uuid ->
                            case filter (\m -> methodId m == uuid) allMethods of
                                [] -> return $ toolError rid "Method not found"
                                (method:_) ->
                                    case Service.resolveActivityAndProcessId db pidText of
                                        Left err -> return $ toolError rid (T.pack $ show err)
                                        Right (processId, activity) -> do
                                            inventory <- computeInventoryMatrix db processId
                                            let mappers = prMappers (dmPlugins dbManager)
                                            unitCfg <- DM.getMergedUnitConfig dbManager
                                            mappings <- mapMethodToFlows mappers db method
                                            let stats = computeMappingStats mappings
                                                score = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
                                                (prodName, prodAmount, prodUnit) = Service.getReferenceProductInfo (dbFlows db) (dbUnits db) activity
                                                functionalUnit = T.pack (showFFloat (Just 2) prodAmount "") <> " " <> prodUnit <> " of " <> prodName
                                                contribs = L.sortOn (\(_,_,c) -> negate (abs c)) (mapMaybe (flowContribution inventory) mappings)
                                                topFlows = take topN contribs
                                                webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText <> "/lcia?method=" <> UUID.toText uuid
                                            return $ toolSuccessJson rid $ object
                                                [ "method"          .= methodName method
                                                , "category"        .= methodCategory method
                                                , "score"           .= score
                                                , "unit"            .= methodUnit method
                                                , "functional_unit" .= functionalUnit
                                                , "mapped_flows"    .= (msTotal stats - msUnmatched stats)
                                                , "web_url"         .= webUrl
                                                , "top_flows"       .= [ object
                                                    [ "flow_name"    .= flowName f
                                                    , "contribution" .= c
                                                    , "share_pct"    .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                                    ]
                                                    | (_, f, c) <- topFlows
                                                    ]
                                                ]

callListMethods :: DatabaseManager -> Value -> IO Value
callListMethods dbManager rid = do
    loadedMethods <- DM.getLoadedMethods dbManager
    let summaries = map (\(_, m) -> object
            [ "id"       .= UUID.toText (methodId m)
            , "name"     .= methodName m
            , "category" .= methodCategory m
            , "unit"     .= methodUnit m
            ]) loadedMethods
    return $ toolSuccessJson rid $ object [ "methods" .= summaries ]

callGetFlowMapping :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetFlowMapping dbManager rid args =
    case (textArg "database" args, textArg "method_id" args) of
        (Nothing, _) -> return $ toolError rid "Missing required parameter: database"
        (_, Nothing) -> return $ toolError rid "Missing required parameter: method_id"
        (Just dbName, Just methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                    loadedMethods <- DM.getLoadedMethods dbManager
                    let allMethods = map snd loadedMethods
                    case UUID.fromText methodIdText of
                        Nothing -> return $ toolError rid "Invalid method UUID format"
                        Just uuid ->
                            case filter (\m -> methodId m == uuid) allMethods of
                                [] -> return $ toolError rid "Method not found"
                                (method:_) -> do
                                    let mappers = prMappers (dmPlugins dbManager)
                                    mappings <- mapMethodToFlows mappers db method
                                    let stats = computeMappingStats mappings
                                        total = msTotal stats
                                        matched = total - msUnmatched stats
                                        coverage = if total > 0
                                            then fromIntegral matched / fromIntegral total * 100 :: Double
                                            else 0
                                    return $ toolSuccessJson rid $ object
                                        [ "method"    .= methodName method
                                        , "total"     .= total
                                        , "matched"   .= matched
                                        , "unmatched" .= msUnmatched stats
                                        , "coverage"  .= coverage
                                        ]

-- | Helper: resolve method from UUID text
resolveMethod :: DatabaseManager -> Text -> IO (Either Text Method)
resolveMethod dbManager methodIdText =
    case UUID.fromText methodIdText of
        Nothing -> return $ Left "Invalid method UUID format"
        Just uuid -> do
            loadedMethods <- DM.getLoadedMethods dbManager
            case filter (\m -> methodId m == uuid) (map snd loadedMethods) of
                []       -> return $ Left "Method not found"
                (m:_)    -> return $ Right m

callAnalyzeFlowHotspots :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callAnalyzeFlowHotspots dbManager baseUrl rid args =
    case (textArg "database" args, textArg "process_id" args, textArg "method_id" args) of
        (Nothing, _, _) -> return $ toolError rid "Missing required parameter: database"
        (_, Nothing, _) -> return $ toolError rid "Missing required parameter: process_id"
        (_, _, Nothing) -> return $ toolError rid "Missing required parameter: method_id"
        (Just dbName, Just pidText, Just methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    eMethod <- resolveMethod dbManager methodIdText
                    case eMethod of
                        Left err -> return $ toolError rid err
                        Right method ->
                            case Service.resolveActivityAndProcessId (ldDatabase ld) pidText of
                                Left err -> return $ toolError rid (T.pack $ show err)
                                Right (processId, _) -> do
                                    let db      = ldDatabase ld
                                        lim     = fromMaybe 20 (intArg "limit" args)
                                        mappers = prMappers (dmPlugins dbManager)
                                        webUrl  = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText <> "/lcia?method=" <> methodIdText
                                    unitCfg  <- DM.getMergedUnitConfig dbManager
                                    scalingVec <- computeScalingVector db processId
                                    let inventory = applyBiosphereMatrix db scalingVec
                                    mappings <- mapMethodToFlows mappers db method
                                    let score   = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
                                        contribs = L.sortOn (\(_,_,c) -> negate (abs c)) (mapMaybe (flowContribution inventory) mappings)
                                        top      = take lim contribs
                                    return $ toolSuccessJson rid $ object
                                        [ "method"       .= methodName method
                                        , "unit"         .= methodUnit method
                                        , "total_score"  .= score
                                        , "web_url"      .= webUrl
                                        , "top_flows"    .= [ object
                                            [ "flow_name"    .= flowName f
                                            , "contribution" .= c
                                            , "share_pct"    .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                            ]
                                            | (_, f, c) <- top
                                            ]
                                        ]

callAnalyzeProcessHotspots :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callAnalyzeProcessHotspots dbManager baseUrl rid args =
    case (textArg "database" args, textArg "process_id" args, textArg "method_id" args) of
        (Nothing, _, _) -> return $ toolError rid "Missing required parameter: database"
        (_, Nothing, _) -> return $ toolError rid "Missing required parameter: process_id"
        (_, _, Nothing) -> return $ toolError rid "Missing required parameter: method_id"
        (Just dbName, Just pidText, Just methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    eMethod <- resolveMethod dbManager methodIdText
                    case eMethod of
                        Left err -> return $ toolError rid err
                        Right method ->
                            case Service.resolveActivityAndProcessId (ldDatabase ld) pidText of
                                Left err -> return $ toolError rid (T.pack $ show err)
                                Right (processId, _) -> do
                                    let db      = ldDatabase ld
                                        lim     = fromMaybe 10 (intArg "limit" args)
                                        mappers = prMappers (dmPlugins dbManager)
                                    unitCfg    <- DM.getMergedUnitConfig dbManager
                                    scalingVec <- computeScalingVector db processId
                                    let inventory = applyBiosphereMatrix db scalingVec
                                    mappings <- mapMethodToFlows mappers db method
                                    let score        = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
                                        cfMap        = M.fromList [(flowId f, mcfValue cf) | (cf, Just (f, _)) <- mappings]
                                        contributions = computeProcessLCIAContributions db scalingVec cfMap
                                        sorted       = L.sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
                                        top          = take lim sorted
                                        mkEntry (pid, c) =
                                            let mAct    = Service.findActivityByProcessId db pid
                                                actName = maybe "" activityName mAct
                                                actLoc  = maybe "" activityLocation mAct
                                                prodName = case mAct of
                                                    Just act -> let (pn, _, _) = Service.getReferenceProductInfo (dbFlows db) (dbUnits db) act in pn
                                                    Nothing  -> ""
                                                pidText' = processIdToText db pid
                                                procWebUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText' <> "/lcia?method=" <> methodIdText
                                            in object
                                                [ "process_id"    .= pidText'
                                                , "activity_name" .= actName
                                                , "product_name"  .= prodName
                                                , "location"      .= actLoc
                                                , "contribution"  .= c
                                                , "share_pct"     .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                                , "web_url"       .= procWebUrl
                                                ]
                                    return $ toolSuccessJson rid $ object
                                        [ "method"      .= methodName method
                                        , "unit"        .= methodUnit method
                                        , "total_score" .= score
                                        , "processes"   .= map mkEntry top
                                        ]

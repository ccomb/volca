{-# LANGUAGE OverloadedStrings #-}

-- | MCP (Model Context Protocol) server endpoint.
-- Implements Streamable HTTP transport (MCP spec 2025-03-26).
-- POST /mcp handles initialize, tools/list, tools/call (JSON or SSE response).
-- GET  /mcp opens an SSE stream for server-initiated messages (stateless: closes immediately).
module API.MCP (mcpApp, toolDefinitions) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Network.HTTP.Types (status200, status202, status405, hContentType)
import Network.URI (escapeURIString, isUnreserved)
import System.Random (randomIO)
import Network.Wai (Application, responseLBS, strictRequestBody, requestMethod, requestHeaders)
import qualified Data.ByteString as BS

import Config (DatabaseConfig(..), ClassificationPreset(..), ClassificationEntry(..))
import Database.Manager (DatabaseManager(..), LoadedDatabase(..), getDatabase)
import qualified Database.Manager as DM

import Matrix (computeInventoryMatrix, computeScalingVector, applyBiosphereMatrix, computeProcessLCIAContributions)
import Method.Mapping (computeLCIAScore, mapMethodToFlows, computeMappingStats, MappingStats(..))
import Method.Types (Method(..), MethodCF(..), FlowDirection(..))
import Plugin.Types (PluginRegistry(..))
import Plugin.Builtin (flowContribution)
import SharedSolver (SharedSolver)
import qualified Data.List as L
import qualified Service
import Types (Database(..), Activity(..), Indexes(..), activityName, activityLocation, flowName, flowId, flowCategory, flowSubcompartment, getUnitNameForFlow, processIdToText, exchangeIsInput)
import API.Types (InventoryExport(..), InventoryFlowDetail(..), ClassificationSystem(..), ActivityInfo(..), ActivityForAPI(..), ExchangeWithUnit(..))
import qualified Service.Aggregate as Agg
import UnitConversion (defaultUnitConfig)
import Network.HTTP.Types.Header (hHost, hAccept)
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

-- | Percent-encode a Text value for use as a URL path segment.
encodeSegment :: Text -> Text
encodeSegment = T.pack . escapeURIString isUnreserved . T.unpack

mcpApp :: DatabaseManager -> [ClassificationPreset] -> IO Application
mcpApp dbManager presets = do
    (a, b) <- (,) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)
    let sessionId = T.pack $ show (abs a) ++ "-" ++ show (abs b)
    stateRef <- newIORef McpState { mcpSessionId = sessionId }
    return $ \req respond -> do
        let method      = requestMethod req
            hdrs        = requestHeaders req
            hostHeader  = fromMaybe "localhost" $ lookup hHost hdrs
            baseUrl     = "http://" <> TE.decodeUtf8 hostHeader
            acceptHdr   = fromMaybe "" $ lookup hAccept hdrs
            wantsSse    = "text/event-stream" `BS.isInfixOf` acceptHdr
        st <- readIORef stateRef
        case method of
            -- GET: open SSE stream for server-initiated messages.
            -- VoLCA is stateless so we return an empty stream immediately.
            "GET" -> respond $ responseLBS status200
                [ (hContentType, "text/event-stream; charset=utf-8")
                , ("Cache-Control", "no-cache")
                , ("Connection", "keep-alive")
                , ("Mcp-Session-Id", TE.encodeUtf8 (mcpSessionId st))
                ] ""
            "POST" -> do
                body <- strictRequestBody req
                case eitherDecode body of
                    Left err ->
                        respond $ jsonResponse (mcpSessionId st) $ rpcError Null (-32700) (T.pack $ "Parse error: " ++ err)
                    Right rpcReq -> do
                        resp <- handleRpc dbManager presets baseUrl st rpcReq
                        case resp of
                            Nothing ->
                                respond $ responseLBS status202
                                    [ (hContentType, "application/json")
                                    , ("Mcp-Session-Id", TE.encodeUtf8 (mcpSessionId st))
                                    ] ""
                            Just val ->
                                if wantsSse
                                then respond $ sseResponse (mcpSessionId st) val
                                else respond $ jsonResponse (mcpSessionId st) val
            _ -> respond $ responseLBS status405 [(hContentType, "application/json")] $
                    encode $ rpcError Null (-32700) "Method not allowed"
  where
    jsonResponse sid v = responseLBS status200
        [ (hContentType, "application/json")
        , ("X-Content-Type-Options", "nosniff")
        , ("Mcp-Session-Id", TE.encodeUtf8 sid)
        ] (encode v)
    -- SSE format: each JSON-RPC message is one SSE event
    sseResponse sid v = responseLBS status200
        [ (hContentType, "text/event-stream; charset=utf-8")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        , ("Mcp-Session-Id", TE.encodeUtf8 sid)
        ] ("event: message\ndata: " <> encode v <> "\n\n")

-- ---------------------------------------------------------------------------
-- RPC dispatch
-- ---------------------------------------------------------------------------

handleRpc :: DatabaseManager -> [ClassificationPreset] -> Text -> McpState -> RpcRequest -> IO (Maybe Value)
handleRpc dbManager presets baseUrl _st req = case rpcMethod req of
    "initialize"                -> Just <$> handleInitialize req
    "notifications/initialized" -> return Nothing  -- notification, no response
    "tools/list"                -> return $ Just $ handleToolsList req
    "tools/call"                -> Just <$> handleToolsCall dbManager presets baseUrl req
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
    [ "protocolVersion" .= ("2025-03-26" :: Text)
    , "capabilities"    .= object [ "tools" .= object [] ]
    , "serverInfo"      .= object
        [ "name"    .= ("volca" :: Text)
        , "version" .= ("0.6.0" :: Text)
        ]
    , "instructions"    .= T.unlines
        [ "VoLCA is a Life Cycle Assessment engine."
        , "Use search_activities to find processes, then get_activity, get_tree, get_inventory, or get_lcia to analyze them."
        , "All tools that operate on activities require a 'database' parameter (name of a loaded DB) and a 'process_id'."
        , "Preferred process_id format: activityUUID_productUUID. A bare activityUUID is also accepted when the activity has a unique reference product — no search_activities lookup is needed in that case."
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
    , mkTool "list_presets" "List named classification filter presets configured in this instance. Each preset bundles multiple (system, value, mode) classification filters under a human-readable label. Use the filter values from a preset as inputs to search_activities classification parameters."
        (props [] [])
    , mkTool "search_activities" "Search for activities (processes) by name, geography, product, classification, or preset. Returns a paginated list of matching activities with their process IDs."
        (props
            [ ("database", "string", "Database name")
            , ("name", "string", "Name substring to search for (or exact name if exact=true)")
            ]
            [ ("geo", "string", "Geography/location filter (e.g. 'FR', 'DE', 'GLO')")
            , ("product", "string", "Product name filter")
            , ("exact", "boolean", "If true, name and geo must match exactly (case-insensitive equality) instead of substring search")
            , ("preset", "string", "Name of a classification preset (from list_presets) — expands to its bundled filters. Can be combined with explicit classification filters.")
            , ("classification", "string", "Classification system name to filter by (e.g. 'ISIC rev.4 ecoinvent', 'CPC'). Use list_classifications to see available systems.")
            , ("classification_value", "string", "Value within the classification system to match")
            , ("classification_match", "string", "Match mode: \"equals\" (case-insensitive equality) or \"contains\" (substring, default)")
            , ("limit", "integer", "Max results (default 20)")
            ])
    , mkTool "search_flows" "Search for biosphere flows (emissions, resources) by name"
        (props
            [ ("database", "string", "Database name")
            , ("query", "string", "Flow name to search for")
            ]
            [ ("limit", "integer", "Max results (default 20)")
            ])
    , mkTool "get_activity" "Get detailed information about an activity: name, location, exchanges, reference product, metadata. Use exchange_type / is_input / flow to filter exchanges and reduce response size."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            ]
            [ ("exchange_type", "string", "Filter exchanges: \"biosphere\" (emissions/resources only), \"technosphere\" (inputs/outputs only), or \"all\" (default)")
            , ("is_input", "boolean", "If true, return only inputs; if false, only outputs; omit for both. Combines with exchange_type.")
            , ("flow", "string", "Filter exchanges by flow name (case-insensitive substring)")
            ])
    , mkTool "get_tree" "Get the supply chain tree of an activity, showing recursive inputs with loop detection"
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            ]
            [ ("depth", "integer", "Max tree depth (default 2)")
            , ("name", "string", "Filter nodes by activity name; keeps matching nodes plus all their ancestors up to the root (case-insensitive substring)")
            ])
    , mkTool "aggregate" "Aggregate exchanges, supply chain entries, or biosphere flows with SQL group-by-style filters. One primitive replaces ad-hoc decomposition tools — express any 'how much X is in Y' question as one call. Examples:\n  - Total electricity in direct inputs: scope=direct, is_input=true, filter_name=Electricity, filter_unit=kWh\n  - Mass breakdown of direct inputs: scope=direct, is_input=true, filter_unit=kg, group_by=name\n  - Total energy across the supply chain: scope=supply_chain, max_depth=2, filter_classification=[\"Category type=energy:exact\"]\n  - Largest pasture occupation flow: scope=biosphere, filter_name=Occupation, pasture, group_by=name\n\nThe filter_classification parameter accepts a list of strings in \"System=Value[:exact]\" form (default mode is 'contains')."
        (props
            [ ("database",   "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            , ("scope",      "string", "direct | supply_chain | biosphere")
            ]
            [ ("is_input",              "boolean", "Only for scope=direct — true=inputs only, false=outputs only")
            , ("max_depth",             "integer", "Only for scope=supply_chain — max hops from the root activity")
            , ("filter_name",           "string",  "Case-insensitive substring on flow/activity name")
            , ("filter_name_not",       "string",  "Comma-separated substring exclude list")
            , ("filter_unit",           "string",  "Exact unit name")
            , ("filter_classification", "array",   "List of \"System=Value[:exact]\" strings; defaults to 'contains' mode")
            , ("filter_target_name",    "string",  "Only for scope=direct technosphere — filter by upstream activity name")
            , ("filter_is_reference",   "boolean", "Filter by reference-product flag (typically for outputs)")
            , ("group_by",              "string",  "name | flow_id | name_prefix | unit | classification.<system> | location | target_name")
            , ("aggregate",             "string",  "sum_quantity | count | share (default: sum_quantity)")
            ])
    , mkTool "get_supply_chain" "Get a flat list of all upstream activities in the supply chain. The 'quantity' field is the cumulative scaled amount relative to the functional unit (scaling_factor × root reference product amount). To get the per-step yield ratio between two connected entries, divide the supplier's scaling_factor by the consumer's scaling_factor."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            ]
            [ ("name", "string", "Filter by activity name")
            , ("location", "string", "Filter by location")
            , ("limit", "integer", "Max results (default 100)")
            , ("min_quantity", "number", "Min scaled quantity threshold")
            , ("max_depth", "integer", "Max depth from root (1 = direct inputs only)")
            , ("classification", "string", "Classification system name (e.g. 'Category', 'Category type')")
            , ("classification_value", "string", "Value within the classification system")
            , ("classification_match", "string", "Match mode: \"exact\" (case-insensitive equality) or \"contains\" (substring, default)")
            ])
    , mkTool "get_inventory" "Compute the Life Cycle Inventory (LCI) — biosphere flows (emissions and resource extractions) for an activity's full supply chain. Returns statistics and top flows by quantity."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            ]
            [ ("flow", "string", "Filter flows by name (case-insensitive substring)")
            , ("limit", "integer", "Max flows to return, sorted by absolute quantity (default 50)")
            ])
    , mkTool "get_lcia" "Compute Life Cycle Impact Assessment (LCIA) score for an activity. Returns the score, functional unit, and top contributing elementary flows."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
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
    , mkTool "get_characterization" "Look up characterization factors for a method matched against database flows. Without 'flow' filter, returns top factors by absolute value. With 'flow', searches by name. Shows CF value, direction, matched database flow, and match strategy."
        (props
            [ ("database", "string", "Database name")
            , ("method_id", "string", "Method UUID")
            ]
            [ ("flow", "string", "Filter by flow name (case-insensitive substring, matches both method CF name and database flow name)")
            , ("limit", "integer", "Max results (default 20)")
            ])
    , mkTool "get_contributing_flows" "Identify which elementary flows (emissions/resources) contribute most to a specific impact category. Answers 'which emissions drive my climate change score?'"
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            , ("method_id", "string", "Method UUID for the impact category")
            ]
            [ ("limit", "integer", "Max flows to return, sorted by contribution (default 20)")
            ])
    , mkTool "get_contributing_activities" "Identify which upstream activities contribute most to a specific impact category. Answers 'which suppliers drive my climate change score?' Uses exact matrix-based computation, valid even for cyclic supply chains."
        (props
            [ ("database", "string", "Database name")
            , ("process_id", "string", "Process ID (activityUUID_productUUID format)")
            , ("method_id", "string", "Method UUID for the impact category")
            ]
            [ ("limit", "integer", "Max processes to return, sorted by contribution (default 10)")
            ])
    , mkTool "list_geographies" "List all geography codes present in a database, with display names and parent regions. Use the 'geo' value as the geography filter in search_activities."
        (props
            [ ("database", "string", "Database name")
            ] [])
    , mkTool "list_classifications" "List classification systems in a database. Without 'system': returns system names and activity counts only (lightweight). With 'system': returns all values for that system. Add 'filter' to narrow values by substring."
        (props
            [ ("database", "string", "Database name") ]
            [ ("system", "string", "Classification system name to inspect (e.g. 'ISIC rev.4 ecoinvent'). If omitted, returns only system names and counts.")
            , ("filter", "string", "Substring filter applied to values when a system is specified (case-insensitive).")
            ])
    , mkTool "get_path_to" "Find the shortest supply chain path from a process to the first upstream activity whose name matches a pattern. Each step includes cumulative_quantity, scaling_factor, and local_step_ratio (upstream ÷ downstream scaling factors). total_ratio is the product of all local_step_ratio values — the end-to-end conversion factor."
        (props
            [ ("database",   "string", "Database name")
            , ("process_id", "string", "Root process ID (activityUUID_productUUID format)")
            , ("target",     "string", "Case-insensitive name substring to stop at")
            ] [])
    , mkTool "get_consumers" "Find all activities that transitively consume (depend on) a given supplier. Returns a flat list, each with a crDepth field: 1 = direct consumer, 2 = consumer of consumer, etc. Useful for tracing downstream use of a raw material — e.g. finding transformed food products in Agribalyse that use a raw ingredient."
        (props
            [ ("database",   "string", "Database name")
            , ("process_id", "string", "Process ID of the supplier (activityUUID_productUUID format)")
            ]
            [ ("name",                 "string",  "Filter by name (case-insensitive substring)")
            , ("location",             "string",  "Filter by geography/location (case-insensitive substring, e.g. 'FR', 'DE')")
            , ("product",              "string",  "Filter by product name (case-insensitive substring)")
            , ("preset",               "string",  "Name of a classification preset (from list_presets) — expands to its bundled filters")
            , ("classification",       "string",  "Classification system name (e.g. 'ISIC rev.4 ecoinvent')")
            , ("classification_value", "string",  "Classification value substring to match")
            , ("limit",                "integer", "Max results (default 1000)")
            , ("max_depth",            "integer", "Max hops from supplier (1 = direct consumers only)")
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

handleToolsCall :: DatabaseManager -> [ClassificationPreset] -> Text -> RpcRequest -> IO Value
handleToolsCall dbManager presets baseUrl req = do
    let rid = fromMaybe Null (rpcId req)
    case rpcParams req >>= parseCallParams of
        Nothing -> return $ rpcError rid (-32602) "Invalid params: expected {name, arguments}"
        Just (toolName, args) -> callTool dbManager presets baseUrl rid toolName args

parseCallParams :: Value -> Maybe (Text, KeyMap Value)
parseCallParams (Object o) = do
    String name <- KM.lookup "name" o
    let args = case KM.lookup "arguments" o of
            Just (Object a) -> a
            _               -> KM.empty
    return (name, args)
parseCallParams _ = Nothing

callTool :: DatabaseManager -> [ClassificationPreset] -> Text -> Value -> Text -> KeyMap Value -> IO Value
callTool dbManager presets baseUrl rid name args = case name of
    "list_databases"          -> callListDatabases dbManager rid
    "list_presets"            -> callListPresets presets rid
    "search_activities"       -> withDb dbManager rid args $ callSearchActivities presets rid args
    "search_flows"            -> withDb dbManager rid args $ callSearchFlows rid args
    "get_activity"            -> withDb dbManager rid args $ callGetActivity rid args
    "get_tree"                -> withDb dbManager rid args $ callGetTree rid args
    "get_supply_chain"        -> withDb dbManager rid args $ callGetSupplyChain rid args
    "aggregate"               -> withDb dbManager rid args $ callAggregate rid args
    "get_inventory"           -> withDb dbManager rid args $ callGetInventory rid args
    "get_lcia"                -> callGetLCIA dbManager baseUrl rid args
    "list_methods"            -> callListMethods dbManager rid
    "get_flow_mapping"        -> callGetFlowMapping dbManager rid args
    "get_characterization"    -> callGetCharacterization dbManager rid args
    "get_contributing_flows"   -> callGetContributingFlows dbManager baseUrl rid args
    "get_contributing_activities" -> callGetContributingActivities dbManager baseUrl rid args
    "list_geographies"         -> callListGeographies dbManager rid args
    "list_classifications"     -> withDb dbManager rid args $ callListClassifications rid args
    "get_path_to"              -> withDb dbManager rid args $ callGetPathTo rid args
    "get_consumers"            -> withDb dbManager rid args $ callGetConsumers presets rid args
    _                          -> return $ toolError rid ("Unknown tool: " <> name)

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

boolArg :: Text -> KeyMap Value -> Maybe Bool
boolArg key args = case KM.lookup (fromText key) args of
    Just (Bool b) -> Just b
    _             -> Nothing

-- | Read an argument that may be either a JSON array of strings or a single string.
textArrayArg :: Text -> KeyMap Value -> [Text]
textArrayArg key args = case KM.lookup (fromText key) args of
    Just (Array arr) -> [t | String t <- toList arr]
    Just (String t)  -> [t]
    _                -> []
  where
    toList = foldr (:) []

-- ---------------------------------------------------------------------------
-- Tool implementations
-- ---------------------------------------------------------------------------

callListDatabases :: DatabaseManager -> Value -> IO Value
callListDatabases dbManager rid = do
    loaded <- readTVarIO (dmLoadedDbs dbManager)
    let mkDbEntry ld =
            let cfg = ldConfig ld
                base = [ "name"         .= dcName cfg
                       , "display_name" .= dcDisplayName cfg
                       ]
                withDesc = case dcDescription cfg of
                    Nothing -> base
                    Just d  -> base ++ ["description" .= d]
                withFmt = case dcFormat cfg of
                    Nothing  -> withDesc
                    Just fmt -> withDesc ++ ["format" .= fmt]
            in object withFmt
        entries = map mkDbEntry (M.elems loaded)
    return $ toolSuccessJson rid $ object [ "databases" .= entries ]

callListPresets :: [ClassificationPreset] -> Value -> IO Value
callListPresets presets rid = return $ toolSuccessJson rid $ toJSON
    [ object
        [ "name"        .= cpName p
        , "label"       .= cpLabel p
        , "description" .= cpDescription p
        , "filters"     .= [ object ["system" .= ceSystem e, "value" .= ceValue e, "mode" .= ceMode e]
                             | e <- cpFilters p ]
        ]
    | p <- presets ]

callSearchActivities :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchActivities presets rid args (db, _) = do
    let name    = textArg "name" args
        geo     = textArg "geo" args
        product' = textArg "product" args
        limit   = intArg "limit" args
        exact   = fromMaybe False (boolArg "exact" args)
        isExact  = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
        presetFilters = case textArg "preset" args of
            Just pn -> case L.find (\p -> cpName p == pn) presets of
                Just p  -> [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
                Nothing -> []
            Nothing -> []
        explicitFilters = case (textArg "classification" args, textArg "classification_value" args) of
            (Just sys, Just val) -> [(sys, val, isExact)]
            _                   -> []
        classFilters = presetFilters ++ explicitFilters
    result <- Service.searchActivities db name geo product' classFilters exact (limit <|> Just 20) Nothing Nothing Nothing
    case result of
        Left err  -> return $ toolError rid (T.pack $ show err)
        Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a       <|> _ = a

callListClassifications :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callListClassifications rid args (db, _) =
    let systems = Service.getClassifications db
        mSystem = textArg "system" args
        mFilter = textArg "filter" args
    in return $ toolSuccessJson rid $ case mSystem of
        Nothing ->
            toJSON [ object ["name" .= csName s, "activityCount" .= csActivityCount s]
                   | s <- systems ]
        Just sys ->
            case L.find (\s -> T.toLower (csName s) == T.toLower sys) systems of
                Nothing -> object ["error" .= ("Classification system not found: " <> sys)]
                Just s  ->
                    let vals = case mFilter of
                            Nothing -> csValues s
                            Just f  -> L.filter (T.isInfixOf (T.toLower f) . T.toLower) (csValues s)
                    in object ["name" .= csName s, "activityCount" .= csActivityCount s, "values" .= vals]

callSearchFlows :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchFlows rid args (db, _) = do
    let query = textArg "query" args
        limit = intArg "limit" args
    result <- Service.searchFlows db query Nothing (limit <|> Just 20) Nothing Nothing Nothing
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
                Right val
                    | noFilters -> return $ toolSuccessJson rid val
                    | otherwise -> case fromJSON val of
                        Error _    -> return $ toolSuccessJson rid val
                        Success ai ->
                            let filtered = ai { piActivity = (piActivity ai) { pfaExchanges = filter matchExchange (pfaExchanges (piActivity ai)) } }
                            in return $ toolSuccessJson rid (toJSON filtered)
  where
    exchangeType = textArg "exchange_type" args
    flowFilter   = textArg "flow" args
    isInputFilter = boolArg "is_input" args
    noFilters    = exchangeType `elem` [Nothing, Just "all"]
                 && isNothing flowFilter
                 && isNothing isInputFilter
    matchExchange ewu = matchType ewu && matchFlow ewu && matchIsInput ewu
    matchType ewu = case exchangeType of
        Just "biosphere"    -> ewuFlowCategory ewu /= "technosphere"
        Just "technosphere" -> ewuFlowCategory ewu == "technosphere"
        _                   -> True
    matchFlow ewu = case flowFilter of
        Nothing -> True
        Just q  -> T.isInfixOf (T.toLower q) (T.toLower (ewuFlowName ewu))
    matchIsInput ewu = case isInputFilter of
        Nothing -> True
        Just want -> exchangeIsInput (ewuExchange ewu) == want

callGetTree :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetTree rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            let depth  = fromMaybe 2 (intArg "depth" args)
                nameF  = textArg "name" args
            in case Service.getActivityTree db pid depth nameF of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val

callGetSupplyChain :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetSupplyChain rid args (db, solver) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid -> do
            let isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
                classFilters = case (textArg "classification" args, textArg "classification_value" args) of
                    (Just sys, Just val) -> [(sys, val, isExact)]
                    _                    -> []
                af = Service.ActivityFilter
                    { Service.afName = textArg "name" args
                    , Service.afLocation = textArg "location" args
                    , Service.afProduct = Nothing
                    , Service.afClassifications = classFilters
                    , Service.afLimit = intArg "limit" args
                    , Service.afOffset = Nothing
                    , Service.afMaxDepth = intArg "max_depth" args
                    , Service.afMinQuantity = doubleArg "min_quantity" args
                    , Service.afSort = Nothing
                    , Service.afOrder = Nothing }
            result <- Service.getSupplyChain db solver pid af False
            case result of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid (toJSON val)

-- | Generic SQL-group-by aggregation. One small primitive for "how much X is
-- in Y" questions — replaces ad-hoc decomposition tools.
callAggregate :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callAggregate rid args (db, solver) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid -> case scopeFromArg of
            Left err -> return $ toolError rid err
            Right scope -> case aggFnFromArg of
                Left err -> return $ toolError rid err
                Right fn -> do
                    let params = Agg.AggregateParams
                            { Agg.apScope                 = scope
                            , Agg.apIsInput               = boolArg "is_input" args
                            , Agg.apMaxDepth              = intArg "max_depth" args
                            , Agg.apFilterName            = textArg "filter_name" args
                            , Agg.apFilterNameNot         =
                                maybe [] (map T.strip . T.splitOn ",") (textArg "filter_name_not" args)
                            , Agg.apFilterUnit            = textArg "filter_unit" args
                            , Agg.apFilterClassifications =
                                mapMaybe parseClassFilter (textArrayArg "filter_classification" args)
                            , Agg.apFilterTargetName      = textArg "filter_target_name" args
                            , Agg.apFilterIsReference     = boolArg "filter_is_reference" args
                            , Agg.apGroupBy               = textArg "group_by" args
                            , Agg.apAggregate             = fn
                            }
                    result <- Agg.aggregate db solver pid params
                    case result of
                        Left err  -> return $ toolError rid (T.pack $ show err)
                        Right agg -> return $ toolSuccessJson rid (toJSON agg)
  where
    scopeFromArg = case textArg "scope" args of
        Just "direct"       -> Right Agg.ScopeDirect
        Just "supply_chain" -> Right Agg.ScopeSupplyChain
        Just "biosphere"    -> Right Agg.ScopeBiosphere
        Nothing             -> Left "Missing required parameter: scope (direct | supply_chain | biosphere)"
        Just other          -> Left ("Invalid scope: " <> other)
    aggFnFromArg = case textArg "aggregate" args of
        Nothing             -> Right Agg.AggSum
        Just "sum_quantity" -> Right Agg.AggSum
        Just "count"        -> Right Agg.AggCount
        Just "share"        -> Right Agg.AggShare
        Just other          -> Left ("Invalid aggregate fn: " <> other)
    parseClassFilter raw =
        let (sys, rest) = T.breakOn "=" raw
        in if T.null rest
             then Nothing
             else
               let valAndMode = T.drop 1 rest
                   (val, mode) = T.breakOn ":" valAndMode
                   isExact = T.drop 1 mode == "exact"
               in Just (T.strip sys, T.strip val, isExact)

callGetPathTo :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetPathTo rid args (db, solver) =
    case (textArg "process_id" args, textArg "target" args) of
        (Nothing, _) -> return $ toolError rid "Missing required parameter: process_id"
        (_, Nothing) -> return $ toolError rid "Missing required parameter: target"
        (Just pid, Just target) -> do
            result <- Service.getPathTo db solver pid target
            case result of
                Left err  -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val

callGetConsumers :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetConsumers presets rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            let isExact      = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
                presetFilters = case textArg "preset" args of
                    Just pn -> case L.find (\p -> cpName p == pn) presets of
                        Just p  -> [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
                        Nothing -> []
                    Nothing -> []
                explicitFilters = case (textArg "classification" args, textArg "classification_value" args) of
                    (Just sys, Just val) -> [(sys, val, isExact)]
                    _                   -> []
                classFilters = presetFilters ++ explicitFilters
                af = Service.ActivityFilter
                    { Service.afName = textArg "name" args
                    , Service.afLocation = textArg "location" args
                    , Service.afProduct = textArg "product" args
                    , Service.afClassifications = classFilters
                    , Service.afLimit = intArg "limit" args
                    , Service.afOffset = Nothing
                    , Service.afMaxDepth = intArg "max_depth" args
                    , Service.afMinQuantity = Nothing
                    , Service.afSort = Nothing
                    , Service.afOrder = Nothing }
            in case Service.getConsumers db pid af of
                Left err      -> return $ toolError rid (T.pack $ show err)
                Right results -> return $ toolSuccessJson rid (toJSON results)

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

callGetLCIA :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetLCIA dbManager baseUrl rid args = do
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
                    case UUID.fromText methodIdText of
                        Nothing -> return $ toolError rid "Invalid method UUID format"
                        Just uuid ->
                            case filter (\(_, m) -> methodId m == uuid) loadedMethods of
                                [] -> return $ toolError rid "Method not found"
                                ((colName, method):_) ->
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
                                                webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText <> "/lcia/" <> encodeSegment colName <> "/" <> UUID.toText uuid
                                            let hasNeg = any (\(_, _, c) -> c < 0) contribs
                                            return $ toolSuccessJson rid $ object
                                                [ "method"                     .= methodName method
                                                , "category"                   .= methodCategory method
                                                , "score"                      .= score
                                                , "unit"                       .= methodUnit method
                                                , "functional_unit"            .= functionalUnit
                                                , "mapped_flows"               .= (msTotal stats - msUnmatched stats)
                                                , "has_negative_contributions" .= hasNeg
                                                , "web_url"                    .= webUrl
                                                , "top_flows"                  .= [ object
                                                    [ "flow_name"           .= flowName f
                                                    , "contribution"        .= c
                                                    , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                                    , "flow_id"             .= UUID.toText (flowId f)
                                                    , "category"            .= flowCategory f
                                                    , "compartment"         .= flowSubcompartment f
                                                    , "cf_value"            .= mcfValue cf
                                                    , "flow_unit"           .= getUnitNameForFlow (dbUnits db) f
                                                    ]
                                                    | (cf, f, c) <- topFlows
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

callGetCharacterization :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetCharacterization dbManager rid args =
    case (textArg "database" args, textArg "method_id" args) of
        (Nothing, _) -> return $ toolError rid "Missing required parameter: database"
        (_, Nothing) -> return $ toolError rid "Missing required parameter: method_id"
        (Just dbName, Just methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    eMethod <- resolveMethod dbManager methodIdText
                    case eMethod of
                        Left err -> return $ toolError rid err
                        Right (_, method) -> do
                            let db      = ldDatabase ld
                                lim     = fromMaybe 20 (intArg "limit" args)
                                flowQ   = textArg "flow" args
                                queryLower = fmap T.toLower flowQ
                                mappers = prMappers (dmPlugins dbManager)
                            mappings <- mapMethodToFlows mappers db method
                            let matched = [ (cf, f, strat)
                                          | (cf, Just (f, strat)) <- mappings
                                          , matchQuery queryLower (mcfFlowName cf) (flowName f)
                                          ]
                                sorted = L.sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
                                top = take lim sorted
                                mkEntry (cf, f, strat) = object
                                    [ "cf_flow_name"   .= mcfFlowName cf
                                    , "cf_value"       .= mcfValue cf
                                    , "cf_unit"        .= mcfUnit cf
                                    , "direction"      .= (case mcfDirection cf of Input -> "Input" :: Text; Output -> "Output")
                                    , "db_flow_name"   .= flowName f
                                    , "flow_id"        .= UUID.toText (flowId f)
                                    , "flow_unit"      .= getUnitNameForFlow (dbUnits db) f
                                    , "category"       .= flowCategory f
                                    , "compartment"    .= flowSubcompartment f
                                    , "match_strategy" .= show strat
                                    ]
                            return $ toolSuccessJson rid $ object
                                [ "method"  .= methodName method
                                , "unit"    .= methodUnit method
                                , "matches" .= length matched
                                , "shown"   .= length top
                                , "factors" .= map mkEntry top
                                ]
  where
    matchQuery Nothing _ _ = True
    matchQuery (Just q) cfName dbFlowName = T.isInfixOf q (T.toLower cfName) || T.isInfixOf q (T.toLower dbFlowName)

-- | Helper: resolve method from UUID text, also returning its collection name
resolveMethod :: DatabaseManager -> Text -> IO (Either Text (Text, Method))
resolveMethod dbManager methodIdText =
    case UUID.fromText methodIdText of
        Nothing -> return $ Left "Invalid method UUID format"
        Just uuid -> do
            loadedMethods <- DM.getLoadedMethods dbManager
            case filter (\(_, m) -> methodId m == uuid) loadedMethods of
                []          -> return $ Left "Method not found"
                ((col, m):_) -> return $ Right (col, m)

callGetContributingFlows :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetContributingFlows dbManager baseUrl rid args =
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
                        Right (colName, method) ->
                            case Service.resolveActivityAndProcessId (ldDatabase ld) pidText of
                                Left err -> return $ toolError rid (T.pack $ show err)
                                Right (processId, _) -> do
                                    let db      = ldDatabase ld
                                        lim     = fromMaybe 20 (intArg "limit" args)
                                        mappers = prMappers (dmPlugins dbManager)
                                        webUrl  = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText <> "/contributing-flows/" <> encodeSegment colName <> "/" <> methodIdText
                                    unitCfg  <- DM.getMergedUnitConfig dbManager
                                    scalingVec <- computeScalingVector db processId
                                    let inventory = applyBiosphereMatrix db scalingVec
                                    mappings <- mapMethodToFlows mappers db method
                                    let score   = computeLCIAScore unitCfg (dbUnits db) (dbFlows db) inventory mappings
                                        contribs = L.sortOn (\(_,_,c) -> negate (abs c)) (mapMaybe (flowContribution inventory) mappings)
                                        top      = take lim contribs
                                    let hasNeg = any (\(_, _, c) -> c < 0) contribs
                                    return $ toolSuccessJson rid $ object
                                        [ "method"                    .= methodName method
                                        , "unit"                      .= methodUnit method
                                        , "total_score"               .= score
                                        , "has_negative_contributions" .= hasNeg
                                        , "web_url"                   .= webUrl
                                        , "top_flows"                 .= [ object
                                            [ "flow_name"           .= flowName f
                                            , "contribution"        .= c
                                            , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                            , "flow_id"             .= UUID.toText (flowId f)
                                            , "category"            .= flowCategory f
                                            , "compartment"         .= flowSubcompartment f
                                            , "cf_value"            .= mcfValue cf
                                            ]
                                            | (cf, f, c) <- top
                                            ]
                                        ]

callGetContributingActivities :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetContributingActivities dbManager baseUrl rid args =
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
                        Right (colName, method) ->
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
                                                procWebUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText' <> "/contributing-activities/" <> encodeSegment colName <> "/" <> methodIdText
                                            in object
                                                [ "process_id"           .= pidText'
                                                , "activity_name"        .= actName
                                                , "product_name"         .= prodName
                                                , "location"             .= actLoc
                                                , "contribution"         .= c
                                                , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                                , "web_url"              .= procWebUrl
                                                ]
                                    let hasNeg = any (\(_, c) -> c < 0) top
                                    return $ toolSuccessJson rid $ object
                                        [ "method"                     .= methodName method
                                        , "unit"                       .= methodUnit method
                                        , "total_score"                .= score
                                        , "has_negative_contributions" .= hasNeg
                                        , "processes"                  .= map mkEntry top
                                        ]

callListGeographies :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callListGeographies dbManager rid args =
    case textArg "database" args of
        Nothing -> return $ toolError rid "Missing required parameter: database"
        Just dbName -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db      = ldDatabase ld
                        geoMap  = dmGeographies dbManager
                        codes   = L.sort $ M.keys (idxByLocation (dbIndexes db))
                        mkEntry code =
                            let (displayName, parents) = M.findWithDefault (code, []) code geoMap
                                parentStr = T.intercalate "|" parents
                            in object
                                [ "geo"            .= code
                                , "display_name"   .= displayName
                                , "parent_regions" .= parentStr
                                ]
                    return $ toolSuccessJson rid $ object
                        [ "geographies" .= map mkEntry codes ]

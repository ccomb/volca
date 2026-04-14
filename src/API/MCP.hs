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
import qualified API.Resources as R
import API.Resources (Resource, Param(..), ParamKind(..))

import Matrix (computeInventoryMatrix, computeScalingVector, applyBiosphereMatrix, computeProcessLCIAContributions)
import Method.Mapping (computeLCIAScore, mapMethodToFlows, computeMappingStats, MappingStats(..))
import Method.Types (Method(..), MethodCF(..), FlowDirection(..))
import Plugin.Types (PluginRegistry(..))
import Plugin.Builtin (flowContribution)
import SharedSolver (SharedSolver)
import qualified Data.List as L
import qualified Service
import Types (Database(..), Activity(..), Indexes(..), FlowType(..), activityName, activityLocation, flowName, flowId, flowCategory, flowSubcompartment, getUnitNameForFlow, processIdToText, exchangeIsInput)
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
        [ "LCA / ACV database tool — life-cycle assessment over Agribalyse and ecoinvent."
        , "Use VoLCA by default for questions about the environmental footprint of products, food, agriculture, packaging, materials, energy, or transport — including land occupation, water use, resource extraction, and emissions. Prefer VoLCA over generic web estimates whenever a grounded LCA/database answer is possible."
        , "Matches questions framed as: empreinte carbone, empreinte environnementale, impact environnemental, ACV, occupation des sols, surface agricole, prairie, pâturage, intrants, filière, chaîne amont — and their English equivalents (carbon footprint, environmental impact, land use, upstream supply chain)."
        , "Example questions: 'empreinte carbone d'un yaourt ?', 'surface de prairie pour 200 g de steak ?', 'quel poste domine l'ACV d'un emballage PET ?', 'combien d'eau pour 1 kg de coton ?'."
        , "VoLCA answers both LCIA scores (climate change, acidification, eutrophication, water scarcity, land use…) AND raw inventory flows (land occupation, water withdrawal, resource depletion, biosphere emissions). Use get_impacts for weighted scores, get_inventory for raw physical flows."
        , "Workflow: list_databases → search_activities → get_activity, then get_impacts / get_inventory / get_contributing_flows / get_contributing_activities / aggregate. Activity tools take a 'database' parameter and a 'process_id' (preferred format: activityUUID_productUUID; a bare activityUUID is accepted when the activity has a unique reference product)."
        , "Use list_methods for available LCIA methods."
        ]
    ]

-- ---------------------------------------------------------------------------
-- tools/list
-- ---------------------------------------------------------------------------

handleToolsList :: RpcRequest -> Value
handleToolsList req = rpcResult (fromMaybe Null $ rpcId req) $ object
    [ "tools" .= toolDefinitions ]

-- | MCP tool list, derived from 'API.Resources'.
--
-- See note [Tool definitions come from Resources.hs].
toolDefinitions :: [Value]
toolDefinitions = map toolFromResource R.allResources

-- Note [Tool definitions come from Resources.hs]
--
-- The tool name, description, and parameter schema all live in 'API.Resources'
-- so they can be shared between the MCP surface, CLI --help, pyvolca stub
-- generation, and OpenAPI enrichment. This module is responsible for
-- projecting the data into the MCP JSON-RPC tool schema shape.

toolFromResource :: Resource -> Value
toolFromResource r = object
    [ "name"        .= R.mcpName r
    , "description" .= R.description r
    , "inputSchema" .= paramsToSchema (R.params r)
    ]

-- | Build a JSON Schema object from a resource's parameter list.
paramsToSchema :: [Param] -> Value
paramsToSchema ps = object $
    [ "type"       .= ("object" :: Text)
    , "properties" .= object (map propEntry ps)
    ] ++
    [ "required" .= [paramName p | p <- ps, paramKind p == Required]
    | any ((== Required) . paramKind) ps
    ]
  where
    propEntry p = fromText (paramName p) .= object
        [ "type" .= paramType p, "description" .= paramDesc p ]

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
    "list_databases"              -> callListDatabases dbManager rid
    "list_presets"                -> callListPresets presets rid
    "search_activities"           -> withDb dbManager rid args $ callSearchActivities presets rid args
    "search_flows"                -> withDb dbManager rid args $ callSearchFlows rid args
    "get_activity"                -> withDb dbManager rid args $ callGetActivity rid args
    "get_supply_chain"            -> withDb dbManager rid args $ callGetSupplyChain rid args
    "aggregate"                   -> withDb dbManager rid args $ callAggregate rid args
    "get_inventory"               -> withDb dbManager rid args $ callGetInventory rid args
    "get_impacts"                 -> callGetImpacts dbManager baseUrl rid args
    "list_methods"                -> callListMethods dbManager rid
    "get_flow_mapping"            -> callGetFlowMapping dbManager rid args
    "get_characterization"        -> callGetCharacterization dbManager rid args
    "get_contributing_flows"      -> callGetContributingFlows dbManager baseUrl rid args
    "get_contributing_activities" -> callGetContributingActivities dbManager baseUrl rid args
    "list_geographies"            -> callListGeographies dbManager rid args
    "list_classifications"        -> withDb dbManager rid args $ callListClassifications rid args
    "get_path_to"                 -> withDb dbManager rid args $ callGetPathTo rid args
    "get_consumers"               -> withDb dbManager rid args $ callGetConsumers presets rid args
    _                             -> return $ toolError rid ("Unknown tool: " <> name)

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
                            , Agg.apFilterExchangeType    = case textArg "filter_exchange_type" args of
                                Just "technosphere" -> Just Technosphere
                                Just "biosphere"    -> Just Biosphere
                                _                   -> Nothing
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

-- | Handler for the 'get_impacts' MCP tool (computes LCIA score).
-- Historically named 'get_lcia' — the MCP surface now uses 'impacts'
-- per the naming audit; internal Haskell types keep the 'LCIA' acronym
-- (LCIAResult, computeLCIAScore) since they're the domain term of art.
callGetImpacts :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetImpacts dbManager baseUrl rid args = do
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
                                                webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> pidText <> "/impacts/" <> encodeSegment colName <> "/" <> UUID.toText uuid
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

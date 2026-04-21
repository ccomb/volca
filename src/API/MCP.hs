{-# LANGUAGE OverloadedStrings #-}

{- | MCP (Model Context Protocol) server endpoint.
Implements Streamable HTTP transport (MCP spec 2025-03-26).
POST /mcp handles initialize, tools/list, tools/call (JSON or SSE response).
GET  /mcp opens an SSE stream for server-initiated messages (stateless: closes immediately).
-}
module API.MCP (mcpApp, toolDefinitions) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Network.HTTP.Types (hContentType, status200, status202, status405)
import Network.URI (escapeURIString, isUnreserved)
import Network.Wai (Application, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import System.Random (randomIO)

import API.Resources (Param (..), ParamKind (..), Resource)
import qualified API.Resources as R
import Config (ClassificationEntry (..), ClassificationPreset (..), DatabaseConfig (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), getDatabase)
import qualified Database.Manager as DM

import API.Types (ActivityForAPI (..), ActivityInfo (..), ClassificationSystem (..), ExchangeWithUnit (..), InventoryExport (..), InventoryFlowDetail (..), Substitution (..))
import Control.Monad (unless)
import qualified Data.List as L
import Method.Mapping (MappingStats (..), computeLCIAScoreFromTables, computeMappingStats, inventoryContributions)
import Method.Types (FlowDirection (..), Method (..), MethodCF (..))
import Network.HTTP.Types.Header (hAccept, hHost)
import Numeric (showFFloat)
import Plugin.Types ()
import Progress (ProgressLevel (Warning), reportProgress)
import qualified Service
import qualified Service.Aggregate as Agg
import SharedSolver (SharedSolver, computeInventoryMatrixWithDepsCached, crossDBProcessContributions)
import Types (Activity (..), Database (..), FlowDB, FlowType (..), Indexes (..), ProcessId, UnitDB, activityLocation, activityName, exchangeIsInput, flowCategory, flowId, flowName, flowSubcompartment, getUnitNameForFlow, processIdToText, unresolvedCount)
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- JSON-RPC 2.0 types
-- ---------------------------------------------------------------------------

data RpcRequest = RpcRequest
    { rpcId :: Maybe Value -- Nothing = notification
    , rpcMethod :: Text
    , rpcParams :: Maybe Value
    }
    deriving (Show)

instance FromJSON RpcRequest where
    parseJSON = withObject "RpcRequest" $ \v ->
        RpcRequest
            <$> v .:? "id"
            <*> v .: "method"
            <*> v .:? "params"

rpcResult :: Value -> Value -> Value
rpcResult rid res =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= rid
        , "result" .= res
        ]

rpcError :: Value -> Int -> Text -> Value
rpcError rid code msg =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= rid
        , "error" .= object ["code" .= code, "message" .= msg]
        ]

toolError :: Value -> Text -> Value
toolError rid msg =
    rpcResult rid $
        object
            [ "content" .= [object ["type" .= ("text" :: Text), "text" .= msg]]
            , "isError" .= True
            ]

toolSuccessJson :: Value -> Value -> Value
toolSuccessJson rid val =
    rpcResult rid $
        object
            [ "content" .= [object ["type" .= ("text" :: Text), "text" .= encodeAsText val]]
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
    stateRef <- newIORef McpState{mcpSessionId = sessionId}
    return $ \req respond -> do
        let method = requestMethod req
            hdrs = requestHeaders req
            hostHeader = fromMaybe "localhost" $ lookup hHost hdrs
            baseUrl = "http://" <> TE.decodeUtf8 hostHeader
            acceptHdr = fromMaybe "" $ lookup hAccept hdrs
            wantsSse = "text/event-stream" `BS.isInfixOf` acceptHdr
        st <- readIORef stateRef
        case method of
            -- GET: open SSE stream for server-initiated messages.
            -- VoLCA is stateless so we return an empty stream immediately.
            "GET" ->
                respond $
                    responseLBS
                        status200
                        [ (hContentType, "text/event-stream; charset=utf-8")
                        , ("Cache-Control", "no-cache")
                        , ("Connection", "keep-alive")
                        , ("Mcp-Session-Id", TE.encodeUtf8 (mcpSessionId st))
                        ]
                        ""
            "POST" -> do
                body <- strictRequestBody req
                case eitherDecode body of
                    Left err ->
                        respond $ jsonResponse (mcpSessionId st) $ rpcError Null (-32700) (T.pack $ "Parse error: " ++ err)
                    Right rpcReq -> do
                        resp <- handleRpc dbManager presets baseUrl st rpcReq
                        case resp of
                            Nothing ->
                                respond $
                                    responseLBS
                                        status202
                                        [ (hContentType, "application/json")
                                        , ("Mcp-Session-Id", TE.encodeUtf8 (mcpSessionId st))
                                        ]
                                        ""
                            Just val ->
                                if wantsSse
                                    then respond $ sseResponse (mcpSessionId st) val
                                    else respond $ jsonResponse (mcpSessionId st) val
            _ ->
                respond $
                    responseLBS status405 [(hContentType, "application/json")] $
                        encode $
                            rpcError Null (-32700) "Method not allowed"
  where
    jsonResponse sid v =
        responseLBS
            status200
            [ (hContentType, "application/json")
            , ("X-Content-Type-Options", "nosniff")
            , ("Mcp-Session-Id", TE.encodeUtf8 sid)
            ]
            (encode v)
    -- SSE format: each JSON-RPC message is one SSE event
    sseResponse sid v =
        responseLBS
            status200
            [ (hContentType, "text/event-stream; charset=utf-8")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            , ("Mcp-Session-Id", TE.encodeUtf8 sid)
            ]
            ("event: message\ndata: " <> encode v <> "\n\n")

-- ---------------------------------------------------------------------------
-- RPC dispatch
-- ---------------------------------------------------------------------------

handleRpc :: DatabaseManager -> [ClassificationPreset] -> Text -> McpState -> RpcRequest -> IO (Maybe Value)
handleRpc dbManager presets baseUrl _st req = case rpcMethod req of
    "initialize" -> Just <$> handleInitialize req
    "notifications/initialized" -> return Nothing -- notification, no response
    "tools/list" -> return $ Just $ handleToolsList req
    "tools/call" -> Just <$> handleToolsCall dbManager presets baseUrl req
    "ping" -> return $ Just $ rpcResult (rid req) (object [])
    other ->
        return $
            Just $
                rpcError
                    (rid req)
                    (-32601)
                    ("Method not found: " <> other)
  where
    rid r = fromMaybe Null (rpcId r)

-- ---------------------------------------------------------------------------
-- initialize
-- ---------------------------------------------------------------------------

handleInitialize :: RpcRequest -> IO Value
handleInitialize req =
    return $
        rpcResult (fromMaybe Null $ rpcId req) $
            object
                [ "protocolVersion" .= ("2025-03-26" :: Text)
                , "capabilities" .= object ["tools" .= object []]
                , "serverInfo"
                    .= object
                        [ "name" .= ("volca" :: Text)
                        , "version" .= ("0.6.0" :: Text)
                        ]
                , "instructions"
                    .= T.unlines
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
handleToolsList req =
    rpcResult (fromMaybe Null $ rpcId req) $
        object
            ["tools" .= toolDefinitions]

{- | MCP tool list, derived from 'API.Resources'.

See note [Tool definitions come from Resources.hs].
-}
toolDefinitions :: [Value]
toolDefinitions = map toolFromResource R.allResources

-- Note [Tool definitions come from Resources.hs]
--
-- The tool name, description, and parameter schema all live in 'API.Resources'
-- so they can be shared between the MCP surface, CLI --help, pyvolca stub
-- generation, and OpenAPI enrichment. This module is responsible for
-- projecting the data into the MCP JSON-RPC tool schema shape.

toolFromResource :: Resource -> Value
toolFromResource r =
    object
        [ "name" .= R.mcpName r
        , "description" .= R.description r
        , "inputSchema" .= paramsToSchema (R.params r)
        ]

-- | Build a JSON Schema object from a resource's parameter list.
paramsToSchema :: [Param] -> Value
paramsToSchema ps =
    object $
        [ "type" .= ("object" :: Text)
        , "properties" .= object (map propEntry ps)
        ]
            ++ [ "required" .= [paramName p | p <- ps, paramKind p == Required]
               | any ((== Required) . paramKind) ps
               ]
  where
    propEntry p =
        fromText (paramName p)
            .= object
                ( ["type" .= paramType p, "description" .= paramDesc p]
                    ++ arrayItemsFor p
                )

    -- Arrays in the 'Param' schema default to items of type string; the
    -- exception is the shared @substitutions@ parameter, whose entries are
    -- @{from, to, consumer}@ objects. We special-case the name rather than
    -- extending the 'Param' record to avoid touching every call site.
    arrayItemsFor p
        | paramType p /= "array" = []
        | paramName p == "substitutions" = ["items" .= substitutionItemSchema]
        | otherwise = ["items" .= object ["type" .= ("string" :: Text)]]

    substitutionItemSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "from" .= stringField "Source supplier ProcessId (bare or dbName::pid)"
                    , "to" .= stringField "Replacement supplier ProcessId (bare or dbName::pid)"
                    , "consumer" .= stringField "Consumer activity ProcessId (root DB only)"
                    ]
            , "required" .= (["from", "to", "consumer"] :: [Text])
            ]
    stringField desc = object ["type" .= ("string" :: Text), "description" .= (desc :: Text)]

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
            _ -> KM.empty
    return (name, args)
parseCallParams _ = Nothing

callTool :: DatabaseManager -> [ClassificationPreset] -> Text -> Value -> Text -> KeyMap Value -> IO Value
callTool dbManager presets baseUrl rid name args = case name of
    "list_databases" -> callListDatabases dbManager rid
    "list_presets" -> callListPresets presets rid
    "search_activities" -> withDb dbManager rid args $ callSearchActivities presets rid args
    "search_flows" -> withDb dbManager rid args $ callSearchFlows rid args
    "get_activity" -> withDb dbManager rid args $ callGetActivity rid args
    "get_supply_chain" -> callGetSupplyChain dbManager rid args
    "aggregate" -> withDb dbManager rid args $ callAggregate dbManager rid args
    "get_inventory" -> callGetInventory dbManager rid args
    "get_impacts" -> callGetImpacts dbManager baseUrl rid args
    "list_methods" -> callListMethods dbManager rid
    "get_flow_mapping" -> callGetFlowMapping dbManager rid args
    "get_characterization" -> callGetCharacterization dbManager rid args
    "get_contributing_flows" -> callGetContributingFlows dbManager baseUrl rid args
    "get_contributing_activities" -> callGetContributingActivities dbManager baseUrl rid args
    "list_geographies" -> callListGeographies dbManager rid args
    "list_classifications" -> withDb dbManager rid args $ callListClassifications rid args
    "get_path_to" -> withDb dbManager rid args $ callGetPathTo rid args
    "get_consumers" -> withDb dbManager rid args $ callGetConsumers presets rid args
    _ -> return $ toolError rid ("Unknown tool: " <> name)

-- Helper: extract database, then run action
withDb ::
    DatabaseManager ->
    Value ->
    KeyMap Value ->
    ((Database, SharedSolver) -> IO Value) ->
    IO Value
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
    _ -> Nothing

intArg :: Text -> KeyMap Value -> Maybe Int
intArg key args = case KM.lookup (fromText key) args of
    Just (Number n) -> Just (round n)
    _ -> Nothing

doubleArg :: Text -> KeyMap Value -> Maybe Double
doubleArg key args = case KM.lookup (fromText key) args of
    Just (Number n) -> Just (realToFrac n)
    _ -> Nothing

boolArg :: Text -> KeyMap Value -> Maybe Bool
boolArg key args = case KM.lookup (fromText key) args of
    Just (Bool b) -> Just b
    _ -> Nothing

{- | Require a text argument, returning 'Left' with a standard error message
when absent. Composes applicatively with 'Either': callers can gather N
required fields with @(,,) \<$\> requireText \"a\" args \<*\> requireText \"b\" args
\<*\> requireText \"c\" args@ and match on the single 'Either' instead of an
@N@-tuple 'case' cascade.
-}
requireText :: Text -> KeyMap Value -> Either Text Text
requireText key args =
    maybe (Left ("Missing required parameter: " <> key)) Right (textArg key args)

-- | Read an argument that may be either a JSON array of strings or a single string.
textArrayArg :: Text -> KeyMap Value -> [Text]
textArrayArg key args = case KM.lookup (fromText key) args of
    Just (Array arr) -> [t | String t <- toList arr]
    Just (String t) -> [t]
    _ -> []
  where
    toList = foldr (:) []

{- | Parse the optional 'substitutions' argument into '[Substitution]'.
Ignores malformed entries silently at parse level — missing or wrong-shape
subs become an empty list (the caller treats empty as \"no what-if\").
Returns 'Nothing' if the JSON is malformed enough to suggest user intent
that can't be satisfied (e.g. entry is a non-object), letting the caller
surface a 422 rather than running a baseline computation silently.
-}
parseSubstitutionsArg :: KeyMap Value -> Either Text [Substitution]
parseSubstitutionsArg args = case KM.lookup (fromText "substitutions") args of
    Nothing -> Right []
    Just Null -> Right []
    Just (Array xs) -> traverse parseOne (foldr (:) [] xs)
    Just _ -> Left "'substitutions' must be an array of {from, to, consumer} objects"
  where
    parseOne (Object o) = case (KM.lookup "from" o, KM.lookup "to" o, KM.lookup "consumer" o) of
        (Just (String f), Just (String t), Just (String c)) ->
            Right Substitution{subFrom = f, subTo = t, subConsumer = c}
        _ ->
            Left "each substitution must have string fields 'from', 'to', 'consumer'"
    parseOne _ = Left "each substitution must be an object"

-- ---------------------------------------------------------------------------
-- Tool implementations
-- ---------------------------------------------------------------------------

callListDatabases :: DatabaseManager -> Value -> IO Value
callListDatabases dbManager rid = do
    loaded <- readTVarIO (dmLoadedDbs dbManager)
    let mkDbEntry ld =
            let cfg = ldConfig ld
                base =
                    [ "name" .= dcName cfg
                    , "display_name" .= dcDisplayName cfg
                    ]
                withDesc = case dcDescription cfg of
                    Nothing -> base
                    Just d -> base ++ ["description" .= d]
                withFmt = case dcFormat cfg of
                    Nothing -> withDesc
                    Just fmt -> withDesc ++ ["format" .= fmt]
             in object withFmt
        entries = map mkDbEntry (M.elems loaded)
    return $ toolSuccessJson rid $ object ["databases" .= entries]

callListPresets :: [ClassificationPreset] -> Value -> IO Value
callListPresets presets rid =
    return $
        toolSuccessJson rid $
            toJSON
                [ object
                    [ "name" .= cpName p
                    , "label" .= cpLabel p
                    , "description" .= cpDescription p
                    , "filters"
                        .= [ object ["system" .= ceSystem e, "value" .= ceValue e, "mode" .= ceMode e]
                           | e <- cpFilters p
                           ]
                    ]
                | p <- presets
                ]

callSearchActivities :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchActivities presets rid args (db, _) = do
    let name = textArg "name" args
        geo = textArg "geo" args
        product' = textArg "product" args
        limit = intArg "limit" args
        exact = fromMaybe False (boolArg "exact" args)
        isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
        presetFilters = case textArg "preset" args of
            Just pn -> case L.find (\p -> cpName p == pn) presets of
                Just p -> [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
                Nothing -> []
            Nothing -> []
        explicitFilters = case (textArg "classification" args, textArg "classification_value" args) of
            (Just sys, Just val) -> [(sys, val, isExact)]
            _ -> []
        classFilters = presetFilters ++ explicitFilters
    let sf =
            Service.SearchFilter
                { Service.sfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = name
                        , Service.afcLocation = geo
                        , Service.afcProduct = product'
                        , Service.afcClassifications = classFilters
                        , Service.afcLimit = limit <|> Just 20
                        , Service.afcOffset = Nothing
                        , Service.afcSort = Nothing
                        , Service.afcOrder = Nothing
                        }
                , Service.sfExactMatch = exact
                }
    result <- Service.searchActivities db sf
    case result of
        Left err -> return $ toolError rid (T.pack $ show err)
        Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a <|> _ = a

callListClassifications :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callListClassifications rid args (db, _) =
    let systems = Service.getClassifications db
        mSystem = textArg "system" args
        mFilter = textArg "filter" args
     in return $ toolSuccessJson rid $ case mSystem of
            Nothing ->
                toJSON
                    [ object ["name" .= csName s, "activityCount" .= csActivityCount s]
                    | s <- systems
                    ]
            Just sys ->
                case L.find (\s -> T.toLower (csName s) == T.toLower sys) systems of
                    Nothing -> object ["error" .= ("Classification system not found: " <> sys)]
                    Just s ->
                        let vals = case mFilter of
                                Nothing -> csValues s
                                Just f -> L.filter (T.isInfixOf (T.toLower f) . T.toLower) (csValues s)
                         in object ["name" .= csName s, "activityCount" .= csActivityCount s, "values" .= vals]

callSearchFlows :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchFlows rid args (db, _) =
    case textArg "query" args of
        Nothing -> return $ toolSuccessJson rid Service.emptyFlowSearchResults
        Just query -> do
            let limit = intArg "limit" args
                ff =
                    Service.FlowFilter
                        { Service.ffQuery = query
                        , Service.ffLang = Nothing
                        , Service.ffLimit = limit <|> Just 20
                        , Service.ffOffset = Nothing
                        , Service.ffSort = Nothing
                        , Service.ffOrder = Nothing
                        }
            result <- Service.searchFlows db ff
            case result of
                Left err -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a <|> _ = a

callGetActivity :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetActivity rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            case Service.getActivityInfo defaultUnitConfig db pid of
                Left err -> return $ toolError rid (T.pack $ show err)
                Right val
                    | noFilters -> return $ toolSuccessJson rid val
                    | otherwise -> case fromJSON val of
                        Error _ -> return $ toolSuccessJson rid val
                        Success ai ->
                            let filtered = ai{piActivity = (piActivity ai){pfaExchanges = filter matchExchange (pfaExchanges (piActivity ai))}}
                             in return $ toolSuccessJson rid (toJSON filtered)
  where
    exchangeType = textArg "exchange_type" args
    flowFilter = textArg "flow" args
    isInputFilter = boolArg "is_input" args
    noFilters =
        exchangeType `elem` [Nothing, Just "all"]
            && isNothing flowFilter
            && isNothing isInputFilter
    matchExchange ewu = matchType ewu && matchFlow ewu && matchIsInput ewu
    matchType ewu = case exchangeType of
        Just "biosphere" -> ewuFlowCategory ewu /= "technosphere"
        Just "technosphere" -> ewuFlowCategory ewu == "technosphere"
        _ -> True
    matchFlow ewu = case flowFilter of
        Nothing -> True
        Just q -> T.isInfixOf (T.toLower q) (T.toLower (ewuFlowName ewu))
    matchIsInput ewu = case isInputFilter of
        Nothing -> True
        Just want -> exchangeIsInput (ewuExchange ewu) == want

callGetSupplyChain :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetSupplyChain dbManager rid args =
    case (,) <$> requireText "database" args <*> requireText "process_id" args of
        Left err -> return $ toolError rid err
        Right (dbName, pid) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                        solver = ldSharedSolver ld
                        isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
                        classFilters = case (textArg "classification" args, textArg "classification_value" args) of
                            (Just sys, Just val) -> [(sys, val, isExact)]
                            _ -> []
                        scf =
                            Service.SupplyChainFilter
                                { Service.scfCore =
                                    Service.ActivityFilterCore
                                        { Service.afcName = textArg "name" args
                                        , Service.afcLocation = textArg "location" args
                                        , Service.afcProduct = Nothing
                                        , Service.afcClassifications = classFilters
                                        , Service.afcLimit = intArg "limit" args
                                        , Service.afcOffset = Nothing
                                        , Service.afcSort = Nothing
                                        , Service.afcOrder = Nothing
                                        }
                                , Service.scfMaxDepth = intArg "max_depth" args
                                , Service.scfMinQuantity = doubleArg "min_quantity" args
                                }
                    case parseSubstitutionsArg args of
                        Left err -> return $ toolError rid err
                        Right [] -> do
                            unitCfg <- DM.getMergedUnitConfig dbManager
                            result <- Service.getSupplyChain unitCfg (DM.mkDepSolverLookup dbManager) db dbName solver pid scf False
                            case result of
                                Left err -> return $ toolError rid (T.pack $ show err)
                                Right val -> return $ toolSuccessJson rid (toJSON val)
                        Right subs -> case Service.resolveActivityAndProcessId db pid of
                            Left err -> return $ toolError rid (T.pack $ show err)
                            Right (processId, _) -> do
                                eScaling <-
                                    Service.computeScalingVectorWithSubstitutionsCrossDB
                                        (DM.mkDepSolverLookup dbManager)
                                        db
                                        dbName
                                        solver
                                        processId
                                        subs
                                case eScaling of
                                    Left err -> return $ toolError rid (T.pack (show err))
                                    Right (scalingVec, virtualLinks) -> do
                                        unitCfg <- DM.getMergedUnitConfig dbManager
                                        eResp <-
                                            Service.buildSupplyChainFromScalingVectorCrossDB
                                                unitCfg
                                                (DM.mkDepSolverLookup dbManager)
                                                db
                                                dbName
                                                processId
                                                scalingVec
                                                virtualLinks
                                                scf
                                                False
                                        case eResp of
                                            Left e -> return $ toolError rid (T.pack (show e))
                                            Right v -> return $ toolSuccessJson rid (toJSON v)

{- | Generic SQL-group-by aggregation. One small primitive for "how much X is
in Y" questions — replaces ad-hoc decomposition tools.
-}
callAggregate :: DatabaseManager -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callAggregate dbManager rid args (db, solver) =
    let dbName = fromMaybe "" (textArg "database" args) -- already validated by withDb
     in case textArg "process_id" args of
            Nothing -> return $ toolError rid "Missing required parameter: process_id"
            Just pid -> case scopeFromArg of
                Left err -> return $ toolError rid err
                Right scope -> case aggFnFromArg of
                    Left err -> return $ toolError rid err
                    Right fn -> do
                        let params =
                                Agg.AggregateParams
                                    { Agg.apScope = scope
                                    , Agg.apIsInput = boolArg "is_input" args
                                    , Agg.apMaxDepth = intArg "max_depth" args
                                    , Agg.apFilterName = textArg "filter_name" args
                                    , Agg.apFilterNameNot =
                                        maybe [] (map T.strip . T.splitOn ",") (textArg "filter_name_not" args)
                                    , Agg.apFilterUnit = textArg "filter_unit" args
                                    , Agg.apFilterClassifications =
                                        mapMaybe parseClassFilter (textArrayArg "filter_classification" args)
                                    , Agg.apFilterTargetName = textArg "filter_target_name" args
                                    , Agg.apFilterExchangeType = case textArg "filter_exchange_type" args of
                                        Just "technosphere" -> Just Technosphere
                                        Just "biosphere" -> Just Biosphere
                                        _ -> Nothing
                                    , Agg.apFilterIsReference = boolArg "filter_is_reference" args
                                    , Agg.apGroupBy = textArg "group_by" args
                                    , Agg.apAggregate = fn
                                    }
                        unitCfg <- DM.getMergedUnitConfig dbManager
                        (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
                        result <- Agg.aggregate unitCfg mFlows mUnits db dbName solver (DM.mkDepSolverLookup dbManager) pid params
                        case result of
                            Left err -> return $ toolError rid (T.pack $ show err)
                            Right agg -> return $ toolSuccessJson rid (toJSON agg)
  where
    scopeFromArg = case textArg "scope" args of
        Just "direct" -> Right Agg.ScopeDirect
        Just "supply_chain" -> Right Agg.ScopeSupplyChain
        Just "biosphere" -> Right Agg.ScopeBiosphere
        Nothing -> Left "Missing required parameter: scope (direct | supply_chain | biosphere)"
        Just other -> Left ("Invalid scope: " <> other)
    aggFnFromArg = case textArg "aggregate" args of
        Nothing -> Right Agg.AggSum
        Just "sum_quantity" -> Right Agg.AggSum
        Just "count" -> Right Agg.AggCount
        Just "share" -> Right Agg.AggShare
        Just other -> Left ("Invalid aggregate fn: " <> other)
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
                Left err -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val

callGetConsumers :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetConsumers presets rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            let isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
                presetFilters = case textArg "preset" args of
                    Just pn -> case L.find (\p -> cpName p == pn) presets of
                        Just p -> [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
                        Nothing -> []
                    Nothing -> []
                explicitFilters = case (textArg "classification" args, textArg "classification_value" args) of
                    (Just sys, Just val) -> [(sys, val, isExact)]
                    _ -> []
                classFilters = presetFilters ++ explicitFilters
                cnf =
                    Service.ConsumerFilter
                        { Service.cnfCore =
                            Service.ActivityFilterCore
                                { Service.afcName = textArg "name" args
                                , Service.afcLocation = textArg "location" args
                                , Service.afcProduct = textArg "product" args
                                , Service.afcClassifications = classFilters
                                , Service.afcLimit = intArg "limit" args
                                , Service.afcOffset = Nothing
                                , Service.afcSort = Nothing
                                , Service.afcOrder = Nothing
                                }
                        , Service.cnfMaxDepth = intArg "max_depth" args
                        }
             in case Service.getConsumers db pid cnf of
                    Left err -> return $ toolError rid (T.pack $ show err)
                    Right results -> return $ toolSuccessJson rid (toJSON results)

{- | MCP get_inventory: route through the cross-DB back-substitution path
so inventories from dep DBs are merged into the returned flows.
-}
callGetInventory :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetInventory dbManager rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                (dbName, pid) <- ExceptT $ pure $ (,) <$> requireText "database" args <*> requireText "process_id" args
                mLoaded <- liftIO $ getDatabase dbManager dbName
                ld <- case mLoaded of
                    Nothing -> throwE ("Database not loaded: " <> dbName)
                    Just x -> pure x
                let db = ldDatabase ld
                    solver = ldSharedSolver ld
                    limit = fromMaybe 50 (intArg "limit" args)
                    nameFilter = textArg "flow" args
                ExceptT $ pure $ ensureLinked dbName "computing inventory" db
                (processId, activity) <- case Service.resolveActivityAndProcessId db pid of
                    Left err -> throwE (T.pack (show err))
                    Right v -> pure v
                subs <- ExceptT $ pure $ parseSubstitutionsArg args
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                -- Empty subs: same as GET path (plain cross-DB inventory).
                -- Non-empty subs: route through the substitution-aware pipeline so
                -- dep DBs re-solve against the substituted root scaling.
                inventory <-
                    ExceptT $
                        if null subs
                            then computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup dbManager) db solver processId
                            else
                                either (Left . T.pack . show) Right
                                    <$> Service.inventoryWithSubsAndDeps
                                        unitCfg
                                        (DM.mkDepSolverLookup dbManager)
                                        db
                                        dbName
                                        solver
                                        processId
                                        subs
                let inv = Service.convertToInventoryExport db mFlows mUnits processId activity inventory
                    flows = ieFlows inv
                    filtered = case nameFilter of
                        Nothing -> flows
                        Just q -> filter (T.isInfixOf (T.toLower q) . T.toLower . flowName . ifdFlow) flows
                    sorted = L.sortBy (\a b -> compare (abs $ ifdQuantity b) (abs $ ifdQuantity a)) filtered
                    topN = take limit sorted
                    slim f =
                        object
                            [ "flow" .= flowName (ifdFlow f)
                            , "quantity" .= ifdQuantity f
                            , "unit" .= ifdUnitName f
                            , "category" .= ifdCategory f
                            , "isEmission" .= ifdIsEmission f
                            ]
                pure $
                    toolSuccessJson rid $
                        object
                            [ "statistics" .= toJSON (ieStatistics inv)
                            , "total_flows" .= length flows
                            , "shown_flows" .= length topN
                            , "flows" .= map slim topN
                            ]
            )

{- | Handler for the 'get_impacts' MCP tool (computes LCIA score).
Historically named 'get_lcia' — the MCP surface now uses 'impacts'
per the naming audit; internal Haskell types keep the 'LCIA' acronym
(LCIAResult, computeLCIAScore) since they're the domain term of art.
-}
callGetImpacts :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetImpacts dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                let ld = lrLoaded req
                    db = ldDatabase ld
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                ExceptT $ pure $ ensureLinked dbName "computing impacts" db
                subs <- ExceptT $ pure $ parseSubstitutionsArg args
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                inventory <-
                    ExceptT $
                        if null subs
                            then computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup dbManager) db (ldSharedSolver ld) (raPid ra)
                            else
                                either (Left . T.pack . show) Right
                                    <$> Service.inventoryWithSubsAndDeps
                                        unitCfg
                                        (DM.mkDepSolverLookup dbManager)
                                        db
                                        dbName
                                        (ldSharedSolver ld)
                                        (raPid ra)
                                        subs
                mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName db method
                tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
                let topN = fromMaybe 5 (intArg "top_flows" args)
                    stats = computeMappingStats mappings
                    score = computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables
                    (prodName, prodAmount, prodUnit) = Service.getReferenceProductInfo mFlows mUnits (raActivity ra)
                    functionalUnit = T.pack (showFFloat (Just 2) prodAmount "") <> " " <> prodUnit <> " of " <> prodName
                    (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
                    contribs = L.sortOn (\(_, _, c) -> negate (abs c)) rawContribs
                    topFlows = take topN contribs
                    webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> raText ra <> "/impacts/" <> encodeSegment (lrCollection req) <> "/" <> lrMethodIdText req
                    hasNeg = any (\(_, _, c) -> c < 0) contribs
                liftIO $
                    unless (null unknownUuids) $
                        reportProgress Warning $
                            "[MCP get_impacts "
                                <> T.unpack (methodName method)
                                <> "] "
                                <> show (length unknownUuids)
                                <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                                <> show (take 3 unknownUuids)
                pure $
                    toolSuccessJson rid $
                        object
                            [ "method" .= methodName method
                            , "category" .= methodCategory method
                            , "score" .= score
                            , "unit" .= methodUnit method
                            , "functional_unit" .= functionalUnit
                            , "mapped_flows" .= (msTotal stats - msUnmatched stats)
                            , "has_negative_contributions" .= hasNeg
                            , "web_url" .= webUrl
                            , "top_flows"
                                .= [ object
                                        [ "flow_name" .= flowName f
                                        , "contribution" .= c
                                        , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                        , "flow_id" .= UUID.toText (flowId f)
                                        , "category" .= flowCategory f
                                        , "compartment" .= flowSubcompartment f
                                        , "cf_value" .= cfVal
                                        , "flow_unit" .= getUnitNameForFlow mUnits f
                                        ]
                                   | (f, cfVal, c) <- topFlows
                                   ]
                            ]
            )

callListMethods :: DatabaseManager -> Value -> IO Value
callListMethods dbManager rid = do
    loadedMethods <- DM.getLoadedMethods dbManager
    let summaries =
            map
                ( \(_, m) ->
                    object
                        [ "id" .= UUID.toText (methodId m)
                        , "name" .= methodName m
                        , "category" .= methodCategory m
                        , "unit" .= methodUnit m
                        ]
                )
                loadedMethods
    return $ toolSuccessJson rid $ object ["methods" .= summaries]

callGetFlowMapping :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetFlowMapping dbManager rid args =
    case (,) <$> requireText "database" args <*> requireText "method_id" args of
        Left err -> return $ toolError rid err
        Right (dbName, methodIdText) -> do
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
                                (method : _) -> do
                                    mappings <- DM.mapMethodToFlowsCached dbManager dbName db method
                                    let stats = computeMappingStats mappings
                                        total = msTotal stats
                                        matched = total - msUnmatched stats
                                        coverage =
                                            if total > 0
                                                then fromIntegral matched / fromIntegral total * 100 :: Double
                                                else 0
                                    return $
                                        toolSuccessJson rid $
                                            object
                                                [ "method" .= methodName method
                                                , "total" .= total
                                                , "matched" .= matched
                                                , "unmatched" .= msUnmatched stats
                                                , "coverage" .= coverage
                                                ]

callGetCharacterization :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetCharacterization dbManager rid args =
    case (,) <$> requireText "database" args <*> requireText "method_id" args of
        Left err -> return $ toolError rid err
        Right (dbName, methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    eMethod <- resolveMethod dbManager methodIdText
                    case eMethod of
                        Left err -> return $ toolError rid err
                        Right (_, method) -> do
                            let db = ldDatabase ld
                                lim = fromMaybe 20 (intArg "limit" args)
                                flowQ = textArg "flow" args
                                queryLower = fmap T.toLower flowQ
                            mappings <- DM.mapMethodToFlowsCached dbManager dbName db method
                            let matched =
                                    [ (cf, f, strat)
                                    | (cf, Just (f, strat)) <- mappings
                                    , matchQuery queryLower (mcfFlowName cf) (flowName f)
                                    ]
                                sorted = L.sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
                                top = take lim sorted
                                mkEntry (cf, f, strat) =
                                    object
                                        [ "cf_flow_name" .= mcfFlowName cf
                                        , "cf_value" .= mcfValue cf
                                        , "cf_unit" .= mcfUnit cf
                                        , "direction" .= (case mcfDirection cf of Input -> "Input" :: Text; Output -> "Output")
                                        , "db_flow_name" .= flowName f
                                        , "flow_id" .= UUID.toText (flowId f)
                                        , "flow_unit" .= getUnitNameForFlow (dbUnits db) f
                                        , "category" .= flowCategory f
                                        , "compartment" .= flowSubcompartment f
                                        , "match_strategy" .= show strat
                                        ]
                            return $
                                toolSuccessJson rid $
                                    object
                                        [ "method" .= methodName method
                                        , "unit" .= methodUnit method
                                        , "matches" .= length matched
                                        , "shown" .= length top
                                        , "factors" .= map mkEntry top
                                        ]
  where
    matchQuery Nothing _ _ = True
    matchQuery (Just q) cfName dbFlowName = T.isInfixOf q (T.toLower cfName) || T.isInfixOf q (T.toLower dbFlowName)

{- | Build the MCP JSON object for a cross-DB activity contribution. Dep-DB
process IDs are qualified as @"dbName::actUUID_prodUUID"@ — same convention
as the activity-detail endpoint, so the @web_url@ round-trips.
-}
mkMcpCrossDBEntry ::
    DatabaseManager ->
    -- | root DB name
    Text ->
    -- | base URL
    Text ->
    -- | method collection name
    Text ->
    -- | method UUID text
    Text ->
    FlowDB ->
    UnitDB ->
    -- | total score (for share %)
    Double ->
    ((Text, ProcessId), Double) ->
    IO Value
mkMcpCrossDBEntry dbManager rootDbName baseUrl colName methodIdText flowDB unitDB score ((depDbName, pid), c) = do
    mLd <- getDatabase dbManager depDbName
    let (actName, actLoc, prodName, pidText) = case mLd of
            Just ld ->
                let d = ldDatabase ld
                    mAct = Service.findActivityByProcessId d pid
                    txt =
                        if depDbName == rootDbName
                            then processIdToText d pid
                            else depDbName <> "::" <> processIdToText d pid
                    (pn, _, _) = maybe ("", 0, "") (Service.getReferenceProductInfo flowDB unitDB) mAct
                 in (maybe "" activityName mAct, maybe "" activityLocation mAct, pn, txt)
            Nothing ->
                ("", "", "", depDbName <> "::<unloaded>")
        procWebUrl =
            baseUrl
                <> "/db/"
                <> rootDbName
                <> "/activity/"
                <> pidText
                <> "/contributing-activities/"
                <> encodeSegment colName
                <> "/"
                <> methodIdText
    pure $
        object
            [ "process_id" .= pidText
            , "activity_name" .= actName
            , "product_name" .= prodName
            , "location" .= actLoc
            , "contribution" .= c
            , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
            , "web_url" .= procWebUrl
            ]

-- | Helper: resolve method from UUID text, also returning its collection name
resolveMethod :: DatabaseManager -> Text -> IO (Either Text (Text, Method))
resolveMethod dbManager methodIdText =
    case UUID.fromText methodIdText of
        Nothing -> return $ Left "Invalid method UUID format"
        Just uuid -> do
            loadedMethods <- DM.getLoadedMethods dbManager
            case filter (\(_, m) -> methodId m == uuid) loadedMethods of
                [] -> return $ Left "Method not found"
                ((col, m) : _) -> return $ Right (col, m)

{- | Raw text + its parsed 'ProcessId' + the looked-up 'Activity'. Bundled so
the three entities (which must always agree) cannot drift apart: the only
way to build a 'ResolvedActivity' is through 'resolveActivityAndProcessId'.
-}
data ResolvedActivity = ResolvedActivity
    { raText :: !Text
    , raPid :: !ProcessId
    , raActivity :: !Activity
    }

{- | Bundle of entities resolved at the start of every LCA handler (impacts,
contributing flows, contributing activities, inventory). Populated once by
'loadLcaRequest' so the handler body stays flat instead of unwrapping four
layers of 'case'.
-}
data LcaRequest = LcaRequest
    { lrDbName :: !Text
    , lrLoaded :: !LoadedDatabase
    , lrResolved :: !ResolvedActivity
    , lrMethodIdText :: !Text
    , lrCollection :: !Text
    , lrMethod :: !Method
    }

{- | Resolve every entity an LCA handler needs from raw JSON-RPC args.
Short-circuits on the first failure (missing arg, unknown DB, bad UUID,
unknown method, unresolvable process id).
-}
loadLcaRequest :: DatabaseManager -> KeyMap Value -> ExceptT Text IO LcaRequest
loadLcaRequest dbManager args = do
    (dbName, pidText, methodIdText) <-
        ExceptT $
            pure $
                (,,)
                    <$> requireText "database" args
                    <*> requireText "process_id" args
                    <*> requireText "method_id" args
    mLoaded <- liftIO $ getDatabase dbManager dbName
    ld <- case mLoaded of
        Nothing -> throwE ("Database not loaded: " <> dbName)
        Just x -> pure x
    (col, method) <- ExceptT (resolveMethod dbManager methodIdText)
    (pid, act) <- case Service.resolveActivityAndProcessId (ldDatabase ld) pidText of
        Left err -> throwE (T.pack (show err))
        Right v -> pure v
    pure
        LcaRequest
            { lrDbName = dbName
            , lrLoaded = ld
            , lrResolved = ResolvedActivity pidText pid act
            , lrMethodIdText = methodIdText
            , lrCollection = col
            , lrMethod = method
            }

{- | Bail if the database has unresolved cross-DB links. 'op' names the
user-visible operation for the error message (e.g. "computing impacts").
-}
ensureLinked :: Text -> Text -> Database -> Either Text ()
ensureLinked dbName op db =
    let n = unresolvedCount (dbLinkingStats db)
     in if n == 0
            then Right ()
            else
                Left $
                    "Database \""
                        <> dbName
                        <> "\" has "
                        <> T.pack (show n)
                        <> " unresolved cross-DB products. Load the missing dependency databases and re-link before "
                        <> op
                        <> "."

callGetContributingFlows :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetContributingFlows dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                let ld = lrLoaded req
                    db = ldDatabase ld
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                    lim = fromMaybe 20 (intArg "limit" args)
                    webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> raText ra <> "/contributing-flows/" <> encodeSegment (lrCollection req) <> "/" <> lrMethodIdText req
                ExceptT $ pure $ ensureLinked dbName "computing contributions" db
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                inventory <-
                    ExceptT $
                        computeInventoryMatrixWithDepsCached
                            unitCfg
                            (DM.mkDepSolverLookup dbManager)
                            db
                            (ldSharedSolver ld)
                            (raPid ra)
                tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
                let score = computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables
                    (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
                    contribs = L.sortOn (\(_, _, c) -> negate (abs c)) rawContribs
                    top = take lim contribs
                    hasNeg = any (\(_, _, c) -> c < 0) contribs
                liftIO $
                    unless (null unknownUuids) $
                        reportProgress Warning $
                            "[MCP get_contributing_flows "
                                <> T.unpack (methodName method)
                                <> "] "
                                <> show (length unknownUuids)
                                <> " inventory flow UUID(s) absent from merged FlowDB. Samples: "
                                <> show (take 3 unknownUuids)
                pure $
                    toolSuccessJson rid $
                        object
                            [ "method" .= methodName method
                            , "unit" .= methodUnit method
                            , "total_score" .= score
                            , "has_negative_contributions" .= hasNeg
                            , "web_url" .= webUrl
                            , "top_flows"
                                .= [ object
                                        [ "flow_name" .= flowName f
                                        , "contribution" .= c
                                        , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                        , "flow_id" .= UUID.toText (flowId f)
                                        , "category" .= flowCategory f
                                        , "compartment" .= flowSubcompartment f
                                        , "cf_value" .= cfVal
                                        ]
                                   | (f, cfVal, c) <- top
                                   ]
                            ]
            )

callGetContributingActivities :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetContributingActivities dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                let ld = lrLoaded req
                    db = ldDatabase ld
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                    lim = fromMaybe 10 (intArg "limit" args)
                ExceptT $ pure $ ensureLinked dbName "computing contributions" db
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
                -- Skip separate inventory compute: contributions sum equals the score.
                contributions <-
                    ExceptT $
                        crossDBProcessContributions
                            unitCfg
                            mUnits
                            mFlows
                            (DM.mkDepSolverLookup dbManager)
                            db
                            dbName
                            (ldSharedSolver ld)
                            (raPid ra)
                            tables
                let score = sum (M.elems contributions)
                    sorted = L.sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
                    top = take lim sorted
                    hasNeg = any (\(_, c) -> c < 0) top
                rows <- liftIO $ mapM (mkMcpCrossDBEntry dbManager dbName baseUrl (lrCollection req) (lrMethodIdText req) mFlows mUnits score) top
                pure $
                    toolSuccessJson rid $
                        object
                            [ "method" .= methodName method
                            , "unit" .= methodUnit method
                            , "total_score" .= score
                            , "has_negative_contributions" .= hasNeg
                            , "processes" .= rows
                            ]
            )

callListGeographies :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callListGeographies dbManager rid args =
    case textArg "database" args of
        Nothing -> return $ toolError rid "Missing required parameter: database"
        Just dbName -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                        geoMap = dmGeographies dbManager
                        codes = L.sort $ M.keys (idxByLocation (dbIndexes db))
                        mkEntry code =
                            let (displayName, parents) = M.findWithDefault (code, []) code geoMap
                                parentStr = T.intercalate "|" parents
                             in object
                                    [ "geo" .= code
                                    , "display_name" .= displayName
                                    , "parent_regions" .= parentStr
                                    ]
                    return $
                        toolSuccessJson rid $
                            object
                                ["geographies" .= map mkEntry codes]

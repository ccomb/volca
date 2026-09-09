{-# LANGUAGE OverloadedStrings #-}

{- | MCP (Model Context Protocol) server endpoint.
Implements Streamable HTTP transport (MCP spec 2025-03-26).
POST /mcp handles initialize, tools/list, tools/call (JSON or SSE response).
GET  /mcp opens an SSE stream for server-initiated messages (stateless: closes immediately).
-}
module API.MCP (mcpApp, mcpCountsAsActivity, WhileWorking, toolDefinitions, callTool, selectMethod, handleInitialize, webUrlBase, RpcRequest (..)) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Network.HTTP.Types (hContentType, status200, status202, status405)
import Network.Wai (Application, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import System.Random (randomIO)
import qualified Version

import API.Resources (Param (..), ParamKind (..), Resource)
import qualified API.Resources as R
import Config (ClassificationEntry (..), ClassificationPreset (..), DatabaseConfig (..), HostingConfig, ReadOnly (..), ServerName, expandClassificationPreset, hostingReadOnly, readOnlyRefusalFor, unServerName)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Data.Bifunctor (first)
import Database (filterByName, flowSearchFields)
import Database.Edit (deriveDatabase, editExchanges, refusalMessage)
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), getDatabase)
import qualified Database.Manager as DM

import qualified API.BatchImpacts as BI
import API.DatabaseHandlers (copyRefusal, coverageReportToAPI, editReportToAPI, explainCFToAPI, gapReportToAPI, loadQuotaRefusal, qualityReportToAPI, quotaCounts)
import API.MCP.Columnar (resolveSingleScoringSet, toColumnarBatch)
import API.MCP.Enrich (addWebUrlMaybe, attachMarketHintByName, encodeSegment, filterScoringSets, scoreActivityWebUrl, slimLCIAPanel, webUrlField)
import API.Routes (collectionNotLoadedMessage)
import API.Types (ActivityForAPI (..), ActivityInfo (..), ClassificationSystem (..), ExchangeEditRequest (..), ExchangeWithUnit (..), InventoryExport (..), InventoryFlowDetail (..), Perturbation (..), Substitution (..), SubstitutionRequest (..), toExchangeEdits)
import Control.Monad (mfilter, unless)
import qualified Data.List as L
import Matrix (applyBiosphereMatrix)
import qualified Method.Explain as Explain
import Method.Mapping (LCIAOutcome (..), MappingStats (..), SimilarCF (..), SimilarReason (..), UncharacterizedFlow (..), applyLongTermMode, computeLCIAScoreAuto, computeLCIAScoreFromTables, computeMappingStats, defaultUncharacterizedOpts, inventoryContributions, longTermModeFromExclude)
import qualified Method.Mapping as Mapping
import Method.Types (FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..))
import Network.HTTP.Types.Header (RequestHeaders, hAccept, hAllow, hHost)
import Progress (ProgressLevel (Warning), reportProgress)
import qualified Search.Normalize as Normalize
import qualified Service
import qualified Service.Aggregate as Agg
import SharedSolver (SharedSolver, computeInventoryMatrixWithDepsCached, crossDBProcessContributions)
import qualified SharedSolver
import Types (Activity (..), BiosphereFlow (..), Database (..), FlowKind (BioKind), Indexes (..), KindFilter (..), ProcessId, UUID, UnitDB, activityLocation, activityName, allocationKeyText, bfCompartmentName, bfCompartmentSub, exchangeIsInput, exchangeKindChoices, exchangeKindOf, getUnitNameForBioFlow, lookupExchangeFlow, parseAllocationKey, parseExchangeKind, parseKindNames, processIdToText, qualifyRef, unresolvedCount)

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

{- | Which MCP calls mean a human is there. @initialize@, @tools\/list@, @ping@
and the notifications announce or maintain a client without asking anything of
the engine: a connected assistant emits them on its own, all day, whether or
not anyone is working. Only @tools\/call@ is someone asking a question.
-}
mcpCountsAsActivity :: Text -> Bool
mcpCountsAsActivity = (== "tools/call")

{- | The absolute base for @web_url@ deep links, or 'Nothing' when no
frontend serves those routes and the links would point at a 404.

Two ways a frontend can exist: bundled with this process, answering on the
request's own host; or upstream, behind a reverse proxy that says so by
setting @X-Forwarded-Prefix@ - the prefix it serves this engine under, which
then belongs in every link, along with the forwarded protocol.

The header's presence is the declaration, not its value: a proxy serving the
routes at the root says @X-Forwarded-Prefix: /@. Trailing slashes are dropped
because every link path starts with its own.
-}
webUrlBase :: Bool -> RequestHeaders -> Maybe Text
webUrlBase hasFrontend hdrs
    | hasFrontend || isJust mPrefix = Just (proto <> "://" <> host <> prefix)
    | otherwise = Nothing
  where
    header n = maybe "" TE.decodeUtf8Lenient (lookup n hdrs)
    mPrefix = TE.decodeUtf8Lenient <$> lookup "X-Forwarded-Prefix" hdrs
    prefix = maybe "" (T.dropWhileEnd (== '/')) mPrefix
    proto = case header "X-Forwarded-Proto" of
        "" -> "http"
        p -> p
    host = case header hHost of
        "" -> "localhost"
        h -> h

{- | Runs one MCP call with the server counted as in use for its whole duration.

A tool call is not an instant: it can be a load that reads a gigabyte of source
and builds its matrices. A server that shuts itself down when idle must not
decide it is idle in the middle of one, so the call is wrapped rather than
merely noted as it arrives. A server with no such policy passes 'id'.
-}
type WhileWorking = IO (Maybe Value) -> IO (Maybe Value)

{- | Build the @\/mcp@ endpoint.

@whileWorking@ wraps every request that 'mcpCountsAsActivity' accepts, which is
how a server that shuts itself down when idle tells a working client from a
merely connected one.
-}
mcpApp :: DatabaseManager -> [ClassificationPreset] -> Bool -> Maybe HostingConfig -> Maybe ServerName -> WhileWorking -> IO Application
mcpApp dbManager presets hasFrontend mHosting mName whileWorking = do
    (a, b) <- (,) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)
    let sessionId = T.pack $ show (abs a) ++ "-" ++ show (abs b)
    stateRef <- newIORef McpState{mcpSessionId = sessionId}
    return $ \req respond -> do
        let method = requestMethod req
            hdrs = requestHeaders req
            mBaseUrl = webUrlBase hasFrontend hdrs
            acceptHdr = fromMaybe "" $ lookup hAccept hdrs
            wantsSse = "text/event-stream" `BS.isInfixOf` acceptHdr
        st <- readIORef stateRef
        case method of
            "POST" -> do
                body <- strictRequestBody req
                case eitherDecode body of
                    Left err ->
                        respond $ jsonResponse (mcpSessionId st) $ rpcError Null (-32700) (T.pack $ "Parse error: " ++ err)
                    Right rpcReq -> do
                        let runCall = if mcpCountsAsActivity (rpcMethod rpcReq) then whileWorking else id
                        resp <- runCall (handleRpc dbManager presets mHosting mBaseUrl mName st rpcReq)
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
            -- Everything else, GET included. A GET opens the stream a server
            -- uses to speak first; VoLCA never does, and answering it with a
            -- stream that closes at once reads to a client as a dropped
            -- connection, which it reconnects, forever. 405 says there is no
            -- stream to open, and the client stops asking.
            _ ->
                respond
                    $ responseLBS
                        status405
                        [(hContentType, "application/json"), (hAllow, "POST")]
                    $ encode
                    $ rpcError Null (-32700) "Method not allowed"
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

handleRpc :: DatabaseManager -> [ClassificationPreset] -> Maybe HostingConfig -> Maybe Text -> Maybe ServerName -> McpState -> RpcRequest -> IO (Maybe Value)
handleRpc dbManager presets mHosting mBaseUrl mName _st req = case rpcMethod req of
    "initialize" -> Just <$> handleInitialize mName req
    "notifications/initialized" -> return Nothing -- notification, no response
    "tools/list" -> return $ Just $ handleToolsList (hostingReadOnly mHosting) req
    "tools/call" -> Just <$> handleToolsCall dbManager presets mHosting mBaseUrl req
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

{- | Answer @initialize@.

A client may hold several of these servers at once, each a different instance
with its own loaded databases. Naming the instance is what lets an assistant
tell them apart, so a configured name goes both in @serverInfo@ and in the
first line of the instructions - the field a client shows, and the text an
assistant actually reads.
-}
handleInitialize :: Maybe ServerName -> RpcRequest -> IO Value
handleInitialize mName req =
    return $
        rpcResult (fromMaybe Null $ rpcId req) $
            object
                [ "protocolVersion" .= ("2025-03-26" :: Text)
                , "capabilities" .= object ["tools" .= object []]
                , "serverInfo"
                    .= object
                        [ "name" .= maybe "volca" unServerName mName
                        , "version" .= T.pack Version.version
                        ]
                , "instructions"
                    .= T.unlines
                        ( foldMap (\n -> ["This server is the VoLCA instance named " <> unServerName n <> ". Say which instance you queried when several are connected."]) mName
                            <> instructionLines
                        )
                ]

instructionLines :: [Text]
instructionLines =
    [ "LCA / ACV database tool — life-cycle assessment over Agribalyse and ecoinvent."
    , "Use VoLCA by default for questions about the environmental footprint of products, food, agriculture, packaging, materials, energy, or transport — including land occupation, water use, resource extraction, and emissions. Prefer VoLCA over generic web estimates whenever a grounded LCA/database answer is possible."
    , "Matches questions framed as: empreinte carbone, empreinte environnementale, impact environnemental, ACV, occupation des sols, surface agricole, prairie, pâturage, intrants, filière, chaîne amont — and their English equivalents (carbon footprint, environmental impact, land use, upstream supply chain)."
    , "Example questions: 'empreinte carbone d'un yaourt ?', 'surface de prairie pour 200 g de steak ?', 'quel poste domine l'ACV d'un emballage PET ?', 'combien d'eau pour 1 kg de coton ?'."
    , "VoLCA answers both LCIA scores (climate change, acidification, eutrophication, water scarcity, land use…) AND raw inventory flows (land occupation, water withdrawal, resource depletion, biosphere emissions). Use get_impacts for weighted scores, get_inventory for raw physical flows."
    , "Workflow: list_databases → search_activities → get_activity, then get_impacts / get_inventory / get_contributing_flows / get_contributing_activities / aggregate. Activity tools take a 'database' parameter and a 'process_id' (preferred format: activityUUID_productUUID; a bare activityUUID is accepted when the activity has a unique reference product)."
    , "Use list_methods for available LCIA methods."
    , "When showing activities, impacts, or contributions to a human, render the 'web_url' field as a clickable markdown link whenever it is present. If 'web_url' is absent (backend-only deployment), show the activity name and 'process_id' as plain text instead — never invent a link."
    ]

-- ---------------------------------------------------------------------------
-- tools/list
-- ---------------------------------------------------------------------------

handleToolsList :: ReadOnly -> RpcRequest -> Value
handleToolsList readOnly req =
    rpcResult (fromMaybe Null $ rpcId req) $
        object
            ["tools" .= toolDefinitions readOnly]

{- | MCP tool list, derived from 'API.Resources'. A read-only instance hides
the mutating tools instead of advertising calls that can only fail; the
dispatch guard in 'callTool' still refuses them should a client call one
anyway.

See note [Tool definitions come from Resources.hs].
-}
toolDefinitions :: ReadOnly -> [Value]
toolDefinitions readOnly =
    map toolFromResource $
        filter (\r -> not (isReadOnly readOnly && R.resourceMutates r)) R.allResources

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
    -- exceptions are the parameters whose entries are objects, listed by name
    -- here rather than by extending the 'Param' record, which would mean
    -- touching every call site.
    arrayItemsFor p
        | paramType p /= "array" = []
        | otherwise = ["items" .= objectItems (paramName p)]

    objectItems name = case name of
        "substitutions" -> substitutionItemSchema
        "remove" -> selectorItemSchema
        "set_amounts" -> setAmountItemSchema
        "add_inputs" -> providerLineSchema "Producer of what is consumed"
        "add_waste_outputs" -> providerLineSchema "Treatment the waste is handed to"
        "add_biosphere" -> bioLineSchema
        _ -> object ["type" .= ("string" :: Text)]

    substitutionItemSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "from" .= stringField "Source supplier ProcessId (bare or dbName::pid)"
                    , "to" .= stringField "Replacement supplier ProcessId (bare or dbName::pid)"
                    , "consumer" .= stringField "Consumer activity ProcessId. Omit to substitute on every consumer of 'from' at once (global swap; 'from' must be in the root DB)."
                    ]
            , "required" .= (["from", "to"] :: [Text])
            ]

    selectorItemSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "kind" .= stringField "input | waste | biosphere"
                    , "provider" .= stringField "ProcessId of the provider, for kind input or waste"
                    , "flow" .= stringField "Flow id, for kind biosphere (from get_activity)"
                    ]
            , "required" .= (["kind"] :: [Text])
            ]

    setAmountItemSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "select" .= selectorItemSchema
                    , "amount" .= numberField "New amount for every line the selector names"
                    ]
            , "required" .= (["select", "amount"] :: [Text])
            ]

    providerLineSchema providerDesc =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "provider" .= stringField providerDesc
                    , "amount" .= numberField "Amount exchanged"
                    , "unit" .= stringField "Unit the amount is stated in. Defaults to the provider's own reference unit."
                    , "comment" .= stringField "Free-text note on this line"
                    ]
            , "required" .= (["provider", "amount"] :: [Text])
            ]

    bioLineSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "direction" .= stringField "resource (taken from the environment) | emission (released into it)"
                    , "amount" .= numberField "Amount exchanged, in the flow's own unit"
                    , "flow" .= stringField "Id of a flow the database already has (from get_activity or search_flows)"
                    , "name" .= stringField "Name of the flow instead, with its compartment and unit: the flow the database declares under them, or a new one when nothing does"
                    , "compartment" .= stringField "air | water | soil | natural resource"
                    , "subCompartment" .= stringField "Sub-compartment, e.g. \"low population density\". Part of what tells two flows of one name apart."
                    , "unit" .= stringField "Unit. Part of a named flow's identity, so it cannot be omitted there."
                    , "comment" .= stringField "Free-text note on this line"
                    ]
            , "required" .= (["direction", "amount"] :: [Text])
            ]

    stringField desc = object ["type" .= ("string" :: Text), "description" .= (desc :: Text)]
    numberField desc = object ["type" .= ("number" :: Text), "description" .= (desc :: Text)]

-- ---------------------------------------------------------------------------
-- tools/call dispatch
-- ---------------------------------------------------------------------------

handleToolsCall :: DatabaseManager -> [ClassificationPreset] -> Maybe HostingConfig -> Maybe Text -> RpcRequest -> IO Value
handleToolsCall dbManager presets mHosting mBaseUrl req = do
    let rid = fromMaybe Null (rpcId req)
    case rpcParams req >>= parseCallParams of
        Nothing -> return $ rpcError rid (-32602) "Invalid params: expected {name, arguments}"
        Just (toolName, args) -> callTool dbManager presets mHosting mBaseUrl rid toolName args

parseCallParams :: Value -> Maybe (Text, KeyMap Value)
parseCallParams (Object o) = do
    String name <- KM.lookup "name" o
    let args = case KM.lookup "arguments" o of
            Just (Object a) -> a
            _ -> KM.empty
    return (name, args)
parseCallParams _ = Nothing

{- | Route a tool call to its handler.

A read-only instance refuses the state-changing tools before dispatch. Which
tools those are comes from the resource registry ('resourceMutates'), so a
newly added mutating tool is covered here without touching this function.
-}
callTool :: DatabaseManager -> [ClassificationPreset] -> Maybe HostingConfig -> Maybe Text -> Value -> Text -> KeyMap Value -> IO Value
callTool _ _ mHosting _ rid name _
    | isReadOnly (hostingReadOnly mHosting) && mutatingTool name =
        return $ toolError rid (readOnlyRefusalFor mHosting)
-- A preset the instance does not carry is refused here rather than in each
-- handler, so no tool can answer as if the caller had asked for no filter.
callTool _ presets _ _ rid _ args
    | Left err <- presetFilters presets args = return $ toolError rid err
callTool dbManager presets mHosting mBaseUrl rid name args = case name of
    "list_databases" -> callListDatabases dbManager rid
    "load_database" -> callLoadDatabase dbManager mHosting rid args
    "unload_database" -> callUnloadDatabase dbManager rid args
    "derive_database" -> callDeriveDatabase dbManager mHosting rid args
    "list_presets" -> callListPresets presets rid
    "search_activities" -> withDb dbManager rid args $ callSearchActivities presets rid args
    "search_flows" -> withDb dbManager rid args $ callSearchFlows rid args
    "count_search_matches" -> withDb dbManager rid args $ callCountSearchMatches rid args
    "get_activity" -> withDb dbManager rid args $ callGetActivity rid args
    "get_supply_chain" -> callGetSupplyChain dbManager presets rid args
    "aggregate" -> withDb dbManager rid args $ callAggregate dbManager presets rid args
    "get_inventory" -> callGetInventory dbManager rid args
    "get_impacts" -> callGetImpacts dbManager mBaseUrl rid args
    "compute_sensitivity" -> callComputeSensitivity dbManager mBaseUrl rid args
    "list_methods" -> callListMethods dbManager rid
    "get_flow_mapping" -> callGetFlowMapping dbManager rid args
    "get_characterization" -> callGetCharacterization dbManager rid args
    "explain_cf" -> callExplainCF dbManager mBaseUrl rid args
    "get_contributing_flows" -> callGetContributingFlows dbManager mBaseUrl rid args
    "get_contributing_activities" -> callGetContributingActivities dbManager mBaseUrl rid args
    "list_geographies" -> callListGeographies dbManager rid args
    "list_classifications" -> withDb dbManager rid args $ callListClassifications rid args
    "get_path_to" -> withDb dbManager rid args $ callGetPathTo rid args
    "get_consumers" -> withDb dbManager rid args $ callGetConsumers presets rid args
    "compare_impacts" -> callCompareImpacts dbManager rid args
    "score_activity" -> callScoreActivity dbManager mBaseUrl rid args
    "score_activities" -> callScoreActivities dbManager mBaseUrl rid args
    "list_scoring_sets" -> callListScoringSets dbManager rid args
    "get_gap_report" -> callGetGapReport dbManager rid args
    "get_quality_report" -> callGetQualityReport dbManager rid args
    "get_computed_quality_report" -> callGetComputedQualityReport dbManager rid args
    "get_characterization_coverage" -> callGetCoverageReport dbManager rid args
    "edit_exchanges" -> callEditExchanges dbManager rid args
    _ -> return $ toolError rid ("Unknown tool: " <> name)

-- Helper: extract database, then run action

{- | Whether a tool name denotes an operation that changes state shared by
every caller. Read from the resource registry rather than a list kept here,
so the two cannot drift apart.
-}
mutatingTool :: Text -> Bool
mutatingTool name = any (\r -> R.mcpName r == name && R.resourceMutates r) R.allResources

withDb ::
    DatabaseManager ->
    Value ->
    KeyMap Value ->
    ((Database, SharedSolver) -> IO Value) ->
    IO Value
withDb dbManager rid args action = runTool rid $ do
    dbName <- except (requireText "database" args)
    ld <- requireDatabase dbManager dbName
    liftIO $ action (ldDatabase ld, ldSharedSolver ld)

-- ---------------------------------------------------------------------------
-- ExceptT plumbing shared by every handler
-- ---------------------------------------------------------------------------

{- | Run a handler body in the shared 'ExceptT Text IO Value' monad: any
'throwE'/'Left' short-circuits to a 'toolError', a success passes through.
Every tool handler is @runTool rid $ do …@.
-}
runTool :: Value -> ExceptT Text IO Value -> IO Value
runTool rid = fmap (either (toolError rid) id) . runExceptT

{- | Resolve a loaded database by name, short-circuiting with the standard
"not loaded" message. The 'ExceptT' counterpart to 'withDb'.
-}
requireDatabase :: DatabaseManager -> Text -> ExceptT Text IO LoadedDatabase
requireDatabase dbManager dbName =
    ExceptT $ maybe (Left ("Database not loaded: " <> dbName)) Right <$> getDatabase dbManager dbName

-- | Lift an 'Either' whose error only has a 'Show' instance into the handler monad.
liftShow :: (Show e) => Either e a -> ExceptT Text IO a
liftShow = either (throwE . T.pack . show) pure

{- | Lift a service result. A refusal to score is a sentence written for the
caller and travels as is; every other error renders as before.
-}
liftService :: Either Service.ServiceError a -> ExceptT Text IO a
liftService = either (throwE . render) pure
  where
    render :: Service.ServiceError -> Text
    render (Service.NotScorable msg) = msg
    render e = T.pack (show e)

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

{- | Optional text argument. Distinguishes three cases that 'requireText'
silently collapses:

  * key absent (or explicitly @null@) — 'Right Nothing'
  * present as a string — 'Right (Just ...)'
  * present but the wrong JSON type — 'Left' with a message naming the
    actual type, so a typo like @{"collection": 42}@ surfaces instead of
    being treated as "omitted".
-}
optionalText :: Text -> KeyMap Value -> Either Text (Maybe Text)
optionalText key args = case KM.lookup (fromText key) args of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just (String t) -> Right (Just t)
    Just (Object _) -> wrongType "object"
    Just (Array _) -> wrongType "array"
    Just (Number _) -> wrongType "number"
    Just (Bool _) -> wrongType "boolean"
  where
    wrongType ty = Left ("Parameter '" <> key <> "' must be a string, got " <> ty)

-- | Read an argument that may be either a JSON array of strings or a single string.
textArrayArg :: Text -> KeyMap Value -> [Text]
textArrayArg key args = case KM.lookup (fromText key) args of
    Just (Array arr) -> [t | String t <- toList arr]
    Just (String t) -> [t]
    _ -> []
  where
    toList = foldr (:) []

{- | Parse an array-valued argument into '[a]' via the 'FromJSON' instance.
A 'Just' @whenMissing@ rejects missing\/null with that message; 'Nothing'
treats both as the empty list. Aeson errors are surfaced verbatim.
-}
parseArrayArg :: (FromJSON a) => Text -> Maybe Text -> KeyMap Value -> Either Text [a]
parseArrayArg key whenMissing args = case KM.lookup (fromText key) args of
    Nothing -> maybe (Right []) Left whenMissing
    Just Null -> maybe (Right []) Left whenMissing
    Just v -> case fromJSON v of
        Success xs -> Right xs
        Error e -> Left (T.pack e)

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

{- | Load a configured database into the working set. Wraps
'DM.loadDatabase', which also auto-loads declared dependencies. Any
dependency that fails to load is surfaced in the 'dependencies' array
(as a DepLoadFailed entry) rather than swallowed. The hosting memory
budget applies here exactly as on the REST endpoint — a quota that only
guards one door is not a quota.
-}
callLoadDatabase :: DatabaseManager -> Maybe HostingConfig -> Value -> KeyMap Value -> IO Value
callLoadDatabase dbManager mHosting rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    liftIO (loadQuotaRefusal dbManager mHosting dbName) >>= maybe (pure ()) throwE
    (_loaded, deps) <- ExceptT (DM.loadDatabase dbManager dbName)
    pure $
        toolSuccessJson rid $
            object
                [ "status" .= ("loaded" :: Text)
                , "database" .= dbName
                , "dependencies" .= deps
                ]

{- | Unload a database from the working set. Wraps 'DM.unloadDatabase',
which refuses (returns 'Left') when another loaded database still
depends on it.
-}
callUnloadDatabase :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callUnloadDatabase dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    ExceptT (DM.unloadDatabase dbManager dbName)
    pure $
        toolSuccessJson rid $
            object
                [ "status" .= ("unloaded" :: Text)
                , "database" .= dbName
                ]

{- | Read a database's sources again under another allocation key. Wraps
'Edit.deriveDatabase', which refuses a key that divides no block rather than
registering the source a second time under a name promising otherwise. The
hosting quota applies as it does to a copy: what comes out is another loaded
database of the user's own.
-}
callDeriveDatabase :: DatabaseManager -> Maybe HostingConfig -> Value -> KeyMap Value -> IO Value
callDeriveDatabase dbManager mHosting rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    newName <- except (requireText "new_name" args)
    key <- except (parseAllocationKey (fromMaybe "declared" (textArg "allocation" args)))
    liftIO (quotaCounts dbManager) >>= \(uploaded, loadedUploads) ->
        maybe (pure ()) throwE (copyRefusal uploaded loadedUploads mHosting)
    (loaded, deps) <- ExceptT (deriveDatabase dbManager dbName newName key)
    pure $
        toolSuccessJson rid $
            object
                [ "status" .= ("loaded" :: Text)
                , "database" .= newName
                , "source" .= dbName
                , "allocation" .= allocationKeyText key
                , "activities" .= V.length (dbActivities (ldDatabase loaded))
                , "dependencies" .= deps
                ]

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

{- | Explicit classification filter from the @classification@ +
@classification_value@ args (honouring @classification_match@). Shared by the
search, consumers, and supply-chain handlers.
-}
explicitClassFilter :: KeyMap Value -> [(Text, Text, Bool)]
explicitClassFilter args = case (textArg "classification" args, textArg "classification_value" args) of
    (Just sys, Just val) -> [(sys, val, isExact)]
    _ -> []
  where
    isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]

-- | The @preset@ argument expanded, as every tool advertising that parameter must.
presetFilters :: [ClassificationPreset] -> KeyMap Value -> Either Text [(Text, Text, Bool)]
presetFilters presets args = expandClassificationPreset presets (textArg "preset" args)

{- | Preset filters (looked up by @preset@ name) followed by the explicit
filter. Shared by the search and consumers handlers.
-}
classificationFilters :: [ClassificationPreset] -> KeyMap Value -> Either Text [(Text, Text, Bool)]
classificationFilters presets args = (++ explicitClassFilter args) <$> presetFilters presets args

callSearchActivities :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchActivities presets rid args (db, _) = runTool rid $ do
    classifications <- except (classificationFilters presets args)
    let sf =
            Service.SearchFilter
                { Service.sfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = textArg "name" args
                        , Service.afcLocation = textArg "geo" args
                        , Service.afcProduct = textArg "product" args
                        , Service.afcClassifications = classifications
                        , Service.afcLimit = intArg "limit" args <|> Just 20
                        , Service.afcOffset = Nothing
                        , Service.afcSort = Nothing
                        , Service.afcOrder = Nothing
                        }
                , Service.sfExactMatch = fromMaybe False (boolArg "exact" args)
                }
    val <- liftIO (Service.searchActivities db sf) >>= liftShow
    pure (toolSuccessJson rid val)

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

{- | The three tab counts for one query, in one call.

A missing query is an error rather than three zeros: zeros would read as "this
database has nothing", which is a different answer from "you asked nothing".
-}
callCountSearchMatches :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callCountSearchMatches rid args (db, _) =
    case mfilter (not . null . Normalize.queryWords) (textArg "query" args) of
        Nothing -> return $ toolError rid "query is required: there is nothing to count without one"
        Just query ->
            let counts = Service.searchCounts db Service.countAsListed query
             in return $
                    toolSuccessJson rid $
                        object
                            [ "processes" .= Service.scProcesses counts
                            , "products" .= Service.scProducts counts
                            , "flows" .= Service.scFlows counts
                            ]

callSearchFlows :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchFlows rid args (db, _) =
    case readKind of
        Left err -> return $ toolError rid err
        Right kinds -> case textArg "query" args of
            Nothing -> return $ toolSuccessJson rid Service.emptyFlowSearchResults
            Just query -> do
                let limit = intArg "limit" args
                    ff =
                        Service.FlowFilter
                            { Service.ffQuery = query
                            , Service.ffLang = Nothing
                            , Service.ffKind = kinds
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
    -- A typo must not read as "every kind", the way a dropped filter would,
    -- and neither must a value that is not text at all: 'textArg' cannot tell
    -- @{"kind": true}@ from an absent key, so the lookup is read here.
    readKind :: Either Text KindFilter
    readKind = case KM.lookup (fromText "kind") args of
        Nothing -> Right AnyKind
        Just Null -> Right AnyKind
        Just (String raw) -> OnlyKinds <$> parseKindNames raw
        Just other -> Left (badKind (TE.decodeUtf8Lenient (BSL.toStrict (encode other))))

    badKind :: Text -> Text
    badKind got = "kind must be one of: " <> exchangeKindChoices <> " (got " <> got <> ")"

callGetActivity :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetActivity rid args (db, _) = runTool rid $ do
    pid <- except (requireText "process_id" args)
    _ <- except validatedExchangeType
    val <- liftShow (Service.getActivityInfo db pid)
    pure $ case fromJSON val of
        -- 'val' was built from an 'ActivityInfo' upstream, so a decode
        -- failure is genuinely defensive — pass it through unchanged,
        -- hint-less. Unless filters were asked for: answering the whole
        -- activity to a caller who asked for a subset of it looks like the
        -- subset, and nothing in the reply says otherwise.
        Error _
            | noFilters -> toolSuccessJson rid val
            | otherwise -> toolError rid "Could not read this activity's exchanges, so the filters asked for could not be applied"
        Success ai ->
            -- Single resolve: take the activity name from the 'ActivityInfo'
            -- already in hand instead of asking the engine to resolve the PID again.
            let attach = attachMarketHintByName (pfaActivityName (piActivity ai))
                payload
                    | noFilters = val
                    | otherwise =
                        toJSON
                            ai
                                { piActivity =
                                    (piActivity ai)
                                        { pfaExchanges =
                                            keptExchanges (pfaExchanges (piActivity ai))
                                        }
                                }
             in toolSuccessJson rid (attach payload)
  where
    exchangeType = textArg "exchange_type" args
    flowFilter = textArg "flow" args
    isInputFilter = boolArg "is_input" args
    -- Mirror /api/aggregate's strictness: silently swallowing typos like
    -- `exchange_type=tecnosphere` would yield "all exchanges" with no signal
    -- to the caller that the filter was ignored. Now four-valued:
    -- waste is its own kind, distinct from biosphere.
    validatedExchangeType = case exchangeType of
        Nothing -> Right Nothing
        Just "all" -> Right Nothing
        Just other -> case parseExchangeKind other of
            Just k -> Right (Just k)
            Nothing ->
                Left $ "exchange_type must be one of: all | " <> exchangeKindChoices <> " (got " <> other <> ")"
    noFilters =
        exchangeType `elem` [Nothing, Just "all"]
            && isNothing flowFilter
            && isNothing isInputFilter
    -- Kind and direction judge one exchange at a time; the flow name is
    -- judged against the whole list, since keeping only the closest match
    -- needs to know what else matched.
    keptExchanges = matchingFlowName . filter (\ewu -> matchType ewu && matchIsInput ewu)
    matchType ewu = case validatedExchangeType of
        Right (Just want) -> exchangeKindOf (ewuExchange ewu) == want
        _ -> True
    -- The query read the way search_flows reads it, so a name found there
    -- filters here, synonyms included whenever the exchange resolves to a
    -- flow.
    matchingFlowName = case flowFilter of
        Nothing -> id
        Just q -> filterByName q searchableFieldsOf
    -- Unresolved flow: the rendered placeholder is all there is to match.
    searchableFieldsOf ewu =
        maybe [ewuFlowName ewu] flowSearchFields (lookupExchangeFlow db (ewuExchange ewu))
    matchIsInput ewu = case isInputFilter of
        Nothing -> True
        Just want -> exchangeIsInput (ewuExchange ewu) == want

callGetSupplyChain :: DatabaseManager -> [ClassificationPreset] -> Value -> KeyMap Value -> IO Value
callGetSupplyChain dbManager presets rid args = runTool rid $ do
    (dbName, pid) <- except $ (,) <$> requireText "database" args <*> requireText "process_id" args
    ld <- requireDatabase dbManager dbName
    classifications <- except (classificationFilters presets args)
    let db = ldDatabase ld
        solver = ldSharedSolver ld
        depLookup = DM.mkDepSolverLookup dbManager
        scf =
            Service.SupplyChainFilter
                { Service.scfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = textArg "name" args
                        , Service.afcLocation = textArg "location" args
                        , Service.afcProduct = Nothing
                        , Service.afcClassifications = classifications
                        , Service.afcLimit = intArg "limit" args
                        , Service.afcOffset = Nothing
                        , Service.afcSort = Nothing
                        , Service.afcOrder = Nothing
                        }
                , Service.scfMaxDepth = intArg "max_depth" args
                , Service.scfMinQuantity = doubleArg "min_quantity" args
                , -- No MCP tool asks for the subgraph's edges; the REST route does.
                  Service.scfEdges = Service.EntriesOnly
                }
    subs <- except (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
    unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
    payload <-
        if null subs
            then -- Plain cross-DB supply chain.
                toJSON <$> (liftIO (Service.getSupplyChain unitCfg depLookup db dbName solver pid scf) >>= liftShow)
            else do
                -- Substitution-aware: re-solve the root scaling, then build from it.
                (processId, _) <- liftService (Service.resolveScorable db pid)
                (scalingVec, virtualLinks) <-
                    liftIO (Service.computeScalingVectorWithSubstitutionsCrossDB unitCfg depLookup db dbName solver processId subs) >>= liftShow
                resp <-
                    liftIO (Service.buildSupplyChainFromScalingVectorCrossDB unitCfg depLookup db dbName processId scalingVec virtualLinks scf) >>= liftShow
                pure (toJSON resp)
    pure $ toolSuccessJson rid payload

{- | Generic SQL-group-by aggregation. One small primitive for "how much X is
in Y" questions — replaces ad-hoc decomposition tools.
-}
callAggregate :: DatabaseManager -> [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callAggregate dbManager presets rid args (db, solver) =
    let dbName = fromMaybe "" (textArg "database" args) -- already validated by withDb
     in case textArg "process_id" args of
            Nothing -> return $ toolError rid "Missing required parameter: process_id"
            Just pid -> case scopeFromArg of
                Left err -> return $ toolError rid err
                Right scope -> case aggFnFromArg of
                    Left err -> return $ toolError rid err
                    Right fn -> case filterExchangeTypeFromArg of
                        Left err -> return $ toolError rid err
                        Right filterExchangeType | Just msg <- Agg.exchangeTypeScopeError scope filterExchangeType -> return $ toolError rid msg
                        Right filterExchangeType -> case presetFilters presets args of
                            Left err -> return $ toolError rid err
                            Right fromPreset -> do
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
                                                fromPreset ++ mapMaybe parseClassFilter (textArrayArg "filter_classification" args)
                                            , Agg.apFilterTargetName = textArg "filter_target_name" args
                                            , Agg.apFilterConsumer = textArg "filter_consumer" args
                                            , Agg.apFilterConsumerNot =
                                                maybe [] (map T.strip . T.splitOn ",") (textArg "filter_consumer_not" args)
                                            , Agg.apFilterExchangeType = filterExchangeType
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
        Just "consumption" -> Right Agg.ScopeConsumption
        Nothing -> Left "Missing required parameter: scope (direct | supply_chain | biosphere | consumption)"
        Just other -> Left ("Invalid scope: " <> other)
    aggFnFromArg = case textArg "aggregate" args of
        Nothing -> Right Agg.AggSum
        Just "sum_quantity" -> Right Agg.AggSum
        Just "count" -> Right Agg.AggCount
        Just "share" -> Right Agg.AggShare
        Just other -> Left ("Invalid aggregate fn: " <> other)
    -- Mirror /api/aggregate's strict parsing: a typo like
    -- @filter_exchange_type=tecnosphere@ used to silently return unfiltered
    -- results; surface it via toolError instead.
    filterExchangeTypeFromArg = case textArg "filter_exchange_type" args of
        Nothing -> Right Nothing
        Just other -> case parseExchangeKind other of
            Just k -> Right (Just k)
            Nothing -> Left ("filter_exchange_type must be one of: " <> exchangeKindChoices <> " (got " <> other <> ")")
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
callGetPathTo rid args (db, solver) = runTool rid $ do
    pid <- except (requireText "process_id" args)
    target <- except (requireText "target" args)
    val <- liftIO (Service.getPathTo db solver pid (Service.NamePattern target)) >>= liftShow
    pure (toolSuccessJson rid val)

callGetConsumers :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetConsumers presets rid args (db, _) = runTool rid $ do
    pid <- except (requireText "process_id" args)
    classifications <- except (classificationFilters presets args)
    let dbName = fromMaybe "" (textArg "database" args) -- validated by withDb
        cnf =
            Service.ConsumerFilter
                { Service.cnfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = textArg "name" args
                        , Service.afcLocation = textArg "location" args
                        , Service.afcProduct = textArg "product" args
                        , Service.afcClassifications = classifications
                        , Service.afcLimit = intArg "limit" args
                        , Service.afcOffset = Nothing
                        , Service.afcSort = Nothing
                        , Service.afcOrder = Nothing
                        }
                , Service.cnfMaxDepth = intArg "max_depth" args
                , Service.cnfEdges = if fromMaybe False (boolArg "include_edges" args) then Service.WithEdges else Service.EntriesOnly
                }
    results <- liftShow (Service.getConsumers db dbName pid cnf)
    pure (toolSuccessJson rid (toJSON results))

{- | MCP get_inventory: route through the cross-DB back-substitution path
so inventories from dep DBs are merged into the returned flows.
-}
callGetInventory :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetInventory dbManager rid args =
    runTool rid $ do
        (dbName, pid) <- except $ (,) <$> requireText "database" args <*> requireText "process_id" args
        ld <- requireDatabase dbManager dbName
        let db = ldDatabase ld
            solver = ldSharedSolver ld
            limit = fromMaybe 50 (intArg "limit" args)
            nameFilter = textArg "flow" args
        except $ ensureLinked dbName "computing inventory" db
        (processId, activity) <- liftService (Service.resolveScorable db pid)
        subs <- except (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
        unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        -- Empty subs: same as GET path (plain cross-DB inventory).
        -- Non-empty subs: route through the substitution-aware pipeline so
        -- dep DBs re-solve against the substituted root scaling.
        inventory <-
            ExceptT $
                if null subs
                    then fmap (fmap SharedSolver.csInventory) (computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup dbManager) db dbName solver processId)
                    else
                        either (Left . T.pack . show) (Right . SharedSolver.csInventory)
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
            -- The query read the way search_flows reads it, synonyms
            -- included, and only its closest match kept.
            filtered = case nameFilter of
                Nothing -> flows
                Just q -> filterByName q (flowSearchFields . BioKind . ifdFlow) flows
            sorted = L.sortBy (\a b -> compare (abs $ ifdQuantity b) (abs $ ifdQuantity a)) filtered
            topN = take limit sorted
            slim f =
                object
                    [ "flow" .= bfName (ifdFlow f)
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
                    , -- What the filter kept, so a caller can tell 50 rows
                      -- shown out of 50 matched from 50 out of 300.
                      "matched_flows" .= length filtered
                    , "shown_flows" .= length topN
                    , "flows" .= map slim topN
                    ]

-- | JSON shape for one uncharacterized-flow diagnostic entry.
encodeUncharacterized :: UncharacterizedFlow -> Value
encodeUncharacterized u =
    object
        [ "flow_id" .= UUID.toText (ucfFlowId u)
        , "name" .= ucfFlowName u
        , "category" .= ucfCategory u
        , "subcompartment" .= ucfSubcomp u
        , "unit" .= ucfFlowUnit u
        , "quantity" .= ucfQuantity u
        , "abs_weight" .= ucfAbsWeight u
        , "similar_cfs" .= map encodeSimilarCF (ucfSimilarCFs u)
        ]

-- | JSON shape for one suggested CF candidate.
encodeSimilarCF :: SimilarCF -> Value
encodeSimilarCF s =
    object
        [ "cf_name" .= scfMethodFlowName s
        , "cas" .= scfCAS s
        , "score" .= scfScore s
        , "reason" .= encodeReason (scfReason s)
        , "cf_value" .= scfCfValue s
        , "cf_unit" .= scfCfUnit s
        ]
  where
    encodeReason :: SimilarReason -> Text
    encodeReason SimByJaccard = "jaccard"
    encodeReason SimBySynonymExpansion = "synonym_expansion"
    encodeReason SimByCASBridge = "cas_bridge"

{- | Everything an LCA-impacts handler needs after running the request.

Bundled so that 'callGetImpacts' and the (future) 'callCompareImpacts'
share one path through the math — there must be no second implementation
to drift from this one.
-}
data ImpactsResult = ImpactsResult
    { irOutcome :: !LCIAOutcome
    , irMappingStats :: !MappingStats
    , irTables :: !Mapping.MethodTables
    {- ^ The method's tables, for annotating each contributing flow with how
    its factor was found. Shared with the manager's cache, not a copy.
    -}
    , irContribs :: ![(BiosphereFlow, Double, Double)]
    -- ^ Sorted descending by absolute contribution.
    , irUnknownUuids :: ![UUID.UUID]
    , irFunctionalUnit :: !Text
    -- ^ What one score is reported against, per 'Service.functionalUnitOf'.
    }

{- | Run a fully resolved LCA request: solve inventory, map flows, score.

Pure-data return: the JSON envelope is the caller's job, so different
audit tools (single-impact, cross-DB compare) can format the same
underlying numbers differently without duplicating the math.
-}
runImpactsRequest ::
    DatabaseManager ->
    KeyMap Value ->
    LcaRequest ->
    ExceptT Text IO ImpactsResult
runImpactsRequest dbManager args req = do
    let ld = lrLoaded req
        db = ldDatabase ld
        method = lrMethod req
        dbName = lrDbName req
        collection = lrCollection req
        ra = lrResolved req
    except $ ensureLinked dbName "computing impacts" db
    subs <- except (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
    unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    solvedInventory <-
        ExceptT $
            if null subs
                then fmap (fmap SharedSolver.csInventory) (computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup dbManager) db dbName (ldSharedSolver ld) (raPid ra))
                else
                    either (Left . T.pack . show) (Right . SharedSolver.csInventory)
                        <$> Service.inventoryWithSubsAndDeps
                            unitCfg
                            (DM.mkDepSolverLookup dbManager)
                            db
                            dbName
                            (ldSharedSolver ld)
                            (raPid ra)
                            subs
    let ltMode = longTermModeFromExclude (fromMaybe False (boolArg "exclude_long_term" args))
        inventory = applyLongTermMode mFlows ltMode solvedInventory
    mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName collection db method
    tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collection db method
    let stats = computeMappingStats mappings
        baseOutcome = computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables
        (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
        contribs = L.sortOn (\(_, _, c) -> negate (abs c)) rawContribs
        functionalUnit = Service.functionalUnitOf (dbTechFlows db) mUnits (raActivity ra)
    -- Diagnostics path: opt-in via include_diagnostics. Skips the suggester
    -- work entirely when not requested, so the hot path stays bit-identical
    -- to runs without the flag.
    outcome <-
        if fromMaybe False (boolArg "include_diagnostics" args)
            then do
                idx <- liftIO $ DM.mapMethodToIndexCached dbManager dbName collection method
                let opts = defaultUncharacterizedOpts
                    diagnostics =
                        Mapping.findUncharacterized
                            unitCfg
                            mUnits
                            mFlows
                            inventory
                            tables
                            (DM.dmChemSynonyms dbManager)
                            idx
                            opts
                pure baseOutcome{loUncharacterized = diagnostics, loUnknownUuids = unknownUuids}
            else pure baseOutcome
    pure
        ImpactsResult
            { irOutcome = outcome
            , irMappingStats = stats
            , irTables = tables
            , irContribs = contribs
            , irUnknownUuids = unknownUuids
            , irFunctionalUnit = functionalUnit
            }

{- | Handler for the 'get_impacts' MCP tool (computes LCIA score).
Historically named 'get_lcia' — the MCP surface now uses 'impacts'
per the naming audit; internal Haskell types keep the 'LCIA' acronym
(LCIAResult, computeLCIAScore) since they're the domain term of art.
-}
callGetImpacts :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callGetImpacts dbManager mBaseUrl rid args =
    runTool rid $ do
        req <- loadLcaRequest dbManager args
        ir <- runImpactsRequest dbManager args req
        (_, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        let topN = fromMaybe 5 (intArg "top_flows" args)
            method = lrMethod req
            dbName = lrDbName req
            ra = lrResolved req
            score = loScore (irOutcome ir)
            stats = irMappingStats ir
            functionalUnit = irFunctionalUnit ir
            contribs = irContribs ir
            topFlows = take topN contribs
            webUrlPair = webUrlField mBaseUrl ("/db/" <> dbName <> "/activity/" <> raText ra <> "/impacts/" <> encodeSegment (DM.unCollectionName (lrCollection req)) <> "/" <> lrMethodIdText req)
            hasNeg = any (\(_, _, c) -> c < 0) contribs
            unknownUuids = irUnknownUuids ir
        liftIO $
            unless (null unknownUuids) $
                reportProgress Warning $
                    "[MCP get_impacts "
                        <> T.unpack (methodName method)
                        <> "] "
                        <> show (length unknownUuids)
                        <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                        <> show (take 3 unknownUuids)
        let outcome = irOutcome ir
            diagnosticsFields =
                [ "uncharacterized_flows" .= map encodeUncharacterized (loUncharacterized outcome)
                , "characterized_share"
                    .= ( if loInventoryAbsSum outcome > 0
                            then loCharacterizedSum outcome / loInventoryAbsSum outcome
                            else 1 :: Double
                       )
                ]
        pure $
            toolSuccessJson rid $
                attachMarketHintByName (activityName (raActivity ra)) $
                    object $
                        [ "method" .= methodName method
                        , "category" .= methodCategory method
                        , "score" .= score
                        , "unit" .= methodUnit method
                        , "functional_unit" .= functionalUnit
                        , "mapped_flows" .= (msTotal stats - msUnmatched stats)
                        , "has_negative_contributions" .= hasNeg
                        , "top_flows"
                            .= [ object
                                    [ "flow_name" .= bfName f
                                    , "contribution" .= c
                                    , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                    , "flow_id" .= UUID.toText (bfId f)
                                    , "category" .= bfCompartmentName f
                                    , "compartment" .= bfCompartmentSub f
                                    , "cf_value" .= cfVal
                                    , "match_kind" .= Explain.flowMatchKind (irTables ir) (bfId f)
                                    , "flow_unit" .= getUnitNameForBioFlow mUnits f
                                    ]
                               | (f, cfVal, c) <- topFlows
                               ]
                        ]
                            ++ webUrlPair
                            ++ (if fromMaybe False (boolArg "include_diagnostics" args) then diagnosticsFields else [])

{- | Handler for the 'compute_sensitivity' MCP tool. Mirrors the REST
@POST /sensitivity/{collection}/{methodId}@ endpoint: runs Service.computeSensitivities
to get baseline + per-perturbation scaling vectors, then computes the LCIA score
for each. Uses 'computeLCIAScoreAuto' so regionalized methods route through the
location-hierarchy walk; non-regionalized methods stay on the classic
'computeLCIAScoreFromTables' path.
-}
callComputeSensitivity :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callComputeSensitivity dbManager mBaseUrl rid args =
    runTool rid $ do
        req <- loadLcaRequest dbManager args
        let ld = lrLoaded req
            db = ldDatabase ld
            method = lrMethod req
            dbName = lrDbName req
            collection = lrCollection req
            ra = lrResolved req
        except $ ensureLinked dbName "computing sensitivity" db
        perts <-
            ExceptT $
                pure
                    ( parseArrayArg
                        "perturbations"
                        (Just "'perturbations' is required (array of {consumer, supplier, delta, label?})")
                        args ::
                        Either Text [Perturbation]
                    )
        unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collection db method
        let hier = DM.dmLocationHierarchy dbManager
        eRes <-
            liftIO $
                Service.computeSensitivities db (ldSharedSolver ld) (raPid ra) perts
        (baselineX, perResults) <- liftShow eRes
        let scoreOf x = computeLCIAScoreAuto unitCfg mUnits mFlows db x (applyBiosphereMatrix db x) hier tables
        baselineScore <- case scoreOf baselineX of
            Right s -> pure s
            Left e -> throwE ("baseline scoring failed: " <> e)
        let webUrlPair = webUrlField mBaseUrl ("/db/" <> dbName <> "/activity/" <> raText ra <> "/sensitivity/" <> encodeSegment (DM.unCollectionName (lrCollection req)) <> "/" <> lrMethodIdText req)
            pertEntry (p, eitherX) =
                let base =
                        [ "perturbation"
                            .= object
                                [ "consumer" .= perConsumer p
                                , "supplier" .= perSupplier p
                                , "delta" .= perDelta p
                                ]
                        ]
                    withLabel = case perLabel p of
                        Just l -> ("label" .= l) : base
                        Nothing -> base
                 in case eitherX of
                        Left err -> object (("error" .= err) : withLabel)
                        Right x' -> case scoreOf x' of
                            Left err -> object (("error" .= err) : withLabel)
                            Right s ->
                                object
                                    ( ("score" .= s)
                                        : ("delta_score" .= (s - baselineScore))
                                        : withLabel
                                    )
        pure $
            toolSuccessJson rid $
                object $
                    [ "method" .= methodName method
                    , "category" .= methodCategory method
                    , "unit" .= methodUnit method
                    , "baseline_score" .= baselineScore
                    , "perturbed" .= map pertEntry perResults
                    ]
                        ++ webUrlPair

{- | Cross-database impact comparison for mapping audits.

Scores the same logical activity twice — once on @(database_a, method_a)@,
once on @(database_b, method_b)@ — and reports the per-impact-category
delta plus a per-flow drill-down. Built for the BAFU+EF3.1 vs SimaPro+EF3.1
audit: the SimaPro side is the trusted ground truth, the BAFU side is the
mapping under test, and 'delta.relative_pct' is the headline metric to
drive down.

Per-flow alignment uses (normalized name, medium, subcompartment) — NOT
UUIDs — because UUIDs differ across databases by construction (each parser
generates them in its own namespace), and that's exactly the problem this
audit is designed to expose.
-}
callCompareImpacts :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callCompareImpacts dbManager rid args =
    runTool rid $ do
        argsA <- except $ subArgs "_a" args
        argsB <- except $ subArgs "_b" args
        reqA <- loadLcaRequest dbManager argsA
        reqB <- loadLcaRequest dbManager argsB
        irA <- runImpactsRequest dbManager argsA reqA
        irB <- runImpactsRequest dbManager argsB reqB
        let topN = fromMaybe 10 (intArg "top_flows" args)
            scoreA = loScore (irOutcome irA)
            scoreB = loScore (irOutcome irB)
            delta = scoreA - scoreB
            relPct =
                if scoreB /= 0
                    then abs delta / abs scoreB * 100
                    else 0
            -- Gathered, not overwritten: 'flowKey' drops the UUID on purpose, so
            -- two flows that differ only by it are one flow on this axis and both
            -- contributions belong to its total. Every number below is read back
            -- from these maps, and so is the ranking, so the two sides are
            -- compared and ordered on the same basis.
            alignedA = alignContribs (irContribs irA)
            alignedB = alignContribs (irContribs irB)
            aMap = M.map snd alignedA
            bMap = M.map snd alignedB
            aTop = topFlows topN alignedA
            bTop = topFlows topN alignedB
            common =
                [ object
                    [ "flow_name" .= bfName f
                    , "category" .= bfCompartmentName f
                    , "compartment" .= bfCompartmentSub f
                    , "a_contrib" .= cA
                    , "b_contrib" .= cB
                    , "delta" .= (cA - cB)
                    ]
                | f <- aTop
                , let k = flowKey f
                , Just cA <- [M.lookup k aMap]
                , Just cB <- [M.lookup k bMap]
                ]
            aOnly =
                [ encodeContrib f c
                | f <- aTop
                , M.notMember (flowKey f) bMap
                , Just c <- [M.lookup (flowKey f) aMap]
                ]
            bOnly =
                [ encodeContrib f c
                | f <- bTop
                , M.notMember (flowKey f) aMap
                , Just c <- [M.lookup (flowKey f) bMap]
                ]
        pure $
            toolSuccessJson rid $
                object
                    [ "a" .= sideJson reqA irA
                    , "b" .= sideJson reqB irB
                    , "delta"
                        .= object
                            [ "absolute" .= delta
                            , "relative_pct" .= relPct
                            ]
                    , "common_flows" .= common
                    , "top_a_only_flows" .= aOnly
                    , "top_b_only_flows" .= bOnly
                    ]
  where
    sideJson req ir =
        let outcome = irOutcome ir
            characterizedShare =
                if loInventoryAbsSum outcome > 0
                    then loCharacterizedSum outcome / loInventoryAbsSum outcome
                    else 1 :: Double
         in object
                [ "database" .= lrDbName req
                , "process_id" .= raText (lrResolved req)
                , "method" .= methodName (lrMethod req)
                , "score" .= loScore outcome
                , "unit" .= methodUnit (lrMethod req)
                , "characterized_share" .= characterizedShare
                ]
    encodeContrib f c =
        object
            [ "flow_name" .= bfName f
            , "category" .= bfCompartmentName f
            , "compartment" .= bfCompartmentSub f
            , "contribution" .= c
            ]

    -- Contributions gathered on the key the two sides are compared by. The rows
    -- arrive largest first, so the first of a key keeps its spelling for
    -- display while the total is what the comparison ranks and reports.
    alignContribs :: [(BiosphereFlow, Double, Double)] -> M.Map (Text, Text, Text) (BiosphereFlow, Double)
    alignContribs contribs =
        M.fromListWith
            (\(_, cNew) (f, cOld) -> (f, cOld + cNew))
            [(flowKey f, (f, c)) | (f, _, c) <- contribs]

    -- The n flows contributing most on that key. Ranking single rows instead
    -- would leave out a flow whose rows each fall outside the window while
    -- their total leads it.
    topFlows :: Int -> M.Map (Text, Text, Text) (BiosphereFlow, Double) -> [BiosphereFlow]
    topFlows n = map fst . take n . L.sortOn (\(_, c) -> negate (abs c)) . M.elems

    -- Align flows across databases by (normalized name, medium, subcompartment).
    -- UUIDs differ across DBs by construction — see Method/Mapping comments.
    flowKey :: BiosphereFlow -> (Text, Text, Text)
    flowKey f =
        ( T.toLower (T.strip (bfName f))
        , T.toLower (bfCompartmentName f)
        , maybe "" T.toLower (bfCompartmentSub f)
        )

{- | Pull side-specific args (suffixed @_a@ / @_b@) up to the standard names
expected by 'loadLcaRequest'. Errors if any required side arg is missing.
-}
subArgs :: Text -> KeyMap Value -> Either Text (KeyMap Value)
subArgs suffix args = do
    db <- requireSide "database"
    pid <- requireSide "process_id"
    method <- requireSide "method_id"
    mCol <- optionalText ("collection" <> suffix) args
    pure $
        KM.fromList $
            [ (fromText "database", String db)
            , (fromText "process_id", String pid)
            , (fromText "method_id", String method)
            ]
                <> foldMap (\c -> [(fromText "collection", String c)]) mCol
  where
    requireSide key =
        let suffixed = key <> suffix
         in case textArg suffixed args of
                Just v -> Right v
                Nothing -> Left ("Missing required parameter: " <> suffixed)

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

{- | Supplier-gap report: what is still unsupplied after cross-DB linking.
Same wire shape as the REST endpoint ('gapReportToAPI'), so both surfaces
stay in lock-step.
-}
callGetGapReport :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetGapReport dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    report <- ExceptT (DM.databaseGapReport dbManager dbName)
    return $ toolSuccessJson rid (toJSON (gapReportToAPI (intArg "limit" args) report))

{- | The one tool that writes. It assembles the same request the HTTP endpoint
reads and goes through the same domain call, so what an assistant may change,
and what it is refused, are exactly what a person is.

Every list defaults to empty: an assistant that only removes a line should not
have to state four empty arrays to say so. An edit that ends up naming nothing
is refused by the domain, so the leniency costs no silence.
-}
callEditExchanges :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callEditExchanges dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    processId <- except (requireText "process_id" args)
    request <-
        except $
            ExchangeEditRequest
                <$> parseArrayArg "remove" Nothing args
                <*> parseArrayArg "set_amounts" Nothing args
                <*> parseArrayArg "add_inputs" Nothing args
                <*> parseArrayArg "add_biosphere" Nothing args
                <*> parseArrayArg "add_waste_outputs" Nothing args
    edits <- except (first (T.intercalate "\n") (toExchangeEdits request))
    report <- ExceptT (first refusalMessage <$> editExchanges dbManager dbName processId edits)
    return $ toolSuccessJson rid (toJSON (editReportToAPI report))

{- | Dataset-soundness report: what is malformed in the database itself. Same
wire shape as the REST endpoint ('qualityReportToAPI'), so both surfaces stay
in lock-step.
-}
callGetQualityReport :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetQualityReport dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    report <- ExceptT (DM.databaseQualityReport dbManager dbName)
    return $ toolSuccessJson rid (toJSON (qualityReportToAPI (intArg "limit" args) report))

{- | Computed-checks report: what a loaded database computes, judged against
the catalogue's own norms. Same wire shape as the REST endpoint, so both
surfaces stay in lock-step.
-}
callGetComputedQualityReport :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetComputedQualityReport dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    res <- liftIO $ BI.runComputedQuality dbManager dbName (textArg "collection" args) (intArg "limit" args)
    case res of
        Left e -> throwE (batchErrorMsg e)
        Right r -> pure (toolSuccessJson rid (toJSON r))

{- | Characterization-coverage report: the flows each loaded method collection
scores only through a name bridge. Same wire shape as the REST endpoint
('coverageReportToAPI'), so both surfaces stay in lock-step.
-}
callGetCoverageReport :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetCoverageReport dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    mCollection <- except (optionalText "collection" args)
    report <- ExceptT (DM.databaseCoverageReport dbManager dbName mCollection)
    return $ toolSuccessJson rid (toJSON (coverageReportToAPI (intArg "limit" args) report))

callGetFlowMapping :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetFlowMapping dbManager rid args = runTool rid $ do
    (dbName, methodIdText, mCol) <- except $ (,,) <$> requireText "database" args <*> requireText "method_id" args <*> optionalText "collection" args
    ld <- requireDatabase dbManager dbName
    (collection, method) <- ExceptT (resolveMethod dbManager mCol methodIdText)
    let db = ldDatabase ld
    mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName collection db method
    let stats = computeMappingStats mappings
        total = msTotal stats
        matched = total - msUnmatched stats
        coverage =
            if total > 0
                then fromIntegral matched / fromIntegral total * 100 :: Double
                else 0
        verbose = fromMaybe False (boolArg "verbose" args)
        maxUnm = fromMaybe 50 (intArg "max_unmatched" args)
    extra <-
        if not verbose
            then pure []
            else do
                let unmatchedCFs =
                        take
                            maxUnm
                            [ object
                                [ "name" .= mcfFlowName cf
                                , "cas" .= mcfCAS cf
                                , "compartment" .= mcfCompartment cf
                                , "cf_value" .= mcfValue cf
                                , "cf_unit" .= mcfUnit cf
                                ]
                            | (cf, Nothing) <- mappings
                            ]
                unmatchedFlows <- liftIO $ buildUnmatchedDbFlows dbManager dbName collection db method args maxUnm
                pure
                    [ "unmatched_cfs" .= unmatchedCFs
                    , "unmatched_db_flows" .= unmatchedFlows
                    ]
    pure $
        toolSuccessJson rid $
            object $
                [ "method" .= methodName method
                , "total" .= total
                , "matched" .= matched
                , "unmatched" .= msUnmatched stats
                , "coverage" .= coverage
                ]
                    ++ extra

{- | Verbose-mode helper: rank unmatched DB flows for a method.

When @process_id@ is given, runs 'findUncharacterized' on that activity's
inventory — the most actionable view (which uncharacterized flows actually
contribute to the score that user is auditing). Without @process_id@, falls
back to an empty list with a hint, so callers know how to ask for the
useful version. The "scan the whole biosphere matrix" mode promised by the
plan would belong here too — left for a follow-up commit if the
process-scoped view turns out to be insufficient in practice.
-}
buildUnmatchedDbFlows ::
    DatabaseManager ->
    Text ->
    DM.CollectionName ->
    Database ->
    Method ->
    KeyMap Value ->
    Int ->
    IO [Value]
buildUnmatchedDbFlows dbManager dbName collection db method args maxN =
    case textArg "process_id" args of
        Nothing -> pure [] -- caller didn't pin a process; nothing actionable to rank by
        Just pidText -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> pure []
                Just ld -> case Service.resolveActivityAndProcessId db pidText of
                    Left _ -> pure []
                    Right (pid, _) -> do
                        unitCfg <- DM.getMergedUnitConfig dbManager
                        (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
                        invE <-
                            computeInventoryMatrixWithDepsCached
                                unitCfg
                                (DM.mkDepSolverLookup dbManager)
                                db
                                dbName
                                (ldSharedSolver ld)
                                pid
                        case invE of
                            Left _ -> pure []
                            Right sol -> do
                                let inventory = SharedSolver.csInventory sol
                                tables <- DM.mapMethodToTablesCached dbManager dbName collection db method
                                idx <- DM.mapMethodToIndexCached dbManager dbName collection method
                                let opts =
                                        defaultUncharacterizedOpts
                                            { Mapping.uoMaxFlows = maxN
                                            , Mapping.uoMaxSimilar = 3
                                            }
                                    uncharacterized =
                                        Mapping.findUncharacterized
                                            unitCfg
                                            mUnits
                                            mFlows
                                            inventory
                                            tables
                                            (DM.dmChemSynonyms dbManager)
                                            idx
                                            opts
                                pure (map encodeUncharacterized uncharacterized)

{- | Why one flow scores with the factor it does.

Serves exactly what the REST route serves, through the same projection, so the
web page and an agent cannot describe the same flow differently. The
'explanation' field is the part meant to be read out; the rest is for a caller
that wants to compare or link.
-}
callExplainCF :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callExplainCF dbManager mBaseUrl rid args = runTool rid $ do
    (dbName, methodIdText, mCol) <- except $ (,,) <$> requireText "database" args <*> requireText "method_id" args <*> optionalText "collection" args
    flowIdText <- except (requireText "flow_id" args)
    ld <- requireDatabase dbManager dbName
    (collection, method) <- ExceptT (resolveMethod dbManager mCol methodIdText)
    fid <- except $ maybe (Left ("Malformed flow id: " <> flowIdText)) Right (UUID.fromText (T.strip flowIdText))
    let db = ldDatabase ld
    (flow, explanation) <- ExceptT (DM.explainFlowFactor dbManager dbName collection db method fid)
    let deepLink = (<> "/db/" <> dbName <> "/method/" <> methodIdText <> "/flow-mapping") <$> mBaseUrl
    pure $ toolSuccessJson rid (addWebUrlMaybe deepLink (toJSON (explainCFToAPI db method flow explanation)))

callGetCharacterization :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetCharacterization dbManager rid args = runTool rid $ do
    (dbName, methodIdText, mCol) <- except $ (,,) <$> requireText "database" args <*> requireText "method_id" args <*> optionalText "collection" args
    ld <- requireDatabase dbManager dbName
    (collection, method) <- ExceptT (resolveMethod dbManager mCol methodIdText)
    let db = ldDatabase ld
        lim = fromMaybe 20 (intArg "limit" args)
        flowQ = textArg "flow" args
        queryLower = fmap T.toLower flowQ
    mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName collection db method
    let matched =
            [ (cf, f, strat)
            | (cf, Just (f, strat)) <- mappings
            , matchQuery queryLower (mcfFlowName cf) (bfName f)
            ]
        sorted = L.sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
        top = take lim sorted
        mkEntry (cf, f, strat) =
            object
                [ "cf_flow_name" .= mcfFlowName cf
                , "cf_value" .= mcfValue cf
                , "cf_unit" .= mcfUnit cf
                , "direction" .= (case mcfDirection cf of Input -> "Input" :: Text; Output -> "Output")
                , "db_flow_name" .= bfName f
                , "flow_id" .= UUID.toText (bfId f)
                , "flow_unit" .= getUnitNameForBioFlow (dbUnits db) f
                , "category" .= bfCompartmentName f
                , "compartment" .= bfCompartmentSub f
                , "match_strategy" .= show strat
                ]
    pure $
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
    -- | base URL (Nothing when no frontend is bundled)
    Maybe Text ->
    DM.CollectionName ->
    -- | method UUID text
    Text ->
    UnitDB ->
    -- | total score (for share %)
    Double ->
    ((Text, ProcessId), Double) ->
    IO Value
mkMcpCrossDBEntry dbManager rootDbName mBaseUrl colName methodIdText unitDB score ((depDbName, pid), c) = do
    mLd <- getDatabase dbManager depDbName
    let (actName, actLoc, prodName, pidText) = case mLd of
            Just ld ->
                let d = ldDatabase ld
                    mAct = Service.findActivityByProcessId d pid
                    txt =
                        if depDbName == rootDbName
                            then processIdToText d pid
                            else qualifyRef depDbName (processIdToText d pid)
                    -- Reference products are technosphere; pull the supplier's tech flow map.
                    (pn, _, _) = maybe ("", 0, "") (Service.getReferenceProductInfo (dbTechFlows d) unitDB) mAct
                 in (maybe "" activityName mAct, maybe "" activityLocation mAct, pn, txt)
            Nothing ->
                ("", "", "", depDbName <> "::<unloaded>")
        webUrlPair =
            webUrlField
                mBaseUrl
                ( "/db/"
                    <> rootDbName
                    <> "/activity/"
                    <> pidText
                    <> "/contributing-activities/"
                    <> encodeSegment (DM.unCollectionName colName)
                    <> "/"
                    <> methodIdText
                )
    pure $
        object $
            [ "process_id" .= pidText
            , "activity_name" .= actName
            , "product_name" .= prodName
            , "location" .= actLoc
            , "contribution" .= c
            , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
            ]
                ++ webUrlPair

{- | Choose the (collection, method) for a UUID from the loaded set, optionally
restricted to a named collection. A method's engine UUID is a UUIDv5 of its
name, so the *same* UUID can be loaded under several collections (e.g. two EF
3.1 versions). Resolving must therefore be loud, not first-match:

  * @Just c@   — resolve within collection @c@; a UUID is unique inside one
                 collection, so this is unambiguous (or a not-found error).
  * @Nothing@  — infer. One match resolves; more than one is reported as an
                 error listing the collections to choose from, rather than
                 silently picking whichever loaded first.

Pure so the disambiguation is total and testable without a 'DatabaseManager'.
-}
selectMethod :: Maybe Text -> UUID -> [(Text, Method)] -> Either Text (Text, Method)
selectMethod mCollection uuid loaded =
    case filter keep loaded of
        [] -> Left notFound
        [hit] -> Right hit
        hit : _ -> maybe (Left ambiguous) (const (Right hit)) mCollection
  where
    keep (col, m) = methodId m == uuid && maybe True (== col) mCollection
    uuidText = UUID.toText uuid
    notFound = case mCollection of
        Nothing -> "Method not found: " <> uuidText
        Just c ->
            "Method "
                <> uuidText
                <> " not found in collection '"
                <> c
                <> "'. Loaded collections: "
                <> T.intercalate ", " (L.nub (map fst loaded))
    ambiguous =
        "Method UUID "
            <> uuidText
            <> " is loaded in multiple collections: "
            <> T.intercalate ", " (L.nub [col | (col, m) <- loaded, methodId m == uuid])
            <> ". Pass 'collection' to disambiguate."

{- | Resolve a method UUID (raw text) to its collection name and 'Method',
optionally pinned to a collection. Thin IO edge over 'selectMethod'.
-}
resolveMethod :: DatabaseManager -> Maybe Text -> Text -> IO (Either Text (DM.CollectionName, Method))
resolveMethod dbManager mCollection methodIdText =
    case UUID.fromText methodIdText of
        Nothing -> return $ Left "Invalid method UUID format"
        Just uuid ->
            fmap (first DM.CollectionName) . selectMethod mCollection uuid
                <$> DM.getLoadedMethods dbManager

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
    , lrCollection :: !DM.CollectionName
    , lrMethod :: !Method
    }

{- | Resolve every entity an LCA handler needs from raw JSON-RPC args.
Short-circuits on the first failure (missing arg, unknown DB, bad UUID,
unknown method, unresolvable process id).
-}
loadLcaRequest :: DatabaseManager -> KeyMap Value -> ExceptT Text IO LcaRequest
loadLcaRequest dbManager args = do
    (dbName, pidText, methodIdText, mCol) <-
        except $
            (,,,)
                <$> requireText "database" args
                <*> requireText "process_id" args
                <*> requireText "method_id" args
                <*> optionalText "collection" args
    ld <- requireDatabase dbManager dbName
    (col, method) <- ExceptT (resolveMethod dbManager mCol methodIdText)
    (pid, act) <- liftService (Service.resolveScorable (ldDatabase ld) pidText)
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

callGetContributingFlows :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callGetContributingFlows dbManager mBaseUrl rid args =
    runTool rid $ do
        req <- loadLcaRequest dbManager args
        let ld = lrLoaded req
            db = ldDatabase ld
            method = lrMethod req
            dbName = lrDbName req
            collection = lrCollection req
            ra = lrResolved req
            lim = fromMaybe 20 (intArg "limit" args)
            webUrlPair = webUrlField mBaseUrl ("/db/" <> dbName <> "/activity/" <> raText ra <> "/contributing-flows/" <> encodeSegment (DM.unCollectionName (lrCollection req)) <> "/" <> lrMethodIdText req)
        except $ ensureLinked dbName "computing contributions" db
        unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        sol <-
            ExceptT $
                computeInventoryMatrixWithDepsCached
                    unitCfg
                    (DM.mkDepSolverLookup dbManager)
                    db
                    dbName
                    (ldSharedSolver ld)
                    (raPid ra)
        let ltMode = longTermModeFromExclude (fromMaybe False (boolArg "exclude_long_term" args))
            inventory = applyLongTermMode mFlows ltMode (SharedSolver.csInventory sol)
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collection db method
        let outcome = computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables
            score = loScore outcome
            (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
            contribs = L.sortOn (\(_, _, c) -> negate (abs c)) rawContribs
            top = take lim contribs
            hasNeg = any (\(_, _, c) -> c < 0) contribs
        diagnosticsFields <-
            if fromMaybe False (boolArg "include_diagnostics" args)
                then do
                    idx <- liftIO $ DM.mapMethodToIndexCached dbManager dbName collection method
                    let opts = defaultUncharacterizedOpts
                        uncharacterized =
                            Mapping.findUncharacterized
                                unitCfg
                                mUnits
                                mFlows
                                inventory
                                tables
                                (DM.dmChemSynonyms dbManager)
                                idx
                                opts
                    pure
                        [ "uncharacterized_flows" .= map encodeUncharacterized uncharacterized
                        , "characterized_share"
                            .= ( if loInventoryAbsSum outcome > 0
                                    then loCharacterizedSum outcome / loInventoryAbsSum outcome
                                    else 1 :: Double
                               )
                        ]
                else pure []
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
                object $
                    [ "method" .= methodName method
                    , "unit" .= methodUnit method
                    , "total_score" .= score
                    , "has_negative_contributions" .= hasNeg
                    , "top_flows"
                        .= [ object
                                [ "flow_name" .= bfName f
                                , "contribution" .= c
                                , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                , "flow_id" .= UUID.toText (bfId f)
                                , "category" .= bfCompartmentName f
                                , "compartment" .= bfCompartmentSub f
                                , "cf_value" .= cfVal
                                , "match_kind" .= Explain.flowMatchKind tables (bfId f)
                                ]
                           | (f, cfVal, c) <- top
                           ]
                    ]
                        ++ webUrlPair
                        ++ diagnosticsFields

callGetContributingActivities :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callGetContributingActivities dbManager mBaseUrl rid args =
    runTool rid $ do
        req <- loadLcaRequest dbManager args
        let ld = lrLoaded req
            db = ldDatabase ld
            method = lrMethod req
            dbName = lrDbName req
            collection = lrCollection req
            ra = lrResolved req
            lim = fromMaybe 10 (intArg "limit" args)
            ltMode = longTermModeFromExclude (fromMaybe False (boolArg "exclude_long_term" args))
        except $ ensureLinked dbName "computing contributions" db
        unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collection db method
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
                    ltMode
        let score = sum (M.elems contributions)
            sorted = L.sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
            top = take lim sorted
            hasNeg = any (\(_, c) -> c < 0) top
        rows <- liftIO $ mapM (mkMcpCrossDBEntry dbManager dbName mBaseUrl (lrCollection req) (lrMethodIdText req) mUnits score) top
        pure $
            toolSuccessJson rid $
                object
                    [ "method" .= methodName method
                    , "unit" .= methodUnit method
                    , "total_score" .= score
                    , "has_negative_contributions" .= hasNeg
                    , "processes" .= rows
                    ]

callListGeographies :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callListGeographies dbManager rid args = runTool rid $ do
    dbName <- except (requireText "database" args)
    ld <- requireDatabase dbManager dbName
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
    pure $
        toolSuccessJson rid $
            object
                ["geographies" .= map mkEntry codes]

-- ============================================================================
-- score_activity / score_activities / list_scoring_sets
--
-- Wrappers around API.BatchImpacts so a single MCP call yields the full
-- LCIA panel + every configured scoring set + per-indicator breakdown,
-- removing the N round-trips of get_impacts a comparative study used to
-- need. Each response is enriched with a 'web_url' deep link to the
-- matching web UI view so a human can continue the exploration visually.
-- ============================================================================

-- | Translate a 'BI.BatchError' into the MCP 'toolError' payload.
batchErrorMsg :: BI.BatchError -> Text
batchErrorMsg err = case err of
    BI.CollectionNotLoaded name available -> collectionNotLoadedMessage name available
    BI.DatabaseNotLoaded name -> "Database not loaded: " <> name
    BI.ActivityResolutionFailed msg -> msg
    BI.LinkingIncomplete msg -> msg
    BI.OtherBatchError code msg -> "HTTP " <> T.pack (show code) <> ": " <> msg

{- | Look up the configured scoring-set names on a loaded method collection.
Returns the empty list when the collection is not loaded; in that case
the batch runner has already returned 'BI.CollectionNotLoaded' and the
filter is never consulted, so the empty result here is harmless. We
read 'mcScoringSets' directly — not the keys of @scoringResults@ — so
that a set whose evaluation produced no scores still counts as
"configured" for the @scoring_sets@ filter.
-}
configuredScoringSetNames :: DatabaseManager -> Text -> IO [Text]
configuredScoringSetNames dbm collName = do
    loaded <- readTVarIO (dmLoadedMethods dbm)
    pure $ case M.lookup collName loaded of
        Just mc -> map ssName (mcScoringSets mc)
        Nothing -> []

{- | Handler for the 'score_activity' MCP tool.

Returns the full LCIABatchResult shape (per-method scores, per-scoring-set
aggregate scores, per-indicator breakdown, units) for a single activity,
enriched with a top-level 'web_url' pointing at the impacts panel page
(which already lists every method). The per-entry @functionalUnit@ is
hoisted to the top level — it is constant across the panel — and
per-method @web_url@s are not emitted: the panel link covers the same
ground at a fraction of the bytes. Replaces the @N@ round-trips of
'get_impacts' a comparative study used to need.
-}
callScoreActivity :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callScoreActivity dbManager mBaseUrl rid args =
    runTool rid $ do
        dbName <- except (requireText "database" args)
        pidText <- except (requireText "process_id" args)
        coll <- except (requireText "collection" args)
        subs <- except (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
        wantedSets <- except (parseArrayArg "scoring_sets" Nothing args :: Either Text [Text])
        let mSub = if null subs then Nothing else Just SubstitutionRequest{srSubstitutions = subs}
            ltMode = longTermModeFromExclude (fromMaybe False (boolArg "exclude_long_term" args))
        res <- liftIO $ BI.runActivityLCIABatch dbManager dbName pidText coll mSub ltMode
        case res of
            Left e -> throwE (batchErrorMsg e)
            Right lbr -> do
                configured <- liftIO $ configuredScoringSetNames dbManager coll
                mActName <- liftIO $ lookupActivityName dbManager dbName pidText
                let mTopUrl = scoreActivityWebUrl mBaseUrl dbName pidText coll
                    enriched =
                        maybe id attachMarketHintByName mActName $
                            addWebUrlMaybe
                                mTopUrl
                                (slimLCIAPanel (toJSON lbr))
                except (toolSuccessJson rid <$> filterScoringSets configured wantedSets enriched)

{- | Resolve the activity name for a (db, processId) pair. 'Nothing' when
the database is not loaded or the PID does not resolve — callers fold
this through 'maybe id attachMarketHintByName', so a missing name
simply skips the hint without making up a default.
-}
lookupActivityName :: DatabaseManager -> Text -> Text -> IO (Maybe Text)
lookupActivityName dbManager dbName pidText = do
    mLd <- getDatabase dbManager dbName
    pure $ case mLd of
        Just ld -> case Service.resolveActivityAndProcessId (ldDatabase ld) pidText of
            Right (_, act) -> Just (activityName act)
            Left _ -> Nothing
        Nothing -> Nothing

{- | Return every 'ScoringSet' configured on a collection (full record, not
just names). Empty list when the collection is not loaded — the same
defensive shape as 'configuredScoringSetNames'.
-}
configuredScoringSets :: DatabaseManager -> Text -> IO [ScoringSet]
configuredScoringSets dbm collName = do
    loaded <- readTVarIO (dmLoadedMethods dbm)
    pure $ maybe [] mcScoringSets (M.lookup collName loaded)

{- | Handler for the 'score_activities' MCP tool.

Ranks N activities against every method in a collection in one
multi-RHS MUMPS solve plus parallel characterization, then projects the
result against a single 'ScoringSet' into a columnar JSON payload
(@{scoringSet, scoringUnit, functionalUnit, columns, rows}@). The shape
hoists the constant metadata once and packs each activity as a flat
array of scalars — typically ~6× smaller than a row-shaped JSON for a
batch of 24+ activities. Unresolved process IDs land in
@notFound@ \/ @invalid@. The chosen scoring set is required to be
unambiguous; see 'resolveSingleScoringSet' for the rules.
-}
callScoreActivities :: DatabaseManager -> Maybe Text -> Value -> KeyMap Value -> IO Value
callScoreActivities dbManager mBaseUrl rid args =
    runTool rid $ do
        dbName <- except (requireText "database" args)
        coll <- except (requireText "collection" args)
        pids <- except (parseArrayArg "process_ids" (Just "'process_ids' required (array of strings)") args :: Either Text [Text])
        wantedSets <- except (parseArrayArg "scoring_sets" Nothing args :: Either Text [Text])
        let summaryOnly = fromMaybe False (boolArg "summary_only" args)
            ltMode = longTermModeFromExclude (fromMaybe False (boolArg "exclude_long_term" args))
        configured <- liftIO $ configuredScoringSets dbManager coll
        chosen <- except (resolveSingleScoringSet wantedSets configured)
        res <- liftIO $ BI.runBatchImpacts dbManager dbName coll Nothing ltMode pids
        case res of
            Left e -> throwE (batchErrorMsg e)
            Right bir -> pure (toolSuccessJson rid (toColumnarBatch summaryOnly mBaseUrl dbName coll chosen bir))

{- | Handler for the 'list_scoring_sets' MCP tool.

Returns the formula-based scoring sets configured on every loaded
'MethodCollection'. Pure read from the live TVar; no HTTP equivalent.
When 'collection' is supplied, filters to that one and errors if it is
not loaded (listing the loaded names in the message).

The projection is explicit (rather than @toJSON ss@) so the wire format
stays in snake_case and is not silently affected by a future field
addition to 'ScoringSet'.
-}
callListScoringSets :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callListScoringSets dbManager rid args = do
    loaded <- readTVarIO (dmLoadedMethods dbManager)
    case optionalText "collection" args of
        Left err -> return $ toolError rid err
        Right Nothing -> return $ toolSuccessJson rid (encodeAll loaded)
        Right (Just collName) -> case M.lookup collName loaded of
            Nothing ->
                return $ toolError rid (collectionNotLoadedMessage collName (M.keys loaded))
            Just mc -> return $ toolSuccessJson rid (encodeAll (M.singleton collName mc))
  where
    encodeAll :: M.Map Text MethodCollection -> Value
    encodeAll loaded =
        object
            [ "collections"
                .= [ object
                        [ "collection" .= cName
                        , "scoring_sets" .= map encodeScoringSet (mcScoringSets mc)
                        ]
                   | (cName, mc) <- M.toList loaded
                   ]
            ]

    encodeScoringSet :: ScoringSet -> Value
    encodeScoringSet ss =
        object
            [ "name" .= ssName ss
            , "unit" .= ssUnit ss
            , "variables" .= ssVariables ss
            , "computed" .= ssComputed ss
            , "labels" .= ssLabels ss
            , "normalization" .= ssNormalization ss
            , "weighting" .= ssWeighting ss
            , "scores" .= ssScores ss
            , "display_multiplier" .= ssDisplayMultiplier ss
            ]

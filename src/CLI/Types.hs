{-# LANGUAGE DeriveGeneric #-}

module CLI.Types where

import Data.Text (Text)
import GHC.Generics

-- | Output format for CLI commands
data OutputFormat
    = JSON -- API-compatible JSON output
    | CSV -- Comma-separated values for data processing
    | Table -- Human-readable table format
    | Pretty -- Pretty-printed format with colors/formatting
    deriving (Eq, Show, Read, Generic)

-- | Global options that apply to all commands
data GlobalOptions = GlobalOptions
    { configFile :: Maybe FilePath -- Config file (--config), required
    , dbName :: Maybe Text -- Database name (--db), selects which loaded database to query
    , methodsDir :: Maybe FilePath -- Methods directory (--methods) for LCIA methods
    , format :: Maybe OutputFormat -- Output format (--format)
    , jsonPath :: Maybe Text -- JSONPath for CSV extraction (--jsonpath)
    , noCache :: Bool -- Disable caching (--no-cache)
    , serverUrl :: Maybe String -- Server URL (--url) for HTTP client mode
    , serverPassword :: Maybe String -- Auth password (--password, VOLCA_PASSWORD, or config)
    }
    deriving (Eq, Show, Generic)

-- | Main CLI commands - all top-level for maximum discoverability
data Command
    = -- Server mode
      Server ServerOptions
    | -- Core resource queries
      Activity Text -- Basic activity info
    | Flow Text (Maybe FlowSubCommand) -- Flow info (keep subcommands for now)
    | Inventory Text -- Life cycle inventory
    -- Search commands promoted to top-level
    | SearchActivities SearchActivitiesOptions -- Search activities
    | SearchFlows SearchFlowsOptions -- Search flows
    -- No separate synonyms command - synonyms are included in flow responses
    | Impacts Text LCIAOptions -- LCIA (impact assessment) computation
    | DebugMatrices Text DebugMatricesOptions -- Matrix debugging for activity
    | ExportMatrices FilePath -- Export matrices in universal format
    -- Resource management (symmetric subcommands)
    | Database DatabaseAction -- Database management
    | Method MethodAction -- Method collection management
    | Plugin PluginAction -- Plugin management
    -- Listing commands (mirror API)
    | Methods -- List loaded methods (flattened)
    | Synonyms -- List synonym sources
    | CompartmentMappings -- List compartment mappings
    | Units -- List unit definitions
    | FlowMapping MappingOptions -- Flow mapping coverage analysis
    | Stop -- Stop running server
    | Repl -- Interactive REPL over HTTP
    -- Hidden tooling commands (not shown in --help)
    | DumpOpenApi -- Dump OpenAPI spec as JSON to stdout
    | DumpMcpTools -- Dump MCP tool definitions as JSON to stdout
    deriving (Eq, Show, Generic)

-- | Database management actions
data DatabaseAction
    = DbList
    | DbUpload UploadArgs
    | DbDelete Text
    deriving (Eq, Show, Generic)

-- | Plugin management actions
data PluginAction
    = PluginList
    deriving (Eq, Show, Generic)

-- | Method collection management actions
data MethodAction
    = McList
    | McUpload UploadArgs
    | McDelete Text
    deriving (Eq, Show, Generic)

-- | Shared upload arguments for database and method uploads
data UploadArgs = UploadArgs
    { uaFile :: FilePath -- File to upload (archive or CSV)
    , uaName :: Text -- Display name (--name)
    , uaDescription :: Maybe Text -- Optional description (--description)
    }
    deriving (Eq, Show, Generic)

-- | Server-specific options
data ServerOptions = ServerOptions
    { serverPort :: Int -- Server port (--port)
    , serverLoadDbs :: Maybe [Text] -- Databases to load at startup (--load db1,db2)
    , serverDesktopMode :: Bool -- Desktop mode (--desktop): print port and minimize logging
    , serverStaticDir :: Maybe FilePath -- Static directory (--static-dir): override default web/dist
    , serverIdleTimeout :: Int -- Idle timeout in minutes (--idle-timeout, 0=disabled). Server exits after being idle.
    , serverTreeDepth :: Int -- Default max depth for /tree endpoint (--tree-depth, default 2)
    }
    deriving (Eq, Show, Generic)

-- | Activity sub-commands (kept for flow activities only now)
data ActivitySubCommand
    = ActivityFlows -- /activity/{uuid}/flows
    | ActivityInputs -- /activity/{uuid}/inputs
    | ActivityOutputs -- /activity/{uuid}/outputs
    | ActivityReferenceProduct -- /activity/{uuid}/reference-product
    deriving (Eq, Show, Generic)

-- | Flow sub-commands
data FlowSubCommand
    = FlowActivities -- /flow/{flowId}/activities
    deriving (Eq, Show, Generic)

-- | Search command types removed - now top-level commands

-- | Search activities options
data SearchActivitiesOptions = SearchActivitiesOptions
    { searchName :: Maybe Text -- --name filter
    , searchGeo :: Maybe Text -- --geo filter
    , searchProduct :: Maybe Text -- --product filter
    , searchLimit :: Maybe Int -- --limit for pagination
    , searchOffset :: Maybe Int -- --offset for pagination
    }
    deriving (Eq, Show, Generic)

-- | Search flows options
data SearchFlowsOptions = SearchFlowsOptions
    { searchQuery :: Maybe Text -- --query search term
    , searchLang :: Maybe Text -- --lang language filter
    , searchFlowsLimit :: Maybe Int -- --limit for pagination
    , searchFlowsOffset :: Maybe Int -- --offset for pagination
    }
    deriving (Eq, Show, Generic)

-- | Synonym command types removed - now top-level commands

-- | LCIA computation options
data LCIAOptions = LCIAOptions
    { lciaMethodId :: Text -- Method UUID (methods loaded on server)
    , lciaOutput :: Maybe FilePath -- --output XML export
    , lciaCSV :: Maybe FilePath -- --csv export
    }
    deriving (Eq, Show, Generic)

-- | Matrix debugging options
data DebugMatricesOptions = DebugMatricesOptions
    { debugOutput :: FilePath -- --output base filename (required)
    , debugFlowFilter :: Maybe Text -- --flow-filter (e.g., "Sulphur dioxide")
    }
    deriving (Eq, Show, Generic)

-- | Mapping command options
data MappingOptions = MappingOptions
    { mappingMethodId :: Text -- Method UUID
    , mappingShowMatched :: Bool -- --matched: list mapped CFs with strategy
    , mappingShowUnmatched :: Bool -- --unmatched: list CFs with no DB match
    , mappingShowUncharacterized :: Bool -- --uncharacterized: list DB flows with no CF
    }
    deriving (Eq, Show, Generic)

{- | Complete CLI configuration
Command is optional: if None, just load database and exit (useful for cache generation)
-}
data CLIConfig = CLIConfig
    { globalOptions :: GlobalOptions
    , command :: Maybe Command
    }
    deriving (Eq, Show, Generic)

-- | Helper function to parse OutputFormat from string
parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat s = case map toLower s of
    "json" -> Just JSON
    "csv" -> Just CSV
    "table" -> Just Table
    "pretty" -> Just Pretty
    _ -> Nothing
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

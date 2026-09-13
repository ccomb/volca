{-# LANGUAGE DeriveGeneric #-}

module CLI.Types where

import Data.Char (isAsciiUpper)
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
    -- Listing commands (mirror API)
    | Methods -- List loaded methods (flattened)
    | Synonyms -- List synonym sources
    | CompartmentMappings -- List compartment mappings
    | Units -- List unit definitions
    | FlowMapping MappingOptions -- Flow mapping coverage analysis
    -- Quality reports (--format csv takes the engine's own CSV rendering)
    | QualityReport (Maybe Int) -- Dataset-soundness report, --limit findings per check
    | ComputedQualityReport ComputedQualityOptions -- Computed checks over a loaded database
    | Stop -- Stop running server
    | Repl -- Interactive REPL over HTTP
    | Dump DumpTarget -- Hidden tooling: write a machine-readable document to stdout
    deriving (Eq, Show, Generic)

{- | What a hidden @dump-*@ command writes out. One constructor for all of
them, because each is answered in exactly one place, before any configuration
is read, and every other dispatch only has to know that it is not its
business.
-}
data DumpTarget
    = DumpOpenApi -- OpenAPI specification
    | DumpMcpTools -- MCP tool definitions
    | DumpConfigSchema -- Names of the keys a configuration file may carry
    deriving (Eq, Show, Generic)

-- | Database management actions
data DatabaseAction
    = DbList
    | -- | Load a configured database into memory (auto-loads its dependencies)
      DbLoad Text
    | -- | Unload a database from memory (refused if a loaded database depends on it)
      DbUnload Text
    | DbUpload UploadArgs
    | DbDelete Text
    | {- | Delete the activities matched by a filter (the whole matching set,
      pagination ignored), keeping/adding explicit ProcessIds.
      -}
      DbDeleteActivities DbDeleteArgs
    | -- | Copy a loaded database (source name → new name)
      DbCopy Text Text
    | {- | Relink a database against one dependency using a name→name supplier
      alias mapping loaded from a local CSV: @db@, @--to depDb@, @--mapping csv@.
      -}
      DbRelinkMapping DbRelinkArgs
    | -- | Export a loaded database to a file: @db@, @--format fmt@, @--out file@.
      DbExport DbExportArgs
    | -- | Write new activities read from a JSON file: @db@, @--from file@.
      DbCreateActivities DbWriteArgs
    | -- | Rewrite one activity: @db@, @--process-id pid@, @--from file@.
      DbReplaceActivity DbActivityArgs
    | {- | Change one activity's inventory, keeping the rest of it: @db@,
      @--process-id pid@, @--from file@.
      -}
      DbEditExchanges DbActivityArgs
    deriving (Eq, Show, Generic)

-- | Arguments for @database export@.
data DbExportArgs = DbExportArgs
    { deaDb :: Text
    -- ^ Database to export
    , deaFormat :: Text
    -- ^ Target format keyword (@--format@): simapro|ecospold1|ecospold2|ilcd|brightway
    , deaOut :: FilePath
    -- ^ Output file path (@--out@)
    }
    deriving (Eq, Show, Generic)

{- | Arguments for @database create-activities@: the database to write to and
a JSON file shaped like the HTTP request body (@{"activities": [...]}@), so
the same document works over either transport.
-}
data DbWriteArgs = DbWriteArgs
    { dwaDb :: Text
    -- ^ Database to write to (@--db@)
    , dwaFile :: FilePath
    -- ^ JSON file holding the activities (@--from@)
    }
    deriving (Eq, Show, Generic)

{- | Arguments for the commands that address one activity by identity:
@database replace-activity@ and @database edit-exchanges@. As 'DbWriteArgs',
plus the process id, and a file holding one document rather than a batch –
which document depends on the command, and each reads the same JSON its HTTP
endpoint does.
-}
data DbActivityArgs = DbActivityArgs
    { daDb :: Text
    -- ^ Database holding the activity (@--db@)
    , daProcessId :: Text
    -- ^ Identity of the activity addressed (@--process-id@)
    , daFile :: FilePath
    -- ^ JSON file holding the request body (@--from@)
    }
    deriving (Eq, Show, Generic)

-- | Arguments for @database relink@ with a name→name supplier alias mapping.
data DbRelinkArgs = DbRelinkArgs
    { draDb :: Text
    -- ^ Database to relink
    , draToDep :: Text
    -- ^ Dependency database to link against (@--to@)
    , draMappingCsv :: FilePath
    -- ^ Path to the mapping CSV (@--mapping@)
    }
    deriving (Eq, Show, Generic)

{- | Arguments for delete-by-selection. The filter fields mirror the activity
search filter; @ddaKeep@ spares matched ProcessIds and @ddaExtra@ adds ones
the filter missed.
-}
data DbDeleteArgs = DbDeleteArgs
    { ddaDb :: Text
    , ddaName :: Maybe Text
    , ddaLocation :: Maybe Text
    , ddaProduct :: Maybe Text
    , ddaClassSystem :: Maybe Text
    , ddaClassValue :: Maybe Text
    , ddaExact :: Bool
    , ddaKeep :: [Text]
    , ddaExtra :: [Text]
    , ddaIds :: [Text] -- Delete exactly these process ids (@--id@, excludes filters)
    }
    deriving (Eq, Show, Generic)

-- | Method collection management actions
data MethodAction
    = McList
    | McUpload UploadArgs
    | McDelete Text
    | -- | Export a loaded collection to a file: @NAME@, @--format fmt@, @--out file@.
      McExport McExportArgs
    deriving (Eq, Show, Generic)

-- | Arguments for @method export@.
data McExportArgs = McExportArgs
    { meaName :: Text
    -- ^ Method collection to export
    , meaFormat :: Text
    -- ^ Target format keyword (@--format@): simapro is the only method writer today
    , meaOut :: FilePath
    -- ^ Output file path (@--out@)
    }
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
    { serverPort :: Maybe Int -- Server port (--port); falls back to config [server].port, then 8080
    , serverLoadDbs :: Maybe [Text] -- Databases to load at startup (--load db1,db2)
    , serverDesktopMode :: Bool -- Desktop mode (--desktop): print port and minimize logging
    , serverStaticDir :: Maybe FilePath -- Static directory (--static-dir): override default web/dist
    , serverIdleTimeout :: Int -- Idle timeout in seconds (--idle-timeout, 0=disabled). Server exits after being idle.
    , serverTreeDepth :: Int -- Default max depth for /tree endpoint (--tree-depth, default 2)
    }
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
newtype LCIAOptions = LCIAOptions
    { lciaMethodId :: Text -- Method UUID (methods loaded on server)
    }
    deriving (Eq, Show, Generic)

{- | Options of the computed quality report. The collection may be left out
when exactly one is loaded - the server picks it, and says so when it can't.
-}
data ComputedQualityOptions = ComputedQualityOptions
    { cqoCollection :: Maybe Text -- --collection: method collection to score against
    , cqoLimit :: Maybe Int -- --limit: findings kept per check
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

{- | Whether the engine answers this command in CSV itself. The generic
@--format csv@ flattens whichever JSON array @--jsonpath@ names; these fetch a
file that is already a table, so the option has nothing left to name.

It lives beside 'Command' and matches every constructor on purpose: a command
added without deciding this would silently inherit the generic treatment, and
be refused for want of a @--jsonpath@ that does not apply to it.
-}
rendersOwnCsv :: Command -> Bool
rendersOwnCsv cmd = case cmd of
    QualityReport _ -> True
    ComputedQualityReport _ -> True
    Server _ -> False
    Activity _ -> False
    Flow _ _ -> False
    Inventory _ -> False
    SearchActivities _ -> False
    SearchFlows _ -> False
    Impacts _ _ -> False
    DebugMatrices _ _ -> False
    ExportMatrices _ -> False
    Database _ -> False
    Method _ -> False
    Methods -> False
    Synonyms -> False
    CompartmentMappings -> False
    Units -> False
    FlowMapping _ -> False
    Stop -> False
    Repl -> False
    Dump _ -> False

-- | Helper function to parse OutputFormat from string
parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat s = case map toLower s of
    "json" -> Just JSON
    "csv" -> Just CSV
    "table" -> Just Table
    "pretty" -> Just Pretty
    _ -> Nothing
  where
    toLower c = if isAsciiUpper c then toEnum (fromEnum c + 32) else c

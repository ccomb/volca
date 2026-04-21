{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Parser where

import CLI.Types
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import qualified Options.Applicative as OA
import Version (buildTarget, gitHash, gitTag, version)

{- | Main CLI parser combining global options and optional command
If no command is given, just load database and exit (useful for cache generation)
-}
cliParser :: Parser CLIConfig
cliParser = CLIConfig <$> globalOptionsParser <*> optional commandParser

-- | Global options parser (applied before commands)
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
    configFile <-
        optional $
            strOption
                ( long "config"
                    <> short 'c'
                    <> metavar "FILE"
                    <> help "TOML config file (required)"
                )

    dbName <-
        optional $
            ( T.pack
                <$> strOption
                    ( long "db"
                        <> metavar "NAME"
                        <> help "Database name to query (from config file)"
                    )
            )

    methodsDir <-
        optional $
            strOption
                ( long "methods"
                    <> metavar "PATH"
                    <> help "Directory containing ILCD method XML files for LCIA"
                )

    format <-
        optional $
            option
                outputFormatReader
                ( long "format"
                    <> metavar "FORMAT"
                    <> help "Output format: json|csv|table|pretty (default depends on command)"
                )

    jsonPath <-
        optional $
            ( T.pack
                <$> strOption
                    ( long "jsonpath"
                        <> metavar "PATH"
                        <> help "JSONPath for CSV extraction (required with --format csv). Examples: 'results', 'activity.exchanges'"
                    )
            )

    noCache <-
        switch
            ( long "no-cache"
                <> help "Disable caching for testing and development"
            )

    serverUrl <-
        optional $
            strOption
                ( long "url"
                    <> metavar "URL"
                    <> help "Server URL for HTTP client mode (or set VOLCA_URL env var)"
                )

    serverPassword <-
        optional $
            strOption
                ( long "password"
                    <> metavar "PASSWORD"
                    <> help "Password for authentication (or set VOLCA_PASSWORD env var)"
                )

    pure GlobalOptions{..}

-- | Output format reader for optparse-applicative
outputFormatReader :: ReadM OutputFormat
outputFormatReader = eitherReader $ \s ->
    case parseOutputFormat s of
        Just fmt -> Right fmt
        Nothing -> Left $ "Invalid format '" ++ s ++ "'. Valid formats: json, csv, table, pretty"

-- | Main command parser - all top-level for maximum discoverability
commandParser :: Parser Command
commandParser =
    subparser
        ( OA.command "server" (info (serverParser <**> helper) (progDesc "Start API server"))
            <> OA.command "activity" (info (activityParser <**> helper) (progDesc "Get basic activity information"))
            <> OA.command "inventory" (info (inventoryParser <**> helper) (progDesc "Get life cycle inventory for activity"))
            <> OA.command "flow" (info (flowParser <**> helper) (progDesc "Query flow information"))
            <> OA.command "activities" (info (searchActivitiesParser <**> helper) (progDesc "Search activities"))
            <> OA.command "flows" (info (searchFlowsParser <**> helper) (progDesc "Search flows"))
            <> OA.command "impacts" (info (impactsParser <**> helper) (progDesc "Compute impact assessment (LCIA) scores with a characterization method"))
            <> OA.command "debug-matrices" (info (debugMatricesParser <**> helper) (progDesc "Export targeted matrix slices for debugging"))
            <> OA.command "export-matrices" (info (exportMatricesParser <**> helper) (progDesc "Export matrices in universal format (Ecoinvent-compatible)"))
            <> OA.command "database" (info (databaseParser <**> helper) (progDesc "Manage databases (list, upload, delete)"))
            <> OA.command "method" (info (methodParser <**> helper) (progDesc "Manage method collections (list, upload, delete)"))
            <> OA.command "plugin" (info (pluginParser <**> helper) (progDesc "Manage plugins (list)"))
            <> OA.command "methods" (info (pure Methods <**> helper) (progDesc "List loaded methods (flattened)"))
            <> OA.command "synonyms" (info (pure Synonyms <**> helper) (progDesc "List synonym sources"))
            <> OA.command "compartment-mappings" (info (pure CompartmentMappings <**> helper) (progDesc "List compartment mappings"))
            <> OA.command "units" (info (pure Units <**> helper) (progDesc "List unit definitions"))
            <> OA.command "flow-mapping" (info (flowMappingParser <**> helper) (progDesc "Analyze flow mapping coverage between a method and database"))
            <> OA.command "stop" (info (pure Stop <**> helper) (progDesc "Stop running server (uses --config or --url to find it)"))
            <> OA.command "repl" (info (pure Repl <**> helper) (progDesc "Interactive REPL over HTTP (connects to running server)"))
        )
        <|> subparser
            ( OA.command "dump-openapi" (info (pure DumpOpenApi) (progDesc "Dump OpenAPI spec as JSON to stdout"))
                <> OA.command "dump-mcp-tools" (info (pure DumpMcpTools) (progDesc "Dump MCP tool definitions as JSON to stdout"))
                <> internal
            )

-- | Database command parser with optional subcommand (defaults to list)
databaseParser :: Parser Command
databaseParser =
    Database . maybe DbList id
        <$> optional
            ( subparser
                ( OA.command "list" (info (pure DbList) (progDesc "List databases"))
                    <> OA.command "upload" (info (DbUpload <$> uploadArgsParser) (progDesc "Upload a database from a local file"))
                    <> OA.command "delete" (info (DbDelete <$> deleteNameParser) (progDesc "Delete a database"))
                )
            )

-- | Method command parser with optional subcommand (defaults to list)
methodParser :: Parser Command
methodParser =
    Method . maybe McList id
        <$> optional
            ( subparser
                ( OA.command "list" (info (pure McList) (progDesc "List method collections"))
                    <> OA.command "upload" (info (McUpload <$> uploadArgsParser) (progDesc "Upload a method collection from a local file"))
                    <> OA.command "delete" (info (McDelete <$> deleteNameParser) (progDesc "Delete a method collection"))
                )
            )

-- | Plugin command parser with optional subcommand (defaults to list)
pluginParser :: Parser Command
pluginParser =
    Plugin . maybe PluginList id
        <$> optional
            ( subparser
                (OA.command "list" (info (pure PluginList) (progDesc "List registered plugins")))
            )

-- | Shared upload arguments parser (positional FILE, --name, --description)
uploadArgsParser :: Parser UploadArgs
uploadArgsParser = do
    uaFile <- argument str (metavar "FILE" <> help "Archive or data file to upload (ZIP, 7z, tar.gz, tar.xz, XML, CSV)")
    uaName <-
        T.pack
            <$> strOption
                ( long "name"
                    <> short 'n'
                    <> metavar "NAME"
                    <> help "Display name (required)"
                )
    uaDescription <-
        optional $
            T.pack
                <$> strOption
                    ( long "description"
                        <> metavar "TEXT"
                        <> help "Optional description"
                    )
    pure UploadArgs{..}

-- | Delete name parser (positional NAME)
deleteNameParser :: Parser Text
deleteNameParser = T.pack <$> argument str (metavar "NAME" <> help "Name of the resource to delete")

-- | Server command parser
serverParser :: Parser Command
serverParser = Server <$> serverOptionsParser

serverOptionsParser :: Parser ServerOptions
serverOptionsParser = do
    serverPort <-
        option
            auto
            ( long "port"
                <> short 'p'
                <> value 8080
                <> metavar "PORT"
                <> help "Server port (default: 8080)"
            )
    serverLoadDbs <-
        optional $
            option
                dbListReader
                ( long "load"
                    <> metavar "DB1,DB2,..."
                    <> help "Comma-separated list of databases to load at startup (overrides config load=true)"
                )
    serverDesktopMode <-
        switch
            ( long "desktop"
                <> help "Desktop mode: print VOLCA_PORT=N on startup for launcher integration"
            )
    serverStaticDir <-
        optional $
            strOption
                ( long "static-dir"
                    <> metavar "PATH"
                    <> help "Override default static file directory (default: web/dist)"
                )
    serverIdleTimeout <-
        option
            auto
            ( long "idle-timeout"
                <> value 0
                <> metavar "SECONDS"
                <> help "Shutdown after N seconds of inactivity (0=disabled, default: 0)"
            )
    serverTreeDepth <-
        option
            auto
            ( long "tree-depth"
                <> value 2
                <> metavar "DEPTH"
                <> help "Default max depth for the /tree endpoint (default: 2)"
            )
    pure ServerOptions{..}

-- | Reader for comma-separated list of database names
dbListReader :: ReadM [Text]
dbListReader = T.splitOn (T.pack ",") . T.pack <$> str

-- | Activity command parser (basic info only now)
activityParser :: Parser Command
activityParser = do
    uuid <- argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format)")
    pure $ Activity uuid

-- | Inventory command parser (now top-level)
inventoryParser :: Parser Command
inventoryParser = do
    uuid <- argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for inventory computation")
    pure $ Inventory uuid

-- | Flow command parser
flowParser :: Parser Command
flowParser = do
    flowId <- argument textReader (metavar "FLOW_ID" <> help "Flow ID")
    subCmd <- optional flowSubCommandParser
    pure $ Flow flowId subCmd

-- | Flow sub-command parser
flowSubCommandParser :: Parser FlowSubCommand
flowSubCommandParser =
    subparser
        (OA.command "activities" (info (pure FlowActivities) (progDesc "List activities using this flow")))

-- | Search activities parser (now top-level)
searchActivitiesParser :: Parser Command
searchActivitiesParser = do
    searchName <-
        optional $
            strOption
                ( long "name"
                    <> metavar "TERM"
                    <> help "Search by activity name"
                )

    searchGeo <-
        optional $
            strOption
                ( long "geo"
                    <> metavar "LOCATION"
                    <> help "Filter by geography (exact match)"
                )

    searchProduct <-
        optional $
            strOption
                ( long "product"
                    <> metavar "PRODUCT"
                    <> help "Filter by reference product"
                )

    searchLimit <-
        optional $
            option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Limit number of results (max 1000, default 50)"
                )

    searchOffset <-
        optional $
            option
                auto
                ( long "offset"
                    <> metavar "N"
                    <> help "Offset for pagination (default 0)"
                )

    pure $ SearchActivities SearchActivitiesOptions{..}

-- | Search flows parser (now top-level)
searchFlowsParser :: Parser Command
searchFlowsParser = do
    searchQuery <-
        optional $
            strOption
                ( long "query"
                    <> short 'q'
                    <> metavar "TERM"
                    <> help "Search term for flow names and synonyms"
                )

    searchLang <-
        optional $
            strOption
                ( long "lang"
                    <> metavar "LANG"
                    <> help "Language for synonym search"
                )

    searchFlowsLimit <-
        optional $
            option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Limit number of results"
                )

    searchFlowsOffset <-
        optional $
            option
                auto
                ( long "offset"
                    <> metavar "N"
                    <> help "Offset for pagination"
                )

    pure $ SearchFlows SearchFlowsOptions{..}

-- | Impacts (LCIA) command parser
impactsParser :: Parser Command
impactsParser = do
    uuid <- argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for impact assessment")
    options <- lciaOptionsParser
    pure $ Impacts uuid options

-- | LCIA options parser
lciaOptionsParser :: Parser LCIAOptions
lciaOptionsParser = do
    lciaMethodId <-
        T.pack
            <$> strOption
                ( long "method"
                    <> short 'm'
                    <> metavar "METHOD_UUID"
                    <> help "Method UUID (method must be loaded on the server)"
                )

    lciaOutput <-
        optional $
            strOption
                ( long "output"
                    <> short 'o'
                    <> metavar "FILE"
                    <> help "Export results to XML ILCD format"
                )

    lciaCSV <-
        optional $
            strOption
                ( long "csv"
                    <> metavar "FILE"
                    <> help "Export results to CSV format"
                )

    pure LCIAOptions{..}

-- | Debug matrices command parser
debugMatricesParser :: Parser Command
debugMatricesParser = do
    uuid <- argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for matrix debugging")
    options <- debugMatricesOptionsParser
    pure $ DebugMatrices uuid options

-- | Debug matrices options parser
debugMatricesOptionsParser :: Parser DebugMatricesOptions
debugMatricesOptionsParser = do
    debugOutput <-
        strOption
            ( long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Base filename for debug output (will generate _supply_chain.csv and _biosphere_matrix.csv)"
            )

    debugFlowFilter <-
        optional $
            strOption
                ( long "flow-filter"
                    <> metavar "FLOW"
                    <> help "Filter to specific biosphere flow (e.g., 'Sulphur dioxide')"
                )

    pure DebugMatricesOptions{..}

-- | Export matrices parser
exportMatricesParser :: Parser Command
exportMatricesParser = do
    outputDir <- argument str (metavar "OUTPUT_DIR" <> help "Output directory for matrix export")
    pure $ ExportMatrices outputDir

{- | Flow mapping command parser (renamed from 'mapping' to disambiguate
from compartment-mapping and similar resources).
-}
flowMappingParser :: Parser Command
flowMappingParser = do
    methodId <- argument textReader (metavar "METHOD_UUID" <> help "UUID of the characterization method")
    showMatched <-
        switch
            ( long "matched"
                <> help "List mapped CFs with their match strategy and DB flow"
            )
    showUnmatched <-
        switch
            ( long "unmatched"
                <> help "List method CFs that found no matching DB flow"
            )
    showUncharacterized <-
        switch
            ( long "uncharacterized"
                <> help "List DB biosphere flows that no CF matched"
            )
    pure $
        FlowMapping
            MappingOptions
                { mappingMethodId = methodId
                , mappingShowMatched = showMatched
                , mappingShowUnmatched = showUnmatched
                , mappingShowUncharacterized = showUncharacterized
                }

-- | Text reader for UUID arguments
textReader :: ReadM Text
textReader = T.pack <$> str

-- | Parser info for the complete CLI
versionOption :: Parser (a -> a)
versionOption =
    infoOption
        versionString
        (long "version" <> help "Show version information")
  where
    versionString =
        "volca "
            <> version
            <> " ("
            <> gitHash
            <> (if null gitTag then "" else ", " <> gitTag)
            <> ", "
            <> buildTarget
            <> ")"

cliParserInfo :: ParserInfo CLIConfig
cliParserInfo =
    info
        (cliParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "VoLCA - Life Cycle Assessment computation engine"
            <> header "volca - Command-line interface for VoLCA"
            <> footer
                "Examples:\n\
                \  volca --config volca.toml server --port 8080         # Start server\n\
                \  volca --config volca.toml --db ecoinvent activities --name electricity\n\
                \  volca --config volca.toml --db ecoinvent activity UUID\n\
                \  volca --config volca.toml --db ecoinvent inventory UUID\n\
                \  volca --config volca.toml --db ecoinvent impacts UUID --method METHOD_UUID\n\
                \  volca --config volca.toml database                   # List databases\n\
                \  volca --config volca.toml database upload mydb.7z --name \"My DB\"\n\
                \  volca --config volca.toml method upload pef.zip --name \"PEF\"\n\
                \  volca --config volca.toml repl                       # Interactive mode"
        )

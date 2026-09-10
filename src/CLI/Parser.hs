{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Parser where

import CLI.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import qualified Options.Applicative as OA
import Version (buildTarget, gitHash, gitTag, version)

-- ---------------------------------------------------------------------------
-- Option-builder helpers: collapses the @long/short/metavar/help@ boilerplate
-- around the four shapes (Text/Int option, Text/String positional arg).
-- ---------------------------------------------------------------------------

-- | @strOption@ with @long/metavar/help@, optionally a short alias.
strOpt :: String -> Maybe Char -> String -> String -> Parser String
strOpt l ms m h = strOption (long l <> maybe mempty short ms <> metavar m <> help h)

-- | @optional strOpt@ — most of the global / command parsers use this.
optStrOpt :: String -> Maybe Char -> String -> String -> Parser (Maybe String)
optStrOpt l ms m h = optional (strOpt l ms m h)

-- | Text variant: read as String then pack.
textOpt :: String -> Maybe Char -> String -> String -> Parser Text
textOpt l ms m h = T.pack <$> strOpt l ms m h

-- | @optional textOpt@.
optTextOpt :: String -> Maybe Char -> String -> String -> Parser (Maybe Text)
optTextOpt l ms m h = optional (textOpt l ms m h)

-- | @option auto@ for Int-shaped options (limit, offset, port, depth…).
intOpt :: String -> Maybe Char -> String -> String -> Parser Int
intOpt l ms m h = option auto (long l <> maybe mempty short ms <> metavar m <> help h)

-- | @optional intOpt@.
optIntOpt :: String -> Maybe Char -> String -> String -> Parser (Maybe Int)
optIntOpt l ms m h = optional (intOpt l ms m h)

-- | Positional @Text@ argument with metavar + help.
textArg :: String -> String -> Parser Text
textArg m h = T.pack <$> argument str (metavar m <> help h)

-- | Positional @String@ argument with metavar + help (for filenames).
strArg :: String -> String -> Parser String
strArg m h = argument str (metavar m <> help h)

{- | A subcommand that answers @--help@.

'OA.command' alone does not: a parser built without 'helper' does not know
the flag, so it rejects it and prints its usage to the error stream behind a
failing exit, where anything capturing a command's help sees nothing at all.
Declaring every subcommand through here is what keeps that from being one
more thing to remember.
-}
cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd name parser desc = OA.command name (info (parser <**> helper) (progDesc desc))

{- | Main CLI parser combining global options and optional command
If no command is given, just load database and exit (useful for cache generation)
-}
cliParser :: Parser CLIConfig
cliParser = CLIConfig <$> globalOptionsParser <*> optional commandParser

-- | Global options parser (applied before commands)
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
    configFile <- optStrOpt "config" (Just 'c') "FILE" "TOML config file (server and stop run on built-in defaults without it; other commands require it)"
    dbName <- optTextOpt "db" Nothing "NAME" "Database name to query (from config file)"
    methodsDir <- optStrOpt "methods" Nothing "PATH" "Directory containing ILCD method XML files for LCIA"
    format <-
        optional $
            option
                outputFormatReader
                (long "format" <> metavar "FORMAT" <> help "Output format: json|csv|table|pretty (default depends on command)")
    jsonPath <- optTextOpt "jsonpath" Nothing "PATH" "Field holding the array to flatten for --format csv; only needed when a response carries several. Examples: 'results', 'activity.exchanges'"
    noCache <- switch (long "no-cache" <> help "Disable caching for testing and development")
    serverUrl <- optStrOpt "url" Nothing "URL" "Server URL for HTTP client mode (or set VOLCA_URL env var)"
    serverPassword <- optStrOpt "password" Nothing "PASSWORD" "Password for authentication (or set VOLCA_PASSWORD env var)"
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
        ( cmd "server" serverParser "Start API server"
            <> cmd "activity" activityParser "Get basic activity information"
            <> cmd "inventory" inventoryParser "Get life cycle inventory for activity"
            <> cmd "flow" flowParser "Query flow information"
            <> cmd "activities" searchActivitiesParser "Search activities"
            <> cmd "flows" searchFlowsParser "Search flows"
            <> cmd "impacts" impactsParser "Compute impact assessment (LCIA) scores with a characterization method"
            <> cmd "debug-matrices" debugMatricesParser "Export targeted matrix slices for debugging"
            <> cmd "export-matrices" exportMatricesParser "Export matrices in universal format (Ecoinvent-compatible)"
            <> cmd "database" databaseParser "Manage databases (list, upload, delete)"
            <> cmd "method" methodParser "Manage method collections"
            <> cmd "methods" (pure Methods) "List loaded methods (flattened)"
            <> cmd "synonyms" (pure Synonyms) "List synonym sources"
            <> cmd "compartment-mappings" (pure CompartmentMappings) "List compartment mappings"
            <> cmd "units" (pure Units) "List unit definitions"
            <> cmd "flow-mapping" flowMappingParser "Analyze flow mapping coverage between a method and database"
            <> cmd "quality-report" qualityReportParser "Report what is malformed in a database (--format csv for a spreadsheet)"
            <> cmd "computed-quality-report" computedQualityReportParser "Report what a loaded database computes, judged against its own norms"
            <> cmd "stop" (pure Stop) "Stop running server (uses --config or --url to find it)"
            <> cmd "repl" (pure Repl) "Interactive REPL over HTTP (connects to running server)"
        )
        <|> subparser
            ( cmd "dump-openapi" (pure (Dump DumpOpenApi)) "Dump OpenAPI spec as JSON to stdout"
                <> cmd "dump-mcp-tools" (pure (Dump DumpMcpTools)) "Dump MCP tool definitions as JSON to stdout"
                <> cmd "dump-config-schema" (pure (Dump DumpConfigSchema)) "Dump the configuration file's key names as JSON to stdout"
                <> internal
            )

-- | Database command parser with optional subcommand (defaults to list)
databaseParser :: Parser Command
databaseParser =
    Database . fromMaybe DbList
        <$> optional
            ( subparser
                ( cmd "list" (pure DbList) "List databases"
                    <> cmd "load" (DbLoad <$> textArg "DB" "Name of the configured database to load") "Load a configured database into memory"
                    <> cmd "unload" (DbUnload <$> textArg "DB" "Name of the loaded database to unload") "Unload a database from memory"
                    <> cmd "upload" (DbUpload <$> uploadArgsParser) "Upload a database from a local file"
                    <> cmd "delete" (DbDelete <$> deleteNameParser) "Delete a database"
                    <> cmd "delete-activities" (DbDeleteActivities <$> deleteActivitiesArgsParser) "Delete the whole filtered set of activities from a loaded database"
                    <> cmd "copy" copyArgsParser "Copy a loaded database under a new name"
                    <> cmd "relink" (DbRelinkMapping <$> relinkArgsParser) "Relink a database to a dependency using a supplier alias CSV (source/target names, optional locations)"
                    <> cmd "export" (DbExport <$> exportArgsParser) "Export a loaded database to a file"
                    <> cmd "create-activities" (DbCreateActivities <$> writeArgsParser) "Write new activities into a database from a JSON file"
                    <> cmd "replace-activity" (DbReplaceActivity <$> replaceArgsParser) "Rewrite one activity of a database from a JSON file"
                    <> cmd "edit-exchanges" (DbEditExchanges <$> editArgsParser) "Change one activity's inventory from a JSON file, keeping the rest of the activity"
                )
            )

-- | Copy arguments parser (positional SRC and NEW_NAME)
copyArgsParser :: Parser DatabaseAction
copyArgsParser =
    DbCopy
        <$> textArg "SRC" "Name of the loaded database to copy"
        <*> textArg "NEW_NAME" "Name for the copy"

{- | Relink-with-mapping parser: positional DB, @--to@ dependency, @--mapping@
CSV path. Mirrors @db relink <db> --to <depDb> --mapping <csv>@.
-}
relinkArgsParser :: Parser DbRelinkArgs
relinkArgsParser =
    DbRelinkArgs
        <$> textArg "DB" "Name of the loaded database to relink"
        <*> textOpt "to" Nothing "DEP_DB" "Dependency database to link against"
        <*> strOpt "mapping" Nothing "CSV" "Path to the supplier alias CSV (source/target names, optional source/target locations)"

{- | Export parser: positional DB, @--format@ keyword, @--out@ file path.
Mirrors @db export <db> --format <fmt> --out <file>@.
-}
exportArgsParser :: Parser DbExportArgs
exportArgsParser =
    DbExportArgs
        <$> textArg "DB" "Name of the loaded database to export"
        <*> textOpt "format" Nothing "FMT" "Target format: simapro|ecospold1|ecospold2|ilcd|brightway"
        <*> strOpt "out" Nothing "FILE" "Output file path"

{- | Authoring parser: positional DB plus @--from@, the JSON file holding the
activities. The file is the same document the HTTP endpoint accepts
(@{"activities": [...]}@), so one description works over either transport.
-}
writeArgsParser :: Parser DbWriteArgs
writeArgsParser =
    DbWriteArgs
        <$> textArg "DB" "Name of the loaded database to write to"
        <*> strOpt "from" Nothing "FILE" "JSON file holding the activities to write"

{- | Replace parser: as 'writeArgsParser', plus the identity being rewritten.
The file holds one activity rather than a batch.
-}
replaceArgsParser :: Parser DbActivityArgs
replaceArgsParser =
    DbActivityArgs
        <$> textArg "DB" "Name of the loaded database holding the activity"
        <*> textOpt "process-id" Nothing "PID" "Identity of the activity to rewrite (activityUUID_productUUID)"
        <*> strOpt "from" Nothing "FILE" "JSON file holding the activity"

{- | Exchange-edit parser: the activity addressed, and the file stating what
changes about its inventory. This one reaches activities a rewrite cannot —
the ones a database file brought in, whose identity no description mints.
-}
editArgsParser :: Parser DbActivityArgs
editArgsParser =
    DbActivityArgs
        <$> textArg "DB" "Name of the loaded database holding the activity"
        <*> textOpt "process-id" Nothing "PID" "Identity of the activity to edit (activityUUID_productUUID)"
        <*> strOpt "from" Nothing "FILE" "JSON file holding the edits (remove, setAmounts, addInputs, addBiosphere, addWasteOutputs)"

{- | Method-export parser: positional collection name, @--format@ keyword,
@--out@ file path. Mirrors @method export <name> --format <fmt> --out <file>@.
-}
mcExportArgsParser :: Parser McExportArgs
mcExportArgsParser =
    McExportArgs
        <$> textArg "NAME" "Name of the loaded method collection to export"
        <*> textOpt "format" Nothing "FMT" "Target format: simapro|csv|openlca|ilcd"
        <*> strOpt "out" Nothing "FILE" "Output file path"

{- | Delete-by-selection parser: positional DB plus filter options. @--keep@ and
@--add@ may be repeated; they spare or add individual ProcessIds.
-}
deleteActivitiesArgsParser :: Parser DbDeleteArgs
deleteActivitiesArgsParser =
    DbDeleteArgs
        <$> textArg "DB" "Name of the loaded database to edit"
        <*> optTextOpt "name" Nothing "NAME" "Filter by activity name"
        <*> optTextOpt "location" Nothing "GEO" "Filter by location"
        <*> optTextOpt "product" Nothing "PRODUCT" "Filter by reference product name"
        <*> optTextOpt "class-system" Nothing "SYSTEM" "Classification system to filter on"
        <*> optTextOpt "class-value" Nothing "VALUE" "Classification value to filter on"
        <*> switch (long "exact" <> help "Exact (case-insensitive) name match instead of token-contains")
        <*> many (textOpt "keep" Nothing "PID" "Process id (activityUUID_productUUID) to spare from deletion (repeatable)")
        <*> many (textOpt "add" Nothing "PID" "Process id to add to deletion (repeatable)")
        <*> many (textOpt "id" Nothing "PID" "Delete exactly this process id (repeatable, excludes the filter options)")

-- | Method command parser with optional subcommand (defaults to list)
methodParser :: Parser Command
methodParser =
    Method . fromMaybe McList
        <$> optional
            ( subparser
                ( cmd "list" (pure McList) "List method collections"
                    <> cmd "upload" (McUpload <$> uploadArgsParser) "Upload a method collection from a local file"
                    <> cmd "delete" (McDelete <$> deleteNameParser) "Delete a method collection"
                    <> cmd "export" (McExport <$> mcExportArgsParser) "Export a loaded method collection to a file (SimaPro CSV, columnar CSV, openLCA JSON-LD, or ILCD method package)"
                )
            )

-- | Shared upload arguments parser (positional FILE, --name, --description)
uploadArgsParser :: Parser UploadArgs
uploadArgsParser = do
    uaFile <- strArg "FILE" "Archive or data file to upload (ZIP, 7z, tar.gz, tar.xz, XML, CSV)"
    uaName <- textOpt "name" (Just 'n') "NAME" "Display name (required)"
    uaDescription <- optTextOpt "description" Nothing "TEXT" "Optional description"
    pure UploadArgs{..}

-- | Delete name parser (positional NAME)
deleteNameParser :: Parser Text
deleteNameParser = textArg "NAME" "Name of the resource to delete"

-- | Server command parser
serverParser :: Parser Command
serverParser = Server <$> serverOptionsParser

serverOptionsParser :: Parser ServerOptions
serverOptionsParser = do
    serverPort <- optIntOpt "port" (Just 'p') "PORT" "Server port (0=OS-assigned, binds loopback only; overrides [server].port; default: 8080)"
    serverLoadDbs <-
        optional $
            option dbListReader (long "load" <> metavar "DB1,DB2,..." <> help "Comma-separated list of databases to load at startup (overrides config load=true)")
    serverDesktopMode <- switch (long "desktop" <> help "Desktop mode: print VOLCA_PORT=N on startup for launcher integration")
    serverStaticDir <- optStrOpt "static-dir" Nothing "PATH" "Override default static file directory (default: web/dist)"
    serverIdleTimeout <- option auto (long "idle-timeout" <> value 0 <> metavar "SECONDS" <> help "Shutdown after N seconds of inactivity (0=disabled, default: 0)")
    serverTreeDepth <- option auto (long "tree-depth" <> value 2 <> metavar "DEPTH" <> help "Default max depth for the /tree endpoint (default: 2)")
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
        (cmd "activities" (pure FlowActivities) "List activities using this flow")

-- | Search activities parser (now top-level)
searchActivitiesParser :: Parser Command
searchActivitiesParser = do
    searchName <- optTextOpt "name" Nothing "TERM" "Search by activity name"
    searchGeo <- optTextOpt "geo" Nothing "LOCATION" "Filter by geography: the location itself, or one the location table places inside it"
    searchProduct <- optTextOpt "product" Nothing "PRODUCT" "Filter by reference product"
    searchLimit <- optIntOpt "limit" Nothing "N" "Limit number of results (max 1000, default 50)"
    searchOffset <- optIntOpt "offset" Nothing "N" "Offset for pagination (default 0)"
    pure $ SearchActivities SearchActivitiesOptions{..}

-- | Search flows parser (now top-level)
searchFlowsParser :: Parser Command
searchFlowsParser = do
    searchQuery <- optTextOpt "query" (Just 'q') "TERM" "Search term for flow names and synonyms"
    searchLang <- optTextOpt "lang" Nothing "LANG" "Language for synonym search"
    searchFlowsLimit <- optIntOpt "limit" Nothing "N" "Limit number of results"
    searchFlowsOffset <- optIntOpt "offset" Nothing "N" "Offset for pagination"
    pure $ SearchFlows SearchFlowsOptions{..}

-- | Impacts (LCIA) command parser
impactsParser :: Parser Command
impactsParser =
    Impacts
        <$> argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for impact assessment")
        <*> lciaOptionsParser

-- | LCIA options parser
lciaOptionsParser :: Parser LCIAOptions
lciaOptionsParser = do
    lciaMethodId <- textOpt "method" (Just 'm') "METHOD_UUID" "Method UUID (method must be loaded on the server)"
    pure LCIAOptions{..}

-- | Debug matrices command parser
debugMatricesParser :: Parser Command
debugMatricesParser =
    DebugMatrices
        <$> argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for matrix debugging")
        <*> debugMatricesOptionsParser

-- | Debug matrices options parser
debugMatricesOptionsParser :: Parser DebugMatricesOptions
debugMatricesOptionsParser = do
    debugOutput <- strOpt "output" (Just 'o') "FILE" "Base filename for debug output (will generate _supply_chain.csv and _biosphere_matrix.csv)"
    debugFlowFilter <- optTextOpt "flow-filter" Nothing "FLOW" "Filter to specific biosphere flow (e.g., 'Sulphur dioxide')"
    pure DebugMatricesOptions{..}

-- | Export matrices parser
exportMatricesParser :: Parser Command
exportMatricesParser = ExportMatrices <$> strArg "OUTPUT_DIR" "Output directory for matrix export"

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

{- | Quality report parsers. Both read the database from the global @--db@,
like every other command that queries one, and both answer CSV under
@--format csv@ - the file the web UI downloads, byte for byte.
-}
qualityReportParser :: Parser Command
qualityReportParser =
    QualityReport <$> optIntOpt "limit" Nothing "N" "Keep at most N findings per check (worst first, default: all)"

computedQualityReportParser :: Parser Command
computedQualityReportParser = do
    cqoCollection <- optTextOpt "collection" Nothing "NAME" "Method collection to score against (default: the only one loaded)"
    cqoLimit <- optIntOpt "limit" Nothing "N" "Keep at most N findings per check (worst first, default: all)"
    pure $ ComputedQualityReport ComputedQualityOptions{..}

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

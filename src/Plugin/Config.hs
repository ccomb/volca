{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Plugin Configuration
--
-- Parses @[[plugin]]@ TOML sections and merges external plugin declarations
-- with the built-in registry. Supports:
--
-- * Adding external plugins (any type, with executable path)
-- * Overriding built-in plugin priorities (match by name)
-- * Disabling built-in plugins (@enabled = false@)
module Plugin.Config
    ( PluginConfig(..)
    , PluginType(..)
    , buildRegistry
    ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import TOML (DecodeTOML(..), Decoder, getField, getFieldOpt)

import Plugin.Types
import Plugin.Builtin (defaultRegistry)
import Plugin.Bridge (callPlugin)
import Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- | Plugin type tag (matches TOML @type@ field)
data PluginType
    = PTImporter
    | PTExporter
    | PTSearcher
    | PTMapper
    | PTTransform
    | PTValidator
    | PTAnalyzer
    | PTReporter
    deriving (Show, Eq)

instance DecodeTOML PluginType where
    tomlDecoder = do
        t <- tomlDecoder :: Decoder Text
        case T.toLower t of
            "importer"  -> pure PTImporter
            "exporter"  -> pure PTExporter
            "searcher"  -> pure PTSearcher
            "mapper"    -> pure PTMapper
            "transform" -> pure PTTransform
            "validator" -> pure PTValidator
            "analyzer"  -> pure PTAnalyzer
            "reporter"  -> pure PTReporter
            other       -> fail $ "Unknown plugin type: " <> T.unpack other

-- | A single @[[plugin]]@ TOML entry
data PluginConfig = PluginConfig
    { pcName     :: !Text
    , pcType     :: !PluginType
    , pcPath     :: !(Maybe FilePath)    -- Nothing for built-in overrides
    , pcEnabled  :: !Bool
    , pcPriority :: !(Maybe Int)
    , pcFormatId :: !(Maybe Text)        -- For exporter/reporter
    , pcPhase    :: !(Maybe Text)        -- For validator: "pre-compute" or "post-compute"
    , pcMimeType :: !(Maybe Text)        -- For reporter
    } deriving (Show, Eq)

instance DecodeTOML PluginConfig where
    tomlDecoder = do
        pcName     <- getField "name"
        pcType     <- getField "type"
        pcPath     <- getFieldOpt "path"
        pcEnabled  <- fromMaybe True <$> getFieldOpt "enabled"
        pcPriority <- getFieldOpt "priority"
        pcFormatId <- getFieldOpt "format-id"
        pcPhase    <- getFieldOpt "phase"
        pcMimeType <- getFieldOpt "mime-type"
        pure PluginConfig{..}

-- | Build a registry from built-in defaults + TOML plugin configs.
--
-- Processing order:
-- 1. Start with defaultRegistry
-- 2. For each TOML entry:
--    - If enabled=false and matches a built-in by name: remove it
--    - If matches a built-in by name: override priority (or other fields)
--    - If has a path: add as external plugin
-- 3. Sort priority-based lists
buildRegistry :: [PluginConfig] -> PluginRegistry
buildRegistry configs =
    let base = defaultRegistry
        -- Process each config entry
        registry = foldl applyConfig base configs
    in registry
        { prMappers    = sortOn mhPriority (prMappers registry)
        , prSearchers  = sortOn shPriority (prSearchers registry)
        , prTransforms = sortOn thPriority (prTransforms registry)
        }

applyConfig :: PluginRegistry -> PluginConfig -> PluginRegistry
applyConfig reg pc
    | not (pcEnabled pc) = disablePlugin reg pc
    | otherwise = case pcType pc of
        PTMapper   -> applyMapper reg pc
        PTReporter -> applyReporter reg pc
        PTExporter -> applyExporter reg pc
        PTAnalyzer -> applyAnalyzer reg pc
        PTTransform -> applyTransform reg pc
        PTValidator -> applyValidator reg pc
        PTImporter  -> applyImporter reg pc
        PTSearcher  -> applySearcher reg pc

-- | Remove a plugin by name
disablePlugin :: PluginRegistry -> PluginConfig -> PluginRegistry
disablePlugin reg pc = case pcType pc of
    PTMapper   -> reg { prMappers   = filter ((/= pcName pc) . mhName) (prMappers reg) }
    PTReporter -> reg { prReporters = M.filter ((/= pcName pc) . rhName) (prReporters reg) }
    PTExporter -> reg { prExporters = M.filter ((/= pcName pc) . ehName) (prExporters reg) }
    PTAnalyzer -> reg { prAnalyzers = M.filter ((/= pcName pc) . ahName) (prAnalyzers reg) }
    PTSearcher  -> reg { prSearchers  = filter ((/= pcName pc) . shName) (prSearchers reg) }
    PTTransform -> reg { prTransforms = filter ((/= pcName pc) . thName) (prTransforms reg) }
    PTValidator -> reg { prValidators = filter ((/= pcName pc) . vhName) (prValidators reg) }
    PTImporter  -> reg { prImporters  = filter ((/= pcName pc) . ihName) (prImporters reg) }

-- | Apply mapper config: override priority of existing, or add external
applyMapper :: PluginRegistry -> PluginConfig -> PluginRegistry
applyMapper reg pc =
    case pcPath pc of
        Nothing ->
            -- Override priority of existing built-in
            let mappers' = map (\m -> if mhName m == pcName pc
                    then m { mhPriority = fromMaybe (mhPriority m) (pcPriority pc) }
                    else m) (prMappers reg)
            in reg { prMappers = mappers' }
        Just path ->
            -- Add external mapper
            let priority = fromMaybe 50 (pcPriority pc)
                handle = makeExternalMapper (pcName pc) path priority
            in reg { prMappers = handle : prMappers reg }

-- | Apply reporter config: add external reporter
applyReporter :: PluginRegistry -> PluginConfig -> PluginRegistry
applyReporter reg pc = case pcPath pc of
    Nothing -> reg  -- Can't override built-in reporter fields (no meaningful overrides)
    Just path ->
        let fmtId = fromMaybe (pcName pc) (pcFormatId pc)
            mime  = fromMaybe "application/octet-stream" (pcMimeType pc)
            handle = makeExternalReporter (pcName pc) path fmtId mime
        in reg { prReporters = M.insert fmtId handle (prReporters reg) }

-- | Apply exporter config: add external exporter
applyExporter :: PluginRegistry -> PluginConfig -> PluginRegistry
applyExporter reg pc = case pcPath pc of
    Nothing -> reg
    Just path ->
        let fmtId = fromMaybe (pcName pc) (pcFormatId pc)
            handle = makeExternalExporter (pcName pc) path fmtId
        in reg { prExporters = M.insert fmtId handle (prExporters reg) }

-- | Apply analyzer config: add external analyzer
applyAnalyzer :: PluginRegistry -> PluginConfig -> PluginRegistry
applyAnalyzer reg pc = case pcPath pc of
    Nothing -> reg
    Just path ->
        let handle = makeExternalAnalyzer (pcName pc) path
        in reg { prAnalyzers = M.insert (pcName pc) handle (prAnalyzers reg) }

-- | Apply transform config: override priority of existing, or add external
applyTransform :: PluginRegistry -> PluginConfig -> PluginRegistry
applyTransform reg pc = case pcPath pc of
    Nothing ->
        let transforms' = map (\t -> if thName t == pcName pc
                then t { thPriority = fromMaybe (thPriority t) (pcPriority pc) }
                else t) (prTransforms reg)
        in reg { prTransforms = transforms' }
    Just path ->
        let priority = fromMaybe 50 (pcPriority pc)
            handle = makeExternalTransform (pcName pc) path priority
        in reg { prTransforms = handle : prTransforms reg }

-- | Apply validator config: add external validator
applyValidator :: PluginRegistry -> PluginConfig -> PluginRegistry
applyValidator reg pc = case pcPath pc of
    Nothing -> reg
    Just path ->
        let phase = case pcPhase pc of
                Just "post-compute" -> PostCompute
                _                   -> PreCompute
            handle = makeExternalValidator (pcName pc) path phase
        in reg { prValidators = handle : prValidators reg }

-- | Apply importer config: add external importer
applyImporter :: PluginRegistry -> PluginConfig -> PluginRegistry
applyImporter reg pc = case pcPath pc of
    Nothing -> reg
    Just path ->
        let handle = makeExternalImporter (pcName pc) path
        in reg { prImporters = handle : prImporters reg }

-- | Apply searcher config: override priority of existing, or add external
applySearcher :: PluginRegistry -> PluginConfig -> PluginRegistry
applySearcher reg pc = case pcPath pc of
    Nothing ->
        let searchers' = map (\s -> if shName s == pcName pc
                then s { shPriority = fromMaybe (shPriority s) (pcPriority pc) }
                else s) (prSearchers reg)
        in reg { prSearchers = searchers' }
    Just path ->
        let priority = fromMaybe 50 (pcPriority pc)
            handle = makeExternalSearcher (pcName pc) path priority
        in reg { prSearchers = handle : prSearchers reg }

-- ──────────────────────────────────────────────
-- External plugin handle constructors
-- ──────────────────────────────────────────────

makeExternalMapper :: Text -> FilePath -> Int -> MapperHandle
makeExternalMapper name path priority = MapperHandle
    { mhName     = name
    , mhBackend  = External path
    , mhPriority = priority
    , mhMatch    = \_ _ -> Nothing  -- External mappers use IO; Phase 2 will add IO mapper support
    }

makeExternalReporter :: Text -> FilePath -> Text -> Text -> ReportHandle
makeExternalReporter name path fmtId mime = ReportHandle
    { rhName     = name
    , rhBackend  = External path
    , rhFormatId = fmtId
    , rhMimeType = mime
    , rhReport   = \value -> do
        result <- callPlugin path (object ["action" .= ("report" :: Text), "data" .= value])
        case result of
            Right (String t) -> pure $ BSL8.pack (T.unpack t)
            Right other      -> pure $ encode other
            Left err         -> pure $ BSL8.pack $ "Plugin error: " <> T.unpack err
    }

makeExternalExporter :: Text -> FilePath -> Text -> ExportHandle
makeExternalExporter name path fmtId = ExportHandle
    { ehName     = name
    , ehBackend  = External path
    , ehFormatId = fmtId
    , ehExport   = \_ outPath -> do
        _ <- callPlugin path (object
            [ "action" .= ("export" :: Text)
            , "output" .= T.pack outPath
            ])
        pure ()
    }

makeExternalAnalyzer :: Text -> FilePath -> AnalyzeHandle
makeExternalAnalyzer name path = AnalyzeHandle
    { ahName    = name
    , ahBackend = External path
    , ahAnalyze = \_ -> do
        result <- callPlugin path (object ["action" .= ("analyze" :: Text)])
        case result of
            Right val -> pure val
            Left err  -> pure $ object ["error" .= err]
    }

makeExternalTransform :: Text -> FilePath -> Int -> TransformHandle
makeExternalTransform name path priority = TransformHandle
    { thName      = name
    , thBackend   = External path
    , thPriority  = priority
    , thTransform = \_ -> pure $ TransformResult
        { trDatabase = error "External transform not yet supported"
        , trLog = ["External transform plugins not yet supported"]
        }
    }

makeExternalValidator :: Text -> FilePath -> ValidationPhase -> ValidateHandle
makeExternalValidator name path phase = ValidateHandle
    { vhName     = name
    , vhBackend  = External path
    , vhPhase    = phase
    , vhValidate = \_ -> do
        result <- callPlugin path (object ["action" .= ("validate" :: Text)])
        case result of
            Right _ -> pure []
            Left err -> pure [ValidationIssue Warning "plugin" err Nothing]
    }

makeExternalImporter :: Text -> FilePath -> ImportHandle
makeExternalImporter name path = ImportHandle
    { ihName    = name
    , ihBackend = External path
    , ihCanRead = \_ -> pure False  -- External importers need explicit configuration
    , ihRead    = \_ _ -> pure $ Left "External importer not yet supported"
    }

makeExternalSearcher :: Text -> FilePath -> Int -> SearchHandle
makeExternalSearcher name path priority = SearchHandle
    { shName     = name
    , shBackend  = External path
    , shPriority = priority
    , shSearch   = \_ _ -> pure []  -- External searchers need IO bridge
    }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config (
    -- * Types
    Config (..),
    ServerConfig (..),
    DatabaseConfig (..),
    MethodConfig (..),
    ScoringSetConfig (..),
    RefDataConfig (..),
    HostingConfig (..),
    ClassificationPreset (..),
    ClassificationEntry (..),

    -- * Loading
    loadConfig,
    loadConfigFile,

    -- * VOLCA_DATA_DIR resolution
    redirectIntoDataDir,
    applyDataDir,

    -- * Default values
    defaultServerConfig,
    defaultConfig,

    -- * Utilities
    getDefaultDatabase,
    getLoadableDatabases,

    -- * Dependency resolution
    resolveLoadOrder,
) where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Upload (DatabaseFormat (..))
import GHC.Generics (Generic)
import Plugin.Config (PluginConfig)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)
import TOML (DecodeTOML (..), decodeFile, getArrayOf, getField, getFieldOpt, getFieldOptWith)

-- | A single classification filter entry (system + value)
data ClassificationEntry = ClassificationEntry
    { ceSystem :: !Text
    , ceValue :: !Text
    , ceMode :: !Text -- "exact" (default) or "contains"
    }
    deriving (Show, Eq, Generic)

-- | A named preset that pre-populates the classification filter
data ClassificationPreset = ClassificationPreset
    { cpName :: !Text
    , cpLabel :: !Text -- defaults to cpName if absent in TOML
    , cpDescription :: !(Maybe Text)
    , cpFilters :: ![ClassificationEntry]
    }
    deriving (Show, Eq, Generic)

-- | Main configuration type
data Config = Config
    { cfgServer :: !ServerConfig
    , cfgDatabases :: ![DatabaseConfig]
    , cfgMethods :: ![MethodConfig]
    , cfgFlowSynonyms :: ![RefDataConfig]
    , cfgCompartmentMappings :: ![RefDataConfig]
    , cfgUnits :: ![RefDataConfig]
    , cfgPlugins :: ![PluginConfig]
    , cfgHosting :: !(Maybe HostingConfig)
    , cfgGeographies :: !(Maybe FilePath) -- Path to geographies CSV (code,display_name,parents)
    , cfgClassificationPresets :: ![ClassificationPreset]
    }
    deriving (Show, Eq, Generic)

-- | Hosting configuration for managed VoLCA instances
data HostingConfig = HostingConfig
    { hcMaxUploads :: !Int -- Max database uploads (-1 = unlimited, 0 = disabled)
    , hcApiAccess :: !Bool -- Programmatic API access allowed
    , hcUpgradeUpload :: !Text -- Upgrade message when upload restricted
    , hcUpgradeApi :: !Text -- Upgrade message when API restricted
    , hcUpgradeVmSize :: !Text -- Upgrade message when memory is high
    }
    deriving (Show, Eq, Generic)

-- | Server configuration
data ServerConfig = ServerConfig
    { scPort :: !Int
    , scHost :: !Text
    , scPassword :: !(Maybe Text) -- Optional password for HTTP Basic Auth
    }
    deriving (Show, Eq, Generic)

-- | Database configuration
data DatabaseConfig = DatabaseConfig
    { dcName :: !Text -- Internal identifier (URL-safe slug)
    , dcDisplayName :: !Text -- Human-readable name for UI
    , dcPath :: !FilePath
    , dcDescription :: !(Maybe Text)
    , dcLoad :: !Bool -- Load at startup (renamed from dcActive)
    , dcDefault :: !Bool
    , dcDepends :: ![Text] -- Names of databases this one depends on (for cross-DB linking)
    , dcLocationAliases :: !(Map Text Text) -- Wrong location → correct location (e.g., "ENTSO" → "ENTSO-E")
    , dcFormat :: !(Maybe DatabaseFormat) -- Detected format (EcoSpold2, EcoSpold1, SimaProCSV)
    , dcIsUploaded :: !Bool -- True for uploaded databases (vs. configured in TOML)
    , dcDeletable :: !Bool -- May the UI delete this entry? Defaults to dcIsUploaded.
    }
    deriving (Show, Eq, Generic)

-- | Method configuration
data MethodConfig = MethodConfig
    { mcName :: !Text
    , mcPath :: !FilePath
    , mcActive :: !Bool
    , mcIsUploaded :: !Bool -- True for uploaded methods (vs. configured in TOML)
    , mcDescription :: !(Maybe Text) -- Optional description
    , mcFormat :: !(Maybe Text) -- Detected format ("SimaPro CSV", "ILCD", etc.)
    , mcScoringSets :: ![ScoringSetConfig] -- Formula-based scoring sets
    }
    deriving (Show, Eq, Generic)

-- | Configuration for a formula-based scoring set (parsed from TOML [[methods.scoring]])
data ScoringSetConfig = ScoringSetConfig
    { sscName :: !Text -- Display name
    , sscUnit :: !Text -- Display unit (e.g., "Pts")
    , sscVariables :: !(M.Map Text Text) -- var → impact category name
    , sscComputed :: !(M.Map Text Text) -- var → formula string
    , sscNormalization :: !(M.Map Text Double) -- var → normalization factor
    , sscWeighting :: !(M.Map Text Double) -- var → weight
    , sscScores :: !(M.Map Text Text) -- score name → formula
    , sscDisplayMultiplier :: !(Maybe Double) -- optional display multiplier (e.g., 1e6)
    }
    deriving (Show, Eq, Generic)

{- | Reusable config for reference data (flow synonyms, compartment mappings, units).
All three resource types share this shape.
-}
data RefDataConfig = RefDataConfig
    { rdName :: !Text
    , rdPath :: !FilePath
    , rdActive :: !Bool
    , rdIsUploaded :: !Bool
    , rdIsAuto :: !Bool -- True for auto-extracted synonym sets
    , rdDescription :: !(Maybe Text)
    }
    deriving (Show, Eq, Generic)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { scPort = 8080
        , scHost = "127.0.0.1"
        , scPassword = Nothing
        }

-- | Default config (empty databases)
defaultConfig :: Config
defaultConfig =
    Config
        { cfgServer = defaultServerConfig
        , cfgDatabases = []
        , cfgMethods = []
        , cfgFlowSynonyms = []
        , cfgCompartmentMappings = []
        , cfgUnits = []
        , cfgPlugins = []
        , cfgHosting = Nothing
        , cfgGeographies = Nothing
        , cfgClassificationPresets = []
        }

-- TOML Decoders

instance DecodeTOML Config where
    tomlDecoder = do
        cfgServer <- fromMaybe defaultServerConfig <$> getFieldOptWith tomlDecoder "server"
        cfgDatabases <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "databases"
        cfgMethods <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "methods"
        cfgFlowSynonyms <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "flow-synonyms"
        cfgCompartmentMappings <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "compartment-mappings"
        cfgUnits <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "units"
        cfgPlugins <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "plugin"
        cfgHosting <- getFieldOptWith tomlDecoder "hosting"
        cfgGeographies <- getFieldOpt "geographies"
        cfgClassificationPresets <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "classification-presets"
        pure Config{..}

instance DecodeTOML ServerConfig where
    tomlDecoder = do
        scPort <- fromMaybe 8080 <$> getFieldOpt "port"
        scHost <- fromMaybe "127.0.0.1" <$> getFieldOpt "host"
        scPassword <- getFieldOpt "password"
        pure ServerConfig{..}

instance DecodeTOML DatabaseConfig where
    tomlDecoder = do
        dcName <- getField "name"
        dcDisplayName <- fromMaybe dcName <$> getFieldOpt "displayName"
        dcPath <- getField "path"
        dcDescription <- getFieldOpt "description"
        dcLoad <- fromMaybe False <$> getFieldOpt "load"
        dcDefault <- fromMaybe False <$> getFieldOpt "default"
        dcDepends <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "depends"
        dcLocationAliases <- fromMaybe M.empty <$> getFieldOpt "locationAliases"
        let dcFormat = Nothing -- Format is detected at runtime, not stored in config
        let dcIsUploaded = False -- Databases from TOML are not uploaded
        dcDeletable <- fromMaybe dcIsUploaded <$> getFieldOpt "deletable"
        pure DatabaseConfig{..}

instance DecodeTOML MethodConfig where
    tomlDecoder = do
        mcName <- getField "name"
        mcPath <- getField "path"
        mcActive <- fromMaybe True <$> getFieldOpt "active"
        let mcIsUploaded = False -- Methods from TOML are not uploaded
        mcDescription <- getFieldOpt "description"
        let mcFormat = Nothing -- Detected later from file content
        mcScoringSets <- fromMaybe [] <$> getFieldOpt "scoring"
        pure MethodConfig{..}

instance DecodeTOML ScoringSetConfig where
    tomlDecoder = do
        sscName <- getField "name"
        sscUnit <- fromMaybe "Pt" <$> getFieldOpt "unit"
        sscVariables <- fromMaybe M.empty <$> getFieldOpt "variables"
        sscComputed <- fromMaybe M.empty <$> getFieldOpt "computed"
        sscNormalization <- fromMaybe M.empty <$> getFieldOpt "normalization"
        sscWeighting <- fromMaybe M.empty <$> getFieldOpt "weighting"
        sscScores <- fromMaybe M.empty <$> getFieldOpt "scores"
        sscDisplayMultiplier <- getFieldOpt "displayMultiplier"
        pure ScoringSetConfig{..}

instance DecodeTOML RefDataConfig where
    tomlDecoder = do
        rdPath <- getField "path"
        rdName <- fromMaybe (T.pack (takeFileName rdPath)) <$> getFieldOpt "name"
        rdActive <- fromMaybe True <$> getFieldOpt "active"
        let rdIsUploaded = False -- TOML entries are not uploaded
        let rdIsAuto = False
        rdDescription <- getFieldOpt "description"
        pure RefDataConfig{..}

instance DecodeTOML HostingConfig where
    tomlDecoder = do
        hcMaxUploads <- fromMaybe (-1) <$> getFieldOpt "max_uploads"
        hcApiAccess <- fromMaybe True <$> getFieldOpt "api_access"
        hcUpgradeUpload <- fromMaybe "" <$> getFieldOpt "upgrade_upload"
        hcUpgradeApi <- fromMaybe "" <$> getFieldOpt "upgrade_api"
        hcUpgradeVmSize <- fromMaybe "" <$> getFieldOpt "upgrade_vm_size"
        pure HostingConfig{..}

instance DecodeTOML ClassificationEntry where
    tomlDecoder = do
        ceSystem <- getField "system"
        ceValue <- getField "value"
        ceMode <- fromMaybe "exact" <$> getFieldOpt "mode"
        pure ClassificationEntry{..}

instance DecodeTOML ClassificationPreset where
    tomlDecoder = do
        cpName <- getField "name"
        cpLabel <- fromMaybe cpName <$> getFieldOpt "label"
        cpDescription <- getFieldOpt "description"
        cpFilters <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "filters"
        pure ClassificationPreset{..}

-- | Load configuration from a TOML file
loadConfigFile :: FilePath -> IO (Either Text Config)
loadConfigFile path = do
    exists <- doesFileExist path
    if not exists
        then pure $ Left $ "Config file not found: " <> T.pack path
        else do
            result <- decodeFile path
            case result of
                Right cfg -> pure $ Right cfg
                Left err -> pure $ Left $ "TOML parse error: " <> T.pack (show err)

{- | Load configuration, with validation. Honours VOLCA_DATA_DIR: when set,
any reference-data path beginning with "data/" (e.g. "data/flows.csv")
is rewritten to "$VOLCA_DATA_DIR/<rest>". This decouples the shipped
data bundle from the binary so they can be versioned independently.
Database and method paths (user content) are unaffected.
-}
loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
    result <- loadConfigFile path
    mDataDir <- lookupEnv "VOLCA_DATA_DIR"
    case result of
        Left err -> pure $ Left err
        Right cfg -> pure $ validateConfig (applyDataDir mDataDir cfg)

{- | Redirect a "data/<rest>" path to "$VOLCA_DATA_DIR/<rest>".
Returns the input unchanged when the env var is unset, or when the path
has no "data/" prefix. Pure: no IO. Accepts both Unix and Windows path
separators on the prefix so configs authored on either platform work.
The output always uses '/' — file APIs on Windows accept it, and it
keeps the path predictable for downstream string-based consumers.
-}
redirectIntoDataDir :: Maybe FilePath -> FilePath -> FilePath
redirectIntoDataDir Nothing p = p
redirectIntoDataDir (Just dataDir) p
    | "data/" `isPrefixOf` p = joinSlash dataDir (drop 5 p)
    | "data\\" `isPrefixOf` p = joinSlash dataDir (drop 5 p)
    | otherwise = p
  where
    joinSlash d r
        | null d = r
        | last d == '/' || last d == '\\' = d ++ r
        | otherwise = d ++ "/" ++ r

{- | Apply redirectIntoDataDir to every reference-data path on the Config.
Other fields (databases, methods, plugins) are user content that lives
outside the shipped data bundle and is left untouched.
-}
applyDataDir :: Maybe FilePath -> Config -> Config
applyDataDir mDataDir cfg =
    cfg
        { cfgGeographies = fmap resolve (cfgGeographies cfg)
        , cfgFlowSynonyms = map (mapPath resolve) (cfgFlowSynonyms cfg)
        , cfgCompartmentMappings = map (mapPath resolve) (cfgCompartmentMappings cfg)
        , cfgUnits = map (mapPath resolve) (cfgUnits cfg)
        }
  where
    resolve = redirectIntoDataDir mDataDir
    mapPath f r = r{rdPath = f (rdPath r)}

-- | Validate configuration
validateConfig :: Config -> Either Text Config
validateConfig cfg = do
    -- Check for duplicate database names
    let dbNames = map dcName (cfgDatabases cfg)
        duplicates = findDuplicates dbNames
    when (not $ null duplicates) $
        Left $
            "Duplicate database names: " <> T.intercalate ", " duplicates

    -- Check that at most one database is marked as default
    let defaultDbs = filter dcDefault (cfgDatabases cfg)
    when (length defaultDbs > 1) $
        Left $
            "Multiple databases marked as default: " <> T.intercalate ", " (map dcName defaultDbs)

    -- Validate dependency references exist
    let nameSet = S.fromList dbNames
    forM_ (cfgDatabases cfg) $ \db ->
        forM_ (dcDepends db) $ \dep ->
            when (not $ S.member dep nameSet) $
                Left $
                    "Database \"" <> dcName db <> "\" depends on unknown database: \"" <> dep <> "\""

    -- Validate no dependency cycles (resolveLoadOrder detects this)
    -- Run it with all databases marked as load=true to check the full graph
    let allLoaded = map (\db -> db{dcLoad = True}) (cfgDatabases cfg)
    case resolveLoadOrder allLoaded of
        Left err -> Left err
        Right _ -> Right cfg

-- | Find duplicates in a list
findDuplicates :: (Eq a) => [a] -> [a]
findDuplicates xs = go [] [] xs
  where
    go _ dups [] = dups
    go seen dups (x : rest)
        | x `elem` seen = go seen (if x `elem` dups then dups else x : dups) rest
        | otherwise = go (x : seen) dups rest

-- | Get the default database (or first loadable if none marked default)
getDefaultDatabase :: Config -> Maybe DatabaseConfig
getDefaultDatabase cfg =
    case filter dcDefault (getLoadableDatabases cfg) of
        (db : _) -> Just db
        [] -> case getLoadableDatabases cfg of
            (db : _) -> Just db
            [] -> Nothing

-- | Get all databases configured to load at startup
getLoadableDatabases :: Config -> [DatabaseConfig]
getLoadableDatabases = filter dcLoad . cfgDatabases

{- | Expand load=true transitively through depends, then topologically sort.
Returns Left on cycle, Right with ordered list of DB names to load.
-}
resolveLoadOrder :: [DatabaseConfig] -> Either Text [Text]
resolveLoadOrder configs =
    let configMap = M.fromList [(dcName c, c) | c <- configs]
        seeds = [dcName c | c <- configs, dcLoad c]
        expanded = expandTransitive configMap seeds S.empty
     in topoSort configMap (S.toList expanded)
  where
    -- Transitively expand seed set through depends
    expandTransitive _ [] visited = visited
    expandTransitive cfgMap (name : rest) visited
        | S.member name visited = expandTransitive cfgMap rest visited
        | otherwise = case M.lookup name cfgMap of
            Nothing -> expandTransitive cfgMap rest visited -- unknown, skip
            Just cfg -> expandTransitive cfgMap (dcDepends cfg ++ rest) (S.insert name visited)

    -- Kahn's algorithm: dependencies come first
    topoSort cfgMap names =
        let nameSet = S.fromList names
            depsOf n = maybe [] (filter (`S.member` nameSet) . dcDepends) (M.lookup n cfgMap)
            inDeg = M.fromList [(n, length (depsOf n)) | n <- names]
            queue = [n | (n, 0) <- M.toList inDeg]
            -- Reverse adjacency: dep → [nodes that depend on dep]
            revAdj = M.fromListWith (++) [(dep, [n]) | n <- names, dep <- depsOf n]
         in go revAdj inDeg queue [] (length names)

    go _ _ [] result expected
        | length result == expected = Right (reverse result)
        | otherwise = Left "Cycle detected in database dependencies"
    go revAdj degrees (n : q) result expected =
        let dependents = M.findWithDefault [] n revAdj
            degrees' = foldl (\d dep -> M.adjust (subtract 1) dep d) degrees dependents
            newReady = [dep | dep <- dependents, M.findWithDefault 1 dep degrees' == 0]
         in go revAdj degrees' (q ++ newReady) (n : result) expected

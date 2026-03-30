{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( -- * Types
      Config(..)
    , ServerConfig(..)
    , DatabaseConfig(..)
    , MethodConfig(..)
    , RefDataConfig(..)
    , HostingConfig(..)
      -- * Loading
    , loadConfig
    , loadConfigFile
      -- * Default values
    , defaultServerConfig
    , defaultConfig
      -- * Utilities
    , getDefaultDatabase
    , getLoadableDatabases
      -- * Dependency resolution
    , resolveLoadOrder
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import TOML (DecodeTOML(..), getField, getFieldOpt, getFieldOptWith, getArrayOf, decodeFile)
import Control.Monad (when, forM_)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import Database.Upload (DatabaseFormat(..))
import Plugin.Config (PluginConfig)

-- | Main configuration type
data Config = Config
    { cfgServer              :: !ServerConfig
    , cfgDatabases           :: ![DatabaseConfig]
    , cfgMethods             :: ![MethodConfig]
    , cfgFlowSynonyms        :: ![RefDataConfig]
    , cfgCompartmentMappings :: ![RefDataConfig]
    , cfgUnits               :: ![RefDataConfig]
    , cfgPlugins             :: ![PluginConfig]
    , cfgHosting             :: !(Maybe HostingConfig)
    , cfgGeographies         :: !(Maybe FilePath)    -- Path to geographies CSV (code,display_name,parents)
    } deriving (Show, Eq, Generic)

-- | Hosting configuration for managed VoLCA instances
data HostingConfig = HostingConfig
    { hcMaxUploads   :: !Int      -- Max database uploads (-1 = unlimited, 0 = disabled)
    , hcApiAccess    :: !Bool     -- Programmatic API access allowed
    , hcUpgradeUpload :: !Text    -- Upgrade message when upload restricted
    , hcUpgradeApi   :: !Text     -- Upgrade message when API restricted
    , hcUpgradeVmSize :: !Text    -- Upgrade message when memory is high
    } deriving (Show, Eq, Generic)

-- | Server configuration
data ServerConfig = ServerConfig
    { scPort     :: !Int
    , scHost     :: !Text
    , scPassword :: !(Maybe Text)  -- Optional password for HTTP Basic Auth
    } deriving (Show, Eq, Generic)

-- | Database configuration
data DatabaseConfig = DatabaseConfig
    { dcName        :: !Text           -- Internal identifier (URL-safe slug)
    , dcDisplayName :: !Text           -- Human-readable name for UI
    , dcPath        :: !FilePath
    , dcDescription :: !(Maybe Text)
    , dcLoad        :: !Bool           -- Load at startup (renamed from dcActive)
    , dcDefault     :: !Bool
    , dcDepends     :: ![Text]         -- Names of databases this one depends on (for cross-DB linking)
    , dcLocationAliases :: !(Map Text Text)  -- Wrong location → correct location (e.g., "ENTSO" → "ENTSO-E")
    , dcFormat      :: !(Maybe DatabaseFormat)  -- Detected format (EcoSpold2, EcoSpold1, SimaProCSV)
    , dcIsUploaded  :: !Bool           -- True for uploaded databases (vs. configured in TOML)
    } deriving (Show, Eq, Generic)

-- | Method configuration
data MethodConfig = MethodConfig
    { mcName        :: !Text
    , mcPath        :: !FilePath
    , mcActive      :: !Bool
    , mcIsUploaded  :: !Bool           -- True for uploaded methods (vs. configured in TOML)
    , mcDescription :: !(Maybe Text)   -- Optional description
    } deriving (Show, Eq, Generic)

-- | Reusable config for reference data (flow synonyms, compartment mappings, units).
-- All three resource types share this shape.
data RefDataConfig = RefDataConfig
    { rdName        :: !Text
    , rdPath        :: !FilePath
    , rdActive      :: !Bool
    , rdIsUploaded  :: !Bool
    , rdIsAuto      :: !Bool       -- True for auto-extracted synonym sets
    , rdDescription :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { scPort = 8080
    , scHost = "127.0.0.1"
    , scPassword = Nothing
    }

-- | Default config (empty databases)
defaultConfig :: Config
defaultConfig = Config
    { cfgServer = defaultServerConfig
    , cfgDatabases = []
    , cfgMethods = []
    , cfgFlowSynonyms = []
    , cfgCompartmentMappings = []
    , cfgUnits = []
    , cfgPlugins = []
    , cfgHosting = Nothing
    , cfgGeographies = Nothing
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
        let dcFormat = Nothing  -- Format is detected at runtime, not stored in config
        let dcIsUploaded = False  -- Databases from TOML are not uploaded
        pure DatabaseConfig{..}

instance DecodeTOML MethodConfig where
    tomlDecoder = do
        mcName <- getField "name"
        mcPath <- getField "path"
        mcActive <- fromMaybe True <$> getFieldOpt "active"
        let mcIsUploaded = False  -- Methods from TOML are not uploaded
        mcDescription <- getFieldOpt "description"
        pure MethodConfig{..}

instance DecodeTOML RefDataConfig where
    tomlDecoder = do
        rdPath <- getField "path"
        rdName <- fromMaybe (T.pack (takeFileName rdPath)) <$> getFieldOpt "name"
        rdActive <- fromMaybe True <$> getFieldOpt "active"
        let rdIsUploaded = False  -- TOML entries are not uploaded
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

-- | Load configuration, with validation
loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
    result <- loadConfigFile path
    case result of
        Left err -> pure $ Left err
        Right cfg -> pure $ validateConfig cfg

-- | Validate configuration
validateConfig :: Config -> Either Text Config
validateConfig cfg = do
    -- Check for duplicate database names
    let dbNames = map dcName (cfgDatabases cfg)
        duplicates = findDuplicates dbNames
    when (not $ null duplicates) $
        Left $ "Duplicate database names: " <> T.intercalate ", " duplicates

    -- Check that at most one database is marked as default
    let defaultDbs = filter dcDefault (cfgDatabases cfg)
    when (length defaultDbs > 1) $
        Left $ "Multiple databases marked as default: " <> T.intercalate ", " (map dcName defaultDbs)

    -- Validate dependency references exist
    let nameSet = S.fromList dbNames
    forM_ (cfgDatabases cfg) $ \db ->
        forM_ (dcDepends db) $ \dep ->
            when (not $ S.member dep nameSet) $
                Left $ "Database \"" <> dcName db <> "\" depends on unknown database: \"" <> dep <> "\""

    -- Validate no dependency cycles (resolveLoadOrder detects this)
    -- Run it with all databases marked as load=true to check the full graph
    let allLoaded = map (\db -> db { dcLoad = True }) (cfgDatabases cfg)
    case resolveLoadOrder allLoaded of
        Left err -> Left err
        Right _  -> Right cfg

-- | Find duplicates in a list
findDuplicates :: Eq a => [a] -> [a]
findDuplicates xs = go [] [] xs
  where
    go _ dups [] = dups
    go seen dups (x:rest)
        | x `elem` seen = go seen (if x `elem` dups then dups else x:dups) rest
        | otherwise = go (x:seen) dups rest

-- | Get the default database (or first loadable if none marked default)
getDefaultDatabase :: Config -> Maybe DatabaseConfig
getDefaultDatabase cfg =
    case filter dcDefault (getLoadableDatabases cfg) of
        (db:_) -> Just db
        []     -> case getLoadableDatabases cfg of
            (db:_) -> Just db
            []     -> Nothing

-- | Get all databases configured to load at startup
getLoadableDatabases :: Config -> [DatabaseConfig]
getLoadableDatabases = filter dcLoad . cfgDatabases

-- | Expand load=true transitively through depends, then topologically sort.
-- Returns Left on cycle, Right with ordered list of DB names to load.
resolveLoadOrder :: [DatabaseConfig] -> Either Text [Text]
resolveLoadOrder configs =
    let configMap = M.fromList [(dcName c, c) | c <- configs]
        seeds = [dcName c | c <- configs, dcLoad c]
        expanded = expandTransitive configMap seeds S.empty
    in topoSort configMap (S.toList expanded)
  where
    -- Transitively expand seed set through depends
    expandTransitive _ [] visited = visited
    expandTransitive cfgMap (name:rest) visited
        | S.member name visited = expandTransitive cfgMap rest visited
        | otherwise = case M.lookup name cfgMap of
            Nothing  -> expandTransitive cfgMap rest visited  -- unknown, skip
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
    go revAdj degrees (n:q) result expected =
        let dependents = M.findWithDefault [] n revAdj
            degrees' = foldl (\d dep -> M.adjust (subtract 1) dep d) degrees dependents
            newReady = [dep | dep <- dependents, M.findWithDefault 1 dep degrees' == 0]
        in go revAdj degrees' (q ++ newReady) (n : result) expected

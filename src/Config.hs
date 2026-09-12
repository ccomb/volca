{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config (
    -- * Types
    Config (..),
    ServerConfig (..),
    ServerName (..),
    DatabaseConfig (..),
    MethodConfig (..),
    ScoringSetConfig (..),
    MethodPatch (..),
    MethodPatchMatch (..),
    CFPatchOp (..),
    RefDataConfig (..),
    HostingConfig (..),
    ReadOnly (..),
    hostingReadOnly,
    messageOr,
    readOnlyRefusal,
    readOnlyRefusalFor,
    ClassificationPreset (..),
    ClassificationEntry (..),
    expandClassificationPreset,

    -- * Loading
    loadConfig,
    loadConfigFile,
    loadConfigOrDefault,
    validateConfig,

    -- * Listening address
    Listen (..),
    listenOn,
    freePortHost,
    clientHost,

    -- * Keys nothing reads
    KeySpec (..),
    configKeys,
    keyPaths,
    documentKeyPaths,
    unknownKeys,

    -- * VOLCA_DATA_DIR resolution
    redirectIntoDataDir,
    applyDataDir,

    -- * The tables the engine carries, and the data it reads
    RefDataSource (..),
    refDataDecoder,
    withBuiltins,
    builtinEntry,
    describeSource,
    DataVersion (..),
    dataBundleDir,
    readDataVersion,

    -- * Config-relative path resolution
    resolveConfigPaths,

    -- * Default values
    defaultConfig,

    -- * Dependency resolution
    resolveLoadOrder,
) where

import Builtin (BuiltinTable (..), DataVersion (..), builtinDataVersion, builtinName)
import Control.Monad (forM_, unless, when)
import Data.Indexing (repeated)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.Upload (DatabaseFormat (..))
import GHC.Generics (Generic)
import Progress (ProgressLevel (..), reportProgress)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, normalise, takeDirectory, takeFileName, (</>))
import TOML (DecodeTOML (..), Decoder, TOMLError, Table, Value (..), decode, decodeFile, getArrayOf, getField, getFieldOpt, getFieldOptWith, getFieldWith)
import Types (AllocationKey (..), GeographyPolicy (..), parseAllocationKey)

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

{- | Expand a preset name into the (system, value, exact) triples every query
surface filters with.

A name no configured preset carries is an error, never an empty filter list: a
preset narrows a query, so dropping it silently answers with the whole database
where the caller asked for a slice of it.
-}
expandClassificationPreset :: [ClassificationPreset] -> Maybe Text -> Either Text [(Text, Text, Bool)]
expandClassificationPreset _ Nothing = Right []
expandClassificationPreset presets (Just name) =
    case filter ((== name) . cpName) presets of
        p : _ -> Right [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
        [] -> Left (unknownPresetMessage presets name)

-- | Why a preset name did not resolve, naming what this instance does carry.
unknownPresetMessage :: [ClassificationPreset] -> Text -> Text
unknownPresetMessage presets name =
    "Unknown classification preset '" <> name <> "'. " <> case map cpName presets of
        [] -> "This instance has no classification presets configured."
        configured -> "Configured presets: " <> T.intercalate ", " configured

-- | Main configuration type
data Config = Config
    { cfgServer :: !ServerConfig
    , cfgDatabases :: ![DatabaseConfig]
    , cfgMethods :: ![MethodConfig]
    , cfgFlowSynonyms :: ![RefDataConfig]
    , cfgCompartmentMappings :: ![RefDataConfig]
    , cfgUnits :: ![RefDataConfig]
    , cfgEnergyDensities :: ![RefDataConfig]
    , cfgHosting :: !(Maybe HostingConfig)
    , cfgGeographies :: !(Maybe FilePath) -- Path to geographies CSV (code,display_name,parents)
    , cfgChemSynonyms :: !(Maybe FilePath) -- Path to chem_synonyms CSV (PubChem snapshot for the suggester)
    , cfgSubstanceEdges :: !(Maybe FilePath) -- Path to substance_edges CSV (typed flow-correspondence edges)
    , cfgClassificationPresets :: ![ClassificationPreset]
    }
    deriving (Show, Eq, Generic)

-- | Hosting configuration for managed VoLCA instances
data HostingConfig = HostingConfig
    { hcMaxUploads :: !Int -- Max database uploads (-1 = unlimited, 0 = disabled)
    , hcMaxUploadMb :: !Int -- Max upload size in MB (-1 = unlimited, 0 = disabled)
    , hcMaxLoadedUploads :: !Int -- Max uploaded databases held in memory at once (-1 = unlimited)
    , hcApiAccess :: !Bool -- Programmatic API access allowed
    , hcReadOnly :: !Bool -- Refuse every state-changing operation
    , hcReadOnlyMessage :: !Text -- Operator's own refusal sentence ("" = default)
    , hcUpgradeUpload :: !Text -- Upgrade message when upload restricted
    , hcUpgradeApi :: !Text -- Upgrade message when API restricted
    , hcUpgradeVmSize :: !Text -- Upgrade message when memory is high
    }
    deriving (Show, Eq, Generic)

{- | Whether this instance refuses every state-changing operation.

One instance serving many unrelated callers cannot let any of them load,
unload, upload, delete — or shut the server down — since each of those acts
on process-wide state that all the others share. A read-only instance still
answers every analysis question; it only declines to change anything.

Its own type rather than a bare 'Bool' because it travels through several
signatures alongside other flags, where a positional swap would silently
invert the guarantee.
-}
newtype ReadOnly = ReadOnly {isReadOnly :: Bool}
    deriving (Show, Eq)

-- | Read the read-only stance of an instance; unconfigured hosting is writable.
hostingReadOnly :: Maybe HostingConfig -> ReadOnly
hostingReadOnly = ReadOnly . maybe False hcReadOnly

{- | The default sentence every surface refuses with. Shared so REST, MCP and
the lifetime middleware cannot drift into three different explanations.
-}
readOnlyRefusal :: Text
readOnlyRefusal = "This engine is configured read-only: it answers queries but changes nothing."

{- | The operator's own words when they wrote any, the given default
otherwise. One rule for every refusal an operator may reword, so "unset"
cannot mean different things on different surfaces - and a message that is
only whitespace counts as unset rather than refusing with a blank sentence.
-}
messageOr :: Text -> Text -> Text
messageOr fallback msg
    | T.null (T.strip msg) = fallback
    | otherwise = msg

{- | The sentence a refusal actually carries: the operator's own words when
@read_only_message@ is configured, the default above otherwise.
-}
readOnlyRefusalFor :: Maybe HostingConfig -> Text
readOnlyRefusalFor = maybe readOnlyRefusal (messageOr readOnlyRefusal . hcReadOnlyMessage)

{- | How this instance introduces itself. A client may hold several VoLCA
servers at once; the name is what tells them apart.
-}
newtype ServerName = ServerName {unServerName :: Text}
    deriving (Show, Eq, Generic)

-- | Server configuration
data ServerConfig = ServerConfig
    { scPort :: !Int
    , scHost :: !Text
    , scPassword :: !(Maybe Text) -- Optional password for HTTP Basic Auth
    , scName :: !(Maybe ServerName)
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
    , dcGeographyPolicy :: !GeographyPolicy -- How aggressively to widen geography when linking suppliers
    , dcAllocation :: !AllocationKey -- How a multi-output block is divided; the same source under two keys is two databases
    , dcSource :: !(Maybe Text)
    {- ^ The database whose files this one reads, when it does not own them: a
    copy, or a source re-keyed under another allocation. Carried here rather
    than left in the upload metadata because a reader of a re-keyed database
    needs to know the shares were recomputed, and from what.
    -}
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
    , mcGlobalMethods :: ![Text]
    {- ^ Method names within this collection to score WITHOUT regionalization: a
    listed method's location-specific CFs are dropped, so each flow falls back to
    the method's own unlocated (global-default) CF instead of its per-region one.
    Use when the reference distribution a collection is compared against is itself
    unregionalized (e.g. a SimaPro EF distribution that flattened the spatial
    Land-use / AWARE factors to one global value); the per-country detail is lost.
    Requires the method to carry an unlocated default for those flows — one whose
    CFs are *all* region-tagged would be left with none. Empty = keep every
    method's native regionalization.
    -}
    , mcPatches :: ![MethodPatch]
    {- ^ Declarative adjustments applied to this collection's characterization
    factors right after parsing, before the collection is registered. The
    equivalent of a Brightway import "strategy", but data instead of code: a
    pure, idempotent transform of the freshly parsed factors, re-derived from
    the untouched source file on every reload rather than mutating a persisted
    store. Empty = the collection is used exactly as parsed.
    -}
    }
    deriving (Show, Eq, Generic)

{- | What a 'MethodPatch' does to a matched CF's value. A sum so a patch is
either a rescale or a hard override, never an ambiguous combination of both.
-}
data CFPatchOp
    = -- | Multiply the matched CF's value (TOML: @scale = 0.6@).
      ScaleBy !Double
    | -- | Replace the matched CF's value outright (TOML: @set-value = 0.0@).
      SetValueTo !Double
    deriving (Show, Eq, Generic)

{- | Selector picking which characterization factors a 'MethodPatch' touches.
Every present field must match (conjunction); a selector with no field set
is rejected by the decoder — a patch that would touch every CF in every
method is almost certainly a mistake, not an intent.
-}
data MethodPatchMatch = MethodPatchMatch
    { mpmCategory :: !(Maybe Text)
    {- ^ Impact category name (TOML: @category@), e.g. \"Resource use, fossils\".
    For a SimaPro CSV method export this is the per-category 'Method.methodName'
    (each \"Impact category\" section becomes its own 'Method' sharing the
    collection's overall methodology name, not this one).
    -}
    , mpmFlowName :: !(Maybe Text)
    -- ^ Exact flow name (TOML: @flow-name@), matched against 'Method.mcfFlowName'.
    , mpmFlowNamePrefix :: !(Maybe Text)
    -- ^ Flow name prefix (TOML: @flow-name-prefix@), matched with 'Data.Text.isPrefixOf'.
    , mpmCAS :: !(Maybe Text)
    {- ^ CAS registry number (TOML: @cas@), matched against 'Method.mcfCAS' with
    both sides canonicalized, so a selector matches whichever way the method's
    source spelled the padding. Only the registry number (the first segment) is
    ever zero-padded, so @0000050-00-0@ and @50-00-0@ are the same substance
    while @50-0-0@ is not one at all.
    -}
    , mpmSubcompartmentContains :: !(Maybe Text)
    {- ^ Case-insensitive substring of the subcompartment (TOML:
    @subcompartment-contains@), matched against 'Method.mcfCompartment'. A CF
    with no compartment never matches this field.
    -}
    }
    deriving (Show, Eq, Generic)

-- | One declarative adjustment to a method collection's characterization factors.
data MethodPatch = MethodPatch
    { mpDescription :: !(Maybe Text)
    -- ^ Free-text note on why this patch exists, surfaced in load logs.
    , mpMatch :: !MethodPatchMatch
    , mpOp :: !CFPatchOp
    }
    deriving (Show, Eq, Generic)

-- | Configuration for a formula-based scoring set (parsed from TOML [[methods.scoring]])
data ScoringSetConfig = ScoringSetConfig
    { sscName :: !Text -- Display name
    , sscUnit :: !Text -- Display unit (e.g., "Pts")
    , sscVariables :: !(M.Map Text Text) -- var → impact category name
    , sscComputed :: !(M.Map Text Text) -- var → formula string
    , sscLabels :: !(M.Map Text Text) -- var → display label (for computed vars)
    , sscNormalization :: !(M.Map Text Double) -- var → normalization factor
    , sscWeighting :: !(M.Map Text Double) -- var → weight
    , sscScores :: !(M.Map Text Text) -- score name → formula
    , sscDisplayMultiplier :: !(Maybe Double) -- optional display multiplier (e.g., 1e6)
    }
    deriving (Show, Eq, Generic)

{- | Where a reference table's bytes come from. A built-in table has no path:
it is in the binary, and a configuration names it only to replace it or to
switch it off.
-}
data RefDataSource = FromFile FilePath | BuiltIn BuiltinTable
    deriving (Show, Eq, Generic)

{- | Reusable config for reference data (flow synonyms, compartment mappings,
units, energy densities). All four resource types share this shape.
-}
data RefDataConfig = RefDataConfig
    { rdName :: !Text
    , rdSource :: !RefDataSource
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
        , scName = Nothing
        }

{- | The address the server listens on.

@--port 0@ asks the operating system for a free port. The only way to learn
which one it picked is to bind the socket first, and that path binds loopback,
so a configured host cannot be honoured there. Naming the case keeps it from
reading as an oversight where the server starts.
-}
data Listen
    = ListenOn Text Int
    | ListenOnFreeLoopbackPort
    deriving (Show, Eq)

{- | Resolve where to listen: @--port@ overrides @[server] port@, and
@[server] host@ decides the interface. Its default keeps a server reachable
from the machine it runs on and from nowhere else, which is what a password
left unset assumes.
-}
listenOn :: Maybe Int -> ServerConfig -> Listen
listenOn mPort sc = case fromMaybe (scPort sc) mPort of
    0 -> ListenOnFreeLoopbackPort
    port -> ListenOn (scHost sc) port

{- | What 'ListenOnFreeLoopbackPort' ends up bound to, so the address the
server announces is the one it is actually reachable at.
-}
freePortHost :: Text
freePortHost = "127.0.0.1"

{- | The host a client dials to reach a server told to listen on this one.

A listening address names interfaces to accept on; it is not a destination.
@0.0.0.0@ and warp\'s wildcards mean "every interface", which nothing can
connect to - on Windows @http:\/\/0.0.0.0@ fails outright, and @*@ is not
even a valid URL host. A server accepting on all of them accepts on this
machine too, so that is where a client on this machine goes.
-}
clientHost :: Text -> Text
clientHost host
    | host `elem` ["0.0.0.0", "::", "*", "*4", "!4", "*6", "!6"] = "localhost"
    | otherwise = host

-- | Default config (empty databases)

{- | What an engine runs on with no configuration at all: the tables it
carries, and nothing else. The geographies are among them: 'cfgGeographies'
at 'Nothing' means the built-in hierarchy, a path replaces it.
-}
defaultConfig :: Config
defaultConfig =
    withBuiltins
        Config
            { cfgServer = defaultServerConfig
            , cfgDatabases = []
            , cfgMethods = []
            , cfgFlowSynonyms = []
            , cfgCompartmentMappings = []
            , cfgUnits = []
            , cfgEnergyDensities = []
            , cfgHosting = Nothing
            , cfgGeographies = Nothing
            , cfgChemSynonyms = Nothing
            , cfgSubstanceEdges = Nothing
            , cfgClassificationPresets = []
            }

{- | The built-in table of every kind a configuration says nothing about. A
kind it lists is exactly what it lists: its own files, the built-in named to
keep it beside them or, named with @active = false@, to switch it off. The
tables of one kind are merged in name order, so adding the built-in to a
list the operator wrote would let it win over their file on every key they
share, silently; a kind they mention is theirs alone.
-}
withBuiltins :: Config -> Config
withBuiltins cfg =
    cfg
        { cfgFlowSynonyms = withTable BuiltinFlowSynonyms (cfgFlowSynonyms cfg)
        , cfgCompartmentMappings = withTable BuiltinCompartments (cfgCompartmentMappings cfg)
        , cfgUnits = withTable BuiltinUnits (cfgUnits cfg)
        , cfgEnergyDensities = withTable BuiltinEnergyDensities (cfgEnergyDensities cfg)
        }
  where
    withTable :: BuiltinTable -> [RefDataConfig] -> [RefDataConfig]
    withTable t [] = [builtinEntry t]
    withTable _ entries = entries

-- | A built-in table as the registry lists it: on, and not the operator's to delete.
builtinEntry :: BuiltinTable -> RefDataConfig
builtinEntry t =
    RefDataConfig
        { rdName = builtinName t
        , rdSource = BuiltIn t
        , rdActive = True
        , rdIsUploaded = False
        , rdIsAuto = False
        , rdDescription = Just "Built into this engine"
        }

-- | Where a table comes from, for a log line.
describeSource :: RefDataSource -> String
describeSource (FromFile path) = path
describeSource (BuiltIn _) = "built-in"

{- | Where the data bundle the engine reads sits: the directory of the flow
registry, the first @flow-synonyms@ entry that is not an upload, when that
registry is a file. The installers key a bundle on @data/<version>/flows.csv@,
and this is the same file seen from the engine. Nothing when the registry is
the built-in one, which is no bundle on disk.
-}
dataBundleDir :: Config -> Maybe FilePath
dataBundleDir cfg = case rdSource <$> find (not . rdIsUploaded) (cfgFlowSynonyms cfg) of
    Just (FromFile path) -> Just (takeDirectory path)
    Just (BuiltIn _) -> Nothing
    Nothing -> Nothing

{- | The version of the reference data the engine reads: the built-in one when
the flow registry is built in, else the @VERSION@ file beside the registry's
file. Nothing when that file is missing: an engine reading a registry of its
operator's own has no bundle version to report, and saying so is the honest
answer. Two engines answering different numbers under one name is what this
is for, so it follows what is read, not what was compiled.
-}
readDataVersion :: Config -> IO (Maybe DataVersion)
readDataVersion cfg = case dataBundleDir cfg of
    Nothing -> pure (Just builtinDataVersion)
    Just dir -> do
        let file = dir </> "VERSION"
        present <- doesFileExist file
        if present
            then Just . DataVersion . T.strip <$> TIO.readFile file
            else pure Nothing

-- TOML Decoders

instance DecodeTOML Config where
    tomlDecoder = do
        cfgServer <- fromMaybe defaultServerConfig <$> getFieldOptWith tomlDecoder "server"
        cfgDatabases <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "databases"
        cfgMethods <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "methods"
        cfgFlowSynonyms <- fromMaybe [] <$> getFieldOptWith (getArrayOf (refDataDecoder BuiltinFlowSynonyms)) "flow-synonyms"
        cfgCompartmentMappings <- fromMaybe [] <$> getFieldOptWith (getArrayOf (refDataDecoder BuiltinCompartments)) "compartment-mappings"
        cfgUnits <- fromMaybe [] <$> getFieldOptWith (getArrayOf (refDataDecoder BuiltinUnits)) "units"
        cfgEnergyDensities <- fromMaybe [] <$> getFieldOptWith (getArrayOf (refDataDecoder BuiltinEnergyDensities)) "energy-densities"
        cfgHosting <- getFieldOptWith tomlDecoder "hosting"
        cfgGeographies <- getFieldOpt "geographies"
        cfgChemSynonyms <- getFieldOpt "chem-synonyms"
        cfgSubstanceEdges <- getFieldOpt "substance-edges"
        cfgClassificationPresets <- fromMaybe [] <$> getFieldOptWith (getArrayOf tomlDecoder) "classification-presets"
        pure Config{..}

instance DecodeTOML ServerConfig where
    tomlDecoder = do
        scPort <- fromMaybe 8080 <$> getFieldOpt "port"
        scHost <- fromMaybe "127.0.0.1" <$> getFieldOpt "host"
        scPassword <- getFieldOpt "password"
        scName <- fmap ServerName <$> getFieldOpt "name"
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
        dcGeographyPolicy <- fromMaybe GeoGlobal <$> getFieldOptWith geographyPolicyDecoder "geography_policy"
        dcAllocation <- fromMaybe Declared <$> getFieldOptWith allocationKeyDecoder "allocation"
        let dcSource = Nothing -- A configured database owns the files it names
        pure DatabaseConfig{..}

{- | @allocation@ on a database entry: how its multi-output blocks are divided.

Naming a property here loads the source under that key instead of the one it
declares. To have both, configure the same path twice under two names: the key
decides the inventory of every process the load produces, so one database
carries one key.
-}
allocationKeyDecoder :: Decoder AllocationKey
allocationKeyDecoder = do
    raw <- tomlDecoder :: Decoder Text
    either (fail . T.unpack . ("allocation: " <>)) pure (parseAllocationKey raw)

geographyPolicyDecoder :: Decoder GeographyPolicy
geographyPolicyDecoder = do
    raw <- tomlDecoder :: Decoder Text
    case T.toLower raw of
        "exact" -> pure GeoExact
        "parent" -> pure GeoParent
        "global" -> pure GeoGlobal
        other -> fail $ "geography_policy: expected one of exact|parent|global, got: " <> T.unpack other

instance DecodeTOML MethodConfig where
    tomlDecoder = do
        mcName <- getField "name"
        mcPath <- getField "path"
        mcActive <- fromMaybe True <$> getFieldOpt "active"
        let mcIsUploaded = False -- Methods from TOML are not uploaded
        mcDescription <- getFieldOpt "description"
        let mcFormat = Nothing -- Detected later from file content
        mcScoringSets <- fromMaybe [] <$> getFieldOpt "scoring"
        mcGlobalMethods <- fromMaybe [] <$> getFieldOpt "global-methods"
        mcPatches <- fromMaybe [] <$> getFieldOpt "patches"
        pure MethodConfig{..}

instance DecodeTOML MethodPatchMatch where
    tomlDecoder = do
        mpmCategory <- getFieldOpt "category"
        mpmFlowName <- getFieldOpt "flow-name"
        mpmFlowNamePrefix <- getFieldOpt "flow-name-prefix"
        mpmCAS <- getFieldOpt "cas"
        mpmSubcompartmentContains <- getFieldOpt "subcompartment-contains"
        when (all isNothing [mpmCategory, mpmFlowName, mpmFlowNamePrefix, mpmCAS, mpmSubcompartmentContains]) $
            fail "match: at least one selector field must be set (a patch matching every CF is almost certainly a mistake)"
        pure MethodPatchMatch{..}

instance DecodeTOML MethodPatch where
    tomlDecoder = do
        mpDescription <- getFieldOpt "description"
        mpMatch <- getFieldWith tomlDecoder "match"
        mScale <- getFieldOpt "scale"
        mSetValue <- getFieldOpt "set-value"
        mpOp <- case (mScale, mSetValue) of
            (Just s, Nothing) -> pure (ScaleBy s)
            (Nothing, Just v) -> pure (SetValueTo v)
            (Nothing, Nothing) -> fail "patch: exactly one of 'scale' or 'set-value' is required, neither was set"
            (Just _, Just _) -> fail "patch: exactly one of 'scale' or 'set-value' is required, both were set"
        pure MethodPatch{..}

instance DecodeTOML ScoringSetConfig where
    tomlDecoder = do
        sscName <- getField "name"
        sscUnit <- fromMaybe "Pt" <$> getFieldOpt "unit"
        sscVariables <- fromMaybe M.empty <$> getFieldOpt "variables"
        sscComputed <- fromMaybe M.empty <$> getFieldOpt "computed"
        sscLabels <- fromMaybe M.empty <$> getFieldOpt "labels"
        sscNormalization <- fromMaybe M.empty <$> getFieldOpt "normalization"
        sscWeighting <- fromMaybe M.empty <$> getFieldOpt "weighting"
        sscScores <- fromMaybe M.empty <$> getFieldOpt "scores"
        sscDisplayMultiplier <- getFieldOpt "displayMultiplier"
        let orphanLabels = M.keysSet sscLabels S.\\ (M.keysSet sscComputed <> M.keysSet sscVariables)
        unless (S.null orphanLabels) $
            fail $
                "labels: unknown scoring variable(s): "
                    <> T.unpack (T.intercalate ", " (S.toList orphanLabels))
        pure ScoringSetConfig{..}

{- | One array of tables, knowing which built-in it may name: an entry with a
path is a file, named after it unless told otherwise; an entry with no path
names the built-in of this very array, which is how a file keeps it beside
its own tables, or switches it off with @active = false@, without pointing
at anything.
-}
refDataDecoder :: BuiltinTable -> Decoder RefDataConfig
refDataDecoder builtin = do
    mPath <- getFieldOpt "path"
    mName <- getFieldOpt "name"
    rdActive <- fromMaybe True <$> getFieldOpt "active"
    rdDescription <- getFieldOpt "description"
    (rdName, rdSource) <- case (mPath, mName) of
        (Just path, _) -> pure (fromMaybe (T.pack (takeFileName path)) mName, FromFile path)
        (Nothing, Just name)
            | name == builtinName builtin -> pure (name, BuiltIn builtin)
            | otherwise ->
                fail $
                    "an entry without a path names the built-in table, \""
                        <> T.unpack (builtinName builtin)
                        <> "\"; got \""
                        <> T.unpack name
                        <> "\""
        (Nothing, Nothing) -> fail "an entry needs a path, or the name of the built-in table (to keep it, or to switch it off with active = false)"
    let rdIsUploaded = False -- TOML entries are not uploaded
        rdIsAuto = False
    pure RefDataConfig{..}

instance DecodeTOML HostingConfig where
    tomlDecoder = do
        hcMaxUploads <- fromMaybe (-1) <$> getFieldOpt "max_uploads"
        hcMaxUploadMb <- fromMaybe 100 <$> getFieldOpt "max_upload_mb"
        hcMaxLoadedUploads <- fromMaybe (-1) <$> getFieldOpt "max_loaded_uploads"
        hcApiAccess <- fromMaybe True <$> getFieldOpt "api_access"
        hcReadOnly <- fromMaybe False <$> getFieldOpt "read_only"
        hcReadOnlyMessage <- fromMaybe "" <$> getFieldOpt "read_only_message"
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
            -- The keys first, and from the text rather than from the decoded
            -- configuration: a document that parses can say which of its keys
            -- nothing reads whether or not it then decodes, and an operator
            -- fixing one mistake per restart pays minutes a time on a machine
            -- that is already running.
            text <- TIO.readFile path
            mapM_ (reportProgress Warning . T.unpack) (unreadKeyWarnings path text)
            -- decodeFile rather than the text just read: it hands the parser
            -- the file name, so a syntax error says which file, which matters
            -- most where the engine reads a document assembled from two.
            result <- decodeFile path
            pure $ case result of
                Right cfg -> Right cfg
                Left err -> Left $ "TOML parse error: " <> T.pack (show err)

{- | One line per key of the file no decoder reads.

A warning, not a refusal: a key from a later release should not stop an older
engine. But saying nothing is worse than either, because the ways a key goes
unread are all invisible - written under the wrong section header, spelled
with the wrong separator, or named the way an older release named it.

Nothing to say about a document that does not parse: the decode that follows
reports that, and better.
-}
unreadKeyWarnings :: FilePath -> Text -> [Text]
unreadKeyWarnings path text = map complain (either (const []) (unknownKeys configKeys) parsed)
  where
    parsed = decode text :: Either TOMLError Table
    complain key = T.pack path <> ": nothing reads " <> key <> maybe "" hint (lookup key renamedKeys)
    hint replacement = " (renamed to " <> replacement <> ")"

{- | Keys an earlier release read, and what replaced them, so a configuration
written against one says why it stopped working rather than only that a key is
unread. Keyed by the path 'unknownKeys' reports, with any array index dropped.
-}
renamedKeys :: [(Text, Text)]
renamedKeys = [("databases[].active", "load")]

{- | What one part of the configuration accepts: the keys read there, each
with what is accepted under it. A key read for its value alone accepts an
empty map.

One map rather than a list of keys beside a list of sections, so a key cannot
be declared as both and then be silently resolved one way.
-}
data KeySpec
    = Accepts (Map Text KeySpec)
    | {- | The caller names the keys, so none of them can be unknown: a scoring
      set's variables, a database's location aliases.
      -}
      AcceptsAnything
    deriving (Show, Eq)

{- | Every key the decoders in this module read, in the shape a file has them.

Kept beside the decoders because it is only worth having while it matches
them: a decoder gaining a field and this not is a key reported unread that is
read perfectly well.
-}
configKeys :: KeySpec
configKeys =
    keys
        [ ("geographies", value)
        , ("chem-synonyms", value)
        , ("substance-edges", value)
        , ("server", keys (map plain ["port", "host", "password", "name"]))
        , ("hosting", keys (map plain hostingKeys))
        ,
            ( "databases"
            , keys $
                map plain ["name", "displayName", "path", "description", "load", "default", "depends", "deletable", "geography_policy", "allocation"]
                    <> [("locationAliases", AcceptsAnything)]
            )
        ,
            ( "methods"
            , keys $
                map plain ["name", "path", "active", "description", "global-methods"]
                    <> [("scoring", scoringSet), ("patches", patch)]
            )
        , ("flow-synonyms", refData)
        , ("compartment-mappings", refData)
        , ("units", refData)
        , ("energy-densities", refData)
        ,
            ( "classification-presets"
            , keys $
                map plain ["name", "label", "description"]
                    <> [("filters", keys (map plain ["system", "value", "mode"]))]
            )
        ]
  where
    keys = Accepts . M.fromList
    value = Accepts M.empty
    plain k = (k, value)
    hostingKeys =
        [ "max_uploads"
        , "max_upload_mb"
        , "max_loaded_uploads"
        , "api_access"
        , "read_only"
        , "read_only_message"
        , "upgrade_upload"
        , "upgrade_api"
        , "upgrade_vm_size"
        ]
    refData = keys (map plain ["path", "name", "active", "description"])
    scoringSet =
        keys $
            map plain ["name", "unit", "displayMultiplier"]
                <> [(k, AcceptsAnything) | k <- ["variables", "computed", "labels", "normalization", "weighting", "scores"]]
    patch =
        keys $
            map plain ["description", "scale", "set-value"]
                <> [("match", keys (map plain ["category", "flow-name", "flow-name-prefix", "cas", "subcompartment-contains"]))]

{- | Every key a shape names by name, dotted: what a document has to spell out
to exercise the whole of it.

Not quite what 'unknownKeys' reports, which marks an array of tables @name[]@
because it is describing one document; a shape has no arity to mark. And not
every key a document may carry: an 'AcceptsAnything' section is named, but the
keys under it are the file author\'s and cannot be enumerated.

Sorted, so a consumer can diff two of these.
-}
keyPaths :: KeySpec -> [Text]
keyPaths = S.toAscList . S.fromList . go []
  where
    go _ AcceptsAnything = []
    go here (Accepts known) =
        concat [T.intercalate "." (reverse (k : here)) : go (k : here) nested | (k, nested) <- M.toList known]

{- | The dotted path of every key of a parsed document that the given shape
does not name, an array written @name[]@ because the complaint is about the
key rather than about one of its occurrences. Sorted and without repeats, so
a file listing twenty databases with the same stale key says it once.
-}
unknownKeys :: KeySpec -> Table -> [Text]
unknownKeys spec = S.toAscList . S.fromList . walk [] spec
  where
    walk _ AcceptsAnything _ = []
    walk here (Accepts known) table = concatMap (judge here known) (M.toList table)

    judge here known (key, val) = case M.lookup key known of
        Nothing -> [T.intercalate "." (here <> [key])]
        Just nested -> let (segment, tables) = sectionUnder key val in concatMap (walk (here <> [segment]) nested) tables

{- | The dotted path of every key a parsed document names, written the way
'unknownKeys' and 'keyPaths' write them. What a document actually exercises,
against what a shape allows.
-}
documentKeyPaths :: Table -> [Text]
documentKeyPaths = S.toAscList . S.fromList . walk []
  where
    walk here table = concatMap (describe here) (M.toList table)
    describe here (key, val) =
        let (segment, tables) = sectionUnder key val
         in T.intercalate "." (here <> [segment]) : concatMap (walk (here <> [segment])) tables

{- | How a section is written in a path, and the tables it holds. A section is
a table or an array of tables wherever a decoder reads one; a value of any
other shape holds nothing to walk into.
-}
sectionUnder :: Text -> Value -> (Text, [Table])
sectionUnder key = \case
    Table t -> (key, [t])
    Array vs -> (key <> "[]", concatMap (snd . sectionUnder key) vs)
    String{} -> (key, [])
    Integer{} -> (key, [])
    Float{} -> (key, [])
    Boolean{} -> (key, [])
    OffsetDateTime{} -> (key, [])
    LocalDateTime{} -> (key, [])
    LocalDate{} -> (key, [])
    LocalTime{} -> (key, [])

{- | Load configuration, with validation. Honours VOLCA_DATA_DIR: when set,
any reference-data path beginning with "data/" (e.g. "data/flows.csv")
is rewritten to "$VOLCA_DATA_DIR/<rest>". This decouples the shipped
data bundle from the binary so they can be versioned independently.
Database and method paths (user content) are unaffected.
-}
loadConfig :: FilePath -> IO (Either Text Config)
loadConfig = loadConfigOrDefault . Just

{- | Resolve the effective configuration: parse the file when a path is given,
otherwise fall back to 'defaultConfig'. Both paths honour VOLCA_DATA_DIR,
resolve relative paths against the file ('resolveConfigPaths') and run
'validateConfig' by construction — an explicit path that does not exist
still fails loudly, while no path at all means "all defaults, no databases".
-}
loadConfigOrDefault :: Maybe FilePath -> IO (Either Text Config)
loadConfigOrDefault mPath = do
    raw <- maybe (pure (Right defaultConfig)) loadConfigFile mPath
    mDataDir <- lookupEnv "VOLCA_DATA_DIR"
    pure $ raw >>= validateConfig . resolveConfigPaths mPath . applyDataDir mDataDir . withBuiltins

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
        | any (`isSuffixOf` d) ["/", "\\"] = d ++ r
        | otherwise = d ++ "/" ++ r

{- | What a path in a configuration file points at.

'applyDataDir' is the one rewrite that treats them differently:
@VOLCA_DATA_DIR@ names where the shipped data bundle was installed, so it
redirects reference data and leaves the operator's own files where they are.
'resolveConfigPaths' resolves every kind alike. Which field is which is
recorded in 'overPaths' and nowhere else.
-}
data PathKind
    = ReferenceDataPath
    | UserContentPath
    deriving (Show, Eq)

{- | Rewrite every path a configuration carries.

The one place that enumerates the path-bearing fields. Both rewrites below run
through here, so a new path-bearing setting is added to this list rather than
to each of them — a config whose @[[databases]]@ path followed the file while
its @[[methods]]@ path followed the process was the bug that taught this.
-}
overPaths :: (PathKind -> FilePath -> FilePath) -> Config -> Config
overPaths f cfg =
    cfg
        { cfgDatabases = map (\d -> d{dcPath = content (dcPath d)}) (cfgDatabases cfg)
        , cfgMethods = map (\m -> m{mcPath = content (mcPath m)}) (cfgMethods cfg)
        , cfgFlowSynonyms = map refData (cfgFlowSynonyms cfg)
        , cfgCompartmentMappings = map refData (cfgCompartmentMappings cfg)
        , cfgUnits = map refData (cfgUnits cfg)
        , cfgEnergyDensities = map refData (cfgEnergyDensities cfg)
        , cfgGeographies = fmap reference (cfgGeographies cfg)
        , cfgChemSynonyms = fmap reference (cfgChemSynonyms cfg)
        , cfgSubstanceEdges = fmap reference (cfgSubstanceEdges cfg)
        }
  where
    reference = f ReferenceDataPath
    content = f UserContentPath
    refData r = r{rdSource = onFile (rdSource r)}
    onFile :: RefDataSource -> RefDataSource
    onFile (FromFile p) = FromFile (reference p)
    onFile b@(BuiltIn _) = b

{- | Apply redirectIntoDataDir to every reference-data path on the Config.
Database and method paths are the operator's own and are left untouched,
which is what keeps a database kept at @data\/agb.7z@ from being redirected
into the shipped bundle.
-}
applyDataDir :: Maybe FilePath -> Config -> Config
applyDataDir mDataDir = overPaths $ \kind path -> case kind of
    ReferenceDataPath -> redirectIntoDataDir mDataDir path
    UserContentPath -> path

{- | Resolve every relative path a config carries against the directory the
config file sits in, so one file describes the same setup from any working
directory. An absolute path is already unambiguous and is left alone; 'Nothing'
(no config file) means the process directory, which is all there is to go on.

Every path a config file carries is listed in 'overPaths', and nothing else
resolves one: a config whose @[[databases]]@ path followed the file while its
@[[methods]]@ path followed the process was the bug this replaces. Both kinds
are resolved the same way — a reference path @VOLCA_DATA_DIR@ already made
absolute is left alone by the @isAbsolute@ test, and one it did not touch
(the variable unset, or naming a relative directory) still needs anchoring.

'loadConfigOrDefault' composes this after 'applyDataDir', which is what
recognises a leading @data/@ as the shipped bundle - prefixing it with a
directory first would hide it.
-}
resolveConfigPaths :: Maybe FilePath -> Config -> Config
resolveConfigPaths mConfigPath = overPaths $ \kind path -> case kind of
    ReferenceDataPath -> resolve path
    UserContentPath -> resolve path
  where
    configDir = maybe "." takeDirectory mConfigPath
    resolve p = normalise $ if isAbsolute p then p else configDir </> p

-- | Validate configuration
validateConfig :: Config -> Either Text Config
validateConfig cfg = do
    -- Check for duplicate database names
    let dbNames = map dcName (cfgDatabases cfg)
        duplicates = repeated dbNames
    unless (null duplicates) $
        Left $
            "Duplicate database names: " <> T.intercalate ", " duplicates

    -- A duplicate name would silently shadow one of its bearers everywhere the
    -- name is the key (preset expansion, method lookup), so refuse it up front.
    let presetDupes = repeated (map cpName (cfgClassificationPresets cfg))
    unless (null presetDupes) $
        Left $
            "Duplicate classification preset names: " <> T.intercalate ", " presetDupes

    let methodDupes = repeated (map mcName (cfgMethods cfg))
    unless (null methodDupes) $
        Left $
            "Duplicate method collection names: " <> T.intercalate ", " methodDupes

    -- Check that at most one database is marked as default
    let defaultDbs = filter dcDefault (cfgDatabases cfg)
    when (length defaultDbs > 1) $
        Left $
            "Multiple databases marked as default: " <> T.intercalate ", " (map dcName defaultDbs)

    -- Validate dependency references exist
    let nameSet = S.fromList dbNames
    forM_ (cfgDatabases cfg) $ \db ->
        forM_ (dcDepends db) $ \dep ->
            unless (S.member dep nameSet) $
                Left $
                    "Database \"" <> dcName db <> "\" depends on unknown database: \"" <> dep <> "\""

    -- Validate no dependency cycles (resolveLoadOrder detects this)
    -- Run it with all databases marked as load=true to check the full graph
    let allLoaded = map (\db -> db{dcLoad = True}) (cfgDatabases cfg)
    case resolveLoadOrder allLoaded of
        Left err -> Left err
        Right _ -> Right cfg

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
            degrees' = foldl (flip (M.adjust (subtract 1))) degrees dependents
            newReady = [dep | dep <- dependents, M.findWithDefault 1 dep degrees' == 0]
         in go revAdj degrees' (q ++ newReady) (n : result) expected

{-# LANGUAGE StrictData #-}

{- | Plugin Architecture Types

8 capability-specific handle types covering the full LCA pipeline:

@
[Import] → [Transform] → [Map] → [Validate] → Compute → [Analyze] → [Report]
                                                 (core)
                          [Search] (orthogonal)
@

The core computation (matrix solve via MUMPS) is NOT a plugin.
Everything around it is pluggable via records of functions (Handle Pattern).
-}
module Plugin.Types (
    -- * Backend
    PluginBackend (..),

    -- * Handle types
    ImportHandle (..),
    ExportHandle (..),
    SearchHandle (..),
    MapperHandle (..),
    TransformHandle (..),
    ValidateHandle (..),
    AnalyzeHandle (..),
    ReportHandle (..),

    -- * Context types
    ImportResult (..),
    ExportContext (..),
    SearchQuery (..),
    SearchResult (..),
    MapContext (..),
    MapQuery (..),
    MapResult (..),
    TransformContext (..),
    TransformResult (..),
    ValidateContext (..),
    ValidationPhase (..),
    ValidationIssue (..),
    Severity (..),
    AnalyzeContext (..),

    -- * Registry
    PluginRegistry (..),
    emptyRegistry,
) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)

import Matrix (Inventory)
import Method.Types (Method, MethodCF)
import SynonymDB (SynonymDB)
import Types (Activity, ActivityMap, Database, Flow, FlowDB, SimpleDatabase, UnitDB)

-- | How a plugin handle is implemented
data PluginBackend
    = -- | Haskell function, no process boundary
      Builtin
    | -- | Standalone executable speaking JSON on stdin/stdout
      External !FilePath
    deriving (Show, Eq)

-- ──────────────────────────────────────────────
-- 1. IMPORT: bring data into the system
-- ──────────────────────────────────────────────
data ImportHandle = ImportHandle
    { ihName :: !Text
    , ihBackend :: !PluginBackend
    , ihCanRead :: FilePath -> IO Bool
    , ihRead :: Map Text Text -> FilePath -> IO (Either Text ImportResult)
    }

data ImportResult = ImportResult
    { irActivities :: !ActivityMap
    , irFlows :: !FlowDB
    , irUnits :: !UnitDB
    , irWarnings :: ![Text]
    }

-- ──────────────────────────────────────────────
-- 2. EXPORT: send data out of the system
-- ──────────────────────────────────────────────
data ExportHandle = ExportHandle
    { ehName :: !Text
    , ehBackend :: !PluginBackend
    , ehFormatId :: !Text
    , ehExport :: ExportContext -> FilePath -> IO ()
    }

data ExportContext = ExportContext
    { ecDatabase :: !Database
    , ecInventory :: !(Maybe Inventory)
    , ecMethods :: ![Method]
    }

-- ──────────────────────────────────────────────
-- 3. SEARCH: query loaded data
-- ──────────────────────────────────────────────
data SearchHandle = SearchHandle
    { shName :: !Text
    , shBackend :: !PluginBackend
    , shPriority :: !Int
    , shSearch :: Database -> SearchQuery -> IO [SearchResult]
    }

data SearchQuery = SearchQuery
    { sqText :: !Text
    , sqFilters :: !(Map Text Text)
    , sqLimit :: !Int
    }

data SearchResult = SearchResult
    { srId :: !UUID
    , srName :: !Text
    , srScore :: !Double
    , srMeta :: !(Map Text Text)
    }
    deriving (Show, Eq)

-- ──────────────────────────────────────────────
-- 4. MAPPER: match entities across domains
-- ──────────────────────────────────────────────
data MapperHandle = MapperHandle
    { mhName :: !Text
    , mhBackend :: !PluginBackend
    , mhPriority :: !Int
    , mhMatch :: MapContext -> MapQuery -> IO (Maybe MapResult)
    }

data MapContext = MapContext
    { mcFlowsByUUID :: !(Map UUID Flow)
    , mcFlowsByName :: !(Map Text [Flow])
    , mcFlowsByCAS :: !(Map Text [Flow])
    , mcSynonymDB :: !SynonymDB
    , mcActivities :: !(Map Text [Activity])
    }

data MapQuery
    = MatchCF !MethodCF
    | MatchMaterial !Text !(Maybe Text)
    | MatchSupplier !Text !Text

data MapResult = MapResult
    { mrTargetId :: !UUID
    , mrStrategy :: !Text
    , mrConfidence :: !Double
    }
    deriving (Show, Eq)

-- ──────────────────────────────────────────────
-- 5. TRANSFORM: modify data before computation
-- ──────────────────────────────────────────────
data TransformHandle = TransformHandle
    { thName :: !Text
    , thBackend :: !PluginBackend
    , thPriority :: !Int
    , thTransform :: TransformContext -> IO TransformResult
    }

data TransformContext = TransformContext
    { tcDatabase :: !SimpleDatabase
    , tcParameters :: !(Map Text Value)
    }

data TransformResult = TransformResult
    { trDatabase :: !SimpleDatabase
    , trLog :: ![Text]
    }

-- ──────────────────────────────────────────────
-- 6. VALIDATE: check data quality and consistency
-- ──────────────────────────────────────────────
data ValidateHandle = ValidateHandle
    { vhName :: !Text
    , vhBackend :: !PluginBackend
    , vhPhase :: !ValidationPhase
    , vhValidate :: ValidateContext -> IO [ValidationIssue]
    }

data ValidationPhase = PreCompute | PostCompute
    deriving (Show, Eq)

data ValidateContext = ValidateContext
    { vcDatabase :: !Database
    , vcInventory :: !(Maybe Inventory)
    }

data Severity = Error | Warning | Info
    deriving (Show, Eq)

data ValidationIssue = ValidationIssue
    { viSeverity :: !Severity
    , viCategory :: !Text
    , viMessage :: !Text
    , viEntity :: !(Maybe UUID)
    }
    deriving (Show, Eq)

-- ──────────────────────────────────────────────
-- 7. ANALYZE: post-computation analysis
-- ──────────────────────────────────────────────
data AnalyzeHandle = AnalyzeHandle
    { ahName :: !Text
    , ahBackend :: !PluginBackend
    , ahAnalyze :: AnalyzeContext -> IO Value
    }

data AnalyzeContext = AnalyzeContext
    { acDatabase :: !Database
    , acInventory :: !Inventory
    , acMethods :: ![Method]
    , acParameters :: !(Map Text Value)
    , acFlowDB :: !FlowDB
    {- ^ Merged flow metadata across every loaded DB. The 'Inventory' passed
    in may be cross-DB-merged, so analyzers that characterize or enrich
    by flow UUID should look up through these rather than @dbFlows acDatabase@
    (which would silently drop dep-DB flows).
    -}
    , acUnitDB :: !UnitDB
    }

-- ──────────────────────────────────────────────
-- 8. REPORT: format and present results
-- ──────────────────────────────────────────────
data ReportHandle = ReportHandle
    { rhName :: !Text
    , rhBackend :: !PluginBackend
    , rhFormatId :: !Text
    , rhMimeType :: !Text
    , rhReport :: Value -> IO BSL.ByteString
    }

-- ──────────────────────────────────────────────
-- REGISTRY: holds all registered plugin handles
-- ──────────────────────────────────────────────
data PluginRegistry = PluginRegistry
    { prImporters :: ![ImportHandle]
    , prExporters :: !(Map Text ExportHandle)
    , prSearchers :: ![SearchHandle]
    , prMappers :: ![MapperHandle]
    , prTransforms :: ![TransformHandle]
    , prValidators :: ![ValidateHandle]
    , prAnalyzers :: !(Map Text AnalyzeHandle)
    , prReporters :: !(Map Text ReportHandle)
    }

-- | Empty registry with no plugins
emptyRegistry :: PluginRegistry
emptyRegistry = PluginRegistry [] M.empty [] [] [] [] M.empty M.empty

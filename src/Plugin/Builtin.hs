{-# LANGUAGE OverloadedStrings #-}

-- | Built-in Plugin Handles
--
-- Wraps existing fpLCA features as plugin handles with zero behavior change.
-- These are always registered; external plugins from TOML config extend or
-- override them.
module Plugin.Builtin
    ( defaultRegistry
      -- * Individual handles (for testing / override)
    , uuidMapper
    , casMapper
    , nameMapper
    , synonymMapper
    , jsonReporter
    , csvReporter
    , tableReporter
    , prettyReporter
    , lciaAnalyzer
    , ecoinventMatrixExporter
    , debugMatrixExporter
      -- * Default mappers list
    , defaultMappers
    ) where

import Data.Aeson (encode, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map.Strict as M

import Plugin.Types

import qualified Method.Mapping as Mapping
import qualified Matrix.Export as MatrixExport
import Method.Types (MethodCF(..))
import Types (Flow(..), Database(..))
import SynonymDB (emptySynonymDB)
import Data.Maybe (fromMaybe)

-- | Default registry with all built-in plugins
defaultRegistry :: PluginRegistry
defaultRegistry = PluginRegistry
    { prImporters  = []  -- Importers stay in Database.Loader for now (complex caching)
    , prExporters  = M.fromList
        [ ("ecoinvent-matrix", ecoinventMatrixExporter)
        , ("debug-matrix", debugMatrixExporter)
        ]
    , prSearchers  = []  -- Search stays in Database.hs for now
    , prMappers    = defaultMappers
    , prTransforms = []
    , prValidators = []
    , prAnalyzers  = M.fromList [("lcia", lciaAnalyzer)]
    , prReporters  = M.fromList
        [ ("json",   jsonReporter)
        , ("csv",    csvReporter)
        , ("table",  tableReporter)
        , ("pretty", prettyReporter)
        ]
    }

-- | The default mapper cascade: UUID → CAS → Name → Synonym
defaultMappers :: [MapperHandle]
defaultMappers = [ uuidMapper, casMapper, nameMapper, synonymMapper ]

-- ──────────────────────────────────────────────
-- Mapper handles (wrap Method.Mapping functions)
-- ──────────────────────────────────────────────

uuidMapper :: MapperHandle
uuidMapper = MapperHandle
    { mhName     = "uuid-mapper"
    , mhBackend  = Builtin
    , mhPriority = 0
    , mhMatch    = \ctx query -> case query of
        MatchCF cf ->
            fmap (\f -> MapResult (flowId f) "uuid" 1.0) $
                Mapping.findFlowByUUID (mcFlowsByUUID ctx) (mcfFlowRef cf)
        _ -> Nothing
    }

casMapper :: MapperHandle
casMapper = MapperHandle
    { mhName     = "cas-mapper"
    , mhBackend  = Builtin
    , mhPriority = 10
    , mhMatch    = \ctx query -> case query of
        MatchCF cf -> do
            cas <- mcfCAS cf
            flow <- Mapping.findFlowByCAS (mcFlowsByCAS ctx) cas (mcfCompartment cf)
            pure $ MapResult (flowId flow) "cas" 1.0
        _ -> Nothing
    }

nameMapper :: MapperHandle
nameMapper = MapperHandle
    { mhName     = "name-mapper"
    , mhBackend  = Builtin
    , mhPriority = 20
    , mhMatch    = \ctx query -> case query of
        MatchCF cf ->
            fmap (\f -> MapResult (flowId f) "name" 0.9) $
                Mapping.findFlowByNameComp (mcFlowsByName ctx) (mcfFlowName cf) (mcfCompartment cf)
        _ -> Nothing
    }

synonymMapper :: MapperHandle
synonymMapper = MapperHandle
    { mhName     = "synonym-mapper"
    , mhBackend  = Builtin
    , mhPriority = 30
    , mhMatch    = \ctx query -> case query of
        MatchCF cf ->
            fmap (\f -> MapResult (flowId f) "synonym" 0.8) $
                Mapping.findFlowBySynonymComp (mcSynonymDB ctx) (mcFlowsByName ctx) (mcfFlowName cf) (mcfCompartment cf)
        _ -> Nothing
    }

-- ──────────────────────────────────────────────
-- Analyzer handles
-- ──────────────────────────────────────────────

lciaAnalyzer :: AnalyzeHandle
lciaAnalyzer = AnalyzeHandle
    { ahName    = "lcia"
    , ahBackend = Builtin
    , ahAnalyze = \ctx -> do
        let db = acDatabase ctx
            inv = acInventory ctx
            methods = acMethods ctx
            mapCtx = MapContext
                { mcFlowsByUUID = dbFlows db
                , mcFlowsByName = dbFlowsByName db
                , mcFlowsByCAS  = dbFlowsByCAS db
                , mcSynonymDB   = fromMaybe emptySynonymDB (dbSynonymDB db)
                , mcActivities  = M.empty
                }
            scores = [ toJSON $ M.fromList
                        [ ("method" :: String, toJSON (show m))
                        , ("score",  toJSON (Mapping.computeLCIAScore inv
                            (Mapping.mapMethodFlows defaultMappers mapCtx m)))
                        ]
                     | m <- methods
                     ]
        pure $ toJSON scores
    }

-- ──────────────────────────────────────────────
-- Export handles (wrap Matrix.Export functions)
-- ──────────────────────────────────────────────

ecoinventMatrixExporter :: ExportHandle
ecoinventMatrixExporter = ExportHandle
    { ehName     = "ecoinvent-matrix"
    , ehBackend  = Builtin
    , ehFormatId = "ecoinvent-matrix"
    , ehExport   = \ctx outPath ->
        MatrixExport.exportUniversalMatrixFormat outPath (ecDatabase ctx)
    }

debugMatrixExporter :: ExportHandle
debugMatrixExporter = ExportHandle
    { ehName     = "debug-matrix"
    , ehBackend  = Builtin
    , ehFormatId = "debug-matrix"
    , ehExport   = \_ _ -> pure ()  -- Debug export requires MatrixDebugInfo, invoked via Service
    }

-- ──────────────────────────────────────────────
-- Reporter handles (wrap CLI output formatting)
-- ──────────────────────────────────────────────

jsonReporter :: ReportHandle
jsonReporter = ReportHandle
    { rhName     = "json"
    , rhBackend  = Builtin
    , rhFormatId = "json"
    , rhMimeType = "application/json"
    , rhReport   = pure . encode
    }

csvReporter :: ReportHandle
csvReporter = ReportHandle
    { rhName     = "csv"
    , rhBackend  = Builtin
    , rhFormatId = "csv"
    , rhMimeType = "text/csv"
    , rhReport   = pure . encode  -- same as JSON for now (users pipe to jq)
    }

tableReporter :: ReportHandle
tableReporter = ReportHandle
    { rhName     = "table"
    , rhBackend  = Builtin
    , rhFormatId = "table"
    , rhMimeType = "text/plain"
    , rhReport   = pure . encodePretty
    }

prettyReporter :: ReportHandle
prettyReporter = ReportHandle
    { rhName     = "pretty"
    , rhBackend  = Builtin
    , rhFormatId = "pretty"
    , rhMimeType = "text/plain"
    , rhReport   = pure . encodePretty
    }

{-# LANGUAGE OverloadedStrings #-}

{- | Built-in Plugin Handles

Wraps existing VoLCA features as plugin handles with zero behavior change.
These are always registered; external plugins from TOML config extend or
override them.
-}
module Plugin.Builtin (
    defaultRegistry,

    -- * Individual handles (for testing / override)
    uuidMapper,
    casMapper,
    nameMapper,
    synonymMapper,
    jsonReporter,
    csvReporter,
    tableReporter,
    prettyReporter,
    lciaAnalyzer,
    hotspotAnalyzer,
    ecoinventMatrixExporter,
    debugMatrixExporter,

    -- * Built-in searchers
    nameSearcher,
    casSearcher,

    -- * Built-in importers
    ecospold2Importer,
    ecospold1Importer,
    simaproImporter,
    ilcdImporter,

    -- * Default mappers list
    defaultMappers,

    -- * Search
    searchWithPlugins,

    -- * Flow contribution (for API handlers)
    flowContribution,
) where

import Data.Aeson (encode, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified UnitConversion

import Plugin.Types

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Matrix (Inventory)
import qualified Matrix.Export as MatrixExport
import Method.Mapping (MatchStrategy)
import qualified Method.Mapping as Mapping
import Method.Types (Method (..), MethodCF (..))
import SynonymDB (emptySynonymDB)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Types (Database (..), Flow (..))

-- | Default registry with all built-in plugins
defaultRegistry :: PluginRegistry
defaultRegistry =
    PluginRegistry
        { prImporters = [ecospold2Importer, ecospold1Importer, simaproImporter, ilcdImporter]
        , prExporters =
            M.fromList
                [ ("ecoinvent-matrix", ecoinventMatrixExporter)
                , ("debug-matrix", debugMatrixExporter)
                ]
        , prSearchers = [nameSearcher, casSearcher]
        , prMappers = defaultMappers
        , prTransforms = []
        , prValidators = []
        , prAnalyzers = M.fromList [("lcia", lciaAnalyzer), ("flow-hotspot", hotspotAnalyzer)]
        , prReporters =
            M.fromList
                [ ("json", jsonReporter)
                , ("csv", csvReporter)
                , ("table", tableReporter)
                , ("pretty", prettyReporter)
                ]
        }

-- | The default mapper cascade: UUID → CAS → Name → Synonym
defaultMappers :: [MapperHandle]
defaultMappers = [uuidMapper, casMapper, nameMapper, synonymMapper]

-- ──────────────────────────────────────────────
-- Mapper handles (wrap Method.Mapping functions)
-- ──────────────────────────────────────────────

uuidMapper :: MapperHandle
uuidMapper =
    MapperHandle
        { mhName = "uuid-mapper"
        , mhBackend = Builtin
        , mhPriority = 0
        , mhMatch = \ctx query -> pure $ case query of
            MatchCF cf ->
                fmap (\f -> MapResult (flowId f) "uuid" 1.0) $
                    Mapping.findFlowByUUID (mcFlowsByUUID ctx) (mcfFlowRef cf)
            _ -> Nothing
        }

casMapper :: MapperHandle
casMapper =
    MapperHandle
        { mhName = "cas-mapper"
        , mhBackend = Builtin
        , mhPriority = 10
        , mhMatch = \ctx query -> pure $ case query of
            MatchCF cf -> do
                cas <- mcfCAS cf
                flow <- Mapping.findFlowByCAS (mcFlowsByCAS ctx) cas (mcfCompartment cf)
                pure $ MapResult (flowId flow) "cas" 1.0
            _ -> Nothing
        }

nameMapper :: MapperHandle
nameMapper =
    MapperHandle
        { mhName = "name-mapper"
        , mhBackend = Builtin
        , mhPriority = 20
        , mhMatch = \ctx query -> pure $ case query of
            MatchCF cf ->
                fmap (\f -> MapResult (flowId f) "name" 0.9) $
                    Mapping.findFlowByNameComp (mcFlowsByName ctx) (mcfFlowName cf) (mcfCompartment cf)
            _ -> Nothing
        }

synonymMapper :: MapperHandle
synonymMapper =
    MapperHandle
        { mhName = "synonym-mapper"
        , mhBackend = Builtin
        , mhPriority = 30
        , mhMatch = \ctx query -> pure $ case query of
            MatchCF cf ->
                fmap (\f -> MapResult (flowId f) "synonym" 0.8) $
                    Mapping.findFlowBySynonymComp (mcSynonymDB ctx) (mcFlowsByName ctx) (mcfFlowName cf) (mcfCompartment cf)
            _ -> Nothing
        }

-- ──────────────────────────────────────────────
-- Analyzer handles
-- ──────────────────────────────────────────────

lciaAnalyzer :: AnalyzeHandle
lciaAnalyzer =
    AnalyzeHandle
        { ahName = "lcia"
        , ahBackend = Builtin
        , ahAnalyze = \ctx -> do
            let db = acDatabase ctx
                inv = acInventory ctx
                methods = acMethods ctx
                mapCtx =
                    MapContext
                        { mcFlowsByUUID = dbFlows db
                        , mcFlowsByName = dbFlowsByName db
                        , mcFlowsByCAS = dbFlowsByCAS db
                        , mcSynonymDB = fromMaybe emptySynonymDB (dbSynonymDB db)
                        , mcActivities = M.empty
                        }
            scores <-
                mapM
                    ( \m -> do
                        mappings <- Mapping.mapMethodFlows defaultMappers mapCtx m
                        pure $
                            toJSON $
                                M.fromList
                                    [ ("method" :: String, toJSON (show m))
                                    , ("score", toJSON (Mapping.computeLCIAScore UnitConversion.defaultUnitConfig (acUnitDB ctx) (acFlowDB ctx) inv mappings))
                                    ]
                    )
                    methods
            pure $ toJSON scores
        }

-- ──────────────────────────────────────────────
-- Import handles (format detection probes)
-- ──────────────────────────────────────────────
-- Actual loading is managed by Database.Manager (caching, cross-DB linking).
-- These handles enable format detection and external format plugin registration.

ecospold2Importer :: ImportHandle
ecospold2Importer =
    ImportHandle
        { ihName = "ecospold2"
        , ihBackend = Builtin
        , ihCanRead = hasFilesWithExt ".spold"
        , ihRead = \_ _ -> pure $ Left "Use Database.Manager for full loading with caching"
        }

ecospold1Importer :: ImportHandle
ecospold1Importer =
    ImportHandle
        { ihName = "ecospold1"
        , ihBackend = Builtin
        , ihCanRead = hasFilesWithExt ".xml"
        , ihRead = \_ _ -> pure $ Left "Use Database.Manager for full loading with caching"
        }

simaproImporter :: ImportHandle
simaproImporter =
    ImportHandle
        { ihName = "simapro-csv"
        , ihBackend = Builtin
        , ihCanRead = hasFilesWithExt ".csv"
        , ihRead = \_ _ -> pure $ Left "Use Database.Manager for full loading with caching"
        }

ilcdImporter :: ImportHandle
ilcdImporter =
    ImportHandle
        { ihName = "ilcd"
        , ihBackend = Builtin
        , ihCanRead = \path -> do
            isDir <- doesDirectoryExist path
            if isDir
                then doesDirectoryExist (path </> "processes")
                else pure False
        , ihRead = \_ _ -> pure $ Left "Use Database.Manager for full loading with caching"
        }

-- | Check if path contains files with a given extension
hasFilesWithExt :: String -> FilePath -> IO Bool
hasFilesWithExt ext path = do
    isFile <- doesFileExist path
    if isFile
        then pure $ map toLower (takeExtension path) == ext
        else do
            isDir <- doesDirectoryExist path
            if isDir
                then do
                    files <- listDirectory path
                    pure $ any (\f -> map toLower (takeExtension f) == ext) files
                else pure False

-- ──────────────────────────────────────────────
-- Search handles (wrap Database search functions)
-- ──────────────────────────────────────────────

-- | Name-based flow search: substring match on flow name and synonyms
nameSearcher :: SearchHandle
nameSearcher =
    SearchHandle
        { shName = "name-searcher"
        , shBackend = Builtin
        , shPriority = 0
        , shSearch = \db query -> do
            let queryLower = T.toLower (sqText query)
                flows = M.elems (dbFlows db)
                matches =
                    [ (f, score)
                    | f <- flows
                    , let nameLower = T.toLower (flowName f)
                          synMatches =
                            any
                                (\syns -> any (T.isInfixOf queryLower . T.toLower) (S.toList syns))
                                (M.elems (flowSynonyms f))
                    , T.isInfixOf queryLower nameLower || synMatches
                    , let score =
                            if queryLower == nameLower
                                then 1.0
                                else
                                    if T.isPrefixOf queryLower nameLower
                                        then 0.9
                                        else 0.7
                    ]
                limited = take (sqLimit query) matches
            pure [SearchResult (flowId f) (flowName f) s M.empty | (f, s) <- limited]
        }

-- | CAS number search: exact CAS match on dbFlowsByCAS index
casSearcher :: SearchHandle
casSearcher =
    SearchHandle
        { shName = "cas-searcher"
        , shBackend = Builtin
        , shPriority = 10
        , shSearch = \db query -> do
            let cas = sqText query
            pure $ case M.lookup cas (dbFlowsByCAS db) of
                Just flows ->
                    [SearchResult (flowId f) (flowName f) 1.0 (M.singleton "cas" cas) | f <- take (sqLimit query) flows]
                Nothing -> []
        }

-- | Run search across all plugin searchers, merge and deduplicate by UUID
searchWithPlugins :: [SearchHandle] -> Database -> SearchQuery -> IO [SearchResult]
searchWithPlugins searchers db query = do
    allResults <- concat <$> mapM (\s -> shSearch s db query) searchers
    -- Deduplicate: keep highest-scoring result per UUID
    let byUUID =
            M.fromListWith
                (\a b -> if srScore a >= srScore b then a else b)
                [(srId r, r) | r <- allResults]
    pure $ M.elems byUUID

-- ──────────────────────────────────────────────
-- Analyzer handles
-- ──────────────────────────────────────────────

{- | Contribution of a single flow to the LCIA score.
Returns (MethodCF, Flow, contribution_value) when the flow is present in the inventory.
-}
flowContribution :: Inventory -> (MethodCF, Maybe (Flow, MatchStrategy)) -> Maybe (MethodCF, Flow, Double)
flowContribution inv (cf, Just (flow, _))
    | Just qty <- M.lookup (flowId flow) inv = Just (cf, flow, qty * mcfValue cf)
flowContribution _ _ = Nothing

{- | Hotspot analyzer: for each method, return top-N flows by contribution to the score.
This answers "why is my product 2.5 kg CO2e?" by showing which flows contribute most.
-}
hotspotAnalyzer :: AnalyzeHandle
hotspotAnalyzer =
    AnalyzeHandle
        { ahName = "flow-hotspot"
        , ahBackend = Builtin
        , ahAnalyze = \ctx -> do
            let db = acDatabase ctx
                inv = acInventory ctx
                methods = acMethods ctx
                mapCtx = Mapping.buildMapContext db
                topN = 20
                flowDB = acFlowDB ctx
                unitDB = acUnitDB ctx
            results <- mapM (analyzeMethod flowDB unitDB mapCtx inv topN) methods
            pure $ toJSON results
        }
  where
    -- Walk the inventory directly via 'inventoryContributions' so flows from
    -- dep DBs (absent from the root-DB method mapping) still contribute.
    -- Characterization uses the merged flow/unit snapshot the caller built.
    analyzeMethod flowDB unitDB mapCtx inv topN method = do
        mappings <- Mapping.mapMethodFlows defaultMappers mapCtx method
        let tables = Mapping.buildMethodTables mappings
            (rawContribs, _) = Mapping.inventoryContributions UnitConversion.defaultUnitConfig unitDB flowDB inv tables
            sorted = take topN $ sortOn (\(_, _, c) -> negate (abs c)) rawContribs
            total = Mapping.computeLCIAScoreFromTables UnitConversion.defaultUnitConfig unitDB flowDB inv tables
        pure $
            toJSON $
                M.fromList
                    [ ("method" :: String, toJSON (methodName method))
                    , ("unit", toJSON (methodUnit method))
                    , ("totalScore", toJSON total)
                    ,
                        ( "topFlows"
                        , toJSON
                            [ toJSON $
                                M.fromList
                                    [ ("flowName" :: String, toJSON (flowName f))
                                    , ("cfValue", toJSON cfVal)
                                    , ("contribution", toJSON contrib)
                                    , ("percent", toJSON (if total /= 0 then contrib / total * 100 else 0 :: Double))
                                    ]
                            | (f, cfVal, contrib) <- sorted
                            ]
                        )
                    ]

-- ──────────────────────────────────────────────
-- Export handles (wrap Matrix.Export functions)
-- ──────────────────────────────────────────────

ecoinventMatrixExporter :: ExportHandle
ecoinventMatrixExporter =
    ExportHandle
        { ehName = "ecoinvent-matrix"
        , ehBackend = Builtin
        , ehFormatId = "ecoinvent-matrix"
        , ehExport = \ctx outPath ->
            MatrixExport.exportUniversalMatrixFormat outPath (ecDatabase ctx)
        }

debugMatrixExporter :: ExportHandle
debugMatrixExporter =
    ExportHandle
        { ehName = "debug-matrix"
        , ehBackend = Builtin
        , ehFormatId = "debug-matrix"
        , ehExport = \_ _ -> pure () -- Debug export requires MatrixDebugInfo, invoked via Service
        }

-- ──────────────────────────────────────────────
-- Reporter handles (wrap CLI output formatting)
-- ──────────────────────────────────────────────

jsonReporter :: ReportHandle
jsonReporter =
    ReportHandle
        { rhName = "json"
        , rhBackend = Builtin
        , rhFormatId = "json"
        , rhMimeType = "application/json"
        , rhReport = pure . encode
        }

csvReporter :: ReportHandle
csvReporter =
    ReportHandle
        { rhName = "csv"
        , rhBackend = Builtin
        , rhFormatId = "csv"
        , rhMimeType = "text/csv"
        , rhReport = pure . encode -- same as JSON for now (users pipe to jq)
        }

tableReporter :: ReportHandle
tableReporter =
    ReportHandle
        { rhName = "table"
        , rhBackend = Builtin
        , rhFormatId = "table"
        , rhMimeType = "text/plain"
        , rhReport = pure . encodePretty
        }

prettyReporter :: ReportHandle
prettyReporter =
    ReportHandle
        { rhName = "pretty"
        , rhBackend = Builtin
        , rhFormatId = "pretty"
        , rhMimeType = "text/plain"
        , rhReport = pure . encodePretty
        }

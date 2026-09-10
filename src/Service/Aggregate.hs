{-# LANGUAGE OverloadedStrings #-}

{- | Generic SQL-group-by-style aggregation over exchanges, supply chain
entries, or biosphere flows. One small primitive that composes over
existing services.

No database-specific knowledge lives here: all filter/group-by keys are
generic and database-agnostic (flow names, units, classifications). Callers
supply database-specific semantics (e.g. "Heat + MJ means heat input") as
filter strings.
-}
module Service.Aggregate (
    AggregateParams (..),
    AggScope (..),
    AggregateFn (..),
    exchangeTypeScopeError,
    emptyAggregateParams,
    aggregate,
) where

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import API.Types (
    ActivitySummary (..),
    Aggregation (..),
    AggregationGroup (..),
    ExchangeDetail (..),
    InventoryExport (..),
    InventoryFlowDetail (..),
    SupplyChainEntry (..),
    SupplyChainResponse (..),
    apiFlowName,
 )
import Database (Geographies)
import Matrix (activityNormalizationFactor, buildDemandVectorFromIndex)
import Service (
    ActivityFilterCore (..),
    Edges (..),
    ServiceError (..),
    SupplyChainFilter (..),
    buildSupplyChainFromScalingVectorCrossDB,
    convertToInventoryExport,
    getActivityExchangeDetails,
    getReferenceProductAmount,
    getReferenceProductName,
    resolveScorable,
 )
import SharedSolver (DepSolverLookup, SharedSolver, computeInventoryMatrixWithDepsCached, solveWithSharedSolver)
import qualified SharedSolver
import Types (
    Activity,
    BioFlowDB,
    BiosphereFlow (..),
    CrossDBLink (..),
    Database (..),
    ExchangeKind (..),
    SparseTriple (..),
    UnitDB,
    activityClassification,
    activityLocation,
    activityName,
    activityUnit,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsInput,
    exchangeIsReference,
    exchangeKindOf,
    processIdToText,
    qualifyRef,
    supplierRefText,
 )
import UnitConversion (UnitConfig)

-- ---------------------------------------------------------------------------
-- Parameters
-- ---------------------------------------------------------------------------

data AggScope = ScopeDirect | ScopeSupplyChain | ScopeBiosphere | ScopeConsumption
    deriving (Eq, Show)

{- | Why a filter_exchange_type value cannot be combined with the given
scope — 'Nothing' when the combination is legal. Shared by the REST and
MCP surfaces so both reject identically instead of one silently
no-opping the filter.
-}
exchangeTypeScopeError :: AggScope -> Maybe ExchangeKind -> Maybe Text
exchangeTypeScopeError scope mKind = case mKind of
    Nothing -> Nothing
    Just _ -> case scope of
        ScopeDirect -> Nothing
        ScopeBiosphere -> Just "filter_exchange_type is redundant with scope=biosphere"
        ScopeSupplyChain -> Just "filter_exchange_type is not supported with scope=supply_chain (all entries are technosphere)"
        ScopeConsumption -> Just "filter_exchange_type is not supported with scope=consumption (all edges are technosphere)"

data AggregateFn = AggSum | AggCount | AggShare
    deriving (Eq, Show)

-- | A typed classification filter entry: (system, value, isExact).
type ClassEntry = (Text, Text, Bool)

data AggregateParams = AggregateParams
    { apScope :: AggScope
    , apIsInput :: Maybe Bool -- only for ScopeDirect
    , apMaxDepth :: Maybe Int -- only for ScopeSupplyChain
    , apFilterName :: Maybe Text -- case-insensitive substring
    , apFilterNameNot :: [Text] -- case-insensitive substrings (exclude-list)
    , apFilterUnit :: Maybe Text -- exact unit name
    , apFilterClassifications :: [ClassEntry]
    , apFilterTargetName :: Maybe Text -- ScopeDirect technosphere / ScopeConsumption (supplier)
    , apFilterConsumer :: Maybe Text -- only ScopeConsumption — case-insensitive substring
    , apFilterConsumerNot :: [Text] -- only ScopeConsumption — exclude-list
    , apFilterExchangeType :: Maybe ExchangeKind -- only ScopeDirect
    , apFilterIsReference :: Maybe Bool
    , apGroupBy :: Maybe Text
    , apAggregate :: AggregateFn
    }
    deriving (Show)

emptyAggregateParams :: AggScope -> AggregateParams
emptyAggregateParams s =
    AggregateParams
        { apScope = s
        , apIsInput = Nothing
        , apMaxDepth = Nothing
        , apFilterName = Nothing
        , apFilterNameNot = []
        , apFilterUnit = Nothing
        , apFilterClassifications = []
        , apFilterTargetName = Nothing
        , apFilterConsumer = Nothing
        , apFilterConsumerNot = []
        , apFilterExchangeType = Nothing
        , apFilterIsReference = Nothing
        , apGroupBy = Nothing
        , apAggregate = AggSum
        }

-- ---------------------------------------------------------------------------
-- Intermediate row, populated from each scope
-- ---------------------------------------------------------------------------

data AggRow = AggRow
    { rowName :: !Text
    , rowFlowId :: !Text -- UUID text or activity processId
    , rowUnit :: !Text
    , rowQuantity :: !Double
    , rowIsInput :: !(Maybe Bool)
    , rowIsReference :: !(Maybe Bool)
    , rowTargetName :: !(Maybe Text) -- direct technosphere / consumption (supplier)
    , rowConsumerName :: !(Maybe Text) -- only consumption
    , rowLocation :: !(Maybe Text) -- only supply_chain / consumption
    , rowExchangeType :: !(Maybe ExchangeKind) -- only direct / biosphere
    , rowClassifications :: !(M.Map Text Text)
    }

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

aggregate ::
    UnitConfig ->
    Geographies ->
    BioFlowDB -> -- merged (root + deps) for biosphere scope
    UnitDB -> -- merged (root + deps) for biosphere scope
    Database ->
    Text -> -- root DB name (for tagging supply-chain entries)
    SharedSolver ->
    DepSolverLookup -> -- cross-DB lookup for ScopeBiosphere
    Text -> -- processId text
    AggregateParams ->
    IO (Either ServiceError Aggregation)
aggregate unitConfig geographies flowDB unitDB db dbName solver depLookup pidText params =
    case resolveScorable db pidText of
        Left err -> return (Left err)
        Right (processId, activity) ->
            case apScope params of
                ScopeDirect ->
                    return $ Right $ reduce params (rowsFromDirect db activity)
                ScopeSupplyChain -> do
                    let demandVec = buildDemandVectorFromIndex (dbActivityIndex db) processId
                    supplyVec <- solveWithSharedSolver solver demandVec
                    let af = emptyFilter (apMaxDepth params)
                    eResp <-
                        buildSupplyChainFromScalingVectorCrossDB
                            unitConfig
                            geographies
                            depLookup
                            db
                            dbName
                            processId
                            supplyVec
                            []
                            af
                    return $ fmap (reduce params . rowsFromSupplyChain) eResp
                ScopeBiosphere -> do
                    solE <- computeInventoryMatrixWithDepsCached unitConfig depLookup db dbName solver processId
                    case solE of
                        Left err -> return (Left (MatrixError err))
                        Right sol ->
                            let inventory = SharedSolver.csInventory sol
                                export = convertToInventoryExport db flowDB unitDB processId activity inventory
                             in return $ Right $ reduce params (rowsFromBiosphere export)
                ScopeConsumption -> do
                    -- ponytail: reuses the biosphere solve, whose inventory half is
                    -- discarded here; a scaling-only cross-DB walk is the upgrade
                    -- path if profiling ever complains.
                    solE <- computeInventoryMatrixWithDepsCached unitConfig depLookup db dbName solver processId
                    case solE of
                        Left err -> return (Left (MatrixError err))
                        Right sol ->
                            let rootRefAmount = getReferenceProductAmount activity
                             in return $ Right $ reduce params (rowsFromConsumption rootRefAmount (SharedSolver.csScalings sol))
  where
    emptyFilter maxD =
        SupplyChainFilter
            { scfCore =
                ActivityFilterCore
                    { afcName = Nothing
                    , afcLocation = Nothing
                    , afcProduct = Nothing
                    , afcClassifications = []
                    , afcLimit = Just maxBound
                    , afcOffset = Nothing
                    , afcSort = Nothing
                    , afcOrder = Nothing
                    }
            , scfMaxDepth = maxD
            , scfMinQuantity = Nothing
            , scfEdges = EntriesOnly
            }

-- ---------------------------------------------------------------------------
-- Scope → AggRow conversions
-- ---------------------------------------------------------------------------

rowsFromDirect :: Database -> Activity -> [AggRow]
rowsFromDirect db act =
    map mkRow (getActivityExchangeDetails db act (const True))
  where
    mkRow (ExchangeDetail ex flow _flowUnit _unit exUnitName target) =
        AggRow
            { rowName = apiFlowName flow
            , rowFlowId = UUID.toText (exchangeFlowId ex)
            , rowUnit = exUnitName
            , rowQuantity = exchangeAmount ex
            , rowIsInput = Just (exchangeIsInput ex)
            , rowIsReference = Just (exchangeIsReference ex)
            , rowTargetName = fmap prsActivityName target
            , rowConsumerName = Nothing
            , rowLocation = fmap prsLocation target
            , rowExchangeType = Just (exchangeKindOf ex)
            , rowClassifications = M.empty
            }

rowsFromSupplyChain :: SupplyChainResponse -> [AggRow]
rowsFromSupplyChain response =
    map mkRow (scrSupplyChain response)
  where
    mkRow e =
        AggRow
            { rowName = sceActivityName e
            , rowFlowId = sceProcessId e
            , rowUnit = sceUnit e
            , rowQuantity = sceQuantity e
            , rowIsInput = Nothing
            , rowIsReference = Nothing
            , rowTargetName = Nothing
            , rowConsumerName = Nothing
            , rowLocation = Just (sceLocation e)
            , rowExchangeType = Nothing
            , rowClassifications = sceClassifications e
            }

rowsFromBiosphere :: InventoryExport -> [AggRow]
rowsFromBiosphere export =
    map mkRow (ieFlows export)
  where
    mkRow (InventoryFlowDetail flow qty uName isEmission _cat) =
        AggRow
            { rowName = bfName flow
            , rowFlowId = UUID.toText (bfId flow)
            , rowUnit = uName
            , rowQuantity = qty
            , rowIsInput = Just (not isEmission)
            , rowIsReference = Nothing
            , rowTargetName = Nothing
            , rowConsumerName = Nothing
            , rowLocation = Nothing
            , rowExchangeType = Just KindBiosphere
            , rowClassifications = M.empty
            }

{- | One row per scaled technosphere edge across the whole chain: for each
coefficient A[supplier, consumer] whose consumer has a non-zero scaling
s_consumer, the quantity is @coefficient × s_consumer × multiplier@ — the
total amount of the supplier's product consumed by that consumer for the
functional unit. Summing filtered edges never double-counts a
transformation chain the way summing cumulative supply-chain productions
does, because each consumption event is one row.

Signs pass through untouched: inputs are stored positive, byproduct
outputs negative, and treatment-convention columns flip both the
coefficient and the scaling, so their product stays sign-correct.
Filtering on positive values here would silently drop real inputs of
treatment activities.

Cross-DB bridge edges (consumer in one DB, supplier in a dependency) are
not matrix triples; they are folded in from 'dbCrossDBLinks' with the
same demand formula as 'Matrix.accumulateDepDemandsWith'. Their
supplier-side fields that live in the dependency database (target name,
classifications) are left empty rather than resolved — a classification
or target-name filter therefore never matches a bridge edge.
-}
rowsFromConsumption :: Double -> NonEmpty (Text, Database, VU.Vector Double) -> [AggRow]
rowsFromConsumption rootRefAmount ((rootDbName, rootDb, rootScaling) :| deps) =
    dbRows False rootDbName rootDb rootScaling rootRefAmount
        <> concatMap (\(depName, depDb, depScaling) -> dbRows True depName depDb depScaling 1.0) deps
  where
    dbRows qualifyPids dbN db' s mult =
        internalEdges qualifyPids dbN db' s mult <> bridgeEdges db' s mult

    -- Matrix indices double as ProcessIds, same convention as
    -- 'Service.collectSupplyChainEntries'.
    internalEdges qualifyPids dbN db' s mult =
        VU.foldr step [] (dbTechnosphereTriples db')
      where
        step (SparseTriple supplierIdx consumerIdx v) acc =
            let sj = s VU.! fromIntegral consumerIdx
             in if sj == 0 then acc else mkEdge supplierIdx consumerIdx v sj : acc
        mkEdge supplierIdx consumerIdx v sj =
            let supplier = dbActivities db' V.! fromIntegral supplierIdx
                consumer = dbActivities db' V.! fromIntegral consumerIdx
                pidText = processIdToText db' (fromIntegral supplierIdx)
             in AggRow
                    { rowName = fromMaybe (activityName supplier) (getReferenceProductName (dbTechFlows db') supplier)
                    , rowFlowId = if qualifyPids then qualifyRef dbN pidText else pidText
                    , rowUnit = activityUnit supplier
                    , rowQuantity = v * sj * mult
                    , rowIsInput = Nothing
                    , rowIsReference = Nothing
                    , rowTargetName = Just (activityName supplier)
                    , rowConsumerName = Just (activityName consumer)
                    , rowLocation = Just (activityLocation supplier)
                    , rowExchangeType = Nothing
                    , rowClassifications = activityClassification supplier
                    }

    bridgeEdges db' s mult =
        mapMaybe bridgeRow (dbCrossDBLinks db')
      where
        bridgeRow link = do
            consumerPid <- M.lookup (cdlConsumerActUUID link, cdlConsumerProdUUID link) (dbProcessIdLookup db')
            let consumerIdx = fromIntegral (dbActivityIndex db' V.! fromIntegral consumerPid)
                sj = s VU.! consumerIdx
                consumer = dbActivities db' V.! fromIntegral consumerPid
            if sj == 0
                then Nothing
                else
                    Just
                        AggRow
                            { rowName = cdlFlowName link
                            , rowFlowId = supplierRefText link
                            , rowUnit = cdlExchangeUnit link
                            , rowQuantity = cdlCoefficient link * sj / activityNormalizationFactor db' consumerPid * mult
                            , rowIsInput = Nothing
                            , rowIsReference = Nothing
                            , rowTargetName = Nothing
                            , rowConsumerName = Just (activityName consumer)
                            , rowLocation = Just (cdlLocation link)
                            , rowExchangeType = Nothing
                            , rowClassifications = M.empty
                            }

-- ---------------------------------------------------------------------------
-- Filter / group / reduce pipeline
-- ---------------------------------------------------------------------------

filterRow :: AggregateParams -> AggRow -> Bool
filterRow p r =
    checkMaybe apIsInput rowIsInput
        && checkMaybe apFilterIsReference rowIsReference
        && nameOk
        && nameNotOk
        && unitOk
        && targetOk
        && consumerOk
        && consumerNotOk
        && exchangeTypeOk
        && classOk
  where
    checkMaybe getter rowGet = case getter p of
        Nothing -> True
        Just want -> case rowGet r of
            Nothing -> True -- row lacks the attribute → don't exclude
            Just actual -> actual == want
    ci = T.toLower
    contains needle hay = ci needle `T.isInfixOf` ci hay
    nameOk = case apFilterName p of
        Nothing -> True
        Just q -> contains q (rowName r)
    nameNotOk = not $ any (`contains` rowName r) (apFilterNameNot p)
    -- A unit is matched whole, not as a substring: "kg" must not take "kgm".
    -- Case and surrounding space are ignored, because the caller writes the
    -- unit the way their database spells it ("MJ") while a row carries the
    -- reference spelling ("mj"), and an exact compare would answer zero rows
    -- rather than say the two are the same unit.
    unitOk = case apFilterUnit p of
        Nothing -> True
        Just u -> ci (T.strip u) == ci (T.strip (rowUnit r))
    targetOk = case apFilterTargetName p of
        Nothing -> True
        Just q -> case rowTargetName r of
            Just t -> contains q t
            Nothing -> False
    consumerOk = case apFilterConsumer p of
        Nothing -> True
        Just q -> case rowConsumerName r of
            Just c -> contains q c
            Nothing -> False
    consumerNotOk = case rowConsumerName r of
        Nothing -> True -- row lacks the attribute → the exclude-list can't hit it
        Just c -> not (any (`contains` c) (apFilterConsumerNot p))
    exchangeTypeOk = case apFilterExchangeType p of
        Nothing -> True
        Just want -> case rowExchangeType r of
            Just actual -> actual == want
            Nothing -> True -- row lacks the attribute → don't exclude
    classOk = all classMatches (apFilterClassifications p)
    classMatches (sys, val, isExact) = case M.lookup sys (rowClassifications r) of
        Nothing -> False
        Just v ->
            if isExact
                then ci val == ci v
                else contains val v

-- | Extract the group key for one row.
groupKey :: Text -> AggRow -> Text
groupKey key r = case key of
    "name" -> rowName r
    "flow_id" -> rowFlowId r
    "name_prefix" -> prefixOf (rowName r)
    "unit" -> rowUnit r
    "location" -> fromMaybe "" (rowLocation r)
    "target_name" -> fromMaybe "" (rowTargetName r)
    "consumer_name" -> fromMaybe "" (rowConsumerName r)
    other
        | Just sys <- T.stripPrefix "classification." other ->
            fromMaybe "" (M.lookup sys (rowClassifications r))
        | otherwise -> ""
  where
    prefixOf n = case T.findIndex (\c -> c == ',' || c == '{' || c == '(') n of
        Just i -> T.strip (T.take i n)
        Nothing -> n

-- | Combine rows into an Aggregation.
reduce :: AggregateParams -> [AggRow] -> Aggregation
reduce p rowsAll =
    let matched = filter (filterRow p) rowsAll
        total = sum (map rowQuantity matched)
        fnCount = length matched
        unit = homogeneousUnit matched
        groups = case apGroupBy p of
            Nothing -> []
            Just key ->
                let pairs = [(groupKey key r, r) | r <- matched]
                    grouped = groupByKey pairs
                 in map (mkGroup total) grouped
     in Aggregation
            { aggScope = scopeText (apScope p)
            , aggFilteredTotal = total
            , aggFilteredUnit = unit
            , aggFilteredCount = fnCount
            , aggGroups = sortGroups groups
            }
  where
    mkGroup total (key, rs) =
        let gqty = sum (map rowQuantity rs)
            gunit = homogeneousUnit rs
         in AggregationGroup
                { aggKey = key
                , aggQuantity = gqty
                , aggUnit = gunit
                , aggShare = case apAggregate p of
                    AggShare -> if total == 0 then Just 0 else Just (gqty / total)
                    _ -> Nothing
                , aggCount = length rs
                }
    sortGroups = L.sortBy (\a b -> compare (abs (aggQuantity b)) (abs (aggQuantity a)))

scopeText :: AggScope -> Text
scopeText ScopeDirect = "direct"
scopeText ScopeSupplyChain = "supply_chain"
scopeText ScopeBiosphere = "biosphere"
scopeText ScopeConsumption = "consumption"

homogeneousUnit :: [AggRow] -> Maybe Text
homogeneousUnit [] = Nothing
homogeneousUnit (r : rs) =
    if all (\x -> rowUnit x == rowUnit r) rs
        then Just (rowUnit r)
        else Nothing

-- | Group rows by the first element of the pair, preserving insertion order.
groupByKey :: [(Text, AggRow)] -> [(Text, [AggRow])]
groupByKey pairs =
    let m = L.foldl' step M.empty pairs
     in M.toList m
  where
    step acc (k, v) = M.insertWith (++) k [v] acc

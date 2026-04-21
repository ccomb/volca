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
    emptyAggregateParams,
    aggregate,
) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import API.Types (
    ActivitySummary (..),
    Aggregation (..),
    AggregationGroup (..),
    ExchangeDetail (..),
    InventoryExport (..),
    InventoryFlowDetail (..),
    SupplyChainEntry (..),
    SupplyChainResponse (..),
 )
import Matrix (buildDemandVectorFromIndex)
import Service (
    ActivityFilterCore (..),
    ServiceError (..),
    SupplyChainFilter (..),
    buildSupplyChainFromScalingVectorCrossDB,
    convertToInventoryExport,
    getActivityExchangeDetails,
    resolveActivityAndProcessId,
 )
import SharedSolver (DepSolverLookup, SharedSolver, computeInventoryMatrixWithDepsCached, solveWithSharedSolver)
import Types (
    Activity,
    Database (..),
    Flow (..),
    FlowDB,
    FlowType (..),
    UnitDB,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsInput,
    exchangeIsReference,
    isTechnosphereExchange,
 )
import UnitConversion (UnitConfig)

-- ---------------------------------------------------------------------------
-- Parameters
-- ---------------------------------------------------------------------------

data AggScope = ScopeDirect | ScopeSupplyChain | ScopeBiosphere
    deriving (Eq, Show)

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
    , apFilterTargetName :: Maybe Text -- only ScopeDirect technosphere
    , apFilterExchangeType :: Maybe FlowType -- only ScopeDirect
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
    , rowTargetName :: !(Maybe Text) -- only direct technosphere
    , rowLocation :: !(Maybe Text) -- only supply_chain
    , rowExchangeType :: !(Maybe FlowType) -- only direct / biosphere
    , rowClassifications :: !(M.Map Text Text)
    }

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

aggregate ::
    UnitConfig ->
    FlowDB -> -- merged (root + deps) for biosphere scope
    UnitDB -> -- merged (root + deps) for biosphere scope
    Database ->
    Text -> -- root DB name (for tagging supply-chain entries)
    SharedSolver ->
    DepSolverLookup -> -- cross-DB lookup for ScopeBiosphere
    Text -> -- processId text
    AggregateParams ->
    IO (Either ServiceError Aggregation)
aggregate unitConfig flowDB unitDB db dbName solver depLookup pidText params =
    case resolveActivityAndProcessId db pidText of
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
                            depLookup
                            db
                            dbName
                            processId
                            supplyVec
                            []
                            af
                            False
                    return $ fmap (reduce params . rowsFromSupplyChain) eResp
                ScopeBiosphere -> do
                    invE <- computeInventoryMatrixWithDepsCached unitConfig depLookup db solver processId
                    case invE of
                        Left err -> return (Left (MatrixError err))
                        Right inventory ->
                            let export = convertToInventoryExport db flowDB unitDB processId activity inventory
                             in return $ Right $ reduce params (rowsFromBiosphere export)
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
            { rowName = flowName flow
            , rowFlowId = UUID.toText (exchangeFlowId ex)
            , rowUnit = exUnitName
            , rowQuantity = exchangeAmount ex
            , rowIsInput = Just (exchangeIsInput ex)
            , rowIsReference = Just (exchangeIsReference ex)
            , rowTargetName = fmap prsName target
            , rowLocation = fmap prsLocation target
            , rowExchangeType = Just (if isTechnosphereExchange ex then Technosphere else Biosphere)
            , rowClassifications = M.empty -- flow-level classifications not yet on Flow; use filter_name instead
            }

rowsFromSupplyChain :: SupplyChainResponse -> [AggRow]
rowsFromSupplyChain response =
    map mkRow (scrSupplyChain response)
  where
    mkRow e =
        AggRow
            { rowName = sceName e
            , rowFlowId = sceProcessId e
            , rowUnit = sceUnit e
            , rowQuantity = sceQuantity e
            , rowIsInput = Nothing
            , rowIsReference = Nothing
            , rowTargetName = Nothing
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
            { rowName = flowName flow
            , rowFlowId = UUID.toText (flowId flow)
            , rowUnit = uName
            , rowQuantity = qty
            , rowIsInput = Just (not isEmission)
            , rowIsReference = Nothing
            , rowTargetName = Nothing
            , rowLocation = Nothing
            , rowExchangeType = Just Biosphere
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
    unitOk = case apFilterUnit p of
        Nothing -> True
        Just u -> u == rowUnit r
    targetOk = case apFilterTargetName p of
        Nothing -> True
        Just q -> case rowTargetName r of
            Just t -> contains q t
            Nothing -> False
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

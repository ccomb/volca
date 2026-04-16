{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Flow Mapping Engine
--
-- Maps characterization factor flows from LCIA methods to database flows
-- using a configurable cascade of MapperHandles (plugin architecture).
-- Default cascade: UUID → CAS → Name → Synonym.
module Method.Mapping
    ( -- * Mapping functions
      mapMethodFlows
    , mapMethodToFlows
    , mapSingleFlow
    , buildMapContext
      -- * LCIA scoring
    , MethodTables(..)
    , buildMethodTables
    , computeLCIAScore
    , computeLCIAScoreFromTables
      -- * Matching strategies
    , MatchStrategy(..)
    , strategyFromText
    , findFlowByUUID
    , findFlowByName
    , findFlowByNameComp
    , findFlowBySynonym
    , findFlowBySynonymComp
    , findFlowByCAS
      -- * Statistics
    , MappingStats(..)
    , computeMappingStats
    ) where

import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Matrix (Inventory)
import Types (Database(..), Flow(..), FlowDB, Unit(..), UnitDB)
import Method.Types
import UnitConversion (UnitConfig, convertUnit)
import Plugin.Types (MapperHandle(..), MapContext(..), MapQuery(..), MapResult(..))
import SynonymDB

-- | Matching strategy used to find a flow
data MatchStrategy
    = ByUUID       -- ^ Exact UUID match
    | ByCAS        -- ^ CAS number match
    | ByName       -- ^ Normalized name match
    | BySynonym    -- ^ Via synonym group
    | ByFuzzy      -- ^ Fuzzy string matching
    | NoMatch      -- ^ No match found
    deriving (Eq, Show)

-- | Statistics about mapping results
data MappingStats = MappingStats
    { msTotal      :: !Int  -- ^ Total CFs in method
    , msByUUID     :: !Int  -- ^ Matched by UUID
    , msByCAS      :: !Int  -- ^ Matched by CAS
    , msByName     :: !Int  -- ^ Matched by name
    , msBySynonym  :: !Int  -- ^ Matched by synonym
    , msByFuzzy    :: !Int  -- ^ Matched by fuzzy
    , msUnmatched  :: !Int  -- ^ Not matched
    } deriving (Eq, Show)

-- | Build a MapContext from a Database (convenience for callers)
buildMapContext :: Database -> MapContext
buildMapContext db = MapContext
    { mcFlowsByUUID = dbFlows db
    , mcFlowsByName = dbFlowsByName db
    , mcFlowsByCAS  = dbFlowsByCAS db
    , mcSynonymDB   = fromMaybe emptySynonymDB (dbSynonymDB db)
    , mcActivities  = M.empty
    }

-- | Map all method flows to database flows using mapper handles.
-- Mappers are tried in priority order (assumed pre-sorted).
mapMethodFlows
    :: [MapperHandle]
    -> MapContext
    -> Method
    -> IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodFlows mappers ctx method =
    mapM (\cf -> fmap (cf,) (mapSingleFlow mappers ctx cf)) (methodFactors method)

-- | Map a single CF using the mapper handle cascade.
-- Each mapper is tried in order; the first match wins.
mapSingleFlow
    :: [MapperHandle]
    -> MapContext
    -> MethodCF
    -> IO (Maybe (Flow, MatchStrategy))
mapSingleFlow mappers ctx cf = go mappers
  where
    go [] = pure Nothing
    go (m:ms) = do
        result <- mhMatch m ctx (MatchCF cf)
        case result of
            Just mr | Just flow <- M.lookup (mrTargetId mr) (mcFlowsByUUID ctx) ->
                pure $ Just (flow, strategyFromText (mrStrategy mr))
            _ -> go ms

-- | Convenience wrapper: map method CFs using the given mappers + DB.
mapMethodToFlows :: [MapperHandle] -> Database -> Method -> IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodToFlows mappers db = mapMethodFlows mappers (buildMapContext db)

-- | Convert strategy text back to MatchStrategy
strategyFromText :: Text -> MatchStrategy
strategyFromText t = case T.toLower t of
    "uuid"    -> ByUUID
    "cas"     -> ByCAS
    "name"    -> ByName
    "synonym" -> BySynonym
    "fuzzy"   -> ByFuzzy
    _         -> ByFuzzy  -- Unknown strategies map to fuzzy

-- ──────────────────────────────────────────────
-- Low-level matching functions (used by built-in MapperHandles)
-- ──────────────────────────────────────────────

-- | Find flow by exact UUID match
findFlowByUUID :: M.Map UUID Flow -> UUID -> Maybe Flow
findFlowByUUID flowsByUUID uuid = M.lookup uuid flowsByUUID

-- | Find flow by CAS number with compartment preference
findFlowByCAS :: M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowByCAS flowsByCAS cas mComp =
    M.lookup cas flowsByCAS >>= \flows -> pickByCompartment flows mComp

-- | Find flow by normalized name match (compartment-aware)
findFlowByName :: M.Map Text [Flow] -> Text -> Maybe Flow
findFlowByName flowsByName name = findFlowByNameComp flowsByName name Nothing

-- | Find flow by normalized name with compartment preference
findFlowByNameComp :: M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowByNameComp flowsByName name mComp =
    M.lookup (normalizeName name) flowsByName >>= \flows -> pickByCompartment flows mComp

-- | Find flow via synonym group (compartment-aware)
findFlowBySynonym :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Flow
findFlowBySynonym synDB flowsByName name = findFlowBySynonymComp synDB flowsByName name Nothing

-- | Find flow via synonym group with compartment preference
findFlowBySynonymComp :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowBySynonymComp synDB flowsByName name mComp =
    case lookupSynonymGroup synDB name of
        Nothing  -> Nothing
        Just gid -> getSynonyms synDB gid >>= \synonyms ->
            pickByCompartment (concatMap (lookupFlows flowsByName) synonyms) mComp
  where
    lookupFlows fbn syn = M.findWithDefault [] (normalizeName syn) fbn

-- | Pick the best flow match based on compartment preference.
-- Returns Nothing for an empty candidate list.
pickByCompartment :: [Flow] -> Maybe Compartment -> Maybe Flow
pickByCompartment []    _          = Nothing
pickByCompartment (f:_) Nothing    = Just f
pickByCompartment (f:fs) (Just comp) = Just $
    case find (exactCompMatch comp) (f:fs) of
        Just m  -> m
        Nothing -> fromMaybe f (find (mediumMatch comp) (f:fs))
  where
    exactCompMatch (Compartment med sub _) fl =
        let cat    = T.toLower (flowCategory fl)
            subcomp = maybe "" T.toLower (flowSubcompartment fl)
        in matchMedium med cat && (T.null sub || sub == subcomp || sub `T.isInfixOf` subcomp)

    mediumMatch (Compartment med _ _) fl =
        matchMedium med (T.toLower (flowCategory fl))

    matchMedium med cat
        | T.null med        = True
        | med == cat        = True
        | med `T.isInfixOf` cat = True
        | otherwise         = False

-- | Compute statistics about mapping results
computeMappingStats :: [(MethodCF, Maybe (Flow, MatchStrategy))] -> MappingStats
computeMappingStats mappings = MappingStats
    { msTotal = length mappings
    , msByUUID = count ByUUID
    , msByCAS = count ByCAS
    , msByName = count ByName
    , msBySynonym = count BySynonym
    , msByFuzzy = count ByFuzzy
    , msUnmatched = length $ filter (isNothing . snd) mappings
    }
  where
    count strategy = length $ filter ((== Just strategy) . fmap snd . snd) mappings

-- | Precomputed CF lookup tables for one (database, method) pair.
-- Building these from raw mappings is O(n log n) over thousands of CFs, so they
-- should be computed once per method and reused across inventories.
data MethodTables = MethodTables
    { mtUuidCF     :: !(M.Map UUID (Double, Text))
    -- ^ UUID-matched CFs: exact flow id → (CF value, CF unit)
    , mtExactCF    :: !(M.Map (Text, Text, Text) (Double, Text))
    -- ^ (normalized name, medium, subcompartment) → (CF, unit)
    , mtFallbackCF :: !(M.Map (Text, Text) (Double, Text))
    -- ^ (normalized name, medium) → (CF, unit) for entries with unspecified subcompartment
    }

-- | Build 'MethodTables' from raw mappings. Run once per (db, method).
buildMethodTables :: [(MethodCF, Maybe (Flow, MatchStrategy))] -> MethodTables
buildMethodTables mappings = MethodTables
    { mtUuidCF     = M.fromList
        [ (flowId flow, (mcfValue cf, mcfUnit cf))
        | (cf, Just (flow, ByUUID)) <- mappings ]
    , mtExactCF    = stripStrategy $ M.fromListWith preferBetter
        [ ((nameKey cf mflow, normalizeMedium medium, subcomp), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
        | (cf, mflow) <- mappings
        , Just (Compartment medium subcomp _) <- [mcfCompartment cf]
        , not (T.null subcomp) ]
    , mtFallbackCF = stripStrategy $ M.fromListWith preferBetter
        [ ((nameKey cf mflow, normalizeMedium medium), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
        | (cf, mflow) <- mappings
        , Just (Compartment medium subcomp _) <- [mcfCompartment cf]
        , T.null subcomp ]
    }
  where
    stripStrategy = M.map (\(v, u, _) -> (v, u))

    preferBetter (v1, u1, s1) (v2, u2, s2)
        | stratPriority s1 < stratPriority s2 = (v1, u1, s1)
        | stratPriority s1 > stratPriority s2 = (v2, u2, s2)
        | v1 >= v2  = (v1, u1, s1)
        | otherwise = (v2, u2, s2)
    stratPriority ByUUID = 0 :: Int
    stratPriority ByCAS = 1
    stratPriority ByName = 2
    stratPriority BySynonym = 3
    stratPriority _ = 4

    matchStrategy mflow = case mflow of
        Just (_, s) -> s
        Nothing     -> NoMatch

    -- Use matched flow's name only for name/synonym matches
    nameKey cf mflow = normalizeName $ case mflow of
        Just (flow, ByName)    -> flowName flow
        Just (flow, BySynonym) -> flowName flow
        _                      -> mcfFlowName cf

    -- Normalize medium names between method CFs and database flows
    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise               = m

-- | Score an inventory against precomputed 'MethodTables'.
-- Hot path: O(|inventory|) per call, no map construction.
computeLCIAScoreFromTables :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> MethodTables -> Double
computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables =
    sum [ scoreFlow fid qty | (fid, qty) <- M.toList inventory, qty /= 0 ]
  where
    scoreFlow fid qty = case lookupCF fid of
        Nothing -> 0
        Just (cfVal, cfUnit) ->
            let flowUnit = maybe "" unitName (M.lookup fid flowDB >>= \f -> M.lookup (flowUnitId f) unitDB)
                converted = if flowUnit == cfUnit || T.null cfUnit then qty
                            else fromMaybe qty (convertUnit unitConfig flowUnit cfUnit qty)
            in converted * cfVal

    lookupCF fid = case M.lookup fid (mtUuidCF tables) of
        Just cfv -> Just cfv
        Nothing  -> case M.lookup fid flowDB of
            Nothing   -> Nothing
            Just flow ->
                let name    = normalizeName (flowName flow)
                    baseMed = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ flowCategory flow
                    subcomp = T.toLower $ fromMaybe "" (flowSubcompartment flow)
                    exact   = M.lookup (name, baseMed, subcomp) (mtExactCF tables)
                in case exact of
                    Just _  -> exact
                    Nothing -> M.lookup (name, baseMed) (mtFallbackCF tables)

    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise               = m

-- | Back-compat wrapper: build tables on the fly. Prefer the cached path
-- ('mapMethodToTablesCached' + 'computeLCIAScoreFromTables') in hot loops.
computeLCIAScore :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> Double
computeLCIAScore unitConfig unitDB flowDB inventory mappings =
    computeLCIAScoreFromTables unitConfig unitDB flowDB inventory (buildMethodTables mappings)

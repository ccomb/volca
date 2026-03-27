{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , computeLCIAScore
      -- * Matching strategies
    , MatchStrategy(..)
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
import Types (Database(..), Flow(..), FlowDB)
import Method.Types
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
    mapM (\cf -> fmap (\r -> (cf, r)) (mapSingleFlow mappers ctx cf)) (methodFactors method)

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
    fmap (`pickByCompartment` mComp) (M.lookup cas flowsByCAS)

-- | Find flow by normalized name match (compartment-aware)
findFlowByName :: M.Map Text [Flow] -> Text -> Maybe Flow
findFlowByName flowsByName name = findFlowByNameComp flowsByName name Nothing

-- | Find flow by normalized name with compartment preference
findFlowByNameComp :: M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowByNameComp flowsByName name mComp =
    case M.lookup (normalizeName name) flowsByName of
        Just flows@(_:_) -> Just (pickByCompartment flows mComp)
        _                -> Nothing

-- | Find flow via synonym group (compartment-aware)
findFlowBySynonym :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Flow
findFlowBySynonym synDB flowsByName name = findFlowBySynonymComp synDB flowsByName name Nothing

-- | Find flow via synonym group with compartment preference
findFlowBySynonymComp :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowBySynonymComp synDB flowsByName name mComp =
    case lookupSynonymGroup synDB name of
        Nothing -> Nothing
        Just groupId ->
            case getSynonyms synDB groupId of
                Nothing -> Nothing
                Just synonyms ->
                    let candidates = concatMap (lookupFlows flowsByName) synonyms
                    in if null candidates then Nothing
                       else Just (pickByCompartment candidates mComp)
  where
    lookupFlows fbn syn = M.findWithDefault [] (normalizeName syn) fbn

-- | Pick the best flow match based on compartment preference.
pickByCompartment :: [Flow] -> Maybe Compartment -> Flow
pickByCompartment flows Nothing = head flows
pickByCompartment flows (Just comp) =
    case find (exactCompMatch comp) flows of
        Just f  -> f
        Nothing -> case find (mediumMatch comp) flows of
            Just f  -> f
            Nothing -> head flows
  where
    exactCompMatch (Compartment med sub _) f =
        let cat = T.toLower (flowCategory f)
            subcomp = maybe "" T.toLower (flowSubcompartment f)
        in matchMedium med cat && (T.null sub || sub == subcomp || sub `T.isInfixOf` subcomp)

    mediumMatch (Compartment med _ _) f =
        matchMedium med (T.toLower (flowCategory f))

    matchMedium med cat
        | T.null med  = True
        | med == cat  = True
        | med `T.isInfixOf` cat = True
        | otherwise   = False

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

-- | Compute LCIA score from inventory and flow mappings.
-- Scores each inventory flow by finding its best matching CF:
--   1. UUID-matched flows use the exact CF from the mapping.
--   2. For other flows, tries exact (name, medium, subcompartment) match.
--   3. Falls back to (name, medium) for CF entries with unspecified subcompartment.
-- This implements SimaPro's fallback behavior: "Air;(unspecified)" CFs apply to
-- any air emission that lacks a more specific CF, without aggregating across media.
computeLCIAScore :: FlowDB -> Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> Double
computeLCIAScore flowDB inventory mappings =
    sum [ scoreFlow fid qty | (fid, qty) <- M.toList inventory, qty /= 0 ]
  where
    scoreFlow fid qty = qty * fromMaybe 0 (lookupCF fid)

    lookupCF fid = case M.lookup fid uuidCF of
        Just cfv -> Just cfv
        Nothing  -> case M.lookup fid flowDB of
            Nothing   -> Nothing
            Just flow ->
                let name    = normalizeName (flowName flow)
                    baseMed = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ flowCategory flow
                    subcomp = T.toLower $ fromMaybe "" (flowSubcompartment flow)
                    exact   = M.lookup (name, baseMed, subcomp) exactCF
                    isLongTerm = "long-term" `T.isInfixOf` subcomp
                in case exact of
                    Just _  -> exact
                    Nothing -> if isLongTerm then Nothing
                               else M.lookup (name, baseMed) fallbackCF

    -- UUID-matched CFs: exact flow_id → CF_value
    uuidCF = M.fromList
        [ (flowId flow, mcfValue cf)
        | (cf, Just (flow, ByUUID)) <- mappings ]

    -- Exact CFs for specific subcompartments: (name, medium, subcomp) → max CF
    exactCF = M.fromListWith max
        [ ((nameKey cf mflow, normalizeMedium medium, subcomp), mcfValue cf)
        | (cf, mflow) <- mappings
        , Just (Compartment medium subcomp _) <- [mcfCompartment cf]
        , not (T.null subcomp) ]

    -- Fallback CFs for unspecified subcompartment: (name, medium) → max CF
    fallbackCF = M.fromListWith max
        [ ((nameKey cf mflow, normalizeMedium medium), mcfValue cf)
        | (cf, mflow) <- mappings
        , Just (Compartment medium subcomp _) <- [mcfCompartment cf]
        , T.null subcomp ]

    -- Use matched flow's name only for name/synonym matches (prevents CAS geographic
    -- variant contamination: "Ammonia, DE" CAS-matched to "Ammonia" DB flow must not
    -- override the "Ammonia" CF entry with the geographic variant's higher value)
    nameKey cf mflow = normalizeName $ case mflow of
        Just (flow, ByName)    -> flowName flow
        Just (flow, BySynonym) -> flowName flow
        _                      -> mcfFlowName cf

    -- Normalize medium names between method CFs and database flows
    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise               = m

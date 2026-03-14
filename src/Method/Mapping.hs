{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Flow Mapping Engine
--
-- Maps characterization factor flows from LCIA methods to database flows
-- using multiple matching strategies:
-- 1. UUID match (exact)
-- 2. CAS match (normalized CAS number)
-- 3. Name match (normalized, compartment-aware)
-- 4. Synonym match (via synonym group, compartment-aware)
module Method.Mapping
    ( -- * Mapping functions
      mapMethodFlows
    , mapMethodToFlows
    , mapSingleFlow
      -- * LCIA scoring
    , computeLCIAScore
      -- * Matching strategies
    , MatchStrategy(..)
    , findFlowByUUID
    , findFlowByName
    , findFlowBySynonym
    , findFlowByCAS
      -- * Statistics
    , MappingStats(..)
    , computeMappingStats
    ) where

import Data.Foldable (asum)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Matrix (Inventory)
import Types (Database(..), Flow(..))
import Method.Types
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

-- | Map all method flows to database flows
--
-- Returns a list of (MethodCF, Maybe (Flow, MatchStrategy)) pairs
mapMethodFlows
    :: SynonymDB
    -> M.Map UUID Flow       -- ^ Flow database indexed by UUID
    -> M.Map Text [Flow]     -- ^ Biosphere flows indexed by normalized name
    -> M.Map Text [Flow]     -- ^ Biosphere flows indexed by CAS number
    -> Method
    -> [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodFlows synDB flowsByUUID flowsByName flowsByCAS method =
    map (\cf -> (cf, mapSingleFlow synDB flowsByUUID flowsByName flowsByCAS cf))
        (methodFactors method)

-- | Map a single method CF to a database flow
--
-- Tries matching strategies in order:
-- 1. UUID match
-- 2. CAS match (with compartment preference)
-- 3. Name match (with compartment preference)
-- 4. Synonym match (with compartment preference)
mapSingleFlow
    :: SynonymDB
    -> M.Map UUID Flow
    -> M.Map Text [Flow]
    -> M.Map Text [Flow]
    -> MethodCF
    -> Maybe (Flow, MatchStrategy)
mapSingleFlow synDB flowsByUUID flowsByName flowsByCAS cf =
    let tag s f = fmap (\x -> (x, s)) f
    in asum
        [ tag ByUUID    $ findFlowByUUID flowsByUUID (mcfFlowRef cf)
        , tag ByCAS     $ mcfCAS cf >>= \cas -> findFlowByCAS flowsByCAS cas (mcfCompartment cf)
        , tag ByName    $ findFlowByNameComp flowsByName (mcfFlowName cf) (mcfCompartment cf)
        , tag BySynonym $ findFlowBySynonymComp synDB flowsByName (mcfFlowName cf) (mcfCompartment cf)
        ]

-- | Find flow by exact UUID match
findFlowByUUID :: M.Map UUID Flow -> UUID -> Maybe Flow
findFlowByUUID flowsByUUID uuid = M.lookup uuid flowsByUUID

-- | Find flow by CAS number with compartment preference
findFlowByCAS :: M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowByCAS flowsByCAS cas mComp =
    case M.lookup cas flowsByCAS of
        Just flows -> Just (pickByCompartment flows mComp)
        Nothing    -> Nothing

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
-- When multiple flows match by name or CAS, prefer the one matching the CF's compartment.
pickByCompartment :: [Flow] -> Maybe Compartment -> Flow
pickByCompartment flows Nothing = head flows
pickByCompartment flows (Just comp) =
    case find (exactCompMatch comp) flows of
        Just f  -> f
        Nothing -> case find (mediumMatch comp) flows of
            Just f  -> f
            Nothing -> head flows
  where
    -- Exact: same medium + subcompartment
    exactCompMatch (Compartment med sub _) f =
        let cat = T.toLower (flowCategory f)
            subcomp = maybe "" T.toLower (flowSubcompartment f)
        in matchMedium med cat && (T.null sub || sub == subcomp || sub `T.isInfixOf` subcomp)

    -- Medium only: "air", "water", "soil"
    mediumMatch (Compartment med _ _) f =
        matchMedium med (T.toLower (flowCategory f))

    -- Check if medium matches the flow category (handles "air/urban" etc.)
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

-- | Convenience wrapper: map method CFs to database flows using DB indexes
mapMethodToFlows :: Database -> Method -> [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodToFlows db method =
    let synDB = maybe emptySynonymDB id (dbSynonymDB db)
    in mapMethodFlows synDB (dbFlows db) (dbFlowsByName db) (dbFlowsByCAS db) method

-- | Compute LCIA score from inventory and flow mappings
--
-- For each mapped flow: score += inventory[flow_uuid] * CF_value
computeLCIAScore :: Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> Double
computeLCIAScore inventory mappings =
    sum [qty * mcfValue cf | (cf, Just (flow, _)) <- mappings
                           , Just qty <- [M.lookup (flowId flow) inventory]]


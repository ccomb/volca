{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Flow Mapping Engine

Maps characterization factor flows from LCIA methods to database flows
using a configurable cascade of MapperHandles (plugin architecture).
Default cascade: UUID → CAS → Name → Synonym.
-}
module Method.Mapping (
    -- * Mapping functions
    mapMethodFlows,
    mapMethodToFlows,
    mapSingleFlow,
    buildMapContext,

    -- * LCIA scoring
    MethodTables (..),
    buildMethodTables,
    computeLCIAScore,
    computeLCIAScoreFromTables,
    inventoryContributions,
    processContributionsFromTables,

    -- * Matching strategies
    MatchStrategy (..),
    strategyFromText,
    findFlowByUUID,
    findFlowByName,
    findFlowByNameComp,
    findFlowBySynonym,
    findFlowBySynonymComp,
    findFlowByCAS,

    -- * Statistics
    MappingStats (..),
    computeMappingStats,
) where

import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Matrix (Inventory)
import qualified Matrix
import Method.Types
import Plugin.Types (MapContext (..), MapQuery (..), MapResult (..), MapperHandle (..))
import SynonymDB
import Types (Database (..), Flow (..), FlowDB, ProcessId, SparseTriple (..), Unit (..), UnitDB)
import UnitConversion (UnitConfig, convertUnit)

-- | Matching strategy used to find a flow
data MatchStrategy
    = -- | Exact UUID match
      ByUUID
    | -- | CAS number match
      ByCAS
    | -- | Normalized name match
      ByName
    | -- | Via synonym group
      BySynonym
    | -- | Fuzzy string matching
      ByFuzzy
    | -- | No match found
      NoMatch
    deriving (Eq, Show)

-- | Statistics about mapping results
data MappingStats = MappingStats
    { msTotal :: !Int
    -- ^ Total CFs in method
    , msByUUID :: !Int
    -- ^ Matched by UUID
    , msByCAS :: !Int
    -- ^ Matched by CAS
    , msByName :: !Int
    -- ^ Matched by name
    , msBySynonym :: !Int
    -- ^ Matched by synonym
    , msByFuzzy :: !Int
    -- ^ Matched by fuzzy
    , msUnmatched :: !Int
    -- ^ Not matched
    }
    deriving (Eq, Show)

-- | Build a MapContext from a Database (convenience for callers)
buildMapContext :: Database -> MapContext
buildMapContext db =
    MapContext
        { mcFlowsByUUID = dbFlows db
        , mcFlowsByName = dbFlowsByName db
        , mcFlowsByCAS = dbFlowsByCAS db
        , mcSynonymDB = fromMaybe emptySynonymDB (dbSynonymDB db)
        , mcActivities = M.empty
        }

{- | Map all method flows to database flows using mapper handles.
Mappers are tried in priority order (assumed pre-sorted).
-}
mapMethodFlows ::
    [MapperHandle] ->
    MapContext ->
    Method ->
    IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodFlows mappers ctx method =
    mapM (\cf -> fmap (cf,) (mapSingleFlow mappers ctx cf)) (methodFactors method)

{- | Map a single CF using the mapper handle cascade.
Each mapper is tried in order; the first match wins.
-}
mapSingleFlow ::
    [MapperHandle] ->
    MapContext ->
    MethodCF ->
    IO (Maybe (Flow, MatchStrategy))
mapSingleFlow mappers ctx cf = go mappers
  where
    go [] = pure Nothing
    go (m : ms) = do
        result <- mhMatch m ctx (MatchCF cf)
        case result of
            Just mr
                | Just flow <- M.lookup (mrTargetId mr) (mcFlowsByUUID ctx) ->
                    pure $ Just (flow, strategyFromText (mrStrategy mr))
            _ -> go ms

-- | Convenience wrapper: map method CFs using the given mappers + DB.
mapMethodToFlows :: [MapperHandle] -> Database -> Method -> IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodToFlows mappers db = mapMethodFlows mappers (buildMapContext db)

-- | Convert strategy text back to MatchStrategy
strategyFromText :: Text -> MatchStrategy
strategyFromText t = case T.toLower t of
    "uuid" -> ByUUID
    "cas" -> ByCAS
    "name" -> ByName
    "synonym" -> BySynonym
    "fuzzy" -> ByFuzzy
    _ -> ByFuzzy -- Unknown strategies map to fuzzy

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
        Nothing -> Nothing
        Just gid ->
            getSynonyms synDB gid >>= \synonyms ->
                pickByCompartment (concatMap (lookupFlows flowsByName) synonyms) mComp
  where
    lookupFlows fbn syn = M.findWithDefault [] (normalizeName syn) fbn

{- | Pick the best flow match based on compartment preference.
Returns Nothing for an empty candidate list.
-}
pickByCompartment :: [Flow] -> Maybe Compartment -> Maybe Flow
pickByCompartment [] _ = Nothing
pickByCompartment (f : _) Nothing = Just f
pickByCompartment (f : fs) (Just comp) = Just $
    case find (exactCompMatch comp) (f : fs) of
        Just m -> m
        Nothing -> fromMaybe f (find (mediumMatch comp) (f : fs))
  where
    exactCompMatch (Compartment med sub _) fl =
        let cat = T.toLower (flowCategory fl)
            subcomp = maybe "" T.toLower (flowSubcompartment fl)
         in matchMedium med cat && (T.null sub || sub == subcomp || sub `T.isInfixOf` subcomp)

    mediumMatch (Compartment med _ _) fl =
        matchMedium med (T.toLower (flowCategory fl))

    matchMedium med cat
        | T.null med = True
        | med == cat = True
        | med `T.isInfixOf` cat = True
        | otherwise = False

-- | Compute statistics about mapping results
computeMappingStats :: [(MethodCF, Maybe (Flow, MatchStrategy))] -> MappingStats
computeMappingStats mappings =
    MappingStats
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

{- | Precomputed CF lookup tables for one (database, method) pair.
Building these from raw mappings is O(n log n) over thousands of CFs, so they
should be computed once per method and reused across inventories.
-}
data MethodTables = MethodTables
    { mtUuidCF :: !(M.Map UUID (Double, Text))
    -- ^ UUID-matched CFs: exact flow id → (CF value, CF unit)
    , mtExactCF :: !(M.Map (Text, Text, Text) (Double, Text))
    -- ^ (normalized name, medium, subcompartment) → (CF, unit)
    , mtFallbackCF :: !(M.Map (Text, Text) (Double, Text))
    -- ^ (normalized name, medium) → (CF, unit) for entries with unspecified subcompartment
    }

-- | Build 'MethodTables' from raw mappings. Run once per (db, method).
buildMethodTables :: [(MethodCF, Maybe (Flow, MatchStrategy))] -> MethodTables
buildMethodTables mappings =
    MethodTables
        { mtUuidCF =
            M.fromList
                [ (flowId flow, (mcfValue cf, mcfUnit cf))
                | (cf, Just (flow, ByUUID)) <- mappings
                ]
        , mtExactCF =
            stripStrategy $
                M.fromListWith
                    preferBetter
                    [ ((nameKey cf mflow, normalizeMedium medium, subcomp), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Just (Compartment medium subcomp _) <- [mcfCompartment cf]
                    , not (T.null subcomp)
                    ]
        , mtFallbackCF =
            stripStrategy $
                M.fromListWith
                    preferBetter
                    [ ((nameKey cf mflow, normalizeMedium medium), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Just (Compartment medium subcomp _) <- [mcfCompartment cf]
                    , T.null subcomp
                    ]
        }
  where
    stripStrategy = M.map (\(v, u, _) -> (v, u))

    preferBetter (v1, u1, s1) (v2, u2, s2)
        | stratPriority s1 < stratPriority s2 = (v1, u1, s1)
        | stratPriority s1 > stratPriority s2 = (v2, u2, s2)
        | v1 >= v2 = (v1, u1, s1)
        | otherwise = (v2, u2, s2)
    stratPriority ByUUID = 0 :: Int
    stratPriority ByCAS = 1
    stratPriority ByName = 2
    stratPriority BySynonym = 3
    stratPriority _ = 4

    matchStrategy mflow = case mflow of
        Just (_, s) -> s
        Nothing -> NoMatch

    -- Use matched flow's name only for name/synonym matches
    nameKey cf mflow = normalizeName $ case mflow of
        Just (flow, ByName) -> flowName flow
        Just (flow, BySynonym) -> flowName flow
        _ -> mcfFlowName cf

    -- Normalize medium names between method CFs and database flows
    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise = m

{- | Score an inventory against precomputed 'MethodTables'.
Hot path: O(|inventory|) per call, no map construction.
-}
computeLCIAScoreFromTables :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> MethodTables -> Double
computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables =
    sum [scoreFlow fid qty | (fid, qty) <- M.toList inventory, qty /= 0]
  where
    scoreFlow fid qty = case lookupCF fid of
        Nothing -> 0
        Just (cfVal, cfUnit) ->
            let flowUnit = maybe "" unitName (M.lookup fid flowDB >>= \f -> M.lookup (flowUnitId f) unitDB)
                converted =
                    if flowUnit == cfUnit || T.null cfUnit
                        then qty
                        else fromMaybe qty (convertUnit unitConfig flowUnit cfUnit qty)
             in converted * cfVal

    lookupCF fid = case M.lookup fid (mtUuidCF tables) of
        Just cfv -> Just cfv
        Nothing -> case M.lookup fid flowDB of
            Nothing -> Nothing
            Just flow ->
                let name = normalizeName (flowName flow)
                    baseMed = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ flowCategory flow
                    subcomp = T.toLower $ fromMaybe "" (flowSubcompartment flow)
                    exact = M.lookup (name, baseMed, subcomp) (mtExactCF tables)
                 in case exact of
                        Just _ -> exact
                        Nothing -> M.lookup (name, baseMed) (mtFallbackCF tables)

    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise = m

{- | Back-compat wrapper: build tables on the fly. Prefer the cached path
('mapMethodToTablesCached' + 'computeLCIAScoreFromTables') in hot loops.
-}
computeLCIAScore :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> Double
computeLCIAScore unitConfig unitDB flowDB inventory mappings =
    computeLCIAScoreFromTables unitConfig unitDB flowDB inventory (buildMethodTables mappings)

{- | Per-flow contributions over an 'Inventory', keyed by flow UUID (possibly
cross-DB-merged). Walks the inventory directly (not the mappings) so any
flow with a matchable CF contributes — including flows from dep DBs that
don't appear in the root-DB method mapping.

Returns @(contributions, unknownUuids)@:
  * @contributions@ — @[(flow, cfValue, contributionInMethodUnit)]@
    for every non-zero inventory entry whose UUID resolves in 'flowDB'
    AND whose (name, medium, subcompartment) matches a CF.
  * @unknownUuids@ — non-zero inventory UUIDs with no record in 'flowDB'.
    Callers should surface these (per the "no silent errors" rule): a
    missing record means the merged metadata is incomplete for this
    inventory and some flows are invisible to characterization.

Flows that resolve in 'flowDB' but have no matching CF are legitimately
uncharacterized and silently omitted — that matches the behaviour of
'computeLCIAScoreFromTables' and is not a data-integrity concern.
-}
inventoryContributions ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Inventory ->
    MethodTables ->
    ([(Flow, Double, Double)], [UUID])
inventoryContributions unitConfig unitDB flowDB inventory tables =
    M.foldlWithKey' step ([], []) inventory
  where
    -- Strict fold over the inventory Map: the old 'foldr' over 'M.toList'
    -- built a thunk chain the size of the inventory on every call, and
    -- characterization runs this 27-wide over K-activity batches — the
    -- dominant garbage source. Strict pair + accumulator prevents the leak.
    -- Result list order is reversed vs. the old version, but every caller
    -- already 'sortOn's by |contribution|.
    step (!contribs, !unknowns) fid qty
        | qty == 0 = (contribs, unknowns)
        | otherwise = case M.lookup fid flowDB of
            Nothing -> (contribs, fid : unknowns) -- metadata missing — surface it
            Just flow -> case lookupCF fid flow of
                Nothing -> (contribs, unknowns) -- no CF match — legitimately uncharacterized
                Just (cfVal, cfUnit) ->
                    let flowUnit = maybe "" unitName (M.lookup (flowUnitId flow) unitDB)
                        converted =
                            if flowUnit == cfUnit || T.null cfUnit
                                then qty
                                else fromMaybe qty (convertUnit unitConfig flowUnit cfUnit qty)
                        !contribution = converted * cfVal
                     in ((flow, cfVal, contribution) : contribs, unknowns)

    lookupCF fid flow = case M.lookup fid (mtUuidCF tables) of
        Just cfv -> Just cfv
        Nothing ->
            let name = normalizeName (flowName flow)
                baseMed = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ flowCategory flow
                subcomp = T.toLower $ fromMaybe "" (flowSubcompartment flow)
                exact = M.lookup (name, baseMed, subcomp) (mtExactCF tables)
             in case exact of
                    Just _ -> exact
                    Nothing -> M.lookup (name, baseMed) (mtFallbackCF tables)

    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise = m

{- | Per-process LCIA contributions for one DB + one method, driven by
'MethodTables' + a merged 'FlowDB'. Mirrors
'Matrix.computeProcessLCIAContributions' but lets dep-DB flows land a CF
via (name, medium, subcompartment) fallback — same lookup path as
'inventoryContributions' — so this helper can be called per-DB while
walking a cross-DB dependency graph and still characterize every flow.

Iterates @dbBiosphereTriples db@; for each @(flowRow, colIdx, bioVal)@ it
attributes @bioVal * scaling[col] * CF@ (in the method's unit, after
flow-unit → CF-unit conversion) to the process owning @colIdx@.
-}
processContributionsFromTables ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Database ->
    Matrix.Vector ->
    MethodTables ->
    M.Map ProcessId Double
processContributionsFromTables unitConfig unitDB flowDB db scalingVec tables =
    U.foldl' step M.empty (dbBiosphereTriples db)
  where
    actIdx = dbActivityIndex db
    bioFlows = dbBiosphereFlows db
    nFlows = V.length bioFlows
    nActs = V.length actIdx

    -- Precompute the effective CF (CF value × flow→CF unit conversion factor)
    -- per biosphere-matrix row so the triple loop becomes pure arithmetic.
    -- O(|bioFlows|) once, vs O(|triples| × map-lookups) before.
    effectiveCF :: U.Vector Double
    effectiveCF = U.generate nFlows $ \i ->
        let flowUUID = bioFlows V.! i
         in case M.lookup flowUUID flowDB of
                Nothing -> 0
                Just flow -> case lookupCF flowUUID flow of
                    Nothing -> 0
                    Just (cfVal, cfUnit) ->
                        let flowUnit = maybe "" unitName (M.lookup (flowUnitId flow) unitDB)
                            factor =
                                if flowUnit == cfUnit || T.null cfUnit
                                    then 1.0
                                    else fromMaybe 1.0 (convertUnit unitConfig flowUnit cfUnit 1.0)
                         in factor * cfVal

    -- Invert dbActivityIndex (pid -> col) into (col -> pid) as an unboxed
    -- vector for O(1) per-triple lookup. Assumes matrix cols are dense in
    -- [0..nActs-1], which matches the existing index construction.
    colToProc :: U.Vector Int
    colToProc = U.create $ do
        mv <- MU.replicate nActs (-1 :: Int)
        V.imapM_ (\pid col -> MU.write mv (fromIntegral col) pid) actIdx
        pure mv

    step acc (SparseTriple flowRow colIdx bioVal) =
        let cf = effectiveCF U.! fromIntegral flowRow
         in if cf == 0
                then acc
                else
                    let colI = fromIntegral colIdx :: Int
                        pid = colToProc U.! colI
                     in if pid < 0
                            then acc
                            else
                                let scale = scalingVec U.! colI
                                    pid' = fromIntegral pid :: ProcessId
                                 in M.insertWith (+) pid' (bioVal * scale * cf) acc

    lookupCF fid flow = case M.lookup fid (mtUuidCF tables) of
        Just cfv -> Just cfv
        Nothing ->
            let name = normalizeName (flowName flow)
                baseMed = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ flowCategory flow
                subcomp = T.toLower $ fromMaybe "" (flowSubcompartment flow)
                exact = M.lookup (name, baseMed, subcomp) (mtExactCF tables)
             in case exact of
                    Just _ -> exact
                    Nothing -> M.lookup (name, baseMed) (mtFallbackCF tables)

    normalizeMedium m
        | m == "natural resource" = "resource"
        | otherwise = m

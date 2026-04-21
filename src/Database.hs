{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Database where

import Data.Either (lefts, rights)
import Data.Int (Int32)
import qualified Data.IntSet as IS
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Progress
import qualified Search.BM25.Types as BM25T
import qualified Search.Fuzzy as Fuzzy
import qualified Search.Normalize as Normalize
import Types
import UnitConversion (UnitConfig, convertUnit, normalizeUnit)

{- | Build complete database with pre-computed sparse matrices

SIGN CONVENTION:
- Technosphere triplets are stored as POSITIVE values (input coefficients per unit output)
- Matrix.hs negates these when constructing (I-A) system matrix for solving
- The biosphere matrix stores ALL flows as POSITIVE (emissions AND resource extractions)
- Resource extractions represent "outputs" from nature into the technosphere (positive like emissions)
- This follows Ecoinvent convention where B matrix contains positive values for all environmental flows

Matrix Construction:
- Accepts a Map with (UUID, UUID) keys and converts to Vector internally
- Builds sparse triplets for technosphere (A) and biosphere (B) matrices
- Normalizes exchanges by NET reference product amounts (gross output - internal consumption)
- SELF-LOOPS (internal consumption) are EXCLUDED from matrix triplets but affect normalization
- Example: Electricity market with 1.0 kWh output and 0.012 kWh internal loss
  * Normalization factor: 1.0 - 0.012 = 0.988 kWh (net output)
  * All inputs divided by 0.988, giving ~1.2% increase in coefficients
  * Self-loop NOT exported as matrix entry (matches Ecoinvent convention)
- Solver constructs (I-A) by adding identity and negating technosphere triplets
-}
buildDatabaseWithMatrices :: UnitConfig -> M.Map (UUID, UUID) Activity -> FlowDB -> UnitDB -> IO (Either Text Database)
buildDatabaseWithMatrices unitConfig activityMap flowDB unitDB = do
    reportMatrixOperation "Building database with pre-computed sparse matrices"

    -- Step 1: Build UUID interning tables from Map keys
    let activityKeys = M.keys activityMap
        sortedKeys = sort activityKeys -- Ensure deterministic ordering

        -- Build forward lookup: ProcessId (Int32) -> (UUID, UUID)
        dbProcessIdTable = V.fromList sortedKeys

        -- Build reverse lookup: (UUID, UUID) -> ProcessId (Int32)
        dbProcessIdLookup = M.fromList $ zip sortedKeys [0 ..]

        -- Build activity UUID index: UUID -> ProcessId (for O(1) lookups)
        dbActivityUUIDIndex = M.fromList [(actUUID, pid) | (pid, (actUUID, _)) <- zip [0 ..] sortedKeys]

        -- Build activity products index: UUID -> [ProcessId] (for multi-product activities)
        dbActivityProductsIndex = M.fromListWith (++) [(actUUID, [pid]) | (pid, (actUUID, _)) <- zip [0 ..] sortedKeys]

        -- Build activity-product lookup for correct multi-output handling
        -- Maps (activityUUID, productFlowUUID) -> ProcessId
        -- This ensures exchanges link to the correct product in multi-output activities
        activityProductLookup = M.fromList [((actUUID, prodUUID), pid) | (pid, (actUUID, prodUUID)) <- zip [0 ..] sortedKeys]

        -- Convert Map to Vector indexed by ProcessId
        dbActivities = V.fromList [activityMap M.! key | key <- sortedKeys]

        -- Build indexes (now using Vector)
        indexes = buildIndexesWithProcessIds dbActivities dbProcessIdTable flowDB

        -- Build supplier reference unit lookup: ProcessId -> unit name of reference product
        -- Used to convert exchange amounts to the supplier's unit for correct A-matrix coefficients
        supplierRefUnits =
            V.map
                ( \act ->
                    let refExs = [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)]
                     in case refExs of
                            (ex : _) -> getUnitNameForExchange unitDB ex
                            [] -> ""
                )
                dbActivities

    -- Build activity index for matrix construction
    reportMatrixOperation "Building activity indexes"
    let activityCount = fromIntegral (V.length dbActivities) :: Int32

    -- Note: ProcessId is already the matrix index (identity mapping removed for performance)
    reportMatrixOperation ("Activity index built: " ++ show activityCount ++ " activities")

    -- Build technosphere sparse triplets
    reportMatrixOperation "Building technosphere matrix triplets"
    let buildTechTriple normalizationFactor j consumerActivity _consumerPid ex
            | not (isTechnosphereExchange ex) = Right ([], [])
            | exchangeIsReference ex = Right ([], []) -- reference product is on the diagonal
            | otherwise =
                let producerResult = case exchangeProcessLinkId ex of
                        Just pid -> (Just pid, [])
                        Nothing -> case exchangeActivityLinkId ex of
                            Just actUUID ->
                                case M.lookup (actUUID, exchangeFlowId ex) activityProductLookup of
                                    Just pid -> (Just pid, [])
                                    Nothing ->
                                        -- Only warn if exchange has non-zero amount (zero-amount are placeholders)
                                        let warning =
                                                [ "Missing activity-product pair referenced by exchange:\n"
                                                    ++ "  Activity UUID: "
                                                    ++ T.unpack (UUID.toText actUUID)
                                                    ++ "\n"
                                                    ++ "  Product UUID: "
                                                    ++ T.unpack (UUID.toText (exchangeFlowId ex))
                                                    ++ "\n"
                                                    ++ "  Consumer: "
                                                    ++ T.unpack (activityName consumerActivity)
                                                    ++ "\n"
                                                    ++ "  Expected file: "
                                                    ++ T.unpack (UUID.toText actUUID)
                                                    ++ "_"
                                                    ++ T.unpack (UUID.toText (exchangeFlowId ex))
                                                    ++ ".spold\n"
                                                    ++ "  This exchange will be skipped."
                                                | abs (exchangeAmount ex) > 1e-15
                                                ]
                                         in (Nothing, warning)
                            Nothing -> (Nothing, [])
                    (producerPid, warnings) = producerResult
                    producerIdx =
                        producerPid >>= \pid ->
                            if pid >= 0 && fromIntegral pid < activityCount
                                then Just $ fromIntegral pid
                                else Nothing
                 in case producerIdx of
                        Just idx ->
                            let rawValue = exchangeAmount ex
                                exchangeUnit = getUnitNameForExchange unitDB ex
                                supplierUnit = supplierRefUnits V.! fromIntegral idx
                                needsConversion =
                                    normalizeUnit exchangeUnit /= normalizeUnit supplierUnit
                                        && not (T.null exchangeUnit)
                                        && not (T.null supplierUnit)
                             in case (needsConversion, convertUnit unitConfig exchangeUnit supplierUnit rawValue) of
                                    (True, Nothing) ->
                                        Left $
                                            "Unknown unit conversion: \""
                                                <> exchangeUnit
                                                <> "\" \8594 \""
                                                <> supplierUnit
                                                <> "\" in "
                                                <> activityName consumerActivity
                                                <> " \8212 add these units to [[units]] CSV"
                                    _ ->
                                        let convertedValue = case (needsConversion, convertUnit unitConfig exchangeUnit supplierUnit rawValue) of
                                                (True, Just v) -> v
                                                _ -> rawValue
                                            denom =
                                                if normalizationFactor > 1e-15
                                                    then normalizationFactor
                                                    else 1.0
                                            sign = if exchangeIsInput ex then 1 else -1
                                            value = sign * convertedValue / denom
                                         in Right ([SparseTriple idx j value | abs value > 1e-15, idx /= j], warnings)
                        Nothing -> Right ([], warnings)

        buildActivityTriplets (j, consumerPid) =
            let consumerActivity = dbActivities V.! fromIntegral consumerPid
                consumerKey = dbProcessIdTable V.! fromIntegral consumerPid
                normalizationFactor = activityNormFactor consumerActivity consumerKey
                buildNormalizedTechTriple = buildTechTriple normalizationFactor j consumerActivity consumerPid
                results = map buildNormalizedTechTriple (exchanges consumerActivity)
             in -- Short-circuit on first Left (unit conversion error)
                case lefts results of
                    (err : _) -> Left err
                    [] -> let rs = rights results in Right (concatMap fst rs, concatMap snd rs)

    -- Collect results, failing on first unit conversion error
    let activityRange = [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1 :: ProcessId]]
        activityResults = map buildActivityTriplets activityRange
    case lefts activityResults of
        (err : _) -> return $ Left err
        [] -> do
            let allResults = rights activityResults
                !techTriples = VU.fromList $ concatMap fst allResults
                techWarnings = concatMap snd allResults

            -- Emit warnings in IO context
            mapM_ (reportProgress Warning) techWarnings

            reportMatrixOperation ("Technosphere matrix: " ++ show (VU.length techTriples) ++ " non-zero entries")

            -- Build biosphere sparse triplets
            reportMatrixOperation "Building biosphere matrix triplets"
            let bioFlowUUIDs =
                    V.fromList $
                        sort $
                            S.toList $
                                S.fromList
                                    [exchangeFlowId ex | pid <- [0 .. fromIntegral activityCount - 1 :: Int], let act = dbActivities V.! pid, ex <- exchanges act, isBiosphereExchange ex]
                bioFlowCount = fromIntegral $ V.length bioFlowUUIDs :: Int32
                bioFlowIndex = M.fromList $ zip (V.toList bioFlowUUIDs) [0 ..]

                !bioTriples =
                    let buildBioTriple normalizationFactor j _activity ex
                            | not (isBiosphereExchange ex) = []
                            | otherwise =
                                case M.lookup (exchangeFlowId ex) bioFlowIndex of
                                    Just i ->
                                        let rawValue = exchangeAmount ex
                                            denom = if normalizationFactor > 1e-15 then normalizationFactor else 1.0
                                            value = rawValue / denom
                                         in [SparseTriple i j value | abs value > 1e-15]
                                    Nothing -> []

                        buildActivityBioTriplets (j, pid) =
                            let activity = dbActivities V.! fromIntegral pid
                                activityKey = dbProcessIdTable V.! fromIntegral pid
                                normalizationFactor = activityNormFactor activity activityKey
                             in concatMap (buildBioTriple normalizationFactor j activity) (exchanges activity)
                     in VU.fromList $ concatMap buildActivityBioTriplets activityRange

            reportMatrixOperation ("Biosphere matrix: " ++ show (VU.length bioTriples) ++ " non-zero entries")
            reportMatrixOperation "Database with matrices built successfully"
            reportMatrixOperation ("Final matrix stats: " ++ show (VU.length techTriples) ++ " tech entries, " ++ show (VU.length bioTriples) ++ " bio entries")

            reportMatrixOperation "Building product index"
            let !productIndex = buildProductIndex dbActivities dbProcessIdTable flowDB
            reportMatrixOperation ("Product index: " ++ show (M.size (piByUUID productIndex)) ++ " products indexed")

            return $
                Right
                    Database
                        { dbProcessIdTable = dbProcessIdTable
                        , dbProcessIdLookup = dbProcessIdLookup
                        , dbActivityUUIDIndex = dbActivityUUIDIndex
                        , dbActivityProductsIndex = dbActivityProductsIndex
                        , dbProductIndex = productIndex
                        , dbActivities = dbActivities
                        , dbFlows = flowDB
                        , dbUnits = unitDB
                        , dbIndexes = indexes
                        , dbTechnosphereTriples = techTriples
                        , dbBiosphereTriples = bioTriples
                        , dbActivityIndex = V.generate (fromIntegral activityCount) fromIntegral
                        , dbBiosphereFlows = bioFlowUUIDs
                        , dbActivityCount = activityCount
                        , dbBiosphereCount = bioFlowCount
                        , dbCrossDBLinks = []
                        , dbDependsOn = []
                        , dbLinkingStats = emptyCrossDBLinkingStats
                        , dbSynonymDB = Nothing
                        , dbFlowsByName = M.empty
                        , dbFlowsByCAS = M.empty
                        , dbProductSearchIndex = M.empty
                        , dbBM25Index = Nothing
                        }

-- | Build indexes with ProcessIds
buildIndexesWithProcessIds :: V.Vector Activity -> V.Vector (UUID, UUID) -> FlowDB -> Indexes
buildIndexesWithProcessIds activityVec processIdTable flowDB =
    let
        -- Convert Vector to temporary Map for index building
        -- We use the ProcessId-to-UUID mapping for lookups
        activityUUIDs = [actUUID | (actUUID, _) <- V.toList processIdTable]
        activities = V.toList activityVec
        activityPairs = zip activityUUIDs activities

        -- Build indexes using activity UUIDs
        nameIdx =
            M.fromListWith
                (++)
                [(T.toLower (activityName activity), [uuid]) | (uuid, activity) <- activityPairs]

        locationIdx =
            M.fromListWith
                (++)
                [(activityLocation activity, [uuid]) | (uuid, activity) <- activityPairs]

        flowIdx =
            M.fromListWith
                (++)
                [ (exchangeFlowId ex, [uuid]) | (uuid, activity) <- activityPairs, ex <- exchanges activity
                ]

        unitIdx =
            M.fromListWith
                (++)
                [(activityUnit activity, [uuid]) | (uuid, activity) <- activityPairs]

        flowCatIdx =
            M.fromListWith
                (++)
                [(flowCategory flow, [flowId]) | (flowId, flow) <- M.toList flowDB]

        flowTypeIdx =
            M.fromListWith
                (++)
                [(flowType flow, [flowId]) | (flowId, flow) <- M.toList flowDB]
     in
        -- Memory optimization: Removed exchange indexes (exchangeIdx, procExchangeIdx, refProdIdx,
        -- inputIdx, outputIdx) that duplicated 600K Exchange records across 5 maps.
        -- Exchanges can be accessed directly from Activity.exchanges when needed.
        -- This saves ~3-4GB of RAM on Ecoinvent 3.12.

        Indexes
            { idxByName = nameIdx
            , idxByLocation = locationIdx
            , idxByFlow = flowIdx
            , idxByUnit = unitIdx
            , idxFlowByCategory = flowCatIdx
            , idxFlowByType = flowTypeIdx
            }

{- | Build ProductIndex for product-based lookups
Used for: (1) SimaPro upstream link resolution, (2) future product search
Maps product flow UUIDs and names to the activities that produce them
-}
buildProductIndex :: V.Vector Activity -> V.Vector (UUID, UUID) -> FlowDB -> ProductIndex
buildProductIndex activities processIdTable flowDb =
    let
        -- Build list of (ProcessId, productUUID, productName, location) for each activity
        entries =
            [ (pid, prodUUID, prodName, actLoc)
            | (pid, (_, prodUUID)) <- zip [0 ..] (V.toList processIdTable)
            , let act = activities V.! fromIntegral pid
            , let actLoc = activityLocation act
            , Just flow <- [M.lookup prodUUID flowDb]
            , let prodName = T.toLower (flowName flow)
            ]
     in
        ProductIndex
            { piByUUID = M.fromList [(prodUUID, pid) | (pid, prodUUID, _, _) <- entries]
            , piByName = M.fromListWith (++) [(name, [pid]) | (pid, _, name, _) <- entries]
            , piByLocation = M.fromListWith (++) [(loc, [pid]) | (pid, _, _, loc) <- entries, not (T.null loc)]
            }

-- | Multi-word AND match: all words must appear in at least one of the given text fields (substring).
allWordsMatch :: Text -> (Activity -> [Text]) -> Activity -> Bool
allWordsMatch query getFields a =
    let searchWords = filter (not . T.null) $ T.words (T.toLower query)
        fields = map T.toLower (getFields a)
     in all (\w -> any (T.isInfixOf w) fields) searchWords

-- | Resolve a set of indices to (ProcessId, Activity) pairs against the activity vector.
resolveActivityIds :: V.Vector Activity -> IS.IntSet -> [(ProcessId, Activity)]
resolveActivityIds actVec ids =
    [(fromIntegral i, actVec V.! i) | i <- IS.toList ids, i < V.length actVec]

{- | Name-only candidate lookup. Does NOT touch geo/product/classification.
Non-exact path routes through the BM25 vocabulary + fuzzy expansion so
typos and stems still retrieve activities. Exact path is a linear scan
for case-insensitive full-name equality.
-}
findActivityNameCandidates :: Database -> Maybe Text -> Bool -> [(ProcessId, Activity)]
findActivityNameCandidates db Nothing _ = allActivities (dbActivities db)
findActivityNameCandidates db (Just name) True = exactNameMatches (dbActivities db) name
findActivityNameCandidates db (Just name) False =
    case dbBM25Index db of
        Just idx -> resolveActivityIds (dbActivities db) (bm25DocsMatchingName idx name)
        Nothing -> fullScanNameMatches (dbActivities db) name

allActivities :: V.Vector Activity -> [(ProcessId, Activity)]
allActivities actVec =
    [(fromIntegral i, a) | (i, a) <- zip [(0 :: Int) ..] (V.toList actVec)]

exactNameMatches :: V.Vector Activity -> Text -> [(ProcessId, Activity)]
exactNameMatches actVec name =
    [pair | pair@(_, a) <- allActivities actVec, T.toCaseFold (activityName a) == nameFold]
  where
    nameFold = T.toCaseFold name

fullScanNameMatches :: V.Vector Activity -> Text -> [(ProcessId, Activity)]
fullScanNameMatches actVec name =
    [pair | pair@(_, a) <- allActivities actVec, allWordsMatch name (\a' -> [activityName a']) a]

{- | Docs whose BM25 postings cover every query token (AND), allowing any
fuzzy expansion of a token to satisfy that token (OR within a token).
-}
bm25DocsMatchingName :: BM25T.BM25Index -> Text -> IS.IntSet
bm25DocsMatchingName idx name =
    intersectAll (map docsForGroup groups)
  where
    groups = Fuzzy.expandTokensGrouped idx (Normalize.tokenize name)
    docsForGroup g = IS.unions [docsForToken t | (t, _) <- g]
    docsForToken t = case M.lookup t (BM25T.bm25Postings idx) of
        Nothing -> IS.empty
        Just postings -> IS.fromList [docId | (docId, _) <- VU.toList postings]
    intersectAll [] = IS.empty
    intersectAll (x : xs) = foldl IS.intersection x xs

{- | Apply geo, product, and classification filters to a pre-built candidate list.
Does NOT touch the name query — callers (BM25 retrieval or name-candidate lookup)
produce the initial list.
-}
applyStructuredFilters ::
    Database ->
    -- | geo
    Maybe Text ->
    -- | product
    Maybe Text ->
    -- | classification filters
    [(Text, Text, Bool)] ->
    -- | exactMatch (affects geo equality)
    Bool ->
    [(ProcessId, Activity)] ->
    [(ProcessId, Activity)]
applyStructuredFilters db geoParam productParam classFilters exactMatch candidates =
    let actVec = dbActivities db
        pidx = dbProductSearchIndex db

        -- geography
        geoFiltered = case geoParam of
            Nothing -> candidates
            Just geo
                | exactMatch ->
                    let geoFold = T.toCaseFold geo
                     in [(pid, a) | (pid, a) <- candidates, T.toCaseFold (activityLocation a) == geoFold]
                | otherwise ->
                    let geoLower = T.toLower geo
                     in [(pid, a) | (pid, a) <- candidates, T.isInfixOf geoLower (T.toLower (activityLocation a))]

        getProductNames a' =
            [ flowName flow
            | ex <- exchanges a'
            , exchangeIsReference ex
            , not (exchangeIsInput ex)
            , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
            ]

        productFiltered = case productParam of
            Nothing -> geoFiltered
            Just prod
                | M.null pidx ->
                    [(pid, a) | (pid, a) <- geoFiltered, allWordsMatch prod getProductNames a]
                | otherwise ->
                    let searchWords = filter (not . T.null) $ T.words (T.toLower prod)
                        wordCandidates w = IS.unions [ids | (key, ids) <- M.toList pidx, T.isInfixOf w key]
                        candidateSet = case map wordCandidates searchWords of
                            [] -> IS.fromList (map (fromIntegral . fst) geoFiltered)
                            (first : rest) -> foldl IS.intersection first rest
                        geoSet = IS.fromList (map (fromIntegral . fst) geoFiltered)
                        hitSet = IS.intersection candidateSet geoSet
                        hitPairs = resolveActivityIds actVec hitSet
                        hitPids = IS.fromList (map (fromIntegral . fst) hitPairs)
                     in -- Preserve the original order of geoFiltered (BM25 score order when BM25-driven).
                        [ (pid, a)
                        | (pid, a) <- geoFiltered
                        , IS.member (fromIntegral pid) hitPids
                        , allWordsMatch prod getProductNames a
                        ]

        classFiltered =
            let groups = M.fromListWith (++) [(sys, [(val, isExact)]) | (sys, val, isExact) <- classFilters]
                matchOne v (q, isExact) =
                    if isExact
                        then T.toLower q == T.toLower v
                        else T.isInfixOf (T.toLower q) (T.toLower v)
                applyGroup acc (sys, pairs) =
                    [ (pid, a)
                    | (pid, a) <- acc
                    , case M.lookup sys (activityClassification a) of
                        Just v -> any (matchOne v) pairs
                        Nothing -> False
                    ]
             in foldl applyGroup productFiltered (M.toList groups)
     in classFiltered

{- | Search activities by multiple fields (name, geography, product, classification).
Non-BM25 path: name filter is substring AND-of-tokens on activity name only.
Returns (ProcessId, Activity) pairs so callers don't need to re-scan for ProcessId.
-}
findActivitiesByFields :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Text, Bool)] -> Bool -> [(ProcessId, Activity)]
findActivitiesByFields db nameParam geoParam productParam classFilters exactMatch =
    applyStructuredFilters
        db
        geoParam
        productParam
        classFilters
        exactMatch
        (findActivityNameCandidates db nameParam exactMatch)

-- | Search flows by synonym
findFlowsBySynonym :: Database -> Text -> [Flow]
findFlowsBySynonym db query =
    let queryLower = T.toLower query
        flows = M.elems (dbFlows db)
     in [ f
        | f <- flows
        , T.isInfixOf queryLower (T.toLower (flowName f))
            || any
                (any (T.isInfixOf queryLower . T.toLower) . S.toList)
                (M.elems (flowSynonyms f))
        ]

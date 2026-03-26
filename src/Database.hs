{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Database where

import Progress
import Types
import UnitConversion (UnitConfig, convertExchangeAmount)
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
buildDatabaseWithMatrices :: UnitConfig -> M.Map (UUID, UUID) Activity -> FlowDB -> UnitDB -> IO Database
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
        supplierRefUnits = V.map (\act ->
            let refExs = [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)]
            in case refExs of
                (ex:_) -> getUnitNameForExchange unitDB ex
                [] -> ""
            ) dbActivities

    -- Build activity index for matrix construction
    reportMatrixOperation "Building activity indexes"
    let activityCount = fromIntegral (V.length dbActivities) :: Int32

    -- Note: ProcessId is already the matrix index (identity mapping removed for performance)
    reportMatrixOperation ("Activity index built: " ++ show activityCount ++ " activities")

    -- Build technosphere sparse triplets
    reportMatrixOperation "Building technosphere matrix triplets"
    let buildTechTriple normalizationFactor j consumerActivity _consumerPid ex
            | not (isTechnosphereExchange ex) = ([], [])
            | not (exchangeIsInput ex) = ([], [])
            | exchangeIsReference ex && not (exchangeIsInput ex) = ([], [])
            | otherwise =
                let producerResult = case exchangeProcessLinkId ex of
                        Just pid -> (Just pid, [])
                        Nothing -> case exchangeActivityLinkId ex of
                            Just actUUID ->
                                case M.lookup (actUUID, exchangeFlowId ex) activityProductLookup of
                                    Just pid -> (Just pid, [])
                                    Nothing ->
                                        -- Only warn if exchange has non-zero amount (zero-amount are placeholders)
                                        let warning = if abs (exchangeAmount ex) > 1e-15
                                                     then [ "Missing activity-product pair referenced by exchange:\n"
                                                            ++ "  Activity UUID: " ++ T.unpack (UUID.toText actUUID) ++ "\n"
                                                            ++ "  Product UUID: " ++ T.unpack (UUID.toText (exchangeFlowId ex)) ++ "\n"
                                                            ++ "  Consumer: " ++ T.unpack (activityName consumerActivity) ++ "\n"
                                                            ++ "  Expected file: " ++ T.unpack (UUID.toText actUUID) ++ "_" ++ T.unpack (UUID.toText (exchangeFlowId ex)) ++ ".spold\n"
                                                            ++ "  This exchange will be skipped." ]
                                                     else []
                                        in (Nothing, warning)
                            Nothing -> (Nothing, [])
                    (producerPid, warnings) = producerResult
                    -- ProcessId is already the matrix index (no identity mapping needed)
                    producerIdx =
                        producerPid >>= \pid ->
                            if pid >= 0 && fromIntegral pid < activityCount
                                then Just $ fromIntegral pid
                                else Nothing
                 in case producerIdx of
                        Just idx ->
                            let rawValue = exchangeAmount ex
                                -- Convert exchange amount to supplier's reference product unit
                                -- e.g., if exchange is 1934.5 kg but supplier produces in "ton",
                                -- convert to 1.9345 ton for correct A-matrix coefficient
                                exchangeUnit = getUnitNameForExchange unitDB ex
                                supplierUnit = supplierRefUnits V.! fromIntegral idx
                                convertedValue = convertExchangeAmount unitConfig exchangeUnit supplierUnit rawValue
                                denom = if normalizationFactor > 1e-15
                                        then normalizationFactor
                                        else 1.0  -- Unreachable: normalizationFactor already falls back to 1.0
                                value = convertedValue / denom
                                -- Exclude self-loops (internal consumption): idx == j
                                -- Self-loops are accounted for in normalization factor but not exported as matrix entries
                                -- This matches Ecoinvent's convention where internal losses affect normalization only
                             in ([SparseTriple idx j value | abs value > 1e-15, idx /= j], warnings)
                        Nothing -> ([], warnings)

        buildActivityTriplets (j, consumerPid) =
            let consumerActivity = dbActivities V.! fromIntegral consumerPid
                -- Get activity UUID from ProcessId table
                (activityUUID, _) = dbProcessIdTable V.! fromIntegral consumerPid

                -- For normalization, only use reference OUTPUTS (not treatment inputs)
                -- Treatment inputs have negative amounts and would incorrectly inflate the normalization factor
                refOutputs = [ exchangeAmount ex | ex <- exchanges consumerActivity, exchangeIsReference ex, not (exchangeIsInput ex) ]
                -- If no outputs (pure treatment), use abs of reference input
                refInputs = [ abs (exchangeAmount ex) | ex <- exchanges consumerActivity, exchangeIsReference ex, exchangeIsInput ex ]

                -- Calculate internal consumption (self-loops): technosphere inputs that link back to same activity
                -- These represent internal losses (e.g., electricity market losses, heat for process)
                internalConsumption = sum [ exchangeAmount ex
                                          | ex <- exchanges consumerActivity
                                          , isTechnosphereExchange ex
                                          , exchangeIsInput ex
                                          , not (exchangeIsReference ex)  -- Don't count reference products
                                          , case exchangeActivityLinkId ex of
                                                Just linkUUID -> linkUUID == activityUUID
                                                Nothing -> False
                                          ]

                normalizationFactor =
                    let grossOutput = sum refOutputs
                        grossInput = sum refInputs
                        -- Net output = gross output - internal consumption
                        -- This matches Ecoinvent's normalization convention for market activities
                        netOutput = if grossOutput > 1e-15
                                   then grossOutput - internalConsumption
                                   else 0.0
                    in if netOutput > 1e-15 then netOutput
                       else if grossInput > 1e-15 then grossInput
                       else 1.0  -- Fallback for activities with no reference products (shouldn't happen)
                buildNormalizedTechTriple = buildTechTriple normalizationFactor j consumerActivity consumerPid
                results = map buildNormalizedTechTriple (exchanges consumerActivity)
             in (concatMap fst results, concatMap snd results)

        allResults = map buildActivityTriplets [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1 :: ProcessId]]
        !techTriples = VU.fromList $ concatMap fst allResults
        techWarnings = concatMap snd allResults

    -- Emit warnings in IO context
    mapM_ (reportProgress Warning) techWarnings

    reportMatrixOperation ("Technosphere matrix: " ++ show (VU.length techTriples) ++ " non-zero entries")

    -- Build biosphere sparse triplets
    reportMatrixOperation "Building biosphere matrix triplets"
    let bioFlowUUIDs =
            V.fromList $ sort $
                S.toList $
                    S.fromList
                        [ exchangeFlowId ex | pid <- [0 .. fromIntegral activityCount - 1 :: Int], let act = dbActivities V.! pid, ex <- exchanges act, isBiosphereExchange ex
                        ]
        bioFlowCount = fromIntegral $ V.length bioFlowUUIDs :: Int32
        bioFlowIndex = M.fromList $ zip (V.toList bioFlowUUIDs) [0 ..]

        !bioTriples =
            let buildBioTriple normalizationFactor j _activity ex
                    | not (isBiosphereExchange ex) = []
                    | otherwise =
                        case M.lookup (exchangeFlowId ex) bioFlowIndex of
                            Just i ->
                                let rawValue = exchangeAmount ex
                                    denom = if normalizationFactor > 1e-15
                                            then normalizationFactor
                                            else 1.0  -- Unreachable: normalizationFactor already falls back to 1.0
                                    -- Ecoinvent convention: ALL biosphere flows are positive (both emissions AND resource extractions)
                                    -- Resource extractions represent "outputs" from nature into the technosphere
                                    -- NO sign inversion needed - store as positive regardless of input/output status
                                    value = rawValue / denom
                                 in [SparseTriple i j value | abs value > 1e-15]
                            Nothing -> []

                buildActivityBioTriplets (j, pid) =
                    let activity = dbActivities V.! fromIntegral pid
                        -- Get activity UUID from ProcessId table
                        (activityUUID, _) = dbProcessIdTable V.! fromIntegral pid

                        refOutputs = [ exchangeAmount ex | ex <- exchanges activity, exchangeIsReference ex, not (exchangeIsInput ex) ]
                        refInputs = [ abs (exchangeAmount ex) | ex <- exchanges activity, exchangeIsReference ex, exchangeIsInput ex ]

                        -- Calculate internal consumption (self-loops) same as for technosphere matrix
                        internalConsumption = sum [ exchangeAmount ex
                                                  | ex <- exchanges activity
                                                  , isTechnosphereExchange ex
                                                  , exchangeIsInput ex
                                                  , not (exchangeIsReference ex)
                                                  , case exchangeActivityLinkId ex of
                                                        Just linkUUID -> linkUUID == activityUUID
                                                        Nothing -> False
                                                  ]

                        normalizationFactor =
                            let grossOutput = sum refOutputs
                                grossInput = sum refInputs
                                -- Net output = gross output - internal consumption
                                netOutput = if grossOutput > 1e-15
                                           then grossOutput - internalConsumption
                                           else 0.0
                            in if netOutput > 1e-15 then netOutput
                               else if grossInput > 1e-15 then grossInput
                               else 1.0
                        buildNormalizedBioTriple = buildBioTriple normalizationFactor j activity
                     in concatMap buildNormalizedBioTriple (exchanges activity)

                !result = VU.fromList $ concatMap buildActivityBioTriplets [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1 :: ProcessId]]
             in result

    reportMatrixOperation ("Biosphere matrix: " ++ show (VU.length bioTriples) ++ " non-zero entries")

    reportMatrixOperation "Database with matrices built successfully"
    reportMatrixOperation ("Final matrix stats: " ++ show (VU.length techTriples) ++ " tech entries, " ++ show (VU.length bioTriples) ++ " bio entries")

    -- Build product index for SimaPro link resolution and future product search
    reportMatrixOperation "Building product index"
    let !productIndex = buildProductIndex dbActivities dbProcessIdTable flowDB
    reportMatrixOperation ("Product index: " ++ show (M.size (piByUUID productIndex)) ++ " products indexed")

    return Database
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
            , dbActivityIndex = V.generate (fromIntegral activityCount) fromIntegral -- Identity mapping for compatibility
            , dbBiosphereFlows = bioFlowUUIDs
            , dbActivityCount = activityCount
            , dbBiosphereCount = bioFlowCount
            -- Cross-database linking (empty by default, populated during cross-DB linking)
            , dbCrossDBLinks = []
            , dbDependsOn = []
            , dbLinkingStats = emptyCrossDBLinkingStats
            -- Runtime-only fields (not cached)
            , dbCachedFactorization = Nothing
            , dbSynonymDB = Nothing  -- Will be populated at runtime from embedded DB
            , dbFlowsByName = M.empty  -- Will be populated at runtime
            , dbFlowsByCAS = M.empty  -- Will be populated at runtime
            , dbSearchIndex = M.empty  -- Will be populated at runtime
            , dbProductSearchIndex = M.empty  -- Will be populated at runtime
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

        -- Memory optimization: Removed exchange indexes (exchangeIdx, procExchangeIdx, refProdIdx,
        -- inputIdx, outputIdx) that duplicated 600K Exchange records across 5 maps.
        -- Exchanges can be accessed directly from Activity.exchanges when needed.
        -- This saves ~3-4GB of RAM on Ecoinvent 3.12.
     in
        Indexes
            { idxByName = nameIdx
            , idxByLocation = locationIdx
            , idxByFlow = flowIdx
            , idxByUnit = unitIdx
            , idxFlowByCategory = flowCatIdx
            , idxFlowByType = flowTypeIdx
            }

-- | Build ProductIndex for product-based lookups
-- Used for: (1) SimaPro upstream link resolution, (2) future product search
-- Maps product flow UUIDs and names to the activities that produce them
buildProductIndex :: V.Vector Activity -> V.Vector (UUID, UUID) -> FlowDB -> ProductIndex
buildProductIndex activities processIdTable flowDb =
    let -- Build list of (ProcessId, productUUID, productName, location) for each activity
        entries =
            [ (pid, prodUUID, prodName, actLoc)
            | (pid, (_, prodUUID)) <- zip [0..] (V.toList processIdTable)
            , let act = activities V.! fromIntegral pid
            , let actLoc = activityLocation act
            , Just flow <- [M.lookup prodUUID flowDb]
            , let prodName = T.toLower (flowName flow)
            ]
    in ProductIndex
        { piByUUID = M.fromList [(prodUUID, pid) | (pid, prodUUID, _, _) <- entries]
        , piByName = M.fromListWith (++) [(name, [pid]) | (pid, _, name, _) <- entries]
        , piByLocation = M.fromListWith (++) [(loc, [pid]) | (pid, _, _, loc) <- entries, not (T.null loc)]
        }

-- | Search activities by multiple fields (name, geography, product, classification)
-- Multi-word search: each word must match either name OR location (AND logic)
-- Returns (ProcessId, Activity) pairs so callers don't need to re-scan for ProcessId.
-- Uses word-token index when available for O(matches) instead of O(total) name search.
findActivitiesByFields :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(ProcessId, Activity)]
findActivitiesByFields db nameParam geoParam productParam classParam classValueParam =
    let actVec = dbActivities db
        idx = dbSearchIndex db

        -- Resolve a set of ProcessIds to (ProcessId, Activity) pairs
        resolveIds :: IS.IntSet -> [(ProcessId, Activity)]
        resolveIds ids = [ (fromIntegral i, actVec V.! i) | i <- IS.toList ids, i < V.length actVec ]

        -- Multi-word AND match: all words must appear in at least one of the given text fields
        allWordsMatch :: Text -> (Activity -> [Text]) -> Activity -> Bool
        allWordsMatch query getFields a =
            let searchWords = filter (not . T.null) $ T.words (T.toLower query)
                fields = map T.toLower (getFields a)
             in all (\w -> any (T.isInfixOf w) fields) searchWords

        -- Filter by name using search index (intersect word sets, then verify substring)
        nameFiltered = case nameParam of
            Nothing -> [(fromIntegral i, a) | (i, a) <- zip [(0::Int)..] (V.toList actVec)]
            Just name
                | M.null idx ->
                    -- Fallback: no search index (shouldn't happen in normal operation)
                    [(fromIntegral i, a) | (i, a) <- zip [(0::Int)..] (V.toList actVec)
                    , allWordsMatch name (\a' -> [activityName a', activityLocation a']) a]
                | otherwise ->
                    let searchWords = filter (not . T.null) $ T.words (T.toLower name)
                        -- For each word, find candidate ProcessIds from the index
                        -- A word like "elec" may not match a token exactly, so we need prefix/infix matching on index keys
                        wordCandidates w = IS.unions [ ids | (key, ids) <- M.toList idx, T.isInfixOf w key ]
                        -- Intersect candidates for all words (AND logic)
                        candidates = case map wordCandidates searchWords of
                            [] -> IS.fromList [0 .. V.length actVec - 1]
                            (first:rest) -> foldl IS.intersection first rest
                     in filter (\(_, a') -> allWordsMatch name (\a'' -> [activityName a'', activityLocation a'']) a') (resolveIds candidates)

        -- Filter by geography if provided (substring match)
        geoFiltered = case geoParam of
            Nothing -> nameFiltered
            Just geo ->
                let geoLower = T.toLower geo
                 in [(pid, a) | (pid, a) <- nameFiltered, T.isInfixOf geoLower (T.toLower (activityLocation a))]

        -- Filter by product if provided (multi-word AND match on reference product name)
        pidx = dbProductSearchIndex db

        getProductNames a' =
            [ flowName flow
            | ex <- exchanges a'
            , exchangeIsReference ex
            , not (exchangeIsInput ex)
            , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
            ]

        -- Filter by product using index when available
        productFiltered = case productParam of
            Nothing -> geoFiltered
            Just prod
                | M.null pidx ->
                    [(pid, a) | (pid, a) <- geoFiltered, allWordsMatch prod getProductNames a]
                | otherwise ->
                    let searchWords = filter (not . T.null) $ T.words (T.toLower prod)
                        wordCandidates w = IS.unions [ ids | (key, ids) <- M.toList pidx, T.isInfixOf w key ]
                        candidates = case map wordCandidates searchWords of
                            [] -> IS.fromList (map (fromIntegral . fst) geoFiltered)
                            (first:rest) -> foldl IS.intersection first rest
                        geoSet = IS.fromList (map (fromIntegral . fst) geoFiltered)
                        filtered = IS.intersection candidates geoSet
                     in filter (\(_, a') -> allWordsMatch prod getProductNames a') (resolveIds filtered)

        -- Filter by classification if provided
        classFiltered = case classValueParam of
            Nothing -> productFiltered
            Just val ->
                let valLower = T.toLower val
                in case classParam of
                    Just sys ->
                        [ (pid, a) | (pid, a) <- productFiltered
                        , case M.lookup sys (activityClassification a) of
                            Just v -> T.isInfixOf valLower (T.toLower v)
                            Nothing -> False
                        ]
                    Nothing ->
                        [ (pid, a) | (pid, a) <- productFiltered
                        , any (T.isInfixOf valLower . T.toLower) (M.elems (activityClassification a))
                        ]
     in classFiltered

-- | Search flows by synonym
findFlowsBySynonym :: Database -> Text -> [Flow]
findFlowsBySynonym db query =
    let queryLower = T.toLower query
        flows = M.elems (dbFlows db)
     in [ f | f <- flows, T.isInfixOf queryLower (T.toLower (flowName f))
                            || any
                                (\synonyms -> any (T.isInfixOf queryLower . T.toLower) (S.toList synonyms))
                                (M.elems (flowSynonyms f))
        ]

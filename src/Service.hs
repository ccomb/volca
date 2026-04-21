{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Service where

import API.Types (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ClassificationSystem (..), ConsumerResult (..), EdgeType (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), GraphEdge (..), GraphExport (..), GraphNode (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), NodeType (..), SearchResults (..), Substitution (..), SupplyChainEdge (..), SupplyChainEntry (..), SupplyChainResponse (..), TreeEdge (..), TreeExport (..), TreeMetadata (..), parseSubRef)
import CLI.Types (DebugMatricesOptions (..))
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Either (lefts, rights)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (applyStructuredFilters, findActivitiesByFields, findFlowsBySynonym)
import Matrix (Inventory, accumulateDepDemandsWith, activityNormalizationFactor, applyBiosphereMatrix, applySparseMatrix, buildDemandVectorFromIndex, computeInventoryMatrix, depDemandsToVector, shermanMorrisonVariant, toList)
import qualified Matrix.Export as MatrixExport
import Plugin.Types (Severity (..), ValidateContext (..), ValidateHandle (..), ValidationIssue (..), ValidationPhase (..))
import qualified Progress
import qualified Search.BM25 as BM25
import qualified Search.Fuzzy as Fuzzy
import qualified Search.Normalize as Normalize
import SharedSolver (SharedSolver, getFactorization, solveWithSharedSolver)
import qualified SharedSolver
import Tree (buildLoopAwareTree)
import Types
import UnitConversion (UnitConfig, defaultUnitConfig, unitsCompatible)

{- | Fields shared by every activity-oriented endpoint (search, supply chain,
consumers). Split out from the endpoint-specific filters so each filter
type carries exactly the knobs it can act on — no more "ignored in this
mode" comments.
-}
data ActivityFilterCore = ActivityFilterCore
    { afcName :: Maybe Text
    , afcLocation :: Maybe Text
    , afcProduct :: Maybe Text
    , afcClassifications :: [(Text, Text, Bool)] -- (system, value, isExact)
    , afcLimit :: Maybe Int
    , afcOffset :: Maybe Int
    , afcSort :: Maybe Text
    , afcOrder :: Maybe Text
    }

{- | Filter for activity search (/activities). Carries the shared core plus
the search-only 'sfExactMatch' toggle that switches between token-contains
and exact-equality name matching.
-}
data SearchFilter = SearchFilter
    { sfCore :: !ActivityFilterCore
    , sfExactMatch :: !Bool
    }

{- | Filter for supply-chain walks. Adds the depth cap and the magnitude
cut-off that only make sense downstream from a root activity.
-}
data SupplyChainFilter = SupplyChainFilter
    { scfCore :: !ActivityFilterCore
    , scfMaxDepth :: !(Maybe Int)
    , scfMinQuantity :: !(Maybe Double)
    }

{- | Filter for reverse-walk (/consumers). Adds a depth cap but no
'minQuantity' — scaling factors are meaningless in the reverse direction.
-}
data ConsumerFilter = ConsumerFilter
    { cnfCore :: !ActivityFilterCore
    , cnfMaxDepth :: !(Maybe Int)
    }

{- | Filter for flow search. 'ffQuery' is required; callers that have no
query at all should short-circuit and return 'emptyFlowSearchResults'
before building this record.
-}
data FlowFilter = FlowFilter
    { ffQuery :: Text
    , ffLang :: Maybe Text
    , ffLimit :: Maybe Int
    , ffOffset :: Maybe Int
    , ffSort :: Maybe Text
    , ffOrder :: Maybe Text
    }

-- | Empty flow-search response used by callers that have no query to run.
emptyFlowSearchResults :: Value
emptyFlowSearchResults = toJSON (SearchResults ([] :: [FlowSearchResult]) 0 0 50 False 0.0)

{- | Match an activity against classification filters.
Semantics: OR within the same classification system, AND across different systems.
This matches the documented behaviour in volca.toml classification-presets.
-}
matchClassifications :: Activity -> [(Text, Text, Bool)] -> Bool
matchClassifications activity filters =
    let groups = M.fromListWith (++) [(sys, [(val, isExact)]) | (sys, val, isExact) <- filters]
        matchOne v (q, isExact) =
            if isExact
                then T.toLower q == T.toLower v
                else T.isInfixOf (T.toLower q) (T.toLower v)
        applyGroup acc (sys, pairs) =
            acc && case M.lookup sys (activityClassification activity) of
                Just v -> any (matchOne v) pairs
                Nothing -> False
     in foldl applyGroup True (M.toList groups)

-- | Domain service errors
data ServiceError
    = InvalidUUID Text
    | InvalidProcessId Text
    | ActivityNotFound Text
    | FlowNotFound Text
    | MatrixError Text -- Generic error from matrix computations
    deriving (Show)

-- | Validate UUID format
validateUUID :: Text -> Either ServiceError Text
validateUUID uuidText
    | Just _ <- UUID.fromText uuidText = Right uuidText
    | otherwise = Left $ InvalidUUID $ "Invalid UUID format: " <> uuidText

-- | Parse ProcessId from text (activity_uuid_product_uuid format)
parseProcessIdFromText :: Database -> Text -> Either ServiceError ProcessId
parseProcessIdFromText db text =
    case parseProcessId db text of
        Just processId -> Right processId
        Nothing -> Left $ InvalidProcessId $ "Invalid ProcessId format (expected activity_uuid_product_uuid): " <> text

-- | Find activity by ProcessId using direct Vector access
findActivityByProcessId :: Database -> ProcessId -> Maybe Activity
findActivityByProcessId db processId =
    if processId >= 0 && fromIntegral processId < V.length (dbActivities db)
        then Just $ dbActivities db V.! fromIntegral processId
        else Nothing

-- | Resolve activity query using ProcessId format with UUID fallback for compatibility
resolveActivityByProcessId :: Database -> Text -> Either ServiceError Activity
resolveActivityByProcessId db queryText =
    case resolveActivityAndProcessId db queryText of
        Right (_processId, activity) -> Right activity
        Left err -> Left err

{- | Resolve activity and get both ProcessId and Activity
This is the preferred function when you need the ProcessId (e.g., for matrix operations)
-}
resolveActivityAndProcessId :: Database -> Text -> Either ServiceError (ProcessId, Activity)
resolveActivityAndProcessId db queryText =
    case parseProcessIdFromText db queryText of
        Right processId ->
            case findActivityByProcessId db processId of
                Just activity -> Right (processId, activity)
                Nothing -> Left $ ActivityNotFound queryText
        Left _ ->
            -- Fallback: try as bare UUID for ECOINVENT data compatibility
            case UUID.fromText queryText of
                Just uuid ->
                    case findProcessIdByActivityUUID db uuid of
                        Just processId ->
                            case findActivityByProcessId db processId of
                                Just activity -> Right (processId, activity)
                                Nothing -> Left $ ActivityNotFound queryText
                        Nothing -> Left $ InvalidProcessId $ "Query must be ProcessId format (activity_uuid_product_uuid) or valid UUID: " <> queryText
                Nothing -> Left $ InvalidProcessId $ "Invalid UUID format: " <> queryText

{- | Validate that a ProcessId exists in the matrix activity index
This check ensures we fail fast with clear error messages before expensive matrix operations
The activity index is required for building demand vectors and performing inventory calculations
-}
validateProcessIdInMatrixIndex :: Database -> ProcessId -> Either ServiceError ()
validateProcessIdInMatrixIndex db processId =
    if processId >= 0 && fromIntegral processId < V.length (dbActivityIndex db)
        then Right ()
        else
            Left $
                MatrixError $
                    "ProcessId not available for matrix calculations: "
                        <> T.pack (show processId)
                        <> ". This activity may exist in the database but is not indexed for inventory calculations."

-- | Rich activity info (returns same format as API)
getActivityInfo :: UnitConfig -> Database -> Text -> Either ServiceError Value
getActivityInfo unitCfg db queryText = do
    (processId, activity) <- resolveActivityAndProcessId db queryText
    let activityForAPI = convertActivityForAPI unitCfg db processId activity
        metadata = calculateActivityMetadata db activity
        stats = calculateActivityStats activity
        -- Use ProcessId (which encodes both activityUUID and productUUID) for links
        activityIdForLinks = processIdToText db processId
        links = generateActivityLinks activityIdForLinks
        activityInfo =
            ActivityInfo
                { piActivity = activityForAPI
                , piMetadata = metadata
                , piStatistics = stats
                , piLinks = links
                }
     in Right $ toJSON activityInfo

{- | Core inventory calculation logic using matrix-based LCA calculations
| Convert raw inventory to structured export format.

The 'FlowDB'/'UnitDB' arguments are independent of the root DB so that
cross-DB-merged inventories (whose flow UUIDs can originate in any loaded
dep DB) can be decoded against a merged metadata snapshot. For single-DB
callers, pass @dbFlows db@ / @dbUnits db@ directly.
-}
convertToInventoryExport :: Database -> FlowDB -> UnitDB -> ProcessId -> Activity -> Inventory -> InventoryExport
convertToInventoryExport db flowDB unitDB processId rootActivity inventory =
    let
        -- Filter out flows with zero quantities to reduce noise in the results
        inventoryList = M.toList inventory

        !flowDetails =
            [ InventoryFlowDetail flow quantity uName isEmission category
            | (flowUUID, quantity) <- inventoryList
            , quantity /= 0 -- Exclude flows with zero quantities
            , Just flow <- [M.lookup flowUUID flowDB]
            , let !uName = getUnitNameForFlow unitDB flow
                  !isEmission = not (isResourceExtraction flow quantity)
                  !category = flowCategory flow
            ]

        !emissionFlows = length [f | f <- flowDetails, ifdIsEmission f]
        !resourceFlows = length [f | f <- flowDetails, not (ifdIsEmission f)]

        !totalQuantity = sum [abs (ifdQuantity f) | f <- flowDetails]
        !emissionQuantity = sum [ifdQuantity f | f <- flowDetails, ifdIsEmission f, ifdQuantity f > 0]
        !resourceQuantity = sum [abs (ifdQuantity f) | f <- flowDetails, not (ifdIsEmission f)]

        !categoryStats =
            take 10 $
                M.toList $
                    M.fromListWith (+) [(ifdCategory f, 1) | f <- flowDetails]

        !(prodName, prodAmount, prodUnit) = getReferenceProductInfo flowDB unitDB rootActivity

        !metadata =
            InventoryMetadata
                { imRootActivity =
                    ActivitySummary
                        (processIdToText db processId)
                        (activityName rootActivity)
                        (activityLocation rootActivity)
                        prodName
                        prodAmount
                        prodUnit
                , imTotalFlows = length flowDetails
                , imEmissionFlows = emissionFlows
                , imResourceFlows = resourceFlows
                }

        !statistics =
            InventoryStatistics
                { isTotalQuantity = totalQuantity
                , isEmissionQuantity = emissionQuantity
                , isResourceQuantity = resourceQuantity
                , isTopCategories = categoryStats
                }
     in
        InventoryExport metadata flowDetails statistics

{- | Determine if a flow represents resource extraction based on flow category
Since B matrix now stores all flows as positive (Ecoinvent convention), we use category instead of sign
EcoSpold2: "natural resource/in ground", SimaPro: "resource/in ground"
-}
isResourceExtraction :: Flow -> Double -> Bool
isResourceExtraction flow _ =
    let cat = T.toLower (flowCategory flow)
     in flowType flow == Biosphere
            && ("natural resource" `T.isPrefixOf` cat || "resource" `T.isPrefixOf` cat)

-- | Get activity inventory as rich InventoryExport (same as API)
getActivityInventory :: Database -> Text -> IO (Either ServiceError Value)
getActivityInventory db processIdText =
    case resolveActivityAndProcessId db processIdText >>= \(pid, act) -> validateProcessIdInMatrixIndex db pid >> Right (pid, act) of
        Left err -> return $ Left err
        Right (processId, activity) -> do
            -- Matrix computation (will not fail if validation passed)
            inventory <- computeInventoryMatrix db processId
            let !inventoryExport = convertToInventoryExport db (dbFlows db) (dbUnits db) processId activity inventory
            return $ Right $ toJSON inventoryExport

-- | Shared solver-aware activity inventory export for concurrent processing
getActivityInventoryWithSharedSolver :: [ValidateHandle] -> SharedSolver -> Database -> Text -> IO (Either ServiceError InventoryExport)
getActivityInventoryWithSharedSolver validators sharedSolver db processIdText = do
    case resolveActivityAndProcessId db processIdText of
        Left err -> return $ Left err
        Right (processId, activity) -> do
            -- Validate ProcessId exists in matrix index before expensive computation
            case validateProcessIdInMatrixIndex db processId of
                Left validationErr -> return $ Left validationErr
                Right () -> do
                    -- Run pre-compute validators
                    preIssues <- runPreComputeValidation validators db
                    if hasValidationErrors preIssues
                        then
                            return $
                                Left $
                                    MatrixError $
                                        T.intercalate "; " [viMessage i | i <- preIssues, viSeverity i == Error]
                        else do
                            -- Inline matrix calculation with shared solver
                            let bioFlowCount = dbBiosphereCount db
                                bioTriples = dbBiosphereTriples db
                                activityIndex = dbActivityIndex db
                                bioFlowUUIDs = dbBiosphereFlows db
                                demandVec = buildDemandVectorFromIndex activityIndex processId

                            -- Use shared solver (lazy factorization on first call, cached thereafter)
                            supplyVec <- solveWithSharedSolver sharedSolver demandVec

                            -- Calculate inventory using sparse biosphere matrix: g = B * supply
                            -- Convert Int32 to Int for applySparseMatrix
                            let bioTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
                                bioFlowCountInt = fromIntegral bioFlowCount
                                inventoryVec = applySparseMatrix bioTriplesInt bioFlowCountInt supplyVec
                                inventory = M.fromList $ zip (V.toList bioFlowUUIDs) (toList inventoryVec)

                            -- Run post-compute validators
                            postIssues <- runPostComputeValidation validators db inventory
                            if hasValidationErrors postIssues
                                then
                                    return $
                                        Left $
                                            MatrixError $
                                                T.intercalate "; " [viMessage i | i <- postIssues, viSeverity i == Error]
                                else do
                                    let inventoryExport = convertToInventoryExport db (dbFlows db) (dbUnits db) processId activity inventory
                                    return $ Right inventoryExport

-- | Simple stats tracking for tree processing
data TreeStats = TreeStats Int Int Int -- total, loops, leaves

combineStats :: TreeStats -> TreeStats -> TreeStats
combineStats (TreeStats t1 l1 v1) (TreeStats t2 l2 v2) = TreeStats (t1 + t2) (l1 + l2) (v1 + v2)

{- | Helper to find ProcessId for an activity by searching the database
This is needed because activities don't store their own ProcessId/UUID
Strategy: match activities by name, location, unit, and first reference product flow
-}
findProcessIdForActivity :: Database -> Activity -> Maybe ProcessId
findProcessIdForActivity db activity =
    let actName = activityName activity
        actLoc = activityLocation activity
        actUnit = activityUnit activity
        refFlowId = case [exchangeFlowId ex | ex <- exchanges activity, exchangeIsReference ex] of
            (fid : _) -> Just fid
            [] -> Nothing

        matchesActivity dbActivity =
            let dbRefFlowId = case [exchangeFlowId ex | ex <- exchanges dbActivity, exchangeIsReference ex] of
                    (dbFid : _) -> Just dbFid
                    [] -> Nothing
             in activityName dbActivity == actName
                    && activityLocation dbActivity == actLoc
                    && activityUnit dbActivity == actUnit
                    && dbRefFlowId == refFlowId

        matchingIndex = V.findIndex matchesActivity (dbActivities db)
     in fmap fromIntegral matchingIndex

{- | Find supplier ProcessId by product flow UUID with fallback to name+unit matching.
Primary: UUID lookup in ProductIndex
Fallback: When UUID fails (e.g., SimaPro tkm vs kgkm), find by name with unit compatibility check
-}
findProcessIdByProductFlowWithFallback :: UnitConfig -> Database -> UUID -> Maybe ProcessId
findProcessIdByProductFlowWithFallback unitCfg db flowUUID =
    case findProcessIdByProductFlow db flowUUID of
        Just pid -> Just pid
        Nothing ->
            -- Fallback: look up the flow to get its name and unit
            case M.lookup flowUUID (dbFlows db) of
                Just inputFlow ->
                    let inputName = T.toLower (flowName inputFlow)
                        inputUnit = getUnitNameForFlow (dbUnits db) inputFlow
                        -- Get candidates by normalized name
                        candidates = searchProductsByName db inputName
                        -- Filter to only dimensionally compatible units
                        compatible = filter (isUnitCompatible inputUnit) candidates
                     in case compatible of
                            [singleMatch] -> Just singleMatch -- Exactly one match
                            _ -> Nothing -- Zero or multiple matches
                Nothing -> Nothing
  where
    isUnitCompatible :: Text -> ProcessId -> Bool
    isUnitCompatible inputUnit pid =
        case getActivity db pid of
            Just act ->
                let prodUnit = getRefProductUnit db act
                 in unitsCompatible unitCfg inputUnit prodUnit
            Nothing -> False

    getRefProductUnit :: Database -> Activity -> Text
    getRefProductUnit db' act =
        case [ex | ex <- exchanges act, exchangeIsReference ex] of
            (ex : _) -> getUnitNameForExchange (dbUnits db') ex
            [] -> ""

{- | Helper to get node ID from LoopAwareTree (returns ProcessId format)
For all node types, we attempt to return ProcessId format for consistency
-}
getTreeNodeId :: Database -> LoopAwareTree -> Text
getTreeNodeId db (TreeLeaf activity) =
    case findProcessIdForActivity db activity of
        Just processId -> processIdToText db processId
        Nothing -> "unknown-activity" -- Fallback
getTreeNodeId db (TreeLoop uuid _ _) =
    -- Use ProcessId format for consistency, maintain UUID_UUID format even in fallback
    case findProcessIdByActivityUUID db uuid of
        Just processId -> processIdToText db processId
        Nothing -> UUID.toText uuid <> "_" <> UUID.toText uuid -- Fallback maintains ProcessId format
getTreeNodeId db (TreeNode activity _) =
    case findProcessIdForActivity db activity of
        Just processId -> processIdToText db processId
        Nothing -> "unknown-activity" -- Fallback

-- | Count potential children for navigation (technosphere inputs that could be expanded)
countPotentialChildren :: Database -> Activity -> Int
countPotentialChildren db activity =
    length
        [ ex
        | ex <- exchanges activity
        , isTechnosphereExchange ex
        , exchangeIsInput ex
        , not (exchangeIsReference ex)
        , Just targetUUID <- [exchangeActivityLinkId ex]
        , Just _ <- [findProcessIdByActivityUUID db targetUUID]
        ]

-- | Helper to extract compartment from flow category
extractCompartment :: Text -> Text
extractCompartment category =
    let lowerCategory = T.toLower category
     in if "air" `T.isInfixOf` lowerCategory
            then "air"
            else
                if "water" `T.isInfixOf` lowerCategory || "aquatic" `T.isInfixOf` lowerCategory
                    then "water"
                    else
                        if "soil" `T.isInfixOf` lowerCategory || "ground" `T.isInfixOf` lowerCategory
                            then "soil"
                            else "other"

-- | Extract biosphere exchanges from an activity and create nodes and edges
extractBiosphereNodesAndEdges :: Database -> Activity -> Text -> Int -> M.Map Text ExportNode -> [TreeEdge] -> (M.Map Text ExportNode, [TreeEdge])
extractBiosphereNodesAndEdges db activity activityProcessId depth nodeAcc edgeAcc =
    let allBiosphereExchanges = [ex | ex <- exchanges activity, isBiosphereExchange ex]
        -- Limit to top 50 most significant flows to prevent performance issues with system processes
        maxBiosphereFlows = 50
        biosphereExchanges =
            take maxBiosphereFlows $
                L.sortBy
                    (\a b -> compare (abs (exchangeAmount b)) (abs (exchangeAmount a)))
                    allBiosphereExchanges
        processBiosphere ex (nodeAcc', edgeAcc') =
            case M.lookup (exchangeFlowId ex) (dbFlows db) of
                Nothing -> (nodeAcc', edgeAcc')
                Just flow ->
                    let flowIdText = UUID.toText (flowId flow)
                        isEmission = not (exchangeIsInput ex) -- False = emission, True = resource
                        nodeType = if isEmission then BiosphereEmissionNode else BiosphereResourceNode
                        compartment = extractCompartment (flowCategory flow)
                        biosphereNode =
                            ExportNode
                                { enId = flowIdText
                                , enName = flowName flow
                                , enDescription = [flowCategory flow]
                                , enLocation = ""
                                , enUnit = getUnitNameForFlow (dbUnits db) flow
                                , enNodeType = nodeType
                                , enDepth = depth
                                , enLoopTarget = Nothing
                                , enParentId = Just activityProcessId
                                , enChildrenCount = 0
                                , enCompartment = Just compartment
                                }
                        nodeAcc'' = M.insert flowIdText biosphereNode nodeAcc'
                        -- Create edge with correct direction
                        (edgeFrom, edgeTo, edgeType) =
                            if isEmission
                                then (activityProcessId, flowIdText, BiosphereEmissionEdge)
                                else (flowIdText, activityProcessId, BiosphereResourceEdge)
                        edge =
                            TreeEdge
                                { teFrom = edgeFrom
                                , teTo = edgeTo
                                , teFlow = FlowInfo (flowId flow) (flowName flow) (flowCategory flow)
                                , teQuantity = exchangeAmount ex
                                , teUnit = getUnitNameForFlow (dbUnits db) flow
                                , teEdgeType = edgeType
                                }
                        edgeAcc'' = edge : edgeAcc'
                     in (nodeAcc'', edgeAcc'')
     in foldr processBiosphere (nodeAcc, edgeAcc) biosphereExchanges

-- | Extract nodes and edges from LoopAwareTree
extractNodesAndEdges :: Database -> LoopAwareTree -> Int -> Maybe Text -> M.Map Text ExportNode -> [TreeEdge] -> (M.Map Text ExportNode, [TreeEdge], TreeStats)
extractNodesAndEdges db tree depth parentId nodeAcc edgeAcc = case tree of
    TreeLeaf activity ->
        let childrenCount = countPotentialChildren db activity
            processIdText = getTreeNodeId db tree
            node =
                ExportNode
                    { enId = processIdText -- Now ProcessId format
                    , enName = activityName activity
                    , enDescription = activityDescription activity
                    , enLocation = activityLocation activity
                    , enUnit = activityUnit activity
                    , enNodeType = ActivityNode
                    , enDepth = depth
                    , enLoopTarget = Nothing
                    , enParentId = parentId
                    , enChildrenCount = childrenCount
                    , enCompartment = Nothing
                    }
            nodes' = M.insert processIdText node nodeAcc -- Use ProcessId as key
            -- Add biosphere nodes and edges only for depth 0 (root level)
            (nodes'', edges') =
                if depth == 0
                    then extractBiosphereNodesAndEdges db activity processIdText depth nodes' edgeAcc
                    else (nodes', edgeAcc)
         in (nodes'', edges', TreeStats 1 0 1)
    TreeLoop uuid name loopDepth ->
        let nodeId = getTreeNodeId db tree -- Use ProcessId format for consistency
            uuidText = UUID.toText uuid -- Keep bare UUID for loopTarget
            -- Look up the actual activity to get real unit and location
            maybeActivity = findActivityByActivityUUID db uuid
            (actualLocation, actualUnit) = case maybeActivity of
                Just activity -> (activityLocation activity, activityUnit activity)
                Nothing -> ("N/A", "N/A") -- Fallback only if activity not found
            node =
                ExportNode
                    { enId = nodeId -- Now uses ProcessId format
                    , enName = name
                    , enDescription = ["Loop reference"]
                    , enLocation = actualLocation
                    , enUnit = actualUnit
                    , enNodeType = LoopNode
                    , enDepth = loopDepth
                    , enLoopTarget = Just uuidText
                    , enParentId = parentId
                    , enChildrenCount = 0 -- Loops don't expand
                    , enCompartment = Nothing
                    }
            nodes' = M.insert nodeId node nodeAcc -- Store with ProcessId format key
         in (nodes', edgeAcc, TreeStats 1 1 0)
    TreeNode activity children ->
        let childrenCount = countPotentialChildren db activity
            currentProcessId = getTreeNodeId db tree
            parentNode =
                ExportNode
                    { enId = currentProcessId -- Now ProcessId format
                    , enName = activityName activity
                    , enDescription = activityDescription activity
                    , enLocation = activityLocation activity
                    , enUnit = activityUnit activity
                    , enNodeType = ActivityNode
                    , enDepth = depth
                    , enLoopTarget = Nothing
                    , enParentId = parentId
                    , enChildrenCount = childrenCount
                    , enCompartment = Nothing
                    }
            nodes' = M.insert currentProcessId parentNode nodeAcc -- Use ProcessId as key
            processChild (quantity, flow, subtree) (nodeAcc', edgeAcc', statsAcc) =
                let (childNodes, childEdges, childStats') = extractNodesAndEdges db subtree (depth + 1) (Just currentProcessId) nodeAcc' edgeAcc'
                    edge =
                        TreeEdge
                            { teFrom = currentProcessId -- Now ProcessId format
                            , teTo = getTreeNodeId db subtree -- This now returns ProcessId format
                            , teFlow = FlowInfo (flowId flow) (flowName flow) (flowCategory flow)
                            , teQuantity = quantity
                            , teUnit = getUnitNameForFlow (dbUnits db) flow
                            , teEdgeType = TechnosphereEdge
                            }
                    newStats = combineStats statsAcc childStats'
                 in (childNodes, edge : childEdges, newStats)
            (finalNodes, finalEdges, combinedStats) = foldr processChild (nodes', edgeAcc, TreeStats 1 0 0) children
            -- Add biosphere nodes and edges only for depth 0 (root level)
            (finalNodesWithBio, finalEdgesWithBio) =
                if depth == 0
                    then extractBiosphereNodesAndEdges db activity currentProcessId depth finalNodes finalEdges
                    else (finalNodes, finalEdges)
         in (finalNodesWithBio, finalEdgesWithBio, combinedStats)

-- | Convert LoopAwareTree to TreeExport format for JSON serialization
convertToTreeExport :: Database -> Text -> Int -> LoopAwareTree -> TreeExport
convertToTreeExport db _rootProcessId maxDepth tree =
    let (nodes, edges, _stats) = extractNodesAndEdges db tree 0 Nothing M.empty []
        -- Use the actual root node ID from the tree, not the passed parameter
        -- This ensures tmRootId always matches a key in the nodes map
        actualRootId = getTreeNodeId db tree
        metadata =
            TreeMetadata
                { tmRootId = actualRootId -- Use actual computed root ID
                , tmMaxDepth = maxDepth
                , tmTotalNodes = M.size nodes
                , tmLoopNodes = length [() | (_, node) <- M.toList nodes, enNodeType node == LoopNode]
                , tmLeafNodes = length [() | (_, node) <- M.toList nodes, null [e | e <- edges, teFrom e == enId node]]
                , tmExpandableNodes = length [() | (_, node) <- M.toList nodes, enChildrenCount node > 0]
                }
     in TreeExport metadata nodes edges

-- | Get activity tree as rich TreeExport with configurable depth
getActivityTree :: Database -> Text -> Int -> Maybe Text -> Either ServiceError Value
getActivityTree db queryText maxDepth nameFilter = do
    (_processId, _activity) <- resolveActivityAndProcessId db queryText
    -- Get the activity UUID from the processIdText (which is activityUUID_productUUID)
    let activityUuidText = case T.splitOn "_" queryText of
            (uuid : _) -> uuid
            [] -> queryText -- Fallback
    case UUID.fromText activityUuidText of
        Just activityUuid ->
            let loopAwareTree = buildLoopAwareTree defaultUnitConfig db activityUuid maxDepth
                treeExport = convertToTreeExport db queryText maxDepth loopAwareTree
                filtered = case nameFilter of
                    Nothing -> treeExport
                    Just pat -> filterTreeExport pat treeExport
             in Right $ toJSON filtered
        Nothing -> Left $ InvalidUUID $ "Invalid activity UUID: " <> activityUuidText

{- | Post-filter a TreeExport by name: keep matching nodes plus all their ancestors up to root.
Uses the enParentId chain already stored in each ExportNode — no extra graph traversal.
-}
filterTreeExport :: Text -> TreeExport -> TreeExport
filterTreeExport pat export =
    let nodes = teNodes export
        matchingIds = M.keysSet $ M.filter (Normalize.caseInsensitiveInfixOf pat . enName) nodes
        ancestorsOf nId = case enParentId =<< M.lookup nId nodes of
            Nothing -> S.empty
            Just pid -> S.insert pid (ancestorsOf pid)
        allKept =
            S.union
                matchingIds
                (S.unions (map ancestorsOf (S.toList matchingIds)))
        filteredNodes = M.filterWithKey (\k _ -> S.member k allKept) nodes
        filteredEdges =
            filter
                ( \e ->
                    S.member (teFrom e) allKept
                        && S.member (teTo e) allKept
                )
                (teEdges export)
        meta = (teTree export){tmTotalNodes = M.size filteredNodes}
     in export{teTree = meta, teNodes = filteredNodes, teEdges = filteredEdges}

{- | Build activity network graph from factorized matrix column
Uses efficient sparse matrix operations to extract connections
-}
buildActivityGraph :: Database -> SharedSolver -> Text -> Double -> IO (Either ServiceError GraphExport)
buildActivityGraph db sharedSolver queryText cutoffPercent = do
    case resolveActivityAndProcessId db queryText of
        Left err -> return $ Left err
        Right (processId, _activity) -> do
            -- Step 1: Get factorized column (cumulative amounts) by solving
            let activityIndex = dbActivityIndex db
                demandVec = buildDemandVectorFromIndex activityIndex processId

            -- Solve to get cumulative amounts (lazy factorization on first call)
            supplyVec <- solveWithSharedSolver sharedSolver demandVec
            let supplyList = toList supplyVec
                totalSupply = sum [abs val | val <- supplyList]
                threshold = totalSupply * (cutoffPercent / 100.0)

            -- Step 2: Filter by cutoff to get significant activities
            -- Build list of (ProcessId, cumulative value) for activities above threshold
            -- Always include the root activity (processId) even if below threshold
            let allSignificantActivities =
                    [ (fromIntegral idx :: ProcessId, val)
                    | (idx, val) <- zip [(0 :: Int) ..] supplyList
                    , abs val > threshold
                    ]
                -- Ensure root activity is always included
                significantActivities =
                    if processId `elem` map fst allSignificantActivities
                        then allSignificantActivities
                        else
                            let rootValue =
                                    if fromIntegral processId < length supplyList
                                        then supplyList !! fromIntegral processId
                                        else 0.0
                             in (processId, rootValue) : allSignificantActivities

            -- Step 3: Build node ID mapping (ProcessId -> Int) for frontend efficiency
            let nodeIdMap = M.fromList [(pid, idx) | (idx, (pid, _)) <- zip [0 ..] significantActivities]

            -- Step 4: Extract direct connections from technosphere matrix
            -- For each significant activity, find edges in dbTechnosphereTriples
            let techTriples = dbTechnosphereTriples db
                activities = dbActivities db
                units = dbUnits db
                flows = dbFlows db

                -- Build edges: iterate through sparse triplets
                edges =
                    [ let sourceNodeId = M.lookup (fromIntegral row :: ProcessId) nodeIdMap
                          targetNodeId = M.lookup (fromIntegral col :: ProcessId) nodeIdMap
                          sourceActivity =
                            if fromIntegral row < V.length activities
                                then Just $ activities V.! fromIntegral row
                                else Nothing
                          targetProcessId = fromIntegral col :: ProcessId
                          -- Get target activity UUID from process ID table
                          targetActivityUUID = case processIdToUUIDs db targetProcessId of
                            Just (actUUID, _prodUUID) -> Just actUUID
                            Nothing -> Nothing
                          -- Get flow information from the source activity's exchanges
                          flowInfo = do
                            srcAct <- sourceActivity
                            targetUUID <- targetActivityUUID
                            -- Find the exchange that corresponds to this technosphere connection
                            -- Use pattern matching to filter technosphere inputs
                            let techExchanges =
                                    [ ex
                                    | ex <- exchanges srcAct
                                    , case ex of
                                        TechnosphereExchange _ _ _ isInput isRef _ _ _ -> isInput && not isRef
                                        _ -> False
                                    ]
                            -- Match by target activity UUID
                            case [ex | ex <- techExchanges, exchangeActivityLinkId ex == Just targetUUID] of
                                (ex : _) -> M.lookup (exchangeFlowId ex) flows
                                [] -> Nothing -- No matching exchange found
                          uName = case flowInfo of
                            Just flow -> getUnitNameForFlow units flow
                            Nothing -> "unknown"
                          flowNameText = case flowInfo of
                            Just flow -> flowName flow
                            Nothing -> "Unknown flow"
                       in case (sourceNodeId, targetNodeId) of
                            (Just src, Just tgt) ->
                                Just $ GraphEdge src tgt (realToFrac value) uName flowNameText
                            _ -> Nothing
                    | SparseTriple row col value <- U.toList techTriples
                    , value /= 0.0
                    ]

                validEdges = [e | Just e <- edges]

            -- Step 5: Build nodes
            let nodes =
                    [ let activity =
                            if fromIntegral pid < V.length activities
                                then activities V.! fromIntegral pid
                                else error $ "Invalid ProcessId in graph: " ++ show pid
                          processIdText = processIdToText db pid
                       in GraphNode
                            { gnNodeId = nodeId
                            , gnLabel = activityName activity
                            , gnValue = cumulativeVal
                            , gnUnit = activityUnit activity
                            , gnProcessId = processIdText
                            , gnLocation = activityLocation activity
                            }
                    | (nodeId, (pid, cumulativeVal)) <- zip [0 ..] significantActivities
                    ]

            -- Step 6: Build unit groups for normalization
            let unitGroups = buildUnitGroups [gnUnit n | n <- nodes]

            return $ Right $ GraphExport nodes validEdges unitGroups

-- | Classify units into groups for edge width normalization
buildUnitGroups :: [Text] -> M.Map Text Text
buildUnitGroups units =
    M.fromList [(unit, classifyUnit unit) | unit <- L.nub units]
  where
    classifyUnit u
        | u `elem` ["kg", "g", "t", "ton", "metric ton", "Mg"] = "mass"
        | u `elem` ["m3", "l", "L", "litre", "liter", "dm3"] = "volume"
        | u `elem` ["MJ", "kWh", "J", "kJ", "GJ", "Wh"] = "energy"
        | u `elem` ["Bq", "kBq", "MBq"] = "radioactivity"
        | u `elem` ["m2", "ha", "km2", "m2*a", "m2*year"] = "area"
        | u `elem` ["m", "km", "mm", "tkm", "vkm", "pkm"] = "distance"
        | u `elem` ["h", "hr", "hour", "hours", "person*hour"] = "time"
        | otherwise = "other"

-- | Get flow usage count across all activities
getFlowUsageCount :: Database -> UUID -> Int
getFlowUsageCount db flowUUID =
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> 0
        Just activityUUIDs -> length activityUUIDs

-- | Get flows used by an activity as lightweight summaries
getActivityFlowSummaries :: Database -> Activity -> [FlowSummary]
getActivityFlowSummaries db activity =
    [ FlowSummary flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow)) (determineFlowRole exchange)
    | exchange <- exchanges activity
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]
  where
    determineFlowRole ex
        | exchangeIsReference ex = ReferenceProductFlow
        | exchangeIsInput ex = InputFlow
        | otherwise = OutputFlow

{- | Search flows (returns same format as API). The query is required by the
type; callers with no query return 'emptyFlowSearchResults' directly.
-}
searchFlows :: Database -> FlowFilter -> IO (Either ServiceError Value)
searchFlows db FlowFilter{ffQuery = query, ffLimit = limitParam, ffOffset = offsetParam, ffSort = sortParam, ffOrder = orderParam} = do
    startTime <- getCurrentTime
    let limit = maybe 50 (min 1000) limitParam
        offset = maybe 0 (max 0) offsetParam
        rawResults = findFlowsBySynonym db query
        isDesc = orderParam == Just "desc"
        flowCmp = case sortParam of
            Just "category" -> \a b -> compare (flowCategory a) (flowCategory b)
            Just "unit" -> \a b -> compare (getUnitNameForFlow (dbUnits db) a) (getUnitNameForFlow (dbUnits db) b)
            _ -> \a b -> compare (flowName a) (flowName b)
        allResults = L.sortBy (if isDesc then flip flowCmp else flowCmp) rawResults
        total = length allResults
        dropped = drop offset allResults
        taken = take (limit + 1) dropped
        hasMore = length taken > limit
        pagedResults = take limit taken
        flowResults = map (\flow -> FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow))) pagedResults
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ Right $ toJSON $ SearchResults flowResults total offset limit hasMore searchTimeMs

{- | Retrieve activities by BM25 score. Returns pairs already ordered by score
descending; only documents with score > 0 are included.
Returns Nothing when the query tokenizes to nothing (e.g. pure punctuation),
signalling the caller to fall back to the non-BM25 path.
-}
bm25Retrieve :: Database -> Text -> Maybe [(ProcessId, Activity)]
bm25Retrieve db queryText = do
    idx <- dbBM25Index db
    let tokens = Normalize.tokenize queryText
        weighted = Fuzzy.expandTokens idx tokens
    case weighted of
        [] -> Nothing
        _ ->
            let actVec = dbActivities db
                scores = BM25.score idx weighted
                scored =
                    [ (fromIntegral i, scores U.! i, actVec V.! i)
                    | i <- [0 .. V.length actVec - 1]
                    , scores U.! i > 0
                    ]
                sorted = L.sortOn (\(_, s, _) -> negate s) scored
             in Just [(pid, a) | (pid, _, a) <- sorted]

{- | Set of ProcessIds whose name fuzzy-matches the query, using the same
semantics as @/activities@ BM25 search. @Nothing@ signals \"no filter\":
either the query tokenizes to nothing or the database has no BM25 index
(only happens in bare test fixtures; production DBs always carry one).
-}
bm25MatchingPids :: Database -> Text -> Maybe IS.IntSet
bm25MatchingPids db =
    fmap (IS.fromList . map (fromIntegral . fst)) . bm25Retrieve db

{- | BM25/fuzzy membership set for the optional @name@ filter carried on any
activity-oriented filter. Blank/absent queries and DBs without a BM25 index
both collapse to @Nothing@ (⇒ predicate accepts every pid), so call sites
stay a one-liner.
-}
nameFilterSet :: Database -> Maybe Text -> Maybe IS.IntSet
nameFilterSet db mq = do
    q <- mq
    if T.null (T.strip q) then Nothing else bm25MatchingPids db q

{- | Search activities (returns same format as API). The exact-match toggle is
carried on 'SearchFilter' itself, so there is no separate positional flag.
-}
searchActivities :: Database -> SearchFilter -> IO (Either ServiceError Value)
searchActivities db (SearchFilter core exactMatch) = do
    let nameParam = afcName core
        geoParam = afcLocation core
        productParam = afcProduct core
        classFilters = afcClassifications core
        limitParam = afcLimit core
        offsetParam = afcOffset core
        sortParam = afcSort core
        orderParam = afcOrder core
    startTime <- getCurrentTime
    let isDesc = orderParam == Just "desc"
        explicitSort = sortParam == Just "name" || sortParam == Just "location"
        -- BM25 retrieval applies only when the user provided a non-empty name
        -- query, didn't request exact matching, and didn't pick a sort column.
        bm25Retrieved = case nameParam of
            Just q
                | not exactMatch
                , not explicitSort
                , not (T.null (T.strip q)) ->
                    bm25Retrieve db q
            _ -> Nothing
        actCmp = case sortParam of
            Just "location" -> \(_, a) (_, b) -> compare (activityLocation a) (activityLocation b)
            _ -> \(_, a) (_, b) -> compare (activityName a) (activityName b)
        allResults = case bm25Retrieved of
            Just ranked ->
                -- BM25 path: ranked candidates → structured filters → preserve score order.
                applyStructuredFilters db geoParam productParam classFilters False ranked
            Nothing ->
                -- Non-BM25 path: AND-of-tokens name filter + lex sort.
                let rawResults = findActivitiesByFields db nameParam geoParam productParam classFilters exactMatch
                 in L.sortBy (if isDesc then flip actCmp else actCmp) rawResults
        offset = maybe 0 (max 0) offsetParam
        limit = fromMaybe 20 limitParam
        total = length allResults
        pagedResults = take limit $ drop offset allResults
        hasMore = offset + limit < total
        activityResults =
            map
                ( \(processId, activity) ->
                    let (prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbFlows db) (dbUnits db) activity
                     in ActivitySummary
                            (processIdToText db processId)
                            (activityName activity)
                            (activityLocation activity)
                            prodName
                            prodAmount
                            prodUnit
                )
                pagedResults
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ Right $ toJSON $ SearchResults activityResults total offset limit hasMore searchTimeMs

-- | List all classification systems and their distinct values for a database
getClassifications :: Database -> [ClassificationSystem]
getClassifications db =
    let activities = V.toList (dbActivities db)
        -- Collect all (system, value) pairs
        allPairs = concatMap (M.toList . activityClassification) activities
        -- Group by system: system -> [value]
        bySystem = M.fromListWith (++) [(sys, [val]) | (sys, val) <- allPairs]
     in [ ClassificationSystem sys (L.sort $ L.nub vals) (length vals)
        | (sys, vals) <- L.sortOn fst (M.toList bySystem)
        ]

-- | Calculate extended metadata for an activity
calculateActivityMetadata :: Database -> Activity -> ActivityMetadata
calculateActivityMetadata _db activity =
    let allExchanges = exchanges activity
        uniqueFlows = length $ M.fromList [(exchangeFlowId ex, ()) | ex <- allExchanges]
        techInputs = length [ex | ex <- allExchanges, isTechnosphereExchange ex, exchangeIsInput ex, not (exchangeIsReference ex)]
        bioExchanges = length [ex | ex <- allExchanges, isBiosphereExchange ex]
        refProduct = case [ex | ex <- allExchanges, exchangeIsReference ex] of
            [] -> Nothing
            (ex : _) -> Just (exchangeFlowId ex)
     in ActivityMetadata
            { pmTotalFlows = uniqueFlows
            , pmTechnosphereInputs = techInputs
            , pmBiosphereExchanges = bioExchanges
            , pmHasReferenceProduct = refProduct /= Nothing
            , pmReferenceProductFlow = refProduct
            }

-- | Generate links to sub-resources for an activity
generateActivityLinks :: Text -> ActivityLinks
generateActivityLinks uuid =
    ActivityLinks
        { plFlowsUrl = "/api/v1/activity/" <> uuid <> "/flows"
        , plInputsUrl = "/api/v1/activity/" <> uuid <> "/inputs"
        , plOutputsUrl = "/api/v1/activity/" <> uuid <> "/outputs"
        , plReferenceProductUrl = Just ("/api/v1/activity/" <> uuid <> "/reference-product")
        }

-- | Calculate activity statistics
calculateActivityStats :: Activity -> ActivityStats
calculateActivityStats activity =
    ActivityStats
        { psInputCount = length $ filter exchangeIsInput (exchanges activity)
        , psOutputCount = length $ filter (not . exchangeIsInput) (exchanges activity)
        , psTotalExchanges = length (exchanges activity)
        , psLocation = activityLocation activity
        }

{- | Convert Activity to ActivityForAPI with unit names
Note: This function requires the ProcessId to get the activity UUID
-}
convertActivityForAPI :: UnitConfig -> Database -> ProcessId -> Activity -> ActivityForAPI
convertActivityForAPI unitCfg db processId activity =
    let allProducts = case processIdToUUIDs db processId of
            Just (activityUUID, _) -> getAllProductsForActivity db activityUUID
            Nothing -> []
        (refProdName, refProdAmount, refProdUnit) = getReferenceProductInfo (dbFlows db) (dbUnits db) activity
     in ActivityForAPI
            { pfaProcessId = processIdToText db processId
            , pfaName = activityName activity
            , pfaDescription = activityDescription activity
            , pfaSynonyms = activitySynonyms activity
            , pfaClassifications = activityClassification activity
            , pfaLocation = activityLocation activity
            , pfaUnit = activityUnit activity
            , pfaReferenceProduct = if T.null refProdName then Nothing else Just refProdName
            , pfaReferenceProductAmount = if T.null refProdName then Nothing else Just refProdAmount
            , pfaReferenceProductUnit = if T.null refProdName then Nothing else Just refProdUnit
            , pfaAllProducts = allProducts
            , pfaExchanges = map convertExchangeWithUnit (exchanges activity)
            }
  where
    -- Build cross-DB link lookup by normalized flow name for this consumer activity
    crossDBLinkMap = case processIdToUUIDs db processId of
        Just (actUUID, _) ->
            M.fromList
                [ (T.toLower (cdlFlowName link), link)
                | link <- dbCrossDBLinks db
                , cdlConsumerActUUID link == actUUID
                ]
        Nothing -> M.empty

    convertExchangeWithUnit exchange =
        let flowInfo = M.lookup (exchangeFlowId exchange) (dbFlows db)
            (targetActivityName, targetActivityLocation, targetProcessId) = case exchange of
                TechnosphereExchange fId _ _ isInput _ linkId _ _
                    | isInput && linkId /= UUID.nil ->
                        -- EcoSpold path: use explicit activity link
                        case findActivityByActivityUUID db linkId of
                            Just targetActivity ->
                                let maybeProcessId = findProcessIdByActivityUUID db linkId
                                    processIdText = fmap (processIdToText db) maybeProcessId
                                 in (Just (activityName targetActivity), Just (activityLocation targetActivity), processIdText)
                            Nothing -> (Nothing, Nothing, Nothing)
                    | isInput ->
                        -- SimaPro path: linkId is nil, resolve by product flow UUID
                        -- Uses fallback to name+unit matching when UUID lookup fails (e.g., tkm vs kgkm)
                        case findProcessIdByProductFlowWithFallback unitCfg db fId of
                            Just pid
                                | Just act <- getActivity db pid ->
                                    (Just (activityName act), Just (activityLocation act), Just (processIdToText db pid))
                            _ ->
                                -- Try cross-DB links
                                case flowInfo >>= \flow -> M.lookup (T.toLower (flowName flow)) crossDBLinkMap of
                                    Just link ->
                                        let crossPid =
                                                cdlSourceDatabase link
                                                    <> "::"
                                                    <> UUID.toText (cdlSupplierActUUID link)
                                                    <> "_"
                                                    <> UUID.toText (cdlSupplierProdUUID link)
                                         in (Just (cdlFlowName link), Just (cdlLocation link), Just crossPid)
                                    Nothing -> (Nothing, Nothing, Nothing)
                    | otherwise -> (Nothing, Nothing, Nothing)
                BiosphereExchange _ _ _ _ _ -> (Nothing, Nothing, Nothing)
         in ExchangeWithUnit
                { ewuExchange = exchange
                , ewuUnitName = getUnitNameForExchange (dbUnits db) exchange
                , ewuFlowName = maybe "unknown" flowName flowInfo
                , ewuFlowCategory = case flowInfo of
                    Just flow -> case isTechnosphereExchange exchange of
                        True -> "technosphere"
                        False -> flowCategory flow -- This will now include compartment info like "water/ground-, long-term"
                    Nothing -> "unknown"
                , ewuTargetActivity = targetActivityName
                , ewuTargetLocation = targetActivityLocation
                , ewuTargetProcessId = targetProcessId
                }

-- | Get reference product name from activity exchanges
getReferenceProductName :: M.Map UUID Flow -> Activity -> Maybe Text
getReferenceProductName flows activity =
    case [ex | ex <- exchanges activity, exchangeIsReference ex] of
        (ex : _) -> fmap flowName (M.lookup (exchangeFlowId ex) flows)
        [] -> Nothing

-- | Get reference product info (name, amount, unit) from activity exchanges
getReferenceProductInfo :: M.Map UUID Flow -> UnitDB -> Activity -> (Text, Double, Text)
getReferenceProductInfo flows units activity =
    case [ex | ex <- exchanges activity, exchangeIsReference ex] of
        (ex : _) ->
            let name = maybe "" flowName (M.lookup (exchangeFlowId ex) flows)
                amount = exchangeAmount ex
                uName = getUnitNameForExchange units ex
             in (name, amount, uName)
        [] -> ("", 1.0, "")

-- | Get all products (ProcessIds) for an activity UUID using the products index
getAllProductsForActivity :: Database -> UUID -> [ActivitySummary]
getAllProductsForActivity db activityUUID =
    case M.lookup activityUUID (dbActivityProductsIndex db) of
        Just processIds ->
            [ let (prodName, prodAmount, prodUnit) = getProductInfo db pid
               in ActivitySummary
                    { prsProcessId = processIdToText db pid
                    , prsName = getActivityNameForPid db pid
                    , prsLocation = maybe "" activityLocation (findActivityByProcessId db pid)
                    , prsProduct = prodName
                    , prsProductAmount = prodAmount
                    , prsProductUnit = prodUnit
                    }
            | pid <- processIds
            ]
        Nothing -> []
  where
    -- Get activity name for a ProcessId
    getActivityNameForPid :: Database -> ProcessId -> Text
    getActivityNameForPid db' pid =
        case findActivityByProcessId db' pid of
            Just activity -> activityName activity
            Nothing -> "Unknown"
    -- Get product info (name, amount, unit) from reference exchange
    getProductInfo :: Database -> ProcessId -> (Text, Double, Text)
    getProductInfo db' pid =
        case findActivityByProcessId db' pid of
            Just activity -> getReferenceProductInfo (dbFlows db') (dbUnits db') activity
            Nothing -> ("Unknown", 1.0, "")

-- | Get target activity for technosphere navigation
getTargetActivity :: Database -> Exchange -> Maybe ActivitySummary
getTargetActivity db exchange = do
    targetId <- exchangeActivityLinkId exchange
    targetActivity <- findActivityByActivityUUID db targetId
    processId <- findProcessIdForActivity db targetActivity
    let (prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbFlows db) (dbUnits db) targetActivity
    return $
        ActivitySummary
            { prsProcessId = processIdToText db processId
            , prsName = activityName targetActivity
            , prsLocation = activityLocation targetActivity
            , prsProduct = prodName
            , prsProductAmount = prodAmount
            , prsProductUnit = prodUnit
            }

-- | Get reference product as FlowDetail (if exists)
getActivityReferenceProductDetail :: Database -> Activity -> Maybe FlowDetail
getActivityReferenceProductDetail db activity = do
    refExchange <- case filter exchangeIsReference (exchanges activity) of
        [] -> Nothing
        (ex : _) -> Just ex
    flow <- M.lookup (exchangeFlowId refExchange) (dbFlows db)
    let usageCount = getFlowUsageCount db (flowId flow)
    let uName = getUnitNameForFlow (dbUnits db) flow
    return $ FlowDetail flow uName usageCount

-- | Get activities that use a specific flow as ActivitySummary list
getActivitiesUsingFlow :: Database -> UUID -> [ActivitySummary]
getActivitiesUsingFlow db flowUUID =
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> []
        Just activityUUIDs ->
            let uniqueUUIDs = S.toList $ S.fromList activityUUIDs -- Deduplicate activity UUIDs
             in [ let (prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbFlows db) (dbUnits db) proc
                   in ActivitySummary
                        (processIdToText db processId)
                        (activityName proc)
                        (activityLocation proc)
                        prodName
                        prodAmount
                        prodUnit
                | procUUID <- uniqueUUIDs
                , Just proc <- [findActivityByActivityUUID db procUUID]
                , Just processId <- [findProcessIdForActivity db proc]
                ]

{- | Helper function to get detailed exchanges with filtering. Resolves
cross-DB technosphere inputs (SimaPro pattern: activityLinkId is nil,
the supplier lives in a dep DB via 'dbCrossDBLinks') by synthesizing an
'ActivitySummary' with a qualified pid @"dbName::actUUID_prodUUID"@ —
same convention the @/activity/{pid}@ endpoint uses.
-}
getActivityExchangeDetails :: Database -> Activity -> (Exchange -> Bool) -> [ExchangeDetail]
getActivityExchangeDetails db activity filterFn =
    [ ExchangeDetail
        exchange
        flow
        (getUnitNameForFlow (dbUnits db) flow)
        unit
        (getUnitNameForExchange (dbUnits db) exchange)
        (resolveTarget exchange flow)
    | exchange <- exchanges activity
    , filterFn exchange
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]
  where
    -- Cross-DB link lookup by consumer's flow name, built once per activity
    -- for O(log n) per-exchange resolution.
    crossLinkByFlow :: M.Map Text CrossDBLink
    crossLinkByFlow = case findProcessIdForActivity db activity >>= processIdToUUIDs db of
        Just (actUUID, _) ->
            M.fromList
                [ (T.toLower (cdlFlowName link), link)
                | link <- dbCrossDBLinks db
                , cdlConsumerActUUID link == actUUID
                ]
        Nothing -> M.empty

    resolveTarget exchange flow =
        case getTargetActivity db exchange of
            Just s -> Just s
            Nothing -> crossDBTarget flow

    crossDBTarget flow =
        case M.lookup (T.toLower (flowName flow)) crossLinkByFlow of
            Nothing -> Nothing
            Just link ->
                let qualifiedPid =
                        cdlSourceDatabase link
                            <> "::"
                            <> UUID.toText (cdlSupplierActUUID link)
                            <> "_"
                            <> UUID.toText (cdlSupplierProdUUID link)
                 in Just
                        ActivitySummary
                            { prsProcessId = qualifiedPid
                            , prsName = cdlFlowName link
                            , prsLocation = cdlLocation link
                            , prsProduct = cdlFlowName link
                            , prsProductAmount = 1.0
                            , prsProductUnit = cdlExchangeUnit link
                            }

-- | Get detailed input exchanges
getActivityInputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityInputDetails db activity = getActivityExchangeDetails db activity exchangeIsInput

-- | Get detailed output exchanges
getActivityOutputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityOutputDetails db activity = getActivityExchangeDetails db activity (not . exchangeIsInput)

-- | Get flow info as JSON (for CLI)
getFlowInfo :: Database -> Text -> Either ServiceError Value
getFlowInfo db flowIdText = do
    case UUID.fromText flowIdText of
        Nothing -> Left $ InvalidUUID $ "Invalid flow UUID: " <> flowIdText
        Just fId ->
            case M.lookup fId (dbFlows db) of
                Nothing -> Left $ FlowNotFound flowIdText
                Just flow ->
                    let usageCount = getFlowUsageCount db fId
                        uName = getUnitNameForFlow (dbUnits db) flow
                        flowDetail = FlowDetail flow uName usageCount
                     in Right $ toJSON flowDetail

-- | Get activities that use a specific flow as JSON (for CLI)
getFlowActivities :: Database -> Text -> Either ServiceError Value
getFlowActivities db flowIdText = do
    case UUID.fromText flowIdText of
        Nothing -> Left $ InvalidUUID $ "Invalid flow UUID: " <> flowIdText
        Just fId ->
            case M.lookup fId (dbFlows db) of
                Nothing -> Left $ FlowNotFound flowIdText
                Just _ ->
                    let activities = getActivitiesUsingFlow db fId
                     in Right $ toJSON activities

{- | Compute the supply chain for an activity using the scaling vector.
Returns all upstream activities with their scaling factors and subgraph edges.
-}
getSupplyChain ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    Database ->
    Text ->
    SharedSolver ->
    Text ->
    SupplyChainFilter ->
    Bool ->
    IO (Either ServiceError SupplyChainResponse)
getSupplyChain unitCfg depLookup db dbName sharedSolver processIdText af includeEdges =
    case resolveActivityAndProcessId db processIdText of
        Left err -> return $ Left err
        Right (processId, _rootActivity) ->
            case validateProcessIdInMatrixIndex db processId of
                Left err -> return $ Left err
                Right () -> do
                    let activityIndex = dbActivityIndex db
                        demandVec = buildDemandVectorFromIndex activityIndex processId
                    supplyVec <- solveWithSharedSolver sharedSolver demandVec
                    buildSupplyChainFromScalingVectorCrossDB
                        unitCfg
                        depLookup
                        db
                        dbName
                        processId
                        supplyVec
                        []
                        af
                        includeEdges

{- | Find the shortest supply chain path from a root process to the first upstream activity
whose name contains the given substring (case-insensitive).
Returns path steps ordered root → target, each with cumulative quantity, scaling factor,
and local_step_ratio (upstream ÷ downstream scaling factors).
-}
getPathTo :: Database -> SharedSolver -> Text -> Text -> IO (Either ServiceError Value)
getPathTo db solver pidText target = do
    case resolveActivityAndProcessId db pidText of
        Left err -> return $ Left err
        Right (rootPid, rootAct) ->
            case validateProcessIdInMatrixIndex db rootPid of
                Left err -> return $ Left err
                Right () -> do
                    eVec <- computeScalingVectorWithSubstitutions db solver rootPid []
                    case eVec of
                        Left err -> return $ Left err
                        Right supplyVec ->
                            let rootRefAmount = getReferenceProductAmount rootAct
                                adj = buildAdjacencyFromTriples (dbTechnosphereTriples db)
                                mPath =
                                    bfsToPattern
                                        (fromIntegral rootPid)
                                        ( \i ->
                                            Normalize.caseInsensitiveInfixOf
                                                target
                                                (activityName (dbActivities db V.! i))
                                        )
                                        adj
                             in return $ case mPath of
                                    Nothing ->
                                        Left $
                                            ActivityNotFound $
                                                "No upstream node matching '" <> target <> "' reachable from " <> pidText
                                    Just pids ->
                                        let scalingOf i = supplyVec U.! i
                                            mkStep i mRatio =
                                                let act = dbActivities db V.! i
                                                    sf = scalingOf i
                                                    base =
                                                        [ "process_id" .= processIdToText db (fromIntegral i)
                                                        , "name" .= activityName act
                                                        , "location" .= activityLocation act
                                                        , "unit" .= activityUnit act
                                                        , "cumulative_quantity" .= (sf * rootRefAmount)
                                                        , "scaling_factor" .= sf
                                                        ]
                                                 in object $ case mRatio of
                                                        Nothing -> base
                                                        Just r -> base ++ ["local_step_ratio" .= r]
                                            steps =
                                                mkStep (head pids) (Nothing :: Maybe Double)
                                                    : [ mkStep c (Just ratio)
                                                      | (p, c) <- zip pids (tail pids)
                                                      , let ratio =
                                                                if scalingOf p == 0
                                                                    then 0
                                                                    else scalingOf c / scalingOf p
                                                      ]
                                            totalRatio =
                                                product
                                                    [ scalingOf c / scalingOf p
                                                    | (p, c) <- zip pids (tail pids)
                                                    , scalingOf p /= 0
                                                    ]
                                         in Right $
                                                object
                                                    [ "path" .= steps
                                                    , "path_length" .= length pids
                                                    , "total_ratio" .= totalRatio
                                                    ]

{- | Collect filtered supply-chain entries + edges from a single DB's scaling
vector. Applies @minQuantity@, name/location/product/class/maxDepth filters,
BFS depth assignment, and upstream-count accumulation — but deliberately
does NOT sort, limit, or offset. Callers merge collections from multiple
databases and then apply sorting/pagination once on the combined list.

Entry @sceProcessId@ is qualified with @dbName::@ iff @qualifyPids@ is True
(used for dep-DB entries in cross-DB expansion; root entries stay bare for
backward compatibility with callers that navigate on bare root PIDs).
-}
collectSupplyChainEntries ::
    Database ->
    -- | DB name
    Text ->
    -- | root PID to exclude (Nothing at dep levels)
    Maybe ProcessId ->
    -- | scaling vector
    U.Vector Double ->
    SupplyChainFilter ->
    -- | include edges
    Bool ->
    -- | qualify processIds with @dbName::@
    Bool ->
    -- | multiplier for sceQuantity (rootRefAmount at root, 1.0 at dep)
    Double ->
    -- | depth offset added to per-DB BFS depth
    Int ->
    -- | (unfiltered non-zero count, filtered entries, edges)
    (Int, [SupplyChainEntry], [SupplyChainEdge])
collectSupplyChainEntries db dbName mRootPid supplyVec scf includeEdges qualifyPids quantityMult depthOffset =
    let core = scfCore scf
        minQ = fromMaybe 0 (scfMinQuantity scf)
        n = U.length supplyVec

        allEntries =
            [ (fromIntegral i :: ProcessId, supplyVec U.! i)
            | i <- [0 .. n - 1]
            , let v = supplyVec U.! i
            , abs v > minQ
            , maybe True ((/=) (fromIntegral i)) mRootPid
            ]

        -- One pass: adjacency (for BFS + edges) + consumer counts.
        rootIdx = maybe (-1) fromIntegral mRootPid :: Int
        activeSet = IS.fromList (rootIdx : [fromIntegral pid | (pid, _) <- allEntries])
        (!adjacency, !consumerCounts) = U.foldl' accumulate (IM.empty, IM.empty) (dbTechnosphereTriples db)
          where
            accumulate (!adj, !counts) (SparseTriple row col _val) =
                let r = fromIntegral row
                    c = fromIntegral col
                    adj' = IM.insertWith (++) c [r] adj
                    counts' =
                        if IS.member r activeSet && IS.member c activeSet
                            then IM.insertWith (+) r (1 :: Int) counts
                            else counts
                 in (adj', counts')

        -- BFS from root (or from every entry at dep level when mRootPid = Nothing).
        depthMap = case mRootPid of
            Just rp -> bfsDepth (fromIntegral rp) adjacency
            Nothing -> bfsDepthMulti [fromIntegral pid | (pid, _) <- allEntries] adjacency

        textMatches = Normalize.caseInsensitiveInfixOf

        -- BM25/fuzzy membership set for afcName, computed once per request so
        -- /supply-chain matches whatever /activities already found for the
        -- same query. Nothing ⇒ no effective filter (absent, blank, or DB
        -- without a BM25 index — only bare test fixtures hit that last case).
        mNameSet = nameFilterSet db (afcName core)
        nameMatchesPid pid = maybe True (IS.member (fromIntegral pid)) mNameSet

        getProductNames activity =
            [ flowName flow
            | ex <- exchanges activity
            , exchangeIsReference ex
            , not (exchangeIsInput ex)
            , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
            ]

        matchesFilters activity pid =
            let nameOk = nameMatchesPid pid
                locOk = maybe True (\pat -> textMatches pat (activityLocation activity)) (afcLocation core)
                productOk = maybe True (\pat -> any (textMatches pat) (getProductNames activity)) (afcProduct core)
                classOk = matchClassifications activity (afcClassifications core)
                localDepth = IM.findWithDefault maxBound (fromIntegral pid) depthMap
                depthOk = maybe True (localDepth <=) (scfMaxDepth scf)
             in nameOk && locOk && productOk && classOk && depthOk

        qualify pid
            | qualifyPids = dbName <> "::" <> processIdToText db pid
            | otherwise = processIdToText db pid

        mkEntry (pid, scalingFactor) =
            let activity = dbActivities db V.! fromIntegral pid
             in SupplyChainEntry
                    { sceProcessId = qualify pid
                    , sceDatabaseName = dbName
                    , sceName = activityName activity
                    , sceLocation = activityLocation activity
                    , sceQuantity = scalingFactor * quantityMult
                    , sceUnit = activityUnit activity
                    , sceScalingFactor = scalingFactor
                    , sceClassifications = activityClassification activity
                    , sceDepth = depthOffset + IM.findWithDefault (-1) (fromIntegral pid) depthMap
                    , sceUpstreamCount = IM.findWithDefault 0 (fromIntegral pid) consumerCounts
                    }

        filteredEntries =
            [ mkEntry (pid, val)
            | (pid, val) <- allEntries
            , matchesFilters (dbActivities db V.! fromIntegral pid) pid
            ]

        allIdxSet = S.fromList (maybe [] ((: []) . fromIntegral) mRootPid ++ map (fromIntegral . fst) allEntries)
        edges =
            if not includeEdges
                then []
                else
                    U.foldl'
                        ( \acc (SparseTriple row col val) ->
                            if S.member (fromIntegral row :: Int) allIdxSet && S.member (fromIntegral col :: Int) allIdxSet
                                then
                                    SupplyChainEdge
                                        (qualify (fromIntegral row))
                                        dbName
                                        (qualify (fromIntegral col))
                                        dbName
                                        val
                                        : acc
                                else acc
                        )
                        []
                        (dbTechnosphereTriples db)
     in (length allEntries, filteredEntries, edges)

{- | Sort, offset, and limit a list of supply-chain entries using the shared
filter core's @afcSort@ / @afcOrder@ / @afcLimit@ / @afcOffset@. All
comparison fields (@sceDepth@, @sceUpstreamCount@, @sceQuantity@…) live on
the entry itself, so this works uniformly for single- and cross-DB lists.
-}
sortAndPaginate :: ActivityFilterCore -> [SupplyChainEntry] -> [SupplyChainEntry]
sortAndPaginate core entries =
    let limit = fromMaybe 100 (afcLimit core)
        offset = fromMaybe 0 (afcOffset core)
        isDesc = afcOrder core == Just "desc"
        comparator = case afcSort core of
            Just "name" -> \a b -> compare (sceName a) (sceName b)
            Just "location" -> \a b -> compare (sceLocation a) (sceLocation b)
            Just "unit" -> \a b -> compare (sceUnit a) (sceUnit b)
            Just "depth" -> \a b -> compare (sceDepth a) (sceDepth b)
            Just "consumers" -> \a b -> compare (sceUpstreamCount a) (sceUpstreamCount b)
            Just "amount" -> \a b -> compare (abs (sceQuantity a)) (abs (sceQuantity b))
            _ -> \a b -> compare (abs (sceQuantity b)) (abs (sceQuantity a))
        applied = if isDesc then flip comparator else comparator
     in take limit . drop offset $ L.sortBy applied entries

{- | Pure function: build a SupplyChainResponse from a scaling vector.
Used by both GET (normal) and POST (with substitutions) supply-chain endpoints.
-}
buildSupplyChainFromScalingVector ::
    Database ->
    Text ->
    ProcessId ->
    U.Vector Double ->
    SupplyChainFilter ->
    -- | include edges (expensive: extra pass over technosphere triples)
    Bool ->
    SupplyChainResponse
buildSupplyChainFromScalingVector db dbName processId supplyVec scf includeEdges =
    let rootActivity = dbActivities db V.! fromIntegral processId
        rootRefAmount = getReferenceProductAmount rootActivity
        (totalActs, entries, edges) =
            collectSupplyChainEntries
                db
                dbName
                (Just processId)
                supplyVec
                scf
                includeEdges
                False
                rootRefAmount
                0
        rootSummary =
            ActivitySummary
                { prsProcessId = processIdToText db processId
                , prsName = activityName rootActivity
                , prsLocation = activityLocation rootActivity
                , prsProduct =
                    fromMaybe
                        (activityName rootActivity)
                        (getReferenceProductName (dbFlows db) rootActivity)
                , prsProductAmount = rootRefAmount
                , prsProductUnit = activityUnit rootActivity
                }
     in SupplyChainResponse
            { scrRoot = rootSummary
            , scrTotalActivities = totalActs
            , scrFilteredActivities = length entries
            , scrSupplyChain = sortAndPaginate (scfCore scf) entries
            , scrEdges = edges
            }

{- | Cross-DB supply-chain expansion: starts with the root DB walk, then for
every cross-DB link whose consumer carries non-zero scaling, solves the
induced demand in the dep DB and walks its upstream too. Dep-DB entries
get qualified process IDs (@"depName::actUUID_prodUUID"@) and are tagged
with their own @sceDatabaseName@. Recursion is bounded by
'SharedSolver.maxDepsDepth' to match the LCIA path.

The root scaling vector must already reflect any substitutions; @extraLinks@
carries virtual cross-DB links synthesised by the substitution classifier.
-}
buildSupplyChainFromScalingVectorCrossDB ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    Database ->
    -- | root DB + name
    Text ->
    ProcessId ->
    -- | root PID + its scaling
    U.Vector Double ->
    -- | extra virtual links from subs
    [CrossDBLink] ->
    SupplyChainFilter ->
    -- | include edges
    Bool ->
    IO (Either ServiceError SupplyChainResponse)
buildSupplyChainFromScalingVectorCrossDB unitCfg depLookup rootDb rootDbName rootPid rootScaling extraLinks scf includeEdges = do
    let rootActivity = dbActivities rootDb V.! fromIntegral rootPid
        rootRefAmount = getReferenceProductAmount rootActivity
        (rootTotal, rootEntries, rootEdges) =
            collectSupplyChainEntries
                rootDb
                rootDbName
                (Just rootPid)
                rootScaling
                scf
                includeEdges
                False
                rootRefAmount
                0
        rootSummary =
            ActivitySummary
                { prsProcessId = processIdToText rootDb rootPid
                , prsName = activityName rootActivity
                , prsLocation = activityLocation rootActivity
                , prsProduct =
                    fromMaybe
                        (activityName rootActivity)
                        (getReferenceProductName (dbFlows rootDb) rootActivity)
                , prsProductAmount = rootRefAmount
                , prsProductUnit = activityUnit rootActivity
                }
    eDep <- walkDepLevels unitCfg depLookup rootDb rootScaling extraLinks scf includeEdges 1 S.empty
    pure $ case eDep of
        Left err -> Left err
        Right (depTotal, depEntries, depEdges) ->
            let combinedEntries = rootEntries ++ depEntries
                combinedEdges = rootEdges ++ depEdges
             in Right
                    SupplyChainResponse
                        { scrRoot = rootSummary
                        , scrTotalActivities = rootTotal + depTotal
                        , scrFilteredActivities = length combinedEntries
                        , scrSupplyChain = sortAndPaginate (scfCore scf) combinedEntries
                        , scrEdges = combinedEdges
                        }

{- | Recursive helper: for every cross-DB link emerging from @consumerScaling@,
solve the induced dep demand and collect entries\/edges from the dep DB
(filtered by the same 'SupplyChainFilter' as the root). Returns
@(total_active_count, filtered_entries, edges)@ summed across all reached
dep DBs at this depth and deeper. No pagination here — that's applied once
at the top level on the merged list.
-}
walkDepLevels ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    -- | current consumer DB
    Database ->
    -- | current level's scaling
    U.Vector Double ->
    -- | extra virtual links visible at this level
    [CrossDBLink] ->
    SupplyChainFilter ->
    Bool ->
    -- | current depth
    Int ->
    -- | visited DB names (cycle guard)
    S.Set Text ->
    IO (Either ServiceError (Int, [SupplyChainEntry], [SupplyChainEdge]))
walkDepLevels unitCfg depLookup consumerDb consumerScaling extras scf includeEdges depth visited
    | depth >= maxSubsDepth = pure (Right (0, [], []))
    | otherwise = do
        let demandsMap = accumulateDepDemandsWith consumerDb extras consumerScaling
        results <-
            mapM
                (resolveOneDep unitCfg depLookup scf includeEdges depth visited)
                (M.toList demandsMap)
        pure $ case lefts results of
            (err : _) -> Left err
            [] -> Right (foldr merge3 (0, [], []) (rights results))
  where
    merge3 (t1, es1, ed1) (t2, es2, ed2) = (t1 + t2, es1 ++ es2, ed1 ++ ed2)

{- | For a single dep DB: solve its induced demand, collect filtered entries
using the shared 'collectSupplyChainEntries' helper (so it gets the same
minQuantity/maxDepth pruning as the root walk), then recurse.
-}
resolveOneDep ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    SupplyChainFilter ->
    Bool ->
    -- | current depth (the one we're entering)
    Int ->
    -- | visited
    S.Set Text ->
    (Text, M.Map (UUID, UUID) (Double, Text)) ->
    IO (Either ServiceError (Int, [SupplyChainEntry], [SupplyChainEdge]))
resolveOneDep unitCfg depLookup scf includeEdges depth visited (depDbName, demands)
    | depDbName `S.member` visited = pure (Right (0, [], []))
    | otherwise = do
        mDep <- depLookup depDbName
        case mDep of
            Nothing -> pure (Right (0, [], [])) -- unloaded dep DB: silent skip (matches LCIA path)
            Just (depDb, depSolver) -> case depDemandsToVector unitCfg depDbName depDb demands of
                Left err -> pure (Left (MatrixError err))
                Right demandVec -> do
                    depScaling <- solveWithSharedSolver depSolver demandVec
                    let (localTotal, localEntries, localEdges) =
                            collectSupplyChainEntries
                                depDb
                                depDbName
                                Nothing
                                depScaling
                                scf
                                includeEdges
                                True
                                1.0
                                depth
                    eDeeper <-
                        walkDepLevels
                            unitCfg
                            depLookup
                            depDb
                            depScaling
                            []
                            scf
                            includeEdges
                            (depth + 1)
                            (S.insert depDbName visited)
                    pure $ case eDeeper of
                        Left err -> Left err
                        Right (deeperTotal, deeperEntries, deeperEdges) ->
                            Right
                                ( localTotal + deeperTotal
                                , localEntries ++ deeperEntries
                                , localEdges ++ deeperEdges
                                )

{- | Build reverse adjacency (consumer -> [supplier]) from a vector of
technosphere sparse triplets. Each triplet @(row=supplier, col=consumer)@
contributes one edge into @col@'s neighbour list. Supplies BFS callers
that don't also need a fused pass (see 'collectSupplyChainEntries' for
the fused variant that computes counts in the same traversal).
-}
buildAdjacencyFromTriples :: U.Vector SparseTriple -> IM.IntMap [Int]
buildAdjacencyFromTriples =
    U.foldl'
        ( \acc (SparseTriple row col _) ->
            IM.insertWith (++) (fromIntegral col) [fromIntegral row] acc
        )
        IM.empty

{- | BFS from a set of starting nodes (all at depth 0), returning node ->
shortest distance. Used at dep levels where every entry activity that
received direct cross-DB demand is a potential starting point.
-}
bfsDepthMulti :: [Int] -> IM.IntMap [Int] -> IM.IntMap Int
bfsDepthMulti roots adj =
    go (foldr (flip (|>)) Empty roots) (IM.fromList [(r, 0) | r <- roots])
  where
    go Empty visited = visited
    go (node :<| queue) visited =
        let depth = visited IM.! node
            neighbors = IM.findWithDefault [] node adj
            (queue', visited') =
                L.foldl'
                    ( \(q, v) n ->
                        if IM.member n v
                            then (q, v)
                            else (q |> n, IM.insert n (depth + 1) v)
                    )
                    (queue, visited)
                    neighbors
         in go queue' visited'

{- | BFS from a single root on adjacency list, returns IntMap of node ->
shortest depth. Specialization of 'bfsDepthMulti'.
-}
bfsDepth :: Int -> IM.IntMap [Int] -> IM.IntMap Int
bfsDepth root = bfsDepthMulti [root]

{- | BFS from root; stop at the first node (other than root) satisfying a predicate.
Returns the path from root to that node (inclusive), or Nothing.
-}
bfsToPattern :: Int -> (Int -> Bool) -> IM.IntMap [Int] -> Maybe [Int]
bfsToPattern from matches adj = go (Empty |> from) (IM.singleton from from)
  where
    go Empty _ = Nothing
    go (node :<| queue) parents
        | node /= from && matches node = Just (reconstruct node parents)
        | otherwise =
            let neighbors = IM.findWithDefault [] node adj
                (queue', parents') =
                    L.foldl'
                        ( \(q, p) n ->
                            if IM.member n p
                                then (q, p)
                                else (q |> n, IM.insert n node p)
                        )
                        (queue, parents)
                        neighbors
             in go queue' parents'
    reconstruct n ps
        | n == from = [n]
        | otherwise = reconstruct (ps IM.! n) ps ++ [n]

-- | Get reference product amount for an activity (defaults to 1.0)
getReferenceProductAmount :: Activity -> Double
getReferenceProductAmount activity =
    case [exchangeAmount ex | ex <- exchanges activity, exchangeIsReference ex] of
        (amt : _) -> amt
        [] -> 1.0

{- | Compute a scaling vector with optional Sherman-Morrison substitutions.
Returns the (possibly modified) scaling vector. When substitutions is empty,
this is the same as a plain solve.
-}
computeScalingVectorWithSubstitutions ::
    Database ->
    SharedSolver ->
    ProcessId ->
    [Substitution] ->
    IO (Either ServiceError (U.Vector Double))
computeScalingVectorWithSubstitutions db sharedSolver processId subs = do
    let activityIndex = dbActivityIndex db
        demandVec = buildDemandVectorFromIndex activityIndex processId
    originalX <- solveWithSharedSolver sharedSolver demandVec
    case subs of
        [] -> return $ Right originalX
        _ -> do
            mFact <- getFactorization sharedSolver
            foldSubstitutions originalX subs (applySubstitution mFact)
  where
    applySubstitution mFact x sub =
        case ( resolveActivityAndProcessId db (subFrom sub)
             , resolveActivityAndProcessId db (subTo sub)
             , resolveActivityAndProcessId db (subConsumer sub)
             ) of
            (Left err, _, _) -> return $ Left err
            (_, Left err, _) -> return $ Left err
            (_, _, Left err) -> return $ Left err
            (Right (oldPid, _), Right (newPid, _), Right (consumerPid, _)) ->
                case findTechCoefficient db consumerPid oldPid of
                    Nothing ->
                        return $
                            Left $
                                MatrixError $
                                    "No technosphere link from "
                                        <> processIdToText db consumerPid
                                        <> " to supplier "
                                        <> subFrom sub
                    Just a -> do
                        -- Symmetric root-only swap: +a at old supplier, -a at new.
                        let perturb = [(fromIntegral oldPid, a), (fromIntegral newPid, -a)]
                        smResult <-
                            shermanMorrisonVariant
                                db
                                mFact
                                x
                                (fromIntegral consumerPid)
                                perturb
                        return $ case smResult of
                            Left msg -> Left $ MatrixError msg
                            Right x' -> Right x'

    foldSubstitutions x [] _ = return $ Right x
    foldSubstitutions x (s : ss) f = do
        result <- f x s
        case result of
            Left err -> return $ Left err
            Right x' -> foldSubstitutions x' ss f

-- | Safety net against pathological dep chains. Matches 'SharedSolver.maxDepsDepth'.
maxSubsDepth :: Int
maxSubsDepth = 10

{- | What-if inventory with substitutions applied at every DB level of the
dep graph. Substitutions are filtered at each level by
'applySubstitutionsAt' — a sub whose consumer lives in a dep DB is
applied when the recursion reaches that dep DB's solver, not at root.

This generalizes the root-only path: substitutions in @subFrom@/@subTo@
may live in any loaded database (qualified as @"dbName::pid"@), and
@subConsumer@ may also be qualified — the filter finds the right level.
-}
inventoryWithSubsAndDeps ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    Database ->
    -- | root DB name (for qualified-PID parsing)
    Text ->
    SharedSolver ->
    ProcessId ->
    [Substitution] ->
    IO (Either ServiceError Inventory)
inventoryWithSubsAndDeps unitCfg depLookup db rootDbName solver pid subs = do
    eValid <- validateConsumerDbs depLookup db rootDbName subs
    case eValid of
        Left e -> pure (Left e)
        Right () -> do
            let demand = buildDemandVectorFromIndex (dbActivityIndex db) pid
            res <- goWithSubsAndDeps unitCfg depLookup db rootDbName solver [demand] subs 0
            pure $ case res of
                Left err -> Left err
                Right (inv : _) -> Right inv
                Right [] -> Right M.empty -- unreachable: K=1

{- | Reject substitutions whose consumer is qualified to a DB that is either
unloaded or not reachable from @rootDbName@ via 'dbCrossDBLinks'. Such
subs would otherwise be silently filtered at every level of the
recursion (because the consumer DB never appears as @thisDbName@),
which violates the no-silent-errors invariant.
-}
validateConsumerDbs ::
    SharedSolver.DepSolverLookup ->
    Database ->
    Text ->
    [Substitution] ->
    IO (Either ServiceError ())
validateConsumerDbs depLookup rootDb rootDbName subs = do
    let externalConsumerDbs =
            S.delete rootDbName $
                S.fromList
                    [ cDb
                    | sub <- subs
                    , let (cDb, _) = parseSubRef rootDbName (subConsumer sub)
                    ]
    if S.null externalConsumerDbs
        then pure (Right ())
        else do
            reachable <- reachableDepDbs depLookup rootDbName rootDb
            let unreachable = externalConsumerDbs `S.difference` reachable
            case S.toList unreachable of
                [] -> pure (Right ())
                (d : _) -> do
                    mLoad <- depLookup d
                    pure $ Left $ MatrixError $ case mLoad of
                        Nothing -> "substitution consumer references unloaded database: " <> d
                        Just _ ->
                            "substitution consumer database '"
                                <> d
                                <> "' is not reachable from root database's dep-graph"

{- | BFS the loaded portion of the dep-DB DAG from @rootDbName@. Returns
the set of DB names that are statically reachable via 'dbCrossDBLinks'
chains (including unloaded leaves — 'validateConsumerDbs' distinguishes
loaded-but-unreachable from unloaded).
-}
reachableDepDbs ::
    SharedSolver.DepSolverLookup ->
    Text ->
    Database ->
    IO (S.Set Text)
reachableDepDbs depLookup rootDbName rootDb = go (S.singleton rootDbName) [rootDb]
  where
    go visited [] = pure visited
    go visited (cur : queue) = do
        let childNames = S.fromList [cdlSourceDatabase l | l <- dbCrossDBLinks cur]
            unvisited = S.toList (childNames `S.difference` visited)
            visited' = visited `S.union` childNames
        mPairs <- mapM depLookup unvisited
        let loadedChildren = [cdb | Just (cdb, _) <- mPairs]
        go visited' (loadedChildren ++ queue)

{- | Recursive what-if inventory with per-level substitution application.
Mirrors 'SharedSolver.goWithDepsFromScalings' but inserts
'applySubstitutionsAt' between the solve and the dep-demand accumulation
at every DB level, letting substitutions target consumers in any DB.
-}
goWithSubsAndDeps ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    -- | THIS DB
    Database ->
    -- | THIS DB's name
    Text ->
    -- | THIS DB's cached solver
    SharedSolver ->
    -- | demand vectors at this level
    [U.Vector Double] ->
    -- | full sub list (filtered per level)
    [Substitution] ->
    -- | recursion depth
    Int ->
    IO (Either ServiceError [Inventory])
goWithSubsAndDeps unitCfg depLookup thisDb thisDbName solver demands allSubs depth = do
    scalings <- SharedSolver.solveMultiWithSharedSolver solver demands
    eApply <- applySubstitutionsAt depLookup thisDb thisDbName solver scalings allSubs
    case eApply of
        Left e -> pure (Left e)
        Right (scalings', virtualLks) -> propagate scalings' virtualLks
  where
    propagate scalings' virtualLks = do
        let localInvs = map (applyBiosphereMatrix thisDb) scalings'
        if depth >= maxSubsDepth
            then pure (Right localInvs)
            else do
                let perRootDepDemands = map (accumulateDepDemandsWith thisDb virtualLks) scalings'
                    allDepDbs = S.toList $ S.unions $ map M.keysSet perRootDepDemands
                if null allDepDbs
                    then pure (Right localInvs)
                    else do
                        depResults <-
                            mapConcurrently
                                (resolveDepWithSubs unitCfg depLookup perRootDepDemands allSubs depth (length scalings'))
                                allDepDbs
                        pure $ case sequence depResults of
                            Left err -> Left err
                            Right depContribsByDb ->
                                let perRootDepInvs = L.transpose depContribsByDb
                                 in Right $
                                        zipWith
                                            (foldr (M.unionWith (+)))
                                            localInvs
                                            perRootDepInvs

{- | Dep resolver variant that threads the substitution list into the
recursion. Matches 'SharedSolver.resolveDep' but delegates to
'goWithSubsAndDeps' instead of the plain path.
-}
resolveDepWithSubs ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    [M.Map Text (M.Map (UUID, UUID) (Double, Text))] ->
    [Substitution] ->
    Int ->
    Int ->
    Text ->
    IO (Either ServiceError [Inventory])
resolveDepWithSubs unitCfg depLookup perRootDepDemands allSubs depth k depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            pure (Right (replicate k M.empty))
        Just (depDb, depSolver) ->
            let demandsPerRoot = map (M.findWithDefault M.empty depDbName) perRootDepDemands
                depVecsE = traverse (depDemandsToVector unitCfg depDbName depDb) demandsPerRoot
             in case depVecsE of
                    Left err -> pure (Left (MatrixError err))
                    Right depDemandVecs ->
                        goWithSubsAndDeps unitCfg depLookup depDb depDbName depSolver depDemandVecs allSubs (depth + 1)

{- | Cross-DB substitution resolver (root-only path, used by supply-chain).

Solves the root scaling vector then delegates to 'applySubstitutionsAt'
against the root DB. Keeps the \"consumer must live in root\" guard
because supply-chain renders only the root technosphere graph — a
dep-DB consumer sub would be silently ignored here, so we surface it as
an error (the inventory/LCIA path lifts this restriction via
'goWithSubsAndDeps').
-}
computeScalingVectorWithSubstitutionsCrossDB ::
    SharedSolver.DepSolverLookup ->
    Database ->
    -- | root DB name
    Text ->
    SharedSolver ->
    ProcessId ->
    [Substitution] ->
    IO (Either ServiceError (U.Vector Double, [CrossDBLink]))
computeScalingVectorWithSubstitutionsCrossDB depLookup db rootDbName solver pid subs = do
    case firstNonRootConsumer of
        Just cDb ->
            pure $
                Left $
                    MatrixError $
                        "substitution consumer must live in root database (got: " <> cDb <> ")"
        Nothing -> do
            let demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            originalX <- solveWithSharedSolver solver demandVec
            res <- applySubstitutionsAt depLookup db rootDbName solver [originalX] subs
            pure $ case res of
                Left e -> Left e
                Right ([x'], links) -> Right (x', links)
                Right (x' : _, links) -> Right (x', links) -- unreachable: K=1
                Right ([], _) -> Right (originalX, []) -- unreachable
  where
    firstNonRootConsumer =
        case [ cDb
             | sub <- subs
             , let (cDb, _) = parseSubRef rootDbName (subConsumer sub)
             , cDb /= rootDbName
             ] of
            (d : _) -> Just d
            [] -> Nothing

{- | Apply all substitutions whose consumer lives in @thisDbName@ to the
given scaling vectors. Substitutions whose consumer lives elsewhere are
skipped at this level — they'll match at the DB where their consumer
lives during the recursive traversal in 'goWithSubsAndDeps'.

Classifies each sub by where its old/new suppliers live relative to
@thisDbName@:

* Case A — both in this DB: symmetric rank-1 update @[(old,+a),(new,-a)]@.
* Case B — old in this DB, new elsewhere: asymmetric root update
  @[(old,+a)]@ plus a virtual @CrossDBLink@ routing demand @+a@ to the
  other-DB supplier.
* Case C — old elsewhere, new in this DB: asymmetric root update
  @[(new,-a_norm)]@ plus a virtual @CrossDBLink@ with negative coefficient
  that cancels the existing static link.
* Case D — both elsewhere: no matrix change; two virtual @CrossDBLink@
  entries (@-a@ on the old supplier, @+a@ on the new).

Missing dep DBs, unresolved qualified PIDs, and Case-C without a matching
static link surface as 'MatrixError' (no silent fallback — the caller
maps to 422).
-}
applySubstitutionsAt ::
    SharedSolver.DepSolverLookup ->
    -- | THIS DB
    Database ->
    -- | THIS DB's name (for parseSubRef)
    Text ->
    -- | THIS DB's cached solver
    SharedSolver ->
    -- | K scalings at this level
    [U.Vector Double] ->
    -- | full sub list (filtered to consumer==this)
    [Substitution] ->
    IO (Either ServiceError ([U.Vector Double], [CrossDBLink]))
applySubstitutionsAt depLookup thisDb thisDbName solver scalings allSubs = do
    let localSubs = filter consumerLivesHere allSubs
    case localSubs of
        [] -> pure $ Right (scalings, [])
        _ -> do
            mFact <- getFactorization solver
            applyAll mFact scalings [] localSubs
  where
    consumerLivesHere sub =
        let (cDb, _) = parseSubRef thisDbName (subConsumer sub)
         in cDb == thisDbName

    applyAll _ xs links [] = pure $ Right (xs, links)
    applyAll mFact xs links (sub : rest) = do
        res <- applySub mFact xs sub
        case res of
            Left e -> pure (Left e)
            Right (xs', extraLks) -> applyAll mFact xs' (links ++ extraLks) rest

    applySub mFact xs sub = do
        let (_, cPidText) = parseSubRef thisDbName (subConsumer sub)
        case resolveActivityAndProcessId thisDb cPidText of
            Left e -> pure (Left e)
            Right (consumerPid, _) -> do
                let (fromDb, fromPidText) = parseSubRef thisDbName (subFrom sub)
                    (toDb, toPidText) = parseSubRef thisDbName (subTo sub)
                eFrom <- resolveRef fromDb fromPidText
                eTo <- resolveRef toDb toPidText
                case (eFrom, eTo) of
                    (Left e, _) -> pure (Left e)
                    (_, Left e) -> pure (Left e)
                    (Right fromRef, Right toRef) ->
                        classify mFact xs consumerPid sub fromRef toRef

    classify mFact xs consumerPid sub fromRef toRef =
        let (fromDb, fromPid, fromUUIDs, _) = fromRef
            (toDb, toPid, toUUIDs, toDbObj) = toRef
         in case (fromDb == thisDbName, toDb == thisDbName) of
                (True, True) ->
                    case findTechCoefficient thisDb consumerPid fromPid of
                        Nothing -> noTechLinkErr sub consumerPid
                        Just aNorm ->
                            applyRankOne
                                mFact
                                xs
                                consumerPid
                                [ (fromIntegral fromPid, aNorm)
                                , (fromIntegral toPid, -aNorm)
                                ]
                                []
                (True, False) ->
                    -- Case B: drop this-DB oldSup, route demand to other-DB newSup.
                    case findTechCoefficient thisDb consumerPid fromPid of
                        Nothing -> noTechLinkErr sub consumerPid
                        Just aNorm ->
                            let aRaw = aNorm * activityNormalizationFactor thisDb consumerPid
                                newLk = mkVirtualLink thisDb consumerPid toDbObj toDb toUUIDs toPid aRaw
                             in applyRankOne
                                    mFact
                                    xs
                                    consumerPid
                                    [(fromIntegral fromPid, aNorm)]
                                    [newLk]
                (False, True) ->
                    -- Case C: cancel existing cross-DB link, pull new this-DB supplier.
                    case findStaticCrossDBLink thisDb consumerPid fromDb fromUUIDs of
                        Nothing -> noStaticLinkErr sub consumerPid fromDb fromPid
                        Just staticLk ->
                            let aRaw = cdlCoefficient staticLk
                                aNorm = aRaw / activityNormalizationFactor thisDb consumerPid
                                cancel = staticLk{cdlCoefficient = -aRaw}
                             in applyRankOne
                                    mFact
                                    xs
                                    consumerPid
                                    [(fromIntegral toPid, -aNorm)]
                                    [cancel]
                (False, False) ->
                    -- Case D: re-route demand between two other DBs; this-DB x unchanged.
                    case findStaticCrossDBLink thisDb consumerPid fromDb fromUUIDs of
                        Nothing -> noStaticLinkErr sub consumerPid fromDb fromPid
                        Just staticLk ->
                            let aRaw = cdlCoefficient staticLk
                                cancel = staticLk{cdlCoefficient = -aRaw}
                                newLk = mkVirtualLink thisDb consumerPid toDbObj toDb toUUIDs toPid aRaw
                             in applyRankOne mFact xs consumerPid [] [cancel, newLk]

    applyRankOne mFact xs consumerPid perturb extra = do
        -- Apply the same rank-1 update to each of the K vectors. z depends
        -- only on u (not x); a future optimization can compute z once.
        results <-
            mapM
                (\x -> shermanMorrisonVariant thisDb mFact x (fromIntegral consumerPid) perturb)
                xs
        pure $ case sequence results of
            Left msg -> Left (MatrixError msg)
            Right xs' -> Right (xs', extra)

    noTechLinkErr sub consumerPid =
        pure $
            Left $
                MatrixError $
                    "No technosphere link from "
                        <> processIdToText thisDb consumerPid
                        <> " to supplier "
                        <> subFrom sub

    noStaticLinkErr sub consumerPid fromDb fromPid =
        pure $
            Left $
                MatrixError $
                    "no cross-DB link from "
                        <> processIdToText thisDb consumerPid
                        <> " to "
                        <> fromDb
                        <> "::"
                        <> T.pack (show fromPid)
                        <> " (requested by substitution "
                        <> subFrom sub
                        <> " -> "
                        <> subTo sub
                        <> ")"

    resolveRef :: Text -> Text -> IO (Either ServiceError (Text, ProcessId, (UUID, UUID), Database))
    resolveRef refDb pidText
        | refDb == thisDbName = case resolveActivityAndProcessId thisDb pidText of
            Left e -> pure (Left e)
            Right (p, _) ->
                let uuids = dbProcessIdTable thisDb V.! fromIntegral p
                 in pure (Right (refDb, p, uuids, thisDb))
        | otherwise = do
            mPair <- depLookup refDb
            case mPair of
                Nothing ->
                    pure $
                        Left $
                            MatrixError $
                                "substitution references unloaded database: " <> refDb
                Just (depDb, _) -> case resolveActivityAndProcessId depDb pidText of
                    Left _ ->
                        pure $
                            Left $
                                MatrixError $
                                    "substitution PID not found in " <> refDb <> ": " <> pidText
                    Right (p, _) ->
                        let uuids = dbProcessIdTable depDb V.! fromIntegral p
                         in pure (Right (refDb, p, uuids, depDb))

{- | Build a synthesized 'CrossDBLink' for a what-if substitution targeting a
dep-DB supplier. Mirrors the fields a real (load-time) link would have so
'accumulateDepDemandsWith' handles it identically — including the raw→refUnit
conversion in 'depDemandsToVector' (we set the exchange unit to the supplier's
own reference-product unit so no conversion is needed).
-}
mkVirtualLink ::
    -- | root DB (consumer side)
    Database ->
    -- | consumer's root ProcessId
    ProcessId ->
    -- | dep DB (supplier side)
    Database ->
    -- | dep DB name
    Text ->
    -- | supplier's (actUUID, prodUUID) in dep DB
    (UUID, UUID) ->
    -- | supplier's dep-DB ProcessId
    ProcessId ->
    -- | raw exchange coefficient (pre-normalization)
    Double ->
    CrossDBLink
mkVirtualLink rootDb consumerPid depDb depDbName supUUIDs supPid coef =
    let (cActU, cProdU) = dbProcessIdTable rootDb V.! fromIntegral consumerPid
        supAct = dbActivities depDb V.! fromIntegral supPid
        refUnit = case [ex | ex <- exchanges supAct, exchangeIsReference ex, not (exchangeIsInput ex)] of
            (ex : _) -> getUnitNameForExchange (dbUnits depDb) ex
            [] -> ""
        (supActU, supProdU) = supUUIDs
     in CrossDBLink
            { cdlConsumerActUUID = cActU
            , cdlConsumerProdUUID = cProdU
            , cdlSupplierActUUID = supActU
            , cdlSupplierProdUUID = supProdU
            , cdlCoefficient = coef
            , cdlExchangeUnit = refUnit
            , cdlFlowName = activityName supAct
            , cdlLocation = activityLocation supAct
            , cdlSourceDatabase = depDbName
            }

{- | Find the static 'CrossDBLink' matching @(rootConsumer, depDbName, depSupplierUUIDs)@.
Returns 'Nothing' if no link exists — caller surfaces as 422 rather than
silently no-op.
-}
findStaticCrossDBLink :: Database -> ProcessId -> Text -> (UUID, UUID) -> Maybe CrossDBLink
findStaticCrossDBLink rootDb consumerPid depDbName depSupUUIDs =
    let (cActU, cProdU) = dbProcessIdTable rootDb V.! fromIntegral consumerPid
        matches lk =
            cdlSourceDatabase lk == depDbName
                && cdlConsumerActUUID lk == cActU
                && cdlConsumerProdUUID lk == cProdU
                && (cdlSupplierActUUID lk, cdlSupplierProdUUID lk) == depSupUUIDs
     in case filter matches (dbCrossDBLinks rootDb) of
            (lk : _) -> Just lk
            [] -> Nothing

-- | Find the technosphere coefficient A[supplier, consumer] from the sparse triples
findTechCoefficient :: Database -> ProcessId -> ProcessId -> Maybe Double
findTechCoefficient db consumer supplier =
    let techTriples = dbTechnosphereTriples db
        consumerIdx = fromIntegral consumer :: Int32
        supplierIdx = fromIntegral supplier :: Int32
        matching =
            U.filter
                (\(SparseTriple row col _) -> row == supplierIdx && col == consumerIdx)
                techTriples
     in if U.null matching
            then Nothing
            else let SparseTriple _ _ val = U.head matching in Just val

{- | Find all activities that transitively depend on a given supplier.
BFS through the technosphere matrix tracking depth; optional max-depth cap.
-}
getConsumers :: Database -> Text -> ConsumerFilter -> Either ServiceError (SearchResults ConsumerResult)
getConsumers db processIdText cnf = do
    (processId, _) <- resolveActivityAndProcessId db processIdText
    let core = cnfCore cnf
        -- Build adjacency list: supplier → [direct consumers]
        adj =
            M.fromListWith
                (++)
                [ (fromIntegral row :: ProcessId, [fromIntegral col :: ProcessId])
                | SparseTriple row col _ <- U.toList (dbTechnosphereTriples db)
                , row /= col -- skip self-loops
                ]

        -- BFS tracking depth per node (Map ProcessId depth)
        bfs depthMap [] = depthMap
        bfs depthMap frontier =
            let next =
                    [ (c, d + 1)
                    | (pid, d) <- frontier
                    , c <- M.findWithDefault [] pid adj
                    , not (M.member c depthMap)
                    ]
                -- deduplicate: keep minimum depth for nodes seen in this wave
                nextDeduped = M.toList $ M.fromListWith min next
                nextFiltered = filter (\(_, d) -> maybe True (d <=) (cnfMaxDepth cnf)) nextDeduped
                depthMap' = L.foldl' (\m (c, d) -> M.insert c d m) depthMap nextFiltered
             in bfs depthMap' nextFiltered

        allConsumers =
            M.delete processId $
                bfs (M.singleton processId 0) [(processId, 0)]

        mNameSet = nameFilterSet db (afcName core)
        nameMatches pid = maybe True (IS.member (fromIntegral pid)) mNameSet

        locationMatches activity = case afcLocation core of
            Nothing -> True
            Just pat -> Normalize.caseInsensitiveInfixOf pat (activityLocation activity)

        productMatches prodName = case afcProduct core of
            Nothing -> True
            Just pat -> Normalize.caseInsensitiveInfixOf pat prodName

        classMatches activity = matchClassifications activity (afcClassifications core)

        limit = fromMaybe 1000 (afcLimit core)
        offset = fromMaybe 0 (afcOffset core)

        rawResults =
            [ ConsumerResult
                (processIdToText db pid)
                (activityName activity)
                (activityLocation activity)
                prodName
                prodAmount
                prodUnit
                depth
            | (pid, depth) <- M.toAscList allConsumers
            , nameMatches pid
            , let activity = dbActivities db V.! fromIntegral pid
            , locationMatches activity
            , classMatches activity
            , let (prodName, prodAmount, prodUnit) =
                    getReferenceProductInfo (dbFlows db) (dbUnits db) activity
            , productMatches prodName
            ]

        isDesc = afcOrder core == Just "desc"
        crCmp = case afcSort core of
            Just "name" -> \a b -> compare (crName a) (crName b)
            Just "location" -> \a b -> compare (crLocation a) (crLocation b)
            Just "amount" -> \a b -> compare (crProductAmount a) (crProductAmount b)
            Just "unit" -> \a b -> compare (crProductUnit a) (crProductUnit b)
            Just "product" -> \a b -> compare (crProduct a) (crProduct b)
            _ -> \a b -> compare (crDepth a) (crDepth b)
        allResults = L.sortBy (if isDesc then flip crCmp else crCmp) rawResults

        total = length allResults
        page = take limit $ drop offset allResults
        hasMore = offset + limit < total

    Right $ SearchResults page total offset limit hasMore 0.0

-- | Compute LCIA scores (placeholder)
computeLCIA :: Database -> Text -> FilePath -> Either ServiceError Value
computeLCIA db queryText methodFile = do
    (processId, _activity) <- resolveActivityAndProcessId db queryText
    let placeholder = M.fromList [("method" :: Text, T.pack methodFile), ("activity", processIdToText db processId)]
     in Right $ toJSON placeholder

-- | Export LCIA results as XML (placeholder)
exportLCIAAsXML :: Value -> FilePath -> Either ServiceError ()
exportLCIAAsXML _ _ = Right ()

-- | Export LCIA results as CSV (placeholder)
exportLCIAAsCSV :: Value -> FilePath -> Either ServiceError ()
exportLCIAAsCSV _ _ = Right ()

-- | Export matrix debug data (delegates to Matrix.Export)
exportMatrixDebugData :: Database -> Text -> DebugMatricesOptions -> IO (Either ServiceError Value)
exportMatrixDebugData database processIdText opts = do
    case resolveActivityAndProcessId database processIdText of
        Left err -> return $ Left err
        Right (_processId, targetActivity) -> do
            let activityUuidText = case T.splitOn "_" processIdText of
                    (uuid : _) -> uuid
                    [] -> processIdText
            case UUID.fromText activityUuidText of
                Nothing -> return $ Left $ InvalidUUID $ "Invalid activity UUID: " <> activityUuidText
                Just activityUuid -> do
                    matrixData <- MatrixExport.extractMatrixDebugInfo database activityUuid (debugFlowFilter opts)
                    let inventoryList = MatrixExport.mdInventoryVector matrixData
                        bioFlowUUIDs = MatrixExport.mdBioFlowUUIDs matrixData
                        inventory = M.fromList $ zip (V.toList bioFlowUUIDs) inventoryList

                    Progress.reportProgress Progress.Info $ "DEBUG: Starting CSV export to " ++ debugOutput opts
                    MatrixExport.exportMatrixDebugCSVs (debugOutput opts) matrixData
                    Progress.reportProgress Progress.Info "DEBUG: CSV export completed"

                    let summary =
                            M.fromList
                                [ ("activity_uuid" :: Text, UUID.toText activityUuid)
                                , ("activity_name" :: Text, activityName targetActivity)
                                , ("total_inventory_flows" :: Text, T.pack $ show $ M.size inventory)
                                , ("matrix_debug_exported" :: Text, "CSV_EXPORTED")
                                , ("supply_chain_file" :: Text, T.pack $ debugOutput opts ++ "_supply_chain.csv")
                                , ("biosphere_matrix_file" :: Text, T.pack $ debugOutput opts ++ "_biosphere_matrix.csv")
                                ]
                    return $ Right $ toJSON summary

-- | Export matrices in universal matrix format (delegates to Matrix.Export)
exportUniversalMatrixFormat :: FilePath -> Database -> IO ()
exportUniversalMatrixFormat = MatrixExport.exportUniversalMatrixFormat

-- ──────────────────────────────────────────────
-- Plugin validation hooks
-- ──────────────────────────────────────────────

-- | Run pre-compute validators (before matrix solve)
runPreComputeValidation :: [ValidateHandle] -> Database -> IO [ValidationIssue]
runPreComputeValidation validators db = do
    let preValidators = filter ((== PreCompute) . vhPhase) validators
    concat <$> mapM (\v -> vhValidate v (ValidateContext db Nothing)) preValidators

-- | Run post-compute validators (after inventory is computed)
runPostComputeValidation :: [ValidateHandle] -> Database -> Inventory -> IO [ValidationIssue]
runPostComputeValidation validators db inv = do
    let postValidators = filter ((== PostCompute) . vhPhase) validators
    concat <$> mapM (\v -> vhValidate v (ValidateContext db (Just inv))) postValidators

-- | Check if any validation issues are errors (should abort computation)
hasValidationErrors :: [ValidationIssue] -> Bool
hasValidationErrors = any ((== Error) . viSeverity)

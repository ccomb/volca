{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Service where

import CLI.Types (DebugMatricesOptions (..))
import Matrix (Inventory, applySparseMatrix, buildDemandVectorFromIndex, computeInventoryMatrix, shermanMorrisonVariant, toList)
import qualified Matrix.Export as MatrixExport
import SharedSolver (SharedSolver, solveWithSharedSolver)
import Database (findActivitiesByFields, findFlowsBySynonym)
import Tree (buildLoopAwareTree)
import Types
import API.Types (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), EdgeType (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), GraphEdge (..), GraphExport (..), GraphNode (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), NodeType (..), SearchResults (..), Substitution (..), SubstitutionResult (..), SupplyChainEntry (..), SupplyChainPathNode (..), SupplyChainResponse (..), TreeEdge (..), TreeExport (..), TreeMetadata (..), VariantRequest (..), VariantResponse (..))
import UnitConversion (UnitConfig, defaultUnitConfig, unitsCompatible)
import Data.Aeson (Value, toJSON)
import Plugin.Types (ValidateHandle(..), ValidateContext(..), ValidationPhase(..), ValidationIssue(..), Severity(..))
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | Domain service errors
data ServiceError
    = InvalidUUID Text
    | InvalidProcessId Text
    | ActivityNotFound Text
    | FlowNotFound Text
    | MatrixError Text  -- Generic error from matrix computations
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

-- | Resolve activity and get both ProcessId and Activity
-- This is the preferred function when you need the ProcessId (e.g., for matrix operations)
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

-- | Validate that a ProcessId exists in the matrix activity index
-- This check ensures we fail fast with clear error messages before expensive matrix operations
-- The activity index is required for building demand vectors and performing inventory calculations
validateProcessIdInMatrixIndex :: Database -> ProcessId -> Either ServiceError ()
validateProcessIdInMatrixIndex db processId =
    if processId >= 0 && fromIntegral processId < V.length (dbActivityIndex db)
    then Right ()
    else Left $ MatrixError $
        "ProcessId not available for matrix calculations: " <>
        T.pack (show processId) <>
        ". This activity may exist in the database but is not indexed for inventory calculations."


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

-- | Core inventory calculation logic using matrix-based LCA calculations
-- | Convert raw inventory to structured export format
convertToInventoryExport :: Database -> ProcessId -> Activity -> Inventory -> InventoryExport
convertToInventoryExport db processId rootActivity inventory =
    let
        -- Filter out flows with zero quantities to reduce noise in the results
        inventoryList = M.toList inventory

        !flowDetails =
            [ InventoryFlowDetail flow quantity uName isEmission category
            | (flowUUID, quantity) <- inventoryList
            , quantity /= 0  -- Exclude flows with zero quantities
            , Just flow <- [M.lookup flowUUID (dbFlows db)]
            , let !uName = getUnitNameForFlow (dbUnits db) flow
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

        !(prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbFlows db) (dbUnits db) rootActivity

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

-- | Determine if a flow represents resource extraction based on flow category
-- Since B matrix now stores all flows as positive (Ecoinvent convention), we use category instead of sign
-- Resource extractions have category starting with "natural resource" (e.g., "natural resource/in ground", "natural resource/in water")
isResourceExtraction :: Flow -> Double -> Bool
isResourceExtraction flow _ =
    flowType flow == Biosphere &&
    ("natural resource" `T.isPrefixOf` T.toLower (flowCategory flow))

-- | Get activity inventory as rich InventoryExport (same as API)
getActivityInventory :: Database -> Text -> IO (Either ServiceError Value)
getActivityInventory db processIdText =
    case resolveActivityAndProcessId db processIdText >>= \(pid, act) -> validateProcessIdInMatrixIndex db pid >> Right (pid, act) of
        Left err -> return $ Left err
        Right (processId, activity) -> do
            -- Matrix computation (will not fail if validation passed)
            inventory <- computeInventoryMatrix db processId
            let !inventoryExport = convertToInventoryExport db processId activity inventory
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
                        then return $ Left $ MatrixError $
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
                                then return $ Left $ MatrixError $
                                    T.intercalate "; " [viMessage i | i <- postIssues, viSeverity i == Error]
                                else do
                                    let inventoryExport = convertToInventoryExport db processId activity inventory
                                    return $ Right inventoryExport

-- | Simple stats tracking for tree processing
data TreeStats = TreeStats Int Int Int -- total, loops, leaves

combineStats :: TreeStats -> TreeStats -> TreeStats
combineStats (TreeStats t1 l1 v1) (TreeStats t2 l2 v2) = TreeStats (t1 + t2) (l1 + l2) (v1 + v2)

-- | Helper to find ProcessId for an activity by searching the database
-- This is needed because activities don't store their own ProcessId/UUID
-- Strategy: match activities by name, location, unit, and first reference product flow
findProcessIdForActivity :: Database -> Activity -> Maybe ProcessId
findProcessIdForActivity db activity =
    let actName = activityName activity
        actLoc = activityLocation activity
        actUnit = activityUnit activity
        refFlowId = case [exchangeFlowId ex | ex <- exchanges activity, exchangeIsReference ex] of
            (fid:_) -> Just fid
            [] -> Nothing

        matchesActivity dbActivity =
            let dbRefFlowId = case [exchangeFlowId ex | ex <- exchanges dbActivity, exchangeIsReference ex] of
                    (dbFid:_) -> Just dbFid
                    [] -> Nothing
            in activityName dbActivity == actName &&
               activityLocation dbActivity == actLoc &&
               activityUnit dbActivity == actUnit &&
               dbRefFlowId == refFlowId

        matchingIndex = V.findIndex matchesActivity (dbActivities db)
    in fmap fromIntegral matchingIndex

-- | Find supplier ProcessId by product flow UUID with fallback to name+unit matching.
-- Primary: UUID lookup in ProductIndex
-- Fallback: When UUID fails (e.g., SimaPro tkm vs kgkm), find by name with unit compatibility check
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
                        [singleMatch] -> Just singleMatch  -- Exactly one match
                        _ -> Nothing  -- Zero or multiple matches
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
            (ex:_) -> getUnitNameForExchange (dbUnits db') ex
            [] -> ""

-- | Helper to get node ID from LoopAwareTree (returns ProcessId format)
-- For all node types, we attempt to return ProcessId format for consistency
getTreeNodeId :: Database -> LoopAwareTree -> Text
getTreeNodeId db (TreeLeaf activity) =
    case findProcessIdForActivity db activity of
        Just processId -> processIdToText db processId
        Nothing -> "unknown-activity"  -- Fallback
getTreeNodeId db (TreeLoop uuid _ _) =
    -- Use ProcessId format for consistency, maintain UUID_UUID format even in fallback
    case findProcessIdByActivityUUID db uuid of
        Just processId -> processIdToText db processId
        Nothing -> UUID.toText uuid <> "_" <> UUID.toText uuid  -- Fallback maintains ProcessId format
getTreeNodeId db (TreeNode activity _) =
    case findProcessIdForActivity db activity of
        Just processId -> processIdToText db processId
        Nothing -> "unknown-activity"  -- Fallback

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
       else if "water" `T.isInfixOf` lowerCategory || "aquatic" `T.isInfixOf` lowerCategory
       then "water"
       else if "soil" `T.isInfixOf` lowerCategory || "ground" `T.isInfixOf` lowerCategory
       then "soil"
       else "other"

-- | Extract biosphere exchanges from an activity and create nodes and edges
extractBiosphereNodesAndEdges :: Database -> Activity -> Text -> Int -> M.Map Text ExportNode -> [TreeEdge] -> (M.Map Text ExportNode, [TreeEdge])
extractBiosphereNodesAndEdges db activity activityProcessId depth nodeAcc edgeAcc =
    let allBiosphereExchanges = [ex | ex <- exchanges activity, isBiosphereExchange ex]
        -- Limit to top 50 most significant flows to prevent performance issues with system processes
        maxBiosphereFlows = 50
        biosphereExchanges = take maxBiosphereFlows $
                            L.sortBy (\a b -> compare (abs (exchangeAmount b)) (abs (exchangeAmount a)))
                            allBiosphereExchanges
        processBiosphere ex (nodeAcc', edgeAcc') =
            case M.lookup (exchangeFlowId ex) (dbFlows db) of
                Nothing -> (nodeAcc', edgeAcc')
                Just flow ->
                    let flowIdText = UUID.toText (flowId flow)
                        isEmission = not (exchangeIsInput ex)  -- False = emission, True = resource
                        nodeType = if isEmission then BiosphereEmissionNode else BiosphereResourceNode
                        compartment = extractCompartment (flowCategory flow)
                        biosphereNode = ExportNode
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
                        (edgeFrom, edgeTo, edgeType) = if isEmission
                            then (activityProcessId, flowIdText, BiosphereEmissionEdge)
                            else (flowIdText, activityProcessId, BiosphereResourceEdge)
                        edge = TreeEdge
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
            (nodes'', edges') = if depth == 0
                                then extractBiosphereNodesAndEdges db activity processIdText depth nodes' edgeAcc
                                else (nodes', edgeAcc)
         in (nodes'', edges', TreeStats 1 0 1)
    TreeLoop uuid name loopDepth ->
        let nodeId = getTreeNodeId db tree  -- Use ProcessId format for consistency
            uuidText = UUID.toText uuid  -- Keep bare UUID for loopTarget
            -- Look up the actual activity to get real unit and location
            maybeActivity = findActivityByActivityUUID db uuid
            (actualLocation, actualUnit) = case maybeActivity of
                Just activity -> (activityLocation activity, activityUnit activity)
                Nothing -> ("N/A", "N/A")  -- Fallback only if activity not found
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
            nodes' = M.insert nodeId node nodeAcc  -- Store with ProcessId format key
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
            (finalNodesWithBio, finalEdgesWithBio) = if depth == 0
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
getActivityTree :: Database -> Text -> Int -> Either ServiceError Value
getActivityTree db queryText maxDepth = do
    (_processId, _activity) <- resolveActivityAndProcessId db queryText
    -- Get the activity UUID from the processIdText (which is activityUUID_productUUID)
    let activityUuidText = case T.splitOn "_" queryText of
            (uuid:_) -> uuid
            [] -> queryText  -- Fallback
    case UUID.fromText activityUuidText of
        Just activityUuid ->
            let loopAwareTree = buildLoopAwareTree defaultUnitConfig db activityUuid maxDepth
                treeExport = convertToTreeExport db queryText maxDepth loopAwareTree
             in Right $ toJSON treeExport
        Nothing -> Left $ InvalidUUID $ "Invalid activity UUID: " <> activityUuidText

-- | Build activity network graph from factorized matrix column
-- Uses efficient sparse matrix operations to extract connections
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
                            | (idx, val) <- zip [(0 :: Int)..] supplyList
                            , abs val > threshold
                            ]
                        -- Ensure root activity is always included
                        significantActivities =
                            if processId `elem` map fst allSignificantActivities
                            then allSignificantActivities
                            else let rootValue = if fromIntegral processId < length supplyList
                                                then supplyList !! fromIntegral processId
                                                else 0.0
                                in (processId, rootValue) : allSignificantActivities

                    -- Step 3: Build node ID mapping (ProcessId -> Int) for frontend efficiency
                    let nodeIdMap = M.fromList [(pid, idx) | (idx, (pid, _)) <- zip [0..] significantActivities]

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
                                  sourceActivity = if fromIntegral row < V.length activities
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
                                      let techExchanges = [ex | ex <- exchanges srcAct,
                                                                case ex of
                                                                    TechnosphereExchange _ _ _ isInput isRef _ _ _ -> isInput && not isRef
                                                                    _ -> False]
                                      -- Match by target activity UUID
                                      case [ex | ex <- techExchanges, exchangeActivityLinkId ex == Just targetUUID] of
                                          (ex:_) -> M.lookup (exchangeFlowId ex) flows
                                          [] -> Nothing  -- No matching exchange found
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
                            [ let activity = if fromIntegral pid < V.length activities
                                            then activities V.! fromIntegral pid
                                            else error $ "Invalid ProcessId in graph: " ++ show pid
                                  processIdText = processIdToText db pid
                              in GraphNode
                                  { gnId = nodeId
                                  , gnLabel = activityName activity
                                  , gnValue = cumulativeVal
                                  , gnUnit = activityUnit activity
                                  , gnProcessId = processIdText
                                  , gnLocation = activityLocation activity
                                  }
                            | (nodeId, (pid, cumulativeVal)) <- zip [0..] significantActivities
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

-- | Search flows (returns same format as API)
searchFlows :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> IO (Either ServiceError Value)
searchFlows _ Nothing _ _ _ = return $ Right $ toJSON $ SearchResults ([] :: [FlowSearchResult]) 0 0 50 False 0.0
searchFlows db (Just query) langParam limitParam offsetParam = do
    startTime <- getCurrentTime
    let _lang = maybe "en" id langParam
        limit = maybe 50 (min 1000) limitParam
        offset = maybe 0 (max 0) offsetParam
        allResults = findFlowsBySynonym db query
        total = length allResults
        pagedResults = take limit $ drop offset allResults
        hasMore = offset + limit < total
        flowResults = map (\flow -> FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow))) pagedResults
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ Right $ toJSON $ SearchResults flowResults total offset limit hasMore searchTimeMs

-- | Search activities (returns same format as API)
searchActivities :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> IO (Either ServiceError Value)
searchActivities db nameParam geoParam productParam limitParam offsetParam = do
    startTime <- getCurrentTime
    let limit = maybe total id limitParam
        offset = maybe 0 (max 0) offsetParam
        allResults = findActivitiesByFields db nameParam geoParam productParam
        total = length allResults
        pagedResults = take limit $ drop offset allResults
        hasMore = offset + limit < total
        activityResults =
            map
                ( \activity ->
                    let (prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbFlows db) (dbUnits db) activity
                    in case findProcessIdForActivity db activity of
                        Just processId ->
                            ActivitySummary
                                (processIdToText db processId)
                                (activityName activity)
                                (activityLocation activity)
                                prodName
                                prodAmount
                                prodUnit
                        Nothing ->
                            -- Fallback if ProcessId not found
                            ActivitySummary
                                "unknown"
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

-- | Convert Activity to ActivityForAPI with unit names
-- Note: This function requires the ProcessId to get the activity UUID
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
                            Just pid | Just act <- getActivity db pid ->
                                (Just (activityName act), Just (activityLocation act), Just (processIdToText db pid))
                            _ -> -- Try cross-DB links
                                case flowInfo >>= \flow -> M.lookup (T.toLower (flowName flow)) crossDBLinkMap of
                                    Just link ->
                                        let crossPid = cdlSourceDatabase link <> "::"
                                                <> UUID.toText (cdlSupplierActUUID link) <> "_"
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
                { prsId = processIdToText db pid
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
            { prsId = processIdToText db processId
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

-- | Helper function to get detailed exchanges with filtering
getActivityExchangeDetails :: Database -> Activity -> (Exchange -> Bool) -> [ExchangeDetail]
getActivityExchangeDetails db activity filterFn =
    [ ExchangeDetail exchange flow (getUnitNameForFlow (dbUnits db) flow) unit (getUnitNameForExchange (dbUnits db) exchange) (getTargetActivity db exchange)
    | exchange <- exchanges activity
    , filterFn exchange
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]

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

-- | Compute the supply chain for an activity using the scaling vector.
-- Returns all upstream activities with their scaling factors and shortest paths.
getSupplyChain :: Database -> SharedSolver -> Text -> Maybe Text -> Maybe Int -> Maybe Double
              -> IO (Either ServiceError SupplyChainResponse)
getSupplyChain db sharedSolver processIdText nameFilter limitParam minQuantityParam = do
    case resolveActivityAndProcessId db processIdText of
        Left err -> return $ Left err
        Right (processId, rootActivity) -> do
            case validateProcessIdInMatrixIndex db processId of
                Left err -> return $ Left err
                Right () -> do
                    -- Solve (I-A)x = d for scaling vector
                    let activityIndex = dbActivityIndex db
                        demandVec = buildDemandVectorFromIndex activityIndex processId
                    supplyVec <- solveWithSharedSolver sharedSolver demandVec
                    let supplyList = toList supplyVec

                    -- BFS from root through technosphere matrix for shortest paths
                    let parentMap = bfsUpstream (dbTechnosphereTriples db) (fromIntegral processId)

                    -- Collect all non-zero entries
                    let minQ = maybe 0 id minQuantityParam
                        limit = maybe 100 id limitParam
                        allEntries =
                            [ (fromIntegral idx :: ProcessId, val)
                            | (idx, val) <- zip [(0 :: Int)..] supplyList
                            , abs val > minQ
                            , fromIntegral idx /= (processId :: ProcessId)  -- exclude root
                            ]

                    -- Filter by name (case-insensitive substring)
                    let nameMatches activity = case nameFilter of
                            Nothing -> True
                            Just pat -> T.toCaseFold pat `T.isInfixOf` T.toCaseFold (activityName activity)

                        filtered =
                            [ (pid, val, activity)
                            | (pid, val) <- allEntries
                            , let activity = dbActivities db V.! fromIntegral pid
                            , nameMatches activity
                            ]

                    -- Sort by quantity descending, apply limit
                    let sorted = take limit $ L.sortBy (\(_, a, _) (_, b, _) -> compare (abs b) (abs a)) filtered

                    -- Build response entries with paths
                    let mkEntry (pid, scalingFactor, activity) =
                            let refAmount = getReferenceProductAmount activity
                                path = reconstructPath parentMap (fromIntegral processId) (fromIntegral pid)
                                pathNodes = map (\i ->
                                    let a = dbActivities db V.! i
                                    in SupplyChainPathNode (processIdToText db (fromIntegral i)) (activityName a)
                                    ) path
                            in SupplyChainEntry
                                { sceProcessId = processIdToText db pid
                                , sceName = activityName activity
                                , sceLocation = activityLocation activity
                                , sceQuantity = scalingFactor * refAmount
                                , sceUnit = activityUnit activity
                                , sceScalingFactor = scalingFactor
                                , scePath = pathNodes
                                }

                        rootSummary = ActivitySummary
                            { prsId = processIdToText db processId
                            , prsName = activityName rootActivity
                            , prsLocation = activityLocation rootActivity
                            , prsProduct = maybe (activityName rootActivity) id
                                (getReferenceProductName (dbFlows db) rootActivity)
                            , prsProductAmount = getReferenceProductAmount rootActivity
                            , prsProductUnit = activityUnit rootActivity
                            }

                    return $ Right SupplyChainResponse
                        { scrRoot = rootSummary
                        , scrTotalActivities = length allEntries
                        , scrFilteredActivities = length filtered
                        , scrSupplyChain = map mkEntry sorted
                        }

-- | Get reference product amount for an activity (defaults to 1.0)
getReferenceProductAmount :: Activity -> Double
getReferenceProductAmount activity =
    case [exchangeAmount ex | ex <- exchanges activity, exchangeIsReference ex] of
        (amt:_) -> amt
        [] -> 1.0

-- | BFS from root through the technosphere matrix.
-- A[row, col] > 0 means activity col consumes product from activity row.
-- Returns parent map: node → parent (for path reconstruction).
bfsUpstream :: U.Vector SparseTriple -> Int -> M.Map Int Int
bfsUpstream techTriples root =
    let -- Build adjacency: consumer (col) → [supplier (row)]
        adj = M.fromListWith (++) [(fromIntegral col, [fromIntegral row])
                                  | SparseTriple row col _val <- U.toList techTriples
                                  , row /= col]
        -- BFS
        go [] parents = parents
        go (node:queue) parents =
            let neighbors = M.findWithDefault [] node adj
                unvisited = filter (`M.notMember` parents) neighbors
                parents' = L.foldl' (\p n -> M.insert n node p) parents unvisited
            in go (queue ++ unvisited) parents'
    in go [root] (M.singleton root root)

-- | Reconstruct path from root to target using BFS parent map
reconstructPath :: M.Map Int Int -> Int -> Int -> [Int]
reconstructPath parents root target
    | target == root = [root]
    | M.notMember target parents = []  -- unreachable
    | otherwise = reverse $ go target []
  where
    go node acc
        | node == root = root : acc
        | otherwise = case M.lookup node parents of
            Just parent -> go parent (node : acc)
            Nothing -> []

-- | Create a variant by applying Sherman-Morrison rank-1 updates.
-- Each substitution swaps one supplier for another in the technosphere matrix.
createVariant :: Database -> SharedSolver -> Text -> VariantRequest
              -> IO (Either ServiceError VariantResponse)
createVariant db sharedSolver processIdText varReq = do
    case resolveActivityAndProcessId db processIdText of
        Left err -> return $ Left err
        Right (processId, _rootActivity) -> do
            case validateProcessIdInMatrixIndex db processId of
                Left err -> return $ Left err
                Right () -> do
                    -- Get original scaling vector
                    let activityIndex = dbActivityIndex db
                        demandVec = buildDemandVectorFromIndex activityIndex processId
                    originalX <- solveWithSharedSolver sharedSolver demandVec

                    -- Apply substitutions sequentially (Sherman-Morrison-Woodbury)
                    let applySubstitution x sub = do
                            case (resolveActivityAndProcessId db (subFrom sub),
                                  resolveActivityAndProcessId db (subTo sub)) of
                                (Left err, _) -> return $ Left err
                                (_, Left err) -> return $ Left err
                                (Right (oldPid, _), Right (newPid, _)) -> do
                                    -- Find the exchange coefficient from the tech matrix
                                    let coeff = findTechCoefficient db processId oldPid
                                    case coeff of
                                        Nothing -> return $ Left $ MatrixError $
                                            "No technosphere link from " <> processIdToText db processId
                                            <> " to supplier " <> subFrom sub
                                        Just a -> do
                                            smResult <- shermanMorrisonVariant db x
                                                (fromIntegral processId)
                                                (fromIntegral oldPid)
                                                (fromIntegral newPid) a
                                            return $ case smResult of
                                                Left msg -> Left $ MatrixError msg
                                                Right x' -> Right x'

                    -- Fold substitutions
                    let subs = vrSubstitutions varReq
                    result <- foldSubstitutions originalX subs applySubstitution

                    case result of
                        Left err -> return $ Left err
                        Right variantX -> do
                            -- Build response with variant supply chain
                            let supplyList = toList variantX
                                entries =
                                    [ (fromIntegral idx :: ProcessId, val)
                                    | (idx, val) <- zip [(0 :: Int)..] supplyList
                                    , abs val > 1e-12
                                    , fromIntegral idx /= (processId :: ProcessId)
                                    ]
                                -- Sort by absolute value descending, take top 100
                                sorted = take 100 $ L.sortBy (\(_, a) (_, b) -> compare (abs b) (abs a)) entries

                                mkEntry (pid, scalingFactor) =
                                    let activity = dbActivities db V.! fromIntegral pid
                                        refAmount = getReferenceProductAmount activity
                                    in SupplyChainEntry
                                        { sceProcessId = processIdToText db pid
                                        , sceName = activityName activity
                                        , sceLocation = activityLocation activity
                                        , sceQuantity = scalingFactor * refAmount
                                        , sceUnit = activityUnit activity
                                        , sceScalingFactor = scalingFactor
                                        , scePath = []  -- No BFS for variant (performance)
                                        }

                                subResults = [ SubstitutionResult
                                    { sbrFrom = subFrom s
                                    , sbrTo = subTo s
                                    , sbrCoefficient = maybe 0 id $
                                        case resolveActivityAndProcessId db (subFrom s) of
                                            Right (oldPid, _) -> findTechCoefficient db processId oldPid
                                            _ -> Nothing
                                    } | s <- subs ]

                            return $ Right VariantResponse
                                { varOriginalProcessId = processIdToText db processId
                                , varSubstitutions = subResults
                                , varSupplyChain = map mkEntry sorted
                                , varTotalActivities = length entries
                                }
  where
    foldSubstitutions :: U.Vector Double -> [Substitution]
                      -> (U.Vector Double -> Substitution -> IO (Either ServiceError (U.Vector Double)))
                      -> IO (Either ServiceError (U.Vector Double))
    foldSubstitutions x [] _ = return $ Right x
    foldSubstitutions x (s:ss) f = do
        result <- f x s
        case result of
            Left (MatrixError msg) -> return $ Left $ MatrixError msg
            Left err -> return $ Left err
            Right x' -> foldSubstitutions x' ss f

-- | Find the technosphere coefficient A[supplier, consumer] from the sparse triples
findTechCoefficient :: Database -> ProcessId -> ProcessId -> Maybe Double
findTechCoefficient db consumer supplier =
    let techTriples = dbTechnosphereTriples db
        consumerIdx = fromIntegral consumer :: Int32
        supplierIdx = fromIntegral supplier :: Int32
        matching = U.filter (\(SparseTriple row col _) -> row == supplierIdx && col == consumerIdx)
                            techTriples
    in if U.null matching
       then Nothing
       else let SparseTriple _ _ val = U.head matching in Just val

-- | Get available synonym languages (placeholder)
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
                    (uuid:_) -> uuid
                    [] -> processIdText
            case UUID.fromText activityUuidText of
                Nothing -> return $ Left $ InvalidUUID $ "Invalid activity UUID: " <> activityUuidText
                Just activityUuid -> do
                    matrixData <- MatrixExport.extractMatrixDebugInfo database activityUuid (debugFlowFilter opts)
                    let inventoryList = MatrixExport.mdInventoryVector matrixData
                        bioFlowUUIDs = MatrixExport.mdBioFlowUUIDs matrixData
                        inventory = M.fromList $ zip (V.toList bioFlowUUIDs) inventoryList

                    putStrLn $ "DEBUG: Starting CSV export to " ++ debugOutput opts
                    MatrixExport.exportMatrixDebugCSVs (debugOutput opts) matrixData
                    putStrLn $ "DEBUG: CSV export completed"

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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Service where

import API.Types (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ApiFlow (..), ClassificationSystem (..), ConsumerResult (..), ConsumersResponse (..), CutoffWasteFlow (..), EdgeType (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), GraphEdge (..), GraphExport (..), GraphNode (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), NodeType (..), Perturbation (..), ProducerFilter (..), RootDb (..), SearchResults (..), Substitution (..), SubstitutionScope (..), SupplyChainEdge (..), SupplyChainEntry (..), SupplyChainResponse (..), ThisDb (..), TreeEdge (..), TreeExport (..), TreeMetadata (..), apiFlowOfKind, parseSubRef, subAnchorRef, unresolvedFlowName)
import CLI.Types (DebugMatricesOptions (..))
import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (foldM, guard)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Either (fromRight, lefts, rights)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (Geographies, IdentifierReach (..), activitiesIdentifiedBy, applyStructuredFilters, findActivitiesByFields, findFlowsBySynonym, flowNameRelevance, locationAnswers)
import Database.Allocation (asAllocated, describeRefusal, propertyShares)
import Database.MatrixBuild (findProducer, linkedProducer)
import Matrix (Demand (..), DepDemands, Inventory, SupplierDemands, accumulateDepDemandsWith, activityNormalizationFactor, applyBiosphereMatrix, buildDemandVectorFromIndex, computeInventoryMatrix, depDemandsToVector, perturbA, perturbABatch, perturbGlobal, toList)
import qualified Matrix.Export as MatrixExport
import qualified Progress
import qualified Search.BM25 as BM25
import qualified Search.Fuzzy as Fuzzy
import qualified Search.Normalize as Normalize
import SharedSolver (SharedSolver, getFactorization, solveWithSharedSolver)
import qualified SharedSolver
import Tree (childTarget)
import Types
import UnitConversion (UnitConfig, convertUnit)

{- | Fields shared by every activity-oriented endpoint (search, supply chain,
consumers). Split out from the endpoint-specific filters so each filter
type carries exactly the knobs it can act on – no more "ignored in this
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

{- | Whether a walk also reports every technosphere edge inside the subgraph it
reached. Edges cost an extra pass over the triples, so they are asked for.
-}
data Edges = WithEdges | EntriesOnly

{- | Filter for supply-chain walks. Adds the depth cap and the magnitude
cut-off that only make sense downstream from a root activity.
-}
data SupplyChainFilter = SupplyChainFilter
    { scfCore :: !ActivityFilterCore
    , scfMaxDepth :: !(Maybe Int)
    , scfMinQuantity :: !(Maybe Double)
    , scfEdges :: !Edges
    }

{- | Filter for reverse-walk (/consumers). Adds a depth cap but no
'minQuantity' – scaling factors are meaningless in the reverse direction.
-}
data ConsumerFilter = ConsumerFilter
    { cnfCore :: !ActivityFilterCore
    , cnfMaxDepth :: !(Maybe Int)
    , cnfEdges :: !Edges -- 'WithEdges' emits every technosphere edge inside the reachable consumer subgraph
    }

{- | Filter for flow search. 'ffQuery' is required; callers that have no
query at all should short-circuit and return 'emptyFlowSearchResults'
before building this record.
-}
data FlowFilter = FlowFilter
    { ffQuery :: Text
    , ffLang :: Maybe Text
    , ffKind :: KindFilter
    , ffLimit :: Maybe Int
    , ffOffset :: Maybe Int
    , ffSort :: Maybe Text
    , ffOrder :: Maybe Text
    }

{- | A case-blind substring a name is searched for, as opposed to a name, a
process reference or a database name. Compared with
'Normalize.caseInsensitiveInfixOf', never parsed.
-}
newtype NamePattern = NamePattern {unNamePattern :: Text}
    deriving (Eq, Show)

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
    | {- | A bare activity UUID naming an activity written as several rows. The
      activity exists, so this is not a not-found; it is under-specified, and
      the caller has to name the product too.
      -}
      AmbiguousActivity Text
    | ActivityNotFound Text
    | FlowNotFound Text
    | {- | The activity is there and can be read, but the allocation gate
      refused it a matrix column, so it has no score. The text says why and
      what would repair it.
      -}
      NotScorable Text
    | MatrixError Text -- Generic error from matrix computations
    deriving (Show)

{- | Validate UUID format, returning the parsed UUID so callers do not have to
re-parse the text afterwards.
-}
validateUUID :: Text -> Either ServiceError UUID.UUID
validateUUID uuidText = case UUID.fromText uuidText of
    Just uuid -> Right uuid
    Nothing -> Left $ InvalidUUID $ "Invalid UUID format: " <> uuidText

-- | Find activity by ProcessId using direct Vector access
findActivityByProcessId :: Database -> ProcessId -> Maybe Activity
findActivityByProcessId db processId =
    if processId >= 0 && fromIntegral processId < V.length (dbActivities db)
        then Just $ dbActivities db V.! fromIntegral processId
        else Nothing

-- | Resolve activity query using ProcessId format with UUID fallback for compatibility
resolveActivityByProcessId :: Database -> Text -> Either ServiceError Activity
resolveActivityByProcessId db = fmap snd . resolveActivityAndProcessId db

{- | Resolve an activity that is about to be scored. One the allocation gate
refused resolves for inspection, never for a score: the refusal names what is
missing, and 'Database.MatrixBuild' left its column empty on the same verdict.
-}
resolveScorable :: Database -> Text -> Either ServiceError (ProcessId, Activity)
resolveScorable db queryText = do
    (pid, act) <- resolveActivityAndProcessId db queryText
    case asAllocated act of
        Right _ -> Right (pid, act)
        Left refusal ->
            Left . NotScorable $
                "Activity \"" <> activityName act <> "\" (" <> queryText <> ") cannot be scored: " <> describeRefusal refusal

{- | Resolve activity and get both ProcessId and Activity, for reading.
Anything that scores goes through 'resolveScorable', which adds the
allocation gate; a matrix operation reached from here bypasses it.
-}
resolveActivityAndProcessId :: Database -> Text -> Either ServiceError (ProcessId, Activity)
resolveActivityAndProcessId db queryText =
    findPid >>= resolveActivity
  where
    notFound = ActivityNotFound queryText
    -- A syntactically valid identifier that does not resolve is not-found, not
    -- a format error. Only genuinely malformed input is 'InvalidProcessId'.
    -- An activity written as several rows is neither: it is there, and the
    -- query names it without saying which of its products is meant.
    findPid
        | Just ref <- parseProcessRef queryText = maybe (Left notFound) Right (findProcessId db (prActivity ref) (prProduct ref))
        -- Bare activity UUID fallback (EcoInvent compatibility).
        | Just uuid <- UUID.fromText queryText = case M.lookup uuid (dbActivityUUIDIndex db) of
            Just (pid NE.:| []) -> Right pid
            Just rows -> Left (AmbiguousActivity (ambiguous rows))
            Nothing -> Left notFound
        | otherwise =
            Left $ InvalidProcessId $ "Query must be a ProcessId (activityUUID_productUUID) or a valid UUID: " <> queryText
    resolveActivity pid =
        maybe (Left notFound) (\act -> Right (pid, act)) (findActivityByProcessId db pid)
    ambiguous rows =
        queryText
            <> " names an activity written as "
            <> T.pack (show (NE.length rows))
            <> " processes, one per product it makes. Name one of them as activityUUID_productUUID."

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

{- | The demand vector that puts the functional unit on @processId@, with the
builder's refusal carried as the matrix error it is. A process id that reached
here without being resolved is the only way to get a 'Left'.
-}
demandFor :: Database -> ProcessId -> Either ServiceError Demand
demandFor db processId =
    either (Left . MatrixError) Right (buildDemandVectorFromIndex (dbActivityIndex db) processId)

-- | Rich activity info (returns same format as API)
getActivityInfo :: Database -> Text -> Either ServiceError Value
getActivityInfo db queryText = do
    (processId, activity) <- resolveActivityAndProcessId db queryText
    let activityForAPI = convertActivityForAPI db processId activity
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

The 'BioFlowDB'/'UnitDB' arguments are independent of the root DB so that
cross-DB-merged inventories (whose flow UUIDs can originate in any loaded
dep DB) can be decoded against a merged metadata snapshot. For single-DB
callers, pass @dbBioFlows db@ / @dbUnits db@ directly.
-}
convertToInventoryExport :: Database -> BioFlowDB -> UnitDB -> ProcessId -> Activity -> Inventory -> InventoryExport
convertToInventoryExport db bioFlowDB unitDB processId rootActivity inventory =
    let
        -- Inventory flows are biosphere by construction (rows of B matrix).
        inventoryList = M.toList inventory

        !flowDetails =
            [ InventoryFlowDetail flow quantity uName isEmission category
            | (flowUUID, quantity) <- inventoryList
            , quantity /= 0
            , Just flow <- [M.lookup flowUUID bioFlowDB]
            , let !uName = getUnitNameForBioFlow unitDB flow
                  !isEmission = not (isResourceExtraction flow)
                  !category = bfCompartmentName flow
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

        !(prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbTechFlows db) unitDB rootActivity
        !rootBlock = sourceBlockOf db processId

        !metadata =
            InventoryMetadata
                { imRootActivity =
                    ActivitySummary
                        { prsProcessId = processIdToText db processId
                        , prsActivityName = activityName rootActivity
                        , prsLocation = activityLocation rootActivity
                        , prsProductName = prodName
                        , prsProductAmount = prodAmount
                        , prsProductUnit = prodUnit
                        , prsAllocationPercent = dsPercent <$> activityReferenceShare rootActivity
                        , prsAllocationFormula = dsFormula =<< activityReferenceShare rootActivity
                        , prsMassAllocationPercent = Nothing
                        , prsNativeType = activityNativeType rootActivity
                        , prsBlock = sbName rootBlock
                        , prsBlockProducts = sbProducts rootBlock
                        }
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

{- | Determine if a biosphere flow represents resource extraction based on its
compartment. Now type-restricted to BiosphereFlow – technosphere can't reach
this code path at compile time.
-}
isResourceExtraction :: BiosphereFlow -> Bool
isResourceExtraction flow = (compartmentName <$> bfCompartment flow) == Just NaturalResource

-- | Get activity inventory as rich InventoryExport (same as API)
getActivityInventory :: Database -> Text -> IO (Either ServiceError Value)
getActivityInventory db processIdText =
    case resolveScorable db processIdText >>= \(pid, act) -> validateProcessIdInMatrixIndex db pid >> Right (pid, act) of
        Left err -> return $ Left err
        Right (processId, activity) -> do
            inventoryE <- computeInventoryMatrix db processId
            return $ case inventoryE of
                Left err -> Left (MatrixError err)
                Right inventory ->
                    let !inventoryExport = convertToInventoryExport db (dbBioFlows db) (dbUnits db) processId activity inventory
                     in Right (toJSON inventoryExport)

{- | The nodes and edges a tree walk has reached so far. Threaded through the
walk in visit order and never combined out of it: a node id can be inserted
twice (two branches looping to the same row give two 'TreeLoop' nodes, and one
row reached under two consumers gives two nodes with different parents), and
the walk order is what decides which one the export carries.
-}
data TreeBuild = TreeBuild
    { tbNodes :: M.Map Text ExportNode
    , tbEdges :: [TreeEdge]
    }

{- | Helper to find ProcessId for an activity by searching the database
This is needed because activities don't store their own ProcessId/UUID
Strategy: match activities by name, location, unit, and first reference product flow
-}
findProcessIdForActivity :: Database -> Activity -> Maybe ProcessId
findProcessIdForActivity db activity =
    let actName = activityName activity
        actLoc = activityLocation activity
        actUnit = activityUnit activity
        refFlowId = exchangeFlowId <$> L.find exchangeIsReference (exchanges activity)

        matchesActivity dbActivity =
            let dbRefFlowId = exchangeFlowId <$> L.find exchangeIsReference (exchanges dbActivity)
             in activityName dbActivity == actName
                    && activityLocation dbActivity == actLoc
                    && activityUnit dbActivity == actUnit
                    && dbRefFlowId == refFlowId

        matchingIndex = V.findIndex matchesActivity (dbActivities db)
     in fmap fromIntegral matchingIndex

{- | Node id of a tree node, in ProcessId format. Every node but one names the
row it was built from; a declared link no row satisfies has none to name, so it
answers with the UUID it carries under a prefix no process id can wear.
-}
getTreeNodeId :: Database -> LoopAwareTree -> Text
getTreeNodeId db = \case
    TreeLeaf pid _ -> processIdToText db pid
    TreeNode pid _ _ -> processIdToText db pid
    TreeLoop pid _ _ -> processIdToText db pid
    TreeMissing uuid _ _ -> "missing:" <> UUID.toText uuid

{- | Count potential children for navigation (technosphere inputs that could be
expanded). It counts what the traversal descends into, 'Tree.childTarget', so a
node's count and the edges leaving it in the same export agree: an input whose
declared supplier is in no loaded database is a child too, the one the export
names a missing node.
-}
countPotentialChildren :: Database -> Activity -> Int
countPotentialChildren db activity =
    length
        [ ex
        | ex <- exchanges activity
        , isTechnosphereExchange ex
        , exchangeIsInput ex
        , not (exchangeIsReference ex)
        , Just _ <- [childTarget db ex]
        ]

{- | Cap on biosphere flows shown per activity. System processes can declare
hundreds; we keep the top-N by |amount| to keep graphs renderable.
-}
maxBiosphereFlows :: Int
maxBiosphereFlows = 50

-- | ExportNode for a single biosphere flow attached to a parent activity.
mkBiosphereExportNode :: UnitDB -> BiosphereFlow -> Text -> Int -> BioDirection -> ExportNode
mkBiosphereExportNode units flow parentPid depth direction =
    let compartmentTxt = bfCompartmentName flow
     in ExportNode
            { enId = UUID.toText (bfId flow)
            , enName = bfName flow
            , enDescription = [compartmentTxt]
            , enLocation = ""
            , enUnit = getUnitNameForBioFlow units flow
            , enNodeType = case direction of
                Emission -> BiosphereEmissionNode
                Resource -> BiosphereResourceNode
            , enDepth = depth
            , enLoopTarget = Nothing
            , enParentId = Just parentPid
            , enChildrenCount = 0
            , enCompartment = Just compartmentTxt
            }

{- | Edge linking an activity to a biosphere flow. Direction depends on whether
the exchange is an emission (activity -> flow) or a resource (flow -> activity).
-}
mkBiosphereTreeEdge :: UnitDB -> BiosphereFlow -> Text -> BioDirection -> Exchange -> TreeEdge
mkBiosphereTreeEdge units flow activityPid direction ex =
    let flowIdText = UUID.toText (bfId flow)
        (edgeFrom, edgeTo, edgeType) = case direction of
            Emission -> (activityPid, flowIdText, BiosphereEmissionEdge)
            Resource -> (flowIdText, activityPid, BiosphereResourceEdge)
     in TreeEdge
            { teFrom = edgeFrom
            , teTo = edgeTo
            , teFlow = FlowInfo (bfId flow) (bfName flow) (bfCompartmentName flow)
            , teQuantity = exchangeAmount ex
            , teUnit = getUnitNameForBioFlow units flow
            , teEdgeType = edgeType
            }

-- | Extract biosphere exchanges from an activity and create nodes and edges.
extractBiosphereNodesAndEdges :: Database -> Activity -> Text -> Int -> TreeBuild -> TreeBuild
extractBiosphereNodesAndEdges db activity activityProcessId depth acc0 =
    foldr step acc0 topBiosphereExchanges
  where
    units :: UnitDB
    units = dbUnits db
    -- The generator both selects the biosphere rows and reads the direction off
    -- the constructor that carries it, so no later step has to recover it.
    topBiosphereExchanges :: [(BioDirection, Exchange)]
    topBiosphereExchanges =
        take maxBiosphereFlows $
            L.sortBy (\a b -> compare (abs (exchangeAmount (snd b))) (abs (exchangeAmount (snd a)))) $
                [(dir, ex) | ex@BiosphereExchange{bioDirection = dir} <- exchanges activity]
    step :: (BioDirection, Exchange) -> TreeBuild -> TreeBuild
    step (direction, ex) acc = case M.lookup (exchangeFlowId ex) (dbBioFlows db) of
        Nothing -> acc
        Just flow ->
            let node = mkBiosphereExportNode units flow activityProcessId depth direction
                edge = mkBiosphereTreeEdge units flow activityProcessId direction ex
             in TreeBuild (M.insert (UUID.toText (bfId flow)) node (tbNodes acc)) (edge : tbEdges acc)

-- | ExportNode for an activity-bearing tree node (TreeLeaf or TreeNode).
mkActivityExportNode :: Database -> Activity -> Text -> Int -> Maybe Text -> ExportNode
mkActivityExportNode db activity nodeId depth parentId =
    ExportNode
        { enId = nodeId
        , enName = activityName activity
        , enDescription = activityDescription activity
        , enLocation = activityLocation activity
        , enUnit = activityUnit activity
        , enNodeType = ActivityNode
        , enDepth = depth
        , enLoopTarget = Nothing
        , enParentId = parentId
        , enChildrenCount = countPotentialChildren db activity
        , enCompartment = Nothing
        }

{- | ExportNode for a TreeLoop. Reads the row it points at for its real unit
and location; falls back to "N/A" sentinels when the referent is missing.
-}
mkLoopExportNode :: Database -> ProcessId -> Text -> Text -> Int -> Maybe Text -> ExportNode
mkLoopExportNode db pid nodeId name loopDepth parentId =
    let (actualLocation, actualUnit) = case getActivity db pid of
            Just act -> (activityLocation act, activityUnit act)
            Nothing -> ("N/A", "N/A")
     in ExportNode
            { enId = nodeId
            , enName = name
            , enDescription = ["Loop reference"]
            , enLocation = actualLocation
            , enUnit = actualUnit
            , enNodeType = LoopNode
            , enDepth = loopDepth
            , enLoopTarget = Just (processIdToText db pid)
            , enParentId = parentId
            , enChildrenCount = 0
            , enCompartment = Nothing
            }

{- | ExportNode for a TreeMissing: a link the source declares that no row in
this database satisfies. It gets a node of its own so the branch stays visible,
and no loop target, having no row to point at.
-}
mkMissingExportNode :: Text -> Text -> Int -> Maybe Text -> ExportNode
mkMissingExportNode nodeId name missingDepth parentId =
    ExportNode
        { enId = nodeId
        , enName = name
        , enDescription = ["Declared link, not loaded"]
        , enLocation = ""
        , enUnit = ""
        , enNodeType = MissingNode
        , enDepth = missingDepth
        , enLoopTarget = Nothing
        , enParentId = parentId
        , enChildrenCount = 0
        , enCompartment = Nothing
        }

{- | Attach biosphere nodes/edges to the accumulator when we're at the root of
the tree (depth == 0). Below the root we leave the accumulator untouched to
keep the graph readable.
-}
withRootBiosphere :: Database -> Activity -> Text -> Int -> TreeBuild -> TreeBuild
withRootBiosphere db activity pid depth acc
    | depth == 0 = extractBiosphereNodesAndEdges db activity pid depth acc
    | otherwise = acc

-- | Technosphere edge from the current node to a child subtree.
mkTechnosphereTreeEdge :: UnitDB -> Text -> Text -> Double -> TechnosphereFlow -> TreeEdge
mkTechnosphereTreeEdge units fromPid toPid quantity flow =
    TreeEdge
        { teFrom = fromPid
        , teTo = toPid
        , teFlow = FlowInfo (tfId flow) (tfName flow) ""
        , teQuantity = quantity
        , teUnit = getUnitNameForTechFlow units flow
        , teEdgeType = TechnosphereEdge
        }

-- | Extract nodes and edges from a 'LoopAwareTree'.
extractNodesAndEdges :: Database -> LoopAwareTree -> Int -> Maybe Text -> TreeBuild -> TreeBuild
extractNodesAndEdges db tree depth parentId acc = case tree of
    TreeLeaf _ activity ->
        let nodeId = getTreeNodeId db tree
         in withRootBiosphere db activity nodeId depth $
                insertNode nodeId (mkActivityExportNode db activity nodeId depth parentId) acc
    TreeLoop pid name loopDepth ->
        let nodeId = getTreeNodeId db tree
         in insertNode nodeId (mkLoopExportNode db pid nodeId name loopDepth parentId) acc
    TreeMissing _ name missingDepth ->
        let nodeId = getTreeNodeId db tree
         in insertNode nodeId (mkMissingExportNode nodeId name missingDepth parentId) acc
    TreeNode _ activity children ->
        let nodeId = getTreeNodeId db tree
            withSelf = insertNode nodeId (mkActivityExportNode db activity nodeId depth parentId) acc
         in withRootBiosphere db activity nodeId depth $
                foldr (processChild nodeId) withSelf children
  where
    insertNode :: Text -> ExportNode -> TreeBuild -> TreeBuild
    insertNode nodeId node built = built{tbNodes = M.insert nodeId node (tbNodes built)}
    processChild :: Text -> TreeChild -> TreeBuild -> TreeBuild
    processChild parentPid TreeChild{childAmount = quantity, childFlow = flow, childSubtree = subtree} built =
        let childBuilt = extractNodesAndEdges db subtree (depth + 1) (Just parentPid) built
            edge = mkTechnosphereTreeEdge (dbUnits db) parentPid (getTreeNodeId db subtree) quantity flow
         in childBuilt{tbEdges = edge : tbEdges childBuilt}

-- | Convert LoopAwareTree to TreeExport format for JSON serialization
convertToTreeExport :: Database -> Int -> LoopAwareTree -> TreeExport
convertToTreeExport db maxDepth tree =
    let TreeBuild nodes edges = extractNodesAndEdges db tree 0 Nothing (TreeBuild M.empty [])
        -- The tree's own root, so tmRootId always names a key in the nodes map.
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

{- | Post-filter a TreeExport by name: keep matching nodes plus all their ancestors up to root.
Uses the enParentId chain already stored in each ExportNode – no extra graph traversal.
-}
filterTreeExport :: NamePattern -> TreeExport -> TreeExport
filterTreeExport pat export =
    let nodes = teNodes export
        matchingIds = M.keysSet $ M.filter (Normalize.caseInsensitiveInfixOf (unNamePattern pat) . enName) nodes
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

{- | Activities whose absolute cumulative value clears the threshold. The root
activity is always surfaced: above threshold it keeps its natural position;
otherwise it is prepended (becoming node 0) with its actual supply value, or
0 when out of bounds.
-}
selectSignificantActivities :: Double -> ProcessId -> [Double] -> [(ProcessId, Double)]
selectSignificantActivities threshold rootPid supplyList =
    let aboveThreshold =
            [ (fromIntegral idx :: ProcessId, val)
            | (idx, val) <- zip [(0 :: Int) ..] supplyList
            , abs val > threshold
            ]
        rootValue = fromMaybe 0.0 (lookup (fromIntegral rootPid :: Int) (zip [0 ..] supplyList))
     in if any ((== rootPid) . fst) aboveThreshold
            then aboveThreshold
            else (rootPid, rootValue) : aboveThreshold

{- | True iff the exchange is a technosphere @Input@ whose link points at the
given target activity. Waste exchanges aren't traversed by the graph builder
(they don't form upstream tech edges).
-}
isInputLinkTo :: UUID -> Exchange -> Bool
isInputLinkTo targetUUID ex@TechnosphereExchange{techRole = Input} =
    exchangeActivityLinkId ex == Just targetUUID
isInputLinkTo _ TechnosphereExchange{} = False
isInputLinkTo _ BiosphereExchange{} = False
isInputLinkTo _ WasteExchange{} = False

{- | Build one 'GraphEdge' from a sparse technosphere triple. Returns 'Nothing'
when either endpoint is outside the projected subgraph (i.e. below cutoff) or
the triple itself is zero. When the supplier flow can't be resolved we still
emit the edge – with sentinel name/unit – so the gap is debuggable instead of
silently dropped.
-}
mkGraphEdgeFromTriple ::
    Database ->
    M.Map ProcessId Int ->
    SparseTriple ->
    Maybe GraphEdge
mkGraphEdgeFromTriple db nodeIdMap (SparseTriple row col value)
    | value == 0.0 = Nothing
    | otherwise = do
        let sourcePid = fromIntegral row :: ProcessId
            targetPid = fromIntegral col :: ProcessId
        src <- M.lookup sourcePid nodeIdMap
        tgt <- M.lookup targetPid nodeIdMap
        let matchingExchange = do
                srcAct <- dbActivities db V.!? fromIntegral row
                targetUUID <- prActivity <$> processIdToRef db targetPid
                L.find (isInputLinkTo targetUUID) (exchanges srcAct)
            flowInfo = matchingExchange >>= \ex -> M.lookup (exchangeFlowId ex) (dbTechFlows db)
            uName = maybe "<unresolved unit>" (getUnitNameForTechFlow (dbUnits db)) flowInfo
            flowName = case (flowInfo, matchingExchange) of
                (Just f, _) -> tfName f
                (Nothing, Just ex) -> unresolvedFlowName (exchangeFlowId ex)
                (Nothing, Nothing) -> "<unresolved flow>"
        pure $ GraphEdge src tgt (realToFrac value) uName flowName

{- | 'GraphNode' for one significant activity. Out-of-bounds 'ProcessId's get a
sentinel node rather than crashing – preserves the project's "no silent
errors, no silent successes" stance.
-}
mkGraphNode :: Database -> Int -> (ProcessId, Double) -> GraphNode
mkGraphNode db nodeId (pid, cumulativeVal) =
    let processIdText = processIdToText db pid
     in case dbActivities db V.!? fromIntegral pid of
            Just activity ->
                GraphNode
                    { gnNodeId = nodeId
                    , gnLabel = activityName activity
                    , gnValue = cumulativeVal
                    , gnUnit = activityUnit activity
                    , gnProcessId = processIdText
                    , gnLocation = activityLocation activity
                    }
            Nothing ->
                GraphNode
                    { gnNodeId = nodeId
                    , gnLabel = "<unresolved activity " <> processIdText <> ">"
                    , gnValue = cumulativeVal
                    , gnUnit = ""
                    , gnProcessId = processIdText
                    , gnLocation = ""
                    }

{- | Build activity network graph from factorized matrix column.
Uses efficient sparse matrix operations to extract connections.
-}
buildActivityGraph :: Database -> SharedSolver -> Text -> Double -> IO (Either ServiceError GraphExport)
buildActivityGraph db sharedSolver queryText cutoffPercent =
    case resolveActivityAndProcessId db queryText >>= \(pid, _) -> (,) pid <$> demandFor db pid of
        Left err -> pure (Left err)
        Right (processId, demandVec) -> do
            supplyVec <- solveWithSharedSolver sharedSolver demandVec
            let supplyList = toList supplyVec
                threshold = sum (map abs supplyList) * (cutoffPercent / 100.0)
                significantActivities = selectSignificantActivities threshold processId supplyList
                nodeIdMap = M.fromList [(pid, idx) | (idx, (pid, _)) <- zip [0 ..] significantActivities]
                edges =
                    mapMaybe
                        (mkGraphEdgeFromTriple db nodeIdMap)
                        (U.toList (dbTechnosphereTriples db))
                nodes = zipWith (mkGraphNode db) [0 ..] significantActivities
                unitGroups = buildUnitGroups (map gnUnit nodes)
            pure $ Right $ GraphExport nodes edges unitGroups

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
    maybe 0 length (M.lookup flowUUID (idxByFlow $ dbIndexes db))

{- | Get flows used by an activity as lightweight summaries. Each exchange
lookup is resolved against the appropriate side (tech vs bio) and wrapped
in 'ApiFlow' to preserve the discriminator for downstream JSON encoders.
An exchange whose flow UUID resolves on neither side becomes an
'ApiUnresolvedFlow' entry rather than being silently dropped – so the
consumer never sees a shorter list than the activity actually carries.
-}
getActivityFlowSummaries :: Database -> Activity -> [FlowSummary]
getActivityFlowSummaries db activity = map mkSummary (exchanges activity)
  where
    mkSummary ex =
        let role = determineFlowRole ex
            exUnit = getUnitNameForExchange (dbUnits db) ex
            fid = exchangeFlowId ex
         in case ex of
                TechnosphereExchange{} -> case M.lookup fid (dbTechFlows db) of
                    Just f -> FlowSummary (ApiTechFlow f) (getUnitNameForTechFlow (dbUnits db) f) (getFlowUsageCount db (tfId f)) role
                    Nothing -> FlowSummary (ApiUnresolvedFlow fid) exUnit 0 role
                BiosphereExchange{} -> case M.lookup fid (dbBioFlows db) of
                    Just f -> FlowSummary (ApiBioFlow f) (getUnitNameForBioFlow (dbUnits db) f) (getFlowUsageCount db (bfId f)) role
                    Nothing -> FlowSummary (ApiUnresolvedFlow fid) exUnit 0 role
                WasteExchange{} -> case M.lookup fid (dbWasteFlows db) of
                    Just f -> FlowSummary (ApiWasteFlow f) (getUnitNameForWasteFlow (dbUnits db) f) (getFlowUsageCount db (wfId f)) role
                    Nothing -> FlowSummary (ApiUnresolvedFlow fid) exUnit 0 role

    determineFlowRole ex
        | exchangeIsReference ex = ReferenceProductFlow
        | exchangeIsInput ex = InputFlow
        | otherwise = OutputFlow

{- | Project matched flows into API results, in the order the filter asks for.

Sorting on the requested column alone leaves flows that share that column in
whatever order the database yields them (their UUID). Seven @Deltamethrin@
flows then arrive interleaved and, since a row shows its medium but not its
sub-compartment, read as duplicates. Every sort key therefore continues with
the remaining displayed fields, so equal-looking rows end up adjacent and
ordered.

With no column asked for, the flows whose name answers the query best come
first ('flowNameRelevance'), because matching word by word returns far more
than the exact name and a client often reads only the first page. Asking for
a column is asking for that column alone, so the ranking steps aside: a
table sorted by name must stay alphabetical.

Shared by the REST and MCP/CLI search paths, which differ only in how they
paginate.
-}
flowSearchResults :: UnitDB -> (FlowKind -> Maybe Int) -> FlowFilter -> [FlowKind] -> [FlowSearchResult]
flowSearchResults units producersOfFlow FlowFilter{ffQuery = query, ffKind = kindParam, ffSort = sortParam, ffOrder = orderParam} =
    L.sortBy (direction (\a b -> compare (sortKey a) (sortKey b))) . map toResult . filter askedFor
  where
    askedFor flow = kindFilterKeeps kindParam (kindOfFlow flow)
    direction = if orderParam == Just "desc" then flip else id
    -- Parsers turn an absent sub-compartment into 'Nothing', never @""@, so
    -- the empty string sorts where 'Nothing' would: ahead of every named one.
    sub = fromMaybe "" . fsrCompartment
    -- A column asked for orders on that column alone, so its key carries a
    -- constant rank; only the default arm ranks.
    sortKey r = case sortParam of
        Just "category" -> (0, fsrCategory r, sub r, fsrName r, fsrUnitName r)
        Just "unit" -> (0, fsrUnitName r, fsrName r, fsrCategory r, sub r)
        Just "name" -> (0, fsrName r, fsrCategory r, sub r, fsrUnitName r)
        _ -> (flowNameRelevance query (fsrName r), fsrName r, fsrCategory r, sub r, fsrUnitName r)
    -- Three-arm projections from Types are total over FlowKind.
    toResult flow =
        FlowSearchResult
            { fsrId = flowKindId flow
            , fsrName = flowKindName flow
            , fsrKind = kindOfFlow flow
            , fsrCategory = flowKindCategory flow
            , fsrCompartment = flowKindCompartmentSub flow
            , fsrUnitName = flowKindUnitName units flow
            , fsrSynonyms = M.map S.toList (flowKindSynonyms flow)
            , fsrProducerCount = producersOfFlow flow
            }

{- | Search flows (returns same format as API). The query is required by the
type; callers with no query return 'emptyFlowSearchResults' directly.
-}
searchFlows :: Database -> FlowFilter -> IO (Either ServiceError Value)
searchFlows db ff@FlowFilter{ffQuery = query, ffLimit = limitParam, ffOffset = offsetParam} = do
    startTime <- getCurrentTime
    let limit = maybe 50 (min 1000) limitParam
        offset = maybe 0 (max 0) offsetParam
        allResults = flowSearchResults (dbUnits db) (producerCount db) ff (findFlowsBySynonym db query)
        total = length allResults
        taken = take (limit + 1) (drop offset allResults)
        hasMore = length taken > limit
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ Right $ toJSON $ SearchResults (take limit taken) total offset limit hasMore searchTimeMs

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
semantics as @/activities@ BM25 search. @Nothing@ means the retrieval could
not run – no BM25 index (only bare test fixtures; production DBs always
carry one) or a query whose fuzzy expansion is empty. What that means is
the caller's call: 'nameFilterSet' treats a present-but-unmatchable query
as \"reject every pid\", never as \"no filter\".
-}
bm25MatchingPids :: Database -> Text -> Maybe IS.IntSet
bm25MatchingPids db =
    fmap (IS.fromList . map (fromIntegral . fst)) . bm25Retrieve db

{- | BM25/fuzzy membership set for the optional @name@ filter carried on any
activity-oriented filter. Blank/absent queries and DBs without a BM25 index
both collapse to @Nothing@ (⇒ predicate accepts every pid). A present query
whose fuzzy expansion matches nothing yields @Just IS.empty@ (⇒ reject every
pid): a non-matching name must return an empty result, never silently
disable the filter.
-}
nameFilterSet :: Database -> Maybe Text -> Maybe IS.IntSet
nameFilterSet db mq = do
    q <- mq
    guard (not (T.null (T.strip q)))
    _ <- dbBM25Index db
    pure (fromMaybe IS.empty (bm25MatchingPids db q))

{- | BM25 retrieval applies only when the user provided a non-empty name
query, didn't request exact matching, and didn't pick an explicit sort
column. Returns 'Nothing' otherwise so the caller falls back to lex-sorted
field matching.
-}
tryBm25Retrieve :: Database -> SearchFilter -> Maybe [(ProcessId, Activity)]
tryBm25Retrieve db (SearchFilter core exactMatch) = do
    q <- afcName core
    guard (not exactMatch)
    guard (afcSort core /= Just "name" && afcSort core /= Just "location")
    guard (not (T.null (T.strip q)))
    bm25Retrieve db q

{- | Lex-comparator for activity rows. Defaults to name; 'Just "location"'
picks the location key.
-}
activityRowComparator :: Maybe Text -> (ProcessId, Activity) -> (ProcessId, Activity) -> Ordering
activityRowComparator (Just "location") (_, a) (_, b) = compare (activityLocation a) (activityLocation b)
activityRowComparator _ (_, a) (_, b) = compare (activityName a) (activityName b)

{- | Apply pagination (offset / limit, defaulting limit to 20) and emit a
'SearchResults' wrapping the projected page. Pure modulo the supplied
@searchTimeMs@.
-}
paginateSearchResults :: Maybe Int -> Maybe Int -> Double -> ((ProcessId, Activity) -> a) -> [(ProcessId, Activity)] -> SearchResults a
paginateSearchResults offsetParam limitParam searchTimeMs project xs =
    let offset = maybe 0 (max 0) offsetParam
        limit = fromMaybe 20 limitParam
        total = length xs
        page = map project (take limit (drop offset xs))
        hasMore = offset + limit < total
     in SearchResults page total offset limit hasMore searchTimeMs

{- | Search activities (returns same format as API). The exact-match toggle is
carried on 'SearchFilter' itself, so there is no separate positional flag.
-}
searchActivities :: Geographies -> Database -> SearchFilter -> IO (Either ServiceError Value)
searchActivities geographies db sFilter@(SearchFilter core _) = do
    startTime <- getCurrentTime
    let allResults = activityMatches geographies db sFilter
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
        results = paginateSearchResults (afcOffset core) (afcLimit core) searchTimeMs (uncurry (mkActivitySummary db)) allResults
    pure $ Right $ toJSON results

{- | Every activity row a filter matches, in the order the search presents
them.

Split out of 'searchActivities' so a caller that only wants how many there
are counts the same rows the list would show. Two matchers would drift, and a
tab counter disagreeing with the tab it labels is worse than no counter.
-}
activityMatches :: Geographies -> Database -> SearchFilter -> [(ProcessId, Activity)]
activityMatches geographies db sFilter@(SearchFilter core exactMatch) =
    identified ++ filter (not . alreadyIdentified) searched
  where
    -- The blocks the query names by identifier, ahead of the search rather than
    -- instead of it: a database whose identifiers are bare numbers would
    -- otherwise let one of them swallow a perfectly ordinary query for that
    -- number. Their own order is the answer (named outright, then named in
    -- part), so the caller's sort no more applies here than on the BM25 branch.
    identified :: [(ProcessId, Activity)]
    identified = structured exactMatch (foldMap (activitiesIdentifiedBy reach (dbActivities db)) (afcName core))

    reach :: IdentifierReach
    reach = if exactMatch then WholeIdentifier else WholeOrFragment

    alreadyIdentified :: (ProcessId, Activity) -> Bool
    alreadyIdentified (pid, _) = IS.member (fromIntegral pid) identifiedPids

    identifiedPids :: IS.IntSet
    identifiedPids = IS.fromList [fromIntegral pid | (pid, _) <- identified]

    searched :: [(ProcessId, Activity)]
    searched = case tryBm25Retrieve db sFilter of
        -- BM25 path: ranked candidates → structured filters → preserve score order.
        Just ranked -> structured False ranked
        -- Non-BM25 path: AND-of-tokens name filter + lex sort.
        Nothing ->
            L.sortBy ordered (findActivitiesByFields geographies db (afcName core) (afcLocation core) (afcProduct core) (afcClassifications core) exactMatch)

    structured :: Bool -> [(ProcessId, Activity)] -> [(ProcessId, Activity)]
    structured = applyStructuredFilters geographies db (afcLocation core) (afcProduct core) (afcClassifications core)

    ordered :: (ProcessId, Activity) -> (ProcessId, Activity) -> Ordering
    ordered
        | afcOrder core == Just "desc" = flip (activityRowComparator (afcSort core))
        | otherwise = activityRowComparator (afcSort core)

{- | How many of each thing one query finds, for the three tabs of a search
box.

The three are disjoint and together cover the database: a process is an
activity row, a product is a technosphere flow, and a flow is what is
exchanged with nature or discarded. Answered in one call because three tabs
should not cost three round trips per keystroke.
-}
data SearchCounts = SearchCounts
    { scProcesses :: !Int
    , scProducts :: !Int
    , scFlows :: !Int
    }
    deriving (Eq, Show)

{- | How a caller is going to list the processes it is about to count.

Not decoration: 'tryBm25Retrieve' drops to the AND-of-tokens matcher when the
caller sorts by name or location, or asks for exact matching, and BM25 keeps
any row matching /one/ token. Counting with different settings from the list
would label a tab "1200" over a table of nine.
-}
data CountAs = CountAs
    { caSort :: !(Maybe Text)
    , caExact :: !Bool
    }

-- | Counted the way the default listing lists.
countAsListed :: CountAs
countAsListed = CountAs{caSort = Nothing, caExact = False}

-- | The counts one query finds. The query is required: an empty box has nothing to count.
searchCounts :: Geographies -> Database -> CountAs -> Text -> SearchCounts
searchCounts geographies db listedAs query =
    SearchCounts
        { scProcesses = length (activityMatches geographies db (nameOnly query))
        , scProducts = count KindTechnosphere
        , scFlows = length matchedFlows - count KindTechnosphere
        }
  where
    matchedFlows :: [FlowKind]
    matchedFlows = findFlowsBySynonym db query

    count :: ExchangeKind -> Int
    count kind = length (filter ((== kind) . kindOfFlow) matchedFlows)

    -- The tab counters describe one search box, so only the name is filtered
    -- on; the sort and exactness come from the caller because they decide
    -- which matcher runs.
    nameOnly :: Text -> SearchFilter
    nameOnly q =
        SearchFilter
            { sfCore =
                ActivityFilterCore
                    { afcName = Just q
                    , afcLocation = Nothing
                    , afcProduct = Nothing
                    , afcClassifications = []
                    , afcLimit = Nothing
                    , afcOffset = Nothing
                    , afcSort = caSort listedAs
                    , afcOrder = Nothing
                    }
            , sfExactMatch = caExact listedAs
            }

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

{- | Consumer-side flow UUIDs on @activity@ that the load-time cross-DB linker
has explicitly resolved via 'dbCrossDBLinks'. Used to distinguish a *true*
cut-off / orphan from an exchange whose demand is routed to a supplier in
another loaded database (and therefore contributes to the score via the
matrix path). Empty when the activity has no entry in the process-id table.
-}
crossDBResolvedFlowIds :: Database -> Activity -> S.Set UUID
crossDBResolvedFlowIds db activity =
    case prActivity <$> (findProcessIdForActivity db activity >>= processIdToRef db) of
        Nothing -> S.empty
        Just actUUID ->
            S.fromList
                [ cdlConsumerFlowId link
                | link <- dbCrossDBLinks db
                , cdlConsumerActUUID link == actUUID
                , cdlConsumerFlowId link /= UUID.nil
                ]

{- | Build the list of orphan waste exchanges on an activity – waste flows the
dataset author left unmodelled (no in-database link to a treatment activity,
and no explicit cross-DB match either). These contribute 0 to LCIA scores;
surfacing them lets consumers see what is excluded rather than silently
undercounting. Waste exchanges already routed to another database via
'dbCrossDBLinks' are excluded – they do contribute to the score and are
therefore not cut-offs.
-}
buildCutoffWaste :: Database -> Activity -> [CutoffWasteFlow]
buildCutoffWaste db activity =
    let resolved = crossDBResolvedFlowIds db activity
     in [ CutoffWasteFlow
            { cwfFlowId = fid
            , cwfFlowName = maybe (unresolvedName fid) wfName mFlow
            , cwfAmount = amt
            , cwfUnit = maybe "" (lookupUnitName . wfUnitId) mFlow
            }
        | WasteExchange{waActivityLinkId = lid, waIsInput = False, waFlowId = fid, waAmount = amt} <- exchanges activity
        , isNothing lid
        , not (S.member fid resolved)
        , let mFlow = M.lookup fid (dbWasteFlows db)
        ]
  where
    unresolvedName fid = "<unresolved waste " <> UUID.toText fid <> ">"
    lookupUnitName uid = maybe "" unitName (M.lookup uid (dbUnits db))

{- | Extended metadata for an activity. Cross-DB-resolved waste exchanges count
as @pmWasteExchangesLinked@, not @pmWasteExchangesOrphan@, so the metric
matches what the score actually consumes.
-}
calculateActivityMetadata :: Database -> Activity -> ActivityMetadata
calculateActivityMetadata db activity =
    let allExchanges = exchanges activity
        uniqueFlows = S.size (S.fromList [exchangeFlowId ex | ex <- allExchanges])
        techInputs = length [ex | ex <- allExchanges, isTechnosphereExchange ex, exchangeIsInput ex, not (exchangeIsReference ex)]
        bioExchanges = length [ex | ex <- allExchanges, isBiosphereExchange ex]
        resolved = crossDBResolvedFlowIds db activity
        wasteExchanges = [(fid, linkId) | WasteExchange{waFlowId = fid, waActivityLinkId = linkId} <- allExchanges]
        wasteLinked = length [() | (fid, linkId) <- wasteExchanges, isJust linkId || S.member fid resolved]
        wasteOrphan = length wasteExchanges - wasteLinked
        refProduct = exchangeFlowId <$> L.find exchangeIsReference allExchanges
     in ActivityMetadata
            { pmTotalFlows = uniqueFlows
            , pmTechnosphereInputs = techInputs
            , pmBiosphereExchanges = bioExchanges
            , pmWasteExchangesLinked = wasteLinked
            , pmWasteExchangesOrphan = wasteOrphan
            , pmHasReferenceProduct = isJust refProduct
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
convertActivityForAPI :: Database -> ProcessId -> Activity -> ActivityForAPI
convertActivityForAPI db processId activity =
    let allProducts = case processIdToRef db processId of
            Just ref -> getAllProductsForActivity db (prActivity ref)
            Nothing -> []
        (refProdName, refProdAmount, refProdUnit) = getReferenceProductInfo (dbTechFlows db) (dbUnits db) activity
        linkMap = buildCrossDBLinkMap db processId
     in ActivityForAPI
            { pfaProcessId = processIdToText db processId
            , pfaActivityName = activityName activity
            , pfaDescription = activityDescription activity
            , pfaDocumentation = activityDocumentation activity
            , pfaSynonyms = activitySynonyms activity
            , pfaClassifications = activityClassification activity
            , pfaLocation = activityLocation activity
            , pfaUnit = activityUnit activity
            , pfaProductName = if T.null refProdName then Nothing else Just refProdName
            , pfaProductAmount = if T.null refProdName then Nothing else Just refProdAmount
            , pfaProductUnit = if T.null refProdName then Nothing else Just refProdUnit
            , pfaAllProducts = allProducts
            , pfaExchanges = map (toExchangeWithUnit db linkMap) (exchanges activity)
            , pfaNativeType = activityNativeType activity
            , pfaNativeId = activityNativeId activity
            }

{- | Resolved target activity for a technosphere or waste exchange. Either all
three fields are present (Just TargetRef) or none (Nothing) – the formerly
correlated triple of Maybes can no longer drift apart.
-}
data TargetRef = TargetRef
    { trName :: !Text
    , trLocation :: !Text
    , trProcessId :: !Text
    }

activityToTarget :: Database -> ProcessId -> Activity -> TargetRef
activityToTarget db pid act =
    TargetRef (activityName act) (activityLocation act) (processIdToText db pid)

crossDBLinkToTarget :: CrossDBLink -> TargetRef
crossDBLinkToTarget link =
    TargetRef
        (cdlFlowName link)
        (cdlLocation link)
        (supplierRefText link)

-- | The target one row names, when that row is in the database.
targetOf :: Database -> ProcessId -> Maybe TargetRef
targetOf db pid = activityToTarget db pid <$> getActivity db pid

{- | SimaPro path: resolve a target by product flow UUID. The index answers a
flow one row produces; a flow several rows produce names none of them, and the
caller says so rather than electing one. It used to elect the one whose unit
was dimensionally compatible with the input, a rule written when the unit was
part of a flow identifier and two spellings of one product were two flows.
-}
resolveByProductFlow :: Database -> UUID -> Maybe TargetRef
resolveByProductFlow db fId = findProcessIdByProductFlow db fId >>= targetOf db

-- | Cross-database link resolution (orphan waste outputs, missing tech links).
resolveByCrossDBLink :: M.Map UUID CrossDBLink -> UUID -> Maybe TargetRef
resolveByCrossDBLink links fId = crossDBLinkToTarget <$> M.lookup fId links

{- | The producer the matrix routes this exchange through: 'findProducer' is the
same function the triples are built with, so a target named here is the row the
score charged. The activity UUID alone would answer with an arbitrary product of
a multi-product activity, and would name a treatment for a pair the matrix never
routed.
-}
resolveByRoutedProducer :: Database -> Exchange -> Maybe TargetRef
resolveByRoutedProducer db ex = findProducer (dbProcessIdLookup db) ex >>= targetOf db

{- | The target an incoming link names: the routed row, else the row its
activity UUID alone names ('linkedProducer'). An input's link is a statement
about where the flow comes from, so answering it with a row of the right
activity but the wrong product is what this replaces.
-}
resolveByLinkedProducer :: Database -> Exchange -> Maybe TargetRef
resolveByLinkedProducer db ex = linkedProducer db ex >>= targetOf db

{- | Resolve the target activity (if any) for one exchange. Technosphere broken
links (linkId set but unresolvable) do NOT fall through to the product-flow
path – that matches the original behaviour. Use '<|>' to chain fallbacks only
where the original code did. Every linked arm resolves the way the matrix
routes it; a waste output is the one that stops there, with no fallback to the
activity UUID, because a link it cannot route is a treatment this database does
not hold and reporting a row for it would hide that.
-}
resolveTarget ::
    Database ->
    M.Map UUID CrossDBLink ->
    Exchange ->
    Maybe TargetRef
resolveTarget db links = \case
    ex@TechnosphereExchange{techRole = role, techActivityLinkId = lid, techFlowId = fid}
        | role /= Input && role /= ReferenceInput -> Nothing
        | isJust lid -> resolveByLinkedProducer db ex
        | otherwise -> resolveByProductFlow db fid <|> resolveByCrossDBLink links fid
    BiosphereExchange{} -> Nothing
    ex@WasteExchange{waIsInput = True, waActivityLinkId = lid}
        | isJust lid -> resolveByLinkedProducer db ex
        | otherwise -> Nothing
    -- An output's link names the activity that treats the waste, exactly as an
    -- input's names the one that supplies it. Reading it as no target at all
    -- makes a linked waste output indistinguishable from a final waste flow,
    -- which is what a consumer reports when nothing treats a waste. A named
    -- treatment this database does not hold falls through to the cross-DB link
    -- the loader built for it, for the same reason as everywhere here: the row
    -- named must be the row the score charged, and that link is charged.
    ex@WasteExchange{waIsInput = False, waActivityLinkId = lid, waFlowId = fid}
        | isJust lid -> resolveByRoutedProducer db ex <|> resolveByCrossDBLink links fid
        | otherwise -> resolveByCrossDBLink links fid

{- | What a waste line does, given the target 'resolveTarget' found for it.
'Nothing' on every other kind, which is what makes the field a statement about
waste rather than a target-shaped restatement of the three fields next to it.

The distinction the caller cannot make for itself is the last two: an output
naming no treatment describes an end-of-life flow completely, while an output
whose named treatment resolved to nothing is a gap in what was loaded. Both
arrive with no target.
-}
wasteRoleOf :: Maybe TargetRef -> Exchange -> Maybe WasteRole
wasteRoleOf target = \case
    TechnosphereExchange{} -> Nothing
    BiosphereExchange{} -> Nothing
    WasteExchange{waIsInput = True} -> Just TreatsWaste
    WasteExchange{waSupplierClaim = claim}
        | isJust target -> Just SentToTreatment
        | namesATreatment claim -> Just TreatmentNotLoaded
        | otherwise -> Just FinalWasteFlow
  where
    -- What the source said, not what linking made of it: a treatment named and
    -- since deleted still leaves a row that named one.
    namesATreatment :: SupplierClaim -> Bool
    namesATreatment = \case
        ClaimById _ -> True
        ClaimByName _ -> True
        ClaimByDatasetNumber _ -> True
        ClaimByProduct -> False

{- | Flow name + (biosphere-only) compartment. Each variant has exactly one
flow side by construction, so no Maybe-merge is needed downstream.
-}
resolveFlow :: Database -> Exchange -> Maybe (Text, Maybe Compartment)
resolveFlow db exchange = do
    fk <- lookupExchangeFlow db exchange
    pure (flowKindName fk, flowKindCompartment fk)

{- | Build the cross-DB link map for one activity, keyed by consumer flow UUID.
UUIDs are unique across flow kinds, so a tech and a waste link on the same
activity cannot collide here.
-}
buildCrossDBLinkMap :: Database -> ProcessId -> M.Map UUID CrossDBLink
buildCrossDBLinkMap db pid = case prActivity <$> processIdToRef db pid of
    Just actUUID ->
        M.fromList
            [ (cdlConsumerFlowId link, link)
            | link <- dbCrossDBLinks db
            , cdlConsumerActUUID link == actUUID
            , cdlConsumerFlowId link /= UUID.nil
            ]
    Nothing -> M.empty

toExchangeWithUnit ::
    Database ->
    M.Map UUID CrossDBLink ->
    Exchange ->
    ExchangeWithUnit
toExchangeWithUnit db links exchange =
    -- Surface the raw UUID when the flow does not resolve – a clear failure
    -- the consumer can debug, not a silent "unknown".
    let unresolvedName = unresolvedFlowName (exchangeFlowId exchange)
        (flowName, compartment) = fromMaybe (unresolvedName, Nothing) (resolveFlow db exchange)
        target = resolveTarget db links exchange
     in ExchangeWithUnit
            { ewuExchange = exchange
            , ewuUnitName = getUnitNameForExchange (dbUnits db) exchange
            , ewuFlowName = flowName
            , ewuCompartment = compartment
            , ewuTargetActivityName = trName <$> target
            , ewuTargetLocation = trLocation <$> target
            , ewuTargetProcessId = trProcessId <$> target
            , ewuWasteRole = wasteRoleOf target exchange
            , ewuExComment = exchangeComment exchange
            , ewuPedigree = exchangePedigree exchange
            }

{- | Get reference product name from activity exchanges. Reference products
are always technosphere.
-}
getReferenceProductName :: TechFlowDB -> Activity -> Maybe Text
getReferenceProductName flows activity = do
    ex <- L.find exchangeIsReference (exchanges activity)
    tfName <$> M.lookup (exchangeFlowId ex) flows

-- | Get reference product info (name, amount, unit) from activity exchanges
getReferenceProductInfo :: TechFlowDB -> UnitDB -> Activity -> (Text, Double, Text)
getReferenceProductInfo flows units activity =
    maybe ("", 1.0, "") describe (L.find exchangeIsReference (exchanges activity))
  where
    describe :: Exchange -> (Text, Double, Text)
    describe ex =
        ( maybe "" tfName (M.lookup (exchangeFlowId ex) flows)
        , exchangeAmount ex
        , getUnitNameForExchange units ex
        )

{- | The functional unit a score is reported against: one unit of the
activity's reference product, named in the unit its matrix column was
normalised to.

Never the amount the source declared. "Database.MatrixBuild" divides every
coefficient of a column by that amount, so a coproduct a source states at
0.0686462 kg is still scored per kilogram, and a reference product ingested
as 3.6 mj is still scored per megajoule. Reporting the declared amount here
described the block, not the number beside it.
-}
functionalUnitOf :: TechFlowDB -> UnitDB -> Activity -> Text
functionalUnitOf flows units activity = "1.00 " <> prodUnit <> " of " <> prodName
  where
    prodName, prodUnit :: Text
    (prodName, _, prodUnit) = getReferenceProductInfo flows units activity

{- | Build an 'ActivitySummary' from a (ProcessId, Activity) pair. Encapsulates
the reference-product + allocation + native-type projection shared by
search results, supply-chain entries, inventory metadata, and exchange-target
navigation. Uses @dbUnits db@ for the unit DB – callers needing a merged
cross-DB unit DB build the record by hand.
-}
mkActivitySummary :: Database -> ProcessId -> Activity -> ActivitySummary
mkActivitySummary db processId activity =
    let (prodName, prodAmount, prodUnit) = getReferenceProductInfo (dbTechFlows db) (dbUnits db) activity
        block = sourceBlockOf db processId
     in ActivitySummary
            { prsProcessId = processIdToText db processId
            , prsActivityName = activityName activity
            , prsLocation = activityLocation activity
            , prsProductName = prodName
            , prsProductAmount = prodAmount
            , prsProductUnit = prodUnit
            , prsAllocationPercent = dsPercent <$> activityReferenceShare activity
            , prsAllocationFormula = dsFormula =<< activityReferenceShare activity
            , prsMassAllocationPercent = Nothing
            , prsNativeType = activityNativeType activity
            , prsBlock = sbName block
            , prsBlockProducts = sbProducts block
            }

{- | Placeholder summary surfaced when the products index points at a
ProcessId that no longer resolves to an Activity. Carries the raw pid so the
consumer can debug, rather than silently dropping the entry.
-}
unknownActivitySummary :: Database -> ProcessId -> ActivitySummary
unknownActivitySummary db pid =
    ActivitySummary
        { prsProcessId = processIdToText db pid
        , prsActivityName = "Unknown"
        , prsLocation = ""
        , prsProductName = "Unknown"
        , prsProductAmount = 1.0
        , prsProductUnit = ""
        , prsAllocationPercent = Nothing
        , prsAllocationFormula = Nothing
        , prsMassAllocationPercent = Nothing
        , prsNativeType = Nothing
        , prsBlock = processIdToText db pid
        , prsBlockProducts = 1
        }

{- | The coproducts of one source dataset block, as 'ActivitySummary'. Keyed on
the activity, which is what a block is: its rows differ by their product.
-}
getAllProductsForActivity :: Database -> UUID -> [ActivitySummary]
getAllProductsForActivity db actUUID =
    case M.lookup actUUID (dbActivityUUIDIndex db) of
        Nothing -> []
        Just processIds ->
            withMassAllocationPercent (biUnitConfig (dbBuiltWith db)) (dbUnits db) (map described (NE.toList processIds))
  where
    {- Each product with the row it was split from, because what a mass key
    would give this block is read from that row: the property it states if it
    states one, its amount if the amount is already a mass. Reading the amount
    alone would answer a different question from the one an @allocation@ key
    answers, and the two numbers sit next to each other. -}
    described :: ProcessId -> (ActivitySummary, Maybe Exchange)
    described pid = case findActivityByProcessId db pid of
        Nothing -> (unknownActivitySummary db pid, Nothing)
        Just act -> (mkActivitySummary db pid act, L.find exchangeIsReference (exchanges act))

{- | Fill in what each product of one block would carry under a mass key, to be
read beside the share its source declared.

Only a block of several products, each carrying a share its source stated,
gets one. That condition is what says these amounts are the joint outputs of
one run: a database whose datasets arrive already allocated declares no share
and normalises each product to one of its own unit, so summing those amounts
would compare quantities that never occurred together. And a lone product has
nothing to be compared against.

Left as it is again when the mass cannot serve as a key. The comparison is one
extra column, so a block whose products are not all stated in a mass has none.
-}
withMassAllocationPercent :: UnitConfig -> UnitDB -> [(ActivitySummary, Maybe Exchange)] -> [ActivitySummary]
withMassAllocationPercent unitCfg unitDB entries
    | length entries < 2 = summaries
    | not (all (isJust . prsAllocationPercent) summaries) = summaries
    | otherwise = fromMaybe summaries attached
  where
    summaries :: [ActivitySummary]
    summaries = map fst entries

    attached :: Maybe [ActivitySummary]
    attached = do
        block <- NE.nonEmpty entries
        rows <- traverse snd block
        shares <- either (const Nothing) Just (propertyShares WetMass unitDB unitCfg rows)
        pure (NE.toList (NE.zipWith attach (NE.map fst block) shares))

    attach :: ActivitySummary -> Double -> ActivitySummary
    attach s percent = s{prsMassAllocationPercent = Just percent}

-- | Get target activity for technosphere navigation.
getTargetActivity :: Database -> Exchange -> Maybe ActivitySummary
getTargetActivity db exchange = do
    pid <- linkedProducer db exchange
    act <- getActivity db pid
    pure (mkActivitySummary db pid act)

{- | Get reference product as FlowDetail (if exists). Reference products are
technosphere by definition.
-}
getActivityReferenceProductDetail :: Database -> Activity -> Maybe FlowDetail
getActivityReferenceProductDetail db activity = do
    refExchange <- L.find exchangeIsReference (exchanges activity)
    flow <- M.lookup (exchangeFlowId refExchange) (dbTechFlows db)
    let usageCount = getFlowUsageCount db (tfId flow)
    let uName = getUnitNameForTechFlow (dbUnits db) flow
    return $ FlowDetail (ApiTechFlow flow) uName usageCount

{- | The activities on one side of a flow: those that make it, those that use
it, or both.

'EitherSide' is what this answered before the side could be asked, so the
route's default is unchanged. It is wider than the two sides put together: an
avoided product is an exchange on the flow that neither makes it for sale nor
consumes it, so it appears under 'EitherSide' alone. Adding the two sides is
therefore not the same as asking for both.
-}
getActivitiesUsingFlow :: Database -> ProducerFilter -> UUID -> [ActivitySummary]
getActivitiesUsingFlow db side flowUUID = case side of
    ProducersOnly -> [mkActivitySummary db pid act | (pid, act) <- producingRows db flowUUID]
    ConsumersOnly -> touching (any consumes . exchanges)
    EitherSide -> touching (const True)
  where
    touching :: (Activity -> Bool) -> [ActivitySummary]
    touching keep = [mkActivitySummary db pid act | (pid, act) <- activitiesTouching db flowUUID, keep act]

    consumes :: Exchange -> Bool
    consumes ex = exchangeFlowId ex == flowUUID && exchangeIsInput ex

{- | The rows that make a flow, as the matrix sees them.

Read from the product index rather than judged exchange by exchange, because
that index /is/ the answer: it holds one entry per process row keyed by the
flow that row produces, built from 'exchangeIsReference'. That matters beyond
saving a scan. A waste treatment activity's reference is an /input/, so
'exchangeIsProductOutput' says no to it and would hide every treatment
activity from "what makes this flow" - the same mistake 'Database.hs' records
having already made once with product filters.

Each coproduct of an allocated block is its own row with its own reference
product, so they are in here too. A block the allocation gate refused has no
row and no honest column either, so it is absent from both.
-}
producingRows :: Database -> UUID -> [(ProcessId, Activity)]
producingRows db flowUUID =
    [ (pid, act)
    | pid <- maybe [] NE.toList (M.lookup flowUUID (piByUUID (dbProductIndex db)))
    , Just act <- [getActivity db pid]
    ]

{- | How many activities make this flow.

'Nothing' where the question does not apply, never 0 as a stand-in: a zero
here is the true statement that no row produces the flow, and 'Nothing' the
different one that this kind of flow has no producing side at all.
-}
producerCount :: Database -> FlowKind -> Maybe Int
producerCount db flow = case flow of
    TechKind tf -> Just (maybe 0 NE.length (M.lookup (tfId tf) (piByUUID (dbProductIndex db))))
    BioKind _ -> Nothing
    WasteKind _ -> Nothing

-- | The activities the flow index names for one flow, each with its id, once.
activitiesTouching :: Database -> UUID -> [(ProcessId, Activity)]
activitiesTouching db flowUUID =
    [ (pid, act)
    | pid <- maybe [] (S.toList . S.fromList) (M.lookup flowUUID (idxByFlow (dbIndexes db)))
    , Just act <- [getActivity db pid]
    ]

{- | Sentinel returned only when an exchange's unit UUID failed to resolve.
The exchange unit-name field already surfaces the same gap via
'getUnitNameForExchange', so consumers see the missing unit in both the
structured 'Unit' and the unit-name string.
-}
unresolvedUnit :: Unit
unresolvedUnit = Unit{unitId = UUID.nil, unitName = "<unresolved unit>", unitSymbol = "", unitComment = ""}

{- | 'ActivitySummary' form of a cross-DB link target. Mirrors
'crossDBLinkToTarget' but produces the richer wire shape consumed by the
exchange-details endpoint.
-}
crossDBLinkToSummary :: CrossDBLink -> ActivitySummary
crossDBLinkToSummary link =
    ActivitySummary
        { prsProcessId = supplierRefText link
        , prsActivityName = cdlFlowName link
        , prsLocation = cdlLocation link
        , prsProductName = cdlFlowName link
        , prsProductAmount = 1.0
        , prsProductUnit = cdlExchangeUnit link
        , prsAllocationPercent = Nothing
        , prsAllocationFormula = Nothing
        , prsMassAllocationPercent = Nothing
        , prsNativeType = Nothing
        , prsBlock = supplierRefText link
        , prsBlockProducts = 1
        }

{- | Resolve an exchange's target as 'ActivitySummary', falling back to the
cross-DB link map for unresolved technosphere/waste links. Biosphere flows
have no target by definition.
-}
resolveTargetSummary ::
    Database ->
    M.Map UUID CrossDBLink ->
    Exchange ->
    Maybe ActivitySummary
resolveTargetSummary db links exchange = case exchange of
    BiosphereExchange{} -> Nothing
    TechnosphereExchange{} -> resolved
    WasteExchange{} -> resolved
  where
    resolved =
        getTargetActivity db exchange
            <|> (crossDBLinkToSummary <$> M.lookup (exchangeFlowId exchange) links)

{- | Detailed exchanges with filtering. Resolves cross-DB technosphere inputs
(SimaPro pattern: no @activityLinkId@, the supplier lives in a dep DB
via 'dbCrossDBLinks') by synthesizing an 'ActivitySummary' with a qualified
pid @"dbName::actUUID_prodUUID"@ – same convention the @/activity/{pid}@
endpoint uses.

A missing flow row used to drop the exchange entirely. We now surface an
unresolved-flow entry instead, so the returned list always has one element
per matching exchange and the gap is reportable.
-}
getActivityExchangeDetails :: Database -> Activity -> (Exchange -> Bool) -> [ExchangeDetail]
getActivityExchangeDetails db activity filterFn =
    let linkMap = case findProcessIdForActivity db activity of
            Just pid -> buildCrossDBLinkMap db pid
            Nothing -> M.empty
     in map (toExchangeDetail db linkMap) (filter filterFn (exchanges activity))

toExchangeDetail :: Database -> M.Map UUID CrossDBLink -> Exchange -> ExchangeDetail
toExchangeDetail db links exchange =
    let unitForExchange = M.findWithDefault unresolvedUnit (exchangeUnitId exchange) (dbUnits db)
        exUnitName = getUnitNameForExchange (dbUnits db) exchange
        target = resolveTargetSummary db links exchange
     in case lookupExchangeFlow db exchange of
            Just fk ->
                ExchangeDetail exchange (apiFlowOfKind fk) (flowKindUnitName (dbUnits db) fk) unitForExchange exUnitName target
            Nothing ->
                ExchangeDetail exchange (ApiUnresolvedFlow (exchangeFlowId exchange)) "" unitForExchange exUnitName Nothing

-- | Get detailed input exchanges
getActivityInputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityInputDetails db activity = getActivityExchangeDetails db activity exchangeIsInput

-- | Get detailed output exchanges
getActivityOutputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityOutputDetails db activity = getActivityExchangeDetails db activity (not . exchangeIsInput)

-- | Get flow info as JSON (for CLI). Resolves against either flow side.
getFlowInfo :: Database -> Text -> Either ServiceError Value
getFlowInfo db flowIdText = do
    case UUID.fromText flowIdText of
        Nothing -> Left $ InvalidUUID $ "Invalid flow UUID: " <> flowIdText
        Just fId ->
            let usageCount = getFlowUsageCount db fId
             in case M.lookup fId (dbTechFlows db) of
                    Just flow ->
                        Right $ toJSON $ FlowDetail (ApiTechFlow flow) (getUnitNameForTechFlow (dbUnits db) flow) usageCount
                    Nothing -> case M.lookup fId (dbBioFlows db) of
                        Just flow ->
                            Right $ toJSON $ FlowDetail (ApiBioFlow flow) (getUnitNameForBioFlow (dbUnits db) flow) usageCount
                        Nothing -> Left $ FlowNotFound flowIdText

{- | Get activities that use a specific flow as JSON (for CLI). Resolves
against either flow side so biosphere flow IDs work too.
-}
getFlowActivities :: Database -> ProducerFilter -> Text -> Either ServiceError Value
getFlowActivities db side flowIdText = do
    case UUID.fromText flowIdText of
        Nothing -> Left $ InvalidUUID $ "Invalid flow UUID: " <> flowIdText
        Just fId
            | M.member fId (dbTechFlows db) || M.member fId (dbBioFlows db) ->
                Right $ toJSON (getActivitiesUsingFlow db side fId)
            | otherwise -> Left $ FlowNotFound flowIdText

{- | Compute the supply chain for an activity using the scaling vector.
Returns all upstream activities with their scaling factors and subgraph edges.
-}
getSupplyChain ::
    UnitConfig ->
    Geographies ->
    SharedSolver.DepSolverLookup ->
    Database ->
    Text ->
    SharedSolver ->
    Text ->
    SupplyChainFilter ->
    IO (Either ServiceError SupplyChainResponse)
getSupplyChain unitCfg geographies depLookup db dbName sharedSolver processIdText af =
    case resolveScorable db processIdText of
        Left err -> return $ Left err
        Right (processId, _rootActivity) ->
            -- Building the demand vector is the matrix-index check: an id with
            -- no column is refused here, by name.
            case demandFor db processId of
                Left err -> return $ Left err
                Right demandVec -> do
                    supplyVec <- solveWithSharedSolver sharedSolver demandVec
                    buildSupplyChainFromScalingVectorCrossDB
                        unitCfg
                        geographies
                        depLookup
                        db
                        dbName
                        processId
                        supplyVec
                        []
                        af

{- | Find the shortest supply chain path from a root process to the first upstream activity
whose name contains the given substring (case-insensitive).
Returns path steps ordered root → target, each with cumulative quantity, scaling factor,
and local_step_ratio (upstream ÷ downstream scaling factors).
-}
getPathTo :: Database -> SharedSolver -> Text -> NamePattern -> IO (Either ServiceError Value)
getPathTo db solver pidText target = do
    case resolveScorable db pidText of
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
                                                (unNamePattern target)
                                                (activityName (dbActivities db V.! i))
                                        )
                                        adj
                             in return $ case mPath of
                                    Nothing ->
                                        Left $
                                            ActivityNotFound $
                                                "No upstream node matching '" <> unNamePattern target <> "' reachable from " <> pidText
                                    Just [] ->
                                        Left $
                                            ActivityNotFound $
                                                "BFS returned empty path from " <> pidText
                                    Just pids@(firstPid : restPids) ->
                                        let scalingOf i = supplyVec U.! i
                                            mkStep i mRatio =
                                                let act = dbActivities db V.! i
                                                    sf = scalingOf i
                                                    base =
                                                        [ "processId" .= processIdToText db (fromIntegral i)
                                                        , "activityName" .= activityName act
                                                        , "location" .= activityLocation act
                                                        , "unit" .= activityUnit act
                                                        , "cumulativeQuantity" .= (sf * rootRefAmount)
                                                        , "scalingFactor" .= sf
                                                        ]
                                                 in object $ case mRatio of
                                                        Nothing -> base
                                                        Just r -> base ++ ["localStepRatio" .= r]
                                            steps =
                                                mkStep firstPid (Nothing :: Maybe Double)
                                                    : [ mkStep c (Just ratio)
                                                      | (p, c) <- zip pids restPids
                                                      , let ratio =
                                                                if scalingOf p == 0
                                                                    then 0
                                                                    else scalingOf c / scalingOf p
                                                      ]
                                            totalRatio =
                                                product
                                                    [ scalingOf c / scalingOf p
                                                    | (p, c) <- zip pids restPids
                                                    , scalingOf p /= 0
                                                    ]
                                         in Right $
                                                object
                                                    [ "path" .= steps
                                                    , "path_length" .= length pids
                                                    , "total_ratio" .= totalRatio
                                                    ]

{- | Where a supply-chain collection stands.

At the root the root row is left out of its own chain, quantities are scaled by
the root's reference amount, process ids stay bare, and depth counts from zero.
In a dependency database nothing is excluded, the scaling that arrives is
already physical, ids are qualified with @db::@, and depth continues from the
level that reached it. The two are one fact, so they travel as one value: as
four separate parameters they admitted sixteen combinations, of which the code
only ever built these two.
-}
data WalkLevel
    = RootLevel {rlRoot :: !ProcessId}
    | DepLevel {dlDepthOffset :: !Int}

{- | What one database contributes to a supply chain: how many non-zero rows it
had before filtering, the entries that passed, and the edges. Summed across
levels, pointwise, nearer level first.
-}
data Collected = Collected
    { cUnfilteredCount :: !Int
    , cEntries :: ![SupplyChainEntry]
    , cEdges :: ![SupplyChainEdge]
    }

instance Semigroup Collected where
    Collected t1 es1 ed1 <> Collected t2 es2 ed2 =
        Collected (t1 + t2) (es1 ++ es2) (ed1 ++ ed2)

instance Monoid Collected where
    mempty = Collected 0 [] []

{- | Collect filtered supply-chain entries + edges from a single DB's scaling
vector. Applies @minQuantity@, name/location/product/class/maxDepth filters,
BFS depth assignment, and upstream-count accumulation – but deliberately
does NOT sort, limit, or offset. Callers merge collections from multiple
databases and then apply sorting/pagination once on the combined list.

Entry @sceProcessId@ is qualified with @dbName::@ at a dep level only; root
entries stay bare for callers that navigate on bare root PIDs.
-}
collectSupplyChainEntries ::
    -- | the declared location table, which is what a geography filter reads
    Geographies ->
    Database ->
    -- | DB name
    Text ->
    WalkLevel ->
    -- | scaling vector
    U.Vector Double ->
    SupplyChainFilter ->
    Collected
collectSupplyChainEntries geographies db dbName level supplyVec scf =
    let core = scfCore scf
        minQ = fromMaybe 0 (scfMinQuantity scf)

        mRootPid = case level of
            RootLevel{rlRoot = r} -> Just r
            DepLevel{} -> Nothing
        -- A root chain is stated per the root's reference product; a dep level
        -- receives a scaling that is already physical.
        quantityMult = case level of
            RootLevel{rlRoot = r} -> getReferenceProductAmount (dbActivities db V.! fromIntegral r)
            DepLevel{} -> 1.0
        depthOffset = case level of
            RootLevel{} -> 0
            DepLevel{dlDepthOffset = d} -> d
        qualifyPids = case level of
            RootLevel{} -> False
            DepLevel{} -> True
        n = U.length supplyVec

        allEntries =
            [ (fromIntegral i :: ProcessId, supplyVec U.! i)
            | i <- [0 .. n - 1]
            , let v = supplyVec U.! i
            , abs v > minQ
            , Just (fromIntegral i) /= mRootPid
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
        -- without a BM25 index – only bare test fixtures hit that last case).
        mNameSet = nameFilterSet db (afcName core)
        nameMatchesPid pid = maybe True (IS.member (fromIntegral pid)) mNameSet

        getProductNames activity =
            [ tfName flow
            | ex <- exchanges activity
            , exchangeIsReference ex
            , not (exchangeIsInput ex)
            , Just flow <- [M.lookup (exchangeFlowId ex) (dbTechFlows db)]
            ]

        matchesFilters activity pid =
            let nameOk = nameMatchesPid pid
                locOk = maybe True (\pat -> locationAnswers geographies pat (activityLocation activity)) (afcLocation core)
                productOk = maybe True (\pat -> any (textMatches pat) (getProductNames activity)) (afcProduct core)
                classOk = matchClassifications activity (afcClassifications core)
                localDepth = IM.findWithDefault maxBound (fromIntegral pid) depthMap
                depthOk = maybe True (localDepth <=) (scfMaxDepth scf)
             in nameOk && locOk && productOk && classOk && depthOk

        qualify pid
            | qualifyPids = qualifyRef dbName (processIdToText db pid)
            | otherwise = processIdToText db pid

        mkEntry (pid, scalingFactor) =
            let activity = dbActivities db V.! fromIntegral pid
             in SupplyChainEntry
                    { sceProcessId = qualify pid
                    , sceDatabaseName = dbName
                    , sceActivityName = activityName activity
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
        edges = case scfEdges scf of
            EntriesOnly -> []
            WithEdges ->
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
     in Collected (length allEntries) filteredEntries edges

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
            Just "name" -> \a b -> compare (sceActivityName a) (sceActivityName b)
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
    Geographies ->
    Database ->
    Text ->
    ProcessId ->
    U.Vector Double ->
    SupplyChainFilter ->
    SupplyChainResponse
buildSupplyChainFromScalingVector geographies db dbName processId supplyVec scf =
    let rootActivity = dbActivities db V.! fromIntegral processId
        rootRefAmount = getReferenceProductAmount rootActivity
        Collected totalActs entries edges =
            collectSupplyChainEntries
                geographies
                db
                dbName
                (RootLevel processId)
                supplyVec
                scf
        rootBlock = sourceBlockOf db processId
        rootSummary =
            ActivitySummary
                { prsProcessId = processIdToText db processId
                , prsActivityName = activityName rootActivity
                , prsLocation = activityLocation rootActivity
                , prsProductName =
                    fromMaybe
                        (activityName rootActivity)
                        (getReferenceProductName (dbTechFlows db) rootActivity)
                , prsProductAmount = rootRefAmount
                , prsProductUnit = activityUnit rootActivity
                , prsAllocationPercent = dsPercent <$> activityReferenceShare rootActivity
                , prsAllocationFormula = dsFormula =<< activityReferenceShare rootActivity
                , prsMassAllocationPercent = Nothing
                , prsNativeType = activityNativeType rootActivity
                , prsBlock = sbName rootBlock
                , prsBlockProducts = sbProducts rootBlock
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
    Geographies ->
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
    IO (Either ServiceError SupplyChainResponse)
buildSupplyChainFromScalingVectorCrossDB unitCfg geographies depLookup rootDb rootDbName rootPid rootScaling extraLinks scf = do
    let rootActivity = dbActivities rootDb V.! fromIntegral rootPid
        rootRefAmount = getReferenceProductAmount rootActivity
        rootBlock = sourceBlockOf rootDb rootPid
        rootCollected =
            collectSupplyChainEntries
                geographies
                rootDb
                rootDbName
                (RootLevel rootPid)
                rootScaling
                scf
        rootSummary =
            ActivitySummary
                { prsProcessId = processIdToText rootDb rootPid
                , prsActivityName = activityName rootActivity
                , prsLocation = activityLocation rootActivity
                , prsProductName =
                    fromMaybe
                        (activityName rootActivity)
                        (getReferenceProductName (dbTechFlows rootDb) rootActivity)
                , prsProductAmount = rootRefAmount
                , prsProductUnit = activityUnit rootActivity
                , prsAllocationPercent = dsPercent <$> activityReferenceShare rootActivity
                , prsAllocationFormula = dsFormula =<< activityReferenceShare rootActivity
                , prsMassAllocationPercent = Nothing
                , prsNativeType = activityNativeType rootActivity
                , prsBlock = sbName rootBlock
                , prsBlockProducts = sbProducts rootBlock
                }
    eDep <- walkDepLevels unitCfg geographies depLookup rootDb rootScaling extraLinks scf 1 S.empty
    pure $ case eDep of
        Left err -> Left err
        Right depCollected ->
            let Collected total entries edges = rootCollected <> depCollected
             in Right
                    SupplyChainResponse
                        { scrRoot = rootSummary
                        , scrTotalActivities = total
                        , scrFilteredActivities = length entries
                        , scrSupplyChain = sortAndPaginate (scfCore scf) entries
                        , scrEdges = edges
                        }

{- | Recursive helper: for every cross-DB link emerging from @consumerScaling@,
solve the induced dep demand and collect entries\/edges from the dep DB
(filtered by the same 'SupplyChainFilter' as the root). Returns
@(total_active_count, filtered_entries, edges)@ summed across all reached
dep DBs at this depth and deeper. No pagination here – that's applied once
at the top level on the merged list.
-}
walkDepLevels ::
    UnitConfig ->
    Geographies ->
    SharedSolver.DepSolverLookup ->
    -- | current consumer DB
    Database ->
    -- | current level's scaling
    U.Vector Double ->
    -- | extra virtual links visible at this level
    [CrossDBLink] ->
    SupplyChainFilter ->
    -- | current depth
    Int ->
    -- | visited DB names (cycle guard)
    S.Set Text ->
    IO (Either ServiceError Collected)
walkDepLevels unitCfg geographies depLookup consumerDb consumerScaling extras scf depth visited
    | depth >= SharedSolver.maxDepsDepth = pure (Right mempty)
    | otherwise = do
        let demandsMap = accumulateDepDemandsWith consumerDb extras consumerScaling
        results <-
            mapM
                (resolveOneDep unitCfg geographies depLookup scf depth visited)
                (M.toList demandsMap)
        pure $ case lefts results of
            (err : _) -> Left err
            [] -> Right (mconcat (rights results))

{- | For a single dep DB: solve its induced demand, collect filtered entries
using the shared 'collectSupplyChainEntries' helper (so it gets the same
minQuantity/maxDepth pruning as the root walk), then recurse.
-}
resolveOneDep ::
    UnitConfig ->
    Geographies ->
    SharedSolver.DepSolverLookup ->
    SupplyChainFilter ->
    -- | current depth (the one we're entering)
    Int ->
    -- | visited
    S.Set Text ->
    (Text, SupplierDemands) ->
    IO (Either ServiceError Collected)
resolveOneDep unitCfg geographies depLookup scf depth visited (depDbName, demands)
    | depDbName `S.member` visited = pure (Right mempty)
    | otherwise = do
        mDep <- depLookup depDbName
        case mDep of
            Nothing -> pure (Right mempty) -- unloaded dep DB: silent skip (matches LCIA path)
            Just (depDb, depSolver) -> case depDemandsToVector unitCfg depDbName depDb demands of
                Left err -> pure (Left (MatrixError err))
                Right demandVec -> do
                    depScaling <- solveWithSharedSolver depSolver demandVec
                    let local =
                            collectSupplyChainEntries
                                geographies
                                depDb
                                depDbName
                                (DepLevel depth)
                                depScaling
                                scf
                    eDeeper <-
                        walkDepLevels
                            unitCfg
                            geographies
                            depLookup
                            depDb
                            depScaling
                            []
                            scf
                            (depth + 1)
                            (S.insert depDbName visited)
                    pure $ (local <>) <$> eDeeper

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
    maybe 1.0 exchangeAmount (L.find exchangeIsReference (exchanges activity))

{- | Root-only scaling vector: solve @(I-A)x = d@. Substitutions are applied by
the cross-DB applicator ('applySubstitutionsAt', via
'computeScalingVectorWithSubstitutionsCrossDB'), never here – this path is only
ever called with an empty list. A non-empty list is a programmer error, surfaced
loudly rather than silently mishandled (the previous in-place applicator used a
'defaultUnitConfig' and could never see the merged unit table).
-}
computeScalingVectorWithSubstitutions ::
    Database ->
    SharedSolver ->
    ProcessId ->
    [Substitution] ->
    IO (Either ServiceError (U.Vector Double))
computeScalingVectorWithSubstitutions db sharedSolver processId subs =
    case subs of
        [] -> either (pure . Left) (fmap Right . solveWithSharedSolver sharedSolver) (demandFor db processId)
        _ ->
            pure . Left . MatrixError $
                "computeScalingVectorWithSubstitutions: substitutions must be applied via the cross-DB path"

{- | Run sensitivity analysis on a process: compute the baseline scaling
vector @x₀@ once, then resolve every 'Perturbation' to a (consumer column,
sparse perturbation) spec and dispatch all valid specs to 'perturbABatch'
in a __single__ MUMPS multi-RHS call.

The 'perDelta' is __relative__: the resolved coefficient @a@ is multiplied
by @(1 + delta)@, so the absolute Δ passed to the kernel is @a * delta@.
A positive entry in @perturb@ adds @u·e_col^T@ to @(I-A)@, which decreases
@A_ij@; we negate to flip the convention so @delta=+0.05@ means \"+5%\".

Per-perturbation errors (missing technosphere link, singular update,
cross-DB qualified id in V1) are returned alongside the perturbation –
they do not abort the sweep. Only the baseline solve and the global
process-id resolution can fail at the outer 'ServiceError' level.

V1: @perConsumer@ and @perSupplier@ must live in the root DB (no
@\"dbName::pid\"@ form). Cross-DB perturbations require generalizing
'applySubstitutionsAt' and are out of scope here.
-}
computeSensitivities ::
    Database ->
    SharedSolver ->
    ProcessId ->
    [Perturbation] ->
    IO (Either ServiceError (U.Vector Double, [(Perturbation, Either Text (U.Vector Double))]))
computeSensitivities db sharedSolver processId perts = case demandFor db processId of
    Left err -> pure (Left err)
    Right demandVec -> do
        -- The baseline solve and factorization retrieval hit MUMPS through the FFI
        -- and can throw via exceptions (singular A, allocation failure, …). Wrap
        -- them so the documented 'Left' branch of the signature is reachable,
        -- instead of letting raw exceptions escape to the caller.
        eBaseline <- try @SomeException $ do
            baselineX <- solveWithSharedSolver sharedSolver demandVec
            mFact <- getFactorization sharedSolver
            pure (baselineX, mFact)
        case eBaseline of
            Left ex ->
                pure $
                    Left $
                        MatrixError $
                            "baseline solve failed: " <> T.pack (show ex)
            Right (baselineX, mFact) -> do
                -- Resolve each perturbation up-front. Resolution errors graft onto
                -- the final result; resolved specs go to the batch (a Left becomes
                -- a no-op empty spec so the batch preserves indexing).
                let resolved = map (resolveSpec db) perts
                smResults <-
                    perturbABatch db mFact baselineX (map (fromRight (0, [])) resolved)
                let combined = zipWith3 step perts resolved smResults
                    step p (Left e) _ = (p, Left e)
                    step p (Right _) sm = (p, sm)
                pure $ Right (baselineX, combined)

resolveSpec :: Database -> Perturbation -> Either Text (Int, [(Int, Double)])
resolveSpec db p = do
    consumerPid <- resolveRootOnly db (perConsumer p)
    supplierPid <- resolveRootOnly db (perSupplier p)
    case findTechCoefficient db (TechLink consumerPid supplierPid) of
        Nothing ->
            Left $
                "no technosphere link from consumer "
                    <> perConsumer p
                    <> " to supplier "
                    <> perSupplier p
        Just a ->
            let deltaAbs = -(a * perDelta p)
             in Right (fromIntegral consumerPid, [(fromIntegral supplierPid, deltaAbs)])

-- V1: root-DB only – qualified "db::pid" is rejected per perturbation
resolveRootOnly :: Database -> Text -> Either Text ProcessId
resolveRootOnly db t
    | "::" `T.isInfixOf` t =
        Left ("cross-DB perturbation not supported in V1: " <> t)
    | otherwise =
        case resolveScorable db t of
            Right (pid, _) -> Right pid
            Left (InvalidProcessId msg) -> Left msg
            Left (AmbiguousActivity msg) -> Left msg
            Left (ActivityNotFound msg) -> Left ("activity not found: " <> msg)
            Left (NotScorable msg) -> Left msg
            Left e -> Left (T.pack (show e))

{- | A global ('AllConsumers') substitution enumerates the replaced supplier's
consumers from the __root__ technosphere row, so @from@ must live in the
root DB. Reject a dep-qualified @from@ up front rather than letting the
per-level filter route it to a dep level (or silently drop it when that dep
is never visited). Per-edge ('OneEdge') subs are unaffected – they keep
their existing cross-DB freedom.
-}
globalFromMustLiveInRoot :: RootDb -> [Substitution] -> Either ServiceError ()
globalFromMustLiveInRoot rootDb subs =
    case [ fromDb
         | sub <- subs
         , let (fromDb, _) = parseSubRef rootDb (subFrom sub)
         , fromDb /= unRootDb rootDb
         , AllConsumers <- [subScope sub]
         ] of
        [] -> Right ()
        (d : _) ->
            Left $
                MatrixError $
                    "global substitution requires the replaced activity (from) to live in the root database (got: " <> d <> ")"

{- | What-if inventory with substitutions applied at every DB level of the
dep graph. Substitutions are filtered at each level by
'applySubstitutionsAt' – a sub whose consumer lives in a dep DB is
applied when the recursion reaches that dep DB's solver, not at root.

This generalizes the root-only path: substitutions in @subFrom@/@subTo@
may live in any loaded database (qualified as @"dbName::pid"@), and a
'OneEdge' consumer may also be qualified – the filter finds the right
level. Global ('AllConsumers') subs are anchored at their root @from@.
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
    IO (Either ServiceError SharedSolver.CrossDBSolution)
inventoryWithSubsAndDeps unitCfg depLookup db rootDbName solver pid subs =
    case globalFromMustLiveInRoot rootDb subs of
        Left e -> pure (Left e)
        Right () -> do
            eValid <- validateAnchorDbs depLookup db rootDb subs
            case eValid of
                Left e -> pure (Left e)
                Right () -> case demandFor db pid of
                    Left e -> pure (Left e)
                    Right demand -> do
                        res <- goWithSubsAndDeps unitCfg depLookup db (ThisDb rootDbName) rootDb solver [demand] subs 0
                        pure $ case res of
                            Left err -> Left err
                            Right (sol : _) -> Right sol
                            Right [] ->
                                -- unreachable: K=1 single-demand always yields one solution.
                                -- Surface as Left rather than fabricate an empty solution
                                -- with no 'csScalings' (NonEmpty forbids it).
                                Left $ MatrixError "inventoryWithSubsAndDeps: empty result for single demand"
  where
    rootDb = RootDb rootDbName

{- | Reject substitutions whose anchor DB (the 'OneEdge' consumer, or the
'AllConsumers' replaced supplier) is qualified to a DB that is either
unloaded or not reachable from @rootDbName@ via 'dbCrossDBLinks'. Such
subs would otherwise be silently filtered at every level of the
recursion (because the anchor DB never appears as @thisDbName@),
which violates the no-silent-errors invariant.
-}
validateAnchorDbs ::
    SharedSolver.DepSolverLookup ->
    Database ->
    RootDb ->
    [Substitution] ->
    IO (Either ServiceError ())
validateAnchorDbs depLookup rootDbObj rootDb subs = do
    let rootDbName = unRootDb rootDb
        externalAnchorDbs =
            S.delete rootDbName $
                S.fromList
                    [ cDb
                    | sub <- subs
                    , let (cDb, _) = parseSubRef rootDb (subAnchorRef sub)
                    ]
    if S.null externalAnchorDbs
        then pure (Right ())
        else do
            reachable <- reachableDepDbs depLookup rootDbName rootDbObj
            maybe (pure (Right ())) refuse (S.lookupMin (externalAnchorDbs `S.difference` reachable))
  where
    refuse :: Text -> IO (Either ServiceError ())
    refuse d = do
        mLoad <- depLookup d
        pure $ Left $ MatrixError $ case mLoad of
            Nothing -> "substitution consumer references unloaded database: " <> d
            Just _ ->
                "substitution consumer database '"
                    <> d
                    <> "' is not reachable from root database's dep-graph"

{- | BFS the loaded portion of the dep-DB DAG from @rootDbName@. Returns
the set of DB names that are statically reachable via 'dbCrossDBLinks'
chains (including unloaded leaves – 'validateAnchorDbs' distinguishes
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
    ThisDb ->
    -- | ROOT DB's name (default for bare consumer/from/to refs)
    RootDb ->
    -- | THIS DB's cached solver
    SharedSolver ->
    -- | demand vectors at this level
    [Demand] ->
    -- | full sub list (filtered per level)
    [Substitution] ->
    -- | recursion depth
    Int ->
    IO (Either ServiceError [SharedSolver.CrossDBSolution])
goWithSubsAndDeps unitCfg depLookup thisDb thisDbName rootDb solver demands allSubs depth = do
    scalings <- SharedSolver.solveMultiWithSharedSolver solver demands
    eApply <- applySubstitutionsAt unitCfg depLookup thisDb thisDbName rootDb solver scalings allSubs
    case eApply of
        Left e -> pure (Left e)
        Right (scalings', virtualLks) -> propagate scalings' virtualLks
  where
    propagate scalings' virtualLks = do
        let localInvs = map (applyBiosphereMatrix thisDb) scalings'
            baseSolutions =
                zipWith
                    (\inv s -> SharedSolver.CrossDBSolution inv (NE.singleton (unThisDb thisDbName, thisDb, s)))
                    localInvs
                    scalings'
        if depth >= SharedSolver.maxDepsDepth
            then pure (Right baseSolutions)
            else do
                let perRootDepDemands = map (accumulateDepDemandsWith thisDb virtualLks) scalings'
                    allDepDbs = S.toList $ S.unions $ map M.keysSet perRootDepDemands
                if null allDepDbs
                    then pure (Right baseSolutions)
                    else do
                        depResults <-
                            mapConcurrently
                                (resolveDepWithSubs unitCfg depLookup rootDb perRootDepDemands allSubs depth)
                                allDepDbs
                        pure $ case sequence depResults of
                            Left err -> Left err
                            Right depSolsByDb ->
                                -- Absent dep DBs contribute 'Nothing'; drop
                                -- them before the merge (see 'mergeSolutions').
                                let perRootDepSols = map catMaybes (L.transpose depSolsByDb)
                                 in Right $
                                        zipWith
                                            SharedSolver.mergeSolutions
                                            baseSolutions
                                            perRootDepSols

{- | Dep resolver variant that threads the substitution list into the
recursion. Matches 'SharedSolver.resolveDep' but delegates to
'goWithSubsAndDeps' instead of the plain path.
-}
resolveDepWithSubs ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    -- | ROOT DB's name (default for bare consumer/from/to refs)
    RootDb ->
    [DepDemands] ->
    [Substitution] ->
    Int ->
    Text ->
    IO (Either ServiceError [Maybe SharedSolver.CrossDBSolution])
resolveDepWithSubs unitCfg depLookup rootDb perRootDepDemands allSubs depth depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            -- Same shape as 'SharedSolver.resolveDep': absent dep DB
            -- contributes 'Nothing' at every root; dropped before merge.
            pure (Right (replicate (length perRootDepDemands) Nothing))
        Just (depDb, depSolver) ->
            case SharedSolver.prepareDepDemandVecs unitCfg depDbName depDb perRootDepDemands of
                Left err -> pure (Left (MatrixError err))
                Right depDemandVecs -> do
                    sols <- goWithSubsAndDeps unitCfg depLookup depDb (ThisDb depDbName) rootDb depSolver depDemandVecs allSubs (depth + 1)
                    pure $ fmap (map Just) sols

{- | Cross-DB substitution resolver (root-only path, used by supply-chain).

Solves the root scaling vector then delegates to 'applySubstitutionsAt'
against the root DB. Keeps the \"consumer must live in root\" guard
because supply-chain renders only the root technosphere graph – a
dep-DB consumer sub would be silently ignored here, so we surface it as
an error (the inventory/LCIA path lifts this restriction via
'goWithSubsAndDeps').
-}
computeScalingVectorWithSubstitutionsCrossDB ::
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    Database ->
    -- | root DB name
    Text ->
    SharedSolver ->
    ProcessId ->
    [Substitution] ->
    IO (Either ServiceError (U.Vector Double, [CrossDBLink]))
computeScalingVectorWithSubstitutionsCrossDB unitCfg depLookup db rootDbName solver pid subs =
    -- A global @from@ outside root is reported as such (not as a "consumer"
    -- error); the per-edge consumer guard below then applies only to 'OneEdge'.
    case globalFromMustLiveInRoot rootDb subs of
        Left e -> pure (Left e)
        Right () -> case firstNonRootAnchor of
            Just cDb ->
                pure $
                    Left $
                        MatrixError $
                            "substitution consumer must live in root database (got: " <> cDb <> ")"
            Nothing -> case demandFor db pid of
                Left e -> pure (Left e)
                Right demandVec -> do
                    originalX <- solveWithSharedSolver solver demandVec
                    res <- applySubstitutionsAt unitCfg depLookup db (ThisDb rootDbName) rootDb solver [originalX] subs
                    pure $ case res of
                        Left e -> Left e
                        Right ([x'], links) -> Right (x', links)
                        Right (x' : _, links) -> Right (x', links) -- unreachable: K=1
                        Right ([], _) -> Right (originalX, []) -- unreachable
  where
    rootDb = RootDb rootDbName
    firstNonRootAnchor =
        case [ cDb
             | sub <- subs
             , let (cDb, _) = parseSubRef rootDb (subAnchorRef sub)
             , cDb /= rootDbName
             ] of
            (d : _) -> Just d
            [] -> Nothing

{- | A substitution endpoint resolved against the loaded databases.
'Here' means the activity lives in @thisDb@ (no cross-DB plumbing
needed); 'Elsewhere' carries the dep-DB descriptor required to look up
static cross-DB links and synthesise virtual ones.

The ADT replaces a @(Bool, Bool)@ dispatch on @(fromDb == thisDbName,
toDb == thisDbName)@: each constructor names what the boolean meant.
-}
data Endpoint
    = Here !ProcessId
    | Elsewhere !DepRef

-- | An endpoint that lives in a dependency database.
data DepRef = DepRef
    { drDbName :: !Text
    , drDb :: !Database
    , drPid :: !ProcessId
    , drRef :: !ProcessRef
    }

{- | A planned rank-1 perturbation of one consumer column plus any virtual
cross-DB links the substitution introduces or cancels. Computed purely
from resolved endpoints by 'planUpdate'; consumed effectfully by
'applyRankOne'. Separating the two keeps the four substitution cases
out of the IO layer and makes them straightforward to test in isolation.
-}
data RankOneUpdate = RankOneUpdate
    { ruConsumerPid :: !ProcessId
    , ruPerturb :: ![(Int, Double)]
    , ruExtras :: ![CrossDBLink]
    }

{- | A planned __global__ rank-1 update: replace one supplier by another on
every consumer at once. @gruU@ is the supplier-axis perturbation, @gruV@
the consumer-axis projection (the replaced supplier's technosphere row),
and @gruExtras@ the virtual cross-DB links (only when the new supplier
lives in a dep DB). Consumed by 'perturbGlobal'. See 'planGlobalWithinDB'.
-}
data GlobalRankOneUpdate = GlobalRankOneUpdate
    { gruU :: ![(Int, Double)]
    , gruV :: ![(Int, Double)]
    , gruExtras :: ![CrossDBLink]
    }

{- | The two ends of one technosphere coefficient @A[supplier, consumer]@.
Named because the ends are both a 'ProcessId': read the matrix at
@A[consumer, supplier]@ and you get a well-formed answer about the wrong
edge, which no test would notice.
-}
data TechLink = TechLink
    { tlConsumer :: !ProcessId
    , tlSupplier :: !ProcessId
    }

{- | A global substitution within one database: every consumer of @swapFrom@
buys @swapTo@ instead. Two 'ProcessId's again, and swapping them inverts the
unit factor κ rather than failing.
-}
data Swap = Swap
    { swapFrom :: !ProcessId
    , swapTo :: !ProcessId
    }

{- | The replaced supplier's technosphere row: every consumer that sources
from @supplier@, paired with the (normalized) coefficient. Index space
matches 'perturbA' / 'findTechCoefficient' (ProcessId == matrix index).
Empty when the supplier is consumed nowhere.
-}
technosphereRow :: Database -> ProcessId -> [(Int, Double)]
technosphereRow db supplier =
    let supplierIdx = fromIntegral supplier :: Int32
     in [ (fromIntegral col, val)
        | SparseTriple row col val <- U.toList (dbTechnosphereTriples db)
        , row == supplierIdx
        ]

{- | The replaced supplier's row, or 'Left' when it is consumed nowhere – a
global substitution on such an activity is vacuous, never a silent no-op.
-}
requireConsumers :: Database -> ProcessId -> Either ServiceError [(Int, Double)]
requireConsumers db supplier = case technosphereRow db supplier of
    [] -> Left $ MatrixError $ "global substitution: activity is consumed nowhere: " <> processIdToText db supplier
    row -> Right row

{- | The reference exchange a substitution reads, which is the produced one.
Deliberately narrower than 'exchangeIsReference' alone: an activity that
treats a waste has a reference input, and no unit to normalize a column to.
-}
isReferenceOutput :: Exchange -> Bool
isReferenceOutput ex = exchangeIsReference ex && not (exchangeIsInput ex)

-- | Reference-product unit an activity's technosphere column is normalized to.
referenceProductUnit :: Database -> ProcessId -> Maybe Text
referenceProductUnit db pid =
    getUnitNameForExchange (dbUnits db)
        <$> L.find isReferenceOutput (exchanges (dbActivities db V.! fromIntegral pid))

{- | Unit-conversion factor κ for a within-DB substitution @from → to@: how
many reference units of @to@'s product equal one of @from@'s, so the
coefficient @a@ (in @from@'s unit) becomes @a·κ@ on @to@. Identical units
give @κ = 1@ (matching the per-edge path, which assumes same-unit
suppliers). 'Left' when the two reference products are dimensionally
incompatible – never a silently wrong coefficient.
-}
substitutionUnitFactor :: UnitConfig -> Database -> Swap -> Either ServiceError Double
substitutionUnitFactor unitCfg db (Swap fromPid toPid) = do
    fromUnit <- maybe (Left $ noRefUnit fromPid) Right $ referenceProductUnit db fromPid
    toUnit <- maybe (Left $ noRefUnit toPid) Right $ referenceProductUnit db toPid
    -- Identical units are κ = 1 by definition, independent of the conversion
    -- table (which may not list every unit). Only differing units need a lookup.
    if fromUnit == toUnit
        then Right 1.0
        else maybe (Left $ incompatible fromUnit toUnit) Right $ convertUnit unitCfg fromUnit toUnit 1.0
  where
    noRefUnit pid = MatrixError $ "substitution endpoint has no reference product unit: " <> processIdToText db pid
    incompatible fu tu =
        MatrixError $ "global substitution units are incompatible: " <> fu <> " -> " <> tu <> " (no conversion factor)"

{- | Plan a within-DB global swap @from → to@ as a two-sided rank-1 update:
@u = e_from - κ·e_to@, @v = from@'s technosphere row. The +1 at @from@
removes it from every consumer; the @-κ@ at @to@ adds the unit-converted
demand. No virtual links (both suppliers live in this DB).
-}
planGlobalWithinDB :: UnitConfig -> Database -> Swap -> Either ServiceError GlobalRankOneUpdate
planGlobalWithinDB unitCfg db swap@(Swap fromPid toPid) = do
    v <- requireConsumers db fromPid
    kappa <- substitutionUnitFactor unitCfg db swap
    Right $ GlobalRankOneUpdate [(fromIntegral fromPid, 1.0), (fromIntegral toPid, negate kappa)] v []

{- | Apply all substitutions whose consumer lives in @thisDbName@ to the
given scaling vectors. Substitutions whose consumer lives elsewhere are
skipped at this level – they'll match at the DB where their consumer
lives during the recursive traversal in 'goWithSubsAndDeps'.

Classifies each sub by where its old/new suppliers live relative to
@thisDbName@:

* Case A – both in this DB: symmetric rank-1 update @[(old,+a),(new,-a)]@.
* Case B – old in this DB, new elsewhere: asymmetric root update
  @[(old,+a)]@ plus a virtual @CrossDBLink@ routing demand @+a@ to the
  other-DB supplier.
* Case C – old elsewhere, new in this DB: asymmetric root update
  @[(new,-a_norm)]@ plus a virtual @CrossDBLink@ with negative coefficient
  that cancels the existing static link.
* Case D – both elsewhere: no matrix change; two virtual @CrossDBLink@
  entries (@-a@ on the old supplier, @+a@ on the new).

Missing dep DBs, unresolved qualified PIDs, and Case-C without a matching
static link surface as 'MatrixError' (no silent fallback – the caller
maps to 422).
-}
applySubstitutionsAt ::
    -- | unit config (κ for within-DB global swaps)
    UnitConfig ->
    SharedSolver.DepSolverLookup ->
    -- | THIS DB
    Database ->
    -- | THIS DB's name (the level the walker currently visits)
    ThisDb ->
    -- | ROOT DB (default for bare consumer/from/to refs, per 'Substitution')
    RootDb ->
    -- | THIS DB's cached solver
    SharedSolver ->
    -- | K scalings at this level
    [U.Vector Double] ->
    -- | full sub list (filtered to anchor==this)
    [Substitution] ->
    IO (Either ServiceError ([U.Vector Double], [CrossDBLink]))
applySubstitutionsAt unitCfg depLookup thisDb thisDbObj rootDb solver scalings allSubs =
    case filter anchorLivesHere allSubs of
        [] -> pure $ Right (scalings, [])
        localSubs -> do
            mFact <- getFactorization solver
            runExceptT $ foldM (step mFact) (scalings, []) localSubs
  where
    thisDbName = unThisDb thisDbObj

    -- Bare consumer/from/to refs all default to the root DB (per the
    -- 'Substitution' docstring), not whichever DB the recursive walker
    -- happens to be visiting. Using 'thisDb' instead would cause a bare
    -- consumer to be retried in every dep DB and fail with a spurious
    -- 'ActivityNotFound' on the first dep where the activity does not
    -- exist, and would also misroute bare suppliers in dep-level
    -- recursion. The 'RootDb' newtype on 'parseSubRef' makes the
    -- argument-swap unrepresentable.
    anchorLivesHere sub =
        let (aDb, _) = parseSubRef rootDb (subAnchorRef sub)
         in aDb == thisDbName

    -- Resolve, plan, and apply one substitution; thread the K scalings and
    -- the accumulated virtual links. 'from' is resolved before 'to' so a
    -- failing 'from' wins when both refs are unresolvable. A 'OneEdge' sub
    -- swaps the supplier on its single consumer column; an 'AllConsumers'
    -- sub swaps it on every consumer at once via one global rank-1 update.
    step mFact (xs, links) sub = case subScope sub of
        OneEdge cRef -> do
            let (_, cPidText) = parseSubRef rootDb cRef
            (cPid, _) <- hoistEither $ resolveScorable thisDb cPidText
            (fromEp, toEp) <- resolveFromTo sub
            upd <- hoistEither $ planUpdate sub cPid fromEp toEp
            (xs', extra) <- ExceptT $ applyRankOne mFact xs upd
            pure (xs', links ++ extra)
        AllConsumers -> do
            (fromEp, toEp) <- resolveFromTo sub
            gupd <- hoistEither $ planGlobalUpdate fromEp toEp
            (xs', extra) <- ExceptT $ applyGlobalRankOne mFact xs gupd
            pure (xs', links ++ extra)

    resolveFromTo sub = do
        let (fromDb, fromPidText) = parseSubRef rootDb (subFrom sub)
            (toDb, toPidText) = parseSubRef rootDb (subTo sub)
        fromEp <- ExceptT $ resolveEndpoint fromDb fromPidText
        toEp <- ExceptT $ resolveEndpoint toDb toPidText
        pure (fromEp, toEp)

    hoistEither = ExceptT . pure

    planUpdate ::
        Substitution ->
        ProcessId ->
        Endpoint ->
        Endpoint ->
        Either ServiceError RankOneUpdate
    -- Case A: both suppliers in this DB. Symmetric rank-1 on the consumer column.
    planUpdate sub cPid (Here fromPid) (Here toPid) = do
        a <- requireTech sub cPid fromPid
        Right $
            RankOneUpdate
                cPid
                [(fromIntegral fromPid, a), (fromIntegral toPid, -a)]
                []
    -- Case B: drop this-DB oldSup, route demand to other-DB newSup.
    -- aRaw = aNorm * normFactor (the cross-DB link stores *raw* coefficients).
    planUpdate sub cPid (Here fromPid) (Elsewhere toRef) = do
        a <- requireTech sub cPid fromPid
        let aRaw = a * activityNormalizationFactor thisDb cPid
            newLk = virtualLinkTo cPid toRef aRaw
        Right $ RankOneUpdate cPid [(fromIntegral fromPid, a)] [newLk]
    -- Case C: cancel existing cross-DB link, pull new this-DB supplier.
    planUpdate sub cPid (Elsewhere fromRef) (Here toPid) = do
        s <- requireStatic sub cPid fromRef
        let aRaw = cdlCoefficient s
            aNorm = aRaw / activityNormalizationFactor thisDb cPid
            cancel = s{cdlCoefficient = -aRaw}
        Right $ RankOneUpdate cPid [(fromIntegral toPid, -aNorm)] [cancel]
    -- Case D: re-route demand between two other DBs; this-DB x unchanged.
    -- Unlike Case B, the new-link coefficient is the *raw* static value,
    -- not aNorm*normFactor – we're forwarding what the cancelled link carried.
    planUpdate sub cPid (Elsewhere fromRef) (Elsewhere toRef) = do
        s <- requireStatic sub cPid fromRef
        let aRaw = cdlCoefficient s
            cancel = s{cdlCoefficient = -aRaw}
            newLk = virtualLinkTo cPid toRef aRaw
        Right $ RankOneUpdate cPid [] [cancel, newLk]

    -- Global swap @from → to@ over every consumer of @from@. @from@ must
    -- live in this (root) DB; @to@ either lives here (within-DB, two-sided
    -- rank-1 with unit factor κ) or in a dep DB (one-sided removal of @from@
    -- plus one virtual link per consumer carrying its raw demand to @to@).
    planGlobalUpdate :: Endpoint -> Endpoint -> Either ServiceError GlobalRankOneUpdate
    planGlobalUpdate fromEp toEp = case fromEp of
        Elsewhere _ ->
            Left $ MatrixError "global substitution requires the replaced activity (from) to live in the root database"
        Here fromPid -> case toEp of
            Here toPid -> planGlobalWithinDB unitCfg thisDb (Swap fromPid toPid)
            Elsewhere toRef -> do
                v <- requireConsumers thisDb fromPid
                let links =
                        [ virtualLinkTo (fromIntegral j) toRef (a * activityNormalizationFactor thisDb (fromIntegral j))
                        | (j, a) <- v
                        ]
                Right $ GlobalRankOneUpdate [(fromIntegral fromPid, 1.0)] v links

    virtualLinkTo :: ProcessId -> DepRef -> Double -> CrossDBLink
    virtualLinkTo = mkVirtualLink thisDb

    requireTech sub cPid fromPid =
        maybe (Left $ noTechLink sub cPid) Right $
            findTechCoefficient thisDb (TechLink cPid fromPid)

    requireStatic sub cPid fromRef =
        maybe (Left $ noStaticLink sub cPid (drDbName fromRef) (drPid fromRef)) Right $
            findStaticCrossDBLink thisDb cPid (drDbName fromRef) (drRef fromRef)

    applyRankOne mFact xs upd = do
        -- Apply the same rank-1 update to each of the K vectors. z depends
        -- only on u (not x); a future optimization can compute z once.
        results <-
            mapM
                (\x -> perturbA thisDb mFact x (fromIntegral (ruConsumerPid upd)) (ruPerturb upd))
                xs
        pure $ case sequence results of
            Left msg -> Left (MatrixError msg)
            Right xs' -> Right (xs', ruExtras upd)

    applyGlobalRankOne mFact xs gupd = do
        results <- mapM (\x -> perturbGlobal thisDb mFact x (gruU gupd) (gruV gupd)) xs
        pure $ case sequence results of
            Left msg -> Left (MatrixError msg)
            Right xs' -> Right (xs', gruExtras gupd)

    noTechLink sub cPid =
        MatrixError $
            "No technosphere link from "
                <> processIdToText thisDb cPid
                <> " to supplier "
                <> subFrom sub

    noStaticLink sub cPid fromDb fromPid =
        MatrixError $
            "no cross-DB link from "
                <> processIdToText thisDb cPid
                <> " to "
                <> fromDb
                <> "::"
                <> T.pack (show fromPid)
                <> " (requested by substitution "
                <> subFrom sub
                <> " -> "
                <> subTo sub
                <> ")"

    resolveEndpoint :: Text -> Text -> IO (Either ServiceError Endpoint)
    resolveEndpoint refDb pidText
        | refDb == thisDbName =
            pure $ Here . fst <$> resolveScorable thisDb pidText
        | otherwise = do
            mPair <- depLookup refDb
            pure $ case mPair of
                Nothing ->
                    Left $
                        MatrixError $
                            "substitution references unloaded database: " <> refDb
                Just (depDb, _) -> case resolveScorable depDb pidText of
                    Left _ ->
                        Left $
                            MatrixError $
                                "substitution PID not found in " <> refDb <> ": " <> pidText
                    Right (p, _) ->
                        Right $
                            Elsewhere
                                DepRef
                                    { drDbName = refDb
                                    , drDb = depDb
                                    , drPid = p
                                    , drRef = uncurry ProcessRef (dbProcessIdTable depDb V.! fromIntegral p)
                                    }

{- | Build a synthesized 'CrossDBLink' for a what-if substitution targeting a
dep-DB supplier. Mirrors the fields a real (load-time) link would have so
'accumulateDepDemandsWith' handles it identically – including the raw→refUnit
conversion in 'depDemandsToVector' (we set the exchange unit to the supplier's
own reference-product unit so no conversion is needed).
-}
mkVirtualLink ::
    -- | root DB (consumer side)
    Database ->
    -- | consumer's root ProcessId
    ProcessId ->
    -- | the supplier, in its own dependency database
    DepRef ->
    -- | raw exchange coefficient (pre-normalization)
    Double ->
    CrossDBLink
mkVirtualLink rootDb consumerPid DepRef{drDbName = depDbName, drDb = depDb, drPid = supPid, drRef = supRef} coef =
    let consumerRef = uncurry ProcessRef (dbProcessIdTable rootDb V.! fromIntegral consumerPid)
        supAct = dbActivities depDb V.! fromIntegral supPid
        refUnit = maybe "" (getUnitNameForExchange (dbUnits depDb)) (L.find isReferenceOutput (exchanges supAct))
     in CrossDBLink
            { cdlConsumerActUUID = prActivity consumerRef
            , cdlConsumerProdUUID = prProduct consumerRef
            , -- Substitution links never enter 'dbCrossDBLinks' and the API
              -- surface only indexes load-time links by 'cdlConsumerFlowId',
              -- so the discriminator is unused for synthetic links.
              cdlConsumerFlowId = UUID.nil
            , cdlSupplierActUUID = prActivity supRef
            , cdlSupplierProdUUID = prProduct supRef
            , cdlCoefficient = coef
            , cdlExchangeUnit = refUnit
            , cdlFlowName = activityName supAct
            , cdlLocation = activityLocation supAct
            , cdlSourceDatabase = depDbName
            , cdlTiedAlternatives = []
            }

{- | Find the static 'CrossDBLink' matching @(rootConsumer, depDbName, depSupplierUUIDs)@.
Returns 'Nothing' if no link exists – caller surfaces as 422 rather than
silently no-op.
-}
findStaticCrossDBLink :: Database -> ProcessId -> Text -> ProcessRef -> Maybe CrossDBLink
findStaticCrossDBLink rootDb consumerPid depDbName depSupRef =
    let consumerRef = uncurry ProcessRef (dbProcessIdTable rootDb V.! fromIntegral consumerPid)
        matches lk =
            cdlSourceDatabase lk == depDbName
                && cdlConsumerActUUID lk == prActivity consumerRef
                && cdlConsumerProdUUID lk == prProduct consumerRef
                && ProcessRef (cdlSupplierActUUID lk) (cdlSupplierProdUUID lk) == depSupRef
     in case filter matches (dbCrossDBLinks rootDb) of
            (lk : _) -> Just lk
            [] -> Nothing

-- | Find the technosphere coefficient A[supplier, consumer] from the sparse triples
findTechCoefficient :: Database -> TechLink -> Maybe Double
findTechCoefficient db link =
    coefficient <$> U.find isWanted (dbTechnosphereTriples db)
  where
    consumerIdx, supplierIdx :: Int32
    consumerIdx = fromIntegral (tlConsumer link)
    supplierIdx = fromIntegral (tlSupplier link)
    isWanted :: SparseTriple -> Bool
    isWanted (SparseTriple row col _) = row == supplierIdx && col == consumerIdx
    coefficient :: SparseTriple -> Double
    coefficient (SparseTriple _ _ val) = val

{- | Find all activities that transitively depend on a given supplier.
BFS through the technosphere matrix tracking depth; optional max-depth cap.
When 'cnfEdges' is 'WithEdges', every technosphere coefficient whose endpoints
are both reachable from the supplier is emitted alongside the paginated
result list, mirroring SupplyChainResponse.scrEdges.
-}
getConsumers :: Geographies -> Database -> Text -> Text -> ConsumerFilter -> Either ServiceError ConsumersResponse
getConsumers geographies db dbName processIdText cnf = do
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
            Just pat -> locationAnswers geographies pat (activityLocation activity)

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
                (activityClassification activity)
            | (pid, depth) <- M.toAscList allConsumers
            , nameMatches pid
            , let activity = dbActivities db V.! fromIntegral pid
            , locationMatches activity
            , classMatches activity
            , let (prodName, prodAmount, prodUnit) =
                    getReferenceProductInfo (dbTechFlows db) (dbUnits db) activity
            , productMatches prodName
            ]

        isDesc = afcOrder core == Just "desc"
        crCmp = case afcSort core of
            Just "name" -> \a b -> compare (crActivityName a) (crActivityName b)
            Just "location" -> \a b -> compare (crLocation a) (crLocation b)
            Just "amount" -> \a b -> compare (crProductAmount a) (crProductAmount b)
            Just "unit" -> \a b -> compare (crProductUnit a) (crProductUnit b)
            Just "product" -> \a b -> compare (crProductName a) (crProductName b)
            _ -> \a b -> compare (crDepth a) (crDepth b)
        allResults = L.sortBy (if isDesc then flip crCmp else crCmp) rawResults

        total = length allResults
        page = take limit $ drop offset allResults
        hasMore = offset + limit < total

        -- Every (supplier, consumer) technosphere coefficient whose endpoints
        -- are both reachable from the queried supplier. Populated only when
        -- the caller asks for edges; keeps the default payload identical to
        -- the pre-edges wire shape.
        visitedSet = M.insert processId 0 allConsumers
        edges = case cnfEdges cnf of
            EntriesOnly -> []
            WithEdges ->
                [ SupplyChainEdge
                    { sceEdgeFrom = processIdToText db (fromIntegral row :: ProcessId)
                    , sceEdgeFromDb = dbName
                    , sceEdgeTo = processIdToText db (fromIntegral col :: ProcessId)
                    , sceEdgeToDb = dbName
                    , sceEdgeAmount = val
                    }
                | SparseTriple row col val <- U.toList (dbTechnosphereTriples db)
                , row /= col
                , M.member (fromIntegral row :: ProcessId) visitedSet
                , M.member (fromIntegral col :: ProcessId) visitedSet
                ]

    Right $ ConsumersResponse (SearchResults page total offset limit hasMore 0.0) edges

{- | Export matrix debug data (delegates to Matrix.Export). The row's own
reference is read up front rather than defaulted later: the summary states the
activity it exported, and a row the process id table cannot name is a broken
database, not an empty field.
-}
exportMatrixDebugData :: Database -> Text -> DebugMatricesOptions -> IO (Either ServiceError Value)
exportMatrixDebugData database processIdText opts = do
    case resolveScorable database processIdText >>= withRef of
        Left err -> return $ Left err
        Right (processId, targetActivity, ref) -> do
            matrixDataE <- MatrixExport.extractMatrixDebugInfo database processId (debugFlowFilter opts)
            case matrixDataE of
                Left err -> return $ Left $ MatrixError err
                Right matrixData -> do
                    let inventoryList = MatrixExport.mdInventoryVector matrixData
                        bioFlowUUIDs = MatrixExport.mdBioFlowUUIDs matrixData
                        inventory = M.fromList $ zip (V.toList bioFlowUUIDs) inventoryList

                    Progress.reportProgress Progress.Info $ "DEBUG: Starting CSV export to " ++ debugOutput opts
                    MatrixExport.exportMatrixDebugCSVs (debugOutput opts) matrixData
                    Progress.reportProgress Progress.Info "DEBUG: CSV export completed"

                    let summary =
                            M.fromList
                                [ ("activity_uuid" :: Text, UUID.toText (prActivity ref))
                                , ("activity_name" :: Text, activityName targetActivity)
                                , ("total_inventory_flows" :: Text, T.pack $ show $ M.size inventory)
                                , ("matrix_debug_exported" :: Text, "CSV_EXPORTED")
                                , ("supply_chain_file" :: Text, T.pack $ debugOutput opts ++ "_supply_chain.csv")
                                , ("biosphere_matrix_file" :: Text, T.pack $ debugOutput opts ++ "_biosphere_matrix.csv")
                                ]
                    return $ Right $ toJSON summary
  where
    withRef (pid, act) =
        maybe
            (Left (InvalidUUID ("No activity reference for " <> processIdText)))
            (\ref -> Right (pid, act, ref))
            (processIdToRef database pid)

-- | Export matrices in universal matrix format (delegates to Matrix.Export)
exportUniversalMatrixFormat :: FilePath -> Database -> IO ()
exportUniversalMatrixFormat = MatrixExport.exportUniversalMatrixFormat

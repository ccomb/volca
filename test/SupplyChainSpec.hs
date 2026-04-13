{-# LANGUAGE OverloadedStrings #-}

module SupplyChainSpec (spec) where

import Test.Hspec
import TestHelpers
import GoldenData
import Types
import API.Types (SupplyChainEntry(..), SupplyChainResponse(..), TreeExport(..), TreeMetadata(..), ExportNode(..), TreeEdge(..), FlowInfo(..), NodeType(..), EdgeType(..))
import Service (ActivityFilter(..), buildSupplyChainFromScalingVector, filterTreeExport, bfsToPattern, getPathTo)
import Matrix (computeScalingVector)
import SharedSolver (createSharedSolver)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import Data.UUID (nil)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromText)
import Data.Text (Text, pack)

-- | An ActivityFilter with every field unset. Tests set only the fields they need.
emptyFilter :: ActivityFilter
emptyFilter = ActivityFilter
    { afName = Nothing, afLocation = Nothing, afProduct = Nothing
    , afClassifications = [], afLimit = Nothing, afOffset = Nothing
    , afMaxDepth = Nothing, afMinQuantity = Nothing
    , afSort = Nothing, afOrder = Nothing
    }

spec :: Spec
spec = do
    describe "sceQuantity uses rootRefAmount (not activity refAmount)" $ do
        it "sceQuantity equals scalingFactor when root refAmount is 1" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- SAMPLE.min3: X → Y(0.6) → Z(0.24), all refAmounts = 1 kg
            let rootProcessId = 0 :: ProcessId
            supplyVec <- computeScalingVector db rootProcessId

            let response = buildSupplyChainFromScalingVector db rootProcessId supplyVec emptyFilter False
                entries = scrSupplyChain response

            -- With rootRefAmount = 1, sceQuantity must equal sceScalingFactor exactly
            mapM_ (\entry ->
                withinTolerance defaultTolerance (sceScalingFactor entry) (sceQuantity entry)
                    `shouldBe` True
                ) entries

    describe "sceDepth (BFS from root)" $ do
        it "assigns positive depth to non-root entries" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let rootProcessId = 0 :: ProcessId
            supplyVec <- computeScalingVector db rootProcessId

            let response = buildSupplyChainFromScalingVector db rootProcessId supplyVec emptyFilter False
                entries = scrSupplyChain response

            -- All entries should have depth > 0 (root is excluded from supply chain)
            mapM_ (\entry -> sceDepth entry `shouldSatisfy` (> 0)) entries

    describe "Server-side filtering" $ do
        it "max-depth filter limits results" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let rootProcessId = 0 :: ProcessId
            supplyVec <- computeScalingVector db rootProcessId

            -- No depth filter: should get Y (depth 1) and Z (depth 2)
            let noFilter = buildSupplyChainFromScalingVector db rootProcessId supplyVec emptyFilter False
            scrFilteredActivities noFilter `shouldSatisfy` (>= 2)

            -- Depth 1: should only get Y (direct supplier)
            let depth1 = buildSupplyChainFromScalingVector db rootProcessId supplyVec
                    emptyFilter { afMaxDepth = Just 1 } False
            scrFilteredActivities depth1 `shouldSatisfy` (< scrFilteredActivities noFilter)

    -- -----------------------------------------------------------------------
    -- filterTreeExport
    -- -----------------------------------------------------------------------
    describe "filterTreeExport" $ do
        let dummyFlow = FlowInfo { fiId = nil, fiName = "", fiCategory = "" }
            dummyEdge f t = TreeEdge { teFrom = f, teTo = t, teFlow = dummyFlow
                                     , teQuantity = 1.0, teUnit = "kg"
                                     , teEdgeType = TechnosphereEdge }
            makeNode nid name parent = ExportNode
                { enId = nid, enName = name, enDescription = [], enLocation = ""
                , enUnit = "", enNodeType = ActivityNode, enDepth = 0
                , enLoopTarget = Nothing, enParentId = parent
                , enChildrenCount = 0, enCompartment = Nothing }
            rootNode   = makeNode "r" "root activity"   Nothing
            midNode    = makeNode "m" "middle activity" (Just "r")
            leafNode   = makeNode "l" "leaf activity"   (Just "m")
            meta = TreeMetadata { tmRootId = "r", tmMaxDepth = 3, tmTotalNodes = 3
                                , tmLoopNodes = 0, tmLeafNodes = 1, tmExpandableNodes = 0 }
            tree = TreeExport
                { teTree = meta
                , teNodes = M.fromList [("r", rootNode), ("m", midNode), ("l", leafNode)]
                , teEdges = [dummyEdge "r" "m", dummyEdge "m" "l"]
                }

        it "filter matching leaf keeps leaf + all ancestors" $ do
            let result = filterTreeExport "leaf" tree
            M.keys (teNodes result) `shouldMatchList` ["r", "m", "l"]
            length (teEdges result) `shouldBe` 2

        it "filter matching middle node keeps middle + root only" $ do
            let result = filterTreeExport "middle" tree
            M.keys (teNodes result) `shouldMatchList` ["r", "m"]
            map (\e -> (teFrom e, teTo e)) (teEdges result) `shouldMatchList` [("r", "m")]

        it "filter matching root keeps root only" $ do
            let result = filterTreeExport "root" tree
            M.keys (teNodes result) `shouldMatchList` ["r"]
            length (teEdges result) `shouldBe` 0

        it "filter with no match returns empty" $ do
            let result = filterTreeExport "nonexistent" tree
            M.size (teNodes result) `shouldBe` 0
            length (teEdges result) `shouldBe` 0

        it "updates tmTotalNodes to match filtered count" $ do
            let result = filterTreeExport "leaf" tree
            tmTotalNodes (teTree result) `shouldBe` 3
            let result2 = filterTreeExport "middle" tree
            tmTotalNodes (teTree result2) `shouldBe` 2

    -- -----------------------------------------------------------------------
    -- bfsToPattern
    -- -----------------------------------------------------------------------
    describe "bfsToPattern" $ do
        -- Graph: 0 -> [1, 2], 1 -> [3], 2 -> [], 3 -> []
        let adj = IM.fromList [(0, [1, 2]), (1, [3])]

        it "finds direct neighbour" $
            bfsToPattern 0 (== 2) adj `shouldBe` Just [0, 2]

        it "finds two-hop path" $
            bfsToPattern 0 (== 3) adj `shouldBe` Just [0, 1, 3]

        it "returns Nothing for unreachable node" $
            bfsToPattern 0 (== 99) adj `shouldBe` Nothing

        it "does not match the root itself" $
            bfsToPattern 0 (== 0) adj `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- getPathTo (integration, SAMPLE.min3: X -> Y -> Z)
    -- -----------------------------------------------------------------------
    describe "getPathTo" $ do
        it "finds path from X to Z and computes correct total_ratio" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples = [(fromIntegral i, fromIntegral j, v)
                              | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
                actCount    = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "test" techTriples actCount
            let rootPid = processIdToText db 0
            result <- getPathTo db solver rootPid "product Z"
            case result of
                Left err -> expectationFailure $ "Expected Right but got Left: " ++ show err
                Right val -> do
                    getIntField "path_length" val `shouldBe` Just 3
                    case getDoubleField "total_ratio" val of
                        Nothing -> expectationFailure "missing total_ratio"
                        Just r  -> withinTolerance 1e-9 0.24 r `shouldBe` True

        it "returns Left when target is not in supply chain" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples = [(fromIntegral i, fromIntegral j, v)
                              | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
                actCount    = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "test" techTriples actCount
            let rootPid = processIdToText db 0
            result <- getPathTo db solver rootPid "no such activity"
            case result of
                Left  _ -> return ()
                Right _ -> expectationFailure "Expected Left but got Right"

-- ---------------------------------------------------------------------------
-- Helpers for inspecting Aeson Values
-- ---------------------------------------------------------------------------

getIntField :: String -> Value -> Maybe Int
getIntField k (Object o) = case KM.lookup (fromText (toText k)) o of
    Just (Number n) -> Just (round n)
    _               -> Nothing
getIntField _ _ = Nothing

getDoubleField :: String -> Value -> Maybe Double
getDoubleField k (Object o) = case KM.lookup (fromText (toText k)) o of
    Just (Number n) -> Just (realToFrac n)
    _               -> Nothing
getDoubleField _ _ = Nothing

toText :: String -> Text
toText = pack

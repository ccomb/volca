{-# LANGUAGE OverloadedStrings #-}

module TreeSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import GoldenData
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Tree (buildLoopAwareTree)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "buildLoopAwareTree" $ do
        -- -----------------------------------------------------------------------
        -- Linear chain: SAMPLE.min3  X → Y(0.6) → Z (leaf)
        -- -----------------------------------------------------------------------
        describe "linear chain (SAMPLE.min3)" $ do
            it "builds TreeNode at root with one child" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 10
                case tree of
                    TreeNode act children -> do
                        activityName act `shouldBe` "production of product X"
                        length children `shouldBe` 1
                    _ -> expectationFailure "Expected TreeNode for X"

            it "second level is also a TreeNode (Y)" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 10
                case tree of
                    TreeNode _ [(_, _, subtree)] ->
                        case subtree of
                            TreeNode act _ -> activityName act `shouldBe` "production of product Y"
                            _ -> expectationFailure "Expected TreeNode for Y"
                    _ -> expectationFailure "Unexpected tree shape for X"

            it "leaf node Z is a TreeLeaf with no children" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 10
                case tree of
                    TreeNode _ [(_, _, TreeNode _ [(_, _, leaf)])] ->
                        case leaf of
                            TreeLeaf act -> activityName act `shouldBe` "production of product Z"
                            _ -> expectationFailure "Expected TreeLeaf for Z"
                    _ -> expectationFailure "Unexpected tree shape"

            it "edge amount from X to Y is 0.6" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 10
                case tree of
                    TreeNode _ [(amount, _, _)] -> amount `shouldBe` 0.6
                    _ -> expectationFailure "Expected TreeNode for X"

        -- -----------------------------------------------------------------------
        -- maxDepth limiting
        -- -----------------------------------------------------------------------
        describe "maxDepth" $ do
            it "depth=1 — Y becomes a TreeLoop (depth limit)" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 1
                case tree of
                    TreeNode _ [(_, _, child)] ->
                        case child of
                            TreeLoop _ _ _ -> return ()
                            _ -> expectationFailure "Expected TreeLoop for Y at depth 1"
                    _ -> expectationFailure "Expected TreeNode for X"

            it "depth=0 — root X itself is a TreeLoop" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 0
                case tree of
                    TreeLoop _ _ _ -> return ()
                    _ -> expectationFailure "Expected TreeLoop for X at maxDepth=0"

            it "depth=2 — Z becomes a TreeLoop at depth 2" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just xUUID = UUID.fromString sampleMin3ActivityX
                    tree = buildLoopAwareTree defaultUnitConfig db xUUID 2
                case tree of
                    TreeNode _ [(_, _, TreeNode _ [(_, _, leaf)])] ->
                        case leaf of
                            TreeLoop _ _ _ -> return ()
                            _ -> expectationFailure "Expected TreeLoop for Z at depth 2"
                    _ -> expectationFailure "Unexpected tree shape"

        -- -----------------------------------------------------------------------
        -- Loop detection: SAMPLE.edge  A → B → C → A (circular)
        -- -----------------------------------------------------------------------
        describe "loop detection (SAMPLE.edge)" $ do
            it "circular chain terminates and contains at least one TreeLoop" $ do
                db <- loadSampleDatabase "SAMPLE.edge"
                case findActivityUUIDByName db "circular loop A (dependency test)" of
                    Nothing -> pendingWith "Activity A not found in SAMPLE.edge"
                    Just uuid -> do
                        let tree = buildLoopAwareTree defaultUnitConfig db uuid 10
                        treeContainsLoop tree `shouldBe` True

            it "circular chain does not expand indefinitely (node count bounded)" $ do
                db <- loadSampleDatabase "SAMPLE.edge"
                case findActivityUUIDByName db "circular loop A (dependency test)" of
                    Nothing -> pendingWith "Activity A not found in SAMPLE.edge"
                    Just uuid -> do
                        let tree = buildLoopAwareTree defaultUnitConfig db uuid 100
                        countNodes tree `shouldSatisfy` (<= 300)

        -- -----------------------------------------------------------------------
        -- Missing activity UUID
        -- -----------------------------------------------------------------------
        describe "missing activity" $ do
            it "returns TreeLoop with 'Missing Activity' when UUID not in DB" $ do
                db <- loadSampleDatabase "SAMPLE.min3"
                let Just missingUUID = UUID.fromString "99999999-9999-9999-9999-999999999999"
                    tree = buildLoopAwareTree defaultUnitConfig db missingUUID 10
                case tree of
                    TreeLoop _ name _ -> name `shouldBe` "Missing Activity"
                    _ -> expectationFailure "Expected TreeLoop for missing UUID"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Find the activity UUID for an activity with the given name.
findActivityUUIDByName :: Database -> Text -> Maybe UUID
findActivityUUIDByName db name =
    case [ uuid
         | (uuid, pid) <- M.toList (dbActivityUUIDIndex db)
         , Just act <- [getActivity db pid]
         , activityName act == name
         ] of
        (uuid : _) -> Just uuid
        [] -> Nothing

treeContainsLoop :: LoopAwareTree -> Bool
treeContainsLoop (TreeLeaf _) = False
treeContainsLoop (TreeLoop _ _ _) = True
treeContainsLoop (TreeNode _ children) = any (\(_, _, t) -> treeContainsLoop t) children

countNodes :: LoopAwareTree -> Int
countNodes (TreeLeaf _) = 1
countNodes (TreeLoop _ _ _) = 1
countNodes (TreeNode _ children) = 1 + sum (map (\(_, _, t) -> countNodes t) children)

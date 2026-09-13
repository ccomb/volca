{-# LANGUAGE OverloadedStrings #-}

module TreeSpec (spec) where

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
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 10
                case tree of
                    TreeNode _ act children -> do
                        activityName act `shouldBe` "production of product X"
                        length children `shouldBe` 1
                    _ -> expectationFailure "Expected TreeNode for X"

            it "second level is also a TreeNode (Y)" $ do
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 10
                case tree of
                    TreeNode _ _ [TreeChild{childSubtree = subtree}] ->
                        case subtree of
                            TreeNode _ act _ -> activityName act `shouldBe` "production of product Y"
                            _ -> expectationFailure "Expected TreeNode for Y"
                    _ -> expectationFailure "Unexpected tree shape for X"

            it "leaf node Z is a TreeLeaf with no children" $ do
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 10
                case tree of
                    TreeNode _ _ [TreeChild{childSubtree = TreeNode _ _ [TreeChild{childSubtree = leaf}]}] ->
                        case leaf of
                            TreeLeaf _ act -> activityName act `shouldBe` "production of product Z"
                            _ -> expectationFailure "Expected TreeLeaf for Z"
                    _ -> expectationFailure "Unexpected tree shape"

            it "edge amount from X to Y is 0.6" $ do
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 10
                case tree of
                    TreeNode _ _ [TreeChild{childAmount = amount}] -> amount `shouldBe` 0.6
                    _ -> expectationFailure "Expected TreeNode for X"

        -- -----------------------------------------------------------------------
        -- maxDepth limiting
        -- -----------------------------------------------------------------------
        describe "maxDepth" $ do
            it "depth=1 – Y becomes a TreeLoop (depth limit)" $ do
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 1
                case tree of
                    TreeNode _ _ [TreeChild{childSubtree = child}] ->
                        case child of
                            TreeLoop{} -> return ()
                            _ -> expectationFailure "Expected TreeLoop for Y at depth 1"
                    _ -> expectationFailure "Expected TreeNode for X"

            it "depth=0 – root X itself is a TreeLoop" $ do
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 0
                case tree of
                    TreeLoop{} -> return ()
                    _ -> expectationFailure "Expected TreeLoop for X at maxDepth=0"

            it "depth=2 – Z becomes a TreeLoop at depth 2" $ do
                tree <- treeFromSample "SAMPLE.min3" sampleMin3ActivityX 2
                case tree of
                    TreeNode _ _ [TreeChild{childSubtree = TreeNode _ _ [TreeChild{childSubtree = leaf}]}] ->
                        case leaf of
                            TreeLoop{} -> return ()
                            _ -> expectationFailure "Expected TreeLoop for Z at depth 2"
                    _ -> expectationFailure "Unexpected tree shape"

        -- -----------------------------------------------------------------------
        -- Loop detection: SAMPLE.edge  A → B → C → A (circular)
        -- -----------------------------------------------------------------------
        describe "loop detection (SAMPLE.edge)" $ do
            it "circular chain terminates and contains at least one TreeLoop" $ do
                db <- loadSampleDatabase "SAMPLE.edge"
                case rootByName db "circular loop A (dependency test)" of
                    Nothing -> pendingWith "Activity A not found in SAMPLE.edge"
                    Just root -> treeContainsLoop (buildLoopAwareTree defaultUnitConfig db 10 root) `shouldBe` True

            it "circular chain does not expand indefinitely (node count bounded)" $ do
                db <- loadSampleDatabase "SAMPLE.edge"
                case rootByName db "circular loop A (dependency test)" of
                    Nothing -> pendingWith "Activity A not found in SAMPLE.edge"
                    Just root -> countNodes (buildLoopAwareTree defaultUnitConfig db 100 root) `shouldSatisfy` (<= 300)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The tree of one sample activity, named by its activity UUID.
treeFromSample :: String -> String -> Int -> IO LoopAwareTree
treeFromSample sample activityUUID maxDepth = do
    db <- loadSampleDatabase sample
    case UUID.fromString activityUUID >>= rootOf db of
        Nothing -> fail ("no row for " <> activityUUID <> " in " <> sample)
        Just root -> pure (buildLoopAwareTree defaultUnitConfig db maxDepth root)

-- | The row an activity UUID names, with the activity it holds.
rootOf :: Database -> UUID -> Maybe (ProcessId, Activity)
rootOf db uuid = do
    pid <- findProcessIdByActivityUUID db uuid
    act <- getActivity db pid
    pure (pid, act)

-- | The first row whose activity carries the given name.
rootByName :: Database -> Text -> Maybe (ProcessId, Activity)
rootByName db name =
    case [ (pid, act)
         | pid <- [0 .. dbActivityCount db - 1]
         , Just act <- [getActivity db pid]
         , activityName act == name
         ] of
        (root : _) -> Just root
        [] -> Nothing

treeContainsLoop :: LoopAwareTree -> Bool
treeContainsLoop (TreeLeaf _ _) = False
treeContainsLoop (TreeMissing{}) = False
treeContainsLoop (TreeLoop{}) = True
treeContainsLoop (TreeNode _ _ children) = any (treeContainsLoop . childSubtree) children

countNodes :: LoopAwareTree -> Int
countNodes (TreeLeaf _ _) = 1
countNodes (TreeMissing{}) = 1
countNodes (TreeLoop{}) = 1
countNodes (TreeNode _ _ children) = 1 + sum (map (countNodes . childSubtree) children)

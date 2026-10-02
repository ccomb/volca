{-# LANGUAGE OverloadedStrings #-}

module MethodEditSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (void)
import qualified Data.Map.Strict as M
import Data.UUID (UUID)
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

import Config (MethodPatch (..), MethodPatchMatch (..), defaultConfig)
import Data.JournalFile (journalPath)
import Database.Manager (CachePolicy (..), CollectionName (..), DatabaseManager (..), getMethodCollection, initDatabaseManager, loadMethodCollection, unloadMethodCollection)
import Database.UploadedDatabase (getMethodUploadsDir)
import Method.Edit
import Method.EditPlan (EditEffect (..), FactorEdit (..), FactorTarget (..))
import Method.Mapping (MethodIndex (..))
import Method.Types (Method (..), MethodCF (..), MethodCollection (..))
import TestHelpers (withScratchDataDir)
import Types (PatchOp (..))

-- | A fresh copy of the built-in collection, its « Methane » category and that category's « Methane, fossil » factor.
copyWithMethane :: IO (DatabaseManager, UUID, MethodCF)
copyWithMethane = do
    manager <- initDatabaseManager defaultConfig NoCache
    copied <- copyMethodCollection manager "plain-indicators" "copy"
    copied `shouldBe` Right "copy"
    collection <- getMethodCollection manager "copy"
    case [(methodId m, f) | m <- maybe [] mcMethods collection, methodName m == "Methane", f <- methodFactors m, mcfFlowName f == "Methane, fossil"] of
        [(category, methane)] -> pure (manager, category, methane)
        found -> fail ("expected one Methane, fossil factor in the Methane category, found " <> show (length found))

anyPatch :: MethodPatch
anyPatch = MethodPatch Nothing (MethodPatchMatch Nothing Nothing (Just "Methane") Nothing Nothing) (ScaleBy 2)

emptyIndex :: MethodIndex
emptyIndex = MethodIndex V.empty V.empty M.empty M.empty

spec :: Spec
spec = describe "changing a method collection of one's own" $ do
    it "sets a factor, records one line, and a reload gives the same collection" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            outcome <- editMethodFactors manager "copy" (SetValue (FactorTarget category (mcfFlowRef methane) Nothing Nothing) 27)
            fmap eoLine outcome `shouldBe` Right 1
            fmap (eeBefore . eoEffect) outcome `shouldBe` Right (Just 1.0)
            edited <- getMethodCollection manager "copy"
            _ <- unloadMethodCollection manager "copy"
            _ <- loadMethodCollection manager "copy"
            reloaded <- getMethodCollection manager "copy"
            reloaded `shouldBe` edited

    it "refuses a configured collection, and says to copy it" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            refused <- editMethodFactors manager "plain-indicators" (Patch anyPatch)
            void refused `shouldBe` Left (NotEditable "plain-indicators")

    it "refuses a collection that is not loaded, and writes nothing" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            _ <- unloadMethodCollection manager "copy"
            refused <- editMethodFactors manager "copy" (Remove (FactorTarget category (mcfFlowRef methane) Nothing Nothing))
            void refused `shouldBe` Left (CollectionNotLoaded "copy")
            home <- (</> "copy") <$> getMethodUploadsDir
            doesFileExist (journalPath home) `shouldReturn` False
            getMethodCollection manager "copy" `shouldReturn` Nothing

    it "drops the tables built from this collection and keeps the others'" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            let key c = ("db", CollectionName c, category)
            atomically $ modifyTVar' (dmMethodIndexCache manager) (M.insert (key "copy") emptyIndex . M.insert (key "plain-indicators") emptyIndex)
            _ <- editMethodFactors manager "copy" (SetValue (FactorTarget category (mcfFlowRef methane) Nothing Nothing) 2)
            M.keys <$> readTVarIO (dmMethodIndexCache manager) `shouldReturn` [key "plain-indicators"]

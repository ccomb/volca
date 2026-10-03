{-# LANGUAGE OverloadedStrings #-}

module MethodEditSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (replicateM_, void)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Config (MethodPatch (..), MethodPatchMatch (..), defaultConfig)
import Data.JournalFile (journalPath)
import Database.Manager (CachePolicy (..), CollectionName (..), DatabaseManager (..), getMethodCollection, initDatabaseManager, loadMethodCollection, mapMethodToIndexCached, unloadMethodCollection)
import Database.UploadedDatabase (getMethodUploadsDir)
import Method.Edit
import Method.EditPlan (CategoryDraft (..), CategoryEdit (..), EditEffect (..), FactorEdit (..), FactorTarget (..))
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

    it "never caches what was built from a factor a change has since replaced" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            let methaneOf = maybe [] (filter ((== category) . methodId) . mcMethods)
            [before] <- methaneOf <$> getMethodCollection manager "copy"
            _ <- editMethodFactors manager "copy" (SetValue (FactorTarget category (mcfFlowRef methane) Nothing Nothing) 2)
            _ <- mapMethodToIndexCached manager "db" (CollectionName "copy") before
            M.keys <$> readTVarIO (dmMethodIndexCache manager) `shouldReturn` []
            [after] <- methaneOf <$> getMethodCollection manager "copy"
            _ <- mapMethodToIndexCached manager "db" (CollectionName "copy") after
            M.keys <$> readTVarIO (dmMethodIndexCache manager) `shouldReturn` [("db", CollectionName "copy", category)]

    it "waits for a change being written before unloading the collection" $
        withScratchDataDir $ do
            (manager, _, _) <- copyWithMethane
            takeMVar (dmMethodEditLock manager)
            done <- newEmptyMVar
            _ <- forkIO (unloadMethodCollection manager "copy" >>= putMVar done)
            timeout 100000 (readMVar done) `shouldReturn` Nothing
            putMVar (dmMethodEditLock manager) ()
            readMVar done `shouldReturn` Right ()

    it "undoes N changes back to the collection it started from, in 2N lines" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            start <- getMethodCollection manager "copy"
            let at v = FactorTarget category (mcfFlowRef methane) Nothing (Just v)
            _ <- editMethodFactors manager "copy" (SetValue (at 1) 2)
            _ <- editMethodFactors manager "copy" (Remove (at 2))
            _ <- editMethodFactors manager "copy" (Patch anyPatch)
            replicateM_ 3 (undoMethodEdit manager "copy" Nothing >>= either (expectationFailure . show) (const (pure ())))
            getMethodCollection manager "copy" `shouldReturn` start
            fmap length <$> methodHistory manager "copy" `shouldReturn` Right 6
            void <$> undoMethodEdit manager "copy" Nothing `shouldReturn` Left (EditRefused "there is no change left to undo")

    it "redoes an undone change when asked for its undo line" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            _ <- editMethodFactors manager "copy" (SetValue (FactorTarget category (mcfFlowRef methane) Nothing Nothing) 5)
            edited <- getMethodCollection manager "copy"
            _ <- undoMethodEdit manager "copy" Nothing
            _ <- undoMethodEdit manager "copy" (Just 2)
            getMethodCollection manager "copy" `shouldReturn` edited

    it "refuses to undo a selector one of whose factors a later line changed, and writes nothing" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            _ <- editMethodFactors manager "copy" (Patch anyPatch)
            _ <- editMethodFactors manager "copy" (SetValue (FactorTarget category (mcfFlowRef methane) Nothing Nothing) 9)
            refused <- undoMethodEdit manager "copy" (Just 1)
            either (T.unpack . refusalText) (const "undone") refused `shouldContain` "Methane, fossil"
            fmap length <$> methodHistory manager "copy" `shouldReturn` Right 2

    it "refuses to undo a change of value a later line changed again, naming that line" $
        withScratchDataDir $ do
            (manager, category, methane) <- copyWithMethane
            let at = FactorTarget category (mcfFlowRef methane) Nothing Nothing
            _ <- editMethodFactors manager "copy" (SetValue at 2)
            _ <- editMethodFactors manager "copy" (SetValue at 3)
            refused <- undoMethodEdit manager "copy" (Just 1)
            either (T.unpack . refusalText) (const "undone") refused `shouldContain` "undo line 2 first"
            fmap length <$> methodHistory manager "copy" `shouldReturn` Right 2

    it "gives a collection the configuration declares an empty history" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            fmap length <$> methodHistory manager "plain-indicators" `shouldReturn` Right 0

    it "adds a category, answers with its identifier, and keeps it across a reload" $
        withScratchDataDir $ do
            (manager, _, _) <- copyWithMethane
            added <- editMethodCategories manager "copy" (NewCategory (CategoryDraft "A category of my own" "kg" Nothing Nothing))
            category <- either (fail . show) (maybe (fail "no category in the answer") pure . eoCategory) added
            before <- getMethodCollection manager "copy"
            _ <- unloadMethodCollection manager "copy"
            _ <- loadMethodCollection manager "copy"
            after <- getMethodCollection manager "copy"
            after `shouldBe` before
            fmap (map methodName . filter ((== category) . methodId) . mcMethods) after `shouldBe` Just ["A category of my own"]

    it "refuses to undo the addition of a category a later line added a factor to, naming that line" $
        withScratchDataDir $ do
            (manager, _, methane) <- copyWithMethane
            added <- editMethodCategories manager "copy" (NewCategory (CategoryDraft "A category of my own" "kg" Nothing Nothing))
            category <- either (fail . show) (maybe (fail "no category in the answer") pure . eoCategory) added
            _ <- editMethodFactors manager "copy" (Add category methane)
            refused <- undoMethodEdit manager "copy" (Just 1)
            either (T.unpack . refusalText) (const "undone") refused `shouldContain` "undo line 2 first"
            fmap length <$> methodHistory manager "copy" `shouldReturn` Right 2

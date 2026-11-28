{-# LANGUAGE OverloadedStrings #-}

module MethodCreateSpec (spec) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Test.Hspec

import Config (defaultConfig)
import Data.JournalFile (noOrigin)
import Database.Manager (CachePolicy (..), MethodCollectionStatus (..), getMethodCollection, initDatabaseManager, listMethodCollections, loadMethodCollection, removeMethodCollection, unloadMethodCollection)
import Database.UploadedDatabase (getMethodUploadsDir)
import Method.Edit (MethodEditRefusal (..), copyMethodCollection, createMethodCollection, editMethodCategories)
import Method.EditPlan (CategoryDraft (..), CategoryEdit (..))
import Method.Types (Method (..), MethodCollection (..))
import TestHelpers (withScratchDataDir)

-- | The categories of a loaded collection, by name.
categoryNames :: Maybe MethodCollection -> Maybe [Text]
categoryNames = fmap (map methodName . mcMethods)

spec :: Spec
spec = describe "creating a method collection of one's own" $ do
    it "creates an empty collection, loaded at once, known by its slug" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            createMethodCollection manager "My Method" `shouldReturn` Right "my-method"
            categoryNames <$> getMethodCollection manager "my-method" `shouldReturn` Just []

    it "finds an empty authored collection again at the next start" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- createMethodCollection manager "My Method"
            restarted <- initDatabaseManager defaultConfig NoCache
            listed <- listMethodCollections restarted
            map mcsName listed `shouldContain` ["my-method"]
            loadMethodCollection restarted "my-method" `shouldReturn` Right ()
            categoryNames <$> getMethodCollection restarted "my-method" `shouldReturn` Just []

    it "finds a copy of an authored collection again at the next start, with its categories" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- createMethodCollection manager "mine"
            added <- editMethodCategories manager "mine" noOrigin (NewCategory (CategoryDraft "Land use" "Pt" Nothing Nothing))
            void added `shouldBe` Right ()
            copyMethodCollection manager "mine" "copy" `shouldReturn` Right "copy"
            restarted <- initDatabaseManager defaultConfig NoCache
            loadMethodCollection restarted "copy" `shouldReturn` Right ()
            categoryNames <$> getMethodCollection restarted "copy" `shouldReturn` Just ["Land use"]

    it "refuses a name another collection already slugs to, writing nothing" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- createMethodCollection manager "my method"
            createMethodCollection manager "My Method" `shouldReturn` Left (NameTaken "my-method")
            createMethodCollection manager "plain indicators" `shouldReturn` Left (NameTaken "plain-indicators")
            uploads <- getMethodUploadsDir
            doesDirectoryExist (uploads </> "plain-indicators") `shouldReturn` False

    it "deletes an authored collection" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- createMethodCollection manager "mine"
            _ <- unloadMethodCollection manager "mine"
            removeMethodCollection manager "mine" `shouldReturn` Right ()
            listed <- listMethodCollections manager
            map mcsName listed `shouldNotContain` ["mine"]
            uploads <- getMethodUploadsDir
            doesDirectoryExist (uploads </> "mine") `shouldReturn` False

    it "leaves out a meta.toml that reads its journal and names a built-in collection" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- createMethodCollection manager "mine"
            home <- (</> "mine") <$> getMethodUploadsDir
            TIO.appendFile (home </> "meta.toml") "builtin = \"plain-indicators\"\n"
            restarted <- initDatabaseManager defaultConfig NoCache
            listed <- listMethodCollections restarted
            map mcsName listed `shouldNotContain` ["mine"]
            map mcsName listed `shouldContain` ["plain-indicators"]

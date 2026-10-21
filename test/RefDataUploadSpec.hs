{-# LANGUAGE OverloadedStrings #-}

{- | An uploaded reference table lives in one directory under the data
directory: the upload writes it there, the next start finds it there, and a
delete removes it from there.
-}
module RefDataUploadSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import System.Directory (doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Hspec
import TestHelpers (withScratchDataDir)

import API.DatabaseHandlers (RefDataKind (..), deleteRefData, uploadRefData)
import API.Types (UploadChunk (..))
import App.Env (AppEnv (..), runApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Servant (runHandler)
import Servant.Types.SourceT (source)

envFor :: IO AppEnv
envFor = do
    manager <- initDatabaseManager defaultConfig NoCache
    pure
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            , aeUsageLog = Nothing
            , aeReader = Nothing
            }

spec :: Spec
spec = describe "An uploaded reference table" $
    -- The table is registered under the name the user typed and written
    -- under a slug of it: the delete must find the slug.
    it "is removed from disk when deleted under the name it was uploaded with" $
        withScratchDataDir $ do
            dataDir <- getEnv "VOLCA_DATA_DIR"
            env <- envFor
            let dir = dataDir </> "uploads" </> "flow-synonyms" </> "my-synonyms"
                csv = source [UploadChunk (BS.pack "name1,name2\nalpha,beta\n")]
                run h = runHandler (runApp env h) >>= either (fail . show) pure
            _ <- run (uploadRefData FlowSynonyms (Just "My Synonyms") Nothing csv)
            doesDirectoryExist dir `shouldReturn` True
            _ <- run (deleteRefData FlowSynonyms "My Synonyms")
            doesDirectoryExist dir `shouldReturn` False

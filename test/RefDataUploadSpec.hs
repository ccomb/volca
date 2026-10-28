{-# LANGUAGE OverloadedStrings #-}

{- | An uploaded reference table lives in one directory under the data
directory: the upload writes it there, the next start finds it there, and a
delete removes it from there.
-}
module RefDataUploadSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Directory (doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Hspec
import TestHelpers (withScratchDataDir)

import API.DatabaseHandlers (RefDataKind (..), deleteRefData, downloadRefDataHandler, refDataRows, refDataRowsHandler, uploadRefData)
import API.Types (BinaryContent (..), RefDataRowsResponse (..), UploadChunk (..))
import App.Env (AppEnv (..), runApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Servant (getResponse, runHandler)
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
spec = do
    describe "The built-in unit table" $
        it "downloads as the table the engine reads" $
            withScratchDataDir $ do
                env <- envFor
                response <- runHandler (runApp env (downloadRefDataHandler UnitDefs "Default units")) >>= either (fail . show) pure
                let BinaryContent csv = getResponse response
                take 2 (BSL.lines csv) `shouldBe` ["name,dimension,factor", "kg,mass,1.0"]
    describe "The rows of a reference table" $ do
        it "are the built-in unit table, header apart" $
            withScratchDataDir $ do
                env <- envFor
                RefDataRowsResponse header rows <- runHandler (runApp env (refDataRowsHandler UnitDefs "Default units")) >>= either (fail . show) pure
                header `shouldBe` ["name", "dimension", "factor"]
                take 1 rows `shouldBe` [["kg", "mass", "1.0"]]
        it "are each as wide as the header, the built-in compartments' optional kind included" $
            withScratchDataDir $ do
                env <- envFor
                RefDataRowsResponse header rows <- runHandler (runApp env (refDataRowsHandler CompartmentMappings "Default compartment mapping")) >>= either (fail . show) pure
                length header `shouldBe` 7
                rows `shouldSatisfy` all ((== 7) . length)
        it "end a short row in empty cells, and refuse a row wider than the header" $ do
            fmap rdrRows (refDataRows "a,b,c\n1,2\n") `shouldBe` Right [["1", "2", ""]]
            fmap rdrRows (refDataRows "a,b\n1,2,3\n") `shouldSatisfy` either (const True) (const False)
    describe "An uploaded reference table" $
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

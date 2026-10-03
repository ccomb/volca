{-# LANGUAGE OverloadedStrings #-}

module MethodJournalLoadSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import Config (defaultConfig)
import Data.JournalFile (appendEntry, journalPath)
import Database.Manager (CachePolicy (..), getMethodCollection, initDatabaseManager, loadMethodCollection)
import Database.UploadedDatabase (getMethodUploadsDir)
import Method.Journal
import Method.Types
import TestHelpers (withScratchDataDir)

-- | A columnar method with one category and one flow written twice at one place.
methodCsv :: T.Text
methodCsv =
    T.unlines
        [ ";;;;Ecotoxicity"
        , ";;;;Ecotoxicity"
        , ";;;;CTUe"
        , "substance;compartment;cas;unit;"
        , "turpentine;soil;;kg;8.399"
        , "turpentine;soil;;kg;1.1619"
        , "Ammonia;air;7664-41-7;kg;3.0"
        ]

-- | An uploaded collection's home, as an upload leaves it.
writeUpload :: FilePath -> IO ()
writeUpload home = do
    createDirectoryIfMissing True home
    TIO.writeFile (home </> "method.csv") methodCsv
    TIO.writeFile (home </> "meta.toml") "version = 5\ndisplayName = \"ecotox\"\nformat = \"simapro\"\ndataPath = \".\"\ndepends = []\nallocation = \"declared\"\n"

loadedEcotox :: IO (Either T.Text MethodCollection)
loadedEcotox = do
    manager <- initDatabaseManager defaultConfig NoCache
    loaded <- loadMethodCollection manager "ecotox"
    collection <- getMethodCollection manager "ecotox"
    pure (loaded >> maybe (Left "not loaded") Right collection)

ammoniaValues :: MethodCollection -> [Double]
ammoniaValues = map mcfValue . filter ((== "Ammonia") . mcfFlowName) . concatMap methodFactors . mcMethods

spec :: Spec
spec = describe "loading an uploaded method collection" $ do
    it "replays its journal over its files" $
        withScratchDataDir $ do
            home <- (</> "ecotox") <$> getMethodUploadsDir
            writeUpload home
            before <- loadedEcotox
            case before of
                Right MethodCollection{mcMethods = [category]}
                    | [ammonia] <- filter ((== "Ammonia") . mcfFlowName) (methodFactors category) -> do
                        appendEntry home (MethodLine (SetFactor (methodId category) ammonia 27) Change) `shouldReturn` Right ()
                        after <- loadedEcotox
                        fmap ammoniaValues after `shouldBe` Right [27]
                other -> expectationFailure ("expected one category with one ammonia factor, got " <> show other)

    it "refuses to load when the source no longer holds what a line recorded, naming the line" $
        withScratchDataDir $ do
            home <- (</> "ecotox") <$> getMethodUploadsDir
            writeUpload home
            BS.writeFile (journalPath home) "{\"v\":1,\"at\":\"t\",\"op\":\"set-global-methods\",\"before\":[\"Nothing\"],\"after\":[]}\n"
            result <- loadedEcotox
            either T.unpack (const "loaded") result `shouldContain` "journal line 1 (set-global-methods)"

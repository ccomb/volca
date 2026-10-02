{-# LANGUAGE OverloadedStrings #-}

module TermsSpec (spec) where

import qualified Data.Map.Strict as M
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import API.DatabaseHandlers (downloadRefusal)
import Config (DatabaseConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager, TermsRefusal (..), addDatabase, databaseTerms, initDatabaseManager, setUploadTerms)
import qualified Database.UploadedDatabase as UploadedDB
import TestHelpers (withScratchDataDir)
import Types (AllocationKey (..), Downloads (..), GeographyPolicy (..), Terms (..), openTerms)

configured :: DatabaseConfig
configured =
    DatabaseConfig
        { dcName = "configured"
        , dcDisplayName = "configured"
        , dcPath = "test-data/SAMPLE.ilcd"
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        , dcTerms = openTerms
        }

refused :: Terms
refused = Terms{termsLicence = Just "Members only", termsDownloads = DownloadsRefused}

-- | A manager holding one upload with its meta.toml, and a copy of it.
withUpload :: (DatabaseManager -> FilePath -> IO ()) -> IO ()
withUpload k = withScratchDataDir $ do
    home <- (</> "upload") <$> UploadedDB.getDatabaseUploadsDir
    createDirectoryIfMissing True home
    UploadedDB.writeUploadMeta
        home
        UploadedDB.UploadMeta
            { UploadedDB.umVersion = UploadedDB.metaVersion
            , UploadedDB.umDisplayName = "upload"
            , UploadedDB.umDescription = Nothing
            , UploadedDB.umFormat = UploadedDB.ILCDProcess
            , UploadedDB.umDataPath = "data"
            , UploadedDB.umDepends = []
            , UploadedDB.umSource = Nothing
            , UploadedDB.umAllocation = Declared
            , UploadedDB.umTerms = openTerms
            }
    manager <- initDatabaseManager defaultConfig NoCache
    let upload = configured{dcName = "upload", dcIsUploaded = True}
    addDatabase manager upload
    addDatabase manager upload{dcName = "copy", dcSource = Just "upload"}
    k manager home

spec :: Spec
spec = do
    describe "setUploadTerms" $ do
        it "records the terms of an upload where a restart reads them" $
            withUpload $ \manager home -> do
                setUploadTerms manager "upload" refused `shouldReturn` Right refused
                databaseTerms manager "upload" `shouldReturn` Just refused
                fmap UploadedDB.umTerms <$> UploadedDB.readUploadMeta home `shouldReturn` Just refused

        it "serves a copy under the terms its source was given after the copy was made" $
            withUpload $ \manager _ -> do
                _ <- setUploadTerms manager "upload" refused
                databaseTerms manager "copy" `shouldReturn` Just refused

        it "refuses to set the terms of a copy, which are its source's" $
            withUpload $ \manager _ ->
                setUploadTerms manager "copy" refused
                    `shouldReturn` Left (TermsHeldElsewhere "copy reads the files of upload and is served under its terms")

        it "refuses to set the terms of a configured database, which are the configuration file's" $
            withScratchDataDir $ do
                manager <- initDatabaseManager defaultConfig NoCache
                addDatabase manager configured
                setUploadTerms manager "configured" refused
                    `shouldReturn` Left (TermsHeldElsewhere "configured is set in the configuration file, which is where its terms are written")

        it "refuses a name the engine does not know" $
            withScratchDataDir $ do
                manager <- initDatabaseManager defaultConfig NoCache
                setUploadTerms manager "nothing" refused `shouldReturn` Left (TermsUnknown "Database not found: nothing")

    describe "downloadRefusal" $ do
        it "lets an allowed database be downloaded" $
            downloadRefusal "db" openTerms `shouldBe` Nothing

        it "names the licence in its refusal" $
            downloadRefusal "db" refused `shouldBe` Just "The terms of db (Members only) do not allow downloading it."

{-# LANGUAGE OverloadedStrings #-}

module LicenceSpec (spec) where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import API.DatabaseHandlers (licenceRefusal)
import API.Resources (Resource (..), resourceNeeds)
import Config (DatabaseConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager, DatabaseStatus (..), LicenceRefusal (..), addDatabase, databaseLicence, initDatabaseManager, listDatabases, setUploadLicence)
import qualified Database.UploadedDatabase as UploadedDB
import TestHelpers (membersOnly, withScratchDataDir)
import Types (
    AllocationKey (..),
    Attribution (..),
    GeographyPolicy (..),
    Licence (..),
    LicenceKeys (..),
    OwnLicence (..),
    Permission (..),
    StandardLicence (..),
    granted,
    licenceFromKeys,
    licenceView,
    lvPermissions,
    parseSpdx,
    psEnforced,
    psPermission,
    spdxId,
    standardLicences,
 )

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
        , dcLicence = LicenceUnstated
        }

refused :: Licence
refused = membersOnly

keys :: LicenceKeys
keys = LicenceKeys Nothing Nothing Nothing Nothing

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
            , UploadedDB.umBuiltIn = Nothing
            , UploadedDB.umLicence = LicenceUnstated
            }
    manager <- initDatabaseManager defaultConfig NoCache
    let upload = configured{dcName = "upload", dcIsUploaded = True}
    addDatabase manager upload
    addDatabase manager upload{dcName = "copy", dcSource = Just "upload"}
    k manager home

spec :: Spec
spec = do
    describe "setUploadLicence" $ do
        it "records the licence of an upload where a restart reads them" $
            withUpload $ \manager home -> do
                setUploadLicence manager "upload" refused `shouldReturn` Right refused
                databaseLicence manager "upload" `shouldReturn` Just refused
                fmap UploadedDB.umLicence <$> UploadedDB.readUploadMeta home `shouldReturn` Just refused

        it "serves a copy under the licence its source was given after the copy was made" $
            withUpload $ \manager _ -> do
                _ <- setUploadLicence manager "upload" refused
                databaseLicence manager "copy" `shouldReturn` Just refused

        it "lists a copy under the licence its source was given after the copy was made" $
            withUpload $ \manager _ -> do
                _ <- setUploadLicence manager "upload" refused
                listed <- listDatabases manager
                [dsLicence s | s <- listed, dsName s == "copy"] `shouldBe` [refused]

        it "refuses to set the licence of a copy, which is its source's" $
            withUpload $ \manager _ ->
                setUploadLicence manager "copy" refused
                    `shouldReturn` Left (LicenceHeldElsewhere "copy reads the files of upload and is served under its licence")

        it "refuses to set the licence of a configured database, which is the configuration file's" $
            withScratchDataDir $ do
                manager <- initDatabaseManager defaultConfig NoCache
                addDatabase manager configured
                setUploadLicence manager "configured" refused
                    `shouldReturn` Left (LicenceHeldElsewhere "configured is set in the configuration file, which is where its licence is written")

        it "refuses an upload whose meta.toml it cannot read, rather than set a licence a restart would lose" $
            withUpload $ \manager home -> do
                writeFile (home </> "meta.toml") "not a meta file"
                setUploadLicence manager "upload" refused
                    `shouldReturn` Left (LicenceUnrecordable ("No readable meta.toml under " <> T.pack home <> ": the licence of upload would be lost at the next restart"))
                databaseLicence manager "upload" `shouldReturn` Just LicenceUnstated

        it "refuses a name the engine does not know" $
            withScratchDataDir $ do
                manager <- initDatabaseManager defaultConfig NoCache
                setUploadLicence manager "nothing" refused `shouldReturn` Left (LicenceUnknown "Database not found: nothing")

    describe "licenceRefusal" $ do
        it "lets a database with no licence be downloaded" $
            licenceRefusal Download "db" LicenceUnstated `shouldBe` Nothing

        it "refuses without quoting the own licence's text" $
            licenceRefusal Download "db" refused `shouldBe` Just "The licence of db does not allow downloading it."

    describe "standard licences" $ do
        it "read back from their identifier, in any case" $ do
            [parseSpdx (spdxId l) | l <- [minBound .. maxBound]] `shouldBe` map Right [minBound .. maxBound]
            parseSpdx "cc-by-4.0" `shouldBe` Right CCBY

        it "are listed once each" $
            standardLicences `shouldBe` nub standardLicences

        it "all grant downloading: refusing it takes an own licence" $
            all (`granted` Download) standardLicences `shouldBe` True

        it "read CC BY-NC as refusing resale and paid applications, not published results" $
            map (granted (LicenceStandard CCBYNC)) [Resell, PaidApplications, PublishResults] `shouldBe` [False, False, True]

    describe "licenceView" $
        it "says the engine enforces the inventory, the detailed scores and downloading" $
            [psPermission p | p <- lvPermissions (licenceView refused), psEnforced p] `shouldBe` [ReadInventory, SeeDetailedScores, Download]

    describe "resourceNeeds" $ do
        it "asks the inventory of an operation answering with exchange amounts" $
            map resourceNeeds [GetInventory, GetSupplyChain, GetConsumers, CompareActivities] `shouldBe` replicate 4 (Just ReadInventory)

        it "asks the detailed scores of an operation answering with what weighs in a score" $
            map resourceNeeds [GetContributingFlows, GetPathTo, CompareImpacts, ComputeSensitivity] `shouldBe` replicate 4 (Just SeeDetailedScores)

        it "asks nothing of an operation that trims its answer instead" $
            map resourceNeeds [GetActivity, GetImpacts, ScoreActivity, ScoreActivities] `shouldBe` replicate 4 Nothing

    describe "the wire" $ do
        it "reads back every kind of licence it writes" $
            mapM_ (\l -> A.decode (A.encode l) `shouldBe` Just l) (LicenceUnstated : refused : standardLicences)

        it "refuses a standard licence sent with refusals its text does not make" $
            (A.decode "{\"kind\":\"standard\",\"id\":\"CC-BY-4.0\",\"refused\":[\"download\"]}" :: Maybe Licence) `shouldBe` Nothing

        it "refuses a standard licence sent with an attribution or a text of its own" $
            mapM_
                (\body -> (A.decode body :: Maybe Licence) `shouldBe` Nothing)
                [ "{\"kind\":\"standard\",\"id\":\"CC0-1.0\",\"attribution\":true}"
                , "{\"kind\":\"standard\",\"id\":\"CC0-1.0\",\"text\":\"Ours\"}"
                ]

    describe "licenceFromKeys" $ do
        it "names the admitted identifiers when it cannot read one" $
            licenceFromKeys keys{lkId = Just "CC BY 4.0"} `shouldSatisfy` either ("CC-BY-4.0" `T.isInfixOf`) (const False)

        it "refuses a permission it cannot read, rather than read it as granted" $
            licenceFromKeys keys{lkText = Just "Ours", lkRefuses = Just ["downlaod"]} `shouldSatisfy` isLeft

        it "refuses a standard licence adjusted with an own licence's keys" $
            licenceFromKeys keys{lkId = Just "CC-BY-4.0", lkRefuses = Just ["download"]} `shouldSatisfy` isLeft

        it "refuses refusals with no text to belong to" $
            licenceFromKeys keys{lkRefuses = Just ["download"]} `shouldSatisfy` isLeft

        it "refuses an own licence with an empty text" $
            licenceFromKeys keys{lkText = Just "  "} `shouldSatisfy` isLeft

        it "refuses keeping a level to itself while granting what rebuilds it" $ do
            licenceFromKeys keys{lkText = Just "Ours", lkRefuses = Just ["scores"]}
                `shouldBe` Left "refusing scores refuses inventory and download too, since they rebuild it"
            licenceFromKeys keys{lkText = Just "Ours", lkRefuses = Just ["inventory"]}
                `shouldBe` Left "refusing inventory refuses download too, since they rebuild it"
            licenceFromKeys keys{lkText = Just "Ours", lkRefuses = Just ["scores", "inventory"]} `shouldSatisfy` isLeft
            (A.decode "{\"kind\":\"own\",\"text\":\"Ours\",\"refused\":[\"inventory\"],\"attribution\":true}" :: Maybe Licence) `shouldBe` Nothing

        it "reads an own licence refusing every level below the one it keeps" $
            licenceFromKeys keys{lkText = Just "Ours", lkRefuses = Just ["scores", "inventory", "download"]} `shouldSatisfy` (not . isLeft)

        it "reads an own licence silent on attribution as requiring it" $
            licenceFromKeys keys{lkText = Just "Ours", lkRefuses = Just ["download", "resell"]}
                `shouldBe` Right (LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.fromList [Download, Resell], ownAttribution = AttributionRequired})
  where
    isLeft :: Either a b -> Bool
    isLeft = either (const True) (const False)

{-# LANGUAGE OverloadedStrings #-}

module UploadedDatabaseSpec (spec) where

import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import qualified Data.Set as S
import Database.UploadedDatabase
import Types (AllocationKey (..), Attribution (..), Licence (..), OwnLicence (..), Permission (..), Release (..), StandardLicence (..))

-- | Minimal UploadMeta without description
baseMeta :: UploadMeta
baseMeta =
    UploadMeta
        { umVersion = 1
        , umDisplayName = "My Database"
        , umDescription = Nothing
        , umFormat = EcoSpold2
        , umDataPath = "data"
        , umDepends = []
        , umSource = Nothing
        , umAllocation = Declared
        , umBuiltIn = Nothing
        , umLicence = LicenceUnstated
        , umRelease = Nothing
        }

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- parseFormat
    -- -----------------------------------------------------------------------
    describe "parseFormat" $ do
        it "parses ecospold2" $ parseFormat "ecospold2" `shouldBe` Just EcoSpold2
        it "parses ecospold1" $ parseFormat "ecospold1" `shouldBe` Just EcoSpold1
        it "parses simapro" $ parseFormat "simapro" `shouldBe` Just SimaProCSV
        it "parses ilcd" $ parseFormat "ilcd" `shouldBe` Just ILCDProcess
        it "parses openlca-jsonld" $ parseFormat "openlca-jsonld" `shouldBe` Just OpenLcaJsonLd
        it "parses unknown" $ parseFormat "other" `shouldBe` Just UnknownFormat

    -- -----------------------------------------------------------------------
    -- isUploadedPath
    -- -----------------------------------------------------------------------
    describe "isUploadedPath" $ do
        it "returns True when path contains 'uploads'" $
            isUploadedPath "/data/uploads/databases/mydb" `shouldBe` True

        it "returns False when path does not contain 'uploads'" $
            isUploadedPath "/data/local/databases/mydb" `shouldBe` False

        it "returns False for empty path" $
            isUploadedPath "" `shouldBe` False

    -- -----------------------------------------------------------------------
    -- formatMetaToml
    -- -----------------------------------------------------------------------
    describe "formatMetaToml" $ do
        it "includes version field" $
            formatMetaToml baseMeta `shouldSatisfy` ("version = 1" `T.isInfixOf`)

        it "includes displayName" $
            formatMetaToml baseMeta `shouldSatisfy` ("My Database" `T.isInfixOf`)

        it "includes format field for ecospold2" $
            formatMetaToml baseMeta `shouldSatisfy` ("ecospold2" `T.isInfixOf`)

        it "includes dataPath field" $
            formatMetaToml baseMeta `shouldSatisfy` ("dataPath" `T.isInfixOf`)

        it "omits description when Nothing" $
            formatMetaToml baseMeta `shouldSatisfy` (not . ("description" `T.isInfixOf`))

        it "includes description when Just" $
            let meta = baseMeta{umDescription = Just "My description"}
             in formatMetaToml meta `shouldSatisfy` ("My description" `T.isInfixOf`)

        it "formats simapro as 'simapro'" $
            let meta = baseMeta{umFormat = SimaProCSV}
             in formatMetaToml meta `shouldSatisfy` ("simapro" `T.isInfixOf`)

        it "formats ilcd as 'ilcd'" $
            let meta = baseMeta{umFormat = ILCDProcess}
             in formatMetaToml meta `shouldSatisfy` ("ilcd" `T.isInfixOf`)

    -- -----------------------------------------------------------------------
    -- parseMetaToml
    -- -----------------------------------------------------------------------
    describe "parseMetaToml" $ do
        it "parses minimal meta without description" $ do
            let toml = "version = 1\ndisplayName = \"My DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\n"
            parseMetaToml toml
                `shouldBe` Just
                    UploadMeta
                        { umVersion = 1
                        , umDisplayName = "My DB"
                        , umDescription = Nothing
                        , umFormat = EcoSpold2
                        , umDataPath = "data"
                        , umDepends = []
                        , umSource = Nothing
                        , umAllocation = Declared
                        , umBuiltIn = Nothing
                        , umLicence = LicenceUnstated
                        , umRelease = Nothing
                        }

        it "parses meta with description" $ do
            let toml = "version = 1\ndisplayName = \"DB\"\ndescription = \"Desc\"\nformat = \"simapro\"\ndataPath = \"d\"\n"
            fmap umDescription (parseMetaToml toml) `shouldBe` Just (Just "Desc")

        it "returns Nothing for missing required field" $
            parseMetaToml "displayName = \"x\"\n" `shouldBe` Nothing

        it "ignores comment lines" $ do
            let toml = "# This is a comment\nversion = 1\ndisplayName = \"DB\"\nformat = \"ilcd\"\ndataPath = \"d\"\n"
            fmap umFormat (parseMetaToml toml) `shouldBe` Just ILCDProcess

    -- -----------------------------------------------------------------------
    -- Round-trip: formatMetaToml → parseMetaToml
    -- -----------------------------------------------------------------------
    describe "formatMetaToml / parseMetaToml roundtrip" $ do
        it "round-trips a meta without description" $
            parseMetaToml (formatMetaToml baseMeta) `shouldBe` Just baseMeta

        it "round-trips a meta with description" $ do
            let meta = baseMeta{umDescription = Just "A nice database"}
            parseMetaToml (formatMetaToml meta) `shouldBe` Just meta

        it "round-trips a path whose separators are backslashes" $ do
            -- A copy records the absolute path of the files it reads, and on
            -- Windows every separator in it is a backslash. The writer doubles
            -- them as TOML requires, so a reader that took the value verbatim
            -- handed back a path that resolves to nothing.
            let meta = baseMeta{umDataPath = "C:\\volca\\uploads\\bafu\\data", umSource = Just "bafu"}
            parseMetaToml (formatMetaToml meta) `shouldBe` Just meta

        it "round-trips a description holding a quote" $ do
            let meta = baseMeta{umDescription = Just "the \"good\" one"}
            parseMetaToml (formatMetaToml meta) `shouldBe` Just meta

        it "round-trips all database formats" $
            mapM_
                ( \fmt -> do
                    let meta = baseMeta{umFormat = fmt}
                    fmap umFormat (parseMetaToml (formatMetaToml meta)) `shouldBe` Just fmt
                )
                [EcoSpold2, EcoSpold1, SimaProCSV, ILCDProcess, OpenLcaJsonLd, UnknownFormat]

        it "round-trips a path with spaces" $ do
            let meta = baseMeta{umDataPath = "my data/sub dir"}
            fmap umDataPath (parseMetaToml (formatMetaToml meta)) `shouldBe` Just "my data/sub dir"

        it "round-trips the dependency pin" $ do
            -- The only durable record of which databases this one draws
            -- suppliers from; it otherwise lived in the staging registry and
            -- the binary cache, so a restart lost it without saying so.
            let meta = baseMeta{umDepends = ["agribalyse", "ecoinvent"]}
            fmap umDepends (parseMetaToml (formatMetaToml meta)) `shouldBe` Just ["agribalyse", "ecoinvent"]

        it "round-trips an own licence, quotes and all" $ do
            let licence = LicenceOwn OwnLicence{ownText = "Licensed to \"members\" only", ownRefused = S.fromList [Download, Resell], ownAttribution = AttributionNotRequired}
            fmap umLicence (parseMetaToml (formatMetaToml baseMeta{umLicence = licence})) `shouldBe` Just licence

        it "round-trips an own licence ending in a quote" $ do
            let licence = LicenceOwn OwnLicence{ownText = "Licensed to \"members\"", ownRefused = S.empty, ownAttribution = AttributionRequired}
            fmap umLicence (parseMetaToml (formatMetaToml baseMeta{umLicence = licence})) `shouldBe` Just licence

        it "round-trips a standard licence" $
            fmap umLicence (parseMetaToml (formatMetaToml baseMeta{umLicence = LicenceStandard ODbL})) `shouldBe` Just (LicenceStandard ODbL)

        it "round-trips a release, system model and all" $ do
            let release = Release "ecoinvent" "3.12" (Just "Allocation, cut-off by classification")
            fmap umRelease (parseMetaToml (formatMetaToml baseMeta{umRelease = Just release})) `shouldBe` Just (Just release)

        it "round-trips a release without a system model" $
            fmap umRelease (parseMetaToml (formatMetaToml baseMeta{umRelease = Just (Release "Agribalyse" "3.2" Nothing)})) `shouldBe` Just (Just (Release "Agribalyse" "3.2" Nothing))

        it "reads a file written before the release existed as declaring none" $ do
            let toml = "version = 6\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\n"
            fmap umRelease (parseMetaToml toml) `shouldBe` Just Nothing

        it "refuses a release missing its version rather than read half of one" $ do
            -- Read as a release, a name alone would match every version of it.
            let toml = "version = 7\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\nrelease_name = \"ecoinvent\"\n"
            parseMetaToml toml `shouldBe` Nothing

        it "reads a file written before the licence existed as having none" $ do
            let toml = "version = 4\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\n"
            fmap umLicence (parseMetaToml toml) `shouldBe` Just LicenceUnstated

        -- Ignored, the switch these keys replaced would read a refusal as no licence at all.
        it "refuses a file still carrying the downloads switch" $ do
            let toml = "version = 5\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\nlicence = \"Members only\"\ndownloads = \"refused\"\n"
            parseMetaToml toml `shouldBe` Nothing

        it "refuses a file with a refusal it cannot read, rather than skip it" $ do
            let toml = "version = 6\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\nlicence_text = \"Ours\"\nrefuses = [\"download\", resell]\n"
            parseMetaToml toml `shouldBe` Nothing

        it "refuses a file whose attribution is neither true nor false" $ do
            let toml = "version = 6\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\nlicence_text = \"Ours\"\nattribution = yes\n"
            parseMetaToml toml `shouldBe` Nothing

        it "reads a file written before the dependency pin existed as pinning nothing" $ do
            let toml = "version = 1\ndisplayName = \"DB\"\nformat = \"ecospold2\"\ndataPath = \"data\"\n"
            fmap umDepends (parseMetaToml toml) `shouldBe` Just []

    -- -----------------------------------------------------------------------
    -- readUploadMeta / writeUploadMeta (IO roundtrip)
    -- -----------------------------------------------------------------------
    describe "readUploadMeta / writeUploadMeta" $ do
        it "returns Nothing for a directory without meta.toml" $
            withSystemTempDirectory "volca-test" $ \dir -> do
                result <- readUploadMeta dir
                result `shouldBe` Nothing

        it "round-trips write then read" $
            withSystemTempDirectory "volca-test" $ \dir -> do
                writeUploadMeta dir baseMeta
                result <- readUploadMeta dir
                result `shouldBe` Just baseMeta

        it "round-trips with description" $
            withSystemTempDirectory "volca-test" $ \dir -> do
                let meta = baseMeta{umDescription = Just "test"}
                writeUploadMeta dir meta
                result <- readUploadMeta dir
                result `shouldBe` Just meta

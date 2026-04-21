{-# LANGUAGE OverloadedStrings #-}

module UploadedDatabaseSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Upload (DatabaseFormat (..))
import Database.UploadedDatabase

-- | Minimal UploadMeta without description
baseMeta :: UploadMeta
baseMeta =
    UploadMeta
        { umVersion = 1
        , umDisplayName = "My Database"
        , umDescription = Nothing
        , umFormat = EcoSpold2
        , umDataPath = "data"
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

        it "round-trips all database formats" $
            mapM_
                ( \fmt -> do
                    let meta = baseMeta{umFormat = fmt}
                    fmap umFormat (parseMetaToml (formatMetaToml meta)) `shouldBe` Just fmt
                )
                [EcoSpold2, EcoSpold1, SimaProCSV, ILCDProcess, UnknownFormat]

        it "round-trips a path with spaces" $ do
            let meta = baseMeta{umDataPath = "my data/sub dir"}
            fmap umDataPath (parseMetaToml (formatMetaToml meta)) `shouldBe` Just "my data/sub dir"

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

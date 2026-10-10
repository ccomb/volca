{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading an openLCA package's documents, before anything is computed.
module OlcaSchemaPackageSpec (spec) where

import Control.Monad ((>=>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import OlcaPackageFixture
import OlcaSchema.Package

withPackage :: (FilePath -> IO a) -> IO a
withPackage act = withSystemTempDirectory "olca-package" $ \dir -> writePackage dir >> act dir

readFixture :: IO Package
readFixture = withPackage (readPackage >=> either (fail . T.unpack) pure)

processNamed :: Package -> T.Text -> Maybe Process
processNamed pkg name = case filter ((== name) . prName) (pkProcesses pkg) of
    [p] -> Just p
    _ -> Nothing

isInput :: ParameterValue -> Bool
isInput = \case
    InputValue _ -> True
    Calculated _ _ -> False

spec :: Spec
spec = describe "readPackage" $ do
    it "knows a package by its openlca.json" $
        withPackage isOlcaPackage `shouldReturn` True

    it "reads every document of the fixture" $ do
        pkg <- readFixture
        M.size (pkUnitGroups pkg) `shouldBe` 2
        M.size (pkFlows pkg) `shouldBe` 16
        length (pkProcesses pkg) `shouldBe` 13
        map paName (pkGlobals pkg) `shouldBe` ["leak_method"]

    it "reads a line with no isInput as an output, and one with no isAvoidedProduct as not avoided" $ do
        pkg <- readFixture
        fmap (map rxSide . prExchanges) (processNamed pkg "steel recycling")
            `shouldBe` Just [Produced, Consumed, Avoided, Produced]

    it "reads the unit factors and the flow's factor for another property" $ do
        pkg <- readFixture
        fmap ueFactor (M.lookup kwhU . ugUnits =<< M.lookup energyG (pkUnitGroups pkg)) `shouldBe` Just 3.6
        (M.lookup energyP . flFactors =<< M.lookup gasF (pkFlows pkg)) `shouldBe` Just 50

    it "reads a parameter with no isInputParameter as an input parameter" $
        withPackage $ \dir -> do
            writeFile (dir </> "parameters" </> "bare.json") "{\"name\": \"bare\", \"value\": 4}"
            r <- readPackage dir
            fmap (map (isInput . paValue) . filter ((== "bare") . paName) . pkGlobals) r `shouldBe` Right [True]

    it "keeps causal factors on the internal id of the line they name" $ do
        pkg <- readFixture
        fmap (map afExchange . prFactors) (processNamed pkg "cogeneration, causal") `shouldBe` Just [Just 1, Just 1, Just 7]

    it "refuses a malformed document, naming its file" $
        withPackage $ \dir -> do
            writeFile (dir </> "flows" </> "broken.json") "{\"@id\": 3}"
            readPackage dir >>= \case
                Left why -> T.unpack why `shouldContain` "broken.json"
                Right _ -> expectationFailure "read a malformed flow"

    it "reads a version 2 package, whose documents read as version 3's" $
        withPackage $ \dir -> do
            writeFile (dir </> "openlca.json") "{\"schemaVersion\": 2}"
            fmap (fmap (length . pkProcesses)) (readPackage dir) `shouldReturn` Right 13

    it "refuses a schema version it does not know" $
        withPackage $ \dir -> do
            writeFile (dir </> "openlca.json") "{\"schemaVersion\": 7}"
            readPackage dir >>= \case
                Left why -> T.unpack why `shouldContain` "schema version 7"
                Right _ -> expectationFailure "read a version 7 package"

    it "refuses two documents of one kind with the same identifier, naming it" $
        withPackage $ \dir -> do
            copyFile (dir </> "flows" </> (UUID.toString co2F <> ".json")) (dir </> "flows" </> "copy.json")
            readPackage dir >>= \case
                Left why -> T.unpack why `shouldContain` UUID.toString co2F
                Right _ -> expectationFailure "read two flows with one identifier"

    it "skips a file that is not JSON, as packages ship some" $
        withPackage $ \dir -> do
            writeFile (dir </> "unit_groups" </> "notes.txt") "not a document"
            fmap (fmap (M.size . pkUnitGroups)) (readPackage dir) `shouldReturn` Right 2

{-# LANGUAGE OverloadedStrings #-}

module ManagerBuiltinSpec (spec) where

import Config (Config (..), MethodConfig, defaultConfig)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Database.Manager (CachePolicy (..), DatabaseLoadStatus (..), DatabaseManager (..), MethodCollectionStatus (..), initDatabaseManager, listMethodCollections)
import Method.ParserCSV (parseMethodCSVBytes)
import Method.Types (Method (..), MethodCollection (..))
import TOML (getArrayOf, getFieldWith)
import qualified TOML
import Test.Hspec

spec :: Spec
spec = describe "initDatabaseManager with the built-in defaults" $ do
    it "loads the four built-in tables and the geographies without a file in sight" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        comps <- readTVarIO (dmLoadedCompMaps manager)
        units <- readTVarIO (dmLoadedUnitDefs manager)
        syns <- readTVarIO (dmLoadedFlowSyns manager)
        eds <- readTVarIO (dmLoadedEnergyDensities manager)
        -- Membership, not the exact key set: startup also picks up whatever
        -- sits under uploads/<kind>/ in the working directory.
        comps `shouldSatisfy` M.member "Default compartment mapping"
        units `shouldSatisfy` M.member "Default units"
        syns `shouldSatisfy` M.member "Default flow synonyms"
        eds `shouldSatisfy` M.member "Default energy densities"
        M.size (dmGeographies manager) `shouldSatisfy` (> 500)

    it "loads the built-in method with every category its file declares" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        loaded <- readTVarIO (dmLoadedMethods manager)
        onDisk <- parseMethodCSVBytes <$> BS.readFile "data/methods/plain-indicators.csv"
        case onDisk of
            Left err -> expectationFailure err
            Right methods ->
                fmap (map methodName . mcMethods) (M.lookup "plain-indicators" loaded)
                    `shouldBe` Just (map methodName methods)

    -- A pathless entry carries no format and no description of its own, so
    -- the listing reads both from what it names, not from a path it lacks.
    it "lists a switched-off built-in method as unloaded, with its format and description" $ do
        case decodeMethods "[[methods]]\nname = \"plain-indicators\"\nactive = false\n" of
            Left err -> expectationFailure (show err)
            Right methods -> do
                manager <- initDatabaseManager defaultConfig{cfgMethods = methods} NoCache
                listed <- filter ((== "plain-indicators") . mcsName) <$> listMethodCollections manager
                map (\m -> (mcsStatus m, mcsPath m, mcsFormat m, mcsDescription m)) listed
                    `shouldBe` [(Unloaded, "built-in", "Columnar CSV", Just "Raw physical quantities counted through the supply chain (CF=1.0)")]
  where
    decodeMethods :: Text -> Either TOML.TOMLError [MethodConfig]
    decodeMethods = TOML.decodeWith (getFieldWith (getArrayOf TOML.tomlDecoder) "methods")

{-# LANGUAGE OverloadedStrings #-}

module DocumentFileSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager, addDatabase, initDatabaseManager, loadDatabase, readDocumentFile)
import Types (AllocationKey (..), GeographyPolicy (..))

ilcdConfig :: DatabaseConfig
ilcdConfig =
    DatabaseConfig
        { dcName = "ilcd"
        , dcDisplayName = "ilcd"
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
        }

loadedManager :: IO DatabaseManager
loadedManager = do
    manager <- initDatabaseManager defaultConfig NoCache
    addDatabase manager ilcdConfig
    loadDatabase manager "ilcd" >>= either (fail . T.unpack) (const (pure ()))
    pure manager

spec :: Spec
spec = describe "readDocumentFile" $ do
    it "serves a file a literature entry lists" $ do
        manager <- loadedManager
        expected <- BS.readFile "test-data/SAMPLE.ilcd/external_docs/coal report.pdf"
        readDocumentFile manager "ilcd" "external_docs/coal report.pdf" `shouldReturn` Right expected

    it "refuses a file of the package the documentation does not list" $ do
        manager <- loadedManager
        readDocumentFile manager "ilcd" "processes/mean-amount.xml"
            `shouldReturn` Left "The documentation of ilcd lists no file processes/mean-amount.xml"

    it "refuses a database that is not loaded" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        readDocumentFile manager "ilcd" "external_docs/coal report.pdf"
            `shouldReturn` Left "Database not loaded: ilcd"

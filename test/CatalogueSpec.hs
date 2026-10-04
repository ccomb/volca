{-# LANGUAGE OverloadedStrings #-}

{- | The catalogue a client indexes a database from: one entry per process,
each unit read against the table or left unread, a fingerprint that moves
exactly when an entry does, and pages that refuse to be asked out of range.
-}
module CatalogueSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Servant (ServerError (..), runHandler)
import Test.Hspec

import API.Routes (getCatalogue, getCatalogueFingerprint, getClassifications)
import API.Types (CatalogueEntry (..), CatalogueFingerprint (..), CatalogueMeasure (..), CataloguePage (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (DatabaseConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager (..), LoadedDatabase (..), initDatabaseManager)
import Service.Catalogue (PageWindow (..), catalogueEntries, catalogueFingerprint, catalogueMaxLimit, cataloguePage, measureOf)
import TestHelpers (loadSampleDatabase, mkSolverFromDb)
import Types (Activity (..), AllocationKey (..), Database (..), GeographyPolicy (..), Licence (..))
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

-- | A tonne beside the kilogram, and a piece that counts rather than weighs.
units :: Either T.Text UnitConfig
units = buildFromCSV "name,dimension,factor\nkg,mass,1.0\nt,mass,1000.0\np,count,1.0\n"

spec :: Spec
spec = do
    describe "measureOf" $ do
        it "puts a tonne and a kilogram on one dimension, a thousand apart" $ do
            let kg = either (const Nothing) (`measureOf` "kg") units
                t = either (const Nothing) (`measureOf` "t") units
            fmap cmDimension t `shouldBe` fmap cmDimension kg
            ((/) <$> fmap cmFactor t <*> fmap cmFactor kg) `shouldBe` Just 1000
        it "keeps a piece apart from a kilogram" $ do
            let dimensionOf u = either (const Nothing) (fmap cmDimension . (`measureOf` u)) units
            dimensionOf "p" `shouldSatisfy` (/= Nothing)
            dimensionOf "p" `shouldNotBe` dimensionOf "kg"
        it "leaves a unit the table does not know unread" $
            measureOf defaultUnitConfig "blorp" `shouldBe` Nothing

    describe "catalogueEntries" $ do
        it "lists every process of the database, none dropped" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            length (catalogueEntries defaultUnitConfig db) `shouldBe` V.length (dbActivities db)
        it "names each entry by the id the other routes answer to" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            any (T.isPrefixOf "invalid-process-id" . ceProcessId) (catalogueEntries defaultUnitConfig db)
                `shouldBe` False

    describe "catalogueFingerprint" $ do
        it "is the same for the same catalogue" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let entries = catalogueEntries defaultUnitConfig db
            catalogueFingerprint entries `shouldBe` catalogueFingerprint entries
        it "moves when one process is renamed" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let rename i a = if i == 0 then a{activityName = activityName a <> " (renamed)"} else a
                renamed = db{dbActivities = V.imap rename (dbActivities db)}
            catalogueFingerprint (catalogueEntries defaultUnitConfig renamed)
                `shouldNotBe` catalogueFingerprint (catalogueEntries defaultUnitConfig db)

    describe "cataloguePage" $ do
        it "carries the fingerprint and the total on every page" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let entries = catalogueEntries defaultUnitConfig db
            fmap (\p -> (cpFingerprint p, cpTotal p, length (cpEntries p))) (cataloguePage entries PageWindow{pwOffset = 1, pwLimit = 1})
                `shouldBe` Right (catalogueFingerprint entries, length entries, 1)
        it "refuses a negative offset, a zero limit and one past the maximum" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let entries = catalogueEntries defaultUnitConfig db
            cataloguePage entries PageWindow{pwOffset = -1, pwLimit = 10} `shouldSatisfy` isLeft
            cataloguePage entries PageWindow{pwOffset = 0, pwLimit = 0} `shouldSatisfy` isLeft
            cataloguePage entries PageWindow{pwOffset = 0, pwLimit = catalogueMaxLimit + 1} `shouldSatisfy` isLeft
        it "answers an offset past the end with an empty page that still says the total" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let entries = catalogueEntries defaultUnitConfig db
            fmap (\p -> (cpTotal p, cpEntries p)) (cataloguePage entries PageWindow{pwOffset = length entries, pwLimit = 10})
                `shouldBe` Right (length entries, [])

    describe "the catalogue routes" $ do
        it "answers the same fingerprint alone and on a page" $ do
            manager <- loadedSample
            page <- runRest manager (getCatalogue sampleName Nothing Nothing)
            fp <- runRest manager (getCatalogueFingerprint sampleName)
            either (const Nothing) (Just . cfFingerprint) fp `shouldBe` either (const Nothing) (Just . cpFingerprint) page
            statusOf page `shouldBe` 200
        it "refuses a limit past the maximum with a 400" $ do
            manager <- loadedSample
            result <- runRest manager (getCatalogue sampleName Nothing (Just (catalogueMaxLimit + 1)))
            statusOf result `shouldBe` 400
        it "answers an unknown database as the other database routes do" $ do
            manager <- loadedSample
            ours <- runRest manager (getCatalogue "no-such-db" Nothing Nothing)
            theirs <- runRest manager (getClassifications "no-such-db")
            statusOf ours `shouldBe` statusOf theirs

statusOf :: Either ServerError a -> Int
statusOf = either errHTTPCode (const 200)

sampleName :: Text
sampleName = "sample"

loadedSample :: IO DatabaseManager
loadedSample = do
    manager <- initDatabaseManager defaultConfig NoCache
    db <- loadSampleDatabase "SAMPLE.min3"
    solver <- mkSolverFromDb db sampleName
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = sampleConfig}
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert sampleName loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert sampleName sampleConfig)
    pure manager

runRest :: DatabaseManager -> AppM a -> IO (Either ServerError a)
runRest manager = runHandler . runApp env
  where
    env :: AppEnv
    env =
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            }

sampleConfig :: DatabaseConfig
sampleConfig =
    DatabaseConfig
        { dcName = sampleName
        , dcDisplayName = sampleName
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = Nothing
        }

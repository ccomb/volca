{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tests for making an edit outlive the process
('Database.Edit.mutateUploadedDatabase' and "Database.Journal").

Editing used to be transient by design: the sources and the matrix cache still
held the pre-edit database, so an unload and reload deliberately undid the
change. That is defensible for a database the engine reads from configuration
and does not own, and wrong for one it does.

What replaced it is not a rewrite of the sources, which only a format that
records process identity could survive. The sources are left exactly as their
author uploaded them and the edits are recorded beside them, so what follows
uses an EcoSpold 1 database on purpose: it is the format whose identities are
derived from the position of a dataset in its file, and the one a rewrite
would move.
-}
module PersistenceSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (bracket_)
import Control.Monad (void)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Edit (
    DeleteOutcome (..),
    DeleteRequest (..),
    MutationOutcome (..),
    copyDatabase,
    deleteActivitiesInDB,
    mutateUploadedDatabase,
 )
import Database.Journal (JournalOp (..), journalPath, readJournal)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    addDatabase,
    initDatabaseManager,
    loadDatabase,
    removeDatabase,
    unloadDatabase,
 )
import Database.Rebuild (renderKey)
import Database.Upload (DatabaseFormat (..))
import Database.UploadedDatabase (UploadMeta (..), readUploadMeta)
import SharedSolver (SharedSolver, createSharedSolver)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
    Medium (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "persisting an edit" $ do
    describe "an uploaded database" $ do
        it "records the edit beside its sources, and leaves the sources alone" $
            withEcoSpold1Database $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                outcome <- deleteFirst manager "bafu-like" keys
                doPersisted outcome `shouldBe` True
                -- What the author uploaded is what is still there: the edit is
                -- a line beside the files, not a rewrite of them.
                listDirectory (home </> "data") >>= (`shouldSatisfy` ((== 2) . length))
                journal <- readJournal home
                fmap length journal `shouldBe` Right 1

        it "reloads to what was written, under the identities it was written with" $
            -- The point of the whole path. EcoSpold 1 derives its identities
            -- from dataset and exchange numbers, so a writer that renumbered
            -- them would give the surviving activity a different process id
            -- and every reference anyone kept would point elsewhere.
            withEcoSpold1Database $ \manager _ -> do
                keys <- processKeysOf manager "bafu-like"
                length keys `shouldBe` 2
                _ <- deleteFirst manager "bafu-like" keys
                reloadOf manager "bafu-like" `shouldReturn` drop 1 keys

        it "replays the journal when the cache cannot say that it holds it" $
            -- A crash between recording an edit and saving the cache leaves a
            -- cache that predates the journal. Only the stamp says otherwise,
            -- so without it the sources are read again and the journal
            -- replayed over them.
            withEcoSpold1Database $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                _ <- deleteFirst manager "bafu-like" keys
                removeFile (home </> "journal.applied")
                reloadOf manager "bafu-like" `shouldReturn` drop 1 keys

        it "refuses to load at all when the journal names something it cannot apply" $
            -- Half-applying it would hand back a database that silently
            -- disagrees with its own record of what was done to it.
            withEcoSpold1Database $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                _ <- deleteFirst manager "bafu-like" keys
                TIO.appendFile (journalPath home) (deleteLineFor (head' keys) <> "\n")
                removeFile (home </> "journal.applied")
                unloadDatabase manager "bafu-like" `shouldReturn` Right ()
                reloaded <- loadDatabase manager "bafu-like"
                void reloaded `shouldSatisfy` failsWith "Unknown process id"

    describe "a copy" $ do
        it "gets a home of its own at once, holding no data" $
            -- A copy shares the source's value and reads the source's files;
            -- what it owns is a directory with its own identity and, once it
            -- is edited, its own journal.
            withEcoSpold1Database $ \manager home -> do
                copyDatabase manager "bafu-like" "bafu-mine" `shouldReturn` Right ()
                let uploads = uploadsDirOf home
                meta <- readUploadMeta (uploads </> "bafu-mine")
                fmap umSource meta `shouldBe` Just (Just "bafu-like")
                fmap umDataPath meta `shouldBe` Just (home </> "data")
                listDirectory (uploads </> "bafu-mine") `shouldReturn` ["meta.toml"]

        it "forks from the source as it stands, not as it was uploaded" $
            -- What was copied is the source's value after its edits, but the
            -- source's files never carry them: the copy starts from a snapshot
            -- of the source's journal, or a reload would quietly resurrect
            -- everything the source had removed before the copy was made.
            withEcoSpold1Database $ \manager _ -> do
                keys <- processKeysOf manager "bafu-like"
                _ <- deleteFirst manager "bafu-like" keys
                copyDatabase manager "bafu-like" "bafu-mine" `shouldReturn` Right ()
                reloadOf manager "bafu-mine" `shouldReturn` drop 1 keys

        it "keeps the database it was copied from from being deleted" $
            withEcoSpold1Database $ \manager home -> do
                copyDatabase manager "bafu-like" "bafu-mine" `shouldReturn` Right ()
                unloadDatabase manager "bafu-like" `shouldReturn` Right ()
                let uploads = uploadsDirOf home
                removed <- removeDatabase manager "bafu-like"
                removed `shouldSatisfy` failsWith "copied from it"
                listDirectory (uploads </> "bafu-like") >>= (`shouldSatisfy` elem "data")

    describe "a database the engine only reads" $ do
        it "says the edit is not saved, and is given no home" $
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig NoCache
                db <- buildTwoActivityFixture
                install manager "configured" db (configuredConfig "configured")
                r <- mutateUploadedDatabase manager "configured" dropSecond
                case r of
                    Left err -> expectationFailure ("mutateUploadedDatabase: " <> show err)
                    Right outcome -> do
                        moPersisted outcome `shouldBe` False
                        -- A configured database owns its files through the
                        -- config file, and the engine writes none of its own.
                        listDirectory (dataRoot </> "uploads" </> "databases") `shouldReturn` []

    describe "refusals that protect the value" $ do
        it "refuses a second edit while one is in progress" $
            withDataDir $ \_ -> do
                manager <- initDatabaseManager defaultConfig NoCache
                db <- buildTwoActivityFixture
                install manager "busy" db (configuredConfig "busy")
                atomically $ modifyTVar' (dmStagingDbs manager) (Set.insert "busy")
                r <- mutateUploadedDatabase manager "busy" dropSecond
                void r `shouldSatisfy` failsWith "already in progress"

        it "refuses while another loaded database depends on this one" $
            withDataDir $ \_ -> do
                manager <- initDatabaseManager defaultConfig NoCache
                db <- buildTwoActivityFixture
                install manager "background" db (configuredConfig "background")
                install manager "foreground" db{dbDependsOn = ["background"]} (configuredConfig "foreground")
                r <- mutateUploadedDatabase manager "background" dropSecond
                void r `shouldSatisfy` failsWith "still required by"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

failsWith :: Text -> Either Text a -> Bool
failsWith needle = either (isInfixOf needle) (const False)

{- | Point the engine's data directory at a scratch tree for the duration of
one example, so nothing is written next to the sources.
-}
withDataDir :: (FilePath -> IO ()) -> IO ()
withDataDir act =
    withSystemTempDirectory "volca-persist" $ \dir ->
        bracket_ (setEnv "VOLCA_DATA_DIR" dir) (unsetEnv "VOLCA_DATA_DIR") (act dir)

-- | The uploads directory a database's home sits in.
uploadsDirOf :: FilePath -> FilePath
uploadsDirOf = takeDirectory

{- | An uploaded EcoSpold 1 database of two datasets, loaded and ready to
edit. Each dataset is a file named after the identifier it carries, the layout
a published EcoSpold 1 collection uses.
-}
withEcoSpold1Database :: (DatabaseManager -> FilePath -> IO ()) -> IO ()
withEcoSpold1Database act =
    withDataDir $ \dataRoot -> do
        let home = dataRoot </> "uploads" </> "databases" </> "bafu-like"
            dataDir = home </> "data"
        createDirectoryIfMissing True dataDir
        TIO.writeFile (dataDir </> "process_" <> T.unpack (UUID.toText (mkUUID 101)) <> ".xml") (dataset 1 "electricity production, wind")
        TIO.writeFile (dataDir </> "process_" <> T.unpack (UUID.toText (mkUUID 102)) <> ".xml") (dataset 2 "electricity production, solar")
        manager <- initDatabaseManager defaultConfig UseCache
        addDatabase manager (uploadedConfig "bafu-like" dataDir)
        loaded <- loadDatabase manager "bafu-like"
        case loaded of
            Left err -> expectationFailure ("loading the fixture: " <> show err)
            Right _ -> act manager home

-- | One EcoSpold 1 dataset: a reference product and one emission.
dataset :: Int -> Text -> Text
dataset number name =
    T.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"" <> T.pack (show number) <> "\" generator=\"Test\" timestamp=\"2026-01-01T00:00:00\">"
        , "    <metaInformation><processInformation>"
        , "      <referenceFunction name=\"" <> name <> "\" category=\"Energy\" subCategory=\"Electricity\" unit=\"kWh\" />"
        , "      <geography location=\"DE\" />"
        , "    </processInformation></metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"" <> name <> ", output\" category=\"Energy\" subCategory=\"Electricity\" unit=\"kWh\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Carbon dioxide, fossil\" category=\"air\" subCategory=\"low population density\" unit=\"kg\" meanValue=\"0.01\">"
        , "        <outputGroup>4</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

-- | The process ids a loaded database currently holds, in table order.
processKeysOf :: DatabaseManager -> Text -> IO [Text]
processKeysOf manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    pure $ case M.lookup dbName loadedDbs of
        Nothing -> []
        Just loaded -> map renderKey (V.toList (dbProcessIdTable (ldDatabase loaded)))

-- | Delete the first process, through the request path a client uses.
deleteFirst :: DatabaseManager -> Text -> [Text] -> IO DeleteOutcome
deleteFirst manager dbName keys = do
    r <- deleteActivitiesInDB manager dbName (deleteIds [head' keys])
    either (fail . T.unpack) pure r

-- | Unload, load again, and say which processes came back.
reloadOf :: DatabaseManager -> Text -> IO [Text]
reloadOf manager dbName = do
    unloadDatabase manager dbName >>= either (fail . T.unpack) pure
    loadDatabase manager dbName >>= either (fail . T.unpack) (const (pure ()))
    processKeysOf manager dbName

deleteLineFor :: Text -> Text
deleteLineFor target =
    "{\"v\":1,\"at\":\"2026-08-03T00:00:00Z\",\"op\":\"delete\",\"targets\":[\"" <> target <> "\"]}"

head' :: [Text] -> Text
head' (x : _) = x
head' [] = "the fixture has no processes"

{- | Remove the second activity of the two-activity fixture: any edit will do
here, and a deletion is the one every database supports, so these examples pin
the mutation path itself rather than what happened to be edited.
-}
dropSecond :: JournalOp
dropSecond = Deleted [renderKey (otherActId, supplierProdId)]

deleteIds :: [Text] -> DeleteRequest
deleteIds ids =
    DeleteRequest
        { drName = Nothing
        , drLocation = Nothing
        , drProduct = Nothing
        , drClassifications = []
        , drExactName = False
        , drKeep = []
        , drExtra = []
        , drIds = Just ids
        }

install :: DatabaseManager -> Text -> Database -> DatabaseConfig -> IO ()
install manager name db config = do
    solver <- mkSolver name db
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = config}
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert name loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert name config)

mkSolver :: Text -> Database -> IO SharedSolver
mkSolver name db =
    let triples = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
     in createSharedSolver name triples (fromIntegral (dbActivityCount db))

baseConfig :: Text -> DatabaseConfig
baseConfig name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Just EcoSpold2
        , dcIsUploaded = False
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        }

uploadedConfig :: Text -> FilePath -> DatabaseConfig
uploadedConfig name dataDir =
    (baseConfig name){dcPath = dataDir, dcIsUploaded = True, dcFormat = Just EcoSpold1}

configuredConfig :: Text -> DatabaseConfig
configuredConfig = baseConfig

-- ---------------------------------------------------------------------------
-- In-memory fixture, for the refusals that never reach a file
-- ---------------------------------------------------------------------------

buildTwoActivityFixture :: IO Database
buildTwoActivityFixture =
    buildFrom $
        M.fromList
            [ ((supplierActId, supplierProdId), milkActivity "milk production")
            , ((otherActId, supplierProdId), milkActivity "milk production, organic")
            ]

buildFrom :: M.Map (UUID, UUID) Activity -> IO Database
buildFrom activities = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = activities
                , sdbTechFlows = M.singleton supplierProdId milkFlow
                , sdbBioFlows = M.singleton co2Id co2Flow
                , sdbWasteFlows = M.empty
                , sdbUnits = unitTable
                }
    either (fail . show) pure r

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

supplierActId, otherActId, supplierProdId, co2Id, kgUnitId :: UUID
supplierActId = mkUUID 1
otherActId = mkUUID 4
supplierProdId = mkUUID 2
co2Id = mkUUID 3
kgUnitId = mkUUID 10

unitTable :: M.Map UUID Unit
unitTable = M.singleton kgUnitId Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

milkFlow :: TechnosphereFlow
milkFlow =
    TechnosphereFlow
        { tfId = supplierProdId
        , tfName = "milk"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

air :: Compartment
air = Compartment{compartmentName = Air, compartmentSub = Nothing}

co2Flow :: BiosphereFlow
co2Flow =
    BiosphereFlow
        { bfId = co2Id
        , bfName = "Carbon dioxide"
        , bfUnitId = kgUnitId
        , bfSynonyms = M.empty
        , bfCAS = Just "124-38-9"
        , bfSubstanceId = Nothing
        , bfCompartment = Just air
        }

milkActivity :: Text -> Activity
milkActivity name =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = supplierProdId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = Just supplierActId
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , BiosphereExchange
                { bioFlowId = co2Id
                , bioAmount = 1.2
                , bioUnitId = kgUnitId
                , bioDirection = Emission
                , bioLocation = ""
                , bioComment = Nothing
                , bioPedigree = Nothing
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

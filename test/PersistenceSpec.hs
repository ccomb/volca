{-# LANGUAGE LambdaCase #-}
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
import Control.Monad (forM_, void)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Author (ExchangeEdit (..), ExchangeSelector (..))
import Database.Edit (
    DeleteOutcome (..),
    DeleteRequest (..),
    MissingRequest (..),
    MutationOutcome (..),
    copyDatabase,
    deleteActivitiesInDB,
    editActivities,
    mutateUploadedDatabase,
    refusalMessage,
    supplyMissing,
 )
import Database.Journal (Change (..), Entry (..), JournalOp (..), journalPath, readJournal)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    DatabaseSetupInfo (..),
    LoadedDatabase (..),
    MissingSupplier (..),
    addDatabase,
    getDatabaseSetupInfo,
    initDatabaseManager,
    loadDatabase,
    removeDatabase,
    unloadDatabase,
 )
import Database.Upload (DatabaseFormat (..))
import Database.UploadedDatabase (UploadMeta (..), readUploadMeta)
import SharedSolver (SharedSolver, createSharedSolver)
import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    FileReading (..),
    GeographyPolicy (..),
    Licence (..),
    LocationSource (..),
    Medium (..),
    ProcessRef (..),
    Reading (..),
    ReferenceConvention (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    noDates,
    noDocumentation,
    noProperties,
    processRefText,
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
                removed `shouldSatisfy` failsWith "made from it"
                listDirectory (uploads </> "bafu-like") >>= (`shouldSatisfy` elem "data")

    describe "deleting an uploaded database" $
        it "leaves no directory behind when its data sits in a subdirectory" $
            -- Its cache sits beside the data, inside the upload: deleting the
            -- cache after the upload must not make the directory again.
            withEcoSpold1Database $ \manager home -> do
                unloadDatabase manager "bafu-like" `shouldReturn` Right ()
                removeDatabase manager "bafu-like" `shouldReturn` Right ()
                doesDirectoryExist home `shouldReturn` False

    describe "a database the engine only reads" $ do
        it "says the edit is not saved, and is given no home" $
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig NoCache
                db <- buildTwoActivityFixture
                install manager "configured" db (configuredConfig "configured")
                r <- mutateUploadedDatabase manager "configured" Nothing (pure dropSecond)
                case r of
                    Left err -> expectationFailure ("mutateUploadedDatabase: " <> show err)
                    Right outcome -> do
                        moPersisted outcome `shouldBe` False
                        -- A configured database owns its files through the
                        -- config file, and the engine writes none of its own.
                        listDirectory (dataRoot </> "uploads" </> "databases") `shouldReturn` []

    describe "edits of several activities at once" $ do
        it "edits every activity named, one journal line each" $
            withEcoSpold1DatabaseOf threeDatasets $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                co2 <- co2Of manager
                outcome <- editActivities manager "bafu-like" Nothing Nothing (setCo2 co2 keys)
                void (first refusalMessage outcome) `shouldBe` Right ()
                emissionsBy manager "bafu-like" `shouldReturn` M.fromList (zip keys [[1], [2], [3]])
                journal <- readJournal home
                fmap (map (opTarget . changeOp . jeOp)) journal `shouldBe` Right (map Just keys)

        it "writes nothing when one of the edits is refused" $
            withEcoSpold1DatabaseOf threeDatasets $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                co2 <- co2Of manager
                before <- emissionsBy manager "bafu-like"
                let missing = SetAmount (SelectBiosphere (mkUUID 999)) 5
                    requests = case setCo2 co2 keys of
                        first' :| (second' : rest) -> first' :| ((fst second', [missing]) : rest)
                        whole -> whole
                outcome <- editActivities manager "bafu-like" Nothing Nothing requests
                void (first refusalMessage outcome) `shouldSatisfy` failsWith (keys !! 1)
                readJournal home `shouldReturn` Right []
                emissionsBy manager "bafu-like" `shouldReturn` before

        it "refuses an activity named twice" $
            withEcoSpold1DatabaseOf threeDatasets $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                co2 <- co2Of manager
                let twice = head' keys
                outcome <- editActivities manager "bafu-like" Nothing Nothing ((twice, [SetAmount (SelectBiosphere co2) 1]) :| [(twice, [SetAmount (SelectBiosphere co2) 2])])
                void (first refusalMessage outcome) `shouldSatisfy` failsWith "more than once"
                readJournal home `shouldReturn` Right []

        it "replays to the database the edits left" $
            withEcoSpold1DatabaseOf threeDatasets $ \manager home -> do
                keys <- processKeysOf manager "bafu-like"
                co2 <- co2Of manager
                _ <- editActivities manager "bafu-like" Nothing Nothing (setCo2 co2 keys)
                live <- emissionsBy manager "bafu-like"
                -- Without the stamp the cache is not trusted, so the reload
                -- reads the sources and replays the journal over them.
                removeFile (home </> "journal.applied")
                _ <- reloadOf manager "bafu-like"
                emissionsBy manager "bafu-like" `shouldReturn` live

    describe "a missing product given a supplier" $ do
        it "moves every line that asks for it, keeps their amounts, and leaves the others alone" $
            withEcoSpold1Datasets farm $ \manager home -> do
                missingCount manager "barley" `shouldReturn` Just 2
                provider <- processIdNamed manager "barley grain, milled"
                outcome <- supplyMissing manager "bafu-like" Nothing Nothing (MissingRequest "barley" Nothing provider)
                void (first refusalMessage outcome) `shouldBe` Right ()
                bought <- boughtBy manager
                M.lookup "bread" bought `shouldBe` Just [(Just provider, 2), (Nothing, 1)]
                M.lookup "beer" bought `shouldBe` Just [(Just provider, 3)]
                missingCount manager "barley" `shouldReturn` Nothing
                missingCount manager "oats" `shouldReturn` Just 1
                journal <- readJournal home
                fmap length journal `shouldBe` Right 2

        it "refuses the whole change when the provider cannot supply a line in its unit" $
            withEcoSpold1Datasets farm $ \manager home -> do
                provider <- processIdNamed manager "electricity production, wind"
                outcome <- supplyMissing manager "bafu-like" Nothing Nothing (MissingRequest "barley" Nothing provider)
                void (first refusalMessage outcome) `shouldSatisfy` failsWith ("input from \"" <> provider <> "\": ")
                readJournal home `shouldReturn` Right []
                missingCount manager "barley" `shouldReturn` Just 2

        it "says so when no input asks for the product" $
            withEcoSpold1Datasets farm $ \manager home -> do
                provider <- processIdNamed manager "barley grain, milled"
                outcome <- supplyMissing manager "bafu-like" Nothing Nothing (MissingRequest "rye" Nothing provider)
                void (first refusalMessage outcome) `shouldSatisfy` failsWith "No input of this database asks for rye"
                readJournal home `shouldReturn` Right []

    describe "refusals that protect the value" $ do
        it "refuses a second edit while one is in progress" $
            withDataDir $ \_ -> do
                manager <- initDatabaseManager defaultConfig NoCache
                db <- buildTwoActivityFixture
                install manager "busy" db (configuredConfig "busy")
                atomically $ modifyTVar' (dmStagingDbs manager) (Set.insert "busy")
                r <- mutateUploadedDatabase manager "busy" Nothing (pure dropSecond)
                void r `shouldSatisfy` failsWith "already in progress"

        it "refuses while another loaded database depends on this one" $
            withDataDir $ \_ -> do
                manager <- initDatabaseManager defaultConfig NoCache
                db <- buildTwoActivityFixture
                install manager "background" db (configuredConfig "background")
                install manager "foreground" db{dbDependsOn = ["background"]} (configuredConfig "foreground")
                r <- mutateUploadedDatabase manager "background" Nothing (pure dropSecond)
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
withEcoSpold1Database = withEcoSpold1DatabaseOf ["electricity production, wind", "electricity production, solar"]

-- | The same, with one dataset per name.
withEcoSpold1DatabaseOf :: [Text] -> (DatabaseManager -> FilePath -> IO ()) -> IO ()
withEcoSpold1DatabaseOf names = withEcoSpold1Datasets (zipWith dataset [1 ..] names)

-- | The same, with these datasets, each the file of its own.
withEcoSpold1Datasets :: [Text] -> (DatabaseManager -> FilePath -> IO ()) -> IO ()
withEcoSpold1Datasets datasets act =
    withDataDir $ \dataRoot -> do
        let home = dataRoot </> "uploads" </> "databases" </> "bafu-like"
            dataDir = home </> "data"
        createDirectoryIfMissing True dataDir
        forM_ (zip [1 :: Int ..] datasets) $ \(number, text) ->
            TIO.writeFile (dataDir </> "process_" <> T.unpack (UUID.toText (mkUUID (100 + number))) <> ".xml") text
        manager <- initDatabaseManager defaultConfig UseCache
        addDatabase manager (uploadedConfig "bafu-like" dataDir)
        loaded <- loadDatabase manager "bafu-like"
        case loaded of
            Left err -> expectationFailure ("loading the fixture: " <> show err)
            Right _ -> act manager home

threeDatasets :: [Text]
threeDatasets = ["electricity production, wind", "electricity production, solar", "electricity production, hydro"]

-- | One EcoSpold 1 dataset: a reference product and one emission.
dataset :: Int -> Text -> Text
dataset number name = datasetBuying number name "kWh" []

{- | The same, in this unit, buying each of these products by name, in kg, from
no supplier in particular.
-}
datasetBuying :: Int -> Text -> Text -> [(Text, Double)] -> Text
datasetBuying number name unit bought =
    T.unlines $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"" <> T.pack (show number) <> "\" generator=\"Test\" timestamp=\"2026-01-01T00:00:00\">"
        , "    <metaInformation><processInformation>"
        , "      <referenceFunction name=\"" <> name <> "\" category=\"Energy\" subCategory=\"Electricity\" unit=\"" <> unit <> "\" />"
        , "      <geography location=\"DE\" />"
        , "    </processInformation></metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"" <> name <> ", output\" category=\"Energy\" subCategory=\"Electricity\" unit=\"" <> unit <> "\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Carbon dioxide, fossil\" category=\"air\" subCategory=\"low population density\" unit=\"kg\" meanValue=\"0.01\">"
        , "        <outputGroup>4</outputGroup>"
        , "      </exchange>"
        ]
            <> concat
                [ [ "      <exchange number=\"" <> T.pack (show n) <> "\" name=\"" <> product' <> "\" category=\"Agriculture\" subCategory=\"Crops\" unit=\"kg\" meanValue=\"" <> T.pack (show amount) <> "\">"
                  , "        <inputGroup>5</inputGroup>"
                  , "      </exchange>"
                  ]
                | (n, (product', amount)) <- zip [3 :: Int ..] bought
                ]
            <> [ "    </flowData>"
               , "  </dataset>"
               , "</ecoSpold>"
               ]

{- | A bakery and a brewery buying barley from nobody, the bakery oats too, and
a mill whose product is named otherwise, so nothing links to it by name.
-}
farm :: [Text]
farm =
    [ datasetBuying 1 "bread" "kg" [("barley", 2), ("oats", 1)]
    , datasetBuying 2 "beer" "kg" [("barley", 3)]
    , datasetBuying 3 "barley grain, milled" "kg" []
    , dataset 4 "electricity production, wind"
    ]

-- | The process id of the activity of this name.
processIdNamed :: DatabaseManager -> Text -> IO Text
processIdNamed manager name = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    let found =
            [ processRefText (uncurry ProcessRef key)
            | Just loaded <- [M.lookup "bafu-like" loadedDbs]
            , let db = ldDatabase loaded
            , (key, activity) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db))
            , activityName activity == name
            ]
    case found of
        [pid] -> pure pid
        _ -> fail ("the fixture should hold one activity named " <> T.unpack name)

-- | Each activity's inputs, by activity name: the supplier each names, and its amount.
boughtBy :: DatabaseManager -> IO (M.Map Text [(Maybe Text, Double)])
boughtBy manager = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    pure $ case M.lookup "bafu-like" loadedDbs of
        Nothing -> M.empty
        Just loaded ->
            let db = ldDatabase loaded
             in M.fromList
                    [ (activityName activity, [(supplierOf ex, techAmount ex) | ex@TechnosphereExchange{techRole = Input} <- exchanges activity])
                    | activity <- V.toList (dbActivities db)
                    ]
  where
    supplierOf ex = (\act -> processRefText (ProcessRef act (techFlowId ex))) <$> techActivityLinkId ex

-- | How many lines the missing-supplier table counts for this product, if it lists it.
missingCount :: DatabaseManager -> Text -> IO (Maybe Int)
missingCount manager product' = do
    setup <- getDatabaseSetupInfo manager "bafu-like"
    case setup of
        Left err -> fail ("setup: " <> show err)
        Right info -> pure $ case [msCount row | row <- dsiMissingSuppliers info, msProductName row == product'] of
            [] -> Nothing
            counts -> Just (sum counts)

-- | The process ids a loaded database currently holds, in table order.
processKeysOf :: DatabaseManager -> Text -> IO [Text]
processKeysOf manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    pure $ case M.lookup dbName loadedDbs of
        Nothing -> []
        Just loaded -> map (processRefText . uncurry ProcessRef) (V.toList (dbProcessIdTable (ldDatabase loaded)))

-- | Delete the first process, through the request path a client uses.
deleteFirst :: DatabaseManager -> Text -> [Text] -> IO DeleteOutcome
deleteFirst manager dbName keys = do
    r <- deleteActivitiesInDB manager dbName Nothing (deleteIds [head' keys])
    either (fail . T.unpack) pure r

-- | Unload, load again, and say which processes came back.
reloadOf :: DatabaseManager -> Text -> IO [Text]
reloadOf manager dbName = do
    unloadDatabase manager dbName >>= either (fail . T.unpack) pure
    loadDatabase manager dbName >>= either (fail . T.unpack) (const (pure ()))
    processKeysOf manager dbName

-- | Each process's biosphere amounts, by process id.
emissionsBy :: DatabaseManager -> Text -> IO (M.Map Text [Double])
emissionsBy manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    pure $ case M.lookup dbName loadedDbs of
        Nothing -> M.empty
        Just loaded ->
            let db = ldDatabase loaded
             in M.fromList
                    [ (processRefText (uncurry ProcessRef key), [bioAmount ex | ex@BiosphereExchange{} <- exchanges activity])
                    | (key, activity) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db))
                    ]

-- | The fixture's one biosphere flow.
co2Of :: DatabaseManager -> IO UUID
co2Of manager = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case maybe [] (M.keys . dbBioFlows . ldDatabase) (M.lookup "bafu-like" loadedDbs) of
        [flow] -> pure flow
        flows -> fail ("the fixture should have one biosphere flow, not " <> show (length flows))

-- | Set the emission of each process to its rank: 1, 2, 3.
setCo2 :: UUID -> [Text] -> NonEmpty (Text, [ExchangeEdit])
setCo2 co2 keys = case zipWith (\key amount -> (key, [SetAmount (SelectBiosphere co2) amount])) keys [1 ..] of
    request : rest -> request :| rest
    [] -> ("the fixture has no processes", []) :| []

opTarget :: JournalOp -> Maybe Text
opTarget = \case
    Edited target _ -> Just target
    Created _ _ -> Nothing
    Replaced _ _ -> Nothing
    Deleted _ -> Nothing
    ParametersEdited _ -> Nothing

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
dropSecond :: Change
dropSecond = Change Nothing (Deleted [processRefText (ProcessRef otherActId supplierProdId)])

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
        , drNote = Nothing
        }

install :: DatabaseManager -> Text -> Database -> DatabaseConfig -> IO ()
install manager name db config = do
    solver <- mkSolver name db
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = config, ldSupplierMapping = Nothing}
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
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = Nothing
        , dcRequires = []
        , dcAdded = Nothing
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
            (BuildInputs defaultUnitConfig mempty ReadDeclared [])
            SimpleDatabase
                { sdbActivities = activities
                , sdbTechFlows = M.singleton supplierProdId milkFlow
                , sdbBioFlows = M.singleton co2Id co2Flow
                , sdbWasteFlows = M.empty
                , sdbUnits = unitTable
                , sdbDocumentation = noDocumentation
                , sdbParameters = M.empty
                , sdbProductSystems = []
                , sdbReferenceConvention = SignedReferences
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
                , techFormula = Nothing
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
                , bioFormula = Nothing
                }
            ]
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

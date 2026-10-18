{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the COPY primitive ('Database.Edit.copyDatabase' and the pure
'Database.Edit.copyDatabaseAs').

A copy is a second, independent registry entry over an immutable 'Database'
value:

* it is registered under the new name in the loaded / available maps;
* its config is renamed (dcName / dcDisplayName);
* mutating or dropping the copy leaves the source untouched, and dropping the
  source leaves the copy fully intact: the value is immutable, so there is no
  aliasing to break;
* copying onto an existing name, or from an unloaded source, fails loudly.
-}
module CopySpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import Data.Store (decodeEx, encode)
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import API.Types (ParameterValue (..), ScenarioAPI (..))
import Config (DatabaseConfig (..), defaultConfig)
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.Edit (copyDatabase, refusalMessage)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
 )
import Database.Scenarios (dropScenario, listScenarios, saveScenario)
import qualified Database.UploadedDatabase as UploadedDB
import Matrix (buildDemandVector)
import SharedSolver (
    SharedSolver,
    createSharedSolver,
    getFactorization,
    solveWithSharedSolver,
 )
import System.FilePath ((</>))
import TestHelpers (membersOnly, withScratchDataDir)
import Types (
    Activity (..),
    AllocationKey (..),
    BuildInputs (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    Licence (..),
    LocationSource (..),
    Parameter (..),
    ParameterDefinition (..),
    Release (..),
    ScenarioValue (..),
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
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
-- A copy now leaves a directory of its own behind, so each example gets a data
-- directory of its own rather than writing into the tree it was run from.
spec = around_ withScratchDataDir $ describe "Database.Edit copy primitive" $ do
    it "registers an independent copy under the new name" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 100 ["p1", "p2"])
        installLoaded manager "source" srcDb

        result <- copyDatabase manager "source" "mycopy"
        result `shouldBe` Right ()

        loaded <- readTVarIO (dmLoadedDbs manager)
        available <- readTVarIO (dmAvailableDbs manager)
        M.keys loaded `shouldMatchList` ["source", "mycopy"]
        M.member "mycopy" available `shouldBe` True

        let copy = loaded M.! "mycopy"
        -- Renamed config.
        dcName (ldConfig copy) `shouldBe` "mycopy"
        dcDisplayName (ldConfig copy) `shouldBe` "mycopy"
        -- Same data: identical activity count to the source.
        V.length (dbActivities (ldDatabase copy))
            `shouldBe` V.length (dbActivities srcDb)

    -- What a restart reads back once the source is gone: the terms in force
    -- when the copy was made, not the ones the source was loaded with.
    it "records the licence its source is served under now" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 100 ["p1"])
        installLoaded manager "source" srcDb
        let refused = membersOnly
        atomically $ modifyTVar' (dmAvailableDbs manager) (M.adjust (\c -> c{dcLicence = refused}) "source")

        _ <- copyDatabase manager "source" "mycopy"
        home <- (</> "mycopy") <$> UploadedDB.getDatabaseUploadsDir
        fmap UploadedDB.umLicence <$> UploadedDB.readUploadMeta home `shouldReturn` Just refused

    -- A copy is made to be changed, so it is the publisher's release only once
    -- its owner says so: the engine serves it without one, as the restart will.
    it "leaves the copy without its source's release, before a restart as after" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 100 ["p1"])
        installLoaded manager "source" srcDb
        let declared c = c{dcRelease = Just Release{releaseName = "ecoinvent", releaseVersion = "3.12", releaseSystemModel = Nothing}}
        atomically $ do
            modifyTVar' (dmLoadedDbs manager) (M.adjust (\l -> l{ldConfig = declared (ldConfig l)}) "source")
            modifyTVar' (dmAvailableDbs manager) (M.adjust declared "source")

        _ <- copyDatabase manager "source" "mycopy"
        served <- M.lookup "mycopy" <$> readTVarIO (dmAvailableDbs manager)
        dcRelease <$> served `shouldBe` Just Nothing
        home <- (</> "mycopy") <$> UploadedDB.getDatabaseUploadsDir
        fmap UploadedDB.umRelease <$> UploadedDB.readUploadMeta home `shouldReturn` Just Nothing

    it "is a deep, independent value: dropping the copy does not touch the source" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 200 ["p1", "p2", "p3"])
        installLoaded manager "source" srcDb
        _ <- copyDatabase manager "source" "mycopy"

        before <- readTVarIO (dmLoadedDbs manager)
        let srcCount = V.length (dbActivities (ldDatabase (before M.! "source")))

        -- Delete the copy outright from every registry map.
        atomically $ do
            modifyTVar' (dmLoadedDbs manager) (M.delete "mycopy")
            modifyTVar' (dmAvailableDbs manager) (M.delete "mycopy")
            modifyTVar' (dmIndexedDbs manager) (M.delete "mycopy")

        after <- readTVarIO (dmLoadedDbs manager)
        M.member "mycopy" after `shouldBe` False
        M.member "source" after `shouldBe` True
        V.length (dbActivities (ldDatabase (after M.! "source"))) `shouldBe` srcCount

    it "is a deep, independent value: dropping the source leaves the copy intact" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 300 ["p1", "p2"])
        installLoaded manager "source" srcDb
        _ <- copyDatabase manager "source" "mycopy"

        let srcCount = V.length (dbActivities srcDb)
        atomically $ do
            modifyTVar' (dmLoadedDbs manager) (M.delete "source")
            modifyTVar' (dmAvailableDbs manager) (M.delete "source")
            modifyTVar' (dmIndexedDbs manager) (M.delete "source")

        after <- readTVarIO (dmLoadedDbs manager)
        M.member "source" after `shouldBe` False
        V.length (dbActivities (ldDatabase (after M.! "mycopy"))) `shouldBe` srcCount

    it "gives the copy its own solver: factorizing the source does not warm the copy's cache" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 700 ["p1", "p2"])
        installLoaded manager "source" srcDb
        _ <- copyDatabase manager "source" "mycopy"

        loaded <- readTVarIO (dmLoadedDbs manager)
        let srcSolver = ldSharedSolver (loaded M.! "source")
            copySolver = ldSharedSolver (loaded M.! "mycopy")

        -- Force the source solver's lazy factorization via a first solve.
        _ <- solveWithSharedSolver srcSolver =<< either (fail . show) pure (buildDemandVector srcDb 0)
        -- MatrixFactorization has no Show instance, so assert on the Bool.
        getFactorization srcSolver >>= (`shouldBe` True) . isJust

        -- A shared MVar would have warmed the copy too; a distinct one stays empty.
        getFactorization copySolver >>= (`shouldBe` True) . isNothing

    it "refuses to overwrite an existing database name" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 400 ["p1"])
        otherDb <- buildOrFail (supplierDB 500 ["q1"])
        installLoaded manager "source" srcDb
        installLoaded manager "taken" otherDb

        result <- copyDatabase manager "source" "taken"
        result `shouldBe` Left "Database already exists: taken"

    it "fails when the source is not loaded" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        result <- copyDatabase manager "ghost" "mycopy"
        result `shouldBe` Left "Database not loaded: ghost"

    it "slugifies a path-traversal copy name to a filesystem-safe slug" $ do
        -- The copy is registered as an uploaded database and later deleted by
        -- name via removeDirectoryRecursive, so the name must never carry a
        -- path separator or parent ref that could escape the uploads directory.
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 800 ["p1", "p2"])
        installLoaded manager "source" srcDb
        result <- copyDatabase manager "source" "../../etc/passwd"
        result `shouldBe` Right ()
        loaded <- readTVarIO (dmLoadedDbs manager)
        let copyNames = filter (/= "source") (M.keys loaded)
        copyNames `shouldSatisfy` (not . null)
        copyNames `shouldSatisfy` all (\n -> not ("/" `isInfixOf` n) && not (".." `isInfixOf` n))

    it "rejects a copy name with no usable characters" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        srcDb <- buildOrFail (supplierDB 900 ["p1", "p2"])
        installLoaded manager "source" srcDb
        result <- copyDatabase manager "source" "///"
        case result of
            Left msg -> msg `shouldSatisfy` isInfixOf "Invalid copy name"
            Right () -> expectationFailure "expected empty-slug copy name to be rejected"

    scenariosSpec

{- | Scenarios are saved on a database of one's own: a copy here, of a source
the configuration declares, whose one parameter @salt_kg@ is 0.01.
-}
scenariosSpec :: Spec
scenariosSpec = describe "saved scenarios" $ do
    let salted = ParameterValue Nothing "salt_kg"
        withCopy run = do
            manager <- initDatabaseManager defaultConfig NoCache
            srcDb <- buildOrFail (supplierDB 100 ["p1"])
            installLoaded manager "source" srcDb{dbParameters = M.fromList [("salt_kg", Parameter (InputParameter 0.01 Nothing) Nothing "")]}
            copyDatabase manager "source" "mine" `shouldReturn` Right ()
            run manager
        listed manager name = either (fail . T.unpack . refusalMessage) (pure . map (\s -> (saName s, map pvValue (saParameters s), saProblem s))) =<< listScenarios manager name
        refusal = fmap (either refusalMessage (const ""))

    it "saves, replaces and drops a scenario by its name" $ withCopy $ \manager -> do
        refusal (saveScenario manager "mine" "high salt" [salted 0.02]) `shouldReturn` ""
        refusal (saveScenario manager "mine" "low salt" [salted 0.005]) `shouldReturn` ""
        refusal (saveScenario manager "mine" "high salt" [salted 0.03]) `shouldReturn` ""
        listed manager "mine" `shouldReturn` [("high salt", [0.03], Nothing), ("low salt", [0.005], Nothing)]
        refusal (dropScenario manager "mine" "low salt") `shouldReturn` ""
        listed manager "mine" `shouldReturn` [("high salt", [0.03], Nothing)]

    it "refuses what it could not apply, naming why" $ withCopy $ \manager -> do
        refusal (dropScenario manager "mine" "low salt") `shouldReturn` "Not in this database: scenario 'low salt'"
        refusal (saveScenario manager "mine" "typo" [ParameterValue Nothing "salt_kgg" 1]) >>= (`shouldSatisfy` ("salt_kgg" `isInfixOf`))
        refusal (saveScenario manager "mine" " " [salted 1]) `shouldReturn` "A scenario needs a name."
        refusal (saveScenario manager "mine" "high salt " [salted 1]) >>= (`shouldSatisfy` ("starts or ends with a space" `isInfixOf`))
        refusal (saveScenario manager "mine" "empty" []) >>= (`shouldSatisfy` ("sets no parameter" `isInfixOf`))
        listed manager "mine" `shouldReturn` []

    it "refuses a database read from the configuration, which holds none" $ withCopy $ \manager -> do
        refusal (saveScenario manager "source" "high salt" [salted 0.02]) >>= (`shouldSatisfy` ("Copy it" `isInfixOf`))
        listed manager "source" `shouldReturn` []

    it "says which scenarios an edit of the database has made stale" $ withCopy $ \manager -> do
        refusal (saveScenario manager "mine" "high salt" [salted 0.02]) `shouldReturn` ""
        atomically $ modifyTVar' (dmLoadedDbs manager) (M.adjust (\l -> l{ldDatabase = (ldDatabase l){dbParameters = M.empty}}) "mine")
        map (\(name, _, problem) -> (name, isJust problem)) <$> listed manager "mine" `shouldReturn` [("high salt", True)]

    describe "shipped with the database" $ do
        let shipped =
                M.fromList
                    [ ("Plant: high salt", [ScenarioValue Nothing "salt_kg" 0.04])
                    , ("Plant: p1's own", [ScenarioValue (Just (mkUUID 101)) "salt_kg" 1])
                    , ("Plant: gone", [ScenarioValue (Just (mkUUID 999)) "salt_kg" 1])
                    ]
            withShipped run = withCopy $ \manager -> do
                atomically $ modifyTVar' (dmLoadedDbs manager) (M.map (\l -> l{ldDatabase = (ldDatabase l){dbScenarios = shipped}}))
                run manager
            listing manager name = either (fail . T.unpack . refusalMessage) (pure . map (\s -> (saName s, saShipped s, map pvProcess (saParameters s)))) =<< listScenarios manager name

        it "are listed on a database read from the configuration, its process named by its first product" $ withShipped $ \manager -> do
            listing manager "source"
                `shouldReturn` [ ("Plant: gone", True, [Just (UUID.toText (mkUUID 999))])
                               , ("Plant: high salt", True, [Nothing])
                               , ("Plant: p1's own", True, [Just (UUID.toText (mkUUID 101) <> "_" <> UUID.toText (mkUUID 102))])
                               ]
            problems <- map (\(_, _, problem) -> problem) <$> listed manager "source"
            problems `shouldSatisfy` \case
                [Just gone, Nothing, Just own] -> UUID.toText (mkUUID 999) `isInfixOf` gone && "does not define 'salt_kg'" `isInfixOf` own
                _ -> False

        it "travel with the database through the cache" $ do
            db <- buildOrFail (supplierDB 100 ["p1"])
            dbScenarios (decodeEx (encode db{dbScenarios = shipped})) `shouldBe` shipped

        it "are neither replaced nor dropped" $ withShipped $ \manager -> do
            refusal (saveScenario manager "mine" "Plant: high salt" [salted 0.02]) >>= (`shouldSatisfy` ("ships with the database" `isInfixOf`))
            refusal (dropScenario manager "mine" "Plant: high salt") >>= (`shouldSatisfy` ("comes back at the next load" `isInfixOf`))

        it "give way to a scenario saved under their name, which stays replaceable and droppable" $ withCopy $ \manager -> do
            refusal (saveScenario manager "mine" "Plant: high salt" [salted 0.02]) `shouldReturn` ""
            atomically $ modifyTVar' (dmLoadedDbs manager) (M.map (\l -> l{ldDatabase = (ldDatabase l){dbScenarios = shipped}}))
            filter (\(name, _, _) -> name == "Plant: high salt") <$> listing manager "mine" `shouldReturn` [("Plant: high salt", False, [Nothing])]
            refusal (saveScenario manager "mine" "Plant: high salt" [salted 0.03]) `shouldReturn` ""
            filter (\(name, _, _) -> name == "Plant: high salt") <$> listed manager "mine" `shouldReturn` [("Plant: high salt", [0.03], Nothing)]
            refusal (dropScenario manager "mine" "Plant: high salt") `shouldReturn` ""
            filter (\(name, _, _) -> name == "Plant: high salt") <$> listing manager "mine" `shouldReturn` [("Plant: high salt", True, [Nothing])]

    it "goes along with a copy of its database" $ withCopy $ \manager -> do
        refusal (saveScenario manager "mine" "high salt" [salted 0.02]) `shouldReturn` ""
        copyDatabase manager "mine" "yours" `shouldReturn` Right ()
        listed manager "yours" `shouldReturn` [("high salt", [0.02], Nothing)]
        refusal (dropScenario manager "yours" "high salt") `shouldReturn` ""
        listed manager "mine" `shouldReturn` [("high salt", [0.02], Nothing)]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

{- | Install a built database as a LoadedDatabase under @name@: solver,
config, loaded map, and available map. Mirrors what the load path does, but
without disk I/O.
-}
installLoaded :: DatabaseManager -> Text -> Database -> IO ()
installLoaded manager name db = do
    solver <- mkSolver name db
    let loaded =
            LoadedDatabase
                { ldDatabase = db
                , ldSharedSolver = solver
                , ldConfig = mkConfig name
                }
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert name loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert name (mkConfig name))

mkSolver :: Text -> Database -> IO SharedSolver
mkSolver name db =
    let triples = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
     in createSharedSolver name triples (fromIntegral (dbActivityCount db))

mkConfig :: Text -> DatabaseConfig
mkConfig name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
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
        , dcRelease = Nothing
        , dcRequires = []
        }

buildOrFail :: SimpleParts -> IO Database
buildOrFail (SimpleParts acts flows units) = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig M.empty Declared [])
            SimpleDatabase
                { sdbActivities = acts
                , sdbTechFlows = flows
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = units
                , sdbDocumentation = noDocumentation
                , sdbParameters = M.empty
                , sdbScenarios = M.empty
                }
    case r of
        Right db -> pure db
        Left err -> fail ("buildDatabaseWithMatrices: " <> show err)

-- ---------------------------------------------------------------------------
-- Fixture builders (single unit "kg", all activities at GLO)
-- ---------------------------------------------------------------------------

data SimpleParts
    = SimpleParts
        (M.Map (UUID, UUID) Activity)
        (M.Map UUID TechnosphereFlow)
        (M.Map UUID Unit)

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 0

kgUnit :: Unit
kgUnit = Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

mkTechFlow :: UUID -> Text -> TechnosphereFlow
mkTechFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

-- | One self-producing activity per product name. 'offset' keeps UUIDs disjoint.
supplierDB :: Int -> [Text] -> SimpleParts
supplierDB offset products =
    let entries =
            [ let actUUID = mkUUID (offset + 10 * i + 1)
                  prodUUID = mkUUID (offset + 10 * i + 2)
                  flowUUID = mkUUID (offset + 10 * i + 3)
                  flow = mkTechFlow flowUUID name
                  refOut =
                    TechnosphereExchange
                        { techFlowId = flowUUID
                        , techAmount = 1.0
                        , techUnitId = kgUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Just actUUID
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techFormula = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                  act =
                    Activity
                        { activityName = "supplier-of-" <> name
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "kg"
                        , exchanges = [refOut]
                        , activityParameters = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        , activityDates = noDates
                        }
               in (((actUUID, prodUUID), act), (flowUUID, flow))
            | (i, name) <- zip [0 ..] products
            ]
     in SimpleParts
            (M.fromList (map fst entries))
            (M.fromList (map snd entries))
            (M.singleton kgUnitId kgUnit)

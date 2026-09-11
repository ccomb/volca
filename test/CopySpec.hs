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
import Data.Text (Text, isInfixOf)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.Edit (copyDatabase)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
 )
import Matrix (buildDemandVectorFromIndex)
import SharedSolver (
    SharedSolver,
    createSharedSolver,
    getFactorization,
    solveWithSharedSolver,
 )
import TestHelpers (withScratchDataDir)
import Types (
    Activity (..),
    AllocationKey (..),
    BuildInputs (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
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
        _ <- solveWithSharedSolver srcSolver =<< either (fail . show) pure (buildDemandVectorFromIndex (dbActivityIndex srcDb) 0)
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
        , dcSource = Nothing
        }

buildOrFail :: SimpleParts -> IO Database
buildOrFail (SimpleParts acts flows units) = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig M.empty Declared)
            SimpleDatabase
                { sdbActivities = acts
                , sdbTechFlows = flows
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = units
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
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
               in (((actUUID, prodUUID), act), (flowUUID, flow))
            | (i, name) <- zip [0 ..] products
            ]
     in SimpleParts
            (M.fromList (map fst entries))
            (M.fromList (map snd entries))
            (M.singleton kgUnitId kgUnit)

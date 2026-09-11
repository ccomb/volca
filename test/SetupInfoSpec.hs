{-# LANGUAGE OverloadedStrings #-}

module SetupInfoSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    DatabaseSetupInfo (..),
    LoadedDatabase (..),
    MissingSupplier (..),
    SetupError (..),
    buildLoadedSetupInfo,
    finalizeDatabase,
    getDatabaseSetupInfo,
    initDatabaseManager,
 )
import SharedSolver (createSharedSolver)
import Types
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Minimal fixtures
-- ---------------------------------------------------------------------------

consumerAct, consumerProd, supplierAct, supplierProd, missingAct, gapProdA, gapProdB :: UUID.UUID
consumerAct = read "cccccccc-0000-0000-0000-000000000001"
consumerProd = read "aaaaaaaa-0000-0000-0000-000000000001"
supplierAct = read "cccccccc-0000-0000-0000-000000000002"
supplierProd = read "bbbbbbbb-0000-0000-0000-000000000002"
missingAct = read "dddddddd-0000-0000-0000-000000000099"
gapProdA = read "bbbbbbbb-0000-0000-0000-000000000003"
gapProdB = read "bbbbbbbb-0000-0000-0000-000000000004"

minimalFlow :: UUID.UUID -> Text -> TechnosphereFlow
minimalFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = UUID.nil
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

minimalActivity :: Text -> [Exchange] -> Activity
minimalActivity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

refExchange :: UUID.UUID -> Exchange
refExchange fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techRole = ReferenceProduct
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

-- | A technosphere input for @prodId@ linked to producer activity @actId@.
linkedInput :: UUID.UUID -> UUID.UUID -> Exchange
linkedInput actId prodId =
    TechnosphereExchange
        { techFlowId = prodId
        , techAmount = 0.5
        , techUnitId = UUID.nil
        , techRole = Input
        , techActivityLinkId = Just actId
        , -- An EcoSpold 2 row: the identifier is both what the source said and
          -- what the load resolved it to.
          techSupplierClaim = ClaimById actId
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

{- | A loaded database has no UI picker; only its name/path matter to the setup
info, so a permissive stub config suffices.
-}
stubConfig :: DatabaseConfig
stubConfig =
    DatabaseConfig
        { dcName = "test"
        , dcDisplayName = "Test DB"
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

buildDb :: [((UUID.UUID, UUID.UUID), Activity)] -> [(UUID.UUID, Text)] -> IO Database
buildDb acts flows = do
    res <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared)
            SimpleDatabase
                { sdbActivities = M.fromList acts
                , sdbTechFlows = M.fromList [(fid, minimalFlow fid name) | (fid, name) <- flows]
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = M.empty
                }
    case res of
        Left err -> error ("buildDatabaseWithMatrices failed: " <> show err)
        Right db -> pure db

-- | Setup info for a self-contained loaded database (no deps, no other DBs).
setupInfoFor :: Database -> DatabaseSetupInfo
setupInfoFor db = buildLoadedSetupInfo stubConfig db M.empty M.empty

{- | Install a built database as a LoadedDatabase under @name@: solver, config,
loaded map, and available map. Mirrors what the load path does, without disk
I/O.
-}
installLoaded :: DatabaseManager -> Text -> Database -> IO ()
installLoaded manager name db = do
    let triples = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
        config = stubConfig{dcName = name, dcDisplayName = name}
    solver <- createSharedSolver name triples (fromIntegral (dbActivityCount db))
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = config}
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert name loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert name config)

-- | A fresh manager with @db@ installed as a loaded database named "test".
managerWithLoaded :: Database -> IO DatabaseManager
managerWithLoaded db = do
    manager <- initDatabaseManager defaultConfig NoCache
    installLoaded manager "test" db
    pure manager

-- | The partial-import shape: a dangling background link no dependency ships.
partialDb :: IO Database
partialDb =
    buildDb
        [
            ( (consumerAct, consumerProd)
            , minimalActivity
                "lyocell fibre"
                [refExchange consumerProd, linkedInput missingAct supplierProd]
            )
        ]
        [(consumerProd, "lyocell fibre"), (supplierProd, "chemical, inorganic")]

-- | The self-contained shape: every input resolves internally.
readyDb :: IO Database
readyDb =
    buildDb
        [
            ( (consumerAct, consumerProd)
            , minimalActivity
                "lyocell fibre"
                [refExchange consumerProd, linkedInput supplierAct supplierProd]
            )
        , ((supplierAct, supplierProd), minimalActivity "chemical, inorganic" [refExchange supplierProd])
        ]
        [(consumerProd, "lyocell fibre"), (supplierProd, "chemical, inorganic")]

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- A single-.spold import: the foreground consumer carries a non-nil
    -- activityLinkId to a background activity it doesn't ship. The matrix
    -- builder drops that input, so a loaded database (which bypasses the
    -- finalize gate) must report as not-ready with 0% completeness and name the
    -- missing background product, never a green "ready" badge over a silently
    -- zero score.
    describe "buildLoadedSetupInfo (partial EcoSpold2 import)" $ do
        it "reports a dangling background link as not ready / 0% / named" $ do
            info <- setupInfoFor <$> partialDb
            dsiIsReady info `shouldBe` False
            dsiCompleteness info `shouldBe` 0.0
            map msProductName (dsiMissingSuppliers info) `shouldBe` ["chemical, inorganic"]

    -- The same shape, but the background activity is present: every input
    -- resolves internally, so the database stays ready at 100% with no gaps.
    describe "buildLoadedSetupInfo (well-formed self-contained database)" $ do
        it "reports a fully resolved database as ready / 100% / no gaps" $ do
            info <- setupInfoFor <$> readyDb
            dsiIsReady info `shouldBe` True
            dsiCompleteness info `shouldBe` 100.0
            dsiMissingSuppliers info `shouldBe` []

    -- Two gaps with different demand counts, alphabetically ordered against
    -- the count order: the list must rank by demand, not map order.
    describe "buildLoadedSetupInfo (missing-supplier ranking)" $ do
        it "ranks missing suppliers by demanding-input count, descending" $ do
            let consumer =
                    minimalActivity
                        "lyocell fibre"
                        [ refExchange consumerProd
                        , linkedInput missingAct gapProdA
                        , linkedInput missingAct gapProdB
                        , linkedInput missingAct gapProdB
                        ]
            db <-
                buildDb
                    [((consumerAct, consumerProd), consumer)]
                    [(consumerProd, "lyocell fibre"), (gapProdA, "aaa gap"), (gapProdB, "zzz gap")]
            let info = setupInfoFor db
            [(msProductName s, msCount s) | s <- dsiMissingSuppliers info]
                `shouldBe` [("zzz gap", 2), ("aaa gap", 1)]

    -- A product is routinely refused several ways: two demands in a unit the
    -- dependency does not ship it in, three more at a location the geography
    -- policy rejects. One row carrying five would name a cause that raised
    -- two of them and hide the one that raised three.
    describe "buildLoadedSetupInfo (a product refused two ways)" $
        it "lists the product once per reason, biggest first" $ do
            db <- readyDb
            let refused =
                    (dbLinkingStats db)
                        { cdlUnresolvedProducts =
                            M.singleton
                                "wheat"
                                (UnresolvedProduct (M.fromList [(UnitIncompatible "m3" "kg", 2), (LocationUnavailable "FR", 3)]))
                        }
                info = setupInfoFor db{dbLinkingStats = refused}
            [(msProductName s, msCount s, msReason s, msDetail s) | s <- dsiMissingSuppliers info]
                `shouldBe` [ ("wheat", 3, "location_unavailable", Just "FR")
                           , ("wheat", 2, "unit_incompatible", Just "m3 vs kg")
                           ]

    -- The dangling-import shape, but its matching background is loaded as a
    -- dependency: the input resolves cross-DB by activityLinkId, recorded in
    -- 'dbCrossDBLinks'. Readiness must follow the matrix (ready at 100% with no
    -- gaps) not keep reporting the now-supplied product as missing.
    describe "buildLoadedSetupInfo (partial import + loaded background)" $ do
        it "reports a cross-DB-supplied background link as ready / 100% / no gaps" $ do
            let consumer =
                    minimalActivity
                        "lyocell fibre"
                        [refExchange consumerProd, linkedInput missingAct supplierProd]
                link =
                    CrossDBLink
                        { cdlConsumerActUUID = consumerAct
                        , cdlConsumerProdUUID = consumerProd
                        , cdlConsumerFlowId = supplierProd
                        , cdlSupplierActUUID = supplierAct
                        , cdlSupplierProdUUID = supplierProd
                        , cdlCoefficient = 0.5
                        , cdlExchangeUnit = "kg"
                        , cdlFlowName = "chemical, inorganic"
                        , cdlLocation = "GLO"
                        , cdlSourceDatabase = "background"
                        , cdlTiedAlternatives = []
                        }
            db <-
                buildDb
                    [((consumerAct, consumerProd), consumer)]
                    [(consumerProd, "lyocell fibre"), (supplierProd, "chemical, inorganic")]
            let info = setupInfoFor db{dbCrossDBLinks = [link]}
            dsiIsReady info `shouldBe` True
            dsiCompleteness info `shouldBe` 100.0
            dsiMissingSuppliers info `shouldBe` []

    -- The wire-level contract between GET /setup and POST /finalize: the two
    -- must never disagree about a loaded database.
    describe "setup / finalize coherence (loaded databases)" $ do
        it "reports a loaded database as loaded" $ do
            manager <- managerWithLoaded =<< readyDb
            result <- getDatabaseSetupInfo manager "test"
            fmap dsiIsLoaded result `shouldBe` Right True

        it "refuses to finalize a loaded database the setup reports as not ready" $ do
            manager <- managerWithLoaded =<< partialDb
            setup <- getDatabaseSetupInfo manager "test"
            fmap dsiIsReady setup `shouldBe` Right False
            finalized <- finalizeDatabase manager "test"
            case finalized of
                Left msg -> msg `shouldBe` "Cannot finalize: 1 unresolved inputs. Add dependencies to resolve them first."
                Right _ -> expectationFailure "expected finalize to refuse a not-ready loaded database"

        it "finalizes a ready loaded database as a no-op success" $ do
            manager <- managerWithLoaded =<< readyDb
            setup <- getDatabaseSetupInfo manager "test"
            fmap dsiIsReady setup `shouldBe` Right True
            finalized <- finalizeDatabase manager "test"
            case finalized of
                Left msg -> expectationFailure ("expected no-op finalize to succeed, got: " <> show msg)
                Right loaded -> dcName (ldConfig loaded) `shouldBe` "test"

        it "answers not-loaded for a configured database that was never loaded" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            let config = stubConfig{dcName = "cfg", dcDisplayName = "cfg"}
            atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert "cfg" config)
            result <- getDatabaseSetupInfo manager "cfg"
            case result of
                Left (SetupNotLoaded name) -> name `shouldBe` "cfg"
                other -> expectationFailure ("expected SetupNotLoaded, got: " <> show (fmap dsiName other))

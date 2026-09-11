{-# LANGUAGE OverloadedStrings #-}

{- | Cross-database back-substitution correctness tests.

Exercises the WithDeps path that includes biosphere contributions reached
through 'dbCrossDBLinks'. Since the test suite has no multi-DB fixture,
we test:

1. pure helpers on a single loaded sample DB;
2. that 'computeInventoryMatrixBatchWithDepsCached' on a DB with no
   cross-DB links reduces exactly to the local-only batch variant.
3. that 'depDemandsToVector' honours the supplier's reference unit and
   fails hard on unknown unit pairs.
4. that 'prepareDepDemandVecs' answers one vector per root, and picks out
   this dep's share of each root's demands rather than another dep's.
-}
module CrossDBInventorySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Matrix (Demand (..), DepDemands, accumulateDepDemands, depDemandsToVector)
import Method.Mapping (CF (..), CFUnit (..), MethodTables (..), inventoryContributions)
import qualified Method.Mapping as Mapping
import Method.Types (CFFamily (..), FlowDirection (..), MethodCF (..))
import SharedSolver (
    computeInventoryMatrixBatchCached,
    computeInventoryMatrixBatchWithDepsCached,
    createSharedSolver,
 )
import qualified SharedSolver
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "accumulateDepDemands" $ do
        it "returns empty map when database has no cross-DB links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                scalingVec = U.replicate n 1.0
            accumulateDepDemands db scalingVec `shouldBe` M.empty

        it "returns empty map when scaling vector is all zeros" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                scalingVec = U.replicate n 0.0
            accumulateDepDemands db scalingVec `shouldBe` M.empty

    describe "depDemandsToVector" $ do
        it "returns zero vector for empty demand map" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
            case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db M.empty of
                Right (Demand vec) -> do
                    U.length vec `shouldBe` n
                    U.all (== 0.0) vec `shouldBe` True
                Left err -> expectationFailure (T.unpack err)

        it "silently drops suppliers not present in the dep DB" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                fakeSupplier = (UUID.nil, UUID.nil)
                demands = M.singleton fakeSupplier (42.0, "kg")
            case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db demands of
                Right (Demand vec) -> do
                    U.length vec `shouldBe` n
                    U.all (== 0.0) vec `shouldBe` True
                Left err -> expectationFailure (T.unpack err)

        it "passes amount through unchanged when exchange unit matches supplier refUnit" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case firstActivityWithRefUnit db of
                Nothing -> pendingWith "SAMPLE.min3 has no activity with a reference output unit"
                Just (supplierKey, supplierIdx, refUnit) -> do
                    let demands = M.singleton supplierKey (7.5, refUnit)
                    case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db demands of
                        Right (Demand vec) -> vec U.! supplierIdx `shouldBe` 7.5
                        Left err -> expectationFailure (T.unpack err)

        it "fails hard when the unit pair is unknown" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case firstActivityWithRefUnit db of
                Nothing -> pendingWith "SAMPLE.min3 has no activity with a reference output unit"
                Just (supplierKey, _, refUnit) -> do
                    let bogusUnit = refUnit <> "__no_conversion__"
                        demands = M.singleton supplierKey (1.0, bogusUnit)
                    case depDemandsToVector defaultUnitConfig "dep-test" db demands of
                        Right _ -> expectationFailure "expected Left for unknown unit pair"
                        Left err -> do
                            err `shouldSatisfy` T.isInfixOf "Unknown unit conversion"
                            err `shouldSatisfy` T.isInfixOf "dep-test"

    describe "prepareDepDemandVecs" $ do
        it "returns one vector per root, each as long as the dep DB" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                perRoot = [M.empty, M.empty, M.empty] :: [DepDemands]
            case SharedSolver.prepareDepDemandVecs defaultUnitConfig "SAMPLE.min3" db perRoot of
                Right vecs -> do
                    length vecs `shouldBe` 3
                    map (U.length . unDemand) vecs `shouldBe` replicate 3 n
                Left err -> expectationFailure (T.unpack err)

        it "gives an all-zero vector for a root that demands nothing of this dep" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case firstActivityWithRefUnit db of
                Nothing -> pendingWith "SAMPLE.min3 has no activity with a reference output unit"
                Just (supplierKey, supplierIdx, refUnit) -> do
                    -- Two roots: the first demands of another dep, the second of this one.
                    let otherDep = M.singleton "some-other-db" (M.singleton supplierKey (7.5, refUnit))
                        thisDep = M.singleton "SAMPLE.min3" (M.singleton supplierKey (7.5, refUnit))
                    case SharedSolver.prepareDepDemandVecs defaultUnitConfig "SAMPLE.min3" db [otherDep, thisDep] of
                        Right [Demand absent, Demand present] -> do
                            U.all (== 0.0) absent `shouldBe` True
                            present U.! supplierIdx `shouldBe` 7.5
                        Right _ -> expectationFailure "expected one vector per root"
                        Left err -> expectationFailure (T.unpack err)

    describe "computeInventoryMatrixBatchWithDepsCached" $ do
        it "matches local-only batch for a DB with no cross-DB links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples =
                    [ (fromIntegral i, fromIntegral j, v)
                    | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
                    ]
                actCount = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "SAMPLE.min3" techTriples actCount
            let pids = [0]
                noDeps _ = pure Nothing

            localInvs <- either (fail . show) pure =<< computeInventoryMatrixBatchCached db solver pids
            withDepsE <- computeInventoryMatrixBatchWithDepsCached defaultUnitConfig noDeps db "SAMPLE.min3" solver pids

            case withDepsE of
                Left err -> expectationFailure (T.unpack err)
                Right withDepsSols -> do
                    length withDepsSols `shouldBe` length localInvs
                    case (localInvs, withDepsSols) of
                        ([a], [b]) -> M.toList a `shouldBe` M.toList (SharedSolver.csInventory b)
                        _ -> expectationFailure "expected one inventory per pid"

        it "empty pid list returns empty result without solving" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples =
                    [ (fromIntegral i, fromIntegral j, v)
                    | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
                    ]
                actCount = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "SAMPLE.min3-empty" techTriples actCount
            let noDeps _ = pure Nothing
            res <- computeInventoryMatrixBatchWithDepsCached defaultUnitConfig noDeps db "SAMPLE.min3-empty" solver []
            case res of
                Right sols -> length sols `shouldBe` 0
                Left err -> expectationFailure (T.unpack err)

    describe "inventoryContributions (cross-DB characterization surface)" $ do
        -- Synthetic data: simulate the shape of a cross-DB-merged Inventory
        -- where some UUIDs come from a 'dep' DB and are absent from the root
        -- 'flowDB' unless we pass a merged snapshot.
        let uuidRoot = mkUuid 1
            uuidDep = mkUuid 2
            uuidGone = mkUuid 3 -- in inventory but absent from any flowDB
            unitKg = Unit (mkUuid 10) "kg" "kg" ""
            unitDB = M.singleton (unitId unitKg) unitKg

            flowRoot =
                BiosphereFlow
                    uuidRoot
                    "Methane, biogenic"
                    (unitId unitKg)
                    M.empty
                    Nothing
                    Nothing
                    (Just (Compartment Air (Just "low. pop.")))
            flowDep =
                BiosphereFlow
                    uuidDep
                    "Carbon dioxide, fossil"
                    (unitId unitKg)
                    M.empty
                    Nothing
                    Nothing
                    (Just (Compartment Air (Just "low. pop.")))

            rootOnlyFlowDB = M.singleton uuidRoot flowRoot
            mergedFlowDB = M.fromList [(uuidRoot, flowRoot), (uuidDep, flowDep)]

            -- UUID-keyed method table entries for both flows
            uuidEntry fid name v =
                Mapping.TableEntry (CF v (CFUnit "kg CO2 eq")) $
                    Mapping.BuildProvenance
                        (Just Mapping.ByUUID)
                        MethodCF
                            { mcfFlowRef = fid
                            , mcfFlowName = name
                            , mcfDirection = Output
                            , mcfValue = v
                            , mcfCompartment = Nothing
                            , mcfCAS = Nothing
                            , mcfUnit = "kg CO2 eq"
                            , mcfConsumerLocation = Nothing
                            }
            tables =
                MethodTables
                    { mtUuidCF =
                        M.fromList
                            [ (uuidRoot, uuidEntry uuidRoot "Methane, biogenic" 27.0)
                            , (uuidDep, uuidEntry uuidDep "Carbon dioxide, fossil" 1.0)
                            ]
                    , mtUnitVariantCF = M.empty
                    , mtExactCF = M.empty
                    , mtFallbackCF = M.empty
                    , mtLongTermFallbackCF = M.empty
                    , mtSubBlindCF = M.empty
                    , mtCasCF = M.empty
                    , mtRegionalCasCF = M.empty
                    , mtRegionalizedCF = M.empty
                    , mtCFFamily = OtherCFFamily
                    , mtSeaWaterCFs = Mapping.MethodSilentOnSeaWater
                    , mtCompartmentMap = M.empty
                    , mtEnergyDensities = M.empty
                    , mtResolution = M.empty
                    , mtJudged = S.empty
                    , mtBroadcast = M.empty
                    , mtRegionalActivityWeights = Nothing
                    }

            inventory = M.fromList [(uuidRoot, 1.0), (uuidDep, 2.0), (uuidGone, 5.0)]

        it "returns empty contributions and no unknowns for empty inventory" $ do
            let (contribs, unknowns) = inventoryContributions defaultUnitConfig unitDB mergedFlowDB M.empty tables
            length contribs `shouldBe` 0
            unknowns `shouldBe` []

        it "surfaces inventory UUIDs absent from the flowDB (no silent drop)" $ do
            let (_, unknowns) = inventoryContributions defaultUnitConfig unitDB rootOnlyFlowDB inventory tables
            -- Dep-DB UUID is present in inventory but absent from root-only flowDB; same for uuidGone.
            S.fromList unknowns `shouldBe` S.fromList [uuidDep, uuidGone]

        it "characterizes dep-DB flows when the merged flowDB is supplied" $ do
            let (contribs, unknowns) = inventoryContributions defaultUnitConfig unitDB mergedFlowDB inventory tables
                namesWithContrib = [(bfName f, c) | (f, _, c) <- contribs]
            -- uuidGone remains unknown (it's in no flowDB at all); uuidRoot and
            -- uuidDep should both produce contributions.
            unknowns `shouldBe` [uuidGone]
            lookup "Carbon dioxide, fossil" namesWithContrib `shouldBe` Just 2.0 -- 2.0 kg * CF 1.0
            lookup "Methane, biogenic" namesWithContrib `shouldBe` Just 27.0 -- 1.0 kg * CF 27.0
        it "matches computeLCIAScoreFromTables when no UUIDs are unknown" $ do
            let (contribs, _) = inventoryContributions defaultUnitConfig unitDB mergedFlowDB (M.delete uuidGone inventory) tables
                sumContribs = sum [c | (_, _, c) <- contribs]
                score = Mapping.loScore (Mapping.computeLCIAScoreFromTables defaultUnitConfig unitDB mergedFlowDB (M.delete uuidGone inventory) tables)
            abs (sumContribs - score) < 1e-9 `shouldBe` True

-- | Build a deterministic test UUID from a small integer tag.
mkUuid :: Int -> UUID
mkUuid tag = UUID.fromWords64 0 (fromIntegral tag)

{- | Pick the first activity that has a reference output exchange with a
known unit, returning ((actUUID, prodUUID), matrixIndex, refUnit).
-}
firstActivityWithRefUnit :: Database -> Maybe ((UUID, UUID), Int, Text)
firstActivityWithRefUnit db =
    listToMaybe
        [ (procId, fromIntegral (dbActivityIndex db V.! idx), unit)
        | (idx, procId) <- zip [0 ..] (V.toList (dbProcessIdTable db))
        , let act = dbActivities db V.! idx
              refExs = [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)]
        , ex <- take 1 refExs
        , let unit = getUnitNameForExchange (dbUnits db) ex
        , not (T.null unit)
        , unit /= "unknown"
        ]

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
-}
module CrossDBInventorySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Matrix (accumulateDepDemands, depDemandsToVector)
import Method.Mapping (MethodTables (..), inventoryContributions)
import qualified Method.Mapping as Mapping
import SharedSolver (
    computeInventoryMatrixBatchCached,
    computeInventoryMatrixBatchWithDepsCached,
    createSharedSolver,
 )
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
                Right vec -> do
                    U.length vec `shouldBe` n
                    U.all (== 0.0) vec `shouldBe` True
                Left err -> expectationFailure (T.unpack err)

        it "silently drops suppliers not present in the dep DB" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                fakeSupplier = (UUID.nil, UUID.nil)
                demands = M.singleton fakeSupplier (42.0, "kg")
            case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db demands of
                Right vec -> do
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
                        Right vec -> vec U.! supplierIdx `shouldBe` 7.5
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

            localInvs <- computeInventoryMatrixBatchCached db solver pids
            withDepsE <- computeInventoryMatrixBatchWithDepsCached defaultUnitConfig noDeps db solver pids

            case withDepsE of
                Left err -> expectationFailure (T.unpack err)
                Right withDepsInvs -> do
                    length withDepsInvs `shouldBe` length localInvs
                    case (localInvs, withDepsInvs) of
                        ([a], [b]) -> M.toList a `shouldBe` M.toList b
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
            res <- computeInventoryMatrixBatchWithDepsCached defaultUnitConfig noDeps db solver []
            res `shouldBe` Right []

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
                Flow
                    uuidRoot
                    "Methane, biogenic"
                    "air"
                    (Just "low. pop.")
                    (unitId unitKg)
                    Biosphere
                    M.empty
                    Nothing
                    Nothing
            flowDep =
                Flow
                    uuidDep
                    "Carbon dioxide, fossil"
                    "air"
                    (Just "low. pop.")
                    (unitId unitKg)
                    Biosphere
                    M.empty
                    Nothing
                    Nothing

            rootOnlyFlowDB = M.singleton uuidRoot flowRoot
            mergedFlowDB = M.fromList [(uuidRoot, flowRoot), (uuidDep, flowDep)]

            -- UUID-keyed method table entries for both flows
            tables =
                MethodTables
                    { mtUuidCF =
                        M.fromList
                            [ (uuidRoot, (27.0, "kg CO2 eq"))
                            , (uuidDep, (1.0, "kg CO2 eq"))
                            ]
                    , mtExactCF = M.empty
                    , mtFallbackCF = M.empty
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
                namesWithContrib = [(flowName f, c) | (f, _, c) <- contribs]
            -- uuidGone remains unknown (it's in no flowDB at all); uuidRoot and
            -- uuidDep should both produce contributions.
            unknowns `shouldBe` [uuidGone]
            lookup "Carbon dioxide, fossil" namesWithContrib `shouldBe` Just 2.0 -- 2.0 kg * CF 1.0
            lookup "Methane, biogenic" namesWithContrib `shouldBe` Just 27.0 -- 1.0 kg * CF 27.0
        it "matches computeLCIAScoreFromTables when no UUIDs are unknown" $ do
            let (contribs, _) = inventoryContributions defaultUnitConfig unitDB mergedFlowDB (M.delete uuidGone inventory) tables
                sumContribs = sum [c | (_, _, c) <- contribs]
                score = Mapping.computeLCIAScoreFromTables defaultUnitConfig unitDB mergedFlowDB (M.delete uuidGone inventory) tables
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

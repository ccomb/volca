{-# LANGUAGE OverloadedStrings #-}

{- | Regression: regional LCIA must score dep-DB biosphere emissions, not
just root-DB ones.

The 'computeRegionalizedLCIAScore' fast path is a dot product
@rawWeights · scalingVec@. Both vectors are built from a single 'Database':
'rawWeights' from that DB's biosphere triples, 'scalingVec' from that DB's
activity columns. So when a root-DB activity consumes a dep-DB activity,
the dep DB's emissions are present in the merged 'Inventory' but invisible
to the regional path – its regional CFs were never queried.

This spec uses the synthetic two-DB fixture from
'CrossDBRegionalLCIAFixture'. The "broken" case shows the root-only score
is 0 (the bug); the "fixed" case shows that summing per-DB regional dot
products recovers the right answer.
-}
module CrossDBRegionalLCIASpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U

import Test.Hspec

import CrossDBRegionalLCIAFixture
import Matrix (applyBiosphereMatrix)
import Method.Mapping
import qualified Method.Mapping as MM
import Method.Types (FlowDirection (..), MethodCF (..))
import qualified SharedSolver as SS
import TestHelpers (mkSolverFromDb)
import Types

spec :: Spec
spec = describe "cross-DB regional LCIA" $ do
    -- Fixture used by both expectations:
    --   * Root DB R: 1 activity at FR. No biosphere emission. Cross-DB link
    --     to dep DB D's activity #1 (the one at DE), coefficient 1.0.
    --   * Dep DB D: 3 activities at FR, DE, GLO. D's activity #1 (DE) emits
    --     1.0 kg of biosphere flow F.
    --   * Method M: one regional CF table on F: FR=1, DE=5, GLO=0.5.
    --
    -- Demanding R's activity #0 forces 1 unit of D's activity #1 (DE).
    --   * Dep scaling x_D = [0, 1, 0].
    --   * Dep regional weights w_D = [0, 1·CF[F,DE], 0] = [0, 5, 0].
    --   * Root weights w_R: empty (R has no biosphere triples).
    --   * Cross-DB regional score = w_R·x_R + w_D·x_D = 0 + 5 = 5.
    let fix = mkRegionalFixture
        rootDb = rfRootDb fix
        depDb = rfDepDb fix
        rootTables = rfRootTables fix
        depTables = rfDepTables fix

    it "OLD path: root-only regional score silently misses dep-DB emissions" $ do
        -- Today's contract: computeLCIAScoreAuto is handed the ROOT db's
        -- scaling vector and the ROOT db's MethodTables. The merged
        -- Inventory containing F=1 kg is passed too, but the regional path
        -- ignores it – score = rawWeights_root · scaling_root = 0.
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0 -- root's only activity column
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let inv = SS.csInventory sol
                -- Sanity: dep DB's emission shows up in the merged inventory.
                M.lookup flowUUID inv `shouldBe` Just 1.0
                -- The bug surface: the regional path scored against root
                -- tables/scaling alone returns 0 (or, depending on how the
                -- caller invokes it, the wrong number – never 5).
                let rootScaling = case [s | (n, _, s) <- NE.toList (SS.csScalings sol), n == "root"] of
                        (s : _) -> s
                        [] -> U.empty
                computeRegionalizedLCIAScore
                    kgUnitConfig
                    (dbUnits rootDb)
                    (dbBioFlows rootDb)
                    rootDb
                    rootScaling
                    M.empty
                    rootTables
                    `shouldBe` Right 0.0

    it "NEW path: per-DB regional sum recovers the dep-DB contribution" $ do
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- NE.toList (SS.csScalings sol)
                        ]
                    tablesFor n = case n of
                        "root" -> rootTables
                        "dep" -> depTables
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 5.0

    it "NEW path: a dep-DB integrity error fails the whole sum, never undercounts" $ do
        -- Same solve as the 5.0 case, but the dep DB's tables carry a
        -- genuine integrity error (regional CFs present, precomputed
        -- weights absent – the stale-cache shape). Summing the healthy
        -- root alone would silently return 0 instead of 5; the sum must
        -- refuse instead.
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- NE.toList (SS.csScalings sol)
                        ]
                    tablesFor n = case n of
                        "root" -> rootTables
                        "dep" -> depTables{mtRegionalActivityWeights = Nothing}
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                case sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb of
                    Left _ -> pure ()
                    Right s -> expectationFailure ("expected the integrity error to propagate, got Right " <> show s)

    it "NEW path: tainted dep-DB column contributes 0; root contribution survives" $ do
        -- Same DBs, but the method only has CF[F, FR]. The dep DB's DE
        -- activity is regionalized in the method (F appears in regional
        -- CFs) but no CF resolves at DE / parents / broadcast – that's a
        -- tainted column, and it carries scaling 1. The precomputed
        -- weights leave it at 0 (a coverage gap, not an integrity error),
        -- so the dep DB scores Right 0 and the cross-DB sum stays Right.
        -- The build-time WARN already names the gap per (flow, location)
        -- pair.
        let strictMappings = regionalMappings [("FR", 1)]
            depTablesStrict = buildTables depDb strictMappings
            rootTablesStrict = buildTables rootDb strictMappings
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- NE.toList (SS.csScalings sol)
                        ]
                    tablesFor n = case n of
                        "root" -> rootTablesStrict
                        "dep" -> depTablesStrict
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                -- Root has no biosphere triples → contributes Right 0.
                -- Dep's DE column is tainted → weight 0 → contributes Right 0.
                -- Sum: Right 0.
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 0.0

    it "NEW path: all-tainted case returns partial sum (tainted columns contribute 0)" $ do
        -- Every DB has tainted columns (no Right to fall back to). Each DB
        -- returns Right with tainted-column contribution = 0; the cross-DB
        -- sum is therefore Right 0. Build-time warnings surface the gaps.
        let strictMappings = regionalMappings [("FR", 1)]
            depTablesStrict = buildTables depDb strictMappings
            -- Root variant with a tainted biosphere triple at DE so both
            -- root and dep tables have tainted columns under strict mappings.
            taintedRoot = mkDB 100 ["DE"] [(0, 1.0)]
            taintedRootTables = buildTables taintedRoot strictMappings
        rootSolver <- mkSolverFromDb taintedRoot "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                taintedRoot
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- NE.toList (SS.csScalings sol)
                        ]
                    tablesFor n = case n of
                        "root" -> taintedRootTables
                        "dep" -> depTablesStrict
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 0.0

    describe "non-regression invariants" $ do
        -- These guard the cross-DB regional fix against drift on adjacent
        -- code paths. The numerical proof-points (Right 5.0 / Right 10.0)
        -- live in the gap-and-fix specs above; here we lock in the
        -- properties the fix must preserve.

        it "non-regional methods score identically on a cross-DB recipe" $ do
            -- A method whose CFs are all universal (no consumer location)
            -- never enters the regional cascade. Its score must be
            -- whatever the merged-inventory matvec produces, regardless
            -- of how many DBs participated. Guards against double-counting
            -- in the non-regional code path of 'computeLCIAScoreFromTables'.
            rootSolver <- mkSolverFromDb rootDb "root"
            depSolver <- mkSolverFromDb depDb "dep"
            let depLookup name =
                    pure $
                        if name == "dep" then Just (depDb, depSolver) else Nothing
                -- Universal CF: same value applied wherever F shows up.
                universalCF =
                    MethodCF
                        { mcfFlowRef = flowUUID
                        , mcfFlowName = "Carbon dioxide"
                        , mcfDirection = Output
                        , mcfValue = 3.0
                        , mcfCompartment = Nothing
                        , mcfCAS = Nothing
                        , mcfUnit = "kg"
                        , mcfConsumerLocation = Nothing -- key: makes it non-regional
                        }
                universalTables =
                    buildTables rootDb [(universalCF, Just (testFlow, ByUUID))]
            eRes <-
                SS.computeInventoryMatrixWithDepsCached
                    kgUnitConfig
                    depLookup
                    rootDb
                    "root"
                    rootSolver
                    0
            case eRes of
                Left err -> expectationFailure ("solve failed: " <> show err)
                Right sol -> do
                    -- Merged inventory has F = 1.0 kg (from dep DB).
                    -- Universal CF = 3.0/kg → score = 3.0.
                    let outcome =
                            MM.computeLCIAScoreFromTables
                                kgUnitConfig
                                (dbUnits rootDb)
                                (dbBioFlows rootDb)
                                (SS.csInventory sol)
                                universalTables
                    MM.loScore outcome `shouldBe` 3.0

        it "recipe without any cross-DB reach scores identically to root-only path" $ do
            -- A root DB whose CrossDBLinks don't fire (or aren't there)
            -- must produce a regional score equal to what the OLD
            -- root-only path returned. The cross-DB sum must collapse to
            -- the single-DB dot product, no spurious dep contribution.
            let rootStandalone = mkDB 100 ["FR"] [(0, 1.0)] -- emits 1.0 on root, no links
                standaloneTables =
                    buildTables rootStandalone (regionalMappings [("FR", 7), ("DE", 0)])
            rootSolver <- mkSolverFromDb rootStandalone "root"
            let noDeps _ = pure Nothing
            eRes <-
                SS.computeInventoryMatrixWithDepsCached
                    kgUnitConfig
                    noDeps
                    rootStandalone
                    "root"
                    rootSolver
                    0
            case eRes of
                Left err -> expectationFailure ("solve failed: " <> show err)
                Right sol -> do
                    -- csScalings has only the root entry; cross-DB sum
                    -- reduces to the single-DB dot product. Score =
                    -- 1·CF[F, FR] = 7.
                    let perDb = [(db, sv, standaloneTables) | (_, db, sv) <- NE.toList (SS.csScalings sol)]
                    sumRegionalizedLCIAScoreCrossDB
                        kgUnitConfig
                        (dbUnits rootStandalone)
                        (dbBioFlows rootStandalone)
                        M.empty
                        perDb
                        `shouldBe` Right 7.0

        it "merged inventory equals the sum of per-DB biosphere matvecs" $ do
            -- Property: csInventory is exactly Σ_d applyBiosphereMatrix d s_d
            -- over csScalings. If 'mergeSolutions' ever drifts (drops
            -- entries, double-counts a DB, mis-orders a transpose), this
            -- catches it directly.
            rootSolver <- mkSolverFromDb rootDb "root"
            depSolver <- mkSolverFromDb depDb "dep"
            let depLookup name =
                    pure $
                        if name == "dep" then Just (depDb, depSolver) else Nothing
            eRes <-
                SS.computeInventoryMatrixWithDepsCached
                    kgUnitConfig
                    depLookup
                    rootDb
                    "root"
                    rootSolver
                    0
            case eRes of
                Left err -> expectationFailure ("solve failed: " <> show err)
                Right sol -> do
                    let perDbInvs =
                            [applyBiosphereMatrix db sv | (_, db, sv) <- NE.toList (SS.csScalings sol)]
                        reconstructed =
                            foldr (M.unionWith (+)) M.empty perDbInvs
                    M.toList (SS.csInventory sol)
                        `shouldBe` M.toList reconstructed

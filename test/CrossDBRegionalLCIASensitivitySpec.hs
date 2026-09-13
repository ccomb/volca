{-# LANGUAGE OverloadedStrings #-}

{- | Regression: sensitivity sweeps must score regional methods correctly
on cross-DB recipes.

'postActivitySensitivity' used to project the perturbed root scaling
through 'applyBiosphereMatrix db x' (root-only), then wrap the result in
a root-only 'CrossDBSolution'. The dep-DB emissions induced by the
perturbed root scaling were silently dropped - same under-count as the
plain cross-DB path had before PR #41.

The fix is to thread the perturbed root scaling through
'SharedSolver.goWithDepsFromScalings', which is documented for exactly
this case: "the caller supplies the root scaling vectors, e.g. after a
Sherman-Morrison substitution update." This spec proves that path
produces a 'CrossDBSolution' whose regional score recovers the
dep-DB contribution.

The fixture is shared with 'CrossDBRegionalLCIASpec' so the same
gap-and-fix proof point (regional cross-DB score = 5.0) reads
identically across plain, substitution, and sensitivity paths.
-}
module CrossDBRegionalLCIASensitivitySpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U

import Test.Hspec

import CrossDBRegionalLCIAFixture
import Method.Mapping (sumRegionalizedLCIAScoreCrossDB)
import qualified SharedSolver as SS
import TestHelpers (mkSolverFromDb)
import Types

spec :: Spec
spec = describe "cross-DB regional LCIA via sensitivity propagation" $ do
    let fix = mkRegionalFixture
        rootDb = rfRootDb fix
        depDb = rfDepDb fix
        rootTables = rfRootTables fix
        depTables = rfDepTables fix

    it "goWithDepsFromScalings on a caller-supplied root scaling recovers the dep-DB regional score" $ do
        -- Sensitivity's path:
        --   1. Sherman-Morrison gives a perturbed root scaling x'.
        --   2. Hand x' to goWithDepsFromScalings → full CrossDBSolution.
        --   3. Score with sumRegionalizedLCIAScoreCrossDB.
        --
        -- This test simulates step 2 with x' = baseline (root activity 0
        -- demanded), which is the sensitivity-baseline case. The score
        -- must equal the plain-path baseline (5.0), proving the
        -- perturbed-scaling path picks up dep-DB emissions instead of
        -- silently zeroing them.
        _rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
            -- Caller-supplied root scaling: 1.0 on root's only activity
            -- column. Same shape as what Sherman-Morrison would hand back
            -- for the unperturbed baseline.
            rootScaling = U.fromList [1.0]
        eSol <-
            SS.goWithDepsFromScalings
                kgUnitConfig
                depLookup
                rootDb
                "root"
                []
                [rootScaling]
                0
        case eSol of
            Left err -> expectationFailure ("goWithDepsFromScalings failed: " <> show err)
            Right [] -> expectationFailure "goWithDepsFromScalings returned empty list"
            Right (sol : _) -> do
                -- Both DBs participate, in BFS order: root first, dep second.
                let names = [n | (n, _, _) <- NE.toList (SS.csScalings sol)]
                names `shouldBe` ["root", "dep"]
                -- The merged inventory carries the dep DB's 1.0 kg emission.
                M.lookup flowUUID (SS.csInventory sol) `shouldBe` Just 1.0
                -- Regional score against both DBs' tables = 0 + 5 = 5.
                let perDb =
                        [ (db, sv, tablesFor n)
                        | (n, db, sv) <- NE.toList (SS.csScalings sol)
                        ]
                    tablesFor "root" = rootTables
                    tablesFor "dep" = depTables
                    tablesFor other = error ("unexpected dbName: " <> show other)
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 5.0

    it "scaled root demand scales the dep-DB contribution linearly" $ do
        -- Sanity gate: doubling the root scaling doubles the dep DB's
        -- regional contribution (because the cross-DB link coefficient
        -- is constant). This is the property a sensitivity sweep
        -- depends on: if Sherman-Morrison produces a perturbed scaling
        -- that differs from baseline by Δ, the downstream regional
        -- score must reflect that Δ, not silently flatten it.
        _rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
            doubledRoot = U.fromList [2.0]
        eSol <-
            SS.goWithDepsFromScalings
                kgUnitConfig
                depLookup
                rootDb
                "root"
                []
                [doubledRoot]
                0
        case eSol of
            Left err -> expectationFailure ("goWithDepsFromScalings failed: " <> show err)
            Right [] -> expectationFailure "goWithDepsFromScalings returned empty list"
            Right (sol : _) -> do
                let perDb =
                        [ (db, sv, tablesFor n)
                        | (n, db, sv) <- NE.toList (SS.csScalings sol)
                        ]
                    tablesFor "root" = rootTables
                    tablesFor "dep" = depTables
                    tablesFor other = error ("unexpected dbName: " <> show other)
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 10.0

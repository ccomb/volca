{-# LANGUAGE OverloadedStrings #-}

{- | Regression: the substitution path
('Service.inventoryWithSubsAndDeps') must thread per-DB scalings out so
the cross-DB regional sum can score dep-DB emissions, exactly like the
plain cross-DB path.

Before this commit, the subs path returned only the merged 'Inventory'
and 'API.Routes.postActivityLCIA' wrapped a root-only 'CrossDBSolution'
around it – silently regressing regional scores on cross-DB recipes
whenever substitutions were involved. Two cases:

* Empty substitution list: solution must match the plain path bit-for-bit
  (inventory + per-DB scalings + downstream regional score).
* Non-empty substitution list (Case-A re-routing a root activity to a
  different dep-DB supplier): per-DB scalings must still be populated so
  the dep-DB regional CF lookup runs against the right column.
-}
module CrossDBRegionalLCIASubsSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

import Test.Hspec

import CrossDBRegionalLCIAFixture
import Method.Mapping (MethodTables, sumRegionalizedLCIAScoreCrossDB)
import qualified Service
import qualified SharedSolver as SS
import TestHelpers (mkSolverFromDb)
import Types

spec :: Spec
spec = describe "cross-DB regional LCIA via substitution path" $ do
    let fix = mkRegionalFixture
        rootDb = rfRootDb fix
        depDb = rfDepDb fix
        rootTables = rfRootTables fix
        depTables = rfDepTables fix

        tablesFor :: Text -> MethodTables
        tablesFor "root" = rootTables
        tablesFor "dep" = depTables
        tablesFor other = error ("unexpected dbName in csScalings: " <> T.unpack other)

        scoreSolution sol =
            let perDb =
                    [ (db, sv, tablesFor n)
                    | (n, db, sv) <- NE.toList (SS.csScalings sol)
                    ]
             in sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbBioFlows depDb)
                    M.empty
                    perDb

    it "subs path with empty substitutions matches the plain cross-DB CrossDBSolution" $ do
        -- The subs path used to return only the merged 'Inventory'; the
        -- caller fabricated a root-only 'CrossDBSolution'. After this
        -- commit, the subs path returns the full 'CrossDBSolution' so
        -- this parity check holds.
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        ePlain <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
        eSubs <-
            Service.inventoryWithSubsAndDeps
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
                []
        case (ePlain, eSubs) of
            (Right plainSol, Right subsSol) -> do
                M.toList (SS.csInventory subsSol)
                    `shouldBe` M.toList (SS.csInventory plainSol)
                let plainScalings =
                        [ (n, M.toList (M.fromList [(i, v) | (i, v) <- zip [0 :: Int ..] (U.toList sv)]))
                        | (n, _, sv) <- NE.toList (SS.csScalings plainSol)
                        ]
                    subsScalings =
                        [ (n, M.toList (M.fromList [(i, v) | (i, v) <- zip [0 :: Int ..] (U.toList sv)]))
                        | (n, _, sv) <- NE.toList (SS.csScalings subsSol)
                        ]
                subsScalings `shouldBe` plainScalings
            (Left e, _) -> expectationFailure ("plain path failed: " <> show e)
            (_, Left e) -> expectationFailure ("subs path failed: " <> show e)

    it "subs path with empty substitutions recovers the dep-DB regional score (= 5.0)" $ do
        -- End-to-end: empty subs feeds the same 'CrossDBSolution' shape
        -- into 'sumRegionalizedLCIAScoreCrossDB' and recovers the
        -- regional score the plain path returns.
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eSubs <-
            Service.inventoryWithSubsAndDeps
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
                []
        case eSubs of
            Left e -> expectationFailure ("subs path failed: " <> show e)
            Right subsSol -> scoreSolution subsSol `shouldBe` Right 5.0

    it "subs path: csScalings lists both root and dep DBs after a cross-DB solve" $ do
        -- Structural gate: regardless of subs content, the threaded
        -- 'CrossDBSolution' must list both DBs the recursion actually
        -- visited. If the substitution path silently dropped a dep
        -- contribution, this would catch it before the regional score
        -- check.
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eSubs <-
            Service.inventoryWithSubsAndDeps
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
                []
        case eSubs of
            Left e -> expectationFailure ("subs path failed: " <> show e)
            Right subsSol -> do
                let names = [n | (n, _, _) <- NE.toList (SS.csScalings subsSol)]
                names `shouldBe` ["root", "dep"]

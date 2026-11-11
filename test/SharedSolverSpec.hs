{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedSolverSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.Manager (solverFor)
import GoldenData
import Matrix (Demand (..), buildDemandVector)
import qualified Service
import SharedSolver (SharedSolver, createSharedSolver, factorized, solveWithSharedSolver)
import qualified SharedSolver as SS
import Test.Hspec
import TestHelpers (assertVectorNear, linkDatabases, loadSampleDatabase, mkActivity, mkDepLookupFromMap, mkSolverFromDb, mkTechFlow, reference, techInput, units)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- Lazy factorization
    -- -----------------------------------------------------------------------
    describe "lazy factorization" $ do
        it "a solver has factorized nothing before its first solve" $ do
            solver <- createSharedSolver "test" [] 0
            factorized solver `shouldReturn` False

        it "a solver has factorized after its first solve (SAMPLE.min3)" $ do
            (solver, _) <- min3Solver
            factorized solver `shouldReturn` True

        it "a solver of no activity answers no column" $ do
            solver <- createSharedSolver "test" [] 0
            SS.solveMultiWithSharedSolver solver [Demand U.empty] `shouldReturn` Right [U.empty]

    describe "a technosphere that cannot be solved" $ do
        -- Each of the two activities takes one unit of the other to make one
        -- unit: together they consume all of what they produce.
        let loop = [(0, 1, 1), (1, 0, 1)]
        it "is refused to a request by the name of an activity of the loop" $ do
            db <- wheatAndFlour
            solver <- solverFor "loop" db
            Service.computeSensitivities db solver 0 [] >>= \case
                Left (Service.MatrixError said) -> said `shouldSatisfy` (\s -> any (`T.isInfixOf` s) ["wheat", "flour"] && "singular" `T.isInfixOf` s)
                Left other -> expectationFailure (show other)
                Right _ -> expectationFailure "a technosphere that cannot be solved was answered"

        it "is refused again without factorizing again" $ do
            solver <- createSharedSolver "loop" loop 2
            first <- solveWithSharedSolver solver (Demand (U.fromList [1, 0]))
            factorized solver `shouldReturn` True
            solveWithSharedSolver solver (Demand (U.fromList [0, 1])) `shouldReturn` first
            first `shouldSatisfy` either ("column " `T.isInfixOf`) (const False)

    -- -----------------------------------------------------------------------
    -- Correctness: solve result matches golden values
    -- -----------------------------------------------------------------------
    describe "solve correctness (SAMPLE.min3)" $ do
        it "scaling vector matches golden values [1.0, 0.6, 0.24]" $ do
            (solver, db) <- min3Solver
            let demandVec = buildDemandVector db 0
            result <- either (fail . show) pure =<< solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            assertVectorNear "scaling vector" defaultTolerance result sampleMin3ExpectedSupply

    -- -----------------------------------------------------------------------
    -- Second solve uses cached factorization
    -- -----------------------------------------------------------------------
    describe "caching" $ do
        it "second solve produces the same result" $ do
            (solver, db) <- min3Solver
            let demandVec = buildDemandVector db 0
            result1 <- either (fail . show) pure =<< solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            result2 <- either (fail . show) pure =<< solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            U.toList result1 `shouldBe` U.toList result2

    -- -----------------------------------------------------------------------
    -- Which databases a cross-database solve reaches, and in what order
    -- -----------------------------------------------------------------------
    describe "the databases a cross-database solve reaches" $ do
        -- R buys from b and from a, and a buys from c: each dependency is
        -- followed by its own chain, and siblings come in name order.
        (rootDb, lookupDep, rootSolver) <- runIO chainOfFour
        it "lists them root first, each dependency followed by its own chain" $ do
            sols <- SS.goWithDepsFromScalings defaultUnitConfig lookupDep rootDb "R" [] [U.fromList [1]] 0
            map reachedNames <$> either (fail . show) pure sols `shouldReturn` [["R", "a", "c", "b"]]

        it "lists them in the same order on the substitution path" $ do
            sol <- Service.inventoryWithSubsAndDeps defaultUnitConfig lookupDep rootDb "R" rootSolver 0 []
            reachedNames <$> either (fail . show) pure sol `shouldReturn` ["R", "a", "c", "b"]

reachedNames :: SS.CrossDBSolution -> [Text]
reachedNames sol = [name | SS.Reached name _ _ <- NE.toList (SS.csScalings sol)]

{- | Four one-activity databases: R links to b and to a, a links to c. Gives
the root, a lookup for the other three, and the root's solver.
-}
chainOfFour :: IO (Database, SS.DepSolverLookup, SharedSolver)
chainOfFour = do
    (r, a, b, c) <- (,,,) <$> oneActivity 1 <*> oneActivity 2 <*> oneActivity 3 <*> oneActivity 4
    let a' = linkDatabases a c "c" 1
        r' = linkDatabases (linkDatabases r b "b" 1) a' "a" 1
    deps <- mapM (\(name, db) -> (\solver -> (name, (db, solver))) <$> mkSolverFromDb db name) [("a", a'), ("b", b), ("c", c)]
    rootSolver <- mkSolverFromDb r' "R"
    pure (r', mkDepLookupFromMap (M.fromList deps), rootSolver)
  where
    oneActivity :: Word -> IO Database
    oneActivity n =
        let act = UUID.fromWords 0 0 0 (fromIntegral n)
            flow = UUID.fromWords 0 0 1 (fromIntegral n)
            sdb =
                SimpleDatabase
                    { sdbActivities = M.singleton (act, flow) (mkActivity "P" [reference flow])
                    , sdbTechFlows = M.singleton flow (mkTechFlow flow "P")
                    , sdbBioFlows = M.empty
                    , sdbWasteFlows = M.empty
                    , sdbUnits = units
                    , sdbDocumentation = noDocumentation
                    , sdbParameters = M.empty
                    , sdbProductSystems = []
                    , sdbReferenceConvention = SignedReferences
                    }
         in buildDatabaseWithMatrices (BuildInputs defaultUnitConfig M.empty ReadDeclared []) sdb >>= either (fail . show) pure

{- | Two activities, each taking a kilogram of the other's product to make a
kilogram of its own.
-}
wheatAndFlour :: IO Database
wheatAndFlour =
    buildDatabaseWithMatrices (BuildInputs defaultUnitConfig M.empty ReadDeclared []) sdb >>= either (fail . show) pure
  where
    (wheat, flour) = (UUID.fromWords 0 0 1 1, UUID.fromWords 0 0 1 2)
    (grower, miller) = (UUID.fromWords 0 0 0 1, UUID.fromWords 0 0 0 2)
    from maker product = (techInput product 1){techActivityLinkId = Just maker, techSupplierClaim = ClaimById maker}
    sdb =
        SimpleDatabase
            { sdbActivities =
                M.fromList
                    [ ((grower, wheat), mkActivity "wheat" [reference wheat, from miller flour])
                    , ((miller, flour), mkActivity "flour" [reference flour, from grower wheat])
                    ]
            , sdbTechFlows = M.fromList [(wheat, mkTechFlow wheat "wheat"), (flour, mkTechFlow flour "flour")]
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = units
            , sdbDocumentation = noDocumentation
            , sdbParameters = M.empty
            , sdbProductSystems = []
            , sdbReferenceConvention = SignedReferences
            }

-- ---------------------------------------------------------------------------
-- Helper: build solver + DB for SAMPLE.min3, with factorization pre-triggered
-- ---------------------------------------------------------------------------

min3Solver :: IO (SharedSolver, Database)
min3Solver = do
    db <- loadSampleDatabase "SAMPLE.min3"
    let techTriples =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
            ]
        actCount = fromIntegral (dbActivityCount db)
    solver <- createSharedSolver "SAMPLE.min3" techTriples actCount
    -- Trigger lazy factorization via first solve
    let demandVec = buildDemandVector db 0
    _ <- either (fail . show) pure =<< solveWithSharedSolver solver =<< either (fail . show) pure demandVec
    return (solver, db)

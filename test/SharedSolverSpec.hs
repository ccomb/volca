{-# LANGUAGE OverloadedStrings #-}

module SharedSolverSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Database (buildDatabaseWithMatrices)
import GoldenData
import Matrix (Demand (..), buildDemandVector)
import qualified Service
import SharedSolver (SharedSolver, createSharedSolver, getFactorization, solveWithSharedSolver)
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
        it "getFactorization returns Nothing before first solve" $ do
            solver <- createSharedSolver "test" U.empty 0
            result <- getFactorization solver
            case result of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing before first solve"

        it "getFactorization returns Just after first solve (SAMPLE.min3)" $ do
            (solver, _) <- min3Solver
            fact <- getFactorization solver
            case fact of
                Just _ -> return ()
                Nothing -> expectationFailure "Expected factorization to be cached after solve"

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

    describe "solves asked at the same time" $
        it "answer as the same solves asked in turn, whichever of them factorises" $ do
            -- Two hundred processes in one loop, each buying from two of the others.
            let n = 200
                ring = U.fromList (concat [[SparseTriple (fromIntegral ((i + 1) `mod` n)) (fromIntegral i) 0.3, SparseTriple (fromIntegral ((i + 7) `mod` n)) (fromIntegral i) 0.2] | i <- [0 .. n - 1]])
                demands = [Demand (U.generate n (\i -> if i `mod` (k + 2) == 0 then 1 else 0)) | k <- [0 .. 19]]
            inTurn <- createSharedSolver "ring" ring n >>= \solver -> traverse (solveWithSharedSolver solver) demands
            together <- createSharedSolver "ring" ring n >>= \solver -> mapConcurrently (solveWithSharedSolver solver) demands
            inTurn `shouldSatisfy` all isRight
            together `shouldBe` inTurn

    describe "a demand that reaches processes with no answer" $ do
        db <- runIO loopedDb
        solver <- runIO (mkSolverFromDb db "looped")
        it "is refused as an error of the request, in a sentence naming the database and a process of the loop" $ do
            answer <- Service.supplyFor db solver =<< demandOf db 3
            case answer of
                Left (Service.MatrixError why) -> do
                    why `shouldSatisfy` T.isPrefixOf "Database 'looped' has no answer to this demand: "
                    why `shouldSatisfy` \sentence -> any (`T.isInfixOf` sentence) ["\"left\" (FR)", "\"right\" (FR)"]
                Left other -> expectationFailure ("refused for another reason: " <> show other)
                Right supply -> expectationFailure ("answered " <> show supply)
        it "is answered when it does not reach them" $ do
            demand <- demandOf db 4
            answer <- Service.supplyFor db solver demand
            either (const Nothing) Just answer `shouldBe` Just (unDemand demand)
        it "refuses only itself when asked beside demands that do not reach them" $ do
            demands <- traverse (demandOf db) [3, 4, 1]
            answers <- SS.solveMultiWithSharedSolver solver demands
            case (answers, demands) of
                ([Left _, Right apart, Left _], [_, asked, _]) -> apart `shouldBe` unDemand asked
                _ -> expectationFailure ("expected the second answered and the others refused: " <> show (map isRight answers))

    describe "a batch whose roots reach another database" $
        it "refuses the root that reaches a loop there, and answers the other" $ do
            -- The root's first process buys from "left" of the looped database; its second buys nothing.
            looped <- loopedDb
            loopedSolver <- mkSolverFromDb looped "looped"
            root <- (\db -> linkDatabases db looped "looped" 1) <$> handBuilt (M.fromList [(1, mkActivity "consumer" [reference (made 1)]), (2, mkActivity "apart" [reference (made 2)])])
            rootSolver <- mkSolverFromDb root "R"
            pids <- traverse (pidOf root) [1, 2]
            sols <- SS.computeInventoryMatrixBatchWithDepsCached defaultUnitConfig (mkDepLookupFromMap (M.singleton "looped" (looped, loopedSolver))) root "R" rootSolver pids
            case sols of
                [Left why, Right _] -> why `shouldSatisfy` T.isPrefixOf "Database 'looped' has no answer to this demand: "
                _ -> expectationFailure ("expected the first refused and the second answered: " <> show (map (either Just (const Nothing)) sols))

    -- -----------------------------------------------------------------------
    -- Which databases a cross-database solve reaches, and in what order
    -- -----------------------------------------------------------------------
    describe "the databases a cross-database solve reaches" $ do
        -- R buys from b and from a, and a buys from c: each dependency is
        -- followed by its own chain, and siblings come in name order.
        (rootDb, lookupDep, rootSolver) <- runIO chainOfFour
        it "lists them root first, each dependency followed by its own chain" $ do
            sols <- SS.goWithDepsFromScalings defaultUnitConfig lookupDep rootDb "R" [] [Right (U.fromList [1])] 0
            map reachedNames <$> either (fail . show) pure (sequence sols) `shouldReturn` [["R", "a", "c", "b"]]

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
    oneActivity :: Word32 -> IO Database
    oneActivity n = handBuilt (M.singleton n (mkActivity "P" [reference (made n)]))

-- | A database of hand-written processes, each under a number, making the product of that number.
handBuilt :: M.Map Word32 Activity -> IO Database
handBuilt processes =
    buildDatabaseWithMatrices (BuildInputs defaultUnitConfig M.empty ReadDeclared []) sdb >>= either (fail . show) pure
  where
    sdb :: SimpleDatabase
    sdb =
        SimpleDatabase
            { sdbActivities = M.mapKeys (\n -> (maker n, made n)) processes
            , sdbTechFlows = M.fromList [(made n, mkTechFlow (made n) (activityName process)) | (n, process) <- M.toList processes]
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = units
            , sdbDocumentation = noDocumentation
            , sdbParameters = M.empty
            , sdbProductSystems = []
            , sdbReferenceConvention = SignedReferences
            }

maker, made :: Word32 -> UUID.UUID
maker = UUID.fromWords 0 0 0
made = UUID.fromWords 0 0 1

-- | An amount of what the process of a number makes, bought from it.
boughtFrom :: Word32 -> Double -> Exchange
boughtFrom n amount = (techInput (made n) amount){techActivityLinkId = Just (maker n), techSupplierClaim = ClaimById (maker n)}

-- | One unit of what the process of a number makes.
demandOf :: Database -> Word32 -> IO Demand
demandOf db n = either (fail . show) pure . buildDemandVector db =<< pidOf db n

-- | The process of a number.
pidOf :: Database -> Word32 -> IO ProcessId
pidOf db n = maybe (fail ("no process " <> show n)) pure (findProcessId db (maker n) (made n))

-- | "left" and "right" each take all of what the other makes; "buyer" buys from "left", "apart" buys nothing.
loopedDb :: IO Database
loopedDb = handBuilt (M.fromList [(1, mkActivity "left" [reference (made 1), boughtFrom 2 1]), (2, mkActivity "right" [reference (made 2), boughtFrom 1 1]), (3, mkActivity "buyer" [reference (made 3), boughtFrom 1 0.5]), (4, mkActivity "apart" [reference (made 4)])])

-- ---------------------------------------------------------------------------
-- Helper: build solver + DB for SAMPLE.min3, with factorization pre-triggered
-- ---------------------------------------------------------------------------

min3Solver :: IO (SharedSolver, Database)
min3Solver = do
    db <- loadSampleDatabase "SAMPLE.min3"
    let techTriples = dbTechnosphereTriples db
        actCount = fromIntegral (dbActivityCount db)
    solver <- createSharedSolver "SAMPLE.min3" techTriples actCount
    -- Trigger lazy factorization via first solve
    let demandVec = buildDemandVector db 0
    _ <- solveWithSharedSolver solver =<< either (fail . show) pure demandVec
    return (solver, db)

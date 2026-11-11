{-# LANGUAGE OverloadedStrings #-}

{- | Requests solving with one solver at the same time: each gets the answer a
dense elimination of the same system gives, whether or not the solver had
factorized when it arrived. On the three activities of SAMPLE.min3.
-}
module ConcurrentSolveSpec (spec) where

import Control.Concurrent.Async (concurrently, mapConcurrently)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import Matrix (Demand (..), Vector, fromList, readSolveCounter, solveSparseLinearSystem)
import Matrix.Dense (factorLU, solveLU)
import SharedSolver (SharedSolver, solveMultiWithSharedSolver, solveWithSharedSolver)
import Test.Hspec
import TestHelpers (loadSampleDatabase, mkSolverFromDb)
import Types

spec :: Spec
spec = do
    describe "requests solving with one solver at the same time" $ do
        it "50 arriving before it has factorized all get the expected answer" $ do
            (solver, db) <- min3Solver
            let demands = take 50 (cycle (basicDemands db))
            actual <- sequence <$> mapConcurrently (solveWithSharedSolver solver . Demand) demands
            actual `shouldSatisfy` closeTo (expected db demands)

        it "one asking for several demands gets their answers in their order" $ do
            (solver, db) <- min3Solver
            let demands = take 10 (cycle (basicDemands db))
            actual <- solveMultiWithSharedSolver solver (map Demand demands)
            actual `shouldSatisfy` closeTo (expected db demands)

        it "one asking for more demands than a call to the solver takes gets them all" $ do
            (solver, db) <- min3Solver
            let demands = take 150 (cycle (basicDemands db))
            actual <- solveMultiWithSharedSolver solver (map Demand demands)
            actual `shouldSatisfy` closeTo (expected db demands)

        it "those asking for one demand and one asking for several do not disturb each other" $ do
            (solver, db) <- min3Solver
            let singles = take 20 (cycle (basicDemands db))
                several = take 6 (cycle (basicDemands db))
            (actualSingles, actualSeveral) <-
                concurrently
                    (sequence <$> mapConcurrently (solveWithSharedSolver solver . Demand) singles)
                    (solveMultiWithSharedSolver solver (map Demand several))
            actualSingles `shouldSatisfy` closeTo (expected db singles)
            actualSeveral `shouldSatisfy` closeTo (expected db several)

    -- A server that shuts itself down when idle watches this number to know
    -- that work is under way. If a solve path stopped moving it, an analysis
    -- longer than the timeout would be killed halfway through.
    describe "solve counter" $ do
        it "moves on a solve for one demand, for several, and on a solve from the technosphere" $ do
            (solver, db) <- min3Solver
            let demand = unitOn (activityCount db) 0
            before <- readSolveCounter
            _ <- solveWithSharedSolver solver (Demand demand)
            afterOne <- readSolveCounter
            _ <- solveMultiWithSharedSolver solver [Demand demand, Demand demand]
            afterSeveral <- readSolveCounter
            _ <- solveSparseLinearSystem (technosphere db) (activityCount db) demand
            afterFresh <- readSolveCounter
            [afterOne > before, afterSeveral > afterOne, afterFresh > afterSeveral]
                `shouldBe` [True, True, True]

-- | SAMPLE.min3 and a solver for it that has not factorized yet.
min3Solver :: IO (SharedSolver, Database)
min3Solver = do
    db <- loadSampleDatabase "SAMPLE.min3"
    solver <- mkSolverFromDb db "SAMPLE.min3.concurrent"
    pure (solver, db)

-- | A few demands of different shapes; cycling through them gives many.
basicDemands :: Database -> [Vector]
basicDemands db =
    [ unitOn n 0
    , unitOn n 1
    , unitOn n 2
    , U.map (* 2.5) (unitOn n 0)
    , U.map (* 0.7) (unitOn n 1)
    , U.zipWith (+) (unitOn n 0) (unitOn n 2)
    ]
  where
    n = activityCount db

unitOn :: Int -> Int -> Vector
unitOn n i = fromList [if k == i then 1.0 else 0.0 | k <- [0 .. n - 1]]

activityCount :: Database -> Int
activityCount = fromIntegral . dbActivityCount

technosphere :: Database -> [(Int, Int, Double)]
technosphere db = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]

{- | What a dense elimination with row exchanges answers for each demand: another
algorithm than the solver under test, on the same system.
-}
expected :: Database -> [Vector] -> Either Text [Vector]
expected db demands = (\lu -> map (solveLU lu) demands) <$> factorLU n (U.accum (+) identity [(i * n + j, -v) | (i, j, v) <- technosphere db])
  where
    n = activityCount db
    identity = U.generate (n * n) (\k -> if k `mod` (n + 1) == 0 then 1 else 0)

-- | Whether a solve answered what was expected, to the tolerance of the golden data.
closeTo :: Either Text [Vector] -> Either Text [Vector] -> Bool
closeTo (Right wanted) (Right got) =
    length wanted == length got && and (zipWith (\w g -> U.length w == U.length g && U.and (U.zipWith (\a b -> abs (a - b) < 1.0e-9) w g)) wanted got)
closeTo _ _ = False

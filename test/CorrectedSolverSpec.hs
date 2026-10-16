{-# LANGUAGE OverloadedStrings #-}

{- | A solver corrected by a few columns of its matrix answers as a new
factorisation of the changed matrix would.
-}
module CorrectedSolverSpec (spec) where

import Control.Exception (bracket)
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector.Unboxed as U
import Matrix (Demand (..), clearCachedSolver)
import Matrix.Dense (factorLU, solveLU)
import SharedSolver (SharedSolver, correctedFor, createSharedSolver, solveMultiWithSharedSolver, solveWithSharedSolver)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "a dense system" $ do
        it "solves a system whose first pivot is zero" $
            -- [[0, 2], [3, 1]] x = [4, 5] gives x = [1, 2].
            (U.toList . (`solveLU` U.fromList [4, 5]) <$> factorLU 2 (U.fromList [0, 2, 3, 1])) `shouldBe` Right [1, 2]
        it "refuses a singular system" $
            isLeft (factorLU 2 (U.fromList [1, 2, 2, 4])) `shouldBe` True

    describe "a solver corrected by the columns that moved" $ do
        it "answers as a new factorisation of the changed matrix" $
            withNumTests 50 . forAll changedMatrix $ \(n, before, after, moved, demands) -> ioProperty $
                withSolver n before $ \loaded -> withSolver n after $ \fresh -> do
                    corrected <- either (fail . show) pure =<< correctedFor loaded (change before after moved)
                    expected <- solveMultiWithSharedSolver fresh (map Demand demands)
                    got <- solveMultiWithSharedSolver corrected (map Demand demands)
                    one <- traverse (solveWithSharedSolver corrected . Demand) demands
                    pure (conjoin [close e g | (e, g) <- zip expected got <> zip expected one])

        it "refuses a change that makes the matrix singular" $
            -- M = I; taking 1 off the first column's diagonal leaves it empty.
            withSolver 2 M.empty $
                \loaded -> isLeft <$> correctedFor loaded [(0, [(0, -1)])] `shouldReturn` True
  where
    close :: U.Vector Double -> U.Vector Double -> Property
    close expected got =
        counterexample (show (expected, got)) $
            U.and (U.zipWith (\e g -> abs (e - g) <= 1e-9 * max 1 (abs e)) expected got)

-- | A matrix A (off the diagonal), the same with some columns changed, which, and right-hand sides.
type Case = (Int, M.Map (Int, Int) Double, M.Map (Int, Int) Double, [Int], [U.Vector Double])

changedMatrix :: Gen Case
changedMatrix = do
    n <- chooseInt (2, 7)
    let entries = M.fromList . concat <$> traverse (\(i, j) -> frequency [(2, pure []), (1, (\v -> [((i, j), v)]) <$> choose (-0.15, 0.15))]) [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i /= j]
    before <- entries
    replacement <- entries
    moved <- sublistOf [0 .. n - 1] `suchThat` (not . null)
    let after = M.union (M.filterWithKey (\(_, j) _ -> j `elem` moved) replacement) (M.filterWithKey (\(_, j) _ -> j `notElem` moved) before)
    demands <- listOf1 (U.fromList <$> vectorOf n (choose (-1, 1)))
    pure (n, before, after, moved, demands)

-- | What moving these columns of A adds to M = I − A, column by column.
change :: M.Map (Int, Int) Double -> M.Map (Int, Int) Double -> [Int] -> [(Int, [(Int, Double)])]
change before after moved =
    [ (j, [(i, M.findWithDefault 0 (i, j) before - M.findWithDefault 0 (i, j) after) | i <- rows j])
    | j <- moved
    ]
  where
    rows j = [i | (i, j') <- M.keys (M.union before after), j' == j]

-- | A solver of I − A under a name of its own, its factorisation destroyed afterwards.
withSolver :: Int -> M.Map (Int, Int) Double -> (SharedSolver -> IO a) -> IO a
withSolver n entries act = do
    name <- ("corrected-" <>) . UUID.toText <$> nextRandom
    bracket (solver name) (const (clearCachedSolver name)) act
  where
    solver :: Text -> IO SharedSolver
    solver name = createSharedSolver name [(i, j, v) | ((i, j), v) <- M.toList entries] n

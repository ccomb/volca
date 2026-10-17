{-# LANGUAGE OverloadedStrings #-}

{- | A small dense square system, factorised once and solved many times.

For the k×k system of a low-rank correction, where k is the number of columns
a request moves: small enough that a dense elimination is the cheapest way.
-}
module Matrix.Dense (
    LU,
    factorLU,
    solveLU,
) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Foldable (foldlM)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | A square matrix as P·L·U, by Gaussian elimination with partial pivoting.
data LU
    = LU
        -- | The size
        !Int
        -- | L below the diagonal (its unit diagonal implied) and U on and above, row by row
        !(U.Vector Double)
        -- | The original row each factored row came from
        !(U.Vector Int)

{- | Factorise the @n@×@n@ matrix given row by row, or refuse when it is
singular: its determinant below 1e-12 in magnitude. For the system of a
correction, the determinant is the changed matrix's over the loaded one's, the
same whatever unit each process is counted in, where the size of a pivot is
not: a process counted in grams has entries a thousand times those of one
counted in kilograms, though nothing else changed.
-}
factorLU :: Int -> U.Vector Double -> Either Text LU
factorLU n entries
    | U.length entries /= n * n = Left "a dense system needs n×n entries"
    | not (U.all isFinite entries) = Left "the system has an entry that is not a finite number"
    | otherwise = case U.createT eliminate of
        (Just rows, factors) | regular factors -> Right (LU n factors rows)
        _ -> Left "the system is singular"
  where
    isFinite :: Double -> Bool
    isFinite x = not (isNaN x || isInfinite x)
    -- In logarithms, so that k pivots of a thousand do not overflow.
    regular :: U.Vector Double -> Bool
    regular factors = let logDet = sum [log (abs (factors U.! (i * n + i))) | i <- [0 .. n - 1]] in isFinite logDet && logDet >= log 1e-12

    eliminate :: ST s (Maybe (U.Vector Int), MU.MVector s Double)
    eliminate = do
        a <- U.thaw entries
        rows <- U.thaw (U.enumFromN 0 n)
        eliminated <- foldlM (\ok col -> if ok then step a rows col else pure False) True [0 .. n - 1]
        frozen <- U.freeze rows
        pure (if eliminated then Just frozen else Nothing, a)

    step :: MU.MVector s Double -> MU.MVector s Int -> Int -> ST s Bool
    step a rows col = do
        (size, pivotRow) <- foldlM (\best r -> max best . (\v -> (abs v, r)) <$> MU.read a (r * n + col)) (-1, col) [col .. n - 1]
        if size == 0
            then pure False
            else do
                when (pivotRow /= col) $ do
                    forM_ [0 .. n - 1] $ \c -> MU.swap a (col * n + c) (pivotRow * n + c)
                    MU.swap rows col pivotRow
                pivot <- MU.read a (col * n + col)
                forM_ [col + 1 .. n - 1] $ \r -> do
                    factor <- (/ pivot) <$> MU.read a (r * n + col)
                    MU.write a (r * n + col) factor
                    forM_ [col + 1 .. n - 1] $ \c -> do
                        above <- MU.read a (col * n + c)
                        MU.modify a (subtract (factor * above)) (r * n + c)
                pure True

-- | Solve the factorised system for one right-hand side.
solveLU :: LU -> U.Vector Double -> U.Vector Double
solveLU (LU n factors rows) b = backward (forward (U.backpermute b rows))
  where
    at :: Int -> Int -> Double
    at r c = factors U.! (r * n + c)
    forward :: U.Vector Double -> U.Vector Double
    forward y0 = U.create $ do
        y <- U.thaw y0
        forM_ [0 .. n - 1] $ \r ->
            forM_ [0 .. r - 1] $ \c -> do
                yc <- MU.read y c
                MU.modify y (subtract (at r c * yc)) r
        pure y
    backward :: U.Vector Double -> U.Vector Double
    backward y0 = U.create $ do
        x <- U.thaw y0
        forM_ [n - 1, n - 2 .. 0] $ \r -> do
            forM_ [r + 1 .. n - 1] $ \c -> do
                xc <- MU.read x c
                MU.modify x (subtract (at r c * xc)) r
            MU.modify x (/ at r r) r
        pure x

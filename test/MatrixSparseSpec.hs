{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MatrixSparseSpec
Description : The sparse solver answers as a dense elimination does, and refuses what it cannot solve.

Each build links the solver archive its own way: fully static against musl,
against the system's C library, through MSYS2 on Windows. A link that succeeds
can still leave the archive unable to allocate or to unwind, and the second
only shows the day something panics: the probe asks the archive itself.
-}
module MatrixSparseSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import Matrix.Dense (factorLU, solveLU)
import Matrix.Sparse (Factors, Refused (..), factorize, probe, solve)
import Test.Hspec
import Test.QuickCheck
import Types (SparseTriple (..))

spec :: Spec
spec = do
    describe "probe" $
        it "finds the linked solver archive sound" $
            probe `shouldReturn` Right ()

    describe "the sparse solver" $ do
        it "answers as a dense elimination of the same matrix" $
            forAll system $ \(n, entries, demands) -> ioProperty $ do
                factors <- accepted n entries
                got <- solved factors demands
                expected <- dense n entries demands
                pure (conjoin (zipWith close expected got))

        it "answers as a dense elimination of a matrix with no zero in it" $ do
            -- Wide and full enough to be eliminated as one block rather than column by column.
            factors <- accepted wide full
            got <- solved factors [ramp]
            expected <- dense wide full [ramp]
            and (zipWith (\e g -> U.and (U.zipWith near e g)) expected got) `shouldBe` True

        it "solves several right-hand sides at once as it solves each alone" $ do
            factors <- accepted wide full
            together <- solved factors several
            alone <- concat <$> traverse (solved factors . pure) several
            together `shouldBe` alone

        it "solves for many requests at the same time" $ do
            factors <- accepted wide full
            alone <- solved factors several
            together <- mapConcurrently (const (solved factors several)) [1 :: Int .. 50]
            together `shouldBe` replicate 50 alone

        it "solves a matrix with no column" $ do
            factors <- accepted 0 []
            solve factors [U.empty] `shouldReturn` Right [U.empty]

        it "refuses two columns that cancel each other, and names one of them" $ do
            -- x + y = a and x + y = b: nothing is left of the second column once the first is eliminated.
            refused <- verdict 2 [(0, 0, 1), (0, 1, 1), (1, 0, 1), (1, 1, 1)]
            refused `shouldSatisfy` (`elem` [Right (Left (TinyPivot column 0)) | column <- [0, 1]])

        it "says which entry lies outside the matrix" $ do
            verdict 2 [(0, 0, 1), (1, 1, 1), (2, 0, 0.5)]
                `shouldReturn` Left "entry 2 of the matrix names a row or a column outside it, or holds no finite number"
            verdict 2 [(0, 0, 1), (0, -1, 0.5)]
                `shouldReturn` Left "entry 1 of the matrix names a row or a column outside it, or holds no finite number"

        it "refuses a right-hand side of another length than the matrix" $ do
            factors <- accepted 2 [(0, 0, 1), (1, 1, 1)]
            solve factors [U.fromList [1, 2], U.fromList [1, 2, 3]]
                `shouldReturn` Left "a right-hand side needs one number for each of the 2 columns of the matrix"
  where
    close :: U.Vector Double -> U.Vector Double -> Property
    close expected got = counterexample (show (expected, got)) (U.and (U.zipWith near expected got))

    near :: Double -> Double -> Bool
    near e g = abs (e - g) <= 1e-9 * max 1 (abs e)

-- | Entries of a matrix: row, column, value.
type Entries = [(Int, Int, Double)]

wide :: Int
wide = 300

-- | Ones on the diagonal, and everywhere else a small term that depends on the place.
full :: Entries
full = [(i, j, if i == j then 1 else fromIntegral ((i * 7 + j * 13) `mod` 11) / 400 - 0.01) | i <- [0 .. wide - 1], j <- [0 .. wide - 1]]

ramp :: U.Vector Double
ramp = U.generate wide (\i -> fromIntegral i / 100 - 1)

-- | More right-hand sides than the solver takes side by side.
several :: [U.Vector Double]
several = [U.map (* fromIntegral k) ramp | k <- [1 :: Int .. 11]]

-- | Ones on the diagonal and a few small terms elsewhere, as @I − A@ has, with right-hand sides for it.
system :: Gen (Int, Entries, [U.Vector Double])
system = do
    n <- chooseInt (2, 7)
    off <- concat <$> traverse (\(i, j) -> frequency [(2, pure []), (1, (\v -> [(i, j, v)]) <$> choose (-0.15, 0.15))]) [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i /= j]
    demands <- listOf1 (U.fromList <$> vectorOf n (choose (-1, 1)))
    pure (n, [(i, i, 1) | i <- [0 .. n - 1]] <> off, demands)

-- | What the solver says of a matrix, the factors left aside.
verdict :: Int -> Entries -> IO (Either Text (Either Refused ()))
verdict n entries = fmap void <$> factorize n (triples entries)

-- | The factors of a matrix the solver has to accept.
accepted :: Int -> Entries -> IO Factors
accepted n entries = either (fail . show) (either (fail . show) pure) =<< factorize n (triples entries)

triples :: Entries -> U.Vector SparseTriple
triples entries = U.fromList [SparseTriple (fromIntegral i) (fromIntegral j) v | (i, j, v) <- entries]

solved :: Factors -> [U.Vector Double] -> IO [U.Vector Double]
solved factors demands = either (fail . show) pure =<< solve factors demands

-- | The same answers by dense elimination with row exchanges.
dense :: Int -> Entries -> [U.Vector Double] -> IO [U.Vector Double]
dense n entries demands =
    either (fail . show) (\lu -> pure (map (solveLU lu) demands)) $
        factorLU n (U.accum (+) (U.replicate (n * n) 0) [(i * n + j, v) | (i, j, v) <- entries])

{-# LANGUAGE OverloadedStrings #-}

{- | Pure numeric edge cases for the matrix pipeline.

These exercise the *pure* helpers in "Matrix" – the ones that run without
touching MUMPS – so we can keep them in the fast unit-test tier. The
MUMPS-backed paths are already covered by MatrixConstructionSpec /
CoalescingSolverSpec via the per-database loaders.
-}
module NumericEdgesSpec (spec) where

import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Matrix (applyShermanMorrison, applySparseMatrix)

spec :: Spec
spec = do
    describe "applySparseMatrix (y = A * x)" $ do
        it "returns the zero vector on an empty matrix" $ do
            let y = applySparseMatrix [] 3 (U.fromList [1.0, 2.0, 3.0])
            U.toList y `shouldBe` [0.0, 0.0, 0.0]

        it "computes y = I*x correctly for the identity matrix" $ do
            let i3 = [(0, 0, 1.0), (1, 1, 1.0), (2, 2, 1.0)]
                y = applySparseMatrix i3 3 (U.fromList [4.0, 5.0, 6.0])
            U.toList y `shouldBe` [4.0, 5.0, 6.0]

        it "sums contributions from multiple non-zeros on the same row" $ do
            -- y[0] = 2*x[0] + 3*x[1] = 2*1 + 3*1 = 5
            let m = [(0, 0, 2.0), (0, 1, 3.0)]
                y = applySparseMatrix m 1 (U.fromList [1.0, 1.0])
            U.toList y `shouldBe` [5.0]

        it "silently skips out-of-bound column indices (defensive, no crash)" $ do
            -- A triple referencing j=5 on a 2-wide vector is dropped, not crashed.
            -- This documents the boundary behaviour – it's defensive, not 'lossy
            -- on real data': the caller guarantees j < length(x) by construction.
            let m = [(0, 0, 1.0), (0, 5, 99.0)]
                y = applySparseMatrix m 1 (U.fromList [7.0, 8.0])
            U.toList y `shouldBe` [7.0]

        it "leaves rows touched by no triple at zero" $ do
            -- Row 1 has no entry, must stay 0.
            let m = [(0, 0, 1.0), (2, 0, 1.0)]
                y = applySparseMatrix m 3 (U.fromList [10.0])
            U.toList y `shouldBe` [10.0, 0.0, 10.0]

    describe "applyShermanMorrison (rank-1 update)" $ do
        it "applies the formula correctly on a well-conditioned case" $ do
            -- x = [1,1], z = [0.5, 0.0], col = 0:
            --   vtx = x[0] = 1
            --   vtz = z[0] = 0.5
            --   denom = 1 + 0.5 = 1.5
            --   scale = vtx/denom = 1/1.5 = 0.6666...
            --   result[i] = x[i] - scale * z[i]
            --           [0] = 1 - 0.6666 * 0.5 ≈ 0.6666
            --           [1] = 1 - 0.6666 * 0.0 = 1
            let x = U.fromList [1.0, 1.0]
                z = U.fromList [0.5, 0.0]
            case applyShermanMorrison x 0 z of
                Right v -> do
                    let [v0, v1] = U.toList v
                    v0 `shouldSatisfy` (\a -> abs (a - 2 / 3) < 1e-9)
                    v1 `shouldBe` 1.0
                Left err -> expectationFailure (T.unpack err)

        it "returns Left when the update is singular (denom ≈ 0)" $ do
            -- vtz at col 0 is -1, so denom = 1 + (-1) = 0 → singular.
            let x = U.fromList [1.0, 1.0]
                z = U.fromList [-1.0, 0.0]
            case applyShermanMorrison x 0 z of
                Left msg -> T.unpack msg `shouldContain` "singular"
                Right _ -> expectationFailure "expected singular detection"

        it "returns Left for a denom just below the tolerance threshold (1e-12)" $ do
            -- vtz = -1 + 1e-13 → |denom| = 1e-13 < 1e-12 → singular.
            let x = U.fromList [1.0]
                z = U.fromList [-1.0 + 1e-13]
            case applyShermanMorrison x 0 z of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected singular detection at 1e-13"

        it "accepts a denom just above the tolerance threshold (1e-12)" $ do
            -- vtz = -1 + 2e-12 → |denom| = 2e-12 > 1e-12 → accepted (even if ill-conditioned).
            -- Documents that we trust the caller above the cutoff.
            let x = U.fromList [1.0]
                z = U.fromList [-1.0 + 2e-12]
            case applyShermanMorrison x 0 z of
                Right _ -> pure ()
                Left msg -> expectationFailure (T.unpack msg)

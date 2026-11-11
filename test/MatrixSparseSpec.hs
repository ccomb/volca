{- |
Module      : MatrixSparseSpec
Description : The solver archive works inside the program that links it.

Each build links the archive its own way: fully static against musl, against
the system's C library, through MSYS2 on Windows. A link that succeeds can
still leave the archive unable to allocate or to unwind, and the second only
shows the day something panics. This asks the archive itself.
-}
module MatrixSparseSpec (spec) where

import Matrix.Sparse (probe)
import Test.Hspec

spec :: Spec
spec =
    describe "probe" $
        it "finds the linked solver archive sound" $
            probe `shouldReturn` Right ()

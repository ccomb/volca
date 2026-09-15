{- |
Module      : BlasThreadsSpec
Description : Creating a MUMPS solver leaves OpenBLAS on one thread.

Each build reaches OpenBLAS its own way: from a static archive, by name on the
link line, or through the library MUMPS itself loads. The pin has to reach it on
every one of them, because a reference that resolves to nothing leaves the
thread pool running and the solve twice as slow without an error.
-}
module BlasThreadsSpec (spec) where

import Numerical.MUMPS (mumpsBlasThreads, mumpsCreate, mumpsDestroy)
import Test.Hspec

spec :: Spec
spec =
    describe "mumpsCreate" $
        it "pins OpenBLAS to one thread" $ do
            solver <- mumpsCreate 1 1 [0] [0] [1.0]
            threads <- mumpsBlasThreads
            mumpsDestroy solver
            threads `shouldBe` Just 1

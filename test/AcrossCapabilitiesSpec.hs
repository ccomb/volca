{- |
Module      : AcrossCapabilitiesSpec
Description : Spreading a batch over the capabilities keeps its order and its count.

A batch request builds its entries in slices, one per capability. The response
pairs each entry with the id the caller sent by position, so a slice returned
out of place, or an element run twice or not at all, would answer for the wrong
process without an error.
-}
module AcrossCapabilitiesSpec (spec) where

import API.Routes (acrossCapabilities)
import Control.Concurrent (getNumCapabilities)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Test.Hspec

spec :: Spec
spec =
    describe "acrossCapabilities" $ do
        it "returns the results in the order of its input, past one slice per capability" $ do
            capabilities <- getNumCapabilities
            let xs = [1 .. 10 * capabilities + 3] :: [Int]
            acrossCapabilities (pure . (* 2)) xs `shouldReturn` map (* 2) xs

        it "runs the action once per element" $ do
            runs <- newIORef (0 :: Int)
            _ <- acrossCapabilities (\_ -> atomicModifyIORef' runs (\n -> (n + 1, ()))) [1 .. 1000 :: Int]
            readIORef runs `shouldReturn` 1000

        it "answers an empty list with an empty list" $
            acrossCapabilities (pure . (+ 1)) ([] :: [Int]) `shouldReturn` []

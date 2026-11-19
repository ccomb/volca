{-# LANGUAGE OverloadedStrings #-}

{- | Amounts under two swaps, where the second swap reaches a consumer the
first one changed.

The root buys one C and one A2; C buys 2 of A1; B2 buys 0.1 of C; B1 buys
nothing. Swapping A1 for B1 and A2 for B2 leaves the root buying C and B2,
B2 buying C, and C buying B1: C = 1.1 and B1 = 2.2. Solving the second swap
against the database as loaded sends B2's extra C to A1, which the first
swap took out of the chain.
-}
module MultiSwapSpec (spec) where

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import API.Types (Substitution (..), SubstitutionScope (..))
import Matrix (buildDemandVector)
import Service
import SharedSolver (solveWithSharedSolver)
import TestHelpers (mkSolverFromDb)
import Types
import UnitConversion (defaultUnitConfig)

import CrossDBRegionalLCIAFixture (mkDB)
import DependencyLicenceSpec (referenceOf)

-- | Root 0, C 1, A1 2, A2 3, B1 4, B2 5.
twoSwapRoot :: Database
twoSwapRoot =
    base
        { dbActivities = V.imap (\i act -> act{activityLines = packExchanges [referenceOf (500 + i)]}) (dbActivities base)
        , dbTechnosphereTriples = U.fromList loaded
        }
  where
    base :: Database
    base = mkDB 500 (replicate 6 "FR") []

-- | Each coefficient as (supplier, consumer, amount).
loaded :: [SparseTriple]
loaded = [SparseTriple c root 1.0, SparseTriple a2 root 1.0, SparseTriple a1 c 2.0, SparseTriple c b2 0.1]

root, c, a1, a2, b1, b2 :: (Num a) => a
root = 0
c = 1
a1 = 2
a2 = 3
b1 = 4
b2 = 5

pidOf :: Int -> Text
pidOf = processIdToText twoSwapRoot . fromIntegral

swapA1, swapA1AtC, swapA2 :: Substitution
swapA1 = Substitution (pidOf a1) (pidOf b1) AllConsumers
swapA1AtC = Substitution (pidOf a1) (pidOf b1) (OneEdge (pidOf c))
swapA2 = Substitution (pidOf a2) (pidOf b2) AllConsumers

-- | The root's amounts under these swaps, or why they were refused.
amountsUnder :: [Substitution] -> IO (Either ServiceError [Double])
amountsUnder subs = do
    solver <- mkSolverFromDb twoSwapRoot "root"
    fmap (U.toList . fst) <$> computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig noDeps twoSwapRoot "root" solver root subs
  where
    noDeps _ = pure Nothing

-- | The root's amounts solved afresh on these coefficients.
solvedOn :: [SparseTriple] -> IO [Double]
solvedOn triples = do
    let db = twoSwapRoot{dbTechnosphereTriples = U.fromList triples}
    solver <- mkSolverFromDb db "fresh"
    d <- either (fail . show) pure (buildDemandVector db root)
    U.toList <$> (either (fail . show) pure =<< solveWithSharedSolver solver d)

-- | The amounts once both swaps hold: root, C, A1, A2, B1, B2.
bothSwapped :: [Double]
bothSwapped = [1, 1.1, 0, 0, 2.2, 1]

near :: [Double] -> [Double] -> Bool
near xs ys = length xs == length ys && and (zipWith (\x y -> abs (x - y) < 1e-9) xs ys)

shouldBeNear :: Either ServiceError [Double] -> [Double] -> Expectation
shouldBeNear (Left e) _ = expectationFailure ("refused: " <> show e)
shouldBeNear (Right xs) ys = xs `shouldSatisfy` near ys

spec :: Spec
spec = describe "two swaps" $ do
    it "moves the second swap's demand to where the first swap sent it" $
        (`shouldBeNear` bothSwapped) =<< amountsUnder [swapA1, swapA2]

    it "gives the same amounts in the other order" $
        (`shouldBeNear` bothSwapped) =<< amountsUnder [swapA2, swapA1]

    it "gives the same amounts when the first swap is at one consumer only" $
        (`shouldBeNear` bothSwapped) =<< amountsUnder [swapA1AtC, swapA2]

    it "gives the amounts of the swapped coefficients solved afresh" $ do
        fresh <- solvedOn [SparseTriple c root 1.0, SparseTriple b2 root 1.0, SparseTriple b1 c 2.0, SparseTriple c b2 0.1]
        (`shouldBeNear` fresh) =<< amountsUnder [swapA1, swapA2]

    it "reads a swap to a supplier another swap replaces against the database as published" $ do
        -- A1's consumers buy A2, and A2's published consumers buy B2.
        fresh <- solvedOn [SparseTriple c root 1.0, SparseTriple b2 root 1.0, SparseTriple a2 c 2.0, SparseTriple c b2 0.1]
        let intoA2 = Substitution (pidOf a1) (pidOf a2) AllConsumers
        (`shouldBeNear` fresh) =<< amountsUnder [swapA2, intoA2]
        (`shouldBeNear` fresh) =<< amountsUnder [intoA2, swapA2]

    describe "refuses two swaps of one supplier at one consumer" $ do
        it "when one of them is at every consumer" $
            amountsUnder [swapA1, Substitution (pidOf a1) (pidOf b2) (OneEdge (pidOf c))] >>= (`shouldSatisfy` refused)
        it "when both are at the same consumer" $
            amountsUnder [swapA1AtC, Substitution (pidOf a1) (pidOf b2) (OneEdge (pidOf c))] >>= (`shouldSatisfy` refused)
  where
    refused (Left (MatrixError _)) = True
    refused _ = False

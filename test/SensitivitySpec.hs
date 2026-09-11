{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the rank-1 perturbation primitive 'perturbA' and the
'computeSensitivities' service that drives the @POST /sensitivity@
endpoint.

Core claim: a sensitivity perturbation with relative @delta@ on the
coefficient @A_ij@ produces a scaling vector @x'@ that matches what a
full re-factorization of @(I - A')@ would produce, where @A'_ij = A_ij *
(1 + delta)@. The Sherman-Morrison shortcut is correct, not just fast.
-}
module SensitivitySpec (spec) where

import API.Types (LCIAResult (..), Perturbation (..), PerturbedEntry (..))
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Matrix (Demand (..), applyShermanMorrison, buildDemandVectorFromIndex, perturbA, solveSparseLinearSystem)
import Service (computeSensitivities)
import SharedSolver (getFactorization, solveWithSharedSolver)
import Test.Hspec
import TestHelpers (loadSampleDatabase, mkSolverFromDb)
import Types

spec :: Spec
spec = do
    describe "perturbA primitive" $ do
        it "with empty perturbation returns x identical (no-op)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolverFromDb db "SAMPLE.min3"
            let demand = buildDemandVectorFromIndex (dbActivityIndex db) 0
            x <- solveWithSharedSolver solver =<< either (fail . show) pure demand
            mFact <- getFactorization solver
            r <- perturbA db mFact x 0 []
            case r of
                Right x' -> U.toList x' `shouldBe` U.toList x
                Left e -> expectationFailure ("perturbA failed: " <> T.unpack e)

    describe "applyShermanMorrison singularity guard" $ do
        -- Regression gate: the rank-1 update is well-defined iff 1 + v^T z /= 0.
        -- The function uses |denom| < 1e-12 as the singular threshold; these
        -- tests pin down both the condition (exact zero) and the boundary.
        it "returns Left when 1 + z[col] is exactly zero" $ do
            let x = U.fromList [1.0, 2.0, 3.0]
                col = 1
                z = U.fromList [0.5, -1.0, 0.7] -- z[1] = -1 ⇒ denom = 0
            case applyShermanMorrison x col z of
                Left msg -> msg `shouldSatisfy` T.isInfixOf "singular"
                Right _ -> expectationFailure "expected Left on denom == 0"

        it "returns Left when |1 + z[col]| is just below the 1e-12 tolerance" $ do
            let x = U.fromList [1.0, 2.0, 3.0]
                col = 1
                z = U.fromList [0.5, -1.0 + 1e-13, 0.7] -- denom ≈ 1e-13 < 1e-12
            case applyShermanMorrison x col z of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left when denom below tolerance"

        it "returns Right when |1 + z[col]| is just above the 1e-12 tolerance" $ do
            let x = U.fromList [1.0, 2.0, 3.0]
                col = 1
                z = U.fromList [0.5, -1.0 + 1e-11, 0.7] -- denom ≈ 1e-11 > 1e-12
            case applyShermanMorrison x col z of
                Right _ -> pure ()
                Left msg -> expectationFailure ("expected Right above tolerance, got: " <> T.unpack msg)

    describe "computeSensitivities" $ do
        it "with empty perturbations list returns baseline only" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolverFromDb db "SAMPLE.min3"
            r <- computeSensitivities db solver 0 []
            case r of
                Right (_, results) -> length results `shouldBe` 0
                Left e -> expectationFailure ("computeSensitivities failed: " <> show e)

        it "missing technosphere link surfaces as per-perturbation error" $ do
            -- SAMPLE.min3: X (idx 0) consumes Y (idx 1) only; X does NOT consume Z (idx 2).
            -- Perturbing the non-existing A[Z,X] link must surface a per-perturbation
            -- 'Left' (not abort the sweep, not fall through to a baseline solve).
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolverFromDb db "SAMPLE.min3"
            let xText = processIdToText db 0
                zText = processIdToText db 2
                bogus = Perturbation{perConsumer = xText, perSupplier = zText, perDelta = 0.05, perLabel = Just "bogus"}
            r <- computeSensitivities db solver 0 [bogus]
            case r of
                Right (_, [(_, Left msg)]) ->
                    msg `shouldSatisfy` T.isInfixOf "no technosphere link"
                Right other ->
                    expectationFailure ("expected Left per-perturbation, got: " <> show (length (snd other)) <> " entries")
                Left e ->
                    expectationFailure ("global failure (should be per-entry): " <> show e)

        it "cross-DB qualified pid in V1 is rejected per-perturbation" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolverFromDb db "SAMPLE.min3"
            let xText = processIdToText db 0
                qualified = "otherdb::" <> processIdToText db 1
                p = Perturbation{perConsumer = xText, perSupplier = qualified, perDelta = 0.05, perLabel = Nothing}
            r <- computeSensitivities db solver 0 [p]
            case r of
                Right (_, [(_, Left msg)]) ->
                    msg `shouldSatisfy` T.isInfixOf "cross-DB"
                _ -> expectationFailure "expected per-perturbation cross-DB rejection"

        it "preserves perturbation order in the result list" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolverFromDb db "SAMPLE.min3"
            let xText = processIdToText db 0
                yText = processIdToText db 1
                mk lbl d = Perturbation{perConsumer = xText, perSupplier = yText, perDelta = d, perLabel = Just lbl}
                perts = [mk "first" (-0.10), mk "second" 0.0, mk "third" 0.10]
            r <- computeSensitivities db solver 0 perts
            case r of
                Right (_, results) ->
                    map (perLabel . fst) results `shouldBe` map perLabel perts
                Left e -> expectationFailure ("computeSensitivities failed: " <> show e)

    describe "PerturbedEntry JSON wire format" $ do
        let p = Perturbation{perConsumer = "c", perSupplier = "s", perDelta = 0.05, perLabel = Just "lbl"}
            dummyLcia =
                LCIAResult
                    { lrMethodId = UUID.nil
                    , lrMethodName = "m"
                    , lrCategory = "cat"
                    , lrDamageCategory = "cat"
                    , lrScore = 1.0
                    , lrUnit = "u"
                    , lrNormalizedScore = Nothing
                    , lrWeightedScore = Nothing
                    , lrMappedFlows = 0
                    , lrFunctionalUnit = "fu"
                    , lrTopContributors = []
                    }
            keysOf bs = case decodeBS bs of
                Just (Object o) -> KM.keys o
                _ -> []
            decodeBS = (decode :: BSL.ByteString -> Maybe Value)
            hasKey k bs = Key.fromText k `elem` keysOf bs
        it "success entry encodes perturbation + impact + deltaImpact (no error)" $ do
            let bs = encode (PerturbedEntry p (Right (dummyLcia, 0.42)))
            hasKey "perturbation" bs `shouldBe` True
            hasKey "impact" bs `shouldBe` True
            hasKey "deltaImpact" bs `shouldBe` True
            hasKey "error" bs `shouldBe` False
        it "failure entry encodes perturbation + error (no impact/deltaImpact)" $ do
            let bs = encode (PerturbedEntry p (Left "boom"))
            hasKey "perturbation" bs `shouldBe` True
            hasKey "error" bs `shouldBe` True
            hasKey "impact" bs `shouldBe` False
            hasKey "deltaImpact" bs `shouldBe` False

    describe "computeSensitivities (continued)" $
        it "matches full re-factorization for a single A_ij perturbation" $ do
            -- The strong correctness check: Sherman-Morrison shortcut must match
            -- a full solve of (I - A') x' = d, where A'_YX = A_YX * (1 + delta).
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolverFromDb db "SAMPLE.min3"
            let xText = processIdToText db 0 -- consumer X (column j=0)
                yText = processIdToText db 1 -- supplier Y (row i=1)
                delta = 0.05
                p = Perturbation{perConsumer = xText, perSupplier = yText, perDelta = delta, perLabel = Just "Y+5%"}

            -- Sherman-Morrison path
            r <- computeSensitivities db solver 0 [p]
            xSM <- case r of
                Right (_, [(_, Right v)]) -> pure v
                _ -> error "computeSensitivities did not return Right"

            -- Ground truth: 'dbTechnosphereTriples' stores positive A_ij for input
            -- exchanges (Database.hs:163-165). We scale the (supplier=1, consumer=0)
            -- entry by (1+delta) and pass the modified triples to solveSparseLinearSystem
            -- (which negates internally to build I - A').
            let consumerCol = 0 :: Int
                supplierRow = 1 :: Int
                techTriples = U.toList (dbTechnosphereTriples db)
                scaledTriples =
                    [ ( fromIntegral i
                      , fromIntegral j
                      , if i == fromIntegral supplierRow && j == fromIntegral consumerCol
                            then v * (1 + delta)
                            else v
                      )
                    | SparseTriple i j v <- techTriples
                    ]
                n = fromIntegral (dbActivityCount db)
            demand <- unDemand <$> either (fail . show) pure (buildDemandVectorFromIndex (dbActivityIndex db) 0)
            xRef <- solveSparseLinearSystem scaledTriples n demand

            -- Compare element-wise within tight numerical tolerance.
            let near a b = abs (a - b) < 1e-10
                ok = and (zipWith near (U.toList xSM) (U.toList xRef))
            ok `shouldBe` True

{-# LANGUAGE OverloadedStrings #-}

{- | Phase-1 substitution tests.

Core claim: the POST-style 'inventoryWithSubsAndDeps' path is a strict
superset of the GET-style cross-DB inventory path. With empty
substitutions it must produce the same inventory bit-for-bit (otherwise
silent-undercount regression). With non-empty substitutions on a DB
that has no 'dbCrossDBLinks', it must match the old behaviour
('applyBiosphereMatrix' on the substituted scaling vector).

Multi-DB propagation (the whole point of Phase 1) is exercised indirectly
through the equality against 'computeInventoryMatrixWithDepsCached',
which is covered by 'CrossDBInventorySpec'.
-}
module SubstitutionSpec (spec) where

import API.Types (Substitution (..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Matrix
import Service (computeScalingVectorWithSubstitutions, inventoryWithSubsAndDeps)
import SharedSolver (
    SharedSolver,
    computeInventoryMatrixWithDepsCached,
    createSharedSolver,
 )
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "inventoryWithSubsAndDeps (Phase 1)" $ do
        it "with empty subs matches computeInventoryMatrixWithDepsCached" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing

            eBase <- computeInventoryMatrixWithDepsCached defaultUnitConfig noDeps db solver pid
            eSub <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid []

            case (eBase, eSub) of
                (Right base, Right sub) -> M.toList sub `shouldBe` M.toList base
                (Left e, _) -> expectationFailure ("baseline failed: " <> T.unpack e)
                (_, Left e) -> expectationFailure ("subs path failed: " <> show e)

        it "with empty subs is equivalent to applyBiosphereMatrix on the raw solve" $ do
            -- Redundant check against a different reference: on a DB with no
            -- cross-DB links, the merged inventory collapses to the root-only
            -- biosphere projection. Guards against accidental double-counting
            -- in 'goWithDepsFromScalings'.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing

            eRaw <- computeScalingVectorWithSubstitutions db solver pid []
            eSub <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid []

            case (eRaw, eSub) of
                (Right x, Right sub) ->
                    M.toList sub `shouldBe` M.toList (Matrix.applyBiosphereMatrix db x)
                (Left e, _) -> expectationFailure ("raw solve failed: " <> show e)
                (_, Left e) -> expectationFailure ("subs path failed: " <> show e)

        it "propagates a 422-style error when the substitution references a missing PID" $ do
            -- Guards the 'no silent errors' invariant: a bad substitution must
            -- surface as Left, not silently fall through to a baseline solve.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing
                -- Well-formed UUID that doesn't resolve to any activity.
                bogusPid = "00000000-0000-0000-0000-000000000000_00000000-0000-0000-0000-000000000000"
                badSub =
                    Substitution
                        { subFrom = bogusPid
                        , subTo = bogusPid
                        , subConsumer = bogusPid
                        }
            eSub <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid [badSub]
            case eSub of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left on unresolvable substitution PID"

-- | Build a fresh 'SharedSolver' from a database's technosphere triples.
mkSolver :: Database -> T.Text -> IO SharedSolver
mkSolver db name =
    let tech =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
            ]
        n = fromIntegral (dbActivityCount db)
     in createSharedSolver name tech n

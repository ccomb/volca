{-# LANGUAGE OverloadedStrings #-}

{- | Global ('AllConsumers') substitution tests.

Core claim: a global swap @from → to@ is a single Sherman-Morrison rank-1
update that equals solving the technosphere matrix with @from@'s row
relocated onto @to@. We prove it by comparing the rank-1 result against a
fresh factorization of the relocated matrix - NOT against an edge-by-edge
expansion, which (using the cached factorization per edge) is only an
approximation of the simultaneous swap.

Plus the guards: identity is a no-op, an unconsumed @from@ fails loudly
(never a silent no-op), a dep-qualified @from@ is rejected (global needs a
root supplier), and incompatible reference units fail rather than silently
reusing the coefficient.
-}
module GlobalSubstitutionSpec (spec) where

import API.Types (Substitution (..), SubstitutionScope (..))
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Matrix (buildDemandVectorFromIndex)
import Service (
    ServiceError (..),
    Swap (..),
    computeScalingVectorWithSubstitutionsCrossDB,
    substitutionUnitFactor,
    technosphereRow,
 )
import SharedSolver (SharedSolver, createSharedSolver, solveWithSharedSolver)
import Test.Hspec
import TestHelpers (linkDatabases, loadSampleDatabase, mkDepLookupFromMap, mkSolverFromDb)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "global substitution (AllConsumers)" $ do
        it "within-DB swap equals a re-solve of the relocated matrix" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let noDeps _ = pure Nothing
            case pickGlobalAB db of
                Nothing -> expectationFailure "fixture has no supplier with an off-diagonal consumer"
                Just (aPid, bPid) -> do
                    let sub = Substitution (processIdToText db aPid) (processIdToText db bPid) AllConsumers
                    gRes <- computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig noDeps db "root" solver 0 [sub]
                    refSolver <- createSharedSolver "ref" (relocateRow db aPid bPid) (fromIntegral (dbActivityCount db))
                    xRef <- solveWithSharedSolver refSolver =<< either (fail . show) pure (buildDemandVectorFromIndex (dbActivityIndex db) 0)
                    case gRes of
                        Right (xGlobal, links) -> do
                            links `shouldBe` []
                            U.toList xGlobal `shouldSatisfy` vecNear (U.toList xRef)
                        Left e -> expectationFailure ("global swap failed: " <> show e)

        it "identity swap (from == to) is a no-op" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let noDeps _ = pure Nothing
            case pickGlobalAB db of
                Nothing -> expectationFailure "fixture has no supplier with an off-diagonal consumer"
                Just (aPid, _) -> do
                    let ident = Substitution (processIdToText db aPid) (processIdToText db aPid) AllConsumers
                    base <- computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig noDeps db "root" solver 0 []
                    gRes <- computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig noDeps db "root" solver 0 [ident]
                    case (base, gRes) of
                        (Right (x0, _), Right (xg, _)) -> U.toList xg `shouldSatisfy` vecNear (U.toList x0)
                        _ -> expectationFailure "identity or baseline solve failed"

        it "fails loudly when 'from' is consumed nowhere (no silent no-op)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let noDeps _ = pure Nothing
            case nonSupplier db of
                Nothing -> expectationFailure "fixture has no unconsumed activity"
                Just leaf -> do
                    let toPid = if leaf == 0 then 1 else 0
                        sub = Substitution (processIdToText db leaf) (processIdToText db toPid) AllConsumers
                    res <- computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig noDeps db "root" solver 0 [sub]
                    case res of
                        Left (MatrixError msg) -> T.unpack msg `shouldSatisfy` isInfixOf "consumed nowhere"
                        other -> expectationFailure ("expected 'consumed nowhere' Left, got " <> show other)

        it "rejects a dep-qualified 'from' (global requires a root supplier)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let noDeps _ = pure Nothing
                bare = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                sub = Substitution ("dep::" <> bare) bare AllConsumers
            res <- computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig noDeps db "root" solver 0 [sub]
            case res of
                Left (MatrixError msg) -> T.unpack msg `shouldSatisfy` isInfixOf "root"
                other -> expectationFailure ("expected root-only Left, got " <> show other)

        it "cross-DB swap removes 'from' from the root and emits one link per consumer" $ do
            -- B lives in a dep DB: the root rank-1 is one-sided (just remove A),
            -- so the root scaling must match a re-solve with A's row deleted, and
            -- the demand is carried to the dep by one virtual link per consumer.
            rootRaw <- loadSampleDatabase "SAMPLE.min3"
            dep <- loadSampleDatabase "SAMPLE.min3"
            let root = linkDatabases rootRaw dep "dep" 0.1
            rootSolver <- mkSolverFromDb root "root"
            depSolver <- mkSolverFromDb dep "dep"
            let lookup_ = mkDepLookupFromMap (M.singleton "dep" (dep, depSolver))
            case pickGlobalAB root of
                Nothing -> expectationFailure "root fixture has no supplier with a consumer"
                Just (aPid, _) -> do
                    let bPid = 0 :: ProcessId
                        nConsumers = length (technosphereRow root aPid)
                        sub = Substitution (processIdToText root aPid) ("dep::" <> processIdToText dep bPid) AllConsumers
                    gRes <- computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig lookup_ root "root" rootSolver 0 [sub]
                    refSolver <- createSharedSolver "ref" (deleteRow root aPid) (fromIntegral (dbActivityCount root))
                    xRef <- solveWithSharedSolver refSolver =<< either (fail . show) pure (buildDemandVectorFromIndex (dbActivityIndex root) 0)
                    case gRes of
                        Right (xGlobal, links) -> do
                            length links `shouldBe` nConsumers
                            U.toList xGlobal `shouldSatisfy` vecNear (U.toList xRef)
                        Left e -> expectationFailure ("cross-DB global failed: " <> show e)

    describe "substitutionUnitFactor (κ guard)" $
        it "κ = 1 for identical units, Left for incompatible reference products" $ do
            db <- loadSampleDatabase "SAMPLE.units"
            -- electricity is in MJ, steel in kg - energy vs mass, no conversion.
            case (activityByInfix db "electricity generation", activityByInfix db "steel production") of
                (Just elec, Just steel) -> do
                    substitutionUnitFactor defaultUnitConfig db (Swap elec steel) `shouldSatisfy` isLeft
                    substitutionUnitFactor defaultUnitConfig db (Swap elec elec) `shouldSatisfy` isRightNear 1.0
                _ -> expectationFailure "SAMPLE.units missing electricity/steel activities"

-- | A fresh 'SharedSolver' over a database's own technosphere triples.
mkSolver :: Database -> T.Text -> IO SharedSolver
mkSolver db name =
    createSharedSolver
        name
        [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
        (fromIntegral (dbActivityCount db))

{- | Technosphere triples with @from@'s row relocated onto @to@ (κ = 1), i.e.
every consumer that sourced @from@ now sources @to@ by the same amount.
This is the ground-truth matrix a global swap must reproduce.
-}
relocateRow :: Database -> ProcessId -> ProcessId -> [(Int, Int, Double)]
relocateRow db aPid bPid =
    let aIdx = fromIntegral aPid :: Int
        bIdx = fromIntegral bPid :: Int
        merged =
            M.fromListWith
                (+)
                [ ((if fromIntegral i == aIdx then bIdx else fromIntegral i, fromIntegral j), v)
                | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
                ]
     in [(i, j, v) | ((i, j), v) <- M.toList merged]

{- | Technosphere triples with @from@'s row deleted: the ground-truth root
matrix after a cross-DB global swap, which only removes @from@ on the root
side (the demand is carried to the dep supplier by virtual links).
-}
deleteRow :: Database -> ProcessId -> [(Int, Int, Double)]
deleteRow db aPid =
    let aIdx = fromIntegral aPid :: Int
     in [ (fromIntegral i, fromIntegral j, v)
        | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
        , fromIntegral i /= aIdx
        ]

{- | First supplier with an off-diagonal consumer, paired with any other
activity as the replacement. SAMPLE.min3 is single-unit, so κ = 1 holds.
-}
pickGlobalAB :: Database -> Maybe (ProcessId, ProcessId)
pickGlobalAB db =
    let n = fromIntegral (dbActivityCount db) :: Int
        suppliers = [fromIntegral i | SparseTriple i j _ <- U.toList (dbTechnosphereTriples db), i /= j]
     in case suppliers of
            (a : _) -> (,) a <$> listToMaybe [fromIntegral p | p <- [0 .. n - 1], fromIntegral p /= a]
            [] -> Nothing

-- | An activity that supplies no consumer (its technosphere row is empty).
nonSupplier :: Database -> Maybe ProcessId
nonSupplier db =
    let n = fromIntegral (dbActivityCount db) :: Int
        rows = [i | SparseTriple i _ _ <- U.toList (dbTechnosphereTriples db)]
     in listToMaybe [fromIntegral p | p <- [0 .. n - 1], (fromIntegral p :: Int32) `notElem` rows]

-- | First activity whose name contains the given substring.
activityByInfix :: Database -> T.Text -> Maybe ProcessId
activityByInfix db nm =
    listToMaybe
        [ fromIntegral i
        | i <- [0 .. V.length (dbActivities db) - 1]
        , nm `T.isInfixOf` activityName (dbActivities db V.! i)
        ]

-- | Elementwise closeness of two scaling vectors.
vecNear :: [Double] -> [Double] -> Bool
vecNear ref xs = length ref == length xs && and (zipWith (\a b -> abs (a - b) < 1e-9) ref xs)

-- | 'ServiceError' has no 'Eq' instance, so assert the κ value via a predicate.
isRightNear :: Double -> Either ServiceError Double -> Bool
isRightNear target = either (const False) (\k -> abs (k - target) < 1e-9)

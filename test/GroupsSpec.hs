{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A score split between groups of processes, in both readings, checked
against parts worked out by hand.

The loop: the root R buys 1 A (transport), 2 B (fertiliser) and 1 C; B buys
0.5 A, A buys 0.1 B, C buys 1 B. Each emits one flow: R 1, A 10, B 100,
C 1000. Solved, A = 2.5 / 0.95 and B = 3 + A / 10.

The linked pair: in database R the root buys 1 T (transport, emits 3), and
T buys 2 D0 from database D. In D, D0 (emits 5) buys 1 D1 (emits 7), and D1
buys 1 D2 (emits 11). The score is 3 + 2·(5 + 7 + 11) = 49.
-}
module GroupsSpec (spec) where

import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import CrossDBRegionalLCIAFixture (kgUnitConfig, linkAt, mkDB)
import DependencyLicenceSpec (referenceOf)
import Groups
import Matrix (Demand (..), buildDemandVector)
import SharedSolver (CrossDBSolution (..), DepSolverLookup, Reached (..), SharedSolver, chainWeights, computeInventoryMatrixWithDepsCached, goWithDeps, weightAt)
import TestHelpers (mkSolverFromDb, unitDef)
import Types hiding (Attribution)
import UnitConversion (defaultDimensionOrder, mkUnitConfig)

-- | A database whose processes each produce one unit of their own product.
producing :: Int -> [(Int, Double)] -> [SparseTriple] -> Database
producing offset emissions triples =
    base
        { dbActivities = V.imap (\i act -> act{activityLines = packExchanges [referenceOf (offset + i)]}) (dbActivities base)
        , dbTechnosphereTriples = U.fromList triples
        }
  where
    base :: Database
    base = mkDB offset (replicate (length emissions) "FR") emissions

-- | R 0, A 1, B 2, C 3; each triple is (supplier, consumer, amount).
loop :: Database
loop =
    producing
        700
        [(0, 1), (1, 10), (2, 100), (3, 1000)]
        [SparseTriple 1 0 1, SparseTriple 2 0 2, SparseTriple 3 0 1, SparseTriple 1 2 0.5, SparseTriple 2 1 0.1, SparseTriple 2 3 1]

-- | Root 1, T 0, in R.
linkedRoot :: Database
linkedRoot = linkAt (producing 800 [(0, 3), (1, 0)] [SparseTriple 0 1 1]) linkedDep "D" 0 2

-- | D0 0, D1 1, D2 2.
linkedDep :: Database
linkedDep = producing 900 [(0, 5), (1, 7), (2, 11)] [SparseTriple 1 0 1, SparseTriple 2 1 1]

-- | Two groups, each a process of the database by its column.
assignedAt :: Text -> [(Int, Int)] -> M.Map Text Assignment
assignedAt name taken = M.singleton name (V.generate (fromIntegral (dbActivityCount (dbNamed name))) (\c -> GroupIndex <$> lookup c taken))
  where
    dbNamed :: Text -> Database
    dbNamed = \case
        "loop" -> loop
        "D" -> linkedDep
        _ -> linkedRoot

twoGroups :: Groups
twoGroups = either (error . T.unpack) id (groups [Group "transport" (named "transport"), Group "fertiliser" (named "fertiliser")])

named :: Text -> GroupFilter
named n = either (error . T.unpack) id (groupFilter (Criteria (Just n) Nothing Nothing (ClassificationFilters [] [])))

-- | The solution of a root, its database solved with its dependencies.
solved :: DepSolverLookup -> Database -> Text -> SharedSolver -> ProcessId -> IO CrossDBSolution
solved deps db name solver pid = either (error . T.unpack) id <$> computeInventoryMatrixWithDepsCached kgUnitConfig deps db name solver pid

-- | The one flow's amount in a part, scored the way the score is.
emitted :: CrossDBSolution -> Scalings -> Double
emitted sol part = sum (M.elems (csInventory (assembled M.empty sol part)))

-- | The rest's amount, then each group's.
amounts :: CrossDBSolution -> Parts -> [Double]
amounts sol parts = map (emitted sol) (partsRest parts : partsGroups parts)

closeTo :: [Double] -> [Double] -> Expectation
closeTo got expected = do
    length got `shouldBe` length expected
    mapM_ (\(g, e) -> abs (g - e) `shouldSatisfy` (<= 1e-9 * max 1 (abs e))) (zip got expected)

noDeps :: DepSolverLookup
noDeps _ = pure Nothing

firstMatch :: DepSolverLookup -> CrossDBSolution -> Database -> ProcessId -> M.Map Text Assignment -> IO Parts
firstMatch deps sol db root asg = do
    let demand = either (error . T.unpack) id (buildDemandVector db root)
    either (error . T.unpack) id <$> firstMatchParts (Solving kgUnitConfig deps id) sol demand (Assigned twoGroups asg)

spec :: Spec
spec = do
    describe "a loop of two groups" $ do
        let a = 2.5 / 0.95
            b = 3 + a / 10
            total = 1 + 10 * a + 100 * b + 1000
            asg = assignedAt "loop" [(1, 0), (2, 1)]
        it "gives each group its own emissions" $ do
            solver <- mkSolverFromDb loop "loop"
            sol <- solved noDeps loop "loop" solver 0
            amounts sol (ownEmissionParts (Assigned twoGroups asg) sol) `closeTo` [1001, 10 * a, 100 * b]
            sum (amounts sol (ownEmissionParts (Assigned twoGroups asg) sol)) `closeTo'` total

        it "gives each group the whole chain below its first process met" $ do
            solver <- mkSolverFromDb loop "loop"
            sol <- solved noDeps loop "loop" solver 0
            parts <- firstMatch (\name -> pure (if name == "loop" then Just (loop, solver) else Nothing)) sol loop 0 asg
            -- Cut, A receives 1 from R and B receives 2 from R and 1 from C.
            amounts sol parts `closeTo` [1001, (10 + 100 * 0.1) / 0.95, 3 * (100 + 10 * 0.5) / 0.95]
            sum (amounts sol parts) `closeTo'` total

        it "scores a part from each process's score per unit as its inventory does" $ do
            solver <- mkSolverFromDb loop "loop"
            sol <- solved noDeps loop "loop" solver 0
            parts <- firstMatch (\name -> pure (if name == "loop" then Just (loop, solver) else Nothing)) sol loop 0 asg
            let perUnit = perUnitByColumn (summedByName (csScalings sol)) (M.fromList [(("loop", p), e) | (p, e) <- zip [0 ..] [1, 10, 100, 1000]])
            map (scoreFromPerUnit perUnit) (partsRest parts : partsGroups parts) `closeTo` amounts sol parts

    describe "groups across linked databases" $ do
        -- Every loaded database, the root's too, as the engine's lookup answers.
        let loaded depSolver rootSolver = \case
                "D" -> pure (Just (linkedDep, depSolver))
                "R" -> pure (Just (linkedRoot, rootSolver))
                _ -> pure Nothing
        it "leaves out what a process of a group buys from another database" $ do
            depSolver <- mkSolverFromDb linkedDep "D"
            rootSolver <- mkSolverFromDb linkedRoot "R"
            sol <- solved (loaded depSolver rootSolver) linkedRoot "R" rootSolver 1
            parts <- firstMatch (loaded depSolver rootSolver) sol linkedRoot 1 (assignedAt "R" [(0, 0)])
            amounts sol parts `closeTo` [0, 49, 0]

        it "cuts a dependency at its processes of a group" $ do
            depSolver <- mkSolverFromDb linkedDep "D"
            rootSolver <- mkSolverFromDb linkedRoot "R"
            sol <- solved (loaded depSolver rootSolver) linkedRoot "R" rootSolver 1
            parts <- firstMatch (loaded depSolver rootSolver) sol linkedRoot 1 (assignedAt "D" [(1, 1)])
            amounts sol parts `closeTo` [3 + 10, 0, 2 * (7 + 11)]
            amounts sol (ownEmissionParts (Assigned twoGroups (assignedAt "D" [(1, 1)])) sol) `closeTo` [3 + 10 + 22, 0, 14]

        it "refuses the whole-chain reading when the databases loop" $ do
            let looped = linkAt linkedDep linkedRoot "R" 0 1
                reached = Reached "R" linkedRoot (U.fromList [1, 1]) :| [Reached "D" looped (U.fromList [2, 2, 2])]
            databaseCycle reached `shouldBe` Just ["R", "D", "R"]
            databaseCycle (Reached "R" linkedRoot (U.fromList [1, 1]) :| [Reached "D" linkedDep (U.fromList [2, 2, 2])]) `shouldBe` Nothing

    describe "every process weighed across linked databases" $
        it "weighs each process as solving for one unit of it, its dependencies included, does" $ do
            -- R buys from D, and in grams from E; D and E both buy from F.
            let f = producing 1000 [(0, 1), (1, 1)] [SparseTriple 1 0 0.5]
                d = linkAt (producing 1100 [(0, 1), (1, 1)] [SparseTriple 1 0 0.3, SparseTriple 0 1 0.2]) f "F" 1 2
                e = linkAt (producing 1200 [(0, 1)] []) f "F" 0 3
                inGrams db = db{dbCrossDBLinks = map (\link -> link{cdlExchangeUnit = "g"}) (dbCrossDBLinks db)}
                r = linkAt (inGrams (linkAt (producing 1300 [(0, 1), (1, 1)] [SparseTriple 1 0 1]) e "E" 0 500)) d "D" 1 4
                withGrams = mkUnitConfig defaultDimensionOrder (M.fromList [("kg", unitDef "mass" 1), ("g", unitDef "mass" 0.001)])
            loaded <- forM [("R", r), ("D", d), ("E", e), ("F", f)] $ \(name, db) -> (\solver -> (name, (db, solver))) <$> mkSolverFromDb db name
            let deps name = pure (lookup name loaded)
                alone = M.fromList [(name, U.generate (fromIntegral (dbActivityCount db)) (\c -> fromIntegral (3 + 7 * c + 11 * k))) | (k, (name, (db, _))) <- zip [0 :: Int ..] loaded]
            weights <- either (error . T.unpack) id <$> chainWeights withGrams deps alone "R"
            M.keys weights `shouldBe` ["D", "E", "F", "R"]
            pairs <- fmap concat . forM loaded $ \(name, (db, solver)) -> do
                let n = fromIntegral (dbActivityCount db)
                forM [0 .. n - 1] $ \c -> do
                    [answer] <- goWithDeps withGrams deps db name solver [Right (Demand (U.generate n (\i -> if i == c then 1 else 0)))] 0
                    let solvedOnce = either (error . T.unpack) (scoreFromPerUnit alone . summedByName . csScalings) answer
                    pure (M.lookup name weights >>= \(_, w) -> weightAt w c, solvedOnce)
            map fst pairs `shouldNotContain` [Nothing]
            concatMap (maybe [] pure . fst) pairs `closeTo` map snd pairs

    describe "which process goes where" $ do
        it "never puts the root in a group" $ do
            let asg = assignment (const (Just (GroupIndex 0))) (Just 0) loop
            V.toList asg `shouldBe` [Nothing, Just (GroupIndex 0), Just (GroupIndex 0), Just (GroupIndex 0)]

        it "refuses a group that would take every process" $ do
            groupFilter (Criteria (Just "  ") Nothing Nothing (ClassificationFilters [] [ClassificationFilter "CPC" "64" MatchPrefix])) `shouldSatisfy` isLeft

        it "refuses two groups of one name, and no group" $ do
            groups [Group "a" (named "x"), Group "a" (named "y")] `shouldSatisfy` isLeft
            groups [] `shouldSatisfy` isLeft

        it "names the groups when asked for one that is not there" $ do
            groupNamed twoGroups "energy" `shouldBe` Left "No group is named 'energy': the groups are transport, fertiliser."

    describe "the starts-with classification mode" $ do
        it "takes a code and the codes under it, not a code that only contains it" $ do
            let classified = M.singleton "CPC"
                prefix = ClassificationFilter "CPC" "64" MatchPrefix
            classificationMatches prefix (classified "6411: Local transport") `shouldBe` True
            classificationMatches prefix (classified "2364: Cement") `shouldBe` False
            readClassificationMode "not-starts-with" `shouldBe` Right (Excluding MatchPrefix)
  where
    closeTo' got expected = abs (got - expected) `shouldSatisfy` (<= 1e-9 * max 1 (abs expected))
    isLeft = either (const True) (const False)

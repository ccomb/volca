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

The fields: the root buys 1 field and 1 transport, each emitting several
flows ('fieldFlows'), each amount a power of two times ten so a sum names the
emissions it holds.
-}
module GroupsSpec (spec) where

import Control.Monad (forM)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import API.Routes (attributionOf, candidateOf, flowCandidates, readGroups)
import API.Types (GroupFilterSpec, GroupSpec (..))
import CrossDBRegionalLCIAFixture (kgUnit, kgUnitConfig, linkAt, mkDB, mkUUID)
import DependencyLicenceSpec (referenceOf)
import Groups
import Matrix (Demand (..), Vector, buildDemandVector)
import Method.ChemSynonyms (ChemSynonyms, parseChemSynonymsCSV)
import SharedSolver (CrossDBSolution (..), DepSolverLookup, Reached (..), SharedSolver, chainWeights, computeInventoryMatrixWithDepsCached, goWithDeps, weightAt)
import TestHelpers (mkSolverFromDb, shippedGeographies, unitDef)
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

twoGroups :: Groups ProcessFilter
twoGroups = either (error . T.unpack) id (groups [Group "transport" (named "transport"), Group "fertiliser" (named "fertiliser")])

named :: Text -> ProcessFilter
named n = either (error . T.unpack) id (processFilter (nameCriteria (Just n)))

nameCriteria :: Maybe Text -> Criteria
nameCriteria n = Criteria n Nothing Nothing (ClassificationFilters [] [])

{- | The flows of the fields, by row: N2O to air, to water and with no
compartment, two methanes of one CAS, a refrigerant whose name holds
@methane@, fossil CO2; row 7 has no metadata.
-}
fieldFlows :: [BiosphereFlow]
fieldFlows =
    [ flowAt 0 "Dinitrogen monoxide" (Just "10024-97-2") (Just Air)
    , flowAt 1 "Dinitrogen monoxide" (Just "10024-97-2") (Just Water)
    , flowAt 2 "Methane, fossil" (Just "74-82-8") (Just Air)
    , flowAt 3 "Methane, biogenic" (Just "74-82-8") (Just Air)
    , flowAt 4 "Methane, dichlorodifluoro-, CFC-12" (Just "75-71-8") (Just Air)
    , flowAt 5 "Carbon dioxide, fossil" (Just "124-38-9") (Just Air)
    , flowAt 6 "Dinitrogen monoxide" (Just "10024-97-2") Nothing
    ]
  where
    flowAt :: Integer -> Text -> Maybe Text -> Maybe Medium -> BiosphereFlow
    flowAt k name cas medium =
        BiosphereFlow
            { bfId = mkUUID (100 + k)
            , bfName = name
            , bfUnitId = unitId kgUnit
            , bfSynonyms = M.empty
            , bfCAS = cas
            , bfSubstanceId = Nothing
            , bfCompartment = (`Compartment` Nothing) <$> medium
            }

-- | Root 0, field 1, transport 2; each triple is (row, column, amount).
fields :: Database
fields =
    base
        { dbBiosphere = byColumn (U.fromList [SparseTriple r c v | (r, c, v) <- emissions])
        , dbBiosphereOrder = V.fromList [mkUUID (100 + k) | k <- [0 .. 7]]
        , dbBiosphereCount = 8
        , dbBioFlows = M.fromList [(bfId f, f) | f <- fieldFlows]
        }
  where
    base :: Database
    base = (producing 1400 [(0, 0), (1, 0), (2, 0)] [SparseTriple 1 0 1, SparseTriple 2 0 1]){dbActivities = V.imap (\i act -> act{activityName = ["root", "field", "transport"] !! i}) (dbActivities (producing 1400 [(0, 0), (1, 0), (2, 0)] []))}

    emissions :: [(Int32, Int32, Double)]
    emissions =
        [(0, 0, 1), (5, 0, 2)]
            ++ [(r, 1, v) | (r, v) <- [(0, 10), (1, 20), (2, 40), (3, 80), (6, 160), (7, 320)]]
            ++ [(0, 2, 1000), (4, 2, 2000), (5, 2, 4000)]

-- | Three substances of the shipped list, as its file writes them.
fieldChemicals :: ChemSynonyms
fieldChemicals =
    either (error . ("chemical names: " <>)) id $
        parseChemSynonymsCSV "cas;canonical_name;synonyms...\n10024-97-2;Nitrous oxide;N2O\n74-82-8;Methane;CH4\n124-38-9;Carbon dioxide;CO2\n"

-- | The fields' parts under groups, each a filter, split by emission.
fieldParts :: CrossDBSolution -> [GroupFilter] -> Parts
fieldParts sol filters = emissionParts (masked (map (reachOf basis) filters)) scalings
  where
    scalings :: Scalings
    scalings = summedByName (csScalings sol)

    basis :: EmissionBasis
    basis = EmissionBasis shippedGeographies fieldChemicals (dbBioFlows fields) ("fields", 0) [] scalings

-- | A group keeping the processes named (any when 'Nothing') and the emissions given.
keeping :: Maybe Text -> Maybe EmissionCriteria -> GroupFilter
keeping name emission = either (error . T.unpack) id (groupFilter (nameCriteria name) emission)

substance :: Text -> Maybe Medium -> EmissionCriteria
substance text = EmissionCriteria (Just (IsTheSubstance, text))

-- | A group as the wire writes it, from its JSON filter.
wired :: Text -> BL.ByteString -> GroupSpec
wired name filterJson = GroupSpec name (either error id (eitherDecode filterJson))

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

{- | The score of a part, each process's scaling times its score per unit:
the score is linear in the scalings, so this is the score of the part without
building its inventory.
-}
scoreFromPerUnit :: M.Map Text Vector -> Scalings -> Double
scoreFromPerUnit perUnit part = sum (M.intersectionWith (\unitScores (_, v) -> U.sum (U.zipWith (*) unitScores v)) perUnit part)

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

    describe "every process weighed across linked databases" $ do
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

        it "leaves out a link that carries no amount, as a solve does, whatever its unit" $ do
            let f = producing 1000 [(0, 1)] []
                inFurlongs db = db{dbCrossDBLinks = map (\link -> link{cdlExchangeUnit = "furlong"}) (dbCrossDBLinks db)}
                r = inFurlongs (linkAt (producing 1300 [(0, 1), (1, 1)] [SparseTriple 1 0 1]) f "F" 0 0)
            loaded <- forM [("R", r), ("F", f)] $ \(name, db) -> (\solver -> (name, (db, solver))) <$> mkSolverFromDb db name
            let deps name = pure (lookup name loaded)
                alone = M.fromList [("R", U.fromList [2, 3]), ("F", U.fromList [5])]
            weights <- either (error . T.unpack) id <$> chainWeights kgUnitConfig deps alone "R"
            [M.lookup "R" weights >>= \(_, w) -> weightAt w c | c <- [0, 1]] `shouldBe` [Just 5, Just 3]

    describe "which process goes where" $ do
        it "never puts the root in a group" $ do
            let asg = assignment (const (Just (GroupIndex 0))) (Just 0) loop
            V.toList asg `shouldBe` [Nothing, Just (GroupIndex 0), Just (GroupIndex 0), Just (GroupIndex 0)]

        it "refuses a group that would take every process" $ do
            isLeft (processFilter (Criteria (Just "  ") Nothing Nothing (ClassificationFilters [] [ClassificationFilter "CPC" "64" MatchPrefix]))) `shouldBe` True

        it "refuses two groups of one name, and no group" $ do
            isLeft (groups [Group "a" (named "x"), Group "a" (named "y")]) `shouldBe` True
            isLeft (groups []) `shouldBe` True

        it "names the groups when asked for one that is not there" $ do
            groupNamed twoGroups "energy" `shouldBe` Left "No group is named 'energy': the groups are transport, fertiliser."

    describe "groups that keep substances, split by emission" $ do
        let solvedFields = do
                solver <- mkSolverFromDb fields "fields"
                solved noDeps fields "fields" solver 0
            total = 1 + 2 + 630 + 7000
        it "gives a group the substance of its processes, then the next group the rest of it, and adds up" $ do
            sol <- solvedFields
            let parts = fieldParts sol [keeping (Just "field") (Just (substance "N2O" Nothing)), keeping Nothing (Just (substance "N2O" Nothing)), keeping (Just "transport") Nothing]
            -- Rest: the root's CO2, the field's methanes and its row without metadata.
            amounts sol parts `closeTo` [2 + 40 + 80 + 320, 10 + 20 + 160, 1 + 1000, 2000 + 4000]
            sum (amounts sol parts) `closeTo'` total
            map takenCount (partsTaken parts) `shouldBe` [1, 2, 1]

        it "reads a substance as the substance, never as a name holding it, unless asked" $ do
            sol <- solvedFields
            amounts sol (fieldParts sol [keeping Nothing (Just (substance "methane" Nothing))]) `closeTo` [total - 120, 40 + 80]
            amounts sol (fieldParts sol [keeping Nothing (Just (EmissionCriteria (Just (NameContains, "methane")) Nothing))]) `closeTo` [total - 2120, 40 + 80 + 2000]

        it "takes no flow named otherwise when the flows carry no CAS" $ do
            let withoutCas = fields{dbBioFlows = M.map (\f -> f{bfCAS = Nothing}) (dbBioFlows fields)}
            solver <- mkSolverFromDb withoutCas "fields"
            sol <- solved noDeps withoutCas "fields" solver 0
            let partsOf filters = emissionParts (masked (map (reachOf (EmissionBasis shippedGeographies fieldChemicals (dbBioFlows withoutCas) ("fields", 0) [] scalings)) filters)) scalings
                scalings = summedByName (csScalings sol)
            -- No registry here: methane names neither flow, and a flow's own name takes that flow alone.
            amounts sol (partsOf [keeping Nothing (Just (substance "methane" Nothing))]) `closeTo` [total, 0]
            amounts sol (partsOf [keeping Nothing (Just (substance "Methane, fossil" Nothing))]) `closeTo` [total - 40, 40]

        it "finds a substance by its CAS, and by its medium alone" $ do
            sol <- solvedFields
            -- In the air only: not the field's N2O to water, nor the one without a compartment.
            amounts sol (fieldParts sol [keeping Nothing (Just (substance "10024-97-2" (Just Air)))]) `closeTo` [total - 1011, 1 + 10 + 1000]
            amounts sol (fieldParts sol [keeping Nothing (Just (EmissionCriteria Nothing (Just Water)))]) `closeTo` [total - 20, 20]

        it "never gives the root to a group of processes, but to a group of a substance alone" $ do
            sol <- solvedFields
            amounts sol (fieldParts sol [keeping (Just "t") (Just (substance "N2O" Nothing))]) `closeTo` [total - 1000, 1000]
            amounts sol (fieldParts sol [keeping (Just "t") Nothing]) `closeTo` [total - 7000, 7000]
            amounts sol (fieldParts sol [keeping Nothing (Just (substance "N2O" Nothing))]) `closeTo` [total - 1191, 1191]

        it "gives a group of processes a row without metadata, never a group of a substance" $ do
            sol <- solvedFields
            amounts sol (fieldParts sol [keeping Nothing (Just (substance "N2O" Nothing)), keeping (Just "field") Nothing]) `closeTo` [2 + 2000 + 4000, 1 + 190 + 1000, 120 + 320]
            -- Every flow named holds an o: all is taken but the row without metadata.
            amounts sol (fieldParts sol [keeping Nothing (Just (EmissionCriteria (Just (NameContains, "o")) Nothing))]) `closeTo` [320, total - 320]

        it "leaves out of a part a database it keeps nothing of, and keeps all of one it keeps every emission of" $ do
            sol <- solvedFields
            let parts = fieldParts sol [keeping Nothing (Just (substance "Xenon" Nothing))]
            map (null . M.lookup "fields") (partsGroups parts) `shouldBe` [True]
            fmap (dbBiosphereTriples . fst) (M.lookup "fields" (partsRest parts)) `shouldBe` Just (dbBiosphereTriples fields)

        it "refuses a substance that would take every emission, or read in an unknown way" $ do
            isLeft (emissionFilter (EmissionCriteria Nothing Nothing)) `shouldBe` True
            isLeft (emissionFilter (EmissionCriteria (Just (IsTheSubstance, "  ")) Nothing)) `shouldBe` True
            isLeft (readSubstanceMatch "exact") `shouldBe` True

    describe "reading groups that keep substances" $ do
        let n2o = wired "N2O" "{\"flow\": {\"name\": \"N2O\"}}"
            transport = wired "Transport" "{\"name\": \"transport\"}"
            reading = either (const Nothing) (Just . attributionWord . attributionOf)
        it "reads them as their own emissions when the reading is left out, and refuses the whole chain" $ do
            reading (readGroups Nothing [transport, n2o]) `shouldBe` Just "own"
            reading (readGroups Nothing [transport]) `shouldBe` Just "first-match"
            isLeft (readGroups (Just "first-match") [transport, n2o]) `shouldBe` True
        it "refuses a way to read a name without the name, an unknown way, and an unknown medium" $ do
            isLeft (readGroups Nothing [wired "x" "{\"flow\": {\"match\": \"contains\"}}"]) `shouldBe` True
            isLeft (readGroups Nothing [wired "x" "{\"flow\": {\"name\": \"CO2\", \"match\": \"exact\"}}"]) `shouldBe` True
            isLeft (eitherDecode "{\"flow\": {\"medium\": \"aire\"}}" :: Either String GroupFilterSpec) `shouldBe` True
        it "takes a process or a flow to group by, exactly one" $ do
            either (const False) (const True) (candidateOf (Just "p") Nothing) `shouldBe` True
            isLeft (candidateOf (Just "p") (Just "f")) `shouldBe` True
            isLeft (candidateOf Nothing Nothing) `shouldBe` True
        it "offers a flow by its name in its compartment, then in any, then by its CAS" $
            map fst (flowCandidates (head fieldFlows)) `shouldBe` ["Dinitrogen monoxide (air)", "Dinitrogen monoxide", "Dinitrogen monoxide (CAS 10024-97-2, air)", "Dinitrogen monoxide (CAS 10024-97-2)"]

    describe "the starts-with classification mode" $ do
        it "takes a code and the codes under it, not a code that only contains it" $ do
            let classified = M.singleton "CPC"
                prefix = ClassificationFilter "CPC" "64" MatchPrefix
            classificationMatches prefix (classified "6411: Local transport") `shouldBe` True
            classificationMatches prefix (classified "2364: Cement") `shouldBe` False
            readClassificationMode "not-starts-with" `shouldBe` Right (Excluding MatchPrefix)

    describe "what a process could be grouped by" $ do
        let kept = cfKept . crClassifications . snd
        it "offers the product, then a code by its prefixes, a plain value, then a path by its levels" $ do
            let offered =
                    candidateCriteria
                        ["transport, freight, lorry", "ignored second product"]
                        (M.fromList [("CPC", "6511: Road transport services of freight"), ("Category type", "transport"), ("Category", "Transport\\Road\\Freight")])
            map fst offered `shouldBe` ["transport, freight, lorry", "CPC 6511", "CPC 651", "CPC 65", "transport", "Transport\\Road\\Freight", "Transport\\Road", "Transport"]
            map (crProduct . snd) (take 1 offered) `shouldBe` [Just "transport, freight, lorry"]
            kept (offered !! 3) `shouldBe` [ClassificationFilter "CPC" "65" MatchPrefix]
            kept (offered !! 6) `shouldBe` [ClassificationFilter "Category" "Transport\\Road" MatchExact, ClassificationFilter "Category" "Transport\\Road\\" MatchPrefix]
        it "takes a level of a path and what lies under it, not a sibling that starts the same" $ do
            let level = concatMap kept (take 1 (candidateCriteria [] (M.singleton "Category" "Transport\\Road")))
                answers value = any (`classificationMatches` M.singleton "Category" value) level
            map answers ["Transport\\Road", "Transport\\Road\\Freight", "Transport\\Roadside"] `shouldBe` [True, True, False]
        it "cuts a path at its backslashes when a level holds a slash, and reads a bare code as it is" $ do
            map fst (candidateCriteria [] (M.singleton "Category" "Materials\\Wood/paper")) `shouldBe` ["Materials\\Wood/paper", "Materials"]
            map fst (candidateCriteria [] (M.singleton "CPC" "31100")) `shouldBe` ["31100"]
            map fst (candidateCriteria ["  "] (M.singleton "CPC" " ")) `shouldBe` []
        it "offers no empty level of a path that starts with its separator" $
            map fst (candidateCriteria [] (M.singleton "Category" "/Agriculture")) `shouldBe` ["/Agriculture"]
        it "keeps one criterion per set of processes taken, the broadest, at the place of the narrowest" $
            distinctTakers [("product" :: Text, "a" :: String), ("CPC 651", "ab"), ("CPC 65", ""), ("ISIC 4923", "a"), ("ISIC 49", "ab")]
                `shouldBe` ["ISIC 4923", "ISIC 49"]
  where
    closeTo' got expected = abs (got - expected) `shouldSatisfy` (<= 1e-9 * max 1 (abs expected))
    isLeft = either (const True) (const False)

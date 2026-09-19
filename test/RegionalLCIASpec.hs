{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the regionalized LCIA fast path
('fillRegionalActivityWeights' + 'computeRegionalizedLCIAScore').

A self-contained synthetic fixture is built per test so the oracle doesn't
depend on any production database or on the cascade itself: the test reads
the regional CF map directly to compute expected weights.
-}
module RegionalLCIASpec (spec) where

import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Method.Explain (flowMatchKind)
import Method.Mapping
import Method.Types (
    Compartment (..),
    FlowDirection (..),
    Location (..),
    MethodCF (..),
 )
import TestHelpers (unitDef)
import Types (
    Activity (..),
    BiosphereFlow (..),
    Database (..),
    Indexes (..),
    LocationSource (..),
    Medium (..),
    SparseTriple (..),
    Unit (..),
    emptyProductIndex,
 )
import qualified Types as VT
import UnitConversion (UnitConfig (..), defaultDimensionOrder, defaultUnitConfig, mkUnitConfig)

-- ---------------------------------------------------------------------------
-- Fixture
-- ---------------------------------------------------------------------------

-- Three activities at three locations, one biosphere flow emitted by each.
-- Biosphere triples: B[F, A1]=10, B[F, A2]=20, B[F, A3]=5.
-- Locations: A1=FR, A2=DE, A3=GLO. No parent hierarchy.

flowUUID :: UUID
flowUUID = mkUUID 1

actUUID :: Int -> UUID
actUUID i = mkUUID (toInteger (1000 + i))

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnit :: Unit
kgUnit = Unit{unitId = mkUUID 9, unitName = "kg", unitSymbol = "kg", unitComment = ""}

kgUnitConfig :: UnitConfig
kgUnitConfig =
    mkUnitConfig
        defaultDimensionOrder
        (M.fromList [("kg", unitDef "mass" 1.0)])

testFlow :: BiosphereFlow
testFlow =
    BiosphereFlow
        { bfId = flowUUID
        , bfName = "Carbon dioxide"
        , bfUnitId = unitId kgUnit
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

mkActivity :: Text -> Activity
mkActivity loc =
    Activity
        { activityName = "act-" <> loc
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = []
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

-- Triples: (bioRow=0, col=i, value=v) for each activity.
mkTriples :: [(Int, Double)] -> U.Vector SparseTriple
mkTriples xs =
    U.fromList [SparseTriple 0 (fromIntegral i) v | (i, v) <- xs]

mkDB :: [(Text, Double)] -> Database
mkDB locsAndEmissions =
    let n = length locsAndEmissions
        activities = V.fromList [mkActivity loc | (loc, _) <- locsAndEmissions]
        actIdx = V.fromList [fromIntegral i :: Int32 | i <- [0 .. n - 1]]
        triples = mkTriples [(i, v) | (i, (_, v)) <- zip [0 ..] locsAndEmissions]
        emptyIdx = Indexes M.empty M.empty M.empty M.empty
     in Database
            { dbProcessIdTable = V.fromList [(actUUID i, mkUUID 0) | i <- [0 .. n - 1]]
            , dbProcessIdLookup = M.empty
            , dbActivityUUIDIndex = M.empty
            , dbProductIndex = emptyProductIndex
            , dbActivities = activities
            , dbTechFlows = M.empty
            , dbBioFlows = M.singleton flowUUID testFlow
            , dbWasteFlows = M.empty
            , dbUnits = M.singleton (unitId kgUnit) kgUnit
            , dbIndexes = emptyIdx
            , dbTechnosphereTriples = U.empty
            , dbBiosphereTriples = triples
            , dbActivityIndex = actIdx
            , dbBiosphereOrder = V.singleton flowUUID
            , dbActivityCount = fromIntegral n
            , dbBiosphereCount = 1
            , dbCrossDBLinks = []
            , dbDependsOn = []
            , dbLinkingStats = mempty
            , dbBuiltWith = VT.BuildInputs defaultUnitConfig mempty VT.Declared []
            , dbSynonymDB = Nothing
            , dbFlowsByName = M.empty
            , dbFlowsByCAS = M.empty
            , dbProductSearchIndex = M.empty
            , dbBM25Index = Nothing
            }

-- Build a method mapping with one regional CF per (location, value), all on
-- the same flow F. ByName so 'mtUuidCF' stays empty – exactly mirrors how
-- EF method CFs (whose UUIDs differ from the database flow UUIDs) get
-- resolved in production: regional cells fill 'mtRegionalizedCF', but the
-- universal broadcast for F remains empty unless a non-regional CF is added.
regionalMappings :: [(Text, Double)] -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
regionalMappings = map (\(loc, v) -> (cf loc v, Just (testFlow, ByName)))
  where
    cf loc v =
        MethodCF
            { mcfFlowRef = flowUUID
            , mcfFlowName = "Carbon dioxide"
            , mcfDirection = Output
            , mcfValue = v
            , mcfCompartment = Nothing
            , mcfCAS = Nothing
            , mcfUnit = "kg"
            , mcfConsumerLocation = Just loc
            }

buildTables ::
    Database ->
    M.Map Location [Location] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    MethodTables
buildTables db hier mappings =
    let raw = buildMethodTables OtherCFFamily mempty M.empty mappings
        withBroadcast =
            fillBroadcastVector kgUnitConfig (dbUnits db) (dbBioFlows db) raw
     in fillRegionalActivityWeights
            kgUnitConfig
            (dbUnits db)
            (dbBioFlows db)
            db
            hier
            withBroadcast

-- A second biosphere flow, one kilogram from every activity, whose factor does
-- not depend on where it happens: the universal side of the same sum.
methaneUUID :: UUID
methaneUUID = mkUUID 2

methaneFlow :: BiosphereFlow
methaneFlow = testFlow{bfId = methaneUUID, bfName = "Methane"}

withUniversalFlow :: Database -> Database
withUniversalFlow db =
    db
        { dbBioFlows = M.insert methaneUUID methaneFlow (dbBioFlows db)
        , dbBiosphereOrder = dbBiosphereOrder db <> V.singleton methaneUUID
        , dbBiosphereCount = 2
        , dbBiosphereTriples =
            dbBiosphereTriples db
                <> U.fromList [SparseTriple 1 (fromIntegral c) 1 | c <- [0 .. fromIntegral (dbActivityCount db) - 1 :: Int]]
        }

universalMapping :: Double -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
universalMapping v =
    [
        ( MethodCF
            { mcfFlowRef = methaneUUID
            , mcfFlowName = "Methane"
            , mcfDirection = Output
            , mcfValue = v
            , mcfCompartment = Just (Compartment "air" "" "")
            , mcfCAS = Nothing
            , mcfUnit = "kg"
            , mcfConsumerLocation = Nothing
            }
        , Just (methaneFlow, ByName)
        )
    ]

-- The same second flow, emitted to a compartment the consumer calls a delayed
-- long-term emission, which 'ExcludeLongTerm' drops.
withLongTermFlow :: Database -> Database
withLongTermFlow db =
    let withFlow = withUniversalFlow db
        delayed = methaneFlow{bfCompartment = Just (VT.Compartment Air (Just "long-term"))}
     in withFlow{dbBioFlows = M.insert methaneUUID delayed (dbBioFlows withFlow)}

-- The rows one database publishes under its regionalized score, by flow name.
contributionRows :: Database -> U.Vector Double -> MethodTables -> [FlowContribution]
contributionRows = contributionRowsUnder IncludeLongTerm

contributionRowsUnder :: LongTermMode -> Database -> U.Vector Double -> MethodTables -> [FlowContribution]
contributionRowsUnder ltMode db scaling tables =
    case regionalizedContributionsCrossDB kgUnitConfig (dbUnits db) (dbBioFlows db) ltMode [(db, scaling, tables)] of
        Left err -> error (T.unpack err)
        Right (rows, _unknown) -> sortOn (bfName . fcFlow) rows

-- ---------------------------------------------------------------------------
-- Oracle: independent score over the regional CF map only
-- ---------------------------------------------------------------------------

-- For each biosphere triple, look up the regional CF (with parent fallback)
-- and accumulate per-column weight. No reliance on the cascade or mtBroadcast,
-- so a bug in either won't mask a bug in fillRegionalActivityWeights.
oracleWeights :: Database -> M.Map Location [Location] -> MethodTables -> U.Vector Double
oracleWeights db hier tables = U.generate (fromIntegral (dbActivityCount db)) wForCol
  where
    regional = mtRegionalizedCF tables
    activities = dbActivities db
    actIdx = dbActivityIndex db
    bioFlows = dbBiosphereOrder db
    colToLoc =
        M.fromList
            [ (fromIntegral (actIdx V.! pid), Location (activityLocation (activities V.! pid)))
            | pid <- [0 .. V.length actIdx - 1]
            ]
    wForCol col =
        sum
            [ bioVal * cfFor flow loc
            | SparseTriple flowRow colIdx bioVal <- U.toList (dbBiosphereTriples db)
            , fromIntegral colIdx == col
            , let flow = bioFlows V.! fromIntegral flowRow
            , let loc = M.findWithDefault (Location "") col colToLoc
            ]
    cfFor flow loc =
        case M.lookup (flow, loc) regional of
            Just (CF v _) -> v
            Nothing ->
                let parents = M.findWithDefault [] loc hier
                 in case [v | p <- parents, Just (CF v _) <- [M.lookup (flow, p) regional]] of
                        (v : _) -> v
                        [] -> 0 -- no fallback used in these fixtures

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "fillRegionalActivityWeights" $ do
        it "matches an independent walk over regional CFs (no parents)" $ do
            -- A1@FR=10, A2@DE=20, A3@GLO=5; regional CFs: FR=2, DE=3, GLO=4.
            -- Expected weights: [10·2, 20·3, 5·4] = [20, 60, 20].
            let db = mkDB [("FR", 10), ("DE", 20), ("GLO", 5)]
                mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)]
                tables = buildTables db M.empty mappings
                Just raw = mtRegionalActivityWeights tables
            U.toList (rawWeights raw) `shouldBe` [20, 60, 20]
            U.toList (rawTainted raw) `shouldBe` [0, 0, 0]
            U.toList (rawWeights raw) `shouldBe` U.toList (oracleWeights db M.empty tables)

        it "walks parent regions when an activity's exact location is absent" $ do
            -- A1@FR, A2@DE; DE has parent EU; CFs: FR=2, EU=7. Activity DE
            -- finds CF via parent walk.
            let db = mkDB [("FR", 10), ("DE", 20)]
                hier = M.fromList [(Location "DE", [Location "EU"])]
                mappings = regionalMappings [("FR", 2), ("EU", 7)]
                tables = buildTables db hier mappings
                Just raw = mtRegionalActivityWeights tables
            -- A1: 10·2 = 20; A2: 20·7 = 140 (via parent EU).
            U.toList (rawWeights raw) `shouldBe` [20, 140]
            U.toList (rawTainted raw) `shouldBe` [0, 0]
            U.toList (rawWeights raw) `shouldBe` U.toList (oracleWeights db hier tables)

        it "is deterministic: two independent runs produce identical weights" $ do
            let db = mkDB [("FR", 10), ("DE", 20), ("GLO", 5)]
                mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)]
                tables1 = buildTables db M.empty mappings
                tables2 = buildTables db M.empty mappings
                Just r1 = mtRegionalActivityWeights tables1
                Just r2 = mtRegionalActivityWeights tables2
            rawWeights r1 `shouldBe` rawWeights r2
            rawTainted r1 `shouldBe` rawTainted r2
            rawMissingPairs r1 `shouldBe` rawMissingPairs r2

    describe "computeRegionalizedLCIAScore" $ do
        it "scores correctly when every activity location has a regional CF" $ do
            let db = mkDB [("FR", 10), ("DE", 20), ("GLO", 5)]
                mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)]
                tables = buildTables db M.empty mappings
                scaling = U.fromList [1, 2, 4]
            -- 1·20 + 2·60 + 4·20 = 220.
            computeRegionalizedLCIAScore
                kgUnitConfig
                (dbUnits db)
                (dbBioFlows db)
                IncludeLongTerm
                db
                scaling
                M.empty
                tables
                `shouldBe` Right 220

        it "returns partial Right when a tainted activity column carries non-zero scaling" $ do
            -- F has a regional CF only at FR. A2@DE has B[F,A2]=20 and no
            -- regional/parent/broadcast CF. With scaling[A2]=1, the column
            -- is touched-tainted and contributes 0 to the score (matches
            -- SimaPro behaviour). A1@FR still contributes 1·20=20.
            -- Build-time warnings + 'rawMissingPairs' surface the gap.
            let db = mkDB [("FR", 10), ("DE", 20)]
                mappings = regionalMappings [("FR", 2)]
                tables = buildTables db M.empty mappings
                scaling = U.fromList [1, 1]
            computeRegionalizedLCIAScore
                kgUnitConfig
                (dbUnits db)
                (dbBioFlows db)
                IncludeLongTerm
                db
                scaling
                M.empty
                tables
                `shouldBe` Right 20

        it "ignores tainted columns whose scaling is zero" $ do
            -- Same fixture as the tainted case, but with scaling[A2]=0.
            -- A2 is still tainted in rawTainted, but the dot product never
            -- touches it (sv == 0 short-circuit), so scoring returns Right.
            let db = mkDB [("FR", 10), ("DE", 20)]
                mappings = regionalMappings [("FR", 2)]
                tables = buildTables db M.empty mappings
                scaling = U.fromList [1, 0]
            computeRegionalizedLCIAScore
                kgUnitConfig
                (dbUnits db)
                (dbBioFlows db)
                IncludeLongTerm
                db
                scaling
                M.empty
                tables
                `shouldBe` Right 20

    describe "regionalizedContributionsCrossDB" $ do
        it "publishes rows that add up to the score" $ do
            -- The same fixture the score is read from above: A1@FR=10,
            -- A2@DE=20, A3@GLO=5 with CFs 2, 3, 4 and scaling [1, 2, 4],
            -- plus a second flow with one factor wherever it is emitted.
            let db = withUniversalFlow (mkDB [("FR", 10), ("DE", 20), ("GLO", 5)])
                mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)] <> universalMapping 5
                tables = buildTables db M.empty mappings
                scaling = U.fromList [1, 2, 4]
                score = computeRegionalizedLCIAScore kgUnitConfig (dbUnits db) (dbBioFlows db) IncludeLongTerm db scaling M.empty tables
                rows = contributionRows db scaling tables
            -- Regional flow: 1·10·2 + 2·20·3 + 4·5·4 = 220.
            -- Universal flow: (1·1 + 2·1 + 4·1)·5 = 35.
            score `shouldBe` Right 255
            sum (map fcContribution rows) `shouldBe` 255
            lookup "Carbon dioxide" [(bfName (fcFlow r), fcContribution r) | r <- rows] `shouldBe` Just 220
            lookup "Methane" [(bfName (fcFlow r), fcContribution r) | r <- rows] `shouldBe` Just 35

        it "carries the factor the score applied, not one of the thirty it could have shown" $ do
            -- One flow, three factors, one row. 220 of impact over the
            -- 1·10 + 2·20 + 4·5 = 70 kilograms emitted.
            let db = mkDB [("FR", 10), ("DE", 20), ("GLO", 5)]
                mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)]
                tables = buildTables db M.empty mappings
                scaling = U.fromList [1, 2, 4]
            map fcFactor (contributionRows db scaling tables) `shouldBe` [220 / 70]

        it "under-counts an uncovered location in the row exactly as in the score" $ do
            -- F has a factor at FR only; A2@DE emits 20 kg that nothing
            -- characterizes. The score is 20, and so is the row - over the
            -- 30 kilograms the inventory holds, gap included.
            let db = mkDB [("FR", 10), ("DE", 20)]
                mappings = regionalMappings [("FR", 2)]
                tables = buildTables db M.empty mappings
                scaling = U.fromList [1, 1]
                rows = contributionRows db scaling tables
            map fcContribution rows `shouldBe` [20]
            map fcFactor rows `shouldBe` [20 / 30]

        it "says a factor came from the regional table when no rung of the cascade did" $ do
            -- This flow is characterized only where it occurs, which is what
            -- the cascade cannot replay: without an answer here a row showing
            -- a contribution would claim no factor reached it.
            let db = mkDB [("FR", 10), ("DE", 20), ("GLO", 5)]
                mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)]
                tables = buildTables db M.empty mappings
            flowMatchKind tables flowUUID `shouldBe` Just "regional"
            flowMatchKind tables methaneUUID `shouldBe` Nothing

    describe "exclude_long_term on the regionalized path" $ do
        -- The same fixture as above: a regional flow worth 220 and a second
        -- flow worth 35, the second one emitted to a compartment that makes it
        -- a delayed long-term emission. Dropping it from an inventory cannot
        -- reach a path that reads columns, so the column sums carry it.
        let db = withLongTermFlow (mkDB [("FR", 10), ("DE", 20), ("GLO", 5)])
            mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)] <> universalMapping 5
            tables = buildTables db M.empty mappings
            scaling = U.fromList [1, 2, 4]
            scoreUnder m =
                computeRegionalizedLCIAScore
                    kgUnitConfig
                    (dbUnits db)
                    (dbBioFlows db)
                    m
                    db
                    scaling
                    M.empty
                    tables

        it "counts the delayed emission when the request keeps it" $ do
            scoreUnder IncludeLongTerm `shouldBe` Right 255
            sum (map fcContribution (contributionRowsUnder IncludeLongTerm db scaling tables))
                `shouldBe` 255

        it "leaves it out of the score when the request excludes it" $
            scoreUnder ExcludeLongTerm `shouldBe` Right 220

        it "leaves it out of the rows by the same triples" $ do
            let rows = contributionRowsUnder ExcludeLongTerm db scaling tables
            map (bfName . fcFlow) rows `shouldBe` ["Carbon dioxide"]
            sum (map fcContribution rows) `shouldBe` 220

    describe "regionalizedProcessContributions" $ do
        -- The same fixture: one flow emitted 10 kg at FR, 20 at DE and 5 at
        -- GLO, factors 2, 3 and 4, scaled by 1, 2 and 4.
        let db = mkDB [("FR", 10), ("DE", 20), ("GLO", 5)]
            mappings = regionalMappings [("FR", 2), ("DE", 3), ("GLO", 4)]
            tables = buildTables db M.empty mappings
            scaling = U.fromList [1, 2, 4]
            byProcess m =
                regionalizedProcessContributions
                    kgUnitConfig
                    (dbUnits db)
                    (dbBioFlows db)
                    m
                    db
                    scaling
                    tables

        it "gives each activity the term of the score it is" $
            byProcess IncludeLongTerm
                `shouldBe` Right (M.fromList [(0, 20), (1, 120), (2, 80)])

        it "adds up to the score, by construction" $ do
            let score =
                    computeRegionalizedLCIAScore
                        kgUnitConfig
                        (dbUnits db)
                        (dbBioFlows db)
                        IncludeLongTerm
                        db
                        scaling
                        M.empty
                        tables
            fmap (sum . M.elems) (byProcess IncludeLongTerm) `shouldBe` score
            score `shouldBe` Right 220

        it "leaves out an activity that contributed nothing" $ do
            -- Nothing is demanded of the DE activity, so it is not a row.
            let quiet =
                    regionalizedProcessContributions
                        kgUnitConfig
                        (dbUnits db)
                        (dbBioFlows db)
                        IncludeLongTerm
                        db
                        (U.fromList [1, 0, 4])
                        tables
            fmap M.keys quiet `shouldBe` Right [0, 2]

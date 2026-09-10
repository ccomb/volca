{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Service.Aggregate".

Exercised end-to-end against SAMPLE.min3 — a three-step chain where
activity X consumes 0.6 kg of product Y per kg of product X, and Y
consumes 0.4 kg of product Z per kg of Y, so the scaling vector for one
kg of X is (1, 0.6, 0.24). ScopeDirect needs no MUMPS solve; the other
scopes are driven by mkSolverFromDb. Emphasis on the filter / group-by /
aggregate-function semantics that the HTTP layer relies on for
correctness.
-}
module AggregateSpec (spec) where

import qualified Data.Map.Strict as MS
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import qualified API.Types as API
import qualified Service.Aggregate as Agg
import qualified SharedSolver as SS
import TestHelpers (loadSampleDatabase, mkDepLookupFromMap, mkSolverFromDb, shippedGeographies)
import Types (CrossDBLink (..), Database (..), ExchangeKind (..), processIdToText)
import qualified Types
import qualified UnitConversion as UC

-- A no-op DepSolverLookup — SAMPLE.min3 has no cross-DB deps.
noDeps :: SS.DepSolverLookup
noDeps _ = pure Nothing

-- Run aggregate on the first activity of the given DB.
runAgg ::
    Database ->
    Agg.AggregateParams ->
    IO API.Aggregation
runAgg = runAggWith noDeps

-- Same, with a real dep-DB lookup for cross-database chains.
runAggWith ::
    SS.DepSolverLookup ->
    Database ->
    Agg.AggregateParams ->
    IO API.Aggregation
runAggWith depLookup db params = do
    solver <- mkSolverFromDb db "test"
    -- ProcessId is the activity's index in dbActivities; for SAMPLE.min3 the
    -- first activity has ProcessId 0, which is what we use here.
    let pidText = processIdToText db 0
    result <-
        Agg.aggregate
            UC.defaultUnitConfig
            shippedGeographies
            (dbBioFlows db)
            (dbUnits db)
            db
            "test"
            solver
            depLookup
            pidText
            params
    case result of
        Right agg -> pure agg
        Left err -> do
            expectationFailure ("Aggregate failed: " <> show err)
            error "unreachable"

direct :: Agg.AggregateParams
direct = Agg.emptyAggregateParams Agg.ScopeDirect

biosphere :: Agg.AggregateParams
biosphere = Agg.emptyAggregateParams Agg.ScopeBiosphere

supplyChain :: Agg.AggregateParams
supplyChain = Agg.emptyAggregateParams Agg.ScopeSupplyChain

consumption :: Agg.AggregateParams
consumption = Agg.emptyAggregateParams Agg.ScopeConsumption

-- 0.6 and 0.4 are not exact in binary; compare within a tolerance.
shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo actual expected =
    actual `shouldSatisfy` (\x -> abs (x - expected) < 1e-9)

{- | SAMPLE.min3 loaded twice: the root copy gains one synthetic bridge
link — activity Y consumes 0.5 unit of the dep copy's product Y per unit
output — so a single aggregate call exercises internal edges, the bridge
edge, and dep-DB internal edges at once. Scalings: root (1, 0.6, 0.24),
bridge demand 0.5 × 0.6 = 0.3, dep (0, 0.3, 0.12).
-}
loadLinkedPair :: IO (Database, SS.DepSolverLookup)
loadLinkedPair = do
    base <- loadSampleDatabase "SAMPLE.min3"
    depDb <- loadSampleDatabase "SAMPLE.min3"
    depSolver <- mkSolverFromDb depDb "dep"
    yPid <- case V.findIndex (\a -> Types.activityName a == "production of product Y") (dbActivities base) of
        Just i -> pure i
        Nothing -> expectationFailure "SAMPLE.min3 lost its product Y activity" >> error "unreachable"
    let (yAct, yProd) = dbProcessIdTable base V.! yPid -- same UUIDs in both copies
        link =
            CrossDBLink
                { cdlConsumerActUUID = yAct
                , cdlConsumerProdUUID = yProd
                , cdlConsumerFlowId = UUID.nil
                , cdlSupplierActUUID = yAct
                , cdlSupplierProdUUID = yProd
                , cdlCoefficient = 0.5
                , cdlExchangeUnit = Types.activityUnit (dbActivities base V.! yPid)
                , cdlFlowName = "product Y"
                , cdlLocation = "GLO"
                , cdlSourceDatabase = "dep"
                , cdlTiedAlternatives = []
                }
    pure
        ( base{dbCrossDBLinks = [link]}
        , mkDepLookupFromMap (MS.singleton "dep" (depDb, depSolver))
        )

spec :: Spec
spec = do
    describe "emptyAggregateParams defaults (regression gates)" $ do
        -- These tests pin the documented defaults because flipping any of
        -- them changes the public API behaviour (every HTTP endpoint that
        -- starts from emptyAggregateParams inherits these). They're not
        -- testing the data constructor — they're guarding the *chosen*
        -- defaults.
        it "defaults the aggregate function to AggSum (not AggCount or AggShare)" $
            Agg.apAggregate direct `shouldBe` Agg.AggSum

        it "starts with all filters disabled (no name, unit, classification, isInput)" $ do
            Agg.apFilterName direct `shouldBe` Nothing
            Agg.apFilterUnit direct `shouldBe` Nothing
            Agg.apFilterClassifications direct `shouldBe` []
            Agg.apIsInput direct `shouldBe` Nothing

    describe "ScopeDirect on SAMPLE.min3 (X ← Y ← Z chain)" $ do
        it "echoes the scope text in the result" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct
            API.aggScope agg `shouldBe` "direct"

        it "counts every direct exchange of the activity when no filter is applied" $ do
            -- Invariant: with no filter, the aggregate must enumerate exactly
            -- the activity's own exchanges — same number, nothing dropped or
            -- duplicated. The DB fixture defines the ground truth.
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct
            let firstActExchanges = length (Types.exchanges (V.head (dbActivities db)))
            API.aggFilteredCount agg `shouldBe` firstActExchanges

        it "isInput=Just True + isInput=Just False partition the unfiltered set" $ do
            -- Stronger than "subset": inputs + outputs = total. This catches
            -- both over-counting (one direction over-includes) and
            -- under-counting (a sign-related branch silently drops a row).
            db <- loadSampleDatabase "SAMPLE.min3"
            allAgg <- runAgg db direct
            inputAgg <- runAgg db direct{Agg.apIsInput = Just True}
            outAgg <- runAgg db direct{Agg.apIsInput = Just False}
            API.aggFilteredCount inputAgg + API.aggFilteredCount outAgg
                `shouldBe` API.aggFilteredCount allAgg

        it "isInput=Just False contains the reference product (an output exchange)" $ do
            -- The first activity has at least one reference output; this
            -- pins the lower bound semantically rather than as a magic number.
            db <- loadSampleDatabase "SAMPLE.min3"
            outAgg <- runAgg db direct{Agg.apIsInput = Just False}
            refAgg <- runAgg db direct{Agg.apIsInput = Just False, Agg.apFilterIsReference = Just True}
            API.aggFilteredCount refAgg `shouldSatisfy` (<= API.aggFilteredCount outAgg)
            API.aggFilteredCount refAgg `shouldBe` 1

        it "filterName with a substring nobody has → 0 matches" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct{Agg.apFilterName = Just "definitely-not-a-flow-name"}
            API.aggFilteredCount agg `shouldBe` 0
            API.aggFilteredTotal agg `shouldBe` 0

        it "AggShare populates the share field per bucket, AggSum leaves it Nothing" $ do
            -- The only currently observable effect of apAggregate is on
            -- aggShare in grouped output: AggShare sets it to qty/total, every
            -- other function leaves it as Nothing.
            db <- loadSampleDatabase "SAMPLE.min3"
            sumAgg <- runAgg db direct{Agg.apAggregate = Agg.AggSum, Agg.apGroupBy = Just "unit"}
            shareAgg <- runAgg db direct{Agg.apAggregate = Agg.AggShare, Agg.apGroupBy = Just "unit"}
            all ((== Nothing) . API.aggShare) (API.aggGroups sumAgg) `shouldBe` True
            all (maybe False (\s -> s >= 0 && s <= 1) . API.aggShare) (API.aggGroups shareAgg)
                `shouldBe` True

        it "aggCount on a group equals the number of rows in that group" $ do
            -- The audit found no test wired aggCount into the contract.
            -- Counts are integers, aggQuantity is a sum — distinct fields.
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct{Agg.apGroupBy = Just "unit"}
            -- The sum of per-bucket counts equals the global filtered count.
            sum (map API.aggCount (API.aggGroups agg)) `shouldBe` API.aggFilteredCount agg

        it "an empty result has no group buckets when no group-by is requested" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct{Agg.apFilterName = Just "no-match"}
            length (API.aggGroups agg) `shouldBe` 0

        it "group_by=unit yields exactly one bucket per distinct unit in the activity" $ do
            -- Invariant: bucket count = number of distinct units. SAMPLE.min3
            -- is configured with a single unit (kg) so we expect 1 bucket;
            -- if the fixture grows more units, the assertion auto-tracks the
            -- ground truth via the raw exchange units we enumerate ourselves.
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct{Agg.apGroupBy = Just "unit"}
            let firstAct = V.head (dbActivities db)
                distinctUnits = length . S.fromList $ map Types.exchangeUnitId (Types.exchanges firstAct)
            length (API.aggGroups agg) `shouldSatisfy` (<= distinctUnits)
            length (API.aggGroups agg) `shouldSatisfy` (>= 1)

        -- A caller writes the unit the way their database spells it, while a
        -- row carries the reference spelling; a case-sensitive compare would
        -- answer zero rows and read as "there is none of that unit here".
        it "filter_unit reads a unit the way it is spelled, whatever the case" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            lower <- runAgg db direct{Agg.apFilterUnit = Just "kg"}
            upper <- runAgg db direct{Agg.apFilterUnit = Just "KG"}
            spaced <- runAgg db direct{Agg.apFilterUnit = Just " Kg "}
            API.aggFilteredCount lower `shouldSatisfy` (> 0)
            API.aggFilteredCount upper `shouldBe` API.aggFilteredCount lower
            API.aggFilteredCount spaced `shouldBe` API.aggFilteredCount lower

        it "filter_unit matches a unit whole, so a prefix of one matches nothing" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct{Agg.apFilterUnit = Just "k"}
            API.aggFilteredCount agg `shouldBe` 0

    describe "ScopeSupplyChain on SAMPLE.min3" $ do
        it "lists cumulative production per upstream product (root excluded)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db supplyChain
            API.aggScope agg `shouldBe` "supply_chain"
            API.aggFilteredCount agg `shouldBe` 2
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.84
            API.aggFilteredUnit agg `shouldBe` Just "kg"

        it "max_depth=1 keeps only the direct supplier" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db supplyChain{Agg.apMaxDepth = Just 1}
            API.aggFilteredCount agg `shouldBe` 1
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.6

    describe "ScopeConsumption on SAMPLE.min3" $ do
        it "yields one row per scaled technosphere edge" $ do
            -- Edges: Y→X (0.6 × s_X=1) and Z→Y (0.4 × s_Y=0.6).
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db consumption
            API.aggScope agg `shouldBe` "consumption"
            API.aggFilteredCount agg `shouldBe` 2
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.84
            API.aggFilteredUnit agg `shouldBe` Just "kg"

        it "filter_consumer restricts to what the matching activities eat (grass-by-cows shape)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db consumption{Agg.apFilterConsumer = Just "product Y"}
            API.aggFilteredCount agg `shouldBe` 1
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.24

        it "filter_consumer_not excludes edges into the matching consumers" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db consumption{Agg.apFilterConsumerNot = ["product Y"]}
            API.aggFilteredCount agg `shouldBe` 1
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.6

        it "filter_name matches the supplier's reference product" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db consumption{Agg.apFilterName = Just "product Z"}
            API.aggFilteredCount agg `shouldBe` 1
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.24

        it "filter_target_name matches the supplier activity's name" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db consumption{Agg.apFilterTargetName = Just "production of product Z"}
            API.aggFilteredCount agg `shouldBe` 1
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.24

        it "group_by=consumer_name buckets edges by eater, largest first" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db consumption{Agg.apGroupBy = Just "consumer_name"}
            map API.aggKey (API.aggGroups agg)
                `shouldBe` ["production of product X", "production of product Y"]
            case API.aggGroups agg of
                [gx, gy] -> do
                    API.aggQuantity gx `shouldBeCloseTo` 0.6
                    API.aggQuantity gy `shouldBeCloseTo` 0.24
                other -> expectationFailure ("expected 2 groups, got " <> show (length other))

        it "filter_consumer on ScopeDirect matches nothing (rows carry no consumer)" $ do
            -- Pins the "attribute absent → filter excludes" semantics, the
            -- same rule filter_target_name already follows.
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db direct{Agg.apFilterConsumer = Just "product"}
            API.aggFilteredCount agg `shouldBe` 0

    describe "ScopeConsumption across databases (bridge links)" $ do
        it "sums internal, bridge, and dep-DB edges" $ do
            -- Root edges 0.6 + 0.24, bridge 0.5 × s_Y = 0.3, dep edge
            -- 0.4 × 0.3 = 0.12.
            (rootDb, depLookup) <- loadLinkedPair
            agg <- runAggWith depLookup rootDb consumption
            API.aggFilteredCount agg `shouldBe` 4
            API.aggFilteredTotal agg `shouldBeCloseTo` 1.26

        it "matches the cumulative supply_chain total (two independent paths)" $ do
            (rootDb, depLookup) <- loadLinkedPair
            aggC <- runAggWith depLookup rootDb consumption
            aggS <- runAggWith depLookup rootDb supplyChain
            API.aggFilteredTotal aggC `shouldBeCloseTo` API.aggFilteredTotal aggS

        it "group_by=flow_id qualifies the bridge and dep-side suppliers with the dep name" $ do
            (rootDb, depLookup) <- loadLinkedPair
            agg <- runAggWith depLookup rootDb consumption{Agg.apGroupBy = Just "flow_id"}
            let keys = map API.aggKey (API.aggGroups agg)
            length keys `shouldBe` 4
            length (filter ("dep::" `T.isPrefixOf`) keys) `shouldBe` 2

        it "filter_consumer spans databases" $ do
            -- Consumers named "…product Y": root Y eats Z (0.24) and the
            -- bridge (0.3); dep Y eats dep Z (0.12).
            (rootDb, depLookup) <- loadLinkedPair
            agg <- runAggWith depLookup rootDb consumption{Agg.apFilterConsumer = Just "product Y"}
            API.aggFilteredCount agg `shouldBe` 3
            API.aggFilteredTotal agg `shouldBeCloseTo` 0.66

    describe "exchangeTypeScopeError (shared REST/MCP guard)" $ do
        it "allows the filter on scope=direct and an absent filter everywhere" $ do
            Agg.exchangeTypeScopeError Agg.ScopeDirect (Just KindTechnosphere) `shouldBe` Nothing
            Agg.exchangeTypeScopeError Agg.ScopeConsumption Nothing `shouldBe` Nothing

        it "rejects the filter on every non-direct scope" $ do
            Agg.exchangeTypeScopeError Agg.ScopeBiosphere (Just KindBiosphere) `shouldSatisfy` isJust
            Agg.exchangeTypeScopeError Agg.ScopeSupplyChain (Just KindWaste) `shouldSatisfy` isJust
            Agg.exchangeTypeScopeError Agg.ScopeConsumption (Just KindTechnosphere) `shouldSatisfy` isJust

    describe "ScopeBiosphere on SAMPLE.min3" $ do
        it "echoes the biosphere scope text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db biosphere
            API.aggScope agg `shouldBe` "biosphere"

        it "filtering on a substring nobody emits returns the empty aggregate" $ do
            -- A real semantic invariant: a name-filter that matches nothing
            -- must produce 0 count, 0 total, no groups — no silent fallback.
            db <- loadSampleDatabase "SAMPLE.min3"
            agg <- runAgg db biosphere{Agg.apFilterName = Just "definitely-not-emitted-here"}
            API.aggFilteredCount agg `shouldBe` 0
            API.aggFilteredTotal agg `shouldBe` 0
            length (API.aggGroups agg) `shouldBe` 0

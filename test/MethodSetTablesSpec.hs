{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the multi-method scoring path: 'MethodSetTables' +
'computeLCIAScoreSetFromTables'. Focused on correctness of the batched
matvec vs per-method dispatch and on cache-key canonicality at the pure
data-structure level.

Cache lifecycle (TVar in 'DatabaseManager') is exercised indirectly by the
existing route tests; here we keep dependencies minimal and test the pure
functions directly.
-}
module MethodSetTablesSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Matrix (Inventory)
import Method.Mapping
import Method.Types (Compartment (..), Location (..), Method (..), MethodCF (..))
import qualified Method.Types as MT
import Types (
    BiosphereFlow (..),
    Database,
    Medium (..),
    Unit (..),
 )
import qualified Types as VT
import qualified UnitConversion

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkUuid :: Int -> UUID
mkUuid n = UUID.fromWords (fromIntegral n) 0 0 0

mkUnit :: UUID -> Text -> Unit
mkUnit uid name = Unit{unitId = uid, unitName = name, unitSymbol = name, unitComment = ""}

mkFlow :: UUID -> Text -> UUID -> BiosphereFlow
mkFlow fid name uId =
    BiosphereFlow
        { bfId = fid
        , bfName = name
        , bfUnitId = uId
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

mkCF :: UUID -> Double -> MethodCF
mkCF flowRef val =
    MethodCF
        { mcfFlowRef = flowRef
        , mcfFlowName = "co2"
        , mcfDirection = MT.Output
        , mcfValue = val
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkMethod :: Int -> Text -> [MethodCF] -> Method
mkMethod n name factors =
    Method
        { methodId = mkUuid n
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg eq"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = factors
        }

-- The non-regio batched path never reads from the 'Database' argument; it
-- lives only on the per-method (regio) branch. We pass 'undefined' to avoid
-- wiring a heavyweight stub. This is safe under Haskell's laziness for the
-- non-regio tests below.
unusedDatabase :: Database
unusedDatabase = error "Database value not used in non-regio scoring"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "buildMethodSetTables" $ do
        it "puts non-regional methods in msBatched, leaves msRegional empty" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                m1 = mkMethod 1 "m1" [cf]
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))]
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                filled = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb tables0
                mst = buildMethodSetTables [(m1, filled)]
                bt = msBatched mst
            V.length (msRegional mst) `shouldBe` 0
            btNMethods bt `shouldBe` 1
            btNFlows bt `shouldBe` 1
            U.length (btMat bt) `shouldBe` 1

        it "puts regional methods in msRegional, keeps msBatched non-regional only" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                tables0 =
                    (buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))])
                        { mtRegionalizedCF = M.singleton (fid, Location "FR") (CF 2.0 (CFUnit "kg"))
                        }
                m1 = mkMethod 1 "m1" [cf]
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                filled = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb tables0
                mst = buildMethodSetTables [(m1, filled)]
                bt = msBatched mst
            V.length (msRegional mst) `shouldBe` 1
            btNMethods bt `shouldBe` 0
            btNFlows bt `shouldBe` 0
            U.null (btMat bt) `shouldBe` True

    describe "computeLCIAScoreSetFromTables (non-regio batched matvec)" $ do
        it "matches per-method computeLCIAScoreFromTables on a 3-method set" $ do
            let fid1 = mkUuid 100
                fid2 = mkUuid 101
                uidKg = mkUuid 200
                flow1 = mkFlow fid1 "co2" uidKg
                flow2 = mkFlow fid2 "ch4" uidKg
                fdb = M.fromList [(fid1, flow1), (fid2, flow2)]
                udb = M.singleton uidKg (mkUnit uidKg "kg")

                cfA1 = mkCF fid1 2.0 -- method A: co2=2, ch4 absent
                cfB1 = mkCF fid1 1.0 -- method B: co2=1, ch4=25
                cfB2 = (mkCF fid2 25.0){mcfFlowName = "ch4"}
                cfC1 = mkCF fid1 0.5 -- method C: co2=0.5, ch4 absent
                mA = mkMethod 1 "A" [cfA1]
                mB = mkMethod 2 "B" [cfB1, cfB2]
                mC = mkMethod 3 "C" [cfC1]

                fill = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb
                tA = fill (buildMethodTables OtherCFFamily M.empty M.empty [(cfA1, Just (flow1, ByUUID))])
                tB = fill (buildMethodTables OtherCFFamily M.empty M.empty [(cfB1, Just (flow1, ByUUID)), (cfB2, Just (flow2, ByUUID))])
                tC = fill (buildMethodTables OtherCFFamily M.empty M.empty [(cfC1, Just (flow1, ByUUID))])

                mst = buildMethodSetTables [(mA, tA), (mB, tB), (mC, tC)]

                inv :: Inventory
                inv = M.fromList [(fid1, 4.0), (fid2, 0.1)]

                -- Per-method legacy scores (golden)
                sA = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tA)
                sB = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tB)
                sC = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tC)

                -- Batched scores via the set path
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        inv
                        M.empty -- hier unused for non-regio
                        (NE.singleton (unusedDatabase, U.empty, mst)) -- scalingVec unused for non-regio
                resultMap = M.fromList results
            -- Sanity: explicit numbers
            sA `shouldBe` 8.0 -- 4*2 + 0.1*0 (no ch4 cf)
            sB `shouldBe` 6.5 -- 4*1 + 0.1*25
            sC `shouldBe` 2.0 -- 4*0.5 + 0
            -- Set scoring matches per-method to the bit
            M.lookup (methodId mA) resultMap `shouldBe` Just (Right sA)
            M.lookup (methodId mB) resultMap `shouldBe` Just (Right sB)
            M.lookup (methodId mC) resultMap `shouldBe` Just (Right sC)

        it "empty inventory scores all methods to 0" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                m1 = mkMethod 1 "m1" [cf]
                m2 = mkMethod 2 "m2" [cf]
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                fill = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb
                t = fill (buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))])
                mst = buildMethodSetTables [(m1, t), (m2, t)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        M.empty
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            map snd results `shouldBe` [Right 0.0, Right 0.0]

        it "inventory UUIDs absent from both broadcast and flowDB contribute 0" $ do
            -- A UUID that exists nowhere – no broadcast row, no flowDB entry –
            -- still scores zero because the per-method cascade fallback also
            -- misses it (nothing for 'lookupCascadeCF' to anchor against).
            let fidIn = mkUuid 100
                fidOut = mkUuid 999 -- in inventory only, unreachable
                uidKg = mkUuid 200
                cf = mkCF fidIn 3.0
                m1 = mkMethod 1 "m1" [cf]
                fdb = M.singleton fidIn (mkFlow fidIn "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb $
                        buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (mkFlow fidIn "co2" uidKg, ByUUID))]
                mst = buildMethodSetTables [(m1, t)]
                inv = M.fromList [(fidIn, 2.0), (fidOut, 100.0)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            -- Only the matched flow contributes: 2 × 3 = 6.
            map snd results `shouldBe` [Right 6.0]

        it "out-of-broadcast UUID resolved via mtUuidCF contributes via cascade fallback" $ do
            -- Regression gate: a merged inventory carries UUIDs whose flows
            -- were not in the root flowDB at 'fillBroadcastVector' time, so
            -- they have no row in 'btMat' and miss 'btUuidIndex'.
            -- The CF table itself ('mtUuidCF', built from the full mapping
            -- set) still resolves them – that's the per-method 'fastScore'
            -- fallback ('lookupCascadeCF'). Before the cascade fallback in
            -- the batched walker, those flows silently scored zero. This
            -- test pins the equivalence.
            let fidBuild = mkUuid 100 -- in build flowDB → in broadcast
                fidCrossDB = mkUuid 999 -- in CF table + scoring flowDB, NOT in build flowDB
                uidKg = mkUuid 200
                cfBuild = mkCF fidBuild 3.0
                cfCross = mkCF fidCrossDB 3.0
                m1 = mkMethod 1 "m1" [cfBuild, cfCross]
                -- flowDB at build time: only fidBuild. 'fillBroadcastVector'
                -- walks this, so 'mtBroadcast' / 'btUuidIndex' = {fidBuild}.
                buildFlowDB = M.singleton fidBuild (mkFlow fidBuild "co2" uidKg)
                -- flowDB at scoring time (merged inventories carry more
                -- flows than the root DB had at table-build time).
                scoringFlowDB =
                    M.fromList
                        [ (fidBuild, mkFlow fidBuild "co2" uidKg)
                        , (fidCrossDB, mkFlow fidCrossDB "co2" uidKg)
                        ]
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb buildFlowDB $
                        buildMethodTables
                            OtherCFFamily
                            M.empty
                            M.empty
                            [ (cfBuild, Just (mkFlow fidBuild "co2" uidKg, ByUUID))
                            , (cfCross, Just (mkFlow fidCrossDB "co2" uidKg, ByUUID))
                            ]
                mst = buildMethodSetTables [(m1, t)]
                inv = M.fromList [(fidBuild, 2.0), (fidCrossDB, 4.0)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        scoringFlowDB
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            -- Both contribute against CF=3: (2 + 4) × 3 = 18.
            map snd results `shouldBe` [Right 18.0]

        it "skips a flow the fill judged without a factor, still cascades one the fill never saw" $ do
            -- The fill walked fidBuild (characterized) and fidNoCF (no factor
            -- anywhere); fidCrossDB reaches the inventory later, as a what-if
            -- substitution into a database loaded since would, and resolves
            -- through the UUID rung. Only fidNoCF is judged: it costs no
            -- cascade, and the two others still score.
            let fidBuild = mkUuid 100
                fidNoCF = mkUuid 101
                fidCrossDB = mkUuid 999
                uidKg = mkUuid 200
                cfBuild = mkCF fidBuild 3.0
                cfCross = mkCF fidCrossDB 3.0
                m1 = mkMethod 1 "m1" [cfBuild, cfCross]
                buildFlowDB =
                    M.fromList
                        [ (fidBuild, mkFlow fidBuild "co2" uidKg)
                        , (fidNoCF, mkFlow fidNoCF "argon" uidKg)
                        ]
                scoringFlowDB = M.insert fidCrossDB (mkFlow fidCrossDB "co2" uidKg) buildFlowDB
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb buildFlowDB $
                        buildMethodTables
                            OtherCFFamily
                            M.empty
                            M.empty
                            [ (cfBuild, Just (mkFlow fidBuild "co2" uidKg, ByUUID))
                            , (cfCross, Just (mkFlow fidCrossDB "co2" uidKg, ByUUID))
                            ]
                mst = buildMethodSetTables [(m1, t)]
                inv = M.fromList [(fidBuild, 2.0), (fidNoCF, 5.0), (fidCrossDB, 4.0)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        scoringFlowDB
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            btJudged (msBatched mst) `shouldBe` Set.singleton fidNoCF
            map snd results `shouldBe` [Right 18.0]

    describe "msAllMethods preserves caller-given order" $ do
        it "preserves the order methods were passed in" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb $
                        buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))]
                mB = mkMethod 2 "B" [cf]
                mA = mkMethod 1 "A" [cf]
                mC = mkMethod 3 "C" [cf]
                mst = buildMethodSetTables [(mB, t), (mA, t), (mC, t)]
                ids = V.toList $ V.map mseMethodId (msAllMethods mst)
            ids `shouldBe` [methodId mB, methodId mA, methodId mC]

    describe "mixed regional + non-regional set" $ do
        -- This is the failure mode the partition fixes. Pre-PR, a single
        -- regional method anywhere in the set flipped 'msAnyRegional' to
        -- True and forced every method (regional or not) down the slow
        -- per-method walk. The partition restores the batched matvec for
        -- the non-regional half while keeping per-method dispatch for the
        -- regional half – and crucially, the merged result list must come
        -- back in caller order, not partition order.
        it "merges batched + regional scores in caller order, bit-identical to mono-method" $ do
            let fid1 = mkUuid 100
                fid2 = mkUuid 101
                uidKg = mkUuid 200
                flow1 = mkFlow fid1 "co2" uidKg
                flow2 = mkFlow fid2 "ch4" uidKg
                fdb = M.fromList [(fid1, flow1), (fid2, flow2)]
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                fill = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb
                -- Two non-regional CFs (m1 on fid1; m3 on fid1+fid2) and a
                -- regional one (m2 with a per-location override on fid1).
                cf1a = mkCF fid1 2.0
                cf2a = mkCF fid1 5.0
                cf3a = mkCF fid1 1.0
                cf3b = (mkCF fid2 25.0){mcfFlowName = "ch4"}
                tNonRegio1 =
                    fill (buildMethodTables OtherCFFamily M.empty M.empty [(cf1a, Just (flow1, ByUUID))])
                tRegio =
                    fill
                        ( (buildMethodTables OtherCFFamily M.empty M.empty [(cf2a, Just (flow1, ByUUID))])
                            { mtRegionalizedCF = M.singleton (fid1, Location "FR") (CF 7.0 (CFUnit "kg"))
                            }
                        )
                tNonRegio2 =
                    fill
                        ( buildMethodTables
                            OtherCFFamily
                            M.empty
                            M.empty
                            [ (cf3a, Just (flow1, ByUUID))
                            , (cf3b, Just (flow2, ByUUID))
                            ]
                        )
                m1 = mkMethod 1 "non-regio A" [cf1a]
                m2 = mkMethod 2 "regio" [cf2a]
                m3 = mkMethod 3 "non-regio B" [cf3a, cf3b]
                -- Interleave: [non-regio, regio, non-regio]. If the merge
                -- pulled batched-first or regional-first, this order would
                -- not survive.
                mst =
                    buildMethodSetTables
                        [(m1, tNonRegio1), (m2, tRegio), (m3, tNonRegio2)]
                inv :: Inventory
                inv = M.fromList [(fid1, 4.0), (fid2, 0.1)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            -- Caller order preserved on the result keys.
            map fst results
                `shouldBe` [methodId m1, methodId m2, methodId m3]
            -- Non-regional scores are bit-identical to scoring the same
            -- method alone via computeLCIAScoreFromTables.
            let s1 = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tNonRegio1)
                s3 = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tNonRegio2)
            case map snd results of
                r1 : _ : r3 : _ -> do
                    r1 `shouldBe` Right s1
                    r3 `shouldBe` Right s3
                _ -> expectationFailure "expected at least three scored results"

    describe "buildMethodTables mtRegionalizedCF subcomp filter" $ do
        -- Regression for the niche-subcomp clobber: ByName / synonym fan-out
        -- pairs every CF sharing a name with every flow sharing that name,
        -- so a CF with a specific subcomp (e.g. "ocean") ends up paired with
        -- flows in *other* subcomps (e.g. "(unspecified)"). With M.fromList
        -- last-write-wins, an explicit-zero niche CF would silently clobber
        -- the correct wildcard CF at the (flow, location) key. The build-time
        -- filter must drop those mismatched (cf, flow) pairs.
        it "drops fan-out pairs where CF subcomp doesn't match flow subcomp" $ do
            let fidUns = mkUuid 100
                fidOcean = mkUuid 101
                uidKg = mkUuid 200
                flowUns =
                    (mkFlow fidUns "water" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just "(unspecified)"))
                        }
                flowOcean =
                    (mkFlow fidOcean "water" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just "ocean"))
                        }
                cfUns =
                    (mkCF fidUns 3.0)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" "(unspecified)" "")
                        , mcfConsumerLocation = Just "FR"
                        }
                cfOcean =
                    (mkCF fidOcean 0.0)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" "ocean" "")
                        , mcfConsumerLocation = Just "FR"
                        }
                -- Simulate ByName fan-out: each CF paired with every flow
                -- sharing the name. Ordering matters for last-write-wins:
                -- the niche-subcomp zero CF comes *after* the wildcard CF
                -- for fidUns, so without the filter it would overwrite the
                -- correct 3.0.
                mappings =
                    [ (cfUns, Just (flowUns, ByName))
                    , (cfUns, Just (flowOcean, ByName))
                    , (cfOcean, Just (flowUns, ByName))
                    , (cfOcean, Just (flowOcean, ByName))
                    ]
                tables = buildMethodTables OtherCFFamily M.empty M.empty mappings
                regio = mtRegionalizedCF tables
            -- Pre-fix: this was (0.0, "kg") – clobbered by cfOcean. The
            -- filter drops (cfOcean, flowUns) so the wildcard cfUns survives.
            M.lookup (fidUns, Location "FR") regio `shouldBe` Just (CF 3.0 (CFUnit "kg"))
            -- The ocean flow legitimately receives the ocean CF (and the
            -- wildcard cfUns also matches, but cfOcean writes last so its
            -- explicit zero stays – that's the intended modeller behaviour).
            M.lookup (fidOcean, Location "FR") regio `shouldBe` Just (CF 0.0 (CFUnit "kg"))

        it "treats CFs with empty / (unspecified) subcomp as wildcards" $ do
            let fid = mkUuid 110
                uidKg = mkUuid 200
                flowRiver =
                    (mkFlow fid "water" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just "river"))
                        }
                cfEmpty =
                    (mkCF fid 2.0)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" "" "")
                        , mcfConsumerLocation = Just "DE"
                        }
                cfUnspecified =
                    (mkCF fid 7.0)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" "(unspecified)" "")
                        , mcfConsumerLocation = Just "DE"
                        }
                -- Bare "unspecified" (no parens) is the spelling 'compartments.csv'
                -- emits (e.g. "emissions to water,unspecified,long-term,…"); the
                -- filter once treated only "" / "(unspecified)" as wildcards and
                -- silently dropped this one. It must also act as a wildcard.
                cfUnspecBare =
                    (mkCF fid 9.0)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" "unspecified" "")
                        , mcfConsumerLocation = Just "DE"
                        }
                tEmpty = buildMethodTables OtherCFFamily M.empty M.empty [(cfEmpty, Just (flowRiver, ByName))]
                tUnspec = buildMethodTables OtherCFFamily M.empty M.empty [(cfUnspecified, Just (flowRiver, ByName))]
                tUnspecBare = buildMethodTables OtherCFFamily M.empty M.empty [(cfUnspecBare, Just (flowRiver, ByName))]
            -- All three wildcard forms apply to a specific-subcomp flow.
            M.lookup (fid, Location "DE") (mtRegionalizedCF tEmpty) `shouldBe` Just (CF 2.0 (CFUnit "kg"))
            M.lookup (fid, Location "DE") (mtRegionalizedCF tUnspec) `shouldBe` Just (CF 7.0 (CFUnit "kg"))
            M.lookup (fid, Location "DE") (mtRegionalizedCF tUnspecBare) `shouldBe` Just (CF 9.0 (CFUnit "kg"))

        it "prefers the CF that names the flow's subcompartment, whatever the row order" $ do
            -- Both a medium-level CF and an exact-subcompartment one apply to a
            -- river flow, and both land on the same (flow, location) key. The
            -- method's more specific line is the answer; before this was ranked,
            -- the winner was whichever row the file happened to list last.
            let fid = mkUuid 134
                uidKg = mkUuid 200
                flowRiver =
                    (mkFlow fid "water" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just "river"))
                        }
                cfAt sub value =
                    (mkCF fid value)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" sub "")
                        , mcfConsumerLocation = Just "DE"
                        }
                tablesFrom rows =
                    mtRegionalizedCF (buildMethodTables OtherCFFamily M.empty M.empty rows)
                medium = (cfAt "(unspecified)" 7.0, Just (flowRiver, ByName))
                exact = (cfAt "river" 3.0, Just (flowRiver, ByName))
            M.lookup (fid, Location "DE") (tablesFrom [medium, exact])
                `shouldBe` Just (CF 3.0 (CFUnit "kg"))
            M.lookup (fid, Location "DE") (tablesFrom [exact, medium])
                `shouldBe` Just (CF 3.0 (CFUnit "kg"))

        it "does not let a wildcard CF reach a sea/ocean flow (foreign medium)" $ do
            -- A freshwater CF must not characterize a sea-water release via the
            -- regionalized wildcard. The method says so itself: EF writes a
            -- sea-water factor of its own, and that line – not the freshwater
            -- one – is what a release to the sea gets.
            let fid = mkUuid 130
                uidKg = mkUuid 200
                flowOcean =
                    (mkFlow fid "water" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just "ocean"))
                        }
                cfAt sub value =
                    (mkCF fid value)
                        { mcfFlowName = "water"
                        , mcfCompartment = Just (Compartment "water" sub "")
                        , mcfConsumerLocation = Just "DE"
                        }
                declaring =
                    buildMethodTables
                        OtherCFFamily
                        M.empty
                        M.empty
                        [ (cfAt "(unspecified)" 7.0, Just (flowOcean, ByName))
                        , (cfAt "ocean" 0.0, Just (flowOcean, ByName))
                        ]
            M.lookup (fid, Location "DE") (mtRegionalizedCF declaring)
                `shouldBe` Just (CF 0.0 (CFUnit "kg"))

        it "lets a wildcard CF reach a sea/ocean flow when the method never names the sea" $ do
            -- The same shape from a method with no sea-water line anywhere. Its
            -- medium-level factor is the only thing it wrote, and refusing it
            -- would score the release as zero on an authority the method never
            -- gave. EF 3.1 has one such category, marine eutrophication, whose
            -- receiving medium is the sea.
            let fid = mkUuid 132
                uidKg = mkUuid 200
                flowOcean =
                    (mkFlow fid "nitrogen, total" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just "ocean"))
                        }
                cfUnspecified =
                    (mkCF fid 1.0)
                        { mcfFlowName = "nitrogen, total"
                        , mcfCompartment = Just (Compartment "water" "(unspecified)" "")
                        , mcfConsumerLocation = Just "DE"
                        }
                silent = buildMethodTables OtherCFFamily M.empty M.empty [(cfUnspecified, Just (flowOcean, ByName))]
            M.lookup (fid, Location "DE") (mtRegionalizedCF silent)
                `shouldBe` Just (CF 1.0 (CFUnit "kg"))

        it "does not let a wildcard CF reach a long-term groundwater flow for a USEtox method" $ do
            -- The USEtox gate is scoped to LONG-TERM groundwater: EF methods
            -- zero "groundwater, long-term" explicitly, so the surface CF must
            -- not sneak back in via the regionalized wildcard – same rule as
            -- the non-regional cascade gate. An IMMEDIATE groundwater emission
            -- inherits the unspecified CF (SimaPro subcompartment semantics;
            -- EF exports leave it implicit on purpose). A non-USEtox (e.g.
            -- nutrient) method keeps characterizing both, since phosphate
            -- migrates to surface water.
            let fid = mkUuid 131
                uidKg = mkUuid 200
                flowSub s =
                    (mkFlow fid "nickel" uidKg)
                        { bfCompartment = Just (VT.Compartment Water (Just s))
                        }
                cfUnspecified =
                    (mkCF fid 7.0)
                        { mcfFlowName = "nickel"
                        , mcfCompartment = Just (Compartment "water" "(unspecified)" "")
                        , mcfConsumerLocation = Just "DE"
                        }
                tablesFor fam sub = buildMethodTables fam M.empty M.empty [(cfUnspecified, Just (flowSub sub, ByName))]
            M.lookup (fid, Location "DE") (mtRegionalizedCF (tablesFor USEtoxFamily "groundwater, long-term")) `shouldBe` Nothing
            M.lookup (fid, Location "DE") (mtRegionalizedCF (tablesFor USEtoxFamily "groundwater")) `shouldBe` Just (CF 7.0 (CFUnit "kg"))
            M.lookup (fid, Location "DE") (mtRegionalizedCF (tablesFor OtherCFFamily "groundwater, long-term")) `shouldBe` Just (CF 7.0 (CFUnit "kg"))
            M.lookup (fid, Location "DE") (mtRegionalizedCF (tablesFor OtherCFFamily "groundwater")) `shouldBe` Just (CF 7.0 (CFUnit "kg"))

        it "keeps CF when mcfCompartment is Nothing (no subcomp info)" $ do
            let fid = mkUuid 120
                uidKg = mkUuid 200
                flowAny =
                    (mkFlow fid "water" uidKg)
                        { bfCompartment = Just (VT.Compartment Air (Just "groundwater, long-term"))
                        }
                cf =
                    (mkCF fid 4.0)
                        { mcfFlowName = "water"
                        , mcfCompartment = Nothing
                        , mcfConsumerLocation = Just "IT"
                        }
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flowAny, ByName))]
            M.lookup (fid, Location "IT") (mtRegionalizedCF tables) `shouldBe` Just (CF 4.0 (CFUnit "kg"))

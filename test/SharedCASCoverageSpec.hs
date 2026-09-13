{-# LANGUAGE OverloadedStrings #-}

{- | Reproduces the water-use sign-inversion defect with hermetic fixtures
(no production database).

Every water biosphere flow shares one CAS number (7732-18-5). The CF→flow
matcher ('mapMethodFlows') resolves each method CF to a __single__ flow, and
the scoring read path ('lookupCascadeCF') keys a flow back to a CF by its
__name__ + compartment. AWARE's resource-side CF names ("freshwater",
"river water", …) do not lexically match ecoinvent's resource-water flow
names ("Water, river", "Water, cooling, …"), so the positive consumption
side is left uncharacterized, while the emission "Water" flow (name
coincides) keeps its negative CF. A net water consumer therefore scores
__negative__ - the opposite sign of the published value.

The fixtures drive the real mapper + table build + scoring, so they track
the engine's true behaviour rather than a hand-modelled approximation.

The second group reproduces the broadcast-table pollution defect of
regionalized methods: location-specific CF rows used to land in the
name-keyed broadcast tables ('mtExactCF' / 'mtFallbackCF') alongside the
global row, where one arbitrary location's value won the key - a
water-abundant region's 0 erased the global credit, and a high-scarcity
region's factor inflated the global charge. Broadcast tables must hold
only non-regionalized CFs.
-}
module SharedCASCoverageSpec (spec) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping
import Method.ParserSimaPro (parseSimaProMethodCSVBytes)
import Method.Types (Compartment (..), FlowDirection (..), Location (..), Method (..), MethodCF (..), MethodCollection (..))
import SubstanceRegistry (CASNumber (..))
import SynonymDB (buildFromPairs, emptySynonymDB, normalizeName)
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Fixture: 3 ecoinvent water flows + an AWARE-style method, all CAS 7732-18-5
-- ---------------------------------------------------------------------------

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

waterCAS :: Text
waterCAS = "7732-18-5"

-- bfUnitId = mkUUID 0 with an empty UnitDB ⇒ identity unit conversion.
mkWaterFlow :: Integer -> Text -> VT.Medium -> BiosphereFlow
mkWaterFlow i name medium =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Just waterCAS
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment medium Nothing)
        }

-- An AWARE-style CF: resolvable only via CAS + compartment medium.
-- mcfFlowRef is a non-flow UUID so the UUID mapper misses and the CAS mapper fires.
mkWaterCF :: Text -> Text -> FlowDirection -> Double -> MethodCF
mkWaterCF name medium dir val =
    MethodCF
        { mcfFlowRef = mkUUID 999
        , mcfFlowName = name
        , mcfDirection = dir
        , mcfValue = val
        , mcfCompartment = Just (Compartment medium "" "")
        , mcfCAS = Just waterCAS
        , mcfUnit = "m3"
        , mcfConsumerLocation = Nothing
        }

-- Real ecoinvent flow names (CAS 7732-18-5).
river, cooling, emWater :: BiosphereFlow
river = mkWaterFlow 1 "Water, river" NaturalResource
cooling = mkWaterFlow 2 "Water, cooling, unspecified natural origin" NaturalResource
emWater = mkWaterFlow 3 "Water" Water

allFlows :: [BiosphereFlow]
allFlows = [river, cooling, emWater]

flowDB :: M.Map UUID BiosphereFlow
flowDB = M.fromList [(bfId f, f) | f <- allFlows]

-- AWARE-style method: positive consumption CFs on water resources, negative on
-- the water emission. Real AWARE input flow names that do NOT lexically match
-- the ecoinvent resource-flow names above.
awareMethod :: Method
awareMethod =
    Method
        { methodId = mkUUID 100
        , methodName = "Water use"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors =
            [ mkWaterCF "freshwater" "natural resource" Input 5
            , mkWaterCF "river water" "natural resource" Input 5
            , mkWaterCF "water" "natural resource" Input 5
            , mkWaterCF "Water" "water" Output (-5)
            ]
        }

-- | Same CF, but applying only to consumers in @loc@ (a regionalized row).
atLocation :: Text -> MethodCF -> MethodCF
atLocation loc cf = cf{mcfConsumerLocation = Just loc}

{- | AWARE-style regionalized method whose CF names coincide with database
flow names, so the name-keyed broadcast tables are actually read. Each
global CF is shadowed by a location row with a wildly different value:
a high-scarcity region (+100) on the charge side, a water-abundant
region (0) on the credit side.
-}
regionalizedMethod :: Method
regionalizedMethod =
    Method
        { methodId = mkUUID 101
        , methodName = "Water use (regionalized)"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors =
            [ mkWaterCF "Water, river" "natural resource" Input 5
            , atLocation "IN" (mkWaterCF "Water, river" "natural resource" Input 100)
            , mkWaterCF "Water" "water" Output (-5)
            , atLocation "GL" (mkWaterCF "Water" "water" Output 0)
            ]
        }

-- mcBioFlowsByName left empty so the CAS bridge is exercised (as for EF/ILCD,
-- whose flow UUIDs and names differ from ecoinvent's).
mapCtx :: MapContext
mapCtx =
    MapContext
        { mcBioFlowsByUUID = flowDB
        , mcBioFlowsByName = M.empty
        , mcBioFlowsByCAS = M.fromList [(waterCAS, allFlows)]
        , mcSynonymDB = emptySynonymDB
        , mcActivities = M.empty
        , mcCompartmentMap = M.empty
        , mcSynGroupFlows = M.empty
        }

-- Build scoring tables through the real mapper + table build + broadcast fill.
buildTablesFor :: Method -> IO MethodTables
buildTablesFor method = do
    mappings <- mapMethodFlows mapCtx method
    let raw = buildMethodTables OtherCFFamily M.empty M.empty mappings
    pure (fillBroadcastVector defaultUnitConfig M.empty flowDB raw)

buildTables :: IO MethodTables
buildTables = buildTablesFor awareMethod

-- ---------------------------------------------------------------------------
-- Fixture: fossil vs biogenic methane - CAS must NOT cross the name split
-- ---------------------------------------------------------------------------

-- Both methane flows share CAS 74-82-8 and medium air; only their name carries
-- the carbon-origin distinction the biogenic method splits on.
methaneCAS :: Text
methaneCAS = "74-82-8"

mkMethane :: Integer -> Text -> BiosphereFlow
mkMethane i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Just methaneCAS
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

methaneNonFossil, methaneFossil :: BiosphereFlow
methaneNonFossil = mkMethane 10 "Methane, non-fossil"
methaneFossil = mkMethane 11 "Methane, fossil"

carbonFlows :: M.Map UUID BiosphereFlow
carbonFlows = M.fromList [(bfId f, f) | f <- [methaneNonFossil, methaneFossil]]

-- Biogenic-climate-style method: the CF is named "methane (biogenic)" and
-- carries CAS 74-82-8. ecoinvent's "Methane, non-fossil" reaches it only via
-- the curated synonym below - fossil methane must stay out.
biogenicMethaneMethod :: Method
biogenicMethaneMethod =
    Method
        { methodId = mkUUID 110
        , methodName = "Climate change-Biogenic"
        , methodDescription = Nothing
        , methodUnit = "kg CO2 eq"
        , methodCategory = "Climate change"
        , methodMethodology = Nothing
        , methodFactors = [mkMethaneCF "methane (biogenic)" 27]
        }
  where
    mkMethaneCF name val =
        MethodCF
            { mcfFlowRef = mkUUID 998
            , mcfFlowName = name
            , mcfDirection = Output
            , mcfValue = val
            , mcfCompartment = Just (Compartment "air" "" "")
            , mcfCAS = Just methaneCAS
            , mcfUnit = "kg CO2 eq"
            , mcfConsumerLocation = Nothing
            }

-- The same biogenic↔non-fossil synonym the production reference data ships.
carbonSynonyms :: MapContext
carbonSynonyms =
    MapContext
        { mcBioFlowsByUUID = carbonFlows
        , mcBioFlowsByName =
            M.fromListWith
                (++)
                [(normalizeName (bfName f), [f]) | f <- [methaneNonFossil, methaneFossil]]
        , mcBioFlowsByCAS = M.fromList [(methaneCAS, [methaneNonFossil, methaneFossil])]
        , mcSynonymDB = buildFromPairs [("Methane, biogenic", "Methane, non-fossil")]
        , mcActivities = M.empty
        , mcCompartmentMap = M.empty
        , mcSynGroupFlows = M.empty
        }

buildCarbonTables :: IO MethodTables
buildCarbonTables = do
    mappings <- mapMethodFlows carbonSynonyms biogenicMethaneMethod
    let raw = buildMethodTables OtherCFFamily M.empty M.empty mappings
    pure (fillBroadcastVector defaultUnitConfig M.empty carbonFlows raw)

-- ---------------------------------------------------------------------------
-- Fixture: regionalized rows must stay out of the global tables
-- ---------------------------------------------------------------------------

-- A regionalized CAS-only-matchable CF: belongs in 'mtRegionalCasCF'.
regionalCasMethod :: Method
regionalCasMethod =
    Method
        { methodId = mkUUID 130
        , methodName = "Water use (regional CAS)"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors = [atLocation "FR" (mkWaterCF "river water" "natural resource" Input 9)]
        }

-- A global + a regionalized row, both UUID-matched to the same flow. The
-- regionalized row comes last so, were it admitted to 'mtUuidCF',
-- 'M.fromList' (last wins) would clobber the global value.
uuidRegionalMethod :: Method
uuidRegionalMethod =
    Method
        { methodId = mkUUID 131
        , methodName = "Water use (regional UUID)"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors =
            [ (mkWaterCF "global row" "natural resource" Input 5){mcfFlowRef = bfId river}
            , atLocation "IN" ((mkWaterCF "regional row" "natural resource" Input 100){mcfFlowRef = bfId river})
            ]
        }

-- ---------------------------------------------------------------------------
-- Fixture: unspecified-subcompartment fallback (air emissions)
-- ---------------------------------------------------------------------------

mkAirFlow :: Integer -> Text -> Text -> BiosphereFlow
mkAirFlow i name sub =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air (if sub == "" then Nothing else Just sub))
        }

mkAirCF :: Text -> Text -> Double -> MethodCF
mkAirCF name sub val =
    MethodCF
        { mcfFlowRef = mkUUID 999
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "air" sub "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

-- A radionuclide emitted to an uncovered subcompartment (the method has no CF
-- for "low population density, long-term"), one to a covered subcompartment,
-- and a toxicant emitted to "unspecified (long-term)" (which the method DOES
-- cover, with a 0 factor).
radonLongTerm, radonNonUrban, mercuryLongTerm :: BiosphereFlow
radonLongTerm = mkAirFlow 11 "Radon-222" "low population density, long-term"
radonNonUrban = mkAirFlow 12 "Radon-222" "non-urban air or from high stacks"
mercuryLongTerm = mkAirFlow 13 "Mercury" "unspecified (long-term)"

fallbackFlows :: [BiosphereFlow]
fallbackFlows = [radonLongTerm, radonNonUrban, mercuryLongTerm]

fallbackFlowDB :: M.Map UUID BiosphereFlow
fallbackFlowDB = M.fromList [(bfId f, f) | f <- fallbackFlows]

fallbackMethod :: Method
fallbackMethod =
    Method
        { methodId = mkUUID 102
        , methodName = "Air method"
        , methodDescription = Nothing
        , methodUnit = "kg"
        , methodCategory = "Test"
        , methodMethodology = Nothing
        , methodFactors =
            [ mkAirCF "Radon-222" "unspecified" 10
            , mkAirCF "Radon-222" "non-urban air or from high stacks" 8
            , mkAirCF "Mercury" "unspecified" 5
            , mkAirCF "Mercury" "unspecified (long-term)" 0
            ]
        }

fallbackCtx :: MapContext
fallbackCtx =
    MapContext
        { mcBioFlowsByUUID = fallbackFlowDB
        , mcBioFlowsByName = M.fromListWith (++) [(normalizeName (bfName f), [f]) | f <- fallbackFlows]
        , mcBioFlowsByCAS = M.empty
        , mcSynonymDB = emptySynonymDB
        , mcActivities = M.empty
        , mcCompartmentMap = M.empty
        , mcSynGroupFlows = M.empty
        }

buildFallbackTables :: IO MethodTables
buildFallbackTables = do
    mappings <- mapMethodFlows fallbackCtx fallbackMethod
    let raw = buildMethodTables OtherCFFamily M.empty M.empty mappings
    pure (fillBroadcastVector defaultUnitConfig M.empty fallbackFlowDB raw)

-- ---------------------------------------------------------------------------
-- Fixture: the CAS bridge broadcasts the unspecified value, not the niche max
-- ---------------------------------------------------------------------------

acrCAS :: Text
acrCAS = "107-13-1"

-- An ecoinvent flow reached only via CAS (its name matches no CF - the JRC
-- method even misspells it), whose CFs diverge wildly by subcompartment:
-- indoor air is 100x the unspecified value.
acrFlow :: BiosphereFlow
acrFlow = (mkAirFlow 21 "Acrylonitrile" "urban air close to ground"){bfCAS = Just acrCAS}

acrMethod :: Method
acrMethod =
    Method
        { methodId = mkUUID 103
        , methodName = "Tox method"
        , methodDescription = Nothing
        , methodUnit = "kg"
        , methodCategory = "Test"
        , methodMethodology = Nothing
        , methodFactors =
            [ (mkAirCF "acryolonitrile" "indoor" 100){mcfCAS = Just acrCAS}
            , (mkAirCF "acryolonitrile" "unspecified" 1){mcfCAS = Just acrCAS}
            ]
        }

acrCtx :: MapContext
acrCtx =
    MapContext
        { mcBioFlowsByUUID = M.fromList [(bfId acrFlow, acrFlow)]
        , mcBioFlowsByName = M.empty
        , mcBioFlowsByCAS = M.fromList [(acrCAS, [acrFlow])]
        , mcSynonymDB = emptySynonymDB
        , mcActivities = M.empty
        , mcCompartmentMap = M.empty
        , mcSynGroupFlows = M.empty
        }

-- Regionalized analogue of 'acrMethod': both CFs carry a location, so they
-- land in 'mtRegionalCasCF' rather than 'mtCasCF'. Same divergence - indoor
-- air is 100x the unspecified value - at one location.
acrRegionalMethod :: Method
acrRegionalMethod =
    Method
        { methodId = mkUUID 104
        , methodName = "Tox method (regional)"
        , methodDescription = Nothing
        , methodUnit = "kg"
        , methodCategory = "Test"
        , methodMethodology = Nothing
        , methodFactors =
            [ atLocation "FR" ((mkAirCF "acryolonitrile" "indoor" 100){mcfCAS = Just acrCAS})
            , atLocation "FR" ((mkAirCF "acryolonitrile" "unspecified" 1){mcfCAS = Just acrCAS})
            ]
        }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Water-use sign: CAS-shared resource flows must be characterized" $ do
    it "characterizes the emission water flow (positive control)" $ do
        tables <- buildTables
        M.member (bfId emWater) (mtBroadcast tables) `shouldBe` True

    it "characterizes a consumed resource water flow sharing the same CAS" $ do
        tables <- buildTables
        -- "Water, river" shares CAS 7732-18-5 and is a water resource. Its
        -- ecoinvent name ("water river") matches no AWARE CF name, and the CAS
        -- bridge resolved a single flow at build time - so before the CAS
        -- read-path fallback it was uncharacterized. Now it is reached by its
        -- own CAS + medium.
        M.member (bfId river) (mtBroadcast tables) `shouldBe` True

    it "scores a net water consumer positive (no sign inversion)" $ do
        tables <- buildTables
        -- Consume 10 m3 of river water, release 4 m3 ⇒ net consumption 6.
        -- Correct: 10·(+5) + 4·(−5) = +30. The old bug left river
        -- uncharacterized ⇒ 0 + 4·(−5) = −20 (wrong sign).
        let inventory = M.fromList [(bfId river, 10), (bfId emWater, 4)]
            outcome = computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables
        loScore outcome `shouldSatisfy` (> 0)

    describe "broadcast tables ignore regionalized CF rows" $ do
        it "a water-abundant region's 0 does not erase the global credit" $ do
            tables <- buildTablesFor regionalizedMethod
            M.lookup (bfId emWater) (mtBroadcast tables) `shouldBe` Just (-5)

        it "a high-scarcity region's factor does not inflate the global charge" $ do
            tables <- buildTablesFor regionalizedMethod
            M.lookup (bfId river) (mtBroadcast tables) `shouldBe` Just 5

    describe "CAS bridge defers to the name/synonym split (fossil vs biogenic)" $ do
        it "characterizes biogenic methane via its synonym to the CF name" $ do
            tables <- buildCarbonTables
            M.lookup (bfId methaneNonFossil) (mtBroadcast tables) `shouldBe` Just 27

        it "does NOT leak the biogenic CF onto fossil methane (same CAS)" $ do
            tables <- buildCarbonTables
            -- The CF pinned "Methane, non-fossil" by synonym, so it is name-
            -- discriminated and never enters the CAS broadcast table. Fossil
            -- methane shares CAS 74-82-8 but is a distinct variant the method
            -- excludes - it must stay uncharacterized, not inherit +27.
            M.member (bfId methaneFossil) (mtBroadcast tables) `shouldBe` False

    describe "regionalized rows stay out of the global tables" $ do
        it "routes a location-bearing CAS-matched CF to mtRegionalCasCF only" $ do
            mappings <- mapMethodFlows mapCtx regionalCasMethod
            let tables = buildMethodTables OtherCFFamily M.empty M.empty mappings
            M.lookup (CASNumber waterCAS, Just NaturalResource) (mtRegionalCasCF tables)
                `shouldBe` Just (M.fromList [(Location "FR", CF 9 (CFUnit "m3"))])
            M.member (CASNumber waterCAS, Just NaturalResource) (mtCasCF tables) `shouldBe` False

        it "keeps regionalized UUID-matched rows out of mtUuidCF" $ do
            mappings <- mapMethodFlows mapCtx uuidRegionalMethod
            let tables = buildMethodTables OtherCFFamily M.empty M.empty mappings
            -- The global row stands; the location row lives in the regional
            -- table instead of clobbering the flow's universal value.
            fmap teCF (M.lookup (bfId river) (mtUuidCF tables)) `shouldBe` Just (CF 5 (CFUnit "m3"))
            M.lookup (bfId river, Location "IN") (mtRegionalizedCF tables) `shouldBe` Just (CF 100 (CFUnit "m3"))

        it "vetoes the CAS bridge when name-regionalized rows disagree with the default (parser path)" $ do
            -- End-to-end through the real parser: 'parseCFRow' leaves
            -- 'mcfConsumerLocation' Nothing for SimaPro CFs (the region lives
            -- in the flow name), so location cannot dispatch the variance -
            -- the CH and IN rows disagree with the region-less default at one
            -- (CAS, medium, sub). No single value stands for the CAS then:
            -- broadcasting the region-less default used to stamp the
            -- world-average deprivation factor onto exactly the flows the
            -- method regionalizes or deliberately excludes (rain, ocean,
            -- turbined water), so the bridge refuses on both sides.
            let csv =
                    BC.pack $
                        unlines
                            [ "{SimaPro 10.2.0.0}"
                            , "{methods}"
                            , "{CSV separator: Semicolon}"
                            , "{Decimal separator: .}"
                            , ""
                            , "Impact category"
                            , "Water use;m3 depriv."
                            , "Substances"
                            , "Raw;(unspecified);Water, turbine use, unspecified natural origin, CH;007732-18-5;1.34;m3"
                            , "Raw;(unspecified);Water, turbine use, unspecified natural origin, IN;007732-18-5;100;m3"
                            , "Raw;(unspecified);Water, turbine use, unspecified natural origin;007732-18-5;42.95;m3"
                            , "Water;(unspecified);Water, CH;007732-18-5;-1.34;m3"
                            , "Water;(unspecified);Water, IN;007732-18-5;-100;m3"
                            , "Water;(unspecified);Water;007732-18-5;-42.95;m3"
                            , "End"
                            ]
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure ("Parse failed: " ++ err)
                Right coll -> do
                    mappings <- concat <$> mapM (mapMethodFlows mapCtx) (mcMethods coll)
                    let tables = buildMethodTables OtherCFFamily M.empty M.empty mappings
                    M.lookup (CASNumber waterCAS, Just NaturalResource) (mtCasCF tables)
                        `shouldBe` Nothing
                    M.lookup (CASNumber waterCAS, Just Water) (mtCasCF tables)
                        `shouldBe` Nothing

    describe "unspecified subcompartment is the medium-level fallback" $ do
        it "an uncovered subcompartment falls back to the unspecified CF" $ do
            -- Radon-222 emitted to "low population density, long-term" has no
            -- exact CF; it picks up the unspecified factor (10) instead of 0.
            tables <- buildFallbackTables
            M.lookup (bfId radonLongTerm) (mtBroadcast tables) `shouldBe` Just 10

        it "an exact subcompartment still wins over the fallback" $ do
            tables <- buildFallbackTables
            M.lookup (bfId radonNonUrban) (mtBroadcast tables) `shouldBe` Just 8

        it "an explicit (long-term) factor takes precedence over the fallback" $ do
            -- Mercury to "unspecified (long-term)" must resolve to its own 0
            -- factor, not the (nonzero) unspecified fallback - so a method that
            -- deliberately zeroes long-term emissions is honoured.
            tables <- buildFallbackTables
            M.lookup (bfId mercuryLongTerm) (mtBroadcast tables) `shouldBe` Just 0

    describe "CAS bridge broadcasts the unspecified value, not the niche max" $ do
        it "keeps the unspecified factor when subcompartment CFs diverge" $ do
            mappings <- mapMethodFlows acrCtx acrMethod
            let tables = buildMethodTables OtherCFFamily M.empty M.empty mappings
            -- indoor air is 100x; the bridge must not broadcast it.
            fmap teCF (M.lookup (CASNumber acrCAS, Just Air) (mtCasCF tables)) `shouldBe` Just (CF 1 (CFUnit "kg"))

        it "reaches the flow with the unspecified factor, not the indoor max" $ do
            mappings <- mapMethodFlows acrCtx acrMethod
            let raw = buildMethodTables OtherCFFamily M.empty M.empty mappings
                tables = fillBroadcastVector defaultUnitConfig M.empty (mcBioFlowsByUUID acrCtx) raw
            M.lookup (bfId acrFlow) (mtBroadcast tables) `shouldBe` Just 1

        it "the regional bridge keeps the unspecified value per location too" $ do
            -- FR carries both the indoor (100) and the unspecified (1) CF; the
            -- regionalized bridge must keep the medium-level default, not the
            -- niche max - same rule as the non-regional 'mtCasCF'.
            mappings <- mapMethodFlows acrCtx acrRegionalMethod
            let tables = buildMethodTables OtherCFFamily M.empty M.empty mappings
            M.lookup (CASNumber acrCAS, Just Air) (mtRegionalCasCF tables)
                `shouldBe` Just (M.fromList [(Location "FR", CF 1 (CFUnit "kg"))])

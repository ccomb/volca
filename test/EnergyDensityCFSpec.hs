{-# LANGUAGE OverloadedStrings #-}

{- | The energy-density bridge: characterize an energy-denominated CF (a JRC
fossil-resource CF in MJ, where the method assumes the inventory is already in
energy units) against an inventory flow given in mass or volume (kg, Sm3).

Native ecoinvent supplies fossil resources by mass/volume, but the EF v3.1
"Resource use, fossils" CFs are denominated in MJ with value 1.0. Without a
per-flow energy density the flow/CF unit pair is cross-dimensional, so
'convertForCharacterization' returns 0 and every fossil flow scores 0. Supplying
an energy density E (MJ per native unit) brings the flow's quantity into the
density's native unit and contributes @qNative * E@ MJ, matching the energy CF.

The fixtures drive the real table build + broadcast fill + scoring so they track
the engine's actual behaviour:
  * a kg flow + an energy CF + a density of 26.4 MJ/kg yields an effective CF of
    26.4 (not 0), and a 1 kg inventory scores 26.4;
  * a flow declared in a *different but compatible* unit (tonne) is converted
    into the density's native unit first, so 1 t scores 1000 × 26.4;
  * a flow whose unit is *dimensionally incompatible* with the native unit (m3
    vs kg) scores 0 — refuse a wrong basis rather than multiply blindly;
  * the SAME flow WITHOUT a density still yields 0 (no regression);
  * a same-dimension pair (mass flow + mass CF) is untouched by the machinery.
Plus the CSV reader: it keys by normalized name and rejects malformed rows.
-}
module EnergyDensityCFSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (fromRight, isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping
import Method.Types (Compartment (..), EnergyDensity (..), EnergyDensityMap, FlowDirection (..), MethodCF (..), buildEnergyDensityMapFromCSV)
import SynonymDB (normalizeName)
import Types (
    BiosphereFlow (..),
    Medium (..),
    Unit (..),
    UnitDB,
 )
import qualified Types as VT
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Unit metadata: a UnitConfig + a UnitDB that know kg, tonne, m3 and MJ
-- ---------------------------------------------------------------------------

-- A UnitConfig that knows two mass units (kg, tonne), a volume unit (m3) and an
-- energy unit (MJ); the shipped 'defaultUnitConfig' has no energy units, so the
-- bridge can't fire against it. Mirrors how production loads data/units.csv.
unitConfig :: UnitConfig
unitConfig =
    fromRight defaultUnitConfig $
        buildFromCSV (BLC.pack "name,dimension,factor\nkg,mass,1.0\ntonne,mass,1000.0\nm3,volume,1.0\nmj,energy,1.0\nm3-world equivalents,result*volume,1.0\n")

uidKg, uidTonne, uidM3, uidMJ :: UUID
uidKg = UUID.fromWords64 1 0
uidTonne = UUID.fromWords64 2 0
uidM3 = UUID.fromWords64 3 0
uidMJ = UUID.fromWords64 4 0

mkUnit :: UUID -> Text -> Unit
mkUnit uid name = Unit{unitId = uid, unitName = name, unitSymbol = name, unitComment = ""}

-- UnitDB so each flow's bfUnitId resolves to its declared unit.
unitDB :: UnitDB
unitDB =
    M.fromList
        [ (uidKg, mkUnit uidKg "kg")
        , (uidTonne, mkUnit uidTonne "tonne")
        , (uidM3, mkUnit uidM3 "m3")
        , (uidMJ, mkUnit uidMJ "mj")
        ]

-- ---------------------------------------------------------------------------
-- Flow + CF fixtures
-- ---------------------------------------------------------------------------

-- All coal-flow variants share this UUID so the energy CF matches by UUID
-- regardless of the declared unit; only 'bfUnitId' differs between them.
coalId :: UUID
coalId = UUID.fromWords64 10 0

-- A fossil-resource flow (like ecoinvent's "Coal, hard") in a chosen unit.
coalFlowIn :: UUID -> BiosphereFlow
coalFlowIn unitId =
    BiosphereFlow
        { bfId = coalId
        , bfName = "Coal, hard"
        , bfUnitId = unitId
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment NaturalResource Nothing)
        }

-- An energy-denominated CF (value 1.0, unit MJ) matched to the coal flow by
-- UUID, so it lands in 'mtUuidCF' and reaches the flow through the cascade.
energyCF :: MethodCF
energyCF =
    MethodCF
        { mcfFlowRef = coalId
        , mcfFlowName = "Coal, hard"
        , mcfDirection = Input
        , mcfValue = 1.0
        , mcfCompartment = Just (Compartment "natural resource" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "MJ"
        , mcfConsumerLocation = Nothing
        }

-- A same-dimension CF (mass, kg) on the same flow — the control: the
-- energy-density machinery must not perturb it.
massCF :: MethodCF
massCF = energyCF{mcfValue = 3.0, mcfUnit = "kg"}

-- Energy density for "Coal, hard": 26.4 MJ per kg. Keyed exactly as
-- 'buildEnergyDensityMapFromCSV' keys a data/energy_density.csv row.
coalDensities :: EnergyDensityMap
coalDensities = M.fromList [(normalizeName "Coal, hard", EnergyDensity 26.4 "MJ" "kg")]

-- A water resource flow in a chosen unit, for the mass→volume bridge.
waterId :: UUID
waterId = UUID.fromWords64 11 0

waterFlowIn :: UUID -> BiosphereFlow
waterFlowIn unitId = (coalFlowIn unitId){bfId = waterId, bfName = "Water"}

-- A volume-denominated CF (an AWARE-style water-scarcity factor per m³).
volumeCF :: MethodCF
volumeCF = energyCF{mcfFlowRef = waterId, mcfFlowName = "Water", mcfValue = 42.95, mcfUnit = "m3"}

-- The same factor as the ILCD collection carrying this indicator writes it:
-- the method's reference quantity is a result expression,
-- "m3-world equivalents", and the parser copies that string onto every factor.
-- It is not a volume, but it is written per one.
volumeResultCF :: MethodCF
volumeResultCF = volumeCF{mcfUnit = "m3-world equivalents"}

-- A per-kilogram water CF — the shape a non-regionalized method takes when it
-- writes water deprivation against a mass basis instead of a volume one.
waterMassCF :: MethodCF
waterMassCF = volumeCF{mcfValue = -0.042955, mcfUnit = "kg"}

-- Mass density of water: 0.001 m³ per kg — same shape as a calorific value.
waterDensities :: EnergyDensityMap
waterDensities = M.fromList [(normalizeName "Water", EnergyDensity 0.001 "m3" "kg")]

-- The same water, tagged with a US grid region the method does not enumerate.
-- Its own UUID keeps it out of 'mtUuidCF', so it can only reach a CF through
-- the region base-name fallback.
regionWaterId :: UUID
regionWaterId = UUID.fromWords64 12 0

regionWaterFlowIn :: UUID -> BiosphereFlow
regionWaterFlowIn unitId = (waterFlowIn unitId){bfId = regionWaterId, bfName = "Water, SERC"}

-- Build tables (with broadcast) for a flow + energy-density set + CF.
tablesFor :: BiosphereFlow -> EnergyDensityMap -> MethodCF -> MethodTables
tablesFor flow densities cf =
    let fdb = M.singleton (bfId flow) flow
        raw = buildMethodTables OtherCFFamily M.empty densities [(cf, Just (flow, ByUUID))]
     in fillBroadcastVector unitConfig unitDB fdb raw

{- | Tables where the CF is name-matched to the __base__ substance while the
inventory holds a __region-suffixed__ flow — the situation a non-regionalized
method meets in a regionalized database, and the only one that exercises the
region rung of both the CF lookup and the density lookup.
-}
tablesForRegion :: BiosphereFlow -> BiosphereFlow -> EnergyDensityMap -> MethodCF -> MethodTables
tablesForRegion baseFlow regionFlow densities cf =
    let fdb = M.singleton (bfId regionFlow) regionFlow
        raw = buildMethodTables OtherCFFamily M.empty densities [(cf, Just (baseFlow, ByName))]
     in fillBroadcastVector unitConfig unitDB fdb raw

-- Score a @qty@-unit inventory of @flow@ against the given tables.
scoreFlow :: BiosphereFlow -> Double -> MethodTables -> Double
scoreFlow flow qty tables =
    loScore (computeLCIAScoreFromTables unitConfig unitDB (M.singleton (bfId flow) flow) (M.singleton (bfId flow) qty) tables)

-- Effective (broadcast) CF for the coal flow, asserted within tolerance to
-- avoid IEEE-754 surprises on the conversion factors.
broadcastShouldBeNear :: MethodTables -> Double -> Expectation
broadcastShouldBeNear tables expected =
    case M.lookup coalId (mtBroadcast tables) of
        Just v -> v `shouldSatisfy` near expected
        Nothing -> expectationFailure "expected a broadcast entry for the coal flow"

near :: Double -> Double -> Bool
near expected v = abs (v - expected) < 1e-6

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "Energy-density bridge: energy CF vs. mass or volume flow" $ do
        it "applies the energy density to the effective CF (26.4, not 0)" $
            broadcastShouldBeNear (tablesFor (coalFlowIn uidKg) coalDensities energyCF) 26.4

        it "scores 1 kg of coal as 26.4 MJ" $
            scoreFlow (coalFlowIn uidKg) 1.0 (tablesFor (coalFlowIn uidKg) coalDensities energyCF)
                `shouldSatisfy` near 26.4

        it "converts a flow declared in tonne into the density's native kg first (1 t → 26400)" $ do
            let flow = coalFlowIn uidTonne
                tables = tablesFor flow coalDensities energyCF
            broadcastShouldBeNear tables 26400
            scoreFlow flow 1.0 tables `shouldSatisfy` near 26400

        it "refuses a flow whose unit is dimensionally incompatible with the native unit (m3 vs kg → 0)" $ do
            let flow = coalFlowIn uidM3
                tables = tablesFor flow coalDensities energyCF
            broadcastShouldBeNear tables 0
            scoreFlow flow 1.0 tables `shouldSatisfy` near 0

        it "leaves the energy CF at 0 when the flow has no energy density (regression guard)" $ do
            let flow = coalFlowIn uidKg
                tables = tablesFor flow M.empty energyCF
            broadcastShouldBeNear tables 0
            scoreFlow flow 1.0 tables `shouldSatisfy` near 0

        it "does not perturb a same-dimension (mass) CF, with or without a density" $ do
            let flow = coalFlowIn uidKg
            broadcastShouldBeNear (tablesFor flow coalDensities massCF) 3.0
            broadcastShouldBeNear (tablesFor flow M.empty massCF) 3.0

    describe "Density bridge generalized: volume CF vs. mass flow (water)" $ do
        -- Same mechanism, different dimension pair: a water-scarcity CF is
        -- denominated per m³ but ecoinvent water emissions come in kg. A
        -- "Water" density row (0.001 m³/kg) must bridge kg → m³ exactly like a
        -- calorific value bridges kg → MJ.
        it "applies the mass density to a per-m3 CF (1 kg water → 0.001 × CF)" $ do
            let flow = waterFlowIn uidKg
                tables = tablesFor flow waterDensities volumeCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near (0.001 * 42.95)

        it "leaves a flow already declared in m3 untouched (no double conversion)" $ do
            let flow = waterFlowIn uidM3
                tables = tablesFor flow waterDensities volumeCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near 42.95

        it "still scores 0 without a density (cross-dimensional, no bridge)" $ do
            let flow = waterFlowIn uidKg
                tables = tablesFor flow M.empty volumeCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near 0

    -- The factor as the collection writes it, in a result expression rather
    -- than in a unit. The three cases above have to come out the same, because
    -- the factor is the same factor: what the result is expressed in says
    -- nothing about what it is written per. This is also what says the row may
    -- not simply leave data/units.csv — take it away and the first case stops
    -- bridging and scores the kilogram under a per-cubic-metre factor.
    describe "The same factor written as a result expression" $ do
        it "bridges a kg flow exactly as the per-m3 spelling does" $ do
            let flow = waterFlowIn uidKg
                tables = tablesFor flow waterDensities volumeResultCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near (0.001 * 42.95)

        it "leaves a flow already in the unit the factor is written per untouched" $ do
            let flow = waterFlowIn uidM3
                tables = tablesFor flow waterDensities volumeResultCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near 42.95

        it "refuses a kg flow with no density rather than scoring the kilogram" $ do
            let flow = waterFlowIn uidKg
                tables = tablesFor flow M.empty volumeResultCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near 0

    describe "Density bridge, inverse direction: mass CF vs. volume flow" $ do
        -- The mirror of the arm above. A density relates two dimensions, and a
        -- flow can meet a CF from either side of it: here the flow is in the
        -- density's target unit (m³) and the CF in its native one (per kg), so
        -- the ratio divides instead of multiplying.
        it "divides a volume flow by the density to reach a per-kg CF" $ do
            let flow = waterFlowIn uidM3
                tables = tablesFor flow waterDensities waterMassCF
            scoreFlow flow 0.283452 tables `shouldSatisfy` near ((0.283452 / 0.001) * (-0.042955))

        it "leaves a flow already in the CF's own unit to the ordinary conversion" $ do
            let flow = waterFlowIn uidKg
                tables = tablesFor flow waterDensities waterMassCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near (-0.042955)

        it "still scores 0 without a density" $ do
            let flow = waterFlowIn uidM3
                tables = tablesFor flow M.empty waterMassCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near 0

        it "refuses a non-positive density rather than dividing by it" $ do
            let flow = waterFlowIn uidM3
                zeroDensity = M.fromList [(normalizeName "Water", EnergyDensity 0 "m3" "kg")]
                score = scoreFlow flow 1.0 (tablesFor flow zeroDensity waterMassCF)
            score `shouldSatisfy` near 0
            score `shouldSatisfy` (not . isNaN)

        it "mirrors for fuel too: an MJ flow against a per-kg CF divides by the calorific value" $ do
            let flow = coalFlowIn uidMJ
                tables = tablesFor flow coalDensities massCF
            scoreFlow flow 1.0 tables `shouldSatisfy` near ((1.0 / 26.4) * 3.0)

    describe "Region-suffixed flow, end to end" $ do
        -- The defect this whole arm exists for. The CF lookup lends "Water,
        -- SERC" the base "Water" factor, which is written per kilogram; the
        -- flow is in m³. Scoring it needs the region rung of the CF lookup, the
        -- region rung of the density lookup, and the inverse bridge — remove
        -- any one of the three and this returns 0.
        it "characterizes a region-tagged m3 flow through the base per-kg CF" $ do
            let base = waterFlowIn uidM3
                flow = regionWaterFlowIn uidM3
                tables = tablesForRegion base flow waterDensities waterMassCF
            scoreFlow flow 0.283452 tables `shouldSatisfy` near ((0.283452 / 0.001) * (-0.042955))

        it "scores 0 without the density, so the bridge is what carries it" $ do
            let base = waterFlowIn uidM3
                flow = regionWaterFlowIn uidM3
                tables = tablesForRegion base flow M.empty waterMassCF
            scoreFlow flow 0.283452 tables `shouldSatisfy` near 0

    describe "buildEnergyDensityMapFromCSV" $ do
        it "parses a valid row, keyed by normalized flow name" $
            case buildEnergyDensityMapFromCSV (BLC.pack "flow_name,value,target_unit,native_unit\n\"Coal, hard\",18.01,MJ,kg\n") of
                Left err -> expectationFailure err
                Right m -> M.lookup (normalizeName "Coal, hard") m `shouldBe` Just (EnergyDensity 18.01 "MJ" "kg")

        it "rejects a non-positive value" $
            buildEnergyDensityMapFromCSV (BLC.pack "flow_name,value,target_unit,native_unit\nPeat,0,MJ,kg\n")
                `shouldSatisfy` isLeft

        it "rejects a missing native unit" $
            buildEnergyDensityMapFromCSV (BLC.pack "flow_name,value,target_unit,native_unit\nPeat,9.76,MJ,\n")
                `shouldSatisfy` isLeft

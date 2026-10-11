{-# LANGUAGE OverloadedStrings #-}

module EnergyResourceFillSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), MethodTables, Resolution (..), buildMethodTables, cfValue, lookupCFForFlow)
import Method.Types (Compartment (..), EnergyDensity (..), EnergyDensityMap, FlowDirection (..), MethodCF (..), parseEnergyDensitySuffix)
import SynonymDB (normalizeName)
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT
import UnitConversion (UnitConfig, UnitDef (..), defaultDimensionOrder, mkUnitConfig, parseDimension)

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A method line for an extracted resource, written per `unit`.
resourceCFIn :: Text -> Text -> Double -> MethodCF
resourceCFIn unit name val =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" "" "")
        , mcfCAS = Nothing
        , mcfUnit = unit
        , mcfConsumerLocation = Nothing
        }

-- The common case: a price per unit of energy.
resourceCF :: Text -> Double -> MethodCF
resourceCF = resourceCFIn "MJ"

-- A method line for an emission, written per `unit`.
emissionCFIn :: Text -> Text -> Double -> MethodCF
emissionCFIn unit name val = (resourceCFIn unit name val){mcfCompartment = Just (Compartment "air" "" "")}

mkFlowIn :: Medium -> Integer -> Text -> BiosphereFlow
mkFlowIn medium i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment medium Nothing)
        }

mkFlow :: Integer -> Text -> BiosphereFlow
mkFlow = mkFlowIn NaturalResource

-- | The units a family's factor and its energy content are written per here.
units :: UnitConfig
units =
    mkUnitConfig
        defaultDimensionOrder
        (M.fromList [("kg", unit "mass" 1), ("t", unit "mass" 1000), ("m3", unit "volume" 1), ("Sm3", unit "volume" 1)])
  where
    unit dim = UnitDef (either (error . show) id (parseDimension defaultDimensionOrder dim))

tablesOf :: EnergyDensityMap -> [MethodCF] -> MethodTables
tablesOf eds cfs =
    buildMethodTables mempty units eds [(cf, Just (Resolution (mkFlow 1 (mcfFlowName cf)) ByName)) | cf <- cfs]

-- Tables where the method prices a unit of energy, the way both the JRC's own
-- fossil method and its SimaPro adaptation do.
coalTables :: EnergyDensityMap -> MethodTables
coalTables eds = tablesOf eds [resourceCF "Coal, hard" 1.0]

coalDensity :: EnergyDensityMap
coalDensity = M.singleton (normalizeName "Coal, hard") (EnergyDensity 18.01 "MJ" "kg")

-- Two energy lines that do not agree: the method has been asked what a unit of
-- energy costs and has not answered, so nothing is lent.
disagreeingTables :: MethodTables
disagreeingTables =
    tablesOf
        M.empty
        [ resourceCF "Coal, hard" 1.0
        , resourceCF "Coal, brown" 2.0
        ]

{- | A method that prices its energy carriers by the mass, as the SimaPro EF 3.1
adaptation prices uranium: 560 GJ in a kilo of metal. A kilo of ore holds 1.11,
so this factor belongs to that substance and to no other.
-}
byTheMassTables :: MethodTables
byTheMassTables = tablesOf M.empty [resourceCFIn "kg" "Uranium" 560000.0]

{- | An indicator of waste heat released: joules, but on the emission side. It
says what a joule leaving the system counts as, never what taking one out of
the ground costs.
-}
heatReleasedTables :: MethodTables
heatReleasedTables = tablesOf coalDensity [emissionCFIn "MJ" "Heat, waste" 1.0]

{- | A method that prices each coal family by the mass and no unit of energy, as
ecoinvent's own Ecological Scarcity 2021 file does: 80 per kg of brown coal,
160 per kg of hard coal.
-}
familyTables :: MethodTables
familyTables =
    tablesOf
        (M.fromList [(normalizeName "Coal, brown", EnergyDensity 9.41 "MJ" "kg"), (normalizeName "Coal, hard", EnergyDensity 18.01 "MJ" "kg")])
        [resourceCFIn "kg" "Coal, brown" 80.0, resourceCFIn "kg" "Coal, hard" 160.0]

borrowedBy :: MethodTables -> Text -> Maybe Double
borrowedBy tables flowName =
    fmap cfValue (lookupCFForFlow tables (mkUUID 99) (Just (mkFlow 99 flowName)))

borrowFor :: EnergyDensityMap -> Text -> Maybe Double
borrowFor eds = borrowedBy (coalTables eds)

spec :: Spec
spec = do
    describe "parseEnergyDensitySuffix" $ do
        it "parses a coal energy density (MJ per kg)" $
            parseEnergyDensitySuffix "Coal, 18 MJ per kg" `shouldBe` Just ("Coal", EnergyDensity 18 "MJ" "kg")
        it "keeps an internal qualifier in the base" $
            parseEnergyDensitySuffix "Gas, natural, 35 MJ per m3" `shouldBe` Just ("Gas, natural", EnergyDensity 35 "MJ" "m3")
        it "parses a GJ density verbatim (unit conversion is downstream)" $
            parseEnergyDensitySuffix "Uranium, 2291 GJ per kg" `shouldBe` Just ("Uranium", EnergyDensity 2291 "GJ" "kg")
        it "ignores a non-joule 'per' phrase" $
            parseEnergyDensitySuffix "Water, per capita" `shouldBe` Nothing
        it "ignores a name with no 'per'" $
            parseEnergyDensitySuffix "Methane, fossil" `shouldBe` Nothing

    describe "what a density-suffixed flow is lent" $ do
        it "is the method's price for a unit of energy" $
            borrowFor coalDensity "Coal, 18 MJ per kg" `shouldBe` Just 1.0
        it "is that same price whatever the name's density (which is applied downstream)" $
            borrowFor coalDensity "Coal, 29.3 MJ per kg" `shouldBe` Just 1.0
        it "is lent without the engine knowing a density for that resource: the name states one" $
            borrowFor M.empty "Coal, 18 MJ per kg" `shouldBe` Just 1.0
        it "is nothing when the method's energy lines disagree" $
            borrowedBy disagreeingTables "Coal, 18 MJ per kg" `shouldBe` Nothing
        it "is nothing when the method prices its carriers by the mass, not by the energy" $
            borrowedBy byTheMassTables "Uranium ore, 1.11 GJ per kg" `shouldBe` Nothing
        it "is nothing when the method's joules are emitted, not extracted" $
            borrowedBy heatReleasedTables "Coal, 18 MJ per kg" `shouldBe` Nothing
        it "is the family's price per unit of energy where the method prices the family by the mass" $
            borrowedBy familyTables "Coal, brown, 8 MJ per kg" `shouldBe` Just (80.0 / 9.41)
        it "converts the family's factor into the unit its energy content is given per" $ do
            borrowedBy (tablesOf (M.fromList [(normalizeName "Coal, brown", EnergyDensity 9.41 "MJ" "kg")]) [resourceCFIn "t" "Coal, brown" 80000.0]) "Coal, brown, 8 MJ per kg"
                `shouldBe` Just (80.0 / 9.41)
            borrowedBy (tablesOf (M.fromList [(normalizeName "Gas, natural", EnergyDensity 36.0 "MJ" "Sm3")]) [resourceCFIn "m3" "Gas, natural" 72.0]) "Gas, natural, 35 MJ per m3"
                `shouldBe` Just 2.0
        it "is nothing where the family's factor is written per a unit of another measure" $
            borrowedBy (tablesOf (M.fromList [(normalizeName "Coal, brown", EnergyDensity 9.41 "MJ" "kg")]) [resourceCFIn "m3" "Coal, brown" 80.0]) "Coal, brown, 8 MJ per kg"
                `shouldBe` Nothing
        it "is nothing where the name says no family the method prices" $
            borrowedBy familyTables "Coal, 29.3 MJ per kg" `shouldBe` Nothing
        it "is the method's price for a unit of energy before any family's" $
            borrowedBy (tablesOf coalDensity [resourceCF "Coal, hard" 1.0, resourceCFIn "kg" "Coal, brown" 80.0]) "Coal, brown, 8 MJ per kg"
                `shouldBe` Just 1.0
        it "is nothing for a name that states no energy content" $
            borrowFor coalDensity "Water, per capita" `shouldBe` Nothing
        it "is nothing for an emission: only an extracted carrier is one" $
            fmap cfValue (lookupCFForFlow (coalTables coalDensity) (mkUUID 98) (Just (mkFlowIn Air 98 "Coal, 18 MJ per kg")))
                `shouldBe` Nothing

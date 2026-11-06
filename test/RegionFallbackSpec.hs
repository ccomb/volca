{-# LANGUAGE OverloadedStrings #-}

module RegionFallbackSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec
import UnitConversion (defaultUnitConfig)

import Method.Mapping (MatchStrategy (..), MethodTables, Placing (..), Resolution (..), RungId (..), RungOutcome (..), buildMethodTables, cascadeTrail, cfValue, lookupCFForFlow, methodVocabulary, withSynonymNames)
import Method.Types (Compartment (..), EnergyDensity (..), EnergyDensityMap, FlowDirection (..), MethodCF (..), extractLocationSuffix, lookupEnergyDensity)
import SynonymDB (buildFromPairs, normalizeName)
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A method CF (emission to air) named for the base substance.
baseCF :: Text -> Double -> MethodCF
baseCF name val =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "air" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkFlow :: Integer -> Text -> BiosphereFlow
mkFlow i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

{- | Tables holding a name-matched CF for a base substance (no region tag), the
situation a non-regionalized method (e.g. EF v3.1 JRC ILCD) produces.
-}
tablesFor :: Text -> Double -> MethodTables
tablesFor base val =
    buildMethodTables mempty defaultUnitConfig M.empty [(baseCF base val, Just (Resolution (mkFlow 1 base) ByName))]

{- | The same base-substance line, in a collection whose other lines (another
category, say) are given: what the collection writes decides which regional
names the base substance may lend its factor to.
-}
tablesIn :: [MethodCF] -> Text -> Double -> MethodTables
tablesIn collection base val =
    buildMethodTables (Placing mempty (methodVocabulary mempty (baseCF base val : collection))) defaultUnitConfig M.empty [(baseCF base val, Just (Resolution (mkFlow 1 base) ByName))]

scoreIn :: MethodTables -> Text -> Maybe Double
scoreIn tables flowName = fmap cfValue (lookupCFForFlow tables (mkUUID 99) (Just (mkFlow 99 flowName)))

regionStep :: MethodTables -> Text -> Maybe RungOutcome
regionStep tables flowName = lookup RungRegionBase (cascadeTrail tables (M.singleton (mkUUID 99) (mkFlow 99 flowName)) (mkUUID 99))

-- | A line of another category, for the regional name itself, in water.
inWater :: Text -> MethodCF
inWater name = (baseCF name 940){mcfCompartment = Just (Compartment "water" "" "")}

-- | Score a flow of the given name against those tables.
scoreOf :: Text -> Double -> Text -> Maybe Double
scoreOf base val flowName =
    fmap cfValue (lookupCFForFlow (tablesFor base val) (mkUUID 99) (Just (mkFlow 99 flowName)))

spec :: Spec
spec = do
    describe "extractLocationSuffix" $ do
        it "splits a real region code off the base name" $
            extractLocationSuffix "Ammonia, FR" `shouldBe` ("Ammonia", Just "FR")
        it "splits a regional aggregate" $
            extractLocationSuffix "Ammonia, RER" `shouldBe` ("Ammonia", Just "RER")
        it "leaves a lowercase chemical qualifier intact" $
            extractLocationSuffix "Methane, fossil" `shouldBe` ("Methane, fossil", Nothing)
        it "leaves 'ion' intact" $
            extractLocationSuffix "Arsenic, ion" `shouldBe` ("Arsenic, ion", Nothing)

    describe "region base-name fallback in the score lookup" $ do
        it "characterizes a region-suffixed flow via the base substance CF" $
            scoreOf "Ammonia" 11.5 "Ammonia, FR" `shouldBe` Just 11.5
        it "characterizes a regional-aggregate suffix too" $
            scoreOf "Ammonia" 11.5 "Ammonia, RER" `shouldBe` Just 11.5
        it "does NOT borrow the base CF for a chemical qualifier (', fossil')" $
            scoreOf "Methane" 28.0 "Methane, fossil" `shouldBe` Nothing
        it "still characterizes the base flow itself" $
            scoreOf "Ammonia" 11.5 "Ammonia" `shouldBe` Just 11.5

    describe "a regional name the method's collection writes itself" $ do
        let named = tablesIn [baseCF "Ammonia, FR" 30] "Ammonia" 11.5
        it "gets no factor lent by its base substance" $
            scoreIn named "Ammonia, FR" `shouldBe` Nothing
        it "says the loan was declined, not that the step did not apply" $
            regionStep named "Ammonia, FR" `shouldBe` Just RungDeclined
        it "leaves the base substance its own factor" $
            scoreIn named "Ammonia" `shouldBe` Just 11.5
        it "leaves another region of the same substance its loan" $
            scoreIn named "Ammonia, DE" `shouldBe` Just 11.5
        it "reads the written base name whatever its case" $
            scoreIn (tablesIn [baseCF "AMMONIA, FR" 30] "Ammonia" 11.5) "Ammonia, FR" `shouldBe` Nothing
        it "counts only a name written in the flow's own medium" $
            scoreIn (tablesIn [inWater "Ammonia, FR"] "Ammonia" 11.5) "Ammonia, FR" `shouldBe` Just 11.5
        it "counts a name an exclusion line writes" $
            scoreIn (tablesIn [baseCF "!Ammonia, FR" 0] "Ammonia" 11.5) "Ammonia, FR" `shouldBe` Nothing
        it "counts a name a synonym of the written one gives" $
            let synonyms = buildFromPairs [("Ammonia", "Azane")]
                vocabulary = withSynonymNames synonyms (methodVocabulary mempty [baseCF "Azane, FR" 30])
                tables = buildMethodTables (Placing mempty vocabulary) defaultUnitConfig M.empty [(baseCF "Ammonia" 11.5, Just (Resolution (mkFlow 1 "Ammonia") ByName))]
             in scoreIn tables "Ammonia, FR" `shouldBe` Nothing
        it "gets no factor lent through a CAS number either" $
            let cas = "7664-41-7"
                line = (baseCF "Azane" 11.5){mcfCAS = Just cas}
                flow = (mkFlow 99 "Ammonia, FR"){bfCAS = Just cas}
                tablesWith collection =
                    buildMethodTables (Placing mempty (methodVocabulary mempty (line : collection))) defaultUnitConfig M.empty [(line, Just (Resolution flow ByCAS))]
                score tables = fmap cfValue (lookupCFForFlow tables (mkUUID 98) (Just flow{bfId = mkUUID 98}))
             in (score (tablesWith []), score (tablesWith [baseCF "Ammonia, FR" 30])) `shouldBe` (Just 11.5, Nothing)

    describe "lookupEnergyDensity strips the same suffix the CF lookup strips" $ do
        -- A borrowed CF carries the base substance's unit, so the density that
        -- bridges to it has to be findable under the base substance's name. If
        -- these two lookups disagree on the key, the flow holds a factor of a
        -- dimension it cannot reach and scores 0.
        it "finds the base substance's density for a region-suffixed flow" $
            lookupEnergyDensity waterRow "Water, SERC" `shouldBe` Just (EnergyDensity 0.001 "m3" "kg")
        it "prefers a row written for the region-suffixed name itself" $
            let rows = M.insert (normalizeName "Water, SERC") (EnergyDensity 0.002 "m3" "kg") waterRow
             in lookupEnergyDensity rows "Water, SERC" `shouldBe` Just (EnergyDensity 0.002 "m3" "kg")
        it "does not borrow across a chemical qualifier" $
            lookupEnergyDensity coalRow "Coal, hard" `shouldBe` Nothing
        it "still reads a density the name itself spells out" $
            lookupEnergyDensity M.empty "Coal, 18 MJ per kg" `shouldBe` Just (EnergyDensity 18 "MJ" "kg")

waterRow :: EnergyDensityMap
waterRow = M.singleton (normalizeName "Water") (EnergyDensity 0.001 "m3" "kg")

-- Keyed on the bare family name: ", hard" is a qualifier, not a region, so
-- 'extractLocationSuffix' leaves it alone and nothing is borrowed.
coalRow :: EnergyDensityMap
coalRow = M.singleton (normalizeName "Coal") (EnergyDensity 26.4 "MJ" "kg")

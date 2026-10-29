{-# LANGUAGE OverloadedStrings #-}

{- | The flows a score probably missed: those it read no factor for, although
the method writes the same substance in the flow's medium at another
subcompartment, where a flow never reads.
-}
module ProbableMissesSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec
import UnitConversion (defaultUnitConfig)

import Method.Mapping (MatchStrategy (..), MethodTables, Placing (..), ProbableMiss (..), Resolution (..), SiblingLines (..), buildMethodTables, fillBroadcastVector, methodVocabulary, missesWorthSaying, pmUpTo, probableMisses)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..), Subcompartment (..))
import Types (BioFlowDB, BiosphereFlow (..), Medium (..), Unit (..), UnitDB)
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgId :: UUID
kgId = mkUUID 0

units :: UnitDB
units = M.singleton kgId Unit{unitId = kgId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

line :: Text -> Text -> Text -> Double -> MethodCF
line name medium sub val =
    MethodCF
        { mcfFlowRef = mkUUID 100
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment medium sub "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

flowAt :: Integer -> Text -> Medium -> Maybe Text -> BiosphereFlow
flowAt i name medium sub =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = kgId
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment medium sub)
        }

-- 0.02 kg of ammonia emitted indoors.
indoor :: BiosphereFlow
indoor = flowAt 1 "Ammonia" Air (Just "indoor")

tablesOf :: [MethodCF] -> MethodTables
tablesOf cfs = buildMethodTables mempty defaultUnitConfig M.empty [(cf, Nothing) | cf <- cfs]

missesOf :: MethodTables -> [(BiosphereFlow, Double)] -> [ProbableMiss]
missesOf tables inventory = probableMisses flows (fillBroadcastVector defaultUnitConfig units flows tables) (M.fromList [(bfId f, q) | (f, q) <- inventory])
  where
    flows :: BioFlowDB
    flows = M.fromList [(bfId f, f) | (f, _) <- inventory]

urbanAndRural :: [MethodCF]
urbanAndRural = [line "Ammonia" "air" "urban" 3.02, line "Ammonia" "air" "non-urban" 3.02]

spec :: Spec
spec = describe "probableMisses" $ do
    it "names a flow whose substance the method writes at other subcompartments of its medium" $ do
        let found = missesOf (tablesOf urbanAndRural) [(indoor, 0.02)]
        map (slWrittenAt . pmLines) found `shouldBe` [[Subcompartment "non-urban", Subcompartment "urban"]]
        map (slFactor . pmLines) found `shouldBe` [3.02]
        map pmUpTo found `shouldBe` [0.02 * 3.02]

    it "names nothing when the method writes the substance for the whole medium" $
        map pmUpTo (missesOf (tablesOf (line "Ammonia" "air" "" 3.02 : urbanAndRural)) [(indoor, 0.02)]) `shouldBe` []

    it "names nothing where the collection writes the flow's own place for another substance" $ do
        -- The method has heard of indoor air: leaving ammonia out there is its choice.
        let placing = Placing mempty (methodVocabulary mempty [line "Formaldehyde" "air" "indoor" 1.0])
            tables = buildMethodTables placing defaultUnitConfig M.empty [(cf, Nothing) | cf <- urbanAndRural]
        map pmUpTo (missesOf tables [(indoor, 0.02)]) `shouldBe` []

    it "names nothing when the method writes the substance only in another medium" $
        map pmUpTo (missesOf (tablesOf [line "Ammonia" "water" "surface water" 3.02]) [(indoor, 0.02)]) `shouldBe` []

    it "reads a CAS line the method lets stand for one substance" $ do
        let cfs = [(withCas (line "Ammonia" "air" "urban" 3.02), Just (Resolution urbanFlow ByCAS))]
            urbanFlow = (flowAt 2 "Ammonia" Air (Just "urban")){bfCAS = Just "7664-41-7"}
            renamed = (flowAt 3 "Ammonia, as N" Air (Just "indoor")){bfCAS = Just "7664-41-7"}
        map pmUpTo (missesOf (buildMethodTables mempty defaultUnitConfig M.empty cfs) [(renamed, 0.02)]) `shouldBe` [0.02 * 3.02]

    it "names nothing through a CAS the method tells apart by name" $ do
        -- Two values for one CAS at one place: the CAS stands for no single substance.
        let cfs =
                [ (withWater (line "Water, lake" "natural resource" "in water" 1.0), Just (Resolution (waterAt 2 "Water, lake") ByCAS))
                , (withWater (line "Water, river" "natural resource" "in water" 2.0), Just (Resolution (waterAt 3 "Water, river") ByCAS))
                ]
            well = (flowAt 4 "Water, well" NaturalResource (Just "in ground")){bfCAS = Just "7732-18-5"}
        map pmUpTo (missesOf (buildMethodTables mempty defaultUnitConfig M.empty cfs) [(well, 1)]) `shouldBe` []

    it "names nothing for a flow a regionalized table characterizes" $ do
        let located = (line "Ammonia" "air" "indoor" 5.0){mcfConsumerLocation = Just "FR"}
            cfs = [(cf, Nothing) | cf <- urbanAndRural] ++ [(located, Just (Resolution indoor ByUUID))]
        map pmUpTo (missesOf (buildMethodTables mempty defaultUnitConfig M.empty cfs) [(indoor, 0.02)]) `shouldBe` []

    describe "missesWorthSaying" $ do
        let found = missesOf (tablesOf urbanAndRural) [(indoor, 0.02)]
        it "drops a miss below a thousandth of the score" $
            map pmUpTo (missesWorthSaying 1000 found) `shouldBe` []
        it "keeps one at a thousandth or more" $
            map pmUpTo (missesWorthSaying 0.5 found) `shouldBe` [0.02 * 3.02]
        it "keeps every miss beside a zero score" $
            map pmUpTo (missesWorthSaying 0 found) `shouldBe` [0.02 * 3.02]
  where
    withCas cf = cf{mcfCAS = Just "7664-41-7"}
    withWater cf = cf{mcfCAS = Just "7732-18-5", mcfDirection = Input}
    waterAt i name = (flowAt i name NaturalResource (Just "in water")){bfCAS = Just "7732-18-5"}

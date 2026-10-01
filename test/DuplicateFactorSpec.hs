{-# LANGUAGE OverloadedStrings #-}

module DuplicateFactorSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (ContestedFactor (..), MatchStrategy (..), buildMethodTables, cfValue, contestedFactors, lookupCFForFlow)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import Types (BiosphereFlow (..), Medium (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A format that derives a flow's identifier from its name and compartment
-- gives both lines of a doubled flow the same one.
turpentineCF :: Text -> Double -> MethodCF
turpentineCF sub val =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = "turpentine"
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "soil" sub "")
        , mcfCAS = Just "008006-64-2"
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

turpentine :: BiosphereFlow
turpentine =
    BiosphereFlow
        { bfId = mkUUID 1
        , bfName = "turpentine"
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Just "008006-64-2"
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Soil (Just "agricultural"))
        }

mappingsOf :: MatchStrategy -> [MethodCF] -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mappingsOf how cfs = [(cf, Just (turpentine, how)) | cf <- cfs]

score :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> Maybe Double
score mappings =
    fmap cfValue (lookupCFForFlow (buildMethodTables mempty mempty M.empty mappings) (bfId turpentine) (Just turpentine))

contested :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> [([Double], Double)]
contested mappings =
    [(cfoValues c, cfoKept c) | c <- contestedFactors mempty (buildMethodTables mempty mempty M.empty mappings) mappings]

spec :: Spec
spec = describe "two factors at one place" $ do
    let large = turpentineCF "agricultural" 8.399
        small = turpentineCF "agricultural" 1.1619

    it "reads the same value whatever the order of the lines, matched by identifier" $ do
        score (mappingsOf ByUUID [large, small]) `shouldBe` Just 8.399
        score (mappingsOf ByUUID [small, large]) `shouldBe` Just 8.399

    it "reads the value a name match reads" $
        score (mappingsOf ByName [small, large]) `shouldBe` score (mappingsOf ByUUID [small, large])

    it "names the place, every value and the one read" $
        contested (mappingsOf ByUUID [small, large]) `shouldBe` [([1.1619, 8.399], 8.399)]

    it "is no contest when the lines agree" $
        contested (mappingsOf ByUUID [large, large]) `shouldBe` []

    it "is no contest across two compartments" $
        contested (mappingsOf ByUUID [large, turpentineCF "non-agricultural" 1.1619]) `shouldBe` []

    it "leaves out lines this database matched to nothing" $
        contested [(small, Nothing), (large, Nothing)] `shouldBe` []

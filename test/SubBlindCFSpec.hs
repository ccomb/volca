{-# LANGUAGE OverloadedStrings #-}

module SubBlindCFSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), buildMethodTables, cfValue, lookupCFForFlow)
import Method.Types (CFFamily (..), Compartment (..), FlowDirection (..), MethodCF (..))
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A resource CF at a specific subcompartment.
mkCF :: Integer -> Text -> Text -> Double -> MethodCF
mkCF i name sub val =
    MethodCF
        { mcfFlowRef = mkUUID i
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" sub "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkFlow :: Integer -> Text -> Maybe Text -> BiosphereFlow
mkFlow i name sub =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment NaturalResource sub)
        }

score :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> BiosphereFlow -> Maybe Double
score mappings flow =
    fmap cfValue (lookupCFForFlow (buildMethodTables OtherCFFamily M.empty M.empty mappings) (bfId flow) (Just flow))

spec :: Spec
spec = describe "sub-blind CF fallback" $ do
    it "borrows a sub-specific CF for an unspecified-sub flow when the factor is sub-independent" $ do
        -- "Cadmium, in ground" = 0.157; the method has no unspecified entry.
        let mappings = [(mkCF 1 "Cadmium" "in ground" 0.157, Just (mkFlow 1 "Cadmium" (Just "in ground"), ByName))]
        score mappings (mkFlow 99 "Cadmium" Nothing) `shouldBe` Just 0.157

    it "still resolves the sub-specific flow itself" $ do
        let mappings = [(mkCF 1 "Cadmium" "in ground" 0.157, Just (mkFlow 1 "Cadmium" (Just "in ground"), ByName))]
        score mappings (mkFlow 1 "Cadmium" (Just "in ground")) `shouldBe` Just 0.157

    it "does NOT guess when the factor varies by subcompartment (ambiguous)" $ do
        -- Mercury differs by sub: in ground 1.0, in water 2.0 - no safe default.
        let mappings =
                [ (mkCF 1 "Mercury" "in ground" 1.0, Just (mkFlow 1 "Mercury" (Just "in ground"), ByName))
                , (mkCF 2 "Mercury" "in water" 2.0, Just (mkFlow 2 "Mercury" (Just "in water"), ByName))
                ]
        score mappings (mkFlow 99 "Mercury" Nothing) `shouldBe` Nothing

    describe "resource base-name fallback (ore-grade variants)" $ do
        -- The method characterizes the base element; ecoinvent supplies dozens
        -- of ore-grade variants ("Copper, 0.99% in sulfide, Cu 0.36% …, in
        -- ground") that carry no CAS and match no CF of their own. Their
        -- reference amount is the mass of the element, so they take its CF.
        let copperCF = mkCF 1 "Copper" "in ground" 1.37e-6
            copperMapping = [(copperCF, Just (mkFlow 1 "Copper" (Just "in ground"), ByName))]

        it "characterizes an ore-grade variant with the base element's CF" $
            score copperMapping (mkFlow 99 "Copper, 0.99% in sulfide, Cu 0.36% and Mo 8.2E-3% in crude ore" (Just "in ground"))
                `shouldBe` Just 1.37e-6

        it "leaves a variant alone when the method has no CF for the base" $
            score copperMapping (mkFlow 99 "Calcite, in ground" Nothing)
                `shouldBe` Nothing

        it "does not fire for a comma-less resource name" $
            score copperMapping (mkFlow 99 "Gravel" (Just "in ground"))
                `shouldBe` Nothing

        it "does not fire for a comma-qualified name without a grade marker" $ do
            -- The "%" pins the fallback to ore-grade variants: an ordinary
            -- comma-qualified resource must not borrow the base CF (a
            -- salt-water withdrawal is not freshwater scarcity).
            let waterMapping = [(mkCF 1 "Water" "in water" 42.0, Just (mkFlow 1 "Water" (Just "in water"), ByName))]
            score waterMapping (mkFlow 99 "Water, salt, ocean" (Just "in water"))
                `shouldBe` Nothing

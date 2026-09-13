{-# LANGUAGE OverloadedStrings #-}

{- | The CAS bridge ('mtCasCF') characterizes a flow the cascade could not
match by name through a factor line sharing its CAS number. That is only
sound when the CAS identifies one factor: a method whose lines carry
different values at one (CAS, medium, subcompartment) – water is the
canonical case, one CAS across regional name variants and deliberate
exclusions like rain or turbined water – distinguishes flows by something
the name-blind bridge cannot see, so the bridge must refuse rather than
stamp an arbitrary value onto exactly the flows the method separated.
This pins the refusal and its two deliberate non-voters – consumer-located
rows (dispatched by the regional tables) and rows at different
subcompartments (arbitrated to the medium-level default) – and the pairing
where located rows do end up voting: against a name-suffixed database the
regional projection materializes them into region-less copies, and both
CAS bridges must refuse.
-}
module CASBridgeAmbiguitySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), buildMethodTables, cfValue, lookupCFForFlow, mtCasCF, mtRegionalCasCF, projectRegionalResourceFlows)
import Method.Types (CFFamily (..), Compartment (..), FlowDirection (..), MethodCF (..))
import SubstanceRegistry (CASNumber (..))
import SynonymDB (emptySynonymDB)
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

mkCF :: Integer -> Text -> Text -> Maybe Text -> Double -> MethodCF
mkCF i name sub cas val =
    MethodCF
        { mcfFlowRef = mkUUID i
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" sub "")
        , mcfCAS = cas
        , mcfUnit = "m3"
        , mcfConsumerLocation = Nothing
        }

mkFlow :: Integer -> Text -> Maybe Text -> BiosphereFlow
mkFlow i name cas =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = cas
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment NaturalResource Nothing)
        }

water :: Maybe Text
water = Just "7732-18-5"

score :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> BiosphereFlow -> Maybe Double
score mappings flow =
    fmap cfValue (lookupCFForFlow (buildMethodTables OtherCFFamily M.empty M.empty mappings) (bfId flow) (Just flow))

spec :: Spec
spec = describe "CAS bridge ambiguity guard" $ do
    it "refuses to bridge a CAS whose same-subcompartment lines disagree (regionalized water)" $ do
        -- The region-less default matched its flow by name; the CH variant has
        -- no same-named flow and landed on a water flow through its CAS –
        -- which is what arms the bridge. Their values differ at the same
        -- (CAS, medium, sub), so no single value stands for the CAS.
        let mappings =
                [ (mkCF 1 "Water" "" water 42.955, Just (mkFlow 1 "Water" water, ByName))
                , (mkCF 2 "Water, lake, CH" "" water 1.44, Just (mkFlow 1 "Water" water, ByCAS))
                ]
        -- Turbined water: same CAS, no CF of its own – deliberately excluded
        -- by the method, and it must stay that way.
        score mappings (mkFlow 99 "Water, turbine use" water) `shouldBe` Nothing

    it "an unmatched row votes: it proves the discrimination without resolving to a flow" $ do
        -- "Water, lake, AT" matches nothing in this database, but its value
        -- still shows the method regionalizes water – the bridge must refuse.
        let mappings =
                [ (mkCF 1 "Water" "" water 42.955, Just (mkFlow 1 "Water" water, ByCAS))
                , (mkCF 2 "Water, lake, AT" "" water 1.89, Nothing)
                ]
        score mappings (mkFlow 99 "Water, turbine use" water) `shouldBe` Nothing

    it "still bridges a CAS with one factor line" $ do
        let mappings =
                [(mkCF 1 "Chlorpyrifos" "" (Just "2921-88-2") 5.0, Just (mkFlow 1 "Chlorpyriphos-ethyl" (Just "2921-88-2"), ByCAS))]
        score mappings (mkFlow 99 "Chlorpyriphos" (Just "2921-88-2")) `shouldBe` Just 5.0

    it "ignores consumer-located rows: their variance is dispatched by location, not guessed" $ do
        -- JRC-style regionalization: the per-country values live on located
        -- rows; the region-less default remains the legitimate answer for a
        -- flow with no location.
        let located = (mkCF 2 "Water" "" water 1.44){mcfConsumerLocation = Just "CH"}
            mappings =
                [ (mkCF 1 "Water" "" water 42.955, Just (mkFlow 1 "Water" water, ByCAS))
                , (located, Just (mkFlow 1 "Water" water, ByCAS))
                ]
        score mappings (mkFlow 99 "Water, unspecified natural origin" water) `shouldBe` Just 42.955

    it "ignores variance across different subcompartments: the medium-level default still bridges" $ do
        -- Sub-specific siblings (an indoor factor ~100x the outdoor one) are
        -- already arbitrated to the unspecified default; that arbitration is
        -- not ambiguity.
        let mappings =
                [ (mkCF 1 "Particulates" "" (Just "1234-56-7") 1.0, Just (mkFlow 1 "Particulates, alias" (Just "1234-56-7"), ByCAS))
                , (mkCF 2 "Particulates" "indoor" (Just "1234-56-7") 100.0, Just (mkFlow 2 "Particulates, indoor" (Just "1234-56-7"), ByName))
                ]
        score mappings (mkFlow 99 "Dust" (Just "1234-56-7")) `shouldBe` Just 1.0

    describe "a located method against a name-suffixed database" $ do
        -- JRC-style rows (per-country values, consumer-located) meeting
        -- SimaPro-style flows (region in the name): location cannot dispatch
        -- anything there, so 'projectRegionalResourceFlows' materializes the
        -- located rows into region-less copies – and those copies vote.
        -- Without their votes the lone region-less row would be unanimous,
        -- and turbine water would take the world-average factor on exactly
        -- the pairing the veto exists for.
        let plainFlow = mkFlow 10 "Water" water
            chFlow = mkFlow 11 "Water, lake, CH" water
            inFlow = mkFlow 12 "Water, lake, IN" water
            bioFlows = M.fromList [(bfId f, f) | f <- [plainFlow, chFlow, inFlow]]
            mappings =
                [ (mkCF 1 "Water" "" water 42.955, Just (plainFlow, ByCAS))
                , ((mkCF 2 "Water, lake" "" water 1.44){mcfConsumerLocation = Just "CH"}, Just (chFlow, ByCAS))
                , ((mkCF 3 "Water, lake" "" water 100.0){mcfConsumerLocation = Just "IN"}, Just (inFlow, ByCAS))
                ]
            tables =
                buildMethodTables OtherCFFamily M.empty M.empty $
                    projectRegionalResourceFlows emptySynonymDB bioFlows mappings

        it "the projected copies veto both CAS bridges" $ do
            M.lookup (CASNumber "7732-18-5", Just NaturalResource) (mtCasCF tables) `shouldBe` Nothing
            M.lookup (CASNumber "7732-18-5", Just NaturalResource) (mtRegionalCasCF tables) `shouldBe` Nothing

        it "a deliberately excluded flow stays uncharacterized" $ do
            let turbine = mkFlow 99 "Water, turbine use" water
            fmap cfValue (lookupCFForFlow tables (bfId turbine) (Just turbine)) `shouldBe` Nothing

        it "a suffixed flow keeps its own region's projected value" $
            fmap cfValue (lookupCFForFlow tables (bfId chFlow) (Just chFlow)) `shouldBe` Just 1.44

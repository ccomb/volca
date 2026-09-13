{-# LANGUAGE OverloadedStrings #-}

module ProjectRegionalSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), projectRegionalResourceFlows)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import SynonymDB (BridgeDirection (..), SynEdge (..), buildFromEdges, buildFromPairs)
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- | A withdrawal CF for a named resource flow at a consumer location.
mkLocatedCF :: Text -> Double -> Maybe Text -> MethodCF
mkLocatedCF name val loc =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "natural resource" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "m3"
        , mcfConsumerLocation = loc
        }

mkResourceFlow :: Integer -> Text -> BiosphereFlow
mkResourceFlow i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment NaturalResource Nothing)
        }

-- A projection nulls the CF's consumer location (it becomes a GLOBAL entry) and
-- re-targets it onto the region-tagged flow's own name. Since the result is
-- @mappings ++ projected@, a projected entry shows up as a @(flow name, Nothing,
-- value)@ triple carried by the region-tagged flow.
projected :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> [(Text, Maybe Text, Double)]
projected xs = [(bfName f, mcfConsumerLocation cf, mcfValue cf) | (cf, Just (f, BySynonym)) <- xs]

spec :: Spec
spec = describe "projectRegionalResourceFlows" $ do
    let synDB = buildFromPairs [("river water", "Water, river")]
        baseFlow = mkResourceFlow 10 "Water, river"
        cfFR = mkLocatedCF "river water" 6.98 (Just "FR")
        baseMappings = [(cfFR, Just (baseFlow, BySynonym))]
        runWith flows =
            projected (projectRegionalResourceFlows synDB (M.fromList [(bfId f, f) | f <- flows]) baseMappings)
        frProjection = ("Water, river, FR", Nothing, 6.98)

    it "projects a region-tagged resource flow onto its region's located CF, globally" $
        runWith [baseFlow, mkResourceFlow 11 "Water, river, FR"]
            `shouldContain` [frProjection]

    it "does not borrow a sibling region's located CF" $
        runWith [baseFlow, mkResourceFlow 12 "Water, river, ZZ"]
            `shouldNotContain` [("Water, river, ZZ", Nothing, 6.98)]

    it "falls a sub-national region back to its parent country's located CF" $ do
        let cfCN = mkLocatedCF "river water" 42.4 (Just "CN")
            cnSC = mkResourceFlow 15 "Water, river, CN-SC"
            flows = M.fromList [(bfId f, f) | f <- [baseFlow, cnSC]]
        projected (projectRegionalResourceFlows synDB flows [(cfFR, Just (baseFlow, BySynonym)), (cfCN, Just (baseFlow, BySynonym))])
            `shouldContain` [("Water, river, CN-SC", Nothing, 42.4)]

    it "falls an untabulated region back to the method's location-less CF through the synonym bridge" $ do
        let cfGeneric = mkLocatedCF "river water" 42.95 Nothing
            rowFlow = mkResourceFlow 16 "Water, river, RoW"
            flows = M.fromList [(bfId f, f) | f <- [baseFlow, rowFlow]]
        projected (projectRegionalResourceFlows synDB flows [(cfFR, Just (baseFlow, BySynonym)), (cfGeneric, Just (baseFlow, BySynonym))])
            `shouldContain` [("Water, river, RoW", Nothing, 42.95)]

    it "prefers the exact located region over the parent and generic fallbacks" $ do
        let cfSC = mkLocatedCF "river water" 39.9 (Just "CN-SC")
            cfCN = mkLocatedCF "river water" 42.4 (Just "CN")
            cfGeneric = mkLocatedCF "river water" 42.95 Nothing
            cnSC = mkResourceFlow 17 "Water, river, CN-SC"
            flows = M.fromList [(bfId f, f) | f <- [baseFlow, cnSC]]
            ms = [(cf, Just (baseFlow, BySynonym)) | cf <- [cfSC, cfCN, cfGeneric]]
        projected (projectRegionalResourceFlows synDB flows ms)
            `shouldContain` [("Water, river, CN-SC", Nothing, 39.9)]

    it "does not route the generic fallback through an input-only bridge for a release flow" $ do
        let inSynDB = buildFromEdges [SynEdge "river water" "Water, river" BridgeInput]
            cfGeneric = mkLocatedCF "river water" 42.95 Nothing
            rowRelease =
                (mkResourceFlow 18 "Water, river, RoW")
                    { bfCompartment = Just (VT.Compartment Water Nothing)
                    }
            flows = M.fromList [(bfId f, f) | f <- [baseFlow, rowRelease]]
        projected (projectRegionalResourceFlows inSynDB flows [(cfFR, Just (baseFlow, BySynonym)), (cfGeneric, Just (baseFlow, BySynonym))])
            `shouldNotContain` [("Water, river, RoW", Nothing, 42.95)]

    it "does not let a located CF in another medium open the water fallback" $ do
        let airCF =
                (mkLocatedCF "Sulfur dioxide" 1.5 (Just "FR"))
                    { mcfCompartment = Just (Compartment "air" "" "")
                    }
            cfGeneric = mkLocatedCF "river water" 42.95 Nothing
            rowFlow = mkResourceFlow 24 "Water, river, RoW"
            flows = M.fromList [(bfId f, f) | f <- [baseFlow, rowFlow]]
        projected (projectRegionalResourceFlows synDB flows [(airCF, Nothing), (cfGeneric, Just (baseFlow, BySynonym))])
            `shouldNotContain` [("Water, river, RoW", Nothing, 42.95)]

    it "projects a resource flow that names a subcompartment as well as its medium" $ do
        let frFlowSubbed =
                (mkResourceFlow 13 "Water, river, FR")
                    { bfCompartment = Just (VT.Compartment VT.NaturalResource (Just "in water"))
                    }
        runWith [baseFlow, frFlowSubbed] `shouldContain` [frProjection]

    it "dedups colliding located CFs deterministically – higher value wins, order-independent" $ do
        let cfLo = mkLocatedCF "river water" 6.98 (Just "FR")
            cfHi = mkLocatedCF "river water" 9.99 (Just "FR")
            frFlow = mkResourceFlow 14 "Water, river, FR"
            flows = M.fromList [(bfId f, f) | f <- [baseFlow, frFlow]]
            run ms = projected (projectRegionalResourceFlows synDB flows ms)
            mLo = [(cfLo, Just (baseFlow, BySynonym)), (cfHi, Just (baseFlow, BySynonym))]
            mHi = [(cfHi, Just (baseFlow, BySynonym)), (cfLo, Just (baseFlow, BySynonym))]
        run mLo `shouldContain` [("Water, river, FR", Nothing, 9.99)]
        run mHi `shouldContain` [("Water, river, FR", Nothing, 9.99)]
        run mLo `shouldNotContain` [("Water, river, FR", Nothing, 6.98)]

    it "leaves an unlocated method untouched (the SimaPro name-regionalized convention)" $ do
        let cfGlobal = mkLocatedCF "river water" 6.98 Nothing
            frFlow = mkResourceFlow 11 "Water, river, FR"
        projected
            (projectRegionalResourceFlows synDB (M.fromList [(bfId frFlow, frFlow)]) [(cfGlobal, Just (baseFlow, BySynonym))])
            `shouldNotContain` [frProjection]

    it "projects a resource withdrawal through an INPUT-only bridge (resource medium picks the input view)" $ do
        let inSynDB = buildFromEdges [SynEdge "river water" "Water, river" BridgeInput]
            frFlow = mkResourceFlow 11 "Water, river, FR"
        projected (projectRegionalResourceFlows inSynDB (M.fromList [(bfId frFlow, frFlow)]) baseMappings)
            `shouldContain` [frProjection]

    it "does not project a resource withdrawal through an OUTPUT-only bridge (view selection is real)" $ do
        let outSynDB = buildFromEdges [SynEdge "river water" "Water, river" BridgeOutput]
            frFlow = mkResourceFlow 11 "Water, river, FR"
        projected (projectRegionalResourceFlows outSynDB (M.fromList [(bfId frFlow, frFlow)]) baseMappings)
            `shouldNotContain` [frProjection]

    it "projects a release CF onto a region-tagged water emission flow by its own name" $ do
        let releaseCF =
                (mkLocatedCF "Water" (-42.0) (Just "FR"))
                    { mcfCompartment = Just (Compartment "water" "" "")
                    }
            waterFR =
                (mkResourceFlow 20 "Water, FR")
                    { bfCompartment = Just (VT.Compartment Water Nothing)
                    }
            bareWater =
                (mkResourceFlow 21 "Water")
                    { bfCompartment = Just (VT.Compartment Water Nothing)
                    }
        projected
            ( projectRegionalResourceFlows
                synDB
                (M.fromList [(bfId waterFR, waterFR), (bfId bareWater, bareWater)])
                [(releaseCF, Just (bareWater, ByName))]
            )
            `shouldContain` [("Water, FR", Nothing, -42.0)]

    it "does not project an air emission flow, so air-regionalized methods stay global" $ do
        let airCF =
                (mkLocatedCF "Sulfur dioxide" 1.5 (Just "FR"))
                    { mcfCompartment = Just (Compartment "air" "" "")
                    }
            so2FR =
                (mkResourceFlow 22 "Sulfur dioxide, FR")
                    { bfCompartment = Just (VT.Compartment Air Nothing)
                    }
            bareSo2 =
                (mkResourceFlow 23 "Sulfur dioxide")
                    { bfCompartment = Just (VT.Compartment Air Nothing)
                    }
        projected
            ( projectRegionalResourceFlows
                synDB
                (M.fromList [(bfId so2FR, so2FR), (bfId bareSo2, bareSo2)])
                [(airCF, Just (bareSo2, ByName))]
            )
            `shouldNotContain` [("Sulfur dioxide, FR", Nothing, 1.5)]

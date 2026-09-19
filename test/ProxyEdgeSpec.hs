{-# LANGUAGE OverloadedStrings #-}

module ProxyEdgeSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (
    MappingStats (msByProxy),
    MatchStrategy (..),
    ProxyTargets (..),
    buildMethodTables,
    cfValue,
    computeMappingStats,
    expandProxyEdges,
    lookupCFForFlow,
 )
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import qualified SubstanceRegistry as SR
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- | A method CF (emission to water) with a given name, value and CAS.
mkCF :: Text -> Double -> Maybe Text -> MethodCF
mkCF name val cas =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "water" "" "")
        , mcfCAS = cas
        , mcfUnit = "kg"
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
        , bfCompartment = Just (VT.Compartment Water Nothing)
        }

nameKey :: Text -> SR.SubstanceKey
nameKey n = SR.ByName (SR.SourceId "test") (SR.NormName n)

casKey :: Text -> SR.SubstanceKey
casKey = SR.ByCAS . SR.CASNumber

proxyEdge :: SR.SubstanceKey -> SR.SubstanceKey -> Double -> SR.SubstanceEdge
proxyEdge a b f = SR.SubstanceEdge a b (SR.ProxyFor (SR.ConversionFactor f))

-- | Only the proxy-tagged rows, as (scaled value, target flow name).
proxyEntries :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> [(Double, Text)]
proxyEntries xs = [(mcfValue cf, bfName f) | (cf, Just (f, ByProxy)) <- xs]

noTargets :: ProxyTargets
noTargets = ProxyTargets M.empty M.empty M.empty

spec :: Spec
spec = describe "expandProxyEdges" $ do
    let phosphorusCF = mkCF "phosphorus" 2.0 (Just "7723-14-0")
        phosphateFlow = mkFlow 10 "phosphate" (Just "14265-44-2")
        byNameTargets = ProxyTargets (M.fromList [("phosphate", [phosphateFlow])]) M.empty M.empty
        baseMappings = [(phosphorusCF, Nothing)]

    it "borrows a CF onto the proxy flow, scaled and tagged ByProxy" $ do
        let edges = [proxyEdge (nameKey "phosphorus") (nameKey "phosphate") 0.5]
        proxyEntries (expandProxyEdges byNameTargets edges baseMappings)
            `shouldBe` [(1.0, "phosphate")] -- 2.0 * 0.5
    it "resolves the target flow by CAS" $ do
        let targets = ProxyTargets M.empty (M.fromList [("14265-44-2", [phosphateFlow])]) M.empty
            edges = [proxyEdge (nameKey "phosphorus") (casKey "14265-44-2") 0.5]
        proxyEntries (expandProxyEdges targets edges baseMappings)
            `shouldBe` [(1.0, "phosphate")]

    it "resolves the source CF by CAS" $ do
        let edges = [proxyEdge (casKey "7723-14-0") (nameKey "phosphate") 0.5]
        proxyEntries (expandProxyEdges byNameTargets edges baseMappings)
            `shouldBe` [(1.0, "phosphate")]

    it "ignores SameAs / Subsumes / DistinctFrom edges" $ do
        let edges =
                [ SR.SubstanceEdge (nameKey "phosphorus") (nameKey "phosphate") SR.SameAs
                , SR.SubstanceEdge (nameKey "phosphorus") (nameKey "phosphate") (SR.Subsumes (SR.SplitWeight 0.9))
                , SR.SubstanceEdge (nameKey "phosphorus") (nameKey "phosphate") SR.DistinctFrom
                ]
        proxyEntries (expandProxyEdges byNameTargets edges baseMappings) `shouldBe` []

    it "is the identity when no edges are loaded" $ do
        let result = expandProxyEdges noTargets [] baseMappings
        length result `shouldBe` length baseMappings
        proxyEntries result `shouldBe` []

    it "emits one proxy CF per matching database flow" $ do
        let f1 = mkFlow 10 "phosphate" Nothing
            f2 = mkFlow 11 "phosphate" Nothing
            targets = ProxyTargets (M.fromList [("phosphate", [f1, f2])]) M.empty M.empty
            edges = [proxyEdge (nameKey "phosphorus") (nameKey "phosphate") 0.5]
        proxyEntries (expandProxyEdges targets edges baseMappings)
            `shouldBe` [(1.0, "phosphate"), (1.0, "phosphate")]

    it "counts proxy matches in MappingStats" $ do
        let edges = [proxyEdge (nameKey "phosphorus") (nameKey "phosphate") 0.5]
            result = expandProxyEdges byNameTargets edges baseMappings
        msByProxy (computeMappingStats result) `shouldBe` 1

    -- End-to-end of the cascade: the proxy CF must be reachable by the scoring
    -- lookup, exactly as a real score queries it.
    it "characterizes an otherwise-uncharacterized flow through the method tables" $ do
        let edges = [proxyEdge (nameKey "phosphorus") (nameKey "phosphate") 0.5]
            tablesNoProxy = buildMethodTables mempty mempty M.empty baseMappings
            tablesProxy = buildMethodTables mempty mempty M.empty (expandProxyEdges byNameTargets edges baseMappings)
            lookup' ts = lookupCFForFlow ts (bfId phosphateFlow) (Just phosphateFlow)
        lookup' tablesNoProxy `shouldBe` Nothing
        fmap cfValue (lookup' tablesProxy) `shouldBe` Just 1.0 -- 2.0 * 0.5

{-# LANGUAGE OverloadedStrings #-}

{- | Counting a method's reach into a database honestly: 'characterizedFlowIds'
probes every database flow with the same lookup scoring uses, so a flow
covered through a fallback counts – where the build-side mappings, which
resolve each factor to at most one flow, would miss it.
-}
module CharacterizedFlowsSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), MethodTables, buildMethodTables, characterizedFlowIds)
import Method.Types (CFFamily (..), Compartment (..), FlowDirection (..), MethodCF (..))
import Types (
    BioFlowDB,
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- An emission CF with an unspecified subcompartment: the kind that lands in
-- the medium-level fallback table and covers every subcompartment of "air".
airCF :: Text -> Double -> MethodCF
airCF name val =
    MethodCF
        { mcfFlowRef = mkUUID 100
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "air" "" "")
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
        , bfCompartment = Just (VT.Compartment Air sub)
        }

-- The database side: ammonia in two subcompartments, methane in one.
ammoniaUrban, ammoniaRural, methane :: BiosphereFlow
ammoniaUrban = mkFlow 1 "Ammonia" (Just "urban air")
ammoniaRural = mkFlow 2 "Ammonia" (Just "rural")
methane = mkFlow 3 "Methane" Nothing

bioFlows :: BioFlowDB
bioFlows = M.fromList [(bfId f, f) | f <- [ammoniaUrban, ammoniaRural, methane]]

-- One ammonia factor, resolved at build time to the urban flow ONLY – the
-- build side pairs each factor with at most one flow.
ammoniaTables :: MethodTables
ammoniaTables =
    buildMethodTables OtherCFFamily M.empty M.empty [(airCF "Ammonia" 2.7, Just (ammoniaUrban, ByName))]

methaneTables :: MethodTables
methaneTables =
    buildMethodTables OtherCFFamily M.empty M.empty [(airCF "Methane" 28.0, Just (methane, ByName))]

spec :: Spec
spec = describe "characterizedFlowIds" $ do
    it "counts every flow the read-side lookup covers, not just the one the factor resolved to" $
        characterizedFlowIds ammoniaTables bioFlows
            `shouldBe` S.fromList [bfId ammoniaUrban, bfId ammoniaRural]

    it "leaves a flow no factor reaches out of the set" $
        S.member (bfId methane) (characterizedFlowIds ammoniaTables bioFlows)
            `shouldBe` False

    it "lets a collection's reach be the union of its methods' sets" $
        S.size
            ( S.unions
                (map (`characterizedFlowIds` bioFlows) [ammoniaTables, methaneTables])
            )
            `shouldBe` 3

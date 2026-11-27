{-# LANGUAGE OverloadedStrings #-}

{- | The flows of one substance in one medium make one line, whatever their
sub-compartments and however their names are cased; a flow of another medium,
or of another name, keeps its own line.
-}
module SubstancesSpec (spec) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import Impact.Substances (SubstanceContribution (..), bySubstance, substanceCompartments, substanceFlowIds)
import Method.Mapping (FlowContribution (..))
import Types (BiosphereFlow (..), Compartment (..), Medium (..))

import CrossDBRegionalLCIAFixture (testFlow)

-- | A flow of this name, medium and sub-compartment, contributing this much.
row :: Int -> Text -> Medium -> Maybe Text -> Double -> FlowContribution
row n name medium sub c =
    FlowContribution
        { fcFlow = testFlow{bfId = UUID.fromWords 0 0 0 (fromIntegral n), bfName = name, bfCompartment = Just (Compartment medium sub)}
        , fcFactor = 1
        , fcContribution = c
        }

spec :: Spec
spec = describe "Impact.Substances.bySubstance" $ do
    let rows =
            [ row 1 "Carbon dioxide, fossil" Air (Just "low. pop.") 23.8
            , row 2 "Methane, biogenic" Air (Just "low. pop.") 25.1
            , row 3 "carbon dioxide, fossil " Air Nothing 11.9
            , row 4 "Carbon dioxide, fossil" Air (Just "high. pop.") 6.9
            , row 5 "Carbon dioxide, fossil" Water Nothing 0.5
            , row 6 "Carbon dioxide, fossil" Air (Just "low. pop.") (-0.1)
            ]
        lines' = sortOn scName (bySubstance rows)

    it "adds up the flows of one name, case and spaces aside, and one medium" $
        [(scName s, scMedium s, round (scContribution s * 10) :: Integer) | s <- lines']
            `shouldBe` [("Carbon dioxide, fossil", Just Air, 425), ("Carbon dioxide, fossil", Just Water, 5), ("Methane, biogenic", Just Air, 251)]

    it "names each sub-compartment once, its flows largest first" $
        [(substanceCompartments s, substanceFlowIds s) | s <- take 1 lines']
            `shouldBe` [(["low. pop.", "high. pop."], map (UUID.toText . UUID.fromWords 0 0 0) [1, 3, 4, 6])]

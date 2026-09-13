{-# LANGUAGE OverloadedStrings #-}

module SynonymExtractSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Method.FlowResolver (ILCDFlowInfo (..))
import SynonymDB.Extract (extractFromILCDFlows)
import Test.Hspec

spec :: Spec
spec = describe "extractFromILCDFlows" $ do
    it "emits baseName↔synonym pairs from <common:synonyms>" $ do
        let flows = M.fromList [(uuidA, mkFlow "Carbon dioxide" Nothing ["CO2", "carbonic anhydride"])]
        extractFromILCDFlows flows
            `shouldMatchList` [("Carbon dioxide", "CO2"), ("Carbon dioxide", "carbonic anhydride")]

    it "does not chain same-CAS flows into synonym pairs (CAS is matched by the cascade)" $ do
        -- Two distinct substances sharing one CAS, neither carrying <synonyms>.
        -- The old CAS grouping would emit ("Substance A","Substance B"); now nothing.
        let flows =
                M.fromList
                    [ (uuidA, mkFlow "Substance A" (Just "100-00-0") [])
                    , (uuidB, mkFlow "Substance B" (Just "100-00-0") [])
                    ]
        extractFromILCDFlows flows `shouldBe` []

    it "drops a name shared by a CAS-less flow and a CAS-bearing flow (absence is its own identity)" $ do
        -- "shared ion" is carried by an element flow with no CAS and by a salt flow
        -- with a CAS. The two cannot be proven the same substance, so the bridge is
        -- dropped rather than fused - even though only one side carries a CAS.
        let flows =
                M.fromList
                    [ (uuidA, mkFlow "Sodium" Nothing ["shared ion"])
                    , (uuidB, mkFlow "Sodium chloride" (Just "7647-14-5") ["shared ion"])
                    ]
        extractFromILCDFlows flows `shouldBe` []

    it "drops pairs whose baseName is CAS-ambiguous, not only ambiguous synonyms" $ do
        -- One baseName "Cresol" is carried by two isomers (distinct CAS), each with a
        -- unique synonym. The shared baseName is the bridge, so both pairs drop.
        let flows =
                M.fromList
                    [ (uuidA, mkFlow "Cresol" (Just "95-48-7") ["o-cresol"])
                    , (uuidB, mkFlow "Cresol" (Just "108-39-4") ["m-cresol"])
                    ]
        extractFromILCDFlows flows `shouldBe` []

    it "keys ambiguity by the closure's normalization, so punctuation variants share an identity" $ do
        -- "foo, bar" and "foo bar" are one node to the closure (normalizeName drops
        -- the comma); carried by two distinct-CAS flows they form a bridge, so a
        -- weaker lower+strip key must not let them slip through as separate names.
        let flows =
                M.fromList
                    [ (uuidA, mkFlow "Substance R" (Just "1-1-1") ["foo, bar"])
                    , (uuidB, mkFlow "Substance S" (Just "2-2-2") ["foo bar"])
                    ]
        extractFromILCDFlows flows `shouldBe` []

mkFlow :: Text -> Maybe Text -> [Text] -> ILCDFlowInfo
mkFlow name cas syns =
    ILCDFlowInfo
        { ilcdBaseName = name
        , ilcdCompartment = Nothing
        , ilcdCAS = cas
        , ilcdSynonyms = syns
        , ilcdFlowType = "Elementary flow"
        , ilcdFlowPropertyRef = Nothing
        }

uuidA, uuidB :: UUID
uuidA = mkUUID "11111111-1111-1111-1111-111111111111"
uuidB = mkUUID "22222222-2222-2222-2222-222222222222"

mkUUID :: String -> UUID
mkUUID s = case UUID.fromString s of
    Just u -> u
    Nothing -> error ("bad test UUID: " <> s)

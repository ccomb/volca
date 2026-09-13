{-# LANGUAGE OverloadedStrings #-}

{- | Pure tests for 'Method.Coverage': from a collection's per-method effective
mappings, does it surface exactly the flows scored only through a name bridge,
grouped under the name the method itself uses?

The fixtures build the mapping tuples directly – @collectionBridges@ is a pure
fold over them, so no database or method tables are needed.
-}
module CoverageSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID, fromWords, nil)
import Data.Word (Word32)
import Test.Hspec

import Data.Aeson (decode, encode)

import API.DatabaseHandlers (coverageReportToAPI)
import API.Types (CollectionBridgesAPI (..), CoverageReportAPI (..))
import Method.Coverage
import Method.Mapping (MatchStrategy (..))
import Method.Types (FlowDirection (..), MethodCF (..))
import Types (BiosphereFlow (..))

uuid :: Word32 -> UUID
uuid = fromWords 0 0 0

flowNamed :: Word32 -> Text -> Maybe Text -> BiosphereFlow
flowNamed n name mCas =
    BiosphereFlow
        { bfId = uuid n
        , bfName = name
        , bfUnitId = nil
        , bfSynonyms = M.empty
        , bfCAS = mCas
        , bfSubstanceId = Nothing
        , bfCompartment = Nothing
        }

cfNamed :: Text -> Maybe Text -> MethodCF
cfNamed name mCas =
    MethodCF
        { mcfFlowRef = nil
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = 1.0
        , mcfCompartment = Nothing
        , mcfCAS = mCas
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

spec :: Spec
spec = describe "Method.Coverage.collectionBridges" $ do
    it "flags a flow scored only through a synonym bridge, naming its rename target" $ do
        -- The method lists a factor under "Bromomethane"; the database emits the
        -- same substance under two names. The exact-name one is fine; the other
        -- is scored only because a synonym links it to the factor.
        let cf = cfNamed "Bromomethane" (Just "74-83-9")
            fExact = flowNamed 1 "Bromomethane" (Just "74-83-9")
            fBridge = flowNamed 2 "Methane, bromo-, Halon 1001" (Just "74-83-9")
            cb = collectionBridges "EF3.1" 2 2 [[(cf, Just (fExact, ByName)), (cf, Just (fBridge, BySynonym))]]
        cbGroups cb
            `shouldBe` [ BridgeGroup
                            (Just "74-83-9")
                            "Bromomethane"
                            [BridgedFlow "Methane, bromo-, Halon 1001" BySynonym]
                       ]

    it "does not flag a flow the method reaches by its own exact name" $ do
        let cf = cfNamed "Bromomethane" Nothing
            f = flowNamed 1 "Bromomethane" Nothing
        cbGroups (collectionBridges "m" 1 1 [[(cf, Just (f, ByName))]]) `shouldBe` []

    it "excludes a flow bridged by one factor but exact-named by another" $ do
        -- The same flow name matches factor B by name, so an exact-name tool
        -- would score it too – not a silent zero. Must not be reported.
        let cfA = cfNamed "Bromomethane" Nothing
            cfB = cfNamed "Methane, bromo-, Halon 1001" Nothing
            f = flowNamed 1 "Methane, bromo-, Halon 1001" Nothing
        cbGroups (collectionBridges "m" 1 1 [[(cfA, Just (f, BySynonym)), (cfB, Just (f, ByName))]])
            `shouldBe` []

    it "groups bridged names under their shared rename target, sorted and deduped across methods" $ do
        let cf = cfNamed "Bromomethane" (Just "74-83-9")
            f1 = flowNamed 1 "Methane, bromo-, Halon 1001" (Just "74-83-9")
            f2 = flowNamed 2 "Bromomethane, halon" Nothing
            m1 = [(cf, Just (f1, BySynonym))]
            m2 = [(cf, Just (f1, BySynonym)), (cf, Just (f2, ByCAS))]
        cbGroups (collectionBridges "m" 5 2 [m1, m2])
            `shouldBe` [ BridgeGroup
                            (Just "74-83-9")
                            "Bromomethane"
                            [ BridgedFlow "Bromomethane, halon" ByCAS
                            , BridgedFlow "Methane, bromo-, Halon 1001" BySynonym
                            ]
                       ]

    it "reports no groups when nothing bridges" $
        cbGroups (collectionBridges "m" 0 0 []) `shouldBe` []

    it "carries the caller's collection name and coverage counts through" $ do
        let cb = collectionBridges "EF3.1 (adapted) v1.03" 1300 1240 []
        cbCollection cb `shouldBe` "EF3.1 (adapted) v1.03"
        cbTotalFlows cb `shouldBe` 1300
        cbCharacterizedFlows cb `shouldBe` 1240

    describe "wire projection" $ do
        -- Two substances, each with one bridged name → two groups.
        let twoGroups =
                collectionBridges
                    "EF3.1"
                    2
                    2
                    [
                        [ (cfNamed "Bromomethane" Nothing, Just (flowNamed 1 "Methane, bromo-, Halon 1001" Nothing, BySynonym))
                        , (cfNamed "Bromotrifluoromethane" Nothing, Just (flowNamed 2 "Methane, bromotrifluoro-, Halon 1301" Nothing, BySynonym))
                        ]
                    ]
            report = CoverageReport "agb" [twoGroups]

        it "caps bridge groups per collection but keeps the full count" $
            case cvrCollections (coverageReportToAPI (Just 1) report) of
                [coll] -> do
                    cvcBridgeGroupCount coll `shouldBe` 2
                    length (cvcBridgeGroups coll) `shouldBe` 1
                _ -> expectationFailure "expected exactly one collection"

        it "returns every bridge group when no limit is given" $
            case cvrCollections (coverageReportToAPI Nothing report) of
                [coll] -> length (cvcBridgeGroups coll) `shouldBe` 2
                _ -> expectationFailure "expected exactly one collection"

        it "round-trips the wire report through JSON" $ do
            let api = coverageReportToAPI Nothing report
            decode (encode api) `shouldBe` Just api

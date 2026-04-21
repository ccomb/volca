{-# LANGUAGE OverloadedStrings #-}

module FlowResolverSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.UUID as UUID
import EcoSpold.Parser2 (normalizeCAS)
import Method.FlowResolver (ILCDFlowInfo (..), parseFlowXML)
import Method.Types (Compartment (..))
import Test.Hspec

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- normalizeCAS (pure, from EcoSpold.Parser2)
    -- -----------------------------------------------------------------------
    describe "normalizeCAS" $ do
        it "strips leading zeros from first segment" $
            normalizeCAS "001309-36-0" `shouldBe` "1309-36-0"

        it "preserves canonical CAS (no leading zeros)" $
            normalizeCAS "7732-18-5" `shouldBe` "7732-18-5"

        it "handles single zero in first segment" $
            normalizeCAS "0074-98-6" `shouldBe` "74-98-6"

        it "preserves the zero when segment is all zeros" $
            normalizeCAS "000-00-0" `shouldBe` "0-00-0"

        it "passes through non-3-segment format stripped" $
            normalizeCAS "  not-valid  " `shouldBe` "not-valid"

    -- -----------------------------------------------------------------------
    -- parseFlowXML — well-formed elementary flow
    -- -----------------------------------------------------------------------
    describe "parseFlowXML" $ do
        it "extracts UUID and baseName from minimal flow XML" $ do
            let result = parseFlowXML minimalFlowXML
            case result of
                Nothing -> expectationFailure "Expected Just result"
                Just (uuid, info) -> do
                    uuid `shouldBe` testUUID
                    ilcdBaseName info `shouldBe` "Carbon dioxide, fossil"

        it "extracts CAS number (with leading-zero normalization)" $ do
            case parseFlowXML flowWithCAS of
                Nothing -> expectationFailure "Expected Just result"
                Just (_, info) -> ilcdCAS info `shouldBe` Just "124-38-9"

        it "extracts compartment from category levels" $ do
            case parseFlowXML flowWithCompartment of
                Nothing -> expectationFailure "Expected Just result"
                Just (_, info) ->
                    case ilcdCompartment info of
                        Nothing -> expectationFailure "Expected compartment"
                        Just (Compartment medium _ _) -> medium `shouldBe` "air"

        it "extracts synonyms from en synonyms element" $ do
            case parseFlowXML flowWithSynonyms of
                Nothing -> expectationFailure "Expected Just result"
                Just (_, info) ->
                    ilcdSynonyms info `shouldContain` ["CO2"]

        it "returns Nothing for XML with no UUID" $
            case parseFlowXML xmlNoUUID of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing for missing UUID"

        it "returns Nothing for XML with no baseName" $
            case parseFlowXML xmlNoBaseName of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing for missing baseName"

        it "records flowType when present" $ do
            case parseFlowXML flowWithType of
                Nothing -> expectationFailure "Expected Just result"
                Just (_, info) -> ilcdFlowType info `shouldBe` "Elementary flow"

-- ---------------------------------------------------------------------------
-- Test UUID
-- ---------------------------------------------------------------------------

testUUID :: UUID.UUID
testUUID = case UUID.fromString "12345678-1234-1234-1234-123456789abc" of
    Just u -> u
    Nothing -> error "test UUID invalid"

-- ---------------------------------------------------------------------------
-- Inline ILCD flow XML fixtures
-- ---------------------------------------------------------------------------

minimalFlowXML :: ByteString
minimalFlowXML =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName xml:lang=\"en\">Carbon dioxide, fossil</baseName></name>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

flowWithCAS :: ByteString
flowWithCAS =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName xml:lang=\"en\">Carbon dioxide, fossil</baseName></name>\
    \<CASNumber>0124-38-9</CASNumber>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

flowWithCompartment :: ByteString
flowWithCompartment =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName xml:lang=\"en\">Carbon dioxide, fossil</baseName></name>\
    \<classificationInformation>\
    \<elementaryFlowCategorization>\
    \<category level=\"0\">Emissions</category>\
    \<category level=\"1\">Emissions to air</category>\
    \<category level=\"2\">Emissions to air, unspecified</category>\
    \</elementaryFlowCategorization>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

flowWithSynonyms :: ByteString
flowWithSynonyms =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name>\
    \<baseName xml:lang=\"en\">Carbon dioxide, fossil</baseName>\
    \<common:synonyms xml:lang=\"en\">CO2; carbon dioxide</common:synonyms>\
    \</name>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

flowWithType :: ByteString
flowWithType =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName xml:lang=\"en\">Carbon dioxide, fossil</baseName></name>\
    \</dataSetInformation>\
    \<dataSetInformation>\
    \<typeOfDataSet>Elementary flow</typeOfDataSet>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

xmlNoUUID :: ByteString
xmlNoUUID =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<name><baseName xml:lang=\"en\">Carbon dioxide, fossil</baseName></name>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

xmlNoBaseName :: ByteString
xmlNoBaseName =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \</dataSetInformation>\
    \</flowInformation>\
    \</flowDataSet>"

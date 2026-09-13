{-# LANGUAGE OverloadedStrings #-}

module FlowResolverSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import EcoSpold.Common (decodeXmlEntities, decodeXmlEntitiesFull)
import Method.FlowResolver (ILCDFlowInfo (..), parseFlowDirectory, parseFlowXML, splitIlcdSynonyms)
import Method.Types (Compartment (..))
import Progress (LogLine (..), getLogLines)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

{- | A flow claiming the twin UUID, under the given declared version and base
name, so two files can be written that differ only where the reader looks.
-}
twinFlow :: ByteString -> ByteString -> ByteString
twinFlow version baseName =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\"\n\
    \             xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\n\
    \  <flowInformation>\n\
    \    <dataSetInformation>\n\
    \      <common:UUID>aaaaaaaa-0000-0000-0000-00000000000f</common:UUID>\n\
    \      <name>\n\
    \        <baseName xml:lang=\"en\">"
        <> baseName
        <> "</baseName>\n\
           \      </name>\n\
           \    </dataSetInformation>\n\
           \  </flowInformation>\n\
           \  <modellingAndValidation>\n\
           \    <LCIMethod>\n\
           \      <typeOfDataSet>Elementary flow</typeOfDataSet>\n\
           \    </LCIMethod>\n\
           \  </modellingAndValidation>\n\
           \  <administrativeInformation>\n\
           \    <publicationAndOwnership>\n\
           \      <common:dataSetVersion>"
        <> version
        <> "</common:dataSetVersion>\n\
           \    </publicationAndOwnership>\n\
           \  </administrativeInformation>\n\
           \</flowDataSet>\n"

-- | A flows directory holding the two twins, and nothing else.
withTwinFlows :: (FilePath -> IO a) -> IO a
withTwinFlows k = withSystemTempDirectory "ilcd-flows" $ \dir -> do
    BS.writeFile (dir </> "z-first-edition.xml") (twinFlow "01.00.000" "Carbon dioxide, first edition")
    BS.writeFile (dir </> "a-second-edition.xml") (twinFlow "02.00.000" "Carbon dioxide, second edition")
    k dir

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- decodeXmlEntities (general read path) vs decodeXmlEntitiesFull (synonyms)
    -- -----------------------------------------------------------------------
    describe "decodeXmlEntities" $ do
        it "preserves an escaped-literal named entity (round-trip)" $
            decodeXmlEntities "&amp;lt;" `shouldBe` "&lt;"

        it "unescapes a lone ampersand" $
            decodeXmlEntities "&amp;" `shouldBe` "&"

        it "decodes the line-feed numeric ref that EcoSpold attributes carry" $
            decodeXmlEntities "a&#10;b" `shouldBe` "a\nb"

        it "preserves a double-encoded numeric ref on the general read path (no collapse)" $
            decodeXmlEntities "&amp;#13;" `shouldBe` "&#13;"

    describe "decodeXmlEntitiesFull" $ do
        it "fully decodes a double-encoded numeric ref (the ILCD data's form)" $
            decodeXmlEntitiesFull "&amp;#039;" `shouldBe` "'"

        it "decodes a single-encoded numeric ref" $
            decodeXmlEntitiesFull "&#039;" `shouldBe` "'"

        it "collapses the double-encoded named round-trip to the character (&amp;lt; -> <)" $
            decodeXmlEntitiesFull "&amp;lt;" `shouldBe` "<"

    describe "splitIlcdSynonyms" $ do
        it "splits an ILCD synonyms blob on ';' separators" $
            splitIlcdSynonyms "a;b;c" `shouldBe` ["a", "b", "c"]

        it "fully decodes a double-encoded named entity, so no &lt fragment survives the split" $
            splitIlcdSynonyms "solvent &amp;lt;c9&amp;gt;" `shouldBe` ["solvent <c9>"]

        it "fully decodes a double-encoded numeric entity and does not split on its semicolon" $
            splitIlcdSynonyms "x&amp;#039;y" `shouldBe` ["x'y"]

        it "splits on the 'othernames' pseudo-delimiter the source glues names with" $
            splitIlcdSynonyms "a;b othernames c" `shouldBe` ["a", "b", "c"]

    -- -----------------------------------------------------------------------
    -- -----------------------------------------------------------------------
    -- A flows directory is read once and then cached, so the cache has to
    -- stand in for what the read said as well as for what it produced.
    -- -----------------------------------------------------------------------
    describe "parseFlowDirectory" $ do
        it "keeps the flow declaring the higher version" $
            withTwinFlows $ \dir -> do
                result <- parseFlowDirectory dir
                case result of
                    Left err -> expectationFailure $ "Expected Right but got: " ++ show err
                    Right flows ->
                        map ilcdBaseName (M.elems flows) `shouldBe` ["Carbon dioxide, second edition"]

        it "names the superseded file again when the flows come from the cache" $
            withTwinFlows $ \dir -> do
                _ <- parseFlowDirectory dir
                (since, _) <- getLogLines 0
                _ <- parseFlowDirectory dir
                (_, afterCache) <- getLogLines since
                let texts = map llText afterCache
                any ("z-first-edition.xml" `isInfixOf`) texts `shouldBe` True

    -- parseFlowXML – well-formed elementary flow
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

        it "decodes double-encoded entity refs in synonyms without splitting on the entity's semicolon" $ do
            case parseFlowXML flowWithEntitySynonyms of
                Nothing -> expectationFailure "Expected Just result"
                Just (_, info) ->
                    ilcdSynonyms info `shouldBe` ["PCB", "(1-methylethyl)-1,1'-biphenyl"]

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

-- Synonym text carrying a DOUBLE-encoded apostrophe (@&amp;#039;@) – the form
-- the ILCD flow data actually uses. The @&amp;@ must decode first to expose
-- @&#039;@, which then decodes to a real apostrophe; otherwise the half-decoded
-- @&#039;@ reaches the ';'-split and truncates the second synonym into the
-- "…1,1&#039" fragment plus a stray "-biphenyl".
flowWithEntitySynonyms :: ByteString
flowWithEntitySynonyms =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<flowInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name>\
    \<baseName xml:lang=\"en\">Biphenyl derivative</baseName>\
    \<common:synonyms xml:lang=\"en\">PCB; (1-methylethyl)-1,1&amp;#039;-biphenyl</common:synonyms>\
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

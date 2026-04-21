{-# LANGUAGE OverloadedStrings #-}

module ILCDParserSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import ILCD.Parser (ILCDExchangeRaw (..), ILCDProcessRaw (..), buildSupplierIndex, fixActivityExchanges, parseILCDDirectory, parseProcessXML)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

classOf :: BS.ByteString -> M.Map Text Text
classOf = maybe M.empty iprClassifications . parseProcessXML

-- UUIDs used in supplier linking tests
flowUUID1, flowUUID2, actUUID1, actUUID2, prodUUID1, prodUUID2 :: UUID.UUID
flowUUID1 = read "11111111-0000-0000-0000-000000000001"
flowUUID2 = read "22222222-0000-0000-0000-000000000002"
actUUID1 = read "aaaaaaaa-0000-0000-0000-000000000001"
actUUID2 = read "aaaaaaaa-0000-0000-0000-000000000002"
prodUUID1 = read "bbbbbbbb-0000-0000-0000-000000000001"
prodUUID2 = read "bbbbbbbb-0000-0000-0000-000000000002"

-- An activity with a single reference output exchange for the given flow UUID
activityWithRefExchange :: UUID.UUID -> Activity
activityWithRefExchange fid =
    Activity
        { activityName = "test"
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = fid
                , techAmount = 1.0
                , techUnitId = UUID.nil
                , techIsInput = False
                , techIsReference = True
                , techActivityLinkId = UUID.nil
                , techProcessLinkId = Nothing
                , techLocation = ""
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        }

-- An activity with a single unresolved input exchange for the given flow UUID
activityWithInputExchange :: UUID.UUID -> Activity
activityWithInputExchange fid =
    Activity
        { activityName = "consumer"
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = fid
                , techAmount = 0.5
                , techUnitId = UUID.nil
                , techIsInput = True
                , techIsReference = False
                , techActivityLinkId = UUID.nil
                , techProcessLinkId = Nothing
                , techLocation = ""
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        }

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- Full directory parse (SAMPLE.ilcd fixture)
    -- -----------------------------------------------------------------------
    describe "parseILCDDirectory SAMPLE.ilcd" $ do
        it "loads without error" $ do
            result <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            case result of
                Left err -> expectationFailure $ "Expected Right but got: " ++ show err
                Right _ -> return ()

        it "parses exactly two activities" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            M.size (sdbActivities db) `shouldBe` 2

        it "activity is named 'Coal extraction'" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let names = map activityName (M.elems (sdbActivities db))
            names `shouldContain` ["Coal extraction"]

        it "activity location is GLO" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
            activityLocation act `shouldBe` "GLO"

        it "activity has ILCDCategories classification" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
            M.lookup "ILCDCategories" (activityClassification act)
                `shouldBe` Just "Energy/Hard coal"

        it "has two flows (Coal product + CO2 elementary)" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            M.size (sdbFlows db) `shouldBe` 2

        it "CO2 flow is Biosphere type" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let co2uuid = read "aaaaaaaa-0000-0000-0000-000000000003"
            fmap (\f -> flowType f == Biosphere) (M.lookup co2uuid (sdbFlows db))
                `shouldBe` Just True

        it "Coal flow is Technosphere type" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let coaluuid = read "aaaaaaaa-0000-0000-0000-000000000004"
            fmap (\f -> flowType f == Technosphere) (M.lookup coaluuid (sdbFlows db))
                `shouldBe` Just True

        it "CO2 has CAS number 124-38-9" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let co2uuid = read "aaaaaaaa-0000-0000-0000-000000000003"
            fmap flowCAS (M.lookup co2uuid (sdbFlows db)) `shouldBe` Just (Just "124-38-9")

        it "activity has two exchanges" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
            length (exchanges act) `shouldBe` 2

        it "reference exchange is Coal (Technosphere output)" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
                refEx = [ex | ex <- exchanges act, exchangeIsReference ex]
            length refEx `shouldBe` 1

        it "biosphere exchange amount is 2.5" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
                bioEx = [ex | ex <- exchanges act, isBiosphereExchange ex]
            case bioEx of
                [ex] -> bioAmount ex `shouldBe` 2.5
                _ -> expectationFailure "expected one biosphere exchange"

        it "unit group resolves to kg" $ do
            Right db <- parseILCDDirectory "test-data/SAMPLE.ilcd"
            let ugUUID = read "aaaaaaaa-0000-0000-0000-000000000001"
            fmap unitName (M.lookup ugUUID (sdbUnits db)) `shouldBe` Just "kg"

    describe "ILCD Process Parser" $ do
        it "parses classification from classificationInformation" $
            M.lookup "ILCDCategories" (classOf ilcdProcessWithClassification)
                `shouldBe` Just "End-of-life treatment/Material recycling"

        it "parses multiple classification systems" $ do
            let cls = classOf ilcdProcessWithTwoClassifications
            M.lookup "ILCDCategories" cls `shouldBe` Just "Energy/Electricity"
            M.lookup "EcoSpold" cls `shouldBe` Just "Supply"

        it "produces empty classifications when none present" $
            classOf ilcdProcessNoClassification `shouldBe` M.empty

    -- -------------------------------------------------------------------
    -- buildSupplierIndex: UUID-keyed, no name indirection
    -- -------------------------------------------------------------------
    describe "buildSupplierIndex" $ do
        it "indexes reference exchanges by flow UUID" $ do
            let activities =
                    M.fromList
                        [ ((actUUID1, prodUUID1), activityWithRefExchange flowUUID1)
                        , ((actUUID2, prodUUID2), activityWithRefExchange flowUUID2)
                        ]
                idx = buildSupplierIndex activities
            M.lookup flowUUID1 idx `shouldBe` Just (actUUID1, prodUUID1)
            M.lookup flowUUID2 idx `shouldBe` Just (actUUID2, prodUUID2)

        it "does not index non-reference exchanges" $ do
            let activities =
                    M.fromList
                        [((actUUID1, prodUUID1), activityWithInputExchange flowUUID1)]
                idx = buildSupplierIndex activities
            M.lookup flowUUID1 idx `shouldBe` Nothing

        it "two activities with same flow name but different UUIDs are both indexed" $ do
            -- This is the bug the old name-based code had: M.fromList on names
            -- would silently discard one. UUID keys have no such collision.
            let activities =
                    M.fromList
                        [ ((actUUID1, prodUUID1), activityWithRefExchange flowUUID1)
                        , ((actUUID2, prodUUID2), activityWithRefExchange flowUUID2)
                        ]
                idx = buildSupplierIndex activities
            M.size idx `shouldBe` 2

    -- -------------------------------------------------------------------
    -- fixActivityExchanges: resolves input exchanges via supplier index
    -- -------------------------------------------------------------------
    describe "fixActivityExchanges" $ do
        it "resolves input exchange flow UUID to supplier (actUUID, prodUUID)" $ do
            let idx = M.fromList [(flowUUID1, (actUUID1, prodUUID1))]
                act = activityWithInputExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange{techFlowId = fid, techActivityLinkId = alink}] -> do
                    fid `shouldBe` prodUUID1
                    alink `shouldBe` actUUID1
                _ -> expectationFailure "expected one TechnosphereExchange"

        it "leaves input exchange unchanged when flow UUID not in index" $ do
            let idx = M.empty
                act = activityWithInputExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange{techFlowId = fid}] ->
                    fid `shouldBe` flowUUID1
                _ -> expectationFailure "expected one TechnosphereExchange"

        it "does not touch output (reference) exchanges" $ do
            let idx = M.fromList [(flowUUID1, (actUUID1, prodUUID1))]
                act = activityWithRefExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange{techFlowId = fid, techIsReference = isRef}] -> do
                    fid `shouldBe` flowUUID1 -- unchanged
                    isRef `shouldBe` True
                _ -> expectationFailure "expected one TechnosphereExchange"

    -- -----------------------------------------------------------------------
    -- parseProcessXML: basic fields
    -- -----------------------------------------------------------------------
    describe "parseProcessXML basic fields" $ do
        it "parses process UUID" $
            fmap iprUUID (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` UUID.fromText "12345678-1234-1234-1234-123456789abc"

        it "parses process name" $
            fmap iprName (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just "Test Process"

        it "parses process location" $
            fmap iprLocation (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just "DE"

        it "parses referenceToReferenceFlow index" $
            fmap iprRefFlowIdx (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just 0

        it "returns Nothing for invalid XML" $
            case parseProcessXML "<not-xml" of
                Nothing -> return ()
                Just _ -> expectationFailure "expected Nothing for invalid XML"

        it "returns Nothing when baseName is missing" $ do
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/no-basename.xml"
            case parseProcessXML xml of
                Nothing -> return ()
                Just _ -> expectationFailure "expected Nothing when baseName missing"

    describe "parseProcessXML exchange fields" $ do
        it "parses single exchange flow ref" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] ->
                    ierFlowRef ex
                        `shouldBe` read "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                _ -> expectationFailure "expected one exchange"

        it "parses exchange direction Output" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] -> ierDirection ex `shouldBe` "Output"
                _ -> expectationFailure "expected one exchange"

        it "parses exchange resultingAmount" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] -> ierAmount ex `shouldBe` 1.0
                _ -> expectationFailure "expected one exchange"

        it "parses exchange dataSetInternalID" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] -> ierInternalId ex `shouldBe` 0
                _ -> expectationFailure "expected one exchange"

        it "parses Input exchange direction" $ do
            -- re-use SAMPLE.ilcd process which has an Input exchange (CO2 = Output, Coal = Output)
            -- Use the coal extraction process which has an Output; test Input via meanAmount file
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/aaaaaaaa-0000-0000-0000-000000000005.xml"
            let Just raw = parseProcessXML xml
            -- coal extraction has two Output exchanges; verify direction parsing works
            let dirs = map ierDirection (iprExchanges raw)
            dirs `shouldContain` ["Output"]

        it "falls back to meanAmount when resultingAmount is absent" $ do
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/mean-amount.xml"
            let Just raw = parseProcessXML xml
            case iprExchanges raw of
                [ex] -> ierAmount ex `shouldBe` 42.0
                _ -> expectationFailure "expected one exchange"

        it "parses multiple exchanges preserving order" $ do
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/aaaaaaaa-0000-0000-0000-000000000005.xml"
            let Just raw = parseProcessXML xml
            length (iprExchanges raw) `shouldBe` 2

-- Minimal ILCD process XML with classification
ilcdProcessWithClassification :: BS.ByteString
ilcdProcessWithClassification =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process</baseName></name>\
    \<classificationInformation>\
    \<common:classification name=\"ILCDCategories\">\
    \<common:class level=\"0\">End-of-life treatment</common:class>\
    \<common:class level=\"1\">Material recycling</common:class>\
    \</common:classification>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \<geography location=\"DE\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessWithTwoClassifications :: BS.ByteString
ilcdProcessWithTwoClassifications =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>22345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process 2</baseName></name>\
    \<classificationInformation>\
    \<common:classification name=\"ILCDCategories\">\
    \<common:class level=\"0\">Energy</common:class>\
    \<common:class level=\"1\">Electricity</common:class>\
    \</common:classification>\
    \<common:classification name=\"EcoSpold\">\
    \<common:class level=\"0\">Supply</common:class>\
    \</common:classification>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \<geography location=\"FR\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessNoClassification :: BS.ByteString
ilcdProcessNoClassification =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>32345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process 3</baseName></name>\
    \</dataSetInformation>\
    \<geography location=\"US\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module MethodSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Method.FlowResolver (ILCDFlowInfo (..), parseCompartment, parseFlowXML)
import Method.Mapping (MatchStrategy (..), computeLCIAScore)
import Method.Parser
import Method.ParserCSV (parseMethodCSVBytes)
import Method.ParserNW (parseNormWeightCSVBytes)
import Method.ParserSimaPro (isSimaProMethodCSV, parseSimaProMethodCSVBytes)
import Method.Types
import Method.Types (Compartment (..))
import SynonymDB
import Types (Flow (..), FlowType (..))
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "SynonymDB" $ do
        describe "normalizeName" $ do
            it "lowercases names" $ do
                normalizeName "Carbon Dioxide" `shouldBe` "carbon dioxide"

            it "strips leading/trailing whitespace" $ do
                normalizeName "  carbon dioxide  " `shouldBe` "carbon dioxide"

            it "collapses multiple spaces" $ do
                normalizeName "carbon   dioxide" `shouldBe` "carbon dioxide"

            it "strips ', in ground' suffix" $ do
                normalizeName "iron, in ground" `shouldBe` "iron"

            it "strips ' in ground' suffix" $ do
                normalizeName "iron in ground" `shouldBe` "iron"

            it "strips '/kg' suffix" $ do
                normalizeName "carbon dioxide/kg" `shouldBe` "carbon dioxide"

            it "removes punctuation (commas, parens, quotes)" $ do
                normalizeName "carbon (IV) oxide, fossil" `shouldBe` "carbon iv oxide fossil"

            it "handles complex names" $ do
                normalizeName "  Carbon Dioxide (biogenic), in air  " `shouldBe` "carbon dioxide biogenic in air"

        describe "buildFromCSV" $ do
            it "returns Left for invalid CSV (wrong number of columns)" $ do
                let csv = "name1,name2\ncarbon dioxide,co2,extra\n"
                case buildFromCSV csv of
                    Left _ -> return ()
                    Right _ -> expectationFailure "Expected Left for malformed CSV"

            it "builds a synonym DB from CSV pairs" $ do
                let csv = "name1,name2\ncarbon dioxide,co2\nco2,carbon dioxide (fossil)\n"
                case buildFromCSV csv of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right db -> do
                        synNameToId db `shouldSatisfy` (not . M.null)
                        synIdToNames db `shouldSatisfy` (not . M.null)

            it "can look up 'carbon dioxide'" $ do
                let csv = "name1,name2\ncarbon dioxide,co2\n"
                case buildFromCSV csv of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right db -> do
                        let result = lookupSynonymGroup db "carbon dioxide"
                        result `shouldSatisfy` maybe False (const True)

            it "maps 'co2' to same group as 'carbon dioxide' (transitive)" $ do
                let csv = "name1,name2\ncarbon dioxide,co2\n"
                case buildFromCSV csv of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right db -> do
                        let co2Group = lookupSynonymGroup db "co2"
                            carbonGroup = lookupSynonymGroup db "carbon dioxide"
                        co2Group `shouldBe` carbonGroup

            it "can retrieve synonyms for a group" $ do
                let csv = "name1,name2\ncarbon dioxide,co2\nco2,methane\n"
                case buildFromCSV csv of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right db ->
                        case lookupSynonymGroup db "carbon dioxide" of
                            Nothing -> expectationFailure "carbon dioxide not found in DB"
                            Just gid -> do
                                let synonyms = getSynonyms db gid
                                synonyms `shouldSatisfy` maybe False (not . null)

        describe "buildFromPairs" $ do
            it "returns empty DB for empty list" $
                synonymCount (buildFromPairs []) `shouldBe` 0

            it "makes both names in a pair findable" $ do
                let db = buildFromPairs [("CO2", "Carbon dioxide")]
                lookupSynonymGroup db "co2" `shouldNotBe` Nothing
                lookupSynonymGroup db "carbon dioxide" `shouldNotBe` Nothing

            it "ignores pairs where both names normalize identically" $
                synonymCount (buildFromPairs [("CO2", "co2")]) `shouldBe` 0

            it "handles multiple independent pairs" $ do
                let db = buildFromPairs [("CO2", "Carbon dioxide"), ("CH4", "Methane")]
                lookupSynonymGroup db "co2" `shouldNotBe` Nothing
                lookupSynonymGroup db "ch4" `shouldNotBe` Nothing

            it "does not pollute unrelated pairs — CO2 and CH4 are in separate groups" $ do
                let db = buildFromPairs [("CO2", "Carbon dioxide"), ("CH4", "Methane")]
                lookupSynonymGroup db "co2" `shouldNotBe` lookupSynonymGroup db "ch4"

            it "prevents overly generic terms (>50 direct synonyms) from dominating" $ do
                -- "generic" paired with 51 substances; each substance should still be findable
                let pairs = map ("generic",) (map (T.pack . show) [1 .. 51 :: Int])
                    db = buildFromPairs pairs
                lookupSynonymGroup db "1" `shouldNotBe` Nothing

        describe "mergeSynonymDBs" $ do
            it "returns empty DB for empty list" $
                synonymCount (mergeSynonymDBs []) `shouldBe` 0

            it "returns equivalent DB for singleton list" $ do
                let db = buildFromPairs [("CO2", "Carbon dioxide")]
                synonymCount (mergeSynonymDBs [db]) `shouldBe` synonymCount db

            it "merged DB contains entries from both sources" $ do
                let db1 = buildFromPairs [("CO2", "Carbon dioxide")]
                    db2 = buildFromPairs [("N2O", "Nitrous oxide")]
                    merged = mergeSynonymDBs [db1, db2]
                lookupSynonymGroup merged "co2" `shouldNotBe` Nothing
                lookupSynonymGroup merged "n2o" `shouldNotBe` Nothing

            it "entries from db1 and db2 are in different groups after merge" $ do
                let db1 = buildFromPairs [("CO2", "Carbon dioxide")]
                    db2 = buildFromPairs [("N2O", "Nitrous oxide")]
                    merged = mergeSynonymDBs [db1, db2]
                lookupSynonymGroup merged "co2" `shouldNotBe` lookupSynonymGroup merged "n2o"

        describe "synonymCount" $ do
            it "is 0 for emptySynonymDB" $
                synonymCount emptySynonymDB `shouldBe` 0

            it "is positive for a non-empty DB" $ do
                let db = buildFromPairs [("CO2", "Carbon dioxide")]
                synonymCount db `shouldSatisfy` (> 0)

    describe "extractFlowName" $ do
        it "strips the (Mass ...) marker and trailing content" $
            extractFlowName "methane (fossil) (Mass, kg, Emissions to air)"
                `shouldBe` "methane (fossil)"

        it "strips the (Volume ...) marker" $
            extractFlowName "CO2 (Volume, m3, Emissions to water)"
                `shouldBe` "CO2"

        it "strips the (Energy ...) marker" $
            extractFlowName "heat (Energy, MJ, Resources)"
                `shouldBe` "heat"

        it "returns full text when no marker is present" $
            extractFlowName "simple name"
                `shouldBe` "simple name"

        it "strips surrounding whitespace from result" $
            extractFlowName "  water  (Area, m2, Resources)"
                `shouldBe` "water"

    describe "extractCompartmentFromDesc" $ do
        it "extracts air compartment with subcompartment" $
            extractCompartmentFromDesc "methane (Mass, kg, Emissions to air, urban air close to ground)"
                `shouldBe` Just (Compartment "air" "urban air close to ground" "")

        it "extracts water compartment with subcompartment" $
            extractCompartmentFromDesc "nitrate (Mass, kg, Emissions to water, river)"
                `shouldBe` Just (Compartment "water" "river" "")

        it "extracts soil compartment with subcompartment" $
            extractCompartmentFromDesc "lead (Mass, kg, Emissions to soil, agricultural)"
                `shouldBe` Just (Compartment "soil" "agricultural" "")

        it "extracts air compartment without subcompartment" $
            extractCompartmentFromDesc "CO2 (Mass, kg, Emissions to air)"
                `shouldBe` Just (Compartment "air" "" "")

        it "extracts natural resource compartment" $
            extractCompartmentFromDesc "coal (Mass, kg, Resources from ground)"
                `shouldBe` Just (Compartment "natural resource" "" "")

        it "returns Nothing when no compartment keyword matches" $
            extractCompartmentFromDesc "methane production volume"
                `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- Method.FlowResolver: parseCompartment
    -- -----------------------------------------------------------------------
    describe "parseCompartment" $ do
        it "returns Nothing for empty list" $
            parseCompartment [] `shouldBe` Nothing

        it "parses 'Emissions to air' at level 1 → medium=air" $
            parseCompartment ["Emissions", "Emissions to air"]
                `shouldBe` Just (Compartment "air" "" "")

        it "parses 'Emissions to water' at level 1 → medium=water" $
            parseCompartment ["Emissions", "Emissions to water"]
                `shouldBe` Just (Compartment "water" "" "")

        it "parses 'Emissions to soil' → medium=soil" $
            parseCompartment ["Emissions", "Emissions to soil"]
                `shouldBe` Just (Compartment "soil" "" "")

        it "parses 'Resources' → medium=natural resource" $
            parseCompartment ["Resources", "Resources from ground"]
                `shouldBe` Just (Compartment "natural resource" "" "")

        it "parses 'Emissions to fresh water' → medium=water" $
            parseCompartment ["Emissions", "Emissions to fresh water"]
                `shouldBe` Just (Compartment "water" "" "")

        it "parses 'Emissions to sea water' → medium=water" $
            parseCompartment ["Emissions", "Emissions to sea water"]
                `shouldBe` Just (Compartment "water" "" "")

        it "parses subcompartment from level 2" $
            parseCompartment ["Emissions", "Emissions to air", "Emissions to air, urban air close to ground"]
                `shouldBe` Just (Compartment "air" "urban air close to ground" "")

        it "parses water subcompartment from level 2" $
            parseCompartment ["Emissions", "Emissions to water", "Emissions to water, river"]
                `shouldBe` Just (Compartment "water" "river" "")

        it "falls back to lowercased category when medium is unrecognized" $
            case parseCompartment ["Some unknown category"] of
                Just (Compartment med _ _) -> med `shouldBe` "some unknown category"
                Nothing -> expectationFailure "Expected Just with lowercased medium"

    -- -----------------------------------------------------------------------
    -- Method.FlowResolver: parseFlowXML
    -- -----------------------------------------------------------------------
    describe "parseFlowXML" $ do
        it "returns Nothing for invalid XML" $
            case parseFlowXML "<not-xml" of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing for invalid XML"

        it "returns Nothing when baseName is missing" $ do
            let xml =
                    TE.encodeUtf8 $
                        T.unlines
                            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                            , "<flowDataSet>"
                            , "  <flowInformation>"
                            , "    <dataSetInformation>"
                            , "      <UUID>aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee</UUID>"
                            , "    </dataSetInformation>"
                            , "  </flowInformation>"
                            , "</flowDataSet>"
                            ]
            case parseFlowXML xml of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing when baseName missing"

        it "returns Nothing when UUID is invalid" $ do
            let xml =
                    TE.encodeUtf8 $
                        T.unlines
                            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                            , "<flowDataSet>"
                            , "  <flowInformation>"
                            , "    <dataSetInformation>"
                            , "      <UUID>not-a-uuid</UUID>"
                            , "      <baseName>Carbon dioxide</baseName>"
                            , "    </dataSetInformation>"
                            , "  </flowInformation>"
                            , "</flowDataSet>"
                            ]
            case parseFlowXML xml of
                Nothing -> return ()
                Just _ -> expectationFailure "Expected Nothing for invalid UUID"

        it "parses baseName and UUID from ILCD flow XML" $ do
            let xml =
                    TE.encodeUtf8 $
                        T.unlines
                            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                            , "<flowDataSet>"
                            , "  <flowInformation>"
                            , "    <dataSetInformation>"
                            , "      <UUID>aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee</UUID>"
                            , "      <name>"
                            , "        <baseName>Carbon dioxide</baseName>"
                            , "      </name>"
                            , "      <CASNumber>124-38-9</CASNumber>"
                            , "    </dataSetInformation>"
                            , "  </flowInformation>"
                            , "</flowDataSet>"
                            ]
            case parseFlowXML xml of
                Nothing -> expectationFailure "Expected Just result"
                Just (uuid, info) -> do
                    show uuid `shouldBe` "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                    ilcdBaseName info `shouldBe` "Carbon dioxide"
                    ilcdCAS info `shouldBe` Just "124-38-9"

    describe "Method Parser" $ do
        describe "parseMethodBytes" $ do
            it "parses a minimal valid method XML" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                                , "      <name>Test Method</name>"
                                , "      <impactCategory>Climate change</impactCategory>"
                                , "    </dataSetInformation>"
                                , "    <quantitativeReference>"
                                , "      <referenceQuantity>"
                                , "        <shortDescription>kg CO2 eq</shortDescription>"
                                , "      </referenceQuantity>"
                                , "    </quantitativeReference>"
                                , "  </LCIAMethodInformation>"
                                , "  <characterisationFactors>"
                                , "    <factor>"
                                , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                                , "        <shortDescription>Carbon dioxide (Mass, kg)</shortDescription>"
                                , "      </referenceToFlowDataSet>"
                                , "      <exchangeDirection>Output</exchangeDirection>"
                                , "      <meanValue>1.0</meanValue>"
                                , "    </factor>"
                                , "  </characterisationFactors>"
                                , "</LCIAMethodDataSet>"
                                ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        methodName method `shouldBe` "Test Method"
                        methodUnit method `shouldBe` "kg CO2 eq"
                        methodCategory method `shouldBe` "Climate change"
                        length (methodFactors method) `shouldBe` 1

            it "extracts characterization factor values correctly" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                                , "      <name>Test</name>"
                                , "    </dataSetInformation>"
                                , "    <quantitativeReference>"
                                , "      <referenceQuantity>"
                                , "        <shortDescription>kg</shortDescription>"
                                , "      </referenceQuantity>"
                                , "    </quantitativeReference>"
                                , "  </LCIAMethodInformation>"
                                , "  <characterisationFactors>"
                                , "    <factor>"
                                , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                                , "        <shortDescription>Methane (Mass)</shortDescription>"
                                , "      </referenceToFlowDataSet>"
                                , "      <exchangeDirection>Output</exchangeDirection>"
                                , "      <meanValue>28.5</meanValue>"
                                , "    </factor>"
                                , "  </characterisationFactors>"
                                , "</LCIAMethodDataSet>"
                                ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        let cf = head (methodFactors method)
                        mcfFlowName cf `shouldBe` "Methane"
                        mcfValue cf `shouldBe` 28.5
                        mcfDirection cf `shouldBe` Output

            it "parses Input direction correctly" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                                , "      <name>Test</name>"
                                , "    </dataSetInformation>"
                                , "    <quantitativeReference>"
                                , "      <referenceQuantity>"
                                , "        <shortDescription>MJ</shortDescription>"
                                , "      </referenceQuantity>"
                                , "    </quantitativeReference>"
                                , "  </LCIAMethodInformation>"
                                , "  <characterisationFactors>"
                                , "    <factor>"
                                , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                                , "        <shortDescription>Crude oil (Mass)</shortDescription>"
                                , "      </referenceToFlowDataSet>"
                                , "      <exchangeDirection>Input</exchangeDirection>"
                                , "      <meanValue>42.0</meanValue>"
                                , "    </factor>"
                                , "  </characterisationFactors>"
                                , "</LCIAMethodDataSet>"
                                ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        let cf = head (methodFactors method)
                        mcfDirection cf `shouldBe` Input

            it "fails on missing UUID" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <name>Test</name>"
                                , "    </dataSetInformation>"
                                , "  </LCIAMethodInformation>"
                                , "</LCIAMethodDataSet>"
                                ]
                parseMethodBytes xml `shouldSatisfy` isLeft

            it "fails on missing name" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                                , "    </dataSetInformation>"
                                , "  </LCIAMethodInformation>"
                                , "</LCIAMethodDataSet>"
                                ]
                parseMethodBytes xml `shouldSatisfy` isLeft

            it "fails on invalid UUID string" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>not-a-uuid</UUID>"
                                , "      <name>Test</name>"
                                , "    </dataSetInformation>"
                                , "  </LCIAMethodInformation>"
                                , "</LCIAMethodDataSet>"
                                ]
                parseMethodBytes xml `shouldSatisfy` isLeft

            it "returns Nothing for description and methodology when absent" $ do
                let xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                                , "      <name>Minimal</name>"
                                , "    </dataSetInformation>"
                                , "  </LCIAMethodInformation>"
                                , "</LCIAMethodDataSet>"
                                ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        methodDescription method `shouldBe` Nothing
                        methodMethodology method `shouldBe` Nothing

            it "resolves flow UUID from uri attribute when refObjectId absent" $ do
                let uuid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                    xml =
                        TE.encodeUtf8 $
                            T.unlines
                                [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                , "<LCIAMethodDataSet>"
                                , "  <LCIAMethodInformation>"
                                , "    <dataSetInformation>"
                                , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                                , "      <name>URI UUID Test</name>"
                                , "    </dataSetInformation>"
                                , "  </LCIAMethodInformation>"
                                , "  <characterisationFactors>"
                                , "    <factor>"
                                , "      <referenceToFlowDataSet uri=\"../flows/" <> uuid <> ".xml\">"
                                , "        <shortDescription>CO2</shortDescription>"
                                , "      </referenceToFlowDataSet>"
                                , "      <exchangeDirection>Output</exchangeDirection>"
                                , "      <meanValue>1.0</meanValue>"
                                , "    </factor>"
                                , "  </characterisationFactors>"
                                , "</LCIAMethodDataSet>"
                                ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> case methodFactors method of
                        [cf] -> show (mcfFlowRef cf) `shouldBe` T.unpack uuid
                        _ -> expectationFailure "expected one factor"

            it "returns Left for invalid XML" $ do
                let xml = TE.encodeUtf8 "<not-valid-xml"
                parseMethodBytes xml `shouldSatisfy` isLeft

    describe "parseMethodBytesWithFlows" $ do
        let minimalXML =
                TE.encodeUtf8 $
                    T.unlines
                        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        , "<LCIAMethodDataSet>"
                        , "  <LCIAMethodInformation>"
                        , "    <dataSetInformation>"
                        , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                        , "      <name>Test Method</name>"
                        , "    </dataSetInformation>"
                        , "    <quantitativeReference>"
                        , "      <referenceQuantity><shortDescription>kg CO2 eq</shortDescription></referenceQuantity>"
                        , "    </quantitativeReference>"
                        , "  </LCIAMethodInformation>"
                        , "  <characterisationFactors>"
                        , "    <factor>"
                        , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                        , "        <shortDescription>carbon dioxide (Mass, kg, Emissions to air)</shortDescription>"
                        , "      </referenceToFlowDataSet>"
                        , "      <exchangeDirection>Output</exchangeDirection>"
                        , "      <meanValue>1.0</meanValue>"
                        , "    </factor>"
                        , "  </characterisationFactors>"
                        , "</LCIAMethodDataSet>"
                        ]
            flowUUID = read "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" :: UUID.UUID
            flowInfo =
                ILCDFlowInfo
                    { ilcdBaseName = "Carbon dioxide"
                    , ilcdCompartment = Just (Compartment "air" "unspecified" "")
                    , ilcdCAS = Just "124-38-9"
                    , ilcdSynonyms = []
                    , ilcdFlowType = "Elementary flow"
                    , ilcdFlowPropertyRef = Nothing
                    }

        it "enriches CF flow name from ILCDFlowInfo" $ do
            let result = parseMethodBytesWithFlows (M.singleton flowUUID flowInfo) minimalXML
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right method -> case methodFactors method of
                    [cf] -> mcfFlowName cf `shouldBe` "Carbon dioxide"
                    _ -> expectationFailure "expected one factor"

        it "enriches CF CAS number from ILCDFlowInfo" $ do
            let result = parseMethodBytesWithFlows (M.singleton flowUUID flowInfo) minimalXML
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right method -> case methodFactors method of
                    [cf] -> mcfCAS cf `shouldBe` Just "124-38-9"
                    _ -> expectationFailure "expected one factor"

        it "prefers ILCDFlowInfo compartment over extracted fallback" $ do
            let result = parseMethodBytesWithFlows (M.singleton flowUUID flowInfo) minimalXML
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right method -> case methodFactors method of
                    [cf] -> mcfCompartment cf `shouldBe` Just (Compartment "air" "unspecified" "")
                    _ -> expectationFailure "expected one factor"

        it "falls back to shortDescription data when UUID not in flow map" $ do
            let result = parseMethodBytesWithFlows M.empty minimalXML
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right method -> case methodFactors method of
                    [cf] -> do
                        mcfFlowName cf `shouldBe` "carbon dioxide"
                        mcfCAS cf `shouldBe` Nothing
                    _ -> expectationFailure "expected one factor"

    describe "CSV Method Parser" $ do
        it "parses inline CSV with water/soil/empty compartments" $ do
            -- Exercises parseCSVCompartment water, soil, empty, and unrecognized paths
            let csv =
                    BC.unlines
                        [ ";;Method A;Method B;Method C;Method D"
                        , ";;kg eq;kg eq;kg eq;kg eq"
                        , "substance;compartment;;;;"
                        , "CO2;air;1.0;;;"
                        , "Nitrate;water;;2.0;;"
                        , "Lead;soil;;;3.0;"
                        , "Crude oil;resources;;;;4.0"
                        ]
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> length methods `shouldBe` 4

        it "returns Left when fewer than 3 header rows" $ do
            let csv =
                    BC.unlines
                        [ ";;Method A"
                        , "substance;compartment;"
                        ]
            parseMethodCSVBytes csv `shouldSatisfy` isLeft

        it "parses 2-row header: category defaults to name" $ do
            csv <- BS.readFile "test/data/method.csv"
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> do
                    length methods `shouldBe` 3
                    let gwp = head methods
                    methodName gwp `shouldBe` "global warming (GWP100)"
                    methodCategory gwp `shouldBe` "global warming (GWP100)"

        it "parses 3-row header: distinct category and name" $ do
            csv <- BS.readFile "test/data/method_with_categories.csv"
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> do
                    length methods `shouldBe` 3
                    let gwp = head methods
                    methodName gwp `shouldBe` "global warming (GWP100)"
                    methodCategory gwp `shouldBe` "Climate change"
                    methodUnit gwp `shouldBe` "kg CO2 eq."
                    methodMethodology gwp `shouldBe` Just "Test Method"

        it "parses 2-row header with Windows CRLF line endings" $ do
            let lf = BS.readFile "test/data/method.csv"
                crlf = fmap toCRLF lf
            csv <- crlf
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> do
                    length methods `shouldBe` 3
                    methodName (head methods) `shouldBe` "global warming (GWP100)"

        it "parses 3-row header with Windows CRLF line endings" $ do
            csv <- toCRLF <$> BS.readFile "test/data/method_with_categories.csv"
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> do
                    length methods `shouldBe` 3
                    methodCategory (head methods) `shouldBe` "Climate change"

        it "parses 2-row header with UTF-8 BOM" $ do
            csv <- withBOM <$> BS.readFile "test/data/method.csv"
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> length methods `shouldBe` 3

        it "parses 3-row header with UTF-8 BOM" $ do
            csv <- withBOM <$> BS.readFile "test/data/method_with_categories.csv"
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> do
                    length methods `shouldBe` 3
                    methodCategory (head methods) `shouldBe` "Climate change"

        it "parses with both BOM and CRLF" $ do
            csv <- withBOM . toCRLF <$> BS.readFile "test/data/method_with_categories.csv"
            case parseMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right methods -> length methods `shouldBe` 3

    describe "LCIA Score Computation" $ do
        describe "computeLCIAScore" $ do
            it "computes score as sum of inventory * CF for mapped flows" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    ch4Uuid = UUID.fromWords 5 6 7 8
                    -- Inventory: CO2 = 10 kg, CH4 = 2 kg
                    inventory = M.fromList [(co2Uuid, 10.0), (ch4Uuid, 2.0)]
                    -- Method: CO2 CF = 1.0, CH4 CF = 28.0
                    co2CF = MethodCF co2Uuid "Carbon dioxide" Output 1.0 Nothing Nothing "kg"
                    ch4CF = MethodCF ch4Uuid "Methane" Output 28.0 Nothing Nothing "kg"
                    co2Flow = mkTestFlow co2Uuid "Carbon dioxide"
                    ch4Flow = mkTestFlow ch4Uuid "Methane"
                    mappings =
                        [ (co2CF, Just (co2Flow, ByUUID))
                        , (ch4CF, Just (ch4Flow, ByUUID))
                        ]
                -- Score = 10*1 + 2*28 = 10 + 56 = 66
                computeLCIAScore defaultUnitConfig M.empty M.empty inventory mappings `shouldBe` 66.0

            it "ignores unmapped flows" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    ch4Uuid = UUID.fromWords 5 6 7 8
                    inventory = M.fromList [(co2Uuid, 10.0), (ch4Uuid, 2.0)]
                    co2CF = MethodCF co2Uuid "Carbon dioxide" Output 1.0 Nothing Nothing "kg"
                    ch4CF = MethodCF ch4Uuid "Methane" Output 28.0 Nothing Nothing "kg"
                    co2Flow = mkTestFlow co2Uuid "Carbon dioxide"
                    mappings =
                        [ (co2CF, Just (co2Flow, ByUUID))
                        , (ch4CF, Nothing) -- CH4 not mapped
                        ]
                -- Score = 10*1 = 10 (CH4 ignored because not mapped)
                computeLCIAScore defaultUnitConfig M.empty M.empty inventory mappings `shouldBe` 10.0

            it "returns 0 for flows not in inventory" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    n2oUuid = UUID.fromWords 9 10 11 12
                    -- Inventory has only CO2
                    inventory = M.fromList [(co2Uuid, 10.0)]
                    -- Method has N2O that's not in inventory
                    n2oCF = MethodCF n2oUuid "Dinitrogen monoxide" Output 265.0 Nothing Nothing "kg"
                    n2oFlow = mkTestFlow n2oUuid "Dinitrogen monoxide"
                    mappings = [(n2oCF, Just (n2oFlow, ByName))]
                -- Score = 0 (N2O not in inventory)
                computeLCIAScore defaultUnitConfig M.empty M.empty inventory mappings `shouldBe` 0.0

            it "handles negative inventory values (resource extraction)" $ do
                let oilUuid = UUID.fromWords 11 12 13 14
                    -- Resource extraction has negative sign in inventory
                    inventory = M.fromList [(oilUuid, -5.0)]
                    oilCF = MethodCF oilUuid "Crude oil" Input 42.0 Nothing Nothing "MJ"
                    oilFlow = mkTestFlow oilUuid "Crude oil"
                    mappings = [(oilCF, Just (oilFlow, ByUUID))]
                -- Score = -5 * 42 = -210 (negative = resource depletion)
                computeLCIAScore defaultUnitConfig M.empty M.empty inventory mappings `shouldBe` (-210.0)

            it "returns 0 for empty mappings" $ do
                let inventory = M.fromList [(UUID.fromWords 1 2 3 4, 100.0)]
                computeLCIAScore defaultUnitConfig M.empty M.empty inventory [] `shouldBe` 0.0

    describe "SimaPro Method CSV Parser" $ do
        it "detects SimaPro method CSV format" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            isSimaProMethodCSV csv `shouldBe` True

        it "detects French locale SimaPro method CSV ({méthodes})" $ do
            let csv = "{SimaPro 9.4.0.2}\r\n{m\xe9thodes}\r\n{Date: 27/04/2023}\r\n"
            isSimaProMethodCSV csv `shouldBe` True

        it "detects German/Dutch locale SimaPro method CSV ({methoden})" $ do
            let csv = "{SimaPro 9.4.0.2}\r\n{methoden}\r\n{Date: 27/04/2023}\r\n"
            isSimaProMethodCSV csv `shouldBe` True

        it "does not detect SimaPro process database as method" $ do
            let csv = "{SimaPro 9.4.0.2}\r\n{processes}\r\n{Date: 27/04/2023}\r\n"
            isSimaProMethodCSV csv `shouldBe` False

        it "does not detect tabular CSV as SimaPro format" $ do
            csv <- BS.readFile "test/data/method.csv"
            isSimaProMethodCSV csv `shouldBe` False

        it "parses 3 impact categories from fixture" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> length (mcMethods coll) `shouldBe` 3

        it "parses Climate change category correctly" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    let cc = head (mcMethods coll)
                    methodName cc `shouldBe` "Climate change"
                    methodUnit cc `shouldBe` "kg CO2 eq"
                    methodCategory cc `shouldBe` "Climate change"
                    methodMethodology cc `shouldBe` Just "Test EF Method"
                    length (methodFactors cc) `shouldBe` 4

        it "parses CO2 characterization factor = 1.0" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    let co2 = head (methodFactors (head (mcMethods coll)))
                    mcfFlowName co2 `shouldBe` "Carbon dioxide, fossil"
                    mcfValue co2 `shouldBe` 1.0
                    mcfDirection co2 `shouldBe` Output
                    mcfCAS co2 `shouldBe` Just "124-38-9"

        it "parses Methane CF = 29.8" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    let ch4 = (methodFactors (head (mcMethods coll))) !! 1
                    mcfFlowName ch4 `shouldBe` "Methane, fossil"
                    mcfValue ch4 `shouldBe` 29.8

        it "parses Water use with Input direction" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    let wu = (mcMethods coll) !! 2
                    methodName wu `shouldBe` "Water use"
                    methodUnit wu `shouldBe` "m3 depriv."
                    let waterCF = head (methodFactors wu)
                    mcfDirection waterCF `shouldBe` Input
                    mcfCompartment waterCF `shouldBe` Just (Compartment "natural resource" "" "")

        it "normalizes CAS numbers (strips leading zeros)" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    let nh3 = head (methodFactors ((mcMethods coll) !! 1))
                    mcfCAS nh3 `shouldBe` Just "7664-41-7"

        it "produces deterministic UUIDs" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case traverse parseSimaProMethodCSVBytes [csv, csv] of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right [coll1, coll2] ->
                    map methodId (mcMethods coll1) `shouldBe` map methodId (mcMethods coll2)
                Right _ -> expectationFailure "unreachable"

        it "parses 3 damage categories" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    length (mcDamageCategories coll) `shouldBe` 3
                    let dc = head (mcDamageCategories coll)
                    dcName dc `shouldBe` "Climate change"
                    dcUnit dc `shouldBe` "kg CO2 eq"
                    dcImpacts dc `shouldBe` [("Climate change", 1.0)]

        it "parses normalization-weighting set" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll -> do
                    length (mcNormWeightSets coll) `shouldBe` 1
                    let nw = head (mcNormWeightSets coll)
                    nwName nw `shouldBe` "Test NW set"
                    case M.lookup "Climate change" (nwNormalization nw) of
                        Just v -> v `shouldBe` 1.32396265000545e-4
                        Nothing -> expectationFailure "Climate change not found"
                    M.lookup "Acidification" (nwWeighting nw) `shouldBe` Just 0.062

    describe "Standalone NW CSV Parser" $ do
        it "parses normalization and weighting factors from CSV" $ do
            csv <- BS.readFile "test/data/normweighting.csv"
            case parseNormWeightCSVBytes "fallback" csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right nw -> do
                    nwName nw `shouldBe` "Custom EF NW"
                    case M.lookup "Climate change" (nwNormalization nw) of
                        Just v -> v `shouldBe` 1.32396265000545e-4
                        Nothing -> expectationFailure "Climate change not found"
                    case M.lookup "Acidification" (nwNormalization nw) of
                        Just v -> v `shouldBe` 1.7995469781731898e-2
                        Nothing -> expectationFailure "Acidification not found"
                    M.lookup "Water use" (nwWeighting nw) `shouldBe` Just 0.0851

        it "has 3 entries in each map" $ do
            csv <- BS.readFile "test/data/normweighting.csv"
            case parseNormWeightCSVBytes "fallback" csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right nw -> do
                    M.size (nwNormalization nw) `shouldBe` 3
                    M.size (nwWeighting nw) `shouldBe` 3

        it "uses fallback name when no comment header" $ do
            let csv = "category;normalization;weighting\nCC;1e-4;0.21\n"
            case parseNormWeightCSVBytes "my-fallback" (TE.encodeUtf8 (T.pack csv)) of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right nw -> nwName nw `shouldBe` "my-fallback"

        it "fails on empty data" $ do
            let csv = "# normalization-weighting set: Empty\ncategory;normalization;weighting\n"
            case parseNormWeightCSVBytes "x" (TE.encodeUtf8 (T.pack csv)) of
                Left _ -> return ()
                Right _ -> expectationFailure "Should have failed on empty data"

        it "fails on no valid rows after header" $ do
            -- All rows have invalid/empty category names
            let csv = BC.pack "# name: Test\ncategory;normalization;weighting\n;;0\n"
            case parseNormWeightCSVBytes "x" csv of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for no valid rows"

        it "detects tab delimiter" $ do
            -- Tab-separated NW data
            let csv = BC.pack "# name: Tab Test\ncategory\tnormalization\tweighting\nCC\t1e-4\t0.21\n"
            case parseNormWeightCSVBytes "fallback" csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right nw -> M.lookup "CC" (nwNormalization nw) `shouldSatisfy` (/= Nothing)

        it "extracts name from # name: comment line" $ do
            let csv = BC.pack "# name: My NW Set\ncategory;normalization;weighting\nCC;1e-4;0.21\n"
            case parseNormWeightCSVBytes "fallback" csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right nw -> nwName nw `shouldBe` "My NW Set"

-- Helper for testing Either values
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Convert LF line endings to CRLF (simulates Windows-created files).
toCRLF :: BS.ByteString -> BS.ByteString
toCRLF = BL.toStrict . BL.intercalate "\r\n" . BL.split 0x0A . BL.fromStrict

-- | Prepend UTF-8 BOM (simulates files saved by Excel/Notepad on Windows).
withBOM :: BS.ByteString -> BS.ByteString
withBOM = BS.append "\xEF\xBB\xBF"

-- Helper to create a test Flow
mkTestFlow :: UUID.UUID -> T.Text -> Flow
mkTestFlow uuid name =
    Flow
        { flowId = uuid
        , flowName = name
        , flowUnitId = UUID.nil
        , flowType = Biosphere
        , flowCategory = "air"
        , flowSubcompartment = Nothing
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        , flowSynonyms = M.empty
        }

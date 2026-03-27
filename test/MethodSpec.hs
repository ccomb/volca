{-# LANGUAGE OverloadedStrings #-}

module MethodSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Method.Mapping (computeLCIAScore, MatchStrategy(..))
import Method.Parser
import Method.ParserCSV (parseMethodCSVBytes)
import Method.ParserSimaPro (parseSimaProMethodCSVBytes, isSimaProMethodCSV)
import Method.ParserNW (parseNormWeightCSVBytes)
import Method.Types
import SynonymDB
import Types (Flow(..), FlowType(..))

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

    describe "Method Parser" $ do
        describe "parseMethodBytes" $ do
            it "parses a minimal valid method XML" $ do
                let xml = TE.encodeUtf8 $ T.unlines
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
                let xml = TE.encodeUtf8 $ T.unlines
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
                let xml = TE.encodeUtf8 $ T.unlines
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
                let xml = TE.encodeUtf8 $ T.unlines
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
                let xml = TE.encodeUtf8 $ T.unlines
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

    describe "CSV Method Parser" $ do
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

    describe "LCIA Score Computation" $ do
        describe "computeLCIAScore" $ do
            it "computes score as sum of inventory * CF for mapped flows" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    ch4Uuid = UUID.fromWords 5 6 7 8
                    -- Inventory: CO2 = 10 kg, CH4 = 2 kg
                    inventory = M.fromList [(co2Uuid, 10.0), (ch4Uuid, 2.0)]
                    -- Method: CO2 CF = 1.0, CH4 CF = 28.0
                    co2CF = MethodCF co2Uuid "Carbon dioxide" Output 1.0 Nothing Nothing
                    ch4CF = MethodCF ch4Uuid "Methane" Output 28.0 Nothing Nothing
                    co2Flow = mkTestFlow co2Uuid "Carbon dioxide"
                    ch4Flow = mkTestFlow ch4Uuid "Methane"
                    mappings = [ (co2CF, Just (co2Flow, ByUUID))
                               , (ch4CF, Just (ch4Flow, ByUUID))
                               ]
                -- Score = 10*1 + 2*28 = 10 + 56 = 66
                computeLCIAScore M.empty inventory mappings `shouldBe` 66.0

            it "ignores unmapped flows" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    ch4Uuid = UUID.fromWords 5 6 7 8
                    inventory = M.fromList [(co2Uuid, 10.0), (ch4Uuid, 2.0)]
                    co2CF = MethodCF co2Uuid "Carbon dioxide" Output 1.0 Nothing Nothing
                    ch4CF = MethodCF ch4Uuid "Methane" Output 28.0 Nothing Nothing
                    co2Flow = mkTestFlow co2Uuid "Carbon dioxide"
                    mappings = [ (co2CF, Just (co2Flow, ByUUID))
                               , (ch4CF, Nothing)  -- CH4 not mapped
                               ]
                -- Score = 10*1 = 10 (CH4 ignored because not mapped)
                computeLCIAScore M.empty inventory mappings `shouldBe` 10.0

            it "returns 0 for flows not in inventory" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    n2oUuid = UUID.fromWords 9 10 11 12
                    -- Inventory has only CO2
                    inventory = M.fromList [(co2Uuid, 10.0)]
                    -- Method has N2O that's not in inventory
                    n2oCF = MethodCF n2oUuid "Dinitrogen monoxide" Output 265.0 Nothing Nothing
                    n2oFlow = mkTestFlow n2oUuid "Dinitrogen monoxide"
                    mappings = [ (n2oCF, Just (n2oFlow, ByName)) ]
                -- Score = 0 (N2O not in inventory)
                computeLCIAScore M.empty inventory mappings `shouldBe` 0.0

            it "handles negative inventory values (resource extraction)" $ do
                let oilUuid = UUID.fromWords 11 12 13 14
                    -- Resource extraction has negative sign in inventory
                    inventory = M.fromList [(oilUuid, -5.0)]
                    oilCF = MethodCF oilUuid "Crude oil" Input 42.0 Nothing Nothing
                    oilFlow = mkTestFlow oilUuid "Crude oil"
                    mappings = [ (oilCF, Just (oilFlow, ByUUID)) ]
                -- Score = -5 * 42 = -210 (negative = resource depletion)
                computeLCIAScore M.empty inventory mappings `shouldBe` (-210.0)

            it "returns 0 for empty mappings" $ do
                let inventory = M.fromList [(UUID.fromWords 1 2 3 4, 100.0)]
                computeLCIAScore M.empty inventory [] `shouldBe` 0.0

    describe "SimaPro Method CSV Parser" $ do
        it "detects SimaPro method CSV format" $ do
            csv <- BS.readFile "test/data/simapro_method.csv"
            isSimaProMethodCSV csv `shouldBe` True

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
            case parseSimaProMethodCSVBytes csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right coll1 ->
                    case parseSimaProMethodCSVBytes csv of
                        Left err -> expectationFailure $ "Parse failed: " ++ err
                        Right coll2 ->
                            map methodId (mcMethods coll1) `shouldBe` map methodId (mcMethods coll2)

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
                    M.lookup "Climate change" (nwNormalization nw) `shouldBe` Just 7575.76
                    M.lookup "Acidification" (nwWeighting nw) `shouldBe` Just 0.062

    describe "Standalone NW CSV Parser" $ do
        it "parses normalization and weighting factors from CSV" $ do
            csv <- BS.readFile "test/data/normweighting.csv"
            case parseNormWeightCSVBytes "fallback" csv of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right nw -> do
                    nwName nw `shouldBe` "Custom EF NW"
                    M.lookup "Climate change" (nwNormalization nw) `shouldBe` Just 7575.76
                    case M.lookup "Acidification" (nwNormalization nw) of
                        Just v -> abs (v - 55.56) `shouldSatisfy` (< 1e-10)
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

-- Helper for testing Either values
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper to create a test Flow
mkTestFlow :: UUID.UUID -> T.Text -> Flow
mkTestFlow uuid name = Flow
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

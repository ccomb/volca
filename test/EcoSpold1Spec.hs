{-# LANGUAGE OverloadedStrings #-}

module EcoSpold1Spec (spec) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.UUID (nil)
import Test.Hspec

import EcoSpold.Parser1
import Types

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A minimal Activity with no exchanges
emptyActivity :: Activity
emptyActivity =
    Activity
        { activityName = "Test Activity"
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityUnit = "kg"
        , exchanges = []
        , activityParams = M.empty
        , activityParamExprs = M.empty
        }

-- | A production output exchange (isInput=False, isReference=False by default)
mkOutput :: UUID -> Double -> Exchange
mkOutput fid amt = TechnosphereExchange fid amt nil False False nil Nothing ""

-- | A reference output exchange
mkRefOutput :: UUID -> Double -> Exchange
mkRefOutput fid amt = TechnosphereExchange fid amt nil False True nil Nothing ""

-- | A technosphere input exchange
mkInput :: UUID -> Double -> Exchange
mkInput fid amt = TechnosphereExchange fid amt nil True False nil Nothing ""

-- | A biosphere exchange
mkBio :: UUID -> Double -> Exchange
mkBio fid amt = BiosphereExchange fid amt nil False ""

fid1, fid2, fid3 :: UUID
fid1 = read "11111111-1111-1111-1111-111111111111"
fid2 = read "22222222-2222-2222-2222-222222222222"
fid3 = read "33333333-3333-3333-3333-333333333333"

-- | Minimal valid EcoSpold1 XML with one reference product and one air emission
minimalXml :: BC.ByteString
minimalXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"42\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"electricity production\" category=\"Energy\""
        , "                           subCategory=\"Electricity\" unit=\"kWh\""
        , "                           generalComment=\"A comment\"/>"
        , "        <geography location=\"DE\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"electricity, high voltage\" category=\"Energy\""
        , "                subCategory=\"Electricity\" unit=\"kWh\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Carbon dioxide, fossil\" category=\"air\""
        , "                subCategory=\"low population density\" unit=\"kg\" meanValue=\"0.05\""
        , "                CASNumber=\"124-38-9\">"
        , "        <outputGroup>4</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"3\" name=\"natural gas\" category=\"resource\""
        , "                subCategory=\"in ground\" unit=\"MJ\" meanValue=\"10.0\">"
        , "        <inputGroup>4</inputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"4\" name=\"fuel oil\" category=\"Liquid fuels\""
        , "                unit=\"kg\" meanValue=\"2.0\">"
        , "        <inputGroup>5</inputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

-- | Multi-dataset EcoSpold1 XML
multiDatasetXml :: BC.ByteString
multiDatasetXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"1\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"process A\" unit=\"kg\"/>"
        , "        <geography location=\"CH\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"product A\" category=\"goods\" unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "  <dataset number=\"2\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"process B\" unit=\"kg\"/>"
        , "        <geography location=\"FR\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"product B\" category=\"goods\" unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "generateFlowUUID" $ do
        it "produces a stable UUID for known inputs" $
            generateFlowUUID 42 1 "CO2" "air"
                `shouldBe` read "4b869b57-e716-5e42-ae6f-0536cf112615"

        it "differs when dataset number changes" $
            generateFlowUUID 1 1 "CO2" "air"
                `shouldNotBe` generateFlowUUID 2 1 "CO2" "air"

        it "differs when exchange number changes" $
            generateFlowUUID 1 1 "CO2" "air"
                `shouldNotBe` generateFlowUUID 1 2 "CO2" "air"

        it "differs when flow name changes" $
            generateFlowUUID 1 1 "CO2" "air"
                `shouldNotBe` generateFlowUUID 1 1 "methane" "air"

    describe "generateUnitUUID" $ do
        it "produces a stable UUID for known inputs" $
            generateUnitUUID "kg" `shouldBe` read "d74bc05e-6502-555a-a40c-e6e7580dbf93"

        it "differs for different unit names" $
            generateUnitUUID "kg" `shouldNotBe` generateUnitUUID "m3"

    describe "isProductionExchange" $ do
        it "is True for a TechnosphereExchange output (isInput=False)" $
            isProductionExchange (mkOutput fid1 1.0) `shouldBe` True

        it "is False for a TechnosphereExchange input (isInput=True)" $
            isProductionExchange (mkInput fid1 1.0) `shouldBe` False

        it "is False for a BiosphereExchange" $
            isProductionExchange (mkBio fid1 1.0) `shouldBe` False

    describe "hasReferenceProduct" $ do
        it "is False for an activity with no exchanges" $
            hasReferenceProduct emptyActivity `shouldBe` False

        it "is False when only non-reference outputs exist" $ do
            let act = emptyActivity{exchanges = [mkOutput fid1 1.0]}
            hasReferenceProduct act `shouldBe` False

        it "is True when at least one reference exchange exists" $ do
            let act = emptyActivity{exchanges = [mkRefOutput fid1 1.0]}
            hasReferenceProduct act `shouldBe` True

    describe "removeZeroAmountCoproducts" $ do
        it "keeps non-zero production outputs" $
            length (removeZeroAmountCoproducts [mkOutput fid1 1.0]) `shouldBe` 1

        it "removes zero-amount non-reference production outputs" $
            length (removeZeroAmountCoproducts [mkOutput fid1 0.0]) `shouldBe` 0

        it "keeps reference outputs even at zero amount" $ do
            let result = removeZeroAmountCoproducts [mkRefOutput fid1 0.0]
            length result `shouldBe` 1

        it "keeps inputs regardless of amount" $
            length (removeZeroAmountCoproducts [mkInput fid1 0.0]) `shouldBe` 1

        it "keeps biosphere exchanges regardless of amount" $
            length (removeZeroAmountCoproducts [mkBio fid1 0.0]) `shouldBe` 1

    describe "assignSingleProductAsReference" $ do
        it "marks the only non-zero production output as reference" $ do
            let act = emptyActivity{exchanges = [mkOutput fid1 1.0, mkBio fid2 0.5]}
                result = assignSingleProductAsReference act
            length (filter exchangeIsReference (exchanges result)) `shouldBe` 1

        it "does not assign when multiple non-zero outputs exist" $ do
            let act = emptyActivity{exchanges = [mkOutput fid1 1.0, mkOutput fid2 2.0]}
                result = assignSingleProductAsReference act
            length (filter exchangeIsReference (exchanges result)) `shouldBe` 0

        it "does not assign when no non-zero outputs exist" $ do
            let act = emptyActivity{exchanges = [mkOutput fid1 0.0]}
                result = assignSingleProductAsReference act
            length (filter exchangeIsReference (exchanges result)) `shouldBe` 0

    describe "applyCutoffStrategy" $ do
        it "returns Right when a reference product exists" $ do
            let act = emptyActivity{exchanges = [mkRefOutput fid1 1.0]}
            case applyCutoffStrategy act of
                Right _ -> return ()
                Left err -> expectationFailure $ "Expected Right: " ++ err

        it "assigns reference product when exactly one non-zero output" $ do
            let act = emptyActivity{exchanges = [mkOutput fid1 1.0]}
            case applyCutoffStrategy act of
                Right result -> length (filter exchangeIsReference (exchanges result)) `shouldBe` 1
                Left err -> expectationFailure $ "Expected Right: " ++ err

        it "removes zero-amount coproducts before strategy" $ do
            let act = emptyActivity{exchanges = [mkRefOutput fid1 1.0, mkOutput fid2 0.0]}
            case applyCutoffStrategy act of
                Right result -> length (exchanges result) `shouldBe` 1
                Left err -> expectationFailure $ "Expected Right: " ++ err

        it "returns Left when no reference product can be assigned" $ do
            let act = emptyActivity{exchanges = []}
            case applyCutoffStrategy act of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for activity with no exchanges"

        it "returns Left when multiple non-zero outputs and no reference" $ do
            let act = emptyActivity{exchanges = [mkOutput fid1 1.0, mkOutput fid2 2.0]}
            case applyCutoffStrategy act of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for ambiguous coproducts"

    -- -----------------------------------------------------------------------
    -- parseWithXeno — parsing inline XML
    -- -----------------------------------------------------------------------
    describe "parseWithXeno" $ do
        it "returns Left for invalid XML" $
            case parseWithXeno "<not-xml" of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for invalid XML"

        it "parses activity name from referenceFunction" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) -> activityName act `shouldBe` "electricity production"

        it "parses activity location from geography" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) -> activityLocation act `shouldBe` "DE"

        it "parses activity unit from referenceFunction" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) -> activityUnit act `shouldBe` "kWh"

        it "parses dataset number" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, _, _, num, _) -> num `shouldBe` 42

        it "produces 4 exchanges" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) -> length (exchanges act) `shouldBe` 4

        it "produces 4 flows" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, flows, _, _, _) -> length flows `shouldBe` 4

        it "marks the reference output (outputGroup 0) as isReference" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) ->
                    length (filter exchangeIsReference (exchanges act)) `shouldBe` 1

        it "marks biosphere exchange (outputGroup 4) as BiosphereExchange" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) ->
                    let bios = filter (\e -> case e of BiosphereExchange{} -> True; _ -> False) (exchanges act)
                     in length bios `shouldBe` 2 -- CO2 output + natural gas input (inputGroup 4)
        it "parses flow with CAS number" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, flows, _, _, _) ->
                    let co2Flows = filter (\f -> flowName f == "Carbon dioxide, fossil") flows
                     in case co2Flows of
                            [f] -> flowCAS f `shouldBe` Just "124-38-9"
                            _ -> expectationFailure "Expected exactly one CO2 flow"

        it "sets activity classification from category/subCategory" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _) ->
                    M.lookup "Category" (activityClassification act) `shouldBe` Just "Energy"

    -- -----------------------------------------------------------------------
    -- parseAllWithXeno — multi-dataset
    -- -----------------------------------------------------------------------
    describe "parseAllWithXeno" $ do
        it "returns Left for invalid XML" $
            case parseAllWithXeno "<not-xml" of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for invalid XML"

        it "parses two datasets from multi-dataset XML" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results -> length results `shouldBe` 2

        it "parses activity names from both datasets" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results ->
                    let names = [activityName act | Right (act, _, _, _, _) <- results]
                     in names `shouldBe` ["process A", "process B"]

        it "preserves dataset numbers in order" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results ->
                    let nums = [n | Right (_, _, _, n, _) <- results]
                     in nums `shouldBe` [1, 2]

        it "parses location from each dataset" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results ->
                    let locs = [activityLocation act | Right (act, _, _, _, _) <- results]
                     in locs `shouldBe` ["CH", "FR"]

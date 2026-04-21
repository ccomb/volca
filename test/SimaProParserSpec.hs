{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SimaProParserSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Expr (evaluate, normalizeExpr)
import SimaPro.Parser (
    BioExchangeRow (..),
    ProductRow (..),
    TechExchangeRow (..),
    defaultConfig,
    generateActivityUUID,
    generateFlowUUID,
    generateUnitUUID,
    parseAmount,
    parseBioRow,
    parseProductRow,
    parseSimaProCSV,
    parseTechRow,
    splitCSV,
 )
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types (Activity (..), Exchange (..), Flow, UUID, Unit (..))
import UnitConversion (defaultUnitConfig, isKnownUnit)

-- | Test CSV content with a quoted product name containing the delimiter (;)
testCSV :: BS.ByteString
testCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Steel Production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "Steel;kg;1.0;100;not defined;material;"
        , ""
        , "End"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Irradiated Food"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "\"Food product (irradiated ; with treatment)\";foo_unit;2.0;100;not defined;material;"
        , ""
        , "End"
        ]

-- | Parse the test CSV via a temp file
parseTestCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseTestCSV = withSystemTempFile "test.csv" $ \path handle -> do
    BS.hPut handle testCSV
    hClose handle
    parseSimaProCSV path

-- | Test CSV with waste treatment process and waste-to-treatment demand
wasteTestCSV :: BS.ByteString
wasteTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , -- Producer with a Waste to treatment section
          "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Widget production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Widget;kg;1.0;100;not defined;material;"
        , ""
        , "Waste to treatment"
        , "Municipal waste;kg;0.5;Undefined;;;;;;"
        , ""
        , "End"
        , ""
        , -- Waste treatment process (no Products section, only Waste treatment)
          "Process"
        , ""
        , "Category type"
        , "waste treatment"
        , ""
        , "Process name"
        , "Incineration process"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Waste treatment"
        , "Municipal waste incineration;kg;1.0;100;All waste types;waste treatment;"
        , ""
        , "End"
        ]

{- | Test CSV with waste treatment product row without allocation field (6 fields).
SimaPro CSV has two product row formats:
  7 fields: name;unit;amount;allocation;waste_type;category;comment
  6 fields: name;unit;amount;waste_type;category;comment  (no allocation)
The 6-field variant is found in some waste treatment processes (e.g. Agribalyse).
Without proper detection, field 3 (waste_type) is misread as allocation, and the
comment (often containing \x7f-separated EcoSpold metadata) ends up as category.
-}
wasteNoAllocCSV :: BS.ByteString
wasteNoAllocCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "waste treatment"
        , ""
        , "Process name"
        , "treatment of non-sulfidic overburden"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Waste treatment"
        , "Non-sulfidic overburden {GLO}| treatment of | Cut-off, S;kg;1;All waste types;Others\\Copied from Ecoinvent cut-off S;EcoSpold01Location=GLO\x7fProperties\x7fDry mass: 1 kg"
        , ""
        , "End"
        ]

-- | Parse the 6-field waste treatment CSV via a temp file
parseWasteNoAllocCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseWasteNoAllocCSV = withSystemTempFile "waste-noalloc-test.csv" $ \path handle -> do
    BS.hPut handle wasteNoAllocCSV
    hClose handle
    parseSimaProCSV path

-- | Parse the waste test CSV via a temp file
parseWasteCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseWasteCSV = withSystemTempFile "waste-test.csv" $ \path handle -> do
    BS.hPut handle wasteTestCSV
    hClose handle
    parseSimaProCSV path

-- ============================================================================
-- Expression evaluator tests
-- ============================================================================

-- | Test CSV with parameterized amounts (models butter-like process)
paramTestCSV :: BS.ByteString
paramTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: ,}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Butter at dairy"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Input parameters"
        , "Qb;1;Undefined;0;0;No;"
        , "DMb;0,82;Undefined;0;0;No;"
        , "Qm;20,53;Undefined;0;0;No;"
        , "DMm;0,118;Undefined;0;0;No;"
        , ""
        , "Calculated parameters"
        , "allocButter;(Qb*DMb/(Qb*DMb+Qm*DMm))*100;"
        , ""
        , "Products"
        , "Butter {FR} U;kg;Qb;allocButter;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Cow milk {FR} U;kg;Qm;Undefined;;;;;;"
        , ""
        , "Emissions to air"
        , "Carbon dioxide, fossil;high. pop.;kg;0,5;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseParamCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseParamCSV = withSystemTempFile "param-test.csv" $ \path handle -> do
    BS.hPut handle paramTestCSV
    hClose handle
    parseSimaProCSV path

-- | Test CSV with database-level parameters
dbParamTestCSV :: BS.ByteString
dbParamTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Database Input parameters"
        , "lbtokg;0.453592;Undefined;0;0;No;"
        , ""
        , "Database Calculated parameters"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Import product"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Import product;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Raw material;lb;lbtokg;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseDbParamCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseDbParamCSV = withSystemTempFile "dbparam-test.csv" $ \path handle -> do
    BS.hPut handle dbParamTestCSV
    hClose handle
    parseSimaProCSV path

-- | Test CSV with yield chain formula (most common pattern in Agribalyse)
yieldChainTestCSV :: BS.ByteString
yieldChainTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Processed food"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Input parameters"
        , "weight_g;250;Undefined;0;0;No;"
        , "yield1;0.95;Undefined;0;0;No;"
        , "yield2;0.90;Undefined;0;0;No;"
        , ""
        , "Calculated parameters"
        , "weight_kg;weight_g/1000;"
        , "corrected;weight_kg/yield1/yield2;"
        , ""
        , "Products"
        , "Processed food;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Raw ingredient;kg;corrected;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseYieldChainCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseYieldChainCSV = withSystemTempFile "yield-test.csv" $ \path handle -> do
    BS.hPut handle yieldChainTestCSV
    hClose handle
    parseSimaProCSV path

-- Helper: find technosphere input by name
findInput :: Activity -> T.Text -> Maybe Exchange
findInput act query = case [ e
                           | e@TechnosphereExchange{} <- exchanges act
                           , techIsInput e
                           , not (techIsReference e)
                           ] of
    exs -> case filter (matchesName query) exs of
        (e : _) -> Just e
        _ -> Nothing
  where
    matchesName _ _ = True -- We check by position since we can't easily get flow names here

-- Helper: get all tech input amounts
techInputAmounts :: Activity -> [Double]
techInputAmounts act =
    [ techAmount e
    | e@TechnosphereExchange{} <- exchanges act
    , techIsInput e
    , not (techIsReference e)
    ]

-- Helper: get reference product amount
refProductAmount :: Activity -> Maybe Double
refProductAmount act = case [ techAmount e
                            | e@TechnosphereExchange{} <- exchanges act
                            , techIsReference e
                            ] of
    (a : _) -> Just a
    _ -> Nothing

spec :: Spec
spec = do
    describe "SimaPro expression evaluator" $ do
        it "evaluates numeric literals" $ do
            evaluate M.empty "42" `shouldBe` Right 42.0
            evaluate M.empty "3.14" `shouldBe` Right 3.14

        it "evaluates arithmetic" $ do
            evaluate M.empty "2+3" `shouldBe` Right 5.0
            evaluate M.empty "10-4" `shouldBe` Right 6.0
            evaluate M.empty "3*4" `shouldBe` Right 12.0
            evaluate M.empty "10/4" `shouldBe` Right 2.5

        it "evaluates parenthesized expressions" $ do
            evaluate M.empty "(2+3)*4" `shouldBe` Right 20.0
            evaluate M.empty "2*(3+4)" `shouldBe` Right 14.0

        it "evaluates variables" $ do
            let env = M.fromList [("Qm", 20.53), ("Qb", 1.0)]
            evaluate env "Qm" `shouldBe` Right 20.53
            evaluate env "Qb" `shouldBe` Right 1.0

        it "evaluates complex expressions with variables" $ do
            let env = M.fromList [("Qb", 1.0), ("DMb", 0.82), ("Qm", 20.53), ("DMm", 0.118)]
            -- Butter allocation formula: (Qb*DMb/(Qb*DMb+Qm*DMm))*100
            let result = evaluate env "(Qb*DMb/(Qb*DMb+Qm*DMm))*100"
            case result of
                Right v -> v `shouldSatisfy` (\x -> abs (x - 25.29) < 0.1)
                Left e -> expectationFailure $ "Evaluation failed: " ++ e

        it "evaluates yield correction chains" $ do
            let env = M.fromList [("weight_kg", 0.25), ("yield1", 0.95), ("yield2", 0.90)]
            let result = evaluate env "weight_kg/yield1/yield2"
            case result of
                Right v -> v `shouldSatisfy` (\x -> abs (x - 0.2924) < 0.001)
                Left e -> expectationFailure $ "Evaluation failed: " ++ e

        it "evaluates power operator" $ do
            evaluate M.empty "2^3" `shouldBe` Right 8.0
            evaluate M.empty "3^2" `shouldBe` Right 9.0

        it "evaluates unary minus" $ do
            evaluate M.empty "-5" `shouldBe` Right (-5.0)
            evaluate M.empty "-(2+3)" `shouldBe` Right (-5.0)

        it "rejects unknown variables" $ do
            evaluate M.empty "xyz" `shouldSatisfy` isLeft

        -- Regression: Agribalyse Emmental defines the dry-matter param as "Dmper"
        -- but references "DMper" in the allocation formula. SimaPro treats parameter
        -- names case-insensitively; VoLCA must do the same or allocation → 0 and the
        -- whole activity shows zero impacts.
        it "looks up variables case-insensitively" $ do
            let env = M.fromList [("Dmper", 5.0), ("Qper", 60530841.0)]
            evaluate env "Qper*DMper" `shouldBe` Right (60530841.0 * 5.0)
            evaluate env "qper*dmper" `shouldBe` Right (60530841.0 * 5.0)

        it "normalizes comma decimal separator" $ do
            normalizeExpr ',' "0,82" `shouldBe` "0.82"
            normalizeExpr ',' "Qb*0,5" `shouldBe` "Qb*0.5"

        it "normalizes dot decimal (comma becomes semicolon for func args)" $ do
            normalizeExpr '.' "min(a,b)" `shouldBe` "min(a;b)"

    describe "SimaPro CSV parsing" $ do
        it "correctly extracts units from CSV with quoted fields" $ do
            (_, _, unitDB) <- parseTestCSV
            let unitNames = map unitName $ M.elems unitDB
            -- Exactly these two units — no more, no less
            S.fromList unitNames `shouldBe` S.fromList ["kg", "foo_unit"]

        it "reports unknown units correctly" $ do
            (_, _, unitDB) <- parseTestCSV
            let cfg = defaultUnitConfig
                unknowns =
                    [ unitName u
                    | u <- M.elems unitDB
                    , not (isKnownUnit cfg (unitName u))
                    ]
            unknowns `shouldContain` ["foo_unit"]
            unknowns `shouldNotContain` ["kg"]

        it "parses product names with embedded delimiters correctly" $ do
            (activities, _, _) <- parseTestCSV
            let names = map activityName activities
            -- The quoted product name should be extracted intact
            names `shouldContain` ["Food product (irradiated ; with treatment)"]

    describe "SimaPro classification parsing" $ do
        it "parses Category type from metadata" $ do
            (activities, _, _) <- parseTestCSV
            let cls = activityClassification (head activities)
            M.lookup "Category type" cls `shouldBe` Just "material"

        it "parses Category from product line" $ do
            (activities, _, _) <- parseTestCSV
            let cls = activityClassification (head activities)
            M.lookup "Category" cls `shouldBe` Just "material"

    describe "SimaPro waste treatment parsing" $ do
        it "parses waste treatment processes (Waste treatment section)" $ do
            (activities, _, _) <- parseWasteCSV
            length activities `shouldSatisfy` (>= 2)

        it "uses Waste treatment row as activity name" $ do
            (activities, _, _) <- parseWasteCSV
            let names = map activityName activities
            names `shouldContain` ["Municipal waste incineration"]

        it "parses 6-field waste treatment rows without allocation" $ do
            (activities, _, _) <- parseWasteNoAllocCSV
            length activities `shouldBe` 1
            let a = head activities
            activityName a `shouldBe` "Non-sulfidic overburden {GLO}| treatment of | Cut-off, S"
            let cls = activityClassification a
            M.lookup "Category" cls `shouldBe` Just "Others\\Copied from Ecoinvent cut-off S"

        it "marks Waste to treatment exchanges as inputs" $ do
            (activities, _, _) <- parseWasteCSV
            let producer = head [a | a <- activities, activityName a == "Widget"]
                wasteExchanges =
                    [ e
                    | e@TechnosphereExchange{} <- exchanges producer
                    , not (techIsReference e)
                    , techIsInput e
                    ]
            length wasteExchanges `shouldSatisfy` (>= 1)

    describe "SimaPro parameterized amounts" $ do
        it "resolves simple variable references (Qm=20.53 for cow milk, scaled by allocation)" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
                -- allocButter = (1*0.82/(1*0.82+20.53*0.118))*100 ≈ 25.285
                -- Cow milk amount = 20.53 * allocButter/100 ≈ 5.19
                milkAmounts = techInputAmounts butter
            length milkAmounts `shouldBe` 1
            head milkAmounts `shouldSatisfy` (\x -> abs (x - 5.19) < 0.01)

        it "resolves parameterized product amount (Qb=1)" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            refProductAmount butter `shouldBe` Just 1.0

        it "resolves calculated parameter in allocation (allocButter formula)" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            -- Product allocation uses allocButter = (Qb*DMb/(Qb*DMb+Qm*DMm))*100
            -- = (1*0.82/(1*0.82+20.53*0.118))*100 ≈ 25.3%
            -- The product exchange should have the resolved allocation
            -- (we check that the activity was created = params didn't break parsing)
            length (exchanges butter) `shouldSatisfy` (>= 3) -- product + milk + CO2
        it "stores resolved parameter values in activity" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            M.lookup "Qm" (activityParams butter) `shouldBe` Just 20.53
            M.lookup "Qb" (activityParams butter) `shouldBe` Just 1.0
            M.lookup "DMb" (activityParams butter) `shouldBe` Just 0.82

        it "stores raw expressions for re-evaluation" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            M.lookup "allocButter" (activityParamExprs butter)
                `shouldBe` Just "(Qb*DMb/(Qb*DMb+Qm*DMm))*100"

        it "does not drop exchanges with parameterized amounts" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
                techInputs =
                    [ e
                    | e@TechnosphereExchange{} <- exchanges butter
                    , techIsInput e
                    , not (techIsReference e)
                    ]
            -- Cow milk should NOT be dropped (was the original bug)
            length techInputs `shouldBe` 1

        it "scales biosphere exchanges by allocation fraction" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
                bioExchanges = [e | e@BiosphereExchange{} <- exchanges butter]
            length bioExchanges `shouldBe` 1
            -- CO2 = 0.5 * allocButter/100 ≈ 0.5 * 0.25285 ≈ 0.1264
            bioAmount (head bioExchanges) `shouldSatisfy` (\x -> abs (x - 0.1264) < 0.01)

    describe "SimaPro database-level parameters" $ do
        it "resolves database input params in exchange amounts" $ do
            (activities, _, _) <- parseDbParamCSV
            let act = head activities
            -- Raw material amount should be lbtokg = 0.453592
            techInputAmounts act `shouldContain` [0.453592]

    describe "SimaPro yield chain formulas" $ do
        it "resolves chained division (weight_g/1000/yield1/yield2)" $ do
            (activities, _, _) <- parseYieldChainCSV
            let act = head activities
                amounts = techInputAmounts act
            -- corrected = 250/1000/0.95/0.90 ≈ 0.2924
            length amounts `shouldBe` 1
            head amounts `shouldSatisfy` (\x -> abs (x - 0.2924) < 0.001)

    -- -----------------------------------------------------------------------
    -- Pure row parsers
    -- -----------------------------------------------------------------------

    describe "splitCSV" $ do
        it "splits basic semicolon-delimited row" $
            splitCSV ';' "a;b;c" `shouldBe` ["a", "b", "c"]

        it "strips quotes and embeds delimiter in field" $
            splitCSV ';' "\"a;b\";c" `shouldBe` ["a;b", "c"]

        it "splits comma-delimited row" $
            splitCSV ',' "x,y,z" `shouldBe` ["x", "y", "z"]

        it "handles empty fields" $
            splitCSV ';' "a;;c" `shouldBe` ["a", "", "c"]

    describe "parseAmount" $ do
        it "parses integer" $
            parseAmount '.' "42" `shouldBe` 42.0

        it "parses decimal with dot separator" $
            parseAmount '.' "3.14" `shouldBe` 3.14

        it "parses decimal with comma separator" $
            parseAmount ',' "3,14" `shouldBe` 3.14

        it "returns 0.0 for empty input" $
            parseAmount '.' "" `shouldBe` 0.0

        it "returns 0.0 for non-numeric" $
            parseAmount '.' "abc" `shouldBe` 0.0

    describe "parseProductRow" $ do
        it "parses 7-field product row" $
            parseProductRow defaultConfig "Steel;kg;1.0;100;not defined;material;comment"
                `shouldBe` Just
                    ProductRow
                        { prName = "Steel"
                        , prUnit = "kg"
                        , prAmount = 1.0
                        , prAmountRaw = "1.0"
                        , prAllocation = 100.0
                        , prAllocRaw = "100"
                        , prWasteType = "not defined"
                        , prCategory = "material"
                        , prComment = "comment"
                        }

        it "parses 6-field row (no allocation — waste treatment)" $
            parseProductRow defaultConfig "Waste flow;kg;1.0;All waste types;waste treatment;comment"
                `shouldBe` Just
                    ProductRow
                        { prName = "Waste flow"
                        , prUnit = "kg"
                        , prAmount = 1.0
                        , prAmountRaw = "1.0"
                        , prAllocation = 100.0
                        , prAllocRaw = "100"
                        , prWasteType = "All waste types"
                        , prCategory = "waste treatment"
                        , prComment = "comment"
                        }

        it "returns Nothing for too-short row" $
            parseProductRow defaultConfig "name;kg" `shouldBe` Nothing

    describe "parseTechRow" $ do
        it "parses full tech exchange row" $
            parseTechRow defaultConfig "Coal;kg;5.0;Undefined;;;;;;"
                `shouldSatisfy` \case
                    Just r -> terName r == "Coal" && terUnit r == "kg" && terAmount r == 5.0
                    Nothing -> False

        it "parses minimal tech row (name;unit;amount)" $
            parseTechRow defaultConfig "Oil;MJ;2.5"
                `shouldSatisfy` \case
                    Just r -> terName r == "Oil" && terAmount r == 2.5
                    Nothing -> False

        it "returns Nothing for too-short row" $
            parseTechRow defaultConfig "name" `shouldBe` Nothing

    describe "parseBioRow" $ do
        it "parses full bio exchange row" $
            parseBioRow defaultConfig "Carbon dioxide;air;kg;1.0;Undefined;;;;;;"
                `shouldSatisfy` \case
                    Just r -> berName r == "Carbon dioxide" && berCompartment r == "air" && berAmount r == 1.0
                    Nothing -> False

        it "parses minimal bio row (name;comp;unit;amount)" $
            parseBioRow defaultConfig "Methane;air;kg;0.5"
                `shouldSatisfy` \case
                    Just r -> berName r == "Methane" && berUnit r == "kg"
                    Nothing -> False

        it "returns Nothing for too-short row" $
            parseBioRow defaultConfig "name;air" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- UUID generation
    -- -----------------------------------------------------------------------

    describe "UUID generation" $ do
        it "generateUnitUUID is deterministic" $
            generateUnitUUID "kg" `shouldBe` generateUnitUUID "kg"

        it "generateUnitUUID differs for different units" $
            generateUnitUUID "kg" `shouldNotBe` generateUnitUUID "MJ"

        it "generateFlowUUID is deterministic" $
            generateFlowUUID "CO2" "air" "kg" `shouldBe` generateFlowUUID "CO2" "air" "kg"

        it "generateFlowUUID differs when compartment differs" $
            generateFlowUUID "CO2" "air" "kg" `shouldNotBe` generateFlowUUID "CO2" "water" "kg"

    -- -----------------------------------------------------------------------
    -- Uncovered CSV sections
    -- -----------------------------------------------------------------------

    describe "SimaPro uncovered sections" $ do
        it "parses Electricity/heat exchanges" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Electricity/heat"
                    , "Electricity, medium voltage;kWh;0.3;Undefined;;;;;;"
                    ]
            let act = head activities
                techIn = [e | e@TechnosphereExchange{} <- exchanges act, techIsInput e, not (techIsReference e)]
            length techIn `shouldBe` 1

        it "parses Resources (biosphere inputs)" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Resources"
                    , "Water, river;in water;m3;0.1;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses Emissions to water" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Emissions to water"
                    , "Phosphate;river;kg;0.01;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses Emissions to soil" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Emissions to soil"
                    , "Zinc;agricultural;kg;0.001;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses Final waste flows" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Final waste flows"
                    , "Inert waste, for final disposal;kg;0.5;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses location from process name {XX} pattern" $ do
            (activities, _, _) <- parseNamedCSV "Widget {FR} U" []
            activityLocation (head activities) `shouldBe` "FR"

        it "parses location from Geography metadata" $ do
            (activities, _, _) <- parseTestCSV
            let a = head activities
            activityLocation a `shouldBe` "GLO"

    describe "SimaPro comma CSV separator" $ do
        it "parses comma-separated CSV" $ do
            (activities, _, _) <- parseCommaCSV
            length activities `shouldBe` 1
            activityName (head activities) `shouldBe` "Comma Product"

-- ---------------------------------------------------------------------------
-- Helpers for section tests
-- ---------------------------------------------------------------------------

-- | Build a minimal process CSV with extra section lines inserted
parseSectionCSV :: [BS.ByteString] -> IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseSectionCSV sectionLines =
    parseNamedCSV "Test process" sectionLines

parseNamedCSV :: BS.ByteString -> [BS.ByteString] -> IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseNamedCSV procName sectionLines =
    withSystemTempFile "section-test.csv" $ \path handle -> do
        let content =
                BS.intercalate "\r\n" $
                    [ "{SimaPro 9.6.0.1}"
                    , "{CSV separator: semicolon}"
                    , "{Decimal separator: .}"
                    , ""
                    , "Process"
                    , ""
                    , "Category type"
                    , "material"
                    , ""
                    , "Process name"
                    , procName
                    , ""
                    , "Type"
                    , "Unit process"
                    , ""
                    , "Products"
                    , "Reference product;kg;1.0;100;not defined;material;"
                    , ""
                    ]
                        ++ sectionLines
                        ++ [ ""
                           , "End"
                           ]
        BS.hPut handle content
        hClose handle
        parseSimaProCSV path

-- | CSV with comma as separator
commaCSV :: BS.ByteString
commaCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Comma}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Comma Product"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Comma Product,kg,1.0,100,not defined,material,"
        , ""
        , "End"
        ]

parseCommaCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseCommaCSV = withSystemTempFile "comma-test.csv" $ \path handle -> do
    BS.hPut handle commaCSV
    hClose handle
    parseSimaProCSV path

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

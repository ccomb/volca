{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the Brightway Excel (.xlsx) importer.

The fixture workbook is generated in-memory (zip of inline-string XML, the
shape openpyxl/bw2io emits) so no third-party inventory data is committed
into the open-source engine. It exercises: inline strings, sparse columns
addressed by cell reference, XML entity decoding, a reordered exchange
header, biosphere categories (with @::@), and within-file supplier linking.
-}
module BrightwayExcelSpec (spec) where

import BrightwayExcel.Parser (CellValue (..), Workbook (..), parseBrightwayExcel, parseSheetXml, sheetToActivities, skippedSheetWarning, splitCategories)
import BrightwayExcel.Writer (defaultWriterConfig, renderWorkbook)
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Database.Loader (loadDatabase)
import Database.Upload (ArchiveFormat (..), detectArchiveFormat)
import qualified Expr
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import TestWorkbook (Cell (..), buildWorkbook, sheetXml)
import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    Compartment (..),
    Exchange (..),
    ExchangeFormula (..),
    Medium (..),
    Parameter (..),
    ParameterDefinition (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    Uncertainty (..),
    exchangeFlowId,
    exchangeIsReference,
 )
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

spec :: Spec
spec = describe "BrightwayExcel.Parser" $ do
    describe "splitCategories" $ do
        it "splits a compartment::subcompartment pair" $
            splitCategories "air::urban air close to ground" `shouldBe` ("air", "urban air close to ground")
        it "treats a single level as compartment with no sub" $
            splitCategories "air" `shouldBe` ("air", "")

    describe "parseSheetXml" $ do
        it "decodes XML entities in inline strings" $ do
            let xml =
                    "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
                        <> "<sheetData><row r=\"1\">"
                        <> "<c r=\"A1\" t=\"inlineStr\"><is><t>furnace &gt;100kW &amp; up</t></is></c>"
                        <> "</row></sheetData></worksheet>"
            case parseSheetXml Nothing (TE.encodeUtf8 xml) of
                Right [row] -> M.lookup 0 row `shouldBe` Just (CellText "furnace >100kW & up")
                other -> expectationFailure ("unexpected parse: " <> show other)
        it "addresses cells by column reference, skipping gaps" $ do
            let xml =
                    "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
                        <> "<sheetData><row r=\"1\">"
                        <> "<c r=\"A1\" t=\"inlineStr\"><is><t>name</t></is></c>"
                        <> "<c r=\"C1\" t=\"n\"><v>0.5</v></c>"
                        <> "</row></sheetData></worksheet>"
            case parseSheetXml Nothing (TE.encodeUtf8 xml) of
                Right [row] -> do
                    M.lookup 0 row `shouldBe` Just (CellText "name")
                    M.lookup 1 row `shouldBe` Nothing
                    M.lookup 2 row `shouldBe` Just (CellNumber 0.5)
                other -> expectationFailure ("unexpected parse: " <> show other)

    describe "parseBrightwayExcel" $ do
        it "parses all activities across sheets" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts} ->
                    map activityName acts
                        `shouldMatchList` [ "Electricity production, natural gas"
                                          , "Widget manufacturing"
                                          , "Cotton fibre production"
                                          ]

        -- A metadata key fixes the whole activity, and two values for one key
        -- name no activity. A repeated column is the other case: it carries
        -- one field of one exchange row, and is reported rather than refused.
        it "refuses a block that states one metadata key two ways" $
            withWorkbook (buildWorkbook [("data", twiceStatedSheet)]) $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Right _ -> expectationFailure "Expected the load to stop on two values for one key"
                    Left err -> do
                        err `shouldSatisfy` T.isInfixOf "location"
                        err `shouldSatisfy` T.isInfixOf "GLO"
                        err `shouldSatisfy` T.isInfixOf "FR"

        it "reads a metadata key the block states twice with one value" $
            withWorkbook (buildWorkbook [("data", twiceAgreeingSheet)]) $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right Workbook{wbActivities = acts} -> map activityLocation acts `shouldBe` ["GLO"]

        it "keys the reference product by its product name" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts, wbTechFlows = techDB} -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    referenceProductName techDB elec `shouldBe` Just "electricity, high voltage"

        it "keys a technosphere input by the supplier reference product, preserving amount" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts, wbTechFlows = techDB} -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    let gas = listToMaybe [ex | ex <- inputExchanges elec, flowName techDB ex == Just "natural gas, high pressure"]
                    fmap techAmount gas `shouldBe` Just 8.5

        it "keeps the supplier activity the row names apart from its product" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts, wbTechFlows = techDB} -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    let gas = listToMaybe [ex | ex <- inputExchanges elec, flowName techDB ex == Just "natural gas, high pressure"]
                    fmap techSupplierClaim gas `shouldBe` Just (ClaimByName "natural gas, burned >100kW")

        it "reads a workbook as Brightway exports it, its lines naming supplier activities alone" $
            withWorkbook (buildWorkbook [("data", exportedSheet)]) $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right Workbook{wbActivities = acts, wbTechFlows = techDB} -> do
                        map activityName acts `shouldBe` ["Bakery", "Flour mill, copied"]
                        mill <- requireActivity acts "Flour mill, copied"
                        referenceProductName techDB mill `shouldBe` Just "wheat flour"
                        map techSupplierClaim (inputExchanges mill)
                            `shouldBe` [ClaimByNameAlone "market for wheat grain", ClaimByNameAlone "wheat flour"]
                        bakery <- requireActivity acts "Bakery"
                        referenceProductName techDB bakery `shouldBe` Just "bread"
                        [(flowName techDB ex, techSupplierClaim ex) | ex@TechnosphereExchange{} <- inputExchanges bakery]
                            `shouldBe` [ (Just "wheat flour", ClaimByName "Flour mill, copied")
                                       , (Just "market for electricity, low voltage", ClaimByNameAlone "market for electricity, low voltage")
                                       ]

        it "names no supplier activity on a production row" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts} -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    let refs = [techSupplierClaim ex | ex@TechnosphereExchange{} <- exchanges elec, exchangeIsReference ex]
                    refs `shouldBe` [ClaimByProduct]

        it "splits biosphere categories into compartment + sub and direction" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts, wbBioFlows = bioDB} -> do
                    co2 <- requireBioFlow bioDB "Carbon dioxide, fossil"
                    bfCompartment co2 `shouldBe` Just (Compartment Air Nothing)
                    water <- requireBioFlow bioDB "Water"
                    bfCompartment water `shouldBe` Just (Compartment NaturalResource (Just "in water"))
                    directionOf bioDB acts "Carbon dioxide, fossil" `shouldBe` Just Emission
                    directionOf bioDB acts "Water" `shouldBe` Just Resource

        it "reads a reordered exchange header by label" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right Workbook{wbActivities = acts, wbTechFlows = techDB} -> do
                    cotton <- requireActivity acts "Cotton fibre production"
                    referenceProductName techDB cotton `shouldBe` Just "cotton fibre"
                    let irrigation = listToMaybe [ex | ex <- inputExchanges cotton, flowName techDB ex == Just "water, irrigation"]
                    fmap techAmount irrigation `shouldBe` Just 3.2

    describe "a formula on an exchange row" $ do
        it "leaves the amount column to say the amount when it reads a name nothing defines, and says so" $ do
            (acts, warnings) <- readSheet (formulaSheet (CT "mass * 2") (CT "mass / 4"))
            filter (T.isInfixOf "formula") warnings
                `shouldBe` [ "activity 'Widget manufacturing', row 'steel': its formula mass * 2 is left aside (unknown variable mass); its amount column is read"
                           , "activity 'Widget manufacturing', row 'Carbon dioxide, fossil': its formula mass / 4 is left aside (unknown variable mass); its amount column is read"
                           ]
            widget <- requireActivity acts "Widget manufacturing"
            map techAmount (inputExchanges widget) `shouldBe` [1.0]
            map techFormula (inputExchanges widget) `shouldBe` [Nothing]

        it "says nothing when the formula column is there but empty" $ do
            (_, warnings) <- readSheet (formulaSheet CE CE)
            filter (T.isInfixOf "formula") warnings `shouldBe` []

    describe "a workbook with parameters" $ do
        it "keeps its project and database parameters, a database one replacing nothing it does not name" $
            withParametric $ \Workbook{wbParameters = parameters} -> do
                M.keys parameters `shouldBe` ["loss", "mass", "spread", "twice"]
                fmap parameterUnit (M.lookup "mass" parameters) `shouldBe` Just (Just "kilogram")
                fmap parameterDescription (M.lookup "mass" parameters) `shouldBe` Just "Mass of a widget"
                fmap parameterDefinition (M.lookup "loss" parameters) `shouldBe` Just (InputParameter 0.1 (Just (Uniform 0 0.2)))
                case parameterDefinition <$> M.lookup "twice" parameters of
                    Just (CalculatedParameter _ value) -> value `shouldBe` Just 4
                    other -> expectationFailure ("twice is not calculated: " <> show other)

        it "reads a lognormal's loc and scale as logarithms, and leaves out one whose loc is the amount itself" $
            withParametric $ \Workbook{wbParameters = parameters} -> do
                case parameterDefinition <$> M.lookup "mass" parameters of
                    Just (InputParameter 2 (Just (Lognormal mean deviation))) -> do
                        mean `shouldSatisfy` Expr.agrees 2
                        deviation `shouldSatisfy` Expr.agrees 1.2
                    other -> expectationFailure ("mass has no lognormal: " <> show other)
                fmap parameterDefinition (M.lookup "spread" parameters) `shouldBe` Just (InputParameter 3 Nothing)

        it "gives an activity the parameters of its group, and only those" $
            withParametric $ \Workbook{wbActivities = acts} -> do
                widget <- requireActivity acts "Widget manufacturing"
                M.toList (M.map parameterDefinition (activityParameters widget))
                    `shouldBe` [("share", InputParameter 0.5 (Just (Normal 0.5 0.1)))]

        it "reads a Python formula, sympy's spellings with it, as the amount its row is worth" $
            withParametric $ \Workbook{wbActivities = acts} -> do
                widget <- requireActivity acts "Widget manufacturing"
                let steel = [ex | ex@TechnosphereExchange{techRole = Input} <- exchanges widget, techAmount ex == 9]
                map (fmap efText . techFormula) steel `shouldBe` [Just "max(mass * 2 - 1; 0) ^ 2"]

        it "carries a formula into the reference unit of its row" $
            withParametric $ \Workbook{wbActivities = acts} -> do
                widget <- requireActivity acts "Widget manufacturing"
                case [ex | ex@TechnosphereExchange{techRole = Input} <- exchanges widget, techAmount ex /= 9] of
                    [plastic] -> do
                        techAmount plastic `shouldSatisfy` Expr.agrees 1.1e-3
                        case techFormula plastic of
                            Just ExchangeFormula{efText = text} ->
                                Expr.evaluate Expr.Volca (M.fromList [("mass", 2), ("loss", 0.1), ("share", 0.5)]) text
                                    `shouldSatisfy` either (const False) (Expr.agrees 1.1e-3)
                            Nothing -> expectationFailure "the plastic row keeps no formula"
                    other -> expectationFailure ("expected one plastic row, got " <> show (length other))

        it "takes the formula's value over the amount column, and keeps the column where the formula reads nothing defined" $
            withParametric $ \Workbook{wbActivities = acts} -> do
                widget <- requireActivity acts "Widget manufacturing"
                [(bioAmount ex, isJust (bioFormula ex)) | ex@BiosphereExchange{} <- exchanges widget]
                    `shouldBe` [(4, True), (0.5, False)]

        it "keeps an unnamed group to its activity, and says what in it gives no value" $ do
            (acts, warnings) <- readSheet ownGroupSheet
            widget <- requireActivity acts "Widget manufacturing"
            M.keys (activityParameters widget) `shouldBe` ["doubled", "halves", "share", "zero"]
            fmap parameterDefinition (M.lookup "halves" (activityParameters widget)) `shouldBe` Just (InputParameter 3 Nothing)
            fmap parameterDefinition (M.lookup "share" (activityParameters widget)) `shouldBe` Just (InputParameter 0.5 Nothing)
            map techAmount (inputExchanges widget) `shouldBe` [1, 1.5]
            map (isJust . techFormula) (inputExchanges widget) `shouldBe` [False, True]
            let about needle = filter (T.isInfixOf needle) warnings
            about "'share'" `shouldBe` ["activity 'Widget manufacturing' parameter 'share': its uncertainty type 9 is left out, the engine has no such distribution"]
            map (T.isSuffixOf "; its amount is read") (about "'halves'") `shouldBe` [True]
            about "'doubled'" `shouldBe` ["activity 'Widget manufacturing' parameter 'doubled': its formula widht * 2 gives no value (unknown variable widht)"]
            map (T.isInfixOf "no finite number") (about "row 'steel'") `shouldBe` [True]

        it "says where a formula and the amount column disagree, and which formula is left aside" $ do
            (_, warnings) <- readSheet parametricSheet
            filter (T.isInfixOf "formula") warnings
                `shouldBe` [ "activity 'Widget manufacturing', row 'Carbon dioxide, fossil': its formula gives 4.0 where its amount column says 5.0; the formula's value is read"
                           , "activity 'Widget manufacturing', row 'Water': its formula volume * 2 is left aside (unknown variable volume); its amount column is read"
                           ]

    describe "loadDatabase dispatch" $ do
        it "resolves within-file technosphere links" $
            withFixture $ \path -> do
                loadDatabase defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right db -> do
                        let links =
                                concatMap (mapMaybe inputLink . exchanges) (M.elems (sdbActivities db))
                        -- Widget consumes the file's own "electricity, high voltage";
                        -- the name-based pass must resolve that supplier.
                        any (/= UUID.nil) links `shouldBe` True

        it "keeps a product the workbook makes apart from one another database sells under its name" $
            withWorkbook (buildWorkbook [("iron", madeElsewhereSheet)]) $ \path -> do
                loadDatabase defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right db -> do
                        let acts = M.elems (sdbActivities db)
                        coke <- requireActivity acts "Coke production"
                        iron <- requireActivity acts "Pig iron production"
                        [cokeProduct] <- pure (filter exchangeIsReference (exchanges coke))
                        [madeHere, boughtElsewhere] <- pure (inputExchanges iron)
                        -- The kilograms come from the workbook's own activity; the
                        -- megajoules are another flow, left for a dependency to supply.
                        exchangeFlowId madeHere `shouldBe` exchangeFlowId cokeProduct
                        inputLink madeHere `shouldSatisfy` isJust
                        exchangeFlowId boughtElsewhere `shouldNotBe` exchangeFlowId cokeProduct
                        inputLink boughtElsewhere `shouldBe` Nothing

        it "refuses to export the two products a workbook would read back as one" $
            withWorkbook (buildWorkbook [("iron", madeElsewhereSheet)]) $ \path -> do
                loadDatabase defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right db -> case renderWorkbook defaultWriterConfig db of
                        Right _ -> expectationFailure "exported a workbook the reader would refuse"
                        Left err -> do
                            err `shouldSatisfy` T.isInfixOf "\"Coke\" (kilogram)"
                            err `shouldSatisfy` T.isInfixOf "\"coke\" (megajoule)"

    describe "upload format detection" $
        it "routes a Brightway .xlsx to ArchiveXlsx, not generic ArchiveZip" $
            -- An .xlsx is a PK zip; without this the upload extractor would unzip
            -- it and detectDatabaseFormat would fall through to UnknownFormat.
            detectArchiveFormat fixtureBytes `shouldBe` ArchiveXlsx

    describe "multi-sheet workbooks" $ do
        it "imports one activity per sheet across many sheets" $ do
            let bytes =
                    buildWorkbook
                        [ ("alpha", activitySheet "Alpha activity" "alpha product")
                        , ("beta", activitySheet "Beta activity" "beta product")
                        , ("gamma", activitySheet "Gamma activity" "gamma product")
                        ]
            withWorkbook bytes $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right Workbook{wbActivities = acts} ->
                        map activityName acts
                            `shouldMatchList` ["Alpha activity", "Beta activity", "Gamma activity"]

        it "skips a sheet whose first cell (A1) is blank, keeping the valid ones" $ do
            let bytes =
                    buildWorkbook
                        [ ("kept", activitySheet "Kept activity" "kept product")
                        , -- a leading blank row makes A1 empty, so bw2io ignores the sheet
                          ("blankA1", [] : activitySheet "Dropped activity" "dropped product")
                        ]
            withWorkbook bytes $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right Workbook{wbActivities = acts} ->
                        map activityName acts `shouldMatchList` ["Kept activity"]

    describe "skippedSheetWarning" $ do
        it "flags a sheet that holds data but whose A1 is blank" $
            skippedSheetWarning ("Inventory", [M.empty, M.singleton 0 (CellText "Activity")])
                `shouldSatisfy` isJust
        it "stays silent on the deliberate 'skip' sentinel" $
            skippedSheetWarning ("Notes", [M.singleton 0 (CellText "skip")]) `shouldBe` Nothing
        it "stays silent on an entirely empty sheet" $
            skippedSheetWarning ("Empty", [M.empty]) `shouldBe` Nothing

-- ---------------------------------------------------------------------------
-- Assertions helpers
-- ---------------------------------------------------------------------------

requireActivity :: [Activity] -> Text -> IO Activity
requireActivity acts name =
    maybe (fail ("missing activity: " <> T.unpack name)) pure (find ((== name) . activityName) acts)

requireBioFlow :: M.Map UUID.UUID BiosphereFlow -> Text -> IO BiosphereFlow
requireBioFlow bioDB name =
    maybe (fail ("missing biosphere flow: " <> T.unpack name)) pure (find ((== name) . bfName) (M.elems bioDB))

referenceProductName :: M.Map UUID.UUID TechnosphereFlow -> Activity -> Maybe Text
referenceProductName techDB act =
    listToMaybe
        [ tfName f
        | ex <- exchanges act
        , exchangeIsReference ex
        , Just f <- [M.lookup (exchangeFlowId ex) techDB]
        ]

flowName :: M.Map UUID.UUID TechnosphereFlow -> Exchange -> Maybe Text
flowName techDB ex = tfName <$> M.lookup (exchangeFlowId ex) techDB

inputExchanges :: Activity -> [Exchange]
inputExchanges = filter isInput . exchanges
  where
    isInput e = case e of
        TechnosphereExchange{techRole = Input} -> True
        TechnosphereExchange{} -> False
        BiosphereExchange{} -> False
        WasteExchange{} -> False

inputLink :: Exchange -> Maybe UUID.UUID
inputLink e = case e of
    TechnosphereExchange{techRole = Input, techActivityLinkId = Just l} -> Just l
    TechnosphereExchange{} -> Nothing
    BiosphereExchange{} -> Nothing
    WasteExchange{} -> Nothing

directionOf :: M.Map UUID.UUID BiosphereFlow -> [Activity] -> Text -> Maybe BioDirection
directionOf bioDB acts name =
    listToMaybe
        [ d
        | ex <- concatMap exchanges acts
        , Just (fid, d) <- [bioDir ex]
        , Just f <- [M.lookup fid bioDB]
        , bfName f == name
        ]
  where
    bioDir e = case e of
        BiosphereExchange{bioFlowId = fid, bioDirection = d} -> Just (fid, d)
        TechnosphereExchange{} -> Nothing
        WasteExchange{} -> Nothing

-- ---------------------------------------------------------------------------
-- Fixture workbook (generated in-memory)
-- ---------------------------------------------------------------------------

-- | The activities one worksheet reads to, with every warning they raised.
readSheet :: [[Cell]] -> IO ([Activity], [Text])
readSheet cells = case parseSheetXml Nothing (TE.encodeUtf8 (sheetXml cells)) of
    Left err -> fail (T.unpack err)
    Right rows -> do
        let outs = sheetToActivities defaultUnitConfig rows
        pure ([a | (a, _, _, _, _) <- outs], concat [ws | (_, _, _, _, ws) <- outs])

withWorkbook :: BL.ByteString -> (FilePath -> IO a) -> IO a
withWorkbook bytes action =
    withSystemTempFile "brightway-fixture.xlsx" $ \path h -> do
        BL.hPut h bytes
        hClose h
        action path

withFixture :: (FilePath -> IO a) -> IO a
withFixture = withWorkbook fixtureBytes

fixtureBytes :: BL.ByteString
fixtureBytes = buildWorkbook [("data", sheet1Rows), ("reordered", sheet2Rows)]

{- | A worksheet whose block states one metadata key twice, with two values.
A workbook can do that, and nothing in it says which value was meant.
-}
twiceStatedSheet :: [[Cell]]
twiceStatedSheet =
    [ [CT "Activity", CT "Widget manufacturing"]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT "widget"]
    , [CT "location", CT "GLO"]
    , [CT "location", CT "FR"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "database"]
    , [CT "Widget manufacturing", CN 1, CT "widget", CT "GLO", CT "kilogram", CE, CT "production", CT "DB"]
    ]

-- | The same block, stating its repeated key the same way both times.
twiceAgreeingSheet :: [[Cell]]
twiceAgreeingSheet = map keepGLO twiceStatedSheet
  where
    keepGLO :: [Cell] -> [Cell]
    keepGLO [CT "location", CT _] = [CT "location", CT "GLO"]
    keepGLO row = row

{- | Two activities as Brightway's export writes them: no @reference product@
and no @database@ column on the lines, each line named by its supplier
activity. The flour mill is copied from an EcoSpold 2 import, so it keeps an
@activity@ field, and makes its product with no production line; it buys from
another database an activity named as its own product.
-}
exportedSheet :: [[Cell]]
exportedSheet =
    [ [CT "Database", CT "fg"]
    , []
    , [CT "Activity", CT "Bakery"]
    , [CT "location", CT "GLO"]
    , [CT "reference product", CT "bread"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "location", CT "unit", CT "type"]
    , [CT "Bakery", CN 1, CT "GLO", CT "kilogram", CT "production"]
    , [CT "Flour mill, copied", CN 0.8, CT "GLO", CT "kilogram", CT "technosphere"]
    , [CT "market for electricity, low voltage", CN 0.2, CT "FR", CT "kilowatt hour", CT "technosphere"]
    , []
    , [CT "Activity", CT "Flour mill, copied"]
    , [CT "activity", CT "0b6a3f8e-2c1d-4e5f-9a7b-8c9d0e1f2a3b"]
    , [CT "location", CT "GLO"]
    , [CT "reference product", CT "wheat flour"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "location", CT "unit", CT "type"]
    , [CT "market for wheat grain", CN 1.2, CT "GLO", CT "kilogram", CT "technosphere"]
    , [CT "wheat flour", CN 0.1, CT "GLO", CT "kilogram", CT "technosphere"]
    ]

{- | A block whose exchange table has a @formula@ column, filled on its
technosphere and biosphere rows with the two cells given.
-}
formulaSheet :: Cell -> Cell -> [[Cell]]
formulaSheet inputFormula emissionFormula =
    [ [CT "Activity", CT "Widget manufacturing"]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT "widget"]
    , [CT "location", CT "GLO"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "formula"]
    , [CT "Widget manufacturing", CN 1, CT "widget", CT "GLO", CT "kilogram", CE, CT "production", CE]
    , [CT "Steel production", CN 1, CT "steel", CT "GLO", CT "kilogram", CE, CT "technosphere", inputFormula]
    , [CT "Carbon dioxide, fossil", CN 0.5, CE, CT "GLO", CT "kilogram", CT "air", CT "biosphere", emissionFormula]
    ]

-- | Read 'parametricSheet' as a whole workbook.
withParametric :: (Workbook -> IO ()) -> IO ()
withParametric check =
    withWorkbook (buildWorkbook [("shop", parametricSheet)]) $
        parseBrightwayExcel gramConfig >=> either (expectationFailure . T.unpack) check

-- | Grams and kilograms, so a row in grams is read in kilograms.
gramConfig :: UnitConfig
gramConfig = either (error . T.unpack) id (buildFromCSV "name,dimension,factor\nkg,mass,1.0\ng,mass,1.0e-3\n")

{- | A workbook laid out as Brightway writes one with parameters at each of its
three levels: the project's, the database's, and a group's, which one activity
names. Its exchange formulas are Python, the last reading a name nothing defines.
-}
parametricSheet :: [[Cell]]
parametricSheet =
    [ [CT "Project parameters"]
    , [CT "name", CT "amount", CT "uncertainty type", CT "loc", CT "scale", CT "minimum", CT "maximum", CT "description", CT "unit"]
    , [CT "mass", CN 2, CN 2, CN (log 2), CN (log 1.2), CE, CE, CT "Mass of a widget", CT "kilogram"]
    , [CT "loss", CN 0.1, CN 4, CE, CE, CN 0, CN 0.2]
    , []
    , [CT "Database", CT "shop"]
    , []
    , [CT "Database parameters"]
    , [CT "name", CT "amount", CT "formula", CT "uncertainty type", CT "loc", CT "scale", CT "database"]
    , [CT "twice", CN 4, CT "mass * 2 if mass > 1 else 0", CE, CE, CE, CT "shop"]
    , [CT "spread", CN 3, CE, CN 2, CN 3, CN 0.1, CT "shop"]
    , []
    , [CT "Activity", CT "Widget manufacturing"]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT "widget"]
    , [CT "location", CT "GLO"]
    , [CT "unit", CT "kilogram"]
    , [CT "Parameters", CT "widget group"]
    , [CT "name", CT "amount", CT "uncertainty type", CT "loc", CT "scale", CT "code", CT "database"]
    , [CT "share", CN 0.5, CN 3, CN 0.5, CN 0.1, CT "widget", CT "shop"]
    , []
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "formula"]
    , [CT "Widget manufacturing", CN 1, CT "widget", CT "GLO", CT "kilogram", CE, CT "production", CE]
    , [CT "Steel production", CN 0, CT "steel", CT "GLO", CT "kilogram", CE, CT "technosphere", CT "Max(mass * 2 - 1, 0) ** 2"]
    , [CT "Plastic production", CN 0, CT "plastic", CT "GLO", CT "g", CE, CT "technosphere", CT "mass * (1 + loss) * share"]
    , [CT "Carbon dioxide, fossil", CN 5, CE, CT "GLO", CT "kilogram", CT "air", CT "biosphere", CT "twice"]
    , [CT "Water", CN 0.5, CE, CT "GLO", CT "cubic meter", CT "natural resource::in water", CT "biosphere", CT "volume * 2"]
    ]

{- | An activity whose @Parameters@ section names no group, so they are its
own. One parameter states a distribution the engine has no shape for, one a
formula Python's floor division writes, one a formula reading a misspelt name;
a line divides by a parameter worth 0.
-}
ownGroupSheet :: [[Cell]]
ownGroupSheet =
    [ [CT "Activity", CT "Widget manufacturing"]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT "widget"]
    , [CT "location", CT "GLO"]
    , [CT "unit", CT "kilogram"]
    , [CT "Parameters"]
    , [CT "name", CT "amount", CT "formula", CT "uncertainty type"]
    , [CT "share", CN 0.5, CE, CN 9]
    , [CT "halves", CN 3, CT "7 // 2"]
    , [CT "doubled", CE, CT "widht * 2"]
    , [CT "zero", CN 0]
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "formula"]
    , [CT "Widget manufacturing", CN 1, CT "widget", CT "GLO", CT "kilogram", CE, CT "production", CE]
    , [CT "Steel production", CN 1, CT "steel", CT "GLO", CT "kilogram", CE, CT "technosphere", CT "1 / zero"]
    , [CT "Plastic production", CN 0, CT "plastic", CT "GLO", CT "kilogram", CE, CT "technosphere", CT "halves * share"]
    ]

{- | A workbook whose own coke, weighed in kilograms, is spelt like the coke
another database sells in megajoules. It has no @Database@ section, so its own
database is the one its production rows name.
-}
madeElsewhereSheet :: [[Cell]]
madeElsewhereSheet =
    [ [CT "Activity", CT "Coke production"]
    , [CT "reference product", CT "Coke"]
    , [CT "location", CT "CN"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , header
    , [CT "Coke production", CN 1, CT "Coke", CT "CN", CT "kilogram", CE, CT "production", CT "Foreground"]
    , []
    , [CT "Activity", CT "Pig iron production"]
    , [CT "reference product", CT "pig iron"]
    , [CT "location", CT "CN"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , header
    , [CT "Pig iron production", CN 1, CT "pig iron", CT "CN", CT "kilogram", CE, CT "production", CT "Foreground"]
    , [CT "Coke production", CN 0.4, CT "Coke", CT "CN", CT "kilogram", CE, CT "technosphere", CT "Foreground"]
    , [CT "market for coke", CN 0.7, CT "coke", CT "CN", CT "megajoule", CE, CT "technosphere", CT "Background"]
    ]
  where
    header = [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "database"]

-- | A single-activity worksheet (standard column order) for multi-sheet tests.
activitySheet :: Text -> Text -> [[Cell]]
activitySheet actName prodName =
    [ [CT "Activity", CT actName]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT prodName]
    , [CT "location", CT "GLO"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "database"]
    , [CT actName, CN 1, CT prodName, CT "GLO", CT "kilogram", CE, CT "production", CT "DB"]
    ]

{- | Sheet 1: standard column order, two activities. Widget consumes the file's
own electricity product, so its supplier link must resolve within the file.
-}
sheet1Rows :: [[Cell]]
sheet1Rows =
    [ [CT "Database", CT "Test_Inventory_DB"]
    , []
    , [CT "Activity", CT "Electricity production, natural gas"]
    , [CT "production amount", CN 1]
    , [CT "comment"]
    , [CT "reference product", CT "electricity, high voltage"]
    , [CT "location", CT "GLO"]
    , [CT "unit", CT "kilowatt hour"]
    , [CT "Exchanges"]
    , excHeader
    , [CT "Electricity production, natural gas", CN 1, CT "electricity, high voltage", CT "GLO", CT "kilowatt hour", CE, CT "production", CT "Test_Inventory_DB"]
    , [CT "natural gas, burned >100kW", CN 8.5, CT "natural gas, high pressure", CT "RoW", CT "cubic meter", CE, CT "technosphere", CT "ecoinvent-3.12-cutoff"]
    , [CT "Carbon dioxide, fossil", CN 0.5, CE, CT "GLO", CT "kilogram", CT "air", CT "biosphere", CT "ecoinvent-3.12-biosphere"]
    , []
    , [CT "Activity", CT "Widget manufacturing"]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT "widget"]
    , [CT "location", CT "GLO"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , excHeader
    , [CT "Widget manufacturing", CN 1, CT "widget", CT "GLO", CT "kilogram", CE, CT "production", CT "Test_Inventory_DB"]
    , [CT "Electricity production, natural gas", CN 2.5, CT "electricity, high voltage", CT "GLO", CT "kilowatt hour", CE, CT "technosphere", CT "Test_Inventory_DB"]
    , [CT "Water", CN 1.0e-3, CE, CT "GLO", CT "cubic meter", CT "natural resource::in water", CT "biosphere", CT "ecoinvent-3.12-biosphere"]
    ]
  where
    excHeader =
        [CT "name", CT "amount", CT "reference product", CT "location", CT "unit", CT "categories", CT "type", CT "database"]

{- | Sheet 2: a reordered exchange header (type/unit/amount first), starting with
an Activity row (no Database section), to exercise label-based column lookup
and multi-sheet handling.
-}
sheet2Rows :: [[Cell]]
sheet2Rows =
    [ [CT "Activity", CT "Cotton fibre production"]
    , [CT "production amount", CN 1]
    , [CT "reference product", CT "cotton fibre"]
    , [CT "location", CT "IN"]
    , [CT "unit", CT "kilogram"]
    , [CT "Exchanges"]
    , [CT "type", CT "unit", CT "amount", CT "reference product", CT "name", CT "location", CT "categories", CT "database"]
    , [CT "production", CT "kilogram", CN 1, CT "cotton fibre", CT "Cotton fibre production", CT "IN", CE, CT "Reordered_DB"]
    , [CT "technosphere", CT "cubic meter", CN 3.2, CT "water, irrigation", CT "market for irrigation", CT "IN", CE, CT "ecoinvent-3.12-cutoff"]
    ]

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

import BrightwayExcel.Parser (CellValue (..), parseBrightwayExcel, parseSheetXml, skippedSheetWarning, splitCategories)
import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Database.Loader (loadDatabase)
import Database.Upload (ArchiveFormat (..), detectArchiveFormat)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    Compartment (..),
    Exchange (..),
    Medium (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    exchangeFlowId,
    exchangeIsReference,
 )
import UnitConversion (defaultUnitConfig)

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
                Right (acts, _, _, _, _) ->
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
                    Right (acts, _, _, _, _) -> map activityLocation acts `shouldBe` ["GLO"]

        it "keys the reference product by its product name" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right (acts, techDB, _, _, _) -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    referenceProductName techDB elec `shouldBe` Just "electricity, high voltage"

        it "keys a technosphere input by the supplier reference product, preserving amount" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right (acts, techDB, _, _, _) -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    let gas = listToMaybe [ex | ex <- inputExchanges elec, flowName techDB ex == Just "natural gas, high pressure"]
                    fmap techAmount gas `shouldBe` Just 8.5

        it "keeps the supplier activity the row names apart from its product" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right (acts, techDB, _, _, _) -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    let gas = listToMaybe [ex | ex <- inputExchanges elec, flowName techDB ex == Just "natural gas, high pressure"]
                    fmap techSupplierClaim gas `shouldBe` Just (ClaimByName "natural gas, burned >100kW")

        it "names no supplier activity on a production row" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right (acts, _, _, _, _) -> do
                    elec <- requireActivity acts "Electricity production, natural gas"
                    let refs = [techSupplierClaim ex | ex@TechnosphereExchange{} <- exchanges elec, exchangeIsReference ex]
                    refs `shouldBe` [ClaimByProduct]

        it "splits biosphere categories into compartment + sub and direction" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right (acts, _, bioDB, _, _) -> do
                    co2 <- requireBioFlow bioDB "Carbon dioxide, fossil"
                    bfCompartment co2 `shouldBe` Just (Compartment Air Nothing)
                    water <- requireBioFlow bioDB "Water"
                    bfCompartment water `shouldBe` Just (Compartment NaturalResource (Just "in water"))
                    directionOf bioDB acts "Carbon dioxide, fossil" `shouldBe` Just Emission
                    directionOf bioDB acts "Water" `shouldBe` Just Resource

        it "reads a reordered exchange header by label" $ withFixture $ \path -> do
            parseBrightwayExcel defaultUnitConfig path >>= \case
                Left err -> expectationFailure (T.unpack err)
                Right (acts, techDB, _, _, _) -> do
                    cotton <- requireActivity acts "Cotton fibre production"
                    referenceProductName techDB cotton `shouldBe` Just "cotton fibre"
                    let irrigation = listToMaybe [ex | ex <- inputExchanges cotton, flowName techDB ex == Just "water, irrigation"]
                    fmap techAmount irrigation `shouldBe` Just 3.2

    describe "loadDatabase dispatch" $
        it "resolves within-file technosphere links" $
            withFixture $ \path -> do
                loadDatabase defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right db -> do
                        let links =
                                mapMaybe
                                    inputLink
                                    (concatMap exchanges (M.elems (sdbActivities db)))
                        -- Widget consumes the file's own "electricity, high voltage";
                        -- the name-based pass must resolve that supplier.
                        any (/= UUID.nil) links `shouldBe` True

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
                    Right (acts, _, _, _, _) ->
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
                    Right (acts, _, _, _, _) ->
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

{- | Assemble a minimal @.xlsx@ from named sheets, wiring the workbook/rels parts
so that worksheet N lives at @xl/worksheets/sheetN.xml@.
-}
buildWorkbook :: [(Text, [[Cell]])] -> BL.ByteString
buildWorkbook sheets =
    fromArchive $
        foldr addEntryToArchive emptyArchive $
            toEntry "xl/workbook.xml" 0 (enc (workbookXml (map fst sheets)))
                : toEntry "xl/_rels/workbook.xml.rels" 0 (enc (relsXml (length sheets)))
                : [ toEntry ("xl/worksheets/sheet" <> show i <> ".xml") 0 (enc (sheetXml rows))
                  | (i, (_, rows)) <- zip [1 :: Int ..] sheets
                  ]
  where
    enc = BL.fromStrict . TE.encodeUtf8

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

workbookXml :: [Text] -> Text
workbookXml names =
    "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\""
        <> " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
        <> "<sheets>"
        <> T.concat
            [ "<sheet name=\"" <> nm <> "\" sheetId=\"" <> tshow i <> "\" r:id=\"rId" <> tshow i <> "\"/>"
            | (i, nm) <- zip [1 :: Int ..] names
            ]
        <> "</sheets></workbook>"

relsXml :: Int -> Text
relsXml n =
    "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> T.concat [rel i | i <- [1 .. n]]
        <> "</Relationships>"
  where
    rel i =
        "<Relationship Id=\"rId"
            <> tshow i
            <> "\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\""
            <> " Target=\"worksheets/sheet"
            <> tshow i
            <> ".xml\"/>"

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

-- ---------------------------------------------------------------------------
-- Minimal SpreadsheetML emitter
-- ---------------------------------------------------------------------------

data Cell = CT Text | CN Double | CE

sheetXml :: [[Cell]] -> Text
sheetXml rows =
    "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>"
        <> T.concat [rowXml n r | (n, r) <- zip [1 ..] rows]
        <> "</sheetData></worksheet>"

rowXml :: Int -> [Cell] -> Text
rowXml n cells =
    "<row r=\""
        <> tshow n
        <> "\">"
        <> T.concat [cellXml c n cell | (c, cell) <- zip [0 ..] cells]
        <> "</row>"

cellXml :: Int -> Int -> Cell -> Text
cellXml _ _ CE = ""
cellXml col n (CN d) =
    "<c r=\"" <> cellRef col n <> "\" t=\"n\"><v>" <> tshow d <> "</v></c>"
cellXml col n (CT t) =
    "<c r=\"" <> cellRef col n <> "\" t=\"inlineStr\"><is><t>" <> escapeXml t <> "</t></is></c>"

cellRef :: Int -> Int -> Text
cellRef col n = T.singleton (chr (ord 'A' + col)) <> tshow n

escapeXml :: Text -> Text
escapeXml = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

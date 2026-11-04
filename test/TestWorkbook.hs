{-# LANGUAGE OverloadedStrings #-}

-- | A minimal @.xlsx@ written by hand, for tests that read workbooks.
module TestWorkbook (
    Cell (..),
    buildWorkbook,
    sheetXml,
) where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

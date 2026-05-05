{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for CSV-based LCIA method files.

A single CSV file encodes multiple impact categories as columns.
Supports two header layouts (both after optional @# methodology:@ comment):

__2-row__ (category defaults to name):

> ;;global warming (GWP100);acidification;...      ← names
> ;;kg CO2 eq.;kg SO2 eq.;...                      ← units
> substance;compartment;;;...                       ← label row

__3-row__ (explicit categories):

> ;;Climate change;Acidification;...                ← categories
> ;;global warming (GWP100);acidification;...       ← names
> ;;kg CO2 eq.;kg SO2 eq.;...                       ← units
> substance;compartment;;;...                        ← label row

One CSV file → multiple 'Method' values (one per column).
-}
module Method.ParserCSV (
    parseMethodCSVBytes,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (zip4)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID5
import Data.Word (Word8)

import Method.Types

-- | Pure parser, exported for testing.
parseMethodCSVBytes :: BS.ByteString -> Either String [Method]
parseMethodCSVBytes bytes =
    let allLines = map stripCR $ BC.lines (stripBOM bytes)
        (comments, dataLines) = span (\l -> BC.isPrefixOf "#" l || BS.null l) allLines
        methodology = extractMethodology comments
     in case splitHeaderFromData dataLines of
            Nothing -> Left "CSV method file needs at least 3 header rows (names, units, column labels)"
            Just (cats, names, units, rows, delim) ->
                Right
                    [ mkMethod methodology catName impactName unit colIdx rows delim
                    | (colIdx, catName, impactName, unit) <- zip4 [0 ..] cats names units
                    , not (T.null impactName)
                    ]

{- | Find the label row (first column starts with "substance") and split header
rows from data rows.  Returns (categories, names, units, dataRows, delimiter).
2-row layout → categories = names.  3-row layout → distinct.
-}
splitHeaderFromData :: [BS.ByteString] -> Maybe ([Text], [Text], [Text], [BS.ByteString], Word8)
splitHeaderFromData dataLines =
    case break isLabelRow dataLines of
        (headerRows, _labelRow : rows) ->
            let nonBlank = filter (not . BS.null) headerRows
             in case nonBlank of
                    [nameRow, unitRow] ->
                        let !delim = detectDelimiter nameRow
                            names = drop 2 $ splitRow delim nameRow
                            units = drop 2 $ splitRow delim unitRow
                         in Just (names, names, units, rows, delim) -- categories = names
                    [catRow, nameRow, unitRow] ->
                        let !delim = detectDelimiter catRow
                            cats = drop 2 $ splitRow delim catRow
                            names = drop 2 $ splitRow delim nameRow
                            units = drop 2 $ splitRow delim unitRow
                         in Just (cats, names, units, rows, delim)
                    _ -> Nothing
        _ -> Nothing

-- | Detect the label row: first cell is a variation of "substance".
isLabelRow :: BS.ByteString -> Bool
isLabelRow line =
    let first = T.toCaseFold . T.strip . head' . splitRow (detectDelimiter line) $ line
     in first == "substance"
  where
    head' (x : _) = x
    head' [] = ""

-- | Build a single Method from one column of the CSV.
mkMethod :: Maybe Text -> Text -> Text -> Text -> Int -> [BS.ByteString] -> Word8 -> Method
mkMethod methodology catName impactName unit colIdx rows delim =
    let !ns = csvMethodNamespace
        !mId = UUID5.generateNamed ns (bsKey $ "method:" <> impactName)
        !factors =
            [ MethodCF
                { mcfFlowRef = UUID5.generateNamed ns (bsKey $ sub <> "::" <> comp)
                , mcfFlowName = sub
                , mcfDirection = directionFromCompartment comp
                , mcfValue = v
                , mcfCompartment = parseCSVCompartment comp
                , mcfCAS = Nothing
                , mcfUnit = unit
                , mcfConsumerLocation = Nothing
                }
            | row <- rows
            , let cells = splitRow delim row
            , length cells > colIdx + 2
            , let !sub = T.strip (cells !! 0)
                  !comp = T.strip (cells !! 1)
                  !raw = T.strip (cells !! (colIdx + 2))
            , not (T.null sub)
            , not (T.null raw)
            , Just v <- [parseDouble raw]
            ]
     in Method
            { methodId = mId
            , methodName = impactName
            , methodDescription = Nothing
            , methodUnit = if T.null unit then "unknown" else unit
            , methodCategory = catName
            , methodMethodology = methodology
            , methodFactors = factors
            }

-- | Deterministic namespace for CSV-derived UUIDs.
csvMethodNamespace :: UUID
csvMethodNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "volca:csv-method")

-- | Direction: "resources" → Input, everything else → Output.
directionFromCompartment :: Text -> FlowDirection
directionFromCompartment comp
    | T.toCaseFold comp == "resources" = Input
    | otherwise = Output

{- | Parse compartment from CSV compartment column.
Format: "Emissions to air", "Emissions to fresh water", "Resources", etc.
-}
parseCSVCompartment :: Text -> Maybe Compartment
parseCSVCompartment comp
    | T.null comp = Nothing
    | "air" `T.isInfixOf` lc = Just (Compartment "air" "" "")
    | "water" `T.isInfixOf` lc = Just (Compartment "water" "" "")
    | "soil" `T.isInfixOf` lc = Just (Compartment "soil" "" "")
    | "resource" `T.isInfixOf` lc = Just (Compartment "natural resource" "" "")
    | otherwise = Nothing
  where
    lc = T.toCaseFold comp

-- | Auto-detect delimiter: semicolon if the line contains ';', else comma.
detectDelimiter :: BS.ByteString -> Word8
detectDelimiter line
    | BC.elem ';' line = 0x3B -- ';'
    | otherwise = 0x2C -- ','

-- | Split a row on the delimiter, decoding to Text.
splitRow :: Word8 -> BS.ByteString -> [Text]
splitRow delim = map (TE.decodeUtf8With TEE.lenientDecode) . BS.split delim

-- | Extract methodology from "# methodology: ..." comment.
extractMethodology :: [BS.ByteString] -> Maybe Text
extractMethodology = go
  where
    go [] = Nothing
    go (l : ls)
        | BC.isPrefixOf "# methodology:" l =
            Just . T.strip . TE.decodeUtf8With TEE.lenientDecode $ BS.drop 15 l
        | otherwise = go ls

-- | Strict double parse, Nothing on failure.
parseDouble :: Text -> Maybe Double
parseDouble t = case TR.double t of
    Right (v, _) -> Just v
    Left _ -> Nothing

-- | Strip trailing carriage return (Windows CRLF line endings).
stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
    | not (BS.null bs) && BS.last bs == 0x0D = BS.init bs
    | otherwise = bs

-- | Strip UTF-8 BOM if present (common in Windows-created CSV files).
stripBOM :: BS.ByteString -> BS.ByteString
stripBOM bs
    | BS.isPrefixOf "\xEF\xBB\xBF" bs = BS.drop 3 bs
    | otherwise = bs

-- | Convert Text to bytes for UUID5 key generation.
bsKey :: Text -> [Word8]
bsKey = BS.unpack . TE.encodeUtf8

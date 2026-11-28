{-# LANGUAGE OverloadedStrings #-}

{- | Shared CSV plumbing for the LCIA method parsers and the columnar writer.

Delimiter detection, row splitting and number parsing were previously
duplicated across the CSV-based method parsers – and had drifted apart
(delimiter detection disagreed on tab support). They live here as the
single source of truth. 'splitRow' and 'joinRow' are inverses: what one
writes with RFC 4180 quoting, the other reads back cell for cell.
-}
module Method.CSV (
    detectDelimiter,
    splitRow,
    quotedCells,
    joinRow,
    parseDouble,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V

import Amount (readAmount)

-- | Auto-detect a row delimiter: semicolon, then tab, then comma.
detectDelimiter :: BS.ByteString -> Char
detectDelimiter line
    | BC.elem ';' line = ';'
    | BC.elem '\t' line = '\t'
    | otherwise = ','

{- | Split a row on the delimiter, respecting RFC 4180 quoted fields, decoding
each cell to 'Text' leniently. A row cassava cannot make sense of (an
unbalanced quote) degrades to the naive split – the historical behavior –
rather than dropping the line. A lone trailing CR is line-terminator residue
(see @SimaPro.Parser.splitCSV@ for the full story): strip it before parsing,
cassava would otherwise wait for the LF of a CRLF and reject the row.
-}
splitRow :: Char -> BS.ByteString -> [Text]
splitRow delim bs = fromMaybe (decodeCells (BC.split delim (stripTrailingCR bs))) (quotedCells delim bs)

{- | The cells of a row read as RFC 4180, or 'Nothing' when the quoting
cannot read it or the line is empty. The reading 'splitRow' prefers, for a
reader that must refuse the row rather than split it naively. A quote left
open at the end of the line is read to the end, not refused.
-}
quotedCells :: Char -> BS.ByteString -> Maybe [Text]
quotedCells delim bs =
    case Csv.decodeWith opts Csv.NoHeader (BL.fromStrict (stripTrailingCR bs)) of
        Right rows | Just (row, _) <- V.uncons rows -> Just (decodeCells (V.toList row))
        _ -> Nothing
  where
    opts :: Csv.DecodeOptions
    opts = Csv.defaultDecodeOptions{Csv.decDelimiter = fromIntegral (fromEnum delim)}

stripTrailingCR :: BS.ByteString -> BS.ByteString
stripTrailingCR = BC.dropWhileEnd (== '\r')

decodeCells :: [BS.ByteString] -> [Text]
decodeCells = map (TE.decodeUtf8With TEE.lenientDecode)

{- | Join cells into one row, quoting any cell that contains the delimiter, a
quote, or a line break (RFC 4180: inner quotes double). The exact inverse of
'splitRow'.
-}
joinRow :: Char -> [Text] -> Text
joinRow delim = T.intercalate (T.singleton delim) . map quote
  where
    quote cell
        | T.any (\c -> c == delim || c == '"' || c == '\n' || c == '\r') cell =
            "\"" <> T.replace "\"" "\"\"" cell <> "\""
        | otherwise = cell

{- | Parse a 'Double', 'Nothing' on failure – 'Amount.readAmount', the
correctly-rounded reader every importer shares ('TR.double' drifts in the
last ulp: @1.2227e-3@ came back as @1.2227000000000002e-3@, a wrong
number). It also rejects what a factor cell must never smuggle in: a
non-finite literal (@NaN@, @Infinity@) and trailing garbage (@1,23@ once
imported as @1.0@ – a silently truncated value).
-}
parseDouble :: Text -> Maybe Double
parseDouble = readAmount

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for standalone normalization/weighting CSV files.

Format (semicolon or comma delimited):

> # normalization-weighting set: My Custom NW
> category;normalization;weighting
> Climate change;1.32e-04;0.2106
> Acidification;1.80e-02;0.062
> ...

The first comment line (@# normalization-weighting set: NAME@) sets the
set name. If absent, the filename is used. The header row is required.
Empty normalization or weighting cells default to 0.
-}
module Method.ParserNW (
    parseNormWeightCSV,
    parseNormWeightCSVBytes,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR

import Method.Types (NormWeightSet (..))

-- | Parse a normalization/weighting CSV file from disk.
parseNormWeightCSV :: FilePath -> IO (Either String NormWeightSet)
parseNormWeightCSV path = do
    bs <- BS.readFile path
    let fallbackName = T.pack path
    return $ parseNormWeightCSVBytes fallbackName bs

-- | Pure parser. Takes a fallback name (used if no comment header found).
parseNormWeightCSVBytes :: Text -> BS.ByteString -> Either String NormWeightSet
parseNormWeightCSVBytes fallbackName bs =
    let allLines = BC.lines bs
        (comments, rest) = span (\l -> BC.isPrefixOf "#" l || BS.null l) allLines
        name = extractName comments fallbackName
        delim = detectDelimiter rest
     in case dropHeader rest of
            [] -> Left "NW CSV: no data rows after header"
            rows ->
                let parsed = [parseRow delim l | l <- rows, not (BS.null (BC.strip l))]
                    normMap = M.fromList [(cat, n) | (cat, n, _) <- parsed, not (T.null cat)]
                    weightMap = M.fromList [(cat, w) | (cat, _, w) <- parsed, not (T.null cat)]
                 in if M.null normMap && M.null weightMap
                        then Left "NW CSV: no valid rows parsed"
                        else Right $ NormWeightSet name normMap weightMap

-- | Extract set name from "# normalization-weighting set: NAME" comment.
extractName :: [BS.ByteString] -> Text -> Text
extractName [] fb = fb
extractName (l : ls) fb
    | "# normalization-weighting set:" `BC.isPrefixOf` lc =
        T.strip $ decode $ BS.drop (BS.length "# normalization-weighting set:") l
    | "# name:" `BC.isPrefixOf` lc =
        T.strip $ decode $ BS.drop (BS.length "# name:") l
    | otherwise = extractName ls fb
  where
    lc = BC.map toLowerASCII l

-- | Drop the header row (first non-empty, non-comment line starting with "category").
dropHeader :: [BS.ByteString] -> [BS.ByteString]
dropHeader [] = []
dropHeader (l : ls)
    | isHeaderRow l = ls
    | otherwise = ls -- skip first row even if not recognized as header

isHeaderRow :: BS.ByteString -> Bool
isHeaderRow l =
    let first = T.toLower . T.strip . decode . head' . splitOn (detectDelimiterLine l) $ l
     in first == "category" || first == "impact category" || first == "damage category"

-- | Parse one data row: "category;normalization;weighting"
parseRow :: Char -> BS.ByteString -> (Text, Double, Double)
parseRow delim line =
    case splitOn delim line of
        (cat : norm : weight : _) ->
            ( T.strip (decode cat)
            , parseDouble (BC.strip norm)
            , parseDouble (BC.strip weight)
            )
        (cat : norm : _) ->
            ( T.strip (decode cat)
            , parseDouble (BC.strip norm)
            , 0
            )
        _ -> ("", 0, 0)

-- | Detect delimiter from the first data-like lines.
detectDelimiter :: [BS.ByteString] -> Char
detectDelimiter [] = ';'
detectDelimiter (l : _) = detectDelimiterLine l

detectDelimiterLine :: BS.ByteString -> Char
detectDelimiterLine l
    | BC.elem ';' l = ';'
    | BC.elem '\t' l = '\t'
    | otherwise = ','

splitOn :: Char -> BS.ByteString -> [BS.ByteString]
splitOn delim = BC.split delim

decode :: BS.ByteString -> Text
decode = TE.decodeUtf8With TEE.lenientDecode

parseDouble :: BS.ByteString -> Double
parseDouble bs = case TR.double (decode bs) of
    Right (v, _) -> v
    Left _ -> 0

head' :: [a] -> a
head' (x : _) = x
head' [] = error "Method.ParserNW: unexpected empty split"

toLowerASCII :: Char -> Char
toLowerASCII c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

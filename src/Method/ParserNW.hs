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
import Data.Char (isAsciiUpper)
import Data.Indexing (uniqueIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Method.CSV (detectDelimiter, parseDouble, splitRow)
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
        delim = case rest of
            (l : _) -> detectDelimiter l
            [] -> ';'
     in case dropHeader rest of
            [] -> Left "NW CSV: no data rows after header"
            rows -> do
                let parsed = [parseRow delim l | l <- rows, not (BS.null (BC.strip l))]
                -- One category named twice carries two normalization factors and
                -- two weights, and the file says nothing about which is meant.
                factors <- categoryIndex [(cat, (n, w)) | (cat, n, w) <- parsed, not (T.null cat)]
                if M.null factors
                    then Left "NW CSV: no valid rows parsed"
                    else Right $ NormWeightSet name (M.map fst factors) (M.map snd factors)

-- | Index rows on their category, or refuse and name the ones spelled twice.
categoryIndex :: [(Text, (Double, Double))] -> Either String (M.Map Text (Double, Double))
categoryIndex rows = case uniqueIndex rows of
    Right table -> Right table
    Left cats -> Left $ "NW CSV: two rows for the same category: " <> T.unpack (T.intercalate ", " (NE.toList cats))

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
    case splitRow (detectDelimiter l) l of
        (first : _) ->
            T.toLower (T.strip first) `elem` ["category", "impact category", "damage category"]
        [] -> False

-- | Parse one data row: "category;normalization;weighting"
parseRow :: Char -> BS.ByteString -> (Text, Double, Double)
parseRow delim line =
    case map T.strip (splitRow delim line) of
        (cat : norm : weight : _) -> (cat, num norm, num weight)
        (cat : norm : _) -> (cat, num norm, 0)
        _ -> ("", 0, 0)
  where
    num = fromMaybe 0 . parseDouble

decode :: BS.ByteString -> Text
decode = TE.decodeUtf8With TEE.lenientDecode

toLowerASCII :: Char -> Char
toLowerASCII c
    | isAsciiUpper c = toEnum (fromEnum c + 32)
    | otherwise = c

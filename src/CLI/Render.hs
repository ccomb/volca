{-# LANGUAGE OverloadedStrings #-}

{- | Turn a result value into the bytes the user asked for.

Pure, so the rendering rules are testable without a server: the previous ones
sat inside the HTTP client and could only be reached with one running.

@--format csv@ flattens an array of objects into a header row plus one row per
element. A response often carries several arrays, and nothing can guess which
one was meant, so @--jsonpath@ names it as a dotted field path over the /wire/
names — @results@, @activity.exchanges@ — not the Haskell record fields, whose
lowercase prefix "API.JsonOptions" strips on the way out. When the response
carries exactly one array, or is itself one, the path is unnecessary.

Cells go out through the same encoder and the same formula guard as the
engine's own CSV routes ("API.Csv"), so a leading @=@ cannot become a
spreadsheet formula on one surface and not the other.
-}
module CLI.Render (
    renderResult,

    -- * Pure parts (exported for testing)
    selectPath,
    csvRows,
) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V

import API.Csv (spreadsheetSafe)
import CLI.Types (OutputFormat (..))

{- | Render a result, or say why it cannot be rendered that way. Returns bytes
rather than 'Text': the caller writes them to stdout as they are, so the output
is UTF-8 whatever locale the process was started in.
-}
renderResult :: OutputFormat -> Maybe Text -> Value -> Either Text BL.ByteString
renderResult fmt mPath val = case fmt of
    JSON -> Right (encode val <> "\n")
    Pretty -> Right (encodePretty val <> "\n")
    Table -> Right (utf8 (renderTable val))
    CSV -> renderCSV <$> csvRows mPath val

utf8 :: Text -> BL.ByteString
utf8 = TLE.encodeUtf8 . TL.fromStrict

fromUtf8 :: BL.ByteString -> Text
fromUtf8 = TL.toStrict . TLE.decodeUtf8

{- | The rows @--format csv@ should flatten: the array the path names, or —
when there is no path — the response itself if it is an array, or its single
array field. Ambiguity is refused rather than guessed at.
-}
csvRows :: Maybe Text -> Value -> Either Text [Value]
csvRows Nothing val = case findArray val of
    Just rows -> Right rows
    Nothing ->
        Left "--format csv needs --jsonpath to name the array to flatten: this result holds no single array field"
csvRows (Just path) val = do
    selected <- selectPath path val
    case selected of
        Array arr -> Right (V.toList arr)
        other ->
            Left $
                "--jsonpath \""
                    <> path
                    <> "\" names a "
                    <> jsonKind other
                    <> ", and --format csv flattens an array"

{- | Resolve a dotted field path against a JSON value: @activity.exchanges@
walks two object fields. A step that finds no such field lists the fields that
are there, because the wire names are the stripped record prefixes and are easy
to misremember.
-}
selectPath :: Text -> Value -> Either Text Value
selectPath path = go [] (T.splitOn "." path)
  where
    go _ [] value = Right value
    go walked (field : rest) value = case value of
        Object o -> case KM.lookup (Key.fromText field) o of
            Just next -> go (field : walked) rest next
            Nothing ->
                Left $
                    "--jsonpath \""
                        <> path
                        <> "\": no field \""
                        <> field
                        <> "\""
                        <> atPath walked
                        <> ". Available: "
                        <> T.intercalate ", " (map Key.toText (KM.keys o))
        other ->
            Left $
                "--jsonpath \""
                    <> path
                    <> "\": cannot look up \""
                    <> field
                    <> "\""
                    <> atPath walked
                    <> ", which is a "
                    <> jsonKind other

    atPath [] = ""
    atPath walked = " in \"" <> T.intercalate "." (reverse walked) <> "\""

jsonKind :: Value -> Text
jsonKind v = case v of
    Object _ -> "object"
    Array _ -> "array"
    String _ -> "string"
    Number _ -> "number"
    Bool _ -> "boolean"
    Null -> "null"

-- | Render a JSON value as an aligned text table
renderTable :: Value -> Text
renderTable val =
    case findArray val of
        Just rows -> T.pack (formatTable (extractTable rows))
        Nothing -> fromUtf8 (encodePretty val) <> "\n" -- fallback for non-array

{- | Render rows as RFC 4180 CSV, through the same encoder and formula guard
as the engine's CSV routes. An empty selection yields no bytes rather than a
blank line, so a consumer can tell "no rows" from a truncated write.
-}
renderCSV :: [Value] -> BL.ByteString
renderCSV [] = ""
renderCSV rows =
    let (headers, dataRows) = extractTable rows
     in Csv.encode (map (map spreadsheetSafe) (headers : dataRows))

-- | Find the sole array in a JSON value: the value itself, or its one array field.
findArray :: Value -> Maybe [Value]
findArray (Array arr) = Just (V.toList arr)
findArray (Object obj) =
    case mapMaybe extractArr (KM.elems obj) of
        [arr] -> Just arr
        _ -> Nothing
  where
    extractArr (Array arr) = Just (V.toList arr)
    extractArr _ = Nothing
findArray _ = Nothing

-- | Extract headers and rows from a list of JSON objects
extractTable :: [Value] -> ([Text], [[Text]])
extractTable [] = ([], [])
extractTable rows@(Object first : _) =
    let keys = KM.keys first
        headers = map Key.toText keys
        dataRows = map (rowValues keys) rows
     in (headers, dataRows)
extractTable rows = (["value"], map (\v -> [cellValue v]) rows)

rowValues :: [KM.Key] -> Value -> [Text]
rowValues keys (Object obj) = map (\k -> cellValue (fromMaybe Null (KM.lookup k obj))) keys
-- A non-object among objects would otherwise emit a one-field record under an
-- N-column header; pad so every row keeps the header's width.
rowValues keys v = cellValue v : replicate (length keys - 1) ""

{- | A JSON value as one cell. Numbers are written in fixed notation: a
spreadsheet reads @1.0e-2@ as text, and an LCA inventory is full of small
amounts.
-}
cellValue :: Value -> Text
cellValue (String t) = t
cellValue (Number n) = T.pack (trimTrailingZero (formatScientific Fixed Nothing n))
cellValue (Bool True) = "yes"
cellValue (Bool False) = ""
cellValue Null = ""
cellValue v = fromUtf8 (encode v)

trimTrailingZero :: String -> String
trimTrailingZero s = if ".0" `isSuffixOf` s then take (length s - 2) s else s

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Format headers + rows as an aligned table with separators
formatTable :: ([Text], [[Text]]) -> String
formatTable ([], _) = ""
formatTable (headers, rows) =
    let allRows = map (map T.unpack) (headers : rows)
        widths = map (foldl' max 0 . map length) (transpose (map (map (take maxColWidth)) allRows))
        padRow = zipWith (\w c -> take maxColWidth c ++ replicate (w - length (take maxColWidth c)) ' ') widths
        sep = intercalate "+" (map (\w -> replicate (w + 2) '-') widths)
        fmtRow r = "  " ++ intercalate " | " (padRow r)
     in unlines $ case allRows of
            (h : rs) -> fmtRow h : ("--" ++ sep) : map fmtRow rs
            [] -> []
  where
    maxColWidth = 60

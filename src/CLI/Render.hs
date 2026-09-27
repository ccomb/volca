{-# LANGUAGE OverloadedStrings #-}

{- | Turn a result value into the bytes the user asked for.

Pure, so the rendering rules are testable without a server: the previous ones
sat inside the HTTP client and could only be reached with one running.

@--format csv@ flattens an array of objects into a header row plus one row per
element. A response often carries several arrays, and nothing can guess which
one was meant, so @--jsonpath@ names it as a dotted field path over the /wire/
names – @results@, @activity.exchanges@ – not the Haskell record fields, whose
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
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Csv as Csv
import Data.List (dropWhileEnd, intercalate, sort, transpose)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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

{- | The rows @--format csv@ should flatten: the array the path names, or –
when there is no path – the response itself if it is an array, or its single
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

{- | Render a JSON value for a person at a terminal. Every field is shown: a
response is often a headline (a score, a coverage, the database just loaded)
next to a list, and picking the list alone would hide the answer. An object's
plain fields come first as aligned name and value lines, then each nested
object or list of objects under its name, indented.
-}
renderTable :: Value -> Text
renderTable = T.pack . unlines . map (dropWhileEnd (== ' ')) . valueLines

valueLines :: Value -> [String]
valueLines val = case val of
    Object o -> objectLines o
    Array arr -> rowsLines (V.toList arr)
    String _ -> [T.unpack (tableCell val)]
    Number _ -> [T.unpack (tableCell val)]
    Bool _ -> [T.unpack (tableCell val)]
    Null -> [T.unpack (tableCell val)]

-- | How one field of an object is shown, when it is shown at all.
data Field
    = -- | On one line beside its name
      Inline Text
    | -- | Under a heading, indented
      Block String [String]

objectLines :: KM.KeyMap Value -> [String]
objectLines o = intercalate [""] (filter (not . null) (aligned : blocks))
  where
    fields :: [(String, Field)]
    fields = [(Key.toString k, f) | (k, v) <- KM.toList o, Just f <- [fieldShown (Key.toString k) v]]
    aligned :: [String]
    aligned = [pad width name ++ "  " ++ T.unpack cell | (name, Inline cell) <- fields]
    width :: Int
    width = foldl' max 0 [length name | (name, Inline _) <- fields]
    blocks :: [[String]]
    blocks = [heading : map indent body | (_, Block heading body) <- fields]

indent :: String -> String
indent "" = ""
indent line = "  " ++ line

-- | A null or an empty object says nothing, so it takes no line.
fieldShown :: String -> Value -> Maybe Field
fieldShown name v = case v of
    Null -> Nothing
    Object o
        | KM.null o -> Nothing
        | otherwise -> Just (Block name (objectLines o))
    Array arr
        | V.null arr -> Just (Inline "none")
        | any isObject arr -> Just (Block (name <> " (" <> show (V.length arr) <> ")") (rowsLines (V.toList arr)))
        | otherwise -> Just (Inline (tableCell v))
    String _ -> Just (Inline (tableCell v))
    Number _ -> Just (Inline (tableCell v))
    Bool _ -> Just (Inline (tableCell v))

asObject :: Value -> Maybe (KM.KeyMap Value)
asObject v = case v of
    Object o -> Just o
    Array _ -> Nothing
    String _ -> Nothing
    Number _ -> Nothing
    Bool _ -> Nothing
    Null -> Nothing

isObject :: Value -> Bool
isObject = isJust . asObject

isScalar :: Value -> Bool
isScalar v = case v of
    Object _ -> False
    Array _ -> False
    String _ -> True
    Number _ -> True
    Bool _ -> True
    Null -> True

-- | One column of a table: its header, then one cell per row.
data Column = Column
    { columnHeader :: String
    , columnCells :: [String]
    }

{- | A list as a table, one row per element. A nested object spreads into
dotted columns (@flow.name@, @flow.compartment.name@), and a column empty in
every row is left out: a search result carries a dozen fields, most of them
unset for any given database.
-}
rowsLines :: [Value] -> [String]
rowsLines [] = ["none"]
rowsLines rows = formatTable (filter (not . all null . columnCells) columns)
  where
    flat :: [[(Text, Value)]]
    flat = map flatRow rows
    columns :: [Column]
    columns = [Column (T.unpack name) (map (shorten . maybe "" tableCell . lookup name) flat) | name <- sort (nubOrd (concatMap (map fst) flat))]

flatRow :: Value -> [(Text, Value)]
flatRow row = maybe [("value", row)] (concatMap spread . KM.toList) (asObject row)
  where
    spread :: (KM.Key, Value) -> [(Text, Value)]
    spread (k, v) = maybe [(Key.toText k, v)] (const [(Key.toText k <> "." <> sub, inner) | (sub, inner) <- flatRow v]) (asObject v)

{- | A table cell. Unlike a CSV cell, false is written: a blank there reads as
"unknown". A list of plain values is spelled out rather than shown as JSON.
A line break inside a value (a comment carried over from the source file)
becomes a space, or it would push the rest of its row onto the next line.
-}
tableCell :: Value -> Text
tableCell = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c) . cellText
  where
    cellText :: Value -> Text
    cellText v = case v of
        Bool False -> "no"
        Bool True -> cellValue v
        Array arr
            | all isScalar arr -> T.intercalate ", " (map cellText (V.toList arr))
            | otherwise -> cellValue v
        Object _ -> cellValue v
        String _ -> cellValue v
        Number _ -> cellValue v
        Null -> cellValue v

{- | Long prose is cut to keep a table on the screen. A value with no space in
it, an identifier or a path, is left whole: it is there to be copied, and a
cut one names nothing.
-}
shorten :: Text -> String
shorten cell
    | T.length cell > maxCell && T.any (== ' ') cell = T.unpack (T.take (maxCell - 1) cell) ++ "…"
    | otherwise = T.unpack cell
  where
    maxCell :: Int
    maxCell = 60

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

-- | Columns, each a header and its cells, as aligned lines under a rule.
formatTable :: [Column] -> [String]
formatTable [] = []
formatTable columns = fmtRow (map columnHeader columns) : rule : map fmtRow (transpose (map columnCells columns))
  where
    widths :: [Int]
    widths = [foldl' max (length h) (map length cells) | Column h cells <- columns]
    fmtRow :: [String] -> String
    fmtRow = intercalate " | " . zipWith pad widths
    rule :: String
    rule = intercalate "-+-" [replicate w '-' | w <- widths]

pad :: Int -> String -> String
pad w s = s ++ replicate (w - length s) ' '

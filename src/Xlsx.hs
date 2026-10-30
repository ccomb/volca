{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Reading the worksheets of an @.xlsx@ workbook, whatever the workbook holds.

An @.xlsx@ is a zip of XML parts, unzipped with @zip-archive@. The small parts
(the workbook, its relationships, the shared-string table) are read with the
@xeno@ DOM. A worksheet is read with the @xeno@ SAX fold, one row at a time,
because a sheet can be large: a table of characterization factors with one
factor per row decompresses to over a hundred megabytes of XML, which a DOM
would multiply several times over.
-}
module Xlsx (
    CellValue (..),
    Row,
    cellText,
    Workbook,
    openWorkbook,
    sheetNames,
    foldSheet,
    readSheets,

    -- * Exposed for testing
    parseSheetXml,
    foldSheetXml,
) where

import Amount (readAmount)
import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry, toArchiveOrFail)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiUpper, ord, toUpper)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import EcoSpold.Common (numericRefChar)
import Xeno.DOM (Content (..), Node)
import qualified Xeno.DOM as X
import qualified Xeno.SAX as SAX

-- | A spreadsheet cell value, already typed at read time.
data CellValue
    = CellText !Text
    | CellNumber !Double
    deriving (Eq, Show)

{- | A row is a sparse map from 0-based column index to value. Empty cells are
absent, so an empty 'M.Map' is an empty row.
-}
type Row = M.Map Int CellValue

cellText :: CellValue -> Maybe Text
cellText = \case
    CellText t -> let s = T.strip t in if T.null s then Nothing else Just s
    CellNumber _ -> Nothing

-- | An opened workbook: its worksheets by name, in workbook order, not yet read.
data Workbook = Workbook
    { wbArchive :: !Archive
    , wbSharedStrings :: !(Maybe (V.Vector Text))
    , wbSheets :: ![(Text, FilePath)]
    }

openWorkbook :: BL.ByteString -> Either Text Workbook
openWorkbook raw = do
    archive <- first T.pack (toArchiveOrFail raw)
    workbook <- parseXml =<< entry archive "xl/workbook.xml"
    rels <- parseXml =<< entry archive "xl/_rels/workbook.xml.rels"
    sharedStrings <- traverse parseSharedStrings (entryMaybe archive "xl/sharedStrings.xml")
    let relMap =
            M.fromList
                [ (TE.decodeUtf8 i, TE.decodeUtf8 tgt)
                | r <- childrenNamed "Relationship" rels
                , Just i <- [attr "Id" r]
                , Just tgt <- [attr "Target" r]
                ]
        sheetPart rid = maybe (Left ("unknown worksheet relationship " <> rid)) (Right . resolveTarget) (M.lookup rid relMap)
    sheets <-
        sequence
            [ (,) sheetName <$> sheetPart (TE.decodeUtf8 rid)
            | sheetsNode <- maybeToList (firstChildNamed "sheets" workbook)
            , s <- childrenNamed "sheet" sheetsNode
            , Just rid <- [attr "r:id" s]
            , let sheetName = maybe (TE.decodeUtf8 rid) TE.decodeUtf8 (attr "name" s)
            ]
    pure Workbook{wbArchive = archive, wbSharedStrings = sharedStrings, wbSheets = sheets}

sheetNames :: Workbook -> [Text]
sheetNames = map fst . wbSheets

{- | Fold over the rows of the worksheet of that name, in sheet order. The
sheet's XML is decompressed whole, since the SAX parser reads a strict string,
but no row outlives the step that consumes it.
-}
foldSheet :: Workbook -> Text -> (a -> Row -> a) -> a -> Either Text a
foldSheet wb name step start = do
    part <- maybe (Left ("the workbook has no sheet named " <> name)) Right (lookup name (wbSheets wb))
    bytes <- entry (wbArchive wb) part
    foldSheetXml (wbSharedStrings wb) step start bytes

{- | Unzip an @.xlsx@ and return its worksheets, in workbook order, paired with
their declared names (so a skipped sheet can be named in a warning).
-}
readSheets :: BL.ByteString -> Either Text [(Text, [Row])]
readSheets raw = do
    wb <- openWorkbook raw
    traverse (\(name, part) -> (,) name <$> (parseSheetXml (wbSharedStrings wb) =<< entry (wbArchive wb) part)) (wbSheets wb)

{- | Resolve a relationship @Target@ to a zip entry path. Absolute targets
(leading @/@) are package-root relative; everything else is relative to the
@xl/@ directory that holds @workbook.xml@.
-}
resolveTarget :: Text -> FilePath
resolveTarget target = case T.stripPrefix "/" target of
    Just rooted -> T.unpack rooted
    Nothing -> "xl/" <> T.unpack target

entry :: Archive -> FilePath -> Either Text BS.ByteString
entry archive path =
    maybe (Left ("missing zip entry: " <> T.pack path)) Right (entryMaybe archive path)

entryMaybe :: Archive -> FilePath -> Maybe BS.ByteString
entryMaybe archive path = BL.toStrict . fromEntry <$> findEntryByPath path archive

-- | Parse a worksheet XML part into rows.
parseSheetXml :: Maybe (V.Vector Text) -> BS.ByteString -> Either Text [Row]
parseSheetXml sharedStrings = fmap reverse . foldSheetXml sharedStrings (flip (:)) []

-- | Where the SAX fold stands inside a worksheet.
data Scan a = Scan
    { scAcc :: !a
    , scRow :: !Row
    , scTag :: !BS.ByteString
    -- ^ The element whose attributes are being read.
    , scColumn :: !(Maybe Int)
    , scType :: !(Maybe BS.ByteString)
    , scText :: !(Maybe [BS.ByteString])
    -- ^ The cell's text so far, reversed; 'Nothing' until a @\<v\>@ or @\<is\>@ opens.
    , scCollecting :: !Bool
    }

{- | Fold over the rows of a worksheet XML part. A cell is decoded by its @t@
type attribute: a shared-string index, an inline string (every text under its
@\<is\>@, rich-text runs included), a formula's string result, a boolean, or a
number.
-}
foldSheetXml :: Maybe (V.Vector Text) -> (a -> Row -> a) -> a -> BS.ByteString -> Either Text a
foldSheetXml sharedStrings step start bytes =
    first (\e -> "worksheet: " <> T.pack (show e)) (scAcc <$> SAX.fold open attribute (\s _ -> s) text close text initial bytes)
  where
    initial = Scan{scAcc = start, scRow = M.empty, scTag = "", scColumn = Nothing, scType = Nothing, scText = Nothing, scCollecting = False}
    open s tag = case tag of
        "row" -> s{scTag = tag, scRow = M.empty}
        "c" -> s{scTag = tag, scColumn = Nothing, scType = Nothing, scText = Nothing, scCollecting = False}
        _ | tag == "v" || tag == "is" -> s{scTag = tag, scText = Just (fromMaybe [] (scText s)), scCollecting = True}
        _ -> s{scTag = tag}
    attribute s k v
        | scTag s /= "c" = s
        | k == "r" = s{scColumn = columnIndex v}
        | k == "t" = s{scType = Just v}
        | otherwise = s
    text s chunk
        | scCollecting s = s{scText = (chunk :) <$> scText s}
        | otherwise = s
    close s tag = case tag of
        _ | tag == "v" || tag == "is" -> s{scCollecting = False}
        "c" -> case (scColumn s, cellValue sharedStrings (scType s) (decoded <$> scText s)) of
            (Just col, Just value) -> s{scRow = M.insert col value (scRow s)}
            _ -> s
        "row" -> s{scAcc = step (scAcc s) (scRow s), scRow = M.empty}
        _ -> s
    decoded = decodeEntities . TE.decodeUtf8With TEE.lenientDecode . BS.concat . reverse

-- | Decode one cell from its @t@ type attribute and its text, if it had any.
cellValue :: Maybe (V.Vector Text) -> Maybe BS.ByteString -> Maybe Text -> Maybe CellValue
cellValue sharedStrings cellType content = case cellType of
    Just "inlineStr" -> nonEmptyText =<< content
    Just "s" -> do
        idx <- readInt =<< content
        table <- sharedStrings
        nonEmptyText =<< (table V.!? idx)
    Just "str" -> nonEmptyText =<< content
    Just "b" -> CellText <$> content
    _ -> do
        txt <- content
        case readAmount txt of
            Just d -> Just (CellNumber d)
            Nothing -> nonEmptyText txt

nonEmptyText :: Text -> Maybe CellValue
nonEmptyText t = let s = T.strip t in if T.null s then Nothing else Just (CellText s)

readInt :: Text -> Maybe Int
readInt t = case TR.decimal (T.strip t) of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

{- | Shared-string table: each @\<si\>@ maps to its concatenated text. A present
but unparseable table is surfaced rather than silently treated as empty,
which would drop every @t="s"@ cell without trace.
-}
parseSharedStrings :: BS.ByteString -> Either Text (V.Vector Text)
parseSharedStrings bs = case X.parse bs of
    Left err -> Left ("shared strings: " <> T.pack (show err))
    Right sst -> Right (V.fromList [nodeText si | si <- childrenNamed "si" sst])

-- ---------------------------------------------------------------------------
-- xeno DOM helpers
-- ---------------------------------------------------------------------------

parseXml :: BS.ByteString -> Either Text Node
parseXml = first (T.pack . show) . X.parse

childrenNamed :: BS.ByteString -> Node -> [Node]
childrenNamed n = filter ((== n) . X.name) . X.children

firstChildNamed :: BS.ByteString -> Node -> Maybe Node
firstChildNamed n = listToMaybe . childrenNamed n

attr :: BS.ByteString -> Node -> Maybe BS.ByteString
attr k = lookup k . X.attributes

{- | All text under a node (recursively), UTF-8 decoded with XML entities
expanded. xeno returns raw byte ranges, so entity expansion is on us.
-}
nodeText :: Node -> Text
nodeText = decodeEntities . TE.decodeUtf8With TEE.lenientDecode . BS.concat . go
  where
    go node = concatMap content (X.contents node)
    content = \case
        Text t -> [t]
        CData t -> [t]
        Element e -> go e

columnIndex :: BS.ByteString -> Maybe Int
columnIndex ref =
    let letters = BS8.takeWhile isAsciiUpper (BS8.map toUpper ref)
     in if BS.null letters
            then Nothing
            else Just (BS8.foldl' (\acc ch -> acc * 26 + (ord ch - ord 'A' + 1)) 0 letters - 1)

-- | Expand the five predefined XML entities and numeric character references.
decodeEntities :: Text -> Text
decodeEntities t
    | T.isInfixOf "&" t = go t
    | otherwise = t
  where
    go s =
        let (before, rest) = T.breakOn "&" s
         in if T.null rest
                then before
                else
                    let (ent, after) = T.breakOn ";" rest
                     in case decodeOne (T.drop 1 ent) of
                            Just c | not (T.null after) -> before <> c <> go (T.drop 1 after)
                            _ -> before <> "&" <> go (T.drop 1 rest)
    decodeOne e
        | e == "lt" = Just "<"
        | e == "gt" = Just ">"
        | e == "amp" = Just "&"
        | e == "quot" = Just "\""
        | e == "apos" = Just "'"
        | Just numBody <- T.stripPrefix "#" e = T.singleton <$> numericRefChar numBody
        | otherwise = Nothing

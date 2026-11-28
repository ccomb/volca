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

The label row names the key columns; data columns start after them.
@substance@ and @compartment@ are required, in that order. Three more key
columns are recognised (in any order – 'Method.WriterCSV' emits them as
@cas@, @unit@, @location@): @cas@ (the substance's CAS number), @unit@ (the
flow unit of that row, when it differs from the category unit in the header –
real methods mix kg, m3 and MJ flows inside one category) and @location@
(the location a regionalized factor applies to; blank for a factor that has
none).

A compartment cell is either a legacy prose form (@Emissions to air@,
@Resources@ – matched by keyword) or a @/@-separated path
(@water/groundwater@ = compartment, subcompartment), which is what the writer
emits and the only way to keep subcompartment distinctions – most of a real
method's factors are subcompartment-specific. The file reader also takes a
third segment as a qualifier; the strict reader takes @top/sub@ alone, with a
qualifier after a comma inside the subcompartment (@water/groundwater,
long-term@), because matching reads two segments and loses a third.

One CSV file → multiple 'Method' values (one per column).

'parseMethodCSVBytes' reads files as they come, dropping a factor it cannot
read and matching prose compartments by keyword. 'parseMethodCSVStrict' is
for a table written to be imported: it takes only @top/sub@ paths, requires
the @unit@ key column, and refuses every line it would otherwise drop or
guess at.
-}
module Method.ParserCSV (
    parseMethodCSVBytes,
    parseMethodCSVStrict,
    CsvDefect (..),
    isLabelRow,
    knownTops,
    stripBOM,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, elemIndex, sortOn, zip4)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe, isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID5
import Data.Word (Word8)

import Data.Indexing (collisions)
import Method.CSV (detectDelimiter, parseDouble, quotedCells, splitRow)
import Method.Mapping (foldSub)
import Method.Types
import SynonymDB (normalizeNameKeepUnit)
import Types (mediumText)
import UnitConversion (UnitConfig, UnitReading (..), readUnit)

-- | Why a line of a method CSV cannot be taken as written.
data CsvDefect = CsvDefect
    { cdLine :: !Int
    -- ^ 1-based, counted in the text as sent, comments included
    , cdReason :: !Text
    }
    deriving (Eq, Show)

-- | Pure parser, exported for testing.
parseMethodCSVBytes :: BS.ByteString -> Either String [Method]
parseMethodCSVBytes = fmap tableMethods . first (T.unpack . cdReason) . readTable

{- | Read a method CSV as written or not at all: what the lenient reader drops
or guesses at is refused, every defect with its line, so the author fixes the
text instead of losing factors in silence. A table without a defect gives the
same methods as the lenient reader.
-}
parseMethodCSVStrict :: UnitConfig -> BS.ByteString -> Either [CsvDefect] [Method]
parseMethodCSVStrict units bytes = do
    table <- first pure (readTable bytes)
    case tableDefects units table of
        [] -> Right (tableMethods table)
        defects -> Left defects

-- | A line of the text with its 1-based number.
data Line = Line !Int !BS.ByteString

lineBytes :: Line -> BS.ByteString
lineBytes (Line _ bytes) = bytes

-- | A data line split into cells.
data Row = Row !Int ![Text]

-- | One impact category column of the header.
data Column = Column
    { colIndex :: !Int
    -- ^ 0-based, counted after the key columns
    , colCategory :: !Text
    , colName :: !Text
    , colUnit :: !Text
    }

-- | The header read, and the data lines after it.
data Table = Table
    { tMethodology :: !(Maybe Text)
    , tLabelLine :: !Int
    , tDelim :: !Char
    , tKeys :: !KeyColumns
    , tColumns :: ![Column]
    -- ^ the named ones: a column with a blank name holds no category
    , tWidth :: !Int
    -- ^ key columns and category columns, named or not
    , tUnitLine :: !Int
    , tHeaderDefects :: ![CsvDefect]
    -- ^ what the lenient reader tolerates in the header and the strict one refuses
    , tRows :: ![Line]
    }

readTable :: BS.ByteString -> Either CsvDefect Table
readTable bytes =
    let allLines = zipWith Line [1 ..] (map stripCR (BC.lines (stripBOM bytes)))
        (comments, rest) = span (\l -> BC.isPrefixOf "#" (lineBytes l) || BS.null (lineBytes l)) allLines
     in splitHeaderFromData (extractMethodology (map lineBytes comments)) rest

-- | Each named column read as one method.
tableMethods :: Table -> [Method]
tableMethods t = [mkMethod (tMethodology t) (tKeys t) col cellRows | col <- tColumns t]
  where
    -- Split each data row once; every method column reads the same cells.
    cellRows :: [[Text]]
    cellRows = map (splitRow (tDelim t) . lineBytes) (tRows t)

{- | The key columns of the label row: the non-blank leading cells. Data
columns start right after them.
-}
data KeyColumns = KeyColumns
    { kcCount :: !Int
    , kcCAS :: !(Maybe Int)
    , kcUnit :: !(Maybe Int)
    , kcLocation :: !(Maybe Int)
    }

{- | Read the key columns off the label row. @substance@ and @compartment@
must come first (that's also what 'isLabelRow' anchors on); @cas@, @unit@
and @location@ are optional. An unrecognised key column is an error, not a
silently skipped column of data.
-}
parseKeyColumns :: [Text] -> Either Text KeyColumns
parseKeyColumns labels =
    let keys = map (T.toCaseFold . T.strip) (takeWhile (not . T.null . T.strip) labels)
     in case keys of
            ("substance" : "compartment" : extras) ->
                case filter (`notElem` ["cas", "unit", "location"]) extras of
                    bad : _ -> Left ("unknown key column in CSV method label row: " <> bad)
                    [] ->
                        Right
                            KeyColumns
                                { kcCount = length keys
                                , kcCAS = (+ 2) <$> elemIndex "cas" extras
                                , kcUnit = (+ 2) <$> elemIndex "unit" extras
                                , kcLocation = (+ 2) <$> elemIndex "location" extras
                                }
            _ -> Left "CSV method label row must start with 'substance;compartment'"

{- | Find the label row (first column starts with "substance") and split header
rows from data rows. 2-row layout → categories = names. 3-row layout →
distinct. A defect of the header is on the label row, or on line 1 when there
is none.
-}
splitHeaderFromData :: Maybe Text -> [Line] -> Either CsvDefect Table
splitHeaderFromData methodology dataLines =
    case break (isLabelRow . lineBytes) dataLines of
        (headerRows, Line labelLine labelRow : rows) -> do
            let !delim = detectDelimiter labelRow
                atLabel = first (CsvDefect labelLine)
            keys <- atLabel (parseKeyColumns (splitRow delim labelRow))
            (catRow, nameRow@(Line nameLine _), unitRow@(Line unitLine _)) <- atLabel $ case filter (not . BS.null . lineBytes) headerRows of
                [nameRow, unitRow] -> Right (Nothing, nameRow, unitRow)
                [catRow, nameRow, unitRow] -> Right (Just catRow, nameRow, unitRow)
                _ -> Left "CSV method file needs at least 3 header rows (names, units, column labels)"
            let dropKeys = drop (kcCount keys) . splitRow delim . lineBytes
                names = dropKeys nameRow
                cats = maybe names dropKeys catRow
                units = dropKeys unitRow
                columns =
                    [ Column i c n u
                    | (i, c, n, u) <- zip4 [0 ..] cats names units
                    , not (T.null n)
                    ]
                -- Rows read past a names row that ends inside the key columns
                -- would each be refused as too long, sending the author to the
                -- wrong lines.
                unnamedCategories =
                    [ CsvDefect
                        nameLine
                        ( "no impact category is named past the "
                            <> tshow (kcCount keys)
                            <> " key columns of the label row: the names and units rows start with "
                            <> tshow (kcCount keys)
                            <> " cells before the category name"
                        )
                    | null columns
                    ]
            Right
                Table
                    { tMethodology = methodology
                    , tLabelLine = labelLine
                    , tDelim = delim
                    , tKeys = keys
                    , tColumns = columns
                    , tWidth = kcCount keys + length names
                    , tUnitLine = unitLine
                    , tHeaderDefects =
                        unnamedCategories
                            <> catMaybes
                                ( widthDefect delim nameRow "units" unitRow
                                    : [widthDefect delim nameRow "categories" r | Just r <- [catRow]]
                                )
                    , tRows = rows
                    }
        _ -> Left (CsvDefect 1 "CSV method file has no 'substance;compartment;...' label row")

{- | A header row whose cell count differs from the names row's, refused on
the shorter of the two: the columns past it would lose their name or unit.
-}
widthDefect :: Char -> Line -> Text -> Line -> Maybe CsvDefect
widthDefect delim (Line nameLine names) what (Line otherLine other)
    | nameWidth == otherWidth = Nothing
    | otherwise =
        Just $
            CsvDefect
                (if otherWidth < nameWidth then otherLine else nameLine)
                ( "the names row has "
                    <> tshow nameWidth
                    <> " cells and the "
                    <> what
                    <> " row "
                    <> tshow otherWidth
                    <> ": every header row needs one cell per column"
                )
  where
    nameWidth, otherWidth :: Int
    nameWidth = length (splitRow delim names)
    otherWidth = length (splitRow delim other)

-- | Detect the label row: first cell is a variation of "substance".
isLabelRow :: BS.ByteString -> Bool
isLabelRow line =
    case splitRow (detectDelimiter line) line of
        (cell : _) -> T.toCaseFold (T.strip cell) == "substance"
        [] -> False

-- | Build a single Method from one column of the pre-split CSV rows.
mkMethod :: Maybe Text -> KeyColumns -> Column -> [[Text]] -> Method
mkMethod methodology keys col cellRows =
    Method
        { methodId = UUID5.generateNamed csvMethodNamespace (bsKey $ "method:" <> colName col)
        , methodName = colName col
        , methodDescription = Nothing
        , methodUnit = columnUnit col
        , methodCategory = colCategory col
        , methodMethodology = methodology
        , methodFactors = [cf | cells <- cellRows, Just (Right cf) <- [cellFactor keys col cells]]
        }

-- | The unit a column's header gives its factors.
columnUnit :: Column -> Text
columnUnit col = if T.null (colUnit col) then "unknown" else colUnit col

{- | The factor one row gives one column: none when the value cell or the
substance is blank, else the factor or why its value is not one. Both readers
go through it; the lenient one drops what the strict one refuses.
-}
cellFactor :: KeyColumns -> Column -> [Text] -> Maybe (Either Text MethodCF)
cellFactor keys col cells
    | T.null sub || T.null raw = Nothing
    | otherwise = Just $ case parseDouble raw of
        Nothing -> Left ("'" <> raw <> "' is not a number")
        Just v ->
            Right
                MethodCF
                    { mcfFlowRef = UUID5.generateNamed csvMethodNamespace (bsKey $ sub <> "::" <> comp)
                    , mcfFlowName = sub
                    , mcfDirection = directionFromCompartment comp compartment
                    , mcfValue = v
                    , mcfCompartment = compartment
                    , mcfCAS = nonBlank (keyedCell cells (kcCAS keys))
                    , mcfUnit = fromMaybe (columnUnit col) (nonBlank (keyedCell cells (kcUnit keys)))
                    , mcfConsumerLocation = nonBlank (keyedCell cells (kcLocation keys))
                    }
  where
    sub, comp, raw :: Text
    sub = cellAt cells 0
    comp = cellAt cells 1
    raw = cellAt cells (colIndex col + kcCount keys)
    compartment :: Maybe Compartment
    compartment = parseCSVCompartment comp

-- | The stripped cell at a 0-based position, blank past the end of the row.
cellAt :: [Text] -> Int -> Text
cellAt cells i = case drop i cells of
    x : _ -> T.strip x
    [] -> ""

-- | The cell of an optional key column, blank when the table has none.
keyedCell :: [Text] -> Maybe Int -> Text
keyedCell cells = maybe "" (cellAt cells)

nonBlank :: Text -> Maybe Text
nonBlank t = if T.null t then Nothing else Just t

-- | Every defect of a table whose header read, in line order.
tableDefects :: UnitConfig -> Table -> [CsvDefect]
tableDefects units t
    -- Rows read against header rows that disagree would only echo them.
    | not (null (tHeaderDefects t)) = tHeaderDefects t
    | otherwise = sortOn cdLine (unitColumn <> blankUnits <> unbalanced <> concatMap (rowDefects units t) rows <> duplicates (tKeys t) rows)
  where
    unbalanced :: [CsvDefect]
    rows :: [Row]
    (unbalanced, rows) = partitionEithers (map (strictRow (tDelim t)) (tRows t))
    unitColumn :: [CsvDefect]
    unitColumn =
        [ CsvDefect (tLabelLine t) "no 'unit' key column: each row must name the unit of its flow"
        | isNothing (kcUnit (tKeys t))
        ]
    -- The lenient reader would give such a category the unit "unknown".
    blankUnits :: [CsvDefect]
    blankUnits =
        [ CsvDefect (tUnitLine t) ("blank unit for the impact category '" <> colName col <> "'")
        | col <- tColumns t
        , T.null (T.strip (colUnit col))
        ]

{- | A data line split as RFC 4180. An empty line is a row of no cells; any
other line the quoting cannot read is refused rather than split naively. An
odd number of quotes is refused too: the quoting would read an open quote to
the end of the line, swallowing every cell after it into one.
-}
strictRow :: Char -> Line -> Either CsvDefect Row
strictRow delim (Line n bs)
    | BS.null bs = Right (Row n [])
    | odd (BC.count '"' bs) = unbalanced
    | otherwise = maybe unbalanced (Right . Row n) (quotedCells delim bs)
  where
    unbalanced :: Either CsvDefect Row
    unbalanced = Left (CsvDefect n "unbalanced quote")

-- | Why a row cannot be taken as written, each cause named once.
rowDefects :: UnitConfig -> Table -> Row -> [CsvDefect]
rowDefects units t (Row n cells) = map (CsvDefect n) (surplus <> unnamed <> valueless <> blanks <> unit <> compartment <> values)
  where
    keys :: KeyColumns
    keys = tKeys t
    -- Trailing blank cells say nothing, and a number split in two always leaves a non-blank one.
    used, extra :: Int
    used = length (dropWhileEnd (T.null . T.strip) cells)
    extra = used - tWidth t
    surplus :: [Text]
    surplus =
        [ "the row has "
            <> (if extra == 1 then "one cell" else tshow extra <> " cells")
            <> " too many: "
            <> tshow used
            <> " where the header has "
            <> tshow (tWidth t)
            <> "; quote a cell that holds the delimiter (under ',' a decimal comma splits a number in two)"
        | extra > 0
        ]
    -- A miscounted delimiter in a header row moves the names off their values.
    unnamed :: [Text]
    unnamed =
        [ "a value under a column the header does not name: count the delimiters of the header rows"
        | any
            (\i -> not (T.null (cellAt cells (kcCount keys + i))))
            (filter (`notElem` map colIndex (tColumns t)) [0 .. tWidth t - kcCount keys - 1])
        ]
    carriesValue :: Bool
    carriesValue = not (all (T.null . T.strip) (drop (kcCount keys) cells))
    -- A cell left out moves the value into a key column, leaving the value cell blank.
    valueless :: [Text]
    valueless =
        [ "no value under any impact category: write the factor in its column (a cell left out of the row moves it into a key column), or remove the row"
        | not carriesValue
        , not (all (T.null . T.strip) cells)
        ]
    blanks :: [Text]
    blanks =
        [ "blank " <> what <> " on a row that carries a value"
        | carriesValue
        , (what, column) <- [("substance", Just 0), ("compartment", Just 1), ("unit", kcUnit keys)]
        , i <- maybeToList column
        , T.null (cellAt cells i)
        ]
    -- Scoring counts a factor in a unit its table does not know as if it were in the base unit.
    unit :: [Text]
    unit =
        [ reason
        | carriesValue
        , i <- maybeToList (kcUnit keys)
        , let written = cellAt cells i
        , not (T.null written)
        , Just reason <- [unitDefect units written]
        ]
    comp :: Text
    comp = cellAt cells 1
    compartment :: [Text]
    compartment = [reason | not (T.null comp), Just reason <- [compartmentDefect comp]]
    values :: [Text]
    values = [reason | col <- tColumns t, Just (Left reason) <- [cellFactor keys col cells]]

tshow :: Int -> Text
tshow = T.pack . show

-- | Why a flow unit is not one the table reads, if it is not.
unitDefect :: UnitConfig -> Text -> Maybe Text
unitDefect units written = case readUnit units written of
    ReadExact _ -> Nothing
    ReadRespelt spelling _ -> Just ("'" <> written <> "' is spelt '" <> spelling <> "' in the engine's unit table: write it so")
    ReadAmbiguous a b rest -> Just ("'" <> written <> "' could be " <> T.intercalate " or " (a : b : rest) <> ": write the unit meant")
    ReadUnknown -> Just ("'" <> written <> "' is not a unit the engine knows: write the flow unit as its unit table spells it (kg, g, m3, MJ, kWh)")

{- | Why a compartment cell is not a @top/sub@ path with a known top, if it is
not. A qualifier goes after a comma inside the sub-compartment, because
matching reads two segments of the path and loses a third.
-}
compartmentDefect :: Text -> Maybe Text
compartmentDefect comp = case map T.strip (T.splitOn "/" comp) of
    [top] | known top -> Nothing
    [top, _] | known top -> Nothing
    top : sub : rest@(_ : _)
        | known top ->
            Just
                ( "'"
                    <> comp
                    <> "' has more than two segments; write the qualifier after a comma: '"
                    <> top
                    <> "/"
                    <> T.intercalate ", " (sub : rest)
                    <> "'"
                )
    _ -> Just ("'" <> comp <> "' is not a compartment: write top/sub, with top one of " <> T.intercalate ", " knownTops)
  where
    known :: Text -> Bool
    known top = T.toCaseFold top `elem` knownTops

{- | What makes two rows one factor to matching: the name as matching keys
it, the compartment with its sub-compartment folded, and the location.
-}
data RowKey = RowKey !Text !(Maybe Compartment) !(Maybe Text)
    deriving (Eq, Ord)

-- | Each row that gives the same factor as another, naming the others.
duplicates :: KeyColumns -> [Row] -> [CsvDefect]
duplicates keys rows =
    [ CsvDefect n ("same substance, compartment and location as line " <> T.intercalate ", " (map tshow others))
    | (_, ns) <- collisions keyed
    , n <- NE.toList ns
    , let others = filter (/= n) (NE.toList ns)
    ]
  where
    -- A row refused for its substance or compartment already says so.
    keyed :: [(RowKey, Int)]
    keyed =
        [ (RowKey (normalizeNameKeepUnit sub) (foldCompartment <$> parseCSVCompartment comp) (nonBlank (keyedCell cells (kcLocation keys))), n)
        | Row n cells <- rows
        , let sub = cellAt cells 0
              comp = cellAt cells 1
        , not (T.null sub)
        , not (T.null comp)
        , isNothing (compartmentDefect comp)
        ]
    foldCompartment :: Compartment -> Compartment
    foldCompartment (Compartment medium s q) = let Subcompartment folded = foldSub (Subcompartment s) in Compartment medium folded q

-- | Deterministic namespace for CSV-derived UUIDs.
csvMethodNamespace :: UUID
csvMethodNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "volca:csv-method")

{- | Direction: a resource or land compartment → Input, everything else →
Output. Reads the same spellings as the SimaPro parser (@Resources@, @Raw@,
@Raw materials@, @Resources from ground@…): a resource CF mis-defaulted to
Output would resolve against the output synonym view and silently lose
input-only bridges. Takes the already-parsed compartment alongside the raw
cell because the raw spellings ("Raw materials") can imply Input even when
no compartment parses.
-}
directionFromCompartment :: Text -> Maybe Compartment -> FlowDirection
directionFromCompartment comp parsed
    | isResourceTop = Input
    | lc == "resources" || "raw" `T.isPrefixOf` lc || "resources " `T.isPrefixOf` lc = Input
    | otherwise = Output
  where
    lc = T.toCaseFold comp
    isResourceTop = case parsed of
        Just (Compartment t _ _) -> t == "natural resource" || "land " `T.isPrefixOf` t
        Nothing -> False

{- | Parse the compartment cell: a @/@-separated path with a known top-level
compartment first, or a legacy prose form matched by keyword ("Emissions to
air", "Emissions to fresh water", "Resources", …). A cell that looks like a
path but has an unknown top (or too many segments) falls back to the keyword
match on the whole cell: legacy prose can contain a @/@ ("Emissions to
water/groundwater"), and losing its compartment silently would degrade files
that parsed before the path form existed.
-}
parseCSVCompartment :: Text -> Maybe Compartment
parseCSVCompartment comp = case map T.strip (T.splitOn "/" comp) of
    [] -> Nothing -- splitOn never returns [], but the match must be total
    [single] -> pathCompartment single "" "" <|> legacyCompartment single
    [top, s] -> pathCompartment top s "" <|> legacyCompartment comp
    [top, s, q] -> pathCompartment top s q <|> legacyCompartment comp
    _ -> legacyCompartment comp -- more path segments than a compartment has fields

-- | A path form is only a compartment when its top segment is one we know.
pathCompartment :: Text -> Text -> Text -> Maybe Compartment
pathCompartment top s q
    | T.toCaseFold top `elem` knownTops = Just (Compartment (T.toCaseFold top) s q)
    | otherwise = Nothing

{- | The top-level compartments this format can name. Shared with
"Method.WriterCSV", whose guard refuses to write a compartment the re-import
would not read back.
-}
knownTops :: [Text]
knownTops = map mediumText [minBound .. maxBound] <> ["land occupation", "land transformation"]

-- | Keyword match for the legacy prose forms.
legacyCompartment :: Text -> Maybe Compartment
legacyCompartment comp
    | T.null comp = Nothing
    | "land occupation" `T.isInfixOf` lc = Just (Compartment "land occupation" "" "")
    | "land transformation" `T.isInfixOf` lc = Just (Compartment "land transformation" "" "")
    | "air" `T.isInfixOf` lc = Just (Compartment "air" "" "")
    | "water" `T.isInfixOf` lc = Just (Compartment "water" "" "")
    | "soil" `T.isInfixOf` lc = Just (Compartment "soil" "" "")
    | "resource" `T.isInfixOf` lc = Just (Compartment "natural resource" "" "")
    | otherwise = Nothing
  where
    lc = T.toCaseFold comp

-- | Extract methodology from "# methodology: ..." comment.
extractMethodology :: [BS.ByteString] -> Maybe Text
extractMethodology = go
  where
    go [] = Nothing
    go (l : ls)
        | BC.isPrefixOf "# methodology:" l =
            Just . T.strip . TE.decodeUtf8With TEE.lenientDecode $ BS.drop 15 l
        | otherwise = go ls

-- | Strip trailing carriage return (Windows CRLF line endings).
stripCR :: BS.ByteString -> BS.ByteString
stripCR bs = fromMaybe bs (BS.stripSuffix "\r" bs)

-- | Strip UTF-8 BOM if present (common in Windows-created CSV files).
stripBOM :: BS.ByteString -> BS.ByteString
stripBOM bs
    | BS.isPrefixOf "\xEF\xBB\xBF" bs = BS.drop 3 bs
    | otherwise = bs

-- | Convert Text to bytes for UUID5 key generation.
bsKey :: Text -> [Word8]
bsKey = BS.unpack . TE.encodeUtf8

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
@substance@ and @compartment@ are required, in that order. Two more key
columns are recognised (in either order — this is what 'Method.WriterCSV'
emits): @cas@ (the substance's CAS number) and @unit@ (the flow unit of
that row, when it differs from the category unit in the header — real
methods mix kg, m3 and MJ flows inside one category).

A compartment cell is either a legacy prose form (@Emissions to air@,
@Resources@ — matched by keyword) or a @/@-separated path
(@water/groundwater/long-term@ = compartment, subcompartment, qualifier),
which is what the writer emits and the only way to keep subcompartment
distinctions — most of a real method's factors are subcompartment-specific.

One CSV file → multiple 'Method' values (one per column).
-}
module Method.ParserCSV (
    parseMethodCSVBytes,
    knownTops,
    stripBOM,
) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (elemIndex, zip4)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID5
import Data.Word (Word8)

import Method.CSV (detectDelimiter, parseDouble, splitRow)
import Method.Types

-- | Pure parser, exported for testing.
parseMethodCSVBytes :: BS.ByteString -> Either String [Method]
parseMethodCSVBytes bytes =
    let allLines = map stripCR $ BC.lines (stripBOM bytes)
        (comments, dataLines) = span (\l -> BC.isPrefixOf "#" l || BS.null l) allLines
        methodology = extractMethodology comments
     in do
            (keys, cats, names, units, rows, delim) <- splitHeaderFromData dataLines
            -- Split each data row once; every method column reads the same cells.
            let cellRows = map (splitRow delim) rows
            Right
                [ mkMethod methodology keys catName impactName unit colIdx cellRows
                | (colIdx, catName, impactName, unit) <- zip4 [0 ..] cats names units
                , not (T.null impactName)
                ]

{- | The key columns of the label row: the non-blank leading cells. Data
columns start right after them.
-}
data KeyColumns = KeyColumns
    { kcCount :: !Int
    , kcCAS :: !(Maybe Int)
    , kcUnit :: !(Maybe Int)
    }

{- | Read the key columns off the label row. @substance@ and @compartment@
must come first (that's also what 'isLabelRow' anchors on); @cas@ and @unit@
are optional. An unrecognised key column is an error, not a silently skipped
column of data.
-}
parseKeyColumns :: [Text] -> Either String KeyColumns
parseKeyColumns labels =
    let keys = map (T.toCaseFold . T.strip) (takeWhile (not . T.null . T.strip) labels)
     in case keys of
            ("substance" : "compartment" : extras) ->
                case filter (`notElem` ["cas", "unit"]) extras of
                    bad : _ -> Left ("unknown key column in CSV method label row: " <> T.unpack bad)
                    [] ->
                        Right
                            KeyColumns
                                { kcCount = length keys
                                , kcCAS = (+ 2) <$> elemIndex "cas" extras
                                , kcUnit = (+ 2) <$> elemIndex "unit" extras
                                }
            _ -> Left "CSV method label row must start with 'substance;compartment'"

{- | Find the label row (first column starts with "substance") and split header
rows from data rows.  Returns (keys, categories, names, units, dataRows,
delimiter).  2-row layout → categories = names.  3-row layout → distinct.
-}
splitHeaderFromData :: [BS.ByteString] -> Either String (KeyColumns, [Text], [Text], [Text], [BS.ByteString], Char)
splitHeaderFromData dataLines =
    case break isLabelRow dataLines of
        (headerRows, labelRow : rows) -> do
            let !delim = detectDelimiter labelRow
            keys <- parseKeyColumns (splitRow delim labelRow)
            let dropKeys = drop (kcCount keys) . splitRow delim
            case filter (not . BS.null) headerRows of
                [nameRow, unitRow] ->
                    let names = dropKeys nameRow
                     in Right (keys, names, names, dropKeys unitRow, rows, delim)
                [catRow, nameRow, unitRow] ->
                    Right (keys, dropKeys catRow, dropKeys nameRow, dropKeys unitRow, rows, delim)
                _ -> Left "CSV method file needs at least 3 header rows (names, units, column labels)"
        _ -> Left "CSV method file has no 'substance;compartment;...' label row"

-- | Detect the label row: first cell is a variation of "substance".
isLabelRow :: BS.ByteString -> Bool
isLabelRow line =
    case splitRow (detectDelimiter line) line of
        (first : _) -> T.toCaseFold (T.strip first) == "substance"
        [] -> False

-- | Build a single Method from one column of the pre-split CSV rows.
mkMethod :: Maybe Text -> KeyColumns -> Text -> Text -> Text -> Int -> [[Text]] -> Method
mkMethod methodology keys catName impactName unit colIdx cellRows =
    let !ns = csvMethodNamespace
        !mId = UUID5.generateNamed ns (bsKey $ "method:" <> impactName)
        !headerUnit = if T.null unit then "unknown" else unit
        cell cells i = case drop i cells of
            x : _ -> T.strip x
            [] -> ""
        keyed cells = maybe "" (cell cells)
        !factors =
            [ MethodCF
                { mcfFlowRef = UUID5.generateNamed ns (bsKey $ sub <> "::" <> comp)
                , mcfFlowName = sub
                , mcfDirection = directionFromCompartment comp compartment
                , mcfValue = v
                , mcfCompartment = compartment
                , mcfCAS = if T.null cas then Nothing else Just cas
                , mcfUnit = if T.null rowUnit then headerUnit else rowUnit
                , mcfConsumerLocation = Nothing
                }
            | cells <- cellRows
            , let !sub = cell cells 0
                  !comp = cell cells 1
                  !cas = keyed cells (kcCAS keys)
                  !rowUnit = keyed cells (kcUnit keys)
                  !raw = cell cells (colIdx + kcCount keys)
            , not (T.null sub)
            , not (T.null raw)
            , let compartment = parseCSVCompartment comp
            , Just v <- [parseDouble raw]
            ]
     in Method
            { methodId = mId
            , methodName = impactName
            , methodDescription = Nothing
            , methodUnit = headerUnit
            , methodCategory = catName
            , methodMethodology = methodology
            , methodFactors = factors
            }

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
    [single] -> legacyCompartment single
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
knownTops = ["air", "water", "soil", "natural resource", "land occupation", "land transformation"]

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

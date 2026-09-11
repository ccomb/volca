{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for the Brightway Excel (@.xlsx@) inventory interchange format.

The layout is the one produced and consumed by Brightway's @bw2io@ @ExcelImporter@:
a single (or multiple) worksheet holding a linear stream of blocks separated by
blank rows —

@
Database              <database name>
Activity              <activity name>
production amount     1
reference product     <product>
location              GLO
unit                  megajoule
Exchanges
name   amount   reference product   location   unit   categories   type           database
...    1        <product>           GLO        MJ                  production     <db>
...    0.5      <ecoinvent product> RoW        MJ                  technosphere   ecoinvent-...
Water  1.6e-4                       GLO        m3     air          biosphere      ecoinvent-...
@

Section keywords live in column A (case-insensitive); a blank row terminates a
section; an @Exchanges@ row introduces a labelled table whose first row is the
column headers. Columns are addressed by header label, not position, because the
column order varies between files.

The reader is deliberately small: @.xlsx@ is a zip of XML parts, unzipped with
@zip-archive@ and parsed with the @xeno@ DOM already used elsewhere in the
engine. openpyxl (Brightway's writer) emits inline strings (@t="inlineStr"@),
so there is usually no shared-string table; we still resolve one if present.

Domain construction reuses the SimaPro helpers (UUID generation, compartment
normalization, unit canonicalization) so Brightway activities, flows and units
hash identically to the other importers and link through the same
cross-database pass. This is the one format that names the supplier activity in
a column apart from its product, so a technosphere row keeps that name
(as a 'ClaimByName') for the linker to match on ahead of the product name.
-}
module BrightwayExcel.Parser (
    parseBrightwayExcel,

    -- * Exposed for testing
    CellValue (..),
    Row,
    readSheets,
    parseSheetXml,
    sheetToActivities,
    skippedSheetWarning,
    splitCategories,
) where

import Amount (readAmount)
import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry, toArchiveOrFail)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiUpper, ord, toUpper)
import Data.Indexing (collisions, repeated)
import Data.List (find, findIndex, partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import EcoSpold.Common (numericRefChar)
import Progress (ProgressLevel (..), reportProgress)
import SimaPro.Parser (
    canonicalRow,
    generateFlowUUID,
    generateUnitUUID,
    indexFlows,
    normalizeSimaProCompartment,
 )
import Types
import qualified UnitConversion as UC
import Xeno.DOM (Content (..), Node)
import qualified Xeno.DOM as X

-- ---------------------------------------------------------------------------
-- Cell / row model
-- ---------------------------------------------------------------------------

-- | A spreadsheet cell value, already typed at read time.
data CellValue
    = CellText !Text
    | CellNumber !Double
    deriving (Eq, Show)

{- | A row is a sparse map from 0-based column index to value. Empty cells are
absent, so an empty 'M.Map' is an empty row (Brightway's @is_empty_line@).
-}
type Row = M.Map Int CellValue

cellText :: CellValue -> Maybe Text
cellText = \case
    CellText t -> let s = T.strip t in if T.null s then Nothing else Just s
    CellNumber _ -> Nothing

cellNum :: CellValue -> Maybe Double
cellNum = \case
    CellNumber d -> Just d
    CellText t -> readAmount t

textAt :: Int -> Row -> Maybe Text
textAt i row = cellText =<< M.lookup i row

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

{- | Parse a Brightway Excel workbook into the same 5-tuple the other importers
return: activities plus the deduplicated technosphere / biosphere / waste flow
and unit databases. Cross-database links are resolved later by the shared
name-based pass in "Database.Loader" / "Database.Manager".
-}
parseBrightwayExcel ::
    UC.UnitConfig ->
    FilePath ->
    IO (Either Text ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB))
parseBrightwayExcel cfg path = do
    raw <- BL.readFile path
    case readSheets raw of
        Left err -> pure (Left ("Brightway Excel: " <> err))
        Right sheets -> do
            let (dataSheets, skipped) = partition (validFirstCell . snd) sheets
                blocks = concatMap (mapMaybe parseBlock . activityBlocks . snd) dataSheets
                results = map (rawToActivity cfg) blocks
                activities = [a | (a, _, _, _, _) <- results]
                techFlows = concat [fs | (_, fs, _, _, _) <- results]
                bioFlows = concat [fs | (_, _, fs, _, _) <- results]
                units = concat [us | (_, _, _, us, _) <- results]
                warnings = concat [ws | (_, _, _, _, ws) <- results] ++ mapMaybe skippedSheetWarning skipped
                unitDB = M.fromList [(unitId u, u) | u <- units]
                unitNames = M.map unitName unitDB
            mapM_ (reportProgress Warning . T.unpack) warnings
            pure $ do
                mapM_ refuseConflictingMeta blocks
                techFlowDB <- indexFlows unitNames (\f -> (tfId f, tfUnitId f, tfName f)) techFlows
                bioFlowDB <- indexFlows unitNames (\f -> (bfId f, bfUnitId f, bfName f)) bioFlows
                pure (activities, techFlowDB, bioFlowDB, M.empty, unitDB)

{- | A worksheet is imported only when its first cell (A1) is a non-empty string
that is not the sentinel @"skip"@ (matches @bw2io@'s @valid_first_cell@).
-}
validFirstCell :: [Row] -> Bool
validFirstCell rows = case listToMaybe rows >>= textAt 0 of
    Just t -> T.toLower t /= "skip"
    Nothing -> False

{- | Warn when a worksheet that /carries data/ is dropped because its first cell
(A1) is blank. Brightway ignores the whole sheet on a blank A1, so a mistyped
or shifted header silently loses every activity below it — surface it instead.
The deliberate @"skip"@ sentinel and genuinely empty sheets are left silent.
-}
skippedSheetWarning :: (Text, [Row]) -> Maybe Text
skippedSheetWarning (name, rows)
    | isNothing (listToMaybe rows >>= textAt 0) && not (all M.null rows) =
        Just $
            "worksheet '"
                <> name
                <> "' ignored: its first cell (A1) is blank, so the whole sheet is skipped; "
                <> "set A1 to 'Database' or 'Activity' to import it"
    | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Block grammar (pure)
-- ---------------------------------------------------------------------------

{- | A parsed activity block: its name, metadata key/value pairs, the exchange
table header (column index → lowercased label) and the data rows.
-}
data RawActivity = RawActivity
    { raName :: !Text
    , raMetaPairs :: ![(Text, CellValue)]
    {- ^ Every metadata row the block wrote, in order, the way 'raHeaders'
    carries every column its header row wrote. 'metaOf' is the map.
    -}
    , raHeaders :: ![(Int, Text)]
    , raRows :: ![Row]
    , raHasParams :: !Bool
    }

{- | The metadata a block states, one value per key.

A key stated twice with one value is one answer; a key stated twice with two is
refused before this is read, by 'conflictingMeta'.
-}
metaOf :: RawActivity -> M.Map Text CellValue
metaOf = M.fromList . raMetaPairs

{- | The metadata keys a block answers two ways, with the answers.

A metadata key fixes the whole activity - its location, its unit, its reference
product - so two values for one key name no activity, and nothing in the block
says which was meant. A repeated column is not the same case: it carries one
field of one exchange row, and the reader is told which column and goes on.
-}
conflictingMeta :: RawActivity -> [(Text, NE.NonEmpty CellValue)]
conflictingMeta ra =
    [ (key, values)
    | (key, values) <- collisions (raMetaPairs ra)
    , NE.length (NE.nub values) > 1
    ]

-- | Refuse a block that answers one metadata key two ways, naming both answers.
refuseConflictingMeta :: RawActivity -> Either Text ()
refuseConflictingMeta ra = case conflictingMeta ra of
    [] -> Right ()
    ((key, values) : _) ->
        Left $
            "Brightway Excel: activity '"
                <> raName ra
                <> "' states '"
                <> key
                <> "' more than once, as "
                <> T.intercalate " and " (map spelt (NE.toList values))
                <> "; nothing says which is meant"
  where
    spelt :: CellValue -> Text
    spelt v = case v of
        CellText t -> T.strip t
        CellNumber n -> T.pack (show n)

sectionKeyword :: Row -> Maybe Text
sectionKeyword row = T.toLower . T.strip <$> textAt 0 row

isSectionStart :: Row -> Bool
isSectionStart row = sectionKeyword row `elem` map Just ["activity", "database", "project parameters"]

isActivityStart :: Row -> Bool
isActivityStart row = sectionKeyword row == Just "activity" && M.member 1 row

rowKeyIs :: Text -> Row -> Bool
rowKeyIs kw row = sectionKeyword row == Just kw

{- | Split a sheet into activity blocks: each starts at an @Activity@ row and runs
up to (not including) the next section start.
-}
activityBlocks :: [Row] -> [[Row]]
activityBlocks = go
  where
    go [] = []
    go (r : rs)
        | isActivityStart r =
            let (blk, rest) = break isSectionStart rs
             in (r : blk) : go rest
        | otherwise = go rs

{- | Parse one block into a 'RawActivity', dropping blank rows first (Brightway
strips empty lines within an activity before locating its sub-sections).
-}
parseBlock :: [Row] -> Maybe RawActivity
parseBlock block0 = do
    let block = filter (not . M.null) block0
    actRow <- listToMaybe block
    name <- textAt 1 actRow
    let excIdx = findIndex (rowKeyIs "exchanges") block
        parIdx = findIndex (rowKeyIs "parameters") block
        metaEnd = minimum (length block : catMaybes [excIdx, parIdx])
        metaRows = take (metaEnd - 1) (drop 1 block)
        metaPairs =
            [ (T.toLower (T.strip k), v)
            | row <- metaRows
            , Just k <- [textAt 0 row]
            , Just v <- [M.lookup 1 row]
            ]
        -- Everything after the Exchanges header is taken as exchange data. This
        -- assumes a @parameters@ section (if any) precedes Exchanges, as bw2io
        -- writes it; a trailing parameters block would be read as exchange rows
        -- and rejected by their missing @type@ (a warning, never a silent drop).
        (headers, dataRows) = case excIdx of
            Nothing -> ([], [])
            Just i -> case drop (i + 1) block of
                (h : rest) -> (rowHeaders h, rest)
                [] -> ([], [])
    pure
        RawActivity
            { raName = T.strip name
            , raMetaPairs = metaPairs
            , raHeaders = headers
            , raRows = dataRows
            , raHasParams = isJust parIdx
            }

-- | Column index → lowercased header label for the non-empty header cells.
rowHeaders :: Row -> [(Int, Text)]
rowHeaders row =
    [ (i, T.toLower lbl)
    | (i, cv) <- M.toList row
    , Just lbl <- [cellText cv]
    ]

-- | Project a data row onto its labelled fields.
rowFields :: [(Int, Text)] -> Row -> M.Map Text CellValue
rowFields headers row =
    M.fromList [(lbl, v) | (i, lbl) <- headers, Just v <- [M.lookup i row]]

{- | The labels a header row spells more than once. 'rowFields' keys on the
label, so one column of each survives per row and which one is whichever
carries a value; the reader is told which labels rather than losing a column in
silence.
-}
duplicateLabels :: [(Int, Text)] -> [Text]
duplicateLabels headers = repeated [lbl | (_, lbl) <- headers]

-- ---------------------------------------------------------------------------
-- Domain construction (pure)
-- ---------------------------------------------------------------------------

-- | The exchange (if any), flows and units produced by one row, plus warnings.
data RowOut = RowOut
    { roExch :: !(Maybe Exchange)
    , roTech :: ![TechnosphereFlow]
    , roBio :: ![BiosphereFlow]
    , roUnit :: ![Unit]
    , roWarn :: ![Text]
    }

emptyRowOut :: RowOut
emptyRowOut = RowOut Nothing [] [] [] []

-- | All activities (with their flows/units/warnings) from one worksheet.
sheetToActivities ::
    UC.UnitConfig ->
    [Row] ->
    [(Activity, [TechnosphereFlow], [BiosphereFlow], [Unit], [Text])]
sheetToActivities cfg = map (rawToActivity cfg) . mapMaybe parseBlock . activityBlocks

rawToActivity ::
    UC.UnitConfig ->
    RawActivity ->
    (Activity, [TechnosphereFlow], [BiosphereFlow], [Unit], [Text])
rawToActivity cfg ra =
    (activity, techFlows, bioFlows, units, warnings)
  where
    meta = metaOf ra
    fieldRows = map (rowFields (raHeaders ra)) (raRows ra)
    isType t r = (T.toLower <$> fieldText r "type") == Just t
    prodRows = filter (isType "production") fieldRows
    otherRows = filter (not . isType "production") fieldRows

    prodOuts = case prodRows of
        [] -> [productRowOut cfg meta True M.empty | M.member "reference product" meta]
        (r : rs) -> productRowOut cfg meta True r : map (productRowOut cfg meta False) rs
    otherOuts = map (exchangeRowOut cfg (raName ra)) otherRows
    allOuts = prodOuts ++ otherOuts

    exchanges' = mapMaybe roExch allOuts
    techFlows = concatMap roTech allOuts
    bioFlows = concatMap roBio allOuts
    units = concatMap roUnit allOuts

    refExch = find exchangeIsReference exchanges'
    refUnitName = case refExch of
        Just ex -> maybe metaUnit unitName (find ((== exchangeUnitId ex) . unitId) units)
        Nothing -> metaUnit
    metaUnit = fromMaybe "" (metaText meta "unit")

    warnings =
        concatMap roWarn allOuts
            ++ [ "activity '" <> raName ra <> "': parameters section ignored (not supported)"
               | raHasParams ra
               ]
            ++ [ "activity '" <> raName ra <> "': multiple production rows; coproducts are emitted without allocation"
               | length prodRows > 1
               ]
            ++ [ "activity '" <> raName ra <> "': no reference product; activity will not be scoreable"
               | not (any exchangeIsReference exchanges')
               ]
            ++ [ "activity '" <> raName ra <> "': column '" <> lbl <> "' appears more than once; one of them is read per row and the others are dropped"
               | lbl <- duplicateLabels (raHeaders ra)
               ]

    location = fromMaybe "" (metaText meta "location")
    activity =
        Activity
            { activityName = raName ra
            , activityDescription = maybeToList (metaText meta "comment")
            , activityDocumentation = [] -- A workbook records no source dossier
            , activitySynonyms = M.empty
            , activityClassification = M.empty
            , activityLocation = location
            , activityLocationSource = declaredLocationSource location
            , activityUnit = refUnitName
            , exchanges = exchanges'
            , activityParams = M.empty
            , activityParamExprs = M.empty
            , activityNativeType = Nothing
            , activityNativeId = Nothing
            , activityFormulaCheck = Nothing
            }

{- | Build the reference-product (or coproduct) exchange from a @production@ row,
falling back to the activity metadata for any field the row omits.

The /reference/ product is normalized to its canonical base unit at ingest (e.g.
@g@ → @kg@, scaling the amount), exactly like the SimaPro importer. This makes
the importer non-injective on the reference unit: a database whose reference unit
is non-canonical does not satisfy @parse (write d) == d@. The writer's contract
is instead fixed-point over the parser's /image/ — once a database has been
parsed (so its reference unit is canonical), @parse (write d) == d@ holds. A
coproduct row is left in its stated unit (no canonicalization), so it round-trips
verbatim.
-}
productRowOut :: UC.UnitConfig -> M.Map Text CellValue -> Bool -> M.Map Text CellValue -> RowOut
productRowOut cfg meta isRef f =
    RowOut (Just exch) [flow] [] [unit] []
  where
    name =
        fromMaybe "(unknown product)" $
            fieldText f "reference product" <|> fieldText f "name" <|> metaText meta "reference product"
    rawUnit = fromMaybe "" (fieldText f "unit" <|> metaText meta "unit")
    rawAmount = fromMaybe 1 (fieldNum f "amount" <|> metaNum meta "production amount")
    (effUnit, effAmount) = canonicalRow cfg rawUnit rawAmount
    flowUUID = generateFlowUUID name ""
    unitUUID = generateUnitUUID effUnit
    exch =
        TechnosphereExchange
            { techFlowId = flowUUID
            , techAmount = effAmount
            , techUnitId = unitUUID
            , techRole = if isRef then ReferenceProduct else Coproduct
            , techActivityLinkId = Nothing
            , techSupplierClaim = ClaimByProduct -- an output of this activity, not a reference to another
            , techLocation = fromMaybe "" (fieldText f "location" <|> metaText meta "location")
            , techComment = fieldText f "comment"
            , techPedigree = Nothing
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }
    flow = TechnosphereFlow flowUUID name unitUUID M.empty Nothing Nothing
    unit = Unit unitUUID effUnit effUnit ""

-- | Build a technosphere or biosphere exchange from a non-production row.
exchangeRowOut :: UC.UnitConfig -> Text -> M.Map Text CellValue -> RowOut
exchangeRowOut cfg actName f =
    case T.toLower <$> fieldText f "type" of
        Just "technosphere" -> technosphereRowOut cfg Input actName f
        Just "substitution" -> technosphereRowOut cfg AvoidedProduct actName f
        Just "biosphere" -> biosphereRowOut cfg actName f
        Just other ->
            emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped exchange with unrecognized type '" <> other <> "'"]}
        Nothing ->
            emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped exchange row with no type"]}

{- | A technosphere row keyed by the supplier's /reference product/ name (the
key 'Database.Loader.buildSupplierIndexByName' matches against), with the
supplier location preserved for geography-aware cross-DB linking: an 'Input'
for a @technosphere@ row, an 'AvoidedProduct' for a @substitution@ row.

A zero amount is kept, like every other amount and like the SimaPro importer:
it says the author disabled this input, which is a statement about the model,
not an empty row.

The @name@ column, which holds the supplier /activity/, is kept alongside in
the row's 'SupplierClaim'. It is what tells @market group for electricity, medium
voltage@ from the 26 other activities of a released background database whose
reference product is also @electricity, medium voltage@ in GLO.
-}
technosphereRowOut :: UC.UnitConfig -> TechRole -> Text -> M.Map Text CellValue -> RowOut
technosphereRowOut cfg role actName f
    | T.null name = emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped technosphere row with no name"]}
    | isNothing (fieldNum f "amount") =
        emptyRowOut{roWarn = ["activity '" <> actName <> "', row '" <> name <> "': skipped, its amount cell holds no number"]}
    | otherwise = RowOut (Just exch) [flow] [] [unit] []
  where
    name = fromMaybe "" (fieldText f "reference product" <|> fieldText f "name")
    (unitName', amount) = canonicalRow cfg (fromMaybe "" (fieldText f "unit")) (fromMaybe 0 (fieldNum f "amount"))
    flowUUID = generateFlowUUID name ""
    unitUUID = generateUnitUUID unitName'
    exch =
        TechnosphereExchange
            { techFlowId = flowUUID
            , techAmount = amount
            , techUnitId = unitUUID
            , techRole = role
            , techActivityLinkId = Nothing
            , techSupplierClaim = maybe ClaimByProduct ClaimByName supplierActivity
            , techLocation = fromMaybe "" (fieldText f "location")
            , techComment = fieldText f "comment"
            , techPedigree = Nothing
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }
    flow = TechnosphereFlow flowUUID name unitUUID M.empty Nothing Nothing
    unit = Unit unitUUID unitName' unitName' ""

    {- The supplier this row names, kept only where it says something the product
    name does not: with no @reference product@ column the @name@ cell /is/ the
    product, and a row that repeats the product in both has nothing to add. -}
    supplierActivity :: Maybe Text
    supplierActivity = do
        rowName <- fieldText f "name"
        _ <- fieldText f "reference product"
        if rowName == name then Nothing else Just rowName

{- | A biosphere exchange. @categories@ (split on @::@) becomes the compartment;
a @natural resource@ compartment is read as an extraction ('Resource'),
everything else as an 'Emission'. Flow UUIDs use the same normalized
compartment string as the SimaPro importer so LCIA characterization matches.
-}
biosphereRowOut :: UC.UnitConfig -> Text -> M.Map Text CellValue -> RowOut
biosphereRowOut cfg actName f
    | T.null name = emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped biosphere row with no name"]}
    | otherwise = RowOut (Just exch) [] [flow] [unit] mediumWarn
  where
    name = fromMaybe "" (fieldText f "name")
    (unitName', amount) = canonicalRow cfg (fromMaybe "" (fieldText f "unit")) (fromMaybe 0 (fieldNum f "amount"))
    (comp, sub) = splitCategories (fromMaybe "" (fieldText f "categories"))
    flowUUID = generateFlowUUID name (normalizeSimaProCompartment comp sub)
    unitUUID = generateUnitUUID unitName'
    exch =
        BiosphereExchange
            { bioFlowId = flowUUID
            , bioAmount = amount
            , bioUnitId = unitUUID
            , bioDirection = if parseMedium comp == Right NaturalResource then Resource else Emission
            , bioLocation = fromMaybe "" (fieldText f "location")
            , bioComment = fieldText f "comment"
            , bioPedigree = Nothing
            }
    flow =
        BiosphereFlow
            { bfId = flowUUID
            , bfName = name
            , bfUnitId = unitUUID
            , bfSynonyms = M.empty
            , bfCAS = Nothing
            , bfSubstanceId = Nothing
            , bfCompartment = case parseMedium comp of
                Right medium -> Just (Compartment medium (if T.null sub then Nothing else Just sub))
                Left _ -> Nothing
            }
    -- A medium nothing can bucket leaves the flow without a compartment, which
    -- costs it every characterization factor. Say which word did it.
    mediumWarn = case parseMedium comp of
        Right _ -> []
        Left got ->
            [ "activity '" <> actName <> "', flow '" <> name <> "': " <> T.pack (unknownMedium got)
            ]
    unit = Unit unitUUID unitName' unitName' ""

{- | Split a Brightway @categories@ cell (@"air"@, @"air::urban air"@) into
(compartment, subcompartment).
-}
splitCategories :: Text -> (Text, Text)
splitCategories cats = case T.splitOn "::" cats of
    [] -> ("", "")
    [a] -> (T.strip a, "")
    (a : rest) -> (T.strip a, T.strip (T.intercalate "::" rest))

fieldText :: M.Map Text CellValue -> Text -> Maybe Text
fieldText m k = cellText =<< M.lookup k m

fieldNum :: M.Map Text CellValue -> Text -> Maybe Double
fieldNum m k = cellNum =<< M.lookup k m

metaText :: M.Map Text CellValue -> Text -> Maybe Text
metaText = fieldText

metaNum :: M.Map Text CellValue -> Text -> Maybe Double
metaNum = fieldNum

-- ---------------------------------------------------------------------------
-- Workbook reader (zip + XML)
-- ---------------------------------------------------------------------------

{- | Unzip an @.xlsx@ and return its worksheets, in workbook order, paired with
their declared names (so a skipped sheet can be named in a warning).
-}
readSheets :: BL.ByteString -> Either Text [(Text, [Row])]
readSheets raw = do
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
        sheetRefs =
            [ (sheetName, TE.decodeUtf8 rid)
            | sheetsNode <- maybeToList (firstChildNamed "sheets" workbook)
            , s <- childrenNamed "sheet" sheetsNode
            , Just rid <- [attr "r:id" s]
            , let sheetName = maybe (TE.decodeUtf8 rid) TE.decodeUtf8 (attr "name" s)
            ]
    traverse (\(name, rid) -> (,) name <$> loadSheet archive relMap sharedStrings rid) sheetRefs

loadSheet :: Archive -> M.Map Text Text -> Maybe (V.Vector Text) -> Text -> Either Text [Row]
loadSheet archive relMap sharedStrings rid = do
    target <- maybe (Left ("unknown worksheet relationship " <> rid)) Right (M.lookup rid relMap)
    bytes <- entry archive (resolveTarget target)
    parseSheetXml sharedStrings bytes

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
parseSheetXml sharedStrings bs = do
    root <- parseXml bs
    pure
        [ parseRow sharedStrings row
        | sheetData <- maybeToList (firstChildNamed "sheetData" root)
        , row <- childrenNamed "row" sheetData
        ]

parseRow :: Maybe (V.Vector Text) -> Node -> Row
parseRow sharedStrings row =
    M.fromList
        [ (col, value)
        | c <- childrenNamed "c" row
        , Just ref <- [attr "r" c]
        , Just col <- [columnIndex ref]
        , Just value <- [cellValue sharedStrings c]
        ]

-- | Decode one @\<c\>@ cell, honouring its @t@ type attribute.
cellValue :: Maybe (V.Vector Text) -> Node -> Maybe CellValue
cellValue sharedStrings c = case TE.decodeUtf8 <$> attr "t" c of
    Just "inlineStr" -> nonEmptyText . nodeText =<< firstChildNamed "is" c
    Just "s" -> do
        idx <- readInt . nodeText =<< firstChildNamed "v" c
        table <- sharedStrings
        nonEmptyText =<< (table V.!? idx)
    Just "str" -> nonEmptyText . nodeText =<< firstChildNamed "v" c
    Just "b" -> CellText . nodeText <$> firstChildNamed "v" c
    _ -> do
        v <- firstChildNamed "v" c
        let txt = nodeText v
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

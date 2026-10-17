{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for the Brightway Excel (@.xlsx@) inventory interchange format.

The layout is the one produced and consumed by Brightway's @bw2io@ @ExcelImporter@:
a single (or multiple) worksheet holding a linear stream of blocks separated by
blank rows –

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
Brightway's own export writes no @reference product@ column on exchanges: a row
bought from another database then names its supplier activity alone (a
'ClaimByNameAlone').
-}
module BrightwayExcel.Parser (
    parseBrightwayExcel,
    Workbook (..),

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
import Control.Monad (mfilter)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiUpper, ord, toUpper)
import Data.Either (partitionEithers)
import Data.Indexing (collisions, repeated)
import Data.List (find, findIndex, partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import EcoSpold.Common (numericRefChar)
import qualified Expr
import Progress (ProgressLevel (..), reportProgress)
import SimaPro.Parser (
    ParamLevel (..),
    ParamRow (..),
    bioFlowInUnit,
    buildParamEnv,
    canonicalFactor,
    canonicalRow,
    generateFlowUUID,
    generateUnitUUID,
    indexFlows,
    normalizeSimaProCompartment,
    techFlowInUnit,
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

-- | What a workbook reads to: its activities, the flows and units they name, and the parameters it defines above its activities.
data Workbook = Workbook
    { wbActivities :: ![Activity]
    , wbTechFlows :: !TechFlowDB
    , wbBioFlows :: !BioFlowDB
    , wbUnits :: !UnitDB
    , wbParameters :: !(M.Map Text Parameter)
    {- ^ Its project's and its database's, a database parameter replacing the
    project one of its name, as Brightway reads them.
    -}
    }

{- | Parse a Brightway Excel workbook into its activities, their deduplicated
technosphere and biosphere flows and units, and its parameters.
Cross-database links are resolved later by the shared name-based pass in
"Database.Loader" / "Database.Manager".
-}
parseBrightwayExcel :: UC.UnitConfig -> FilePath -> IO (Either Text Workbook)
parseBrightwayExcel cfg path = do
    raw <- BL.readFile path
    case readSheets raw of
        Left err -> pure (Left ("Brightway Excel: " <> err))
        Right sheets -> do
            let (dataSheets, skipped) = partition (validFirstCell . snd) sheets
                WorkbookRows{wrResults = results, wrBlocks = blocks, wrParameters = parameters, wrWarnings = globalWarnings} = readRows cfg (map snd dataSheets)
                activities = [a | (a, _, _, _, _) <- results]
                techFlows = concat [fs | (_, fs, _, _, _) <- results]
                bioFlows = concat [fs | (_, _, fs, _, _) <- results]
                units = concat [us | (_, _, _, us, _) <- results]
                warnings = globalWarnings ++ concat [ws | (_, _, _, _, ws) <- results] ++ mapMaybe skippedSheetWarning skipped
                unitDB = M.fromList [(unitId u, u) | u <- units]
                unitNames = M.map unitName unitDB
            mapM_ (reportProgress Warning . T.unpack) warnings
            pure $ do
                mapM_ refuseConflictingMeta blocks
                techFlowDB <- indexFlows unitNames techFlowInUnit tfName techFlows
                bioFlowDB <- indexFlows unitNames bioFlowInUnit bfName bioFlows
                pure
                    Workbook
                        { wbActivities = activities
                        , wbTechFlows = techFlowDB
                        , wbBioFlows = bioFlowDB
                        , wbUnits = unitDB
                        , wbParameters = snd (buildParamEnv Expr.Python (wpGlobals parameters))
                        }

-- | What the worksheets of a workbook read to.
data WorkbookRows = WorkbookRows
    { wrResults :: ![(Activity, [TechnosphereFlow], [BiosphereFlow], [Unit], [Text])]
    -- ^ Each activity with its flows, units and warnings.
    , wrBlocks :: ![RawActivity]
    , wrParameters :: !WorkbookParameters
    , wrWarnings :: ![Text]
    -- ^ What reading the parameters above the activities raised.
    }

-- | The parameters a workbook defines outside any one activity.
data WorkbookParameters = WorkbookParameters
    { wpGlobals :: ![ParamLevel]
    -- ^ The project's, then the databases'.
    , wpGroups :: !(M.Map Text ParamLevel)
    -- ^ Each named group's, gathered from every activity that names it.
    }

readRows :: UC.UnitConfig -> [[Row]] -> WorkbookRows
readRows cfg sheets =
    WorkbookRows
        { wrResults = map (rawToActivity cfg (Own (ownDatabases sheets blocks) madeHere) parameters) blocks
        , wrBlocks = blocks
        , wrParameters = parameters
        , wrWarnings = globalWarnings ++ groupWarnings ++ globalValueless ++ groupValueless ++ madeHereClashes
        }
  where
    (madeHere, madeHereClashes) = madeHereNames blocks
    blocks :: [RawActivity]
    blocks = concatMap (mapMaybe parseBlock . activityBlocks) sheets
    (globals, globalWarnings) = workbookLevels sheets
    (groups, groupWarnings) = groupLevels blocks
    parameters :: WorkbookParameters
    parameters = WorkbookParameters globals groups
    globalValueless :: [Text]
    globalValueless = concat [valueless whose (take n globals) (levelCalculated level) | (n, whose, level) <- zip3 [1 :: Int ..] ["project", "database"] globals]
    groupValueless :: [Text]
    groupValueless = concat (M.mapWithKey (\group level -> valueless ("group '" <> group <> "'") (globals ++ [level]) (levelCalculated level)) groups)

{- | The parameters above every activity: the project's, then the databases',
each replacing a project parameter of its name.
-}
workbookLevels :: [[Row]] -> ([ParamLevel], [Text])
workbookLevels sheets = (map fst levels, concatMap snd levels)
  where
    levels :: [(ParamLevel, [Text])]
    levels =
        [ parameterLevel whose (concatMap (sectionTables keyword) sheets)
        | (whose, keyword) <- [("project", "project parameters"), ("database", "database parameters")]
        ]

-- | Each named group's parameters, gathered from every activity that names the group.
groupLevels :: [RawActivity] -> (M.Map Text ParamLevel, [Text])
groupLevels blocks = (M.map fst levels, concatMap snd (M.elems levels))
  where
    levels :: M.Map Text (ParamLevel, [Text])
    levels =
        M.mapWithKey
            (\group -> parameterLevel ("group '" <> group <> "'"))
            (M.fromListWith (flip (++)) [(group, [table]) | RawActivity{raParameters = Just (SharedGroup group, table)} <- blocks])

-- | The calculated parameters given whose formula gives no value in the levels given, with why: a formula reading them gets none either.
valueless :: Text -> [ParamLevel] -> [ParamRow] -> [Text]
valueless whose levels calculated =
    [ whose <> " parameter '" <> parName r <> "': its formula " <> parValue r <> " gives no value (" <> Expr.describeRefusal refusal <> ")"
    | r <- calculated
    , S.notMember (T.toLower (parName r)) valued
    , Left refusal <- [Expr.evaluate Expr.Python env (parValue r)]
    ]
  where
    env :: M.Map Text Double
    env = fst (buildParamEnv Expr.Python levels)
    valued :: S.Set Text
    valued = S.map T.toLower (M.keysSet env)

{- | Every table a worksheet opens with the keyword given: the header row after
it, then the rows down to a blank row or the next section.
-}
sectionTables :: Text -> [Row] -> [ParameterTable]
sectionTables keyword rows = case break (rowKeyIs keyword) rows of
    (_, _ : header : rest) ->
        let (body, after) = break (\row -> M.null row || isSectionStart row) rest
         in ParameterTable (rowHeaders header) body : sectionTables keyword after
    _ -> []

{- | One level of parameters from its tables: a row with a formula is calculated
from it, a row with only an amount is an input of that amount.
-}
parameterLevel :: Text -> [ParameterTable] -> (ParamLevel, [Text])
parameterLevel whose tables = (ParamLevel inputs calculated, warnings)
  where
    (warnings, kept) = foldMap (parameterRow whose . uncurry rowFields) [(ptHeaders t, r) | t <- tables, r <- ptRows t]
    (inputs, calculated) = partitionEithers kept

-- | One parameter row: an input on the left, a calculated parameter on the right, or nothing and why.
parameterRow :: Text -> M.Map Text CellValue -> ([Text], [Either ParamRow ParamRow])
parameterRow whose f = case (fieldText f "name", fieldText f "formula", fieldNum f "amount") of
    (Nothing, _, _) -> ([whose <> " parameters: skipped a row with no name"], [])
    (Just name, Just formula, Just amount)
        | Left refusal <- Expr.translate Expr.Python formula ->
            ( [whose <> " parameter '" <> name <> "': its formula " <> formula <> " is left aside (" <> Expr.describeRefusal refusal <> "); its amount is read"]
            , [Left (row name (T.pack (show amount)) Nothing)]
            )
    (Just name, Just formula, _) -> ([], [Right (row name formula Nothing)])
    (Just name, Nothing, Just amount) ->
        let (warnings, spread') = uncertaintyOf (whose <> " parameter '" <> name <> "'") amount f
         in (warnings, [Left (row name (T.pack (show amount)) spread')])
    (Just name, Nothing, Nothing) -> ([whose <> " parameter '" <> name <> "': skipped, it has neither an amount nor a formula"], [])
  where
    row :: Text -> Text -> Maybe Uncertainty -> ParamRow
    row name value spread' =
        ParamRow
            { parName = name
            , parValue = value
            , parComment = fromMaybe "" (fieldText f "description" <|> fieldText f "label")
            , parUncertainty = spread'
            , parUnit = fieldText f "unit"
            }

{- | The distribution a row's columns state, by its uncertainty type: 2
lognormal, whose @loc@ and @scale@ are the logarithms of its geometric mean and
deviation; 3 normal; 4 uniform; 5 triangular, whose @loc@ is the most likely
value. 0 and 1 state no spread. A lognormal whose @loc@ is not the logarithm
of the amount is left out and said: its @loc@ is then most likely the amount
itself. So is a distribution missing a cell it needs, and one of a type the
engine has no shape for.
-}
uncertaintyOf :: Text -> Double -> M.Map Text CellValue -> ([Text], Maybe Uncertainty)
uncertaintyOf what amount f = case fieldNum f "uncertainty type" of
    Nothing -> ([], Nothing)
    Just 0 -> ([], Nothing)
    Just 1 -> ([], Nothing)
    Just 2 -> case (fieldNum f "loc", fieldNum f "scale") of
        (Just loc, Just scale)
            | Expr.agrees (exp loc) (abs amount) -> ([], spreading (Lognormal (signum amount * exp loc) (exp scale)))
            | otherwise -> leftOut ("its loc " <> T.pack (show loc) <> " is not the logarithm of its amount " <> T.pack (show amount))
        _ -> incomplete "lognormal"
    Just 3 -> stated "normal" (Normal <$> fieldNum f "loc" <*> fieldNum f "scale")
    Just 4 -> stated "uniform" (Uniform <$> fieldNum f "minimum" <*> fieldNum f "maximum")
    Just 5 -> stated "triangular" (Triangular <$> fieldNum f "minimum" <*> fieldNum f "loc" <*> fieldNum f "maximum")
    Just other -> ([what <> ": its uncertainty type " <> T.pack (show (round other :: Integer)) <> " is left out, the engine has no such distribution"], Nothing)
  where
    stated :: Text -> Maybe Uncertainty -> ([Text], Maybe Uncertainty)
    stated name = maybe (incomplete name) (\u -> ([], spreading u))
    incomplete :: Text -> ([Text], Maybe Uncertainty)
    incomplete name = ([what <> ": its " <> name <> " distribution is left out, a cell it needs is empty"], Nothing)
    leftOut :: Text -> ([Text], Maybe Uncertainty)
    leftOut why = ([what <> ": its lognormal distribution is left out, " <> why], Nothing)

{- | A worksheet is imported only when its first cell (A1) is a non-empty string
that is not the sentinel @"skip"@ (matches @bw2io@'s @valid_first_cell@).
-}
validFirstCell :: [Row] -> Bool
validFirstCell rows = case listToMaybe rows >>= textAt 0 of
    Just t -> T.toLower t /= "skip"
    Nothing -> False

{- | Warn when a worksheet that /carries data/ is dropped because its first cell
(A1) is blank. Brightway ignores the whole sheet on a blank A1, so a mistyped
or shifted header silently loses every activity below it – surface it instead.
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
    , raParameters :: !(Maybe (ParameterGroup, ParameterTable))
    -- ^ The group its @Parameters@ section names, and the parameters it defines there.
    }

{- | Whose parameters an activity's @Parameters@ section defines: a group it
names, which other activities may name too, or, naming none, its own alone.
-}
data ParameterGroup = SharedGroup !Text | OwnGroup

-- | A parameter table as a workbook writes one: its column labels and its rows.
data ParameterTable = ParameterTable
    { ptHeaders :: ![(Int, Text)]
    , ptRows :: ![Row]
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
isSectionStart row = sectionKeyword row `elem` map Just ["activity", "database", "project parameters", "database parameters"]

isActivityStart :: Row -> Bool
isActivityStart row = sectionKeyword row == Just "activity" && M.member 1 row

rowKeyIs :: Text -> Row -> Bool
rowKeyIs kw row = sectionKeyword row == Just kw

{- | Split a sheet into activity blocks: each starts at an @Activity@ row and runs
up to (not including) the next section start.

Among the activity's fields, before its @Exchanges@ or @Parameters@ row, a row
keyed @activity@ in lower case is a field: Brightway writes every field under
its own key, and an activity copied from an EcoSpold 2 import keeps its
@activity@ identifier, while the section itself is written @Activity@.
-}
activityBlocks :: [Row] -> [[Row]]
activityBlocks = go
  where
    go [] = []
    go (r : rs)
        | isActivityStart r =
            let (fields, body) = break endsFields rs
                (blk, rest) = break isSectionStart body
             in (r : fields ++ blk) : go rest
        | otherwise = go rs
    endsFields :: Row -> Bool
    endsFields row = rowKeyIs "exchanges" row || rowKeyIs "parameters" row || (isSectionStart row && textAt 0 row /= Just "activity")

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
        -- The table runs from the header after @Parameters@ to @Exchanges@.
        parameters = do
            i <- parIdx
            start <- listToMaybe (drop i block)
            header : rows <- Just (drop (i + 1) (take (fromMaybe (length block) excIdx) block))
            pure (maybe OwnGroup SharedGroup (textAt 1 start), ParameterTable (rowHeaders header) rows)
    pure
        RawActivity
            { raName = T.strip name
            , raMetaPairs = metaPairs
            , raHeaders = headers
            , raRows = dataRows
            , raParameters = parameters
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
sheetToActivities cfg rows = wrResults (readRows cfg [rows])

-- | A block's exchange rows, each keyed by its column labels.
blockFields :: RawActivity -> [M.Map Text CellValue]
blockFields ra = map (rowFields (raHeaders ra)) (raRows ra)

isProductionRow :: M.Map Text CellValue -> Bool
isProductionRow f = (T.toLower <$> fieldText f "type") == Just "production"

{- | The databases a workbook writes its own activities into: those its
@Database@ sections name and those its production rows name. A technosphere row
naming any other database asks for a product made there.
-}
newtype OwnDatabases = OwnDatabases (S.Set Text)

ownDatabases :: [[Row]] -> [RawActivity] -> OwnDatabases
ownDatabases sheets blocks =
    OwnDatabases . S.fromList $
        [name | row <- concat sheets, rowKeyIs "database" row, Just name <- [textAt 1 row]]
            ++ [name | ra <- blocks, f <- blockFields ra, isProductionRow f, Just name <- [fieldText f "database"]]

{- | The names a technosphere row may give a product the workbook makes, each
with the product it means: the name its production row gives it, or the name of
the activity making it as its reference, which is what Brightway's export
writes. A row giving none of them buys from another database. An activity name
standing for two products stands for neither, and is named in a warning.
-}
madeHereNames :: [RawActivity] -> (M.Map Text Text, [Text])
madeHereNames blocks = (M.union products (M.mapMaybe theOne byActivity), M.foldMapWithKey clash (M.withoutKeys byActivity (M.keysSet products)))
  where
    products :: M.Map Text Text
    products = M.fromSet id (S.fromList [productName (metaOf ra) isRef f | ra <- blocks, (isRef, f) <- zip (True : repeat False) (productionFields ra)])
    byActivity :: M.Map Text (S.Set Text)
    byActivity = M.fromListWith S.union [(raName ra, S.singleton (productName (metaOf ra) True f)) | ra <- blocks, f <- take 1 (productionFields ra)]
    theOne :: S.Set Text -> Maybe Text
    theOne made = if S.size made == 1 then S.lookupMin made else Nothing
    clash :: Text -> S.Set Text -> [Text]
    clash name made
        | S.size made > 1 =
            ["activities named '" <> name <> "' make " <> T.intercalate ", " (map (\p -> "'" <> p <> "'") (S.toList made)) <> ": a row naming '" <> name <> "' alone is read as bought from another database"]
        | otherwise = []

{- | An activity's production rows. With none, Brightway counts one unit of the
product its fields name, which the empty row stands for.
-}
productionFields :: RawActivity -> [M.Map Text CellValue]
productionFields ra = case filter isProductionRow (blockFields ra) of
    [] -> [M.empty | M.member "reference product" (metaOf ra)]
    rows -> rows

{- | The product a production row makes. A row with no @reference product@
column names the activity itself, as Brightway's export writes it, so the
activity's own field names its reference product before the row's name does.
-}
productName :: M.Map Text CellValue -> Bool -> M.Map Text CellValue -> Text
productName meta isRef f =
    fromMaybe "(unknown product)" (fieldText f "reference product" <|> (if isRef then metaText meta "reference product" else Nothing) <|> fieldText f "name")

{- | Where a technosphere row says its product is made: in this workbook, or in
the database its @database@ column names when that is none of the workbook's
own. A row naming no database is made here, as every row was read before.
-}
data MadeIn = MadeHere | MadeIn !Text

{- | What the workbook makes: the databases it writes into, and the names a
row may give one of its products ('madeHereNames').
-}
data Own = Own !OwnDatabases !(M.Map Text Text)

madeIn :: OwnDatabases -> M.Map Text CellValue -> MadeIn
madeIn (OwnDatabases own) f = maybe MadeHere MadeIn (mfilter (`S.notMember` own) (fieldText f "database"))

{- | The flow a product row designates. A product made in another database is
not the product of this workbook spelt the same way: another activity makes it,
often counted in another unit (a coke weighed here in kilograms, bought there in
megajoules), and one flow for both refused the whole workbook for its two
units. The database takes the place a biosphere flow gives its compartment, so
a product made here keeps the identity the other readers give it.
-}
productFlowUUID :: MadeIn -> Text -> UUID
productFlowUUID = \case
    MadeHere -> (`generateFlowUUID` "")
    MadeIn db -> (`generateFlowUUID` ("database " <> db))

-- | What every exchange row of one block is read against.
data RowContext = RowContext
    { rcUnits :: !UC.UnitConfig
    , rcOwn :: !Own
    , rcActivity :: !Text
    -- ^ The activity the row belongs to, named in every warning it raises.
    , rcEnv :: !(M.Map Text Double)
    -- ^ The values of the parameters its formulas read.
    }

-- | A row's amount, the formula that gives it in the engine's language, and what reading them raised.
data AmountReading = AmountReading
    { arValue :: !(Maybe Double)
    , arFormula :: !(Maybe Text)
    , arWarnings :: ![Text]
    }

{- | Read a row's amount.
Brightway writes 0, or the value it last computed, in the amount column of a row
with a formula, so the formula's value at the parameters' values is the amount.
A formula that does not read, or reads a name with no value, leaves the amount
column to say it. A formula that is a number keeps no formula.
-}
rowAmount :: RowContext -> Text -> M.Map Text CellValue -> AmountReading
rowAmount RowContext{rcEnv = env, rcActivity = actName} rowName f = case fieldText f "formula" of
    Nothing -> AmountReading written Nothing []
    Just raw -> case (,) <$> Expr.translate Expr.Python raw <*> Expr.evaluate Expr.Python env raw of
        Left refusal ->
            AmountReading written Nothing [whose <> ": its formula " <> raw <> " is left aside (" <> Expr.describeRefusal refusal <> "); its amount column is read"]
        Right (text, value) ->
            AmountReading
                (Just value)
                (if isJust (readAmount raw) then Nothing else Just text)
                [ whose <> ": its formula gives " <> T.pack (show value) <> " where its amount column says " <> T.pack (show w) <> "; the formula's value is read"
                | Just w <- [written]
                , w /= 0
                , not (Expr.agrees w value)
                ]
  where
    written :: Maybe Double
    written = fieldNum f "amount"
    whose :: Text
    whose = "activity '" <> actName <> "', row '" <> rowName <> "'"

-- | A row in the reference unit of its dimension: the unit, the amount, and the formula carried there.
data Measure = Measure
    { measureUnit :: !Text
    , measureAmount :: !Double
    , measureFormula :: !(Maybe ExchangeFormula)
    }

inReferenceUnit :: UC.UnitConfig -> Text -> Double -> Maybe Text -> Measure
inReferenceUnit cfg unit amount formula =
    Measure unit' amount' (ExchangeFormula Nothing . Expr.times (canonicalFactor cfg unit) <$> formula)
  where
    (unit', amount') = canonicalRow cfg unit amount

rawToActivity ::
    UC.UnitConfig ->
    Own ->
    WorkbookParameters ->
    RawActivity ->
    (Activity, [TechnosphereFlow], [BiosphereFlow], [Unit], [Text])
rawToActivity cfg own WorkbookParameters{wpGlobals = globals, wpGroups = groups} ra =
    (activity, techFlows, bioFlows, units, warnings)
  where
    meta = metaOf ra
    ctx = RowContext cfg own (raName ra) env

    -- The activity's own parameters are its group's; the levels above are the workbook's.
    (group, groupWarnings) = case raParameters ra of
        Nothing -> (Nothing, [])
        Just (SharedGroup name, _) -> (M.lookup name groups, [])
        Just (OwnGroup, table) ->
            let (level, ws) = parameterLevel ("activity '" <> raName ra <> "'") [table]
             in (Just level, ws ++ valueless ("activity '" <> raName ra <> "'") (globals ++ [level]) (levelCalculated level))
    (env, defined) = buildParamEnv Expr.Python (globals ++ maybeToList group)
    groupNames = S.fromList [T.toLower (parName r) | level <- maybeToList group, r <- levelInputs level ++ levelCalculated level]
    fieldRows = blockFields ra
    prodRows = filter isProductionRow fieldRows
    otherRows = filter (not . isProductionRow) fieldRows

    prodOuts = zipWith (productRowOut ctx meta) (True : repeat False) (productionFields ra)
    otherOuts = map (exchangeRowOut ctx) otherRows
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
        groupWarnings
            ++ concatMap roWarn allOuts
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
            , activityParameters = M.filterWithKey (\name _ -> S.member (T.toLower name) groupNames) defined
            , activityNativeType = Nothing
            , activityNativeId = Nothing
            , activityFormulaCheck = Nothing
            , activityDates = noDates
            }

{- | Build the reference-product (or coproduct) exchange from a @production@ row,
falling back to the activity metadata for any field the row omits.

The /reference/ product is normalized to its canonical base unit at ingest (e.g.
@g@ → @kg@, scaling the amount), exactly like the SimaPro importer. This makes
the importer non-injective on the reference unit: a database whose reference unit
is non-canonical does not satisfy @parse (write d) == d@. The writer's contract
is instead fixed-point over the parser's /image/ – once a database has been
parsed (so its reference unit is canonical), @parse (write d) == d@ holds, for a
database whose products are all made in the workbook: the writer does not keep
the database a product made elsewhere came from ('productFlowUUID'). A
coproduct row is left in its stated unit (no canonicalization), so it round-trips
verbatim.
-}
productRowOut :: RowContext -> M.Map Text CellValue -> Bool -> M.Map Text CellValue -> RowOut
productRowOut ctx@RowContext{rcUnits = cfg} meta isRef f =
    RowOut (Just exch) [flow] [] [unit] (arWarnings reading)
  where
    name = productName meta isRef f
    rawUnit = fromMaybe "" (fieldText f "unit" <|> metaText meta "unit")
    reading = rowAmount ctx name f
    rawAmount = fromMaybe 1 (arValue reading <|> metaNum meta "production amount")
    Measure{measureUnit = effUnit, measureAmount = effAmount, measureFormula = keptFormula} = inReferenceUnit cfg rawUnit rawAmount (arFormula reading)
    flowUUID = productFlowUUID MadeHere name
    unitUUID = generateUnitUUID effUnit
    exch =
        TechnosphereExchange
            { techFlowId = flowUUID
            , techAmount = effAmount
            , techUnitId = unitUUID
            , techRole = if isRef then ReferenceProduct else Coproduct
            , techActivityLinkId = Nothing
            , techSupplierClaim = ClaimByProduct -- an output of this activity, not a reference to another
            , techLocation = readExchangeLocation (fromMaybe "" (fieldText f "location" <|> metaText meta "location"))
            , techComment = fieldText f "comment"
            , techPedigree = Nothing
            , techFormula = keptFormula
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }
    flow = TechnosphereFlow flowUUID name unitUUID M.empty Nothing Nothing
    unit = Unit unitUUID effUnit effUnit ""

-- | Build a technosphere or biosphere exchange from a non-production row.
exchangeRowOut :: RowContext -> M.Map Text CellValue -> RowOut
exchangeRowOut ctx@RowContext{rcActivity = actName} f =
    case T.toLower <$> fieldText f "type" of
        Just "technosphere" -> technosphereRowOut ctx Input f
        Just "substitution" -> technosphereRowOut ctx AvoidedProduct f
        Just "biosphere" -> biosphereRowOut ctx f
        Just other ->
            emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped exchange with unrecognized type '" <> other <> "'"]}
        Nothing ->
            emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped exchange row with no type"]}

{- | A technosphere row keyed by the supplier's /reference product/ name (the
key 'Database.Loader.buildSupplierIndexByName' matches against), with the
supplier location preserved for geography-aware cross-DB linking: an 'Input'
for a @technosphere@ row, an 'AvoidedProduct' for a @substitution@ row. A row
whose @database@ column names another database than the workbook's own asks for
a flow of its own ('productFlowUUID').

A zero amount is kept, like every other amount and like the SimaPro importer:
it says the author disabled this input, which is a statement about the model,
not an empty row.

The @name@ column, which holds the supplier /activity/, is kept alongside in
the row's 'SupplierClaim'. It is what tells @market group for electricity, medium
voltage@ from the 26 other activities of a released background database whose
reference product is also @electricity, medium voltage@ in GLO.
-}
technosphereRowOut :: RowContext -> TechRole -> M.Map Text CellValue -> RowOut
technosphereRowOut ctx@RowContext{rcUnits = cfg, rcOwn = Own own madeHere, rcActivity = actName} role f
    | T.null name = emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped technosphere row with no name"]}
    | isNothing (arValue reading) =
        emptyRowOut{roWarn = arWarnings reading ++ ["activity '" <> actName <> "', row '" <> name <> "': skipped, its amount cell holds no number"]}
    | otherwise = RowOut (Just exch) [flow] [] [unit] (arWarnings reading)
  where
    name = case (fieldText f "reference product", fieldText f "name", madeIn own f) of
        (Just stated, _, _) -> stated
        (Nothing, Just rowName, MadeHere) -> M.findWithDefault rowName rowName madeHere
        (Nothing, rowName, _) -> fromMaybe "" rowName
    reading = rowAmount ctx name f
    Measure{measureUnit = unitName', measureAmount = amount, measureFormula = keptFormula} = inReferenceUnit cfg (fromMaybe "" (fieldText f "unit")) (fromMaybe 0 (arValue reading)) (arFormula reading)
    flowUUID = productFlowUUID (madeIn own f) name
    unitUUID = generateUnitUUID unitName'
    exch =
        TechnosphereExchange
            { techFlowId = flowUUID
            , techAmount = amount
            , techUnitId = unitUUID
            , techRole = role
            , techActivityLinkId = Nothing
            , techSupplierClaim = supplierClaim
            , techLocation = readExchangeLocation (fromMaybe "" (fieldText f "location"))
            , techComment = fieldText f "comment"
            , techPedigree = Nothing
            , techFormula = keptFormula
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }
    flow = TechnosphereFlow flowUUID name unitUUID M.empty Nothing Nothing
    unit = Unit unitUUID unitName' unitName' ""

    {- The supplier this row names, kept only where it says something the product
    name does not. With no @reference product@ column the @name@ cell keys a
    product made here, or the activity making it, and is the supplier activity
    of a product bought elsewhere, as Brightway means it; a row that repeats the
    product in both columns has nothing to add. -}
    supplierClaim :: SupplierClaim
    supplierClaim = case fieldText f "name" of
        Just rowName
            | boughtElsewhere && isNothing (fieldText f "reference product") -> ClaimByNameAlone rowName
            | rowName /= name -> ClaimByName rowName
        _ -> ClaimByProduct
    boughtElsewhere :: Bool
    boughtElsewhere = case madeIn own f of
        MadeIn _ -> True
        MadeHere -> M.notMember (fromMaybe "" (fieldText f "name")) madeHere

{- | A biosphere exchange. @categories@ (split on @::@) becomes the compartment;
a @natural resource@ compartment is read as an extraction ('Resource'),
everything else as an 'Emission'. Flow UUIDs use the same normalized
compartment string as the SimaPro importer so LCIA characterization matches.
-}
biosphereRowOut :: RowContext -> M.Map Text CellValue -> RowOut
biosphereRowOut ctx@RowContext{rcUnits = cfg, rcActivity = actName} f
    | T.null name = emptyRowOut{roWarn = ["activity '" <> actName <> "': skipped biosphere row with no name"]}
    | otherwise = RowOut (Just exch) [] [flow] [unit] (arWarnings reading ++ mediumWarn)
  where
    name = fromMaybe "" (fieldText f "name")
    reading = rowAmount ctx name f
    Measure{measureUnit = unitName', measureAmount = amount, measureFormula = keptFormula} = inReferenceUnit cfg (fromMaybe "" (fieldText f "unit")) (fromMaybe 0 (arValue reading)) (arFormula reading)
    (comp, sub) = splitCategories (fromMaybe "" (fieldText f "categories"))
    flowUUID = generateFlowUUID name (normalizeSimaProCompartment comp sub)
    unitUUID = generateUnitUUID unitName'
    exch =
        BiosphereExchange
            { bioFlowId = flowUUID
            , bioAmount = amount
            , bioUnitId = unitUUID
            , bioDirection = if parseMedium comp == Right NaturalResource then Resource else Emission
            , bioLocation = readExchangeLocation (fromMaybe "" (fieldText f "location"))
            , bioComment = fieldText f "comment"
            , bioPedigree = Nothing
            , bioFormula = keptFormula
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

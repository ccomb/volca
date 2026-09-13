{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Writer for the Brightway Excel (@.xlsx@) inventory interchange format – the
inverse of "BrightwayExcel.Parser".

It serializes a 'SimpleDatabase' (the natural writer input, obtained from a
'Database' via 'toSimpleDatabase') back into the linear block-stream layout that
@bw2io@'s @ExcelImporter@ – and our own parser – consumes:

@
Database              <database name>

Activity              <activity name>
production amount     1
reference product     <product>
location              GLO
unit                  kilogram
Exchanges
name   amount   reference product   location   unit   categories   type           database
...    1        <product>           GLO        kg                  production     <db>
...    0.5      <supplier product>  RoW        kg                  technosphere   <db>
Water  1.6e-4                       GLO        m3     air           biosphere      <db>

@

== Determinism

The output is canonical and deterministic: activities are emitted sorted by
@(name, location)@, exchanges in a fixed role order (reference product, then
coproducts, then technosphere inputs, then biosphere flows – each group sorted
by flow name), and a single fixed @Exchanges@ column order is used regardless of
how the source file was laid out. Numbers are rendered with 'formatAmount' so
@1.0@ and @8.5@ are stable. The only volatile field – the workbook-level
database name – is supplied explicitly via 'WriterConfig', so a round-trip never
depends on ambient state (timestamps, tool version, machine).

== Encoding

An @.xlsx@ is a zip of XML parts. We mirror the parser's toolchain exactly
(@zip-archive@ + hand-built SpreadsheetML, the same shape openpyxl/bw2io emit),
rather than pulling in a new dependency: one worksheet @xl/worksheets/sheet1.xml@
of inline-string / numeric cells, wired through @xl/workbook.xml@ and its rels,
and announced by the OPC package manifest (@[Content_Types].xml@ and
@_rels/.rels@) that every reader but our own parser opens the archive through.
Inline strings (@t="inlineStr"@) are used throughout, so no shared-string table
is needed – and the parser already resolves either form.

Byte-identical round-trips are not a goal (zip stores per-entry metadata): the
contract proven by the spec is /logical-cell/ idempotence – re-exporting the
parsed content yields the same workbook, and 'parseBrightwayExcel' of the output
is structurally equal to the input.
-}
module BrightwayExcel.Writer (
    WriterConfig (..),
    defaultWriterConfig,
    writeBrightwayExcel,
    renderWorkbook,
    checkBrightwayExportable,
    wasteManifest,

    -- * Exposed for testing
    Cell (..),
    activityRows,
    formatAmount,
    renderCategories,
) where

import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Amount (readAmount)
import EcoSpold.Common (showFFloatTrim)
import Types
import Zip (zipFiles)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

{- | The only export-time choice that is not derived from the database content:
the workbook-level @Database@ name written at the top of the sheet and into each
exchange's @database@ column. Pinning it here (rather than reading a clock or a
build version) is what makes round-trips reproducible.
-}
newtype WriterConfig = WriterConfig
    { wcDatabaseName :: Text
    }
    deriving (Eq, Show)

-- | A neutral default name for ad-hoc exports.
defaultWriterConfig :: WriterConfig
defaultWriterConfig = WriterConfig{wcDatabaseName = "exported-database"}

-- ---------------------------------------------------------------------------
-- Cell model (mirrors the parser's CellValue / the spec emitter)
-- ---------------------------------------------------------------------------

-- | A cell to emit: text, a number, or an empty (omitted) cell.
data Cell
    = CText !Text
    | CNum !Double
    | CEmpty
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

{- | Serialize a 'SimpleDatabase' to a Brightway @.xlsx@ workbook on disk, or
return the export guard's 'Left' without touching disk.
-}
writeBrightwayExcel :: WriterConfig -> SimpleDatabase -> FilePath -> IO (Either Text ())
writeBrightwayExcel cfg db path =
    case renderWorkbook cfg db of
        Left err -> pure (Left err)
        Right bytes -> Right <$> BL.writeFile path bytes

-- | Pure: assemble the full @.xlsx@ byte stream from the database.
renderWorkbook :: WriterConfig -> SimpleDatabase -> Either Text BL.ByteString
renderWorkbook cfg db = do
    checkBrightwayExportable db
    pure $
        zipFiles
            [ ("[Content_Types].xml", enc contentTypesXml)
            , ("_rels/.rels", enc packageRelsXml)
            , ("xl/workbook.xml", enc workbookXml)
            , ("xl/_rels/workbook.xml.rels", enc relsXml)
            , ("xl/worksheets/sheet1.xml", enc (sheetXml (sheetRows cfg db)))
            ]
  where
    enc = TE.encodeUtf8

{- | Guard a Brightway Excel export against exchanges the writer cannot encode.
'exchangeRow' resolves each exchange's flow name and unit name from the database
maps; when either lookup misses, the row is 'Nothing' and dropped from the sheet,
silently losing an amount-bearing exchange. This check rejects such a database at
the export boundary instead, reporting the first offending activity and whether a
flow or a unit is missing.

Brightway also has no native waste exchange type. A 'WasteExchange' that links to
a producer is emitted as @technosphere@ only at the cost of its sign: the matrix
gives a waste /output/ a negative coefficient ('Database.MatrixBuild.techTriple'),
but the re-parsed technosphere row is read back as a positive 'Input', inverting
it. A /linked/ waste exchange is therefore rejected here. An /orphan/ waste
exchange (no producer link) never enters the matrix, so 'exchangeRow' instead
best-efforts it as technosphere and 'wasteManifest' reports it.

A biosphere exchange's 'BioDirection' is likewise never written: the parser
re-derives it from the @categories@ compartment, reading 'Resource' only for
'NaturalResource'. A 'Resource' flow of any other medium would round-trip as an
'Emission' – a sign flip, since the two directions act as input vs output. Such
a flow is rejected here too.

An amount that does not re-parse to itself is rejected. The written decimal must
re-parse through 'Amount.readAmount' (the importer's correctly-rounded reader);
every finite amount does, so this rejects only the non-finite @NaN@/@Infinity@
that would otherwise substitute a different value on re-import.

Databases whose exchanges all resolve, carry no linked waste, keep every resource
direction recoverable, and whose amounts all re-parse pass unchanged.
-}

{- | A waste exchange with no producer link: matrix-invisible, so 'exchangeRow'
rewrites it as a technosphere flow (best-effort) rather than rejecting it.
-}
orphanWaste :: Exchange -> Bool
orphanWaste ex = isWasteExchange ex && not (linkedWaste ex)

{- | Best-effort export note for a database with /orphan/ waste exchanges –
waste rows that name no producer at all. Brightway has no waste
type, so 'exchangeRow' writes each as a technosphere flow. Such an exchange never
participates in the technosphere matrix ('Database.MatrixBuild.findProducer'
returns 'Nothing'), so the rewrite is inventory-neutral; only the waste
classification is lost on re-import. (A /linked/ waste exchange would invert its
sign and is rejected by 'checkBrightwayExportable', so it never reaches here.)
Report which activities are affected so the loss is never silent.
-}
wasteManifest :: SimpleDatabase -> [Text]
wasteManifest db = case wasteActs of
    [] -> []
    _ -> [summary]
  where
    wasteActs = [activityName a | a <- M.elems (sdbActivities db), any orphanWaste (exchanges a)]
    summary =
        tshow (length wasteActs)
            <> " activit"
            <> (if length wasteActs == 1 then "y" else "ies")
            <> " with end-of-life waste exchanges: Brightway has no waste type, so each"
            <> " was written as a technosphere flow. These outputs carry no producer link,"
            <> " so the inventory result is unchanged – only the waste classification is"
            <> " lost on re-import: "
            <> T.intercalate ", " (take 10 wasteActs)
            <> (if length wasteActs > 10 then ", … and " <> tshow (length wasteActs - 10) <> " more" else "")

checkBrightwayExportable :: SimpleDatabase -> Either Text ()
checkBrightwayExportable db =
    case catMaybes
        [ flowMsg <$> listToMaybe flowOffenders
        , unitMsg <$> listToMaybe unitOffenders
        , wasteMsg <$> listToMaybe wasteOffenders
        , refInputMsg <$> listToMaybe refInputOffenders
        , directionMsg <$> listToMaybe directionOffenders
        , roundTripMsg <$> listToMaybe roundTripOffenders
        ] of
        [] -> Right ()
        violations -> Left (T.intercalate "\n\n" violations)
  where
    cannot consumer = "Brightway Excel export cannot represent activity \"" <> consumer <> "\": "
    flowMsg consumer = cannot consumer <> "an exchange references a flow absent from the database."
    unitMsg consumer = cannot consumer <> "an exchange references a unit absent from the registry."
    wasteMsg consumer =
        cannot consumer
            <> "a waste exchange links to a producer; Brightway has no waste type,"
            <> " and rewriting it as a technosphere flow would invert its sign on re-import."
    refInputMsg consumer =
        cannot consumer
            <> "a reference input (treatment process) has no Brightway encoding;"
            <> " it would round-trip to a duplicated, role-flipped exchange."
    directionMsg consumer = cannot consumer <> "a resource biosphere flow's compartment would re-parse as an emission."
    roundTripMsg (consumer, amt) =
        cannot consumer
            <> "exchange amount "
            <> tshow amt
            <> " does not re-parse to the same value (a non-finite amount)."
    -- Names of activities with at least one exchange satisfying @p@. Only the
    -- first offender is ever reported, so one entry per activity (not per
    -- exchange) is equivalent – and lets every guard share one comprehension.
    activitiesWith p =
        [activityName act | act <- M.elems (sdbActivities db), any p (exchanges act)]
    flowOffenders = activitiesWith (not . flowResolvable db)
    unitOffenders = activitiesWith (\ex -> M.notMember (exchangeUnitId ex) (sdbUnits db))
    wasteOffenders = activitiesWith linkedWaste
    refInputOffenders = activitiesWith isReferenceInput
    directionOffenders = activitiesWith (resourceDirectionLost db)
    roundTripOffenders =
        [ (activityName act, amt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , let amt = exchangeAmount ex
        , not (amountRoundTrips amt)
        ]
    amountRoundTrips amt = readAmount (formatAmount amt) == Just amt

{- | A 'Resource' biosphere exchange whose compartment would not re-parse as a
resource: the writer never records the direction, so the parser reconstructs it
from the @categories@ compartment, which names a resource only when the medium
is 'NaturalResource'. Under any other medium 'Resource' silently flips to
'Emission'. A flow absent from the map is left to 'flowResolvable' to report.
-}
resourceDirectionLost :: SimpleDatabase -> Exchange -> Bool
resourceDirectionLost db = \case
    ex@BiosphereExchange{bioDirection = Resource} ->
        case M.lookup (exchangeFlowId ex) (sdbBioFlows db) of
            Just flow -> (compartmentName <$> bfCompartment flow) /= Just NaturalResource
            Nothing -> False
    BiosphereExchange{bioDirection = Emission} -> False
    TechnosphereExchange{} -> False
    WasteExchange{} -> False

{- | Whether an exchange's flow is present in the map 'exchangeRow' reads it
from – the same per-role lookup as 'flowNameOf', so this predicts exactly the
rows the writer would drop.
-}
flowResolvable :: SimpleDatabase -> Exchange -> Bool
flowResolvable db ex = case ex of
    TechnosphereExchange{} -> M.member (exchangeFlowId ex) (sdbTechFlows db)
    WasteExchange{} -> M.member (exchangeFlowId ex) (sdbWasteFlows db)
    BiosphereExchange{} -> M.member (exchangeFlowId ex) (sdbBioFlows db)

-- ---------------------------------------------------------------------------
-- Sheet content (pure)
-- ---------------------------------------------------------------------------

{- | The whole worksheet as a list of rows. A leading @Database@ section, then
one activity block per activity, separated by blank rows. Activities are sorted
by @(name, location)@ for determinism.
-}
sheetRows :: WriterConfig -> SimpleDatabase -> [[Cell]]
sheetRows cfg db =
    [ [CText "Database", CText (wcDatabaseName cfg)]
    , []
    ]
        ++ concatMap (\a -> activityRows cfg db a ++ [[]]) sortedActivities
  where
    sortedActivities =
        sortOn (\a -> (activityName a, activityLocation a)) (M.elems (sdbActivities db))

-- | The fixed canonical @Exchanges@ table header.
exchangeHeader :: [Cell]
exchangeHeader =
    map
        CText
        ["name", "amount", "reference product", "location", "unit", "categories", "type", "comment", "database"]

{- | One activity block: the @Activity@ row, metadata key/value rows, the
@Exchanges@ header, then one row per exchange in canonical order. The metadata
@production amount@ / @unit@ / @reference product@ are taken from the activity's
reference exchange so a parse → write round-trip reproduces them, and the
@production amount@ fallback stays consistent with the reference row's amount.

'activityDescription' is newline-joined into the single @comment@ cell the
format allows. The parser reads it back as a one-element description
('Data.Maybe.maybeToList'), so an activity carrying a /multi-paragraph/
description is not a fixed point of @parse . write@ (the text survives; the
paragraph split does not). Once parsed it has ≤1 element and round-trips exactly
– the same "fixed-point over the parser's image" caveat the parser documents for
reference-unit canonicalization.
-}
activityRows :: WriterConfig -> SimpleDatabase -> Activity -> [[Cell]]
activityRows cfg db act =
    [ [CText "Activity", CText (activityName act)]
    , [CText "production amount", CNum refAmount]
    ]
        ++ [[CText "reference product", CText p] | p <- maybeToList refProduct]
        ++ [[CText "location", CText (activityLocation act)]]
        ++ [[CText "unit", CText u] | u <- maybeToList refUnit, not (T.null u)]
        ++ [[CText "comment", CText comment] | not (null (activityDescription act))]
        ++ [[CText "Exchanges"], exchangeHeader]
        ++ exchangeDataRows
  where
    comment = T.intercalate "\n" (activityDescription act)
    ordered = orderedExchanges db (exchanges act)
    exchangeDataRows = mapMaybe (exchangeRow cfg db) ordered
    refExchange = lookup' exchangeIsReference ordered
    refProduct = refExchange >>= flowNameOf db
    refUnit = (`unitNameOf` db) . exchangeUnitId =<< refExchange
    refAmount = maybe 1 exchangeAmount refExchange

{- | Canonical exchange order: reference product first, then coproducts, then
ordinary technosphere inputs, then biosphere flows – each group sorted by flow
name, with the flow id as a stable tiebreaker for same-named flows. Sorting by
name keeps the exported columns legible; the id tiebreaker keeps the order total
and deterministic, so two databases with the same content serialize identically.
-}
orderedExchanges :: SimpleDatabase -> [Exchange] -> [Exchange]
orderedExchanges db exs = concatMap (sortOn sortKey) groups
  where
    groups = [refs, coproducts, techInputs, bios, wastes]
    refs = filter exchangeIsReference exs
    coproducts = filter (\ex -> isCoproduct ex || isAvoidedProduct ex) exs
    techInputs = filter isTechInput exs
    bios = filter isBio exs
    wastes = filter isWaste exs
    sortKey ex = (flowNameOf db ex, exchangeFlowId ex)

isCoproduct :: Exchange -> Bool
isCoproduct = \case
    TechnosphereExchange{techRole = Coproduct} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

{- | Ordinary technosphere inputs only. 'ReferenceInput' is intentionally
excluded: it already belongs to the reference group ('exchangeIsReference'),
so matching it here too would emit the exchange twice – double-counting its
coefficient when the workbook is re-imported and duplicate (i,j) entries are
summed. Each role thus lands in exactly one group.
-}
isAvoidedProduct :: Exchange -> Bool
isAvoidedProduct = \case
    TechnosphereExchange{techRole = AvoidedProduct} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

isTechInput :: Exchange -> Bool
isTechInput = \case
    TechnosphereExchange{techRole = Input} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

isBio :: Exchange -> Bool
isBio = \case
    BiosphereExchange{} -> True
    TechnosphereExchange{} -> False
    WasteExchange{} -> False

isWaste :: Exchange -> Bool
isWaste = \case
    WasteExchange{} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False

-- | A treatment process's reference input – rejected by 'checkBrightwayExportable'.
isReferenceInput :: Exchange -> Bool
isReferenceInput = \case
    TechnosphereExchange{techRole = ReferenceInput} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

{- | Render one exchange to a data row aligned to 'exchangeHeader'. Returns
'Nothing' for an exchange whose flow or unit is missing from the database (it
cannot be written faithfully, so it is dropped rather than emitted with blanks
that would re-parse into a different flow). The @reference product@ column
carries the flow name, from which the parser reconstructs the same flow UUID;
@name@ carries the supplier activity where the source named one, and repeats the
flow name where it did not.
-}
exchangeRow :: WriterConfig -> SimpleDatabase -> Exchange -> Maybe [Cell]
exchangeRow cfg db = \case
    ex@TechnosphereExchange{techAmount = amt, techRole = role, techLocation = loc, techSupplierClaim = claim} -> do
        name <- flowNameOf db ex
        unit <- unitNameOf (exchangeUnitId ex) db
        Just
            [ CText (namedSupplier claim name)
            , CNum amt
            , CText name
            , locCell loc
            , CText unit
            , CEmpty
            , CText (techTypeLabel role)
            , commentCell (techComment ex)
            , CText (wcDatabaseName cfg)
            ]
    ex@BiosphereExchange{bioAmount = amt, bioLocation = loc} -> do
        flow <- M.lookup (exchangeFlowId ex) (sdbBioFlows db)
        unit <- unitNameOf (exchangeUnitId ex) db
        Just
            [ CText (bfName flow)
            , CNum amt
            , CEmpty
            , locCell loc
            , CText unit
            , CText (renderCategories (bfCompartment flow))
            , CText "biosphere"
            , commentCell (bioComment ex)
            , CText (wcDatabaseName cfg)
            ]
    ex@WasteExchange{waAmount = amt, waLocation = loc} -> do
        name <- flowNameOf db ex
        unit <- unitNameOf (exchangeUnitId ex) db
        -- Best-effort: Brightway has no waste type. Only orphan waste reaches here
        -- ('checkBrightwayExportable' rejects linked waste, which would sign-invert),
        -- and an orphan waste exchange never enters the technosphere matrix, so
        -- writing it as a technosphere flow is inventory-neutral; only the waste
        -- classification is lost on re-import. 'wasteManifest' reports the activities.
        Just
            [ CText name
            , CNum amt
            , CText name
            , locCell loc
            , CText unit
            , CEmpty
            , CText "technosphere"
            , commentCell (waComment ex)
            , CText (wcDatabaseName cfg)
            ]
  where
    locCell l = if T.null l then CEmpty else CText l
    commentCell = maybe CEmpty CText

{- | The @name@ column: the supplier activity where the source named one, and
the flow name where it designated by the product row instead.
-}
namedSupplier :: SupplierClaim -> Text -> Text
namedSupplier claim flowName = case claim of
    ClaimByName supplier -> supplier
    ClaimByProduct -> flowName
    ClaimById _ -> flowName
    ClaimByDatasetNumber _ -> flowName

-- | Brightway @type@ label for a technosphere role.
techTypeLabel :: TechRole -> Text
techTypeLabel = \case
    ReferenceProduct -> "production"
    Coproduct -> "production"
    AvoidedProduct -> "substitution"
    ReferenceInput -> "technosphere"
    Input -> "technosphere"

{- | Render a 'Compartment' to a Brightway @categories@ cell (@"air"@ or
@"natural resource::in water"@). 'Nothing' (no compartment recorded) and an
empty medium both become an empty cell – the inverse of 'splitCategories'.
-}
renderCategories :: Maybe Compartment -> Text
renderCategories = \case
    Nothing -> ""
    Just (Compartment comp sub) -> case sub of
        Just s | not (T.null (T.strip s)) -> mediumText comp <> "::" <> s
        _ -> mediumText comp

-- ---------------------------------------------------------------------------
-- Lookups (pure)
-- ---------------------------------------------------------------------------

-- | Resolve the display name of a technosphere/waste exchange's flow.
flowNameOf :: SimpleDatabase -> Exchange -> Maybe Text
flowNameOf db = \case
    ex@TechnosphereExchange{} -> tfName <$> M.lookup (exchangeFlowId ex) (sdbTechFlows db)
    ex@WasteExchange{} -> wfName <$> M.lookup (exchangeFlowId ex) (sdbWasteFlows db)
    ex@BiosphereExchange{} -> bfName <$> M.lookup (exchangeFlowId ex) (sdbBioFlows db)

unitNameOf :: UUID -> SimpleDatabase -> Maybe Text
unitNameOf uid db = unitName <$> M.lookup uid (sdbUnits db)

-- | First element matching a predicate (total; no partial 'head').
lookup' :: (a -> Bool) -> [a] -> Maybe a
lookup' p = foldr (\x acc -> if p x then Just x else acc) Nothing

-- ---------------------------------------------------------------------------
-- Numeric formatting (pure, deterministic)
-- ---------------------------------------------------------------------------

{- | Canonical numeric rendering for amount cells. Integers print without a
decimal point (@1@, not @1.0@); every other value uses the shared fixed-point
'showFFloatTrim' (never scientific), the exact inverse of 'Amount.readAmount':
every finite amount re-parses through that correctly-rounded reader. A
non-finite value has no numeric-cell form; 'checkBrightwayExportable' rejects it,
so the guarded path never emits one. A non-finite value still renders as
its (non-parseable) @"NaN"@/@"Infinity"@ form so a stray re-import fails loudly
rather than reading a misleading number.
-}
formatAmount :: Double -> Text
formatAmount d
    | isNaN d || isInfinite d = tshow d
    | d == fromIntegral i && abs d < 1.0e15 = T.pack (show i)
    | otherwise = T.pack (showFFloatTrim d)
  where
    i = round d :: Integer

-- ---------------------------------------------------------------------------
-- SpreadsheetML emitter (hand-built, mirrors the parser's reader)
-- ---------------------------------------------------------------------------

{- | The OPC package manifest. Our parser reads @xl/worksheets/sheet1.xml@
straight out of the zip, but openpyxl (and so @bw2io@, and Excel) resolves parts
through this map of extension and part name to content type, and fails with a
plain @KeyError@ when the archive has none.
-}
contentTypesXml :: Text
contentTypesXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
        <> "<Default Extension=\"rels\""
        <> " ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
        <> "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
        <> "<Override PartName=\"/xl/workbook.xml\""
        <> " ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
        <> "<Override PartName=\"/xl/worksheets/sheet1.xml\""
        <> " ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
        <> "</Types>"

-- | Package-level relationships: which part is the workbook.
packageRelsXml :: Text
packageRelsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> "<Relationship Id=\"rId1\""
        <> " Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\""
        <> " Target=\"xl/workbook.xml\"/>"
        <> "</Relationships>"

workbookXml :: Text
workbookXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\""
        <> " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
        <> "<sheets><sheet name=\"data\" sheetId=\"1\" r:id=\"rId1\"/></sheets>"
        <> "</workbook>"

relsXml :: Text
relsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> "<Relationship Id=\"rId1\""
        <> " Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\""
        <> " Target=\"worksheets/sheet1.xml\"/>"
        <> "</Relationships>"

sheetXml :: [[Cell]] -> Text
sheetXml rows =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>"
        <> T.concat [rowXml n r | (n, r) <- zip [1 ..] rows]
        <> "</sheetData></worksheet>"

rowXml :: Int -> [Cell] -> Text
rowXml n cells =
    "<row r=\""
        <> tshow n
        <> "\">"
        <> T.concat [cellXml col n cell | (col, cell) <- zip [0 ..] cells]
        <> "</row>"

-- | Empty cells are omitted entirely (sparse rows, like the parser expects).
cellXml :: Int -> Int -> Cell -> Text
cellXml _ _ CEmpty = ""
cellXml col n (CNum d) =
    "<c r=\"" <> cellRef col n <> "\" t=\"n\"><v>" <> formatAmount d <> "</v></c>"
cellXml col n (CText t) =
    "<c r=\""
        <> cellRef col n
        <> "\" t=\"inlineStr\"><is><t xml:space=\"preserve\">"
        <> escapeXml t
        <> "</t></is></c>"

-- | A1-style cell reference. Columns beyond Z use multi-letter references.
cellRef :: Int -> Int -> Text
cellRef col n = columnLetters col <> tshow n

columnLetters :: Int -> Text
columnLetters = T.pack . reverse . go
  where
    go c =
        let (q, r) = (c `div` 26, c `mod` 26)
            letter = chr (ord 'A' + r)
         in letter : if q == 0 then [] else go (q - 1)

escapeXml :: Text -> Text
escapeXml =
    T.replace "\"" "&quot;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

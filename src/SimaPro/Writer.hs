{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | SimaPro CSV Writer for volca – the inverse of "SimaPro.Parser".

Serializes an in-memory 'Database' / 'SimpleDatabase' back to a SimaPro CSV
export. The output is /canonical/ and /deterministic/: given the same database
(modulo a pinned header version) it always produces byte-identical bytes.

Determinism is achieved by:

  * sorting every activity by (name, location) and every section's rows by
    (flow name, unit, signed amount), so 'Map'/'Set' iteration order never
    leaks into the output;
  * a single fixed numeric formatter ('formatAmount') that round-trips through
    the parser's 'parseAmount' / 'Expr.normalizeExpr';
  * pinning the only volatile header field – the SimaPro version banner – via
    'WriterConfig'. No export timestamp or generator line is emitted, so a
    write→parse→write cycle is stable.

Encoding/layout mirrors the parser's expectations:

  * semicolon-separated fields, CRLF line endings;
  * dot decimal separator (matching @{Decimal separator: .}@);
  * the @{SimaPro …}@ / @{CSV separator: Semicolon}@ / @{Decimal separator: .}@
    header block;
  * one @Process … End@ block per activity, with the metadata keys and section
    headers the parser recognises.

Field shapes are kept aligned with the parser's row parsers
('parseProductRow', 'parseTechRow', 'parseBioRow') so a faithful round-trip is
possible. CSV escaping reuses the same rule as 'Matrix.Export.escapeCsvField'.

== Known lossy round-trips

The writer is a faithful inverse for /SimaPro-origin/ databases: a
@parse → write → parse@ cycle preserves activities, flows, units, and the LCIA
inventory exactly (pinned by "SimaProWriterSpec"). It writes /resolved numeric/
amounts and only the metadata SimaPro itself carries, so a few things present in
databases imported from /other/ formats are dropped – always score-preserving
(the numbers that drive the matrix are kept), never silently wrong:

  * parameter provenance – @Input@ / @Calculated parameters@ sections and any
    per-exchange raw formula expression are flattened to their resolved
    'Double'; 'activityAllocationFormula' likewise re-parses as 'Nothing' (the
    numeric allocation percentage is preserved);
  * classification keys other than @Category type@ / @Category@ (e.g. ISIC/CPC
    from an EcoSpold import) are not emitted;
  * reference- and co-product comments are dropped (SimaPro product rows carry a
    comment column, but a SimaPro parse never populates it);
  * an emission with an empty (unspecified) medium is written to
    @Emissions to air@ and re-parses as @air@ – SimaPro has no
    unspecified-emission section. This shifts the flow's medium (and hence its
    generated UUID), so it is the one lossy case that can affect characterisation
    for a cross-format export; air/water/soil media and all SimaPro-origin
    emissions are unaffected;
  * multi-paragraph descriptions are joined into the single physical @Comment@
    line with @\\x7f@ separators, so the re-parse reads one description entry
    holding the paragraph breaks rather than the original list. A description
    that already /contains/ a literal @\\x7f@ re-parses with that character read
    as a line break, since the format reserves it for exactly that; no SimaPro
    parse can produce one, so this only reaches a cross-format import.

Anything the format /cannot/ represent without silently corrupting the data on
re-import (non-finite amounts, a zero allocation, a missing unit, newlines in an
identity field, a pedigree-shaped comment, a metadata-key collision, an activity
without exactly one reference product) is rejected outright by
'checkSimaProExportable' rather than written lossily.
-}
module SimaPro.Writer (
    WriterConfig (..),
    defaultWriterConfig,
    writeSimaProCSV,
    serializeSimaProCSV,
    checkSimaProExportable,

    -- * Pure helpers (exposed for testing and for "Method.WriterSimaPro")
    escapeField,
    formatAmount,
    headerLines,
) where

import qualified Data.ByteString as BS
import Data.Either (lefts)
import Data.List (partition, sortOn)
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Allocation (AllocationRefusal (..), asAllocated, describeRefusal)
import SimaPro.Parser (isMetadataKey, parsePedigreePrefix)
import Types

-- ============================================================================
-- Configuration
-- ============================================================================

{- | Writer knobs. The only volatile field a SimaPro export normally carries is
the version banner; pinning it here keeps a round-trip byte-stable. We
deliberately omit the export-date / generator lines entirely (the parser
ignores them anyway) so there is no timestamp to normalise away.
-}
newtype WriterConfig = WriterConfig
    { wcVersion :: Text
    -- ^ Value of the @{SimaPro …}@ banner line.
    }
    deriving (Eq, Show)

-- | Default config: a fixed, neutral version banner (no timestamp).
defaultWriterConfig :: WriterConfig
defaultWriterConfig = WriterConfig{wcVersion = "SimaPro 9.6.0.1"}

-- ============================================================================
-- Export guard
-- ============================================================================

{- | Every activity must have exactly one reference product. The writer emits
one @Process@ block per activity, with one @Products@ row per reference output;
zero references would leave an empty @Products@ section that the parser discards
(it keeps a block only if it has a product), and more than one would re-parse
into a separate activity per row. Reject either rather than silently drop or
split the activity on re-import.

SimaPro CSV files emissions into air / water / soil sections only. An
emission whose compartment medium is some other non-empty value has no faithful
section, so report it rather than silently filing it under @Emissions to air@.
An unspecified (empty) medium is allowed – it carries no medium to lose and
follows SimaPro's air default. Resources are bucketed by direction, not medium,
so they never offend.

Amounts must also be finite: a NaN or ±Infinity has no parseable literal, so
'formatAmount' would otherwise flatten it to @0@ and silently undercount the
inventory. Report it here instead. This is the export-boundary check;
'serializeSimaProCSV' itself stays pure and total.

Identity fields must also be free of newline characters. 'escapeField'
RFC-4180-quotes them, but the parser splits the file on physical lines
('BS8.lines') /before/ CSV parsing, so an embedded @\\n@ or @\\r@ tears the row
apart and corrupts or drops it. Reject such fields rather than emit a row the
parser cannot read back. Free text is the exception, and never reaches this
guard: 'encodeNewlines' turns its line breaks into @\\x7f@, SimaPro's in-cell
newline, before they are written, so descriptions and exchange comments
round-trip instead of being rejected. That distinction is the whole rule: a line
break carries meaning in prose and none in a name, and a @\\x7f@ in a name would
poison the matching a re-import does on it.

Two more round-trip hazards are specific to SimaPro's textual layout, and bite
only for databases imported from other formats (a SimaPro parse can't produce
them):

  * a pedigree-less exchange whose comment /begins/ with a @(r,c,t,g,f)@
    quintuple – 'parsePedigreePrefix' would read it back as a 'Pedigree' and
    strip it, fabricating a data-quality score; and
  * a metadata value (activity name, location, …) equal to a SimaPro metadata
    key – the parser checks 'isMetadataKey' before reading a value line, so it
    would mistake the value for a new field and silently drop it.

Both are rejected here rather than emitted as a row the parser misreads.
-}
checkSimaProExportable :: SimpleDatabase -> Either Text ()
checkSimaProExportable db =
    case lefts
        [ checkReferences
        , checkMedia
        , checkAmounts
        , checkAllocation
        , checkCoproducts
        , checkUnits
        , checkNewlines
        , checkComments
        , checkMetaKeys
        ] of
        [] -> Right ()
        violations -> Left (T.intercalate "\n\n" violations)
  where
    checkReferences =
        case referenceOffenders of
            [] -> Right ()
            ((name, n) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": it has "
                        <> T.pack (show n)
                        <> " reference products, but a faithful round-trip needs exactly"
                        <> " one – zero would drop the whole Process block on re-import"
                        <> " (the parser keeps a block only if it has a product), and more"
                        <> " than one would split it into separate activities."
    checkMedia =
        case mediumOffenders of
            [] -> Right ()
            ((name, medium) : _) ->
                Left $
                    "SimaPro export cannot represent emission \""
                        <> name
                        <> "\": compartment medium \""
                        <> medium
                        <> "\" is not one of air, water, soil."
    checkAmounts =
        case amountOffenders of
            [] -> Right ()
            ((name, amt) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": exchange amount "
                        <> T.pack (show amt)
                        <> " is not finite."
    checkNewlines =
        case newlineOffenders of
            [] -> Right ()
            (field : _) ->
                Left $
                    "SimaPro export cannot represent field \""
                        <> field
                        <> "\": it contains a newline, which the line-based parser"
                        <> " would split across rows."
    checkAllocation =
        case allocationOffenders of
            [] -> Right ()
            ((name, pct) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": allocation percentage "
                        <> T.pack (show pct)
                        <> " is not finite – the writer divides the allocation-scaled"
                        <> " amounts back out, so a non-finite percentage would lose"
                        <> " them on re-import."
    -- A block gives every product row a share, so an activity whose products
    -- carry none cannot be written as one: each row would claim the whole
    -- and re-import would give every product the full inventory.
    checkCoproducts =
        case coproductOffenders of
            [] -> Right ()
            ((name, refusal) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": "
                        <> describeRefusal refusal
                        <> ". A SimaPro block carries a share on every product row, so"
                        <> " re-importing it would give each product the whole inventory."
    checkUnits =
        case unitOffenders of
            [] -> Right ()
            ((name, uid) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": an exchange references unit "
                        <> T.pack (show uid)
                        <> ", which is absent from the unit registry (it would be"
                        <> " written as a blank unit and re-parsed as UNKNOWN)."
    checkComments =
        case commentOffenders of
            [] -> Right ()
            ((name, cmt) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": the comment \""
                        <> cmt
                        <> "\" begins with a pedigree-shaped (r,c,t,g,f) quintuple, so the"
                        <> " parser would re-read it as a data-quality pedigree on import."
    checkMetaKeys =
        case metaKeyOffenders of
            [] -> Right ()
            ((name, val) : _) ->
                Left $
                    "SimaPro export cannot represent activity \""
                        <> name
                        <> "\": the metadata value \""
                        <> val
                        <> "\" collides with a SimaPro metadata key, so the parser would"
                        <> " mistake it for a new field and drop it on import."
    -- Each activity must have exactly one reference product. The writer emits
    -- one Process block per activity with one Products row per reference; zero
    -- references → an empty Products section → the parser drops the block, and
    -- >1 → several Products rows → the parser splits it into one activity each.
    coproductOffenders =
        [ (activityName act, refusal)
        | act <- M.elems (sdbActivities db)
        , Left refusal@UnallocatedOutputs{} <- [asAllocated act]
        ]
    referenceOffenders =
        [ (activityName act, n)
        | act <- M.elems (sdbActivities db)
        , let n = length (filter exchangeIsReference (exchanges act))
        , n /= 1
        ]
    mediumOffenders =
        [ (bfName flow, bfCompartmentName flow)
        | act <- M.elems (sdbActivities db)
        , ex@BiosphereExchange{bioDirection = Emission} <- exchanges act
        , Just flow <- [M.lookup (exchangeFlowId ex) (sdbBioFlows db)]
        , Just comp <- [bfCompartment flow]
        , -- A final waste flow is not an emission to a medium; it has its own
        -- section ('SecWaste'), so it is not held to the emission media.
        compartmentName comp `notElem` [Waste, InventoryIndicator, Air, Water, Soil]
        ]
    amountOffenders =
        [ (activityName act, amt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , let amt = exchangeAmount ex
        , isNaN amt || isInfinite amt
        ]
    allocationOffenders =
        [ (activityName act, pct)
        | act <- M.elems (sdbActivities db)
        , pct <- map (maybe 100 dsPercent) (activityDeclaredShares act)
        , isNaN pct || isInfinite pct
        ]
    unitOffenders =
        [ (activityName act, exchangeUnitId ex)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , M.notMember (exchangeUnitId ex) (sdbUnits db)
        ]
    commentOffenders =
        [ (activityName act, cmt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , Nothing <- [exchangePedigree ex]
        , Just cmt <- [exchangeComment ex]
        , isJust (fst (parsePedigreePrefix cmt))
        ]
    metaKeyOffenders =
        [ (activityName act, val)
        | act <- M.elems (sdbActivities db)
        , val <- map snd (activityMetaLines act)
        , not (T.null val)
        , isMetadataKey (TE.encodeUtf8 (T.strip val))
        ]
    hasNewline = T.any (\c -> c == '\n' || c == '\r')
    -- Every text field that lands in the output verbatim: the bare metadata
    -- value lines (so a newline in any of them – the Type label included – is
    -- caught), the "Category" product-column value, and all flow names.
    -- Free text never offends: 'activityMetaLines' and 'renderComment' both
    -- run 'encodeNewlines' over it first, so what this sees is already
    -- \x7f-encoded. The line-based parser splits on physical newlines /before/
    -- CSV parsing, so even a quoted newline tears a row apart; reject upstream.
    activityTexts act =
        map snd (activityMetaLines act)
            ++ M.elems (activityClassification act)
    newlineOffenders =
        filter hasNewline $
            concatMap activityTexts (M.elems (sdbActivities db))
                ++ map tfName (M.elems (sdbTechFlows db))
                ++ map bfName (M.elems (sdbBioFlows db))
                ++ map wfName (M.elems (sdbWasteFlows db))

-- ============================================================================
-- Constants
-- ============================================================================

-- | Field delimiter – semicolon, matching @{CSV separator: Semicolon}@.
delim :: Text
delim = ";"

-- | Line terminator – SimaPro exports use Windows CRLF.
crlf :: Text
crlf = "\r\n"

-- ============================================================================
-- Numeric formatting
-- ============================================================================

{- | Canonical numeric formatter. Produces a dot-decimal literal that the
parser's 'parseAmount' / 'Expr.normalizeExpr' read back to the same 'Double'.

Integral values are written without a trailing @.0@ (e.g. @100@, not
@100.0@) so allocation percentages match the parser's @"100"@ raw form;
all other values use Haskell's 'show', which is dot-decimal and exact
enough to round-trip a 'Double'.

A non-finite value has no parseable literal: it renders as its (non-parseable)
@"NaN"@/@"Infinity"@ form so a re-import fails loudly rather than silently
reading @0@. 'checkSimaProExportable' rejects non-finite amounts at the export
boundary, so this never fires for a validated database – but the formatter
stays honest for any direct caller.
-}
formatAmount :: Double -> Text
formatAmount x
    | isNaN x || isInfinite x = T.pack (show x)
    | x == fromIntegral (round x :: Integer) =
        T.pack (show (round x :: Integer))
    | otherwise = T.pack (show x)

-- ============================================================================
-- CSV escaping
-- ============================================================================

{- | Escape a field for semicolon-delimited CSV. Same rule as
'Matrix.Export.escapeCsvField': quote only when the field contains the
delimiter, a quote, or a newline, doubling embedded quotes.

The newline case is kept only for parity with 'Matrix.Export.escapeCsvField':
it never fires in practice because 'checkSimaProExportable' rejects newlines
upstream, and quoting would not help anyway – the parser splits the file on
physical lines /before/ CSV parsing, so an embedded newline tears the row apart
regardless of quoting.
-}
escapeField :: Text -> Text
escapeField text
    | T.any (\c -> c == ';' || c == '"' || c == '\n' || c == '\r') text =
        "\"" <> T.replace "\"" "\"\"" text <> "\""
    | otherwise = text

-- | Join fields with the delimiter (each escaped).
row :: [Text] -> Text
row = T.intercalate delim . map escapeField

-- ============================================================================
-- Flow / unit name resolution
-- ============================================================================

{- | Resolve a unit UUID to its name via the unit DB, falling back to the empty
string when absent (a missing unit is surfaced downstream by the matrix
builder, not silently defaulted to a wrong unit here).
-}
unitNameOf :: UnitDB -> UUID -> Text
unitNameOf units uid = maybe "" unitName (M.lookup uid units)

{- | The four flow/unit catalogs an activity is serialized against, gathered
into one record so the per-section helpers take a single argument instead of
threading four maps positionally (which invites silent arg-swaps between maps
of the same shape).
-}
data Catalogs = Catalogs
    { catTech :: !TechFlowDB
    , catBio :: !BioFlowDB
    , catWaste :: !WasteFlowDB
    , catUnits :: !UnitDB
    }

-- ============================================================================
-- Process block serialization
-- ============================================================================

{- | A single technosphere/biosphere/waste exchange projected to the
(name, unit, compartment, amount, comment) tuple the writer needs, plus a
sort key. Keeping this intermediate makes the per-section sorting and
formatting uniform.
-}
data Line = Line
    { lName :: !Text
    , lCompartment :: !Text -- sub-compartment for bio rows; "" otherwise
    , lUnit :: !Text
    , lAmount :: !Double
    , lComment :: !Text
    }

-- | Deterministic ordering key for a section's lines.
lineKey :: Line -> (Text, Text, Text, Double)
lineKey l = (lName l, lCompartment l, lUnit l, lAmount l)

{- | The @Type@ metadata value the writer emits for an activity's native type,
or @""@ when there is none (so the line is omitted). Shared between
'serializeActivity' and 'checkSimaProExportable' so the export guard inspects
exactly the value that gets written.
-}
typeLabelOf :: Maybe NativeActivityType -> Text
typeLabelOf mnt = case mnt of
    Just (SimaProProcessType lbl) -> lbl
    Just (EcoSpoldActivityType{eatLabel = lbl}) -> lbl
    Just (ILCDProcessType lbl) -> lbl
    Nothing -> ""

{- | The metadata @key/value@ pairs the writer emits as bare @key⏎value⏎@ lines
in a @Process@ block. Single source of truth shared by 'serializeActivity'
(which writes them) and 'checkSimaProExportable' (which guards every value
against the two hazards a bare value line is prone to): an embedded newline,
which the line-based parser would split across rows, and a value equal to a
SimaPro metadata key, which the parser would mistake for a new field. Empty
values are dropped on emission by 'serializeActivity'’s @meta@, but kept here so
the guards still inspect exactly what /would/ be written.

The "Category" classification value is intentionally absent: it rides the
@Products@ row's category column, not a bare metadata line. The description is
joined and 'encodeNewlines'-encoded, so paragraph breaks survive as @\\x7f@ on
the one physical "Comment" line the format allows; the re-parse decodes them
back into a single description entry carrying the breaks. A missing native type
yields an empty "Type" value, so @meta@ omits the line and a re-parse yields
'Nothing' again rather than drifting to "Unit process".
-}
activityMetaLines :: Activity -> [(Text, Text)]
activityMetaLines Activity{..} =
    [ ("Category type", M.findWithDefault "" "Category type" activityClassification)
    , ("Process identifier", foldMap (\(NativeProcessId nativeId) -> nativeId) activityNativeId)
    , ("Process name", activityName)
    , ("Type", typeLabelOf activityNativeType)
    , ("Geography", activityLocation)
    , ("Comment", encodeNewlines (T.intercalate "\n" activityDescription))
    ]

{- | Encode line breaks as @\\x7f@ (DEL), SimaPro's in-cell newline, after
normalising CRLF and lone CR to LF. This is how the format itself carries
multi-line free text: the row stays one physical line, and the parser decodes
@\\x7f@ back to a newline ('SimaPro.Parser.nonEmptyText'), so the text
round-trips instead of tearing the row apart.

Every free-text field the writer emits goes through this. Identity fields
(names, geography, type, classification) do not: a line break there is rejected
by 'checkSimaProExportable' rather than encoded, because it carries no meaning
worth keeping and a @\\x7f@ would poison the name a re-import matches on.
-}
encodeNewlines :: Text -> Text
encodeNewlines = T.replace "\n" "\x7f" . T.replace "\r" "\n" . T.replace "\r\n" "\n"

{- | Render the comment column, re-attaching a pedigree prefix when present.
Line breaks are 'encodeNewlines'-encoded, so multi-line comments round-trip.
-}
renderComment :: Maybe Pedigree -> Maybe Text -> Text
renderComment ped cmt =
    let pedTxt = maybe "" renderPedigree ped
        cmtTxt = encodeNewlines (fromMaybe "" cmt)
     in case (pedTxt, cmtTxt) of
            ("", c) -> c
            (p, "") -> p <> ","
            (p, c) -> p <> "," <> c

-- | @(r,c,t,g,f)@ pedigree quintuple, matching 'parsePedigreePrefix'.
renderPedigree :: Pedigree -> Text
renderPedigree Pedigree{..} =
    "("
        <> T.intercalate
            ","
            (map (T.pack . show) [pedReliability, pedCompleteness, pedTemporal, pedGeographical, pedTechnological])
        <> ")"

{- | Build a 'Line' for the @Materials\/fuels@ section, which holds inputs and
only inputs. 'Nothing' for every other role, and for an input whose flow is
unknown to the tech-flow catalogue.

Each of the other roles has a section of its own: the reference, product or
treatment input, and the coproducts go to @Products@, a substitution to
@Avoided products@.
Writing one here as well puts it in the file twice, and a reader then sees two
exchanges where the source had one. On a substitution that is not a duplicate
but a cancellation: the credit meets an equal input, and the activity loses the
benefit it existed to record, along with every chain standing on it.
-}
techInputLine :: Catalogs -> Exchange -> Maybe Line
techInputLine cats TechnosphereExchange{..} = case techRole of
    Input -> inputLine
    ReferenceProduct -> Nothing
    ReferenceInput -> Nothing
    Coproduct -> Nothing
    AvoidedProduct -> Nothing
  where
    inputLine :: Maybe Line
    inputLine = do
        flow <- M.lookup techFlowId (catTech cats)
        pure
            Line
                { lName = tfName flow
                , lCompartment = ""
                , lUnit = unitNameOf (catUnits cats) techUnitId
                , lAmount = techAmount
                , lComment = renderComment techPedigree techComment
                }
techInputLine _ BiosphereExchange{} = Nothing
techInputLine _ WasteExchange{} = Nothing

-- | Build a 'Line' for a biosphere exchange (resource or emission).
bioLine :: Catalogs -> Exchange -> Maybe Line
bioLine cats BiosphereExchange{..} =
    case M.lookup bioFlowId (catBio cats) of
        Nothing -> Nothing
        Just flow ->
            Just
                Line
                    { lName = bfName flow
                    , lCompartment = fromMaybe "" (bfCompartmentSub flow)
                    , lUnit = unitNameOf (catUnits cats) bioUnitId
                    , lAmount = bioAmount
                    , lComment = renderComment bioPedigree bioComment
                    }
bioLine _ TechnosphereExchange{} = Nothing
bioLine _ WasteExchange{} = Nothing

-- | Build a 'Line' for a waste exchange (Final waste flows section).
wasteLine :: Catalogs -> Exchange -> Maybe Line
wasteLine cats WasteExchange{..} =
    case M.lookup waFlowId (catWaste cats) of
        Nothing -> Nothing
        Just flow ->
            Just
                Line
                    { lName = wfName flow
                    , lCompartment = ""
                    , lUnit = unitNameOf (catUnits cats) waUnitId
                    , lAmount = waAmount
                    , lComment = renderComment waPedigree waComment
                    }
wasteLine _ TechnosphereExchange{} = Nothing
wasteLine _ BiosphereExchange{} = Nothing

{- | The medium a biosphere exchange belongs to, derived from the flow's
compartment name. SimaPro splits biosphere rows into five sections keyed on
the medium: Resources, Emissions to air/water/soil, and Final waste flows.
Resources are the 'Resource' direction; emissions are bucketed by compartment
name. A final waste flow and an inventory indicator both count what the
activity sends away rather than naming a substance exchanged with a medium, so
they belong to Final waste flows whatever their direction says: their medium
decides first.
-}
bioSection :: Catalogs -> Exchange -> Maybe BioSec
bioSection cats ex@BiosphereExchange{bioDirection = dir}
    | medium `elem` [Just Waste, Just InventoryIndicator] = Just SecWaste
    | otherwise = case dir of
        Resource -> Just SecRes
        -- Unknown flow → Nothing, mirroring 'bioLine' so an emission keeps a
        -- section only when it also keeps a row (the two are paired in
        -- 'serializeActivity'). A *known* flow that records no compartment
        -- keeps both: it has a row, so it needs a section, and air is where
        -- this format files an emission it cannot place.
        Emission -> maybe SecAir sectionForMedium medium <$ flow
  where
    flow :: Maybe BiosphereFlow
    flow = M.lookup (exchangeFlowId ex) (catBio cats)

    medium :: Maybe Medium
    medium = compartmentName <$> (bfCompartment =<< flow)
    sectionForMedium :: Medium -> BioSec
    sectionForMedium = \case
        Water -> SecWater
        Soil -> SecSoil
        -- 'checkSimaProExportable' rejects every medium but air, water and soil
        -- at the export boundary, so a real export only ever lands air here.
        Air -> SecAir
        NaturalResource -> SecAir
        InventoryIndicator -> SecAir
        Economic -> SecAir
        Waste -> SecAir
bioSection _ TechnosphereExchange{} = Nothing
bioSection _ WasteExchange{} = Nothing

data BioSec = SecRes | SecAir | SecWater | SecSoil | SecWaste
    deriving (Eq)

{- | Output rows (@name;unit;amount;allocation;waste_type;category;comment@) for
the technosphere outputs matching @keep@. Used for both the @Products@ section
(references) and the @Avoided products@ section (coproducts) – the two are
rendered identically but the parser routes them to different exchange roles, so
they must be emitted under their own headers, never merged.
-}
productLines :: (Exchange -> Bool) -> Catalogs -> Text -> [Exchange] -> [Text]
productLines keep cats category exchs =
    let
        -- Extract the row fields in the comprehension, where the exchange is
        -- known to be a TechnosphereExchange – so mkRow is total (no unreachable
        -- blank-row arm). Sort by (name, unit, amount) for determinism. Each
        -- row carries the share and the category its source row declared;
        -- a row that declared none takes the whole and the activity's category.
        entries =
            [ ( tfName flow
              , unitNameOf (catUnits cats) (exchangeUnitId ex)
              , exchangeAmount ex
              , maybe 100 dsPercent (exchangeDeclaredShare ex)
              , M.findWithDefault category "Category" (exchangeClassification ex)
              , renderComment (exchangePedigree ex) (exchangeComment ex)
              )
            | ex@TechnosphereExchange{} <- exchs
            , keep ex
            , Just flow <- [M.lookup (exchangeFlowId ex) (catTech cats)]
            ]
        mkRow (nm, unit, amt, share, rowCategory, comment) =
            row [nm, unit, formatAmount amt, formatAmount share, "not defined", rowCategory, comment]
     in
        map mkRow (sortOn id entries)

-- | A product output the gate left unsplit: written as a further @Products@ row of its block.
isCoproduct :: Exchange -> Bool
isCoproduct ex = case ex of
    TechnosphereExchange{techRole = Coproduct} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

-- | A substitution (SimaPro @Avoided products@ section), which the parser reads back as 'AvoidedProduct'.
isAvoidedProduct :: Exchange -> Bool
isAvoidedProduct ex = case ex of
    TechnosphereExchange{techRole = AvoidedProduct} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

{- | Prepend the @Avoided products@ header to coproduct rows, or emit nothing
when there are none (an empty section would be noise the parser ignores).
-}
avoidedHeader :: [Text] -> [Text]
avoidedHeader [] = []
avoidedHeader rows = "Avoided products" : rows

{- | Render one section: a header line followed by its sorted rows. Emits
nothing when there are no rows, matching the parser (it tolerates absent
sections).
-}
section :: Text -> (Line -> Text) -> [Line] -> [Text]
section _ _ [] = []
section header render ls = header : map render (sortOn lineKey ls)

{- | Render a technosphere input row: name;unit;amount;Undefined;0;0;0;comment
The parser skips three distribution columns after the uncertainty type
(parseTechRow: name:unit:amount:unc:_:_:_:rest), so the comment/pedigree must
sit in the eighth column or it lands in a placeholder and is dropped on parse-back.
-}
techRowText :: Line -> Text
techRowText Line{..} =
    row [lName, lUnit, formatAmount lAmount, "Undefined", "0", "0", "0", lComment]

{- | Render a biosphere/waste row: name;compartment;unit;amount;Undefined;;;;;;comment
The parser skips three distribution columns after the uncertainty type
(parseBioRow: name:compartment:unit:amount:unc:_:_:_:rest), so the
comment/pedigree must land past the eighth column or it is dropped on
parse-back. Pad with five empty distribution columns to mirror the parser's
expected layout.
-}
bioRowText :: Line -> Text
bioRowText Line{..} =
    row [lName, lCompartment, lUnit, formatAmount lAmount, "Undefined", "", "", "", "", "", lComment]

{- | Scale an exchange's amount by a factor – the inverse of the parser's
allocation scaling (see 'serializeActivity').
-}
scaleExchangeAmount :: Double -> Exchange -> Exchange
scaleExchangeAmount f ex@TechnosphereExchange{} = ex{techAmount = techAmount ex * f}
scaleExchangeAmount f ex@BiosphereExchange{} = ex{bioAmount = bioAmount ex * f}
scaleExchangeAmount f ex@WasteExchange{} = ex{waAmount = waAmount ex * f}

{- | Serialize a single activity to a @Process … End@ block (list of lines,
without trailing terminator). Flow/unit names are resolved through the
respective DBs; exchanges whose flow is missing are dropped (the matrix
builder is the authority on unknown flows, not the serializer).
-}
serializeActivity :: Catalogs -> Activity -> [Text]
serializeActivity cats act@Activity{..} =
    let category = M.findWithDefault "" "Category" activityClassification

        -- The parser scales every shared exchange (everything but the reference
        -- product) by allocFraction = allocPercent/100 on import. To be its exact
        -- inverse, emit the *pre-allocation* amounts: divide the shared exchanges
        -- back out so the re-import lands on the stored amounts again (emitting
        -- them as-is would let the parser scale a second time). The reference
        -- product is never scaled, so it passes through untouched.
        -- A 0% allocation is the degenerate case: the parser scaled every shared
        -- amount to 0, so there is nothing to divide back out – emit the stored
        -- zeros as-is and the re-import scales 0 by 0 again. 'checkSimaProExportable'
        -- still rejects a non-finite allocation, so the division below is finite.
        allocFraction = maybe 100 dsPercent (activityReferenceShare act) / 100
        unscale ex
            | exchangeIsReference ex = ex
            | allocFraction == 0 = ex
            | otherwise = scaleExchangeAmount (1 / allocFraction) ex
        unscaledExchanges = map unscale exchanges

        techLines = mapMaybe (techInputLine cats) unscaledExchanges
        bioByName name = [l | (sec, l) <- bioPaired, sec == name]
        bioPaired =
            [ (sec, l)
            | ex@BiosphereExchange{} <- unscaledExchanges
            , Just sec <- [bioSection cats ex]
            , Just l <- [bioLine cats ex]
            ]
        -- A waste row goes to the section that says what the format will
        -- read back: a named treatment makes it an input from that treatment,
        -- and with none nothing treats it, which is a final waste flow.
        (treatedWaste, finalWaste) = partition linkedWaste unscaledExchanges
        treatmentLines = mapMaybe (wasteLine cats) treatedWaste
        wasteLines = mapMaybe (wasteLine cats) finalWaste
     in concat
            [ ["Process", ""]
            , concatMap (uncurry meta) (activityMetaLines act)
            , -- Products section is always present (an activity has a reference).
              -- The reference comes first, which is how the parser tells it; the
              -- coproducts of a block the gate left unsplit follow, each with its
              -- declared share. Avoided products go under their own header.
              "Products"
                : productLines exchangeIsReference cats category unscaledExchanges
                ++ productLines isCoproduct cats category unscaledExchanges
            , [""]
            , withBlank (avoidedHeader (productLines isAvoidedProduct cats category unscaledExchanges))
            , -- Inputs.
              withBlank (section "Materials/fuels" techRowText techLines)
            , withBlank (section "Waste to treatment" techRowText treatmentLines)
            , withBlank (section "Resources" bioRowText (bioByName SecRes))
            , withBlank (section "Emissions to air" bioRowText (bioByName SecAir))
            , withBlank (section "Emissions to water" bioRowText (bioByName SecWater))
            , withBlank (section "Emissions to soil" bioRowText (bioByName SecSoil))
            , -- Both readings of a final waste flow: the elementary one every
              -- source but this format's own waste axis produces
              -- ('bioSection'), and a waste exchange that names no treatment.
              withBlank (section "Final waste flows" bioRowText (bioByName SecWaste ++ wasteLines))
            , ["End", ""]
            ]
  where
    -- A metadata line is emitted only when its value is non-empty; an empty
    -- value drops the whole key/value/blank triple so a re-parse yields the
    -- absent field again rather than an empty string.
    meta key val = if T.null val then [] else [key, val, ""]
    -- Append a blank separator line after a non-empty section.
    withBlank [] = []
    withBlank ls = ls ++ [""]

-- ============================================================================
-- Header
-- ============================================================================

-- | The fixed SimaPro header block. No timestamp / generator line is emitted.
headerLines :: WriterConfig -> [Text]
headerLines cfg =
    [ "{" <> wcVersion cfg <> "}"
    , "{CSV separator: Semicolon}"
    , "{Decimal separator: .}"
    , "{Date separator: /}"
    , "{Short date format: dd/MM/yyyy}"
    , ""
    ]

-- ============================================================================
-- Top-level serialization
-- ============================================================================

{- | Serialize a 'SimpleDatabase' to canonical SimaPro CSV bytes (UTF-8, CRLF).
Runs 'checkSimaProExportable' first and returns its 'Left' on a database the
format cannot represent faithfully, so an unguarded caller can never silently
emit a corrupt or lossy file. Activities are sorted by (name, location) so the
byte stream is independent of the underlying 'Map' iteration order.
-}
serializeSimaProCSV :: WriterConfig -> SimpleDatabase -> Either Text BS.ByteString
serializeSimaProCSV cfg db@SimpleDatabase{..} = do
    checkSimaProExportable db
    let cats = Catalogs sdbTechFlows sdbBioFlows sdbWasteFlows sdbUnits
        acts = sortOn (\a -> (activityName a, activityLocation a)) (M.elems sdbActivities)
        blocks = concatMap (serializeActivity cats) acts
        allLines = headerLines cfg ++ blocks
    pure (TE.encodeUtf8 (T.intercalate crlf allLines <> crlf))

-- | Write canonical SimaPro CSV bytes to a file, or return the guard's 'Left'.
writeSimaProCSV :: WriterConfig -> FilePath -> SimpleDatabase -> IO (Either Text ())
writeSimaProCSV cfg path db =
    case serializeSimaProCSV cfg db of
        Left err -> pure (Left err)
        Right bytes -> Right <$> BS.writeFile path bytes

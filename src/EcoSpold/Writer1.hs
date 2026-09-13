{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | EcoSpold1 export writer - the canonical re-emitter for "EcoSpold.Parser1".

Serializes a 'Database' / 'SimpleDatabase' back to the EcoSpold1 XML format
(Ecoinvent 2.x, @http://www.EcoInvent.org/EcoSpold01@). The output is
__canonical and deterministic__:

  * activities are emitted in a stable order (sorted by @(activityName,
    location)@), each in its own @\<dataset\>@ inside one @\<ecoSpold\>@;
  * dataset numbers are assigned sequentially (1-based) in that order, and
    exchange numbers follow the convention EcoSpold1 exports use, which the
    parser reads as a flow's identity: a number names a flow for the whole
    export, not a position in one dataset. A reference product carries its own
    dataset's number, a linked technosphere input carries its supplier's, and
    every other exchange carries a number assigned once per distinct flow.
    That makes the writer's output __self-stable for every exchange except a
    linked technosphere input__: reference products, co-products, biosphere
    exchanges and /unlinked/ inputs all reproduce the same flow UUIDs on
    a write→parse→write cycle, and one substance stays one flow across the
    re-imported export. A waste exchange is the one row that changes axis by
    design: EcoSpold1 has no third flow class, so it is written as the
    technosphere or biosphere row the mapping below names, and it is that shape
    which is stable from then on. This is fixed-point stability of the writer's own
    canonical form, __not__ reproduction of the UUIDs of an arbitrary parsed
    source file - that source carried its own numbering, which the writer does
    not preserve;
  * a /linked/ technosphere input is the one exception. Its @number@ attribute
    carries the /supplier's/ dataset number (so the loader can re-link it by
    'techActivityLinkId'); but 'SimpleDatabase' has no field for that link, so a
    bare re-parse drops it and the next write falls back to the positional
    index. Byte-stability of a linked input therefore relies on the full
    loader re-resolving the link (by supplier name/location) rather than on a
    direct 'SimpleDatabase' round-trip. The semantic round-trip - flow name,
    amount, role, unit - is preserved either way;
  * exchange numbers come from the flow, not from the position in the dataset;
  * attributes appear in a fixed order, classification maps are sorted by
    key, numbers use a fixed textual form, and there is no insignificant
    whitespace beyond a single newline between top-level lines;
  * volatile metadata (@generator@, @timestamp@) is pinned or omitted via
    'WriterOptions' so a write→parse→write round-trip is byte-stable.

The mapping mirrors 'EcoSpold.Parser1.buildExchange' exactly:

  * 'ReferenceProduct' / 'ReferenceInput' → @\<outputGroup\>0\</outputGroup\>@
  * 'Coproduct'                          → @\<outputGroup\>1\</outputGroup\>@
  * 'AvoidedProduct'                     → @\<inputGroup\>5\</inputGroup\>@,
    amount negated (a substitution re-parses to a negative input, the same
    matrix entry)
  * technosphere 'Input'                 → @\<inputGroup\>5\</inputGroup\>@
    (parser treats any non-empty inputGroup as a tech input)
  * biosphere 'Resource'                 → @\<inputGroup\>4\</inputGroup\>@
  * biosphere 'Emission'                 → @\<outputGroup\>4\</outputGroup\>@
  * waste input  (waIsInput=True)        → @\<inputGroup\>5\</inputGroup\>@,
    written as a technosphere input, carrying its supplier's dataset number
    when the treatment it names is exported too
  * waste output, unlinked                → @\<outputGroup\>4\</outputGroup\>@,
    category=@"waste"@, written as a biosphere emission
  * waste output naming a treatment       → rejected by
    'checkEcoSpold1Exportable'

One rule shapes those three: EcoSpold1 can name a supplier only from an
input's @number@ attribute. A waste input is therefore the only waste row the
format can carry with its link intact, and an output that names a treatment
has to be refused rather than written with the link dropped. An output that
names none is what a final waste flow is, and the parser reads it back as the
elementary flow of medium 'Waste' that it is.

The SimaPro writer partitions on the link alone, so it sends an /unlinked/
waste input to its own final-waste section, where this writer sends it to the
technosphere. The matrix agrees with this one: an unlinked input is a demand
of +1 still waiting for a supplier, and a technosphere input keeps it.

Flow names, categories, CAS numbers and units are not stored on the
'Exchange'; they live on the flow / unit tables and are resolved by UUID.
A biosphere flow's @category@ / @subCategory@ come from its 'Compartment';
technosphere flows carry no category (the parser only used it to seed the
UUID), and a waste flow borrows whichever of the two shapes its row was
written in.
-}
module EcoSpold.Writer1 (
    -- * Options
    WriterOptions (..),
    defaultWriterOptions,
    canonicalWriterOptions,

    -- * Writers
    writeDatabase,
    writeSimpleDatabase,

    -- * Export boundary check
    checkEcoSpold1Exportable,

    -- * Pure helpers (exported for testing)
    escapeXmlAttr,
    formatAmount,
) where

import Amount (readAmount)
import Data.Either (lefts)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import EcoSpold.Common (showFFloatTrim)
import Types

-- ----------------------------------------------------------------------------
-- Options
-- ----------------------------------------------------------------------------

{- | Knobs controlling the volatile, non-semantic parts of the output.
Keeping these out of band lets a round-trip be byte-stable: write with
'canonicalWriterOptions' (no @generator@/@timestamp@) and the second write
reproduces the first exactly.
-}
data WriterOptions = WriterOptions
    { woGenerator :: !(Maybe Text)
    -- ^ Value of the @\<dataset generator=...\>@ attribute, or 'Nothing' to omit.
    , woTimestamp :: !(Maybe Text)
    -- ^ Value of the @\<dataset timestamp=...\>@ attribute, or 'Nothing' to omit.
    }
    deriving (Eq, Show)

-- | Self-describing default: pins generator to "VoLCA", omits the timestamp.
defaultWriterOptions :: WriterOptions
defaultWriterOptions = WriterOptions (Just "VoLCA") Nothing

{- | Fully canonical: omits both volatile attributes. Use this for stable
round-trip / golden tests.
-}
canonicalWriterOptions :: WriterOptions
canonicalWriterOptions = WriterOptions Nothing Nothing

-- ----------------------------------------------------------------------------
-- Top-level writers
-- ----------------------------------------------------------------------------

-- | Serialize a built 'Database' (via 'toSimpleDatabase').
writeDatabase :: WriterOptions -> Database -> Either Text Text
writeDatabase opts = writeSimpleDatabase opts . toSimpleDatabase

{- | Serialize a 'SimpleDatabase'. Flow / unit names are resolved from its
tables. Runs 'checkEcoSpold1Exportable' first and returns its 'Left' on a
database the format cannot represent faithfully, so an unguarded caller can
never silently emit a lossy or role-flipped file.
-}
writeSimpleDatabase :: WriterOptions -> SimpleDatabase -> Either Text Text
writeSimpleDatabase opts sdb = do
    checkEcoSpold1Exportable sdb
    pure $
        writeActivities
            opts
            (sdbTechFlows sdb)
            (sdbBioFlows sdb)
            (sdbWasteFlows sdb)
            (sdbUnits sdb)
            (sdbActivities sdb)

{- | Serialize the database's activities against the flow / unit tables that
resolve their exchange UUIDs. Internal: it does no export-boundary checking,
so it is reached only through 'writeSimpleDatabase', which runs
'checkEcoSpold1Exportable' first. Not exported, so no caller can bypass the
guard.
-}
writeActivities ::
    WriterOptions ->
    TechFlowDB ->
    BioFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    ActivityMap ->
    Text
writeActivities opts techs bios wastes units activities =
    T.unlines $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        ]
            ++ concat (zipWith (datasetLines opts res) [1 ..] (map snd ordered))
            ++ ["</ecoSpold>"]
  where
    res = Resolvers techs bios wastes units (supplierNumberIndex ordered) (flowNumberIndex ordered)
    -- Stable, content-derived order so dataset numbers (and thus flow UUIDs)
    -- are reproducible regardless of the source Map's ordering.
    ordered = orderedActivities activities

{- | Canonical export order: sorted by @(activityName, location)@ so dataset
numbers (and thus flow UUIDs) are reproducible regardless of the source Map's
ordering. Each activity is paired with the stored activity UUID - the first
component of its 'sdbActivities' key - that a technosphere input's
'techActivityLinkId' points at. Shared by the writer and
'checkEcoSpold1Exportable'.
-}
orderedActivities :: ActivityMap -> [((UUID, UUID), Activity)]
orderedActivities =
    sortOn (\(_, a) -> (activityName a, activityLocation a))
        . M.toList

{- | Map each exported row to the dataset number it is assigned in canonical
order. A technosphere input names its supplier by the pair 'techActivityLinkId'
plus 'techFlowId' - the supplier row's 'sdbActivities' key - and the parser reads
the input's @number@ attribute back as that supplier dataset number
('EcoSpold.Parser1.closeExchange'). Keying by the stored UUIDs rather than
re-deriving them lets a link resolve whatever namespace the source format minted
them in.

The key is the whole pair because an allocated activity is exported as one
dataset per coproduct, each with its own number. Keying on the activity UUID
alone kept one of them and gave every link to that supplier the number of an
arbitrary coproduct, which the parser then read back as a link to the wrong
product.
-}
supplierNumberIndex :: [((UUID, UUID), Activity)] -> M.Map (UUID, UUID) Int
supplierNumberIndex ordered =
    M.fromList [(key, n) | (n, (key, _)) <- zip [1 ..] ordered]

{- | Map each flow that is not a reference product to the number the export
gives it, once, across every dataset that carries it.

EcoSpold1 numbers a flow for the whole export, not for the dataset it appears
in, and the parser reads that number as part of the flow's identity. Numbering
by position instead would re-import one substance as one flow per position it
happens to occupy, which is exactly the splintering the parser stopped doing.

Numbers start above the dataset numbers, which occupy @1 .. length ordered@ and
are what a reference product and a linked input carry.
-}
flowNumberIndex :: [((UUID, UUID), Activity)] -> M.Map UUID Int
flowNumberIndex ordered =
    M.fromList (zip flows [length ordered + 1 ..])
  where
    flows =
        S.toAscList $
            S.fromList
                [ exchangeFlowId ex
                | (_, act) <- ordered
                , ex <- exchanges act
                , not (exchangeIsReference ex)
                ]

{- | Guard an EcoSpold1 export against data the writer cannot faithfully
re-encode. Each check reports its first offender and fails loudly rather than
emit silently wrong data:

  * __Dangling supplier links.__ The format names a supplier from a
    technosphere input's @number@ attribute, which the parser reads as the
    supplier's dataset number. The writer can only re-emit that number when the
    supplier row the link names - the (activity, product) pair - is itself being
    exported (present in 'supplierNumberIndex'). A linked input naming a pair
    absent from the database would otherwise force a positional index the parser
    would misread as a different supplier.

  * __Reference inputs.__ EcoSpold1 has no marker for a reference /input/, so
    the writer would emit @outputGroup 0@ and the parser would read it back as
    a reference product - a direction flip.

  * __Waste outputs naming a treatment.__ The format names a supplier only
    from an input's @number@ attribute, so the link of a waste /output/ cannot
    be written. Emitting the row anyway would re-import it as waste nothing
    treats, silently deleting the treatment it named.

  * __Waste-marker collision.__ A biosphere flow whose compartment name is
    @"Final waste flows"@ would re-import under compartment @"waste"@, because
    the parser reads that category as the marker of a final waste flow - a
    silent change of the compartment a method characterizes it under. Rejected.

  * __Missing flows / units.__ An exchange whose flow or unit is absent from
    the tables would be written with a blank name and re-parse as a different
    (or @UNKNOWN@) entity.

  * __Non-finite amounts.__ @NaN@/@Infinity@ have no parseable literal.

  * __Non-round-tripping amounts.__ A defensive check that the written decimal
    re-parses to the same 'Double' through 'Amount.readAmount' (the importer's
    correctly-rounded reader). Every finite amount round-trips, so this guards
    the formatter↔reader contract against future drift.

Databases free of all of these pass unchanged.
-}
checkEcoSpold1Exportable :: SimpleDatabase -> Either Text ()
checkEcoSpold1Exportable db =
    case lefts [checkLinks, checkRefInputs, checkLinkedWasteOutputs, checkFlows, checkUnits, checkAmounts, checkAmountRoundTrip] of
        [] -> Right ()
        violations -> Left (T.intercalate "\n\n" violations)
  where
    checkLinks =
        case danglingLinks of
            [] -> Right ()
            ((consumer, link) : _) ->
                Left $
                    "EcoSpold1 export cannot encode the supplier link of an input in \""
                        <> consumer
                        <> "\": linked activity "
                        <> UUID.toText link
                        <> " is not among the exported datasets, so its dataset number is unknown."
    checkRefInputs =
        case refInputOffenders of
            [] -> Right ()
            (consumer : _) ->
                Left $
                    "EcoSpold1 export cannot encode a reference input (treatment process) in \""
                        <> consumer
                        <> "\": the format has no marker for it, so the writer would emit"
                        <> " outputGroup 0 and the parser would read it back as a reference"
                        <> " product - a direction flip from input to output."
    checkLinkedWasteOutputs =
        case linkedWasteOutputs of
            [] -> Right ()
            (producer : _) ->
                Left $
                    "EcoSpold1 export cannot encode the treatment named by a waste output in \""
                        <> producer
                        <> "\": the format names a supplier only from an input's number"
                        <> " attribute, so the writer would have to drop the link and the"
                        <> " parser would read the row back as waste nothing treats."
    checkFlows =
        case flowOffenders of
            [] -> Right ()
            (consumer : _) ->
                Left $
                    "EcoSpold1 export cannot represent activity \""
                        <> consumer
                        <> "\": an exchange references a flow absent from the database,"
                        <> " which would be written with a blank name."
    checkUnits =
        case unitOffenders of
            [] -> Right ()
            ((consumer, uid) : _) ->
                Left $
                    "EcoSpold1 export cannot represent activity \""
                        <> consumer
                        <> "\": an exchange references unit "
                        <> UUID.toText uid
                        <> ", absent from the registry (it would be written with a blank unit)."
    checkAmounts =
        case amountOffenders of
            [] -> Right ()
            ((consumer, amt) : _) ->
                Left $
                    "EcoSpold1 export cannot represent activity \""
                        <> consumer
                        <> "\": exchange amount "
                        <> T.pack (show amt)
                        <> " is not finite."
    checkAmountRoundTrip =
        case roundTripOffenders of
            [] -> Right ()
            ((consumer, amt) : _) ->
                Left $
                    "EcoSpold1 export cannot represent activity \""
                        <> consumer
                        <> "\": exchange amount "
                        <> T.pack (show amt)
                        <> " has no decimal form that re-parses to the same value."
    index = supplierNumberIndex (orderedActivities (sdbActivities db))
    danglingLinks =
        [ (activityName act, link)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , (link, fid) <- namedSupplier ex
        , not (M.member (link, fid) index)
        ]
    -- The rows whose number names a supplier: a technosphere input, and a
    -- waste input, which the writer emits as one. An output names none, on
    -- either axis.
    namedSupplier :: Exchange -> [(UUID, UUID)]
    namedSupplier ex = case ex of
        TechnosphereExchange{techRole = Input, techActivityLinkId = Just link, techFlowId = fid} -> [(link, fid)]
        TechnosphereExchange{} -> []
        BiosphereExchange{} -> []
        WasteExchange{waIsInput = True, waActivityLinkId = Just link, waFlowId = fid} -> [(link, fid)]
        WasteExchange{} -> []
    linkedWasteOutputs =
        [ activityName act
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , linkedWaste ex && not (exchangeIsInput ex)
        ]
    amountOffenders =
        [ (activityName act, amt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , let amt = exchangeAmount ex
        , isNaN amt || isInfinite amt
        ]
    roundTripOffenders =
        [ (activityName act, amt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , let amt = exchangeAmount ex
        , not (isNaN amt || isInfinite amt) -- non-finite already reported by checkAmounts
        , not (amountRoundTrips amt)
        ]
    -- The written decimal must re-parse to the same Double through the importer's
    -- correctly-rounded reader ('Amount.readAmount'), or the value would change
    -- on re-import.
    amountRoundTrips amt = readAmount (formatAmount amt) == Just amt
    refInputOffenders =
        [ activityName act
        | act <- M.elems (sdbActivities db)
        , TechnosphereExchange{techRole = ReferenceInput} <- exchanges act
        ]
    flowOffenders =
        [ activityName act
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , flowMissing ex
        ]
    flowMissing ex = case ex of
        TechnosphereExchange{techFlowId = fid} -> M.notMember fid (sdbTechFlows db)
        BiosphereExchange{bioFlowId = fid} -> M.notMember fid (sdbBioFlows db)
        WasteExchange{waFlowId = fid} -> M.notMember fid (sdbWasteFlows db)
    unitOffenders =
        [ (activityName act, exchangeUnitId ex)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , M.notMember (exchangeUnitId ex) (sdbUnits db)
        ]

-- | Bundle of lookup tables threaded through the per-exchange renderers.
data Resolvers = Resolvers
    { rTech :: !TechFlowDB
    , rBio :: !BioFlowDB
    , rWaste :: !WasteFlowDB
    , rUnits :: !UnitDB
    , rSupplierNumbers :: !(M.Map (UUID, UUID) Int)
    , rFlowNumbers :: !(M.Map UUID Int)
    }

-- ----------------------------------------------------------------------------
-- Per-dataset rendering
-- ----------------------------------------------------------------------------

-- | Render one @\<dataset\>@ block (already indented), as a list of lines.
datasetLines :: WriterOptions -> Resolvers -> Int -> Activity -> [Text]
datasetLines opts res num act =
    [ indent 1 <> "<dataset" <> datasetAttrs opts num <> ">"
    , indent 2 <> "<metaInformation>"
    , indent 3 <> "<processInformation>"
    , indent 4 <> "<referenceFunction" <> refFunctionAttrs act <> "/>"
    , indent 4 <> "<geography location=" <> attr (activityLocation act) <> "/>"
    , indent 3 <> "</processInformation>"
    , indent 2 <> "</metaInformation>"
    , indent 2 <> "<flowData>"
    ]
        ++ concatMap (exchangeLines res num) (exchanges act)
        ++ [ indent 2 <> "</flowData>"
           , indent 1 <> "</dataset>"
           ]

{- | @\<dataset\>@ attributes: @number@ always, then the volatile @generator@
and @timestamp@ only when the options provide them.
-}
datasetAttrs :: WriterOptions -> Int -> Text
datasetAttrs opts num =
    " number="
        <> attr (T.pack (show num))
        <> maybe "" (\g -> " generator=" <> attr g) (woGenerator opts)
        <> maybe "" (\t -> " timestamp=" <> attr t) (woTimestamp opts)

{- | @\<referenceFunction\>@ attributes in a fixed order. @category@ /
@subCategory@ come from 'activityClassification' (the parser's @Category@ /
@SubCategory@ keys); a non-empty activity description is joined and emitted
as @generalComment@.

Note: @generalComment@ is a single ES1 attribute, so paragraph cardinality
does not round-trip - the joined text survives, but 'Parser1' reads it back
as a one-element 'activityDescription' regardless of how many paragraphs went in.
-}
refFunctionAttrs :: Activity -> Text
refFunctionAttrs act =
    " name="
        <> attr (activityName act)
        <> optAttr "category" (classif "Category")
        <> optAttr "subCategory" (classif "SubCategory")
        <> " unit="
        <> attr (activityUnit act)
        <> optAttr "generalComment" generalComment
  where
    classif k = M.findWithDefault "" k (activityClassification act)
    generalComment = T.intercalate "\n" (activityDescription act)

-- ----------------------------------------------------------------------------
-- Exchange rendering
-- ----------------------------------------------------------------------------

{- | Render one @\<exchange\>@ as opening tag + group element + closing tag.
The group element (@inputGroup@ / @outputGroup@) is what the parser keys on
to classify the exchange, so it is derived from the exchange variant +
role/direction, never guessed.
-}
exchangeLines :: Resolvers -> Int -> Exchange -> [Text]
exchangeLines res datasetNum ex =
    [ indent 3 <> "<exchange" <> exchangeAttrs res datasetNum ex <> ">"
    , indent 4 <> groupElement ex
    , indent 3 <> "</exchange>"
    ]

{- | The @number@ attribute to emit for an exchange, following the convention
the parser reads: a number names a flow across the whole export.

A reference product carries its own dataset's number, which is what every
EcoSpold1 export observed here does and what lets a consumer name its supplier.
A technosphere input the loader resolved carries its supplier's dataset number
('EcoSpold.Parser1.closeExchange' reads it back as the supplier link), so the
two agree on one number for one product; a resolved waste input does the same,
being written as a technosphere input. Everything else - co-products,
biosphere, waste outputs, and unlinked inputs - carries the number its flow was
given once for the whole export ('flowNumberIndex').

'checkEcoSpold1Exportable' guarantees any resolved link's (activity, product)
pair is present in the supplier index before export, so that fallback is
unreachable on the wired-up path; the flow index covers every non-reference
exchange by construction.
-}
exchangeNumber :: Resolvers -> Int -> Exchange -> Int
exchangeNumber res datasetNum ex
    | exchangeIsReference ex = datasetNum
    | otherwise = case ex of
        TechnosphereExchange{techRole = Input, techActivityLinkId = Just link, techFlowId = fid} ->
            M.findWithDefault flowNum (link, fid) (rSupplierNumbers res)
        TechnosphereExchange{} -> flowNum
        BiosphereExchange{} -> flowNum
        -- A waste input is written as a technosphere input, so it names its
        -- treatment the same way. An output carries no link to name.
        WasteExchange{waIsInput = True, waActivityLinkId = Just link, waFlowId = fid} ->
            M.findWithDefault flowNum (link, fid) (rSupplierNumbers res)
        WasteExchange{} -> flowNum
  where
    flowNum = M.findWithDefault datasetNum (exchangeFlowId ex) (rFlowNumbers res)

{- | Common exchange attributes in fixed order: number, name, category,
subCategory, location, unit, meanValue, then optional CAS / generalComment.
Number, name, category, subCategory and unit are the inputs the parser hashes
into the flow UUID, so they must be reproduced verbatim for a stable round-trip.
-}
exchangeAttrs :: Resolvers -> Int -> Exchange -> Text
exchangeAttrs res datasetNum ex =
    " number="
        <> attr (T.pack (show (exchangeNumber res datasetNum ex)))
        <> " name="
        <> attr name
        <> optAttr "category" category
        <> optAttr "subCategory" subCategory
        <> optAttr "location" (exchangeLocation ex)
        <> " unit="
        <> attr (unitNameFor (rUnits res) (exchangeUnitId ex))
        <> " meanValue="
        <> attr (formatAmount (writtenAmount ex))
        <> maybe "" (\c -> " CASNumber=" <> attr c) cas
        <> maybe "" (\c -> " generalComment=" <> attr c) (exchangeComment ex)
  where
    FlowFields name category subCategory cas = flowFields res ex

-- | The amount as the file states it: a substitution is a negative input.
writtenAmount :: Exchange -> Double
writtenAmount ex = case ex of
    TechnosphereExchange{techRole = AvoidedProduct, techAmount = amount} -> negate amount
    TechnosphereExchange{techAmount = amount} -> amount
    BiosphereExchange{} -> exchangeAmount ex
    WasteExchange{} -> exchangeAmount ex

-- | The @\<inputGroup\>@ / @\<outputGroup\>@ element for an exchange.
groupElement :: Exchange -> Text
groupElement ex = case ex of
    TechnosphereExchange{techRole = role} -> case role of
        ReferenceProduct -> wrapOut "0"
        ReferenceInput -> wrapOut "0"
        Coproduct -> wrapOut "1"
        AvoidedProduct -> wrapIn "5"
        Input -> wrapIn "5"
    BiosphereExchange{bioDirection = dir} -> case dir of
        Resource -> wrapIn "4"
        Emission -> wrapOut "4"
    -- An input is the only side of the format that can name a supplier, so a
    -- waste input goes where a technosphere input goes. An output cannot name
    -- one; the linked case is refused before it gets here, and the unlinked
    -- case is an elementary flow.
    WasteExchange{waIsInput = isInput} ->
        if isInput then wrapIn "5" else wrapOut "4"
  where
    wrapIn g = "<inputGroup>" <> g <> "</inputGroup>"
    wrapOut g = "<outputGroup>" <> g <> "</outputGroup>"

-- ----------------------------------------------------------------------------
-- Flow-field resolution (name / category / subCategory / CAS by UUID)
-- ----------------------------------------------------------------------------

{- | The four flow-derived attribute values the parser stored on a flow:
name, category, subCategory, CAS. Positional - always consumed as a whole.
-}
data FlowFields = FlowFields !Text !Text !Text !(Maybe Text)

{- | Resolve a flow's serialised fields from the matching table. A biosphere
flow's compartment becomes category/subCategory; a technosphere flow has no
category; a waste flow takes the shape of the row it is written in, blank for
an input (a technosphere row) and 'Waste' for an output (a biosphere
row, which is the compartment the parser reads back). A UUID absent from its
table yields empty/Nothing - never a crash.
-}
flowFields :: Resolvers -> Exchange -> FlowFields
flowFields res ex = case ex of
    TechnosphereExchange{techFlowId = fid} ->
        case M.lookup fid (rTech res) of
            Just tf -> FlowFields (tfName tf) "" "" (tfCAS tf)
            Nothing -> FlowFields "" "" "" Nothing
    BiosphereExchange{bioFlowId = fid} ->
        case M.lookup fid (rBio res) of
            Just bf ->
                FlowFields
                    (bfName bf)
                    (maybe "" (categoryText . compartmentName) (bfCompartment bf))
                    (fromMaybe "" (bfCompartmentSub bf))
                    (bfCAS bf)
            Nothing -> FlowFields "" "" "" Nothing
    WasteExchange{waFlowId = fid, waIsInput = isInput} ->
        let category = if isInput then "" else mediumText Waste
         in case M.lookup fid (rWaste res) of
                Just wf -> FlowFields (wfName wf) category "" (wfCAS wf)
                Nothing -> FlowFields "" category "" Nothing

{- | A medium as EcoSpold 1 spells it in @category@.

The format writes @resource@ where EcoSpold 2 and ILCD write @natural
resource@, and 'EcoSpold.Parser1' hashes this attribute into the flow's
identity. Emitting anything but the format's own word would give our export a
different identity from the file it came from, and make writing it twice
produce two different files.
-}
categoryText :: Medium -> Text
categoryText = \case
    NaturalResource -> "resource"
    Air -> mediumText Air
    Water -> mediumText Water
    Soil -> mediumText Soil
    InventoryIndicator -> mediumText InventoryIndicator
    Economic -> mediumText Economic
    Waste -> mediumText Waste

{- | Resolve a unit UUID to its name. The parser stored both unit name and
symbol as the source unit string, so the name field is the faithful echo.
-}
unitNameFor :: UnitDB -> UUID -> Text
unitNameFor units uid = maybe "" unitName (M.lookup uid units)

-- ----------------------------------------------------------------------------
-- Canonical encoding helpers
-- ----------------------------------------------------------------------------

-- | Two-space indentation, @n@ levels deep.
indent :: Int -> Text
indent n = T.replicate (n * 2) " "

-- | A double-quoted, XML-escaped attribute value.
attr :: Text -> Text
attr v = "\"" <> escapeXmlAttr v <> "\""

-- | Emit @ name="value"@ only when the value is non-empty; otherwise nothing.
optAttr :: Text -> Text -> Text
optAttr name v
    | T.null v = ""
    | otherwise = " " <> name <> "=" <> attr v

{- | Escape the five XML metacharacters plus the newline / carriage-return
control characters, matching the entity set 'EcoSpold.Common.decodeXmlEntities'
decodes on the way in. Order matters: @&@ is escaped first so we don't
double-escape the entities we introduce.
-}
escapeXmlAttr :: Text -> Text
escapeXmlAttr =
    T.replace "\r" "&#13;"
        . T.replace "\n" "&#10;"
        . T.replace "'" "&apos;"
        . T.replace "\"" "&quot;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

{- | Deterministic textual form for a @meanValue@. Uses the shared
'showFFloatTrim' (fixed-point, never scientific), the exact inverse of
'Amount.readAmount': every finite amount round-trips through that
correctly-rounded reader. 'checkEcoSpold1Exportable' rejects any amount that does
not re-parse, which now leaves only the non-finite.

A non-finite value renders as its (non-parseable) @"NaN"@/@"Infinity"@ form so
a bad re-import fails loudly rather than silently reading @0@; the export guard
rejects non-finite amounts before they reach here.
-}
formatAmount :: Double -> Text
formatAmount x
    | isNaN x || isInfinite x = T.pack (show x)
    | x == 0 = "0.0" -- normalise negative zero (showFFloat would emit "-0.0")
    | otherwise = T.pack (showFFloatTrim x)

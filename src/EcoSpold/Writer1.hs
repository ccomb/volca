{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | EcoSpold1 export writer – the canonical re-emitter for "EcoSpold.Parser1".

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
    source file – that source carried its own numbering, which the writer does
    not preserve;
  * a /linked/ technosphere input is the one exception. Its @number@ attribute
    carries the /supplier's/ dataset number (so the loader can re-link it by
    'techActivityLinkId'); but 'SimpleDatabase' has no field for that link, so a
    bare re-parse drops it and the next write falls back to the positional
    index. Byte-stability of a linked input therefore relies on the full
    loader re-resolving the link (by supplier name/location) rather than on a
    direct 'SimpleDatabase' round-trip. The semantic round-trip – amount,
    role, unit, and the flow name of every exchange but a reference product
    and an input bought inside the file, which say their dataset's name
    ('datasetNames') – is preserved either way;
  * exchange numbers come from the flow, not from the position in the dataset;
  * attributes appear in a fixed order, classification maps are sorted by
    key, numbers use a fixed textual form, and there is no insignificant
    whitespace beyond a single newline between top-level lines;
  * the @timestamp@ is the activity's own creation date, the volatile
    @generator@ is pinned or omitted via 'WriterOptions', and so is the export
    day an undated dataset is dated by, so a write→parse→write round-trip with
    the same options is byte-stable.

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
    writeSimpleDatabase,
    writeSimpleDatabaseChunks,

    -- * Export boundary check
    checkEcoSpold1Exportable,

    -- * What the format makes the export say
    renamedProducts,

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
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as UUID
import Database.ExportSupplier (Maker (..), madeAt, makerOf, noDependencies)
import EcoSpold.Common (showFFloatTrim)
import Types

-- ----------------------------------------------------------------------------
-- Options
-- ----------------------------------------------------------------------------

{- | Knobs controlling the volatile, non-semantic parts of the output.
Keeping these out of band lets a round-trip be byte-stable: write with
'canonicalWriterOptions' (no @generator@) on one day and the second write
reproduces the first exactly. The @timestamp@ is not one of them: it is the
dataset's creation date, and comes from the activity.
-}
data WriterOptions = WriterOptions
    { woGenerator :: Maybe Text
    -- ^ Value of the @\<dataset generator=...\>@ attribute, or 'Nothing' to omit.
    , woExportDay :: Day
    {- ^ The day of the export. A dataset stating no date of its own is written
    with it, since a reader such as Brightway's needs a timestamp and a period
    of validity on every dataset.
    -}
    }
    deriving (Eq, Show)

-- | Self-describing default: pins generator to "VoLCA".
defaultWriterOptions :: Day -> WriterOptions
defaultWriterOptions = WriterOptions (Just "VoLCA")

{- | Omits the volatile generator: given the same day, the output is stable,
for round-trip / golden tests.
-}
canonicalWriterOptions :: Day -> WriterOptions
canonicalWriterOptions = WriterOptions Nothing

-- ----------------------------------------------------------------------------
-- Top-level writers
-- ----------------------------------------------------------------------------

{- | Serialize a 'SimpleDatabase'. Flow / unit names are resolved from its
tables. Runs 'checkEcoSpold1Exportable' first and returns its 'Left' on a
database the format cannot represent faithfully, so an unguarded caller can
never silently emit a lossy or role-flipped file.

The whole file as one value. A caller that sends it somewhere wants
'writeSimpleDatabaseChunks' instead.
-}
writeSimpleDatabase :: WriterOptions -> SimpleDatabase -> Either Text Text
writeSimpleDatabase opts = fmap T.concat . writeSimpleDatabaseChunks opts

{- | The same file as the pieces it can be sent in: the opening envelope, then
one chunk per dataset, then the closing tag. @'T.concat'@ of them is what
'writeSimpleDatabase' answers, to the byte.

The pieces matter because this format has no directory to break a database up:
one archive of aggregated inventories runs to several gigabytes of XML. Held as
one value it is copied twice more before it reaches the socket, and the chunked
transfer encoding writes a single chunk's length in eight hexadecimal digits, so
a body past four gibibytes announces its length modulo 2^32 and the reader finds
XML where the next chunk header belongs. A dataset is about a megabyte, which
answers both.

The list is produced lazily, so a caller that consumes it as it goes holds one
dataset rather than the file.
-}
writeSimpleDatabaseChunks :: WriterOptions -> SimpleDatabase -> Either Text [Text]
writeSimpleDatabaseChunks opts sdb = do
    checkEcoSpold1Exportable sdb
    pure $
        activityChunks
            opts
            (sdbTechFlows sdb)
            (sdbBioFlows sdb)
            (sdbWasteFlows sdb)
            (sdbUnits sdb)
            (placedAtMakers sdb)

{- | Each input linked to a dataset of the file, placed where that dataset
makes its product.

A reader holding the whole file links the input by its @number@. One holding a
dataset alone, beside the database it was taken from, links it by name and
location, the name being its maker's dataset name ('datasetNames'), which that
database gives a product only where activity and product share a name, as one
read from this format does. An input that states no location takes any maker
of that name:
the Italian tap water comes back as the Dutch one. Each country's tap water is
its own product in a file read from this format, so this holds whether the file
makes the product once or several times. An input that states a place other
than its maker's is placed at its maker's, since that is the link the database
scores.
-}
placedAtMakers :: SimpleDatabase -> ActivityMap
placedAtMakers sdb = M.mapWithKey place (sdbActivities sdb)
  where
    maker = makerOf noDependencies sdb
    place key act = act{activityLines = packExchanges (zipWith (placed key) [0 ..] (exchanges act))}
    placed key i ex@TechnosphereExchange{} = case maker key (i, ex) of
        SoleMaker m -> ex{techLocation = readExchangeLocation (madeAt m)}
        OneOfMakers m -> ex{techLocation = readExchangeLocation (madeAt m)}
        _ -> ex
    placed _ _ ex = ex

{- | Serialize the database's activities against the flow / unit tables that
resolve their exchange UUIDs, one chunk per dataset between the envelope's two
halves. Internal: it does no export-boundary checking, so it is reached only
through 'writeSimpleDatabaseChunks', which runs 'checkEcoSpold1Exportable'
first. Not exported, so no caller can bypass the guard.
-}
activityChunks ::
    WriterOptions ->
    TechFlowDB ->
    BioFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    ActivityMap ->
    [Text]
activityChunks opts techs bios wastes units activities =
    T.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        ]
        : zipWith dataset [1 ..] ordered
        ++ [T.unlines ["</ecoSpold>"]]
  where
    dataset :: Int -> ((UUID, UUID), Activity) -> Text
    dataset num row = T.unlines (datasetLines opts res num row)
    res = Resolvers techs bios wastes units (supplierNumberIndex ordered) (flowNumberIndex ordered) (datasetNames techs ordered)
    -- Stable, content-derived order so dataset numbers (and thus flow UUIDs)
    -- are reproducible regardless of the source Map's ordering.
    ordered = orderedActivities techs activities

{- | The one name each dataset is written under, by its row key.

EcoSpold 1 gives a dataset a single name: its @referenceFunction@, its
reference exchange and every input buying from it say the same words, and a
reader linking by name, as Brightway's does, matches an input on them. It is
the activity's name, which tells the activities of a file apart where their
products need not: one file can make one yarn by three spinning processes,
each its own activity. Activities sharing a name at one place are the datasets of one
allocated activity, one per coproduct, and they keep their products' names,
which tell them apart within that activity ('renamedProducts' says so).
-}
datasetNames :: TechFlowDB -> [((UUID, UUID), Activity)] -> M.Map (UUID, UUID) Text
datasetNames techs ordered = M.fromList [(key, named key act) | (key, act) <- ordered]
  where
    place act = (activityName act, activityLocation act)
    shared = M.keysSet (M.filter (> (1 :: Int)) (M.fromListWith (+) [(place act, 1) | (_, act) <- ordered]))
    named (_, productId) act
        | S.member (place act) shared = maybe (activityName act) tfName (M.lookup productId techs)
        | otherwise = activityName act

{- | What writing one name per dataset costs, each said once with the first
case: the products written under the name of the activity making them, and
the datasets of activities making several products at one place, written
under their product's name, the activity's name left out of the file.
-}
renamedProducts :: SimpleDatabase -> [Text]
renamedProducts sdb = products renamed ++ datasets unnamed
  where
    products = \case
        [] -> []
        [(productName, name)] -> ["EcoSpold 1 gives a dataset one name: the product \"" <> productName <> "\" is written under the name of the activity making it, \"" <> name <> "\"."]
        all'@((productName, name) : _) ->
            [ "EcoSpold 1 gives a dataset one name: "
                <> T.pack (show (length all'))
                <> " products are written under the name of the activity making them, \""
                <> productName
                <> "\" as \""
                <> name
                <> "\" first."
            ]
    datasets = \case
        [] -> []
        all'@(activity : _) ->
            [ "EcoSpold 1 gives a dataset one name: "
                <> T.pack (show (length all'))
                <> " datasets are written under their product's name, their activity making several products at one place, so the activity's name is not in the file, \""
                <> activity
                <> "\" first."
            ]
    ordered = orderedActivities (sdbTechFlows sdb) (sdbActivities sdb)
    names = datasetNames (sdbTechFlows sdb) ordered
    unnamed = [activityName act | (key, act) <- ordered, M.lookup key names /= Just (activityName act)]
    renamed =
        [ (tfName flow, name)
        | (key@(_, productId), _) <- ordered
        , Just name <- [M.lookup key names]
        , Just flow <- [M.lookup productId (sdbTechFlows sdb)]
        , tfName flow /= name
        ]

{- | Canonical export order: sorted by the name each dataset is written under
('datasetNames') and its location, so dataset numbers (and thus flow UUIDs)
are reproducible regardless of the source Map's ordering, and come out the
same once the file is read back under those names. Each activity is paired with the stored activity UUID – the first
component of its 'sdbActivities' key – that a technosphere input's
'techActivityLinkId' points at. Shared by the writer and
'checkEcoSpold1Exportable'.
-}
orderedActivities :: TechFlowDB -> ActivityMap -> [((UUID, UUID), Activity)]
orderedActivities techs activities = sortOn (\(key, a) -> (M.findWithDefault (activityName a) key names, activityLocation a)) rows
  where
    rows = M.toList activities
    names = datasetNames techs rows

{- | Map each exported row to the dataset number it is assigned in canonical
order. A technosphere input names its supplier by the pair 'techActivityLinkId'
plus 'techFlowId' – the supplier row's 'sdbActivities' key – and the parser reads
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
    supplier row the link names – the (activity, product) pair – is itself being
    exported (present in 'supplierNumberIndex'). A linked input naming a pair
    absent from the database would otherwise force a positional index the parser
    would misread as a different supplier.

  * __Reference inputs.__ EcoSpold1 has no marker for a reference /input/, so
    the writer would emit @outputGroup 0@ and the parser would read it back as
    a reference product – a direction flip.

  * __Waste outputs naming a treatment.__ The format names a supplier only
    from an input's @number@ attribute, so the link of a waste /output/ cannot
    be written. Emitting the row anyway would re-import it as waste nothing
    treats, silently deleting the treatment it named.

  * __Waste-marker collision.__ A biosphere flow whose compartment name is
    @"Final waste flows"@ would re-import under compartment @"waste"@, because
    the parser reads that category as the marker of a final waste flow – a
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
                        <> " product – a direction flip from input to output."
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
    index = supplierNumberIndex (orderedActivities (sdbTechFlows db) (sdbActivities db))
    danglingLinks =
        [ (activityName act, link)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , Just (link, fid) <- [suppliedBy ex]
        , not (M.member (link, fid) index)
        ]
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
        , TechnosphereExchange{techRole = ReferenceInput} <- nonPlainExchanges act
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
    , rDatasetNames :: !(M.Map (UUID, UUID) Text)
    }

-- ----------------------------------------------------------------------------
-- Per-dataset rendering
-- ----------------------------------------------------------------------------

{- | Render one @\<dataset\>@ block (already indented), as a list of lines.

Past the reference function and the place, the metadata is what a reader of
the format cannot do without: Brightway's dereferences the technology, the
period, the dataset information and the person who entered the data on every
dataset, and stops on the first one missing. Each says what the dataset stated,
and no more: the technology it described, its date, and otherwise the day of
the export, said as such.
-}
datasetLines :: WriterOptions -> Resolvers -> Int -> ((UUID, UUID), Activity) -> [Text]
datasetLines opts res num (key, act) =
    [ indent 1 <> "<dataset" <> datasetAttrs opts num (activityDates act) <> ">"
    , indent 2 <> "<metaInformation>"
    , indent 3 <> "<processInformation>"
    , indent 4 <> "<referenceFunction" <> refFunctionAttrs (M.findWithDefault (activityName act) key (rDatasetNames res)) act <> "/>"
    , indent 4 <> "<geography location=" <> attr (activityLocation act) <> "/>"
    , indent 4 <> "<technology text=" <> attr (documented "Technology" act) <> "/>"
    ]
        ++ periodLines dated
        ++ [ indent 4 <> "<dataSetInformation timestamp=" <> attr (isoDateTime (datedDay dated)) <> "/>"
           , indent 3 <> "</processInformation>"
           , indent 3 <> "<modellingAndValidation/>"
           , indent 3 <> "<administrativeInformation>"
           , indent 4 <> "<dataEntryBy person=\"1\"/>"
           , indent 4 <> "<person number=\"1\" name=\"VoLCA\"/>"
           , indent 3 <> "</administrativeInformation>"
           , indent 2 <> "</metaInformation>"
           , indent 2 <> "<flowData>"
           ]
        ++ concatMap (exchangeLines res num key) (exchanges act)
        ++ [ indent 2 <> "</flowData>"
           , indent 1 <> "</dataset>"
           ]
  where
    dated = datedBy opts (activityDates act)

-- | The text of the documentation section a dataset states under this label, or nothing.
documented :: Text -> Activity -> Text
documented label act = T.intercalate "\n" [docText s | s <- activityDocumentation act, docLabel s == label]

{- | The day a dataset is dated by: its creation, the one date the format
reads back, or the export's.
-}
data Dated
    = Created !Day
    | ExportedOn !Day

datedDay :: Dated -> Day
datedDay = \case
    Created day -> day
    ExportedOn day -> day

{- | The day the dataset was created, else the day of the export. A revision
or an unnamed date is not taken: the format reads back only the creation, and
the next export would date the dataset otherwise.
-}
datedBy :: WriterOptions -> DatasetDates -> Dated
datedBy opts = maybe (ExportedOn (woExportDay opts)) Created . datesCreated

{- | The period a dataset is valid for, as the whole year of its date, and a
text saying where that year comes from. A period a source stated is kept only
as the text of its documentation, which the format has no dates for.
-}
periodLines :: Dated -> [Text]
periodLines dated =
    [ indent 4 <> "<timePeriod text=" <> attr note <> ">"
    , indent 5 <> "<startDate>" <> T.pack (iso8601Show (fromGregorian year 1 1)) <> "</startDate>"
    , indent 5 <> "<endDate>" <> T.pack (iso8601Show (fromGregorian year 12 31)) <> "</endDate>"
    , indent 4 <> "</timePeriod>"
    ]
  where
    (year, _, _) = toGregorian (datedDay dated)
    note = case dated of
        Created _ -> "The year the dataset was created."
        ExportedOn _ -> "The source states no creation date: the year of the export."

{- | @\<dataset\>@ attributes: @number@ always, the volatile @generator@ when
the options provide it, and @timestamp@ when the dataset states its creation,
the one date this format has a place for.
-}
datasetAttrs :: WriterOptions -> Int -> DatasetDates -> Text
datasetAttrs opts num dates =
    " number="
        <> attr (T.pack (show num))
        <> maybe "" (\g -> " generator=" <> attr g) (woGenerator opts)
        <> maybe "" (\d -> " timestamp=" <> attr (isoDateTime d)) (datesCreated dates)

{- | @\<referenceFunction\>@ attributes in a fixed order. @category@ /
@subCategory@ come from 'activityClassification' (the parser's @Category@ /
@SubCategory@ keys); a non-empty activity description is joined and emitted
as @generalComment@.

Note: @generalComment@ is a single ES1 attribute, so paragraph cardinality
does not round-trip – the joined text survives, but 'Parser1' reads it back
as a one-element 'activityDescription' regardless of how many paragraphs went in.
-}
refFunctionAttrs :: Text -> Activity -> Text
refFunctionAttrs name act =
    " name="
        <> attr name
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
exchangeLines :: Resolvers -> Int -> (UUID, UUID) -> Exchange -> [Text]
exchangeLines res datasetNum key ex =
    [ indent 3 <> "<exchange" <> exchangeAttrs res datasetNum key ex <> ">"
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
being written as a technosphere input. Everything else – co-products,
biosphere, waste outputs, and unlinked inputs – carries the number its flow was
given once for the whole export ('flowNumberIndex').

'checkEcoSpold1Exportable' guarantees any resolved link's (activity, product)
pair is present in the supplier index before export, so that fallback is
unreachable on the wired-up path; the flow index covers every non-reference
exchange by construction.
-}
exchangeNumber :: Resolvers -> Int -> Exchange -> Int
exchangeNumber res datasetNum ex
    | exchangeIsReference ex = datasetNum
    | Just supplier <- suppliedBy ex = M.findWithDefault flowNum supplier (rSupplierNumbers res)
    | otherwise = flowNum
  where
    flowNum = M.findWithDefault datasetNum (exchangeFlowId ex) (rFlowNumbers res)

{- | The supplier row whose dataset number and name an exchange carries: a
technosphere input's, an avoided product's, which the format writes as an
input and reads back as one, and a waste input's, written as a technosphere
input so that it names its treatment the same way. An output names none, on
either axis.
-}
suppliedBy :: Exchange -> Maybe (UUID, UUID)
suppliedBy = \case
    TechnosphereExchange{techRole = role, techActivityLinkId = Just link, techFlowId = fid}
        | role == Input || role == AvoidedProduct -> Just (link, fid)
    TechnosphereExchange{} -> Nothing
    BiosphereExchange{} -> Nothing
    WasteExchange{waIsInput = True, waActivityLinkId = Just link, waFlowId = fid} -> Just (link, fid)
    WasteExchange{} -> Nothing

{- | Common exchange attributes in fixed order: number, name, category,
subCategory, location, unit, meanValue, then optional CAS / generalComment.
Number, name, category, subCategory and unit are the inputs the parser hashes
into the flow UUID, so they must be reproduced verbatim for a stable round-trip.

A reference product, and an input carrying the number of a dataset of the
file, say that dataset's name ('datasetNames'), the one name the format has for
it; every other exchange says its flow's.
-}
exchangeAttrs :: Resolvers -> Int -> (UUID, UUID) -> Exchange -> Text
exchangeAttrs res datasetNum key ex =
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
    FlowFields flowName category subCategory cas = flowFields res ex
    dataset = if exchangeIsReference ex then Just key else suppliedBy ex
    name = fromMaybe flowName (dataset >>= (`M.lookup` rDatasetNames res))

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
name, category, subCategory, CAS. Positional – always consumed as a whole.
-}
data FlowFields = FlowFields !Text !Text !Text !(Maybe Text)

{- | Resolve a flow's serialised fields from the matching table. A biosphere
flow's compartment becomes category/subCategory; a technosphere flow has no
category; a waste flow takes the shape of the row it is written in, blank for
an input (a technosphere row) and 'Waste' for an output (a biosphere
row, which is the compartment the parser reads back). A UUID absent from its
table yields empty/Nothing – never a crash.
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
    Social -> mediumText Social
    Immaterial -> "non material emissions"

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

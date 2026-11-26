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
    dataset's number, a coproduct of a block kept whole a number of its own in
    that block, a linked technosphere input carries its supplier's (one
    bought from a database it depends on, a number no dataset of the file
    has), and every other exchange carries a number assigned once per
    distinct flow.
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
    ('datasetNames'), and an input bought from a database it depends on, which
    says its supplier's activity name – is preserved either way;
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
  * 'Coproduct'                          → @\<outputGroup\>2\</outputGroup\>@
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

A block of several products kept whole is one dataset: its reference product
in group 0 and each coproduct in group 2, each line named after its product,
the @referenceFunction@ after the activity. When its products are weighed, by
the prices they state and failing that by the shares they declare, the dataset
ends with one @\<allocation\>@ per product over every line they share, at its
share scaled to 100, and one giving each product the whole of its own line;
stated prices make the key economic (@allocationMethod="1"@). A block nothing
weighs is written without keys, and an input buying one of its coproducts is
refused: read back, the block states no share and answers no such input. Keys
address lines by number, so one flow stated on several lines of a dataset is
written as one line of their total, and a dataset with keys whose lines still
share a number is refused.

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
    unlinkableByProduct,
    blockWarnings,

    -- * A dataset as the file writes it (exported for testing)
    Written1 (..),
    writtenDatasets,

    -- * Pure helpers (exported for testing)
    escapeXmlAttr,
    formatAmount,
) where

import Amount (readAmount)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Either (lefts)
import Data.Indexing (groupInOrder)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as UUID
import Database.Allocation (Weights (..), blockWeights, isCoproduct, producerKeyOf)
import Database.CrossLinking (normalizeText)
import Database.ExportSupplier (Dependencies, Elsewhere (..), Maker (..), madeAt, makerOf, noDependencies)
import EcoSpold.Allocation1 (Allocation1 (..))
import EcoSpold.Common (showFFloatTrim)
import Types
import UnitConversion (UnitConfig, defaultUnitConfig)

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
    , woDependencies :: Dependencies
    {- ^ The suppliers its inputs buy from the databases it depends on, each
    such input written under its supplier's name, place and unit.
    -}
    , woUnits :: UnitConfig
    -- ^ What each unit is worth, to compare the prices weighing the products of a block.
    }
    deriving (Eq, Show)

-- | Self-describing default: pins generator to "VoLCA".
defaultWriterOptions :: Day -> WriterOptions
defaultWriterOptions day = WriterOptions (Just "VoLCA") day noDependencies defaultUnitConfig

{- | Omits the volatile generator: given the same day, the output is stable,
for round-trip / golden tests.
-}
canonicalWriterOptions :: Day -> WriterOptions
canonicalWriterOptions day = WriterOptions Nothing day noDependencies defaultUnitConfig

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
    checkEcoSpold1Exportable opts sdb
    pure (activityChunks opts (prepared opts sdb))

{- | What the writer and its guard both work from: the datasets in the order
they are numbered in, and the tables resolving their lines.
-}
data Prepared = Prepared !Resolvers ![((UUID, UUID), Activity)]

prepared :: WriterOptions -> SimpleDatabase -> Prepared
prepared opts sdb =
    Prepared
        Resolvers
            { rTech = techs
            , rBio = sdbBioFlows sdb
            , rWaste = sdbWasteFlows sdb
            , rUnits = sdbUnits sdb
            , rSupplierNumbers = suppliers
            , rFlowNumbers = flowNumberIndex (flowFieldsIn techs (sdbBioFlows sdb) (sdbWasteFlows sdb)) (1 + foldr max 0 suppliers) ordered
            , rDatasetNames = datasetNames techs ordered <> M.fromList (supplierNames deps sdb)
            , rActivities = activities
            , rUnitConfig = woUnits opts
            }
        ordered
  where
    deps = woDependencies opts
    techs = sdbTechFlows sdb
    activities = placedAtMakers deps sdb
    -- Stable, content-derived order so dataset numbers (and thus flow UUIDs)
    -- are reproducible regardless of the source Map's ordering.
    ordered = orderedActivities techs activities
    suppliers :: M.Map (UUID, UUID) Int
    suppliers = supplierNumberIndex ordered

-- | A line a database it depends on supplies.
data BoughtLine = BoughtLine
    { blRow :: !(UUID, UUID)
    , blPosition :: !Int
    -- ^ Its place among the row's exchanges.
    , blActivity :: !Activity
    , blExchange :: !Exchange
    , blSupplier :: !Elsewhere
    }

-- | Each line a database it depends on supplies.
dependencyLines :: Dependencies -> SimpleDatabase -> [BoughtLine]
dependencyLines deps sdb =
    [ BoughtLine row position act ex e
    | (row, act) <- M.toList (sdbActivities sdb)
    , (position, ex) <- zip [0 ..] (exchanges act)
    , InDependency e <- [maker row (position, ex)]
    ]
  where
    maker = makerOf deps sdb

-- | The activity name each supplier in a database it depends on is written under, by supplier row.
supplierNames :: Dependencies -> SimpleDatabase -> [((UUID, UUID), Text)]
supplierNames deps sdb = [(supplier, elActivity (blSupplier l)) | l <- dependencyLines deps sdb, Just supplier <- [suppliedBy (blExchange l)]]

{- | The lines bought from a database it depends on whose supplier's activity
is named apart from its product: they are written under the activity's name,
which a reader holding that database links them by, and which this engine,
linking an input by its product, does not find.
-}
unlinkableByProduct :: Dependencies -> SimpleDatabase -> [Text]
unlinkableByProduct deps sdb = case apart of
    [] -> []
    (activity, productName) : _ ->
        [ "EcoSpold 1 writes an input bought from a database it depends on under its supplier's activity name: "
            <> T.pack (show (length apart))
            <> " such inputs name a supplier whose product is named otherwise, which a reader linking by product, as VoLCA does, does not find, \""
            <> activity
            <> "\" making \""
            <> productName
            <> "\" first."
        ]
  where
    apart =
        [ (elActivity (blSupplier l), tfName flow)
        | l <- dependencyLines deps sdb
        , TechnosphereExchange{techFlowId = fid} <- [blExchange l]
        , Just flow <- [M.lookup fid (sdbTechFlows sdb)]
        , normalizeText (tfName flow) /= normalizeText (elActivity (blSupplier l))
        ]

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
scores. An input bought from a database it depends on is placed where its
supplier is, for a reader holding that database beside the file.
-}
placedAtMakers :: Dependencies -> SimpleDatabase -> ActivityMap
placedAtMakers deps sdb = M.mapWithKey place (sdbActivities sdb)
  where
    maker = makerOf deps sdb
    place key act = act{activityLines = packExchanges (zipWith (placed key) [0 ..] (exchanges act))}
    placed key i ex@TechnosphereExchange{} = case maker key (i, ex) of
        SoleMaker m -> ex{techLocation = readExchangeLocation (madeAt m)}
        OneOfMakers m -> ex{techLocation = readExchangeLocation (madeAt m)}
        InDependency e -> ex{techLocation = readExchangeLocation (elLocation e)}
        _ -> ex
    placed _ _ ex = ex

{- | Serialize the database's activities against the flow / unit tables that
resolve their exchange UUIDs, one chunk per dataset between the envelope's two
halves. Internal: it does no export-boundary checking, so it is reached only
through 'writeSimpleDatabaseChunks', which runs 'checkEcoSpold1Exportable'
first. Not exported, so no caller can bypass the guard.
-}
activityChunks :: WriterOptions -> Prepared -> [Text]
activityChunks opts (Prepared res ordered) =
    T.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        ]
        : zipWith dataset [1 ..] ordered
        ++ [T.unlines ["</ecoSpold>"]]
  where
    dataset :: Int -> ((UUID, UUID), Activity) -> Text
    dataset num row = T.unlines (datasetLines opts res num row)

{- | The name each dataset's reference exchange, and every input buying its
reference product, is written under, by its row key.

EcoSpold 1 gives a dataset making one product a single name: its
@referenceFunction@, its reference exchange and every input buying from it say
the same words, and a reader linking by name, as Brightway's does, matches an
input on them. It is the activity's name, which tells the activities of a file
apart where their products need not: one file can make one yarn by three
spinning processes, each its own activity. Activities sharing a name at one
place are the datasets of one allocated activity, one per coproduct, and they
keep their products' names, which tell them apart within that activity
('renamedProducts' says so).

A dataset making several products names each product line after its product,
and its @referenceFunction@ after the activity ('referenceFunctionName'): the
products are what its buyers name, a reader linking each to the product line of
that name, and the activity's name is what is left to tell it by.
-}
datasetNames :: TechFlowDB -> [((UUID, UUID), Activity)] -> M.Map (UUID, UUID) Text
datasetNames techs ordered = M.fromList [(key, named key act) | (key, act) <- ordered]
  where
    place act = (activityName act, activityLocation act)
    shared = M.keysSet (M.filter (> (1 :: Int)) (M.fromListWith (+) [(place act, 1) | (_, act) <- ordered]))
    named (_, productId) act
        | makesCoproducts act || S.member (place act) shared = maybe (activityName act) tfName (M.lookup productId techs)
        | otherwise = activityName act

-- | The @referenceFunction@ name of a dataset: its activity's where it makes several products, else the name its buyers say.
referenceFunctionName :: Resolvers -> (UUID, UUID) -> Activity -> Text
referenceFunctionName res key act
    | makesCoproducts act = activityName act
    | otherwise = M.findWithDefault (activityName act) key (rDatasetNames res)

-- | Whether a dataset makes a product beside its reference.
makesCoproducts :: Activity -> Bool
makesCoproducts act = any isCoproduct (nonPlainExchanges act)

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
    unnamed = [activityName act | (key, act) <- ordered, not (makesCoproducts act), M.lookup key names /= Just (activityName act)]
    renamed =
        [ (tfName flow, name)
        | (key@(_, productId), _) <- ordered
        , Just name <- [M.lookup key names]
        , Just flow <- [M.lookup productId (sdbTechFlows sdb)]
        , tfName flow /= name
        ]

{- | What writing the datasets of several products, and merging repeated
lines, costs the export: each kind once, with the first datasets it concerns.
-}
blockWarnings :: WriterOptions -> SimpleDatabase -> [Text]
blockWarnings opts sdb = mapMaybe counted kinds
  where
    written = writtenDatasets opts sdb

    counted :: (Text, [Text]) -> Maybe Text
    counted (what, named) = case named of
        [] -> Nothing
        _ -> Just ("EcoSpold 1: " <> T.pack (show (length named)) <> " " <> what <> foldMap ("\n  " <>) (take 10 named))

    kinds :: [(Text, [Text])]
    kinds =
        [ ("datasets of several products are written without allocation keys, which leaves a reader nothing to divide them by:", [nameOf w <> ": " <> why | w <- written, Unkeyed why <- [w1Division w]])
        , ("datasets of several products are divided by the shares their products declare, since their prices cannot weigh them:", [nameOf w <> ": " <> why | w <- written, Keyed (SharesOverPrices why) _ <- [w1Division w]])
        , ("datasets declare shares adding up to other than 100 %, written scaled to 100 %:", [nameOf w <> " (" <> T.pack (show total) <> " %)" | w <- byShares, let total = sum (map dsPercent (declared w)), abs (total - 100) > 0.01])
        , ("datasets have a share formula, which the format has no place for, and are written at the share's value:", [nameOf w | w <- byShares, any (isJust . dsFormula) (declared w)])
        , ("datasets state one flow on several lines, written as one line of their total, the comment of the first kept:", [nameOf w | w <- written, length (w1Lines w) < length (exchanges (w1Activity w))])
        ]

    byShares :: [Written1]
    byShares = [w | w <- written, Keyed basis _ <- [w1Division w], fromShares basis]

    fromShares :: KeyBasis -> Bool
    fromShares = \case
        FromPrices -> False
        FromShares -> True
        SharesOverPrices _ -> True

    nameOf :: Written1 -> Text
    nameOf = activityName . w1Activity

    declared :: Written1 -> [DeclaredShare]
    declared w = mapMaybe exchangeDeclaredShare (filter exchangeIsProductOutput (exchanges (w1Activity w)))

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

A coproduct of a block kept whole is a supplier too, under the pair of the
block's activity and its own flow ('producerKeyOf'). It gets a number of its own
after the datasets', which its line in the block carries and its buyers quote:
two blocks making one product are two suppliers of it, as two datasets making
one reference product are, and a buyer of one must read back linked to that one.
-}
supplierNumberIndex :: [((UUID, UUID), Activity)] -> M.Map (UUID, UUID) Int
supplierNumberIndex ordered =
    M.fromList (zip (map fst ordered <> coproducts) [1 ..])
  where
    coproducts :: [(UUID, UUID)]
    coproducts = [(activity, exchangeFlowId ex) | ((activity, _), act) <- ordered, ex <- exchanges act, isCoproduct ex]

{- | Map each flow that is not a reference product to the number the export
gives it, once, across every dataset that carries it.

EcoSpold1 numbers a flow for the whole export, not for the dataset it appears
in, and the parser reads that number as part of the flow's identity. Numbering
by position instead would re-import one substance as one flow per position it
happens to occupy, which is exactly the splintering the parser stopped doing.

Numbers start at the first one no supplier carries ('supplierNumberIndex'):
the suppliers' numbers are what a product line and a linked input carry.

The flows are numbered in the order of what the file says of them, their
identifier only breaking a tie. The parser makes a flow's identifier from its
number, so an order by identifier would number the flows of the file read back
otherwise, and writing it again would not give the same file.
-}
flowNumberIndex :: (Exchange -> FlowFields) -> Int -> [((UUID, UUID), Activity)] -> M.Map UUID Int
flowNumberIndex fieldsOf first ordered =
    M.fromList (zip (map snd (S.toAscList flows)) [first ..])
  where
    flows :: S.Set (FlowFields, UUID)
    flows =
        S.fromList
            [ (fields, flow)
            | (flow, fields) <-
                M.toList $
                    M.fromListWith
                        min
                        [ (exchangeFlowId ex, fieldsOf ex)
                        | (_, act) <- ordered
                        , ex <- exchanges act
                        , not (exchangeIsReference ex)
                        ]
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

  * __Suppliers in a database it depends on.__ Such an input is written under
    its supplier's name, place and unit, for a reader holding that database
    beside the file to link it there. It cannot be when the supplier states no
    place, shares its activity name and place with another product of its database,
    or makes its product in a unit the line is not in; nor when a dataset of
    the file has its name, since readers link an input inside the file first.
    A waste sent to a treatment of that database has no line naming it.

  * __Datasets of several products the format cannot tell apart.__ One with
    no reference or two, or making one product on two lines; and one where
    two lines carry one number once repeated lines are merged ('Written1'),
    since an allocation key names a line by its number, a number names a flow,
    and a product's key leaves out the line carrying its own number.

  * __Coproducts nothing answers once read back.__ An input buying a coproduct
    of a block kept whole names it by the block's number and the product's
    name; a block written without allocation keys reads back stating no
    share, kept whole by nothing, and answers no such input.

Databases free of all of these pass unchanged.
-}
checkEcoSpold1Exportable :: WriterOptions -> SimpleDatabase -> Either Text ()
checkEcoSpold1Exportable opts db =
    case lefts [checkLinks, checkDependencies, checkSupplierNames, checkRefInputs, checkLinkedWasteOutputs, checkFlows, checkUnits, checkAmounts, checkAmountRoundTrip, checkProducts, checkNumbers] of
        [] -> Right ()
        violations -> Left (T.intercalate "\n\n" violations)
  where
    deps = woDependencies opts
    Prepared res ordered = prepared opts db
    written = writtenDatasets opts db
    checkProducts =
        case [(activityName act, why) | (_, act) <- ordered, makesCoproducts act, Just why <- [productsUntold act]] of
            [] -> Right ()
            ((producer, why) : _) ->
                Left $
                    "EcoSpold1 export cannot write the products of \""
                        <> producer
                        <> "\": "
                        <> why
                        <> ", which the format cannot tell apart."
    productsUntold act
        | references /= 1 = Just (T.pack (show references) <> " of them are its reference")
        | length (nubOrd (map exchangeFlowId products)) < length products = Just "it makes one product on two lines"
        | otherwise = Nothing
      where
        products = filter exchangeIsProductOutput (nonPlainExchanges act)
        references = length (filter exchangeIsReference products)
    checkNumbers =
        case [(activityName (w1Activity w), n, names) | w <- written, makesCoproducts (w1Activity w), (n, names@(_ :| _ : _)) <- M.toList (numberedNames res w)] of
            [] -> Right ()
            ((producer, n, names) : _) ->
                Left $
                    "EcoSpold1 export cannot write the allocation keys of \""
                        <> producer
                        <> "\": the lines "
                        <> T.intercalate " and " (map (\name -> "\"" <> name <> "\"") (NE.toList names))
                        <> " carry one number, "
                        <> T.pack (show n)
                        <> ", which the format reads as one flow, so a key cannot tell them apart."
    checkDependencies =
        case [(activityName (blActivity l), why) | l <- dependencyLines deps db, Just why <- [unwritable (blExchange l) (blSupplier l)]] of
            [] -> Right ()
            ((consumer, why) : _) ->
                Left $
                    "EcoSpold1 export cannot name the supplier of an input in \""
                        <> consumer
                        <> "\": "
                        <> why
                        <> ". EcoSpold 2 and openLCA point at the supplier itself."
    -- One supplier row written under two names would be one dataset for a reader and two for us.
    checkSupplierNames =
        case [supplier | (supplier, names) <- M.toList (M.fromListWith S.union [(supplier, S.singleton name) | (supplier, name) <- supplierNames deps db]), S.size names > 1] of
            [] -> Right ()
            ((act, _) : _) ->
                Left $
                    "EcoSpold1 export cannot name the supplier "
                        <> UUID.toText act
                        <> ": the databases it depends on give it more than one activity name."
    -- A reader links an input inside the file by the name its buyers say, or by the dataset's own.
    ownNames = S.fromList (map normalizeText (M.elems (datasetNames (sdbTechFlows db) ordered) <> [activityName act | (_, act) <- ordered, makesCoproducts act]))
    unwritable ex e = case ex of
        TechnosphereExchange{techUnitId = unit}
            | T.null (T.strip (elLocation e)) -> Just (supplier <> " states no location")
            | other : _ <- elNamesakes e -> Just (elDatabase e <> " also makes \"" <> other <> "\" at " <> elLocation e <> " under the name \"" <> elActivity e <> "\"")
            | S.member (normalizeText (elActivity e)) ownNames -> Just (supplier <> " has the name of a dataset of the file, which a reader links it to instead")
            | T.null (elUnit e) -> Just ("the unit " <> supplier <> " makes it in is not known")
            | unitNameFor (sdbUnits db) unit /= elUnit e -> Just ("it is in " <> unitNameFor (sdbUnits db) unit <> ", which does not convert to the " <> elUnit e <> " " <> supplier <> " makes it in")
            | otherwise -> Nothing
          where
            supplier = "its supplier \"" <> elActivity e <> "\" in " <> elDatabase e
        WasteExchange{} -> Just ("it sends a waste to \"" <> elActivity e <> "\" in " <> elDatabase e <> ", and the format names a treatment only among the datasets of its own file")
        BiosphereExchange{} -> Nothing
    checkLinks =
        case danglingLinks of
            [] -> Right ()
            ((consumer, why) : _) ->
                Left $
                    "EcoSpold1 export cannot encode the supplier link of an input in \""
                        <> consumer
                        <> "\": "
                        <> why
                        <> "."
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
    bought = S.fromList [(blRow l, blPosition l) | l <- dependencyLines deps db]
    danglingLinks =
        [ (activityName act, why)
        | (row, act) <- M.toList (sdbActivities db)
        , (position, ex) <- zip [0 :: Int ..] (exchanges act)
        , S.notMember (row, position) bought
        , Just supplier <- [suppliedBy ex]
        , Just why <- [unanswered supplier]
        ]
    unanswered :: (UUID, UUID) -> Maybe Text
    unanswered supplier@(link, flow) = case producerKeyOf (sdbActivities db) supplier of
        Nothing
            | (block : _) <- [act | ((a, _), act) <- M.toList (sdbActivities db), a == link, any (\ex -> isCoproduct ex && exchangeFlowId ex == flow) (exchanges act)] ->
                Just ("it buys a coproduct of \"" <> activityName block <> "\", which does not answer it, its products not all stating a share or another block of the activity making it too")
            | otherwise -> Just ("linked activity " <> UUID.toText link <> " is not among the exported datasets, so its dataset number is unknown")
        Just key
            | key /= supplier
            , Just block <- M.lookup key (sdbActivities db)
            , Unkeyed why <- divisionOf (woUnits opts) (sdbUnits db) block ->
                Just ("it buys a coproduct of \"" <> activityName block <> "\", which the file writes without allocation keys (" <> why <> "): read back, that block states no share and answers no input buying its coproduct")
            | otherwise -> Nothing
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
    -- ^ The name each supplier row is written under: a dataset of the file, or the activity of a dependency.
    , rActivities :: !ActivityMap
    -- ^ The datasets of the file, by row key, to find the block answering an input that buys its coproduct.
    , rUnitConfig :: !UnitConfig
    }

-- ----------------------------------------------------------------------------
-- A dataset as the file writes it
-- ----------------------------------------------------------------------------

{- | One dataset as the file writes it: its lines under the number each is
written with, and the allocation keys dividing them between its products.

The number of a line names its flow across the whole export ('exchangeNumber'),
and the parser hashes it into the flow's identity. Two lines of one dataset
carrying one number in one shape (same side, same flow, unit and place) are
therefore one flow to any reader, and are written as one line of their total:
a fresh number would split one substance into two flows on reading back, and
the format wants a line's number unique within its dataset. The first line's
comment is the one kept ('blockWarnings' names the datasets).
-}
data Written1 = Written1
    { w1Key :: !(UUID, UUID)
    , w1Activity :: !Activity
    , w1Number :: !Int
    , w1Lines :: ![(Int, Exchange)]
    , w1Division :: !Division1
    , w1Keys :: ![Allocation1]
    }

-- | Every dataset of the export as the file writes it, in the order they are numbered in.
writtenDatasets :: WriterOptions -> SimpleDatabase -> [Written1]
writtenDatasets opts sdb = zipWith (written1 res) [1 ..] ordered
  where
    Prepared res ordered = prepared opts sdb

written1 :: Resolvers -> Int -> ((UUID, UUID), Activity) -> Written1
written1 res num (key, act) =
    Written1
        { w1Key = key
        , w1Activity = act
        , w1Number = num
        , w1Lines = numbered
        , w1Division = division
        , w1Keys = case division of
            Keyed _ shares -> keysOf shares numbered
            SingleProduct -> []
            Unkeyed _ -> []
        }
  where
    numbered = mergeRepeated [(exchangeNumber res key num ex, ex) | ex <- exchanges act]
    division = divisionOf (rUnitConfig res) (rUnits res) act

-- | Where a line goes among the lines of its dataset: a product apart, any other line with those of its number and shape.
type Shape = Either Int (Int, Text, Bool, UUID, UUID, Text)

{- | Lines of one number and one shape merged into the first of them, carrying
their total. A product line is never merged: two of them are refused.
-}
mergeRepeated :: [(Int, Exchange)] -> [(Int, Exchange)]
mergeRepeated numbered = [(n, total shape ex) | (shape, (n, ex)) <- nubOrdOn fst shaped]
  where
    shaped :: [(Shape, (Int, Exchange))]
    shaped = [(shapeOf i line, line) | (i, line) <- zip [0 ..] numbered]

    shapeOf :: Int -> (Int, Exchange) -> Shape
    shapeOf i (n, ex)
        | exchangeIsProductOutput ex = Left i
        | otherwise = Right (n, groupElement ex, isAvoided ex, exchangeFlowId ex, exchangeUnitId ex, exchangeLocation ex)

    together :: M.Map Shape (NonEmpty Double)
    together = groupInOrder [(shape, exchangeAmount ex) | (shape, (_, ex)) <- shaped]

    total :: Shape -> Exchange -> Exchange
    total shape ex = case M.lookup shape together of
        Just amounts@(_ :| _ : _) -> withAmount (sum amounts) ex
        _ -> ex

    isAvoided :: Exchange -> Bool
    isAvoided = \case
        TechnosphereExchange{techRole = role} -> role == AvoidedProduct
        BiosphereExchange{} -> False
        WasteExchange{} -> False

-- | How the products of one dataset are told apart in the file.
data Division1
    = -- | One product, which takes every line.
      SingleProduct
    | -- | Several, each taking this percentage of every line they share, the percentages adding up to 100.
      Keyed !KeyBasis ![Double]
    | -- | Several, written without keys, and why.
      Unkeyed !Text

-- | What the percentages of a block's keys come from.
data KeyBasis
    = -- | The prices its products are stated to sell for: an economic key.
      FromPrices
    | -- | The shares its products declare, or the prices deduced from them, which weigh alike.
      FromShares
    | -- | The shares its products declare, and why the prices stated beside them could not weigh it.
      SharesOverPrices !Text

{- | The keys of a dataset making several products, from what weighs them
('blockWeights'), scaled to add up to 100: a reader takes the fractions a line
receives to add up to that. A block with nothing to weigh by, every product at
0 %, or no line its products share is written without keys.
-}
divisionOf :: UnitConfig -> UnitDB -> Activity -> Division1
divisionOf cfg unitDB act
    | not (any isCoproduct products) = SingleProduct
    | all exchangeIsProductOutput (exchanges act) = Unkeyed "its products share no line"
    | otherwise = case blockWeights cfg unitDB products of
        PricesWeigh shares
            | any statesPrice products -> scaled FromPrices shares
            -- The prices deduced from the shares give them back only to the last
            -- digit, which would then move at every export: the shares weigh.
            | otherwise -> scaled FromShares (fromMaybe shares (traverse (fmap dsPercent . exchangeDeclaredShare) products))
        SharesWeigh why shares -> scaled (SharesOverPrices why) shares
        NothingWeighs why -> Unkeyed why
  where
    products :: [Exchange]
    products = filter exchangeIsProductOutput (exchanges act)

    -- A price deduced from a share is that share again, and no reason to call the key economic.
    statesPrice :: Exchange -> Bool
    statesPrice ex = case epPrice (exchangeProperties ex) of
        Just (StatedPrice _) -> True
        Just (DeducedFromShare _) -> False
        Nothing -> False

    scaled :: KeyBasis -> [Double] -> Division1
    scaled basis shares
        | total <= 0 = Unkeyed "every product declares 0 %"
        -- Shares once scaled add up to 100 only to the last digit, and scaling
        -- them again would move that digit at every export.
        | abs (total - 100) <= 1e-9 = Keyed basis shares
        | otherwise = Keyed basis [100 * share / total | share <- shares]
      where
        total :: Double
        total = sum shares

{- | The keys dividing a block's lines: one per product, at its percentage of
every line its products share, and one giving each product the whole of its
own line. A reader of this format leaves the second out, and EcoSpold 1 has
it valid; a reader such as Brightway's takes a product it finds no key for as
an error, which a product at 0 % would otherwise be.
-}
keysOf :: [Double] -> [(Int, Exchange)] -> [Allocation1]
keysOf shares numbered = concat [[Allocation1 p share sharedLines, Allocation1 p 100 [p]] | (p, share) <- zip productLines shares]
  where
    productLines = [n | (n, ex) <- numbered, exchangeIsProductOutput ex]
    sharedLines = [n | (n, ex) <- numbered, not (exchangeIsProductOutput ex)]

-- | The names of the lines of a dataset, by the number they are written with.
numberedNames :: Resolvers -> Written1 -> M.Map Int (NonEmpty Text)
numberedNames res w = groupInOrder [(n, writtenName res (w1Key w) ex) | (n, ex) <- w1Lines w]

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
    , indent 4 <> "<referenceFunction" <> refFunctionAttrs (referenceFunctionName res key act) act <> "/>"
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
        ++ concatMap (exchangeLines res key) (w1Lines w)
        ++ concatMap allocationLines (w1Keys w)
        ++ [ indent 2 <> "</flowData>"
           , indent 1 <> "</dataset>"
           ]
  where
    dated = datedBy opts (activityDates act)
    w = written1 res num (key, act)

    allocationLines :: Allocation1 -> [Text]
    allocationLines k =
        (indent 3 <> "<allocation referenceToCoProduct=" <> attr (T.pack (show (alCoProduct k))) <> method <> " fraction=" <> attr (formatAmount (alFraction k)) <> ">")
            : [indent 4 <> "<referenceToInputOutput>" <> T.pack (show n) <> "</referenceToInputOutput>" | n <- alLines k]
                <> [indent 3 <> "</allocation>"]

    -- 1 is economic causality, which a price is; the format has no method for a declared share, and leaves it out.
    method :: Text
    method = case w1Division w of
        Keyed FromPrices _ -> " allocationMethod=\"1\""
        Keyed FromShares _ -> ""
        Keyed (SharesOverPrices _) _ -> ""
        SingleProduct -> ""
        Unkeyed _ -> ""

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
exchangeLines :: Resolvers -> (UUID, UUID) -> (Int, Exchange) -> [Text]
exchangeLines res key (number, ex) =
    [ indent 3 <> "<exchange" <> exchangeAttrs res number key ex <> ">"
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
being written as a technosphere input. A coproduct of a block kept whole is a
supplier of its own ('supplierNumberIndex'): its line carries its number, and
so does an input buying it. Everything else – biosphere, waste outputs, and
unlinked inputs – carries the number its flow was given once for the whole
export ('flowNumberIndex').

'checkEcoSpold1Exportable' guarantees any resolved link's (activity, product)
pair is present in the supplier index before export, so that fallback is
taken only by an input bought from a database it depends on, on purpose: no
dataset of the file is its supplier. The flow index covers every non-reference
exchange by construction.
-}
exchangeNumber :: Resolvers -> (UUID, UUID) -> Int -> Exchange -> Int
exchangeNumber res (activity, _) datasetNum ex
    | exchangeIsReference ex = datasetNum
    | isCoproduct ex = supplierNumber (activity, exchangeFlowId ex)
    | Just supplier <- suppliedBy ex = supplierNumber supplier
    | otherwise = flowNum
  where
    supplierNumber :: (UUID, UUID) -> Int
    supplierNumber supplier = M.findWithDefault flowNum supplier (rSupplierNumbers res)
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
it; an input bought from a database it depends on says its supplier's
activity, the name that database gives the dataset; every other exchange says
its flow's.
-}
exchangeAttrs :: Resolvers -> Int -> (UUID, UUID) -> Exchange -> Text
exchangeAttrs res number key ex =
    " number="
        <> attr (T.pack (show number))
        <> " name="
        <> attr (writtenName res key ex)
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
    FlowFields _ category subCategory cas = flowFields res ex

{- | The name a line is written under: its dataset's for a reference exchange,
its supplier's for a bought input ('datasetNames'), its flow's otherwise. An
input buying a coproduct of a block finds no supplier row of its own, and says
its flow's, which is that product's name.
-}
writtenName :: Resolvers -> (UUID, UUID) -> Exchange -> Text
writtenName res key ex = fromMaybe flowName (dataset >>= (`M.lookup` rDatasetNames res))
  where
    FlowFields flowName _ _ _ = flowFields res ex
    dataset = if exchangeIsReference ex then Just key else suppliedBy ex

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
        Coproduct -> wrapOut "2"
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
    deriving (Eq, Ord)

{- | Resolve a flow's serialised fields from the matching table. A biosphere
flow's compartment becomes category/subCategory; a technosphere flow has no
category; a waste flow takes the shape of the row it is written in, blank for
an input (a technosphere row) and 'Waste' for an output (a biosphere
row, which is the compartment the parser reads back). A UUID absent from its
table yields empty/Nothing – never a crash.
-}
flowFields :: Resolvers -> Exchange -> FlowFields
flowFields res = flowFieldsIn (rTech res) (rBio res) (rWaste res)

-- | 'flowFields' against the three flow tables.
flowFieldsIn :: TechFlowDB -> BioFlowDB -> WasteFlowDB -> Exchange -> FlowFields
flowFieldsIn techs bios wastes ex = case ex of
    TechnosphereExchange{techFlowId = fid} ->
        case M.lookup fid techs of
            Just tf -> FlowFields (tfName tf) "" "" (tfCAS tf)
            Nothing -> FlowFields "" "" "" Nothing
    BiosphereExchange{bioFlowId = fid} ->
        case M.lookup fid bios of
            Just bf ->
                FlowFields
                    (bfName bf)
                    (maybe "" (categoryText . compartmentName) (bfCompartment bf))
                    (fromMaybe "" (bfCompartmentSub bf))
                    (bfCAS bf)
            Nothing -> FlowFields "" "" "" Nothing
    WasteExchange{waFlowId = fid, waIsInput = isInput} ->
        let category = if isInput then "" else mediumText Waste
         in case M.lookup fid wastes of
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

{-# LANGUAGE OverloadedStrings #-}

{- | Pure builders for the sparse matrices and UUID interning tables that back
'Database'. The IO entrypoint in "Database" composes these helpers and adds
progress reporting; all numerical work lives here.

Sign and normalization conventions are documented on
'buildDatabaseWithMatrices'.
-}
module Database.MatrixBuild (
    InterningTables (..),
    buildInterningTables,
    buildSupplierRefUnits,
    collectBioFlowOrder,
    buildTechTriples,
    buildBioTriples,
    withActivities,
    LineRow (..),
    LineRows (..),
    lineRows,
    technosphereChange,
    findProducer,
    linkedProducer,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Int (Int32)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Database.Allocation (AllocatedActivity, AllocationRefusal, allocatedActivity, asAllocated, describeRefusal, producerKeyBy)
import Types
import UnitConversion (UnitConfig, convertUnit, missingConversion, unitKey)

{- | Per-process lookup tables built once from the ascending activity-key list.

All fields share a single 'zip' [0..] (M.toAscList activityMap)' traversal, so
row order and ProcessId ↔ (UUID, UUID) consistency hold by construction, and the
activity of each row is the one its key maps to.
-}
data InterningTables = InterningTables
    { itProcessIdTable :: !(V.Vector (UUID, UUID))
    , itProcessIdLookup :: !(M.Map (UUID, UUID) ProcessId)
    , itActivityUUIDIndex :: !(M.Map UUID (NonEmpty ProcessId))
    , itActivities :: !(V.Vector Activity)
    , itActivityCount :: !Int32
    }

buildInterningTables :: M.Map (UUID, UUID) Activity -> InterningTables
buildInterningTables activityMap =
    InterningTables
        { itProcessIdTable = V.fromList [k | (_, k, _) <- indexed]
        , itProcessIdLookup = M.fromList [(k, pid) | (pid, k, _) <- indexed]
        , -- One activity, every row it was written as, which is also the
          -- products of the block it came out of: an allocated activity is
          -- written once per coproduct, and 'M.fromList' would have kept
          -- whichever row came last.
          itActivityUUIDIndex = M.fromListWith (flip (<>)) [(actUUID, pid :| []) | (pid, (actUUID, _), _) <- indexed]
        , itActivities = V.fromList [act | (_, _, act) <- indexed]
        , itActivityCount = fromIntegral (length indexed)
        }
  where
    -- 'M.toAscList' is already sorted on the key, so this is the same row order
    -- as before, with the activity carried alongside instead of looked up again.
    indexed = [(pid, k, act) | (pid, (k, act)) <- zip [0 ..] (M.toAscList activityMap)]

{- | Reference-product output unit for each activity (empty when the activity
has no produced reference exchange – same fallback as the previous inline
expression).
-}
buildSupplierRefUnits :: UnitDB -> V.Vector Activity -> V.Vector Text
buildSupplierRefUnits unitDB = V.map refUnit
  where
    refUnit act =
        case [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)] of
            (ex : _) -> getUnitNameForExchange unitDB ex
            [] -> ""

-- | Ascending vector of every biosphere flow UUID present in the activity set.
collectBioFlowOrder :: V.Vector Activity -> V.Vector UUID
collectBioFlowOrder activities =
    V.fromList . S.toAscList . S.fromList $
        [ exchangeFlowId ex
        | act <- V.toList activities
        , ex <- exchanges act
        , isBiosphereExchange ex
        ]

{- | Divide-by-zero guard for normalization factors. Activities with no
reference output normalize by 1.0 instead of a near-zero denominator. Guards on
magnitude, not sign: a waste-treatment reference is a NEGATIVE production (e.g.
-1 kg of the treated waste), and that sign must be preserved through the
normalization – collapsing it to 1.0 silently flips the activity's inventory.
-}
safeDenom :: Double -> Double
safeDenom f = if abs f > 1e-15 then f else 1.0

-- | Producer of a technosphere exchange: the row its (activityUUID, flowUUID) pair names.
findProducer :: M.Map (UUID, UUID) ProcessId -> Exchange -> Maybe ProcessId
findProducer lkp ex =
    exchangeActivityLinkId ex >>= \actUUID -> M.lookup (actUUID, exchangeFlowId ex) lkp

{- | The row a consumer names as this exchange's target: the row the matrix
routes it through, else the row its activity link alone names. The fallback
keeps the targets of links whose (activity, flow) pair is no row at all: the
matrix drops such an exchange, so the row named is one no score charged, but
that is a lesser evil than dropping targets that answer today. It answers only
for an activity written as a single row, so an allocated activity reached this
way is left without a target rather than given the wrong coproduct.
-}
linkedProducer :: Database -> Exchange -> Maybe ProcessId
linkedProducer db ex =
    findProducer (dbProcessIdLookup db) ex
        <|> (exchangeActivityLinkId ex >>= findProcessIdByActivityUUID db)

{- | Warning text for an exchange whose declared producer cannot be located.
Zero-amount placeholder exchanges produce no warning.
-}
missingActivityWarning :: Activity -> Exchange -> UUID -> [String]
missingActivityWarning consumer ex actUUID
    | abs (exchangeAmount ex) <= 1e-15 = []
    | otherwise =
        [ "Missing activity-product pair referenced by exchange:\n"
            ++ "  Activity UUID: "
            ++ T.unpack (UUID.toText actUUID)
            ++ "\n"
            ++ "  Product UUID: "
            ++ T.unpack (UUID.toText (exchangeFlowId ex))
            ++ "\n"
            ++ "  Consumer: "
            ++ T.unpack (activityName consumer)
            ++ "\n"
            ++ "  Expected file: "
            ++ T.unpack (UUID.toText actUUID)
            ++ "_"
            ++ T.unpack (UUID.toText (exchangeFlowId ex))
            ++ ".spold\n"
            ++ "  This exchange will be skipped."
        ]

unitConversionError :: UnitConfig -> Activity -> Text -> Text -> Text
unitConversionError unitConfig consumer fromU toU =
    "Unknown unit conversion: \""
        <> fromU
        <> "\" \8594 \""
        <> toU
        <> "\" in "
        <> activityName consumer
        <> " \8212 "
        <> missingConversion unitConfig fromU toU

{- | Flatten the activity set into a stream of @(normFactor, j, activity, ex)@
tuples, over the activities the allocation gate accepts. An activity it
refuses gets no triple at all: its column stays empty, so it produces itself
and nothing else, and the warning says why. The same verdict refuses to score
it ('Service.resolveScorable') and names it in the quality report.
-}
perActivity :: InterningTables -> ([String], [(Double, ProcessId, AllocatedActivity, Exchange)])
perActivity tables = (concat warnings, concat rows)
  where
    (warnings, rows) = unzip [column j | j <- [0 .. itActivityCount tables - 1]]

    column :: ProcessId -> ([String], [(Double, ProcessId, AllocatedActivity, Exchange)])
    column j =
        let act = itActivities tables V.! fromIntegral j
         in case columnSource (itProcessIdTable tables V.! fromIntegral j) act of
                Left warning -> ([warning], [])
                Right (normFactor, allocated) -> ([], [(normFactor, j, allocated, ex) | ex <- exchanges act])

{- | What a process's column is divided by, and the process as the allocation
gate lets it in, or the warning that says why it gets no column.
-}
columnSource :: (UUID, UUID) -> Activity -> Either String (Double, AllocatedActivity)
columnSource key act = case asAllocated act of
    Left refusal -> Left (refusalWarning refusal)
    Right allocated -> Right (activityNormFactor act key, allocated)
  where
    refusalWarning :: AllocationRefusal -> String
    refusalWarning refusal =
        "Activity \""
            ++ T.unpack (activityName act)
            ++ "\" ("
            ++ T.unpack (UUID.toText (fst key))
            ++ "_"
            ++ T.unpack (UUID.toText (snd key))
            ++ ") is not allocated: "
            ++ T.unpack (describeRefusal refusal)
            ++ ". It loads, but it will not be scored."

{- | Technosphere sparse triplets + skipped-link warnings.

Short-circuits on the first unit-conversion error. Accumulates triplets
and warnings via the tuple Monoid, mirroring the @foldMap@ stats pattern
used in "Method.Mapping".
-}
buildTechTriples ::
    UnitConfig ->
    UnitDB ->
    InterningTables ->
    V.Vector Text ->
    Either Text (VU.Vector SparseTriple, [String])
buildTechTriples unitConfig unitDB tables supplierRefUnits =
    fmap pack (traverse step rows)
  where
    (refusals, rows) = perActivity tables
    lkp = itProcessIdLookup tables
    actCount = itActivityCount tables
    step (normFactor, j, allocated, ex)
        | boughtFromWholeBlock ex = Right mempty
        | otherwise = techTriple unitConfig unitDB lkp supplierRefUnits actCount normFactor j (allocatedActivity allocated) ex

    -- A coproduct of a block a parent keeps whole: the block has no column to
    -- draw on, as for its reference, and warned once for itself.
    boughtFromWholeBlock :: Exchange -> Bool
    boughtFromWholeBlock ex = case exchangeActivityLinkId ex of
        Just activity
            | Nothing <- findProducer lkp ex ->
                isJust (producerKeyBy ((itActivities tables V.!) . fromIntegral) lkp (activity, exchangeFlowId ex))
        _ -> False
    pack rs = let (ts, ws) = fold rs in (VU.fromList ts, refusals ++ ws)

techTriple ::
    UnitConfig ->
    UnitDB ->
    M.Map (UUID, UUID) ProcessId ->
    V.Vector Text ->
    Int32 ->
    Double ->
    ProcessId ->
    Activity ->
    Exchange ->
    Either Text ([SparseTriple], [String])
techTriple unitConfig unitDB lkp supplierRefUnits actCount normFactor j consumer ex
    -- Biosphere flows live in B; reference products sit on the diagonal of (I-A).
    -- WasteExchanges share A: same product-link calculation as a technosphere flow.
    -- Orphan waste outputs (no activityLinkId) drop out below when findProducer is Nothing.
    | isBiosphereExchange ex = Right mempty
    | exchangeIsReference ex = Right mempty
    | otherwise =
        case validProducerIdx of
            Nothing -> Right ([], producerWarnings)
            Just idx -> emitTriple idx
  where
    producerPid = findProducer lkp ex
    validProducerIdx = do
        pid <- producerPid
        if pid >= 0 && pid < actCount then Just pid else Nothing
    -- Only warn when the activity-link lookup itself failed (Just actUUID + no pid).
    -- A missing processLink with no activityLink is a true orphan, not a data gap.
    producerWarnings = case (producerPid, exchangeActivityLinkId ex) of
        (Nothing, Just actUUID) -> missingActivityWarning consumer ex actUUID
        _ -> []

    emitTriple idx =
        let raw = exchangeAmount ex
            exchUnit = getUnitNameForExchange unitDB ex
            suppUnit = supplierRefUnits V.! fromIntegral idx
            needsConversion =
                unitKey unitConfig exchUnit /= unitKey unitConfig suppUnit
                    && not (T.null exchUnit)
                    && not (T.null suppUnit)
         in case (needsConversion, convertUnit unitConfig exchUnit suppUnit raw) of
                (True, Nothing) -> Left (unitConversionError unitConfig consumer exchUnit suppUnit)
                (True, Just v) -> Right (triplesFor idx v, [])
                (False, _) -> Right (triplesFor idx raw, [])

    triplesFor idx v =
        let sign = if exchangeIsInput ex then 1 else -1
            value = sign * v / safeDenom normFactor
         in [SparseTriple idx j value | v /= 0, idx /= j]

{- | Biosphere sparse triplets. No unit conversion or producer cascade – each
biosphere exchange maps directly to its row via 'collectBioFlowOrder'.
-}
buildBioTriples :: V.Vector UUID -> InterningTables -> VU.Vector SparseTriple
buildBioTriples bioOrder tables =
    VU.fromList $ concatMap step (snd (perActivity tables))
  where
    bioIndex = bioRows bioOrder
    step (normFactor, j, _act, ex) = bioTriple bioIndex normFactor j ex

bioRows :: V.Vector UUID -> M.Map UUID Int32
bioRows bioOrder = M.fromList $ zip (V.toList bioOrder) [0 ..]

bioTriple :: M.Map UUID Int32 -> Double -> ProcessId -> Exchange -> [SparseTriple]
bioTriple bioIndex normFactor j ex
    | not (isBiosphereExchange ex) = []
    | otherwise = case M.lookup (exchangeFlowId ex) bioIndex of
        Nothing -> []
        Just i ->
            let raw = exchangeAmount ex
                value = raw / safeDenom normFactor
             in [SparseTriple i j value | raw /= 0]

-- | A line of a process and the row of A or B it enters.
data LineRow row = LineRow
    { linePosition :: !Int
    -- ^ Among the activity's exchanges.
    , lineExchange :: !Exchange
    , lineRow :: !row
    , lineFactor :: !Double
    -- ^ What one unit of the line's amount puts in that row: sign and unit conversion.
    }

-- | A process's lines entering A, by supplier, and B, by flow row.
data LineRows = LineRows
    { technosphereLines :: ![LineRow ProcessId]
    , biosphereLines :: ![LineRow Int32]
    }

{- | The rows a process's lines enter in its columns of A and of B, each with
what one unit of the line's amount puts there before the column is divided by
the process's normalisation: the supplier's row and the line's sign and unit
conversion, or the flow's row. Read with the load's own builders for an amount
of one, so a line whose amount is zero today, which the load leaves out, still
says where it goes. A line no row of the database takes is left out; a process
the allocation gate refuses has no column, and gives why. Each line comes with
its position among the activity's exchanges.
-}
lineRows :: UnitConfig -> Database -> ProcessId -> Activity -> Either Text LineRows
lineRows unitConfig db = rowsOf
  where
    supplierRefUnits :: V.Vector Text
    supplierRefUnits = buildSupplierRefUnits (dbUnits db) (dbActivities db)

    bioIndex :: M.Map UUID Int32
    bioIndex = bioRows (dbBiosphereOrder db)

    rowsOf :: ProcessId -> Activity -> Either Text LineRows
    rowsOf j act = do
        (_, allocated) <- first T.pack (columnSource (dbProcessIdTable db V.! fromIntegral j) act)
        tech <- traverse (techRows allocated) numbered
        pure (LineRows (concat tech) [LineRow n ex i 1 | (n, ex) <- numbered, SparseTriple i _ _ <- bioTriple bioIndex 1 j (withAmount 1 ex)])
      where
        numbered = zip [0 ..] (exchanges act)
        techRows allocated (n, ex) =
            (\(triples, _) -> [LineRow n ex i v | SparseTriple i _ v <- triples])
                <$> techTriple unitConfig (dbUnits db) (dbProcessIdLookup db) supplierRefUnits (dbActivityCount db) 1 j (allocatedActivity allocated) (withAmount 1 ex)

{- | What the columns of these processes change in M = I − A from one
database to the other: each column with the entries it adds to M, by row,
summed as the factorisation sums two exchanges of one product. A column none
of whose entries moved is left out.
-}
technosphereChange :: S.Set ProcessId -> Database -> Database -> [(Int, [(Int, Double)])]
technosphereChange moved before after =
    [ (fromIntegral j, entries)
    | (j, byRow) <- M.toList (M.unionWith (M.unionWith (+)) (columns 1 before) (columns (-1) after))
    , let entries = [(fromIntegral i, v) | (i, v) <- M.toList byRow, v /= 0]
    , not (null entries)
    ]
  where
    -- M′ − M = A − A′: an entry of A before adds to M, one after takes away.
    columns :: Double -> Database -> M.Map Int32 (M.Map Int32 Double)
    columns sign db =
        M.fromListWith
            (M.unionWith (+))
            [(j, M.singleton i (sign * v)) | SparseTriple i j v <- VU.toList (dbTechnosphereTriples db), S.member j moved]

{- | The database with these processes in place of its own, their columns of A
and B built again as a load builds them, and their links to other databases
asking for their new amounts. The whole column, since a reference amount that
moves changes what every entry of it is divided by. The biosphere rows stay, as
they already list the flows whose amount is zero.
-}
withActivities :: UnitConfig -> Database -> M.Map ProcessId Activity -> Either Text Database
withActivities unitConfig db changed
    | M.null changed = Right db
    | otherwise = do
        columns <- traverse column (M.toList changed)
        let (tech, bio) = fold columns
        pure
            db
                { dbActivities = activities
                , dbTechnosphereTriples = replaced (dbTechnosphereTriples db) tech
                , dbBiosphereTriples = replaced (dbBiosphereTriples db) bio
                , dbCrossDBLinks = map followed (dbCrossDBLinks db)
                }
  where
    -- A link to another database asks for the amount of the exchange it supplies.
    followed :: CrossDBLink -> CrossDBLink
    followed link = fromMaybe link $ do
        AnsweredExchange{aePosition = position, aeFactor = factor} <- cdlAnswers link
        pid <- findProcessId db (cdlConsumerActUUID link) (cdlConsumerProdUUID link)
        act <- M.lookup pid changed
        ex <- listToMaybe (drop position (exchanges act))
        pure link{cdlCoefficient = exchangeAmount ex * factor}

    -- Kept as it was when no entry moved: the factorisation and the
    -- regionalized weights of the loaded database then still serve.
    replaced :: VU.Vector SparseTriple -> [SparseTriple] -> VU.Vector SparseTriple
    replaced triples new
        | entries (VU.filter (not . elsewhere) triples) == entries (VU.fromList new) = triples
        | otherwise = VU.filter elsewhere triples <> VU.fromList new

    entries :: VU.Vector SparseTriple -> [(Int32, Int32, Double)]
    entries = sort . map (\(SparseTriple i j v) -> (j, i, v)) . VU.toList

    activities :: V.Vector Activity
    activities = dbActivities db V.// [(fromIntegral j, act) | (j, act) <- M.toList changed]

    supplierRefUnits :: V.Vector Text
    supplierRefUnits = buildSupplierRefUnits (dbUnits db) activities

    bioIndex :: M.Map UUID Int32
    bioIndex = bioRows (dbBiosphereOrder db)

    elsewhere :: SparseTriple -> Bool
    elsewhere (SparseTriple _ j _) = M.notMember j changed

    column :: (ProcessId, Activity) -> Either Text ([SparseTriple], [SparseTriple])
    column (j, act) = do
        (normFactor, allocated) <- first T.pack (columnSource (dbProcessIdTable db V.! fromIntegral j) act)
        -- The warnings name links the load already could not resolve: amounts do not change them.
        (tech, _) <- fold <$> traverse (techTriple unitConfig (dbUnits db) (dbProcessIdLookup db) supplierRefUnits (dbActivityCount db) normFactor j (allocatedActivity allocated)) (exchanges act)
        pure (tech, concatMap (bioTriple bioIndex normFactor j) (exchanges act))

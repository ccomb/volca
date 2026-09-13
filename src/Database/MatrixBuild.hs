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
    findProducer,
    linkedProducer,
) where

import Control.Applicative ((<|>))
import Data.Foldable (fold)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Database.Allocation (AllocatedActivity, AllocationRefusal, allocatedActivity, asAllocated, describeRefusal)
import Types
import UnitConversion (UnitConfig, convertUnit, unitKey)

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

unitConversionError :: Activity -> Text -> Text -> Text
unitConversionError consumer fromU toU =
    "Unknown unit conversion: \""
        <> fromU
        <> "\" \8594 \""
        <> toU
        <> "\" in "
        <> activityName consumer
        <> " \8212 add these units to [[units]] CSV"

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
            key = itProcessIdTable tables V.! fromIntegral j
         in case asAllocated act of
                Left refusal -> ([refusalWarning act key refusal], [])
                Right allocated -> ([], [(activityNormFactor act key, j, allocated, ex) | ex <- exchanges act])

    refusalWarning :: Activity -> (UUID, UUID) -> AllocationRefusal -> String
    refusalWarning act (actUUID, prodUUID) refusal =
        "Activity \""
            ++ T.unpack (activityName act)
            ++ "\" ("
            ++ T.unpack (UUID.toText actUUID)
            ++ "_"
            ++ T.unpack (UUID.toText prodUUID)
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
    step (normFactor, j, allocated, ex) =
        techTriple unitConfig unitDB lkp supplierRefUnits actCount normFactor j (allocatedActivity allocated) ex
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
                (True, Nothing) -> Left (unitConversionError consumer exchUnit suppUnit)
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
    bioIndex = M.fromList $ zip (V.toList bioOrder) [0 ..]
    step (normFactor, j, _act, ex)
        | not (isBiosphereExchange ex) = []
        | otherwise = case M.lookup (exchangeFlowId ex) bioIndex of
            Nothing -> []
            Just i ->
                let raw = exchangeAmount ex
                    value = raw / safeDenom normFactor
                 in [SparseTriple i j value | raw /= 0]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Matrix
Description : The matrix side of an assessment: demands, scalings, inventories

Solves the equation of a supply chain, @(I - A) s = f@, where @A@ is the
technosphere (activities by activities), @f@ the final demand and @s@ the
scaling of each activity, then reads the inventory off the biosphere,
@g = B s@ (flows by activities).

The system itself is factorised and solved by "Matrix.Sparse".
-}
module Matrix (
    Vector,
    Demand (..),
    Inventory,
    SupplierDemands,
    DepDemands,
    computeScalingVector,
    applyBiosphereMatrix,
    computeInventoryMatrix,
    accumulateDepDemands,
    accumulateDepDemandsWith,
    activityNormalizationFactor,
    processScaling,
    depDemandsToVector,
    inSupplierUnit,
    linkConsumer,
    computeProcessLCIAContributions,
    Solve,
    perturbA,
    perturbABatch,
    perturbGlobal,
    applyShermanMorrison,
    applyShermanMorrisonV,
    buildDemandVector,
    referenceSign,
    solveOnce,
    unsolvableText,
    applySparseMatrix,
    fromList,
    toList,
    chunksOf,
    countingSolve,
    readSolveCounter,
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Matrix.Sparse (Unsolvable (..))
import qualified Matrix.Sparse as Sparse
import System.IO.Unsafe (unsafePerformIO) -- Used only for NOINLINE global singletons
import Types
import qualified UnitConversion

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

{- | The right-hand side @d@ of @(I - A) x = d@: what a functional unit asks
of the system, one entry per activity column. The solution @x@ it produces
is a plain 'Vector', which is what keeps the two from being confused: a
demand can be solved, and nothing else accepts one.
-}
newtype Demand = Demand {unDemand :: Vector}

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

{- | Demand a consumer DB places on one supplier, stated in the consumer's
exchange unit (for cross-DB unit conversion).
-}
type SupplierDemands = M.Map ProcessRef StatedAmount

-- | Per-dependency-DB supplier demands, keyed by source database name.
type DepDemands = M.Map Text SupplierDemands

{- | Count of matrix solves performed since the process started.

A solve is the expensive thing this server does, and the only work that can
outlast a request: an analysis started just before an idle deadline must not
have the process exit under it. Callers therefore only ever ask whether the
number moved between two moments, never what it means.
-}
{-# NOINLINE solveCounter #-}
solveCounter :: IORef Int
solveCounter = unsafePerformIO $ newIORef 0

-- | Record that one matrix solve happened.
bumpSolveCounter :: IO ()
bumpSolveCounter = atomicModifyIORef' solveCounter (\n -> (n + 1, ()))

-- | Read the running solve count. Meaningful only compared against itself.
readSolveCounter :: IO Int
readSolveCounter = readIORef solveCounter

{- | Bracket a solve. The count moves when the work starts and again when it
ends, so a solve begun moments before an idle deadline is not cut off in
flight, and a long chain of solves keeps the deadline moving throughout.
The count is silent between the two moves, so a single solve that alone
outlasts the whole timeout is still cut off mid-flight – a ceiling that
stands far above real work, since solves take seconds and timeouts minutes.
-}
countingSolve :: IO a -> IO a
countingSolve act = bumpSolveCounter *> act <* bumpSolveCounter

-- Vector operations
fromList :: [Double] -> Vector
fromList = U.fromList

toList :: Vector -> [Double]
toList = U.toList

{- | Split a list into successive chunks of at most @n@ elements (a non-positive
@n@ yields the whole list as a single chunk).
-}
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
    | n <= 0 = [xs]
    | null xs = []
    | otherwise = let (h, t) = splitAt n xs in h : chunksOf n t

-- | Efficient sparse matrix multiplication: result = A * vector
applySparseMatrix :: [(Int, Int, Double)] -> Int -> Vector -> Vector
applySparseMatrix sparseTriples nRows inputVec =
    let resultVec = U.create $ do
            result <- MU.new nRows
            forM_ [0 .. nRows - 1] $ \i -> MU.write result i 0.0
            forM_ sparseTriples $ \(i, j, val) ->
                when (j < U.length inputVec) $ do
                    oldVal <- MU.read result i
                    let newVal = oldVal + val * (inputVec U.! j)
                    MU.write result i newVal
            return result
     in resultVec

{- |
Compute the scaling vector by solving (I - A)x = d.

Returns the supply vector where x[i] is the scaling factor for activity i
(how much of activity i is needed to produce one unit of the root activity).
-}
computeScalingVector :: Database -> ProcessId -> IO (Either Text Vector)
computeScalingVector db rootProcessId =
    either (pure . Left) (countingSolve . evaluate . force . first (unsolvableText db) . solveOnce db) (buildDemandVector db rootProcessId)

{- | Solve a database's system for one demand, factorising it for that demand
alone. A server keeps its factors from one demand to the next: see
"SharedSolver".
-}
solveOnce :: Database -> Demand -> Either Unsolvable Vector
solveOnce db (Demand demand) = Sparse.solve (Sparse.factorize (fromIntegral (dbActivityCount db)) (dbTechnosphereTriples db)) demand

{- | Why a demand got no answer, said to whoever asked, the process in the way
named as the database names it.
-}
unsolvableText :: Database -> Unsolvable -> Text
unsolvableText db = \case
    Singular column -> "the supply chain reaches " <> named column <> ", and nothing is left of what that process makes once its own supply chain has taken its share: there is no amount of it that meets a demand"
    NotFinite column -> "the supply chain reaches " <> named column <> ", one of whose inputs has an amount that is not a finite number"
    OutOfRange place -> "technosphere entry " <> T.pack (show place) <> " names a process the database does not hold"
    WrongLength entries -> "the demand has " <> T.pack (show entries) <> " entries for " <> T.pack (show (dbActivityCount db)) <> " processes"
  where
    named :: Int -> Text
    named column = maybe ("the process at column " <> T.pack (show column)) (described . fromIntegral) (V.findIndex (== fromIntegral column) (dbActivityIndex db))
    described :: ProcessId -> Text
    described pid = maybe "" (\activity -> "\"" <> activityName activity <> "\" (" <> activityLocation activity <> "), ") (getActivity db pid) <> "process " <> processIdToText db pid

{- |
Apply the biosphere matrix to a scaling vector: g = B * x.

Converts the scaling vector (activity scaling factors) into a biosphere inventory
(environmental flows) by multiplying with the biosphere intervention matrix.

Reads the columns of the processes the scaling vector asks something of, and
no other: a supply chain reaches a small part of a database, and reading every
entry of B for each process scored took nearly all the time of scoring a
database whose biosphere is large.
-}
applyBiosphereMatrix :: Database -> Vector -> Inventory
applyBiosphereMatrix db supplyVec =
    let bioFlowCount = fromIntegral (dbBiosphereCount db) :: Int
        bioFlows = dbBiosphereOrder db
        inventoryVec = U.create $ do
            mv <- MU.replicate bioFlowCount 0.0
            U.iforM_ supplyVec $ \j x -> when (x /= 0) $
                U.forM_ (entriesOfColumn (dbBiosphere db) j) $ \(SparseTriple i _ v) -> do
                    let !ci = fromIntegral i :: Int
                    old <- MU.unsafeRead mv ci
                    MU.unsafeWrite mv ci (old + v * x)
            pure mv
        invLen = U.length inventoryVec
     in M.fromList
            [ (bioFlows V.! i, U.unsafeIndex inventoryVec i)
            | i <- [0 .. V.length bioFlows - 1]
            , i < invLen
            ]

computeInventoryMatrix :: Database -> ProcessId -> IO (Either Text Inventory)
computeInventoryMatrix db rootProcessId =
    fmap (applyBiosphereMatrix db) <$> computeScalingVector db rootProcessId

{- |
Per-process LCIA contributions for one impact category.
contribution[p] = Σ_f  B[f,p] * s[p] * CF[f]   (in the method's impact unit)

Exact for cyclic systems because it uses the full matrix-solved scaling vector.
-}
computeProcessLCIAContributions ::
    Database ->
    -- | Scaling vector (from computeScalingVector)
    Vector ->
    -- | CF map: flowUUID → CF value (single impact category)
    M.Map UUID Double ->
    -- | ProcessId → LCIA contribution in impact unit
    M.Map ProcessId Double
computeProcessLCIAContributions db scalingVec cfMap =
    let actIdx = dbActivityIndex db
        bioTriples = dbBiosphereTriples db
        bioFlows = dbBiosphereOrder db
        -- Build inverse map: matrix column index → ProcessId
        colToProcess :: M.Map Int ProcessId
        colToProcess =
            M.fromList
                [ (fromIntegral (actIdx V.! pid), fromIntegral pid)
                | pid <- [0 .. V.length actIdx - 1]
                ]
        step acc (SparseTriple flowRow colIdx bioVal) =
            let flowUUID = bioFlows V.! fromIntegral flowRow
             in case M.lookup flowUUID cfMap of
                    Nothing -> acc
                    Just cfVal ->
                        case M.lookup (fromIntegral colIdx :: Int) colToProcess of
                            Nothing -> acc
                            Just pid ->
                                let s = scalingVec U.! fromIntegral colIdx
                                 in M.insertWith (+) pid (bioVal * s * cfVal) acc
     in U.foldl' step M.empty bioTriples

{- |
Rank-1 perturbation of (I-A) via Sherman-Morrison (~4ms per call).

Given a baseline scaling vector @x@ such that @(I-A) x = d@, this returns
@x'@ for the perturbed system @(I-A + u·e_col^T) x' = d@, where @u@ is a
sparse vector on the supplier (row) axis and @e_col@ selects a single
consumer column. The kernel reuses the cached factorization:

    x' = x - z * (v^T * x) / (1 + v^T * z)

with @z = inv(I-A) * u@ (one back-substitution) and @v = e_col@.

== Sign convention

A positive entry in @perturb@ adds to @(I-A)@, which __decreases__ @A@ by
the same amount. So @perturb = [(i, Δ)]@ at @col = j@ encodes
@A_ij -= Δ@. To __increase__ @A_ij@ by @Δ@, pass @[(i, -Δ)]@.

== Encoding common changes

* __Single matrix entry__ @A_ij -= Δ@ – @col = j@, @perturb = [(i, Δ)]@.
* __Symmetric supplier swap__ at consumer @j@ (drop @old@ at coefficient
  @a@, add @new@ at the same amount) – @col = j@,
  @perturb = [(old, +a), (new, -a)]@. The @+a@ on @old@ subtracts @a@
  from @A_(old,j)@ (was @a@, becomes @0@); the @-a@ on @new@ adds @a@
  to @A_(new,j)@ (was @0@, becomes @a@).
* __Asymmetric cross-DB substitution__ where one side lives in a dep DB –
  pass only the root-side entry; the dep-side change is carried by a
  virtual 'CrossDBLink'.
* __No-op__ – @perturb = []@ returns @x@ unchanged (bypasses the
  singularity check).

Returns @Left@ if the update is singular (@|1 + v^T·z| < epsilon@), which
means the perturbation drives @(I-A)@ into a degenerate state.
-}
perturbA ::
    -- | The solver of the system @x@ solves
    Solve ->
    -- | Baseline scaling vector x
    Vector ->
    -- | Consumer column index (j in @A_ij@)
    Int ->
    -- | Sparse perturbation u on the supplier axis: list of (row i, Δ)
    [(Int, Double)] ->
    -- | Perturbed scaling vector x'
    IO (Either T.Text Vector)
perturbA solve x col perturb
    | null perturb = pure (Right x)
    | otherwise = (>>= applyShermanMorrison x col) <$> solveRankOneZ solve (U.length x) perturb

{- | Solve the system for several right-hand sides at once, answering in their
order, or say why one of them has no answer: the one way the rank-1 paths
reach a solver, so a solver that corrects its answers is never bypassed.
-}
type Solve = [Vector] -> IO (Either Text [Vector])

{- | Solve @z = inv(I-A) * u@ for a sparse supplier-axis perturbation @u@.
Shared by the per-edge ('perturbA') and global ('perturbGlobal') rank-1 paths
so the back-sub plumbing lives in one place.
-}
solveRankOneZ :: Solve -> Int -> [(Int, Double)] -> IO (Either T.Text Vector)
solveRankOneZ solve n perturb =
    solve [U.accum (+) (U.replicate n 0.0) perturb] <&> \case
        Left why -> Left why
        Right [z] -> Right z
        Right zs -> Left ("the solver answered " <> T.pack (show (length zs)) <> " vectors for one")

{- | Apply the Sherman-Morrison rank-1 formula given the precomputed
z = inv(I-A) * u, with the left projection fixed at the basis column
@e_col@. Thin wrapper over 'applyShermanMorrisonV'.
-}
applyShermanMorrison :: Vector -> Int -> Vector -> Either T.Text Vector
applyShermanMorrison x col = applyShermanMorrisonV x [(col, 1.0)]

{- | Generalized Sherman-Morrison: the left projection @v@ is an arbitrary
sparse vector rather than a single basis column. The rank-1 update is
@(I-A) + u·vᵀ@; with @v = e_col@ this is the per-edge update, while a full
technosphere row @v = row_A(A)@ applies a __global__ supplier swap (every
consumer of one activity at once) in a single back-substitution. @z =
inv(I-A)·u@ is supplied by the caller.
-}
applyShermanMorrisonV :: Vector -> [(Int, Double)] -> Vector -> Either T.Text Vector
applyShermanMorrisonV x v z =
    let dot w = sum [c * (w U.! i) | (i, c) <- v]
        denom = 1.0 + dot z
     in if abs denom < 1e-12
            then Left "Sherman-Morrison update is singular \x2014 perturbation drives (I-A) into a degenerate state"
            else
                let scale = dot x / denom
                 in Right $ U.imap (\i xi -> xi - scale * (z U.! i)) x

{- | Rank-1 update for a __global__ supplier substitution: replace activity
@A@ by @B@ in every consumer that sources from @A@, in one solve. @u@ is the
supplier-axis perturbation (@e_A - κ·e_B@ within a DB, or @e_A@ alone when
@B@ lives in a dependency DB); @v@ is the consumer-axis projection – @A@'s
technosphere row (its coefficient at each consumer). The single back-sub
@z = inv(I-A)·u@ replaces the @N@ solves an edge-by-edge expansion needs.

An empty @u@ or @v@ leaves @x@ unchanged; callers surface "A consumed
nowhere" as an explicit error before reaching here, so an empty @v@ is
never a silent no-op in practice.
-}
perturbGlobal ::
    Solve ->
    Vector ->
    -- | u : sparse supplier-axis perturbation
    [(Int, Double)] ->
    -- | v : sparse consumer-axis projection (A's technosphere row)
    [(Int, Double)] ->
    IO (Either T.Text Vector)
perturbGlobal solve x u v
    | null u || null v = pure (Right x)
    | otherwise = (>>= applyShermanMorrisonV x v) <$> solveRankOneZ solve (U.length x) u

{- |
Batched 'perturbA': computes z_k = inv(I-A) * u_k for every non-empty
perturbation in one call to the solver, then applies the Sherman-Morrison
formula to each.

Empty perturbations short-circuit to @Right x@ without being solved for, and
when the solver refuses the batch every perturbation solved for carries the
refusal. Order of results matches the input.
-}
perturbABatch ::
    -- | The solver of the system @x@ solves
    Solve ->
    -- | Baseline scaling vector x
    Vector ->
    -- | One @(col, perturb)@ per spec, in caller order
    [(Int, [(Int, Double)])] ->
    -- | One result per spec, in the same order
    IO [Either T.Text Vector]
perturbABatch solve x specs = do
    let n = U.length x
        indexed = zip [0 :: Int ..] specs
        nonEmpty =
            [ (idx, col, U.accum (+) (U.replicate n 0.0) p)
            | (idx, (col, p)) <- indexed
            , not (null p)
            ]
        us = [u | (_, _, u) <- nonEmpty]
    solved <- if null us then pure (Right []) else solve us
    let resultsByIdx :: M.Map Int (Either T.Text Vector)
        resultsByIdx =
            M.fromList $ case solved of
                Left why -> [(idx, Left why) | (idx, _, _) <- nonEmpty]
                Right zs -> [(idx, applyShermanMorrison x col z) | ((idx, col, _), z) <- zip nonEmpty zs]
    pure $ map (\(idx, _) -> M.findWithDefault (Right x) idx resultsByIdx) indexed

{- |
Accumulate cross-database supplier demands for a given root scaling vector.

For every 'CrossDBLink' whose consumer has non-zero scaling, add
@coefficient / normFactor(consumer) * scale@ to the demand on the supplier's
'ProcessRef'. Results are grouped by the supplier's database name
so the caller can solve each dependency database in one multi-RHS batch.

The division by the consumer's normalization factor matches the convention
of 'dbTechnosphereTriples' (entries are already divided by the producer's
refAmount). 'cdlCoefficient' is the amount one ref-unit of the consumer
takes, in 'cdlExchangeUnit' (the product's unit the loader converts the line into, or a relink mapping's
target unit where its conversion bridged two measures), while the scaling
vector is in per-kg units – so we re-scale before accumulating.
-}
accumulateDepDemands ::
    Database ->
    Vector ->
    DepDemands
accumulateDepDemands db = accumulateDepDemandsWith db []

{- | Same as 'accumulateDepDemands' but folds over an additional list of
synthesized 'CrossDBLink' records (e.g. virtual links emitted by a
what-if substitution that re-routes a consumer to a dep-DB supplier).
Negative 'cdlCoefficient' is allowed: it cancels a static link at the
same supplier key because the inner 'M.unionWith mergeEntry' sums
amounts. A 0-net entry remains in the map and becomes a zero in
'depDemandsToVector' – silently dropped is never correct (it would
hide a bug); a zero is.
-}
accumulateDepDemandsWith ::
    Database ->
    [CrossDBLink] ->
    Vector ->
    DepDemands
accumulateDepDemandsWith db extraLinks scalingVec =
    foldr step M.empty (dbCrossDBLinks db ++ extraLinks)
  where
    actIdx = dbActivityIndex db
    step link acc =
        case linkConsumer db link of
            Nothing -> acc
            Just (consumerPid, normFactor) ->
                let consumerIdx = fromIntegral $ actIdx V.! fromIntegral consumerPid
                    consumerScale = scalingVec U.! consumerIdx
                    demand = cdlCoefficient link * consumerScale / normFactor
                    supplierKey = linkSupplier link
                    entry = StatedAmount{saUnit = cdlExchangeUnit link, saAmount = demand}
                 in if demand == 0
                        then acc
                        else
                            M.insertWith
                                (M.unionWith mergeEntry)
                                (cdlSourceDatabase link)
                                (M.singleton supplierKey entry)
                                acc
    -- Two links hitting the same supplier should share the same exchange unit
    -- (it's the supplier's product, and the loader converts each line into
    -- that product's unit). Keep the first; conversion will catch any
    -- inconsistency because depDemandsToVector converts exchangeUnit →
    -- supplierRefUnit per entry.
    mergeEntry :: StatedAmount -> StatedAmount -> StatedAmount
    mergeEntry new old = new{saAmount = saAmount new + saAmount old}

{- | A link's consumer in its own database, and the factor its matrix column
is normalized by: what one unit of the consumer takes from the supplier is the
link's coefficient over that factor. Shared by the demand a dependency solves
and the edge a supply chain draws, so the two cannot drift apart. Read from
the consumer itself, so a substitution's virtual link, whose consumer may have
no link of its own in the database, is normalized like any other.
-}
linkConsumer :: Database -> CrossDBLink -> Maybe (ProcessId, Double)
linkConsumer db link =
    (\pid -> (pid, activityNormalizationFactor db pid))
        <$> M.lookup (cdlConsumerActUUID link, cdlConsumerProdUUID link) (dbProcessIdLookup db)

-- | A process's scaling in a database's scaling vector, which is indexed by matrix column.
processScaling :: Database -> U.Vector Double -> ProcessId -> Double
processScaling db scaling pid = scaling U.! fromIntegral (dbActivityIndex db V.! fromIntegral pid)

{- | Activity's reference-product amount used to normalize its matrix column.
Thin wrapper around 'activityNormFactor' that resolves the activity and
its (actUUID, prodUUID) key from the database by 'ProcessId'.
-}
activityNormalizationFactor :: Database -> ProcessId -> Double
activityNormalizationFactor db pid =
    activityNormFactor
        (dbActivities db V.! fromIntegral pid)
        (dbProcessIdTable db V.! fromIntegral pid)

{- |
Convert a sparse supplier-demand map into a length-@n_dep@ demand vector for
the dependency database.

Each entry's consumer-side exchange unit is converted to the supplier's
reference-product unit via 'convertUnit', mirroring the internal technosphere
path in 'Database.buildDatabaseWithMatrices'. Fails with 'Left' on an unknown
unit pair – we never silently use raw values when units are incompatible.

Suppliers whose 'ProcessRef' does not resolve in the dep DB are
silently dropped (they've already been accepted as cross-DB links; a missing
ProcessId here means the dep DB was loaded but doesn't have that exact
supplier, which is recorded at link time).
-}
depDemandsToVector ::
    UnitConversion.UnitConfig ->
    -- | dep DB name, for error messages
    Text ->
    Database ->
    SupplierDemands ->
    Either Text Demand
depDemandsToVector unitConfig depDbName depDb demands = do
    converted <- traverse convertEntry (M.toList demands)
    let n = fromIntegral (dbActivityCount depDb) :: Int
        entries = catMaybes converted
    Right $ Demand $ U.accum (+) (U.replicate n 0.0) entries
  where
    actIdx = dbActivityIndex depDb
    convertEntry (supplierKey, amount) =
        fmap
            (fmap (\(pid, v) -> (fromIntegral (actIdx V.! fromIntegral pid), v)))
            (inSupplierUnit unitConfig depDbName depDb supplierKey amount)

{- | An amount of a dependency's supplier, stated in the unit of the exchange
that asks for it, in the unit of the supplier's reference product. 'Nothing'
when the dependency has no such supplier. Fails on a unit pair it cannot
convert: the raw value is never used in its place.
-}
inSupplierUnit ::
    UnitConversion.UnitConfig ->
    -- | dep DB name, for error messages
    Text ->
    Database ->
    ProcessRef ->
    StatedAmount ->
    Either Text (Maybe (ProcessId, Double))
inSupplierUnit unitConfig depDbName depDb supplierKey StatedAmount{saUnit = exchangeUnit, saAmount = amt} =
    case findProcessId depDb (prActivity supplierKey) (prProduct supplierKey) of
        Nothing -> Right Nothing -- supplier absent in dep DB; drop
        Just pid ->
            let act = dbActivities depDb V.! fromIntegral pid
                refExs = [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)]
                supplierUnit = case refExs of
                    (ex : _) -> getUnitNameForExchange (dbUnits depDb) ex
                    [] -> ""
                needsConversion =
                    UnitConversion.unitKey unitConfig exchangeUnit /= UnitConversion.unitKey unitConfig supplierUnit
                        && not (T.null exchangeUnit)
                        && not (T.null supplierUnit)
             in if not needsConversion
                    then Right (Just (pid, amt))
                    else case UnitConversion.convertUnit unitConfig exchangeUnit supplierUnit amt of
                        Just v -> Right (Just (pid, v))
                        Nothing ->
                            Left $
                                "Unknown unit conversion: \""
                                    <> exchangeUnit
                                    <> "\" \8594 \""
                                    <> supplierUnit
                                    <> "\" for supplier "
                                    <> activityName act
                                    <> " in database "
                                    <> depDbName
                                    <> " \8212 "
                                    <> UnitConversion.missingConversion unitConfig exchangeUnit supplierUnit

{- | The direction one unit of an activity runs in, read from its reference
product.

An activity that declares its reference as a negative output states that it
consumes what it is named after: the treatment of a waste, which the matrix
column is normalized by. One unit of that column is a unit produced, the
reverse of what the activity does, so the functional unit belongs on the other
side: demanding -1 asks for the kilogram to be treated, and the whole solution
(its inputs, its emissions, every process upstream) comes out the way the
activity was written. A reference declared as an input normalizes on its
absolute value and already runs the right way, so it takes +1 like any other.
-}
referenceSign :: Database -> ProcessId -> Double
referenceSign db pid = if activityNormalizationFactor db pid < 0 then -1.0 else 1.0

{- |
Build the final demand vector f for LCA calculations.

The demand vector represents external demand for products from each activity:
- f[i] = one unit of the root activity, signed by 'referenceSign'
- f[i] = 0.0 for all other activities

A process id the index does not hold names no column, so there is no vector
to build and the caller is told. A vector of zeros would solve to an
inventory of zeros and be reported as an answer.
-}
buildDemandVector :: Database -> ProcessId -> Either Text Demand
buildDemandVector db rootProcessId =
    maybe (Left noColumn) (Right . loadColumn) (activityIndex V.!? fromIntegral rootProcessId)
  where
    activityIndex :: V.Vector Int32
    activityIndex = dbActivityIndex db
    n :: Int
    n = V.length activityIndex
    -- Read only from 'loadColumn', which the index lookup above has already
    -- gated: 'referenceSign' reads the activity vector at that same position.
    unit :: Double
    unit = referenceSign db rootProcessId
    loadColumn :: Int32 -> Demand
    loadColumn col = Demand $ fromList [if i == fromIntegral col then unit else 0.0 | i <- [0 .. n - 1 :: Int]]
    noColumn :: Text
    noColumn =
        "ProcessId "
            <> T.pack (show rootProcessId)
            <> " is not in the activity index ("
            <> T.pack (show n)
            <> " processes), so it names no column to put the functional unit in"

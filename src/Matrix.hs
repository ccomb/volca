{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Matrix
Description : Matrix-based LCA calculations using MUMPS sparse direct solver

This module implements matrix-based Life Cycle Assessment (LCA) calculations using
the MUMPS sequential direct solver for high-performance sparse linear algebra.
It solves the fundamental LCA equation: (I - A)^-1 * f = s, where:

- I is the identity matrix
- A is the technosphere matrix (activities x activities)
- f is the final demand vector
- s is the supply vector (scaling factors)

The biosphere inventory is then calculated as: B * s = g, where:
- B is the biosphere matrix (flows x activities)
- g is the final inventory vector

Key features:
- Uses MUMPS direct solver (LU factorization) for numerical stability
- Handles sparse matrices efficiently with coordinate triplet format
- Supports large-scale problems (tested with 15K+ activities)

Performance characteristics:
- Matrix assembly: O(nnz) where nnz is number of non-zero entries
- MUMPS factorization: O(n^1.5) for sparse LCA matrices
- Forward/backward solve: O(n log n)
- Memory usage: ~50-100 MB for typical Ecoinvent database
-}
module Matrix (
    Vector,
    Inventory,
    computeScalingVector,
    applyBiosphereMatrix,
    computeInventoryMatrix,
    accumulateDepDemands,
    accumulateDepDemandsWith,
    activityNormalizationFactor,
    depDemandsToVector,
    computeProcessLCIAContributions,
    shermanMorrisonVariant,
    buildDemandVectorFromIndex,
    solveSparseLinearSystem,
    applySparseMatrix,
    fromList,
    toList,
    initializeSolverForServer,
    precomputeMatrixFactorization,
    solveSparseLinearSystemWithFactorization,
    solveSparseLinearSystemWithFactorizationMulti,
    computeInventoryMatrixBatch,
    clearCachedSolver,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM_, when)
import Data.Int (Int32)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numerical.MUMPS (MUMPSSolver, mumpsAnalyzeAndFactorize, mumpsCreate, mumpsDestroy, mumpsSolve, mumpsSolveMulti)
import Progress
import System.IO.Unsafe (unsafePerformIO) -- Used only for NOINLINE global singletons
import Types
import qualified UnitConversion

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

{- | Global cache for pre-factorized MUMPS solvers per database.
Each entry has its own MVar lock for concurrent forward/backward solves.
-}
{-# NOINLINE cachedSolver #-}
cachedSolver :: MVar (M.Map Text (MUMPSSolver, Int, MVar ()))
cachedSolver = unsafePerformIO $ newMVar M.empty

{- | Global mutex for MUMPS factorization/creation/destruction operations.
MUMPS_SEQ has global Fortran state that is NOT thread-safe for concurrent
create/factorize/destroy calls. Only mumpsSolve on an already-factorized
instance is safe (per-database locks protect same-instance concurrent access).
-}
{-# NOINLINE mumpsFactorizationMutex #-}
mumpsFactorizationMutex :: MVar ()
mumpsFactorizationMutex = unsafePerformIO $ newMVar ()

-- | Initialize solver for server lifetime. No-op for MUMPS (no global state needed).
initializeSolverForServer :: IO ()
initializeSolverForServer = return ()

-- | Clear cached solver for a database (call when unloading)
clearCachedSolver :: Text -> IO ()
clearCachedSolver dbName =
    modifyMVar_ cachedSolver $ \cache ->
        case M.lookup dbName cache of
            Just (solver, _, _) -> do
                mumpsDestroy solver
                return $ M.delete dbName cache
            Nothing -> return cache

-- | Aggregate duplicate matrix entries by summing values for same (i,j) coordinates
aggregateMatrixEntries :: [(Int, Int, Double)] -> [(Int, Int, Double)]
aggregateMatrixEntries entries =
    let groupedEntries = M.fromListWith (+) [((i, j), val) | (i, j, val) <- entries]
     in [(i, j, val) | ((i, j), val) <- M.toList groupedEntries]

-- Vector operations
fromList :: [Double] -> Vector
fromList = U.fromList

toList :: Vector -> [Double]
toList = U.toList

{- |
Fast solver using the globally cached pre-factorized MUMPS solver.

This function uses the cached solver with pre-computed LU factorization,
eliminating both matrix assembly and factorization time for concurrent requests.
Achieves sub-second inventory calculations after server startup.

The solver is looked up by database ID from the per-database cache, enabling
instant switching between databases without re-factorization.
-}
solveSparseLinearSystemWithFactorization :: MatrixFactorization -> Vector -> IO Vector
solveSparseLinearSystemWithFactorization factorization demandVec = do
    let dbId = mfDatabaseId factorization
    cachedSolvers <- readMVar cachedSolver
    case M.lookup dbId cachedSolvers of
        Nothing -> do
            reportMatrixOperation $ "No cached factorization found for database '" ++ T.unpack dbId ++ "', falling back to matrix assembly"
            let systemMatrix = mfSystemMatrix factorization
                n = mfActivityCount factorization
                techTriples = U.toList $ U.filter (\(SparseTriple i j _) -> i /= j) $ U.map (\(SparseTriple i j val) -> SparseTriple i j (-val)) systemMatrix
            solveSparseLinearSystemMUMPS [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- techTriples] (fromIntegral n) demandVec
        Just (_solver, n, dbLock) -> do
            reportMatrixOperation $ "Using cached solver for database '" ++ T.unpack dbId ++ "' (" ++ show n ++ " activities) - ultra-fast solve"

            withProgressTiming Solver "MUMPS cached solve" $ do
                result <-
                    catch
                        ( withMVar dbLock $ \_ -> do
                            cachedSolvers' <- readMVar cachedSolver
                            case M.lookup dbId cachedSolvers' of
                                Nothing -> return Nothing
                                Just (mumpsSolver, _, _) -> do
                                    let rhsVec = VS.fromList $ toList demandVec
                                    solutionVS <- mumpsSolve mumpsSolver rhsVec
                                    return $ Just (VS.convert solutionVS :: Vector)
                        )
                        ( \e -> do
                            reportMatrixOperation $ "MUMPS cached solver failed: " ++ show (e :: SomeException)
                            reportMatrixOperation "Falling back to fresh solver for this request"
                            return Nothing
                        )

                case result of
                    Just vec -> return vec
                    Nothing -> do
                        let systemMatrix = mfSystemMatrix factorization
                            techTriples = U.toList $ U.filter (\(SparseTriple i j _) -> i /= j) $ U.map (\(SparseTriple i j val) -> SparseTriple i j (-val)) systemMatrix
                        solveSparseLinearSystemMUMPS [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- techTriples] (fromIntegral n) demandVec

{- |
Batch variant: solve (I - A) * xᵢ = dᵢ for every demand vector dᵢ in one MUMPS
multi-RHS call per chunk, holding the per-database lock across the whole batch.

Chunk size caps the n·k·8-byte scratch buffer: for Ecoinvent (n≈18k), K_MAX=64
→ ~9 MB per call. Larger client batches are transparently split into chunks;
the lock is held once for the whole batch so concurrent single-solve requests
queue behind us (same semantics as a single solve of similar duration).

Falls back to per-demand solves via 'solveSparseLinearSystemWithFactorization'
if the cached solver is absent or the multi-solve raises an exception.
-}
solveSparseLinearSystemWithFactorizationMulti :: MatrixFactorization -> [Vector] -> IO [Vector]
solveSparseLinearSystemWithFactorizationMulti _ [] = pure []
solveSparseLinearSystemWithFactorizationMulti factorization demandVecs = do
    let dbId = mfDatabaseId factorization
        k = length demandVecs
    cachedSolvers <- readMVar cachedSolver
    case M.lookup dbId cachedSolvers of
        Nothing -> do
            reportMatrixOperation $
                "No cached factorization for '"
                    ++ T.unpack dbId
                    ++ "' — batch falling back to per-demand solve"
            mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs
        Just (_solver, _, dbLock) -> do
            reportMatrixOperation $ "Multi-RHS solve for '" ++ T.unpack dbId ++ "' (k=" ++ show k ++ ")"
            catch
                ( withProgressTiming Solver "MUMPS multi-RHS solve" $
                    withMVar dbLock $ \_ -> do
                        cached' <- readMVar cachedSolver
                        case M.lookup dbId cached' of
                            Nothing -> mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs
                            Just (solver, _, _) ->
                                concat <$> mapM (solveChunk solver) (chunksOf multiRhsChunkSize demandVecs)
                )
                ( \e -> do
                    reportMatrixOperation $
                        "Multi-RHS solve failed ("
                            ++ show (e :: SomeException)
                            ++ ") — falling back to per-demand solve"
                    mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs
                )
  where
    solveChunk :: MUMPSSolver -> [Vector] -> IO [Vector]
    solveChunk solver chunk = do
        let rhsList = map (VS.fromList . toList) chunk
        solutions <- mumpsSolveMulti solver rhsList
        pure $ map (VS.convert :: VS.Vector Double -> Vector) solutions

-- | Cap MUMPS scratch allocation at n·K·8 bytes per call (~10 MB on Ecoinvent).
multiRhsChunkSize :: Int
multiRhsChunkSize = 64

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
    | n <= 0 = [xs]
    | null xs = []
    | otherwise = let (h, t) = splitAt n xs in h : chunksOf n t

{- |
Compute biosphere inventories for k root processes with one multi-RHS solve.

Equivalent to @mapM (computeInventoryMatrix db)@ but all k scaling vectors come
from one MUMPS call against the cached factorization, and biosphere matrix
application runs in parallel across the resulting scaling vectors.

The caller must supply a 'MatrixFactorization' — typically obtained from
'SharedSolver.ensureFactorization'. There is no lazy fallback: falling back
would re-factorize per demand vector (~2 s each on Ecoinvent), defeating
the point of batching.

Precondition: every ProcessId must resolve against 'dbActivityIndex'. Callers
should validate with 'Service.resolveActivityAndProcessId' first —
'buildDemandVectorFromIndex' crashes on an out-of-range id.
-}
computeInventoryMatrixBatch :: Database -> MatrixFactorization -> [ProcessId] -> IO [Inventory]
computeInventoryMatrixBatch _ _ [] = pure []
computeInventoryMatrixBatch db fact pids = do
    let activityIndex = dbActivityIndex db
        demandVecs = map (buildDemandVectorFromIndex activityIndex) pids
    scalingVecs <- solveSparseLinearSystemWithFactorizationMulti fact demandVecs
    mapConcurrently (\x -> evaluate $! applyBiosphereMatrix db x) scalingVecs

{- |
Solve the fundamental LCA equation (I - A) * x = b using MUMPS direct solver.

This function:
1. Constructs the (I - A) system matrix from technosphere triplets
2. Uses MUMPS LU factorization for numerical stability
3. Returns the supply vector (scaling factors) for all activities

Performance: ~3s for 14,457 activities with 116K technosphere entries
-}

-- | Uses global mutex: MUMPS_SEQ create/factorize/destroy have global Fortran state.
solveSparseLinearSystemMUMPS :: [(Int, Int, Double)] -> Int -> Vector -> IO Vector
solveSparseLinearSystemMUMPS techTriples n demandVec = withMVar mumpsFactorizationMutex $ \_ -> do
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
        systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        allTriples = aggregateMatrixEntries (identityTriples ++ systemTechTriples)
        (rows, cols, vals) = unzip3 allTriples
        nnz = length allTriples

    reportMatrixOperation $ "Matrix assembly completed - starting MUMPS direct solve for " ++ show n ++ " activities"

    withProgressTiming Solver "MUMPS solve" $ do
        solver <- mumpsCreate n nnz rows cols vals
        mumpsAnalyzeAndFactorize solver
        let rhsVec = VS.fromList $ toList demandVec
        solutionVS <- mumpsSolve solver rhsVec
        mumpsDestroy solver

        let solutionVec = VS.convert solutionVS :: Vector
        when (U.any (\x -> isInfinite x || isNaN x) solutionVec) $ do
            reportMatrixOperation "ERROR: Solution contains infinity or NaN values"
            reportMatrixOperation "Matrix is singular - likely missing reference flows or treatment activities"

        return solutionVec

-- | Solve linear system (I - A) * x = b using MUMPS direct solver (wrapper)
solveSparseLinearSystem :: [(Int, Int, Double)] -> Int -> Vector -> IO Vector
solveSparseLinearSystem = solveSparseLinearSystemMUMPS

-- | Efficient sparse matrix multiplication: result = A * vector
applySparseMatrix :: [(Int, Int, Double)] -> Int -> Vector -> Vector
applySparseMatrix sparseTriples nRows inputVec =
    let resultVec = U.create $ do
            result <- MU.new nRows
            forM_ [0 .. nRows - 1] $ \i -> MU.write result i 0.0
            forM_ sparseTriples $ \(i, j, val) -> do
                if j < U.length inputVec
                    then do
                        oldVal <- MU.read result i
                        let newVal = oldVal + val * (inputVec U.! j)
                        MU.write result i newVal
                    else return ()
            return result
     in resultVec

{- |
Compute the scaling vector by solving (I - A)x = d.

Returns the supply vector where x[i] is the scaling factor for activity i
(how much of activity i is needed to produce one unit of the root activity).
-}
computeScalingVector :: Database -> ProcessId -> IO Vector
computeScalingVector db rootProcessId = do
    let activityCount = dbActivityCount db
        techTriples = dbTechnosphereTriples db
        activityIndex = dbActivityIndex db
        demandVec = buildDemandVectorFromIndex activityIndex rootProcessId
    solveSparseLinearSystem [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples] (fromIntegral activityCount) demandVec

{- |
Apply the biosphere matrix to a scaling vector: g = B * x.

Converts the scaling vector (activity scaling factors) into a biosphere inventory
(environmental flows) by multiplying with the biosphere intervention matrix.
-}
applyBiosphereMatrix :: Database -> Vector -> Inventory
applyBiosphereMatrix db supplyVec =
    let bioFlowCount = dbBiosphereCount db
        bioTriples = dbBiosphereTriples db
        bioFlowIndex = M.fromList $ zip (V.toList $ dbBiosphereFlows db) [0 ..]
        inventoryVec = applySparseMatrix [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples] (fromIntegral bioFlowCount) supplyVec
     in M.fromList
            [ (uuid, inventoryVec U.! idx)
            | (uuid, idx) <- M.toList bioFlowIndex
            , idx < U.length inventoryVec
            ]

computeInventoryMatrix :: Database -> ProcessId -> IO Inventory
computeInventoryMatrix db rootProcessId =
    applyBiosphereMatrix db <$> computeScalingVector db rootProcessId

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
        bioFlows = dbBiosphereFlows db
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
Sherman-Morrison rank-1 update for ingredient substitution (~4ms per variant).

A substitution applied at technosphere row 'row' (the consumer) is a rank-1
update to (I-A) represented by a perturbation vector u. Instead of
re-factorizing (~3s), we reuse the cached factorization:

    x' = x - z * (v^T * x) / (1 + v^T * z)

where z = inv(I-A) * u is one back-substitution and v = e_row.

The perturbation is passed as a sparse list @[(supplierIdx, delta)]@ so both
symmetric swaps (same-DB oldSup -> newSup: @[(old, +a), (new, -a)]@) and
asymmetric cross-DB cases work with the same code path:

* @[(old, +a)]@ — drop a root-DB supplier (new supplier lives in a dep DB;
  a virtual 'CrossDBLink' carries the new demand).
* @[(new, -a)]@ — add a root-DB supplier (old supplier was a dep-DB link;
  a virtual 'CrossDBLink' with negative coefficient cancels the static one).
* @[]@ — no root-matrix change (both old and new suppliers live in dep DBs);
  returns @x@ unchanged, bypassing the singularity check entirely.

Returns Left if the update is singular (|1 + v^T*z| < epsilon).
-}
shermanMorrisonVariant ::
    Database ->
    -- | Cached factorization from SharedSolver (Nothing = full re-solve)
    Maybe MatrixFactorization ->
    -- | Original scaling vector x
    Vector ->
    -- | Activity being modified (row = consumer index)
    Int ->
    -- | Non-zero entries of the perturbation vector u
    [(Int, Double)] ->
    -- | Variant scaling vector x'
    IO (Either T.Text Vector)
shermanMorrisonVariant db mFact x row perturb
    | null perturb = pure (Right x) -- no root-matrix change (cross-DB-only sub)
    | otherwise = do
        let n = U.length x
            u = U.accum (+) (U.replicate n 0.0) perturb
        z <- case mFact of
            Just f -> solveSparseLinearSystemWithFactorization f u
            Nothing -> do
                let techTriples = dbTechnosphereTriples db
                    activityCount = dbActivityCount db
                solveSparseLinearSystem
                    [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                    (fromIntegral activityCount)
                    u
        let vtx = x U.! row
            vtz = z U.! row
            denom = 1.0 + vtz
        if abs denom < 1e-12
            then pure $ Left "Sherman-Morrison update is singular \x2014 substitution creates a degenerate supply chain"
            else do
                let scale = vtx / denom
                pure $ Right $ U.imap (\i xi -> xi - scale * (z U.! i)) x

{- |
Accumulate cross-database supplier demands for a given root scaling vector.

For every 'CrossDBLink' whose consumer has non-zero scaling, add
@coefficient / normFactor(consumer) * scale@ to the demand on the supplier
@(actUUID, prodUUID)@. Results are grouped by the supplier's database name
so the caller can solve each dependency database in one multi-RHS batch.

The division by the consumer's normalization factor matches the convention
of 'dbTechnosphereTriples' (entries are already divided by the producer's
refAmount). 'cdlCoefficient' is the raw exchange amount per ref-unit of the
consumer, while the scaling vector is in per-kg units — so we re-scale
before accumulating.
-}
accumulateDepDemands ::
    Database ->
    Vector ->
    M.Map Text (M.Map (UUID, UUID) (Double, Text))
accumulateDepDemands db = accumulateDepDemandsWith db []

{- | Same as 'accumulateDepDemands' but folds over an additional list of
synthesized 'CrossDBLink' records (e.g. virtual links emitted by a
what-if substitution that re-routes a consumer to a dep-DB supplier).
Negative 'cdlCoefficient' is allowed: it cancels a static link at the
same supplier key because the inner 'M.unionWith mergeEntry' sums
amounts. A 0-net entry remains in the map and becomes a zero in
'depDemandsToVector' — silently dropped is never correct (it would
hide a bug); a zero is.
-}
accumulateDepDemandsWith ::
    Database ->
    [CrossDBLink] ->
    Vector ->
    M.Map Text (M.Map (UUID, UUID) (Double, Text))
accumulateDepDemandsWith db extraLinks scalingVec =
    foldr step M.empty (dbCrossDBLinks db ++ extraLinks)
  where
    actIdx = dbActivityIndex db
    procLookup = dbProcessIdLookup db
    normFactors = consumerNormFactors db
    step link acc =
        case M.lookup (cdlConsumerActUUID link, cdlConsumerProdUUID link) procLookup of
            Nothing -> acc
            Just consumerPid ->
                let consumerIdx = fromIntegral $ actIdx V.! fromIntegral consumerPid
                    consumerScale = scalingVec U.! consumerIdx
                    normFactor = M.findWithDefault 1.0 consumerPid normFactors
                    demand = cdlCoefficient link * consumerScale / normFactor
                    supplierKey = (cdlSupplierActUUID link, cdlSupplierProdUUID link)
                    entry = (demand, cdlExchangeUnit link)
                 in if demand == 0
                        then acc
                        else
                            M.insertWith
                                (M.unionWith mergeEntry)
                                (cdlSourceDatabase link)
                                (M.singleton supplierKey entry)
                                acc
    -- Two links hitting the same supplier should share the same exchange unit
    -- (it's the supplier's product, and the consumer-side exchange is written
    -- in that product's unit). Keep the first; conversion will catch any
    -- inconsistency because depDemandsToVector converts exchangeUnit →
    -- supplierRefUnit per entry.
    mergeEntry (a, u) (b, _) = (a + b, u)

{- | Normalization factor (ref-product amount) for each consumer activity that
appears in 'dbCrossDBLinks'. Mirrors the factor used in 'buildActivityTriplets'.
-}
consumerNormFactors :: Database -> M.Map ProcessId Double
consumerNormFactors db =
    M.fromList [(pid, activityNormalizationFactor db pid) | pid <- consumers]
  where
    procLookup = dbProcessIdLookup db
    consumers =
        [ pid
        | link <- dbCrossDBLinks db
        , Just pid <- [M.lookup (cdlConsumerActUUID link, cdlConsumerProdUUID link) procLookup]
        ]

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
unit pair — we never silently use raw values when units are incompatible.

Suppliers whose @(actUUID, prodUUID)@ does not resolve in the dep DB are
silently dropped (they've already been accepted as cross-DB links; a missing
ProcessId here means the dep DB was loaded but doesn't have that exact
supplier, which is recorded at link time).
-}
depDemandsToVector ::
    UnitConversion.UnitConfig ->
    -- | dep DB name, for error messages
    Text ->
    Database ->
    M.Map (UUID, UUID) (Double, Text) ->
    Either Text Vector
depDemandsToVector unitConfig depDbName depDb demands = do
    converted <- traverse convertEntry (M.toList demands)
    let n = fromIntegral (dbActivityCount depDb) :: Int
        entries = [e | Just e <- converted]
    Right $ U.accum (+) (U.replicate n 0.0) entries
  where
    actIdx = dbActivityIndex depDb
    procLookup = dbProcessIdLookup depDb
    activities = dbActivities depDb
    unitsDB = dbUnits depDb
    convertEntry ((actUUID, prodUUID), (amt, exchangeUnit)) =
        case M.lookup (actUUID, prodUUID) procLookup of
            Nothing -> Right Nothing -- supplier absent in dep DB; drop
            Just pid ->
                let act = activities V.! fromIntegral pid
                    refExs = [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)]
                    supplierUnit = case refExs of
                        (ex : _) -> getUnitNameForExchange unitsDB ex
                        [] -> ""
                    needsConversion =
                        UnitConversion.normalizeUnit exchangeUnit /= UnitConversion.normalizeUnit supplierUnit
                            && not (T.null exchangeUnit)
                            && not (T.null supplierUnit)
                    idx = fromIntegral (actIdx V.! fromIntegral pid) :: Int
                 in if not needsConversion
                        then Right (Just (idx, amt))
                        else case UnitConversion.convertUnit unitConfig exchangeUnit supplierUnit amt of
                            Just v -> Right (Just (idx, v))
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
                                        <> " \8212 add these units to [[units]] CSV"

{- |
Build the final demand vector f for LCA calculations.

The demand vector represents external demand for products from each activity:
- f[i] = 1.0 for the root activity (functional unit)
- f[i] = 0.0 for all other activities
-}
buildDemandVectorFromIndex :: V.Vector Int32 -> ProcessId -> Vector
buildDemandVectorFromIndex activityIndex rootProcessId =
    let n = V.length activityIndex
        rootIndex =
            if fromIntegral rootProcessId >= (0 :: Int) && fromIntegral rootProcessId < n
                then fromIntegral $ activityIndex V.! fromIntegral rootProcessId
                else error $ "FATAL: ProcessId not found in activity index: " ++ show rootProcessId
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1 :: Int]]

{- |
Pre-compute matrix factorization for concurrent inventory calculations.

This function builds the (I - A) system matrix from technosphere triplets and
pre-computes the LU factorization during server startup. The resulting
MatrixFactorization can be stored in the Database for fast concurrent solves.

Performance: ~3s factorization time for full Ecoinvent, saves 2.9s per inventory request
-}
precomputeMatrixFactorization :: Text -> [(Int, Int, Double)] -> Int -> IO MatrixFactorization
precomputeMatrixFactorization dbName techTriples n = withMVar mumpsFactorizationMutex $ \_ -> do
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
        systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        systemMatrix = aggregateMatrixEntries (identityTriples ++ systemTechTriples)
        (rows, cols, vals) = unzip3 systemMatrix
        nnz = length systemMatrix

    reportMatrixOperation $ "Pre-computing factorization for database '" ++ T.unpack dbName ++ "' (" ++ show n ++ " activities, " ++ show nnz ++ " entries)"

    solver <- mumpsCreate n nnz rows cols vals
    mumpsAnalyzeAndFactorize solver

    dbLock <- newMVar ()
    modifyMVar_ cachedSolver $ \solvers -> return $ M.insert dbName (solver, n, dbLock) solvers

    reportMatrixOperation $ "MUMPS solver for database '" ++ T.unpack dbName ++ "' factorized and cached"

    let factorization =
            MatrixFactorization
                { mfSystemMatrix = U.fromList [SparseTriple (fromIntegral i) (fromIntegral j) v | (i, j, v) <- systemMatrix]
                , mfActivityCount = fromIntegral n
                , mfDatabaseId = dbName
                }

    return factorization

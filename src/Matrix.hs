{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    depDemandsToVector,
    computeProcessLCIAContributions,
    perturbA,
    perturbABatch,
    perturbGlobal,
    applyShermanMorrison,
    applyShermanMorrisonV,
    buildDemandVectorFromIndex,
    solveSparseLinearSystem,
    applySparseMatrix,
    fromList,
    toList,
    precomputeMatrixFactorization,
    solveSparseLinearSystemWithFactorization,
    solveSparseLinearSystemWithFactorizationMulti,
    computeInventoryMatrixBatch,
    clearCachedSolver,
    chunksOf,
    readSolveCounter,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Concurrent.STM (
    STM,
    TQueue,
    TVar,
    atomically,
    flushTQueue,
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    writeTQueue,
    writeTVar,
 )
import Control.Exception (SomeException, catch, evaluate, throwIO, toException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
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

{- | The right-hand side @d@ of @(I - A) x = d@: what a functional unit asks
of the system, one entry per activity column. The solution @x@ it produces
is a plain 'Vector', which is what keeps the two from being confused: a
demand can be solved, and nothing else accepts one.
-}
newtype Demand = Demand {unDemand :: Vector}

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

{- | Demand a consumer DB places on one supplier @(activityUUID, productUUID)@:
the amount plus the consumer's exchange unit (for cross-DB unit conversion).
-}
type SupplierDemands = M.Map (UUID, UUID) (Double, Text)

-- | Per-dependency-DB supplier demands, keyed by source database name.
type DepDemands = M.Map Text SupplierDemands

{- | Per-database coalescing solver. A single worker thread owns the MUMPS
handle and drains a queue of solve requests, batching whatever has piled up
into one 'mumpsSolveMulti' call per round. Concurrent requests on the same
database therefore amortize one triangular descent/remontée across all
in-flight demands rather than queuing behind each other.

The MUMPS handle stays single-threaded (its mutable RHS/SOL workspace is the
real reason concurrent solves on a shared handle are unsafe — not MUMPS_SEQ
per se), but request *latency under contention* drops because K simultaneous
requests collapse into one solve.
-}
data CoalescingSolver = CoalescingSolver
    { csMumps :: !MUMPSSolver
    , csSize :: !Int
    , csInbox :: !(TQueue WorkerMsg)
    , csAlive :: !(TVar Bool)
    }

-- | Worker mailbox message: either a solve request or a shutdown signal.
data WorkerMsg
    = WSolve ![Vector] !(MVar (Either SomeException [Vector]))
    | WStop !(MVar ())

{- | Global cache for pre-factorized MUMPS solvers per database.
Each entry owns a worker thread that serializes the MUMPS handle and
coalesces concurrent requests into multi-RHS calls.
-}
{-# NOINLINE cachedSolver #-}
cachedSolver :: MVar (M.Map Text CoalescingSolver)
cachedSolver = unsafePerformIO $ newMVar M.empty

{- | Global mutex for MUMPS factorization/creation/destruction operations.
MUMPS_SEQ has global Fortran state that is NOT thread-safe for concurrent
create/factorize/destroy calls.
-}
{-# NOINLINE mumpsFactorizationMutex #-}
mumpsFactorizationMutex :: MVar ()
mumpsFactorizationMutex = unsafePerformIO $ newMVar ()

{- | Global mutex for the @mumpsSolveMulti@ FFI call. MUMPS_SEQ is built on
Fortran modules with SAVE'd state that two concurrent @mumps_solve@ calls
corrupt even when each call targets its own factorized handle. The FFI
import is annotated @safe@ (see mumps-hs FFI), so the GHC runtime is free
to schedule multiple OS threads inside MUMPS at once — exactly the race
the SAVE'd state can't survive. Empirically this manifests as a silent
SIGKILL mid-solve when several DBs' coalescing workers run @mumpsSolveMulti@
concurrently. Holding this mutex around the FFI call serializes those
solves across DBs while leaving the per-DB coalescing workers intact.
-}
{-# NOINLINE mumpsSolveMutex #-}
mumpsSolveMutex :: MVar ()
mumpsSolveMutex = unsafePerformIO $ newMVar ()

{- | Count of matrix solves performed since the process started.

A solve is the expensive thing this server does, and the only work that can
outlast a request: an analysis started just before an idle deadline must not
have the process exit under it. Callers therefore only ever ask whether the
number moved between two moments, never what it means, so the double count on
a fallback path (a batch solve that degrades to per-demand solves) is harmless.
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
outlasts the whole timeout is still cut off mid-flight — a ceiling that
stands far above real work, since solves take seconds and timeouts minutes.
-}
countingSolve :: IO a -> IO a
countingSolve act = bumpSolveCounter *> act <* bumpSolveCounter

-- ---------------------------------------------------------------------------
-- Coalescing solver: per-database worker that batches concurrent requests
-- into one mumpsSolveMulti call.
-- ---------------------------------------------------------------------------

{- | Fork a worker thread bound to the given factorized MUMPS handle and
return the handle paired with its inbox + liveness flag. The worker owns
the MUMPS instance for the rest of its lifetime; only it touches the
mutable RHS/SOL workspace.
-}
spawnCoalescingSolver :: MUMPSSolver -> Int -> IO CoalescingSolver
spawnCoalescingSolver solver n = do
    inbox <- newTQueueIO
    alive <- newTVarIO True
    void $ forkIO (workerLoop solver inbox)
    pure $ CoalescingSolver solver n inbox alive

{- | Submit a batch of demand vectors to the worker and block until the
solution comes back. Throws if the solver has been shut down (csAlive
flipped before we managed to enqueue) or if the underlying MUMPS call
raises — caller decides whether to fall back.
-}
submitBatch :: CoalescingSolver -> [Vector] -> IO [Vector]
submitBatch _ [] = pure []
submitBatch cs demands = do
    rv <- newEmptyMVar
    accepted <- atomically (offer cs (WSolve demands rv))
    unless accepted $
        throwIO (userError "MUMPS solver shut down before request was accepted")
    result <- takeMVar rv
    case result of
        Left e -> throwIO e
        Right sols -> pure sols

-- | Single-RHS convenience wrapper around 'submitBatch'.
submitOne :: CoalescingSolver -> Vector -> IO Vector
submitOne cs demand = do
    sols <- submitBatch cs [demand]
    case sols of
        [v] -> pure v
        _ -> throwIO (userError "MUMPS coalescing solver: unexpected batch cardinality")

-- | Atomic check-then-write: only enqueue if the solver is still alive.
offer :: CoalescingSolver -> WorkerMsg -> STM Bool
offer cs msg = do
    alive <- readTVar (csAlive cs)
    if alive
        then writeTQueue (csInbox cs) msg >> pure True
        else pure False

{- | Drain pending requests, run them as one multi-RHS solve, demux the
result back to each requester. On exception, every request in the batch
sees the same error. Exits cleanly when a 'WStop' is observed.

Note on MUMPS_SEQ thread-safety: the per-DB worker is the only thread that
touches its MUMPS handle's mutable RHS/SOL workspace, so intra-DB races on
the workspace can't happen. Inter-DB races are a separate hazard: MUMPS_SEQ
modules carry SAVE'd Fortran state that corrupts under concurrent
@mumps_solve@ calls even on distinct handles. 'mumpsSolveMutex' (acquired
inside 'solveChunk') serializes that boundary.
-}
workerLoop :: MUMPSSolver -> TQueue WorkerMsg -> IO ()
workerLoop solver inbox = do
    first <- atomically $ readTQueue inbox
    rest <- atomically $ flushTQueue inbox
    let (solves, stops) = partitionMsgs (first : rest)
    runSolves solver solves
    forM_ stops $ \ack -> putMVar ack ()
    if null stops
        then workerLoop solver inbox
        else drainShutdown inbox

{- | After observing a stop signal, flush anything that snuck in and
reply with a shutdown error so requesters never deadlock. Racing
submitters that committed before 'csAlive' flipped end up here.
-}
drainShutdown :: TQueue WorkerMsg -> IO ()
drainShutdown inbox = do
    leftover <- atomically $ flushTQueue inbox
    forM_ leftover $ \case
        WSolve _ rv ->
            putMVar rv (Left $ toException $ userError "MUMPS solver shut down")
        WStop ack -> putMVar ack ()

-- | Run a batch as one multi-RHS solve (chunked to bound scratch memory).
runSolves ::
    MUMPSSolver ->
    [([Vector], MVar (Either SomeException [Vector]))] ->
    IO ()
runSolves _ [] = pure ()
runSolves solver pendings = do
    let flatRhs = concatMap fst pendings
        sizes = map (length . fst) pendings
    outcome <- try $ runMultiSolveChunked solver flatRhs
    case outcome of
        Left (e :: SomeException) ->
            forM_ pendings $ \(_, rv) -> putMVar rv (Left e)
        Right allSols ->
            forM_ (zip pendings (splitBySizes sizes allSols)) $ \((_, rv), sols) ->
                putMVar rv (Right sols)

-- | Chunked multi-RHS solve, mirroring the original chunking budget.
runMultiSolveChunked :: MUMPSSolver -> [Vector] -> IO [Vector]
runMultiSolveChunked solver demands =
    concat <$> mapM (solveChunk solver) (chunksOf multiRhsChunkSize demands)

solveChunk :: MUMPSSolver -> [Vector] -> IO [Vector]
solveChunk solver chunk = do
    let rhsList = map (VS.fromList . toList) chunk
    solutions <- withMVar mumpsSolveMutex $ \_ -> mumpsSolveMulti solver rhsList
    pure $ map (VS.convert :: VS.Vector Double -> Vector) solutions

-- | Split a flat list of solutions back into per-request slices.
splitBySizes :: [Int] -> [a] -> [[a]]
splitBySizes [] _ = []
splitBySizes (n : ns) xs =
    let (h, t) = splitAt n xs in h : splitBySizes ns t

partitionMsgs ::
    [WorkerMsg] ->
    ([([Vector], MVar (Either SomeException [Vector]))], [MVar ()])
partitionMsgs = foldr step ([], [])
  where
    step (WSolve d rv) (ss, ts) = ((d, rv) : ss, ts)
    step (WStop ack) (ss, ts) = (ss, ack : ts)

{- | Clear cached solver for a database (call when unloading).

Removes the entry from the cache atomically (so new submissions fall back),
flips the 'csAlive' flag so any racing submitter that already had a handle
on the 'CoalescingSolver' record sees it dead and throws, signals the worker
to drain its inbox, then destroys the MUMPS instance under the global
factorization mutex (matching the create-side serialization).
-}
clearCachedSolver :: Text -> IO ()
clearCachedSolver dbName = do
    mCs <- modifyMVar cachedSolver $ \cache ->
        case M.lookup dbName cache of
            Just cs -> pure (M.delete dbName cache, Just cs)
            Nothing -> pure (cache, Nothing)
    forM_ mCs $ \cs -> do
        ack <- newEmptyMVar
        -- Mark dead and submit the stop signal in one transaction so any
        -- racing submitter either observes !alive (throws, no enqueue) or
        -- commits before the stop and gets a shutdown reply from the worker.
        atomically $ do
            writeTVar (csAlive cs) False
            writeTQueue (csInbox cs) (WStop ack)
        takeMVar ack
        withMVar mumpsFactorizationMutex $ \_ -> mumpsDestroy (csMumps cs)

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
solveSparseLinearSystemWithFactorization factorization demandVec = countingSolve $ do
    let dbId = mfDatabaseId factorization
    cachedSolvers <- readMVar cachedSolver
    case M.lookup dbId cachedSolvers of
        Nothing -> do
            reportMatrixOperation $ "No cached factorization found for database '" ++ T.unpack dbId ++ "', falling back to matrix assembly"
            fallbackFreshSolve factorization demandVec
        Just cs -> do
            reportMatrixOperation $ "Using cached solver for database '" ++ T.unpack dbId ++ "' (" ++ show (csSize cs) ++ " activities) - ultra-fast solve"
            withProgressTiming Solver "MUMPS cached solve" $ do
                result <-
                    catch
                        (Just <$> submitOne cs demandVec)
                        ( \e -> do
                            reportMatrixOperation $ "MUMPS cached solver failed: " ++ show (e :: SomeException)
                            reportMatrixOperation "Falling back to fresh solver for this request"
                            pure Nothing
                        )
                case result of
                    Just vec -> pure vec
                    Nothing -> fallbackFreshSolve factorization demandVec

-- | Cold-path fallback: rebuild (I-A) and solve from scratch via MUMPS.
fallbackFreshSolve :: MatrixFactorization -> Vector -> IO Vector
fallbackFreshSolve factorization demandVec =
    let systemMatrix = mfSystemMatrix factorization
        n = mfActivityCount factorization
        techTriples =
            U.toList $
                U.filter (\(SparseTriple i j _) -> i /= j) $
                    U.map (\(SparseTriple i j val) -> SparseTriple i j (-val)) systemMatrix
     in solveSparseLinearSystemMUMPS
            [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- techTriples]
            (fromIntegral n)
            demandVec

{- |
Batch variant: solve (I - A) * xᵢ = dᵢ for every demand vector dᵢ in one MUMPS
multi-RHS call per chunk. The request is submitted to the per-database
coalescing worker, which may merge it with other in-flight requests into a
single 'mumpsSolveMulti' call before chunking.

Chunk size caps the n·k·8-byte scratch buffer: for Ecoinvent (n≈18k), K_MAX=64
→ ~9 MB per call. Larger batches (whether from this caller or coalesced from
several callers) are split transparently across chunks.

Falls back to per-demand solves via 'solveSparseLinearSystemWithFactorization'
if the cached solver is absent or the multi-solve raises an exception.
-}
solveSparseLinearSystemWithFactorizationMulti :: MatrixFactorization -> [Vector] -> IO [Vector]
solveSparseLinearSystemWithFactorizationMulti _ [] = pure []
solveSparseLinearSystemWithFactorizationMulti factorization demandVecs = countingSolve $ do
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
        Just cs -> do
            reportMatrixOperation $ "Multi-RHS solve for '" ++ T.unpack dbId ++ "' (k=" ++ show k ++ ")"
            catch
                (withProgressTiming Solver "MUMPS multi-RHS solve" (submitBatch cs demandVecs))
                ( \e -> do
                    reportMatrixOperation $
                        "Multi-RHS solve failed ("
                            ++ show (e :: SomeException)
                            ++ ") — falling back to per-demand solve"
                    mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs
                )

-- | Cap MUMPS scratch allocation at n·K·8 bytes per call (~10 MB on Ecoinvent).
multiRhsChunkSize :: Int
multiRhsChunkSize = 64

{- | Split a list into successive chunks of at most @n@ elements (a non-positive
@n@ yields the whole list as a single chunk).
-}
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
    scalingVecs <- solveSparseLinearSystemWithFactorizationMulti fact (coerce demandVecs)
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
solveSparseLinearSystemMUMPS techTriples n demandVec = countingSolve $ withMVar mumpsFactorizationMutex $ \_ -> do
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
computeScalingVector :: Database -> ProcessId -> IO Vector
computeScalingVector db rootProcessId = do
    let activityCount = dbActivityCount db
        techTriples = dbTechnosphereTriples db
        activityIndex = dbActivityIndex db
        demandVec = buildDemandVectorFromIndex activityIndex rootProcessId
    solveSparseLinearSystem [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples] (fromIntegral activityCount) (unDemand demandVec)

{- |
Apply the biosphere matrix to a scaling vector: g = B * x.

Converts the scaling vector (activity scaling factors) into a biosphere inventory
(environmental flows) by multiplying with the biosphere intervention matrix.

Walks 'dbBiosphereTriples' as an unboxed 'U.Vector' directly. The previous
implementation went via @U.toList bioTriples@ → list of boxed
@(Int,Int,Double)@ tuples → 'applySparseMatrix', allocating ~150 MB per pid
on agribalyse's ~4.5M biosphere triples. Profiling on a 633-pid batch run
attributed 56% of total allocation and 14% of CPU to that path
(@applyBiosphereMatrix.inventoryVec@). With the list intermediate gone, the
matvec is a tight 'U.forM_' over contiguous memory.
-}
applyBiosphereMatrix :: Database -> Vector -> Inventory
applyBiosphereMatrix db supplyVec =
    let bioFlowCount = fromIntegral (dbBiosphereCount db) :: Int
        bioTriples = dbBiosphereTriples db
        bioFlows = dbBiosphereOrder db
        supplyLen = U.length supplyVec
        inventoryVec = U.create $ do
            mv <- MU.replicate bioFlowCount 0.0
            U.forM_ bioTriples $ \(SparseTriple i j v) -> do
                let !ci = fromIntegral i :: Int
                    !cj = fromIntegral j :: Int
                when (cj < supplyLen) $ do
                    old <- MU.unsafeRead mv ci
                    MU.unsafeWrite mv ci (old + v * U.unsafeIndex supplyVec cj)
            pure mv
        invLen = U.length inventoryVec
     in M.fromList
            [ (bioFlows V.! i, U.unsafeIndex inventoryVec i)
            | i <- [0 .. V.length bioFlows - 1]
            , i < invLen
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

* __Single matrix entry__ @A_ij -= Δ@ — @col = j@, @perturb = [(i, Δ)]@.
* __Symmetric supplier swap__ at consumer @j@ (drop @old@ at coefficient
  @a@, add @new@ at the same amount) — @col = j@,
  @perturb = [(old, +a), (new, -a)]@. The @+a@ on @old@ subtracts @a@
  from @A_(old,j)@ (was @a@, becomes @0@); the @-a@ on @new@ adds @a@
  to @A_(new,j)@ (was @0@, becomes @a@).
* __Asymmetric cross-DB substitution__ where one side lives in a dep DB —
  pass only the root-side entry; the dep-side change is carried by a
  virtual 'CrossDBLink'.
* __No-op__ — @perturb = []@ returns @x@ unchanged (bypasses the
  singularity check).

Returns @Left@ if the update is singular (@|1 + v^T·z| < epsilon@), which
means the perturbation drives @(I-A)@ into a degenerate state.
-}
perturbA ::
    Database ->
    -- | Cached factorization from SharedSolver (Nothing = full re-solve)
    Maybe MatrixFactorization ->
    -- | Baseline scaling vector x
    Vector ->
    -- | Consumer column index (j in @A_ij@)
    Int ->
    -- | Sparse perturbation u on the supplier axis: list of (row i, Δ)
    [(Int, Double)] ->
    -- | Perturbed scaling vector x'
    IO (Either T.Text Vector)
perturbA db mFact x col perturb
    | null perturb = pure (Right x)
    | otherwise = do
        z <- solveRankOneZ db mFact (U.length x) perturb
        pure $ applyShermanMorrison x col z

{- | Solve @z = inv(I-A) * u@ for a sparse supplier-axis perturbation @u@,
reusing the cached factorization when present. Shared by the per-edge
('perturbA') and global ('perturbGlobal') rank-1 paths so the back-sub
plumbing lives in one place.
-}
solveRankOneZ :: Database -> Maybe MatrixFactorization -> Int -> [(Int, Double)] -> IO Vector
solveRankOneZ db mFact n perturb =
    let u = U.accum (+) (U.replicate n 0.0) perturb
     in case mFact of
            Just f -> solveSparseLinearSystemWithFactorization f u
            Nothing ->
                solveSparseLinearSystem
                    [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
                    (fromIntegral (dbActivityCount db))
                    u

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
@B@ lives in a dependency DB); @v@ is the consumer-axis projection — @A@'s
technosphere row (its coefficient at each consumer). The single back-sub
@z = inv(I-A)·u@ replaces the @N@ solves an edge-by-edge expansion needs.

An empty @u@ or @v@ leaves @x@ unchanged; callers surface "A consumed
nowhere" as an explicit error before reaching here, so an empty @v@ is
never a silent no-op in practice.
-}
perturbGlobal ::
    Database ->
    Maybe MatrixFactorization ->
    Vector ->
    -- | u : sparse supplier-axis perturbation
    [(Int, Double)] ->
    -- | v : sparse consumer-axis projection (A's technosphere row)
    [(Int, Double)] ->
    IO (Either T.Text Vector)
perturbGlobal db mFact x u v
    | null u || null v = pure (Right x)
    | otherwise = do
        z <- solveRankOneZ db mFact (U.length x) u
        pure $ applyShermanMorrisonV x v z

{- |
Batched 'perturbA': computes z_k = inv(I-A) * u_k for every non-empty
perturbation in __one__ MUMPS multi-RHS solve, then applies the
Sherman-Morrison formula per-k in pure Haskell.

This is the right entry point for sensitivity sweeps — the per-perturbation
back-substitution is the only thing that touches the global MUMPS lock, so
batching collapses N serialized back-subs into one chunked multi-RHS call
(see 'solveSparseLinearSystemWithFactorizationMulti').

Empty perturbations short-circuit to @Right x@ without consuming a solve slot.
Order of results matches the input.
-}
perturbABatch ::
    Database ->
    -- | Cached factorization (Nothing = per-spec re-solve, slow path)
    Maybe MatrixFactorization ->
    -- | Baseline scaling vector x
    Vector ->
    -- | One @(col, perturb)@ per spec, in caller order
    [(Int, [(Int, Double)])] ->
    -- | One result per spec, in the same order
    IO [Either T.Text Vector]
perturbABatch db mFact x specs = do
    let n = U.length x
        indexed = zip [0 :: Int ..] specs
        nonEmpty =
            [ (idx, col, U.accum (+) (U.replicate n 0.0) p)
            | (idx, (col, p)) <- indexed
            , not (null p)
            ]
        us = [u | (_, _, u) <- nonEmpty]
    zs <- case (mFact, us) of
        (_, []) -> pure []
        (Just f, _) -> solveSparseLinearSystemWithFactorizationMulti f us
        (Nothing, _) -> do
            let techTriples = dbTechnosphereTriples db
                activityCount = dbActivityCount db
                aTriples =
                    [ (fromIntegral i, fromIntegral j, v)
                    | SparseTriple i j v <- U.toList techTriples
                    ]
            mapM (solveSparseLinearSystem aTriples (fromIntegral activityCount)) us
    let resultsByIdx :: M.Map Int (Either T.Text Vector)
        resultsByIdx =
            M.fromList
                [ (idx, applyShermanMorrison x col z)
                | ((idx, col, _), z) <- zip nonEmpty zs
                ]
    pure $ map (\(idx, _) -> M.findWithDefault (Right x) idx resultsByIdx) indexed

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
    DepDemands
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
    DepDemands
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
    SupplierDemands ->
    Either Text Demand
depDemandsToVector unitConfig depDbName depDb demands = do
    converted <- traverse convertEntry (M.toList demands)
    let n = fromIntegral (dbActivityCount depDb) :: Int
        entries = catMaybes converted
    Right $ Demand $ U.accum (+) (U.replicate n 0.0) entries
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
                        UnitConversion.unitKey unitConfig exchangeUnit /= UnitConversion.unitKey unitConfig supplierUnit
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
buildDemandVectorFromIndex :: V.Vector Int32 -> ProcessId -> Demand
buildDemandVectorFromIndex activityIndex rootProcessId =
    let n = V.length activityIndex
        rootIndex =
            if fromIntegral rootProcessId >= (0 :: Int) && fromIntegral rootProcessId < n
                then fromIntegral $ activityIndex V.! fromIntegral rootProcessId
                else error $ "FATAL: ProcessId not found in activity index: " ++ show rootProcessId
     in Demand $ fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1 :: Int]]

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

    cs <- spawnCoalescingSolver solver n
    modifyMVar_ cachedSolver $ \solvers -> pure $ M.insert dbName cs solvers

    reportMatrixOperation $ "MUMPS solver for database '" ++ T.unpack dbName ++ "' factorized and cached"

    let factorization =
            MatrixFactorization
                { mfSystemMatrix = U.fromList [SparseTriple (fromIntegral i) (fromIntegral j) v | (i, j, v) <- systemMatrix]
                , mfActivityCount = fromIntegral n
                , mfDatabaseId = dbName
                }

    return factorization

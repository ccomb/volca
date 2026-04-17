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

import Progress
import Types
import Control.Exception (catch, evaluate, SomeException)
import Control.Monad (forM_, when)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, readMVar, modifyMVar_)
import Data.Int (Int32)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Concurrent.Async (mapConcurrently)
import Numerical.MUMPS (MUMPSSolver, mumpsCreate, mumpsAnalyzeAndFactorize, mumpsSolve, mumpsSolveMulti, mumpsDestroy)
import System.IO.Unsafe (unsafePerformIO)  -- Used only for NOINLINE global singletons

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

-- | Global cache for pre-factorized MUMPS solvers per database.
-- Each entry has its own MVar lock for concurrent forward/backward solves.
{-# NOINLINE cachedSolver #-}
cachedSolver :: MVar (M.Map Text (MUMPSSolver, Int, MVar ()))
cachedSolver = unsafePerformIO $ newMVar M.empty

-- | Global mutex for MUMPS factorization/creation/destruction operations.
-- MUMPS_SEQ has global Fortran state that is NOT thread-safe for concurrent
-- create/factorize/destroy calls. Only mumpsSolve on an already-factorized
-- instance is safe (per-database locks protect same-instance concurrent access).
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
                result <- catch
                    (withMVar dbLock $ \_ -> do
                        cachedSolvers' <- readMVar cachedSolver
                        case M.lookup dbId cachedSolvers' of
                            Nothing -> return Nothing
                            Just (mumpsSolver, _, _) -> do
                                let rhsVec = VS.fromList $ toList demandVec
                                solutionVS <- mumpsSolve mumpsSolver rhsVec
                                return $ Just (VS.convert solutionVS :: Vector))
                    (\e -> do
                        reportMatrixOperation $ "MUMPS cached solver failed: " ++ show (e :: SomeException)
                        reportMatrixOperation "Falling back to fresh solver for this request"
                        return Nothing)

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
        k    = length demandVecs
    cachedSolvers <- readMVar cachedSolver
    case M.lookup dbId cachedSolvers of
        Nothing -> do
            reportMatrixOperation $ "No cached factorization for '" ++ T.unpack dbId
                ++ "' — batch falling back to per-demand solve"
            mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs
        Just (_solver, _, dbLock) -> do
            reportMatrixOperation $ "Multi-RHS solve for '" ++ T.unpack dbId ++ "' (k=" ++ show k ++ ")"
            catch
                (withProgressTiming Solver "MUMPS multi-RHS solve" $
                    withMVar dbLock $ \_ -> do
                        cached' <- readMVar cachedSolver
                        case M.lookup dbId cached' of
                            Nothing -> mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs
                            Just (solver, _, _) ->
                                concat <$> mapM (solveChunk solver) (chunksOf multiRhsChunkSize demandVecs))
                (\e -> do
                    reportMatrixOperation $ "Multi-RHS solve failed (" ++ show (e :: SomeException)
                        ++ ") — falling back to per-demand solve"
                    mapM (solveSparseLinearSystemWithFactorization factorization) demandVecs)
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
    | n <= 0    = [xs]
    | null xs   = []
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
computeInventoryMatrixBatch _  _    []   = pure []
computeInventoryMatrixBatch db fact pids = do
    let activityIndex = dbActivityIndex db
        demandVecs    = map (buildDemandVectorFromIndex activityIndex) pids
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
        bioFlowIndex = M.fromList $ zip (V.toList $ dbBiosphereFlows db) [0..]
        inventoryVec = applySparseMatrix [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples] (fromIntegral bioFlowCount) supplyVec
    in M.fromList [(uuid, inventoryVec U.! idx)
                  | (uuid, idx) <- M.toList bioFlowIndex
                  , idx < U.length inventoryVec]

computeInventoryMatrix :: Database -> ProcessId -> IO Inventory
computeInventoryMatrix db rootProcessId =
    applyBiosphereMatrix db <$> computeScalingVector db rootProcessId

{- |
Per-process LCIA contributions for one impact category.
contribution[p] = Σ_f  B[f,p] * s[p] * CF[f]   (in the method's impact unit)

Exact for cyclic systems because it uses the full matrix-solved scaling vector.
-}
computeProcessLCIAContributions
    :: Database
    -> Vector               -- ^ Scaling vector (from computeScalingVector)
    -> M.Map UUID Double    -- ^ CF map: flowUUID → CF value (single impact category)
    -> M.Map ProcessId Double  -- ^ ProcessId → LCIA contribution in impact unit
computeProcessLCIAContributions db scalingVec cfMap =
    let actIdx      = dbActivityIndex db
        bioTriples  = dbBiosphereTriples db
        bioFlows    = dbBiosphereFlows db
        -- Build inverse map: matrix column index → ProcessId
        colToProcess :: M.Map Int ProcessId
        colToProcess = M.fromList
            [ (fromIntegral (actIdx V.! pid), fromIntegral pid)
            | pid <- [0 .. V.length actIdx - 1]
            ]
        step acc (SparseTriple flowRow colIdx bioVal) =
            let flowUUID = bioFlows V.! fromIntegral flowRow
            in case M.lookup flowUUID cfMap of
                Nothing    -> acc
                Just cfVal ->
                    case M.lookup (fromIntegral colIdx :: Int) colToProcess of
                        Nothing  -> acc
                        Just pid ->
                            let s = scalingVec U.! fromIntegral colIdx
                            in M.insertWith (+) pid (bioVal * s * cfVal) acc
    in U.foldl' step M.empty bioTriples

{- |
Sherman-Morrison rank-1 update for ingredient substitution (~4ms per variant).

Swapping supplier oldSup -> newSup for activity 'row' is a rank-1 update to (I-A).
Instead of re-factorizing (~3s), we reuse the cached factorization:

    x' = x - z * (v^T * x) / (1 + v^T * z)

where z = inv(I-A) * u is one back-substitution, u = coeff*(e_oldSup - e_newSup),
and v = e_row.

Returns Left if the update is singular (|1 + v^T*z| < epsilon).
-}
shermanMorrisonVariant
    :: Database
    -> Maybe MatrixFactorization  -- ^ Cached factorization from SharedSolver (Nothing = full re-solve)
    -> Vector              -- ^ Original scaling vector x
    -> Int                 -- ^ Activity being modified (row = consumer index)
    -> Int                 -- ^ Old supplier index (to remove)
    -> Int                 -- ^ New supplier index (to add)
    -> Double              -- ^ Exchange coefficient a (amount consumed)
    -> IO (Either T.Text Vector)  -- ^ Variant scaling vector x'
shermanMorrisonVariant db mFact x row oldSup newSup coeff = do
    let n = U.length x
        u = fromList [if i == oldSup then coeff
                      else if i == newSup then -coeff
                      else 0.0
                     | i <- [0 .. n - 1]]
    z <- case mFact of
        Just f  -> solveSparseLinearSystemWithFactorization f u
        Nothing -> do
            let techTriples = dbTechnosphereTriples db
                activityCount = dbActivityCount db
            solveSparseLinearSystem
                [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                (fromIntegral activityCount) u
    let vtx = x U.! row
        vtz = z U.! row
        denom = 1.0 + vtz
    if abs denom < 1e-12
        then return $ Left "Sherman-Morrison update is singular \x2014 substitution creates a degenerate supply chain"
        else do
            let scale = vtx / denom
            return $ Right $ U.imap (\i xi -> xi - scale * (z U.! i)) x

{- |
Accumulate cross-database supplier demands for a given root scaling vector.

For every 'CrossDBLink' in the database whose consumer has non-zero scaling,
add @coefficient * scale@ to the demand on the supplier @(actUUID, prodUUID)@.
Results are grouped by the supplier's database name so the caller can solve
each dependency database in one multi-RHS batch.
-}
accumulateDepDemands :: Database -> Vector -> M.Map Text (M.Map (UUID, UUID) Double)
accumulateDepDemands db scalingVec =
    foldr step M.empty (dbCrossDBLinks db)
  where
    actIdx     = dbActivityIndex db
    procLookup = dbProcessIdLookup db
    step link acc =
        case M.lookup (cdlConsumerActUUID link, cdlConsumerProdUUID link) procLookup of
            Nothing -> acc
            Just consumerPid ->
                let consumerIdx   = fromIntegral $ actIdx V.! fromIntegral consumerPid
                    consumerScale = scalingVec U.! consumerIdx
                    demand        = cdlCoefficient link * consumerScale
                    supplierKey   = (cdlSupplierActUUID link, cdlSupplierProdUUID link)
                in if demand == 0
                     then acc
                     else M.insertWith (M.unionWith (+))
                            (cdlSourceDatabase link)
                            (M.singleton supplierKey demand)
                            acc

{- |
Convert a sparse supplier-demand map into a length-@n_dep@ demand vector for
the dependency database. Suppliers whose @(actUUID, prodUUID)@ does not resolve
in the dep DB are silently dropped.
-}
depDemandsToVector :: Database -> M.Map (UUID, UUID) Double -> Vector
depDemandsToVector depDb demands =
    let actIdx     = dbActivityIndex depDb
        procLookup = dbProcessIdLookup depDb
        n          = fromIntegral (dbActivityCount depDb) :: Int
        entries    = [ (fromIntegral (actIdx V.! fromIntegral pid), d)
                     | (key, d) <- M.toList demands
                     , Just pid <- [M.lookup key procLookup]
                     ]
    in U.accum (+) (U.replicate n 0.0) entries

{- |
Build the final demand vector f for LCA calculations.

The demand vector represents external demand for products from each activity:
- f[i] = 1.0 for the root activity (functional unit)
- f[i] = 0.0 for all other activities
-}
buildDemandVectorFromIndex :: V.Vector Int32 -> ProcessId -> Vector
buildDemandVectorFromIndex activityIndex rootProcessId =
    let n = V.length activityIndex
        rootIndex = if fromIntegral rootProcessId >= (0 :: Int) && fromIntegral rootProcessId < n
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

    let factorization = MatrixFactorization
            { mfSystemMatrix = U.fromList [SparseTriple (fromIntegral i) (fromIntegral j) v | (i, j, v) <- systemMatrix]
            , mfActivityCount = fromIntegral n
            , mfDatabaseId = dbName
            }

    return factorization

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
    computeInventoryWithDependencies,
    computeProcessLCIAContributions,
    shermanMorrisonVariant,
    buildDemandVectorFromIndex,
    solveSparseLinearSystem,
    applySparseMatrix,
    fromList,
    toList,
    initializeSolverForServer,
    precomputeMatrixFactorization,
    addFactorizationToDatabase,
    solveSparseLinearSystemWithFactorization,
    clearCachedSolver,
) where

import Progress
import Types
import Control.Exception (catch, SomeException)
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
import Numerical.MUMPS (MUMPSSolver, mumpsCreate, mumpsAnalyzeAndFactorize, mumpsSolve, mumpsDestroy)
import System.IO.Unsafe (unsafePerformIO)  -- Used only for NOINLINE global singletons

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

-- | Global cache for pre-factorized MUMPS solvers per database with thread synchronization
-- Maps database name to (MUMPS solver, activity count)
{-# NOINLINE cachedSolver #-}
cachedSolver :: MVar (M.Map Text (MUMPSSolver, Int))
cachedSolver = unsafePerformIO $ newMVar M.empty

-- Global mutex to serialize all solver operations (matrix assembly, solving, etc.)
-- MUMPS is not thread-safe for concurrent calls to the same instance
{-# NOINLINE solverGlobalMutex #-}
solverGlobalMutex :: MVar ()
solverGlobalMutex = unsafePerformIO $ newMVar ()

-- | Initialize solver for server lifetime. No-op for MUMPS (no global state needed).
initializeSolverForServer :: IO ()
initializeSolverForServer = return ()

-- | Clear cached solver for a database (call when unloading)
clearCachedSolver :: Text -> IO ()
clearCachedSolver dbName =
    modifyMVar_ cachedSolver $ \cache ->
        case M.lookup dbName cache of
            Just (solver, _) -> do
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

        Just (_solver, n) -> do
            reportMatrixOperation $ "Using cached solver for database '" ++ T.unpack dbId ++ "' (" ++ show n ++ " activities) - ultra-fast solve"

            withProgressTiming Solver "MUMPS cached solve" $ do
                result <- catch
                    (withMVar solverGlobalMutex $ \_ -> do
                        cachedSolvers' <- readMVar cachedSolver
                        case M.lookup dbId cachedSolvers' of
                            Nothing -> error $ "Cached solver for database '" ++ T.unpack dbId ++ "' disappeared during solve"
                            Just (mumpsSolver, _) -> do
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
Solve the fundamental LCA equation (I - A) * x = b using MUMPS direct solver.

This function:
1. Constructs the (I - A) system matrix from technosphere triplets
2. Uses MUMPS LU factorization for numerical stability
3. Returns the supply vector (scaling factors) for all activities

Performance: ~3s for 14,457 activities with 116K technosphere entries
-}
solveSparseLinearSystemMUMPS :: [(Int, Int, Double)] -> Int -> Vector -> IO Vector
solveSparseLinearSystemMUMPS techTriples n demandVec =
    withMVar solverGlobalMutex $ \_ -> do
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
    case dbCachedFactorization db of
        Just factorization -> solveSparseLinearSystemWithFactorization factorization demandVec
        Nothing -> solveSparseLinearSystem [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples] (fromIntegral activityCount) demandVec

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
    -> Vector              -- ^ Original scaling vector x
    -> Int                 -- ^ Activity being modified (row = consumer index)
    -> Int                 -- ^ Old supplier index (to remove)
    -> Int                 -- ^ New supplier index (to add)
    -> Double              -- ^ Exchange coefficient a (amount consumed)
    -> IO (Either T.Text Vector)  -- ^ Variant scaling vector x'
shermanMorrisonVariant db x row oldSup newSup coeff = do
    let n = U.length x
        u = fromList [if i == oldSup then coeff
                      else if i == newSup then -coeff
                      else 0.0
                     | i <- [0 .. n - 1]]
    z <- case dbCachedFactorization db of
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
Compute inventory with cross-database dependencies.

This function implements the block matrix back-substitution algorithm:
1. Solve the local database system: (I - A_local) x s_local = f
2. For each cross-database link, accumulate demand on the supplier
3. Group demand by dependency database
4. Recursively solve each dependency database
5. Sum inventories from all databases
-}
computeInventoryWithDependencies
    :: (UUID -> UUID -> Maybe Database)  -- ^ Lookup dependency DB by supplier (actUUID, prodUUID)
    -> Database                          -- ^ Current database
    -> ProcessId                         -- ^ Root activity
    -> IO Inventory
computeInventoryWithDependencies lookupDb db rootProcessId = do
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        bioFlowIndex = M.fromList $ zip (V.toList $ dbBiosphereFlows db) [0..]
        demandVec = buildDemandVectorFromIndex activityIndex rootProcessId

    localSupplyVec <- case dbCachedFactorization db of
        Just factorization -> solveSparseLinearSystemWithFactorization factorization demandVec
        Nothing -> solveSparseLinearSystem
            [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
            (fromIntegral activityCount)
            demandVec

    let localInventoryVec = applySparseMatrix
            [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
            (fromIntegral bioFlowCount)
            localSupplyVec
        localInventory = M.fromList
            [(uuid, localInventoryVec U.! idx) | (uuid, idx) <- M.toList bioFlowIndex, idx < U.length localInventoryVec]

        crossLinks = dbCrossDBLinks db
        processIdLookup = dbProcessIdLookup db

        crossDBDemands :: M.Map (UUID, UUID) Double
        crossDBDemands = foldr accumulateDemand M.empty crossLinks
          where
            accumulateDemand :: CrossDBLink -> M.Map (UUID, UUID) Double -> M.Map (UUID, UUID) Double
            accumulateDemand link acc =
                let consumerKey = (cdlConsumerActUUID link, cdlConsumerProdUUID link)
                    supplierKey = (cdlSupplierActUUID link, cdlSupplierProdUUID link)
                in case M.lookup consumerKey processIdLookup of
                    Nothing -> acc
                    Just consumerPid ->
                        let consumerIdx = fromIntegral $ activityIndex V.! fromIntegral consumerPid
                            consumerScale = localSupplyVec U.! consumerIdx
                            demand = cdlCoefficient link * consumerScale
                        in M.insertWith (+) supplierKey demand acc

    let processSupplier (supplierActUUID, supplierProdUUID) demand accIO = do
            acc <- accIO
            case lookupDb supplierActUUID supplierProdUUID of
                Nothing -> return acc
                Just depDb ->
                    case M.lookup (supplierActUUID, supplierProdUUID) (dbProcessIdLookup depDb) of
                        Nothing -> return acc
                        Just supplierPid -> do
                            depInventory <- computeInventoryWithDependencies lookupDb depDb supplierPid
                            let scaledInventory = M.map (* demand) depInventory
                            return $ M.unionWith (+) acc scaledInventory
    crossDBInventory <- M.foldrWithKey processSupplier (return M.empty) crossDBDemands

    return $ M.unionWith (+) localInventory crossDBInventory

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
precomputeMatrixFactorization dbName techTriples n =
    withMVar solverGlobalMutex $ \_ -> do
        let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
            systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
            systemMatrix = aggregateMatrixEntries (identityTriples ++ systemTechTriples)
            (rows, cols, vals) = unzip3 systemMatrix
            nnz = length systemMatrix

        reportMatrixOperation $ "Pre-computing factorization for database '" ++ T.unpack dbName ++ "' (" ++ show n ++ " activities, " ++ show nnz ++ " entries)"

        solver <- mumpsCreate n nnz rows cols vals
        mumpsAnalyzeAndFactorize solver

        modifyMVar_ cachedSolver $ \solvers -> return $ M.insert dbName (solver, n) solvers

        reportMatrixOperation $ "MUMPS solver for database '" ++ T.unpack dbName ++ "' factorized and cached"

        let factorization = MatrixFactorization
                { mfSystemMatrix = U.fromList [SparseTriple (fromIntegral i) (fromIntegral j) v | (i, j, v) <- systemMatrix]
                , mfActivityCount = fromIntegral n
                , mfDatabaseId = dbName
                }

        return factorization

-- | Add a pre-computed matrix factorization to the database.
addFactorizationToDatabase :: Database -> MatrixFactorization -> Database
addFactorizationToDatabase db factorization = db { dbCachedFactorization = Just factorization }

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SharedSolver
Description : The solver of a loaded database, factorized at its first solve

The matrix of a database is not factorized when the database loads but at the
first solve that needs it, so a database nobody queries costs nothing. The
factors are then kept with the solver and read by every request at once.
-}
module SharedSolver (
    -- * Shared solver types
    SharedSolver,

    -- * Solver management
    createSharedSolver,
    namingColumns,
    correctedFor,

    -- * Concurrent solving
    solveWithSharedSolver,
    solveMultiWithSharedSolver,
    factorized,

    -- * Cached scaling / inventory
    computeScalingVectorCached,
    computeInventoryMatrixCached,

    -- * Cross-database back-substitution
    DepSolverLookup,
    CrossDBSolution (..),
    Reached (..),
    atZero,
    computeInventoryMatrixWithDepsCached,
    computeInventoryMatrixBatchWithDepsCached,
    goWithDeps,
    goWithDepsFromScalings,
    maxDepsDepth,
    mergeSolutions,
    prepareDepDemandVecs,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Matrix (
    Demand (..),
    DepDemands,
    Inventory,
    Vector,
    accumulateDepDemandsWith,
    applyBiosphereMatrix,
    buildDemandVector,
    byPosition,
    depDemandsToVector,
    factorizeSystem,
    refusal,
    solveOne,
    solveWithFactors,
 )
import Matrix.Dense (LU, factorLU, solveLU)
import qualified Matrix.Sparse as Sparse
import Method.Mapping (LongTermMode (..))
import Progress
import Types
import UnitConversion (UnitConfig)

{- | The solver of one matrix. Its factors are a field of the solver: a
database that is rebuilt gets a new solver and with it new factors, and a
request already solving keeps the ones it holds.
-}
data SharedSolver = SharedSolver
    { solverFactors :: MVar (Maybe (Either Sparse.Refused Sparse.Factors))
    {- ^ 'Nothing' until the first solve. A refusal is kept as factors are: it
    is a verdict on the matrix, the same at every try.
    -}
    , solverTechTriples :: [(Int, Int, Double)]
    -- ^ Technosphere matrix data
    , solverActivityCount :: Int
    -- ^ Number of activities
    , solverDbName :: Text
    -- ^ What the log calls this solver: its database, and what sets it apart from the loaded one
    , solverColumnName :: Int -> Text
    -- ^ How a refusal names the column it points at
    , solverCorrection :: Maybe Correction
    -- ^ The columns a request moved, carried into every solve
    }

{- | A change of k columns C of the matrix, carried into every solve without
a factorisation of the changed matrix (Woodbury). The change is M′ = M + U Eᵀ,
U holding each column's change and E selecting C; with Z = M⁻¹U and
S = I + Z[C,:], the solution of M′ is @y - Z (S⁻¹ y[C])@ for @y@ that of M.
-}
data Correction
    = Correction
        -- | C
        [Int]
        -- | Z, one column per column of C
        [Vector]
        -- | S
        LU

-- | The solution of the corrected matrix from that of the loaded one.
corrected :: Maybe Correction -> Vector -> Vector
corrected Nothing y = y
corrected (Just (Correction columns zs s)) y =
    foldl' (\x (w, z) -> U.zipWith (\xi zi -> xi - w * zi) x z) y (zip (U.toList (solveLU s (U.fromList (map (y U.!) columns)))) zs)

{- | The solver of the loaded matrix with these columns changed: each column
with the entries of M = I − A it adds, by row. It shares the loaded solver's
factors, so nothing is factorised again. Refused when the changed matrix is
singular.
-}
correctedFor :: SharedSolver -> [(Int, [(Int, Double)])] -> IO (Either Text SharedSolver)
correctedFor SharedSolver{solverCorrection = Just _} _ = pure (Left "a corrected solver is not corrected again")
correctedFor base change = runExceptT $ do
    zs <- ExceptT (solveMultiWithSharedSolver base [Demand (U.accum (+) (U.replicate (solverActivityCount base) 0) entries) | (_, entries) <- change])
    let s = U.fromList [(if i == j then 1 else 0) + z U.! c | (i, c) <- zip [0 :: Int ..] columns, (j, z) <- zip [0 ..] zs]
    except $ (\lu -> base{solverCorrection = Just (Correction columns zs lu)}) <$> first ("these values make the technosphere singular: " <>) (factorLU (length columns) s)
  where
    columns = map fst change

-- | Create a shared solver. No factorization happens here – deferred to first solve.
createSharedSolver :: Text -> [(Int, Int, Double)] -> Int -> IO SharedSolver
createSharedSolver dbName techTriples activityCount = do
    reportProgress Info $ "Creating shared solver for '" ++ show dbName ++ "' (factorization deferred)"
    factors <- newMVar Nothing
    return $ SharedSolver factors techTriples activityCount dbName byPosition Nothing

{- | The same solver, naming this way the column it points at when its matrix
is refused: a position says nothing to whoever asked.
-}
namingColumns :: (Int -> Text) -> SharedSolver -> SharedSolver
namingColumns name solver = solver{solverColumnName = name}

{- | The factors of the solver's matrix, made by the first request that needs
them; requests arriving meanwhile wait for that one instead of factorizing
too. A computation that did not happen is not kept: the next request tries
again.
-}
factorsOf :: SharedSolver -> IO (Either Text Sparse.Factors)
factorsOf solver = readMVar known >>= maybe (modifyMVar known firstUse) (pure . verdict)
  where
    known :: MVar (Maybe (Either Sparse.Refused Sparse.Factors))
    known = solverFactors solver
    verdict :: Either Sparse.Refused Sparse.Factors -> Either Text Sparse.Factors
    verdict = first (refusal (solverColumnName solver))
    -- Looks again: another request may have factorized between the read and the take.
    firstUse :: Maybe (Either Sparse.Refused Sparse.Factors) -> IO (Maybe (Either Sparse.Refused Sparse.Factors), Either Text Sparse.Factors)
    firstUse (Just found) = pure (Just found, verdict found)
    firstUse Nothing = do
        reportProgress Info $ "Factorizing '" ++ show (solverDbName solver) ++ "' on first use"
        factorizeSystem (solverActivityCount solver) (solverTechTriples solver) >>= \case
            Left failure -> pure (Nothing, Left failure)
            Right found -> do
                for_ found (reportProgress Info . measured solver . Sparse.report)
                pure (Just found, verdict found)

-- | What a factorization measured, for the log.
measured :: SharedSolver -> Sparse.Report -> String
measured solver found =
    "Factorized '"
        ++ show (solverDbName solver)
        ++ "': factors of "
        ++ show (Sparse.factorBytes found `div` 1048576)
        ++ " MiB, "
        ++ show (Sparse.denseColumns found)
        ++ " columns eliminated as one full block, "
        ++ maybe "" weakest (Sparse.weakestPivot found)
        ++ "a trial solve off by "
        ++ show (Sparse.trialError found)
  where
    weakest :: Sparse.Pivot -> String
    weakest pivot = "the weakest pivot keeps " ++ show (Sparse.pivotShare pivot) ++ " of its diagonal at " ++ T.unpack (solverColumnName solver (Sparse.pivotColumn pivot)) ++ ", "

-- | Solve for one demand. The first call factorizes.
solveWithSharedSolver :: SharedSolver -> Demand -> IO (Either Text Vector)
solveWithSharedSolver solver = solveOne (solveMultiWithSharedSolver solver . coerce) . unDemand

{- | Whether the solver has been through its factorization, to factors or to a
refusal: false until the first solve.
-}
factorized :: SharedSolver -> IO Bool
factorized = fmap isJust . readMVar . solverFactors

{- | Solve for several demands in one call to the solver, answering in their
order. The first call factorizes; every later one reads the same factors.
-}
solveMultiWithSharedSolver :: SharedSolver -> [Demand] -> IO (Either Text [Vector])
solveMultiWithSharedSolver solver demands = runExceptT $ do
    factors <- ExceptT (factorsOf solver)
    map (corrected (solverCorrection solver)) <$> ExceptT (solveWithFactors factors (coerce demands))

{- | Compute the scaling vector for @pid@, routing through the shared solver's
lazy factorization cache. Same shape as 'Matrix.computeScalingVector' but
amortizes factorization across every call in a server's lifetime – the
right default for endpoint handlers.
-}
computeScalingVectorCached :: Database -> SharedSolver -> ProcessId -> IO (Either Text Vector)
computeScalingVectorCached db solver pid =
    either (pure . Left) (solveWithSharedSolver solver) (buildDemandVector db pid)

-- | Inventory for @pid@ using the shared-solver factorization cache.
computeInventoryMatrixCached :: Database -> SharedSolver -> ProcessId -> IO (Either Text Inventory)
computeInventoryMatrixCached db solver pid =
    fmap (applyBiosphereMatrix db) <$> computeScalingVectorCached db solver pid

{- | Resolve a dependency database by name into its (Database, SharedSolver) pair.
Returns @Nothing@ if the dep DB is not loaded; the caller treats it as a
zero-contribution supplier and continues.
-}
type DepSolverLookup = Text -> IO (Maybe (Database, SharedSolver))

{- | Per-pid result of a cross-DB inventory solve: the merged biosphere
'Inventory' plus the per-DB scaling vectors that produced it.

The scaling vectors are needed by the regionalized LCIA path, which scores
each DB's biosphere triples against THAT DB's scaling – a sum across all
DBs reached at request time, not a single dot product against the root.
Without per-DB scalings, dep-DB regional CFs are silently invisible
(the merged 'Inventory' has the emissions but lost the per-activity
location context the regional CF lookup needs).

'csScalings' lists every DB visited: root first, then each dep DB followed
by the DBs it reached in turn, siblings in name order – both the order and
the recursion depth follow the same dep-graph walk that built
'csInventory'. The list is non-empty: the root's 'Reached' entry is always
added by the top-level solve; dep entries are appended as the recursion
fans out.
A DB whose scaling vector is the zero vector for this pid is still
included; the solver never silently omits a participating DB.
-}
data CrossDBSolution = CrossDBSolution
    { csInventory :: !Inventory
    , csScalings :: !(NonEmpty Reached)
    , csLongTerm :: !LongTermMode
    {- ^ The policy 'csInventory' was filtered under. A scaling vector cannot
    carry it, a column not being a flow, so a path that reads the vectors has
    to apply this itself; keeping it beside them is what stops the two from
    disagreeing. A solve leaves it 'IncludeLongTerm' and one function sets it.
    -}
    }

-- | One database a cross-database solve reached, at the scaling the solve found for it.
data Reached = Reached
    { reachedName :: !Text
    -- ^ the name the database is loaded under
    , reachedDatabase :: !Database
    , reachedScaling :: !Vector
    -- ^ indexed by the activities of 'reachedDatabase'
    }

{- | The same database at a zero scaling: it keeps its place in a solution and
adds nothing to it.
-}
atZero :: Reached -> Reached
atZero r = r{reachedScaling = U.map (const 0) (reachedScaling r)}

{- | Combine two cross-DB solutions: sum their inventories and concatenate
their visited-DB scalings. Associative; folding from a base preserves the
base-first layout 'csScalings' documents.
-}
instance Semigroup CrossDBSolution where
    a <> b =
        CrossDBSolution
            (M.unionWith (+) (csInventory a) (csInventory b))
            (csScalings a <> csScalings b)
            -- A union keeps what either side kept, so it is filtered only when
            -- both sides were. Solutions are merged during the solve, where
            -- both are 'IncludeLongTerm'.
            (case (csLongTerm a, csLongTerm b) of (ExcludeLongTerm, ExcludeLongTerm) -> ExcludeLongTerm; _ -> IncludeLongTerm)

{- |
Batch inventory with cross-DB back-substitution. Multi-RHS is preserved at
every level of the dependency DAG:

* Root DB: one multi-RHS solve for the K root demand vectors.
* For each dependency DB reached via 'dbCrossDBLinks', the K supplier-demand
  maps become K dense demand vectors – one multi-RHS solve on that DB.
* Recurse into the dep DB's own cross-DB links (Agribalyse → Ecoinvent, etc.).
* Sum local + all dep contributions per root by 'M.unionWith (+)'.

Returns one 'CrossDBSolution' per pid: the merged inventory plus the per-DB
scaling vectors that produced it. The regional LCIA path uses the scalings
to sum per-DB regional dot products; callers that only need the inventory
extract 'csInventory'.

Depth is capped at 10 as a safety net against pathological data (cyclic links).
-}
computeInventoryMatrixBatchWithDepsCached ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | root DB name (recorded in the head of each 'csScalings')
    Text ->
    SharedSolver ->
    [ProcessId] ->
    IO (Either Text [CrossDBSolution])
computeInventoryMatrixBatchWithDepsCached _ _ _ _ _ [] = pure (Right [])
computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db dbName solver pids =
    either
        (pure . Left)
        (\demands -> goWithDeps unitConfig depLookup db dbName solver demands 0)
        (traverse (buildDemandVector db) pids)

-- | Single-process convenience wrapper. One-element batch.
computeInventoryMatrixWithDepsCached ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | root DB name (recorded in the head of 'csScalings')
    Text ->
    SharedSolver ->
    ProcessId ->
    IO (Either Text CrossDBSolution)
computeInventoryMatrixWithDepsCached unitConfig depLookup db dbName solver pid = do
    res <- computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db dbName solver [pid]
    pure $ case res of
        Left err -> Left err
        Right (sol : _) -> Right sol
        Right [] ->
            -- unreachable: a single-pid batch always returns a singleton list.
            -- Surface as Left rather than fabricate an empty 'CrossDBSolution'
            -- with no 'csScalings' (the type now forbids it via NonEmpty).
            Left "computeInventoryMatrixWithDepsCached: empty batch result for one pid"

-- | Safety net against cyclic cross-DB links (Ginko → Agribalyse → Ginko).
maxDepsDepth :: Int
maxDepsDepth = 10

goWithDeps ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | THIS DB's name (recorded in the 'csScalings' entry contributed at this level)
    Text ->
    SharedSolver ->
    -- | K demand vectors, length = dbActivityCount db
    [Demand] ->
    -- | recursion depth
    Int ->
    IO (Either Text [CrossDBSolution])
goWithDeps unitConfig depLookup db dbName solver demands depth =
    solveMultiWithSharedSolver solver demands
        >>= either (pure . Left) (\scalings -> goWithDepsFromScalings unitConfig depLookup db dbName [] scalings depth)

{- | Propagate pre-computed root scalings into the dep-DB graph. Same body as
the dep-propagation half of 'goWithDeps' but skips the root solve – the
caller supplies the root scaling vectors (e.g. after a Sherman-Morrison
substitution update) and an optional list of synthesized 'CrossDBLink'
entries to fold into 'accumulateDepDemands' at this level only.

Extra links are applied at the root DB only; recursive calls into dep DBs
use their static 'dbCrossDBLinks'. Supporting nested substitutions would
require threading per-DB extras through 'resolveDep' – out of scope.

Each returned 'CrossDBSolution' is built bottom-up: the current DB
contributes its 'Reached' entry, then dep DBs append theirs through
'resolveDep', in the name order of 'allDepDbs', each followed by its own.
-}
goWithDepsFromScalings ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | THIS DB's name
    Text ->
    -- | virtual links to inject at this level (root only)
    [CrossDBLink] ->
    -- | K pre-computed root scaling vectors
    [Vector] ->
    -- | recursion depth
    Int ->
    IO (Either Text [CrossDBSolution])
goWithDepsFromScalings unitConfig depLookup db dbName extraLinks scalings depth = do
    let localInvs = map (applyBiosphereMatrix db) scalings
        baseSolutions =
            zipWith
                (\inv s -> CrossDBSolution inv (NE.singleton (Reached dbName db s)) IncludeLongTerm)
                localInvs
                scalings
    if depth >= maxDepsDepth
        then pure (Right baseSolutions)
        else do
            let perRootDepDemands = map (accumulateDepDemandsWith db extraLinks) scalings
                allDepDbs = depDbsOf perRootDepDemands
            if null allDepDbs
                then pure (Right baseSolutions)
                else do
                    depResults <-
                        mapConcurrently
                            (resolveDep unitConfig depLookup perRootDepDemands depth (length scalings))
                            allDepDbs
                    pure $ case sequence depResults of
                        Left err -> Left err
                        Right depSolsByDb ->
                            -- Each dep returns @[Maybe CrossDBSolution]@ of
                            -- length K. Absent-dep entries (depLookup
                            -- returned Nothing) are 'Nothing' and drop out
                            -- of the merge – an absent dep contributes
                            -- nothing to inventory or csScalings.
                            let perRootDepSols = map catMaybes (transpose depSolsByDb)
                             in Right $
                                    zipWith
                                        mergeSolutions
                                        baseSolutions
                                        perRootDepSols

{- | Merge a base 'CrossDBSolution' (this DB's own contribution) with the
list of dep-DB solutions resolved at this level. Inventories are summed
via 'M.unionWith (+)'; scaling vectors are concatenated so the final
'csScalings' lists every visited DB in the order it documents.

Exported so the substitution-aware solver ('Service.goWithSubsAndDeps')
reuses the same merge shape as the plain cross-DB solver.
-}
mergeSolutions :: CrossDBSolution -> [CrossDBSolution] -> CrossDBSolution
mergeSolutions = foldl' (<>)

-- | Every dependency DB referenced across a level's per-root demand maps.
depDbsOf :: [DepDemands] -> [Text]
depDbsOf = S.toList . S.unions . map M.keysSet

{- | Turn a level's per-root demand maps into the dep DB's per-root demand
vectors, performing unit conversion. Picks out @depDbName@'s share of each
root's demands. Shared by every dep resolver (inventory, contributions, and
the substitution-aware path in 'Service').
-}
prepareDepDemandVecs :: UnitConfig -> Text -> Database -> [DepDemands] -> Either Text [Demand]
prepareDepDemandVecs unitConfig depDbName depDb =
    traverse (depDemandsToVector unitConfig depDbName depDb . M.findWithDefault M.empty depDbName)

resolveDep ::
    UnitConfig ->
    DepSolverLookup ->
    [DepDemands] ->
    -- | current depth (for recursion)
    Int ->
    -- | K (so absent-dep returns the right number of 'Nothing' padding)
    Int ->
    Text ->
    IO (Either Text [Maybe CrossDBSolution])
resolveDep unitConfig depLookup perRootDepDemands depth k depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            -- Dep DB not loaded: contribute nothing at every root. The
            -- 'Nothing' propagates through 'mergeSolutions' and drops out
            -- before the inventory union / csScalings concat, exactly
            -- equivalent to an empty 'CrossDBSolution' but without
            -- materialising one (which the NonEmpty type forbids).
            pure (Right (replicate k Nothing))
        Just (depDb, depSolver) ->
            case prepareDepDemandVecs unitConfig depDbName depDb perRootDepDemands of
                Left err -> pure (Left err)
                Right depDemandVecs -> do
                    sols <- goWithDeps unitConfig depLookup depDb depDbName depSolver depDemandVecs (depth + 1)
                    pure $ fmap (map Just) sols

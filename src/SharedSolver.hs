{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SharedSolver
Description : The solver of a loaded database, factorised at its first solve

A database is not factorised when it loads but at the first demand made of
it, so that loading many databases costs nothing for those never asked. The
factors are then a value every request reads at once: demands are solved side
by side, on as many cores as the machine has.
-}
module SharedSolver (
    -- * Shared solver types
    SharedSolver,

    -- * Solver management
    createSharedSolver,
    Uncorrected (..),
    correctedFor,

    -- * Concurrent solving
    solveWithSharedSolver,
    solveMultiWithSharedSolver,
    getFactorization,
    refusal,

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
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Bifunctor (bimap, first)
import Data.Either (rights)
import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import EcoSpold.Common (acrossCapabilities)
import Matrix (
    Demand (..),
    DepDemands,
    Inventory,
    Vector,
    accumulateDepDemandsWith,
    applyBiosphereMatrix,
    buildDemandVector,
    countingSolve,
    depDemandsToVector,
    unsolvableText,
 )
import Matrix.Dense (LU, factorLU, solveLU)
import Matrix.Sparse (Extent (..), Factors, Unsolvable, extent, factorize, solve)
import Method.Mapping (LongTermMode (..))
import Progress
import Types
import UnitConversion (UnitConfig)

-- | The solver of one database. Its factors are computed at the first solve.
data SharedSolver = SharedSolver
    { solverFactors :: MVar (Maybe Factors)
    -- ^ Nothing until the first solve, and empty while that solve factorises
    , solverTriples :: U.Vector SparseTriple
    -- ^ The technosphere, as the database holds it
    , solverActivityCount :: Int
    , solverDbName :: Text
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

-- | Why the loaded solver was not corrected.
data Uncorrected
    = {- | The loaded matrix has no answer for a column that moved, so it says
      nothing of the changed one, which may well have answers: that takes a
      factorisation of the changed matrix.
      -}
      LoadedUnsolved
    | -- | The correction is refused, and why.
      Refused Text
    deriving (Eq, Show)

{- | The solver of the loaded matrix with these columns changed: each column
with the entries of M = I − A it adds, by row. It shares the loaded solver's
factors, so nothing is factorised again. Refused when the changed matrix is
singular.
-}
correctedFor :: SharedSolver -> [(Int, [(Int, Double)])] -> IO (Either Uncorrected SharedSolver)
correctedFor SharedSolver{solverCorrection = Just _} _ = pure (Left (Refused "a corrected solver is not corrected again"))
correctedFor base change = do
    corrections <- solveMultiWithSharedSolver base [Demand (U.accum (+) (U.replicate (solverActivityCount base) 0) entries) | (_, entries) <- change]
    pure $ case sequence corrections of
        Left _ -> Left LoadedUnsolved
        Right zs -> bimap (Refused . ("these values make the technosphere singular: " <>)) (\lu -> base{solverCorrection = Just (Correction columns zs lu)}) (factorLU (length columns) (capacitance zs))
  where
    columns :: [Int]
    columns = map fst change
    -- S, row by row.
    capacitance :: [Vector] -> Vector
    capacitance zs = U.fromList [(if i == j then 1 else 0) + z U.! c | (i, c) <- zip [0 :: Int ..] columns, (j, z) <- zip [0 :: Int ..] zs]

{- | The solver of a technosphere of so many activities. Nothing is factorised
here: the first solve does it.
-}
createSharedSolver :: Text -> U.Vector SparseTriple -> Int -> IO SharedSolver
createSharedSolver dbName techTriples activityCount = do
    reportProgress Info $ "Creating shared solver for '" ++ T.unpack dbName ++ "' (factorization deferred)"
    factors <- newMVar Nothing
    pure (SharedSolver factors techTriples activityCount dbName Nothing)

-- | The factors, if a solve has computed them yet.
getFactorization :: SharedSolver -> IO (Maybe Factors)
getFactorization = readMVar . solverFactors

{- | The factors, computed by whoever asks first. The cell is empty while they
are computed, so whoever asks meanwhile waits for them where it would have
factorised a second time, and a factorisation that is interrupted leaves the
cell as it found it.
-}
factorsOf :: SharedSolver -> IO Factors
factorsOf solver = getFactorization solver >>= maybe (modifyMVar (solverFactors solver) (fmap (\factors -> (Just factors, factors)) . maybe factorised pure)) pure
  where
    factorised :: IO Factors
    factorised = do
        factors <- withProgressTiming Solver ("Factorizing '" ++ T.unpack (solverDbName solver) ++ "' on first use") (evaluate (force (factorize (solverActivityCount solver) (solverTriples solver))))
        described (extent factors)
        pure factors
    described :: Extent -> IO ()
    described held = do
        reportProgress Solver $
            (show (solverActivityCount solver) ++ " activities, " ++ show (loopCount held) ++ " loops of which the largest holds " ++ show (largestLoop held))
                ++ (", " ++ show (factorEntries held + linkEntries held) ++ " entries kept, smallest pivot " ++ show (smallestPivot held))
        when (refusedProcesses held > 0) . reportProgress Warning $
            show (refusedProcesses held) ++ " activities of '" ++ T.unpack (solverDbName solver) ++ "' cannot be solved for: a demand whose supply chain reaches one of them is refused"
        when (smallestPivot held < trusted) . reportProgress Warning $
            "'" ++ T.unpack (solverDbName solver) ++ "' is close to having no solution: the answers that pass through its smallest pivot have lost more than six digits"
    -- A pivot starts from 1 or more: under this it has lost more than six of its sixteen digits.
    trusted :: Double
    trusted = 1e-6

-- | One demand answered from the factors, the solver's correction carried in.
answered :: SharedSolver -> Factors -> Demand -> Either Unsolvable Vector
answered solver factors (Demand demand) = corrected (solverCorrection solver) <$> solve factors demand

{- | Solve for one demand, factorising first if nobody has yet. A demand is
refused when its supply chain reaches a process that cannot be solved for.
-}
solveWithSharedSolver :: SharedSolver -> Demand -> IO (Either Unsolvable Vector)
solveWithSharedSolver solver demand = countingSolve $ do
    factors <- factorsOf solver
    evaluate (force (answered solver factors demand))

{- | Solve for several demands, shared out over the cores, answering in their
order. Each demand is answered or refused on its own: one whose supply chain
reaches a process that cannot be solved for is refused, and the others are
answered all the same.
-}
solveMultiWithSharedSolver :: SharedSolver -> [Demand] -> IO [Either Unsolvable Vector]
solveMultiWithSharedSolver solver = solveWhereAsked solver id . map Right

{- | Solve for the demands that are there, each one already refused left in
its place, and each one the solver refuses given what @why@ makes of it. One
answer per demand, in their order, whatever is refused: nothing is taken out
of the list to be put back afterwards.
-}
solveWhereAsked :: SharedSolver -> (Unsolvable -> e) -> [Either e Demand] -> IO [Either e Vector]
solveWhereAsked _ _ [] = pure []
solveWhereAsked solver why demands = countingSolve $ do
    factors <- factorsOf solver
    -- Each answer is computed where it is asked for: handed back unevaluated,
    -- they would all be computed afterwards, on the one thread that reads them.
    acrossCapabilities (either (pure . Left) (fmap (first why) . evaluate . force . answered solver factors)) demands

{- | Why a solver refused a demand, for whoever made it: the database, since a
supply chain crosses several, and the process in the way.
-}
refusal :: Database -> SharedSolver -> Unsolvable -> Text
refusal db solver why = "Database '" <> solverDbName solver <> "' has no answer to this demand: " <> unsolvableText db why

{- | Compute the scaling vector for @pid@ with the solver the server keeps for
its database, so that the factorisation is paid once for every demand.
-}
computeScalingVectorCached :: Database -> SharedSolver -> ProcessId -> IO (Either Text Vector)
computeScalingVectorCached db solver pid =
    either
        (pure . Left)
        (fmap (first (refusal db solver)) . solveWithSharedSolver solver)
        (buildDemandVector db pid)

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

Returns one 'CrossDBSolution' per pid, in their order: the merged inventory
plus the per-DB scaling vectors that produced it. The regional LCIA path uses
the scalings to sum per-DB regional dot products; callers that only need the
inventory extract 'csInventory'. A pid with no answer is refused alone, with
why: its supply chain reaches a process that cannot be solved for, or it
cannot be put to a database it reaches.

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
    IO [Either Text CrossDBSolution]
computeInventoryMatrixBatchWithDepsCached unitConfig depLookup db dbName solver pids =
    goWithDeps unitConfig depLookup db dbName solver (map (buildDemandVector db) pids) 0

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
        sol : _ -> sol
        [] ->
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
    -- | K demand vectors, length = dbActivityCount db, or why a root has none
    [Either Text Demand] ->
    -- | recursion depth
    Int ->
    IO [Either Text CrossDBSolution]
goWithDeps unitConfig depLookup db dbName solver demands depth =
    solveWhereAsked solver (refusal db solver) demands
        >>= \scalings -> goWithDepsFromScalings unitConfig depLookup db dbName [] scalings depth

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

One answer per root, in their order. A root refused here or in any database
its supply chain reaches is refused with the first reason met, its own
database's before its dependencies', and the other roots are answered.
-}
goWithDepsFromScalings ::
    UnitConfig ->
    DepSolverLookup ->
    Database ->
    -- | THIS DB's name
    Text ->
    -- | virtual links to inject at this level (root only)
    [CrossDBLink] ->
    -- | K pre-computed root scaling vectors, or why a root has none
    [Either Text Vector] ->
    -- | recursion depth
    Int ->
    IO [Either Text CrossDBSolution]
goWithDepsFromScalings unitConfig depLookup db dbName extraLinks scalings depth = do
    let baseSolutions = map (fmap (\s -> CrossDBSolution (applyBiosphereMatrix db s) (NE.singleton (Reached dbName db s)) IncludeLongTerm)) scalings
    if depth >= maxDepsDepth
        then pure baseSolutions
        else do
            let perRootDepDemands = map (fmap (accumulateDepDemandsWith db extraLinks)) scalings
                allDepDbs = depDbsOf (rights perRootDepDemands)
            if null allDepDbs
                then pure baseSolutions
                else do
                    -- Each dep answers every root, refused ones included, in
                    -- the roots' order, so a column of the transpose is one
                    -- root's. An absent dep answers 'Nothing', which drops out
                    -- of the merge: it adds nothing to inventory or csScalings.
                    depSolsByDb <- mapConcurrently (resolveDep unitConfig depLookup perRootDepDemands depth) allDepDbs
                    pure (zipWith withDeps baseSolutions (transpose depSolsByDb))
  where
    withDeps :: Either Text CrossDBSolution -> [Either Text (Maybe CrossDBSolution)] -> Either Text CrossDBSolution
    withDeps base deps = mergeSolutions <$> base <*> (catMaybes <$> sequence deps)

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
prepareDepDemandVecs :: UnitConfig -> Text -> Database -> [Either Text DepDemands] -> [Either Text Demand]
prepareDepDemandVecs unitConfig depDbName depDb =
    map (>>= depDemandsToVector unitConfig depDbName depDb . M.findWithDefault M.empty depDbName)

resolveDep ::
    UnitConfig ->
    DepSolverLookup ->
    [Either Text DepDemands] ->
    -- | current depth (for recursion)
    Int ->
    Text ->
    IO [Either Text (Maybe CrossDBSolution)]
resolveDep unitConfig depLookup perRootDepDemands depth depDbName = do
    depM <- depLookup depDbName
    case depM of
        Nothing ->
            -- Dep DB not loaded: contribute nothing at every root. The
            -- 'Nothing' propagates through 'mergeSolutions' and drops out
            -- before the inventory union / csScalings concat, exactly
            -- equivalent to an empty 'CrossDBSolution' but without
            -- materialising one (which the NonEmpty type forbids).
            pure (map (const (Right Nothing)) perRootDepDemands)
        Just (depDb, depSolver) ->
            map (fmap Just)
                <$> goWithDeps unitConfig depLookup depDb depDbName depSolver (prepareDepDemandVecs unitConfig depDbName depDb perRootDepDemands) (depth + 1)

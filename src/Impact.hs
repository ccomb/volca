{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Scoring one method against one solved inventory, and saying which flows
and which activities made the score.

A method carrying regional characterization factors is scored from the
per-database scaling vectors rather than from the merged inventory, because a
factor that depends on where a flow occurs cannot be applied to a total that
has forgotten. Which of the two paths a method takes is a property of the
method, not of who is asking, so the choice belongs below every surface rather
than inside one of them.

The score and the flows that make it leave by the same path, which is what
keeps the shares summing to the total: a score taken from the dot product over
per-column weights and shares taken from a region-blind walk over the merged
inventory published percentages that added up to anything but a hundred.

Everything that publishes a score comes through here: the REST impact routes,
the two contributing endpoints and the assistant tools. A list by flow and a
list by activity are the same sum folded on its two axes, so they leave by the
same path as the score and add up to it.

The long-term policy travels with the solution rather than beside it. Dropping
the delayed emissions from an inventory cannot reach a path that reads columns,
a column not being a flow, so the regionalized path reads the policy and the
per-column sum those emissions were left out of. 'withLongTermPolicy' is the
one place that records it, so a filtered inventory and a policy saying otherwise
cannot be built.

Whether a method is scored that way is asked of every database the solution
reads, not of the root alone. A database's tables answer whether this method's
regional factors reached the flows /that/ database holds, and the root's
answered for all of them only as long as its flow closure kept reaching its
dependencies'. A root blind to them would otherwise score a dependency's
located emissions with one world factor, or with none.
-}
module Impact (
    scoreSolution,
    contributionsOf,
    processContributionsOf,
    withLongTermPolicy,
    LicencedSolution (..),
    WithheldPart (..),
    PartScore (..),
    partitionByLicence,
    licencedSolution,
    licencedCutoffs,
    solutionCutoffs,
    scoreParts,
    LicencedContributions (..),
    licencedContributionsOf,
    withheldDatabases,
    inventoryRefusal,
    ProcessParts (..),
    splitProcessParts,
    unknownInventoryFlows,
    warnUnknownFlowIds,
) where

import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import qualified Data.Vector.Unboxed as U
import Database.Cutoffs (Cutoffs (..), cutoffsReached, indexesOf)
import Database.Manager (CollectionName, DatabaseManager (..), getGapIndex, getMergedFlowMetadata, getMergedUnitConfig, mapMethodToTablesCached, refusingDatabases)
import Matrix (Inventory, Vector, applyBiosphereMatrix)
import Method.Mapping (
    FlowContribution (..),
    LCIAOutcome (..),
    LongTermMode (..),
    MethodTables (..),
    applyLongTermMode,
    computeLCIAScoreFromTables,
    inventoryContributions,
    processContributionsFromTables,
    regionalizedContributionsCrossDB,
    regionalizedProcessContributions,
    sumRegionalizedLCIAScoreCrossDB,
 )
import Method.Types (Method (..))
import Progress (ProgressLevel (..), reportProgress)
import qualified SharedSolver
import Types (BioFlowDB, Database, Permission (..), ProcessId, includedSentence)

{- | Record the long-term emission policy on a solution: drop the delayed
emissions from its inventory and say so, in one move.

Both halves are needed because the two scoring paths read different things. A
flat score reads the inventory and wants them gone; a regionalized score reads
the per-database scaling vectors, where they cannot be taken out, and wants to
be told. Setting one without the other is what let @exclude_long_term@ be
honoured by one path and ignored by the other.
-}
withLongTermPolicy :: DatabaseManager -> LongTermMode -> SharedSolver.CrossDBSolution -> IO SharedSolver.CrossDBSolution
withLongTermPolicy _ IncludeLongTerm sol = pure sol
withLongTermPolicy dbManager ExcludeLongTerm sol = do
    (mFlows, _) <- getMergedFlowMetadata dbManager
    pure
        sol
            { SharedSolver.csInventory = applyLongTermMode mFlows ExcludeLongTerm (SharedSolver.csInventory sol)
            , SharedSolver.csLongTerm = ExcludeLongTerm
            }

{- | A solution split by what the licences of the databases it reads let a
reader see in detail: the part shown, and one part per dependency that keeps
its detail to itself, each to be answered as a single line.

The root is never one of them: what the requested database refuses is refused
before anything is solved, so a root revisited through a cycle stays shown.
-}
data LicencedSolution = LicencedSolution
    { lsWhole :: SharedSolver.CrossDBSolution
    -- ^ What the score is read on: the shown part and the withheld ones add up to it.
    , lsShown :: SharedSolver.CrossDBSolution
    , lsWithheld :: [WithheldPart]
    }

-- | One dependency's share of a solution, alone.
data WithheldPart = WithheldPart
    { wpDatabase :: Text
    , wpSolution :: SharedSolver.CrossDBSolution
    }

{- | The cut-off inputs a solution meets, with the indexes the manager keeps:
named in the part the licences show, one count for each dependency that keeps
its detail, since the products it misses are part of that detail. A database
whose licence keeps the amounts of its exchanges to itself (it refuses
ReadInventory), the root's own included, is counted rather than named too: an
unsupplied input and what the chain asks of it are such amounts, as the
unlinked waste a batch drops under that licence is.
-}
licencedCutoffs :: DatabaseManager -> LicencedSolution -> IO Cutoffs
licencedCutoffs dbManager ls = do
    amountsKept <- refusingDatabases dbManager ReadInventory
    indexOf <- indexesOf (getGapIndex dbManager) scalings
    pure (cutoffsReached indexOf (S.fromList (map wpDatabase (lsWithheld ls)) <> amountsKept) scalings)
  where
    scalings :: [(Text, Database, Vector)]
    scalings = NE.toList (SharedSolver.csScalings (lsWhole ls))

-- | The cut-offs of a solution, read under the licences for one permission.
solutionCutoffs :: DatabaseManager -> Permission -> SharedSolver.CrossDBSolution -> IO Cutoffs
solutionCutoffs dbManager permission sol = licencedCutoffs dbManager =<< licencedSolution dbManager permission sol

{- | Split a solution by the databases whose licence refuses a permission.

A part keeps every database of the solution and zeroes the vectors of the
others, rather than dropping them: whether a method is scored regionalized is
asked of the databases a solution lists, and a dependency dropped from the
list would switch the others to the flat path, scored with other tables, and
the parts would no longer add up to the score. A database listed twice (two
links reaching it, or a cycle) moves as one, its parts summed.

The inventory of a part is rebuilt the way the solver builds it, each
database's biosphere times its vector, then the long-term policy again. When
no dependency keeps anything the solution is returned as it is.
-}
partitionByLicence :: BioFlowDB -> S.Set Text -> SharedSolver.CrossDBSolution -> LicencedSolution
partitionByLicence flowDB refusing sol
    | null withheld = LicencedSolution{lsWhole = sol, lsShown = sol, lsWithheld = []}
    | otherwise =
        LicencedSolution
            { lsWhole = sol
            , lsShown = keeping (`notElem` withheld)
            , lsWithheld = [WithheldPart{wpDatabase = name, wpSolution = keeping (== name)} | name <- withheld]
            }
  where
    scalings :: NE.NonEmpty (Text, Database, Vector)
    scalings = SharedSolver.csScalings sol

    withheld :: [Text]
    withheld = withheldNames refusing sol

    keeping :: (Text -> Bool) -> SharedSolver.CrossDBSolution
    keeping kept =
        let scalings' = fmap (\(name, db, sv) -> (name, db, if kept name then sv else U.map (const 0) sv)) scalings
         in sol
                { SharedSolver.csScalings = scalings'
                , SharedSolver.csInventory =
                    applyLongTermMode flowDB (SharedSolver.csLongTerm sol) $
                        M.unionsWith (+) [applyBiosphereMatrix db sv | (name, db, sv) <- NE.toList scalings, kept name]
                }

-- | The dependencies of a solution among the refusing databases, each once.
withheldNames :: S.Set Text -> SharedSolver.CrossDBSolution -> [Text]
withheldNames refusing sol =
    nubOrd [name | (name, _, _) <- NE.toList scalings, name /= root, name `S.member` refusing]
  where
    scalings :: NE.NonEmpty (Text, Database, Vector)
    scalings = SharedSolver.csScalings sol

    root :: Text
    root = let (name, _, _) = NE.head scalings in name

-- | 'partitionByLicence' under the licences the engine serves.
licencedSolution :: DatabaseManager -> Permission -> SharedSolver.CrossDBSolution -> IO LicencedSolution
licencedSolution dbManager permission sol = do
    (mFlows, _) <- getMergedFlowMetadata dbManager
    refusing <- refusingDatabases dbManager permission
    pure (partitionByLicence mFlows refusing sol)

-- | 'withheldNames' under the licences the engine serves.
withheldDatabases :: DatabaseManager -> Permission -> SharedSolver.CrossDBSolution -> IO [Text]
withheldDatabases dbManager permission sol = (`withheldNames` sol) <$> refusingDatabases dbManager permission

{- | Why the aggregated inventory of a solution is not answered, when it
sums the exchanges of a dependency whose licence keeps their amounts. A line
for that dependency could carry no amount, and the inventory without its part
would read as a total.
-}
inventoryRefusal :: DatabaseManager -> SharedSolver.CrossDBSolution -> IO (Maybe Text)
inventoryRefusal dbManager sol = fmap (includedSentence root) . listToMaybe <$> withheldDatabases dbManager ReadInventory sol
  where
    root :: Text
    root = let (name, _, _) = NE.head (SharedSolver.csScalings sol) in name

-- | What one database that keeps its detail adds to a score, in one number.
data PartScore = PartScore
    { psDatabase :: Text
    , psScore :: Double
    }

-- | Each withheld part's score, by the database that keeps it.
scoreParts ::
    DatabaseManager ->
    CollectionName ->
    Method ->
    MethodTables ->
    [WithheldPart] ->
    IO (Either Text [PartScore])
scoreParts dbManager collection method tables parts =
    sequence <$> traverse (\p -> fmap (PartScore (wpDatabase p)) <$> scoreSolution dbManager collection method tables (wpSolution p)) parts

{- | The flows of a score as the licences of its databases let them be read:
those of the part shown, with the flow UUIDs the merged metadata has no record
of, and one score per dependency that keeps what weighs in its scores. The
rows and those scores add up to the score.
-}
data LicencedContributions = LicencedContributions
    { lcRows :: [FlowContribution]
    , lcUnknown :: [UUID]
    , lcWithheld :: [PartScore]
    }

licencedContributionsOf ::
    DatabaseManager ->
    CollectionName ->
    Method ->
    MethodTables ->
    LicencedSolution ->
    IO (Either Text LicencedContributions)
licencedContributionsOf dbManager collection method tables LicencedSolution{lsShown = shown, lsWithheld = parts} = do
    rowsE <- contributionsOf dbManager collection method tables shown
    partsE <- scoreParts dbManager collection method tables parts
    pure (LicencedContributions <$> fmap fst rowsE <*> fmap snd rowsE <*> partsE)

{- | Every process's part of a score, split by the databases that keep theirs:
the parts shown, and each withheld database's parts summed into one. They add
up to the score.
-}
data ProcessParts = ProcessParts
    { ppShown :: M.Map (Text, ProcessId) Double
    , ppWithheld :: [PartScore]
    }

-- | A process id names its database, so nothing is solved again.
splitProcessParts :: [Text] -> M.Map (Text, ProcessId) Double -> ProcessParts
splitProcessParts names contributions =
    ProcessParts
        { ppShown = M.filterWithKey (\(name, _) _ -> name `notElem` names) contributions
        , ppWithheld = [PartScore name (sum [c | ((n, _), c) <- M.toList contributions, n == name]) | name <- names]
        }

{- | The score of one method against a cross-database solution.

A 'Left' is a scoring integrity error – a regionalized method with a gap it
cannot fill. It propagates rather than collapsing to a zero the consumer could
not tell from a real score.
-}
scoreSolution ::
    DatabaseManager ->
    -- | Reaches each dependency database's tables
    CollectionName ->
    Method ->
    MethodTables ->
    SharedSolver.CrossDBSolution ->
    IO (Either Text Double)
scoreSolution dbManager collection method tables sol = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- getMergedFlowMetadata dbManager
    perDb <- perDatabaseTables dbManager collection method sol
    label method $
        if anyRegionalized perDb
            then traverse evaluate (sumRegionalizedLCIAScoreCrossDB unitCfg mUnits mFlows ltMode (dmLocationHierarchy dbManager) (map unnamed perDb))
            else Right <$> evaluate (loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows (SharedSolver.csInventory sol) tables))
  where
    ltMode :: LongTermMode
    ltMode = SharedSolver.csLongTerm sol

{- | The flows that made that score, each with what it contributed and the
factor the score applied to it.

Same dispatch as 'scoreSolution', and that is the point: the rows sum to the
score because they are the same products, added in another order.

Returns what 'Method.Mapping.inventoryContributions' returns – the rows, and
the flow UUIDs the merged metadata has no record of, which the caller
surfaces.
-}
contributionsOf ::
    DatabaseManager ->
    CollectionName ->
    Method ->
    MethodTables ->
    SharedSolver.CrossDBSolution ->
    IO (Either Text ([FlowContribution], [UUID]))
contributionsOf dbManager collection method tables sol = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- getMergedFlowMetadata dbManager
    perDb <- perDatabaseTables dbManager collection method sol
    label method $
        if anyRegionalized perDb
            then pure (regionalizedContributionsCrossDB unitCfg mUnits mFlows ltMode (map unnamed perDb))
            else pure (Right (inventoryContributions unitCfg mUnits mFlows (SharedSolver.csInventory sol) tables))
  where
    ltMode :: LongTermMode
    ltMode = SharedSolver.csLongTerm sol

{- | Whether any database of the solution carries factors that depend on where
a flow occurs. One that carries none is not evidence that the method has none:
it is evidence about that database's flows.
-}
anyRegionalized :: [(Text, Database, Vector, MethodTables)] -> Bool
anyRegionalized = any (\(_, _, _, tables) -> not (M.null (mtRegionalizedCF tables)))

{- | The activities that made that score, each with what it contributed.

The same sum as 'contributionsOf', folded on its other axis: a regionalized
score is @Σ_a s[a] · w[a]@ over activity columns, and one column is one process,
so the term is what that process contributed and there is nothing to walk. A
database whose tables caught none of the method's located factors is read flat
over its own slice, as the score reads it.

Same dispatch as the other two, and for the same reason: a method no database
locates is scored from the merged inventory with the root's tables, so its
activities are read from each slice with those same tables. Read with each
database's own, they would answer a total the impact routes do not publish,
because a root's tables and a dependency's need not agree on a dependency flow.

A process id is local to its database, so the key names both: the same id in
two databases is two processes.
-}
processContributionsOf ::
    DatabaseManager ->
    CollectionName ->
    Method ->
    -- | The root's tables, which a method no database locates is scored from
    MethodTables ->
    SharedSolver.CrossDBSolution ->
    IO (Either Text (M.Map (Text, ProcessId) Double))
processContributionsOf dbManager collection method tables sol = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- getMergedFlowMetadata dbManager
    perDb <- perDatabaseTables dbManager collection method sol
    let ltMode = SharedSolver.csLongTerm sol
        regional (name, db, sv, dbTables) =
            M.mapKeys (name,)
                <$> regionalizedProcessContributions unitCfg mUnits mFlows ltMode db sv dbTables
        flat (name, db, sv, _) =
            M.mapKeys (name,) $
                processContributionsFromTables unitCfg mUnits mFlows ltMode db sv tables
    label method $
        pure $
            if anyRegionalized perDb
                then M.unionsWith (+) <$> traverse regional perDb
                else Right (M.unionsWith (+) (map flat perDb))

{- | The flows of an inventory the merged metadata has no record of.

A flow nothing describes is a flow nothing can characterize, and a score that
quietly leaves it out is a score nobody can tell from a complete one. Read from
the inventory rather than from the rows a score produced: those name what was
characterized, which is the other question.
-}
unknownInventoryFlows :: BioFlowDB -> Inventory -> [UUID]
unknownInventoryFlows mFlows inventory =
    [fid | (fid, qty) <- M.toList inventory, qty /= 0, not (M.member fid mFlows)]

-- | Say so, naming the surface that asked.
warnUnknownFlowIds :: Text -> [UUID] -> IO ()
warnUnknownFlowIds surface unknown =
    unless (null unknown) $
        reportProgress Warning $
            "["
                <> T.unpack surface
                <> "] "
                <> show (length unknown)
                <> " inventory flow UUID(s) absent from merged FlowDB – characterization incomplete. Samples: "
                <> show (take 3 unknown)

{- | Each database of the solution, named, with this method's tables built
against it. Named because a process id means nothing without the database it
belongs to; the scoring calls drop it with 'unnamed'.
-}
perDatabaseTables ::
    DatabaseManager ->
    CollectionName ->
    Method ->
    SharedSolver.CrossDBSolution ->
    IO [(Text, Database, Vector, MethodTables)]
perDatabaseTables dbManager collection method sol =
    forM (NE.toList (SharedSolver.csScalings sol)) $ \(n, d, sv) -> do
        tbls <- mapMethodToTablesCached dbManager n collection d method
        pure (n, d, sv, tbls)

-- | The three a per-database score takes, the name dropped.
unnamed :: (Text, Database, Vector, MethodTables) -> (Database, Vector, MethodTables)
unnamed (_, db, sv, tables) = (db, sv, tables)

-- | Name the method in front of whatever went wrong, once for both paths.
label :: Method -> IO (Either Text a) -> IO (Either Text a)
label method = fmap (first (("[LCIA " <> methodName method <> "] ") <>))

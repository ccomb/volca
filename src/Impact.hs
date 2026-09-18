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
    unknownInventoryFlows,
    warnUnknownFlowIds,
) where

import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Database.Manager (CollectionName, DatabaseManager (..), getMergedFlowMetadata, getMergedUnitConfig, mapMethodToTablesCached)
import Matrix (Inventory, Vector)
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
import Types (BioFlowDB, Database, ProcessId)

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

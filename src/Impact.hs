{-# LANGUAGE OverloadedStrings #-}

{- | Scoring one method against one solved inventory, and saying which flows
made the score.

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

It is not yet reached from below every surface. The REST impact routes come
through here; the assistant tools score regionalized methods with the flat
path, and so do both contributing-flows endpoints and both
contributing-activities endpoints. Routing them here is now a matter of
calling these two functions, the walk they needed being 'contributionsOf'.

One further gap, older than this module and not fixed by it: long-term-emission
filtering applies to the inventory only, so the regionalized path ignores
@exclude_long_term@. A database whose regional factors all arrive from a
dependency now takes that path, so it stops honouring the flag – which is what
the dependency it mirrors already did.

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
) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)

import Database.Manager (CollectionName, DatabaseManager (..), getMergedFlowMetadata, getMergedUnitConfig, mapMethodToTablesCached)
import Matrix (Inventory, Vector)
import Method.Mapping (
    FlowContribution (..),
    LCIAOutcome (..),
    MethodTables (..),
    computeLCIAScoreFromTables,
    inventoryContributions,
    regionalizedContributionsCrossDB,
    sumRegionalizedLCIAScoreCrossDB,
 )
import Method.Types (Method (..))
import qualified SharedSolver
import Types (Database)

{- | The score of one method against a cross-database solution.

@inventory@ is passed separately from @sol@ because a caller may have filtered
it (long-term emissions) after solving. The regionalized path reads the
solution's scaling vectors instead, so that filtering does not reach it – see
the note above.

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
    -- | The inventory to score, after any filtering the caller applied
    Inventory ->
    IO (Either Text Double)
scoreSolution dbManager collection method tables sol inventory = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- getMergedFlowMetadata dbManager
    perDb <- perDatabaseTables dbManager collection method sol
    label method $
        if anyRegionalized perDb
            then traverse evaluate (sumRegionalizedLCIAScoreCrossDB unitCfg mUnits mFlows (dmLocationHierarchy dbManager) perDb)
            else Right <$> evaluate (loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables))

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
    -- | The inventory to read, after any filtering the caller applied
    Inventory ->
    IO (Either Text ([FlowContribution], [UUID]))
contributionsOf dbManager collection method tables sol inventory = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- getMergedFlowMetadata dbManager
    perDb <- perDatabaseTables dbManager collection method sol
    label method $
        if anyRegionalized perDb
            then pure (regionalizedContributionsCrossDB unitCfg mUnits mFlows perDb)
            else pure (Right (inventoryContributions unitCfg mUnits mFlows inventory tables))

{- | Whether any database of the solution carries factors that depend on where
a flow occurs. One that carries none is not evidence that the method has none:
it is evidence about that database's flows.
-}
anyRegionalized :: [(Database, Vector, MethodTables)] -> Bool
anyRegionalized = any (\(_, _, tables) -> not (M.null (mtRegionalizedCF tables)))

-- | Each database of the solution with this method's tables built against it.
perDatabaseTables ::
    DatabaseManager ->
    CollectionName ->
    Method ->
    SharedSolver.CrossDBSolution ->
    IO [(Database, Vector, MethodTables)]
perDatabaseTables dbManager collection method sol =
    forM (NE.toList (SharedSolver.csScalings sol)) $ \(n, d, sv) -> do
        tbls <- mapMethodToTablesCached dbManager n collection d method
        pure (d, sv, tbls)

-- | Name the method in front of whatever went wrong, once for both paths.
label :: Method -> IO (Either Text a) -> IO (Either Text a)
label method = fmap (first (("[LCIA " <> methodName method <> "] ") <>))

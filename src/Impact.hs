{-# LANGUAGE OverloadedStrings #-}

{- | Scoring one method against one solved inventory.

A method carrying regional characterization factors is scored from the
per-database scaling vectors rather than from the merged inventory, because a
factor that depends on where a flow occurs cannot be applied to a total that
has forgotten. Which of the two paths a method takes is a property of the
method, not of who is asking, so the choice belongs below every surface rather
than inside one of them.

It is not yet reached from below every surface. The REST impact routes come
through here; the assistant tools score regionalized methods with the flat
path, and so do both contributing-flows endpoints and both
contributing-activities endpoints. Routing them here is not a matter of
calling this function: the per-flow contributions each reports are computed
region-blind by 'Method.Mapping.inventoryContributions', so a surface that
took its total from here and its shares from there would publish percentages
that no longer sum. The contributions have to move with the total, and that
walk does not exist yet.

One further gap, older than this module and not fixed by it: long-term-emission
filtering applies to the inventory only, so the regionalized path ignores
@exclude_long_term@. A database whose regional factors all arrive from a
dependency now takes that path, so it stops honouring the flag – which is what
the dependency it mirrors already did.

The dispatch below still asks the /root/ database's tables whether the method is
regionalized. That reads right now only because those tables are built over the
root's flow closure and so carry its dependencies' regional factors; it is a
property of the method, and asking one database's tables for it is incidental.
-}
module Impact (
    scoreSolution,
) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Database.Manager (CollectionName, DatabaseManager (..), getMergedFlowMetadata, getMergedUnitConfig, mapMethodToTablesCached)
import Matrix (Inventory)
import Method.Mapping (
    LCIAOutcome (..),
    MethodTables (..),
    computeLCIAScoreFromTables,
    sumRegionalizedLCIAScoreCrossDB,
 )
import Method.Types (Method (..))
import qualified SharedSolver

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
    fmap (first (("[LCIA " <> methodName method <> "] ") <>)) $
        if M.null (mtRegionalizedCF tables)
            then Right <$> evaluate (loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables))
            else do
                let hier = dmLocationHierarchy dbManager
                perDb <-
                    forM (NE.toList (SharedSolver.csScalings sol)) $ \(n, d, sv) -> do
                        tbls <- mapMethodToTablesCached dbManager n collection d method
                        pure (d, sv, tbls)
                traverse evaluate (sumRegionalizedLCIAScoreCrossDB unitCfg mUnits mFlows hier perDb)

{-# LANGUAGE OverloadedStrings #-}

module Tree (buildLoopAwareTree, childTarget) where

import Control.Monad.Trans.State.Strict (State, evalState, get, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Database.MatrixBuild (linkedProducer)
import Numeric.Natural (Natural)
import Types
import UnitConversion (UnitConfig, convertExchangeAmount)

isTechnosphereInput :: Exchange -> Bool
isTechnosphereInput ex =
    case ex of
        TechnosphereExchange{techRole = Input} -> True
        TechnosphereExchange{} -> False
        BiosphereExchange{} -> False
        WasteExchange{} -> False -- waste flows aren't upstream tech inputs in the tree-builder sense

{- | Get converted exchange amount ensuring unit compatibility.
Converts the exchange amount into the unit the edge carrying it declares, for
proper scaling. Either side reading "unknown" leaves the amount as the exchange
states it, there being nothing to convert between.
-}
getConvertedExchangeAmount :: UnitConfig -> Database -> Exchange -> Text -> Double
getConvertedExchangeAmount unitCfg db exchange targetUnit =
    let originalAmount = exchangeAmount exchange
        exchangeUnitName = maybe "unknown" unitName (M.lookup (exchangeUnitId exchange) (dbUnits db))
     in if exchangeUnitName == "unknown" || targetUnit == "unknown"
            then originalAmount
            else convertExchangeAmount unitCfg exchangeUnitName targetUnit originalAmount

{- | Read-only context threaded through tree construction. @tcMaxDepth@ is a
'Natural' so negative depths are unrepresentable; the public entrypoint
clamps its 'Int' argument at the boundary.
-}
data TreeConfig = TreeConfig
    { tcUnitConfig :: !UnitConfig
    , tcDatabase :: !Database
    , tcMaxDepth :: !Natural
    }

{- | What one technosphere input points at: the row that supplies it, or a
declared activity link no row satisfies. An input declaring no link at all is
'Nothing' - the SimaPro shape, resolved by product flow elsewhere, which this
traversal has always skipped.
-}
type ChildTarget = Either UUID (ProcessId, Activity)

childTarget :: Database -> Exchange -> Maybe ChildTarget
childTarget db ex =
    case linkedProducer db ex >>= \pid -> (,) pid <$> getActivity db pid of
        Just row -> Just (Right row)
        Nothing -> Left <$> exchangeActivityLinkId ex

{- | The unit the amount on the edge to this child is stated in: the row's own
reference unit when there is a row, and otherwise the flow's, which is what the
edge is labelled with either way. A missing target with no unit of its own would
leave a number in the exchange's unit under a label naming another.
-}
childUnit :: Database -> TechnosphereFlow -> ChildTarget -> Text
childUnit db flow = either (const (getUnitNameForTechFlow (dbUnits db) flow)) (activityUnit . snd)

{- | Build loop-aware tree for SVG export with maximum depth and a fixed
per-tree node budget (300) to keep export latency bounded. Negative
@maxDepth@ arguments are clamped to 0. The root arrives as the row it sits at
and the activity that row holds, so a root outside the database is
unrepresentable rather than reported as a missing node.
-}
buildLoopAwareTree :: UnitConfig -> Database -> Int -> (ProcessId, Activity) -> LoopAwareTree
buildLoopAwareTree unitCfg db maxDepth (rootPid, rootActivity) =
    let cfg = TreeConfig unitCfg db (fromIntegral (max 0 maxDepth))
        maxNodes = 300
     in evalState (buildNode cfg rootPid rootActivity S.empty 0) maxNodes

{- | Recursive tree builder running in 'State Int' for the node budget.
Every visit that produces a node (loop, leaf, missing, or branch)
decrements the budget by one; when the budget hits zero further
exploration is cut off and a truncation leaf is emitted.
-}
buildNode :: TreeConfig -> ProcessId -> Activity -> S.Set ProcessId -> Int -> State Int LoopAwareTree
buildNode cfg pid activity visited depth = do
    budget <- get
    if budget <= 0
        then pure (TreeLoop pid (activityName activity) depth)
        else do
            modify (subtract 1)
            if fromIntegral depth >= tcMaxDepth cfg || pid `S.member` visited
                then pure (TreeLoop pid (activityName activity) depth)
                else do
                    let visited' = S.insert pid visited
                        techInputs = filter isTechnosphereInput (exchanges activity)
                    children <- buildChildren cfg techInputs visited' (depth + 1)
                    pure $
                        if null children
                            then TreeLeaf pid activity
                            else TreeNode pid activity children

{- | Fold over technosphere input exchanges, pairing each linked child
with its converted amount and recursing. Bails out early when the node
budget runs out so the tree stays within the 300-node envelope.
-}
buildChildren :: TreeConfig -> [Exchange] -> S.Set ProcessId -> Int -> State Int [TreeChild]
buildChildren _ [] _ _ = pure []
buildChildren cfg (ex : exs) visited depth = do
    budget <- get
    if budget <= 0
        then pure []
        else case (childTarget db ex, M.lookup (exchangeFlowId ex) (dbTechFlows db)) of
            (Just target, Just flow) -> do
                let amount = getConvertedExchangeAmount (tcUnitConfig cfg) db ex (childUnit db flow target)
                subtree <- buildTarget cfg target visited depth
                rest <- buildChildren cfg exs visited depth
                pure (TreeChild{childAmount = amount, childFlow = flow, childSubtree = subtree} : rest)
            _ -> buildChildren cfg exs visited depth
  where
    db = tcDatabase cfg

buildTarget :: TreeConfig -> ChildTarget -> S.Set ProcessId -> Int -> State Int LoopAwareTree
buildTarget cfg target visited depth = case target of
    Right (pid, activity) -> buildNode cfg pid activity visited depth
    Left uuid -> do
        modify (subtract 1)
        pure (TreeMissing uuid "Missing Activity" depth)

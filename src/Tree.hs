{-# LANGUAGE OverloadedStrings #-}

module Tree (buildLoopAwareTree) where

import Control.Monad.Trans.State.Strict (State, evalState, get, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric.Natural (Natural)
import Types
import UnitConversion (UnitConfig, convertExchangeAmount)

isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput _ ex =
    case ex of
        TechnosphereExchange _ _ _ isInput isRef _ _ _ -> isInput && not isRef
        BiosphereExchange _ _ _ _ _ -> False

{- | Get converted exchange amount ensuring unit compatibility.
Converts exchange amount to the target activity's reference unit for proper scaling.
-}
getConvertedExchangeAmount :: UnitConfig -> Database -> Exchange -> UUID -> Double
getConvertedExchangeAmount unitCfg db exchange targetActivityUUID =
    let originalAmount = exchangeAmount exchange
        exchangeUnitName = case M.lookup (exchangeUnitId exchange) (dbUnits db) of
            Just unit -> unitName unit
            Nothing -> "unknown"
        targetReferenceUnit = case findActivityByActivityUUID db targetActivityUUID of
            Just targetActivity -> activityUnit targetActivity
            Nothing -> "unknown"
     in if exchangeUnitName == "unknown" || targetReferenceUnit == "unknown"
            then originalAmount
            else convertExchangeAmount unitCfg exchangeUnitName targetReferenceUnit originalAmount

{- | Read-only context threaded through tree construction. @tcMaxDepth@ is a
'Natural' so negative depths are unrepresentable; the public entrypoint
clamps its 'Int' argument at the boundary.
-}
data TreeConfig = TreeConfig
    { tcUnitConfig :: !UnitConfig
    , tcDatabase :: !Database
    , tcMaxDepth :: !Natural
    }

{- | Build loop-aware tree for SVG export with maximum depth and a fixed
per-tree node budget (300) to keep export latency bounded. Negative
@maxDepth@ arguments are clamped to 0.
-}
buildLoopAwareTree :: UnitConfig -> Database -> UUID -> Int -> LoopAwareTree
buildLoopAwareTree unitCfg db rootUUID maxDepth =
    let cfg = TreeConfig unitCfg db (fromIntegral (max 0 maxDepth))
        maxNodes = 300
     in evalState (buildNode cfg rootUUID S.empty 0) maxNodes

{- | Recursive tree builder running in 'State Int' for the node budget.
Every visit that produces a node (loop, leaf, missing, or branch)
decrements the budget by one; when the budget hits zero further
exploration is cut off and a truncation leaf is emitted.
-}
buildNode :: TreeConfig -> UUID -> S.Set UUID -> Int -> State Int LoopAwareTree
buildNode cfg activityUUID visited depth = do
    budget <- get
    let mActivity = findActivityByActivityUUID (tcDatabase cfg) activityUUID
        name = maybe "Missing Activity" activityName mActivity
    if budget <= 0
        then pure (TreeLoop activityUUID name depth)
        else do
            modify (subtract 1)
            if fromIntegral depth >= tcMaxDepth cfg || activityUUID `S.member` visited
                then pure (TreeLoop activityUUID name depth)
                else case mActivity of
                    Nothing -> pure (TreeLoop activityUUID "Missing Activity" depth)
                    Just activity -> do
                        let visited' = S.insert activityUUID visited
                            techInputs =
                                [ ex
                                | ex <- exchanges activity
                                , isTechnosphereInput (dbFlows (tcDatabase cfg)) ex
                                ]
                        children <- buildChildren cfg techInputs visited' (depth + 1)
                        pure $
                            if null children
                                then TreeLeaf activity
                                else TreeNode activity children

{- | Fold over technosphere input exchanges, pairing each resolvable child
with its converted amount and recursing. Bails out early when the node
budget runs out so the tree stays within the 300-node envelope.
-}
buildChildren :: TreeConfig -> [Exchange] -> S.Set UUID -> Int -> State Int [(Double, Flow, LoopAwareTree)]
buildChildren _ [] _ _ = pure []
buildChildren cfg (ex : exs) visited depth = do
    budget <- get
    if budget <= 0
        then pure []
        else case (exchangeActivityLinkId ex, M.lookup (exchangeFlowId ex) (dbFlows (tcDatabase cfg))) of
            (Just targetUUID, Just flow) -> do
                let amount = getConvertedExchangeAmount (tcUnitConfig cfg) (tcDatabase cfg) ex targetUUID
                subtree <- buildNode cfg targetUUID visited depth
                rest <- buildChildren cfg exs visited depth
                pure ((amount, flow, subtree) : rest)
            _ -> buildChildren cfg exs visited depth

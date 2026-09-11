{- | Characterization-coverage report, for the people who maintain a database
against a given LCIA method collection.

A score tells you a flow is characterized; it does not tell you /how/ it was
reached. VoLCA's matcher bridges a name difference — it will score a flow named
@"Methane, bromo-, Halon 1001"@ off a factor the method lists under
@"Bromomethane"@, because a synonym or CAS number links the two. A tool that
matches factors by their exact name has no such bridge: it scores that flow as
zero, silently.

This report surfaces exactly those flows: the ones a method scores /only/
through a name bridge, grouped by the factor they bridge to (the name the
method itself uses, i.e. the rename target). It is the coverage a raw score
hides, seen from the side of an exact-name consumer.

The heavy lifting — cascading a factor to a flow and recording which strategy
won — already happens in 'Method.Mapping'. This module is a pure fold over that
result: partition the reached flows into exact-name and bridge-only, keep the
bridge-only ones, group them.
-}
module Method.Coverage (
    CoverageReport (..),
    CollectionBridges (..),
    BridgeGroup (..),
    BridgedFlow (..),
    collectionBridges,
) where

import Data.Containers.ListUtils (nubOrdOn)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)

import Method.Mapping (MatchStrategy (..))
import Method.Types (MethodCF (..))
import Types (BiosphereFlow (..))

{- | The whole report for one database: one entry per loaded method collection,
so a maintainer comparing two method versions sees both asymmetries side by
side.
-}
data CoverageReport = CoverageReport
    { crDbName :: !Text
    , crCollections :: ![CollectionBridges]
    }
    deriving (Show, Eq)

{- | One collection's coverage of the database. 'cbCharacterizedFlows' of
'cbTotalFlows' is the honest reach (name matches /and/ bridges); 'cbGroups' is
the subset reached only through a bridge — empty means every scored flow
matches by its exact name.
-}
data CollectionBridges = CollectionBridges
    { cbCollection :: !Text
    , cbTotalFlows :: !Int
    , cbCharacterizedFlows :: !Int
    , cbGroups :: ![BridgeGroup]
    }
    deriving (Show, Eq)

{- | The database flows that bridge to one factor. 'bgMethodName' is the name
the method carries for the substance — the name each bridged flow should be
renamed to for the database to score in an exact-name tool.
-}
data BridgeGroup = BridgeGroup
    { bgCas :: !(Maybe Text)
    , bgMethodName :: !Text
    , bgBridged :: ![BridgedFlow]
    }
    deriving (Show, Eq)

-- | One database flow scored only through a bridge, and which bridge won.
data BridgedFlow = BridgedFlow
    { brfFlowName :: !Text
    , brfStrategy :: !MatchStrategy
    }
    deriving (Show, Eq)

-- | Whether a strategy reached a flow by its own name, or bridged to it.
data Reach = Exact | Bridged

{- | An exact-name match is what an exact-string consumer also sees; every other
strategy is a bridge it would miss. Total on 'MatchStrategy' so a new variant
forces a decision here.
-}
strategyReach :: MatchStrategy -> Reach
strategyReach ByName = Exact
strategyReach ByUUID = Exact
strategyReach ByCAS = Bridged
strategyReach BySynonym = Bridged
strategyReach ByProxy = Bridged

{- | Fold a collection's effective per-method mappings into its bridge groups.
'total' and 'characterized' are the caller's honest counts (from
'Method.Mapping.characterizedFlowIds'); the groups are derived here.
-}
collectionBridges ::
    -- | collection name
    Text ->
    -- | total biosphere flows in the database
    Int ->
    -- | flows the collection characterizes (distinct, bridges included)
    Int ->
    -- | per-method effective mappings (factor, matched flow + winning strategy)
    [[(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]] ->
    CollectionBridges
collectionBridges name total characterized perMethod =
    CollectionBridges name total characterized groups
  where
    tuples =
        [ (cf, flow, strat)
        | mappings <- perMethod
        , (cf, Just (flow, strat)) <- mappings
        ]
    reachOf (_, _, strat) = strategyReach strat
    exactNameIds =
        Set.fromList [bfId flow | t@(_, flow, _) <- tuples, isExact (reachOf t)]
    bridged =
        [ (cf, flow, strat)
        | t@(cf, flow, strat) <- tuples
        , isBridged (reachOf t)
        , bfId flow `Set.notMember` exactNameIds
        ]
    -- Group by the factor's own name: the flows that all bridge to it are the
    -- same substance as the method names it, so that name is their rename target.
    byMethodName =
        M.fromListWith
            (<>)
            [(mcfFlowName cf, [(cf, flow, strat)]) | (cf, flow, strat) <- bridged]
    groups = sortOn bgMethodName (map toGroup (M.toList byMethodName))
    toGroup (methodName, items) =
        BridgeGroup
            { bgCas = firstJust ([mcfCAS cf | (cf, _, _) <- items] ++ [bfCAS f | (_, f, _) <- items])
            , bgMethodName = methodName
            , bgBridged = flowsOf items
            }
    -- One row per distinct database name; a flow bridged by several factors
    -- keeps its first-seen strategy.
    flowsOf items =
        sortOn brfFlowName $
            [ BridgedFlow (bfName f) s
            | (_, f, s) <- nubOrdOn (\(_, f, _) -> bfName f) items
            ]

isExact :: Reach -> Bool
isExact Exact = True
isExact Bridged = False

isBridged :: Reach -> Bool
isBridged Bridged = True
isBridged Exact = False

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

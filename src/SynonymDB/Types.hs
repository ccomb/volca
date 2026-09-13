{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- | Types for the synonym database used for flow matching.
module SynonymDB.Types (
    SynonymDB (..),
    BridgeDirection (..),
    SynEdge (..),
    SynViews (..),
    emptySynonymDB,
) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Store (Store)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Which flow direction a synonym bridge is valid for. Most bridges are pure
substance identity and apply to both directions ('BridgeBoth'). Some hold only
one way: a water @withdrawal@ name ("river water") bridges to a resource flow
only for an INPUT CF; letting an OUTPUT (release) CF inherit it would apply a
withdrawal scarcity factor to a release, wrong in sign or magnitude.
-}
data BridgeDirection = BridgeBoth | BridgeInput | BridgeOutput
    deriving (Eq, Ord, Show, Generic, NFData, Store)

{- | A normalized @SameAs@ edge carrying the direction it is valid for. Kept in
'synEdges' so the relation can be re-closed (merging) without losing the
direction constraint.
-}
data SynEdge = SynEdge
    { seA :: !Text
    , seB :: !Text
    , seDir :: !BridgeDirection
    }
    deriving (Eq, Show, Generic, NFData, Store)

{- | Direction-restricted views of a 'SynonymDB', precomputed at build time.

'AllBoth' is the common case – no directed edge exists, so the input and output
views both coincide with the union tables; no view is materialized (zero cost
for large untyped sets). 'DirectedViews' holds the closure of @both ∪ input@ and
of @both ∪ output@ separately, because a direction restriction can SPLIT a group
(@a-b [input]@, @b-c [both]@ ⇒ input {a,b,c} but output {b,c}) – a split that the
union tables cannot recover at lookup time. Views are terminal: their own
'synViews' is 'AllBoth' and their 'synEdges' is empty – nothing re-closes a
view, so only its lookup tables are materialized (and serialized).
-}
data SynViews
    = AllBoth
    | DirectedViews !SynonymDB !SynonymDB
    deriving (Eq, Show, Generic, NFData, Store)

{- | Synonym database with bidirectional lookups.

- @synNameToId@: normalized flow name → synonym group ID (union closure)
- @synIdToNames@: group ID → all names in that group (union closure)
- @synEdges@: the normalized 'SynEdge's the classes were closed from, kept so the
  relation can be re-closed on a restricted node set (an induced subgraph on the
  used flow names) at fan-out time – a closed class cannot be re-split once its
  internal edges are gone.
- @synViews@: direction-restricted views (see 'SynViews'). The top-level tables
  stay the union closure so direction-agnostic consumers are unchanged; the
  matching layer selects a view by the CF's direction.
-}
data SynonymDB = SynonymDB
    { synNameToId :: !(Map Text Int)
    , synIdToNames :: !(Map Int [Text])
    , synEdges :: ![SynEdge]
    , synViews :: !SynViews
    }
    deriving (Eq, Show, Generic, NFData, Store)

-- | Empty synonym database
emptySynonymDB :: SynonymDB
emptySynonymDB = SynonymDB M.empty M.empty [] AllBoth

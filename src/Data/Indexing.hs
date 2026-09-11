{- | Building an index asserts that the key determines the value. This module
holds the three things there are to do when a key turns out not to.

'Data.Map.fromList' keeps the last row of a repeated key and says nothing, so a
list read out of a file - where nothing guarantees the key is a primary key -
needs one of these instead:

* 'uniqueIndex' refuses, naming the keys that repeat. For a table where two rows
  give two answers and nothing can choose between them.
* 'collisions' hands back every row a key carried, so the caller can say what
  they disagreed on. For a message that has to name both.
* 'repeated' answers the yes-or-no version: which keys appear more than once.

Keeping all the rows instead of refusing is 'Data.Map.fromListWith', which needs
no help from here; pass @flip (<>)@ to keep them in the order they were read.
-}
module Data.Indexing (
    repeated,
    collisions,
    uniqueIndex,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

-- | The keys a list carries more than once, ascending, each named once.
repeated :: (Ord k) => [k] -> [k]
repeated ks = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(k, 1) | k <- ks]))

{- | The keys carried by more than one row, each with every row it carried, in
the order the rows were given.
-}
collisions :: (Ord k) => [(k, v)] -> [(k, NonEmpty v)]
collisions kvs =
    [ (k, vs)
    | (k, vs) <- M.toList (M.fromListWith (flip (<>)) [(k, v :| []) | (k, v) <- kvs])
    , NE.length vs > 1
    ]

{- | Index rows on a key that must not repeat, or refuse and name the keys that
did. The caller owns the wording: a unit table and a method archive fail for the
same reason and do not say it the same way.
-}
uniqueIndex :: (Ord k) => [(k, v)] -> Either (NonEmpty k) (M.Map k v)
uniqueIndex kvs = case repeated (map fst kvs) of
    [] -> Right (M.fromList kvs)
    (k : ks) -> Left (k :| ks)

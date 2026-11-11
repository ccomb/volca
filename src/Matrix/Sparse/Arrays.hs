{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The suite builds the engine unoptimised, and these loops are the sparse
-- solver's innermost: unoptimised, a real technosphere would take minutes.
{-# OPTIONS_GHC -O2 #-}

-- | The array layout and the loops the modules of the sparse solver share.
module Matrix.Sparse.Arrays (
    Lists (..),
    lineCount,
    forRange,
    add,
    filled,
) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)

{- | Lists of whole numbers, one per line, laid end to end. 'listStart' has one
entry more than there are lines: line @k@ is the stretch of 'listItem' from
@listStart[k]@ up to @listStart[k + 1]@.
-}
data Lists = Lists
    { listStart :: !(PrimArray Int)
    , listItem :: !(PrimArray Int)
    }

lineCount :: Lists -> Int
lineCount = max 0 . subtract 1 . sizeofPrimArray . listStart

-- | Run the action on each number from the first bound up to the second, left out.
forRange :: forall s. Int -> Int -> (Int -> ST s ()) -> ST s ()
forRange !from !to body = go from
  where
    go :: Int -> ST s ()
    go !i = when (i < to) (body i >> go (i + 1))
{-# INLINE forRange #-}

add :: (Prim a, Num a) => MutablePrimArray s a -> Int -> a -> ST s ()
add array at by = readPrimArray array at >>= writePrimArray array at . (+ by)
{-# INLINE add #-}

filled :: (Prim a) => Int -> a -> ST s (MutablePrimArray s a)
filled count value = do
    array <- newPrimArray count
    setPrimArray array 0 count value
    pure array
{-# INLINE filled #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | The entries of a sparse matrix, and those entries held column after column.
module Matrix.Triples (
    SparseTriple (..),
    ByColumn,
    byColumn,
    columnTriples,
    entriesOfColumn,
) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Store (Size (..), Store (..))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Generics (Generic)

{- | Sparse matrix coordinate triplet (row, col, value)
Using Int32 for matrix indices to support large databases (up to 2 billion activities)
Unboxed to eliminate per-element boxing overhead (~48 bytes → 16 bytes per triple)
With ~800K triples, this saves ~25MB + significant GC pressure
-}
data SparseTriple = SparseTriple {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 {-# UNPACK #-} !Double
    deriving (Eq, Show, Generic)

-- Manual Unbox instance for SparseTriple to enable VU.Vector storage
newtype instance VU.MVector s SparseTriple = MV_SparseTriple (VU.MVector s (Int32, Int32, Double))
newtype instance VU.Vector SparseTriple = V_SparseTriple (VU.Vector (Int32, Int32, Double))

instance VGM.MVector VU.MVector SparseTriple where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MV_SparseTriple v) = VGM.basicLength v
    basicUnsafeSlice i n (MV_SparseTriple v) = MV_SparseTriple $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_SparseTriple v1) (MV_SparseTriple v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = MV_SparseTriple <$> VGM.basicUnsafeNew n
    basicInitialize (MV_SparseTriple v) = VGM.basicInitialize v
    basicUnsafeRead (MV_SparseTriple v) i = do
        (r, c, val) <- VGM.basicUnsafeRead v i
        return $ SparseTriple r c val
    basicUnsafeWrite (MV_SparseTriple v) i (SparseTriple r c val) =
        VGM.basicUnsafeWrite v i (r, c, val)

instance VG.Vector VU.Vector SparseTriple where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_SparseTriple v) = V_SparseTriple <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_SparseTriple v) = MV_SparseTriple <$> VG.basicUnsafeThaw v
    basicLength (V_SparseTriple v) = VG.basicLength v
    basicUnsafeSlice i n (V_SparseTriple v) = V_SparseTriple $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_SparseTriple v) i = do
        (r, c, val) <- VG.basicUnsafeIndexM v i
        return $ SparseTriple r c val

instance VU.Unbox SparseTriple

-- Store instance for cache serialization
instance Store SparseTriple where
    size = ConstSize 16 -- Int32 (4) + Int32 (4) + Double (8)
    poke (SparseTriple r c v) = poke r >> poke c >> poke v
    peek = SparseTriple <$> peek <*> peek <*> peek

-- NFData derived via Generic
instance NFData SparseTriple

-- | Store instance for VU.Vector SparseTriple (store has no built-in instance for custom Unbox types)
instance Store (VU.Vector SparseTriple) where
    size = VarSize $ \v -> 8 + VU.length v * 16
    poke v = do
        poke (VU.length v)
        VU.mapM_ poke v
    peek = do
        n <- peek
        VU.replicateM n peek

{- | Entries whose columns never go down from one to the next, so the entries
of a column sit together and a reader finds them without reading the others.
Only 'byColumn' makes one.
-}
newtype ByColumn = ByColumn (VU.Vector SparseTriple)
    deriving (Eq, Show, Generic, NFData)

columnTriples :: ByColumn -> VU.Vector SparseTriple
columnTriples (ByColumn triples) = triples

-- | The entries of one column, found without reading those of the others.
entriesOfColumn :: ByColumn -> Int -> VU.Vector SparseTriple
entriesOfColumn (ByColumn triples) j = VU.unsafeSlice from (firstFrom (j + 1) - from) triples
  where
    from :: Int
    from = firstFrom j

    -- The place of the first entry whose column is this one or a later one.
    firstFrom :: Int -> Int
    firstFrom c = within 0 (VU.length triples)
      where
        within :: Int -> Int -> Int
        within lo hi
            | lo >= hi = lo
            | columnAt mid < c = within (mid + 1) hi
            | otherwise = within lo mid
          where
            mid = (lo + hi) `div` 2

    columnAt :: Int -> Int
    columnAt k = case VU.unsafeIndex triples k of SparseTriple _ c _ -> fromIntegral c

{- | The same entries column after column, those of one column in the order
they came in. Entries already in that order are kept as the vector they are.
-}
byColumn :: VU.Vector SparseTriple -> ByColumn
byColumn triples
    | VU.all (\k -> columnAt k <= columnAt (k + 1)) (VU.enumFromN 0 (max 0 (VU.length triples - 1))) = ByColumn triples
    | otherwise = ByColumn (VU.unsafeBackpermute triples order)
  where
    columnAt :: Int -> Int
    columnAt k = case VU.unsafeIndex triples k of SparseTriple _ j _ -> fromIntegral j

    places :: [Int]
    places = [0 .. VU.length triples - 1]

    -- Where the first entry of each column goes.
    starts :: VU.Vector Int
    starts = VU.prescanl' (+) 0 $ VU.create $ do
        counts <- VUM.replicate (foldl' (\widest k -> max widest (columnAt k)) 0 places + 1) 0
        forM_ places $ \k -> VUM.modify counts (+ 1) (columnAt k)
        pure counts

    -- The entry each place takes: a counting sort, which keeps the order within a column.
    order :: VU.Vector Int
    order = VU.create $ do
        next <- VU.thaw starts
        taken <- VUM.new (VU.length triples)
        forM_ places $ \k -> do
            place <- VUM.read next (columnAt k)
            VUM.write taken place k
            VUM.write next (columnAt k) (place + 1)
        pure taken

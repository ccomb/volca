{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A list of lines, most of which say nothing but a flow, an amount and one
of a few shapes, kept in a fraction of the memory a list of them takes.

A process of an aggregated inventory lists thousands of elementary flows, and
a database of such processes tens of millions of lines. As a list, each line
is an object of its own with room for everything a line may say, and a list
cell: about 116 bytes. Here a line that says only the usual is a row of an
array, 28 bytes, and its shape (what it shares with its neighbours: a unit, a
direction, a place) is written once per process. A line that says more stays
as it is.

The list comes back in the order it was given, since a line is also known by
its position.

The type is opaque. Only 'pack' builds a value, and reading one from bytes
checks what 'pack' guarantees, so 'unpack' never meets a row whose shape is
missing.
-}
module PackedLines (
    Packable (..),
    PackedLines,
    pack,
    unpack,
    unpackWith,
    keptLines,
) where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless)
import Data.Either (isRight, lefts, rights)
import Data.List (mapAccumL, sortOn)
import qualified Data.Map.Strict as M
import Data.Store (Peek, Size (..), Store (..))
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)

{- | A line that may be plain: a flow, an amount, and a shape few lines of one
list differ by. One instance says both ways, so a list cannot be packed by one
rule and read back by another.

Law: @'toPlain' x == Just (f, n, s)@ implies @'fromPlain' f n s == x@.
-}
class (Ord (Shape a)) => Packable a where
    -- | What a plain line has beside its flow and its amount.
    type Shape a

    -- | The line as a plain one, or 'Nothing' for a line that says more.
    toPlain :: a -> Maybe (UUID, Double, Shape a)

    fromPlain :: UUID -> Double -> Shape a -> a

-- | A flow's identifier in two words, an amount, and the number of a shape.
type Row = (Word64, Word64, Double, Word32)

data PackedLines a = PackedLines
    { plKept :: ![a]
    -- ^ The lines that are not plain, in order.
    , plRows :: !(VU.Vector Row)
    -- ^ The plain lines, in order.
    , plShapes :: !(V.Vector (Shape a))
    -- ^ The shapes the rows name by their number.
    , plIsPlain :: !(VU.Vector Bool)
    -- ^ For each line of the list, whether it is the next row or the next kept line.
    }

instance (NFData a, NFData (Shape a)) => NFData (PackedLines a) where
    rnf (PackedLines kept rows shapes isPlain) = rnf kept `seq` rnf rows `seq` rnf shapes `seq` rnf isPlain

pack :: forall a. (Packable a) => [a] -> PackedLines a
pack xs = PackedLines kept rows shapes isPlain
  where
    (numbers, classified) = mapAccumL classify M.empty xs

    classify :: M.Map (Shape a) Word32 -> a -> (M.Map (Shape a) Word32, Either a Row)
    classify !seen x = case toPlain x of
        Nothing -> (seen, Left x)
        Just (flow, amount, shape) ->
            let (hi, lo) = UUID.toWords64 flow
             in case M.lookup shape seen of
                    Just number -> (seen, Right (hi, lo, amount, number))
                    Nothing ->
                        let number = fromIntegral (M.size seen)
                         in (M.insert shape number seen, Right (hi, lo, amount, number))

    -- Each built to its exact length, and the kept lines to the end of their
    -- list, so nothing here holds on to the list it was packed from.
    !isPlain = VU.fromListN (length xs) (map isRight classified)
    !rows = VU.fromListN (VU.length (VU.filter id isPlain)) (rights classified)
    !shapes = V.fromListN (M.size numbers) (map fst (sortOn snd (M.toList numbers)))
    !kept = let ls = lefts classified in length ls `seq` ls

{- | The list that was packed, line for line. Built as it is read: taking its
first lines does not build the rest.
-}
unpack :: (Packable a) => PackedLines a -> [a]
unpack = unpackWith id fromPlain

{- | One reading of every line, in order: of a kept line as it is, of a plain
one from its flow, its amount and its shape. A reader that wants no more than a
flow or an amount of each line builds no plain line to get it.
-}
unpackWith :: (a -> b) -> (UUID -> Double -> Shape a -> b) -> PackedLines a -> [b]
unpackWith ofKept ofPlain packed = go 0 0 (plKept packed)
  where
    rows = plRows packed
    plain (hi, lo, amount, number) = ofPlain (UUID.fromWords64 hi lo) amount (shapeOf packed number)
    go !line !row kept = case (plIsPlain packed VU.!? line, rows VU.!? row, kept) of
        (Just True, Just r, _) -> plain r : go (line + 1 :: Int) (row + 1) kept
        (Just False, _, k : rest) -> ofKept k : go (line + 1) row rest
        -- Past the last line. Nothing remains of a value 'pack' built.
        _ -> map ofKept kept <> map plain (VU.toList (VU.drop row rows))

-- | The lines that are not plain, in order. They are held as they are, so this builds nothing.
keptLines :: PackedLines a -> [a]
keptLines = plKept

-- A row's number is below the number of shapes: 'pack' numbers the shapes as
-- it meets them, and 'peek' refuses bytes where one is not.
shapeOf :: PackedLines a -> Word32 -> Shape a
shapeOf packed number = plShapes packed V.! fromIntegral number

{- | The four parts one after the other. Reading checks what 'pack' guarantees
and fails otherwise, as on any bytes that are not a value: a cache that fails
to decode is built again.
-}
instance (Store a, Store (Shape a)) => Store (PackedLines a) where
    size = VarSize $ \(PackedLines kept rows shapes isPlain) -> sizeOf kept + sizeOf rows + sizeOf shapes + sizeOf isPlain
      where
        sizeOf :: (Store b) => b -> Int
        sizeOf x = case size of
            ConstSize n -> n
            VarSize f -> f x
    poke (PackedLines kept rows shapes isPlain) = poke kept >> poke rows >> poke shapes >> poke isPlain
    peek = do
        kept <- peek
        rows <- peek
        shapes <- peek
        isPlain <- peek
        let plain = VU.length (VU.filter id isPlain)
        refuseUnless (plain == VU.length rows) "as many plain lines as rows"
        refuseUnless (VU.length isPlain - plain == length (kept :: [a])) "as many other lines as kept"
        refuseUnless (VU.all (\(_, _, _, number) -> fromIntegral number < V.length shapes) rows) "a shape for every row"
        pure (PackedLines kept rows shapes isPlain)
      where
        refuseUnless :: Bool -> String -> Peek ()
        refuseUnless holds what = unless holds (fail ("packed lines without " <> what))

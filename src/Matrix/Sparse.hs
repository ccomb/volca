{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The suite builds the engine unoptimised, and an unoptimised factorisation
-- of a real technosphere would take minutes.
{-# OPTIONS_GHC -O2 #-}

{- | The system of a technosphere, @(I − A) x = d@, factorised once and solved
for as many demands as are asked.

The factors are a value like any other: nothing has to be locked to read
them, so any number of demands are solved at the same time.

Pivots stay on the diagonal. The diagonal of @I − A@ is what a process keeps
of its own output once it has fed itself, and in a technosphere that
dominates its column. Where it does not, the process cannot be solved for,
and a demand that reaches it is refused rather than answered with numbers
that a division by almost nothing made up.
-}
module Matrix.Sparse (
    Factors,
    Unsolvable (..),
    factorize,
    solve,
) where

import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Matrix.Sparse.Arrays (Lists (..), add, filled, forRange)
import Matrix.Sparse.Order (Elimination (..), eliminate)
import Types (SparseTriple (..))

-- | Why a demand has no answer.
data Unsolvable
    = -- | Nothing is left of the output of the process at this column once its own supply chain has taken its share
      Singular !Int
    | -- | The column of this process holds a coefficient that is not a finite number
      NotFinite !Int
    | -- | The coefficient at this place of the list names a process outside the matrix
      OutOfRange !Int
    | -- | The demand has this many entries, where one per process is expected
      WrongLength !Int
    deriving (Eq, Show)

instance NFData Unsolvable where
    rnf = rwhnf

{- | @I − A@, factorised. Places count the processes in the order they are
solved.
-}
data Factors = Factors
    { size :: !Int
    , columnAt :: !(PrimArray Int)
    -- ^ The column solved at each place
    , knots :: !(SmallArray Knot)
    }

instance NFData Factors where
    rnf factors = foldr (\knot rest -> either (`seq` rest) (`seq` rest) (knotFactors knot)) () (knots factors)

{- | A stretch of places solved together. Its factors are there or the reason
they are not: a pivot of a refused knot is nowhere to divide by.
-}
data Knot = Knot
    { knotStart :: !Int
    , knotSize :: !Int
    , knotFactors :: !(Either Unsolvable LU)
    }

{- | A knot as L·U, the diagonal of L being ones. Both are held by columns on
the pattern the elimination order announced, places counted from the knot's
first.
-}
data LU = LU
    { reciprocal :: !(PrimArray Double)
    -- ^ One over each pivot: a solve multiplies where it would divide
    , lower :: !Lists
    , lowerValue :: !(PrimArray Double)
    , upper :: !Lists
    , upperValue :: !(PrimArray Double)
    }

{- | A pivot has to exceed this share of what its diagonal started from, the 1
of the identity and every coefficient of the process on itself, to be divided
by. Judging it against the rest of its column would not do: a process counted
in grams beside one counted in tonnes has coefficients of a million, and a
sound pivot of 0.4 would look like nothing.
-}
tolerance :: Double
tolerance = 1e-12

{- | Factorise @I − A@ for a technosphere of so many processes, @A@ given as
coefficients in any order, those naming the same entry added up.

Never fails: what cannot be factorised is kept as the reason, and returned to
the demands that reach it.
-}
factorize :: Int -> U.Vector SparseTriple -> Factors
factorize processes triples = case U.findIndex outside triples of
    Just at -> whole (generatePrimArray count id) (Left (OutOfRange at))
    Nothing -> whole (pivotOrder elimination) (runST (factorBlock columns numbering (Block 0 count) elimination))
  where
    count :: Int
    count = max 0 processes
    outside :: SparseTriple -> Bool
    outside (SparseTriple row column _) = row < 0 || column < 0 || fromIntegral row >= count || fromIntegral column >= count
    columns :: Columns
    columns = runST (assemble count triples)
    elimination :: Elimination
    elimination = eliminate (entries columns)
    numbering :: Numbering
    numbering = Numbering (pivotOrder elimination) (inverted (pivotOrder elimination))
    whole :: PrimArray Int -> Either Unsolvable LU -> Factors
    whole order outcome = Factors count order (smallArrayFromList [Knot 0 count outcome])

-- | Where each item of a permutation stands.
inverted :: PrimArray Int -> PrimArray Int
inverted order = runST $ do
    places <- newPrimArray (sizeofPrimArray order)
    forRange 0 (sizeofPrimArray order) (\at -> writePrimArray places (indexPrimArray order at) at)
    unsafeFreezePrimArray places

-- | @I − A@ by columns, each entry held once.
data Columns = Columns
    { entries :: !Lists
    -- ^ Per column, the rows it holds
    , entryValue :: !(PrimArray Double)
    , scale :: !(PrimArray Double)
    -- ^ Per column, what its pivot is judged against
    , unsound :: !(PrimArray Int)
    -- ^ Per column, 1 when one of its coefficients is not a finite number
    }

{- | Lay @I − A@ out by columns. The coefficients must all name a process of
the matrix.
-}
assemble :: forall s. Int -> U.Vector SparseTriple -> ST s Columns
assemble count triples = do
    -- One entry per column for the identity, one per coefficient: room for
    -- everything before the repeated entries are added up.
    starts <- filled (count + 1) 0
    U.forM_ triples (\(SparseTriple _ column _) -> add starts (fromIntegral column + 1) 1)
    forRange 0 count (\column -> readPrimArray starts column >>= add starts (column + 1) . (+ 1))
    room <- readPrimArray starts count
    rows <- newPrimArray room
    values <- newPrimArray room
    next <- newPrimArray count
    scales <- filled count 1
    flaws <- filled count 0
    forRange 0 count $ \column -> do
        at <- readPrimArray starts column
        writePrimArray rows at column
        writePrimArray values at 1
        writePrimArray next column (at + 1)
    U.forM_ triples $ \(SparseTriple row column value) -> do
        let into = fromIntegral column
        at <- readPrimArray next into
        writePrimArray next into (at + 1)
        writePrimArray rows at (fromIntegral row)
        writePrimArray values at (negate value)
        when (row == column) (add scales into (abs value))
        when (isNaN value || isInfinite value) (writePrimArray flaws into 1)
    heldAt <- filled count (-1)
    kept <- merge (Merging rows values heldAt) starts 0 0
    writePrimArray starts count kept
    shrinkMutablePrimArray rows kept
    shrinkMutablePrimArray values kept
    Columns
        <$> (Lists <$> unsafeFreezePrimArray starts <*> unsafeFreezePrimArray rows)
        <*> unsafeFreezePrimArray values
        <*> unsafeFreezePrimArray scales
        <*> unsafeFreezePrimArray flaws
  where
    -- Column by column, the entries move down over the room the repeated
    -- ones leave, so each column starts where the one before ends.
    merge :: Merging s -> MutablePrimArray s Int -> Int -> Int -> ST s Int
    merge merging starts !column !to
        | column >= count = pure to
        | otherwise = do
            from <- readPrimArray starts column
            end <- readPrimArray starts (column + 1)
            writePrimArray starts column to
            mergeColumn merging (Span from end) to to >>= merge merging starts (column + 1)

-- | The entries being merged: their rows, their values, and where the column being merged holds each row.
data Merging s = Merging !(MutablePrimArray s Int) !(MutablePrimArray s Double) !(MutablePrimArray s Int)

-- | From a place up to another, left out.
data Span = Span !Int !Int

{- | Add up the entries of one column that name the same row, in the order
they were given. The column is rewritten from @first@ on; an entry found
there or after is one of this column's own, anything before belongs to an
earlier column.
-}
mergeColumn :: forall s. Merging s -> Span -> Int -> Int -> ST s Int
mergeColumn (Merging rows values heldAt) (Span from end) first = go from
  where
    go :: Int -> Int -> ST s Int
    go !at !to
        | at >= end = pure to
        | otherwise = do
            row <- readPrimArray rows at
            value <- readPrimArray values at
            held <- readPrimArray heldAt row
            if held >= first
                then add values held value >> go (at + 1) to
                else do
                    writePrimArray heldAt row to
                    writePrimArray rows to row
                    writePrimArray values to value
                    go (at + 1) (to + 1)

-- | The order the columns are solved in, read both ways.
data Numbering = Numbering
    { columnOf :: !(PrimArray Int)
    , placeOf :: !(PrimArray Int)
    }

-- | A stretch of places: the first and how many.
data Block = Block !Int !Int

-- | What a factorisation writes into.
data Scratch s = Scratch
    { dense :: !(MutablePrimArray s Double)
    -- ^ The column being eliminated, one cell per place of the block, all zero between two columns
    , inverses :: !(MutablePrimArray s Double)
    , lowerValues :: !(MutablePrimArray s Double)
    , upperValues :: !(MutablePrimArray s Double)
    }

{- | Factorise one block, column after column: a column first receives what
every earlier pivot reaching it sends down, then becomes a pivot itself. The
pattern is known beforehand, so nothing is searched and nothing moves.
-}
factorBlock :: forall s. Columns -> Numbering -> Block -> Elimination -> ST s (Either Unsolvable LU)
factorBlock columns numbering (Block first count) (Elimination _ under over) = do
    scratch <- Scratch <$> filled count 0 <*> newPrimArray count <*> newPrimArray (held under) <*> newPrimArray (held over)
    refusal <- eliminateFrom scratch 0
    case refusal of
        Just why -> pure (Left why)
        Nothing ->
            fmap Right $
                LU
                    <$> unsafeFreezePrimArray (inverses scratch)
                    <*> pure under
                    <*> unsafeFreezePrimArray (lowerValues scratch)
                    <*> pure over
                    <*> unsafeFreezePrimArray (upperValues scratch)
  where
    held :: Lists -> Int
    held = sizeofPrimArray . listItem

    eliminateFrom :: Scratch s -> Int -> ST s (Maybe Unsolvable)
    eliminateFrom scratch !place
        | place >= count = pure Nothing
        | indexPrimArray (unsound columns) column /= 0 = pure (Just (NotFinite column))
        | otherwise = do
            spreadOut scratch column
            forRange (lineStart over place) (lineStart over (place + 1)) $ \at -> do
                let earlier = indexPrimArray (listItem over) at
                reaching <- readPrimArray (dense scratch) earlier
                writePrimArray (dense scratch) earlier 0
                writePrimArray (upperValues scratch) at reaching
                subtractScaled (dense scratch) 0 (stretch under (lowerValues scratch) earlier) reaching
            pivot <- readPrimArray (dense scratch) place
            writePrimArray (dense scratch) place 0
            -- A pivot that is not a number fails this test too, and is refused.
            if abs pivot > tolerance * indexPrimArray (scale columns) column
                then do
                    let inverse = 1 / pivot
                    writePrimArray (inverses scratch) place inverse
                    forRange (lineStart under place) (lineStart under (place + 1)) $ \at -> do
                        let later = indexPrimArray (listItem under) at
                        readPrimArray (dense scratch) later >>= writePrimArray (lowerValues scratch) at . (* inverse)
                        writePrimArray (dense scratch) later 0
                    eliminateFrom scratch (place + 1)
                else pure (Just (Singular column))
      where
        column :: Int
        column = indexPrimArray (columnOf numbering) (first + place)

    -- The entries of a column that fall in the block, each at its place.
    spreadOut :: Scratch s -> Int -> ST s ()
    spreadOut scratch column =
        forRange (lineStart (entries columns) column) (lineStart (entries columns) (column + 1)) $ \at -> do
            let place = indexPrimArray (placeOf numbering) (indexPrimArray (listItem (entries columns)) at) - first
            when (place >= 0 && place < count) (writePrimArray (dense scratch) place (indexPrimArray (entryValue columns) at))

lineStart :: Lists -> Int -> Int
lineStart = indexPrimArray . listStart
{-# INLINE lineStart #-}

-- | Entries of the factors: where each one lands, and its value.
data Stretch s = Stretch !(PrimArray Int) !(MutablePrimArray s Double) !Span

-- | One line of lists whose values are held beside them.
stretch :: Lists -> MutablePrimArray s Double -> Int -> Stretch s
stretch lists values line = Stretch (listItem lists) values (Span (lineStart lists line) (lineStart lists (line + 1)))
{-# INLINE stretch #-}

{- | @target[offset + item] −= value × by@ along a stretch: the loop nearly
all the time of a factorisation and of a solve is spent in. Kept out of line:
inlined into its callers it shares their registers and runs slower.
-}
subtractScaled :: forall s. MutablePrimArray s Double -> Int -> Stretch s -> Double -> ST s ()
subtractScaled target !offset (Stretch items values (Span from to)) !by = go from
  where
    go :: Int -> ST s ()
    go !at = when (at < to) $ do
        let into = offset + indexPrimArray items at
        value <- readPrimArray values at
        was <- readPrimArray target into
        writePrimArray target into (was - value * by)
        go (at + 1)
{-# NOINLINE subtractScaled #-}

{- | Solve for one demand, one entry per process. A demand is refused only
when it reaches a process that cannot be solved for; the reason names it.
-}
solve :: Factors -> U.Vector Double -> Either Unsolvable (U.Vector Double)
solve factors demand
    | U.length demand /= size factors = Left (WrongLength (U.length demand))
    | otherwise = runST (answer factors demand)

answer :: forall s. Factors -> U.Vector Double -> ST s (Either Unsolvable (U.Vector Double))
answer factors demand = do
    asked <- newPrimArray count
    forRange 0 count (\place -> writePrimArray asked place (U.unsafeIndex demand (indexPrimArray (columnAt factors) place)))
    refusal <- descend asked (sizeofSmallArray (knots factors) - 1)
    case refusal of
        Just why -> pure (Left why)
        Nothing -> do
            supplied <- MU.unsafeNew count
            forRange 0 count (\place -> readPrimArray asked place >>= MU.unsafeWrite supplied (indexPrimArray (columnAt factors) place))
            Right <$> U.unsafeFreeze supplied
  where
    count :: Int
    count = size factors
    -- From the last knot to the first: a knot only ever asks of earlier ones.
    descend :: MutablePrimArray s Double -> Int -> ST s (Maybe Unsolvable)
    descend asked !nth
        | nth < 0 = pure Nothing
        | otherwise = do
            let knot = indexSmallArray (knots factors) nth
            idle <- nothingAsked asked (Span (knotStart knot) (knotStart knot + knotSize knot))
            -- Nothing asked of a knot, nothing it has to supply: a demand
            -- that stays clear of a refused knot is answered.
            if idle
                then descend asked (nth - 1)
                else case knotFactors knot of
                    Left why -> pure (Just why)
                    Right lu -> solveKnot lu asked (knotStart knot) >> descend asked (nth - 1)

nothingAsked :: forall s. MutablePrimArray s Double -> Span -> ST s Bool
nothingAsked asked (Span from to) = go from
  where
    go :: Int -> ST s Bool
    go !at
        | at >= to = pure True
        | otherwise = readPrimArray asked at >>= \value -> if value == 0 then go (at + 1) else pure False

{- | What is asked of a knot becomes what it supplies, in place: down through
L, then up through U. A place of which nothing is asked sends nothing on,
which is most places when a demand names one process.
-}
solveKnot :: forall s. LU -> MutablePrimArray s Double -> Int -> ST s ()
solveKnot lu asked first = do
    -- Only read: the factors are shared by every demand being solved.
    lowers <- unsafeThawPrimArray (lowerValue lu)
    uppers <- unsafeThawPrimArray (upperValue lu)
    forRange 0 count $ \place -> do
        value <- readPrimArray asked (first + place)
        when (value /= 0) (subtractScaled asked first (stretch (lower lu) lowers place) value)
    ascend uppers (count - 1)
  where
    count :: Int
    count = sizeofPrimArray (reciprocal lu)
    ascend :: MutablePrimArray s Double -> Int -> ST s ()
    ascend uppers !place = when (place >= 0) $ do
        value <- readPrimArray asked (first + place)
        when (value /= 0) $ do
            let supplied = value * indexPrimArray (reciprocal lu) place
            writePrimArray asked (first + place) supplied
            subtractScaled asked first (stretch (upper lu) uppers place) supplied
        ascend uppers (place - 1)

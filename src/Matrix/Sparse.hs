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
    Extent (..),
    extent,
) where

import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Matrix.Sparse.Arrays (Lists (..), add, filled, forRange, lineCount)
import Matrix.Sparse.Order (Elimination (..), eliminate, loops)
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

{- | @I − A@, factorised, with nothing left to compute. Places count the
processes suppliers first: a process comes after every process it takes from,
unless both are in a loop of the supply chain, and the processes of a loop
stand side by side.
-}
data Factors = Factors
    { size :: !Int
    , columnAt :: !(PrimArray Int)
    -- ^ The column solved at each place
    , knots :: !(SmallArray Knot)
    -- ^ The places cut into stretches solved one after the other, the last first
    , between :: !Links
    }

instance NFData Factors where
    rnf = rwhnf

{- | A stretch of places solved together. Only what can be divided by is kept
as a pivot: a refused knot holds its reason and nothing to compute with.
-}
data Knot
    = -- | One process that no loop passes through, as one over its pivot
      Alone !Double
    | -- | So many processes that feed each other, factorised together
      Looped !Int !LU
    | -- | So many processes that cannot be solved for, and why
      Refused !Int !Unsolvable

{- | The entries of @I − A@ that link two knots, by the place of their column:
the place of their row, always an earlier one, and their value.
-}
data Links = Links !Lists !(PrimArray Double)

-- | How much a factorisation holds, and how far its answers can be trusted.
data Extent = Extent
    { loopCount :: !Int
    -- ^ The sets of several processes that feed each other
    , largestLoop :: !Int
    -- ^ How many processes the largest of them holds
    , factorEntries :: !Int
    -- ^ The entries off the diagonal that the factors of all of them hold
    , linkEntries :: !Int
    -- ^ The entries left as they were, between two knots
    , refusedProcesses :: !Int
    -- ^ The processes that cannot be solved for
    , smallestPivot :: !Double
    {- ^ The smallest pivot divided by, in absolute value, infinite when there
    is none. A diagonal starts from 1 or more, so a pivot of 1e-6 has lost six
    of the sixteen digits a number carries.
    -}
    }
    deriving (Eq, Show)

extent :: Factors -> Extent
extent factors = foldl' counted (Extent 0 0 0 (held links) 0 (1 / 0)) (knots factors)
  where
    links :: Lists
    Links links _ = between factors
    held :: Lists -> Int
    held = sizeofPrimArray . listItem
    -- The pivots are kept as one over each: the largest of those is the smallest pivot.
    dividedBy :: Extent -> Double -> Extent
    dividedBy so inverse = so{smallestPivot = min (smallestPivot so) (1 / abs inverse)}
    counted :: Extent -> Knot -> Extent
    counted so (Alone inverse) = so `dividedBy` inverse
    counted so (Refused processes _) = so{refusedProcesses = refusedProcesses so + processes}
    counted so (Looped processes lu) =
        (so `dividedBy` foldlPrimArray' (\most inverse -> max most (abs inverse)) 0 (reciprocal lu))
            { loopCount = loopCount so + 1
            , largestLoop = max (largestLoop so) processes
            , factorEntries = factorEntries so + held (lower lu) + held (upper lu)
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
    Just at ->
        Factors
            count
            (generatePrimArray count id)
            (smallArrayFromList [Refused count (OutOfRange at)])
            (Links (Lists (replicatePrimArray (count + 1) 0) emptyPrimArray) emptyPrimArray)
    Nothing -> Factors count (columnOf numbering) (mapSmallArray' knot shapes) (linked columns numbering shapes)
  where
    count :: Int
    count = max 0 processes
    outside :: SparseTriple -> Bool
    outside (SparseTriple row column _) = row < 0 || column < 0 || fromIntegral row >= count || fromIntegral column >= count
    columns :: Columns
    columns = runST (assemble count triples)
    sets :: Lists
    sets = loops (entries columns)
    -- Set after set, the processes of a set in the order of their numbers:
    -- what each loop is then put in order from.
    rough :: Numbering
    rough = numbered (listItem sets)
    shapes :: SmallArray Shape
    shapes = smallArrayFromListN (lineCount sets) (map shape [0 .. lineCount sets - 1])
    shape :: Int -> Shape
    shape nth
        | held == 1 = Single first
        | otherwise = Loop (Block first held) (eliminate (patternWithin (entries columns) rough (Block first held)))
      where
        first, held :: Int
        first = lineStart sets nth
        held = lineStart sets (nth + 1) - first
    numbering :: Numbering
    numbering = numbered (settled rough shapes)
    knot :: Shape -> Knot
    knot (Single place) = alone columns (indexPrimArray (columnOf numbering) place)
    knot (Loop block@(Block _ held) elimination) = either (Refused held) (Looped held) (runST (factorBlock columns numbering block elimination))

-- | A stretch of places: the first and how many.
data Block = Block !Int !Int

-- | How a set of processes is to be solved, before any number is computed.
data Shape
    = -- | The place of a process that no loop passes through
      Single !Int
    | -- | The places of processes that feed each other, and the order to eliminate them in, counted from the first of them
      Loop !Block !Elimination

placesOf :: Shape -> Block
placesOf (Single place) = Block place 1
placesOf (Loop block _) = block

-- | The order the columns are solved in, read both ways.
data Numbering = Numbering
    { columnOf :: !(PrimArray Int)
    , placeOf :: !(PrimArray Int)
    }

numbered :: PrimArray Int -> Numbering
numbered order = Numbering order $ runST $ do
    places <- newPrimArray (sizeofPrimArray order)
    forRange 0 (sizeofPrimArray order) (\at -> writePrimArray places (indexPrimArray order at) at)
    unsafeFreezePrimArray places

{- | The pattern of one block alone: its columns in the order of their places,
each row named by its place counted from the block's first, the rows of other
blocks left out.
-}
patternWithin :: Lists -> Numbering -> Block -> Lists
patternWithin whole numbering (Block first count) = runST $ do
    starts <- newPrimArray (count + 1)
    writePrimArray starts 0 0
    items <- newPrimArray (room 0 0)
    forRange 0 count $ \nth -> do
        readPrimArray starts nth >>= writePrimArray starts (nth + 1)
        forRange (lineStart whole (column nth)) (lineStart whole (column nth + 1)) $ \at -> do
            let row = indexPrimArray (placeOf numbering) (indexPrimArray (listItem whole) at) - first
            when (row >= 0 && row < count) $ do
                to <- readPrimArray starts (nth + 1)
                writePrimArray items to row
                writePrimArray starts (nth + 1) (to + 1)
    readPrimArray starts count >>= shrinkMutablePrimArray items
    Lists <$> unsafeFreezePrimArray starts <*> unsafeFreezePrimArray items
  where
    column :: Int -> Int
    column nth = indexPrimArray (columnOf numbering) (first + nth)
    -- What the block's columns hold in all, the rows of other blocks included.
    room :: Int -> Int -> Int
    room !nth !total
        | nth >= count = total
        | otherwise = room (nth + 1) (total + lineStart whole (column nth + 1) - lineStart whole (column nth))

-- | The column at each place, once every loop stands in the order it is eliminated in.
settled :: Numbering -> SmallArray Shape -> PrimArray Int
settled rough shapes = runST $ do
    order <- newPrimArray (sizeofPrimArray (columnOf rough))
    mapM_ (settle order) shapes
    unsafeFreezePrimArray order
  where
    settle :: MutablePrimArray s Int -> Shape -> ST s ()
    settle order (Single place) = writePrimArray order place (indexPrimArray (columnOf rough) place)
    settle order (Loop (Block first count) elimination) =
        forRange 0 count (\nth -> writePrimArray order (first + nth) (indexPrimArray (columnOf rough) (first + indexPrimArray (pivotOrder elimination) nth)))

-- | The links being written: where those of each place start, the place each comes from, its value.
data Linking s = Linking
    { linkStart :: !(MutablePrimArray s Int)
    , linkFrom :: !(MutablePrimArray s Int)
    , linkValue :: !(MutablePrimArray s Double)
    }

linked :: Columns -> Numbering -> SmallArray Shape -> Links
linked columns numbering shapes = runST $ do
    linking <- Linking <$> newPrimArray (count + 1) <*> newPrimArray room <*> newPrimArray room
    writePrimArray (linkStart linking) 0 0
    mapM_ (linkKnot linking . placesOf) shapes
    kept <- readPrimArray (linkStart linking) count
    shrinkMutablePrimArray (linkFrom linking) kept
    shrinkMutablePrimArray (linkValue linking) kept
    Links
        <$> (Lists <$> unsafeFreezePrimArray (linkStart linking) <*> unsafeFreezePrimArray (linkFrom linking))
        <*> unsafeFreezePrimArray (linkValue linking)
  where
    count, room :: Int
    count = sizeofPrimArray (columnOf numbering)
    room = sizeofPrimArray (entryValue columns)
    -- Numbered suppliers first, a column holds nothing after its own knot:
    -- what it holds before the knot's first place is all it holds outside.
    linkKnot :: Linking s -> Block -> ST s ()
    linkKnot linking (Block first held) = forRange first (first + held) $ \place -> do
        readPrimArray (linkStart linking) place >>= writePrimArray (linkStart linking) (place + 1)
        let column = indexPrimArray (columnOf numbering) place
        forRange (lineStart (entries columns) column) (lineStart (entries columns) (column + 1)) $ \at -> do
            let from = indexPrimArray (placeOf numbering) (indexPrimArray (listItem (entries columns)) at)
            when (from < first) $ do
                to <- readPrimArray (linkStart linking) (place + 1)
                writePrimArray (linkFrom linking) to from
                writePrimArray (linkValue linking) to (indexPrimArray (entryValue columns) at)
                writePrimArray (linkStart linking) (place + 1) (to + 1)

{- | Whether a pivot can be divided by. One that is not a number fails the
test too.
-}
dividable :: Columns -> Int -> Double -> Bool
dividable columns column pivot = abs pivot > tolerance * indexPrimArray (scale columns) column

-- | A process that no loop passes through is solved by its diagonal alone.
alone :: Columns -> Int -> Knot
alone columns column
    | indexPrimArray (unsound columns) column /= 0 = Refused 1 (NotFinite column)
    | dividable columns column pivot = Alone (1 / pivot)
    | otherwise = Refused 1 (Singular column)
  where
    pivot :: Double
    pivot = indexPrimArray (entryValue columns) (lineStart (entries columns) column)

-- | @I − A@ by columns, each entry held once.
data Columns = Columns
    { entries :: !Lists
    -- ^ Per column, the rows it holds, its own first
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
                subtractScaledFromScratch (dense scratch) 0 (stretch under (lowerValues scratch) earlier) reaching
            pivot <- readPrimArray (dense scratch) place
            writePrimArray (dense scratch) place 0
            if dividable columns column pivot
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
data Stretch values = Stretch !(PrimArray Int) !values !Span

-- | One line of lists whose values are held beside them.
stretch :: Lists -> values -> Int -> Stretch values
stretch lists values line = Stretch (listItem lists) values (Span (lineStart lists line) (lineStart lists (line + 1)))
{-# INLINE stretch #-}

{- | @target[offset + item] −= value × by@ along a stretch of finished
factors: the loop nearly all the time of a solve is spent in. The factors can
only be read here, which is what lets every demand being solved share them.
Kept out of line, as is the one below: inlined into its callers it shares
their registers and runs slower.
-}
subtractScaled :: MutablePrimArray s Double -> Int -> Stretch (PrimArray Double) -> Double -> ST s ()
subtractScaled = alongStretch (\values at -> pure (indexPrimArray values at))
{-# NOINLINE subtractScaled #-}

-- | The same, from a column of L that the factorisation writing it has finished.
subtractScaledFromScratch :: MutablePrimArray s Double -> Int -> Stretch (MutablePrimArray s Double) -> Double -> ST s ()
subtractScaledFromScratch = alongStretch readPrimArray
{-# NOINLINE subtractScaledFromScratch #-}

-- Only the reader on the left: each of the two above, naming only the reader,
-- takes a copy of the loop made for it, not one that calls a function per entry.
alongStretch :: forall s values. (values -> Int -> ST s Double) -> MutablePrimArray s Double -> Int -> Stretch values -> Double -> ST s ()
alongStretch valueAt = along
  where
    along :: MutablePrimArray s Double -> Int -> Stretch values -> Double -> ST s ()
    along target !offset (Stretch items values (Span from to)) !by = go from
      where
        go :: Int -> ST s ()
        go !at = when (at < to) $ do
            let into = offset + indexPrimArray items at
            value <- valueAt values at
            was <- readPrimArray target into
            writePrimArray target into (was - value * by)
            go (at + 1)
{-# INLINE alongStretch #-}

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
    refusal <- descend asked (sizeofSmallArray (knots factors) - 1) count
    case refusal of
        Just why -> pure (Left why)
        Nothing -> do
            supplied <- MU.unsafeNew count
            forRange 0 count (\place -> readPrimArray asked place >>= MU.unsafeWrite supplied (indexPrimArray (columnAt factors) place))
            Right <$> U.unsafeFreeze supplied
  where
    count :: Int
    count = size factors
    -- From the last knot to the first, each ending where the one after it
    -- starts: a knot only ever asks of earlier ones.
    descend :: MutablePrimArray s Double -> Int -> Int -> ST s (Maybe Unsolvable)
    descend asked !nth !end
        | nth < 0 = pure Nothing
        | otherwise = case indexSmallArray (knots factors) nth of
            Alone inverse -> do
                value <- readPrimArray asked (end - 1)
                when (value /= 0) $ do
                    writePrimArray asked (end - 1) (value * inverse)
                    passOn asked (between factors) (end - 1)
                descend asked (nth - 1) (end - 1)
            Looped held lu -> do
                idle <- nothingAsked asked (Span (end - held) end)
                unless idle $ do
                    solveKnot lu asked (end - held)
                    forRange (end - held) end (passOn asked (between factors))
                descend asked (nth - 1) (end - held)
            Refused held why -> do
                idle <- nothingAsked asked (Span (end - held) end)
                -- Nothing asked of a knot, nothing it has to supply: a demand
                -- that stays clear of a refused knot is answered.
                if idle then descend asked (nth - 1) (end - held) else pure (Just why)

{- | What a place supplies, the earlier knots its column takes from are asked
for their share of.
-}
passOn :: MutablePrimArray s Double -> Links -> Int -> ST s ()
passOn asked (Links from values) place = do
    supplied <- readPrimArray asked place
    when (supplied /= 0) (subtractScaled asked 0 (stretch from values place) supplied)
{-# INLINE passOn #-}

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
    forRange 0 count $ \place -> do
        value <- readPrimArray asked (first + place)
        when (value /= 0) (subtractScaled asked first (stretch (lower lu) (lowerValue lu) place) value)
    ascend (count - 1)
  where
    count :: Int
    count = sizeofPrimArray (reciprocal lu)
    ascend :: Int -> ST s ()
    ascend !place = when (place >= 0) $ do
        value <- readPrimArray asked (first + place)
        when (value /= 0) $ do
            let supplied = value * indexPrimArray (reciprocal lu) place
            writePrimArray asked (first + place) supplied
            subtractScaled asked first (stretch (upper lu) (upperValue lu) place) supplied
        ascend (place - 1)

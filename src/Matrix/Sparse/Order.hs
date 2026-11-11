{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The suite builds the engine unoptimised, and an unoptimised elimination of
-- a real technosphere would take minutes.
{-# OPTIONS_GHC -O2 #-}

{- | In which order to eliminate a square sparse matrix whose pivots stay on
the diagonal, and which entries its factors will then hold.

A supply chain is almost never symmetric: a process buys from a supplier that
buys nothing back. An order chosen on the symmetric pattern cannot see that,
and fills a factor several times more than it needs to. The order here is
chosen on the pattern as it is: each pivot is the node whose row and column,
in what is left to eliminate, would create the fewest new entries.
-}
module Matrix.Sparse.Order (
    Elimination (..),
    eliminate,
    loops,
) where

import Control.Monad (unless, void, when, (>=>))
import Control.Monad.ST (ST, runST)
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import Matrix.Sparse.Arrays (Lists (..), add, filled, forRange, lineCount)

{- | An elimination order and the pattern of the factors it leaves. Places
count the pivots in the order they are eliminated.
-}
data Elimination = Elimination
    { pivotOrder :: !(PrimArray Int)
    -- ^ The node eliminated at each place
    , below :: !Lists
    -- ^ Per place, the later places its column holds once eliminated
    , above :: !Lists
    -- ^ Per place, the earlier places whose row reaches its column, in increasing order
    }

{- | Eliminate the square pattern given by columns, line @j@ listing the rows
column @j@ holds. The diagonal is taken as full whatever the lists say, an
entry listed twice is one entry, and a row outside the square is not part of
the pattern.

The pivot is each time the node with the smallest product of what its row and
its column still hold, the lowest number among equals: that product is the
number of entries the pivot can create. The pattern is kept exactly, so the
lists returned are the factors' own, to the entry.
-}
eliminate :: Lists -> Elimination
eliminate columns = runST $ do
    work <- start columns
    order <- newPrimArray size
    forRange 0 size $ \place -> do
        node <- pop (heap work)
        writePrimArray order place node
        pivot work node
    finish work =<< unsafeFreezePrimArray order
  where
    size :: Int
    size = lineCount columns

{- | The loops of a square pattern given by columns: the largest sets of nodes
in which each leads to every other, a node leading to the rows its column
holds. Line @k@ of the result lists the nodes of one set in increasing order.

A set comes after every set its columns hold a row of. Numbered set after
set, the matrix then has nothing under its diagonal blocks: each set is
solved from the sets after it, and only a set of several nodes needs
eliminating.

A row outside the square is not part of the pattern.
-}
loops :: Lists -> Lists
loops columns = runST $ do
    search <-
        Search
            <$> filled size unreached
            <*> newPrimArray size
            <*> newPrimArray size
            <*> newPrimArray size
            <*> emptyStack size
            <*> emptyStack size
            <*> filled 1 0
            <*> filled 1 0
    forRange 0 size $ \root -> do
        state <- readPrimArray (reached search) root
        when (state == unreached) (enter columns search root >> explore columns search)
    sets <- readPrimArray (setCount search) 0
    -- "The set of each node" turned round reads "the nodes of each set".
    transposed sets . Lists (generatePrimArray (size + 1) id) <$> unsafeFreezePrimArray (setOf search)
  where
    size :: Int
    size = lineCount columns

{- | A walk through the pattern, one path at a time. A node that leads back to
an earlier node of its path is in a loop with it, so each node keeps the
earliest it is known to lead back to; a node that leads back no further than
itself closes a set, made of everything reached since it.
-}
data Search s = Search
    { reached :: !(MutablePrimArray s Int)
    -- ^ Per node: 'unreached', then its rank among the nodes reached, then 'closed'
    , lowest :: !(MutablePrimArray s Int)
    -- ^ Per node reached, the lowest rank it is known to lead back to
    , cursor :: !(MutablePrimArray s Int)
    -- ^ Per node reached, the entry of its column to follow next
    , setOf :: !(MutablePrimArray s Int)
    -- ^ Per node closed, the number of its set
    , path :: !(Stack s)
    -- ^ The nodes being followed, each reached from the one before
    , open :: !(Stack s)
    -- ^ The nodes reached and not closed, in the order they were reached
    , nextRank :: !(MutablePrimArray s Int)
    -- ^ One cell
    , setCount :: !(MutablePrimArray s Int)
    -- ^ One cell
    }

unreached :: Int
unreached = -1

-- | Above every rank, so that leading to a closed node leads back nowhere.
closed :: Int
closed = maxBound

-- | Items piled up, and in one cell how many.
data Stack s = Stack !(MutablePrimArray s Int) !(MutablePrimArray s Int)

emptyStack :: Int -> ST s (Stack s)
emptyStack capacity = Stack <$> newPrimArray capacity <*> filled 1 0

depth :: Stack s -> ST s Int
depth (Stack _ held) = readPrimArray held 0
{-# INLINE depth #-}

push :: Stack s -> Int -> ST s ()
push (Stack items held) item = do
    count <- readPrimArray held 0
    writePrimArray items count item
    writePrimArray held 0 (count + 1)

-- | Take the last item off. The stack must hold one.
pull :: Stack s -> ST s Int
pull (Stack items held) = do
    count <- subtract 1 <$> readPrimArray held 0
    writePrimArray held 0 count
    readPrimArray items count

-- | The last item, left where it is. The stack must hold one.
lastOf :: Stack s -> ST s Int
lastOf (Stack items held) = readPrimArray held 0 >>= readPrimArray items . subtract 1

enter :: Lists -> Search s -> Int -> ST s ()
enter columns search node = do
    rank <- readPrimArray (nextRank search) 0
    writePrimArray (nextRank search) 0 (rank + 1)
    writePrimArray (reached search) node rank
    writePrimArray (lowest search) node rank
    writePrimArray (cursor search) node (max 0 (indexPrimArray (listStart columns) node))
    push (path search) node
    push (open search) node

-- | A node is found to lead back to a rank.
leadsBack :: Search s -> Int -> Int -> ST s ()
leadsBack search node rank = do
    known <- readPrimArray (lowest search) node
    when (rank < known) (writePrimArray (lowest search) node rank)
{-# INLINE leadsBack #-}

{- | Follow the path to its end, entry after entry. Kept as a loop over the
path held in an array: a supply chain is thousands of processes deep, deeper
than a recursion should go.
-}
explore :: forall s. Lists -> Search s -> ST s ()
explore columns search = go
  where
    size :: Int
    size = lineCount columns
    go :: ST s ()
    go = do
        left <- depth (path search)
        when (left > 0) $ do
            node <- lastOf (path search)
            at <- readPrimArray (cursor search) node
            if at < min (sizeofPrimArray (listItem columns)) (indexPrimArray (listStart columns) (node + 1))
                then do
                    writePrimArray (cursor search) node (at + 1)
                    follow node (indexPrimArray (listItem columns) at)
                else leave search node
            go
    follow :: Int -> Int -> ST s ()
    follow node next = when (next >= 0 && next < size) $ do
        state <- readPrimArray (reached search) next
        if state == unreached then enter columns search next else leadsBack search node state

{- | A node has nothing left to follow. It leaves the path, and the node before
it leads back wherever it does.
-}
leave :: forall s. Search s -> Int -> ST s ()
leave search node = do
    void (pull (path search))
    back <- readPrimArray (lowest search) node
    left <- depth (path search)
    when (left > 0) (lastOf (path search) >>= \previous -> leadsBack search previous back)
    rank <- readPrimArray (reached search) node
    when (back == rank) $ do
        set <- readPrimArray (setCount search) 0
        writePrimArray (setCount search) 0 (set + 1)
        close set
  where
    close :: Int -> ST s ()
    close set = do
        member <- pull (open search)
        writePrimArray (reached search) member closed
        writePrimArray (setOf search) member set
        unless (member == node) (close set)

{- | The entries of each line still to eliminate, one array per line so that a
line can grow without moving the others. A line keeps the members eliminated
since it was last read: sweeping them out is left to whoever reads it next.
-}
data Lines s = Lines
    { lineItems :: !(SmallMutableArray s (MutablePrimArray s Int))
    , lineLength :: !(MutablePrimArray s Int)
    }

-- | The first so many members of the array of a line.
data Line s = Line !(MutablePrimArray s Int) !Int

-- | The pattern read one way, by rows or by columns.
data Side s = Side
    { sideLines :: !(Lines s)
    , sideCount :: !(MutablePrimArray s Int)
    -- ^ Per line, how many of its members are still to eliminate
    }

{- | Every entry is held twice, by its row and by its column: a view reads the
pattern one way and names the other, which has to hear of every change.
-}
data View s = View
    { along :: !(Side s)
    , across :: !(Side s)
    }

data Work s = Work
    { byRow :: !(View s)
    , byColumn :: !(View s)
    , mark :: !(MutablePrimArray s Int)
    -- ^ Per node: 'gone' once eliminated, otherwise the last tag it was given
    , nextTag :: !(MutablePrimArray s Int)
    -- ^ One cell: the first tag no node carries yet
    , heap :: !(Heap s)
    }

-- | The mark of an eliminated node, below every tag.
gone :: Int
gone = -1

forLine :: Line s -> (Int -> ST s ()) -> ST s ()
forLine (Line items count) body = forRange 0 count (readPrimArray items >=> body)
{-# INLINE forLine #-}

-- | The array of a line, with room for so many members after those it holds.
roomFor :: Lines s -> Int -> Int -> ST s (MutablePrimArray s Int)
roomFor ls line extra = do
    held <- readSmallArray (lineItems ls) line
    len <- readPrimArray (lineLength ls) line
    capacity <- getSizeofMutablePrimArray held
    if len + extra <= capacity
        then pure held
        else do
            -- Doubling keeps the copies of a line that grows one member at a
            -- time proportional to its final length.
            larger <- newPrimArray (max (2 * len) (len + extra))
            copyMutablePrimArray larger 0 held 0 len
            writeSmallArray (lineItems ls) line larger
            pure larger

append :: Lines s -> Int -> Int -> ST s ()
append ls line member = do
    held <- roomFor ls line 1
    len <- readPrimArray (lineLength ls) line
    writePrimArray held len member
    writePrimArray (lineLength ls) line (len + 1)

{- | The pattern off the diagonal, by rows and by columns, and every node
ranked by what eliminating it first would cost.
-}
start :: forall s. Lists -> ST s (Work s)
start columns = do
    rowLength <- filled size 0
    columnLength <- filled size 0
    seenIn <- filled size (-1)
    emptyLine <- newPrimArray 0
    columnItems <- newSmallArray size emptyLine
    forRange 0 size $ \column -> do
        -- Clamped, so that lists that do not hold together cost a wrong
        -- pattern and never a read outside the arrays.
        let from = max 0 (indexPrimArray (listStart columns) column)
            to = min (sizeofPrimArray (listItem columns)) (indexPrimArray (listStart columns) (column + 1))
        held <- newPrimArray (max 0 (to - from))
        writeSmallArray columnItems column held
        forRange from to $ \at -> do
            let row = indexPrimArray (listItem columns) at
            fresh <- if row < 0 || row >= size || row == column then pure False else (/= column) <$> readPrimArray seenIn row
            when fresh $ do
                writePrimArray seenIn row column
                len <- readPrimArray columnLength column
                writePrimArray held len row
                writePrimArray columnLength column (len + 1)
                add rowLength row 1
    rowItems <- newSmallArray size emptyLine
    forRange 0 size $ \row -> readPrimArray rowLength row >>= newPrimArray >>= writeSmallArray rowItems row
    rowFilled <- filled size 0
    let rows = Lines rowItems rowFilled
        columnLines = Lines columnItems columnLength
    forRange 0 size $ \column -> do
        held <- readSmallArray columnItems column
        len <- readPrimArray columnLength column
        forLine (Line held len) (\row -> append rows row column)
    columnCount <- cloneMutablePrimArray columnLength 0 size
    let rowSide = Side rows rowLength
        columnSide = Side columnLines columnCount
    work <- Work (View rowSide columnSide) (View columnSide rowSide) <$> filled size 0 <*> filled 1 1 <*> emptyHeap size
    forRange 0 size (cost work >=> insert (heap work))
    pure work
  where
    size :: Int
    size = lineCount columns

-- | What a node would cost as the next pivot, the entries it can create.
cost :: Work s -> Int -> ST s Ranked
cost work node = do
    inRow <- readPrimArray (sideCount (along (byRow work))) node
    inColumn <- readPrimArray (sideCount (along (byColumn work))) node
    pure (Ranked (inRow * inColumn) node)
{-# INLINE cost #-}

{- | Eliminate a node: every row under it in its column receives every column
its row holds.
-}
pivot :: Work s -> Int -> ST s ()
pivot work node = do
    writePrimArray (mark work) node gone
    under@(Line _ rowsUnder) <- detach work (byColumn work) node
    right@(Line _ columnsRight) <- detach work (byRow work) node
    unless (rowsUnder == 0 || columnsRight == 0) $ do
        -- The same entries can be found from either side: read the lines
        -- that are the shorter to read.
        throughRows <- reach (along (byRow work)) under
        throughColumns <- reach (along (byColumn work)) right
        spread work $
            if throughRows <= throughColumns
                then Fill (byRow work) under right
                else Fill (byColumn work) right under
    forLine under (cost work >=> rerank (heap work))
    forLine right (cost work >=> rerank (heap work))

{- | Take the line of an eliminated node out of the pattern. What it still
holds is what the pivot leaves in the factors, and each line crossing it there
counts one member fewer.
-}
detach :: forall s. Work s -> View s -> Int -> ST s (Line s)
detach work view node = do
    held <- readSmallArray (lineItems ls) node
    len <- readPrimArray (lineLength ls) node
    kept <- sweep held len 0 0
    writePrimArray (lineLength ls) node kept
    pure (Line held kept)
  where
    ls :: Lines s
    ls = sideLines (along view)
    sweep :: MutablePrimArray s Int -> Int -> Int -> Int -> ST s Int
    sweep held !len !from !to
        | from >= len = pure to
        | otherwise = do
            member <- readPrimArray held from
            state <- readPrimArray (mark work) member
            if state == gone
                then sweep held len (from + 1) to
                else do
                    writePrimArray held to member
                    add (sideCount (across view)) member (-1)
                    sweep held len (from + 1) (to + 1)

-- | How much there is to read in the lines a line names.
reach :: forall s. Side s -> Line s -> ST s Int
reach side (Line items count) = go 0 0
  where
    go :: Int -> Int -> ST s Int
    go !at !total
        | at >= count = pure total
        | otherwise = do
            len <- readPrimArray items at >>= readPrimArray (lineLength (sideLines side))
            go (at + 1) (total + len)

{- | One way of adding what a pivot fills in: read along the view, each line
the first list names takes the members of the second it lacks.
-}
data Fill s = Fill !(View s) !(Line s) !(Line s)

spread :: Work s -> Fill s -> ST s ()
spread work fill@(Fill _ (Line _ takerCount) offered) = do
    tag <- readPrimArray (nextTag work) 0
    -- One tag for "offered", then one per taker for "offered and already held".
    writePrimArray (nextTag work) 0 (tag + takerCount + 1)
    forLine offered (\member -> writePrimArray (mark work) member tag)
    forRange 0 takerCount (extend work fill tag)

{- | One taker takes what it lacks. Its eliminated members are swept out on the
way, and each offered member it already holds is tagged as held, so that the
second pass adds only the others.
-}
extend :: forall s. Work s -> Fill s -> Int -> Int -> ST s ()
extend work (Fill view (Line takerItems _) (Line offered offeredCount)) tag nth = do
    taker <- readPrimArray takerItems nth
    held <- readSmallArray (lineItems ls) taker
    len <- readPrimArray (lineLength ls) taker
    kept <- sweep held len 0 0
    writePrimArray (lineLength ls) taker kept
    roomy <- roomFor ls taker offeredCount
    extended <- absorb roomy taker 0 kept
    writePrimArray (lineLength ls) taker extended
    add (sideCount (along view)) taker (extended - kept)
  where
    ls :: Lines s
    ls = sideLines (along view)
    heldTag :: Int
    heldTag = tag + 1 + nth
    sweep :: MutablePrimArray s Int -> Int -> Int -> Int -> ST s Int
    sweep held !len !from !to
        | from >= len = pure to
        | otherwise = do
            member <- readPrimArray held from
            state <- readPrimArray (mark work) member
            if state == gone
                then sweep held len (from + 1) to
                else do
                    when (state >= tag) (writePrimArray (mark work) member heldTag)
                    writePrimArray held to member
                    sweep held len (from + 1) (to + 1)
    absorb :: MutablePrimArray s Int -> Int -> Int -> Int -> ST s Int
    absorb roomy taker !at !to
        | at >= offeredCount = pure to
        | otherwise = do
            member <- readPrimArray offered at
            state <- readPrimArray (mark work) member
            if member == taker || state == heldTag
                then absorb roomy taker (at + 1) to
                else do
                    writePrimArray roomy to member
                    append (sideLines (across view)) member taker
                    add (sideCount (across view)) member 1
                    absorb roomy taker (at + 1) (to + 1)

{- | The lists of the factors, from the lines the eliminated nodes were left
with: nodes renamed to their places, and the rows' lists turned round so that
each column names the earlier pivots that reach it.
-}
finish :: forall s. Work s -> PrimArray Int -> ST s Elimination
finish work order = do
    place <- newPrimArray size
    forRange 0 size (\at -> writePrimArray place (indexPrimArray order at) at)
    placeOf <- unsafeFreezePrimArray place
    under <- gather (sideLines (along (byColumn work))) order
    right <- gather (sideLines (along (byRow work))) order
    pure (Elimination order (renamed placeOf under) (transposed size (renamed placeOf right)))
  where
    size :: Int
    size = sizeofPrimArray order

-- | The lines of the nodes, in the order given, end to end.
gather :: forall s. Lines s -> PrimArray Int -> ST s Lists
gather ls order = do
    starts <- newPrimArray (size + 1)
    writePrimArray starts 0 0
    forRange 0 size $ \at -> do
        len <- readPrimArray (lineLength ls) (indexPrimArray order at)
        readPrimArray starts at >>= writePrimArray starts (at + 1) . (+ len)
    items <- newPrimArray =<< readPrimArray starts size
    forRange 0 size $ \at -> do
        let node = indexPrimArray order at
        held <- readSmallArray (lineItems ls) node
        len <- readPrimArray (lineLength ls) node
        from <- readPrimArray starts at
        copyMutablePrimArray items from held 0 len
    Lists <$> unsafeFreezePrimArray starts <*> unsafeFreezePrimArray items
  where
    size :: Int
    size = sizeofPrimArray order

renamed :: PrimArray Int -> Lists -> Lists
renamed names (Lists starts items) = Lists starts (mapPrimArray (indexPrimArray names) items)

{- | The same entries read the other way: line @v@ of the result names, in
increasing order, the lines that held @v@. Every item has to name one of the
@width@ lines of the result.
-}
transposed :: Int -> Lists -> Lists
transposed width (Lists starts items) = runST $ do
    turnedStart <- filled (width + 1) 0
    forRange 0 (sizeofPrimArray items) (\at -> add turnedStart (indexPrimArray items at + 1) 1)
    forRange 0 width (\v -> readPrimArray turnedStart v >>= add turnedStart (v + 1))
    next <- cloneMutablePrimArray turnedStart 0 width
    turned <- newPrimArray (sizeofPrimArray items)
    forRange 0 (sizeofPrimArray starts - 1) $ \line ->
        forRange (indexPrimArray starts line) (indexPrimArray starts (line + 1)) $ \at -> do
            let v = indexPrimArray items at
            to <- readPrimArray next v
            writePrimArray turned to line
            writePrimArray next v (to + 1)
    Lists <$> unsafeFreezePrimArray turnedStart <*> unsafeFreezePrimArray turned

{- | The nodes still to eliminate, the cheapest on top: least cost first, least
number first at equal cost, so that the order owes nothing to how the heap
happened to be arranged.
-}
data Heap s = Heap
    { slotCost :: !(MutablePrimArray s Int)
    , slotNode :: !(MutablePrimArray s Int)
    , nodeSlot :: !(MutablePrimArray s Int)
    , heapSize :: !(MutablePrimArray s Int)
    -- ^ One cell
    }

-- | A node and what it costs.
data Ranked = Ranked !Int !Int

before :: Ranked -> Ranked -> Bool
before (Ranked costA a) (Ranked costB b) = costA < costB || (costA == costB && a < b)
{-# INLINE before #-}

emptyHeap :: Int -> ST s (Heap s)
emptyHeap capacity = Heap <$> newPrimArray capacity <*> newPrimArray capacity <*> newPrimArray capacity <*> filled 1 0

settle :: Heap s -> Int -> Ranked -> ST s ()
settle h slot (Ranked value node) = do
    writePrimArray (slotCost h) slot value
    writePrimArray (slotNode h) slot node
    writePrimArray (nodeSlot h) node slot
{-# INLINE settle #-}

ranked :: Heap s -> Int -> ST s Ranked
ranked h slot = Ranked <$> readPrimArray (slotCost h) slot <*> readPrimArray (slotNode h) slot
{-# INLINE ranked #-}

-- | Place an entry at a slot or above, wherever its rank puts it.
rise :: Heap s -> Int -> Ranked -> ST s ()
rise h !slot entry
    | slot == 0 = settle h 0 entry
    | otherwise = do
        let parent = (slot - 1) `quot` 2
        over <- ranked h parent
        if entry `before` over
            then settle h slot over >> rise h parent entry
            else settle h slot entry

-- | Place an entry at a slot or below, wherever its rank puts it.
sink :: forall s. Heap s -> Int -> Ranked -> ST s ()
sink h start0 entry = readPrimArray (heapSize h) 0 >>= go start0
  where
    go :: Int -> Int -> ST s ()
    go !slot !size
        | left >= size = settle h slot entry
        | otherwise = do
            l <- ranked h left
            r <- if left + 1 < size then ranked h (left + 1) else pure l
            let (child, least) = if r `before` l then (left + 1, r) else (left, l)
            if least `before` entry
                then settle h slot least >> go child size
                else settle h slot entry
      where
        left :: Int
        left = 2 * slot + 1

insert :: Heap s -> Ranked -> ST s ()
insert h entry = do
    size <- readPrimArray (heapSize h) 0
    writePrimArray (heapSize h) 0 (size + 1)
    rise h size entry

-- | The cost of a node in the heap changed: move it to its rank.
rerank :: Heap s -> Ranked -> ST s ()
rerank h entry@(Ranked value node) = do
    slot <- readPrimArray (nodeSlot h) node
    was <- readPrimArray (slotCost h) slot
    if value < was then rise h slot entry else when (value > was) (sink h slot entry)

-- | Take the cheapest node out. The heap must hold one.
pop :: Heap s -> ST s Int
pop h = do
    size <- subtract 1 <$> readPrimArray (heapSize h) 0
    top <- readPrimArray (slotNode h) 0
    final <- ranked h size
    writePrimArray (heapSize h) 0 size
    when (size > 0) (sink h 0 final)
    pure top

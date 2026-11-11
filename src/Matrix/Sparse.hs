{-# LANGUAGE OverloadedStrings #-}

{- | The sparse solver, written in Rust and linked into the engine as a static
archive.

A matrix is factorized once, and its factors then solve any number of
right-hand sides. The factors are a block of bytes allocated on this side and
never written again: the garbage collector counts them in the heap and frees
them once no request holds them, and any number of threads solve with the same
ones at once.
-}
module Matrix.Sparse (
    Factors,
    Report (..),
    Pivot (..),
    report,
    Refused (..),
    factorize,
    solve,
    probe,
) where

import Control.Exception (mask_, onException)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, peekElemOff, pokeElemOff)
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import Types (SparseTriple (..))

-- | The factors of a matrix the solver accepted.
data Factors = Factors
    { factorBlock :: !(ForeignPtr Word8)
    -- ^ The factors as the solver wrote them
    , factorSize :: !Int
    -- ^ The size of the matrix
    , factorReport :: !Report
    }

-- | What a factorization measured on its way to its factors.
data Report = Report
    { factorBytes :: !Int
    -- ^ The memory the factors take
    , denseColumns :: !Int
    -- ^ How many columns were eliminated as one full block
    , weakestPivot :: !(Maybe Pivot)
    {- ^ The pivot that kept the smallest share of the diagonal entry it
    started from: how close the matrix came to a 'TinyPivot'. None for a
    matrix without columns.
    -}
    , trialError :: !Double
    -- ^ The share of a row that a trial solve left unexplained
    }
    deriving (Eq, Show)

-- | A pivot the elimination divided by.
data Pivot = Pivot
    { pivotColumn :: !Int
    -- ^ Its column, counted from 0
    , pivotShare :: !Double
    -- ^ What it kept of the diagonal entry it started from
    }
    deriving (Eq, Show)

-- | What the factorization that made these factors measured.
report :: Factors -> Report
report = factorReport

-- | Why a matrix has no factors: a verdict on the matrix, the same at every try.
data Refused
    = {- | Elimination leaves this on the diagonal of this column (counted from
      0), too little to divide by: the matrix is singular, or as good as.
      -}
      TinyPivot !Int !Double
    | -- | A trial solve with the factors left this share of a row unexplained.
      Unreliable !Double
    deriving (Eq, Show)

-- | Factors the solver still owns, to export or to discard once.
data Owned

-- | What a factorization says beyond its code: six fields of eight bytes.
data Found

foreign import ccall safe "volca_lu_factorize"
    c_factorize :: CSize -> CSize -> Ptr Word32 -> Ptr Word32 -> Ptr Double -> Ptr Found -> IO Word32

foreign import ccall safe "volca_lu_export"
    c_export :: Ptr Owned -> Ptr Word8 -> CSize -> IO Word32

foreign import ccall safe "volca_lu_discard"
    c_discard :: Ptr Owned -> IO ()

foreign import ccall safe "volca_lu_scratch_len"
    c_scratchLen :: CSize -> CSize -> CSize

foreign import ccall safe "volca_lu_solve"
    c_solve :: Ptr Word8 -> CSize -> Ptr Double -> CSize -> Ptr Double -> CSize -> IO Word32

foreign import ccall safe "volca_solver_probe" c_probe :: IO Word32

{- | Factorize the @n@×@n@ matrix holding these entries, one given several
times counting for the sum.

The outer 'Left' is a computation that did not happen (no memory, an entry
outside the matrix) and says nothing lasting; the inner one is the solver's
verdict on the matrix.
-}
factorize :: Int -> U.Vector SparseTriple -> IO (Either Text (Either Refused Factors))
factorize n triples
    | n < 0 = pure (Left "a matrix cannot have a negative size")
    | otherwise =
        allocaArray count $ \rows -> allocaArray count $ \cols -> allocaArray count $ \vals -> do
            -- A negative index becomes one above two billion, which the solver refuses as outside the matrix.
            U.imapM_ (\k (SparseTriple i j v) -> pokeElemOff rows k (fromIntegral i) >> pokeElemOff cols k (fromIntegral j) >> pokeElemOff vals k v) triples
            -- Masked from the call to the export: a request cancelled in between would leave factors nobody frees.
            allocaBytes 48 $ \found -> mask_ $ do
                code <- c_factorize (fromIntegral n) (fromIntegral count) rows cols vals found
                at <- peekByteOff found 16 :: IO Word64
                value <- peekByteOff found 24
                case code of
                    0 -> do
                        owned <- peekByteOff found 0
                        bytes <- fromIntegral <$> (peekByteOff found 8 :: IO Word64)
                        dense <- fromIntegral <$> (peekByteOff found 32 :: IO Word64)
                        trial <- peekByteOff found 40
                        block <- mallocPlainForeignPtrAlignedBytes bytes 8 `onException` c_discard owned
                        exported <- withForeignPtr block $ \p -> c_export owned p (fromIntegral bytes)
                        let weakest = if n == 0 then Nothing else Just Pivot{pivotColumn = fromIntegral at, pivotShare = value}
                            measured = Report{factorBytes = bytes, denseColumns = dense, weakestPivot = weakest, trialError = trial}
                        pure $ if exported == 0 then Right (Right Factors{factorBlock = block, factorSize = n, factorReport = measured}) else Left (failure exported at)
                    1 -> pure (Right (Left (TinyPivot (fromIntegral at) value)))
                    2 -> pure (Right (Left (Unreliable value)))
                    _ -> pure (Left (failure code at))
  where
    count = U.length triples

{- | Solve for each right-hand side, in the order given. Reads the factors
without writing to them: requests solve with the same ones at the same time.
-}
solve :: Factors -> [U.Vector Double] -> IO (Either Text [U.Vector Double])
solve Factors{factorBlock = block, factorSize = n, factorReport = Report{factorBytes = bytes}} rhs
    | any ((/= n) . U.length) rhs = pure (Left ("a right-hand side needs one number for each of the " <> T.pack (show n) <> " columns of the matrix"))
    | otherwise =
        -- The engine's vectors can move while foreign code runs: the numbers go through memory that cannot.
        allocaArray (n * columns) $ \numbers -> allocaArray scratch $ \space -> do
            for_ (zip starts rhs) $ \(start, column) -> U.imapM_ (pokeElemOff numbers . (start +)) column
            code <- withForeignPtr block $ \factors ->
                c_solve factors (fromIntegral bytes) numbers (fromIntegral (n * columns)) space (fromIntegral scratch)
            if code == 0
                then Right <$> traverse (\start -> U.generateM n (peekElemOff numbers . (start +))) (take columns starts)
                else pure (Left (failure code 0))
  where
    columns = length rhs
    starts = [0, n ..]
    scratch = fromIntegral (c_scratchLen (fromIntegral n) (fromIntegral columns))

{- | Whether the archive is sound in this program: its code runs, its allocator
answers, and a panic stops at the boundary instead of killing the engine. On a
failure, which of these did not hold.
-}
probe :: IO (Either Text ())
probe = verdict <$> c_probe
  where
    verdict :: Word32 -> Either Text ()
    verdict 0 = Right ()
    verdict 9 = Left "the solver solved a known system wrongly"
    verdict 10 = Left "an unwinding the solver started was not caught where it should have been"
    verdict code = Left (failure code 0)

{- | What a code of the solver says when it is neither an answer nor a verdict
on the matrix, with the position that came with it. The codes are those of
@solver/ffi/src/lib.rs@: the two lists change together.
-}
failure :: Word32 -> Word64 -> Text
failure 3 _ = "the solver ran out of memory"
failure 4 _ = "the matrix is too large for the solver"
failure 5 at = "entry " <> T.pack (show at) <> " of the matrix names a row or a column outside it, or holds no finite number"
failure 6 _ = "the solver does not recognise the factors it is given"
failure 7 _ = "the solver was given lists whose lengths do not go together"
failure 8 _ = "the solver panicked"
failure code _ = "the solver answered a code this engine does not know: " <> T.pack (show code)

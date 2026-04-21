module Numerical.MUMPS.Types (
    MUMPSSolver (..),
    MUMPSError (..),
) where

import Foreign.Ptr (Ptr)
import Numerical.MUMPS.FFI (MumpsSolverC)

-- | Handle to a MUMPS solver instance. Owns the C-level memory.
data MUMPSSolver = MUMPSSolver
    { solverPtr :: !(Ptr MumpsSolverC)
    , solverSize :: !Int
    -- ^ Matrix dimension n
    }

-- | MUMPS error with the INFOG(1) code
newtype MUMPSError = MUMPSError Int
    deriving (Show)

instance Show MUMPSSolver where
    show s = "MUMPSSolver{n=" ++ show (solverSize s) ++ "}"

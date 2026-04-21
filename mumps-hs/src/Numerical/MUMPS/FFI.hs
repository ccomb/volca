module Numerical.MUMPS.FFI (
    MumpsSolverC,
    c_mumps_create,
    c_mumps_analyze,
    c_mumps_factorize,
    c_mumps_solve,
    c_mumps_solve_multi,
    c_mumps_destroy,
    c_mumps_get_error,
) where

import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Ptr (Ptr)

-- | Opaque C struct — never allocated from Haskell
data MumpsSolverC

foreign import ccall unsafe "mumps_create"
    c_mumps_create :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO (Ptr MumpsSolverC)

foreign import ccall unsafe "mumps_analyze"
    c_mumps_analyze :: Ptr MumpsSolverC -> IO CInt

foreign import ccall unsafe "mumps_factorize"
    c_mumps_factorize :: Ptr MumpsSolverC -> IO CInt

foreign import ccall safe "mumps_solve"
    c_mumps_solve :: Ptr MumpsSolverC -> Ptr CDouble -> Ptr CDouble -> IO CInt

foreign import ccall safe "mumps_solve_multi"
    c_mumps_solve_multi :: Ptr MumpsSolverC -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "mumps_destroy"
    c_mumps_destroy :: Ptr MumpsSolverC -> IO ()

foreign import ccall unsafe "mumps_get_error"
    c_mumps_get_error :: Ptr MumpsSolverC -> IO CInt

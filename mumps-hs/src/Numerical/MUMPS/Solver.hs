module Numerical.MUMPS.Solver (
    mumpsCreate,
    mumpsAnalyze,
    mumpsFactorize,
    mumpsSolve,
    mumpsSolveMulti,
    mumpsDestroy,
    mumpsAnalyzeAndFactorize,
    withMUMPSSolver,
) where

import Control.Exception (bracket, throwIO)
import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peekElemOff)

import Numerical.MUMPS.FFI
import Numerical.MUMPS.Types

-- | Create a MUMPS solver from COO triplets (0-indexed row/col).
mumpsCreate :: Int -> Int -> [Int] -> [Int] -> [Double] -> IO MUMPSSolver
mumpsCreate n nnz rows cols vals = do
    let cRows = map fromIntegral rows :: [CInt]
        cCols = map fromIntegral cols :: [CInt]
        cVals = map realToFrac vals :: [CDouble]
    ptr <- withArray cRows $ \pRows ->
        withArray cCols $ \pCols ->
            withArray cVals $ \pVals ->
                c_mumps_create (fromIntegral n) (fromIntegral nnz) pRows pCols pVals
    if ptr == nullPtr
        then throwIO $ userError "MUMPS: allocation failed"
        else do
            err <- c_mumps_get_error ptr
            if err < 0
                then do
                    c_mumps_destroy ptr
                    throwIO $ userError $ "MUMPS init failed: INFOG(1) = " ++ show err
                else return $ MUMPSSolver ptr n

-- | Run symbolic analysis phase.
mumpsAnalyze :: MUMPSSolver -> IO ()
mumpsAnalyze s = do
    rc <- c_mumps_analyze (solverPtr s)
    checkError "analyze" rc

-- | Run numerical factorization phase.
mumpsFactorize :: MUMPSSolver -> IO ()
mumpsFactorize s = do
    rc <- c_mumps_factorize (solverPtr s)
    checkError "factorize" rc

{- | Solve Ax = b. Takes RHS vector, returns solution vector.
Can be called repeatedly after factorization with different RHS.
-}
mumpsSolve :: MUMPSSolver -> VS.Vector Double -> IO (VS.Vector Double)
mumpsSolve s rhs = do
    let n = solverSize s
    VS.unsafeWith (VS.map realToFrac rhs :: VS.Vector CDouble) $ \pRhs ->
        allocaArray n $ \pSol -> do
            rc <- c_mumps_solve (solverPtr s) pRhs pSol
            checkError "solve" rc
            VS.generateM n $ \i -> realToFrac <$> peekElemOff pSol i

{- | Solve A X = B for multiple RHS in one MUMPS call.
Input is a list of k RHS vectors each of length n; output is the matching
list of k solution vectors. Empty input returns empty output.
One MUMPS triangular-solve call amortizes setup across all k vectors.
-}
mumpsSolveMulti :: MUMPSSolver -> [VS.Vector Double] -> IO [VS.Vector Double]
mumpsSolveMulti _ [] = pure []
mumpsSolveMulti s rhss = do
    let n = solverSize s
        k = length rhss
        total = n * k
        packed :: VS.Vector CDouble
        packed = VS.concat (map (VS.map realToFrac) rhss)
    VS.unsafeWith packed $ \pRhs ->
        allocaArray total $ \pSol -> do
            rc <- c_mumps_solve_multi (solverPtr s) (fromIntegral k) pRhs pSol
            checkError "solveMulti" rc
            flat <- VS.generateM total $ \i -> realToFrac <$> peekElemOff pSol i
            pure [VS.slice (col * n) n flat | col <- [0 .. k - 1]]

-- | Destroy the solver and release all memory.
mumpsDestroy :: MUMPSSolver -> IO ()
mumpsDestroy = c_mumps_destroy . solverPtr

-- | Convenience: analyze + factorize in one call.
mumpsAnalyzeAndFactorize :: MUMPSSolver -> IO ()
mumpsAnalyzeAndFactorize s = mumpsAnalyze s >> mumpsFactorize s

-- | Bracket-style: create, use, destroy.
withMUMPSSolver :: Int -> Int -> [Int] -> [Int] -> [Double] -> (MUMPSSolver -> IO a) -> IO a
withMUMPSSolver n nnz rows cols vals =
    bracket (mumpsCreate n nnz rows cols vals) mumpsDestroy

checkError :: String -> CInt -> IO ()
checkError phase rc
    | rc < 0 = throwIO $ userError $ "MUMPS " ++ phase ++ " failed: INFOG(1) = " ++ show rc
    | otherwise = return ()

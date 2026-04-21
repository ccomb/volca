{- | Haskell bindings for MUMPS sequential direct sparse solver.

Provides a thin, safe wrapper around MUMPS_SEQ for solving
sparse linear systems via LU factorization.
-}
module Numerical.MUMPS (
    -- * Types
    MUMPSSolver,
    MUMPSError (..),

    -- * Lifecycle
    mumpsCreate,
    mumpsDestroy,
    withMUMPSSolver,

    -- * Phases
    mumpsAnalyze,
    mumpsFactorize,
    mumpsAnalyzeAndFactorize,

    -- * Solve
    mumpsSolve,
    mumpsSolveMulti,
) where

import Numerical.MUMPS.Solver
import Numerical.MUMPS.Types (MUMPSError (..), MUMPSSolver)

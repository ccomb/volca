-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2024-present Christophe Combelles

{- | Haskell bindings for MUMPS sequential direct sparse solver.

Provides a thin, safe wrapper around MUMPS_SEQ for solving
sparse linear systems via LU factorization.
-}
module Numerical.MUMPS (
    -- * Types
    MUMPSSolver,

    -- * Lifecycle
    mumpsCreate,
    mumpsDestroy,

    -- * Phases
    mumpsAnalyze,
    mumpsFactorize,
    mumpsAnalyzeAndFactorize,

    -- * Solve
    mumpsSolve,
    mumpsSolveMulti,

    -- * BLAS
    mumpsBlasThreads,
) where

import Numerical.MUMPS.Solver
import Numerical.MUMPS.Types (MUMPSSolver)

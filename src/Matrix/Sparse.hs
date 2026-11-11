{-# LANGUAGE OverloadedStrings #-}

{- | The sparse solver, written in Rust and linked into the engine as a static
archive.

For now this module only asks that archive whether it works where it has been
linked, which a build that succeeds does not show.
-}
module Matrix.Sparse (
    probe,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

foreign import ccall safe "volca_solver_probe" c_probe :: IO Word32

{- | Whether the archive is sound in this program: the code of its dependency
runs, its allocator answers, and a panic stops at the boundary instead of
killing the engine. On a failure, which of these did not hold.
-}
probe :: IO (Either Text ())
probe = verdict <$> c_probe
  where
    verdict :: Word32 -> Either Text ()
    verdict 0 = Right ()
    verdict 1 = Left "the solver ordered a known matrix wrongly"
    verdict 2 = Left "an unwinding the solver started was not caught where it should have been"
    verdict 3 = Left "the solver panicked"
    verdict code = Left ("the solver answered a code this engine does not know: " <> T.pack (show code))

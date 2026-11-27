module SpecHook (hook) where

import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import System.IO.Temp (createTempDirectory)
import Test.Hspec (Spec, beforeAll_)

{- | The whole run writes into a data directory of its own: the engine keeps
files there (the dependencies chosen for a database among them) that would
otherwise land in the working tree, and leak from one run into the next.
-}
hook :: Spec -> Spec
hook = beforeAll_ (getTemporaryDirectory >>= (`createTempDirectory` "volca-spec-data") >>= setEnv "VOLCA_DATA_DIR")

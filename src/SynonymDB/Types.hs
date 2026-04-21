{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- | Types for the synonym database used for flow matching.
module SynonymDB.Types (
    SynonymDB (..),
    emptySynonymDB,
) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Store (Store)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Synonym database with bidirectional lookups.

- @synNameToId@: Maps normalized flow names to synonym group IDs
- @synIdToNames@: Maps group IDs back to all names in that group
-}
data SynonymDB = SynonymDB
    { synNameToId :: !(Map Text Int)
    , synIdToNames :: !(Map Int [Text])
    }
    deriving (Eq, Show, Generic, NFData, Store)

-- | Empty synonym database
emptySynonymDB :: SynonymDB
emptySynonymDB = SynonymDB M.empty M.empty

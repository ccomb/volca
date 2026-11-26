{- | The contributions of a score gathered by substance: the flows of one name
and one medium make one line.

A database files one substance under several flows, one per sub-compartment
("Carbon dioxide, fossil" to urban air, to rural air, to the stratosphere),
and a list of flows splits what reads as one emission across rows. The name is
compared with its case folded and its ends trimmed, as a flow search reads it.
Two databases that spell a substance differently keep two lines: the grouping
can miss a pair, never join two substances. The CAS number is not the key,
since fossil and biogenic carbon dioxide share it.
-}
module Impact.Substances (
    SubstanceContribution (..),
    bySubstance,
    substanceCompartments,
    substanceFlowIds,
) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.UUID as UUID

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Data.Indexing (groupInOrder)
import Method.Mapping (FlowContribution (..))
import Types (BiosphereFlow (..), Compartment (..), Medium, bfCompartmentSub)

-- | One substance in one medium, and what its flows add to the score.
data SubstanceContribution = SubstanceContribution
    { scName :: !Text
    -- ^ As its largest flow spells it
    , scMedium :: !(Maybe Medium)
    , scFlows :: !(NonEmpty FlowContribution)
    , scContribution :: !Double
    }

-- | What makes two flows one substance.
data SubstanceKey = SubstanceKey
    { skName :: !Text
    , skMedium :: !(Maybe Medium)
    }
    deriving (Eq, Ord)

{- | One line per substance and medium, in no particular order: 'Impact.Ranking.rankRows'
orders them. The flows of a line come largest first in absolute value, whatever
order they were given in.
-}
bySubstance :: [FlowContribution] -> [SubstanceContribution]
bySubstance = map line . M.elems . groupInOrder . map keyed . sortOn (Down . abs . fcContribution)
  where
    keyed :: FlowContribution -> (SubstanceKey, FlowContribution)
    keyed row = (keyOf (fcFlow row), row)
    keyOf :: BiosphereFlow -> SubstanceKey
    keyOf f = SubstanceKey{skName = T.toCaseFold (T.strip (bfName f)), skMedium = compartmentName <$> bfCompartment f}
    line :: NonEmpty FlowContribution -> SubstanceContribution
    line rows@(first :| _) =
        SubstanceContribution
            { scName = bfName (fcFlow first)
            , scMedium = compartmentName <$> bfCompartment (fcFlow first)
            , scFlows = rows
            , scContribution = sum (NE.map fcContribution rows)
            }

-- | The sub-compartments of a substance's flows, each once.
substanceCompartments :: SubstanceContribution -> [Text]
substanceCompartments = nubOrd . mapMaybe (bfCompartmentSub . fcFlow) . NE.toList . scFlows

-- | A substance's flows, by UUID.
substanceFlowIds :: SubstanceContribution -> [Text]
substanceFlowIds = map (UUID.toText . bfId . fcFlow) . NE.toList . scFlows

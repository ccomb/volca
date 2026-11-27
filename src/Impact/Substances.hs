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
    SubstanceGrid (..),
    Spelling (..),
    PartRow (..),
    substanceGrid,
    substancesNamed,
    substanceCompartments,
    substanceFlowIds,
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Sum (..))
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
bySubstance = M.elems . substanceLines

-- | The lines of 'bySubstance', under the key that makes each one.
substanceLines :: [FlowContribution] -> M.Map SubstanceKey SubstanceContribution
substanceLines = fmap line . groupInOrder . map keyed . sortOn (Down . abs . fcContribution)
  where
    keyed :: FlowContribution -> (SubstanceKey, FlowContribution)
    keyed row = (keyOf (fcFlow row), row)
    line :: NonEmpty FlowContribution -> SubstanceContribution
    line rows@(first :| _) =
        SubstanceContribution
            { scName = bfName (fcFlow first)
            , scMedium = compartmentName <$> bfCompartment (fcFlow first)
            , scFlows = rows
            , scContribution = sum (NE.map fcContribution rows)
            }

-- | The substance a flow is one of.
keyOf :: BiosphereFlow -> SubstanceKey
keyOf f = SubstanceKey{skName = T.toCaseFold (T.strip (bfName f)), skMedium = compartmentName <$> bfCompartment f}

-- | How many substances a split by substance names; every other one goes in each part's others.
substancesNamed :: Int
substancesNamed = 8

-- | A substance as one of its flows spells it.
data Spelling = Spelling
    { spName :: !Text
    , spMedium :: !(Maybe Medium)
    }
    deriving (Eq, Show)

{- | Several parts of one score on common substance lines: the substances
named, and how a part reads on them.
-}
data SubstanceGrid = SubstanceGrid
    { sgSubstances :: ![Spelling]
    -- ^ Heaviest first, each spelled as its largest flow over every part
    , sgRow :: [FlowContribution] -> PartRow
    -- ^ A part's flows on those lines
    }

-- | What one part gives each substance named, by its rank, and every other substance together.
data PartRow = PartRow
    { prCells :: !(M.Map Int Double)
    -- ^ By rank from 0, a substance the part gives nothing left out
    , prOthers :: !Double
    }
    deriving (Eq, Show)

{- | The @n@ substances weighing most over every part, by the sum of the
absolute values of what each part gives them, so a substance that lowers the
score is named as readily as one that raises it; a substance no part gives
anything is never named. A part's flows land in a cell or in its others: its
row adds up to its flows.
-}
substanceGrid :: (Foldable t) => Int -> t [FlowContribution] -> SubstanceGrid
substanceGrid n parts =
    SubstanceGrid
        { sgSubstances = [spelling | (_, (_, Max (Arg _ spelling))) <- named]
        , sgRow = row . substanceLines
        }
  where
    -- Over every part, a substance's weight and the spelling of its largest flow.
    summed :: M.Map SubstanceKey (Sum Double, Max (Arg Double Spelling))
    summed = M.unionsWith (<>) (map (M.map summary . substanceLines) (toList parts))
    summary :: SubstanceContribution -> (Sum Double, Max (Arg Double Spelling))
    summary s = (Sum (abs (scContribution s)), Max (Arg (abs (fcContribution (NE.head (scFlows s)))) (Spelling (scName s) (scMedium s))))
    named :: [(SubstanceKey, (Sum Double, Max (Arg Double Spelling)))]
    named = take n (sortOn (Down . weight) (filter ((> 0) . weight) (M.toList summed)))
    weight :: (SubstanceKey, (Sum Double, Max (Arg Double Spelling))) -> Double
    weight = getSum . fst . snd
    rank :: M.Map SubstanceKey Int
    rank = M.fromList (zip (map fst named) [0 ..])
    row :: M.Map SubstanceKey SubstanceContribution -> PartRow
    row line =
        let (cells, others) = M.partitionWithKey (\k _ -> M.member k rank) (M.map scContribution line)
         in PartRow
                { prCells = M.fromList [(r, c) | (k, c) <- M.toList cells, c /= 0, Just r <- [M.lookup k rank]]
                , prOthers = sum others
                }

-- | The sub-compartments of a substance's flows, each once.
substanceCompartments :: SubstanceContribution -> [Text]
substanceCompartments = nubOrd . mapMaybe (bfCompartmentSub . fcFlow) . NE.toList . scFlows

-- | A substance's flows, by UUID.
substanceFlowIds :: SubstanceContribution -> [Text]
substanceFlowIds = map (UUID.toText . bfId . fcFlow) . NE.toList . scFlows

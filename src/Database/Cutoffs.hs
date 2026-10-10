{- | The product inputs a calculation met that no loaded database supplies.

The supplier-gap report lists them for a whole database; a result names only
the ones its own chain reaches, those of a process the solution scales by
something other than zero.
-}
module Database.Cutoffs (
    GapIndex (..),
    gapIndexOf,
    Cutoffs (..),
    noCutoffs,
    cutoffInputsIn,
) where

import API.Types (CutoffInput (..), WithheldCutoffs (..))
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import Database.Loader (GapEdge (..), gapEdgesForLoaded, gapReasons)
import Matrix (activityNormalizationFactor, processScaling)
import SharedSolver (CrossDBSolution (..))
import Types (BlockerReason, Database (..), ProcessId)

-- | A database's unsupplied edges, by the process that asks for them.
newtype GapIndex = GapIndex (M.Map ProcessId [GapEdge])
    deriving (Show, Eq)

-- | One scan of the database, so a result only looks up its own processes.
gapIndexOf :: Database -> GapIndex
gapIndexOf db =
    GapIndex $
        M.map reverse . M.fromListWith (++) $
            [ (pid, [e])
            | e <- gapEdgesForLoaded db
            , Just pid <- [M.lookup (gapConsumerAct e, gapConsumerProd e) (dbProcessIdLookup db)]
            ]

-- | The cut-offs of a result: named where the licence shows them, counted where it does not.
data Cutoffs = Cutoffs
    { cutoffShown :: [CutoffInput]
    , cutoffWithheld :: [WithheldCutoffs]
    }
    deriving (Show, Eq)

noCutoffs :: Cutoffs
noCutoffs = Cutoffs{cutoffShown = [], cutoffWithheld = []}

-- | What one cut-off input is grouped by: the same product asked for the same way.
data CutoffKey = CutoffKey
    { ckDatabase :: Text
    , ckProduct :: Text
    , ckLocation :: Text
    , ckUnit :: Text
    }
    deriving (Eq, Ord)

-- | What the processes asking for one cut-off product add up to.
data Met = Met
    { metAmount :: Double
    , metConsumers :: S.Set ProcessId
    , metReasons :: NE.NonEmpty BlockerReason
    }

instance Semigroup Met where
    a <> b =
        Met
            { metAmount = metAmount a + metAmount b
            , metConsumers = metConsumers a <> metConsumers b
            , metReasons = NE.nub (metReasons a <> metReasons b)
            }

{- | The cut-off inputs a solution meets, largest demand first.

A process's input of @a@ per its reference amount @r@ asks @x * a / r@ of the
chain when the process runs at @x@: the matrix column holds @a / r@. A
database listed twice in the solution (two links reaching it, or a cycle)
adds its demands up.

The index must be the one of the database each entry of the solution carries,
and is read at that database's process identifiers without a bound check: an
index taken from another version of the database stops the request rather
than answering with the cut-offs of a chain that is not this one.
-}
cutoffInputsIn :: (Text -> GapIndex) -> CrossDBSolution -> [CutoffInput]
cutoffInputsIn indexOf sol =
    sortOn (Down . abs . ciAmount) (map input (M.toList grouped))
  where
    grouped :: M.Map CutoffKey Met
    grouped =
        M.fromListWith
            (flip (<>))
            [ (keyOf name e, Met (x * gapAmount e / activityNormalizationFactor db pid) (S.singleton pid) (gapReasons (gapReason e)))
            | (name, db, scaling) <- NE.toList (csScalings sol)
            , let GapIndex byConsumer = indexOf name
            , (pid, edges) <- M.toList byConsumer
            , let x = processScaling db scaling pid
            , x /= 0
            , e <- edges
            ]

    keyOf :: Text -> GapEdge -> CutoffKey
    keyOf name e = CutoffKey name (gapFlowName e) (gapLocation e) (gapUnit e)

    input :: (CutoffKey, Met) -> CutoffInput
    input (k, m) =
        CutoffInput
            { ciDatabase = ckDatabase k
            , ciProduct = ckProduct k
            , ciLocation = ckLocation k
            , ciUnit = ckUnit k
            , ciAmount = metAmount m
            , ciConsumers = S.size (metConsumers m)
            , ciReasons = metReasons m
            }

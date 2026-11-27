{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The order in which the contributions to a score are listed, and where the
list stops.

The rows come largest first in absolute value, each with its share of the sum
of all the absolute values. A row that lowers the score weighs by its size, as
the most-relevant rule of the PEF method reads it: a credit of 30 % is as
relevant as a burden of 30 %. The parts a licence keeps unlisted count in that
sum too, so a list never claims to cover what it cannot see.

Every list of contributions comes through here, so a REST answer and a tool
answer cut at the same row.
-}
module Impact.Ranking (
    RowCut (..),
    Ranked (..),
    rowCut,
    rankRows,
) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)

-- | Where a list of contributions stops.
data RowCut
    = -- | After this many rows
      FirstRows !Int
    | {- | Once the rows cover more than this percentage of the absolute sum,
      and never after the second number of rows
      -}
      Covering !Double !Int
    deriving (Eq, Show)

-- | One row, with what it and the rows before it weigh in the absolute sum.
data Ranked a = Ranked
    { rkRow :: a
    , rkAbsolutePercent :: !Double
    -- ^ Its absolute value, as a percentage of the sum of all the absolute values
    , rkCumulativePercent :: !Double
    -- ^ The same for it and every row before it
    }
    deriving (Eq, Show)

{- | The cut a request asks for: a number of rows, or a percentage the rows
must cover, capped by the number of rows when one is given.
-}
rowCut :: Int -> Maybe Int -> Maybe Double -> Either Text RowCut
rowCut defaultRows rows = \case
    Nothing -> Right (FirstRows (fromMaybe defaultRows rows))
    Just percent
        | percent > 0 && percent <= 100 -> Right (Covering percent (fromMaybe coveringCap rows))
        | otherwise -> Left "cover_percent must be more than 0 and at most 100"
  where
    -- A long tail of small rows can take hundreds to reach a high percentage: the answer stays readable.
    coveringCap :: Int
    coveringCap = 100

{- | The rows, largest first in absolute value, cut where asked. The unlisted
parts are the scores a licence keeps whole: their absolute values join the sum,
though a part summing burdens and credits weighs less than they do.
-}
rankRows :: forall a. (a -> Double) -> [Double] -> RowCut -> [a] -> [Ranked a]
rankRows contribution unlisted cut rows = cutAt cut (zipWith3 Ranked sorted shares (scanl1 (+) shares))
  where
    sorted :: [a]
    sorted = sortOn (Down . abs . contribution) rows

    total :: Double
    total = sum (map (abs . contribution) rows) + sum (map abs unlisted)

    shares :: [Double]
    shares = [if total > 0 then abs (contribution r) / total * 100 else 0 | r <- sorted]

cutAt :: RowCut -> [Ranked a] -> [Ranked a]
cutAt (FirstRows n) = take n
cutAt (Covering percent cap) = take cap . throughFirstOver
  where
    throughFirstOver :: [Ranked a] -> [Ranked a]
    throughFirstOver ranked = case span ((<= percent) . rkCumulativePercent) ranked of
        (covered, next) -> covered <> take 1 next

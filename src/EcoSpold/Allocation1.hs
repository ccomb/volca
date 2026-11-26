{-# LANGUAGE OverloadedStrings #-}

{- | The allocation keys of an EcoSpold 1 dataset making several products, read
into what the engine divides a block by.

The format states a key per product: a fraction, in percent, and the numbers of
the lines it applies to. The sum of the fractions a line receives is 100. A
line a product's keys leave out is not that product's: the reading other tools
make of the format, and the only one a key implies, so it counts 0 for it, and
the reading says so. A product's key on its own line says nothing about the
lines it shares.

Two shapes come out of it:

  * every product takes one fraction of every line it shares, or none of any:
    that fraction is the product's declared share, as any other reader states
    it, and a reading that keeps the block whole keeps it;
  * the fractions differ from line to line: each product is marked as divided
    line by line ('ShareByLine'), with no share of its own, and each (product,
    line) pair carries its own fraction, which only a division by declared
    shares applies.

A key the dataset cannot be read by (a number that names no line or several,
a product that is not one, two fractions for one pair) leaves the block
undivided, and says why: dividing by the rest would apply keys their author
wrote against lines they did not mean. So do keys giving no product any line
it shares: dividing by them would make every product from nothing.
-}
module EcoSpold.Allocation1 (
    Allocation1 (..),
    Division (..),
    divide,
) where

import Data.Either (lefts)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Allocation (ExchangeShares, SharedExchange (..), exchangeShares, noExchangeShares)
import Types

-- | One @\<allocation\>@: the product it is for, its fraction in percent, and the numbers of the lines it applies to.
data Allocation1 = Allocation1
    { alCoProduct :: !Int
    , alFraction :: !Double
    , alLines :: ![Int]
    }
    deriving (Eq, Show)

-- | A dataset's lines with the shares its keys declare, the pairs they add, and what the reading has to say.
data Division = Division
    { dvLines :: ![Exchange]
    , dvPairs :: !ExchangeShares
    , dvWarnings :: ![Text]
    }

{- | Read a dataset's keys against its lines, each with the number the file
gives it, in the order the activity holds them: the positions the shares and
pairs are keyed by are those of this list.
-}
divide :: (Exchange -> Text) -> [(Int, Exchange)] -> [Allocation1] -> Division
divide nameOf numbered keys
    | null keys || null shared = whole []
    | not (null refused) = whole ["its allocation keys are not applied, and the block is kept whole: " <> T.intercalate "; " (nub refused)]
    | M.null fractions = whole ["its allocation keys give no product a line it shares, so they are not applied, and the block is kept whole"]
    | Just shares <- traverse uniform productsAt =
        Division (declared (M.fromList (zip productsAt (map percent shares)))) noExchangeShares (unkeyed <> sums)
    | otherwise =
        Division
            (declared (M.fromList [(at, ShareByLine) | at <- productsAt]))
            (exchangeShares (M.fromList [(pair, M.findWithDefault 0 pair fractions) | pair <- pairs]))
            (lineByLine : sums)
  where
    whole = Division lines' noExchangeShares
    lines' = map snd numbered
    placed = zip [0 :: Int ..] numbered
    products = [(at, n) | (at, (n, ex)) <- placed, exchangeIsProductOutput ex]
    shared = [(at, n) | (at, (n, ex)) <- placed, not (exchangeIsProductOutput ex)]
    productsAt = map fst products
    sharedAt = map fst shared
    pairs = [SharedExchange p s | p <- productsAt, s <- sharedAt]
    nameAt at = maybe "" (nameOf . snd) (lookup at placed)

    resolved :: [Either Text [(SharedExchange, Double)]]
    resolved = map resolve keys

    resolve :: Allocation1 -> Either Text [(SharedExchange, Double)]
    resolve key = do
        productAt <- case [at | (at, n) <- products, n == alCoProduct key] of
            [at] -> Right at
            [] -> Left ("a key is for number " <> tshow (alCoProduct key) <> ", which is no product of the dataset")
            _ -> Left ("number " <> tshow (alCoProduct key) <> " names several products")
        linesAt <- traverse lineAt [n | n <- alLines key, n /= alCoProduct key]
        Right [(SharedExchange productAt s, alFraction key) | s <- linesAt]

    lineAt :: Int -> Either Text Int
    lineAt n = case [at | (at, m) <- shared, m == n] of
        [at] -> Right at
        [] -> Left ("a key applies to number " <> tshow n <> ", which names no line the products share")
        _ -> Left ("number " <> tshow n <> " names several lines")

    -- Every fraction the keys give each pair: more than one distinct is refused.
    given :: M.Map SharedExchange [Double]
    given = M.fromListWith (<>) [(pair, [f]) | Right fs <- resolved, (pair, f) <- fs]

    refused :: [Text]
    refused =
        lefts resolved
            <> [ "two keys give " <> nameAt (seExchange pair) <> " to " <> nameAt (seProduct pair) <> " different fractions"
               | (pair, fs) <- M.toList given
               , length (nub fs) > 1
               ]

    fractions :: M.Map SharedExchange Double
    fractions = M.mapMaybe single given
      where
        single fs = case nub fs of
            [f] -> Just f
            _ -> Nothing

    -- One fraction of every shared line, or of none.
    uniform :: Int -> Maybe Double
    uniform p = case [M.lookup (SharedExchange p s) fractions | s <- sharedAt] of
        found
            | all isNothing found -> Just 0
            | Just (f : rest) <- sequence found, all (== f) rest -> Just f
            | otherwise -> Nothing

    declared :: M.Map Int ProductShare -> [Exchange]
    declared shares = [maybe ex (`withShare` ex) (M.lookup at shares) | (at, (_, ex)) <- placed]

    unkeyed :: [Text]
    unkeyed = case [nameAt p | p <- productsAt, all (\s -> M.notMember (SharedExchange p s) fractions) sharedAt] of
        [] -> []
        names -> ["its allocation keys give no line it shares to " <> T.intercalate ", " names <> ", which is declared at 0 %"]

    lineByLine :: Text
    lineByLine =
        "its allocation keys divide its lines between its products line by line, which only a division by declared shares applies"
            <> case [nameAt (seProduct pair) <> ": " <> nameAt (seExchange pair) | pair <- pairs, M.notMember pair fractions] of
                [] -> ""
                uncovered -> "; a line no key gives a product counts 0 for it: " <> T.intercalate ", " uncovered

    sums :: [Text]
    sums = case mapMaybe offTotal sharedAt of
        [] -> []
        off -> ["the fractions its allocation keys give a line add up to other than 100 %: " <> T.intercalate ", " off]

    offTotal :: Int -> Maybe Text
    offTotal s
        | abs (total - 100) > 0.01 = Just (nameAt s <> " (" <> tshow total <> " %)")
        | otherwise = Nothing
      where
        total = sum [M.findWithDefault 0 (SharedExchange p s) fractions | p <- productsAt]

-- | A share of the whole inventory, as a key states it.
percent :: Double -> ProductShare
percent share = ShareDeclared DeclaredShare{dsPercent = share, dsFormula = Nothing, dsBy = Nothing}

-- | A product row dividing its block this way.
withShare :: ProductShare -> Exchange -> Exchange
withShare share ex = case ex of
    TechnosphereExchange{} -> ex{techShare = Just share}
    BiosphereExchange{} -> ex
    WasteExchange{} -> ex

tshow :: (Show a) => a -> Text
tshow = T.pack . show

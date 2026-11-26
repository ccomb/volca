{-# LANGUAGE OverloadedStrings #-}

{- | The allocation keys of an EcoSpold 1 dataset making several products, read
into what the engine divides a block by.

The format states a key per product: a fraction, in percent, and the numbers of
the lines it applies to. The sum of the fractions a line receives is 100. A
line a product's keys leave out is not that product's: the reading other tools
make of the format, and the only one a key implies, so it counts 0 for it. A
product's key on its own line says nothing about the lines it shares.

Two shapes come out of it:

  * every product takes one fraction of every line it shares, or none of any:
    that fraction is the product's declared share, as any other reader states
    it, and a reading that keeps the block whole keeps it;
  * the fractions differ from line to line: each product is declared at 100 %
    and each (product, line) pair carries its own fraction, which only a
    division by declared shares applies.

A key the dataset cannot be read by (a number that names no line or several,
a product that is not one, two fractions for one pair) leaves the block
undivided, and says why: dividing by the rest would apply keys their author
wrote against lines they did not mean.
-}
module EcoSpold.Allocation1 (
    Allocation1 (..),
    Division (..),
    divide,
    withFirstProductAsReference,
) where

import Data.Either (lefts)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Allocation (ExchangeShares, SharedExchange (..), asReference, exchangeShares, isCoproduct, noExchangeShares)
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

-- | Where a key points, by position among the dataset's lines.
data Resolved = Resolved
    { rsProduct :: !Int
    , rsFractions :: ![(Int, Double)]
    }

{- | Read a dataset's keys against its lines, each with the number the file
gives it, in the order the activity holds them: the positions the shares and
pairs are keyed by are those of this list.
-}
divide :: (Exchange -> Text) -> [(Int, Exchange)] -> [Allocation1] -> Division
divide nameOf numbered keys
    | null keys || null shared = Division lines' noExchangeShares []
    | not (null refused) = Division lines' noExchangeShares ["its allocation keys are not applied, and the block is kept whole: " <> T.intercalate "; " (nub refused)]
    | Just shares <- traverse uniform productsAt =
        Division (declared (M.fromList (zip productsAt shares))) noExchangeShares sums
    | otherwise =
        Division
            (declared (M.fromList [(at, 100) | at <- productsAt]))
            (exchangeShares (M.fromList [(SharedExchange p s, M.findWithDefault 0 s (coverOf p)) | p <- productsAt, s <- sharedAt]))
            (lineByLine : sums)
  where
    lines' = map snd numbered
    placed = zip [0 :: Int ..] numbered
    products = [(at, n) | (at, (n, ex)) <- placed, exchangeIsProductOutput ex]
    shared = [(at, n) | (at, (n, ex)) <- placed, not (exchangeIsProductOutput ex)]
    productsAt = map fst products
    sharedAt = map fst shared
    nameAt at = maybe "" (nameOf . snd) (lookup at placed)

    resolved :: [Either Text Resolved]
    resolved = map resolve keys

    resolve :: Allocation1 -> Either Text Resolved
    resolve key = do
        productAt <- case [at | (at, n) <- products, n == alCoProduct key] of
            [at] -> Right at
            [] -> Left ("a key is for number " <> tshow (alCoProduct key) <> ", which is no product of the dataset")
            _ -> Left ("number " <> tshow (alCoProduct key) <> " names several products")
        fractions <- traverse lineAt [n | n <- alLines key, n /= alCoProduct key]
        Right (Resolved productAt [(at, alFraction key) | at <- fractions])

    lineAt :: Int -> Either Text Int
    lineAt n = case [at | (at, m) <- shared, m == n] of
        [at] -> Right at
        [] -> Left ("a key applies to number " <> tshow n <> ", which names no line the products share")
        _ -> Left ("number " <> tshow n <> " names several lines")

    -- Each product's fraction of each line, or the pairs given two.
    covers :: M.Map Int (M.Map Int [Double])
    covers = M.fromListWith (M.unionWith (<>)) [(rsProduct r, M.singleton at [f]) | Right r <- resolved, (at, f) <- rsFractions r]

    refused :: [Text]
    refused =
        lefts resolved
            <> [ "two keys give " <> nameAt s <> " to " <> nameAt p <> " different fractions"
               | (p, byLine) <- M.toList covers
               , (s, fs) <- M.toList byLine
               , length (nub fs) > 1
               ]

    coverOf :: Int -> M.Map Int Double
    coverOf p = maybe M.empty (M.mapMaybe single) (M.lookup p covers)
      where
        single fs = case nub fs of
            [f] -> Just f
            _ -> Nothing

    -- One fraction of every shared line, or of none.
    uniform :: Int -> Maybe Double
    uniform p
        | M.null cover = Just 0
        | M.keysSet cover == S.fromList sharedAt, [f] <- nub (M.elems cover) = Just f
        | otherwise = Nothing
      where
        cover = coverOf p

    declared :: M.Map Int Double -> [Exchange]
    declared shares = [maybe ex (`withShare` ex) (M.lookup at shares) | (at, (_, ex)) <- placed]

    lineByLine :: Text
    lineByLine =
        "its allocation keys divide its lines between its products line by line, which a reading keeping the block whole does not keep"
            <> case [nameAt p <> ": " <> nameAt s | p <- productsAt, s <- sharedAt, M.notMember s (coverOf p)] of
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
        total = sum [M.findWithDefault 0 s (coverOf p) | p <- productsAt]

-- | A product row declaring this share.
withShare :: Double -> Exchange -> Exchange
withShare share ex = case ex of
    TechnosphereExchange{} -> ex{techShare = Just DeclaredShare{dsPercent = share, dsFormula = Nothing, dsBy = Nothing}}
    BiosphereExchange{} -> ex
    WasteExchange{} -> ex

{- | A dataset naming no reference product but filing products as allocated
ones, the format's own way of writing a process of several products, takes the
first of them as its reference: a block kept whole is filed under its
reference, and one with none would be filed under no product.
-}
withFirstProductAsReference :: [Exchange] -> [Exchange]
withFirstProductAsReference lines'
    | any exchangeIsReference lines' = lines'
    | otherwise = case break isCoproduct lines' of
        (before, first : after) -> before <> (asReference first : after)
        (_, []) -> lines'

tshow :: (Show a) => a -> Text
tshow = T.pack . show

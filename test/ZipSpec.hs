{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Contract for "Zip": pack every entry, and do it in linear time.

The size guard is the point of the module. @zip-archive@'s 'addEntryToArchive'
rescans the whole entry list on each insert, so the fold it invites costs
O(n²) – on a real ILCD package (53 508 files) that turned an export into a
ten-minute timeout. Wall-clock is the only signal that separates the two
regimes, hence the 'timeout': 30 000 entries pack in well under a second when
linear, and take tens of seconds (or exhaust the stack) when not. The margin is
wide enough that a loaded machine cannot flip the verdict.

The companion test is that the fast path did not get fast by dropping entries.
-}
module ZipSpec (spec) where

import Codec.Archive.Zip (filesInArchive, findEntryByPath, fromEntry, toArchive)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Maybe (isJust)
import System.Timeout (timeout)
import Test.Hspec

import Zip (zipFiles)

-- | 30 000 small entries, the shape an ILCD package has (many tiny XML files).
manyFiles :: [(FilePath, BS.ByteString)]
manyFiles =
    [ ("processes/" ++ show i ++ ".xml", BS.replicate 64 65)
    | i <- [1 .. 30_000 :: Int]
    ]

spec :: Spec
spec = describe "Zip.zipFiles" $ do
    it "packs 30 000 entries without going quadratic" $ do
        packed <- timeout 5_000_000 (evaluate (BL.length (zipFiles manyFiles)))
        packed `shouldSatisfy` isJust

    it "keeps every entry it was given" $ do
        let archive = toArchive (zipFiles manyFiles)
        sort (filesInArchive archive) `shouldBe` sort (map fst manyFiles)

    it "round-trips the entry bytes" $ do
        let archive = toArchive (zipFiles [("a/one.xml", "hello"), ("b/two.xml", "world")])
            contents p = fromEntry <$> findEntryByPath p archive
        contents "a/one.xml" `shouldBe` Just "hello"
        contents "b/two.xml" `shouldBe` Just "world"

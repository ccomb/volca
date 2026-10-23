{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Contract for "Zip": pack every entry, and do it in linear time.

The size guard is the point of the module. @zip-archive@'s 'addEntryToArchive'
rescans the whole entry list on each insert, so the fold it invites costs
O(n²) – on a real ILCD package (53 508 files) that turned an export into a
ten-minute timeout. The guard counts what packing allocates rather than how
long it takes: doubling the entries doubles the allocation when linear and
quadruples it when not, on any machine and under any load.

The companion test is that the fast path did not get fast by dropping entries.
-}
module ZipSpec (spec) where

import Codec.Archive.Zip (filesInArchive, findEntryByPath, fromEntry, toArchive)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (sort)
import System.Mem (getAllocationCounter)
import Test.Hspec

import Zip (zipFiles)

-- | Small entries, the shape an ILCD package has (many tiny XML files).
filesOf :: Int -> [(FilePath, BS.ByteString)]
filesOf n =
    [ ("processes/" ++ show i ++ ".xml", BS.replicate 64 65)
    | i <- [1 .. n]
    ]

manyFiles :: [(FilePath, BS.ByteString)]
manyFiles = filesOf 30_000

-- | Bytes this thread allocates packing @n@ entries.
allocatedPacking :: Int -> IO Int64
allocatedPacking n = do
    before <- getAllocationCounter
    _ <- evaluate (BL.length (zipFiles (filesOf n)))
    after <- getAllocationCounter
    pure (before - after)

spec :: Spec
spec = describe "Zip.zipFiles" $ do
    it "packs twice the entries for about twice the allocation, not four times" $ do
        once <- allocatedPacking 10_000
        twice <- allocatedPacking 20_000
        fromIntegral twice / fromIntegral once `shouldSatisfy` (< (3 :: Double))

    it "keeps every entry it was given" $ do
        let archive = toArchive (zipFiles manyFiles)
        sort (filesInArchive archive) `shouldBe` sort (map fst manyFiles)

    it "round-trips the entry bytes" $ do
        let archive = toArchive (zipFiles [("a/one.xml", "hello"), ("b/two.xml", "world")])
            contents p = fromEntry <$> findEntryByPath p archive
        contents "a/one.xml" `shouldBe` Just "hello"
        contents "b/two.xml" `shouldBe` Just "world"

{-# LANGUAGE OverloadedStrings #-}

{- | What the three tabs of a search box count.

The three are disjoint and together cover the database, so a query's three
counts partition what it matched. A counter that disagreed with the tab it
labels would be worse than no counter, which is why the process count is
taken from the very function the process list is built from.
-}
module SearchCountsSpec (spec) where

import Service (CountAs (..), SearchCounts (..), countAsListed, searchCounts)
import Test.Hspec
import TestHelpers (loadSampleDatabase, shippedGeographies)

spec :: Spec
spec = describe "the counts behind a search box's tabs" $ do
    it "counts the processes a term matches" $ do
        db <- loadSampleDatabase "SAMPLE.ilcd"
        scProcesses (searchCounts shippedGeographies db countAsListed "Coal") `shouldBe` 1

    it "counts a product under products, not under flows" $ do
        -- SAMPLE.ilcd's outputs are technosphere flows, so a term matching
        -- them belongs to the middle tab and leaves the right-hand one empty.
        db <- loadSampleDatabase "SAMPLE.ilcd"
        let counts = searchCounts shippedGeographies db countAsListed "Coal"
        (scProducts counts > 0, scFlows counts) `shouldBe` (True, 0)

    it "answers zero everywhere for a term the database does not have" $ do
        db <- loadSampleDatabase "SAMPLE.ilcd"
        searchCounts shippedGeographies db countAsListed "zirconium" `shouldBe` SearchCounts 0 0 0

    it "counts the way an exact listing would list, not the way a fuzzy one would" $ do
        -- Two matchers answer this query: BM25 keeps a row matching one token,
        -- the exact path keeps only a row matching them all. A tab labelled
        -- with one number over a table built by the other is the bug.
        db <- loadSampleDatabase "SAMPLE.ilcd"
        let fuzzy = searchCounts shippedGeographies db countAsListed "Coal extraction"
            exact = searchCounts shippedGeographies db CountAs{caSort = Nothing, caExact = True} "Coal extraction"
        (scProcesses fuzzy >= scProcesses exact) `shouldBe` True

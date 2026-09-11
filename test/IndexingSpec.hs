{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Data.Indexing", the three answers to a key that does not
determine its value.

What they pin is what the callers rely on and a rewrite could quietly lose: a
repeated key is named once however many rows carried it, the rows a key
collected come back in the order they were read, and a key seen once is not a
collision.
-}
module IndexingSpec (spec) where

import Data.Indexing (collisions, repeated, uniqueIndex)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "repeated" $ do
        it "finds nothing in a list without repeats" $
            repeated ["a", "b", "c" :: Text] `shouldBe` []

        it "names a repeated key once, however many times it appears" $
            repeated ["a", "b", "a", "a" :: Text] `shouldBe` ["a"]

        it "returns the keys in ascending order, whatever the input order" $
            repeated ["c", "a", "c", "a" :: Text] `shouldBe` ["a", "c"]

        it "finds nothing in an empty list" $
            repeated ([] :: [Text]) `shouldBe` []

    describe "collisions" $ do
        it "keeps the rows of a colliding key in the order they were read" $
            collisions [("a", 1 :: Int), ("b", 2), ("a", 3), ("a", 4)]
                `shouldBe` [("a" :: Text, 1 :| [3, 4])]

        it "ignores a key carried by a single row" $
            collisions [("a", 1 :: Int), ("b", 2 :: Int)] `shouldBe` ([] :: [(Text, NonEmpty Int)])

    describe "uniqueIndex" $ do
        it "indexes rows whose keys are all distinct" $
            uniqueIndex [("a", 1 :: Int), ("b", 2)]
                `shouldBe` Right (M.fromList [("a" :: Text, 1), ("b", 2)])

        it "refuses, naming every key that repeats, and indexes nothing" $
            uniqueIndex [("a", 1 :: Int), ("b", 2), ("a", 3), ("c", 4), ("c", 5)]
                `shouldBe` Left ("a" :| ["c" :: Text])

        it "accepts an empty list" $
            uniqueIndex ([] :: [(Text, Int)]) `shouldBe` Right M.empty

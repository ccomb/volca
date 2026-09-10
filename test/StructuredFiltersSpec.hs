{-# LANGUAGE OverloadedStrings #-}

{- | The structured (geo, product, classification) filters applied on top of
name candidates. Pins the exact-match contract: @exact=true@ turns the
product filter into case-insensitive equality, matching what it already
meant for names; and pins what a geography filter answers, since location
codes overlap as text and the same filter narrows a delete.
-}
module StructuredFiltersSpec (spec) where

import Database (findActivitiesByFields, locationAnswers)
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types

spec :: Spec
spec = do
    exactProductFilter
    geographyFilter

geographyFilter :: Spec
geographyFilter = describe "a geography filter names a place" $ do
    it "answers for the location itself, whatever the case" $
        map (`locationAnswers` "FR") ["FR", "fr", " FR "] `shouldBe` [True, True, True]

    it "answers for a location written under the one asked for" $
        map (uncurry locationAnswers) [("US", "US-WECC"), ("CH", "CH-VD"), ("Europe", "Europe without Switzerland")]
            `shouldBe` [True, True, True]

    it "does not answer for a code that merely sits inside another" $
        -- DE inside NORDEL, SE inside US-SERC: different places whose codes
        -- share letters, which a substring match cannot tell apart.
        map (uncurry locationAnswers) [("DE", "NORDEL"), ("SE", "US-SERC"), ("IN", "CN-IN")]
            `shouldBe` [False, False, False]

    it "does not answer for a region named after the place it excludes" $
        -- "RER w/o CH+DE" is Europe minus Switzerland and Germany, so answering
        -- a question about either with it states the opposite of the truth.
        map (uncurry locationAnswers) [("CH", "RER w/o CH+DE"), ("DE", "RER w/o CH+DE")]
            `shouldBe` [False, False]

    it "answers a filter typed one letter at a time" $
        map (uncurry locationAnswers) [("F", "FR"), ("F", "FI"), ("F", "DE")]
            `shouldBe` [True, True, False]

exactProductFilter :: Spec
exactProductFilter = describe "exact product filter" $ do
    it "matches the full product name only" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db Nothing Nothing (Just "product C") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

    it "is case-insensitive" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db Nothing Nothing (Just "PRODUCT c") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

    it "rejects a partial product name" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db Nothing Nothing (Just "product") [] True
        map (activityName . snd) hits `shouldBe` []

    it "combines with an exact name filter" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db (Just "production of product C") Nothing (Just "product C") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

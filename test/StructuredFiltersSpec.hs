{-# LANGUAGE OverloadedStrings #-}

{- | The structured (geo, product, classification) filters applied on top of
name candidates. Pins the exact-match contract: @exact=true@ turns the
product filter into case-insensitive equality, matching what it already
meant for names; and pins what a geography filter answers, since location
codes overlap as text and the same filter narrows a delete.
-}
module StructuredFiltersSpec (spec) where

import Data.Text (Text)
import Database (findActivitiesByFields, locationAnswers, locationIs)
import Test.Hspec
import TestHelpers (loadSampleDatabase, shippedGeographies)
import Types

spec :: Spec
spec = do
    exactProductFilter
    geographyFilter

geographyFilter :: Spec
geographyFilter = describe "a geography filter names a place" $ do
    it "answers for the location itself, whatever the case" $
        map (`answers` "FR") ["FR", "fr", " FR "] `shouldBe` [True, True, True]

    it "answers for a location the table declares inside the one asked for" $
        map
            (uncurry answers)
            [ ("US", "US-WECC")
            , ("RER", "FR")
            , ("NAFTA", "CA-QC")
            , ("Europe", "Europe without Switzerland")
            , ("Europe without Switzerland", "Europe without Switzerland and France")
            ]
            `shouldBe` [True, True, True, True, True]

    it "does not answer for a place that merely starts with the same letters" $
        -- Greenland is not the world, Romania is not the rest of it, and Norway
        -- is neither the Nordic grid nor a part of Cyprus. Reading a place out
        -- of the letters another one begins with answers all of those.
        map
            (uncurry answers)
            [ ("GL", "GLO")
            , ("RO", "RoW")
            , ("NO", "NORDEL")
            , ("NO", "Northern Cyprus")
            , ("CA", "Canary Islands")
            , ("RE", "RER")
            , ("NA", "NAFTA")
            , ("MR", "MRO")
            ]
            `shouldBe` replicate 8 False

    it "does not answer for a region named after the place it excludes" $
        -- "RER w/o CH+DE" is Europe minus Switzerland and Germany, so answering
        -- a question about either with it states the opposite of the truth.
        map (uncurry answers) [("CH", "RER w/o CH+DE"), ("DE", "RER w/o CH+DE")]
            `shouldBe` [False, False]

    it "answers for a place wider than the one asked for in neither direction" $
        -- Containment runs one way. US-WECC is inside the US; the US is not
        -- inside US-WECC, and a filter that answered both would hand back data
        -- more precise than the question.
        map (uncurry answers) [("US", "US-WECC"), ("US-WECC", "US")]
            `shouldBe` [True, False]

    it "answers for the world with the world, not with everywhere in it" $
        -- The table lists GLO and RoW among the wider places of every country,
        -- so a characterization factor found only for the world can still
        -- answer for a country. A filter asking for the world is asking for the
        -- datasets written that way.
        map (uncurry answers) [("GLO", "GLO"), ("GLO", "FR"), ("RoW", "CN"), ("RoW", "GLO")]
            `shouldBe` [True, False, False, False]

    it "answers for a place the table does not know with that place alone" $
        -- Nothing here is inferred from how the name is spelled, so a location
        -- nobody declared is a location with no places inside it.
        map (uncurry answers) [("Commonwealth", "Commonwealth"), ("Commonwealth", "GB"), ("Common", "Commonwealth")]
            `shouldBe` [True, False, False]

    it "reads a location the same way under exact, so only the question differs" $
        -- Whitespace and case are not part of a place. Reading them on one arm
        -- and not the other would make " FR " answer or not depending on which
        -- of the two filters was asked.
        map (uncurry locationIs) [(" FR ", "fr"), ("US", "US-WECC"), ("F", "FR")]
            `shouldBe` [True, False, False]

{- | The filter asked against the location table the engine actually ships, not
a fixture: the pairs above are the ones real databases write, and a hand-made
table would pin the test to itself rather than to the shipped answer. A table
that failed to load would answer no to everything, which the pairs expecting
yes above are what catches.
-}
answers :: Text -> Text -> Bool
answers = locationAnswers shippedGeographies

exactProductFilter :: Spec
exactProductFilter = describe "exact product filter" $ do
    it "matches the full product name only" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields shippedGeographies db Nothing Nothing (Just "product C") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

    it "is case-insensitive" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields shippedGeographies db Nothing Nothing (Just "PRODUCT c") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

    it "rejects a partial product name" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields shippedGeographies db Nothing Nothing (Just "product") [] True
        map (activityName . snd) hits `shouldBe` []

    it "combines with an exact name filter" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields shippedGeographies db (Just "production of product C") Nothing (Just "product C") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

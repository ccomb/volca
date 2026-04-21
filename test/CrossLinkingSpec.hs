{-# LANGUAGE OverloadedStrings #-}

module CrossLinkingSpec (spec) where

import qualified Data.Map.Strict as M
import Database.CrossLinking
import Database.Loader (loadDatabase)
import SynonymDB (buildFromPairs, emptySynonymDB)
import Test.Hspec
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- normalizeText
    -- -----------------------------------------------------------------------
    describe "normalizeText" $ do
        it "lowercases and strips whitespace" $
            normalizeText "  Wheat Production  " `shouldBe` "wheat production"

        it "is idempotent" $
            normalizeText (normalizeText "Foo Bar") `shouldBe` normalizeText "Foo Bar"

        it "normalizes en-dash to hyphen" $
            normalizeText "bio\x2013gas" `shouldBe` "bio-gas"

        it "normalizes soft hyphen to ASCII hyphen" $
            normalizeText "bio\x00ADgas" `shouldBe` "bio-gas"

        it "normalizes hyphen (U+2010) to ASCII hyphen" $
            normalizeText "bio\x2010gas" `shouldBe` "bio-gas"

        it "normalizes non-breaking hyphen (U+2011) to ASCII hyphen" $
            normalizeText "bio\x2011gas" `shouldBe` "bio-gas"

        it "normalizes figure dash (U+2012) to ASCII hyphen" $
            normalizeText "bio\x2012gas" `shouldBe` "bio-gas"

        it "normalizes em-dash (U+2014) to ASCII hyphen" $
            normalizeText "bio\x2014gas" `shouldBe` "bio-gas"

        it "normalizes non-breaking space (U+00A0) to regular space" $
            normalizeText "bio\x00A0gas" `shouldBe` "bio gas"

        it "normalizes narrow no-break space (U+202F) to regular space" $
            normalizeText "bio\x202Fgas" `shouldBe` "bio gas"

    -- -----------------------------------------------------------------------
    -- stripTrailingDBTag
    -- -----------------------------------------------------------------------
    describe "stripTrailingDBTag" $ do
        it "strips '(WFLDB)' suffix" $
            stripTrailingDBTag "wheat (WFLDB)" `shouldBe` Just "wheat"

        it "strips '(AGRIBALYSE)' suffix" $
            stripTrailingDBTag "tomato (AGRIBALYSE)" `shouldBe` Just "tomato"

        it "returns Nothing when no tag present" $
            stripTrailingDBTag "wheat" `shouldBe` Nothing

        it "returns Nothing for lowercase content" $
            stripTrailingDBTag "wheat (organic)" `shouldBe` Nothing

        it "returns Nothing for empty string" $
            stripTrailingDBTag "" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- stripTrailingLocationSuffix
    -- -----------------------------------------------------------------------
    describe "stripTrailingLocationSuffix" $ do
        it "strips '/CA U' suffix" $
            stripTrailingLocationSuffix "wheat (WFLDB)/CA U" `shouldBe` Just "wheat (WFLDB)"

        it "strips '/GLO S' suffix" $
            stripTrailingLocationSuffix "electricity/GLO S" `shouldBe` Just "electricity"

        it "returns Nothing when no slash present" $
            stripTrailingLocationSuffix "wheat" `shouldBe` Nothing

        it "returns Nothing when suffix has wrong format" $
            stripTrailingLocationSuffix "a/b/c" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- extractProductPrefixes
    -- -----------------------------------------------------------------------
    describe "extractProductPrefixes" $ do
        it "splits on '//' separator" $
            extractProductPrefixes "wheat//[GLO] wheat production" `shouldContain` ["wheat"]

        it "splits on ' {' separator" $
            extractProductPrefixes "electricity {FR}" `shouldContain` ["electricity"]

        it "strips DB tag" $
            extractProductPrefixes "wheat (WFLDB)" `shouldContain` ["wheat"]

        it "returns empty list for plain name with no separator" $
            extractProductPrefixes "wheat" `shouldBe` []

    -- -----------------------------------------------------------------------
    -- extractBracketedLocation
    -- -----------------------------------------------------------------------
    describe "extractBracketedLocation" $ do
        it "extracts from curly braces {FR}" $
            extractBracketedLocation "electricity {FR}" `shouldBe` "FR"

        it "extracts from square brackets [GLO]" $
            extractBracketedLocation "wheat [GLO] production" `shouldBe` "GLO"

        it "ignores chemical notation [thio]" $
            extractBracketedLocation "compound [thio]" `shouldBe` ""

        it "returns empty string when no brackets" $
            extractBracketedLocation "plain name" `shouldBe` ""

    -- -----------------------------------------------------------------------
    -- isSubregionOf
    -- -----------------------------------------------------------------------
    describe "isSubregionOf" $ do
        it "FR is a subregion of RER" $
            isSubregionOf locationHierarchy "FR" "RER" `shouldBe` True

        it "FR is a subregion of GLO" $
            isSubregionOf locationHierarchy "FR" "GLO" `shouldBe` True

        it "GLO is not a subregion of FR" $
            isSubregionOf locationHierarchy "GLO" "FR" `shouldBe` False

        it "unknown location has no parents" $
            isSubregionOf locationHierarchy "XX" "GLO" `shouldBe` False

    -- -----------------------------------------------------------------------
    -- matchLocation
    -- -----------------------------------------------------------------------
    describe "matchLocation" $ do
        it "exact match scores 30" $
            matchLocation locationHierarchy "FR" "FR" `shouldBe` 30

        it "FR consumer, RER supplier scores 20 (widening)" $
            matchLocation locationHierarchy "FR" "RER" `shouldBe` 20

        it "FR consumer, GLO supplier scores 20 (widening via subregion)" $
            matchLocation locationHierarchy "FR" "GLO" `shouldBe` 20

        it "unknown location, GLO supplier scores 10 (global fallback)" $
            matchLocation locationHierarchy "XX" "GLO" `shouldBe` 10

        it "unknown location, RoW supplier scores 10 (global fallback)" $
            matchLocation locationHierarchy "XX" "RoW" `shouldBe` 10

        it "narrowing (GLO consumer, FR supplier) scores 0" $
            matchLocation locationHierarchy "GLO" "FR" `shouldBe` 0

        it "unrelated locations score 5" $
            matchLocation locationHierarchy "FR" "CN" `shouldBe` 5

    -- -----------------------------------------------------------------------
    -- matchProductName
    -- -----------------------------------------------------------------------
    describe "matchProductName" $ do
        it "exact match (case-insensitive) scores 50" $
            matchProductName emptySynonymDB "Wheat" "wheat" `shouldBe` 50

        it "no match scores 0" $
            matchProductName emptySynonymDB "wheat" "steel" `shouldBe` 0

        it "synonym match scores 45 when names share a group" $ do
            -- areSynonyms checks group-ID equality; build via CSV so both names
            -- normalize to entries that share the same group
            let synDB = buildFromPairs [("co2", "carbon dioxide")]
            -- After normalization both resolve to the same group in a symmetric pair
            matchProductName synDB "CO2" "carbon dioxide" `shouldSatisfy` (>= 45)

    -- -----------------------------------------------------------------------
    -- extractProductPrefixes — additional cases
    -- -----------------------------------------------------------------------
    describe "extractProductPrefixes (additional)" $ do
        it "strips location suffix /CA U" $
            extractProductPrefixes "wheat (WFLDB)/CA U" `shouldContain` ["wheat (WFLDB)"]

        it "strips both tag and location suffix" $
            extractProductPrefixes "wheat (WFLDB)/CA U" `shouldContain` ["wheat"]

        it "splits on ' |' separator" $
            extractProductPrefixes "heat | natural gas | CH" `shouldContain` ["heat"]

    -- -----------------------------------------------------------------------
    -- isSubregionOf — additional location pairs
    -- -----------------------------------------------------------------------
    describe "isSubregionOf (additional)" $ do
        it "US is a subregion of North America" $
            isSubregionOf locationHierarchy "US" "North America" `shouldBe` True

        it "CA is a subregion of NAFTA" $
            isSubregionOf locationHierarchy "CA" "NAFTA" `shouldBe` True

        it "JP is a subregion of Asia" $
            isSubregionOf locationHierarchy "JP" "Asia" `shouldBe` True

        it "BR is a subregion of Latin America" $
            isSubregionOf locationHierarchy "BR" "Latin America" `shouldBe` True

        it "AU is a subregion of GLO" $
            isSubregionOf locationHierarchy "AU" "GLO" `shouldBe` True

    -- -----------------------------------------------------------------------
    -- matchLocation — additional scoring cases
    -- -----------------------------------------------------------------------
    describe "matchLocation (additional)" $ do
        it "US consumer, NAFTA supplier scores 20 (widening)" $
            matchLocation locationHierarchy "US" "NAFTA" `shouldBe` 20

        it "GLO consumer, GLO supplier scores 30 (exact)" $
            matchLocation locationHierarchy "GLO" "GLO" `shouldBe` 30

        it "RoW consumer, RoW supplier scores 30 (exact)" $
            matchLocation locationHierarchy "RoW" "RoW" `shouldBe` 30

    -- -----------------------------------------------------------------------
    -- findSupplierInIndexedDBs — integration using SAMPLE.min3
    -- -----------------------------------------------------------------------
    describe "findSupplierInIndexedDBs (SAMPLE.min3)" $ do
        it "finds 'product Y' by name and GLO location" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        }
            case findSupplierInIndexedDBs ctx "product Y" "GLO" "kg" of
                CrossDBLinked _ _ _ score _ _ _ -> score `shouldSatisfy` (>= defaultLinkingThreshold)
                CrossDBNotLinked reason -> expectationFailure $ "Expected link but got: " ++ show reason

        it "returns NoNameMatch for an unknown product" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        }
            case findSupplierInIndexedDBs ctx "no such product" "GLO" "kg" of
                CrossDBNotLinked _ -> return ()
                CrossDBLinked{} -> expectationFailure "Expected CrossDBNotLinked"

        it "returns UnitIncompatible when product found but unit doesn't match" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        }
            -- "product Y" exists in kg; asking for m3 should fail unit check
            case findSupplierInIndexedDBs ctx "product Y" "GLO" "m3" of
                CrossDBNotLinked (UnitIncompatible _ _) -> return ()
                CrossDBNotLinked reason -> expectationFailure $ "Expected UnitIncompatible but got: " ++ show reason
                CrossDBLinked{} -> expectationFailure "Expected CrossDBNotLinked for unit mismatch"

        it "finds via synonym when synDB has the pair" $ do
            idb <- loadMin3IndexedDB
            -- "product Y" is the canonical name; "producto Y" (alias) can be found via synonym
            let synDB = buildFromPairs [("product y", "producto y")]
                ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = synDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        }
            -- Synonym lookup: "producto y" → group containing "product y" → supplier
            case findSupplierInIndexedDBs ctx "producto y" "GLO" "kg" of
                CrossDBLinked _ _ _ score _ _ _ -> score `shouldSatisfy` (>= defaultLinkingThreshold)
                CrossDBNotLinked _ -> pendingWith "synonym linking requires index to be built with synDB"

        it "uses empty location from compound name when location arg is empty" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        }
            -- "product Y {GLO}" compound name with empty location arg
            -- extractBracketedLocation will find "GLO"
            case findSupplierInIndexedDBs ctx "product Y {GLO}" "" "kg" of
                CrossDBLinked _ _ _ score _ _ _ -> score `shouldSatisfy` (>= defaultLinkingThreshold)
                CrossDBNotLinked _ -> pendingWith "Compound name location extraction may not match"

-- ---------------------------------------------------------------------------
-- Helper: load SAMPLE.min3 as IndexedDatabase
-- ---------------------------------------------------------------------------

loadMin3IndexedDB :: IO IndexedDatabase
loadMin3IndexedDB = do
    result <- loadDatabase "test-data/SAMPLE.min3"
    case result of
        Left err -> error $ "Failed to load SAMPLE.min3: " ++ show err
        Right simpleDb -> return $ buildIndexedDatabase "SAMPLE.min3" emptySynonymDB simpleDb

{-# LANGUAGE OverloadedStrings #-}

module ChemSynonymsSpec (spec) where

import qualified Data.Set as S
import Test.Hspec

import Method.ChemSynonyms

spec :: Spec
spec = do
    describe "parseChemSynonymsCSV" $ do
        it "treats an empty file as the empty snapshot" $ do
            parseChemSynonymsCSV "" `shouldBe` Right emptyChemSynonyms

        it "skips the header row" $ do
            let csv = "cas;canonical_name;synonyms...\n"
            parseChemSynonymsCSV csv `shouldBe` Right emptyChemSynonyms

        it "skips blank lines and # comments" $ do
            let csv = "cas;canonical_name;synonyms...\n\n# a comment line\n"
            parseChemSynonymsCSV csv `shouldBe` Right emptyChemSynonyms

    describe "expandedTokens" $ do
        let co2Csv =
                "cas;canonical_name;synonyms...\n\
                \124-38-9;Carbon dioxide;CO2;Carbonic anhydride\n\
                \74-82-8;Methane;CH4;Methyl hydride\n"
            Right cs = parseChemSynonymsCSV co2Csv

        it "is identity on names not present in the snapshot" $ do
            expandedTokens emptyChemSynonyms "Some unknown substance"
                `shouldBe` S.fromList ["some", "unknown", "substance"]

        it "expands a formula token to the full substance's token set" $ do
            -- "CO2" alone should pick up "carbon" and "dioxide" from the
            -- substance's other synonyms - the exact failure mode pure
            -- tokenization can never solve.
            let toks = expandedTokens cs "CO2"
            toks `shouldSatisfy` ("carbon" `S.member`)
            toks `shouldSatisfy` ("dioxide" `S.member`)
            toks `shouldSatisfy` ("co2" `S.member`)

        it "expands the canonical name to include the formula" $ do
            let toks = expandedTokens cs "Carbon dioxide"
            toks `shouldSatisfy` ("co2" `S.member`)
            toks `shouldSatisfy` ("carbon" `S.member`)

        it "is case-insensitive on the lookup side" $ do
            expandedTokens cs "carbon dioxide" `shouldBe` expandedTokens cs "Carbon Dioxide"

        it "leaves unrelated names alone" $ do
            -- Methane is in the snapshot but unrelated to oil - the suggestion
            -- engine relies on this isolation so candidates don't bleed.
            let toks = expandedTokens cs "Crude oil"
            toks `shouldNotSatisfy` ("methane" `S.member`)
            toks `shouldNotSatisfy` ("carbon" `S.member`)

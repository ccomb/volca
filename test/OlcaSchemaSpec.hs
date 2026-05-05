{-# LANGUAGE OverloadedStrings #-}

module OlcaSchemaSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MethodTables (..), buildMethodTables)
import Method.Parser.OlcaSchema (isOlcaImpactCategoryJson, parseOlcaImpactCategoryBytes)
import Method.Types

spec :: Spec
spec = do
    describe "isOlcaImpactCategoryJson" $ do
        it "recognizes an openLCA ImpactCategory" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            isOlcaImpactCategoryJson bytes `shouldBe` True

        it "rejects unrelated JSON" $ do
            isOlcaImpactCategoryJson "{\"foo\": 1}" `shouldBe` False
            isOlcaImpactCategoryJson "[1,2,3]" `shouldBe` False
            isOlcaImpactCategoryJson "not json" `shouldBe` False

        it "rejects another openLCA entity type" $
            -- The auto-detection must not pull in Process / Flow / etc. files
            -- that may sit in the same method directory.
            isOlcaImpactCategoryJson "{\"@type\":\"Process\",\"name\":\"x\"}"
                `shouldBe` False

    describe "parseOlcaImpactCategoryBytes" $ do
        it "parses the mini fixture and yields one MethodCF per ImpactFactor" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    methodName method `shouldBe` "Regional LCIA Mini"
                    methodUnit method `shouldBe` "m2*year"
                    length (methodFactors method) `shouldBe` 4

        it "preserves location code, value, and flow UUID on each cell" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    let factors = methodFactors method
                        landFR =
                            head
                                [ f
                                | f <- factors
                                , mcfFlowName f == "Occupation, agriculture"
                                , mcfConsumerLocation f == Just "FR"
                                ]
                        landGLO =
                            head
                                [ f
                                | f <- factors
                                , mcfFlowName f == "Occupation, agriculture"
                                , mcfConsumerLocation f == Just "GLO"
                                ]
                    mcfValue landFR `shouldBe` 22.15
                    mcfValue landGLO `shouldBe` 10.0
                    -- The fixture's flow @id round-trips into mcfFlowRef
                    UUID.toText (mcfFlowRef landFR)
                        `shouldBe` "0305b169-255d-4041-8f5d-6e095bcb6358"

        it "rejects a non-object top level" $
            case parseOlcaImpactCategoryBytes "[1,2,3]" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse failure on array root"

        it "rejects a wrong @type" $
            case parseOlcaImpactCategoryBytes "{\"@type\":\"Process\",\"name\":\"x\"}" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse failure on @type=Process"

    describe "buildMethodTables on parsed openLCA methods" $ do
        it "leaves mtRegionalizedCF empty when no flow matched (Nothing in mappings)" $ do
            -- Without database flows to match against, every CF stays unmapped, so
            -- the regionalized table is empty (it only indexes successfully-matched
            -- cells). Documents the contract: regional indexing requires a DB match
            -- by name/UUID/CAS/synonym first.
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    let mappings = [(cf, Nothing) | cf <- methodFactors method]
                        tables = buildMethodTables mappings
                    M.size (mtRegionalizedCF tables) `shouldBe` 0

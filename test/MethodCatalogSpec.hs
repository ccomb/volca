{-# LANGUAGE OverloadedStrings #-}

module MethodCatalogSpec (spec) where

import Data.Char (isHexDigit, isUpper)
import Data.List (nub)
import qualified Data.Text as T
import Test.Hspec

import Method.Catalog (CatalogEntry (..), methodCatalog, parseMethodCatalog)

entry :: T.Text -> T.Text
entry extra =
    T.unlines
        [ "[[collection]]"
        , "name = \"A method\""
        , "publisher = \"Someone\""
        , "version = \"1\""
        , "source_url = \"https://example.org\""
        , "size = 3"
        , "sha256 = \"" <> T.replicate 64 "0" <> "\""
        , "description = \"Three bytes.\""
        , extra
        ]

spec :: Spec
spec = describe "Method catalogue" $ do
    it "reads the catalogue the binary carries, one name per entry" $
        case methodCatalog of
            Left err -> expectationFailure (T.unpack err)
            Right entries -> do
                entries `shouldNotBe` []
                let names = map ceName entries
                nub names `shouldBe` names
                mapM_ (\e -> (T.length (ceSha256 e), T.all (\c -> isHexDigit c && not (isUpper c)) (ceSha256 e)) `shouldBe` (64, True)) entries
                mapM_ (\e -> ceSize e `shouldSatisfy` (> 0)) entries

    it "reads a licence as its SPDX identifier, and none as unstated" $ do
        fmap (map ceLicence) (parseMethodCatalog (entry "url = \"https://example.org/f\""))
            `shouldBe` Right [Nothing]
        fmap (map (fmap show . ceLicence)) (parseMethodCatalog (entry "url = \"https://example.org/f\"\nlicence = \"CC-BY-SA-4.0\""))
            `shouldBe` Right [Just "CCBYSA"]

    it "refuses a licence it cannot read" $
        parseMethodCatalog (entry "url = \"https://example.org/f\"\nlicence = \"Free for all\"")
            `shouldSatisfy` either (const True) (const False)

    it "refuses a file not downloaded over https" $
        either (T.isInfixOf "https") (const False) (parseMethodCatalog (entry "url = \"http://example.org/f\""))
            `shouldBe` True

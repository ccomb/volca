{-# LANGUAGE OverloadedStrings #-}

module LicensesSpec (spec) where

import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Vector as V
import Network.HTTP.Types (hContentType, statusCode)
import Network.Wai (responseHeaders, responseStatus)
import Test.Hspec

import API.Licenses (licensesJson, licensesResponse)

-- The /api/v1/licenses payload is a stable contract: clients (web SPA, desktop,
-- pyvolca) decode it without negotiation. These tests guard the shape so a
-- refactor doesn't silently break a downstream renderer.

spec :: Spec
spec = describe "/api/v1/licenses payload" $ do
    it "is valid JSON" $ do
        let parsed = decode licensesJson :: Maybe Value
        parsed `shouldSatisfy` isJust

    it "declares the engine as Apache-2.0" $ do
        let lic = lookupPath ["engine", "license"] =<< decode licensesJson
        lic `shouldBe` Just (String "Apache-2.0")

    it "keeps the components key clients decode, with nothing to list" $ do
        let cs = lookupPath ["components"] =<< decode licensesJson
        cs `shouldBe` Just (Array V.empty)

    it "points clients at the Haskell-deps markdown" $ do
        let url = lookupPath ["haskell_dependencies_url"] =<< decode licensesJson
        url `shouldBe` Just (String "https://github.com/ccomb/volca/blob/main/THIRD_PARTY_LICENSES.md")

    it "wraps the JSON in a 200 response with the right content-type" $ do
        statusCode (responseStatus licensesResponse) `shouldBe` 200
        lookup hContentType (responseHeaders licensesResponse)
            `shouldBe` Just "application/json; charset=utf-8"

-- helpers

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] v = Just v
lookupPath (k : ks) (Object o) = lookupPath ks =<< KM.lookup (Key.fromText k) o
lookupPath _ _ = Nothing

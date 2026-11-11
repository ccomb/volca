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

    it "exposes a non-empty components list" $ do
        let cs = lookupPath ["components"] =<< decode licensesJson
        case cs of
            Just (Array v) -> V.length v `shouldSatisfy` (>= 4)
            _ -> expectationFailure "components is missing or not an array"

    it "lists the ordering the solver takes from AMD as a BSD-3-Clause component" $ do
        let ordering = do
                Array cs <- lookupPath ["components"] =<< decode licensesJson
                find (componentNamed "AMD ordering (ported in faer)") (V.toList cs)
        ordering `shouldSatisfy` isJust
        let licField = lookupPath ["license"] =<< ordering
        licField `shouldBe` Just (String "BSD-3-Clause")

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

componentNamed :: Text -> Value -> Bool
componentNamed expected v = case lookupPath ["name"] v of
    Just (String n) -> n == expected
    _ -> False

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x : xs) = if p x then Just x else find p xs

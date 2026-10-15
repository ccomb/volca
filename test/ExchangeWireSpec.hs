{-# LANGUAGE OverloadedStrings #-}

{- | An exchange says on the wire how its source designates its supplier.

The claim is a sum type inside the engine, and a client has to be able to read
it: which constructor, and what it carries. This holds the two shapes that
answer that - the key on a line, and the property on the schema a client
generates from.
-}
module ExchangeWireSpec (spec) where

import Data.Aeson (Key, Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import Data.OpenApi (toSchema)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import Types

spec :: Spec
spec = describe "the wire shape of an exchange" $ do
    it "declares the claim on both lines that carry one" $
        declaring "supplierClaim" `shouldBe` ["TechnosphereExchange", "WasteExchange"]

    it "says nothing of it on a biosphere line, which designates no supplier" $
        keyOf "supplierClaim" (toJSON bioEx) `shouldBe` Nothing

    it "names the activity a row claims by name" $
        keyOf "supplierClaim" (toJSON techEx{techSupplierClaim = ClaimByName "market for electricity"})
            `shouldBe` Just (object ["tag" .= ("ClaimByName" :: T.Text), "contents" .= ("market for electricity" :: T.Text)])

    it "carries nothing beside the tag when the product row is the claim" $
        keyOf "supplierClaim" (toJSON techEx)
            `shouldBe` Just (object ["tag" .= ("ClaimByProduct" :: T.Text)])

-- | The value an object gives a key, when it is an object and gives it one.
keyOf :: Key -> Value -> Maybe Value
keyOf k (Object o) = KM.lookup k o
keyOf _ _ = Nothing

{- | Titles of the exchange constructors whose published schema declares @key@,
read from the schema as a consumer reads it: as JSON.
-}
declaring :: Key -> [T.Text]
declaring key =
    [ title
    | Just (Array members) <- [keyOf "oneOf" (toJSON (toSchema (Proxy :: Proxy Exchange)))]
    , member <- V.toList members
    , Just (String title) <- [keyOf "title" member]
    , Just props <- [keyOf "properties" member]
    , Just _ <- [keyOf key props]
    ]

techEx :: Exchange
techEx =
    TechnosphereExchange
        { techFlowId = UUID.nil
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techRole = Input
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

bioEx :: Exchange
bioEx =
    BiosphereExchange
        { bioFlowId = UUID.nil
        , bioAmount = 1.0
        , bioUnitId = UUID.nil
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        , bioFormula = Nothing
        }

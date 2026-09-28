{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for 'selectMethod', the collection-aware method resolver behind
the MCP tools and the REST method routes.

A method's engine UUID is a UUIDv5 of its name, so the same UUID can be loaded
under several collections (e.g. two EF 3.1 versions). Resolving must be loud,
not first-match: an ambiguous UUID has to surface the collections to choose
from instead of silently picking whichever loaded first.
-}
module MethodResolveSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import API.Routes (MethodRefusal (..), selectMethod)
import Method.Types (Method (..))
import Types (UUID)

import CrossDBRegionalLCIAFixture (mkUUID)

sharedId :: UUID
sharedId = mkUUID 4242

otherId :: UUID
otherId = mkUUID 777

collA :: Text
collA = "Environmental Footprint 3.1 (adapted) 1.0"

collB :: Text
collB = "Environmental Footprint 3.1 (adapted) 1.03"

-- | A bare named method carrying the given UUID; CF list is irrelevant here.
mkMethod :: UUID -> Method
mkMethod uuid =
    Method
        { methodId = uuid
        , methodName = "Resource use, fossils"
        , methodDescription = Nothing
        , methodUnit = "MJ"
        , methodCategory = "Resource use, fossils"
        , methodMethodology = Nothing
        , methodFactors = []
        }

-- | Both collections carry the colliding UUID.
bothLoaded :: [(Text, Method)]
bothLoaded = [(collA, mkMethod sharedId), (collB, mkMethod sharedId)]

spec :: Spec
spec = describe "selectMethod" $ do
    it "infers the single match when only one collection carries the UUID" $
        fmap fst (selectMethod Nothing sharedId [(collA, mkMethod sharedId)])
            `shouldBe` Right collA

    it "fails loudly on an ambiguous UUID, naming every collection" $
        case selectMethod Nothing sharedId bothLoaded of
            Right _ -> expectationFailure "expected ambiguity error, got a match"
            Left (MethodNotFound msg) -> expectationFailure ("expected ambiguity, got not-found: " <> T.unpack msg)
            Left (MethodAmbiguous msg) -> do
                msg `shouldSatisfy` T.isInfixOf collA
                msg `shouldSatisfy` T.isInfixOf collB
                msg `shouldSatisfy` T.isInfixOf "collection"

    it "resolves within the pinned collection when the UUID is ambiguous" $
        fmap fst (selectMethod (Just collB) sharedId bothLoaded)
            `shouldBe` Right collB

    it "errors when the pinned collection lacks the UUID, listing what is loaded" $
        case selectMethod (Just "EF 3.1 (adapted) 9.9") sharedId bothLoaded of
            Right _ -> expectationFailure "expected not-found error, got a match"
            Left (MethodAmbiguous msg) -> expectationFailure ("expected not-found, got ambiguity: " <> T.unpack msg)
            Left (MethodNotFound msg) -> do
                msg `shouldSatisfy` T.isInfixOf collA
                msg `shouldSatisfy` T.isInfixOf collB

    it "reports not-found for a UUID that is loaded nowhere" $
        selectMethod Nothing otherId [(collA, mkMethod sharedId)]
            `shouldSatisfy` isLeft
  where
    isLeft = either (const True) (const False)

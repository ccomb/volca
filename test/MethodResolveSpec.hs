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

import API.Routes (CollectionAsk (..), MethodRefusal (..), chooseCollection, selectMethod)
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
spec = do
    describe "chooseCollection" $ do
        it "takes the collection named, when it is loaded" $
            chooseCollection (Named collB) [collA, collB] `shouldBe` Just collB
        it "takes no collection named that is not loaded" $
            chooseCollection (Named "EF 9.9") [collA, collB] `shouldBe` Nothing
        it "takes the only candidate, whatever the default" $
            chooseCollection (Unnamed (Just collB)) [collA] `shouldBe` Just collA
        it "takes the default among several candidates" $
            chooseCollection (Unnamed (Just collB)) [collA, collB] `shouldBe` Just collB
        it "takes nothing when the default is not among the candidates" $
            chooseCollection (Unnamed (Just "EF 9.9")) [collA, collB] `shouldBe` Nothing
        it "takes nothing among several candidates without a default" $
            chooseCollection (Unnamed Nothing) [collA, collB] `shouldBe` Nothing
    selectMethodSpec

selectMethodSpec :: Spec
selectMethodSpec = describe "selectMethod" $ do
    it "takes the default collection when several carry the UUID" $
        fmap fst (selectMethod (Unnamed (Just collB)) sharedId bothLoaded)
            `shouldBe` Right collB

    it "says the default is missing when it does not carry the UUID" $
        case selectMethod (Unnamed (Just "EF 9.9")) sharedId bothLoaded of
            Left (MethodAmbiguous msg) -> msg `shouldSatisfy` T.isInfixOf "EF 9.9"
            other -> expectationFailure ("expected ambiguity, got " <> show (fmap fst other))

    it "infers the single match when only one collection carries the UUID" $
        fmap fst (selectMethod (Unnamed Nothing) sharedId [(collA, mkMethod sharedId)])
            `shouldBe` Right collA

    it "fails loudly on an ambiguous UUID, naming every collection" $
        case selectMethod (Unnamed Nothing) sharedId bothLoaded of
            Right _ -> expectationFailure "expected ambiguity error, got a match"
            Left (MethodNotFound msg) -> expectationFailure ("expected ambiguity, got not-found: " <> T.unpack msg)
            Left (MethodAmbiguous msg) -> do
                msg `shouldSatisfy` T.isInfixOf collA
                msg `shouldSatisfy` T.isInfixOf collB
                msg `shouldSatisfy` T.isInfixOf "collection"

    it "resolves within the pinned collection when the UUID is ambiguous" $
        fmap fst (selectMethod (Named collB) sharedId bothLoaded)
            `shouldBe` Right collB

    it "errors when the pinned collection lacks the UUID, listing what is loaded" $
        case selectMethod (Named "EF 3.1 (adapted) 9.9") sharedId bothLoaded of
            Right _ -> expectationFailure "expected not-found error, got a match"
            Left (MethodAmbiguous msg) -> expectationFailure ("expected not-found, got ambiguity: " <> T.unpack msg)
            Left (MethodNotFound msg) -> do
                msg `shouldSatisfy` T.isInfixOf collA
                msg `shouldSatisfy` T.isInfixOf collB

    it "reports not-found for a UUID that is loaded nowhere" $
        selectMethod (Unnamed Nothing) otherId [(collA, mkMethod sharedId)]
            `shouldSatisfy` isLeft
  where
    isLeft = either (const True) (const False)

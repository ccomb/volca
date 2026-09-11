{-# LANGUAGE OverloadedStrings #-}

{- | What shape does the published spec give the @collection@ parameter?

A method collection is named by a path capture on some routes and by a query
parameter on others, and the Haskell type behind those positions is what
decides the schema the published spec carries. 'ResourcesDriftSpec' compares
parameter /names/ only, so a parameter whose schema changed underneath keeps
every test in this suite green. This is the test that notices, and it is what
lets the type behind a capture change without changing the contract pyvolca is
generated from.

No server and no database: the spec is a pure value.
-}
module CollectionParamSpec (spec) where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.OpenApi (OpenApiType (..), Operation, Param, ParamLocation (..), PathItem, Referenced (..), Schema)
import qualified Data.OpenApi.Lens as OA
import Data.Text (Text)
import Test.Hspec

import API.Routes (volcaOpenApi)

-- | Every parameter named @collection@ the published spec carries.
collectionParams :: [Param]
collectionParams =
    [ p
    | item <- toList (volcaOpenApi ^. OA.paths)
    , op <- operationsOf item
    , Inline p <- op ^. OA.parameters
    , p ^. OA.name == "collection"
    ]

-- | The operations a path item carries, whatever verb they answer.
operationsOf :: PathItem -> [Operation]
operationsOf item =
    mapMaybe
        (item ^.)
        [OA.get, OA.put, OA.post, OA.delete, OA.patch, OA.head_, OA.options]

-- | The parameters named @collection@ that sit in one place.
paramsIn :: ParamLocation -> [Param]
paramsIn loc = [p | p <- collectionParams, p ^. OA.in_ == loc]

{- | What the published spec says a parameter is, in the words a reader of the
document would use. Written as a sentence rather than a predicate so a failure
says which of the four things it became.
-}
shapeOf :: Param -> Text
shapeOf p = case p ^. OA.schema of
    Nothing -> "no schema at all"
    Just (Ref _) -> "a named schema reference"
    Just (Inline s) -> inlineShape s

inlineShape :: Schema -> Text
inlineShape s
    | s ^. OA.type_ /= Just OpenApiString = "not a string"
    | not (null (s ^. OA.format)) = "a string carrying a format"
    | not (null (s ^. OA.enum_)) = "a string carrying an enumeration"
    | otherwise = "a plain string"

spec :: Spec
spec = describe "the collection parameter of the published spec" $ do
    it "appears both as a path capture and as a query parameter" $ do
        paramsIn ParamPath `shouldSatisfy` not . null
        paramsIn ParamQuery `shouldSatisfy` not . null

    it "is a plain string in every one of them" $
        nub (map shapeOf collectionParams) `shouldBe` ["a plain string"]

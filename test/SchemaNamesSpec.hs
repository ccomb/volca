{-# LANGUAGE OverloadedStrings #-}

{- | Does each record the published spec describes keep its own fields?

The spec files a record's schema under the record's type name, and two types of
one name in different modules collide there without a word: the one declared
last replaces the other, and every route reaching the other is described with
the wrong fields. The server is untouched, so only the spec can show it.

No server and no database: the spec is a pure value.
-}
module SchemaNamesSpec (spec) where

import Control.Lens (asIndex, at, itraversed, (^.), (^..))
import Data.List (sort)
import qualified Data.OpenApi.Lens as OA
import Data.Text (Text)
import Test.Hspec

import API.Routes (volcaOpenApi)

-- | The fields of the schema filed under a name, when one is.
fieldsOf :: Text -> Maybe [Text]
fieldsOf name = sort . (^.. OA.properties . itraversed . asIndex) <$> volcaOpenApi ^. OA.components . OA.schemas . at name

spec :: Spec
spec = describe "Schema names" $ do
    it "describes a what-if substitution by the process it replaces and the one it takes" $
        fieldsOf "Substitution" `shouldBe` Just ["consumer", "from", "to"]

    it "describes a database chosen for a required release apart from it" $
        fieldsOf "ReleaseSubstitution" `shouldBe` Just ["database", "release"]

{-# LANGUAGE OverloadedStrings #-}

{- | A database, a method collection and a reference source are each reached by
name, and what reaches those indexes is the configuration concatenated with the
uploads directory. A name held twice therefore drops one of its bearers, and
this is what says so.
-}
module ShadowedNamesSpec (spec) where

import Config (MethodConfig (..))
import Data.Text (Text)
import Database.Manager (shadowedMethods, shadowedNames)
import Test.Hspec

named :: [(Text, String)] -> [String]
named = shadowedNames "database" fst snd

spec :: Spec
spec = describe "shadowedNames" $ do
    it "says nothing when every name is held once" $
        named [("one", "a.toml"), ("other", "b.toml")] `shouldBe` []

    it "names the entry that is read and the one that is not" $
        named [("one", "configured.toml"), ("one", "uploads/one")]
            `shouldBe` ["More than one database named one; reading uploads/one, ignoring configured.toml"]

    it "names every entry a winner hides, in the order they were given" $
        named [("one", "first"), ("one", "second"), ("one", "third")]
            `shouldBe` ["More than one database named one; reading third, ignoring first, second"]

    it "reports one line per name, not one per entry" $
        length (named [("one", "a"), ("other", "b"), ("one", "c"), ("other", "d")]) `shouldBe` 2

    -- A method collection answers through two registries, and under a repeated
    -- name they answer with two different collections: the listing keeps the
    -- last entry, the boot load takes the active one, and an uploaded
    -- collection is never active.
    it "names both registries for a method collection, rather than a winner" $
        shadowedMethods [collection "one" "configured", collection "one" "uploads/one"]
            `shouldBe` ["More than one method collection named one; listed from uploads/one and loaded from configured, so the name answers with two different collections"]

collection :: Text -> FilePath -> MethodConfig
collection name path =
    MethodConfig
        { mcName = name
        , mcPath = path
        , mcActive = True
        , mcIsUploaded = False
        , mcDescription = Nothing
        , mcFormat = Nothing
        , mcScoringSets = []
        , mcGlobalMethods = []
        , mcPatches = []
        }

{-# LANGUAGE OverloadedStrings #-}

{- | A database, a method collection and a reference source are each reached by
name, and what reaches those indexes is the configuration concatenated with the
uploads directory. A name held twice therefore drops one of its bearers, and
this is what says so.
-}
module ShadowedNamesSpec (spec) where

import Data.Text (Text)
import Database.Manager (shadowedNames)
import Test.Hspec

named :: [(Text, String)] -> [String]
named = shadowedNames "database" fst snd

spec :: Spec
spec = describe "shadowedNames" $ do
    it "says nothing when every name is held once" $
        named [("agri", "a.toml"), ("eco", "b.toml")] `shouldBe` []

    it "names the entry that is read and the one that is not" $
        named [("agri", "configured.toml"), ("agri", "uploads/agri")]
            `shouldBe` ["More than one database named agri; reading uploads/agri, ignoring configured.toml"]

    it "names every entry a winner hides, in the order they were given" $
        named [("agri", "first"), ("agri", "second"), ("agri", "third")]
            `shouldBe` ["More than one database named agri; reading third, ignoring first, second"]

    it "reports one line per name, not one per entry" $
        length (named [("agri", "a"), ("eco", "b"), ("agri", "c"), ("eco", "d")]) `shouldBe` 2

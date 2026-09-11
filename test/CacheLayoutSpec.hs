{-# LANGUAGE OverloadedStrings #-}

{- | The database cache is bytes, and naming a shape it holds must not move them.

'Database.Loader.schemaSignature' hashes the identity of 'Types.Database' and
nothing inside it, so a layout that shifted under a cache built before the shift
would not be rejected, it would be decoded as the layout it is not. Wrapping a value in a record or a newtype is meant to cost exactly nothing
here: a type with one constructor carries no tag, so it pokes the fields it
wraps, in the order it wraps them. This says that out loud, and fails the day
it stops being true.
-}
module CacheLayoutSpec (spec) where

import qualified Data.Map as M
import Data.Store (encode)
import Test.Hspec

import Types (LinkBlocker (..), UnresolvedProduct (..))

spec :: Spec
spec =
    describe "the shapes the database cache holds" $
        it "records an unsupplied product as the reason tally it wraps" $
            encode unresolved `shouldBe` encode (upBlockers unresolved)
  where
    unresolved :: UnresolvedProduct
    unresolved = UnresolvedProduct (M.fromList [(LocationUnavailable "FR", 7), (NoNameMatch, 2)])

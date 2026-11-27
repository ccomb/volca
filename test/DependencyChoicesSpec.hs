{-# LANGUAGE OverloadedStrings #-}

module DependencyChoicesSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Config (DatabaseConfig (..), Dependencies (..), DependencyOrigin (..), declaredDepends)
import Database.DependencyChoices
import Database.Requirements (linksTo)
import TestHelpers (dbConfigFor)

on :: Text -> Dependencies -> DatabaseConfig
on name deps = (dbConfigFor name){dcDepends = deps}

spec :: Spec
spec = do
    describe "the dependencies a configuration declares" $ do
        it "settle nothing when absent or empty, and exactly the ones named otherwise" $ do
            declaredDepends [] `shouldBe` Unsettled
            declaredDepends ["mill"] `shouldBe` Settled Configured ["mill"]

    describe "the file the engine keeps" $ do
        it "reads back what it wrote, a choice of none included" $ do
            let configs = [on "bakery" (Settled Clicked []), on "brewery" (Settled Found ["mill", "farm \"north\""]), on "mill" (Settled Configured ["farm"]), on "farm" Unsettled]
            decodeKept (renderKept configs)
                `shouldBe` Right (M.fromList [("bakery", Kept ByClick []), ("brewery", Kept ByLinking ["mill", "farm \"north\""])])

        it "says why a file does not read" $
            decodeKept "[bakery]\norigin = \"guessed\"\ndepends = []\n" `shouldSatisfy` either (const True) (const False)

    describe "a database's dependencies, once the file is read" $ do
        let kept = M.fromList [("clicked", Kept ByClick []), ("found", Kept ByLinking ["mill"])]
        it "are the ones clicked, over the configuration" $
            dcDepends (settleWith kept (on "clicked" (declaredDepends ["mill"]))) `shouldBe` Settled Clicked []
        it "are the configuration's, over the ones linking found" $
            dcDepends (settleWith kept (on "found" (declaredDepends ["farm"]))) `shouldBe` Settled Configured ["farm"]
        it "are the ones linking found, when the configuration names none" $
            dcDepends (settleWith kept (on "found" Unsettled)) `shouldBe` Settled Found ["mill"]
        it "stay unsettled when nothing names any" $
            dcDepends (settleWith kept (on "other" Unsettled)) `shouldBe` Unsettled
        it "say when a click replaces the configuration's" $
            overriddenDeclarations kept [on "clicked" (declaredDepends ["mill"]), on "found" (declaredDepends ["farm"])]
                `shouldBe` ["clicked links to nothing, as chosen on its setup page, not to the mill its configuration names"]

    describe "the databases a database links to" $ do
        let configs = M.fromList [(dcName c, c) | c <- [on "mill" Unsettled, on "farm" Unsettled]]
        it "are every one when it has settled none" $
            map (linksTo configs (on "bakery" Unsettled)) ["mill", "farm"] `shouldBe` [True, True]
        it "are only the ones settled otherwise, none included" $ do
            map (linksTo configs (on "bakery" (Settled Found ["mill"]))) ["mill", "farm"] `shouldBe` [True, False]
            map (linksTo configs (on "bakery" (Settled Clicked []))) ["mill", "farm"] `shouldBe` [False, False]

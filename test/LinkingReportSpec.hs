{-# LANGUAGE OverloadedStrings #-}

module LinkingReportSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Loader (UnlinkedExchange (..), UnlinkedSummary (..), leftForADependency)
import Test.Hspec

-- | A summary as the within-database pass leaves it: one consumer, its inputs.
summaryOf :: [(T.Text, [UnlinkedExchange])] -> UnlinkedSummary
summaryOf activities =
    mempty{usActivities = M.fromList activities}

buys :: T.Text -> T.Text -> UnlinkedExchange
buys = UnlinkedExchange

spec :: Spec
spec = describe "the inputs a database does not answer itself" $ do
    it "says nothing when every input found its supplier here" $
        leftForADependency (summaryOf []) `shouldBe` []

    it "names the activity, its inputs and where each is bought" $
        leftForADependency
            (summaryOf [("Steel production", [buys "aluminium, wrought alloy" "GLO", buys "hard coal" "CN"])])
            `shouldBe` [ "Left for a dependency, by the activity that buys them:"
                       , "  - Steel production: 2 inputs"
                       , "      * aluminium, wrought alloy [GLO]"
                       , "      * hard coal [CN]"
                       ]

    -- A row that states no geography buys the product wherever it is made.
    it "leaves out the brackets when the input stated no location" $
        leftForADependency (summaryOf [("Steel production", [buys "hard coal" ""])])
            `shouldSatisfy` elem "      * hard coal"

    {- The word a reader acts on. These inputs are not missing: the cross-database
    pass is asked next, and only what it cannot answer is a missing supplier.
    -}
    it "never calls them missing suppliers" $
        leftForADependency (summaryOf [("Steel production", [buys "hard coal" "CN"])])
            `shouldSatisfy` not . any (T.isInfixOf "missing")

    it "keeps ten activities and three inputs each, counting the rest" $ do
        let manyInputs = [buys (T.pack ("product " <> show i)) "GLO" | i <- [1 :: Int .. 5]]
            lines' = leftForADependency (summaryOf [(T.pack ("activity " <> show i), manyInputs) | i <- [1 :: Int .. 12]])
        head lines' `shouldBe` "Left for a dependency, by the activity that buys them:"
        last lines' `shouldBe` "  ... and 2 more activities"
        length (filter (T.isPrefixOf "  - ") lines') `shouldBe` 10
        lines' `shouldSatisfy` elem "      ... and 2 more"

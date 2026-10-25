{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the system model file ('Database.SystemModel'): what it refuses
to read, and what it takes from a parent.

A model is read strictly, so every way of writing a rule the engine would not
apply is a refusal naming it; and it is checked against the parent before any
block is divided, so a rule that takes nothing, a process it names wrongly, or
a block two rules take, is a refusal too.
-}
module SystemModelSpec (spec) where

import Data.Either (fromLeft, isRight)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Database.SystemModel (Resolution (..), parseSystemModel, resolveModel)
import TestHelpers (mkActivity, reference)
import Types (Activity (..), AllocationProperty (..), Exchange (..), SystemModel (..), TechRole (..))

spec :: Spec
spec = do
    describe "parseSystemModel" $ do
        it "reads a model and keeps its text as written" $ do
            let text = model ["default = \"price\""]
            fmap smDefault (parseSystemModel text) `shouldBe` Right Price
            fmap smText (parseSystemModel text) `shouldBe` Right text

        it "refuses a key nothing reads" $
            refusal (model ["default = \"price\""] <> "[recycling]\nrule = \"cut-off\"\n") `shouldSatisfy` isInfixOf "recycling"

        it "refuses a default that is not a property" $
            refusal (model ["default = \"declared\""]) `shouldSatisfy` isInfixOf "not by declared"

        it "refuses an exception without its reason" $
            refusal (model ("default = \"price\"" : exception ["key = \"wet mass\"", "reason = \"  \"", "processes = [\"" <> uuidText cheese <> "\"]"])) `shouldSatisfy` isInfixOf "needs its reason"

        it "refuses a field missing inside an exception, rather than losing the exception" $ do
            refusal (model ("default = \"price\"" : exception ["key = \"wet mass\"", "processes = [\"" <> uuidText cheese <> "\"]"])) `shouldSatisfy` isInfixOf "reason"
            refusal (model ("default = \"price\"" : exception ["key = \"wet mass\"", "reason = \"Dairy.\"", "classification = { system = \"Category\" }"]))
                `shouldSatisfy` isInfixOf "value"

        it "refuses an exception that takes no process, or takes them twice over" $ do
            refusal (model ("default = \"price\"" : exception ["key = \"wet mass\"", "reason = \"Dairy.\""])) `shouldSatisfy` isInfixOf "takes no process"
            refusal (model ("default = \"price\"" : exception ["key = \"wet mass\"", "reason = \"Dairy.\"", "processes = [\"" <> uuidText cheese <> "\"]", category "Dairy"]))
                `shouldSatisfy` isInfixOf "writes both"

        it "refuses an exception keyed like the default" $
            refusal (model ("default = \"price\"" : exception ["key = \"price\"", "reason = \"Dairy.\"", category "Dairy"])) `shouldSatisfy` isInfixOf "changes nothing"

    describe "resolveModel" $ do
        it "divides the blocks an exception takes by its property, the others by the default" $
            fmap rsKeys (resolve (exception ["key = \"wet mass\"", "reason = \"Dairy.\"", category "Dairy"]))
                `shouldBe` Right (M.fromList [(cheese, WetMass), (oven, Price)])

        it "takes a block by its activity id" $
            fmap rsKeys (resolve (exception ["key = \"wet mass\"", "reason = \"Ovens.\"", "processes = [\"" <> uuidText oven <> "\"]"]))
                `shouldBe` Right (M.fromList [(cheese, Price), (oven, WetMass)])

        it "takes a block once, however many times its id is written" $
            fmap (map snd . rsTaken) (resolve (exception ["key = \"wet mass\"", "reason = \"Ovens.\"", "processes = [\"" <> uuidText oven <> "\", \"" <> uuidText oven <> "\"]"]))
                `shouldBe` Right [[oven]]

        it "refuses an id the parent lacks, or one naming a process of one product" $ do
            let named u = resolve (exception ["key = \"wet mass\"", "reason = \"Odd.\"", "processes = [\"" <> uuidText u <> "\"]"])
            problems (named nowhere) `shouldSatisfy` isInfixOf "which the parent does not hold"
            problems (named steel) `shouldSatisfy` isInfixOf "which the parent holds with one product only"

        it "refuses an exception that takes no block" $
            problems (resolve (exception ["key = \"wet mass\"", "reason = \"Mills.\"", category "Milling"]))
                `shouldSatisfy` isInfixOf "takes no process of several products"

        it "refuses a block two exceptions take, naming both" $
            problems (resolve (exception ["key = \"wet mass\"", "reason = \"Dairy.\"", category "Dairy"] <> exception ["key = \"dry mass\"", "reason = \"Cheese.\"", "processes = [\"" <> uuidText cheese <> "\"]"]))
                `shouldSatisfy` isInfixOf "cheese is taken by two exceptions"

        it "resolves a model without exception over every block" $
            isRight (resolve []) `shouldBe` True
  where
    refusal :: Text -> Text
    refusal = fromLeft "" . parseSystemModel

    resolve :: [Text] -> Either (NE.NonEmpty Text) Resolution
    resolve lines' = case parseSystemModel (model ("default = \"price\"" : lines')) of
        Left err -> Left (pure err)
        Right m -> resolveModel m parent

    problems :: Either (NE.NonEmpty Text) Resolution -> Text
    problems = either (T.intercalate "; " . NE.toList) (const "")

-- | A model file whose @[allocation]@ table holds these lines.
model :: [Text] -> Text
model lines' = T.unlines (["name = \"Test\"", "version = \"1.0.0\"", "[allocation]"] <> lines')

exception :: [Text] -> [Text]
exception lines' = "[[allocation.exceptions]]" : lines'

category :: Text -> Text
category value = "classification = { system = \"Category\", value = \"" <> value <> "\" }"

uuidText :: UUID -> Text
uuidText = UUID.toText

-- | A parent of two blocks of two products, a dairy and an oven, and a steel mill of one.
parent :: M.Map UUID Activity
parent =
    M.fromList
        [ (cheese, (block "cheese"){activityClassification = M.singleton "Category" "Dairy"})
        , (oven, (block "oven"){activityClassification = M.singleton "Category" "Bakery"})
        , (steel, (mkActivity "steel" [reference (flow 9)]){activityClassification = M.singleton "Category" "Dairy"})
        ]
  where
    block name = mkActivity name [reference (flow 1), (reference (flow 2)){techRole = Coproduct}]

cheese, oven, steel, nowhere :: UUID
cheese = flow 11
oven = flow 12
steel = flow 13
nowhere = flow 14

flow :: Int -> UUID
flow n = UUID.fromWords 0 0 0 (fromIntegral n)

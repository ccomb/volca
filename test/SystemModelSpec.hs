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

import Database.SystemModel (Resolution (..), Substituted (..), parseSystemModel, resolveModel)
import TestHelpers (mkActivity, reference, techInput)
import Types (Activity (..), AllocationProperty (..), Exchange (..), ModelSubstitution (..), SystemModel (..), TechRole (..), ratioValue)

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

    describe "substitutions" $ do
        it "reads a substitution, its ratio written as an integer" $
            fmap (map (ratioValue . subRatio) . smSubstitutions) (parseSystemModel (model ["default = \"price\""] <> substitution (ref cheese 2) (ref steel 9) "2"))
                `shouldBe` Right [2]

        it "refuses a ratio that is not above zero, or not a number" $
            mapM_
                (\r -> refusal (model ["default = \"price\""] <> substitution (ref cheese 2) (ref steel 9) r) `shouldSatisfy` isInfixOf "ratio above zero")
                ["0", "-1.5", "nan", "inf"]

        it "refuses a process it cannot read, or a substitution without its reason" $ do
            refusal (model ["default = \"price\""] <> substitution (uuidText cheese) (ref steel 9) "1") `shouldSatisfy` isInfixOf "not a process identifier"
            refusal (model ["default = \"price\""] <> T.replace "Whey feeds pigs." " " (substitution (ref cheese 2) (ref steel 9) "1")) `shouldSatisfy` isInfixOf "needs its reason"

        it "resolves a coproduct of a block against a process of the parent" $
            fmap (map (activityName . sdReplaced) . rsSubstitutions) (substituting [(ref cheese 2, ref steel 9)]) `shouldBe` Right ["steel"]

        it "takes any product of a block, its reference too, so long as one is left" $
            isRight (substituting [(ref cheese 1, ref steel 9)]) `shouldBe` True

        it "refuses a process the parent does not make" $ do
            problems (substituting [(ref nowhere 2, ref steel 9)]) `shouldSatisfy` isInfixOf "as its coproduct, which the parent does not make"
            problems (substituting [(ref cheese 2, ref nowhere 9)]) `shouldSatisfy` isInfixOf "as the process it displaces, which the parent does not make"

        it "refuses a coproduct of a process of one product, or one of no amount" $ do
            problems (substituting [(ref steel 9, ref drink 10)]) `shouldSatisfy` isInfixOf "a process of one product"
            problems (resolveIn (M.insert (mill, flow 1) millBlock parent) (substitutions [(ref mill 2, ref steel 9)])) `shouldSatisfy` isInfixOf "displaces nothing"

        it "refuses a product of its own block, a product another substitution takes away, or a process buying the coproduct" $ do
            problems (substituting [(ref cheese 2, ref cheese 1)]) `shouldSatisfy` isInfixOf "a product of the same block"
            problems (substituting [(ref cheese 2, ref steel 9), (ref oven 2, ref cheese 2)]) `shouldSatisfy` isInfixOf "another substitution takes away"
            problems (substituting [(ref cheese 2, ref drink 10)]) `shouldSatisfy` isInfixOf "the line would loop"

        it "refuses a coproduct taken twice, or a block left with no product" $ do
            problems (substituting [(ref cheese 2, ref steel 9), (ref cheese 2, ref drink 10)]) `shouldSatisfy` isInfixOf "is taken by two substitutions"
            problems (substituting [(ref cheese 1, ref steel 9), (ref cheese 2, ref oven 1)]) `shouldSatisfy` isInfixOf "cheese has every product substituted"
  where
    refusal :: Text -> Text
    refusal = fromLeft "" . parseSystemModel

    resolve :: [Text] -> Either (NE.NonEmpty Text) Resolution
    resolve lines' = resolveIn parent (model ("default = \"price\"" : lines'))

    substituting :: [(Text, Text)] -> Either (NE.NonEmpty Text) Resolution
    substituting = resolveIn parent . substitutions

    substitutions :: [(Text, Text)] -> Text
    substitutions pairs = model ["default = \"price\""] <> T.concat [substitution co rep "2" | (co, rep) <- pairs]

    resolveIn :: M.Map (UUID, UUID) Activity -> Text -> Either (NE.NonEmpty Text) Resolution
    resolveIn processes text = case parseSystemModel text of
        Left err -> Left (pure err)
        Right m -> resolveModel m processes

    problems :: Either (NE.NonEmpty Text) Resolution -> Text
    problems = either (T.intercalate "; " . NE.toList) (const "")

-- | A model file whose @[allocation]@ table holds these lines.
model :: [Text] -> Text
model lines' = T.unlines (["name = \"Test\"", "version = \"1.0.0\"", "[allocation]"] <> lines')

-- | A substitution of a coproduct, written after the @[allocation]@ table.
substitution :: Text -> Text -> Text -> Text
substitution coproduct replaces ratio =
    T.unlines ["[[substitutions]]", "coproduct = \"" <> coproduct <> "\"", "replaces = \"" <> replaces <> "\"", "ratio = " <> ratio, "reason = \"Whey feeds pigs.\""]

-- | A process of the parent by its activity and the product numbered so.
ref :: UUID -> Int -> Text
ref activity n = uuidText activity <> "_" <> uuidText (flow n)

exception :: [Text] -> [Text]
exception lines' = "[[allocation.exceptions]]" : lines'

category :: Text -> Text
category value = "classification = { system = \"Category\", value = \"" <> value <> "\" }"

uuidText :: UUID -> Text
uuidText = UUID.toText

{- | A parent of two blocks of two products, a dairy and an oven, a steel mill
of one, and a drink of one product made from the dairy's second, keyed as a
load keys them.
-}
parent :: M.Map (UUID, UUID) Activity
parent =
    M.fromList
        [ ((cheese, flow 1), (block "cheese"){activityClassification = M.singleton "Category" "Dairy"})
        , ((oven, flow 1), (block "oven"){activityClassification = M.singleton "Category" "Bakery"})
        , ((steel, flow 9), (mkActivity "steel" [reference (flow 9)]){activityClassification = M.singleton "Category" "Dairy"})
        , ((drink, flow 10), mkActivity "drink" [reference (flow 10), (techInput (flow 2) 0.5){techActivityLinkId = Just cheese}])
        ]
  where
    block name = mkActivity name [reference (flow 1), (reference (flow 2)){techRole = Coproduct}]

-- | A block whose coproduct row is there and makes nothing.
millBlock :: Activity
millBlock = mkActivity "mill" [reference (flow 1), (reference (flow 2)){techRole = Coproduct, techAmount = 0}]

cheese, oven, steel, nowhere, drink, mill :: UUID
cheese = flow 11
oven = flow 12
steel = flow 13
nowhere = flow 14
drink = flow 15
mill = flow 16

flow :: Int -> UUID
flow n = UUID.fromWords 0 0 0 (fromIntegral n)

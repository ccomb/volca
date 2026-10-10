{-# LANGUAGE OverloadedStrings #-}

{- | The formula languages: what the openLCA one computes, and that the two
older ones compute what they did.
-}
module ExprSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Expr (Dialect (..), Refusal (..), collectIdentifiers, describeRefusal, settle)
import qualified Expr

olca :: [(Text, Double)] -> Text -> Either Refusal Double
olca env = Expr.evaluate OpenLca (M.fromList env)

-- | Within rounding of the expected value; a refusal fails with its reason.
near :: Double -> Either Refusal Double -> Expectation
near expected got = case got of
    Right x -> x `shouldSatisfy` (\v -> abs (v - expected) <= 1e-12 * max 1 (abs expected))
    Left refusal -> expectationFailure (T.unpack (describeRefusal refusal))

-- | A refusal saying it cannot read the formula, mentioning the word given.
unreadableNaming :: Text -> Either Refusal Double -> Expectation
unreadableNaming word got = case got of
    Left (Unreadable reason) -> reason `shouldSatisfy` T.isInfixOf word
    Left (Unresolved names) -> expectationFailure ("unresolved " <> show names)
    Right x -> expectationFailure ("read as " <> show x)

spec :: Spec
spec = do
    describe "the openLCA dialect" $ do
        it "reads log as the decimal logarithm and ln as the natural one" $ do
            near 3 (olca [] "log(1000)")
            near 1 (olca [] "ln(e())")
        it "chains ^ from the left" $
            near 64 (olca [] "2^3^2")
        it "binds a sign tighter than ^" $ do
            near 4 (olca [] "-2^2")
            near 0.5 (olca [] "2^-1")
        it "separates arguments with ;" $
            near 4 (olca [("a", 1), ("b", 4)] "max(a; b)")
        it "gives min, max, sum and avg any number of arguments" $ do
            near 7 (olca [] "max(1; 7; 3)")
            near 2 (olca [] "min(4; 2; 8)")
            near 6 (olca [] "sum(1; 2; 3)")
            near 2 (olca [] "avg(1; 2; 3)")
        it "compares numbers into truth values, below the arithmetic" $ do
            near 4 (olca [] "if(1 < 2; 1; 0) + if(2 <= 2; 1; 0) + if(3 > 4; 1; 0) + if(3 >= 4; 1; 0) + if(1 == 1; 1; 0) + if(1 = 2; 1; 0) + if(1 != 2; 1; 0) + if(1 <> 1; 1; 0)")
            near 1 (olca [] "if(1 + 2 == 3; 1; 0)")
        it "reads the logical operators and functions over truth values" $
            near 4 (olca [] "if(1 < 2 && 2 < 1; 1; 0) + if(1 < 2 & 1 < 2; 1; 0) + if(1 > 2 || 2 > 3; 1; 0) + if(1 > 2 | 1 < 2; 1; 0) + if(not(1 > 2); 1; 0) + if(and(true; true; false); 1; 0) + if(or(false; false; true()); 1; 0) + if(1 < 2 xor 2 < 3; 1; 0)")
        it "refuses to mix a truth value with a number, as openLCA does" $ do
            unreadableNaming "truth" (olca [] "(1 < 2) + 1")
            unreadableNaming "if" (olca [("m", 1)] "if(m; 1; 2)")
            unreadableNaming "truth" (olca [] "1 < 2")
        it "evaluates only the branch of if it takes" $ do
            near 6 (olca [("m", 1), ("a", 2)] "if(m == 1; a * 3; missing)")
            olca [("m", 1)] "if(m == 1; missing; 5)" `shouldBe` Left (Unresolved ("missing" :| []))
        it "nests if, and knows it as iff and iif" $ do
            near 20 (olca [("m", 0.5)] "if(m > 1; 10; if(m > 0; 20; 30))")
            near 2 (olca [] "iff(false; 1; 2)")
            near 1 (olca [] "iif(true(); 1; 2)")
        it "matches function and constant names whatever their case" $
            near 1 (olca [] "IF(TRUE; 1; 2)")
        it "rounds and truncates as openLCA does" $ do
            near 21.25 (olca [] "sqr(3) + round(2.5) + int(-2.7) + frac(1.25) + ceil(1.2) + floor(1.8) + pow(2; 3)")
            near (-2) (olca [] "round(-2.5)")
        it "divides with div and mod as openLCA does" $ do
            near 3 (olca [] "7 div 2")
            near 4 (olca [] "7.6 div 2")
            near (-1) (olca [] "-7 mod 3")
            near 2 (olca [("divisor", 2)] "divisor")
        it "knows the constants bare and as calls, a parameter of that name first" $ do
            near pi (olca [] "pi()")
            near (exp 1) (olca [] "e")
            near 10 (olca [("e", 10)] "e")
            near 1 (olca [] "if(true() & not(false); 1; 0)")
        it "reads a point without leading digits, and $ in a name" $ do
            near 0.5 (olca [] ".5")
            near 2 (olca [("$a", 2)] "$a")
        it "gives the n-ary functions 0 with no argument" $
            near 0 (olca [] "max() + min() + sum() + avg()")
        it "refuses random, which would give another number each load" $ do
            unreadableNaming "random" (olca [] "random()")
            unreadableNaming "rand" (olca [] "rand()")
        it "names the function a wrong number of arguments was given to" $ do
            unreadableNaming "abs" (olca [] "abs(1; 2)")
            unreadableNaming "foo" (olca [] "foo(1)")
        it "names every missing variable, once each" $
            olca [("b", 1)] "a + b * c + a" `shouldBe` Left (Unresolved ("a" :| ["c"]))
        it "matches variable names whatever their case, against a lowercase environment" $
            near 6 (olca [("leak", 2)] "LEAK * 3")
        it "collects identifiers without the functions, lowercased" $
            collectIdentifiers OpenLca "if(Leak == 1; a; MAX(b; c))" `shouldBe` ["leak", "a", "b", "c"]
        it "does not take div, mod or xor for variables, and keeps $ names and constants" $ do
            collectIdentifiers OpenLca "a div b + c mod d" `shouldBe` ["a", "b", "c", "d"]
            collectIdentifiers OpenLca "$a * 2" `shouldBe` ["$a"]
            collectIdentifiers OpenLca "e * x" `shouldBe` ["e", "x"]
        it "refuses a formula that gives no finite number" $ do
            unreadableNaming "finite" (olca [] "1 div 0")
            unreadableNaming "finite" (olca [] "1/0")
    describe "the older dialects keep a non-finite result" $
        it "gives Infinity for 1/0" $ do
            Expr.evaluate Arithmetic M.empty "1/0" `shouldBe` Right (1 / 0)
            Expr.evaluate SimaPro M.empty "1/0" `shouldBe` Right (1 / 0)

    describe "the older dialects are unchanged" $ do
        it "keeps a sign below ^, ^ chaining from the right, and log natural" $ do
            Expr.evaluate Arithmetic M.empty "-2^2" `shouldBe` Right (-4)
            Expr.evaluate Arithmetic M.empty "2^3^2" `shouldBe` Right 512
            near (log 1000) (Expr.evaluate Arithmetic M.empty "log(1000)")
            -- Windows' pow rounds 10^-3 differently in the last digit.
            near 5.0e-2 (Expr.evaluate SimaPro M.empty "1*10^-3*50 // a comment")

    describe "settle" $ do
        it "evaluates calculated parameters in whatever order they refer to each other" $
            settle OpenLca (M.fromList [("a", 2)]) [("c", "b + 1"), ("b", "a * 3")]
                `shouldBe` M.fromList [("a", 2), ("b", 6), ("c", 7)]
        it "leaves out a parameter whose formula never evaluates" $
            M.member "d" (settle OpenLca M.empty [("d", "nowhere + 1")]) `shouldBe` False

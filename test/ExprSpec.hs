{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The formula languages: what each computes, how VoLCA's writes any of
them, and how calculated parameters get their values.
-}
module ExprSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (encode)
import Data.Foldable (toList)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, arbitraryBoundedEnum, choose, elements, forAll, oneof, sized, vectorOf, (===))

import Expr (Dialect (..), Formula (..), Refusal (..), Unsettled (..), collectIdentifiers, describeRefusal, settle, translate)
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

isUnreadable :: Either Refusal Double -> Bool
isUnreadable = \case
    Left (Unreadable _) -> True
    _ -> False

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
    describe "the EcoSpold 2 dialect" $ do
        let es2 env = Expr.evaluate (EcoSpold2 M.empty) (M.fromList env)
        it "reads a function name whatever its case" $ do
            near 6 (es2 [("x", -3)] "ABS(x) * 2")
            near 6 (es2 [("x", -3)] "abs(x) * 2")
            near 2 (es2 [] "Average(1; 3)")
        it "reads LOG as decimal unless given a base" $ do
            near 2 (es2 [] "LOG(100)")
            near 3 (es2 [] "log(8; 2)")
            near 3 (es2 [] "LOG10(1000)")
            near 1 (es2 [] "LN(EXP(1))")
        it "gives MIN, MAX, SUM and AVERAGE any number of arguments" $ do
            near 5 (es2 [] "MAX(1; 5; 3)")
            near 1 (es2 [] "MIN(4; 1; 8)")
            near 6 (es2 [] "SUM(1; 2; 3)")
            near 2 (es2 [] "AVERAGE(1; 2; 3)")
        it "reads POWER" $
            near 8 (es2 [] "POWER(2; 3)")
        it "reads IF over a comparison, and over a bare number with the other value 0 by default" $ do
            near 0.2 (es2 [("EP", 150)] "IF(EP>130;0.2;0.3)")
            near 0.3 (es2 [("EP", 100)] "IF(EP>130;0.2;0.3)")
            near 0.66 (es2 [("RP", 1)] "IF(RP;0.66)")
            near 0 (es2 [("RP", 0)] "IF(RP;0.66)")
            near 4 (es2 [] "IF(1<=1;1;0) + IF(1<>2;1;0) + IF(2>=1;1;0) + IF(1=1;1;0) + IF(2<1;1;0)")
        it "evaluates only the branch of IF it takes" $
            near 2 (es2 [] "IF(1; 2; missing)")
        it "reads a postfix % as a hundredth" $ do
            near 0.05057976 (es2 [] "0.06072*83.3%")
            near 0.7 (es2 [("nylon", 1)] "nylon*70%")
        it "reads a comma between two digits as a decimal mark, any other as a separator" $ do
            near 0.02 (es2 [] "0,02")
            near 0.675 (es2 [] "average(160/200, 1)*0,75")
            near 4 (es2 [] "POWER((4/2),2)")
        it "refuses a function the format does not define, by its name" $
            es2 [] "ROUND(1.5)" `shouldBe` Left (Unreadable "unknown function ROUND")
        it "says how many arguments a function takes" $
            unreadableNaming "two or three arguments" (es2 [] "IF(1)")
        it "collects a name as a variable unless a parenthesis calls it" $ do
            collectIdentifiers (EcoSpold2 M.empty) "IF(RP_1; 0.8) + Average(a; b)" `shouldBe` ["RP_1", "a", "b"]
            collectIdentifiers (EcoSpold2 M.empty) "power * 2" `shouldBe` ["power"]

    describe "SimaPro's own formula language" $ do
        -- a = 3 and b = 7, commas made ';' as the loader does.
        let simapro formula = Expr.evaluate SimaPro (M.fromList [("a", 3), ("b", 7)]) (Expr.normalizeExpr '.' formula)
        it "computes its functions, comparisons and and" $ do
            simapro "iff(a>1,1,2 )" `shouldBe` Right 1
            simapro "log(100)" `shouldBe` Right 2
            simapro "max(1,2,3)" `shouldBe` Right 3
            simapro "round(2.5)" `shouldBe` Right 2
            simapro "iff(a=1 and b<>2,1,0)" `shouldBe` Right 0
        it "binds and below the comparisons and above or" $ do
            simapro "iff(a=3 and b=7,1,0)" `shouldBe` Right 1
            simapro "iff(1 or 0 and 0,1,0)" `shouldBe` Right 1
        it "reads function and operator names whatever their case" $ do
            simapro "IFF(a>1 AND b>1, Max(1,4), 0)" `shouldBe` Right 4
            near 3 (simapro "LOG(1000)")
        it "rounds half to even and has not as a function" $ do
            simapro "round(3.5)" `shouldBe` Right 4
            simapro "not(0) + not(5)" `shouldBe` Right 1
        it "has no if" $
            unreadableNaming "if" (simapro "if(a>1,1,2)")
        it "does not read words alone as a formula, a waste type staying a waste type" $
            Expr.isExpression SimaPro "not defined" `shouldBe` False
        it "collects a parameter named like a function, and not and or" $
            collectIdentifiers SimaPro "iff(a=1 AND b<>2;1;0) + int" `shouldBe` ["a", "b", "int"]

    describe "Python, in which a Brightway workbook writes its formulas" $ do
        let python env = Expr.evaluate Python (M.fromList env)
        it "binds ** tighter than a sign before it, chaining from the right, and takes a signed exponent" $ do
            python [] "2 ** 3 ** 2" `shouldBe` Right 512
            python [] "-2 ** 2" `shouldBe` Right (-4)
            python [] "2 ** -1" `shouldBe` Right 0.5
            python [] "7 / 2 * 2" `shouldBe` Right 7
        it "chains comparisons, each meaning the next as with and" $ do
            python [("x", 2)] "1 < x < 3" `shouldBe` Right 1
            python [("x", 5)] "1 < x < 3" `shouldBe` Right 0
            python [("x", 2)] "(x == 2) + (x != 2)" `shouldBe` Right 1
        it "reads a conditional expression, computing only the branch it takes" $ do
            python [("mass", 2)] "mass * 2 if mass > 1 else 0" `shouldBe` Right 4
            python [("mass", 0.5)] "mass * 2 if mass > 1 else 0" `shouldBe` Right 0
            python [("a", 0)] "1 if a else 2 if missing else 3" `shouldBe` Left (Unresolved ("missing" :| []))
            python [("a", 1)] "1 if a else 2 if missing else 3" `shouldBe` Right 1
        it "gives an operand from and and or, as Python does, and stops at the first that settles it" $ do
            python [] "0 or 5" `shouldBe` Right 5
            python [] "3 and 4" `shouldBe` Right 4
            python [] "0 and missing" `shouldBe` Right 0
            python [] "not 0" `shouldBe` Right 1
            python [] "True + True + False" `shouldBe` Right 2
        it "calls functions with commas, by their Python names or sympy's" $ do
            python [("mass", 2)] "Max(mass * 2 - 1, 0) ** 2" `shouldBe` Right 9
            near 3 (python [] "log(8, 2)")
            near 1 (python [] "log(e)")
            near 1 (python [] "sin(pi / 2)")
            python [] "round(2.5) + ceiling(1.2) + ceil(1.2)" `shouldBe` Right 6
        it "refuses what Python means otherwise: ^ is not a power, // and % are not division" $ do
            python [] "2 ^ 3" `shouldSatisfy` isUnreadable
            python [] "7 // 2" `shouldSatisfy` isUnreadable
            python [] "7 % 2" `shouldSatisfy` isUnreadable
        it "refuses a formula that gives no finite number, as Python raises" $
            unreadableNaming "finite" (python [] "1 / 0")
        it "is written in VoLCA's language" $ do
            translate Python "mass * (1 + loss)" `shouldBe` Right "mass * (1 + loss)"
            translate Python "x ** 2" `shouldBe` Right "x ^ 2"
            translate Python "a if c > 1 else b" `shouldBe` Right "if(c > 1; a; b)"
            translate Python "Max(a, 0) + log(x)" `shouldBe` Right "max(a; 0) + ln(x)"

    describe "the older dialects keep a non-finite result" $
        it "gives Infinity for 1/0" $ do
            Expr.evaluate Volca M.empty "1/0" `shouldBe` Right (1 / 0)
            Expr.evaluate SimaPro M.empty "1/0" `shouldBe` Right (1 / 0)

    describe "VoLCA's language" $ do
        let volca env = Expr.evaluate Volca (M.fromList env)
        it "computes a scoring set's formulas as it always has: a sign below ^, ^ chaining from the right, log natural" $ do
            volca [] "-2^2" `shouldBe` Right (-4)
            volca [] "2^3^2" `shouldBe` Right 512
            near (log 1000) (volca [] "log(1000)")
            near 2 (volca [("a", 2), ("b", 5)] "min(a, b)")
            -- Windows' pow rounds 10^-3 differently in the last digit.
            near 5.0e-2 (Expr.evaluate SimaPro M.empty "1*10^-3*50 // a comment")
        it "reads comparisons, if, and functions of any number of arguments, by names in any case" $ do
            near 1 (volca [("a", 3)] "a > 2")
            near 20 (volca [("a", 3)] "if(a <= 2; 10; 20)")
            near 3 (volca [] "max(1; 2; 3)")
            near (log 1000) (volca [] "LOG(1000)")
        it "has no word operator, so and is a name like any other" $
            near 6 (volca [("and", 2)] "and * 3")
        it "gives every function the value its reference's example states" $
            forM_ Expr.functions $ \f ->
                let r = Expr.reference f
                 in near (Expr.exampleValue r) (volca [] (Expr.example r))
        it "writes every example of its reference as the reference does" $
            forM_ Expr.functions $ \f ->
                let r = Expr.reference f
                 in translate Volca (Expr.example r) `shouldBe` Right (Expr.example r)
        it "calls each function by a name of its own" $
            nub Expr.functionNames `shouldBe` Expr.functionNames
        it "sends a formula as a tree: a number, a name in lower case, or a call spelled as it is written" $
            encode <$> Expr.readFormula Volca "-2 * Yield + if(a > 1; sqrt(b); 0)"
                `shouldBe` Right "[\"+\",[\"*\",[\"-\",2],\"yield\"],[\"if\",[\">\",\"a\",1],[\"sqrt\",\"b\"],0]]"

    describe "translate" $ do
        it "writes SimaPro's functions by VoLCA's names, without its comment" $ do
            translate SimaPro "log(x)" `shouldBe` Right "log10(x)"
            translate SimaPro "round(x)" `shouldBe` Right "roundeven(x)"
            translate SimaPro "iff(a=1 and b<>2;1;0)" `shouldBe` Right "if(and(a = 1; b <> 2); 1; 0)"
            translate SimaPro "1*10^-3*50 // a note" `shouldBe` Right "1 * 10 ^ -3 * 50"
        it "writes EcoSpold 2's log, IF and % in VoLCA's terms" $ do
            translate (EcoSpold2 M.empty) "LOG(100)" `shouldBe` Right "log10(100)"
            translate (EcoSpold2 M.empty) "log(8;2)" `shouldBe` Right "log(8; 2)"
            translate (EcoSpold2 M.empty) "IF(RP;0.66)" `shouldBe` Right "if(RP; 0.66; 0)"
            translate (EcoSpold2 M.empty) "0.06072*83.3%" `shouldBe` Right "0.06072 * (83.3 / 100)"
        it "writes EcoSpold 2's UnitConversion as its factor from the database's table" $ do
            let conversions = M.fromList [(("MJ", "kWh"), 0.25), (("m3", "l"), 1000)]
            translate (EcoSpold2 conversions) "UnitConversion(3.79, 'MJ', 'kWh')*(1-0.35)" `shouldBe` Right "3.79 * 0.25 * (1 - 0.35)"
            translate (EcoSpold2 conversions) "fuel*unitconversion(1;'m3';'l')" `shouldBe` Right "fuel * (1 * 1000)"
        it "refuses a UnitConversion the database's table has no factor for, naming both units" $
            translate (EcoSpold2 (M.fromList [(("MJ", "kWh"), 0.25)])) "UnitConversion(1, 'kWh', 'MJ')"
                `shouldSatisfy` either (T.isInfixOf "no conversion from kWh to MJ" . describeRefusal) (const False)
        it "says a database without a unit conversion table has none, rather than lacking one pair" $
            translate (EcoSpold2 M.empty) "UnitConversion(1, 'kWh', 'MJ')"
                `shouldSatisfy` either (T.isInfixOf "the database has no unit conversion table" . describeRefusal) (const False)
        it "keeps openLCA's grouping of ^ and of a sign with the parentheses VoLCA needs" $ do
            translate OpenLca "-2^2" `shouldBe` Right "(-2) ^ 2"
            translate OpenLca "2^3^2" `shouldBe` Right "(2 ^ 3) ^ 2"
            translate OpenLca "if(a > 1 xor b > 1; 7 div 2; round(c))" `shouldBe` Right "if(xor(a > 1; b > 1); div(7; 2); round(c))"
            translate OpenLca "max()" `shouldBe` Right "0"
        it "refuses a name VoLCA cannot write, by that name" $
            translate OpenLca "$a * 2" `shouldBe` Left (Unreadable "VoLCA's formulas cannot write the name $a")
        it "puts a part in parentheses only when its reading needs them" $ do
            translate Volca "a - (b - c)" `shouldBe` Right "a - (b - c)"
            translate Volca "(a - b) - c" `shouldBe` Right "a - b - c"
            translate Volca "a ^ (b ^ c)" `shouldBe` Right "a ^ b ^ c"
            translate Volca "-(a ^ 2) + (-a) ^ 2" `shouldBe` Right "-a ^ 2 + (-a) ^ 2"
            translate Volca "(a < b) * 2" `shouldBe` Right "(a < b) * 2"
        prop "writes any formula so that it reads back as the same one" $
            forAll (sized formula) $ \f ->
                Expr.readFormula Volca (Expr.render f) === Right f

    describe "times" $ do
        it "multiplies a formula by a number, with the parentheses its reading needs" $ do
            Expr.times 0.5 "2 * a" `shouldBe` "2 * a * 0.5"
            Expr.times 0.5 "a + b" `shouldBe` "(a + b) * 0.5"
            Expr.times (-2) "a" `shouldBe` "a * -2"
        it "leaves a formula whole when the factor is one" $
            Expr.times 1 "(a)" `shouldBe` "(a)"
        it "multiplies a formula that does not read all the same" $
            Expr.times 0.5 "a +" `shouldBe` "(a +) * 0.5"

    describe "settle" $ do
        it "evaluates calculated parameters in whatever order they refer to each other" $
            settle OpenLca (M.fromList [("a", 2)]) [("c", "b + 1"), ("b", "a * 3")]
                `shouldBe` (M.fromList [("a", 2), ("b", 6), ("c", 7)], [])
        it "lets a calculated parameter shadow a known value for every formula reading it" $
            fst (settle Volca (M.fromList [("a", 1)]) [("b", "a * 10"), ("a", "5")]) `shouldBe` M.fromList [("a", 5), ("b", 50)]
        it "replaces a known value spelled in another case, whichever spelling sorts first" $ do
            fst (settle SimaPro (M.fromList [("dmper", 1)]) [("DMPER", "5"), ("b", "DMper * 10")]) `shouldBe` M.fromList [("DMPER", 5), ("b", 50)]
            fst (settle SimaPro (M.fromList [("DMPER", 1)]) [("dmper", "5"), ("b", "DMper * 10")]) `shouldBe` M.fromList [("dmper", 5), ("b", 50)]
        it "gives no value to a name declared with two formulas, and one to a name declared twice alike" $ do
            settle SimaPro (M.fromList [("a", 1)]) [("X", "a * 2"), ("x", "nowhere")]
                `shouldBe` (M.fromList [("a", 1)], [("X", Ambiguous ("a * 2" :| ["nowhere"]))])
            fst (settle SimaPro (M.fromList [("a", 1)]) [("x", "a * 2"), ("x", "a * 2")]) `shouldBe` M.fromList [("a", 1), ("x", 2)]
        it "says why a parameter has no value: a name nobody gives, or a cycle" $ do
            settle OpenLca M.empty [("d", "nowhere + 1")] `shouldBe` (M.empty, [("d", NoValue (Unresolved ("nowhere" :| [])))])
            settle Volca M.empty [("x", "x + 1")] `shouldBe` (M.empty, [("x", Circular ("x" :| []))])
            let (values, refused) = settle Volca M.empty [("x", "y + 1"), ("y", "x * 2"), ("z", "1")]
            values `shouldBe` M.fromList [("z", 1)]
            sort (map fst refused) `shouldBe` ["x", "y"]
            [sort (toList cycle') | (_, Circular cycle') <- refused] `shouldBe` [["x", "y"], ["x", "y"]]

-- | A formula of any shape, its numbers not negative, as no grammar reads a negative literal.
formula :: Int -> Gen Formula
formula size
    | size <= 1 = leaf
    | otherwise =
        oneof
            [ leaf
            , Unary <$> arbitraryBoundedEnum <*> part
            , Binary <$> arbitraryBoundedEnum <*> part <*> part
            , Variadic <$> arbitraryBoundedEnum <*> ((:|) <$> part <*> (choose (0, 2) >>= (`vectorOf` part)))
            , If <$> part <*> part <*> part
            ]
  where
    leaf :: Gen Formula
    leaf = oneof [Literal . abs <$> arbitrary, Literal <$> elements [0, 2, 1.0e-3, 1.0e20], Name <$> elements ["a", "b_2", "Leak"]]

    part :: Gen Formula
    part = formula (size `div` 3)

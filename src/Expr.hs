{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Formulas: VoLCA's own language, and the ones the formats a database
arrives in write theirs in.

One grammar reads SimaPro, EcoSpold 2 and VoLCA, because they agree on
everything an expression is made of. Where they disagree is at the edges, and a
'Dialect' says which set of edges a formula was written against: every entry
point asks for one, so no formula is read in a language nobody chose for it.
openLCA's language departs from the others on precedence and on truth values,
and Python's on its operators, so each is read by a grammar of its own
('pOlca', 'pPython'), sharing the tokens.

Every dialect reads into the same tree, whose functions are VoLCA's: a
dialect's name for a function is where its meaning is settled (SimaPro's @log@
is 'Log10'), so one evaluator computes them all and 'translate' writes any of
them in VoLCA's language.

A formula is read first and given values second. The two steps fail for two
reasons a reader acts on differently, text that is not a formula and a formula
naming something without a value, and every such name is only known once the
whole formula has been read.
-}
module Expr (
    Dialect (..),
    UnitConversions,
    Refusal (..),
    Unsettled (..),
    Formula (..),
    Unary (..),
    Binary (..),
    Variadic (..),
    Function (..),
    Spelling (..),
    Reference (..),
    readFormula,
    evaluate,
    translate,
    render,
    writtenIn,
    respelt,
    times,
    dividedBy,
    agrees,
    settle,
    settledInOrder,
    describeRefusal,
    describeUnsettled,
    normalizeExpr,
    isExpression,
    collectIdentifiers,
    functions,
    reference,
    functionNames,
    operators,
    volcaRules,
) where

import Amount (readAmount)
import Control.Lens ((&), (?~))
import Control.Monad (when)
import Data.Aeson (ToJSON (..), Value (String))
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit)
import Data.Either (isRight, lefts)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.OpenApi (NamedSchema (..), ToSchema (..), description)
import Data.Semigroup (Max (..), Min (..), sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (showFFloat)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

{- | The language a formula was written in.

Five, because five are what the callers actually have.
-}
data Dialect
    = {- | SimaPro's own: @\/\/@ opens a comment and the expression ends
      there. Its text reaches the evaluator already through 'normalizeExpr',
      which is where the file's decimal separator is known. A parameter named
      @and@ or @or@ cannot be read.
      -}
      SimaPro
    | {- | VoLCA's own, in which a scoring set's formulas are written: the
      functions of 'functions', by the names 'reference' gives them and in any
      case, comparisons, a comma or @;@ between two arguments, and no comment
      and no word operator, so a name not followed by a parenthesis is always
      a variable. @log@ of one argument is natural, as it has always been in a
      scoring set.
      -}
      Volca
    | {- | An EcoSpold 2 @mathematicalRelation@, in the subset of OpenFormula the
      format specifies: function names in any case, @LOG@ decimal unless given
      a base, @IF@ with comparisons, functions of any number of arguments, and
      a postfix @%@, and @UnitConversion(amount; 'unit'; 'unit')@, which
      multiplies the amount by the factor the database's own table gives from
      the first unit to the second, by their names as written. Read like
      'Volca' otherwise.
      -}
      EcoSpold2 UnitConversions
    | {- | openLCA's formula language: @;@ between arguments, comparisons and
      logical operators, a lazy @if@, and three readings that change a number
      against the others: @log@ is decimal, @^@ chains from the left, and
      a sign binds tighter than @^@ (@-2^2@ is 4). It tells numbers from
      truth values and refuses to mix them, as openLCA does. Names match
      whatever their case.
      -}
      OpenLca
    | {- | A Brightway formula, which is Python: @**@ for a power, @==@ and @!=@
      among the comparisons, which chain (@1 < x < 3@), the word operators
      @and@, @or@ and @not@, and @a if condition else b@. @and@ and @or@ give
      one of their operands, as in Python, not a truth value. Functions are
      called with commas between their arguments, by their Python names
      (@log@ is natural, @round@ goes to the even number) or their names in a
      formula printed by sympy (@Max@, @ceiling@). @pi@ and @e@ are the
      constants: Brightway lets no parameter take their names. A formula that
      divides by zero or overflows gives no number, as Python raises then.
      -}
      Python
    deriving (Eq, Show)

{- | What a writer of VoLCA's language must know beyond its functions and
operators, as 'Volca' reads it: 'readable' parts the arguments, 'pNumberToken'
reads a number, 'pIdentTok' a name, 'evaluate' looks names up without case.
-}
volcaRules :: [Text]
volcaRules =
    [ "A number takes a point before its decimals and may take an exponent: 0.5, 1.2e-3."
    , "Arguments are parted by a semicolon or a comma: max(a; b) and max(a, b) are one formula."
    , "A name starts with a letter or an underscore and goes on with letters, digits and underscores. It is a parameter, or a named line of the process, whatever its case: Yield and yield are one name."
    , "A name followed by an opening parenthesis calls a function; a function the language does not have is refused by its name."
    , "A comparison gives 1 when it holds and 0 when it does not; a condition holds when it is not 0."
    , "There is no comment and no word operator: and, or and not are functions, and pi or e is a name like any other."
    ]

-- | A database's factors from one unit to another, keyed by the two names as written.
type UnitConversions = M.Map (Text, Text) Double

-- | Why a formula has no value.
data Refusal
    = {- | The text does not read as a formula: what the parser met and what it
      expected, on one line.
      -}
      Unreadable Text
    | {- | It reads, but these names have no value: each once, in the order the
      formula uses them.
      -}
      Unresolved (NonEmpty Text)
    deriving (Eq, Show)

-- | Why a calculated parameter has no value.
data Unsettled
    = -- | Its formula has none.
      NoValue Refusal
    | -- | It needs, through these parameters, its own value to have one.
      Circular (NonEmpty Text)
    | -- | It is declared with these formulas, and which one is meant cannot be told.
      Ambiguous (NonEmpty Text)
    deriving (Eq, Show)

-- | A refusal as a reader is told it.
describeRefusal :: Refusal -> Text
describeRefusal = \case
    Unreadable reason -> reason
    Unresolved (name :| []) -> "unknown variable " <> name
    Unresolved names -> "unknown variables " <> T.intercalate ", " (NE.toList names)

describeUnsettled :: Unsettled -> Text
describeUnsettled = \case
    NoValue refusal -> describeRefusal refusal
    Circular (name :| []) -> name <> " needs its own value"
    Circular names -> T.intercalate ", " (NE.toList names) <> " need each other's values"
    Ambiguous formulas -> "declared with several formulas: " <> T.intercalate "; " (NE.toList formulas)

-- | A formula as read, before any name in it is looked up.
data Formula
    = Literal Double
    | Name Text
    | Unary Unary Formula
    | Binary Binary Formula Formula
    | Variadic Variadic (NonEmpty Formula)
    | -- | A condition and two branches; only the branch taken is resolved.
      If Formula Formula Formula
    deriving (Eq, Show)

{- | A formula as a tree a program walks without reading text: a number is a
number, a string a name (lowered, as every dialect reads names), and an array a
function, its VoLCA name or operator first and its arguments after it. @-@ with
one argument changes a sign, with two it subtracts.
-}
instance ToJSON Formula where
    toJSON = \case
        Literal x -> toJSON x
        Name name -> String (T.toLower name)
        Unary f a -> called (OneArgument f) [a]
        Binary f a b -> called (TwoArguments f) [a, b]
        Variadic f args -> called (AnyArguments f) (NE.toList args)
        If c y n -> called Conditional [c, y, n]
      where
        called :: Function -> [Formula] -> Value
        called f args = toJSON (String (word (spelling (reference f))) : map toJSON args)

        word :: Spelling -> Text
        word = \case
            Called name -> name
            Infix op -> op
            Prefix op -> op

instance ToSchema Formula where
    declareNamedSchema _ =
        pure . NamedSchema (Just "Formula") $
            mempty & description ?~ "A number, a name in lower case, or an array: a function or an operator, then its arguments."

-- | VoLCA's functions of one number.
data Unary
    = Negate
    | Not
    | Abs
    | Sqrt
    | Sqr
    | Exp
    | Ln
    | Log10
    | Trunc
    | Frac
    | Round
    | RoundEven
    | Ceil
    | Floor
    | Sin
    | Cos
    | Tan
    | Cotan
    | Asin
    | Acos
    | Atan
    | Sinh
    | Cosh
    | Tanh
    deriving (Eq, Show, Enum, Bounded)

-- | VoLCA's functions of two numbers, its operators among them.
data Binary
    = Add
    | Subtract
    | Multiply
    | Divide
    | Power
    | Equal
    | NotEqual
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual
    | LogBase
    | IntegerPower
    | Xor
    | Quotient
    | Remainder
    deriving (Eq, Show, Enum, Bounded)

-- | VoLCA's functions of one number or more.
data Variadic = Minimum | Maximum | Sum | Average | And | Or
    deriving (Eq, Show, Enum, Bounded)

-- | Every function VoLCA's language has.
data Function
    = OneArgument Unary
    | TwoArguments Binary
    | AnyArguments Variadic
    | Conditional
    deriving (Eq, Show)

functions :: [Function]
functions = map OneArgument [minBound ..] <> map TwoArguments [minBound ..] <> map AnyArguments [minBound ..] <> [Conditional]

-- | How a function is written in VoLCA's language.
data Spelling = Called Text | Infix Text | Prefix Text
    deriving (Eq, Show)

-- | What the language reference says of a function.
data Reference = Reference
    { spelling :: Spelling
    , usage :: Text
    , meaning :: Text
    , example :: Text
    , exampleValue :: Double
    }

reference :: Function -> Reference
reference = \case
    OneArgument f -> case f of
        Negate -> Reference (Prefix "-") "-x" "x with its sign changed" "-(2 - 5)" 3
        Not -> called "not" "not(x)" "1 when x is 0, 0 otherwise" "not(2 > 3)" 1
        Abs -> called "abs" "abs(x)" "the absolute value of x" "abs(-3)" 3
        Sqrt -> called "sqrt" "sqrt(x)" "the square root of x" "sqrt(16)" 4
        Sqr -> called "sqr" "sqr(x)" "x times x" "sqr(3)" 9
        Exp -> called "exp" "exp(x)" "e raised to the power x" "exp(0)" 1
        Ln -> called "ln" "ln(x)" "the natural logarithm of x" "ln(1)" 0
        Log10 -> called "log10" "log10(x)" "the decimal logarithm of x" "log10(1000)" 3
        Trunc -> called "trunc" "trunc(x)" "x without its fractional part" "trunc(-2.7)" (-2)
        Frac -> called "frac" "frac(x)" "the fractional part of x, with the sign of x" "frac(-2.75)" (-0.75)
        Round -> called "round" "round(x)" "x rounded to the nearest whole number, a half upward: round(-2.5) is -2" "round(2.5)" 3
        RoundEven -> called "roundeven" "roundeven(x)" "x rounded to the nearest whole number, a half to the even one" "roundeven(2.5)" 2
        Ceil -> called "ceil" "ceil(x)" "the smallest whole number not below x" "ceil(1.2)" 2
        Floor -> called "floor" "floor(x)" "the largest whole number not above x" "floor(1.8)" 1
        Sin -> called "sin" "sin(x)" "the sine of x, in radians" "sin(0)" 0
        Cos -> called "cos" "cos(x)" "the cosine of x, in radians" "cos(0)" 1
        Tan -> called "tan" "tan(x)" "the tangent of x, in radians" "tan(0)" 0
        Cotan -> called "cotan" "cotan(x)" "the cotangent of x, in radians: 1 / tan(x)" "cotan(1)" (recip (tan 1))
        Asin -> called "asin" "asin(x)" "the angle whose sine is x, in radians" "asin(1)" (pi / 2)
        Acos -> called "acos" "acos(x)" "the angle whose cosine is x, in radians" "acos(1)" 0
        Atan -> called "atan" "atan(x)" "the angle whose tangent is x, in radians" "atan(0)" 0
        Sinh -> called "sinh" "sinh(x)" "the hyperbolic sine of x" "sinh(0)" 0
        Cosh -> called "cosh" "cosh(x)" "the hyperbolic cosine of x" "cosh(0)" 1
        Tanh -> called "tanh" "tanh(x)" "the hyperbolic tangent of x" "tanh(0)" 0
    TwoArguments f -> case f of
        Add -> Reference (Infix "+") "a + b" "a plus b" "2 + 3" 5
        Subtract -> Reference (Infix "-") "a - b" "a minus b" "5 - 3" 2
        Multiply -> Reference (Infix "*") "a * b" "a times b" "2 * 3" 6
        Divide -> Reference (Infix "/") "a / b" "a divided by b" "6 / 4" 1.5
        Power -> Reference (Infix "^") "a ^ b" "a raised to the power b: 2 ^ 3 ^ 2 is 2 ^ 9, -2 ^ 2 is -4" "2 ^ 3" 8
        Equal -> Reference (Infix "=") "a = b" "1 when a equals b, 0 otherwise" "2 = 2" 1
        NotEqual -> Reference (Infix "<>") "a <> b" "1 when a differs from b, 0 otherwise" "2 <> 2" 0
        Less -> Reference (Infix "<") "a < b" "1 when a is below b, 0 otherwise" "1 < 2" 1
        LessOrEqual -> Reference (Infix "<=") "a <= b" "1 when a is not above b, 0 otherwise" "2 <= 2" 1
        Greater -> Reference (Infix ">") "a > b" "1 when a is above b, 0 otherwise" "1 > 2" 0
        GreaterOrEqual -> Reference (Infix ">=") "a >= b" "1 when a is not below b, 0 otherwise" "1 >= 2" 0
        LogBase -> called "log" "log(x; base)" "the logarithm of x in the given base; log(x) alone is ln(x)" "log(8; 2)" 3
        IntegerPower -> called "ipower" "ipower(x; n)" "x raised to the power n, n without its fractional part" "ipower(2; 3.7)" 8
        Xor -> called "xor" "xor(a; b)" "1 when exactly one of a and b is not 0, 0 otherwise" "xor(1; 0)" 1
        Quotient -> called "div" "div(a; b)" "a and b rounded half upward, then divided, without the fractional part" "div(7.6; 2)" 4
        Remainder -> called "mod" "mod(a; b)" "the remainder of a divided by b, with the sign of a" "mod(-7; 3)" (-1)
    AnyArguments f -> case f of
        Minimum -> called "min" "min(a; b; ...)" "the smallest of its arguments" "min(4; 2; 8)" 2
        Maximum -> called "max" "max(a; b; ...)" "the largest of its arguments" "max(1; 7; 3)" 7
        Sum -> called "sum" "sum(a; b; ...)" "the sum of its arguments" "sum(1; 2; 3)" 6
        Average -> called "average" "average(a; b; ...)" "the mean of its arguments" "average(1; 2; 3)" 2
        And -> called "and" "and(a; b; ...)" "1 when none of its arguments is 0, 0 otherwise" "and(1; 2 > 1)" 1
        Or -> called "or" "or(a; b; ...)" "1 when any of its arguments is not 0, 0 otherwise" "or(0; 2 < 1)" 0
    Conditional -> called "if" "if(condition; then; else)" "then when the condition is not 0, else otherwise, 0 when left out; only the value taken is computed" "if(2 > 1; 10; 20)" 10
  where
    called :: Text -> Text -> Text -> Text -> Double -> Reference
    called = Reference . Called

-- | The names VoLCA's language calls its functions by.
functionNames :: [Text]
functionNames = [name | f <- functions, Called name <- [spelling (reference f)]]

unary :: Unary -> Double -> Double
unary = \case
    Negate -> negate
    Not -> \x -> if x == 0 then 1 else 0
    Abs -> abs
    Sqrt -> sqrt
    Sqr -> \x -> x * x
    Exp -> exp
    Ln -> log
    Log10 -> logBase 10
    Trunc -> truncated
    Frac -> \x -> x - truncated x
    Round -> fromInteger . halfUp
    RoundEven -> fromInteger . round
    Ceil -> fromInteger . ceiling
    Floor -> fromInteger . floor
    Sin -> sin
    Cos -> cos
    Tan -> tan
    Cotan -> recip . tan
    Asin -> asin
    Acos -> acos
    Atan -> atan
    Sinh -> sinh
    Cosh -> cosh
    Tanh -> tanh

binary :: Binary -> Double -> Double -> Double
binary = \case
    Add -> (+)
    Subtract -> (-)
    Multiply -> (*)
    Divide -> (/)
    Power -> (**)
    Equal -> truth (==)
    NotEqual -> truth (/=)
    Less -> truth (<)
    LessOrEqual -> truth (<=)
    Greater -> truth (>)
    GreaterOrEqual -> truth (>=)
    LogBase -> flip logBase
    IntegerPower -> \x y -> x ^^ (truncate y :: Integer)
    Xor -> truth ((/=) `on` (/= 0))
    Quotient -> roundedQuotient
    Remainder -> remainder

variadic :: Variadic -> NonEmpty Double -> Double
variadic = \case
    Minimum -> getMin . sconcat . fmap Min
    Maximum -> getMax . sconcat . fmap Max
    Sum -> sum
    Average -> \xs -> sum xs / fromIntegral (length xs)
    And -> \xs -> if 0 `notElem` xs then 1 else 0
    Or -> \xs -> if any (/= 0) xs then 1 else 0

-- | 1 when it holds, 0 otherwise: how a truth value is computed.
truth :: (Double -> Double -> Bool) -> Double -> Double -> Double
truth holds x y = if holds x y then 1 else 0

truncated :: Double -> Double
truncated = fromInteger . truncate

-- | @div@: the two operands rounded half up, then divided, truncated. Zero gives NaN.
roundedQuotient :: Double -> Double -> Double
roundedQuotient x y = case halfUp y of
    0 -> 0 / 0
    d -> fromInteger (halfUp x `quot` d)

-- | @mod@: the remainder with the dividend's sign.
remainder :: Double -> Double -> Double
remainder x y = x - y * fromInteger (truncate (x / y))

halfUp :: Double -> Integer
halfUp x = floor (x + 0.5)

{- | Bring a formula to the one form the grammar reads.

Cutting a comment off as text rather than skipping it in the lexer is what lets
one grammar serve every dialect, and it also keeps 'collectIdentifiers' and
'evaluate' looking at the very same characters. Per line, because a formula
that spans two of them ends its comment at the first.
-}
readable :: Dialect -> Text -> Text
readable SimaPro = T.strip . T.intercalate "\n" . map (fst . T.breakOn "//") . T.lines
readable Volca = T.strip . normalizeExpr '.'
readable (EcoSpold2 _) = T.strip . ecoSpold2Commas
readable OpenLca = T.toLower . T.strip
readable Python = T.strip

{- | Evaluate an expression of the given dialect, with variable substitution.

Variable lookup is case-insensitive in every dialect. SimaPro needs it, mixing
the casing of one name freely - a parameter defined as @Dmper@ and referenced
as @DMper@ - and the others are served rather than harmed by it: an
EcoSpold 2 environment arrives folded already, and a scoring set writes its own
variable names on both sides of the formula. Two names that differ only in case
are therefore one name, and an environment stating both keeps one of them.
-}
evaluate :: Dialect -> M.Map Text Double -> Text -> Either Refusal Double
evaluate dialect env input = do
    formula <- readFormula dialect input
    value <- first (Unresolved . NE.nubBy ((==) `on` T.toLower)) (resolve (M.union (M.mapKeys T.toLower env) (constants dialect)) formula)
    finite dialect value

-- | openLCA and Python give no number for a formula that divides by zero or overflows; the others keep what they computed.
finite :: Dialect -> Double -> Either Refusal Double
finite dialect value = case dialect of
    SimaPro -> Right value
    Volca -> Right value
    EcoSpold2 _ -> Right value
    OpenLca -> finiteOnly
    Python -> finiteOnly
  where
    finiteOnly :: Either Refusal Double
    finiteOnly
        | isNaN value || isInfinite value = Left (Unreadable ("the formula gives no finite number: " <> T.pack (show value)))
        | otherwise = Right value

-- | The names a dialect knows without a parameter: openLCA's @pi@ and @e@ (@true@ and @false@ are read as truth values).
constants :: Dialect -> M.Map Text Double
constants = \case
    SimaPro -> M.empty
    Volca -> M.empty
    EcoSpold2 _ -> M.empty
    OpenLca -> M.fromList [("pi", pi), ("e", exp 1)]
    Python -> M.empty

readFormula :: Dialect -> Text -> Either Refusal Formula
readFormula dialect = first (Unreadable . refusalReason) . parse (sc *> grammar dialect <* eof) "" . readable dialect

-- | The grammar a dialect is read with.
grammar :: Dialect -> Parser Formula
grammar = \case
    SimaPro -> pFormula simaProLanguage
    Volca -> pFormula volcaLanguage
    EcoSpold2 conversions -> pFormula (ecoSpold2Language conversions)
    OpenLca -> pOlca
    Python -> pPython

{- | A formula of a dialect written in VoLCA's language.

What is written is read back and must give the very same formula, which is
checked rather than trusted: a name VoLCA's language cannot write (openLCA
allows @$@ in one) is refused by name instead of changing what the formula
computes. shortcut: openLCA's bare @pi@ and @e@ stay names, which a VoLCA
formula resolves only through a parameter of that name; the openLCA importer
supplies them when it keeps formulas.
-}
translate :: Dialect -> Text -> Either Refusal Text
translate dialect input = do
    formula <- readFormula dialect input
    let written = render formula
    case [name | name <- namesIn formula, readFormula Volca name /= Right (Name name)] of
        name : _ -> Left (Unreadable ("VoLCA's formulas cannot write the name " <> name))
        []
            | readFormula Volca written == Right formula -> Right written
            | otherwise -> Left (Unreadable ("VoLCA's formulas read " <> written <> " as another formula"))

{- | A formula in VoLCA's language multiplied by a number, as the amount it
gives is when a line is scaled. One that does not read is kept whole in
parentheses, which multiplies it all the same.
-}
times :: Double -> Text -> Text
times 1 formula = formula
times factor formula = case readFormula Volca formula of
    Right tree -> render (Binary Multiply tree (Literal factor))
    Left _ -> "(" <> formula <> ") * " <> render (Literal factor)

{- | A formula in VoLCA's language divided by a number, as the amount it gives
is when a scaled line is brought back: the factor 'times' put on it is taken
off when it ends in that very factor, else the formula is divided by it.
-}
dividedBy :: Double -> Text -> Text
dividedBy 1 formula = formula
dividedBy factor formula = case readFormula Volca formula of
    Right (Binary Multiply inner (Literal k)) | k == factor -> render inner
    Right tree -> render (Binary Divide tree (Literal factor))
    Left _ -> "(" <> formula <> ") / " <> render (Literal factor)

{- | Whether a formula's value is the amount stated beside it: within 1e-9
relative, and with no floor, since a formula kept on a line of 1e-11
recomputes it and must not let 3e-10 pass. An infinite value agrees with
nothing, though its distance to any number is within that bound.
-}
agrees :: Double -> Double -> Bool
agrees a b = not (isInfinite a || isInfinite b) && (a == b || abs (a - b) <= 1e-9 * max (abs a) (abs b))

{- | VoLCA's operators by how tightly they bind, loosest first, as its grammar
reads them from 'pComparison' to 'pPower'. Operators of two arguments chain
from the left, but the power, which chains from the right and takes a sign in
its exponent.
-}
operators :: [[Function]]
operators =
    [ map TwoArguments [Equal, NotEqual, Less, LessOrEqual, Greater, GreaterOrEqual]
    , map TwoArguments [Add, Subtract]
    , map TwoArguments [Multiply, Divide]
    , [OneArgument Negate]
    , [TwoArguments Power]
    ]

{- | How a language writes VoLCA's functions: each one's spelling, in the
effect @f@ (a language with no name for a function gives none), and its
operators by how tightly they bind, loosest first, each chaining from the left
but the power, which chains as 'powerChains' says, the text between a call's
arguments, how it writes a condition, and the names it reserves, which a
parameter cannot have although its grammar may read them so.
-}
data Writing f = Writing
    { spelledAs :: Function -> f Spelling
    , bindings :: [[Function]]
    , powerChains :: Chaining
    , argumentSeparator :: Text
    , conditions :: ConditionForm
    , reserves :: Text -> Bool
    }

-- | The side a chain of one operator groups from: @2 ^ 3 ^ 2@ is @2 ^ 9@ from the right.
data Chaining = FromLeft | FromRight

-- | A condition as a call of three arguments, or as Python's @y if c else n@, binding as its row in 'bindings' says.
data ConditionForm = ConditionCalled | ValueIfElse

volcaWriting :: (Applicative f) => Writing f
volcaWriting = Writing{spelledAs = pure . spelling . reference, bindings = operators, powerChains = FromRight, argumentSeparator = "; ", conditions = ConditionCalled, reserves = const False}

-- | A formula in VoLCA's language, with the parentheses its reading needs and no other.
render :: Formula -> Text
render = runIdentity . writeWith volcaWriting

-- | A formula as a language writes it, with the parentheses its reading needs and no other, or none when the language has no name for one of its functions.
writeWith :: forall f. (Monad f) => Writing f -> Formula -> f Text
writeWith writing = go 0
  where
    go :: Int -> Formula -> f Text
    go context formula = (\text -> if level formula < context then "(" <> text <> ")" else text) <$> plain formula

    plain :: Formula -> f Text
    plain = \case
        Literal x
            | x < 0 -> pure ("-" <> number (negate x))
            | otherwise -> pure (number x)
        Name name -> pure name
        formula@(Unary f a) ->
            spelledAs writing (OneArgument f) >>= \case
                Prefix op -> (prefixed op <>) <$> go (level formula) a
                other -> call other [a]
        formula@(Binary f a b) ->
            spelledAs writing (TwoArguments f) >>= \case
                Infix op -> case (f, powerChains writing) of
                    (Power, FromRight) -> infixed op <$> go (level formula + 1) a <*> go sign b
                    _ -> infixed op <$> go (level formula) a <*> go (level formula + 1) b
                other -> call other [a, b]
        formula@(Variadic f (x :| xs)) ->
            spelledAs writing (AnyArguments f) >>= \case
                Infix op -> foldl (infixed op) <$> go (level formula) x <*> traverse (go (level formula + 1)) xs
                other -> call other (x : xs)
        formula@(If c y n) -> case conditions writing of
            -- Python reads the value and the test tighter than a condition, and the otherwise as one.
            ValueIfElse -> (\value test otherwise' -> value <> " if " <> test <> " else " <> otherwise') <$> go (level formula + 1) y <*> go (level formula + 1) c <*> go (level formula) n
            ConditionCalled -> spelledAs writing Conditional >>= \name -> call name [c, y, n]

    infixed :: Text -> Text -> Text -> Text
    infixed op a b = a <> " " <> op <> " " <> b

    -- A prefix in letters is a word, which its operand must not run into.
    prefixed :: Text -> Text
    prefixed op = if T.all isAlpha op then op <> " " else op

    call :: Spelling -> [Formula] -> f Text
    call name args = (\parts -> word name <> "(" <> T.intercalate (argumentSeparator writing) parts <> ")") <$> traverse (go 0) args

    word :: Spelling -> Text
    word = \case
        Called name -> name
        Infix op -> op
        Prefix op -> op

    -- How tightly a function binds: its row counted from 1, and tighter than all of them when it is called by name.
    binding :: Function -> Int
    binding f = maybe atom (+ 1) (findIndex (elem f) (bindings writing))

    -- How tightly a sign binds, and a negative number with it.
    sign :: Int
    sign = binding (OneArgument Negate)

    atom :: Int
    atom = length (bindings writing) + 1

    level :: Formula -> Int
    level = \case
        Binary f _ _ -> binding (TwoArguments f)
        Unary f _ -> binding (OneArgument f)
        Variadic f _ -> binding (AnyArguments f)
        If{} -> binding Conditional
        Literal x | x < 0 -> sign
        Literal _ -> atom
        Name _ -> atom

    -- Without an exponent where that stays short; 'readAmount' reads back the same Double either way.
    number :: Double -> Text
    number x
        | x == fromInteger whole && x < 1e15 = T.pack (show whole)
        | x >= 1e-4 && x < 1e15 = T.pack (showFFloat Nothing x "")
        | otherwise = T.pack (show x)
      where
        whole :: Integer
        whole = truncate x

{- | A formula of VoLCA's language as a dialect writes it, when the dialect
reads the text back to the very same formula, names aside of their case. None
when the formula does not read, when the dialect has no name for one of its
functions, or when the text reads otherwise: SimaPro reads @a and b and c@ as
@(a and b) and c@, and has no @round@ that goes half upward.
-}
writtenIn :: Dialect -> Text -> Maybe Text
writtenIn dialect text = do
    formula <- either (const Nothing) Just (readFormula Volca text)
    writing <- writingOf dialect
    out <- writeWith writing formula
    back <- either (const Nothing) Just (readFormula dialect out)
    let reserved = any (reserves writing) (collectIdentifiers Volca text)
    if folded back == folded formula && not reserved then Just out else Nothing
  where
    folded :: Formula -> Formula
    folded = renamed T.toLower

{- | A formula of VoLCA's language with each name it reads spelt as the map
gives it by the name's lowercase form, any other left as it is: the names as
their definitions spell them, for a format that reads a name in its case. A
formula that does not read is left as it is.
-}
respelt :: M.Map Text Text -> Text -> Text
respelt spellings text = either (const text) (render . renamed (\name -> M.findWithDefault name (T.toLower name) spellings)) (readFormula Volca text)

-- | A formula with each name it reads renamed.
renamed :: (Text -> Text) -> Formula -> Formula
renamed f = \case
    Name name -> Name (f name)
    Unary g a -> Unary g (renamed f a)
    Binary g a b -> Binary g (renamed f a) (renamed f b)
    Variadic g args -> Variadic g (fmap (renamed f) args)
    If c y n -> If (renamed f c) (renamed f y) (renamed f n)
    literal@(Literal _) -> literal

-- | How a dialect writes VoLCA's functions, for the dialects VoLCA writes formulas in.
writingOf :: Dialect -> Maybe (Writing Maybe)
writingOf = \case
    Volca -> Just volcaWriting
    SimaPro -> Just (languageWriting simaProLanguage)
    EcoSpold2 conversions -> Just (languageWriting (ecoSpold2Language conversions))
    OpenLca -> Just olcaWriting
    Python -> Just pythonWriting

{- | How Python, as Brightway evaluates it, writes VoLCA's functions: its
operators binding as 'pPython' reads them, a condition as @y if c else n@, and
for a function it calls by name, the first of its names in 'pythonCalls'. It
writes no @and@, @or@ or @xor@ (Python's own give an operand, and Brightway's
@&@ and @|@ refuse numbers), no integer division or remainder, which 'pPython'
does not read, and no @round@ going half upward: Python's goes half to even.
-}
pythonWriting :: Writing Maybe
pythonWriting =
    Writing
        { spelledAs = \f -> lookup f (symbols <> namedBy pythonCalls)
        , bindings =
            [ [Conditional]
            , [OneArgument Not]
            , map TwoArguments [Equal, NotEqual, Less, LessOrEqual, Greater, GreaterOrEqual]
            , map TwoArguments [Add, Subtract]
            , map TwoArguments [Multiply, Divide]
            , [OneArgument Negate]
            , [TwoArguments Power]
            ]
        , powerChains = FromRight
        , argumentSeparator = ", "
        , conditions = ValueIfElse
        , reserves = (`elem` pythonReserved)
        }
  where
    symbols :: [(Function, Spelling)]
    symbols =
        [ (TwoArguments f, Infix op)
        | (op, f) <- [("+", Add), ("-", Subtract), ("*", Multiply), ("/", Divide), ("**", Power), ("==", Equal), ("!=", NotEqual), ("<", Less), ("<=", LessOrEqual), (">", Greater), (">=", GreaterOrEqual)]
        ]
            <> [(OneArgument Negate, Prefix "-"), (OneArgument Not, Prefix "not")]

{- | The names a Brightway parameter cannot have, in their case: Python's
keywords, and the symbols its interpreter defines (asteval 1.0, through
bw2parameters 1.1), which refuses a parameter named like one of them.
-}
pythonReserved :: [Text]
pythonReserved =
    concatMap
        T.words
        [ "ArithmeticError AssertionError AttributeError BaseException BufferError"
        , "BytesWarning DeprecationWarning EOFError EnvironmentError Exception False"
        , "FloatingPointError IOError ImportError ImportWarning IndentationError"
        , "IndexError KeyError KeyboardInterrupt LookupError MemoryError NameError None"
        , "NotImplementedError OSError OverflowError ReferenceError RuntimeError"
        , "RuntimeWarning StopIteration SyntaxError SyntaxWarning SystemError True"
        , "TypeError UnboundLocalError UnicodeDecodeError UnicodeEncodeError"
        , "UnicodeError UnicodeTranslateError UnicodeWarning ValueError Warning"
        , "ZeroDivisionError abs acos acosh add all amax amin and angle any append"
        , "arange arccos arccosh arcsin arcsinh arctan arctan2 arctanh argmax argmin"
        , "argsort argwhere around array as asarray asin asinh assert async atan atan2"
        , "atanh atleast_1d atleast_2d atleast_3d average await bartlett bin"
        , "bitwise_and bitwise_not bitwise_or bitwise_xor blackman bool break broadcast"
        , "bytearray bytes ceil choose chr class clip column_stack common_type complex"
        , "complex128 compress concatenate conjugate continue convolve copysign"
        , "corrcoef correlate cos cosh cov cross cumprod cumsum datetime_data def"
        , "deg2rad degrees del delete diag diag_indices diag_indices_from diagflat"
        , "diagonal dict diff digitize dir divide divmod dot dsplit dstack dtype e"
        , "ediff1d elif else empty empty_like enumerate equal except exp exp2"
        , "expand_dims expm1 extract eye fabs factorial fill_diagonal filter finally"
        , "finfo fix flatiter flatnonzero fliplr flipud float float64 floor"
        , "floor_divide fmax fmin fmod for format frexp from frombuffer fromfile"
        , "fromfunction fromiter frompyfunc fromregex fromstring frozenset fsum"
        , "genfromtxt getbufsize geterr global gradient greater greater_equal hamming"
        , "hanning hash hex histogram histogram2d histogramdd hsplit hstack hypot i0 id"
        , "identity if iinfo imag import in indices inexact inf info inner insert int"
        , "int32 integer interp intersect1d invert is iscomplex iscomplexobj isfinite"
        , "isinf isinstance isnan isneginf isposinf isreal isrealobj isscalar iterable"
        , "kaiser kron lambda ldexp left_shift len less less_equal linspace list"
        , "little_endian ln loadtxt log log10 log1p log2 logaddexp logaddexp2"
        , "logical_and logical_not logical_or logical_xor logspace longdouble longlong"
        , "map mask_indices matrix max maximum may_share_memory mean median meshgrid"
        , "min minimum mintypecode mod modf multiply nan nan_to_num nanargmax nanargmin"
        , "nanmax nanmin nansum ndarray ndenumerate ndim ndindex negative nextafter"
        , "nonlocal nonzero not not_equal number oct ones ones_like open or ord outer"
        , "packbits pass percentile pi piecewise place poly poly1d polyadd polyder"
        , "polydiv polyint polymul polysub polyval pow power print prod ptp put putmask"
        , "rad2deg radians raise range ravel real real_if_close reciprocal record"
        , "remainder repeat repr reshape resize return reversed right_shift rint roll"
        , "rollaxis roots rot90 round searchsorted select set setbufsize setdiff1d"
        , "seterr setxor1d shape short sign signbit signedinteger sin sinc single sinh"
        , "size slice sort sort_complex sorted spacing split sqrt square squeeze std"
        , "str subtract sum swapaxes take tan tanh tensordot tile trace transpose tri"
        , "tril tril_indices tril_indices_from trim_zeros triu triu_indices"
        , "triu_indices_from true_divide trunc try tuple type ubyte uint uint32 union1d"
        , "unique unravel_index unsignedinteger unwrap ushort vander var vdot vectorize"
        , "vsplit vstack where while with yield zeros zeros_like zip"
        ]

{- | How openLCA writes VoLCA's functions: its operators, @xor@, @div@ and
@mod@ among them, binding as 'pOlcaExpression' reads them, and for a
function it calls by name, the first of its names in 'olcaFunctions'. Where a
truth value is wanted and a number given, the text does not read back, which
'writtenIn' sees.
-}
olcaWriting :: Writing Maybe
olcaWriting =
    Writing
        { spelledAs = \f -> lookup f (keywords <> named)
        , bindings =
            [ [TwoArguments Xor]
            , map TwoArguments [Equal, NotEqual, Less, LessOrEqual, Greater, GreaterOrEqual]
            , map TwoArguments [Add, Subtract]
            , map TwoArguments [Multiply, Divide, Quotient, Remainder]
            , [TwoArguments Power]
            , [OneArgument Negate]
            ]
        , powerChains = FromLeft
        , argumentSeparator = "; "
        , conditions = ConditionCalled
        , reserves = (`elem` map snd infixWords) . T.toLower
        }
  where
    infixWords :: [(Binary, Text)]
    infixWords = [(Xor, "xor"), (Quotient, "div"), (Remainder, "mod")]

    keywords :: [(Function, Spelling)]
    keywords =
        [(f, operator) | row <- operators, f <- row, let operator = spelling (reference f)]
            <> [(TwoArguments f, Infix name) | (f, name) <- infixWords]

    named :: [(Function, Spelling)]
    named = [(f, Called name) | (name, known) <- olcaFunctions, Just f <- [builds known]]

    builds :: OlcaFunction -> Maybe Function
    builds = \case
        OneNumber f -> Just (OneArgument f)
        TwoNumbers f -> Just (TwoArguments f)
        Numbers f -> Just (AnyArguments f)
        Negation -> Just (OneArgument Not)
        Connective f _ -> Just (AnyArguments f)
        Choosing -> Just Conditional
        Constant _ -> Nothing
        Refused _ -> Nothing

{- | How a language read by 'pFormula' writes VoLCA's functions: the operators
of the grammar they share, @and@ and @or@ as words where it reads them so, and
for a function it calls by name, the first of its names that builds that
function.
-}
languageWriting :: Language -> Writing Maybe
languageWriting language =
    Writing
        { spelledAs = spelled
        , bindings = [[AnyArguments f] | connectives language, f <- [Or, And]] <> operators
        , powerChains = FromRight
        , argumentSeparator = "; "
        , conditions = ConditionCalled
        , reserves = (`elem` [name | (_, Infix name) <- connectiveWords]) . T.toLower
        }
  where
    spelled :: Function -> Maybe Spelling
    spelled f = case spelling (reference f) of
        Called _ -> lookup f (connectiveWords <> namedBy (calls language))
        operator -> Just operator

    connectiveWords :: [(Function, Spelling)]
    connectiveWords = [(AnyArguments f, Infix name) | connectives language, (f, name) <- [(And, "and"), (Or, "or")]]

-- | Each function a table of calls builds, called by the first of its names that builds it.
namedBy :: [(Text, Builder)] -> [(Function, Spelling)]
namedBy table = [(f, Called name) | (name, build) <- table, Right formula <- map build probes, Just f <- [builtBy formula]]
  where
    probes :: [[Formula]]
    probes = [map Name (take n ["a", "b", "c"]) | n <- [1 .. 3]]

    builtBy :: Formula -> Maybe Function
    builtBy = \case
        Unary f _ -> Just (OneArgument f)
        Binary f _ _ -> Just (TwoArguments f)
        Variadic f _ -> Just (AnyArguments f)
        If{} -> Just Conditional
        Literal _ -> Nothing
        Name _ -> Nothing

{- | The value of a formula, or every name in it without one. All of them rather
than the first: a reader who fixes one would otherwise learn of the next only
on the following load.
-}
resolve :: M.Map Text Double -> Formula -> Either (NonEmpty Text) Double
resolve env = \case
    Literal x -> Right x
    Name name -> maybe (Left (name :| [])) Right (M.lookup (T.toLower name) env)
    Unary f a -> unary f <$> resolve env a
    Binary f a b -> case (resolve env a, resolve env b) of
        (Right x, Right y) -> Right (binary f x y)
        (Left missing, Right _) -> Left missing
        (Right _, Left missing) -> Left missing
        (Left missing, Left more) -> Left (missing <> more)
    Variadic f args -> variadic f <$> resolveEach env args
    If condition yes no -> do
        c <- resolve env condition
        resolve env (if c /= 0 then yes else no)

-- | Every argument's value, or every name missing from any of them.
resolveEach :: M.Map Text Double -> NonEmpty Formula -> Either (NonEmpty Text) (NonEmpty Double)
resolveEach env args = maybe (sequence resolved) (Left . sconcat) (NE.nonEmpty (lefts (NE.toList resolved)))
  where
    resolved :: NonEmpty (Either (NonEmpty Text) Double)
    resolved = fmap (resolve env) args

-- | Every name a formula reads, in the order it reads them.
namesIn :: Formula -> [Text]
namesIn = \case
    Literal _ -> []
    Name name -> [name]
    Unary _ a -> namesIn a
    Binary _ a b -> namesIn a <> namesIn b
    Variadic _ args -> foldMap namesIn args
    If c y n -> namesIn c <> namesIn y <> namesIn n

{- | Why the parser refused a formula, on one line: what it met and what it
expected. The position and the caret 'errorBundlePretty' draws are left out,
since they point into the normalised text, which is not the one the file
carries.
-}
refusalReason :: ParseErrorBundle Text Void -> Text
refusalReason bundle = case bundleErrors bundle of
    err :| _ -> T.intercalate "; " (T.lines (T.pack (parseErrorTextPretty err)))

{- | EcoSpold 2 writes a comma both ways: as a decimal mark (@0,02*cement@) and
between arguments (@average(160/240, 160/270)@, @POWER((0.440/2),2)@), and
both decimal marks within one formula (@average(0.71;0,77)@), so no locale can
say which. A comma between two digits is read as the decimal mark, any other as
a separator: of the three readings, the one that agrees with the most stored
amounts in ecoinvent 3.12. shortcut: @max(1,2)@ with no space reads as 1.2,
which no ecoinvent relation writes; revisit if a database does.
-}
ecoSpold2Commas :: Text -> Text
ecoSpold2Commas formula = T.pack (zipWith3 mark (' ' : chars) chars (drop 1 chars <> " "))
  where
    chars :: String
    chars = T.unpack formula

    mark :: Char -> Char -> Char -> Char
    mark before c after
        | c /= ',' = c
        | isDigit before && isDigit after = '.'
        | otherwise = ';'

-- | Normalize expression text so decimal is always '.' and function arg separator is always ';'.
normalizeExpr :: Char -> Text -> Text
normalizeExpr '.' = T.map (\c -> if c == ',' then ';' else c)
normalizeExpr ',' = T.map (\c -> if c == ',' then '.' else c)
normalizeExpr _ = id

-- | Whitespace consumer. A comment, where a dialect has one, is gone before this runs.
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | What a dialect's name for a function builds from its arguments, or, given the wrong number of them, how many it takes.
type Builder = [Formula] -> Either String Formula

-- | A VoLCA function called with its arguments.
builder :: Function -> Builder
builder = \case
    OneArgument f -> \case
        [x] -> Right (Unary f x)
        _ -> Left "one argument"
    TwoArguments LogBase -> logOr Ln
    TwoArguments f -> \case
        [x, y] -> Right (Binary f x y)
        _ -> Left "two arguments"
    AnyArguments f -> maybe (Left "one argument or more") (Right . Variadic f) . NE.nonEmpty
    Conditional -> \case
        [condition, yes] -> Right (If condition yes (Literal 0))
        [condition, yes, no] -> Right (If condition yes no)
        _ -> Left "two or three arguments"

-- | @log@, the logarithm it means when its base is left out.
logOr :: Unary -> Builder
logOr alone = \case
    [x] -> Right (Unary alone x)
    [x, base] -> Right (Binary LogBase x base)
    _ -> Left "one or two arguments"

-- | What a language read by 'pFormula' adds to the arithmetic they all share.
data Language = Language
    { calls :: [(Text, Builder)]
    , connectives :: Bool
    , percent :: Bool
    , unitConversions :: Maybe UnitConversions
    }

volcaLanguage :: Language
volcaLanguage = Language{calls = [(name, builder f) | f <- functions, Called name <- [spelling (reference f)]], connectives = False, percent = False, unitConversions = Nothing}

-- | EcoSpold 2's, from its formula language specification, with the database's unit conversions.
ecoSpold2Language :: UnitConversions -> Language
ecoSpold2Language conversions =
    Language
        { calls =
            [ ("abs", one Abs)
            , ("sqrt", one Sqrt)
            , ("exp", one Exp)
            , ("ln", one Ln)
            , ("log", logOr Log10)
            , ("log10", one Log10)
            , ("power", two Power)
            , ("min", several Minimum)
            , ("max", several Maximum)
            , ("sum", several Sum)
            , ("average", several Average)
            , ("if", builder Conditional)
            ]
        , connectives = False
        , percent = True
        , unitConversions = Just conversions
        }

simaProLanguage :: Language
simaProLanguage =
    Language
        { calls =
            [ ("abs", one Abs)
            , ("sqr", one Sqr)
            , ("sqrt", one Sqrt)
            , ("exp", one Exp)
            , ("ln", one Ln)
            , ("log", one Log10)
            , ("trunc", one Trunc)
            , ("int", one Trunc)
            , ("frac", one Frac)
            , ("round", one RoundEven)
            , ("ceil", one Ceil)
            , ("floor", one Floor)
            , ("power", two Power)
            , ("ipower", two IntegerPower)
            , ("sin", one Sin)
            , ("cos", one Cos)
            , ("tan", one Tan)
            , ("cotan", one Cotan)
            , ("arcsin", one Asin)
            , ("arccos", one Acos)
            , ("arctan", one Atan)
            , ("sinh", one Sinh)
            , ("cosh", one Cosh)
            , ("tanh", one Tanh)
            , ("min", several Minimum)
            , ("max", several Maximum)
            , ("not", one Not)
            , ("iff", builder Conditional)
            ]
        , connectives = True
        , percent = False
        , unitConversions = Nothing
        }

one :: Unary -> Builder
one = builder . OneArgument

two :: Binary -> Builder
two = builder . TwoArguments

several :: Variadic -> Builder
several = builder . AnyArguments

-- | Python's @min@ and @max@, which iterate an argument given alone.
twoOrMore :: Variadic -> Builder
twoOrMore f = \case
    args@(_ : _ : _) -> several f args
    _ -> Left "two or more arguments"

-- | Longer operators first: a symbol that is a prefix of another would take its first character.
comparisonSymbols :: [(Text, Binary)]
comparisonSymbols = [("<=", LessOrEqual), ("<>", NotEqual), ("<", Less), (">=", GreaterOrEqual), (">", Greater), ("=", Equal)]

-- | Precedence-climbing formula parser.
pFormula :: Language -> Parser Formula
pFormula language
    | connectives language = joined (connective "or") Or (joined (connective "and") And (pComparison language))
    | otherwise = pComparison language

joined :: Parser Text -> Variadic -> Parser Formula -> Parser Formula
joined operator f operand = operand >>= go
  where
    go :: Formula -> Parser Formula
    go acc = (operator *> operand >>= go . \x -> Variadic f (acc :| [x])) <|> pure acc

-- | A word operator in any case, which a longer name merely starting with it (@order@) is not.
connective :: Text -> Parser Text
connective word = lexeme (try (string' word <* notFollowedBy (alphaNumChar <|> char '_')))

pComparison :: Language -> Parser Formula
pComparison language = pAddSub language >>= go
  where
    go :: Formula -> Parser Formula
    go acc = choice [symbol op *> pAddSub language >>= go . Binary f acc | (op, f) <- comparisonSymbols] <|> pure acc

pAddSub :: Language -> Parser Formula
pAddSub language = pMulDiv language >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "+" *> pMulDiv language >>= go . Binary Add acc)
            <|> (symbol "-" *> pMulDiv language >>= go . Binary Subtract acc)
            <|> pure acc

pMulDiv :: Language -> Parser Formula
pMulDiv language = pUnary language >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "*" *> pUnary language >>= go . Binary Multiply acc)
            <|> (symbol "/" *> pUnary language >>= go . Binary Divide acc)
            <|> pure acc

pUnary :: Language -> Parser Formula
pUnary language =
    (symbol "-" *> (Unary Negate <$> pUnary language))
        <|> (symbol "+" *> pUnary language)
        <|> pPower language

{- | Exponentiation, right-associative and binding tighter than @*@ and @/@.

The exponent goes through 'pUnary' rather than straight back to 'pPower', so it
may carry a sign. SimaPro writes scale factors that way – @1*10^-3*50@ – and
without it the @-@ met 'pPrimary', which knows numbers but not signs, and the
whole expression failed.
-}
pPower :: Language -> Parser Formula
pPower language = do
    base <- pPercent language
    (symbol "^" *> (Binary Power base <$> pUnary language)) <|> pure base

-- | OpenFormula's postfix @%@, a hundredth, binding tighter than @^@: ecoinvent writes @0.06072*83.3%@.
pPercent :: Language -> Parser Formula
pPercent language
    | percent language = foldl' (\x _ -> Binary Divide x (Literal 100)) <$> pPrimary language <*> many (symbol "%")
    | otherwise = pPrimary language

pPrimary :: Language -> Parser Formula
pPrimary language =
    choice
        [ between (symbol "(") (symbol ")") (pFormula language)
        , maybe empty (pUnitConversion language) (unitConversions language)
        , pCall language
        , Literal <$> pNumber
        , Name <$> pIdentTok
        ]

{- | A function applied to its arguments, its name matched whatever its case.

A name directly followed by an opening parenthesis is a call even when no
function goes by that name, and it is refused by that name: read as a variable
instead, the refusal would land on the parenthesis and never say which function
was missing. Only the name and its parenthesis are tried: past them the text is
committed to the call, so a refusal inside the arguments is the one reported.
-}
pCall :: Language -> Parser Formula
pCall language = pCallWith (calls language) (pFormula language) ";"

-- | A call to one of the functions given, its arguments read by the parser given and parted by the separator.
pCallWith :: [(Text, Builder)] -> Parser Formula -> Text -> Parser Formula
pCallWith known argument separator = do
    name <- try (pIdentTok <* symbol "(")
    build <- maybe (fail ("unknown function " <> T.unpack name)) pure (lookup (T.toLower name) known)
    args <- sepBy1 argument (symbol separator) <* symbol ")"
    either (\takes -> fail (T.unpack name <> " takes " <> takes <> ", given " <> show (length args))) pure (build args)

{- | EcoSpold 2's @UnitConversion@, read as the amount times its factor: the
units are names, which no other part of a formula is, so the call is settled
here and nothing of it is left to write in VoLCA's language.
-}
pUnitConversion :: Language -> UnitConversions -> Parser Formula
pUnitConversion language conversions = do
    _ <- try (lexeme (string' "UnitConversion") <* symbol "(")
    amount <- pFormula language
    from <- symbol ";" *> pUnitName
    to <- symbol ";" *> pUnitName <* symbol ")"
    let missing
            | M.null conversions = "the database has no unit conversion table"
            | otherwise = "no conversion from " <> T.unpack from <> " to " <> T.unpack to
    maybe (fail missing) (pure . Binary Multiply amount . Literal) (M.lookup (from, to) conversions)
  where
    pUnitName :: Parser Text
    pUnitName = lexeme (char '\'' *> takeWhileP Nothing (/= '\'') <* char '\'')

-- | A formula read in openLCA's language, and what it gives: a number or a truth value.
data Typed = Number Formula | Truth Formula

-- | The number a part of a formula gives, or why it gives none.
asNumber :: String -> Typed -> Either String Formula
asNumber what = \case
    Number f -> Right f
    Truth _ -> Left (what <> " needs a number, given a truth value")

asTruth :: String -> Typed -> Either String Formula
asTruth what = \case
    Truth f -> Right f
    Number _ -> Left (what <> " needs a truth value, given a number")

-- | Two operands made one, or why they cannot be.
type Combine = Typed -> Typed -> Either String Typed

arithmetic :: String -> Binary -> Combine
arithmetic op f x y = Number <$> (Binary f <$> asNumber op x <*> asNumber op y)

-- | A comparison, of two numbers or of two truth values, as openLCA allows both.
compared :: String -> Binary -> Combine
compared op f x y = case (x, y) of
    (Number a, Number b) -> Right (Truth (Binary f a b))
    (Truth a, Truth b) -> Right (Truth (Binary f a b))
    (Number _, Truth _) -> Left (op <> " compares a number with a truth value")
    (Truth _, Number _) -> Left (op <> " compares a truth value with a number")

logical :: String -> (Formula -> Formula -> Formula) -> Combine
logical op f x y = Truth <$> (f <$> asTruth op x <*> asTruth op y)

both :: Variadic -> Formula -> Formula -> Formula
both f x y = Variadic f (x :| [y])

{- | openLCA's precedence, lowest first: or, xor, and, comparison, sum,
product, power, sign. Every level chains from the left, @^@ included, and the
sign applies to one element, below @^@, so @-2^2@ is @(-2)^2@ and @--2@ is
refused. A whole formula must give a number.
-}
pOlca :: Parser Formula
pOlca = pOlcaExpression >>= either fail pure . asNumber "a formula"

pOlcaExpression :: Parser Typed
pOlcaExpression = chainLeft pOlcaXor [(symbol "||", logical "||" (both Or)), (symbol "|", logical "|" (both Or))]

pOlcaXor :: Parser Typed
pOlcaXor = chainLeft pOlcaAnd [(keyword "xor", logical "xor" (Binary Xor))]

pOlcaAnd :: Parser Typed
pOlcaAnd = chainLeft pOlcaComparison [(symbol "&&", logical "&&" (both And)), (symbol "&", logical "&" (both And))]

-- Longer operators first: a symbol that is a prefix of another would take its first character.
pOlcaComparison :: Parser Typed
pOlcaComparison =
    chainLeft
        pOlcaSum
        [ (symbol "<=", compared "<=" LessOrEqual)
        , (symbol "<>", compared "<>" NotEqual)
        , (symbol "<", compared "<" Less)
        , (symbol ">=", compared ">=" GreaterOrEqual)
        , (symbol ">", compared ">" Greater)
        , (symbol "==", compared "==" Equal)
        , (symbol "=", compared "=" Equal)
        , (symbol "!=", compared "!=" NotEqual)
        ]

pOlcaSum :: Parser Typed
pOlcaSum = chainLeft pOlcaProduct [(symbol "+", arithmetic "+" Add), (symbol "-", arithmetic "-" Subtract)]

pOlcaProduct :: Parser Typed
pOlcaProduct =
    chainLeft
        pOlcaPower
        [ (symbol "*", arithmetic "*" Multiply)
        , (symbol "/", arithmetic "/" Divide)
        , (keyword "div", arithmetic "div" Quotient)
        , (keyword "mod", arithmetic "mod" Remainder)
        ]

pOlcaPower :: Parser Typed
pOlcaPower = chainLeft pOlcaSigned [(symbol "^", arithmetic "^" Power)]

pOlcaSigned :: Parser Typed
pOlcaSigned =
    (symbol "-" *> (pOlcaElement >>= signed (Unary Negate)))
        <|> (symbol "+" *> (pOlcaElement >>= signed id))
        <|> pOlcaElement
  where
    signed :: (Formula -> Formula) -> Typed -> Parser Typed
    signed f = either fail (pure . Number . f) . asNumber "a sign"

pOlcaElement :: Parser Typed
pOlcaElement =
    choice
        [ between (symbol "(") (symbol ")") pOlcaExpression
        , pOlcaCall
        , Number . Literal <$> pNumber
        , named <$> pOlcaName
        ]
  where
    -- Bare true and false are truth values; pi and e resolve through 'constants', a parameter first.
    named :: Text -> Typed
    named = \case
        "true" -> Truth (Literal 1)
        "false" -> Truth (Literal 0)
        other -> Number (Name other)

-- | A name as openLCA writes one: letters, digits, @_@ and @$@, not starting with a digit.
pOlcaName :: Parser Text
pOlcaName = lexeme (T.pack <$> ((:) <$> (letterChar <|> oneOf ("_$" :: String)) <*> many (alphaNumChar <|> oneOf ("_$" :: String))))

-- | A word operator, which a longer name merely starting with it (@divisor@) is not.
keyword :: Text -> Parser Text
keyword word = lexeme (try (string word <* notFollowedBy (alphaNumChar <|> oneOf ("_$" :: String))))

-- | One operand, then any number of (operator, operand), grouped from the left.
chainLeft :: Parser Typed -> [(Parser Text, Combine)] -> Parser Typed
chainLeft operand symbols = operand >>= rest
  where
    rest :: Typed -> Parser Typed
    rest acc = (choice [op *> operand >>= either fail pure . combine acc | (op, combine) <- symbols] >>= rest) <|> pure acc

{- | A name followed by its arguments. Only the name and the parenthesis are
tried, as in 'pCall': past them a refusal is about this call.
-}
pOlcaCall :: Parser Typed
pOlcaCall = do
    name <- try (pOlcaName <* symbol "(")
    args <- sepBy pOlcaExpression (symbol ";") <* symbol ")"
    either fail pure (olcaCall name args)

-- | What an openLCA function takes and gives.
data OlcaFunction
    = Constant Typed
    | OneNumber Unary
    | TwoNumbers Binary
    | -- | min, max, sum, avg: any number of numbers, none giving 0, as in openLCA.
      Numbers Variadic
    | -- | not: one truth value, or none, which openLCA reads as false.
      Negation
    | -- | and, or: any number of truth values, and what none gives.
      Connective Variadic Double
    | -- | if: a truth value and two numbers, only one of them evaluated.
      Choosing
    | -- | Known to openLCA but not computed here, and why.
      Refused String

-- | openLCA's functions, by their lowercase name.
olcaFunctions :: [(Text, OlcaFunction)]
olcaFunctions =
    [ ("pi", Constant (Number (Literal pi)))
    , ("e", Constant (Number (Literal (exp 1))))
    , ("true", Constant (Truth (Literal 1)))
    , ("false", Constant (Truth (Literal 0)))
    , ("abs", OneNumber Abs)
    , ("sqrt", OneNumber Sqrt)
    , ("sqr", OneNumber Sqr)
    , ("exp", OneNumber Exp)
    , ("ln", OneNumber Ln)
    , ("log", OneNumber Log10)
    , ("lg", OneNumber Log10)
    , ("ceil", OneNumber Ceil)
    , ("floor", OneNumber Floor)
    , -- Half up, as openLCA rounds: round(-2.5) is -2.
      ("round", OneNumber Round)
    , ("int", OneNumber Trunc)
    , ("trunc", OneNumber Trunc)
    , ("frac", OneNumber Frac)
    , ("sin", OneNumber Sin)
    , ("cos", OneNumber Cos)
    , ("tan", OneNumber Tan)
    , ("cotan", OneNumber Cotan)
    , ("cot", OneNumber Cotan)
    , ("asin", OneNumber Asin)
    , ("arcsin", OneNumber Asin)
    , ("acos", OneNumber Acos)
    , ("arccos", OneNumber Acos)
    , ("atan", OneNumber Atan)
    , ("arctan", OneNumber Atan)
    , ("sinh", OneNumber Sinh)
    , ("cosh", OneNumber Cosh)
    , ("tanh", OneNumber Tanh)
    , ("pow", TwoNumbers Power)
    , ("power", TwoNumbers Power)
    , ("ipower", TwoNumbers IntegerPower)
    , ("min", Numbers Minimum)
    , ("max", Numbers Maximum)
    , ("sum", Numbers Sum)
    , ("avg", Numbers Average)
    , ("mean", Numbers Average)
    , ("not", Negation)
    , ("and", Connective And 1)
    , ("or", Connective Or 0)
    , ("if", Choosing)
    , ("iff", Choosing)
    , ("iif", Choosing)
    , ("random", Refused "a load must give the same numbers twice")
    , ("rand", Refused "a load must give the same numbers twice")
    ]

-- | A call, or why it is not one: an unknown name, a refused function, a wrong number or type of arguments.
olcaCall :: Text -> [Typed] -> Either String Typed
olcaCall name args = case lookup name olcaFunctions of
    Nothing -> Left ("unknown function " <> function)
    Just known -> case known of
        Constant value -> case args of
            [] -> Right value
            _ : _ -> arity "no argument"
        OneNumber f ->
            numbers >>= \case
                [x] -> Right (Number (Unary f x))
                _ -> arity "one argument"
        TwoNumbers f ->
            numbers >>= \case
                [x, y] -> Right (Number (Binary f x y))
                _ -> arity "two arguments"
        Numbers f -> Number . maybe (Literal 0) (Variadic f) . NE.nonEmpty <$> numbers
        Negation ->
            truths >>= \case
                [] -> Right (Truth (Literal 0))
                [x] -> Right (Truth (Unary Not x))
                _ -> arity "one argument"
        Connective f none -> Truth . maybe (Literal none) (Variadic f) . NE.nonEmpty <$> truths
        Choosing -> case args of
            [condition, yes, no] -> Number <$> (If <$> asTruth function condition <*> asNumber function yes <*> asNumber function no)
            _ -> arity "three arguments"
        Refused why -> Left (function <> " is refused: " <> why)
  where
    function :: String
    function = T.unpack name

    numbers, truths :: Either String [Formula]
    numbers = traverse (asNumber function) args
    truths = traverse (asTruth function) args

    arity :: String -> Either String a
    arity expected = Left (function <> " takes " <> expected <> ", given " <> show (length args))

{- | Python's precedence, lowest first: @if else@, @or@, @and@, @not@,
comparison, @|@, @&@, sum, product, sign, power. A power binds tighter than the
sign before it and takes a signed exponent, so @-2 ** 2@ is -4 and @2 ** -1@ is
0.5.
-}
pPython :: Parser Formula
pPython = do
    value <- pPyOr
    option value (If <$> (pyKeyword "if" *> pPyOr) <*> pure value <*> (pyKeyword "else" *> pPython))

-- | @a or b@ is @a@ when it is not 0, @b@ otherwise; @a and b@ is @b@ when @a@ is not 0, @a@ otherwise.
pPyOr, pPyAnd :: Parser Formula
pPyOr = pPyAnd >>= \x -> option x (If x x <$> (pyKeyword "or" *> pPyOr))
pPyAnd = pPyNot >>= \x -> option x ((\rest -> If x rest x) <$> (pyKeyword "and" *> pPyAnd))

pPyNot :: Parser Formula
pPyNot = (pyKeyword "not" *> (Unary Not <$> pPyNot)) <|> pPyComparison

-- | @a < b < c@ is @a < b and b < c@, each operand read once.
pPyComparison :: Parser Formula
pPyComparison = do
    first' <- pPyJoined
    rest <- many ((,) <$> pyComparison <*> pPyJoined)
    let each = zipWith3 Binary (map fst rest) (first' : map snd rest) (map snd rest)
    pure $ case each of
        [] -> first'
        [one'] -> one'
        x : xs -> Variadic And (x :| xs)
  where
    pyComparison :: Parser Binary
    pyComparison = choice [f <$ symbol op | (op, f) <- [("<=", LessOrEqual), ("<", Less), (">=", GreaterOrEqual), (">", Greater), ("==", Equal), ("!=", NotEqual)]]

{- | sympy's @|@ and @&@, which join truth values as its @Or@ and @And@ do.
They bind tighter than a comparison, as in Python, which is why sympy writes
each comparison it joins in parentheses.
-}
pPyJoined :: Parser Formula
pPyJoined = joined (symbol "|") Or (joined (symbol "&") And pPySum)

pPySum :: Parser Formula
pPySum = pPyProduct >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "+" *> pPyProduct >>= go . Binary Add acc)
            <|> (symbol "-" *> pPyProduct >>= go . Binary Subtract acc)
            <|> pure acc

-- | @*@ and @/@, which @**@ and Python's floor division @//@ are not.
pPyProduct :: Parser Formula
pPyProduct = pPySigned >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (lexeme (try (char '*' <* notFollowedBy (char '*'))) *> pPySigned >>= go . Binary Multiply acc)
            <|> (lexeme (try (char '/' <* notFollowedBy (char '/'))) *> pPySigned >>= go . Binary Divide acc)
            <|> pure acc

pPySigned :: Parser Formula
pPySigned =
    (symbol "-" *> (Unary Negate <$> pPySigned))
        <|> (symbol "+" *> pPySigned)
        <|> pPyPower

pPyPower :: Parser Formula
pPyPower = do
    base <- pPyPrimary
    (symbol "**" *> (Binary Power base <$> pPySigned)) <|> pure base

pPyPrimary :: Parser Formula
pPyPrimary =
    choice
        [ between (symbol "(") (symbol ")") pPython
        , pPiecewise
        , pCallWith pythonCalls pPython ","
        , Literal <$> pNumber
        , named =<< pIdentTok
        ]
  where
    named :: Text -> Parser Formula
    named = \case
        "True" -> pure (Literal 1)
        "False" -> pure (Literal 0)
        "pi" -> pure (Literal pi)
        "e" -> pure (Literal (exp 1))
        word
            | word `elem` pythonKeywords -> fail ("unexpected " <> T.unpack word)
            | otherwise -> pure (Name word)

{- | sympy's @Piecewise((v1, c1), (v2, c2), ...)@: the value of the first clause
whose condition holds. A last clause on @True@ is the otherwise; with none, no
clause holding gives no number, as sympy gives nan.
-}
pPiecewise :: Parser Formula
pPiecewise = do
    _ <- try (pyKeyword "Piecewise" <* symbol "(")
    clauses <- sepBy1 clause (symbol ",") <* symbol ")"
    pure (foldr choose (Binary Divide (Literal 0) (Literal 0)) clauses)
  where
    clause :: Parser (Formula, Formula)
    clause = between (symbol "(") (symbol ")") ((,) <$> pPython <* symbol "," <*> pPython)

    choose :: (Formula, Formula) -> Formula -> Formula
    choose (value, Literal 1) _ = value
    choose (value, condition) otherwise' = If condition value otherwise'

-- | A word operator, which a longer name merely starting with it (@order@) is not.
pyKeyword :: Text -> Parser Text
pyKeyword word = lexeme (try (string word <* notFollowedBy (alphaNumChar <|> char '_')))

pythonKeywords :: [Text]
pythonKeywords = ["if", "else", "and", "or", "not", "None", "lambda", "in", "is"]

-- | Python's functions and sympy's names for them, by their lowercase name.
pythonCalls :: [(Text, Builder)]
pythonCalls =
    [ ("abs", one Abs)
    , ("sqrt", one Sqrt)
    , ("exp", one Exp)
    , ("log", logOr Ln)
    , ("log10", one Log10)
    , ("pow", two Power)
    , ("min", twoOrMore Minimum)
    , ("max", twoOrMore Maximum)
    , ("ceil", one Ceil)
    , ("ceiling", one Ceil)
    , ("floor", one Floor)
    , ("trunc", one Trunc)
    , ("int", one Trunc)
    , ("round", one RoundEven)
    , ("sin", one Sin)
    , ("cos", one Cos)
    , ("tan", one Tan)
    , ("asin", one Asin)
    , ("acos", one Acos)
    , ("atan", one Atan)
    , ("sinh", one Sinh)
    , ("cosh", one Cosh)
    , ("tanh", one Tanh)
    ]

{- | A numeric literal, tokenized here and read by 'readAmount'.

The integer part is optional. SimaPro exports drop it: Agribalyse writes the
cereal fungicide mix as @0,45+0,247+,067@, whose last term normalizes to
@.067@. Megaparsec's 'L.float' requires a digit before the point, so one such
term used to fail the /whole/ expression, and the caller then fell back to
reading the leading number – 0.45 where the file says 0.764.

Handing the token to 'readAmount' also makes a literal inside an expression
round exactly as the same literal does on its own.
-}
pNumber :: Parser Double
pNumber = lexeme $ do
    literal <- pNumberToken
    maybe (fail ("not a number: " <> T.unpack literal)) pure (readAmount literal)

{- | Digits with an optional point and an optional exponent. The sign belongs to
'pUnary', so it is not part of the token.
-}
pNumberToken :: Parser Text
pNumberToken = try $ do
    whole <- takeWhileP (Just "digit") isDigit
    fractional <- option "" (T.cons <$> char '.' <*> takeWhileP (Just "digit") isDigit)
    -- Digits somewhere: a lone "." is not a number, and neither is the empty
    -- string, which would otherwise match every identifier and every operator.
    when (T.null whole && T.length fractional < 2) (fail "expected a number")
    exponent' <- option "" (try pExponent)
    pure (whole <> fractional <> exponent')
  where
    pExponent = do
        marker <- oneOf ("eE" :: String)
        sign <- option "" (T.singleton <$> oneOf ("+-" :: String))
        digits <- takeWhile1P (Just "digit") isDigit
        pure (T.cons marker (sign <> digits))

{- | Whether the text reads as a formula, whatever the names in it.
Used to detect allocation fields vs waste type descriptions in SimaPro CSV.
-}
isExpression :: Dialect -> Text -> Bool
isExpression dialect = isRight . readFormula dialect

{- | Every name a formula reads, in the order it reads them, the empty list for
text that does not read as a formula. A name a dialect gives a value of its own
(openLCA's @true@) is not one.
-}
collectIdentifiers :: Dialect -> Text -> [Text]
collectIdentifiers dialect = either (const []) namesIn . readFormula dialect

pIdentTok :: Parser Text
pIdentTok = lexeme (T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')))

{- | The known values with every calculated parameter given its own, in the
order they read each other, whatever order they are listed in; those that get
none come back with the reason. A calculated parameter shadows a known value of
the same name, case aside.
-}
settle :: Dialect -> M.Map Text Double -> [(Text, Text)] -> (M.Map Text Double, [(Text, Unsettled)])
settle dialect known calculated = let (values, refused, _) = settling dialect known calculated in (values, refused)

{- | The calculated parameters 'settle' gives a value, each with its formula, in
the order it computes them: each after those it reads. In VoLCA's language a
formula fails only on a name without a value, so the list is the same whatever
the known values are, and computing it top to bottom at other values gives
what 'settle' gives at those values.
-}
settledInOrder :: Dialect -> M.Map Text Double -> [(Text, Text)] -> [(Text, Text)]
settledInOrder dialect known calculated = let (_, _, order) = settling dialect known calculated in reverse order

-- | 'settle', with the parameters it gave a value, the last first.
settling :: Dialect -> M.Map Text Double -> [(Text, Text)] -> (M.Map Text Double, [(Text, Unsettled)], [(Text, Text)])
settling dialect known calculated = foldl' step (known, ambiguous, []) (stronglyConnComp graph)
  where
    declared :: M.Map Text (NonEmpty (Text, Text))
    declared = M.fromListWith (flip (<>)) [(T.toLower name, (name, formula) :| []) | (name, formula) <- calculated]

    byKey :: M.Map Text (Text, Text)
    byKey = M.mapMaybe sole declared

    sole :: NonEmpty (Text, Text) -> Maybe (Text, Text)
    sole parameters@(first' :| _)
        | all ((== snd first') . snd) parameters = Just first'
        | otherwise = Nothing

    ambiguous :: [(Text, Unsettled)]
    ambiguous = [(fst (NE.head parameters), Ambiguous (snd <$> parameters)) | (key, parameters) <- M.toList declared, M.notMember key byKey]

    -- Every spelling of a known name, so the calculated value replaces them instead of sitting beside them.
    knownSpellings :: M.Map Text [Text]
    knownSpellings = M.fromListWith (<>) [(T.toLower name, [name]) | name <- M.keys known]

    graph :: [((Text, Text), Text, [Text])]
    graph = [(parameter, key, filter (`M.member` byKey) (map T.toLower (collectIdentifiers dialect formula))) | (key, parameter@(_, formula)) <- M.toList byKey]

    step :: (M.Map Text Double, [(Text, Unsettled)], [(Text, Text)]) -> SCC (Text, Text) -> (M.Map Text Double, [(Text, Unsettled)], [(Text, Text)])
    step (values, refused, order) = \case
        AcyclicSCC (name, formula) -> case evaluate dialect values formula of
            Right v -> (M.insert name v (foldr M.delete values (M.findWithDefault [] (T.toLower name) knownSpellings)), refused, (name, formula) : order)
            Left why -> (values, refused <> [(name, NoValue why)], order)
        NECyclicSCC members -> (values, refused <> [(name, Circular (fst <$> members)) | (name, _) <- NE.toList members], order)

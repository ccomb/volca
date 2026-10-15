{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Formulas: VoLCA's own language, and the ones the formats a database
arrives in write theirs in.

One grammar reads SimaPro, EcoSpold 2 and VoLCA, because they agree on
everything an expression is made of. Where they disagree is at the edges, and a
'Dialect' says which set of edges a formula was written against: every entry
point asks for one, so no formula is read in a language nobody chose for it.
openLCA's language departs from the others on precedence and on truth values,
so it is read by a grammar of its own ('pOlca'), sharing the tokens.

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
    times,
    settle,
    describeRefusal,
    describeUnsettled,
    normalizeExpr,
    isExpression,
    collectIdentifiers,
    functions,
    reference,
    functionNames,
) where

import Amount (readAmount)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Either (isRight, lefts)
import Data.Function (on)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
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

Four, because four are what the callers actually have.
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
      a postfix @%@. Read like 'Volca' otherwise.
      -}
      EcoSpold2
    | {- | openLCA's formula language: @;@ between arguments, comparisons and
      logical operators, a lazy @if@, and three readings that change a number
      against the others: @log@ is decimal, @^@ chains from the left, and
      a sign binds tighter than @^@ (@-2^2@ is 4). It tells numbers from
      truth values and refuses to mix them, as openLCA does. Names match
      whatever their case.
      -}
      OpenLca
    deriving (Eq, Show)

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
readable EcoSpold2 = T.strip . ecoSpold2Commas
readable OpenLca = T.toLower . T.strip

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

-- | openLCA gives no number for a formula that divides by zero or overflows; the others keep what they computed.
finite :: Dialect -> Double -> Either Refusal Double
finite dialect value = case dialect of
    SimaPro -> Right value
    Volca -> Right value
    EcoSpold2 -> Right value
    OpenLca
        | isNaN value || isInfinite value -> Left (Unreadable ("the formula gives no finite number: " <> T.pack (show value)))
        | otherwise -> Right value

-- | The names a dialect knows without a parameter: openLCA's @pi@ and @e@ (@true@ and @false@ are read as truth values).
constants :: Dialect -> M.Map Text Double
constants = \case
    SimaPro -> M.empty
    Volca -> M.empty
    EcoSpold2 -> M.empty
    OpenLca -> M.fromList [("pi", pi), ("e", exp 1)]

readFormula :: Dialect -> Text -> Either Refusal Formula
readFormula dialect = first (Unreadable . refusalReason) . parse (sc *> grammar dialect <* eof) "" . readable dialect

-- | The grammar a dialect is read with.
grammar :: Dialect -> Parser Formula
grammar = \case
    SimaPro -> pFormula simaProLanguage
    Volca -> pFormula volcaLanguage
    EcoSpold2 -> pFormula ecoSpold2Language
    OpenLca -> pOlca

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

{- | A formula in VoLCA's language, with the parentheses its reading needs and
no other. The levels are the grammar's rules, from 'pComparison' to an atom.
-}
render :: Formula -> Text
render = go 0
  where
    go :: Int -> Formula -> Text
    go context formula
        | level formula < context = "(" <> written formula <> ")"
        | otherwise = written formula

    written :: Formula -> Text
    written = \case
        Literal x
            | x < 0 -> "-" <> number (negate x)
            | otherwise -> number x
        Name name -> name
        Unary f a -> case spelling (reference (OneArgument f)) of
            Prefix op -> op <> go 4 a
            other -> call other [a]
        Binary Power a b -> go 6 a <> " ^ " <> go 4 b
        formula@(Binary f a b) -> case spelling (reference (TwoArguments f)) of
            Infix op -> go (level formula) a <> " " <> op <> " " <> go (level formula + 1) b
            other -> call other [a, b]
        Variadic f xs -> call (spelling (reference (AnyArguments f))) (NE.toList xs)
        If c y n -> call (spelling (reference Conditional)) [c, y, n]

    call :: Spelling -> [Formula] -> Text
    call name args = word name <> "(" <> T.intercalate "; " (map (go 0) args) <> ")"

    word :: Spelling -> Text
    word = \case
        Called name -> name
        Infix op -> op
        Prefix op -> op

    level :: Formula -> Int
    level = \case
        Binary f _ _ -> binaryLevel f
        Unary Negate _ -> 4
        Literal x | x < 0 -> 4
        _ -> 6

    binaryLevel :: Binary -> Int
    binaryLevel = \case
        Equal -> 1
        NotEqual -> 1
        Less -> 1
        LessOrEqual -> 1
        Greater -> 1
        GreaterOrEqual -> 1
        Add -> 2
        Subtract -> 2
        Multiply -> 3
        Divide -> 3
        Power -> 5
        LogBase -> 6
        IntegerPower -> 6
        Xor -> 6
        Quotient -> 6
        Remainder -> 6

    -- Without an exponent where that stays short; 'readAmount' reads back the same Double either way.
    number :: Double -> Text
    number x
        | x == fromInteger whole && x < 1e15 = T.pack (show whole)
        | x >= 1e-4 && x < 1e15 = T.pack (showFFloat Nothing x "")
        | otherwise = T.pack (show x)
      where
        whole :: Integer
        whole = truncate x

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
    }

volcaLanguage :: Language
volcaLanguage = Language{calls = [(name, builder f) | f <- functions, Called name <- [spelling (reference f)]], connectives = False, percent = False}

-- | EcoSpold 2's, from its formula language specification.
ecoSpold2Language :: Language
ecoSpold2Language =
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
        }

one :: Unary -> Builder
one = builder . OneArgument

two :: Binary -> Builder
two = builder . TwoArguments

several :: Variadic -> Builder
several = builder . AnyArguments

-- | Longer operators first: a symbol that is a prefix of another would take its first character.
comparisonSymbols :: [(Text, Binary)]
comparisonSymbols = [("<=", LessOrEqual), ("<>", NotEqual), ("<", Less), (">=", GreaterOrEqual), (">", Greater), ("=", Equal)]

-- | Precedence-climbing formula parser.
pFormula :: Language -> Parser Formula
pFormula language
    | connectives language = joined "or" Or (joined "and" And (pComparison language))
    | otherwise = pComparison language

joined :: Text -> Variadic -> Parser Formula -> Parser Formula
joined word f operand = operand >>= go
  where
    go :: Formula -> Parser Formula
    go acc = (connective word *> operand >>= go . \x -> Variadic f (acc :| [x])) <|> pure acc

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
pCall language = do
    name <- try (pIdentTok <* symbol "(")
    build <- maybe (fail ("unknown function " <> T.unpack name)) pure (lookup (T.toLower name) (calls language))
    args <- sepBy1 (pFormula language) (symbol ";") <* symbol ")"
    either (\takes -> fail (T.unpack name <> " takes " <> takes <> ", given " <> show (length args))) pure (build args)

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
chainLeft operand operators = operand >>= rest
  where
    rest :: Typed -> Parser Typed
    rest acc = (choice [op *> operand >>= either fail pure . combine acc | (op, combine) <- operators] >>= rest) <|> pure acc

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
settle dialect known calculated = foldl' step (known, ambiguous) (stronglyConnComp graph)
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

    step :: (M.Map Text Double, [(Text, Unsettled)]) -> SCC (Text, Text) -> (M.Map Text Double, [(Text, Unsettled)])
    step (values, refused) = \case
        AcyclicSCC (name, formula) -> case evaluate dialect values formula of
            Right v -> (M.insert name v (foldr M.delete values (M.findWithDefault [] (T.toLower name) knownSpellings)), refused)
            Left why -> (values, refused <> [(name, NoValue why)])
        NECyclicSCC members -> (values, refused <> [(name, Circular (fst <$> members)) | (name, _) <- NE.toList members])

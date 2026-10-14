{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Expression evaluator: arithmetic (+, -, *, \/, ^), variables, parentheses
and a few functions, over formulas written in more than one language.

One grammar reads them all, because they agree on everything an expression is
made of. Where they disagree is at the edges, and a 'Dialect' says which set of
edges a formula was written against: every entry point asks for one, so no
formula is read in a language nobody chose for it. openLCA's language departs
from the others on precedence and on @log@, so it is read by a grammar of
its own ('pOlca'), sharing the tokens.

A formula is read into a 'Formula' first and given values second. The two steps
fail for two reasons a reader acts on differently, text that is not a formula
and a formula naming something without a value, and every such name is only
known once the whole formula has been read.
-}
module Expr (
    Dialect (..),
    Refusal (..),
    evaluate,
    settle,
    describeRefusal,
    normalizeExpr,
    isExpression,
    collectIdentifiers,
    functionNames,
) where

import Amount (readAmount)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Either (isRight, lefts)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isNothing)
import Data.Semigroup (Max (..), Min (..), sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
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
    | {- | Arithmetic with no comment and a comma between two arguments: the
      formulas someone writes in a scoring set. It has no line comment, so a
      @\/\/@ in one is a mistake and reads as one. Its functions are the
      seven it has always had, @log@ natural, so a published score keeps its
      value.
      -}
      Arithmetic
    | {- | An EcoSpold 2 @mathematicalRelation@, in the subset of OpenFormula the
      format specifies: function names in any case, @LOG@ decimal unless given
      a base, @IF@ with comparisons, functions of any number of arguments, and
      a postfix @%@. Read like 'Arithmetic' otherwise.
      -}
      EcoSpold2
    | {- | openLCA's formula language: @;@ between arguments, comparisons and
      logical operators, a lazy @if@, and three readings that change a number
      against the two above: @log@ is decimal, @^@ chains from the left, and
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

-- | A refusal as a reader is told it.
describeRefusal :: Refusal -> Text
describeRefusal = \case
    Unreadable reason -> reason
    Unresolved (name :| []) -> "unknown variable " <> name
    Unresolved names -> "unknown variables " <> T.intercalate ", " (NE.toList names)

{- | A formula as read, before any name in it is looked up. An operator is kept
as the function it computes, since 'resolve' is the only reader of the tree.
-}
data Formula
    = Literal Double
    | Name Text
    | Apply1 (Double -> Double) Formula
    | Apply2 (Double -> Double -> Double) Formula Formula
    | -- | A function of any number of arguments, none included.
      ApplyMany ([Double] -> Double) [Formula]
    | -- | A condition and two branches; only the branch taken is resolved.
      Choose Formula Formula Formula

{- | Bring a formula to the one form the grammar reads.

Cutting a comment off as text rather than skipping it in the lexer is what lets
one grammar serve both dialects, and it also keeps 'collectIdentifiers' and
'evaluate' looking at the very same characters. Per line, because a formula
that spans two of them ends its comment at the first.
-}
readable :: Dialect -> Text -> Text
readable SimaPro = T.strip . T.intercalate "\n" . map (fst . T.breakOn "//") . T.lines
readable Arithmetic = T.strip . normalizeExpr '.'
readable EcoSpold2 = T.strip . ecoSpold2Commas
readable OpenLca = T.toLower . T.strip

{- | Evaluate an expression of the given dialect, with variable substitution.

Variable lookup is case-insensitive in every dialect. SimaPro needs it, mixing
the casing of one name freely - a parameter defined as @Dmper@ and referenced
as @DMper@ - and the other two are served rather than harmed by it: an
EcoSpold 2 environment arrives folded already, and a scoring set writes its own
variable names on both sides of the formula. Two names that differ only in case
are therefore one name, and an environment stating both keeps one of them.
-}
evaluate :: Dialect -> M.Map Text Double -> Text -> Either Refusal Double
evaluate dialect env input = do
    formula <- readFormula dialect input
    value <- first (Unresolved . NE.nubBy ((==) `on` T.toLower)) (resolve (M.union (M.mapKeys T.toLower env) (constants dialect)) formula)
    finite dialect value

-- | openLCA gives no number for a formula that divides by zero or overflows; the older dialects keep what they computed.
finite :: Dialect -> Double -> Either Refusal Double
finite dialect value = case dialect of
    SimaPro -> Right value
    Arithmetic -> Right value
    EcoSpold2 -> Right value
    OpenLca
        | isNaN value || isInfinite value -> Left (Unreadable ("the formula gives no finite number: " <> T.pack (show value)))
        | otherwise -> Right value

-- | The names a dialect knows without a parameter: openLCA's @pi@ and @e@ (@true@ and @false@ are read as truth values).
constants :: Dialect -> M.Map Text Double
constants = \case
    SimaPro -> M.empty
    Arithmetic -> M.empty
    EcoSpold2 -> M.empty
    OpenLca -> M.fromList [("pi", pi), ("e", exp 1)]

readFormula :: Dialect -> Text -> Either Refusal Formula
readFormula dialect = first (Unreadable . refusalReason) . parse (sc *> grammar dialect <* eof) "" . readable dialect

-- | The grammar a dialect is read with.
grammar :: Dialect -> Parser Formula
grammar = \case
    SimaPro -> pFormula simaProLanguage
    Arithmetic -> pFormula arithmeticLanguage
    EcoSpold2 -> pFormula ecoSpold2Language
    OpenLca -> pOlca

{- | The value of a formula, or every name in it without one. All of them rather
than the first: a reader who fixes one would otherwise learn of the next only
on the following load.
-}
resolve :: M.Map Text Double -> Formula -> Either (NonEmpty Text) Double
resolve env = \case
    Literal x -> Right x
    Name name -> maybe (Left (name :| [])) Right (M.lookup (T.toLower name) env)
    Apply1 f a -> f <$> resolve env a
    Apply2 f a b -> case (resolve env a, resolve env b) of
        (Right x, Right y) -> Right (f x y)
        (Left missing, Right _) -> Left missing
        (Right _, Left missing) -> Left missing
        (Left missing, Left more) -> Left (missing <> more)
    ApplyMany f args -> f <$> resolveEach env args
    Choose condition yes no -> do
        c <- resolve env condition
        resolve env (if c /= 0 then yes else no)

-- | Every argument's value, or every name missing from any of them.
resolveEach :: M.Map Text Double -> [Formula] -> Either (NonEmpty Text) [Double]
resolveEach env args = maybe (sequence resolved) (Left . sconcat) (NE.nonEmpty (lefts resolved))
  where
    resolved :: [Either (NonEmpty Text) Double]
    resolved = map (resolve env) args

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

-- | What a language read by 'pFormula' adds to the arithmetic they all share.
data Language = Language
    { calls :: Text -> Maybe Function
    , comparisons :: [(Text, Double -> Double -> Bool)]
    , connectives :: Bool
    , percent :: Bool
    }

-- | A scoring set's: 'arithmeticFunctions', matched in lower case only.
arithmeticLanguage :: Language
arithmeticLanguage = Language{calls = (`lookup` arithmeticFunctions), comparisons = [], connectives = False, percent = False}

-- | EcoSpold 2's: 'ecoSpold2Functions', matched whatever their case.
ecoSpold2Language :: Language
ecoSpold2Language = Language{calls = (`lookup` ecoSpold2Functions) . T.toLower, comparisons = written, connectives = False, percent = True}

-- | SimaPro's: 'simaProFunctions', matched whatever their case.
simaProLanguage :: Language
simaProLanguage = Language{calls = (`lookup` simaProFunctions) . T.toLower, comparisons = written, connectives = True, percent = False}

-- | Longer operators first: a symbol that is a prefix of another would take its first character.
written :: [(Text, Double -> Double -> Bool)]
written = [("<=", (<=)), ("<>", (/=)), ("<", (<)), (">=", (>=)), (">", (>)), ("=", (==))]

-- | Precedence-climbing formula parser.
pFormula :: Language -> Parser Formula
pFormula language
    | connectives language = joined "or" (||) (joined "and" (&&) (pComparison language))
    | otherwise = pComparison language

joined :: Text -> (Bool -> Bool -> Bool) -> Parser Formula -> Parser Formula
joined word holds operand = operand >>= go
  where
    go :: Formula -> Parser Formula
    go acc = (connective word *> operand >>= go . Apply2 (truth (\x y -> holds (x /= 0) (y /= 0))) acc) <|> pure acc

-- | A word operator in any case, which a longer name merely starting with it (@order@) is not.
connective :: Text -> Parser Text
connective word = lexeme (try (string' word <* notFollowedBy (alphaNumChar <|> char '_')))

pComparison :: Language -> Parser Formula
pComparison language = pAddSub language >>= go
  where
    go :: Formula -> Parser Formula
    go acc = choice [symbol op *> pAddSub language >>= go . Apply2 (truth holds) acc | (op, holds) <- comparisons language] <|> pure acc

pAddSub :: Language -> Parser Formula
pAddSub language = pMulDiv language >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "+" *> pMulDiv language >>= go . Apply2 (+) acc)
            <|> (symbol "-" *> pMulDiv language >>= go . Apply2 (-) acc)
            <|> pure acc

pMulDiv :: Language -> Parser Formula
pMulDiv language = pUnary language >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "*" *> pUnary language >>= go . Apply2 (*) acc)
            <|> (symbol "/" *> pUnary language >>= go . Apply2 (/) acc)
            <|> pure acc

pUnary :: Language -> Parser Formula
pUnary language =
    (symbol "-" *> (Apply1 negate <$> pUnary language))
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
    (symbol "^" *> (Apply2 (**) base <$> pUnary language)) <|> pure base

-- | OpenFormula's postfix @%@, a hundredth, binding tighter than @^@: ecoinvent writes @0.06072*83.3%@.
pPercent :: Language -> Parser Formula
pPercent language
    | percent language = foldl' (\x _ -> Apply1 (/ 100) x) <$> pPrimary language <*> many (symbol "%")
    | otherwise = pPrimary language

pPrimary :: Language -> Parser Formula
pPrimary language =
    choice
        [ between (symbol "(") (symbol ")") (pFormula language)
        , pCall language
        , Literal <$> pNumber
        , Name <$> pIdentTok
        ]

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

arithmetic :: String -> (Double -> Double -> Double) -> Combine
arithmetic op f x y = Number <$> (Apply2 f <$> asNumber op x <*> asNumber op y)

-- | A comparison, of two numbers or of two truth values, as openLCA allows both.
compared :: String -> (Double -> Double -> Bool) -> Combine
compared op holds x y = case (x, y) of
    (Number a, Number b) -> Right (Truth (Apply2 (truth holds) a b))
    (Truth a, Truth b) -> Right (Truth (Apply2 (truth holds) a b))
    (Number _, Truth _) -> Left (op <> " compares a number with a truth value")
    (Truth _, Number _) -> Left (op <> " compares a truth value with a number")

logical :: String -> (Bool -> Bool -> Bool) -> Combine
logical op holds x y = Truth <$> (Apply2 (truth (\a b -> holds (a /= 0) (b /= 0))) <$> asTruth op x <*> asTruth op y)

-- | 1 when it holds, 0 otherwise: how a truth value is computed.
truth :: (Double -> Double -> Bool) -> Double -> Double -> Double
truth holds x y = if holds x y then 1 else 0

{- | openLCA's precedence, lowest first: or, xor, and, comparison, sum,
product, power, sign. Every level chains from the left, @^@ included, and the
sign applies to one element, below @^@, so @-2^2@ is @(-2)^2@ and @--2@ is
refused. A whole formula must give a number.
-}
pOlca :: Parser Formula
pOlca = pOlcaExpression >>= either fail pure . asNumber "a formula"

pOlcaExpression :: Parser Typed
pOlcaExpression = chainLeft pOlcaXor [(symbol "||", logical "||" (||)), (symbol "|", logical "|" (||))]

pOlcaXor :: Parser Typed
pOlcaXor = chainLeft pOlcaAnd [(keyword "xor", logical "xor" (/=))]

pOlcaAnd :: Parser Typed
pOlcaAnd = chainLeft pOlcaComparison [(symbol "&&", logical "&&" (&&)), (symbol "&", logical "&" (&&))]

-- Longer operators first: a symbol that is a prefix of another would take its first character.
pOlcaComparison :: Parser Typed
pOlcaComparison =
    chainLeft
        pOlcaSum
        [ (symbol "<=", compared "<=" (<=))
        , (symbol "<>", compared "<>" (/=))
        , (symbol "<", compared "<" (<))
        , (symbol ">=", compared ">=" (>=))
        , (symbol ">", compared ">" (>))
        , (symbol "==", compared "==" (==))
        , (symbol "=", compared "=" (==))
        , (symbol "!=", compared "!=" (/=))
        ]

pOlcaSum :: Parser Typed
pOlcaSum = chainLeft pOlcaProduct [(symbol "+", arithmetic "+" (+)), (symbol "-", arithmetic "-" (-))]

pOlcaProduct :: Parser Typed
pOlcaProduct =
    chainLeft
        pOlcaPower
        [ (symbol "*", arithmetic "*" (*))
        , (symbol "/", arithmetic "/" (/))
        , (keyword "div", arithmetic "div" roundedQuotient)
        , (keyword "mod", arithmetic "mod" remainder)
        ]

pOlcaPower :: Parser Typed
pOlcaPower = chainLeft pOlcaSigned [(symbol "^", arithmetic "^" (**))]

pOlcaSigned :: Parser Typed
pOlcaSigned =
    (symbol "-" *> (pOlcaElement >>= signed (Apply1 negate)))
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

-- | openLCA's @div@: the two operands rounded half up, then divided, truncated. Zero gives NaN, which 'evaluate' refuses.
roundedQuotient :: Double -> Double -> Double
roundedQuotient x y = case halfUp y of
    0 -> 0 / 0
    d -> fromInteger (halfUp x `quot` d)

-- | openLCA's @mod@: the remainder with the dividend's sign.
remainder :: Double -> Double -> Double
remainder x y = x - y * fromInteger (truncate (x / y))

halfUp :: Double -> Integer
halfUp x = floor (x + 0.5)

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
    | Unary (Double -> Double)
    | Binary (Double -> Double -> Double)
    | -- | min, max, sum, avg: any number of numbers, none giving 0, as in openLCA.
      Many ([Double] -> Double)
    | -- | not: one truth value, or none, which openLCA reads as false.
      Negation
    | -- | and, or: any number of truth values.
      Connective ([Bool] -> Bool)
    | -- | if: a truth value and two numbers, only one of them evaluated.
      Conditional
    | -- | Known to openLCA but not computed here, and why.
      Refused String

-- | openLCA's functions, by their lowercase name.
olcaFunctions :: [(Text, OlcaFunction)]
olcaFunctions =
    [ ("pi", Constant (Number (Literal pi)))
    , ("e", Constant (Number (Literal (exp 1))))
    , ("true", Constant (Truth (Literal 1)))
    , ("false", Constant (Truth (Literal 0)))
    , ("abs", Unary abs)
    , ("sqrt", Unary sqrt)
    , ("sqr", Unary (\x -> x * x))
    , ("exp", Unary exp)
    , ("ln", Unary log)
    , ("log", Unary (logBase 10))
    , ("lg", Unary (logBase 10))
    , ("ceil", Unary (fromInteger . ceiling))
    , ("floor", Unary (fromInteger . floor))
    , -- Half up, as openLCA rounds: round(-2.5) is -2.
      ("round", Unary (fromInteger . halfUp))
    , ("int", Unary truncated)
    , ("trunc", Unary truncated)
    , ("frac", Unary (\x -> x - truncated x))
    , ("sin", Unary sin)
    , ("cos", Unary cos)
    , ("tan", Unary tan)
    , ("cotan", Unary (recip . tan))
    , ("cot", Unary (recip . tan))
    , ("asin", Unary asin)
    , ("arcsin", Unary asin)
    , ("acos", Unary acos)
    , ("arccos", Unary acos)
    , ("atan", Unary atan)
    , ("arctan", Unary atan)
    , ("sinh", Unary sinh)
    , ("cosh", Unary cosh)
    , ("tanh", Unary tanh)
    , ("pow", Binary (**))
    , ("power", Binary (**))
    , ("ipower", Binary (\x y -> x ^^ (truncate y :: Integer)))
    , ("min", Many smallest)
    , ("max", Many largest)
    , ("sum", Many sum)
    , ("avg", Many mean)
    , ("mean", Many mean)
    , ("not", Negation)
    , ("and", Connective and)
    , ("or", Connective or)
    , ("if", Conditional)
    , ("iff", Conditional)
    , ("iif", Conditional)
    , ("random", Refused "a load must give the same numbers twice")
    , ("rand", Refused "a load must give the same numbers twice")
    ]

truncated :: Double -> Double
truncated = fromInteger . truncate

-- | Of any number of numbers, none giving 0.
smallest, largest, mean :: [Double] -> Double
smallest = maybe 0 (getMin . sconcat . fmap Min) . NE.nonEmpty
largest = maybe 0 (getMax . sconcat . fmap Max) . NE.nonEmpty
mean xs = if null xs then 0 else sum xs / fromIntegral (length xs)

-- | A call, or why it is not one: an unknown name, a refused function, a wrong number or type of arguments.
olcaCall :: Text -> [Typed] -> Either String Typed
olcaCall name args = case lookup name olcaFunctions of
    Nothing -> Left ("unknown function " <> function)
    Just known -> case known of
        Constant value -> case args of
            [] -> Right value
            _ : _ -> arity "no argument"
        Unary f ->
            numbers >>= \case
                [x] -> Right (Number (Apply1 f x))
                _ -> arity "one argument"
        Binary f ->
            numbers >>= \case
                [x, y] -> Right (Number (Apply2 f x y))
                _ -> arity "two arguments"
        Many f -> Number . ApplyMany f <$> numbers
        Negation ->
            truths >>= \case
                [] -> Right (Truth (Literal 0))
                [x] -> Right (Truth (Apply1 (\v -> if v == 0 then 1 else 0) x))
                _ -> arity "one argument"
        Connective f -> Truth . ApplyMany (\vs -> if f (map (/= 0) vs) then 1 else 0) <$> truths
        Conditional -> case args of
            [condition, yes, no] -> Number <$> (Choose <$> asTruth function condition <*> asNumber function yes <*> asNumber function no)
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

-- | What a function of the languages read by 'pFormula' takes.
data Function
    = OneArg (Double -> Double)
    | TwoArgs (Double -> Double -> Double)
    | -- | A second argument that may be left out, and the value it then has.
      SecondOr Double (Double -> Double -> Double)
    | -- | One argument or more.
      AnyArgs ([Double] -> Double)
    | -- | A condition, the value when it holds (any number but 0), and the value otherwise, 0 when left out.
      IfThen

-- | The functions scoring-set formulas know, by the lower-case name a formula calls them with.
arithmeticFunctions :: [(Text, Function)]
arithmeticFunctions =
    [ ("abs", OneArg abs)
    , ("sqrt", OneArg sqrt)
    , ("log", OneArg log)
    , ("exp", OneArg exp)
    , ("ln", OneArg log)
    , ("min", TwoArgs min)
    , ("max", TwoArgs max)
    ]

-- | EcoSpold 2's, from its formula language specification, by lower-case name.
ecoSpold2Functions :: [(Text, Function)]
ecoSpold2Functions =
    [ ("abs", OneArg abs)
    , ("sqrt", OneArg sqrt)
    , ("exp", OneArg exp)
    , ("ln", OneArg log)
    , ("log", SecondOr 10 (flip logBase))
    , ("log10", OneArg (logBase 10))
    , ("power", TwoArgs (**))
    , ("min", AnyArgs smallest)
    , ("max", AnyArgs largest)
    , ("sum", AnyArgs sum)
    , ("average", AnyArgs mean)
    , ("if", IfThen)
    ]

simaProFunctions :: [(Text, Function)]
simaProFunctions =
    [ ("abs", OneArg abs)
    , ("sqr", OneArg (\x -> x * x))
    , ("sqrt", OneArg sqrt)
    , ("exp", OneArg exp)
    , ("ln", OneArg log)
    , ("log", OneArg (logBase 10))
    , ("trunc", OneArg truncated)
    , ("int", OneArg truncated)
    , ("frac", OneArg (\x -> x - truncated x))
    , -- Half to even: round(2.5) is 2.
      ("round", OneArg (fromInteger . round))
    , ("ceil", OneArg (fromInteger . ceiling))
    , ("floor", OneArg (fromInteger . floor))
    , ("power", TwoArgs (**))
    , ("ipower", TwoArgs (\x y -> x ^^ (truncate y :: Integer)))
    , ("sin", OneArg sin)
    , ("cos", OneArg cos)
    , ("tan", OneArg tan)
    , ("cotan", OneArg (recip . tan))
    , ("arcsin", OneArg asin)
    , ("arccos", OneArg acos)
    , ("arctan", OneArg atan)
    , ("sinh", OneArg sinh)
    , ("cosh", OneArg cosh)
    , ("tanh", OneArg tanh)
    , ("min", AnyArgs smallest)
    , ("max", AnyArgs largest)
    , ("not", OneArg (\x -> if x == 0 then 1 else 0))
    , ("iff", IfThen)
    ]

functionNames :: [Text]
functionNames = map fst arithmeticFunctions

{- | A function applied to its arguments.

A name directly followed by an opening parenthesis is a call even when no
function goes by that name, and it is refused by that name: read as a variable
instead, the refusal would land on the parenthesis and never say which function
was missing. Only the name and its parenthesis are tried: past them the text is
committed to the call, so a refusal inside the arguments is the one reported.
-}
pCall :: Language -> Parser Formula
pCall language = do
    name <- try (pIdentTok <* symbol "(")
    function <- maybe (fail ("unknown function " <> T.unpack name)) pure (calls language name)
    args <- sepBy1 (pFormula language) (symbol ";") <* symbol ")"
    either fail pure (applied name function args)

-- | A call, or why its arguments do not fit the function.
applied :: Text -> Function -> [Formula] -> Either String Formula
applied name function args = case (function, args) of
    (OneArg f, [x]) -> Right (Apply1 f x)
    (TwoArgs f, [x, y]) -> Right (Apply2 f x y)
    (SecondOr _ f, [x, y]) -> Right (Apply2 f x y)
    (SecondOr y f, [x]) -> Right (Apply2 f x (Literal y))
    (AnyArgs f, _) -> Right (ApplyMany f args)
    (IfThen, [condition, yes]) -> Right (Choose condition yes (Literal 0))
    (IfThen, [condition, yes, no]) -> Right (Choose condition yes no)
    _ -> Left (T.unpack name <> " takes " <> takes <> ", given " <> show (length args))
  where
    takes :: String
    takes = case function of
        OneArg _ -> "one argument"
        TwoArgs _ -> "two arguments"
        SecondOr _ _ -> "one or two arguments"
        AnyArgs _ -> "one argument or more"
        IfThen -> "two or three arguments"

{- | Whether the text reads as a formula, whatever the names in it.
Used to detect allocation fields vs waste type descriptions in SimaPro CSV.
-}
isExpression :: Dialect -> Text -> Bool
isExpression dialect = isRight . readFormula dialect

{- | Collect all variable identifiers referenced in an expression.
Built-in function names are excluded.
Returns the empty list if the expression cannot be tokenized.
-}
collectIdentifiers :: Dialect -> Text -> [Text]
collectIdentifiers dialect input =
    case parse (sc *> (catMaybes <$> many (pToken dialect)) <* eof) "" (readable dialect input) of
        Right names -> filter (isVariable dialect) names
        Left _ -> []

-- | Whether a name read in this dialect is a variable rather than one of its function or operator words.
isVariable :: Dialect -> Text -> Bool
isVariable dialect name = case dialect of
    -- A name SimaPro calls is told by the parenthesis after it ('pToken'); and and or join.
    SimaPro -> T.toLower name `notElem` ["and", "or"]
    Arithmetic -> isNothing (calls arithmeticLanguage name)
    -- A name EcoSpold 2 calls is told by the parenthesis after it ('pToken').
    EcoSpold2 -> True
    -- pi, e, true and false stay: a parameter may bear the name, and the caller decides.
    OpenLca -> name `notElem` ["div", "mod", "xor"]

-- | One token, or one character of whatever this is not meant to collect.
pToken :: Dialect -> Parser (Maybe Text)
pToken dialect =
    try (Just <$> name)
        <|> (Nothing <$ try (lexeme pNumber))
        <|> (Nothing <$ anySingle)
  where
    -- An openLCA, EcoSpold 2 or SimaPro name followed by a parenthesis is a call, not a variable.
    name :: Parser Text
    name = case dialect of
        SimaPro -> pIdentTok <* notFollowedBy (symbol "(")
        Arithmetic -> pIdentTok
        EcoSpold2 -> pIdentTok <* notFollowedBy (symbol "(")
        OpenLca -> pOlcaName <* notFollowedBy (symbol "(")

pIdentTok :: Parser Text
pIdentTok = lexeme (T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')))

{- | The known values with every calculated parameter that evaluates added,
each formula tried again until a pass adds nothing: a parameter may refer to
one declared after it. A formula that never evaluates is left out, and its
caller learns of it by its absence.
-}
settle :: Dialect -> M.Map Text Double -> [(Text, Text)] -> M.Map Text Double
settle dialect known calculated
    | M.size next == M.size known = next
    | otherwise = settle dialect next calculated
  where
    next :: M.Map Text Double
    next = foldl' step known calculated

    step :: M.Map Text Double -> (Text, Text) -> M.Map Text Double
    step acc (name, formula) = either (const acc) (\v -> M.insert name v acc) (evaluate dialect acc formula)

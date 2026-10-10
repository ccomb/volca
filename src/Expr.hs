{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Expression evaluator: arithmetic (+, -, *, \/, ^), variables, parentheses
and a few functions, over formulas written in more than one language.

One grammar reads them all, because they agree on everything an expression is
made of. Where they disagree is at the edges, and a 'Dialect' says which set of
edges a formula was written against: every entry point asks for one, so no
formula is read in a language nobody chose for it. openLCA's language departs
from the other two on precedence and on @log@, so it is read by a grammar of
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
import Control.Monad (mfilter, when)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Either (isRight, lefts)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Semigroup (Max (..), Min (..), sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

{- | The language a formula was written in.

Three, because three are what the callers actually have.
-}
data Dialect
    = {- | SimaPro's own, as its Delphi formula parser reads it: @\/\/@ opens a
      comment and the expression ends there. Its text reaches the evaluator
      already through 'normalizeExpr', which is where the file's decimal
      separator is known.
      -}
      SimaPro
    | {- | Arithmetic with no comment and a comma between two arguments: an
      EcoSpold 2 @mathematicalRelation@, and the formulas someone writes in
      a scoring set. Neither language has a line comment, so a @\/\/@ in one
      of them is a mistake and reads as one.
      -}
      Arithmetic
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
    OpenLca
        | isNaN value || isInfinite value -> Left (Unreadable ("the formula gives no finite number: " <> T.pack (show value)))
        | otherwise -> Right value

-- | The names a dialect knows without a parameter: openLCA's @pi@ and @e@ (@true@ and @false@ are read as truth values).
constants :: Dialect -> M.Map Text Double
constants = \case
    SimaPro -> M.empty
    Arithmetic -> M.empty
    OpenLca -> M.fromList [("pi", pi), ("e", exp 1)]

readFormula :: Dialect -> Text -> Either Refusal Formula
readFormula dialect = first (Unreadable . refusalReason) . parse (sc *> grammar dialect <* eof) "" . readable dialect

-- | The grammar a dialect is read with.
grammar :: Dialect -> Parser Formula
grammar = \case
    SimaPro -> pFormula
    Arithmetic -> pFormula
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

-- | Precedence-climbing formula parser
pFormula :: Parser Formula
pFormula = pAddSub

pAddSub :: Parser Formula
pAddSub = pMulDiv >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "+" *> pMulDiv >>= go . Apply2 (+) acc)
            <|> (symbol "-" *> pMulDiv >>= go . Apply2 (-) acc)
            <|> pure acc

pMulDiv :: Parser Formula
pMulDiv = pUnary >>= go
  where
    go :: Formula -> Parser Formula
    go acc =
        (symbol "*" *> pUnary >>= go . Apply2 (*) acc)
            <|> (symbol "/" *> pUnary >>= go . Apply2 (/) acc)
            <|> pure acc

pUnary :: Parser Formula
pUnary =
    (symbol "-" *> (Apply1 negate <$> pUnary))
        <|> (symbol "+" *> pUnary)
        <|> pPower

{- | Exponentiation, right-associative and binding tighter than @*@ and @/@.

The exponent goes through 'pUnary' rather than straight back to 'pPower', so it
may carry a sign. SimaPro writes scale factors that way – @1*10^-3*50@ – and
without it the @-@ met 'pPrimary', which knows numbers but not signs, and the
whole expression failed.
-}
pPower :: Parser Formula
pPower = do
    base <- pPrimary
    (symbol "^" *> (Apply2 (**) base <$> pUnary)) <|> pure base

pPrimary :: Parser Formula
pPrimary =
    choice
        [ between (symbol "(") (symbol ")") pFormula
        , pCall
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
tried, as in 'pCall1': past them a refusal is about this call.
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
    , ("min", Many (maybe 0 (getMin . sconcat . fmap Min) . NE.nonEmpty))
    , ("max", Many (maybe 0 (getMax . sconcat . fmap Max) . NE.nonEmpty))
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
  where
    truncated :: Double -> Double
    truncated = fromInteger . truncate

    mean :: [Double] -> Double
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

-- | The functions the grammar knows, by the name a formula calls them with.
functions1 :: [(Text, Double -> Double)]
functions1 = [("abs", abs), ("sqrt", sqrt), ("log", log), ("exp", exp), ("ln", log)]

functions2 :: [(Text, Double -> Double -> Double)]
functions2 = [("min", min), ("max", max)]

functionNames :: [Text]
functionNames = map fst functions1 <> map fst functions2

{- | A function applied to its arguments.

A name directly followed by an opening parenthesis is a call even when no
function goes by that name, and it is refused by that name: read as a variable
instead, the refusal would land on the parenthesis and never say which function
was missing.
-}
pCall :: Parser Formula
pCall = choice (map (uncurry pCall1) functions1 <> map (uncurry pCall2) functions2 <> [pUnknownCall])

{- | Only the name and its opening parenthesis are tried: past them the text is
committed to the call, so a refusal inside the arguments is the one reported
rather than lost to a backtrack that rereads the name as a variable.
-}
pCall1 :: Text -> (Double -> Double) -> Parser Formula
pCall1 name f = try (lexeme (string name) *> symbol "(") *> (Apply1 f <$> pFormula) <* symbol ")"

pCall2 :: Text -> (Double -> Double -> Double) -> Parser Formula
pCall2 name f = do
    _ <- try (lexeme (string name) *> symbol "(")
    x <- pFormula
    _ <- symbol ";"
    y <- pFormula
    _ <- symbol ")"
    pure (Apply2 f x y)

pUnknownCall :: Parser Formula
pUnknownCall = do
    name <- try (mfilter (`notElem` functionNames) pIdentTok <* lookAhead (symbol "("))
    fail ("unknown function " <> T.unpack name)

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
    SimaPro -> name `notElem` functionNames
    Arithmetic -> name `notElem` functionNames
    -- pi, e, true and false stay: a parameter may bear the name, and the caller decides.
    OpenLca -> name `notElem` ["div", "mod", "xor"]

-- | One token, or one character of whatever this is not meant to collect.
pToken :: Dialect -> Parser (Maybe Text)
pToken dialect =
    try (Just <$> name)
        <|> (Nothing <$ try (lexeme pNumber))
        <|> (Nothing <$ anySingle)
  where
    -- An openLCA name followed by a parenthesis is a call, not a variable.
    name :: Parser Text
    name = case dialect of
        SimaPro -> pIdentTok
        Arithmetic -> pIdentTok
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

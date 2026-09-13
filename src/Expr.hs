{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Expression evaluator: arithmetic (+, -, *, \/, ^), variables, parentheses
and a few functions, over formulas written in more than one language.

One grammar reads them all, because they agree on everything an expression is
made of. Where they disagree is at the edges, and a 'Dialect' says which set of
edges a formula was written against: every entry point asks for one, so no
formula is read in a language nobody chose for it.

A formula is read into a 'Formula' first and given values second. The two steps
fail for two reasons a reader acts on differently, text that is not a formula
and a formula naming something without a value, and every such name is only
known once the whole formula has been read.
-}
module Expr (
    Dialect (..),
    Refusal (..),
    evaluate,
    describeRefusal,
    normalizeExpr,
    isExpression,
    collectIdentifiers,
) where

import Amount (readAmount)
import Control.Monad (mfilter, when)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

{- | The language a formula was written in.

Two, because two are what the callers actually have. A third belongs here the
day a third language is read, and not before.
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

{- | Bring a formula to the one form the grammar reads.

Cutting a comment off as text rather than skipping it in the lexer is what lets
one grammar serve both dialects, and it also keeps 'collectIdentifiers' and
'evaluate' looking at the very same characters. Per line, because a formula
that spans two of them ends its comment at the first.
-}
readable :: Dialect -> Text -> Text
readable SimaPro = T.strip . T.intercalate "\n" . map (fst . T.breakOn "//") . T.lines
readable Arithmetic = T.strip . normalizeExpr '.'

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
    first (Unresolved . NE.nub) (resolve (M.mapKeys T.toLower env) formula)

readFormula :: Dialect -> Text -> Either Refusal Formula
readFormula dialect = first (Unreadable . refusalReason) . parse (sc *> pFormula <* eof) "" . readable dialect

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

pCall1 :: Text -> (Double -> Double) -> Parser Formula
pCall1 name f = try $ lexeme (string name) *> between (symbol "(") (symbol ")") (Apply1 f <$> pFormula)

pCall2 :: Text -> (Double -> Double -> Double) -> Parser Formula
pCall2 name f = try $ do
    _ <- lexeme (string name)
    _ <- symbol "("
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
    case parse (sc *> pCollect <* eof) "" (readable dialect input) of
        Right names -> filter (`notElem` functionNames) names
        Left _ -> []

pCollect :: Parser [Text]
pCollect = catMaybes <$> many pToken

-- | One token, or one character of whatever this is not meant to collect.
pToken :: Parser (Maybe Text)
pToken =
    try (Just <$> pIdentTok)
        <|> (Nothing <$ try (lexeme pNumber))
        <|> (Nothing <$ anySingle)

pIdentTok :: Parser Text
pIdentTok = lexeme (T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')))

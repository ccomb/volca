{-# LANGUAGE OverloadedStrings #-}

{- | Expression evaluator: arithmetic (+, -, *, \/, ^), variables, parentheses
and a few functions, over formulas written in more than one language.

One grammar reads them all, because they agree on everything an expression is
made of. Where they disagree is at the edges, and a 'Dialect' says which set of
edges a formula was written against: every entry point asks for one, so no
formula is read in a language nobody chose for it.
-}
module Expr (
    Dialect (..),
    evaluate,
    normalizeExpr,
    isExpression,
    collectIdentifiers,
) where

import Amount (readAmount)
import Control.Monad (void, when)
import Data.Char (isDigit)
import Data.Either (isRight)
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
evaluate :: Dialect -> M.Map Text Double -> Text -> Either String Double
evaluate dialect env input =
    let envCI = M.mapKeys T.toLower env
     in case parse (sc *> pExpr envCI <* eof) "" (readable dialect input) of
            Left err -> Left (errorBundlePretty err)
            Right val -> Right val

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

-- | Precedence-climbing expression parser
pExpr :: M.Map Text Double -> Parser Double
pExpr = pAddSub

pAddSub :: M.Map Text Double -> Parser Double
pAddSub env = pMulDiv env >>= go
  where
    go acc =
        (symbol "+" *> pMulDiv env >>= go . (acc +))
            <|> (symbol "-" *> pMulDiv env >>= go . (acc -))
            <|> pure acc

pMulDiv :: M.Map Text Double -> Parser Double
pMulDiv env = pUnary env >>= go
  where
    go acc =
        (symbol "*" *> pUnary env >>= go . (acc *))
            <|> (symbol "/" *> pUnary env >>= go . (acc /))
            <|> pure acc

pUnary :: M.Map Text Double -> Parser Double
pUnary env =
    (symbol "-" *> (negate <$> pUnary env))
        <|> (symbol "+" *> pUnary env)
        <|> pPower env

{- | Exponentiation, right-associative and binding tighter than @*@ and @/@.

The exponent goes through 'pUnary' rather than straight back to 'pPower', so it
may carry a sign. SimaPro writes scale factors that way — @1*10^-3*50@ — and
without it the @-@ met 'pPrimary', which knows numbers but not signs, and the
whole expression failed.
-}
pPower :: M.Map Text Double -> Parser Double
pPower env = do
    base <- pPrimary env
    (symbol "^" *> ((base **) <$> pUnary env)) <|> pure base

pPrimary :: M.Map Text Double -> Parser Double
pPrimary env =
    choice
        [ between (symbol "(") (symbol ")") (pExpr env)
        , pFunc env
        , pNumber
        , pVariable env
        ]

{- | A numeric literal, tokenized here and read by 'readAmount'.

The integer part is optional. SimaPro exports drop it: Agribalyse writes the
cereal fungicide mix as @0,45+0,247+,067@, whose last term normalizes to
@.067@. Megaparsec's 'L.float' requires a digit before the point, so one such
term used to fail the /whole/ expression, and the caller then fell back to
reading the leading number — 0.45 where the file says 0.764.

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

-- | Look up a variable in the pre-lowercased env. Case-insensitive by construction.
pVariable :: M.Map Text Double -> Parser Double
pVariable env = do
    name <- lexeme $ T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    case M.lookup (T.toLower name) env of
        Just val -> pure val
        Nothing -> fail $ "Unknown variable: " ++ T.unpack name

pFunc :: M.Map Text Double -> Parser Double
pFunc env =
    choice
        [ pFunc1 "abs" abs env
        , pFunc1 "sqrt" sqrt env
        , pFunc1 "log" log env
        , pFunc1 "exp" exp env
        , pFunc1 "ln" log env
        , pFunc2 "min" min env
        , pFunc2 "max" max env
        ]

pFunc1 :: Text -> (Double -> Double) -> M.Map Text Double -> Parser Double
pFunc1 name f env = try $ lexeme (string name) *> between (symbol "(") (symbol ")") (f <$> pExpr env)

pFunc2 :: Text -> (Double -> Double -> Double) -> M.Map Text Double -> Parser Double
pFunc2 name f env = try $ do
    _ <- lexeme (string name)
    _ <- symbol "("
    x <- pExpr env
    _ <- symbol ";"
    y <- pExpr env
    _ <- symbol ")"
    pure (f x y)

{- | Check if text is syntactically a valid expression (number, variable, or formula).
Does NOT evaluate — accepts any variable name without needing an environment.
Used to detect allocation fields vs waste type descriptions in SimaPro CSV.
-}
isExpression :: Dialect -> Text -> Bool
isExpression dialect input =
    isRight $ parse (sc *> pSynExpr <* eof) "" (readable dialect input)

{- | Collect all variable identifiers referenced in an expression.
Built-in function names (abs, sqrt, log, exp, ln, min, max) are excluded.
Returns the empty list if the expression cannot be tokenized.
-}
collectIdentifiers :: Dialect -> Text -> [Text]
collectIdentifiers dialect input =
    case parse (sc *> pCollect <* eof) "" (readable dialect input) of
        Right names -> filter (`notElem` reservedFuncs) names
        Left _ -> []
  where
    reservedFuncs = ["abs", "sqrt", "log", "exp", "ln", "min", "max"]

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

-- Syntax-only parsers: mirror pExpr structure but discard values, accept any identifier
pSynExpr :: Parser ()
pSynExpr = pSynAddSub

pSynAddSub :: Parser ()
pSynAddSub = pSynMulDiv >> go
  where
    go = (symbol "+" *> pSynMulDiv >> go) <|> (symbol "-" *> pSynMulDiv >> go) <|> pure ()

pSynMulDiv :: Parser ()
pSynMulDiv = pSynUnary >> go
  where
    go = (symbol "*" *> pSynUnary >> go) <|> (symbol "/" *> pSynUnary >> go) <|> pure ()

pSynUnary :: Parser ()
pSynUnary = (symbol "-" *> pSynUnary) <|> (symbol "+" *> pSynUnary) <|> pSynPower

pSynPower :: Parser ()
pSynPower = pSynPrimary >> ((symbol "^" *> pSynUnary) <|> pure ())

pSynPrimary :: Parser ()
pSynPrimary =
    choice
        [ between (symbol "(") (symbol ")") pSynExpr
        , pSynFunc
        , void pNumber
        , pSynIdent
        ]

pSynIdent :: Parser ()
pSynIdent = void (lexeme ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')))

pSynFunc :: Parser ()
pSynFunc =
    choice
        [ pSynFunc1 "abs"
        , pSynFunc1 "sqrt"
        , pSynFunc1 "log"
        , pSynFunc1 "exp"
        , pSynFunc1 "ln"
        , pSynFunc2 "min"
        , pSynFunc2 "max"
        ]

pSynFunc1 :: Text -> Parser ()
pSynFunc1 name = try $ lexeme (string name) *> between (symbol "(") (symbol ")") pSynExpr

pSynFunc2 :: Text -> Parser ()
pSynFunc2 name = try $ void (lexeme (string name) *> symbol "(" *> pSynExpr *> symbol ";" *> pSynExpr *> symbol ")")

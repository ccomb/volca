{-# LANGUAGE OverloadedStrings #-}

{- | Expression evaluator.
Supports arithmetic (+, -, *, /, ^), variables, parentheses, and common functions.
-}
module Expr (
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

{- | Evaluate a SimaPro expression with variable substitution.
All expressions must be pre-normalized via 'normalizeExpr' (decimal = '.', arg separator = ';').
Variable lookup is case-insensitive to match SimaPro semantics: Agribalyse and other
databases freely mix casing (e.g. param defined as @Dmper@, referenced as @DMper@).
-}
evaluate :: M.Map Text Double -> Text -> Either String Double
evaluate env input =
    let envCI = M.mapKeys T.toLower env
     in case parse (sc *> pExpr envCI <* eof) "" (T.strip input) of
            Left err -> Left (errorBundlePretty err)
            Right val -> Right val

-- | Normalize expression text so decimal is always '.' and function arg separator is always ';'.
normalizeExpr :: Char -> Text -> Text
normalizeExpr '.' = T.map (\c -> if c == ',' then ';' else c)
normalizeExpr ',' = T.map (\c -> if c == ',' then '.' else c)
normalizeExpr _ = id

{- | Whitespace, and the rest of a line once @\/\/@ opens a comment.

SimaPro is a Delphi program and its formula parser keeps Pascal's line comment,
so @weight_PET_g\/\/process1_yield\/process2_yield@ is the weight and nothing
else: the two yields are commented out, and the number the author meant to
divide is the number that goes into the result. Reading @\/\/@ as a division
gives a different result, and reading it as an error gives none at all, which is
how the amounts under it became zero.
-}
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

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
isExpression :: Char -> Text -> Bool
isExpression decimalSep input =
    isRight $ parse (sc *> pSynExpr <* eof) "" (T.strip (normalizeExpr decimalSep input))

{- | Collect all variable identifiers referenced in an expression.
Built-in function names (abs, sqrt, log, exp, ln, min, max) are excluded.
Returns the empty list if the expression cannot be tokenized.
-}
collectIdentifiers :: Char -> Text -> [Text]
collectIdentifiers decimalSep input =
    case parse (sc *> pCollect <* eof) "" (T.strip (normalizeExpr decimalSep input)) of
        Right names -> filter (`notElem` reservedFuncs) names
        Left _ -> []
  where
    reservedFuncs = ["abs", "sqrt", "log", "exp", "ln", "min", "max"]

pCollect :: Parser [Text]
pCollect = catMaybes <$> many pToken

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

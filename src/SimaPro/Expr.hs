{-# LANGUAGE OverloadedStrings #-}

-- | Expression evaluator for SimaPro parameterized amounts.
-- Supports arithmetic (+, -, *, /, ^), variables, parentheses, and common functions.
module SimaPro.Expr
    ( evaluate
    , normalizeExpr
    , isExpression
    ) where

import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Evaluate a SimaPro expression with variable substitution.
-- All expressions must be pre-normalized via 'normalizeExpr' (decimal = '.', arg separator = ';').
evaluate :: M.Map Text Double -> Text -> Either String Double
evaluate env input =
    case parse (sc *> pExpr env <* eof) "" (T.strip input) of
        Left err -> Left (errorBundlePretty err)
        Right val -> Right val

-- | Normalize expression text so decimal is always '.' and function arg separator is always ';'.
normalizeExpr :: Char -> Text -> Text
normalizeExpr '.' = T.map (\c -> if c == ',' then ';' else c)
normalizeExpr ',' = T.map (\c -> if c == ',' then '.' else c)
normalizeExpr _   = id

-- Whitespace consumer
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
    go acc = (symbol "+" *> pMulDiv env >>= go . (acc +))
         <|> (symbol "-" *> pMulDiv env >>= go . (acc -))
         <|> pure acc

pMulDiv :: M.Map Text Double -> Parser Double
pMulDiv env = pUnary env >>= go
  where
    go acc = (symbol "*" *> pUnary env >>= go . (acc *))
         <|> (symbol "/" *> pUnary env >>= go . (acc /))
         <|> pure acc

pUnary :: M.Map Text Double -> Parser Double
pUnary env = (symbol "-" *> (negate <$> pUnary env))
         <|> (symbol "+" *> pUnary env)
         <|> pPower env

pPower :: M.Map Text Double -> Parser Double
pPower env = do
    base <- pPrimary env
    (symbol "^" *> ((base **) <$> pPower env)) <|> pure base

pPrimary :: M.Map Text Double -> Parser Double
pPrimary env = choice
    [ between (symbol "(") (symbol ")") (pExpr env)
    , pFunc env
    , pNumber
    , pVariable env
    ]

pNumber :: Parser Double
pNumber = lexeme $ try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer))

pVariable :: M.Map Text Double -> Parser Double
pVariable env = do
    name <- lexeme $ T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    case M.lookup name env of
        Just val -> pure val
        Nothing -> fail $ "Unknown variable: " ++ T.unpack name

pFunc :: M.Map Text Double -> Parser Double
pFunc env = choice
    [ pFunc1 "abs" abs env, pFunc1 "sqrt" sqrt env
    , pFunc1 "log" log env, pFunc1 "exp" exp env, pFunc1 "ln" log env
    , pFunc2 "min" min env, pFunc2 "max" max env
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

-- | Check if text is syntactically a valid expression (number, variable, or formula).
-- Does NOT evaluate — accepts any variable name without needing an environment.
-- Used to detect allocation fields vs waste type descriptions in SimaPro CSV.
isExpression :: Char -> Text -> Bool
isExpression decimalSep input =
    isRight $ parse (sc *> pSynExpr <* eof) "" (T.strip (normalizeExpr decimalSep input))

-- Syntax-only parsers: mirror pExpr structure but discard values, accept any identifier
pSynExpr :: Parser ()
pSynExpr = pSynAddSub

pSynAddSub :: Parser ()
pSynAddSub = pSynMulDiv >> go
  where go = (symbol "+" *> pSynMulDiv >> go) <|> (symbol "-" *> pSynMulDiv >> go) <|> pure ()

pSynMulDiv :: Parser ()
pSynMulDiv = pSynUnary >> go
  where go = (symbol "*" *> pSynUnary >> go) <|> (symbol "/" *> pSynUnary >> go) <|> pure ()

pSynUnary :: Parser ()
pSynUnary = (symbol "-" *> pSynUnary) <|> (symbol "+" *> pSynUnary) <|> pSynPower

pSynPower :: Parser ()
pSynPower = pSynPrimary >> ((symbol "^" *> pSynPower) <|> pure ())

pSynPrimary :: Parser ()
pSynPrimary = choice
    [ between (symbol "(") (symbol ")") pSynExpr
    , pSynFunc
    , () <$ pNumber
    , pSynIdent
    ]

pSynIdent :: Parser ()
pSynIdent = () <$ lexeme ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

pSynFunc :: Parser ()
pSynFunc = choice
    [ pSynFunc1 "abs", pSynFunc1 "sqrt", pSynFunc1 "log", pSynFunc1 "exp", pSynFunc1 "ln"
    , pSynFunc2 "min", pSynFunc2 "max"
    ]

pSynFunc1 :: Text -> Parser ()
pSynFunc1 name = try $ lexeme (string name) *> between (symbol "(") (symbol ")") pSynExpr

pSynFunc2 :: Text -> Parser ()
pSynFunc2 name = try $ lexeme (string name) *> symbol "(" *> pSynExpr *> symbol ";" *> pSynExpr *> symbol ")" *> pure ()

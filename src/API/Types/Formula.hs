{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Formula where

import API.JsonOptions (Stripped (..), parseClosed)
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Expr
import GHC.Generics

-- | VoLCA's formula language, as the engine reads it.
data FormulaLanguage = FormulaLanguage
    { flFunctions :: [FormulaFunction]
    , flOperators :: [[Text]] -- The operators by how tightly they bind, loosest first, each by its usage so the sign (-x) and the subtraction (a - b) stay apart; a function called by name binds tighter than all of them
    , flRules :: [Text] -- What a writer must know beyond the functions and operators
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FormulaLanguage)

-- | One function or operator of the language.
data FormulaFunction = FormulaFunction
    { ffName :: Text -- As it is written: a function's name or an operator's symbol
    , ffForm :: Text -- "function", "operator" between two arguments, or "prefix" before one
    , ffUsage :: Text
    , ffMeaning :: Text
    , ffExample :: Text
    , ffValue :: Double -- What the example gives
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FormulaFunction)

formulaLanguage :: FormulaLanguage
formulaLanguage =
    FormulaLanguage
        { flFunctions = map (described . Expr.reference) Expr.functions
        , flOperators = map (map (Expr.usage . Expr.reference)) Expr.operators
        , flRules = Expr.volcaRules
        }
  where
    described :: Expr.Reference -> FormulaFunction
    described r =
        FormulaFunction
            { ffName = written (Expr.spelling r)
            , ffForm = case Expr.spelling r of
                Expr.Called _ -> "function"
                Expr.Infix _ -> "operator"
                Expr.Prefix _ -> "prefix"
            , ffUsage = Expr.usage r
            , ffMeaning = Expr.meaning r
            , ffExample = Expr.example r
            , ffValue = Expr.exampleValue r
            }

    written :: Expr.Spelling -> Text
    written = \case
        Expr.Called name -> name
        Expr.Infix op -> op
        Expr.Prefix op -> op

-- | A formula to check where it would be written: in a process, on one of its lines, or among the database's parameters.
data FormulaCheckRequest = FormulaCheckRequest
    { fcqFormula :: Text
    , fcqProcess :: Maybe Text -- The process whose names it reads; the database's parameters alone when absent
    , fcqExchange :: Maybe Int -- The line it would be the formula of, by its position among the process's exchanges
    }
    deriving (Generic)
    deriving (ToSchema) via (Stripped FormulaCheckRequest)

instance FromJSON FormulaCheckRequest where
    parseJSON = parseClosed

-- | What a formula gives at the defaults, or why it gives nothing: the problem is set exactly when the value is not.
data FormulaCheckResponse = FormulaCheckResponse
    { fcrValue :: Maybe Double
    , fcrReads :: [Text] -- The names it reads, each once, in lower case
    , fcrWarnings :: [Text] -- What looks wrong without stopping the formula, such as a parameter whose unit does not convert to the line's
    , fcrProblem :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FormulaCheckResponse)

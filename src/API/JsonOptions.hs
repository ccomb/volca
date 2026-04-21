{-# LANGUAGE FlexibleContexts #-}

module API.JsonOptions (
    stripLowerPrefix,
    strippedToJSON,
    strippedToEncoding,
    strippedParseJSON,
    strippedSchemaOptions,
) where

import Data.Aeson (
    Encoding,
    Options,
    Value,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
 )
import Data.Aeson.Types (GFromJSON, GToEncoding, GToJSON', Parser, Zero)
import Data.Char (isLower, toLower)
import Data.OpenApi.Schema (SchemaOptions, fromAesonOptions)
import GHC.Generics (Generic, Rep)

stripLowerPrefix :: Options
stripLowerPrefix =
    defaultOptions
        { fieldLabelModifier = lowerFirst . dropWhile isLower
        }
  where
    lowerFirst "" = ""
    lowerFirst (c : cs) = toLower c : cs

strippedToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
strippedToJSON = genericToJSON stripLowerPrefix

strippedToEncoding :: (Generic a, GToEncoding Zero (Rep a)) => a -> Encoding
strippedToEncoding = genericToEncoding stripLowerPrefix

strippedParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
strippedParseJSON = genericParseJSON stripLowerPrefix

strippedSchemaOptions :: SchemaOptions
strippedSchemaOptions = fromAesonOptions stripLowerPrefix

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module API.JsonOptions (
    stripLowerPrefix,
    strippedSchemaOptions,
    Stripped (..),
    parseClosed,
) where

import Data.Aeson (
    FromJSON (..),
    Options,
    ToJSON (..),
    Value,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
    rejectUnknownFields,
 )
import Data.Aeson.Types (GFromJSON, GToEncoding, GToJSON', Parser, Zero)
import Data.Char (isLower, toLower)
import Data.OpenApi.Internal.Schema (GToSchema)
import Data.OpenApi.Schema (SchemaOptions, ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Rep)

stripLowerPrefix :: Options
stripLowerPrefix =
    defaultOptions
        { fieldLabelModifier = stripPrefix
        }
  where
    -- Drop the lowercase Type-prefix (@fooBar -> bar@). An all-lowercase field
    -- has no prefix boundary, so keep it verbatim rather than emit an empty key.
    stripPrefix label = case dropWhile isLower label of
        "" -> label
        (c : cs) -> toLower c : cs

{- | 'Stripped''s reading, refusing a field the type does not have. For a
request whose absent field means something (a factor with no location, a
selector that does not filter on the flow name), where a misspelt one would be
read as absent and the request taken as another.
-}
parseClosed :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
parseClosed = genericParseJSON stripLowerPrefix{rejectUnknownFields = True}

strippedSchemaOptions :: SchemaOptions
strippedSchemaOptions = fromAesonOptions stripLowerPrefix

{- | DerivingVia carrier: strips the lowercase field-name prefix
(@fooBar -> bar@) and uses the @Generic@ instances. Usage:

@
  data Foo = Foo { fooBar :: Int } deriving Generic
  deriving via (Stripped Foo) instance ToJSON   Foo
  deriving via (Stripped Foo) instance FromJSON Foo
  deriving via (Stripped Foo) instance ToSchema Foo
@
-}
newtype Stripped a = Stripped {unStripped :: a}

instance (Generic a, GToJSON' Value Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (Stripped a) where
    toJSON (Stripped a) = genericToJSON stripLowerPrefix a
    toEncoding (Stripped a) = genericToEncoding stripLowerPrefix a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Stripped a) where
    parseJSON v = Stripped <$> genericParseJSON stripLowerPrefix v

instance (Typeable a, Generic a, GToSchema (Rep a)) => ToSchema (Stripped a) where
    declareNamedSchema _ = genericDeclareNamedSchema strippedSchemaOptions (Proxy :: Proxy a)

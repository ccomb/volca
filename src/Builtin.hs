{-# LANGUAGE OverloadedStrings #-}

{- | The reference tables and the method collections the binary carries, read
from @data/@ at build time by gen-version.sh. A configuration refers to one by
its name: to replace it with a file of its own, or to switch it off. The four
list tables have a name because a TOML array entry names them; the geographies
are a single top-level key and have none.
-}
module Builtin (
    BuiltinTable (..),
    builtinTables,
    builtinName,
    builtinContent,
    builtinGeographies,
    BuiltinMethod (..),
    builtinMethods,
    builtinMethodName,
    builtinMethodDescription,
    builtinMethodContent,
    DataVersion (..),
    builtinDataVersion,
) where

import qualified Builtin.Literals as L
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T

data BuiltinTable = BuiltinFlowSynonyms | BuiltinCompartments | BuiltinUnits | BuiltinEnergyDensities
    deriving (Eq, Ord, Show, Enum, Bounded)

builtinTables :: [BuiltinTable]
builtinTables = [minBound .. maxBound]

-- | The name a configuration uses to refer to the table.
builtinName :: BuiltinTable -> Text
builtinName BuiltinFlowSynonyms = "Default flow synonyms"
builtinName BuiltinCompartments = "Default compartment mapping"
builtinName BuiltinUnits = "Default units"
builtinName BuiltinEnergyDensities = "Default energy densities"

{- | Lenient because the literal is base64 the generator wrote from a file;
'BuiltinSpec' compares the result with that file, so a mangled literal fails
a test rather than a startup.
-}
builtinContent :: BuiltinTable -> BL.ByteString
builtinContent = decode . literal
  where
    literal :: BuiltinTable -> String
    literal BuiltinFlowSynonyms = L.flowsCsv
    literal BuiltinCompartments = L.compartmentsCsv
    literal BuiltinUnits = L.unitsCsv
    literal BuiltinEnergyDensities = L.energyDensityCsv

builtinGeographies :: BL.ByteString
builtinGeographies = decode L.geographiesCsv

-- | The method collections the binary carries, each a columnar method CSV.
data BuiltinMethod = BuiltinPlainIndicators
    deriving (Eq, Ord, Show, Enum, Bounded)

builtinMethods :: [BuiltinMethod]
builtinMethods = [minBound .. maxBound]

-- | The name a configuration uses to refer to the collection.
builtinMethodName :: BuiltinMethod -> Text
builtinMethodName BuiltinPlainIndicators = "plain-indicators"

builtinMethodDescription :: BuiltinMethod -> Text
builtinMethodDescription BuiltinPlainIndicators = "Raw physical quantities counted through the supply chain (CF=1.0)"

builtinMethodContent :: BuiltinMethod -> BL.ByteString
builtinMethodContent BuiltinPlainIndicators = decode L.plainIndicatorsCsv

decode :: String -> BL.ByteString
decode = BL.fromStrict . B64.decodeLenient . BC.pack

-- | The version of the built-in reference data, as @data/VERSION@ states it.
newtype DataVersion = DataVersion {unDataVersion :: Text}
    deriving (Show, Eq)

builtinDataVersion :: DataVersion
builtinDataVersion = DataVersion (T.pack L.dataVersion)

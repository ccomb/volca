{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

-- Import test modules (will be created)
import qualified MatrixConstructionSpec
import qualified InventorySpec
import qualified ParserSpec
import qualified MatrixExportSpec
import qualified MethodSpec
import qualified UnitConversionSpec
import qualified SimaProParserSpec
import qualified PluginSpec
import qualified ServerSpec
import qualified ILCDParserSpec
import qualified SupplyChainSpec
import qualified HotspotSpec

main :: IO ()
main = hspec $ do
    describe "VoLCA Test Suite" $ do
        describe "Matrix Construction" MatrixConstructionSpec.spec
        describe "Inventory Calculation" InventorySpec.spec
        describe "Parser" ParserSpec.spec
        describe "Matrix Export" MatrixExportSpec.spec
        describe "Method & SynonymDB" MethodSpec.spec
        describe "Unit Conversion" UnitConversionSpec.spec
        describe "SimaPro Parser" SimaProParserSpec.spec
        describe "Plugin System" PluginSpec.spec
        describe "Server Lifecycle" ServerSpec.spec
        describe "ILCD Parser" ILCDParserSpec.spec
        describe "Supply Chain" SupplyChainSpec.spec
        describe "Hotspot Analysis" HotspotSpec.spec

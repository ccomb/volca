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
import qualified TreeSpec
import qualified ServiceSpec
import qualified CrossLinkingSpec
import qualified CrossDBInventorySpec
import qualified SubstitutionSpec
import qualified CrossDBSubstitutionSpec
import qualified SharedSolverSpec
import qualified FlowResolverSpec
import qualified PluginConfigSpec
import qualified LoaderSpec
import qualified MappingSpec
import qualified UploadedDatabaseSpec
import qualified ProgressSpec
import qualified EcoSpold1Spec
import qualified MCPSchemaSpec
import qualified NormalizeSpec
import qualified BM25Spec
import qualified FuzzySpec
import qualified NestedSubstitutionSpec

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
        describe "Loop-Aware Tree" TreeSpec.spec
        describe "Service Layer" ServiceSpec.spec
        describe "Cross-Database Linking" CrossLinkingSpec.spec
        describe "Cross-DB Inventory" CrossDBInventorySpec.spec
        describe "Substitutions" SubstitutionSpec.spec
        describe "Cross-DB Substitutions" CrossDBSubstitutionSpec.spec
        describe "Shared Solver" SharedSolverSpec.spec
        describe "ILCD Flow Resolver" FlowResolverSpec.spec
        describe "Plugin Config" PluginConfigSpec.spec
        describe "Loader" LoaderSpec.spec
        describe "Method Mapping" MappingSpec.spec
        describe "Uploaded Database" UploadedDatabaseSpec.spec
        describe "Progress Formatting" ProgressSpec.spec
        describe "EcoSpold1 Parser" EcoSpold1Spec.spec
        describe "MCP Tool Schemas" MCPSchemaSpec.spec
        describe "Search.Normalize" NormalizeSpec.spec
        describe "Search.BM25" BM25Spec.spec
        describe "Search.Fuzzy" FuzzySpec.spec
        describe "Nested Substitutions" NestedSubstitutionSpec.spec

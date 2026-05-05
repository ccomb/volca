{-# LANGUAGE OverloadedStrings #-}

module PluginSpec (spec) where

import qualified Data.Map.Strict as M
import Data.UUID (nil)
import Test.Hspec

import Method.Types (FlowDirection (..), MethodCF (..))
import Plugin.Builtin
import Plugin.Config (PluginConfig (..), PluginType (..), buildRegistry)
import Plugin.Types
import SynonymDB (emptySynonymDB)
import Types (Database)

-- | A minimal MapContext for testing
emptyMapCtx :: MapContext
emptyMapCtx = MapContext M.empty M.empty M.empty emptySynonymDB M.empty

-- | A test CF with a known UUID
testCF :: MethodCF
testCF =
    MethodCF
        { mcfFlowRef = nil
        , mcfFlowName = "carbon dioxide"
        , mcfDirection = Output
        , mcfValue = 1.0
        , mcfCAS = Just "124-38-9"
        , mcfCompartment = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

spec :: Spec
spec = do
    describe "Mapper handles" $ do
        it "UUID mapper returns Nothing when flow not in context" $ do
            result <- mhMatch uuidMapper emptyMapCtx (MatchCF testCF)
            result `shouldBe` Nothing

        it "CAS mapper returns Nothing when CAS not in index" $ do
            result <- mhMatch casMapper emptyMapCtx (MatchCF testCF)
            result `shouldBe` Nothing

        it "Name mapper returns Nothing when name not in index" $ do
            result <- mhMatch nameMapper emptyMapCtx (MatchCF testCF)
            result `shouldBe` Nothing

        it "Synonym mapper returns Nothing when no synonym DB" $ do
            result <- mhMatch synonymMapper emptyMapCtx (MatchCF testCF)
            result `shouldBe` Nothing

        it "Mappers return Nothing for non-CF queries" $ do
            r1 <- mhMatch uuidMapper emptyMapCtx (MatchMaterial "steel" Nothing)
            r1 `shouldBe` Nothing
            r2 <- mhMatch casMapper emptyMapCtx (MatchSupplier "Acme" "US")
            r2 `shouldBe` Nothing

    describe "Mapper cascade ordering" $ do
        it "defaultMappers has 4 mappers in priority order" $ do
            length defaultMappers `shouldBe` 4
            map mhName defaultMappers `shouldBe` ["uuid-mapper", "cas-mapper", "name-mapper", "synonym-mapper"]

        it "priorities are strictly increasing" $ do
            let priorities = map mhPriority defaultMappers
            priorities `shouldBe` [0, 10, 20, 30]

    describe "Registry building" $ do
        it "buildRegistry [] produces default set" $ do
            let reg = buildRegistry []
            length (prMappers reg) `shouldBe` 4
            M.size (prReporters reg) `shouldBe` 4
            M.size (prExporters reg) `shouldBe` 2
            M.size (prAnalyzers reg) `shouldBe` 2 -- lcia + hotspot
            length (prSearchers reg) `shouldBe` 2 -- name + cas
            length (prImporters reg) `shouldBe` 4 -- ecospold2, ecospold1, simapro, ilcd
            length (prValidators reg) `shouldBe` 0
            length (prTransforms reg) `shouldBe` 0

        it "disable plugin removes it by name" $ do
            let configs =
                    [ PluginConfig
                        { pcName = "uuid-mapper"
                        , pcType = PTMapper
                        , pcPath = Nothing
                        , pcEnabled = False
                        , pcPriority = Nothing
                        , pcFormatId = Nothing
                        , pcPhase = Nothing
                        , pcMimeType = Nothing
                        }
                    ]
                reg = buildRegistry configs
            length (prMappers reg) `shouldBe` 3
            map mhName (prMappers reg) `shouldNotContain` ["uuid-mapper"]

        it "priority override re-orders mappers" $ do
            let configs =
                    [ PluginConfig
                        { pcName = "synonym-mapper"
                        , pcType = PTMapper
                        , pcPath = Nothing
                        , pcEnabled = True
                        , pcPriority = Just (-1) -- Move to front
                        , pcFormatId = Nothing
                        , pcPhase = Nothing
                        , pcMimeType = Nothing
                        }
                    ]
                reg = buildRegistry configs
            mhName (head (prMappers reg)) `shouldBe` "synonym-mapper"

    describe "Empty registry" $ do
        it "emptyRegistry has all empty collections" $ do
            let reg = emptyRegistry
            length (prImporters reg) `shouldBe` 0
            M.size (prExporters reg) `shouldBe` 0
            length (prSearchers reg) `shouldBe` 0
            length (prMappers reg) `shouldBe` 0
            length (prTransforms reg) `shouldBe` 0
            length (prValidators reg) `shouldBe` 0
            M.size (prAnalyzers reg) `shouldBe` 0
            M.size (prReporters reg) `shouldBe` 0

    describe "Validator functions" $ do
        it "runPreComputeValidation with empty list returns []" $ do
            let db = error "unused" :: Database
            issues <- runPreComputeValidation [] db
            issues `shouldBe` []

        it "hasValidationErrors distinguishes Error vs Warning" $ do
            let warning = ValidationIssue Warning "test" "warn" Nothing
                err = ValidationIssue Error "test" "err" Nothing
                info = ValidationIssue Info "test" "info" Nothing
            hasValidationErrors [warning, info] `shouldBe` False
            hasValidationErrors [warning, err] `shouldBe` True

    describe "Search merger" $ do
        it "searchWithPlugins merges results from multiple searchers" $ do
            let s1 = SearchHandle "s1" Builtin 0 (\_ _ -> pure [SearchResult nil "Flow A" 0.8 M.empty])
                s2 = SearchHandle "s2" Builtin 10 (\_ _ -> pure [SearchResult nil "Flow A" 0.9 M.empty])
                query = SearchQuery "test" M.empty 50
                db = error "unused" :: Database
            results <- searchWithPlugins [s1, s2] db query
            length results `shouldBe` 1 -- Deduplicated by UUID
            srScore (head results) `shouldBe` 0.9 -- Higher score kept

-- Re-export validation functions from Service for testing
runPreComputeValidation :: [ValidateHandle] -> Database -> IO [ValidationIssue]
runPreComputeValidation validators db = do
    let preValidators = filter ((== PreCompute) . vhPhase) validators
    concat <$> mapM (\v -> vhValidate v (ValidateContext db Nothing)) preValidators

hasValidationErrors :: [ValidationIssue] -> Bool
hasValidationErrors = any ((== Error) . viSeverity)

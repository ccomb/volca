{-# LANGUAGE OverloadedStrings #-}

module PluginConfigSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Plugin.Builtin (defaultRegistry)
import Plugin.Config (PluginConfig (..), PluginType (..), buildRegistry)
import Plugin.Types
import Test.Hspec

spec :: Spec
spec = do
    -- -------------------------------------------------------------------
    -- buildRegistry with no overrides = defaultRegistry
    -- -------------------------------------------------------------------
    describe "buildRegistry []" $ do
        it "returns the default mappers when no configs given" $
            map mhName (prMappers (buildRegistry []))
                `shouldBe` map mhName (prMappers defaultRegistry)

        it "returns the default searchers" $
            map shName (prSearchers (buildRegistry []))
                `shouldBe` map shName (prSearchers defaultRegistry)

    -- -------------------------------------------------------------------
    -- disablePlugin
    -- -------------------------------------------------------------------
    describe "disablePlugin (enabled = false)" $ do
        it "removes a mapper by name" $ do
            let cfg = disabledPlugin "uuid-mapper" PTMapper
                reg = buildRegistry [cfg]
            map mhName (prMappers reg) `shouldNotContain` ["uuid-mapper"]

        it "removes a searcher by name" $ do
            let cfg = disabledPlugin "name-searcher" PTSearcher
                reg = buildRegistry [cfg]
            map shName (prSearchers reg) `shouldNotContain` ["name-searcher"]

        it "removes an exporter by name" $ do
            let cfg = disabledPlugin "ecoinvent-matrix" PTExporter
                reg = buildRegistry [cfg]
            M.member "ecoinvent-matrix" (prExporters reg) `shouldBe` False

        it "removes an analyzer by name" $ do
            let cfg = disabledPlugin "lcia" PTAnalyzer
                reg = buildRegistry [cfg]
            M.member "lcia" (prAnalyzers reg) `shouldBe` False

        it "leaves other plugins untouched when disabling one mapper" $ do
            let cfg = disabledPlugin "uuid-mapper" PTMapper
                reg = buildRegistry [cfg]
            length (prMappers reg) `shouldBe` length (prMappers defaultRegistry) - 1

    -- -------------------------------------------------------------------
    -- priority override (no path = override built-in)
    -- -------------------------------------------------------------------
    describe "priority override" $ do
        it "overrides mapper priority by name" $ do
            let cfg = overridePriority "uuid-mapper" PTMapper 99
                reg = buildRegistry [cfg]
            case filter ((== "uuid-mapper") . mhName) (prMappers reg) of
                [m] -> mhPriority m `shouldBe` 99
                _ -> expectationFailure "uuid-mapper not found"

        it "overrides searcher priority by name" $ do
            let cfg = overridePriority "name-searcher" PTSearcher 77
                reg = buildRegistry [cfg]
            case filter ((== "name-searcher") . shName) (prSearchers reg) of
                [s] -> shPriority s `shouldBe` 77
                _ -> expectationFailure "name-searcher not found"

        it "overrides transform priority by name" $ do
            let cfg = overridePriority "my-transform" PTTransform 10
                reg = buildRegistry [cfg]
            -- No built-in transforms — override is a no-op, transforms stays empty
            length (prTransforms reg) `shouldBe` 0

    -- -------------------------------------------------------------------
    -- external plugin registration
    -- -------------------------------------------------------------------
    describe "external mapper" $ do
        it "adds external mapper to the registry" $ do
            let cfg = externalPlugin "my-mapper" PTMapper "/usr/bin/mapper" Nothing
                reg = buildRegistry [cfg]
            map mhName (prMappers reg) `shouldContain` ["my-mapper"]

        it "uses provided priority" $ do
            let cfg = externalPlugin "my-mapper" PTMapper "/usr/bin/mapper" (Just 42)
                reg = buildRegistry [cfg]
            case filter ((== "my-mapper") . mhName) (prMappers reg) of
                [m] -> mhPriority m `shouldBe` 42
                _ -> expectationFailure "my-mapper not found"

        it "defaults priority to 50 when not specified" $ do
            let cfg = externalPlugin "my-mapper" PTMapper "/usr/bin/mapper" Nothing
                reg = buildRegistry [cfg]
            case filter ((== "my-mapper") . mhName) (prMappers reg) of
                [m] -> mhPriority m `shouldBe` 50
                _ -> expectationFailure "my-mapper not found"

    describe "external reporter" $ do
        it "adds external reporter keyed by format-id" $ do
            let cfg =
                    (externalPlugin "my-reporter" PTReporter "/bin/report" Nothing)
                        { pcFormatId = Just "my-fmt"
                        , pcMimeType = Just "text/plain"
                        }
                reg = buildRegistry [cfg]
            M.member "my-fmt" (prReporters reg) `shouldBe` True

        it "falls back to plugin name as format-id when not specified" $ do
            let cfg = externalPlugin "my-reporter" PTReporter "/bin/report" Nothing
                reg = buildRegistry [cfg]
            M.member "my-reporter" (prReporters reg) `shouldBe` True

    describe "external exporter" $ do
        it "adds external exporter keyed by format-id" $ do
            let cfg =
                    (externalPlugin "my-exporter" PTExporter "/bin/export" Nothing)
                        { pcFormatId = Just "my-export-fmt"
                        }
                reg = buildRegistry [cfg]
            M.member "my-export-fmt" (prExporters reg) `shouldBe` True

    describe "external analyzer" $ do
        it "adds external analyzer keyed by name" $ do
            let cfg = externalPlugin "my-analyzer" PTAnalyzer "/bin/analyze" Nothing
                reg = buildRegistry [cfg]
            M.member "my-analyzer" (prAnalyzers reg) `shouldBe` True

    describe "external importer" $ do
        it "adds external importer to the registry" $ do
            let cfg = externalPlugin "my-importer" PTImporter "/bin/import" Nothing
                reg = buildRegistry [cfg]
            map ihName (prImporters reg) `shouldContain` ["my-importer"]

    describe "external searcher" $ do
        it "adds external searcher to the registry" $ do
            let cfg = externalPlugin "my-searcher" PTSearcher "/bin/search" Nothing
                reg = buildRegistry [cfg]
            map shName (prSearchers reg) `shouldContain` ["my-searcher"]

    -- -------------------------------------------------------------------
    -- external validator — phase parsing
    -- -------------------------------------------------------------------
    describe "external validator" $ do
        it "defaults to PreCompute when phase not specified" $ do
            let cfg = externalPlugin "my-validator" PTValidator "/bin/validate" Nothing
                reg = buildRegistry [cfg]
            case prValidators reg of
                [v] -> vhPhase v `shouldBe` PreCompute
                _ -> expectationFailure "expected one validator"

        it "uses PostCompute when phase = post-compute" $ do
            let cfg =
                    (externalPlugin "my-validator" PTValidator "/bin/validate" Nothing)
                        { pcPhase = Just "post-compute"
                        }
                reg = buildRegistry [cfg]
            case prValidators reg of
                [v] -> vhPhase v `shouldBe` PostCompute
                _ -> expectationFailure "expected one validator"

        it "uses PreCompute for any phase value other than post-compute" $ do
            let cfg =
                    (externalPlugin "my-validator" PTValidator "/bin/validate" Nothing)
                        { pcPhase = Just "pre-compute"
                        }
                reg = buildRegistry [cfg]
            case prValidators reg of
                [v] -> vhPhase v `shouldBe` PreCompute
                _ -> expectationFailure "expected one validator"

    -- -------------------------------------------------------------------
    -- sort order after buildRegistry
    -- -------------------------------------------------------------------
    describe "sort order" $ do
        it "mappers are sorted by priority after buildRegistry" $ do
            let cfgs =
                    [ externalPlugin "low" PTMapper "/bin/a" (Just 100)
                    , externalPlugin "high" PTMapper "/bin/b" (Just 1)
                    ]
                reg = buildRegistry cfgs
                priorities = map mhPriority (prMappers reg)
            priorities `shouldBe` foldr (\a acc -> if null acc || a <= head acc then a : acc else acc) [] priorities

        it "searchers are sorted by priority after buildRegistry" $ do
            let cfgs =
                    [ externalPlugin "s1" PTSearcher "/bin/s1" (Just 200)
                    , externalPlugin "s2" PTSearcher "/bin/s2" (Just 1)
                    ]
                reg = buildRegistry cfgs
                priorities = map shPriority (prSearchers reg)
            priorities `shouldBe` foldr (\a acc -> if null acc || a <= head acc then a : acc else acc) [] priorities

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

disabledPlugin :: Text -> PluginType -> PluginConfig
disabledPlugin name ptype =
    PluginConfig
        { pcName = name
        , pcType = ptype
        , pcPath = Nothing
        , pcEnabled = False
        , pcPriority = Nothing
        , pcFormatId = Nothing
        , pcPhase = Nothing
        , pcMimeType = Nothing
        }

overridePriority :: Text -> PluginType -> Int -> PluginConfig
overridePriority name ptype p =
    PluginConfig
        { pcName = name
        , pcType = ptype
        , pcPath = Nothing
        , pcEnabled = True
        , pcPriority = Just p
        , pcFormatId = Nothing
        , pcPhase = Nothing
        , pcMimeType = Nothing
        }

externalPlugin :: Text -> PluginType -> FilePath -> Maybe Int -> PluginConfig
externalPlugin name ptype path prio =
    PluginConfig
        { pcName = name
        , pcType = ptype
        , pcPath = Just path
        , pcEnabled = True
        , pcPriority = prio
        , pcFormatId = Nothing
        , pcPhase = Nothing
        , pcMimeType = Nothing
        }

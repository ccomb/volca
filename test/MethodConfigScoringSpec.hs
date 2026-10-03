{-# LANGUAGE OverloadedStrings #-}

-- | How a collection's configured scoring sets meet the ones read from its file.
module MethodConfigScoringSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Config (MethodConfig (MethodConfig), MethodOrigin (..), ScoringSetConfig (..))
import qualified Config
import Database.Manager (applyMethodConfig, namedOnce)
import Method.Types

configured :: Text -> ScoringSetConfig
configured name = ScoringSetConfig name "Pt" M.empty M.empty M.empty M.empty M.empty M.empty Nothing

fromFile :: Text -> ScoringSet
fromFile name = ScoringSet name "Pt" M.empty M.empty M.empty M.empty M.empty M.empty Nothing M.empty ReadFromSimaProFile

config :: [ScoringSetConfig] -> MethodConfig
config sets =
    MethodConfig
        { Config.mcName = "test"
        , Config.mcOrigin = MethodFromFile "unused.csv"
        , Config.mcActive = True
        , Config.mcHome = Nothing
        , Config.mcSource = Nothing
        , Config.mcDescription = Nothing
        , Config.mcFormat = Nothing
        , Config.mcScoringSets = sets
        , Config.mcGlobalMethods = []
        , Config.mcPatches = []
        }

spec :: Spec
spec = describe "applyMethodConfig" $ do
    it "keeps the file's scoring sets and adds the configured ones after them" $
        fmap (map (\s -> (ssName s, ssOrigin s)) . mcScoringSets . fst) (applyMethodConfig (config [configured "Mine"]) (MethodCollection [] [fromFile "EF"] []))
            `shouldBe` Right [("EF", ReadFromSimaProFile), ("Mine", DeclaredInConfig)]

    it "refuses two sets of one name read from the method files" $
        case namedOnce (MethodCollection [] [fromFile "EF", fromFile "EF"] []) of
            Left err -> err `shouldSatisfy` T.isInfixOf "scoring set 'EF' is read twice from the method files"
            Right _ -> expectationFailure "expected the duplicate to be refused"

    it "refuses a configured set named like one read from the file" $
        case applyMethodConfig (config [configured "EF"]) (MethodCollection [] [fromFile "EF"] []) of
            Left err -> err `shouldSatisfy` T.isInfixOf "scoring set 'EF' is declared in the configuration and also read from the method file; rename the configured one"
            Right _ -> expectationFailure "expected the clash to be refused"

    it "carries the configuration's global-methods into the collection" $
        fmap (mcUnregionalized . fst) (applyMethodConfig (config []){Config.mcGlobalMethods = ["Land use"]} (MethodCollection [] [] []))
            `shouldBe` Right ["Land use"]

{-# LANGUAGE OverloadedStrings #-}

module DependencyChoiceSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Config (DatabaseConfig (..))
import Database.CrossLinking (IndexedDatabase (..))
import Database.Manager (
    DependencyChoice (..),
    DependencyStatus (..),
    buildDependencyChoices,
 )
import Types (AllocationKey (..), GeographyPolicy (..))

cfg :: Text -> Text -> DatabaseConfig
cfg name display =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = display
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        }

-- IndexedDatabase whose idbByProductName has 'n' distinct dummy keys, so
-- M.size returns 'n' (only field buildDependencyChoices reads from it).
indexedWith :: Text -> Int -> IndexedDatabase
indexedWith name n =
    IndexedDatabase
        { idbName = name
        , idbByProductName = M.fromList [(name <> "-" <> k, []) | k <- take n keys]
        , idbBySynonymGroup = M.empty
        , idbWasteTreatmentByFlowUUID = M.empty
        , idbWasteTreatmentByCanonicalName = M.empty
        , idbByActivityAndProductName = M.empty
        , idbByActivityProduct = M.empty
        }
  where
    keys :: [Text]
    keys = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]

configs :: Map Text DatabaseConfig
configs =
    M.fromList
        [ ("agribalyse", cfg "agribalyse" "Agribalyse 3.2")
        , ("ecoinvent", cfg "ecoinvent" "Ecoinvent 3.9")
        , ("wfldb", cfg "wfldb" "WFLDB")
        ]

indexed :: Map Text IndexedDatabase
indexed =
    M.fromList
        [ ("agribalyse", indexedWith "agribalyse" 3)
        , ("ecoinvent", indexedWith "ecoinvent" 7)
        , ("wfldb", indexedWith "wfldb" 2)
        ]

spec :: Spec
spec = do
    describe "buildDependencyChoices" $ do
        it "excludes the current database from the result" $ do
            let result = buildDependencyChoices "agribalyse" [] [] configs indexed
            map dchDatabaseName result `shouldBe` ["ecoinvent", "wfldb"]

        it "sorts results alphabetically by database name" $ do
            let result = buildDependencyChoices "wfldb" [] [] configs indexed
            map dchDatabaseName result `shouldBe` ["agribalyse", "ecoinvent"]

        it "tags entries selected vs available based on the selected set" $ do
            let result = buildDependencyChoices "agribalyse" ["ecoinvent"] [] configs indexed
            [(dchDatabaseName d, dchStatus d) | d <- result]
                `shouldBe` [("ecoinvent", SelectedDep), ("wfldb", AvailableDep)]

        it "tags entries in the redundant set as RedundantDep" $ do
            let result = buildDependencyChoices "agribalyse" ["ecoinvent"] ["wfldb"] configs indexed
            [(dchDatabaseName d, dchStatus d) | d <- result]
                `shouldBe` [("ecoinvent", SelectedDep), ("wfldb", RedundantDep)]

        it "selected wins over redundant when a name appears in both sets" $ do
            let result = buildDependencyChoices "agribalyse" ["wfldb"] ["wfldb"] configs indexed
            [(dchDatabaseName d, dchStatus d) | d <- result]
                `shouldBe` [("ecoinvent", AvailableDep), ("wfldb", SelectedDep)]

        it "flips status in place without changing order when an entry is toggled" $ do
            let before = buildDependencyChoices "agribalyse" [] [] configs indexed
                after = buildDependencyChoices "agribalyse" ["wfldb"] [] configs indexed
            map dchDatabaseName before `shouldBe` map dchDatabaseName after
            map dchStatus after `shouldBe` [AvailableDep, SelectedDep]

        it "populates matchCount from idbByProductName size" $ do
            let result = buildDependencyChoices "agribalyse" [] [] configs indexed
                byName = M.fromList [(dchDatabaseName d, dchMatchCount d) | d <- result]
            M.lookup "ecoinvent" byName `shouldBe` Just 7
            M.lookup "wfldb" byName `shouldBe` Just 2

        it "falls back to the database name when no config is present" $ do
            let result = buildDependencyChoices "agribalyse" [] [] M.empty indexed
                byName = M.fromList [(dchDatabaseName d, dchDisplayName d) | d <- result]
            M.lookup "ecoinvent" byName `shouldBe` Just "ecoinvent"
            M.lookup "wfldb" byName `shouldBe` Just "wfldb"

        it "uses dcDisplayName from the config when available" $ do
            let result = buildDependencyChoices "agribalyse" [] [] configs indexed
                byName = M.fromList [(dchDatabaseName d, dchDisplayName d) | d <- result]
            M.lookup "ecoinvent" byName `shouldBe` Just "Ecoinvent 3.9"
            M.lookup "wfldb" byName `shouldBe` Just "WFLDB"

        it "ignores selected names that are not in the indexed set" $ do
            let result = buildDependencyChoices "agribalyse" ["ghost-db"] [] configs indexed
            map dchStatus result `shouldBe` [AvailableDep, AvailableDep]

{-# LANGUAGE OverloadedStrings #-}

module RequirementsSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Config (DatabaseConfig (..))
import Database.Requirements
import Types (AllocationKey (..), GeographyPolicy (..), Licence (..), Release (..), Requirement (..))

ei :: Text -> Release
ei version = Release{releaseName = "ecoinvent", releaseVersion = version, releaseSystemModel = Just "Allocation, cut-off by classification"}

config :: Text -> Maybe Release -> [Requirement] -> DatabaseConfig
config name release requires =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = True
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = release
        , dcRequires = requires
        }

-- | A model built on ecoinvent 3.12, beside the databases a reader may hold.
bread :: Maybe Text -> DatabaseConfig
bread substitute = config "bread" Nothing [Requirement (ei "3.12") substitute]

engine :: [DatabaseConfig] -> M.Map Text DatabaseConfig
engine cs = M.fromList [(dcName c, c) | c <- cs]

held :: [DatabaseConfig]
held =
    [ config "ei-312" (Just (ei "3.12")) []
    , -- Spelt the way someone typed it: still the same release.
      config "ei-312-copy" (Just (Release "EcoInvent" " 3.12" (Just "allocation,  cut-off by classification"))) []
    , config "ei-311" (Just (ei "3.11")) []
    , config "agb" Nothing []
    ]

spec :: Spec
spec = do
    describe "admits" $ do
        it "lets a database that requires nothing link to any other" $
            all (admits (engine held) (config "plain" Nothing [])) ["ei-312", "ei-311", "agb"] `shouldBe` True

        it "lets a model link only to databases of the release it requires" $
            filter (admits (engine held) (bread Nothing)) (map dcName held) `shouldBe` ["ei-312", "ei-312-copy"]

        it "lets an accepted substitute stand in for the release, and nothing else" $
            filter (admits (engine held) (bread (Just "ei-311"))) (map dcName held) `shouldBe` ["ei-311"]

    describe "requiredReleases" $ do
        it "names every database of the required release" $
            requiredReleases (engine (bread Nothing : held)) (bread Nothing)
                `shouldBe` [RequiredRelease (ei "3.12") Satisfied ["ei-312", "ei-312-copy"]]

        it "offers the other versions of the same name when the release is missing" $
            requiredReleases (engine (bread Nothing : drop 2 held)) (bread Nothing)
                `shouldBe` [RequiredRelease (ei "3.12") Missing ["ei-311"]]

        it "names the substitute its reader accepted" $
            requiredReleases (engine (bread (Just "ei-311") : held)) (bread (Just "ei-311"))
                `shouldBe` [RequiredRelease (ei "3.12") Substituted ["ei-311"]]

        it "reads a substitute since deleted as the release missing again" $
            requiredReleases (engine (bread (Just "ei-310") : drop 2 held)) (bread (Just "ei-310"))
                `shouldBe` [RequiredRelease (ei "3.12") Missing ["ei-311"]]

    describe "acceptSubstitute" $ do
        it "records the database accepted in place of the release" $
            acceptSubstitute (ei "3.12") "ei-311" (bread Nothing) `shouldBe` Right [Requirement (ei "3.12") (Just "ei-311")]

        it "refuses a release the database does not require" $
            acceptSubstitute (ei "3.11") "ei-311" (bread Nothing)
                `shouldBe` Left "bread does not require ecoinvent 3.11 (Allocation, cut-off by classification)."

        it "refuses the database itself" $
            acceptSubstitute (ei "3.12") "bread" (bread Nothing) `shouldBe` Left "bread cannot stand in for a release it requires itself."

{-# LANGUAGE OverloadedStrings #-}

module CrateSpec (spec) where

import Codec.Archive.Zip (filesInArchive, findEntryByPath, fromEntry, toArchive)
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.Aeson (Value (..), decode, encode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Test.Hspec

import Config (DatabaseConfig (..))
import Database.Crate
import Database.Upload (DatabaseFormat (..))
import Types (AllocationKey (..), Attribution (..), GeographyPolicy (..), Licence (..), OwnLicence (..), Permission (..), Release (..), StandardLicence (..))
import Zip (zipFiles)

ecoinvent :: Release
ecoinvent = Release{releaseName = "ecoinvent", releaseVersion = "3.12", releaseSystemModel = Just "Allocation, cut-off by classification"}

input :: Licence -> CrateInput
input licence =
    CrateInput
        { ciName = "Bread"
        , ciDescription = Just "A loaf, baked in France"
        , ciPublished = fromGregorian 2026 10 4
        , ciLicence = licence
        , ciRequires = [ecoinvent]
        , ciFormat = EcoSpold2
        }

payload :: BL.ByteString
payload = "the export, byte for byte"

crate :: Licence -> Value
crate licence = crateMetadata "bread" (input licence) payload

-- | The entity of the graph with this @\@id@, when there is exactly one.
entity :: Text -> Value -> Maybe Value
entity target (Object o) = case KM.lookup "@graph" o of
    Just (Array graph) -> case [e | e@(Object fields) <- foldr (:) [] graph, KM.lookup "@id" fields == Just (String target)] of
        [e] -> Just e
        _ -> Nothing
    _ -> Nothing
entity _ _ = Nothing

field :: Text -> Value -> Maybe Value
field key (Object o) = KM.lookup (K.fromText key) o
field _ _ = Nothing

config :: Text -> Maybe Release -> DatabaseConfig
config name release =
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
        , dcRequires = []
        }

spec :: Spec
spec = do
    describe "crateMetadata" $ do
        it "points the descriptor at the root, as RO-Crate requires" $
            (entity "ro-crate-metadata.json" (crate LicenceUnstated) >>= field "about")
                `shouldBe` Just (toJSON (KM.singleton "@id" (String "./")))

        it "names the export it carries, with its digest" $ do
            let file = entity "payload/bread.zip" (crate LicenceUnstated)
            (file >>= field "sha256") `shouldBe` Just (String (showDigest payload))
            (file >>= field "encodingFormat") `shouldBe` Just (String "application/zip")

        it "names each release it was built on, system model and all" $ do
            let release = entity "#release-0" (crate LicenceUnstated)
            (release >>= field "name") `shouldBe` Just (String "ecoinvent")
            (release >>= field "version") `shouldBe` Just (String "3.12")
            (release >>= field "additionalProperty") `shouldBe` Just (toJSON (KM.singleton "@id" (String "#release-0-system-model")))
            (entity "#release-0-system-model" (crate LicenceUnstated) >>= field "value") `shouldBe` Just (String "Allocation, cut-off by classification")

        it "points a standard licence at its SPDX page" $
            (entity "./" (crate (LicenceStandard CCBY)) >>= field "license")
                `shouldBe` Just (toJSON (KM.singleton "@id" (String "https://spdx.org/licenses/CC-BY-4.0")))

        it "carries an own licence's text and what it refuses" $ do
            let own = LicenceOwn OwnLicence{ownText = "Members only", ownRefused = S.fromList [Download], ownAttribution = AttributionRequired}
                licence = entity "#licence" (crate own)
            (licence >>= field "description") `shouldBe` Just (String "Members only")
            (entity "#licence-refuses" (crate own) >>= field "value") `shouldBe` Just (toJSON ["download" :: Text])
            (entity "#licence-attribution" (crate own) >>= field "value") `shouldBe` Just (Bool True)

        it "states no licence rather than claim one" $
            (entity "./" (crate LicenceUnstated) >>= field "license") `shouldBe` Nothing

    describe "requiredReleases" $ do
        it "reads the release of every dependency" $
            requiredReleases "bread" (M.fromList [("ei", config "ei" (Just ecoinvent))]) ["ei"] `shouldBe` Right [ecoinvent]

        it "refuses a dependency with no release, naming each one" $
            requiredReleases "bread" (M.fromList [("ei", config "ei" Nothing), ("agb", config "agb" Nothing)]) ["ei", "agb"]
                `shouldBe` Left "Declare the release of ei, agb before packaging bread: the package names the published databases it links to, so a reader's engine can tell whether it holds them."

    describe "packageExport" $ do
        it "holds the export unchanged beside its description" $ do
            let archive = toArchive (packageExport "bread" (input LicenceUnstated) payload)
            filesInArchive archive `shouldMatchList` ["ro-crate-metadata.json", "payload/bread.zip"]
            fromEntry <$> findEntryByPath "payload/bread.zip" archive `shouldBe` Just payload
            (decode . fromEntry =<< findEntryByPath "ro-crate-metadata.json" archive) `shouldBe` Just (crate LicenceUnstated)

    describe "openPackage" $ do
        let packaged licence = packageExport "bread" (input licence) payload
            opened = fmap (\p -> (pkPayload p, pkLicence p, pkRequires p)) <$> openPackage (packaged own)
            own = LicenceOwn OwnLicence{ownText = "Members only", ownRefused = S.fromList [Resell], ownAttribution = AttributionNotRequired}
        it "reads back the export, its licence and the releases it requires" $
            opened `shouldBe` Right (Just (payload, own, [ecoinvent]))

        it "reads a standard licence back from its SPDX page" $
            fmap pkLicence <$> openPackage (packaged (LicenceStandard CCBY)) `shouldBe` Right (Just (LicenceStandard CCBY))

        it "refuses an export changed after it was packaged" $ do
            let tampered = zipFiles [("ro-crate-metadata.json", BL.toStrict (encode (crate LicenceUnstated))), ("payload/bread.zip", "another export")]
            fmap pkPayload <$> openPackage tampered `shouldBe` Left "The package's payload/bread.zip does not match the digest its description gives: it was changed after it was packaged."

        it "reads a zip without a description as no package" $
            fmap pkPayload <$> openPackage (zipFiles [("data.csv", "a,b")]) `shouldBe` Right Nothing

        it "reads bytes that are no zip as no package" $
            fmap pkPayload <$> openPackage "<?xml version=\"1.0\"?>" `shouldBe` Right Nothing

    describe "parsePackaging" $ do
        it "reads an absent package as the export alone" $ parsePackaging Nothing `shouldBe` Right Plain
        it "reads ro-crate" $ parsePackaging (Just "RO-Crate") `shouldBe` Right RoCrate
        it "refuses a package it does not know" $ parsePackaging (Just "bagit") `shouldBe` Left "unknown package: bagit (expected ro-crate)"
  where
    showDigest :: BL.ByteString -> Text
    showDigest bytes = T.pack (show (hashlazy bytes :: Digest SHA256))

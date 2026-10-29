{-# LANGUAGE OverloadedStrings #-}

module CrateSpec (spec) where

import Codec.Archive.Zip (filesInArchive, findEntryByPath, fromEntry, toArchive)
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.Aeson (Value (..), decode, encode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (fromGregorian)
import qualified Data.UUID as UUID
import Test.Hspec

import Config (DatabaseConfig (..))
import Database.Crate
import Database.SystemModel (Rule (..), Traced (..), parseSystemModel)
import Database.Upload (DatabaseFormat (..))
import Types (Acceptance (..), AllocationProperty (..), Attribution (..), Disclosure (..), FileReading (..), GeographyPolicy (..), Licence (..), OwnLicence (..), Permission (..), Reading (..), Reason (..), Release (..), StandardLicence (..), UsageCount (..))
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
        , ciChanges = Nothing
        , ciMadeFrom = ReadFrom sources
        }

sources :: Identity
sources = Identity{idFiles = "f11e5", idPatches = Nothing, idJournal = Just "j0a1", idReading = "none"}

modelText :: Text
modelText = "# Cheese apart\nname = \"Dairy\"\nversion = \"1.0.0\"\n[allocation]\ndefault = \"price\"\n"

-- | A child of the bread's own sources, one piece of a dairy block traced.
division :: Division
division =
    Division
        { dvParent = "Bread parent"
        , dvParentIdentity = sources
        , dvJournal = Nothing
        , dvModel = either (error . T.unpack) id (parseSystemModel modelText)
        , dvEngine = "0.16.1"
        , dvMade = Nothing
        , dvTrace = [Traced{trProcess = (UUID.nil, UUID.nil), trParent = (UUID.nil, UUID.nil), trActivity = "Dairy", trProduct = "Whey, \"sweet\"", trShare = Just 23.4, trRule = Divided WetMass (Just (Reason "Dairies weigh their milk.")) [], trParameters = []}]
        }

-- | Every @\@id@ an entity of the graph refers to, the entities' own left out.
references :: Value -> [Text]
references (Object o) = case KM.lookup "@graph" o of
    Just (Array graph) -> concat [concatMap inside (KM.elems (KM.delete "@id" fields)) | Object fields <- foldr (:) [] graph]
    _ -> []
  where
    inside :: Value -> [Text]
    inside (Object r) | Just (String target) <- KM.lookup "@id" r, KM.size r == 1 = [target]
    inside (Array vs) = concatMap inside (foldr (:) [] vs)
    inside _ = []
references _ = []

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

text :: Value -> Maybe Text
text (String t) = Just t
text _ = Nothing

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
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = release
        , dcRequires = []
        , dcAdded = Nothing
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
            let own = LicenceOwn OwnLicence{ownText = "Members only", ownRefused = S.fromList [Download], ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked (Counted ReadersAnonymous)}
                licence = entity "#licence" (crate own)
            (licence >>= field "description") `shouldBe` Just (String "Members only")
            (entity "#licence-refuses" (crate own) >>= field "value") `shouldBe` Just (toJSON ["download" :: Text])
            (entity "#licence-attribution" (crate own) >>= field "value") `shouldBe` Just (Bool True)
            (entity "#licence-accept" (crate own) >>= field "value") `shouldBe` Just (Bool True)
            (entity "#licence-usage" (crate own) >>= field "value") `shouldBe` Just (String "anonymous")

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

        -- An engine that reads exactly one file in the package must keep reading an unchanged database's.
        it "adds no file beside an unchanged database's export, whatever its licence" $
            filesInArchive (toArchive (packageExport "bread" (input (LicenceStandard CCBY)) payload))
                `shouldMatchList` ["ro-crate-metadata.json", "payload/bread.zip"]

    describe "a modified database's package" $ do
        let own = LicenceOwn OwnLicence{ownText = "Members only", ownRefused = S.fromList [Resell], ownAttribution = AttributionRequired, ownAcceptance = AcceptanceAsked NotCounted}
            original = Release{releaseName = "Bakery data", releaseVersion = "2025", releaseSystemModel = Nothing}
            modified = (input own){ciChanges = Just Changes{chReport = "# Changes to Bread\n", chOriginal = "bakery", chOriginalRelease = Just original}}
            packaged = packageExport "bread" modified payload
            metadata = crateMetadata "bread" modified payload
        it "carries the changes report and the licence in plain words beside the export" $ do
            let archive = toArchive packaged
            filesInArchive archive `shouldMatchList` ["ro-crate-metadata.json", "payload/bread.zip", "CHANGES.md", "LICENCE.txt"]
            fromEntry <$> findEntryByPath "CHANGES.md" archive `shouldBe` Just "# Changes to Bread\n"
            fromEntry <$> findEntryByPath "LICENCE.txt" archive `shouldBe` Just "Members only"

        it "cites the original it was made from, and says it is not the original" $ do
            (entity "./" metadata >>= field "citation") `shouldBe` Just (toJSON (KM.singleton "@id" (String "#original")))
            (entity "#original" metadata >>= field "version") `shouldBe` Just (String "2025")
            fmap (T.isInfixOf "not endorsed by the publisher of the original") (entity "./" metadata >>= field "description" >>= text)
                `shouldBe` Just True

        it "reads back as its export alone, requiring no more than it did" $
            fmap (\p -> (pkPayload p, pkLicence p, pkRequires p)) <$> openPackage packaged `shouldBe` Right (Just (payload, own, [ecoinvent]))

    describe "a child's package" $ do
        let modified = (input LicenceUnstated){ciChanges = Just Changes{chReport = "# Changes to Bread\n", chOriginal = "bakery", chOriginalRelease = Nothing}}
            child = modified{ciMadeFrom = DividedFrom division}
            packaged = packageExport "bread" child payload
            metadata = crateMetadata "bread" child payload
            parentMetadata = crateMetadata "bread-parent" modified payload
        it "carries the model as its author wrote it, and the table of each piece" $ do
            let archive = toArchive packaged
            filesInArchive archive `shouldMatchList` ["ro-crate-metadata.json", "payload/bread.zip", "CHANGES.md", "system-model.toml", "allocation.csv"]
            fromEntry <$> findEntryByPath "system-model.toml" archive `shouldBe` Just (BL.fromStrict (T.encodeUtf8 modelText))
            fromEntry <$> findEntryByPath "allocation.csv" archive
                `shouldBe` Just
                    ( "process_id,parent_process_id,activity,product,share_percent,rule\r\n"
                        <> "00000000-0000-0000-0000-000000000000_00000000-0000-0000-0000-000000000000,00000000-0000-0000-0000-000000000000_00000000-0000-0000-0000-000000000000,Dairy,\"Whey, \"\"sweet\"\"\",23.4,\"wet mass, exception: Dairies weigh their milk.\"\r\n"
                    )

        it "names the parent by the same digests as the parent's own package" $ do
            (entity "#parent-files-sha256" metadata >>= field "value") `shouldBe` Just (String "f11e5")
            (entity "#parent-journal-sha256" metadata >>= field "value") `shouldBe` Just (String "j0a1")
            (entity "#files-sha256" parentMetadata >>= field "value") `shouldBe` Just (String "f11e5")
            (entity "#journal-sha256" parentMetadata >>= field "value") `shouldBe` Just (String "j0a1")

        it "describes its division as a Process Run Crate action of the engine on the parent and the model" $ do
            (entity "./" metadata >>= field "conformsTo") `shouldBe` Just (toJSON (KM.singleton "@id" (String "https://w3id.org/ro/wfrun/process/0.5")))
            (entity "#division" metadata >>= field "instrument") `shouldBe` Just (toJSON (KM.singleton "@id" (String "#engine")))
            (entity "#division" metadata >>= field "object") `shouldBe` Just (toJSON [KM.singleton "@id" (String "#parent"), KM.singleton "@id" (String "system-model.toml")])
            (entity "#engine" metadata >>= field "softwareVersion") `shouldBe` Just (String "0.16.1")

        it "refers only to entities its graph holds" $
            filter (\target -> isNothing (entity target metadata) && not (T.isPrefixOf "https://" target)) (references metadata) `shouldBe` []

        it "reads back as its export alone" $
            fmap pkPayload <$> openPackage packaged `shouldBe` Right (Just payload)

    describe "openPackage" $ do
        let packaged licence = packageExport "bread" (input licence) payload
            opened = fmap (\p -> (pkPayload p, pkLicence p, pkRequires p)) <$> openPackage (packaged own)
            own = LicenceOwn OwnLicence{ownText = "Members only", ownRefused = S.fromList [Resell], ownAttribution = AttributionNotRequired, ownAcceptance = AcceptanceAsked (Counted ReadersNamed)}
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

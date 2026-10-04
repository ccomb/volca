{-# LANGUAGE OverloadedStrings #-}

{- | A database exported as a package: the export itself, unchanged, beside an
RO-Crate description of it (<https://w3id.org/ro/crate/1.2>). The description
says what a reader needs before loading it: its licence, and the published
releases it was built on, so a reader's engine can tell whether it holds them.

Only schema.org terms are used, plus the RO-Crate @sha256@ term, declared in the
context itself so a reader does not depend on the published context carrying it.
-}
module Database.Crate (
    CrateInput (..),
    Packaging (..),
    parsePackaging,
    requiredReleases,
    payloadPath,
    crateMetadata,
    packageExport,
) where

import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)

import Config (DatabaseConfig (..))
import Database.Upload (DatabaseFormat (..), formatDisplayText)
import Types (Attribution (..), Licence (..), OwnLicence (..), Release (..), StandardLicence, StandardTerms (..), permissionCode, spdxId, standardTerms)
import Zip (zipFiles)

-- | How an export is handed over: the bytes alone, or packaged with their description.
data Packaging = Plain | RoCrate
    deriving (Show, Eq)

-- | Read the @package@ an export request names; absent means the bytes alone.
parsePackaging :: Maybe Text -> Either Text Packaging
parsePackaging = maybe (Right Plain) (named . T.toLower . T.strip)
  where
    named :: Text -> Either Text Packaging
    named "ro-crate" = Right RoCrate
    named other = Left ("unknown package: " <> other <> " (expected ro-crate)")

{- | The releases of the databases a package links to. A dependency with no
release declared refuses the package, every such one named: a package that
cannot say what it was built on leaves its reader nothing to check.
-}
requiredReleases :: Text -> M.Map Text DatabaseConfig -> [Text] -> Either Text [Release]
requiredReleases name configs deps = case partitionEithers (map releaseOf deps) of
    ([], releases) -> Right releases
    (undeclared, _) ->
        Left
            ( "Declare the release of "
                <> T.intercalate ", " undeclared
                <> " before packaging "
                <> name
                <> ": the package names the published databases it links to, so a reader's engine can tell whether it holds them."
            )
  where
    releaseOf :: Text -> Either Text Release
    releaseOf dep = maybe (Left dep) Right (dcRelease =<< M.lookup dep configs)

-- | What the description of a packaged database says.
data CrateInput = CrateInput
    { ciName :: !Text
    , ciDescription :: !(Maybe Text)
    , ciPublished :: !Day
    , ciLicence :: !Licence
    , ciRequires :: ![Release]
    -- ^ The releases of the databases its exchanges link to
    , ciFormat :: !DatabaseFormat
    }

{- | Where the export sits in the package, named after the database, with the
extension and media type of what the format writes.
-}
payloadPath :: Text -> DatabaseFormat -> (FilePath, Text)
payloadPath name format = ("payload/" <> T.unpack name <> extension, mediaType)
  where
    (extension, mediaType) = case format of
        EcoSpold2 -> (".zip", "application/zip")
        ILCDProcess -> (".zip", "application/zip")
        EcoSpold1 -> (".xml", "application/xml")
        SimaProCSV -> (".csv", "text/csv")
        BrightwayExcel -> (".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        OpenLcaJsonLd -> (".json", "application/ld+json")
        UnknownFormat -> ("", "application/octet-stream")

-- | The package: the export at its 'payloadPath', and the description of it.
packageExport :: Text -> CrateInput -> BL.ByteString -> BL.ByteString
packageExport name input payload =
    zipFiles
        [ ("ro-crate-metadata.json", BL.toStrict (encodePretty (crateMetadata name input payload)))
        , (path, BL.toStrict payload)
        ]
  where
    path :: FilePath
    path = fst (payloadPath name (ciFormat input))

{- | The @ro-crate-metadata.json@ describing a packaged export. The graph is
flat, as RO-Crate requires: every property value that is an object is an
entity of its own, referenced by its @\@id@.
-}
crateMetadata :: Text -> CrateInput -> BL.ByteString -> Value
crateMetadata name input payload =
    object
        [ "@context" .= [A.String "https://w3id.org/ro/crate/1.2/context", object ["sha256" .= ("https://w3id.org/ro/terms#sha256" :: Text)]]
        , "@graph" .= (descriptor : root : file : licenceEntities (ciName input) (ciLicence input) <> concat (zipWith releaseEntities [0 ..] (ciRequires input)))
        ]
  where
    (path, mediaType) = payloadPath name (ciFormat input)

    descriptor :: Value
    descriptor =
        object
            [ "@id" .= ("ro-crate-metadata.json" :: Text)
            , "@type" .= ("CreativeWork" :: Text)
            , "conformsTo" .= ref "https://w3id.org/ro/crate/1.2"
            , "about" .= ref "./"
            ]

    root :: Value
    root =
        object $
            [ "@id" .= ("./" :: Text)
            , "@type" .= ("Dataset" :: Text)
            , "name" .= ciName input
            , "description" .= fromMaybe (ciName input <> ", packaged with the licence it is published under and the releases it was built on.") (ciDescription input)
            , "datePublished" .= iso8601Show (ciPublished input)
            , "hasPart" .= refs [T.pack path]
            ]
                <> ["isBasedOn" .= refs (map releaseId [0 .. length (ciRequires input) - 1]) | not (null (ciRequires input))]
                <> maybe [] (\l -> ["license" .= ref l]) (licenceId (ciLicence input))

    file :: Value
    file =
        object
            [ "@id" .= path
            , "@type" .= ("File" :: Text)
            , "name" .= (ciName input <> ", " <> formatDisplayText (ciFormat input))
            , "description" .= ("The " <> formatDisplayText (ciFormat input) <> " export of " <> ciName input <> ", unchanged." :: Text)
            , "encodingFormat" .= mediaType
            , "contentSize" .= show (BL.length payload)
            , "sha256" .= show (hashlazy payload :: Digest SHA256)
            ]

ref :: Text -> Value
ref target = object ["@id" .= target]

-- | References written as RO-Crate 1.2 recommends: one as a value, several as a list.
refs :: [Text] -> Value
refs [one] = ref one
refs several = A.toJSON (map ref several)

releaseId :: Int -> Text
releaseId n = "#release-" <> T.pack (show n)

-- | One release a package requires, as the published dataset it names, and its system model.
releaseEntities :: Int -> Release -> [Value]
releaseEntities n release =
    object
        ( [ "@id" .= releaseId n
          , "@type" .= ("Dataset" :: Text)
          , "name" .= releaseName release
          , "version" .= releaseVersion release
          ]
            <> ["additionalProperty" .= ref modelId | Just _ <- [releaseSystemModel release]]
        )
        : [property modelId "system model" (A.String model) | Just model <- [releaseSystemModel release]]
  where
    modelId :: Text
    modelId = releaseId n <> "-system-model"

property :: Text -> Text -> Value -> Value
property propertyId name value = object ["@id" .= propertyId, "@type" .= ("PropertyValue" :: Text), "name" .= name, "value" .= value]

-- | Where the root's @license@ points, when a licence is stated.
licenceId :: Licence -> Maybe Text
licenceId LicenceUnstated = Nothing
licenceId (LicenceStandard l) = Just (spdxUrl l)
licenceId (LicenceOwn _) = Just ownLicenceId

spdxUrl :: StandardLicence -> Text
spdxUrl l = "https://spdx.org/licenses/" <> spdxId l

ownLicenceId :: Text
ownLicenceId = "#licence"

{- | The licence as an entity of its own. An own licence carries what it refuses
and whether results must name the publisher, so a reader's engine applies the
same terms the publisher's did.
-}
licenceEntities :: Text -> Licence -> [Value]
licenceEntities _ LicenceUnstated = []
licenceEntities _ (LicenceStandard l) =
    [ object
        [ "@id" .= spdxUrl l
        , "@type" .= ("CreativeWork" :: Text)
        , "identifier" .= spdxId l
        , "name" .= stName (standardTerms l)
        ]
    ]
licenceEntities name (LicenceOwn own) =
    [ object
        [ "@id" .= ownLicenceId
        , "@type" .= ("CreativeWork" :: Text)
        , "name" .= ("Licence of " <> name)
        , "description" .= ownText own
        , "additionalProperty" .= refs [refusesId, attributionId]
        ]
    , property refusesId "refuses" (A.toJSON (map permissionCode (S.toList (ownRefused own))))
    , property attributionId "attribution" (A.Bool (ownAttribution own == AttributionRequired))
    ]
  where
    refusesId, attributionId :: Text
    refusesId = ownLicenceId <> "-refuses"
    attributionId = ownLicenceId <> "-attribution"

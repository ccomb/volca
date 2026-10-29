{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | A database exported as a package: the export itself, unchanged, beside an
RO-Crate description of it (<https://w3id.org/ro/crate/1.2>). The description
says what a reader needs before loading it: its licence, and the published
releases it was built on, so a reader's engine can tell whether it holds them.

Only schema.org terms are used, plus the RO-Crate @sha256@ term, declared in the
context itself so a reader does not depend on the published context carrying it.
-}
module Database.Crate (
    CrateInput (..),
    Changes (..),
    MadeFrom (..),
    Identity (..),
    Division (..),
    modelFile,
    traceFile,
    Packaging (..),
    parsePackaging,
    requiredReleases,
    Payload (..),
    payloadOf,
    crateMetadata,
    packageExport,
    Package (..),
    openPackage,
) where

import Codec.Archive.Zip (findEntryByPath, fromEntry, toArchiveOrFail)
import Control.Monad ((<=<))
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.Aeson (FromJSON, Object, Value, object, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Numeric (showFFloat)

import Config (DatabaseConfig (..))
import Database.SystemModel (Traced (..), tracedRule)
import Database.Upload (DatabaseFormat (..), formatDisplayText)
import Types (Attribution (..), Licence (..), LicenceKeys (..), OwnLicence (..), ProcessRef (..), Release (..), StandardLicence, StandardTerms (..), SystemModel (..), acceptanceAsked, countedDisclosure, licenceFromKeys, licenceUsage, permissionCode, processRefText, spdxId, standardTerms)
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
    , ciChanges :: !(Maybe Changes)
    -- ^ What was changed, for a database whose data differ from its original
    , ciMadeFrom :: !MadeFrom
    }

{- | What a database was made from, named by digest so a reader can tell two
deliveries of it apart: its own files, or a parent and the model dividing it.
-}
data MadeFrom
    = ReadFrom !Identity
    | DividedFrom !Division

{- | A database read from files, by SHA-256 digest: the files, the patches its
configuration applies to them, and its journal of changes, with the reading
of their shares. A digest left out is a part the database does not have.
-}
data Identity = Identity
    { idFiles :: !Text
    , idPatches :: !(Maybe Text)
    , idJournal :: !(Maybe Text)
    , idReading :: !Text
    }

{- | A child, made from its parent by a system model. The parent's package
carries the same identity, and its changes.
-}
data Division = Division
    { dvParent :: !Text
    , dvParentIdentity :: !Identity
    , dvJournal :: !(Maybe Text)
    -- ^ The child's own journal of changes, made after the division
    , dvModel :: !SystemModel
    , dvEngine :: !Text
    , dvMade :: !(Maybe UTCTime)
    , dvTrace :: ![Traced]
    }

{- | What a modified database's package says of its changes: the report listing
each with its reason, and the original it was made from, which the package
cites. The original is not a release the package requires: the export holds
every activity it needs.
-}
data Changes = Changes
    { chReport :: !Text
    , chOriginal :: !Text
    , chOriginalRelease :: !(Maybe Release)
    }

metadataFile :: FilePath
metadataFile = "ro-crate-metadata.json"

changesFile, licenceFile, modelFile, traceFile :: FilePath
changesFile = "CHANGES.md"
licenceFile = "LICENCE.txt"
modelFile = "system-model.toml"
traceFile = "allocation.csv"

{- | The table tracing a child's processes back to the parent's blocks, one row
each, as RFC 4180 CSV.
-}
traceCsv :: [Traced] -> Text
traceCsv rows = T.concat (map line (header : map cells rows))
  where
    header :: [Text]
    header = ["process_id", "parent_process_id", "activity", "product", "share_percent", "rule"]

    cells :: Traced -> [Text]
    cells t@Traced{..} =
        [processRefText (uncurry ProcessRef trProcess), processRefText (uncurry ProcessRef trParent), trActivity, trProduct, maybe "" (\s -> T.pack (showFFloat Nothing s "")) trShare, tracedRule t]

    line :: [Text] -> Text
    line fields = T.intercalate "," (map quoted fields) <> "\r\n"

    quoted :: Text -> Text
    quoted field
        | T.any (`elem` [',', '"', '\r', '\n']) field = "\"" <> T.replace "\"" "\"\"" field <> "\""
        | otherwise = field

-- | The licence in plain words, for a reader who opens no JSON.
licenceText :: Licence -> Maybe Text
licenceText = \case
    LicenceUnstated -> Nothing
    LicenceStandard l -> Just (stName (standardTerms l) <> "\n" <> spdxUrl l <> "\n")
    LicenceOwn own -> Just (ownText own)

-- | The files beside the export and its description, each with its content.
companions :: CrateInput -> [(FilePath, Text)]
companions input = changed <> divided
  where
    changed = case ciChanges input of
        -- An unchanged database's package keeps the one file engines already read.
        Nothing -> []
        Just changes -> (changesFile, chReport changes) : [(licenceFile, text) | Just text <- [licenceText (ciLicence input)]]
    -- The model as its author wrote it, comments and all, never written again from what was read.
    divided = case ciMadeFrom input of
        ReadFrom _ -> []
        DividedFrom division -> [(modelFile, smText (dvModel division)), (traceFile, traceCsv (dvTrace division))]

-- | Where the export sits in the package, and what kind of file it is.
data Payload = Payload
    { payloadFile :: !FilePath
    , payloadMediaType :: !Text
    }

{- | The export's place in the package, named after the database, with the
extension and media type of what the format writes.
-}
payloadOf :: Text -> DatabaseFormat -> Payload
payloadOf name format = Payload{payloadFile = "payload/" <> T.unpack name <> extension, payloadMediaType = mediaType}
  where
    extension :: FilePath
    mediaType :: Text
    (extension, mediaType) = case format of
        EcoSpold2 -> (".zip", "application/zip")
        ILCDProcess -> (".zip", "application/zip")
        EcoSpold1 -> (".xml", "application/xml")
        SimaProCSV -> (".csv", "text/csv")
        BrightwayExcel -> (".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        OpenLcaImpactCategory -> (".json", "application/ld+json")
        OpenLcaPackage -> (".zip", "application/zip")
        UnknownFormat -> ("", "application/octet-stream")

-- | The package: the export at its 'payloadPath', and the description of it.
packageExport :: Text -> CrateInput -> BL.ByteString -> BL.ByteString
packageExport name input payload =
    zipFiles $
        [ (metadataFile, BL.toStrict (encodePretty (crateMetadata name input payload)))
        , (path, BL.toStrict payload)
        ]
            <> [(file, T.encodeUtf8 text) | (file, text) <- companions input]
  where
    path :: FilePath
    path = payloadFile (payloadOf name (ciFormat input))

{- | The @ro-crate-metadata.json@ describing a packaged export. The graph is
flat, as RO-Crate requires: every property value that is an object is an
entity of its own, referenced by its @\@id@.
-}
crateMetadata :: Text -> CrateInput -> BL.ByteString -> Value
crateMetadata name input payload =
    object
        [ "@context" .= [A.String "https://w3id.org/ro/crate/1.2/context", object ["sha256" .= ("https://w3id.org/ro/terms#sha256" :: Text)]]
        , "@graph" .= (descriptor : root : file : map companion (companions input) <> licenceEntities (ciName input) (ciLicence input) <> concat (zipWith releaseEntities [0 ..] (ciRequires input)) <> foldMap originalEntities (ciChanges input) <> madeFromEntities)
        ]
  where
    Payload{payloadFile = path, payloadMediaType = mediaType} = payloadOf name (ciFormat input)

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
            , "description" .= maybe plainDescription (\changes -> modified changes <> foldMap (" " <>) (ciDescription input)) (ciChanges input)
            , "datePublished" .= iso8601Show (ciPublished input)
            , "hasPart" .= refs (T.pack path : map (T.pack . fst) (companions input))
            ]
                <> ["isBasedOn" .= refs (map releaseId [0 .. length (ciRequires input) - 1]) | not (null (ciRequires input))]
                <> ["citation" .= ref originalId | Just _ <- [ciChanges input]]
                <> maybe [] (\l -> ["license" .= ref l]) (licenceId (ciLicence input))
                <> case ciMadeFrom input of
                    ReadFrom identity -> ["additionalProperty" .= refs (fst (identityProperties "#" identity))]
                    DividedFrom division ->
                        ["conformsTo" .= ref processRunCrate, "mentions" .= ref divisionId]
                            <> ["additionalProperty" .= ref ownJournalId | Just _ <- [dvJournal division]]

    madeFromEntities :: [Value]
    madeFromEntities = case ciMadeFrom input of
        ReadFrom identity -> snd (identityProperties "#" identity)
        DividedFrom division -> divisionEntities path division <> [property ownJournalId "journal sha256" (A.String digest) | Just digest <- [dvJournal division]]

    ownJournalId :: Text
    ownJournalId = "#journal-sha256"

    file :: Value
    file =
        object
            [ "@id" .= path
            , "@type" .= ("File" :: Text)
            , "name" .= (ciName input <> ", " <> formatDisplayText (ciFormat input))
            , "description" .= ("The " <> formatDisplayText (ciFormat input) <> " export of " <> ciName input <> "." :: Text)
            , "encodingFormat" .= mediaType
            , "contentSize" .= show (BL.length payload)
            , "sha256" .= digestOf payload
            ]

    plainDescription :: Text
    plainDescription = fromMaybe (ciName input <> ", packaged with the licence it is published under and the releases it was built on.") (ciDescription input)

    modified :: Changes -> Text
    modified changes =
        ciName input
            <> ", modified from "
            <> chOriginal changes
            <> foldMap ((" " <>) . releaseVersion) (chOriginalRelease changes)
            <> ". Modified data; not endorsed by the publisher of the original. "
            <> T.pack changesFile
            <> " lists every change and its reason."

    companion :: (FilePath, Text) -> Value
    companion (companionPath, text) =
        object $
            [ "@id" .= companionPath
            , "@type" .= ("File" :: Text)
            , "encodingFormat" .= mediaTypeOf companionPath
            , "contentSize" .= show (BS.length bytes)
            , "sha256" .= digestOf (BL.fromStrict bytes)
            ]
                <> ["description" .= traceDescription | companionPath == traceFile]
      where
        bytes = T.encodeUtf8 text

    mediaTypeOf :: FilePath -> Text
    mediaTypeOf companionPath
        | companionPath == changesFile = "text/markdown"
        | companionPath == modelFile = "application/toml"
        | companionPath == traceFile = "text/csv"
        | otherwise = "text/plain"

    traceDescription :: Text
    traceDescription =
        "Each process of this child that comes from a block of its parent, with its share and the rule that gave it, and each share below 100 the source declared on a process of one product. Any other process is the parent's as it stands. A block left whole is listed once, under its reference product."

originalId :: Text
originalId = "#original"

-- | The original a modified database was made from, as its package cites it.
originalEntities :: Changes -> [Value]
originalEntities changes =
    [ object $
        ["@id" .= originalId, "@type" .= ("Dataset" :: Text), "name" .= maybe (chOriginal changes) releaseName (chOriginalRelease changes)]
            <> ["version" .= releaseVersion release | Just release <- [chOriginalRelease changes]]
    ]

{- | An identity as properties, each id under the prefix, and the ids the
holder refers to them by.
-}
identityProperties :: Text -> Identity -> ([Text], [Value])
identityProperties prefix identity = (map fst named, map (\(propertyId, (name, value)) -> property propertyId name (A.String value)) named)
  where
    named :: [(Text, (Text, Text))]
    named =
        [ (prefix <> slug, (name, value))
        | (slug, name, Just value) <-
            [ ("files-sha256", "files sha256", Just (idFiles identity))
            , ("patches-sha256", "patches sha256", idPatches identity)
            , ("journal-sha256", "journal sha256", idJournal identity)
            , ("reading", "reading", Just (idReading identity))
            ]
        ]

divisionId, parentId, engineId, processRunCrate :: Text
divisionId = "#division"
parentId = "#parent"
engineId = "#engine"
processRunCrate = "https://w3id.org/ro/wfrun/process/0.5"

{- | The division of a child as a Process Run Crate action: the engine, given
the parent and the model, made the export and its table.
-}
divisionEntities :: FilePath -> Division -> [Value]
divisionEntities payloadPath division =
    [ object $
        [ "@id" .= divisionId
        , "@type" .= ("CreateAction" :: Text)
        , "name" .= ("Division of " <> dvParent division <> " by the system model " <> smName model <> " " <> smVersion model)
        , "instrument" .= ref engineId
        , "object" .= refs [parentId, T.pack modelFile]
        , "result" .= refs [T.pack payloadPath, T.pack traceFile]
        ]
            <> ["startTime" .= iso8601Show made | Just made <- [dvMade division]]
    , object
        [ "@id" .= engineId
        , "@type" .= ("SoftwareApplication" :: Text)
        , "name" .= ("VoLCA" :: Text)
        , "url" .= ("https://volca.run" :: Text)
        , "softwareVersion" .= dvEngine division
        ]
    , object
        [ "@id" .= parentId
        , "@type" .= ("Dataset" :: Text)
        , "name" .= dvParent division
        , "description" .= ("The parent this child was divided from, named by the digests of its files, of the patches applied to them and of its journal of changes. The parent's own package carries the same digests, and lists its changes." :: Text)
        , "additionalProperty" .= refs ids
        ]
    , object ["@id" .= processRunCrate, "@type" .= ("CreativeWork" :: Text), "name" .= ("Process Run Crate" :: Text), "version" .= ("0.5" :: Text)]
    ]
        <> parentProperties
  where
    model :: SystemModel
    model = dvModel division

    (ids, parentProperties) = identityProperties (parentId <> "-") (dvParentIdentity division)

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
spdxUrl l = spdxPrefix <> spdxId l

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
licenceEntities name licence@(LicenceOwn own) =
    [ object
        [ "@id" .= ownLicenceId
        , "@type" .= ("CreativeWork" :: Text)
        , "name" .= ("Licence of " <> name)
        , "description" .= ownText own
        , "additionalProperty" .= refs ([refusesId, attributionId, acceptId] <> (usageId <$ maybeToList usage))
        ]
    , property refusesId "refuses" (A.toJSON (map permissionCode (S.toList (ownRefused own))))
    , property attributionId "attribution" (A.Bool (ownAttribution own == AttributionRequired))
    , property acceptId "accept" (A.Bool (acceptanceAsked (ownAcceptance own)))
    ]
        <> [property usageId "usage" (A.toJSON d) | d <- maybeToList usage]
  where
    usage = countedDisclosure (licenceUsage licence)
    refusesId, attributionId, acceptId, usageId :: Text
    refusesId = ownLicenceId <> "-refuses"
    attributionId = ownLicenceId <> "-attribution"
    acceptId = ownLicenceId <> "-accept"
    usageId = ownLicenceId <> "-usage"

digestOf :: BL.ByteString -> Text
digestOf payload = T.pack (show (hashlazy payload :: Digest SHA256))

-- | What a package says, read back where it is uploaded.
data Package = Package
    { pkPayload :: !BL.ByteString
    -- ^ The export it carries, checked against its digest
    , pkLicence :: !Licence
    , pkRequires :: ![Release]
    }

{- | Read an upload as a package. 'Nothing' when it is not one: not a zip, or a
zip with no description at its root, which is any export uploaded as it is. A
package whose description cannot be read, or whose export does not match its
digest, is refused: loaded anyway, it would be served under terms it may not
carry, or be other data than its description says.
-}
openPackage :: BL.ByteString -> Either Text (Maybe Package)
openPackage bytes = either (const (Right Nothing)) opened (toArchiveOrFail bytes)
  where
    opened archive = traverse (readPackage archive . fromEntry) (findEntryByPath metadataFile archive)

    readPackage archive metadata = do
        Described{..} <- first (("The package description cannot be read: " <>) . T.pack) (parseEither described =<< A.eitherDecode metadata)
        payload <- maybe (Left ("The package holds no " <> T.pack dPath <> ", which its description names.")) (Right . fromEntry) (findEntryByPath dPath archive)
        licence <- first ("The package licence cannot be read: " <>) (licenceFromKeys dLicence)
        if T.toLower dDigest == digestOf payload
            then Right Package{pkPayload = payload, pkLicence = licence, pkRequires = dRequires}
            else Left ("The package's " <> T.pack dPath <> " does not match the digest its description gives: it was changed after it was packaged.")

-- | The description of a package, as far as loading it needs.
data Described = Described
    { dPath :: !FilePath
    , dDigest :: !Text
    , dLicence :: !LicenceKeys
    , dRequires :: ![Release]
    }

-- | Read back what 'crateMetadata' writes, following the graph's references.
described :: Value -> Parser Described
described = A.withObject "RO-Crate description" $ \o -> do
    graph <- o .: "@graph"
    let entity :: Text -> Parser Object
        entity target = case filter ((== Just (A.String target)) . KM.lookup "@id") graph of
            [e] -> pure e
            [] -> fail ("no entity " <> T.unpack target)
            _ -> fail ("several entities " <> T.unpack target)
        properties :: Object -> Parser [Object]
        properties e = traverse entity =<< maybe (pure []) refsOf =<< e .:? "additionalProperty"
        licenceOf :: Text -> Parser LicenceKeys
        licenceOf target = case T.stripPrefix spdxPrefix target of
            Just spdx -> pure (LicenceKeys (Just spdx) Nothing Nothing Nothing Nothing Nothing)
            Nothing -> do
                own <- entity target
                props <- properties own
                LicenceKeys Nothing <$> (Just <$> own .: "description") <*> propertyValue "refuses" props <*> propertyValue "attribution" props <*> propertyValue "accept" props <*> propertyValue "usage" props
        releaseOf :: Text -> Parser Release
        releaseOf target = do
            dataset <- entity target
            Release <$> dataset .: "name" <*> dataset .: "version" <*> (propertyValue "system model" =<< properties dataset)
    root <- entity "./"
    -- The export is the one part under payload/; a modified database's report and licence sit beside it.
    parts <- filter (T.isPrefixOf "payload/") <$> (refsOf =<< root .: "hasPart")
    path <- case parts of
        [one] -> pure one
        several -> fail ("expected one file under payload/ in hasPart, found " <> show (length several))
    file <- entity path
    Described (T.unpack path)
        <$> file .: "sha256"
        <*> (maybe (pure (LicenceKeys Nothing Nothing Nothing Nothing Nothing Nothing)) (licenceOf <=< refOf) =<< root .:? "license")
        <*> (maybe (pure []) (traverse releaseOf <=< refsOf) =<< root .:? "isBasedOn")

-- | The value of the property of that name, when there is one.
propertyValue :: (FromJSON a) => Text -> [Object] -> Parser (Maybe a)
propertyValue name = traverse (.: "value") . find ((== Just (A.String name)) . KM.lookup "name")

refOf :: Value -> Parser Text
refOf = A.withObject "reference" (.: "@id")

-- | One reference or a list of them, the two shapes 'refs' writes.
refsOf :: Value -> Parser [Text]
refsOf v@(A.Object _) = pure <$> refOf v
refsOf v = traverse refOf =<< A.parseJSON v

spdxPrefix :: Text
spdxPrefix = "https://spdx.org/licenses/"

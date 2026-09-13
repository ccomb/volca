{-# LANGUAGE OverloadedStrings #-}

{- | Covers the end-to-end upload pipeline for openLCA JSON-LD method files:
the upload byte-stream is sniffed, persisted under the right extension,
and the loader picks it up via OlcaSchema. Regression gate for the
pre-existing bug where a JSON blob was mis-classified as CSV.
-}
module MethodUploadSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.DatabaseHandlers (formatToText)
import Config (MethodConfig (..))
import Database.Manager (loadMethodCollectionFromConfig)
import Database.Upload
import Method.Types (Method (..), MethodCF (..), mcMethods)

{- | A minimal, hand-written openLCA ImpactCategory JSON-LD document.
One impact factor is enough to assert the full pipeline parses correctly.
-}
miniImpactCategoryJson :: BL.ByteString
miniImpactCategoryJson =
    BLC.pack $
        unlines
            [ "{"
            , "  \"@context\": \"http://greendelta.github.io/olca-schema/context.jsonld\","
            , "  \"@type\": \"ImpactCategory\","
            , "  \"@id\": \"00000000-0000-0000-0000-000000000001\","
            , "  \"name\": \"Test category\","
            , "  \"referenceUnitName\": \"m2*year\","
            , "  \"impactFactors\": ["
            , "    {"
            , "      \"@type\": \"ImpactFactor\","
            , "      \"value\": 1.5,"
            , "      \"flow\": {"
            , "        \"@type\": \"Flow\","
            , "        \"@id\": \"00000000-0000-0000-0000-000000000002\","
            , "        \"name\": \"Occupation, test\","
            , "        \"flowType\": \"ELEMENTARY_FLOW\""
            , "      },"
            , "      \"unit\": { \"@type\": \"Unit\", \"name\": \"m2*year\" }"
            , "    }"
            , "  ]"
            , "}"
            ]

-- | A minimal ILCD LCIA method dataset – only the root element matters here.
miniLciaMethodXml :: BL.ByteString
miniLciaMethodXml =
    BLC.pack $
        unlines
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<LCIAMethodDataSet xmlns=\"http://lca.jrc.it/ILCD/LCIAMethod\">"
            , "</LCIAMethodDataSet>"
            ]

-- | An openLCA Process document – must NOT be picked up as a method.
miniProcessJson :: BL.ByteString
miniProcessJson =
    BLC.pack "{ \"@type\": \"Process\", \"name\": \"not a method\" }"

spec :: Spec
spec = do
    describe "detectArchiveFormat on JSON blobs" $ do
        it "routes an openLCA ImpactCategory JSON to ArchivePlainJSON (regression: was ArchivePlainCSV)" $
            detectArchiveFormat miniImpactCategoryJson `shouldBe` ArchivePlainJSON

        it "leaves an unrelated JSON blob on the plain-text branch" $
            -- Process documents start with '{' too but lack the ImpactCategory
            -- marker, so the sniff must not over-fire.
            detectArchiveFormat miniProcessJson `shouldBe` ArchivePlainCSV

    describe "detectArchiveFormat on the plain-text sniff" $ do
        it "answers ArchiveUnknown on empty input" $
            -- The emptiness guard answers first, so this passes whether or not
            -- the sniff below it reads the first byte totally. It pins the
            -- result the guard owes; the byte set is what the cases below pin.
            detectArchiveFormat BL.empty `shouldBe` ArchiveUnknown

        it "accepts the printable ASCII range and nothing outside it" $ do
            detectArchiveFormat (BL.pack [0x20]) `shouldBe` ArchivePlainCSV
            detectArchiveFormat (BL.pack [0x7E]) `shouldBe` ArchivePlainCSV
            detectArchiveFormat (BL.pack [0x1F]) `shouldBe` ArchiveUnknown
            detectArchiveFormat (BL.pack [0x7F]) `shouldBe` ArchiveUnknown

    describe "handleUpload writes JSON-LD as .json" $
        it "persists data.json (not data.csv) so the loader can dispatch through OlcaSchema" $
            withSystemTempDirectory "volca-method-upload" $ \tmp -> do
                let payload =
                        UploadData
                            { udName = "Test JSON-LD method"
                            , udDescription = Nothing
                            , udZipData = miniImpactCategoryJson
                            }
                result <- handleUpload tmp payload (\_ -> pure ())
                case result of
                    Left err -> expectationFailure ("upload failed: " ++ show err)
                    Right res -> do
                        urFormat res `shouldBe` OpenLcaJsonLd
                        -- Lock the side fix: the API response advertises the
                        -- detected format slug, not a hardcoded "ILCD".
                        formatToText (urFormat res) `shouldBe` "openlca-jsonld"
                        let slugDir = tmp </> "test-json-ld-method"
                        files <- listDirectory slugDir
                        files `shouldContain` ["data.json"]
                        doesFileExist (slugDir </> "data.csv") `shouldReturn` False

    describe "detectDatabaseFormat on a directory with a JSON-LD ImpactCategory" $
        it "returns OpenLcaJsonLd (covers the directory branch missed by the single-file test)" $
            withSystemTempDirectory "volca-method-detect" $ \tmp -> do
                let dir = tmp </> "method-dir"
                createDirectoryIfMissing True dir
                BL.writeFile (dir </> "impact-category.json") miniImpactCategoryJson
                detectDatabaseFormat dir `shouldReturn` OpenLcaJsonLd

    describe "detectMethodFormat on an ILCD method package" $
        -- Regression: an EF 3.1 ILCD package ships companion spreadsheets
        -- (normalisation factors, UUID mappings) outside the method directory.
        -- The database detector saw those and called the whole thing Brightway
        -- Excel, so the Methods page advertised the wrong format.
        it "reads ILCD off the method directory, ignoring companion spreadsheets" $
            withSystemTempDirectory "volca-method-format" $ \tmp -> do
                let methodDir = tmp </> "ILCD" </> "lciamethods"
                    otherDir = tmp </> "other"
                createDirectoryIfMissing True methodDir
                createDirectoryIfMissing True otherDir
                BL.writeFile (methodDir </> "climate.xml") miniLciaMethodXml
                BL.writeFile (otherDir </> "Normalisation_Weighting_Factors.xlsx") (BLC.pack "PK stub")
                -- The database detector is the one that gets it wrong:
                detectDatabaseFormat tmp `shouldReturn` BrightwayExcel
                found <- findMethodDirectory tmp
                found `shouldBe` methodDir
                detectMethodFormat found `shouldReturn` ILCDProcess
                formatDisplayText ILCDProcess `shouldBe` "ILCD"

    describe "detectMethodFormat when nothing matches" $ do
        it "does not read a non-SimaPro CSV sitting next to the method files as SimaPro" $
            withSystemTempDirectory "volca-method-format-csv" $ \tmp -> do
                BL.writeFile (tmp </> "climate.xml") miniLciaMethodXml
                BL.writeFile (tmp </> "factors.csv") (BLC.pack "flow,cf\nCO2,1.0\n")
                detectMethodFormat tmp `shouldReturn` ILCDProcess

        it "stays UnknownFormat, with no label to advertise, on an unrecognized directory" $
            withSystemTempDirectory "volca-method-format-empty" $ \tmp -> do
                BL.writeFile (tmp </> "readme.txt") (BLC.pack "nothing to see")
                detectMethodFormat tmp `shouldReturn` UnknownFormat
                detectedFormatLabel UnknownFormat `shouldBe` Nothing

        it "reports UnknownFormat instead of throwing on a missing directory" $
            withSystemTempDirectory "volca-method-format-gone" $ \tmp ->
                detectMethodFormat (tmp </> "does-not-exist") `shouldReturn` UnknownFormat

    describe "loadMethodCollectionFromConfig on the uploaded JSON" $
        it "produces one Method with one CF carrying the fixture's value" $
            withSystemTempDirectory "volca-method-load" $ \tmp -> do
                let payload =
                        UploadData
                            { udName = "Test JSON-LD method"
                            , udDescription = Nothing
                            , udZipData = miniImpactCategoryJson
                            }
                Right res <- handleUpload tmp payload (\_ -> pure ())
                let mc =
                        MethodConfig
                            { mcName = "Test JSON-LD method"
                            , mcPath = urPath res
                            , mcActive = False
                            , mcIsUploaded = True
                            , mcDescription = Nothing
                            , mcFormat = Just "openlca-jsonld"
                            , mcScoringSets = []
                            , mcGlobalMethods = []
                            , mcPatches = []
                            }
                loaded <- loadMethodCollectionFromConfig mc
                case loaded of
                    Left err -> expectationFailure ("load failed: " ++ show err)
                    Right (collection, _) -> do
                        let methods = mcMethods collection
                        length methods `shouldBe` 1
                        let factors = methodFactors (head methods)
                        length factors `shouldBe` 1
                        mcfValue (head factors) `shouldBe` 1.5
                        methodName (head methods) `shouldBe` "Test category"
                        fromMaybe "" (methodMethodology (head methods))
                            `shouldBe` "openLCA JSON-LD"

    describe "loadMethodCollectionFromConfig on a bare file path" $ do
        -- SimaPro exports a method as a single .csv; requiring users to wrap it
        -- in a zip or directory is pure friction, so the loader takes it as-is.
        it "loads a single SimaPro method .csv directly" $ do
            loaded <- loadMethodCollectionFromConfig (bareConfig "test/data/simapro_method.csv")
            case loaded of
                Left err -> expectationFailure ("load failed: " ++ show err)
                Right (collection, _) -> mcMethods collection `shouldSatisfy` (not . null)

        it "reports an existing file of unsupported type as such, not as missing" $
            withSystemTempDirectory "volca-method-load" $ \tmp -> do
                let path = tmp </> "not-a-method.txt"
                BL.writeFile path (BLC.pack "hello")
                loaded <- loadMethodCollectionFromConfig (bareConfig path)
                case loaded of
                    Left err -> err `shouldSatisfy` T.isInfixOf "Unsupported method file type"
                    Right _ -> expectationFailure "expected a Left for a .txt file"

        it "still reports a missing path as not found" $ do
            loaded <- loadMethodCollectionFromConfig (bareConfig "test/data/no-such-method.csv")
            case loaded of
                Left err -> err `shouldSatisfy` T.isInfixOf "Method path not found"
                Right _ -> expectationFailure "expected a Left for a missing path"

        -- A .zip is a supported wrapper, so a failed extraction must not be
        -- reported as an unsupported file type (that message even lists 'archive').
        it "reports an unextractable archive as an extraction failure, not an unsupported type" $
            withSystemTempDirectory "volca-method-load" $ \tmp -> do
                let path = tmp </> "broken.zip"
                -- Binary garbage: recognised as no known archive format, so
                -- extraction fails and resolveDataPath refuses.
                BL.writeFile path (BL.pack [0, 1, 2, 3, 4, 5, 6, 7])
                loaded <- loadMethodCollectionFromConfig (bareConfig path)
                case loaded of
                    Left err -> err `shouldSatisfy` T.isInfixOf "Archive could not be extracted"
                    Right _ -> expectationFailure "expected a Left for an unextractable .zip"
  where
    bareConfig path =
        MethodConfig
            { mcName = "bare"
            , mcPath = path
            , mcActive = False
            , mcIsUploaded = False
            , mcDescription = Nothing
            , mcFormat = Nothing
            , mcScoringSets = []
            , mcGlobalMethods = []
            , mcPatches = []
            }

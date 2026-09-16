{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the openLCA JSON-LD method writer ("Method.WriterOlcaSchema").
module MethodWriterOlcaSchemaSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID, fromWords)
import qualified Data.UUID as UUID
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (MethodConfig (..), MethodOrigin (..))
import Database.Export (MethodExportFormat (..), parseMethodExportFormat, serializeMethodCollection)
import Database.Manager (loadMethodCollectionFromConfig)
import Method.Parser.OlcaSchema (parseOlcaImpactCategoryBytes)
import Method.Types
import Method.WriterOlcaSchema (serializeOlcaMethodEntries)

mkCF :: Text -> Maybe Compartment -> Double -> MethodCF
mkCF name comp v =
    MethodCF
        { mcfFlowRef = fromWords 0 0 0 9
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = v
        , mcfCompartment = comp
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkId :: Word -> UUID
mkId n = fromWords (fromIntegral n) 0 0 0

mkMethod :: Word -> Text -> [MethodCF] -> Method
mkMethod n name cfs =
    Method
        { methodId = mkId n
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg eq"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = cfs
        }

collection :: [Method] -> MethodCollection
collection ms = MethodCollection ms [] [] []

{- | What a method reads back as: the methodology is the only field the
format cannot carry, so re-import stamps its own.
-}
reimported :: Method -> Method
reimported m = m{methodMethodology = Just "openLCA JSON-LD"}

-- | Re-parse every archive entry, failing the test on a parse error.
reparseEntries :: [(FilePath, BS.ByteString)] -> Either String [Method]
reparseEntries = traverse (parseOlcaImpactCategoryBytes . snd)

spec :: Spec
spec = describe "Method.WriterOlcaSchema" $ do
    describe "round-trip with the openLCA parser" $ do
        it "write → parse reproduces each method exactly (UUIDs, order, factors)" $ do
            let cfA = (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1.0){mcfCAS = Just "124-38-9"}
                cfB =
                    (mkCF "Water" (Just (Compartment "natural resource" "in water" "")) 42.95)
                        { mcfDirection = Input
                        , mcfConsumerLocation = Just "FR"
                        , mcfUnit = "m3"
                        }
                m =
                    (mkMethod 1 "Water use" [cfA, cfB])
                        { methodDescription = Just "deprivation-weighted"
                        , methodUnit = "m3 world eq"
                        , methodCategory = "EF 3.1"
                        }
            case serializeOlcaMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, warnings) -> do
                    warnings `shouldBe` []
                    map fst entries `shouldBe` ["lcia_categories/" <> UUID.toString (mkId 1) <> ".json"]
                    reparseEntries entries `shouldBe` Right [reimported m]

        it "write → parse → write is byte-stable" $ do
            let m =
                    (mkMethod 3 "Acidification" [mkCF "Ammonia" (Just (Compartment "air" "" "")) 3.02])
                        { methodCategory = "EF 3.1"
                        , methodMethodology = Just "openLCA JSON-LD"
                        }
            case serializeOlcaMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) -> case reparseEntries entries of
                    Left err -> expectationFailure ("re-parse failed: " <> err)
                    Right ms ->
                        serializeOlcaMethodEntries (collection ms)
                            `shouldBe` Right (entries, [])

        it "round-trips the mini fixture identically" $ do
            raw <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes raw of
                Left err -> expectationFailure ("fixture parse failed: " <> err)
                Right m ->
                    case serializeOlcaMethodEntries (collection [m]) of
                        Left err -> expectationFailure (T.unpack err)
                        Right (entries, warnings) -> do
                            warnings `shouldBe` []
                            reparseEntries entries `shouldBe` Right [m]

        it "folds the compartment qualifier into the subcompartment" $ do
            let cf = mkCF "Arsenic" (Just (Compartment "water" "groundwater" "long-term")) 1.5
            case serializeOlcaMethodEntries (collection [mkMethod 4 "Ecotoxicity" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) -> case reparseEntries entries of
                    Right [m] ->
                        map mcfCompartment (methodFactors m)
                            `shouldBe` [Just (Compartment "water" "groundwater/long-term" "")]
                    other -> expectationFailure ("unexpected re-parse: " <> show other)

        it "sorts archive entries by method UUID" $ do
            let ms = [mkMethod 2 "B" [], mkMethod 1 "A" []]
            case serializeOlcaMethodEntries (collection ms) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) ->
                    map fst entries
                        `shouldBe` [ "lcia_categories/" <> UUID.toString (mkId 1) <> ".json"
                                   , "lcia_categories/" <> UUID.toString (mkId 2) <> ".json"
                                   ]

    describe "document shape" $ do
        it "omits empty optional fields instead of writing them blank" $ do
            let cf = (mkCF "Ammonia" Nothing 2.0){mcfUnit = ""}
                m = (mkMethod 5 "Acidification" [cf]){methodUnit = ""}
            case serializeOlcaMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) -> case entries of
                    [(_, bytes)] -> do
                        let doc = TE.decodeUtf8 bytes
                        doc `shouldNotSatisfy` T.isInfixOf "referenceUnitName"
                        doc `shouldNotSatisfy` T.isInfixOf "description"
                        doc `shouldNotSatisfy` T.isInfixOf "\"unit\""
                        doc `shouldNotSatisfy` T.isInfixOf "location"
                        doc `shouldNotSatisfy` T.isInfixOf "cas"
                        doc `shouldNotSatisfy` T.isInfixOf "category"
                    other -> expectationFailure ("expected one entry, got " <> show (length other))

        it "writes the category label only when it differs from the name" $ do
            let m = (mkMethod 6 "Climate change" []){methodCategory = "EF 3.1"}
            case serializeOlcaMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) -> case entries of
                    [(_, bytes)] -> do
                        TE.decodeUtf8 bytes `shouldSatisfy` T.isInfixOf "\"category\":\"EF 3.1\""
                        reparseEntries entries `shouldBe` Right [reimported m]
                    other -> expectationFailure ("expected one entry, got " <> show (length other))

    describe "representation-loss warnings" $ do
        it "counts methodology labels, blank group labels, damage, NW and scoring sets" $ do
            let m1 = (mkMethod 7 "Climate change" []){methodMethodology = Just "Environmental Footprint"}
                m2 = (mkMethod 8 "Acidification" []){methodCategory = ""}
                dc = DamageCategory "Human health" "DALY" [("Climate change", 1)]
                nw = NormWeightSet "EF" (M.singleton "Climate change" 1) M.empty
                ss = ScoringSet "EF score" "Pt" M.empty M.empty M.empty M.empty M.empty M.empty Nothing
            case serializeOlcaMethodEntries (MethodCollection [m1, m2] [dc] [nw] [ss]) of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> do
                    warnings `shouldSatisfy` any (T.isInfixOf "1 methodology labels")
                    warnings `shouldSatisfy` any (T.isInfixOf "1 blank impact category group labels")
                    warnings `shouldSatisfy` any (T.isInfixOf "1 damage categories")
                    warnings `shouldSatisfy` any (T.isInfixOf "1 normalization/weighting sets")
                    warnings `shouldSatisfy` any (T.isInfixOf "1 formula scoring sets")

        it "does not warn about the methodology re-import itself stamps" $ do
            let m = (mkMethod 9 "A" []){methodMethodology = Just "openLCA JSON-LD"}
            case serializeOlcaMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> warnings `shouldBe` []

    describe "exportability guard" $ do
        it "rejects an empty collection" $
            serializeOlcaMethodEntries (collection []) `shouldSatisfy` isRefused "no impact categories"

        it "rejects two methods sharing a UUID" $ do
            let ms = [mkMethod 1 "A" [], mkMethod 1 "B" []]
            serializeOlcaMethodEntries (collection ms) `shouldSatisfy` isRefused "share the id"

        it "rejects a factor with no flow name" $ do
            let cf = mkCF "" (Just (Compartment "air" "" "")) 1
            serializeOlcaMethodEntries (collection [mkMethod 1 "A" [cf]])
                `shouldSatisfy` isRefused "no flow name"

        it "rejects a non-finite characterization factor" $ do
            let cf = mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) (0 / 0)
            serializeOlcaMethodEntries (collection [mkMethod 1 "A" [cf]])
                `shouldSatisfy` isRefused "non-finite"

        it "rejects a compartment with an empty medium" $ do
            let cf = mkCF "Zinc" (Just (Compartment " " "ground" "")) 1
            serializeOlcaMethodEntries (collection [mkMethod 1 "A" [cf]])
                `shouldSatisfy` isRefused "empty medium"

        it "rejects a medium containing the path separator" $ do
            let cf = mkCF "Zinc" (Just (Compartment "wa/ter" "" "")) 1
            serializeOlcaMethodEntries (collection [mkMethod 1 "A" [cf]])
                `shouldSatisfy` isRefused "path separator"

    describe "zip archive end to end" $ do
        it "the exported zip loads back through the method-collection loader" $ do
            let cfA = (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1.0){mcfCAS = Just "124-38-9"}
                cfB = (mkCF "Occupation, arable" (Just (Compartment "resource" "land" "")) 50){mcfDirection = Input, mcfUnit = "m2a"}
                ms = [mkMethod 2 "Climate change" [cfA], mkMethod 1 "Land use" [cfB]]
            case serializeMethodCollection MethodOpenLcaJsonLd "x" (collection ms) of
                Left err -> expectationFailure (T.unpack err)
                Right (zipBytes, _) -> withSystemTempDirectory "olca-export" $ \dir -> do
                    let path = dir </> "methods.zip"
                    BL.writeFile path zipBytes
                    loaded <- loadMethodCollectionFromConfig (methodConfig path)
                    case loaded of
                        Left err -> expectationFailure ("reload failed: " <> T.unpack err)
                        Right (coll, _) ->
                            sortOn methodId (mcMethods coll)
                                `shouldBe` sortOn methodId (map reimported ms)

    describe "format dispatch (Database.Export)" $ do
        it "accepts the 'openlca' format name" $
            parseMethodExportFormat "openlca" `shouldBe` Right MethodOpenLcaJsonLd

methodConfig :: FilePath -> MethodConfig
methodConfig path =
    MethodConfig
        { mcName = "reload"
        , mcOrigin = MethodFromFile path
        , mcActive = True
        , mcIsUploaded = False
        , mcDescription = Nothing
        , mcFormat = Nothing
        , mcScoringSets = []
        , mcGlobalMethods = []
        , mcPatches = []
        }

isRefused :: Text -> Either Text a -> Bool
isRefused needle result = case result of
    Left err -> needle `T.isInfixOf` err
    Right _ -> False

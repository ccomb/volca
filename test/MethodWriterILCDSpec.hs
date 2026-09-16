{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the ILCD LCIA-method package writer ("Method.WriterILCD").
module MethodWriterILCDSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromWords)
import qualified Data.UUID as UUID
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (MethodConfig (..), MethodOrigin (..))
import Database.Export (MethodExportFormat (..), parseMethodExportFormat, serializeMethodCollection)
import Database.Manager (loadMethodCollectionFromConfig)
import Method.FlowResolver (parseCompartment, parseFlowXML)
import Method.Parser (parseMethodBytesWithFlows)
import Method.Types
import Method.WriterILCD (checkIlcdMethodExportable, compartmentCategories, projectCompartment, serializeIlcdMethodEntries)

-- | A factor whose per-flow unit matches its method's, so it round-trips exactly.
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

-- | Give each flow a distinct UUID from its index, so no two collide.
withRef :: Word -> MethodCF -> MethodCF
withRef n cf = cf{mcfFlowRef = fromWords 0 0 0 (fromIntegral n)}

mkMethod :: Word -> Text -> [MethodCF] -> Method
mkMethod n name cfs =
    Method
        { methodId = mkId n
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = cfs
        }

collection :: [Method] -> MethodCollection
collection ms = MethodCollection ms [] [] []

{- | Re-read the exported package the way the loader does: build the flow
enrichment map from the @flows\/@ entries, then parse each @lciamethods\/@ file
against it. No disk, same enrichment path.
-}
reparse :: [(FilePath, BS.ByteString)] -> Either String [Method]
reparse entries =
    let flowInfo = M.fromList [fi | (p, bytes) <- entries, "flows/" `isPrefixOf` p, Just fi <- [parseFlowXML bytes]]
        methodBytes = [bytes | (p, bytes) <- entries, "lciamethods/" `isPrefixOf` p]
     in traverse (parseMethodBytesWithFlows flowInfo) methodBytes

{- | Normalize for comparison: factors sorted, per-flow unit collapsed to the
method's reference unit (what ILCD stores).
-}
expect :: Method -> Method
expect m = sortFactors m{methodFactors = map (\cf -> cf{mcfUnit = methodUnit m}) (methodFactors m)}

sortFactors :: Method -> Method
sortFactors m = m{methodFactors = sortOn factorOrder (methodFactors m)}
  where
    factorOrder cf = show (mcfFlowRef cf, mcfConsumerLocation cf, mcfDirection cf, mcfValue cf)

spec :: Spec
spec = describe "Method.WriterILCD" $ do
    describe "round-trip through the ILCD method + flow readers" $ do
        it "reproduces each method exactly (values, directions, compartments, CAS, location)" $ do
            let cfA = withRef 1 (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1.0){mcfCAS = Just "124-38-9"}
                cfB = withRef 2 (mkCF "Water" (Just (Compartment "natural resource" "in ground" "")) 42.95){mcfDirection = Input, mcfConsumerLocation = Just "FR"}
                cfC = withRef 3 (mkCF "Occupation, arable" (Just (Compartment "land occupation" "" "")) 50){mcfDirection = Input}
                m =
                    (mkMethod 1 "Water use" [cfA, cfB, cfC])
                        { methodDescription = Just "deprivation-weighted"
                        , methodMethodology = Just "Environmental Footprint"
                        , methodCategory = "EF 3.1"
                        }
            case serializeIlcdMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, warnings) -> do
                    warnings `shouldBe` []
                    reparse entries `shouldBe` Right [expect m]

        it "names the entries lciamethods/<id>.xml and flows/<ref>.xml" $ do
            let cf = withRef 7 (mkCF "Methane" (Just (Compartment "air" "" "")) 29.8)
            case serializeIlcdMethodEntries (collection [mkMethod 1 "Climate change" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) ->
                    map fst entries
                        `shouldBe` [ "flows/" <> UUID.toString (fromWords 0 0 0 7) <> ".xml"
                                   , "lciamethods/" <> UUID.toString (mkId 1) <> ".xml"
                                   ]

        it "write → parse → write is byte-stable" $ do
            let cf = withRef 5 (mkCF "Ammonia" (Just (Compartment "air" "" "")) 3.02)
                m = (mkMethod 3 "Acidification" [cf]){methodMethodology = Just "Environmental Footprint"}
            case serializeIlcdMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, _) -> case reparse entries of
                    Left err -> expectationFailure ("re-parse failed: " <> err)
                    Right ms -> serializeIlcdMethodEntries (collection ms) `shouldBe` Right (entries, [])

        it "keeps a compartment-less flow compartment-less, even when its name mentions a medium" $ do
            -- The reader's shortDescription fallback used to fabricate a
            -- compartment from a name containing "Resources" or "Emissions
            -- to ..."; with a flow file present, the flow file is the authority.
            let cf = withRef 6 (mkCF "Fish, Resources penned" Nothing 2.5)
            case serializeIlcdMethodEntries (collection [mkMethod 1 "Biotic resources" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, warnings) -> do
                    warnings `shouldBe` []
                    case reparse entries of
                        Right [m'] -> map mcfCompartment (methodFactors m') `shouldBe` [Nothing]
                        other -> expectationFailure ("unexpected re-parse: " <> show other)

        it "collapses a per-flow unit to the method reference unit, and says so" $ do
            let cf = withRef 4 (mkCF "Beryllium" (Just (Compartment "water" "" "")) 1.5){mcfUnit = "kg"}
                m = (mkMethod 2 "Toxicity" [cf]){methodUnit = "CTUh"}
            case serializeIlcdMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (entries, warnings) -> do
                    warnings `shouldSatisfy` any (T.isInfixOf "1 characterization factors carry a per-factor flow unit")
                    case reparse entries of
                        Right [m'] -> map mcfUnit (methodFactors m') `shouldBe` ["CTUh"]
                        other -> expectationFailure ("unexpected re-parse: " <> show other)

    describe "compartment inverse" $ do
        it "every real compartment shape round-trips through parseCompartment" $
            mapM_
                (\c -> parseCompartment (compartmentCategories c) `shouldBe` Just (projectCompartment c))
                [ Compartment "air" "" ""
                , Compartment "air" "non-urban air or from high stacks" ""
                , Compartment "water" "surface water" ""
                , Compartment "soil" "agricultural" ""
                , Compartment "natural resource" "" ""
                , Compartment "natural resource" "in ground" ""
                , Compartment "land occupation" "" ""
                , Compartment "land transformation" "" ""
                ]

        it "folds the qualifier into the subcompartment" $ do
            let c = Compartment "water" "groundwater" "long-term"
            projectCompartment c `shouldBe` Compartment "water" "groundwater/long-term" ""
            parseCompartment (compartmentCategories c) `shouldBe` Just (Compartment "water" "groundwater/long-term" "")

    describe "representation-loss warnings" $ do
        it "counts damage, normalization/weighting and scoring sets" $ do
            let m = mkMethod 1 "Climate change" []
                dc = DamageCategory "Human health" "DALY" [("Climate change", 1)]
                nw = NormWeightSet "EF" (M.singleton "Climate change" 1) M.empty
                ss = ScoringSet "EF score" "Pt" M.empty M.empty M.empty M.empty M.empty M.empty Nothing
            case serializeIlcdMethodEntries (MethodCollection [m] [dc] [nw] [ss]) of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> do
                    warnings `shouldSatisfy` any (T.isInfixOf "1 damage categories")
                    warnings `shouldSatisfy` any (T.isInfixOf "1 normalization/weighting sets")
                    warnings `shouldSatisfy` any (T.isInfixOf "1 formula scoring sets")

        it "does not warn about methodology or description, which round-trip natively" $ do
            let cf = withRef 1 (mkCF "Ammonia" (Just (Compartment "air" "" "")) 1)
                m = (mkMethod 1 "A" [cf]){methodMethodology = Just "Environmental Footprint", methodDescription = Just "note"}
            case serializeIlcdMethodEntries (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> warnings `shouldBe` []

    describe "exportability guard" $ do
        it "rejects an empty collection" $
            serializeIlcdMethodEntries (collection []) `shouldSatisfy` isRefused "no methods"

        it "rejects two methods sharing a UUID" $
            serializeIlcdMethodEntries (collection [mkMethod 1 "A" [], mkMethod 1 "B" []])
                `shouldSatisfy` isRefused "share the id"

        it "rejects a method with no name" $
            checkIlcdMethodExportable (collection [mkMethod 1 " " []])
                `shouldSatisfy` isRefused "has no name"

        it "rejects a method with no reference unit" $
            checkIlcdMethodExportable (collection [(mkMethod 1 "A" []){methodUnit = ""}])
                `shouldSatisfy` isRefused "no reference unit"

        it "rejects a method with no impact category" $
            checkIlcdMethodExportable (collection [(mkMethod 1 "A" []){methodCategory = " "}])
                `shouldSatisfy` isRefused "no impact category"

        it "rejects a factor with no flow name" $
            serializeIlcdMethodEntries (collection [mkMethod 1 "A" [withRef 1 (mkCF "" Nothing 1)]])
                `shouldSatisfy` isRefused "no flow name"

        it "rejects a non-finite characterization factor" $
            serializeIlcdMethodEntries (collection [mkMethod 1 "A" [withRef 1 (mkCF "CO2" Nothing (0 / 0))]])
                `shouldSatisfy` isRefused "Non-finite"

        it "rejects one flow UUID carrying two different flow definitions" $ do
            let a = withRef 1 (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1)
                b = withRef 1 (mkCF "Methane" (Just (Compartment "air" "" "")) 2)
            serializeIlcdMethodEntries (collection [mkMethod 1 "A" [a, b]])
                `shouldSatisfy` isRefused "different names"

        it "rejects a compartment the flow categorization cannot represent exactly" $
            serializeIlcdMethodEntries (collection [mkMethod 1 "A" [withRef 1 (mkCF "Zinc" (Just (Compartment "Air" "" "")) 1)]])
                `shouldSatisfy` isRefused "not representable"

    describe "zip archive end to end" $ do
        it "the exported zip loads back through the method-collection loader" $ do
            let cfA = withRef 1 (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1.0){mcfCAS = Just "124-38-9"}
                cfB = withRef 2 (mkCF "Occupation, arable" (Just (Compartment "land occupation" "" "")) 50){mcfDirection = Input}
                ms = [mkMethod 2 "Climate change" [cfA], mkMethod 1 "Land use" [cfB]]
            case serializeMethodCollection MethodIlcdXml "x" (collection ms) of
                Left err -> expectationFailure (T.unpack err)
                Right (zipBytes, _) -> withSystemTempDirectory "ilcd-export" $ \dir -> do
                    let path = dir </> "methods.zip"
                    BL.writeFile path zipBytes
                    loaded <- loadMethodCollectionFromConfig (methodConfig path)
                    case loaded of
                        Left err -> expectationFailure ("reload failed: " <> T.unpack err)
                        Right (coll, _) ->
                            map (sortFactors . expect) (sortOn methodId (mcMethods coll))
                                `shouldBe` map (sortFactors . expect) (sortOn methodId ms)

    describe "format dispatch (Database.Export)" $
        it "accepts the 'ilcd' format name" $
            parseMethodExportFormat "ilcd" `shouldBe` Right MethodIlcdXml

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

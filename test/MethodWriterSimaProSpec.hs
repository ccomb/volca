{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the SimaPro method CSV writer ("Method.WriterSimaPro").
module MethodWriterSimaProSpec (spec) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (nil)
import Test.Hspec

import Database.Export (parseMethodExportFormat)
import Method.ParserSimaPro (isSimaProMethodCSV, parseSimaProMethodCSVBytes)
import Method.SimaProScoring (DamageCategory (..), NormWeightSet (..), SimaProMethodFile (..), simaProCollection)
import Method.Types
import Method.WriterSimaPro (serializeSimaProMethodCSV)
import SimaPro.Writer (defaultWriterConfig)

mkCF :: Text -> Maybe Compartment -> Double -> MethodCF
mkCF name comp v =
    MethodCF
        { mcfFlowRef = nil
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = v
        , mcfCompartment = comp
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkMethod :: Text -> [MethodCF] -> Method
mkMethod name cfs =
    Method
        { methodId = nil
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg eq"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = cfs
        }

collection :: [Method] -> MethodCollection
collection ms = MethodCollection ms [] []

-- | Parse a SimaPro method file into the collection the engine keeps.
parseCollection :: BS.ByteString -> Either String MethodCollection
parseCollection = fmap (fst . simaProCollection) . parseSimaProMethodCSVBytes

{- | A normalization is kept as its inverse and written back inverted, so a
round trip compares it within rounding. Damages come back in the order of
their variable names, the file's own order having no place in a scoring set.
Everything else compares exactly.
-}
withoutNormalization :: SimaProMethodFile -> SimaProMethodFile
withoutNormalization f =
    f
        { smfDamages = sortOn dcName (smfDamages f)
        , smfNWSets = map (\nw -> nw{nwNormalization = M.empty}) (smfNWSets f)
        }

normalizations :: SimaProMethodFile -> [(Text, Text, Double)]
normalizations f = [(nwName nw, k, v) | nw <- smfNWSets f, (k, v) <- M.toList (nwNormalization nw)]

closeTo :: [(Text, Text, Double)] -> [(Text, Text, Double)] -> Bool
closeTo xs ys = length xs == length ys && and (zipWith same xs ys)
  where
    same :: (Text, Text, Double) -> (Text, Text, Double) -> Bool
    same (s1, k1, a) (s2, k2, b) = s1 == s2 && k1 == k2 && abs (a - b) <= 1e-12 * max (abs a) (abs b)

-- | Serialize with a fixed collection name, decoding the bytes for inspection.
serialize :: MethodCollection -> Either Text (Text, [Text])
serialize mc =
    first TE.decodeUtf8
        <$> serializeSimaProMethodCSV defaultWriterConfig "Test collection" mc

spec :: Spec
spec = describe "Method.WriterSimaPro" $ do
    describe "round-trip with the SimaPro method parser" $ do
        it "parse → write → parse reproduces the file, damages and normalization-weighting sets included" $ do
            raw <- BS.readFile "test/data/simapro_method.csv"
            case parseSimaProMethodCSVBytes raw of
                Left err -> expectationFailure ("fixture parse failed: " <> err)
                Right f1 ->
                    case serializeSimaProMethodCSV defaultWriterConfig "fallback" (fst (simaProCollection f1)) of
                        Left err -> expectationFailure ("write failed: " <> T.unpack err)
                        Right (bytes, warnings) -> do
                            warnings `shouldBe` []
                            case parseSimaProMethodCSVBytes bytes of
                                Left err -> expectationFailure ("re-parse failed: " <> err)
                                Right f2 -> do
                                    smfDamages f1 `shouldSatisfy` (not . null)
                                    withoutNormalization f2 `shouldBe` withoutNormalization f1
                                    normalizations f2 `shouldSatisfy` closeTo (normalizations f1)

        it "write → parse → write is byte-stable and self-detecting" $ do
            raw <- BS.readFile "test/data/simapro_method.csv"
            case parseCollection raw of
                Left err -> expectationFailure ("fixture parse failed: " <> err)
                Right c1 ->
                    case serializeSimaProMethodCSV defaultWriterConfig "fallback" c1 of
                        Left err -> expectationFailure ("write failed: " <> T.unpack err)
                        Right (b1, _) -> do
                            isSimaProMethodCSV b1 `shouldBe` True
                            case parseCollection b1 of
                                Left err -> expectationFailure ("re-parse failed: " <> err)
                                Right c2 ->
                                    case serializeSimaProMethodCSV defaultWriterConfig "fallback" c2 of
                                        Left err -> expectationFailure ("re-write failed: " <> T.unpack err)
                                        Right (b2, _) -> b2 `shouldBe` b1

        it "keeps the original file-level Name via the shared methodology" $ do
            raw <- BS.readFile "test/data/simapro_method.csv"
            case parseCollection raw of
                Left err -> expectationFailure ("fixture parse failed: " <> err)
                Right c1 ->
                    case serialize c1 of
                        Left err -> expectationFailure (T.unpack err)
                        Right (out, _) -> out `shouldSatisfy` T.isInfixOf "\r\nTest EF Method\r\n"

        it "a method name containing the delimiter survives the round-trip" $ do
            let m = mkMethod "Ecotoxicity; freshwater" [mkCF "Zinc" (Just (Compartment "water" "" "")) 2.5]
            case serialize (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, _) ->
                    case parseCollection (TE.encodeUtf8 out) of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right c2 -> map methodName (mcMethods c2) `shouldBe` ["Ecotoxicity; freshwater"]

    describe "projections onto SimaPro conventions" $ do
        it "writes a regionalized CF as a name-suffixed substance row" $ do
            let cf = (mkCF "Water" (Just (Compartment "natural resource" "in water" "")) 42.95){mcfDirection = Input, mcfConsumerLocation = Just "FR"}
            case serialize (collection [mkMethod "Water use" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    warnings `shouldBe` []
                    out `shouldSatisfy` T.isInfixOf "Raw;in water;Water, FR;;42.95;kg"
                    case parseCollection (TE.encodeUtf8 out) of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right c2 -> do
                            let cfs = concatMap methodFactors (mcMethods c2)
                            map mcfFlowName cfs `shouldBe` ["Water, FR"]
                            map mcfConsumerLocation cfs `shouldBe` [Nothing]

        it "folds a compartment qualifier into the subcompartment column" $ do
            let cf = mkCF "Arsenic" (Just (Compartment "water" "groundwater" "long-term")) 1.5
            case serialize (collection [mkMethod "Ecotoxicity" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, _) -> out `shouldSatisfy` T.isInfixOf "Water;groundwater, long-term;Arsenic"

        it "files land flows under Raw without a direction warning" $ do
            let occ = (mkCF "Occupation, annual crop" (Just (Compartment "land occupation" "" "")) 50.2){mcfDirection = Input}
                to = mkCF "Transformation, to annual crop" (Just (Compartment "land transformation" "" "")) 1.1
            case serialize (collection [mkMethod "Land use" [occ, to]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    warnings `shouldBe` []
                    out `shouldSatisfy` T.isInfixOf "Raw;(unspecified);Occupation, annual crop"
                    out `shouldSatisfy` T.isInfixOf "Raw;(unspecified);Transformation, to annual crop"

        it "pads a normalized CAS back to SimaPro's 6-digit first segment" $ do
            let cf = (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1){mcfCAS = Just "124-38-9"}
            case serialize (collection [mkMethod "Climate change" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, _) -> out `shouldSatisfy` T.isInfixOf "Air;(unspecified);Carbon dioxide;000124-38-9;1;kg"

    describe "warnings (never silent)" $ do
        it "warns when a CF has no compartment and still emits the row" $ do
            let cf = mkCF "Mystery flow" Nothing 3
            case serialize (collection [mkMethod "Climate change" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    out `shouldSatisfy` T.isInfixOf ";;Mystery flow;;3;kg"
                    warnings `shouldSatisfy` any (T.isInfixOf "without compartment")

        it "warns when the compartment implies the opposite direction" $ do
            let cf = (mkCF "Occupation, arable" (Just (Compartment "air" "" "")) 1){mcfDirection = Input}
            case serialize (collection [mkMethod "Land use" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> warnings `shouldSatisfy` any (T.isInfixOf "direction")

        it "leaves out a scoring set SimaPro cannot hold, with a warning, and writes the rest" $ do
            let ss = ScoringSet "EF score" "Pt" M.empty M.empty M.empty M.empty M.empty M.empty (Just 1000) M.empty DeclaredInConfig
                mc = MethodCollection [mkMethod "Climate change" []] [ss] []
            case serialize mc of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    out `shouldNotSatisfy` T.isInfixOf "EF score"
                    out `shouldSatisfy` T.isInfixOf "Climate change"
                    warnings `shouldSatisfy` any (T.isInfixOf "Scoring set 'EF score' is not exported: SimaPro has no place for a display multiplier.")

    describe "exportability guard" $ do
        it "rejects an empty collection" $
            serialize (collection []) `shouldSatisfy` isRefused "no impact categories"

        it "rejects a non-finite characterization factor" $ do
            let cf = mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) (0 / 0)
            serialize (collection [mkMethod "Climate change" [cf]]) `shouldSatisfy` isRefused "non-finite"

        it "rejects a line break inside a field" $ do
            let cf = mkCF "Carbon\ndioxide" (Just (Compartment "air" "" "")) 1
            serialize (collection [mkMethod "Climate change" [cf]]) `shouldSatisfy` isRefused "line break"

        it "rejects a file-level name that collides with a section marker" $ do
            let m = (mkMethod "Climate change" []){methodMethodology = Just "Impact category"}
            serialize (collection [m]) `shouldSatisfy` isRefused "marker"

        it "rejects a blank normalization-weighting set name" $ do
            -- A blank name line would make the re-import take the next section
            -- keyword as the name and drop that section's factors.
            let nw = NormWeightSet "  " (M.singleton "Climate change" 1.0) M.empty
                mc = fst (simaProCollection (SimaProMethodFile [mkMethod "Climate change" []] [] [nw]))
            serialize mc `shouldSatisfy` isRefused "blank name"

    describe "format dispatch (Database.Export)" $ do
        it "refuses a format without a method writer at parse time" $ do
            case parseMethodExportFormat "xlsx" of
                Left err -> err `shouldSatisfy` T.isInfixOf "unknown method export format"
                Right f -> expectationFailure ("expected a Left, got: " <> show f)

isRefused :: Text -> Either Text a -> Bool
isRefused needle result = case result of
    Left err -> needle `T.isInfixOf` err
    Right _ -> False

{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the columnar CSV method writer ("Method.WriterCSV").
module MethodWriterCSVSpec (spec) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (nil)
import Test.Hspec

import Database.Export (MethodExportFormat (..), parseMethodExportFormat)
import Method.ParserCSV (parseMethodCSVBytes)
import Method.Types
import Method.WriterCSV (serializeColumnarMethodCSV)

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
collection ms = MethodCollection ms [] [] []

-- | Serialize, decoding the bytes for inspection.
serialize :: MethodCollection -> Either Text (Text, [Text])
serialize = fmap (first TE.decodeUtf8) . serializeColumnarMethodCSV

{- | The writer sorts rows by key, so a round-trip preserves the factor
/set/ per method, not the original order. The flow UUID is derived from the
raw compartment cell at parse time, and the writer canonicalizes legacy
prose cells ("Resources" → "natural resource"), so it is regenerated – not
preserved – data; the comparison drops it and keeps everything else.
-}
normalize :: [Method] -> [Method]
normalize =
    map
        ( \m ->
            m
                { methodFactors =
                    sortOn
                        (\cf -> (mcfFlowName cf, show (mcfCompartment cf), mcfValue cf))
                        (map (\cf -> cf{mcfFlowRef = nil}) (methodFactors m))
                }
        )

reparse :: Text -> Either String [Method]
reparse = parseMethodCSVBytes . TE.encodeUtf8

spec :: Spec
spec = describe "Method.WriterCSV" $ do
    describe "round-trip with the columnar parser" $ do
        it "parse → write → parse reproduces the methods (modulo row order)" $ do
            raw <- BS.readFile "test/data/method.csv"
            case parseMethodCSVBytes raw of
                Left err -> expectationFailure ("fixture parse failed: " <> err)
                Right ms1 ->
                    case serializeColumnarMethodCSV (collection ms1) of
                        Left err -> expectationFailure ("write failed: " <> T.unpack err)
                        Right (bytes, warnings) -> do
                            warnings `shouldBe` []
                            case parseMethodCSVBytes bytes of
                                Left err -> expectationFailure ("re-parse failed: " <> err)
                                Right ms2 -> normalize ms2 `shouldBe` normalize ms1

        it "write → parse → write is byte-stable" $ do
            raw <- BS.readFile "test/data/method.csv"
            case parseMethodCSVBytes raw of
                Left err -> expectationFailure ("fixture parse failed: " <> err)
                Right ms1 ->
                    case serializeColumnarMethodCSV (collection ms1) of
                        Left err -> expectationFailure ("write failed: " <> T.unpack err)
                        Right (b1, _) ->
                            case parseMethodCSVBytes b1 of
                                Left err -> expectationFailure ("re-parse failed: " <> err)
                                Right ms2 ->
                                    case serializeColumnarMethodCSV (collection ms2) of
                                        Left err -> expectationFailure ("re-write failed: " <> T.unpack err)
                                        Right (b2, _) -> b2 `shouldBe` b1

        it "a category name containing the delimiter survives via quoting" $ do
            let m = mkMethod "Ecotoxicity; freshwater" [mkCF "Zinc" (Just (Compartment "water" "" "")) 2.5]
            case serialize (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, _) ->
                    case reparse out of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right ms -> map methodName ms `shouldBe` ["Ecotoxicity; freshwater"]

        it "keeps subcompartment and qualifier through the compartment path" $ do
            let cf = mkCF "Arsenic" (Just (Compartment "water" "groundwater" "long-term")) 1.5
            case serialize (collection [mkMethod "Ecotoxicity" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, _) -> do
                    out `shouldSatisfy` T.isInfixOf "Arsenic;water/groundwater/long-term;;kg;1.5"
                    case reparse out of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right ms ->
                            concatMap (map mcfCompartment . methodFactors) ms
                                `shouldBe` [Just (Compartment "water" "groundwater" "long-term")]

        it "keeps a CAS number and a per-row flow unit" $ do
            let a = (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1){mcfCAS = Just "124-38-9"}
                b = (mkCF "Occupation, annual crop" (Just (Compartment "land occupation" "" "")) 50){mcfDirection = Input, mcfUnit = "m2a"}
            case serialize (collection [mkMethod "Mixed" [a, b]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    warnings `shouldBe` []
                    out `shouldSatisfy` T.isInfixOf "Carbon dioxide;air;124-38-9;kg;1"
                    out `shouldSatisfy` T.isInfixOf "Occupation, annual crop;land occupation;;m2a;50"
                    case reparse out of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right ms -> do
                            let cfs = sortOn mcfFlowName (concatMap methodFactors ms)
                            map mcfCAS cfs `shouldBe` [Just "124-38-9", Nothing]
                            map mcfUnit cfs `shouldBe` ["kg", "m2a"]
                            map mcfDirection cfs `shouldBe` [Output, Input]

        it "writes a regionalized CF as a name-suffixed substance row" $ do
            let cf = (mkCF "Water" (Just (Compartment "natural resource" "in water" "")) 42.95){mcfDirection = Input, mcfConsumerLocation = Just "FR"}
            case serialize (collection [mkMethod "Water use" [cf]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    warnings `shouldBe` []
                    out `shouldSatisfy` T.isInfixOf "Water, FR;natural resource/in water;;kg;42.95"
                    case reparse out of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right ms -> do
                            let cfs = concatMap methodFactors ms
                            map mcfFlowName cfs `shouldBe` ["Water, FR"]
                            map mcfConsumerLocation cfs `shouldBe` [Nothing]

        it "keeps duplicate factors for one key as separate rows" $ do
            let cf = mkCF "Chlordane" (Just (Compartment "air" "indoor" ""))
            case serialize (collection [mkMethod "Ecotoxicity" [cf 3.1, cf 6.2]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, _) ->
                    case reparse out of
                        Left err -> expectationFailure ("re-parse failed: " <> err)
                        Right ms -> sortOn id (map mcfValue (concatMap methodFactors ms)) `shouldBe` [3.1, 6.2]

    describe "warnings (never silent)" $ do
        it "warns once, bounded, when directions cannot be re-derived" $ do
            let cf = (mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) 1){mcfDirection = Input}
            case serialize (collection [mkMethod "Uptake" [cf, cf{mcfFlowName = "Methane"}]]) of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> do
                    warnings `shouldSatisfy` any (T.isInfixOf "2 factors have a flow direction")
                    length warnings `shouldBe` 1

        it "warns about descriptions, damage categories, NW sets and scoring sets" $ do
            let m = (mkMethod "Climate change" []){methodDescription = Just "GWP100"}
                dc = DamageCategory "Human health" "DALY" [("Climate change", 1)]
                nw = NormWeightSet "EF" (M.singleton "Climate change" 1) M.empty
                ss = ScoringSet "EF score" "Pt" M.empty M.empty M.empty M.empty M.empty M.empty Nothing
                mc = MethodCollection [m] [dc] [nw] [ss]
            case serialize mc of
                Left err -> expectationFailure (T.unpack err)
                Right (_, warnings) -> do
                    warnings `shouldSatisfy` any (T.isInfixOf "descriptions")
                    warnings `shouldSatisfy` any (T.isInfixOf "damage categories")
                    warnings `shouldSatisfy` any (T.isInfixOf "normalization/weighting")
                    warnings `shouldSatisfy` any (T.isInfixOf "scoring sets")

        it "warns when methodologies differ and omits the comment" $ do
            let m1 = (mkMethod "A" []){methodMethodology = Just "EF"}
                m2 = (mkMethod "B" []){methodMethodology = Just "ReCiPe"}
            case serialize (collection [m1, m2]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    out `shouldNotSatisfy` T.isInfixOf "# methodology"
                    warnings `shouldSatisfy` any (T.isInfixOf "methodologies")

        it "warns when a stated methodology sits next to an absent one" $ do
            -- An absent methodology is not a distinct one, but it blocks the
            -- shared comment – the one stated methodology is still lost.
            let m1 = (mkMethod "A" []){methodMethodology = Just "EF"}
                m2 = mkMethod "B" []
            case serialize (collection [m1, m2]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    out `shouldNotSatisfy` T.isInfixOf "# methodology"
                    warnings `shouldSatisfy` any (T.isInfixOf "1 distinct stated methodologies")

        it "writes the shared methodology as the file comment" $ do
            let m = (mkMethod "A" []){methodMethodology = Just "Environmental Footprint"}
            case serialize (collection [m]) of
                Left err -> expectationFailure (T.unpack err)
                Right (out, warnings) -> do
                    warnings `shouldBe` []
                    out `shouldSatisfy` T.isPrefixOf "# methodology: Environmental Footprint\n"

    describe "exportability guard" $ do
        it "rejects an empty collection" $
            serialize (collection []) `shouldSatisfy` isRefused "no impact categories"

        it "rejects a blank category name" $
            serialize (collection [mkMethod "  " []]) `shouldSatisfy` isRefused "blank name"

        it "rejects a non-finite characterization factor" $ do
            let cf = mkCF "Carbon dioxide" (Just (Compartment "air" "" "")) (0 / 0)
            serialize (collection [mkMethod "Climate change" [cf]]) `shouldSatisfy` isRefused "non-finite"

        it "rejects a line break inside a field" $ do
            let cf = mkCF "Carbon\ndioxide" (Just (Compartment "air" "" "")) 1
            serialize (collection [mkMethod "Climate change" [cf]]) `shouldSatisfy` isRefused "line break"

        it "rejects a compartment segment containing the path separator" $ do
            let cf = mkCF "Zinc" (Just (Compartment "water" "ground/deep" "")) 1
            serialize (collection [mkMethod "Ecotoxicity" [cf]]) `shouldSatisfy` isRefused "path separator"

        it "rejects a compartment the parser cannot read back" $ do
            let cf = mkCF "Zinc" (Just (Compartment "economic" "" "")) 1
            serialize (collection [mkMethod "Ecotoxicity" [cf]]) `shouldSatisfy` isRefused "outside the ones"

    describe "format dispatch (Database.Export)" $ do
        it "accepts the 'csv' format name" $
            parseMethodExportFormat "csv" `shouldBe` Right MethodColumnarCSV

isRefused :: Text -> Either Text a -> Bool
isRefused needle result = case result of
    Left err -> needle `T.isInfixOf` err
    Right _ -> False

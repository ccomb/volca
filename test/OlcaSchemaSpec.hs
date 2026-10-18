{-# LANGUAGE OverloadedStrings #-}

module OlcaSchemaSpec (spec) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnitConversion (defaultUnitConfig)

import Database.Manager (loadMethodCollectionFromPath)
import Method.Mapping (MethodTables (..), buildMethodTables)
import Method.Parser.OlcaSchema (OlcaImpactMethod (..), choosePackageMethod, isOlcaImpactCategoryJson, parseOlcaImpactCategoryBytes, parseOlcaImpactMethodBytes)
import Method.SimaProScoring (NormWeightSet (..))
import Method.Types

spec :: Spec
spec = do
    describe "isOlcaImpactCategoryJson" $ do
        it "recognizes an openLCA ImpactCategory" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            isOlcaImpactCategoryJson bytes `shouldBe` True

        it "rejects unrelated JSON" $ do
            isOlcaImpactCategoryJson "{\"foo\": 1}" `shouldBe` False
            isOlcaImpactCategoryJson "[1,2,3]" `shouldBe` False
            isOlcaImpactCategoryJson "not json" `shouldBe` False

        it "rejects another openLCA entity type" $
            -- The auto-detection must not pull in Process / Flow / etc. files
            -- that may sit in the same method directory.
            isOlcaImpactCategoryJson "{\"@type\":\"Process\",\"name\":\"x\"}"
                `shouldBe` False

    describe "parseOlcaImpactCategoryBytes" $ do
        it "parses the mini fixture and yields one MethodCF per ImpactFactor" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    methodName method `shouldBe` "Regional LCIA Mini"
                    methodUnit method `shouldBe` "m2*year"
                    length (methodFactors method) `shouldBe` 4

        it "preserves location code, value, and flow UUID on each cell" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    let factors = methodFactors method
                        landFR =
                            head
                                [ f
                                | f <- factors
                                , mcfFlowName f == "Occupation, agriculture"
                                , mcfConsumerLocation f == Just "FR"
                                ]
                        landGLO =
                            head
                                [ f
                                | f <- factors
                                , mcfFlowName f == "Occupation, agriculture"
                                , mcfConsumerLocation f == Just "GLO"
                                ]
                    mcfValue landFR `shouldBe` 22.15
                    mcfValue landGLO `shouldBe` 10.0
                    -- The fixture's flow @id round-trips into mcfFlowRef
                    UUID.toText (mcfFlowRef landFR)
                        `shouldBe` "0305b169-255d-4041-8f5d-6e095bcb6358"

        it "reads the document-level category as the group label" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"Acidification\",\
                    \\"category\":\"EF 3.1\",\"referenceUnitName\":\"mol H+ eq\",\
                    \\"impactFactors\":[]}"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> methodCategory method `shouldBe` "EF 3.1"

        it "falls back to the name when category is absent" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> methodCategory method `shouldBe` methodName method

        it "reads the olca-schema refUnit spelling of the reference unit" $ do
            -- A genuine openLCA export writes `refUnit`; VoLCA's own exports
            -- write `referenceUnitName`. Both must land in methodUnit.
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\
                    \\"refUnit\":\"kg CO2 eq\",\"impactFactors\":[]}"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> methodUnit method `shouldBe` "kg CO2 eq"

        it "rejects a non-object top level" $
            case parseOlcaImpactCategoryBytes "[1,2,3]" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse failure on array root"

        it "rejects a wrong @type" $
            case parseOlcaImpactCategoryBytes "{\"@type\":\"Process\",\"name\":\"x\"}" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse failure on @type=Process"

    describe "parseImpactFactor flow.category → Compartment" $ do
        -- Regression gate for d8a054d & 3d2f7e5: openLCA flows with identical
        -- 'name' values but different category paths (e.g. Agribalyse has 3
        -- "Occupation, annual crop" flows under resource, resource/land and
        -- resource/biotic) must surface their full compartment path so the
        -- DB-flow matcher can disambiguate. Without this, only one of the
        -- three is reachable and ~30% of regionalized factors miss their
        -- target flow silently.
        it "extracts (medium, subcompartment) from a slash-separated path" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\"referenceUnitName\":\"u\",\
                    \\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,\
                    \\"flow\":{\"@type\":\"Flow\",\"name\":\"Occupation, annual crop\",\
                    \\"category\":{\"@type\":\"Category\",\"name\":\"resource/land\"}}}]}"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method ->
                    case methodFactors method of
                        [cf] -> mcfCompartment cf `shouldBe` Just (Compartment "resource" "land" "")
                        _ -> expectationFailure "expected exactly one factor"

        it "keeps deeper subpaths intact (e.g. resource/in air / long-term)" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\"referenceUnitName\":\"u\",\
                    \\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,\
                    \\"flow\":{\"@type\":\"Flow\",\"name\":\"f\",\
                    \\"category\":{\"@type\":\"Category\",\"name\":\"resource/in air/upper stratosphere\"}}}]}"
            case parseOlcaImpactCategoryBytes bytes of
                Right method ->
                    case methodFactors method of
                        [cf] ->
                            mcfCompartment cf
                                `shouldBe` Just (Compartment "resource" "in air/upper stratosphere" "")
                        _ -> expectationFailure "expected exactly one factor"
                Left err -> expectationFailure ("parse failed: " ++ err)

        it "leaves mcfCompartment Nothing when the flow has no category" $ do
            -- The mini fixture has no category fields, so Compartment must
            -- stay 'Nothing' – the matcher falls back to the legacy name-only
            -- path. Regression gate against the disambiguation breaking
            -- non-openLCA / non-Agribalyse methods that ship without category.
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method ->
                    map mcfCompartment (methodFactors method)
                        `shouldBe` replicate (length (methodFactors method)) Nothing

        it "single-segment category resolves to medium with empty subcompartment" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\"referenceUnitName\":\"u\",\
                    \\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,\
                    \\"flow\":{\"@type\":\"Flow\",\"name\":\"f\",\
                    \\"category\":{\"@type\":\"Category\",\"name\":\"air\"}}}]}"
            case parseOlcaImpactCategoryBytes bytes of
                Right method ->
                    case methodFactors method of
                        [cf] -> mcfCompartment cf `shouldBe` Just (Compartment "air" "" "")
                        _ -> expectationFailure "expected exactly one factor"
                Left err -> expectationFailure ("parse failed: " ++ err)

        it "reads the olca-schema string form of the category" $
            -- A genuine openLCA export carries the category as a plain string
            -- on the flow Ref, not as a Category Ref object.
            factorCompartments (directionDocStr "" "" "Emission to air/urban")
                `shouldBe` Right [Just (Compartment "Emission to air" "urban" "")]

        it "drops the Elementary flows category-tree root" $
            factorCompartments (directionDocStr "" "" "Elementary flows/Resource/in ground")
                `shouldBe` Right [Just (Compartment "Resource" "in ground" "")]

        it "a path that is only the tree root leaves no compartment" $
            factorCompartments (directionDocStr "" "" "Elementary flows")
                `shouldBe` Right [Nothing]

    describe "factor direction derivation" $ do
        -- Real openLCA files carry no per-factor direction field; a resource
        -- CF mis-defaulted to Output resolves against the output synonym
        -- view and silently loses input-only bridges. The signals below are
        -- tried most-specific first.
        it "the factor's own direction field wins over the category path" $
            factorDirections
                (directionDoc "" "\"direction\":\"OUTPUT\"," "natural resource/in water")
                `shouldBe` Right [Output]

        it "a resource category path means Input" $
            factorDirections (directionDoc "" "" "natural resource/in water")
                `shouldBe` Right [Input]

        it "recognizes the resource segment behind openLCA's category-tree root" $
            factorDirections (directionDoc "" "" "Elementary flows/Resource/in ground")
                `shouldBe` Right [Input]

        it "reads the olca-schema string form of the category path" $
            factorDirections (directionDocStr "" "" "Elementary flows/Resource/in ground")
                `shouldBe` Right [Input]

        it "reads the prose resource spellings of the columnar parser" $
            factorDirections (directionDoc "" "" "Resources from ground")
                `shouldBe` Right [Input]

        it "an emission category path stays Output" $
            factorDirections (directionDoc "" "" "Emission to air/urban")
                `shouldBe` Right [Output]

        it "falls back to the document-level direction when the path says nothing" $
            factorDirections (directionDoc "\"direction\":\"INPUT\"," "" "water")
                `shouldBe` Right [Input]

        it "a resource category path beats the document-level direction" $
            factorDirections (directionDoc "\"direction\":\"OUTPUT\"," "" "resource/land")
                `shouldBe` Right [Input]

        it "an emission category path beats the document-level direction" $
            -- A water-use category oriented Input at the document level still
            -- has release factors filed under Emission to water – they must
            -- stay Output.
            factorDirections (directionDoc "\"direction\":\"INPUT\"," "" "Emission to water/unspecified")
                `shouldBe` Right [Output]

        it "defaults to Output with no signal at all" $
            factorDirections (directionDoc "" "" "water")
                `shouldBe` Right [Output]

    describe "a method package" $ do
        -- openLCA divides a result by the normalisation factor and multiplies it
        -- by the weight; a zero factor is one openLCA ships for a category it
        -- does not normalise, and must not become a division by zero.
        it "reads a method's categories and its normalisation as a divisor" $
            case parseOlcaImpactMethodBytes (methodDoc "EF 3.1" "acid" [("Acidification", 2, 0.5), ("Water use", 0, 0.1)]) of
                Left err -> expectationFailure err
                Right m -> do
                    oimCategories m `shouldBe` ["acid"]
                    map nwNormalization (oimNWSets m) `shouldBe` [M.singleton "Acidification" 0.5]
                    map nwWeighting (oimNWSets m) `shouldBe` [M.fromList [("Acidification", 0.5), ("Water use", 0.1)]]

        it "names the methods it holds when it is not told which to read" $ do
            let held = [OlcaImpactMethod "EF 3.1" [] [], OlcaImpactMethod "IPCC 2021" [] []]
            oimName <$> choosePackageMethod (Just "IPCC 2021") held `shouldBe` Right "IPCC 2021"
            oimName <$> choosePackageMethod Nothing (take 1 held) `shouldBe` Right "EF 3.1"
            choosePackageMethod Nothing held `shouldSatisfy` refusedWith "\"EF 3.1\", \"IPCC 2021\""
            choosePackageMethod (Just "ReCiPe") held `shouldSatisfy` refusedWith "no method named \"ReCiPe\""

        it "loads the method a configuration names, with its single score" $
            withSystemTempDirectory "olca-package" $ \root -> do
                createDirectoryIfMissing True (root </> "lcia_methods")
                createDirectoryIfMissing True (root </> "lcia_categories")
                BS.writeFile (root </> "lcia_methods" </> "ef.json") (methodDoc "EF 3.1" "acid" [("Acidification", 2, 0.5)])
                BS.writeFile (root </> "lcia_methods" </> "other.json") (methodDoc "Other" "ozone" [])
                BS.writeFile (root </> "lcia_categories" </> "acid.json") (categoryDoc "Acidification")
                BS.writeFile (root </> "lcia_categories" </> "ozone.json") (categoryDoc "Ozone depletion")
                loaded <- loadMethodCollectionFromPath (Just "EF 3.1") root
                case loaded of
                    Left err -> expectationFailure (T.unpack err)
                    Right (collection, _) -> do
                        map methodName (mcMethods collection) `shouldBe` ["Acidification"]
                        map ssName (mcScoringSets collection) `shouldBe` ["EF 3.1 weighting"]
                unnamed <- loadMethodCollectionFromPath Nothing root
                unnamed `shouldSatisfy` refusedWith "several methods"

    describe "buildMethodTables on parsed openLCA methods" $ do
        it "leaves mtRegionalizedCF empty when no flow matched (Nothing in mappings)" $ do
            -- Without database flows to match against, every CF stays unmapped, so
            -- the regionalized table is empty (it only indexes successfully-matched
            -- cells). Documents the contract: regional indexing requires a DB match
            -- by name/UUID/CAS/synonym first.
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    let mappings = [(cf, Nothing) | cf <- methodFactors method]
                        tables = buildMethodTables mempty defaultUnitConfig M.empty mappings
                    M.size (mtRegionalizedCF tables) `shouldBe` 0

{- | A one-factor ImpactCategory document: @docFields@ / @factorFields@ are
raw JSON fragments (trailing comma when non-empty), @categoryPath@ the
flow's category name, carried as a Category Ref object (VoLCA's own
export shape).
-}
directionDoc :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
directionDoc docFields factorFields categoryPath =
    olcaDoc docFields factorFields ("{\"@type\":\"Category\",\"name\":\"" <> categoryPath <> "\"}")

-- | Same document with the category as the olca-schema plain string.
directionDocStr :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
directionDocStr docFields factorFields categoryPath =
    olcaDoc docFields factorFields ("\"" <> categoryPath <> "\"")

olcaDoc :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
olcaDoc docFields factorFields categoryJson =
    "{\"@type\":\"ImpactCategory\",\"name\":\"D\","
        <> docFields
        <> "\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,"
        <> factorFields
        <> "\"flow\":{\"@type\":\"Flow\",\"name\":\"f\",\"category\":"
        <> categoryJson
        <> "}}]}"

-- | The direction of every parsed factor of the given document.
factorDirections :: BS.ByteString -> Either String [FlowDirection]
factorDirections bytes =
    map mcfDirection . methodFactors <$> parseOlcaImpactCategoryBytes bytes

-- | An ImpactMethod grouping one category, with one set of (category, normalisation, weight).
methodDoc :: T.Text -> T.Text -> [(T.Text, Double, Double)] -> BS.ByteString
methodDoc name category factors =
    BL.toStrict . encode $
        object
            [ "@type" .= ("ImpactMethod" :: T.Text)
            , "name" .= name
            , "impactCategories" .= [object ["@type" .= ("ImpactCategory" :: T.Text), "@id" .= category]]
            , "nwSets"
                .= [ object
                        [ "name" .= (name <> " weighting")
                        , "factors"
                            .= [ object ["impactCategory" .= object ["name" .= c], "normalisationFactor" .= n, "weightingFactor" .= w]
                               | (c, n, w) <- factors
                               ]
                        ]
                   | not (null factors)
                   ]
            ]

-- | An ImpactCategory with one factor on carbon dioxide.
categoryDoc :: T.Text -> BS.ByteString
categoryDoc name =
    BL.toStrict . encode $
        object
            [ "@type" .= ("ImpactCategory" :: T.Text)
            , "name" .= name
            , "impactFactors"
                .= [object ["value" .= (1 :: Double), "flow" .= object ["name" .= ("Carbon dioxide" :: T.Text), "category" .= ("Elementary flows/Emission to air/unspecified" :: T.Text)]]]
            ]

refusedWith :: T.Text -> Either T.Text a -> Bool
refusedWith needle = either (needle `T.isInfixOf`) (const False)

-- | The compartment of every parsed factor of the given document.
factorCompartments :: BS.ByteString -> Either String [Maybe Compartment]
factorCompartments bytes =
    map mcfCompartment . methodFactors <$> parseOlcaImpactCategoryBytes bytes

{-# LANGUAGE OverloadedStrings #-}

module EcoSpold2Spec (spec) where

import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import EcoSpold.Common (ParsedDataset (..))
import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import Progress (LogLine (llText), getLogLines)
import Types

{- | The bundled fixture has a `<comment xml:lang="en">...</comment>` on each
of its four exchanges (1 input, 1 reference output, 2 emissions).
-}
withFixture :: (ParsedDataset -> IO ()) -> IO ()
withFixture k = do
    result <- streamParseActivityAndFlowsFromFile "test-data/electricity-production.spold"
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right res -> k res

spec :: Spec
spec = describe "per-exchange comments" $ do
    it "captures English <comment> on intermediateExchange and elementaryExchange" $
        withFixture $ \ParsedDataset{pdActivity = act} ->
            map exchangeComment (exchanges act)
                `shouldMatchList` [ Just "Coal input for electricity generation"
                                  , Just "Electricity output (reference product)"
                                  , Just "CO2 emission from coal combustion"
                                  , Just "SO2 emission from coal combustion"
                                  ]

    it "preserves all four exchanges" $
        withFixture $
            \ParsedDataset{pdActivity = act} -> length (exchanges act) `shouldBe` 4

    it "comments contain no &-entity artefacts" $
        withFixture $ \ParsedDataset{pdActivity = act} ->
            let comments = [c | ex <- exchanges act, Just c <- [exchangeComment ex]]
             in not (any (T.isInfixOf "&") comments) `shouldBe` True

    -- Critical correctness test: <property> children of an exchange may carry
    -- their own <comment> describing the property (e.g. "dry mass on a kg
    -- basis"). Those must NOT be attributed to the exchange itself.
    it "ignores <comment> nested inside <property> children of an exchange" $ do
        result <- streamParseActivityAndFlowsFromFile "test-data/sawnwood-properties_12345678-1234-5678-9abc-12345678aaaa.spold"
        case result of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right ParsedDataset{pdActivity = act} ->
                map exchangeComment (exchanges act)
                    `shouldMatchList` [ Nothing -- ex1 has no top-level comment, only property comments
                                      , Just "Adhesive applied during pressing" -- ex2's exchange-level comment, NOT the noisy property comment
                                      ]

    {- The same fixture, read for what those <property> children say rather
    than for what they must not leak. A property is recorded per unit of the
    exchange, as the file states it: the glue line is 0.1 kg and declares 1.0,
    so 1.0 is what is recorded and 0.1 would mean the record had been made to
    depend on the amount beside it. Its unit is kept as written, so whoever
    needs a mass can refuse a dimensionless quantity rather than read
    kilograms into it. "carbon content" names no property this engine holds
    and is dropped rather than guessed at. -}
    describe "properties an exchange states" $ do
        it "reads the dry mass a product declares" $
            withPropertyFixture $ \act ->
                propertiesOf "22222222-2222-2222-2222-222222222222" act
                    `shouldBe` Just noProperties{epDryMass = Just (StatedAmount "kg" 614.4)}

        it "records the property per unit of the line, not per line" $
            withPropertyFixture $ \act ->
                propertiesOf "55555555-5555-5555-5555-555555555555" act
                    `shouldBe` Just noProperties{epDryMass = Just (StatedAmount "dimensionless" 1.0)}

        it "leaves the exchange amounts exactly as the file states them" $
            withPropertyFixture $ \act ->
                map exchangeAmount (exchanges act) `shouldMatchList` [1.0, 0.1]

    -- The waste axis has exactly one EcoSpold2 marker: an intermediateExchange
    -- classified By-product classification=Waste. Elementary exchanges stay
    -- biosphere whatever their compartment reads - a flow in the "inventory
    -- indicator" compartment is an accounting total a method characterizes, not
    -- a demand. Routing it to the waste axis both hid it from every LCIA method
    -- and, since such a flow is commonly written with inputGroup 4, invented a
    -- supplier demand no activity can meet.
    describe "waste flow detection" $ do
        it "keeps an elementary 'inventory indicator/waste' input on the biosphere axis" $
            withWastePatternsFixture $ \ParsedDataset{pdActivity = act, pdBioFlows = bios, pdWasteFlows = wastes} -> do
                length [e | e@WasteExchange{} <- exchanges act] `shouldBe` 1
                length wastes `shouldBe` 1
                -- the indicator input joins the genuine CO2 emission
                map bfName bios
                    `shouldMatchList` ["Carbon dioxide", "Waste mass placed in landfill"]
        -- The fixture writes the indicator with inputGroup 4, which would
        -- otherwise read as Resource. A source writes the same indicator under
        -- both groups from one dataset to the next, so the group says nothing;
        -- recording one direction is what lets a writer that reconstructs
        -- direction from the compartment round-trip the flow.
        it "records an inventory indicator as an output whichever group it carries" $
            withWastePatternsFixture $ \ParsedDataset{pdActivity = act} ->
                [bioDirection e | e@BiosphereExchange{} <- exchanges act]
                    `shouldBe` [Emission, Emission]
        it "routes an intermediate classified 'By-product:Waste' to WasteExchange" $
            withWastePatternsFixture $ \ParsedDataset{pdActivity = act} -> do
                let wasteInputs = [e | e@WasteExchange{waIsInput = True} <- exchanges act]
                    wasteOutputs = [e | e@WasteExchange{waIsInput = False} <- exchanges act]
                length wasteInputs `shouldBe` 1
                length wasteOutputs `shouldBe` 0

    -- Regression: e17dc21 established "all 25,412 ecoinvent .spold parse"; the
    -- WasteFlow axis (#83) silently re-broke it. A treatment / market-for-waste
    -- activity's reference flow is itself waste (negative amount, outputGroup="0",
    -- By-product classification=Waste). Routing it to the waste axis leaves the
    -- activity reference-less, so applyCutoffStrategy drops it (≈4,619 ecoinvent
    -- datasets) and every input into the treatment subsystem silently goes
    -- unresolved. The reference must stay the technosphere reference product.
    describe "waste-classified reference flow (treatment / market-for-waste)" $ do
        it "does not drop the activity" $ do
            result <- parseWasteReference
            case result of
                Left err -> expectationFailure $ "activity was dropped: " ++ err
                Right _ -> pure ()

        it "keeps the reference on the technosphere axis, not the waste axis" $
            withWasteReferenceFixture $ \ParsedDataset{pdActivity = act, pdTechFlows = techs, pdWasteFlows = wastes} -> do
                [techRole e | e@TechnosphereExchange{} <- exchanges act] `shouldBe` [ReferenceProduct]
                length [() | WasteExchange{} <- exchanges act] `shouldBe` 0
                length techs `shouldBe` 1 -- reference registered as a TechnosphereFlow
                length wastes `shouldBe` 0 -- and not as a WasteFlow
        it "carries the reference unit through (not UNKNOWN_UNIT)" $
            withWasteReferenceFixture $ \ParsedDataset{pdActivity = act} ->
                activityUnit act `shouldBe` "kg"

        it "preserves the negative reference amount for the matrix diagonal" $
            withWasteReferenceFixture $ \ParsedDataset{pdActivity = act} ->
                [exchangeAmount e | e@TechnosphereExchange{techRole = ReferenceProduct} <- exchanges act]
                    `shouldBe` [-1.0]

    -- -----------------------------------------------------------------------
    -- Robustness: malformed input should never crash; we expect a clean Left.
    -- The byte-level fuzzing inputs are the ones the parser actually sees in
    -- the wild when an upload is truncated or a file is mis-extended.
    -- -----------------------------------------------------------------------
    describe "malformed input — returns Left without crashing" $ do
        let runOnBytes bytes = withSystemTempDirectory "es2-bad" $ \dir -> do
                let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
                BS.writeFile path bytes
                streamParseActivityAndFlowsFromFile path
        let shouldBeLeft res = case res of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a Left for malformed input"

        it "returns Left on an empty file" $
            runOnBytes "" >>= shouldBeLeft

        it "returns Left on a stray non-XML byte sequence" $
            runOnBytes "this is not xml at all" >>= shouldBeLeft

        it "returns Left on truncated XML (unclosed tag)" $
            runOnBytes "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\"><activityDataset>" >>= shouldBeLeft

        it "returns Left on well-formed XML that is not an EcoSpold dataset" $
            runOnBytes "<?xml version=\"1.0\"?><root><child>hello</child></root>" >>= shouldBeLeft

    -- -----------------------------------------------------------------------
    -- Native activity-type capture: ecospold2's <activity activityType="…"
    -- specialActivityType="…"> attributes are the authoritative discriminator
    -- between markets, ordinary transforming activities, market groups, etc.
    -- We expose them verbatim with the spec's documented labels.
    -- -----------------------------------------------------------------------
    describe "native activityType / specialActivityType capture" $ do
        let runOnBytes bytes = withSystemTempDirectory "es2-attr" $ \dir -> do
                let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
                BS.writeFile path bytes
                streamParseActivityAndFlowsFromFile path

        it "captures activityType=2 as Market activity with code+label" $ do
            result <- runOnBytes (activityTypeFixtureXml "2" (Just "1"))
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right ParsedDataset{pdActivity = act} ->
                    activityNativeType act
                        `shouldBe` Just
                            EcoSpoldActivityType
                                { eatCode = 2
                                , eatLabel = "Market activity"
                                , eatSpecialCode = Just 1
                                , eatSpecialLabel = Just "Hard link"
                                }

        it "captures activityType=1 as Ordinary transforming activity, no special" $ do
            result <- runOnBytes (activityTypeFixtureXml "1" Nothing)
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right ParsedDataset{pdActivity = act} ->
                    activityNativeType act
                        `shouldBe` Just
                            EcoSpoldActivityType
                                { eatCode = 1
                                , eatLabel = "Ordinary transforming activity"
                                , eatSpecialCode = Nothing
                                , eatSpecialLabel = Nothing
                                }

        it "returns Nothing when no activityType attribute is present" $ do
            result <- runOnBytes wastePatternsXml -- existing fixture has no activityType
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right ParsedDataset{pdActivity = act} ->
                    activityNativeType act `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- mathematicalRelation formulas: <parameter> variables plus exchange
    -- variableNames form a dataset-local environment; an exchange's
    -- mathematicalRelation is checked against it as a consistency control.
    -- The stored amount always stays authoritative — the check's outcome is
    -- recorded on the activity for the quality report, never logged, never
    -- changes a number, never crashes.
    -- -----------------------------------------------------------------------
    describe "mathematicalRelation formulas" $ do
        it "keeps the stored amount when the formula evaluates to a different value" $
            withFormulaFixture $ \ParsedDataset{pdActivity = act} ->
                -- fuel_input(2.0) * 2 + production(1.0) = 5.0 diverges from the
                -- stored 4.0; the stored amount wins (the recorded divergence is
                -- asserted below, proving the nested <property>'s own
                -- mathematicalRelation "9999" did not leak into the evaluation).
                [exchangeAmount e | e@TechnosphereExchange{techRole = Input} <- exchanges act]
                    `shouldBe` [4.0]

        it "stores <parameter> values and raw formulas on the activity" $
            withFormulaFixture $ \ParsedDataset{pdActivity = act} -> do
                activityParams act `shouldBe` M.fromList [("fuel_input", 2.0)]
                activityParamExprs act `shouldBe` M.fromList [("fuel_input", "4.0 / 2")]

        it "keeps the stored amount when a formula references an unknown variable" $
            withFormulaFixture $ \ParsedDataset{pdActivity = act} ->
                [exchangeAmount e | e@BiosphereExchange{} <- exchanges act]
                    `shouldBe` [3.0]

        it "does not keep a <parameter> without a usable amount" $
            withFormulaFixture $ \ParsedDataset{pdActivity = act} ->
                M.member "ghost" (activityParams act) `shouldBe` False

        it "records the check outcome on the activity, with the divergent example" $
            withFormulaFixture $ \ParsedDataset{pdActivity = act} ->
                case activityFormulaCheck act of
                    Nothing -> expectationFailure "expected a FormulaCheck on the activity"
                    Just fc -> do
                        fcEvaluated fc `shouldBe` 1
                        fcDivergent fc `shouldBe` 1
                        fcUnevaluable fc `shouldBe` 1
                        fcExample fc `shouldBe` Just "\"fuel_input * 2 + production\" evaluates to 5.0 but the dataset stores 4.0"

        it "logs the dropped parameter but nothing about the formulas" $ do
            (since, _) <- getLogLines 0
            withFormulaFixture $ \_ -> pure ()
            (_, newLines) <- getLogLines since
            let newTexts = map llText newLines
            any ("Ignoring <parameter> \"ghost\"" `isInfixOf`) newTexts
                `shouldBe` True
            any ("mathematicalRelation" `isInfixOf`) newTexts
                `shouldBe` False

    -- A dataset without a <geography> element gets "GLO" as a stand-in; the
    -- source declared nothing, and the record keeps that distinction for the
    -- quality report.
    describe "geography stand-in" $ do
        let runOnBytes bytes = withSystemTempDirectory "es2-geo" $ \dir -> do
                let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
                BS.writeFile path bytes
                streamParseActivityAndFlowsFromFile path

        it "records a geography the dataset declares as declared" $ do
            result <- runOnBytes (activityTypeFixtureXml "1" Nothing)
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right ParsedDataset{pdActivity = act} -> do
                    activityLocation act `shouldBe` "TEST"
                    activityLocationSource act `shouldBe` LocationDeclared

        it "fills in GLO for a dataset with no geography, recorded as undeclared" $ do
            result <- runOnBytes noGeographyXml
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right ParsedDataset{pdActivity = act} -> do
                    activityLocation act `shouldBe` "GLO"
                    activityLocationSource act `shouldBe` LocationUnspecified

    describe "placeholders the reader stood in for" $ do
        let runOnBytes bytes = withSystemTempDirectory "es2-placeholder" $ \dir -> do
                let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
                BS.writeFile path bytes
                streamParseActivityAndFlowsFromFile path
            -- The fixtures name their unit "unit-kg", which is not a UUID, so
            -- each reading also remarks on that. Only the stand-ins are read here.
            standIns = filter (T.isInfixOf "read as") . pdWarnings

        it "says which fields it stood in for" $ do
            result <- runOnBytes namelessXml
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right parsed -> do
                    activityName (pdActivity parsed) `shouldBe` "Unknown Activity"
                    activityUnit (pdActivity parsed) `shouldBe` "UNKNOWN_UNIT"
                    standIns parsed
                        `shouldBe` [ "no activity name, read as \"Unknown Activity\""
                                   , "no reference unit, read as \"UNKNOWN_UNIT\""
                                   ]

        it "has nothing to say about a dataset that named both" $ do
            result <- runOnBytes (activityTypeFixtureXml "1" Nothing)
            case result of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right parsed -> standIns parsed `shouldBe` []

    describe "dataset documentation" $ do
        let sectionNamed label act = lookup label [(docLabel s, docText s) | s <- activityDocumentation act]
            onDocumented k = withSystemTempDirectory "es2-docs" $ \dir -> do
                let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
                BS.writeFile path documentedXml
                result <- streamParseActivityAndFlowsFromFile path
                case result of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right ParsedDataset{pdActivity = act} -> k act

        it "reads the sections of a dataset whose general comment runs to several paragraphs" $
            -- The shape four ecoinvent datasets in five have. Each paragraph of
            -- a general comment used to leave its <text> on the element path,
            -- so every later <text> in the file read as more general comment.
            onDocumented $ \act -> do
                activityDescription act
                    `shouldBe` [ "The dataset represents the construction of one port."
                               , "Its life time is assumed to be 100 years."
                               ]
                sectionNamed "Technology" act
                    `shouldBe` Just "Conditions at the Port of Rotterdam.\nMaterial composition from Maibach et al."

        it "assembles the published source from the three attributes it is spread over" $
            onDocumented $ \act ->
                sectionNamed "Published in" act `shouldBe` Just "Spielmann M. (2007), Water Transport"

        it "reads a comment through its <text> children, in English and in order" $
            onDocumented $ \act ->
                sectionNamed "Technology" act
                    `shouldBe` Just "Conditions at the Port of Rotterdam.\nMaterial composition from Maibach et al."

        it "keeps the English of a field the dataset repeats per language" $
            onDocumented $ \act -> do
                -- The German repeat comes second: taking the last one read
                -- would answer "Literaturstudien.", and an empty repeat would
                -- erase the rubric altogether.
                sectionNamed "Sampling procedure" act `shouldBe` Just "Literature studies."
                sectionNamed "Extrapolations" act `shouldBe` Just "none"

        it "keeps what a person signed, whether or not they wrote anything, and drops the checker's report" $
            -- The [System] review writes its log in <details> like a person's,
            -- and the second person signed without writing: neither the shape
            -- nor the presence of a text tells the two apart, only the name.
            onDocumented $ \act ->
                sectionNamed "Review" act
                    `shouldBe` Just
                        "Carl Vadenbo (2012-06-29): The amounts of the exchanges were reviewed.\nGregor Wernet (2014-06-03)"

        it "reads a comment written straight into the element, with no <text> child" $
            withFixture $ \ParsedDataset{pdActivity = act} -> do
                sectionNamed "Technology" act `shouldBe` Just "Coal-fired power plant"
                sectionNamed "Geography" act `shouldBe` Just "Test geography"

        it "reads the period as its dates followed by what the dataset says about them" $
            withFixture $ \ParsedDataset{pdActivity = act} ->
                sectionNamed "Time period" act `shouldBe` Just "2020-01-01 - 2020-12-31 Test time period"

        it "reads what the dataset includes, and how it was sampled" $
            withFixture $ \ParsedDataset{pdActivity = act} -> do
                sectionNamed "Included activities" act `shouldBe` Just "Coal combustion Electricity generation"
                sectionNamed "System model" act `shouldBe` Just "Test system model"
                sectionNamed "Sampling procedure" act `shouldBe` Just "Test sampling"
                sectionNamed "Extrapolations" act `shouldBe` Just "Test extrapolation"

{- | Synthetic dataset in the shape a real ecoinvent file uses: every free text
wrapped in @\<comment\>\<text\>@, a published source spread over three
attributes, one review with details, and one @[System]@ review whose only
content is the machine validation log.
-}
documentedXml :: BS.ByteString
documentedXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"doc-test\" activityType=\"1\">\n\
    \        <activityName xml:lang=\"en\">port facilities construction</activityName>\n\
    \        <generalComment>\n\
    \          <text xml:lang=\"en\" index=\"0\">The dataset represents the construction of one port.</text>\n\
    \          <text xml:lang=\"en\" index=\"1\">Its life time is assumed to be 100 years.</text>\n\
    \        </generalComment>\n\
    \      </activity>\n\
    \      <geography geographyId=\"RER\"><shortname xml:lang=\"en\">RER</shortname></geography>\n\
    \      <technology technologyLevel=\"3\">\n\
    \        <comment>\n\
    \          <text xml:lang=\"en\" index=\"0\">Conditions at the Port of Rotterdam.</text>\n\
    \          <text xml:lang=\"de\" index=\"1\">Bedingungen im Hafen von Rotterdam.</text>\n\
    \          <text xml:lang=\"en\" index=\"2\">Material composition from Maibach et al.</text>\n\
    \        </comment>\n\
    \      </technology>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">port facilities</name>\n\
    \        <unitName xml:lang=\"en\">unit</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \    </flowData>\n\
    \    <modellingAndValidation>\n\
    \      <representativeness systemModelId=\"m\">\n\
    \        <samplingProcedure xml:lang=\"en\">Literature studies.</samplingProcedure>\n\
    \        <samplingProcedure xml:lang=\"de\">Literaturstudien.</samplingProcedure>\n\
    \        <extrapolations xml:lang=\"en\">none</extrapolations>\n\
    \        <extrapolations xml:lang=\"de\"></extrapolations>\n\
    \      </representativeness>\n\
    \      <review reviewerName=\"Carl Vadenbo\" reviewDate=\"2012-06-29\">\n\
    \        <details>\n\
    \          <text xml:lang=\"en\" index=\"0\">The amounts of the exchanges were reviewed.</text>\n\
    \        </details>\n\
    \      </review>\n\
    \      <review reviewerName=\"[System]\" reviewDate=\"2012-06-29\">\n\
    \        <details>\n\
    \          <text xml:lang=\"en\" index=\"0\">Validation warnings: mass deficit of 22% in activity dataset.</text>\n\
    \        </details>\n\
    \      </review>\n\
    \      <review reviewerName=\"Gregor Wernet\" reviewDate=\"2014-06-03\" reviewedMajorRelease=\"3\"/>\n\
    \    </modellingAndValidation>\n\
    \    <administrativeInformation>\n\
    \      <dataGeneratorAndPublication personId=\"p\" publishedSourceFirstAuthor=\"Spielmann M.\"\n\
    \                                  publishedSourceYear=\"2007\" pageNumbers=\"Water Transport\"/>\n\
    \    </administrativeInformation>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"

{- | Synthetic ecospold2 dataset parameterised on the activityType code and
optional specialActivityType code. One reference output, no other exchanges.
-}
activityTypeFixtureXml :: BS.ByteString -> Maybe BS.ByteString -> BS.ByteString
activityTypeFixtureXml actType mSpec =
    let specAttr = case mSpec of
            Just s -> " specialActivityType=\"" <> s <> "\""
            Nothing -> ""
     in "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
        \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
        \  <activityDataset>\n\
        \    <activityDescription>\n\
        \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"attr-test\""
            <> " activityType=\""
            <> actType
            <> "\""
            <> specAttr
            <> ">\n\
               \        <activityName xml:lang=\"en\">attr test activity</activityName>\n\
               \      </activity>\n\
               \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
               \    </activityDescription>\n\
               \    <flowData>\n\
               \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
               \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
               \        <name xml:lang=\"en\">attr test product</name>\n\
               \        <unitName xml:lang=\"en\">kg</unitName>\n\
               \        <outputGroup>0</outputGroup>\n\
               \      </intermediateExchange>\n\
               \    </flowData>\n\
               \  </activityDataset>\n\
               \</ecoSpold>\n"

{- | Same dataset as 'activityTypeFixtureXml' but naming neither its activity
nor the unit of its reference product: both are read as placeholders, which is
what the reading has to say out loud.
-}
namelessXml :: BS.ByteString
namelessXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"nameless\">\n\
    \      </activity>\n\
    \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">nameless product</name>\n\
    \        <unitName xml:lang=\"en\"></unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"

-- | Same dataset as 'activityTypeFixtureXml' but with no @\<geography\>@ element.
noGeographyXml :: BS.ByteString
noGeographyXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"geo-test\">\n\
    \        <activityName xml:lang=\"en\">geography test activity</activityName>\n\
    \      </activity>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">geography test product</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"

-- | The sawnwood fixture, whose two exchanges carry @\<property\>@ children.
withPropertyFixture :: (Activity -> IO ()) -> IO ()
withPropertyFixture k = do
    result <- streamParseActivityAndFlowsFromFile "test-data/sawnwood-properties_12345678-1234-5678-9abc-12345678aaaa.spold"
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right ParsedDataset{pdActivity = act} -> k act

-- | The properties recorded on the exchange of a given flow, by flow id.
propertiesOf :: Text -> Activity -> Maybe ExchangeProperties
propertiesOf flowId act =
    listToMaybe
        [ techProperties ex
        | ex@TechnosphereExchange{} <- exchanges act
        , Just (exchangeFlowId ex) == UUID.fromText flowId
        ]

withWastePatternsFixture :: (ParsedDataset -> IO ()) -> IO ()
withWastePatternsFixture k = withSystemTempDirectory "es2-waste-spec" $ \dir -> do
    let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
    BS.writeFile path wastePatternsXml
    result <- streamParseActivityAndFlowsFromFile path
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right res -> k res

{- | Synthetic fixture separating the one real EcoSpold2 waste marker from the
look-alike that is not one:
  - an intermediate exchange tagged via classification
    (System="By-product classification", Value="Waste") - the waste axis
  - an elementary exchange with compartment "inventory indicator" / "waste",
    written the way such an indicator usually is (inputGroup 4) - the
    biosphere axis
Plus one genuine biosphere emission and the mandatory reference output.
-}
wastePatternsXml :: BS.ByteString
wastePatternsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"waste-test\">\n\
    \        <activityName xml:lang=\"en\">Waste patterns test activity</activityName>\n\
    \      </activity>\n\
    \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <!-- Reference output (kept as a real product so applyCutoffStrategy is happy) -->\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">Treated thing</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <!-- Intermediate exchange with By-product:Waste classification (treated as INPUT)-->\n\
    \      <intermediateExchange id=\"pat-b\" unitId=\"unit-kg\" amount=\"0.4\"\n\
    \                           intermediateExchangeId=\"cccccccc-cccc-cccc-cccc-cccccccccccc\">\n\
    \        <name xml:lang=\"en\">Spent solvent for treatment</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <inputGroup>5</inputGroup>\n\
    \        <classification classificationId=\"bp-waste\">\n\
    \          <classificationSystem xml:lang=\"en\">By-product classification</classificationSystem>\n\
    \          <classificationValue xml:lang=\"en\">Waste</classificationValue>\n\
    \        </classification>\n\
    \      </intermediateExchange>\n\
    \      <!-- Inventory indicator: elementary, compartment=inventory indicator / waste -->\n\
    \      <elementaryExchange id=\"indicator\" unitId=\"unit-kg\" amount=\"0.2\"\n\
    \                         elementaryExchangeId=\"dddddddd-dddd-dddd-dddd-dddddddddddd\">\n\
    \        <name xml:lang=\"en\">Waste mass placed in landfill</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <compartment>\n\
    \          <compartment xml:lang=\"en\">inventory indicator</compartment>\n\
    \          <subcompartment xml:lang=\"en\">waste</subcompartment>\n\
    \        </compartment>\n\
    \        <inputGroup>4</inputGroup>\n\
    \      </elementaryExchange>\n\
    \      <!-- Genuine biosphere emission for contrast -->\n\
    \      <elementaryExchange id=\"bio\" unitId=\"unit-kg\" amount=\"0.1\"\n\
    \                         elementaryExchangeId=\"eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee\">\n\
    \        <name xml:lang=\"en\">Carbon dioxide</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <compartment>\n\
    \          <compartment xml:lang=\"en\">air</compartment>\n\
    \          <subcompartment xml:lang=\"en\">unspecified</subcompartment>\n\
    \        </compartment>\n\
    \        <outputGroup>4</outputGroup>\n\
    \      </elementaryExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"

{- | A waste-treatment / market-for-waste activity whose reference flow is itself
waste: negative amount, outputGroup="0", tagged By-product classification=Waste.
This is the ecoinvent EcoSpold2 shape (e.g. "market for refinery sludge") that
the WasteFlow axis silently dropped. The reference must remain the technosphere
reference product so the activity loads and its consumers' links resolve.
-}
wasteReferenceXml :: BS.ByteString
wasteReferenceXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"market-for-waste\">\n\
    \        <activityName xml:lang=\"en\">market for refinery sludge</activityName>\n\
    \      </activity>\n\
    \      <geography geographyId=\"GLO\"><shortname xml:lang=\"en\">GLO</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"-1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\"\n\
    \                           productionVolumeAmount=\"1000.0\">\n\
    \        <name xml:lang=\"en\">refinery sludge</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <classification classificationId=\"bp-waste\">\n\
    \          <classificationSystem xml:lang=\"en\">By-product classification</classificationSystem>\n\
    \          <classificationValue xml:lang=\"en\">Waste</classificationValue>\n\
    \        </classification>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"

parseWasteReference :: IO (Either String ParsedDataset)
parseWasteReference = withSystemTempDirectory "es2-waste-ref" $ \dir -> do
    let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
    BS.writeFile path wasteReferenceXml
    streamParseActivityAndFlowsFromFile path

withWasteReferenceFixture :: (ParsedDataset -> IO ()) -> IO ()
withWasteReferenceFixture k =
    parseWasteReference >>= either (expectationFailure . ("Parse failed: " ++)) k

{- | Synthetic dataset exercising mathematicalRelation checking:
  - a reference output carrying variableName="production" (amount 1.0)
  - a fuel input whose stored amount (4.0) diverges from its formula
    "fuel_input * 2 + production" (= 5.0); its nested <property> carries its
    own variableName/mathematicalRelation which must NOT leak onto the exchange
  - an emission whose formula references an unknown variable (kept at 3.0)
  - a <parameter> (fuel_input = 2.0, formula "4.0 / 2") placed AFTER the
    exchanges, as the EcoSpold2 schema orders flowData
  - a <parameter> with a variableName but no amount, dropped with a warning
-}
formulaXml :: BS.ByteString
formulaXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"formula-test\">\n\
    \        <activityName xml:lang=\"en\">Formula test activity</activityName>\n\
    \      </activity>\n\
    \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kwh\" amount=\"1.0\" variableName=\"production\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">Formula test product</name>\n\
    \        <unitName xml:lang=\"en\">kWh</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <intermediateExchange id=\"fuel\" unitId=\"unit-kg\" amount=\"4.0\"\n\
    \                           mathematicalRelation=\"fuel_input * 2 + production\"\n\
    \                           intermediateExchangeId=\"cccccccc-cccc-cccc-cccc-cccccccccccc\">\n\
    \        <name xml:lang=\"en\">Fuel</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <property propertyId=\"prop-1\" amount=\"7.0\" variableName=\"prop_var\" mathematicalRelation=\"9999\">\n\
    \          <name xml:lang=\"en\">dry mass</name>\n\
    \          <unitName xml:lang=\"en\">kg</unitName>\n\
    \        </property>\n\
    \        <inputGroup>5</inputGroup>\n\
    \      </intermediateExchange>\n\
    \      <elementaryExchange id=\"em\" unitId=\"unit-kg\" amount=\"3.0\"\n\
    \                         mathematicalRelation=\"missing_var * 2\"\n\
    \                         elementaryExchangeId=\"dddddddd-dddd-dddd-dddd-dddddddddddd\">\n\
    \        <name xml:lang=\"en\">Carbon dioxide</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <compartment>\n\
    \          <compartment xml:lang=\"en\">air</compartment>\n\
    \          <subcompartment xml:lang=\"en\">unspecified</subcompartment>\n\
    \        </compartment>\n\
    \        <outputGroup>4</outputGroup>\n\
    \      </elementaryExchange>\n\
    \      <parameter parameterId=\"par-1\" variableName=\"fuel_input\" amount=\"2.0\" mathematicalRelation=\"4.0 / 2\">\n\
    \        <name xml:lang=\"en\">fuel input</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \      </parameter>\n\
    \      <parameter parameterId=\"par-2\" variableName=\"ghost\">\n\
    \        <name xml:lang=\"en\">ghost parameter</name>\n\
    \      </parameter>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"

withFormulaFixture :: (ParsedDataset -> IO ()) -> IO ()
withFormulaFixture k = withSystemTempDirectory "es2-formula" $ \dir -> do
    let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
    BS.writeFile path formulaXml
    result <- streamParseActivityAndFlowsFromFile path
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right res -> k res

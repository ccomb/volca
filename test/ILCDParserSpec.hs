{-# LANGUAGE OverloadedStrings #-}

module ILCDParserSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.List (find, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import ILCD.Common (readDataSetVersion)
import ILCD.Parser (ILCDExchangeRaw (..), ILCDProcessRaw (..), buildSupplierIndex, fixActivityExchanges, parseILCDDirectory, parseProcessXML)
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

classOf :: BS.ByteString -> M.Map Text Text
classOf = maybe M.empty iprClassifications . parseProcessXML

-- UUIDs used in supplier linking tests
flowUUID1, flowUUID2, actUUID1, actUUID2, prodUUID1, prodUUID2 :: UUID.UUID
flowUUID1 = read "11111111-0000-0000-0000-000000000001"
flowUUID2 = read "22222222-0000-0000-0000-000000000002"
actUUID1 = read "aaaaaaaa-0000-0000-0000-000000000001"
actUUID2 = read "aaaaaaaa-0000-0000-0000-000000000002"
prodUUID1 = read "bbbbbbbb-0000-0000-0000-000000000001"
prodUUID2 = read "bbbbbbbb-0000-0000-0000-000000000002"

-- An activity with a single reference output exchange for the given flow UUID

-- | The same activity under another name, so a ranking has something to rank on.
named :: Text -> Activity -> Activity
named name act = act{activityName = name}

-- | The same activity, marked retired the way an EcoSpold 1 file marks one.
retired :: Activity -> Activity
retired act = act{activityClassification = M.singleton "Category" "material, obsolete"}

activityWithRefExchange :: UUID.UUID -> Activity
activityWithRefExchange fid =
    Activity
        { activityName = "test"
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = fid
                , techAmount = 1.0
                , techUnitId = UUID.nil
                , techRole = ReferenceProduct
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

-- An activity with a single unresolved input exchange for the given flow UUID
activityWithInputExchange :: UUID.UUID -> Activity
activityWithInputExchange fid =
    Activity
        { activityName = "consumer"
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = fid
                , techAmount = 0.5
                , techUnitId = UUID.nil
                , techRole = Input
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

-- ---------------------------------------------------------------------------
-- Two files, one dataset
-- ---------------------------------------------------------------------------

{- | A process claiming the twin UUID, under the given declared version and
base name. Everything else is the sample's coal extraction, so the two files
differ only where the test reads them.
-}
twinProcess :: BS.ByteString -> BS.ByteString -> BS.ByteString
twinProcess version baseName =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\"\n\
    \                xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\n\
    \  <processInformation>\n\
    \    <dataSetInformation>\n\
    \      <common:UUID>aaaaaaaa-0000-0000-0000-000000000009</common:UUID>\n\
    \      <name>\n\
    \        <baseName xml:lang=\"en\">"
        <> baseName
        <> "</baseName>\n\
           \      </name>\n\
           \    </dataSetInformation>\n\
           \    <geography location=\"GLO\"/>\n\
           \    <quantitativeReference>\n\
           \      <referenceToReferenceFlow>0</referenceToReferenceFlow>\n\
           \    </quantitativeReference>\n\
           \  </processInformation>\n\
           \  <exchanges>\n\
           \    <exchange dataSetInternalID=\"0\">\n\
           \      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-0000-0000-0000-000000000004\"\n\
           \                              type=\"flow data set\"/>\n\
           \      <exchangeDirection>Output</exchangeDirection>\n\
           \      <resultingAmount>1.0</resultingAmount>\n\
           \    </exchange>\n\
           \  </exchanges>\n\
           \  <administrativeInformation>\n\
           \    <publicationAndOwnership>\n\
           \      <common:dataSetVersion>"
        <> version
        <> "</common:dataSetVersion>\n\
           \    </publicationAndOwnership>\n\
           \  </administrativeInformation>\n\
           \</processDataSet>\n"

{- | The sample package copied beside the given extra process files, so a test
can add a twin without touching the fixture every other test reads.
-}
withSampleAnd :: [(FilePath, BS.ByteString)] -> (FilePath -> IO a) -> IO a
withSampleAnd extra k = withSystemTempDirectory "ilcd-twin" $ \dir -> do
    forM_ ["processes", "flows", "flowproperties", "unitgroups"] $ \sub -> do
        createDirectoryIfMissing True (dir </> sub)
        names <- listDirectory ("test-data/SAMPLE.ilcd" </> sub)
        forM_ names $ \n -> copyFile ("test-data/SAMPLE.ilcd" </> sub </> n) (dir </> sub </> n)
    forM_ extra $ \(name, bytes) -> BS.writeFile (dir </> "processes" </> name) bytes
    k dir

-- | The activity names of a loaded database, sorted.
namesOf :: SimpleDatabase -> [Text]
namesOf db = sortOn id [activityName a | a <- M.elems (sdbActivities db)]

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- Full directory parse (SAMPLE.ilcd fixture)
    -- -----------------------------------------------------------------------
    describe "parseILCDDirectory SAMPLE.ilcd" $ do
        it "loads without error" $ do
            result <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            case result of
                Left err -> expectationFailure $ "Expected Right but got: " ++ show err
                Right _ -> return ()

        it "parses exactly two activities" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            M.size (sdbActivities db) `shouldBe` 2

        it "activity is named 'Coal extraction'" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let names = map activityName (M.elems (sdbActivities db))
            names `shouldContain` ["Coal extraction"]

        it "activity location is GLO" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
            activityLocation act `shouldBe` "GLO"

        it "activity has ILCDCategories classification" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
            M.lookup "ILCDCategories" (activityClassification act)
                `shouldBe` Just "Energy/Hard coal"

        it "has two flows (Coal product + CO2 elementary) split across tech and bio" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            (M.size (sdbTechFlows db) + M.size (sdbBioFlows db)) `shouldBe` 2

        it "CO2 flow is biosphere" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let co2uuid = read "aaaaaaaa-0000-0000-0000-000000000003"
            M.member co2uuid (sdbBioFlows db) `shouldBe` True

        it "Coal flow is technosphere" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let coaluuid = read "aaaaaaaa-0000-0000-0000-000000000004"
            M.member coaluuid (sdbTechFlows db) `shouldBe` True

        it "CO2 has CAS number 124-38-9" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let co2uuid = read "aaaaaaaa-0000-0000-0000-000000000003"
            fmap bfCAS (M.lookup co2uuid (sdbBioFlows db)) `shouldBe` Just (Just "124-38-9")

        it "activity has two exchanges" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
            length (exchanges act) `shouldBe` 2

        it "reference exchange is Coal (Technosphere output)" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
                refEx = [ex | ex <- exchanges act, exchangeIsReference ex]
            length refEx `shouldBe` 1

        it "biosphere exchange amount is 2.5" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let Just act = find ((== "Coal extraction") . activityName) (M.elems (sdbActivities db))
                bioEx = [ex | ex <- exchanges act, isBiosphereExchange ex]
            case bioEx of
                [ex] -> bioAmount ex `shouldBe` 2.5
                _ -> expectationFailure "expected one biosphere exchange"

        it "unit group resolves to kg" $ do
            Right db <- parseILCDDirectory defaultUnitConfig Declared "test-data/SAMPLE.ilcd"
            let ugUUID = read "aaaaaaaa-0000-0000-0000-000000000001"
            fmap unitName (M.lookup ugUUID (sdbUnits db)) `shouldBe` Just "kg"

    describe "ILCD Process Parser" $ do
        it "parses classification from classificationInformation" $
            M.lookup "ILCDCategories" (classOf ilcdProcessWithClassification)
                `shouldBe` Just "End-of-life treatment/Material recycling"

        it "parses multiple classification systems" $ do
            let cls = classOf ilcdProcessWithTwoClassifications
            M.lookup "ILCDCategories" cls `shouldBe` Just "Energy/Electricity"
            M.lookup "EcoSpold" cls `shouldBe` Just "Supply"

        it "produces empty classifications when none present" $
            classOf ilcdProcessNoClassification `shouldBe` M.empty

    -- -------------------------------------------------------------------
    -- buildSupplierIndex: UUID-keyed, no name indirection
    -- -------------------------------------------------------------------
    describe "buildSupplierIndex" $ do
        it "indexes reference exchanges by flow UUID" $ do
            let activities =
                    M.fromList
                        [ ((actUUID1, prodUUID1), activityWithRefExchange flowUUID1)
                        , ((actUUID2, prodUUID2), activityWithRefExchange flowUUID2)
                        ]
                idx = buildSupplierIndex activities
            M.lookup flowUUID1 idx `shouldBe` Just ((actUUID1, prodUUID1) NE.:| [])
            M.lookup flowUUID2 idx `shouldBe` Just ((actUUID2, prodUUID2) NE.:| [])

        it "does not index non-reference exchanges" $ do
            let activities =
                    M.fromList
                        [((actUUID1, prodUUID1), activityWithInputExchange flowUUID1)]
                idx = buildSupplierIndex activities
            M.lookup flowUUID1 idx `shouldBe` Nothing

        it "two activities with same flow name but different UUIDs are both indexed" $ do
            -- This is the bug the old name-based code had: M.fromList on names
            -- would silently discard one. UUID keys have no such collision.
            let activities =
                    M.fromList
                        [ ((actUUID1, prodUUID1), activityWithRefExchange flowUUID1)
                        , ((actUUID2, prodUUID2), activityWithRefExchange flowUUID2)
                        ]
                idx = buildSupplierIndex activities
            M.size idx `shouldBe` 2

        -- One product made in two places is two processes declaring one
        -- product flow, and an exchange names the flow. Keeping one entry kept
        -- whichever the identifiers put last.
        it "keeps every process declaring one product flow" $ do
            let activities =
                    M.fromList
                        [ ((actUUID1, prodUUID1), named "wheat production, a" (activityWithRefExchange flowUUID1))
                        , ((actUUID2, prodUUID2), named "wheat production, b" (activityWithRefExchange flowUUID1))
                        ]
                idx = buildSupplierIndex activities
            fmap NE.length (M.lookup flowUUID1 idx) `shouldBe` Just 2

        it "links to the process still in service, whatever its identifier" $ do
            let activities =
                    M.fromList
                        [ ((actUUID1, prodUUID1), retired (named "wheat production, a" (activityWithRefExchange flowUUID1)))
                        , ((actUUID2, prodUUID2), named "wheat production, b" (activityWithRefExchange flowUUID1))
                        ]
                idx = buildSupplierIndex activities
            fmap NE.head (M.lookup flowUUID1 idx) `shouldBe` Just (actUUID2, prodUUID2)

    -- -------------------------------------------------------------------
    -- fixActivityExchanges: resolves input exchanges via supplier index
    -- -------------------------------------------------------------------
    describe "fixActivityExchanges" $ do
        it "resolves input exchange flow UUID to supplier (actUUID, prodUUID)" $ do
            let idx = M.fromList [(flowUUID1, (actUUID1, prodUUID1) NE.:| [])]
                act = activityWithInputExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange{techFlowId = fid, techActivityLinkId = Just alink}] -> do
                    fid `shouldBe` prodUUID1
                    alink `shouldBe` actUUID1
                _ -> expectationFailure "expected one TechnosphereExchange"

        it "leaves input exchange unchanged when flow UUID not in index" $ do
            let idx = M.empty
                act = activityWithInputExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange{techFlowId = fid}] ->
                    fid `shouldBe` flowUUID1
                _ -> expectationFailure "expected one TechnosphereExchange"

        it "does not touch output (reference) exchanges" $ do
            let idx = M.fromList [(flowUUID1, (actUUID1, prodUUID1) NE.:| [])]
                act = activityWithRefExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange{techFlowId = fid, techRole = role}] -> do
                    fid `shouldBe` flowUUID1 -- unchanged
                    role `shouldBe` ReferenceProduct
                _ -> expectationFailure "expected one TechnosphereExchange"

    -- -----------------------------------------------------------------------
    -- An ILCD dataset is named by the UUID in the file, so two files can claim
    -- one dataset. The version each declares says which, and nothing else can.
    -- -----------------------------------------------------------------------
    describe "two files claiming one dataset" $ do
        -- The file names are chosen so the higher version sorts first: a table
        -- keeping whichever file came last would take the older dataset, and a
        -- listing in the other order would take the newer one.
        let older = ("z-first-edition.xml", twinProcess "01.00.000" "Coal extraction, first edition")
            newer = ("a-second-edition.xml", twinProcess "02.00.000" "Coal extraction, second edition")

        it "keeps the dataset declaring the higher version" $
            withSampleAnd [older, newer] $ \dir -> do
                result <- parseILCDDirectory defaultUnitConfig Declared dir
                case result of
                    Left err -> expectationFailure $ "Expected Right but got: " ++ show err
                    Right db -> namesOf db `shouldSatisfy` elem "Coal extraction, second edition"

        it "does not keep the dataset it replaces" $
            withSampleAnd [older, newer] $ \dir -> do
                result <- parseILCDDirectory defaultUnitConfig Declared dir
                case result of
                    Left err -> expectationFailure $ "Expected Right but got: " ++ show err
                    Right db -> namesOf db `shouldSatisfy` notElem "Coal extraction, first edition"

        -- The files are read by as many workers as the machine has cores, and
        -- the answer used to depend on which worker each file fell to.
        it "answers the same on one core as on four" $
            withSampleAnd [older, newer] $ \dir -> do
                cores <- getNumCapabilities
                setNumCapabilities 1
                onOne <- parseILCDDirectory defaultUnitConfig Declared dir
                setNumCapabilities 4
                onFour <- parseILCDDirectory defaultUnitConfig Declared dir
                setNumCapabilities cores
                fmap namesOf onOne `shouldBe` fmap namesOf onFour

        it "refuses when two files declare one dataset at the same version"
            $ withSampleAnd
                [ ("twin-a.xml", twinProcess "03.00.000" "Coal extraction, one way")
                , ("twin-b.xml", twinProcess "03.00.000" "Coal extraction, another way")
                ]
            $ \dir -> do
                result <- parseILCDDirectory defaultUnitConfig Declared dir
                case result of
                    Right _ -> expectationFailure "Expected the load to stop on two files at one version"
                    Left err -> do
                        err `shouldSatisfy` T.isInfixOf "twin-a.xml"
                        err `shouldSatisfy` T.isInfixOf "twin-b.xml"

    describe "readDataSetVersion" $ do
        it "compares the numbers and not the text" $
            (readDataSetVersion "10.00.000" > readDataSetVersion "09.00.000") `shouldBe` True

        it "reads nothing from a version that is not dotted numbers" $
            readDataSetVersion "draft" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- parseProcessXML: basic fields
    -- -----------------------------------------------------------------------
    describe "parseProcessXML basic fields" $ do
        it "parses process UUID" $
            fmap iprUUID (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` UUID.fromText "12345678-1234-1234-1234-123456789abc"

        it "parses process name" $
            fmap iprName (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just "Test Process"

        it "parses process location" $
            fmap iprLocation (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just "DE"

        it "parses referenceToReferenceFlow index" $
            fmap iprRefFlowIdx (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just 0

        it "returns Nothing for invalid XML" $
            case parseProcessXML "<not-xml" of
                Nothing -> return ()
                Just _ -> expectationFailure "expected Nothing for invalid XML"

        it "returns Nothing when baseName is missing" $ do
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/no-basename.xml"
            case parseProcessXML xml of
                Nothing -> return ()
                Just _ -> expectationFailure "expected Nothing when baseName missing"

        it "captures <processType> element verbatim" $
            fmap iprProcessType (parseProcessXML ilcdProcessWithProcessType)
                `shouldBe` Just "Unit process, single operation"

        it "leaves iprProcessType empty when <processType> is absent" $
            fmap iprProcessType (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just ""

    describe "the shares an ILCD dataset declares" $ do
        it "reads the fraction a product output allocates to itself" $
            fmap (map ierShare . sortOn ierInternalId . iprExchanges) (parseProcessXML ilcdAllocatedProcess)
                `shouldBe` Just [Just 60.0, Just 40.0]

        it "leaves a dataset that declares none without shares" $
            fmap (map ierShare . iprExchanges) (parseProcessXML ilcdProcessWithClassification)
                `shouldBe` Just [Nothing]

        it "reads no share when the exchange allocates to several co-products" $
            -- The general form: each exchange is distributed across the
            -- co-products, and its entry pointing at itself is its own share
            -- of itself. Reading that as a declared share would give every
            -- product 100 % and hand each one the whole inventory.
            fmap (map ierShare . iprExchanges) (parseProcessXML ilcdMatrixAllocatedProcess)
                `shouldBe` Just [Nothing, Nothing]

        it "ignores a fraction allocated to another co-product" $
            -- ILCD lets an exchange allocate to any co-product by internal id.
            -- Only the entry naming the exchange itself says "this product's
            -- share", which is the one DeclaredShare can hold.
            fmap (map ierShare . sortOn ierInternalId . iprExchanges) (parseProcessXML ilcdCrossAllocatedProcess)
                `shouldBe` Just [Just 60.0, Nothing]

    describe "parseProcessXML exchange fields" $ do
        it "parses single exchange flow ref" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] ->
                    ierFlowRef ex
                        `shouldBe` read "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                _ -> expectationFailure "expected one exchange"

        it "parses exchange direction Output" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] -> ierDirection ex `shouldBe` "Output"
                _ -> expectationFailure "expected one exchange"

        it "parses exchange resultingAmount" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] -> ierAmount ex `shouldBe` 1.0
                _ -> expectationFailure "expected one exchange"

        it "parses exchange dataSetInternalID" $ do
            let Just raw = parseProcessXML ilcdProcessWithClassification
            case iprExchanges raw of
                [ex] -> ierInternalId ex `shouldBe` 0
                _ -> expectationFailure "expected one exchange"

        it "parses Input exchange direction" $ do
            -- re-use SAMPLE.ilcd process which has an Input exchange (CO2 = Output, Coal = Output)
            -- Use the coal extraction process which has an Output; test Input via meanAmount file
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/aaaaaaaa-0000-0000-0000-000000000005.xml"
            let Just raw = parseProcessXML xml
            -- coal extraction has two Output exchanges; verify direction parsing works
            let dirs = map ierDirection (iprExchanges raw)
            dirs `shouldContain` ["Output"]

        it "falls back to meanAmount when resultingAmount is absent" $ do
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/mean-amount.xml"
            let Just raw = parseProcessXML xml
            case iprExchanges raw of
                [ex] -> ierAmount ex `shouldBe` 42.0
                _ -> expectationFailure "expected one exchange"

    describe "per-exchange common:generalComment" $ do
        it "captures exchange-level comment, leaves comment-less exchange Nothing, prefers English" $ do
            let Just raw = parseProcessXML ilcdProcessWithExchangeComments
            map ierComment (iprExchanges raw)
                `shouldBe` [ Just "Reference output of the process"
                           , Nothing
                           , Just "English text wins"
                           ]

        it "does not leak the process-level <common:generalComment> into any exchange" $ do
            let Just raw = parseProcessXML ilcdProcessWithExchangeComments
                processNote = "Process-level note that must not leak"
            all (\ex -> ierComment ex /= Just processNote) (iprExchanges raw)
                `shouldBe` True

        it "parses multiple exchanges preserving order" $ do
            xml <- BS.readFile "test-data/SAMPLE.ilcd/processes/aaaaaaaa-0000-0000-0000-000000000005.xml"
            let Just raw = parseProcessXML xml
            length (iprExchanges raw) `shouldBe` 2

-- Minimal ILCD process XML with classification
ilcdProcessWithClassification :: BS.ByteString
ilcdProcessWithClassification =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process</baseName></name>\
    \<classificationInformation>\
    \<common:classification name=\"ILCDCategories\">\
    \<common:class level=\"0\">End-of-life treatment</common:class>\
    \<common:class level=\"1\">Material recycling</common:class>\
    \</common:classification>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \<geography location=\"DE\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessWithTwoClassifications :: BS.ByteString
ilcdProcessWithTwoClassifications =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>22345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process 2</baseName></name>\
    \<classificationInformation>\
    \<common:classification name=\"ILCDCategories\">\
    \<common:class level=\"0\">Energy</common:class>\
    \<common:class level=\"1\">Electricity</common:class>\
    \</common:classification>\
    \<common:classification name=\"EcoSpold\">\
    \<common:class level=\"0\">Supply</common:class>\
    \</common:classification>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \<geography location=\"FR\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

{- | Three exchanges with different comment configurations:
* dataSetInternalID="0": top-level English `<common:generalComment>` only
* dataSetInternalID="1": no comment (sibling of a process-level
  `<common:generalComment>` that must NOT leak in)
* dataSetInternalID="2": both English and German translations; English wins
-}
ilcdProcessWithExchangeComments :: BS.ByteString
ilcdProcessWithExchangeComments =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>42345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Process with exchange comments</baseName></name>\
    \<common:generalComment xml:lang=\"en\">Process-level note that must not leak</common:generalComment>\
    \</dataSetInformation>\
    \<geography location=\"GLO\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \<common:generalComment xml:lang=\"en\">Reference output of the process</common:generalComment>\
    \</exchange>\
    \<exchange dataSetInternalID=\"1\">\
    \<referenceToFlowDataSet refObjectId=\"bbbbbbbb-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Input</exchangeDirection>\
    \<resultingAmount>0.5</resultingAmount>\
    \</exchange>\
    \<exchange dataSetInternalID=\"2\">\
    \<referenceToFlowDataSet refObjectId=\"cccccccc-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Input</exchangeDirection>\
    \<resultingAmount>0.25</resultingAmount>\
    \<common:generalComment xml:lang=\"de\">Deutscher Text zuerst</common:generalComment>\
    \<common:generalComment xml:lang=\"en\">English text wins</common:generalComment>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessNoClassification :: BS.ByteString
ilcdProcessNoClassification =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>32345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process 3</baseName></name>\
    \</dataSetInformation>\
    \<geography location=\"US\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessWithProcessType :: BS.ByteString
ilcdProcessWithProcessType =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>42345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process With ProcessType</baseName></name>\
    \</dataSetInformation>\
    \<geography location=\"FR\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<modellingAndValidation>\
    \<LCIMethodAndAllocation>\
    \<processType>Unit process, single operation</processType>\
    \</LCIMethodAndAllocation>\
    \</modellingAndValidation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

{- | Two product outputs, each allocating a fraction to itself: the shape a
dataset takes when its author states an allocation key.
-}
ilcdAllocatedProcess :: BS.ByteString
ilcdAllocatedProcess = allocatedProcess "0" "60.0" "1" "40.0"

{- | Both exchanges written in the general form: each distributes itself
across the two co-products, so the entry naming the exchange itself is 100
and says nothing about allocation.
-}
ilcdMatrixAllocatedProcess :: BS.ByteString
ilcdMatrixAllocatedProcess = matrixAllocatedProcess

{- | The same block, except the second exchange allocates to the first. No
share is then readable for it, and the allocation gate refuses the dataset
rather than splitting on a number that meant something else.
-}
ilcdCrossAllocatedProcess :: BS.ByteString
ilcdCrossAllocatedProcess = allocatedProcess "0" "60.0" "0" "40.0"

-- | A two-output dataset whose two @<allocation>@ entries are given.
allocatedProcess :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
allocatedProcess ref0 pct0 ref1 pct1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>52345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Allocated block</baseName></name>\
    \</dataSetInformation>\
    \<geography location=\"FR\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \<allocations><allocation internalReferenceToCoProduct=\""
        <> ref0
        <> "\" allocatedFraction=\""
        <> pct0
        <> "\"/></allocations>\
           \</exchange>\
           \<exchange dataSetInternalID=\"1\">\
           \<referenceToFlowDataSet refObjectId=\"bbbbbbbb-cccc-dddd-eeee-ffffffffffff\"/>\
           \<exchangeDirection>Output</exchangeDirection>\
           \<resultingAmount>2.0</resultingAmount>\
           \<allocations><allocation internalReferenceToCoProduct=\""
        <> ref1
        <> "\" allocatedFraction=\""
        <> pct1
        <> "\"/></allocations>\
           \</exchange>\
           \</exchanges>\
           \</processDataSet>"

{- | A two-output dataset where each exchange spreads itself over both
co-products, the general form ILCD also allows.
-}
matrixAllocatedProcess :: BS.ByteString
matrixAllocatedProcess =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>62345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Matrix allocated block</baseName></name>\
    \</dataSetInformation>\
    \<geography location=\"FR\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \<allocations>\
    \<allocation internalReferenceToCoProduct=\"0\" allocatedFraction=\"100.0\"/>\
    \<allocation internalReferenceToCoProduct=\"1\" allocatedFraction=\"0.0\"/>\
    \</allocations>\
    \</exchange>\
    \<exchange dataSetInternalID=\"1\">\
    \<referenceToFlowDataSet refObjectId=\"bbbbbbbb-cccc-dddd-eeee-ffffffffffff\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>2.0</resultingAmount>\
    \<allocations>\
    \<allocation internalReferenceToCoProduct=\"0\" allocatedFraction=\"0.0\"/>\
    \<allocation internalReferenceToCoProduct=\"1\" allocatedFraction=\"100.0\"/>\
    \</allocations>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

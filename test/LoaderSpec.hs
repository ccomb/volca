{-# LANGUAGE OverloadedStrings #-}

module LoaderSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Loader
import EcoSpold.Common (ParsedDataset (..))
import TestHelpers (loadSampleDatabase)
import Types
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Minimal fixtures
-- ---------------------------------------------------------------------------

flowUUID1, flowUUID2, actUUID1 :: UUID.UUID
flowUUID1 = read "aaaaaaaa-0000-0000-0000-000000000001"
flowUUID2 = read "bbbbbbbb-0000-0000-0000-000000000002"
actUUID1 = read "cccccccc-0000-0000-0000-000000000001"

minimalFlow :: UUID.UUID -> Text -> TechnosphereFlow
minimalFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = UUID.nil
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

minimalActivity :: Text -> Text -> [Exchange] -> Activity
minimalActivity name loc exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

-- | The same activity, filed in the source's obsolete category.
retired :: Activity -> Activity
retired act = act{activityClassification = M.singleton "Category" "Autres\\Obsolete"}

-- | One dataset as a reader hands it over: an activity and the flows it declared.
minimalDataset :: Activity -> [TechnosphereFlow] -> ParsedDataset
minimalDataset act techs =
    ParsedDataset
        { pdActivity = act
        , pdTechFlows = techs
        , pdBioFlows = []
        , pdWasteFlows = []
        , pdUnits = []
        , pdDatasetNumber = 0
        , pdWarnings = []
        }

{- | One reader's share of a load, always under the same (activity, product)
key, so that two of them collide and the merging law has to decide.
-}
share :: Text -> [TechnosphereFlow] -> ((UUID.UUID, UUID.UUID), ParsedDataset)
share name techs = ((actUUID1, flowUUID1), minimalDataset (minimalActivity name "GLO" []) techs)

{- | One coproduct of a block published under dataset number 7: allocation gives
each of them its own key, and they all carry the number the block was read under.
-}
numbered :: (UUID.UUID, UUID.UUID) -> ((UUID.UUID, UUID.UUID), ParsedDataset)
numbered key = (key, (minimalDataset (minimalActivity "cogeneration" "GLO" []) []){pdDatasetNumber = 7})

refExchange :: UUID.UUID -> Exchange
refExchange fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techRole = ReferenceProduct
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

inputExchange :: UUID.UUID -> Text -> Exchange
inputExchange fid loc =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 0.5
        , techUnitId = UUID.nil
        , techRole = Input
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = loc
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

actUUID2, missingActUUID :: UUID.UUID
actUUID2 = read "cccccccc-0000-0000-0000-000000000002"
missingActUUID = read "dddddddd-0000-0000-0000-000000000099"

-- | A name-only index holding one producer of one product name.
oneProducer :: Text -> UUID.UUID -> UUID.UUID -> Text -> NameOnlyIndex
oneProducer key actId prodId unit = M.singleton key (namedProducerOf "producer" actId prodId unit NE.:| [])

-- | A producer of a product, named, in no location and in service.
namedProducerOf :: Text -> UUID.UUID -> UUID.UUID -> Text -> NameProducer
namedProducerOf name actId prodId unit =
    NameProducer
        { npActivityUUID = actId
        , npProductUUID = prodId
        , npActivityName = name
        , npLocation = ""
        , npObsolete = False
        , npReferenceUnit = unit
        }

-- | The producer the index ranks first, as (activity, product, reference unit).
firstProducer :: Text -> NameOnlyIndex -> Maybe (UUID.UUID, UUID.UUID, Text)
firstProducer key = fmap (triple . NE.head) . M.lookup key
  where
    triple :: NameProducer -> (UUID.UUID, UUID.UUID, Text)
    triple p = (npActivityUUID p, npProductUUID p, npReferenceUnit p)

{- | A minimal EcoSpold1 document, one @<dataset>@ per (name, location) pair.
Each dataset declares "<name> production" as its activity and @name@ as its
reference product, mirroring @test-data/SAMPLE.ecospold1@.
-}
es1Xml :: [(String, String)] -> String
es1Xml datasets =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        <> "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">\n"
        <> concat (zipWith dataset [1 :: Int ..] datasets)
        <> "</ecoSpold>\n"
  where
    dataset n (name, loc) =
        "  <dataset number=\""
            <> show n
            <> "\" generator=\"Test\" timestamp=\"2025-01-01T00:00:00\">\n"
            <> "    <metaInformation><processInformation>\n"
            <> ("      <referenceFunction name=\"" <> name <> " production\" category=\"Energy\" subCategory=\"Electricity\" unit=\"kWh\" />\n")
            <> ("      <geography location=\"" <> loc <> "\" />\n")
            <> "      <timePeriod startYear=\"2020\" endYear=\"2024\" />\n"
            <> "    </processInformation></metaInformation>\n"
            <> "    <flowData>\n"
            <> ("      <exchange number=\"1\" name=\"" <> name <> "\" category=\"Energy\" subCategory=\"Electricity\" unit=\"kWh\" meanValue=\"1.0\"><outputGroup>0</outputGroup></exchange>\n")
            <> "    </flowData>\n"
            <> "  </dataset>\n"

-- | An input naming the dataset number its supplier is published under.
buyingDataset :: Int -> Exchange -> Exchange
buyingDataset n ex = ex{techSupplierClaim = ClaimByDatasetNumber n}

-- | An input linked (non-nil) to producer activity @actId@ producing @prodId@.
linkedInput :: UUID.UUID -> UUID.UUID -> Exchange
linkedInput actId prodId = (inputExchange prodId "GLO"){techActivityLinkId = Just actId}

{- | A treatment process's reference input (the waste it treats): an input-side
reference exchange that the matrix builder skips, so it is no supplier demand.
-}
referenceInput :: UUID.UUID -> Exchange
referenceInput fid = (inputExchange fid "GLO"){techRole = ReferenceInput}

simpleDBOf :: [((UUID.UUID, UUID.UUID), Activity)] -> [(UUID.UUID, Text)] -> SimpleDatabase
simpleDBOf acts flows =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows = M.fromList [(fid, minimalFlow fid name) | (fid, name) <- flows]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.empty
        }

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- findFilesByExtRecursive
    -- -----------------------------------------------------------------------
    describe "findFilesByExtRecursive" $ do
        it "finds .spold datasets in a subdirectory beside a root CSV" $
            -- Native ecoinvent layout: datasets/*.spold plus a top-level
            -- FilenameToActivityLookup.csv. The package must load from its root.
            withSystemTempDirectory "ecospold-load" $ \dir -> do
                createDirectoryIfMissing True (dir </> "datasets")
                writeFile (dir </> "datasets" </> "a_b.spold") "<x/>"
                writeFile (dir </> "FilenameToActivityLookup.csv") "Filename;ActivityName\n"
                found <- findFilesByExtRecursive ".spold" dir
                found `shouldBe` [dir </> "datasets" </> "a_b.spold"]

    -- -----------------------------------------------------------------------
    -- normalizeText
    -- -----------------------------------------------------------------------
    describe "normalizeText" $ do
        it "lowercases text" $
            normalizeText "WHEAT Production" `shouldBe` "wheat production"

        it "strips leading and trailing whitespace" $
            normalizeText "  wheat  " `shouldBe` "wheat"

        it "handles empty text" $
            normalizeText "" `shouldBe` ""

    -- -----------------------------------------------------------------------
    -- mergeTechFlows
    -- -----------------------------------------------------------------------
    describe "mergeTechFlows" $ do
        it "unions synonyms from both flows" $ do
            let a = (minimalFlow flowUUID1 "CO2"){tfSynonyms = M.singleton "en" (S.fromList ["carbon dioxide"])}
                b = (minimalFlow flowUUID1 "CO2"){tfSynonyms = M.singleton "en" (S.fromList ["CO2"])}
                merged = mergeTechFlows a b
            M.lookup "en" (tfSynonyms merged) `shouldBe` Just (S.fromList ["carbon dioxide", "CO2"])

        it "keeps all other fields from the first flow" $ do
            let a = minimalFlow flowUUID1 "flow-a"
                b = minimalFlow flowUUID2 "flow-b"
            tfName (mergeTechFlows a b) `shouldBe` "flow-a"

    -- -----------------------------------------------------------------------
    -- Which row wins a duplicate key. The answer is not one rule but two
    -- facing opposite ways, and a load reads both: the files one reader
    -- harvested are folded into a table with 'fromListWith', several readers'
    -- tables are then merged with 'unionsWith'. Written down here because
    -- 'mergeTechFlows' above says what merging two flows does and nothing says
    -- which of the two is handed to it first.
    -- -----------------------------------------------------------------------
    describe "duplicate keys inside a harvest and between harvests" $ do
        it "keeps the last dataset read, inside one reader's share" $ do
            let a = (minimalFlow flowUUID1 "flow-a"){tfCAS = Just "7732-18-5"}
                b = minimalFlow flowUUID1 "flow-b"
                harvest = harvestOf [share "activity-a" [a], share "activity-b" [b]]
            fmap tfName (M.lookup flowUUID1 (hvTechFlows harvest)) `shouldBe` Just "flow-b"
            -- The loser is not lost entirely: its CAS fills the gap.
            fmap tfCAS (M.lookup flowUUID1 (hvTechFlows harvest)) `shouldBe` Just (Just "7732-18-5")

        it "keeps the first reader's flow, when two shares meet" $ do
            let a = minimalFlow flowUUID1 "flow-a"
                b = (minimalFlow flowUUID1 "flow-b"){tfCAS = Just "7732-18-5"}
                merged = harvestOf [share "activity-a" [a]] <> harvestOf [share "activity-b" [b]]
            fmap tfName (M.lookup flowUUID1 (hvTechFlows merged)) `shouldBe` Just "flow-a"
            fmap tfCAS (M.lookup flowUUID1 (hvTechFlows merged)) `shouldBe` Just (Just "7732-18-5")

        it "keeps the first reader's activity, when two shares meet" $ do
            let merged = harvestOf [share "activity-a" []] <> harvestOf [share "activity-b" []]
            fmap activityName (M.lookup (actUUID1, flowUUID1) (hvActivities merged))
                `shouldBe` Just "activity-a"

        -- The dataset-number table is under neither law: an allocated block is
        -- written once per coproduct, all of them under the block's number, so
        -- a repeated number is the ordinary shape and every side is kept.
        it "keeps every coproduct of one dataset number, in reading order" $ do
            let harvest = harvestOf [numbered (actUUID1, flowUUID1), numbered (actUUID2, flowUUID2)]
            M.lookup 7 (hvDatasetNumbers harvest)
                `shouldBe` Just ((actUUID1, flowUUID1) NE.:| [(actUUID2, flowUUID2)])

        it "keeps both readers' datasets under one number, the first reader's first" $ do
            let merged = harvestOf [numbered (actUUID1, flowUUID1)] <> harvestOf [numbered (actUUID2, flowUUID2)]
            M.lookup 7 (hvDatasetNumbers merged)
                `shouldBe` Just ((actUUID1, flowUUID1) NE.:| [(actUUID2, flowUUID2)])

    -- -----------------------------------------------------------------------
    describe "indexActivities" $ do
        it "names the two spellings a case fold brings together, and keeps the last read" $ do
            let acts =
                    [ minimalActivity "Steel, low-alloyed" "GLO" [refExchange flowUUID1]
                    , minimalActivity "steel, low-alloyed" "GLO" [refExchange flowUUID1]
                    ]
                (procMap, collisions) = indexActivities acts
            M.size procMap `shouldBe` 1
            map activityName (M.elems procMap) `shouldBe` ["steel, low-alloyed"]
            length collisions `shouldBe` 1
            head collisions `shouldSatisfy` \msg ->
                all (`T.isInfixOf` msg) ["'Steel, low-alloyed'", "'steel, low-alloyed'", "GLO"]

        it "says nothing about one spelling written twice at two locations" $ do
            let acts =
                    [ minimalActivity "Steel, low-alloyed" "GLO" [refExchange flowUUID1]
                    , minimalActivity "Steel, low-alloyed" "FR" [refExchange flowUUID1]
                    ]
                (procMap, collisions) = indexActivities acts
            M.size procMap `shouldBe` 2
            collisions `shouldBe` []

        it "says nothing when the two blocks state two products" $ do
            let acts =
                    [ minimalActivity "Steel, low-alloyed" "GLO" [refExchange flowUUID1]
                    , minimalActivity "steel, low-alloyed" "GLO" [refExchange flowUUID2]
                    ]
                (procMap, collisions) = indexActivities acts
            M.size procMap `shouldBe` 2
            collisions `shouldBe` []

        it "leaves blocks their file identifies alone" $ do
            let published name = (minimalActivity name "GLO" [refExchange flowUUID1]){activityNativeId = Just (NativeProcessId name)}
                (procMap, collisions) = indexActivities [published "Steel, low-alloyed", published "steel, low-alloyed"]
            M.size procMap `shouldBe` 2
            collisions `shouldBe` []

    -- -----------------------------------------------------------------------
    -- generateActivityUUIDFromActivity
    -- -----------------------------------------------------------------------
    describe "generateActivityUUIDFromActivity" $ do
        it "is deterministic for the same activity" $ do
            let act = minimalActivity "wheat production" "GLO" []
            generateActivityUUIDFromActivity act
                `shouldBe` generateActivityUUIDFromActivity act

        it "differs for different name" $ do
            let a = minimalActivity "wheat production" "GLO" []
                b = minimalActivity "barley production" "GLO" []
            generateActivityUUIDFromActivity a
                `shouldNotBe` generateActivityUUIDFromActivity b

        it "differs for different location" $ do
            let a = minimalActivity "wheat production" "GLO" []
                b = minimalActivity "wheat production" "FR" []
            generateActivityUUIDFromActivity a
                `shouldNotBe` generateActivityUUIDFromActivity b

    -- -----------------------------------------------------------------------
    -- datasetUUIDFromPath
    -- -----------------------------------------------------------------------
    describe "datasetUUIDFromPath" $ do
        it "reads the identifier out of a process_<uuid>.xml file name" $
            datasetUUIDFromPath "/db/process_0004e814-c18d-42e2-a3f7-ce1fa51a3c2c.xml"
                `shouldBe` UUID.fromText "0004e814-c18d-42e2-a3f7-ce1fa51a3c2c"

        it "reads a bare <uuid>.xml file name too" $
            datasetUUIDFromPath "0004e814-c18d-42e2-a3f7-ce1fa51a3c2c.xml"
                `shouldBe` UUID.fromText "0004e814-c18d-42e2-a3f7-ce1fa51a3c2c"

        it "declines a numbered file name, so the minted UUID stands" $
            datasetUUIDFromPath "/db/1234.xml" `shouldBe` Nothing

        it "declines a name that only looks like an identifier" $
            datasetUUIDFromPath "process_not-a-uuid.xml" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- EcoSpold1 file-name identity, end to end through loadDatabase
    -- -----------------------------------------------------------------------
    describe "EcoSpold1 file-name identity" $ do
        let fileUUID = read "0004e814-c18d-42e2-a3f7-ce1fa51a3c2c" :: UUID.UUID
            loadedActUUIDs dir = do
                result <- loadDatabase defaultUnitConfig dir
                either (fail . show) (return . map fst . M.keys . sdbActivities) result

        it "keys each per-dataset file on its own identifier, minting only where the name carries none" $
            withSystemTempDirectory "es1-ident" $ \dir -> do
                writeFile (dir </> "process_0004e814-c18d-42e2-a3f7-ce1fa51a3c2c.xml") (es1Xml [("wind", "DE")])
                writeFile (dir </> "dataset2.xml") (es1Xml [("solar", "FR")])
                actUUIDs <- loadedActUUIDs dir
                actUUIDs `shouldMatchList` [fileUUID, generateActivityUUIDFromActivity (minimalActivity "solar production" "FR" [])]

        it "keys a lone single-dataset file on its identifier too" $
            withSystemTempDirectory "es1-ident" $ \dir -> do
                writeFile (dir </> "process_0004e814-c18d-42e2-a3f7-ce1fa51a3c2c.xml") (es1Xml [("wind", "DE")])
                actUUIDs <- loadedActUUIDs dir
                actUUIDs `shouldBe` [fileUUID]

        it "never hands one file's identifier to several datasets inside it" $
            withSystemTempDirectory "es1-ident" $ \dir -> do
                writeFile (dir </> "process_0004e814-c18d-42e2-a3f7-ce1fa51a3c2c.xml") (es1Xml [("wind", "DE"), ("solar", "FR")])
                actUUIDs <- loadedActUUIDs dir
                length actUUIDs `shouldBe` 2
                actUUIDs `shouldSatisfy` notElem fileUUID

    -- -----------------------------------------------------------------------
    -- getReferenceProductUUID
    -- -----------------------------------------------------------------------
    describe "getReferenceProductUUID" $ do
        it "returns the flow UUID of the reference exchange" $ do
            let act = minimalActivity "prod" "GLO" [refExchange flowUUID1]
            getReferenceProductUUID act `shouldBe` flowUUID1

        it "returns UUID.nil when there is no reference exchange" $ do
            let act = minimalActivity "prod" "GLO" []
            getReferenceProductUUID act `shouldBe` UUID.nil

    -- -----------------------------------------------------------------------
    -- UnlinkedSummary Monoid (product of monoids: Map-union + 3× Int addition)
    -- -----------------------------------------------------------------------
    describe "UnlinkedSummary Monoid" $ do
        it "sums all counters via (<>)" $ do
            let s1 = UnlinkedSummary M.empty 10 8 2 [] []
                s2 = UnlinkedSummary M.empty 5 3 2 [] []
                m = s1 <> s2
            usTotalLinks m `shouldBe` 15
            usFoundLinks m `shouldBe` 11
            usMissingLinks m `shouldBe` 4

        it "unions activity maps via (<>)" $ do
            let s1 = UnlinkedSummary (M.singleton "actA" []) 1 0 1 [] []
                s2 = UnlinkedSummary (M.singleton "actB" []) 1 0 1 [] []
                m = s1 <> s2
            M.size (usActivities m) `shouldBe` 2

        it "mempty is the identity" $ do
            let s = UnlinkedSummary M.empty 3 2 1 [] []
                m = s <> mempty
            usTotalLinks m `shouldBe` 3
            usFoundLinks m `shouldBe` 2
            usMissingLinks m `shouldBe` 1

    -- -----------------------------------------------------------------------
    -- buildSupplierIndex (name+location keyed, EcoSpold1 style)
    -- -----------------------------------------------------------------------
    describe "buildSupplierIndex" $ do
        it "indexes reference exchanges by (normalizedName, location)" $ do
            let act =
                    minimalActivity
                        "wheat production"
                        "GLO"
                        [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndex acts flows
            M.lookup ("wheat", "GLO") idx `shouldBe` Just (actUUID1, flowUUID1)

        it "does not index input (non-reference) exchanges" $ do
            let act =
                    minimalActivity
                        "consumer"
                        "DE"
                        [inputExchange flowUUID1 "GLO"]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndex acts flows
            M.null idx `shouldBe` True

    -- -----------------------------------------------------------------------
    -- buildSupplierIndexByName (name-only keyed, SimaPro style)
    -- -----------------------------------------------------------------------
    describe "buildSupplierIndexByName" $ do
        it "indexes reference exchanges by normalized flow name" $ do
            let act =
                    minimalActivity
                        "wheat production"
                        "GLO"
                        [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat Production")]
                idx = buildSupplierIndexByName M.empty acts flows
            -- empty UnitDB → reference unit resolves to the "unknown" sentinel
            firstProducer "wheat production" idx `shouldBe` Just (actUUID1, flowUUID1, "unknown")

        it "picks a duplicate producer by name, never by identifier" $ do
            -- The typo pair: two blocks exported under names differing by one
            -- letter, both declaring the same product. Either answers, and the
            -- one picked must not move when identity is minted differently.
            let acts =
                    M.fromList
                        [ ((actUUID1, flowUUID1), minimalActivity "Pork, meat without bone" "FR" [refExchange flowUUID1])
                        , ((actUUID2, flowUUID2), minimalActivity "Pork, meat whitout bone" "FR" [refExchange flowUUID2])
                        ]
                flows =
                    M.fromList
                        [ (flowUUID1, minimalFlow flowUUID1 "Pork, bone")
                        , (flowUUID2, minimalFlow flowUUID2 "Pork, bone")
                        ]
            -- "whitout" sorts before "without", and holds flowUUID2.
            firstProducer "pork, bone" (buildSupplierIndexByName M.empty acts flows)
                `shouldBe` Just (actUUID2, flowUUID2, "unknown")

        it "lets the block the source retired lose the tie" $ do
            -- The same pair, one of them now filed under an obsolete
            -- category. The file says which of the two it means, so the name
            -- sort is not consulted and the block still in service supplies.
            let acts =
                    M.fromList
                        [ ((actUUID1, flowUUID1), minimalActivity "Pork, meat without bone" "FR" [refExchange flowUUID1])
                        , ((actUUID2, flowUUID2), retired (minimalActivity "Pork, meat whitout bone" "FR" [refExchange flowUUID2]))
                        ]
                flows =
                    M.fromList
                        [ (flowUUID1, minimalFlow flowUUID1 "Pork, bone")
                        , (flowUUID2, minimalFlow flowUUID2 "Pork, bone")
                        ]
            firstProducer "pork, bone" (buildSupplierIndexByName M.empty acts flows)
                `shouldBe` Just (actUUID1, flowUUID1, "unknown")

        it "does not index a prefix of a product name" $ do
            -- "Urea {RER}| urea production" and "Urea {RoW}| urea production"
            -- are not the same product, and neither is "Urea".
            let act = minimalActivity "urea market" "RER" [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Urea {RER}| market for urea | Cut-off, S")]
                idx = buildSupplierIndexByName M.empty acts flows
            M.keys idx `shouldBe` ["urea {rer}| market for urea | cut-off, s"]

        it "does not index non-reference exchanges" $ do
            let act =
                    minimalActivity
                        "consumer"
                        "DE"
                        [inputExchange flowUUID1 "GLO"]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndexByName M.empty acts flows
            M.null idx `shouldBe` True

    -- -----------------------------------------------------------------------
    -- fixExchangeLinkByName (SimaPro-style name-only linking)
    -- -----------------------------------------------------------------------
    describe "fixExchangeLinkByName" $ do
        it "resolves input exchange when supplier in index" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = oneProducer "wheat" actUUID1 flowUUID2 ""
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` Just actUUID1
            usFoundLinks summary `shouldBe` 1
            usMissingLinks summary `shouldBe` 0

        it "leaves exchange unlinked when supplier not in index" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.empty
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` Nothing
            usMissingLinks summary `shouldBe` 1

        it "leaves an input unlinked rather than resolving a prefix of its name" $ do
            -- The nine rows of the Agribalyse 4.0 export that name an ecoinvent
            -- unit process the export does not carry. Answering with the
            -- Chinese market because both start with "Urea" is not an answer.
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Urea {RoW}| urea production | Cut-off, S")]
                idx = oneProducer "urea" actUUID1 flowUUID2 ""
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` Nothing
            usMissingLinks summary `shouldBe` 1

        it "leaves exchange unlinked when flow not in flowDB" $ do
            let flows = M.empty
                idx = M.empty
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` Nothing
            usMissingLinks summary `shouldBe` 1

        it "does not touch output reference exchanges" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = oneProducer "wheat" actUUID1 flowUUID2 ""
                ex = refExchange flowUUID1
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "producer" ex
            techActivityLinkId fixed `shouldBe` Nothing -- unchanged
            usTotalLinks summary `shouldBe` 0 -- not counted
        it "does not touch biosphere exchanges" $ do
            let flows = M.empty
                idx = M.empty
                bioEx =
                    BiosphereExchange
                        { bioFlowId = flowUUID1
                        , bioAmount = 1.0
                        , bioUnitId = UUID.nil
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "act" bioEx
            -- BiosphereExchange is returned unchanged: verify it is still biosphere
            isBiosphereExchange fixed `shouldBe` True
            usTotalLinks summary `shouldBe` 0

        -- Regression: a candidate whose reference-product unit is in a different
        -- dimension than the consumer exchange must NOT be linked — the matrix
        -- builder could not convert it, and forming the link aborts the whole
        -- load. (This is what 'delete then re-export' surfaced on real data: a
        -- piece-counted input fuzzy-matched a mass-counted survivor.)
        it "rejects a dimensionally-incompatible candidate (count input vs mass supplier)" $ do
            let unitItemUUID = read "dddddddd-0000-0000-0000-00000000000a" :: UUID.UUID
                unitKgUUID = read "dddddddd-0000-0000-0000-00000000000b" :: UUID.UUID
                unitDB =
                    M.fromList
                        [ (unitItemUUID, Unit unitItemUUID "item" "item" "")
                        , (unitKgUUID, Unit unitKgUUID "kg" "kg" "")
                        ]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "pump")]
                -- supplier indexed with a mass (kg) reference unit
                idx = oneProducer "pump" actUUID1 flowUUID2 "kg"
                -- consumer wants the pump by the piece (item)
                ex = (inputExchange flowUUID1 "GLO"){techUnitId = unitItemUUID}
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig unitDB idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` Nothing
            usMissingLinks summary `shouldBe` 1

        it "links a dimensionally-compatible candidate (mass input vs mass supplier)" $ do
            let unitKgUUID = read "dddddddd-0000-0000-0000-00000000000b" :: UUID.UUID
                unitDB = M.fromList [(unitKgUUID, Unit unitKgUUID "kg" "kg" "")]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "pump")]
                idx = oneProducer "pump" actUUID1 flowUUID2 "kg"
                ex = (inputExchange flowUUID1 "GLO"){techUnitId = unitKgUUID}
                (fixed, summary) = fixExchangeLinkByName defaultUnitConfig unitDB idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` Just actUUID1
            usFoundLinks summary `shouldBe` 1

        -- A Brightway Excel workbook names the activity each input buys from,
        -- and 51 of the products in one real inventory are made by more than
        -- one activity. The name is what tells them apart.
        let twoProducers =
                M.singleton
                    "electricity"
                    ( namedProducerOf "coal power plant" actUUID1 flowUUID2 ""
                        NE.:| [namedProducerOf "wind power plant" actUUID2 flowUUID2 ""]
                    )
            electricity = M.fromList [(flowUUID1, minimalFlow flowUUID1 "electricity")]
            buying claim = (inputExchange flowUUID1 "GLO"){techSupplierClaim = claim}

        it "links to the activity the input names, not the ranked first" $ do
            let (fixed, summary) =
                    fixExchangeLinkByName defaultUnitConfig M.empty twoProducers electricity "consumer" (buying (ClaimByName "Wind power plant"))
            techActivityLinkId fixed `shouldBe` Just actUUID2
            usAmbiguousProducers summary `shouldBe` []

        it "reports the tie when the input names no activity" $ do
            let (fixed, summary) =
                    fixExchangeLinkByName defaultUnitConfig M.empty twoProducers electricity "consumer" (buying ClaimByProduct)
            techActivityLinkId fixed `shouldBe` Just actUUID1
            map apCandidates (usAmbiguousProducers summary) `shouldBe` [2]
            map apChosen (usAmbiguousProducers summary) `shouldBe` ["coal power plant"]

        it "leaves the input for the cross-database linker when it names an activity this database has not" $ do
            -- A row can name an activity of a database this one only depends
            -- on. Answering it with a local producer of the same product would
            -- link it here and the cross-database linker, whose index answers
            -- on the pair, would never see it.
            let (fixed, summary) =
                    fixExchangeLinkByName defaultUnitConfig M.empty twoProducers electricity "consumer" (buying (ClaimByName "gas power plant"))
            techActivityLinkId fixed `shouldBe` Nothing
            usMissingLinks summary `shouldBe` 1

        it "reports the tie when several activities carry the name the input gives" $ do
            -- Two plants of the same name in two locations: the name narrowed
            -- the field and did not close it, so the ranking chose and says so.
            let sameName =
                    M.singleton
                        "electricity"
                        ( namedProducerOf "wind power plant" actUUID1 flowUUID2 ""
                            NE.:| [namedProducerOf "Wind Power Plant" actUUID2 flowUUID2 ""]
                        )
                (fixed, summary) =
                    fixExchangeLinkByName defaultUnitConfig M.empty sameName electricity "consumer" (buying (ClaimByName "wind power plant"))
            techActivityLinkId fixed `shouldBe` Just actUUID1
            map apCandidates (usAmbiguousProducers summary) `shouldBe` [2]

        it "reports no tie when one activity produces the name" $ do
            let idx = oneProducer "wheat" actUUID1 flowUUID2 ""
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                (_, summary) = fixExchangeLinkByName defaultUnitConfig M.empty idx flows "consumer" (inputExchange flowUUID1 "GLO")
            usAmbiguousProducers summary `shouldBe` []

    -- -----------------------------------------------------------------------
    -- fixEcoSpold1ActivityLinks (name-only fallback, EcoSpold1 style)
    -- -----------------------------------------------------------------------
    describe "fixEcoSpold1ActivityLinks" $ do
        let consumerUUID = read "cccccccc-0000-0000-0000-000000000003" :: UUID.UUID
            breadUUID = read "bbbbbbbb-0000-0000-0000-000000000003" :: UUID.UUID
            wheatFR = ((actUUID1, flowUUID1), minimalActivity "wheat production" "FR" [refExchange flowUUID1])
            wheatDE = ((actUUID2, flowUUID2), minimalActivity "wheat production" "DE" [refExchange flowUUID2])
            bread =
                ( (consumerUUID, breadUUID)
                , minimalActivity "bread production" "CH" [refExchange breadUUID, inputExchange flowUUID1 ""]
                )
            flowNames = [(flowUUID1, "Wheat"), (flowUUID2, "Wheat"), (breadUUID, "Bread")]
            -- The suppliers the consumer's inputs end up naming.
            inputLinksIn acts =
                [ link
                | Just act <- [M.lookup (consumerUUID, breadUUID) acts]
                , TechnosphereExchange{techRole = Input, techActivityLinkId = link} <- exchanges act
                ]
            wheatLink = inputLinksIn . sdbActivities

        it "leaves an unlocated input unlinked when the product name covers several geographies" $ do
            fixed <- fixEcoSpold1ActivityLinks M.empty M.empty (simpleDBOf [wheatFR, wheatDE, bread] flowNames)
            wheatLink fixed `shouldBe` [Nothing]

        it "links an unlocated input when the product name covers one dataset" $ do
            fixed <- fixEcoSpold1ActivityLinks M.empty M.empty (simpleDBOf [wheatFR, bread] flowNames)
            wheatLink fixed `shouldBe` [Just actUUID1]

        -- BAFU 2026 v1 has power plants whose gas input carries the number of
        -- their own country's gas supply and the label RER (volca#347). The number
        -- is what the file links, and the official results follow it.
        let gasBG = ((actUUID1, flowUUID1), minimalActivity "gas supply" "BG" [refExchange flowUUID1])
            gasRER = ((actUUID2, flowUUID2), minimalActivity "gas supply" "RER" [refExchange flowUUID2])
            plantDeclaring loc =
                ( (consumerUUID, breadUUID)
                , minimalActivity "power plant" "BG" [refExchange breadUUID, buyingDataset 300474 (inputExchange flowUUID1 loc)]
                )
            gasNames = [(flowUUID1, "Natural gas"), (flowUUID2, "Natural gas"), (breadUUID, "Heat")]
            linkPlantDeclaring loc =
                let db = simpleDBOf [gasBG, gasRER, plantDeclaring loc] gasNames
                    ctx = ecoSpold1LinkContext M.empty (M.singleton 300474 ((actUUID1, flowUUID1) NE.:| [])) db
                 in fixAllActivities ctx (sdbActivities db)

        -- The number is on the row that carries it. Held in one map per
        -- database keyed by the flow, it said the same thing (the flow
        -- identifier is minted from that very number), but a reader had to
        -- know that to trust it.
        it "gives each consumer of one product the supplier its own number names" $ do
            let plantBuying n loc =
                    minimalActivity ("power plant " <> loc) loc [refExchange breadUUID, buyingDataset n (inputExchange flowUUID1 "")]
                plantBG = ((consumerUUID, breadUUID), plantBuying 1 "BG")
                plantRER = ((missingActUUID, flowUUID2), plantBuying 2 "RER")
                db = simpleDBOf [gasBG, gasRER, plantBG, plantRER] gasNames
                ctx =
                    ecoSpold1LinkContext
                        M.empty
                        (M.fromList [(1, (actUUID1, flowUUID1) NE.:| []), (2, (actUUID2, flowUUID2) NE.:| [])])
                        db
                (acts, _) = fixAllActivities ctx (sdbActivities db)
                supplierOf key =
                    [ link
                    | Just act <- [M.lookup key acts]
                    , TechnosphereExchange{techRole = Input, techActivityLinkId = link} <- exchanges act
                    ]
            supplierOf (consumerUUID, breadUUID) `shouldBe` [Just actUUID1]
            supplierOf (missingActUUID, flowUUID2) `shouldBe` [Just actUUID2]

        it "follows the dataset number over the declared location, and records the override" $ do
            let (acts, summary) = linkPlantDeclaring "RER"
            inputLinksIn acts `shouldBe` [Just actUUID1]
            usLocationOverrides summary
                `shouldBe` [ LocationOverride
                                { loConsumer = "power plant"
                                , loConsumerLocation = "BG"
                                , loFlowName = "Natural gas"
                                , loDeclared = "RER"
                                , loLinked = "BG"
                                , loDatasetNumber = 300474
                                }
                           ]

        it "records no override when the declared location names no dataset" $ do
            let (acts, summary) = linkPlantDeclaring "ENTSO"
            inputLinksIn acts `shouldBe` [Just actUUID1]
            usLocationOverrides summary `shouldBe` []

        -- One number, several suppliers: the block was allocated into a heat
        -- dataset and an electricity one, both published under number 7. Only
        -- the product name says which the input meant, and nothing else can:
        -- "Heat" is made in two places, so neither the name alone nor the name
        -- with a location the input does not declare reaches a dataset.
        it "links a numbered input to the coproduct its product names" $ do
            let rivalHeatUUID = read "bbbbbbbb-0000-0000-0000-000000000004" :: UUID.UUID
                cogenHeat = ((actUUID1, flowUUID1), minimalActivity "cogeneration, heat" "GLO" [refExchange flowUUID1])
                cogenPower = ((actUUID2, flowUUID2), minimalActivity "cogeneration, electricity" "GLO" [refExchange flowUUID2])
                boilerHeat = ((missingActUUID, rivalHeatUUID), minimalActivity "heat, boiler" "FR" [refExchange rivalHeatUUID])
                greenhouse =
                    ( (consumerUUID, breadUUID)
                    , minimalActivity "greenhouse" "CH" [refExchange breadUUID, buyingDataset 7 (inputExchange flowUUID1 "")]
                    )
                names = [(flowUUID1, "Heat"), (flowUUID2, "Electricity"), (rivalHeatUUID, "Heat"), (breadUUID, "Bread")]
                -- The electricity coproduct is read last, which is the one a
                -- table keeping a single dataset per number would have kept.
                dsIndex = hvDatasetNumbers (harvestOf [numbered (actUUID1, flowUUID1), numbered (actUUID2, flowUUID2)])
                db = simpleDBOf [cogenHeat, cogenPower, boilerHeat, greenhouse] names
                (acts, _) = fixAllActivities (ecoSpold1LinkContext M.empty dsIndex db) (sdbActivities db)
            inputLinksIn acts `shouldBe` [Just actUUID1]

    -- -----------------------------------------------------------------------
    -- countTotalTechInputs / countUnlinkedExchanges / collectUnlinkedProductNames
    -- (integration tests via SAMPLE.min3)
    -- -----------------------------------------------------------------------
    describe "countTotalTechInputs" $ do
        it "counts all technosphere inputs in SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            countTotalTechInputs sdb `shouldSatisfy` (> 0)

    describe "countUnlinkedExchanges" $ do
        it "returns 0 for fully linked SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            countUnlinkedExchanges sdb `shouldBe` 0

    describe "collectUnlinkedProductNames" $ do
        it "returns empty map for fully linked SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            M.null (collectUnlinkedProductNames sdb) `shouldBe` True

    -- A partial EcoSpold2 import carries non-nil activityLinkIds that point at
    -- background activities it doesn't ship. Those links must read as unlinked,
    -- not silently masquerade as resolved internal links (the matrix builder
    -- drops them, so the score would otherwise undercount with no warning).
    describe "countUnlinkedExchanges (producer presence)" $ do
        it "counts a non-nil link to an absent activity as unlinked" $ do
            let consumer = minimalActivity "lyocell fibre" "GLO" [refExchange flowUUID1, linkedInput missingActUUID flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), consumer)] [(flowUUID1, "lyocell fibre"), (flowUUID2, "chemical, inorganic")]
            countUnlinkedExchanges sdb `shouldBe` 1

        it "does not count a non-nil link to a present activity" $ do
            let consumer = minimalActivity "lyocell fibre" "GLO" [refExchange flowUUID1, linkedInput actUUID2 flowUUID2]
                supplier = minimalActivity "chemical, inorganic" "GLO" [refExchange flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), consumer), ((actUUID2, flowUUID2), supplier)] [(flowUUID1, "lyocell fibre"), (flowUUID2, "chemical, inorganic")]
            countUnlinkedExchanges sdb `shouldBe` 0

    describe "collectUnlinkedProductNames (producer presence)" $ do
        it "surfaces the product of a dangling non-nil link" $ do
            let consumer = minimalActivity "lyocell fibre" "GLO" [refExchange flowUUID1, linkedInput missingActUUID flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), consumer)] [(flowUUID1, "lyocell fibre"), (flowUUID2, "chemical, inorganic")]
            collectUnlinkedProductNames sdb `shouldBe` M.fromList [("chemical, inorganic", 1)]

    -- A treatment process's reference input (ReferenceInput) is a self-edge the
    -- matrix builder skips, not a supplier demand. Counting it would drag a
    -- solvable treatment database below 100% complete and wrongly refuse
    -- finalize, so it must stay out of both the total and the unlinked tally.
    describe "reference inputs are not supplier demands" $ do
        it "excludes a treatment ReferenceInput from the input total" $ do
            let treatment = minimalActivity "waste treatment" "GLO" [referenceInput flowUUID1, linkedInput actUUID2 flowUUID2]
                supplier = minimalActivity "electricity" "GLO" [refExchange flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), treatment), ((actUUID2, flowUUID2), supplier)] [(flowUUID1, "waste"), (flowUUID2, "electricity")]
            -- only the linked electricity input is a demand; the ReferenceInput is not
            countTotalTechInputs sdb `shouldBe` 1

        it "does not count a nil-link ReferenceInput as unlinked" $ do
            let treatment = minimalActivity "waste treatment" "GLO" [referenceInput flowUUID1]
                sdb = simpleDBOf [((actUUID1, flowUUID1), treatment)] [(flowUUID1, "waste")]
            countUnlinkedExchanges sdb `shouldBe` 0

    -- ---------------------------------------------------------------------
    -- activityNormFactor — exercises every TechRole branch so the
    -- treatment-process (ReferenceInput) case can't silently regress to
    -- the "no reference output" 1.0 fallback.
    -- ---------------------------------------------------------------------
    describe "activityNormFactor" $ do
        let actUUID = actUUID1
            prodUUID = flowUUID1
            wasteUUID = flowUUID2
            withRole role amt fid =
                TechnosphereExchange
                    { techFlowId = fid
                    , techAmount = amt
                    , techUnitId = UUID.nil
                    , techRole = role
                    , techActivityLinkId = Nothing
                    , techSupplierClaim = ClaimByProduct
                    , techLocation = ""
                    , techComment = Nothing
                    , techPedigree = Nothing
                    , techShare = Nothing
                    , techClassification = M.empty
                    , techProperties = noProperties
                    }
        it "returns the reference output amount for a normal producer" $ do
            let act = minimalActivity "producer" "GLO" [withRole ReferenceProduct 3.0 prodUUID]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 3.0

        it "returns abs(reference-input amount) for a treatment process" $ do
            -- ReferenceInput is the only role that drives the refInputs fallback;
            -- SimaPro waste-treatment processes encode a negative amount.
            let act = minimalActivity "incineration" "GLO" [withRole ReferenceInput (-2.5) wasteUUID]
            activityNormFactor act (actUUID, wasteUUID) `shouldBe` 2.5

        it "falls back to 1.0 when no reference exchange is present" $ do
            let act = minimalActivity "empty" "GLO" [withRole Input 1.0 wasteUUID]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 1.0

        it "subtracts self-loop consumption from the reference output" $ do
            let selfInput =
                    (withRole Input 0.2 prodUUID){techActivityLinkId = Just actUUID}
                refOut = withRole ReferenceProduct 1.0 prodUUID
                act = minimalActivity "self-looper" "GLO" [refOut, selfInput]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 0.8

        it "ignores Coproduct exchanges when computing the norm" $ do
            -- Coproducts are outputs but don't contribute to the activity's
            -- reference-output sum. An activity with only Coproducts (no
            -- ReferenceProduct, no ReferenceInput) hits the 1.0 fallback,
            -- not "sum of all outputs".
            let coproduct = withRole Coproduct 7.0 wasteUUID
                act = minimalActivity "coproduct-only" "GLO" [coproduct]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 1.0

    describe "activity classifications (full-load integration)" $ do
        it "EcoSpold2 SAMPLE.min3: every activity carries a non-empty classification map" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
            any (M.null . activityClassification) activities `shouldBe` False

        it "EcoSpold2 SAMPLE.min3: surfaces ISIC rev.4 ecoinvent values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
                isicValues =
                    [ v
                    | a <- activities
                    , Just v <- [M.lookup "ISIC rev.4 ecoinvent" (activityClassification a)]
                    ]
            isicValues `shouldContain` ["2394:Manufacture of cement"]
            isicValues `shouldContain` ["0810:Quarrying of stone, sand and clay"]

        it "EcoSpold2 SAMPLE.min3: surfaces CPC values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
                cpcValues =
                    [ v
                    | a <- activities
                    , Just v <- [M.lookup "CPC" (activityClassification a)]
                    ]
            cpcValues `shouldBe` ["3744:Cement"]

        it "EcoSpold1 SAMPLE.ecospold1: category and subCategory promoted to classifications" $ do
            db <- loadSampleDatabase "SAMPLE.ecospold1"
            let activities = V.toList $ dbActivities db
            length activities `shouldSatisfy` (>= 1)
            let cls = activityClassification (head activities)
            M.lookup "Category" cls `shouldBe` Just "Energy"
            M.lookup "SubCategory" cls `shouldBe` Just "Electricity"

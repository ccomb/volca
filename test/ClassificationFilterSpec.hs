{-# LANGUAGE OverloadedStrings #-}

{- | Which words each surface reads as an exact classification filter, and what
each matcher then does with it.

The fixture classifies three activities under one system as @food@,
@Food waste@ and @energy@, so the value @food@ selects one activity compared
whole and two compared as a substring. Every surface that decides the mode
(presets, MCP arguments, the REST parameters, the aggregate's
@System=Value[:mode]@ form, delete-by-selection over HTTP and from the CLI)
and every matcher that applies it is asked once each way.
-}
module ClassificationFilterSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (foldM, forM_)
import Data.Aeson (Result (..), Value (..), decodeStrict, fromJSON, toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import Servant (ServerError (..), runHandler)
import Test.Hspec

import API.DatabaseHandlers (deleteActivitiesHandler)
import API.MCP (callTool, noRequestId)
import API.Routes (getActivityAggregate, getActivityConsumers, getActivitySupplyChain, searchActivitiesWithCount)
import API.Types (
    ActivitySummary (..),
    Aggregation (..),
    ConsumersResponse (..),
    DeleteClassFilter (..),
    DeleteSelectionRequest (..),
    DeleteSelectionResponse (..),
    SearchResults (..),
    SupplyChainResponse (..),
 )
import App.Env (AppEnv (..), AppM, runApp)
import CLI.Client (deleteSelectionBody)
import CLI.Types (DbDeleteArgs (..))
import Config (ClassificationEntry (..), ClassificationPreset (..), DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Manager (CachePolicy (..), DatabaseManager (..), LoadedDatabase (..), initDatabaseManager)
import SharedSolver (createSharedSolver)
import Types (
    Activity (..),
    BuildInputs (..),
    Database (..),
    Exchange (..),
    FileReading (..),
    GeographyPolicy (..),
    Licence (..),
    LocationSource (..),
    Reading (..),
    ReferenceConvention (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    findProcessId,
    noDates,
    noDocumentation,
    noProperties,
    packExchanges,
    processIdToText,
 )
import UnitConversion (defaultUnitConfig)
import Usage (anonymous)

spec :: Spec
spec = describe "classification filter match mode" $ do
    describe "MCP classification_match, through search, supply chain and consumers" $ do
        forM_ [(Just "exact", 1), (Just "contains", 2), (Nothing, 2)] $ \(mode, n) ->
            it (show mode <> " selects " <> show n) $ selectedByMode mode n

        it "reads equals and EXACT as exact" $ do
            selectedByMode (Just "equals") 1
            selectedByMode (Just "EXACT") 1

        -- Five processes: the three classified ones, a supplier and a root that are not.
        it "leaves out the matches of not-exact, from supply chain and consumers too" $ do
            (manager, db) <- loadedFixture
            root <- processOf db rootU
            supplier <- processOf db supplierU
            let filterArgs = singleClassification (Just "not-exact")
            search <- tool manager [] "search_activities" filterArgs
            answerAt ["total"] search `shouldBe` Just (count 4)
            chain <- tool manager [] "get_supply_chain" (("process_id", String root) : filterArgs)
            answerAt ["filteredActivities"] chain `shouldBe` Just (count 3)
            consumers <- tool manager [] "get_consumers" (("process_id", String supplier) : filterArgs)
            answerAt ["results", "total"] consumers `shouldBe` Just (count 3)

        it "search_activities leaves out what a not-contains classification names" $ do
            (manager, _) <- loadedFixture
            single <- tool manager [] "search_activities" (singleClassification (Just "not-contains"))
            answerAt ["total"] single `shouldBe` Just (count 3)
            listed <- tool manager [] "search_activities" [("classifications", listedClassifications [("food", "NOT-CONTAINS")])]
            answerAt ["total"] listed `shouldBe` Just (count 3)
            -- Two exclusions on one system both apply; a listed filter without a mode contains.
            both <- tool manager [] "search_activities" [("classifications", listedClassifications [("food", "not-exact"), ("energy", "not-exact")])]
            answerAt ["total"] both `shouldBe` Just (count 3)
            positive <- tool manager [] "search_activities" [("classifications", listedClassifications [("food", "")])]
            answerAt ["total"] positive `shouldBe` Just (count 2)

        it "search_activities answers a search made of exclusions only" $ do
            (manager, _) <- loadedFixture
            byName <- tool manager [] "search_activities" [("exclude_name", toJSON ["food" :: Text])]
            answerAt ["total"] byName `shouldBe` Just (count 3)
            byPlace <- tool manager [] "search_activities" [("exclude_location", toJSON ["glo" :: Text])]
            answerAt ["total"] byPlace `shouldBe` Just (count 0)
            elsewhere <- tool manager [] "search_activities" [("exclude_location", toJSON ["FR" :: Text])]
            answerAt ["total"] elsewhere `shouldBe` Just (count activityTotal)

        it "refuses the single classification and the list together" $ do
            (manager, _) <- loadedFixture
            reply <- tool manager [] "search_activities" (singleClassification (Just "contains") ++ [("classifications", listedClassifications [("food", "not-exact")])])
            refusalOf reply `shouldSatisfy` maybe False (T.isInfixOf "classifications")

        it "refuses an unknown classification_match and names the four" $ do
            (manager, db) <- loadedFixture
            root <- processOf db rootU
            supplier <- processOf db supplierU
            forM_ [("search_activities", []), ("get_supply_chain", [("process_id", String root)]), ("get_consumers", [("process_id", String supplier)])] $ \(name, extra) -> do
                reply <- tool manager [] name (extra ++ singleClassification (Just "maybe"))
                forM_ ["contains", "exact", "not-contains", "not-exact"] $ \word ->
                    refusalOf reply `shouldSatisfy` maybe False (T.isInfixOf word)
            listed <- tool manager [] "search_activities" [("classifications", listedClassifications [("food", "maybe")])]
            refusalOf listed `shouldSatisfy` maybe False (T.isInfixOf "not-exact")

    describe "a preset entry's mode" $
        forM_ [("exact", 1), ("contains", 2), ("Exact", 2)] $ \(mode, n) ->
            it (show mode <> " selects " <> show n) $ do
                (manager, _) <- loadedFixture
                let preset = ClassificationPreset "p" "P" Nothing [ClassificationEntry "category" "food" mode]
                search <- tool manager [preset] "search_activities" [("preset", String "p")]
                answerAt ["total"] search `shouldBe` Just (count n)

    describe "aggregate filter_classification, on MCP and REST" $
        forM_ [("category=food:exact", 1), ("category=food", 2), ("category=food:equals", 2)] $ \(raw, n) ->
            it (show raw <> " counts " <> show n) $ do
                (manager, db) <- loadedFixture
                root <- processOf db rootU
                mcp <-
                    tool
                        manager
                        []
                        "aggregate"
                        [("process_id", String root), ("scope", String "supply_chain"), ("filter_classification", toJSON [raw])]
                answerAt ["filteredCount"] mcp `shouldBe` Just (count n)
                rest <-
                    runRest manager $
                        getActivityAggregate fixtureName root (Just "supply_chain") Nothing Nothing Nothing Nothing Nothing Nothing [raw] Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                fmap aggFilteredCount rest `shouldBe` Right n

    describe "REST classification-mode" $ do
        forM_ [(["exact"], 1), (["equals"], 1), (["EXACT"], 1), (["Contains"], 2)] $ \(modes, n) ->
            it (show modes <> " selects " <> show n) $ do
                (manager, _) <- loadedFixture
                rest <- runRest manager (searchREST ["category"] ["food"] modes)
                fmap srTotal rest `shouldBe` Right n

        it "still reads a missing mode as contains" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (searchREST ["category"] ["food"] [])
            fmap srTotal rest `shouldBe` Right 2

        it "pairs systems with values up to the shorter list" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (searchREST ["category", "category"] ["food"] ["exact"])
            fmap srTotal rest `shouldBe` Right 1

        -- Five processes: the three classified ones, a supplier and a root that are not.
        forM_ [(["not-contains"], 3), (["not-exact"], 4), (["NOT-EXACT"], 4)] $ \(modes, n) ->
            it (show modes <> " leaves out the matches, so " <> show n <> " remain") $ do
                (manager, _) <- loadedFixture
                rest <- runRest manager (searchREST ["category"] ["food"] modes)
                fmap srTotal rest `shouldBe` Right n

        it "applies two exclusions on one system together" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (searchREST ["category", "category"] ["food", "energy"] ["not-exact", "not-exact"])
            fmap srTotal rest `shouldBe` Right 3

        it "keeps a positive filter and an exclusion on one system apart" $ do
            (manager, _) <- loadedFixture
            -- the two that hold "food", less the one that is exactly "food"
            rest <- runRest manager (searchREST ["category", "category"] ["food", "food"] ["contains", "not-exact"])
            fmap (map prsActivityName . srResults) rest `shouldBe` Right ["Food waste"]

        it "keeps everything when a negative classification names a blank value" $ do
            (manager, _) <- loadedFixture
            forM_ ["", "  "] $ \blank -> do
                rest <- runRest manager (searchREST ["category"] [blank] ["not-contains"])
                fmap srTotal rest `shouldBe` Right activityTotal

        it "refuses an unknown mode and names the four" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (searchREST ["category"] ["food"] ["maybe"])
            case rest of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    forM_ ["contains", "exact", "not-contains", "not-exact"] $ \word ->
                        BSL.toStrict (errBody err) `shouldSatisfy` BS.isInfixOf word
                Right _ -> expectationFailure "an unknown mode was accepted"

        it "leaves out the processes of a supply chain and of consumers the same way" $ do
            (manager, db) <- loadedFixture
            root <- processOf db rootU
            supplier <- processOf db supplierU
            chain <- runRest manager (supplyChainREST root ["category"] ["food"] ["not-contains"])
            fmap scrFilteredActivities chain `shouldBe` Right 2
            consumers <- runRest manager (consumersREST supplier ["category"] ["food"] ["not-contains"])
            fmap (srTotal . crrResults) consumers `shouldBe` Right 2

    describe "REST exclude-name and exclude-location" $ do
        it "leave out the rows they name" $ do
            (manager, _) <- loadedFixture
            byName <- runRest manager (excludingREST ["food"] [])
            fmap (sort . map prsActivityName . srResults) byName `shouldBe` Right ["energy", "root", "supplier"]
            byPlace <- runRest manager (excludingREST [] ["glo"])
            fmap srTotal byPlace `shouldBe` Right 0
            elsewhere <- runRest manager (excludingREST [] ["FR"])
            fmap srTotal elsewhere `shouldBe` Right activityTotal

        it "drops a blank exclude-name and a blank exclude-location" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (excludingREST ["", "  "] ["", " "])
            fmap srTotal rest `shouldBe` Right activityTotal
            kept <- runRest manager (excludingREST ["", "energy"] [""])
            fmap srTotal kept `shouldBe` Right (activityTotal - 1)

    describe "REST order" $
        it "reverses the listing on desc and on no other word" $ do
            (manager, _) <- loadedFixture
            let listed order = fmap (map prsProcessId . srResults) <$> runRest manager (orderedREST order)
            plain <- listed Nothing
            plain `shouldNotBe` fmap reverse plain
            listed (Just "desc") `shouldReturn` fmap reverse plain
            forM_ ["asc", "DESC", ""] $ \word -> listed (Just word) `shouldReturn` plain

    describe "REST process" $ do
        it "answers the processes named, in the order named" $ do
            (manager, db) <- loadedFixture
            root <- processOf db rootU
            supplier <- processOf db supplierU
            rest <- runRest manager (namedREST [supplier, root])
            fmap (map prsProcessId . srResults) rest `shouldBe` Right [supplier, root]

        it "lists the processes named over MCP too" $ do
            (manager, db) <- loadedFixture
            root <- processOf db rootU
            search <- tool manager [] "search_activities" [("process", toJSON [root])]
            answerAt ["total"] search `shouldBe` Just (count 1)

        it "refuses an identifier that is not one" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (namedREST ["not an identifier"])
            either (Just . errHTTPCode) (const Nothing) rest `shouldBe` Just 400

    describe "delete-by-selection, over HTTP and from the CLI" $
        forM_ [(True, 1), (False, 2)] $ \(exact, n) ->
            it ("exact " <> show exact <> " removes " <> show n) $ do
                (httpManager, _) <- loadedFixture
                http <- runRest httpManager (deleteActivitiesHandler fixtureName (deleteRequest exact))
                fmap dsrDeleted http `shouldBe` Right n
                -- The command line sends this body to the same route.
                (cliManager, _) <- loadedFixture
                cli <- case fromJSON (deleteSelectionBody (deleteArgs exact)) of
                    Success body -> runRest cliManager (deleteActivitiesHandler fixtureName body)
                    Error err -> fail err
                fmap dsrDeleted cli `shouldBe` Right n
                activitiesLeft cliManager `shouldReturn` Just (activityTotal - n)

-- ---------------------------------------------------------------------------
-- Surfaces
-- ---------------------------------------------------------------------------

tool :: DatabaseManager -> [ClassificationPreset] -> Text -> [(Key, Value)] -> IO Value
tool manager presets name args =
    callTool anonymous manager presets Nothing Nothing noRequestId name (KM.fromList (("database", String fixtureName) : args))

{- | A search, a supply chain and a consumers listing each filtered on
@category = food@ in the given mode, selecting @n@ processes of the three
classified ones.
-}
selectedByMode :: Maybe Text -> Int -> IO ()
selectedByMode mode n = do
    (manager, db) <- loadedFixture
    root <- processOf db rootU
    supplier <- processOf db supplierU
    let filterArgs = singleClassification mode
    search <- tool manager [] "search_activities" filterArgs
    answerAt ["total"] search `shouldBe` Just (count n)
    chain <- tool manager [] "get_supply_chain" (("process_id", String root) : filterArgs)
    answerAt ["filteredActivities"] chain `shouldBe` Just (count n)
    consumers <- tool manager [] "get_consumers" (("process_id", String supplier) : filterArgs)
    answerAt ["results", "total"] consumers `shouldBe` Just (count n)

-- | @category = food@ with the given @classification_match@, if any.
singleClassification :: Maybe Text -> [(Key, Value)]
singleClassification mode =
    [("classification", String "category"), ("classification_value", String "food")]
        ++ maybe [] (\m -> [("classification_match", String m)]) mode

-- | The @classifications@ argument: each value under @category@, in the mode given ("" for none).
listedClassifications :: [(Text, Text)] -> Value
listedClassifications entries =
    toJSON
        [ Object (KM.fromList (("system", "category") : ("value", String v) : [("mode", String m) | not (T.null m)]))
        | (v, m) <- entries
        ]

-- | The message of a tool reply that refused.
refusalOf :: Value -> Maybe Text
refusalOf reply = do
    Object o <- Just reply
    Object r <- KM.lookup "result" o
    Bool True <- KM.lookup "isError" r
    Array content <- KM.lookup "content" r
    Object c <- listToMaybe (toList content)
    String t <- KM.lookup "text" c
    pure t

-- | A field of a tool reply's JSON payload, followed down the given keys.
answerAt :: [Key] -> Value -> Maybe Value
answerAt path reply = do
    Object o <- Just reply
    Object r <- KM.lookup "result" o
    Array content <- KM.lookup "content" r
    Object c <- listToMaybe (toList content)
    String t <- KM.lookup "text" c
    payload <- decodeStrict (encodeUtf8 t)
    foldM field payload path
  where
    field :: Value -> Key -> Maybe Value
    field (Object m) k = KM.lookup k m
    field _ _ = Nothing

count :: Int -> Value
count = Number . fromIntegral

runRest :: DatabaseManager -> AppM a -> IO (Either ServerError a)
runRest manager = runHandler . runApp env
  where
    env :: AppEnv
    env =
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            , aeUsageLog = Nothing
            , aeReader = Nothing
            , aeAuthor = Nothing
            }

searchREST :: [Text] -> [Text] -> [Text] -> AppM (SearchResults ActivitySummary)
searchREST systems values modes =
    searchActivitiesWithCount fixtureName Nothing Nothing Nothing Nothing Nothing systems values modes [] [] [] Nothing Nothing Nothing Nothing

-- | A search that leaves out the given names and places, and asks for nothing.
excludingREST :: [Text] -> [Text] -> AppM (SearchResults ActivitySummary)
excludingREST names places =
    searchActivitiesWithCount fixtureName Nothing Nothing Nothing Nothing Nothing [] [] [] names places [] Nothing Nothing Nothing Nothing

supplyChainREST :: Text -> [Text] -> [Text] -> [Text] -> AppM SupplyChainResponse
supplyChainREST root systems values modes =
    getActivitySupplyChain fixtureName root Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing systems values modes Nothing Nothing Nothing

consumersREST :: Text -> [Text] -> [Text] -> [Text] -> AppM ConsumersResponse
consumersREST supplier systems values modes =
    getActivityConsumers fixtureName supplier Nothing Nothing Nothing Nothing systems values modes Nothing Nothing Nothing Nothing Nothing Nothing

orderedREST :: Maybe Text -> AppM (SearchResults ActivitySummary)
orderedREST =
    searchActivitiesWithCount fixtureName Nothing Nothing Nothing Nothing Nothing [] [] [] [] [] [] Nothing Nothing Nothing

namedREST :: [Text] -> AppM (SearchResults ActivitySummary)
namedREST processes =
    searchActivitiesWithCount fixtureName Nothing Nothing Nothing Nothing Nothing [] [] [] [] [] processes Nothing Nothing Nothing Nothing

deleteRequest :: Bool -> DeleteSelectionRequest
deleteRequest exact =
    DeleteSelectionRequest
        { dsqName = Nothing
        , dsqLocation = Nothing
        , dsqProduct = Nothing
        , dsqClassifications = [DeleteClassFilter "category" "food" exact]
        , dsqExact = Nothing
        , dsqKeep = []
        , dsqExtra = []
        , dsqIds = Nothing
        , dsqNote = Nothing
        , dsqSource = Nothing
        }

deleteArgs :: Bool -> DbDeleteArgs
deleteArgs exact =
    DbDeleteArgs
        { ddaDb = fixtureName
        , ddaName = Nothing
        , ddaLocation = Nothing
        , ddaProduct = Nothing
        , ddaClassSystem = Just "category"
        , ddaClassValue = Just "food"
        , ddaExact = exact
        , ddaKeep = []
        , ddaExtra = []
        , ddaIds = []
        }

activitiesLeft :: DatabaseManager -> IO (Maybe Int)
activitiesLeft manager =
    fmap (fromIntegral . dbActivityCount . ldDatabase) . M.lookup fixtureName <$> readTVarIO (dmLoadedDbs manager)

-- ---------------------------------------------------------------------------
-- Fixture
-- ---------------------------------------------------------------------------

fixtureName :: Text
fixtureName = "classified"

supplierU, foodU, foodWasteU, energyU, rootU :: UUID
supplierU = mkUUID 1
foodU = mkUUID 11
foodWasteU = mkUUID 21
energyU = mkUUID 31
rootU = mkUUID 41

activityTotal :: Int
activityTotal = 5

{- | A supplier every classified activity consumes, and a root consuming all
three, so a supply chain from the root and the consumers of the supplier both
reach the three classified activities and one unclassified one.
-}
fixture :: SimpleDatabase
fixture =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [ ((supplierU, supplierU), mkActivity "supplier" M.empty [refOut supplierU])
                , ((foodU, foodU), classified "food" foodU)
                , ((foodWasteU, foodWasteU), classified "Food waste" foodWasteU)
                , ((energyU, energyU), classified "energy" energyU)
                , ((rootU, rootU), mkActivity "root" M.empty (refOut rootU : map inputFrom [foodU, foodWasteU, energyU]))
                ]
        , sdbTechFlows =
            M.fromList [(u, techFlow u) | u <- [supplierU, foodU, foodWasteU, energyU, rootU]]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton kgUnitId (Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""})
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        , sdbProductSystems = []
        , sdbReferenceConvention = SignedReferences
        }
  where
    classified :: Text -> UUID -> Activity
    classified value u = mkActivity value (M.singleton "category" value) [refOut u, inputFrom supplierU]

loadedFixture :: IO (DatabaseManager, Database)
loadedFixture = do
    manager <- initDatabaseManager defaultConfig NoCache
    db <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty ReadDeclared []) fixture >>= either (fail . show) pure
    solver <-
        createSharedSolver
            fixtureName
            (dbTechnosphereTriples db)
            (fromIntegral (dbActivityCount db))
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = fixtureConfig, ldSupplierMapping = Nothing}
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert fixtureName loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert fixtureName fixtureConfig)
    pure (manager, db)

processOf :: Database -> UUID -> IO Text
processOf db u = maybe (fail ("no process for " <> show u)) (pure . processIdToText db) (findProcessId db u u)

fixtureConfig :: DatabaseConfig
fixtureConfig =
    DatabaseConfig
        { dcName = fixtureName
        , dcDisplayName = fixtureName
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = Nothing
        , dcRequires = []
        , dcAdded = Nothing
        }

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 0

techFlow :: UUID -> TechnosphereFlow
techFlow u =
    TechnosphereFlow
        { tfId = u
        , tfName = T.pack (show u)
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

-- | The reference product of an activity whose product UUID is its own.
refOut :: UUID -> Exchange
refOut = technosphere ReferenceProduct 1.0

-- | An input from the activity whose product UUID is its own.
inputFrom :: UUID -> Exchange
inputFrom = technosphere Input 0.5

technosphere :: TechRole -> Double -> UUID -> Exchange
technosphere role amount u =
    TechnosphereExchange
        { techFlowId = u
        , techAmount = amount
        , techUnitId = kgUnitId
        , techRole = role
        , techActivityLinkId = Just u
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

mkActivity :: Text -> M.Map Text Text -> [Exchange] -> Activity
mkActivity name classification exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = classification
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , activityLines = packExchanges exs
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

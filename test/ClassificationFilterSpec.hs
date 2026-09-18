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
import Data.Aeson (Value (..), decodeStrict, toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Servant (ServerError, runHandler)
import Test.Hspec

import API.DatabaseHandlers (deleteActivitiesHandler)
import API.MCP (callTool)
import API.Routes (getActivityAggregate, searchActivitiesWithCount)
import API.Types (
    ActivitySummary,
    Aggregation (..),
    DeleteClassFilter (..),
    DeleteSelectionRequest (..),
    DeleteSelectionResponse (..),
    SearchResults (..),
 )
import App.Env (AppEnv (..), AppM, runApp)
import CLI.Command (executeDbDeleteActivities)
import CLI.Types (DbDeleteArgs (..), OutputFormat (..))
import Config (ClassificationEntry (..), ClassificationPreset (..), DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Manager (CachePolicy (..), DatabaseManager (..), LoadedDatabase (..), initDatabaseManager)
import SharedSolver (createSharedSolver)
import Types (
    Activity (..),
    AllocationKey (..),
    BuildInputs (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    findProcessId,
    noProperties,
    processIdToText,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "classification filter match mode" $ do
    describe "MCP classification_match, through search, supply chain and consumers" $
        forM_ [(Just "exact", 1), (Just "equals", 1), (Just "EXACT", 2), (Nothing, 2)] $ \(mode, n) ->
            it (show mode <> " selects " <> show n) $ do
                (manager, db) <- loadedFixture
                root <- processOf db rootU
                supplier <- processOf db supplierU
                let filterArgs =
                        [("classification", String "category"), ("classification_value", String "food")]
                            ++ maybe [] (\m -> [("classification_match", String m)]) mode
                search <- tool manager [] "search_activities" filterArgs
                answerAt ["total"] search `shouldBe` Just (count n)
                chain <- tool manager [] "get_supply_chain" (("process_id", String root) : filterArgs)
                answerAt ["filteredActivities"] chain `shouldBe` Just (count n)
                consumers <- tool manager [] "get_consumers" (("process_id", String supplier) : filterArgs)
                answerAt ["results", "total"] consumers `shouldBe` Just (count n)

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
        forM_ [(["exact"], 1), ([], 2), (["equals"], 2)] $ \(modes, n) ->
            it (show modes <> " selects " <> show n) $ do
                (manager, _) <- loadedFixture
                rest <- runRest manager (searchREST ["category"] ["food"] modes)
                fmap srTotal rest `shouldBe` Right n

        it "pairs systems with values up to the shorter list" $ do
            (manager, _) <- loadedFixture
            rest <- runRest manager (searchREST ["category", "category"] ["food"] ["exact"])
            fmap srTotal rest `shouldBe` Right 1

    describe "delete-by-selection, over HTTP and from the CLI" $
        forM_ [(True, 1), (False, 2)] $ \(exact, n) ->
            it ("exact " <> show exact <> " removes " <> show n) $ do
                (httpManager, _) <- loadedFixture
                http <- runRest httpManager (deleteActivitiesHandler fixtureName (deleteRequest exact))
                fmap dsrDeleted http `shouldBe` Right n
                (cliManager, _) <- loadedFixture
                executeDbDeleteActivities JSON cliManager (deleteArgs exact)
                activitiesLeft cliManager `shouldReturn` Just (activityTotal - n)

-- ---------------------------------------------------------------------------
-- Surfaces
-- ---------------------------------------------------------------------------

tool :: DatabaseManager -> [ClassificationPreset] -> Text -> [(Key, Value)] -> IO Value
tool manager presets name args =
    callTool manager presets Nothing Nothing Null name (KM.fromList (("database", String fixtureName) : args))

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
            }

searchREST :: [Text] -> [Text] -> [Text] -> AppM (SearchResults ActivitySummary)
searchREST systems values modes =
    searchActivitiesWithCount fixtureName Nothing Nothing Nothing Nothing Nothing systems values modes Nothing Nothing Nothing Nothing

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
        }
  where
    classified :: Text -> UUID -> Activity
    classified value u = mkActivity value (M.singleton "category" value) [refOut u, inputFrom supplierU]

loadedFixture :: IO (DatabaseManager, Database)
loadedFixture = do
    manager <- initDatabaseManager defaultConfig NoCache
    db <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared []) fixture >>= either (fail . show) pure
    solver <-
        createSharedSolver
            fixtureName
            [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
            (fromIntegral (dbActivityCount db))
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = fixtureConfig}
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
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
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
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

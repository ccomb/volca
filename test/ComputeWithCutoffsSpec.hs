{-# LANGUAGE OverloadedStrings #-}

{- | A database whose inputs stay unsupplied is computed, and says so.

The engine used to refuse any calculation on a database the linker left
products unresolved in. It now solves it, counting each unsupplied input as
zero, and every answer carries the list of the ones its chain met. The fixture
is the one 'CutoffsSpec' works out by hand: R takes 2 kg of P and 0.5 kg of M,
P takes 3 kg of M, nobody makes M, so solving R meets 6.5 kg of M asked by two
processes. Z needs nothing, and D asks 0.5 kg of N from an activity no database
holds.
-}
module ComputeWithCutoffsSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import API.MCP (callTool, noRequestId)
import API.Routes (
    getActivityAggregate,
    getActivityInventory,
    getActivityLCIA,
    getActivityLCIABatch,
    getActivitySupplyChain,
    getContributingActivities,
    getContributingFlows,
    getScoreContributingActivities,
    getScoreContributingFlows,
    postActivitySensitivity,
 )
import API.Types
import App.Env (AppM)
import Config (defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (emptyAliasMap)
import Database.Loader (relinkSimpleDatabase)
import qualified Database.Manager as DM
import DependencyFlowClosureSpec (install)
import DependencyLicenceSpec (climate, collection, configFor, own, runIn)
import FlowContributionSpec (payloadOf)
import Method.Types (Method (..), MethodCollection (..), ScoringSet (..), ScoringSetOrigin (..))
import SynonymDB (emptySynonymDB)
import TestHelpers (linkDatabases, mkActivity, mkTechFlow, reference, techInput, units)
import Types
import UnitConversion (defaultUnitConfig)

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000002" <> suffix)

rFlow, pFlow, mFlow, nFlow, zFlow, dFlow, kFlow :: UUID
rFlow = u "01"
pFlow = u "02"
mFlow = u "03"
nFlow = u "04"
zFlow = u "05"
dFlow = u "06"
kFlow = u "07"

actR, actP, actZ, actD, actS, absentAct :: UUID
actR = u "a1"
actP = u "a2"
actZ = u "a3"
actD = u "a4"
actS = u "a5"
absentAct = u "ff" -- named by a source identity, held by no database

linkedTo :: UUID -> UUID -> Double -> Exchange
linkedTo act fid amount = (techInput fid amount){techActivityLinkId = Just act, techSupplierClaim = ClaimById act}

simple :: [((UUID, UUID), Activity)] -> SimpleDatabase
simple acts =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows =
            M.fromList
                [ (f, mkTechFlow f n)
                | (f, n) <- [(rFlow, "R"), (pFlow, "P"), (mFlow, "M"), (nFlow, "N"), (zFlow, "Z"), (dFlow, "D"), (kFlow, "K")]
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = units
        , sdbDocumentation = noDocumentation
        }

{- | The database as the loader leaves it: its linking statistics name what
stayed unresolved, which is what the refusal used to read.
-}
build :: SimpleDatabase -> IO Database
build sdb = do
    db <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig M.empty Declared []) sdb >>= either (fail . show) pure
    pure db{dbLinkingStats = relinkSimpleDatabase [] emptySynonymDB defaultUnitConfig M.empty GeoGlobal emptyAliasMap sdb}

mainSdb :: SimpleDatabase
mainSdb =
    simple
        [ ((actR, rFlow), mkActivity "R" [reference rFlow, linkedTo actP pFlow 2, techInput mFlow 0.5])
        , ((actP, pFlow), mkActivity "P" [reference pFlow, techInput mFlow 3])
        , ((actZ, zFlow), mkActivity "Z" [reference zFlow])
        , ((actD, dFlow), mkActivity "D" [reference dFlow, linkedTo absentAct nFlow 0.5])
        ]

-- | A single score on the one method, so the score breakdowns have a score to break down.
single :: ScoringSet
single =
    ScoringSet
        { ssName = "Single"
        , ssUnit = "Pts"
        , ssVariables = M.fromList [("cc", methodName climate)]
        , ssComputed = M.empty
        , ssLabels = M.empty
        , ssNormalization = M.empty
        , ssWeighting = M.empty
        , ssScores = M.fromList [("total", "cc")]
        , ssDisplayMultiplier = Nothing
        , ssUnits = M.empty
        , ssOrigin = DeclaredInConfig
        }

-- | The databases given loaded under their licences, with the one-method collection.
managerOf :: [(Text, Licence, Database)] -> IO DM.DatabaseManager
managerOf dbs = do
    manager <- DM.initDatabaseManager defaultConfig DM.NoCache
    mapM_ (\(name, licence, _) -> DM.addDatabase manager (configFor name licence)) dbs
    mapM_ (\(name, _, db) -> install manager name db) dbs
    atomically $ modifyTVar' (DM.dmLoadedMethods manager) (M.insert collection (MethodCollection [climate] [single] []))
    pure manager

mainManager :: IO (DM.DatabaseManager, Database)
mainManager = do
    db <- build mainSdb
    manager <- managerOf [("main", LicenceUnstated, db)]
    pure (manager, db)

pidOf :: Database -> (UUID, UUID) -> IO Text
pidOf db key = maybe (fail "fixture process not interned") (pure . processIdToText db) (M.lookup key (dbProcessIdLookup db))

methodText :: Text
methodText = UUID.toText (methodId climate)

coll :: DM.CollectionName
coll = DM.CollectionName collection

ok :: DM.DatabaseManager -> AppM a -> IO a
ok manager handler = runIn manager handler >>= either (\code -> fail ("HTTP " <> show code)) pure

noNameMatch :: NE.NonEmpty BlockerReason
noNameMatch = NE.singleton (BlockerReason "no_name_match" Nothing)

cutoff :: Text -> Text -> Double -> Int -> NE.NonEmpty BlockerReason -> CutoffInput
cutoff db product amount consumers reasons =
    CutoffInput
        { ciDatabase = db
        , ciProduct = product
        , ciLocation = "FR"
        , ciUnit = "kg"
        , ciAmount = amount
        , ciConsumers = consumers
        , ciReasons = reasons
        }

-- | What solving R meets: 0.5 kg of M for R itself and 2 × 3 for P.
metByR :: [CutoffInput]
metByR = [cutoff "main" "M" 6.5 2 noNameMatch]

{- | Amounts rounded to a billionth, so a sum the solver carries to the last bit
compares with the one worked out by hand.
-}
rounded :: [CutoffInput] -> [CutoffInput]
rounded = map (\c -> c{ciAmount = fromIntegral (round (ciAmount c * 1e9) :: Integer) / 1e9})

-- | Every REST surface that solves, asked about one process, read down to its list.
surfaces :: DM.DatabaseManager -> Text -> [(String, IO [CutoffInput])]
surfaces manager pid =
    map
        (fmap (fmap rounded))
        [ ("inventory", ieCutoffInputs <$> ok manager (getActivityInventory "main" pid))
        , ("single-method score", lrCutoffInputs <$> ok manager (getActivityLCIA "main" pid coll methodText Nothing))
        , ("score panel", lbrCutoffInputs <$> ok manager (getActivityLCIABatch "main" pid coll Nothing))
        , ("sensitivity baseline", srCutoffInputs <$> ok manager (postActivitySensitivity "main" pid coll methodText (SensitivityRequest [])))
        , ("contributing flows", cfrCutoffInputs <$> ok manager (getContributingFlows "main" pid coll methodText Nothing Nothing))
        , ("contributing activities", carCutoffInputs <$> ok manager (getContributingActivities "main" pid coll methodText Nothing Nothing))
        , ("flows of a single score", cfrCutoffInputs <$> ok manager (getScoreContributingFlows "main" pid coll "Single" "total" Nothing Nothing))
        , ("activities of a single score", carCutoffInputs <$> ok manager (getScoreContributingActivities "main" pid coll "Single" "total" Nothing Nothing))
        , ("supply chain", scrCutoffInputs <$> ok manager (getActivitySupplyChain "main" pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing Nothing))
        , ("supply-chain aggregate", aggCutoffInputs <$> aggregateIn manager pid "supply_chain")
        , ("biosphere aggregate", aggCutoffInputs <$> aggregateIn manager pid "biosphere")
        , ("consumption aggregate", aggCutoffInputs <$> aggregateIn manager pid "consumption")
        ]

aggregateIn :: DM.DatabaseManager -> Text -> Text -> IO Aggregation
aggregateIn manager pid scope =
    ok manager (getActivityAggregate "main" pid (Just scope) Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

-- | Call an MCP tool on the main database, and read its payload.
mcp :: DM.DatabaseManager -> Text -> [(Key, Value)] -> IO Object
mcp manager tool args = do
    reply <-
        callTool manager [] Nothing Nothing noRequestId tool $
            KM.fromList ([("database", String "main"), ("collection", String collection), ("method_id", String methodText)] ++ args)
    maybe (fail ("unexpected " <> T.unpack tool <> " reply: " <> show reply)) pure (payloadOf reply)

spec :: Spec
spec = describe "computing a database whose inputs stay unsupplied" $ do
    it "starts from a database the old refusal would have turned away" $ do
        (_, db) <- mainManager
        unresolvedCount (dbLinkingStats db) `shouldSatisfy` (> 0)

    describe "a chain meeting unsupplied inputs" $ do
        (manager, db) <- runIO mainManager
        pid <- runIO (pidOf db (actR, rFlow))
        mapM_ (\(name, run) -> it ("answers the " <> name <> " and names what it counted as zero") (run `shouldReturn` metByR)) (surfaces manager pid)

        it "gives each method of the panel the panel's list" $ do
            panel <- ok manager (getActivityLCIABatch "main" pid coll Nothing)
            map (rounded . lrCutoffInputs) (lbrResults panel) `shouldBe` [metByR]

        it "gives nothing for the direct aggregate, which solves nothing" $
            (aggCutoffInputs <$> aggregateIn manager pid "direct") `shouldReturn` []

    describe "a sibling whose chain meets none" $ do
        (manager, db) <- runIO mainManager
        pid <- runIO (pidOf db (actZ, zFlow))
        mapM_ (\(name, run) -> it ("answers the " <> name <> " with an empty list") (run `shouldReturn` [])) (surfaces manager pid)

    it "lists an input named by an activity no database holds" $ do
        (manager, db) <- mainManager
        pid <- pidOf db (actD, dFlow)
        ieCutoffInputs <$> ok manager (getActivityInventory "main" pid)
            `shouldReturn` [cutoff "main" "N" 0.5 1 (NE.singleton (BlockerReason "dangling_source_identity" Nothing))]

    it "counts, without naming it, the unsupplied input inside a dependency whose licence keeps its detail" $ do
        -- R's 0.5 kg of M goes to S in the dependency, run at 0.5, which takes 1 kg of K nobody makes.
        supplier <- build (simple [((actS, mFlow), mkActivity "S" [reference mFlow, techInput kFlow 1])])
        consumer <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, techInput mFlow 0.5])])
        let linked = linkDatabases consumer supplier "dep" 0.5
            rootDb = linked{dbCrossDBLinks = map (\l -> l{cdlConsumerFlowId = mFlow}) (dbCrossDBLinks linked), dbDependsOn = ["dep"]}
        manager <- managerOf [("dep", own [SeeDetailedScores], supplier), ("main", LicenceUnstated, rootDb)]
        pid <- pidOf rootDb (actR, rFlow)
        result <- ok manager (getActivityLCIA "main" pid coll methodText Nothing)
        (lrCutoffInputs result, lrWithheldCutoffs result) `shouldBe` ([], [WithheldCutoffs "dep" 1])

    describe "the MCP tools" $ do
        (manager, db) <- runIO mainManager
        pidR <- runIO (pidOf db (actR, rFlow))
        pidZ <- runIO (pidOf db (actZ, zFlow))

        it "open get_inventory with a notice naming the input and its amount" $ do
            payload <- mcp manager "get_inventory" [("process_id", String pidR)]
            KM.lookup "cutoff_notice" payload
                `shouldBe` Just (String "This result counts 1 input no loaded database supplies as zero: M (6.500 kg); load the database that makes them, or see the gap report.")

        it "say nothing when the chain meets none" $ do
            payload <- mcp manager "get_inventory" [("process_id", String pidZ)]
            KM.member "cutoff_notice" payload `shouldBe` False

        it "count each row's cut-offs in a score_activities column" $ do
            payload <- mcp manager "score_activities" [("process_ids", Array (V.fromList [String pidR, String pidZ]))]
            let column = do
                    Array columns <- KM.lookup "columns" payload
                    lookup (String "cutoffs") (zip (toList columns) [0 :: Int ..])
                cells = do
                    i <- column
                    Array rows <- KM.lookup "rows" payload
                    traverse (cellAt i) (toList rows)
            cells `shouldBe` Just [Number 1, Number 0]
            KM.member "cutoff_notice" payload `shouldBe` True

        it "give score_activity its list once, not once per method" $ do
            payload <- mcp manager "score_activity" [("process_id", String pidR)]
            KM.member "cutoff_notice" payload `shouldBe` True
            (length <$> listAt "cutoffInputs" payload) `shouldBe` Just 1
            let perMethod = do
                    Array results <- KM.lookup "results" payload
                    pure [KM.member "cutoffInputs" o | Object o <- toList results]
            perMethod `shouldBe` Just [False]
  where
    cellAt :: Int -> Value -> Maybe Value
    cellAt i row = do
        Array cs <- Just row
        cs V.!? i

    listAt :: Key -> Object -> Maybe [Value]
    listAt key o = do
        Array xs <- KM.lookup key o
        pure (toList xs)

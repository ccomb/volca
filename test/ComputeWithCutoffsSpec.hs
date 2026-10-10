{-# LANGUAGE OverloadedStrings #-}

{- | A database whose inputs stay unsupplied is computed, and says so.

The engine used to refuse any calculation on a database the linker left
products unresolved in. It now solves it, counting each unsupplied input as
zero, and every answer carries the list of the ones its chain met. The fixture
is the one 'CutoffsSpec' works out by hand: R takes 2 kg of P and 0.5 kg of M,
P takes 3 kg of M, nobody makes M, so solving R meets 6.5 kg of M asked by two
processes. P also emits 1 kg of carbon dioxide per kilogram, which the method
weighs at 1. Z needs nothing, and D asks 0.5 kg of N from an activity no database
holds.
-}
module ComputeWithCutoffsSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Aeson (Object, Value (..), object, (.=))
import Data.Aeson.Key (Key, fromText)
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
import API.Types hiding (bioDirection)
import App.Env (AppM)
import Config (defaultConfig)
import CrossDBRegionalLCIAFixture (testFlow)
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (emptyAliasMap)
import Database.Loader (relinkSimpleDatabase)
import qualified Database.Manager as DM
import DependencyFlowClosureSpec (install)
import DependencyLicenceSpec (climate, collection, configFor, own, runIn)
import FlowContributionSpec (payloadOf)
import Method.Types (Method (..), MethodCollection (..), ScoringSet (..), ScoringSetOrigin (..))
import SynonymDB (emptySynonymDB)
import TestHelpers (kgUnit, linkDatabases, mkActivity, mkTechFlow, reference, techInput, units)
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

-- | The flow the method weighs at 1, in the unit these fixtures use.
co2 :: BiosphereFlow
co2 = testFlow{bfUnitId = kgUnit}

emits :: Double -> Exchange
emits amount =
    BiosphereExchange
        { bioFlowId = bfId co2
        , bioAmount = amount
        , bioUnitId = kgUnit
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

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
        , sdbBioFlows = M.singleton (bfId co2) co2
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
        , ((actP, pFlow), mkActivity "P" [reference pFlow, techInput mFlow 3, emits 1])
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
rounded = map (\c -> c{ciAmount = billionth (ciAmount c)})

billionth :: Double -> Double
billionth x = fromIntegral (round (x * 1e9) :: Integer) / 1e9

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

{- | R buys its 0.5 kg of M from S in a dependency keeping its detail; S, run at
0.5, takes 1 kg of K nobody makes.
-}
withheldDependency :: IO DM.DatabaseManager
withheldDependency = do
    supplier <- build (simple [((actS, mFlow), mkActivity "S" [reference mFlow, techInput kFlow 1])])
    consumer <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, techInput mFlow 0.5])])
    let linked = linkDatabases consumer supplier "dep" 0.5
        rootDb = linked{dbCrossDBLinks = map (\l -> l{cdlConsumerFlowId = mFlow}) (dbCrossDBLinks linked), dbDependsOn = ["dep"]}
    managerOf [("dep", own [SeeDetailedScores], supplier), ("main", LicenceUnstated, rootDb)]

rootOf :: DM.DatabaseManager -> IO Database
rootOf manager = DM.getDatabase manager "main" >>= maybe (fail "main not loaded") (pure . DM.ldDatabase)

{- | The product, consumer count and amount (to a millionth) of each entry of an
MCP list of cut-off inputs.
-}
inputsOf :: Value -> Maybe [(Value, Value, Integer)]
inputsOf v = do
    Array xs <- Just v
    traverse entry (toList xs)
  where
    entry :: Value -> Maybe (Value, Value, Integer)
    entry e = do
        Object o <- Just e
        product <- KM.lookup "product" o
        consumers <- KM.lookup "consumers" o
        Number amount <- KM.lookup "amount" o
        pure (product, consumers, round (realToFrac amount * 1e6 :: Double))

-- | What 'inputsOf' reads for the inputs solving R meets.
metByRInputs :: Maybe [(Value, Value, Integer)]
metByRInputs = Just [(String "M", Number 2, 6500000)]

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

        -- x_R = 1 and x_P = 2, R taking 2 kg of P; P emits 1 kg per kilogram, so 2 kg in
        -- all. The 6.5 kg of M nobody makes bring nothing, and the method weighs the
        -- carbon dioxide at 1: a score of 2.
        it "solves the chain with the unsupplied input at zero" $ do
            inventory <- ok manager (getActivityInventory "main" pid)
            [(bfId (ifdFlow f), billionth (ifdQuantity f)) | f <- ieFlows inventory] `shouldBe` [(bfId co2, 2)]
            billionth . lrScore <$> ok manager (getActivityLCIA "main" pid coll methodText Nothing) `shouldReturn` 2

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
        manager <- withheldDependency
        rootDb <- rootOf manager
        pid <- pidOf rootDb (actR, rFlow)
        result <- ok manager (getActivityLCIA "main" pid coll methodText Nothing)
        (lrCutoffInputs result, lrWithheldCutoffs result) `shouldBe` ([], [WithheldCutoffs "dep" 1])

    it "counts, without naming them, the root's own when its licence keeps its amounts" $ do
        db <- build mainSdb
        manager <- managerOf [("main", own [ReadInventory], db)]
        pid <- pidOf db (actR, rFlow)
        panel <- ok manager (getActivityLCIABatch "main" pid coll Nothing)
        (lbrCutoffInputs panel, lbrWithheldCutoffs panel) `shouldBe` ([], [WithheldCutoffs "main" 1])
        map (\r -> (lrCutoffInputs r, lrWithheldCutoffs r)) (lbrResults panel) `shouldBe` [([], [WithheldCutoffs "main" 1])]

    it "says in the MCP notice how many a dependency keeping its detail holds, naming none" $ do
        manager <- withheldDependency
        rootDb <- rootOf manager
        pid <- pidOf rootDb (actR, rFlow)
        payload <- mcp manager "get_impacts" [("process_id", String pid)]
        KM.lookup "cutoff_notice" payload
            `shouldBe` Just (String "This result counts 1 input no loaded database supplies as zero: 1 inside dep, whose licence keeps the detail; load the database that makes them, or see the gap report.")
        KM.lookup "cutoff_inputs" payload `shouldBe` Just (Array V.empty)
        KM.lookup "withheld_cutoffs" payload `shouldBe` Just (Array (V.singleton (object ["database" .= ("dep" :: Text), "count" .= (1 :: Int)])))

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
            let perRow = do
                    Object byPid <- KM.lookup "cutoff_inputs" payload
                    pure (KM.toList byPid)
            fmap (map (fmap inputsOf)) perRow `shouldBe` Just [(fromText pidR, metByRInputs)]

        it "give score_activity its list once, not once per method" $ do
            payload <- mcp manager "score_activity" [("process_id", String pidR)]
            KM.member "cutoff_notice" payload `shouldBe` True
            (inputsOf =<< KM.lookup "cutoffInputs" payload) `shouldBe` metByRInputs
            let perMethod = do
                    Array results <- KM.lookup "results" payload
                    pure [KM.member "cutoffInputs" o | Object o <- toList results]
            perMethod `shouldBe` Just [False]

        it "give each side of compare_impacts its own list" $ do
            reply <-
                callTool manager [] Nothing Nothing noRequestId "compare_impacts" $
                    KM.fromList
                        [ ("database_a", String "main")
                        , ("process_id_a", String pidR)
                        , ("method_id_a", String methodText)
                        , ("collection_a", String collection)
                        , ("database_b", String "main")
                        , ("process_id_b", String pidZ)
                        , ("method_id_b", String methodText)
                        , ("collection_b", String collection)
                        ]
            compared <- maybe (fail ("unexpected compare_impacts reply: " <> show reply)) pure (payloadOf reply)
            let side key = do
                    Object o <- KM.lookup key compared
                    pure o
            (inputsOf =<< KM.lookup "cutoff_inputs" =<< side "a") `shouldBe` metByRInputs
            (KM.member "cutoff_notice" <$> side "b") `shouldBe` Just False
  where
    cellAt :: Int -> Value -> Maybe Value
    cellAt i row = do
        Array cs <- Just row
        cs V.!? i

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | A database read at other values of its parameters.

The fixture is a bakery whose bread takes flour by a formula of the
database's @yield@ and electricity by two lines, France and Germany, whose
formulas read the bread's own @mix_fr@ and @mix_de@. The electricity comes
from a second database, where the German kilowatt-hour emits ten times the
French one.
-}
module ParametricSpec (spec) where

import API.Routes (activityLCIABatchH, batchImpactsH)
import API.Types (BatchImpactsEntry (..), BatchImpactsRequest (..), BatchImpactsResponse (..), LCIABatchResult (..), LCIAResult (..), ParameterValue (..), SubstitutionRequest (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (defaultConfig)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (evaluate)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold, emptyAliasMap)
import Database.Cutoffs (gapIndexOf)
import Database.Loader (findAllCrossDBLinks)
import Database.Manager (CachePolicy (..), CollectionName (..), DatabaseManager (..), getGapIndex, initDatabaseManager, withSettings)
import Database.MatrixBuild (technosphereChange, withActivities)
import Database.Rebuild (rebuildFromActivities)
import Method.Mapping (LongTermMode (..))
import Method.Types (Method (..), MethodCF (..), MethodCollection (..))
import qualified Method.Types as MT
import Parametric (ParameterRefusal (..), Setting (..), instantiate, parameterised)
import Servant (ServerError (..), runHandler)
import SharedSolver (CrossDBSolution (..), SharedSolver (solverDbName), computeInventoryMatrixWithDepsCached, computeScalingVectorCached)
import SynonymDB (emptySynonymDB)
import System.Mem.StableName (makeStableName)
import Test.Hspec
import TestHelpers (install, mkDepLookupFromMap, mkSolverFromDb, withinTolerance)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "a database at other values of its parameters" $ do
    it "is the database itself when nothing is set, or nothing set moves an amount" $ do
        (bakery, _) <- databases
        M.null <$> instantiate bakery [] `shouldBe` Right True
        M.keys <$> instantiate bakery [Setting Nothing "yield" 1] `shouldBe` Right []

    it "builds the columns a rebuild from the same processes builds" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        let settings = [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1]
        changed <- either (fail . show) pure (instantiate bakery settings)
        M.keys changed `shouldMatchList` [bread, flour bakery]
        fast <- either (fail . T.unpack) pure (withActivities defaultUnitConfig bakery changed)
        let byKey = M.fromList (zip (V.toList (dbProcessIdTable bakery)) (V.toList (dbActivities fast)))
        slow <- either (fail . T.unpack) pure (rebuildFromActivities defaultUnitConfig bakery byKey)
        triples (dbTechnosphereTriples fast) `shouldBe` triples (dbTechnosphereTriples slow)
        triples (dbBiosphereTriples fast) `shouldBe` triples (dbBiosphereTriples slow)

    it "keeps the technosphere as it was when only emissions move" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        changed <- either (fail . show) pure (instantiate bakery [Setting (Just bread) "smoke" 2])
        fast <- either (fail . T.unpack) pure (withActivities defaultUnitConfig bakery changed)
        dbTechnosphereTriples fast `shouldBe` dbTechnosphereTriples bakery
        dbBiosphereTriples fast `shouldNotBe` dbBiosphereTriples bakery

    it "names, on each link to the other database, the line it supplies" $ do
        (bakery, _) <- databases
        sort [(cdlLocation link, aePosition <$> cdlAnswers link) | link <- dbCrossDBLinks bakery]
            `shouldBe` [("DE", Just 3), ("FR", Just 2)]

    it "scores the supplier the settings move the electricity to, in the other database" $ do
        (bakery, grid) <- databases
        bread <- pidOf bakery (aBread, pBread)
        co2Of bakery grid "bakery" `shouldReturn'` 2.5
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised defaultUnitConfig bakery [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1])
        -- Flour 4 kg at 0.125, the bread's own 1, and a German kilowatt-hour.
        co2Of atSettings grid "bakery at settings" `shouldReturn'` 11.5

    it "solves values that move inputs on the loaded factorisation, as a new one would" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        manager <- initDatabaseManager defaultConfig NoCache
        let settings = [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1]
        loaded <- mkSolverFromDb bakery "bakery-loaded"
        (solvedBy, corrected) <- either (fail . T.unpack) pure =<< withSettings manager "bakery-loaded" (bakery, loaded) settings (\db solver -> (solverDbName solver,) <$> computeScalingVectorCached db solver bread)
        solvedBy `shouldBe` "bakery-loaded"
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised defaultUnitConfig bakery settings)
        fresh <- mkSolverFromDb atSettings "bakery-fresh"
        expected <- either (fail . T.unpack) pure =<< computeScalingVectorCached atSettings fresh bread
        moved <- either (fail . show) pure (instantiate bakery settings)
        technosphereChange (M.keysSet moved) bakery atSettings `shouldSatisfy` (not . null)
        (U.toList . U.zipWith (withinTolerance 1e-9) expected <$> corrected) `shouldSatisfy` either (const False) and

    it "factorises again for values that move more inputs than a correction holds" $ do
        -- Each of 130 processes buys the one before it by the same formula.
        chain <- build "chain" chainFlows chainActivities (M.singleton "yield" (input 1))
        top <- pidOf chain (link 129)
        manager <- initDatabaseManager defaultConfig NoCache
        let settings = [Setting Nothing "yield" 1.01]
        loaded <- mkSolverFromDb chain "chain-loaded"
        (solvedBy, got) <- either (fail . T.unpack) pure =<< withSettings manager "chain-loaded" (chain, loaded) settings (\db solver -> (solverDbName solver,) <$> computeScalingVectorCached db solver top)
        solvedBy `shouldNotBe` "chain-loaded"
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised defaultUnitConfig chain settings)
        fresh <- mkSolverFromDb atSettings "chain-fresh"
        expected <- either (fail . T.unpack) pure =<< computeScalingVectorCached atSettings fresh top
        (U.toList . U.zipWith (withinTolerance 1e-9) expected <$> got) `shouldSatisfy` either (const False) and

    it "lets a process's own value of a name win over its database's, in the database's formulas too" $ do
        (bakery, _) <- databases
        cake <- pidOf bakery (aCake, pCake)
        M.keys <$> instantiate bakery [Setting Nothing "yield" 5] `shouldBe` Right (sort [bread' bakery, flour bakery])
        Right changed <- pure (instantiate bakery [Setting (Just cake) "yield" 1])
        map exchangeAmount . exchanges <$> M.lookup cake changed `shouldBe` Just [1, 4]

    it "sets a process's value on every product of its block" $ do
        (bakery, _) <- databases
        meal <- pidOf bakery (aMill, pMeal)
        bran <- pidOf bakery (aMill, pBran)
        M.keys <$> instantiate bakery [Setting (Just bran) "grind" 2] `shouldBe` Right (sort [meal, bran])

    it "refuses what it cannot compute, naming it" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        refusal (instantiate bakery [Setting Nothing "nope" 1]) `shouldBe` Just (UnknownParameter Nothing "nope")
        refusal (instantiate bakery [Setting Nothing "ratio" 1]) `shouldBe` Just (FormulaDefined Nothing "ratio")
        refusal (instantiate bakery [Setting (Just bread) "yield" 1]) `shouldBe` Just (DatabaseWide "bread" "yield")
        refusal (instantiate bakery [Setting Nothing "yield" 1, Setting Nothing "YIELD" 2]) `shouldBe` Just (SetTwice Nothing "YIELD")
        case instantiate bakery [Setting Nothing "yield" 0] of
            Left (NoAmount "flour" "carbon dioxide" _) -> pure ()
            other -> expectationFailure ("expected the flour's emission refused, got " <> show (refusal other))
        refusal (instantiate bakery [Setting Nothing "share_pct" 50]) `shouldBe` Just (ShareMoves "jam" "share_pct")

    it "reads the gap index at other values from the loaded database's, and caches only that one" $ do
        -- Without its links to the grid, the bakery's electricity is a cut-off input.
        bakery <- evaluate . (\(linked, _) -> linked{dbCrossDBLinks = []}) =<< databases
        manager <- initDatabaseManager defaultConfig NoCache
        install manager "bakery" bakery
        -- The German electricity, asked for nothing by default, becomes one; the French one stops being one.
        bread <- Just <$> pidOf bakery (aBread, pBread)
        atMix <- either (fail . T.unpack) (pure . snd) (parameterised defaultUnitConfig bakery [Setting bread "mix_fr" 0, Setting bread "mix_de" 1])
        gapIndexOf atMix `shouldNotBe` gapIndexOf bakery
        loaded <- makeStableName =<< evaluate bakery
        let cachedFromLoaded = fmap ((== loaded) . fst) . M.lookup "bakery" <$> readTVarIO (dmGapIndexCache manager)
        getGapIndex manager "bakery" atMix `shouldReturn` gapIndexOf atMix
        cachedFromLoaded `shouldReturn` Just True
        getGapIndex manager "bakery" bakery `shouldReturn` gapIndexOf bakery
        getGapIndex manager "bakery" atMix `shouldReturn` gapIndexOf atMix
        cachedFromLoaded `shouldReturn` Just True

    it "scores each process of a batch at each set, as the request at that set alone would" $ do
        manager <- scoringManager
        let processes = [processIdText (aBread, pBread), processIdText (aFlour, pFlour)]
            sets =
                [ [ParameterValue Nothing "yield" 2]
                , [ParameterValue (Just (processIdText (aBread, pBread))) "mix_fr" 0, ParameterValue (Just (processIdText (aBread, pBread))) "mix_de" 1]
                , []
                ]
        batch <- answer manager (batchImpactsH "bakery" (CollectionName "co2") Nothing IncludeLongTerm (BatchImpactsRequest processes sets))
        alone <-
            sequence
                [ answer manager (activityLCIABatchH "bakery" process (CollectionName "co2") (Just (SubstitutionRequest [] values)) IncludeLongTerm)
                | values <- sets
                , process <- processes
                ]
        [(bieParameterSet e, bieProcessId e) | e <- birResults batch] `shouldBe` [(Just i, p) | i <- [0, 1, 2], p <- processes]
        map (scoresOf . bieImpacts) (birResults batch) `shouldBe` map scoresOf alone
        -- The bread's score moves with each set, so the rows are not all one number.
        length (nub (map (scoresOf . bieImpacts) (birResults batch))) `shouldSatisfy` (> 2)
        atDefaults <- answer manager (batchImpactsH "bakery" (CollectionName "co2") Nothing IncludeLongTerm (BatchImpactsRequest processes []))
        [(bieParameterSet e, scoresOf (bieImpacts e)) | e <- birResults atDefaults] `shouldBe` [(Nothing, scoresOf (bieImpacts e)) | e <- drop 4 (birResults batch)]

    it "refuses a batch one of whose sets it cannot compute, naming the set" $ do
        manager <- scoringManager
        refused <- runHandler . runApp (envOf manager) $ batchImpactsH "bakery" (CollectionName "co2") Nothing IncludeLongTerm (BatchImpactsRequest [processIdText (aBread, pBread)] [[ParameterValue Nothing "yield" 2], [ParameterValue Nothing "nope" 1]])
        either (\e -> (errHTTPCode e, take 16 (BSL.unpack (errBody e)))) (const (200, "")) refused `shouldBe` (400, "parameter set 1:")

    it "reads a request that only sets parameters, and refuses a misspelt field of one" $ do
        let request = decode "{\"parameters\": [{\"process\": \"p\", \"name\": \"yield\", \"value\": 2}]}" :: Maybe SubstitutionRequest
        fmap (null . srSubstitutions) request `shouldBe` Just True
        fmap (map (\v -> (pvProcess v, pvName v, pvValue v)) . srParameters) request `shouldBe` Just [(Just "p", "yield", 2)]
        isNothing (decode "{\"parameters\": [{\"proces\": \"p\", \"name\": \"yield\", \"value\": 2}]}" :: Maybe SubstitutionRequest) `shouldBe` True
  where
    shouldReturn' :: IO Double -> Double -> Expectation
    shouldReturn' action expected = action >>= \got -> (got, withinTolerance 1e-9 expected got) `shouldBe` (got, True)

    flour db = fromMaybe (-1) (findProcessId db aFlour pFlour)
    bread' db = fromMaybe (-1) (findProcessId db aBread pBread)

refusal :: Either ParameterRefusal a -> Maybe ParameterRefusal
refusal = either Just (const Nothing)

-- | A manager holding the bakery and its grid, and a collection scoring their carbon dioxide.
scoringManager :: IO DatabaseManager
scoringManager = do
    (bakery, grid) <- databases
    manager <- initDatabaseManager defaultConfig NoCache
    install manager "grid" grid
    install manager "bakery" bakery
    atomically $ modifyTVar' (dmLoadedMethods manager) (M.insert "co2" (MethodCollection [climate] [] []))
    pure manager
  where
    climate :: Method
    climate =
        Method
            { methodId = UUID.fromWords 7 0 0 1
            , methodName = "Climate change"
            , methodDescription = Nothing
            , methodUnit = "kg CO2 eq"
            , methodCategory = "Climate change"
            , methodMethodology = Nothing
            , methodFactors = [MethodCF co2 "carbon dioxide" MT.Output 1 (Just (MT.Compartment "air" "unspecified" "")) Nothing "kg" Nothing]
            }

envOf :: DatabaseManager -> AppEnv
envOf manager =
    AppEnv
        { aeDbManager = manager
        , aeMaxTreeDepth = 5
        , aePassword = Nothing
        , aeHostingConfig = Nothing
        , aeClassificationPresets = []
        , aeDataVersion = Nothing
        , aeUsageLog = Nothing
        , aeReader = Nothing
        }

-- | Run a handler, failing the test on an error status.
answer :: DatabaseManager -> AppM a -> IO a
answer manager handler = runHandler (runApp (envOf manager) handler) >>= either (\e -> fail (show (errHTTPCode e) <> ": " <> BSL.unpack (errBody e))) pure

scoresOf :: LCIABatchResult -> [Double]
scoresOf = map lrScore . lbrResults

processIdText :: (UUID, UUID) -> Text
processIdText (act, prod) = UUID.toText act <> "_" <> UUID.toText prod

triples :: U.Vector SparseTriple -> [(Int, Int, Double)]
triples = sort . map (\(SparseTriple i j v) -> (fromIntegral i, fromIntegral j, v)) . U.toList

pidOf :: Database -> (UUID, UUID) -> IO ProcessId
pidOf db (act, prod) = maybe (fail "process not interned") pure (findProcessId db act prod)

-- | The bread's carbon dioxide, its electricity supplied by the grid.
co2Of :: Database -> Database -> Text -> IO Double
co2Of bakery grid name = do
    solver <- mkSolverFromDb bakery name
    gridSolver <- mkSolverFromDb grid "grid"
    bread <- pidOf bakery (aBread, pBread)
    result <- computeInventoryMatrixWithDepsCached defaultUnitConfig (mkDepLookupFromMap (M.singleton "grid" (grid, gridSolver))) bakery name solver bread
    either (fail . T.unpack) (pure . M.findWithDefault 0 co2 . csInventory) result

-- | The bakery, linked to the grid the way a load links it, and the grid.
databases :: IO (Database, Database)
databases = do
    grid <- build "grid" techFlows (M.fromList [((aFR, pElectricity), electricity "FR" 1), ((aDE, pElectricity), electricity "DE" 10)]) M.empty
    bakery <- build "bakery" techFlows bakeryActivities bakeryParameters
    let context =
            LinkingContext
                { lcIndexedDatabases = [buildIndexedDatabaseFromDB "grid" emptySynonymDB grid]
                , lcSynonymDB = emptySynonymDB
                , lcUnitConfig = defaultUnitConfig
                , lcThreshold = defaultLinkingThreshold
                , lcLocationHierarchy = M.empty
                , lcGeographyPolicy = GeoExact
                , lcSupplierAliases = emptyAliasMap
                }
        links = cdlLinks (findAllCrossDBLinks context techFlows M.empty units bakeryActivities)
    pure (bakery{dbCrossDBLinks = links}, grid)

build :: Text -> M.Map UUID TechnosphereFlow -> M.Map (UUID, UUID) Activity -> M.Map Text Parameter -> IO Database
build name flows acts parameters =
    either (\e -> fail (T.unpack name <> ": " <> T.unpack e)) pure
        =<< buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig M.empty Declared [])
            SimpleDatabase
                { sdbActivities = acts
                , sdbTechFlows = flows
                , sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "carbon dioxide" kg M.empty Nothing Nothing (Just (Compartment Air Nothing)))
                , sdbWasteFlows = M.empty
                , sdbUnits = units
                , sdbDocumentation = noDocumentation
                , sdbParameters = parameters
                }

-- | The activity and product of the chain's link @i@.
link :: Int -> (UUID, UUID)
link i = (UUID.fromWords 9 (fromIntegral i) 0 1, UUID.fromWords 9 (fromIntegral i) 0 2)

chainFlows :: M.Map UUID TechnosphereFlow
chainFlows = M.fromList [(product', TechnosphereFlow product' "link" kg M.empty Nothing Nothing) | i <- [0 .. 129], let product' = snd (link i)]

chainActivities :: M.Map (UUID, UUID) Activity
chainActivities =
    M.fromList
        [ (link i, activity "link" (reference (snd (link i)) : [withFormula (formulaOf "yield") (line (snd (link (i - 1))) 1 Input){techActivityLinkId = Just (fst (link (i - 1)))} | i > 0]))
        | i <- [0 .. 129]
        ]

bakeryParameters :: M.Map Text Parameter
bakeryParameters =
    M.fromList
        [ ("yield", input 1)
        , ("ratio", parameter (CalculatedParameter "yield * 4" (Just 4)))
        , ("share_pct", input 100)
        ]

bakeryActivities :: M.Map (UUID, UUID) Activity
bakeryActivities =
    M.fromList
        [ ((aBread, pBread), (activity "bread" [reference pBread, flourLine, power "FR" "mix_fr" 1, power "DE" "mix_de" 0, emission "smoke" 1]){activityParameters = M.fromList [("mix_fr", input 1), ("mix_de", input 0), ("smoke", input 1)]})
        , ((aFlour, pFlour), activity "flour" [reference pFlour, emission "0.25 / yield" 0.25])
        , ((aCake, pCake), (activity "cake" [reference pCake, emission "ratio" 12]){activityParameters = M.singleton "yield" (input 3)})
        , ((aMill, pMeal), milled pMeal)
        , ((aMill, pBran), milled pBran)
        , ((aJam, pJam), activity "jam" [(reference pJam){techShare = Just DeclaredShare{dsPercent = 100, dsFormula = Just "share_pct"}}])
        ]
  where
    flourLine = withFormula (formulaOf "2 * yield") (line pFlour 2 Input){techActivityLinkId = Just aFlour}
    power location formula amount = withFormula (formulaOf formula) (line pElectricity amount Input){techLocation = location}
    milled product' = (activity "mill" [reference product', emission "grind" 1]){activityParameters = M.singleton "grind" (input 1)}

electricity :: Text -> Double -> Activity
electricity location amount = (activity "electricity" [reference pElectricity, (emission "" amount){bioFormula = Nothing}]){activityLocation = location}

activity :: Text -> [Exchange] -> Activity
activity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

reference :: UUID -> Exchange
reference product' = line product' 1 ReferenceProduct

line :: UUID -> Double -> TechRole -> Exchange
line product' amount role =
    TechnosphereExchange
        { techFlowId = product'
        , techAmount = amount
        , techUnitId = kg
        , techRole = role
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

emission :: Text -> Double -> Exchange
emission formula amount =
    BiosphereExchange
        { bioFlowId = co2
        , bioAmount = amount
        , bioUnitId = kg
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        , bioFormula = formulaOf formula
        }

formulaOf :: Text -> Maybe ExchangeFormula
formulaOf formula = Just ExchangeFormula{efName = Nothing, efText = formula}

input :: Double -> Parameter
input value = parameter (InputParameter value Nothing)

parameter :: ParameterDefinition -> Parameter
parameter definition = Parameter{parameterDefinition = definition, parameterUnit = Nothing, parameterDescription = ""}

techFlows :: M.Map UUID TechnosphereFlow
techFlows =
    M.fromList
        [ (product', TechnosphereFlow product' name kg M.empty Nothing Nothing)
        | (product', name) <- [(pBread, "bread"), (pFlour, "flour"), (pCake, "cake"), (pMeal, "meal"), (pBran, "bran"), (pJam, "jam"), (pElectricity, "electricity")]
        ]

units :: M.Map UUID Unit
units = M.singleton kg (Unit kg "kg" "kg" "")

kg, co2, aBread, pBread, aFlour, pFlour, aCake, pCake, aMill, pMeal, pBran, aJam, pJam, aFR, aDE, pElectricity :: UUID
kg = UUID.fromWords 0 0 0 1
co2 = UUID.fromWords 0 0 0 2
aBread = UUID.fromWords 1 0 0 1
pBread = UUID.fromWords 1 0 0 2
aFlour = UUID.fromWords 2 0 0 1
pFlour = UUID.fromWords 2 0 0 2
aCake = UUID.fromWords 3 0 0 1
pCake = UUID.fromWords 3 0 0 2
aMill = UUID.fromWords 4 0 0 1
pMeal = UUID.fromWords 4 0 0 2
pBran = UUID.fromWords 4 0 0 3
aJam = UUID.fromWords 5 0 0 1
pJam = UUID.fromWords 5 0 0 2
aFR = UUID.fromWords 6 0 0 1
aDE = UUID.fromWords 6 0 0 2
pElectricity = UUID.fromWords 6 0 0 3

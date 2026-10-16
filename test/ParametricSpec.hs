{-# LANGUAGE OverloadedStrings #-}

{- | A database read at other values of its parameters.

The fixture is a bakery whose bread takes flour by a formula of the
database's @yield@ and electricity by two lines, France and Germany, whose
formulas read the bread's own @mix_fr@ and @mix_de@. The electricity comes
from a second database, where the German kilowatt-hour emits ten times the
French one.
-}
module ParametricSpec (spec) where

import API.Types (ParameterValue (..), SubstitutionRequest (..))
import Data.Aeson (decode)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold, emptyAliasMap)
import Database.Loader (findAllCrossDBLinks)
import Database.MatrixBuild (withActivities)
import Database.Rebuild (rebuildFromActivities)
import Parametric (ParameterRefusal (..), Setting (..), instantiate, parameterised)
import SharedSolver (CrossDBSolution (..), computeInventoryMatrixWithDepsCached)
import SynonymDB (emptySynonymDB)
import Test.Hspec
import TestHelpers (mkDepLookupFromMap, mkSolverFromDb, withinTolerance)
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
        atSettings <- either (fail . T.unpack) pure (parameterised defaultUnitConfig bakery [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1])
        -- Flour 4 kg at 0.125, the bread's own 1, and a German kilowatt-hour.
        co2Of atSettings grid "bakery at settings" `shouldReturn'` 11.5

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
    grid <- build "grid" (M.fromList [((aFR, pElectricity), electricity "FR" 1), ((aDE, pElectricity), electricity "DE" 10)]) M.empty
    bakery <- build "bakery" bakeryActivities bakeryParameters
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
  where
    build :: Text -> M.Map (UUID, UUID) Activity -> M.Map Text Parameter -> IO Database
    build name acts parameters =
        either (\e -> fail (T.unpack name <> ": " <> T.unpack e)) pure
            =<< buildDatabaseWithMatrices
                (BuildInputs defaultUnitConfig M.empty Declared [])
                SimpleDatabase
                    { sdbActivities = acts
                    , sdbTechFlows = techFlows
                    , sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "carbon dioxide" kg M.empty Nothing Nothing (Just (Compartment Air Nothing)))
                    , sdbWasteFlows = M.empty
                    , sdbUnits = units
                    , sdbDocumentation = noDocumentation
                    , sdbParameters = parameters
                    }

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

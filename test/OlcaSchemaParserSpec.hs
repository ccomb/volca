{-# LANGUAGE OverloadedStrings #-}

-- | An openLCA package read into a database, checked on the inventories it computes.
module OlcaSchemaParserSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database (buildDatabaseWithMatrices)
import Matrix (computeInventoryMatrix)
import OlcaPackageFixture
import OlcaSchema.Package
import qualified OlcaSchema.Package as P
import OlcaSchema.Parser
import Types
import UnitConversion (defaultUnitConfig)

readFixture :: IO Package
readFixture = withSystemTempDirectory "olca-package" $ \dir -> do
    writePackage dir
    readPackage dir >>= either (fail . T.unpack) pure

built :: Package -> IO Built
built = either (fail . T.unpack) pure . buildDatabase defaultUnitConfig Declared

matrices :: Built -> IO Database
matrices b = buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared []) (builtDatabase b) >>= either (fail . T.unpack) pure

loaded :: IO (Built, Database)
loaded = do
    b <- built =<< readFixture
    db <- matrices b
    pure (b, db)

inventoryOf :: Database -> UUID -> UUID -> IO (M.Map UUID Double)
inventoryOf db process product = case M.lookup (process, product) (dbProcessIdLookup db) of
    Nothing -> fail ("no process " <> show (process, product))
    Just pid -> computeInventoryMatrix db pid >>= either (fail . T.unpack) pure

-- | The non-zero flows of an inventory are these, each within rounding of its amount.
carriesOnly :: M.Map UUID Double -> [(UUID, Double)] -> Expectation
carriesOnly inventory expected = do
    sort (M.keys (M.filter (/= 0) inventory)) `shouldBe` sort (map fst expected)
    forM_ expected $ \(flow, amount) ->
        (UUID.toString flow, M.findWithDefault 0 flow inventory)
            `shouldSatisfy` (\(_, got) -> abs (got - amount) <= 1e-9 * max 1 (abs amount))

-- | An exchange of one process as read, by its flow.
exchangeOf :: Built -> UUID -> UUID -> UUID -> Maybe Exchange
exchangeOf b process product flow = case M.lookup (process, product) (sdbActivities (builtDatabase b)) of
    Nothing -> Nothing
    Just act -> case filter ((== flow) . exchangeFlowId) (exchanges act) of
        [ex] -> Just ex
        _ -> Nothing

-- | The package with one process changed.
editing :: UUID -> (Process -> Process) -> Package -> Package
editing process change pkg = pkg{pkProcesses = map (\p -> if prId p == process then change p else p) (pkProcesses pkg)}

spec :: Spec
spec = beforeAll loaded $ describe "an openLCA package read into a database" $ do
    it "converts units and other flow properties, and links a default provider" $ \(_, db) -> do
        inv <- inventoryOf db steelA steelF
        inv `carriesOnly` [(co2F, 0.46305), (oreF, 0.018), (zincF, 5.0e-5)]

    it "computes a waste treatment per unit treated, as openLCA does" $ \(_, db) -> do
        inv <- inventoryOf db slagD slagF
        inv `carriesOnly` [(co2F, 0.025), (zincF, 0.001)]

    it "prefers an aggregated producer when an input names none" $ \(b, _) ->
        (exchangeOf b slagD slagF electricityF >>= exchangeActivityLinkId) `shouldBe` Just elecB2

    it "splits a process on its physical factors" $ \(_, db) -> do
        heat <- inventoryOf db cogenF heatF
        power <- inventoryOf db cogenF powerF
        heat `carriesOnly` [(co2F, 0.303), (oreF, 0.03)]
        power `carriesOnly` [(co2F, 0.404), (oreF, 0.04)]

    it "splits a process on its causal factors, an exchange with none kept whole" $ \(_, db) -> do
        heat <- inventoryOf db cogenG heatF
        power <- inventoryOf db cogenG powerF
        heat `carriesOnly` [(co2F, 0.4525), (oreF, 0.025)]
        power `carriesOnly` [(co2F, 0.11), (oreF, 0.1)]

    it "gives every product the whole inventory when the process names no method, and says so" $ \(b, db) -> do
        heat <- inventoryOf db boilerH heatF
        ash <- inventoryOf db boilerH ashF
        heat `carriesOnly` [(co2F, 0.052), (oreF, 0.02)]
        ash `carriesOnly` [(co2F, 2.6), (oreF, 1.0)]
        [n | n@(WithoutFactor _) <- builtNotices b] `shouldBe` [WithoutFactor "boiler"]

    it "credits an avoided product to its provider" $ \(_, db) -> do
        inv <- inventoryOf db recyclingI recycledF
        inv `carriesOnly` [(co2F, -0.11944), (oreF, -0.0044), (zincF, -4.0e-5)]

    it "computes amounts from formulas and parameters, keeping the stored amount where one cannot be read" $ \(_, db) -> do
        inv <- inventoryOf db formulasJ widgetF
        inv `carriesOnly` [(co2F, 0.85), (oreF, 0.06), (zincF, 0.001)]

    it "says which formulas disagree with the file and which could not be read" $ \(b, _) -> do
        [n | n@(Divergent _) <- builtNotices b] `shouldSatisfy` ((== 1) . length)
        [what | Unevaluable what <- builtNotices b] `shouldSatisfy` any (T.isInfixOf "random")
        fmap fcDivergent (activityFormulaCheck =<< M.lookup (formulasJ, widgetF) (sdbActivities (builtDatabase b))) `shouldBe` Just 1

    it "leaves an input no process produces cut off, and counts it" $ \(b, _) -> do
        (exchangeOf b formulasJ widgetF landF >>= exchangeActivityLinkId) `shouldBe` Nothing
        [what | CutOff what <- builtNotices b] `shouldBe` ["widget assembly · land occupation"]

    it "breaks a tie between producers by identifier, and reports it" $ \(b, db) -> do
        inv <- inventoryOf db tieL thingF
        inv `carriesOnly` [(co2F, 1)]
        [what | Tied what <- builtNotices b] `shouldBe` ["thing making · tie"]

    it "places elementary flows in their compartment, and counts those it cannot place" $ \(b, _) -> do
        let compartment fid = bfCompartment =<< M.lookup fid (sdbBioFlows (builtDatabase b))
        compartment co2F `shouldBe` Just (Compartment Air Nothing)
        compartment zincF `shouldBe` Just (Compartment Soil Nothing)
        compartment oreF `shouldBe` Just (Compartment NaturalResource (Just "ground"))
        compartment serviceF `shouldBe` Nothing
        [what | Unplaced what <- builtNotices b] `shouldBe` ["pollination"]

    it "refuses a line it cannot convert, naming every one" $ \_ -> do
        pkg <- readFixture
        -- CO2 is a mass flow; kWh is in the energy group.
        let wrong = editing steelA (\p -> p{prExchanges = map (\x -> if rxFlow x == co2F then x{rxUnit = kwhU} else x) (prExchanges p)}) pkg
        case buildDatabase defaultUnitConfig Declared wrong of
            Left why -> T.unpack why `shouldContain` "steel production · carbon dioxide"
            Right _ -> expectationFailure "loaded a line in a unit its flow has no conversion from"

    it "lets a process parameter shadow a global one, case aside" $ \_ -> do
        pkg <- readFixture
        -- LEAK_METHOD = 2 sends b to its missing branch, which only the process's value can do.
        let shadowing = editing formulasJ (\p -> p{prParameters = Parameter "LEAK_METHOD" (InputValue 2) : prParameters p}) pkg
        b <- built shadowing
        [what | Unsettled what <- builtNotices b] `shouldBe` ["widget assembly · b"]

    it "says which allocation factor formulas could not be read, keeping the stored factor" $ \_ -> do
        pkg <- readFixture
        let unreadable = editing cogenF (\p -> p{prFactors = map (\f -> if P.afMethod f == Physical && P.afProduct f == heatF then f{afFormula = Just "unknown_name * 2"} else f) (prFactors p)}) pkg
        b <- built unreadable
        [what | Unevaluable what <- builtNotices b, "cogeneration, physical" `T.isPrefixOf` what]
            `shouldSatisfy` (\whats -> length whats == 1 && all (T.isInfixOf "heat · allocation factor unknown_name * 2") whats)
        db <- matrices b
        heat <- inventoryOf db cogenF heatF
        heat `carriesOnly` [(co2F, 0.303), (oreF, 0.03)]

    it "keeps a default provider the package does not hold" $ \_ -> do
        pkg <- readFixture
        let elsewhere = uid' 999
            moved = editing recyclingI (\p -> p{prExchanges = map (\x -> if rxSide x == Avoided then x{rxProvider = Just elsewhere} else x) (prExchanges p)}) pkg
        b <- built moved
        fmap exchangeSupplierClaim (exchangeOf b recyclingI recycledF steelF) `shouldBe` Just (ClaimById elsewhere)
        db <- matrices b
        inv <- inventoryOf db recyclingI recycledF
        -- The credit is gone: electricity's burden and the direct CO2 remain.
        inv `carriesOnly` [(co2F, 0.251), (oreF, 0.01)]
  where
    uid' :: Int -> UUID
    uid' n = UUID.fromWords 0 0 0 (fromIntegral n)

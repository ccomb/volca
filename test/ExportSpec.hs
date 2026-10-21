{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the export dispatcher ('Database.Export.serializeDatabase').

The dispatcher is the single entry point the CLI and HTTP handler use to turn an
in-memory 'Database' into bytes. It must: wire every writable format, fail loudly
(never emit empty bytes) for the formats with no writer, and propagate a writer's
own export-guard 'Left' rather than swallow it.
-}
module ExportSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (fromRight, isLeft, isRight)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Test.Hspec

import qualified Database as DB
import Database.Export (MethodExportFormat (..), namedAfterLinks, serializeDatabase, serializeMethodCollection)
import Database.ExportSupplier (Dependencies (..), Elsewhere (..), noDependencies)
import Database.Upload (DatabaseFormat (..))
import qualified Method.Types as MT
import Types
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

spec :: Spec
spec = describe "Database.Export dispatcher" $ do
    it "serializes a simple database to every writable format" $ do
        db <- buildFixture (Compartment Air (Just "unspecified"))
        forM_ [SimaProCSV, EcoSpold1, EcoSpold2, ILCDProcess, BrightwayExcel, OpenLcaPackage] $ \fmt ->
            case serializeDatabase defaultUnitConfig fmt M.empty (Right noDependencies) db of
                Left err -> expectationFailure (show fmt <> ": " <> T.unpack err)
                Right (bytes, _warnings) -> BL.null bytes `shouldBe` False

    -- Suppliers that could not be worked out stop only the formats naming them.
    it "exports the formats pointing at a supplier when the suppliers could not be named" $ do
        db <- buildFixture (Compartment Air (Just "unspecified"))
        let unnamed fmt = serializeDatabase defaultUnitConfig fmt M.empty (Left "its supplier is not loaded") db
        forM_ [EcoSpold2, OpenLcaPackage] $ \fmt -> unnamed fmt `shouldSatisfy` isRight
        forM_ [SimaProCSV, EcoSpold1, ILCDProcess, BrightwayExcel] $ \fmt -> unnamed fmt `shouldSatisfy` isLeft

    it "fails loudly for formats with no writer (never a silent empty file)" $ do
        db <- buildFixture (Compartment Air (Just "unspecified"))
        serializeDatabase defaultUnitConfig OpenLcaImpactCategory M.empty (Right noDependencies) db `shouldSatisfy` isLeft
        serializeDatabase defaultUnitConfig UnknownFormat M.empty (Right noDependencies) db `shouldSatisfy` isLeft

    it "writes an exclusion row only to the format that reads the marker back" $ do
        -- VoLCA's own columnar CSV parses "!…" as an exception and must keep it,
        -- or a round-trip loses it and the category counts what it excepted.
        -- SimaPro has no such notion: the row would land as a characterized flow
        -- named "!Occupation, sea*": the sea counted at 1, the exception inverted.
        let mc = MT.MethodCollection [landOccupied] [] []
            landOccupied =
                MT.Method
                    { MT.methodId = UUID.nil
                    , MT.methodName = "Land occupied"
                    , MT.methodDescription = Nothing
                    , MT.methodUnit = "m2a"
                    , MT.methodCategory = "Land occupied"
                    , MT.methodMethodology = Nothing
                    , MT.methodFactors = [mkFactor "Occupation, annual crop", mkFactor "!Occupation, sea*"]
                    }
            mkFactor name =
                MT.MethodCF
                    { MT.mcfFlowRef = UUID.nil
                    , MT.mcfFlowName = name
                    , MT.mcfDirection = MT.Input
                    , MT.mcfValue = 1.0
                    , MT.mcfCompartment = Just (MT.Compartment "natural resource" "" "")
                    , MT.mcfCAS = Nothing
                    , MT.mcfUnit = "m2a"
                    , MT.mcfConsumerLocation = Nothing
                    }
            written fmt = case serializeMethodCollection fmt "plain-indicators" mc of
                Left err -> T.unpack err
                Right (bytes, _warnings) -> T.unpack (TE.decodeUtf8 (BL.toStrict bytes))
        written MethodColumnarCSV `shouldSatisfy` isInfixOf "!Occupation, sea*"
        written MethodSimaProCSV `shouldSatisfy` (not . isInfixOf "!Occupation")
        written MethodSimaProCSV `shouldSatisfy` isInfixOf "Occupation, annual crop"

    it "propagates a writer's export-guard failure" $ do
        -- A "raw" emission compartment has no faithful SimaPro section, so the
        -- SimaPro writer's own guard rejects it; the dispatcher must surface that
        -- Left rather than emit a corrupt file.
        db <- buildFixture (Compartment NaturalResource Nothing)
        serializeDatabase defaultUnitConfig SimaProCSV M.empty (Right noDependencies) db `shouldSatisfy` isLeft

    describe "an input linked to another database" $ do
        it "is written under the supplier's product and pointed at it, in the unit it was written in" $ do
            let (sdb, said) = namedAfterLinks energyUnits (suppliers [(1, grid)]) [linkAt 1 gridAct 1 "MJ"] spinning
            (\ex -> (techFlowId ex, techActivityLinkId ex, techAmount ex, techUnitId ex)) (lineAt 1 sdb)
                `shouldBe` (gridProd, Just gridAct, 33.336, mjU)
            fmap (\f -> (tfName f, tfUnitId f)) (M.lookup gridProd (sdbTechFlows sdb)) `shouldBe` Just ("Electricity, medium voltage, at grid", mjU)
            said `shouldBe` []

        it "is written in the link's unit when a relink bridged two measures, and says its formula is dropped" $ do
            let (sdb, said) = namedAfterLinks energyUnits (suppliers [(1, grid)]) [linkAt 1 gridAct 0.5 "kg"] spinning
            (\ex -> (techAmount ex, techUnitId ex, isNothing (techFormula ex))) (lineAt 1 sdb) `shouldBe` (16.668, kgU, True)
            said `shouldSatisfy` any (T.isInfixOf "formula")

        it "takes the supplier of its own link when two lines of one product have two" $ do
            let (sdb, _) = namedAfterLinks energyUnits (suppliers [(1, grid), (2, grid)]) [linkAt 1 gridAct 1 "MJ", linkAt 2 otherAct 1 "MJ"] spinning
            fmap techActivityLinkId (lookupLine 1 sdb) `shouldBe` Just (Just gridAct)
            fmap techActivityLinkId (lookupLine 2 sdb) `shouldBe` Just (Just otherAct)

        it "is written as its source named it, and said, when its suppliers could not be worked out" $ do
            linked <- linkedSpinning
            case serializeDatabase energyUnits EcoSpold2 M.empty (Left "its supplier is not loaded") linked of
                Left err -> expectationFailure (T.unpack err)
                Right (_, said) -> said `shouldSatisfy` any (T.isInfixOf "as their source named them")

        it "exports to the formats that name or point at a supplier, two lines in MJ and kWh on one supplier included" $ do
            linked <- linkedSpinning
            let deps = Right (suppliers [(1, grid), (2, grid)])
            forM_ [SimaProCSV, EcoSpold2, BrightwayExcel, OpenLcaPackage] $ \fmt ->
                case serializeDatabase energyUnits fmt M.empty deps linked of
                    Left err -> expectationFailure (show fmt <> ": " <> T.unpack err)
                    Right _ -> pure ()
            forM_ [EcoSpold1, ILCDProcess] $ \fmt -> serializeDatabase energyUnits fmt M.empty deps linked `shouldSatisfy` isLeft
            let simapro = fmap (BLC.unpack . fst) (serializeDatabase energyUnits SimaProCSV M.empty deps linked)
            fromRight "" simapro `shouldSatisfy` isInfixOf "Electricity, medium voltage, at grid {RER}| Electricity, medium voltage, at grid |"
            fromRight "" simapro `shouldSatisfy` (not . isInfixOf "market group for electricity")

{- | One activity: a reference product and a single biosphere emission whose
compartment is @comp@ (air for the all-formats case, "raw" for the guard case).
-}
buildFixture :: Compartment -> IO Database
buildFixture comp = do
    r <-
        DB.buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = M.singleton (actU, prodU) act
                , sdbTechFlows = M.singleton prodU (TechnosphereFlow prodU "product" unitU M.empty Nothing Nothing)
                , sdbBioFlows = M.singleton co2U (BiosphereFlow co2U "Carbon dioxide" unitU M.empty Nothing Nothing (Just comp))
                , sdbWasteFlows = M.empty
                , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
                , sdbDocumentation = noDocumentation
                , sdbParameters = M.empty
                , sdbProductSystems = []
                , sdbReferenceConvention = SignedReferences
                }
    either (fail . ("buildDatabaseWithMatrices: " <>) . T.unpack) pure r
  where
    actU, prodU, co2U, unitU :: UUID
    actU = read "eeeeeeee-0000-4000-8000-000000000001"
    prodU = read "eeeeeeee-0000-4000-8000-0000000000a1"
    co2U = read "eeeeeeee-0000-4000-8000-0000000000c1"
    unitU = read "22222222-0000-4000-8000-000000000099"
    act =
        Activity
            "maker"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing
            , BiosphereExchange co2U 0.5 unitU Emission "" Nothing Nothing Nothing
            ]
            M.empty
            Nothing
            Nothing
            Nothing
            noDates

-- | A UnitConfig that knows kg and two energy units; the shipped default knows no energy.
energyUnits :: UnitConfig
energyUnits = either (error . T.unpack) id (buildFromCSV (BLC.pack "name,dimension,factor\nkg,mass,1.0\nMJ,energy,1.0\nkWh,energy,3.6\n"))

kgU, mjU, kwhU, spinAct, spinProd, marketFlow, gridAct, gridProd, otherAct :: UUID
kgU = read "33333333-0000-4000-8000-000000000001"
mjU = read "33333333-0000-4000-8000-000000000002"
kwhU = read "33333333-0000-4000-8000-000000000003"
spinAct = read "33333333-0000-4000-8000-0000000000a1"
spinProd = read "33333333-0000-4000-8000-0000000000a2"
marketFlow = read "33333333-0000-4000-8000-0000000000a3"
gridAct = read "33333333-0000-4000-8000-0000000000b1"
gridProd = read "33333333-0000-4000-8000-0000000000b2"
otherAct = read "33333333-0000-4000-8000-0000000000b3"

unitsOf :: M.Map UUID Unit
unitsOf = M.fromList [(u, Unit u n n "") | (u, n) <- [(kgU, "kg"), (mjU, "MJ"), (kwhU, "kWh")]]

techLine :: UUID -> Double -> UUID -> TechRole -> SupplierClaim -> ExchangeLocation -> Maybe ExchangeFormula -> Exchange
techLine flow amount unit role claim loc formula =
    TechnosphereExchange
        { techFlowId = flow
        , techAmount = amount
        , techUnitId = unit
        , techRole = role
        , techActivityLinkId = Nothing
        , techSupplierClaim = claim
        , techLocation = loc
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        , techFormula = formula
        }

activityOf :: Text -> Text -> Text -> [Exchange] -> Activity
activityOf name loc unit lines' = Activity name [] [] M.empty M.empty loc LocationDeclared unit lines' M.empty Nothing Nothing Nothing noDates

databaseOf :: M.Map (UUID, UUID) Activity -> [TechnosphereFlow] -> SimpleDatabase
databaseOf acts flows =
    SimpleDatabase
        { sdbActivities = acts
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- flows]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = unitsOf
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        , sdbProductSystems = []
        , sdbReferenceConvention = SignedReferences
        }

{- | A spinning step buying electricity under the name its source gave it: 33.336
MJ through a formula at position 1, and 1 kWh at position 2.
-}
spinning :: SimpleDatabase
spinning =
    databaseOf
        ( M.singleton
            (spinAct, spinProd)
            ( activityOf
                "spinning"
                "GLO"
                "kg"
                [ techLine spinProd 1 kgU ReferenceProduct ClaimByProduct "GLO" Nothing
                , techLine marketFlow 33.336 mjU Input (ClaimByName "market group for electricity, medium voltage") "GLO" (Just (ExchangeFormula Nothing "33.336"))
                , techLine marketFlow 1 kwhU Input (ClaimByName "market group for electricity, medium voltage") "GLO" Nothing
                ]
            )
        )
        [ TechnosphereFlow spinProd "yarn" kgU M.empty Nothing Nothing
        , TechnosphereFlow marketFlow "market group for electricity, medium voltage" mjU M.empty Nothing Nothing
        ]

grid :: Elsewhere
grid = Elsewhere{elDatabase = "grid", elActivity = "Electricity, medium voltage, at grid", elLocation = "RER"}

-- | The suppliers of the spinning lines at these positions.
suppliers :: [(Int, Elsewhere)] -> Dependencies
suppliers found = Dependencies (M.fromList [((spinAct, spinProd, position), e) | (position, e) <- found])

-- | The spinning step built, its MJ and kWh lines linked to the grid.
linkedSpinning :: IO Database
linkedSpinning = do
    consumer <- built spinning
    pure consumer{dbCrossDBLinks = [linkAt 1 gridAct 1 "MJ", linkAt 2 gridAct 3.6 "MJ"]}

-- | The link a relink records for the spinning line at a position.
linkAt :: Int -> UUID -> Double -> Text -> CrossDBLink
linkAt position supplier factor unit =
    CrossDBLink
        { cdlConsumerActUUID = spinAct
        , cdlConsumerProdUUID = spinProd
        , cdlConsumerFlowId = marketFlow
        , cdlSupplierActUUID = supplier
        , cdlSupplierProdUUID = gridProd
        , cdlCoefficient = factor
        , cdlExchangeUnit = unit
        , cdlFlowName = "Electricity, medium voltage, at grid"
        , cdlLocation = "RER"
        , cdlSourceDatabase = "grid"
        , cdlTiedAlternatives = []
        , cdlAnswers = Just AnsweredExchange{aePosition = position, aeFactor = factor}
        }

lookupLine :: Int -> SimpleDatabase -> Maybe Exchange
lookupLine position sdb = do
    act <- M.lookup (spinAct, spinProd) (sdbActivities sdb)
    case drop position (exchanges act) of
        ex : _ -> Just ex
        [] -> Nothing

lineAt :: Int -> SimpleDatabase -> Exchange
lineAt position = fromMaybe (error "no such line") . lookupLine position

built :: SimpleDatabase -> IO Database
built sdb = DB.buildDatabaseWithMatrices (BuildInputs energyUnits mempty Declared []) sdb >>= either (fail . T.unpack) pure

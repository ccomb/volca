{-# LANGUAGE OverloadedStrings #-}

{- | What every reader of an activity's reference product answers, for the
three shapes an activity can take: one whose reference product is a known
flow, one with no reference exchange at all, and one whose reference
exchange names a flow the database does not hold.
-}
module ReferenceProductSpec (spec) where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types (
    ActivityComparison (..),
    ActivityForAPI (..),
    ActivityMatch (..),
    ActivitySummary (..),
    ChangedActivity (..),
    ConsumerResult (..),
    ConsumersResponse (..),
    DatabaseComparison (..),
    InventoryExport (..),
    InventoryMetadata (..),
    SearchResults (..),
 )
import Database (buildDatabaseWithMatrices)
import Database.Cutoffs (noCutoffs)
import Service (
    ActivityFilterCore (..),
    ConsumerFilter (..),
    Edges (..),
    convertActivityForAPI,
    convertToInventoryExport,
    functionalUnitOf,
    getConsumers,
    mkActivitySummary,
    resolveActivityAndProcessId,
 )
import Service.Compare (Sides (..), compareDatabases)
import TestHelpers (shippedGeographies)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Database (..),
    Exchange (..),
    LocationSource (..),
    ProcessId,
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    Unit (..),
    noDates,
    noDocumentation,
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "an activity's reference product" $ do
    it "is summarised with its name, declared amount and unit, or a stand-in" $ do
        db <- database 0
        summaries <- mapM (summaryOf db) [cheese, noReference, strayReference]
        map (\s -> (prsProductName s, prsProductAmount s, prsProductUnit s)) summaries
            `shouldBe` [("Cheese", 2.0, "kg"), ("", 1.0, ""), ("", 3.0, "kg")]

    it "is reported on an activity only when it has a name" $ do
        db <- database 0
        details <- mapM (detailOf db) [cheese, noReference, strayReference]
        map (\d -> (pfaProductName d, pfaProductAmount d, pfaProductUnit d)) details
            `shouldBe` [ (Just "Cheese", Just 2.0, Just "kg")
                       , (Nothing, Nothing, Nothing)
                       , (Nothing, Nothing, Nothing)
                       ]

    it "names the functional unit" $ do
        db <- database 0
        activities <- mapM (fmap snd . resolved db) [cheese, noReference, strayReference]
        map (functionalUnitOf (dbTechFlows db) (dbUnits db)) activities
            `shouldBe` ["1.00 kg of Cheese", "1.00  of ", "1.00 kg of "]

    it "heads an inventory export" $ do
        db <- database 0
        exports <- mapM (inventoryOf db) [cheese, noReference, strayReference]
        map ((\s -> (prsProductName s, prsProductAmount s, prsProductUnit s)) . imRootActivity . ieMetadata) exports
            `shouldBe` [("Cheese", 2.0, "kg"), ("", 1.0, ""), ("", 3.0, "kg")]

    -- The activity with no reference is refused a column, so it never consumes:
    -- the flow missing from the table is the case a consumer can carry.
    it "describes a consumer whose reference flow is missing, and keeps it" $ do
        db <- database 0
        response <- either (fail . show) pure $ getConsumers shippedGeographies db "test-db" (processIdOf cheese) consumers
        sortOn (\(n, _, _, _) -> n) [(crActivityName c, crProductName c, crProductAmount c, crProductUnit c) | c <- srResults (crrResults response)]
            `shouldBe` [("cheese ripening", "", 3.0, "kg")]

    it "keys the comparison by name only when it has one" $ do
        base <- database 0
        other <- database 10
        let c = compareDatabases Sides{baseSide = base, otherSide = other}
        sortOn fst [(prsActivityName (acmpBase (chaComparison ch)), chaMatch ch) | ch <- dbcChanged c]
            `shouldBe` [("cheese production", SameNames), ("cheese ripening", SameProduct)]
        map prsActivityName (dbcRemoved c) `shouldBe` ["cheese tasting"]
        map prsActivityName (dbcAdded c) `shouldBe` ["cheese tasting"]

-- | One activity of the fixture: its number, the product its key names, its name.
data Fixture = Fixture {fxActivity :: Int, fxKeyProduct :: UUID, fxName :: Text}

cheese, noReference, strayReference :: Fixture
cheese = Fixture 1 (uuid 100) "cheese production"
-- No exchange of this one is the reference; its key names a product no line carries.
noReference = Fixture 2 (uuid 200) "cheese tasting"
-- Its reference line names flow 300, which the tech-flow table does not hold.
strayReference = Fixture 3 (uuid 300) "cheese ripening"

processIdOf :: Fixture -> Text
processIdOf = processIdAt 0

processIdAt :: Int -> Fixture -> Text
processIdAt shift fx = UUID.toText (uuid (fxActivity fx + shift)) <> "_" <> UUID.toText (fxKeyProduct fx)

resolved :: Database -> Fixture -> IO (ProcessId, Activity)
resolved db fx = either (fail . show) pure (resolveActivityAndProcessId db (processIdOf fx))

summaryOf :: Database -> Fixture -> IO ActivitySummary
summaryOf db fx = uncurry (mkActivitySummary db) <$> resolved db fx

detailOf :: Database -> Fixture -> IO ActivityForAPI
detailOf db fx = uncurry (convertActivityForAPI db) <$> resolved db fx

inventoryOf :: Database -> Fixture -> IO InventoryExport
inventoryOf db fx = do
    (p, act) <- resolved db fx
    pure (convertToInventoryExport db (dbBioFlows db) (dbUnits db) p act noCutoffs M.empty)

consumers :: ConsumerFilter
consumers =
    ConsumerFilter
        ActivityFilterCore
            { afcName = Nothing
            , afcLocation = Nothing
            , afcProduct = Nothing
            , afcClassifications = []
            , afcLimit = Nothing
            , afcOffset = Nothing
            , afcSort = Nothing
            , afcOrder = Nothing
            }
        Nothing
        EntriesOnly

{- | The three activities, their identifiers shifted by the argument, so a
second version renumbers every activity and the comparison has to pair them
by something else. Each version emits a different amount, so a paired
activity is a change rather than unchanged.
-}
database :: Int -> IO Database
database shift = do
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig M.empty Declared [])
            SimpleDatabase
                { sdbActivities =
                    M.fromList
                        [ entry cheese [reference (uuid 100) 2.0, co2Line]
                        , entry noReference [eats, co2Line]
                        , entry strayReference [reference (uuid 300) 3.0, eats, co2Line]
                        ]
                , sdbTechFlows = M.fromList [(uuid 100, cheeseFlow)]
                , sdbBioFlows = M.fromList [(bfId co2, co2)]
                , sdbWasteFlows = M.empty
                , sdbUnits = M.fromList [(unitId kg, kg)]
                , sdbDocumentation = noDocumentation
                }
    either (fail . ("buildDatabaseWithMatrices: " <>) . show) pure built
  where
    entry :: Fixture -> [Exchange] -> ((UUID, UUID), Activity)
    entry fx lines' = ((uuid (fxActivity fx + shift), fxKeyProduct fx), activity (fxName fx) lines')
    co2Line :: Exchange
    co2Line = emission (1 + fromIntegral shift)
    eats :: Exchange
    eats = techLine (uuid 100) 0.5 Input (Just (uuid (fxActivity cheese + shift)))
    reference :: UUID -> Double -> Exchange
    reference flow amount = techLine flow amount ReferenceProduct Nothing

activity :: Text -> [Exchange] -> Activity
activity name lines' =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = lines'
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

techLine :: UUID -> Double -> TechRole -> Maybe UUID -> Exchange
techLine flow amount role supplier =
    TechnosphereExchange
        { techFlowId = flow
        , techAmount = amount
        , techUnitId = unitId kg
        , techRole = role
        , techActivityLinkId = supplier
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

emission :: Double -> Exchange
emission amount =
    BiosphereExchange
        { bioFlowId = bfId co2
        , bioAmount = amount
        , bioUnitId = unitId kg
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

cheeseFlow :: TechnosphereFlow
cheeseFlow =
    TechnosphereFlow
        { tfId = uuid 100
        , tfName = "Cheese"
        , tfUnitId = unitId kg
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

co2 :: BiosphereFlow
co2 =
    BiosphereFlow
        { bfId = uuid 10
        , bfName = "Carbon dioxide, fossil"
        , bfUnitId = unitId kg
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Nothing
        }

kg :: Unit
kg = Unit{unitId = uuid 1, unitName = "kg", unitSymbol = "kg", unitComment = ""}

uuid :: Int -> UUID
uuid n = UUID.fromWords64 (fromIntegral n) 0

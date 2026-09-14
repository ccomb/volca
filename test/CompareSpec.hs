{-# LANGUAGE OverloadedStrings #-}

{- | Comparing two activities, and two versions of a database.

Every database here is a handful of activities whose identifiers the test
chooses, so each case changes one thing between the two versions and says
what the comparison must make of it.
-}
module CompareSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types (
    ActivityComparison (..),
    ActivityMatch (..),
    AmbiguousActivities (..),
    ChangedActivity (..),
    DatabaseComparison (..),
    ExchangeChange (..),
    LineChange (..),
    LineMatch (..),
    LineRole (..),
    Quantity (..),
    SummaryChange (..),
    UncomparedLine (..),
    UncomparedReason (..),
 )
import Database (buildDatabaseWithMatrices)
import Service.Compare (Sides (..), compareActivities, compareDatabases, limitComparison, resolveProcess)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Database,
    Exchange (..),
    LocationSource (..),
    NativeActivityType (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    Unit (..),
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "Service.Compare" $ do
    describe "lines" $ do
        it "a version compared with itself reports nothing" $ do
            let rows = [row 1 wheat "wheat production" [emits co2 kg 1], row 2 barley "barley production" []]
            c <- compareVersions rows rows
            counts c `shouldBe` (0, 0, 0, 0, 2)

        it "an amount moved by float noise is no change" $ do
            c <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1]] [row 1 wheat "wheat production" [emits co2 kg (1 + 1e-12)]]
            dbcChangedCount c `shouldBe` 0

        it "an amount that moved is a change, found by its flow" $ do
            c <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1]] [row 1 wheat "wheat production" [emits co2 kg 2]]
            comparison <- onlyChange c
            acmpExchanges comparison
                `shouldBe` [ ExchangeChange
                                { ecFlowId = bfId co2
                                , ecFlowName = "Carbon dioxide, fossil"
                                , ecCompartment = Nothing
                                , ecRole = BioLine Emission
                                , ecChange = LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg")
                                }
                           ]

        it "a unit written under another identifier is the same unit" $ do
            c <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1]] [row 1 wheat "wheat production" [emits co2 kgAgain 1]]
            dbcChangedCount c `shouldBe` 0

        it "a line written in another unit is a change" $ do
            c <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1]] [row 1 wheat "wheat production" [emits co2 gram 1]]
            comparison <- onlyChange c
            map ecChange (acmpExchanges comparison) `shouldBe` [LineChanged SameFlow (Quantity 1 "kg") (Quantity 1 "g")]

        it "a flow changing role is one line removed and one added" $ do
            c <- compareVersions [row 1 wheat "wheat production" [takes water kg 1]] [row 1 wheat "wheat production" [emits water kg 1]]
            comparison <- onlyChange c
            map (\e -> (ecRole e, ecChange e)) (acmpExchanges comparison)
                `shouldMatchList` [ (BioLine Resource, LineRemoved (Quantity 1 "kg"))
                                  , (BioLine Emission, LineAdded (Quantity 1 "kg"))
                                  ]

        it "sums the lines of one flow in one unit" $ do
            c <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 0.5, emits co2 kg 0.5]] [row 1 wheat "wheat production" [emits co2 kg 1]]
            dbcChangedCount c `shouldBe` 0

        it "names a flow written in two units rather than summing it" $ do
            c <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1, emits co2 gram 1]] [row 1 wheat "wheat production" [emits co2 kg 1]]
            comparison <- onlyChange c
            acmpExchanges comparison `shouldBe` []
            acmpUncompared comparison
                `shouldBe` [UncomparedLine "Carbon dioxide, fossil" Nothing (BioLine Emission) (MixedUnits ["g", "kg"] ["kg"])]

        it "finds a renumbered flow again by its name, whatever its case" $ do
            same <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1]] [row 1 wheat "wheat production" [emits co2Renumbered kg 1]]
            dbcChangedCount same `shouldBe` 0
            moved <- compareVersions [row 1 wheat "wheat production" [emits co2 kg 1]] [row 1 wheat "wheat production" [emits co2Renumbered kg 3]]
            comparison <- onlyChange moved
            map ecChange (acmpExchanges comparison) `shouldBe` [LineChanged SameFlowName (Quantity 1 "kg") (Quantity 3 "kg")]

        it "says a product's amount once, on its line" $ do
            c <- compareVersions [row 1 wheat "wheat production" []] [(row 1 wheat "wheat production" []){rowAmount = 2}]
            comparison <- onlyChange c
            acmpSummary comparison `shouldBe` []
            map ecRole (acmpExchanges comparison) `shouldBe` [TechLine ReferenceProduct]

        it "compares two activities of two databases" $ do
            original <- database [row 1 wheat "wheat production" [emits co2 kg 1]]
            adapted <- database [row 9 wheat "wheat production, adapted" [emits co2 kg 2]]
            sides <- either (fail . show) pure $ Sides <$> resolveProcess original (pid 1 wheat) <*> resolveProcess adapted (pid 9 wheat)
            let comparison = compareActivities sides
            acmpSummary comparison `shouldBe` [ActivityNameChanged "wheat production" "wheat production, adapted"]
            map ecChange (acmpExchanges comparison) `shouldBe` [LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg")]

    describe "pairing activities" $ do
        it "pairs regenerated identifiers by name, case and geography aside" $ do
            c <- compareVersions [row 1 wheat "wheat production" []] [row 2 (productFlow 200 "Wheat grain {FR}") "Wheat Production {FR}" []]
            map chaMatch (dbcChanged c) `shouldBe` [SameNames]
            (dbcAddedCount c, dbcRemovedCount c) `shouldBe` (0, 0)

        it "pairs a renamed activity by its product" $ do
            c <- compareVersions [row 1 barley "barley production" []] [row 2 barley "barley grain production" []]
            map chaMatch (dbcChanged c) `shouldBe` [SameProduct]
            comparison <- onlyChange c
            acmpSummary comparison `shouldBe` [ActivityNameChanged "barley production" "barley grain production"]

        it "never pairs the market for a product with its production" $ do
            c <- compareVersions [(row 1 oat "market for oat grain" []){rowType = ecoSpold 2 Nothing}] [(row 2 oat "oat grain production" []){rowType = ecoSpold 1 Nothing}]
            counts c `shouldBe` (1, 1, 0, 0, 0)

        it "keeps pairing by product when only the special activity type changed" $ do
            c <- compareVersions [(row 1 oat "oat production" []){rowType = ecoSpold 1 Nothing}] [(row 2 oat "oat grain production" []){rowType = ecoSpold 1 (Just 2)}]
            map chaMatch (dbcChanged c) `shouldBe` [SameProduct]

        it "names several candidates and leaves them out of the looser rungs" $ do
            c <-
                compareVersions
                    [row 1 rye "rye production" [], row 2 (productFlow 510 "rye grain") "rye production" []]
                    [row 3 rye "rye production" []]
            counts c `shouldBe` (0, 0, 0, 1, 0)
            map (\a -> (ambMatch a, length (ambBase a), length (ambOther a))) (dbcAmbiguous c) `shouldBe` [(SameNames, 2, 1)]

        it "lets a key present on one side only reach the next rung" $ do
            c <-
                compareVersions
                    [row 1 maize "maize production" [], row 2 (productFlow 610 "maize grain") "maize production" []]
                    [row 3 maize "maize grain production" []]
            map chaMatch (dbcChanged c) `shouldBe` [SameProduct]
            dbcRemovedCount c `shouldBe` 1

    it "a limit shortens the lists, never the counts" $ do
        c <- compareVersions [row 1 wheat "wheat production" []] [row 1 wheat "wheat production" [], row 2 barley "barley production" [], row 3 oat "oat production" []]
        let limited = limitComparison 1 c
        (dbcAddedCount limited, length (dbcAdded limited)) `shouldBe` (2, 1)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Added, removed, changed, ambiguous, unchanged.
counts :: DatabaseComparison -> (Int, Int, Int, Int, Int)
counts c = (dbcAddedCount c, dbcRemovedCount c, dbcChangedCount c, dbcAmbiguousCount c, dbcUnchangedCount c)

compareVersions :: [Row] -> [Row] -> IO DatabaseComparison
compareVersions older newer = do
    base <- database older
    other <- database newer
    pure (compareDatabases Sides{baseSide = base, otherSide = other})

onlyChange :: DatabaseComparison -> IO ActivityComparison
onlyChange c = case dbcChanged c of
    [one] -> pure (chaComparison one)
    several -> fail ("expected one changed activity, got " <> show (length several))

uuid :: Int -> UUID
uuid n = UUID.fromWords64 (fromIntegral n) 0

pid :: Int -> TechnosphereFlow -> Text
pid n flow = UUID.toText (uuid n) <> "_" <> UUID.toText (tfId flow)

kg, gram, kgAgain :: Unit
kg = Unit{unitId = uuid 1, unitName = "kg", unitSymbol = "kg", unitComment = ""}
gram = Unit{unitId = uuid 2, unitName = "g", unitSymbol = "g", unitComment = ""}
-- The same unit under another identifier, as a second format would mint it.
kgAgain = Unit{unitId = uuid 3, unitName = "kg", unitSymbol = "kg", unitComment = ""}

co2, co2Renumbered, water :: BiosphereFlow
co2 = bioFlow 10 "Carbon dioxide, fossil"
co2Renumbered = bioFlow 11 "carbon dioxide, fossil"
water = bioFlow 12 "Water"

bioFlow :: Int -> Text -> BiosphereFlow
bioFlow n name =
    BiosphereFlow
        { bfId = uuid n
        , bfName = name
        , bfUnitId = unitId kg
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Nothing
        }

wheat, barley, oat, rye, maize :: TechnosphereFlow
wheat = productFlow 100 "wheat grain"
barley = productFlow 300 "barley grain"
oat = productFlow 400 "oat grain"
rye = productFlow 500 "rye grain"
maize = productFlow 600 "maize grain"

productFlow :: Int -> Text -> TechnosphereFlow
productFlow n name =
    TechnosphereFlow
        { tfId = uuid n
        , tfName = name
        , tfUnitId = unitId kg
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

ecoSpold :: Int -> Maybe Int -> Maybe NativeActivityType
ecoSpold code special =
    Just
        EcoSpoldActivityType
            { eatCode = code
            , eatLabel = "type " <> UUID.toText (uuid code)
            , eatSpecialCode = special
            , eatSpecialLabel = Nothing
            }

emits, takes :: BiosphereFlow -> Unit -> Double -> Exchange
emits = bioLine Emission
takes = bioLine Resource

bioLine :: BioDirection -> BiosphereFlow -> Unit -> Double -> Exchange
bioLine direction flow unit amount =
    BiosphereExchange
        { bioFlowId = bfId flow
        , bioAmount = amount
        , bioUnitId = unitId unit
        , bioDirection = direction
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

-- | One process of a test database, located in France and producing one kilogram unless a case says otherwise.
data Row = Row
    { rowActivity :: Int
    , rowProduct :: TechnosphereFlow
    , rowName :: Text
    , rowAmount :: Double
    , rowType :: Maybe NativeActivityType
    , rowLines :: [Exchange]
    }

row :: Int -> TechnosphereFlow -> Text -> [Exchange] -> Row
row n flow name lines' = Row{rowActivity = n, rowProduct = flow, rowName = name, rowAmount = 1, rowType = Nothing, rowLines = lines'}

database :: [Row] -> IO Database
database rows = do
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig M.empty Declared)
            SimpleDatabase
                { sdbActivities = M.fromList (map entry rows)
                , sdbTechFlows = M.fromList [(tfId (rowProduct r), rowProduct r) | r <- rows]
                , sdbBioFlows = M.fromList [(bfId f, f) | f <- [co2, co2Renumbered, water]]
                , sdbWasteFlows = M.empty
                , sdbUnits = M.fromList [(unitId u, u) | u <- [kg, gram, kgAgain]]
                }
    either (fail . ("buildDatabaseWithMatrices: " <>) . show) pure built

entry :: Row -> ((UUID, UUID), Activity)
entry r =
    ( (uuid (rowActivity r), tfId (rowProduct r))
    , Activity
        { activityName = rowName r
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = reference : rowLines r
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = rowType r
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }
    )
  where
    reference :: Exchange
    reference =
        TechnosphereExchange
            { techFlowId = tfId (rowProduct r)
            , techAmount = rowAmount r
            , techUnitId = unitId kg
            , techRole = ReferenceProduct
            , techActivityLinkId = Just (uuid (rowActivity r))
            , techSupplierClaim = ClaimByProduct
            , techLocation = ""
            , techComment = Nothing
            , techPedigree = Nothing
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }

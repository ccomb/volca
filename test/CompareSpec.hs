{-# LANGUAGE OverloadedStrings #-}

{- | Comparing two activities, and two versions of a database.

Every database here is a handful of activities whose identifiers the test
chooses, so each case changes one thing between the two versions and says
what the comparison must make of it.
-}
module CompareSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types (
    ActivityComparison (..),
    ActivityMatch (..),
    AmbiguousActivities (..),
    ChangeOutcome (..),
    ChangePresence (..),
    ChangedActivity (..),
    ChangesApplied (..),
    ChangesPresence (..),
    ChangesQuery (..),
    DatabaseComparison (..),
    ExchangeChange (..),
    LineChange (..),
    LineMatch (..),
    LineRole (..),
    Quantity (..),
    SummaryChange (..),
    Supplier (..),
    UncomparedLine (..),
    UncomparedReason (..),
 )
import Database (buildDatabaseWithMatrices)
import Database.Author (AuthorContext (..), EditedActivity (..), ExchangeEdit, applyExchangeEdits, authoredActivityUUID)
import Service.Compare (ProcessIn (..), Sides (..), applicableChanges, changesPresent, compareActivities, compareDatabases, limitComparison, resolveProcess)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    CrossDBLink (..),
    Database,
    DatasetDates (..),
    Exchange (..),
    LocationSource (..),
    NativeActivityType (..),
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

        it "never reads a unit the registry lacks as a unit name" $ do
            fixed <- compareVersions [row 1 wheat "wheat production" [emits co2 strayUnit 1]] [row 1 wheat "wheat production" [emits co2 kg 1]]
            comparison <- onlyChange fixed
            map ecChange (acmpExchanges comparison)
                `shouldBe` [LineChanged SameFlow (Quantity 1 ("<unresolved unit " <> UUID.toText (unitId strayUnit) <> ">")) (Quantity 1 "kg")]
            twoStrays <- compareVersions [row 1 wheat "wheat production" [emits co2 strayUnit 1]] [row 1 wheat "wheat production" [emits co2 otherStrayUnit 1]]
            dbcChangedCount twoStrays `shouldBe` 1

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

        it "reads a flow written in two units the same way on both sides as no change" $ do
            let rows = [row 1 wheat "wheat production" [emits co2 kg 1, emits co2 gram 1]]
            c <- compareVersions rows rows
            dbcChangedCount c `shouldBe` 0

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

    describe "texts and suppliers" $ do
        it "reports a description that changed, paragraph by paragraph" $ do
            c <- compareVersions [row 1 wheat "wheat production" []] [(row 1 wheat "wheat production" []){rowDescription = ["From the 2024 survey."]}]
            comparison <- onlyChange c
            acmpSummary comparison `shouldBe` [DescriptionChanged [] ["From the 2024 survey."]]

        it "reports an input drawn from another supplier, named by activity and location" $ do
            let suppliers = [row 2 barley "barley production" [], row 3 barley "barley production, organic" []]
            c <- compareVersions (row 1 wheat "wheat production" [buys 2 barley 1] : suppliers) (row 1 wheat "wheat production" [buys 3 barley 1] : suppliers)
            comparison <- onlyChange c
            map ecChange (acmpExchanges comparison)
                `shouldBe` [SupplierChanged (Supplier "barley production" "FR") (Supplier "barley production, organic" "FR")]

        it "does not report a renamed supplier on the activities that buy from it" $ do
            c <- compareVersions [row 1 wheat "wheat production" [buys 2 barley 1], row 2 barley "barley production" []] [row 1 wheat "wheat production" [buys 2 barley 1], row 2 barley "barley production, corrected" []]
            comparison <- onlyChange c
            acmpSummary comparison `shouldBe` [ActivityNameChanged "barley production" "barley production, corrected"]
            acmpExchanges comparison `shouldBe` []

        it "compares only the total of a flow drawn from several suppliers" $ do
            let suppliers = [row 2 barley "barley production" [], row 3 barley "barley production, organic" []]
            c <- compareVersions (row 1 wheat "wheat production" [buys 2 barley 0.5, buys 3 barley 0.5] : suppliers) (row 1 wheat "wheat production" [buys 2 barley 1] : suppliers)
            dbcChangedCount c `shouldBe` 0

    describe "changes looked for in an activity" $ do
        let raised = ExchangeChange (bfId co2) "Carbon dioxide, fossil" Nothing (BioLine Emission) (LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg"))
            renamed = ActivityNameChanged "wheat production" "wheat production, corrected"
            presence name lines' = do
                db <- database [row 1 wheat name lines']
                p <- either (fail . show) pure (resolveProcess db (pid 1 wheat))
                let answer = changesPresent p (ChangesQuery [renamed] [raised])
                pure (cpSummary answer, cpExchanges answer)
        it "finds a change a later version made" $
            presence "wheat production, corrected" [emits co2 kg 2] `shouldReturn` ([ChangePresent], [ChangePresent])
        it "says a version still holds what the change replaced" $
            presence "wheat production" [emits co2 kg 1] `shouldReturn` ([ChangeAbsent], [ChangeAbsent])
        it "tells a third value apart from both" $
            presence "wheat" [emits co2 kg 3] `shouldReturn` ([ChangeDifferent], [ChangeDifferent])
        it "says when the line a change is about is gone" $
            presence "wheat production" [] `shouldReturn` ([ChangeAbsent], [ChangeLineGone])
        it "finds a renumbered flow by its name" $
            presence "wheat production" [emits co2Renumbered kg 2] `shouldReturn` ([ChangeAbsent], [ChangePresent])

    describe "changes applied to an activity" $ do
        let lowered = ExchangeChange (bfId co2) "Carbon dioxide, fossil" Nothing (BioLine Emission) (LineChanged SameFlow (Quantity 1 "kg") (Quantity 0.5 "kg"))
            relocated = LocationChanged "FR" "FR-N"
            renamed = ActivityNameChanged "wheat production" "wheat production, corrected"
            barleyLine = ExchangeChange (tfId barley) "barley grain" Nothing (TechLine Input)
            organic = SupplierChanged (Supplier "barley production" "FR") (Supplier "barley production, organic" "FR")
            barleySuppliers = [row 2 barley "barley production" [], row 3 barley "barley production, organic" []]
        it "applies what the activity does not say yet, and finds it present on a second pass" $ do
            (p, ctx) <- wheatIn [emits co2 kg 1] []
            let query = ChangesQuery [renamed, relocated] [lowered]
                (answer, edits) = applicableChanges ctx p query
            (capSummary answer, capExchanges answer) `shouldBe` ([OutcomeApplied, OutcomeApplied], [OutcomeApplied])
            after <- applied ctx p edits
            (activityName after, activityLocation after) `shouldBe` ("wheat production, corrected", "FR-N")
            [a | BiosphereExchange{bioAmount = a} <- exchanges after] `shouldBe` [0.5]
            let (again, none) = applicableChanges ctx p{inActivity = after} query
            (capSummary again, capExchanges again, length none) `shouldBe` ([OutcomePresent, OutcomePresent], [OutcomePresent], 0)
        it "leaves a value the database changed otherwise, and a line that is gone" $ do
            (p, ctx) <- wheatIn [emits co2 kg 3] []
            let gone = ExchangeChange (bfId water) "Water" Nothing (BioLine Resource) (LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg"))
                (answer, edits) = applicableChanges ctx p (ChangesQuery [] [lowered, gone])
            (capExchanges answer, length edits) `shouldBe` ([OutcomeDifferent, OutcomeLineGone], 0)
        it "does not write an amount stated in another unit" $ do
            (p, ctx) <- wheatIn [emits co2 kg 1] []
            let inGrams = lowered{ecChange = LineChanged SameFlow (Quantity 1 "kg") (Quantity 500 "g")}
            notApplied ctx p inGrams "The change is in g and the line in kg."
        it "does not set one amount on a flow drawn from several suppliers" $ do
            (p, ctx) <- wheatIn [buys 2 barley 0.5, buys 3 barley 0.5] barleySuppliers
            notApplied ctx p (barleyLine (LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg"))) "Several lines answer to this flow: an edit would change each of them."
        it "does not select a line supplied from another database" $ do
            (p, ctx) <- wheatIn [buysUnlinked oat 1] []
            notApplied
                ctx
                p{inLinks = M.singleton (tfId oat) (oatFrom "grains-db")}
                (ExchangeChange (tfId oat) "oat grain" Nothing (TechLine Input) (LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg")))
                "The supplier lives in grains-db: an edit selects a line by a supplier of this database."
        it "moves a line to another supplier at its new amount" $ do
            (p, ctx) <- wheatIn [buys 2 barley 1] barleySuppliers
            let (answer, edits) = applicableChanges ctx p (ChangesQuery [] [barleyLine organic, barleyLine (LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg"))])
            capExchanges answer `shouldBe` [OutcomeApplied, OutcomeApplied]
            after <- applied ctx p edits
            [(l, a) | TechnosphereExchange{techRole = Input, techActivityLinkId = l, techAmount = a} <- exchanges after] `shouldBe` [(Just (uuid 3), 2)]
        it "reports a change no edit can make without holding back the others" $ do
            (p, ctx) <- wheatIn [] barleySuppliers
            let addedBarley = barleyLine (LineAdded (Quantity 1 "kg"))
                addedWater = ExchangeChange (bfId water) "Water" Nothing (BioLine Resource) (LineAdded (Quantity 3 "kg"))
                (answer, edits) = applicableChanges ctx p (ChangesQuery [ProductNameChanged "wheat grain" "soft wheat grain", renamed] [addedBarley, addedWater])
            capSummary answer `shouldBe` [OutcomeNotApplicable "An edit does not rename a product.", OutcomeApplied]
            capExchanges answer
                `shouldBe` [OutcomeNotApplicable "Several activities of this database make this product: the change does not say which one supplies the added line.", OutcomeApplied]
            after <- applied ctx p edits
            (activityName after, [a | BiosphereExchange{bioAmount = a} <- exchanges after]) `shouldBe` ("wheat production, corrected", [3])
        it "does not select a line supplied by its product rather than linked to its supplier" $ do
            (p, ctx) <- wheatIn [buysUnlinked barley 1] [row 2 barley "barley production" []]
            notApplied
                ctx
                p
                (barleyLine (LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg")))
                "The line is supplied by its product, not linked to a supplier: an edit selects a line by the supplier it is linked to."
        it "does not rename an activity written here, whose name and location make its identity" $ do
            let written = authoredActivityUUID "wheat production" "FR"
            db <- databaseKeyed (const written) [row 1 wheat "wheat production" [emits co2 kg 1]]
            p <- either (fail . show) pure (resolveProcess db (UUID.toText written <> "_" <> UUID.toText (tfId wheat)))
            let ctx = AuthorContext{acDb = db, acDeps = [], acUnitConfig = defaultUnitConfig}
                (answer, edits) = applicableChanges ctx p (ChangesQuery [renamed] [lowered])
            (capSummary answer, capExchanges answer)
                `shouldBe` ([OutcomeNotApplicable "This activity was written here and its name and location make its identity: rewrite it to rename or relocate it."], [OutcomeApplied])
            after <- applied ctx p edits
            (activityName after, [a | BiosphereExchange{bioAmount = a} <- exchanges after]) `shouldBe` ("wheat production", [0.5])
        it "makes a change asked twice once" $ do
            (p, ctx) <- wheatIn [] []
            let addedWater = ExchangeChange (bfId water) "Water" Nothing (BioLine Resource) (LineAdded (Quantity 3 "kg"))
                (answer, edits) = applicableChanges ctx p (ChangesQuery [renamed, renamed] [addedWater, addedWater])
            (capSummary answer, capExchanges answer) `shouldBe` ([OutcomeApplied, OutcomeApplied], [OutcomeApplied, OutcomeApplied])
            after <- applied ctx p edits
            (activityName after, [a | BiosphereExchange{bioAmount = a} <- exchanges after]) `shouldBe` ("wheat production, corrected", [3])
        it "refuses a change that would undo one applied before it" $ do
            (p, ctx) <- wheatIn [emits co2 kg 1] []
            let raised = lowered{ecChange = LineChanged SameFlow (Quantity 1 "kg") (Quantity 2 "kg")}
                (answer, edits) = applicableChanges ctx p (ChangesQuery [] [lowered, raised])
            capExchanges answer `shouldBe` [OutcomeApplied, OutcomeNotApplicable "Another change of this request says otherwise."]
            after <- applied ctx p edits
            [a | BiosphereExchange{bioAmount = a} <- exchanges after] `shouldBe` [0.5]

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

    describe "dates" $ do
        it "counts a pair that differs by its dates alone as redated, not changed" $ do
            let dated year = (row 1 wheat "wheat production" []){rowDates = noDates{datesLastRevised = Just (fromGregorian year 1 1)}}
            c <- compareVersions [dated 2023] [dated 2024]
            counts c `shouldBe` (0, 0, 0, 0, 0)
            dbcRedatedCount c `shouldBe` 1

        it "counts a pair whose lines moved as changed, its dates with it" $ do
            let dated year amount = (row 1 wheat "wheat production" [emits co2 kg amount]){rowDates = noDates{datesLastRevised = Just (fromGregorian year 1 1)}}
            c <- compareVersions [dated 2023 1] [dated 2024 2]
            (dbcChangedCount c, dbcRedatedCount c) `shouldBe` (1, 0)

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

-- Units a line names but the registry of its database lacks.
strayUnit, otherStrayUnit :: Unit
strayUnit = Unit{unitId = uuid 4, unitName = "kg", unitSymbol = "kg", unitComment = ""}
otherStrayUnit = Unit{unitId = uuid 5, unitName = "kg", unitSymbol = "kg", unitComment = ""}

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

-- | An input of one kilogram-unit product, linked to the activity that supplies it.
buys :: Int -> TechnosphereFlow -> Double -> Exchange
buys supplier flow amount =
    TechnosphereExchange
        { techFlowId = tfId flow
        , techAmount = amount
        , techUnitId = unitId kg
        , techRole = Input
        , techActivityLinkId = Just (uuid supplier)
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

-- | An input that names no supplier: one the loader linked to another database, or nothing.
buysUnlinked :: TechnosphereFlow -> Double -> Exchange
buysUnlinked flow amount = (buys 0 flow amount){techActivityLinkId = Nothing}

-- | A link to oat grain made in another database.
oatFrom :: Text -> CrossDBLink
oatFrom source =
    CrossDBLink
        { cdlConsumerActUUID = uuid 1
        , cdlConsumerProdUUID = tfId wheat
        , cdlConsumerFlowId = tfId oat
        , cdlSupplierActUUID = uuid 40
        , cdlSupplierProdUUID = tfId oat
        , cdlCoefficient = 1
        , cdlExchangeUnit = "kg"
        , cdlFlowName = "oat production"
        , cdlLocation = "FR"
        , cdlSourceDatabase = source
        , cdlTiedAlternatives = []
        }

-- | Wheat production with these lines, in a database beside these other rows.
wheatIn :: [Exchange] -> [Row] -> IO (ProcessIn, AuthorContext)
wheatIn lines' others = do
    db <- database (row 1 wheat "wheat production" lines' : others)
    p <- either (fail . show) pure (resolveProcess db (pid 1 wheat))
    pure (p, AuthorContext{acDb = db, acDeps = [], acUnitConfig = defaultUnitConfig})

-- | The activity once these edits are made.
applied :: AuthorContext -> ProcessIn -> [ExchangeEdit] -> IO Activity
applied ctx p edits = either (fail . show) (pure . eaActivity) (applyExchangeEdits ctx edits (inActivity p))

-- | A change left as it is, for this reason, with nothing to edit.
notApplied :: AuthorContext -> ProcessIn -> ExchangeChange -> Text -> Expectation
notApplied ctx p change reason = do
    let (answer, edits) = applicableChanges ctx p (ChangesQuery [] [change])
    (capExchanges answer, edits) `shouldBe` ([OutcomeNotApplicable reason], [])

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
    , rowDates :: DatasetDates
    , rowDescription :: [Text]
    }

row :: Int -> TechnosphereFlow -> Text -> [Exchange] -> Row
row n flow name lines' = Row{rowActivity = n, rowProduct = flow, rowName = name, rowAmount = 1, rowType = Nothing, rowLines = lines', rowDates = noDates, rowDescription = []}

database :: [Row] -> IO Database
database = databaseKeyed (uuid . rowActivity)

-- | A database whose activities are keyed by this function of their row.
databaseKeyed :: (Row -> UUID) -> [Row] -> IO Database
databaseKeyed key rows = do
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig M.empty Declared [])
            SimpleDatabase
                { sdbActivities = M.fromList (map (entry key) rows)
                , sdbTechFlows = M.fromList [(tfId (rowProduct r), rowProduct r) | r <- rows]
                , sdbBioFlows = M.fromList [(bfId f, f) | f <- [co2, co2Renumbered, water]]
                , sdbWasteFlows = M.empty
                , sdbUnits = M.fromList [(unitId u, u) | u <- [kg, gram, kgAgain]]
                , sdbDocumentation = noDocumentation
                }
    either (fail . ("buildDatabaseWithMatrices: " <>) . show) pure built

entry :: (Row -> UUID) -> Row -> ((UUID, UUID), Activity)
entry key r =
    ( (key r, tfId (rowProduct r))
    , Activity
        { activityName = rowName r
        , activityDescription = rowDescription r
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
        , activityDates = rowDates r
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
            , techActivityLinkId = Just (key r)
            , techSupplierClaim = ClaimByProduct
            , techLocation = ""
            , techComment = Nothing
            , techPedigree = Nothing
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }

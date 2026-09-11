{-# LANGUAGE OverloadedStrings #-}

{- | An allocated dataset is one activity written as one row per coproduct, all
sharing an activity UUID. Anything keyed on that UUID alone therefore answers
with an arbitrary one of them. These tests pin the places a consumer reads a
coproduct back: the target of an exchange, the tree it descends, and the rows
that use a flow.

The fixture is built so the wrong answer is visible: the input names the
coproduct with the /lower/ product UUID, which is exactly the row a
UUID-keyed map drops.
-}
module CoproductTargetSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types (ActivityForAPI (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), NodeType (..), ProducerFilter (..), TreeEdge (..), TreeExport (..))
import Database (buildDatabaseWithMatrices)
import qualified Service
import TestHelpers (unitDef)
import Tree (buildLoopAwareTree)
import Types
import UnitConversion (UnitConfig (..), defaultUnitConfig)

spec :: Spec
spec = do
    describe "the target of an exchange" $ do
        it "names the coproduct the input asks for, not another product of the same activity" $ do
            db <- twoCoproductFixture
            targetPidOfInput db `shouldBe` Just (pidText milkId)

        it "names it on the exchange-details path too" $ do
            db <- twoCoproductFixture
            targetSummaryOfInput db `shouldBe` Just (pidText milkId)

        it "reports a waste output whose treatment is not loaded as such" $ do
            db <- untreatedWasteFixture
            wasteLineOf db `shouldBe` (Nothing, Just TreatmentNotLoaded)

    describe "the tree" $ do
        it "descends into the coproduct the input asks for" $ do
            db <- twoCoproductFixture
            case treeOfConsumer db of
                Right (TreeNode _ _ [TreeChild{childSubtree = TreeLeaf pid _}]) -> processIdToText db pid `shouldBe` pidText milkId
                Right other -> expectationFailure ("unexpected tree shape: " <> show (shapeOf other))
                Left err -> expectationFailure (show err)

        it "keeps a declared link no row satisfies as a visible node" $ do
            db <- danglingLinkFixture
            case treeOfConsumer db of
                Right (TreeNode _ _ [TreeChild{childSubtree = TreeMissing uuid _ _}]) -> uuid `shouldBe` ghostActId
                Right other -> expectationFailure ("unexpected tree shape: " <> show (shapeOf other))
                Left err -> expectationFailure (show err)

        it "exports that node instead of dropping the branch" $ do
            db <- danglingLinkFixture
            case treeOfConsumer db of
                Left err -> expectationFailure (show err)
                Right tree -> do
                    let export = Service.convertToTreeExport db 10 tree
                        missing = [n | n <- M.elems (teNodes export), enNodeType n == MissingNode]
                    map enLoopTarget missing `shouldBe` [Nothing]
                    length (teEdges export) `shouldBe` 1

        it "states the amount reaching it in the unit the edge is labelled with" $ do
            db <- danglingLinkGramsFixture
            case consumerRow db of
                Nothing -> expectationFailure "no consumer row"
                Just root ->
                    -- 2000 g of a flow measured in kg, on an edge that says kg.
                    let tree = buildLoopAwareTree gramsAware db 10 root
                     in case teEdges (Service.convertToTreeExport db 10 tree) of
                            [edge] -> (teQuantity edge, teUnit edge) `shouldBe` (2.0, "kg")
                            other -> expectationFailure ("expected one edge, got " <> show (length other))

        it "counts a link no row satisfies among the children it can expand" $ do
            db <- danglingLinkFixture
            map enChildrenCount (rootNodes db) `shouldBe` [1]

    describe "the activities that use a flow" $ do
        it "lists the row that produces it and the row that consumes it" $ do
            db <- twoCoproductFixture
            map prsProcessId (Service.getActivitiesUsingFlow db EitherSide milkId)
                `shouldBe` [pidText milkId, consumerPid]

        it "asked for producers, leaves the consumer out" $ do
            db <- twoCoproductFixture
            map prsProcessId (Service.getActivitiesUsingFlow db ProducersOnly milkId)
                `shouldBe` [pidText milkId]

        it "asked for consumers, leaves the producer out" $ do
            db <- twoCoproductFixture
            map prsProcessId (Service.getActivitiesUsingFlow db ConsumersOnly milkId)
                `shouldBe` [consumerPid]

        it "counts the producers, not everyone who touches the flow" $ do
            db <- twoCoproductFixture
            fmap (Service.producerCount db . TechKind) (M.lookup milkId (dbTechFlows db))
                `shouldBe` Just (Just 1)

        it "counts a treatment activity among the producers of what it treats" $ do
            -- A treatment activity's reference is an input, so a producer test
            -- written on "is this an output" answers no to every one of them
            -- and reports that nothing makes the flow.
            db <- treatmentFixture
            fmap (Service.producerCount db . TechKind) (M.lookup milkId (dbTechFlows db))
                `shouldBe` Just (Just 1)

        it "lists that treatment activity when asked for producers" $ do
            db <- treatmentFixture
            map prsProcessId (Service.getActivitiesUsingFlow db ProducersOnly milkId)
                `shouldBe` [pidText milkId]

        it "counts no producer for a biosphere flow, rather than none at all" $ do
            -- A zero would say "nothing makes it"; Nothing says the question
            -- does not apply to this side of the inventory.
            db <- twoCoproductFixture
            map (Service.producerCount db . BioKind) (M.elems (dbBioFlows db))
                `shouldSatisfy` all (== Nothing)

-- ---------------------------------------------------------------------------
-- Reading one exchange back
-- ---------------------------------------------------------------------------

-- | The target process id of the consumer's single technosphere input.
targetPidOfInput :: Database -> Maybe Text
targetPidOfInput db = do
    (pid, act) <- consumerRow db
    case [ewu | ewu <- pfaExchanges (Service.convertActivityForAPI db pid act), exchangeIsInput (ewuExchange ewu)] of
        [ewu] -> ewuTargetProcessId ewu
        _ -> Nothing

-- | The same, read through 'Service.getActivityInputDetails'.
targetSummaryOfInput :: Database -> Maybe Text
targetSummaryOfInput db = do
    (_, act) <- consumerRow db
    case Service.getActivityInputDetails db act of
        [ed] -> prsProcessId <$> edTargetActivity ed
        _ -> Nothing

-- | Target and waste role of the consumer's single waste output.
wasteLineOf :: Database -> (Maybe Text, Maybe WasteRole)
wasteLineOf db = case consumerRow db of
    Nothing -> (Just "no consumer row", Nothing)
    Just (pid, act) ->
        case [ewu | ewu <- pfaExchanges (Service.convertActivityForAPI db pid act), WasteExchange{waIsInput = False} <- [ewuExchange ewu]] of
            [ewu] -> (ewuTargetProcessId ewu, ewuWasteRole ewu)
            _ -> (Just "no waste output", Nothing)

treeOfConsumer :: Database -> Either Text LoopAwareTree
treeOfConsumer db = case consumerRow db of
    Nothing -> Left "no consumer row"
    Just root -> Right (buildLoopAwareTree defaultUnitConfig db 10 root)

-- | The exported nodes sitting at depth 0.
rootNodes :: Database -> [ExportNode]
rootNodes db = case treeOfConsumer db of
    Left _ -> []
    Right tree ->
        [ n
        | n <- M.elems (teNodes (Service.convertToTreeExport db 10 tree))
        , enDepth n == 0
        ]

-- | Constructor names only, so a shape mismatch reports something readable.
shapeOf :: LoopAwareTree -> [Text]
shapeOf (TreeLeaf _ _) = ["leaf"]
shapeOf (TreeLoop{}) = ["loop"]
shapeOf (TreeMissing{}) = ["missing"]
shapeOf (TreeNode _ _ children) = "node" : concatMap (shapeOf . childSubtree) children

consumerRow :: Database -> Maybe (ProcessId, Activity)
consumerRow db = do
    pid <- findProcessId db consumerActId consumerProdId
    act <- getActivity db pid
    pure (pid, act)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

{- | A supplier written as two coproduct rows, and a consumer whose input names
the one with the lower product UUID.
-}
twoCoproductFixture :: IO Database
twoCoproductFixture = buildFixture (consumerActivity [milkInput] []) supplierRows supplierFlows

{- | A row whose reference exchange is an /input/: the shape of an activity
that treats what it is given rather than selling what it makes. It still
produces its reference flow as far as the matrix is concerned.
-}
treatmentFixture :: IO Database
treatmentFixture =
    buildFixture
        (consumerActivity [milkInput] [])
        (M.singleton (supplierActId, milkId) treatmentProcess)
        (M.singleton milkId (techFlow milkId "milk"))

treatmentProcess :: Activity
treatmentProcess =
    bareActivity
        "milk treatment"
        [(techExchange milkId 1.0 ReferenceProduct (Just supplierActId)){techRole = ReferenceInput}]

{- | The same consumer, its input naming an activity no row in the database
carries.
-}
danglingLinkFixture :: IO Database
danglingLinkFixture = buildFixture (consumerActivity [ghostInput] []) supplierRows supplierFlows

{- | The same, the input stated in grams while the flow is measured in
kilogrammes. Nothing resolves, so nothing carries a reference unit but the
flow, and the edge is labelled with it.
-}
danglingLinkGramsFixture :: IO Database
danglingLinkGramsFixture =
    buildFixture (consumerActivity [ghostInput{techAmount = 2000.0, techUnitId = gUnitId}] []) supplierRows supplierFlows

{- | A consumer whose waste output names a treatment the database does not hold
at that pair. The treatment activity is present, but under another flow, so an
answer built from the activity UUID alone would name it anyway.
-}
untreatedWasteFixture :: IO Database
untreatedWasteFixture =
    buildFixture
        (consumerActivity [] [wasteOutput])
        supplierRows
        supplierFlows

buildFixture :: Activity -> M.Map (UUID, UUID) Activity -> M.Map UUID TechnosphereFlow -> IO Database
buildFixture consumer rows flows = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared)
            SimpleDatabase
                { sdbActivities = M.insert (consumerActId, consumerProdId) consumer rows
                , sdbTechFlows = M.insert consumerProdId (techFlow consumerProdId "cheese") flows
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.singleton scrapId (wasteFlow scrapId "scrap")
                , sdbUnits = unitTable
                }
    either (fail . show) pure r

supplierRows :: M.Map (UUID, UUID) Activity
supplierRows =
    M.fromList
        [ ((supplierActId, milkId), supplierProcess milkId 1.0)
        , ((supplierActId, creamId), supplierProcess creamId 0.3)
        ]

supplierFlows :: M.Map UUID TechnosphereFlow
supplierFlows =
    M.fromList
        [ (milkId, techFlow milkId "milk")
        , (creamId, techFlow creamId "cream")
        ]

{- | The supplier: one activity written as one process per coproduct, each
with its own product as the reference, the way the loader splits a block.
-}
supplierProcess :: UUID -> Double -> Activity
supplierProcess productId amount =
    bareActivity
        "milk production"
        [techExchange productId amount ReferenceProduct (Just supplierActId)]

consumerActivity :: [Exchange] -> [Exchange] -> Activity
consumerActivity inputs outputs =
    bareActivity
        "cheese production"
        ([techExchange consumerProdId 1.0 ReferenceProduct (Just consumerActId)] ++ inputs ++ outputs)

-- | An input naming the supplier's lower-UUID coproduct.
milkInput :: Exchange
milkInput = techExchange milkId 2.0 Input (Just supplierActId)

-- | An input naming an activity the database does not hold.
ghostInput :: Exchange
ghostInput = techExchange milkId 2.0 Input (Just ghostActId)

{- | A waste output naming the supplier: the activity exists, no row of it
produces the waste flow, so nothing routes it.
-}
wasteOutput :: Exchange
wasteOutput =
    WasteExchange
        { waFlowId = scrapId
        , waAmount = 0.5
        , waUnitId = kgUnitId
        , waIsInput = False
        , waActivityLinkId = Just supplierActId
        , waSupplierClaim = ClaimById supplierActId
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

techExchange :: UUID -> Double -> TechRole -> Maybe UUID -> Exchange
techExchange flowId amount role link =
    TechnosphereExchange
        { techFlowId = flowId
        , techAmount = amount
        , techUnitId = kgUnitId
        , techRole = role
        , techActivityLinkId = link
        , techSupplierClaim = maybe ClaimByProduct ClaimById link
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

bareActivity :: Text -> [Exchange] -> Activity
bareActivity name exs =
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
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

techFlow :: UUID -> Text -> TechnosphereFlow
techFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

wasteFlow :: UUID -> Text -> WasteFlow
wasteFlow fid name =
    WasteFlow
        { wfId = fid
        , wfName = name
        , wfUnitId = kgUnitId
        , wfSynonyms = M.empty
        , wfCAS = Nothing
        , wfSubstanceId = Nothing
        }

unitTable :: M.Map UUID Unit
unitTable =
    M.fromList
        [ (kgUnitId, Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""})
        , (gUnitId, Unit{unitId = gUnitId, unitName = "g", unitSymbol = "g", unitComment = ""})
        ]

{- | The default unit table knows no gramme, and a conversion it cannot make
leaves the amount alone, which would hide the very thing the edge test is
about.
-}
gramsAware :: UnitConfig
gramsAware =
    defaultUnitConfig
        { ucUnits = M.insert "g" (unitDef "mass" 0.001) (ucUnits defaultUnitConfig)
        }

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

{- | The supplier's two products are ordered on purpose: 'milkId' is the one a
map keyed on the activity UUID alone drops.
-}
supplierActId, milkId, creamId, consumerActId, consumerProdId, ghostActId, scrapId, kgUnitId, gUnitId :: UUID
supplierActId = mkUUID 1
ghostActId = mkUUID 77
gUnitId = mkUUID 11
milkId = mkUUID 2
creamId = mkUUID 9
consumerActId = mkUUID 3
consumerProdId = mkUUID 4
scrapId = mkUUID 5
kgUnitId = mkUUID 10

-- | Process id text of a supplier coproduct row.
pidText :: UUID -> Text
pidText prodId = UUID.toText supplierActId <> "_" <> UUID.toText prodId

consumerPid :: Text
consumerPid = UUID.toText consumerActId <> "_" <> UUID.toText consumerProdId

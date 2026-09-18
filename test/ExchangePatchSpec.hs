{-# LANGUAGE OverloadedStrings #-}

module ExchangePatchSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database (buildDatabaseWithMatrices)
import Database.Loader (LoadOptions (..), defaultLoadOptions, loadDatabaseWithLocationAliases)
import Database.Patch (ProcessNames (..), applyExchangePatches, describeExchangePatch, exchangeMatches)
import Test.Hspec
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Database (..),
    Exchange (..),
    ExchangePatch (..),
    ExchangePatchMatch (..),
    LocationSource (..),
    PatchOp (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsReference,
    noProperties,
    toSimpleDatabase,
 )
import UnitConversion (defaultUnitConfig)

uuid :: Word -> UUID
uuid n = UUID.fromWords 0 0 0 (fromIntegral n)

kgUnitId :: UUID
kgUnitId = uuid 900

-- | The four flows the fixture exchanges, by the id its exchanges name them with.
creosoteId, pyreneId, dioxideId, trellisProductId, appleProductId :: UUID
creosoteId = uuid 1
pyreneId = uuid 2
dioxideId = uuid 3
trellisProductId = uuid 10
appleProductId = uuid 11

techFlow :: UUID -> Text -> TechnosphereFlow
techFlow flowId name =
    TechnosphereFlow
        { tfId = flowId
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

bioFlow :: UUID -> Text -> BiosphereFlow
bioFlow flowId name =
    BiosphereFlow
        { bfId = flowId
        , bfName = name
        , bfUnitId = kgUnitId
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Nothing
        }

technosphere :: TechRole -> Double -> UUID -> Exchange
technosphere role amount flowId =
    TechnosphereExchange
        { techFlowId = flowId
        , techAmount = amount
        , techUnitId = kgUnitId
        , techRole = role
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

emission :: Double -> UUID -> Exchange
emission amount flowId =
    BiosphereExchange
        { bioFlowId = flowId
        , bioAmount = amount
        , bioUnitId = kgUnitId
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

activity :: Text -> Text -> [Exchange] -> Activity
activity name location exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = location
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

{- | Two processes: a trellis system creosoting its poles, and an orchard
emitting the same substance on its own account.
-}
fixture :: SimpleDatabase
fixture =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [
                    ( (uuid 100, trellisProductId)
                    , activity
                        "trellis system construction"
                        "GLO"
                        [ technosphere ReferenceProduct 1.0 trellisProductId
                        , technosphere Input 78.9 creosoteId
                        , emission 0.09 pyreneId
                        , emission 2.0 dioxideId
                        ]
                    )
                ,
                    ( (uuid 101, appleProductId)
                    , activity
                        "apple production"
                        "FR"
                        [ technosphere ReferenceProduct 1.0 appleProductId
                        , emission 0.5 pyreneId
                        ]
                    )
                ]
        , sdbTechFlows =
            M.fromList
                [ (creosoteId, techFlow creosoteId "Wood preservative, creosote")
                , (trellisProductId, techFlow trellisProductId "Trellis system, wooden poles")
                , (appleProductId, techFlow appleProductId "Apple")
                ]
        , sdbBioFlows =
            M.fromList
                [ (pyreneId, bioFlow pyreneId "Pyrene")
                , (dioxideId, bioFlow dioxideId "Carbon dioxide")
                ]
        , sdbWasteFlows = M.empty
        , sdbUnits = M.empty
        }

emptyMatch :: ExchangePatchMatch
emptyMatch =
    ExchangePatchMatch
        { xpmActivityNameContains = Nothing
        , xpmProductNameContains = Nothing
        , xpmLocation = Nothing
        , xpmFlowName = Nothing
        , xpmFlowNameContains = Nothing
        }

patchOf :: ExchangePatchMatch -> ExchangePatch
patchOf selector = ExchangePatch{xpDescription = Nothing, xpMatch = selector, xpOp = SetValueTo 0}

-- | Every (flow id, amount) the database states, process by process, in fixture order.
amounts :: SimpleDatabase -> [(UUID, Double)]
amounts sdb =
    [ (flowId ex, exchangeAmount ex)
    | act <- M.elems (sdbActivities sdb)
    , ex <- exchanges act
    ]
  where
    flowId :: Exchange -> UUID
    flowId TechnosphereExchange{techFlowId = f} = f
    flowId BiosphereExchange{bioFlowId = f} = f
    flowId WasteExchange{waFlowId = f} = f

spec :: Spec
spec = do
    describe "exchangeMatches" $ do
        let process = ProcessNames{pnActivity = "trellis system construction", pnProduct = "Trellis system, wooden poles", pnLocation = "GLO"}

        it "matches on product name and flow name together (conjunction)" $ do
            let selector = emptyMatch{xpmProductNameContains = Just "wooden poles", xpmFlowNameContains = Just "creosote"}
            exchangeMatches selector process (Just "Wood preservative, creosote") `shouldBe` True

        it "fails when only part of the conjunction matches" $ do
            let selector = emptyMatch{xpmProductNameContains = Just "wooden poles", xpmFlowNameContains = Just "creosote"}
            exchangeMatches selector process (Just "Pyrene") `shouldBe` False

        it "reads two spellings of a name through the substring they share, either case" $ do
            let selector = emptyMatch{xpmProductNameContains = Just "System, Wooden Poles"}
            exchangeMatches selector process (Just "Pyrene") `shouldBe` True
            exchangeMatches selector process{pnProduct = "Treillis system, wooden poles"} (Just "Pyrene") `shouldBe` True

        it "compares a location exactly, where a name is a substring" $ do
            let selector = emptyMatch{xpmLocation = Just "FR"}
            exchangeMatches selector process (Just "Pyrene") `shouldBe` False
            exchangeMatches selector process{pnLocation = "FR"} (Just "Pyrene") `shouldBe` True

        it "matches no flow selector when the database does not name the flow" $ do
            exchangeMatches emptyMatch{xpmFlowName = Just "Pyrene"} process Nothing `shouldBe` False
            exchangeMatches emptyMatch{xpmFlowNameContains = Just "pyr"} process Nothing `shouldBe` False

    describe "applyExchangePatches" $ do
        it "zeroes the matched input of the matched process alone" $ do
            let patch = patchOf emptyMatch{xpmProductNameContains = Just "wooden poles", xpmFlowNameContains = Just "creosote"}
                (patched, stats) = applyExchangePatches [patch] fixture
            map snd stats `shouldBe` [1]
            amounts patched
                `shouldBe` [ (trellisProductId, 1.0)
                           , (creosoteId, 0.0)
                           , (pyreneId, 0.09)
                           , (dioxideId, 2.0)
                           , (appleProductId, 1.0)
                           , (pyreneId, 0.5)
                           ]

        it "leaves the same substance alone in a process the selector does not name" $ do
            let patch = patchOf emptyMatch{xpmProductNameContains = Just "wooden poles", xpmFlowName = Just "Pyrene"}
                (patched, stats) = applyExchangePatches [patch] fixture
            map snd stats `shouldBe` [1]
            lookup pyreneId (reverse (amounts patched)) `shouldBe` Just 0.5

        it "never touches the row saying what the process makes" $ do
            let patch = patchOf emptyMatch{xpmFlowNameContains = Just "Trellis system"}
                (patched, stats) = applyExchangePatches [patch] fixture
            map snd stats `shouldBe` [0]
            amounts patched `shouldBe` amounts fixture

        it "reports the count of a selector that matched nothing" $ do
            let patch = patchOf emptyMatch{xpmFlowName = Just "Acetamiprid"}
                (patched, stats) = applyExchangePatches [patch] fixture
            map snd stats `shouldBe` [0]
            amounts patched `shouldBe` amounts fixture

        it "scales rather than replaces where the patch says so" $ do
            let patch = (patchOf emptyMatch{xpmLocation = Just "FR", xpmFlowName = Just "Pyrene"}){xpOp = ScaleBy 0.6}
                (patched, _) = applyExchangePatches [patch] fixture
            lookup pyreneId (reverse (amounts patched)) `shouldBe` Just 0.3

    describe "a database loaded under patches" $
        it "carries them once, however many times its matrices are built" $ do
            let halveB =
                    ExchangePatch
                        { xpDescription = Nothing
                        , xpMatch = emptyMatch{xpmActivityNameContains = Just "market for product A", xpmFlowName = Just "product B"}
                        , xpOp = ScaleBy 0.5
                        }
                built :: SimpleDatabase -> BuildInputs -> IO Database
                built sdb inputs = either (fail . T.unpack) pure =<< buildDatabaseWithMatrices inputs sdb
            loaded <-
                either (fail . T.unpack) pure
                    =<< loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig){loPatches = [halveB]} "test-data/SAMPLE.min"
            first <- built loaded (BuildInputs defaultUnitConfig M.empty Declared [halveB])
            -- A re-staged database is rebuilt from the activities it was built with.
            second <- built (toSimpleDatabase first) (dbBuiltWith first)
            productBInputs loaded `shouldBe` [0.4]
            productBInputs (toSimpleDatabase second) `shouldBe` [0.4]

    describe "describeExchangePatch" $
        it "renders the selector when the patch carries no description" $ do
            let patch = patchOf emptyMatch{xpmLocation = Just "FR", xpmFlowName = Just "Acetamiprid"}
            describeExchangePatch patch `shouldBe` "location=FR, flow-name=Acetamiprid (set-value 0.0)"

-- | What the market for product A takes of product B, row by row.
productBInputs :: SimpleDatabase -> [Double]
productBInputs sdb =
    [ exchangeAmount ex
    | act <- M.elems (sdbActivities sdb)
    , activityName act == "market for product A"
    , ex <- exchanges act
    , not (exchangeIsReference ex)
    , fmap tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows sdb)) == Just "product B"
    ]

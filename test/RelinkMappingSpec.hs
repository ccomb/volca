{-# LANGUAGE OverloadedStrings #-}

{- | Relink-with-mapping tests.

A small target database supplies "wheat production" at FR and at CH; a
consumer input flow whose name only matches the target *via* an alias row must
link once the alias map is threaded into the 'LinkingContext'. The geo-aware
semantics are pinned here: an exact (name, location) row wins over the
name-only row, a pinned target location bypasses the geography policy, a
missing designated target surfaces as 'AliasTargetMissing' (never a silent
fallback), and a matching row preempts the direct name cascade. So is the file
a mapping page writes: its notes ignored, a row without a target declining
its link, a demand keyed by the supplier process it names, a supplier pinned by
its own identity, and a conversion between units of different measures.
-}
module RelinkMappingSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Database.CrossLinking (
    AliasKey (..),
    AliasMap (..),
    AliasSupplier (..),
    AliasTarget (..),
    CrossDBLinkResult (..),
    IndexedDatabase,
    LinkingContext (..),
    SupplierQuery (..),
    UnitBridge (..),
    buildIndexedDatabase,
    defaultLinkingThreshold,
    emptyAliasMap,
    findSupplierInIndexedDBs,
    locationHierarchy,
    lookupAlias,
 )
import Database.Loader (CrossDBLinkingStats (..), GapEntry (..), GapReason (..), GapReport (..), gapReportForStaged, relinkSimpleDatabase)
import Database.RelinkMapping (
    AliasRow (..),
    buildAliasMap,
    parseAliasCSV,
    parseAliasMap,
 )
import SynonymDB (emptySynonymDB)
import TestHelpers (kgUnit, mkActivity, mkTechFlow, reference, techInput, units)
import Types (
    Activity (..),
    BlockerReason (..),
    CrossDBLink (..),
    Exchange (..),
    GeographyPolicy (..),
    LinkBlocker (..),
    LocationSource (..),
    ProcessRef (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UnresolvedProduct (..),
    noDates,
    noDocumentation,
    noProperties,
    processRefText,
 )
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Target fixture: "wheat production" supplied at FR and at CH, in kg.
-- ---------------------------------------------------------------------------

targetIndexed :: IndexedDatabase
targetIndexed = buildIndexedDatabase "background" emptySynonymDB targetDB

wheatFlow, carFlow :: UUID
wheatFlow = read "aaaaaaaa-0000-0000-0000-000000000001"
carFlow = read "aaaaaaaa-0000-0000-0000-000000000002"

actFR, actCH, actFROrganic, actCar :: UUID
actFR = read "cccccccc-0000-0000-0000-000000000001"
actCH = read "cccccccc-0000-0000-0000-000000000002"
actFROrganic = read "cccccccc-0000-0000-0000-000000000003"
actCar = read "cccccccc-0000-0000-0000-000000000004"

-- | One target activity: its name, location, unit, and the product it makes.
targetActivity :: Text -> Text -> Text -> UUID -> Activity
targetActivity name loc unit flowUUID =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityLocationSource = LocationDeclared
        , activityUnit = unit
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = flowUUID
                , techAmount = 1.0
                , techUnitId = UUID.nil
                , techRole = ReferenceProduct
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
            ]
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

targetDB :: SimpleDatabase
targetDB =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [ ((actFR, wheatFlow), targetActivity "wheat production" "FR" "kg" wheatFlow)
                , ((actCH, wheatFlow), targetActivity "wheat production" "CH" "kg" wheatFlow)
                , ((actCar, carFlow), targetActivity "passenger car production" "CH" "p" carFlow)
                ]
        , sdbTechFlows =
            M.fromList
                [ (wheatFlow, techFlow wheatFlow "wheat production")
                , (carFlow, techFlow carFlow "passenger car")
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.empty
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        }
  where
    techFlow fid name =
        TechnosphereFlow
            { tfId = fid
            , tfName = name
            , tfUnitId = UUID.nil
            , tfSynonyms = M.empty
            , tfCAS = Nothing
            , tfSubstanceId = Nothing
            }

-- | The same product at FR from a second activity, so name and location no longer tell them apart.
twinIndexed :: IndexedDatabase
twinIndexed =
    buildIndexedDatabase "background" emptySynonymDB $
        targetDB
            { sdbActivities =
                M.insert (actFROrganic, wheatFlow) (targetActivity "wheat production, organic" "FR" "kg" wheatFlow) (sdbActivities targetDB)
            }

-- | Linking context against the target, with a geography policy and aliases.
mkCtxPolicy :: GeographyPolicy -> AliasMap -> LinkingContext
mkCtxPolicy policy aliases =
    LinkingContext
        { lcIndexedDatabases = [targetIndexed]
        , lcSynonymDB = emptySynonymDB
        , lcUnitConfig = defaultUnitConfig
        , lcThreshold = defaultLinkingThreshold
        , lcLocationHierarchy = locationHierarchy
        , lcGeographyPolicy = policy
        , lcSupplierAliases = aliases
        }

mkCtx :: AliasMap -> LinkingContext
mkCtx = mkCtxPolicy GeoGlobal

-- | The consumer's differently-named input that only resolves via the alias.
consumerName :: Text
consumerName = "wheat, background name"

named :: Text -> Maybe Text -> AliasTarget
named name loc = AliasTarget (SupplierNamed name loc) Nothing

singletonAlias :: AliasKey -> AliasTarget -> AliasMap
singletonAlias key target = AliasMap M.empty (M.singleton key (target :| []))

aliasMap :: AliasMap
aliasMap = singletonAlias (AliasKey consumerName Nothing) (named "wheat production" Nothing)

-- | A row as a person writes it by hand: a source name and a target.
row :: Text -> Maybe Text -> AliasTarget -> AliasRow
row source loc = AliasRow (Just source) loc Nothing

isLeft' :: Either a b -> Bool
isLeft' = either (const True) (const False)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "alias CSV loading" $ do
        it "parses source/target columns and builds the map" $ do
            -- The source name contains a comma, so it must be CSV-quoted.
            let csv = BLC.pack "source,target\n\"wheat, background name\",wheat production\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            map arSource rows `shouldBe` [Just consumerName]
            buildAliasMap rows `shouldBe` aliasMap

        it "keys the row by source location and pins the target location" $ do
            let csv = BLC.pack "from,to,source_location,target_location\na,b,FR,CH\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            buildAliasMap rows
                `shouldBe` singletonAlias (AliasKey "a" (Just "FR")) (named "b" (Just "CH"))

        it "accepts the same source name with and without a location as two rows" $
            buildAliasMap [row "x" Nothing (named "y" Nothing), row "x" (Just "FR") (named "z" Nothing)]
                `shouldBe` AliasMap
                    M.empty
                    ( M.fromList
                        [ (AliasKey "x" Nothing, named "y" Nothing :| [])
                        , (AliasKey "x" (Just "FR"), named "z" Nothing :| [])
                        ]
                    )

        it "keeps both targets of rows that disagree on a key" $
            buildAliasMap [row "x" Nothing (named "y" Nothing), row "x" Nothing (named "z" Nothing)]
                `shouldBe` AliasMap M.empty (M.singleton (AliasKey "x" Nothing) (named "y" Nothing :| [named "z" Nothing]))

        it "fails when the header names no target column" $
            parseAliasCSV (BLC.pack "source\nfoo\n") `shouldSatisfy` isLeft'

        it "reads a row with a source and no target as a declined link" $
            parseAliasCSV (BLC.pack "source,target\nwheat production,\nbarley,barley production\n")
                `shouldBe` Right [row "wheat production" Nothing (AliasTarget NoSupplier Nothing), row "barley" Nothing (named "barley production" Nothing)]

        it "refuses a target without a source, naming its row" $
            parseAliasCSV (BLC.pack "source,target\n,wheat production\n")
                `shouldSatisfy` either ("row 1" `T.isInfixOf`) (const False)

        it "refuses a file with no row" $
            parseAliasMap (BLC.pack "source,target\n") `shouldSatisfy` isLeft'

        it "collapses two identical duplicate rows harmlessly" $
            buildAliasMap [row "x" Nothing (named "y" Nothing), row "x" Nothing (named "y" Nothing)]
                `shouldBe` singletonAlias (AliasKey "x" Nothing) (named "y" Nothing)

        it "round-trips a quoted target that contains a comma" $ do
            let csv = BLC.pack "source,target\nwheat,\"production, FR\"\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            buildAliasMap rows
                `shouldBe` singletonAlias (AliasKey "wheat" Nothing) (named "production, FR" Nothing)

    describe "a mapping page's file" $ do
        let header = "source_id,source_activity,source_product,source_location,source_unit,target_id,target_activity,target_product,target_location,target_unit,conversion,state,proximity,suggestion_product\n"
            carRef = ProcessRef actCar carFlow
            sourceRef = ProcessRef (read "dddddddd-0000-0000-0000-000000000001") (read "dddddddd-0000-0000-0000-000000000002")
            refText r = UUID.toText (prActivity r) <> "_" <> UUID.toText (prProduct r)

        it "reads the product, the identities and the conversion, and ignores its notes" $ do
            let csv =
                    BLC.pack . T.unpack $
                        header
                            <> refText sourceRef
                            <> ",market for passenger car,\"passenger car, background\",GLO,kg,"
                            <> refText carRef
                            <> ",passenger car production,passenger car,CH,p,1200,confirmed,0.91,\n"
                            <> refText sourceRef
                            <> ",market for passenger car,\"passenger car, other\",GLO,kg,,,,,,,suggested,0.8,passenger car\n"
            parseAliasCSV csv
                `shouldBe` Right
                    [ AliasRow
                        { arSource = Just "passenger car, background"
                        , arSourceLocation = Just "GLO"
                        , arSourceProcess = Just sourceRef
                        , arTarget = AliasTarget (SupplierProcess carRef "passenger car") (Just (UnitBridge "kg" "p" 1200))
                        }
                    , -- A suggestion nobody decided declines the link: its column is a note.
                      AliasRow
                        { arSource = Just "passenger car, other"
                        , arSourceLocation = Just "GLO"
                        , arSourceProcess = Just sourceRef
                        , arTarget = AliasTarget NoSupplier Nothing
                        }
                    ]

        it "refuses a conversion that does not say which way it reads" $
            parseAliasCSV (BLC.pack "source,target,conversion\ncar,passenger car,1200\n") `shouldSatisfy` isLeft'

        it "refuses a conversion that is not a positive number" $ do
            parseAliasCSV (BLC.pack "source,target,conversion,source_unit,target_unit\ncar,passenger car,-3,kg,p\n") `shouldSatisfy` isLeft'
            parseAliasCSV (BLC.pack "source,target,conversion,source_unit,target_unit\ncar,passenger car,heavy,kg,p\n") `shouldSatisfy` isLeft'

        it "refuses an identity that is not one" $
            parseAliasCSV (BLC.pack "source,target_id\ncar,not-an-id\n") `shouldSatisfy` isLeft'

    describe "lookupAlias" $ do
        it "prefers the exact (name, location) row over the name-only row" $ do
            let m = buildAliasMap [row "x" Nothing (named "anywhere" Nothing), row "x" (Just "FR") (named "france" Nothing)]
            lookupAlias m Nothing "x" "FR" `shouldBe` Just (named "france" Nothing :| [])
            lookupAlias m Nothing "x" "DE" `shouldBe` Just (named "anywhere" Nothing :| [])

        it "matches only name-only rows for a demand without a location" $ do
            let m = singletonAlias (AliasKey "x" (Just "FR")) (named "france" Nothing)
            lookupAlias m Nothing "x" "" `shouldBe` Nothing

        it "prefers a row keyed on the process the demand names" $ do
            let ref = ProcessRef actFR wheatFlow
                m =
                    buildAliasMap
                        [ AliasRow Nothing Nothing (Just ref) (named "by identity" Nothing)
                        , row "x" Nothing (named "by name" Nothing)
                        ]
            lookupAlias m (Just ref) "x" "" `shouldBe` Just (named "by identity" Nothing :| [])
            lookupAlias m Nothing "x" "" `shouldBe` Just (named "by name" Nothing :| [])

        it "lets a row on a process that names no supplier decline that process only" $ do
            let market = ProcessRef actFR wheatFlow
                producer = ProcessRef actCar carFlow
                m =
                    buildAliasMap
                        [ AliasRow (Just "x") Nothing (Just market) (named "linked" Nothing)
                        , AliasRow (Just "x") Nothing (Just producer) (AliasTarget NoSupplier Nothing)
                        ]
            lookupAlias m Nothing "x" "" `shouldBe` Just (named "linked" Nothing :| [])
            lookupAlias m (Just producer) "x" "" `shouldBe` Just (AliasTarget NoSupplier Nothing :| [])

    describe "findSupplierInIndexedDBs with supplier aliases" $ do
        it "does NOT link the aliased consumer name without the alias map" $
            case findSupplierInIndexedDBs (mkCtx emptyAliasMap) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked NoNameMatch -> pure ()
                CrossDBNotLinked other -> expectationFailure $ "Expected NoNameMatch, got: " ++ show other
                CrossDBLinked{} -> expectationFailure "Expected no link without alias map"

        it "links the aliased consumer name to the target via the alias map" $
            case findSupplierInIndexedDBs (mkCtx aliasMap) (aliasQuery consumerName "FR" "kg") of
                CrossDBLinked{cdlrProductName = name, cdlrDatabaseName = db} -> do
                    name `shouldBe` "wheat production"
                    db `shouldBe` "background"
                CrossDBNotLinked reason -> expectationFailure $ "Expected link, got: " ++ show reason

        it "scores name(50) + exact-location(30) = 80 for an FR→FR alias link" $
            case findSupplierInIndexedDBs (mkCtx aliasMap) (aliasQuery consumerName "FR" "kg") of
                CrossDBLinked{cdlrScore = score, cdlrLocation = loc} -> do
                    score `shouldBe` 80
                    loc `shouldBe` "FR"
                CrossDBNotLinked reason -> expectationFailure $ "Expected link, got: " ++ show reason

        it "surfaces a unit mismatch as UnitIncompatible, never a silent drop" $
            case findSupplierInIndexedDBs (mkCtx aliasMap) (aliasQuery consumerName "FR" "m3") of
                CrossDBNotLinked UnitIncompatible{uiQueryUnit = req, uiSupplierUnit = got} -> do
                    req `shouldBe` "m3"
                    got `shouldBe` "kg"
                other -> expectationFailure $ "Expected UnitIncompatible, got: " ++ describe' other

        it "resolves a name without an alias row independently of the mapping" $
            -- The target's own canonical name resolves directly; a mapping
            -- must never change the behaviour of names it doesn't mention.
            case findSupplierInIndexedDBs (mkCtx aliasMap) (aliasQuery "wheat production" "FR" "kg") of
                CrossDBLinked{cdlrScore = score} -> score `shouldBe` 80
                CrossDBNotLinked reason -> expectationFailure $ "Expected direct link, got: " ++ show reason

        it "lets a matching row preempt a direct name match" $ do
            -- "wheat production" matches the target directly at FR, but the
            -- curator redirected it to CH: the row must win, otherwise a
            -- curated answer could be silently overridden.
            let redirect = singletonAlias (AliasKey "wheat production" Nothing) (named "wheat production" (Just "CH"))
            case findSupplierInIndexedDBs (mkCtx redirect) (aliasQuery "wheat production" "FR" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "CH"
                CrossDBNotLinked reason -> expectationFailure $ "Expected CH link, got: " ++ show reason

        it "links to a pinned target location even when the geography policy forbids it" $ do
            -- GeoExact would reject CH for an FR demand; the curator's pinned
            -- location is a deliberate designation, so it bypasses the policy.
            let pinned = singletonAlias (AliasKey consumerName Nothing) (named "wheat production" (Just "CH"))
            case findSupplierInIndexedDBs (mkCtxPolicy GeoExact pinned) (aliasQuery consumerName "FR" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "CH"
                CrossDBNotLinked reason -> expectationFailure $ "Expected CH link, got: " ++ show reason

        it "reports a designated target name that matches nowhere" $ do
            let missing = singletonAlias (AliasKey consumerName Nothing) (named "no such product" (Just "CH"))
            case findSupplierInIndexedDBs (mkCtx missing) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name loc) -> do
                    name `shouldBe` "no such product"
                    loc `shouldBe` Nothing
                other -> expectationFailure $ "Expected AliasTargetMissing, got a different result: " ++ describe' other

        it "reports a designated target that exists but not at the pinned location" $ do
            let wrongLoc = singletonAlias (AliasKey consumerName Nothing) (named "wheat production" (Just "DE"))
            case findSupplierInIndexedDBs (mkCtx wrongLoc) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name loc) -> do
                    name `shouldBe` "wheat production"
                    loc `shouldBe` Just "DE"
                other -> expectationFailure $ "Expected AliasTargetMissing, got a different result: " ++ describe' other

        it "applies the located row only at its location" $ do
            -- The FR row designates a target nobody ships, so an FR demand
            -- fails loudly through the row – while a CH demand has no row in
            -- force and links normally through the cascade.
            let m = singletonAlias (AliasKey "wheat production" (Just "FR")) (named "no such product" Nothing)
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery "wheat production" "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name _) -> name `shouldBe` "no such product"
                other -> expectationFailure $ "Expected AliasTargetMissing, got a different result: " ++ describe' other
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery "wheat production" "CH" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "CH"
                CrossDBNotLinked reason -> expectationFailure $ "Expected CH link, got: " ++ show reason

        it "links the activity a target identity names, where name and location tie" $ do
            let pinned = singletonAlias (AliasKey consumerName Nothing) (AliasTarget (SupplierProcess (ProcessRef actFROrganic wheatFlow) "organic") Nothing)
                ctx = (mkCtx pinned){lcIndexedDatabases = [twinIndexed]}
            case findSupplierInIndexedDBs ctx (aliasQuery consumerName "FR" "kg") of
                CrossDBLinked{cdlrActivityUUID = act} -> act `shouldBe` actFROrganic
                CrossDBNotLinked reason -> expectationFailure $ "Expected the organic activity, got: " ++ show reason

        it "reports a target identity no dependency ships" $ do
            let pinned = singletonAlias (AliasKey consumerName Nothing) (AliasTarget (SupplierProcess (ProcessRef actCH carFlow) "gone") Nothing)
            case findSupplierInIndexedDBs (mkCtx pinned) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name Nothing) -> name `shouldBe` "gone"
                other -> expectationFailure $ "Expected AliasTargetMissing, got: " ++ describe' other

        it "tells apart rows that disagree on two processes of one product" $ do
            let organic = ProcessRef actFROrganic wheatFlow
                plain = ProcessRef actFR wheatFlow
                m =
                    buildAliasMap
                        [ row consumerName Nothing (AliasTarget (SupplierProcess organic "wheat production") Nothing)
                        , row consumerName Nothing (AliasTarget (SupplierProcess plain "wheat production") Nothing)
                        ]
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasConflict targets) ->
                    targets `shouldBe` ["wheat production (" <> processRefText organic <> ")", "wheat production (" <> processRefText plain <> ")"]
                other -> expectationFailure $ "Expected AliasConflict, got: " ++ describe' other

        it "reports rows that disagree instead of choosing, and applies the others" $ do
            let m =
                    buildAliasMap
                        [ row consumerName Nothing (named "wheat production" (Just "FR"))
                        , row consumerName Nothing (named "wheat production" (Just "CH"))
                        , row "barley, background name" Nothing (named "wheat production" Nothing)
                        ]
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasConflict targets) -> targets `shouldBe` ["wheat production @ FR", "wheat production @ CH"]
                other -> expectationFailure $ "Expected AliasConflict, got: " ++ describe' other
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery "barley, background name" "FR" "kg") of
                CrossDBLinked{} -> pure ()
                CrossDBNotLinked reason -> expectationFailure $ "Expected the other row to link, got: " ++ show reason

    describe "a conversion between units of different measures" $ do
        let carRow bridge = singletonAlias (AliasKey "passenger car, background" Nothing) (AliasTarget (SupplierNamed "passenger car" Nothing) bridge)
            kgToP = Just (UnitBridge "kg" "p" 1200)

        it "lets a kg demand reach a supplier counted in p through the row" $
            case findSupplierInIndexedDBs (mkCtx (carRow kgToP)) (aliasQuery "passenger car, background" "CH" "kg") of
                CrossDBLinked{cdlrBridge = bridge} -> bridge `shouldBe` kgToP
                CrossDBNotLinked reason -> expectationFailure $ "Expected a link, got: " ++ show reason

        it "refuses the same demand when the row has no conversion" $
            case findSupplierInIndexedDBs (mkCtx (carRow Nothing)) (aliasQuery "passenger car, background" "CH" "kg") of
                CrossDBNotLinked UnitIncompatible{} -> pure ()
                other -> expectationFailure $ "Expected UnitIncompatible, got: " ++ describe' other

        it "ignores the conversion where the units meet without it" $ do
            let wheatRow = singletonAlias (AliasKey consumerName Nothing) (AliasTarget (SupplierNamed "wheat production" Nothing) (Just (UnitBridge "kg" "t" 500)))
            case findSupplierInIndexedDBs (mkCtx wheatRow) (aliasQuery consumerName "FR" "kg") of
                CrossDBLinked{cdlrBridge = bridge} -> bridge `shouldBe` Nothing
                CrossDBNotLinked reason -> expectationFailure $ "Expected a link, got: " ++ show reason

        it "asks the supplier for 2 p where the consumer used 2400 kg" $ do
            let links = cdlLinks (relinkCar (carRow kgToP) ClaimByProduct)
            map (\l -> (cdlCoefficient l, cdlExchangeUnit l)) links `shouldBe` [(2.0, "p")]

        it "keys an EcoSpold 2 input with no location on the process it names" $ do
            -- The input names its supplier by identity and carries no
            -- location, as an EcoSpold 2 input does: a row keyed on that
            -- identity applies, where a row keyed on name and place could not.
            let byIdentity =
                    buildAliasMap
                        [AliasRow Nothing Nothing (Just (ProcessRef oldCarAct oldCarFlow)) (AliasTarget (SupplierNamed "passenger car" Nothing) kgToP)]
            map (\l -> (cdlCoefficient l, cdlExchangeUnit l)) (cdlLinks (relinkCar byIdentity (ClaimById oldCarAct)))
                `shouldBe` [(2.0, "p")]

        it "reports a row's failure for an input that names its supplier by identity" $ do
            let refused =
                    buildAliasMap
                        [AliasRow Nothing Nothing (Just (ProcessRef oldCarAct oldCarFlow)) (AliasTarget (SupplierNamed "passenger car" Nothing) Nothing)]
            M.elems (M.map (M.keys . upBlockers) (cdlUnresolvedRequests (relinkCar refused (ClaimById oldCarAct))))
                `shouldSatisfy` any (any isUnitIncompatible)
            -- The gap report gives that refusal, not a dangling identity.
            map geReason (grGaps (gapReportForStaged "consumer" (carConsumer (ClaimById oldCarAct)) (relinkCar refused (ClaimById oldCarAct))))
                `shouldBe` [GapBlocked (BlockerReason "unit_incompatible" (Just "kg vs p") :| [])]

        it "lets a row keyed on the process an input names speak before that process" $ do
            -- The dependency still ships the process the input names: the
            -- row, the curator's word on that process, wins over it.
            let moved =
                    buildAliasMap
                        [AliasRow Nothing Nothing (Just (ProcessRef actFR wheatFlow)) (AliasTarget (SupplierProcess (ProcessRef actFROrganic wheatFlow) "organic") Nothing)]
            map cdlSupplierActUUID (cdlLinks (relinkInput [twinIndexed] (wheatConsumer actFR) moved))
                `shouldBe` [actFROrganic]
            map cdlSupplierActUUID (cdlLinks (relinkInput [twinIndexed] (wheatConsumer actFR) emptyAliasMap))
                `shouldBe` [actFR]

        it "keeps unlinked an input whose row names no supplier, and says why" $ do
            -- The dependency ships the very process the input names; the row
            -- declines it all the same.
            let declined = buildAliasMap [AliasRow Nothing Nothing (Just (ProcessRef actFR wheatFlow)) (AliasTarget NoSupplier Nothing)]
                stats = relinkInput [twinIndexed] (wheatConsumer actFR) declined
            cdlLinks stats `shouldBe` []
            map geReason (grGaps (gapReportForStaged "consumer" (wheatConsumer actFR) stats))
                `shouldBe` [GapBlocked (BlockerReason "alias_declined" Nothing :| [])]
  where
    describe' result = case result of
        CrossDBLinked{cdlrLocation = loc} -> "linked @ " ++ show loc
        CrossDBNotLinked reason -> show reason
    isUnitIncompatible blocker = case blocker of
        UnitIncompatible{} -> True
        _ -> False

-- | A demand naming no supplier activity: product, location, unit.
aliasQuery :: Text -> Text -> Text -> SupplierQuery
aliasQuery p loc u =
    SupplierQuery{sqProductName = p, sqSupplierActivity = Nothing, sqLocation = loc, sqUnit = u, sqClaimedProcess = Nothing}

oldCarAct, oldCarFlow, consumerAct, consumerFlow :: UUID
oldCarAct = read "eeeeeeee-0000-0000-0000-000000000001"
oldCarFlow = read "eeeeeeee-0000-0000-0000-000000000002"
consumerAct = read "eeeeeeee-0000-0000-0000-000000000003"
consumerFlow = read "eeeeeeee-0000-0000-0000-000000000004"

-- | Relink a consumer against some dependencies.
relinkInput :: [IndexedDatabase] -> SimpleDatabase -> AliasMap -> CrossDBLinkingStats
relinkInput deps consumer aliases =
    relinkSimpleDatabase deps emptySynonymDB defaultUnitConfig M.empty GeoGlobal aliases consumer

-- | Relink the consumer of 'carConsumer' against the target.
relinkCar :: AliasMap -> SupplierClaim -> CrossDBLinkingStats
relinkCar aliases claim = relinkInput [targetIndexed] (carConsumer claim) aliases

{- | A consumer that takes 2400 kg of "passenger car, background", at CH, the
input naming its supplier as the claim says.
-}
carConsumer :: SupplierClaim -> SimpleDatabase
carConsumer claim =
    consumerTaking
        oldCarFlow
        "passenger car, background"
        (techInput oldCarFlow 2400)
            { techLocation = if claim == ClaimByProduct then "CH" else ""
            , techActivityLinkId = case claim of
                ClaimById act -> Just act
                _ -> Nothing
            , techSupplierClaim = claim
            }

-- | A consumer taking 1 kg of "wheat production" from the activity it names by identity, as EcoSpold 2 writes it.
wheatConsumer :: UUID -> SimpleDatabase
wheatConsumer act =
    consumerTaking wheatFlow "wheat production" (techInput wheatFlow 1){techActivityLinkId = Just act, techSupplierClaim = ClaimById act}

-- | A consumer whose one activity takes one input of the named flow, in kg.
consumerTaking :: UUID -> Text -> Exchange -> SimpleDatabase
consumerTaking flow name input =
    SimpleDatabase
        { sdbActivities = M.singleton (consumerAct, consumerFlow) (mkActivity "driving" [reference consumerFlow, input])
        , sdbTechFlows =
            M.fromList
                [ (consumerFlow, mkTechFlow consumerFlow "driving")
                , (flow, (mkTechFlow flow name){tfUnitId = kgUnit})
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = units
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        }

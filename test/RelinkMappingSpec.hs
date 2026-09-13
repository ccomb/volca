{-# LANGUAGE OverloadedStrings #-}

{- | Relink-with-mapping tests.

A small target database supplies "wheat production" at FR and at CH; a
consumer input flow whose name only matches the target *via* an alias row must
link once the alias map is threaded into the 'LinkingContext'. The geo-aware
semantics are pinned here: an exact (name, location) row wins over the
name-only row, a pinned target location bypasses the geography policy, a
missing designated target surfaces as 'AliasTargetMissing' (never a silent
fallback), and a matching row preempts the direct name cascade.
-}
module RelinkMappingSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

import Database.CrossLinking (
    AliasKey (..),
    AliasMap (..),
    AliasTarget (..),
    CrossDBLinkResult (..),
    IndexedDatabase,
    LinkingContext (..),
    SupplierQuery (..),
    buildIndexedDatabase,
    defaultLinkingThreshold,
    emptyAliasMap,
    findSupplierInIndexedDBs,
    locationHierarchy,
    lookupAlias,
 )
import Database.RelinkMapping (
    AliasRow (..),
    buildAliasMap,
    loadAliasMap,
    parseAliasCSV,
    rejectEmpty,
 )
import SynonymDB (emptySynonymDB)
import Types (
    Activity (..),
    Exchange (..),
    GeographyPolicy (..),
    LinkBlocker (..),
    LocationSource (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Target fixture: "wheat production" supplied at FR and at CH, in kg.
-- ---------------------------------------------------------------------------

targetIndexed :: IndexedDatabase
targetIndexed = buildIndexedDatabase "background" emptySynonymDB targetDB

targetDB :: SimpleDatabase
targetDB =
    let flowUUID = read "aaaaaaaa-0000-0000-0000-000000000001"
        mkAct loc =
            Activity
                { activityName = "wheat production"
                , activityDescription = []
                , activityDocumentation = []
                , activitySynonyms = M.empty
                , activityClassification = M.empty
                , activityLocation = loc
                , activityLocationSource = LocationDeclared
                , activityUnit = "kg"
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
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                    ]
                , activityParams = M.empty
                , activityParamExprs = M.empty
                , activityNativeType = Nothing
                , activityNativeId = Nothing
                , activityFormulaCheck = Nothing
                }
        flow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = "wheat production"
                , tfUnitId = UUID.nil
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
        actFR = read "cccccccc-0000-0000-0000-000000000001"
        actCH = read "cccccccc-0000-0000-0000-000000000002"
     in SimpleDatabase
            { sdbActivities =
                M.fromList
                    [ ((actFR, flowUUID), mkAct "FR")
                    , ((actCH, flowUUID), mkAct "CH")
                    ]
            , sdbTechFlows = M.singleton flowUUID flow
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = M.empty
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

singletonAlias :: AliasKey -> AliasTarget -> AliasMap
singletonAlias key target = AliasMap (M.singleton key target)

aliasMap :: AliasMap
aliasMap = singletonAlias (AliasKey consumerName Nothing) (AliasTarget "wheat production" Nothing)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "alias CSV loading" $ do
        it "parses source/target columns and builds the map" $ do
            -- The source name contains a comma, so it must be CSV-quoted.
            let csv = BLC.pack "source,target\n\"wheat, background name\",wheat production\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            map arSource rows `shouldBe` [consumerName]
            map arTarget rows `shouldBe` ["wheat production"]
            buildAliasMap rows `shouldBe` Right aliasMap

        it "keys the row by source location and pins the target location" $ do
            let csv = BLC.pack "from,to,source_location,target_location\na,b,FR,CH\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            buildAliasMap rows
                `shouldBe` Right (singletonAlias (AliasKey "a" (Just "FR")) (AliasTarget "b" (Just "CH")))

        it "accepts the same source name with and without a location as two rows" $ do
            let rows =
                    [ AliasRow "x" "y" Nothing Nothing
                    , AliasRow "x" "z" (Just "FR") Nothing
                    ]
            buildAliasMap rows
                `shouldBe` Right
                    ( AliasMap $
                        M.fromList
                            [ (AliasKey "x" Nothing, AliasTarget "y" Nothing)
                            , (AliasKey "x" (Just "FR"), AliasTarget "z" Nothing)
                            ]
                    )

        it "rejects a conflicting alias for the same (source, location) key" $ do
            let rows =
                    [ AliasRow "x" "y" Nothing Nothing
                    , AliasRow "x" "z" Nothing Nothing
                    ]
            buildAliasMap rows `shouldSatisfy` either (const True) (const False)

        it "fails when a required column is missing" $ do
            let csv = BLC.pack "source\nfoo\n"
            parseAliasCSV csv `shouldSatisfy` either (const True) (const False)

        it "rejects a present-but-blank target as a half-specified row" $ do
            -- Source filled, target blank: a curation mistake, not a no-op,
            -- so it must fail loudly rather than be silently dropped.
            let csv = BLC.pack "source,target\nwheat production,\n"
            parseAliasCSV csv `shouldSatisfy` either (const True) (const False)

        it "rejects a header-only file as having no usable rows" $
            withSystemTempFile "relink-empty.csv" $ \path h -> do
                BLC.hPut h (BLC.pack "source,target\n")
                hClose h
                result <- loadAliasMap path
                result `shouldSatisfy` either (const True) (const False)

        it "rejectEmpty rejects an empty map and passes a non-empty one" $ do
            -- The HTTP relink handler relies on this so a header-only CSV is a 4xx,
            -- not a silent 200 no-op (matching the CLI's loadAliasMap).
            rejectEmpty emptyAliasMap `shouldSatisfy` either (const True) (const False)
            rejectEmpty aliasMap `shouldBe` Right aliasMap

        it "collapses two identical duplicate rows harmlessly" $ do
            let rows =
                    [ AliasRow "x" "y" Nothing Nothing
                    , AliasRow "x" "y" Nothing Nothing
                    ]
            buildAliasMap rows `shouldBe` Right (singletonAlias (AliasKey "x" Nothing) (AliasTarget "y" Nothing))

        it "round-trips a quoted target that contains a comma" $ do
            -- The target name contains a comma, so it must be CSV-quoted.
            let csv = BLC.pack "source,target\nwheat,\"production, FR\"\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            buildAliasMap rows
                `shouldBe` Right (singletonAlias (AliasKey "wheat" Nothing) (AliasTarget "production, FR" Nothing))

    describe "lookupAlias" $ do
        it "prefers the exact (name, location) row over the name-only row" $ do
            let m =
                    AliasMap $
                        M.fromList
                            [ (AliasKey "x" Nothing, AliasTarget "anywhere" Nothing)
                            , (AliasKey "x" (Just "FR"), AliasTarget "france" Nothing)
                            ]
            lookupAlias m "x" "FR" `shouldBe` Just (AliasTarget "france" Nothing)
            lookupAlias m "x" "DE" `shouldBe` Just (AliasTarget "anywhere" Nothing)

        it "matches only name-only rows for a demand without a location" $ do
            let m = singletonAlias (AliasKey "x" (Just "FR")) (AliasTarget "france" Nothing)
            lookupAlias m "x" "" `shouldBe` Nothing

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
                CrossDBNotLinked other -> expectationFailure $ "Expected UnitIncompatible, got: " ++ show other
                CrossDBLinked{} -> expectationFailure "Expected unit mismatch to block the link"

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
            let redirect = singletonAlias (AliasKey "wheat production" Nothing) (AliasTarget "wheat production" (Just "CH"))
            case findSupplierInIndexedDBs (mkCtx redirect) (aliasQuery "wheat production" "FR" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "CH"
                CrossDBNotLinked reason -> expectationFailure $ "Expected CH link, got: " ++ show reason

        it "links to a pinned target location even when the geography policy forbids it" $ do
            -- GeoExact would reject CH for an FR demand; the curator's pinned
            -- location is a deliberate designation, so it bypasses the policy.
            let pinned = singletonAlias (AliasKey consumerName Nothing) (AliasTarget "wheat production" (Just "CH"))
            case findSupplierInIndexedDBs (mkCtxPolicy GeoExact pinned) (aliasQuery consumerName "FR" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "CH"
                CrossDBNotLinked reason -> expectationFailure $ "Expected CH link, got: " ++ show reason

        it "reports a designated target name that matches nowhere" $ do
            let missing = singletonAlias (AliasKey consumerName Nothing) (AliasTarget "no such product" (Just "CH"))
            case findSupplierInIndexedDBs (mkCtx missing) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name loc) -> do
                    name `shouldBe` "no such product"
                    loc `shouldBe` Nothing
                other -> expectationFailure $ "Expected AliasTargetMissing, got a different result: " ++ describe' other

        it "reports a designated target that exists but not at the pinned location" $ do
            let wrongLoc = singletonAlias (AliasKey consumerName Nothing) (AliasTarget "wheat production" (Just "DE"))
            case findSupplierInIndexedDBs (mkCtx wrongLoc) (aliasQuery consumerName "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name loc) -> do
                    name `shouldBe` "wheat production"
                    loc `shouldBe` Just "DE"
                other -> expectationFailure $ "Expected AliasTargetMissing, got a different result: " ++ describe' other

        it "applies the located row only at its location" $ do
            -- The FR row designates a target nobody ships, so an FR demand
            -- fails loudly through the row - while a CH demand has no row in
            -- force and links normally through the cascade.
            let m = singletonAlias (AliasKey "wheat production" (Just "FR")) (AliasTarget "no such product" Nothing)
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery "wheat production" "FR" "kg") of
                CrossDBNotLinked (AliasTargetMissing name _) -> name `shouldBe` "no such product"
                other -> expectationFailure $ "Expected AliasTargetMissing, got a different result: " ++ describe' other
            case findSupplierInIndexedDBs (mkCtx m) (aliasQuery "wheat production" "CH" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "CH"
                CrossDBNotLinked reason -> expectationFailure $ "Expected CH link, got: " ++ show reason
  where
    describe' result = case result of
        CrossDBLinked{cdlrLocation = loc} -> "linked @ " ++ show loc
        CrossDBNotLinked reason -> show reason

-- | A demand naming no supplier activity: product, location, unit.
aliasQuery :: Text -> Text -> Text -> SupplierQuery
aliasQuery p loc u =
    SupplierQuery{sqProductName = p, sqSupplierActivity = Nothing, sqLocation = loc, sqUnit = u}

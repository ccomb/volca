{-# LANGUAGE OverloadedStrings #-}

{- | Pure-function tests for "API.MCP.Enrich".

These helpers do all the JSON munging that turns the serialized
'LCIABatchResult' / 'BatchImpactsResponse' into the shape the MCP
client sees: @web_url@ deep links at every level, and (optionally) a
restricted subset of the configured scoring sets. They're easy to
regress on a JSON-shape change, so each branch is covered here
independently of the live server.
-}
module MCPEnrichSpec (spec) where

import API.MCP.Enrich (
    addWebUrl,
    addWebUrlMaybe,
    attachMarketHintByName,
    encodeSegment,
    filterScoringSets,
    groupsDiagramQuery,
    groupsPagePath,
    groupsQuery,
    isMarketActivityName,
    scoreActivityWebUrl,
    slimLCIAPanel,
    summarizeLCIAPanel,
    webUrlField,
 )
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import API.Types (FunctionalUnit (..), GroupClassification (..), GroupFilterSpec (..), GroupFlowSpec (..), GroupSpec (..), GroupsRequest (..))
import Database.Manager (CollectionName (..))
import Types (Medium (..))

butter :: Value
butter = toJSON (FunctionalUnit "kg" "butter")

-- ---------------------------------------------------------------------------
-- Fixtures
--
-- These mirror the camelCase JSON shapes that 'strippedToJSON' emits for
-- LCIABatchResult / BatchImpactsEntry, not the snake_case names that
-- 'list_scoring_sets' uses for its own projection. Stay aligned with
-- those producers or these tests stop catching real regressions.
-- ---------------------------------------------------------------------------

{- | Minimal LCIABatchResult-shaped Value: one method entry, three
scoring sets, scoringResults missing one of them (simulating a
set whose formula failed to evaluate).
-}
sampleLBR :: Value
sampleLBR =
    object
        [ "results"
            .= [ object
                    [ "methodId" .= ("uuid-method-1" :: Text)
                    , "score" .= (1.23 :: Double)
                    ]
               ]
        , "scoringResults"
            .= object
                [ "PEF" .= object ["score" .= (1.0 :: Double)]
                , "ECS" .= object ["score" .= (2.0 :: Double)]
                ]
        , "scoringUnits"
            .= object
                [ "PEF" .= ("Pt" :: Text)
                , "ECS" .= ("Pt" :: Text)
                , "FAILED" .= ("Pt" :: Text)
                ]
        , "scoringIndicators"
            .= object
                [ "PEF" .= object []
                , "ECS" .= object []
                , "FAILED" .= object []
                ]
        ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

{- | A group whose values hold what an address must escape, a classification
code's start and a classification left out, then a group by name.
-}
twoGroups :: [GroupSpec]
twoGroups =
    [ GroupSpec
        "Packaging & films"
        GroupFilterSpec
            { gfsName = Nothing
            , gfsProduct = Just "film"
            , gfsLocation = Nothing
            , gfsClassifications = Just [GroupClassification "ISIC rev.4" "2220:plastics/rubber" "starts-with", GroupClassification "CPC" "36" "not-exact"]
            , gfsFlow = Nothing
            }
    , GroupSpec "Transport" (GroupFilterSpec (Just "transport") Nothing (Just "FR") Nothing Nothing)
    ]

spec :: Spec
spec = do
    describe "groupsQuery" $ do
        it "spells the reading, then each group at its rank, every value escaped" $
            groupsQuery (GroupsRequest (Just "own") twoGroups)
                `shouldBe` "?reading=own&g1=Packaging%20%26%20films&g1-product=film&g1-classification=ISIC%20rev.4&g1-classification-value=2220%3Aplastics%2Frubber&g1-classification-mode=starts-with&g1-classification=CPC&g1-classification-value=36&g1-classification-mode=not-exact&g2=Transport&g2-name=transport&g2-location=FR"
        it "leaves the whole chain reading out" $
            groupsQuery (GroupsRequest (Just "first-match") (drop 1 twoGroups))
                `shouldBe` "?g1=Transport&g1-name=transport&g1-location=FR"
        it "says an empty list, which is not the ready-made set" $
            groupsQuery (GroupsRequest Nothing []) `shouldBe` "?groups=none"
        it "spells the substance a group keeps, and the own-emissions reading it is read in" $ do
            let fields = GroupSpec "N2O of the fields" (GroupFilterSpec (Just "at farm") Nothing Nothing Nothing (Just (GroupFlowSpec (Just "nitrous") (Just "contains") (Just Air))))
                n2o = GroupSpec "N2O" (GroupFilterSpec Nothing Nothing Nothing Nothing (Just (GroupFlowSpec (Just "N2O") (Just "substance") Nothing)))
                water = GroupSpec "Water" (GroupFilterSpec Nothing Nothing Nothing Nothing (Just (GroupFlowSpec Nothing Nothing (Just NaturalResource))))
            groupsQuery (GroupsRequest Nothing [fields, n2o, water])
                `shouldBe` "?reading=own&g1=N2O%20of%20the%20fields&g1-name=at%20farm&g1-flow-name=nitrous&g1-flow-match=contains&g1-flow-medium=air&g2=N2O&g2-flow-name=N2O&g3=Water&g3-flow-medium=natural%20resource"
    describe "groupsDiagramQuery" $
        it "opens the same split on the diagram" $
            groupsDiagramQuery (GroupsRequest (Just "first-match") (drop 1 twoGroups))
                `shouldBe` "?g1=Transport&g1-name=transport&g1-location=FR&display=sankey"
    describe "groupsPagePath" $
        it "escapes the collection like the other pages" $
            groupsPagePath "agb" "p1" (CollectionName "EF 3.1") `shouldBe` "/db/agb/activity/p1/groups/EF%203.1"
    describe "scoreActivityWebUrl" $ do
        it "encodes every dynamic segment (no raw slashes leak through)" $
            scoreActivityWebUrl (Just "https://volca.run") "db/with-slash" "pid/with-slash" "EF 3.1"
                `shouldBe` Just "https://volca.run/db/db%2Fwith-slash/activity/pid%2Fwith-slash/impacts/EF%203.1"

        it "round-trips ASCII-safe segments unchanged" $
            scoreActivityWebUrl (Just "https://x") "agribalyse" "abc_def" "EF31"
                `shouldBe` Just "https://x/db/agribalyse/activity/abc_def/impacts/EF31"

        it "yields Nothing when no base URL is configured (no frontend)" $
            scoreActivityWebUrl Nothing "agribalyse" "abc_def" "EF31" `shouldBe` Nothing

    describe "encodeSegment" $
        it "percent-encodes unsafe URL characters" $ do
            encodeSegment "a/b" `shouldBe` "a%2Fb"
            encodeSegment "a b" `shouldBe` "a%20b"

    describe "addWebUrl" $ do
        it "inserts web_url into an Object" $
            addWebUrl "https://x" (object ["a" .= (1 :: Int)])
                `shouldBe` object ["a" .= (1 :: Int), "web_url" .= ("https://x" :: Text)]

        it "passes Array through unchanged" $
            addWebUrl "https://x" (Array (V.fromList [Null])) `shouldBe` Array (V.fromList [Null])

        it "passes String/Number/Bool/Null through unchanged" $ do
            addWebUrl "https://x" (String "s") `shouldBe` String "s"
            addWebUrl "https://x" (Number 42) `shouldBe` Number 42
            addWebUrl "https://x" (Bool True) `shouldBe` Bool True
            addWebUrl "https://x" Null `shouldBe` Null

    describe "addWebUrlMaybe" $ do
        it "inserts web_url when Just" $
            addWebUrlMaybe (Just "https://x") (object ["a" .= (1 :: Int)])
                `shouldBe` object ["a" .= (1 :: Int), "web_url" .= ("https://x" :: Text)]

        it "is a no-op when Nothing" $
            addWebUrlMaybe Nothing (object ["a" .= (1 :: Int)])
                `shouldBe` object ["a" .= (1 :: Int)]

    describe "webUrlField" $ do
        it "yields a single web_url pair when a base URL is configured" $
            object (webUrlField (Just "https://x") "/db/foo/activity/bar/impacts/EF31")
                `shouldBe` object ["web_url" .= ("https://x/db/foo/activity/bar/impacts/EF31" :: Text)]

        it "yields an empty pair list when no base URL is configured" $
            webUrlField Nothing "/anything" `shouldBe` []

    describe "slimLCIAPanel" $ do
        let panelWithFnUnit =
                object
                    [ "functionalUnit" .= butter
                    , "results"
                        .= [ object
                                [ "methodId" .= ("uuid-method-1" :: Text)
                                , "functionalUnit" .= butter
                                , "web_url" .= ("https://x/should-be-dropped" :: Text)
                                , "score" .= (1.0 :: Double)
                                ]
                           , object
                                [ "methodId" .= ("uuid-method-2" :: Text)
                                , "functionalUnit" .= butter
                                , "web_url" .= ("https://x/should-be-dropped-too" :: Text)
                                , "score" .= (2.0 :: Double)
                                ]
                           ]
                    , "scoringResults" .= object []
                    ]

        it "keeps the panel's functionalUnit at the top level" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
            KM.lookup (fromText "functionalUnit") km `shouldBe` Just butter

        it "drops functionalUnit from every entry in results" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
                Just (Array rs) = KM.lookup (fromText "results") km
            forM_ (V.toList rs) $ \(Object e) ->
                KM.lookup (fromText "functionalUnit") e `shouldBe` Nothing

        it "drops web_url from every entry in results" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
                Just (Array rs) = KM.lookup (fromText "results") km
            forM_ (V.toList rs) $ \(Object e) ->
                KM.lookup (fromText "web_url") e `shouldBe` Nothing

        it "preserves the other top-level fields (scoringResults, etc.)" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
            KM.lookup (fromText "scoringResults") km `shouldBe` Just (object [])

        it "is a no-op on a panel without results" $
            slimLCIAPanel (object ["scoringResults" .= object []])
                `shouldBe` object ["scoringResults" .= object []]

        it "handles a panel with an empty results array" $ do
            let v = object ["results" .= ([] :: [Value]), "scoringResults" .= object []]
                Object km = slimLCIAPanel v
            KM.lookup (fromText "functionalUnit") km `shouldBe` Nothing

        it "leaves entries without a functionalUnit untouched apart from web_url" $ do
            let v = object ["results" .= [object ["score" .= (1 :: Int)]]]
                Object km = slimLCIAPanel v
            KM.lookup (fromText "functionalUnit") km `shouldBe` Nothing

    describe "summarizeLCIAPanel" $ do
        let panel =
                object
                    [ "functionalUnit" .= butter
                    , "results"
                        .= [ object
                                [ "methodId" .= ("m1" :: Text)
                                , "functionalUnit" .= butter
                                , "score" .= (1.0 :: Double)
                                ]
                           , object
                                [ "methodId" .= ("m2" :: Text)
                                , "functionalUnit" .= butter
                                , "score" .= (2.0 :: Double)
                                ]
                           ]
                    , "scoringResults" .= object ["PEF" .= object ["score" .= (3.0 :: Double)]]
                    , "scoringIndicators" .= object []
                    ]

        it "drops the results array entirely" $ do
            let Object km = summarizeLCIAPanel panel
            KM.lookup (fromText "results") km `shouldBe` Nothing

        it "keeps the panel's functionalUnit at the top level" $ do
            let Object km = summarizeLCIAPanel panel
            KM.lookup (fromText "functionalUnit") km `shouldBe` Just butter

        it "preserves scoringResults and scoringIndicators" $ do
            let Object km = summarizeLCIAPanel panel
            KM.lookup (fromText "scoringResults") km
                `shouldBe` Just (object ["PEF" .= object ["score" .= (3.0 :: Double)]])
            KM.lookup (fromText "scoringIndicators") km
                `shouldBe` Just (object [])

        it "handles an empty results array" $ do
            let v = object ["results" .= ([] :: [Value]), "scoringResults" .= object []]
                Object km = summarizeLCIAPanel v
            KM.lookup (fromText "functionalUnit") km `shouldBe` Nothing
            KM.lookup (fromText "results") km `shouldBe` Nothing

    describe "filterScoringSets" $ do
        it "is a no-op when requested is empty" $
            filterScoringSets ["PEF", "ECS"] [] sampleLBR `shouldBe` Right sampleLBR

        it "accepts configured names that are missing from scoringResults" $ do
            -- 'FAILED' is configured (in scoringUnits / scoringIndicators) but
            -- did not produce a score; requesting it must succeed.
            case filterScoringSets ["PEF", "ECS", "FAILED"] ["FAILED"] sampleLBR of
                Right (Object km) -> do
                    KM.lookup (fromText "scoringResults") km
                        `shouldBe` Just (object [])
                    KM.lookup (fromText "scoringUnits") km
                        `shouldBe` Just (object ["FAILED" .= ("Pt" :: Text)])
                Right v -> expectationFailure ("expected Object, got " <> show v)
                Left e -> expectationFailure ("unexpected Left: " <> show e)

        it "rejects names not in configured" $
            filterScoringSets ["PEF", "ECS"] ["typo"] sampleLBR
                `shouldBe` Left "Unknown scoring set(s): typo. Configured on this method collection: PEF, ECS"

        it "restricts all three scoring maps to the requested keys" $ do
            case filterScoringSets ["PEF", "ECS", "FAILED"] ["PEF"] sampleLBR of
                Right (Object km) -> do
                    KM.lookup (fromText "scoringResults") km
                        `shouldBe` Just (object ["PEF" .= object ["score" .= (1.0 :: Double)]])
                    KM.lookup (fromText "scoringUnits") km
                        `shouldBe` Just (object ["PEF" .= ("Pt" :: Text)])
                    KM.lookup (fromText "scoringIndicators") km
                        `shouldBe` Just (object ["PEF" .= object []])
                _ -> expectationFailure "expected an Object"

        it "restricts the rows and the category shares as well" $ do
            let withSplits = case sampleLBR of
                    Object km ->
                        Object
                            ( km
                                <> KM.fromList
                                    [ (fromText "scoringRows", object ["PEF" .= object [], "ECS" .= object []])
                                    , (fromText "scoringShares", object ["PEF" .= object [], "ECS" .= object []])
                                    ]
                            )
                    other -> other
            case filterScoringSets ["PEF", "ECS"] ["PEF"] withSplits of
                Right (Object km) -> do
                    KM.lookup (fromText "scoringRows") km `shouldBe` Just (object ["PEF" .= object []])
                    KM.lookup (fromText "scoringShares") km `shouldBe` Just (object ["PEF" .= object []])
                _ -> expectationFailure "expected an Object"

    describe "PR #79 – slimLCIAPanel NaN-score edge case" $ do
        let panelWithNaNScore =
                object
                    [ "functionalUnit" .= butter
                    , "results"
                        .= [ object
                                [ "methodId" .= ("m-nan" :: Text)
                                , "functionalUnit" .= butter
                                , "score" .= (0 / 0 :: Double)
                                , "web_url" .= ("https://x/drop" :: Text)
                                ]
                           ]
                    , "scoringResults" .= object []
                    ]

        it "slimLCIAPanel leaves a NaN score alone (only drops web_url and the per-entry fnUnit)" $ do
            let Object km = slimLCIAPanel panelWithNaNScore
            -- the panel's functionalUnit stays, the entry has no web_url anymore.
            KM.lookup (fromText "functionalUnit") km `shouldBe` Just butter
            case KM.lookup (fromText "results") km of
                Just (Array rs) -> case V.toList rs of
                    [Object e] -> do
                        KM.lookup (fromText "web_url") e `shouldBe` Nothing
                        KM.lookup (fromText "functionalUnit") e `shouldBe` Nothing
                        -- score remains a JSON value (Aeson encodes NaN as Null).
                        KM.member (fromText "score") e `shouldBe` True
                    _ -> expectationFailure "expected one entry"
                _ -> expectationFailure "expected results array"

    describe "isMarketActivityName" $ do
        it "matches the canonical ecoinvent prefix" $
            isMarketActivityName "market for sawlog and veneer log, softwood, ..." `shouldBe` True

        it "is case-insensitive" $
            isMarketActivityName "MARKET FOR something" `shouldBe` True

        it "tolerates leading whitespace" $
            isMarketActivityName "   market for X" `shouldBe` True

        it "rejects producer activities" $ do
            isMarketActivityName "hardwood forestry, oak, sustainable forest management" `shouldBe` False
            isMarketActivityName "marketing services" `shouldBe` False

    describe "attachMarketHintByName" $ do
        it "adds hint to a market activity" $ do
            let Object km =
                    attachMarketHintByName
                        "market for X"
                        (object ["name" .= ("market for X" :: Text)])
            case KM.lookup (fromText "hint") km of
                Just (Object hk) ->
                    KM.lookup (fromText "kind") hk `shouldBe` Just (String "market_activity")
                other ->
                    expectationFailure ("expected hint object, got " <> show other)

        it "is a no-op on a producer activity" $
            attachMarketHintByName
                "softwood forestry, spruce"
                (object ["name" .= ("softwood forestry, spruce" :: Text)])
                `shouldBe` object ["name" .= ("softwood forestry, spruce" :: Text)]

        it "passes non-objects through unchanged" $ do
            attachMarketHintByName "market for X" (String "s") `shouldBe` String "s"
            attachMarketHintByName "market for X" Null `shouldBe` Null

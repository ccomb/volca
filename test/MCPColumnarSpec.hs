{-# LANGUAGE OverloadedStrings #-}

{- | Pure-function tests for "API.MCP.Columnar".

The columnar projection is the heart of the @score_activities@ MCP
tool – it decides which keys land at the top level, which columns each
row carries, and how the dominant indicator is shaped. Every branch is
covered here so a wire-shape regression fails fast, independently of
the live server.
-}
module MCPColumnarSpec (spec) where

import API.MCP.Columnar (dominantIndicatorCell, resolveSingleScoringSet, toColumnarBatch)
import API.Types (
    BatchImpactsEntry (..),
    BatchImpactsResponse (..),
    LCIABatchResult (..),
    LCIAResult (..),
    ScoringIndicator (..),
 )
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Method.Types (ScoringSet (..))
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Minimal PEF-shaped set: two primitive indicators, one score named "total".
pefSet :: ScoringSet
pefSet =
    ScoringSet
        { ssName = "PEF"
        , ssUnit = "µPts PEF"
        , ssVariables = M.fromList [("acd", "Acidification"), ("cch", "Climate change")]
        , ssComputed = M.empty
        , ssLabels = M.empty
        , ssNormalization = M.empty
        , ssWeighting = M.empty
        , ssScores = M.fromList [("total", "acd + cch")]
        , ssDisplayMultiplier = Nothing
        }

ecsSet :: ScoringSet
ecsSet = pefSet{ssName = "ECS"}

lciaResult :: Text -> LCIAResult
lciaResult fu =
    LCIAResult
        { lrMethodId = UUID.nil
        , lrMethodName = "m"
        , lrCategory = "cch"
        , lrDamageCategory = "cch"
        , lrScore = 1.0
        , lrUnit = "kg CO2 eq"
        , lrNormalizedScore = Nothing
        , lrWeightedScore = Nothing
        , lrMappedFlows = 0
        , lrFunctionalUnit = fu
        , lrTopContributors = []
        }

-- | Build an entry against a single scoring set, with explicit total + indicators.
mkEntry :: Text -> Text -> Text -> Text -> Double -> [(Text, Double)] -> BatchImpactsEntry
mkEntry pid name fu setName total inds =
    BatchImpactsEntry
        { bieProcessId = pid
        , bieActivityName = name
        , bieImpacts =
            LCIABatchResult
                { lbrResults = [lciaResult fu]
                , lbrSingleScore = Just total
                , lbrSingleScoreUnit = Just "µPts PEF"
                , lbrNormWeightSetName = Nothing
                , lbrAvailableNWsets = []
                , lbrScoringResults = M.singleton setName (M.singleton "total" total)
                , lbrScoringUnits = M.singleton setName "µPts PEF"
                , lbrScoringIndicators =
                    M.singleton setName $
                        M.fromList [(k, ScoringIndicator k v) | (k, v) <- inds]
                , lbrCutoffWaste = []
                }
        }

emptyBir :: BatchImpactsResponse
emptyBir = BatchImpactsResponse{birResults = [], birNotFound = [], birInvalid = [], birUnscorable = []}

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "resolveSingleScoringSet" $ do
        it "returns the only configured set when caller passes none" $
            resolveSingleScoringSet [] [pefSet] `shouldBe` Right pefSet

        it "errors when nothing is configured" $
            resolveSingleScoringSet [] [] `shouldBe` Left "No scoring sets configured on this collection."

        it "errors when multiple are configured and caller picks none" $
            case resolveSingleScoringSet [] [pefSet, ecsSet] of
                Left msg -> T.isPrefixOf "Multiple scoring sets are configured (PEF, ECS)" msg `shouldBe` True
                Right _ -> expectationFailure "expected Left"

        it "accepts the configured set the caller named" $
            resolveSingleScoringSet ["PEF"] [pefSet, ecsSet] `shouldBe` Right pefSet

        it "errors when the requested name is unknown, listing what's configured" $
            resolveSingleScoringSet ["typo"] [pefSet, ecsSet]
                `shouldBe` Left "Unknown scoring set: typo. Configured on this collection: PEF, ECS"

        it "errors when caller passes more than one name" $
            case resolveSingleScoringSet ["PEF", "ECS"] [pefSet, ecsSet] of
                Left msg -> T.isPrefixOf "score_activities accepts at most one scoring set" msg `shouldBe` True
                Right _ -> expectationFailure "expected Left"

    describe "toColumnarBatch (homogeneous FU)" $ do
        let bir =
                BatchImpactsResponse
                    { birResults =
                        [ mkEntry "pidA" "oak forestry, RoW" "1 m³ wood" "PEF" 10.0 [("acd", 4.0), ("cch", 6.0)]
                        , mkEntry "pidB" "spruce forestry, RoW" "1 m³ wood" "PEF" 20.0 [("acd", 5.0), ("cch", 15.0)]
                        ]
                    , birNotFound = []
                    , birInvalid = []
                    , birUnscorable = []
                    }
            Object km = toColumnarBatch False (Just "https://x") "ei" "EF31" pefSet bir

        it "uses snake_case top-level keys throughout" $ do
            KM.lookup (fromText "scoring_set") km `shouldBe` Just (String "PEF")
            KM.lookup (fromText "scoring_unit") km `shouldBe` Just (String "µPts PEF")
            KM.lookup (fromText "not_found") km `shouldBe` Just (Array (V.fromList []))
            KM.lookup (fromText "invalid") km `shouldBe` Just (Array (V.fromList []))

        it "hoists functional_unit to top level when all rows share one" $
            KM.lookup (fromText "functional_unit") km `shouldBe` Just (String "1 m³ wood")

        it "emits snake_case fixed columns followed by the sorted indicator keys" $
            KM.lookup (fromText "columns") km
                `shouldBe` Just
                    ( Array
                        ( V.fromList
                            [ String "activity_name"
                            , String "process_id"
                            , String "web_url"
                            , String "total"
                            , String "acd"
                            , String "cch"
                            ]
                        )
                    )

        it "packs each row as a flat array of scalars (no per-row functional_unit cell)" $ do
            let pidARow =
                    Array $
                        V.fromList
                            [ String "oak forestry, RoW"
                            , String "pidA"
                            , String "https://x/db/ei/activity/pidA/impacts/EF31"
                            , Number 10.0
                            , Number 4.0
                            , Number 6.0
                            ]
                pidBRow =
                    Array $
                        V.fromList
                            [ String "spruce forestry, RoW"
                            , String "pidB"
                            , String "https://x/db/ei/activity/pidB/impacts/EF31"
                            , Number 20.0
                            , Number 5.0
                            , Number 15.0
                            ]
            case KM.lookup (fromText "rows") km of
                Just (Array rs) -> V.toList rs `shouldBe` [pidARow, pidBRow]
                other -> expectationFailure ("expected rows array, got " <> show other)

    describe "toColumnarBatch (heterogeneous FU – the silent-misrepresentation fix)" $ do
        let bir =
                BatchImpactsResponse
                    { birResults =
                        [ mkEntry "pidM" "raw milk, FR" "1 kg of milk" "PEF" 1.0 []
                        , mkEntry "pidS" "beef steak, FR" "1 kg of steak" "PEF" 30.0 []
                        ]
                    , birNotFound = []
                    , birInvalid = []
                    , birUnscorable = []
                    }
            Object km = toColumnarBatch False (Just "https://x") "agri" "EF31" pefSet bir

        it "drops the top-level functional_unit field when rows disagree" $
            KM.lookup (fromText "functional_unit") km `shouldBe` Nothing

        it "inserts functional_unit as a per-row column instead" $
            KM.lookup (fromText "columns") km
                `shouldBe` Just
                    ( Array
                        ( V.fromList
                            [ String "activity_name"
                            , String "process_id"
                            , String "web_url"
                            , String "functional_unit"
                            , String "total"
                            , String "acd"
                            , String "cch"
                            ]
                        )
                    )

        it "carries each row's actual functional unit, not the first row's" $ do
            case KM.lookup (fromText "rows") km of
                Just (Array rs) -> case V.toList rs of
                    [Array a, Array b] -> do
                        V.toList a !! 3 `shouldBe` String "1 kg of milk"
                        V.toList b !! 3 `shouldBe` String "1 kg of steak"
                    other -> expectationFailure ("expected two rows, got " <> show other)
                other -> expectationFailure ("expected rows array, got " <> show other)

    describe "toColumnarBatch (summary_only)" $ do
        let bir =
                BatchImpactsResponse
                    { birResults =
                        [ mkEntry "pidA" "oak" "1 m³ wood" "PEF" 10.0 [("acd", 2.0), ("cch", 8.0)]
                        ]
                    , birNotFound = []
                    , birInvalid = []
                    , birUnscorable = []
                    }
            Object km = toColumnarBatch True (Just "https://x") "ei" "EF31" pefSet bir

        it "replaces per-indicator columns with a single dominant_indicator column" $
            KM.lookup (fromText "columns") km
                `shouldBe` Just
                    ( Array
                        ( V.fromList
                            [ String "activity_name"
                            , String "process_id"
                            , String "web_url"
                            , String "total"
                            , String "dominant_indicator"
                            ]
                        )
                    )

        it "fills the cell with a {key, label, share_pct} object, not a delimited string" $ do
            case KM.lookup (fromText "rows") km of
                Just (Array rs) -> case V.toList rs of
                    [Array a] ->
                        last (V.toList a)
                            `shouldBe` object ["key" .= ("cch" :: Text), "label" .= ("cch" :: Text), "share_pct" .= (80.0 :: Double)]
                    other -> expectationFailure ("expected one row, got " <> show other)
                other -> expectationFailure ("expected rows array, got " <> show other)

    describe "toColumnarBatch (no frontend bundled – Nothing baseUrl)" $ do
        let bir =
                BatchImpactsResponse
                    { birResults =
                        [ mkEntry "pidA" "oak forestry, RoW" "1 m³ wood" "PEF" 10.0 [("acd", 4.0), ("cch", 6.0)]
                        ]
                    , birNotFound = []
                    , birInvalid = []
                    , birUnscorable = []
                    }
            Object km = toColumnarBatch False Nothing "ei" "EF31" pefSet bir

        it "drops the web_url column from the header" $
            KM.lookup (fromText "columns") km
                `shouldBe` Just
                    ( Array
                        ( V.fromList
                            [ String "activity_name"
                            , String "process_id"
                            , String "total"
                            , String "acd"
                            , String "cch"
                            ]
                        )
                    )

        it "drops the web_url cell from each row (no dead-link slot)" $
            case KM.lookup (fromText "rows") km of
                Just (Array rs) ->
                    V.toList rs
                        `shouldBe` [ Array $
                                        V.fromList
                                            [ String "oak forestry, RoW"
                                            , String "pidA"
                                            , Number 10.0
                                            , Number 4.0
                                            , Number 6.0
                                            ]
                                   ]
                other -> expectationFailure ("expected rows array, got " <> show other)

    describe "toColumnarBatch (edge: empty results)" $ do
        let Object km = toColumnarBatch False (Just "https://x") "ei" "EF31" pefSet emptyBir

        it "omits top-level functional_unit (nothing to hoist)" $
            KM.lookup (fromText "functional_unit") km `shouldBe` Nothing

        it "still emits scoring_set / scoring_unit / empty rows" $ do
            KM.lookup (fromText "scoring_set") km `shouldBe` Just (String "PEF")
            KM.lookup (fromText "scoring_unit") km `shouldBe` Just (String "µPts PEF")
            KM.lookup (fromText "rows") km `shouldBe` Just (Array (V.fromList []))

    describe "dominantIndicatorCell" $ do
        -- The indicator's siCategory is its display name – it rides along as
        -- 'label' so clients never have to show the raw variable key.
        let inds = M.fromList [("a", ScoringIndicator "a" 1.0), ("b", ScoringIndicator "Big impact" 9.0)]

        it "picks the indicator with the largest absolute share, labelled with its display name" $
            dominantIndicatorCell (Just 10.0) inds
                `shouldBe` object ["key" .= ("b" :: Text), "label" .= ("Big impact" :: Text), "share_pct" .= (90.0 :: Double)]

        it "uses absolute value so negative contributors can win" $
            dominantIndicatorCell
                (Just 10.0)
                (M.fromList [("a", ScoringIndicator "a" 1.0), ("b", ScoringIndicator "Big impact" (-9.0))])
                `shouldBe` object ["key" .= ("b" :: Text), "label" .= ("Big impact" :: Text), "share_pct" .= (90.0 :: Double)]

        it "returns Null when the total is zero (share undefined)" $
            dominantIndicatorCell (Just 0.0) inds `shouldBe` Null

        it "returns Null when the total is missing" $
            dominantIndicatorCell Nothing inds `shouldBe` Null

        it "returns Null when the indicator map is empty" $
            dominantIndicatorCell (Just 10.0) M.empty `shouldBe` Null

{-# LANGUAGE OverloadedStrings #-}

{- | Does the resource registry describe the routes that actually exist?

"API.Resources" calls itself the single source of truth for every operation's
name, path and parameters, and "API.OpenApi" stamps those onto the published
spec by matching on names. When a name does not match, the enrichment leaves
the parameter alone and says nothing, so a description can be written, shipped
and read by nobody with every test green.

This is the test that says otherwise. It has no server and no database: the
spec is a pure value.
-}
module ResourcesDriftSpec (spec) where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Data.OpenApi (OpenApi, Operation, PathItem, Referenced (..))
import qualified Data.OpenApi.Lens as OA
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import qualified API.Resources as R
import API.Routes (volcaOpenApi)

{- | Every operation the published spec carries, by its operationId, paired
with every parameter name it accepts.

servant-openapi3 hangs both query parameters and path captures on the
operation - its @addParam@ writes @allOperations . parameters@ - and never
fills a path item's own parameter list, so reading the operation reads all of
them.
-}
operationsById :: OpenApi -> [(Text, [Text])]
operationsById api =
    [ (opId, [p ^. OA.name | Inline p <- op ^. OA.parameters])
    | item <- toList (api ^. OA.paths)
    , op <- operationsOf item
    , Just opId <- [op ^. OA.operationId]
    ]

operationsOf :: PathItem -> [Operation]
operationsOf item =
    mapMaybe
        (item ^.)
        [OA.get, OA.put, OA.post, OA.delete, OA.patch, OA.head_, OA.options]

-- | A registry parameter whose name no parameter of its own route carries.
type Divergence = (Text, Text)

{- | Every registry parameter whose name its own route does not carry.

Ninety of them, which is nearly all of them: 'API.OpenApi.enrichParameters'
matches on the name, so each of these is a description that reaches the
published spec for nobody. Three separate causes, none of them a typo:

  * __Path captures are named by Servant.__ The registry says @database@ and
    @process_id@; the route says @dbName@ and @processId@. Every capture on
    every route diverges.
  * __Body fields are not parameters at all.__ @substitutions@, @remove@,
    @set_amounts@ and the rest travel in a @ReqBody@, which has no
    @parameters@ entry to enrich. The registry has no way to say so - a
    'R.Param' records a name and a JSON-Schema type, not where the value rides.
  * __Query parameters disagree on spelling.__ @exclude_long_term@ against
    @exclude-long-term@, @query@ against @q@, @max_depth@ against @max-depth@.

Pinned rather than fixed: the first two want the registry to record what kind
of parameter each is, and the third changes the query parameters clients send.
Both are their own decisions. Pinned so the list can only shrink, and so the
next name that drifts is one this test names rather than one nobody counts.
-}
knownDivergences :: [Divergence]
knownDivergences =
    [ ("aggregate", "database")
    , ("aggregate", "process_id")
    , ("compute_sensitivity", "database")
    , ("compute_sensitivity", "method_id")
    , ("compute_sensitivity", "perturbations")
    , ("compute_sensitivity", "process_id")
    , ("count_search_matches", "database")
    , ("count_search_matches", "query")
    , ("derive_database", "database")
    , ("derive_database", "new_name")
    , ("edit_exchanges", "add_biosphere")
    , ("edit_exchanges", "add_inputs")
    , ("edit_exchanges", "add_waste_outputs")
    , ("edit_exchanges", "database")
    , ("edit_exchanges", "process_id")
    , ("edit_exchanges", "remove")
    , ("edit_exchanges", "set_amounts")
    , ("explain_cf", "collection")
    , ("explain_cf", "database")
    , ("explain_cf", "flow_id")
    , ("explain_cf", "method_id")
    , ("get_activity", "database")
    , ("get_activity", "exchange_type")
    , ("get_activity", "flow")
    , ("get_activity", "is_input")
    , ("get_activity", "process_id")
    , ("get_characterization", "collection")
    , ("get_characterization", "database")
    , ("get_characterization", "method_id")
    , ("get_characterization_coverage", "database")
    , ("get_computed_quality_report", "database")
    , ("get_consumers", "classification_value")
    , ("get_consumers", "database")
    , ("get_consumers", "include_edges")
    , ("get_consumers", "max_depth")
    , ("get_consumers", "process_id")
    , ("get_contributing_activities", "database")
    , ("get_contributing_activities", "exclude_long_term")
    , ("get_contributing_activities", "method_id")
    , ("get_contributing_activities", "process_id")
    , ("get_contributing_flows", "database")
    , ("get_contributing_flows", "exclude_long_term")
    , ("get_contributing_flows", "include_diagnostics")
    , ("get_contributing_flows", "method_id")
    , ("get_contributing_flows", "process_id")
    , ("get_flow_mapping", "collection")
    , ("get_flow_mapping", "database")
    , ("get_flow_mapping", "max_unmatched")
    , ("get_flow_mapping", "method_id")
    , ("get_flow_mapping", "process_id")
    , ("get_flow_mapping", "verbose")
    , ("get_gap_report", "database")
    , ("get_impacts", "database")
    , ("get_impacts", "exclude_long_term")
    , ("get_impacts", "include_diagnostics")
    , ("get_impacts", "method_id")
    , ("get_impacts", "process_id")
    , ("get_impacts", "substitutions")
    , ("get_impacts", "top_flows")
    , ("get_inventory", "database")
    , ("get_inventory", "flow")
    , ("get_inventory", "limit")
    , ("get_inventory", "process_id")
    , ("get_inventory", "substitutions")
    , ("get_path_to", "database")
    , ("get_path_to", "process_id")
    , ("get_quality_report", "database")
    , ("get_supply_chain", "classification_match")
    , ("get_supply_chain", "classification_value")
    , ("get_supply_chain", "database")
    , ("get_supply_chain", "max_depth")
    , ("get_supply_chain", "min_quantity")
    , ("get_supply_chain", "process_id")
    , ("get_supply_chain", "substitutions")
    , ("list_classifications", "database")
    , ("list_classifications", "filter")
    , ("list_classifications", "system")
    , ("load_database", "database")
    , ("score_activities", "database")
    , ("score_activities", "exclude_long_term")
    , ("score_activities", "process_ids")
    , ("score_activities", "scoring_sets")
    , ("score_activities", "summary_only")
    , ("score_activity", "database")
    , ("score_activity", "exclude_long_term")
    , ("score_activity", "process_id")
    , ("score_activity", "scoring_sets")
    , ("score_activity", "substitutions")
    , ("search_activities", "classification_match")
    , ("search_activities", "classification_value")
    , ("search_activities", "database")
    , ("search_flows", "database")
    , ("search_flows", "query")
    , ("unload_database", "database")
    ]

spec :: Spec
spec = describe "API.Resources against the published spec" $ do
    let spec' = volcaOpenApi
        byId = operationsById spec'
        withPath = [r | r <- R.allResources, Just _ <- [R.apiPath r]]

    it "publishes an operation for every resource that names a route" $
        let missing = [R.mcpName r | r <- withPath, R.mcpName r `notElem` map fst byId]
         in missing `shouldBe` []

    -- The registry's whole claim. A description stamped onto a name the route
    -- does not carry is a description nobody will ever read.
    -- Reported as two differences rather than one equality: a wall of ninety
    -- tuples does not say which one moved. The first list is a name that has
    -- started diverging, the second one that has stopped.
    it "describes only parameters the routes accept, beyond the pinned list" $ do
        let divergences =
                [ (R.mcpName r, R.paramName p)
                | r <- withPath
                , Just accepted <- [lookup (R.mcpName r) byId]
                , p <- R.params r
                , R.paramName p `notElem` accepted
                ]
        (divergences \\ knownDivergences) `shouldBe` []
        (knownDivergences \\ divergences) `shouldBe` []

    -- paramDesc is what the enrichment carries; an empty one reaches the
    -- published spec as a parameter with no explanation at all.
    it "gives every parameter of a published operation a description" $
        let blank =
                [ (R.mcpName r, R.paramName p)
                | r <- withPath
                , p <- R.params r
                , T.null (T.strip (R.paramDesc p))
                ]
         in blank `shouldBe` []

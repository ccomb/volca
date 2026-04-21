{-# LANGUAGE OverloadedStrings #-}

{- | Canonical enumeration of user-facing VoLCA operations.

This module is the single source of truth for naming and metadata across
all VoLCA user surfaces: MCP tools, CLI subcommands, pyvolca Python client,
and OpenAPI documentation. Each surface consumes this module via a
projection function ('mcpName', 'cliName', 'description', 'params').

Per-language naming conventions are respected: projections emit the form
natural to their target (snake_case for MCP/Python, kebab-case for CLI/URLs).
The Haskell call sites use 'Resource' PascalCase constructors directly.

Adding a new operation means extending the 'Resource' ADT and adding one
equation to each projection function. The compiler catches missing cases
for 'mcpName'/'cliName'/'description'/'params'. A separate drift test
(ResourcesDriftSpec) enforces that each 'Resource' corresponds to an
actual Servant route in 'API.Routes.LCAAPI'.
-}
module API.Resources (
    Resource (..),
    Param (..),
    ParamKind (..),
    allResources,
    mcpName,
    cliName,
    description,
    params,
    requiredParams,
    optionalParams,
    apiPath,
    apiPathText,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Method (StdMethod (..))

{- | Every operation VoLCA exposes through its user-facing surfaces.

Note: this covers the *queryable* operations. Infrastructure endpoints
(database load/unload, method-collection management, auth, version) are
not represented here — they live in Routes.hs only, because they have
no analyst-facing equivalent across all surfaces.
-}
data Resource
    = ListDatabases
    | ListPresets
    | SearchActivities
    | SearchFlows
    | GetActivity
    | Aggregate
    | GetSupplyChain
    | GetInventory
    | GetImpacts
    | ListMethods
    | GetFlowMapping
    | GetCharacterization
    | GetContributingFlows
    | GetContributingActivities
    | ListGeographies
    | ListClassifications
    | GetPathTo
    | GetConsumers
    deriving (Eq, Ord, Show, Bounded, Enum)

-- | Whether a parameter must be supplied by the caller.
data ParamKind = Required | Optional
    deriving (Eq, Show)

{- | A single parameter accepted by a resource operation.

'paramType' uses JSON Schema type names ("string", "integer", "number",
"boolean", "array") so the MCP tool schema can emit it directly. The
CLI and OpenAPI projections use it too.
-}
data Param = Param
    { paramName :: Text
    , paramType :: Text
    , paramKind :: ParamKind
    , paramDesc :: Text
    }
    deriving (Eq, Show)

-- | All resources, in declaration order.
allResources :: [Resource]
allResources = [minBound .. maxBound]

-- | Filter to just the required parameters of a resource.
requiredParams :: Resource -> [Param]
requiredParams = filter (\p -> paramKind p == Required) . params

-- | Filter to just the optional parameters of a resource.
optionalParams :: Resource -> [Param]
optionalParams = filter (\p -> paramKind p == Optional) . params

-- ---------------------------------------------------------------------------
-- Projection: canonical HTTP route (primary GET)
-- ---------------------------------------------------------------------------

{- | The canonical HTTP route that this resource operation corresponds to,
as a @(method, pathSegments)@ pair. Path segments use @{name}@ for
captured parameters (matching what OpenAPI/Servant emit in path templates).

Returns 'Nothing' for resources that are MCP-only (i.e. don't have an
equivalent HTTP endpoint). pyvolca cannot call these via the runtime
dispatcher; they're reachable only through the MCP server.

For resources where multiple HTTP routes implement the same operation
(e.g. @get_impacts@ has GET and POST variants), this returns the
primary GET. The runtime dispatcher in pyvolca upgrades to POST when a
@substitutions@ body parameter is supplied.

The 'API.OpenApi' enrichment step uses this to stamp @operationId@,
@summary@, and parameter descriptions onto the auto-generated OpenAPI
spec. A build-time drift test (ResourcesDriftSpec) asserts that every
'Just' result matches a real Servant route in 'API.Routes.LCAAPI'.
-}
apiPath :: Resource -> Maybe (StdMethod, [Text])
apiPath r = case r of
    ListDatabases -> Just (GET, ["db"])
    ListPresets -> Just (GET, ["classification-presets"])
    SearchActivities -> Just (GET, ["db", "{dbName}", "activities"])
    SearchFlows -> Just (GET, ["db", "{dbName}", "flows"])
    GetActivity -> Just (GET, ["db", "{dbName}", "activity", "{processId}"])
    Aggregate -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "aggregate"])
    GetSupplyChain -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "supply-chain"])
    GetInventory -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "inventory"])
    GetImpacts -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "impacts", "{collection}", "{methodId}"])
    ListMethods -> Just (GET, ["methods"])
    GetFlowMapping -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "flow-mapping"])
    GetCharacterization -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "characterization"])
    GetContributingFlows -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-flows", "{collection}", "{methodId}"])
    GetContributingActivities -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-activities", "{collection}", "{methodId}"])
    ListGeographies -> Nothing -- MCP-only: synthesizes geography list from in-memory database, no HTTP route
    ListClassifications -> Just (GET, ["db", "{dbName}", "classifications"])
    GetPathTo -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "path-to"])
    GetConsumers -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "consumers"])

{- | The full OpenAPI path template for a resource, e.g.
@"/api/v1/db/{dbName}/activity/{processId}/impacts/{collection}/{methodId}"@.
Returns 'Nothing' for MCP-only resources.
-}
apiPathText :: Resource -> Maybe (StdMethod, Text)
apiPathText r = do
    (m, segs) <- apiPath r
    pure (m, "/api/v1/" <> T.intercalate "/" segs)

-- ---------------------------------------------------------------------------
-- Projection: MCP tool names (snake_case, JSON-friendly)
-- ---------------------------------------------------------------------------

-- | Name as exposed to MCP clients and (via OpenAPI operationId) to pyvolca.
mcpName :: Resource -> Text
mcpName r = case r of
    ListDatabases -> "list_databases"
    ListPresets -> "list_presets"
    SearchActivities -> "search_activities"
    SearchFlows -> "search_flows"
    GetActivity -> "get_activity"
    Aggregate -> "aggregate"
    GetSupplyChain -> "get_supply_chain"
    GetInventory -> "get_inventory"
    GetImpacts -> "get_impacts"
    ListMethods -> "list_methods"
    GetFlowMapping -> "get_flow_mapping"
    GetCharacterization -> "get_characterization"
    GetContributingFlows -> "get_contributing_flows"
    GetContributingActivities -> "get_contributing_activities"
    ListGeographies -> "list_geographies"
    ListClassifications -> "list_classifications"
    GetPathTo -> "get_path_to"
    GetConsumers -> "get_consumers"

-- ---------------------------------------------------------------------------
-- Projection: CLI subcommand names (kebab-case)
-- ---------------------------------------------------------------------------

{- | Name as exposed on the command line.

Note: some MCP tools don't have a standalone CLI subcommand (e.g.
'list_databases' is 'database list' under the 'database' namespace,
not a top-level command). For those, 'cliName' returns the nested form.
-}
cliName :: Resource -> Text
cliName r = case r of
    ListDatabases -> "database list"
    ListPresets -> "presets"
    SearchActivities -> "activities"
    SearchFlows -> "flows"
    GetActivity -> "activity"
    Aggregate -> "aggregate"
    GetSupplyChain -> "supply-chain"
    GetInventory -> "inventory"
    GetImpacts -> "impacts"
    ListMethods -> "methods"
    GetFlowMapping -> "flow-mapping"
    GetCharacterization -> "characterization"
    GetContributingFlows -> "contributing-flows"
    GetContributingActivities -> "contributing-activities"
    ListGeographies -> "geographies"
    ListClassifications -> "classifications"
    GetPathTo -> "path-to"
    GetConsumers -> "consumers"

-- ---------------------------------------------------------------------------
-- Projection: human-readable description (shared across surfaces)
-- ---------------------------------------------------------------------------

{- | Description of the resource operation.

These strings are consumed as-is by the MCP tool metadata and are
summarised (first sentence) by the CLI --help. They are written for
LLM tool use, so they're detailed and include usage hints.
-}
description :: Resource -> Text
description r = case r of
    ListDatabases ->
        "LCA / ACV — list all loaded LCA databases (Agribalyse, ecoinvent, …). \
        \Call this first to discover which databases are available before searching."
    ListPresets ->
        "LCA / ACV — list named classification filter presets configured in this \
        \instance. Each preset bundles multiple (system, value, mode) classification \
        \filters under a human-readable label. Use the filter values from a preset as \
        \inputs to search_activities classification parameters."
    SearchActivities ->
        "LCA / ACV — search for activities (processes) by name, geography, product, \
        \classification, or preset. Returns a paginated list of matching activities \
        \with their process IDs. Entry point for any LCA/ACV question about a \
        \specific product or process — food (yaourt, steak, pain, lait, fromage), \
        \packaging (PET, verre, carton), matériaux, énergie, transport. Accepts \
        \non-technical synonyms: empreinte carbone, empreinte environnementale, \
        \impact environnemental, occupation des sols, surface agricole, prairie, \
        \pâturage, intrants, filière, chaîne amont."
    SearchFlows ->
        "LCA / ACV — search for biosphere flows (emissions, resources) by name. \
        \Use this to locate specific substances (CO2, CH4, water, land occupation) \
        \before querying characterization factors or inventory contributions."
    GetActivity ->
        "LCA / ACV — get detailed information about an activity: name, location, \
        \exchanges, reference product, metadata. Use exchange_type / is_input / flow \
        \to filter exchanges and reduce response size."
    Aggregate ->
        "LCA / ACV — aggregate exchanges, supply chain entries, or biosphere flows \
        \with SQL group-by-style filters. One primitive replaces ad-hoc decomposition \
        \tools — express any 'how much X is in Y' question as one call. Also the \
        \right tool for 'combien de surface agricole / d'eau / d'énergie dans un \
        \produit ?' style questions via scope=biosphere or scope=supply_chain. \
        \Examples:\n\
        \  - Total electricity in direct inputs: scope=direct, is_input=true, filter_name=Electricity, filter_unit=kWh\n\
        \  - Mass breakdown of direct inputs: scope=direct, is_input=true, filter_unit=kg, group_by=name\n\
        \  - Total energy across the supply chain: scope=supply_chain, max_depth=2, filter_classification=[\"Category type=energy:exact\"]\n\
        \  - Largest pasture occupation flow: scope=biosphere, filter_name=Occupation, pasture, group_by=name\n\
        \\n\
        \The filter_classification parameter accepts a list of strings in \"System=Value[:exact]\" form (default mode is 'contains')."
    GetSupplyChain ->
        "LCA / ACV — get a flat list of all upstream activities in the supply chain \
        \(chaîne amont, filière, intrants). The 'quantity' field is the cumulative \
        \scaled amount relative to the functional unit (scaling_factor × root \
        \reference product amount). To get the per-step yield ratio between two \
        \connected entries, divide the supplier's scaling_factor by the consumer's \
        \scaling_factor."
    GetInventory ->
        "LCA / ACV — compute the Life Cycle Inventory (LCI): biosphere flows \
        \(emissions and resource extractions) for an activity's full supply chain. \
        \Returns statistics and top flows by quantity. Use this (not get_impacts) \
        \when the question targets raw physical flows rather than weighted scores: \
        \land / pasture occupation (m²·year), water withdrawal (m³), specific \
        \emissions (kg CO₂, kg CH₄, kg N), resource extraction."
    GetImpacts ->
        "LCA / ACV — compute Life Cycle Impact Assessment (LCIA) scores for an \
        \activity. Returns the score, functional unit, and top contributing \
        \elementary flows. Answers 'empreinte carbone / environmental footprint' \
        \questions. Covers all LCIA categories: climate change, acidification, \
        \eutrophication, land use, water scarcity, resource depletion. Prefer this \
        \over web estimates for grounded, database-backed answers."
    ListMethods ->
        "LCA / ACV — list all loaded LCIA methods (impact assessment methods like \
        \climate change, acidification, eutrophication, land use, water scarcity)."
    GetFlowMapping ->
        "LCA / ACV — get the mapping between a method's characterization factors \
        \and database flows, showing match coverage."
    GetCharacterization ->
        "LCA / ACV — look up characterization factors for a method matched against \
        \database flows. Without 'flow' filter, returns top factors by absolute \
        \value. With 'flow', searches by name. Shows CF value, direction, matched \
        \database flow, and match strategy."
    GetContributingFlows ->
        "LCA / ACV — identify which elementary flows (emissions/resources) \
        \contribute most to a specific impact category. Answers 'which emissions \
        \drive my climate change score?'"
    GetContributingActivities ->
        "LCA / ACV — identify which upstream activities contribute most to a \
        \specific impact category. Answers 'which suppliers drive my climate change \
        \score?' Uses exact matrix-based computation, valid even for cyclic supply \
        \chains."
    ListGeographies ->
        "LCA / ACV — list all geography codes present in a database, with display \
        \names and parent regions. Use the 'geo' value as the geography filter in \
        \search_activities."
    ListClassifications ->
        "LCA / ACV — list classification systems in a database. Without 'system': \
        \returns system names and activity counts only (lightweight). With 'system': \
        \returns all values for that system. Add 'filter' to narrow values by \
        \substring."
    GetPathTo ->
        "LCA / ACV — find the shortest supply chain path from a process to the \
        \first upstream activity whose name matches a pattern. Each step includes \
        \cumulative_quantity, scaling_factor, and local_step_ratio (upstream ÷ \
        \downstream scaling factors). total_ratio is the product of all \
        \local_step_ratio values — the end-to-end conversion factor."
    GetConsumers ->
        "LCA / ACV — find all activities that transitively consume (depend on) a \
        \given supplier. Returns a flat list, each with a crDepth field: 1 = direct \
        \consumer, 2 = consumer of consumer, etc. Useful for tracing downstream \
        \use of a raw material — e.g. finding transformed food products in \
        \Agribalyse that use a raw ingredient."

-- ---------------------------------------------------------------------------
-- Projection: parameter schema
-- ---------------------------------------------------------------------------

-- Common parameters that appear repeatedly. Defining them once reduces
-- noise in 'params' and keeps descriptions consistent across resources.

pDatabase :: Param
pDatabase = Param "database" "string" Required "Database name"

pProcessId :: Param
pProcessId =
    Param
        "process_id"
        "string"
        Required
        "Process ID (activityUUID_productUUID format)"

pMethodId :: Param
pMethodId = Param "method_id" "string" Required "Method UUID"

pLimit :: Text -> Param
pLimit = Param "limit" "integer" Optional

{- | Optional what-if substitutions. When non-empty, upgrades the underlying
request to the substitution-aware POST pipeline. Each entry is an object
with 'from', 'to', 'consumer' fields (bare 'actUUID_productUUID' or
qualified 'dbName::actUUID_productUUID' for cross-DB swaps).
-}
pSubstitutions :: Param
pSubstitutions =
    Param
        "substitutions"
        "array"
        Optional
        "Optional what-if supplier substitutions. Each entry: \
        \{from: oldSupplierPID, to: newSupplierPID, consumer: consumerPID}. \
        \PIDs can be bare (root DB) or qualified as dbName::pid (cross-DB). \
        \When empty or absent, the call behaves as a plain GET."

-- | Parameters accepted by a resource operation.
params :: Resource -> [Param]
params r = case r of
    ListDatabases -> []
    ListPresets -> []
    SearchActivities ->
        [ pDatabase
        , Param "name" "string" Required "Name substring to search for (or exact name if exact=true)"
        , Param "geo" "string" Optional "Geography/location filter (e.g. 'FR', 'DE', 'GLO')"
        , Param "product" "string" Optional "Product name filter"
        , Param "exact" "boolean" Optional "If true, name and geo must match exactly (case-insensitive equality) instead of substring search"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets) — expands to its bundled filters. Can be combined with explicit classification filters."
        , Param "classification" "string" Optional "Classification system name to filter by (e.g. 'ISIC rev.4 ecoinvent', 'CPC'). Use list_classifications to see available systems."
        , Param "classification_value" "string" Optional "Value within the classification system to match"
        , Param "classification_match" "string" Optional "Match mode: \"equals\" (case-insensitive equality) or \"contains\" (substring, default)"
        , pLimit "Max results (default 20)"
        ]
    SearchFlows ->
        [ pDatabase
        , Param "query" "string" Required "Flow name to search for"
        , pLimit "Max results (default 20)"
        ]
    GetActivity ->
        [ pDatabase
        , pProcessId
        , Param "exchange_type" "string" Optional "Filter exchanges: \"biosphere\" (emissions/resources only), \"technosphere\" (inputs/outputs only), or \"all\" (default)"
        , Param "is_input" "boolean" Optional "If true, return only inputs; if false, only outputs; omit for both. Combines with exchange_type."
        , Param "flow" "string" Optional "Filter exchanges by flow name (case-insensitive substring)"
        ]
    Aggregate ->
        [ pDatabase
        , pProcessId
        , Param "scope" "string" Required "direct | supply_chain | biosphere"
        , Param "is_input" "boolean" Optional "Only for scope=direct — true=inputs only, false=outputs only"
        , Param "max_depth" "integer" Optional "Only for scope=supply_chain — max hops from the root activity"
        , Param "filter_name" "string" Optional "Case-insensitive substring on flow/activity name"
        , Param "filter_name_not" "string" Optional "Comma-separated substring exclude list"
        , Param "filter_unit" "string" Optional "Exact unit name"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets) — expanded and merged into filter_classification."
        , Param "filter_classification" "array" Optional "List of \"System=Value[:exact]\" strings; defaults to 'contains' mode"
        , Param "filter_target_name" "string" Optional "Only for scope=direct technosphere — filter by upstream activity name"
        , Param "filter_is_reference" "boolean" Optional "Filter by reference-product flag (typically for outputs)"
        , Param "group_by" "string" Optional "name | flow_id | name_prefix | unit | classification.<system> | location | target_name"
        , Param "aggregate" "string" Optional "sum_quantity | count | share (default: sum_quantity)"
        ]
    GetSupplyChain ->
        [ pDatabase
        , pProcessId
        , Param "name" "string" Optional "Filter by activity name"
        , Param "location" "string" Optional "Filter by location"
        , pLimit "Max results (default 100)"
        , Param "min_quantity" "number" Optional "Min scaled quantity threshold"
        , Param "max_depth" "integer" Optional "Max depth from root (1 = direct inputs only)"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets) — expands to its bundled filters. Unioned with any explicit classification / classification_value / classification_mode parameters."
        , Param "classification" "string" Optional "Classification system name (e.g. 'Category', 'Category type')"
        , Param "classification_value" "string" Optional "Value within the classification system"
        , Param "classification_match" "string" Optional "Match mode: \"exact\" (case-insensitive equality) or \"contains\" (substring, default)"
        , pSubstitutions
        ]
    GetInventory ->
        [ pDatabase
        , pProcessId
        , Param "flow" "string" Optional "Filter flows by name (case-insensitive substring)"
        , pLimit "Max flows to return, sorted by absolute quantity (default 50)"
        , pSubstitutions
        ]
    GetImpacts ->
        [ pDatabase
        , pProcessId
        , pMethodId
        , Param "top_flows" "integer" Optional "Number of top contributing flows to return (default 5)"
        , pSubstitutions
        ]
    ListMethods -> []
    GetFlowMapping ->
        [ pDatabase
        , pMethodId
        ]
    GetCharacterization ->
        [ pDatabase
        , pMethodId
        , Param "flow" "string" Optional "Filter by flow name (case-insensitive substring, matches both method CF name and database flow name)"
        , pLimit "Max results (default 20)"
        ]
    GetContributingFlows ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "Method UUID for the impact category"
        , pLimit "Max flows to return, sorted by contribution (default 20)"
        ]
    GetContributingActivities ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "Method UUID for the impact category"
        , pLimit "Max processes to return, sorted by contribution (default 10)"
        ]
    ListGeographies ->
        [ pDatabase
        ]
    ListClassifications ->
        [ pDatabase
        , Param "system" "string" Optional "Classification system name to inspect (e.g. 'ISIC rev.4 ecoinvent'). If omitted, returns only system names and counts."
        , Param "filter" "string" Optional "Substring filter applied to values when a system is specified (case-insensitive)."
        ]
    GetPathTo ->
        [ pDatabase
        , pProcessId
        , Param "target" "string" Required "Case-insensitive name substring to stop at"
        ]
    GetConsumers ->
        [ pDatabase
        , Param "process_id" "string" Required "Process ID of the supplier (activityUUID_productUUID format)"
        , Param "name" "string" Optional "Filter by name (case-insensitive substring)"
        , Param "location" "string" Optional "Filter by geography/location (case-insensitive substring, e.g. 'FR', 'DE')"
        , Param "product" "string" Optional "Filter by product name (case-insensitive substring)"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets) — expands to its bundled filters"
        , Param "classification" "string" Optional "Classification system name (e.g. 'ISIC rev.4 ecoinvent')"
        , Param "classification_value" "string" Optional "Classification value substring to match"
        , pLimit "Max results (default 1000)"
        , Param "max_depth" "integer" Optional "Max hops from supplier (1 = direct consumers only)"
        ]

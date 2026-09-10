{-# LANGUAGE OverloadedStrings #-}

{- | Canonical enumeration of user-facing VoLCA operations.

This module names every user-facing operation once, and two surfaces read it:
the MCP tool metadata ("API.MCP") and the published OpenAPI document
("API.OpenApi"). The Haskell call sites use 'Resource' PascalCase constructors
directly.

The command line does not read it. Its subcommands and their help text are
written out by hand in "CLI.Parser", and a projection that claimed otherwise
('cliName') had drifted into naming fifteen commands that do not exist before
it was removed. Nor does pyvolca: it reads the published spec at runtime.

How much of what is written here actually reaches the OpenAPI document is
measured by @test\/ResourcesDriftSpec.hs@, and the answer today is: the
operation names and descriptions, and almost none of the parameter
descriptions.

Adding a new operation means extending the 'Resource' ADT and adding one
equation to each projection function. The compiler catches missing cases
for 'mcpName'/'description'/'params'.
-}
module API.Resources (
    Resource (..),
    Param (..),
    ParamKind (..),
    allResources,
    mcpName,
    description,
    params,
    requiredParams,
    optionalParams,
    resourceMutates,
    apiPath,
    apiPathText,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Method (StdMethod (..))

{- | Every operation VoLCA exposes through its user-facing surfaces.

What belongs here is what an analyst does. That covers the analysis operations,
and loading, unloading or re-keying a database: those change which databases
are in the working set rather than the data itself, and which key divides a
multi-output block is a choice of analysis, not of installation -- it is the
question "what would this study look like allocated by mass", asked of a
source nobody may edit. It also covers editing the inventory
of an activity: adjusting an imported dataset to the study at hand is analysis
work, done on a database of one's own, and the engine refuses it on the
background data it reads from its configuration.

Infrastructure stays in Routes.hs only: method-collection management,
upload, delete, relink, copy, auth, version. Those administer the installation
rather than answer a question about it, and have no analyst-facing equivalent
across every surface.
-}
data Resource
    = ListDatabases
    | LoadDatabase
    | UnloadDatabase
    | DeriveDatabase
    | ListPresets
    | SearchActivities
    | SearchFlows
    | GetActivity
    | Aggregate
    | GetSupplyChain
    | GetInventory
    | GetImpacts
    | ComputeSensitivity
    | ListMethods
    | GetFlowMapping
    | GetCharacterization
    | ExplainCF
    | GetContributingFlows
    | GetContributingActivities
    | ListGeographies
    | ListClassifications
    | GetPathTo
    | GetConsumers
    | CountSearchMatches
    | CompareImpacts
    | ScoreActivity
    | ScoreActivities
    | ListScoringSets
    | GetGapReport
    | GetQualityReport
    | GetComputedQualityReport
    | GetCoverageReport
    | EditExchanges
    deriving (Eq, Ord, Show, Bounded, Enum)

-- | Whether a parameter must be supplied by the caller.
data ParamKind = Required | Optional
    deriving (Eq, Show)

{- | A single parameter accepted by a resource operation.

'paramType' uses JSON Schema type names ("string", "integer", "number",
"boolean", "array") so the MCP tool schema can emit it directly, and the
OpenAPI enrichment reads it too.

A 'Param' does not record /where/ the value rides (path capture, query
parameter, or request body), which is why most of these descriptions never
reach the published spec: the enrichment matches on the name against a route's
query parameters, and a path capture is named by Servant while a body field is
not a parameter at all. @test\/ResourcesDriftSpec.hs@ pins the whole list.
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

{- | Whether the operation changes state shared by every caller of the server,
as opposed to only reading it. Loading and unloading a database change the
working set for the whole process, so they count; every analysis operation
reads and does not.

An instance configured read-only refuses the 'True' ones on every surface.
The match is exhaustive on purpose: a new operation cannot be added without
declaring which side of that line it falls on.
-}
resourceMutates :: Resource -> Bool
resourceMutates r = case r of
    LoadDatabase -> True
    UnloadDatabase -> True
    DeriveDatabase -> True
    ListDatabases -> False
    ListPresets -> False
    SearchActivities -> False
    SearchFlows -> False
    GetActivity -> False
    Aggregate -> False
    GetSupplyChain -> False
    GetInventory -> False
    GetImpacts -> False
    ComputeSensitivity -> False
    ListMethods -> False
    GetFlowMapping -> False
    GetCharacterization -> False
    ExplainCF -> False
    GetContributingFlows -> False
    GetContributingActivities -> False
    ListGeographies -> False
    ListClassifications -> False
    GetPathTo -> False
    GetConsumers -> False
    CountSearchMatches -> False
    CompareImpacts -> False
    ScoreActivity -> False
    ScoreActivities -> False
    ListScoringSets -> False
    GetGapReport -> False
    GetQualityReport -> False
    GetComputedQualityReport -> False
    GetCoverageReport -> False
    EditExchanges -> True

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
spec.
-}
apiPath :: Resource -> Maybe (StdMethod, [Text])
apiPath r = case r of
    ListDatabases -> Just (GET, ["db"])
    LoadDatabase -> Just (POST, ["db", "{dbName}", "load"])
    UnloadDatabase -> Just (POST, ["db", "{dbName}", "unload"])
    DeriveDatabase -> Just (POST, ["db", "{dbName}", "derive", "{newName}"])
    ListPresets -> Just (GET, ["classification-presets"])
    SearchActivities -> Just (GET, ["db", "{dbName}", "activities"])
    SearchFlows -> Just (GET, ["db", "{dbName}", "flows"])
    GetActivity -> Just (GET, ["db", "{dbName}", "activity", "{processId}"])
    Aggregate -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "aggregate"])
    GetSupplyChain -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "supply-chain"])
    GetInventory -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "inventory"])
    GetImpacts -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "impacts", "{collection}", "{methodId}"])
    ComputeSensitivity -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "sensitivity", "{collection}", "{methodId}"])
    ListMethods -> Just (GET, ["methods"])
    GetFlowMapping -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "flow-mapping"])
    GetCharacterization -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "characterization"])
    ExplainCF -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "explain-cf", "{flowId}"])
    GetContributingFlows -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-flows", "{collection}", "{methodId}"])
    GetContributingActivities -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-activities", "{collection}", "{methodId}"])
    ListGeographies -> Nothing -- MCP-only: synthesizes geography list from in-memory database, no HTTP route
    ListClassifications -> Just (GET, ["db", "{dbName}", "classifications"])
    GetPathTo -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "path-to"])
    GetConsumers -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "consumers"])
    CountSearchMatches -> Just (GET, ["db", "{dbName}", "search-counts"])
    CompareImpacts -> Nothing -- MCP-only audit tool: cross-DB diff, no canonical HTTP route
    ScoreActivity -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "impacts", "{collection}"])
    ScoreActivities -> Just (POST, ["db", "{dbName}", "impacts", "{collection}"])
    ListScoringSets -> Nothing -- MCP-only: scoring sets are configuration metadata, no REST equivalent yet
    GetGapReport -> Just (GET, ["db", "{dbName}", "gap-report"])
    GetQualityReport -> Just (GET, ["db", "{dbName}", "quality-report"])
    GetComputedQualityReport -> Just (GET, ["db", "{dbName}", "computed-quality-report"])
    GetCoverageReport -> Just (GET, ["db", "{dbName}", "characterization-coverage"])
    EditExchanges -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "exchanges"])

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
    LoadDatabase -> "load_database"
    UnloadDatabase -> "unload_database"
    DeriveDatabase -> "derive_database"
    ListPresets -> "list_presets"
    SearchActivities -> "search_activities"
    SearchFlows -> "search_flows"
    GetActivity -> "get_activity"
    Aggregate -> "aggregate"
    GetSupplyChain -> "get_supply_chain"
    GetInventory -> "get_inventory"
    GetImpacts -> "get_impacts"
    ComputeSensitivity -> "compute_sensitivity"
    ListMethods -> "list_methods"
    GetFlowMapping -> "get_flow_mapping"
    GetCharacterization -> "get_characterization"
    ExplainCF -> "explain_cf"
    GetContributingFlows -> "get_contributing_flows"
    GetContributingActivities -> "get_contributing_activities"
    ListGeographies -> "list_geographies"
    ListClassifications -> "list_classifications"
    GetPathTo -> "get_path_to"
    GetConsumers -> "get_consumers"
    CountSearchMatches -> "count_search_matches"
    CompareImpacts -> "compare_impacts"
    ScoreActivity -> "score_activity"
    ScoreActivities -> "score_activities"
    ListScoringSets -> "list_scoring_sets"
    GetGapReport -> "get_gap_report"
    GetQualityReport -> "get_quality_report"
    GetComputedQualityReport -> "get_computed_quality_report"
    GetCoverageReport -> "get_characterization_coverage"
    EditExchanges -> "edit_exchanges"

-- ---------------------------------------------------------------------------
-- Projection: human-readable description (shared across surfaces)
-- ---------------------------------------------------------------------------

{- | Trailing rendering tip for tools whose response includes a single
'web_url'. Appended to the bespoke description of each such tool so the
wording stays consistent across them.
-}
webUrlTip :: Text -> Text
webUrlTip page =
    " The response includes a 'web_url' deep link to the "
        <> page
        <> " page in the VoLCA web UI: render it as a clickable markdown \
           \link when presenting results to a human."

{- | Description of the resource operation.

These strings are consumed as-is by the MCP tool metadata and stamped onto
the published OpenAPI operation. They are written for LLM tool use, so they
are detailed and include usage hints.
-}
description :: Resource -> Text
description r = case r of
    ListDatabases ->
        "LCA / ACV: list all loaded LCA databases (Agribalyse, ecoinvent, …). \
        \Call this first to discover which databases are available before searching."
    LoadDatabase ->
        "LCA / ACV: load a configured database into memory so it can be queried. \
        \Its declared dependencies are loaded first (needed for cross-database flow \
        \linking). A database must be loaded before search/score/impact tools can \
        \target it; use list_databases to see which are configured. No effect if it \
        \is already loaded."
    UnloadDatabase ->
        "LCA / ACV: unload a database from memory to free RAM. The on-disk data is \
        \kept and the database can be reloaded later with load_database. Refuses if \
        \another loaded database still depends on it: unload the dependents first."
    DeriveDatabase ->
        "LCA / ACV: read a database's source files again under another allocation \
        \key, and register the result under a new name. Use it to ask what a study \
        \would look like with the multi-output blocks divided by a physical property \
        \instead of the shares the source declares: 'wet mass' divides a block by the \
        \mass of each product, 'dry mass' by its dry matter, 'declared' keeps the \
        \source's own shares. The source is untouched and both stay usable side by \
        \side. This is a full load, not a copy: seconds to minutes on a large \
        \database. Refused when the key divides no block, when it is the key the \
        \source already reads under, when the products carry no such property, and \
        \on a read-only instance."
    ListPresets ->
        "LCA / ACV: list named classification filter presets configured in this \
        \instance. Each preset bundles multiple (system, value, mode) classification \
        \filters under a human-readable label. Use the filter values from a preset as \
        \inputs to search_activities classification parameters."
    SearchActivities ->
        "LCA / ACV: search for activities (processes) by name, geography, product, \
        \classification, or preset. Returns a paginated list of matching activities \
        \with their process IDs. Entry point for any LCA/ACV question about a \
        \specific product or process: food (yaourt, steak, pain, lait, fromage), \
        \packaging (PET, verre, carton), matériaux, énergie, transport. Accepts \
        \non-technical synonyms: empreinte carbone, empreinte environnementale, \
        \impact environnemental, occupation des sols, surface agricole, prairie, \
        \pâturage, intrants, filière, chaîne amont."
    SearchFlows ->
        "LCA / ACV: search flows by name. Three kinds of flow answer, and each \
        \result says which it is in its 'kind' field: a biosphere flow, meaning a \
        \substance exchanged with nature (CO2, CH4, water, land occupation); a \
        \technosphere flow, meaning a product one activity makes and another \
        \consumes; or a waste flow. Pass kind=biosphere to look for a substance \
        \alone, which is what a question about characterization factors or \
        \inventory contributions is usually after. A biosphere flow also reports \
        \the medium it is exchanged with in 'category' (air, water, soil, \
        \resource), which is how a resource taken from nature is told apart from \
        \an emission released to it; the other two kinds have no medium."
    GetActivity ->
        "LCA / ACV: get detailed information about an activity: name, location, \
        \exchanges, reference product, metadata. Use exchange_type / is_input / flow \
        \to filter exchanges and reduce response size."
    Aggregate ->
        "LCA / ACV: aggregate exchanges, supply chain entries, or biosphere flows \
        \with SQL group-by-style filters. One primitive replaces ad-hoc decomposition \
        \tools: express any 'how much X is in Y' question as one call. Also the \
        \right tool for 'combien de surface agricole / d'eau / d'énergie dans un \
        \produit ?' style questions via scope=biosphere or scope=supply_chain. \
        \Examples:\n\
        \  - Total electricity in direct inputs: scope=direct, is_input=true, filter_name=Electricity, filter_unit=kWh\n\
        \  - Mass breakdown of direct inputs: scope=direct, is_input=true, filter_unit=kg, group_by=name\n\
        \  - Total energy across the supply chain: scope=supply_chain, max_depth=2, filter_classification=[\"Category type=energy:exact\"]\n\
        \  - Largest pasture occupation flow: scope=biosphere, filter_name=Occupation, pasture, group_by=name\n\
        \  - Total upstream electricity without double counting: scope=consumption, filter_name=electricity, filter_consumer_not=electricity\n\
        \  - Grass eaten by cattle across the whole chain: scope=consumption, filter_name=grass, filter_consumer=cattle\n\
        \\n\
        \scope=supply_chain rows are cumulative productions: when a filtered product \
        \feeds another filtered product (electricity high→medium→low voltage), their \
        \sum double-counts the chain. scope=consumption has one row per scaled \
        \technosphere edge (product, supplier, consumer), so its sums are actual \
        \consumption events; the default total is gross throughput; exclude \
        \intra-family edges with filter_consumer_not to get the amount delivered \
        \outside the filtered family. Byproduct edges keep their negative sign.\n\
        \\n\
        \The filter_classification parameter accepts a list of strings in \"System=Value[:exact]\" form (default mode is 'contains')."
    GetSupplyChain ->
        "LCA / ACV: get a flat list of all upstream activities in the supply chain \
        \(chaîne amont, filière, intrants). The 'quantity' field is the cumulative \
        \scaled amount relative to the functional unit (scaling_factor × root \
        \reference product amount). To get the per-step yield ratio between two \
        \connected entries, divide the supplier's scaling_factor by the consumer's \
        \scaling_factor. Summing quantities across entries that feed each other \
        \(electricity high→medium→low voltage) double-counts the chain, so use \
        \aggregate with scope=consumption for upstream totals.\n\
        \\n\
        \Every entry states its own 'unit': the producing activity's reference \
        \product unit, which a SimaPro or Brightway Excel import records in the \
        \canonical unit of its dimension (kg, mj, m3). It is not always the unit \
        \written on the exchange that consumes it, so an input of 0.22 kWh appears \
        \here in mj (1 kWh = 3.6 MJ). Read the unit off the entry, never assume it."
    GetInventory ->
        "LCA / ACV: compute the Life Cycle Inventory (LCI): biosphere flows \
        \(emissions and resource extractions) for an activity's full supply chain. \
        \Returns statistics and top flows by quantity. Use this (not get_impacts) \
        \when the question targets raw physical flows rather than weighted scores: \
        \land / pasture occupation (m²·year), water withdrawal (m³), specific \
        \emissions (kg CO₂, kg CH₄, kg N), resource extraction."
    GetImpacts ->
        "LCA / ACV: compute Life Cycle Impact Assessment (LCIA) scores for an \
        \activity. Returns the score, functional unit, and top contributing \
        \elementary flows. Answers 'empreinte carbone / environmental footprint' \
        \questions. Covers all LCIA categories: climate change, acidification, \
        \eutrophication, land use, water scarcity, resource depletion. Prefer this \
        \over web estimates for grounded, database-backed answers. Each \
        \contributing flow carries 'match_kind': how its factor was found, in \
        \the rung names documented on explain_cf; null means no factor in the \
        \method reaches this flow. Ask explain_cf for the full story on \
        \one flow."
            <> webUrlTip "impacts"
    ComputeSensitivity ->
        "LCA / ACV: sensitivity analysis: sweep relative perturbations of \
        \technosphere coefficients A_ij and report the resulting impact for each. \
        \Each perturbation specifies (consumer, supplier, delta) where 'delta' is \
        \relative: A_ij is multiplied by (1+delta). delta=+0.05 → +5%; delta=-1 \
        \removes the link. Returns the baseline impact plus one entry per \
        \perturbation with the new score and deltaImpact. Per-perturbation errors \
        \(no link, singular update) are returned in the entry; the sweep continues. \
        \Internally uses Sherman-Morrison rank-1 updates against the cached \
        \factorization (~4 ms per perturbation). V1: root DB only."
            <> webUrlTip "sensitivity"
    ListMethods ->
        "LCA / ACV: list all loaded LCIA methods (impact assessment methods like \
        \climate change, acidification, eutrophication, land use, water scarcity)."
    GetFlowMapping ->
        "LCA / ACV: get the mapping between a method's characterization factors \
        \and database flows, showing match coverage."
    GetCharacterization ->
        "LCA / ACV: look up characterization factors for a method matched against \
        \database flows. Without 'flow' filter, returns top factors by absolute \
        \value. With 'flow', searches by name. Shows CF value, direction, matched \
        \database flow, and match strategy."
    ExplainCF ->
        "LCA / ACV: explain why one elementary flow scores with the \
        \characterization factor it does. Answers 'why this factor, and which \
        \line of the method was used?'. The 'explanation' field is a list of \
        \sentences written by the engine: relay them as they are rather than \
        \interpreting the codes yourself. 'outcome' is one of: 'characterized' \
        \(a factor applies), 'conversion_refused' (a factor was found but the \
        \flow's unit cannot be converted to the factor's basis, so the flow \
        \scores nothing), 'no_factor' (nothing in the method reaches this \
        \flow). 'match.rung' names how the factor was found: 'flow_id' (the \
        \method names this exact flow), 'same_unit_name' (a factor line \
        \declared in this flow's own unit), 'exact_name' (name and compartment \
        \match), 'long_term_default' (the method's default for long-term \
        \emissions), 'compartment_default' (the method's default for the whole \
        \compartment), 'cas_number' (a factor for the same substance by CAS), \
        \'subcompartment_blind' (the factor is the same in every \
        \subcompartment), 'region_base_name' (the base substance, the name's \
        \region suffix being untagged by the method), 'energy_content' (the \
        \family factor per unit of energy, bridged by the flow's calorific \
        \value), 'ore_base_element' (the base element of a graded ore). \
        \'steps_tried' lists the rungs tried before that one, including any \
        \refused by a subcompartment veto. 'match.unitConversion' names the \
        \bridge that carried the amount onto the factor's basis: 'same_unit', \
        \'unknown_unit' (the flow's unit is not in the unit table, so the \
        \amount passed as declared), 'unit_converted', \
        \'normalized_to_base_unit' (the factor is written as a result \
        \expression like 'kg CO2 eq', so the amount was brought to the flow's \
        \base unit), 'energy_content' (carried across dimensions by the flow's \
        \energy density). 'match.refusal' names why no bridge could: \
        \'different_dimensions', 'no_base_unit', 'energy_bridge_failed'. A \
        \vetoed step names its rule in 'veto': 'different_receiving_medium' \
        \(the method writes sea-water lines and so meant to leave this foreign \
        \medium out), 'long_term_groundwater' (a long-term groundwater \
        \emission must not borrow a surface-fate factor)."
            <> webUrlTip "explain-cf"
    GetContributingFlows ->
        "LCA / ACV: identify which elementary flows (emissions/resources) \
        \contribute most to a specific impact category. Answers 'which emissions \
        \drive my climate change score?'. Each flow carries 'match_kind': how \
        \its factor was found, in the rung names documented on explain_cf; null \
        \means no factor in the method reaches this flow. Ask explain_cf \
        \for the full story on one flow."
            <> webUrlTip "contributing-flows"
    GetContributingActivities ->
        "LCA / ACV: identify which upstream activities contribute most to a \
        \specific impact category. Answers 'which suppliers drive my climate change \
        \score?' Uses exact matrix-based computation, valid even for cyclic supply \
        \chains. Each contributing activity carries a 'web_url' deep link to its \
        \page in the VoLCA web UI: render these as clickable markdown links when \
        \presenting results to a human so they can drill into a specific supplier."
    ListGeographies ->
        "LCA / ACV: list all geography codes present in a database, with display \
        \names and parent regions. Use the 'geo' value as the geography filter in \
        \search_activities."
    ListClassifications ->
        "LCA / ACV: list classification systems in a database. Without 'system': \
        \returns system names and activity counts only (lightweight). With 'system': \
        \returns all values for that system. Add 'filter' to narrow values by \
        \substring."
    GetPathTo ->
        "LCA / ACV: find the shortest supply chain path from a process to the \
        \first upstream activity whose name matches a pattern. Each step includes \
        \cumulative_quantity, scaling_factor, and local_step_ratio (upstream ÷ \
        \downstream scaling factors). total_ratio is the product of all \
        \local_step_ratio values: the end-to-end conversion factor."
    GetConsumers ->
        "LCA / ACV: find all activities that transitively consume (depend on) a \
        \given supplier. Returns a flat list, each with a crDepth field: 1 = direct \
        \consumer, 2 = consumer of consumer, etc. Useful for tracing downstream \
        \use of a raw material, e.g. finding transformed food products in \
        \Agribalyse that use a raw ingredient."
    CountSearchMatches ->
        "LCA / ACV: how many processes, products and flows one query matches in \
        \a database, in a single call. The three are disjoint and together cover \
        \the database: a process is an activity row (what you search, get and \
        \score), a product is a technosphere flow one activity makes and another \
        \consumes, a flow is what is exchanged with nature or discarded. Use it \
        \to tell which of the three a query is really about before listing any of \
        \them: a term matching 2 processes and 300 flows is a substance name, not \
        \a product."
    CompareImpacts ->
        "LCA / ACV audit: score the same logical activity on two (database, \
        \method) pairs and return the per-impact-category delta plus a per-flow \
        \drill-down. Built for cross-database mapping audits: e.g. compare BAFU + \
        \EF3.1 vs SimaPro + EF3.1 to surface flows whose contributions diverge \
        \because of mapping gaps, not because of underlying chemistry. Headline \
        \field is delta.relative_pct: the metric to drive down by adding \
        \synonym pairs to data/flows.csv or by regenerating the chem_synonyms \
        \snapshot."
    ScoreActivity ->
        "LCA / ACV: compute the full LCIA panel + every configured scoring \
        \set for an activity in one call. Returns per-method impact scores, \
        \per-scoring-set aggregate scores, per-scoring-set indicator \
        \breakdown (one entry per scoring variable), display units, and a \
        \'web_url' to the matching view. Use this when you would otherwise \
        \call get_impacts N times across every method of a collection: \
        \replaces N round-trips with one batched solve. Discover available \
        \scoring sets with list_scoring_sets. Render the 'web_url' as a \
        \clickable markdown link when presenting results to a human."
    ScoreActivities ->
        "LCA / ACV: rank N activities against one scoring set in one call. \
        \Returns a columnar JSON shape: {scoring_set, scoring_unit, \
        \functional_unit?, columns, rows, not_found, invalid}. 'columns' is the \
        \header (['name', 'process_id', 'web_url', 'total', <indicator keys...>]) \
        \and 'rows' is a 2D array of scalars: one row per resolved activity. \
        \Hoisting the constant metadata once and packing each activity as a flat \
        \array of scalars makes this shape ~6× smaller than a row-shaped JSON \
        \for batches of 24+ activities. The top-level 'functional_unit' is only \
        \emitted when every resolved row shares the same one; otherwise it is \
        \dropped and 'functional_unit' appears as a per-row column instead (the \
        \'columns' header reflects which shape was emitted). Per-method scores \
        \are NOT included: call score_activity on a specific process_id for \
        \that drill-down. Unresolved process IDs land in not_found / invalid. \
        \\n\n\
        \The chosen scoring set must be unambiguous: pass scoring_sets: \
        \[\"<one>\"] when the collection has more than one scoring set \
        \configured. The 'web_url' column on each row is a deep link to the \
        \activity's impacts page in the VoLCA web UI: render it as a clickable \
        \markdown link when presenting results to a human."
    ListScoringSets ->
        "LCA / ACV: list formula-based scoring sets defined in loaded \
        \method collections. A scoring set is a configured aggregation of \
        \LCIA category scores into one or more weighted/normalized 'score' \
        \values (e.g. an overall single score plus per-area-of-protection \
        \sub-scores). For each set returns: name, display unit, variables \
        \referenced (with the impact category each binds to), computed \
        \intermediates, display labels (variable → human-readable indicator \
        \name shown in score breakdowns), normalization and weighting \
        \factors, and the score \
        \formulas. Use the returned set names as keys when interpreting \
        \score_activity / score_activities responses."
    GetGapReport ->
        "LCA / ACV: supplier-gap report of a database: every input demand \
        \still unsupplied after internal resolution and cross-database \
        \linking, aggregated per (product, location, unit) and ranked by \
        \demanding edges. Each gap carries the blocking reason, the number of \
        \consumer edges and distinct consumers, the total demanded amount, \
        \and the top consuming processes. Answers 'what is missing to switch \
        \or complete this database's background dependency?': typically read \
        \right after a relink."
    GetQualityReport ->
        "LCA / ACV: dataset-soundness report of a database, for the people \
        \who build or repair one: the structural defects a score cannot \
        \reveal. All checks run on staged and loaded databases alike: \
        \entries without exactly one reference exchange, coproduct allocation \
        \percentages that don't sum to 100% (or blocks where only some \
        \coproducts carry one), entries duplicated outright \
        \(same name, location and reference product), non-finite amounts or a \
        \zero reference amount, missing metadata (description, \
        \classification, location, units absent from the registry), \
        \geography the source never declared, read off the dataset name \
        \(SimaPro writes 'Unspecified' in whole databases) or filled in by the \
        \loader; stored \
        \amounts that disagree with the formulas documenting them \
        \(mathematicalRelation, checked at parse time), distinct names \
        \that merge under SimaPro's 80-character truncation, exchanges \
        \without the pedigree scores their database otherwise carries, \
        \reference products nothing in the database consumes (expected for \
        \final products), land transformation whose 'to' and 'from' \
        \areas don't balance within an activity, oxygen-demand or \
        \organic-carbon measures in a physically impossible order (BOD5 above \
        \COD, or dissolved above total organic carbon), flow CAS numbers \
        \whose check digit does not confirm them, and individual allocation \
        \percentages outside the 0-100% range. Each \
        \finding carries a severity (danger, warning, info), the activity it \
        \was found on, and a readable detail. Answers 'is this dataset well \
        \formed?': the complement of the supplier-gap report, which answers \
        \'what is this database missing?'"
    GetComputedQualityReport ->
        "LCA / ACV: computed-checks report of a LOADED database: what the \
        \data computes, judged against the catalogue's own norms. Scores \
        \every (activity, product) entry against one method collection (the \
        \single loaded one, or the 'collection' parameter) and reports: \
        \per-category score outliers, judged on a log scale within \
        \(category, reference-unit) groups by median/MAD: a mg-read-as-kg \
        \unit slip lands three orders of magnitude out; entries whose every \
        \category score is zero (empty or uncharacterized inventory); and \
        \negative category scores (info: legitimate where avoided-production \
        \credits or waste treatment dominate). Complements get_quality_report, \
        \which checks what the database STORES and runs on staged databases \
        \too; this one needs the matrices and a loaded method collection. \
        \Same finding shape: severity, the entry, a readable detail."
    GetCoverageReport ->
        "LCA / ACV: characterization-coverage report of a database against the \
        \loaded LCIA method collections, for the people who maintain databases. \
        \Surfaces the flows a method scores ONLY through a name bridge: VoLCA \
        \matches a factor to a flow that carries a different name for the same \
        \substance (via synonym or CAS number), so the flow is characterized \
        \here, but a tool that matches factors by their exact name has no such \
        \bridge and scores it as zero, silently. Each \
        \bridged flow is grouped under the name the method itself uses (its \
        \rename target). One entry per loaded collection, so two method versions \
        \can be compared side by side. Optionally filtered to a single \
        \collection. Answers 'which of this database's flow names would an \
        \exact-name tool fail to characterize?'"
    EditExchanges ->
        "LCA / ACV: change what one activity consumes and emits, keeping the \
        \activity itself. The only tool that writes data. Use it to adjust an \
        \imported dataset to the study at hand: drop a substance the scope \
        \excludes, correct an amount, add a supplier the dataset is missing. \
        \Everything the edit does not name stays as it is (classification, \
        \synonyms, parameters, pedigree, coproducts), which is why this exists \
        \rather than rewriting the activity. Only the inventory side is \
        \addressable: an input by its provider's process_id, a waste output by \
        \its treatment's process_id, a biosphere line by its flow id (from \
        \get_activity). The reference product and any coproduct are not \
        \reachable, because changing those changes what the activity IS. A \
        \selector that matches nothing is refused rather than silently doing \
        \nothing, and one that matches several lines edits all of them and says \
        \how many. Refused outright on a database the engine reads from its \
        \configuration: copy it first (that background data is shared with \
        \everyone). If the answer says transient, the edit is in memory only \
        \and an unload undoes it."

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

{- | Optional collection disambiguator for the @method_id@-only tools. A method
UUID is a hash of its name, so the same UUID can be loaded under several
collections (e.g. two EF 3.1 versions). Only needed then: with a single match
the collection is inferred, and an ambiguous UUID fails loudly listing the
collections to choose from.
-}
pCollection :: Param
pCollection =
    Param
        "collection"
        "string"
        Optional
        "Method collection name (from list_methods). Only needed when the same \
        \method UUID is loaded in more than one collection (e.g. two EF 3.1 \
        \versions); otherwise the single match is used, and an ambiguous UUID \
        \fails with the list of collections to choose from."

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

{- | Optional switch to drop delayed long-term emissions before scoring.
Shared by 'get_impacts', 'score_activity' and 'score_activities'. Long-term
flows are always emissions (never resources), so excluding them never
touches regionalized water/land categories.
-}
pExcludeLongTerm :: Param
pExcludeLongTerm =
    Param
        "exclude_long_term"
        "boolean"
        Optional
        "When true, drop delayed long-term (> 100 yr) emissions before \
        \characterization: the score is computed as if those emissions were \
        \out of scope. Long-term flows are emissions (never resources), so \
        \regionalized water/land categories are unaffected. Default false \
        \(keep them, per the ecoinvent/EF convention)."

{- | The 'scoring_sets' parameter, shared by 'score_activity' and
'score_activities' but with slightly different semantics:

  * 'score_activity' (single activity): when supplied, restricts the
    response's scoringResults / scoringUnits / scoringIndicators to
    these scoring set names. Omitted or empty keeps every set
    configured on the collection.
  * 'score_activities' (batch): picks the single scoring set the
    columnar response is projected against (one unit, one column list).
    Pass exactly one name in the array. When omitted and the collection
    has a single scoring set configured, that one is auto-picked; with
    multiple sets configured, omitting is an error.

In both cases an unknown name is a hard error that lists what's
configured.
-}
pScoringSetsFilter :: Param
pScoringSetsFilter =
    Param
        "scoring_sets"
        "array"
        Optional
        "For score_activity: restricts the response's scoringResults / \
        \scoringUnits / scoringIndicators to these scoring set names. \
        \For score_activities: selects the single scoring set the \
        \columnar response is projected against; pass one name. Auto-picked \
        \when the collection has exactly one configured set. Use \
        \list_scoring_sets to discover the configured names. An unknown \
        \name fails the call with the list of available names."

-- | Opt-in summary mode for 'score_activities'.
pSummaryOnly :: Param
pSummaryOnly =
    Param
        "summary_only"
        "boolean"
        Optional
        "When true, score_activities replaces the per-indicator columns with \
        \a single 'dominant_indicator' column whose cells are objects \
        \{key, share_pct} (e.g. {\"key\": \"ldu\", \"share_pct\": 82.3}): \
        \the indicator with the largest absolute share of each activity's \
        \total. Use this when ranking large batches before drilling into a \
        \single PID with score_activity. Default false."

-- | Parameters accepted by a resource operation.
params :: Resource -> [Param]
params r = case r of
    ListDatabases -> []
    LoadDatabase -> [pDatabase]
    UnloadDatabase -> [pDatabase]
    DeriveDatabase ->
        [ pDatabase
        , Param "new_name" "string" Required "Name to register the re-keyed database under. Must not already exist."
        , Param "allocation" "string" Optional "The key its multi-output blocks are divided by: declared (the source's own shares, the default) | dry mass | wet mass."
        ]
    ListPresets -> []
    SearchActivities ->
        [ pDatabase
        , Param "name" "string" Required "Name substring to search for (or exact name if exact=true). The identifier a source file gave a dataset (a SimaPro 'Process identifier'), whole or the four or more characters that tell it apart, brings that dataset's products to the top of the results."
        , Param "geo" "string" Optional "Geography/location filter (e.g. 'FR', 'DE', 'GLO'). Matches the location itself, or one the location table places inside it ('US' answers for 'US-WECC', 'RER' for 'FR'). Containment is read from that table and never from how a code is spelled, so 'GL' does not answer for 'GLO'."
        , Param "product" "string" Optional "Product name filter"
        , Param "exact" "boolean" Optional "If true, the name and the geography must match exactly (case-insensitive equality) instead of a substring, respectively prefix, search"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets): expands to its bundled filters. Can be combined with explicit classification filters."
        , Param "classification" "string" Optional "Classification system name to filter by (e.g. 'ISIC rev.4 ecoinvent', 'CPC'). Use list_classifications to see available systems."
        , Param "classification_value" "string" Optional "Value within the classification system to match"
        , Param "classification_match" "string" Optional "Match mode: \"equals\" (case-insensitive equality) or \"contains\" (substring, default)"
        , pLimit "Max results (default 20)"
        ]
    SearchFlows ->
        [ pDatabase
        , Param "query" "string" Required "Flow name to search for"
        , Param "kind" "string" Optional "Keep only these kinds of flow: technosphere | biosphere | waste. Name several separated by commas, as in \"biosphere,waste\". Omit for all three."
        , pLimit "Max results (default 20)"
        ]
    GetActivity ->
        [ pDatabase
        , pProcessId
        , Param "exchange_type" "string" Optional "Filter exchanges by kind: \"technosphere\" (product/input flows), \"biosphere\" (emissions/resources), \"waste\" (third flow kind: residuals routed to treatment), or \"all\" (default)"
        , Param "is_input" "boolean" Optional "If true, return only inputs; if false, only outputs; omit for both. Combines with exchange_type."
        , Param "flow" "string" Optional "Filter exchanges by flow name or synonym, the way search_flows reads a query: every word must appear, case-blind, in any order, punctuation optional"
        ]
    Aggregate ->
        [ pDatabase
        , pProcessId
        , Param "scope" "string" Required "direct | supply_chain | biosphere | consumption"
        , Param "is_input" "boolean" Optional "Only for scope=direct: true=inputs only, false=outputs only"
        , Param "max_depth" "integer" Optional "Only for scope=supply_chain: max hops from the root activity"
        , Param "filter_name" "string" Optional "Case-insensitive substring on flow/activity name"
        , Param "filter_name_not" "string" Optional "Comma-separated substring exclude list"
        , Param "filter_unit" "string" Optional "Whole unit name, case-insensitive: \"MJ\" and \"mj\" are the same unit, \"kg\" does not match \"kgm\""
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets): expanded and merged into filter_classification."
        , Param "filter_classification" "array" Optional "List of \"System=Value[:exact]\" strings; defaults to 'contains' mode"
        , Param "filter_target_name" "string" Optional "Only for scope=direct technosphere or scope=consumption: filter by supplier activity name"
        , Param "filter_consumer" "string" Optional "Only for scope=consumption: case-insensitive substring on the consuming activity's name"
        , Param "filter_consumer_not" "string" Optional "Only for scope=consumption: comma-separated consumer-name exclude list (each item is a substring; a name containing a comma cannot be expressed)"
        , Param "filter_is_reference" "boolean" Optional "Filter by reference-product flag (typically for outputs)"
        , Param "group_by" "string" Optional "name | flow_id | name_prefix | unit | classification.<system> | location | target_name | consumer_name"
        , Param "aggregate" "string" Optional "sum_quantity | count | share (default: sum_quantity)"
        ]
    GetSupplyChain ->
        [ pDatabase
        , pProcessId
        , Param "name" "string" Optional "Filter by activity name"
        , Param "location" "string" Optional "Filter by geography/location: the location itself, or one the location table places inside it, case-insensitive"
        , pLimit "Max results (default 100)"
        , Param "min_quantity" "number" Optional "Min scaled quantity threshold"
        , Param "max_depth" "integer" Optional "Max depth from root (1 = direct inputs only)"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets): expands to its bundled filters. Unioned with any explicit classification / classification_value / classification_mode parameters."
        , Param "classification" "string" Optional "Classification system name (e.g. 'Category', 'Category type')"
        , Param "classification_value" "string" Optional "Value within the classification system"
        , Param "classification_match" "string" Optional "Match mode: \"exact\" (case-insensitive equality) or \"contains\" (substring, default)"
        , pSubstitutions
        ]
    GetInventory ->
        [ pDatabase
        , pProcessId
        , Param "flow" "string" Optional "Filter flows by name or synonym, the way search_flows reads a query: every word must appear, case-blind, in any order, punctuation optional"
        , pLimit "Max flows to return, sorted by absolute quantity (default 50)"
        , pSubstitutions
        ]
    GetImpacts ->
        [ pDatabase
        , pProcessId
        , pMethodId
        , pCollection
        , Param "top_flows" "integer" Optional "Number of top contributing flows to return (default 5)"
        , pSubstitutions
        , pExcludeLongTerm
        , Param "include_diagnostics" "boolean" Optional "When true, surface uncharacterized inventory flows above 0.1% of total |qty|, each with up to 3 candidate similar CFs (PubChem-expanded Jaccard + CAS bridge). Lets reviewers tell genuine method gaps from mapping bugs."
        ]
    ComputeSensitivity ->
        [ pDatabase
        , pProcessId
        , pMethodId
        , pCollection
        , Param
            "perturbations"
            "array"
            Required
            "Array of perturbations to apply in parallel. Each entry is an object \
            \{consumer: ProcessId, supplier: ProcessId, delta: number, label?: string}. \
            \'delta' is RELATIVE: the technosphere coefficient A_ij is multiplied by \
            \(1+delta). Use +0.05 for +5%, -1 to remove a link. 'consumer' is the \
            \activity that consumes the input; 'supplier' is the activity that \
            \produces it. Both must live in the root database (V1 limitation). \
            \'label' is optional and echoed in the response for correlation."
        ]
    ListMethods -> []
    GetFlowMapping ->
        [ pDatabase
        , pMethodId
        , pCollection
        , Param "verbose" "boolean" Optional "When true, return per-CF and per-flow detail beyond the coverage stats: an unmatched_cfs list (CFs with no DB flow), and an unmatched_db_flows list ranked by inventory contribution to a chosen process when process_id is set."
        , Param "process_id" "string" Optional "Required for the unmatched_db_flows ranking: ranks unmatched flows by their share of this process's inventory."
        , Param "max_unmatched" "integer" Optional "Cap on each unmatched list (default 50)"
        ]
    GetCharacterization ->
        [ pDatabase
        , pMethodId
        , pCollection
        , Param "flow" "string" Optional "Filter by flow name (case-insensitive substring, matches both method CF name and database flow name)"
        , pLimit "Max results (default 20)"
        ]
    ExplainCF ->
        [ pDatabase
        , pMethodId
        , pCollection
        , Param "flow_id" "string" Required "Database flow UUID, as returned by search_flows or in the flow_id field of get_contributing_flows"
        ]
    GetContributingFlows ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "Method UUID for the impact category"
        , pCollection
        , pLimit "Max flows to return, sorted by contribution (default 20)"
        , pExcludeLongTerm
        , Param "include_diagnostics" "boolean" Optional "When true, surface uncharacterized inventory flows above 0.1% of total |qty|, each with up to 3 candidate similar CFs (PubChem-expanded Jaccard + CAS bridge)."
        ]
    GetContributingActivities ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "Method UUID for the impact category"
        , pCollection
        , pLimit "Max processes to return, sorted by contribution (default 10)"
        , pExcludeLongTerm
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
    CountSearchMatches ->
        [ pDatabase
        , Param "query" "string" Required "The search term to count matches for"
        ]
    GetConsumers ->
        [ pDatabase
        , Param "process_id" "string" Required "Process ID of the supplier (activityUUID_productUUID format)"
        , Param "name" "string" Optional "Filter by name (case-insensitive substring)"
        , Param "location" "string" Optional "Filter by geography/location (e.g. 'FR', 'DE'): the location itself, or one the location table places inside it, case-insensitive"
        , Param "product" "string" Optional "Filter by product name (case-insensitive substring)"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets): expands to its bundled filters"
        , Param "classification" "string" Optional "Classification system name (e.g. 'ISIC rev.4 ecoinvent')"
        , Param "classification_value" "string" Optional "Classification value substring to match"
        , pLimit "Max results (default 1000)"
        , Param "max_depth" "integer" Optional "Max hops from supplier (1 = direct consumers only)"
        , Param "include_edges" "boolean" Optional "When true, the response carries every technosphere edge whose endpoints are both reachable from the supplier. Lets callers reconstruct supplier→consumer paths without a second get_path_to call."
        ]
    CompareImpacts ->
        [ Param "database_a" "string" Required "First database name"
        , Param "process_id_a" "string" Required "Process ID in database_a (activityUUID_productUUID format)"
        , Param "method_id_a" "string" Required "Method UUID for the A side"
        , Param "collection_a" "string" Optional "Method collection for the A side; needed only when method_id_a is loaded in more than one collection."
        , Param "database_b" "string" Required "Second database name"
        , Param "process_id_b" "string" Required "Process ID in database_b (activityUUID_productUUID format)"
        , Param "method_id_b" "string" Required "Method UUID for the B side"
        , Param "collection_b" "string" Optional "Method collection for the B side; needed only when method_id_b is loaded in more than one collection."
        , Param "top_flows" "integer" Optional "Per-side flow drill-down depth (default 10)"
        ]
    ScoreActivity ->
        [ pDatabase
        , pProcessId
        , Param "collection" "string" Required "Method collection name (use list_methods to discover)"
        , pSubstitutions
        , pExcludeLongTerm
        , pScoringSetsFilter
        ]
    ScoreActivities ->
        [ pDatabase
        , Param "collection" "string" Required "Method collection name"
        , Param "process_ids" "array" Required "Process IDs to score (activityUUID_productUUID). All resolved in one multi-RHS solve."
        , pScoringSetsFilter
        , pSummaryOnly
        , pExcludeLongTerm
        ]
    ListScoringSets ->
        [ Param "collection" "string" Optional "Method collection name. If omitted, returns scoring sets across all loaded collections, grouped by collection."
        ]
    GetGapReport ->
        [ pDatabase
        , pLimit "Max gap entries to return, biggest first (default: all). The header counts always cover the full report, so a truncated list stays countable."
        ]
    GetQualityReport ->
        [ pDatabase
        , pLimit "Max findings to return per check, worst first (default: all). Each check's offenderCount always covers its full list, so a truncated list stays countable."
        ]
    GetComputedQualityReport ->
        [ pDatabase
        , Param "collection" "string" Optional "Method collection to score the catalogue against. Defaults to the single loaded collection; required when several are loaded."
        , pLimit "Max findings to return per check, worst first (default: all). Each check's offenderCount always covers its full list, so a truncated list stays countable."
        ]
    GetCoverageReport ->
        [ pDatabase
        , Param "collection" "string" Optional "Restrict the report to one loaded method collection (from list_methods). If omitted, every loaded collection is reported, so two method versions can be compared side by side."
        , pLimit "Max bridge groups to return per collection (default: all). Each collection's bridgeGroupCount always covers its full list, so a truncated list stays countable."
        ]
    EditExchanges ->
        [ pDatabase
        , pProcessId
        , Param "remove" "array" Optional "Lines to drop. Each is {kind, provider|flow}: kind \"input\" or \"waste\" with the provider's process_id, or kind \"biosphere\" with the flow id."
        , Param "set_amounts" "array" Optional "Lines to restate. Each is {select: {kind, provider|flow}, amount}."
        , Param "add_inputs" "array" Optional "Technosphere inputs to add. Each is {provider, amount} plus optional unit and comment. The flow follows from the provider."
        , Param "add_biosphere" "array" Optional "Biosphere lines to add. Each is {direction, amount} plus either flow (an existing flow id) or name + compartment + unit, which reach the flow the database declares under them and introduce one only when nothing does."
        , Param "add_waste_outputs" "array" Optional "Waste outputs to add. Each is {provider, amount} plus optional unit and comment, where the provider is the treatment process."
        ]

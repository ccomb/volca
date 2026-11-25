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
    fromMcpName,
    description,
    params,
    requiredParams,
    optionalParams,
    resourceMutates,
    resourceNeeds,
    resourceUsage,
    apiPath,
    apiPathText,
) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Groups (maxGroups)
import Network.HTTP.Types.Method (StdMethod (..))
import ReducedModel (maxProcesses)
import Types (Permission (..))
import Usage (UsageKind (..))

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

Infrastructure stays in Routes.hs only: method-collection loading, upload
and delete, relink, database copy, auth, version. Those administer the
installation rather than answer a question about it, and have no
analyst-facing equivalent across every surface. Copying and changing a method
collection are analysis work, the method counterpart of editing an inventory,
and so are here.
-}
data Resource
    = ListDatabases
    | LoadDatabase
    | UnloadDatabase
    | AcceptTerms
    | DeriveDatabase
    | ListPresets
    | SearchActivities
    | SearchFlows
    | GetFlowActivities
    | GetActivity
    | Aggregate
    | GetSupplyChain
    | GetInventory
    | GetImpacts
    | ComputeSensitivity
    | GetReducedModel
    | GetFormulaLanguage
    | CheckFormula
    | ListMethods
    | GetFlowMapping
    | GetCharacterization
    | ExplainCF
    | GetFlowFactors
    | GetContributingFlows
    | GetContributingActivities
    | GetGroupContributions
    | GetGroupMembers
    | GetGroupFlows
    | GetGroupCandidates
    | GetScoreContributingFlows
    | GetScoreContributingActivities
    | ListGeographies
    | ListClassifications
    | ListGroupSets
    | GetPathTo
    | GetConsumers
    | CountSearchMatches
    | CompareImpacts
    | CompareActivities
    | CompareDatabases
    | CompareMethodCollections
    | ProfileMethodCollection
    | ScoreActivity
    | ScoreActivities
    | ListScoringSets
    | GetGapReport
    | GetQualityReport
    | GetComputedQualityReport
    | GetCoverageReport
    | EditExchanges
    | EditActivities
    | SupplyMissing
    | DatabaseParameters
    | DatabaseHistory
    | EditDatabaseParameters
    | ListScenarios
    | SaveScenario
    | DropScenario
    | CopyMethodCollection
    | EditMethodFactors
    | EditMethodCategories
    | EditMethodScoringSets
    | UndoMethodEdit
    | GetMethodHistory
    | SearchMethodFlows
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
    AcceptTerms -> False
    DeriveDatabase -> True
    ListDatabases -> False
    ListPresets -> False
    SearchActivities -> False
    SearchFlows -> False
    GetFlowActivities -> False
    GetActivity -> False
    Aggregate -> False
    GetSupplyChain -> False
    GetInventory -> False
    GetImpacts -> False
    ComputeSensitivity -> False
    GetReducedModel -> False
    GetFormulaLanguage -> False
    CheckFormula -> False
    ListMethods -> False
    GetFlowMapping -> False
    GetCharacterization -> False
    ExplainCF -> False
    GetFlowFactors -> False
    GetContributingFlows -> False
    GetContributingActivities -> False
    GetGroupContributions -> False
    GetGroupMembers -> False
    GetGroupFlows -> False
    GetGroupCandidates -> False
    GetScoreContributingFlows -> False
    GetScoreContributingActivities -> False
    ListGeographies -> False
    ListClassifications -> False
    ListGroupSets -> False
    GetPathTo -> False
    GetConsumers -> False
    CountSearchMatches -> False
    CompareImpacts -> False
    CompareActivities -> False
    CompareDatabases -> False
    CompareMethodCollections -> False
    ProfileMethodCollection -> False
    ScoreActivity -> False
    ScoreActivities -> False
    ListScoringSets -> False
    GetGapReport -> False
    GetQualityReport -> False
    GetComputedQualityReport -> False
    GetCoverageReport -> False
    EditExchanges -> True
    EditActivities -> True
    SupplyMissing -> True
    DatabaseParameters -> False
    DatabaseHistory -> False
    EditDatabaseParameters -> True
    ListScenarios -> False
    SaveScenario -> True
    DropScenario -> True
    CopyMethodCollection -> True
    EditMethodFactors -> True
    EditMethodCategories -> True
    EditMethodScoringSets -> True
    UndoMethodEdit -> True
    GetMethodHistory -> False
    SearchMethodFlows -> False

{- | The permission a database's licence must grant for the operation to answer
at all, judged on each database it is asked about. 'Nothing' for the ones
always answered: their answers carry names, counts and factors, or are trimmed
by the operation itself to what the licence leaves (an activity's exchanges
without their amounts, a score without what weighs in it).

Exhaustive like 'resourceMutates', so a new operation is classed when it is
added.
-}
resourceNeeds :: Resource -> Maybe Permission
resourceNeeds r = case r of
    -- Exchange amounts: an aggregate's direct scope, and a supply chain's
    -- first rung, are the activity's own inputs with their quantities.
    GetInventory -> Just ReadInventory
    Aggregate -> Just ReadInventory
    GetSupplyChain -> Just ReadInventory
    GetConsumers -> Just ReadInventory
    CompareActivities -> Just ReadInventory
    CompareDatabases -> Just ReadInventory
    -- Their gaps and offenders quote amounts.
    GetGapReport -> Just ReadInventory
    GetQualityReport -> Just ReadInventory
    -- Each step carries its cumulative quantity and the exchange ratio
    -- between it and the next.
    GetPathTo -> Just ReadInventory
    -- The formulas of the exchanges the parameters move, and their amounts.
    GetReducedModel -> Just ReadInventory
    -- A formula's value at the defaults reads parameters and named lines: amounts.
    CheckFormula -> Just ReadInventory
    -- Their values give the amounts of the lines whose formulas read them.
    DatabaseParameters -> Just ReadInventory
    -- Each change names the amounts it replaced.
    DatabaseHistory -> Just ReadInventory
    -- What weighs in a score.
    ComputeSensitivity -> Just SeeDetailedScores
    CompareImpacts -> Just SeeDetailedScores
    GetContributingFlows -> Just SeeDetailedScores
    GetContributingActivities -> Just SeeDetailedScores
    GetGroupContributions -> Just SeeDetailedScores
    GetGroupMembers -> Just SeeDetailedScores
    GetGroupFlows -> Just SeeDetailedScores
    GetGroupCandidates -> Just SeeDetailedScores
    GetScoreContributingFlows -> Just SeeDetailedScores
    GetScoreContributingActivities -> Just SeeDetailedScores
    -- Trimmed by the operation itself.
    GetActivity -> Nothing
    GetImpacts -> Nothing
    ScoreActivity -> Nothing
    ScoreActivities -> Nothing
    -- Category scores of each process, which a score shows anyway.
    GetComputedQualityReport -> Nothing
    ListDatabases -> Nothing
    LoadDatabase -> Nothing
    UnloadDatabase -> Nothing
    AcceptTerms -> Nothing
    DeriveDatabase -> Nothing
    ListPresets -> Nothing
    SearchActivities -> Nothing
    SearchFlows -> Nothing
    GetFlowActivities -> Nothing
    CountSearchMatches -> Nothing
    ListClassifications -> Nothing
    ListGroupSets -> Nothing
    ListGeographies -> Nothing
    GetFormulaLanguage -> Nothing
    ListMethods -> Nothing
    ListScoringSets -> Nothing
    GetFlowMapping -> Nothing
    GetCharacterization -> Nothing
    ExplainCF -> Nothing
    GetCoverageReport -> Nothing
    GetFlowFactors -> Nothing
    -- Its reply echoes the lines the caller wrote.
    EditExchanges -> Nothing
    EditActivities -> Nothing
    SupplyMissing -> Nothing
    -- Its reply names the parameters the caller wrote and the processes that moved, no amount.
    EditDatabaseParameters -> Nothing
    -- Values a caller chose, never the database's.
    ListScenarios -> Nothing
    SaveScenario -> Nothing
    DropScenario -> Nothing
    CompareMethodCollections -> Nothing
    ProfileMethodCollection -> Nothing
    CopyMethodCollection -> Nothing
    EditMethodFactors -> Nothing
    EditMethodCategories -> Nothing
    EditMethodScoringSets -> Nothing
    UndoMethodEdit -> Nothing
    GetMethodHistory -> Nothing
    SearchMethodFlows -> Nothing

{- | What an operation does with the process it is asked about, for the usage
log; 'Nothing' for the ones that ask about no process, or only list, search
and describe. Exhaustive like 'resourceNeeds', so a new operation is classed
when it is added.
-}
resourceUsage :: Resource -> Maybe UsageKind
resourceUsage r = case r of
    GetActivity -> Just Reading
    GetSupplyChain -> Just Reading
    GetPathTo -> Just Reading
    GetConsumers -> Just Reading
    GetInventory -> Just Inventorying
    Aggregate -> Just Inventorying
    GetImpacts -> Just Scoring
    ScoreActivity -> Just Scoring
    ScoreActivities -> Just Scoring
    ComputeSensitivity -> Just Scoring
    GetReducedModel -> Just Scoring
    CheckFormula -> Nothing
    GetContributingFlows -> Just Contributing
    GetContributingActivities -> Just Contributing
    GetGroupContributions -> Just Contributing
    GetGroupMembers -> Just Contributing
    GetGroupFlows -> Just Contributing
    GetGroupCandidates -> Just Contributing
    GetScoreContributingFlows -> Just Contributing
    GetScoreContributingActivities -> Just Contributing
    CompareImpacts -> Just Comparing
    CompareActivities -> Just Comparing
    -- A whole database against another, no process asked about.
    CompareDatabases -> Nothing
    GetGapReport -> Nothing
    GetQualityReport -> Nothing
    GetComputedQualityReport -> Nothing
    GetCoverageReport -> Nothing
    ListDatabases -> Nothing
    LoadDatabase -> Nothing
    UnloadDatabase -> Nothing
    AcceptTerms -> Nothing
    DeriveDatabase -> Nothing
    ListPresets -> Nothing
    SearchActivities -> Nothing
    SearchFlows -> Nothing
    GetFlowActivities -> Nothing
    CountSearchMatches -> Nothing
    ListClassifications -> Nothing
    ListGroupSets -> Nothing
    ListGeographies -> Nothing
    GetFormulaLanguage -> Nothing
    ListMethods -> Nothing
    ListScoringSets -> Nothing
    GetFlowMapping -> Nothing
    GetCharacterization -> Nothing
    ExplainCF -> Nothing
    EditExchanges -> Nothing
    EditActivities -> Nothing
    SupplyMissing -> Nothing
    DatabaseParameters -> Nothing
    DatabaseHistory -> Nothing
    EditDatabaseParameters -> Nothing
    ListScenarios -> Nothing
    SaveScenario -> Nothing
    DropScenario -> Nothing
    GetFlowFactors -> Nothing
    CompareMethodCollections -> Nothing
    ProfileMethodCollection -> Nothing
    CopyMethodCollection -> Nothing
    EditMethodFactors -> Nothing
    EditMethodCategories -> Nothing
    EditMethodScoringSets -> Nothing
    UndoMethodEdit -> Nothing
    GetMethodHistory -> Nothing
    SearchMethodFlows -> Nothing

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
    AcceptTerms -> Nothing -- MCP-only: an HTTP reader accepts with the volca_terms cookie
    DeriveDatabase -> Just (POST, ["db", "{dbName}", "derive", "{newName}"])
    ListPresets -> Just (GET, ["classification-presets"])
    SearchActivities -> Just (GET, ["db", "{dbName}", "activities"])
    SearchFlows -> Just (GET, ["db", "{dbName}", "flows"])
    GetFlowActivities -> Nothing -- MCP-only: the REST route answers the bare list, with no total and no limit
    GetActivity -> Just (GET, ["db", "{dbName}", "activity", "{processId}"])
    Aggregate -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "aggregate"])
    GetSupplyChain -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "supply-chain"])
    GetInventory -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "inventory"])
    GetImpacts -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "impacts", "{collection}", "{methodId}"])
    ComputeSensitivity -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "sensitivity", "{collection}", "{methodId}"])
    GetReducedModel -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "reduced-model", "{collection}"])
    GetFormulaLanguage -> Just (GET, ["formula-language"])
    CheckFormula -> Just (POST, ["db", "{dbName}", "formula-check"])
    ListMethods -> Just (GET, ["methods"])
    GetFlowMapping -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "flow-mapping"])
    GetCharacterization -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "characterization"])
    ExplainCF -> Just (GET, ["db", "{dbName}", "method", "{methodId}", "explain-cf", "{flowId}"])
    GetFlowFactors -> Just (GET, ["db", "{dbName}", "flow", "{flowId}", "factors"])
    GetContributingFlows -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-flows", "{collection}", "{methodId}"])
    GetContributingActivities -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-activities", "{collection}", "{methodId}"])
    GetGroupContributions -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "groups", "{collection}"])
    GetGroupMembers -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "group-members", "{collection}", "{methodId}"])
    GetGroupFlows -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "group-flows", "{collection}", "{methodId}"])
    GetGroupCandidates -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "group-candidates", "{collection}", "{methodId}"])
    GetScoreContributingFlows -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-flows", "{collection}", "score", "{scoringSet}", "{score}"])
    GetScoreContributingActivities -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "contributing-activities", "{collection}", "score", "{scoringSet}", "{score}"])
    ListGeographies -> Nothing -- MCP-only: synthesizes geography list from in-memory database, no HTTP route
    ListClassifications -> Just (GET, ["db", "{dbName}", "classifications"])
    ListGroupSets -> Just (GET, ["db", "{dbName}", "group-sets"])
    GetPathTo -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "path-to"])
    GetConsumers -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "consumers"])
    CountSearchMatches -> Just (GET, ["db", "{dbName}", "search-counts"])
    CompareImpacts -> Nothing -- MCP-only audit tool: cross-DB diff, no canonical HTTP route
    CompareActivities -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "compare"])
    CompareDatabases -> Just (GET, ["db", "{dbName}", "compare"])
    CompareMethodCollections -> Just (GET, ["method-collections", "{collection}", "compare"])
    ProfileMethodCollection -> Just (GET, ["method-collections", "{collection}", "profile"])
    ScoreActivity -> Just (GET, ["db", "{dbName}", "activity", "{processId}", "impacts", "{collection}"])
    ScoreActivities -> Just (POST, ["db", "{dbName}", "impacts", "{collection}"])
    ListScoringSets -> Just (GET, ["method-collections", "{collection}", "scoring-sets"])
    GetGapReport -> Just (GET, ["db", "{dbName}", "gap-report"])
    GetQualityReport -> Just (GET, ["db", "{dbName}", "quality-report"])
    GetComputedQualityReport -> Just (GET, ["db", "{dbName}", "computed-quality-report"])
    GetCoverageReport -> Just (GET, ["db", "{dbName}", "characterization-coverage"])
    EditExchanges -> Just (POST, ["db", "{dbName}", "activity", "{processId}", "exchanges"])
    EditActivities -> Just (POST, ["db", "{dbName}", "activities", "exchanges"])
    SupplyMissing -> Just (POST, ["db", "{dbName}", "missing-suppliers", "supply"])
    DatabaseParameters -> Just (GET, ["db", "{dbName}", "parameters"])
    DatabaseHistory -> Just (GET, ["db", "{dbName}", "history"])
    EditDatabaseParameters -> Just (POST, ["db", "{dbName}", "parameters"])
    ListScenarios -> Just (GET, ["db", "{dbName}", "scenarios"])
    SaveScenario -> Just (PUT, ["db", "{dbName}", "scenarios", "{name}"])
    DropScenario -> Just (DELETE, ["db", "{dbName}", "scenarios", "{name}"])
    CopyMethodCollection -> Just (POST, ["method-collections", "{collection}", "copy", "{newName}"])
    EditMethodFactors -> Just (POST, ["method-collections", "{collection}", "factors"])
    EditMethodCategories -> Just (POST, ["method-collections", "{collection}", "categories"])
    EditMethodScoringSets -> Just (POST, ["method-collections", "{collection}", "scoring-sets"])
    UndoMethodEdit -> Just (POST, ["method-collections", "{collection}", "undo"])
    GetMethodHistory -> Just (GET, ["method-collections", "{collection}", "history"])
    SearchMethodFlows -> Just (GET, ["method-collections", "{collection}", "flows"])

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
    AcceptTerms -> "accept_terms"
    DeriveDatabase -> "derive_database"
    ListPresets -> "list_presets"
    SearchActivities -> "search_activities"
    SearchFlows -> "search_flows"
    GetFlowActivities -> "get_flow_activities"
    GetActivity -> "get_activity"
    Aggregate -> "aggregate"
    GetSupplyChain -> "get_supply_chain"
    GetInventory -> "get_inventory"
    GetImpacts -> "get_impacts"
    ComputeSensitivity -> "compute_sensitivity"
    GetReducedModel -> "get_reduced_model"
    GetFormulaLanguage -> "get_formula_language"
    CheckFormula -> "check_formula"
    ListMethods -> "list_methods"
    GetFlowMapping -> "get_flow_mapping"
    GetCharacterization -> "get_characterization"
    ExplainCF -> "explain_cf"
    GetFlowFactors -> "get_flow_factors"
    GetContributingFlows -> "get_contributing_flows"
    GetContributingActivities -> "get_contributing_activities"
    GetGroupContributions -> "get_group_contributions"
    GetGroupMembers -> "get_group_members"
    GetGroupFlows -> "get_group_flows"
    GetGroupCandidates -> "get_group_candidates"
    GetScoreContributingFlows -> "get_score_contributing_flows"
    GetScoreContributingActivities -> "get_score_contributing_activities"
    ListGeographies -> "list_geographies"
    ListClassifications -> "list_classifications"
    ListGroupSets -> "list_group_sets"
    GetPathTo -> "get_path_to"
    GetConsumers -> "get_consumers"
    CountSearchMatches -> "count_search_matches"
    CompareImpacts -> "compare_impacts"
    CompareActivities -> "compare_activities"
    CompareDatabases -> "compare_databases"
    CompareMethodCollections -> "compare_method_collections"
    ProfileMethodCollection -> "profile_method_collection"
    ScoreActivity -> "score_activity"
    ScoreActivities -> "score_activities"
    ListScoringSets -> "list_scoring_sets"
    GetGapReport -> "get_gap_report"
    GetQualityReport -> "get_quality_report"
    GetComputedQualityReport -> "get_computed_quality_report"
    GetCoverageReport -> "get_characterization_coverage"
    EditExchanges -> "edit_exchanges"
    EditActivities -> "edit_activities"
    SupplyMissing -> "supply_missing_product"
    DatabaseParameters -> "get_database_parameters"
    DatabaseHistory -> "get_database_history"
    EditDatabaseParameters -> "edit_database_parameters"
    ListScenarios -> "list_scenarios"
    SaveScenario -> "save_scenario"
    DropScenario -> "drop_scenario"
    CopyMethodCollection -> "copy_method_collection"
    EditMethodFactors -> "edit_method_factors"
    EditMethodCategories -> "edit_method_categories"
    EditMethodScoringSets -> "edit_scoring_sets"
    UndoMethodEdit -> "undo_method_edit"
    GetMethodHistory -> "get_method_history"
    SearchMethodFlows -> "search_method_flows"

-- | The resource a client names by its MCP tool name, if the registry declares one.
fromMcpName :: Text -> Maybe Resource
fromMcpName name = find ((== name) . mcpName) allResources

-- ---------------------------------------------------------------------------
-- Projection: human-readable description (shared across surfaces)
-- ---------------------------------------------------------------------------

{- | Closes the description of each tool whose answer carries a 'web_url'.
The server's instructions ("API.MCP") say it too, but a client may show its
model the tool list alone, and models left without it print bare ids.
-}
webUrlTip :: Text
webUrlTip = " Show each 'web_url' as a clickable link: it opens the VoLCA page."

{- | Description of the resource operation.

These strings are consumed as-is by the MCP tool metadata and stamped onto
the published OpenAPI operation. A client loads every one of them into its
model's context before the first question, so they are written terse: each
fact a caller needs, once, in as few words as hold it.
-}
description :: Resource -> Text
description r = case r of
    ListDatabases ->
        "LCA / ACV: list loaded LCA databases (Agribalyse, ecoinvent…). Call \
        \first, before searching."
    LoadDatabase ->
        "LCA / ACV: load a configured database into memory, its declared \
        \dependencies first (cross-database links). Search, score and impact tools \
        \need it loaded; list_databases names the configured ones. No effect if \
        \loaded."
    UnloadDatabase ->
        "LCA / ACV: unload a database to free memory; files stay, load_database \
        \reloads it. Refused while a loaded database depends on it: unload those \
        \first."
    AcceptTerms ->
        "LCA / ACV: record that the user accepts the terms of use a database's \
        \publisher requires before reading; until then tools reading it are \
        \refused with the terms. Call only after showing the user the full terms \
        \and getting their explicit agreement: never on your own initiative, never \
        \to get past the refusal for them."
    DeriveDatabase ->
        "LCA / ACV: make a parent or child of a database under a new name. Parent \
        \(parent=true): source files reread, each multi-product block kept whole, \
        \declared shares turned into prices. Child (system_model): a parent \
        \divided by a system model, TOML text naming the default property (price, \
        \dry mass, wet mass) and exceptions, each with property, reason, and \
        \processes taken by classification or activity id. Shows a study with \
        \multi-output blocks divided by a product property, not the source's \
        \shares. Source untouched; all databases usable side by side. Full load, \
        \not a copy: seconds to minutes when large. Refused: neither or both given, \
        \child of a non-parent, model that does not read, exception taking or \
        \dividing nothing, read-only instance."
    ListPresets ->
        "LCA / ACV: this instance's classification filter presets: each a label \
        \over (system, value, mode) filters, for search_activities' classification \
        \parameters."
    SearchActivities ->
        "LCA / ACV: search processes by name, geography, product, \
        \classification or preset. Ranked, paginated, best first, with process \
        \IDs. Activity and product names together must hold every \
        \word, or a close spelling. Entry point for any \
        \product or process question, any language: \
        \food (yaourt, steak, pain, lait, fromage), packaging (PET, verre, carton), \
        \matériaux, énergie, transport, empreinte carbone, empreinte \
        \environnementale, impact environnemental, occupation des sols, surface \
        \agricole, prairie, pâturage, intrants, filière, chaîne amont. Names are \
        \mostly English: translate into their language first (pomme de terre: \
        \potato, acier: steel). \
        \Everything except: a not-contains, not-exact or not-starts-with classification mode, or \
        \exclude_name and exclude_location, leaves out what they match. \
        \Nothing fits: try the database's own wording, or a broader word."
    SearchFlows ->
        "LCA / ACV: search flows by name. 'kind': biosphere (substance exchanged \
        \with nature: CO2, CH4, water, land occupation), technosphere (product \
        \one process makes, another consumes) or waste. kind=biosphere keeps \
        \substances only, what factor and inventory questions \
        \want. For kind=biosphere, 'category' is the medium \
        \(air, water, soil, resource): resource taken vs \
        \emission released; other kinds have none. \
        \A whole query also finds by formula (N2O), by a name the \
        \synonym registry gives the same substance (nitrous oxide), or by CAS \
        \(10024-97-2). Only the whole query is read these ways: 'N2O \
        \emission' finds by its words alone. \
        \Next: get_flow_activities lists the processes on one side of a flow."
    GetFlowActivities ->
        "LCA / ACV: the processes on one side of one flow, by its flow_id from \
        \search_flows. role=producer: for an emission, the processes that emit \
        \it; for a product, those that supply it. role=consumer: for a \
        \resource, the processes that take it; for a product or a waste, those \
        \that use it. role=any (default): every process with an exchange on \
        \the flow. 'total' counts them all, 'processes' lists the first \
        \'limit' of them."
    GetActivity ->
        "LCA / ACV: one process in detail: \
        \name, location, exchanges, reference \
        \product, metadata, parameters its formulas read (own and database's), \
        \input uncertainty. Amounts are as the process declares them, where \
        \computed tools are for 1 unit, the functional unit of their answer. \
        \exchange_type / is_input / flow filter exchanges. \
        \Metadata (description, documentation, source) comes with every filter; \
        \without hundreds of exchanges: exchange_type=technosphere, \
        \is_input=false."
    Aggregate ->
        "LCA / ACV: sum exchanges, upstream entries, or \
        \emissions and resources, with group-by filters: any \
        \'how much X in Y' in one call, e.g. 'combien de \
        \surface agricole / d'eau / d'énergie dans un produit ?' via \
        \scope=biosphere or scope=supply_chain. Examples:\n\
        \  - Electricity in direct inputs: scope=direct, is_input=true, filter_name=Electricity, filter_unit=kWh\n\
        \  - Mass breakdown of direct inputs: scope=direct, is_input=true, filter_unit=kg, group_by=name\n\
        \  - Energy upstream: scope=supply_chain, max_depth=2, filter_classification=[\"Category type=energy:exact\"]\n\
        \  - Largest pasture occupation flow: scope=biosphere, filter_name=Occupation, pasture, group_by=name\n\
        \  - Upstream electricity without double counting: scope=consumption, filter_name=electricity, filter_consumer_not=electricity\n\
        \  - Grass eaten by cattle upstream: scope=consumption, filter_name=grass, filter_consumer=cattle\n\
        \scope=supply_chain rows are cumulative \
        \upstream amounts: a filtered product \
        \feeding another (electricity high→medium→low voltage) counts twice. \
        \scope=consumption: one row per scaled supplier → consumer edge (with \
        \its product), so sums are real consumption; gross throughput by \
        \default, filter_consumer_not drops edges inside the family. Byproduct \
        \edges stay negative. filter_classification: \"System=Value[:exact|:starts-with]\" \
        \(default contains). Amounts are for 1 unit of the reference product; see \
        \'functionalUnit'. They are 'filteredTotalPerUnit' and, per group, \
        \'quantityPerUnit'. Inputs no loaded database supplies count as zero, in \
        \'cutoffInputs'; 'withheldCutoffs' counts those a licence hides."
    GetSupplyChain ->
        "LCA / ACV: flat list of all upstream \
        \processes (chaîne amont, filière, \
        \intrants). 'quantityPerUnit': cumulative amount for 1 unit of the \
        \reference product (see 'functionalUnit'); step yield = supplier's \
        \scalingFactor ÷ consumer's. Summing entries feeding each other \
        \(electricity high→medium→low voltage) counts twice: use aggregate \
        \scope=consumption for totals. 'unit' \
        \is the supplier's reference unit, \
        \which SimaPro and Brightway Excel imports write in the dimension's \
        \canonical unit (kg, mj, m3), so may differ \
        \from the unit on the consumer's line: \
        \0.22 kWh shows in mj (1 kWh = 3.6 MJ). Read it off the entry. Inputs no \
        \loaded database supplies count as zero, in 'cutoffInputs'; \
        \'withheldCutoffs' counts those a licence hides."
    GetInventory ->
        "LCA / ACV: Life Cycle Inventory (LCI) of a process and its whole \
        \upstream: emissions and resources as statistics and top flows by \
        \amount. Use it, not get_impacts, for raw physical flows rather than \
        \weighted scores: land / pasture occupation (m²·year), water withdrawal \
        \(m³), emissions (kg CO₂, kg CH₄, kg N), resource \
        \extraction. Amounts are for 1 unit of the \
        \reference product; see 'functional_unit'. Inputs \
        \no loaded database supplies count as zero: 'cutoff_notice' opens the \
        \answer, 'cutoff_inputs' lists them, 'withheld_cutoffs' counts those a \
        \licence hides."
    GetImpacts ->
        "LCA / ACV: LCIA score of a process for one impact category, with \
        \functional unit and top contributing flows. Answers empreinte carbone / \
        \environmental footprint questions, \
        \any impact category (climate change, \
        \acidification, eutrophication, land use, water scarcity, resource \
        \depletion); prefer it to web estimates. Flow 'match_kind': how its factor \
        \was found (rungs on explain_cf), null if none reaches it. \
        \'probable_misses': flows scored without a factor though \
        \the impact category writes their substance elsewhere in \
        \their medium, at a subcompartment the flow \
        \does not read, where the method collection \
        \writes nothing at the flow's own \
        \place; 'writtenAt' says where, 'upTo' what it would add at the largest of \
        \those factors, in the score's unit. 'basis' as on score_activity. Inputs \
        \no loaded database supplies count as zero: 'cutoff_notice' opens the \
        \answer, 'cutoff_inputs' lists them, 'withheld_cutoffs' counts those a \
        \licence hides."
            <> webUrlTip
    ComputeSensitivity ->
        "LCA / ACV: sensitivity: score relative perturbations of supplier → \
        \consumer amounts. Each (consumer, supplier, delta) multiplies A_ij by \
        \(1+delta): +0.05 is +5%, -1 removes the link. Returns baseline, then per \
        \perturbation new score and deltaImpact; an error (no link, singular \
        \update) stays in its entry, the sweep goes on. Rank-1 updates on the \
        \cached factorization, ~4 ms each. Root database only. 'basis' as on \
        \score_activity."
            <> webUrlTip
    GetReducedModel ->
        "LCA / ACV: a process's scores as a function of some input parameters, \
        \to run many values outside the engine (Monte Carlo, Sobol). Parameters as \
        \get_activity lists them: {name} for the database's, {process, name} for a \
        \process's own. Per impact category of the \
        \method collection: score at the database's \
        \values ('baseline'); processes these parameters move, with exchanges as \
        \formula trees, stored amounts, emission factors; suppliers' unit scores \
        \and the scaling each gives the moving processes; \
        \scoring sets as formulas over impact category \
        \totals. Refused when a parameter moves an \
        \allocation share, or more than "
            <> T.pack (show maxProcesses)
            <> " processes."
    GetFormulaLanguage ->
        "The formula language of parameters and exchanges: each function and \
        \operator with usage, meaning, worked example; operators by binding, \
        \loosest first; rules for numbers, names, arguments."
    CheckFormula ->
        "Check a formula where it would be written, at the database's values: \
        \among a process's parameters ('process_id'), on one of its lines \
        \('exchange', position among exchanges), or among database parameters if \
        \no process. Answers value and names read, or the problem: name without \
        \value, cycle, formula that does not read. On a line, one parameter (or a \
        \number times one) whose unit does not convert to the line's is warned; \
        \products and quotients of units are not judged."
    ListMethods ->
        "LCA / ACV: the impact categories of the \
        \loaded method collections (climate change, \
        \acidification, eutrophication, land use, water scarcity…)."
    GetFlowMapping ->
        "LCA / ACV: how an impact category's \
        \characterization factors map onto database \
        \flows, with match coverage."
    GetCharacterization ->
        "LCA / ACV: an impact category's characterization \
        \factors matched to database flows. No 'flow': top \
        \factors by absolute value; with it, a name search. \
        \Gives CF value, direction, matched flow, match strategy."
    ExplainCF ->
        "LCA / ACV: why one emission or resource scores with its \
        \characterization factor, and which line of the impact \
        \category applied. Relay the 'explanation' sentences \
        \as written; do not decode the codes yourself. 'outcome': characterized, \
        \conversion_refused (factor found, unit does not convert: scores nothing) \
        \or no_factor. 'match.rung', how the factor was \
        \found: flow_id (flow named by the impact category), \
        \same_unit_name (line in the flow's unit), exact_name \
        \(name and compartment), compartment_default (the compartment's line), \
        \if_absent (subcompartment without line, sent to another by the \
        \compartment table), resource_unspecified (resource read at its name's \
        \unspecified line), cas_number (same CAS), region_base_name (base \
        \substance, region suffix untagged by the \
        \impact category), energy_content (charge \
        \per energy, or the carrier family's factor over its energy content, \
        \applied to the content the name states), ore_base_element (graded ore's \
        \base element). 'steps_tried': rungs tried before. \
        \'match.unitConversion': same_unit, unknown_unit (unit not in the table, \
        \amount kept), unit_converted, normalized_to_base_unit (factor written as \
        \'kg CO2 eq'-like expression, amount brought to base unit), energy_content \
        \(across dimensions by energy density). 'match.refusal': \
        \different_dimensions, no_base_unit, energy_bridge_failed."
            <> webUrlTip
    GetFlowFactors ->
        "LCA / ACV: which impact categories count one flow, with what factor, \
        \in every loaded method collection (or one): 'does this emission count \
        \anywhere?' in one call; explain_cf answers one impact \
        \category at a time. Per method collection, 'factors' \
        \lists impact categories with something to say, shaped as \
        \explain_cf answers (relay 'explanation'). 'outcome': characterized, \
        \conversion_refused (factor unusable: scores nothing), or no_factor with \
        \'regionalFactorCount' > 0: charged \
        \by the emitting process's location, \
        \so it counts though no single factor answers. \
        \'noFactor': impact categories giving no factor anywhere. \
        \All 'factors' empty: the flow adds nothing to any \
        \score, whatever its amount."
            <> webUrlTip
    GetContributingFlows ->
        "LCA / ACV: emissions and resources weighing most in one impact \
        \category: 'which emissions drive my climate change score?'. Flow \
        \'match_kind': how its factor was found (rungs on explain_cf), null if \
        \none reaches it. 'basis' as on score_activity."
            <> webUrlTip
    GetContributingActivities ->
        "LCA / ACV: upstream processes weighing most in one impact category: \
        \'which suppliers drive my climate change score?'. Exact matrix \
        \computation, valid for cyclic chains. 'basis' as on score_activity."
            <> webUrlTip
    GetGroupContributions ->
        "LCA / ACV: how much transport, electricity, fertilisers… weigh in a \
        \score, wherever they sit upstream. Each group is a name and \
        \a filter; a process goes to the first group it answers, the root to \
        \none. attribution 'first-match' (default reading): a group counts the \
        \whole chain below its first process met; 'own': its processes' own \
        \emissions only. Every impact category at once, scores keyed by \
        \method id: groups, 'rest' and 'withheldDatabases' add up to 'total'. \
        \'activitiesCounted': processes of the group carrying a part. Exact, \
        \valid for cyclic chains. Then get_group_members to see a group."
            <> webUrlTip
    GetGroupMembers ->
        "LCA / ACV: the processes of one group of get_group_contributions, or \
        \of the rest when 'group' is left out, heaviest first, for one impact \
        \category. Same 'attribution' and 'groups' as that call. Under \
        \first-match, each process where the chain enters the group, with all \
        \it brings. Under 'own' with a group keeping a substance, each \
        \process's part of the emissions it gives the group, so a process may \
        \be in a group and in the rest. They add up to the group's part. \
        \'basis' as on score_activity."
            <> webUrlTip
    GetGroupFlows ->
        "LCA / ACV: the emissions and resources of one group of \
        \get_group_contributions, or of the rest when 'group' is left out, \
        \heaviest first, for one impact category: what 'Transport' or 'N2O of \
        \the fields' emits. Same 'attribution' and 'groups' as that call; \
        \'total_score' is the group's part. A database whose licence withholds \
        \its detail lists no flow, only its line in the rest."
            <> webUrlTip
    GetGroupCandidates ->
        "LCA / ACV: groups a process or a flow could start, narrowest first. \
        \A process: its product, then each level of its classifications. A \
        \flow: that substance in its compartment, then in any, then by its CAS \
        \(which takes every form of the molecule, e.g. fossil and biogenic). \
        \Each is tried at the end of 'groups' and comes back with its part of \
        \one impact category, how many processes it takes something of and the \
        \products of the heaviest; a group taking nothing, or the same as a \
        \broader one, is left out. Name 'candidate' (a process id as \
        \get_group_members writes it) or 'candidate_flow' (a flow id as \
        \get_contributing_flows writes it), not both. Add the group chosen to \
        \'groups' as it comes."
    GetScoreContributingFlows ->
        "LCA / ACV: emissions and resources weighing most in one \
        \score of a scoring set (one number weighing a method \
        \collection's impact categories; list_scoring_sets): \
        \'which emissions drive my single score?'. Contribution \
        \= weighted sum over impact categories, so flows add up \
        \to score_activity's score. 'cf_value': \
        \what one unit of the flow adds. A score that is \
        \not a weighted sum of impact categories has no \
        \split: refused. 'basis' as on score_activity."
            <> webUrlTip
    GetScoreContributingActivities ->
        "LCA / ACV: upstream processes weighing most in one score of a scoring \
        \set (list_scoring_sets): 'which suppliers drive my single score?'. \
        \Contribution = weighted sum over impact \
        \categories, so processes add up to \
        \score_activity's score. A score that is not a weighted sum of impact \
        \categories has no split: refused. 'basis' as on score_activity."
            <> webUrlTip
    ListGeographies ->
        "LCA / ACV: a database's geography codes, with display names and parent \
        \regions. 'geo' is search_activities' geography filter."
    ListClassifications ->
        "LCA / ACV: a database's classification systems. \
        \No 'system': names and process counts only. \
        \With it: all its values, 'filter' narrowing by \
        \substring."
    ListGroupSets ->
        "LCA / ACV: groups made ready (transport, electricity, waste…) for \
        \the classification systems a database's processes carry, first \
        \set first. Pass a set's 'groups' to get_group_contributions as \
        \they come."
    GetPathTo ->
        "LCA / ACV: shortest upstream path from a process to the first \
        \upstream process whose name matches a pattern, in its database or \
        \in a dependency database it buys from. Each step: \
        \cumulativeQuantityPerUnit (cumulative amount for 1 unit of the \
        \reference product; see the top-level 'functionalUnit'), scalingFactor, \
        \localStepRatio (supplier's ÷ consumer's scalingFactor); total_ratio, \
        \their product, is the end-to-end conversion factor. Under \
        \substitutions, read in the chain get_supply_chain lists with them."
    GetConsumers ->
        "LCA / ACV: every process consuming a supplier, directly or \
        \not, as a flat list; crDepth 1 = direct consumer, 2 = \
        \consumer's consumer… Follows a raw material to what consumes \
        \it, e.g. processed foods made with one ingredient."
    CountSearchMatches ->
        "LCA / ACV: how many processes, products and flows one query matches in a \
        \database, one call. Disjoint, together the whole database: process \
        \= what is searched, read, scored, product = what one process makes \
        \and another consumes, flow = exchanged with nature or \
        \discarded. Tells what a query is about before listing: 2 processes and \
        \300 flows is a substance, not a product."
    CompareImpacts ->
        "LCA / ACV audit: score one logical process on two \
        \(database, impact category) pairs: per-impact-category \
        \delta and per-flow drill-down. For cross-database \
        \mapping audits, e.g. BAFU + EF3.1 vs SimaPro + EF3.1: flows diverging from \
        \mapping gaps, not chemistry. Headline delta.relative_pct, driven down by \
        \synonym pairs in data/flows.csv or a regenerated chem_synonyms snapshot. \
        \Each side carries 'probable_misses' (see get_impacts) and 'basis' (see \
        \score_activity)."
    CompareActivities ->
        "LCA / ACV: compare two processes line by line: lines only one has, \
        \lines whose amount or unit differ. Other process \
        \may be in another loaded database (other_database): \
        \adapted copy vs original. Lines pair on \
        \flow id and role, then flow name (case, trailing ' {GEO}' and SimaPro's \
        \' U'/' S' aside), compartment and role; 'match' says which ('SameFlow', \
        \'SameFlowName'). Role is part of a line: input turned coproduct = one \
        \removed, one added. One flow's lines in one unit are summed; one line \
        \each side with another supplier = 'SupplierChanged' (each by activity \
        \name and location); several suppliers compare totals only. A flow in \
        \several units on one side: 'uncompared'. Amounts equal within relative \
        \1e-9; units by name. 'summary': differing activity name, location, \
        \product name, allocation share, description, stated dates; product amount \
        \by its reference line. Nothing listed: same content."
    CompareDatabases ->
        "LCA / ACV: compare two loaded databases, usually two versions: \
        \processes added, removed, changed (with compare_activities' detail). \
        \Pairing cascade, each rung on what earlier ones left: 'SameProcessId' \
        \(same activityUUID_productUUID), 'SameNames' (activity and product names, \
        \case, trailing ' {GEO}' and SimaPro's ' U'/' S' aside, same location), \
        \'SameProduct' (same reference product flow, location and activity kind, \
        \so no market pairs a production), 'SameCutNames' (names over 80 \
        \characters cut with their geography, as SimaPro \
        \does). Changed processes name their rung. A key \
        \several processes share, either side, pairs none: \
        \'ambiguous', not compared. Only stated dates differ: 'redated'; same \
        \content: 'unchanged', with rung; the three lists are all pairs. \
        \'database' is the base, usually older. Counts cover full lists, limit \
        \truncates each: pass one in a conversation (thousands differ, \
        \'unchanged' holds nearly all)."
    CompareMethodCollections ->
        "LCA / ACV: compare two loaded method collections, usually \
        \two versions or ports of one method collection: per \
        \impact category pair, factors added, removed, changed \
        \(both values and ratio). Impact categories pair on 'pairs', then \
        \by name ('SameMethodName'), then by the group label each \
        \carries ('SameImpactCategory'), case and spacing aside; a \
        \name several share is 'ambiguous', settled by 'base=other' in 'pairs'. \
        \Factors pair per direction, compartment and location in a cascade: \
        \'SameFlowId', 'SameName', 'SameSynonymClass', 'SameCAS' (refused when the \
        \registry keeps the names apart), 'SamePattern' for pattern and exclusion \
        \rows. Compartment-table fallbacks not followed: a precise subcompartment \
        \never equals unspecified; 'unspecified' is the whole medium. A factor \
        \without location whose name ends in a geography code ('Ammonia, FR') is \
        \read there. 'Occupation, ...' / 'Transformation, ...' under the whole \
        \natural-resource medium read in the land subcompartment, every land \
        \factor as input. 'reading': how both values were read per unit; equal \
        \within relative 1e-9. 'collection' is the base. Counts cover full lists, \
        \limit truncates each list of each impact \
        \category: pass one in a conversation."
    ProfileMethodCollection ->
        "LCA / ACV: what each impact category of a \
        \loaded method collection holds, without \
        \listing factors: factors per medium ('media', none for unfiled), how many \
        \written for one location and over how many locations, how many carry it \
        \at the end of the name ('Ammonia, FR'), how many zero, how many pattern or \
        \exclusion rows, and duplicates (one key, one place: a comparison cannot \
        \pair them). Compartments and locations read as \
        \compare_method_collections does."
    ScoreActivity ->
        "LCA / ACV: a process's full LCIA panel \
        \and every configured scoring set \
        \in one call: per-impact-category scores, \
        \per-set scores and impact category breakdown \
        \(one entry per scoring variable), display units, 'web_url'. One batched \
        \solve instead of get_impacts per impact \
        \category. list_scoring_sets names the \
        \sets. exchange_contributions splits each score \
        \by the process's own lines, to group into stages \
        \(ingredients, transport, packaging). Inputs \
        \no loaded database supplies count as zero, in 'cutoffInputs'; \
        \'withheldCutoffs' counts those a licence hides. 'basis': what the score \
        \rests on (engine and commit; each database read, its release, whether \
        \copied, edited, allocated otherwise or patched; method collection, \
        \whether a modified copy; swaps and parameter values sent). Quote \
        \it with the number: scores compare only on the same basis."
            <> webUrlTip
    ScoreActivities ->
        "LCA / ACV: rank N processes on one \
        \scoring set, one call. Columnar JSON \
        \{scoring_set, scoring_unit, functional_unit?, columns, rows, not_found, \
        \invalid, unscorable, unsolvable, cutoff_notice?, \
        \cutoff_inputs_by_process_id?, withheld_cutoffs_by_process_id?}. \
        \'columns' heads 'rows' (one scalar array per process): ['activity_name', \
        \'process_id', 'web_url', 'total', \
        \'cutoffs', <impact category keys...>]; \
        \'cutoffs' counts the row's upstream \
        \inputs no loaded database supplies \
        \(scored zero, listed in 'cutoff_inputs_by_process_id'). \
        \'functional_unit' ({amount: 1, unit, product}: every score is for one \
        \unit of the reference product) top-level when all rows share it, else \
        \a column holding that object per row. No \
        \per-impact-category scores: score_activity \
        \gives them. Unresolved IDs: not_found / \
        \invalid; refused allocation: unscorable; no solver answer (a loop \
        \consuming all it makes): unsolvable; one line per reason with its \
        \process_ids, the rest still ranked. 'basis' once for all (see \
        \score_activity); a row's parameter set is a position in 'parameterSets'. \
        \Set that weighs (list_scoring_sets): quote 'reading_notice' with any \
        \comparison. Method collection with several sets: pass scoring_sets: \
        \[\"<one>\"]. 'parameter_sets': each process scored at each set, \
        \'parameter_set' column after 'process_id'."
            <> webUrlTip
    ListScoringSets ->
        "LCA / ACV: formula-based scoring sets of loaded \
        \method collections, each aggregating impact category \
        \scores into weighted or normalized 'score' values \
        \(e.g. single score plus per-area-of-protection sub-scores). Per set: \
        \name, display unit, variables (each \
        \with its impact category), computed \
        \intermediates, display labels (variable \
        \→ impact category name), normalization \
        \and weighting factors, score formulas; the same set as 'rows' (label, \
        \impact categories with coefficient or \
        \formula, normalization, weight) with \
        \'sum_of_rows', scores adding up the rows, which a new row joins. \
        \'weighs': scores rest on value choices (an impact \
        \category weighed or counted otherwise than once, or \
        \normalized impact categories added), which ISO 14044 \
        \forbids in a comparison disclosed to the public. Set names key \
        \score_activity / score_activities answers; edit_scoring_sets changes rows \
        \by variable."
    GetGapReport ->
        "LCA / ACV: supplier gaps of a database: input demands still unsupplied \
        \after internal resolution and cross-database linking, per (product, \
        \supplier asked for, location, unit), ranked by demanding edges. Each \
        \gap: every blocking reason, consumer edge and distinct consumer counts, \
        \total demand, top consumers. 'What is missing to switch or complete this \
        \database's background?', typically after a relink."
    GetQualityReport ->
        "LCA / ACV: soundness report of a database, staged or loaded, for those \
        \who build or repair one: structural defects a score cannot show. Checks: \
        \reference exchange not exactly one; allocation shares not summing to \
        \100%, on some coproducts only, or outside 0-100%; outright duplicates \
        \(name, location, reference product); non-finite amounts, zero reference \
        \amount; missing metadata (description, classification, location, unit \
        \unknown to the registry); undeclared geography, read off the name (SimaPro \
        \writes 'Unspecified' in whole databases) or filled by the loader; amounts \
        \disagreeing with their formulas, unevaluable formulas \
        \(mathematicalRelation); names merging under SimaPro's 80-character cut; \
        \exchanges missing the pedigree their database carries; reference products \
        \nothing consumes (expected for final products); unbalanced land \
        \transformation 'to'/'from' areas; impossible oxygen-demand or \
        \organic-carbon order (BOD5 > COD, dissolved > total organic carbon); \
        \failing CAS check digit. Finding: severity (danger, warning, info), \
        \process, detail. get_gap_report answers 'what is missing?'."
    GetComputedQualityReport ->
        "LCA / ACV: computed checks of a LOADED database, judged against the \
        \catalogue's own norms. Scores every process with \
        \one method collection (the only loaded one, or \
        \'collection'): per-impact-category outliers \
        \on a log scale by median/MAD within (impact \
        \category, reference unit) groups (a \
        \mg-read-as-kg slip lands three orders out); entries scoring zero \
        \everywhere (empty or uncharacterized inventory); negative scores (info: \
        \fine where avoided-production credits or waste treatment dominate). \
        \get_quality_report checks what is STORED, staged databases too; this \
        \needs matrices and a loaded method collection. Same finding shape."
    GetCoverageReport ->
        "LCA / ACV: characterization coverage of a database against \
        \loaded method collections, for maintainers. Lists flows an \
        \impact category scores ONLY through a name bridge (synonym \
        \or CAS): VoLCA scores them, a tool matching factors \
        \by exact name scores them zero, silently. Grouped under the impact \
        \category's own name (rename target). One entry per method \
        \collection, so two versions compare side by side; optionally one."
    EditExchanges ->
        "LCA / ACV: change what one process consumes and \
        \emits, or its name, location, description, \
        \keeping the process: the tool that writes data. \
        \Adapts an imported process to the \
        \study: drop an out-of-scope substance, \
        \fix an amount, add a missing supplier, fix name or location, give a line \
        \a formula, set or remove a process parameter. Unnamed parts stay \
        \(classification, synonyms, pedigree, coproducts). Inventory side only: \
        \input by its supplier's process_id, waste output by \
        \treatment process_id, emission or resource line by flow \
        \id (get_activity); reference product and coproducts \
        \unreachable (they make the process what it is). Selector \
        \matching nothing: refused; matching several lines: all edited, count \
        \given. Formulas in the engine's language (get_formula_language; \
        \check_formula tries one). After all changes, a line whose formula no \
        \longer gives its amount takes the formula's value, listed under \
        \restated. Refused on databases read from the engine's configuration \
        \(shared background): copy first. 'transient': in memory only, an unload \
        \undoes it."
    EditActivities ->
        "edit_exchanges on several processes as one change: all edits judged \
        \first, all refused if one is; database rebuilt and relinked once (most of \
        \an edit's cost when large). Each element of edits: {process_id} plus \
        \edit_exchanges' arguments, no note (one note for the change). Each \
        \activity once per request, two products of one activity included. Answer: \
        \each process's report in request order, then transient and warnings \
        \once."
    SupplyMissing ->
        "Give a missing product a supplier for every process lacking it: each \
        \unanswered input asking for this product (and \
        \naming 'supplier_activity', if given) moves to the \
        \new supplier, keeping amount, formula, comment, as \
        \edit_activities' set_suppliers would. 'product' and \
        \'supplier_activity': name and supplier_activity of a \
        \get_gap_report gap. 'provider': a process_id here or \
        \in a dependency, making the product in a unit the \
        \lines convert from; one line it cannot supply refuses the whole change. \
        \Answers as edit_activities."
    DatabaseHistory ->
        "Every change to a database of one's own, oldest first: when; what, in \
        \words (processes added, rewritten, deleted; exchanges edited, amounts \
        \before and after; parameters changed); author's reason, if any; the \
        \'database' it was made on. A child lists its parent's changes first (its \
        \data carry them). Databases read from the engine's configuration have \
        \none."
    DatabaseParameters ->
        "A database's parameters, read by every process's formulas unless the \
        \process defines its own of that name: value (or formula and its value), \
        \unit, description, uncertainty. A process's own are in get_activity."
    EditDatabaseParameters ->
        "Set or remove a database's parameters, then restate every process \
        \reading them: a line whose formula no longer gives its amount takes the \
        \formula's value; the answer lists processes that moved. Formulas in the \
        \engine's language (get_formula_language). Refused, saying why: name not \
        \one word of the language or a function's, second spelling of a parameter, \
        \non-finite value, formula that does not read, process left without value, \
        \removing a name a formula reads, moving an allocation share. Refused on \
        \databases read from the engine's configuration, as edit_exchanges."
    ListScenarios ->
        "A database's scenarios: name and parameter values, as {process?, name, \
        \value}. To score one, send its values as a scoring request's parameters. \
        \Marked shipped: come with the database (an openLCA package's parameter \
        \sets, named after their product system); others saved on it, which \
        \databases read from the configuration cannot hold. One an edit made \
        \unusable (parameter removed, say) carries the problem."
    SaveScenario ->
        "Save parameter values on a database under a name, replacing what it \
        \held; the answer lists saved scenarios. Each value {name, value}, plus \
        \process for a process's own. Refused, saying why: values a scoring \
        \request would refuse, a shipped scenario's name, a database read from the \
        \configuration (copy first)."
    DropScenario ->
        "Drop a scenario saved on a database; the answer lists those left. \
        \Refused for an unknown name and for shipped scenarios (back at every \
        \load)."
    CopyMethodCollection ->
        "LCA / ACV: copy a method collection under a new name, to change it. \
        \Declared and built-in method collections \
        \never change in place: their copies \
        \do. The copy scores as its source until changed, loaded at once, reads \
        \the source's files (source undeletable meanwhile). Named by the slug of \
        \'new_name' (lower case, dashes), given in the response; a taken name is \
        \refused."
    EditMethodFactors ->
        "LCA / ACV: in a loaded method collection \
        \of one's own (copy or upload), change, \
        \remove or add one characterization factor, or rescale or set every factor \
        \a selector reaches. 'op': set / remove name a factor by method_id \
        \(list_methods), flow_id and location \
        \(get_characterization or the impact category's factors), \
        \'value' (present value) choosing among several at that \
        \place; add names method_id and the whole factor; scale / set-all name a \
        \selector 'match'. Not exactly one factor: refused with candidates; two \
        \identical factors only by selector; a selector touching nothing: refused. \
        \Response: the journal line and factors touched. Declared \
        \method collections refused: copy_method_collection first."
    EditMethodCategories ->
        "LCA / ACV: in a loaded method collection \
        \of one's own (copy or upload), add, \
        \rename, re-unit or remove an impact category. 'op': add names 'name' and \
        \'unit' (optional 'impact_category', 'methodology'), starting with no \
        \factor (edit_method_factors adds them); rename, set-unit, remove name \
        \'method_id' (list_methods). A rename carries along scoring sets and \
        \unregionalized impact categories naming it. Refused: \
        \a name in use, removing an impact category a scoring \
        \set weighs (set named). Response: journal line and \
        \method_id."
    EditMethodScoringSets ->
        "LCA / ACV: change scoring sets of a loaded \
        \method collection of one's own (copy \
        \or upload), each named by 'set'. 'op': create from 'rows' (optional \
        \'unit'), remove, rename to 'name', set-unit to 'unit', set-multiplier to \
        \'multiplier' (none drops it); add-row a 'row', change-row / remove-row the \
        \row of 'variable' (list_scoring_sets); set-formula of a computed \
        \'variable'; set-score a 'score' to a 'formula', remove-score. Row: \
        \{label, terms: [{methodId, coefficient}], normalization, weight, unit}, \
        \impact categories by method_id (list_methods). \
        \A weighted row joins the score \
        \summing the rows; other scores left alone. Refused, in a sentence: \
        \formula naming nothing the set holds, cycle, zero normalization, removing \
        \a row a score reads otherwise. Response: journal line; undo_method_edit \
        \undoes it whole."
    UndoMethodEdit ->
        "LCA / ACV: undo a change to a method \
        \collection of one's own by writing its \
        \inverse as a new journal line. No 'line': latest change in effect, so \
        \repeated calls walk back one at a time, never into what a copy took from \
        \its source's configuration. 'line': that line, whatever it is (an undo \
        \line: redone). Selectors undone by restoring replaced values; a factor a \
        \later line changed refuses that, named."
    GetMethodHistory ->
        "LCA / ACV: a method collection's journal, one entry per change: line, \
        \when, what (impact category, factor, \
        \values before and after, selector and \
        \factors touched), whether it undoes another line, whether still in \
        \effect. Declared method collections: empty."
    SearchMethodFlows ->
        "LCA / ACV: flows a loaded method collection \
        \characterizes, once per direction \
        \and compartment, whose name holds every word of 'q' (case aside), by \
        \name. Gives flow_id and compartment for writing a new factor."

-- ---------------------------------------------------------------------------
-- Projection: parameter schema
-- ---------------------------------------------------------------------------

-- Common parameters that appear repeatedly. Defining them once reduces
-- noise in 'params' and keeps descriptions consistent across resources.

pDatabase :: Param
pDatabase = Param "database" "string" Required "Database name"

pSource :: Param
pSource = Param "source" "object" Optional "Where the change's figures come from: {document, page?, passage?}: the paper or report, page counted from 1, the words relied on. Covers the whole call (one page per exchange: one call each). Kept in the history (get_database_history) and the export's changes report."

pProcessId :: Param
pProcessId =
    Param
        "process_id"
        "string"
        Required
        "Process ID (activityUUID_productUUID format)"

pMethodId :: Param
pMethodId = Param "method_id" "string" Required "Impact category UUID"

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
        "Method collection (list_methods). Only when the impact \
        \category UUID is loaded in several method collections \
        \(e.g. two EF 3.1 versions); an ambiguous UUID fails \
        \listing them."

-- | The collection a scoring set is configured on.
pScoreCollection :: Param
pScoreCollection = Param "collection" "string" Required "Method collection name the scoring set is configured on (from list_scoring_sets)"

pScoringSet :: Param
pScoringSet = Param "scoring_set" "string" Required "Scoring set name (from list_scoring_sets)"

pScore :: Param
pScore = Param "score" "string" Required "Name of one score of that scoring set (from list_scoring_sets)"

pAttribution :: Param
pAttribution = Param "attribution" "string" Optional "first-match: a group counts the whole chain below its first process met; own: its processes' own emissions, the only reading a group keeping a substance ('flow') can have. Left out: first-match, or own when a group keeps a substance"

pGroups :: Param
pGroups =
    Param
        "groups"
        "array"
        Required
        ( "Groups in order, at most "
            <> T.pack (show maxGroups)
            <> ", names unique: {name, filter: {name?, product?, location?, \
               \classifications?: [{system, value, mode}]}}. name and product: contained, case aside; location: \
               \that place or one within it; mode: contains, exact, starts-with (49 takes 4923), or a not- form, \
               \which leaves out what it matches. A filter keeps something: name, product, location or a \
               \keeping classification."
        )

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
        "Supplier swaps, not saved in the data, each {from: \
        \oldSupplierPID, to: newSupplierPID, consumer: consumerPID}; \
        \a PID bare (root database) or dbName::pid (other \
        \database)."

{- | Optional parameter values: the database at these values is the one the
request reads, and substitutions are made on it.
-}
pParameters :: Param
pParameters =
    Param
        "parameters"
        "array"
        Optional
        "Input parameter values, as get_activity lists them: {name, value} for the \
        \database's, {process, name, value} for a process's own. Exchanges whose \
        \formulas read them are recomputed first."

{- | Optional sets of parameter values: each activity of a batch is scored
once at each of them.
-}
pParameterSets :: Param
pParameterSets =
    Param
        "parameter_sets"
        "array"
        Optional
        "Sets of input parameter values, each a list shaped as get_impacts' \
        \'parameters'. Every process is scored \
        \at each set in turn; without it, \
        \at the database's own values."

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
        "True: score without delayed long-term (> 100 yr) emissions, as out of \
        \scope. They are never resources, so regionalized water and land are \
        \unaffected. Default false (kept, the ecoinvent/EF convention)."

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
        "Scoring set names (list_scoring_sets). score_activity: keeps only these \
        \in scoringResults / scoringUnits / scoringIndicators. score_activities: \
        \the one set to rank on, one name, auto-picked when the method collection has \
        \one. An unknown name fails, listing the names."

-- | Opt-in summary mode for 'score_activities'.
pSummaryOnly :: Param
pSummaryOnly =
    Param
        "summary_only"
        "boolean"
        Optional
        "True: one 'dominant_indicator' column \
        \instead of one per scoring-set column, \
        \cells {key, share_pct} (e.g. {\"key\": \"ldu\", \
        \\"share_pct\": 82.3}), the column (an impact category or \
        \a computed variable) with the largest absolute share of the row's total. For large \
        \batches before score_activity on one PID. Default false."

-- | Parameters accepted by a resource operation.
params :: Resource -> [Param]
params r = case r of
    ListDatabases -> []
    LoadDatabase -> [pDatabase]
    UnloadDatabase -> [pDatabase]
    AcceptTerms -> [pDatabase]
    DeriveDatabase ->
        [ pDatabase
        , Param "new_name" "string" Required "Name to register the new database under. Must not already exist."
        , Param "parent" "boolean" Optional "If true, make a parent: the source's files read again with each block of several products kept whole. Give this or system_model, not both."
        , Param "system_model" "string" Optional "The text of a system model's TOML file, to make a child of a parent. Example: name = \"Agribalyse, economic\"  version = \"1.0.0\"  [allocation] default = \"price\"  [[allocation.exceptions]] key = \"wet mass\"  reason = \"...\"  classification = { system = \"Category\", value = \"Agricultural\\\\Food\\\\Cheese production\" } (processes = [\"<activity id>\", ...] instead of classification for a few processes)."
        ]
    ListPresets -> []
    SearchActivities ->
        [ pDatabase
        , Param "name" "string" Optional "Words in the language of the database's names (usually English), or the exact name with exact=true. A source file's dataset identifier (SimaPro 'Process identifier'), whole or its four or more distinguishing characters, puts that dataset's products first. Omitted: the other filters alone choose."
        , Param "geo" "string" Optional "Location (e.g. 'FR', 'DE', 'GLO') and every location inside it ('US' matches 'US-WECC', 'RER' matches 'FR'), per the engine's location table, never the spelling: 'GL' does not match 'GLO'."
        , Param "product" "string" Optional "Product name filter"
        , Param "exact" "boolean" Optional "True: the name equals (case aside) instead of containing, and geo is that location alone, not the ones inside it"
        , Param "preset" "string" Optional "Classification preset (list_presets), expanded to its filters; combines with explicit classification filters."
        , Param "classification" "string" Optional "Classification system (e.g. 'ISIC rev.4 ecoinvent', 'CPC'; list_classifications)."
        , Param "classification_value" "string" Optional "Value within the classification system to match"
        , Param "classification_match" "string" Optional "Match mode, case aside: contains (substring, default), exact (or equals: the whole value), starts-with (a code and the codes under it: 65 takes 6511 but not 2365), not-contains, not-exact or not-starts-with (leave out what matches). Any other word is refused."
        , Param "classifications" "array" Optional "Several classification filters, each {system, value, mode}, mode as classification_match gives it (default contains); instead of classification, classification_value and classification_match, not with them. Filters on one system keep what any of them matches; every not- filter leaves out what it matches."
        , Param "exclude_name" "array" Optional "Names to leave out: each removes the processes a search for that name alone finds. Blank ones are dropped."
        , Param "exclude_location" "array" Optional "Locations to leave out: each removes every process located there or inside it. Blank ones are dropped."
        , Param "process" "array" Optional "Process IDs (activityUUID_productUUID) to list instead of searching, in the order given, through the geo, product, exact and classification filters; name and sort ignored. An ID not held is left out; a malformed one, or a bare activity UUID naming several processes, is refused."
        , pLimit "Max results (default 20)"
        ]
    SearchFlows ->
        [ pDatabase
        , Param "query" "string" Required "Flow name, formula or CAS to search for"
        , Param "kind" "string" Optional "Keep only these kinds of flow: technosphere | biosphere | waste. Name several separated by commas, as in \"biosphere,waste\". Omit for all three."
        , pLimit "Max results (default 20)"
        ]
    GetFlowActivities ->
        [ pDatabase
        , Param "flow_id" "string" Required "Database flow UUID, as returned by search_flows"
        , Param "role" "string" Optional "producer | consumer | any (default)"
        , pLimit "Max processes listed (default 50)"
        ]
    GetActivity ->
        [ pDatabase
        , pProcessId
        , Param "exchange_type" "string" Optional "\"technosphere\" (products, inputs), \"biosphere\" (emissions, resources), \"waste\" (residuals sent to treatment) or \"all\" (default)"
        , Param "is_input" "boolean" Optional "True: inputs only; false: outputs only; omitted: both. Combines with exchange_type."
        , Param "flow" "string" Optional "Flow name or synonym, read as search_flows does: every word, case-blind, any order, punctuation optional"
        ]
    Aggregate ->
        [ pDatabase
        , pProcessId
        , Param "scope" "string" Required "direct | supply_chain | biosphere | consumption"
        , Param "is_input" "boolean" Optional "Only for scope=direct: true=inputs only, false=outputs only"
        , Param "max_depth" "integer" Optional "Only for scope=supply_chain: max hops from the root process"
        , Param "filter_name" "string" Optional "Case-insensitive substring on flow or process name"
        , Param "filter_name_not" "string" Optional "Comma-separated substring exclude list"
        , Param "filter_unit" "string" Optional "Whole unit name, case-insensitive: \"MJ\" and \"mj\" are the same unit, \"kg\" does not match \"kgm\""
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets): expanded and merged into filter_classification."
        , Param "filter_classification" "array" Optional "List of \"System=Value[:exact|:starts-with]\" strings; defaults to 'contains' mode"
        , Param "filter_target_name" "string" Optional "Only for scope=direct on consumed products, or scope=consumption: filter by supplier name"
        , Param "filter_consumer" "string" Optional "Only for scope=consumption: case-insensitive substring on the consumer's name"
        , Param "filter_consumer_not" "string" Optional "Only for scope=consumption: comma-separated consumer-name exclude list (each item is a substring; a name containing a comma cannot be expressed)"
        , Param "filter_is_reference" "boolean" Optional "Filter by reference-product flag (typically for outputs)"
        , Param "group_by" "string" Optional "name | flow_id | name_prefix | unit | classification.<system> | location | target_name | consumer_name"
        , Param "aggregate" "string" Optional "sum_quantity | count | share (default: sum_quantity)"
        ]
    GetSupplyChain ->
        [ pDatabase
        , pProcessId
        , Param "name" "string" Optional "Filter by process name"
        , Param "location" "string" Optional "Filter by geography/location: that location, plus every location inside it ('US' also matches 'US-WECC'), case-insensitive"
        , pLimit "Max results (default 100)"
        , Param "min_quantity" "number" Optional "Min 'quantityPerUnit', absolute value: entries at or below it are dropped"
        , Param "max_depth" "integer" Optional "Max depth from root (1 = direct inputs only)"
        , Param "preset" "string" Optional "Classification preset (list_presets), expanded to its filters; unioned with explicit classification filters."
        , Param "classification" "string" Optional "Classification system name (e.g. 'Category', 'Category type')"
        , Param "classification_value" "string" Optional "Value within the classification system"
        , Param "classification_match" "string" Optional "Match mode, case aside: contains (substring, default), exact (or equals: the whole value), starts-with (a code and the codes under it: 65 takes 6511 but not 2365), not-contains, not-exact or not-starts-with (leave out what matches). Any other word is refused."
        , pSubstitutions
        , pParameters
        ]
    GetInventory ->
        [ pDatabase
        , pProcessId
        , Param "flow" "string" Optional "Flow name or synonym, read as search_flows does: every word, case-blind, any order, punctuation optional"
        , pLimit "Max flows, largest absolute amount first. Default: 50 over MCP, all over HTTP"
        , pSubstitutions
        , pParameters
        ]
    GetImpacts ->
        [ pDatabase
        , pProcessId
        , pMethodId
        , pCollection
        , Param "top_flows" "integer" Optional "Number of top contributing flows to return (default 5)"
        , pSubstitutions
        , pParameters
        , pExcludeLongTerm
        , Param "include_diagnostics" "boolean" Optional "True: uncharacterized flows above 0.1% of total |amount|, each with up to 3 similar candidate CFs (PubChem-expanded Jaccard + CAS), to tell impact category gaps from mapping bugs."
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
            "Perturbations, each scored on its own: {consumer: ProcessId, supplier: \
            \ProcessId, delta: number, label?: string}. delta is RELATIVE: A_ij × \
            \(1+delta); +0.05 for +5%, -1 removes the link. Both processes in the root \
            \database. label is echoed back."
        ]
    GetReducedModel ->
        [ pDatabase
        , pProcessId
        , pCollection
        , Param
            "parameters"
            "array"
            Required
            "The input parameters the model is a function of, as get_activity lists them. \
            \Each entry: {name} for a parameter of the database, {process, name} for a \
            \process's own."
        , pExcludeLongTerm
        ]
    GetFormulaLanguage -> []
    CheckFormula ->
        [ pDatabase
        , Param "formula" "string" Required "The formula, in VoLCA's language (get_formula_language)"
        , Param "process_id" "string" Optional "The process whose parameters and named lines the formula reads; the database's parameters alone when absent"
        , Param "exchange" "integer" Optional "The line the formula would be written on, by its position among the process's exchanges as get_activity lists them"
        ]
    ListMethods -> []
    GetFlowMapping ->
        [ pDatabase
        , pMethodId
        , pCollection
        , Param "verbose" "boolean" Optional "True: beyond coverage stats, unmatched_cfs (CFs with no database flow) and unmatched_db_flows (ranked when process_id is set)."
        , Param "process_id" "string" Optional "Ranks unmatched_db_flows by their share of this process's inventory."
        , Param "max_unmatched" "integer" Optional "Cap on each unmatched list (default 50)"
        ]
    GetCharacterization ->
        [ pDatabase
        , pMethodId
        , pCollection
        , Param "flow" "string" Optional "Filter by flow name (case-insensitive substring, matches both the impact category's CF name and the database flow name)"
        , pLimit "Max results (default 20)"
        ]
    ExplainCF ->
        [ pDatabase
        , pMethodId
        , pCollection
        , Param "flow_id" "string" Required "Database flow UUID, as returned by search_flows or in the flow_id field of get_contributing_flows"
        ]
    GetFlowFactors ->
        [ pDatabase
        , Param "flow_id" "string" Required "Database flow UUID, as returned by search_flows or in the flow_id field of get_contributing_flows"
        , Param "collection" "string" Optional "Method collection name (from list_methods) to ask alone; every loaded method collection when omitted"
        ]
    GetContributingFlows ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "UUID of the impact category"
        , pCollection
        , pLimit "Max flows to return, sorted by contribution (default 20)"
        , pExcludeLongTerm
        , Param "include_diagnostics" "boolean" Optional "True: uncharacterized flows above 0.1% of total |amount|, each with up to 3 similar candidate CFs (PubChem-expanded Jaccard + CAS)."
        ]
    GetContributingActivities ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "UUID of the impact category"
        , pCollection
        , pLimit "Max processes to return, sorted by contribution (default 10)"
        , pExcludeLongTerm
        ]
    GetGroupContributions ->
        [ pDatabase
        , pProcessId
        , Param "collection" "string" Required "Method collection name (list_methods)"
        , pAttribution
        , pGroups
        , pExcludeLongTerm
        ]
    GetGroupMembers ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "UUID of the impact category"
        , pCollection
        , pAttribution
        , pGroups
        , Param "group" "string" Optional "Name of the group to list; left out, the rest"
        , pLimit "Max processes to return, sorted by contribution (default 10)"
        , pExcludeLongTerm
        ]
    GetGroupFlows ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "UUID of the impact category"
        , pCollection
        , pAttribution
        , pGroups
        , Param "group" "string" Optional "Name of the group to list; left out, the rest"
        , pLimit "Max flows to return, sorted by contribution (default 20)"
        , pExcludeLongTerm
        ]
    GetGroupCandidates ->
        [ pDatabase
        , pProcessId
        , Param "method_id" "string" Required "UUID of the impact category"
        , pCollection
        , pAttribution
        , pGroups
        , Param "candidate" "string" Optional "Process to group by, as get_group_members writes its id (db::id for another database)"
        , Param "candidate_flow" "string" Optional "Flow to group by, as get_contributing_flows writes its flow_id"
        , pExcludeLongTerm
        ]
    GetScoreContributingFlows ->
        [ pDatabase
        , pProcessId
        , pScoreCollection
        , pScoringSet
        , pScore
        , pLimit "Max flows to return, sorted by contribution (default 20)"
        , pExcludeLongTerm
        ]
    GetScoreContributingActivities ->
        [ pDatabase
        , pProcessId
        , pScoreCollection
        , pScoringSet
        , pScore
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
    ListGroupSets ->
        [ pDatabase
        ]
    GetPathTo ->
        [ pDatabase
        , pProcessId
        , Param "target" "string" Required "Case-insensitive name substring to stop at"
        , pSubstitutions
        , pParameters
        ]
    CountSearchMatches ->
        [ pDatabase
        , Param "query" "string" Required "The search term to count matches for"
        ]
    GetConsumers ->
        [ pDatabase
        , Param "process_id" "string" Required "Process ID of the supplier (activityUUID_productUUID format)"
        , Param "name" "string" Optional "Filter by name (case-insensitive substring)"
        , Param "location" "string" Optional "Filter by geography/location (e.g. 'FR', 'DE'): that location, plus every location inside it ('RER' also matches 'FR'), case-insensitive"
        , Param "product" "string" Optional "Filter by product name (case-insensitive substring)"
        , Param "preset" "string" Optional "Name of a classification preset (from list_presets): expands to its bundled filters"
        , Param "classification" "string" Optional "Classification system name (e.g. 'ISIC rev.4 ecoinvent')"
        , Param "classification_value" "string" Optional "Classification value substring to match"
        , Param "classification_match" "string" Optional "Match mode, case aside: contains (substring, default), exact (or equals: the whole value), starts-with (a code and the codes under it: 65 takes 6511 but not 2365), not-contains, not-exact or not-starts-with (leave out what matches). Any other word is refused."
        , pLimit "Max results (default 1000)"
        , Param "max_depth" "integer" Optional "Max hops from supplier (1 = direct consumers only)"
        , Param "include_edges" "boolean" Optional "True: also every supplier → consumer edge between processes reachable from the supplier, to rebuild paths without get_path_to."
        ]
    CompareImpacts ->
        [ Param "database_a" "string" Required "First database name"
        , Param "process_id_a" "string" Required "Process ID in database_a (activityUUID_productUUID format)"
        , Param "method_id_a" "string" Required "Impact category UUID for the A side"
        , Param "collection_a" "string" Optional "Method collection for the A side; needed only when method_id_a is loaded in more than one method collection."
        , Param "database_b" "string" Required "Second database name"
        , Param "process_id_b" "string" Required "Process ID in database_b (activityUUID_productUUID format)"
        , Param "method_id_b" "string" Required "Impact category UUID for the B side"
        , Param "collection_b" "string" Optional "Method collection for the B side; needed only when method_id_b is loaded in more than one method collection."
        , Param "top_flows" "integer" Optional "Per-side flow drill-down depth (default 10)"
        ]
    CompareActivities ->
        [ pDatabase
        , pProcessId
        , Param "other_process_id" "string" Required "Process ID of the process to compare against (activityUUID_productUUID format)"
        , Param "other_database" "string" Optional "Loaded database holding that process, when it is not this one"
        ]
    CompareDatabases ->
        [ pDatabase
        , Param "other_database" "string" Required "Loaded database to compare against, usually the newer version"
        , pLimit "Max entries per list (added, removed, changed, redated, ambiguous, unchanged), in name order (default: all). The counts always cover the full lists."
        ]
    CompareMethodCollections ->
        [ Param "collection" "string" Required "Base method collection, usually the older version"
        , Param "other_collection" "string" Required "Loaded method collection to compare against"
        , Param "pairs" "array" Optional "Impact categories to pair by hand, each written \"base=other\" with the two impact category names; taken before any other pairing"
        , Param "category" "string" Optional "Compare only the pair whose base impact category has this name, which is much faster than comparing them all; the unpaired and ambiguous impact categories are still listed"
        , pLimit "Max entries per list of each impact category (added, removed, changed, ambiguous, unconvertible), in order (default: all). The counts always cover the full lists."
        ]
    ProfileMethodCollection ->
        [ Param "collection" "string" Required "Loaded method collection to profile"
        ]
    ScoreActivity ->
        [ pDatabase
        , pProcessId
        , Param "collection" "string" Required "Method collection name (use list_methods to discover)"
        , pSubstitutions
        , pParameters
        , pExcludeLongTerm
        , pScoringSetsFilter
        , Param "exchange_contributions" "boolean" Optional "True: each impact category's result lists what each of the process's lines adds ('exchangeContributions': position as get_activity numbers exchanges, contribution, share), largest first, summing to the score. Scoring-set scores not split. Not with 'substitutions'. Empty when the licence keeps detail or inventory to itself. Default false."
        ]
    ScoreActivities ->
        [ pDatabase
        , Param "collection" "string" Required "Method collection name"
        , Param "process_ids" "array" Required "Process IDs to score (activityUUID_productUUID). All resolved in one multi-RHS solve."
        , pScoringSetsFilter
        , pSummaryOnly
        , pExcludeLongTerm
        , pParameterSets
        ]
    ListScoringSets ->
        [ Param "collection" "string" Optional "Method collection name. If omitted, returns scoring sets across all loaded method collections, grouped by method collection."
        ]
    GetGapReport ->
        [ pDatabase
        , pLimit "Max gaps, biggest first (default all); header counts cover the full report."
        ]
    GetQualityReport ->
        [ pDatabase
        , pLimit "Max findings per check, worst first (default all); offenderCount covers the full list."
        ]
    GetComputedQualityReport ->
        [ pDatabase
        , Param "collection" "string" Optional "Method collection to score with; required when several are loaded."
        , pLimit "Max findings per check, worst first (default all); offenderCount covers the full list."
        ]
    GetCoverageReport ->
        [ pDatabase
        , Param "collection" "string" Optional "One loaded method collection (list_methods); every one when omitted."
        , pLimit "Max bridge groups per method collection (default all); bridgeGroupCount covers the full list."
        ]
    EditExchanges ->
        [ pDatabase
        , pProcessId
        , Param "remove" "array" Optional "Lines to drop, each by the selector get_activity gives: {kind, provider|flow, location?}; kind \"input\" or \"waste\" with the supplier's process_id (or flow id and location for a line naming no supplier), or \"biosphere\" with the flow id."
        , Param "set_amounts" "array" Optional "Lines to restate: {select: {kind, provider|flow, location?}, amount}."
        , Param "add_inputs" "array" Optional "Consumed products to add: {provider, amount, unit?, comment?}, provider being the supplier process; the flow follows from it."
        , Param "add_biosphere" "array" Optional "Emission and resource lines to add: {direction, amount} plus flow (an existing flow id) or name + compartment + unit, reaching the flow the database declares under them, new only when none does."
        , Param "add_waste_outputs" "array" Optional "Waste outputs to add: {provider, amount, unit?, comment?}, provider being the treatment process."
        , Param "set_name" "string" Optional "The new activity_name."
        , Param "set_location" "string" Optional "The process's new location code (e.g. FR)."
        , Param "set_description" "array" Optional "The process's new description, one string per paragraph; [] clears it."
        , Param "set_parameters" "array" Optional "Process parameters to add, or replace whole under the same spelling (unit, description or uncertainty left out is dropped): {name} plus value (uncertainty?) or formula, unit?, description?."
        , Param "set_formulas" "array" Optional "Lines to give a formula: {select: {kind, provider|flow, location?}, formula}; null drops the formula, keeping the amount."
        , Param "set_prices" "array" Optional "Product prices, for the price allocation key: {product, price}, product the flow id of the reference product or a coproduct, price {amount, unit} with the currency as unit, or null to remove."
        , Param "set_dry_masses" "array" Optional "Product dry masses, for the dry mass key: {product, dryMass}, product as in set_prices, dryMass {amount, unit} per unit of product in a mass unit, or null to remove."
        , Param "set_wet_masses" "array" Optional "Product wet masses, for the wet mass key: {product, wetMass}, as set_dry_masses. On a product in litres or cubic metres it is the density, multiplied by the line's amount, which alone cannot be weighed."
        , Param "set_suppliers" "array" Optional "Inputs to move to another supplier, linked or not: {select: {kind: \"input\", provider|flow, location?}, provider}, provider the new supplier's process_id here or in a dependency. The line keeps amount, unit, formula, comment, pedigree."
        , Param "remove_parameters" "array" Optional "Process parameters to remove, once the formulas set here no longer read them. Refused while a formula reads one the database does not define."
        , Param "note" "string" Optional "Why, in a sentence. Kept in the history (get_database_history) and the export's changes report."
        , pSource
        ]
    SupplyMissing ->
        [ pDatabase
        , Param "product" "string" Required "The missing product: the name of a gap get_gap_report lists."
        , Param "supplier_activity" "string" Optional "The supplier_activity of that gap, when it names one. Absent: the inputs that name none."
        , Param "provider" "string" Required "The process_id of the process that will supply the product, in this database or one it depends on."
        , Param "note" "string" Optional "Why, in a sentence. Kept in the history (get_database_history) of every process it edits."
        , pSource
        ]
    EditActivities ->
        [ pDatabase
        , Param "edits" "array" Required "One element per process: {process_id} plus the arguments of edit_exchanges (remove, set_amounts, add_inputs, add_biosphere, add_waste_outputs, set_name, set_location, set_description, set_parameters, set_formulas, set_prices, set_dry_masses, set_wet_masses, set_suppliers, remove_parameters), each as edit_exchanges reads it."
        , Param "note" "string" Optional "Why, in a sentence, for every process edited. Kept in the history (get_database_history) and the export's changes report."
        , pSource
        ]
    DatabaseParameters -> [pDatabase]
    DatabaseHistory -> [pDatabase]
    EditDatabaseParameters ->
        [ pDatabase
        , Param "set_parameters" "array" Optional "Parameters of the database to add, or to replace under the same spelling. Each is {name} plus either value (with optional uncertainty) or formula, and optional unit and description."
        , Param "remove_parameters" "array" Optional "Names of the database's parameters to remove, after the ones set here. Refused while a formula still reads one."
        , Param "note" "string" Optional "Why, in a sentence. Kept in the history (get_database_history) and the export's changes report."
        , pSource
        ]
    ListScenarios -> [pDatabase]
    SaveScenario ->
        [ pDatabase
        , Param "name" "string" Required "Name of the scenario"
        , Param "parameters" "array" Required "Values it sets, each {name, value} and process for a process's own parameter"
        ]
    DropScenario ->
        [ pDatabase
        , Param "name" "string" Required "Name of the scenario"
        ]
    CopyMethodCollection ->
        [ Param "collection" "string" Required "Method collection to copy"
        , Param "new_name" "string" Required "Name of the copy; it is known by its slug"
        ]
    EditMethodFactors ->
        [ Param "collection" "string" Required "Loaded method collection of one's own: a copy or an upload"
        , Param "op" "string" Required "set, remove, add, scale or set-all"
        , Param "method_id" "string" Optional "The impact category (from list_methods), for set, remove and add"
        , Param "flow_id" "string" Optional "The factor's flow, for set and remove"
        , Param "location" "string" Optional "The factor's location, for set and remove; absent for a factor written for no location"
        , Param "value" "number" Optional "The factor's present value, for set and remove, when several factors answer at that place"
        , Param "new_value" "number" Optional "The value to write, for set and set-all"
        , Param "scale" "number" Optional "The factor to multiply by, for scale"
        , Param "match" "object" Optional "The selector, for scale and set-all: {category, flowName, flowNamePrefix, cas, subcompartmentContains}, at least one of them"
        , Param "factor" "object" Optional "The factor to add: {flowId, name, direction (Input or Output), value, unit, compartment {medium, subcompartment, qualifier}, cas, location}"
        ]
    EditMethodCategories ->
        [ Param "collection" "string" Required "Loaded method collection of one's own: a copy or an upload"
        , Param "op" "string" Required "add, rename, set-unit or remove"
        , Param "method_id" "string" Optional "The impact category (from list_methods), for rename, set-unit and remove"
        , Param "name" "string" Optional "The impact category's name, for add and rename"
        , Param "unit" "string" Optional "The impact category's unit, for add and set-unit"
        , Param "impact_category" "string" Optional "The group label the impact category carries; its own name when absent"
        , Param "methodology" "string" Optional "The methodology of a new impact category"
        ]
    EditMethodScoringSets ->
        [ Param "collection" "string" Required "Loaded method collection of one's own: a copy or an upload"
        , Param "op" "string" Required "create, remove, rename, set-unit, set-multiplier, add-row, change-row, remove-row, set-formula, set-score or remove-score"
        , Param "set" "string" Required "The scoring set (from list_scoring_sets); for create, the new set's name"
        , Param "name" "string" Optional "The set's new name, for rename"
        , Param "unit" "string" Optional "The set's unit, for create and set-unit"
        , Param "multiplier" "number" Optional "The display multiplier, for set-multiplier; absent to drop it"
        , Param "variable" "string" Optional "The row or computed variable (from list_scoring_sets), for change-row, remove-row and set-formula"
        , Param "formula" "string" Optional "The formula, for set-formula and set-score"
        , Param "score" "string" Optional "The score's name, for set-score and remove-score"
        , Param "row" "object" Optional "The row, for add-row and change-row: {label, terms: [{methodId, coefficient}], normalization, weight, unit}"
        , Param "rows" "array" Optional "The rows of a new set, for create"
        ]
    UndoMethodEdit ->
        [ Param "collection" "string" Required "Loaded method collection of one's own"
        , Param "line" "integer" Optional "The journal line to undo (from get_method_history); the latest change in effect when absent"
        ]
    GetMethodHistory ->
        [ Param "collection" "string" Required "Method collection name"
        ]
    SearchMethodFlows ->
        [ Param "collection" "string" Required "Loaded method collection"
        , Param "q" "string" Optional "Words the flow name holds; every flow when absent"
        , pLimit "Max flows to return, in name order (default: 50)"
        ]

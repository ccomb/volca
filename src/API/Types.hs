{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types where

import Types (Exchange, Flow, UUID, Unit)
import API.JsonOptions (strippedParseJSON, strippedToEncoding, strippedToJSON)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Servant.API.ContentTypes (MimeRender(..), OctetStream)

-- | Search response combining results and count
data SearchResults a = SearchResults
    { srResults :: [a] -- The actual search results
    , srTotal :: Int -- Total count of all matching items (before pagination)
    , srOffset :: Int -- Starting offset for pagination
    , srLimit :: Int -- Maximum number of results requested
    , srHasMore :: Bool -- Whether there are more results available
    , srSearchTimeMs :: Double -- Search execution time in milliseconds
    }
    deriving (Generic)

-- | Minimal activity information for navigation
data ActivitySummary = ActivitySummary
    { prsProcessId :: Text  -- ProcessId format: activity_uuid_product_uuid
    , prsName :: Text
    , prsLocation :: Text
    , prsProduct :: Text  -- Reference product name
    , prsProductAmount :: Double  -- Reference product amount
    , prsProductUnit :: Text  -- Reference product unit name
    }
    deriving (Generic)

-- | Consumer result — ActivitySummary enriched with BFS depth from the queried supplier
data ConsumerResult = ConsumerResult
    { crProcessId     :: Text
    , crName          :: Text
    , crLocation      :: Text
    , crProduct       :: Text
    , crProductAmount :: Double
    , crProductUnit   :: Text
    , crDepth         :: Int   -- hops from the queried supplier (1 = direct consumer)
    }
    deriving (Generic)

-- | Enhanced flow information for search results (now includes synonyms)
data FlowSearchResult = FlowSearchResult
    { fsrId :: UUID
    , fsrName :: Text
    , fsrCategory :: Text
    , fsrUnitName :: Text
    , fsrSynonyms :: M.Map Text [Text]  -- Synonyms by language (converted from Set to List for JSON)
    }
    deriving (Generic)

-- | Inventory export data structures
data InventoryExport = InventoryExport
    { ieMetadata :: InventoryMetadata
    , ieFlows :: [InventoryFlowDetail]
    , ieStatistics :: InventoryStatistics
    }
    deriving (Generic)

data InventoryMetadata = InventoryMetadata
    { imRootActivity :: ActivitySummary
    , imTotalFlows :: Int
    , imEmissionFlows :: Int -- Biosphere outputs (negative environmental impact)
    , imResourceFlows :: Int -- Biosphere inputs (resource extraction)
    }
    deriving (Generic)

data InventoryFlowDetail = InventoryFlowDetail
    { ifdFlow :: Flow
    , ifdQuantity :: Double
    , ifdUnitName :: Text
    , ifdIsEmission :: Bool -- True for emissions, False for resource extraction
    , ifdCategory :: Text -- Flow category for grouping
    }
    deriving (Generic)

data InventoryStatistics = InventoryStatistics
    { isTotalQuantity :: Double -- Sum of absolute values
    , isEmissionQuantity :: Double -- Sum of emissions (should be positive)
    , isResourceQuantity :: Double -- Sum of resource extraction (should be positive)
    , isTopCategories :: [(Text, Int)] -- Top flow categories by count
    }
    deriving (Generic)

-- | Tree export data structures for visualization
data TreeExport = TreeExport
    { teTree :: TreeMetadata
    , teNodes :: M.Map Text ExportNode  -- Changed to Text (ProcessId format)
    , teEdges :: [TreeEdge]
    }
    deriving (Generic)

data TreeMetadata = TreeMetadata
    { tmRootId :: Text  -- Changed to Text (ProcessId format)
    , tmMaxDepth :: Int
    , tmTotalNodes :: Int
    , tmLoopNodes :: Int
    , tmLeafNodes :: Int
    , tmExpandableNodes :: Int -- Nodes that could expand further
    }
    deriving (Generic)

data ExportNode = ExportNode
    { enId :: Text  -- Changed to Text (ProcessId format)
    , enName :: Text
    , enDescription :: [Text]
    , enLocation :: Text
    , enUnit :: Text
    , enNodeType :: NodeType
    , enDepth :: Int
    , enLoopTarget :: Maybe Text  -- Changed to Text (ProcessId format)
    , enParentId :: Maybe Text -- Changed to Text (ProcessId format) -- For navigation back up
    , enChildrenCount :: Int -- Number of potential children for expandability
    , enCompartment :: Maybe Text -- Biosphere compartment (air/water/soil), only for BiosphereNodes
    }
    deriving (Generic)

data NodeType = ActivityNode | LoopNode | BiosphereEmissionNode | BiosphereResourceNode
    deriving (Eq, Show, Generic)

data EdgeType = TechnosphereEdge | BiosphereEmissionEdge | BiosphereResourceEdge
    deriving (Eq, Show, Generic)

data TreeEdge = TreeEdge
    { teFrom :: Text  -- Changed to Text (ProcessId format)
    , teTo :: Text    -- Changed to Text (ProcessId format)
    , teFlow :: FlowInfo
    , teQuantity :: Double
    , teUnit :: Text
    , teEdgeType :: EdgeType -- Type of edge (technosphere or biosphere)
    }
    deriving (Generic)

data FlowInfo = FlowInfo
    { fiId :: UUID
    , fiName :: Text
    , fiCategory :: Text
    }
    deriving (Generic)

-- | Graph export data structures for network visualization
data GraphExport = GraphExport
    { geNodes :: [GraphNode]
    , geEdges :: [GraphEdge]
    , geUnitGroups :: M.Map Text Text -- Unit to unit group mapping
    }
    deriving (Generic)

data GraphNode = GraphNode
    { gnNodeId :: Int -- Numeric ID for efficient frontend processing
    , gnLabel :: Text -- Activity name
    , gnValue :: Double -- Cumulative amount from factorized matrix
    , gnUnit :: Text -- Unit (kg, MJ, etc.)
    , gnProcessId :: Text -- Original ProcessId for linking
    , gnLocation :: Text -- Geography
    }
    deriving (Generic)

data GraphEdge = GraphEdge
    { geSource :: Int -- Source node ID
    , geTarget :: Int -- Target node ID
    , geValue :: Double -- Direct flow amount from technosphere matrix
    , geUnit :: Text -- Flow unit
    , geFlowName :: Text -- Name of the flow
    }
    deriving (Generic)

-- | Lightweight flow information for lists
data FlowSummary = FlowSummary
    { fsFlow :: Flow -- Core flow data
    , fsUnitName :: Text -- Unit name for the flow
    , fsUsageCount :: Int -- How many activities use this flow
    , fsRole :: FlowRole -- Role in this specific activity
    }
    deriving (Generic)

-- | Role of a flow in a specific activity context
data FlowRole = InputFlow | OutputFlow | ReferenceProductFlow
    deriving (Show, Generic)

-- Synonym types removed - synonyms are now included directly in flow responses

-- | Method summary for listing methods
data MethodSummary = MethodSummary
    { msmId :: UUID           -- Method UUID
    , msmName :: Text         -- Method name
    , msmCategory :: Text     -- Impact category
    , msmUnit :: Text         -- Reference unit (e.g., "kg CO2 eq")
    , msmFactorCount :: Int   -- Number of characterization factors
    , msmCollection :: Text   -- Parent collection name (e.g., "ef-31")
    }
    deriving (Generic)

-- | Method collection list response
newtype MethodCollectionListResponse = MethodCollectionListResponse
    { mclMethods :: [MethodCollectionStatusAPI]
    }
    deriving (Generic)

-- | Method collection status for API responses
data MethodCollectionStatusAPI = MethodCollectionStatusAPI
    { mcaName :: Text              -- Internal identifier
    , mcaDisplayName :: Text       -- Human-readable name
    , mcaDescription :: Maybe Text -- Optional description
    , mcaStatus :: Text            -- "loaded" | "unloaded"
    , mcaIsUploaded :: Bool        -- True if uploaded
    , mcaPath :: Text              -- Data path
    , mcaMethodCount :: Int        -- Number of impact categories (0 if unloaded)
    , mcaFormat :: Maybe Text      -- Format (e.g. "ILCD")
    }
    deriving (Generic)

-- | Reference data list response (flow synonyms, compartment mappings, units)
newtype RefDataListResponse = RefDataListResponse
    { rdlItems :: [RefDataStatusAPI]
    }
    deriving (Generic)

-- | Reference data status for API responses
data RefDataStatusAPI = RefDataStatusAPI
    { rdaName        :: Text
    , rdaDisplayName :: Text
    , rdaDescription :: Maybe Text
    , rdaStatus      :: Text  -- "loaded" | "unloaded"
    , rdaIsUploaded  :: Bool
    , rdaIsAuto      :: Bool
    , rdaEntryCount  :: Int
    }
    deriving (Generic)

-- | Synonym groups response
newtype SynonymGroupsResponse = SynonymGroupsResponse
    { sgrGroups :: [[Text]]
    }
    deriving (Generic)

-- | Full method details
data MethodDetail = MethodDetail
    { mdId :: UUID
    , mdName :: Text
    , mdDescription :: Maybe Text
    , mdUnit :: Text
    , mdCategory :: Text
    , mdMethodology :: Maybe Text
    , mdFactorCount :: Int
    }
    deriving (Generic)

-- | Characterization factor for API response
data MethodFactorAPI = MethodFactorAPI
    { mfaFlowRef :: UUID        -- ILCD flow UUID
    , mfaFlowName :: Text       -- Flow name
    , mfaDirection :: Text      -- "Input" or "Output"
    , mfaValue :: Double        -- CF value
    }
    deriving (Generic)

-- | A single flow's contribution to an LCIA score
data FlowContributionEntry = FlowContributionEntry
    { fcoFlowName    :: Text         -- Biosphere flow name (e.g. "Carbon dioxide, fossil")
    , fcoContribution :: Double      -- Contribution in impact unit
    , fcoSharePct    :: Double       -- Percentage of total score (0-100)
    , fcoFlowId      :: Text         -- Flow UUID for disambiguation
    , fcoCategory    :: Text         -- e.g. "air/urban air"
    , fcoCompartment :: Maybe Text   -- Sub-compartment (e.g. "urban air")
    , fcoCfValue     :: Double       -- Raw characterization factor value
    }
    deriving (Generic)

-- | LCIA result for a single impact category
data LCIAResult = LCIAResult
    { lrMethodId        :: UUID        -- Method UUID
    , lrMethodName      :: Text        -- Method name
    , lrCategory        :: Text        -- Impact category
    , lrDamageCategory  :: Text        -- Parent damage category (may == category)
    , lrScore           :: Double      -- Total impact score (raw)
    , lrUnit            :: Text        -- Unit (e.g., "kg CO2 eq")
    , lrNormalizedScore :: Maybe Double  -- score * normalization factor
    , lrWeightedScore   :: Maybe Double  -- normalized * weight (in Pt)
    , lrMappedFlows     :: Int         -- Number of flows successfully mapped
    , lrFunctionalUnit  :: Text        -- e.g. "1.0 kg of Butter, unsalted"
    , lrTopContributors :: [FlowContributionEntry]  -- Top contributing elementary flows
    }
    deriving (Generic)

-- | Contributing flows result: top elementary flows for a specific impact category
data ContributingFlowsResult = ContributingFlowsResult
    { cfrMethod     :: Text
    , cfrUnit       :: Text
    , cfrTotalScore :: Double
    , cfrTopFlows   :: [FlowContributionEntry]
    }
    deriving (Generic)

-- | A single activity's contribution to an LCIA score
data ActivityContribution = ActivityContribution
    { acProcessId    :: Text    -- "activityUUID_productUUID" — usable as API process_id
    , acActivityName :: Text    -- e.g. "electricity production, nuclear"
    , acProductName  :: Text    -- e.g. "electricity, medium voltage"
    , acLocation     :: Text    -- e.g. "FR"
    , acContribution :: Double  -- Contribution in impact unit
    , acSharePct     :: Double  -- Percentage of total score (0-100)
    }
    deriving (Generic)

-- | Contributing activities result: top upstream activities for a specific impact category
data ContributingActivitiesResult = ContributingActivitiesResult
    { carMethod      :: Text
    , carUnit        :: Text
    , carTotalScore  :: Double
    , carActivities  :: [ActivityContribution]
    }
    deriving (Generic)

-- | Batch impacts request: compute LCIA for every process in one call.
newtype BatchImpactsRequest = BatchImpactsRequest
    { birProcessIds :: [Text]
    }
    deriving (Generic)

-- | One entry of a batch impacts response.
data BatchImpactsEntry = BatchImpactsEntry
    { bieProcessId    :: Text
    , bieActivityName :: Text
    , bieImpacts      :: LCIABatchResult
    }
    deriving (Generic)

-- | Batch impacts response: one entry per successfully computed process,
-- plus lists of process ids that could not be resolved.
data BatchImpactsResponse = BatchImpactsResponse
    { birResults  :: [BatchImpactsEntry]
    , birNotFound :: [Text]
    , birInvalid  :: [Text]
    }
    deriving (Generic)

-- | Batch LCIA result with optional single score
data LCIABatchResult = LCIABatchResult
    { lbrResults :: [LCIAResult]
    , lbrSingleScore :: Maybe Double    -- sum of weighted scores (Pt)
    , lbrSingleScoreUnit :: Maybe Text  -- "Pt"
    , lbrNormWeightSetName :: Maybe Text
    , lbrAvailableNWsets :: [Text]
    , lbrScoringResults :: M.Map Text (M.Map Text Double)
      -- ^ Scoring set name → (score name → value). All formula-based scoring sets computed at once.
    , lbrScoringUnits :: M.Map Text Text
      -- ^ Scoring set name → display unit (e.g., "Pts", "µPts PEF")
    }
    deriving (Generic)

-- | Flow mapping status for a method
data MappingStatus = MappingStatus
    { mstMethodId :: UUID       -- Method UUID
    , mstMethodName :: Text     -- Method name
    , mstTotalFactors :: Int    -- Total CFs in method
    , mstMappedByUUID :: Int    -- Matched by exact UUID
    , mstMappedByCAS :: Int     -- Matched by CAS number
    , mstMappedByName :: Int    -- Matched by normalized name
    , mstMappedBySynonym :: Int -- Matched via synonym group
    , mstUnmapped :: Int        -- Not matched
    , mstCoverage :: Double     -- Percentage of mapped flows (0-100)
    , mstDbBiosphereCount :: Int      -- Total biosphere flows in the DB
    , mstUniqueDbFlowsMatched :: Int  -- Unique DB flows hit by this method's CFs
    , mstUnmappedFlows :: [UnmappedFlowAPI] -- Details of unmapped flows
    }
    deriving (Generic)

-- | Details about an unmapped flow
data UnmappedFlowAPI = UnmappedFlowAPI
    { ufaFlowRef :: UUID        -- Flow UUID in method
    , ufaFlowName :: Text       -- Flow name in method
    , ufaDirection :: Text      -- "Input" or "Output"
    }
    deriving (Generic)

-- | DB-flow-centric mapping: all biosphere flows with their CF assignments
data FlowCFMapping = FlowCFMapping
    { fcmMethodName :: Text
    , fcmMethodUnit :: Text
    , fcmTotalFlows :: Int          -- Total biosphere flows in DB
    , fcmMatchedFlows :: Int        -- How many have a CF
    , fcmFlows :: [FlowCFEntry]
    }
    deriving (Generic)

-- | A single DB biosphere flow with its CF assignment (if any)
data FlowCFEntry = FlowCFEntry
    { fceFlowId :: UUID
    , fceFlowName :: Text
    , fceFlowCategory :: Text
    , fceCfValue :: Maybe Double     -- CF value (Nothing if no match)
    , fceCfFlowName :: Maybe Text    -- Method CF flow name
    , fceMatchStrategy :: Maybe Text -- "uuid" | "name" | "synonym"
    }
    deriving (Generic)

-- | Characterization result: matched CFs for a method in a database
data CharacterizationResult = CharacterizationResult
    { chrMethod  :: Text
    , chrUnit    :: Text
    , chrMatches :: Int
    , chrShown   :: Int
    , chrFactors :: [CharacterizationEntry]
    }
    deriving (Generic)

-- | A single matched characterization factor
data CharacterizationEntry = CharacterizationEntry
    { cheMethodFlowName :: Text        -- CF flow name from method
    , cheCfValue        :: Double      -- Characterization factor
    , cheCfUnit         :: Text        -- CF unit (e.g. "kg")
    , cheDirection      :: Text        -- "Input" or "Output"
    , cheDbFlowName     :: Text        -- Matched DB flow name
    , cheFlowId         :: Text        -- DB flow UUID
    , cheFlowUnit       :: Text        -- DB flow default unit (e.g. "m3", "kg")
    , cheCategory       :: Text        -- Flow category
    , cheCompartment    :: Maybe Text  -- Sub-compartment
    , cheMatchStrategy  :: Text        -- "uuid", "cas", "name", "synonym", "fuzzy"
    }
    deriving (Generic)

-- | Database list response
newtype DatabaseListResponse = DatabaseListResponse
    { dlrDatabases :: [DatabaseStatusAPI]  -- All available databases
    }
    deriving (Generic)

-- | Database status for API responses
data DatabaseStatusAPI = DatabaseStatusAPI
    { dsaName          :: Text           -- Internal identifier (slug)
    , dsaDisplayName   :: Text           -- Human-readable name for UI
    , dsaDescription   :: Maybe Text
    , dsaLoadAtStartup :: Bool           -- Configured to load at startup
    , dsaStatus        :: Text           -- "unloaded" | "partially_linked" | "loaded"
    , dsaIsUploaded    :: Bool           -- True if path starts with "uploads/"
    , dsaPath          :: Text           -- Data path
    , dsaFormat        :: Maybe Text     -- Database format (EcoSpold 2, EcoSpold 1, SimaPro CSV)
    , dsaActivityCount :: Int            -- Number of activities (0 if unloaded)
    }
    deriving (Generic)

-- | Response for database activation
data ActivateResponse = ActivateResponse
    { arSuccess  :: Bool
    , arMessage  :: Text
    , arDatabase :: Maybe DatabaseStatusAPI
    }
    deriving (Generic)

-- | Result of auto-loading a single dependency
data DepLoadResult
    = DepLoaded    { dlrName :: Text }
    | DepLoadFailed { dlfName :: Text, dlfError :: Text }
    deriving (Generic)

-- | Response for the load database endpoint
data LoadDatabaseResponse
    = LoadFailed    { ldrError :: Text }
    | LoadSucceeded { ldrDatabase :: DatabaseStatusAPI, ldrDeps :: [DepLoadResult] }
    deriving (Generic)

-- | Request for database upload (base64-encoded ZIP)
data UploadRequest = UploadRequest
    { urName        :: Text        -- Display name for the database
    , urDescription :: Maybe Text  -- Optional description
    , urFileData    :: Text        -- Base64-encoded ZIP file content
    }
    deriving (Generic)

-- | Response for database upload
data UploadResponse = UploadResponse
    { uprSuccess :: Bool
    , uprMessage :: Text
    , uprSlug    :: Maybe Text    -- Generated slug (if successful)
    , uprFormat  :: Maybe Text    -- Detected format (if successful)
    }
    deriving (Generic)

-- | Supply chain response — all upstream activities with scaling factors
data SupplyChainResponse = SupplyChainResponse
    { scrRoot :: ActivitySummary
    , scrTotalActivities :: Int
    , scrFilteredActivities :: Int
    , scrSupplyChain :: [SupplyChainEntry]
    , scrEdges :: [SupplyChainEdge]
    }
    deriving (Generic)

-- | A single entry in the supply chain
data SupplyChainEntry = SupplyChainEntry
    { sceProcessId :: Text
    , sceName :: Text
    , sceLocation :: Text
    , sceQuantity :: Double       -- scalingFactor × root reference product amount (physical amount per functional unit)
    , sceUnit :: Text
    , sceScalingFactor :: Double   -- raw value from scaling vector
    , sceClassifications :: M.Map Text Text  -- Classifications (ISIC, CPC, Category, etc.)
    , sceDepth :: Int              -- shortest path distance from root (BFS)
    , sceUpstreamCount :: Int      -- number of unique upstream activities reachable from this one
    }
    deriving (Generic)

-- | An edge in the upstream supply chain subgraph
data SupplyChainEdge = SupplyChainEdge
    { sceEdgeFrom   :: Text    -- supplier processId
    , sceEdgeTo     :: Text    -- consumer processId
    , sceEdgeAmount :: Double  -- technosphere coefficient
    }
    deriving (Generic)

-- | Request body for POST endpoints that accept substitutions.
-- Substitutions modify the scaling vector via Sherman-Morrison rank-1 updates.
newtype SubstitutionRequest = SubstitutionRequest
    { srSubstitutions :: [Substitution]
    }
    deriving (Generic)

-- | A single supplier substitution
data Substitution = Substitution
    { subFrom     :: Text  -- Original supplier ProcessId
    , subTo       :: Text  -- Replacement supplier ProcessId
    , subConsumer :: Text  -- Activity that consumes the original supplier
    }
    deriving (Generic)

-- | Exchange with unit and flow information for API responses
data ExchangeWithUnit = ExchangeWithUnit
    { ewuExchange :: Exchange
    , ewuUnitName :: Text -- Unit name for the exchange
    , ewuFlowName :: Text -- Name of the flow being exchanged
    , ewuFlowCategory :: Text -- Category/compartment (for biosphere) or "technosphere"
    , ewuTargetActivity :: Maybe Text -- For technosphere: name of target activity
    , ewuTargetLocation :: Maybe Text -- For technosphere: location of target activity
    , ewuTargetProcessId :: Maybe Text -- For technosphere: ProcessId for navigation (activityUUID_productUUID)
    }
    deriving (Generic)

-- | Activity information optimized for API responses
data ActivityForAPI = ActivityForAPI
    { pfaProcessId :: Text  -- ProcessId format: "activityUUID_productUUID"
    , pfaName :: Text
    , pfaDescription :: [Text] -- Description par paragraphes
    , pfaSynonyms :: M.Map Text (S.Set Text) -- Synonymes par langue
    , pfaClassifications :: M.Map Text Text -- Classifications (ISIC, CPC, etc.)
    , pfaLocation :: Text
    , pfaUnit :: Text -- Unité de référence
    , pfaReferenceProduct :: Maybe Text -- Name of the reference product (output)
    , pfaReferenceProductAmount :: Maybe Double -- Amount of reference product
    , pfaReferenceProductUnit :: Maybe Text -- Unit of reference product
    , pfaAllProducts :: [ActivitySummary] -- All products from same activityUUID
    , pfaExchanges :: [ExchangeWithUnit] -- Exchanges with unit names
    }
    deriving (Generic)

-- | Streamlined activity information - core data only
data ActivityInfo = ActivityInfo
    { piActivity :: ActivityForAPI -- Enhanced activity with unit names
    , piMetadata :: ActivityMetadata -- Extended metadata
    , piStatistics :: ActivityStats -- Usage statistics
    , piLinks :: ActivityLinks -- Links to sub-resources
    }
    deriving (Generic)

-- | Extended activity metadata
data ActivityMetadata = ActivityMetadata
    { pmTotalFlows :: Int -- Number of unique flows used
    , pmTechnosphereInputs :: Int -- Count of technosphere inputs
    , pmBiosphereExchanges :: Int -- Count of biosphere exchanges
    , pmHasReferenceProduct :: Bool -- Whether activity has reference product
    , pmReferenceProductFlow :: Maybe UUID -- Flow ID of reference product
    }
    deriving (Generic)

-- | Links to related resources
data ActivityLinks = ActivityLinks
    { plFlowsUrl :: Text -- URL to flows endpoint
    , plInputsUrl :: Text -- URL to inputs endpoint
    , plOutputsUrl :: Text -- URL to outputs endpoint
    , plReferenceProductUrl :: Maybe Text -- URL to reference product (if exists)
    }
    deriving (Generic)

-- | Activity statistics
data ActivityStats = ActivityStats
    { psInputCount :: Int
    , psOutputCount :: Int
    , psTotalExchanges :: Int
    , psLocation :: Text
    }
    deriving (Generic)

-- | Flow with additional metadata
data FlowDetail = FlowDetail
    { fdFlow :: Flow
    , fdUnitName :: Text -- Unit name for the flow
    , fdUsageCount :: Int -- How many activities use this flow
    }
    deriving (Generic)

-- | Exchange with flow, unit, and target activity information
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: Flow
    , edFlowUnitName :: Text -- Unit name for the flow's default unit
    , edUnit :: Unit -- Unit information for the exchange
    , edExchangeUnitName :: Text -- Unit name for the exchange's specific unit
    , edTargetActivity :: Maybe ActivitySummary -- Target activity for technosphere inputs
    }
    deriving (Generic)

-- | A single filter entry returned in a preset
data ClassificationEntryInfo = ClassificationEntryInfo
    { ceiSystem :: !Text
    , ceiValue  :: !Text
    , ceiMode   :: !Text   -- "exact" or "contains"
    } deriving (Show, Eq, Generic)

-- | A named filter preset (from TOML config)
data ClassificationPresetInfo = ClassificationPresetInfo
    { cpiName        :: !Text
    , cpiLabel       :: !Text
    , cpiDescription :: !(Maybe Text)
    , cpiFilters     :: ![ClassificationEntryInfo]
    } deriving (Show, Eq, Generic)

-- | Classification system with its values for browsing/filtering
data ClassificationSystem = ClassificationSystem
    { csName :: Text           -- e.g. "ISIC rev.4 ecoinvent", "CPC", "HS2017"
    , csValues :: [Text]       -- Distinct values, sorted
    , csActivityCount :: Int   -- How many activities have this system
    }
    deriving (Generic)

-- | Result of an /activity/{pid}/aggregate call.
--
-- A SQL-group-by-style aggregation over exchanges, supply chain entries, or
-- biosphere flows, depending on the requested scope.
data Aggregation = Aggregation
    { aggScope          :: Text                  -- echoed scope: "direct" | "supply_chain" | "biosphere"
    , aggFilteredTotal  :: Double                -- total summed across all matching items (after filters)
    , aggFilteredUnit   :: Maybe Text            -- Nothing when matched items have heterogeneous units
    , aggFilteredCount  :: Int                   -- count of items matching the filters
    , aggGroups         :: [AggregationGroup]    -- one entry per group_by bucket (empty when group_by omitted)
    }
    deriving (Generic)

-- | One bucket in an aggregation result.
data AggregationGroup = AggregationGroup
    { aggKey      :: Text
    , aggQuantity :: Double
    , aggUnit     :: Maybe Text       -- Nothing when group's items are heterogeneous
    , aggShare    :: Maybe Double     -- only set when aggregate=share
    , aggCount    :: Int
    }
    deriving (Generic)

-- JSON instances. All record types use API.JsonOptions.stripLowerPrefix
-- via the strippedToJSON/strippedToEncoding/strippedParseJSON helpers.
-- Sum-only types (NodeType, EdgeType, FlowRole) keep default derivation.
instance ToJSON ConsumerResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance FromJSON ConsumerResult where { parseJSON = strippedParseJSON }
instance ToJSON ClassificationEntryInfo where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ClassificationPresetInfo where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ClassificationSystem where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON Aggregation where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON AggregationGroup where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance (ToJSON a) => ToJSON (SearchResults a) where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivitySummary where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowSearchResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON InventoryMetadata where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON InventoryStatistics where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON TreeExport where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON TreeMetadata where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ExportNode where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON NodeType
instance ToJSON EdgeType
instance ToJSON TreeEdge where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowInfo where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowRole
instance ToJSON ExchangeWithUnit where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivityForAPI where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivityInfo where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivityMetadata where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivityLinks where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivityStats where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON InventoryFlowDetail where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON Flow where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowSummary where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON InventoryExport where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ExchangeDetail where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON Unit where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowDetail where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON GraphExport where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON GraphNode where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON GraphEdge where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON MethodSummary where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON MethodCollectionListResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON MethodCollectionStatusAPI where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON MethodDetail where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON MethodFactorAPI where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowContributionEntry where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON LCIAResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON LCIABatchResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON BatchImpactsEntry where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON BatchImpactsResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance FromJSON BatchImpactsRequest where { parseJSON = strippedParseJSON }
instance ToJSON ContributingFlowsResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivityContribution where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ContributingActivitiesResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON MappingStatus where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON UnmappedFlowAPI where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowCFMapping where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON FlowCFEntry where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON CharacterizationResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON CharacterizationEntry where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON SupplyChainResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON SupplyChainEntry where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON SupplyChainEdge where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance FromJSON SubstitutionRequest where { parseJSON = strippedParseJSON }
instance FromJSON Substitution where { parseJSON = strippedParseJSON }

-- FromJSON instances needed for API conversion
instance (FromJSON a) => FromJSON (SearchResults a) where { parseJSON = strippedParseJSON }
instance FromJSON ActivitySummary where { parseJSON = strippedParseJSON }
instance FromJSON ActivityInfo where { parseJSON = strippedParseJSON }
instance FromJSON ActivityForAPI where { parseJSON = strippedParseJSON }
instance FromJSON ActivityMetadata where { parseJSON = strippedParseJSON }
instance FromJSON ActivityLinks where { parseJSON = strippedParseJSON }
instance FromJSON ActivityStats where { parseJSON = strippedParseJSON }
instance FromJSON ExchangeWithUnit where { parseJSON = strippedParseJSON }
instance ToJSON DatabaseListResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON DatabaseStatusAPI where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON ActivateResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON DepLoadResult where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON LoadDatabaseResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON UploadRequest where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON UploadResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance FromJSON DatabaseListResponse where { parseJSON = strippedParseJSON }
instance FromJSON DatabaseStatusAPI where { parseJSON = strippedParseJSON }
instance FromJSON ActivateResponse where { parseJSON = strippedParseJSON }
instance FromJSON DepLoadResult where { parseJSON = strippedParseJSON }
instance FromJSON LoadDatabaseResponse where { parseJSON = strippedParseJSON }
instance FromJSON UploadRequest where { parseJSON = strippedParseJSON }
instance FromJSON UploadResponse where { parseJSON = strippedParseJSON }
instance FromJSON MethodCollectionListResponse where { parseJSON = strippedParseJSON }
instance FromJSON MethodCollectionStatusAPI where { parseJSON = strippedParseJSON }
instance ToJSON RefDataListResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON RefDataStatusAPI where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance ToJSON SynonymGroupsResponse where { toJSON = strippedToJSON; toEncoding = strippedToEncoding }
instance FromJSON RefDataListResponse where { parseJSON = strippedParseJSON }
instance FromJSON RefDataStatusAPI where { parseJSON = strippedParseJSON }
instance FromJSON SynonymGroupsResponse where { parseJSON = strippedParseJSON }

-- openapi3 cannot derive ToSchema for BSL.ByteString directly
newtype BinaryContent = BinaryContent BSL.ByteString

instance MimeRender OctetStream BinaryContent where
    mimeRender _ (BinaryContent bs) = bs

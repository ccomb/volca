{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types where

import Types (Exchange, Flow, UUID, Unit)
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics

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
    { prsId :: Text  -- ProcessId format: activity_uuid_product_uuid
    , prsName :: Text
    , prsLocation :: Text
    , prsProduct :: Text  -- Reference product name
    , prsProductAmount :: Double  -- Reference product amount
    , prsProductUnit :: Text  -- Reference product unit name
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
    { gnId :: Int -- Numeric ID for efficient frontend processing
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

-- | LCIA computation request
data LCIARequest = LCIARequest
    { lciaMethod :: Text -- Method file path or content
    , lciaFormat :: Maybe Text -- Optional format specification
    }
    deriving (Show, Generic)

-- | Method summary for listing methods
data MethodSummary = MethodSummary
    { msmId :: UUID           -- Method UUID
    , msmName :: Text         -- Method name
    , msmCategory :: Text     -- Impact category
    , msmUnit :: Text         -- Reference unit (e.g., "kg CO2 eq")
    , msmFactorCount :: Int   -- Number of characterization factors
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

-- | LCIA result for a single impact category
data LCIAResult = LCIAResult
    { lrMethodId :: UUID        -- Method UUID
    , lrMethodName :: Text      -- Method name
    , lrCategory :: Text        -- Impact category
    , lrScore :: Double         -- Total impact score
    , lrUnit :: Text            -- Unit (e.g., "kg CO2 eq")
    , lrMappedFlows :: Int      -- Number of flows successfully mapped
    , lrUnmappedFlows :: Int    -- Number of flows not mapped
    }
    deriving (Generic)

-- | Flow mapping status for a method
data MappingStatus = MappingStatus
    { mstMethodId :: UUID       -- Method UUID
    , mstMethodName :: Text     -- Method name
    , mstTotalFactors :: Int    -- Total CFs in method
    , mstMappedByUUID :: Int    -- Matched by exact UUID
    , mstMappedByName :: Int    -- Matched by normalized name
    , mstMappedBySynonym :: Int -- Matched via synonym group
    , mstUnmapped :: Int        -- Not matched
    , mstCoverage :: Double     -- Percentage of mapped flows (0-100)
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

-- | Database list response
data DatabaseListResponse = DatabaseListResponse
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

-- JSON instances
instance (ToJSON a) => ToJSON (SearchResults a)
instance ToJSON ActivitySummary
instance ToJSON FlowSearchResult
instance ToJSON InventoryMetadata
instance ToJSON InventoryStatistics
instance ToJSON TreeExport
instance ToJSON TreeMetadata
instance ToJSON ExportNode
instance ToJSON NodeType
instance ToJSON EdgeType
instance ToJSON TreeEdge
instance ToJSON FlowInfo
instance ToJSON FlowRole
instance ToJSON ExchangeWithUnit
instance ToJSON ActivityForAPI
instance ToJSON ActivityInfo
instance ToJSON ActivityMetadata
instance ToJSON ActivityLinks
instance ToJSON ActivityStats
instance ToJSON InventoryFlowDetail
instance ToJSON Flow
instance ToJSON FlowSummary
instance ToJSON InventoryExport
instance ToJSON ExchangeDetail
instance ToJSON Unit
instance ToJSON FlowDetail
instance ToJSON GraphExport
instance ToJSON GraphNode
instance ToJSON GraphEdge
instance ToJSON MethodSummary
instance ToJSON MethodDetail
instance ToJSON MethodFactorAPI
instance ToJSON LCIAResult
instance ToJSON MappingStatus
instance ToJSON UnmappedFlowAPI

-- FromJSON instances needed for API conversion
instance (FromJSON a) => FromJSON (SearchResults a)
instance FromJSON ActivitySummary
instance FromJSON ActivityInfo
instance FromJSON ActivityForAPI
instance FromJSON ActivityMetadata
instance FromJSON ActivityLinks
instance FromJSON ActivityStats
instance FromJSON ExchangeWithUnit
instance FromJSON LCIARequest
instance ToJSON DatabaseListResponse
instance ToJSON DatabaseStatusAPI
instance ToJSON ActivateResponse
instance ToJSON DepLoadResult
instance ToJSON LoadDatabaseResponse
instance ToJSON UploadRequest
instance ToJSON UploadResponse
instance FromJSON DatabaseListResponse
instance FromJSON DatabaseStatusAPI
instance FromJSON ActivateResponse
instance FromJSON DepLoadResult
instance FromJSON LoadDatabaseResponse
instance FromJSON UploadRequest
instance FromJSON UploadResponse

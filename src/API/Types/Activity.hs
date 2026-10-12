{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Activity where

import API.JsonOptions (Stripped (..))
import API.Types.Common
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map as M
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToSchema (..), declareSchemaRef, properties, required, type_)
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Types (
    BiosphereFlow (..),
    Compartment (..),
    DatasetDates (..),
    DocSection (..),
    Exchange,
    ExchangeKind (..),
    NativeActivityType (..),
    NativeProcessId (..),
    Pedigree,
    UUID,
    Unit,
    WasteRole (..),
 )

-- | Where a unit sits in the unit table: its dimension, and how many of the dimension's reference unit one of it is.
data CatalogueMeasure = CatalogueMeasure
    { cmDimension :: [Int] -- exponent vector, in the table's dimension order
    , cmFactor :: Double
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CatalogueMeasure)

-- | One process as a client indexing the database needs it: what it is called, how it is classified, where, and in what unit.
data CatalogueEntry = CatalogueEntry
    { ceProcessId :: Text
    , ceActivityName :: Text
    , ceProductName :: Text -- empty when the reference exchange names a flow the database does not hold, as in a search result
    , ceLocation :: Text
    , ceUnit :: Text -- the reference product's unit as the database writes it
    , ceMeasure :: Maybe CatalogueMeasure -- Nothing when the unit table cannot settle 'ceUnit': unknown, or matching several spellings
    , ceClassification :: M.Map Text Text
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CatalogueEntry)

-- | One page of a catalogue. Every page names the catalogue it was cut from, so a client reading several notices when the database changed under it.
data CataloguePage = CataloguePage
    { cpFingerprint :: Text
    , cpTotal :: Int
    , cpOffset :: Int
    , cpEntries :: [CatalogueEntry]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CataloguePage)

-- | The fingerprint alone, for a client that only wants to know whether its copy is still current.
newtype CatalogueFingerprint = CatalogueFingerprint {cfFingerprint :: Text}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CatalogueFingerprint)

-- | Consumer result – ActivitySummary enriched with BFS depth from the queried supplier
data ConsumerResult = ConsumerResult
    { crProcessId :: Text
    , crActivityName :: Text
    , crLocation :: Text
    , crProductName :: Text
    , crProductAmount :: Double
    , crProductUnit :: Text
    , crDepth :: Int -- hops from the queried supplier (1 = direct consumer)
    , crClassifications :: M.Map Text Text -- Classifications (ISIC, CPC, Category, etc.), mirrors SupplyChainEntry
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ConsumerResult)

{- | Wrapper for /consumers responses. Mirrors 'SupplyChainResponse' so clients
have a uniform {entries, edges} shape in both traversal directions. Edges
are only populated when include-edges=true; callers can walk them to
reconstruct supplier-to-consumer paths without a second /path-to round trip.
-}
data ConsumersResponse = ConsumersResponse
    { crrResults :: !(SearchResults ConsumerResult)
    , crrEdges :: ![SupplyChainEdge]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ConsumersResponse)

{- | Enhanced flow information for search results (now includes synonyms).

'fsrKind' is what tells a product apart from a substance. Without it the two
are told apart only by an empty 'fsrCategory', which a waste flow has as well.
-}
data FlowSearchResult = FlowSearchResult
    { fsrId :: UUID
    , fsrName :: Text
    , fsrKind :: ExchangeKind
    , fsrCategory :: Text -- Medium only (e.g. "soil"), never the sub-compartment
    , fsrCompartment :: Maybe Text -- Sub-compartment (e.g. "agricultural")
    , fsrUnitName :: Text
    , fsrSynonyms :: M.Map Text [Text] -- Synonyms by language (converted from Set to List for JSON)
    , fsrProducerCount :: Maybe Int -- How many activities make this flow; Nothing on a flow no activity can produce (biosphere, waste), never 0 as a stand-in for "not asked"
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowSearchResult)

{- | How many of each thing one query finds, for the three tabs of a search
box. Disjoint, and together they cover the database.
-}
data SearchCountsAPI = SearchCountsAPI
    { scaProcesses :: Int -- Activity rows, what you search, get and score
    , scaProducts :: Int -- Technosphere flows: what one activity makes and another consumes
    , scaFlows :: Int -- Biosphere and waste flows: what is exchanged with nature or discarded
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SearchCountsAPI)

-- | Inventory export data structures
data InventoryExport = InventoryExport
    { ieMetadata :: InventoryMetadata
    , ieFlows :: [InventoryFlowDetail]
    , ieStatistics :: InventoryStatistics
    , ieCutoffInputs :: [CutoffInput] -- The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them
    , ieWithheldCutoffs :: [WithheldCutoffs] -- The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped InventoryExport)

data InventoryMetadata = InventoryMetadata
    { imRootActivity :: ActivitySummary
    , imTotalFlows :: Int
    , imEmissionFlows :: Int -- Biosphere outputs (negative environmental impact)
    , imResourceFlows :: Int -- Biosphere inputs (resource extraction)
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped InventoryMetadata)

data InventoryFlowDetail = InventoryFlowDetail
    { ifdFlow :: BiosphereFlow -- Inventory flows are always biosphere
    , ifdQuantity :: Double
    , ifdUnitName :: Text
    , ifdIsEmission :: Bool -- True for emissions, False for resource extraction
    , ifdCategory :: Text -- Flow category for grouping
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped InventoryFlowDetail)

data InventoryStatistics = InventoryStatistics
    { isTotalQuantity :: Double -- Sum of absolute values
    , isEmissionQuantity :: Double -- Sum of emissions (should be positive)
    , isResourceQuantity :: Double -- Sum of resource extraction (should be positive)
    , isTopCategories :: [(Text, Int)] -- Top flow categories by count
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped InventoryStatistics)

-- | Tree export data structures for visualization
data TreeExport = TreeExport
    { teTree :: TreeMetadata
    , teNodes :: M.Map Text ExportNode -- Changed to Text (ProcessId format)
    , teEdges :: [TreeEdge]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped TreeExport)

data TreeMetadata = TreeMetadata
    { tmRootId :: Text -- Changed to Text (ProcessId format)
    , tmMaxDepth :: Int
    , tmTotalNodes :: Int
    , tmLoopNodes :: Int
    , tmLeafNodes :: Int
    , tmExpandableNodes :: Int -- Nodes that could expand further
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped TreeMetadata)

{- | One node of a tree export. Every node names the row it sits at, in
ProcessId format, save a 'MissingNode': it stands for a link no row satisfies,
so it has none to name and wears "missing:" and the activity identifier
instead. Anything reading an id as a process id has to allow for that one, here
and on the two ends of an edge.
-}
data ExportNode = ExportNode
    { enId :: Text -- ProcessId format, or "missing:<activityUUID>" on a MissingNode
    , enName :: Text
    , enDescription :: [Text]
    , enLocation :: Text
    , enUnit :: Text
    , enNodeType :: NodeType
    , enDepth :: Int
    , enLoopTarget :: Maybe Text -- ProcessId format; set on a LoopNode alone
    , enParentId :: Maybe Text -- Node id of the parent, for navigation back up
    , enChildrenCount :: Int -- Number of potential children for expandability
    , enCompartment :: Maybe Text -- Biosphere compartment (air/water/soil), only for BiosphereNodes
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExportNode)

data NodeType = ActivityNode | LoopNode | MissingNode | BiosphereEmissionNode | BiosphereResourceNode
    deriving (Eq, Show, Generic)
    deriving anyclass (ToSchema)

data EdgeType = TechnosphereEdge | BiosphereEmissionEdge | BiosphereResourceEdge
    deriving (Eq, Show, Generic)
    deriving anyclass (ToSchema)

data TreeEdge = TreeEdge
    { teFrom :: Text -- Node id, as ExportNode carries it
    , teTo :: Text -- Node id, as ExportNode carries it
    , teFlow :: FlowInfo
    , teQuantity :: Double
    , teUnit :: Text
    , teEdgeType :: EdgeType -- Type of edge (technosphere or biosphere)
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped TreeEdge)

data FlowInfo = FlowInfo
    { fiId :: UUID
    , fiName :: Text
    , fiCategory :: Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowInfo)

-- | Graph export data structures for network visualization
data GraphExport = GraphExport
    { geNodes :: [GraphNode]
    , geEdges :: [GraphEdge]
    , geUnitGroups :: M.Map Text Text -- Unit to unit group mapping
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped GraphExport)

data GraphNode = GraphNode
    { gnNodeId :: Int -- Numeric ID for efficient frontend processing
    , gnLabel :: Text -- Activity name
    , gnValue :: Double -- Cumulative amount from factorized matrix
    , gnUnit :: Text -- Unit (kg, MJ, etc.)
    , gnProcessId :: Text -- Original ProcessId for linking
    , gnLocation :: Text -- Geography
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped GraphNode)

data GraphEdge = GraphEdge
    { geSource :: Int -- Source node ID
    , geTarget :: Int -- Target node ID
    , geValue :: Double -- Direct flow amount from technosphere matrix
    , geUnit :: Text -- Flow unit
    , geFlowName :: Text -- Name of the flow
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped GraphEdge)

{- | Lightweight flow information for lists. Carries either a tech or a bio
flow; the @ApiFlow@ tag is the wire discriminator.
-}
data FlowSummary = FlowSummary
    { fsFlow :: ApiFlow
    , fsUnitName :: Text -- Unit name for the flow
    , fsUsageCount :: Int -- How many activities use this flow
    , fsRole :: FlowRole -- Role in this specific activity
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowSummary)

-- | Role of a flow in a specific activity context
data FlowRole = InputFlow | OutputFlow | ReferenceProductFlow
    deriving (Show, Generic)
    deriving anyclass (ToSchema)

{- | Which side of a flow the caller is asking about: the activities that make
it, the ones that use it, or both.

The two questions have different answers and only one of them is "how is this
product made". 'EitherSide' is what the route answered before it could be
asked, and stays its default.
-}
data ProducerFilter = ProducersOnly | ConsumersOnly | EitherSide
    deriving (Eq, Show)

{- | Read the @role@ query parameter. An absent parameter is 'EitherSide'; an
unrecognised one is 'Nothing', so the route can refuse it rather than quietly
answer a different question.
-}
parseProducerFilter :: Maybe Text -> Maybe ProducerFilter
parseProducerFilter mRole = case mRole of
    Nothing -> Just EitherSide
    Just "producer" -> Just ProducersOnly
    Just "consumer" -> Just ConsumersOnly
    Just "any" -> Just EitherSide
    Just _ -> Nothing

-- | Supply chain response – all upstream activities with scaling factors
data SupplyChainResponse = SupplyChainResponse
    { scrRoot :: ActivitySummary
    , scrTotalActivities :: Int
    , scrFilteredActivities :: Int
    , scrSupplyChain :: [SupplyChainEntry]
    , scrEdges :: [SupplyChainEdge]
    , scrWithheldDatabases :: [WithheldProcesses] -- Not in the chain above nor in its counts but the total
    , scrWithheldInputs :: [WithheldInput]
    , scrCutoffInputs :: [CutoffInput] -- The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them, over the chain the walk reached
    , scrWithheldCutoffs :: [WithheldCutoffs] -- The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped SupplyChainResponse)

{- | A listed process that takes inputs from a database whose licence keeps the
amounts of its exchanges. Its edges to that database are not listed, so this
is what tells a reader drawing the chain that part of the process's weight
comes from there, and not from the process itself. Listed with the edges only.
-}
data WithheldInput = WithheldInput
    { wiConsumer :: Text -- processId, as an edge names it
    , wiDatabase :: Text
    }
    deriving (Eq, Ord, Generic)
    deriving (ToJSON, ToSchema) via (Stripped WithheldInput)

{- | A single entry in the supply chain. @sceProcessId@ is bare for entries
from the root DB and qualified (@"dbName::pid"@) for entries reached via
a cross-DB link. @sceDatabaseName@ carries the same information in a
dedicated field so the UI can render a Database column without parsing.
-}
data SupplyChainEntry = SupplyChainEntry
    { sceProcessId :: Text
    , sceDatabaseName :: Text -- database the entry lives in
    , sceActivityName :: Text
    , sceLocation :: Text
    , sceQuantity :: Double -- scalingFactor × root reference product amount (physical amount per functional unit)
    , sceUnit :: Text
    , sceScalingFactor :: Double -- raw value from scaling vector
    , sceClassifications :: M.Map Text Text -- Classifications (ISIC, CPC, Category, etc.)
    , sceDepth :: Int -- shortest path distance from root (BFS)
    , sceUpstreamCount :: Int -- number of direct consumers of this entry inside the chain
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped SupplyChainEntry)

-- | An edge in the upstream supply chain subgraph
data SupplyChainEdge = SupplyChainEdge
    { sceEdgeFrom :: Text -- supplier processId
    , sceEdgeFromDb :: Text -- supplier database name
    , sceEdgeTo :: Text -- consumer processId
    , sceEdgeToDb :: Text -- consumer database name
    , sceEdgeAmount :: Double -- technosphere coefficient
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SupplyChainEdge)

{- | Request body for POST endpoints that accept substitutions.
Substitutions modify the scaling vector via Sherman-Morrison rank-1 updates.
-}
newtype SubstitutionRequest = SubstitutionRequest
    { srSubstitutions :: [Substitution]
    }
    deriving (Generic)
    deriving (FromJSON, ToSchema) via (Stripped SubstitutionRequest)

{- | Scope of a supplier substitution.

* 'OneEdge' carries the consumer 'ProcessId' ref and replaces the supplier
  on that single technosphere edge – the historical per-edge behaviour.
* 'AllConsumers' replaces @from@ by @to@ on __every__ consumer that sources
  from @from@, as one rank-1 update. @from@ must live in the root DB.

On the wire the scope is the optional @consumer@ field: present →
'OneEdge', absent → 'AllConsumers'. Making it a sum type keeps the
applicator's dispatch total (no "is the consumer empty?" guard).
-}
data SubstitutionScope
    = OneEdge Text
    | AllConsumers

{- | A single supplier substitution.

@subFrom@/@subTo@ are 'ProcessId' texts, either bare @"actUUID_prodUUID"@
(resolved in the URL's database, i.e. the root DB) or in the cross-DB
qualified form @"dbName::actUUID_prodUUID"@ (resolved against the named
dep DB). See 'parseSubRef'. The 'OneEdge' consumer ref follows the same
bare/qualified rule and may live in the root DB or any loaded, reachable
dep DB; the per-level applicator dispatches to the right database.
-}
data Substitution = Substitution
    { subFrom :: Text -- Original supplier ProcessId (bare or dbName::pid)
    , subTo :: Text -- Replacement supplier ProcessId (bare or dbName::pid)
    , subScope :: SubstitutionScope -- Per-edge consumer, or all consumers
    }

{- | The DB-qualified ref that decides which recursion level processes a
substitution: the consumer for 'OneEdge', the replaced supplier @from@ for
'AllConsumers' (whose consumers are enumerated where @from@ lives).
-}
subAnchorRef :: Substitution -> Text
subAnchorRef sub = case subScope sub of
    OneEdge cRef -> cRef
    AllConsumers -> subFrom sub

{- | @consumer@ present → 'OneEdge'; absent or @null@ → 'AllConsumers' (global
swap). An empty-string @consumer@ is taken literally as a 'OneEdge' ref and
fails to resolve, never silently promoted to a global swap.
-}
instance FromJSON Substitution where
    parseJSON = withObject "Substitution" $ \o ->
        Substitution
            <$> o .: "from"
            <*> o .: "to"
            <*> (maybe AllConsumers OneEdge <$> o .:? "consumer")

-- | @from@/@to@ required; @consumer@ optional (omit for a global swap).
instance ToSchema Substitution where
    declareNamedSchema _ = do
        textRef <- declareSchemaRef (Proxy :: Proxy Text)
        pure $
            NamedSchema (Just "Substitution") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("from", textRef)
                            , ("to", textRef)
                            , ("consumer", textRef)
                            ]
                    & required .~ ["from", "to"]

{- | Name of the request-level "root" database – the DB extracted from the
URL path and the implicit target of any bare 'ProcessId' (one without the
@"dbName::"@ qualifier).

The newtype exists so that the recursive substitution walker cannot
accidentally confuse the *root* DB (where bare refs are resolved, per the
'Substitution' docstring) with the *current* DB being visited during the
descent – they are different concepts and were previously both plain
'Text', which caused a real bug where bare consumers were retried in
every dep DB.
-}
newtype RootDb = RootDb {unRootDb :: Text}
    deriving (Eq, Show)

{- | Name of the database currently being visited by the recursive
substitution walker – distinct from 'RootDb' precisely so that the
@thisDb@/@rootDb@ argument pair cannot be silently swapped.
-}
newtype ThisDb = ThisDb {unThisDb :: Text}
    deriving (Eq, Show)

{- | Parse a substitution reference into @(targetDB, bare pid)@. A bare
@"actUUID_prodUUID"@ resolves in the caller-supplied root DB; a qualified
@"dbName::actUUID_prodUUID"@ resolves in @dbName@. The @::@ separator is
unambiguous because UUIDs contain no colons.
-}
parseSubRef :: RootDb -> Text -> (Text, Text)
parseSubRef (RootDb rootDb) raw = case T.breakOn (T.pack "::") raw of
    (pid, rest)
        | T.null rest -> (rootDb, pid)
        | otherwise -> (pid, T.drop 2 rest)

{- | Exchange with unit and flow information for API responses.
Compartment is biosphere-only; technosphere exchanges have no taxonomy here
(their classification lives on the producing activity).
-}
data ExchangeWithUnit = ExchangeWithUnit
    { ewuExchange :: Exchange
    , ewuUnitName :: Text -- Unit name for the exchange
    , ewuFlowName :: Text -- Name of the flow being exchanged
    , ewuCompartment :: Maybe Compartment -- Biosphere compartment, Nothing for technosphere
    , ewuTargetActivityName :: Maybe Text -- Supplier, or the treatment a waste goes to; Nothing on a biosphere line
    , ewuTargetLocation :: Maybe Text -- Location of that activity
    , ewuTargetProcessId :: Maybe Text -- ProcessId for navigation (activityUUID_productUUID)
    , ewuWasteRole :: Maybe WasteRole -- What a waste line does; Nothing on every other kind
    , ewuExComment :: Maybe Text -- Free-text per-exchange comment (mirrors exchangeComment)
    , ewuPedigree :: Maybe Pedigree -- LCA data-quality scores when available (mirrors exchangePedigree)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeWithUnit)

{- | An exchange named without its amount: what a reader sees of a dataset whose
licence keeps the amounts of its exchanges to itself. Enough to tell what the
process is made of, not to rebuild it.
-}
data ExchangeName = ExchangeName
    { xnFlowName :: Text
    , xnUnitName :: Text
    , xnKind :: ExchangeKind
    , xnIsInput :: Bool
    , xnCompartment :: Maybe Compartment -- Biosphere compartment, Nothing for technosphere
    , xnTargetActivityName :: Maybe Text -- Supplier, or the treatment a waste goes to
    , xnTargetLocation :: Maybe Text
    , xnTargetProcessId :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeName)

-- | The exchanges of a dataset whose licence keeps their amounts, and why.
data WithheldExchanges = WithheldExchanges
    { weReason :: Text
    , weLines :: [ExchangeName]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped WithheldExchanges)

-- | Activity information optimized for API responses
data ActivityForAPI = ActivityForAPI
    { pfaProcessId :: Text -- ProcessId format: "activityUUID_productUUID"
    , pfaActivityName :: Text
    , pfaDescription :: [Text] -- Description par paragraphes
    , pfaDocumentation :: [DocSection] -- Provenance the dataset states about itself (source, technology, review); empty when the format records none
    , pfaDates :: DatasetDates -- The days the dataset says it was created, last revised, or written without saying which
    , pfaSynonyms :: M.Map Text (S.Set Text) -- Synonymes par langue
    , pfaClassifications :: M.Map Text Text -- Classifications (ISIC, CPC, etc.)
    , pfaLocation :: Text
    , pfaUnit :: Text -- Unité de référence
    , pfaProductName :: Maybe Text -- Name of the reference product (output)
    , pfaProductAmount :: Maybe Double -- Amount of reference product
    , pfaProductUnit :: Maybe Text -- Unit of reference product
    , pfaAllProducts :: [ActivitySummary] -- All products from same activityUUID
    , pfaExchanges :: [ExchangeWithUnit] -- Exchanges with unit names
    , pfaNativeType :: Maybe NativeActivityType -- Source-native activity type
    , pfaWithheld :: Maybe WithheldExchanges -- Set when the licence keeps the amounts to itself: then the exchanges are empty, and these name them
    , pfaNativeId :: Maybe NativeProcessId -- The identifier the source gave the dataset block this came out of (SimaPro's "Process identifier", EcoSpold 1's dataset number); Nothing when the format has none, and when the format's identifier is the activity UUID the process id already spells
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityForAPI)

-- | Streamlined activity information - core data only
data ActivityInfo = ActivityInfo
    { piActivity :: ActivityForAPI -- Enhanced activity with unit names
    , piMetadata :: ActivityMetadata -- Extended metadata
    , piStatistics :: ActivityStats -- Usage statistics
    , piLinks :: ActivityLinks -- Links to sub-resources
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityInfo)

-- | Extended activity metadata
data ActivityMetadata = ActivityMetadata
    { pmTotalFlows :: Int -- Number of unique flows used
    , pmTechnosphereInputs :: Int -- Count of technosphere inputs
    , pmBiosphereExchanges :: Int -- Count of biosphere exchanges (strict: excludes waste)
    , pmWasteExchangesLinked :: Int -- Waste exchanges resolved to a treatment activity
    , pmWasteExchangesOrphan :: Int -- Cut-off waste exchanges (no modelled treatment)
    , pmHasReferenceProduct :: Bool -- Whether activity has reference product
    , pmReferenceProductFlow :: Maybe UUID -- Flow ID of reference product
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityMetadata)

-- | Links to related resources
data ActivityLinks = ActivityLinks
    { plFlowsUrl :: Text -- URL to flows endpoint
    , plInputsUrl :: Text -- URL to inputs endpoint
    , plOutputsUrl :: Text -- URL to outputs endpoint
    , plReferenceProductUrl :: Maybe Text -- URL to reference product (if exists)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityLinks)

-- | Activity statistics
data ActivityStats = ActivityStats
    { psInputCount :: Int
    , psOutputCount :: Int
    , psTotalExchanges :: Int
    , psLocation :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityStats)

-- | Flow with additional metadata. Carries either a tech or bio flow.
data FlowDetail = FlowDetail
    { fdFlow :: ApiFlow
    , fdUnitName :: Text -- Unit name for the flow
    , fdUsageCount :: Int -- How many activities use this flow
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowDetail)

{- | Exchange with flow, unit, and target activity information.
The carried flow's variant lines up with the Exchange variant.
-}
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: ApiFlow
    , edFlowUnitName :: Text -- Unit name for the flow's default unit
    , edUnit :: Unit -- Unit information for the exchange
    , edExchangeUnitName :: Text -- Unit name for the exchange's specific unit
    , edTargetActivity :: Maybe ActivitySummary -- Target activity for technosphere inputs
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExchangeDetail)

-- | A single filter entry returned in a preset
data ClassificationEntryInfo = ClassificationEntryInfo
    { ceiSystem :: !Text
    , ceiValue :: !Text
    , ceiMode :: !Text -- "exact" or "contains"
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped ClassificationEntryInfo)

-- | A named filter preset (from TOML config)
data ClassificationPresetInfo = ClassificationPresetInfo
    { cpiName :: !Text
    , cpiLabel :: !Text
    , cpiDescription :: !(Maybe Text)
    , cpiFilters :: ![ClassificationEntryInfo]
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped ClassificationPresetInfo)

-- | Classification system with its values for browsing/filtering
data ClassificationSystem = ClassificationSystem
    { csName :: Text -- e.g. "ISIC rev.4 ecoinvent", "CPC", "HS2017"
    , csValues :: [Text] -- Distinct values, sorted
    , csActivityCount :: Int -- How many activities have this system
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ClassificationSystem)

{- | Result of an /activity/{pid}/aggregate call.

A SQL-group-by-style aggregation over exchanges, supply chain entries, or
biosphere flows, depending on the requested scope.
-}
data Aggregation = Aggregation
    { aggScope :: Text -- echoed scope: "direct" | "supply_chain" | "biosphere"
    , aggFilteredTotal :: Double -- total summed across all matching items (after filters)
    , aggFilteredUnit :: Maybe Text -- Nothing when matched items have heterogeneous units
    , aggFilteredCount :: Int -- count of items matching the filters
    , aggGroups :: [AggregationGroup] -- one entry per group_by bucket (empty when group_by omitted)
    , aggWithheldDatabases :: [WithheldProcesses] -- Not in the total, the count nor the groups
    , aggCutoffInputs :: [CutoffInput] -- The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them; empty for the direct scope, which solves nothing
    , aggWithheldCutoffs :: [WithheldCutoffs] -- The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped Aggregation)

-- | One bucket in an aggregation result.
data AggregationGroup = AggregationGroup
    { aggKey :: Text
    , aggQuantity :: Double
    , aggUnit :: Maybe Text -- Nothing when group's items are heterogeneous
    , aggShare :: Maybe Double -- only set when aggregate=share
    , aggCount :: Int
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped AggregationGroup)

instance ToJSON NodeType
instance ToJSON EdgeType
instance ToJSON FlowRole

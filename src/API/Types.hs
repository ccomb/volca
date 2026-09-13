{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types where

import API.JsonOptions (Stripped (..))
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map as M
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Referenced (..), ToSchema (..), binarySchema, declareSchemaRef, enum_, format, nullable, properties, required, type_)
import qualified Data.OpenApi.Lens as OA
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.Author (
    AuthoredActivity (..),
    AuthoredExchange (..),
    ExchangeEdit (..),
    ExchangeSelector (..),
    FlowRef (..),
 )
import GHC.Generics
import Servant.API.ContentTypes (MimeRender (..), MimeUnrender (..), OctetStream)
import Types (
    BioDirection (..),
    BiosphereFlow (..),
    BlockerReason (..),
    Compartment (..),
    DocSection (..),
    Exchange,
    ExchangeKind (..),
    FlowKind (..),
    NativeActivityType (..),
    NativeProcessId (..),
    Pedigree,
    Severity,
    TechnosphereFlow (..),
    UUID,
    Unit,
    WasteFlow (..),
    WasteRole (..),
    exchangeKindName,
    parseMedium,
    unknownMedium,
 )

{- | Tagged wire representation of either side of the flow split.

Encodes as @{"kind":"technosphere","flow":{…TechnosphereFlow…}}@ or
@{"kind":"biosphere","flow":{…BiosphereFlow…}}@. The tag is the active
constructor; clients dispatch on @kind@ before reading @flow@.

We could derive this via Aeson's @TaggedObject@ sum encoding, but the
constructor names would leak into the wire, and the per-side flow records
would be flattened in-place rather than nested under @flow@. The hand-rolled
instances are short and they keep the on-the-wire shape symmetric with the
Exchange variant tag used elsewhere in the response.
-}
data ApiFlow
    = ApiTechFlow !TechnosphereFlow
    | ApiBioFlow !BiosphereFlow
    | ApiWasteFlow !WasteFlow
    | {- | The exchange referenced a flow UUID that resolved on neither side.
      Surfaced so consumers can spot a broken link instead of silently
      receiving a shorter list or a stub named @"unknown"@.
      -}
      ApiUnresolvedFlow !UUID
    deriving (Generic)

apiFlowId :: ApiFlow -> UUID
apiFlowId (ApiTechFlow f) = tfId f
apiFlowId (ApiBioFlow f) = bfId f
apiFlowId (ApiWasteFlow f) = wfId f
apiFlowId (ApiUnresolvedFlow uuid) = uuid

{- | Best-effort name for display. For unresolved flows we return the
@<unresolved flow UUID>@ sentinel – the same shape already used by
'ExchangeWithUnit'.
-}
apiFlowName :: ApiFlow -> Text
apiFlowName (ApiTechFlow f) = tfName f
apiFlowName (ApiBioFlow f) = bfName f
apiFlowName (ApiWasteFlow f) = wfName f
apiFlowName (ApiUnresolvedFlow uuid) = unresolvedFlowName uuid

{- | Sentinel display name for an exchange whose flow UUID resolves on
neither side. Kept in one place so HTTP and graph paths agree.
-}
unresolvedFlowName :: UUID -> Text
unresolvedFlowName uuid = "<unresolved flow " <> T.pack (show uuid) <> ">"

apiFlowSynonyms :: ApiFlow -> M.Map Text (S.Set Text)
apiFlowSynonyms (ApiTechFlow f) = tfSynonyms f
apiFlowSynonyms (ApiBioFlow f) = bfSynonyms f
apiFlowSynonyms (ApiWasteFlow f) = wfSynonyms f
apiFlowSynonyms (ApiUnresolvedFlow _) = M.empty

-- | Lift a 'FlowKind' into the wire wrapper. Three constructors, three arms.
apiFlowOfKind :: FlowKind -> ApiFlow
apiFlowOfKind (TechKind f) = ApiTechFlow f
apiFlowOfKind (BioKind f) = ApiBioFlow f
apiFlowOfKind (WasteKind f) = ApiWasteFlow f

instance ToJSON ApiFlow where
    toJSON (ApiTechFlow f) = object ["kind" .= ("technosphere" :: Text), "flow" .= f]
    toJSON (ApiBioFlow f) = object ["kind" .= ("biosphere" :: Text), "flow" .= f]
    toJSON (ApiWasteFlow f) = object ["kind" .= ("waste" :: Text), "flow" .= f]
    toJSON (ApiUnresolvedFlow uuid) = object ["kind" .= ("unresolved" :: Text), "id" .= uuid]
    toEncoding (ApiTechFlow f) = pairs ("kind" .= ("technosphere" :: Text) <> "flow" .= f)
    toEncoding (ApiBioFlow f) = pairs ("kind" .= ("biosphere" :: Text) <> "flow" .= f)
    toEncoding (ApiWasteFlow f) = pairs ("kind" .= ("waste" :: Text) <> "flow" .= f)
    toEncoding (ApiUnresolvedFlow uuid) = pairs ("kind" .= ("unresolved" :: Text) <> "id" .= uuid)

instance FromJSON ApiFlow where
    parseJSON = withObject "ApiFlow" $ \o -> do
        kind <- o .: "kind" :: Parser Text
        case kind of
            "technosphere" -> ApiTechFlow <$> o .: "flow"
            "biosphere" -> ApiBioFlow <$> o .: "flow"
            "waste" -> ApiWasteFlow <$> o .: "flow"
            "unresolved" -> ApiUnresolvedFlow <$> o .: "id"
            other -> fail $ "ApiFlow.kind must be \"technosphere\", \"biosphere\", \"waste\", or \"unresolved\", got: " <> T.unpack other

{- | Manual schema for ApiFlow – discriminated by 'kind' so OpenAPI consumers
see a real tagged union instead of a generic Either.
-}
instance ToSchema ApiFlow where
    declareNamedSchema _ = do
        techRef <- declareSchemaRef (Proxy :: Proxy TechnosphereFlow)
        bioRef <- declareSchemaRef (Proxy :: Proxy BiosphereFlow)
        let kindEnum =
                mempty
                    & type_ ?~ OpenApiString
                    & enum_
                        ?~ [ toJSON ("technosphere" :: Text)
                           , toJSON ("biosphere" :: Text)
                           , toJSON ("unresolved" :: Text)
                           ]
            tech =
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("kind", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ [toJSON ("technosphere" :: Text)]))
                            , ("flow", techRef)
                            ]
                    & required .~ ["kind", "flow"]
            bio =
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("kind", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ [toJSON ("biosphere" :: Text)]))
                            , ("flow", bioRef)
                            ]
                    & required .~ ["kind", "flow"]
            unresolved =
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("kind", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ [toJSON ("unresolved" :: Text)]))
                            , ("id", Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid"))
                            ]
                    & required .~ ["kind", "id"]
        pure $
            NamedSchema (Just "ApiFlow") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("kind", Inline kindEnum)
                            ]
                    & required .~ ["kind"]
                    & OA.oneOf ?~ [Inline tech, Inline bio, Inline unresolved]

{- | Search response combining results and count. ToSchema is added below
via a standalone deriving (needed because of the `(ToSchema a) =>` context).
-}
data SearchResults a = SearchResults
    { srResults :: [a] -- The actual search results
    , srTotal :: Int -- Total count of all matching items (before pagination)
    , srOffset :: Int -- Starting offset for pagination
    , srLimit :: Int -- Maximum number of results requested
    , srHasMore :: Bool -- Whether there are more results available
    , srSearchTimeMs :: Double -- Search execution time in milliseconds
    }
    deriving (Generic)

deriving via (Stripped (SearchResults a)) instance (ToJSON a) => ToJSON (SearchResults a)
deriving via (Stripped (SearchResults a)) instance (FromJSON a) => FromJSON (SearchResults a)
deriving via (Stripped (SearchResults a)) instance (ToSchema a) => ToSchema (SearchResults a)

-- | Minimal activity information for navigation
data ActivitySummary = ActivitySummary
    { prsProcessId :: Text -- ProcessId format: activity_uuid_product_uuid
    , prsActivityName :: Text
    , prsLocation :: Text
    , prsProductName :: Text -- Reference product name
    , prsProductAmount :: Double -- Reference product amount
    , prsProductUnit :: Text -- Reference product unit name
    , prsAllocationPercent :: Maybe Double -- SimaPro coproduct allocation (%, 0..100); Nothing for non-allocated bases
    , prsAllocationFormula :: Maybe Text -- Raw SimaPro allocation formula; Nothing if purely numeric
    , prsMassAllocationPercent :: Maybe Double -- What the share would be if the key were the product's mass (%, 0..100), to be read beside the declared one; Nothing outside a block's own product list, on a block of one product or one whose source declares no share, and when its products are not all stated in a mass
    , prsNativeType :: Maybe NativeActivityType -- Source-native activity type (ecospold @activityType, SimaPro Type, ILCD processType); Nothing when source lacks the field
    , prsBlock :: Text
    {- ^ The source block this process came out of, as a name to compare and
    never to read: the coproducts of one block carry the same one and nothing
    else does. A listing gathers its rows on it, which the process id cannot
    do: it names the product, not the block it was written in.
    -}
    , prsBlockProducts :: Int
    {- ^ How many products that block holds, which is not the number of rows
    a page shows of it: a page holding the last row of a block has no other
    way to say the block goes on.
    -}
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivitySummary)

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

-- Synonym types removed - synonyms are now included directly in flow responses

-- | Method summary for listing methods
data MethodSummary = MethodSummary
    { msmId :: UUID -- Method UUID
    , msmName :: Text -- Method name
    , msmCategory :: Text -- Impact category
    , msmUnit :: Text -- Reference unit (e.g., "kg CO2 eq")
    , msmFactorCount :: Int -- Number of characterization factors
    , msmCollection :: Text -- Parent collection name (e.g., "ef-31")
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodSummary)

-- | Method collection list response
newtype MethodCollectionListResponse = MethodCollectionListResponse
    { mclMethods :: [MethodCollectionStatusAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped MethodCollectionListResponse)

-- | Method collection status for API responses
data MethodCollectionStatusAPI = MethodCollectionStatusAPI
    { mcaName :: Text -- Internal identifier
    , mcaDisplayName :: Text -- Human-readable name
    , mcaDescription :: Maybe Text -- Optional description
    , mcaStatus :: Text -- "loaded" | "unloaded"
    , mcaIsUploaded :: Bool -- True if uploaded
    , mcaPath :: Text -- Data path
    , mcaMethodCount :: Int -- Number of impact categories (0 if unloaded)
    , mcaFormat :: Maybe Text -- Format (e.g. "ILCD")
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped MethodCollectionStatusAPI)

-- | Reference data list response (flow synonyms, compartment mappings, units)
newtype RefDataListResponse = RefDataListResponse
    { rdlItems :: [RefDataStatusAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RefDataListResponse)

-- | Reference data status for API responses
data RefDataStatusAPI = RefDataStatusAPI
    { rdaName :: Text
    , rdaDisplayName :: Text
    , rdaDescription :: Maybe Text
    , rdaStatus :: Text -- "loaded" | "unloaded"
    , rdaIsUploaded :: Bool
    , rdaIsAuto :: Bool
    , rdaEntryCount :: Int
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RefDataStatusAPI)

-- | Synonym groups response
newtype SynonymGroupsResponse = SynonymGroupsResponse
    { sgrGroups :: [[Text]]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SynonymGroupsResponse)

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
    deriving (ToJSON, ToSchema) via (Stripped MethodDetail)

{- | Characterization factor for API response. A method routinely carries
several CFs sharing one flow name – same substance emitted to air vs. water,
or one regionalized CF per location – so the distinguishing axes
(compartment, location, unit) travel with each row instead of leaving
consumers with apparent duplicates.
-}
data MethodFactorAPI = MethodFactorAPI
    { mfaFlowRef :: UUID -- ILCD flow UUID
    , mfaFlowName :: Text -- Flow name
    , mfaDirection :: Text -- "Input" or "Output"
    , mfaValue :: Double -- CF value
    , mfaUnit :: Maybe Text -- CF reference unit (e.g. "kg", "kBq"); Nothing when the source method states none
    , mfaCompartment :: Maybe Text -- e.g. "air/urban air", "water/unspecified/long-term"
    , mfaLocation :: Maybe Text -- Consumer location for regionalized CFs
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodFactorAPI)

-- | A single flow's contribution to an LCIA score
data FlowContributionEntry = FlowContributionEntry
    { fcoFlowName :: Text -- Biosphere flow name (e.g. "Carbon dioxide, fossil")
    , fcoContribution :: Double -- Contribution in impact unit
    , fcoSharePct :: Double -- Percentage of total score (0-100)
    , fcoFlowId :: Text -- Flow UUID for disambiguation
    , fcoCategory :: Text -- Medium only (e.g. "air")
    , fcoCompartment :: Maybe Text -- Sub-compartment (e.g. "urban air")
    , fcoCfValue :: Double -- Raw characterization factor value
    , fcoMatchKind :: Maybe Text -- How the factor was found ("exact_name", "cas_number", …); absent for a flow no rung of the cascade reached
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowContributionEntry)

-- | LCIA result for a single impact category
data LCIAResult = LCIAResult
    { lrMethodId :: UUID -- Method UUID
    , lrMethodName :: Text -- Method name
    , lrCategory :: Text -- Impact category
    , lrDamageCategory :: Text -- Parent damage category (may == category)
    , lrScore :: Double -- Total impact score (raw)
    , lrUnit :: Text -- Unit (e.g., "kg CO2 eq")
    , lrNormalizedScore :: Maybe Double -- score * normalization factor
    , lrWeightedScore :: Maybe Double -- normalized * weight (in Pt)
    , lrMappedFlows :: Int -- Number of flows successfully mapped
    , lrFunctionalUnit :: Text -- What the score is per: one unit of the reference product, e.g. "1.00 kg of Butter, unsalted"
    , lrTopContributors :: [FlowContributionEntry] -- Top contributing elementary flows
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped LCIAResult)

-- | Contributing flows result: top elementary flows for a specific impact category
data ContributingFlowsResult = ContributingFlowsResult
    { cfrMethod :: Text
    , cfrUnit :: Text
    , cfrTotalScore :: Double
    , cfrTopFlows :: [FlowContributionEntry]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ContributingFlowsResult)

-- | A single activity's contribution to an LCIA score
data ActivityContribution = ActivityContribution
    { acProcessId :: Text -- "activityUUID_productUUID" – usable as API process_id
    , acActivityName :: Text -- e.g. "electricity production, nuclear"
    , acProductName :: Text -- e.g. "electricity, medium voltage"
    , acLocation :: Text -- e.g. "FR"
    , acContribution :: Double -- Contribution in impact unit
    , acSharePct :: Double -- Percentage of total score (0-100)
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ActivityContribution)

-- | Contributing activities result: top upstream activities for a specific impact category
data ContributingActivitiesResult = ContributingActivitiesResult
    { carMethod :: Text
    , carUnit :: Text
    , carTotalScore :: Double
    , carActivities :: [ActivityContribution]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ContributingActivitiesResult)

-- | Batch impacts request: compute LCIA for every process in one call.
newtype BatchImpactsRequest = BatchImpactsRequest
    { birProcessIds :: [Text]
    }
    deriving (Generic)
    deriving (FromJSON, ToSchema) via (Stripped BatchImpactsRequest)

-- | One entry of a batch impacts response.
data BatchImpactsEntry = BatchImpactsEntry
    { bieProcessId :: Text
    , bieActivityName :: Text
    , bieImpacts :: LCIABatchResult
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped BatchImpactsEntry)

{- | Batch impacts response: one entry per successfully computed process,
plus lists of process ids that could not be resolved.
-}
data BatchImpactsResponse = BatchImpactsResponse
    { birResults :: [BatchImpactsEntry]
    , birNotFound :: [Text]
    , birInvalid :: [Text]
    , birUnscorable :: [Text]
    {- ^ Ids that resolve but name an activity the allocation gate refused; the
    quality report's @unallocated@ check says why for each
    -}
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped BatchImpactsResponse)

{- | A single scoring-set indicator: the per-variable normalized-weighted value
plus the impact category it came from. Value is pre-multiplied by the
scoring set's display multiplier, expressed in the set's display unit.
-}
data ScoringIndicator = ScoringIndicator
    { siCategory :: Text
    , siValue :: Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoringIndicator)

-- | Batch LCIA result with optional single score
data LCIABatchResult = LCIABatchResult
    { lbrResults :: [LCIAResult]
    , lbrSingleScore :: Maybe Double -- sum of weighted scores (Pt)
    , lbrSingleScoreUnit :: Maybe Text -- "Pt"
    , lbrNormWeightSetName :: Maybe Text
    , lbrAvailableNWsets :: [Text]
    , lbrScoringResults :: M.Map Text (M.Map Text Double)
    -- ^ Scoring set name → (score name → value). All formula-based scoring sets computed at once.
    , lbrScoringUnits :: M.Map Text Text
    -- ^ Scoring set name → display unit (e.g., "Pts", "µPts PEF")
    , lbrScoringIndicators :: M.Map Text (M.Map Text ScoringIndicator)
    -- ^ Scoring set name → (variable name → indicator). One row per scoring variable.
    , lbrCutoffWaste :: [CutoffWasteFlow]
    {- ^ Orphan waste exchanges on the scored activity – flows the dataset author
    left unmodelled. They contribute 0 to the score; surfacing them lets
    consumers see what's excluded rather than silently undercounting.
    -}
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped LCIABatchResult)

{- | A single orphan waste exchange on a scored activity: the dataset author
declared this waste output but did not link it to a treatment activity,
and no other loaded database provides an explicit match either. The score
excludes it; this record makes that exclusion visible.
-}
data CutoffWasteFlow = CutoffWasteFlow
    { cwfFlowId :: UUID
    {- ^ Waste flow UUID – lets consumers programmatically address the
    cut-off (e.g. to propose a treatment activity that would close it).
    -}
    , cwfFlowName :: Text
    , cwfAmount :: Double
    , cwfUnit :: Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CutoffWasteFlow)

-- | Flow mapping status for a method
data MappingStatus = MappingStatus
    { mstMethodId :: UUID -- Method UUID
    , mstMethodName :: Text -- Method name
    , mstTotalFactors :: Int -- Total CFs in method
    , mstMappedByUUID :: Int -- Matched by exact UUID
    , mstMappedByCAS :: Int -- Matched by CAS number
    , mstMappedByName :: Int -- Matched by normalized name
    , mstMappedBySynonym :: Int -- Matched via synonym group
    , mstUnmapped :: Int -- Not matched
    , mstCoverage :: Double -- Percentage of mapped flows (0-100)
    , mstDbBiosphereCount :: Int -- Total biosphere flows in the DB
    , mstUniqueDbFlowsMatched :: Int -- Distinct DB flows the method characterizes, counted with the same lookup scoring uses (fallbacks included)
    , mstUnmappedFlows :: [UnmappedFlowAPI] -- Details of unmapped flows
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MappingStatus)

-- | Details about an unmapped flow
data UnmappedFlowAPI = UnmappedFlowAPI
    { ufaFlowRef :: UUID -- Flow UUID in method
    , ufaFlowName :: Text -- Flow name in method
    , ufaDirection :: Text -- "Input" or "Output"
    , ufaCompartment :: Maybe Text -- The compartment the method states, rendered as the factors route renders it ("air", "water/groundwater/long-term"); Nothing when the row states none
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped UnmappedFlowAPI)

-- | DB-flow-centric mapping: all biosphere flows with their CF assignments
data FlowCFMapping = FlowCFMapping
    { fcmMethodName :: Text
    , fcmMethodUnit :: Text
    , fcmTotalFlows :: Int -- Total biosphere flows in DB
    , fcmMatchedFlows :: Int -- How many have a CF
    , fcmFlows :: [FlowCFEntry]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowCFMapping)

-- | A single DB biosphere flow with its CF assignment (if any)
data FlowCFEntry = FlowCFEntry
    { fceFlowId :: UUID
    , fceFlowName :: Text
    , fceFlowCategory :: Text
    , fceCfValue :: Maybe Double -- CF value (Nothing if no match)
    , fceCfFlowName :: Maybe Text -- Method CF flow name
    , fceMatchStrategy :: Maybe Text -- "uuid" | "name" | "synonym"
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowCFEntry)

{- | How much of one database a whole method collection characterizes: the
distinct emission and resource flows that at least one of its methods
resolves a factor for, probed with the same lookup scoring uses.

Distinct is the point. A collection's methods overlap heavily – every
climate-change variant characterizes the same gases – so summing per-method
figures counts a flow once per method that reaches it, and no sum of the
per-method mapping statuses can recover this number.
-}
data CollectionCoverage = CollectionCoverage
    { ccvCollection :: Text
    , ccvDbName :: Text
    , ccvTotalFlows :: Int -- Emission and resource flows the database carries
    , ccvCharacterizedFlows :: Int -- Distinct flows at least one method characterizes
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CollectionCoverage)

-- | Characterization result: matched CFs for a method in a database
data CharacterizationResult = CharacterizationResult
    { chrMethod :: Text
    , chrUnit :: Text
    , chrMatches :: Int
    , chrShown :: Int
    , chrFactors :: [CharacterizationEntry]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CharacterizationResult)

-- | A single matched characterization factor
data CharacterizationEntry = CharacterizationEntry
    { cheMethodFlowName :: Text -- CF flow name from method
    , cheCfValue :: Double -- Characterization factor
    , cheCfUnit :: Text -- CF unit (e.g. "kg")
    , cheDirection :: Text -- "Input" or "Output"
    , cheDbFlowName :: Text -- Matched DB flow name
    , cheFlowId :: Text -- DB flow UUID
    , cheFlowUnit :: Text -- DB flow default unit (e.g. "m3", "kg")
    , cheCategory :: Text -- Flow category
    , cheCompartment :: Maybe Text -- Sub-compartment
    , cheMatchStrategy :: Text -- 'Method.Mapping.strategyToText' of the rung that matched
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CharacterizationEntry)

{- | Why one flow scores with the factor it does.

Two layers on purpose. 'ecrExplanation' is written by the engine and is what a
reader should be shown; the structured fields below it are for a consumer that
wants to compare, filter or link, not for one that wants to restate the
sentences in its own words.
-}
data ExplainCFResult = ExplainCFResult
    { ecrMethod :: Text
    , ecrMethodUnit :: Text
    , ecrFlow :: ExplainedFlowAPI
    , ecrOutcome :: Text -- "characterized" | "conversion_refused" | "no_factor"
    , ecrExplanation :: [Text] -- Engine-authored sentences, in reading order
    , ecrMatch :: Maybe ExplainedMatchAPI -- Absent when nothing reaches the flow
    , ecrStepsTried :: [ExplainedStepAPI]
    , ecrRegionalFactorCount :: Int -- Factors this flow holds that vary by the consuming activity's location
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainCFResult)

-- | The flow being explained, as the cascade sees it after normalization.
data ExplainedFlowAPI = ExplainedFlowAPI
    { eflId :: Text
    , eflName :: Text
    , eflUnit :: Text
    , eflCategory :: Text
    , eflCompartment :: Maybe Text
    , eflCas :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainedFlowAPI)

-- | The factor that was served, and where it came from.
data ExplainedMatchAPI = ExplainedMatchAPI
    { emaRung :: Text -- How it was found: "exact_name", "cas_number", "energy_content", …
    , emaCfValue :: Double
    , emaCfUnit :: Text
    , emaMethodFlowName :: Text -- The method line that authored the factor
    , emaMethodCas :: Maybe Text
    , emaMatchStrategy :: Text -- How that line was attached at load time
    , emaUnitConversion :: Maybe Text -- How the amount reached the factor's basis
    , emaRefusal :: Maybe Text -- Why it could not, when outcome is conversion_refused
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainedMatchAPI)

-- | One rung of the cascade, and what it made of the flow.
data ExplainedStepAPI = ExplainedStepAPI
    { estRung :: Text
    , estResult :: Text -- "hit" | "miss" | "not_applicable" | "vetoed" | "ambiguous"
    , estVeto :: Maybe Text -- Which rule vetoed, when result is "vetoed"
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainedStepAPI)

-- | Database list response
newtype DatabaseListResponse = DatabaseListResponse
    { dlrDatabases :: [DatabaseStatusAPI] -- All available databases
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DatabaseListResponse)

-- | Database status for API responses
data DatabaseStatusAPI = DatabaseStatusAPI
    { dsaName :: Text -- Internal identifier (slug)
    , dsaDisplayName :: Text -- Human-readable name for UI
    , dsaDescription :: Maybe Text
    , dsaLoadAtStartup :: Bool -- Configured to load at startup
    , dsaStatus :: Text -- "unloaded" | "partially_linked" | "loaded"
    , dsaIsUploaded :: Bool -- True if path starts with "uploads/"
    , dsaPath :: Text -- Data path
    , dsaFormat :: Maybe Text -- Database format (EcoSpold 2, EcoSpold 1, SimaPro CSV)
    , dsaActivityCount :: Int -- Number of activities (0 if unloaded)
    , dsaDependsOn :: [Text] -- Names of databases this one depends on (for cross-DB linking)
    , dsaAllocation :: Text
    {- ^ The key its multi-output blocks were divided under: @declared@, @dry
    mass@ or @wet mass@. On the wire because a reader looking at a column of
    shares has no other way to tell whether the source stated them or a
    property recomputed them, and a column headed @declared@ over recomputed
    shares says something untrue.
    -}
    , dsaSource :: Maybe Text -- The database whose files it reads, when it owns none (a copy, or a re-keyed load)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DatabaseStatusAPI)

-- | Response for database activation
data ActivateResponse = ActivateResponse
    { arSuccess :: Bool
    , arMessage :: Text
    , arDatabase :: Maybe DatabaseStatusAPI
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivateResponse)

{- | What @GET /api/v1/hosting@ answers: the hosting limits a client may want
to explain before running into them. One type rather than an inline object so
the wire shape has a name a golden test can pin. Keys are written by hand
because this endpoint's wire is snake_case.
-}
data HostingInfo = HostingInfo
    { hiIsHosted :: Bool
    , hiMaxUploads :: Int
    , hiMaxUploadMb :: Int
    , hiMaxLoadedUploads :: Int
    , hiApiAccess :: Bool
    , hiReadOnly :: Bool
    , hiReadOnlyMessage :: Text
    , hiUpgradeUpload :: Text
    , hiUpgradeApi :: Text
    , hiUpgradeVmSize :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON HostingInfo where
    toJSON hi =
        object
            [ "is_hosted" .= hiIsHosted hi
            , "max_uploads" .= hiMaxUploads hi
            , "max_upload_mb" .= hiMaxUploadMb hi
            , "max_loaded_uploads" .= hiMaxLoadedUploads hi
            , "api_access" .= hiApiAccess hi
            , "read_only" .= hiReadOnly hi
            , "read_only_message" .= hiReadOnlyMessage hi
            , "upgrade_upload" .= hiUpgradeUpload hi
            , "upgrade_api" .= hiUpgradeApi hi
            , "upgrade_vm_size" .= hiUpgradeVmSize hi
            ]

{- | Response for the re-link endpoint: fresh cross-DB link stats after a
second-pass linking against the currently-loaded databases.
-}
data RelinkResponse = RelinkResponse
    { rrDbName :: Text
    , rrUnresolvedBefore :: Int
    , rrUnresolvedAfter :: Int
    , rrCrossDBLinks :: Int
    , rrDependsOn :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RelinkResponse)

{- | Request body for the relink endpoint. Both fields absent (an empty @{}@
body) means a plain relink – re-resolve links within the existing pin. Both
present switches to mapping mode: relink against @rmrDepDb@ using the inline
name→name supplier-alias CSV in @rmrMappingCsv@ (sent inline so the client can
forward a local file without the server needing filesystem access). Supplying
exactly one is rejected – they are only meaningful together.
-}
data RelinkRequest = RelinkRequest
    { rmrDepDb :: Maybe Text
    -- ^ Dependency database to link against (must be a declared dependency)
    , rmrMappingCsv :: Maybe Text
    -- ^ Mapping CSV content (header row + source/target columns)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RelinkRequest)

-- | One consuming process of a supplier-gap entry.
data GapConsumerAPI = GapConsumerAPI
    { gcaProcessId :: Text
    , gcaActivityName :: Text
    , gcaProductName :: Text
    , gcaLocation :: Text
    , gcaEdges :: Int
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped GapConsumerAPI)

{- | One supplier gap, aggregated per (product name, location, unit) so
@demandSum@ never mixes units. @reasons@ carries every stable blocker code the
product was refused under ('Types.blockerReason'), plus
@dangling_source_identity@ for inputs whose named source activity no dependency
ships, and @unlinked_waste_input@ for treatment-side waste inputs with no
internal producer. @reason@ and @detail@ are the first of them, kept so a client
written before @reasons@ still decodes; what they answer moves all the same, and
they cannot be made not to. @reason@ used to name whichever blocker the linking
runs merged first and now names the most demanded, and @detail@ is left out
where the refusals under one code disagree on it, having carried one of them
before. A product refused two ways is the ordinary case: read @reasons@.
-}
data GapEntryAPI = GapEntryAPI
    { gaeName :: Text
    , gaeLocation :: Text
    , gaeUnit :: Text
    , gaeReasons :: [BlockerReason]
    , gaeReason :: Text
    , gaeDetail :: Maybe Text
    , gaeEdges :: Int
    , gaeConsumers :: Int
    , gaeDemandSum :: Double
    , gaeTopConsumers :: [GapConsumerAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped GapEntryAPI)

{- | Supplier-gap report of a database: everything still unsupplied after
internal resolution and cross-DB linking, aggregated and ranked by demanding
edges – the work list for switching or completing a background dependency.
-}
data GapReportAPI = GapReportAPI
    { graDbName :: Text
    , graTotalInputs :: Int
    , graInternalLinks :: Int
    , graCrossDBLinks :: Int
    , graUnresolvedEdges :: Int
    , graUnresolvedProducts :: Int
    , graCompleteness :: Double
    , graGaps :: [GapEntryAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped GapReportAPI)

-- | One dataset-soundness finding: where it was found, and what is wrong.
data QualityOffenderAPI = QualityOffenderAPI
    { qoaSeverity :: Severity
    , qoaProcessId :: Text -- "activityUUID_productUUID" – usable as API process_id
    , qoaActivityName :: Text
    , qoaLocation :: Text
    , qoaProductName :: Maybe Text
    , qoaDetail :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped QualityOffenderAPI)

{- | One check of the quality report. @offenderCount@ always covers the whole
finding list, so a list capped by @limit@ stays countable – never a silent cap.
@applicable@ is 'False' when the database carries nothing the check could judge,
which is not the same as passing it.
-}
data QualityCheckAPI = QualityCheckAPI
    { qcaApplicable :: Bool
    , qcaOffenderCount :: Int
    , qcaOffenders :: [QualityOffenderAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped QualityCheckAPI)

{- | Dataset-soundness report of a database: the structural defects a score
can't reveal, one named field per check. The methodological counterpart of
'GapReportAPI', which reports what a database is missing rather than what is
malformed in it.
-}
data QualityReportAPI = QualityReportAPI
    { qraDbName :: Text
    , qraProcessCount :: Int
    , qraReferenceProduct :: QualityCheckAPI
    , qraAllocationSums :: QualityCheckAPI
    , qraDuplicateActivities :: QualityCheckAPI
    , qraDuplicateProducts :: QualityCheckAPI
    , qraSuspiciousAmounts :: QualityCheckAPI
    , qraMissingMetadata :: QualityCheckAPI
    , qraUndeclaredGeography :: QualityCheckAPI
    , qraFormulaConsistency :: QualityCheckAPI
    , qraTruncatedNameCollisions :: QualityCheckAPI
    , qraMissingPedigree :: QualityCheckAPI
    , qraUnconsumedProducts :: QualityCheckAPI
    , qraUnsuppliedInputs :: QualityCheckAPI
    , qraObsoleteInputs :: QualityCheckAPI
    , qraLandTransformationBalance :: QualityCheckAPI
    , qraOxygenDemandOrder :: QualityCheckAPI
    , qraInvalidCas :: QualityCheckAPI
    , qraAllocationOutOfRange :: QualityCheckAPI
    , qraUnallocated :: QualityCheckAPI
    , qraUnmeasurableAmounts :: QualityCheckAPI
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped QualityReportAPI)

{- | Computed-checks report of a loaded database: what the data computes,
judged against the catalogue's own norms. Same check and offender shape as
'QualityReportAPI', so consumers render both reports alike; a separate
report because these checks need matrices and a method collection, which
the structural report deliberately does not.
-}
data ComputedQualityReportAPI = ComputedQualityReportAPI
    { cqaDbName :: Text
    , cqaCollection :: Text
    , cqaProcessCount :: Int
    , cqaScoreOutliers :: QualityCheckAPI
    , cqaZeroScores :: QualityCheckAPI
    , cqaNegativeScores :: QualityCheckAPI
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ComputedQualityReportAPI)

-- | One database flow a method scores only through a name bridge.
data BridgedFlowAPI = BridgedFlowAPI
    { cvfFlowName :: Text
    , cvfStrategy :: Text -- how the match was reached: 'Method.Mapping.strategyToText'
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BridgedFlowAPI)

{- | The database flows that bridge to one factor. @methodName@ is the name the
method carries for the substance – the name each bridged flow should be renamed
to for the database to score in an exact-name tool.
-}
data BridgeGroupAPI = BridgeGroupAPI
    { cvgCas :: Maybe Text
    , cvgMethodName :: Text
    , cvgBridgedFlows :: [BridgedFlowAPI]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BridgeGroupAPI)

{- | One collection's coverage of the database. @characterizedFlows@ of
@totalFlows@ is the honest reach; @bridgeGroups@ is the subset reached only
through a bridge (empty means every scored flow matches by its exact name).
@bridgeGroupCount@ always covers the whole list, so a list capped by @limit@
stays countable – never a silent cap.
-}
data CollectionBridgesAPI = CollectionBridgesAPI
    { cvcCollection :: Text
    , cvcTotalFlows :: Int
    , cvcCharacterizedFlows :: Int
    , cvcBridgeGroupCount :: Int
    , cvcBridgeGroups :: [BridgeGroupAPI]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CollectionBridgesAPI)

{- | Characterization-coverage report of a database against the loaded method
collections: the flows each collection scores only through a synonym/CAS bridge,
which an exact-name consumer would score as zero. One entry
per loaded collection, so two method versions can be compared side by side.
-}
data CoverageReportAPI = CoverageReportAPI
    { cvrDbName :: Text
    , cvrCollections :: [CollectionBridgesAPI]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CoverageReportAPI)

-- | Result of auto-loading a single dependency
data DepLoadResult
    = DepLoaded {dlrName :: Text}
    | DepLoadFailed {dlfName :: Text, dlfError :: Text}
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DepLoadResult)

-- | Response for the load database endpoint
data LoadDatabaseResponse
    = LoadFailed {ldrError :: Text}
    | LoadSucceeded {ldrDatabase :: DatabaseStatusAPI, ldrDeps :: [DepLoadResult]}
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped LoadDatabaseResponse)

{- | One classification filter in a delete request: the @system@ to match, the
@value@ to look for, and whether the match is exact (else token-contains).
Mirrors the search endpoint's classification query parameters so the deleted
set is exactly the searched set.
-}
data DeleteClassFilter = DeleteClassFilter
    { dcfSystem :: Text
    , dcfValue :: Text
    , dcfExact :: Bool
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeleteClassFilter)

{- | Request for delete-by-selection. Two exclusive selection modes: the
filter fields select the whole matching set (pagination ignored), or @dsqIds@
names the set exactly – the filter fields must then be absent. @dsqKeep@
spares selected process ids and @dsqExtra@ adds ones the selection missed.
Process ids are the canonical @activityUUID_productUUID@ strings the UI/CLI
carry, not matrix indices.
-}

{- | One technosphere input: a product this activity consumes, named by the
process that supplies it.

@tiProvider@ is a @process_id@ (@activityUUID_productUUID@, or a bare activity
UUID when that activity has a single product) – the same currency the read
endpoints hand out. The flow follows from the supplier, so it is never stated
separately. @tiUnit@ defaults to the supplier's own reference unit; stating
another one is allowed as long as it converts.
-}
data TechInputAPI = TechInputAPI
    { tiProvider :: Text
    , tiAmount :: Double
    , tiUnit :: Maybe Text
    , tiComment :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped TechInputAPI)

{- | One biosphere exchange: a resource taken from the environment or an
emission released into it.

Either @beFlow@ names a flow by its identifier, or @beName@ with
@beCompartment@ and @beUnit@ names one in words, which reaches the flow the
vocabulary already declares under them and introduces one only when nothing
answers to them. Giving both is refused rather than guessed at. A biosphere
amount is never converted, so an exchange states its amount in the flow's own
unit.
-}
data BioExchangeAPI = BioExchangeAPI
    { beFlow :: Maybe Text
    , beName :: Maybe Text
    , beCompartment :: Maybe Text
    , beSubCompartment :: Maybe Text
    , beDirection :: Text
    -- ^ @resource@ (taken from the environment) or @emission@ (released into it)
    , beAmount :: Double
    , beUnit :: Maybe Text
    , beComment :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BioExchangeAPI)

{- | One waste output: a residue this activity hands to a treatment process.
@woProvider@ names that treatment process, exactly as a technosphere input
names its producer.
-}
data WasteOutputAPI = WasteOutputAPI
    { woProvider :: Text
    , woAmount :: Double
    , woUnit :: Maybe Text
    , woComment :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped WasteOutputAPI)

{- | An activity as a client writes it.

The inventory arrives as three lists rather than one tagged list, so a field
that makes sense for a supplier link cannot be sent on an emission and back
again. One reference product per activity: coproducts and allocation are a
later phase, and this shape does not pretend to accept them.

Identity is not sent. It is minted from the name, location, product and unit
below, so writing the same activity twice addresses the same row instead of
creating a second one.
-}
data ActivityInput = ActivityInput
    { aiName :: Text
    , aiLocation :: Text
    , aiDescription :: [Text]
    , aiProductName :: Text
    , aiProductAmount :: Double
    , aiProductUnit :: Text
    , aiInputs :: [TechInputAPI]
    , aiBiosphere :: [BioExchangeAPI]
    , aiWasteOutputs :: [WasteOutputAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityInput)

-- | A batch of activities to write.
newtype ActivityWriteRequest = ActivityWriteRequest
    { awrActivities :: [ActivityInput]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityWriteRequest)

{- | What a write produced: the process ids now addressable, whether they
survive an unload, and everything the engine wants the author to know but did
not refuse over.
-}
data ActivityWriteResponse = ActivityWriteResponse
    { awpWritten :: [Text]
    , awpTransient :: Bool
    {- ^ True when the write lives in memory only, because the database is one
    the engine reads from configuration rather than owns.
    -}
    , awpWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityWriteResponse)

{- | Which lines of an inventory an edit addresses.

@esKind@ is @input@, @waste@ or @biosphere@: the first two name their provider
by process id, the third names its flow by identity. The product side – the
reference product, coproducts, a treatment's reference input – has no kind
here, because changing those changes what the activity /is/, not what it
consumes.

A selector naming several lines applies to all of them, and the answer says
how many. Naming none is refused: an edit that quietly did nothing reads
exactly like one that worked.
-}
data ExchangeSelectorAPI = ExchangeSelectorAPI
    { esKind :: Text
    , esProvider :: Maybe Text
    , esFlow :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeSelectorAPI)

-- | The lines to restate, and the amount to restate them to.
data SetAmountAPI = SetAmountAPI
    { saSelect :: ExchangeSelectorAPI
    , saAmount :: Double
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SetAmountAPI)

{- | Changes to one activity's inventory: lines to drop, lines to restate,
lines to add.

Five lists rather than one tagged list, for the reason 'ActivityInput' has
three: what makes sense on an addition cannot be sent on a removal and back
again. They apply in the order they are listed here, so an edit that drops one
supplier and adds another is never ambiguous about which happened first.

Added lines are resolved exactly as written ones are – same provider lookup,
same unit rules – because an inventory should not be able to tell how a line
got there.
-}
data ExchangeEditRequest = ExchangeEditRequest
    { eerRemove :: [ExchangeSelectorAPI]
    , eerSetAmounts :: [SetAmountAPI]
    , eerAddInputs :: [TechInputAPI]
    , eerAddBiosphere :: [BioExchangeAPI]
    , eerAddWasteOutputs :: [WasteOutputAPI]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExchangeEditRequest)

{- | A list left unstated is a list of nothing – a document that only removes
a line should not have to say four times that it does nothing else. The
assistant tool already reads its arguments this way.
-}
instance FromJSON ExchangeEditRequest where
    parseJSON = withObject "ExchangeEditRequest" $ \o ->
        ExchangeEditRequest
            <$> o .:? "remove" .!= []
            <*> o .:? "setAmounts" .!= []
            <*> o .:? "addInputs" .!= []
            <*> o .:? "addBiosphere" .!= []
            <*> o .:? "addWasteOutputs" .!= []

{- | What an inventory edit produced: one count per selector, in the order the
selectors were stated. A caller that meant to drop one line and reads three
learns it here, rather than from a score that moved more than it should have.
-}
data ExchangeEditResponse = ExchangeEditResponse
    { eepRemoved :: [Int]
    , eepAmountsSet :: [Int]
    , eepAdded :: Int
    , eepTransient :: Bool
    -- ^ True when the edit lives in memory only and an unload would undo it.
    , eepWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeEditResponse)

data DeleteSelectionRequest = DeleteSelectionRequest
    { dsqName :: Maybe Text -- Filter by activity name
    , dsqLocation :: Maybe Text -- Filter by location
    , dsqProduct :: Maybe Text -- Filter by reference product
    , dsqClassifications :: [DeleteClassFilter] -- Classification filters (AND across systems)
    , dsqExact :: Maybe Bool -- Exact name match (default False)
    , dsqKeep :: [Text] -- Process-id strings to spare from deletion
    , dsqExtra :: [Text] -- Process-id strings to add to deletion
    , dsqIds :: Maybe [Text] -- Delete exactly these process ids (no filter fields allowed)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeleteSelectionRequest)

-- | Response for delete-by-selection: count of activities removed.
data DeleteSelectionResponse = DeleteSelectionResponse
    { dsrSuccess :: Bool
    , dsrMessage :: Text
    , dsrDeleted :: Int
    , dsrTransient :: Bool
    {- ^ True when the deletion lives in memory only, because the database is
    one the engine reads from configuration rather than owns. Reloading it
    brings the removed activities back, and a caller has to be able to say
    so rather than let the user assume the change stuck.
    -}
    , dsrWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeleteSelectionResponse)

{- | Request for database export. @exrFormat@ is the target-format keyword
(@simapro|ecospold1|ecospold2|ilcd|brightway@), matching the CLI.
-}
newtype ExportRequest = ExportRequest
    { exrFormat :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExportRequest)

-- | Response for database upload
data UploadResponse = UploadResponse
    { uprSuccess :: Bool
    , uprMessage :: Text
    , uprSlug :: Maybe Text -- Generated slug (if successful)
    , uprFormat :: Maybe Text -- Detected format (if successful)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped UploadResponse)

-- | Supply chain response – all upstream activities with scaling factors
data SupplyChainResponse = SupplyChainResponse
    { scrRoot :: ActivitySummary
    , scrTotalActivities :: Int
    , scrFilteredActivities :: Int
    , scrSupplyChain :: [SupplyChainEntry]
    , scrEdges :: [SupplyChainEdge]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped SupplyChainResponse)

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

{- | A single rank-1 perturbation of a technosphere coefficient @A_ij@.

@delta@ is __relative__: the resolved coefficient @a@ is multiplied by
@(1 + delta)@. So @delta = +0.05@ means \"+5%\", and @delta = -1.0@
removes the link entirely. The kernel passes @a * delta@ to 'perturbA'.
-}
data Perturbation = Perturbation
    { perConsumer :: Text -- Consumer ProcessId (column j of A) – root DB only in V1
    , perSupplier :: Text -- Supplier ProcessId (row i of A) – root DB only in V1
    , perDelta :: Double -- Relative perturbation of A_ij (1 + delta)
    , perLabel :: Maybe Text -- Optional label for response correlation
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Perturbation)

-- | Request body for POST sensitivity endpoints. Flat list, V1.
newtype SensitivityRequest = SensitivityRequest
    { srPerturbations :: [Perturbation]
    }
    deriving (Generic)
    deriving (FromJSON, ToSchema) via (Stripped SensitivityRequest)

{- | One result entry per perturbation. The 'peResult' carries either an
error message ('Left') or the (impact, deltaImpact) pair ('Right'). The
echoed @perturbation@ carries the @label@ if the caller supplied one, so
callers don't need to thread an out-of-band identifier. The wire format
flattens the Either: success → @{perturbation, impact, deltaImpact}@,
failure → @{perturbation, error}@.
-}
data PerturbedEntry = PerturbedEntry
    { pePerturbation :: Perturbation
    , peResult :: Either Text (LCIAResult, Double)
    }
    deriving (Generic)

{- | Manual schema for PerturbedEntry: the Either inside is flattened by ToJSON
to {perturbation, impact, deltaImpact} on success and {perturbation, error}
on failure. The Generic-derived schema would expose the Haskell shape
(a oneOf wrapper around the Either) instead of the flat wire format.
-}
instance ToSchema PerturbedEntry where
    declareNamedSchema _ = do
        pertRef <- declareSchemaRef (Proxy :: Proxy Perturbation)
        lciaRef <- declareSchemaRef (Proxy :: Proxy LCIAResult)
        doubleRef <- declareSchemaRef (Proxy :: Proxy Double)
        textRef <- declareSchemaRef (Proxy :: Proxy Text)
        pure $
            NamedSchema (Just "PerturbedEntry") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("perturbation", pertRef)
                            , ("impact", lciaRef)
                            , ("deltaImpact", doubleRef)
                            , ("error", textRef)
                            ]
                    & required .~ ["perturbation"]

-- | Sensitivity response: baseline LCIA + one entry per perturbation (in order).
data SensitivityResponse = SensitivityResponse
    { srBaseline :: LCIAResult
    , srPerturbed :: [PerturbedEntry]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped SensitivityResponse)

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

-- | Activity information optimized for API responses
data ActivityForAPI = ActivityForAPI
    { pfaProcessId :: Text -- ProcessId format: "activityUUID_productUUID"
    , pfaActivityName :: Text
    , pfaDescription :: [Text] -- Description par paragraphes
    , pfaDocumentation :: [DocSection] -- Provenance the dataset states about itself (source, technology, review); empty when the format records none
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

-- JSON instances. Record types derive ToJSON/FromJSON/ToSchema via the
-- API.JsonOptions.Stripped carrier, which strips the lowercase field prefix.
-- Sum-only types (NodeType, EdgeType, FlowRole) keep default derivation.
-- ToJSON / FromJSON / ToSchema for SearchResults a are standalone-derived
-- alongside the data declaration above (line ~169).
-- ConsumerResult, ConsumersResponse, ClassificationEntryInfo,
-- ClassificationPresetInfo, ClassificationSystem, Aggregation,
-- AggregationGroup, ActivitySummary, FlowSearchResult, InventoryMetadata,
-- InventoryStatistics, TreeExport, TreeMetadata, ExportNode now derive
-- ToJSON / FromJSON via Stripped attached to their data declarations.

{- | NativeActivityType is a sum type internally but serialises to a single
flat record so MCP / pyvolca consumers see one uniform shape regardless of
which source database produced the activity. Discriminator is the 'source'
field; format-specific fields (code, special_*) are null when irrelevant.

Hand-rolled (kept out of the Stripped DerivingVia path) because Stripped is
for records only; this is a sum-of-records flattened to one wire record.
-}
instance ToJSON NativeActivityType where
    toJSON = \case
        EcoSpoldActivityType code label specCode specLabel ->
            object
                [ "source" .= ("ecospold2" :: Text)
                , "label" .= label
                , "code" .= code
                , "special_code" .= specCode
                , "special_label" .= specLabel
                ]
        SimaProProcessType label ->
            object
                [ "source" .= ("simapro" :: Text)
                , "label" .= label
                , "code" .= Null
                , "special_code" .= Null
                , "special_label" .= Null
                ]
        ILCDProcessType label ->
            object
                [ "source" .= ("ilcd" :: Text)
                , "label" .= label
                , "code" .= Null
                , "special_code" .= Null
                , "special_label" .= Null
                ]
    toEncoding = \case
        EcoSpoldActivityType code label specCode specLabel ->
            pairs
                ( "source" .= ("ecospold2" :: Text)
                    <> "label" .= label
                    <> "code" .= code
                    <> "special_code" .= specCode
                    <> "special_label" .= specLabel
                )
        SimaProProcessType label ->
            pairs
                ( "source" .= ("simapro" :: Text)
                    <> "label" .= label
                    <> "code" .= Null
                    <> "special_code" .= Null
                    <> "special_label" .= Null
                )
        ILCDProcessType label ->
            pairs
                ( "source" .= ("ilcd" :: Text)
                    <> "label" .= label
                    <> "code" .= Null
                    <> "special_code" .= Null
                    <> "special_label" .= Null
                )

-- | Inverse of the ToJSON instance: discriminate on the @source@ field.
instance FromJSON NativeActivityType where
    parseJSON = withObject "NativeActivityType" $ \o -> do
        src <- o .: "source"
        label <- o .: "label"
        case (src :: Text) of
            "ecospold2" ->
                EcoSpoldActivityType
                    <$> o .: "code"
                    <*> pure label
                    <*> o .:? "special_code"
                    <*> o .:? "special_label"
            "simapro" -> pure (SimaProProcessType label)
            "ilcd" -> pure (ILCDProcessType label)
            other -> fail $ "Unknown NativeActivityType source: " <> T.unpack other

{- | Wire schema mirrors the flat ToJSON shape: a single object with a
'source' discriminator and source-specific fields that are null when
irrelevant. Co-located with the JSON instances above so that downstream
DerivingVia clauses (e.g. ToSchema for ActivitySummary, ActivityForAPI)
can resolve the instance without forming a circular dep on API.OpenApi.
-}
instance ToSchema NativeActivityType where
    declareNamedSchema _ = do
        let sourceEnum =
                mempty
                    & type_ ?~ OpenApiString
                    & enum_ ?~ [toJSON ("ecospold2" :: Text), toJSON ("simapro" :: Text), toJSON ("ilcd" :: Text)]
            labelSchema =
                mempty
                    & type_ ?~ OpenApiString
            nullableIntSchema =
                mempty
                    & type_ ?~ OpenApiInteger
                    & nullable ?~ True
            nullableTextSchema =
                mempty
                    & type_ ?~ OpenApiString
                    & nullable ?~ True
        pure $
            NamedSchema (Just "NativeActivityType") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("source", Inline sourceEnum)
                            , ("label", Inline labelSchema)
                            , ("code", Inline nullableIntSchema)
                            , ("special_code", Inline nullableIntSchema)
                            , ("special_label", Inline nullableTextSchema)
                            ]
                    & required .~ ["source", "label"]

{- | A native identifier reaches the wire as the bare string the source wrote,
because that is the only form it has: we neither parse it nor compare it to
anything but itself, and a reader is going to paste it back into the file it
came from.
-}
instance ToJSON NativeProcessId where
    toJSON (NativeProcessId nativeId) = toJSON nativeId

instance FromJSON NativeProcessId where
    parseJSON = fmap NativeProcessId . parseJSON

instance ToSchema NativeProcessId where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToJSON NodeType
instance ToJSON EdgeType
instance ToJSON FlowRole

{- | A kind reaches the wire as the word a request would name it by, so what a
result reports and what a filter accepts are the same three strings.
-}
instance ToJSON ExchangeKind where
    toJSON = toJSON . exchangeKindName

instance ToSchema ExchangeKind where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ExchangeKind") $
                mempty
                    & type_ ?~ OpenApiString
                    & enum_ ?~ map (toJSON . exchangeKindName) [minBound .. maxBound]

-- ToJSON / FromJSON / ToSchema for Unit are derived via Stripped alongside
-- its data declaration in src/Types.hs.
-- CutoffWasteFlow (added on main) derives via Stripped attached to its data
-- declaration above.

-- Custom ToJSON for PerturbedEntry: flatten the Either so success entries
-- have impact+deltaImpact and error entries have error.
instance ToJSON PerturbedEntry where
    toJSON (PerturbedEntry p result) =
        object $
            ("perturbation" .= p) : case result of
                Left err -> ["error" .= err]
                Right (lcia, d) -> ["impact" .= lcia, "deltaImpact" .= d]

-- FromJSON instances needed for API conversion
-- (FromJSON (SearchResults a) above)

-- openapi3 cannot derive ToSchema for BSL.ByteString directly
newtype BinaryContent = BinaryContent BSL.ByteString

instance MimeRender OctetStream BinaryContent where
    mimeRender _ (BinaryContent bs) = bs

{- | One raw chunk of a streamed octet-stream upload request body.

A newtype around strict 'BS.ByteString' for two reasons: openapi3 refuses a
'ToSchema' for a bare 'ByteString' (we give it a binary schema here), and the
'StreamBody' decoder needs a 'MimeUnrender' target for the chunk type.
-}
newtype UploadChunk = UploadChunk {unUploadChunk :: BS.ByteString}

instance MimeUnrender OctetStream UploadChunk where
    mimeUnrender _ = Right . UploadChunk . BSL.toStrict

instance ToSchema UploadChunk where
    declareNamedSchema _ = pure $ NamedSchema (Just "UploadChunk") binarySchema

-- ---------------------------------------------------------------------------
-- Wire → authoring input
-- ---------------------------------------------------------------------------

{- | Translate the request activities into authoring inputs. Shared by the
HTTP endpoints and the command line, which read the same JSON document.

What can fail here is only shape – a biosphere line that names its flow in
two ways at once or in none, a direction that is neither of the two, a flow
introduced without the unit that is part of its identity; every value is the
validator's to judge. Complaints accumulate across the whole batch and name
the activity they belong to, exactly as the validator's do, so a batch is
fixed in one round trip whichever layer refused it.
-}
toAuthoredActivities :: [ActivityInput] -> Either [Text] [AuthoredActivity]
toAuthoredActivities inputs = case partitionEithers (map toAuthoredActivity inputs) of
    ([], authored) -> Right authored
    (errs, _) -> Left (concat errs)

toAuthoredActivity :: ActivityInput -> Either [Text] AuthoredActivity
toAuthoredActivity ai = case partitionEithers (map toBio (aiBiosphere ai)) of
    (errs@(_ : _), _) -> Left (map here (concat errs))
    ([], bio) ->
        Right
            AuthoredActivity
                { aaName = aiName ai
                , aaLocation = aiLocation ai
                , aaDescription = aiDescription ai
                , aaProductName = aiProductName ai
                , aaProductAmount = aiProductAmount ai
                , aaProductUnit = aiProductUnit ai
                , aaExchanges = map toTechInput (aiInputs ai) <> bio <> map toWasteOutput (aiWasteOutputs ai)
                }
  where
    here msg = aiName ai <> " {" <> aiLocation ai <> "}: " <> msg

toTechInput :: TechInputAPI -> AuthoredExchange
toTechInput ti =
    AuthoredTechInput
        { atiProvider = tiProvider ti
        , atiAmount = tiAmount ti
        , atiUnit = tiUnit ti
        , atiComment = tiComment ti
        }

toWasteOutput :: WasteOutputAPI -> AuthoredExchange
toWasteOutput wo =
    AuthoredWasteOutput
        { awProvider = woProvider wo
        , awAmount = woAmount wo
        , awUnit = woUnit wo
        , awComment = woComment wo
        }

-- | One bad line can be bad both ways at once; both complaints travel.
toBio :: BioExchangeAPI -> Either [Text] AuthoredExchange
toBio be = case (bioFlowRef be, bioDirection (beDirection be)) of
    (Right flow, Right direction) ->
        Right
            AuthoredBio
                { abFlow = flow
                , abDirection = direction
                , abAmount = beAmount be
                , abUnit = beUnit be
                , abComment = beComment be
                }
    (flow, direction) -> Left (leftList flow <> leftList direction)
  where
    leftList = either pure (const [])

{- | Translate an edit request into the changes the domain applies, in the
order the request lists them. Shared by the HTTP endpoint, the command line and
the assistant tool, which all read the same document.

What can fail here is only shape – a selector naming its provider where its
kind names a flow, a kind that is none of the three. Whether a selector reaches
anything is the domain's to judge, and its complaints accumulate the same way,
so a request is fixed in one round trip whichever layer refused it.
-}
toExchangeEdits :: ExchangeEditRequest -> Either [Text] [ExchangeEdit]
toExchangeEdits req = case partitionEithers stated of
    ([], edits) -> Right edits
    (errs, _) -> Left (concat errs)
  where
    stated =
        map (fmap RemoveExchange . toSelector) (eerRemove req)
            <> map toSetAmount (eerSetAmounts req)
            <> map (Right . AddExchange . toTechInput) (eerAddInputs req)
            <> map (fmap AddExchange . toBio) (eerAddBiosphere req)
            <> map (Right . AddExchange . toWasteOutput) (eerAddWasteOutputs req)
    toSetAmount sa = flip SetAmount (saAmount sa) <$> toSelector (saSelect sa)

{- | A selector names a provider or a flow, according to its kind. Sending the
other one, or both, is refused rather than resolved by precedence: a caller who
meant a flow and typed a provider would otherwise edit whatever the provider
happened to name.
-}
toSelector :: ExchangeSelectorAPI -> Either [Text] ExchangeSelector
toSelector es = case (T.toLower (T.strip (esKind es)), esProvider es, esFlow es) of
    ("input", Just provider, Nothing) -> Right (SelectInput provider)
    ("waste", Just provider, Nothing) -> Right (SelectWaste provider)
    ("biosphere", Nothing, Just flowId) ->
        maybe (Left ["not a flow identifier: " <> flowId]) (Right . SelectBiosphere) (UUID.fromText flowId)
    (kind, _, _)
        | kind `elem` ["input", "waste"] ->
            Left ["a selector of kind " <> kind <> " names its provider, and nothing else"]
        | kind == "biosphere" ->
            Left ["a biosphere selector names its flow, and nothing else"]
        | otherwise ->
            Left ["unknown selector kind: " <> kind <> " (expected input|waste|biosphere)"]

{- | A biosphere line names a flow by identifier, or in words. Both at once is
ambiguous and neither says nothing, so both are refused instead of picking a
winner.
-}
bioFlowRef :: BioExchangeAPI -> Either Text FlowRef
bioFlowRef be = case (beFlow be, beName be) of
    (Just _, Just _) -> Left "a biosphere exchange names its flow by identifier or in words, not both"
    (Nothing, Nothing) -> Left "a biosphere exchange needs either a flow identifier or a name, a compartment and a unit"
    (Just flowId, Nothing) ->
        maybe (Left (notAnIdentifier flowId)) (Right . FlowById) (UUID.fromText flowId)
    (Nothing, Just name) -> case (beCompartment be, beUnit be) of
        (Nothing, _) -> Left ("biosphere flow \"" <> name <> "\" needs a compartment (air, water, soil, natural resource)")
        -- The unit is half of what makes a flow named in words that flow, both
        -- when the words reach one the database declares and when they mint a
        -- new one (see 'Database.Author.authoredBioFlowUUID'), so it cannot be
        -- defaulted.
        (_, Nothing) -> Left ("biosphere flow \"" <> name <> "\" needs a unit")
        (Just medium, Just unit) -> do
            reading <- first (T.pack . unknownMedium) (parseMedium medium)
            Right $
                FlowByName
                    name
                    Compartment{compartmentName = reading, compartmentSub = beSubCompartment be}
                    unit

{- | A name written into the identifier field is the common mistake, so the
refusal says where identifiers come from and how a name is written instead.
-}
notAnIdentifier :: Text -> Text
notAnIdentifier flowId =
    "not a flow identifier: "
        <> flowId
        <> " (an identifier is the UUID a flow search returns; a flow written in words is stated as its name, compartment and unit instead)"

bioDirection :: Text -> Either Text BioDirection
bioDirection raw = case T.toLower (T.strip raw) of
    "resource" -> Right Resource
    "emission" -> Right Emission
    other -> Left ("unknown biosphere direction: " <> other <> " (expected resource|emission)")

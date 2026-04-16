{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI 3.0 schema instances and enrichment for the volca REST API.
--
-- This module collects the orphan 'ToSchema' instances for domain types
-- (avoids scattering them across all domain modules) and defines the
-- 'enrichWithResources' post-processor that stamps @operationId@,
-- @summary@, and long @description@ onto each operation with a matching
-- entry in 'API.Resources'. The actual spec derivation from 'LCAAPI'
-- lives in 'API.Routes' to break an otherwise-circular dependency
-- (API.Routes -> API.OpenApi -> API.Routes).
module API.OpenApi (enrichWithResources) where

import qualified API.Resources as R
import API.Resources (Resource)
import API.JsonOptions (strippedSchemaOptions)
import API.Types
import Control.Lens ((&), (?~), (%~), (^.))
import Data.Aeson (Value)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi
import qualified Data.OpenApi.Lens as OA
import Data.Text (Text)
import qualified Data.Text as T
import Database.Manager (DatabaseSetupInfo, MissingSupplier, DependencySuggestion)
import Network.HTTP.Types.Method (StdMethod (..))
import Types (Exchange, Flow, FlowType, Unit)

-- | Orphan schema instance forward declaration for the login request body.
-- The real type lives in "API.Routes"; this is defined there and re-imported
-- here would create a cycle. Instead, the instance is declared adjacent to
-- the type in "API.Routes" — see 'instance ToSchema LoginRequest' there.

-- Aeson Value: used for untyped JSON endpoints (logs, version, stats, hosting)
instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JsonValue") mempty

-- Domain types
instance ToSchema FlowType
instance ToSchema Unit where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema Flow where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema Exchange where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }

-- Database.Manager types
instance ToSchema MissingSupplier
instance ToSchema DependencySuggestion
instance ToSchema DatabaseSetupInfo

-- API.Types — every record type uses strippedSchemaOptions so the generated
-- OpenAPI spec matches the wire JSON keys produced by API.JsonOptions.stripLowerPrefix.
instance ToSchema ClassificationSystem where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance (ToSchema a) => ToSchema (SearchResults a) where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivitySummary where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ConsumerResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowSearchResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema InventoryExport where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema InventoryMetadata where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema InventoryFlowDetail where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema InventoryStatistics where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema TreeExport where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema TreeMetadata where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ExportNode where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema NodeType
instance ToSchema EdgeType
instance ToSchema TreeEdge where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowInfo where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowSummary where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowRole
instance ToSchema GraphExport where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema GraphNode where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema GraphEdge where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema LCIAResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema LCIABatchResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema BatchImpactsRequest where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema BatchImpactsEntry where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema BatchImpactsResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowContributionEntry where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ContributingFlowsResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivityContribution where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ContributingActivitiesResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema MappingStatus where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema UnmappedFlowAPI where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowCFMapping where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowCFEntry where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema CharacterizationResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema CharacterizationEntry where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema DatabaseListResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema DatabaseStatusAPI where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivateResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema DepLoadResult where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema LoadDatabaseResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema UploadRequest where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema UploadResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema MethodCollectionListResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema MethodCollectionStatusAPI where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema RefDataListResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema RefDataStatusAPI where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema SynonymGroupsResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema MethodSummary where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema MethodDetail where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema MethodFactorAPI where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema SupplyChainResponse where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema SupplyChainEntry where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema SupplyChainEdge where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema Aggregation where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema AggregationGroup where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema SubstitutionRequest where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema Substitution where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ExchangeDetail where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ExchangeWithUnit where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivityForAPI where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivityInfo where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivityMetadata where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivityLinks where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ActivityStats where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema FlowDetail where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ClassificationEntryInfo where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema ClassificationPresetInfo where { declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions }
instance ToSchema BinaryContent where
    declareNamedSchema _ = pure $ NamedSchema (Just "OctetStream") $
        mempty & type_ ?~ OpenApiString & format ?~ "binary"

-- | Walk the spec and stamp metadata from 'API.Resources' onto each
-- resource-backed operation. Operations without a matching 'Resource'
-- (e.g. infrastructure endpoints like @/auth@, @/logs@, @/version@) are
-- left unchanged.
--
-- Operations with parameters also get their parameter @description@ fields
-- populated from 'Resources.params'.
enrichWithResources :: OpenApi -> OpenApi
enrichWithResources spec0 = foldr stampResource spec0 R.allResources
  where
    stampResource :: Resource -> OpenApi -> OpenApi
    stampResource r spec = case R.apiPathText r of
        Nothing             -> spec  -- MCP-only resource, no HTTP route to stamp
        Just (method, path) -> spec & OA.paths %~ InsOrdHashMap.adjust (stampPathItem r method) (T.unpack path)

    stampPathItem :: Resource -> StdMethod -> PathItem -> PathItem
    stampPathItem r method = case method of
        GET    -> OA.get    %~ fmap (stampOperation r)
        POST   -> OA.post   %~ fmap (stampOperation r)
        PUT    -> OA.put    %~ fmap (stampOperation r)
        DELETE -> OA.delete %~ fmap (stampOperation r)
        _      -> id  -- HEAD/OPTIONS/TRACE/PATCH/CONNECT: not used by VoLCA today

    stampOperation :: Resource -> Operation -> Operation
    stampOperation r op = op
        & OA.operationId ?~ R.mcpName r
        & OA.summary     ?~ firstSentence (R.description r)
        & OA.description ?~ R.description r
        & OA.parameters  %~ enrichParameters (R.params r)

-- | Update parameter descriptions in-place, keyed on name. Any parameter
-- whose name doesn't appear in the resource's param list is left as-is
-- (this covers implicit Servant-generated query params like @sort@/@order@
-- that we don't describe in 'API.Resources').
enrichParameters :: [R.Param] -> [Referenced Param] -> [Referenced Param]
enrichParameters resParams = map enrich
  where
    paramMap :: [(Text, Text)]
    paramMap = [(R.paramName p, R.paramDesc p) | p <- resParams]

    enrich :: Referenced Param -> Referenced Param
    enrich (Inline p) =
        case lookup (p ^. OA.name) paramMap of
            Just desc -> Inline (p & OA.description ?~ desc)
            Nothing   -> Inline p
    enrich ref = ref  -- $ref-style parameters (rare in servant-openapi3 output) left alone

-- | First sentence of a description, for the OpenAPI 'summary' field
-- (which should fit on one line in Swagger UI).
firstSentence :: Text -> Text
firstSentence t =
    let (before, _) = T.breakOn ". " t
    in if T.null before then t else before <> "."

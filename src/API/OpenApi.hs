{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI 3.0 specification for the volca REST API.
-- ToSchema instances are orphans collected here to avoid scattering them
-- across all domain modules.
module API.OpenApi (volcaOpenApi) where

import API.Routes (LCAAPI, LoginRequest)
import API.Types
import Control.Lens ((&), (?~))
import Data.Aeson (Value)
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Database.Manager (DatabaseSetupInfo, MissingSupplier, DependencySuggestion)
import Servant.OpenApi (toOpenApi)
import Types (Exchange, Flow, FlowType, Unit)

-- Aeson Value: used for untyped JSON endpoints (logs, version, stats, hosting)
instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JsonValue") mempty

-- Domain types
instance ToSchema FlowType
instance ToSchema Unit
instance ToSchema Flow
instance ToSchema Exchange

-- Database.Manager types
instance ToSchema MissingSupplier
instance ToSchema DependencySuggestion
instance ToSchema DatabaseSetupInfo

-- API.Routes types
instance ToSchema LoginRequest

-- API.Types
instance ToSchema ClassificationSystem
instance (ToSchema a) => ToSchema (SearchResults a)
instance ToSchema ActivitySummary
instance ToSchema ConsumerResult
instance ToSchema FlowSearchResult
instance ToSchema InventoryExport
instance ToSchema InventoryMetadata
instance ToSchema InventoryFlowDetail
instance ToSchema InventoryStatistics
instance ToSchema TreeExport
instance ToSchema TreeMetadata
instance ToSchema ExportNode
instance ToSchema NodeType
instance ToSchema EdgeType
instance ToSchema TreeEdge
instance ToSchema FlowInfo
instance ToSchema FlowSummary
instance ToSchema FlowRole
instance ToSchema GraphExport
instance ToSchema GraphNode
instance ToSchema GraphEdge
instance ToSchema LCIARequest
instance ToSchema LCIAResult
instance ToSchema LCIABatchResult
instance ToSchema FlowContributionEntry
instance ToSchema ContributingFlowsResult
instance ToSchema ActivityContribution
instance ToSchema ContributingActivitiesResult
instance ToSchema MappingStatus
instance ToSchema UnmappedFlowAPI
instance ToSchema FlowCFMapping
instance ToSchema FlowCFEntry
instance ToSchema CharacterizationResult
instance ToSchema CharacterizationEntry
instance ToSchema DatabaseListResponse
instance ToSchema DatabaseStatusAPI
instance ToSchema ActivateResponse
instance ToSchema DepLoadResult
instance ToSchema LoadDatabaseResponse
instance ToSchema UploadRequest
instance ToSchema UploadResponse
instance ToSchema MethodCollectionListResponse
instance ToSchema MethodCollectionStatusAPI
instance ToSchema RefDataListResponse
instance ToSchema RefDataStatusAPI
instance ToSchema SynonymGroupsResponse
instance ToSchema MethodSummary
instance ToSchema MethodDetail
instance ToSchema MethodFactorAPI
instance ToSchema SupplyChainResponse
instance ToSchema SupplyChainEntry
instance ToSchema SupplyChainEdge
instance ToSchema SubstitutionRequest
instance ToSchema Substitution
instance ToSchema ExchangeDetail
instance ToSchema ExchangeWithUnit
instance ToSchema ActivityForAPI
instance ToSchema ActivityInfo
instance ToSchema ActivityMetadata
instance ToSchema ActivityLinks
instance ToSchema ActivityStats
instance ToSchema FlowDetail
instance ToSchema ClassificationEntryInfo
instance ToSchema ClassificationPresetInfo
instance ToSchema BinaryContent where
    declareNamedSchema _ = pure $ NamedSchema (Just "OctetStream") $
        mempty & type_ ?~ OpenApiString & format ?~ "binary"

-- | The complete OpenAPI 3.0 specification, derived from the Servant API type.
volcaOpenApi :: OpenApi
volcaOpenApi = toOpenApi (Proxy :: Proxy LCAAPI)

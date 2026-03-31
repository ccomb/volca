{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI 3.0 specification for the volca REST API.
-- ToSchema instances are orphans collected here to avoid scattering them
-- across all domain modules.
module API.OpenApi (volcaOpenApi) where

import API.Routes (LCAAPI, LoginRequest)
import API.Types
import Database.Manager (DatabaseSetupInfo, MissingSupplier, DependencySuggestion)
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.UUID (UUID)
import Servant.OpenApi (toOpenApi)
import Types (Exchange, Flow, FlowType, Unit)

-- | UUID represented as a string with uuid format
instance ToSchema UUID where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "UUID") $
            mempty
                & type_ ?~ OpenApiString
                & format ?~ "uuid"

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
instance ToSchema FlowHotspotResult
instance ToSchema ProcessContribution
instance ToSchema ProcessHotspotResult
instance ToSchema MappingStatus
instance ToSchema UnmappedFlowAPI
instance ToSchema FlowCFMapping
instance ToSchema FlowCFEntry
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

-- | The complete OpenAPI 3.0 specification, derived from the Servant API type.
volcaOpenApi :: OpenApi
volcaOpenApi = toOpenApi (Proxy :: Proxy LCAAPI)

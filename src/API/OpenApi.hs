{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | OpenAPI 3.0 schema instances and enrichment for the volca REST API.

This module collects the orphan 'ToSchema' instances for domain types
(avoids scattering them across all domain modules) and defines the
'enrichWithResources' post-processor that stamps @operationId@,
@summary@, and long @description@ onto each operation with a matching
entry in 'API.Resources'. The actual spec derivation from 'LCAAPI'
lives in 'API.Routes' to break an otherwise-circular dependency
(API.Routes -> API.OpenApi -> API.Routes).
-}
module API.OpenApi (enrichWithResources, stampInfo) where

import API.JsonOptions (strippedSchemaOptions)
import API.Resources (Resource)
import qualified API.Resources as R
import API.Types
import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Data.Aeson (Value)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi
import qualified Data.OpenApi.Lens as OA
import Data.Text (Text)
import qualified Data.Text as T
import Database.Manager (DatabaseSetupInfo)
import Network.HTTP.Types.Method (StdMethod (..))
import qualified Version

{- | Orphan schema instance forward declaration for the login request body.
The real type lives in "API.Routes"; this is defined there and re-imported
here would create a cycle. Instead, the instance is declared adjacent to
the type in "API.Routes" – see 'instance ToSchema LoginRequest' there.
-}

-- Aeson Value: used for untyped JSON endpoints (logs, version, stats, hosting)
instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JsonValue") mempty

-- Domain types: TechRole, BioDirection, Unit now derive ToSchema next to
-- their data declarations in src/Types.hs (via anyclass / DerivingVia).

-- Database.Manager ToSchema instances (MissingSupplier, DependencyChoice via
-- Stripped; DependencyStatus as a lowercase string-enum) now live next to
-- their data declarations in src/Database/Manager.hs.

-- ToSchema for LocationKind / LocationFallback / LocationUnresolved derived
-- alongside their data declarations in src/Types.hs (LocationKind as a string
-- enum matching the lowercase wire codes; the records via Stripped).
instance ToSchema DatabaseSetupInfo where declareNamedSchema = genericDeclareNamedSchema strippedSchemaOptions

-- API.Types – every record type uses strippedSchemaOptions so the generated
-- OpenAPI spec matches the wire JSON keys produced by API.JsonOptions.stripLowerPrefix.
-- ToSchema (SearchResults a) standalone-derived in API.Types via Stripped.
-- ToSchema for ApiFlow, NodeType, EdgeType, FlowRole now derived next to
-- their data declarations in src/API/Types.hs (NodeType/EdgeType/FlowRole
-- via anyclass; ApiFlow has a custom instance there to keep the discriminated
-- `kind` union representation).
-- CutoffWasteFlow (added on main) derives ToSchema via Stripped alongside
-- its data declaration in src/API/Types.hs.

-- PerturbedEntry's custom schema moved to API.Types alongside its data decl.

-- ToSchema for NativeActivityType lives alongside its ToJSON/FromJSON
-- siblings in src/API/Types.hs – moved out of this module so DerivingVia
-- clauses for record types that contain a NativeActivityType field
-- (ActivitySummary, ActivityForAPI) can resolve the instance at API.Types
-- compile time without forming a circular dependency on API.OpenApi.

instance ToSchema BinaryContent where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "OctetStream") $
                mempty & type_ ?~ OpenApiString & format ?~ "binary"

{- | Stamp the spec's @info@ block so each generated @openapi.json@ self-identifies:
@info.version@ = the engine version, @info.title@ = the engine name. servant-openapi3
leaves both blank, which makes a published spec un-anchorable; stamping them lets a
release's surface be diffed against the previous one.
-}
stampInfo :: OpenApi -> OpenApi
stampInfo spec =
    spec
        & OA.info . OA.title .~ "VoLCA"
        & OA.info . OA.version .~ T.pack Version.version

{- | Walk the spec and stamp metadata from 'API.Resources' onto each
resource-backed operation. Operations without a matching 'Resource'
(e.g. infrastructure endpoints like @/auth@, @/logs@, @/version@) are
left unchanged.

Operations with parameters also get their parameter @description@ fields
populated from 'Resources.params'.
-}
enrichWithResources :: OpenApi -> OpenApi
enrichWithResources spec0 = foldr stampResource spec0 R.allResources
  where
    stampResource :: Resource -> OpenApi -> OpenApi
    stampResource r spec = case R.apiPathText r of
        Nothing -> spec -- MCP-only resource, no HTTP route to stamp
        Just (method, path) -> spec & OA.paths %~ InsOrdHashMap.adjust (stampPathItem r method) (T.unpack path)

    stampPathItem :: Resource -> StdMethod -> PathItem -> PathItem
    stampPathItem r method = case method of
        GET -> OA.get %~ fmap (stampOperation r)
        POST -> OA.post %~ fmap (stampOperation r)
        PUT -> OA.put %~ fmap (stampOperation r)
        DELETE -> OA.delete %~ fmap (stampOperation r)
        _ -> id -- HEAD/OPTIONS/TRACE/PATCH/CONNECT: not used by VoLCA today
    stampOperation :: Resource -> Operation -> Operation
    stampOperation r op =
        op
            & OA.operationId ?~ R.mcpName r
            & OA.summary ?~ firstSentence (R.description r)
            & OA.description ?~ R.description r
            & OA.parameters %~ enrichParameters (R.params r)

{- | Update parameter descriptions in-place, keyed on name. Any parameter
whose name doesn't appear in the resource's param list is left as-is
(this covers implicit Servant-generated query params like @sort@/@order@
that we don't describe in 'API.Resources').
-}
enrichParameters :: [R.Param] -> [Referenced Param] -> [Referenced Param]
enrichParameters resParams = map enrich
  where
    paramMap :: [(Text, Text)]
    paramMap = [(R.paramName p, R.paramDesc p) | p <- resParams]

    enrich :: Referenced Param -> Referenced Param
    enrich (Inline p) =
        case lookup (p ^. OA.name) paramMap of
            Just desc -> Inline (p & OA.description ?~ desc)
            Nothing -> Inline p
    enrich ref = ref

-- \$ref-style parameters (rare in servant-openapi3 output) left alone

{- | First sentence of a description, for the OpenAPI 'summary' field
(which should fit on one line in Swagger UI).
-}
firstSentence :: Text -> Text
firstSentence t =
    let (before, _) = T.breakOn ". " t
     in if T.null before then t else before <> "."

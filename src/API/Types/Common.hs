{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types.Common where

import API.JsonOptions (Stripped (..))
import Control.DeepSeq (NFData)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Referenced (..), ToSchema (..), binarySchema, declareSchemaRef, enum_, format, nullable, properties, required, type_)
import qualified Data.OpenApi.Lens as OA
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Servant.API.ContentTypes (MimeRender (..), MimeUnrender (..), OctetStream)
import Types (
    AllocationProperty,
    BiosphereFlow (..),
    BlockerReason (..),
    ExchangeKind (..),
    FlowKind (..),
    NativeActivityType (..),
    NativeProcessId (..),
    TechnosphereFlow (..),
    UUID,
    WasteFlow (..),
    exchangeKindChoices,
    exchangeKindName,
    parseExchangeKind,
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
    , prsAllocationBy :: Maybe AllocationProperty -- The property a child's system model divided this block by; Nothing when the share is the one its source declared
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

{- | What a database whose licence keeps what weighs in its scores adds to a
score it is read by, in one line: its part and why it is not detailed. The
detailed rows and these lines add up to the score.
-}
data WithheldShare = WithheldShare
    { wsDatabase :: Text
    , wsContribution :: Double -- In the score's unit
    , wsSharePct :: Double -- Percentage of the total score (0-100)
    , wsReason :: Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped WithheldShare)

{- | The processes a chain reaches in a database whose licence keeps the
amounts of its exchanges, in one line: how many, and why they are not listed.
A count says how far the chain reaches, not what it takes. In a supply chain
it counts the processes the request's filters kept, the ones it would have
listed; in an aggregation by consumption, every process the chain reaches.
-}
data WithheldProcesses = WithheldProcesses
    { wprDatabase :: Text
    , wprProcesses :: Int
    , wprReason :: Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped WithheldProcesses)

instance NFData WithheldShare
instance NFData WithheldProcesses

instance NFData CutoffWasteFlow

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

{- | A product input the calculation met that no loaded database supplies: what
it asks for is left out of the result, which counts it as zero.

These are the inputs of every process the chain runs, the root's and its
suppliers' alike, where 'CutoffWasteFlow' names the root's own waste outputs
left without a treatment; a waste output of a process upstream is in neither.

What counts is what the supplier-gap report counts, with its two imprecisions:
a link to another database whose supplier that database does not hold is still
counted as covering the input, and when a process has more inputs of one
product than links for it, the inputs taken as covered are the first ones read.
-}
data CutoffInput = CutoffInput
    { ciDatabase :: Text
    , ciProduct :: Text
    , ciSupplier :: Maybe Text
    {- ^ The supplier activity the input names, when it names one by name: the
    same product named from two absent suppliers is two entries, as in the gap
    report.
    -}
    , ciLocation :: Text
    -- ^ The location the input asks for, empty when it names none
    , ciUnit :: Text
    , ciAmount :: Double
    {- ^ What the chain asks of this product per unit of the activity computed,
    in 'ciUnit', summed over every process that asks for it.
    -}
    , ciConsumers :: Int
    -- ^ How many processes of the chain ask for it
    , ciReasons :: NE.NonEmpty BlockerReason
    -- ^ Why no supplier was found
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped CutoffInput)

instance NFData CutoffInput

{- | The cut-off inputs met inside a database whose licence keeps its detail to
itself, the one asked included: how many, not which.
-}
data WithheldCutoffs = WithheldCutoffs
    { wcDatabase :: Text
    , wcCount :: Int
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped WithheldCutoffs)

instance NFData WithheldCutoffs

{- | Where a line comes from or goes to: the activity that supplies an input,
or treats a waste, by its name and location. Named rather than identified, so
two releases that mint new identifiers still name the same supplier alike.
-}
data Supplier = Supplier
    { supActivityName :: !Text
    , supLocation :: !Text
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Supplier)

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

{- | A kind reaches the wire as the word a request would name it by, so what a
result reports and what a filter accepts are the same three strings.
-}
instance ToJSON ExchangeKind where
    toJSON = toJSON . exchangeKindName

instance FromJSON ExchangeKind where
    parseJSON = withText "ExchangeKind" (\t -> maybe (fail (T.unpack ("kind: expected " <> exchangeKindChoices <> ", got " <> t))) pure (parseExchangeKind t))

instance ToSchema ExchangeKind where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ExchangeKind") $
                mempty
                    & type_ ?~ OpenApiString
                    & enum_ ?~ map (toJSON . exchangeKindName) [minBound .. maxBound]

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

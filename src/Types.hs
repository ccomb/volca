{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types (
    module Types,
    UUID,
) where

import API.JsonOptions (Stripped (..))
import Control.DeepSeq (NFData)
import Control.Monad ((<=<), (>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Int (Int32)
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Store (Size (..), Store (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)

import Control.Lens ((&), (?~))
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (find, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToSchema (..), enum_, type_)
import Search.BM25.Types (BM25Index)
import SubstanceRegistry (CASNumber (..), NormName (..), nonEmptyCAS)
import SynonymDB (normalizeName)
import SynonymDB.Types (SynonymDB)
import UnitConversion (UnitConfig)

-- | Orphan Store instance for UUID (16 bytes, host-native word order)
instance Store UUID where
    size = ConstSize 16
    poke uuid =
        let (a, b, c, d) = UUID.toWords uuid
         in poke a >> poke b >> poke c >> poke d
    peek = UUID.fromWords <$> peek <*> peek <*> peek <*> peek

-- Note: UUID is now Data.UUID.UUID (16 bytes) instead of Text (~80+ bytes)
-- This saves ~2-3GB of RAM by reducing memory footprint from ~100,000+ UUID instances
-- NFData instance is provided by the uuid package; Store instance defined above

{- | Process identifier - compact Int32 index for efficient matrix operations
Maps to (activityUUID, productUUID) via Database.dbProcessIdTable
Based on EcoSpold filename pattern {activity_uuid}_{product_uuid}.spold
-}
type ProcessId = Int32

{- | The (activity, product) pair a process is. 'ProcessId' is the matrix row
index for that pair inside one database; a 'ProcessRef' is the pair itself,
which is what travels — over the wire, in a @.spold@ file name, in an ILCD
process identifier.

Named fields rather than a bare @(UUID, UUID)@ because the two halves are
indistinguishable to the compiler, and a swapped pair produces a reference
that is well-formed and wrong.
-}
data ProcessRef = ProcessRef
    { prActivity :: !UUID
    , prProduct :: !UUID
    }
    deriving (Eq, Ord, Show)

{- | The medium a biosphere flow exchanges with.

Every format spells these differently and none of them spells more than these:
EcoSpold 2 writes @natural resource@ where SimaPro writes @Raw@ and EcoSpold 1
writes @resource@, and the whole vocabulary observed across EcoSpold 1 and 2,
SimaPro CSV and ILCD is the constructors below. Holding it as text meant
two databases loaded side by side described one physical flow with two strings,
and every reader and writer that needed them to agree carried its own list of
spellings. The medium is the closed part; the subcompartment underneath it
("high. pop.", "river water") is genuinely open and stays 'Text'.

'Economic', 'InventoryIndicator' and 'Waste' are not media in the physical
sense. They are what the sources file those flows under, and a flow has to keep
saying which it is for the readers and writers that bucket by medium.
-}
data Medium
    = Air
    | Water
    | Soil
    | NaturalResource
    | InventoryIndicator
    | Economic
    | {- | Waste that leaves the system with no treatment modelled for it, so
      no activity produces or consumes it. SimaPro files these in their own
      section and a method characterizes them under this name, which is why
      they are elementary flows rather than a demand on the technosphere.
      Waste that IS sent to treatment keeps its producer and stays on the
      technosphere side.
      -}
      Waste
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, NFData, Store)
    deriving anyclass (ToSchema)

{- | The one spelling the engine says out loud: on the wire, in an identity, and
wherever a medium is compared to itself. A file's own spelling belongs to the
writer that emits that format, not here.
-}
mediumText :: Medium -> Text
mediumText = \case
    Air -> "air"
    Water -> "water"
    Soil -> "soil"
    NaturalResource -> "natural resource"
    InventoryIndicator -> "inventory indicator"
    Economic -> "economic"
    Waste -> "waste"

{- | Read a medium as any format writes it. Case and surrounding space are
ignored; @raw@, @resource@ and @resources@ are the three ways the formats name
'NaturalResource', and a file that states the direction as well as the medium
(@emissions to air@) names the same three emission media as the bare words do.

'Left' names the string it could not place, so an unknown medium is reported
rather than turned into a flow that quietly belongs nowhere.
-}
parseMedium :: Text -> Either Text Medium
parseMedium raw = case T.toLower (T.strip raw) of
    "air" -> Right Air
    "emissions to air" -> Right Air
    "water" -> Right Water
    "emissions to water" -> Right Water
    "soil" -> Right Soil
    "emissions to soil" -> Right Soil
    "natural resource" -> Right NaturalResource
    "raw" -> Right NaturalResource
    "resource" -> Right NaturalResource
    "resources" -> Right NaturalResource
    "inventory indicator" -> Right InventoryIndicator
    "economic" -> Right Economic
    "waste" -> Right Waste
    other -> Left other

-- | On the wire a medium is its canonical spelling, and reads back from any.
instance ToJSON Medium where
    toJSON = toJSON . mediumText

instance FromJSON Medium where
    parseJSON = parseJSON >=> either (fail . unknownMedium) pure . parseMedium

-- | What to say about a medium string that belongs to no known medium.
unknownMedium :: Text -> String
unknownMedium got =
    "unknown compartment medium "
        <> show got
        <> " (expected one of: "
        <> T.unpack (T.intercalate ", " (map mediumText [minBound .. maxBound]))
        <> ")"

{- | Biosphere compartment — the natural medium a biosphere flow exchanges
with. Present only on `BiosphereFlow`; technosphere flows have no
compartment (their taxonomy, when meaningful, lives on the producing
activity's `activityClassification`).
-}
data Compartment = Compartment
    { compartmentName :: !Medium
    , compartmentSub :: !(Maybe Text) -- "high. pop.", "river water", …
    }
    deriving (Eq, Show, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Compartment)

{- | The biosphere flow's medium (air | water | soil | …), or @""@ when the
source dataset omitted the compartment. Use 'bfCompartment' directly when
you need to distinguish "absent" from "empty string".
-}
bfCompartmentName :: BiosphereFlow -> Text
bfCompartmentName = maybe "" (mediumText . compartmentName) . bfCompartment

{- | The biosphere flow's sub-compartment (e.g. "high. pop."), or @Nothing@
when neither the source nor the medium recorded one.
-}
bfCompartmentSub :: BiosphereFlow -> Maybe Text
bfCompartmentSub = compartmentSub <=< bfCompartment

{- | Direction of a biosphere exchange. Mirrors the @TechRole@ sum so the
biosphere side also gets named variants instead of a load-bearing 'Bool'.

* @Resource@ — extraction from the environment (e.g. crude oil, ore, water
  withdrawal). Acts as an input to the activity.
* @Emission@ — release into the environment (e.g. CO₂ to air, P to water).
  Acts as an output from the activity.
-}
data BioDirection = Resource | Emission
    deriving (Eq, Show, Generic, NFData, Store)
    deriving anyclass (ToSchema)

{- | Role of a technosphere exchange within its host activity. `ReferenceInput`
is the treatment-process case where the activity is defined by the waste flow
it consumes (see `activityNormFactor`).

A 'Coproduct' is a product output the source left unallocated. No activity
still carrying one reaches the matrix: "Database.Allocation" either splits the
activity into one process per product, or refuses it and says why. An
'AvoidedProduct' is a substitution, the product this activity displaces; it
is a credit on that product's producer and behaves as a negative input.
-}
data TechRole
    = ReferenceProduct -- main output of a production process
    | Coproduct -- a product output the source did not allocate
    | AvoidedProduct -- substitution: a product this activity displaces
    | ReferenceInput -- main input of a treatment process
    | Input -- ordinary technosphere input
    deriving (Eq, Show, Generic, NFData, Store)
    deriving anyclass (ToSchema)

{- | What a source declares about one product output of a multi-output
activity: its share of the inventory in percent, as written, and the raw
expression when that share was a formula rather than a number (the SimaPro
allocation column). 'Database.Allocation.allocate' splits on it, the SimaPro
writer writes it back, and it stays on the reference exchange of a split
process as the record of what the source said.
-}
data DeclaredShare = DeclaredShare
    { dsPercent :: !Double
    , dsFormula :: !(Maybe Text)
    }
    deriving (Eq, Show, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeclaredShare)

{- | An amount together with the unit it is stated in.

Kept as written rather than converted: the conversion needs the unit table,
which the parsers do not carry, and a number whose unit has been forgotten
cannot be checked against the file it came from.
-}
data StatedAmount = StatedAmount
    { saUnit :: !Text
    , saAmount :: !Double
    }
    deriving (Eq, Show, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped StatedAmount)

{- | The physical properties of one exchange, as the source states them.

Each is stated /per unit of the exchange amount/, which is how EcoSpold 2
writes them: 614.4 kg of dry matter per m3 of board is recorded as 614.4 kg,
whether the line is 1 m3 or 2. The mass of the whole line is the product of
the two, taken where it is needed.

Per unit rather than per line on purpose. A property that referred to the
amount would have to be recomputed by every operation that touches an amount,
and the one that forgot would leave a line saying 2 m3 and 614.4 kg. Referring
to the unit instead, it cannot contradict the amount beside it.

'Nothing' is "the source states none", never a zero standing in for it: a
product of no mass and a product whose mass is unknown are different answers
to whether the mass can serve as a key.
-}
data ExchangeProperties = ExchangeProperties
    { epDryMass :: !(Maybe StatedAmount)
    , epWetMass :: !(Maybe StatedAmount)
    }
    deriving (Eq, Show, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeProperties)

-- | An exchange whose source states no property.
noProperties :: ExchangeProperties
noProperties = ExchangeProperties{epDryMass = Nothing, epWetMass = Nothing}

-- | Unit representation (kg, MJ, m³, etc.)
data Unit = Unit
    { unitId :: !UUID -- Unique unit identifier
    , unitName :: !Text -- Unit name (e.g. "kg", "kilogram")
    , unitSymbol :: !Text -- Symbol (e.g. "kg", "MJ")
    , unitComment :: !Text -- Description/comment
    }
    deriving (Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Unit)

{- | A technosphere flow — an intermediate product that activities produce and
consume. Carries no compartment and no taxonomy: the product classification
(when meaningful) lives on the producing activity's `activityClassification`.
-}
data TechnosphereFlow = TechnosphereFlow
    { tfId :: !UUID
    , tfName :: !Text
    , tfUnitId :: !UUID
    , tfSynonyms :: !(M.Map Text (S.Set Text))
    , tfCAS :: !(Maybe Text)
    , tfSubstanceId :: !(Maybe Int)
    }
    deriving (Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped TechnosphereFlow)

{- | A biosphere flow — an environmental exchange (resource extraction or
emission). Always carries a `Compartment` identifying the medium.
-}
data BiosphereFlow = BiosphereFlow
    { bfId :: !UUID
    , bfName :: !Text
    , bfUnitId :: !UUID
    , bfSynonyms :: !(M.Map Text (S.Set Text))
    , bfCAS :: !(Maybe Text)
    , bfSubstanceId :: !(Maybe Int)
    , bfCompartment :: !(Maybe Compartment)
    {- ^ The source dataset's compartment (medium + optional sub) for this
    flow, or @Nothing@ when the source omitted it. Distinguishing
    "no compartment recorded" from "the empty string" is required to
    avoid silently broadening LCIA matches in 'Method.Mapping'.
    -}
    }
    deriving (Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BiosphereFlow)

{- | A waste flow — a residual output that a process generates and which a
treatment activity may consume as its reference input. Sister type to
'TechnosphereFlow' and 'BiosphereFlow'. Distinct from product flows so the
UI and import logic can surface them separately, but routed to the same
technosphere matrix because the underlying calculation is identical to a
product link.

When no treatment activity is present in the loaded data, a waste output
flow stays orphan and contributes zero impact — same cut-off semantics
as an orphan product input.
-}
data WasteFlow = WasteFlow
    { wfId :: !UUID
    , wfName :: !Text
    , wfUnitId :: !UUID
    , wfSynonyms :: !(M.Map Text (S.Set Text))
    , wfCAS :: !(Maybe Text)
    , wfSubstanceId :: !(Maybe Int)
    }
    deriving (Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped WasteFlow)

{- | Pedigree matrix (Weidema & Wesnæs 1996) — five LCA data-quality scores
each in 1..5 (1 = best, 5 = worst). SimaPro CSV encodes it as a prefix in the
trailing comment column; ecoinvent/EcoSpold2 stores it as structured XML.
-}
data Pedigree = Pedigree
    { pedReliability :: !Int -- 1..5
    , pedCompleteness :: !Int -- 1..5
    , pedTemporal :: !Int -- 1..5
    , pedGeographical :: !Int -- 1..5
    , pedTechnological :: !Int -- 1..5
    }
    deriving (Eq, Show, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Pedigree)

{- | Smart constructor: rejects out-of-range values (anything not in 1..5)
by returning Nothing. Callers should treat Nothing as "no pedigree
recorded" — never silently clamp.
-}
mkPedigree :: Int -> Int -> Int -> Int -> Int -> Maybe Pedigree
mkPedigree r c t g f
    | all inRange [r, c, t, g, f] = Just (Pedigree r c t g f)
    | otherwise = Nothing
  where
    inRange n = n >= 1 && n <= 5

{- | How a source designates the supplier of one exchange.

Every format says it differently, and until this type they all had to say it
through 'techActivityLinkId', which linking then overwrites with what it
resolved to: the question and the answer shared one field, so the question was
lost the moment it was answered. Here the claim is what the source said, and it
is never rewritten.

There is no constructor for "says nothing": a row that designates by its
product line still designates, through the flow and location it already
carries, which is what 'ClaimByProduct' means.
-}
data SupplierClaim
    = {- | The product row itself: its flow, and its location where it has one.
      SimaPro, ILCD, and a Brightway workbook whose row names no activity.
      -}
      ClaimByProduct
    | -- | The activity's own identifier, as EcoSpold 2 writes it.
      ClaimById !UUID
    | -- | The supplier activity by name, from a Brightway workbook's @name@ column.
      ClaimByName !Text
    | -- | The number the supplier dataset is published under, as EcoSpold 1 writes it.
      ClaimByDatasetNumber !Int
    deriving (Eq, Show, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SupplierClaim)

-- | Exchange in an activity - Mirrors EcoSpold intermediateExchange/elementaryExchange structure
data Exchange
    = TechnosphereExchange
        { techFlowId :: !UUID -- Flow being exchanged
        , techAmount :: !Double -- Quantity exchanged
        , techUnitId :: !UUID -- Unit of measurement
        , techRole :: !TechRole -- Role within the activity
        , techActivityLinkId :: !(Maybe UUID) -- Supplier this exchange resolved to, Nothing until it does
        , techSupplierClaim :: !SupplierClaim
        {- ^ How the source designates its supplier. Written once, by a parser,
        and never rewritten by linking, unlike 'techActivityLinkId'.
        -}
        , techLocation :: !Text -- Supplier location (EcoSpold1) or "" (EcoSpold2)
        , techComment :: !(Maybe Text) -- Free-text per-exchange comment from source
        , techPedigree :: !(Maybe Pedigree) -- LCA data-quality scores when available
        , techShare :: !(Maybe DeclaredShare) -- The share a product output was declared with; Nothing on inputs and where the source states none
        , techClassification :: !(M.Map Text Text) -- What the source says of this product row (SimaPro "Category"); carried onto the process split for it
        , techProperties :: !ExchangeProperties -- Physical properties the source states of this line, the material an allocation key other than the declared one is computed from
        }
    | BiosphereExchange
        { bioFlowId :: !UUID -- Flow being exchanged
        , bioAmount :: !Double -- Quantity exchanged
        , bioUnitId :: !UUID -- Unit of measurement
        , bioDirection :: !BioDirection -- 'Resource' for extraction, 'Emission' for release
        , bioLocation :: !Text -- Exchange location (EcoSpold1) or "" (EcoSpold2)
        , bioComment :: !(Maybe Text) -- Free-text per-exchange comment from source
        , bioPedigree :: !(Maybe Pedigree) -- LCA data-quality scores when available
        }
    | WasteExchange
        { waFlowId :: !UUID -- Flow being exchanged (points at a WasteFlow)
        , waAmount :: !Double -- Quantity exchanged
        , waUnitId :: !UUID -- Unit of measurement
        , waIsInput :: !Bool
        {- ^ True when consumed by a treatment activity; False when generated
        by it. Waste with no treatment modelled at all is not on this axis:
        it is an elementary flow of medium 'Waste'.
        -}
        , waActivityLinkId :: !(Maybe UUID) -- Treatment this exchange resolved to (Nothing if orphan)
        , waSupplierClaim :: !SupplierClaim
        -- ^ How the source designates the treatment, as on a technosphere line.
        , waLocation :: !Text -- Supplier location (EcoSpold1) or "" (EcoSpold2)
        , waComment :: !(Maybe Text) -- Free-text per-exchange comment from source
        , waPedigree :: !(Maybe Pedigree) -- LCA data-quality scores when available
        }
    deriving (Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Exchange)

-- | Helper functions for Exchange variants
exchangeFlowId :: Exchange -> UUID
exchangeFlowId TechnosphereExchange{techFlowId = fid} = fid
exchangeFlowId BiosphereExchange{bioFlowId = fid} = fid
exchangeFlowId WasteExchange{waFlowId = fid} = fid

exchangeAmount :: Exchange -> Double
exchangeAmount TechnosphereExchange{techAmount = amt} = amt
exchangeAmount BiosphereExchange{bioAmount = amt} = amt
exchangeAmount WasteExchange{waAmount = amt} = amt

exchangeUnitId :: Exchange -> UUID
exchangeUnitId TechnosphereExchange{techUnitId = uid} = uid
exchangeUnitId BiosphereExchange{bioUnitId = uid} = uid
exchangeUnitId WasteExchange{waUnitId = uid} = uid

exchangeIsInput :: Exchange -> Bool
exchangeIsInput TechnosphereExchange{techRole = role} = case role of
    Input -> True
    ReferenceInput -> True
    ReferenceProduct -> False
    Coproduct -> False
    AvoidedProduct -> False
exchangeIsInput BiosphereExchange{bioDirection = dir} = case dir of
    Resource -> True
    Emission -> False
exchangeIsInput WasteExchange{waIsInput = b} = b

exchangeIsReference :: Exchange -> Bool
exchangeIsReference TechnosphereExchange{techRole = role} = case role of
    ReferenceProduct -> True
    ReferenceInput -> True
    Input -> False
    Coproduct -> False
    AvoidedProduct -> False
exchangeIsReference BiosphereExchange{} = False
exchangeIsReference WasteExchange{} = False

-- | A product the activity makes: its reference, or a coproduct not yet allocated.
exchangeIsProductOutput :: Exchange -> Bool
exchangeIsProductOutput TechnosphereExchange{techRole = role} = case role of
    ReferenceProduct -> True
    Coproduct -> True
    AvoidedProduct -> False
    ReferenceInput -> False
    Input -> False
exchangeIsProductOutput BiosphereExchange{} = False
exchangeIsProductOutput WasteExchange{} = False

-- | What the source states of a technosphere row beyond its amount (SimaPro "Category"); empty on the other axes.
exchangeClassification :: Exchange -> M.Map Text Text
exchangeClassification TechnosphereExchange{techClassification = cls} = cls
exchangeClassification BiosphereExchange{} = M.empty
exchangeClassification WasteExchange{} = M.empty

-- | The share a technosphere exchange was declared with; nothing on the other axes.
exchangeDeclaredShare :: Exchange -> Maybe DeclaredShare
exchangeDeclaredShare TechnosphereExchange{techShare = share} = share
exchangeDeclaredShare BiosphereExchange{} = Nothing
exchangeDeclaredShare WasteExchange{} = Nothing

-- | The physical properties the source states of a row; none on the other axes.
exchangeProperties :: Exchange -> ExchangeProperties
exchangeProperties TechnosphereExchange{techProperties = props} = props
exchangeProperties BiosphereExchange{} = noProperties
exchangeProperties WasteExchange{} = noProperties

{- | The share each product output was declared with, in source order;
'Nothing' where the source states none. One entry on a process split from
a block, as many as the block had products on one the gate refused.
-}
activityDeclaredShares :: Activity -> [Maybe DeclaredShare]
activityDeclaredShares act = [exchangeDeclaredShare ex | ex <- exchanges act, exchangeIsProductOutput ex]

-- | The share the reference product was declared with, when the source stated one.
activityReferenceShare :: Activity -> Maybe DeclaredShare
activityReferenceShare act =
    listToMaybe [share | ex <- exchanges act, exchangeIsReference ex, Just share <- [exchangeDeclaredShare ex]]

{- | How the source designates the supplier of an exchange, on the two axes
that have one. A biosphere line designates none: it has no supplier.
-}
exchangeSupplierClaim :: Exchange -> SupplierClaim
exchangeSupplierClaim TechnosphereExchange{techSupplierClaim = claim} = claim
exchangeSupplierClaim BiosphereExchange{} = ClaimByProduct
exchangeSupplierClaim WasteExchange{waSupplierClaim = claim} = claim

-- | The supplier an exchange resolved to, on the two axes that can have one.
exchangeActivityLinkId :: Exchange -> Maybe UUID
exchangeActivityLinkId TechnosphereExchange{techActivityLinkId = linkId} = linkId
exchangeActivityLinkId BiosphereExchange{} = Nothing
exchangeActivityLinkId WasteExchange{waActivityLinkId = linkId} = linkId

-- | Get exchange location (for EcoSpold1 supplier lookup)
exchangeLocation :: Exchange -> Text
exchangeLocation TechnosphereExchange{techLocation = loc} = loc
exchangeLocation BiosphereExchange{bioLocation = loc} = loc
exchangeLocation WasteExchange{waLocation = loc} = loc

-- | Get free-text comment attached to the exchange by the source dataset
exchangeComment :: Exchange -> Maybe Text
exchangeComment TechnosphereExchange{techComment = c} = c
exchangeComment BiosphereExchange{bioComment = c} = c
exchangeComment WasteExchange{waComment = c} = c

-- | Get pedigree matrix attached to the exchange (when the source provides it)
exchangePedigree :: Exchange -> Maybe Pedigree
exchangePedigree TechnosphereExchange{techPedigree = p} = p
exchangePedigree BiosphereExchange{bioPedigree = p} = p
exchangePedigree WasteExchange{waPedigree = p} = p

-- | Check if exchange is technosphere
isTechnosphereExchange :: Exchange -> Bool
isTechnosphereExchange TechnosphereExchange{} = True
isTechnosphereExchange BiosphereExchange{} = False
isTechnosphereExchange WasteExchange{} = False

-- | Check if exchange is biosphere
isBiosphereExchange :: Exchange -> Bool
isBiosphereExchange TechnosphereExchange{} = False
isBiosphereExchange BiosphereExchange{} = True
isBiosphereExchange WasteExchange{} = False

-- | Check if exchange is waste
isWasteExchange :: Exchange -> Bool
isWasteExchange TechnosphereExchange{} = False
isWasteExchange BiosphereExchange{} = False
isWasteExchange WasteExchange{} = True

{- | A waste exchange that names the treatment it goes to. It is the question a
writer choosing where to put such a row has to answer: a named treatment makes
it technosphere, and with none nothing treats it, which is what a final waste
flow is.
-}
linkedWaste :: Exchange -> Bool
linkedWaste ex = isWasteExchange ex && isJust (exchangeActivityLinkId ex)

{- | Activity's reference-product amount used to normalize its matrix column.
Net output = sum of reference outputs minus self-loop consumption; falls back
to reference inputs (treatment processes) or 1.0 when nothing else applies.
Self-loops require both activityLinkId == activityUUID AND flowId == productUUID,
so cross-product inputs in multi-output processes are not mistakenly subtracted.
-}
activityNormFactor :: Activity -> (UUID, UUID) -> Double
activityNormFactor act (actUUID, prodUUID)
    | abs netOutput > epsilon = netOutput
    | refInputs > epsilon = refInputs
    | otherwise = 1.0
  where
    -- Below this, an amount is read as absent rather than as a quantity.
    epsilon :: Double
    epsilon = 1e-15

    isSelfLoop :: Exchange -> Bool
    isSelfLoop ex =
        exchangeActivityLinkId ex == Just actUUID && exchangeFlowId ex == prodUUID

    refOutputs :: Double
    refOutputs =
        sum
            [ exchangeAmount ex
            | ex <- exchanges act
            , exchangeIsReference ex
            , not (exchangeIsInput ex)
            ]

    refInputs :: Double
    refInputs =
        sum
            [ abs (exchangeAmount ex)
            | ex <- exchanges act
            , exchangeIsReference ex
            , exchangeIsInput ex
            ]

    internalConsumption :: Double
    internalConsumption =
        sum
            [ exchangeAmount ex
            | ex <- exchanges act
            , isTechnosphereExchange ex
            , exchangeIsInput ex
            , not (exchangeIsReference ex)
            , isSelfLoop ex
            ]

    -- Magnitude, not sign, decides whether a reference output is real: a
    -- waste-treatment / market-for-waste activity has a NEGATIVE reference
    -- output (e.g. -1 kg of the waste it treats). Its signed net output must
    -- flow through as the normalization factor; forcing a positive fallback
    -- here flips the sign of the activity's entire inventory contribution.
    netOutput :: Double
    netOutput
        | abs refOutputs > epsilon = refOutputs - internalConsumption
        | otherwise = 0.0

-- | Get unit information for an exchange
getUnitForExchange :: UnitDB -> Exchange -> Maybe Unit
getUnitForExchange unitDB exchange = M.lookup (exchangeUnitId exchange) unitDB

-- | Get unit name for an exchange (fallback to "unknown" if not found)
getUnitNameForExchange :: UnitDB -> Exchange -> Text
getUnitNameForExchange unitDB exchange =
    maybe "unknown" unitName (getUnitForExchange unitDB exchange)

-- | Get unit information for a technosphere flow
getUnitForTechFlow :: UnitDB -> TechnosphereFlow -> Maybe Unit
getUnitForTechFlow unitDB f = M.lookup (tfUnitId f) unitDB

-- | Get unit name for a technosphere flow (fallback to "unknown" if not found)
getUnitNameForTechFlow :: UnitDB -> TechnosphereFlow -> Text
getUnitNameForTechFlow unitDB f =
    maybe "unknown" unitName (getUnitForTechFlow unitDB f)

-- | Get unit information for a biosphere flow
getUnitForBioFlow :: UnitDB -> BiosphereFlow -> Maybe Unit
getUnitForBioFlow unitDB f = M.lookup (bfUnitId f) unitDB

-- | Get unit name for a biosphere flow (fallback to "unknown" if not found)
getUnitNameForBioFlow :: UnitDB -> BiosphereFlow -> Text
getUnitNameForBioFlow unitDB f =
    maybe "unknown" unitName (getUnitForBioFlow unitDB f)

-- | Get unit information for a waste flow
getUnitForWasteFlow :: UnitDB -> WasteFlow -> Maybe Unit
getUnitForWasteFlow unitDB f = M.lookup (wfUnitId f) unitDB

-- | Get unit name for a waste flow (fallback to "unknown" if not found)
getUnitNameForWasteFlow :: UnitDB -> WasteFlow -> Text
getUnitNameForWasteFlow unitDB f =
    maybe "unknown" unitName (getUnitForWasteFlow unitDB f)

{- | Native activity-type metadata captured verbatim from the source database.

Three variants matching the three supported source formats. Each variant carries
only the fields the source format actually provides — no cross-format
normalisation, no heuristic on names. The sum type makes it impossible to
attach (for example) an ecospold integer code to a SimaPro activity.

Wire-layer JSON flattens these three variants into a single unified record
\{source, label, code?, special_label?, special_code?\} for consumer simplicity
(see ToJSON instance in API.Types).
-}
data NativeActivityType
    = {- | ecospold 2 @\<activity@ @activityType@ attribute (1..8) and optional
      @specialActivityType@. Codes are the spec's enumeration; labels are
      the spec's documented strings.
      -}
      EcoSpoldActivityType
        { eatCode :: !Int
        , eatLabel :: !Text
        , eatSpecialCode :: !(Maybe Int)
        , eatSpecialLabel :: !(Maybe Text)
        }
    | -- | SimaPro CSV @Type@ header (\"Unit process\" / \"System\").
      SimaProProcessType
        { sptLabel :: !Text
        }
    | -- | ILCD @\<processType@ XML element value.
      ILCDProcessType
        { iptLabel :: !Text
        }
    deriving (Show, Eq, Generic, NFData, Store)

{- | The identifier a source format gives the dataset block an activity was read
from: SimaPro's @Process identifier@ header, EcoSpold 1's @number@ attribute on
the dataset. It is what the SimaPro parser mints the activity UUID from, so it
is also what keeps two blocks apart where their name and location would not, and
it is what a reader copies to find the dataset back in its source file.

'Nothing' when the source format has no such field, and also when EcoSpold 2 or
ILCD name a dataset: there the identifier /is/ the activity UUID, already
spelled out in every process id, and repeating it here would give one fact two
homes.

Not a 'ProcessId': that one is a matrix row index, minted by 'buildInterningTables'.
This is the source's own string, opaque to us and stable only within one release
of a database.
-}
newtype NativeProcessId = NativeProcessId Text
    deriving (Show, Eq, Ord, Generic, NFData, Store)

{- | Per-dataset outcome of the mathematicalRelation consistency check,
computed at parse time (exchange formulas are discarded after parsing).
The stored amounts always stay authoritative; this only records how well the
dataset's formulas agree with them, for the database quality report.
-}
data FormulaCheck = FormulaCheck
    { fcEvaluated :: !Int
    -- ^ Formulas successfully evaluated
    , fcDivergent :: !Int
    -- ^ Evaluated to a value different from the stored amount (beyond float tolerance)
    , fcUnevaluable :: !Int
    -- ^ Not evaluable (unsupported functions, external references)
    , fcExample :: !(Maybe Text)
    -- ^ One divergent example, pre-rendered for display
    }
    deriving (Generic, NFData, Store)

{- | Where an activity's 'activityLocation' came from. A format with a geography
field publishes one ('LocationDeclared'); when it is blank or says
@Unspecified@, the parser falls back to the code embedded in the dataset name
(@"… {FR}"@, @"…//[RER]"@, @"…/CN U"@), which is a reading of the name and not
a declaration ('LocationInferredFromName'). 'LocationUnspecified' when neither
yielded anything, and 'activityLocation' is empty.

Recorded at parse time because only the parser can still tell the two apart:
downstream, a declared @FR@ and a guessed @FR@ are the same text. The quality
report is what surfaces the difference.
-}
data LocationSource
    = LocationDeclared
    | LocationInferredFromName
    | LocationUnspecified
    deriving (Show, Eq, Generic, NFData, Store)

{- | The source of a geography a format declares directly: the field itself when
it carries something, nothing to infer from when it doesn't.
-}
declaredLocationSource :: Text -> LocationSource
declaredLocationSource loc
    | T.null (T.strip loc) = LocationUnspecified
    | otherwise = LocationDeclared

{- | One labelled section of the documentation a dataset carries about itself:
where it was published, the technology it describes, how it was sampled, who
reviewed it. The label is what the source format calls the section, so the
consumer renders whatever the database happened to record rather than a fixed
list this type would have to keep in step with five formats.

Kept as an ordered list rather than a map because the order is the source's
own reading order (what the dataset covers, then how, then who vouched for
it), which a map would sort away.
-}
data DocSection = DocSection
    { docLabel :: !Text -- What the source format calls this section ("Technology", "Published in")
    , docText :: !Text -- Its text, already assembled from whatever fields the format spreads it over
    }
    deriving (Show, Eq, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DocSection)

{- | Base LCA activity
Note: ProcessId is the index in dbActivities vector, UUIDs stored in dbProcessIdTable
-}
data Activity = Activity
    { activityName :: !Text -- Name
    , activityDescription :: ![Text] -- General description (generalComment) by paragraphs
    , activityDocumentation :: ![DocSection] -- Provenance the dataset states about itself, in the source's own order; empty when the format records none
    , activitySynonyms :: !(M.Map Text (S.Set Text)) -- Synonyms by language, same structure as flows
    , activityClassification :: !(M.Map Text Text) -- Classifications (ISIC, CPC, etc.)
    , activityLocation :: !Text -- Location code (e.g. FR, RER)
    , activityLocationSource :: !LocationSource -- Whether the source declared that location or the parser read it off the dataset name
    , activityUnit :: !Text -- Reference unit
    , exchanges :: ![Exchange] -- List of exchanges
    , activityParams :: !(M.Map Text Double) -- Resolved dataset parameter values (SimaPro parameters, EcoSpold2 <parameter> variables)
    , activityParamExprs :: !(M.Map Text Text) -- Raw parameter expressions/formulas keyed like activityParams (for inspection and re-evaluation)
    , activityNativeType :: !(Maybe NativeActivityType) -- Source-format-native activity type (ecospold @activityType, SimaPro Type, ILCD processType); Nothing when source format lacks the field
    , activityNativeId :: !(Maybe NativeProcessId) -- Source dataset block this activity was read from: what the SimaPro parser mints the activity UUID from, and what its writer writes back. Nothing when the source format lacks the field
    , activityFormulaCheck :: !(Maybe FormulaCheck) -- Outcome of the mathematicalRelation consistency check; Nothing when the dataset has no formulas or the format has none
    }
    deriving (Generic, NFData, Store)

{- | The source block one process came out of, as a listing needs to know it.

'sbName' is for comparing, never for reading: every process of one block
carries the same one and nothing else does. It is the activity UUID, because
that is what a block is: a block is written as one row per product, and those
rows differ by their product, never by their activity.

'sbProducts' is how many products the block holds, and it is the second field
rather than something a reader could count for itself: a page showing the last
row of a block has no other way to know the block goes on.
-}
data SourceBlock = SourceBlock
    { sbName :: !Text
    , sbProducts :: !Int
    }
    deriving (Show, Eq)

{- | The block a process belongs to, read off the database that holds it.

A process no longer in the table is its own block of one, named the way
'processIdToText' names it: it is not grouped with anything, which is the
truth about a row nothing resolves.
-}
sourceBlockOf :: Database -> ProcessId -> SourceBlock
sourceBlockOf db pid = maybe orphan blockOfRef (processIdToRef db pid)
  where
    orphan :: SourceBlock
    orphan = SourceBlock{sbName = processIdToText db pid, sbProducts = 1}

    blockOfRef :: ProcessRef -> SourceBlock
    blockOfRef ref =
        SourceBlock
            { sbName = UUID.toText (prActivity ref)
            , -- An activity the index does not know is a block of one, which
              -- is what 'orphan' says of a row the table does not know.
              sbProducts = maybe 1 NE.length (M.lookup (prActivity ref) (dbActivityUUIDIndex db))
            }

{- | Is this dataset filed in its source's obsolete category?

A source that keeps a retired process in its export says so in its
classification, and the two conventions differ in where the word goes and which
cell carries it. A SimaPro CSV export gives it a segment of its own in the
category (@Others\\Obsolete@, or @Autres\\Obsolete@ in a French export). An
EcoSpold 1 export appends it to the words a segment is already made of, one
segment at a time, and writes it in whichever of the two cells describes the
retired thing: @category="material, obsolete"@, or a category that says nothing
beside @subCategory="transport, obsolete\\road, obsolete"@.

Both are the same statement, so both cells are read and cut the same way: into
segments, then each segment into its comma-separated words, and the dataset is
retired when one of those words is @obsolete@. Such a process still carries its
exchanges and still computes; what its author says is that a newer one has
replaced it. Formats with no such convention never say yes.
-}
activityIsObsolete :: Activity -> Bool
activityIsObsolete act =
    any (elem "obsolete" . classificationWords) (mapMaybe cell ["Category", "SubCategory"])
  where
    cell :: Text -> Maybe Text
    cell k = M.lookup k (activityClassification act)

    classificationWords :: Text -> [Text]
    classificationWords = map (T.toCaseFold . T.strip) . concatMap (T.splitOn ",") . T.splitOn "\\"

{- | Loop-aware tree for SVG export. Every node that exists names the row it
sits at, so an allocated activity written as several coproduct rows is several
nodes here rather than one. A declared link that no row satisfies is its own
constructor: it keeps the branch visible instead of dropping it, and it is the
only node with no row to name.
-}
data LoopAwareTree
    = TreeLeaf !ProcessId !Activity
    | TreeNode !ProcessId !Activity ![TreeChild] -- Row + activity + what it consumes
    | TreeLoop !ProcessId !Text !Int -- Already visited, or depth/budget spent: row + ActivityName + Depth
    | TreeMissing !UUID !Text !Int -- Declared link no row satisfies: activity UUID + name + Depth

{- | One branch under a 'TreeNode': how much the parent consumes, the product
flow the edge is labelled with, and the subtree that supplies it.
-}
data TreeChild = TreeChild
    { childAmount :: !Double
    , childFlow :: !TechnosphereFlow
    , childSubtree :: !LoopAwareTree
    }

-- | Technosphere flow database (deduplicated by UUID)
type TechFlowDB = M.Map UUID TechnosphereFlow

-- | Biosphere flow database (deduplicated by UUID)
type BioFlowDB = M.Map UUID BiosphereFlow

-- | Map from waste flow UUID to WasteFlow (Map for O(log n) lookups)
type WasteFlowDB = M.Map UUID WasteFlow

{- | A parsed flow tagged with its kind. Returned by parsers when an
@Exchange@ carries its corresponding @*Flow@ catalog entry. Distinct from
'ApiFlow' (which is wire-shape, with an 'ApiUnresolvedFlow' fallback) —
parsers never produce unresolved entries.
-}
data ParsedFlow
    = ParsedTech !TechnosphereFlow
    | ParsedBio !BiosphereFlow
    deriving (Generic, NFData)

{- | A resolved flow tagged with its kind. Returned by lookup/search code
(e.g. 'findFlowsBySynonym') so consumers can render the appropriate shape
via the 'flowKind*' projections below, or lift into the wire layer via
@API.Types.apiFlowOfKind@.
-}
data FlowKind
    = TechKind !TechnosphereFlow
    | BioKind !BiosphereFlow
    | WasteKind !WasteFlow

-- | UUID accessor. Total over the three flow kinds.
flowKindId :: FlowKind -> UUID
flowKindId (TechKind f) = tfId f
flowKindId (BioKind f) = bfId f
flowKindId (WasteKind f) = wfId f

-- | Display-name accessor. Total over the three flow kinds.
flowKindName :: FlowKind -> Text
flowKindName (TechKind f) = tfName f
flowKindName (BioKind f) = bfName f
flowKindName (WasteKind f) = wfName f

-- | Synonyms accessor (keyed by language code). Total.
flowKindSynonyms :: FlowKind -> M.Map Text (S.Set Text)
flowKindSynonyms (TechKind f) = tfSynonyms f
flowKindSynonyms (BioKind f) = bfSynonyms f
flowKindSynonyms (WasteKind f) = wfSynonyms f

{- | "Category" projection for flat list views. Biosphere flows carry a
compartment; technosphere and waste flows have none here (their taxonomy
lives on the producing/consuming activity).
-}
flowKindCategory :: FlowKind -> Text
flowKindCategory (TechKind _) = ""
flowKindCategory (BioKind f) = bfCompartmentName f
flowKindCategory (WasteKind _) = ""

{- | Sub-compartment projection, the companion of 'flowKindCategory'. Two
biosphere flows of the same name and medium are told apart by this alone
(e.g. @soil / agricultural@ vs @soil / forestry@), so a flat list view that
drops it shows what look like duplicate rows.
-}
flowKindCompartmentSub :: FlowKind -> Maybe Text
flowKindCompartmentSub (TechKind _) = Nothing
flowKindCompartmentSub (BioKind f) = bfCompartmentSub f
flowKindCompartmentSub (WasteKind _) = Nothing

-- | Unit-id accessor — for unit-name lookup against the 'UnitDB'.
flowKindUnitId :: FlowKind -> UUID
flowKindUnitId (TechKind f) = tfUnitId f
flowKindUnitId (BioKind f) = bfUnitId f
flowKindUnitId (WasteKind f) = wfUnitId f

{- | Unit-name accessor. Falls back to "unknown" when the unit UUID isn't in
the database, matching the per-flow @getUnitNameForXxxFlow@ helpers.
-}
flowKindUnitName :: UnitDB -> FlowKind -> Text
flowKindUnitName udb (TechKind f) = getUnitNameForTechFlow udb f
flowKindUnitName udb (BioKind f) = getUnitNameForBioFlow udb f
flowKindUnitName udb (WasteKind f) = getUnitNameForWasteFlow udb f

-- | Biosphere compartment, if any. Tech/waste flows carry no compartment.
flowKindCompartment :: FlowKind -> Maybe Compartment
flowKindCompartment (TechKind _) = Nothing
flowKindCompartment (BioKind f) = bfCompartment f
flowKindCompartment (WasteKind _) = Nothing

{- | Which of the three an inventory line is, as a value rather than as the
line itself: a product taken from the technosphere, a substance exchanged with
nature, or a waste. An exchange has one and so does the flow it carries, which
is why one type answers for both, 'exchangeKindOf' reading it off a line and
'kindOfFlow' off a resolved flow.

It is what the @exchange_type@, @filter_exchange_type@ and @kind@ parameters
name, and 'exchangeKindName' is the one place their spelling lives.
-}
data ExchangeKind = KindTechnosphere | KindBiosphere | KindWaste
    deriving (Eq, Show, Enum, Bounded)

{- | Project an 'Exchange' onto its kind. Total, so a fourth variant would
surface as a compile error here.
-}
exchangeKindOf :: Exchange -> ExchangeKind
exchangeKindOf TechnosphereExchange{} = KindTechnosphere
exchangeKindOf BiosphereExchange{} = KindBiosphere
exchangeKindOf WasteExchange{} = KindWaste

-- | Project a resolved flow onto the same three. Total.
kindOfFlow :: FlowKind -> ExchangeKind
kindOfFlow (TechKind _) = KindTechnosphere
kindOfFlow (BioKind _) = KindBiosphere
kindOfFlow (WasteKind _) = KindWaste

-- | How a kind is spelled on every surface that names one.
exchangeKindName :: ExchangeKind -> Text
exchangeKindName KindTechnosphere = "technosphere"
exchangeKindName KindBiosphere = "biosphere"
exchangeKindName KindWaste = "waste"

{- | What a waste line does within its activity.

A consumer used to read this off the target being absent, which conflates two
opposite statements: a waste nothing treats, and a waste whose named treatment
is in no loaded database. The first is a complete description of an end-of-life
flow, the second is a gap in what was loaded, and calling the second final says
the burden is accounted for when it is missing. Only the engine holds both
facts, so the engine states the role instead of leaving it to be inferred.
-}
data WasteRole
    = -- | An input: this activity is the one treating the waste.
      TreatsWaste
    | -- | An output whose treatment resolved.
      SentToTreatment
    | -- | An output naming no treatment: nothing treats this waste.
      FinalWasteFlow
    | -- | An output naming a treatment no loaded database ships.
      TreatmentNotLoaded
    deriving (Eq, Show, Generic, NFData, Enum, Bounded)
    deriving anyclass (ToSchema)

{- | Read a kind a request asked for. 'Nothing' for anything else, so the
caller refuses with its own message rather than filtering on a guess.
-}
parseExchangeKind :: Text -> Maybe ExchangeKind
parseExchangeKind = codedBy exchangeKindName

{- | The kinds a request may name, for the message that refuses the others.
Read off the type, so a fourth kind cannot go unmentioned.
-}
exchangeKindChoices :: Text
exchangeKindChoices = T.intercalate " | " (map exchangeKindName [minBound .. maxBound])

{- | Which kinds of flow a search keeps.

Not a list, because an empty list would have to mean either "every kind" or
"no kind at all" and a caller could not tell which it had asked for. A search
box offering a tab per bucket needs more than one kind at a time: the third
bucket is what is exchanged with nature or discarded, which is two of them.
-}
data KindFilter = AnyKind | OnlyKinds (NonEmpty ExchangeKind)
    deriving (Eq, Show)

{- | Read the kinds a request named, comma-separated.

Every reading that names no kind is a refusal, an empty parameter included.
@kind=@ and @kind=,@ come from a caller that meant to name something, and
answering them with every kind would widen the search with no sign the filter
was dropped - the same mistake as reading a typo as "every kind". Only an
absent parameter means every kind, and that is its caller's own arm, not a
reading of the text.
-}
parseKindNames :: Text -> Either Text (NonEmpty ExchangeKind)
parseKindNames raw = case NE.nonEmpty (filter (not . T.null) (map T.strip (T.splitOn "," raw))) of
    Nothing -> Left ("kind names no kind: use " <> exchangeKindChoices <> ", separated by commas")
    Just named -> traverse readOne named
  where
    readOne :: Text -> Either Text ExchangeKind
    readOne name =
        maybe (Left ("kind must be one of: " <> exchangeKindChoices <> " (got " <> name <> ")")) Right $
            parseExchangeKind name

-- | Whether a search that asked for these kinds keeps this one.
kindFilterKeeps :: KindFilter -> ExchangeKind -> Bool
kindFilterKeeps kinds kind = case kinds of
    AnyKind -> True
    OnlyKinds named -> kind `elem` named

-- | Unit database (deduplicated)
type UnitDB = M.Map UUID Unit

{- | Temporary Map structure used during database loading phase
Maps from (activityUUID, productUUID) pairs extracted from .spold filenames
This is converted to ActivityDB Vector during database construction
-}
type ActivityMap = M.Map (UUID, UUID) Activity

{- | Activity database - Vector for direct indexing by ProcessId (Int32)
Each spold file (activity_uuid_product_uuid.spold) becomes a separate entry
-}
type ActivityDB = V.Vector Activity

-- | Index by activity name - case-insensitive name lookup
type NameIndex = M.Map Text [UUID] -- Name -> [ActivityUUID]

-- | Index by location - geographic search
type LocationIndex = M.Map Text [UUID] -- Location -> [ActivityUUID]

{- | Index by flow - find the rows that use a given flow. Keyed on the row and
not on the activity UUID, so an activity written as several coproduct rows is
listed once per row rather than once per activity.
-}
type FlowIndex = M.Map UUID [ProcessId] -- FlowID -> [ProcessId]

-- | Index by reference unit - unit-based activity search
type ActivityUnitIndex = M.Map Text [UUID] -- Unit -> [ActivityUUID]

{- | Complete index structure for efficient searches
Memory optimization: Removed unused exchange indexes that were duplicating 600K Exchange records
across 5 maps (idxExchangeByFlow, idxExchangeByActivity, idxReferenceProducts,
idxInputsByActivity, idxOutputsByActivity) - saves ~3-4GB RAM
-}
data Indexes = Indexes
    { -- Activity-level indexes
      idxByName :: !NameIndex -- Search activities by name
    , idxByLocation :: !LocationIndex -- Search activities by location
    , idxByFlow :: !FlowIndex -- Search activities using a flow
    , idxByUnit :: !ActivityUnitIndex -- Search activities by reference unit
    -- Note: Exchange-level indexes removed - exchanges can be accessed directly from Activity.exchanges
    }
    deriving (Generic, NFData, Store)

{- | Sparse matrix coordinate triplet (row, col, value)
Using Int32 for matrix indices to support large databases (up to 2 billion activities)
Unboxed to eliminate per-element boxing overhead (~48 bytes → 16 bytes per triple)
With ~800K triples, this saves ~25MB + significant GC pressure
-}
data SparseTriple = SparseTriple {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 {-# UNPACK #-} !Double
    deriving (Eq, Show, Generic)

-- Manual Unbox instance for SparseTriple to enable VU.Vector storage
newtype instance VU.MVector s SparseTriple = MV_SparseTriple (VU.MVector s (Int32, Int32, Double))
newtype instance VU.Vector SparseTriple = V_SparseTriple (VU.Vector (Int32, Int32, Double))

instance VGM.MVector VU.MVector SparseTriple where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MV_SparseTriple v) = VGM.basicLength v
    basicUnsafeSlice i n (MV_SparseTriple v) = MV_SparseTriple $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_SparseTriple v1) (MV_SparseTriple v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = MV_SparseTriple <$> VGM.basicUnsafeNew n
    basicInitialize (MV_SparseTriple v) = VGM.basicInitialize v
    basicUnsafeRead (MV_SparseTriple v) i = do
        (r, c, val) <- VGM.basicUnsafeRead v i
        return $ SparseTriple r c val
    basicUnsafeWrite (MV_SparseTriple v) i (SparseTriple r c val) =
        VGM.basicUnsafeWrite v i (r, c, val)

instance VG.Vector VU.Vector SparseTriple where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_SparseTriple v) = V_SparseTriple <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_SparseTriple v) = MV_SparseTriple <$> VG.basicUnsafeThaw v
    basicLength (V_SparseTriple v) = VG.basicLength v
    basicUnsafeSlice i n (V_SparseTriple v) = V_SparseTriple $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_SparseTriple v) i = do
        (r, c, val) <- VG.basicUnsafeIndexM v i
        return $ SparseTriple r c val

instance VU.Unbox SparseTriple

-- Store instance for cache serialization
instance Store SparseTriple where
    size = ConstSize 16 -- Int32 (4) + Int32 (4) + Double (8)
    poke (SparseTriple r c v) = poke r >> poke c >> poke v
    peek = SparseTriple <$> peek <*> peek <*> peek

-- NFData derived via Generic
instance NFData SparseTriple

-- | Store instance for VU.Vector SparseTriple (store has no built-in instance for custom Unbox types)
instance Store (VU.Vector SparseTriple) where
    size = VarSize $ \v -> 8 + VU.length v * 16
    poke v = do
        poke (VU.length v)
        VU.mapM_ poke v
    peek = do
        n <- peek
        VU.replicateM n peek

-- | Compute serialized size for a Store value
getSize :: (Store a) => a -> Int
getSize x = case size of
    ConstSize n -> n
    VarSize f -> f x

-- | Pre-computed matrix factorization for fast inventory calculations
data MatrixFactorization = MatrixFactorization
    { mfSystemMatrix :: !(VU.Vector SparseTriple) -- Cached (I - A) system matrix (unboxed)
    , mfActivityCount :: !Int32 -- Matrix dimension
    , mfDatabaseId :: !Text -- Database identifier for per-database cache lookup
    }
    deriving (Generic, NFData, Store)

{- | Index for looking up activities by their reference product attributes
Used for: (1) upstream link resolution for SimaPro data, (2) future product search
-}
data ProductIndex = ProductIndex
    { piByUUID :: !(M.Map UUID (NonEmpty ProcessId)) -- Product flow UUID → the rows producing it (for upstream links)
    , piByName :: !(M.Map Text [ProcessId]) -- Normalized product name → [ProcessId] (for search)
    , piByLocation :: !(M.Map Text [ProcessId]) -- Location → [ProcessId] (for search)
    }
    deriving (Generic, NFData, Store)

-- | Empty product index (used as default when loading old cache files)
emptyProductIndex :: ProductIndex
emptyProductIndex = ProductIndex M.empty M.empty M.empty

{- | What the loader read besides the source files, and what shapes a database
as much as they do. The unit table decides which exchanges convert, which of
them link, and the unit every amount is recorded in; the location aliases
decide which dataset an EcoSpold 1 exchange resolves to. A database records
the pair it was built with in 'dbBuiltWith', and its matrix cache is trusted
only while that pair is the one in force.

Everything else the loader is handed (the synonym set, the geography
hierarchy and policy, the other databases) shapes the cross-database links
alone, and every cache hit derives those again. It is left out on purpose:
the synonym set grows after every load, so stamping it would rebuild every
database at the next start.
-}
data BuildInputs = BuildInputs
    { biUnitConfig :: !UnitConfig
    , biLocationAliases :: !(M.Map Text Text)
    , biAllocation :: !AllocationKey
    }
    deriving (Eq, Show, Generic, NFData, Store)

-- | Complete database with indexes for efficient searches
data Database = Database
    { -- UUID interning tables for ProcessId ↔ (UUID, UUID) conversion
      dbProcessIdTable :: !(V.Vector (UUID, UUID)) -- ProcessId (Int32) → (activityUUID, productUUID)
    , dbProcessIdLookup :: !(M.Map (UUID, UUID) ProcessId) -- reverse lookup
    , dbActivityUUIDIndex :: !(M.Map UUID (NonEmpty ProcessId)) -- Activity UUID → the rows that activity was written as, which is also the products of one source block
    , dbProductIndex :: !ProductIndex -- Product flow → ProcessId lookups (for SimaPro links & product search)
    , dbActivities :: !ActivityDB -- Vector of activities indexed by ProcessId
    , dbTechFlows :: !TechFlowDB -- Technosphere flows by UUID
    , dbBioFlows :: !BioFlowDB -- Biosphere flows by UUID
    , dbWasteFlows :: !WasteFlowDB -- Waste flows by UUID
    , dbUnits :: !UnitDB
    , dbIndexes :: !Indexes
    , -- Pre-computed sparse matrices for efficient LCA calculations (unboxed for memory efficiency)
      dbTechnosphereTriples :: !(VU.Vector SparseTriple) -- A matrix: activities × activities (sparse, unboxed)
    , dbBiosphereTriples :: !(VU.Vector SparseTriple) -- B matrix: biosphere flows × activities (sparse, unboxed)
    , dbActivityIndex :: !(V.Vector Int32) -- ProcessId → matrix index mapping (direct vector indexing)
    , dbBiosphereOrder :: !(V.Vector UUID) -- Ordered vector of biosphere flow UUIDs — B-matrix row order
    , dbActivityCount :: !Int32 -- Number of activities (matrix dimension)
    , dbBiosphereCount :: !Int32 -- Number of biosphere flows (matrix dimension)
    -- Cross-database linking (serialized to cache)
    , dbCrossDBLinks :: ![CrossDBLink] -- Cross-database supplier links (for chained solving)
    , dbDependsOn :: ![Text] -- Names of databases this database depends on
    -- Linking statistics (serialized to cache for setup page)
    , dbLinkingStats :: !CrossDBLinkingStats -- Cross-DB linking statistics (completeness, fallbacks, etc.)
    -- What it was built with (serialized to cache, compared before a cache is trusted)
    , dbBuiltWith :: !BuildInputs
    , -- Runtime-only fields (not serialized to cache)
      dbSynonymDB :: !(Maybe SynonymDB) -- Embedded synonym database for flow matching
    , dbFlowsByName :: !(M.Map Text [BiosphereFlow]) -- Biosphere flow name index for LCIA matching
    , dbFlowsByCAS :: !(M.Map Text [BiosphereFlow]) -- CAS → biosphere flows for LCIA matching
    -- Product name search index: word token → ProcessId set (built at runtime)
    , dbProductSearchIndex :: !(M.Map Text IS.IntSet)
    , -- BM25 ranking index (built at runtime, not serialized)
      dbBM25Index :: !(Maybe BM25Index)
    }
    deriving (Generic, NFData)

{- | Custom Store instance for Database
Only serializes persistent fields (not runtime-only: factorization, synonymDB, flowsByName)
-}
instance Store Database where
    size = VarSize $ \db ->
        getSize (dbProcessIdTable db)
            + getSize (dbProcessIdLookup db)
            + getSize (dbActivityUUIDIndex db)
            + getSize (dbProductIndex db)
            + getSize (dbActivities db)
            + getSize (dbTechFlows db)
            + getSize (dbBioFlows db)
            + getSize (dbWasteFlows db)
            + getSize (dbUnits db)
            + getSize (dbIndexes db)
            + getSize (dbTechnosphereTriples db)
            + getSize (dbBiosphereTriples db)
            + getSize (dbActivityIndex db)
            + getSize (dbBiosphereOrder db)
            + getSize (dbActivityCount db)
            + getSize (dbBiosphereCount db)
            + getSize (dbCrossDBLinks db)
            + getSize (dbDependsOn db)
            + getSize (dbLinkingStats db)
            + getSize (dbBuiltWith db)

    poke db = do
        poke (dbProcessIdTable db)
        poke (dbProcessIdLookup db)
        poke (dbActivityUUIDIndex db)
        poke (dbProductIndex db)
        poke (dbActivities db)
        poke (dbTechFlows db)
        poke (dbBioFlows db)
        poke (dbWasteFlows db)
        poke (dbUnits db)
        poke (dbIndexes db)
        poke (dbTechnosphereTriples db)
        poke (dbBiosphereTriples db)
        poke (dbActivityIndex db)
        poke (dbBiosphereOrder db)
        poke (dbActivityCount db)
        poke (dbBiosphereCount db)
        -- Cross-database linking fields
        poke (dbCrossDBLinks db)
        poke (dbDependsOn db)
        poke (dbLinkingStats db)
        poke (dbBuiltWith db)

    -- Runtime-only fields are NOT serialized

    peek = do
        processIdTable <- peek
        processIdLookup <- peek
        activityUUIDIndex <- peek
        productIndex <- peek
        activities <- peek
        techFlows <- peek
        bioFlows <- peek
        wasteFlows <- peek
        units <- peek
        indexes <- peek
        techTriples <- peek
        bioTriples <- peek
        activityIndex <- peek
        biosphereOrder <- peek
        activityCount <- peek
        biosphereCount <- peek
        crossDBLinks <- peek
        dependsOn <- peek
        linkingStats <- peek
        builtWith <- peek
        return
            Database
                { dbProcessIdTable = processIdTable
                , dbProcessIdLookup = processIdLookup
                , dbActivityUUIDIndex = activityUUIDIndex
                , dbProductIndex = productIndex
                , dbActivities = activities
                , dbTechFlows = techFlows
                , dbBioFlows = bioFlows
                , dbWasteFlows = wasteFlows
                , dbUnits = units
                , dbIndexes = indexes
                , dbTechnosphereTriples = techTriples
                , dbBiosphereTriples = bioTriples
                , dbActivityIndex = activityIndex
                , dbBiosphereOrder = biosphereOrder
                , dbActivityCount = activityCount
                , dbBiosphereCount = biosphereCount
                , -- Cross-database linking fields
                  dbCrossDBLinks = crossDBLinks
                , dbDependsOn = dependsOn
                , dbLinkingStats = linkingStats
                , dbBuiltWith = builtWith
                , -- Runtime-only fields set to defaults
                  dbSynonymDB = Nothing
                , dbFlowsByName = M.empty
                , dbFlowsByCAS = M.empty
                , dbProductSearchIndex = M.empty
                , dbBM25Index = Nothing
                }

-- | Helper functions for ProcessId and Database operations

-- | Get activity by ProcessId (direct vector indexing)
getActivity :: Database -> ProcessId -> Maybe Activity
getActivity db pid = dbActivities db V.!? fromIntegral pid

-- | Find ProcessId from UUID pair
findProcessId :: Database -> UUID -> UUID -> Maybe ProcessId
findProcessId db actUUID prodUUID =
    M.lookup (actUUID, prodUUID) (dbProcessIdLookup db)

{- | The row an activity UUID names, when it names one.

An EcoSpold link and a bare activity UUID typed into the API both carry the
activity alone, while a row is a pair. An allocated activity is written as one
row per coproduct, so the UUID names several and there is no way to tell which
one the caller meant: answering with any of them is guesswork the caller cannot
see. Callers that hold the product too should use 'findProcessId'.
-}
findProcessIdByActivityUUID :: Database -> UUID -> Maybe ProcessId
findProcessIdByActivityUUID db searchUUID =
    M.lookup searchUUID (dbActivityUUIDIndex db) >>= sole

-- | The one element of a 'NonEmpty' that has exactly one.
sole :: NonEmpty a -> Maybe a
sole (x :| []) = Just x
sole (_ :| (_ : _)) = Nothing

{- | Find supplier ProcessId by product flow UUID.
ESSENTIAL for SimaPro data: exchanges have no techActivityLinkId, but techFlowId is valid.

Answers only when exactly one row produces the flow. A product made in several
geographies is produced by several rows, and naming one of them would answer a
question the caller never asked.
-}
findProcessIdByProductFlow :: Database -> UUID -> Maybe ProcessId
findProcessIdByProductFlow db flowUUID =
    M.lookup flowUUID (piByUUID $ dbProductIndex db) >>= sole

{- | Look up an exchange's flow on the appropriate side. Each exchange variant
has exactly one flow side by construction (tech, bio, or waste), so the
result is a single 'FlowKind' or 'Nothing' when the UUID is absent from the
database.
-}
lookupExchangeFlow :: Database -> Exchange -> Maybe FlowKind
lookupExchangeFlow db TechnosphereExchange{techFlowId = fid} =
    TechKind <$> M.lookup fid (dbTechFlows db)
lookupExchangeFlow db BiosphereExchange{bioFlowId = fid} =
    BioKind <$> M.lookup fid (dbBioFlows db)
lookupExchangeFlow db WasteExchange{waFlowId = fid} =
    WasteKind <$> M.lookup fid (dbWasteFlows db)

{- | Search products by name (for future product search feature)
Returns all ProcessIds that produce products matching the given name
-}
searchProductsByName :: Database -> Text -> [ProcessId]
searchProductsByName db query =
    M.findWithDefault [] (T.toLower query) (piByName $ dbProductIndex db)

{- | Search products by location (for future product search feature)
Returns all ProcessIds that produce products at the given location
-}
searchProductsByLocation :: Database -> Text -> [ProcessId]
searchProductsByLocation db loc =
    M.findWithDefault [] loc (piByLocation $ dbProductIndex db)

-- | The pair a 'ProcessId' indexes, if the index is in range.
processIdToRef :: Database -> ProcessId -> Maybe ProcessRef
processIdToRef db pid = refOf <$> dbProcessIdTable db V.!? fromIntegral pid
  where
    refOf :: (UUID, UUID) -> ProcessRef
    refOf (act, prod) = ProcessRef{prActivity = act, prProduct = prod}

{- | The one spelling of a process reference: @activityUUID_productUUID@.
Everything that writes a reference — the wire, a @.spold@ file name, an ILCD
process identifier — goes through here, so there is one place the separator
is decided.
-}
processRefText :: ProcessRef -> Text
processRefText r = UUID.toText (prActivity r) <> refSeparator <> UUID.toText (prProduct r)

-- | The separator between the two halves. Inverse of 'parseProcessRef'.
refSeparator :: Text
refSeparator = "_"

{- | A process reference qualified by the database that holds it,
@db::activityUUID_productUUID@ — how a cross-database supplier is named on the
wire, and what a substitution endpoint accepts back. Its reader is
@API.Types.parseSubRef@.
-}
qualifyRef :: Text -> Text -> Text
qualifyRef dbName ref = dbName <> "::" <> ref

{- | How a cross-database supplier is named on the wire: the reference to the
supplying process, qualified by the database that holds it. One renderer, so
the several payloads carrying a link's identity cannot spell it three ways.
-}
supplierRefText :: CrossDBLink -> Text
supplierRefText link =
    qualifyRef
        (cdlSourceDatabase link)
        (processRefText ProcessRef{prActivity = cdlSupplierActUUID link, prProduct = cdlSupplierProdUUID link})

-- | Text form of the process a 'ProcessId' indexes, for display.
processIdToText :: Database -> ProcessId -> Text
processIdToText db pid =
    maybe ("invalid-process-id-" <> T.pack (show pid)) processRefText (processIdToRef db pid)

{- | Pure syntactic parse of a process reference. Returns the pair when the
text has the expected shape, regardless of whether it exists in any database.
'Nothing' is a genuine format error — callers treat a well-formed-but-absent
reference as not-found, not malformed.
-}
parseProcessRef :: Text -> Maybe ProcessRef
parseProcessRef t = case T.splitOn refSeparator t of
    [actText, prodText]
        | not (T.null actText)
        , not (T.null prodText) ->
            ProcessRef <$> UUID.fromText actText <*> UUID.fromText prodText
    _ -> Nothing

{- | The activity a reference names. Accepts a bare activity UUID as well as
the full @activityUUID_productUUID@ form, because callers that only need the
activity half are given both by the surfaces above them.
-}
refActivityUUID :: Text -> Maybe UUID
refActivityUUID t = case parseProcessRef t of
    Just r -> Just (prActivity r)
    Nothing -> UUID.fromText t

-- | Add SynonymDB to a Database (used after loading from cache)
addSynonymDBToDatabase :: Database -> SynonymDB -> Database
addSynonymDBToDatabase db synDB = db{dbSynonymDB = Just synDB}

{- | Build the biosphere flow name index. Groups flows by normalized name
(primary + synonyms) for efficient LCIA lookup.
-}
buildFlowNameIndex :: BioFlowDB -> M.Map Text [BiosphereFlow]
buildFlowNameIndex bioDB =
    M.fromListWith (++) $ concatMap flowEntries (M.elems bioDB)
  where
    flowEntries f =
        let primary = normalizeName (bfName f)
            synKeys =
                [ normalizeName syn
                | syns <- M.elems (bfSynonyms f)
                , syn <- S.toList syns
                ]
         in [(k, [f]) | k <- nub (primary : synKeys)]

{- | Build CAS index from biosphere flows.

Keyed by the canonical spelling, because a flow and the method factor that
characterizes it are read by different parsers and only one of them
canonicalizes on the way in. A flow stating no CAS — empty, or a placeholder
made of zeros and dashes — is left out rather than indexed under a key
unrelated substances would share.
-}
buildFlowCASIndex :: BioFlowDB -> M.Map Text [BiosphereFlow]
buildFlowCASIndex bioDB =
    M.fromListWith
        (++)
        [ (cas, [f])
        | f <- M.elems bioDB
        , Just stated <- [bfCAS f]
        , Just cas <- [nonEmptyCAS stated]
        ]

-- | Add biosphere flow indexes (name + CAS) and search index to a Database
addFlowNameIndexToDatabase :: Database -> Database
addFlowNameIndexToDatabase db =
    db
        { dbFlowsByName = buildFlowNameIndex (dbBioFlows db)
        , dbFlowsByCAS = buildFlowCASIndex (dbBioFlows db)
        , dbProductSearchIndex = buildProductSearchIndex (dbActivities db) (dbTechFlows db)
        }

{- | The biosphere flows a database's characterization has to reach: its own,
plus those of every database it depends on.

A cross-database inventory carries the dependencies' flows, so a matcher
cascade built on the root's flows alone resolves nothing for them — the
synonym, proxy and regional bridges all need a flow to point at. The score
then loses whatever those bridges would have found, silently, because the
inventory is right and only the factors are missing.
-}
data FlowClosure = FlowClosure
    { clByUUID :: !BioFlowDB
    , clByName :: !(M.Map Text [BiosphereFlow])
    , clByCAS :: !(M.Map Text [BiosphereFlow])
    }

-- | The closure of a database that depends on nothing: its own prebuilt indexes.
ownFlowClosure :: Database -> FlowClosure
ownFlowClosure db =
    FlowClosure
        { clByUUID = dbBioFlows db
        , clByName = dbFlowsByName db
        , clByCAS = dbFlowsByCAS db
        }

{- | The closure of a database over its dependencies. The root comes first
everywhere: a UUID two databases both declare keeps the root's metadata, and a
name or CAS two of them declare under /different/ UUIDs still resolves to the
root's flow, since 'Method.Mapping.pickByCompartment' reads its candidates in
order. Adding a dependency therefore never moves a resolution the root already
had.
-}
flowClosure :: Database -> [Database] -> FlowClosure
flowClosure root [] = ownFlowClosure root
flowClosure root deps =
    let !depFlows = M.unions (map dbBioFlows deps) `M.difference` dbBioFlows root
        !merged = M.union (dbBioFlows root) depFlows
     in FlowClosure
            { clByUUID = merged
            , clByName = M.unionWith (++) (dbFlowsByName root) (buildFlowNameIndex depFlows)
            , clByCAS = M.unionWith (++) (dbFlowsByCAS root) (buildFlowCASIndex depFlows)
            }

{- | Fill empty @bfCAS@ from registry name→CAS bindings, then rebuild the CAS
index so the native CAS bridge fires. Holes only — a CAS the database itself
provided is authoritative and never overwritten, which bounds any risk from a
binding applying to a name another source uses differently. A no-op (the same
'Database') when there are no bindings, so this never touches databases the
registry doesn't speak to.
-}
enrichBioFlowCAS :: M.Map NormName CASNumber -> Database -> Database
enrichBioFlowCAS bindings db
    | M.null bindings = db
    | otherwise = addFlowNameIndexToDatabase db{dbBioFlows = fillBioFlowCAS bindings (dbBioFlows db)}

{- | Fill empty @bfCAS@ from name→CAS bindings (holes only) — the pure core of
'enrichBioFlowCAS', over the flow map alone so it is testable without a whole
'Database'. A flow that already carries a non-empty CAS is left untouched.
-}
fillBioFlowCAS :: M.Map NormName CASNumber -> BioFlowDB -> BioFlowDB
fillBioFlowCAS bindings = M.map fill
  where
    fill f
        | hasCAS (bfCAS f) = f
        | otherwise = case M.lookup (NormName (normalizeName (bfName f))) bindings of
            Just (CASNumber c) -> f{bfCAS = Just c}
            Nothing -> f
    hasCAS (Just c) = not (T.null c)
    hasCAS Nothing = False

{- | Build word-token product search index: lowercased word → IntSet of ProcessIds
Tokenizes reference product flow names so product search can use index intersection.
-}
buildProductSearchIndex :: V.Vector Activity -> TechFlowDB -> M.Map Text IS.IntSet
buildProductSearchIndex activities techDB =
    V.ifoldl' addActivity M.empty activities
  where
    addActivity !acc i a =
        let pid = fromIntegral i
            productWords =
                [ w
                | ex <- exchanges a
                , exchangeIsReference ex
                , not (exchangeIsInput ex)
                , Just flow <- [M.lookup (exchangeFlowId ex) techDB]
                , w <- T.words (T.toLower (tfName flow))
                , not (T.null w)
                ]
         in foldl' (\m w -> MS.insertWith IS.union w (IS.singleton pid) m) acc productWords

-- | Lookup a technosphere flow by UUID
lookupTechFlow :: Database -> UUID -> Maybe TechnosphereFlow
lookupTechFlow db uuid = M.lookup uuid (dbTechFlows db)

-- | Lookup a biosphere flow by UUID
lookupBioFlow :: Database -> UUID -> Maybe BiosphereFlow
lookupBioFlow db uuid = M.lookup uuid (dbBioFlows db)

{- | Add both SynonymDB and flow name index to a Database
Convenience function for post-load initialization
-}
initializeRuntimeFields :: Database -> SynonymDB -> Database
initializeRuntimeFields db synDB =
    let db' = addSynonymDBToDatabase db synDB
     in addFlowNameIndexToDatabase db'

{- | Simplified version without indexes (for loading compatibility)
Used during database loading, before conversion to final Vector structure
-}
data SimpleDatabase = SimpleDatabase
    { sdbActivities :: !ActivityMap -- Temporary Map structure
    , sdbTechFlows :: !TechFlowDB
    , sdbBioFlows :: !BioFlowDB
    , sdbWasteFlows :: !WasteFlowDB
    , sdbUnits :: !UnitDB
    }
    deriving (Generic, Store)

{- | Reconstruct SimpleDatabase from a fully-built Database
Used to skip re-parsing when a valid cache exists during staging
-}
toSimpleDatabase :: Database -> SimpleDatabase
toSimpleDatabase db =
    SimpleDatabase
        { sdbActivities = M.fromList $ V.toList $ V.zipWith (,) (dbProcessIdTable db) (dbActivities db)
        , sdbTechFlows = dbTechFlows db
        , sdbBioFlows = dbBioFlows db
        , sdbWasteFlows = dbWasteFlows db
        , sdbUnits = dbUnits db
        }

-- | Blocking reason for cross-database linking failure
data LinkBlocker
    = -- | Product not found at all
      NoNameMatch
    | -- | The supplier ships that product under a unit the demand cannot use.
      UnitIncompatible
        { uiQueryUnit :: !Text
        , uiSupplierUnit :: !Text
        }
    | -- | requestedLoc (no fallback found above threshold)
      LocationUnavailable !Text
    | -- | A match existed and the database's geography_policy rejected it.
      LocationRejectedByPolicy
        { lrRequested :: !Text
        , lrBestCandidate :: !Text
        , lrBestKind :: !LocationKind
        }
    | {- | targetName, targetLocation — a relink-mapping row designated a supplier
      that no pinned dependency ships ('Nothing' when the name matches nowhere,
      'Just' the pinned location when the name exists but not there). A curated
      designation must fail loudly, never fall back to the generic cascade.
      -}
      AliasTargetMissing !Text !(Maybe Text)
    deriving (Show, Eq, Generic, NFData, Store)

{- | A physical property of a product, which shares can be divided on.

Each names one measurable quantity and nothing else. There is no plain @Mass@:
EcoSpold 2 states dry and wet mass separately, and the Abondance cheese block
divides 51 % on dry matter where its wet mass would give 12 %, so a key that
did not say which one it meant would be a key nobody could check.
-}
data AllocationProperty
    = DryMass
    | WetMass
    deriving (Show, Eq, Generic, NFData, Store)

{- | How the shares of a multi-output activity are decided. Maps to TOML field
@allocation@ on a database entry.

* 'Declared'   — the shares the source states, a number or an evaluated formula.
* 'ByProperty' — recomputed from a physical property of the products.

A database is loaded under one key, and the same source loaded twice under two
keys is two databases: the choice belongs to the load, because it decides the
inventory of every process the load produces.
-}
data AllocationKey
    = Declared
    | ByProperty !AllocationProperty
    deriving (Show, Eq, Generic, NFData, Store)

{- | The word a property is written as, wherever one is written: a warning, a
TOML file, a @meta.toml@, a query parameter.
-}
propertyName :: AllocationProperty -> Text
propertyName prop = case prop of
    DryMass -> "dry mass"
    WetMass -> "wet mass"

-- | The word a key is written as. Inverse of 'parseAllocationKey'.
allocationKeyText :: AllocationKey -> Text
allocationKeyText key = case key of
    Declared -> "declared"
    ByProperty prop -> propertyName prop

{- | The key a word names, case-blind, with the words it could have been when
it names none.

One vocabulary for the config file, the upload metadata and the request that
asks for a key, so a word that works in one works in all three. An unknown
word is refused rather than read as 'Declared': a database loaded under a key
nobody asked for, carrying the name of the key they did ask for, is the silent
misbehaviour this returns 'Left' to avoid.
-}
parseAllocationKey :: Text -> Either Text AllocationKey
parseAllocationKey raw =
    maybe (Left refusal) Right (lookup (T.toLower (T.strip raw)) table)
  where
    table :: [(Text, AllocationKey)]
    table = [(allocationKeyText k, k) | k <- [Declared, ByProperty DryMass, ByProperty WetMass]]

    refusal :: Text
    refusal = "expected one of " <> T.intercalate " | " (map fst table) <> ", got: " <> raw

{- | Per-database knob controlling how aggressively geography may be widened when
linking an exchange to a supplier. Maps to TOML field @geography_policy@.

* 'GeoExact'  — only accept candidates with the exact same location code.
* 'GeoParent' — also accept any ancestor that names a real region in
                @locationHierarchy@ (e.g. @Europe@, @RER@). Excludes @GLO@,
                @RoW@, @Unspecified@ and unrelated locations.
* 'GeoGlobal' — accept any candidate the linker can match (current behaviour).
-}
data GeographyPolicy
    = GeoExact
    | GeoParent
    | GeoGlobal
    deriving (Show, Eq, Generic, NFData, Store)

{- | Classification of how a candidate's location relates to the requested one.
Produced by 'Database.CrossLinking.acceptableLocation' and surfaced alongside
fallback warnings so the UI can distinguish gentle widening (parent region)
from hard widening (global / unrelated).
-}
data LocationKind
    = -- | Exact code match
      ExactLoc
    | -- | Ancestor region in the hierarchy (e.g. FR → Europe / RER)
      ParentLoc
    | -- | @GLO@, @RoW@ or @Unspecified@
      GlobalLoc
    | -- | Different but not in the hierarchy (e.g. SimaPro "Mixed data")
      UnrelatedLoc
    deriving (Show, Eq, Enum, Bounded, Generic, NFData, Store)

{- | Stable lowercase wire code for a 'LocationKind'. Single source of truth
shared by the JSON encoder and the human-readable rejection reason, so the UI
never sees raw Haskell constructor names like @"ParentLoc"@.
-}
locationKindCode :: LocationKind -> Text
locationKindCode ExactLoc = "exact"
locationKindCode ParentLoc = "parent"
locationKindCode GlobalLoc = "global"
locationKindCode UnrelatedLoc = "unrelated"

{- | Lowercase-wire-code ToJSON for 'LocationKind'. Stays in lock-step with
'locationKindCode' so the JSON output and the rejection-reason text can
never drift apart.
-}
instance ToJSON LocationKind where
    toJSON = toJSON . locationKindCode

-- | How a 'LinkBlocker' is spelled on the wire: a stable code, and what it was about.
data BlockerReason = BlockerReason
    { brReason :: !Text
    , brDetail :: !(Maybe Text)
    }

{- | The wire spelling of a 'LinkBlocker' — single source of truth shared by the
setup page's missing-supplier list and the supplier-gap report, so the two
surfaces can never name the same blocker differently.
-}
blockerReason :: LinkBlocker -> BlockerReason
blockerReason blocker = case blocker of
    NoNameMatch -> BlockerReason "no_name_match" Nothing
    UnitIncompatible{uiQueryUnit = q, uiSupplierUnit = s} ->
        BlockerReason "unit_incompatible" (Just (q <> " vs " <> s))
    LocationUnavailable loc -> BlockerReason "location_unavailable" (Just loc)
    LocationRejectedByPolicy{lrRequested = req, lrBestCandidate = act, lrBestKind = kind} ->
        BlockerReason "location_rejected" (Just (req <> " ↛ " <> act <> " (" <> locationKindCode kind <> ")"))
    AliasTargetMissing name mLoc ->
        BlockerReason "alias_target_missing" (Just (name <> maybe "" (" @ " <>) mLoc))

instance FromJSON LocationKind where
    parseJSON v = do
        s <- parseJSON v
        maybe (fail $ "Invalid LocationKind: " <> T.unpack s) pure (codedBy locationKindCode s)

{- | The value a wire code names, read off the code function itself so a decoder
cannot fall behind the encoder. Inverse of the @*Code@ functions.
-}
codedBy :: (Enum a, Bounded a) => (a -> Text) -> Text -> Maybe a
codedBy code s = find ((== s) . code) [minBound .. maxBound]

{- | OpenAPI schema for an enum spelled on the wire as lowercase codes. The
generic schema would expose the raw Haskell constructor names; taking the codes
from the same function the encoder uses keeps the spec in sync with the ToJSON.
-}
codeSchema :: (Enum a, Bounded a) => Text -> (a -> Text) -> NamedSchema
codeSchema name code =
    NamedSchema (Just name) $
        mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ map (toJSON . code) [minBound .. maxBound]

instance ToSchema LocationKind where
    declareNamedSchema _ = pure (codeSchema "LocationKind" locationKindCode)

{- | How serious a dataset-soundness finding is. Declaration order is the
severity order, so 'Ord' sorts the worst findings first.

The constructors carry a @Sev@ suffix for the same reason 'LocationKind' has
@Loc@: this module is re-exported wholesale, and bare @Warning@ / @Info@ would
collide with 'Progress.ProgressLevel' wherever both are imported.
-}
data Severity
    = -- | Breaks scoring or a faithful round-trip
      DangerSev
    | -- | Suspicious: legal data, but likely a mistake
      WarningSev
    | -- | Incomplete rather than wrong
      InfoSev
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

{- | Stable lowercase wire code for a 'Severity'. Single source of truth shared
by the JSON encoder and the schema, so consumers never see raw Haskell
constructor names like @"DangerSev"@.
-}
severityCode :: Severity -> Text
severityCode DangerSev = "danger"
severityCode WarningSev = "warning"
severityCode InfoSev = "info"

instance ToJSON Severity where
    toJSON = toJSON . severityCode

instance FromJSON Severity where
    parseJSON v = do
        s <- parseJSON v
        maybe (fail $ "Invalid Severity: " <> T.unpack s) pure (codedBy severityCode s)

instance ToSchema Severity where
    declareNamedSchema _ = pure (codeSchema "Severity" severityCode)

-- | A product whose supplier was found at a wider geography than requested.
data LocationFallback = LocationFallback
    { lfProduct :: !Text
    , lfRequested :: !Text
    , lfActual :: !Text
    , lfKind :: !LocationKind
    }
    deriving (Show, Eq, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped LocationFallback)

{- | A product whose supplier could not be linked — either because no candidate
matched the name/unit, or because every geographic candidate was rejected by
the database's 'GeographyPolicy'.
-}
data LocationUnresolved = LocationUnresolved
    { luProduct :: !Text
    , luRequested :: !Text
    , luReason :: !Text
    }
    deriving (Show, Eq, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped LocationUnresolved)

{- | An input that named a specific source supplier by @activityLinkId@ which no
loaded dependency provides, so it was resolved by attribute matching (name /
location / unit) instead of exact identity.

This is the cross-version signal: a partial EcoSpold2 import carries the
activity UUIDs of the exact ecoinvent release it was cut from. Linking it
against a *different* release leaves those UUIDs unmatched, and the attribute
matcher stitches an approximate supplier in their place. Surfacing these lets a
consumer verify the dependency is the intended release rather than trust an
approximate match as exact. Distinct from nil-link inputs (SimaPro), which never
had a source identity and whose attribute match is expected, not a caveat.
-}
data AttributeFallback = AttributeFallback
    { afProduct :: !Text
    -- ^ Consumer-side product name that was matched
    , afRequested :: !Text
    -- ^ Location requested by the consumer input
    , afMatched :: !Text
    -- ^ Location of the supplier actually linked
    , afSourceDatabase :: !Text
    -- ^ Dependency that supplied the attribute match
    }
    deriving (Show, Eq, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped AttributeFallback)

{- | One input several activities of the same dependency answer equally well.

Their scores tie, so the ranking's winner is a choice nothing in the data made:
in a released background database, @electricity, medium voltage@ in GLO is the reference
product of 27 activities, and most of them are cut-off co-outputs carrying no
burden — linking to one of those rather than the market group is off by orders
of magnitude, in the quiet direction. Reported so the consumer can name the
supplier it meant, by the activity name its source states or a relink mapping.
-}
data SupplierAmbiguity = SupplierAmbiguity
    { saProduct :: !Text
    -- ^ Consumer-side product name that was matched
    , saRequested :: !Text
    -- ^ Location requested by the consumer input
    , saChosen :: !Text
    -- ^ Activity the ranking returned
    , saCandidates :: !Int
    -- ^ How many activities of that database tied for it
    , saSourceDatabase :: !Text
    -- ^ Dependency the tie is inside
    }
    deriving (Show, Eq, Generic, NFData, Store)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SupplierAmbiguity)

{- | One product no dependency supplies: how many activities asked for it,
and what stopped the first of them.

The single blocker is a known gap, not a decision: merging two linking runs
sums the demands and keeps the first blocker, so a product blocked for two
different reasons is counted under both and named after one. Widening it to a
count per blocker changes what the setup page and the load log display, so it
is issue #391 rather than a field of this record.
-}
data UnresolvedProduct = UnresolvedProduct
    { upDemands :: !Int
    -- ^ Activities that asked for this product
    , upBlocker :: !LinkBlocker
    -- ^ What stopped the first of them
    }
    deriving (Show, Eq, Generic, NFData, Store)

{- | Statistics from cross-database linking
Only essential state is stored; counts are derived via accessor functions.
-}
data CrossDBLinkingStats = CrossDBLinkingStats
    { cdlLinks :: ![CrossDBLink]
    -- ^ Resolved cross-DB links (technosphere + waste)
    , cdlUnresolvedProducts :: !(M.Map Text UnresolvedProduct)
    -- ^ Product name -> the demands it left unsupplied
    , cdlUnknownUnits :: !(S.Set Text)
    -- ^ Unknown units from sdbUnits
    , cdlLocationFallbacks :: ![LocationFallback]
    -- ^ Accepted links with widened geography, tagged with 'LocationKind'
    , cdlLocationUnresolved :: ![LocationUnresolved]
    -- ^ Inputs rejected by policy or with no candidate
    , cdlAttributeFallbacks :: ![AttributeFallback]
    -- ^ Source-identity inputs matched by attributes instead (cross-version)
    , cdlSupplierAmbiguities :: ![SupplierAmbiguity]
    -- ^ Inputs several activities of one dependency answered equally well
    , cdlTotalInputs :: !Int
    -- ^ Total technosphere inputs at time of linking
    , cdlWasteExactLinks :: !Int
    -- ^ Orphan waste exchanges resolved by exact UUID / canonical-name match
    , cdlWasteAmbiguous :: !Int
    -- ^ Orphan waste exchanges with matches in 2+ databases (stayed orphan)
    , cdlCutoffWasteCount :: !Int
    -- ^ Orphan waste exchanges with no match in any DB (true cut-offs)
    }
    deriving (Generic, NFData, Store)

{- | Field-wise '<>'. On unresolved-product collision counts are summed
and the first 'LinkBlocker' wins (tiebreaker). Hand-written: bare 'Int'
has no canonical 'Monoid', and 'UnresolvedProduct' is not one either.
-}
instance Semigroup CrossDBLinkingStats where
    s1 <> s2 =
        CrossDBLinkingStats
            { cdlLinks = cdlLinks s1 <> cdlLinks s2
            , cdlUnresolvedProducts = M.unionWith mergeUnresolved (cdlUnresolvedProducts s1) (cdlUnresolvedProducts s2)
            , cdlUnknownUnits = cdlUnknownUnits s1 <> cdlUnknownUnits s2
            , cdlLocationFallbacks = cdlLocationFallbacks s1 <> cdlLocationFallbacks s2
            , cdlLocationUnresolved = cdlLocationUnresolved s1 <> cdlLocationUnresolved s2
            , cdlAttributeFallbacks = cdlAttributeFallbacks s1 <> cdlAttributeFallbacks s2
            , cdlSupplierAmbiguities = cdlSupplierAmbiguities s1 <> cdlSupplierAmbiguities s2
            , cdlTotalInputs = cdlTotalInputs s1 + cdlTotalInputs s2
            , cdlWasteExactLinks = cdlWasteExactLinks s1 + cdlWasteExactLinks s2
            , cdlWasteAmbiguous = cdlWasteAmbiguous s1 + cdlWasteAmbiguous s2
            , cdlCutoffWasteCount = cdlCutoffWasteCount s1 + cdlCutoffWasteCount s2
            }
      where
        mergeUnresolved u1 u2 = u1{upDemands = upDemands u1 + upDemands u2}

instance Monoid CrossDBLinkingStats where
    mempty =
        CrossDBLinkingStats
            { cdlLinks = []
            , cdlUnresolvedProducts = M.empty
            , cdlUnknownUnits = S.empty
            , cdlLocationFallbacks = []
            , cdlLocationUnresolved = []
            , cdlAttributeFallbacks = []
            , cdlSupplierAmbiguities = []
            , cdlTotalInputs = 0
            , cdlWasteExactLinks = 0
            , cdlWasteAmbiguous = 0
            , cdlCutoffWasteCount = 0
            }

{- | One row per key, the first one seen, ordered by the key.

A report list holds one row per occurrence, and the same case is met once per
input that runs into it: a reader wants the case, not the tally. Which of the
rows survives has to be said rather than left to the argument order of
'M.fromListWith', and the key order is what groups a product's rows together on
the page that shows them.
-}
oncePerKey :: (Ord k) => (a -> k) -> [a] -> [a]
oncePerKey key = sortOn key . nubOrdOn key

-- | One location fallback per (product, requested location).
deduplicateFallbacks :: [LocationFallback] -> [LocationFallback]
deduplicateFallbacks = oncePerKey (\f -> (lfProduct f, lfRequested f))

-- | One unresolved entry per (product, requested location).
deduplicateUnresolved :: [LocationUnresolved] -> [LocationUnresolved]
deduplicateUnresolved = oncePerKey (\u -> (luProduct u, luRequested u))

-- | One attribute fallback per (product, requested location).
deduplicateAttributeFallbacks :: [AttributeFallback] -> [AttributeFallback]
deduplicateAttributeFallbacks = oncePerKey (\a -> (afProduct a, afRequested a))

-- | One supplier ambiguity per (product, requested location).
deduplicateSupplierAmbiguities :: [SupplierAmbiguity] -> [SupplierAmbiguity]
deduplicateSupplierAmbiguities = oncePerKey (\a -> (saProduct a, saRequested a))

-- | Number of resolved cross-DB links
crossDBLinksCount :: CrossDBLinkingStats -> Int
crossDBLinksCount = length . cdlLinks

-- | Number of unresolved inputs
unresolvedCount :: CrossDBLinkingStats -> Int
unresolvedCount = sum . map upDemands . M.elems . cdlUnresolvedProducts

-- | Cross-DB links grouped by source database
crossDBBySource :: CrossDBLinkingStats -> M.Map Text Int
crossDBBySource = M.fromListWith (+) . map (\l -> (cdlSourceDatabase l, 1)) . cdlLinks

{- | Minimal set of source databases needed to preserve every link's best
supplier choice. A DB is included iff at least one link cannot be supplied
by any already-included DB at the same score (its tied set is disjoint
from the running selection). Ties between equally-valid DBs are broken
alphabetically for determinism.

This is the canonical pre-selection rule: any DB that wins links only
"by tie-break" against an already-needed DB is dropped as redundant.
-}
computeMinimalSelectedDeps :: [CrossDBLink] -> [Text]
computeMinimalSelectedDeps links =
    let tiedSets = [S.insert (cdlSourceDatabase l) (S.fromList (cdlTiedAlternatives l)) | l <- links]
        essential = S.unions [s | s <- tiedSets, S.size s == 1]
        uncovered = filter (S.null . S.intersection essential) tiedSets
     in S.toAscList (greedyCover essential uncovered)
  where
    greedyCover :: S.Set Text -> [S.Set Text] -> S.Set Text
    greedyCover covered [] = covered
    greedyCover covered uncov =
        let pick = S.findMin (S.unions uncov)
            covered' = S.insert pick covered
            uncov' = filter (S.notMember pick) uncov
         in greedyCover covered' uncov'

{- | Databases that contributed at least one resolved link but are redundant
under 'computeMinimalSelectedDeps'. Useful to surface in the setup UI as
"available but not needed".
-}
crossDBRedundantSources :: [CrossDBLink] -> [Text] -> [Text]
crossDBRedundantSources links selected =
    let winners = S.fromList (map cdlSourceDatabase links)
     in S.toAscList (winners `S.difference` S.fromList selected)

{- | Cross-database link: records that an exchange in this database
sources from a supplier in another database.

At solve time, these links are used to compute demand for the
dependency database and chain the inventory calculations.

Note: We store both consumer and supplier as UUIDs for flexibility.
Consumer UUIDs are resolved to ProcessIds at solve time via dbProcessIdLookup.
Supplier UUIDs are looked up across loaded databases at solve time.
-}
data CrossDBLink = CrossDBLink
    { cdlConsumerActUUID :: !UUID
    -- ^ Consumer activity UUID (in this database)
    , cdlConsumerProdUUID :: !UUID
    -- ^ Consumer product UUID (in this database)
    , cdlConsumerFlowId :: !UUID
    {- ^ Consumer-side flow UUID that this link resolves. For technosphere
    inputs this is the tech-flow UUID; for orphan waste outputs it is the
    waste-flow UUID. Used as the keying discriminator on the API surface so
    a tech "X" link and a waste "X" link on the same activity cannot
    collide. 'UUID.nil' for synthetic substitution links built by
    'mkVirtualLink', which never enter 'dbCrossDBLinks'.
    -}
    , cdlSupplierActUUID :: !UUID
    -- ^ Supplier activity UUID (in another database)
    , cdlSupplierProdUUID :: !UUID
    -- ^ Supplier product UUID (in another database)
    , cdlCoefficient :: !Double
    -- ^ Amount consumed per unit output of consumer
    , cdlExchangeUnit :: !Text
    -- ^ Consumer's exchange unit (converted to supplier refUnit at matrix-build time)
    , cdlFlowName :: !Text
    -- ^ Product name (for display/debugging)
    , cdlLocation :: !Text
    -- ^ Supplier location (for display)
    , cdlSourceDatabase :: !Text
    -- ^ Source database name
    , cdlTiedAlternatives :: ![Text]
    {- ^ Other source databases whose best candidate matched the winner's score.
    A non-empty list means this link could equivalently be supplied from
    another database — used to compute the minimal dependency pre-selection.
    -}
    }
    deriving (Generic, NFData, Store, Show, Eq, Ord)

-- ToJSON/FromJSON for the records above are produced via `deriving via (Stripped X)`
-- attached to each `data` declaration. The two enums TechRole and BioDirection use
-- the default Generic encoding (constructor name as JSON string).
instance ToJSON TechRole
instance FromJSON TechRole

instance ToJSON WasteRole
instance FromJSON WasteRole

instance ToJSON BioDirection
instance FromJSON BioDirection

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for LCIA characterization methods.
--
-- LCIA methods define how to convert inventory results (LCI) into
-- impact assessment scores by applying characterization factors (CFs)
-- to biosphere flows.
module Method.Types
    ( -- * Method
      Method(..)
    , MethodCF(..)
    , FlowDirection(..)
    , Compartment(..)
      -- * Method Collection (with normalization/weighting)
    , MethodCollection(..)
    , DamageCategory(..)
    , NormWeightSet(..)
    , emptyMethodCollection
      -- * Compartment Mapping
    , CompartmentMap
    , buildCompartmentMapFromCSV
    , normalizeCompartment
    , compartmentMapSize
      -- * Flow Mapping
    , FlowMapping(..)
    , MatchType(..)
    ) where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader(..), decode)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Store (Store)

-- | Direction of a biosphere flow (input from or output to environment)
data FlowDirection
    = Input   -- ^ Resource from environment (e.g., water, minerals)
    | Output  -- ^ Emission to environment (e.g., CO2, pollutants)
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Compartment triple: (medium, subcompartment, qualifier)
-- medium: "air", "water", "soil", "natural resource"
-- subcompartment: "non-urban air or from high stacks", "surface water", etc.
-- qualifier: "long-term" or ""
data Compartment = Compartment !Text !Text !Text
    deriving (Eq, Show, Generic, NFData, Store, ToJSON, FromJSON)

-- | A characterization factor from a method file
--
-- Each CF defines how much impact a unit of a specific flow contributes
-- to the impact category.
data MethodCF = MethodCF
    { mcfFlowRef     :: !UUID                -- ^ ILCD flow UUID from method file
    , mcfFlowName    :: !Text                -- ^ Flow name (for matching & display)
    , mcfDirection   :: !FlowDirection       -- ^ Input (resource) or Output (emission)
    , mcfValue       :: !Double              -- ^ Characterization factor value
    , mcfCompartment :: !(Maybe Compartment) -- ^ Compartment from ILCD flow XML
    , mcfCAS         :: !(Maybe Text)        -- ^ CAS number (normalized, no leading zeros)
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | An LCIA characterization method (loaded from ILCD XML)
--
-- Methods contain a list of characterization factors that convert
-- inventory flows into impact scores.
data Method = Method
    { methodId          :: !UUID        -- ^ Method UUID
    , methodName        :: !Text        -- ^ Human-readable name
    , methodDescription :: !(Maybe Text) -- ^ Optional description
    , methodUnit        :: !Text        -- ^ Reference unit (e.g., "kg CO2 eq")
    , methodCategory    :: !Text        -- ^ Impact category (e.g., "Climate change")
    , methodMethodology :: !(Maybe Text) -- ^ Methodology (e.g., "Environmental Footprint")
    , methodFactors     :: ![MethodCF]  -- ^ List of characterization factors
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Damage category: groups impact subcategories into a parent category.
-- E.g., "Ecotoxicity, freshwater" groups "...part 1", "...part 2", etc.
-- Each impact maps with a factor (usually 1.0).
data DamageCategory = DamageCategory
    { dcName    :: !Text              -- ^ Damage category name
    , dcUnit    :: !Text              -- ^ Unit (e.g., "CTUe")
    , dcImpacts :: ![(Text, Double)]  -- ^ [(subcategory name, aggregation factor)]
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Normalization and weighting factor set.
-- Normalization converts raw scores to person-equivalents.
-- Weighting assigns relative importance for single-score aggregation.
data NormWeightSet = NormWeightSet
    { nwName          :: !Text                -- ^ Set name
    , nwNormalization :: !(M.Map Text Double)  -- ^ Damage category → normalization factor
    , nwWeighting     :: !(M.Map Text Double)  -- ^ Damage category → weight
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | A method collection with optional damage categories and NW sets.
data MethodCollection = MethodCollection
    { mcMethods          :: ![Method]
    , mcDamageCategories :: ![DamageCategory]
    , mcNormWeightSets   :: ![NormWeightSet]
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Empty method collection (for wrapping plain method lists).
emptyMethodCollection :: [Method] -> MethodCollection
emptyMethodCollection ms = MethodCollection ms [] []

-- | Compartment normalization map.
-- Maps (lowercase source_medium, source_sub, source_qualifier) to target Compartment.
type CompartmentMap = M.Map (Text, Text, Text) Compartment

-- | Build a CompartmentMap from CSV content.
-- CSV columns: source_medium, source_sub, source_qualifier, target_medium, target_sub, target_qualifier
buildCompartmentMapFromCSV :: BL.ByteString -> Either String CompartmentMap
buildCompartmentMapFromCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> err
        Right rows ->
            let entries = V.toList (rows :: V.Vector (Text, Text, Text, Text, Text, Text))
                pairs = [(( T.toLower (T.strip sm)
                          , T.toLower (T.strip ss)
                          , T.toLower (T.strip sq))
                         , Compartment (T.strip tm) (T.strip ts) (T.strip tq))
                        | (sm, ss, sq, tm, ts, tq) <- entries]
            in Right $ M.fromList pairs

-- | Normalize a compartment using the mapping. Returns original if not found.
normalizeCompartment :: CompartmentMap -> Compartment -> Compartment
normalizeCompartment cmap (Compartment med sub qual) =
    let key = (T.toLower med, T.toLower sub, T.toLower qual)
    in M.findWithDefault (Compartment med sub qual) key cmap

-- | Number of entries in the compartment map.
compartmentMapSize :: CompartmentMap -> Int
compartmentMapSize = M.size

-- | How a method flow was matched to a database flow
data MatchType
    = ExactUUID           -- ^ Same UUID
    | CASMatch            -- ^ Via CAS number
    | ExactName           -- ^ Same normalized name
    | SynonymMatch !Int   -- ^ Via synonym group ID
    | FuzzyMatch !Double  -- ^ Fuzzy similarity score (0-1)
    | Unmatched           -- ^ No match found
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Mapping between a method flow and a database flow
--
-- Used to track how method CFs are linked to the actual flows
-- in the database being analyzed.
data FlowMapping = FlowMapping
    { fmMethodFlowRef  :: !UUID         -- ^ Flow UUID from method file
    , fmMethodFlowName :: !Text         -- ^ Flow name in method
    , fmDbFlowId       :: !(Maybe UUID) -- ^ Matched database flow (if found)
    , fmMatchType      :: !MatchType    -- ^ How the match was determined
    , fmConfidence     :: !Double       -- ^ Match confidence (0.0-1.0)
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

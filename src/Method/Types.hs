{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

{- | Types for LCIA characterization methods.

LCIA methods define how to convert inventory results (LCI) into
impact assessment scores by applying characterization factors (CFs)
to biosphere flows.
-}
module Method.Types (
    -- * Method
    Method (..),
    MethodCF (..),
    FlowDirection (..),
    Compartment (..),

    -- * Method Collection (with normalization/weighting)
    MethodCollection (..),
    DamageCategory (..),
    NormWeightSet (..),
    emptyMethodCollection,

    -- * Scoring sets (formula-based N/W)
    ScoringSet (..),
    ScoringEvaluation (..),
    computeFormulaScores,

    -- * Compartment Mapping
    CompartmentMap,
    buildCompartmentMapFromCSV,
    normalizeCompartment,
    compartmentMapSize,

    -- * Flow Mapping
    FlowMapping (..),
    MatchType (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (..), decode)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Maybe
import Data.Ord (comparing)
import Data.Store (Store)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Expr
import GHC.Generics (Generic)

-- | Direction of a biosphere flow (input from or output to environment)
data FlowDirection
    = -- | Resource from environment (e.g., water, minerals)
      Input
    | -- | Emission to environment (e.g., CO2, pollutants)
      Output
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Compartment triple: (medium, subcompartment, qualifier)
medium: "air", "water", "soil", "natural resource"
subcompartment: "non-urban air or from high stacks", "surface water", etc.
qualifier: "long-term" or ""
-}
data Compartment = Compartment !Text !Text !Text
    deriving (Eq, Show, Generic, NFData, Store, ToJSON, FromJSON)

{- | A characterization factor from a method file

Each CF defines how much impact a unit of a specific flow contributes
to the impact category.
-}
data MethodCF = MethodCF
    { mcfFlowRef :: !UUID
    -- ^ ILCD flow UUID from method file
    , mcfFlowName :: !Text
    -- ^ Flow name (for matching & display)
    , mcfDirection :: !FlowDirection
    -- ^ Input (resource) or Output (emission)
    , mcfValue :: !Double
    -- ^ Characterization factor value
    , mcfCompartment :: !(Maybe Compartment)
    -- ^ Compartment from ILCD flow XML
    , mcfCAS :: !(Maybe Text)
    -- ^ CAS number (normalized, no leading zeros)
    , mcfUnit :: !Text
    -- ^ CF reference unit (e.g., "kg", "kBq")
    , mcfConsumerLocation :: !(Maybe Text)
    -- ^ Consumer location for regionalized CFs (ISO 2-3 letter code).
    -- 'Nothing' = universal CF (broadcast on all locations).
    -- 'Just loc' = single cell of the C matrix at (flow, loc).
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | An LCIA characterization method (loaded from ILCD XML)

Methods contain a list of characterization factors that convert
inventory flows into impact scores.
-}
data Method = Method
    { methodId :: !UUID
    -- ^ Method UUID
    , methodName :: !Text
    -- ^ Human-readable name
    , methodDescription :: !(Maybe Text)
    -- ^ Optional description
    , methodUnit :: !Text
    -- ^ Reference unit (e.g., "kg CO2 eq")
    , methodCategory :: !Text
    -- ^ Impact category (e.g., "Climate change")
    , methodMethodology :: !(Maybe Text)
    -- ^ Methodology (e.g., "Environmental Footprint")
    , methodFactors :: ![MethodCF]
    -- ^ List of characterization factors
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Damage category: groups impact subcategories into a parent category.
E.g., "Ecotoxicity, freshwater" groups "...part 1", "...part 2", etc.
Each impact maps with a factor (usually 1.0).
-}
data DamageCategory = DamageCategory
    { dcName :: !Text
    -- ^ Damage category name
    , dcUnit :: !Text
    -- ^ Unit (e.g., "CTUe")
    , dcImpacts :: ![(Text, Double)]
    -- ^ [(subcategory name, aggregation factor)]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Normalization and weighting factor set.
Normalization: score is divided by the reference-per-person value to get person-equivalents.
Weighting assigns relative importance for single-score aggregation.
-}
data NormWeightSet = NormWeightSet
    { nwName :: !Text
    -- ^ Set name
    , nwNormalization :: !(M.Map Text Double)
    -- ^ Damage category → reference per person value (divisor)
    , nwWeighting :: !(M.Map Text Double)
    -- ^ Damage category → weight
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | A method collection with optional damage categories and NW sets.
data MethodCollection = MethodCollection
    { mcMethods :: ![Method]
    , mcDamageCategories :: ![DamageCategory]
    , mcNormWeightSets :: ![NormWeightSet]
    , mcScoringSets :: ![ScoringSet]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Empty method collection (for wrapping plain method lists).
emptyMethodCollection :: [Method] -> MethodCollection
emptyMethodCollection ms = MethodCollection ms [] [] []

{- | Formula-based scoring set.
Variables map short names to impact category names.
Computed variables are formulas over other variables (evaluated via Expr).
Normalization divides, weighting multiplies.
Scores are named output formulas over the normalized/weighted environment.
-}
data ScoringSet = ScoringSet
    { ssName :: !Text
    -- ^ Display name
    , ssUnit :: !Text
    -- ^ Display unit (e.g., "Pts")
    , ssVariables :: !(M.Map Text Text)
    -- ^ var → impact category name
    , ssComputed :: !(M.Map Text Text)
    -- ^ var → formula (e.g., "2 * etfo + etfi")
    , ssNormalization :: !(M.Map Text Double)
    -- ^ var → normalization factor (divisor)
    , ssWeighting :: !(M.Map Text Double)
    -- ^ var → weight (multiplier)
    , ssScores :: !(M.Map Text Text)
    -- ^ score name → formula
    , ssDisplayMultiplier :: !(Maybe Double)
    {- ^ Multiplier applied to nwEnv values and final scores for display
    (e.g., 1e6 to convert "Pts" into "µPts"). Nothing ≡ 1.0.
    -}
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Result of evaluating a ScoringSet against raw LCIA results.
Both maps carry the same numeric scale — the display multiplier has been applied.
-}
data ScoringEvaluation = ScoringEvaluation
    { seNwEnv :: !(M.Map Text Double)
    -- ^ var → normalized-weighted value (× displayMultiplier)
    , seScores :: !(M.Map Text Double)
    -- ^ score name → formula output (× displayMultiplier)
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Evaluate all scores in a ScoringSet given raw LCIA results.
Input: map from impact category name → raw score.
Output: normalized-weighted per-variable map plus per-score-formula output, or an error.
Both output maps are pre-multiplied by `ssDisplayMultiplier` (default 1.0).
-}
computeFormulaScores :: ScoringSet -> M.Map Text Double -> Either String ScoringEvaluation
computeFormulaScores ss rawScores = do
    -- 1. Resolve primitive variables: lookup raw score by category name
    let primitiveEnv = M.mapMaybe (`M.lookup` rawScores) (ssVariables ss)
    -- 2. Resolve computed variables in topological order
    computedEnv <- resolveComputed primitiveEnv (ssComputed ss)
    let rawEnv = M.union computedEnv primitiveEnv
        -- 3. Apply normalization and weighting: nw(v) = raw(v) / norm(v) * weight(v)
        nwEnv =
            M.mapWithKey
                ( \v raw ->
                    let n = M.findWithDefault 1.0 v (ssNormalization ss)
                        w = M.findWithDefault 0.0 v (ssWeighting ss)
                     in raw / n * w
                )
                rawEnv
        mul = Data.Maybe.fromMaybe 1.0 (ssDisplayMultiplier ss)
    -- 4. Evaluate each score formula in the nw environment
    scores <-
        M.traverseWithKey
            ( \scoreName formula ->
                case Expr.evaluate nwEnv formula of
                    Left err ->
                        Left $
                            "Score '"
                                <> T.unpack scoreName
                                <> "': "
                                <> err
                    Right val -> Right val
            )
            (ssScores ss)
    pure
        ScoringEvaluation
            { seNwEnv = M.map (* mul) nwEnv
            , seScores = M.map (* mul) scores
            }

{- | Resolve computed variables by evaluating formulas.
Uses topological sort to handle dependencies between computed variables.
-}
resolveComputed :: M.Map Text Double -> M.Map Text Text -> Either String (M.Map Text Double)
resolveComputed env formulas = foldl step (Right env) sorted
  where
    -- Simple topological sort: evaluate in order of formula length as heuristic
    -- (shorter formulas are less likely to depend on longer ones)
    sorted = sortBy (comparing (T.length . snd)) (M.toList formulas)
    step (Left err) _ = Left err
    step (Right currentEnv) (varName, formula) =
        case Expr.evaluate currentEnv formula of
            Left err ->
                Left $
                    "Computed variable '"
                        <> T.unpack varName
                        <> "': "
                        <> err
            Right val -> Right $ M.insert varName val currentEnv

{- | Compartment normalization map.
Maps (lowercase source_medium, source_sub, source_qualifier) to target Compartment.
-}
type CompartmentMap = M.Map (Text, Text, Text) Compartment

{- | Build a CompartmentMap from CSV content.
CSV columns: source_medium, source_sub, source_qualifier, target_medium, target_sub, target_qualifier
-}
buildCompartmentMapFromCSV :: BL.ByteString -> Either String CompartmentMap
buildCompartmentMapFromCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> err
        Right rows ->
            let entries = V.toList (rows :: V.Vector (Text, Text, Text, Text, Text, Text))
                pairs =
                    [ (
                          ( T.toLower (T.strip sm)
                          , T.toLower (T.strip ss)
                          , T.toLower (T.strip sq)
                          )
                      , Compartment (T.strip tm) (T.strip ts) (T.strip tq)
                      )
                    | (sm, ss, sq, tm, ts, tq) <- entries
                    ]
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
    = -- | Same UUID
      ExactUUID
    | -- | Via CAS number
      CASMatch
    | -- | Same normalized name
      ExactName
    | -- | Via synonym group ID
      SynonymMatch !Int
    | -- | Fuzzy similarity score (0-1)
      FuzzyMatch !Double
    | -- | No match found
      Unmatched
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Mapping between a method flow and a database flow

Used to track how method CFs are linked to the actual flows
in the database being analyzed.
-}
data FlowMapping = FlowMapping
    { fmMethodFlowRef :: !UUID
    -- ^ Flow UUID from method file
    , fmMethodFlowName :: !Text
    -- ^ Flow name in method
    , fmDbFlowId :: !(Maybe UUID)
    -- ^ Matched database flow (if found)
    , fmMatchType :: !MatchType
    -- ^ How the match was determined
    , fmConfidence :: !Double
    -- ^ Match confidence (0.0-1.0)
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

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
    Location (..),
    Subcompartment (..),

    -- * Method Collection (with normalization/weighting)
    MethodCollection (..),
    DamageCategory (..),
    NormWeightSet (..),

    -- * Scoring sets (formula-based N/W)
    ScoringSet (..),
    ScoringEvaluation (..),
    computeFormulaScores,

    -- * Compartment Mapping
    CompartmentMap (..),
    buildCompartmentMapFromCSV,
    normalizeCompartment,
    compartmentMapSize,

    -- * Energy density (mass/volume → energy for energy-denominated CFs)
    EnergyDensity (..),
    EnergyDensityMap,
    buildEnergyDensityMapFromCSV,
    energyDensityMapSize,
    parseEnergyDensitySuffix,
    isEnergyUnit,
    lookupEnergyDensity,

    -- * Region-suffixed flow names
    extractLocationSuffix,
) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Csv (HasHeader (..), decode)
import Data.Indexing (collisions, uniqueIndex)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Maybe
import Data.Store (Store)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Expr
import GHC.Generics (Generic)
import SynonymDB (normalizeName)
import Text.Read (readMaybe)
import Types (Medium, mediumText, parseMedium)

-- | Direction of a biosphere flow (input from or output to environment)
data FlowDirection
    = -- | Resource from environment (e.g., water, minerals)
      Input
    | -- | Emission to environment (e.g., CO2, pollutants)
      Output
    deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

{- | Compartment triple: (medium, subcompartment, qualifier)
medium: "air", "water", "soil", "natural resource"
subcompartment: "non-urban air or from high stacks", "surface water", etc.
qualifier: "long-term" or ""
-}
data Compartment = Compartment !Text !Text !Text
    deriving (Eq, Show, Generic, NFData, Store, ToJSON, FromJSON)

{- | A normalized compartment subcompartment (e.g. @"surface water"@,
@"(unspecified)"@). A lookup-key axis alongside the medium and a normalized
name; a newtype so it can't be swapped with either in a key tuple.
-}
newtype Subcompartment = Subcompartment Text
    deriving (Eq, Ord, Show, Generic, NFData)

{- | A location / geography code (@"FR"@, @"CN-SC"@, @"RER"@, @"GLO"@) as used
by regionalized CFs and the location hierarchy. A newtype so a location can't
be swapped with any other Text axis in a regionalized lookup key.
-}
newtype Location = Location Text
    deriving (Eq, Ord, Show)

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
    {- ^ Consumer location for regionalized CFs (ISO 2-3 letter code).
    'Nothing' = universal CF (broadcast on all locations).
    'Just loc' = single cell of the C matrix at (flow, loc).
    -}
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
    , ssLabels :: !(M.Map Text Text)
    {- ^ var → display label, overriding the breakdown name. Needed for
    computed variables, whose raw key would otherwise leak; on a primitive
    variable it deliberately overrides the 'ssVariables' category name.
    Keys are validated against declared variables when the config decodes.
    -}
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
Both maps carry the same numeric scale – the display multiplier has been applied.
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
                case Expr.evaluate Expr.Arithmetic nwEnv formula of
                    Left err ->
                        Left $
                            "Score '"
                                <> T.unpack scoreName
                                <> "': "
                                <> T.unpack (Expr.describeRefusal err)
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
    sorted = sortOn (T.length . snd) (M.toList formulas)
    step (Left err) _ = Left err
    step (Right currentEnv) (varName, formula) =
        case Expr.evaluate Expr.Arithmetic currentEnv formula of
            Left err ->
                Left $
                    "Computed variable '"
                        <> T.unpack varName
                        <> "': "
                        <> T.unpack (Expr.describeRefusal err)
            Right val -> Right $ M.insert varName val currentEnv

{- | What @compartments.csv@ says about compartments, in its two kinds of row.

A @same@ row names one place written another way – SimaPro's @low. pop.@ is
ecoinvent's @non-urban air or from high stacks@ – and rewrites both a flow and
a method line to the one spelling, so the two meet. It changes no meaning, so a
@same@ row never joins two places one vocabulary keeps apart.

An @if_absent@ row names two different places, written canonically: a flow at
the first reads the factor written at the second, but only for a method that
never writes the first. The EF 3.1 flow list has no forestry soil, so a flow
emitted to it reads the method's non-agricultural soil, while a method with its
own forestry factors keeps them. It rewrites no method line.
-}
data CompartmentMap = CompartmentMap
    { cmSpellings :: !(M.Map (Text, Text, Text) Compartment)
    -- ^ @same@ rows: (lowercase source medium, sub, qualifier) → target.
    , cmIfAbsent :: !(M.Map (Medium, Subcompartment) Subcompartment)
    -- ^ @if_absent@ rows: (medium, lowercase sub) → the sub read instead.
    }
    deriving (Eq, Show, Generic, NFData)

-- | First wins on either kind, as the merged tables have always been read.
instance Semigroup CompartmentMap where
    CompartmentMap a b <> CompartmentMap c d = CompartmentMap (M.union a c) (M.union b d)

instance Monoid CompartmentMap where
    mempty = CompartmentMap M.empty M.empty

-- | The two kinds of row a compartment table holds; see 'CompartmentMap'.
data RowKind = SameRow | IfAbsentRow

{- | Build a CompartmentMap from CSV content.
CSV columns: source_medium, source_sub, source_qualifier, target_medium,
target_sub, target_qualifier, and an optional seventh, kind (@same@, the
default, or @if_absent@).
-}
buildCompartmentMapFromCSV :: BL.ByteString -> Either String CompartmentMap
buildCompartmentMapFromCSV csvData = do
    rows <- first ("CSV parse error: " <>) (decode HasHeader csvData)
    parsed <- traverse parseRow (V.toList (rows :: V.Vector [Text]))
    spellings <- first repeatedCompartments (uniqueIndex [(k, c) | Left (k, c) <- parsed])
    ifAbsent <- first repeatedIfAbsent (uniqueIndex [(k, t) | Right (k, t) <- parsed])
    pure (CompartmentMap spellings ifAbsent)
  where
    parseRow :: [Text] -> Either String (Either ((Text, Text, Text), Compartment) ((Medium, Subcompartment), Subcompartment))
    parseRow cells = case map T.strip cells of
        [sm, ss, sq, tm, ts, tq] -> row SameRow sm ss sq tm ts tq
        [sm, ss, sq, tm, ts, tq, kind] -> kindOf kind >>= \k -> row k sm ss sq tm ts tq
        _ -> Left ("a row needs six or seven columns: " <> T.unpack (T.intercalate (T.pack ",") cells))

    kindOf :: Text -> Either String RowKind
    kindOf k = case T.unpack (T.toLower k) of
        "" -> Right SameRow
        "same" -> Right SameRow
        "if_absent" -> Right IfAbsentRow
        other -> Left ("unknown kind \"" <> other <> "\", expected same or if_absent")

    row kind sm ss sq tm ts tq = case kind of
        SameRow -> Right (Left ((T.toLower sm, T.toLower ss, T.toLower sq), Compartment tm ts tq))
        IfAbsentRow
            | not (T.null sq && T.null tq) -> refuse "takes no qualifier"
            | T.null ss || T.null ts -> refuse "names a subcompartment on both sides"
            | T.toLower sm /= T.toLower tm -> refuse "stays in one medium"
            | T.toLower ss == T.toLower ts -> refuse "names two different subcompartments"
            | otherwise -> case parseMedium sm of
                Left _ -> refuse "names a medium this reader knows"
                Right medium -> Right (Right ((medium, Subcompartment (T.toLower ss)), Subcompartment (T.toLower ts)))
      where
        refuse why = Left ("an if_absent row " <> why <> ": " <> T.unpack (T.intercalate (T.pack ",") [sm, ss, sq, tm, ts, tq]))

    -- Two rows for one source compartment give two targets, and the file says
    -- nothing about which was meant.
    repeatedCompartments :: NonEmpty (Text, Text, Text) -> String
    repeatedCompartments keys =
        "two rows normalize the same compartment: "
            <> T.unpack (T.intercalate (T.pack "; ") (map spelt (NE.toList keys)))

    repeatedIfAbsent :: NonEmpty (Medium, Subcompartment) -> String
    repeatedIfAbsent keys =
        "two if_absent rows redirect the same compartment: "
            <> T.unpack (T.intercalate (T.pack "; ") [mediumText m <> T.pack "," <> s | (m, Subcompartment s) <- NE.toList keys])

    spelt :: (Text, Text, Text) -> Text
    spelt (sm, ss, sq) = T.intercalate (T.pack ",") [sm, ss, sq]

{- | Normalize a compartment using the mapping.

Tries the full @(medium, sub, qualifier)@ key first. On miss, falls back to a
medium-only key – so a single CSV entry like @"Emissions to air,,,air,,"@
covers every @(emissions to air, *, *)@ variant by remapping just the medium
and preserving the original sub/qualifier. Returns the input unchanged if
neither key resolves; callers can use that as a "no rule for this
compartment" signal.
-}
normalizeCompartment :: CompartmentMap -> Compartment -> Compartment
normalizeCompartment cmap (Compartment med sub qual) =
    let lmed = T.toLower med
        lsub = T.toLower sub
        lqual = T.toLower qual
     in case M.lookup (lmed, lsub, lqual) (cmSpellings cmap) of
            Just c -> c
            Nothing -> case M.lookup (lmed, T.empty, T.empty) (cmSpellings cmap) of
                Just (Compartment med' _ _) -> Compartment med' sub qual
                Nothing -> Compartment med sub qual

-- | Number of entries in the compartment map.
compartmentMapSize :: CompartmentMap -> Int
compartmentMapSize cmap = M.size (cmSpellings cmap) + M.size (cmIfAbsent cmap)

{- | Physical content of a flow per native unit – a calorific value (MJ per kg
of coal) or a mass density (m³ per kg of water) – used to characterize a CF
denominated in that target unit against an inventory flow given in another
dimension (kg, Sm3, …).

A row @Coal, hard,18.01,MJ,kg@ reads: 1 kg of @Coal, hard@ carries 18.01 MJ;
@Water,0.001,m3,kg@ reads: 1 kg of water occupies 0.001 m³. 'edTargetUnit' is
the unit of the content amount (converted to the CF's unit at score time);
'edNativeUnit' is the unit the content is denominated against, so the inventory
quantity is brought into that unit before the density is applied. Naming the
native unit keeps the bridge dimensionally honest: a flow reported in a
different but compatible unit (g, t) still scores correctly, and one whose unit
can't be converted to the native unit scores 0 instead of by a wrong basis.
-}
data EnergyDensity = EnergyDensity
    { edValue :: !Double
    , edTargetUnit :: !Text
    , edNativeUnit :: !Text
    }
    deriving (Eq, Show, Generic)

instance NFData EnergyDensity

{- | Normalized flow name → energy density. Keyed by 'SynonymDB.normalizeName'
so the scoring read paths can look a flow up by the same canonical name they
already use for CF matching.
-}
type EnergyDensityMap = M.Map Text EnergyDensity

{- | Build an 'EnergyDensityMap' from CSV content.
CSV columns (with header): @flow_name, value, target_unit, native_unit@ – e.g.
@Coal, hard,18.01,MJ,kg@. Keys are normalized at parse time so the union of
active CSVs and the read-path lookups agree. Each row is validated: a
non-positive value or a missing unit is a load error, never a silently inert or
wrong-dimension entry.
-}
buildEnergyDensityMapFromCSV :: BL.ByteString -> Either String EnergyDensityMap
buildEnergyDensityMapFromCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> err
        Right rows -> do
            let entries = V.toList (rows :: V.Vector (Text, Double, Text, Text))
            table <- traverse toEntry entries
            case collisions [(normalizeName name, name) | (name, _, _, _) <- entries] of
                [] -> Right (M.fromList table)
                clashes -> Left (repeatedFlows clashes)
  where
    -- Two densities for one flow are two answers at score time, and the file
    -- has to give one row instead. The spellings are what the message names:
    -- the key is the normalized name, which lowercases and drops a unit
    -- suffix, so two rows can collide without looking alike.
    repeatedFlows :: [(Text, NonEmpty Text)] -> String
    repeatedFlows clashes =
        "two rows give a density for the same flow, where the table needs one: "
            <> T.unpack (T.intercalate (T.pack "; ") (map spelt clashes))

    spelt :: (Text, NonEmpty Text) -> Text
    spelt (key, names) = key <> T.pack " (" <> T.intercalate (T.pack ", ") (map quoted (NE.toList names)) <> T.pack ")"

    quoted :: Text -> Text
    quoted t = T.pack "\"" <> t <> T.pack "\""

    toEntry :: (Text, Double, Text, Text) -> Either String (Text, EnergyDensity)
    toEntry (name, value, targetUnit, nativeUnit)
        | value <= 0 =
            Left $ "density must be positive (flow: " <> T.unpack name <> ")"
        | T.null (T.strip targetUnit) || T.null (T.strip nativeUnit) =
            Left $ "density needs a target unit and a native unit (flow: " <> T.unpack name <> ")"
        | otherwise =
            Right (normalizeName name, EnergyDensity value (T.strip targetUnit) (T.strip nativeUnit))

-- | Number of entries in the energy-density map.
energyDensityMapSize :: EnergyDensityMap -> Int
energyDensityMapSize = M.size

{- | Parse a flow name that encodes an energy density as a suffix –
@"Coal, 18 MJ per kg"@, @"Gas, natural, 35 MJ per m3"@, @"Uranium, 2291 GJ per
kg"@ – into @(base substance name, density)@. The value and energy unit are
taken verbatim (@332 GJ@), leaving the GJ→MJ conversion to the unit machinery
downstream. The @per {unit}@ denominator becomes the density's native unit, so
the conversion can bring the flow's quantity into the unit the density is
denominated against (a flow in @g@ against a @per kg@ density still converts).

Only the joule family (kJ/MJ/GJ/TJ) counts as the energy token, so a name that
merely contains @" per "@ (e.g. @"Water, per capita"@) is not mistaken for a
density. The base keeps internal qualifiers ("Gas, natural") and drops only the
trailing separator.
-}
parseEnergyDensitySuffix :: Text -> Maybe (Text, EnergyDensity)
parseEnergyDensitySuffix name =
    case T.breakOn (T.pack " per ") name of
        (left, rest)
            | Just after <- T.stripPrefix (T.pack " per ") rest
            , (eunit : nTok : baseRev) <- reverse (T.words left)
            , isEnergyUnit eunit
            , Just n <- readMaybe (T.unpack nTok)
            , (nativeUnit : _) <- T.words after ->
                Just (cleanBase (reverse baseRev), EnergyDensity n eunit nativeUnit)
        _ -> Nothing
  where
    cleanBase ws = T.dropWhileEnd (\c -> c == ',' || c == ' ') (T.unwords ws)

{- | A unit of the joule family, spelled as a method line or a flow name spells
it. Deliberately literal: an expression a result is written in
(@"MJ, net calorific value"@, @"kg CO2 eq"@) is not one, which is what keeps a
column header from being read as the quantity its factors are written per.
-}
isEnergyUnit :: Text -> Bool
isEnergyUnit u = T.toUpper u `elem` map T.pack ["KJ", "MJ", "GJ", "TJ"]

{- | The density for a flow name, following the same cascade of names the CF
lookup follows: the curated map under the flow's own name, then under its
region-stripped base name (@"Water, SERC"@ → @"Water"@), then the density the
name itself encodes (@"Coal, 18 MJ per kg"@).

The region rung is what keeps the two lookups on the same key. A
region-suffixed flow is lent the base substance's CF (see
'extractLocationSuffix'), and that CF carries the base substance's unit – so if
the density is only ever looked up under the full name, the flow ends up
holding a factor of a dimension it cannot be converted to and nothing to bridge
with, and scores 0. Stripping here exactly as the CF lookup strips there is the
invariant: whatever name lent the factor must also lend the density.

Curated rows outrank the name-encoded density, so a database that spells a
density into its flow names cannot override a curated correction.
-}
lookupEnergyDensity :: EnergyDensityMap -> Text -> Maybe EnergyDensity
lookupEnergyDensity densities name =
    M.lookup (normalizeName name) densities
        <|> regionBase
        <|> fmap snd (parseEnergyDensitySuffix name)
  where
    regionBase = case extractLocationSuffix name of
        (base, Just _) -> M.lookup (normalizeName base) densities
        (_, Nothing) -> Nothing

{- | SimaPro encodes regional variants of a flow as a suffix on the flow name:
@"Nitrogen dioxide, FR"@. Split that suffix off, returning the base name and
the region code. Two layers use it: the CF parser, to index a CF by
@(flow, location)@ instead of an opaque concatenated name; and the read-side
score lookup, to fall a region-suffixed database flow back to the base
substance's CF when the method doesn't tag that region.

Heuristic: the trailing token must start with an uppercase ASCII letter,
contain only letters or hyphens, and be 2–6 characters long. This catches:

  * ISO-2 country codes: @FR@, @DE@, @AD@
  * Regional aggregates: @RER@, @GLO@
  * @RoW@ (rest of world; mixed case)
  * Sub-national codes: @FR-IDF@ (if a database adopts them)

But not @"change"@ (lowercase first), @"indoor"@, @"fossil"@, @"ion"@, which
are legitimate parts of compound flow names. If nothing matches, the original
name is returned unchanged with no location.
-}
extractLocationSuffix :: Text -> (Text, Maybe Text)
extractLocationSuffix name =
    case T.breakOnEnd (T.pack ", ") name of
        (prefixWithSep, candidate)
            | T.null prefixWithSep -> (name, Nothing) -- no ", " separator
            | isLocationCode candidate
            , let cleaned = T.dropEnd 2 prefixWithSep -- drop trailing ", "
            , not (T.null cleaned) ->
                (cleaned, Just candidate)
            | otherwise -> (name, Nothing)
  where
    isLocationCode t = case T.uncons t of
        Nothing -> False
        Just (firstC, rest) ->
            T.length t >= 2
                && T.length t <= 6
                && isAsciiUpper firstC
                && T.all (\c -> isAsciiUpper c || isAsciiLower c || c == '-') rest

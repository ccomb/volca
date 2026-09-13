{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Flow Mapping Engine

Maps characterization factor flows from LCIA methods to database flows
using a configurable cascade of MapperHandles (plugin architecture).
Default cascade: UUID → Name → Synonym → CAS.
-}
module Method.Mapping (
    -- * Mapping functions
    MapContext (..),
    mapMethodFlows,
    mapMethodToFlows,
    resolveCF,
    isPatternCF,
    isExclusionCF,
    expandPatternCF,
    dropExcludedMappings,
    exclusionWarning,
    compartmentGapWarning,
    buildMapContext,
    mapContextFor,

    -- * LCIA scoring
    CF (..),
    CFUnit (..),
    CFFamily (..),
    cfFamily,
    SeaWaterCFs (..),
    MethodTables (..),
    MethodIndex (..),
    LCIAOutcome (..),
    UncharacterizedFlow (..),
    SimilarCF (..),
    SimilarReason (..),
    UncharacterizedOpts (..),
    defaultUncharacterizedOpts,
    buildMethodTables,
    buildMethodIndex,
    fillBroadcastVector,
    zeroedMatchedCFs,
    fillRegionalActivityWeights,
    RegionalActivityWeights (..),
    computeLCIAScore,
    computeLCIAScoreFromTables,
    LongTermMode (..),
    longTermModeFromExclude,
    excludeLongTermFlows,
    applyLongTermMode,
    isLongTermFlow,
    computeLCIAScoreAuto,
    computeRegionalizedLCIAScore,
    sumRegionalizedLCIAScoreCrossDB,
    computeLCIAScoreWithDiagnostics,
    findUncharacterized,
    findSimilarCFs,
    inventoryContributions,
    processContributionsFromTables,
    lookupCFForFlow,
    lookupEntryForFlow,
    characterizedFlowIds,
    cascadeTrail,
    lookupCascadeEntry,
    RungId (..),
    RungOutcome (..),
    VetoReason (..),
    TableEntry (..),
    BuildProvenance (..),
    convertForCharacterization,
    characterizationOutcome,
    energyAwareOutcome,
    flowToCFOutcome,
    convertedQuantity,
    ConversionOutcome (..),
    FlowCFTag (..),
    UnitBridge (..),
    DensityDirection (..),
    RefusalReason (..),
    expandSynonymMappings,
    directionExcludedCFs,
    projectRegionalResourceFlows,
    ProxyTargets (..),
    expandProxyEdges,

    -- * Multi-method scoring
    MethodSetTables (..),
    MethodSetEntry (..),
    BatchedTables (..),
    buildMethodSetTables,
    computeLCIAScoreSetFromTables,

    -- * Matching strategies
    MatchStrategy (..),
    strategyToText,
    provenanceStrategyText,
    findFlowByUUID,
    findFlowByName,
    findFlowByNameComp,
    SynonymSearch (..),
    findFlowBySynonym,
    findFlowBySynonymComp,
    findFlowBySynonymMemo,
    findFlowByCAS,

    -- * Statistics
    MappingStats (..),
    computeMappingStats,
    entryPriority,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData)
import Control.Exception (evaluate)
import Control.Monad.ST (runST)
import Data.Aeson (ToJSON)
import Data.Either (lefts, rights)
import Data.List (find, partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import GHC.Generics (Generic)

import qualified Data.Set as Set
import Matrix (Inventory, Vector, applyBiosphereMatrix, chunksOf)
import Method.ChemSynonyms (ChemSynonyms, expandedTokens)
import Method.Types
import Progress (ProgressLevel (..), reportProgress)
import SubstanceRegistry (nonEmptyCAS, normalizeCAS)
import qualified SubstanceRegistry as SR
import SynonymDB
import Types (Activity (..), BioFlowDB, BiosphereFlow (..), Database (..), FlowClosure (..), Medium (..), ProcessId, SparseTriple (..), Unit (..), UnitDB, ownFlowClosure, parseMedium)
import qualified Types as VT
import UnitConversion (UnitConfig, convertOntoFactorBasis, convertUnit, isKnownUnit, normalizeToCanonical, unitKey)

-- | Matching strategy used to find a flow
data MatchStrategy
    = -- | Exact UUID match
      ByUUID
    | -- | CAS number match
      ByCAS
    | -- | Normalized name match
      ByName
    | -- | Via synonym group
      BySynonym
    | {- | Via a typed @ProxyFor@ edge: a CF borrowed from another flow, scaled
      by the edge's conversion factor. An approximation, ranked below every
      direct match so an explicit CF always wins.
      -}
      ByProxy
    deriving (Eq, Show)

{- | Per-strategy mapping counters. Forms a 'Monoid' (field-wise sum, all-zero
identity) so per-batch stats compose with '<>' and 'computeMappingStats'
is a single 'foldMap' pass over the mapping list.
-}
data MappingStats = MappingStats
    { msTotal :: !Int
    -- ^ Total CFs in method
    , msByUUID :: !Int
    -- ^ Matched by UUID
    , msByCAS :: !Int
    -- ^ Matched by CAS
    , msByName :: !Int
    -- ^ Matched by name
    , msBySynonym :: !Int
    -- ^ Matched by synonym
    , msByProxy :: !Int
    -- ^ Matched via a @ProxyFor@ edge
    , msUnmatched :: !Int
    -- ^ Not matched
    }
    deriving (Eq, Show)

instance Semigroup MappingStats where
    a <> b =
        MappingStats
            (msTotal a + msTotal b)
            (msByUUID a + msByUUID b)
            (msByCAS a + msByCAS b)
            (msByName a + msByName b)
            (msBySynonym a + msBySynonym b)
            (msByProxy a + msByProxy b)
            (msUnmatched a + msUnmatched b)

instance Monoid MappingStats where
    mempty = MappingStats 0 0 0 0 0 0 0

-- | Everything the CF matcher cascade needs, precomputed once per method.
data MapContext = MapContext
    { mcBioFlowsByUUID :: !BioFlowDB -- Biosphere flows by UUID (CF matching targets these)
    , mcBioFlowsByName :: !(M.Map Text [BiosphereFlow])
    , mcBioFlowsByCAS :: !(M.Map Text [BiosphereFlow])
    , mcSynonymDB :: !SynonymDB
    , mcActivities :: !(M.Map Text [Activity])
    , mcCompartmentMap :: !CompartmentMap
    {- ^ The table both sides of a compartment comparison go through, so the
    cascade reads a compartment the way the scoring tables do. Without it a row
    written "urban air" would not meet a flow filed under "air", subcompartment
    "urban": only the table relates a spelling that carries more than a medium.
    -}
    , mcSynGroupFlows :: !(M.Map (FlowDirection, Int) [BiosphereFlow])
    {- ^ Memoized @(direction, synonym-group id)@ → candidate flows, precomputed
    once per method by 'mapMethodFlows'. The synonym matcher resolves a CF whose
    name lands in a shared (often large) synonym group via a single lookup here,
    instead of re-expanding that group for every such CF. Keyed by direction
    because input-only and output-only bridges yield different groups. Empty when
    the cascade runs without the precompute; the matcher then falls back to
    expanding the group per call, so the result is unchanged either way.
    -}
    }

{- | Build a MapContext over the flows a database's characterization has to
reach, which is its dependencies' as much as its own – see 'FlowClosure'.
-}
mapContextFor :: FlowClosure -> SynonymDB -> CompartmentMap -> MapContext
mapContextFor closure synDB cmap =
    MapContext
        { mcBioFlowsByUUID = clByUUID closure
        , mcBioFlowsByName = clByName closure
        , mcBioFlowsByCAS = clByCAS closure
        , mcSynonymDB = synDB
        , mcActivities = M.empty
        , mcCompartmentMap = cmap
        , mcSynGroupFlows = M.empty
        }

{- | Build a MapContext from one Database alone. For callers holding no manager
and therefore no dependencies to close over – the CLI, and the tests.
-}
buildMapContext :: CompartmentMap -> Database -> MapContext
buildMapContext cmap db =
    mapContextFor (ownFlowClosure db) (fromMaybe emptySynonymDB (dbSynonymDB db)) cmap

{- | Map every method CF to a database biosphere flow via the built-in matcher
cascade ('resolveCF'). Each CF resolves independently (no cross-CF state), so
chunk the CFs across capabilities and resolve the chunks concurrently:
'mapConcurrently' preserves order, so the result – and every table built from it
– is identical to the serial 'mapM'; this is a pure speedup, not a behaviour
change. 'evaluate' forces each CF's resolution inside its own task so the lookups
run in parallel rather than as thunks the caller forces later. The chunking wins
on a cold single-method mapping; the method-set build already fans out across
methods, so there the inner parallelism mostly nests under that outer fan-out.
-}
mapMethodFlows ::
    MapContext ->
    Method ->
    IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mapMethodFlows ctx0 method = do
    caps <- getNumCapabilities
    let (exclusionCFs, plainCFs) = partition isExclusionCF (methodFactors method)
        (patternCFs, cfs) = partition isPatternCF plainCFs
        n = length cfs
        -- Precompute the synonym-group → flows memo for the groups this method's
        -- CFs reference (each group expanded once), then resolve every CF against
        -- it. Without it the synonym matcher re-expands a shared group – often a
        -- large closure class – once per CF whose name lands in it.
        ctx = ctx0{mcSynGroupFlows = buildSynGroupFlows ctx0 cfs}
        resolve cf = (cf,) <$> evaluate (resolveCF ctx cf)
    concrete <-
        if caps <= 1 || n < parCfThreshold
            then mapM resolve cfs
            else concat <$> mapConcurrently (mapM resolve) (chunksOf (max 1 ((n + caps - 1) `div` caps)) cfs)
    mapM_ (warn . pure) (mapMaybe (exclusionWarning (mcBioFlowsByUUID ctx0)) exclusionCFs)
    warn (maybeToList (compartmentGapWarning (mcCompartmentMap ctx0) (mcBioFlowsByName ctx0) concrete))
    expanded <- fmap concat . mapM (materialize exclusionCFs) $ patternCFs
    pure (concrete ++ expanded)
  where
    parCfThreshold = 1000
    warn = mapM_ (reportProgress Warning . T.unpack . (("[LCIA " <> methodName method <> "] ") <>))
    materialize exclusions cf = do
        let (rows, warnings) = expandPatternCF (mcBioFlowsByUUID ctx0) exclusions cf
        warn warnings
        pure rows

{- | Resolve one method CF to a database biosphere flow. The built-in matchers
are tried in cascade order – UUID → name → synonym → CAS – and the first whose
target flow is present in the by-UUID index wins. A matcher resolving to a flow
absent from that index is skipped, so resolution falls through to the next.
-}
resolveCF :: MapContext -> MethodCF -> Maybe (BiosphereFlow, MatchStrategy)
resolveCF ctx cf =
    canon ByUUID (findFlowByUUID (mcBioFlowsByUUID ctx) (mcfFlowRef cf))
        <|> canon ByName (findFlowByNameComp cmap (mcBioFlowsByName ctx) (mcfFlowName cf) (mcfCompartment cf))
        <|> canon BySynonym (findFlowBySynonymMemo ctx cf)
        <|> canon ByCAS (mcfCAS cf >>= \cas -> findFlowByCAS cmap (mcBioFlowsByCAS ctx) cas (mcfCompartment cf))
  where
    cmap = mcCompartmentMap ctx
    canon strat found = found >>= \flow -> (,strat) <$> M.lookup (bfId flow) (mcBioFlowsByUUID ctx)

-- | Convenience wrapper: map method CFs using the built-in cascade + DB.
mapMethodToFlows :: CompartmentMap -> Database -> Method -> IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mapMethodToFlows cmap db = mapMethodFlows (buildMapContext cmap db)

{- | A CF whose substance is a wildcard pattern rather than a literal flow
name. A trailing @*@ makes the text before it a case-insensitive prefix
(@"Occupation, *"@ covers every occupation flow); a bare @"*"@ matches any
name, leaving the row's CAS and compartment cells as the whole predicate.
Pattern CFs never enter the matcher cascade – 'expandPatternCF' materializes
them against the database – so a method can declare an open family of flows
as one rule instead of a per-database list that silently goes stale.
-}
isPatternCF :: MethodCF -> Bool
isPatternCF cf = not (isExclusionCF cf) && T.isSuffixOf "*" (mcfFlowName cf)

{- | A row whose substance starts with @!@: an exception carved out of the
patterns declared for the same impact category. Some open families have members
that do not belong to the quantity the category counts – an occupation family
holds the sea floor as well as the fields – and no set of prefixes can separate
them, because @"Occupation, industrial area, benthos"@ shares its prefix with
real industrial land.

The substance after the @!@ reads exactly like a pattern's: a case-insensitive
name prefix, with the trailing @*@ optional since a full flow name is already
the prefix of nothing else. The value cell says which categories the exception
applies to; its magnitude is never used.

Exceptions narrow patterns only. A literal row is a deliberate statement about
one flow, so excluding it would be the method contradicting itself rather than
qualifying an open family.
-}
isExclusionCF :: MethodCF -> Bool
isExclusionCF = T.isPrefixOf "!" . mcfFlowName

{- | The case-insensitive name prefix a pattern or exclusion row selects on:
the substance cell without its @!@ marker and without its trailing @*@.
-}
patternPrefix :: MethodCF -> Text
patternPrefix = T.toCaseFold . dropStar . dropBang . mcfFlowName
  where
    dropBang t = fromMaybe t (T.stripPrefix "!" t)
    dropStar t = fromMaybe t (T.stripSuffix "*" t)

{- | Does a pattern-shaped row select that flow? Every predicate the row
carries must hold: name prefix, CAS, compartment medium and – when the row
states one – subcompartment. Shared by 'expandPatternCF' and by the exclusions
that narrow it, so a family and its exceptions are read by one rule.
-}
selectsFlow :: MethodCF -> BiosphereFlow -> Bool
-- Written as a lambda over the flow so the row's own share of the work – case
-- folding its prefix, normalizing its CAS – is done once per row rather than
-- once per (row, flow) pair: this runs over the whole biosphere.
selectsFlow cf = \f -> prefix `T.isPrefixOf` T.toCaseFold (bfName f) && casFits f && compFits f
  where
    prefix = patternPrefix cf
    wantCAS = normalizeCAS <$> mcfCAS cf
    casFits f = maybe True (\cas -> bfCAS f == Just cas) wantCAS
    compFits f = case mcfCompartment cf of
        Nothing -> True
        Just (Compartment med sub _) ->
            maybe False (\c -> mediumEq med (VT.compartmentName c) && subFits sub c) (bfCompartment f)
    subFits sub c = subcompartmentFits sub (fromMaybe "" (VT.compartmentSub c))
    -- The stated word against the flow's own value: a statement this reader
    -- cannot place names no medium the flow could be filed under.
    mediumEq :: Text -> Medium -> Bool
    mediumEq stated actual = parseMedium stated == Right actual

{- | The medium a word names, or 'Nothing' when this reader cannot place it.
The one conversion from a source's spelling to the value the tables key on.
-}
mediumKey :: Text -> MediumKey
mediumKey = either (const Nothing) Just . parseMedium

{- | The medium a factor states, after the compartment rules but before any
table: 'Nothing' when it names none, or none this reader can place.
-}
cfMedium :: MethodCF -> MediumKey
cfMedium cf = do
    Compartment med _ _ <- mcfCompartment cf
    mediumKey med

{- | The medium axis of a lookup key: the medium a source stated, or 'Nothing'
where it stated none. A flow whose file gave it no compartment keys on
'Nothing' and meets no factor, which is also what a factor without one gets:
'cfMediumSub' leaves such a factor out of these tables rather than filing it
under a medium it never claimed.
-}
type MediumKey = Maybe Medium

{- | Does a flow's medium satisfy the one a method row states? Equal after
the same value, as the scoring tables key it: 'buildMethodTables' indexes a
factor under the row's normalized medium and 'flowMediumSub' files a flow
under its own, so a match judged any looser here promises a factor the read
side never serves. Both sides read their words with 'parseMedium', so two
spellings of one medium meet in the value they name; "air" against "urban air"
is a vocabulary gap that only a compartment rule bridges, not a widening. An
empty statement constrains nothing.
-}
mediumFits :: Text -> MediumKey -> Bool
mediumFits stated actual
    | T.null stated = True
    | otherwise = either (const False) (\m -> actual == Just m) (parseMedium stated)

{- | Does a flow's subcompartment satisfy the one a method row states? A
statement that names no particular subcompartment constrains only the medium,
judged by 'isUnspecifiedSub' so this reads "unspecified" the way every other
caller does; a stated one must be the flow's, folded in case and nothing more.
Containment would make a row written for "low. pop." the exact match of a flow
at "low. pop., long-term", a distinction every method that draws it draws on
purpose. Qualifiers are ignored, as 'buildMethodTables' ignores them.
-}
subcompartmentFits :: Text -> Text -> Bool
subcompartmentFits stated actual =
    isUnspecifiedSub (T.toCaseFold stated) || T.toCaseFold stated == T.toCaseFold actual

{- | Is that flow taken back by any of these exclusion rows? The predicates are
built once for the row set, then run over as many flows as the caller has.
-}
excludedBy :: [MethodCF] -> BiosphereFlow -> Bool
excludedBy exclusions = \f -> any ($ f) predicates
  where
    predicates = map selectsFlow exclusions

{- | Is the row constrained by anything at all? A selector with no name prefix,
no CAS and no compartment would match the entire biosphere, which is never what
a method means.
-}
isConstrainedCF :: MethodCF -> Bool
isConstrainedCF cf =
    not (T.null (patternPrefix cf)) || isJust (mcfCAS cf) || isJust (mcfCompartment cf)

{- | Materialize one pattern CF against the database's biosphere: one concrete
CF per flow the row selects and no exclusion of the same category takes back.
Each concrete CF takes the flow's own identity (UUID, name, CAS, compartment) –
so every table built from the mapping lands exactly where the inventory flow
will look – and keeps the pattern row's value and unit. A database introducing
a new flow under the pattern is thus counted on its next mapping without
touching the method file.

Failure is loud, not silent: a pattern matching no flow comes back as an
unmatched row plus a warning (coverage then shows the gap instead of the
category quietly counting zero), and a bare @"*"@ constrained by nothing is
refused the same way – matching the entire biosphere is never intended. A
pattern whose every match is excluded is refused on the same grounds: a family
that survives as nothing is a method file that has stopped saying anything.
-}
expandPatternCF ::
    BioFlowDB ->
    -- | exclusions declared for this category, see 'isExclusionCF'
    [MethodCF] ->
    MethodCF ->
    ([(MethodCF, Maybe (BiosphereFlow, MatchStrategy))], [Text])
expandPatternCF flows exclusions cf
    | not (isConstrainedCF cf) = refuse "has no name prefix, CAS or compartment; refusing to match every flow"
    | null selected = refuse "matches no flow in this database"
    | null matches = refuse "matches only flows its exclusions take back"
    | otherwise = ([(materialize f, Just (f, ByName)) | f <- matches], [])
  where
    refuse why = ([(cf, Nothing)], ["wildcard CF '" <> mcfFlowName cf <> "' " <> why])
    selected = filter (selectsFlow cf) (M.elems flows)
    matches = filter (not . excludedBy exclusions) selected
    materialize f =
        cf
            { mcfFlowRef = bfId f
            , mcfFlowName = bfName f
            , mcfCAS = bfCAS f
            , mcfCompartment = fromFlowCompartment <$> bfCompartment f
            }
    fromFlowCompartment c =
        Compartment (VT.mediumText (VT.compartmentName c)) (fromMaybe "" (VT.compartmentSub c)) ""

{- | Take the exclusions' flows back out of a finished mapping list.

'expandPatternCF' already leaves them out when it materializes a family, but
the mappings scoring reads are that output re-expanded – synonym fan-out,
regional projection, proxy edges – and those expansions travel by flow name,
knowing nothing of the method's exceptions. The curated flow registry bridges
@"Occupation, industrial area"@ and @"Occupation, industrial area, benthos"@
through the label they share, so a surviving sibling would hand the excluded
sea floor its own factor back. Applied after the expansions, this holds the
exception wherever the flow re-enters.
-}
dropExcludedMappings ::
    -- | exclusions declared for this category, see 'isExclusionCF'
    [MethodCF] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
dropExcludedMappings [] = id
dropExcludedMappings exclusions = filter kept
  where
    excluded = excludedBy exclusions
    -- An unmatched row carries no flow to judge, and dropping it would hide the
    -- gap the coverage report exists to show.
    kept (_, Just (f, _)) = not (excluded f)
    kept (_, Nothing) = True

{- | Why an exclusion row could not do its job, if it could not. An exclusion
that takes nothing back is almost always a misspelling, and left unsaid it
reads exactly like a category that never needed the exception.
-}
exclusionWarning :: BioFlowDB -> MethodCF -> Maybe Text
exclusionWarning flows cf
    | not (isConstrainedCF cf) = Just (why "has no name prefix, CAS or compartment; refusing to exclude every flow")
    | not (any (selectsFlow cf) (M.elems flows)) = Just (why "matches no flow in this database")
    | otherwise = Nothing
  where
    why reason = "exclusion CF '" <> mcfFlowName cf <> "' " <> reason

{- | The factors the cascade left unmatched although a flow of their name is
in the database, stated in a medium no flow of the database is filed under.
That is a vocabulary gap ("air" against "urban air"), which a compartment
mapping bridges and nothing else will: said once per method, with both
vocabularies, so the operator knows what to declare. A spelling that names
only a medium is not one of these, 'parseMedium' having read it already.

Judged on the whole database, not on the flows of that name: a substance the
database has in water and not in air leaves a factor stated in air unmatched
too, and that is the database's inventory, not its vocabulary.
-}
compartmentGapWarning :: CompartmentMap -> M.Map Text [BiosphereFlow] -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> Maybe Text
compartmentGapWarning cmap flowsByName mappings
    | null gaps = Nothing
    | otherwise =
        Just $
            T.pack (show (length gaps))
                <> " factor(s) name a flow this database files under another compartment (method: "
                <> quoted (S.fromList (map fst gaps))
                <> "; database: "
                <> quoted (S.map VT.mediumText (S.unions (map snd gaps)))
                <> "). Declare a [[compartment-mappings]] table bridging them."
  where
    -- The gap is between vocabularies, so the method's side stays the word the
    -- method wrote: the ones worth reporting are precisely those this reader
    -- cannot place, and a value would have nothing left to print.
    statedMedium :: MethodCF -> Maybe Text
    statedMedium cf = do
        comp <- mcfCompartment cf
        let Compartment med _ _ = normalizeCompartment cmap comp
        pure med
    databaseMedia :: S.Set Medium
    databaseMedia =
        S.fromList (mapMaybe (fst . flowMediumSub cmap) (concat (M.elems flowsByName)))
    speaks :: Text -> Bool
    speaks med = either (const False) (`S.member` databaseMedia) (parseMedium med)
    gaps :: [(Text, S.Set Medium)]
    gaps =
        [ (stated, seen)
        | (cf, Nothing) <- mappings
        , Just stated <- [statedMedium cf]
        , not (speaks stated)
        , Just flows <- [M.lookup (normalizeName (mcfFlowName cf)) flowsByName]
        , let seen = S.fromList (mapMaybe (fst . flowMediumSub cmap) flows)
        , not (S.null seen)
        ]
    quoted :: S.Set Text -> Text
    quoted = T.intercalate ", " . map (\t -> "\"" <> t <> "\"") . S.toList

-- | Wire name for a match strategy.
strategyToText :: MatchStrategy -> Text
strategyToText ByUUID = "uuid"
strategyToText ByCAS = "cas"
strategyToText ByName = "name"
strategyToText BySynonym = "synonym"
strategyToText ByProxy = "proxy"

{- | How an entry was attached, as every surface that reports it words the
answer. An entry no build-side resolution reached reads @"none"@: it is filed
under the name the method itself uses, and the read cascade may still serve it.
-}
provenanceStrategyText :: BuildProvenance -> Text
provenanceStrategyText = maybe "none" strategyToText . bpStrategy

-- ──────────────────────────────────────────────
-- Low-level matching functions (used by built-in MapperHandles)
-- ──────────────────────────────────────────────

-- | Find flow by exact UUID match
findFlowByUUID :: M.Map UUID BiosphereFlow -> UUID -> Maybe BiosphereFlow
findFlowByUUID flowsByUUID uuid = M.lookup uuid flowsByUUID

{- | Find flow by CAS number with compartment preference. Canonicalizes on the
way in, as 'findFlowByNameComp' does for names: the index is keyed canonically,
and a method that states a padded CAS must still meet the flow that states it
unpadded.
-}
findFlowByCAS :: CompartmentMap -> M.Map Text [BiosphereFlow] -> Text -> Maybe Compartment -> Maybe BiosphereFlow
findFlowByCAS cmap flowsByCAS cas mComp =
    nonEmptyCAS cas >>= (`M.lookup` flowsByCAS) >>= \flows -> pickByCompartment cmap flows mComp

-- | Find flow by normalized name match (compartment-aware)
findFlowByName :: CompartmentMap -> M.Map Text [BiosphereFlow] -> Text -> Maybe BiosphereFlow
findFlowByName cmap flowsByName name = findFlowByNameComp cmap flowsByName name Nothing

-- | Find flow by normalized name with compartment preference
findFlowByNameComp :: CompartmentMap -> M.Map Text [BiosphereFlow] -> Text -> Maybe Compartment -> Maybe BiosphereFlow
findFlowByNameComp cmap flowsByName name mComp =
    M.lookup (normalizeName name) flowsByName >>= \flows -> pickByCompartment cmap flows mComp

{- | What a synonym search reads: the group table it expands a name in, the
flows it can land on, and the compartment table it judges a landing with.
-}
data SynonymSearch = SynonymSearch
    { ssSynonyms :: !SynonymDB
    , ssFlowsByName :: !(M.Map Text [BiosphereFlow])
    , ssCompartments :: !CompartmentMap
    }

-- | Find flow via synonym group (compartment-aware)
findFlowBySynonym :: SynonymSearch -> Text -> Maybe BiosphereFlow
findFlowBySynonym search name = findFlowBySynonymComp search name Nothing

-- | Find flow via synonym group with compartment preference
findFlowBySynonymComp :: SynonymSearch -> Text -> Maybe Compartment -> Maybe BiosphereFlow
findFlowBySynonymComp (SynonymSearch synDB flowsByName cmap) name mComp =
    case lookupSynonymGroup synDB name of
        Nothing -> Nothing
        Just gid ->
            getSynonyms synDB gid >>= \synonyms ->
                pickByCompartment cmap (concatMap (lookupFlows flowsByName) synonyms) mComp
  where
    lookupFlows :: M.Map Text [BiosphereFlow] -> Text -> [BiosphereFlow]
    lookupFlows fbn syn = M.findWithDefault [] (normalizeName syn) fbn

{- | The synonym view a CF resolves against: input-only bridges apply to INPUT
(resource) CFs, output-only to OUTPUT (emission) CFs. On untyped data both
views coincide with the union tables, so this is a no-op.
-}
viewFor :: FlowDirection -> SynonymDB -> SynonymDB
viewFor Input = inputView
viewFor Output = outputView

{- | Synonym match that reads a precomputed group → flows memo. When
'mapMethodFlows' has expanded this CF's group (the common path), the resolution
is a single map lookup plus the compartment pick; otherwise it falls back to
'findFlowBySynonymComp'. The memoized flow list is exactly that function's
inline @concatMap@ (same elements, same order), so the pick – and the flow it
returns – is identical: a pure speedup. The group is resolved in the CF's
direction view, and the memo is keyed by @(direction, group id)@ because the two
views number their groups independently. Takes the CF and context whole so
direction, name and compartment can never be mixed from different CFs.
-}
findFlowBySynonymMemo :: MapContext -> MethodCF -> Maybe BiosphereFlow
findFlowBySynonymMemo ctx cf =
    case lookupSynonymGroup dirDB name of
        Nothing -> Nothing
        Just gid -> case M.lookup (dir, gid) (mcSynGroupFlows ctx) of
            Just flows -> pickByCompartment (mcCompartmentMap ctx) flows mComp
            Nothing -> findFlowBySynonymComp (SynonymSearch dirDB (mcBioFlowsByName ctx) (mcCompartmentMap ctx)) name mComp
  where
    dir = mcfDirection cf
    dirDB = viewFor dir (mcSynonymDB ctx)
    name = mcfFlowName cf
    mComp = mcfCompartment cf

{- | Expand, once per @(direction, synonym group)@, the candidate flows reachable
from that group's names – for exactly the groups the given CFs reference.
'mapMethodFlows' runs this before resolving a method's CFs, so the synonym matcher
reads a shared (often large) group's flows from the memo instead of re-expanding
it per CF. The per-group value is identical to 'findFlowBySynonymComp''s inline
@concatMap@. Keyed by direction because a CF resolves against its direction's view.
-}
buildSynGroupFlows :: MapContext -> [MethodCF] -> M.Map (FlowDirection, Int) [BiosphereFlow]
buildSynGroupFlows ctx cfs =
    M.fromList
        [ ((dir, gid), maybe [] (concatMap groupFlows) (getSynonyms (viewFor dir synDB) gid))
        | (dir, gid) <- keys
        ]
  where
    synDB = mcSynonymDB ctx
    flowsByName = mcBioFlowsByName ctx
    groupFlows syn = M.findWithDefault [] (normalizeName syn) flowsByName
    keys =
        S.toList $
            S.fromList
                [ (mcfDirection cf, gid)
                | cf <- cfs
                , Just gid <- [lookupSynonymGroup (viewFor (mcfDirection cf) synDB) (mcfFlowName cf)]
                ]

{- | The flow a method row's compartment names, among the flows sharing its
name: the one whose subcompartment the row states, else the one filed under no
particular subcompartment, else any in the row's medium.

Both sides go through 'normalizeCompartment' first, which is what lets a row
written "urban air" meet a flow filed under "air", subcompartment "urban"; the
scoring tables read the same table, so the cascade and the tables agree on what
a compartment is. Without it the row's medium, now a condition, would veto a
spelling only the table relates.

A compartment a row states is a condition, not a preference. When no candidate
is in that medium this answers Nothing, so 'resolveCF' moves on to the next
matcher and the mapping counts say what they mean. It used to return the first
candidate whatever its medium, which reported a name match on a flow the row
does not describe, and stopped the cascade before CAS could try.

The catch-all rung is what keeps the remaining choice from being arbitrary: a
row stating "low. pop." against candidates at "low. pop., long-term" and at no
subcompartment takes the second, the one that claims nothing, rather than
whichever the index listed first.
-}
pickByCompartment :: CompartmentMap -> [BiosphereFlow] -> Maybe Compartment -> Maybe BiosphereFlow
pickByCompartment _ [] _ = Nothing
pickByCompartment _ (f : _) Nothing = Just f
pickByCompartment cmap flows (Just stated) =
    find exactMatch flows <|> find catchAllSub flows <|> find inMedium flows
  where
    Compartment statedMed statedSub _ = normalizeCompartment cmap stated
    flowSub :: BiosphereFlow -> Text
    flowSub = unSub . snd . flowMediumSub cmap
    inMedium :: BiosphereFlow -> Bool
    inMedium fl = mediumFits statedMed (fst (flowMediumSub cmap fl))
    exactMatch :: BiosphereFlow -> Bool
    exactMatch fl = inMedium fl && subcompartmentFits statedSub (flowSub fl)
    catchAllSub :: BiosphereFlow -> Bool
    catchAllSub fl = inMedium fl && isUnspecifiedSub (flowSub fl)

{- | Per-strategy counts of mapping results in one pass.
Each 'MatchStrategy' must be named below – adding a new variant is a
compile error here until it gets a row.
-}
computeMappingStats :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> MappingStats
computeMappingStats = foldMap (tally . fmap snd . snd)
  where
    one = mempty{msTotal = 1}
    tally Nothing = one{msUnmatched = 1}
    tally (Just ByUUID) = one{msByUUID = 1}
    tally (Just ByCAS) = one{msByCAS = 1}
    tally (Just ByName) = one{msByName = 1}
    tally (Just BySynonym) = one{msBySynonym = 1}
    tally (Just ByProxy) = one{msByProxy = 1}

{- | The unit a CF value is denominated in ('mcfUnit' as parsed – a flow
reference unit like @"kg"@, or an impact-result expression like @"kg CO2 eq"@).
A distinct type from the flow's own unit so the two sides of a
flow-unit → CF-unit conversion can't be silently swapped.
-}
newtype CFUnit = CFUnit Text
    deriving (Eq, Show)

{- | A characterization factor as stored in the lookup tables: the value and
the unit it is denominated in, kept together so they can't drift apart.
-}
data CF = CF
    { cfValue :: !Double
    , cfUnit :: !CFUnit
    }
    deriving (Eq, Show)

{- | How a method line got attached to a lookup-table key at build time.
'bpSource' shares the already-cached 'MethodCF' – a pointer, not a copy –
so keeping provenance in the tables costs a few words per entry.
-}
data BuildProvenance = BuildProvenance
    { bpStrategy :: !(Maybe MatchStrategy)
    {- ^ The bridge a build-side resolution took to attach the line to a
    database flow; 'Nothing' when the entry is keyed under the method's own
    flow name because no database flow resolved at build time (the read-time
    cascade can still serve it to a flow arriving at that key).
    -}
    , bpSource :: !MethodCF
    -- ^ The method line that authored the entry.
    }
    deriving (Eq, Show)

{- | A CF as served by the read-time lookup tables, together with its build
provenance. Scoring only ever reads 'teCF'; the provenance exists for
explanation surfaces and for the match-strategy the API reports.
-}
data TableEntry = TableEntry
    { teCF :: !CF
    , teProvenance :: !BuildProvenance
    }
    deriving (Eq, Show)

{- | Precomputed CF lookup tables for one (database, method) pair.
Building these from raw mappings is O(n log n) over thousands of CFs, so they
should be computed once per method and reused across inventories.
-}
data MethodTables = MethodTables
    { mtUuidCF :: !(M.Map UUID TableEntry)
    -- ^ UUID-matched CFs: exact flow id → CF
    , mtUnitVariantCF :: !(M.Map (SR.NormName, MediumKey) TableEntry)
    {- ^ (unit-suffix-preserving normalized name, medium) → CF, holding only
    rows whose name carries a SimaPro unit suffix (@"Gas, natural\/m3"@).
    'normalizeName' strips that suffix, so a method's own per-unit rows
    (@\/kg@ 43.1 vs @\/m3@ 34.5 – same substance, different densities)
    collapse onto one name key where a single winner is crowned; the losing
    variant's flow then reads a dimensionally incompatible CF and its unit
    conversion silently zeroes the score. This table keys each variant by its
    full name ('normalizeNameKeepUnit') so a suffixed flow finds the row
    declared in its own unit. Consulted before the collapsed-name tables;
    same-key rows that disagree are dropped ('agreedValue' – never guess).
    -}
    , mtExactCF :: !(M.Map (SR.NormName, MediumKey, Subcompartment) TableEntry)
    -- ^ (normalized name, medium, subcompartment) → CF
    , mtFallbackCF :: !(M.Map (SR.NormName, MediumKey) TableEntry)
    -- ^ (normalized name, medium) → CF for entries with unspecified subcompartment
    , mtLongTermFallbackCF :: !(M.Map (SR.NormName, MediumKey) TableEntry)
    {- ^ (normalized name, medium) → CF for entries with the long-term
    UNSPECIFIED subcompartment ("unspecified (long-term)"). A long-term flow at an
    uncovered specific subcompartment ("groundwater, long-term") inherits this –
    the method's long-term default, often a deliberate zero – instead of the
    immediate-emission 'mtFallbackCF', so JRC scores delayed emissions with the
    method's own long-term factor rather than the immediate one.
    -}
    , mtSubBlindCF :: !(M.Map (SR.NormName, MediumKey) TableEntry)
    {- ^ (normalized name, medium) → CF, but only where the substance's
    factor is the SAME across every subcompartment – i.e. the subcompartment
    genuinely doesn't change it (mineral/metal extraction: "Cadmium, in ground"
    == any sub). Lets a flow whose sub matches no exact CF and has no
    unspecified fallback still resolve, without guessing for a substance whose
    factor DOES vary by sub (water by source), which is omitted as ambiguous.
    -}
    , mtCasCF :: !(M.Map (SR.CASNumber, MediumKey) TableEntry)
    {- ^ (CAS, normalized medium) → CF, from non-regionalized CFs.
    Read-path fallback after UUID and name. Without it, a CF resolves to a
    single database flow at build time, so when many flows share one CAS in a
    compartment (e.g. every water flow shares 7732-18-5) only that one flow is
    characterized and the rest score zero. Keyed by the CF's own CAS+medium so
    the read path can reach every same-CAS flow by its own CAS+medium. The
    bridge is deliberately subcompartment-blind: a resource flow and the CF
    that characterizes it routinely disagree on subcompartment after
    normalization, so requiring agreement zeroes whole resource categories
    (minerals are reachable only through this bridge). Empty for methods whose
    CFs carry no CAS.
    -}
    , mtRegionalCasCF :: !(M.Map (SR.CASNumber, MediumKey) (M.Map Location CF))
    {- ^ (CAS, normalized medium) → (location → CF), from regionalized
    CFs. The regionalized analogue of 'mtCasCF': lets the regionalized build
    characterize every same-CAS flow per location, not just the one a CF
    resolved to. Empty for methods with no regionalized CAS-bearing CFs.
    -}
    , mtRegionalizedCF :: !(M.Map (UUID, Location) CF)
    {- ^ Regionalized cells of the C matrix: (DB flow UUID, consumer location) → CF.
    Empty for non-regionalized methods. When non-empty, callers should dispatch
    to the regionalized scoring path (see 'Matrix.computeRegionalizedLCIAScore').
    -}
    , mtCFFamily :: !CFFamily
    {- ^ The CF family the method's result unit implies (see 'cfFamily'). The
    subcompartment gates key off this – a USEtox toxicity method
    ('USEtoxFamily') doesn't characterize groundwater, a nutrient method does.
    -}
    , mtSeaWaterCFs :: !SeaWaterCFs
    {- ^ Whether this method writes any sea-water factor of its own
    (see 'SeaWaterCFs'). The sea half of the medium-level gate keys off it, so
    the engine defers to a method that distinguishes the sea and does not
    invent a zero for one that never mentions it.
    -}
    , mtCompartmentMap :: !CompartmentMap
    {- ^ Compartment-normalization rules (e.g. @"Emissions to air" → "air"@).
    Applied to both CF compartments at build time and database flow
    compartments at query time, so both sides converge to the same
    canonical form. Empty map = identity, no normalization.
    -}
    , mtEnergyDensities :: !EnergyDensityMap
    {- ^ Normalized flow name → physical content per native flow unit (a
    calorific value in MJ/kg, a mass density in m³/kg). Lets a CF denominated
    in the content's target unit (a JRC fossil CF in MJ, a water-scarcity CF
    in m³) characterize an inventory flow of another dimension (kg, Sm3): the
    flow quantity is bridged into the target unit before the CF multiply.
    Flows with no entry behave exactly as before – a cross-dimensional CF
    still yields a zero effective CF. Empty map = feature inactive.
    -}
    , mtResolution :: !(M.Map UUID FlowCFTag)
    {- ^ How each broadcast-covered flow got its factor: the rung that served
    it, and the refusal if the flow→CF conversion could not be made. Filled in
    the same pass as 'mtBroadcast', which collapses a factor to one 'Double'
    and so cannot distinguish "no CF" from "CF refused on units" from "the
    method's own zero" – this map is what keeps those three apart. Absent key
    = the cascade found no CF at all.
    -}
    , mtJudged :: !(S.Set UUID)
    {- ^ The flows 'fillBroadcastVector' walked, the keys of the flow table it
    was given. Each went through the cascade, so one this set holds that
    'mtBroadcast' omits carries no factor and a scorer need not ask again; a
    flow outside it arrived after the fill (a what-if substitution into a
    database loaded since) and still takes the cascade. Only the keys are
    kept: the records themselves would outlive their database's unload.
    Empty until the fill.
    -}
    , mtBroadcast :: !(M.Map UUID Double)
    {- ^ Pre-multiplied broadcast CFs: flow UUID → effective CF (CF value × flow→CF unit conversion).
    Collapses the UUID/exact/fallback cascade into a single Map and absorbs unit conversion
    so the scoring hot path is a pure multiply-accumulate. Empty when 'buildMethodTables' is
    called directly without DB-level dependencies; fill with 'fillBroadcastVector' to enable
    the fast path (scoring falls back to the cascade when this Map is empty).
    -}
    , mtRegionalActivityWeights :: !(Maybe RegionalActivityWeights)
    {- ^ Optional per-activity-column precomputed weights for regionalized methods.
    When present, regionalized LCIA score collapses to a single dot product
    @w · s@ instead of walking the whole biosphere triples for every pid.
    Built by 'fillRegionalActivityWeights' against a specific 'Database'; Nothing
    when 'mtRegionalizedCF' is empty or precomputation hasn't been run.
    -}
    }

{- | Per-matrix-column precomputed contributions for a regionalized method.

For every activity column @a@ in the technosphere matrix, @rawWeights[a]@ is
the inventory-weighted sum of regionalized CFs that activity emits in its own
location:

  @rawWeights[a] = Σ_f B[f,a] · CF(f, loc(a))@

With this in hand, a regionalized LCIA score for any scaling vector @s_k@
reduces to one dot product:

  @score_k = Σ_a rawWeights[a] · s_k[a]@

instead of re-walking the full biosphere triples once per pid.

@rawTainted[a] == 1@ means at least one biosphere triple at column @a@ has a
regionalized flow with no CF for @loc(a)@; the partial weight is still stored
(matching the existing surface where missing-CF activities under-count), but
callers can surface a 'Left' when any tainted column carries non-zero scaling.

@rawMissingPairs@ accumulates the deduplicated (flow, location) gaps so the
caller can emit one warning per gap rather than per pid × per method.
-}
data RegionalActivityWeights = RegionalActivityWeights
    { rawWeights :: !(U.Vector Double)
    , rawTainted :: !(U.Vector Word8)
    , rawMissingPairs :: ![(UUID, Location)]
    }

{- | Inverted indices over a 'Method' for the post-scoring suggester.

Built from the raw 'Method' (not 'MethodTables', which has lost the source CF
metadata after the lookup tables are constructed). Cached separately because
the suggester is opt-in and only consulted on the small uncharacterized tail.

* 'miCFs' – all CFs in source order; vector-indexed for cheap parallel arrays.
* 'miCFTokens' – parallel to 'miCFs', each CF's normalized-name tokens.
* 'miByMedium' – medium → indices into 'miCFs', for short-circuiting
  candidate scans to the same compartment medium. The 'Nothing' key holds the
  CFs whose compartment metadata names no medium this reader can place.
* 'miByCAS' – normalized CAS → indices into 'miCFs'. Multiple CFs can share a
  CAS (same substance in different compartments); caller picks the best.
-}
data MethodIndex = MethodIndex
    { miCFs :: !(V.Vector MethodCF)
    , miCFTokens :: !(V.Vector (S.Set Text))
    , miByMedium :: !(M.Map MediumKey [Int])
    , miByCAS :: !(M.Map Text [Int])
    }

{- | Result of scoring an inventory: the score plus diagnostics.

* 'loScore' – the LCIA score in the method's reference unit. Bit-equivalent
  to the previous Double-only return.
* 'loCharacterizedSum' / 'loInventoryAbsSum' – together they reveal how much
  of the inventory was actually characterized (by absolute mass). The
  difference is the silent-omission tail; small ratio means the score is
  trustworthy, large ratio means many flows had no CF.
* 'loUncharacterized' – flows with non-trivial inventory weight but no
  matching CF, ranked by 'ucfAbsWeight'. Cap and threshold via
  'UncharacterizedOpts'. Empty list ⇒ no diagnostics requested OR no flows
  above threshold.
* 'loUnknownUuids' – non-zero inventory UUIDs with no record in 'flowDB'.
  These indicate merged-metadata gaps, not mapping bugs; surface
  separately so the caller can react (per the "no silent errors" rule).
-}
data LCIAOutcome = LCIAOutcome
    { loScore :: !Double
    , loCharacterizedSum :: !Double
    , loInventoryAbsSum :: !Double
    , loUncharacterized :: ![UncharacterizedFlow]
    , loUnknownUuids :: ![UUID]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON)

{- | A flow that carries inventory weight but found no matching CF.

The 'ucfSimilarCFs' field lets consumers distinguish the two cases:

* @[]@ – the method genuinely has no CF resembling this flow → legitimate gap.
* non-empty – the method has CFs that look homologous → likely mapping bug,
  worth a synonym entry in @data/flows.csv@ or a fresh PubChem regen.
-}
data UncharacterizedFlow = UncharacterizedFlow
    { ucfFlowId :: !UUID
    , ucfFlowName :: !Text
    , ucfCategory :: !Text
    , ucfSubcomp :: !(Maybe Text)
    , ucfFlowUnit :: !Text
    , ucfQuantity :: !Double
    , ucfAbsWeight :: !Double
    -- ^ |qty| / Σ|qty| over the whole inventory, in [0, 1].
    , ucfSimilarCFs :: ![SimilarCF]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON)

-- | A candidate CF flagged by the suggester for an uncharacterized flow.
data SimilarCF = SimilarCF
    { scfMethodFlowName :: !Text
    , scfCAS :: !(Maybe Text)
    , scfCompartment :: !(Maybe Compartment)
    , scfScore :: !Double
    -- ^ Combined similarity in [0, 1] (max of the three signals).
    , scfReason :: !SimilarReason
    -- ^ Which signal produced this candidate – guides human validation.
    , scfCfValue :: !Double
    , scfCfUnit :: !Text
    }
    deriving (Eq, Show, Generic, NFData, ToJSON)

{- | Why a 'SimilarCF' was flagged. Carried through to the audit JSON so a
human reviewer knows what to verify (formula, CAS, or just the names).
-}
data SimilarReason
    = {- | Token-overlap Jaccard on normalized names. Catches word-order /
      punctuation variants like "Methane, biogenic" ↔ "Methane biogenic".
      -}
      SimByJaccard
    | {- | Token Jaccard after expanding via PubChem synonyms. Catches
      formula↔name pairs like "CO2" ↔ "Carbon dioxide".
      -}
      SimBySynonymExpansion
    | {- | The flow's CAS matched a CF's CAS even though names diverged.
      Highest-confidence reason.
      -}
      SimByCASBridge
    deriving (Eq, Show, Generic, NFData, ToJSON)

{- | Tunable knobs for uncharacterized-flow diagnostics.

* 'uoMinAbsWeight' – drop flows below this share of total |qty|. Defaults
  to 0.001 (0.1%) so noise doesn't drown signal.
* 'uoMaxSimilar' – top-N candidate CFs per uncharacterized flow. 0 disables
  the similarity scan (useful in hot paths).
* 'uoMaxFlows' – cap on the diagnostics list size, so payloads stay bounded
  even on inventories with many tiny gaps.
-}
data UncharacterizedOpts = UncharacterizedOpts
    { uoMinAbsWeight :: !Double
    , uoMaxSimilar :: !Int
    , uoMaxFlows :: !Int
    }
    deriving (Eq, Show)

defaultUncharacterizedOpts :: UncharacterizedOpts
defaultUncharacterizedOpts =
    UncharacterizedOpts
        { uoMinAbsWeight = 0.001
        , uoMaxSimilar = 3
        , uoMaxFlows = 50
        }

-- | Build a 'MethodIndex' from a raw 'Method'. Run once per method, cache.
buildMethodIndex :: Method -> MethodIndex
buildMethodIndex method =
    let cfs = V.fromList (methodFactors method)
        tokens = V.map cfTokens cfs
        indexed = zip [0 ..] (methodFactors method)
     in MethodIndex
            { miCFs = cfs
            , miCFTokens = tokens
            , miByMedium = M.fromListWith (++) [(cfMedium cf, [i]) | (i, cf) <- indexed]
            , miByCAS = M.fromListWith (++) [(cas, [i]) | (i, cf) <- indexed, Just cas <- [mcfCAS cf]]
            }
  where
    cfTokens :: MethodCF -> S.Set Text
    cfTokens = S.fromList . T.words . normalizeName . mcfFlowName

{- | Build 'MethodTables' from raw mappings and a 'CompartmentMap'.

The map is applied to each CF's compartment before keying the lookup
tables, so all keys land in canonical form. The same map is then stored
in the result so 'computeLCIAScoreFromTables' can normalize inventory-side
compartments at query time and meet at the same canonical keys.

Pass 'M.empty' for the compartment map when no normalization is desired
(behaves identically to the pre-CompartmentMap implementation).
-}

{- | Fan out one-to-many synonym matches: for each CF, look up the
synonym group of the CF name and emit one extra @(cf, Just (peerFlow,
BySynonym))@ row per flow whose name is in the group, resolved in the
CF's direction view. The original mapping list is preserved.

This is the missing piece when a method-side CF (e.g. ILCD's bare
@copper@) covers many inventory-side variants (@Copper, 2.19% in
sulfide, …@, @Copper, 1.18% in sulfide, …@, etc.). Without expansion,
@buildMethodTables@ would key the CF under a single matched-flow name
and the other variants would silently look up as @Nothing@. With
expansion, the CF is keyed under every group member, so every inventory
variant of the same substance finds the same CF.

The fan-out follows the registry's own closure classes, exactly as
'lookupSynonymGroup' reports them. A curated chain routinely pivots
through an alias that names no loaded flow (@Energy, from coal@ =
@hard coal@ = @Coal, hard@ – only the endpoints are flows), so the walk
must not require every intermediate to be a flow or CF name: an earlier
version re-closed the relation on that induced subgraph and silently
cut every such bridge. Keeping junk hubs out of the closure is the
ingestion layer's job – matching trusts only the curated registry plus
sources the user explicitly activates, and candidates pass
'excludeJunkSynonyms' / 'excludeOverFrequentSynonyms' with an
'oversizedClasses' audit before they can be activated.

Duplicates are harmless – 'buildMethodTables' uses @fromListWith
preferBetter@.
-}
expandSynonymMappings ::
    SynonymDB ->
    M.Map Text [BiosphereFlow] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
expandSynonymMappings synDB flowsByName mappings =
    mappings ++ concatMap expand mappings
  where
    expand (cf, _) =
        let dirDB = viewFor (mcfDirection cf) synDB
            peers = fromMaybe [] (getSynonyms dirDB =<< lookupSynonymGroup dirDB (mcfFlowName cf))
         in [ (cf, Just (flow, BySynonym))
            | syn <- peers
            , flow <- M.findWithDefault [] syn flowsByName
            ]

{- | Unmapped CFs whose name matches through the UNION synonym tables but not
through their own direction's view: the direction restriction alone stands
between them and a synonym match. Empty on untyped data, where the views
coincide. The typical cause is a parser that defaulted 'mcfDirection' (the
method carried no direction metadata), making a one-way bridge unreachable –
the loader surfaces these so the loss is distinguishable from a genuinely
uncharacterized flow, per the no-silent-misbehaviour rule.
-}
directionExcludedCFs ::
    CompartmentMap ->
    SynonymDB ->
    M.Map Text [BiosphereFlow] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [MethodCF]
directionExcludedCFs cmap synDB flowsByName mappings =
    [ cf
    | (cf, Nothing) <- mappings
    , isJust (matchIn synDB cf)
    , isNothing (matchIn (viewFor (mcfDirection cf) synDB) cf)
    ]
  where
    matchIn :: SynonymDB -> MethodCF -> Maybe BiosphereFlow
    matchIn db cf =
        findFlowBySynonymComp (SynonymSearch db flowsByName cmap) (mcfFlowName cf) (mcfCompartment cf)

{- | Project a region-tagged resource (withdrawal) flow onto its region's located
CF, in the GLOBAL name tables. An ILCD method whose CFs carry a consumer location
keeps them out of the broadcast tables – they reach 'mtRegionalizedCF', keyed by
the ACTIVITY location. But a resource flow that encodes its region in the NAME
(@"Water, river, FR"@) never reaches that table: the region survives
'normalizeName', so the synonym match to the unregioned base fails, and the
regional path keys by activity location, not the flow's own origin region.

For each such flow, if a located CF of the same substance (synonym group) and
medium carries that exact region, emit a GLOBAL mapping (location nulled) so the
flow resolves to its region's factor in 'mtExactCF'/'mtFallbackCF' – exactly as a
name-regionalized @"Water, river, FR"@ CF (the SimaPro convention) does today.

Restricted to the water dimensions – the resource (withdrawal/input) and water
(release/output) media; air and soil stay global so a method compared against an
unregionalized reference is left untouched there. A method with no located CF
in those media – all-unlocated (e.g. the name-regionalized SimaPro one) or
located only on air/soil – is left alone.

The flow's region resolves through a fallback chain: its exact located CF,
then the parent country's (@"CN-SC"@ → @"CN"@), then the method's own
location-less (world-average) factor. A sibling region's factor is never
borrowed. The generic step matters for sign correctness: ILCD AWARE tabulates
locations by ISO-2 country only, while inventory names also carry sub-national
codes and aggregates (@"RoW"@, @"GLO"@, @"WEU"@). Without it, the release side
of such a water balance is fully counted – the bare @"Water"@ release CF is
reachable by name at score time – while the withdrawal side of the same volume
scores zero (its base name only resolves through a synonym bridge, which the
score-time region fallback does not traverse), flipping the sign of net water
use.
-}
projectRegionalResourceFlows ::
    SynonymDB ->
    M.Map UUID BiosphereFlow ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
projectRegionalResourceFlows synDB bioFlows mappings =
    mappings ++ projected
  where
    flowMedium :: BiosphereFlow -> MediumKey
    flowMedium = fmap VT.compartmentName . VT.bfCompartment
    -- A projection key (group/name, medium, region) drops the subcompartment, so
    -- two located CFs of the same substance can land on one key. 'M.fromList' would
    -- then keep whichever came last in 'mappings' – order-dependent and silent.
    -- Keep the larger (more conservative, never-undercounting) factor instead, the
    -- same value preference 'buildMethodTables' applies through 'preferBetter'.
    preferHigherCF a b = if mcfValue a >= mcfValue b then a else b
    -- The synonym view a resource/water flow bridges through: a withdrawal
    -- (resource-medium) flow uses the input view, a release (water-medium) flow
    -- the output view, so an input-only bridge (@"river water"@ → @"Water,
    -- river"@) never lets a release flow inherit a withdrawal CF. On untyped data
    -- both views coincide, so this preserves today's grouping.
    dirView med = viewFor (if med == Just Water then Output else Input) synDB
    -- CFs reached two ways: by the matched flow's synonym GROUP – a CF whose
    -- name is bridged to the flow (withdrawal @"river water"@ → @"Water,
    -- river"@) – and by the CF's own NAME – a CF whose name equals the flow's
    -- region-stripped base (the bare @"Water"@ release CF → @"Water, FR"@).
    -- Keyed by 'Maybe' region: 'Just' entries are the located factors,
    -- 'Nothing' the method's own location-less (world-average) ones – the
    -- last step of the fallback chain.
    byGroup :: M.Map (Int, MediumKey, Maybe Text) MethodCF
    byGroup =
        M.fromListWith
            preferHigherCF
            [ ((grp, med, mcfConsumerLocation cf), cf)
            | (cf, Just (flow, _)) <- mappings
            , let med = flowMedium flow
            , Just grp <- [lookupSynonymGroup (dirView med) (bfName flow)]
            ]
    byName :: M.Map (Text, MediumKey, Maybe Text) MethodCF
    byName =
        M.fromListWith
            preferHigherCF
            [ ((normalizeName (mcfFlowName cf), cfMedium cf, mcfConsumerLocation cf), cf)
            | (cf, _) <- mappings
            ]
    -- @"CN-SC"@ → @"CN"@; a region without a hyphen has no parent.
    parentRegion loc = case T.breakOn "-" loc of
        (parent, rest) | not (T.null rest), not (T.null parent) -> Just parent
        _ -> Nothing
    -- Scope to the water dimensions only: the resource (withdrawal/input) and
    -- water (release/output) media. Excludes air/soil – acidification and PM
    -- carry located CFs too, but they must stay GLOBAL to match an unregionalized
    -- reference, so projecting their region-tagged flows would wrongly regionalize
    -- them.
    isWaterMedium med = med == Just NaturalResource || med == Just Water
    -- The walk runs only for a method with located CFs in the water media: a
    -- location on an air/soil CF alone must not open the water fallback, and a
    -- method whose CFs are all unlocated (the name-regionalized SimaPro
    -- convention) is left alone. The predicate short-circuits without building
    -- the maps, so most methods skip the flow walk for free.
    hasLocatedWaterCF =
        any (\(cf, _) -> isJust (mcfConsumerLocation cf) && isWaterMedium (cfMedium cf)) mappings
    projected
        | not hasLocatedWaterCF = []
        | otherwise =
            [ (cf{mcfConsumerLocation = Nothing}, Just (flow, BySynonym))
            | flow <- M.elems bioFlows
            , let med = flowMedium flow
            , isWaterMedium med
            , (base, Just loc) <- [extractLocationSuffix (bfName flow)]
            , let bname = normalizeName base
                  grp = lookupSynonymGroup (dirView med) base
                  atLoc l =
                    M.lookup (bname, med, l) byName
                        <|> (grp >>= \g -> M.lookup (g, med, l) byGroup)
            , Just cf <- [atLoc (Just loc) <|> (parentRegion loc >>= atLoc . Just) <|> atLoc Nothing]
            ]

-- No compartment filter here on purpose: 'buildMethodTables' keys
-- entries by the CF's compartment (after 'normalizeCompartment'), and
-- 'lookupCFForFlowAt' looks up by the inventory flow's compartment, so
-- mismatched (cf, peer) pairs simply land at keys nothing ever queries.
-- A pre-filter would have to mirror 'normalizeCompartment' to be
-- correct (e.g. ILCD's @land occupation@ medium → BAFU's
-- @resources/land@ via the compartment map), so it's simpler to let
-- the table keys do the filtering.

{- | Database flow indexes a @ProxyFor@ edge's @to@ side resolves against, keyed
the three ways a proxy can name it: normalized name, CAS, and flow UUID.
-}
data ProxyTargets = ProxyTargets
    { ptByName :: !(M.Map Text [BiosphereFlow])
    , ptByCAS :: !(M.Map Text [BiosphereFlow])
    , ptByUUID :: !(M.Map UUID BiosphereFlow)
    }

{- | Apply @ProxyFor@ edges at method-table build – the directional counterpart of
'expandSynonymMappings'. For each edge @from -(f)-> to@, every method CF identified
by @from@ contributes a CF scaled by @f@ to every database flow identified by @to@,
tagged 'ByProxy' so it keys under that flow's name and loses to any direct match.

Only 'SR.ProxyFor' edges act here; @SameAs@/@Subsumes@/@DistinctFrom@ live in the
class/diagnostics layer. An empty edge list is the identity, so the method tables are
unchanged when no @substance_edges.csv@ is loaded.
-}
expandProxyEdges ::
    ProxyTargets ->
    [SR.SubstanceEdge] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
expandProxyEdges targets edges mappings =
    mappings
        ++ [ (cf{mcfValue = mcfValue cf * f}, Just (toFlow, ByProxy))
           | (from, to, f) <- proxies
           , cf <- cfsMatching from
           , toFlow <- flowsMatching to
           ]
  where
    proxies =
        [ (SR.seFrom e, SR.seTo e, f)
        | e <- edges
        , SR.ProxyFor (SR.ConversionFactor f) <- [SR.seRelation e]
        ]

    cfs = map fst mappings
    cfsByName = M.fromListWith (++) [(normalizeName (mcfFlowName cf), [cf]) | cf <- cfs]
    cfsByCAS = M.fromListWith (++) [(cas, [cf]) | cf <- cfs, Just cas <- [mcfCAS cf], not (T.null cas)]
    cfsByUUID = M.fromListWith (++) [(mcfFlowRef cf, [cf]) | cf <- cfs]

    cfsMatching (SR.ByName _ (SR.NormName n)) = M.findWithDefault [] n cfsByName
    cfsMatching (SR.ByCAS (SR.CASNumber c)) = M.findWithDefault [] c cfsByCAS
    cfsMatching (SR.ByUUID (SR.FlowUUID u)) = M.findWithDefault [] u cfsByUUID

    flowsMatching (SR.ByName _ (SR.NormName n)) = M.findWithDefault [] n (ptByName targets)
    flowsMatching (SR.ByCAS (SR.CASNumber c)) = M.findWithDefault [] c (ptByCAS targets)
    flowsMatching (SR.ByUUID (SR.FlowUUID u)) = maybe [] pure (M.lookup u (ptByUUID targets))

{- | How a CF's own subcompartment met the flow's: by naming it, or by naming
none at all and standing as the method's medium-level default. Only
'mtRegionalizedCF' needs to tell the two apart, because it is the one table
where both compete for a single key.
-}
data SubMatch = ExactSub | MediumLevelSub
    deriving (Eq, Show)

{- | Cascade-order rank of a match strategy (UUID → name → synonym → CAS →
heuristic/expanded): when two CFs collide on one flow or table key, the lower
rank – the more discriminating match – wins. What the score tables actually
compare is 'entryPriority', which also ranks an entry no resolution reached.
-}
strategyPriority :: MatchStrategy -> Int
strategyPriority ByUUID = 0
strategyPriority ByName = 1
strategyPriority BySynonym = 2
strategyPriority ByCAS = 3
strategyPriority ByProxy = 4

{- | Cascade rank of a table entry. One no build-side resolution reached ranks
with 'ByProxy', the least discriminating match, so the two tie here and the
entry is chosen on the rungs below: the raw-name rank first, then the factor.

Exported so a consumer ranking entries outside this module reaches the same
verdict as the score tables do.
-}
entryPriority :: BuildProvenance -> Int
entryPriority = maybe (strategyPriority ByProxy) strategyPriority . bpStrategy

{- | The medium and subcompartment a characterization factor keys on, or
'Nothing' when it states no compartment, and 'Nothing' too when it states a
medium 'parseMedium' cannot place: such a factor is left out of the tables
rather than filed under a word no flow is ever looked up by.

The compartment-keyed tables 'buildMethodTables' fills are spelled in terms of
this – one derivation rather than one per table. (It still runs per table; what
is shared is the derivation, not its result. 'mtUuidCF' and 'mtRegionalizedCF'
key on the flow, not on this pair.)

The subcompartment is the half still spelled as its source wrote it, and this
and its read-path twin 'flowMediumSub' do not fold it. On a compartment map
whose target subcompartment column carries a capital – nothing forbids one, and
the shipped file simply has none – the two sides key differently and every
factor for that subcompartment goes silently unmatched. Closing that changes
which factors resolve, so it wants its own change and its own test rather than
a quiet edit here. The medium half is no longer exposed to it: both sides read
their word with 'parseMedium', which folds.
-}

{- | A subcompartment as the matchers compare them: case-folded and trimmed.

The lookup tables key on the subcompartment as its source spelled it, while
every predicate that classifies one ('isUnspecifiedSub', 'isForeignMediumSub')
folds first. That difference is older than this function and is not resolved
here – folding the table keys too would change which factors resolve, which
wants its own change and its own test.
-}
foldSub :: Subcompartment -> Subcompartment
foldSub (Subcompartment s) = Subcompartment (T.toLower (T.strip s))

unSub :: Subcompartment -> Text
unSub (Subcompartment s) = s

cfMediumSub :: CompartmentMap -> MethodCF -> Maybe (MediumKey, Subcompartment)
cfMediumSub cmap cf = do
    comp <- mcfCompartment cf
    let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
    medium <- mediumKey normMedRaw
    pure (Just medium, Subcompartment normSub)

buildMethodTables :: CFFamily -> CompartmentMap -> EnergyDensityMap -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> MethodTables
buildMethodTables methodFamily cmap energyDensities mappings =
    MethodTables
        { mtUuidCF =
            -- Non-regionalized rows only, like the name tables below: a
            -- location-specific row landing here would let one arbitrary
            -- location's value stand for the flow everywhere ('M.fromList'
            -- keeps the last row). Regionalized UUID-matched rows reach
            -- 'mtRegionalizedCF' keyed by flow UUID + location.
            M.fromList
                [ (bfId flow, entryOf cf mflow)
                | (cf, mflow@(Just (flow, ByUUID))) <- mappings
                , Nothing <- [mcfConsumerLocation cf]
                ]
        , mtUnitVariantCF =
            -- Keyed by the CF's OWN name with the unit suffix kept, so each
            -- per-unit row serves the flow declared in its unit – including a
            -- row whose build-time resolution went to a sibling variant (the
            -- collapsed-name pick is exactly what this table corrects). Only
            -- names that actually carry a suffix enter ('normalizeNameKeepUnit'
            -- differs from 'normalizeName'), keeping the table tiny.
            -- Subcompartment-blind like 'mtSubBlindCF', with the same
            -- 'agreedValue' veto: a variant name whose rows disagree (across
            -- subs or true duplicates) resolves nothing rather than guessing.
            M.mapMaybe agreedValue $
                M.fromListWith
                    (++)
                    [ ((SR.NormName rawName, medium), [entryOf cf mflow])
                    | (cf, mflow) <- mappings
                    , let rawName = normalizeNameKeepUnit (mcfFlowName cf)
                    , rawName /= normalizeName (mcfFlowName cf)
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just (medium, _) <- [cfMediumSub cmap cf]
                    ]
        , -- The broadcast name tables (exact + fallback) hold only
          -- non-regionalized CFs. Location-specific rows belong to
          -- 'mtRegionalizedCF' / 'mtRegionalCasCF'; letting them in here makes
          -- every location compete for one name key and 'preferBetter' crowns
          -- an arbitrary location's value (e.g. a region whose factor is 0
          -- silently erases the global credit).
          mtExactCF =
            dropRank $
                M.fromListWith
                    preferBetter
                    [ ((SR.NormName (nameKey cf mflow), medium, Subcompartment normSub), (entryOf cf mflow, rawNameMatches cf mflow))
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just (medium, Subcompartment normSub) <- [cfMediumSub cmap cf]
                    , not (T.null normSub)
                    ]
        , mtFallbackCF =
            -- Medium-level default CF for a flow whose specific subcompartment
            -- has no exact CF. Holds CFs whose own subcompartment is empty or
            -- "unspecified" – the value a method intends as the catch-all for
            -- that (name, medium). A flow at an uncovered subcompartment (e.g.
            -- a radionuclide emitted to "low population density, long-term",
            -- which EF leaves uncharacterized) thus picks up the unspecified CF,
            -- as ecoinvent's own implementation does, instead of scoring zero.
            -- This is safe against the long-term toxicity case: a substance
            -- that DOES carry a "(long-term)" CF keeps it in 'mtExactCF', which
            -- is consulted before this fallback, so long-term emissions still
            -- resolve to their explicit (often zero) factor.
            dropRank $
                M.fromListWith
                    preferBetter
                    [ ((SR.NormName (nameKey cf mflow), medium), (entryOf cf mflow, rawNameMatches cf mflow))
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just (medium, Subcompartment normSub) <- [cfMediumSub cmap cf]
                    , isUnspecifiedSub normSub
                    ]
        , mtLongTermFallbackCF =
            -- The long-term analogue of 'mtFallbackCF': the method's "unspecified
            -- (long-term)" catch-all. A long-term flow at an uncovered specific
            -- subcompartment ("groundwater, long-term") has no exact CF and must
            -- pick up THIS, not the immediate-emission default, so a delayed
            -- emission gets the method's own long-term factor (e.g. EF's explicit
            -- zero) rather than the full immediate CF.
            dropRank $
                M.fromListWith
                    preferBetter
                    [ ((SR.NormName (nameKey cf mflow), medium), (entryOf cf mflow, rawNameMatches cf mflow))
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just (medium, Subcompartment normSub) <- [cfMediumSub cmap cf]
                    , isLongTermUnspecifiedSub normSub
                    ]
        , mtSubBlindCF =
            -- Keep a (name, medium) entry only when every subcompartment's CF
            -- agrees: then the sub is irrelevant and a sub-mismatched flow can
            -- safely borrow it. A substance whose factor varies by sub is
            -- dropped here, so this never guesses across a real distinction.
            M.mapMaybe agreedValue $
                M.fromListWith
                    (++)
                    [ ((SR.NormName (nameKey cf mflow), medium), [entryOf cf mflow])
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just (medium, _) <- [cfMediumSub cmap cf]
                    ]
        , mtCasCF =
            -- Keyed by the CF's own CAS + medium (not by a matched flow), so the
            -- read path reaches every database flow sharing that CAS+medium –
            -- the fix for many flows collapsing onto one CAS (e.g. water).
            -- Subcompartment-blind on purpose: a resource flow and the CF that
            -- characterizes it routinely disagree on subcompartment, so a
            -- subcomp-strict bridge would zero whole resource categories.
            -- Regionalized CFs stay out, whichever way they carry their
            -- region: consumer-located rows go to 'mtRegionalCasCF', and
            -- name-regionalized rows ("Water, CH" – the SimaPro convention)
            -- are dropped via 'extractLocationSuffix'. The bridge is
            -- name-blind, so a region-specific row would collide with the
            -- region-less default on the one (CAS, medium) key and the
            -- magnitude tie-break would broadcast an arbitrary region's
            -- value (an arid ~100 over AWARE's region-less 42.95) to every
            -- uncovered same-CAS flow.
            --
            -- Only CFs that matched by CAS populate this table. A CF whose name
            -- or synonym pinned a specific flow (e.g. "methane (biogenic)" →
            -- "Methane, non-fossil") is name-discriminated: broadcasting it to
            -- every same-CAS flow would leak it onto a sibling the method
            -- distinguishes (fossil methane), so it stays out of the CAS bridge.
            --
            -- When several CFs share one (CAS, medium), broadcast the
            -- substance's medium-level default – its unspecified-subcompartment
            -- factor – rather than the largest. The largest is often a niche
            -- subcompartment (indoor air can be ~100x the outdoor value) that
            -- would over-characterize every same-CAS flow the bridge reaches.
            --
            -- And no bridge at all for a CAS class the method discriminates
            -- within ('casDiscriminated'): when rows at one (CAS, medium,
            -- subcompartment) carry different factor values – water is the
            -- canonical case, one CAS across regional name variants and
            -- deliberate exclusions like rain, ocean and turbined water – no
            -- single value is "the" factor for that CAS, and a name-blind
            -- bridge would stamp an arbitrary one (AWARE's region-less 42.95)
            -- onto exactly the flows the method chose to distinguish or leave
            -- out. Same never-guess rule as 'agreedValue'.
            (`M.withoutKeys` casDiscriminated) . M.map snd $
                M.fromListWith
                    (preferUnspecifiedBy teCF)
                    [ ((casNo, medium), (casSubRank normSub, entryOf cf mflow))
                    | (cf, mflow@(Just (_, ByCAS))) <- mappings
                    , Just cas <- [mcfCAS cf]
                    , Just casNo <- [SR.casKey cas]
                    , Nothing <- [mcfConsumerLocation cf]
                    , (_, Nothing) <- [extractLocationSuffix (mcfFlowName cf)]
                    , Just (medium, Subcompartment normSub) <- [cfMediumSub cmap cf]
                    ]
        , mtRegionalCasCF =
            -- Same unspecified-default preference as 'mtCasCF', applied per
            -- location: when several subcompartment CFs collide on one
            -- (CAS, medium, location) the medium-level default wins over a
            -- niche subcompartment, rather than the largest magnitude.
            --
            -- And the same 'casDiscriminated' veto: this table feeds the
            -- name-blind CAS fallback of the regionalized read path
            -- ('fillRegionalActivityWeights'), so serving a discriminated
            -- class here would hand an excluded flow (turbine water) the
            -- consuming activity's regional factor – and would regionalize
            -- a name-suffixed flow ("Water, lake, CH") by the consuming
            -- activity's location instead of the flow's own projected value.
            (`M.withoutKeys` casDiscriminated) . M.map (M.map snd) $
                M.fromListWith
                    (M.unionWith (preferUnspecifiedBy id))
                    [ ((casNo, medium), M.singleton (Location loc) (casSubRank normSub, cfOf cf))
                    | (cf, Just (_, ByCAS)) <- mappings
                    , Just cas <- [mcfCAS cf]
                    , Just casNo <- [SR.casKey cas]
                    , Just loc <- [mcfConsumerLocation cf]
                    , Just (medium, Subcompartment normSub) <- [cfMediumSub cmap cf]
                    ]
        , mtRegionalizedCF =
            -- Filter: a CF whose own compartment carries a specific subcomp
            -- (e.g. "groundwater, long-term" or "ocean") must only apply to
            -- flows in that exact subcomp – otherwise a CF=0 set explicitly for
            -- a niche subcomp leaks onto flows in other subcomps via
            -- ByName/synonym fan-out and clobbers the correct (unspecified)
            -- fallback CF. CFs with no specific subcomp ('isUnspecifiedSub')
            -- are wildcards and match any flow subcomp.
            M.map snd $
                M.fromListWith
                    preferRegional
                    [ ((bfId flow, Location loc), (m, cfOf cf))
                    | (cf, Just (flow, _)) <- mappings
                    , Just m <- [cfSubcompMatchesFlow cf flow]
                    , Just loc <- [mcfConsumerLocation cf]
                    ]
        , mtCFFamily = methodFamily
        , mtSeaWaterCFs = seaWaterCFs
        , mtCompartmentMap = cmap
        , mtEnergyDensities = energyDensities
        , mtResolution = M.empty -- filled alongside 'mtBroadcast'
        , mtJudged = S.empty -- filled alongside 'mtBroadcast'
        , mtBroadcast = M.empty -- fill via 'fillBroadcastVector' to enable the fast path
        , mtRegionalActivityWeights = Nothing -- fill via 'fillRegionalActivityWeights' for regional fast path
        }
  where
    cfOf cf = CF (mcfValue cf) (CFUnit (mcfUnit cf))

    entryOf cf mflow = TableEntry (cfOf cf) (BuildProvenance (snd <$> mflow) cf)

    -- The Bool rode along only for 'preferBetter''s raw-name rank.
    dropRank = M.map fst

    -- All subcompartments of a (name, medium) agree on the CF ⇒ the sub is
    -- irrelevant; keep that common value. Disagreement ⇒ Nothing (ambiguous).
    -- Agreement is judged on the CF alone; the surviving entry's provenance
    -- is the first row's – the values are equal, which method line authored
    -- the survivor is presentation detail.
    agreedValue vus = case vus of
        v : rest | all ((== teCF v) . teCF) rest -> Just v
        _ -> Nothing

    -- (CAS, medium) keys the CAS bridge must not serve: two rows agreeing on
    -- (CAS, medium, subcompartment) but not on the factor value prove the
    -- method distinguishes flows by something the name-blind bridge cannot
    -- see – a name-suffixed region ("Water, lake, CH" against the region-less
    -- "Water" default) or a plain name distinction (fossil against non-fossil
    -- methane). Every such row votes whatever it resolved to: an unmatched
    -- "Water, lake, AT" row still proves the method regionalizes water.
    -- Two deliberate non-voters: consumer-located rows, whose variance the
    -- regional tables dispatch by the flow's own location, and rows at
    -- \*different* subcompartments, whose variance 'preferUnspecifiedCas'
    -- already arbitrates to the medium-level default. The rule behind both:
    -- a value votes exactly when the method can serve it with no location
    -- attached. So located rows abstain only as such – against a database
    -- that writes its regions into flow names,
    -- 'projectRegionalResourceFlows' has already materialized them into
    -- region-less copies upstream, and those copies do vote: no location
    -- can dispatch the variance on that pairing, so both CAS bridges must
    -- refuse instead. Values compare without
    -- their unit: the read path unit-converts the bridged CF anyway, and
    -- pattern-expanded rows carry each flow's own unit, which is flow
    -- diversity, not authored discrimination.
    casDiscriminated =
        S.fromList
            [ (casKey, med)
            | ((casKey, med, _), vals) <- M.toList casValuesBySub
            , S.size vals > 1
            ]
    casValuesBySub =
        M.fromListWith
            S.union
            [ ((SR.CASNumber cas, medium, Subcompartment subClass), S.singleton (mcfValue cf))
            | (cf, _) <- mappings
            , Just cas <- [mcfCAS cf]
            , not (T.null cas)
            , Nothing <- [mcfConsumerLocation cf]
            , Just (medium, Subcompartment normSub) <- [cfMediumSub cmap cf]
            , let subClass = if isUnspecifiedSub normSub then "" else normSub
            ]

    -- For the CAS bridge: rank the unspecified / empty subcompartment ahead of
    -- any specific one, so 'preferUnspecifiedCas' keeps the medium-level
    -- default value when several subcompartment CFs collide on one key. On a
    -- tie (no unspecified CF present) keep the larger magnitude – the bridge is
    -- a last-resort fallback, so an overstated factor surfaces in validation
    -- while an understated one would be invisible.
    casSubRank s
        | isUnspecifiedSub s = 0 :: Int
        | otherwise = 1
    preferUnspecifiedBy val a@(ra, ea) b@(rb, eb)
        | ra /= rb = if ra < rb then a else b
        | abs (cfValue (val ea)) >= abs (cfValue (val eb)) = a
        | otherwise = b

    -- Counted from the method's own factor lines: does it name the sea
    -- anywhere? Both sides go through 'cmap', like every other subcompartment
    -- comparison here, so a compartments.csv rule that rewrites a spelling
    -- cannot make the count disagree with the gate that reads it.
    seaWaterCFs
        | any namesTheSea mappings = MethodDeclaresSeaWater
        | otherwise = MethodSilentOnSeaWater
    namesTheSea (cf, _) =
        maybe False (isForeignMediumSub . foldSub . snd) (cfMediumSub cmap cf)

    -- A CF whose subcomp names no specific subcompartment ('isUnspecifiedSub')
    -- is a wildcard. A CF with a specific subcomp must match the flow's subcomp
    -- exactly – otherwise an explicit-zero niche-subcomp CF would clobber the
    -- correct (unspecified) CF for flows in other subcomps via ByName/synonym
    -- fan-out.
    --
    -- Both sides go through 'normalizeCompartment' so a compartments.csv rule
    -- that rewrites a subcomp can't desynchronise the filter from the sibling
    -- 'mtExactCF' / 'mtFallbackCF' tables or the 'lookupCascadeCF' read path.
    -- Flow subcomp resolution mirrors 'lookupCascadeCF': prefer the explicit
    -- 'compartmentSub' field, fall back to the tail of "<medium>/<sub>"
    -- parsed from the compartment name.
    cfSubcompMatchesFlow cf flow = case mcfCompartment cf of
        Nothing -> Just MediumLevelSub
        Just _ ->
            let !cfSubN = maybe T.empty (unSub . foldSub . snd) (cfMediumSub cmap cf)
                !flowSubN = unSub (foldSub (snd (flowMediumSub cmap flow)))
             in -- A wildcard (unspecified) CF matches any subcompartment except
                -- the ones the 'lookupCascadeCF' gate excludes on the
                -- non-regional path – both tiers: a foreign medium (sea/ocean)
                -- never borrows a freshwater CF, and long-term groundwater
                -- borrows no surface USEtox CF. An explicit same-sub CF still
                -- matches, and says so, because the two are ranked against each
                -- other when both land on one (flow, location).
                subMatch cfSubN flowSubN

    subMatch cfSubN flowSubN
        | cfSubN == flowSubN = Just ExactSub
        | isUnspecifiedSub cfSubN
        , wildcardReachesSub methodFamily seaWaterCFs (Subcompartment flowSubN) =
            Just MediumLevelSub
        | otherwise = Nothing

    -- Rank two CFs competing for one (flow, location) key. 'mtRegionalizedCF' is
    -- the only table where an exact-subcompartment CF and a medium-level one
    -- compete at all: 'mtExactCF' and 'mtFallbackCF' are kept apart by
    -- construction and the read cascade orders them. Here both land on the same
    -- key, so without a rank the winner is whichever row the method file listed
    -- last. The method's more specific line wins; between two of the same kind,
    -- the larger factor, as everywhere else in this module.
    preferRegional a@(mA, CF va _) b@(mB, CF vb _)
        | mA /= mB = if mA == ExactSub then a else b
        | abs va >= abs vb = a
        | otherwise = b

    -- Rank colliding CFs for one name key: better match strategy first, then a
    -- CF whose own (raw) flow name equals the matched DB flow's name – when
    -- normalization collapses unit-suffixed homonyms ("Gas, natural/kg" and
    -- "Gas, natural/m3" both normalize to "gas, natural"), the row that matched
    -- verbatim carries the unit the flow is actually declared in; the other
    -- variant is dimensionally incompatible and would silently convert to 0.
    -- Only then the historical higher-value tie-break.
    preferBetter a@(ea, r1) b@(eb, r2)
        | p1 < p2 = a
        | p1 > p2 = b
        | r1 /= r2 = if r1 then a else b
        | cfValue (teCF ea) >= cfValue (teCF eb) = a
        | otherwise = b
      where
        p1 = entryPriority (teProvenance ea)
        p2 = entryPriority (teProvenance eb)

    rawNameMatches cf mflow = case mflow of
        Just (flow, _) -> T.toLower (T.strip (mcfFlowName cf)) == T.toLower (T.strip (bfName flow))
        Nothing -> False

    -- Use matched flow's name only for name/synonym/proxy matches: those key
    -- the CF under the database flow it resolved to, not the method CF's own name.
    nameKey cf mflow = normalizeName $ case mflow of
        Just (flow, ByName) -> bfName flow
        Just (flow, BySynonym) -> bfName flow
        Just (flow, ByProxy) -> bfName flow
        _ -> mcfFlowName cf

{- | How a flow quantity reached the basis its CF value is denominated in.
The score only needs the converted number; this says which route produced it,
so a surface can explain a factor instead of restating it.
-}
data UnitBridge
    = -- | Flow and CF share a unit, or one of the two declares none.
      UnitsIdentical
    | {- | The flow's unit is not in the unit configuration, so the quantity
      passes through unconverted – there is no base to normalize it to.
      -}
      UnitUnknown !Text
    | -- | Ordinary same-dimension conversion, flow unit → CF unit.
      UnitConverted !Text !Text
    | {- | The CF unit is a result expression (@"kg CO2 eq"@), so the factor is
      defined per the flow's canonical base unit and the quantity is scaled
      to that base.
      -}
      NormalizedToBase !Text
    | {- | Flow and CF sit on either side of a physical density, which bridges
      the two dimensions (see 'energyAwareConversion').
      -}
      EnergyBridged !EnergyDensity !DensityDirection
    deriving (Eq, Show)

-- | Which way a density was read: @qNative × E@ or @qTarget ÷ E@.
data DensityDirection = DensityForward | DensityInverse
    deriving (Eq, Show)

{- | Why a flow quantity could not be brought to its CF's basis. Refusing is
right – wrong-dimension data must not score – but the refusal is a fact worth
reporting, not a bare zero.
-}
data RefusalReason
    = {- | Flow unit and CF unit measure different dimensions, and no density
      bridges them.
      -}
      DimensionalMismatch !Text !Text
    | {- | The flow's dimension declares no canonical base unit, so a CF in a
      result expression has no basis to normalize to (a unit-config defect).
      -}
      NoCanonicalBase !Text
    | -- | A density applies but one leg of the bridge would not convert.
      EnergyBridgeRefused !EnergyDensity
    deriving (Eq, Show)

{- | What the read cascade decided for one flow, recorded when the broadcast
vector is filled so no surface has to re-derive it in bulk.
-}
data FlowCFTag = FlowCFTag
    { ftRung :: !RungId
    -- ^ The cascade rung that served this flow's factor.
    , ftRefusal :: !(Maybe RefusalReason)
    {- ^ Set when the factor exists but could not be converted onto the flow's
    basis, so the flow scores 0 while looking characterized.
    -}
    }
    deriving (Eq, Show)

-- | A flow quantity on its CF's basis, or the reason it could not get there.
data ConversionOutcome
    = Converted !Double !UnitBridge
    | Unconvertible !RefusalReason
    deriving (Eq, Show)

{- | The number scoring uses. A refusal contributes @0@ – the deliberate
"never score wrong-dimension data" rule – and the reason it was refused stays
in the 'ConversionOutcome' for the surfaces that report it.
-}
convertedQuantity :: ConversionOutcome -> Double
convertedQuantity (Converted q _) = q
convertedQuantity (Unconvertible _) = 0
{-# INLINE convertedQuantity #-}

{- | Convert the inventory @qty@ (in @flowUnit@) to the basis the CF value
expects, for characterization.

  * Units match, or the flow carries no unit → @qty@ unchanged.
  * Both units known to the 'UnitConfig' → ordinary same-dimension conversion,
    read onto the basis the CF is written per, so a factor in
    @"m3-world equivalents"@ is reached by a flow in litres
    ('convertOntoFactorBasis'); a dimensional mismatch (flow @m@, CF @kg@)
    hard-fails to @0@ rather than injecting wrong-dimension data into the score.
  * The CF unit is a result expression unknown to the 'UnitConfig' (e.g.
    @"kg CO2 eq"@ – the common ILCD/EF case, where 'mcfUnit' carries the impact
    unit, not the flow's reference unit) → the CF value is defined per the
    flow's canonical base unit, so we normalize @qty@ to that base unit
    ('normalizeToCanonical'). A flow already in its base unit (kg) is left as
    is; a flow in @g@/@mg@ is scaled to kg. Without this, grams would be
    characterized as if they were kilograms (a ×1000 / ×1e6 over-count). If the
    flow's dimension defines no canonical base (a 'UnitConfig' defect),
    'normalizeToCanonical' returns 'Nothing' and we hard-fail to @0@ – as in the
    dimensional-mismatch case – rather than silently scoring the un-normalized
    amount.
  * The flow unit itself is unknown → @qty@ unchanged (no base to normalize to).
-}
convertForCharacterization :: UnitConfig -> Text -> CFUnit -> Double -> Double
convertForCharacterization cfg flowUnit cfu = convertedQuantity . characterizationOutcome cfg flowUnit cfu
{-# INLINE convertForCharacterization #-}

-- | 'convertForCharacterization', saying how it converted or why it refused.
characterizationOutcome :: UnitConfig -> Text -> CFUnit -> Double -> ConversionOutcome
characterizationOutcome cfg flowUnit (CFUnit cfu) qty
    | flowUnit == cfu || T.null cfu || T.null flowUnit = Converted qty UnitsIdentical
    | not (isKnownUnit cfg flowUnit) = Converted qty (UnitUnknown flowUnit)
    | isKnownUnit cfg cfu =
        maybe
            (Unconvertible (DimensionalMismatch flowUnit cfu))
            (`Converted` UnitConverted flowUnit cfu)
            (convertOntoFactorBasis cfg flowUnit cfu qty)
    | otherwise =
        maybe
            (Unconvertible (NoCanonicalBase flowUnit))
            (\(base, q) -> Converted q (NormalizedToBase base))
            (normalizeToCanonical cfg flowUnit qty)

{- | Pre-compute the broadcast CF Map: each flow UUID covered by the method maps
to its effective CF (CF value × flow-unit→CF-unit conversion). Collapses the
UUID/exact/fallback cascade into a single Map and absorbs the unit conversion
so the scoring hot path becomes pure multiply-accumulate.

Walks every flow in @flowDB@ once. Flows with no CF match are not inserted
(sparse Map). Conversions route through 'flowToCFOutcome', so
dimensionally-incompatible flow/CF unit pairs land an effective CF of @0@
(matching the per-flow scoring path) rather than silently keeping the
unconverted quantity and contaminating the score.

The same walk fills 'mtResolution', so the rung that served each flow and any
conversion refusal survive the collapse into one 'Double' – at no extra pass,
because both answers come from the one cascade lookup this already does.
-}
fillBroadcastVector :: UnitConfig -> UnitDB -> BioFlowDB -> MethodTables -> MethodTables
fillBroadcastVector unitConfig unitDB flowDB tables =
    tables
        { mtBroadcast = M.map fst resolved
        , mtResolution = M.map snd resolved
        , mtJudged = M.keysSet flowDB
        }
  where
    resolved = M.mapMaybeWithKey buildEntry flowDB
    buildEntry fid flow = do
        (rung, entry) <- lookupCascadeEntry tables flowDB fid
        let CF cfVal cfu = teCF entry
            outcome = flowToCFOutcome unitConfig unitDB (mtEnergyDensities tables) (Just flow) cfu 1.0
        pure (convertedQuantity outcome * cfVal, FlowCFTag rung (refusalOf outcome))
    refusalOf (Converted _ _) = Nothing
    refusalOf (Unconvertible reason) = Just reason

{- | Matched CFs the flow's own unit cannot be converted to, although the
factor itself is nonzero: a dimensional mismatch, a missing canonical base, or
a failed energy bridge. The refusal itself is right – wrong-dimension data
must not score – but left unreported it is indistinguishable from an
uncharacterized flow, and the method silently undercounts. Callers surface
these once per (db, method) at build time, with the reason.

Covers both read paths: the broadcast vector (read straight off
'mtResolution', which recorded each refusal as it happened) and the
regionalized CF table (one representative CF per flow – whether a conversion
is refused depends on the units, not on the per-location value). The
name-blind regional CAS bridge is not scanned: it has no fixed flow to
convert against until scoring. One entry per flow.
-}
zeroedMatchedCFs :: UnitConfig -> UnitDB -> BioFlowDB -> MethodTables -> [(BiosphereFlow, CF, RefusalReason)]
zeroedMatchedCFs unitConfig unitDB flowDB tables =
    M.elems (M.union broadcastZeroed regionalZeroed)
  where
    broadcastZeroed =
        M.fromList
            [ (fid, (flow, teCF entry, reason))
            | (fid, tag) <- M.toList (mtResolution tables)
            , Just reason <- [ftRefusal tag]
            , Just flow <- [M.lookup fid flowDB]
            , Just (_, entry) <- [lookupCascadeEntry tables flowDB fid]
            , cfValue (teCF entry) /= 0
            ]
    regionalZeroed =
        M.fromList
            [ (fid, (flow, cf, reason))
            | (fid, cf) <- M.toList regionalRep
            , Just flow <- [M.lookup fid flowDB]
            , Unconvertible reason <-
                [flowToCFOutcome unitConfig unitDB (mtEnergyDensities tables) (Just flow) (cfUnit cf) 1.0]
            ]
    regionalRep =
        M.fromList
            [ (fid, cf)
            | ((fid, _), cf) <- M.toList (mtRegionalizedCF tables)
            , cfValue cf /= 0
            ]

{- | Precompute per-activity-column contributions for a regionalized method.

This is the regionalized analogue of 'fillBroadcastVector': it walks the
'Database' biosphere triples ONCE and stores, per matrix column @a@,

  @w[a] = Σ_f B[f,a] · CF(f, loc(a)) · unit-conversion(f)@

So any later regionalized score reduces to @w · s_k@ – one dot product per
pid instead of one biosphere-triple walk per pid. For agribalyse this
trades 632 walks of ~50K triples for 632 dot products over 21K activities.

CF lookups follow the same hierarchical fallback as 'resolveRegionalCF'
(exact → parents → universal broadcast). When a regionalized flow has no CF
for that activity's location even after walking parents, the activity is
marked tainted ('rawTainted[a] = 1') and the missing @(flow, location)@
pair is accumulated in 'rawMissingPairs' for deduplicated warning emission
by the caller – instead of one warning per pid × per method × per missing CF
under the old per-call path.

Score-time semantics: 'computeRegionalizedLCIAScore' returns a partial
score that under-counts tainted activities (matching the broadcast path's
silent-omission behaviour for non-regio flows, and SimaPro). The coverage
gap is surfaced once via 'rawMissingPairs' + the build-time WARN, not by
'Left'-ing the whole method – that contract used to be strict but masked
every other column's valid contribution as soon as one (flow, location)
pair was uncovered.

No-op when 'mtRegionalizedCF tables' is empty – non-regionalized methods
keep 'mtRegionalActivityWeights = Nothing' so the broadcast fast path stays
the right answer.
-}
fillRegionalActivityWeights ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Database ->
    M.Map Location [Location] ->
    MethodTables ->
    MethodTables
fillRegionalActivityWeights unitCfg unitDB flowDB db hier tables
    | M.null (mtRegionalizedCF tables) = tables
    | otherwise = tables{mtRegionalActivityWeights = Just precomputed}
  where
    nCols = fromIntegral (dbActivityCount db) :: Int
    actIdx = dbActivityIndex db
    activities = dbActivities db
    bioFlows = dbBiosphereOrder db
    bioTriples = dbBiosphereTriples db
    regional = mtRegionalizedCF tables

    -- Flow-row-indexed view of 'regional'. For each biosphere flow row index
    -- @r@, @regionalByRow V.! r@ is @Just locMap@ when that flow has any
    -- regionalized CF, @Nothing@ otherwise. Most biosphere flows in a
    -- typical inventory are not regionalized, so the hot loop's outer test
    -- becomes a single 'V.!' + Nothing match instead of two
    -- @M.lookup (UUID, Text)@ walks (exact + parents) against the big shared
    -- @(UUID, Text)@-keyed map. Subsumes the old
    -- @regionalizedFlows :: Set UUID@ check – @Just _@ here is the
    -- "this flow is regionalized" signal needed at the taint branch.
    regionalByRow :: V.Vector (Maybe (M.Map Location CF))
    regionalByRow =
        let perFlow :: M.Map UUID (M.Map Location CF)
            perFlow =
                M.fromListWith
                    M.union
                    [(f, M.singleton loc cf) | ((f, loc), cf) <- M.toList regional]
            regionalCas = mtRegionalCasCF tables
            cmap = mtCompartmentMap tables
            -- Direct flow→locMap, then fall back to the flow's own CAS+medium so
            -- every flow sharing a CAS is regionalized per location – not just
            -- the one a CF resolved to at build time.
            lookupRow fid =
                M.lookup fid perFlow
                    <|> ( M.lookup fid flowDB >>= \flow ->
                            bfCAS flow
                                >>= SR.casKey
                                >>= \casNo -> M.lookup (casNo, fst (flowMediumSub cmap flow)) regionalCas
                        )
         in V.map lookupRow bioFlows

    -- ProcessId → matrix column index → activity's reference location.
    -- Built once (O(nActivities)) and indexed by column inside the hot loop.
    colLoc :: V.Vector Location
    colLoc =
        V.replicate nCols (Location T.empty)
            V.// [ (fromIntegral (actIdx V.! pid), Location (activityLocation (activities V.! pid)))
                 | pid <- [0 .. V.length actIdx - 1]
                 , let !col = fromIntegral (actIdx V.! pid) :: Int
                 , col >= 0
                 , col < nCols
                 ]

    -- Cached locally so the hot loop reads from one stable pointer per field.
    broadcast = mtBroadcast tables

    -- Walk biosphere triples once; build weights, tainted flags and the
    -- deduplicated missing-(flow, location) set in a single ST action.
    --
    -- The CF cascade is inlined here so the universal-fallback branch can
    -- read from 'mtBroadcast' (pre-multiplied by unit conversion at
    -- 'fillBroadcastVector' time) instead of re-running 'lookupCascadeCF'
    -- which calls 'normalizeName' per triple. Profiling on EF31-biomaps ×
    -- agribalyse showed 'normalizeName' eating ~83% of fillRegional time
    -- across 90M biosphere triples; the broadcast map already has the
    -- answer the cascade was recomputing.
    --
    -- The parent-region fallback uses a manual short-circuit recursion
    -- ('lookupParents') rather than @firstJust [M.lookup .. | p <- parents]@:
    -- the list comprehension materialised a cons cell per parent per triple,
    -- accounting for ~30% of warmup heap allocations on EF31-biomaps. The
    -- manual recursion stops at the first hit and allocates nothing.
    precomputed :: RegionalActivityWeights
    precomputed = runST $ do
        ws <- MU.replicate nCols (0 :: Double)
        ts <- MU.replicate nCols (0 :: Word8)
        missRef <- newSTRef (Set.empty :: Set.Set (UUID, Location))
        U.forM_ bioTriples $ \(SparseTriple flowRow colIdx bioVal) -> do
            let !col = fromIntegral colIdx :: Int
                !row = fromIntegral flowRow :: Int
                !flowUUID = bioFlows V.! row
                applyRaw cf =
                    let !contribution =
                            convertAndMultiply
                                unitCfg
                                unitDB
                                (mtEnergyDensities tables)
                                (M.lookup flowUUID flowDB)
                                cf
                                bioVal
                     in MU.unsafeModify ws (+ contribution) col
                -- 'mtBroadcast' is the unit-converted CF per unit of flow, so
                -- the contribution is @bioVal * preMultipliedCF@ – same
                -- algebra as 'computeLCIAScoreFromTables's fast path.
                applyBroadcast = case M.lookup flowUUID broadcast of
                    Just preMultipliedCF ->
                        MU.unsafeModify ws (+ bioVal * preMultipliedCF) col
                    Nothing -> pure ()
            case regionalByRow V.! row of
                -- Common case: flow is not regionalized at all. No exact /
                -- parent walk; just broadcast (universal CF) if present.
                Nothing -> applyBroadcast
                Just locMap ->
                    let !loc = colLoc V.! col
                        -- Walk the parent locations until one yields a CF.
                        -- Allocates no list cells – replaces
                        -- @firstJust [.. | p <- parents]@.
                        lookupParents [] = Nothing
                        lookupParents (p : ps) = case M.lookup p locMap of
                            Just cf -> Just cf
                            Nothing -> lookupParents ps
                     in case M.lookup loc locMap of
                            Just cf -> applyRaw cf
                            Nothing -> case lookupParents (M.findWithDefault [] loc hier) of
                                Just cf -> applyRaw cf
                                Nothing -> case M.lookup flowUUID broadcast of
                                    Just preMultipliedCF ->
                                        MU.unsafeModify ws (+ bioVal * preMultipliedCF) col
                                    Nothing -> do
                                        -- Flow IS regionalized (locMap is
                                        -- @Just _@) but has no CF for this
                                        -- location even after parents and
                                        -- no universal broadcast – taint.
                                        MU.unsafeWrite ts col 1
                                        modifySTRef' missRef (Set.insert (flowUUID, loc))
        wsF <- U.unsafeFreeze ws
        tsF <- U.unsafeFreeze ts
        miss <- readSTRef missRef
        pure
            RegionalActivityWeights
                { rawWeights = wsF
                , rawTainted = tsF
                , rawMissingPairs = Set.toAscList miss
                }

{- | Score an inventory against precomputed 'MethodTables'.
Hot path: O(|inventory|) per call, no map construction.

Returns 'LCIAOutcome' carrying the score plus characterized-vs-total
inventory mass (so callers can detect tail erosion when many small flows
go uncharacterized).

Uses 'mtBroadcast' (single Map lookup, conversion pre-multiplied) when filled,
falling back to the legacy UUID→exact→fallback cascade with on-the-fly unit
conversion when 'mtBroadcast' is empty. Tests and back-compat callers that use
'buildMethodTables' directly hit the legacy path; the cached
'mapMethodToTablesCached' fills the broadcast and gets the fast path.
-}
computeLCIAScoreFromTables :: UnitConfig -> UnitDB -> BioFlowDB -> Inventory -> MethodTables -> LCIAOutcome
computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables =
    let (!score, !charSum, !invSum) = M.foldlWithKey' step (0, 0, 0) inventory
     in LCIAOutcome
            { loScore = score
            , loCharacterizedSum = charSum
            , loInventoryAbsSum = invSum
            , loUncharacterized = []
            , loUnknownUuids = []
            }
  where
    useFast = not (M.null (mtBroadcast tables))

    step (!s, !cs, !is) fid qty
        | qty == 0 = (s, cs, is)
        | otherwise =
            let !absQty = abs qty
                !is' = is + absQty
             in case scoreFlow fid qty of
                    Nothing -> (s, cs, is')
                    Just contribution -> (s + contribution, cs + absQty, is')

    scoreFlow fid qty
        | useFast = case M.lookup fid (mtBroadcast tables) of
            Just cf -> Just (qty * cf)
            -- Inventory may reference flows not in the broadcast (e.g. cross-DB
            -- merged flows beyond the original flowDB at build time): fall back
            -- to the cascade so we don't silently drop them.
            Nothing -> legacyScoreFlow fid qty
        | otherwise = legacyScoreFlow fid qty

    legacyScoreFlow fid qty = case lookupCascadeCF tables flowDB fid of
        Nothing -> Nothing
        Just cf -> Just (convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) (M.lookup fid flowDB) cf qty)

{- | Back-compat wrapper: build tables on the fly, with no compartment map,
energy densities, or CF-family gating ('OtherCFFamily'). Prefer the cached path
('mapMethodToTablesCached' + 'computeLCIAScoreFromTables') in hot loops, and
'buildMethodTables' with the method's real 'cfFamily' wherever the method is
in hand.
-}
computeLCIAScore :: UnitConfig -> UnitDB -> BioFlowDB -> Inventory -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> LCIAOutcome
computeLCIAScore unitConfig unitDB flowDB inventory mappings =
    computeLCIAScoreFromTables unitConfig unitDB flowDB inventory (buildMethodTables OtherCFFamily M.empty M.empty mappings)

{- | LCIA score with automatic dispatch.

If the method has no regionalized CFs ('mtRegionalizedCF' empty), uses the
classic vector path ('computeLCIAScoreFromTables'). Otherwise switches to
the matrix path ('computeRegionalizedLCIAScore').

The caller is expected to provide both an 'Inventory' (cheap if already
computed for other purposes) and a scaling 'Vector' (cheap if the MUMPS
factorization is cached). Pass them both and let this function pick.

Returns 'Either' so the regionalized path can surface integrity errors
(scaling/weights length mismatch, weights absent) explicitly. Coverage
gaps are NOT surfaced here – they appear once at table-build time as a
WARN listing the uncovered (flow, location) pairs; score-time returns
the partial 'Right'.
-}
computeLCIAScoreAuto ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Database ->
    -- | Scaling vector @s@ (only consulted if the method is regionalized)
    Vector ->
    -- | Pre-computed inventory @g = B · s@ (only used in the classic path)
    Inventory ->
    -- | Location hierarchy: child → ordered list of parents
    M.Map Location [Location] ->
    MethodTables ->
    Either Text Double
computeLCIAScoreAuto unitCfg unitDB flowDB db scalingVec inventory hier tables
    | M.null (mtRegionalizedCF tables) =
        Right (loScore (computeLCIAScoreFromTables unitCfg unitDB flowDB inventory tables))
    | otherwise =
        computeRegionalizedLCIAScore unitCfg unitDB flowDB db scalingVec hier tables

{- | Streaming regionalized LCIA score over the biosphere matrix.

@score = Σ_{(f, a)} B[f, a] · s[a] · C[f, loc(a)]@

Where @C[f, l]@ is resolved by hierarchical fallback:

  1. Exact regional cell @(f, l)@ in 'mtRegionalizedCF'.
  2. Cell at any parent region of @l@ (walked via the location hierarchy).
  3. Universal broadcast: the same lookup that 'computeLCIAScoreFromTables' uses
     ('mtUuidCF' / 'mtExactCF' / 'mtFallbackCF').
  4. If none of the above and @f@ is regionalized in this method (i.e. the
     regional table mentions @f@ for some other location), fail with a 'Left'
     surfacing the gap – silent zero would under-count.
  5. If @f@ is not covered at all by the method, contribute 0.

The hierarchy is the same one used by 'Database.CrossLinking.isSubregionOf':
@Map ChildLocation [ParentLocation]@ from broader to broadest.

This path is selected by 'Service' when 'mtRegionalizedCF' is non-empty;
non-regionalized methods continue to use 'computeLCIAScoreFromTables'.
-}
computeRegionalizedLCIAScore ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Database ->
    -- | Scaling vector @s@ from 'Matrix.computeScalingVector'
    Vector ->
    -- | Location hierarchy: child → ordered list of parents
    M.Map Location [Location] ->
    MethodTables ->
    Either Text Double
computeRegionalizedLCIAScore unitConfig unitDB flowDB db scalingVec _hier tables =
    case mtRegionalActivityWeights tables of
        Just raw -> scoreFromPrecomputed raw scalingVec
        Nothing
            -- Cross-DB scoring passes per-DB tables in; a dep DB whose flow
            -- mappings caught none of this method's regional CFs has empty
            -- 'mtRegionalizedCF', so 'fillRegionalActivityWeights' left
            -- 'mtRegionalActivityWeights' unfilled. Its emissions are still
            -- this method's business – they just all go through the broadcast
            -- tables – so score its own slice flat instead of contributing a
            -- zero that reads exactly like a database with nothing to say.
            -- Unlike the fast path this walks the DB's biosphere triples once
            -- per score call; precompute it the way
            -- 'fillRegionalActivityWeights' does if a batch ever makes it hot.
            | M.null (mtRegionalizedCF tables) ->
                Right
                    ( loScore
                        (computeLCIAScoreFromTables unitConfig unitDB flowDB (applyBiosphereMatrix db scalingVec) tables)
                    )
            | otherwise ->
                Left
                    "Regionalized score requested but precomputed activity weights\
                    \ are absent. Call 'fillRegionalActivityWeights' on the\
                    \ MethodTables before scoring (mapMethodToTablesCached does\
                    \ this automatically)."
  where
    -- Fast path: one dot product over precomputed per-column weights.
    -- Tainted columns contribute 0 by construction in
    -- 'fillRegionalActivityWeights' (weights[i] == 0 when no CF matched),
    -- so summing them yields the correct partial score. The coverage gap is
    -- surfaced once at table-build time via 'rawMissingPairs' + the WARN
    -- in 'Database.Manager' – not by collapsing the whole method to a
    -- 'Left', which forced every category with even one uncovered
    -- (flow, location) pair to 0 µPt and masked the partial score.
    -- Matches SimaPro behaviour.
    --
    -- 'Left' is reserved here for genuine integrity errors (length mismatch
    -- below; "weights absent" in the outer 'case'), not coverage gaps.
    scoreFromPrecomputed raw s =
        let !weights = rawWeights raw
            !n = U.length weights
            !sLen = U.length s
         in if sLen /= n
                then
                    Left $
                        "Regionalized score: scaling/weights length mismatch ("
                            <> T.pack (show sLen)
                            <> " vs "
                            <> T.pack (show n)
                            <> "). Activity index and precomputed weights are built from the same database – this means the cache is stale or the wrong tables were paired."
                else
                    let go !i !acc
                            | i >= n = acc
                            | otherwise =
                                let !sv = U.unsafeIndex s i
                                 in if sv == 0
                                        then go (i + 1) acc
                                        else go (i + 1) (acc + sv * U.unsafeIndex weights i)
                        !score = go 0 0
                     in Right score

{- | Cross-DB regionalized LCIA score.

For each participating database @d@ (root + each dep DB reached at request
time), call 'computeRegionalizedLCIAScore' against THAT DB's scaling
vector and MethodTables, then sum the per-DB scores. Equivalent to one
dot product over the concatenated activity space
@[s_root, s_dep1, …] · [w_root, w_dep1, …]@ – just computed per-DB so
each side keeps its own activity index, hierarchy walks and tainted-column
diagnostics local.

Closes the gap where dep-DB biosphere emissions are present in the merged
inventory but invisible to the regional dot product (which was previously
keyed on the root DB's activity columns alone).

Coverage gaps no longer show up here: 'computeRegionalizedLCIAScore'
returns 'Right' with tainted columns contributing 0, so an incomplete-
coverage DB just contributes its partial score to the sum. The build-time
WARN + 'rawMissingPairs' per @(db, method)@ is the single source of truth
for what's missing – score time stays quiet.

A per-DB 'Left' is a genuine integrity error (scaling/weights length
mismatch, weights absent on a regionalized method – never a coverage gap)
and fails the whole sum: dropping that DB to a 0 contribution would return
a silently undercounted total the consumer cannot tell from a real score.
The concatenated per-DB error messages are surfaced so the caller can act
on them.
-}
sumRegionalizedLCIAScoreCrossDB ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    M.Map Location [Location] ->
    {- | Per-DB triples: one per database participating in the cross-DB solve
    (root + dep DBs in the same order returned by 'SharedSolver.csScalings').
    -}
    [(Database, Vector, MethodTables)] ->
    Either Text Double
sumRegionalizedLCIAScoreCrossDB unitCfg unitDB flowDB hier triples =
    let results = [computeRegionalizedLCIAScore unitCfg unitDB flowDB db sv hier t | (db, sv, t) <- triples]
     in case lefts results of
            [] -> Right (sum (rights results))
            es -> Left (T.intercalate "; " es)

{- | Cascade CF lookup: UUID → exact (name, medium, subcomp) → fallback (name, medium).
The same logic is baked into 'mtBroadcast' once unit conversion is available;
this helper is the source of truth for the legacy / cross-DB / regionalized
fallback paths that don't go through the broadcast.

Both the CF table keys (built by 'buildMethodTables') and the inventory flow
compartments are normalized through @tables.'mtCompartmentMap'@ so each side
converges on the same canonical form – a compartments.csv rule like
@"Emissions to air,,,air,,"@ then bridges BAFU-style prefixed compartments
against ILCD-style bare media without requiring an explicit (medium, sub)
pair for every combination.
-}

-- | One rung of the read-time cascade, in the order 'cascadeTrail' tries them.
data RungId
    = RungUuid
    | RungUnitVariant
    | RungExactName
    | RungLongTermDefault
    | RungMediumDefault
    | RungCasBridge
    | RungSubBlind
    | RungRegionBase
    | RungEnergyResource
    | RungOreGradeBase
    deriving (Eq, Show, Enum, Bounded)

{- | Why a wildcard rung refused a flow's subcompartment – the two tiers of
'wildcardVeto'.
-}
data VetoReason = ForeignMediumVeto | LongTermUSEtoxVeto
    deriving (Eq, Show)

-- | One rung's verdict about one flow.
data RungOutcome
    = RungHit !TableEntry
    | -- | The rung's table holds nothing for this flow.
      RungMiss
    | {- | The rung's precondition on the flow itself fails (not a long-term
      emission, no CAS, no region or density suffix, not an ore-grade
      resource, no unit-suffixed rows in the method): nothing is looked up.
      -}
      RungNotApplicable
    | {- | A wildcard rung the flow's subcompartment refuses. Whether an entry
      sat behind the veto is deliberately not recorded: no surface reports
      it, and not looking is what keeps a vetoed rung free on the score path.
      -}
      RungVetoed !VetoReason
    | {- | Several candidates disagree and the rung refuses to pick one by Map
      order (energy-family disagreement) – the "never guess" rule.
      -}
      RungAmbiguous
    deriving (Eq, Show)

{- | Every rung's verdict for one flow, in cascade order. Lazy in each
outcome: 'lookupCascadeEntry' forces outcomes only until the first hit – the
same work as the previous '<|>' chain – while an explanation may force the
whole trail.

Rung order, and why: a unit-suffixed flow first tries the method row declared
in its own unit ('mtUnitVariantCF') – the collapsed-name tables below crown
one winner per base name, which for a sibling unit variant is dimensionally
wrong and zeroes on conversion; a right-unit, sub-blind factor beats a
right-sub, wrong-unit one. Then the exact (name, medium, sub) entry. A
long-term (delayed) emission then tries the method's long-term default
('mtLongTermFallbackCF') so it inherits the long-term factor, not the
immediate-emission one. UUID/name misses then fall back to the flow's own
CAS + medium, so every flow sharing a CAS in a compartment is characterized,
not just the one a CF resolved to at build time. The parametric rungs
(region-stripped base name, energy family, ore grade) come last: they only
fire when every direct lookup misses.
-}
cascadeTrail :: MethodTables -> BioFlowDB -> UUID -> [(RungId, RungOutcome)]
cascadeTrail tables flowDB fid =
    (RungUuid, plain (M.lookup fid (mtUuidCF tables)))
        : maybe [] namedRungs (M.lookup fid flowDB)
  where
    plain = maybe RungMiss RungHit

    namedRungs flow =
        [ (RungUnitVariant, unitVariantOutcome)
        , (RungExactName, plain (M.lookup (name, baseMed, normSub) (mtExactCF tables)))
        , (RungLongTermDefault, longTermOutcome)
        , (RungMediumDefault, gated (M.lookup (name, baseMed) (mtFallbackCF tables)))
        , (RungCasBridge, casOutcome)
        , (RungSubBlind, gated (M.lookup (name, baseMed) (mtSubBlindCF tables)))
        , (RungRegionBase, regionOutcome)
        , (RungEnergyResource, energyOutcome)
        , (RungOreGradeBase, oreGradeOutcome)
        ]
      where
        name = SR.NormName (normalizeName (bfName flow))
        (baseMed, normSub) = flowMediumSub (mtCompartmentMap tables) flow

        -- The medium-level / CAS / sub-blind fallbacks all stand for a
        -- surface, immediate emission, so gate a resolved one by the flow's
        -- subcompartment: a foreign medium (sea/ocean) gets no freshwater CF
        -- at all, but only from a method that names sea water somewhere and so
        -- meant to leave this one out; a LONG-TERM groundwater emission drops a
        -- surface-freshwater-fate USEtox CF (CTUe/CTUh) – the method's
        -- explicit "groundwater, long-term" zero must win, never the CAS
        -- bridge. An immediate groundwater emission keeps the fallback
        -- (SimaPro semantics: an implicit sub inherits the unspecified CF),
        -- as do nutrient/other freshwater CFs (phosphate migrates to
        -- surface water, so the method characterizes it). An explicit
        -- exact-sub CF and the method's own long-term default are never
        -- gated.
        mVeto = wildcardVeto (mtCFFamily tables) (mtSeaWaterCFs tables) normSub
        gated r = maybe (plain r) RungVetoed mVeto

        -- 'mtUnitVariantCF' is empty for every method whose factor lines
        -- carry no unit suffix – the common case – and 'M.lookup' is strict
        -- in its key, so an unguarded lookup would make every flow pay a
        -- second full name normalization on the warmup-hot path for a table
        -- that cannot answer.
        unitVariantOutcome
            | M.null (mtUnitVariantCF tables) = RungNotApplicable
            | otherwise =
                gated
                    ( M.lookup
                        (SR.NormName (normalizeNameKeepUnit (bfName flow)), baseMed)
                        (mtUnitVariantCF tables)
                    )

        longTermOutcome
            | isLongTermSub normSub = plain (M.lookup (name, baseMed) (mtLongTermFallbackCF tables))
            | otherwise = RungNotApplicable

        casOutcome = case bfCAS flow of
            Nothing -> RungNotApplicable
            Just cas -> case SR.casKey cas of
                Nothing -> RungNotApplicable
                Just casNo -> gated (M.lookup (casNo, baseMed) (mtCasCF tables))

        -- A SimaPro region-suffixed flow ("Ammonia, FR") whose region the method
        -- doesn't tag falls back to the base substance's CF: an unregionalized CF
        -- for "Ammonia" applies to the emission wherever it occurs. Only fires
        -- after every direct lookup misses, and only when the suffix is a real
        -- region code (extractLocationSuffix leaves "Methane, fossil" untouched).
        --
        -- The borrowed CF keeps the base substance's *unit*, which may differ in
        -- dimension from the flow's. 'lookupEnergyDensity' therefore strips the
        -- suffix with this same 'extractLocationSuffix' before looking a density
        -- up: whatever name lends the factor must also lend the density, or the
        -- flow holds a factor it cannot be converted to and scores 0.
        regionOutcome = case mVeto of
            Just veto -> RungVetoed veto
            Nothing -> case extractLocationSuffix (bfName flow) of
                (base, Just _) -> plain (regionLookupAt base)
                (_, Nothing) -> RungNotApplicable
        regionLookupAt base =
            let bname = SR.NormName (normalizeName base)
             in M.lookup (bname, baseMed, normSub) (mtExactCF tables)
                    <|> M.lookup (bname, baseMed) (mtFallbackCF tables)

        -- An energy-resource flow whose name encodes its density ("Coal, 18 MJ per
        -- kg") borrows the CF of its resource family (coal/oil/gas/uranium…) – the
        -- generic per-MJ resource CF. The density itself is applied downstream by
        -- 'convertAndMultiply', which name-parses the same suffix, so here we only
        -- return the base CF. The resource family is resolved through the known
        -- energy resources ('mtEnergyDensities'), so an unknown resource never
        -- borrows a CF.
        --
        -- Borrow only when the family's resolving CFs agree (the generic
        -- per-MJ factor). If "Coal, hard" and "Coal, brown" disagree the
        -- family CF is ambiguous ('RungAmbiguous'), so drop rather than pick
        -- one arbitrarily by Map order – same "never guess" rule as
        -- 'agreedValue'.
        energyOutcome = case parseEnergyDensitySuffix (bfName flow) of
            Nothing -> RungNotApplicable
            Just (base, _) ->
                let fam = firstWord (normalizeName base)
                    candidates =
                        [ cf
                        | rname <- M.keys (mtEnergyDensities tables)
                        , firstWord rname == fam
                        , Just cf <- [resourceCF (SR.NormName rname)]
                        ]
                 in case candidates of
                        [] -> RungMiss
                        e : rest
                            | all ((== teCF e) . teCF) rest -> RungHit e
                            | otherwise -> RungAmbiguous

        resourceCF rname =
            M.lookup (rname, baseMed, normSub) (mtExactCF tables)
                <|> M.lookup (rname, baseMed) (mtFallbackCF tables)

        -- An ecoinvent metal-ore resource flow ("Copper, 0.99% in sulfide, Cu 0.36%
        -- …, in ground", "Gold, Au 7.1E-4%, in ore") carries no CAS and matches no CF
        -- of its own, but its reference amount is the mass of the base element, so it
        -- takes that element's resource CF. Without this the whole ore-grade family
        -- scores zero and mineral/metal depletion silently under-counts (copper- and
        -- gold-intensive products by 100×+). Resource medium only; base = the element
        -- before the first comma; the "%" requirement pins the fallback to
        -- grade-bearing variants – every ore-grade name encodes its grade as a
        -- percentage – so an ordinary comma-qualified resource ("Water, salt,
        -- ocean", "Coal, 18 MJ per kg") never borrows the base CF, and in
        -- particular an ambiguity the energy-family rung refused to resolve
        -- stays unresolved. Self-scoping and last in the cascade: 'resourceCF'
        -- returns Nothing when the base element has no CF in the method.
        oreGradeOutcome
            | baseMed == Just NaturalResource
            , "%" `T.isInfixOf` bfName flow
            , (base, rest) <- T.breakOn "," (bfName flow)
            , not (T.null rest) =
                plain (resourceCF (SR.NormName (normalizeName base)))
            | otherwise = RungNotApplicable

        firstWord = T.takeWhile (/= ' ') . T.strip

{- | The first rung that answers, and with what – the score path's view of
'cascadeTrail'.
-}
lookupCascadeEntry :: MethodTables -> BioFlowDB -> UUID -> Maybe (RungId, TableEntry)
lookupCascadeEntry tables flowDB fid =
    listToMaybe [(rid, e) | (rid, RungHit e) <- cascadeTrail tables flowDB fid]

lookupCascadeCF :: MethodTables -> BioFlowDB -> UUID -> Maybe CF
lookupCascadeCF tables flowDB fid = teCF . snd <$> lookupCascadeEntry tables flowDB fid

{- | A subcompartment that names no specific subcompartment – empty, or either
spelling of unspecified. Such a CF is the medium-level default: it
characterizes any flow in that medium whose own subcompartment the method
doesn't cover. The single source of truth for "is this the catch-all subcomp",
so the fallback table, the CAS-bridge rank, and the regionalized wildcard
filter can't drift apart on which spellings count (they did: the filter once
omitted bare @unspecified@). Inputs are expected already lower-cased via
'normalizeCompartment'.
-}
isUnspecifiedSub :: Text -> Bool
isUnspecifiedSub s = T.null s || s == "unspecified" || s == "(unspecified)"

{- | True when a subcompartment marks a long-term (delayed) emission, e.g.
@"groundwater, long-term"@ or @"unspecified (long-term)"@. EF distinguishes the
time horizon: a substance often carries a different (frequently zero) CF for its
long-term emission than for its immediate one.
-}
isLongTermSub :: Subcompartment -> Bool
isLongTermSub (Subcompartment s) = "long-term" `T.isInfixOf` s || "long term" `T.isInfixOf` s

{- | A subcompartment that names a different fate than the surface / immediate
emission a method's unspecified (medium-level) CF stands for, so that CF must
not silently reach it. Two tiers, gated differently in 'lookupCascadeCF':

  * 'isDetachedSub' && 'isLongTermSub' – long-term emissions to groundwater
    (ecoinvent tailings/landfill leachate). A surface-freshwater-fate USEtox CF
    ('USEtoxFamily') does not apply: EF methods carry an explicit zero for
    "groundwater, long-term", and a name-mismatched flow must not borrow the
    immediate CF through the CAS bridge (that over-counted ecotoxicity by two
    orders of magnitude on metal-intensive products). An IMMEDIATE groundwater
    emission is NOT gated: SimaPro subcompartment semantics fall back to the
    unspecified CF for any sub the method leaves implicit – EF exports zero out
    only "groundwater, long-term" and ocean explicitly – and compartments.csv
    already maps the ecoinvent spelling ("ground-") to surface water, so gating
    the SimaPro spelling would characterize the same emission inconsistently.
    Nutrient/other freshwater CFs stay ungated either way (phosphate migrates
    to surface water, so the method characterizes it). Scoped to groundwater,
    NOT every long-term sub: a "river, long-term" release is still surface
    freshwater and stays characterized.
  * 'isForeignMediumSub' – the sea/ocean, a different receiving medium
    altogether: a freshwater CF does not apply at all (water released to the sea
    is not freshwater depletion), and the method says so itself with an explicit
    sea-water line. This tier applies only to a method that writes such lines
    ('MethodDeclaresSeaWater'); see 'SeaWaterCFs' for why a method that never
    mentions the sea must not have a zero invented for it.

Names are the post-'normalizeCompartment' lower-cased subcompartment, so each
tier names the canonical spelling only: the source spellings a database or a
method file may use ("sea water", "Emissions to sea water") are translated by
compartments.csv before they reach here, and a new one is added there.
-}
isDetachedSub :: Subcompartment -> Bool
isDetachedSub (Subcompartment s) = "groundwater" `T.isPrefixOf` s

isForeignMediumSub :: Subcompartment -> Bool
isForeignMediumSub (Subcompartment s) = s == "ocean"

{- | The veto a medium-level (wildcard / fallback) CF meets at the given
subcompartment, or 'Nothing' when it reaches – both tiers above, decided in
one place. Shared by the read-path cascade gate (which also reports the
reason in a trail) and the build-time regionalized wildcard match, so the
two scoring paths apply the same rule, under the same name, and can't drift.
-}
wildcardVeto :: CFFamily -> SeaWaterCFs -> Subcompartment -> Maybe VetoReason
wildcardVeto family seaWater sub
    | seaWater == MethodDeclaresSeaWater && isForeignMediumSub sub = Just ForeignMediumVeto
    | isDetachedSub sub && isLongTermSub sub && family == USEtoxFamily = Just LongTermUSEtoxVeto
    | otherwise = Nothing

-- | 'wildcardVeto' for the caller that only asks whether the CF reaches.
wildcardReachesSub :: CFFamily -> SeaWaterCFs -> Subcompartment -> Bool
wildcardReachesSub family seaWater = isNothing . wildcardVeto family seaWater

{- | A subcompartment that names the long-term catch-all: @"unspecified
(long-term)"@, @"(long-term)"@ – i.e. unspecified once the time-horizon marker is
removed. These hold a method's medium-level default for long-term emissions, so a
long-term flow at an uncovered specific subcompartment ("groundwater, long-term")
must inherit THIS, not the immediate-emission unspecified CF.
-}
isLongTermUnspecifiedSub :: Text -> Bool
isLongTermUnspecifiedSub s =
    isLongTermSub (Subcompartment s) && isUnspecifiedSub (stripLongTerm s)
  where
    stripLongTerm =
        T.strip
            . T.filter (`notElem` ("()" :: String))
            . T.replace "long term" ""
            . T.replace "long-term" ""

{- | Whether delayed long-term (> 100 yr) biosphere emissions count toward the
impact score. 'IncludeLongTerm' is the default and preserves the standard
ecoinvent/EF convention; 'ExcludeLongTerm' characterizes the process as if the
delayed emissions were out of scope.
-}
data LongTermMode = IncludeLongTerm | ExcludeLongTerm
    deriving (Eq, Show)

{- | Read a request/tool @exclude@ flag into a 'LongTermMode'. Absent or false
means keep the delayed emissions (the convention default).
-}
longTermModeFromExclude :: Bool -> LongTermMode
longTermModeFromExclude excl = if excl then ExcludeLongTerm else IncludeLongTerm

{- | Whether a biosphere flow is a delayed long-term emission: it is known to the
FlowDB and its sub-compartment carries the "long-term" marker ('isLongTermSub').
An unknown UUID is never treated as long-term (absence of evidence is not
evidence). Shared by every exclusion path so they all agree on what "long-term"
means.
-}
isLongTermFlow :: BioFlowDB -> UUID -> Bool
isLongTermFlow flowDB = maybe False subIsLongTerm . (`M.lookup` flowDB)
  where
    subIsLongTerm = maybe False (isLongTermSub . Subcompartment . T.toLower) . VT.bfCompartmentSub

{- | Drop delayed long-term biosphere emissions from an inventory before
characterization. These flows are always emissions, never resources, so this
never removes a regionalized resource flow (water use / land use) – those
categories are computed from a separate path and stay untouched.
-}
excludeLongTermFlows :: BioFlowDB -> Inventory -> Inventory
excludeLongTermFlows flowDB = M.filterWithKey (\fid _ -> not (isLongTermFlow flowDB fid))

{- | Apply a 'LongTermMode' to an inventory. 'IncludeLongTerm' is the identity
(no extra work); 'ExcludeLongTerm' drops the delayed long-term emissions. This
is the single entry point every score path uses so include/exclude stays
consistent across the batch, single-method, and contribution surfaces.
-}
applyLongTermMode :: BioFlowDB -> LongTermMode -> Inventory -> Inventory
applyLongTermMode _ IncludeLongTerm = id
applyLongTermMode flowDB ExcludeLongTerm = excludeLongTermFlows flowDB

{- | The @(normalized medium, subcompartment)@ a database flow resolves to after
compartment normalization. Shared by the name/CAS read path
('lookupCascadeCF') and the regionalized CAS fallback so both key a flow the
same way. Subcomp resolution prefers the explicit 'compartmentSub' field,
falling back to the tail of a @"medium/sub"@ category name.
-}
flowMediumSub :: CompartmentMap -> BiosphereFlow -> (MediumKey, Subcompartment)
flowMediumSub cmap flow =
    let rawMed = VT.bfCompartmentName flow
        rawSub = T.toLower (fromMaybe T.empty (VT.bfCompartmentSub flow))
        Compartment normMedRaw normSub _ =
            normalizeCompartment cmap (Compartment rawMed rawSub T.empty)
     in (mediumKey normMedRaw, Subcompartment normSub)

{- | Flow→CF conversion factor for @qty@ units of flow, applying the
energy-density bridge when it is needed and available.

A density relates two dimensions – @target@ per @native@, MJ per kg for a
calorific value, m³ per kg for a mass density – and a flow can meet a CF from
either side of it, so the bridge reads the ratio both ways:

  * __forward__, the CF is in the target unit and the flow in the native one
    (a mass flow against a per-MJ or per-m³ factor): @qNative × E@;
  * __inverse__, the mirror image, the flow is in the target unit and the CF in
    the native one (a volume flow against a per-kg factor): @qTarget ÷ E@.

Both require the flow's own unit to be dimensionally unreachable from the CF's
– a pair the ordinary conversion can already handle is its business, not the
bridge's – and both require a positive density: a zero would divide, and a
negative one would silently flip the sign of a score.

Any leg failing to convert is an 'EnergyBridgeRefused', and so is a
non-positive density: we refuse a wrong-basis or wrong-dimension factor rather
than silently using the raw value, and the refusal travels in the outcome
instead of collapsing to a bare zero. Every other case (matching dimensions,
no density) defers to 'characterizationOutcome', so flows without a density
are unchanged.
-}
energyAwareOutcome :: UnitConfig -> Text -> CFUnit -> Maybe EnergyDensity -> Double -> ConversionOutcome
energyAwareOutcome cfg flowUnit cfu@(CFUnit rawCfUnit) mDensity qty =
    case mDensity of
        -- Generalizing the guard from "energy" to "same dimension as one leg of
        -- the density" lets one mechanism serve the fossil energy CF (kg → MJ
        -- via calorific value), the water-scarcity CF (kg → m³ via density) and
        -- their mirrors, which is every case where a per-physical-quantity CF
        -- meets an inventory flow of another dimension.
        Just density@(EnergyDensity ev targetUnit nativeUnit)
            | crossDimension
            , ev > 0
            , cfIsWrittenPer targetUnit ->
                bridged density DensityForward $ do
                    qtyNative <- toUnit nativeUnit
                    factor <- convertOntoFactorBasis cfg targetUnit rawCfUnit ev
                    pure (qtyNative * factor)
            | crossDimension
            , ev > 0
            , cfIsWrittenPer nativeUnit ->
                bridged density DensityInverse $ do
                    qtyTarget <- toUnit targetUnit
                    convertOntoFactorBasis cfg nativeUnit rawCfUnit (qtyTarget / ev)
        _ -> characterizationOutcome cfg flowUnit cfu qty
  where
    bridged density dir =
        maybe (Unconvertible (EnergyBridgeRefused density)) (`Converted` EnergyBridged density dir)
    -- What the CF is written per, which for a result expression the table holds
    -- ("m3-world equivalents") is the quantity the result is expressed per, and
    -- for one it does not know ("kg CO2 eq") is nothing, so that one is never
    -- bridged.
    cfIsWrittenPer :: Text -> Bool
    cfIsWrittenPer u = isJust (convertOntoFactorBasis cfg rawCfUnit u 1)
    -- Both arms need the flow's unit to be unreachable from the CF's: a
    -- same-dimension pair belongs to the ordinary conversion.
    crossDimension :: Bool
    crossDimension = not (cfIsWrittenPer flowUnit)
    -- Bring the inventory quantity into the leg the arm applies the density
    -- against: identical units need no table entry, otherwise a same-dimension
    -- conversion. An incompatible or unknown unit yields 'Nothing' → 0.
    toUnit u
        | unitKey cfg flowUnit == unitKey cfg u = Just qty
        | otherwise = convertUnit cfg flowUnit u qty

{- | Apply the flow→CF unit conversion factor and multiply by the CF value.

Delegates to 'energyAwareConversion', which falls back to
'convertForCharacterization' unless the energy-density bridge applies – so a
dimensional mismatch between flow and CF units lands an effective @0@ (refuse
to score wrong-dimension data) rather than silently passing the unconverted
quantity through. Pass @qty = 1.0@ to obtain the effective-CF factor used at
build time; pass an actual quantity for inline scoring.
-}
convertAndMultiply ::
    UnitConfig ->
    UnitDB ->
    -- | Normalized-flow-name → energy density, for the energy-CF bridge.
    EnergyDensityMap ->
    {- | Pre-resolved flow if the caller already has it; @Nothing@ defaults to
    the identity factor (no flow record means no flow unit known).
    -}
    Maybe BiosphereFlow ->
    CF ->
    Double ->
    Double
convertAndMultiply unitConfig unitDB energyDensities mflow (CF cfVal cfu) qty =
    convertedQuantity (flowToCFOutcome unitConfig unitDB energyDensities mflow cfu qty) * cfVal
{-# INLINE convertAndMultiply #-}

{- | The conversion 'convertAndMultiply' applies before the CF multiply, kept
whole: which bridge carried the flow quantity onto the CF's basis, or why none
could. Same inputs, same decision – the multiply is all 'convertAndMultiply'
adds.
-}
flowToCFOutcome ::
    UnitConfig ->
    UnitDB ->
    EnergyDensityMap ->
    Maybe BiosphereFlow ->
    CFUnit ->
    Double ->
    ConversionOutcome
flowToCFOutcome unitConfig unitDB energyDensities mflow cfu qty =
    let flowUnit = maybe "" unitName (mflow >>= \f -> M.lookup (bfUnitId f) unitDB)
        -- 'lookupEnergyDensity' walks the same names the CF lookup walks –
        -- including the region-stripped one, so a flow lent the base substance's
        -- factor is lent its density too.
        mDensity = mflow >>= \f -> lookupEnergyDensity energyDensities (bfName f)
     in energyAwareOutcome unitConfig flowUnit cfu mDensity qty

{- | Per-flow contributions over an 'Inventory', keyed by flow UUID (possibly
cross-DB-merged). Walks the inventory directly (not the mappings) so any
flow with a matchable CF contributes – including flows from dep DBs that
don't appear in the root-DB method mapping.

Returns @(contributions, unknownUuids)@:
  * @contributions@ – @[(flow, cfValue, contributionInMethodUnit)]@
    for every non-zero inventory entry whose UUID resolves in 'flowDB'
    AND whose (name, medium, subcompartment) matches a CF.
  * @unknownUuids@ – non-zero inventory UUIDs with no record in 'flowDB'.
    Callers should surface these (per the "no silent errors" rule): a
    missing record means the merged metadata is incomplete for this
    inventory and some flows are invisible to characterization.

Flows that resolve in 'flowDB' but have no matching CF are legitimately
uncharacterized and silently omitted – that matches the behaviour of
'computeLCIAScoreFromTables' and is not a data-integrity concern.
-}
inventoryContributions ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Inventory ->
    MethodTables ->
    ([(BiosphereFlow, Double, Double)], [UUID])
inventoryContributions unitConfig unitDB flowDB inventory tables =
    M.foldlWithKey' step ([], []) inventory
  where
    -- Strict fold over the inventory Map: the old 'foldr' over 'M.toList'
    -- built a thunk chain the size of the inventory on every call, and
    -- characterization runs this 27-wide over K-activity batches – the
    -- dominant garbage source. Strict pair + accumulator prevents the leak.
    -- Result list order is reversed vs. the old version, but every caller
    -- already 'sortOn's by |contribution|.
    step (!contribs, !unknowns) fid qty
        | qty == 0 = (contribs, unknowns)
        | otherwise = case M.lookup fid flowDB of
            Nothing -> (contribs, fid : unknowns) -- metadata missing – surface it
            Just flow -> case lookupCascadeCF tables flowDB fid of
                Nothing -> (contribs, unknowns) -- no CF match – legitimately uncharacterized
                Just found@(CF cfVal _) ->
                    let !contribution = convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) (Just flow) found qty
                     in ((flow, cfVal, contribution) : contribs, unknowns)

{- | Per-process LCIA contributions for one DB + one method, driven by
'MethodTables' + a merged 'BioFlowDB'. Mirrors
'Matrix.computeProcessLCIAContributions' but lets dep-DB flows land a CF
via (name, medium, subcompartment) fallback – same lookup path as
'inventoryContributions' – so this helper can be called per-DB while
walking a cross-DB dependency graph and still characterize every flow.

Iterates @dbBiosphereTriples db@; for each @(flowRow, colIdx, bioVal)@ it
attributes @bioVal * scaling[col] * CF@ (in the method's unit, after
flow-unit → CF-unit conversion) to the process owning @colIdx@.
-}
processContributionsFromTables ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    {- | long-term emission policy: 'ExcludeLongTerm' zeroes the delayed flows so
    per-activity contributions match a score computed the same way
    -}
    LongTermMode ->
    Database ->
    Vector ->
    MethodTables ->
    M.Map ProcessId Double
processContributionsFromTables unitConfig unitDB flowDB ltMode db scalingVec tables =
    U.foldl' step M.empty (dbBiosphereTriples db)
  where
    actIdx = dbActivityIndex db
    bioFlows = dbBiosphereOrder db
    nFlows = V.length bioFlows
    nActs = V.length actIdx

    excluded = case ltMode of
        IncludeLongTerm -> const False
        ExcludeLongTerm -> isLongTermFlow flowDB

    -- Precompute the effective CF (CF value × flow→CF unit conversion factor)
    -- per biosphere-matrix row so the triple loop becomes pure arithmetic.
    -- O(|bioFlows|) once, vs O(|triples| × map-lookups) before. A long-term
    -- flow gets 0 under 'ExcludeLongTerm', dropping it exactly as the inventory
    -- pre-filter would.
    effectiveCF :: U.Vector Double
    effectiveCF = U.generate nFlows $ \i ->
        let flowUUID = bioFlows V.! i
            mflow = M.lookup flowUUID flowDB
         in if excluded flowUUID
                then 0
                else case mflow of
                    Nothing -> 0
                    Just _ -> case lookupCascadeCF tables flowDB flowUUID of
                        Nothing -> 0
                        Just cf -> convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) mflow cf 1.0

    -- Invert dbActivityIndex (pid -> col) into (col -> pid) as an unboxed
    -- vector for O(1) per-triple lookup. Assumes matrix cols are dense in
    -- [0..nActs-1], which matches the existing index construction.
    colToProc :: U.Vector Int
    colToProc = U.create $ do
        mv <- MU.replicate nActs (-1 :: Int)
        V.imapM_ (\pid col -> MU.write mv (fromIntegral col) pid) actIdx
        pure mv

    step acc (SparseTriple flowRow colIdx bioVal) =
        let cf = effectiveCF U.! fromIntegral flowRow
         in if cf == 0
                then acc
                else
                    let colI = fromIntegral colIdx :: Int
                        pid = colToProc U.! colI
                     in if pid < 0
                            then acc
                            else
                                let scale = scalingVec U.! colI
                                    pid' = fromIntegral pid :: ProcessId
                                 in M.insertWith (+) pid' (bioVal * scale * cf) acc

-- ────────────────────────────────────────────────────────────────────────────
-- Multi-method scoring (set-level)
-- ────────────────────────────────────────────────────────────────────────────

{- | One method's worth of data inside a 'MethodSetTables'. Carries the full
'MethodTables' plus the original 'Method' so the scoring path can fall back
to the per-method dispatcher ('computeLCIAScoreAuto') for regionalized
methods without having to re-look up anything.
-}
data MethodSetEntry = MethodSetEntry
    { mseMethodId :: !UUID
    , mseMethod :: !Method
    , mseTables :: !MethodTables
    }

{- | Stacked CF tables for scoring the same inventory against many methods at
once. Built once per (db, sortedMethodIds) and cached.

The set is partitioned at build time: non-regional methods land in
'msBatched' (one shared dense broadcast matrix, scored via a single
matvec); regional methods land in 'msRegional' (per-method dispatch through
'computeLCIAScoreAuto', which after the PR #39 precompute is itself a tight
dot product). Mixing the two regimes was previously a hard either/or – a
single regional method in a set of 17 would force all 17 down the slow
per-method walk. The partition recovers the batched matvec for the
non-regional half of mixed sets like EF 3.1 (4 regional + 13 non-regional).
-}
data MethodSetTables = MethodSetTables
    { msAllMethods :: !(V.Vector MethodSetEntry)
    {- ^ Every method in the set, in canonical (sorted) order. Used to
    emit results in the caller's order regardless of partition.
    -}
    , msBatched :: !BatchedTables
    -- ^ Non-regional half. May be empty (@btNMethods == 0@) for all-regional sets.
    , msRegional :: !(V.Vector MethodSetEntry)
    -- ^ Regional half. May be empty for all-non-regional sets (the PR #29 case).
    }

{- | Matvec data for the non-regional subset of a 'MethodSetTables'.

Cell @(method i, flow j)@ lives at @btMat[j * btNMethods + i]@: all CFs for
a single flow row are contiguous so the sparse-inventory scoring loop reads
one cache line per non-zero inventory entry.
-}
data BatchedTables = BatchedTables
    { btMethods :: !(V.Vector MethodSetEntry)
    -- ^ Non-regional methods, in caller-given order.
    , btUuidIndex :: !(M.Map UUID Int)
    -- ^ Flow UUID → row in 'btMat'.
    , btNFlows :: !Int
    -- ^ @M.size btUuidIndex@. Cached for the matvec inner loop.
    , btNMethods :: !Int
    -- ^ @V.length btMethods@. Cached for the matvec inner loop.
    , btMat :: !(U.Vector Double)
    {- ^ Column-major dense broadcast, length @btNFlows * btNMethods@. Empty
    when @btNMethods == 0@.
    -}
    , btJudged :: !(Set.Set UUID)
    {- ^ Flows every method's fill walked and none characterizes: the
    intersection of the methods' 'mtJudged', minus 'btUuidIndex'. Such a
    flow contributes nothing, and the scoring loop knows it in one lookup
    instead of one cascade per method.
    -}
    }

{- | Build 'MethodSetTables' from per-method 'MethodTables'. The list order
is preserved in 'msAllMethods' and drives the order results come back in;
the same order, restricted to non-regional methods, is also the column
order in 'btMat'. Callers should sort by 'methodId' for cache-key
canonicality.
-}
buildMethodSetTables :: [(Method, MethodTables)] -> MethodSetTables
buildMethodSetTables pairs =
    let entries = V.fromList [MethodSetEntry (methodId m) m t | (m, t) <- pairs]
        isRegional = not . M.null . mtRegionalizedCF . mseTables
        (regional, batched) = V.partition isRegional entries
     in MethodSetTables
            { msAllMethods = entries
            , msBatched = buildBatchedTables batched
            , msRegional = regional
            }

{- | Build the broadcast-matrix payload for the non-regional subset of a
method set. Returns an empty 'BatchedTables' (zero-length matrix, zero
methods) when the subset is empty, so the scoring path can skip it
without a special case.

Column-major layout: @btMat[j * nMethods + i]@ holds method @i@'s effective
CF for flow row @j@. All @nMethods@ CFs for a single flow are contiguous so
the sparse-inventory scoring loop walks one cache line per non-zero entry.
-}
buildBatchedTables :: V.Vector MethodSetEntry -> BatchedTables
buildBatchedTables entries =
    let nMethods = V.length entries
        uuidSet =
            Set.unions
                [Set.fromList (M.keys (mtBroadcast (mseTables e))) | e <- V.toList entries]
        uuidList = Set.toAscList uuidSet
        uuidIndex = M.fromList (zip uuidList [0 ..])
        nFlows = length uuidList
        judged = intersections (map (mtJudged . mseTables) (V.toList entries)) `Set.difference` uuidSet
        mat = U.create $ do
            mv <- MU.replicate (nFlows * nMethods) (0.0 :: Double)
            V.iforM_ entries $ \i e ->
                mapM_
                    ( \(uuid, cf) -> case M.lookup uuid uuidIndex of
                        Just j -> MU.unsafeWrite mv (j * nMethods + i) cf
                        Nothing -> pure ()
                    )
                    (M.toList (mtBroadcast (mseTables e)))
            pure mv
     in BatchedTables
            { btMethods = entries
            , btUuidIndex = uuidIndex
            , btNFlows = nFlows
            , btNMethods = nMethods
            , btMat = mat
            , btJudged = judged
            }
  where
    intersections :: [Set.Set UUID] -> Set.Set UUID
    intersections [] = Set.empty
    intersections (x : xs) = foldr Set.intersection x xs

{- | Score an inventory against every method in a 'MethodSetTables', emitting
@(methodId, Right score)@ on success and @(methodId, Left err)@ for a
regional method whose coverage is incomplete (mirrors 'computeLCIAScoreAuto').

The two halves of the set are scored by separate code paths and merged by
methodId so the result list follows the input order ('msAllMethods'):

  * non-regional half ('msBatched' on root): one matvec over the shared
    dense broadcast – the PR #29 fast path, restored for mixed sets like
    EF 3.1. Reads the merged cross-DB inventory, so dep-DB flows are
    characterized without any per-DB bookkeeping.
  * regional half ('msRegional' on root): per-method cross-DB sum
    ('sumRegionalizedLCIAScoreCrossDB'). For each regional method, the
    score is @Σ_d rawWeights_d · scaling_d@ across every participating
    database – root + each dep DB reached at request time. Closes the
    gap where dep-DB regional CFs were previously invisible.

Callers pass the per-DB triples as a 'NonEmpty' with the ROOT triple at
'NE.head'. The non-regional matvec is keyed off the root entry's
'msBatched'; the regional cross-DB sum walks every entry. The 'NonEmpty'
constraint makes the impossible state (no DBs participated) unrepresentable.
-}
computeLCIAScoreSetFromTables ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Inventory ->
    M.Map Location [Location] ->
    {- | Non-empty per-DB triples: 'NE.head' is root, tail is each participating
    dep DB. Building order matches 'SharedSolver.csScalings'.
    -}
    NonEmpty (Database, Vector, MethodSetTables) ->
    [(UUID, Either Text Double)]
computeLCIAScoreSetFromTables unitCfg unitDB flowDB inventory hier perDb =
    let (_, _, mstRoot) = NE.head perDb
        batched = scoreBatched unitCfg unitDB flowDB (msBatched mstRoot) inventory
        regional = scoreRegionalCrossDB unitCfg unitDB flowDB hier (msRegional mstRoot) perDb
        byId = M.fromList (batched ++ regional)
     in [ (mseMethodId e, byId M.! mseMethodId e)
        | e <- V.toList (msAllMethods mstRoot)
        ]

{- | Per-method cross-DB regional sum. For each regional method, scores
@Σ_d rawWeights_d · scaling_d@ across all participating DBs by looking
up that method's tables in each DB's 'MethodSetTables' and summing the
per-DB dot products via 'sumRegionalizedLCIAScoreCrossDB'.

A DB whose 'MethodSetTables' has no entry for a given methodId (mismatched
sets) is skipped, and its emissions are dropped from that method's sum. This
is not the same as a DB whose mappings caught none of the method's regional
CFs: that one is scored flat by 'computeRegionalizedLCIAScore'. Method sets
are built from one collection and are expected to agree, so the skip is a
defensive case rather than a live one.
-}
scoreRegionalCrossDB ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    M.Map Location [Location] ->
    V.Vector MethodSetEntry ->
    NonEmpty (Database, Vector, MethodSetTables) ->
    [(UUID, Either Text Double)]
scoreRegionalCrossDB unitCfg unitDB flowDB hier ms perDb =
    [ ( mseMethodId e
      , sumRegionalizedLCIAScoreCrossDB unitCfg unitDB flowDB hier (triples (mseMethodId e))
      )
    | e <- V.toList ms
    ]
  where
    -- Per-method per-DB triples; skip DBs that don't carry this method.
    triples mid =
        [ (db, sv, t)
        | (db, sv, mst) <- NE.toList perDb
        , Just t <- [lookupMethodTables mid mst]
        ]
    lookupMethodTables mid mst =
        mseTables
            <$> V.find ((== mid) . mseMethodId) (msAllMethods mst)

{- | One sparse-inventory matvec over the non-regional half of a method set.
For each non-zero @(uuid, qty)@ in the inventory, accumulates
@qty * btMat[j*nMethods + i]@ into score @i@ for every method @i@, where
@j@ is the flow's row. The inner loop walks contiguous CF cells (column-
major layout) so the cost is @nnz × nMethods@ FMAs with cache-friendly
reads. A UUID outside the broadcast that every method's fill walked
('btJudged') carries no factor and is skipped; any other falls back to each
non-regional method's 'lookupCascadeCF', so an inventory reaching flows the
fill never saw does not silently lose them.
-}
scoreBatched ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    BatchedTables ->
    Inventory ->
    [(UUID, Either Text Double)]
scoreBatched unitCfg unitDB flowDB bt inventory
    | btNMethods bt == 0 = []
    | otherwise =
        let nMethods = btNMethods bt
            mat = btMat bt
            uuidIdx = btUuidIndex bt
            entriesV = btMethods bt
            cascadeContrib !tables !uuid !qty =
                case lookupCascadeCF tables flowDB uuid of
                    Nothing -> 0
                    Just cf ->
                        convertAndMultiply unitCfg unitDB (mtEnergyDensities tables) (M.lookup uuid flowDB) cf qty
            scores = U.create $ do
                mv <- MU.replicate nMethods (0.0 :: Double)
                mapM_
                    ( \(uuid, qty) ->
                        if qty == 0
                            then pure ()
                            else case M.lookup uuid uuidIdx of
                                Just j -> do
                                    let rowStart = j * nMethods
                                        loop !i
                                            | i >= nMethods = pure ()
                                            | otherwise = do
                                                let !cf = U.unsafeIndex mat (rowStart + i)
                                                MU.unsafeModify mv (+ qty * cf) i
                                                loop (i + 1)
                                    loop 0
                                Nothing
                                    | Set.member uuid (btJudged bt) -> pure ()
                                    | otherwise ->
                                        V.imapM_
                                            ( \i e -> do
                                                let !c = cascadeContrib (mseTables e) uuid qty
                                                MU.unsafeModify mv (+ c) i
                                            )
                                            entriesV
                    )
                    (M.toList inventory)
                pure mv
         in [ (mseMethodId e, Right (scores U.! i))
            | (i, e) <- zip [0 ..] (V.toList entriesV)
            ]

-- ──────────────────────────────────────────────
-- Post-scoring suggester
-- ──────────────────────────────────────────────

{- | Top-level CF lookup helper used by the suggester. Delegates to
'lookupCascadeCF' (with the caller's already-resolved flow as a singleton
DB) so the suggester sees exactly what scoring sees – including the CAS
bridge – and 'findUncharacterized' never flags a flow the score path
characterizes. Cold path; the singleton allocation is irrelevant here.
-}
lookupCFForFlow :: MethodTables -> UUID -> Maybe BiosphereFlow -> Maybe CF
lookupCFForFlow tables fid mFlow = teCF . snd <$> lookupEntryForFlow tables fid mFlow

{- | 'lookupCFForFlow' with the served entry and the rung that answered –
for surfaces that report how a flow is covered, not just by how much.
-}
lookupEntryForFlow :: MethodTables -> UUID -> Maybe BiosphereFlow -> Maybe (RungId, TableEntry)
lookupEntryForFlow tables fid mFlow =
    lookupCascadeEntry tables (maybe M.empty (M.singleton fid) mFlow) fid

{- | The database flows a method's tables characterize, each probed with
'lookupCFForFlow' – the read-side lookup scoring uses – so a flow reached
through a name-level, subcompartment or CAS-bridge fallback counts as covered.

This is the honest way to count a method's reach into a database. The
build-side mappings undercount it: there each factor resolves to at most one
flow, so a factor that covers a substance across many compartments or
locations surfaces as a single flow. And because the answer is a set, a whole
collection's reach is the union of its methods' sets – summing per-method
counts would count a flow once per method that characterizes it.
-}
characterizedFlowIds :: MethodTables -> BioFlowDB -> S.Set UUID
characterizedFlowIds tables bioFlows =
    S.fromList
        [ fid
        | (fid, flow) <- M.toList bioFlows
        , isJust (lookupCFForFlow tables fid (Just flow))
        ]

{- | Find the top-N method CFs that most resemble a database flow.

Three signals are stacked, the highest-scoring reason wins:

1. Plain Jaccard on normalized-name tokens (cheap baseline; catches
   word-order and punctuation variants).
2. Jaccard after expanding tokens via the PubChem snapshot
   ('expandedTokens'). This is what bridges \"CO2\" and
   \"Carbon dioxide\" – pure tokenization can never see they relate.
3. CAS bridge: when the flow's CAS matches a CF's CAS, the candidate is
   surfaced at score 0.95 regardless of name overlap. Highest-confidence
   reason; catches cases where one side has CAS and the other doesn't,
   so the existing CAS-cascade match already failed.

Candidate space is pre-filtered to the same compartment medium when the
flow has one (via 'miByMedium'), plus any CAS-bridge hits. This keeps the
scan cheap even on methods with thousands of CFs.

Empty result list = no signal above zero. Caller should treat that as
\"this flow is genuinely uncharacterized by the method\", not a bug.
-}
findSimilarCFs :: ChemSynonyms -> MethodIndex -> BiosphereFlow -> Int -> [SimilarCF]
findSimilarCFs syns idx flow maxN
    | maxN <= 0 = []
    | otherwise =
        let flowName' = bfName flow
            flowCAS' = bfCAS flow
            flowMedium = VT.compartmentName <$> VT.bfCompartment flow

            flowRawTokens = S.fromList (T.words (normalizeName flowName'))
            flowExpTokens = expandedTokens syns flowName'

            -- Same-medium candidates (cheap scan); fall back to whole index
            -- only when we have no medium info to filter by.
            mediumIdxs = fromMaybe [0 .. V.length (miCFs idx) - 1] (M.lookup flowMedium (miByMedium idx))

            casBridgeIdxs = case flowCAS' of
                Nothing -> []
                Just cas -> M.findWithDefault [] cas (miByCAS idx)

            -- Score one CF index. Two Jaccards: raw (plain tokens) vs
            -- expanded (synonyms folded in). The reason follows whichever
            -- signal won, so the audit JSON tells the reviewer what to
            -- verify.
            scoreCandidate i =
                let cfRawTokens = miCFTokens idx V.! i
                    cf = miCFs idx V.! i
                    cfExpTokens = expandedTokens syns (mcfFlowName cf)
                    rawJ = jaccard flowRawTokens cfRawTokens
                    expJ = jaccard flowExpTokens cfExpTokens
                 in if expJ > rawJ
                        then (i, expJ, SimBySynonymExpansion)
                        else (i, rawJ, SimByJaccard)

            mediumScored = map scoreCandidate mediumIdxs
            casScored = [(i, 0.95, SimByCASBridge) | i <- casBridgeIdxs]

            -- Merge: same CF can be in both lists (medium hit AND CAS hit);
            -- keep the higher-scoring entry so we don't show duplicates.
            mergedMap =
                M.fromListWith
                    pickHigher
                    [(i, (s, r)) | (i, s, r) <- mediumScored ++ casScored]

            ranked =
                take maxN $
                    sortOn (\(_, (s, _)) -> Down s) $
                        filter (\(_, (s, _)) -> s > 0) $
                            M.toList mergedMap
         in [ SimilarCF
                { scfMethodFlowName = mcfFlowName cf
                , scfCAS = mcfCAS cf
                , scfCompartment = mcfCompartment cf
                , scfScore = s
                , scfReason = r
                , scfCfValue = mcfValue cf
                , scfCfUnit = mcfUnit cf
                }
            | (i, (s, r)) <- ranked
            , let cf = miCFs idx V.! i
            ]
  where
    pickHigher (s1, r1) (s2, r2) = if s1 >= s2 then (s1, r1) else (s2, r2)

    jaccard :: S.Set Text -> S.Set Text -> Double
    jaccard a b
        | S.null a || S.null b = 0
        | otherwise =
            let inter = S.size (S.intersection a b)
                uni = S.size (S.union a b)
             in fromIntegral inter / fromIntegral uni

{- | Collect uncharacterized flows from an inventory, ranked by their share of
total inventory mass. For each, surface the top-N similar CFs from the
method (so the caller can tell genuine method gaps from mapping bugs).

This walks the inventory once for the totals and once for the unmatched
collection – two cheap O(|inventory|) passes. The expensive part (the
suggester) only runs on the surviving flows after weight filtering and
'uoMaxFlows' truncation.

Returns @[]@ when no flow exceeds 'uoMinAbsWeight' or when the method's
diagnostics are disabled (@uoMaxFlows == 0@ / @uoMaxSimilar == 0@).
-}
findUncharacterized ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Inventory ->
    MethodTables ->
    ChemSynonyms ->
    MethodIndex ->
    UncharacterizedOpts ->
    [UncharacterizedFlow]
findUncharacterized _ unitDB flowDB inventory tables syns idx opts
    | uoMaxFlows opts <= 0 = []
    | totalAbs == 0 = []
    | otherwise =
        let unmatched =
                [ (flow, qty, w)
                | (fid, qty) <- M.toList inventory
                , qty /= 0
                , Just flow <- [M.lookup fid flowDB]
                , isNothing (lookupCFForFlow tables fid (Just flow))
                , let w = abs qty / totalAbs
                , w >= uoMinAbsWeight opts
                ]
            ranked =
                take (uoMaxFlows opts) $
                    sortOn (\(_, _, w) -> Down w) unmatched
         in [ UncharacterizedFlow
                { ucfFlowId = bfId flow
                , ucfFlowName = bfName flow
                , ucfCategory = VT.bfCompartmentName flow
                , ucfSubcomp = VT.bfCompartmentSub flow
                , ucfFlowUnit = flowUnitText flow
                , ucfQuantity = qty
                , ucfAbsWeight = w
                , ucfSimilarCFs = findSimilarCFs syns idx flow (uoMaxSimilar opts)
                }
            | (flow, qty, w) <- ranked
            ]
  where
    !totalAbs = M.foldr (\q s -> s + abs q) 0 inventory
    flowUnitText flow = maybe "" unitName (M.lookup (bfUnitId flow) unitDB)

{- | Score an inventory and attach diagnostics in one call.

Convenience wrapper that runs 'computeLCIAScoreFromTables' and then
'findUncharacterized' on the same inputs, splicing the results into one
'LCIAOutcome'. Use this on the diagnostics path; stick to
'computeLCIAScoreFromTables' on the hot scoring path where the extra
suggester work is wasted.
-}
computeLCIAScoreWithDiagnostics ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Inventory ->
    MethodTables ->
    ChemSynonyms ->
    MethodIndex ->
    UncharacterizedOpts ->
    LCIAOutcome
computeLCIAScoreWithDiagnostics unitConfig unitDB flowDB inventory tables syns idx opts =
    let outcome = computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables
        diagnostics = findUncharacterized unitConfig unitDB flowDB inventory tables syns idx opts
     in outcome{loUncharacterized = diagnostics}

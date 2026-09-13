{-# LANGUAGE OverloadedStrings #-}

{- | Why one flow scores with the factor it does.

A score collapses a whole cascade of decisions into one number: which rung of
the read-time lookup answered, which method line that rung's table entry came
from, how that line got attached to the key in the first place, and how the
flow's amount was carried onto the factor's basis. 'Method.Mapping' makes all
of that available; this module turns it into an answer.

Two design rules hold the module together.

  * __The engine writes the sentences.__ 'renderResolution' matches
    exhaustively on every constructor of every type it reads, so a new cascade
    rung, unit bridge or refusal cannot ship without its wording: the compiler
    refuses. A consumer that renders the enum itself would drift the moment
    the cascade grew, and an agent asked to interpret a bare code would guess.
  * __Replay, do not record.__ The cascade is pure, so an explanation is
    recomputed for the one flow somebody asked about rather than carried
    through scoring for the tens of thousands nobody will. Scoring keeps its
    pre-multiplied broadcast vector untouched.

Scope: this explains the broadcast cascade, the path 'Method.Mapping.mtBroadcast'
bakes in. Factors that vary by the consuming activity's location need an
activity to resolve against, which a per-flow question does not carry;
'ceRegionalCFCount' reports how many such factors the flow also holds rather
than passing over them in silence.
-}
module Method.Explain (
    -- * The answer
    CFExplanation (..),
    CFResolution (..),
    CFMatch (..),
    explainFlowCF,

    -- * The rungs that were tried
    StepTried (..),
    StepResult (..),

    -- * Batch annotation
    flowMatchKind,

    -- * Rendering
    renderResolution,
    rungName,
    outcomeName,
    vetoName,
    stepName,
    bridgeName,
    refusalName,
) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import ILCD.Writer (formatDouble)
import Method.Mapping (
    BuildProvenance (..),
    CF (..),
    CFUnit (..),
    ConversionOutcome (..),
    DensityDirection (..),
    FlowCFTag (..),
    MatchStrategy (..),
    MethodTables (..),
    RefusalReason (..),
    RungId (..),
    RungOutcome (..),
    TableEntry (..),
    UnitBridge (..),
    VetoReason (..),
    cascadeTrail,
    flowToCFOutcome,
 )
import Method.Types (EnergyDensity (..), MethodCF (..))
import Types (BiosphereFlow, UUID, UnitDB)
import UnitConversion (UnitConfig)

-- | The factor a flow resolved to, and how it got there.
data CFMatch = CFMatch
    { cmRung :: !RungId
    -- ^ The cascade rung that answered.
    , cmCF :: !CF
    -- ^ The factor as served, before the flow amount is applied.
    , cmProvenance :: !BuildProvenance
    -- ^ The method line behind the entry, and how it was attached to the key.
    }
    deriving (Eq, Show)

{- | What became of one flow under one method. The three cases are exactly the
three a bare effective factor of @0@ cannot tell apart.
-}
data CFResolution
    = -- | A factor was found and the amount reaches its basis.
      Characterized !CFMatch !UnitBridge
    | {- | A factor was found but the amount cannot be carried onto its basis,
      so the flow contributes nothing despite looking characterized.
      -}
      ConversionRefused !CFMatch !RefusalReason
    | -- | No rung of the cascade reaches this flow.
      Uncharacterized
    deriving (Eq, Show)

-- | What one rung made of the flow.
data StepResult
    = StepHit
    | StepMiss
    | {- | The rung does not apply to this flow at all (no CAS, no region or
      density suffix, not a long-term emission, not a graded ore).
      -}
      StepNotApplicable
    | -- | A wildcard rung the flow's subcompartment refuses.
      StepVetoed !VetoReason
    | -- | Candidates disagreed and the rung refused to pick one.
      StepAmbiguous
    deriving (Eq, Show)

data StepTried = StepTried
    { stRung :: !RungId
    , stResult :: !StepResult
    }
    deriving (Eq, Show)

-- | A flow's factor, the sentences that explain it, and the rungs it took.
data CFExplanation = CFExplanation
    { ceResolution :: !CFResolution
    , ceTrail :: ![StepTried]
    -- ^ Every rung tried, up to and including the one that answered.
    , ceRegionalCFCount :: !Int
    {- ^ Factors this flow also carries that vary by the consuming activity's
    location, and which therefore no per-flow question can resolve.
    -}
    }
    deriving (Eq, Show)

{- | Replay the read-time cascade for one flow and report what it decided.

Pure and cheap: the same lookups scoring does, for one flow.
-}
explainFlowCF :: UnitConfig -> UnitDB -> MethodTables -> UUID -> BiosphereFlow -> CFExplanation
explainFlowCF unitCfg unitDB tables fid flow =
    CFExplanation
        { ceResolution = resolution
        , ceTrail = [StepTried rung (stepResult outcome) | (rung, outcome) <- walked]
        , ceRegionalCFCount = M.size (M.filterWithKey (\(f, _) _ -> f == fid) (mtRegionalizedCF tables))
        }
  where
    -- A singleton flow DB is what the cascade needs to reach the name, CAS and
    -- compartment rungs, and it is exactly what 'lookupCFForFlow' passes on
    -- the scoring side, so the replay sees what scoring sees.
    trail = cascadeTrail tables (M.singleton fid flow) fid

    walked = case break (isHit . snd) trail of
        (before, hit : _) -> before ++ [hit]
        (before, []) -> before

    isHit (RungHit _) = True
    isHit RungMiss = False
    isHit RungNotApplicable = False
    isHit (RungVetoed _) = False
    isHit RungAmbiguous = False

    stepResult (RungHit _) = StepHit
    stepResult RungMiss = StepMiss
    stepResult RungNotApplicable = StepNotApplicable
    stepResult (RungVetoed reason) = StepVetoed reason
    stepResult RungAmbiguous = StepAmbiguous

    resolution = case [(rung, entry) | (rung, RungHit entry) <- walked] of
        [] -> Uncharacterized
        (rung, entry) : _ ->
            let cf = teCF entry
                match = CFMatch rung cf (teProvenance entry)
             in case flowToCFOutcome unitCfg unitDB (mtEnergyDensities tables) (Just flow) (cfUnit cf) 1.0 of
                    Converted _ bridge -> Characterized match bridge
                    Unconvertible reason -> ConversionRefused match reason

{- | How a flow's factor was found, read from the resolution recorded when the
broadcast vector was filled rather than replayed. This is the cheap answer, for
annotating a whole table of contributing flows at once; 'explainFlowCF' is the
full one, for the flow somebody clicked.

'Nothing' means the tables hold no recorded resolution for this flow: no rung
of the cascade reached it. The tables are built over the flows the database
reaches, its dependencies' included, so a flow arriving from a dependency is
walked like any other and this answer is about the method's coverage, not
about where the flow came from.
-}
flowMatchKind :: MethodTables -> UUID -> Maybe Text
flowMatchKind tables fid = rungName . ftRung <$> M.lookup fid (mtResolution tables)

--------------------------------------------------------------------------------
-- Wire names
--------------------------------------------------------------------------------

{- | Stable name for a cascade rung. These reach clients, so they are spelled
for a reader rather than after the field they index.
-}
rungName :: RungId -> Text
rungName RungUuid = "flow_id"
rungName RungUnitVariant = "same_unit_name"
rungName RungExactName = "exact_name"
rungName RungLongTermDefault = "long_term_default"
rungName RungMediumDefault = "compartment_default"
rungName RungCasBridge = "cas_number"
rungName RungSubBlind = "subcompartment_blind"
rungName RungRegionBase = "region_base_name"
rungName RungEnergyResource = "energy_content"
rungName RungOreGradeBase = "ore_base_element"

-- | Stable name for the three outcomes.
outcomeName :: CFResolution -> Text
outcomeName (Characterized _ _) = "characterized"
outcomeName (ConversionRefused _ _) = "conversion_refused"
outcomeName Uncharacterized = "no_factor"

-- | Stable name for a wildcard veto.
vetoName :: VetoReason -> Text
vetoName ForeignMediumVeto = "different_receiving_medium"
vetoName LongTermUSEtoxVeto = "long_term_groundwater"

-- | Stable name for what one rung made of the flow.
stepName :: StepResult -> Text
stepName StepHit = "hit"
stepName StepMiss = "miss"
stepName StepNotApplicable = "not_applicable"
stepName (StepVetoed _) = "vetoed"
stepName StepAmbiguous = "ambiguous"

{- | Stable name for how the amount reached the factor's basis. The numbers
behind it live in the sentences; this is what a client groups or filters on.
-}
bridgeName :: UnitBridge -> Text
bridgeName UnitsIdentical = "same_unit"
bridgeName (UnitUnknown _) = "unknown_unit"
bridgeName (UnitConverted _ _) = "unit_converted"
bridgeName (NormalizedToBase _) = "normalized_to_base_unit"
bridgeName (EnergyBridged _ _) = "energy_content"

-- | Stable name for why the amount could not reach the factor's basis.
refusalName :: RefusalReason -> Text
refusalName (DimensionalMismatch _ _) = "different_dimensions"
refusalName (NoCanonicalBase _) = "no_base_unit"
refusalName (EnergyBridgeRefused _) = "energy_bridge_failed"

--------------------------------------------------------------------------------
-- Sentences
--------------------------------------------------------------------------------

{- | The explanation in plain sentences, written here so no consumer has to
invent a meaning for a code. Every branch below is exhaustive on purpose: a
new rung, bridge or refusal will not compile until it says what it means.
-}
renderResolution :: CFResolution -> [Text]
renderResolution Uncharacterized =
    ["No factor in this method reaches this flow, so it adds nothing to the score."]
renderResolution (Characterized match bridge) =
    filter (not . T.null) $
        [rungSentence match, provenanceSentence (cmProvenance match), bridgeSentence bridge]
            ++ [factorSentence bridge match]
renderResolution (ConversionRefused match reason) =
    filter
        (not . T.null)
        [rungSentence match, provenanceSentence (cmProvenance match), refusalSentence reason]

-- | Which rung answered, and what that means for this flow.
rungSentence :: CFMatch -> Text
rungSentence (CFMatch rung _ provenance) = case rung of
    RungUuid ->
        "The method declares a factor for this exact flow."
    RungUnitVariant ->
        "No factor carries this flow's name on its own, but " <> lineName <> " is declared in this flow's unit, so that one applies."
    RungExactName ->
        "The factor line " <> lineName <> " matches this flow's name and compartment."
    RungLongTermDefault ->
        "This is a long-term emission and the method sets no factor for its exact subcompartment, so its long-term default, " <> lineName <> ", applies."
    RungMediumDefault ->
        "The method sets no factor for this flow's subcompartment, so its default for the whole compartment, " <> lineName <> ", applies."
    RungCasBridge ->
        "No factor carries this flow's name. " <> lineName <> " describes the same substance" <> casClause <> " in the same compartment, so its factor applies."
    RungSubBlind ->
        "The method gives " <> lineName <> " the same factor in every subcompartment of this compartment, so the subcompartment makes no difference here."
    RungRegionBase ->
        "This flow's name ends in a region the method does not distinguish, so the factor of the base substance, " <> lineName <> ", applies."
    RungEnergyResource ->
        "No factor carries this flow's name. The flow is an energy resource, so its family's factor per unit of energy, from " <> lineName <> ", applies."
    RungOreGradeBase ->
        "This is a graded ore, and its amount is the mass of the base element, so that element's factor, from " <> lineName <> ", applies."
  where
    source = bpSource provenance
    lineName = quoted (mcfFlowName source)
    casClause = case mcfCAS source of
        Just cas | not (T.null cas) -> " (CAS " <> cas <> ")"
        _ -> ""

{- | How the method line reached the key the rung looked under. Silent for a
direct match, where the rung sentence already says it.
-}
provenanceSentence :: BuildProvenance -> Text
provenanceSentence provenance = case bpStrategy provenance of
    Just ByUUID -> ""
    Just ByName -> ""
    Just BySynonym ->
        "That line was tied to this flow's name through a known synonym when the method was loaded."
    Just ByCAS ->
        "That line was tied to this flow by CAS number when the method was loaded."
    Just ByProxy ->
        "That line stands in for a related substance, and its factor was scaled by a curated conversion factor."
    Nothing ->
        "No database flow claimed that line when the method was loaded; it is filed under the name the method itself uses."

{- | The factor as applied, so the sentence stands on its own. The bridge says
which side of the factor its unit sits on: usually the unit is the basis the
factor is per ("per kg"), but under 'NormalizedToBase' it is a result
expression ("kg CO2 eq") – what the factor yields per base unit – and
"per kg CO2 eq" would read the factor backwards.
-}
factorSentence :: UnitBridge -> CFMatch -> Text
factorSentence bridge (CFMatch _ (CF value (CFUnit unit)) _) =
    "The factor applied is " <> formatDouble value <> basis <> "."
  where
    basis = case bridge of
        NormalizedToBase base -> " " <> unit <> " per " <> base
        UnitsIdentical -> perUnit
        UnitUnknown _ -> perUnit
        UnitConverted _ _ -> perUnit
        EnergyBridged _ _ -> perUnit
    perUnit
        | T.null unit = ""
        | otherwise = " per " <> unit

-- | How the flow's amount was carried onto the factor's basis.
bridgeSentence :: UnitBridge -> Text
bridgeSentence UnitsIdentical = ""
bridgeSentence (UnitUnknown flowUnit) =
    "The unit " <> quoted flowUnit <> " is not in the engine's unit table, so the amount was taken as declared."
bridgeSentence (UnitConverted from to) =
    "The amount was converted from " <> from <> " to " <> to <> "."
bridgeSentence (NormalizedToBase base) =
    "The factor is written per " <> base <> ", so the amount was brought to " <> base <> " first."
bridgeSentence (EnergyBridged density direction) =
    densityClause density <> ", which carries the amount " <> way direction <> "."
  where
    way DensityForward = "from " <> edNativeUnit density <> " to " <> edTargetUnit density
    way DensityInverse = "from " <> edTargetUnit density <> " back to " <> edNativeUnit density

-- | Why the amount could not be carried onto the factor's basis.
refusalSentence :: RefusalReason -> Text
refusalSentence (DimensionalMismatch flowUnit cfUnitText) =
    "The factor is written per "
        <> cfUnitText
        <> ", which does not measure the same thing as this flow's "
        <> flowUnit
        <> ". The engine refuses to convert between them, so the flow adds nothing to the score."
refusalSentence (NoCanonicalBase flowUnit) =
    "The unit "
        <> quoted flowUnit
        <> " declares no base unit to normalise to, so the amount cannot be put on the factor's basis and the flow adds nothing to the score."
refusalSentence (EnergyBridgeRefused density) =
    densityClause density
        <> ", but that conversion did not resolve, so the flow adds nothing to the score."

-- | The density a flow name (or the curated table) supplies, spelled out.
densityClause :: EnergyDensity -> Text
densityClause (EnergyDensity value target native) =
    "This flow holds " <> formatDouble value <> " " <> target <> " per " <> native

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

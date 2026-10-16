{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Impacts where

import API.JsonOptions (Stripped (..))
import API.Types.Activity (ParameterValue)
import API.Types.Common
import API.Types.MethodComparison
import Control.DeepSeq (NFData)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map as M
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToSchema (..), declareSchemaRef, properties, required, type_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics
import Types (
    UUID,
 )

-- Synonym types removed - synonyms are now included directly in flow responses

-- | Method summary for listing methods
data MethodSummary = MethodSummary
    { msmId :: UUID -- Method UUID
    , msmName :: Text -- Method name
    , msmCategory :: Text -- Impact category
    , msmUnit :: Text -- Reference unit (e.g., "kg CO2 eq")
    , msmFactorCount :: Int -- Number of characterization factors
    , msmCollection :: Text -- Parent collection name (e.g., "ef-31")
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodSummary)

-- | Method collection list response
data MethodCollectionListResponse = MethodCollectionListResponse
    { mclMethods :: [MethodCollectionStatusAPI]
    , mclDefault :: Maybe Text
    -- ^ The collection the configuration offers a reader first, by name.
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped MethodCollectionListResponse)

-- | Method collection status for API responses
data MethodCollectionStatusAPI = MethodCollectionStatusAPI
    { mcaName :: Text -- Internal identifier
    , mcaDisplayName :: Text -- Human-readable name
    , mcaDescription :: Maybe Text -- Optional description
    , mcaStatus :: Text -- "loaded" | "unloaded"
    , mcaIsUploaded :: Bool -- True if uploaded
    , mcaPath :: Text -- Data path
    , mcaMethodCount :: Int -- Number of impact categories (0 if unloaded)
    , mcaFormat :: Maybe Text -- Format (e.g. "ILCD")
    , mcaSource :: Maybe Text -- The collection this one is a copy of
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped MethodCollectionStatusAPI)

-- | Full method details
data MethodDetail = MethodDetail
    { mdId :: UUID
    , mdName :: Text
    , mdDescription :: Maybe Text
    , mdUnit :: Text
    , mdCategory :: Text
    , mdMethodology :: Maybe Text
    , mdFactorCount :: Int
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodDetail)

{- | Characterization factor for API response. A method routinely carries
several CFs sharing one flow name – same substance emitted to air vs. water,
or one regionalized CF per location – so the distinguishing axes
(compartment, location, unit) travel with each row instead of leaving
consumers with apparent duplicates.
-}
data MethodFactorAPI = MethodFactorAPI
    { mfaFlowRef :: UUID -- ILCD flow UUID
    , mfaFlowName :: Text -- Flow name
    , mfaDirection :: Text -- "Input" or "Output"
    , mfaValue :: Double -- CF value
    , mfaUnit :: Maybe Text -- CF reference unit (e.g. "kg", "kBq"); Nothing when the source method states none
    , mfaCompartment :: Maybe Text -- e.g. "air/urban air", "water/unspecified/long-term"
    , mfaLocation :: Maybe Text -- Consumer location for regionalized CFs
    , mfaReading :: FactorReading
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodFactorAPI)

-- | A single flow's contribution to an LCIA score
data FlowContributionEntry = FlowContributionEntry
    { fcoFlowName :: Text -- Biosphere flow name (e.g. "Carbon dioxide, fossil")
    , fcoContribution :: Double -- Contribution in impact unit
    , fcoSharePct :: Double -- Percentage of total score (0-100)
    , fcoFlowId :: Text -- Flow UUID for disambiguation
    , fcoCategory :: Text -- Medium only (e.g. "air")
    , fcoCompartment :: Maybe Text -- Sub-compartment (e.g. "urban air")
    , fcoCfValue :: Double -- The factor the score applied to this flow. A category whose factors depend on where the flow occurs has several, so the row carries the effective one: its contribution over its quantity
    , fcoMatchKind :: Maybe Text -- How the factor was found ("exact_name", "cas_number", …); absent for a flow no rung of the cascade reached, and on every row of a single score, which weighs several factors into one
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowContributionEntry)

-- | LCIA result for a single impact category
data LCIAResult = LCIAResult
    { lrMethodId :: UUID -- Method UUID
    , lrMethodName :: Text -- Method name
    , lrCategory :: Text -- Impact category
    , lrDamageCategory :: Text -- Parent damage category (may == category)
    , lrScore :: Double -- Total impact score (raw)
    , lrUnit :: Text -- Unit (e.g., "kg CO2 eq")
    , lrNormalizedScore :: Maybe Double -- score * normalization factor
    , lrWeightedScore :: Maybe Double -- normalized * weight (in Pt)
    , lrMappedFlows :: Int -- Number of flows successfully mapped
    , lrFunctionalUnit :: Text -- What the score is per: one unit of the reference product, e.g. "1.00 kg of Butter, unsalted"
    , lrTopContributors :: [FlowContributionEntry] -- Top contributing elementary flows
    , lrWithheld :: Maybe Text -- Set when the licence keeps what weighs in the score to itself: then the contributors are empty
    , lrWithheldDatabases :: [WithheldShare] -- The databases the score reads whose licence keeps their detail: their part in one line each, their flows left out of the contributors
    , lrCutoffInputs :: [CutoffInput] -- The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them
    , lrWithheldCutoffs :: [WithheldCutoffs] -- The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped LCIAResult)

-- | Contributing flows result: top elementary flows for a specific impact category
data ContributingFlowsResult = ContributingFlowsResult
    { cfrMethod :: Text
    , cfrUnit :: Text
    , cfrTotalScore :: Double
    , cfrTopFlows :: [FlowContributionEntry]
    , cfrWithheldDatabases :: [WithheldShare] -- Dependencies whose licence keeps their detail: their flows are not among the top flows, their part is one line each
    , cfrCutoffInputs :: [CutoffInput] -- The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them
    , cfrWithheldCutoffs :: [WithheldCutoffs] -- The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ContributingFlowsResult)

-- | A single activity's contribution to an LCIA score
data ActivityContribution = ActivityContribution
    { acProcessId :: Text -- "activityUUID_productUUID" – usable as API process_id
    , acActivityName :: Text -- e.g. "electricity production, nuclear"
    , acProductName :: Text -- e.g. "electricity, medium voltage"
    , acLocation :: Text -- e.g. "FR"
    , acContribution :: Double -- Contribution in impact unit
    , acSharePct :: Double -- Percentage of total score (0-100)
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ActivityContribution)

-- | Contributing activities result: top upstream activities for a specific impact category
data ContributingActivitiesResult = ContributingActivitiesResult
    { carMethod :: Text
    , carUnit :: Text
    , carTotalScore :: Double
    , carActivities :: [ActivityContribution]
    , carWithheldDatabases :: [WithheldShare] -- Dependencies whose licence keeps their detail: their processes are not listed, their part is one line each
    , carCutoffInputs :: [CutoffInput] -- The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them
    , carWithheldCutoffs :: [WithheldCutoffs] -- The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ContributingActivitiesResult)

{- | Batch impacts request: compute LCIA for every process in one call, at
the database's own values, or once at each set of parameter values.
-}
data BatchImpactsRequest = BatchImpactsRequest
    { birProcessIds :: [Text]
    , birParameterSets :: [[ParameterValue]]
    }
    deriving (Generic)

-- | Sets left unstated are none: the processes are scored at the database's own values.
instance FromJSON BatchImpactsRequest where
    parseJSON = withObject "BatchImpactsRequest" $ \o ->
        BatchImpactsRequest
            <$> o .: "processIds"
            <*> o .:? "parameterSets" .!= []

instance ToSchema BatchImpactsRequest where
    declareNamedSchema _ = do
        processIds <- declareSchemaRef (Proxy :: Proxy [Text])
        parameterSets <- declareSchemaRef (Proxy :: Proxy [[ParameterValue]])
        pure $
            NamedSchema (Just "BatchImpactsRequest") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties .~ InsOrdHashMap.fromList [("processIds", processIds), ("parameterSets", parameterSets)]
                    & required .~ ["processIds"]

-- | One entry of a batch impacts response.
data BatchImpactsEntry = BatchImpactsEntry
    { bieProcessId :: Text
    , bieActivityName :: Text
    , bieParameterSet :: Maybe Int
    -- ^ The position of the parameter set this entry was scored at, when the request gave sets
    , bieImpacts :: LCIABatchResult
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped BatchImpactsEntry)

{- | A batch entry and everything it reaches, forceable. A chunk of a large
batch is forced where it is built so the solutions behind it can go; every
field here is evaluated on the way out anyway, so this moves the work rather
than adding any.
-}
instance NFData BatchImpactsEntry

instance NFData LCIABatchResult

instance NFData LCIAResult

instance NFData FlowContributionEntry

instance NFData ScoringIndicator

{- | Batch impacts response: one entry per successfully computed process,
plus lists of process ids that could not be resolved.
-}
data BatchImpactsResponse = BatchImpactsResponse
    { birResults :: [BatchImpactsEntry]
    , birNotFound :: [Text]
    , birInvalid :: [Text]
    , birUnscorable :: [Text]
    {- ^ Ids that resolve but name an activity the allocation gate refused; the
    quality report's @unallocated@ check says why for each
    -}
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped BatchImpactsResponse)

{- | A single scoring-set indicator: the per-variable normalized-weighted value
plus the impact category it came from. Value is pre-multiplied by the
scoring set's display multiplier, expressed in the set's display unit.
-}
data ScoringIndicator = ScoringIndicator
    { siCategory :: Text
    , siValue :: Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoringIndicator)

-- | Batch LCIA result with optional single score
data LCIABatchResult = LCIABatchResult
    { lbrResults :: [LCIAResult]
    , lbrSingleScore :: Maybe Double -- sum of weighted scores (Pt)
    , lbrSingleScoreUnit :: Maybe Text -- "Pt"
    , lbrNormWeightSetName :: Maybe Text
    , lbrAvailableNWsets :: [Text]
    , lbrScoringResults :: M.Map Text (M.Map Text Double)
    -- ^ Scoring set name → (score name → value). All formula-based scoring sets computed at once.
    , lbrScoringUnits :: M.Map Text Text
    -- ^ Scoring set name → display unit (e.g., "Pts", "µPts PEF")
    , lbrScoringIndicators :: M.Map Text (M.Map Text ScoringIndicator)
    -- ^ Scoring set name → (variable name → indicator). One row per scoring variable.
    , lbrScoringRows :: M.Map Text (M.Map Text ScoringIndicator)
    {- ^ Scoring set name → (row variable → its label and its normalized,
    weighted value), for every row of the set as list_scoring_sets reads it,
    including a row no score reads.
    -}
    , lbrWithheld :: [Text]
    -- ^ What the licence of the scored database keeps to itself, one sentence each: the contributors, the amounts of its unlinked waste
    , lbrCutoffWaste :: [CutoffWasteFlow]
    {- ^ Orphan waste exchanges on the scored activity – flows the dataset author
    left unmodelled. They contribute 0 to the score; surfacing them lets
    consumers see what's excluded rather than silently undercounting.
    -}
    , lbrCutoffInputs :: [CutoffInput]
    -- ^ The cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them; each of 'lbrResults' carries the same list
    , lbrWithheldCutoffs :: [WithheldCutoffs]
    -- ^ The cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped LCIABatchResult)

-- | Flow mapping status for a method
data MappingStatus = MappingStatus
    { mstMethodId :: UUID -- Method UUID
    , mstMethodName :: Text -- Method name
    , mstTotalFactors :: Int -- Total CFs in method
    , mstMappedByUUID :: Int -- Matched by exact UUID
    , mstMappedByCAS :: Int -- Matched by CAS number
    , mstMappedByName :: Int -- Matched by normalized name
    , mstMappedBySynonym :: Int -- Matched via synonym group
    , mstUnmapped :: Int -- Not matched
    , mstCoverage :: Double -- Percentage of mapped flows (0-100)
    , mstDbBiosphereCount :: Int -- Total biosphere flows in the DB
    , mstUniqueDbFlowsMatched :: Int -- Distinct DB flows the method characterizes, counted with the same lookup scoring uses (fallbacks included)
    , mstUnmappedFlows :: [UnmappedFlowAPI] -- Details of unmapped flows
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MappingStatus)

-- | Details about an unmapped flow
data UnmappedFlowAPI = UnmappedFlowAPI
    { ufaFlowRef :: UUID -- Flow UUID in method
    , ufaFlowName :: Text -- Flow name in method
    , ufaDirection :: Text -- "Input" or "Output"
    , ufaCompartment :: Maybe Text -- The compartment the method states, rendered as the factors route renders it ("air", "water/groundwater/long-term"); Nothing when the row states none
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped UnmappedFlowAPI)

-- | DB-flow-centric mapping: all biosphere flows with their CF assignments
data FlowCFMapping = FlowCFMapping
    { fcmMethodName :: Text
    , fcmMethodUnit :: Text
    , fcmTotalFlows :: Int -- Total biosphere flows in DB
    , fcmMatchedFlows :: Int -- How many have a CF
    , fcmFlows :: [FlowCFEntry]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowCFMapping)

-- | A single DB biosphere flow with its CF assignment (if any)
data FlowCFEntry = FlowCFEntry
    { fceFlowId :: UUID
    , fceFlowName :: Text
    , fceFlowCategory :: Text
    , fceCfValue :: Maybe Double -- CF value (Nothing if no match)
    , fceCfFlowName :: Maybe Text -- Method CF flow name
    , fceMatchStrategy :: Maybe Text -- "uuid" | "name" | "synonym"
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowCFEntry)

{- | How much of one database a whole method collection characterizes: the
distinct emission and resource flows that at least one of its methods
resolves a factor for, probed with the same lookup scoring uses.

Distinct is the point. A collection's methods overlap heavily – every
climate-change variant characterizes the same gases – so summing per-method
figures counts a flow once per method that reaches it, and no sum of the
per-method mapping statuses can recover this number.
-}
data CollectionCoverage = CollectionCoverage
    { ccvCollection :: Text
    , ccvDbName :: Text
    , ccvTotalFlows :: Int -- Emission and resource flows the database carries
    , ccvCharacterizedFlows :: Int -- Distinct flows at least one method characterizes
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CollectionCoverage)

-- | Characterization result: matched CFs for a method in a database
data CharacterizationResult = CharacterizationResult
    { chrMethod :: Text
    , chrUnit :: Text
    , chrMatches :: Int
    , chrShown :: Int
    , chrFactors :: [CharacterizationEntry]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CharacterizationResult)

-- | A single matched characterization factor
data CharacterizationEntry = CharacterizationEntry
    { cheMethodFlowName :: Text -- CF flow name from method
    , cheCfValue :: Double -- Characterization factor
    , cheCfUnit :: Text -- CF unit (e.g. "kg")
    , cheDirection :: Text -- "Input" or "Output"
    , cheDbFlowName :: Text -- Matched DB flow name
    , cheFlowId :: Text -- DB flow UUID
    , cheFlowUnit :: Text -- DB flow default unit (e.g. "m3", "kg")
    , cheCategory :: Text -- Flow category
    , cheCompartment :: Maybe Text -- Sub-compartment
    , cheMatchStrategy :: Text -- 'Method.Mapping.strategyToText' of the rung that matched
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CharacterizationEntry)

{- | Why one flow scores with the factor it does.

Two layers on purpose. 'ecrExplanation' is written by the engine and is what a
reader should be shown; the structured fields below it are for a consumer that
wants to compare, filter or link, not for one that wants to restate the
sentences in its own words.
-}
data ExplainCFResult = ExplainCFResult
    { ecrMethodId :: UUID -- What explain-cf and get-characterization take to ask about this method again
    , ecrMethod :: Text
    , ecrMethodUnit :: Text
    , ecrFlow :: ExplainedFlowAPI
    , ecrOutcome :: Text -- "characterized" | "conversion_refused" | "no_factor"
    , ecrExplanation :: [Text] -- Engine-authored sentences, in reading order
    , ecrMatch :: Maybe ExplainedMatchAPI -- Absent when nothing reaches the flow
    , ecrStepsTried :: [ExplainedStepAPI]
    , ecrRegionalFactorCount :: Int -- Factors this flow holds that vary by the consuming activity's location
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainCFResult)

{- | What every loaded collection, or the one asked for, makes of one flow.

A method whose factors reach the flow, applied or refused, or that writes
factors for it by the consuming activity's location, is explained in full under
'cfcFactors'. The others are only named under 'cfcNoFactor': the reason is the
same for each of them, and spelling it out a hundred times would bury the few
lines a reader came for.
-}
data FlowFactorsResult = FlowFactorsResult
    { ffrFlow :: ExplainedFlowAPI
    , ffrCollections :: [CollectionFactors]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FlowFactorsResult)

data CollectionFactors = CollectionFactors
    { cfcCollection :: Text
    , cfcFactors :: [ExplainCFResult]
    , cfcNoFactor :: [MethodSummary]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CollectionFactors)

-- | The flow being explained, as the cascade sees it after normalization.
data ExplainedFlowAPI = ExplainedFlowAPI
    { eflId :: Text
    , eflName :: Text
    , eflUnit :: Text
    , eflCategory :: Text
    , eflCompartment :: Maybe Text
    , eflCas :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainedFlowAPI)

-- | The factor that was served, and where it came from.
data ExplainedMatchAPI = ExplainedMatchAPI
    { emaRung :: Text -- How it was found: "exact_name", "cas_number", "energy_content", …
    , emaCfValue :: Double
    , emaCfUnit :: Text
    , emaMethodFlowName :: Text -- The method line that authored the factor
    , emaMethodCas :: Maybe Text
    , emaMatchStrategy :: Text -- How that line was attached at load time
    , emaUnitConversion :: Maybe Text -- How the amount reached the factor's basis
    , emaRefusal :: Maybe Text -- Why it could not, when outcome is conversion_refused
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainedMatchAPI)

-- | One rung of the cascade, and what it made of the flow.
data ExplainedStepAPI = ExplainedStepAPI
    { estRung :: Text
    , estResult :: Text -- "hit" | "miss" | "not_applicable" | "ambiguous"
    , estVeto :: Maybe Text -- Always absent: no rung vetoes any more. Dropped in 0.15.
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExplainedStepAPI)

{- | A single rank-1 perturbation of a technosphere coefficient @A_ij@.

@delta@ is __relative__: the resolved coefficient @a@ is multiplied by
@(1 + delta)@. So @delta = +0.05@ means \"+5%\", and @delta = -1.0@
removes the link entirely. The kernel passes @a * delta@ to 'perturbA'.
-}
data Perturbation = Perturbation
    { perConsumer :: Text -- Consumer ProcessId (column j of A) – root DB only in V1
    , perSupplier :: Text -- Supplier ProcessId (row i of A) – root DB only in V1
    , perDelta :: Double -- Relative perturbation of A_ij (1 + delta)
    , perLabel :: Maybe Text -- Optional label for response correlation
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Perturbation)

-- | Request body for POST sensitivity endpoints. Flat list, V1.
newtype SensitivityRequest = SensitivityRequest
    { srPerturbations :: [Perturbation]
    }
    deriving (Generic)
    deriving (FromJSON, ToSchema) via (Stripped SensitivityRequest)

{- | One result entry per perturbation. The 'peResult' carries either an
error message ('Left') or the (impact, deltaImpact) pair ('Right'). The
echoed @perturbation@ carries the @label@ if the caller supplied one, so
callers don't need to thread an out-of-band identifier. The wire format
flattens the Either: success → @{perturbation, impact, deltaImpact}@,
failure → @{perturbation, error}@.
-}
data PerturbedEntry = PerturbedEntry
    { pePerturbation :: Perturbation
    , peResult :: Either Text (LCIAResult, Double)
    }
    deriving (Generic)

{- | Manual schema for PerturbedEntry: the Either inside is flattened by ToJSON
to {perturbation, impact, deltaImpact} on success and {perturbation, error}
on failure. The Generic-derived schema would expose the Haskell shape
(a oneOf wrapper around the Either) instead of the flat wire format.
-}
instance ToSchema PerturbedEntry where
    declareNamedSchema _ = do
        pertRef <- declareSchemaRef (Proxy :: Proxy Perturbation)
        lciaRef <- declareSchemaRef (Proxy :: Proxy LCIAResult)
        doubleRef <- declareSchemaRef (Proxy :: Proxy Double)
        textRef <- declareSchemaRef (Proxy :: Proxy Text)
        pure $
            NamedSchema (Just "PerturbedEntry") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("perturbation", pertRef)
                            , ("impact", lciaRef)
                            , ("deltaImpact", doubleRef)
                            , ("error", textRef)
                            ]
                    & required .~ ["perturbation"]

-- | Sensitivity response: baseline LCIA + one entry per perturbation (in order).
data SensitivityResponse = SensitivityResponse
    { srBaseline :: LCIAResult
    , srPerturbed :: [PerturbedEntry]
    , srCutoffInputs :: [CutoffInput] -- The baseline's cut-off inputs: the product inputs this calculation counted as zero, no loaded database supplying them
    , srWithheldCutoffs :: [WithheldCutoffs] -- The baseline's cut-off inputs inside each database whose licence keeps its detail to itself, the one asked included, counted
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped SensitivityResponse)

-- ToJSON / FromJSON / ToSchema for Unit are derived via Stripped alongside
-- its data declaration in src/Types.hs.
-- CutoffWasteFlow (added on main) derives via Stripped attached to its data
-- declaration above.

-- Custom ToJSON for PerturbedEntry: flatten the Either so success entries
-- have impact+deltaImpact and error entries have error.
instance ToJSON PerturbedEntry where
    toJSON (PerturbedEntry p result) =
        object $
            ("perturbation" .= p) : case result of
                Left err -> ["error" .= err]
                Right (lcia, d) -> ["impact" .= lcia, "deltaImpact" .= d]

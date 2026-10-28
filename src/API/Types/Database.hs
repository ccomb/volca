{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Database where

import API.JsonOptions (Stripped (..), parseClosed, stripLowerPrefix)
import Data.Aeson
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import GHC.Generics
import Types (
    AllocationProperty,
    BlockerReason (..),
    Licence,
    ModelTitle,
    Modification,
    Release (..),
    Severity,
 )

-- | One change made to a database, oldest first, in the words its reader uses.
data DatabaseHistoryEntry = DatabaseHistoryEntry
    { dheAt :: Text
    , dheNote :: Maybe Text
    -- ^ Why, in its author's words; absent when they gave no reason
    , dheSummary :: Text
    , dheDetails :: [Text]
    -- ^ Each line the change touched, with its value before when it was kept
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DatabaseHistoryEntry)

-- | Reference data list response (flow synonyms, compartment mappings, units)
newtype RefDataListResponse = RefDataListResponse
    { rdlItems :: [RefDataStatusAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RefDataListResponse)

-- | Reference data status for API responses
data RefDataStatusAPI = RefDataStatusAPI
    { rdaName :: Text
    , rdaDisplayName :: Text
    , rdaDescription :: Maybe Text
    , rdaStatus :: Text -- "loaded" | "unloaded"
    , rdaIsUploaded :: Bool
    , rdaIsAuto :: Bool
    , rdaEntryCount :: Int
    , rdaAdded :: Maybe Text -- When an upload made it, as 2026-10-28T13:30:59Z (UTC, to the second); none for a built-in or extracted table
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RefDataStatusAPI)

-- | Synonym groups response
newtype SynonymGroupsResponse = SynonymGroupsResponse
    { sgrGroups :: [[Text]]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SynonymGroupsResponse)

-- | A reference table as its CSV says it: the header, then rows as wide as it.
data RefDataRowsResponse = RefDataRowsResponse
    { rdrHeader :: [Text]
    , rdrRows :: [[Text]]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RefDataRowsResponse)

-- | Database list response
newtype DatabaseListResponse = DatabaseListResponse
    { dlrDatabases :: [DatabaseStatusAPI] -- All available databases
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DatabaseListResponse)

-- | Database status for API responses
data DatabaseStatusAPI = DatabaseStatusAPI
    { dsaName :: Text -- Internal identifier (slug)
    , dsaDisplayName :: Text -- Human-readable name for UI
    , dsaDescription :: Maybe Text
    , dsaLoadAtStartup :: Bool -- Configured to load at startup
    , dsaStatus :: Text -- "unloaded" | "partially_linked" | "loaded"
    , dsaIsUploaded :: Bool -- True if path starts with "uploads/"
    , dsaPath :: Text -- Data path
    , dsaFormat :: Maybe Text -- Database format (EcoSpold 2, EcoSpold 1, SimaPro CSV)
    , dsaActivityCount :: Int -- Number of activities (0 if unloaded)
    , dsaDependsOn :: [Text] -- Names of databases this one depends on (for cross-DB linking)
    , dsaAllocation :: Text
    {- ^ The key its multi-output blocks were divided under: @declared@, @dry
    mass@, @wet mass@, @price@, or @none@ for a block kept whole. On the wire because a reader looking at a column of
    shares has no other way to tell whether the source stated them or a
    property recomputed them, and a column headed @declared@ over recomputed
    shares says something untrue.
    -}
    , dsaSystemModel :: Maybe ModelTitle -- The system model a child was divided by; its default is dsaAllocation
    , dsaSource :: Maybe Text -- The database whose files it reads, when it owns none (a copy, or the parent of a child)
    , dsaLicence :: Licence -- What it is served under, its source's for a copy
    , dsaRelease :: Maybe Release -- Which published database it is, as its owner declared
    , dsaModification :: Modification -- How its data differs from the files it reads
    , dsaAdded :: Maybe Text
    {- ^ When the engine made it, as @2026-10-28T13:30:59Z@ (UTC, to the
    second, so the text sorts in the order of time). None for a database the
    configuration declares and for one made before the engine kept the
    moment: it is not guessed from the age of a file, which every edit renews.
    -}
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DatabaseStatusAPI)

{- | Response for database activation. A failure is an HTTP error, so
@arSuccess@ is always true: it stays on the wire for clients that still read
it, until 0.16.0.
-}
data ActivateResponse = ActivateResponse
    { arSuccess :: Bool
    , arMessage :: Text
    , arDatabase :: Maybe DatabaseStatusAPI
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivateResponse)

{- | What @GET /api/v1/hosting@ answers: the hosting limits a client may want
to explain before running into them. One type rather than an inline object so
the wire shape has a name a golden test can pin. Keys are written by hand
because this endpoint's wire is snake_case.
-}
data HostingInfo = HostingInfo
    { hiIsHosted :: Bool
    , hiMaxUploads :: Int
    , hiMaxUploadMb :: Int
    , hiMaxLoadedUploads :: Int
    , hiApiAccess :: Bool
    , hiReadOnly :: Bool
    , hiReadOnlyMessage :: Text
    , hiUpgradeUpload :: Text
    , hiUpgradeApi :: Text
    , hiUpgradeVmSize :: Text
    , hiMaxBatchActivities :: Maybe Int -- null when there is no limit
    , hiMaxTopFlows :: Maybe Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON HostingInfo where
    toJSON hi =
        object
            [ "is_hosted" .= hiIsHosted hi
            , "max_uploads" .= hiMaxUploads hi
            , "max_upload_mb" .= hiMaxUploadMb hi
            , "max_loaded_uploads" .= hiMaxLoadedUploads hi
            , "api_access" .= hiApiAccess hi
            , "read_only" .= hiReadOnly hi
            , "read_only_message" .= hiReadOnlyMessage hi
            , "upgrade_upload" .= hiUpgradeUpload hi
            , "upgrade_api" .= hiUpgradeApi hi
            , "upgrade_vm_size" .= hiUpgradeVmSize hi
            , "max_batch_activities" .= hiMaxBatchActivities hi
            , "max_top_flows" .= hiMaxTopFlows hi
            ]

{- | Response for the re-link endpoint: fresh cross-DB link stats after a
second-pass linking against the currently-loaded databases.
-}
data RelinkResponse = RelinkResponse
    { rrDbName :: Text
    , rrUnresolvedBefore :: Int
    , rrUnresolvedAfter :: Int
    , rrCrossDBLinks :: Int
    , rrDependsOn :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RelinkResponse)

{- | Request body for the relink endpoint. Both fields absent (an empty @{}@
body) means a plain relink – re-resolve links within the existing pin. Both
present switches to mapping mode: relink against @rmrDepDb@ using the inline
supplier-alias CSV in @rmrMappingCsv@ (sent inline so the client can
forward a local file without the server needing filesystem access). Supplying
exactly one is rejected – they are only meaningful together.
-}
data RelinkRequest = RelinkRequest
    { rmrDepDb :: Maybe Text
    -- ^ Dependency database to link against (must be a declared dependency)
    , rmrMappingCsv :: Maybe Text
    -- ^ Mapping CSV content (header row + source/target columns, see "Database.RelinkMapping")
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RelinkRequest)

-- | One consuming process of a supplier-gap entry.
data GapConsumerAPI = GapConsumerAPI
    { gcaProcessId :: Text
    , gcaActivityName :: Text
    , gcaProductName :: Text
    , gcaLocation :: Text
    , gcaEdges :: Int
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped GapConsumerAPI)

{- | One supplier gap, aggregated per (product name, supplier activity,
location, unit) so @demandSum@ never mixes units. @supplierActivity@ is the
activity the demands named, @null@ where they named none by name.
@reasons@ carries every stable blocker code the request was refused under ('Types.blockerReason'), plus
@dangling_source_identity@ for inputs whose named source activity no dependency
ships, and @unlinked_waste_input@ for treatment-side waste inputs with no
internal producer. @reason@ and @detail@ are the first of them, kept so a client
written before @reasons@ still decodes; what they answer moves all the same, and
they cannot be made not to. @reason@ used to name whichever blocker the linking
runs merged first and now names the most demanded, and @detail@ is left out
where the refusals under one code disagree on it, having carried one of them
before. A product refused two ways is the ordinary case: read @reasons@.
-}
data GapEntryAPI = GapEntryAPI
    { gaeName :: Text
    , gaeSupplierActivity :: Maybe Text
    , gaeLocation :: Text
    , gaeUnit :: Text
    , gaeReasons :: [BlockerReason]
    , gaeReason :: Text
    , gaeDetail :: Maybe Text
    , gaeEdges :: Int
    , gaeConsumers :: Int
    , gaeDemandSum :: Double
    , gaeTopConsumers :: [GapConsumerAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped GapEntryAPI)

{- | Supplier-gap report of a database: everything still unsupplied after
internal resolution and cross-DB linking, aggregated and ranked by demanding
edges – the work list for switching or completing a background dependency.
-}
data GapReportAPI = GapReportAPI
    { graDbName :: Text
    , graTotalInputs :: Int
    , graInternalLinks :: Int
    , graCrossDBLinks :: Int
    , graUnresolvedEdges :: Int
    , graUnresolvedProducts :: Int
    , graCompleteness :: Double
    , graGaps :: [GapEntryAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped GapReportAPI)

-- | One dataset-soundness finding: where it was found, and what is wrong.
data QualityOffenderAPI = QualityOffenderAPI
    { qoaSeverity :: Severity
    , qoaProcessId :: Text -- "activityUUID_productUUID" – usable as API process_id
    , qoaActivityName :: Text
    , qoaLocation :: Text
    , qoaProductName :: Maybe Text
    , qoaDetail :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped QualityOffenderAPI)

{- | One check of the quality report. @offenderCount@ always covers the whole
finding list, so a list capped by @limit@ stays countable – never a silent cap.
@applicable@ is 'False' when the database carries nothing the check could judge,
which is not the same as passing it.
-}
data QualityCheckAPI = QualityCheckAPI
    { qcaApplicable :: Bool
    , qcaOffenderCount :: Int
    , qcaOffenders :: [QualityOffenderAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped QualityCheckAPI)

{- | Dataset-soundness report of a database: the structural defects a score
can't reveal, one named field per check. The methodological counterpart of
'GapReportAPI', which reports what a database is missing rather than what is
malformed in it.
-}
data QualityReportAPI = QualityReportAPI
    { qraDbName :: Text
    , qraProcessCount :: Int
    , qraReferenceProduct :: QualityCheckAPI
    , qraAllocationSums :: QualityCheckAPI
    , qraDuplicateActivities :: QualityCheckAPI
    , qraDuplicateProducts :: QualityCheckAPI
    , qraSuspiciousAmounts :: QualityCheckAPI
    , qraMissingMetadata :: QualityCheckAPI
    , qraUndeclaredGeography :: QualityCheckAPI
    , qraFormulaConsistency :: QualityCheckAPI
    , qraTruncatedNameCollisions :: QualityCheckAPI
    , qraMissingPedigree :: QualityCheckAPI
    , qraUnconsumedProducts :: QualityCheckAPI
    , qraUnsuppliedInputs :: QualityCheckAPI
    , qraObsoleteInputs :: QualityCheckAPI
    , qraLandTransformationBalance :: QualityCheckAPI
    , qraOxygenDemandOrder :: QualityCheckAPI
    , qraInvalidCas :: QualityCheckAPI
    , qraAllocationOutOfRange :: QualityCheckAPI
    , qraUnallocated :: QualityCheckAPI
    , qraUnmeasurableAmounts :: QualityCheckAPI
    , qraWaterBalance :: QualityCheckAPI
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped QualityReportAPI)

{- | Computed-checks report of a loaded database: what the data computes,
judged against the catalogue's own norms. Same check and offender shape as
'QualityReportAPI', so consumers render both reports alike; a separate
report because these checks need matrices and a method collection, which
the structural report deliberately does not.
-}
data ComputedQualityReportAPI = ComputedQualityReportAPI
    { cqaDbName :: Text
    , cqaCollection :: Text
    , cqaProcessCount :: Int
    , cqaScoreOutliers :: QualityCheckAPI
    , cqaZeroScores :: QualityCheckAPI
    , cqaNegativeScores :: QualityCheckAPI
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ComputedQualityReportAPI)

-- | One database flow a method scores only through a name bridge.
data BridgedFlowAPI = BridgedFlowAPI
    { cvfFlowName :: Text
    , cvfStrategy :: Text -- how the match was reached: 'Method.Mapping.strategyToText'
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BridgedFlowAPI)

{- | The database flows that bridge to one factor. @methodName@ is the name the
method carries for the substance – the name each bridged flow should be renamed
to for the database to score in an exact-name tool.
-}
data BridgeGroupAPI = BridgeGroupAPI
    { cvgCas :: Maybe Text
    , cvgMethodName :: Text
    , cvgBridgedFlows :: [BridgedFlowAPI]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BridgeGroupAPI)

{- | One collection's coverage of the database. @characterizedFlows@ of
@totalFlows@ is the honest reach; @bridgeGroups@ is the subset reached only
through a bridge (empty means every scored flow matches by its exact name).
@bridgeGroupCount@ always covers the whole list, so a list capped by @limit@
stays countable – never a silent cap.
-}
data CollectionBridgesAPI = CollectionBridgesAPI
    { cvcCollection :: Text
    , cvcTotalFlows :: Int
    , cvcCharacterizedFlows :: Int
    , cvcBridgeGroupCount :: Int
    , cvcBridgeGroups :: [BridgeGroupAPI]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CollectionBridgesAPI)

{- | Characterization-coverage report of a database against the loaded method
collections: the flows each collection scores only through a synonym/CAS bridge,
which an exact-name consumer would score as zero. One entry
per loaded collection, so two method versions can be compared side by side.
-}
data CoverageReportAPI = CoverageReportAPI
    { cvrDbName :: Text
    , cvrCollections :: [CollectionBridgesAPI]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped CoverageReportAPI)

-- | Result of auto-loading a single dependency
data DepLoadResult
    = DepLoaded {dlrName :: Text}
    | DepLoadFailed {dlfName :: Text, dlfError :: Text}
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DepLoadResult)

{- | What a derivation makes: a parent of a database read from its files
(@parent: true@), or a child of a parent divided by a system model, given as
the text of its TOML file (@systemModel@). Exactly one of the two.
-}
data DeriveRequest = DeriveRequest
    { drParent :: Maybe Bool
    , drSystemModel :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeriveRequest)

{- | A child's system model: its file as written, each exception with the
processes of the parent it takes, and each substitution with the processes it
names, so a reader can check a rule took what its reason says.
-}
data SystemModelAPI = SystemModelAPI
    { smaText :: Text
    , smaExceptions :: [ModelExceptionAPI]
    , smaSubstitutions :: [ModelSubstitutionAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SystemModelAPI)

data ModelExceptionAPI = ModelExceptionAPI
    { meaKey :: AllocationProperty
    , meaReason :: Text
    , meaProcesses :: [TakenProcessAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ModelExceptionAPI)

-- | A process of the parent an exception takes: its activity and its name.
data TakenProcessAPI = TakenProcessAPI
    { tpaActivity :: Text
    , tpaName :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped TakenProcessAPI)

{- | A coproduct a model substitutes rather than divides, named: its block and
product, the process of the parent it displaces and that process's product,
how much of that product one unit of the coproduct displaces, and why.
-}
data ModelSubstitutionAPI = ModelSubstitutionAPI
    { msaCoproduct :: Text
    , msaBlock :: Text
    , msaCoproductName :: Text
    , msaReplaces :: Text
    , msaReplacedProcess :: Text
    , msaReplacedProduct :: Text
    , msaRatio :: Double
    , msaReason :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ModelSubstitutionAPI)

{- | Response for the load database endpoint. A failure is an HTTP error. The
@"tag": "LoadSucceeded"@ the answer carried when it could also be a failure
stays on the wire for clients that still read it, until 0.16.0.
-}
data LoadDatabaseResponse = LoadSucceeded {ldrDatabase :: DatabaseStatusAPI, ldrDeps :: [DepLoadResult]}
    deriving (Generic)
    deriving (ToSchema) via (Stripped LoadDatabaseResponse)

instance ToJSON LoadDatabaseResponse where
    toJSON = genericToJSON stripLowerPrefix{tagSingleConstructors = True}

instance FromJSON LoadDatabaseResponse where
    parseJSON = genericParseJSON stripLowerPrefix{tagSingleConstructors = True}

{- | One classification filter in a delete request: the @system@ to match, the
@value@ to look for, and whether the match is exact (else token-contains).
Mirrors the search endpoint's classification query parameters so the deleted
set is exactly the searched set.
-}
data DeleteClassFilter = DeleteClassFilter
    { dcfSystem :: Text
    , dcfValue :: Text
    , dcfExact :: Bool
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeleteClassFilter)

data DeleteSelectionRequest = DeleteSelectionRequest
    { dsqName :: Maybe Text -- Filter by activity name
    , dsqLocation :: Maybe Text -- Filter by location
    , dsqProduct :: Maybe Text -- Filter by reference product
    , dsqClassifications :: [DeleteClassFilter] -- Classification filters (AND across systems)
    , dsqExact :: Maybe Bool -- Exact name match (default False)
    , dsqKeep :: [Text] -- Process-id strings to spare from deletion
    , dsqExtra :: [Text] -- Process-id strings to add to deletion
    , dsqIds :: Maybe [Text] -- Delete exactly these process ids (no filter fields allowed)
    , dsqNote :: Maybe Text -- Why, kept in the database's history
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeleteSelectionRequest)

-- | Response for delete-by-selection: count of activities removed.
data DeleteSelectionResponse = DeleteSelectionResponse
    { dsrSuccess :: Bool
    , dsrMessage :: Text
    , dsrDeleted :: Int
    , dsrTransient :: Bool
    {- ^ True when the deletion lives in memory only, because the database is
    one the engine reads from configuration rather than owns. Reloading it
    brings the removed activities back, and a caller has to be able to say
    so rather than let the user assume the change stuck.
    -}
    , dsrWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped DeleteSelectionResponse)

{- | Request for database export. @exrFormat@ is the target-format keyword
(@simapro|ecospold1|ecospold2|ilcd|brightway|openlca@), matching the CLI.
-}
newtype ExportRequest = ExportRequest
    { exrFormat :: Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExportRequest)

{- | Request for a database export: the format, and @package@ (@ro-crate@) to
receive it packaged with the description of its licence and of the releases it
links to. A method collection has no package, so it keeps 'ExportRequest'.
-}
data DatabaseExportRequest = DatabaseExportRequest
    { derFormat :: Text
    , derPackage :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped DatabaseExportRequest)

-- Closed: a misspelt @package@ would be read as absent, and the export sent without its description.
instance FromJSON DatabaseExportRequest where
    parseJSON = parseClosed

{- | Response for database upload. A failure is an HTTP error, so
@uprSuccess@ is always true: it stays on the wire for clients that still read
it, until 0.16.0.
-}
data UploadResponse = UploadResponse
    { uprSuccess :: Bool
    , uprMessage :: Text
    , uprSlug :: Maybe Text -- Generated slug (if successful)
    , uprFormat :: Maybe Text -- Detected format (if successful)
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped UploadResponse)

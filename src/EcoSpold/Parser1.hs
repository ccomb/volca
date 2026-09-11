{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | EcoSpold1 SAX Parser for Ecoinvent 2.x formats
This parser handles the older EcoSpold1 XML format (.XML files)
used in Ecoinvent versions 2.x (e.g., 2.2)
-}
module EcoSpold.Parser1 (
    streamParseActivityAndFlowsFromFile1,
    streamParseAllDatasetsFromFile1,

    -- * XML parsing (exported for testing)
    parseWithXeno,
    parseAllWithXeno,

    -- * Pure helpers (exported for testing)
    generateFlowUUID,
    generateUnitUUID,
) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Either (lefts, rights)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (ParsedDataset (..), bsToDouble, bsToIntMaybe, bsToText, docSection, isElement, joinParts, nonEmptyText)
import Progress (ProgressLevel (..), reportProgress)
import Types
import qualified Xeno.SAX as X

{- | Namespace UUID for generating deterministic UUIDs from EcoSpold1 numeric IDs
Using UUID v5 (SHA1-based) with a custom namespace
-}
ecospold1Namespace :: UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "ecospold1.ecoinvent.org")

{- | Generate deterministic UUID from the exchange number, the full compartment
(category + subCategory) and the unit.

The exchange number is already the identity EcoSpold1 gives a flow across the
whole export: an elementary flow carries its substance number (@Water, fossil@
is 62793 in every dataset that draws it), and a technosphere input carries the
number of the dataset producing it, the same number that dataset stamps on its
own reference product (true of all 11947 datasets of the export measured here).
So the fields below name one flow, once, for the whole database.

The unit is in the key because a matrix row is summed without conversion. Real
exports record one substance in two units: the measured one writes @Heat,
waste@ in MJ in some datasets and in kWh in others, and @Natural gas, at
production@ in m3 and Nm3, 193 such flows in all. Merging those onto one row
would add MJ to kWh and report the total under whichever unit won the merge.
The SimaPro parser keys on the unit for the same reason.

The dataset a flow was *read from* is deliberately not part of the key. Keying
on it splintered every shared substance into one flow per dataset: the export
measured here carried 27935 biosphere flows for 2515 substances, and a single
inventory of it listed @Lead@ 150 times, once per dataset its supply chain
crossed that emits lead.

The subCategory is part of the key because it is part of a flow's identity: an
emission of one substance to two subcompartments (e.g. a leachate to both
@river@ and @groundwater, long-term@) is two distinct flows with distinct
environmental fates and distinct characterization factors. Dropping subCategory
collapsed them onto one UUID, so the matrix summed their amounts into a single
row carrying whichever subcompartment label happened to win the flow-map merge —
silently scoring gated groundwater/ocean mass at a surface-freshwater CF (or the
reverse). Keying on the full compartment keeps each subcompartment a separate row
scored at its own CF.
-}
generateFlowUUID :: Int -> Text -> Text -> Text -> Text -> UUID
generateFlowUUID exchangeNumber flowName category subCategory unitName =
    let key =
            T.intercalate
                ":"
                [ T.pack (show exchangeNumber)
                , flowName
                , category
                , subCategory
                , unitName
                ]
     in UUID5.generateNamed ecospold1Namespace (BS.unpack $ TE.encodeUtf8 key)

-- | Generate deterministic UUID for unit from unit name
generateUnitUUID :: Text -> UUID
generateUnitUUID unitName =
    UUID5.generateNamed ecospold1Namespace (BS.unpack $ TE.encodeUtf8 $ "unit:" <> unitName)

-- ============================================================================
-- Xeno SAX Parser Implementation for EcoSpold1
-- ============================================================================

-- | Element context tracker
data ElementContext
    = InReferenceFunction
    | InGeography
    | InExchange !ExchangeData
    | InInputGroup !ExchangeData -- Keep parent exchange data
    | InOutputGroup !ExchangeData -- Keep parent exchange data
    | Other
    deriving (Eq)

{- | Exchange accumulator for EcoSpold1 format
All data comes from attributes on the <exchange> element
-}
data ExchangeData = ExchangeData
    { exNumber :: !Int -- Exchange number (numeric ID)
    , exName :: !Text -- Flow name
    , exCategory :: !Text -- Category
    , exSubCategory :: !Text -- Subcategory
    , exLocation :: !Text -- Location (for technosphere)
    , exUnit :: !Text -- Unit name
    , exMeanValue :: !Double -- Amount
    , exInputGroup :: !Text -- Input group (1-4 = technosphere input, 4 = resource)
    , exOutputGroup :: !Text -- Output group (0 = reference, 1-3 = byproduct, 4 = emission)
    , exCASNumber :: !Text -- CAS number (optional)
    , exFormula :: !Text -- Chemical formula (optional)
    , exInfrastructure :: !Bool -- Infrastructure process flag
    , exComment :: !Text -- Free-text comment from `generalComment` attribute
    }
    deriving (Eq)

-- | Initial exchange data
emptyExchangeData :: ExchangeData
emptyExchangeData = ExchangeData 0 "" "" "" "" "" 0.0 "" "" "" "" False ""

{- | One @\<source\>@ of the dataset's own bibliography. EcoSpold1 numbers them
within the dataset, and @dataGeneratorAndPublication\@referenceToPublishedSource@
names the one the dataset was published in - the methodological report.
-}
data Source1 = Source1
    { s1Number :: !Int
    , s1FirstAuthor :: !Text
    , s1AdditionalAuthors :: !Text
    , s1Year :: !Text
    , s1Title :: !Text
    , s1TitleOfAnthology :: !Text -- Where ecoinvent puts "ecoinvent report No. 1"
    , s1Publisher :: !Text
    , s1Place :: !Text
    }
    deriving (Eq)

emptySource1 :: Source1
emptySource1 = Source1 0 "" "" "" "" "" "" ""

{- | The provenance a dataset states about itself, accumulated as the metadata
elements go by. Sources and persons are keyed by the number the dataset gives
them, because the elements referring to them (the published source, the
validator) may be read before or after the thing they name.
-}
data DatasetDocs = DatasetDocs
    { ddIncludedProcesses :: !Text
    , ddGeography :: !Text
    , ddTechnology :: !Text
    , ddTimePeriod :: !Text
    , ddPeriodStart :: !Text -- <startYear> or the more precise <startDate>
    , ddPeriodEnd :: !Text
    , ddSampling :: !Text
    , ddExtrapolations :: !Text
    , ddProductionVolume :: !Text
    , ddProofReading :: !Text
    , ddValidator :: !Int -- <person> number of the proof reader
    , ddPublishedSource :: !Int -- <source> number the dataset was published in
    , ddSources :: !(IM.IntMap Source1)
    , ddPersons :: !(IM.IntMap Text)
    , ddPendingSource :: !Source1 -- attributes of the open <source>
    , ddPendingPersonNumber :: !Int -- attributes of the open <person>
    , ddPendingPersonName :: !Text
    }

emptyDatasetDocs :: DatasetDocs
emptyDatasetDocs = DatasetDocs "" "" "" "" "" "" "" "" "" "" 0 0 IM.empty IM.empty emptySource1 0 ""

-- | Parsing state accumulator
data ParseState = ParseState
    { psDatasetNumber :: !Int
    , psActivityName :: !(Maybe Text)
    , psActivityCategory :: !Text
    , psActivitySubCategory :: !Text
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![Exchange]
    , psTechFlows :: ![TechnosphereFlow]
    , psBioFlows :: ![BiosphereFlow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString]
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString]
    , psDocs :: !DatasetDocs -- Provenance the dataset states about itself
    , psUnplacedMedia :: !(S.Set Text)
    {- ^ The categories this dataset filed an elementary exchange under that
    'parseMedium' could not place. Such a flow carries no compartment, so no
    characterization factor reaches it and every method scores it zero: the
    reading names the word instead of letting the zero pass for an answer.
    -}
    , psCompletedActivities :: ![Either String ParsedDataset]
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState =
    ParseState
        { psDatasetNumber = 0
        , psActivityName = Nothing
        , psActivityCategory = ""
        , psActivitySubCategory = ""
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psTechFlows = []
        , psBioFlows = []
        , psUnits = []
        , psPath = []
        , psContext = Other
        , psTextAccum = []
        , psDocs = emptyDatasetDocs
        , psUnplacedMedia = S.empty
        , psCompletedActivities = []
        }

-- ----------------------------------------------------------------------------
-- Shared SAX handlers (used by both parseWithXeno and parseAllWithXeno)
-- ----------------------------------------------------------------------------

-- | Drop the current element from the path, keeping accumulated text.
popPath :: ParseState -> ParseState
popPath s = s{psPath = drop 1 (psPath s)}

-- | Drop the current element from the path and clear accumulated text.
popElement :: ParseState -> ParseState
popElement s = s{psPath = drop 1 (psPath s), psTextAccum = []}

{- | Open tag: push the element onto the path and switch context on the
structural elements we care about. Groups only enter their context from
within an exchange; any other current context is preserved.
-}
onOpenTag :: ParseState -> BS.ByteString -> ParseState
onOpenTag state tagName =
    state{psPath = tagName : psPath state, psContext = newContext, psTextAccum = []}
  where
    enterGroup wrap = case psContext state of
        InExchange edata -> wrap edata
        ctx -> ctx
    newContext
        | isElement tagName "referenceFunction" = InReferenceFunction
        | isElement tagName "geography" = InGeography
        | isElement tagName "exchange" = InExchange emptyExchangeData
        | isElement tagName "inputGroup" = enterGroup InInputGroup
        | isElement tagName "outputGroup" = enterGroup InOutputGroup
        | otherwise = psContext state

-- | Attribute: route by current context to the matching field setter.
onAttribute :: ParseState -> BS.ByteString -> BS.ByteString -> ParseState
onAttribute state name value = case psContext state of
    InReferenceFunction -> setRefFunctionAttr name value state
    InGeography
        | isElement name "location" -> state{psLocation = Just (bsToText value)}
        | isElement name "text" -> setDocs (\d -> d{ddGeography = bsToText value})
        | otherwise -> state
    InExchange edata -> state{psContext = InExchange (setExchangeAttr name value edata)}
    InInputGroup _ -> datasetNumberAttr
    InOutputGroup _ -> datasetNumberAttr
    Other -> docAttr name value datasetNumberAttr
  where
    setDocs f = state{psDocs = f (psDocs state)}
    -- The dataset's numeric id lives on the <dataset> element itself, which is
    -- the head of the path. Accepting it anywhere under <dataset> let the
    -- metadata's own numbered elements (<source>, <person>) overwrite it, so
    -- what was recorded was really the last data generator's id.
    --
    -- A number that will not parse leaves the dataset with none, which drops
    -- it out of the supplier index the same way a dataset carrying no number
    -- does. 'bsToInt' would call @error@ from inside the pure fold instead,
    -- killing the whole load over one malformed attribute.
    datasetNumberAttr
        | isElement name "number"
        , currentElement : _ <- psPath state
        , isElement currentElement "dataset" =
            state{psDatasetNumber = fromMaybe 0 (bsToIntMaybe value)}
        | otherwise = state

{- | Attributes of the metadata elements a dataset states its provenance on.
EcoSpold1 writes each of them as one self-closing element carrying everything
in attributes, so the element currently open is what tells them apart - the
attribute names alone collide (@text@ is on both @\<geography\>@ and
@\<technology\>@, @number@ is on @\<dataset\>@, @\<source\>@ and @\<person\>@).
-}
docAttr :: BS.ByteString -> BS.ByteString -> ParseState -> ParseState
docAttr name value state
    | on "technology" "text" = setDocs (\d -> d{ddTechnology = txt})
    | on "timePeriod" "text" = setDocs (\d -> d{ddTimePeriod = txt})
    | on "representativeness" "samplingProcedure" = setDocs (\d -> d{ddSampling = txt})
    | on "representativeness" "extrapolations" = setDocs (\d -> d{ddExtrapolations = txt})
    | on "representativeness" "productionVolume" = setDocs (\d -> d{ddProductionVolume = txt})
    | on "validation" "proofReadingDetails" = setDocs (\d -> d{ddProofReading = txt})
    | on "validation" "proofReadingValidator" = setDocs (\d -> d{ddValidator = num})
    | on "dataGeneratorAndPublication" "referenceToPublishedSource" = setDocs (\d -> d{ddPublishedSource = num})
    | on "source" "number" = setSource (\s -> s{s1Number = num})
    | on "source" "firstAuthor" = setSource (\s -> s{s1FirstAuthor = txt})
    | on "source" "additionalAuthors" = setSource (\s -> s{s1AdditionalAuthors = txt})
    | on "source" "year" = setSource (\s -> s{s1Year = txt})
    | on "source" "title" = setSource (\s -> s{s1Title = txt})
    | on "source" "titleOfAnthology" = setSource (\s -> s{s1TitleOfAnthology = txt})
    | on "source" "publisher" = setSource (\s -> s{s1Publisher = txt})
    | on "source" "placeOfPublications" = setSource (\s -> s{s1Place = txt})
    | on "person" "number" = setDocs (\d -> d{ddPendingPersonNumber = num})
    | on "person" "name" = setDocs (\d -> d{ddPendingPersonName = txt})
    | otherwise = state
  where
    txt = bsToText value
    -- A number that will not parse leaves the reference at 0, which resolves to
    -- no source and no person rather than killing the load.
    num = fromMaybe 0 (bsToIntMaybe value)
    on element attr = case psPath state of
        current : _ -> isElement current element && isElement name attr
        [] -> False
    setDocs f = state{psDocs = f (psDocs state)}
    setSource f = setDocs (\d -> d{ddPendingSource = f (ddPendingSource d)})

-- | Apply a single referenceFunction attribute to the parse state.
setRefFunctionAttr :: BS.ByteString -> BS.ByteString -> ParseState -> ParseState
setRefFunctionAttr name value st
    | isElement name "name" = st{psActivityName = Just (bsToText value)}
    | isElement name "unit" = st{psRefUnit = Just (bsToText value)}
    | isElement name "category" = st{psActivityCategory = bsToText value}
    | isElement name "subCategory" = st{psActivitySubCategory = bsToText value}
    | isElement name "generalComment"
    , not (BS.null value) =
        st{psDescription = bsToText value : psDescription st}
    | isElement name "includedProcesses" =
        st{psDocs = (psDocs st){ddIncludedProcesses = bsToText value}}
    | otherwise = st

-- | Apply a single exchange attribute to the in-progress exchange.
setExchangeAttr :: BS.ByteString -> BS.ByteString -> ExchangeData -> ExchangeData
setExchangeAttr name value e
    -- An unparseable number leaves the exchange at 0, which merges it with the
    -- other unnumbered exchanges of the same name and compartment rather than
    -- killing the load; 'bsToInt' would call @error@ from inside the fold.
    | isElement name "number" = e{exNumber = fromMaybe 0 (bsToIntMaybe value)}
    | isElement name "name" = e{exName = bsToText value}
    | isElement name "category" = e{exCategory = bsToText value}
    | isElement name "subCategory" = e{exSubCategory = bsToText value}
    | isElement name "location" = e{exLocation = bsToText value}
    | isElement name "unit" = e{exUnit = bsToText value}
    | isElement name "meanValue" = e{exMeanValue = bsToDouble value}
    | isElement name "CASNumber" = e{exCASNumber = bsToText value}
    | isElement name "formula" = e{exFormula = bsToText value}
    | isElement name "infrastructureProcess" = e{exInfrastructure = bsToText value == "true"}
    | isElement name "generalComment" = e{exComment = bsToText value}
    | otherwise = e

-- | End of an opening tag: nothing to do for this format.
onEndOpen :: ParseState -> BS.ByteString -> ParseState
onEndOpen state _tagName = state

-- | Accumulate non-blank text content (also used for CDATA).
onText :: ParseState -> BS.ByteString -> ParseState
onText state content =
    let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
     in if BS.null trimmed
            then state
            else state{psTextAccum = trimmed : psTextAccum state}

-- | Close tag: finalise the element that is ending.
onCloseTag :: ParseState -> BS.ByteString -> ParseState
onCloseTag state tagName
    | isElement tagName "inputGroup" = closeGroup restoreInputGroup (\e t -> e{exInputGroup = t}) state
    | isElement tagName "outputGroup" = closeGroup restoreOutputGroup (\e t -> e{exOutputGroup = t}) state
    | isElement tagName "exchange" = closeExchange state
    | isElement tagName "referenceFunction" = (popElement state){psContext = Other}
    | isElement tagName "geography" = (popElement state){psContext = Other}
    | isElement tagName "source" = closeSource state
    | isElement tagName "person" = closePerson state
    | isElement tagName "startYear" = closeDocText (\d t -> d{ddPeriodStart = statedAsYear (ddPeriodStart d) t}) state
    | isElement tagName "endYear" = closeDocText (\d t -> d{ddPeriodEnd = statedAsYear (ddPeriodEnd d) t}) state
    | isElement tagName "startDate" = closeDocText (\d t -> d{ddPeriodStart = statedAsDate (ddPeriodStart d) t}) state
    | isElement tagName "endDate" = closeDocText (\d t -> d{ddPeriodEnd = statedAsDate (ddPeriodEnd d) t}) state
    | isElement tagName "dataset" = closeDataset state
    | otherwise = popPath state

{- | Close a @\<source\>@: file it under the number the dataset gave it, so
@referenceToPublishedSource@ can name it whichever order the two were read in.
An unnumbered source is dropped - nothing can refer to it, and it would
overwrite the previous one at key 0.
-}
closeSource :: ParseState -> ParseState
closeSource state =
    let docs = psDocs state
        pending = ddPendingSource docs
        filed
            | s1Number pending == 0 = ddSources docs
            | otherwise = IM.insert (s1Number pending) pending (ddSources docs)
     in (popElement state){psDocs = docs{ddSources = filed, ddPendingSource = emptySource1}}

-- | Close a @\<person\>@: file their name under their number, same as a source.
closePerson :: ParseState -> ParseState
closePerson state =
    let docs = psDocs state
        filed
            | ddPendingPersonNumber docs == 0 = ddPersons docs
            | otherwise = IM.insert (ddPendingPersonNumber docs) (ddPendingPersonName docs) (ddPersons docs)
     in (popElement state){psDocs = docs{ddPersons = filed, ddPendingPersonNumber = 0, ddPendingPersonName = ""}}

{- | A period bound the dataset states as a year. EcoSpold1 has both a
@\<startYear\>@ / @\<endYear\>@ pair and a @\<startDate\>@ / @\<endDate\>@ one
(the ESU/BAFU export writes dates on 338 of its 400 datasets, years on the
rest), so a dataset writing both states the same period twice and the year does
not overwrite a date already read.
-}
statedAsYear :: Text -> Text -> Text
statedAsYear existing t
    | T.null (T.strip existing) = T.strip t
    | otherwise = existing

-- | The same bound stated as a full date: the precise form, so it wins.
statedAsDate :: Text -> Text -> Text
statedAsDate existing t
    | T.null (T.strip t) = existing
    | otherwise = T.strip t

-- | Close an element whose documentation value is its text rather than an attribute.
closeDocText :: (DatasetDocs -> Text -> DatasetDocs) -> ParseState -> ParseState
closeDocText setField state =
    (popElement state){psDocs = setField (psDocs state) (T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state))}

{- | Close an input/output group: fold its accumulated text into the parent
exchange's matching group field and return to the exchange context.
@ownGroup@ yields the exchange data to restore when the current context is
the group being closed (or, defensively, a bare exchange); any other context
just pops the element. The opposite group never restores, so a stray
</inputGroup> inside an <outputGroup> (malformed) is ignored, not merged.
-}
closeGroup :: (ElementContext -> Maybe ExchangeData) -> (ExchangeData -> Text -> ExchangeData) -> ParseState -> ParseState
closeGroup ownGroup setField state =
    case ownGroup (psContext state) of
        Just edata -> (popElement state){psContext = InExchange (setField edata txt)}
        Nothing -> popElement state
  where
    txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)

{- | Exchange data to restore when closing an <inputGroup>: the group we opened,
or (defensively) a bare exchange. The opposite group does not restore.
-}
restoreInputGroup :: ElementContext -> Maybe ExchangeData
restoreInputGroup (InInputGroup edata) = Just edata
restoreInputGroup (InExchange edata) = Just edata
restoreInputGroup InOutputGroup{} = Nothing
restoreInputGroup InReferenceFunction = Nothing
restoreInputGroup InGeography = Nothing
restoreInputGroup Other = Nothing

{- | Exchange data to restore when closing an <outputGroup>: the group we opened,
or (defensively) a bare exchange. The opposite group does not restore.
-}
restoreOutputGroup :: ElementContext -> Maybe ExchangeData
restoreOutputGroup (InOutputGroup edata) = Just edata
restoreOutputGroup (InExchange edata) = Just edata
restoreOutputGroup InInputGroup{} = Nothing
restoreOutputGroup InReferenceFunction = Nothing
restoreOutputGroup InGeography = Nothing
restoreOutputGroup Other = Nothing

{- | Close an exchange: build its exchange/flow/unit, accumulate them, and
record the supplier link for technosphere inputs.
-}
closeExchange :: ParseState -> ParseState
closeExchange state = case psContext state of
    InExchange edata ->
        let (exchange, parsedFlow, unit) = buildExchange (psLocation state) edata
            (techs, bios) = case parsedFlow of
                ParsedTech tf -> (tf : psTechFlows state, psBioFlows state)
                ParsedBio bf -> (psTechFlows state, bf : psBioFlows state)
         in (popElement state)
                { psExchanges = exchange : psExchanges state
                , psTechFlows = techs
                , psBioFlows = bios
                , psUnits = unit : psUnits state
                , psUnplacedMedia = maybe id S.insert (unplacedMedium edata parsedFlow) (psUnplacedMedia state)
                , psContext = Other
                }
    InInputGroup _ -> popPath state
    InOutputGroup _ -> popPath state
    InReferenceFunction -> popPath state
    InGeography -> popPath state
    Other -> popPath state

{- | Close a dataset: snapshot the completed activity and reset per-dataset
accumulators for the next one (multi-dataset files).
-}
closeDataset :: ParseState -> ParseState
closeDataset state =
    let !result = buildResult state
     in popPath ((resetDataset state){psCompletedActivities = result : psCompletedActivities state})

{- | Clear per-dataset accumulators, preserving cross-dataset state
(psPath and psCompletedActivities).
-}
resetDataset :: ParseState -> ParseState
resetDataset state =
    state
        { psDatasetNumber = 0
        , psActivityName = Nothing
        , psActivityCategory = ""
        , psActivitySubCategory = ""
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psTechFlows = []
        , psBioFlows = []
        , psUnits = []
        , psContext = Other
        , psTextAccum = []
        , psDocs = emptyDatasetDocs
        , psUnplacedMedia = S.empty
        }

{- | The category an elementary exchange was filed under, when that word names
no medium this reader knows and the flow therefore came back with no
compartment. A technosphere row has no compartment to place, and says nothing.
-}
unplacedMedium :: ExchangeData -> ParsedFlow -> Maybe Text
unplacedMedium edata (ParsedBio flow)
    | isNothing (bfCompartment flow) = Just (exCategory edata)
    | otherwise = Nothing
unplacedMedium _ (ParsedTech _) = Nothing

{- | Build exchange, flow, and unit from exchange data.
@activityLoc@ is the activity's location, used as a biosphere fallback.

EcoSpold1 groups:
  Input:  1-3 = technosphere, 4 = resource (biosphere)
  Output: 0 = reference product, 1-3 = byproduct/co-product, 4 = emission (biosphere)

A row filed under @category="Final waste flows"@ is an elementary flow of
medium 'Waste' whatever group it carries, so it is read as biosphere
before the groups are consulted. Waste that does have a treatment is not
written that way and stays on the technosphere side.
-}
buildExchange :: Maybe Text -> ExchangeData -> (Exchange, ParsedFlow, Unit)
buildExchange activityLoc edata
    | isBiosphere = (bioEx, ParsedBio bioFlow, unit)
    | otherwise = (techEx, ParsedTech techFlow, unit)
  where
    flowId = generateFlowUUID (exNumber edata) (exName edata) category (exSubCategory edata) (exUnit edata)
    unitId = generateUnitUUID (exUnit edata)
    unit = Unit unitId (exUnit edata) (exUnit edata) ""

    inputGroup = exInputGroup edata
    outputGroup = exOutputGroup edata
    isBiosphere = inputGroup == "4" || outputGroup == "4" || isFinalWaste
    isInput = not (T.null inputGroup)
    isReferenceProduct = outputGroup == "0"
    -- Waste with no treatment modelled for it, which an EcoSpold1 export files
    -- under this category. It surfaces on inputGroup=5, the export's way of
    -- fitting a fifth flow class into a 4-type input/output model, but nothing
    -- treats it, so nothing produces it: it is an elementary flow, and reading
    -- it as an input would ask for a supplier no database can provide.
    isFinalWaste = exCategory edata == "Final waste flows"
    -- The medium a method characterizes it under, and what the flow stores.
    category = if isFinalWaste then mediumText Waste else exCategory edata

    -- Technosphere: leave empty if unspecified so the Loader can do name-only
    -- lookup. Biosphere: fall back to the activity location (no supplier link).
    exchangeLocation
        | not (T.null (exLocation edata)) = exLocation edata
        | isBiosphere = fromMaybe "" activityLoc
        | otherwise = ""

    cas = if T.null (exCASNumber edata) then Nothing else Just (exCASNumber edata)

    -- EcoSpold1 never emits ReferenceInput (no waste-treatment encoding here).
    techRoleFor
        | isReferenceProduct = ReferenceProduct
        | isInput = Input
        | otherwise = Coproduct

    {- An EcoSpold 1 input carries the number the dataset producing it is
    published under, on the exchange's own @number@ attribute. An output
    carries its own number, which designates nobody. -}
    supplierClaim :: SupplierClaim
    supplierClaim
        | isInput, not isReferenceProduct, exNumber edata /= 0 = ClaimByDatasetNumber (exNumber edata)
        | otherwise = ClaimByProduct

    subCat = if T.null (exSubCategory edata) then Nothing else Just (exSubCategory edata)
    -- The medium of a group-4 exchange is its @category@, and EcoSpold 1 is
    -- written in more than one vocabulary there: one family of exports names
    -- the bare medium, another states the direction too. 'parseMedium' reads
    -- both. A category it still cannot place leaves the flow with no
    -- compartment, and 'unplacedMedium' carries the word to the dataset's
    -- warnings so the resulting zero score is not silent.
    compartment = case parseMedium category of
        Right medium -> Just (Compartment medium subCat)
        Left _ -> Nothing
    bioFlow = BiosphereFlow flowId (exName edata) unitId M.empty cas Nothing compartment
    bioEx =
        BiosphereExchange
            { bioFlowId = flowId
            , bioAmount = exMeanValue edata
            , bioUnitId = unitId
            , bioDirection = if inputGroup == "4" then Resource else Emission
            , bioLocation = exchangeLocation
            , bioComment = nonEmptyText (exComment edata)
            , bioPedigree = Nothing
            }

    techFlow = TechnosphereFlow flowId (exName edata) unitId M.empty cas Nothing
    techEx =
        TechnosphereExchange
            { techFlowId = flowId
            , techAmount = exMeanValue edata
            , techUnitId = unitId
            , techRole = techRoleFor
            , techActivityLinkId = Nothing
            , techSupplierClaim = supplierClaim
            , techLocation = exchangeLocation
            , techComment = nonEmptyText (exComment edata)
            , techPedigree = Nothing
            , techShare = Nothing
            , techClassification = M.empty
            , techProperties = noProperties
            }

{- | One bibliographic line for a @\<source\>@. ecoinvent files put the
methodological report in @titleOfAnthology@ ("ecoinvent report No. 1"), which is
usually the piece a reader is after, so it follows the title directly.
-}
renderSource :: Source1 -> Text
renderSource s = case joinParts ". " [authors, s1Title s, s1TitleOfAnthology s, publisher] of
    "" -> ""
    line -> line <> "."
  where
    authors = joinParts " " [joinParts ", " [s1FirstAuthor s, s1AdditionalAuthors s], year]
    year = maybe "" (\y -> "(" <> y <> ")") (nonEmptyText (s1Year s))
    publisher = joinParts ", " [s1Publisher s, s1Place s]

{- | The provenance sections of one dataset, in the order a reader wants them:
what the dataset covers, then how it was built, then where it was published and
who vouched for it.
-}
documentationSections :: DatasetDocs -> [DocSection]
documentationSections d =
    concat
        [ docSection "Included processes" (ddIncludedProcesses d)
        , docSection "Geography" (ddGeography d)
        , docSection "Technology" (ddTechnology d)
        , docSection "Time period" (joinParts " " [period, ddTimePeriod d])
        , docSection "Sampling procedure" (ddSampling d)
        , docSection "Extrapolations" (ddExtrapolations d)
        , docSection "Production volume" (ddProductionVolume d)
        , docSection "Published in" (maybe "" renderSource publishedIn)
        , docSection "Sources" (joinParts "\n" (map renderSource otherSources))
        , docSection "Review" (joinParts " " [ddProofReading d, reviewer])
        ]
  where
    period = joinParts " - " [ddPeriodStart d, ddPeriodEnd d]
    publishedIn = IM.lookup (ddPublishedSource d) (ddSources d)
    otherSources = IM.elems (IM.delete (ddPublishedSource d) (ddSources d))
    reviewer = maybe "" (\p -> "(" <> p <> ")") (IM.lookup (ddValidator d) (ddPersons d) >>= nonEmptyText)

{- | The number a dataset is published under, as the identifier its source gave
it. It is what an EcoSpold 1 file prints beside a process and what its own
exchanges name to reach it, so it is the string a reader has in hand.

Zero is 'psDatasetNumber' saying the dataset declared no number, or one that
would not parse, rather than a dataset numbered zero.
-}
datasetIdentifier :: Int -> Maybe NativeProcessId
datasetIdentifier 0 = Nothing
datasetIdentifier n = Just (NativeProcessId (T.pack (show n)))

{- | The fields the dataset left out and this reader stood in for.

A placeholder is indistinguishable downstream from a name the file really
carried, so the substitution is said out loud rather than merged into the data.
The geography is not among them: a dataset that declares none is recorded as
'LocationUnspecified', which says so in the type.
-}
placeholdersUsed :: ParseState -> [Text]
placeholdersUsed st =
    [ datasetPrefix st <> what
    | (what, absent) <-
        [ ("no activity name, read as \"Unknown Activity\"", isNothing (psActivityName st))
        , ("no reference unit, read as \"UNKNOWN_UNIT\"", isNothing (psRefUnit st))
        ]
    , absent
    ]

{- | What a warning about this dataset calls it. A dataset that declared no
number is left unnamed rather than called number zero, which is how
'datasetIdentifier' reads the same field.
-}
datasetPrefix :: ParseState -> Text
datasetPrefix st =
    maybe "" (\(NativeProcessId n) -> "dataset " <> n <> ": ") (datasetIdentifier (psDatasetNumber st))

{- | The compartment vocabularies this dataset used that the reader could not
place, said once for the dataset rather than once per exchange.

An elementary flow with no compartment is characterized by no method, so it
contributes zero to every score while looking like any other flow. The reading
names the words it could not place, which is what a reader needs to add them.
-}
unplacedMediaSeen :: ParseState -> [Text]
unplacedMediaSeen st
    | S.null media = []
    | otherwise =
        [ datasetPrefix st
            <> "elementary exchanges filed under "
            <> T.intercalate ", " (map quoted (S.toList media))
            <> " carry no compartment, so no method characterizes them"
        ]
  where
    media :: S.Set Text
    media = psUnplacedMedia st

    -- A group-4 row can carry no category at all, and "filed under \"\"" would
    -- name nothing; the absence is what there is to report about it.
    quoted :: Text -> Text
    quoted "" = "no category"
    quoted t = "\"" <> t <> "\""

-- | Build the final per-dataset result, applying the cut-off strategy.
buildResult :: ParseState -> Either String ParsedDataset
buildResult st =
    let name = fromMaybe "Unknown Activity" (psActivityName st)
        location = fromMaybe "GLO" (psLocation st)
        -- "GLO" above is this loader's stand-in, not something the dataset said:
        -- a dataset without a geography is recorded as declaring none.
        locationSource = maybe LocationUnspecified declaredLocationSource (psLocation st)
        refUnit = fromMaybe "UNKNOWN_UNIT" (psRefUnit st)
        description = reverse (psDescription st)
        classifications =
            M.fromList $
                filter
                    (not . T.null . snd)
                    [("Category", psActivityCategory st), ("SubCategory", psActivitySubCategory st)]
        activity =
            Activity
                { activityName = name
                , activityDescription = description
                , activityDocumentation = documentationSections (psDocs st)
                , activitySynonyms = M.empty
                , activityClassification = classifications
                , activityLocation = location
                , activityLocationSource = locationSource
                , activityUnit = refUnit
                , exchanges = reverse (psExchanges st)
                , activityParams = M.empty
                , activityParamExprs = M.empty
                , activityNativeType = Nothing
                , activityNativeId = datasetIdentifier (psDatasetNumber st)
                , activityFormulaCheck = Nothing
                }
        pack act =
            ParsedDataset
                { pdActivity = act
                , pdTechFlows = reverse (psTechFlows st)
                , pdBioFlows = reverse (psBioFlows st)
                , -- This format has no waste axis. Waste sent to treatment is a
                  -- technosphere input like any other, and waste with none is an
                  -- elementary flow, so nothing here builds a waste flow.
                  pdWasteFlows = []
                , pdUnits = reverse (psUnits st)
                , pdDatasetNumber = psDatasetNumber st
                , pdWarnings = placeholdersUsed st ++ unplacedMediaSeen st
                }
     in -- A file that yields no exchange at all is not a dataset: a stray or
        -- truncated XML the SAX fold walked through without complaint.
        if null (exchanges activity)
            then Left "not an EcoSpold1 dataset: no exchange found"
            else Right (pack activity)

-- | Run the shared SAX fold, surfacing any Xeno error as a String.
foldEcoSpold1 :: BS.ByteString -> Either String ParseState
foldEcoSpold1 =
    first show . X.fold onOpenTag onAttribute onEndOpen onText onCloseTag onText initialParseState

-- | Xeno SAX parser for EcoSpold1 — first dataset in the file.
parseWithXeno :: BS.ByteString -> Either String ParsedDataset
parseWithXeno xmlContent = do
    finalState <- foldEcoSpold1 xmlContent
    case psCompletedActivities finalState of
        (result : _) -> result
        [] -> buildResult finalState

{- | Parse ALL datasets from an EcoSpold1 file (multi-dataset support).
Outer Either = XML parse failure; inner Either = per-activity failure.
-}
parseAllWithXeno :: BS.ByteString -> Either String [Either String ParsedDataset]
parseAllWithXeno = fmap (reverse . psCompletedActivities) . foldEcoSpold1

-- | Parse EcoSpold1 file using Xeno SAX parser
streamParseActivityAndFlowsFromFile1 :: FilePath -> IO (Either String ParsedDataset)
streamParseActivityAndFlowsFromFile1 path = do
    !xmlContent <- BS.readFile path
    -- The failure is named after its file like the warnings below: read across
    -- a directory of several thousand, a reason that names no file is a reason
    -- nobody can act on.
    let parsed = first ((path ++ ": ") ++) (parseWithXeno xmlContent)
    either (const (pure ())) (reportReading path) parsed
    return parsed

-- | Send what a reading had to say to the progress log, the one place it can go.
reportReading :: FilePath -> ParsedDataset -> IO ()
reportReading path = mapM_ (reportProgress Warning . ((path ++ ": ") ++) . T.unpack) . pdWarnings

-- ============================================================================
-- Multi-dataset file support
-- ============================================================================

{- | Parse ALL datasets from a single EcoSpold1 file
Used for multi-dataset files where <ecoSpold> contains multiple <dataset> elements
Skips activities that fail (e.g. no reference product) and logs warnings

A file that yields not one dataset answers with the reason rather than with an
empty list: the caller has nothing to load and is going to say so, and the only
place the reason would otherwise be is a log line it cannot quote.
-}
streamParseAllDatasetsFromFile1 :: FilePath -> IO (Either String [ParsedDataset])
streamParseAllDatasetsFromFile1 path = do
    !xmlContent <- BS.readFile path
    case parseAllWithXeno xmlContent of
        Left err -> return $ Left (path ++ ": " ++ err)
        Right results -> do
            forM_ (lefts results) $ \e ->
                reportProgress Warning $ "Skipping dataset in " ++ path ++ ": " ++ e
            mapM_ (reportReading path) (rights results)
            return $ case rights results of
                [] -> Left (yieldedNothing path (lefts results))
                kept -> Right kept

{- | Why a file that parsed holds no dataset: what each of them was skipped
for, or that there were none to skip.
-}
yieldedNothing :: FilePath -> [String] -> String
yieldedNothing path [] = path ++ ": the file holds no dataset"
yieldedNothing path skipped = path ++ ": every dataset in it was skipped: " ++ intercalate "; " skipped

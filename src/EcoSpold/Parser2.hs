{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile) where

import Amount (readAmount)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (ParsedDataset (..), bsToDouble, bsToInt, bsToIntMaybe, bsToText, docSection, isElement, joinParts, nonEmptyText)
import qualified Expr
import Progress (ProgressLevel (..), reportProgress)
import SubstanceRegistry (nonEmptyCAS)
import Types
import qualified Xeno.SAX as X

{- | The identifier a row carries, absent where the file wrote none. An
EcoSpold 2 row says the same thing twice with it: it is what the source claims
and, at parse time, what the load has resolved.
-}
nonNil :: UUID.UUID -> Maybe UUID.UUID
nonNil linkUUID
    | linkUUID == UUID.nil = Nothing
    | otherwise = Just linkUUID

{- | Namespace UUID for generating deterministic UUIDs from invalid text
Using UUID v5 (SHA1-based) with a custom namespace for test data compatibility
-}
testDataNamespace :: UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "acvengine.test")

{- | Helper to safely parse UUID from Text, generating deterministic UUID for invalid formats
This ensures test data with invalid UUIDs like "productX-uuid" get unique UUIDs
Returns (UUID, Maybe warning) to avoid unsafePerformIO in pure context
-}
parseUUID :: Text -> (UUID, Maybe String)
parseUUID txt = case UUID.fromText txt of
    Just uuid -> (uuid, Nothing)
    Nothing ->
        -- Generate deterministic UUID from the text using UUID v5
        -- This prevents deduplication issues where all invalid UUIDs would map to nil
        let generatedUUID = UUID5.generateNamed testDataNamespace (BS.unpack $ TE.encodeUtf8 txt)
            -- Only warn for non-empty invalid UUIDs (empty is expected for optional fields)
            warning =
                if T.null txt
                    then Nothing
                    else Just $ "Invalid UUID format: " ++ T.unpack txt ++ " - generated UUID: " ++ show generatedUUID
         in (generatedUUID, warning)

-- ============================================================================
-- Xeno SAX Parser Implementation (8-15x faster than xml-conduit)
-- ============================================================================

-- | Element context tracker - what element are we currently parsing?
data ElementContext
    = InActivityName
    | InGeographyShortname
    | InIntermediateExchange !IntermediateData
    | InElementaryExchange !ElementaryData
    | InGeneralCommentText !Int -- Track index
    | Other
    deriving (Eq)

-- | Intermediate exchange accumulator
data IntermediateData = IntermediateData
    { idFlowId :: !Text
    , idAmount :: !Double
    , idUnitId :: !Text
    , idFlowName :: !Text
    , idUnitName :: !Text
    , idInputGroup :: !Text
    , idOutputGroup :: !Text
    , idActivityLinkId :: !Text
    , idSynonyms :: !(M.Map Text (S.Set Text))
    , idComment :: !(Maybe (Text, Text)) -- (xml:lang, comment text) — English wins
    , idClassifications :: !(M.Map Text Text) -- per-exchange classifications (e.g. By-product classification → Waste)
    , idVariableName :: !Text -- variableName attribute (referencable from other formulas in the dataset)
    , idMathRel :: !Text -- mathematicalRelation attribute (formula defining the amount)
    , idProperties :: !ExchangeProperties -- <property> children, each per unit of the exchange amount
    }
    deriving (Eq)

-- | Elementary exchange accumulator
data ElementaryData = ElementaryData
    { edFlowId :: !Text
    , edAmount :: !Double
    , edUnitId :: !Text
    , edFlowName :: !Text
    , edUnitName :: !Text
    , edInputGroup :: !Text
    , edOutputGroup :: !Text
    , edCompartments :: ![Text]
    , edSubcompartments :: ![Text]
    , edSynonyms :: !(M.Map Text (S.Set Text))
    , edCAS :: !(Maybe Text)
    , edComment :: !(Maybe (Text, Text))
    , edVariableName :: !Text -- variableName attribute (referencable from other formulas in the dataset)
    , edMathRel :: !Text -- mathematicalRelation attribute (formula defining the amount)
    }
    deriving (Eq)

{- | Dataset-local formula metadata carried by an exchange: its own
@variableName@ (referencable from other formulas in the same dataset) and its
@mathematicalRelation@ (checked against the stored amount once the whole
dataset is parsed, since the variables it references may be declared after it
in the file).
-}
data ExchangeFormula = ExchangeFormula
    { efVariableName :: !(Maybe Text)
    , efMathRel :: !(Maybe Text)
    }

-- | Attribute accumulator for the currently-open @\<parameter\>@ element.
data PendingParam = PendingParam
    { ppVariableName :: !Text
    , ppAmount :: !(Maybe Double)
    , ppMathRel :: !Text
    }

emptyPendingParam :: PendingParam
emptyPendingParam = PendingParam "" Nothing ""

{- | The @\<property\>@ currently open, assembled from its @amount@ attribute
and its @\<name\>@ and @\<unitName\>@ children, which arrive in no fixed order
and are only complete when the element closes.
-}
data PendingProperty = PendingProperty
    { ppyName :: !Text
    , ppyUnit :: !Text
    , ppyAmount :: !(Maybe Double)
    }

emptyPendingProperty :: PendingProperty
emptyPendingProperty = PendingProperty "" "" Nothing

{- | File a closed @\<property\>@ under the exchange property it names, if any.

The name is the one ecoinvent writes; a property this engine has no field for
is dropped rather than guessed at, and so is one whose @amount@ the file omits
or writes as something no number can be read from. The schema requires that
attribute, so a file missing it is malformed, and recording the zero the
absence would otherwise leave says the line weighs nothing.

The amount stays as the file states it, per unit of the exchange.
-}
recordProperty :: PendingProperty -> ExchangeProperties -> ExchangeProperties
recordProperty pending props = case (T.toLower (T.strip (ppyName pending)), ppyAmount pending) of
    ("dry mass", Just amount) -> props{epDryMass = Just (stated amount)}
    ("wet mass", Just amount) -> props{epWetMass = Just (stated amount)}
    _ -> props
  where
    stated :: Double -> StatedAmount
    stated amount = StatedAmount{saUnit = T.strip (ppyUnit pending), saAmount = amount}

-- | One @\<review\>@ of the dataset: who read it, when, and what they said.
data Review = Review
    { rvReviewer :: !Text
    , rvDate :: !Text
    , rvText :: !Text
    }

emptyReview :: Review
emptyReview = Review "" "" ""

{- | The provenance a dataset states about itself, accumulated as the metadata
elements go by. Reviews are held in reverse arrival order and flipped back at
the end, the same way exchanges are.
-}
data DatasetDocs = DatasetDocs
    { ddIncludedStart :: !Text
    , ddIncludedEnd :: !Text
    , ddGeography :: !Text
    , ddTechnology :: !Text
    , ddTimePeriod :: !Text
    , ddStartDate :: !Text
    , ddEndDate :: !Text
    , ddSystemModel :: !Text
    , ddSampling :: !Text
    , ddExtrapolations :: !Text
    , ddPublishedAuthor :: !Text
    , ddPublishedYear :: !Text
    , ddPublishedPages :: !Text
    , ddReviews :: ![Review]
    , ddPendingReview :: !Review -- the open <review>, filed when it closes
    , ddElementLang :: !Text -- xml:lang of the element currently open, whichever it is
    }

emptyDatasetDocs :: DatasetDocs
emptyDatasetDocs = DatasetDocs "" "" "" "" "" "" "" "" "" "" "" "" "" [] emptyReview ""

{- | Update a comment slot with a newly seen `<comment xml:lang="…">` text.
Prefer English; otherwise keep the first non-empty entry. Empty / blank
incoming text never overwrites an existing slot.
-}
pickComment :: Maybe (Text, Text) -> Text -> Text -> Maybe (Text, Text)
pickComment existing lang txt =
    let s = T.strip txt
     in if T.null s
            then existing
            else case existing of
                Just ("en", _) -> existing
                _ | lang == "en" -> Just ("en", s)
                Nothing -> Just (lang, s)
                Just _ -> existing

-- | Parsing state accumulator for SAX parsing
data ParseState = ParseState
    { psActivityName :: !(Maybe Text)
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![(Exchange, ExchangeFormula)]
    , psTechFlows :: ![TechnosphereFlow]
    , psBioFlows :: ![BiosphereFlow]
    , psWasteFlows :: ![WasteFlow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString] -- Element path stack
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString] -- Accumulated text content
    , psPendingInputGroup :: !Text -- Pending inputGroup value from child element
    , psPendingOutputGroup :: !Text -- Pending outputGroup value from child element
    , psWarnings :: ![String] -- Accumulated warnings (emitted in IO after fold)
    , psClassifications :: !(M.Map Text Text) -- Classification system -> value
    , psPendingClassSystem :: !Text -- Current classification system name
    , psPendingCommentLang :: !Text -- xml:lang on the currently-open <comment>
    , psActivityType :: !(Maybe Int) -- ecospold2 <activity activityType="1..8"> attribute
    , psSpecialActivityType :: !(Maybe Int) -- ecospold2 <activity specialActivityType="…"> attribute
    , psParams :: !(M.Map Text Double) -- <parameter> variableName → amount (amounts are pre-evaluated in the source)
    , psAmbiguousParams :: !(S.Set Text) -- variableNames declared twice with two amounts, struck from psParams at the end
    , psParamExprs :: !(M.Map Text Text) -- <parameter> variableName → mathematicalRelation (raw formula, for inspection)
    , psPendingParam :: !PendingParam -- attribute accumulator for the open <parameter>
    , psPendingProperty :: !PendingProperty -- attribute and child accumulator for the open <property>
    , psDocs :: !DatasetDocs -- Provenance the dataset states about itself
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState =
    ParseState
        { psActivityName = Nothing
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psTechFlows = []
        , psBioFlows = []
        , psWasteFlows = []
        , psUnits = []
        , psPath = []
        , psContext = Other
        , psTextAccum = []
        , psPendingInputGroup = ""
        , psPendingOutputGroup = ""
        , psWarnings = []
        , psClassifications = M.empty
        , psPendingClassSystem = ""
        , psPendingCommentLang = ""
        , psActivityType = Nothing
        , psSpecialActivityType = Nothing
        , psParams = M.empty
        , psAmbiguousParams = S.empty
        , psParamExprs = M.empty
        , psPendingParam = emptyPendingParam
        , psPendingProperty = emptyPendingProperty
        , psDocs = emptyDatasetDocs
        }

{- | Build the source-native activity-type record from the ecospold2
@activityType@ and @specialActivityType@ attribute values, both verbatim
integers from the XML. Returns 'Nothing' when the primary @activityType@
attribute is absent (we never fabricate a value).

Labels are the ecoinvent v3 schema's documented strings. Unknown codes
keep the integer and yield an empty label rather than a guess.
-}
ecoSpoldNativeType :: Maybe Int -> Maybe Int -> Maybe NativeActivityType
ecoSpoldNativeType Nothing _ = Nothing
ecoSpoldNativeType (Just code) special =
    Just
        EcoSpoldActivityType
            { eatCode = code
            , eatLabel = ecoSpoldActivityTypeLabel code
            , eatSpecialCode = special
            , eatSpecialLabel = ecoSpoldSpecialActivityTypeLabel <$> special
            }

{- | ecospold2 activityType enum labels (v3 schema). Unknown codes yield an
explicit "Unknown (code N)" sentinel so a future spec extension or a
parser bug is visible to consumers, not silently empty.
-}
ecoSpoldActivityTypeLabel :: Int -> Text
ecoSpoldActivityTypeLabel = \case
    1 -> "Ordinary transforming activity"
    2 -> "Market activity"
    3 -> "IO activity"
    4 -> "Residual activity"
    5 -> "Production mix"
    6 -> "Import activity"
    7 -> "Correction activity"
    8 -> "Market group"
    n -> "Unknown (code " <> T.pack (show n) <> ")"

{- | ecospold2 specialActivityType enum labels (v3 schema). Same unknown-code
treatment as 'ecoSpoldActivityTypeLabel'.
-}
ecoSpoldSpecialActivityTypeLabel :: Int -> Text
ecoSpoldSpecialActivityTypeLabel = \case
    0 -> "Default"
    1 -> "Hard link"
    2 -> "Pre-aggregation"
    3 -> "Combined production with byproducts"
    4 -> "Combined production without byproducts"
    5 -> "Combined production"
    6 -> "Import activity"
    n -> "Unknown (code " <> T.pack (show n) <> ")"

-- ============================================================================
-- SAX state combinators (pure, allocation-neutral — inlined into the fold)
-- ============================================================================

-- | Concatenated, entity-decoded text accumulated since the element opened.
accumText :: ParseState -> Text
accumText = T.concat . reverse . map bsToText . psTextAccum

-- | Pop one element off the path stack.
popPath :: ParseState -> ParseState
popPath st = st{psPath = drop 1 (psPath st)}

-- | The common close-tag epilogue: pop the path and discard accumulated text.
popText :: ParseState -> ParseState
popText st = (popPath st){psTextAccum = []}

{- | Transform the open @\<property\>@ accumulator and close the child that
carried the text, whichever child it was.
-}
onPendingProperty :: (PendingProperty -> PendingProperty) -> ParseState -> ParseState
onPendingProperty f st = (popText st){psPendingProperty = f (psPendingProperty st)}

-- | Is the element at the given depth (0 = currently-open tag) named @name@?
pathAt :: Int -> BS.ByteString -> ParseState -> Bool
pathAt depth name st = case drop depth (psPath st) of
    (e : _) -> isElement e name
    [] -> False

{- | Transform the open exchange accumulator, leaving non-exchange contexts
untouched. The exhaustive match lives here so the call sites stay wildcard-free.
-}
mapExchange ::
    (IntermediateData -> IntermediateData) ->
    (ElementaryData -> ElementaryData) ->
    ElementContext ->
    ElementContext
mapExchange fi fe = \case
    InIntermediateExchange d -> InIntermediateExchange (fi d)
    InElementaryExchange d -> InElementaryExchange (fe d)
    InActivityName -> InActivityName
    InGeographyShortname -> InGeographyShortname
    InGeneralCommentText i -> InGeneralCommentText i
    Other -> Other

-- | Apply per-kind updates to the open exchange accumulator, then pop path+text.
onExchange ::
    (IntermediateData -> IntermediateData) ->
    (ElementaryData -> ElementaryData) ->
    ParseState ->
    ParseState
onExchange fi fe st = popText st{psContext = mapExchange fi fe (psContext st)}

-- | The currently-open intermediate exchange, if any.
currentIntermediate :: ParseState -> Maybe IntermediateData
currentIntermediate st = case psContext st of
    InIntermediateExchange d -> Just d
    InElementaryExchange _ -> Nothing
    InActivityName -> Nothing
    InGeographyShortname -> Nothing
    InGeneralCommentText _ -> Nothing
    Other -> Nothing

-- | The currently-open elementary exchange, if any.
currentElementary :: ParseState -> Maybe ElementaryData
currentElementary st = case psContext st of
    InElementaryExchange d -> Just d
    InIntermediateExchange _ -> Nothing
    InActivityName -> Nothing
    InGeographyShortname -> Nothing
    InGeneralCommentText _ -> Nothing
    Other -> Nothing

-- | Are we inside a @generalComment@ @\<text\>@ element?
inGeneralComment :: ParseState -> Bool
inGeneralComment st = case psContext st of
    InGeneralCommentText _ -> True
    InIntermediateExchange _ -> False
    InElementaryExchange _ -> False
    InActivityName -> False
    InGeographyShortname -> False
    Other -> False

{- | Attributes of the metadata elements that carry the dataset's provenance:
the period it covers, where it was published, who reviewed it. Routed by the
element currently open, since the same attribute names appear in several blocks.
-}
docAttr :: BS.ByteString -> BS.ByteString -> ParseState -> ParseState
docAttr name value state
    | on "timePeriod" "startDate" = setDocs (\d -> d{ddStartDate = txt})
    | on "timePeriod" "endDate" = setDocs (\d -> d{ddEndDate = txt})
    | on "dataGeneratorAndPublication" "publishedSourceFirstAuthor" = setDocs (\d -> d{ddPublishedAuthor = txt})
    | on "dataGeneratorAndPublication" "publishedSourceYear" = setDocs (\d -> d{ddPublishedYear = txt})
    | on "dataGeneratorAndPublication" "pageNumbers" = setDocs (\d -> d{ddPublishedPages = txt})
    | on "review" "reviewerName" = setReview (\r -> r{rvReviewer = txt})
    | on "review" "reviewDate" = setReview (\r -> r{rvDate = txt})
    -- ecospold2 states the language on the element carrying the text, not on
    -- the <comment> around it, which is why 'psPendingCommentLang' does not
    -- see it. Kept for whichever element is open, since the multilingual ones
    -- are not only the <text> children.
    | isElement name "xml:lang" = setDocs (\d -> d{ddElementLang = txt})
    | otherwise = state
  where
    txt = bsToText value
    on element attr = pathAt 0 element state && isElement name attr
    setDocs f = state{psDocs = f (psDocs state)}
    setReview f = setDocs (\d -> d{ddPendingReview = f (ddPendingReview d)})

{- | Which documentation field the element now closing belongs to, if any,
given how far up the path the element it documents sits. ecospold2 wraps a free
text in @\<comment\>\<text\>@ and a review's words in @\<details\>\<text\>@, so
closing a @\<text\>@ looks two levels up; files that write the text straight
into the @\<comment\>@ (the shape the exchange comments already accept) look
one level up.
-}
docTextTarget :: Int -> ParseState -> Maybe (DatasetDocs -> Text -> DatasetDocs)
docTextTarget depth state
    | documents "geography" = Just (\d t -> d{ddGeography = paragraph (ddGeography d) t})
    | documents "technology" = Just (\d t -> d{ddTechnology = paragraph (ddTechnology d) t})
    | documents "timePeriod" = Just (\d t -> d{ddTimePeriod = paragraph (ddTimePeriod d) t})
    | documents "review" = Just (\d t -> d{ddPendingReview = appendReview (ddPendingReview d) t})
    | otherwise = Nothing
  where
    documents element = pathAt depth element state
    appendReview r t = r{rvText = paragraph (rvText r) t}

-- | Successive @\<text index="…"\>@ elements are the paragraphs of one field.
paragraph :: Text -> Text -> Text
paragraph existing t = joinParts "\n" [existing, t]

{- | Capture a metadata text into the field it documents. A text stating a
language other than English is skipped, the same preference 'pickComment'
applies to exchange comments; ecospold2 states it on the @\<text\>@ when there
is one and on the @\<comment\>@ otherwise, so the caller passes whichever it
read.
-}
captureDocText :: Int -> Text -> ParseState -> ParseState
captureDocText depth lang state = case docTextTarget depth state of
    Just setField | readableAsEnglish lang -> state{psDocs = setField (psDocs state) (accumText state)}
    _ -> state

{- | Is a stated language one this reader keeps? An element stating none is
taken at its word; ecospold2 repeats a translated element once per language,
and keeping them all would leave whichever came last.
-}
readableAsEnglish :: Text -> Bool
readableAsEnglish lang = lang `elem` ["", "en"]

{- | Close an element whose whole text content is one documentation field.

@samplingProcedure@, @extrapolations@ and the rest state their language on
themselves rather than on a @\<text\>@ child, and a dataset that says the same
thing twice writes the element twice. Skipping the other languages is what
stops the German version from replacing the English one; refusing a blank is
what stops an empty repeat from erasing the section.
-}
closeDocText :: (DatasetDocs -> Text -> DatasetDocs) -> ParseState -> ParseState
closeDocText setField state = case nonEmptyText (accumText state) of
    Just txt | readableAsEnglish (ddElementLang (psDocs state)) -> popText state{psDocs = setField (psDocs state) txt}
    _ -> popText state

{- | Close a @\<review\>@, keeping what a person signed and dropping what the
database's own checker filed.

ecoinvent runs an automated check and files its report as a review under the
reviewer name @[System]@: mass-balance warnings and property deviations,
kilobytes per dataset addressed to the build process rather than to a reader.
Which of the two a review is cannot be read off its content - measured over
1500 ecoinvent datasets, 488 of those machine reports are written in
@\<details\>@ exactly like a person's, and 578 reviews signed by a named person
carry no text at all, only who passed the dataset and when. So the reviewer's
name is what decides, and the only other review dropped is one that says
nothing whatsoever.
-}
closeReview :: ParseState -> ParseState
closeReview state =
    let docs = psDocs state
        pending = ddPendingReview docs
        automated = T.strip (rvReviewer pending) == "[System]"
        saysNothing = T.null (joinParts " " [rvReviewer pending, rvDate pending, rvText pending])
        kept
            | automated || saysNothing = ddReviews docs
            | otherwise = pending : ddReviews docs
     in popText state{psDocs = docs{ddReviews = kept, ddPendingReview = emptyReview}}

{- | The provenance sections of one dataset, in the order a reader wants them:
what the dataset covers, then how it was built, then where it was published and
who vouched for it.
-}
documentationSections :: DatasetDocs -> [DocSection]
documentationSections d =
    concat
        [ docSection "Included activities" (joinParts " " [ddIncludedStart d, ddIncludedEnd d])
        , docSection "Geography" (ddGeography d)
        , docSection "Technology" (ddTechnology d)
        , docSection "Time period" (joinParts " " [period, ddTimePeriod d])
        , docSection "System model" (ddSystemModel d)
        , docSection "Sampling procedure" (ddSampling d)
        , docSection "Extrapolations" (ddExtrapolations d)
        , docSection "Published in" (joinParts ", " [publishedBy, ddPublishedPages d])
        , docSection "Review" (joinParts "\n" (map renderReview (reverse (ddReviews d))))
        ]
  where
    period = joinParts " - " [ddStartDate d, ddEndDate d]
    publishedBy = joinParts " " [ddPublishedAuthor d, parenthesised (ddPublishedYear d)]
    renderReview r = joinParts ": " [joinParts " " [rvReviewer r, parenthesised (rvDate r)], rvText r]
    parenthesised = maybe "" (\t -> "(" <> t <> ")") . nonEmptyText

-- | Build a 'Unit', substituting placeholders for a missing unit name.
mkUnit :: UUID -> Text -> Unit
mkUnit uuid name
    | T.null name = Unit uuid "UNKNOWN_UNIT" "?" ""
    | otherwise = Unit uuid name name ""

{- | Resolve in/out group: prefer the attribute value, fall back to the pending
value captured from the child @\<inputGroup\>@ / @\<outputGroup\>@ element.
-}
resolveGroups :: Text -> Text -> ParseState -> (Text, Text)
resolveGroups inG outG st =
    ( if T.null inG then psPendingInputGroup st else inG
    , if T.null outG then psPendingOutputGroup st else outG
    )

-- | Warning emitted (as a singleton, else empty) when an exchange has no unit name.
missingUnitWarning :: String -> Text -> Text -> [String]
missingUnitWarning kind flowId unitNm =
    [ "[WARNING] Missing unit name for "
        ++ kind
        ++ " exchange with flow ID: "
        ++ T.unpack flowId
        ++ " - using 'UNKNOWN_UNIT' placeholder"
    | T.null unitNm
    ]

-- | Parse a UUID, treating the empty string as the nil UUID (no warning).
parseUUIDOrNil :: Text -> (UUID, Maybe String)
parseUUIDOrNil t
    | T.null t = (UUID.nil, Nothing)
    | otherwise = parseUUID t

-- | Second argument when non-blank, otherwise the fallback.
nonBlankOr :: Text -> Text -> Text
nonBlankOr fallback t = if T.null t then fallback else t

addExchange :: Exchange -> ExchangeFormula -> ParseState -> ParseState
addExchange ex ef st = st{psExchanges = (ex, ef) : psExchanges st}

addTechFlow :: TechnosphereFlow -> ParseState -> ParseState
addTechFlow f st = st{psTechFlows = f : psTechFlows st}

addBioFlow :: BiosphereFlow -> ParseState -> ParseState
addBioFlow f st = st{psBioFlows = f : psBioFlows st}

addWasteFlow :: WasteFlow -> ParseState -> ParseState
addWasteFlow f st = st{psWasteFlows = f : psWasteFlows st}

{- | Common exchange-close bookkeeping: leave the exchange context, pop the
path/text, clear pending groups, record the unit and any warnings.
-}
finishExchange :: Unit -> [String] -> ParseState -> ParseState
finishExchange unit warns st =
    (popText st)
        { psContext = Other
        , psPendingInputGroup = ""
        , psPendingOutputGroup = ""
        , psUnits = unit : psUnits st
        , psWarnings = warns ++ psWarnings st
        }

{- | Check exchange @mathematicalRelation@ formulas against the dataset-local
environment: the dataset's @\<parameter\>@ variables plus every exchange's own
@variableName@ bound to its stored amount.

The stored amount always stays authoritative. EcoSpold2 sources carry
pre-evaluated amounts, and this evaluator only supports a subset of the
formula language (no @UnitConversion@, no cross-dataset @Ref@) with
SimaPro-flavoured semantics, so its result serves as a consistency check, not
a replacement. Divergences are expected in real EcoSpold2 databases
(system-model exports rescale amounts without updating the copied formulas),
so nothing is logged: the outcome is recorded on the activity for the
database quality report.
-}
checkFormulas :: M.Map Text Double -> [(Exchange, ExchangeFormula)] -> Maybe FormulaCheck
checkFormulas params pairs = case checked of
    [] -> Nothing
    _ ->
        Just
            FormulaCheck
                { fcEvaluated = length evaluated
                , fcDivergent = length divergent
                , fcUnevaluable = length [() | (_, _, Left _) <- checked]
                , fcExample = listToMaybe (map example divergent)
                }
  where
    -- A <parameter> and an exchange variableName share one space of names, so
    -- both go in together and a name they disagree on is as unreadable as one
    -- two exchanges disagree on.
    env :: M.Map Text Double
    env =
        amountsAgreedOn $
            M.toList params
                ++ [(v, exchangeAmount ex) | (ex, ef) <- pairs, Just v <- [efVariableName ef]]
    checked = [(ex, rel, Expr.evaluate env (Expr.normalizeExpr '.' rel)) | (ex, ef) <- pairs, Just rel <- [efMathRel ef]]
    evaluated = [(ex, rel, v) | (ex, rel, Right v) <- checked]
    divergent = [(ex, rel, v) | (ex, rel, v) <- evaluated, not (nearlyEqual v (exchangeAmount ex))]
    example (ex, rel, v) =
        "\""
            <> rel
            <> "\" evaluates to "
            <> T.pack (show v)
            <> " but the dataset stores "
            <> T.pack (show (exchangeAmount ex))
    nearlyEqual a b = abs (a - b) <= 1e-9 * max 1 (max (abs a) (abs b))

{- | Bind each variable the declarations agree on, and leave the rest unbound.

Two declarations of one name with two amounts leave the formulas that use it no
single reading, and taking whichever was read last would make the check above
answer on a coin toss. Unbound, those formulas come back unevaluable, which is
what this check already reports when it cannot judge.

The names are folded to lower case because 'Expr.evaluate' looks them up that
way: judging agreement on the exact spelling would let @Water@ and @water@ both
bind, and the evaluator would then silently keep one of the two.
-}
amountsAgreedOn :: [(Text, Double)] -> M.Map Text Double
amountsAgreedOn kvs = M.mapMaybe agreed (M.fromListWith (++) [(T.toLower k, [v]) | (k, v) <- kvs])
  where
    agreed :: [Double] -> Maybe Double
    agreed [] = Nothing
    agreed (v : vs) = if all (== v) vs then Just v else Nothing

{- | The fields the dataset left out and this reader stood in for.

A placeholder is indistinguishable downstream from a name the file really
carried, so the substitution is said out loud rather than merged into the data.
The geography is not among them: a dataset that declares none is recorded as
'LocationUnspecified', which says so in the type.
-}
placeholdersUsed :: ParseState -> [Text]
placeholdersUsed st =
    [ what
    | (what, absent) <-
        [ ("no activity name, read as \"Unknown Activity\"", isNothing (psActivityName st))
        , ("no reference unit, read as \"UNKNOWN_UNIT\"", isNothing (psRefUnit st))
        ]
    , absent
    ]

-- | Xeno SAX parser implementation
parseWithXeno :: BS.ByteString -> Either String ParsedDataset
parseWithXeno xmlContent = do
    finalState <- first show (X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent)
    buildResult finalState
  where
    -- Open tag handler - update path and context
    openTag state tagName =
        let newPath = tagName : psPath state
            cleanState
                | isElement tagName "intermediateExchange" || isElement tagName "elementaryExchange" =
                    state{psPendingInputGroup = "", psPendingOutputGroup = ""}
                | isElement tagName "comment" =
                    state{psPendingCommentLang = ""}
                | isElement tagName "parameter" =
                    state{psPendingParam = emptyPendingParam}
                | isElement tagName "property" =
                    state{psPendingProperty = emptyPendingProperty}
                | otherwise = state
            newContext
                | isElement tagName "activityName" = InActivityName
                | isElement tagName "shortname" && any (isElement "geography") (psPath cleanState) = InGeographyShortname
                | isElement tagName "intermediateExchange" =
                    InIntermediateExchange (IntermediateData "" 0.0 "" "" "" "" "" "" M.empty Nothing M.empty "" "" noProperties)
                | isElement tagName "elementaryExchange" =
                    InElementaryExchange (ElementaryData "" 0.0 "" "" "" "" "" [] [] M.empty Nothing Nothing "" "")
                | isElement tagName "text" && any (isElement "generalComment") (psPath cleanState) = InGeneralCommentText 0
                -- Classification elements: don't switch context. Handled via psTextAccum + psPendingClassSystem.
                -- Switching context here would destroy InIntermediateExchange when classifications appear inside exchanges.
                -- DON'T switch context for child elements (synonym, compartment, etc) - keep parent exchange context
                | otherwise = psContext cleanState
         in -- The language is forgotten at every open tag, so an element
            -- stating none is read as unstated rather than as its
            -- predecessor's.
            cleanState
                { psPath = newPath
                , psContext = newContext
                , psTextAccum = []
                , psDocs = (psDocs cleanState){ddElementLang = ""}
                }

    -- Attribute handler - extract attributes
    attribute state name value =
        let isInsideProperty = pathAt 0 "property" state
            -- The amount and unit an open <property> states. The exchange arms
            -- above refuse both under the same guard, so this is the only
            -- reader of them.
            onProperty st
                | isInsideProperty && isElement name "amount" =
                    st{psPendingProperty = (psPendingProperty st){ppyAmount = readAmount (bsToText value)}}
                | otherwise = st
            -- xml:lang on the currently-open <comment>; remembered until closeTag.
            -- Attribute order is not significant for entity ref selection — we
            -- only need the lang at close-time.
            withLang st
                | pathAt 0 "comment" state && isElement name "xml:lang" = st{psPendingCommentLang = bsToText value}
                | otherwise = st
         in case psContext state of
                InIntermediateExchange idata ->
                    let updated
                            | isElement name "intermediateExchangeId" = idata{idFlowId = bsToText value}
                            | isElement name "amount" && not isInsideProperty = idata{idAmount = bsToDouble value}
                            | isElement name "unitId" && not isInsideProperty = idata{idUnitId = bsToText value}
                            | isElement name "inputGroup" = idata{idInputGroup = bsToText value}
                            | isElement name "outputGroup" = idata{idOutputGroup = bsToText value}
                            | isElement name "activityLinkId" = idata{idActivityLinkId = bsToText value}
                            | isElement name "variableName" && not isInsideProperty = idata{idVariableName = bsToText value}
                            | isElement name "mathematicalRelation" && not isInsideProperty = idata{idMathRel = bsToText value}
                            | otherwise = idata
                     in onProperty (withLang state{psContext = InIntermediateExchange updated})
                InElementaryExchange edata ->
                    let updated
                            | isElement name "elementaryExchangeId" = edata{edFlowId = bsToText value}
                            | isElement name "amount" && not isInsideProperty = edata{edAmount = bsToDouble value}
                            | isElement name "unitId" && not isInsideProperty = edata{edUnitId = bsToText value}
                            | isElement name "inputGroup" = edata{edInputGroup = bsToText value}
                            | isElement name "outputGroup" = edata{edOutputGroup = bsToText value}
                            | isElement name "casNumber" = edata{edCAS = nonEmptyCAS (bsToText value)}
                            | isElement name "variableName" && not isInsideProperty = edata{edVariableName = bsToText value}
                            | isElement name "mathematicalRelation" && not isInsideProperty = edata{edMathRel = bsToText value}
                            | otherwise = edata
                     in onProperty (withLang state{psContext = InElementaryExchange updated})
                InGeneralCommentText _ ->
                    let idx = if isElement name "index" then bsToInt value else 0
                     in withLang state{psContext = InGeneralCommentText idx}
                _ ->
                    -- Attributes on the <activity> opening tag carry the
                    -- ecospold2 activityType and specialActivityType enums;
                    -- attributes on a <parameter> carry its variableName,
                    -- pre-evaluated amount and mathematicalRelation formula.
                    let onActivity = pathAt 0 "activity" state
                        onParameter = pathAt 0 "parameter" state
                        pending = psPendingParam state
                        captured
                            | onActivity && isElement name "activityType" =
                                state{psActivityType = bsToIntMaybe value}
                            | onActivity && isElement name "specialActivityType" =
                                state{psSpecialActivityType = bsToIntMaybe value}
                            | onParameter && isElement name "variableName" =
                                state{psPendingParam = pending{ppVariableName = bsToText value}}
                            | onParameter && isElement name "amount" =
                                state{psPendingParam = pending{ppAmount = readAmount (bsToText value)}}
                            | onParameter && isElement name "mathematicalRelation" =
                                state{psPendingParam = pending{ppMathRel = bsToText value}}
                            | otherwise = docAttr name value state
                     in onProperty (withLang captured)

    -- End of opening tag - no action needed for SAX
    endOpen state _tagName = state

    -- Text content handler - accumulate text
    text state content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content -- Trim spaces
         in if BS.null trimmed
                then state
                else state{psTextAccum = trimmed : psTextAccum state}

    -- Close tag handler - finalize elements
    closeTag state tagName
        | isElement tagName "activityName" =
            (popText state){psActivityName = Just (accumText state), psContext = Other}
        | isElement tagName "comment" =
            -- Capture <comment> text only when the immediate parent is the
            -- exchange itself, not a nested <property>. Property comments
            -- describe the property (e.g. "carbon content"), not the exchange.
            let txt = accumText state
                lang = psPendingCommentLang state
                st' =
                    onExchange
                        (\d -> if pathAt 1 "intermediateExchange" state then d{idComment = pickComment (idComment d) lang txt} else d)
                        (\d -> if pathAt 1 "elementaryExchange" state then d{edComment = pickComment (edComment d) lang txt} else d)
                        -- A metadata comment written without a <text> child is
                        -- captured here, before onExchange pops the path.
                        (captureDocText 1 lang state)
             in st'{psPendingCommentLang = ""}
        | isElement tagName "shortname" && psContext state == InGeographyShortname =
            (popText state){psLocation = Just (accumText state), psContext = Other}
        | isElement tagName "intermediateExchange" =
            case currentIntermediate state of
                Nothing -> popPath state
                Just idata ->
                    let (finalInputGroup, finalOutputGroup) = resolveGroups (idInputGroup idata) (idOutputGroup idata) state
                        isInput = not (T.null finalInputGroup)
                        isOutput = T.null finalInputGroup
                        -- Reference products are identified ONLY by outputGroup="0"; this holds for
                        -- normal production (positive amount) and waste treatment (negative amount).
                        -- Negative inputs (e.g. wastewater discharge) are never reference products.
                        isReferenceProduct = isOutput && finalOutputGroup == "0"
                        -- The one waste marker EcoSpold2 carries: an intermediateExchange
                        -- classified (System='By-product classification', Value='Waste'), a waste
                        -- output that consumers treat via a treatment activity.
                        isWasteFlow = M.lookup "By-product classification" (idClassifications idata) == Just "Waste"
                        (flowUUID, flowWarn) = parseUUID (idFlowId idata)
                        (unitUUID, unitWarn) = parseUUID (idUnitId idata)
                        (linkUUID, linkWarn) = parseUUIDOrNil (idActivityLinkId idata)
                        warns =
                            catMaybes [flowWarn, unitWarn, linkWarn]
                                ++ missingUnitWarning "intermediate" (idFlowId idata) (idUnitName idata)
                        techRoleFor
                            | isReferenceProduct = ReferenceProduct
                            | isInput = Input
                            | otherwise = Coproduct
                        resolvedFlowName = nonBlankOr (idFlowId idata) (idFlowName idata)
                        unit = mkUnit unitUUID (idUnitName idata)
                        techExchange =
                            TechnosphereExchange
                                { techFlowId = flowUUID
                                , techAmount = idAmount idata
                                , techUnitId = unitUUID
                                , techRole = techRoleFor
                                , techActivityLinkId = nonNil linkUUID
                                , techSupplierClaim = maybe ClaimByProduct ClaimById (nonNil linkUUID)
                                , techLocation = "" -- EcoSpold2: no per-exchange location
                                , techComment = snd <$> idComment idata
                                , techPedigree = Nothing
                                , techShare = Nothing
                                , techClassification = M.empty
                                , techProperties = idProperties idata
                                }
                        techFlow = TechnosphereFlow flowUUID resolvedFlowName unitUUID (idSynonyms idata) Nothing Nothing
                        wasteExchange =
                            WasteExchange
                                { waFlowId = flowUUID
                                , waAmount = idAmount idata
                                , waUnitId = unitUUID
                                , waIsInput = isInput
                                , waActivityLinkId = nonNil linkUUID
                                , waSupplierClaim = maybe ClaimByProduct ClaimById (nonNil linkUUID)
                                , waLocation = ""
                                , waComment = snd <$> idComment idata
                                , waPedigree = Nothing
                                }
                        wasteFlow = WasteFlow flowUUID resolvedFlowName unitUUID (idSynonyms idata) Nothing Nothing
                        -- A waste-classified flow normally routes to the waste axis. The one
                        -- exception is the reference flow of a waste-treatment / market-for-waste
                        -- activity: it is itself waste (negative amount, outputGroup="0") yet it
                        -- IS the reference product. Diverting it to the waste axis leaves the
                        -- activity with no reference product, so 'applyCutoffStrategy' rejects it
                        -- and the whole dataset is dropped — silently severing every input that
                        -- links into the treatment subsystem.
                        refOnWasteAxis = isWasteFlow && not isReferenceProduct
                        newRefUnit =
                            if isReferenceProduct && not (T.null (idUnitName idata))
                                then Just (idUnitName idata)
                                else psRefUnit state
                        formula = ExchangeFormula (nonEmptyText (idVariableName idata)) (nonEmptyText (idMathRel idata))
                        base = (finishExchange unit warns state){psRefUnit = newRefUnit}
                     in if refOnWasteAxis
                            then addExchange wasteExchange formula (addWasteFlow wasteFlow base)
                            else addExchange techExchange formula (addTechFlow techFlow base)
        | isElement tagName "elementaryExchange" =
            case currentElementary state of
                Nothing -> popPath state
                Just edata ->
                    let (finalInputGroup, finalOutputGroup) = resolveGroups (edInputGroup edata) (edOutputGroup edata) state
                        -- A missing compartment becomes 'Nothing', not an empty 'Compartment ""'
                        -- sentinel — the latter used to silently collide with method-side empty mediums.
                        -- A medium the reader cannot place is reported and the
                        -- compartment dropped, rather than carried as a string
                        -- nothing downstream can bucket.
                        mediumReading = case edCompartments edata of
                            (c : _) | not (T.null c) -> Just (parseMedium c)
                            _ -> Nothing
                        mCompName = mediumReading >>= either (const Nothing) Just
                        subCompartment = case edSubcompartments edata of
                            (s : _) | not (T.null s) -> Just s
                            _ -> Nothing
                        compartment = case (mCompName, subCompartment) of
                            (Nothing, Nothing) -> Nothing
                            (Just c, sc) -> Just (Compartment c sc)
                            (Nothing, Just _) -> Nothing -- sub without medium is meaningless; drop
                            -- Biosphere direction: prefer inputGroup/outputGroup, else fall back to the
                            -- compartment heuristic (natural-resource flows are extractions).
                            -- An inventory indicator is the exception: it counts what the activity
                            -- sends away, and a source writes the same indicator under both groups
                            -- from one dataset to the next, so the group is not information. Reading
                            -- it would make the direction of a flow depend on which dataset one
                            -- happens to look at, and cost the writers that reconstruct direction
                            -- from the compartment their round-trip.
                        direction
                            | isInventoryIndicator = Emission
                            | not (T.null finalInputGroup) = Resource
                            | not (T.null finalOutputGroup) = Emission
                            | mCompName == Just NaturalResource = Resource
                            | otherwise = Emission
                        isInventoryIndicator = mCompName == Just InventoryIndicator
                        (flowUUID, flowWarn) = parseUUID (edFlowId edata)
                        (unitUUID, unitWarn) = parseUUID (edUnitId edata)
                        mediumWarn = case mediumReading of
                            Just (Left got) -> Just (unknownMedium got)
                            Just (Right _) -> Nothing
                            Nothing -> Nothing
                        warns =
                            catMaybes [flowWarn, unitWarn, mediumWarn]
                                ++ missingUnitWarning "elementary" (edFlowId edata) (edUnitName edata)
                        resolvedFlowName = nonBlankOr (edFlowId edata) (edFlowName edata)
                        unit = mkUnit unitUUID (edUnitName edata)
                        bioExchange =
                            BiosphereExchange
                                { bioFlowId = flowUUID
                                , bioAmount = edAmount edata
                                , bioUnitId = unitUUID
                                , bioDirection = direction
                                , bioLocation = "" -- EcoSpold2: no per-exchange location
                                , bioComment = snd <$> edComment edata
                                , bioPedigree = Nothing
                                }
                        bioFlow = BiosphereFlow flowUUID resolvedFlowName unitUUID (edSynonyms edata) (edCAS edata) Nothing compartment
                        formula = ExchangeFormula (nonEmptyText (edVariableName edata)) (nonEmptyText (edMathRel edata))
                        base = finishExchange unit warns state
                     in addExchange bioExchange formula (addBioFlow bioFlow base)
        | isElement tagName "text" =
            if inGeneralComment state
                then
                    let txt = accumText state
                        withDesc = if T.null txt then state else state{psDescription = txt : psDescription state}
                     in (popText withDesc){psContext = Other}
                else popText (captureDocText 2 (ddElementLang (psDocs state)) state)
        | isElement tagName "includedActivitiesStart" = closeDocText (\d t -> d{ddIncludedStart = t}) state
        | isElement tagName "includedActivitiesEnd" = closeDocText (\d t -> d{ddIncludedEnd = t}) state
        | isElement tagName "systemModelName" = closeDocText (\d t -> d{ddSystemModel = t}) state
        | isElement tagName "samplingProcedure" = closeDocText (\d t -> d{ddSampling = t}) state
        | isElement tagName "extrapolations" = closeDocText (\d t -> d{ddExtrapolations = t}) state
        | isElement tagName "review" = closeReview state
        | isElement tagName "name" =
            let txt = accumText state
             in if pathAt 1 "property" state
                    then onPendingProperty (\pending -> pending{ppyName = txt}) state
                    else onExchange (\d -> d{idFlowName = txt}) (\d -> d{edFlowName = txt}) state
        | isElement tagName "unitName" =
            let txt = accumText state
             in if pathAt 1 "property" state
                    then onPendingProperty (\pending -> pending{ppyUnit = txt}) state
                    else onExchange (\d -> d{idUnitName = txt}) (\d -> d{edUnitName = txt}) state
        | isElement tagName "property" =
            let filed = case psContext state of
                    InIntermediateExchange idata ->
                        state{psContext = InIntermediateExchange idata{idProperties = recordProperty (psPendingProperty state) (idProperties idata)}}
                    InElementaryExchange _ -> state
                    InActivityName -> state
                    InGeographyShortname -> state
                    InGeneralCommentText _ -> state
                    Other -> state
             in (popText filed){psPendingProperty = emptyPendingProperty}
        | isElement tagName "synonym" =
            let txt = T.strip (accumText state)
                ins m = if T.null txt then m else M.insertWith S.union "en" (S.singleton txt) m
             in onExchange (\d -> d{idSynonyms = ins (idSynonyms d)}) (\d -> d{edSynonyms = ins (edSynonyms d)}) state
        -- inputGroup / outputGroup: stash the pending value, keep the parent exchange context.
        | isElement tagName "inputGroup" =
            (popText state){psPendingInputGroup = T.strip (accumText state)}
        | isElement tagName "outputGroup" =
            (popText state){psPendingOutputGroup = T.strip (accumText state)}
        | isElement tagName "compartment" =
            let txt = T.strip (accumText state)
                add d = if T.null txt then d else d{edCompartments = txt : edCompartments d}
             in onExchange id add state
        | isElement tagName "subcompartment" =
            let txt = T.strip (accumText state)
                add d = if T.null txt then d else d{edSubcompartments = txt : edSubcompartments d}
             in onExchange id add state
        -- A <parameter> is referencable from formulas only through its
        -- variableName; its amount is pre-evaluated in the source. An entry
        -- with no variableName has nothing to contribute and is skipped; one
        -- with a variableName but no usable amount is dropped with a warning,
        -- since formulas referencing it will fail to evaluate downstream.
        | isElement tagName "parameter" =
            let PendingParam var amt rel = psPendingParam state
                committed = case (nonEmptyText var, amt) of
                    (Just v, Just a)
                        | S.member v (psAmbiguousParams state) -> state
                        | Just previous <- M.lookup v (psParams state)
                        , previous /= a ->
                            state
                                { psAmbiguousParams = S.insert v (psAmbiguousParams state)
                                , psWarnings =
                                    ("[WARNING] Ignoring <parameter> \"" ++ T.unpack v ++ "\" - declared twice with two amounts")
                                        : psWarnings state
                                }
                        | otherwise ->
                            state
                                { psParams = M.insert v a (psParams state)
                                , psParamExprs = maybe (psParamExprs state) (\r -> M.insert v r (psParamExprs state)) (nonEmptyText rel)
                                }
                    (Just v, Nothing) ->
                        state
                            { psWarnings =
                                ("[WARNING] Ignoring <parameter> \"" ++ T.unpack v ++ "\" - missing or unparseable amount")
                                    : psWarnings state
                            }
                    (Nothing, _) -> state
             in popText committed{psPendingParam = emptyPendingParam}
        | isElement tagName "classificationSystem" =
            (popText state){psPendingClassSystem = T.strip (accumText state)}
        | isElement tagName "classificationValue" =
            let txt = T.strip (accumText state)
                sys = psPendingClassSystem state
             in popText $
                    if T.null sys || T.null txt
                        then state
                        else case currentIntermediate state of
                            -- Exchange-scoped classification (e.g. By-product → Waste).
                            Just d -> state{psContext = InIntermediateExchange d{idClassifications = M.insert sys txt (idClassifications d)}}
                            -- Otherwise an activity-level classification.
                            Nothing -> state{psClassifications = M.insert sys txt (psClassifications state)}
        | otherwise =
            popPath state

    -- CDATA handler - treat as text
    cdata = text

    -- Build final result from parse state
    buildResult :: ParseState -> Either String ParsedDataset
    buildResult st =
        let name = fromMaybe "Unknown Activity" (psActivityName st)
            location = fromMaybe "GLO" (psLocation st)
            -- "GLO" above is this loader's stand-in, not something the dataset
            -- said: a dataset without a geography declares none.
            locationSource = maybe LocationUnspecified declaredLocationSource (psLocation st)
            description = reverse (psDescription st) -- Reverse to get correct order
            refUnit = fromMaybe "UNKNOWN_UNIT" (psRefUnit st)
            nativeType = ecoSpoldNativeType (psActivityType st) (psSpecialActivityType st)
            pairs = reverse (psExchanges st)
            -- A variable the file declared twice with two amounts names no
            -- value, so it is not one of the dataset's parameters.
            params = M.withoutKeys (psParams st) (psAmbiguousParams st)
            paramExprs = M.withoutKeys (psParamExprs st) (psAmbiguousParams st)
            formulaCheck = checkFormulas params pairs
            -- Apply cutoff strategy to exchanges
            activity =
                Activity
                    { activityName = name
                    , activityDescription = description
                    , activityDocumentation = documentationSections (psDocs st)
                    , activitySynonyms = M.empty
                    , activityClassification = psClassifications st
                    , activityLocation = location
                    , activityLocationSource = locationSource
                    , activityUnit = refUnit
                    , exchanges = map fst pairs
                    , activityParams = params
                    , activityParamExprs = paramExprs
                    , activityNativeType = nativeType
                    , activityNativeId = Nothing
                    , activityFormulaCheck = formulaCheck
                    }
         in -- A file that yields no exchange at all is not a dataset: a stray or
            -- truncated XML the SAX fold walked through without complaint.
            if null (exchanges activity)
                then Left "not an EcoSpold2 dataset: no exchange found"
                else
                    Right
                        ParsedDataset
                            { pdActivity = activity
                            , pdTechFlows = reverse (psTechFlows st)
                            , pdBioFlows = reverse (psBioFlows st)
                            , pdWasteFlows = reverse (psWasteFlows st)
                            , pdUnits = reverse (psUnits st)
                            , -- This format addresses a supplier by UUID, so it
                              -- numbers no dataset and links none by number.
                              pdDatasetNumber = 0
                            , pdWarnings = map T.pack (reverse (psWarnings st)) ++ placeholdersUsed st
                            }

-- | Parse EcoSpold file using Xeno SAX parser
streamParseActivityAndFlowsFromFile :: FilePath -> IO (Either String ParsedDataset)
streamParseActivityAndFlowsFromFile path = do
    !xmlContent <- BS.readFile path
    -- The failure is named after its file like the warnings below: read across
    -- an archive of several thousand, a reason that names no file is a reason
    -- nobody can act on.
    let parsed = first ((path ++ ": ") ++) (parseWithXeno xmlContent)
    either (const (pure ())) (reportReading path) parsed
    return parsed

-- | Send what a reading had to say to the progress log, the one place it can go.
reportReading :: FilePath -> ParsedDataset -> IO ()
reportReading path = mapM_ (reportProgress Warning . ((path ++ ": ") ++) . T.unpack) . pdWarnings

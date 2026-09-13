{-# LANGUAGE OverloadedStrings #-}

{- | Canonical, deterministic EcoSpold2 serializer – the inverse of
'EcoSpold.Parser2'.

Given a 'SimpleDatabase' (the natural writer input: activities keyed by
@(activityUUID, productUUID)@ plus flow/unit registries), this produces one
EcoSpold2 @.spold@ document per activity, named @{actUUID}_{prodUUID}.spold@
exactly as the loader expects.

Design goals (all pure, effect-free):

  * __Canonical__: fixed attribute order, fixed namespaces, fixed two-space
    indentation, no insignificant whitespace beyond the layout.
  * __Deterministic__: every @Map@/@Set@-derived list is emitted in sorted
    key order; doubles use one canonical renderer; output bytes are a pure
    function of the input plus the explicit 'VolatileMeta'.
  * __Round-trippable__: re-parsing the output reconstructs a structurally
    equal 'Activity'/flow set, and volatile metadata (timestamps, generator)
    is funnelled through 'VolatileMeta' so it can be pinned or omitted to make
    byte-level idempotence testable.

The only thing the writer cannot recover losslessly is information the parser
discards (per-exchange @id@/@unitId@ attribute *strings*, production volumes,
properties). Those are either omitted or synthesised from the stable UUIDs, so
a parse→write→parse cycle is a fixed point.

The contract is fixed-point over the parser's /image/, not @parse(write(x)) == x@
for an arbitrary 'SimpleDatabase'. The parser normalises as it reads – e.g. it
trims surrounding whitespace from text nodes – so a value that never came from a
parse (a name padded with leading spaces) is outside the round-trip's domain;
@parse(write(parse(s))) == parse(s)@ is what holds.
-}
module EcoSpold.Writer2 (
    VolatileMeta (..),
    noVolatileMeta,
    writeEcoSpold2,
    checkEcoSpold2Exportable,
    activityFileName,
    renderActivity,
    sortExchanges,
) where

import Amount (readAmount)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import EcoSpold.Common (showFFloatTrim)
import Types

{- | Volatile, non-semantic metadata that the parser ignores but a writer would
otherwise stamp with the current time / tool version. Threaded explicitly so a
caller can pin it (reproducible export) or omit it entirely (byte-stable
round-trip). 'Nothing' fields emit no corresponding attribute/element.
-}
data VolatileMeta = VolatileMeta
    { vmCreationTimestamp :: !(Maybe Text)
    -- ^ @administrativeInformation/fileAttributes/@creationTimestamp@
    , vmGenerator :: !(Maybe Text)
    -- ^ free-text generator note emitted as an XML comment, or omitted
    }
    deriving (Eq, Show)

-- | The reproducible default: omit every volatile field.
noVolatileMeta :: VolatileMeta
noVolatileMeta = VolatileMeta Nothing Nothing

-- ============================================================================
-- Public entry points
-- ============================================================================

{- | Serialize a whole 'SimpleDatabase' to a sorted list of
@(filename, document)@ pairs, one per activity. Sorted by filename so the
sequence itself is deterministic. Flow names and unit names are resolved from
the registries because the parser stores them on exchanges, not centrally.
Runs 'checkEcoSpold2Exportable' first and returns its 'Left' on a database the
format cannot represent faithfully, so an unguarded caller cannot silently emit
a corrupt archive.
-}
writeEcoSpold2 :: VolatileMeta -> SimpleDatabase -> Either Text [(FilePath, Text)]
writeEcoSpold2 meta sdb = do
    checkEcoSpold2Exportable sdb
    pure
        [ (activityFileName actUUID prodUUID, renderActivity meta env act)
        | ((actUUID, prodUUID), act) <- M.toAscList (sdbActivities sdb)
        ]
  where
    env =
        ResolveEnv
            { reTechName = \u -> tfName <$> M.lookup u techFlows
            , reBioFlow = (`M.lookup` bioFlows)
            , reWasteName = \u -> wfName <$> M.lookup u wasteFlows
            , reTechSyns = \u -> maybe M.empty tfSynonyms (M.lookup u techFlows)
            , reBioSyns = \u -> maybe M.empty bfSynonyms (M.lookup u bioFlows)
            , reWasteSyns = \u -> maybe M.empty wfSynonyms (M.lookup u wasteFlows)
            , reUnitName = \u -> unitName <$> M.lookup u units
            }
    techFlows = sdbTechFlows sdb
    bioFlows = sdbBioFlows sdb
    wasteFlows = sdbWasteFlows sdb
    units = sdbUnits sdb

{- | Guard an EcoSpold2 export against data the writer cannot faithfully
re-encode.

  * __Non-finite amounts.__ 'doubleAttr' renders @NaN@/@Infinity@ as their
    non-parseable literal forms; the guard rejects such an amount rather than
    emit one. 'Amount.readAmount' also refuses non-finite and overflowing
    literals (e.g. @1e400@) on import, so an in-memory non-finite can only arise
    from programmatic construction.

  * __Reference inputs.__ A treatment activity's 'ReferenceInput' is written as
    @inputGroup 5@, but no ES2 inputGroup re-parses to 'ReferenceInput' (the
    parser yields 'Input'), so the reference designation is silently lost on
    re-parse – the activity loses its reference product. ES2 has no encoding for
    it, so reject rather than corrupt.

  * __Unresolvable units.__ The @\<unitName\>@ line is resolved from 'sdbUnits';
    an exchange whose @unitId@ is absent from the registry would emit no
    @\<unitName\>@, which the parser re-reads as the @UNKNOWN_UNIT@ placeholder –
    a silent unit downgrade. Reject rather than lose the unit.

  * __Unresolvable flows.__ A flow's name, compartment, CAS and synonyms are all
    resolved from the flow registries; an exchange whose flow id is absent emits
    a bare exchange that re-parses with the flow's UUID string as its name and
    /no compartment/ – a silent identity and compartment downgrade. Symmetric to
    the unit case: reject rather than corrupt.

  * __Non-round-tripping amounts.__ A defensive check that the written decimal
    re-parses to the same 'Double' through 'Amount.readAmount' (the importer's
    correctly-rounded reader). Every finite amount round-trips, so this guards
    the formatter↔reader contract against future drift.

Reports the first offender and fails loudly rather than emit silently wrong
data. Databases free of it pass unchanged.
-}
checkEcoSpold2Exportable :: SimpleDatabase -> Either Text ()
checkEcoSpold2Exportable db =
    case mapMaybe listToMaybe orderedViolations of
        [] -> Right ()
        violations -> Left (T.intercalate "\n\n" violations)
  where
    -- Categories in priority order; the first offending activity of each is
    -- reported. Each inner list carries one message per offender.
    orderedViolations =
        [ [nonFiniteMsg c a | (c, a) <- amountOffenders]
        , [refInputMsg c | c <- refInputOffenders]
        , [unknownUnitMsg c | c <- unknownUnitOffenders]
        , [missingFlowMsg c | c <- missingFlowOffenders]
        , [nonRoundTripMsg c a | (c, a) <- roundTripOffenders]
        ]
    amountOffenders =
        [ (activityName act, amt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , let amt = exchangeAmount ex
        , isNaN amt || isInfinite amt
        ]
    roundTripOffenders =
        [ (activityName act, amt)
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , let amt = exchangeAmount ex
        , not (isNaN amt || isInfinite amt) -- non-finite already reported above
        , not (amountRoundTrips amt)
        ]
    -- The written decimal must re-parse to the same Double through the importer's
    -- correctly-rounded reader ('Amount.readAmount'), or the value would change
    -- on re-import.
    amountRoundTrips amt = readAmount (doubleAttr amt) == Just amt
    refInputOffenders =
        [ activityName act
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isReferenceInput ex
        ]
    unknownUnitOffenders =
        [ activityName act
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , M.notMember (exchangeUnitId ex) (sdbUnits db)
        ]
    -- Each exchange's flow must resolve in the registry matching its kind, or the
    -- writer emits a nameless, compartment-less exchange (see Haddock above).
    missingFlowOffenders =
        [ activityName act
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , flowMissing ex
        ]
    flowMissing ex = case ex of
        TechnosphereExchange{techFlowId = fid} -> M.notMember fid (sdbTechFlows db)
        WasteExchange{waFlowId = fid} -> M.notMember fid (sdbWasteFlows db)
        BiosphereExchange{bioFlowId = fid} -> M.notMember fid (sdbBioFlows db)
    isReferenceInput ex = case ex of
        TechnosphereExchange{techRole = ReferenceInput} -> True
        TechnosphereExchange{} -> False
        WasteExchange{} -> False
        BiosphereExchange{} -> False
    cannotRepresent c = "EcoSpold2 export cannot represent activity \"" <> c <> "\": "
    nonFiniteMsg c a = cannotRepresent c <> "exchange amount " <> T.pack (show a) <> " is not finite."
    refInputMsg c = cannotRepresent c <> "a reference input (treatment process) has no EcoSpold2 encoding."
    unknownUnitMsg c = cannotRepresent c <> "an exchange references a unit absent from the registry."
    missingFlowMsg c = cannotRepresent c <> "an exchange references a flow absent from the registry."
    nonRoundTripMsg c a =
        cannotRepresent c
            <> "exchange amount "
            <> T.pack (show a)
            <> " has no decimal form that re-parses to the same value."

-- | Canonical filename for an activity: @{actUUID}_{prodUUID}.spold@.
activityFileName :: UUID.UUID -> UUID.UUID -> FilePath
activityFileName actUUID prodUUID =
    T.unpack (processRefText (ProcessRef actUUID prodUUID) <> ".spold")

{- | Resolver for the registry-held data the parser writes onto exchanges.
Passed in so 'renderActivity' stays a pure function of its inputs.
-}
data ResolveEnv = ResolveEnv
    { reTechName :: UUID.UUID -> Maybe Text
    , reBioFlow :: UUID.UUID -> Maybe BiosphereFlow
    , reWasteName :: UUID.UUID -> Maybe Text
    , reTechSyns :: UUID.UUID -> M.Map Text (S.Set Text)
    , reBioSyns :: UUID.UUID -> M.Map Text (S.Set Text)
    , reWasteSyns :: UUID.UUID -> M.Map Text (S.Set Text)
    , reUnitName :: UUID.UUID -> Maybe Text
    }

-- ============================================================================
-- Document rendering
-- ============================================================================

-- | Render one activity to a complete EcoSpold2 document.
renderActivity :: VolatileMeta -> ResolveEnv -> Activity -> Text
renderActivity meta env act =
    T.unlines $
        [ "<?xml version='1.0' encoding='UTF-8'?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">"
        ]
            ++ generatorComment meta
            ++ [ "  <activityDataset>"
               , "    <activityDescription>"
               ]
            ++ renderActivityElem act
            ++ renderGeneralComment (activityDescription act)
            ++ renderClassifications (activityClassification act)
            ++ renderGeography (activityLocation act)
            ++ ["    </activityDescription>", "    <flowData>"]
            ++ concatMap (renderExchange env) (sortExchanges (exchanges act))
            ++ ["    </flowData>"]
            ++ renderAdminInfo meta
            ++ ["  </activityDataset>", "</ecoSpold>"]

-- | Optional generator note as an XML comment (volatile, omitted by default).
generatorComment :: VolatileMeta -> [Text]
generatorComment meta = case vmGenerator meta of
    Nothing -> []
    Just g -> ["  <!-- generator: " <> escapeText g <> " -->"]

{- | The @\<activity\>@ opening element. We do not have the source's raw
@id@/@activityNameId@ strings on 'Activity' (the parser keeps UUIDs in the
filename, not here), so we omit them: the parser derives identity from the
filename, never from these attributes. activityType / specialActivityType are
re-emitted from 'activityNativeType' when present.
-}
renderActivityElem :: Activity -> [Text]
renderActivityElem act =
    [ "      <activity" <> activityTypeAttrs (activityNativeType act) <> ">"
    , "        <activityName xml:lang=\"en\">" <> escapeText (activityName act) <> "</activityName>"
    , "      </activity>"
    ]

{- | Re-emit the ecospold2 @activityType@/@specialActivityType@ attributes from
the captured native type. Only 'EcoSpoldActivityType' carries them; the SimaPro
and ILCD variants have no ecospold2 representation, so we emit nothing (a
faithful round-trip of an ES2-sourced database never hits those cases).
-}
activityTypeAttrs :: Maybe NativeActivityType -> Text
activityTypeAttrs Nothing = ""
activityTypeAttrs (Just nt) = case nt of
    EcoSpoldActivityType code _ special _ ->
        " activityType=\"" <> intText code <> "\"" <> specialAttr special
    SimaProProcessType _ -> ""
    ILCDProcessType _ -> ""
  where
    specialAttr Nothing = ""
    specialAttr (Just s) = " specialActivityType=\"" <> intText s <> "\""

{- | @\<generalComment\>@ with one @\<text index="n">@ per description
paragraph, in the original order. Empty description emits nothing.
-}
renderGeneralComment :: [Text] -> [Text]
renderGeneralComment [] = []
renderGeneralComment paras =
    ["      <generalComment>"]
        ++ [ "        <text xml:lang=\"en\" index=\"" <> intText i <> "\">" <> escapeText p <> "</text>"
           | (i, p) <- zip [0 :: Int ..] paras
           ]
        ++ ["      </generalComment>"]

{- | Activity-level @\<classification\>@ blocks (ISIC/CPC, …) under
@activityDescription@. One block per (system, value) pair, sorted by system for
determinism. Parser2 routes any @classification@ outside an intermediate
exchange to @activityClassification@, so these round-trip. Empty map emits
nothing.
-}
renderClassifications :: M.Map Text Text -> [Text]
renderClassifications =
    concatMap (classificationBlock . Just) . M.toAscList

-- | @\<geography\>\<shortname\>@ holding the location code.
renderGeography :: Text -> [Text]
renderGeography loc =
    [ "      <geography>"
    , "        <shortname xml:lang=\"en\">" <> escapeText loc <> "</shortname>"
    , "      </geography>"
    ]

{- | @administrativeInformation@ carrying only the (optional, volatile)
creation timestamp. Omitted entirely when no timestamp is pinned, keeping the
default output minimal and byte-stable.
-}
renderAdminInfo :: VolatileMeta -> [Text]
renderAdminInfo meta = case vmCreationTimestamp meta of
    Nothing -> []
    Just ts ->
        [ "    <administrativeInformation>"
        , "      <fileAttributes defaultLanguage=\"en\" creationTimestamp=\"" <> escapeAttr ts <> "\"/>"
        , "    </administrativeInformation>"
        ]

-- ============================================================================
-- Exchange rendering
-- ============================================================================

{- | Deterministic exchange order: group by kind (technosphere, then waste,
then biosphere), and within a kind sort by flow UUID. The reference product is
forced first within the technosphere group so the canonical layout matches the
ecospold convention of leading with the produced product.
-}
sortExchanges :: [Exchange] -> [Exchange]
sortExchanges = sortOn exchangeSortKey

{- | Sort key: (kindRank, not-reference, flowUUID). @not-reference@ sorts the
reference product/input ahead of the others within a kind. The flow UUID is the
tiebreaker; 'sortOn' is stable, so two exchanges sharing a key keep their input
order and are both emitted – never collapsed (a 'Map' keyed on this would have
silently dropped a duplicate biosphere/technosphere line, undercounting the
inventory).
-}
exchangeSortKey :: Exchange -> (Int, Bool, Text)
exchangeSortKey ex = (kindRank, not (exchangeIsReference ex), UUID.toText (exchangeFlowId ex))
  where
    kindRank = case ex of
        TechnosphereExchange{} -> 0
        WasteExchange{} -> 1
        BiosphereExchange{} -> 2

-- | Render a single exchange to its XML lines.
renderExchange :: ResolveEnv -> Exchange -> [Text]
renderExchange env ex = case ex of
    TechnosphereExchange{} -> renderTechnosphere env ex
    WasteExchange{} -> renderWaste env ex
    BiosphereExchange{} -> renderBiosphere env ex

{- | Technosphere exchange → @intermediateExchange@.

Inverse of the parser's role logic:

  * 'ReferenceProduct' → output, @\<outputGroup\>0\</outputGroup\>@
  * 'Coproduct'        → output, @\<outputGroup\>2\</outputGroup\>@ (any
    non-zero output group re-parses to 'Coproduct')
  * 'AvoidedProduct'   → input, @\<inputGroup\>5\</inputGroup\>@, amount
    negated: the negative linked input ecoinvent itself writes for a
    substitution. It re-parses to a negative 'Input', the same matrix entry.
  * 'Input' / 'ReferenceInput' → input, @\<inputGroup\>5\</inputGroup\>@
-}
renderTechnosphere :: ResolveEnv -> Exchange -> [Text]
renderTechnosphere env ex =
    intermediateExchange
        (reTechName env flowId)
        flowId
        (techUnitId ex)
        signedAmount
        (reUnitName env (techUnitId ex))
        (reTechSyns env flowId)
        groupLine
        (techActivityLinkId ex)
        Nothing
        (techComment ex)
  where
    flowId = techFlowId ex
    groupLine = case techRole ex of
        ReferenceProduct -> outputGroupLine "0"
        Coproduct -> outputGroupLine "2"
        AvoidedProduct -> inputGroupLine "5"
        Input -> inputGroupLine "5"
        ReferenceInput -> inputGroupLine "5"
    signedAmount = case techRole ex of
        AvoidedProduct -> negate (techAmount ex)
        ReferenceProduct -> techAmount ex
        Coproduct -> techAmount ex
        Input -> techAmount ex
        ReferenceInput -> techAmount ex

{- | Waste exchange → @intermediateExchange@ tagged with the
@By-product classification = Waste@ classification. The
@waIsInput@ flag picks the group so direction round-trips: an input takes
@inputGroup 5@ (from technosphere), an output the @outputGroup 2@ (by-product) –
the only output groups EcoSpold2 defines for an intermediate exchange besides the
reference @0@. A waste output shares group @2@ with an ordinary coproduct; it is
the @Waste@ classification, not the group number, that re-routes it to a
'WasteExchange' on parse, so the kind is preserved.
-}
renderWaste :: ResolveEnv -> Exchange -> [Text]
renderWaste env ex =
    intermediateExchange
        (reWasteName env flowId)
        flowId
        (waUnitId ex)
        (waAmount ex)
        (reUnitName env (waUnitId ex))
        (reWasteSyns env flowId)
        groupLine
        (waActivityLinkId ex)
        (Just ("By-product classification", "Waste"))
        (waComment ex)
  where
    flowId = waFlowId ex
    groupLine = if waIsInput ex then inputGroupLine "5" else outputGroupLine "2"

{- | Biosphere exchange → @elementaryExchange@. Direction maps to in/out group:
'Resource' → @inputGroup@ 4, 'Emission' → @outputGroup@ 4. The compartment is
taken from the registry's 'BiosphereFlow'.
-}
renderBiosphere :: ResolveEnv -> Exchange -> [Text]
renderBiosphere env ex =
    [openTag]
        ++ nameLine 8 (reBioFlow env flowId >>= justName)
        ++ unitNameLine 8 (reUnitName env (bioUnitId ex))
        ++ compartmentBlock (reBioFlow env flowId >>= bfCompartment)
        ++ [groupLine]
        ++ synonymLines 8 (reBioSyns env flowId)
        ++ commentLines 8 (bioComment ex)
        ++ ["      </elementaryExchange>"]
  where
    flowId = bioFlowId ex
    justName f = let n = bfName f in if T.null n then Nothing else Just n
    casAttr = case reBioFlow env flowId >>= bfCAS of
        Just cas -> " casNumber=\"" <> escapeAttr cas <> "\""
        Nothing -> ""
    openTag =
        "      <elementaryExchange elementaryExchangeId=\""
            <> escapeAttr (UUID.toText flowId)
            <> "\" unitId=\""
            <> escapeAttr (UUID.toText (bioUnitId ex))
            <> "\" amount=\""
            <> doubleAttr (bioAmount ex)
            <> "\""
            <> casAttr
            <> ">"
    groupLine = case bioDirection ex of
        Resource -> "        <inputGroup>4</inputGroup>"
        Emission -> "        <outputGroup>4</outputGroup>"

{- | Shared @intermediateExchange@ emitter for technosphere and waste flows.
The @activityLinkId@ is emitted only when the exchange has one (matching the parser, which
treats a nil or empty attribute as "no link"). An optional @classification@ tags waste flows.
-}
intermediateExchange ::
    Maybe Text -> -- resolved flow name
    UUID.UUID -> -- flow id
    UUID.UUID -> -- unit id
    Double -> -- amount
    Maybe Text -> -- resolved unit name
    M.Map Text (S.Set Text) -> -- synonyms
    Text -> -- the in/out group line (8-space indent)
    Maybe UUID.UUID -> -- activity link id (absent = omit)
    Maybe (Text, Text) -> -- optional (classificationSystem, classificationValue)
    Maybe Text -> -- comment
    [Text]
intermediateExchange mName flowId unitUUID amount mUnit syns groupLine linkId mClass mComment =
    [openTag]
        ++ nameLine 8 mName
        ++ unitNameLine 8 mUnit
        ++ [groupLine]
        ++ classificationBlock mClass
        ++ synonymLines 8 syns
        ++ commentLines 8 mComment
        ++ ["      </intermediateExchange>"]
  where
    openTag =
        "      <intermediateExchange intermediateExchangeId=\""
            <> escapeAttr (UUID.toText flowId)
            <> "\" unitId=\""
            <> escapeAttr (UUID.toText unitUUID)
            <> "\" amount=\""
            <> doubleAttr amount
            <> "\""
            <> linkAttr
            <> ">"
    linkAttr
        | Just linked <- linkId = " activityLinkId=\"" <> escapeAttr (UUID.toText linked) <> "\""
        | otherwise = ""

-- ============================================================================
-- Small element emitters
-- ============================================================================

inputGroupLine :: Text -> Text
inputGroupLine g = "        <inputGroup>" <> g <> "</inputGroup>"

outputGroupLine :: Text -> Text
outputGroupLine g = "        <outputGroup>" <> g <> "</outputGroup>"

-- | @\<name\>@ line at the given indent, omitted when no name resolved.
nameLine :: Int -> Maybe Text -> [Text]
nameLine ind = maybe [] (\n -> [indent ind <> "<name xml:lang=\"en\">" <> escapeText n <> "</name>"])

-- | @\<unitName\>@ line, omitted when no unit resolved.
unitNameLine :: Int -> Maybe Text -> [Text]
unitNameLine ind = maybe [] (\u -> [indent ind <> "<unitName xml:lang=\"en\">" <> escapeText u <> "</unitName>"])

{- | @\<comment\>@ line for an exchange. The parser keeps only the English
comment text, so we emit it verbatim under @xml:lang="en"@.
-}
commentLines :: Int -> Maybe Text -> [Text]
commentLines ind = maybe [] (\c -> [indent ind <> "<comment xml:lang=\"en\">" <> escapeText c <> "</comment>"])

{- | @\<synonym\>@ lines, one per synonym, sorted. The parser collapses all
languages into the @"en"@ bucket and ignores the language, so we flatten and
sort the unique synonym strings for a deterministic, round-trip-stable layout.
-}
synonymLines :: Int -> M.Map Text (S.Set Text) -> [Text]
synonymLines ind syns =
    [ indent ind <> "<synonym xml:lang=\"en\">" <> escapeText s <> "</synonym>"
    | s <- S.toAscList (S.unions (M.elems syns))
    , not (T.null s)
    ]

{- | The nested @\<compartment\>\<compartment\>\<subcompartment\>@ block the
parser expects. Omitted when the flow carries no compartment.
-}
compartmentBlock :: Maybe Compartment -> [Text]
compartmentBlock Nothing = []
compartmentBlock (Just (Compartment name mSub)) =
    ["        <compartment>", "          <compartment xml:lang=\"en\">" <> escapeText (mediumText name) <> "</compartment>"]
        ++ subLine mSub
        ++ ["        </compartment>"]
  where
    subLine = maybe [] (\s -> ["          <subcompartment xml:lang=\"en\">" <> escapeText s <> "</subcompartment>"])

{- | @\<classification\>@ block tagging an exchange (used for waste). Emits the
classificationSystem / classificationValue pair the parser reads.
-}
classificationBlock :: Maybe (Text, Text) -> [Text]
classificationBlock Nothing = []
classificationBlock (Just (system, value)) =
    [ "        <classification>"
    , "          <classificationSystem xml:lang=\"en\">" <> escapeText system <> "</classificationSystem>"
    , "          <classificationValue xml:lang=\"en\">" <> escapeText value <> "</classificationValue>"
    , "        </classification>"
    ]

-- ============================================================================
-- Pure formatting helpers
-- ============================================================================

-- | @n@ spaces of indentation.
indent :: Int -> Text
indent n = T.replicate n " "

intText :: Int -> Text
intText = T.pack . show

{- | Canonical 'Double' rendering for amounts via the shared 'showFFloatTrim'
(fixed-point, never scientific), the exact inverse of 'Amount.readAmount': every
finite amount round-trips through that correctly-rounded reader.
'checkEcoSpold2Exportable' rejects any amount that does not re-parse, which now
leaves only the non-finite. A non-finite value renders as its (non-parseable)
@"NaN"@/@"Infinity"@ form so a
bad re-import fails loudly rather than silently reading @0@; the guard rejects
non-finite amounts before they reach here.
-}
doubleAttr :: Double -> Text
doubleAttr d
    | isNaN d || isInfinite d = T.pack (show d)
    | otherwise = T.pack (showFFloatTrim d)

-- | Escape the three XML predefined entities for element text content.
escapeText :: Text -> Text
escapeText =
    T.replace "&" "&amp;"
        >>> T.replace "<" "&lt;"
        >>> T.replace ">" "&gt;"
  where
    (>>>) f g = g . f

{- | Escape for a double-quoted attribute value: the text escapes plus the
double quote. The parser's 'decodeXmlEntities' reverses all of these.
-}
escapeAttr :: Text -> Text
escapeAttr =
    escapeText
        >>> T.replace "\"" "&quot;"
        >>> T.replace "\n" "&#10;"
        >>> T.replace "\r" "&#13;"
  where
    (>>>) f g = g . f

{-# LANGUAGE LambdaCase #-}
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
    equal 'Activity'/flow set, its dates included; the only volatile field,
    the generator, is funnelled through 'VolatileMeta' so it can be pinned or
    omitted to make byte-level idempotence testable.

The only thing the writer cannot recover losslessly is information the parser
discards (per-exchange @id@/@unitId@ attribute *strings*, production volumes,
properties). Those are either omitted or synthesised from the stable UUIDs, so
a parse→write→parse cycle is a fixed point.

A dataset carries its model: its parameters as @\<parameter\>@ elements, a
line's formula as its @mathematicalRelation@ and the name other formulas read
it by as its @variableName@, each in the format's language. EcoSpold 2 has no
database-wide parameter, so each dataset carries a copy of those of the
database it reads, at the value they settle to there. A formula the format
cannot write is written as its value, and the warnings name it.

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
    writeEcoSpold2WithWarnings,
    checkEcoSpold2Exportable,
    activityFileName,
    renderActivity,
    sortExchanges,
) where

import Amount (readAmount)
import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import Database.ExportFormulas (Exported, ProcessExport (..), Scaled (..), Written (..), databaseCopies, exportFormulas, frozenWarnings, processExport)
import EcoSpold.Common (showFFloatTrim)
import qualified Expr
import Types

{- | Volatile, non-semantic metadata that the parser ignores: the tool that
wrote the file. Threaded explicitly so a caller can pin it or omit it (byte-stable
round-trip). The dataset's dates are not volatile and come from the activity.
-}
newtype VolatileMeta = VolatileMeta
    { vmGenerator :: Maybe Text
    -- ^ free-text generator note emitted as an XML comment, or omitted
    }
    deriving (Eq, Show)

-- | The reproducible default: omit every volatile field.
noVolatileMeta :: VolatileMeta
noVolatileMeta = VolatileMeta Nothing

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
writeEcoSpold2 meta = fmap fst . writeEcoSpold2WithWarnings meta

-- | The documents, and what the export says of the formulas and parameters it could not write.
writeEcoSpold2WithWarnings :: VolatileMeta -> SimpleDatabase -> Either Text ([(FilePath, Text)], [Text])
writeEcoSpold2WithWarnings meta sdb = do
    checkEcoSpold2Exportable sdb
    pure
        ( [ (activityFileName actUUID prodUUID, renderActivity meta env (DatasetModel actUUID (processExport exported key)) act)
          | (key@(actUUID, prodUUID), act) <- M.toAscList (sdbActivities carrying)
          ]
        , frozenWarnings exported
            <> [T.pack (show (S.size unread)) <> " parameters of the database are read by no process, and not written" | not (S.null unread)]
        )
  where
    copies :: M.Map (UUID.UUID, UUID.UUID) (M.Map Text Parameter)
    copies = databaseCopies sdb

    -- Each dataset with the database's parameters it reads as its own.
    carrying :: SimpleDatabase
    carrying = sdb{sdbActivities = M.mapWithKey (\key act -> act{activityParameters = M.union (activityParameters act) (M.findWithDefault M.empty key copies)}) (sdbActivities sdb), sdbParameters = M.empty}

    exported :: Exported
    exported = exportFormulas (Expr.EcoSpold2 M.empty) (\_ ex -> Times (writtenSign ex)) carrying

    unread :: S.Set Text
    unread = S.difference (M.keysSet (sdbParameters sdb)) (S.unions (map M.keysSet (M.elems copies)))

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

-- | What a dataset writes of its model: its parameters and its lines' formulas and names, as exported.
data DatasetModel = DatasetModel
    { dmActivity :: !UUID.UUID
    , dmExport :: !ProcessExport
    }

-- | Render one activity to a complete EcoSpold2 document.
renderActivity :: VolatileMeta -> ResolveEnv -> DatasetModel -> Activity -> Text
renderActivity meta env model act =
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
            -- Numbered before the sort: the exported formulas know a line by its place in the activity.
            ++ concatMap (\(i, ex) -> renderExchange env (lineFormula (dmExport model) i ex) ex) (sortOn (exchangeSortKey . snd) (zip [0 ..] (exchanges act)))
            ++ concatMap (renderParameter (dmActivity model)) (M.toAscList (peParameters (dmExport model)))
            ++ ["    </flowData>"]
            ++ renderAdminInfo (activityDates act)
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

{- | @administrativeInformation@ carrying the dataset's creation and last edit,
the two dates this format has a place for. Omitted when the dataset states
neither. A date with no meaning stated ('datesStated') has no place here: it
would have to be passed off as one of the two.
-}
renderAdminInfo :: DatasetDates -> [Text]
renderAdminInfo dates = case catMaybes [stamp "creationTimestamp" (datesCreated dates), stamp "lastEditTimestamp" (datesLastRevised dates)] of
    [] -> []
    stamps ->
        [ "    <administrativeInformation>"
        , "      <fileAttributes defaultLanguage=\"en\"" <> T.concat stamps <> "/>"
        , "    </administrativeInformation>"
        ]
  where
    stamp :: Text -> Maybe Day -> Maybe Text
    stamp attr = fmap (\day -> " " <> attr <> "=\"" <> isoDateTime day <> "\"")

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

-- | The name other formulas read a line by, and the formula its amount follows, as written.
data LineFormula = LineFormula
    { lfName :: !(Maybe Text)
    , lfRelation :: !(Maybe Text)
    }

{- | A line's name and formula from the process's export. A named line whose
formula is a bare number is written with its name alone, as the reader reads
such a line back.
-}
lineFormula :: ProcessExport -> Int -> Exchange -> LineFormula
lineFormula pe i ex = LineFormula{lfName = name, lfRelation = if isJust name && stated then Nothing else IM.lookup i (peLines pe)}
  where
    name :: Maybe Text
    name = IM.lookup i (peLineNames pe)

    stated :: Bool
    stated = case Expr.readFormula Expr.Volca . efText <$> exchangeFormula ex of
        Just (Right (Expr.Literal _)) -> True
        Just (Right _) -> False
        Just (Left _) -> False
        Nothing -> False

-- | The @variableName@, @mathematicalRelation@ and @isCalculatedAmount@ attributes of a line.
formulaAttrs :: LineFormula -> Text
formulaAttrs lf =
    maybe "" (\n -> " variableName=\"" <> escapeAttr n <> "\"") (lfName lf)
        <> maybe "" (\r -> " mathematicalRelation=\"" <> escapeAttr r <> "\" isCalculatedAmount=\"true\"") (lfRelation lf)

-- | Render a single exchange to its XML lines.
renderExchange :: ResolveEnv -> LineFormula -> Exchange -> [Text]
renderExchange env lf ex = case ex of
    TechnosphereExchange{} -> renderTechnosphere env lf ex
    WasteExchange{} -> renderWaste env lf ex
    BiosphereExchange{} -> renderBiosphere env lf ex

{- | The sign a line's amount is written with, which its formula follows: an
avoided product is written as the negative input ecoinvent writes for a
substitution.
-}
writtenSign :: Exchange -> Double
writtenSign = \case
    TechnosphereExchange{techRole = role} -> case role of
        AvoidedProduct -> -1
        ReferenceProduct -> 1
        Coproduct -> 1
        Input -> 1
        ReferenceInput -> 1
    WasteExchange{} -> 1
    BiosphereExchange{} -> 1

{- | A parameter as a @\<parameter\>@ element, its id derived from the
activity and the name (the reader keeps none, the bytes stay stable). A
calculated parameter with no value is not written here: EcoSpold 2 states an
amount beside every formula, and 'exportFormulas' leaves such a one out for it.
-}
renderParameter :: UUID.UUID -> (Text, (Parameter, Written)) -> [Text]
renderParameter activity (name, (parameter, written)) = case amount of
    Nothing -> []
    Just value ->
        [ "      <parameter parameterId=\""
            <> UUID.toText (UUID5.generateNamed activity (BS.unpack (TE.encodeUtf8 name)))
            <> "\" variableName=\""
            <> escapeAttr name
            <> "\" amount=\""
            <> doubleAttr value
            <> "\""
            <> relation
            <> ">"
        ]
            ++ nameLine 8 (Just name)
            ++ unitNameLine 8 (parameterUnit parameter)
            ++ uncertaintyLines spread
            ++ commentLines 8 (if T.null (parameterDescription parameter) then Nothing else Just (parameterDescription parameter))
            ++ ["      </parameter>"]
  where
    amount :: Maybe Double
    amount = case written of
        WrittenValue v _ -> Just v
        WrittenFormula _ -> case parameterDefinition parameter of
            CalculatedParameter _ value -> value
            InputParameter v _ -> Just v

    relation :: Text
    relation = case written of
        WrittenFormula f -> " mathematicalRelation=\"" <> escapeAttr f <> "\" isCalculatedAmount=\"true\""
        WrittenValue{} -> ""

    spread :: Maybe Uncertainty
    spread = case written of
        WrittenValue _ u -> u
        WrittenFormula _ -> Nothing

{- | A spread as the reader reads it back: a lognormal's deviation is the
exponential of the root of its variance, a normal's deviation the root of its
own. A number with no finite value (the @mu@ of a zero mean) is left out.
-}
uncertaintyLines :: Maybe Uncertainty -> [Text]
uncertaintyLines = \case
    Nothing -> []
    Just u ->
        [ "        <uncertainty>"
        , "          <" <> element <> T.concat [" " <> k <> "=\"" <> doubleAttr v <> "\"" | (k, v) <- numbers, not (isNaN v || isInfinite v)] <> "/>"
        , "        </uncertainty>"
        ]
      where
        (element, numbers) = case u of
            Lognormal mean deviation ->
                let variance = log deviation ^ (2 :: Int)
                 in ("lognormal", [("meanValue", mean), ("mu", log (abs mean)), ("variance", variance), ("varianceWithPedigreeUncertainty", variance)])
            Normal mean deviation ->
                ("normal", [("meanValue", mean), ("variance", deviation * deviation), ("varianceWithPedigreeUncertainty", deviation * deviation)])
            Triangular low likely high -> ("triangular", [("minValue", low), ("mostLikelyValue", likely), ("maxValue", high)])
            Uniform low high -> ("uniform", [("minValue", low), ("maxValue", high)])

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
renderTechnosphere :: ResolveEnv -> LineFormula -> Exchange -> [Text]
renderTechnosphere env lf ex =
    intermediateExchange
        IntermediateLine
            { ilName = reTechName env flowId
            , ilFlow = flowId
            , ilUnit = techUnitId ex
            , ilAmount = writtenSign ex * techAmount ex
            , ilUnitName = reUnitName env (techUnitId ex)
            , ilSynonyms = reTechSyns env flowId
            , ilGroup = groupLine
            , ilLink = statedLink ex
            , ilClassification = Nothing
            , ilComment = techComment ex
            , ilFormula = lf
            }
  where
    flowId = techFlowId ex
    groupLine = case techRole ex of
        ReferenceProduct -> outputGroupLine "0"
        Coproduct -> outputGroupLine "2"
        AvoidedProduct -> inputGroupLine "5"
        Input -> inputGroupLine "5"
        ReferenceInput -> inputGroupLine "5"

{- | Waste exchange → @intermediateExchange@ tagged with the
@By-product classification = Waste@ classification. The
@waIsInput@ flag picks the group so direction round-trips: an input takes
@inputGroup 5@ (from technosphere), an output the @outputGroup 2@ (by-product) –
the only output groups EcoSpold2 defines for an intermediate exchange besides the
reference @0@. A waste output shares group @2@ with an ordinary coproduct; it is
the @Waste@ classification, not the group number, that re-routes it to a
'WasteExchange' on parse, so the kind is preserved.
-}
renderWaste :: ResolveEnv -> LineFormula -> Exchange -> [Text]
renderWaste env lf ex =
    intermediateExchange
        IntermediateLine
            { ilName = reWasteName env flowId
            , ilFlow = flowId
            , ilUnit = waUnitId ex
            , ilAmount = waAmount ex
            , ilUnitName = reUnitName env (waUnitId ex)
            , ilSynonyms = reWasteSyns env flowId
            , ilGroup = groupLine
            , ilLink = statedLink ex
            , ilClassification = Just ("By-product classification", "Waste")
            , ilComment = waComment ex
            , ilFormula = lf
            }
  where
    flowId = waFlowId ex
    groupLine = if waIsInput ex then inputGroupLine "5" else outputGroupLine "2"

{- | Biosphere exchange → @elementaryExchange@. Direction maps to in/out group:
'Resource' → @inputGroup@ 4, 'Emission' → @outputGroup@ 4. The compartment is
taken from the registry's 'BiosphereFlow'.
-}
renderBiosphere :: ResolveEnv -> LineFormula -> Exchange -> [Text]
renderBiosphere env lf ex =
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
            <> formulaAttrs lf
            <> ">"
    groupLine = case bioDirection ex of
        Resource -> "        <inputGroup>4</inputGroup>"
        Emission -> "        <outputGroup>4</outputGroup>"

{- | The activity a line's @activityLinkId@ should name: the supplier the load
resolved, or, where nothing was resolved, the one the source itself named. The
two differ once the supplier has been deleted, which is how a foreground model
is exported without the background it stands on: the link is gone, the claim
still holds the identity, and writing it is what lets the file relink exactly
when it is read back beside that background.
-}
statedLink :: Exchange -> Maybe UUID.UUID
statedLink ex = exchangeActivityLinkId ex <|> claimedId (exchangeSupplierClaim ex)

-- | What an @intermediateExchange@ element writes, for a technosphere or a waste line.
data IntermediateLine = IntermediateLine
    { ilName :: !(Maybe Text)
    -- ^ The flow's name, as resolved.
    , ilFlow :: !UUID.UUID
    , ilUnit :: !UUID.UUID
    , ilAmount :: !Double
    , ilUnitName :: !(Maybe Text)
    , ilSynonyms :: !(M.Map Text (S.Set Text))
    , ilGroup :: !Text
    -- ^ The in or out group line, at an indent of eight.
    , ilLink :: !(Maybe UUID.UUID)
    , ilClassification :: !(Maybe (Text, Text))
    -- ^ The classification system and value tagging a waste line.
    , ilComment :: !(Maybe Text)
    , ilFormula :: !LineFormula
    }

{- | Shared @intermediateExchange@ emitter for technosphere and waste flows.
The @activityLinkId@ is emitted only when the exchange names an activity (matching
the parser, which treats a nil or empty attribute as "no link"). An optional
@classification@ tags waste flows.
-}
intermediateExchange :: IntermediateLine -> [Text]
intermediateExchange line =
    [openTag]
        ++ nameLine 8 (ilName line)
        ++ unitNameLine 8 (ilUnitName line)
        ++ [ilGroup line]
        ++ classificationBlock (ilClassification line)
        ++ synonymLines 8 (ilSynonyms line)
        ++ commentLines 8 (ilComment line)
        ++ ["      </intermediateExchange>"]
  where
    openTag =
        "      <intermediateExchange intermediateExchangeId=\""
            <> escapeAttr (UUID.toText (ilFlow line))
            <> "\" unitId=\""
            <> escapeAttr (UUID.toText (ilUnit line))
            <> "\" amount=\""
            <> doubleAttr (ilAmount line)
            <> "\""
            <> linkAttr
            <> formulaAttrs (ilFormula line)
            <> ">"
    linkAttr
        | Just linked <- ilLink line = " activityLinkId=\"" <> escapeAttr (UUID.toText linked) <> "\""
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

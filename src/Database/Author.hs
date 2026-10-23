{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Database.Author
Description : Turning authored activity descriptions into database rows

Importing a database is tolerant: a supplier link that resolves to nothing is
warned about and dropped ('Database.MatrixBuild.findProducer'), because the
alternative – refusing a 20 000-dataset file over one bad row – helps nobody.
Authoring is the opposite situation. The author is present, the batch is small,
and every defect is fixable on the spot, so this module refuses instead of
repairing: an unresolvable supplier, an impossible unit conversion, a
non-finite amount and an empty name are all 'Left'. Nothing is guessed, and
nothing is silently dropped.

Identity is deterministic. An authored activity is addressed by
@(activityUUID, productUUID)@ like every other process, and both halves are
UUID5-minted from what the author wrote (see 'authoredNamespace'), never from
a counter or a clock. Authoring the same description twice therefore yields
the same key – which is what makes "write this activity again with one number
changed" a replace rather than a silent duplicate.

The product UUID keys on name *and* unit. Two activities that both produce
@milk@ in @kg@ share one product flow, exactly as a database with several
producers of the same product does; @milk@ in @kg@ and @milk@ in @l@ are two
flows, because they are. That makes "one flow UUID carrying two different
units" unrepresentable rather than merely unlikely.

Scope: one reference product per authored activity. Coproducts and allocation
are a later phase, and the types here do not pretend to support them – an
'AuthoredActivity' has exactly one product field group, not a list.

Editing an imported activity is the other half ('applyExchangeEdits'). A row
that came in from a database file cannot be re-authored: its identity was
minted by whichever parser read it, so re-describing it addresses a different
row. And even if it could be addressed, a description cannot carry back what
it never expressed – classification, synonyms, parameters, pedigree,
coproducts. So adjusting an imported inventory names the lines to change and
leaves everything else exactly as it was. Its name, location and description
can be restated the same way: an imported row's identity is the one its parser
minted, which none of them enters.
-}
module Database.Author (
    -- * What an author writes
    AuthoredActivity (..),
    AuthoredExchange (..),
    FlowRef (..),
    AuthorContext (..),
    editingContext,

    -- * What the database can take
    ResolvedInsert (..),
    validateAuthored,
    resolveProcess,

    -- * Editing an inventory in place
    ExchangeSelector (..),
    ExchangeEdit (..),
    ActivityText (..),
    EditedActivity (..),
    RestatedLine (..),
    applyExchangeEdits,
    amountsBeforeEach,
    retextsAuthoredIdentity,
    describeSelector,
    lineSelector,
    selectorPredicate,

    -- * Deterministic identity
    authoredNamespace,
    authoredActivityUUID,
    authoredProductUUID,
    authoredBioFlowUUID,
) where

import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Indexing (repeated)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V

import qualified Expr
import Parametric (Before (..), describeParameterRefusal, formulaProblems, parameterProblems, restated, shareFormulas)
import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    ExchangeFormula (..),
    ExchangeLocation (..),
    ExchangeProperties (..),
    Medium (..),
    Parameter (..),
    ParameterDefinition (..),
    Price (..),
    ProcessId,
    ProcessRef (..),
    StatedAmount (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    UnitDB,
    WasteFlow (..),
    declaredLocationSource,
    exchangeAmount,
    exchangeFlowId,
    exchangeFormula,
    exchangeIsInput,
    exchangeIsProductOutput,
    exchangeIsReference,
    exchangeProperties,
    findProcessIdByActivityUUID,
    getUnitNameForExchange,
    isPlaceholderUnit,
    locationCode,
    mediumText,
    noDates,
    noProperties,
    parseProcessRef,
    processRefText,
    withAmount,
    withFormula,
 )
import UnitConversion (UnitConfig, UnitReading (..), convertUnit, readUnit, unitKey)

-- ---------------------------------------------------------------------------
-- Deterministic identity
-- ---------------------------------------------------------------------------

{- | UUID5 namespace for everything an author creates, alongside the
per-format namespaces each parser owns ('SimaPro.Parser.simaproNamespace',
@ecospold1Namespace@). A separate namespace is what keeps an authored
@wheat, at farm/FR@ from colliding with an imported one of the same name: the
two are different datasets and must stay different rows.
-}
authoredNamespace :: UUID
authoredNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "volca:authored")

{- | Fields are joined with NUL before hashing: an author may write any
printable character in a name, so a printable separator would let two
different descriptions mint one key ("wheat @farm" in "FR" versus "wheat"
in "farm@FR").
-}
mintAuthored :: [Text] -> UUID
mintAuthored = UUID5.generateNamed authoredNamespace . BS.unpack . TE.encodeUtf8 . T.intercalate "\0"

-- | Activity half of the process key: what the activity does, and where.
authoredActivityUUID :: Text -> Text -> UUID
authoredActivityUUID name location = mintAuthored ["activity", name, location]

{- | Product half of the process key. Keyed on unit as well as name – see the
module header on why @milk@ in @kg@ and @milk@ in @l@ are two flows.
-}
authoredProductUUID :: Text -> Text -> UUID
authoredProductUUID productName unit = mintAuthored ["product", productName, unit]

{- | Identity of a biosphere flow an author introduces: name, compartment and
unit, so re-declaring the same flow in a second activity reuses the first one
instead of forking it. An imported flow is identified by what its file says
instead, which for SimaPro is the name and the compartment alone.
-}
authoredBioFlowUUID :: Text -> Compartment -> Text -> UUID
authoredBioFlowUUID name comp unit =
    mintAuthored ["flow", name, mediumText (compartmentName comp), fromMaybe "" (compartmentSub comp), unit]

-- ---------------------------------------------------------------------------
-- What an author writes
-- ---------------------------------------------------------------------------

{- | The flow a biosphere exchange names: by identifier, or in words.

Words reach the flow the database already declares under that name and
compartment, and introduce one only when nothing answers to them. An author
reading an inventory writes the names it shows, so the alternative would have
them mint a twin of a curated flow, uncharacterized and scoring as zero.

There is deliberately no technosphere counterpart. A technosphere input always
names a *supplier* – a product that something in scope produces – so the flow
is whatever that supplier's process key already says it is. Authoring therefore
never mints a technosphere flow from words: the activity's own product is
minted from the activity, and a supplier living in a dependency has its product
flow copied out of that dependency ('adoptTechFlow'), never invented.
-}
data FlowRef
    = FlowById UUID
    | -- | name, compartment, unit: an existing flow when one carries them, a new one otherwise
      FlowByName Text Compartment Text
    deriving (Eq, Show)

{- | One line of an authored activity's inventory.

@ati*@ / @aw*@ carry a @process_id@ in the same currency the API and the UI
speak (@activityUUID_productUUID@, or a bare activity UUID when the activity
has a single product), never a matrix index – those renumber on every edit.

'AuthoredWasteOutput' is a waste this activity generates and hands to a
treatment process: the provider is the treatment activity, exactly as a
technosphere input's provider is the producer. The two resolve identically;
only the direction differs.
-}
data AuthoredExchange
    = AuthoredTechInput
        { atiProvider :: Text
        , atiAmount :: Double
        , atiUnit :: Maybe Text
        , atiComment :: Maybe Text
        }
    | AuthoredBio
        { abFlow :: FlowRef
        , abDirection :: BioDirection
        , abAmount :: Double
        , abUnit :: Maybe Text
        , abComment :: Maybe Text
        }
    | AuthoredWasteOutput
        { awProvider :: Text
        , awAmount :: Double
        , awUnit :: Maybe Text
        , awComment :: Maybe Text
        }
    deriving (Eq, Show)

-- | A complete activity as an author states it: what it is, and what it exchanges.
data AuthoredActivity = AuthoredActivity
    { aaName :: Text
    , aaLocation :: Text
    , aaDescription :: [Text]
    , aaProductName :: Text
    , aaProductAmount :: Double
    , aaProductUnit :: Text
    , aaExchanges :: [AuthoredExchange]
    }
    deriving (Eq, Show)

{- | Everything resolution needs to read: the database being edited, the
databases it may draw suppliers from, and the unit table conversions are
judged against.
-}
data AuthorContext = AuthorContext
    { acDb :: Database
    , acDeps :: [Database]
    , acUnitConfig :: UnitConfig
    }

{- | The context an edit of @db@ is judged in, @db@ taking along every unit of
its dependencies it has no row for.

An amount to a supplier in a dependency is stated in that supplier's own unit,
and the exchange names that unit by a row of the edited database. A database
read from SimaPro records each amount in the base unit of its dimension, so it
has no row for a kWh or a t*km, and writing it a link to an EcoSpold 2
supplier sold by the kWh would be refused for want of one. The dependency's
row is adopted instead, as its product flow already is. A unit the edited
database names itself, under any of its spellings, keeps its own row, and of
two dependencies naming one unit the first one's row is adopted, so every
spelling keeps one row.
-}
editingContext :: UnitConfig -> Database -> [Database] -> AuthorContext
editingContext cfg db deps =
    AuthorContext{acDb = db{dbUnits = foldl' adopt (dbUnits db) offered}, acDeps = deps, acUnitConfig = cfg}
  where
    offered :: [Unit]
    offered = [u | dep <- deps, u <- M.elems (dbUnits dep), not (isPlaceholderUnit u)]

    adopt :: UnitDB -> Unit -> UnitDB
    adopt units u
        | any (`M.member` unitIndex cfg units) (spellings u) = units
        | otherwise = M.insert (unitId u) u units

    spellings :: Unit -> [Text]
    spellings u = [unitKey cfg key | key <- [unitSymbol u, unitName u], not (T.null (T.strip key))]

{- | An authored activity that the database can accept, with the vocabulary it
brings along. The caller inserts the flows and the activity together –
'Database.Edit.insertActivities' does – so the activity never lands referring
to a flow nothing declares.
-}
data ResolvedInsert = ResolvedInsert
    { riKey :: (UUID, UUID)
    , riActivity :: Activity
    , riNewTechFlows :: [TechnosphereFlow]
    , riNewWasteFlows :: [WasteFlow]
    , riNewBioFlows :: [BiosphereFlow]
    }

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

{- | Resolve a batch of authored activities, or report everything wrong with it.

Errors accumulate across the whole batch and across every exchange of every
activity: an author fixing a ten-line inventory sees ten complaints once, not
one complaint ten times. Each message names the activity it belongs to.

Warnings ('snd' of the success case) never block. Today the only one is a
biosphere flow new to the database, which by construction no characterization
factor matches by UUID – it may still be reached by name, so it is a caution
and not a refusal.

Whether the key may already exist is not decided here: this function does not
know if the caller means to insert or to replace.
'Database.Edit.insertActivities' and 'Database.Edit.replaceActivities' own
that check, each in the direction it means. What *is* checked here is that the
batch does not mint one key twice, since no intent makes that coherent.
-}
validateAuthored :: AuthorContext -> [AuthoredActivity] -> Either [Text] ([ResolvedInsert], [Text])
validateAuthored ctx activities =
    case partitionEithers (map (validateOne ctx) activities) of
        ([], oks) ->
            let inserts = map fst oks
             in case repeated (map riKey inserts) of
                    [] -> Right (inserts, concatMap snd oks)
                    dups -> Left (map duplicateMessage dups)
        (errs, _) -> Left (concat errs)
  where
    duplicateMessage (a, p) =
        "Two activities in this batch mint the same identity ("
            <> UUID.toText a
            <> "_"
            <> UUID.toText p
            <> "): same name, location, product and unit."

validateOne :: AuthorContext -> AuthoredActivity -> Either [Text] (ResolvedInsert, [Text])
validateOne ctx a =
    case (productChecks, partitionEithers (map resolveExchange (aaExchanges a)), mProductUnit) of
        ([], ([], resolved), Just (unitRef, unitLabel)) ->
            let key = (authoredActivityUUID (aaName a) (aaLocation a), authoredProductUUID (aaProductName a) unitLabel)
                productExchange =
                    TechnosphereExchange
                        { techFlowId = snd key
                        , techAmount = aaProductAmount a
                        , techUnitId = unitRef
                        , techRole = ReferenceProduct
                        , -- Self-link, as every loaded database records its
                          -- reference products.
                          techActivityLinkId = Just (fst key)
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techFormula = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                productFlow =
                    TechnosphereFlow
                        { tfId = snd key
                        , tfName = aaProductName a
                        , tfUnitId = unitRef
                        , tfSynonyms = M.empty
                        , tfCAS = Nothing
                        , tfSubstanceId = Nothing
                        }
             in Right
                    ( ResolvedInsert
                        { riKey = key
                        , riActivity = buildActivity a unitLabel (productExchange : map reExchange resolved)
                        , riNewTechFlows =
                            [productFlow | not (M.member (snd key) (dbTechFlows (acDb ctx)))]
                                <> mapMaybe reNewTechFlow resolved
                        , riNewWasteFlows = mapMaybe reNewWasteFlow resolved
                        , riNewBioFlows = mapMaybe reNewBioFlow resolved
                        }
                    , map here (concatMap reWarnings resolved)
                    )
        (perActivityErrs, (exchangeErrs, _), _) -> Left (map here (perActivityErrs <> concat exchangeErrs))
  where
    here msg = aaName a <> " {" <> aaLocation a <> "}: " <> msg
    resolveExchange ex = mapLeft (map ((describeExchange ex <> ": ") <>)) (resolveOne ctx ex)
    mProductUnit = lookupUnit ctx (aaProductUnit a)
    productChecks =
        concat
            [ ["the activity name is empty" | T.null (T.strip (aaName a))]
            , ["the product name is empty" | T.null (T.strip (aaProductName a))]
            , [ "the product amount is " <> T.pack (show (aaProductAmount a)) <> "; it must be a finite non-zero number"
              | not (isUsableAmount (aaProductAmount a))
              ]
            , [ unitRefusal ctx (aaProductUnit a) <> " for the product"
              | Nothing <- [mProductUnit]
              ]
            ]

buildActivity :: AuthoredActivity -> Text -> [Exchange] -> Activity
buildActivity a unitLabel exchangeList =
    Activity
        { activityName = aaName a
        , activityDescription = aaDescription a
        , activityDocumentation = [] -- An authored activity states no source of its own
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = aaLocation a
        , activityLocationSource = declaredLocationSource (aaLocation a)
        , activityUnit = unitLabel
        , exchanges = exchangeList
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

{- | How a complaint names the line it is about. What an author can act on is
which supplier or which flow, not a position in a list – an API caller may not
even have sent the exchanges as one list.
-}
describeExchange :: AuthoredExchange -> Text
describeExchange ex = case ex of
    AuthoredTechInput{atiProvider = provider} -> "input from \"" <> provider <> "\""
    AuthoredWasteOutput{awProvider = provider} -> "waste output to \"" <> provider <> "\""
    AuthoredBio{abFlow = FlowById flowId} -> "biosphere flow " <> UUID.toText flowId
    AuthoredBio{abFlow = FlowByName name _ _} -> "biosphere flow \"" <> name <> "\""

{- | An amount that can carry information: finite, and not zero. A zero
exchange is not a measurement of nothing, it is a line that should not have
been written – and it would divide into a zero normalization factor.
-}
isUsableAmount :: Double -> Bool
isUsableAmount x = not (isNaN x) && not (isInfinite x) && x /= 0

-- | The complaint an unusable amount earns, in the one wording every surface shows.
amountCheck :: Double -> [Text]
amountCheck amount =
    [ "the amount is " <> T.pack (show amount) <> "; it must be a finite non-zero number"
    | not (isUsableAmount amount)
    ]

-- ---------------------------------------------------------------------------
-- Editing an inventory in place
-- ---------------------------------------------------------------------------

{- | Which line of an inventory an edit is about.

A technosphere input names its provider, a waste output names the treatment it
hands the waste to, a biosphere exchange names its flow – the same currency an
author writes in ('AuthoredExchange'). A line that names no provider is named
by its flow and its location instead, which is what the loader finds its
supplier by: the supplier found depends on the databases loaded, the line does
not, so an edit replays whatever is loaded. What is missing is missing on purpose:
the reference product and any coproduct carry the activity's identity and its
allocation, and a reference input belongs to the treatment that consumes it.
Those lines match no selector, so an edit cannot reach them by accident.
-}
data ExchangeSelector
    = -- | Technosphere input, by its provider's process id.
      SelectInput Text
    | -- | Biosphere exchange, by flow identity.
      SelectBiosphere UUID
    | -- | Waste output, by its treatment provider's process id.
      SelectWaste Text
    | -- | Technosphere input naming no provider, by its flow, and its location unless any will do.
      SelectUnlinkedInput UUID (Maybe ExchangeLocation)
    | -- | Waste output naming no treatment, by its flow, and its location unless any will do.
      SelectUnlinkedWaste UUID (Maybe ExchangeLocation)
    deriving (Eq, Show)

{- | One change to an activity: a line of its inventory, one of its texts, or
one of its parameters. Edits apply in the order given, so removing a line and
then setting its amount is refused – by then it matches nothing – rather than
quietly reordered into something that works.
-}
data ExchangeEdit
    = RemoveExchange ExchangeSelector
    | SetAmount ExchangeSelector Double
    | AddExchange AuthoredExchange
    | SetText ActivityText
    | -- | A line's formula in VoLCA's language, or none; a named line keeps its name either way.
      SetFormula ExchangeSelector (Maybe Text)
    | {- | The price of a product output, named by its flow, or none. It is the
      one edit that reaches the product side: a price weighs a product against
      the others of its block, and leaves what the activity is untouched.
      -}
      SetPrice UUID (Maybe StatedAmount)
    | -- | A parameter of the activity, added, or replaced under the same spelling.
      SetParameter Text Parameter
    | RemoveParameter Text
    deriving (Eq, Show)

-- | A text of an activity, as an edit restates it.
data ActivityText
    = ActivityName Text
    | ActivityLocation Text
    | -- | By paragraphs; none at all clears it.
      ActivityDescription [Text]
    deriving (Eq, Show)

{- | Why an edit cannot restate this activity's texts, when it cannot. An
authored activity's identity is minted from its name and location
('authoredActivityUUID'), so restating either would leave it under a key its
own description no longer mints, and the next rewrite of it would be refused
as addressing another activity. Such an activity is rewritten instead, which
moves it to the identity its new name mints.
-}
retextsAuthoredIdentity :: (UUID, UUID) -> Activity -> [ExchangeEdit] -> Maybe Text
retextsAuthoredIdentity (activityKey, _) act edits
    | activityKey == authoredActivityUUID (activityName act) (activityLocation act)
    , any entersIdentity edits =
        Just "This activity was written here and its name and location make its identity: rewrite it to rename or relocate it."
    | otherwise = Nothing
  where
    entersIdentity :: ExchangeEdit -> Bool
    entersIdentity = \case
        SetText (ActivityName _) -> True
        SetText (ActivityLocation _) -> True
        _ -> False

{- | An edited activity, and what it took to get there.

'eaMatched' holds one count per edit, in the order the edits were given: a
selector that named three lines says three, so a caller can tell "removed the
one I meant" from "removed three". 'Database.Journal' records those counts and
compares them when replaying the edit, which is what stops a recorded edit
from landing on a different number of lines than it did the first time.
-}
data EditedActivity = EditedActivity
    { eaActivity :: Activity
    , eaMatched :: [Int]
    , eaNewBioFlows :: [BiosphereFlow]
    , eaNewTechFlows :: [TechnosphereFlow]
    , eaNewWasteFlows :: [WasteFlow]
    , eaWarnings :: [Text]
    , eaRestated :: [RestatedLine]
    }

-- | A line whose formula no longer gave its amount once the edits were made, and the amount it took.
data RestatedLine = RestatedLine
    { rlPosition :: Int
    , rlFrom :: Double
    , rlTo :: Double
    }
    deriving (Eq, Show)

{- | Apply edits to one activity's inventory, or report everything wrong with
them.

Only 'exchanges' and the texts and parameters an edit names change.
Classification, synonyms, allocation, native type, pedigree on the lines left
alone – all carried through as they were, which is the whole point: an
imported activity can be adjusted without being re-described as something a
description can express.

A kept formula gives its line's amount at the defaults, and an edit can break
that: a new default, a new formula, a named line set to another amount that
other formulas read. So the edited activity is restated ('Parametric.restated')
once every edit is in, and the lines it moved are reported.

A selector matching nothing is refused rather than passed off as done. One
matching several lines applies to all of them and reports how many. Complaints
accumulate across the whole list, as they do for a written batch.
-}
applyExchangeEdits :: AuthorContext -> [ExchangeEdit] -> Activity -> Either [Text] EditedActivity
applyExchangeEdits ctx edits act = case accErrors final of
    [] -> case restated (Before (acDb ctx) act) (acDb ctx) (accActivity final) of
        Left refusal -> Left [describeParameterRefusal refusal]
        Right stated ->
            Right
                EditedActivity
                    { eaActivity = stated
                    , eaMatched = accMatched final
                    , eaNewBioFlows = accNewFlows final
                    , eaNewTechFlows = accNewTechFlows final
                    , eaNewWasteFlows = accNewWasteFlows final
                    , eaWarnings = accWarnings final
                    , eaRestated =
                        [ RestatedLine{rlPosition = position, rlFrom = exchangeAmount before, rlTo = exchangeAmount after}
                        | (position, before, after) <- zip3 [0 ..] (exchanges (accActivity final)) (exchanges stated)
                        , exchangeAmount before /= exchangeAmount after
                        ]
                    }
    errs -> Left errs
  where
    final = foldl' (applyStep ctx) (startingFrom act) edits

startingFrom :: Activity -> EditAcc
startingFrom act =
    EditAcc
        { accActivity = act
        , accMatched = []
        , accNewFlows = []
        , accNewTechFlows = []
        , accNewWasteFlows = []
        , accWarnings = []
        , accErrors = []
        }

{- | The amounts of the lines each edit names, just before it: what the history
shows a reader beside the value that replaced them. Each edit is read against
the lines the edits before it in the batch left, as 'applyExchangeEdits' applies
it. An edit naming no line has none.
-}
amountsBeforeEach :: AuthorContext -> Activity -> [ExchangeEdit] -> [[Double]]
amountsBeforeEach ctx act edits = zipWith (amountsBefore . accActivity) (scanl (applyStep ctx) (startingFrom act) edits) edits

amountsBefore :: Activity -> ExchangeEdit -> [Double]
amountsBefore activity = \case
    SetAmount selector _ -> named selector
    RemoveExchange selector -> named selector
    SetFormula selector _ -> named selector
    SetPrice _ _ -> []
    AddExchange _ -> []
    SetText _ -> []
    SetParameter _ _ -> []
    RemoveParameter _ -> []
  where
    named selector = either (const []) (\selected -> map exchangeAmount (filter selected (exchanges activity))) (selectorPredicate selector)

-- | The activity as edited so far, and what there is to report about it.
data EditAcc = EditAcc
    { accActivity :: Activity
    , accMatched :: [Int]
    , accNewFlows :: [BiosphereFlow]
    , accNewTechFlows :: [TechnosphereFlow]
    , accNewWasteFlows :: [WasteFlow]
    , accWarnings :: [Text]
    , accErrors :: [Text]
    }

{- | Fold one edit in. A refused edit leaves the inventory as it was, so the
edits after it are still judged against something coherent and the author sees
every complaint in one pass.
-}
applyStep :: AuthorContext -> EditAcc -> ExchangeEdit -> EditAcc
applyStep ctx acc edit = case applyOneEdit ctx (accActivity acc) edit of
    Left errs -> acc{accErrors = accErrors acc <> errs}
    Right step ->
        acc
            { accActivity = esActivity step
            , accMatched = accMatched acc <> [esMatched step]
            , accNewFlows = accNewFlows acc <> esNewFlows step
            , accNewTechFlows = accNewTechFlows acc <> esNewTechFlows step
            , accNewWasteFlows = accNewWasteFlows acc <> esNewWasteFlows step
            , accWarnings = accWarnings acc <> esWarnings step
            }

-- | What one applied edit leaves behind.
data EditStep = EditStep
    { esActivity :: Activity
    , esMatched :: Int
    , esNewFlows :: [BiosphereFlow]
    , esNewTechFlows :: [TechnosphereFlow]
    , esNewWasteFlows :: [WasteFlow]
    , esWarnings :: [Text]
    }

applyOneEdit :: AuthorContext -> Activity -> ExchangeEdit -> Either [Text] EditStep
applyOneEdit ctx act edit = case edit of
    RemoveExchange sel -> case selectFrom sel current of
        Left err -> Left [err]
        Right (matched, isSelected) -> Right (changed (filter (not . isSelected) current) matched)
    SetAmount sel amount -> onSelected (amountCheck amount) (selectFrom sel current) (withAmount amount)
    SetFormula sel formula -> onSelected (foldMap formulaProblems formula) (selectFrom sel current) (reformulated formula)
    SetPrice flow price ->
        (\step -> step{esWarnings = besideDeduced flow price current})
            <$> onSelected (foldMap priceCheck price) (selectProduct flow current) (priced price)
    -- An added line resolves exactly as a written one does: same provider
    -- lookup, same unit rules, same new-flow warning.
    AddExchange authored -> case resolveOne ctx authored of
        Left errs -> Left (map ((describeExchange authored <> ": ") <>) errs)
        Right resolved ->
            Right
                EditStep
                    { esActivity = act{exchanges = current <> [reExchange resolved]}
                    , esMatched = 1
                    , esNewFlows = maybeToList (reNewBioFlow resolved)
                    , esNewTechFlows = maybeToList (reNewTechFlow resolved)
                    , esNewWasteFlows = maybeToList (reNewWasteFlow resolved)
                    , esWarnings = reWarnings resolved
                    }
    SetText text -> case textCheck text of
        [] -> Right (stepTo (restateText text act) 1)
        errs -> Left errs
    SetParameter name parameter -> case parameterProblems "this activity" (activityParameters act) (lineNames act) name parameter of
        [] -> Right (stepTo act{activityParameters = M.insert name parameter (activityParameters act)} 1)
        errs -> Left errs
    RemoveParameter name -> case removalCheck (acDb ctx) act name of
        Left errs -> Left errs
        Right spelt -> Right (stepTo act{activityParameters = M.delete spelt (activityParameters act)} 1)
  where
    current :: [Exchange]
    current = exchanges act
    changed :: [Exchange] -> Int -> EditStep
    changed exchangeList = stepTo act{exchanges = exchangeList}
    stepTo :: Activity -> Int -> EditStep
    stepTo edited matched =
        EditStep
            { esActivity = edited
            , esMatched = matched
            , esNewFlows = []
            , esNewTechFlows = []
            , esNewWasteFlows = []
            , esWarnings = []
            }
    onSelected :: [Text] -> Either Text (Int, Exchange -> Bool) -> (Exchange -> Exchange) -> Either [Text] EditStep
    onSelected complaints selection change = case (complaints, selection) of
        ([], Right (matched, isSelected)) ->
            Right (changed (map (\ex -> if isSelected ex then change ex else ex) current) matched)
        (errs, Right _) -> Left errs
        (errs, Left err) -> Left (errs <> [err])

-- | The product outputs of this flow, or why there are none.
selectProduct :: UUID -> [Exchange] -> Either Text (Int, Exchange -> Bool)
selectProduct flow current = case length (filter isProduct current) of
    0 -> Left ("the flow " <> UUID.toText flow <> " is no product output of this activity")
    matched -> Right (matched, isProduct)
  where
    isProduct :: Exchange -> Bool
    isProduct ex = exchangeIsProductOutput ex && exchangeFlowId ex == flow

-- | A price stated by hand: a finite amount, in a currency it names.
priceCheck :: StatedAmount -> [Text]
priceCheck (StatedAmount currency amount) =
    ["the price is " <> T.pack (show amount) <> "; it must be a finite number, zero or more" | isNaN amount || isInfinite amount || amount < 0]
        <> ["a price names its currency" | T.null (T.strip currency)]

{- | A stated price set beside a price another product of the block deduced
from its share: the price key cannot compare the two, so it would refuse the
block until every product is priced by hand.
-}
besideDeduced :: UUID -> Maybe StatedAmount -> [Exchange] -> [Text]
besideDeduced flow price current =
    [ "the other products of this activity carry prices deduced from their declared shares, which the price key cannot compare with this one: price each of them"
    | isJust price
    , any deducedElsewhere current
    ]
  where
    deducedElsewhere :: Exchange -> Bool
    deducedElsewhere ex = exchangeIsProductOutput ex && exchangeFlowId ex /= flow && isDeduced (epPrice (exchangeProperties ex))
    isDeduced :: Maybe Price -> Bool
    isDeduced = \case
        Just DeducedFromShare{} -> True
        _ -> False

priced :: Maybe StatedAmount -> Exchange -> Exchange
priced price ex = case ex of
    TechnosphereExchange{techProperties = props} -> ex{techProperties = props{epPrice = StatedPrice <$> price}}
    _ -> ex

{- | A line given a formula keeps its name, and its amount until the activity
is restated; a line given none keeps its amount, and a named line its name on
that number.
-}
reformulated :: Maybe Text -> Exchange -> Exchange
reformulated = \case
    Nothing -> \ex -> withAmount (exchangeAmount ex) ex
    Just formula -> \ex -> withFormula (Just ExchangeFormula{efName = efName =<< exchangeFormula ex, efText = T.strip formula}) ex

{- | The spelling of the parameter to remove, unless a formula of the activity
still reads it and its database has no parameter of that name to take over.
-}
removalCheck :: Database -> Activity -> Text -> Either [Text] Text
removalCheck db act name = case [other | other <- M.keys (activityParameters act), T.toLower other == key] of
    [] ->
        Left ["this activity has no parameter '" <> name <> "'"]
    spelt : _
        | key `elem` map T.toLower (M.keys (dbParameters db)) -> Right spelt
        | otherwise -> case readers of
            [] -> Right spelt
            some -> Left ["the parameter '" <> name <> "' is still read by " <> T.intercalate ", " (map (\f -> "'" <> f <> "'") some)]
  where
    key :: Text
    key = T.toLower name
    readers :: [Text]
    readers =
        nub
            ( [f | f <- exchangeFormulas <> parameterFormulas, reads' Expr.Volca f]
                <> [f | f <- shareFormulas act, reads' Expr.SimaPro f]
            )
    reads' :: Expr.Dialect -> Text -> Bool
    reads' dialect formula = key `elem` map T.toLower (Expr.collectIdentifiers dialect formula)
    exchangeFormulas :: [Text]
    exchangeFormulas = [efText f | Just f <- map exchangeFormula (exchanges act)]
    parameterFormulas :: [Text]
    parameterFormulas = [f | (other, Parameter{parameterDefinition = CalculatedParameter f _}) <- M.toList (activityParameters act), T.toLower other /= key]

lineNames :: Activity -> [Text]
lineNames act = [T.toLower n | Just ExchangeFormula{efName = Just n} <- map exchangeFormula (exchanges act)]

-- | A name or a location left blank names nothing to find the activity by.
textCheck :: ActivityText -> [Text]
textCheck = \case
    ActivityName name -> ["an activity's name cannot be blank" | T.null (T.strip name)]
    ActivityLocation location -> ["an activity's location cannot be blank" | T.null (T.strip location)]
    ActivityDescription _ -> []

restateText :: ActivityText -> Activity -> Activity
restateText = \case
    ActivityName name -> \act -> act{activityName = T.strip name}
    ActivityLocation location ->
        \act -> act{activityLocation = T.strip location, activityLocationSource = declaredLocationSource location}
    ActivityDescription paragraphs -> \act -> act{activityDescription = paragraphs}

{- | The lines a selector names, and how many there are. Zero is a refusal: an
edit that matched nothing did not do what it was asked, and reporting success
would hide that from the only person who can fix it.
-}
selectFrom :: ExchangeSelector -> [Exchange] -> Either Text (Int, Exchange -> Bool)
selectFrom sel current = do
    isSelected <- selectorPredicate sel
    case length (filter isSelected current) of
        0 -> Left (describeSelector sel <> " matches no exchange of this activity")
        matched -> Right (matched, isSelected)

{- | Turn a selector into the test it stands for, or say why it cannot be one.
The product side is out of reach by construction: no branch below answers
'True' for a reference product, a coproduct, a reference input or a waste
input.
-}
selectorPredicate :: ExchangeSelector -> Either Text (Exchange -> Bool)
selectorPredicate sel = case sel of
    SelectBiosphere flowId -> Right (isBiosphereFlow flowId)
    SelectInput provider -> byProvider provider isInputFrom
    SelectWaste provider -> byProvider provider isWasteOutputTo
    SelectUnlinkedInput flowId location -> Right (isUnlinkedInputOf flowId location)
    SelectUnlinkedWaste flowId location -> Right (isUnlinkedWasteOutputOf flowId location)
  where
    byProvider provider build = case parseProvider provider of
        Nothing ->
            Left
                ( describeSelector sel
                    <> " is not a process id: expected activityUUID_productUUID, or a bare activity UUID"
                )
        Just key -> Right (build key)

isBiosphereFlow :: UUID -> Exchange -> Bool
isBiosphereFlow flowId = \case
    BiosphereExchange{bioFlowId = f} -> f == flowId
    TechnosphereExchange{} -> False
    WasteExchange{} -> False

isInputFrom :: ProviderKey -> Exchange -> Bool
isInputFrom key = \case
    TechnosphereExchange{techRole = Input, techActivityLinkId = a, techFlowId = f} -> matchesProvider key a f
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

isWasteOutputTo :: ProviderKey -> Exchange -> Bool
isWasteOutputTo key = \case
    WasteExchange{waIsInput = False, waActivityLinkId = a, waFlowId = f} -> matchesProvider key a f
    WasteExchange{} -> False
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False

isUnlinkedInputOf :: UUID -> Maybe ExchangeLocation -> Exchange -> Bool
isUnlinkedInputOf flowId location = \case
    TechnosphereExchange{techRole = Input, techActivityLinkId = Nothing, techFlowId = f, techLocation = l} -> f == flowId && atLocation location l
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

isUnlinkedWasteOutputOf :: UUID -> Maybe ExchangeLocation -> Exchange -> Bool
isUnlinkedWasteOutputOf flowId location = \case
    WasteExchange{waIsInput = False, waActivityLinkId = Nothing, waFlowId = f, waLocation = l} -> f == flowId && atLocation location l
    WasteExchange{} -> False
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False

atLocation :: Maybe ExchangeLocation -> ExchangeLocation -> Bool
atLocation wanted stated = maybe True (== stated) wanted

{- | The selector that names this line from what the line itself stores, so it
names the same line at every replay; none for the product side, which no
selector reaches. Two lines it cannot tell apart get the same one, and an edit
by it changes both.
-}
lineSelector :: Exchange -> Maybe ExchangeSelector
lineSelector = \case
    TechnosphereExchange{techRole = Input, techActivityLinkId = link, techFlowId = f, techLocation = l} ->
        Just (maybe (SelectUnlinkedInput f (Just l)) (\a -> SelectInput (providerText a f)) link)
    TechnosphereExchange{} -> Nothing
    WasteExchange{waIsInput = False, waActivityLinkId = link, waFlowId = f, waLocation = l} ->
        Just (maybe (SelectUnlinkedWaste f (Just l)) (\a -> SelectWaste (providerText a f)) link)
    WasteExchange{} -> Nothing
    BiosphereExchange{bioFlowId = f} -> Just (SelectBiosphere f)
  where
    providerText :: UUID -> UUID -> Text
    providerText a f = processRefText (ProcessRef a f)

{- | A provider named either way the engine accepts one. Matched against the
link the exchange already carries rather than resolved in the database, so a
provider living in a dependency – or one that has gone missing since the
import – is still addressable.
-}
data ProviderKey = ProviderPair UUID UUID | ProviderActivity UUID

parseProvider :: Text -> Maybe ProviderKey
parseProvider provider = case parseProcessRef provider of
    Just ref -> Just (ProviderPair (prActivity ref) (prProduct ref))
    Nothing -> ProviderActivity <$> UUID.fromText provider

matchesProvider :: ProviderKey -> Maybe UUID -> UUID -> Bool
matchesProvider key linkedActivity linkedFlow = case key of
    ProviderPair activityId productId -> Just activityId == linkedActivity && productId == linkedFlow
    ProviderActivity activityId -> Just activityId == linkedActivity

{- | How a complaint names a selector, phrased like 'describeExchange': the
author who writes a line and the author who selects one are the same person.
-}
describeSelector :: ExchangeSelector -> Text
describeSelector sel = case sel of
    SelectInput provider -> "input from \"" <> provider <> "\""
    SelectWaste provider -> "waste output to \"" <> provider <> "\""
    SelectBiosphere flowId -> "biosphere flow " <> UUID.toText flowId
    SelectUnlinkedInput flowId location -> "input of flow " <> UUID.toText flowId <> atText location <> " naming no provider"
    SelectUnlinkedWaste flowId location -> "waste output of flow " <> UUID.toText flowId <> atText location <> " naming no treatment"
  where
    atText :: Maybe ExchangeLocation -> Text
    atText = \case
        Nothing -> ""
        Just LocationNone -> " with no location"
        Just stated -> " in " <> locationCode stated

-- ---------------------------------------------------------------------------
-- Exchange resolution
-- ---------------------------------------------------------------------------

-- | One resolved inventory line, plus whatever it drags into the vocabulary.
data ResolvedExchange = ResolvedExchange
    { reExchange :: Exchange
    , reNewBioFlow :: Maybe BiosphereFlow
    , reNewTechFlow :: Maybe TechnosphereFlow
    , reNewWasteFlow :: Maybe WasteFlow
    , reWarnings :: [Text]
    }

-- | The flow a linked exchange brings into the edited database: one of a kind, or none.
data Adoption = NothingToAdopt | AdoptProduct TechnosphereFlow | AdoptWaste WasteFlow

resolveOne :: AuthorContext -> AuthoredExchange -> Either [Text] ResolvedExchange
resolveOne ctx ex = case ex of
    AuthoredTechInput provider amount mUnit comment ->
        resolveLinked ctx provider amount mUnit (fmap (maybe NothingToAdopt AdoptProduct) . adoptTechFlow ctx) $ \sup unitRef ->
            TechnosphereExchange
                { techFlowId = prProduct (supKey sup)
                , techAmount = amount
                , techUnitId = unitRef
                , techRole = Input
                , techActivityLinkId = Just (prActivity (supKey sup))
                , techSupplierClaim = ClaimById (prActivity (supKey sup))
                , techLocation = ""
                , techComment = comment
                , techPedigree = Nothing
                , techFormula = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
    AuthoredWasteOutput provider amount mUnit comment ->
        resolveLinked ctx provider amount mUnit (fmap (maybe NothingToAdopt AdoptWaste) . adoptWasteFlow ctx) $ \sup unitRef ->
            WasteExchange
                { waFlowId = prProduct (supKey sup)
                , waAmount = amount
                , waUnitId = unitRef
                , waIsInput = False
                , waActivityLinkId = Just (prActivity (supKey sup))
                , waSupplierClaim = ClaimById (prActivity (supKey sup))
                , waLocation = ""
                , waComment = comment
                , waPedigree = Nothing
                , waFormula = Nothing
                }
    AuthoredBio flowRef direction amount mUnit comment ->
        resolveBio ctx flowRef direction amount mUnit comment

{- | A technosphere input and a waste output differ only in the constructor
they build: both name a provider by process id, both take their flow from that
provider's process key, and both convert into that provider's reference unit.
What each must adopt differs: an input reads its flow from the product table, a
waste output from the waste table.
-}
resolveLinked ::
    AuthorContext ->
    Text ->
    Double ->
    Maybe Text ->
    (Supplier -> Either Text Adoption) ->
    (Supplier -> UUID -> Exchange) ->
    Either [Text] ResolvedExchange
resolveLinked ctx provider amount mUnit adopt build =
    case (amountCheck amount, resolveSupplier ctx provider) of
        ([], Right sup) -> mapLeft (: []) (resolved sup)
        (errs, Right _) -> Left errs
        (errs, Left err) -> Left (errs <> [err])
  where
    resolved :: Supplier -> Either Text ResolvedExchange
    resolved sup = do
        let stated = fromMaybe (defaultUnit sup) mUnit
        (unitRef, unitLabel) <-
            maybe (Left (unitRefusal ctx stated)) Right (lookupUnit ctx stated)
        maybe (Right ()) Left (unitError ctx sup unitLabel)
        adoption <- adopt sup
        maybe (Right ()) Left (dependencyUnitError ctx sup unitLabel)
        pure
            ResolvedExchange
                { reExchange = build sup unitRef
                , reNewBioFlow = Nothing
                , reNewTechFlow = case adoption of AdoptProduct flow -> Just flow; _ -> Nothing
                , reNewWasteFlow = case adoption of AdoptWaste flow -> Just flow; _ -> Nothing
                , reWarnings = []
                }
    defaultUnit :: Supplier -> Text
    defaultUnit sup =
        case (supProducedUnit sup, supAnyRefUnit sup) of
            ("", "") -> ""
            ("", other) -> other
            (produced, _) -> produced

{- | Judge the unit an amount is stated in against what the matrix will do
with it. A provider with a produced reference output gets the conversion
check; a provider whose only reference is an input (a treatment process) gets
an exact-match rule instead, because 'Database.MatrixBuild.techTriple' never
converts into a reference input – a mismatched unit would land as a wrong raw
number, not as a conversion.
-}
unitError :: AuthorContext -> Supplier -> Text -> Maybe Text
unitError ctx sup stated
    | T.null (supProducedUnit sup)
    , not (T.null (supAnyRefUnit sup))
    , unitKey (acUnitConfig ctx) stated /= unitKey (acUnitConfig ctx) (supAnyRefUnit sup) =
        Just $
            "the provider's reference is stated in \""
                <> supAnyRefUnit sup
                <> "\" but the exchange states \""
                <> stated
                <> "\"; amounts to a provider with no produced output are not converted, so restate it in \""
                <> supAnyRefUnit sup
                <> "\""
    | otherwise = conversionError ctx stated (supProducedUnit sup)

{- | The unit a value must be stated in before it can enter the technosphere
matrix. Mirrors 'Database.MatrixBuild.techTriple' exactly – same emptiness
guards, same conversion table – so a batch that validates here cannot fail the
rebuild afterwards for a reason authoring could have named first.
-}
conversionError :: AuthorContext -> Text -> Text -> Maybe Text
conversionError ctx stated supplierUnit
    | not needsConversion = Nothing
    | otherwise = case convertUnit (acUnitConfig ctx) stated supplierUnit 1 of
        Just _ -> Nothing
        Nothing ->
            Just $
                "cannot convert \""
                    <> stated
                    <> "\" into the supplier's \""
                    <> supplierUnit
                    <> "\""
  where
    needsConversion :: Bool
    needsConversion =
        unitKey (acUnitConfig ctx) stated /= unitKey (acUnitConfig ctx) supplierUnit
            && not (T.null stated)
            && not (T.null supplierUnit)

resolveBio ::
    AuthorContext ->
    FlowRef ->
    BioDirection ->
    Double ->
    Maybe Text ->
    Maybe Text ->
    Either [Text] ResolvedExchange
resolveBio ctx flowRef direction amount mUnit comment
    | not (isUsableAmount amount) =
        Left ["the amount is " <> T.pack (show amount) <> "; it must be a finite non-zero number"]
    | otherwise = case flowRef of
        FlowById flowId -> case findBioFlow ctx flowId of
            Nothing ->
                Left
                    [ "no biosphere flow "
                        <> UUID.toText flowId
                        <> " in this database or its dependencies"
                    ]
            Just (flow, ownerUnits, local) ->
                let flowUnit = unitNameOf ownerUnits (bfUnitId flow)
                 in case mUnit of
                        Just stated | not (sameUnitAs stated flowUnit) -> Left [mismatch stated flowUnit]
                        _ -> emitKnown flowId flow flowUnit local
        FlowByName name comp unit -> case findBioFlowsByName ctx name comp of
            [] -> introduce name comp unit
            [found] -> attach unit found
            several -> case filter (statedIn unit) several of
                [found] -> attach unit found
                [] -> Left [unitAmong unit several]
                ties -> Left [severalNamed comp ties]
  where
    -- The unit the author states and the one the flow carries must be the
    -- same unit, since the amount is carried through unconverted: @L@ passes
    -- for @l@, at factor 1, and case still keeps @mJ@ from passing for @MJ@.
    sameUnitAs :: Text -> Text -> Bool
    sameUnitAs = sameUnit (acUnitConfig ctx)
    -- A name the database already carries addresses that flow, rather than
    -- minting a second one under it: an introduced flow matches no
    -- characterization factor by identity, so the twin of a curated flow
    -- would score as zero next to the original.
    attach stated (flow, ownerUnits, local) =
        let flowUnit = unitNameOf ownerUnits (bfUnitId flow)
         in if not (sameUnitAs stated flowUnit)
                then Left [mismatch stated flowUnit]
                else emitKnown (bfId flow) flow flowUnit local
    -- One name and compartment in two units (an energy carrier recorded in
    -- kg and in MJ) is told apart by the unit the exchange states, which the
    -- author has already written. Nothing else is guessed at.
    statedIn stated (flow, ownerUnits, _) =
        sameUnitAs stated (unitNameOf ownerUnits (bfUnitId flow))
    introduce name comp unit = case lookupUnit ctx unit of
        Nothing -> Left [unitRefusal ctx unit <> " for flow \"" <> name <> "\""]
        Just (unitRef, unitLabel) ->
            let flowId = authoredBioFlowUUID name comp unitLabel
             in emit
                    flowId
                    unitRef
                    (Just (newBioFlow flowId name comp unitRef))
                    [ "biosphere flow \""
                        <> name
                        <> "\" is new to this database; no characterization factor matches it by identity yet"
                    ]
    -- Several flows carry the name and none is recorded in the unit stated.
    -- That is a unit to restate, not a choice to make: calling it ambiguous
    -- would send the author to identifiers that refuse for the same reason.
    unitAmong stated cands =
        "the flows carrying this name are recorded in "
            <> T.intercalate " and " (map quoted (nub (map unitOfCandidate cands)))
            <> " but the exchange states \""
            <> stated
            <> "\"; biosphere amounts are not converted, so restate it in one of them"
    -- The refusal carries the identifiers, because an author who reaches it
    -- has to name one of them and has no other place to read them from.
    severalNamed comp several =
        "more than one flow carries this name in "
            <> renderCompartment comp
            <> ": "
            <> T.intercalate ", " (map describeFlow several)
            <> "; name the one this exchange means by its identifier"
    describeFlow (flow, ownerUnits, _) =
        UUID.toText (bfId flow) <> " (" <> unitNameOf ownerUnits (bfUnitId flow) <> ")"
    unitOfCandidate (flow, ownerUnits, _) = unitNameOf ownerUnits (bfUnitId flow)
    quoted u = "\"" <> u <> "\""
    -- A flow found in a dependency is copied into the edited database with
    -- its unit remapped: characterization resolves flows through the edited
    -- database's own vocabulary ('dbBioFlows'), so an exchange must never
    -- reference a flow only a dependency declares.
    emitKnown flowId flow flowUnit local
        | local = emit flowId (bfUnitId flow) Nothing []
        | otherwise = case lookupUnit ctx flowUnit of
            Nothing ->
                Left
                    [ "biosphere flow \""
                        <> bfName flow
                        <> "\" from a dependency is stated in \""
                        <> flowUnit
                        <> "\", a unit this database does not have"
                    ]
            Just (unitRef, _) -> emit flowId unitRef (Just flow{bfUnitId = unitRef}) []
    emit flowId unitRef mNew warnings =
        Right
            ( ResolvedExchange
                BiosphereExchange
                    { bioFlowId = flowId
                    , bioAmount = amount
                    , bioUnitId = unitRef
                    , bioDirection = direction
                    , bioLocation = ""
                    , bioComment = comment
                    , bioPedigree = Nothing
                    , bioFormula = Nothing
                    }
                mNew
                Nothing
                Nothing
                warnings
            )
    -- The biosphere matrix carries amounts through unconverted, so a unit the
    -- flow does not use would land as a wrong number rather than as a
    -- conversion. Refuse and name both units.
    mismatch stated flowUnit =
        "the flow is recorded in \""
            <> flowUnit
            <> "\" but the exchange states \""
            <> stated
            <> "\"; biosphere amounts are not converted, so restate it in \""
            <> flowUnit
            <> "\""

newBioFlow :: UUID -> Text -> Compartment -> UUID -> BiosphereFlow
newBioFlow flowId name comp unitRef =
    BiosphereFlow
        { bfId = flowId
        , bfName = name
        , bfUnitId = unitRef
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just comp
        }

-- ---------------------------------------------------------------------------
-- Lookups across the edited database and its dependencies
-- ---------------------------------------------------------------------------

{- | A resolved provider. Only its @(activityUUID, productUUID)@ key goes into
the exchange, never a 'ProcessId': process ids renumber on every rebuild, so
an embedded one would silently point at whichever row inherits the number.
'Database.MatrixBuild.findProducer' resolves the pair, and a provider living
in a dependency resolves the same way through cross-database relinking.
-}
data Supplier = Supplier
    { supKey :: ProcessRef
    , supProducedUnit :: Text
    {- ^ unit of the produced reference output, @""@ for a treatment process
    that has only a reference input. Mirrors
    'Database.MatrixBuild.buildSupplierRefUnits', which is what the matrix
    converts into.
    -}
    , supAnyRefUnit :: Text
    {- ^ unit of the first reference exchange either way, used only to default
    an omitted unit – a treatment provider has no produced unit to borrow.
    -}
    , supHome :: SupplierHome
    -- ^ which database the provider was found in, and what has to be copied out of it.
    }

{- | Where a resolved provider lives, and what a foreign one obliges the edited
database to adopt.

'Dependency' carries the product flow that database declares for the provider
and the name of the unit that flow is stated in over there – or nothing, when
that database names a process whose product flow it never declared. That is a
malformed database rather than an impossible one, so it is a state here and
'adoptTechFlow' says which of the two it is.
-}
data SupplierHome
    = OwnDatabase
    | Dependency (Maybe (TechnosphereFlow, Text))

resolveSupplier :: AuthorContext -> Text -> Either Text Supplier
resolveSupplier ctx provider =
    case mapMaybe inDatabase (zip (OwnDatabase : repeat foreign') (acDb ctx : acDeps ctx)) of
        [] -> Left ("unknown provider \"" <> provider <> "\"")
        (sup : _) -> Right sup
  where
    -- The first database in the list is the one being written, and the rest
    -- are its dependencies; only those oblige a flow to be copied in.
    foreign' :: SupplierHome
    foreign' = Dependency Nothing

    inDatabase :: (SupplierHome, Database) -> Maybe Supplier
    inDatabase (home, db) = do
        pid <- resolveProcess db provider
        key <- dbProcessIdTable db V.!? fromIntegral pid
        act <- dbActivities db V.!? fromIntegral pid
        pure
            Supplier
                { supKey = uncurry ProcessRef key
                , supProducedUnit = refUnitOf db act (\e -> exchangeIsReference e && not (exchangeIsInput e))
                , supAnyRefUnit = refUnitOf db act exchangeIsReference
                , supHome = case home of
                    OwnDatabase -> OwnDatabase
                    Dependency _ -> Dependency (declaredFlow db (snd key))
                }

    declaredFlow :: Database -> UUID -> Maybe (TechnosphereFlow, Text)
    declaredFlow db flowId = do
        flow <- M.lookup flowId (dbTechFlows db)
        pure (flow, unitNameOf (dbUnits db) (tfUnitId flow))

{- | The product flow the edited database has to gain before this exchange can
be linked, if any.

Cross-database relinking reads the *consumer's* flow table
('Database.Loader.findExchangeCrossDBLink' opens on @M.lookup fid techFlowDb@)
and drops in silence what it does not find there, so an exchange must never
reference a product flow only a dependency declares. The biosphere side already
copies for exactly this reason; this is the same rule on the technosphere side.
-}
adoptTechFlow :: AuthorContext -> Supplier -> Either Text (Maybe TechnosphereFlow)
adoptTechFlow ctx sup = case supHome sup of
    OwnDatabase -> Right Nothing
    Dependency Nothing -> Left undeclaredProduct
    Dependency (Just _)
        | M.member (prProduct (supKey sup)) (dbTechFlows (acDb ctx)) -> Right Nothing
        | otherwise -> Just <$> declaredHere ctx sup

{- | The waste flow the edited database has to gain before a waste output to
this provider can be written, if any.

Every reader of a waste line – the exporters, the activity view – looks its
flow up in the waste table, so the flow has to be there whichever database the
treatment lives in. A treatment read from EcoSpold 2 declares its flow as a
product only, its own database included; the waste copy carries the same id,
as the EcoSpold 2 and openLCA readers already file one flow in both tables.
-}
adoptWasteFlow :: AuthorContext -> Supplier -> Either Text (Maybe WasteFlow)
adoptWasteFlow ctx sup
    | M.member (prProduct (supKey sup)) (dbWasteFlows (acDb ctx)) = Right Nothing
    | otherwise = Just . asWaste <$> declaredHere ctx sup
  where
    asWaste flow =
        WasteFlow
            { wfId = tfId flow
            , wfName = tfName flow
            , wfUnitId = tfUnitId flow
            , wfSynonyms = tfSynonyms flow
            , wfCAS = tfCAS flow
            , wfSubstanceId = tfSubstanceId flow
            }

{- | The provider's product flow, its unit a row of the edited database.

The edited database's own row wins when it has one; a dependency's flow has its
unit remapped into the edited database's table, because the link carries the
flow's unit name and would otherwise read it out of the wrong one.
-}
declaredHere :: AuthorContext -> Supplier -> Either Text TechnosphereFlow
declaredHere ctx sup = case (M.lookup flowId (dbTechFlows (acDb ctx)), supHome sup) of
    (Just local, _) -> Right local
    (Nothing, OwnDatabase) -> Left undeclaredProduct
    (Nothing, Dependency Nothing) -> Left undeclaredProduct
    (Nothing, Dependency (Just (flow, flowUnit))) -> case lookupUnit ctx flowUnit of
        Nothing ->
            Left
                ( "the product \""
                    <> tfName flow
                    <> "\" of this provider is stated in \""
                    <> flowUnit
                    <> "\", a unit this database does not have"
                )
        Just (unitRef, _) -> Right flow{tfUnitId = unitRef}
  where
    flowId = prProduct (supKey sup)

undeclaredProduct :: Text
undeclaredProduct = "the database declaring this provider does not declare its product flow"

{- | Whether two unit names denote the same unit, spelling aside.

Two databases keep two unit tables, so one unit can be written @kg@ in one and
@kilogram@ in the other. What decides is the factor between them, not the
string.
-}
sameUnit :: UnitConfig -> Text -> Text -> Bool
sameUnit cfg stated other =
    unitKey cfg stated == unitKey cfg other
        || maybe False (\factor -> abs (factor - 1) < 1e-12) (convertUnit cfg stated other 1)

{- | Judge the unit an exchange to a dependency's supplier is stated in.

A link into a dependency carries the *flow's* unit, not the exchange's
('Database.Loader.findExchangeCrossDBLink' reads it off the flow), and
'Matrix.depDemandsToVector' then converts the raw amount from that unit. So an
exchange stated in anything else enters the matrix as a number in the wrong
unit – two tonnes of a product recorded in kilograms would be demanded as two
kilograms – where the same exchange to a local supplier is converted. Refuse
and name the unit to restate in, exactly as a biosphere amount is.
-}
dependencyUnitError :: AuthorContext -> Supplier -> Text -> Maybe Text
dependencyUnitError ctx sup stated = case supHome sup of
    OwnDatabase -> Nothing
    Dependency Nothing -> Nothing
    Dependency (Just (_, flowUnit))
        | T.null flowUnit -> Nothing
        | sameUnit (acUnitConfig ctx) stated flowUnit -> Nothing
        | otherwise ->
            Just
                ( "this provider's product is recorded in \""
                    <> flowUnit
                    <> "\" but the exchange states \""
                    <> stated
                    <> "\"; an amount to a supplier in another database is not "
                    <> "converted, so restate it in \""
                    <> flowUnit
                    <> "\""
                )

{- | Resolve a process id string the same two ways the rest of the engine
does: the canonical @activityUUID_productUUID@ pair, or a bare activity UUID
when that activity has a single product.
-}
resolveProcess :: Database -> Text -> Maybe ProcessId
resolveProcess db queryText = case parseProcessRef queryText of
    Just ref -> M.lookup (prActivity ref, prProduct ref) (dbProcessIdLookup db)
    Nothing -> UUID.fromText queryText >>= findProcessIdByActivityUUID db

refUnitOf :: Database -> Activity -> (Exchange -> Bool) -> Text
refUnitOf db act keep = case filter keep (exchanges act) of
    (ex : _) -> getUnitNameForExchange (dbUnits db) ex
    [] -> ""

{- | Every biosphere flow the edited database or a dependency declares under
this name and compartment, with what 'findBioFlow' returns beside each.

Matched on the flow's own name, case and spacing aside, and never on a
synonym: an author writing a name in words means the flow that name belongs
to, while a synonym match would attach the exchange to a flow they never
wrote. A blank compartment matches nothing, as does a flow whose source
recorded no compartment, since neither says where the exchange happens.

Read from 'dbBioFlows' rather than from the name index a loaded database
carries, because that index is a runtime attachment a rebuild drops: replaying
a journal against a database that has lost it would resolve a name differently
than the write that recorded it did.
-}
findBioFlowsByName :: AuthorContext -> Text -> Compartment -> [(BiosphereFlow, UnitDB, Bool)]
findBioFlowsByName ctx name comp = case matchesIn True (acDb ctx) of
    [] -> concatMap (matchesIn False) (acDeps ctx)
    found -> found
  where
    wantedName = foldName name
    wantedCompartment = compartmentKey comp
    matchesIn local db =
        [ (flow, dbUnits db, local)
        | flow <- M.elems (dbBioFlows db)
        , foldName (bfName flow) == wantedName
        , Just flowComp <- [bfCompartment flow]
        , compartmentKey flowComp == wantedCompartment
        ]

-- | Names, compartments and sub-compartments compare as written, case and spacing aside.
foldName :: Text -> Text
foldName = T.toLower . T.unwords . T.words

compartmentKey :: Compartment -> (Medium, Text)
compartmentKey comp = (compartmentName comp, foldName (fromMaybe "" (compartmentSub comp)))

renderCompartment :: Compartment -> Text
renderCompartment comp = mediumText (compartmentName comp) <> maybe "" ("/" <>) (compartmentSub comp)

{- | Find a biosphere flow, with the unit table that can name its unit and
whether it lives in the edited database itself.
-}
findBioFlow :: AuthorContext -> UUID -> Maybe (BiosphereFlow, UnitDB, Bool)
findBioFlow ctx flowId =
    case mapMaybe look (zip (True : repeat False) (acDb ctx : acDeps ctx)) of
        [] -> Nothing
        (found : _) -> Just found
  where
    look (local, db) = (,dbUnits db,local) <$> M.lookup flowId (dbBioFlows db)

{- | Resolve a unit the author names to the identifier an exchange carries,
plus the canonical name of that unit. Names and symbols both resolve, so
@kilogram@ and @kg@ reach the same row; a name wins over a symbol when a
database happens to use one string for both.
-}

{- | Why the unit an author named could not be resolved, in the reader's terms.

'lookupUnit' answers 'Nothing' for two different reasons, and calling both
"unknown" hides the one that can be acted on: a spelling the unit table cannot
settle has candidates, and naming them is the whole point of refusing it.
-}
unitRefusal :: AuthorContext -> Text -> Text
unitRefusal ctx stated = case readUnit (acUnitConfig ctx) stated of
    ReadAmbiguous a b rest -> couldBe (a : b : rest)
    ReadExact _ -> unrecorded
    ReadRespelt _ _ -> unrecorded
    ReadUnknown -> "unknown unit " <> quoted stated
  where
    -- The table knows the unit, so what is missing is a row of this database.
    unrecorded :: Text
    unrecorded = case map snd (unitsMeant ctx stated) of
        names@(_ : _ : _) -> couldBe names
        _ -> "this database records no unit " <> quoted stated <> " nor one equal to it"

    couldBe :: [Text] -> Text
    couldBe names = "unit " <> quoted stated <> " could be " <> T.intercalate " or " (map quoted names)

    quoted :: Text -> Text
    quoted u = "\"" <> u <> "\""

lookupUnit :: AuthorContext -> Text -> Maybe (UUID, Text)
lookupUnit ctx stated = case unitsMeant ctx stated of
    [found] -> Just found
    _ -> Nothing

{- | The units of this database a spelling names: the one filed under that
spelling, or else every one it equals at factor 1.

The second half is what lets an author write @L@ against a database that
records @l@, or @kg⋅km@ against one that records @kgkm@: the table holds each
spelling as a row of its own, so the spellings no longer meet on a key, but
they are one unit and nothing needs converting. Two units of the database
answering that way are two answers, and 'unitRefusal' names them.
-}
unitsMeant :: AuthorContext -> Text -> [(UUID, Text)]
unitsMeant ctx stated =
    maybe (nub [unit | (key, unit) <- M.toList index, sameUnit cfg stated key]) pure (M.lookup (unitKey cfg stated) index)
  where
    cfg :: UnitConfig
    cfg = acUnitConfig ctx

    index :: M.Map Text (UUID, Text)
    index = unitIndex cfg (dbUnits (acDb ctx))

unitIndex :: UnitConfig -> UnitDB -> M.Map Text (UUID, Text)
unitIndex cfg units =
    M.fromList
        [ (unitKey cfg key, (unitId u, unitName u))
        | u <- M.elems units
        , key <- [unitSymbol u, unitName u]
        , not (T.null (T.strip key))
        ]

unitNameOf :: UnitDB -> UUID -> Text
unitNameOf units uid = maybe "" unitName (M.lookup uid units)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

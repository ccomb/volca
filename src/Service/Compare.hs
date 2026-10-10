{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | What changed between two activities, or between two versions of a
database, down to the exchanges.

Both sides are values in memory, so a comparison is a pure function of them.
The rules below are the ones a client comparing two versions would otherwise
write for itself, and each client wrote them a little differently.

* Activities pair in a cascade, each rung seeing only what the rungs before it
  left unpaired: the same process id; then the same activity and product
  names, case and a trailing @ {GEO}@ aside, at the same location; then the
  same reference product at the same location, from the same kind of activity.
  The second rung catches a release that regenerates its identifiers, the
  third an activity renamed around a product that kept its identifier, and the
  kind is what keeps the market for a product apart from its production.
* A key several activities answer to, on either side, pairs none of them. They
  are reported as ambiguous and leave the cascade: a looser rung cannot settle
  what a stricter one found undecidable. A key present on one side only decides
  nothing, and its activities go on to the next rung.
* Lines pair the same way: on the flow identifier and the role, then on the
  flow name, case and geography aside, the compartment and the role. The role
  is part of a line, so a flow moving from input to coproduct is one line
  removed and one added.
* The lines of one flow in one unit are summed. When each side holds a single
  line of the flow, its supplier is compared too: a different process with
  another activity name or location (a supplier renamed in place is the same
  supplier); a flow drawn from several suppliers (two electricity mixes)
  compares its total only, so a supplier swapped among them does not show.
  A flow written in several units
  compares unit by unit when both sides write it in the same ones; otherwise it
  is named and not compared, since no sum of kilograms and grams reads as
  either.
* Amounts are equal within a relative 1e-9, the noise a re-export leaves in
  the last bits. Units compare by name, since one format reads a unit's
  identifier from the file and another mints it from the name.
-}
module Service.Compare (
    Sides (..),
    ProcessIn (..),
    resolveProcess,
    compareActivities,
    compareDatabases,
    limitComparison,
    changesPresent,
    applicableChanges,

    -- * The cascade, for other comparisons
    Cascade (..),
    Rung (..),
    cascadeWith,
    pairOn,
    refusing,
    close,
) where

import Control.Monad (mfilter)
import Data.List (mapAccumL)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V

import API.Types (
    ActivityComparison (..),
    ActivityMatch (..),
    ActivitySummary (..),
    AmbiguousActivities (..),
    ChangeOutcome (..),
    ChangePresence (..),
    ChangedActivity (..),
    ChangesApplied (..),
    ChangesPresence (..),
    ChangesQuery (..),
    DatabaseComparison (..),
    ExchangeChange (..),
    LineChange (..),
    LineMatch (..),
    LineRole (..),
    Quantity (..),
    SummaryChange (..),
    Supplier (..),
    UncomparedLine (..),
    UncomparedReason (..),
    WasteSide (..),
    unresolvedFlowName,
 )
import Database.Author (
    ActivityText (..),
    AuthorContext,
    AuthoredExchange (..),
    EditedActivity (..),
    ExchangeEdit (..),
    ExchangeSelector (..),
    FlowRef (..),
    applyExchangeEdits,
    retextsAuthoredIdentity,
 )
import Service (ReferenceProductInfo (..), ServiceError, TargetRef (..), buildCrossDBLinkMap, crossDBLinkIndex, crossDBLinksOf, mkActivitySummary, referenceProductOf, resolveActivityAndProcessId, resolveTarget)
import Types (
    Activity (..),
    BioDirection,
    Compartment,
    CrossDBLink,
    Database (..),
    DatasetDates,
    Exchange (..),
    FlowKind,
    NativeActivityType (..),
    ProcessId,
    ProcessRef (..),
    ProductIndex (..),
    TechRole (..),
    UUID,
    Unit (..),
    UnitDB,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsReference,
    exchangeUnitId,
    flowKindCompartment,
    flowKindName,
    getActivity,
    getUnitForExchange,
    lookupExchangeFlow,
    processIdToRef,
    processIdToText,
 )

-- | The two things compared, named so that swapping them shows at the call.
data Sides a = Sides
    { baseSide :: a
    , otherSide :: a
    }
    deriving (Functor, Foldable, Traversable)

-- | One process of one database, resolved, with the links it draws from other databases.
data ProcessIn = ProcessIn
    { inDatabase :: Database
    , inProcessId :: ProcessId
    , inActivity :: Activity
    , inLinks :: M.Map UUID CrossDBLink
    }

{- | Read a process to compare. Comparing computes nothing, so an activity the
allocation gate refuses to score still compares as it reads.
-}
resolveProcess :: Database -> Text -> Either ServiceError ProcessIn
resolveProcess db = fmap (\(pid, act) -> ProcessIn db pid act (buildCrossDBLinkMap db pid)) . resolveActivityAndProcessId db

-- ---------------------------------------------------------------------------
-- Two activities
-- ---------------------------------------------------------------------------

compareActivities :: Sides ProcessIn -> ActivityComparison
compareActivities sides =
    ActivityComparison
        { acmpBase = baseSide summaries
        , acmpOther = otherSide summaries
        , acmpSummary =
            summaryChanges summaries
                ++ datesChange (fmap (activityDates . inActivity) sides)
                ++ descriptionChange (fmap (activityDescription . inActivity) sides)
        , acmpExchanges = [change | Differs change <- verdicts]
        , acmpUncompared = [line | Uncompared line <- verdicts]
        }
  where
    summaries :: Sides ActivitySummary
    summaries = fmap summaryOf sides
    verdicts :: [LineVerdict]
    verdicts = compareLines (fmap linesOf sides)

summaryOf :: ProcessIn -> ActivitySummary
summaryOf p = mkActivitySummary (inDatabase p) (inProcessId p) (inActivity p)

summaryChanges :: Sides ActivitySummary -> [SummaryChange]
summaryChanges (Sides b o) =
    concat
        [ differing ActivityNameChanged prsActivityName
        , differing LocationChanged prsLocation
        , differing ProductNameChanged prsProductName
        , [ AllocationChanged (prsAllocationPercent b) (prsAllocationPercent o)
          | not (sameShare (prsAllocationPercent b) (prsAllocationPercent o))
          ]
        ]
  where
    differing :: (Text -> Text -> SummaryChange) -> (ActivitySummary -> Text) -> [SummaryChange]
    differing change field = [change (field b) (field o) | field b /= field o]

{- | The dates the two datasets state, when they differ: the plainest answer to
whether one side is a later version of the other.
-}
datesChange :: Sides DatasetDates -> [SummaryChange]
datesChange (Sides b o) = [DatesChanged b o | b /= o]

descriptionChange :: Sides [Text] -> [SummaryChange]
descriptionChange (Sides b o) = [DescriptionChanged b o | b /= o]

sameShare :: Maybe Double -> Maybe Double -> Bool
sameShare (Just x) (Just y) = close x y
sameShare Nothing Nothing = True
sameShare (Just _) Nothing = False
sameShare Nothing (Just _) = False

-- | Equal within the noise a re-export leaves in the last bits of a float.
close :: Double -> Double -> Bool
close x y = x == y || abs (x - y) <= 1e-9 * max (abs x) (abs y)

-- ---------------------------------------------------------------------------
-- Lines
-- ---------------------------------------------------------------------------

-- | Every line of one flow in one role on one side, its amounts summed per unit.
data LineGroup = LineGroup
    { lgFlowId :: UUID
    , lgFlowName :: Text
    , lgNameKey :: Maybe Text -- Nothing when the flow resolves nowhere: there is no name to find it by
    , lgCompartment :: Maybe Compartment
    , lgRole :: LineRole
    , lgAmounts :: M.Map Text Double -- unit name to summed amount, never empty
    , lgSuppliers :: [Maybe TargetRef] -- one per line; Nothing for a line with no supplier, or one that resolves nowhere
    }

data LineKey
    = ByFlow UUID LineRole
    | ByFlowName Text (Maybe Compartment) LineRole
    deriving (Eq, Ord)

data LineVerdict
    = Same
    | Differs ExchangeChange
    | Uncompared UncomparedLine

linesOf :: ProcessIn -> [LineGroup]
linesOf p =
    M.elems $
        M.fromListWith
            merge
            [((lgFlowId line, lgRole line), line) | line <- map (lineOf (inDatabase p) (inLinks p)) (exchanges (inActivity p))]
  where
    merge :: LineGroup -> LineGroup -> LineGroup
    merge later earlier =
        earlier
            { lgAmounts = M.unionWith (+) (lgAmounts earlier) (lgAmounts later)
            , lgSuppliers = lgSuppliers earlier ++ lgSuppliers later
            }

lineOf :: Database -> M.Map UUID CrossDBLink -> Exchange -> LineGroup
lineOf db links ex =
    LineGroup
        { lgFlowId = exchangeFlowId ex
        , lgFlowName = maybe (unresolvedFlowName (exchangeFlowId ex)) flowKindName flow
        , lgNameKey = normalName . flowKindName <$> flow
        , lgCompartment = flowKindCompartment =<< flow
        , lgRole = roleOf ex
        , lgAmounts = M.singleton (unitNameOf (dbUnits db) ex) (exchangeAmount ex)
        , lgSuppliers = [resolveTarget db links ex]
        }
  where
    flow :: Maybe FlowKind
    flow = lookupExchangeFlow db ex

{- | A unit by its name, since one format reads a unit's identifier from the
file and another mints it from the name. A unit the registry lacks has no name
to compare by: its identifier stands in, visibly, so it neither passes for a
real unit nor matches an unresolved unit of another identifier.
-}
unitNameOf :: UnitDB -> Exchange -> Text
unitNameOf units ex =
    maybe ("<unresolved unit " <> UUID.toText (exchangeUnitId ex) <> ">") unitName (getUnitForExchange units ex)

roleOf :: Exchange -> LineRole
roleOf TechnosphereExchange{techRole = role} = TechLine role
roleOf BiosphereExchange{bioDirection = direction} = BioLine direction
roleOf WasteExchange{waIsInput = consumed} = WasteLine (if consumed then WasteInput else WasteOutput)

lineKey :: LineMatch -> LineGroup -> Maybe LineKey
lineKey SameFlow line = Just (ByFlow (lgFlowId line) (lgRole line))
lineKey SameFlowName line = (\name -> ByFlowName name (lgCompartment line) (lgRole line)) <$> lgNameKey line

compareLines :: Sides [LineGroup] -> [LineVerdict]
compareLines sides =
    map (severalFlows . snd) (cAmbiguous paired)
        ++ L.sortOn
            verdictOrder
            ( concat [judgePair match pair | (match, pair) <- cPairs paired]
                ++ map judgeRemoved (baseSide (cUnpaired paired))
                ++ map judgeAdded (otherSide (cUnpaired paired))
            )
  where
    paired :: Cascade LineMatch LineGroup
    paired = cascade lineKey [SameFlow, SameFlowName] sides

{- | Two sides that write a flow in the same units, each amount close, say the
same thing however many units that is. Otherwise the change is stated when each
side holds one unit, and named as not compared when a side holds several. The
supplier is judged apart, so a line can change both.
-}
judgePair :: LineMatch -> Sides LineGroup -> [LineVerdict]
judgePair match pair = amountVerdict : supplierVerdict
  where
    amountVerdict
        | sameAmounts (lgAmounts (baseSide pair)) (lgAmounts (otherSide pair)) = Same
        | otherwise =
            maybe
                (Uncompared (uncomparedOn (baseSide pair) (mixedUnits (fmap unitsOf pair))))
                (Differs . changeOn (baseSide pair) . uncurry (LineChanged match))
                ((,) <$> singleUnit (baseSide pair) <*> singleUnit (otherSide pair))
    -- The same process, renamed, is the same supplier: a copy keeps the
    -- process ids of its source, so renaming a supplier there changes none of
    -- the activities that buy from it. Two releases that re-number their
    -- processes are told apart by name and location alone.
    supplierVerdict = case fmap soleTarget pair of
        Sides (Just before) (Just after)
            | trProcessId before /= trProcessId after
            , supplierOf before /= supplierOf after ->
                [Differs (changeOn (baseSide pair) (SupplierChanged (supplierOf before) (supplierOf after)))]
        _ -> []

{- | The supplier of a flow drawn from one line. A line whose supplier resolves
nowhere has none to compare: a broken link is not a new supplier.
-}
soleTarget :: LineGroup -> Maybe TargetRef
soleTarget line = case lgSuppliers line of
    [target] -> target
    _ -> Nothing

-- | A supplier as a change names it.
supplierOf :: TargetRef -> Supplier
supplierOf target = Supplier (trName target) (trLocation target)

sameAmounts :: M.Map Text Double -> M.Map Text Double -> Bool
sameAmounts base other = M.keys base == M.keys other && and (M.intersectionWith close base other)

judgeRemoved :: LineGroup -> LineVerdict
judgeRemoved line =
    maybe
        (Uncompared (uncomparedOn line (mixedUnits (Sides (unitsOf line) []))))
        (Differs . changeOn line . LineRemoved)
        (singleUnit line)

judgeAdded :: LineGroup -> LineVerdict
judgeAdded line =
    maybe
        (Uncompared (uncomparedOn line (mixedUnits (Sides [] (unitsOf line)))))
        (Differs . changeOn line . LineAdded)
        (singleUnit line)

severalFlows :: Sides (NonEmpty LineGroup) -> LineVerdict
severalFlows groups =
    Uncompared $
        uncomparedOn
            (NE.head (baseSide groups))
            (SeveralFlows (flowIds (baseSide groups)) (flowIds (otherSide groups)))
  where
    flowIds :: NonEmpty LineGroup -> [UUID]
    flowIds = map lgFlowId . NE.toList

mixedUnits :: Sides [Text] -> UncomparedReason
mixedUnits units = MixedUnits (baseSide units) (otherSide units)

unitsOf :: LineGroup -> [Text]
unitsOf = M.keys . lgAmounts

singleUnit :: LineGroup -> Maybe Quantity
singleUnit line = case M.toList (lgAmounts line) of
    [(unit, amount)] -> Just Quantity{qtyAmount = amount, qtyUnit = unit}
    [] -> Nothing
    (_ : _ : _) -> Nothing

changeOn :: LineGroup -> LineChange -> ExchangeChange
changeOn line change =
    ExchangeChange
        { ecFlowId = lgFlowId line
        , ecFlowName = lgFlowName line
        , ecCompartment = lgCompartment line
        , ecRole = lgRole line
        , ecChange = change
        }

uncomparedOn :: LineGroup -> UncomparedReason -> UncomparedLine
uncomparedOn line reason =
    UncomparedLine
        { ulFlowName = lgFlowName line
        , ulCompartment = lgCompartment line
        , ulRole = lgRole line
        , ulReason = reason
        }

-- | Changed lines read in flow order; the verdicts that are no change sort anywhere, being dropped.
verdictOrder :: LineVerdict -> Maybe (Text, LineRole)
verdictOrder Same = Nothing
verdictOrder (Differs change) = Just (ecFlowName change, ecRole change)
verdictOrder (Uncompared line) = Just (ulFlowName line, ulRole line)

-- ---------------------------------------------------------------------------
-- Changes looked for in one activity
-- ---------------------------------------------------------------------------

{- | Whether an activity already says what each change says: what a reader
proposed against one version, asked of a later one. A text is judged by
equality, an amount within the same noise a comparison allows.
-}
changesPresent :: ProcessIn -> ChangesQuery -> ChangesPresence
changesPresent p query =
    ChangesPresence
        { cpSummary = map (summaryPresence p) (cqSummary query)
        , cpExchanges = map (exchangePresence (linesOf p)) (cqExchanges query)
        }

summaryPresence :: ProcessIn -> SummaryChange -> ChangePresence
summaryPresence p = \case
    ActivityNameChanged before after -> judged (==) (prsActivityName summary) before after
    LocationChanged before after -> judged (==) (prsLocation summary) before after
    ProductNameChanged before after -> judged (==) (prsProductName summary) before after
    AllocationChanged before after -> judged sameShare (prsAllocationPercent summary) before after
    DatesChanged before after -> judged (==) (activityDates (inActivity p)) before after
    DescriptionChanged before after -> judged (==) (activityDescription (inActivity p)) before after
  where
    summary :: ActivitySummary
    summary = summaryOf p

-- | What the activity says now, against what the change replaced and what it made it say.
judged :: (a -> a -> Bool) -> a -> a -> a -> ChangePresence
judged same now before after
    | same now after = ChangePresent
    | same now before = ChangeAbsent
    | otherwise = ChangeDifferent

exchangePresence :: [LineGroup] -> ExchangeChange -> ChangePresence
exchangePresence groups change = case lineLook groups change of
    LookPresent -> ChangePresent
    LookDifferent -> ChangeDifferent
    LookGone -> ChangeLineGone
    Lacks _ -> ChangeAbsent

-- | What an activity says of a change to one of its lines.
data LineLook = LookPresent | LookDifferent | LookGone | Lacks Absence

-- | What a line change asks of an activity that does not say it yet.
data Absence
    = AddLine Quantity
    | RemoveLine LineGroup
    | SetLine LineGroup Quantity
    | MoveLine LineGroup Supplier

lineLook :: [LineGroup] -> ExchangeChange -> LineLook
lineLook groups change = case (ecChange change, lineFor groups change) of
    (_, Several) -> LookGone
    (LineAdded after, Missing) -> Lacks (AddLine after)
    (LineAdded after, Found line) -> if holds after line then LookPresent else LookDifferent
    (LineRemoved _, Missing) -> LookPresent
    (LineRemoved before, Found line) -> if holds before line then Lacks (RemoveLine line) else LookDifferent
    (LineChanged{}, Missing) -> LookGone
    (LineChanged _ before after, Found line) -> looked (judged sameAmount (singleUnit line) (Just before) (Just after)) (SetLine line after)
    (SupplierChanged _ _, Missing) -> LookGone
    (SupplierChanged before after, Found line) -> looked (judged (==) (supplierOf <$> soleTarget line) (Just before) (Just after)) (MoveLine line after)
  where
    holds :: Quantity -> LineGroup -> Bool
    holds q line = sameAmount (Just q) (singleUnit line)
    looked :: ChangePresence -> Absence -> LineLook
    looked presence absence = case presence of
        ChangePresent -> LookPresent
        ChangeAbsent -> Lacks absence
        ChangeDifferent -> LookDifferent
        ChangeLineGone -> LookGone

-- | One unit on both sides, and amounts close in it; a line in several units holds no one quantity.
sameAmount :: Maybe Quantity -> Maybe Quantity -> Bool
sameAmount (Just a) (Just b) = qtyUnit a == qtyUnit b && close (qtyAmount a) (qtyAmount b)
sameAmount _ _ = False

data Found a = Found a | Missing | Several

{- | The line a change is about, found the way a comparison pairs lines: by
flow and role, else by the flow's name, compartment and role.
-}
lineFor :: [LineGroup] -> ExchangeChange -> Found LineGroup
lineFor groups change = case [line | line <- groups, lgFlowId line == ecFlowId change, lgRole line == ecRole change] of
    [line] -> Found line
    _ -> case [line | line <- groups, lgNameKey line == Just nameKey, lgCompartment line == ecCompartment change, lgRole line == ecRole change] of
        [] -> Missing
        [line] -> Found line
        _ -> Several
  where
    nameKey :: Text
    nameKey = normalName (ecFlowName change)

-- ---------------------------------------------------------------------------
-- Changes applied to one activity
-- ---------------------------------------------------------------------------

{- | What a change asks of an activity that does not say it yet: the edits
that would make it say it, or why no edit can.
-}
data Landing = Lands [ExchangeEdit] | Stays ChangeOutcome

-- | A change of the request, as asked: what is checked again once edits land.
data Asked = AskedSummary SummaryChange | AskedLine ExchangeChange

-- | Where a request stands: the edits accepted, the changes they carry, and the activity they make.
data Accepted = Accepted [ExchangeEdit] [Asked] ProcessIn

{- | The edits that make an activity say each change it does not say yet, and
what came of every change. A change only lands where the activity still says
what it replaced: a value the database changed otherwise is never overwritten.

Each change is tried against the ones accepted before it, so one an edit
refuses is reported with the engine's reason and does not hold back the
others. A change the accepted edits already make is applied without an edit of
its own, and one whose edits would undo an accepted change is refused. The
edits returned are the ones tried together last: committing them changes the
activity exactly as the outcomes say.
-}
applicableChanges :: AuthorContext -> ProcessIn -> ChangesQuery -> (ChangesApplied, [ExchangeEdit])
applicableChanges ctx p query =
    ( ChangesApplied{capSummary = summaryOutcomes, capExchanges = exchangeOutcomes}
    , inEditOrder accepted
    )
  where
    groups :: [LineGroup]
    groups = linesOf p
    asked :: [(Asked, Landing)]
    asked =
        [(AskedSummary change, summaryLanding change (summaryPresence p change)) | change <- cqSummary query]
            <> [(AskedLine change, exchangeLanding p groups (cqExchanges query) change) | change <- cqExchanges query]
    accepted :: [ExchangeEdit]
    outcomes :: [ChangeOutcome]
    (Accepted accepted _ _, outcomes) = mapAccumL try (Accepted [] [] p) asked
    try :: Accepted -> (Asked, Landing) -> (Accepted, ChangeOutcome)
    try sofar@(Accepted edits held now) (change, landing) = case landing of
        Stays outcome -> (sofar, outcome)
        Lands more
            | presentIn now change -> (Accepted edits (change : held) now, OutcomeApplied)
            | Just why <- retextsIdentity (edits <> more) -> refused why
            | otherwise -> case applyExchangeEdits ctx (inEditOrder (edits <> more)) (inActivity p) of
                Left errs -> refused (T.intercalate "; " errs)
                Right edited
                    | any (/= 1) (eaMatched edited) -> refused "Several lines answer to it: an edit would change each of them."
                    | not (all (presentIn after) (change : held)) -> refused "Another change of this request says otherwise."
                    | otherwise -> (Accepted (edits <> more) (change : held) after, OutcomeApplied)
                  where
                    after :: ProcessIn
                    after = p{inActivity = eaActivity edited}
      where
        refused :: Text -> (Accepted, ChangeOutcome)
        refused why = (sofar, OutcomeNotApplicable why)
    -- An activity written here is keyed by its name and location, which an edit must leave alone.
    retextsIdentity :: [ExchangeEdit] -> Maybe Text
    retextsIdentity edits = do
        ref <- processIdToRef (inDatabase p) (inProcessId p)
        retextsAuthoredIdentity (prActivity ref, prProduct ref) (inActivity p) edits
    summaryOutcomes :: [ChangeOutcome]
    exchangeOutcomes :: [ChangeOutcome]
    (summaryOutcomes, exchangeOutcomes) = splitAt (length (cqSummary query)) outcomes

-- | Whether an activity says a change of the request.
presentIn :: ProcessIn -> Asked -> Bool
presentIn p = \case
    AskedSummary change -> summaryPresence p change == ChangePresent
    AskedLine change -> exchangePresence (linesOf p) change == ChangePresent

{- | Texts first, then amounts on the lines as they are, then removals, then
additions: an amount set on a line a supplier change then replaces is
harmless, a line removed before its amount is set is a refusal.
-}
inEditOrder :: [ExchangeEdit] -> [ExchangeEdit]
inEditOrder = L.sortOn rank
  where
    rank :: ExchangeEdit -> Int
    rank = \case
        SetText _ -> 0
        SetAmount _ _ -> 1
        RemoveExchange _ -> 2
        AddExchange _ -> 3

summaryLanding :: SummaryChange -> ChangePresence -> Landing
summaryLanding change = \case
    ChangePresent -> Stays OutcomePresent
    ChangeDifferent -> Stays OutcomeDifferent
    ChangeLineGone -> Stays OutcomeLineGone
    ChangeAbsent -> case change of
        ActivityNameChanged _ after -> Lands [SetText (ActivityName after)]
        LocationChanged _ after -> Lands [SetText (ActivityLocation after)]
        DescriptionChanged _ after -> Lands [SetText (ActivityDescription after)]
        ProductNameChanged _ _ -> notApplicable "An edit does not rename a product."
        AllocationChanged _ _ -> notApplicable "An edit does not change an allocation."
        DatesChanged _ _ -> notApplicable "An edit does not change the dates of an activity."

notApplicable :: Text -> Landing
notApplicable = Stays . OutcomeNotApplicable

exchangeLanding :: ProcessIn -> [LineGroup] -> [ExchangeChange] -> ExchangeChange -> Landing
exchangeLanding p groups query change = case lineLook groups change of
    LookPresent -> Stays OutcomePresent
    LookDifferent -> Stays OutcomeDifferent
    LookGone -> Stays OutcomeLineGone
    Lacks absence -> either notApplicable Lands $ case absence of
        AddLine after -> pure . AddExchange <$> addedLine (inDatabase p) change after
        RemoveLine line -> pure . RemoveExchange <$> selectorOf (inActivity p) line
        SetLine line after -> sameUnit line after *> (pure . (`SetAmount` qtyAmount after) <$> selectorOf (inActivity p) line)
        MoveLine line after -> do
            sel <- selectorOf (inActivity p) line
            quantity <- amountAfter line
            provider <- producerNamed (inDatabase p) (lgFlowId line) after
            added <- linkedLine (ecRole change) provider quantity
            pure [RemoveExchange sel, AddExchange added]
  where
    -- A supplier change on a line whose amount changed too takes the new amount,
    -- as long as the activity still holds the old one.
    amountAfter :: LineGroup -> Either Text Quantity
    amountAfter line = case [after | other <- query, sameLine other, Lacks (SetLine _ after) <- [lineLook groups other]] of
        (after : _) -> after <$ sameUnit line after
        [] -> maybe (Left "The line is in several units.") Right (singleUnit line)
    sameLine :: ExchangeChange -> Bool
    sameLine other = ecFlowId other == ecFlowId change && ecRole other == ecRole change

-- | A change stated in the unit the line has: an edit writes a number, never a unit.
sameUnit :: LineGroup -> Quantity -> Either Text ()
sameUnit line after = case singleUnit line of
    Just now
        | qtyUnit now == qtyUnit after -> Right ()
        | otherwise -> Left ("The change is in " <> qtyUnit after <> " and the line in " <> qtyUnit now <> ".")
    Nothing -> Left "The line is in several units."

{- | How an edit names the one line a group holds. The selector is read from
the line found, not from the change, since the line may have been found by its
flow's name.
-}
selectorOf :: Activity -> LineGroup -> Either Text ExchangeSelector
selectorOf act line = case (lgRole line, lgSuppliers line) of
    (_, _ : _ : _) -> Left "Several lines answer to this flow: an edit would change each of them."
    (BioLine _, _) -> Right (SelectBiosphere (lgFlowId line))
    (TechLine Input, suppliers) -> SelectInput <$> (localSupplier suppliers <* linked)
    (WasteLine WasteOutput, suppliers) -> SelectWaste <$> (localSupplier suppliers <* linked)
    (TechLine role, _) -> Left (unreachable (TechLine role))
    (WasteLine WasteInput, _) -> Left (unreachable (WasteLine WasteInput))
  where
    localSupplier :: [Maybe TargetRef] -> Either Text Text
    localSupplier = \case
        [Just target] -> case T.breakOn "::" (trProcessId target) of
            (pid, "") -> Right pid
            (dbName, _) -> Left ("The supplier lives in " <> dbName <> ": an edit selects a line by a supplier of this database.")
        _ -> Left "The line names no supplier to select it by."
    -- An edit selects a line by the supplier it is linked to; a line the
    -- database supplies by its product alone carries no link to select it by.
    linked :: Either Text ()
    linked
        | all (isJust . linkOf) [ex | ex <- exchanges act, exchangeFlowId ex == lgFlowId line, roleOf ex == lgRole line] = Right ()
        | otherwise = Left "The line is supplied by its product, not linked to a supplier: an edit selects a line by the supplier it is linked to."
    linkOf :: Exchange -> Maybe UUID
    linkOf = \case
        TechnosphereExchange{techActivityLinkId = lid} -> lid
        WasteExchange{waActivityLinkId = lid} -> lid
        BiosphereExchange{} -> Nothing

unreachable :: LineRole -> Text
unreachable = \case
    TechLine ReferenceProduct -> "An edit does not reach the reference product."
    TechLine Coproduct -> "An edit does not reach a coproduct."
    TechLine AvoidedProduct -> "An edit does not reach an avoided product."
    TechLine ReferenceInput -> "An edit does not reach the waste a treatment takes in."
    TechLine Input -> "An edit reaches an input by its supplier."
    BioLine _ -> "An edit reaches a biosphere line by its flow."
    WasteLine WasteInput -> "An edit does not reach the waste a treatment takes in."
    WasteLine WasteOutput -> "An edit reaches a waste output by its treatment."

{- | The line a change adds. A change keeps the flow of an added line, not its
supplier: the one activity of this database that makes the flow supplies it.
-}
addedLine :: Database -> ExchangeChange -> Quantity -> Either Text AuthoredExchange
addedLine db change quantity = case ecRole change of
    BioLine direction -> Right (bioLine (ecFlowId change) direction quantity)
    _ -> case makersOf db (ecFlowId change) of
        [pid] -> linkedLine (ecRole change) (processIdToText db pid) quantity
        [] -> Left "No activity of this database makes this product, so none can supply the added line."
        _ -> Left "Several activities of this database make this product: the change does not say which one supplies the added line."

bioLine :: UUID -> BioDirection -> Quantity -> AuthoredExchange
bioLine flowId direction quantity =
    AuthoredBio{abFlow = FlowById flowId, abDirection = direction, abAmount = qtyAmount quantity, abUnit = Just (qtyUnit quantity), abComment = Nothing}

-- | A line drawn from a supplier, or sent to a treatment, of this database.
linkedLine :: LineRole -> Text -> Quantity -> Either Text AuthoredExchange
linkedLine role provider quantity = case role of
    TechLine Input -> Right AuthoredTechInput{atiProvider = provider, atiAmount = qtyAmount quantity, atiUnit = Just (qtyUnit quantity), atiComment = Nothing}
    WasteLine WasteOutput -> Right AuthoredWasteOutput{awProvider = provider, awAmount = qtyAmount quantity, awUnit = Just (qtyUnit quantity), awComment = Nothing}
    other -> Left (unreachable other)

-- | The activities of this database that make a flow.
makersOf :: Database -> UUID -> [ProcessId]
makersOf db flowId = maybe [] NE.toList (M.lookup flowId (piByUUID (dbProductIndex db)))

-- | The one activity of this database that makes a flow and is named as the supplier.
producerNamed :: Database -> UUID -> Supplier -> Either Text Text
producerNamed db flowId supplier =
    case filter named (makersOf db flowId) of
        [pid] -> Right (processIdToText db pid)
        [] -> Left ("No activity of this database named " <> described <> " makes this product.")
        _ -> Left ("Several activities of this database named " <> described <> " make this product.")
  where
    named :: ProcessId -> Bool
    named pid = maybe False (\act -> Supplier (activityName act) (activityLocation act) == supplier) (getActivity db pid)
    described :: Text
    described = supActivityName supplier <> " {" <> supLocation supplier <> "}"

-- ---------------------------------------------------------------------------
-- Two databases
-- ---------------------------------------------------------------------------

-- | The kind of activity a source says a dataset is.
data ActivityGenre
    = EcoSpoldGenre Int
    | SimaProGenre Text
    | ILCDGenre Text
    deriving (Eq, Ord)

data NamesKey = NamesKey
    { nkActivity :: Text
    , nkLocation :: Text
    , nkProduct :: Text
    }
    deriving (Eq, Ord)

data ProductKey = ProductKey
    { pkFlow :: UUID
    , pkLocation :: Text
    , pkGenre :: Maybe ActivityGenre
    }
    deriving (Eq, Ord)

data ActivityKey
    = ByProcess ProcessRef
    | ByNames NamesKey
    | ByProduct ProductKey
    deriving (Eq, Ord)

compareDatabases :: Sides Database -> DatabaseComparison
compareDatabases dbs =
    DatabaseComparison
        { dbcAddedCount = length added
        , dbcRemovedCount = length removed
        , dbcChangedCount = length changed
        , dbcRedatedCount = length redated
        , dbcAmbiguousCount = length ambiguous
        , dbcUnchangedCount = length (cPairs paired) - length differing
        , dbcAdded = added
        , dbcRemoved = removed
        , dbcChanged = changed
        , dbcRedated = redated
        , dbcAmbiguous = ambiguous
        }
  where
    paired :: Cascade ActivityMatch ProcessIn
    paired = cascade activityKey [minBound .. maxBound] (fmap processesOf dbs)
    redated, changed :: [ChangedActivity]
    (redated, changed) = L.partition (onlyRedated . chaComparison) differing
    differing :: [ChangedActivity]
    differing =
        L.sortOn
            (summaryOrder . acmpOther . chaComparison)
            [ ChangedActivity{chaMatch = match, chaComparison = comparison}
            | (match, pair) <- cPairs paired
            , let comparison = compareActivities pair
            , not (identical comparison)
            ]
    added :: [ActivitySummary]
    added = summariesOf (otherSide (cUnpaired paired))
    removed :: [ActivitySummary]
    removed = summariesOf (baseSide (cUnpaired paired))
    ambiguous :: [AmbiguousActivities]
    ambiguous =
        L.sortOn
            (fmap summaryOrder . listToMaybe . ambBase)
            [ AmbiguousActivities
                { ambMatch = match
                , ambBase = summariesOf (NE.toList (baseSide candidates))
                , ambOther = summariesOf (NE.toList (otherSide candidates))
                }
            | (match, candidates) <- cAmbiguous paired
            ]

-- | Keep the first @n@ entries of each list. The counts still cover them all.
limitComparison :: Int -> DatabaseComparison -> DatabaseComparison
limitComparison n c =
    c
        { dbcAdded = take n (dbcAdded c)
        , dbcRemoved = take n (dbcRemoved c)
        , dbcChanged = take n (dbcChanged c)
        , dbcRedated = take n (dbcRedated c)
        , dbcAmbiguous = take n (dbcAmbiguous c)
        }

processesOf :: Database -> [ProcessIn]
processesOf db = zipWith (\pid act -> ProcessIn db pid act (crossDBLinksOf db index pid)) [0 ..] (V.toList (dbActivities db))
  where
    index :: M.Map UUID (M.Map UUID CrossDBLink)
    index = crossDBLinkIndex db

activityKey :: ActivityMatch -> ProcessIn -> Maybe ActivityKey
activityKey match p = case match of
    SameProcessId -> ByProcess <$> processIdToRef db (inProcessId p)
    SameNames -> byNames <$> productName
    SameProduct ->
        byProduct . exchangeFlowId <$> L.find exchangeIsReference (exchanges act)
  where
    db :: Database
    db = inDatabase p
    act :: Activity
    act = inActivity p
    -- A reference whose flow is unknown has no name to pair on.
    productName :: Maybe Text
    productName = mfilter (not . T.null) (rpName <$> referenceProductOf (dbTechFlows db) (dbUnits db) act)
    byNames :: Text -> ActivityKey
    byNames name =
        ByNames
            NamesKey
                { nkActivity = normalName (activityName act)
                , nkLocation = activityLocation act
                , nkProduct = normalName name
                }
    byProduct :: UUID -> ActivityKey
    byProduct flow =
        ByProduct
            ProductKey
                { pkFlow = flow
                , pkLocation = activityLocation act
                , pkGenre = genreOf <$> activityNativeType act
                }

{- | EcoSpold 2 keeps the activity type without its special type, so a dataset
that became a combined production stays pairable.
-}
genreOf :: NativeActivityType -> ActivityGenre
genreOf EcoSpoldActivityType{eatCode = code} = EcoSpoldGenre code
genreOf SimaProProcessType{sptLabel = label} = SimaProGenre label
genreOf ILCDProcessType{iptLabel = label} = ILCDGenre label

identical :: ActivityComparison -> Bool
identical c = null (acmpSummary c) && null (acmpExchanges c) && null (acmpUncompared c)

-- | Of two activities that differ, whether the dates are all they differ by.
onlyRedated :: ActivityComparison -> Bool
onlyRedated c = all isDates (acmpSummary c) && null (acmpExchanges c) && null (acmpUncompared c)
  where
    isDates :: SummaryChange -> Bool
    isDates DatesChanged{} = True
    isDates ActivityNameChanged{} = False
    isDates LocationChanged{} = False
    isDates ProductNameChanged{} = False
    isDates AllocationChanged{} = False
    isDates DescriptionChanged{} = False

summariesOf :: [ProcessIn] -> [ActivitySummary]
summariesOf = L.sortOn summaryOrder . map summaryOf

summaryOrder :: ActivitySummary -> [Text]
summaryOrder s = [prsActivityName s, prsLocation s, prsProductName s, prsProcessId s]

-- ---------------------------------------------------------------------------
-- The cascade
-- ---------------------------------------------------------------------------

-- | What the rungs of a cascade made of the candidates on each side.
data Cascade m a = Cascade
    { cPairs :: [(m, Sides a)]
    , cAmbiguous :: [(m, Sides (NonEmpty a))]
    , cUnpaired :: Sides [a]
    }

-- | What one rung made of the candidates it was given.
data Rung a = Rung
    { rPairs :: [Sides a]
    , rAmbiguous :: [Sides (NonEmpty a)]
    , rLeft :: Sides [a]
    }

cascade :: (Ord k) => (m -> a -> Maybe k) -> [m] -> Sides [a] -> Cascade m a
cascade keyOf = cascadeWith (pairOn . keyOf)

{- | A cascade whose rungs are any way of pairing what is left, not only a key:
a rung may refuse a pair its key made, and the refused go on to the next rung.
-}
cascadeWith :: (m -> Sides [a] -> Rung a) -> [m] -> Sides [a] -> Cascade m a
cascadeWith rungOf rungs start = L.foldl' (climb rungOf) (Cascade [] [] start) rungs

climb :: forall m a. (m -> Sides [a] -> Rung a) -> Cascade m a -> m -> Cascade m a
climb rungOf acc rung =
    Cascade
        { cPairs = cPairs acc ++ map (rung,) (rPairs step)
        , cAmbiguous = cAmbiguous acc ++ map (rung,) (rAmbiguous step)
        , cUnpaired = rLeft step
        }
  where
    step :: Rung a
    step = rungOf rung (cUnpaired acc)

{- | A rung whose pairs a test may still refuse; the refused go on unpaired.
So does a group the rung found ambiguous when the test refuses every pair it
could make: there was never a choice between its candidates.
-}
refusing :: forall a. (Sides a -> Bool) -> Rung a -> Rung a
refusing refused rung =
    rung
        { rPairs = kept
        , rAmbiguous = undecided
        , rLeft =
            Sides
                { baseSide = map baseSide dropped ++ concatMap (NE.toList . baseSide) settled ++ baseSide (rLeft rung)
                , otherSide = map otherSide dropped ++ concatMap (NE.toList . otherSide) settled ++ otherSide (rLeft rung)
                }
        }
  where
    dropped :: [Sides a]
    kept :: [Sides a]
    (dropped, kept) = L.partition refused (rPairs rung)
    settled :: [Sides (NonEmpty a)]
    undecided :: [Sides (NonEmpty a)]
    (settled, undecided) = L.partition everyPairRefused (rAmbiguous rung)
    everyPairRefused :: Sides (NonEmpty a) -> Bool
    everyPairRefused (Sides bs os) = and [refused (Sides b o) | b <- NE.toList bs, o <- NE.toList os]

pairOn :: forall k a. (Ord k) => (a -> Maybe k) -> Sides [a] -> Rung a
pairOn keyOf sides =
    Rung
        { rPairs = [Sides b o | Sides (b :| []) (o :| []) <- M.elems both]
        , rAmbiguous = filter several (M.elems both)
        , rLeft = fmap (filter (maybe True (`M.notMember` both) . keyOf)) sides
        }
  where
    both :: M.Map k (Sides (NonEmpty a))
    both = M.intersectionWith Sides (indexBy keyOf (baseSide sides)) (indexBy keyOf (otherSide sides))

several :: Sides (NonEmpty a) -> Bool
several (Sides b o) = NE.length b > 1 || NE.length o > 1

indexBy :: (Ord k) => (a -> Maybe k) -> [a] -> M.Map k (NonEmpty a)
indexBy keyOf xs = M.fromListWith (flip (<>)) [(k, x :| []) | x <- xs, Just k <- [keyOf x]]

-- | A name without case and without its trailing @ {GEO}@, which some releases write and some do not.
normalName :: Text -> Text
normalName = T.toCaseFold . stripGeoSuffix

stripGeoSuffix :: Text -> Text
stripGeoSuffix name
    | not (T.null upToBrace)
    , Just code <- T.stripSuffix "}" geography
    , not (T.any (== '}') code) =
        T.dropEnd 2 upToBrace
    | otherwise = name
  where
    upToBrace :: Text
    geography :: Text
    (upToBrace, geography) = T.breakOnEnd " {" name

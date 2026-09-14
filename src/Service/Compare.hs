{-# LANGUAGE DeriveFunctor #-}
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
* The lines of one flow in one unit are summed, which also means a supplier
  swapped at an equal total does not show. One flow written in two units on a
  side is named and not compared: no sum of kilograms and grams reads as
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
) where

import Control.Monad (guard)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V

import API.Types (
    ActivityComparison (..),
    ActivityMatch (..),
    ActivitySummary (..),
    AmbiguousActivities (..),
    ChangedActivity (..),
    DatabaseComparison (..),
    ExchangeChange (..),
    LineChange (..),
    LineMatch (..),
    LineRole (..),
    Quantity (..),
    SummaryChange (..),
    UncomparedLine (..),
    UncomparedReason (..),
    WasteSide (..),
    unresolvedFlowName,
 )
import Service (ServiceError, getReferenceProductInfo, mkActivitySummary, resolveActivityAndProcessId)
import Types (
    Activity (..),
    Compartment,
    Database (..),
    Exchange (..),
    FlowKind,
    NativeActivityType (..),
    ProcessId,
    ProcessRef,
    UUID,
    Unit (..),
    UnitDB,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsReference,
    exchangeUnitId,
    flowKindCompartment,
    flowKindName,
    getUnitForExchange,
    lookupExchangeFlow,
    processIdToRef,
 )

-- | The two things compared, named so that swapping them shows at the call.
data Sides a = Sides
    { baseSide :: a
    , otherSide :: a
    }
    deriving (Functor)

-- | One process of one database, resolved.
data ProcessIn = ProcessIn
    { inDatabase :: Database
    , inProcessId :: ProcessId
    , inActivity :: Activity
    }

{- | Read a process to compare. Comparing computes nothing, so an activity the
allocation gate refuses to score still compares as it reads.
-}
resolveProcess :: Database -> Text -> Either ServiceError ProcessIn
resolveProcess db = fmap (uncurry (ProcessIn db)) . resolveActivityAndProcessId db

-- ---------------------------------------------------------------------------
-- Two activities
-- ---------------------------------------------------------------------------

compareActivities :: Sides ProcessIn -> ActivityComparison
compareActivities sides =
    ActivityComparison
        { acmpBase = baseSide summaries
        , acmpOther = otherSide summaries
        , acmpSummary = summaryChanges summaries
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
            [((lgFlowId line, lgRole line), line) | line <- map (lineOf (inDatabase p)) (exchanges (inActivity p))]
  where
    merge :: LineGroup -> LineGroup -> LineGroup
    merge later earlier = earlier{lgAmounts = M.unionWith (+) (lgAmounts earlier) (lgAmounts later)}

lineOf :: Database -> Exchange -> LineGroup
lineOf db ex =
    LineGroup
        { lgFlowId = exchangeFlowId ex
        , lgFlowName = maybe (unresolvedFlowName (exchangeFlowId ex)) flowKindName flow
        , lgNameKey = normalName . flowKindName <$> flow
        , lgCompartment = flowKindCompartment =<< flow
        , lgRole = roleOf ex
        , lgAmounts = M.singleton (unitNameOf (dbUnits db) ex) (exchangeAmount ex)
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
            ( [judgePair match pair | (match, pair) <- cPairs paired]
                ++ map judgeRemoved (baseSide (cUnpaired paired))
                ++ map judgeAdded (otherSide (cUnpaired paired))
            )
  where
    paired :: Cascade LineMatch LineGroup
    paired = cascade lineKey [SameFlow, SameFlowName] sides

judgePair :: LineMatch -> Sides LineGroup -> LineVerdict
judgePair match pair =
    maybe
        (Uncompared (uncomparedOn (baseSide pair) (mixedUnits (fmap unitsOf pair))))
        judge
        ((,) <$> singleUnit (baseSide pair) <*> singleUnit (otherSide pair))
  where
    judge :: (Quantity, Quantity) -> LineVerdict
    judge (before, after)
        | sameQuantity before after = Same
        | otherwise = Differs (changeOn (baseSide pair) (LineChanged match before after))

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

sameQuantity :: Quantity -> Quantity -> Bool
sameQuantity x y = qtyUnit x == qtyUnit y && close (qtyAmount x) (qtyAmount y)

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
        , dbcAmbiguousCount = length ambiguous
        , dbcUnchangedCount = length (cPairs paired) - length changed
        , dbcAdded = added
        , dbcRemoved = removed
        , dbcChanged = changed
        , dbcAmbiguous = ambiguous
        }
  where
    paired :: Cascade ActivityMatch ProcessIn
    paired = cascade activityKey [minBound .. maxBound] (fmap processesOf dbs)
    changed :: [ChangedActivity]
    changed =
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
        , dbcAmbiguous = take n (dbcAmbiguous c)
        }

processesOf :: Database -> [ProcessIn]
processesOf db = zipWith (ProcessIn db) [0 ..] (V.toList (dbActivities db))

activityKey :: ActivityMatch -> ProcessIn -> Maybe ActivityKey
activityKey match p = case match of
    SameProcessId -> ByProcess <$> processIdToRef db (inProcessId p)
    SameNames ->
        ByNames
            NamesKey
                { nkActivity = normalName (activityName act)
                , nkLocation = activityLocation act
                , nkProduct = normalName productName
                }
            <$ guard (not (T.null productName))
    SameProduct ->
        byProduct . exchangeFlowId <$> L.find exchangeIsReference (exchanges act)
  where
    db :: Database
    db = inDatabase p
    act :: Activity
    act = inActivity p
    productName :: Text
    (productName, _, _) = getReferenceProductInfo (dbTechFlows db) (dbUnits db) act
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
cascade keyOf rungs start = L.foldl' (climb keyOf) (Cascade [] [] start) rungs

climb :: forall k m a. (Ord k) => (m -> a -> Maybe k) -> Cascade m a -> m -> Cascade m a
climb keyOf acc rung =
    Cascade
        { cPairs = cPairs acc ++ map (rung,) (rPairs step)
        , cAmbiguous = cAmbiguous acc ++ map (rung,) (rAmbiguous step)
        , cUnpaired = rLeft step
        }
  where
    step :: Rung a
    step = pairOn (keyOf rung) (cUnpaired acc)

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

{-# LANGUAGE OverloadedStrings #-}

{- | Dataset-soundness checks, for the people who build or repair databases.

A score tells you whether a database computes; it says nothing about whether
the dataset is well formed. These checks look for the structural defects a
score can't reveal: processes without exactly one reference exchange,
coproduct allocation that doesn't sum to 100%, entries duplicated outright,
products two activities both declare,
amounts that aren't finite, missing metadata, stored amounts that disagree
with the formulas documenting them, distinct names that merge under
SimaPro's 80-character truncation, exchanges without the pedigree scores
their database otherwise carries, reference products nothing in the
database consumes, inputs no reference product in the database supplies,
inputs a dataset the source retired supplies,
geography no dataset declared (read off the name
or filled in by the loader instead), land transformation whose "to" and
"from" areas don't balance within an activity, oxygen-demand or
organic-carbon measures reported in a physically impossible order, CAS
numbers whose check digit doesn't confirm them, allocation percentages
outside the 0-100% range, and amounts too small to have been measured.

Every check is a pure scan over a 'SimpleDatabase', so the report is
identical on a staged database (parsed, matrices not built) and on a loaded
one - a maker can read it before committing to a build.
-}
module Database.Quality (
    QualityOffender (..),
    QualityCheck (..),
    QualityReport (..),
    qualityReport,
    qualityChecks,
    allocationTolerance,
) where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, isAlphaNum, isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Semigroup (First (..), Min (..), Sum (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.Allocation (asAllocated, describeRefusal)
import Numeric (showFFloat, showGFloat)

import Types (
    Activity (..),
    BiosphereFlow (..),
    DeclaredShare (..),
    Exchange (..),
    FormulaCheck (..),
    LocationSource (..),
    ProcessRef (..),
    Severity (..),
    SimpleDatabase (..),
    TechRole (..),
    TechnosphereFlow (..),
    Unit (..),
    WasteFlow (..),
    activityDeclaredShares,
    activityIsObsolete,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsReference,
    exchangePedigree,
    exchangeUnitId,
    processRefText,
 )

-- | One finding: the activity it was found on, and what is wrong with it.
data QualityOffender = QualityOffender
    { qoSeverity :: !Severity
    , qoProcessId :: !Text
    {- ^ Canonical @activityUUID_productUUID@ address of the entry the finding
    anchors to, so a consumer can navigate to it. A finding that covers
    several entries (duplicates) names one of them.
    -}
    , qoActivityName :: !Text
    , qoLocation :: !Text
    , qoProductName :: !(Maybe Text)
    -- ^ Reference product, when the check knows which one it means
    , qoDetail :: !Text
    -- ^ Human-readable specifics, e.g. @"3 reference exchanges instead of exactly one"@
    }
    deriving (Show, Eq)

{- | The outcome of one check. Offenders are sorted worst-first and complete -
capping for display happens at the wire boundary, not here.
-}
data QualityCheck = QualityCheck
    { qcApplicable :: !Bool
    {- ^ 'False' when the database carries nothing this check could judge, which
    is not the same as passing. See the allocation check in 'qualityReport'.
    -}
    , qcOffenders :: ![QualityOffender]
    }
    deriving (Show, Eq)

{- | Soundness report of a database: one named field per check. Named fields
rather than a list keyed by codes - the field name /is/ the check's identity,
so neither this module nor its consumers can misspell one.
-}
data QualityReport = QualityReport
    { qrDbName :: !Text
    , qrProcessCount :: !Int
    -- ^ Entries scanned: one per (activity, product) pair
    , qrReferenceProduct :: !QualityCheck
    , qrAllocationSums :: !QualityCheck
    , qrDuplicateActivities :: !QualityCheck
    , qrDuplicateProducts :: !QualityCheck
    , qrSuspiciousAmounts :: !QualityCheck
    , qrMissingMetadata :: !QualityCheck
    , qrUndeclaredGeography :: !QualityCheck
    , qrFormulaConsistency :: !QualityCheck
    , qrTruncatedNameCollisions :: !QualityCheck
    , qrMissingPedigree :: !QualityCheck
    , qrUnconsumedProducts :: !QualityCheck
    , qrUnsuppliedInputs :: !QualityCheck
    , qrObsoleteInputs :: !QualityCheck
    , qrLandTransformationBalance :: !QualityCheck
    , qrOxygenDemandOrder :: !QualityCheck
    , qrInvalidCas :: !QualityCheck
    , qrAllocationOutOfRange :: !QualityCheck
    , qrUnallocated :: !QualityCheck
    , qrUnmeasurableAmounts :: !QualityCheck
    }
    deriving (Show, Eq)

{- | Every check of a report, in report order. For consumers that treat the
checks uniformly (counting findings, rendering a list) without losing the
per-field identity above.
-}
qualityChecks :: QualityReport -> [QualityCheck]
qualityChecks r =
    [ qrReferenceProduct r
    , qrAllocationSums r
    , qrDuplicateActivities r
    , qrDuplicateProducts r
    , qrSuspiciousAmounts r
    , qrMissingMetadata r
    , qrUndeclaredGeography r
    , qrFormulaConsistency r
    , qrTruncatedNameCollisions r
    , qrMissingPedigree r
    , qrUnconsumedProducts r
    , qrUnsuppliedInputs r
    , qrObsoleteInputs r
    , qrLandTransformationBalance r
    , qrOxygenDemandOrder r
    , qrInvalidCas r
    , qrAllocationOutOfRange r
    , qrUnallocated r
    , qrUnmeasurableAmounts r
    ]

{- | Allowed drift when summing coproduct allocation percentages. Sources round
their percentages (33.3 + 33.3 + 33.4), so an exact comparison would flag
correct data; half a point is far below what a dropped or mistyped coproduct
costs.
-}
allocationTolerance :: Double
allocationTolerance = 0.5

{- | SimaPro caps process names at this many characters on import and reuses
the truncated text verbatim, so names that only differ beyond it merge into
one process there.
-}
simaproNameLimit :: Int
simaproNameLimit = 80

{- | Two-decimal rendering for detail texts. The judgement uses the exact
double; only the message is rounded, so a drifting sum reads as @69.90@ rather
than as floating-point dust like @69.89999999999999@.
-}
formatPercent :: Double -> Text
formatPercent x = T.pack (showFFloat (Just 2) x "")

{- | Compact rendering for physical amounts, which span orders of magnitude
(@1.5e-4@ m² to thousands): three significant figures, scientific where it
keeps the number legible.
-}
formatAmount :: Double -> Text
formatAmount x = T.pack (showGFloat (Just 3) x "")

{- | True when @name@ begins with @prefix@ and the prefix ends on a word
boundary - the next character is absent or non-alphanumeric. So @"COD"@ matches
@"COD, Chemical Oxygen Demand"@ but not a longer token that merely starts with
the same letters.
-}
startsWithField :: Text -> Text -> Bool
startsWithField prefix name = case T.stripPrefix prefix name of
    Nothing -> False
    Just rest -> maybe True (not . isAlphaNum . fst) (T.uncons rest)

{- | A CAS registry number is @<digits>-<2 digits>-<check digit>@, where the
check digit confirms the rest: numbering the other digits 1, 2, 3, … from the
right, their weighted sum modulo ten equals it. Leading zeros contribute
nothing to that sum, so this accepts both the zero-padded spelling some formats
emit and the canonical one.
-}
validCas :: Text -> Bool
validCas cas = case T.splitOn "-" cas of
    [body, pair, check] -> case (T.unpack body, T.unpack pair, T.unpack check) of
        (bs@(_ : _), ps@[_, _], [c])
            | all isDigit bs
            , all isDigit ps
            , isDigit c ->
                let ds = map digitToInt (bs <> ps)
                 in sum (zipWith (*) [1 ..] (reverse ds)) `mod` 10 == digitToInt c
        _ -> False
    _ -> False

{- | Allowed drift when comparing two physical sums that a law says should be
equal (land transformation in vs out, oxygen-demand ordering). Sources round
their amounts, so an exact comparison would flag correct data; one percent is
far below what a dropped or mistyped flow costs.
-}
physicalBalanceTolerance :: Double
physicalBalanceTolerance = 0.01

{- | Magnitude below which an exchange amount cannot be a measurement.

A hydrogen atom weighs 1.7e-27 kg, so a mass under 1e-27 is less than one atom;
the same figure in joules sits eight orders below a single visible photon, and a
count of items is quantised at one. Whatever the unit, nothing is measured this
small - an amount under the floor is a residue of computation (an underflow, a
conversion through a zero, an allocation of nothing) wearing the costume of
data. Set far below the smallest real trace amounts in any inventory, so the
check accuses only what no instrument could have produced.
-}
measurableMagnitudeFloor :: Double
measurableMagnitudeFloor = 1e-27

-- | Run every check over a database.
qualityReport :: Text -> SimpleDatabase -> QualityReport
qualityReport dbName db =
    QualityReport
        { qrDbName = dbName
        , qrProcessCount = M.size (sdbActivities db)
        , qrReferenceProduct = QualityCheck True (worstFirst referenceOffenders)
        , qrAllocationSums = QualityCheck allocationApplicable (worstFirst allocationOffenders)
        , qrDuplicateActivities = QualityCheck True (worstFirst duplicateOffenders)
        , qrDuplicateProducts = QualityCheck True (worstFirst duplicateProductOffenders)
        , qrSuspiciousAmounts = QualityCheck True (worstFirst amountOffenders)
        , qrMissingMetadata = QualityCheck True (worstFirst metadataOffenders)
        , qrUndeclaredGeography = QualityCheck True (worstFirst geographyOffenders)
        , qrFormulaConsistency = QualityCheck formulaApplicable (worstFirst formulaOffenders)
        , qrTruncatedNameCollisions = QualityCheck True (worstFirst truncationOffenders)
        , qrMissingPedigree = QualityCheck pedigreeApplicable (worstFirst pedigreeOffenders)
        , qrUnconsumedProducts = QualityCheck True (worstFirst unconsumedOffenders)
        , qrUnsuppliedInputs = QualityCheck True (worstFirst unsuppliedOffenders)
        , qrObsoleteInputs = QualityCheck True (worstFirst obsoleteInputOffenders)
        , qrLandTransformationBalance = QualityCheck landBalanceApplicable (worstFirst landBalanceOffenders)
        , qrOxygenDemandOrder = QualityCheck oxygenApplicable (worstFirst oxygenOffenders)
        , qrInvalidCas = QualityCheck casApplicable (worstFirst casOffenders)
        , qrAllocationOutOfRange = QualityCheck allocationApplicable (worstFirst allocationRangeOffenders)
        , qrUnallocated = QualityCheck True (worstFirst unallocatedOffenders)
        , qrUnmeasurableAmounts = QualityCheck True (worstFirst unmeasurableOffenders)
        }
  where
    entries = M.toList (sdbActivities db)
    acts = map snd entries
    pidText = processRefText . uncurry ProcessRef
    worstFirst = sortOn (\o -> (qoSeverity o, qoActivityName o))
    offender sev key act = QualityOffender sev (pidText key) (activityName act) (activityLocation act)

    -- Names of the flow an exchange points at, whichever registry holds it.
    -- An unresolved id degrades to its UUID rather than to a blank: a finding
    -- naming nothing would be unactionable.
    techOrWasteFlowName fid =
        (tfName <$> M.lookup fid (sdbTechFlows db))
            <|> (wfName <$> M.lookup fid (sdbWasteFlows db))
    bioFlowName fid = bfName <$> M.lookup fid (sdbBioFlows db)
    anyFlowName fid =
        fromMaybe (UUID.toText fid) $
            techOrWasteFlowName fid
                <|> bioFlowName fid
    unitLabel uid = maybe "?" unitName (M.lookup uid (sdbUnits db))

    -- Exactly one reference exchange defines the process: none leaves nothing
    -- to normalize against, several make the entry ambiguous. 'exchangeIsReference'
    -- counts a treatment activity's reference /input/ too, so waste treatment
    -- passes on its own terms.
    referenceOffenders =
        [ offender DangerSev key act Nothing $
            T.pack (show n) <> " reference exchanges instead of exactly one"
        | (key, act) <- entries
        , let n = length (filter exchangeIsReference (exchanges act))
        , n /= 1
        ]

    -- The coproducts of one source block share an activity. Two SimaPro blocks
    -- whose names collide after the format's 80-character truncation, and which
    -- publish no identifier to be told apart by, hash to one activity and merge
    -- here: their percentages then sum to ~200%. That is the same over-grouping
    -- 'Database.MatrixBuild' accepts.
    allocationApplicable = any (any isJust . activityDeclaredShares) acts
    allocationGroups =
        M.fromListWith
            (<>)
            [ (actUUID, [(key, act)])
            | (key@(actUUID, _productUUID), act) <- entries
            ]
    allocationOffenders = concatMap allocationGroupOffenders (M.elems allocationGroups)

    -- A block whose every coproduct carries a percentage is judged on its sum.
    -- A block where only some do is its own defect - the sum means nothing
    -- until the missing percentages are restored, so reporting it as a bad sum
    -- would misdiagnose. A block where none do is simply unallocated: nothing
    -- to judge.
    allocationGroupOffenders group' = case group' of
        [] -> []
        (repKey, representative) : _
            | null carried -> []
            | missing > 0 ->
                [ offender WarningSev repKey representative Nothing $
                    T.pack (show missing)
                        <> " of "
                        <> T.pack (show (length outputs))
                        <> " coproduct(s) carry no allocation percentage"
                ]
            -- NaN needs its own test: any comparison against it is False, so
            -- the tolerance check alone would let it through.
            | isNaN total || isInfinite total || abs (total - 100) > allocationTolerance ->
                [ offender DangerSev repKey representative Nothing $
                    "allocation sums to "
                        <> formatPercent total
                        <> "% across "
                        <> T.pack (show (length outputs))
                        <> " coproduct(s)"
                ]
            | otherwise -> []
      where
        outputs = map (fmap dsPercent) (concatMap (activityDeclaredShares . snd) group')
        carried = catMaybes outputs
        missing = length outputs - length carried
        total = sum carried

    -- A single allocation factor outside 0–100% is wrong on its own terms: a
    -- coproduct cannot take a negative share or more than the whole. Distinct
    -- from the sums check, which judges the block total - a factor can be out
    -- of range while its block still happens to sum to 100. NaN is left to the
    -- sums check, which already reports it as a bad total.
    allocationRangeOffenders =
        [ offender WarningSev key act Nothing $
            "allocation percentage is " <> formatPercent pct <> "%, outside the 0-100% range"
        | (key, act) <- entries
        , Just pct <- map (fmap dsPercent) (activityDeclaredShares act)
        , pct < 0 || pct > 100
        ]

    -- What the matrix refuses to hold, and why: the verdict of the allocation
    -- gate, the same one that refuses to score the entry. It loads and can be
    -- read; its column is empty. The detail says what would repair it.
    unallocatedOffenders =
        [ offender DangerSev key act Nothing (describeRefusal refusal)
        | (key, act) <- entries
        , Left refusal <- [asAllocated act]
        ]

    -- Same name, same place, same product, twice: one of them is stale. Entries
    -- without exactly one reference are skipped - check 1 already reports them,
    -- and grouping them by a missing product would invent duplicates.
    referenceProductName act = case filter exchangeIsReference (exchanges act) of
        [ex] -> Just (fromMaybe (UUID.toText (exchangeFlowId ex)) (techOrWasteFlowName (exchangeFlowId ex)))
        _ -> Nothing
    duplicateGroups =
        M.fromListWith
            (<>)
            [ ((activityName act, activityLocation act, productName), (Min (pidText key), Sum (1 :: Int)))
            | (key, act) <- entries
            , Just productName <- [referenceProductName act]
            ]
    duplicateOffenders =
        [ QualityOffender WarningSev pid name location (Just productName) $
            T.pack (show n) <> " identical entries (same name, location and reference product)"
        | ((name, location, productName), (Min pid, Sum n)) <- M.toList duplicateGroups
        , n > 1
        ]

    productProducers =
        M.fromListWith
            (<>)
            [ (exchangeFlowId refEx, [(key, act)])
            | (key, act) <- entries
            , [refEx] <- [filter exchangeIsReference (exchanges act)]
            ]

    {- Two activities declaring one product at one location, which an input
    naming that product has to choose between. The supplier index is keyed by
    the product, so one of them answers and the others supply nothing. Which
    one is settled by a rule of ours, on a question only the file can answer,
    and a stale twin left in an export wins as easily as the current entry.
    Reported per activity, each naming the others, so the maker can see both
    ends of the collision.

    The location belongs in the key because making one product in several
    places is how a database is meant to be written: ecoinvent carries hundreds
    of activities producing "electricity, high voltage", one per geography, and
    an input names the one it means. Two of them at one location is what no
    file states a difference between.
    -}
    coLocatedProducers =
        M.fromListWith
            (<>)
            [ ((exchangeFlowId refEx, activityLocation act), [(key, act)])
            | (key, act) <- entries
            , [refEx] <- [filter exchangeIsReference (exchanges act)]
            ]
    otherProducerNames as = case S.toAscList (S.fromList (map activityName as)) of
        names ->
            T.intercalate ", " (map (\n -> "\"" <> n <> "\"") (take 3 names))
                <> if length names > 3
                    then " and " <> T.pack (show (length names - 3)) <> " more"
                    else ""
    duplicateProductOffenders =
        [ offender WarningSev key act (Just (anyFlowName fid)) $
            "this product is also the reference product of " <> otherProducerNames others <> " at the same location; an input naming it is answered by one of them"
        | ((fid, _), group) <- M.toList coLocatedProducers
        , (key, act) <- group
        , let others = [a | (k, a) <- group, k /= key]
        , not (null others)
        ]

    -- A non-finite amount poisons every downstream sum; a zero reference amount
    -- is what normalization divides by. Zero on an ordinary input is legal and
    -- says the activity simply doesn't consume it.
    amountOffenders =
        [ offender DangerSev key act Nothing detail
        | (key, act) <- entries
        , ex <- exchanges act
        , let amount = exchangeAmount ex
        , let flowName = anyFlowName (exchangeFlowId ex)
        , Just detail <-
            [ if isNaN amount || isInfinite amount
                then Just ("exchange \"" <> flowName <> "\" has a non-finite amount")
                else
                    if exchangeIsReference ex && amount == 0
                        then Just ("reference exchange \"" <> flowName <> "\" has amount 0, which normalization would divide by")
                        else Nothing
            ]
        ]

    -- Amounts too small to have been measured (see 'measurableMagnitudeFloor').
    -- Not a rounding complaint: at this magnitude the value is a computational
    -- residue that reads as data, and it survives every copy of the dataset
    -- until someone looks. An ordinary exchange this small distorts nothing in
    -- a score - it just isn't true - hence Warning. A reference exchange is
    -- what normalization divides by: dividing by ~1e-37 scales every other
    -- amount in the process by its reciprocal (a negative one flips their
    -- signs too), the near-zero cousin of the zero-reference case above,
    -- hence Danger.
    unmeasurableOffenders =
        [ offender sev key act Nothing $
            role
                <> " \""
                <> anyFlowName (exchangeFlowId ex)
                <> "\" carries "
                <> formatAmount (exchangeAmount ex)
                <> " "
                <> unitLabel (exchangeUnitId ex)
                <> ", smaller than anything a measurement can yield"
                <> consequence
        | (key, act) <- entries
        , ex <- exchanges act
        , let amount = abs (exchangeAmount ex)
        , amount > 0
        , amount < measurableMagnitudeFloor
        , let (sev, role, consequence)
                | exchangeIsReference ex = (DangerSev, "reference exchange", ", and normalization divides by it")
                | otherwise = (WarningSev, "exchange", "")
        ]

    -- The parse-time mathematicalRelation check (EcoSpold2): formulas that
    -- re-evaluate away from the amount they document. Expected in system-model
    -- exports - allocation rescales amounts without updating the copied
    -- formulas - hence Info: the stored amounts stay authoritative, this only
    -- tells a maker where their own formulas and amounts drifted apart.
    -- Datasets whose formulas merely could not be evaluated are not findings;
    -- 'False' applicability means no dataset carried a formula at all.
    formulaApplicable = any (isJust . activityFormulaCheck) acts
    formulaOffenders =
        [ offender InfoSev key act Nothing (formulaDetail fc)
        | (key, act) <- entries
        , Just fc <- [activityFormulaCheck act]
        , fcDivergent fc > 0
        ]
    formulaDetail fc =
        T.pack (show (fcDivergent fc))
            <> " of "
            <> T.pack (show (fcEvaluated fc))
            <> " evaluable formula(s) disagree with the stored amount"
            <> maybe "" (\e -> " (e.g. " <> e <> ")") (fcExample fc)
            <> ( if fcUnevaluable fc > 0
                    then "; " <> T.pack (show (fcUnevaluable fc)) <> " more could not be evaluated"
                    else ""
               )

    -- Incomplete rather than wrong, hence Info - except a missing location or an
    -- unknown unit, which change how the entry links and converts.
    metadataOffenders =
        concat
            [ [offender InfoSev key act Nothing "the dataset carries no description" | all (T.null . T.strip) (activityDescription act)]
                <> [offender InfoSev key act Nothing "the dataset carries no classification" | M.null (activityClassification act)]
                <> [offender WarningSev key act Nothing "the dataset carries no location" | T.null (T.strip (activityLocation act))]
                <> [ offender WarningSev key act Nothing $
                        T.pack (show unknownUnits) <> " exchange(s) whose unit is absent from the unit registry"
                   | let unknownUnits = length [() | ex <- exchanges act, M.notMember (exchangeUnitId ex) (sdbUnits db)]
                   , unknownUnits > 0
                   ]
            | (key, act) <- entries
            ]

    -- A geography the source declares is a fact about the dataset; one read off
    -- the dataset name is this loader's reading of a string, and one supplied by
    -- the loader is neither. SimaPro writes "Unspecified" in the Geography field
    -- of entire databases, so their geography survives only in names like
    -- "… {FR}"; an EcoSpold dataset with no geography is filled in with "GLO".
    -- Both are usable and neither was declared, and downstream the two are the
    -- same text - hence Info, and hence a count: a maker reads it before
    -- treating the geography as source data.
    geographyOffenders =
        [ offender InfoSev key act Nothing detail
        | (key, act) <- entries
        , Just detail <- [undeclaredGeography (activityLocationSource act) (activityLocation act)]
        ]
    undeclaredGeography src loc = case src of
        LocationDeclared -> Nothing
        LocationInferredFromName ->
            Just ("geography \"" <> loc <> "\" was read from the dataset name, not declared by the source")
        LocationUnspecified
            | T.null (T.strip loc) -> Just "the source declares no geography"
            | otherwise -> Just ("the source declares no geography; \"" <> loc <> "\" stands in for it")

    -- Names that only differ beyond SimaPro's cap become one name on export -
    -- the over-grouping the allocation check above tolerates at parse time
    -- turns into data loss on the way out. One finding per distinct name, each
    -- anchored to one of its entries, so every colliding name stays navigable.
    truncationGroups =
        M.fromListWith
            (M.unionWith (<>))
            [ (T.take simaproNameLimit name, M.singleton name (Min (pidText key), First act))
            | (key, act) <- entries
            , let name = activityName act
            ]
    truncationOffenders =
        [ QualityOffender WarningSev pid name (activityLocation act) Nothing $
            "shares its first "
                <> T.pack (show simaproNameLimit)
                <> " characters with "
                <> T.pack (show (M.size names - 1))
                <> " other name(s), which a SimaPro export would merge"
        | names <- M.elems truncationGroups
        , M.size names > 1
        , (name, (Min pid, First act)) <- M.toList names
        ]

    -- Pedigree scores travel on the data lines of formats that publish them
    -- (SimaPro today). A database without a single one has nothing to judge -
    -- flagging every exchange of a format that can't carry them would be
    -- noise, not a finding. Reference exchanges are definitional rather than
    -- measured, so they are not counted.
    pedigreeApplicable = any (any (isJust . exchangePedigree) . exchanges) acts
    pedigreeOffenders =
        [ offender InfoSev key act Nothing $
            T.pack (show missing)
                <> " of "
                <> T.pack (show (length dataLines))
                <> " exchange(s) carry no pedigree scores"
        | pedigreeApplicable
        , (key, act) <- entries
        , let dataLines = filter (not . exchangeIsReference) (exchanges act)
        , let missing = length (filter (isNothing . exchangePedigree) dataLines)
        , missing > 0
        ]

    -- A product is in use when some data line takes it in: an ordinary
    -- technosphere input, or a waste line on either side - a producer's
    -- waste output is exactly what exercises a treatment's reference input.
    -- Product lines are production and an avoided product is a substitution, not use;
    -- reference lines define their own entry. Cross-database consumers are
    -- out of sight here, hence "within this database" in the finding.
    consumesFlow ex = case ex of
        TechnosphereExchange{techRole = role} -> role == Input
        BiosphereExchange{} -> False
        WasteExchange{} -> True
    usedFlowIds = S.fromList [exchangeFlowId ex | act <- acts, ex <- exchanges act, consumesFlow ex]
    unconsumedOffenders =
        [ offender InfoSev key act (Just prodName) "the reference product is never consumed within this database (expected for a final product)"
        | (key, act) <- entries
        , [refEx] <- [filter exchangeIsReference (exchanges act)]
        , exchangeFlowId refEx `S.notMember` usedFlowIds
        , let prodName = fromMaybe (UUID.toText (exchangeFlowId refEx)) (techOrWasteFlowName (exchangeFlowId refEx))
        ]

    {- The other direction: an input naming a product no reference product of
    this database supplies. Expected of a foreground database, which draws its
    background from another; a hole in one that is meant to stand alone. Either
    way the engine says so rather than resolving the input to something else,
    which is what a shortened or mistyped product name used to get.
    -}
    suppliedFlowIds =
        S.fromList [exchangeFlowId ex | act <- acts, ex <- exchanges act, exchangeIsReference ex]
    needsSupplier ex = case ex of
        TechnosphereExchange{techRole = role} -> role `elem` [Input, ReferenceInput, AvoidedProduct]
        BiosphereExchange{} -> False
        WasteExchange{} -> False
    unsuppliedOffenders =
        [ offender InfoSev key act (Just (anyFlowName fid)) "no reference product of this database supplies this input; it comes from a database this one depends on, or from nowhere"
        | (key, act) <- entries
        , ex <- exchanges act
        , needsSupplier ex
        , let fid = exchangeFlowId ex
        , fid `S.notMember` suppliedFlowIds
        ]

    {- An input whose every producer is a dataset the source filed as obsolete.
    Such a dataset still carries its exchanges and still computes, so the score
    is a number; it is the superseded number, and its author expects it to be
    replaced. The tool that writes these files raises the same warning when a
    calculation reaches one. A product one obsolete block and one live block
    both declare is not flagged: the live one supplies it.
    -}
    obsoleteProductIds =
        M.keysSet (M.filter (all (activityIsObsolete . snd)) productProducers)
    obsoleteInputOffenders =
        [ offender WarningSev key act (Just (anyFlowName fid)) "this input is supplied only by a dataset its source filed as obsolete, which the source expects to be replaced"
        | (key, act) <- entries
        , ex <- exchanges act
        , needsSupplier ex
        , let fid = exchangeFlowId ex
        , fid `S.member` obsoleteProductIds
        ]

    -- Land transformation is conserved: a parcel changed into one use was
    -- changed out of another, so within an activity the "Transformation, to …"
    -- areas must match the "Transformation, from …" areas. A gap means one side
    -- was dropped or mistyped. Compared per unit - only same-unit areas add -
    -- though in practice these flows are all m². A database with no such flow
    -- has nothing to judge.
    landBalanceApplicable = not (all (M.null . transformationByUnit) acts)
    landBalanceOffenders =
        [ offender WarningSev key act Nothing $
            "land transformation is unbalanced: "
                <> formatAmount fromSum
                <> " "
                <> unit
                <> " transformed from vs "
                <> formatAmount toSum
                <> " "
                <> unit
                <> " transformed to ("
                <> formatPercent (100 * abs (fromSum - toSum) / denom)
                <> "% apart)"
        | (key, act) <- entries
        , (uid, (fromSum, toSum)) <- M.toList (transformationByUnit act)
        , let denom = max fromSum toSum
        , denom > 0
        , abs (fromSum - toSum) > physicalBalanceTolerance * denom
        , let unit = unitLabel uid
        ]
    transformationByUnit act =
        M.fromListWith
            (\(a, b) (c, d) -> (a + c, b + d))
            [ (exchangeUnitId ex, side)
            | ex <- exchanges act
            , Just nm <- [bioFlowName (exchangeFlowId ex)]
            , Just side <- [transformationSide nm (exchangeAmount ex)]
            ]
    transformationSide nm amt
        | startsWithField "Transformation, from" nm = Just (amt, 0)
        | startsWithField "Transformation, to" nm = Just (0, amt)
        | otherwise = Nothing

    -- The biological oxygen demand is a fraction of the chemical one, and
    -- dissolved organic carbon a fraction of the total: BOD5 ≤ COD and
    -- DOC ≤ TOC, always. A reversed pair is a measurement or transcription
    -- error, not a modelling choice. Compared only where both members are
    -- present - a lone measure has nothing to be out of order with. These
    -- flows are reported in kilograms across every format, so the per-activity
    -- sums are directly comparable.
    oxygenApplicable = any (any (maybe False isOxygenName . bioFlowName . exchangeFlowId) . exchanges) acts
    isOxygenName nm = any (`startsWithField` nm) ["BOD5", "COD", "DOC", "TOC"]
    oxygenSum prefix act =
        sum [exchangeAmount ex | ex <- exchanges act, Just nm <- [bioFlowName (exchangeFlowId ex)], startsWithField prefix nm]
    oxygenOffenders =
        [ offender WarningSev key act Nothing detail
        | (key, act) <- entries
        , detail <- oxygenViolations act
        ]
    oxygenViolations act =
        [ "BOD5 (" <> formatAmount bod <> ") exceeds COD (" <> formatAmount cod <> ") in this entry - the biological oxygen demand cannot exceed the chemical"
        | let bod = oxygenSum "BOD5" act
        , let cod = oxygenSum "COD" act
        , bod > 0
        , cod > 0
        , bod - cod > physicalBalanceTolerance * cod
        ]
            <> [ "DOC (" <> formatAmount doc <> ") exceeds TOC (" <> formatAmount toc <> ") in this entry - the dissolved organic carbon cannot exceed the total"
               | let doc = oxygenSum "DOC" act
               , let toc = oxygenSum "TOC" act
               , doc > 0
               , toc > 0
               , doc - toc > physicalBalanceTolerance * toc
               ]

    -- A CAS registry number is self-checking: a corrupt one is silently wrong
    -- and breaks the name→CAS bridge that matches flows across databases. One
    -- finding per distinct flow, anchored to the lowest-addressed entry that
    -- uses it so it stays navigable. Flows the registry lists but no activity
    -- uses are inert and left out - the report scans what activities carry.
    allFlowCas =
        [(tfId f, tfName f, cas) | f <- M.elems (sdbTechFlows db), Just cas <- [tfCAS f]]
            <> [(bfId f, bfName f, cas) | f <- M.elems (sdbBioFlows db), Just cas <- [bfCAS f]]
            <> [(wfId f, wfName f, cas) | f <- M.elems (sdbWasteFlows db), Just cas <- [wfCAS f]]
    casApplicable = not (null allFlowCas)
    flowRep =
        M.fromListWith
            min
            [ (exchangeFlowId ex, (pidText key, activityName act, activityLocation act))
            | (key, act) <- entries
            , ex <- exchanges act
            ]
    casOffenders =
        [ QualityOffender WarningSev pid name loc (Just flowName) $
            "CAS number \"" <> cas <> "\" is not a valid CAS registry number"
        | (fid, flowName, cas) <- allFlowCas
        , not (validCas cas)
        , Just (pid, name, loc) <- [M.lookup fid flowRep]
        ]

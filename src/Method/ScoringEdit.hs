{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Method.ScoringEdit
Description : What a requested change to a scoring set records

A gesture on a scoring set is computed as the set it should leave, and the
line it records is the difference: one change per entry, each with what it
found. A guided row names categories, never the short names a formula reads,
which are made here once and kept.
-}
module Method.ScoringEdit (
    RowDraft (..),
    ScoringEdit (..),
    planScoringEdit,
) where

import Control.Monad (foldM, unless, when)
import Data.List (sort, (\\))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import qualified Expr
import Method.EditPlan (EditEffect (..))
import Method.Journal (MethodOp (..), applyMethodOp, oneCategory)
import Method.Scoring (
    ScoringGesture (..),
    ScoringRow (..),
    counted,
    diffSets,
    freshName,
    rowsOf,
    singleScoreName,
    sumOfRows,
    writeSum,
 )
import Method.Types (Method (..), MethodCollection (..), ScoringSet (..), ScoringSetOrigin (..))

-- | A row of the guided grouping as a caller writes it: categories by their identifier.
data RowDraft = RowDraft
    { rdLabel :: Text
    , rdUnit :: Maybe Text
    , rdTerms :: NonEmpty (UUID, Double)
    , rdNormalization :: Maybe Double
    , rdWeight :: Maybe Double
    }
    deriving (Eq, Show)

-- | A change asked of a collection's scoring sets, each named by its set.
data ScoringEdit
    = -- | name, unit ("Pt" when absent), rows
      NewSet Text (Maybe Text) [RowDraft]
    | DeleteSet Text
    | RenameScoringSet Text Text
    | ChangeSetUnit Text Text
    | ChangeMultiplier Text (Maybe Double)
    | AddRow Text RowDraft
    | -- | the set, the row by its variable, the row as it should be
      ChangeRow Text Text RowDraft
    | DeleteRow Text Text
    | -- | the set, an existing computed variable, its formula
      WriteFormula Text Text Text
    | -- | the set, a score, its formula
      PutScore Text Text Text
    | DeleteScore Text Text
    deriving (Eq, Show)

-- | A row with its categories found by name, its text trimmed.
data Row = Row
    { rLabel :: Text
    , rUnit :: Maybe Text
    , rTerms :: [(Text, Double)]
    , rNormalization :: Maybe Double
    , rWeight :: Maybe Double
    }

{- | The line a change to a scoring set records, or why it cannot be recorded.
The line is applied to the collection before it is returned, so whatever the
replay would refuse is refused now, by the same words.
-}
planScoringEdit :: MethodCollection -> ScoringEdit -> Either Text (MethodOp, EditEffect)
planScoringEdit collection edit = do
    op <- case edit of
        NewSet name unit drafts -> newSet collection name unit drafts
        DeleteSet name -> RemoveScoringSet <$> findSet collection name
        RenameScoringSet name asked -> do
            new <- nonBlank "a scoring set needs a name" asked
            gesture name (RenamedSet name new) (\set -> Right set{ssName = new})
        ChangeSetUnit name asked -> do
            unit <- nonBlank "a scoring set needs a unit" asked
            set <- findSet collection name
            gesture name (SetUnitTo (ssUnit set) unit) (\s -> Right s{ssUnit = unit})
        ChangeMultiplier name multiplier -> do
            set <- findSet collection name
            gesture name (SetMultiplierTo (ssDisplayMultiplier set) multiplier) (\s -> Right s{ssDisplayMultiplier = multiplier})
        AddRow name draft -> do
            row <- resolve collection draft
            gesture name (AddedRow (rLabel row)) (addRow row)
        ChangeRow name variable draft -> do
            row <- resolve collection draft
            gesture name (ChangedRow (rLabel row)) (changeRow variable row)
        DeleteRow name variable -> do
            set <- findSet collection name
            row <- findRow set variable
            gesture name (RemovedRow (srLabel row)) (deleteRow variable)
        WriteFormula name variable asked -> do
            formula <- nonBlank "a formula is needed" asked
            set <- findSet collection name
            unless (M.member variable (ssComputed set)) $
                Left ("the scoring set '" <> name <> "' has no computed variable '" <> variable <> "'; its computed variables are " <> listed (M.keys (ssComputed set)))
            gesture name (WroteFormula (M.findWithDefault variable variable (ssLabels set))) (\s -> Right s{ssComputed = M.insert variable formula (ssComputed s)})
        PutScore name score asked -> do
            scoreName <- nonBlank "a score needs a name" score
            formula <- nonBlank "a score needs a formula" asked
            set <- findSet collection name
            let g = if M.member scoreName (ssScores set) then ChangedScore scoreName else AddedScore scoreName
            gesture name g (\s -> Right s{ssScores = M.insert scoreName formula (ssScores s)})
        DeleteScore name score -> do
            set <- findSet collection name
            unless (M.member score (ssScores set)) $
                Left ("the scoring set '" <> name <> "' has no score '" <> score <> "'; its scores are " <> listed (M.keys (ssScores set)))
            gesture name (RemovedScore score) (\s -> Right s{ssScores = M.delete score (ssScores s)})
    _ <- applyMethodOp collection op
    pure (op, EditEffect 0 Nothing Nothing)
  where
    -- A gesture computed as the set it should leave; one that leaves it as it is would only crowd the history.
    gesture :: Text -> ScoringGesture -> (ScoringSet -> Either Text ScoringSet) -> Either Text MethodOp
    gesture name g wanted = do
        set <- findSet collection name
        new <- wanted set
        maybe (Left ("this leaves the scoring set '" <> name <> "' as it is")) (Right . ChangeScoringSet name g) (NE.nonEmpty (diffSets set new))

{- | A set created with its rows: each row is added as it would be to a set
already there, through the same line, so a new set holds what a set grown
row by row would.
-}
newSet :: MethodCollection -> Text -> Maybe Text -> [RowDraft] -> Either Text MethodOp
newSet collection asked unit drafts = do
    name <- nonBlank "a scoring set needs a name" asked
    when (name `elem` map ssName (mcScoringSets collection)) $
        Left ("a scoring set named '" <> name <> "' is already there")
    u <- maybe (Right "Pt") (nonBlank "a scoring set needs a unit") unit
    rows <- traverse (resolve collection) drafts
    built <- foldM (grow name) collection{mcScoringSets = mcScoringSets collection <> [emptySet name u]} rows
    CreateScoringSet <$> findSet built name
  where
    grow :: Text -> MethodCollection -> Row -> Either Text MethodCollection
    grow name c row = do
        set <- findSet c name
        new <- addRow row set
        maybe (Right c) (applyMethodOp c . ChangeScoringSet name (AddedRow (rLabel row))) (NE.nonEmpty (diffSets set new))

emptySet :: Text -> Text -> ScoringSet
emptySet name unit =
    ScoringSet
        { ssName = name
        , ssUnit = unit
        , ssVariables = M.empty
        , ssComputed = M.empty
        , ssLabels = M.empty
        , ssNormalization = M.empty
        , ssWeighting = M.empty
        , ssScores = M.empty
        , ssDisplayMultiplier = Nothing
        , ssUnits = M.empty
        , ssOrigin = CreatedInJournal
        }

-- | A draft with its categories named and its text trimmed, or why it is not a row.
resolve :: MethodCollection -> RowDraft -> Either Text Row
resolve collection draft = do
    label <- nonBlank "a row needs a label" (rdLabel draft)
    terms <- traverse term (NE.toList (rdTerms draft))
    let names = map fst terms
    unless (length (S.fromList names) == length names) $
        Left ("the row '" <> label <> "' names a category twice")
    mapM_ (divisor ("the normalization of '" <> label <> "'")) (rdNormalization draft)
    mapM_ (finite ("the weight of '" <> label <> "'")) (rdWeight draft)
    pure (Row label (rdUnit draft >>= either (const Nothing) Just . nonBlank "") terms (rdNormalization draft) (rdWeight draft))
  where
    term :: (UUID, Double) -> Either Text (Text, Double)
    term (category, coefficient) = do
        method <- oneCategory collection category
        finite ("the coefficient of " <> methodName method) coefficient
        pure (methodName method, coefficient)

    -- The journal refuses a zero too, but by the variable, a name the caller never wrote.
    divisor :: Text -> Double -> Either Text ()
    divisor what x = do
        finite what x
        when (x == 0) (Left (what <> " is " <> T.pack (show x) <> ", which no score can divide by"))
    finite :: Text -> Double -> Either Text ()
    finite what x = when (isNaN x || isInfinite x) (Left (what <> " is " <> T.pack (show x) <> ", which is not a number a score can use"))

-- | A new row: a computed variable named from its label, grouping its categories.
addRow :: Row -> ScoringSet -> Either Text ScoringSet
addRow row set = do
    labelIsFree set Nothing (rLabel row)
    (bound, terms) <- bind set (rTerms row)
    let variable = freshName (taken bound) (rLabel row)
    rescore set (setRow variable row terms bound)

{- | A row changed. A row that is a category with a weight, and stays that one
category, stays as it is; given other categories it becomes a computed
variable named from its label, which takes its weight, its normalization, its
label and its unit, and the category stays a variable the new one reads.
-}
changeRow :: Text -> Row -> ScoringSet -> Either Text ScoringSet
changeRow variable row set = do
    _ <- findRow set variable
    labelIsFree set (Just variable) (rLabel row)
    changed <- case (M.lookup variable (ssComputed set), M.lookup variable (ssVariables set)) of
        (Just _, _) -> do
            (bound, terms) <- bind set (rTerms row)
            Right (setRow variable row terms bound)
        (Nothing, Just category)
            | rTerms row == [(category, 1)] -> Right (describe variable row (if rLabel row == category then Nothing else Just (rLabel row)) set)
            | otherwise -> do
                (bound, terms) <- bind set (rTerms row)
                let promoted = freshName (taken bound) (rLabel row)
                Right (setRow promoted row terms (clear variable bound))
        (Nothing, Nothing) -> Left ("the scoring set '" <> ssName set <> "' has no variable '" <> variable <> "'")
    rescore set (dropUnread (readBy set variable) changed)

-- | A row taken out, with the simple variables it alone read.
deleteRow :: Text -> ScoringSet -> Either Text ScoringSet
deleteRow variable set = do
    _ <- findRow set variable
    let cleared = (clear variable set){ssComputed = M.delete variable (ssComputed set)}
    rescore set (dropUnread (readBy set variable) cleared)

-- | The simple variables a row reads: those its formula names, or itself.
readBy :: ScoringSet -> Text -> [Text]
readBy set variable = case M.lookup variable (ssComputed set) of
    Just formula -> [v | v <- M.keys (ssVariables set), T.toLower v `elem` identifiers formula]
    Nothing -> [variable]

-- | A computed row written in full: its grouping, label, unit, normalization and weight.
setRow :: Text -> Row -> [(Text, Double)] -> ScoringSet -> ScoringSet
setRow variable row terms set =
    (describe variable row (Just (rLabel row)) set){ssComputed = M.insert variable (writeSum terms) (ssComputed set)}

-- | A row's label, unit, normalization and weight, each written or taken away.
describe :: Text -> Row -> Maybe Text -> ScoringSet -> ScoringSet
describe variable row label set =
    set
        { ssLabels = M.alter (const label) variable (ssLabels set)
        , ssUnits = M.alter (const (rUnit row)) variable (ssUnits set)
        , ssNormalization = M.alter (const (rNormalization row)) variable (ssNormalization set)
        , ssWeighting = M.alter (const (rWeight row)) variable (ssWeighting set)
        }

-- | A variable's label, unit, normalization and weight taken away.
clear :: Text -> ScoringSet -> ScoringSet
clear variable set =
    set
        { ssLabels = M.delete variable (ssLabels set)
        , ssUnits = M.delete variable (ssUnits set)
        , ssNormalization = M.delete variable (ssNormalization set)
        , ssWeighting = M.delete variable (ssWeighting set)
        }

{- | The variable each category is read through: the one the set already has
for it, or a new one named from it. Two variables reading one category leave
two readings, and the row is refused naming both.
-}
bind :: ScoringSet -> [(Text, Double)] -> Either Text (ScoringSet, [(Text, Double)])
bind start = foldM step (start, [])
  where
    step :: (ScoringSet, [(Text, Double)]) -> (Text, Double) -> Either Text (ScoringSet, [(Text, Double)])
    step (set, acc) (category, coefficient) = case [v | (v, c) <- M.toList (ssVariables set), c == category] of
        [v] -> Right (set, acc <> [(v, coefficient)])
        [] ->
            let v = freshName (taken set) category
             in Right (set{ssVariables = M.insert v category (ssVariables set)}, acc <> [(v, coefficient)])
        several -> Left ("the variables " <> listed several <> " of the scoring set '" <> ssName set <> "' all read " <> category <> "; the row cannot tell which one to use")

{- | The simple variables among these that nothing reads any more, taken out:
not a row, and named by no formula. Variables the set held unread before are
left as they were.
-}
dropUnread :: [Text] -> ScoringSet -> ScoringSet
dropUnread candidates set = foldr drop' set [v | v <- candidates, M.member v (ssVariables set), unread v]
  where
    unread :: Text -> Bool
    unread v = not (M.member v (ssWeighting set)) && T.toLower v `notElem` foldMap identifiers (M.elems (ssComputed set) <> M.elems (ssScores set))

    drop' :: Text -> ScoringSet -> ScoringSet
    drop' v s = (clear v s){ssVariables = M.delete v (ssVariables s)}

{- | The scores after the rows changed. A score that was the sum of the rows
is again the sum of the rows, and taken out when no row is left to sum; a set
with no score gets one at its first row a single score counts, as a file's
translation gives it. A score reading a row that stops being one otherwise
than as that sum is refused by name: it would score something else.
-}
rescore :: ScoringSet -> ScoringSet -> Either Text ScoringSet
rescore before after = do
    case [s | (s, formula) <- M.toList (ssScores after), s `notElem` sums, any ((`elem` identifiers formula) . T.toLower) gone] of
        [] -> Right ()
        readers -> Left ("the scores " <> listed readers <> " read a row this changes, otherwise than as the sum of the rows; change them first")
    Right (rewrite after)
  where
    scoredBefore, scoredAfter, gone, sums :: [Text]
    scoredBefore = scored before
    scoredAfter = scored after
    gone = (scoredBefore \\ scoredAfter) <> (variables before \\ variables after)
    sums = filter (sumOfRows before) (M.keys (ssScores before))

    rewrite :: ScoringSet -> ScoringSet
    rewrite set
        | sort scoredBefore == sort scoredAfter = set
        | not (null sums) = set{ssScores = foldr resum (ssScores set) sums}
        | M.null (ssScores before) && null scoredBefore && not (null scoredAfter) = set{ssScores = M.singleton singleScoreName total}
        | otherwise = set

    resum :: Text -> M.Map Text Text -> M.Map Text Text
    resum s
        | null scoredAfter = M.delete s
        | otherwise = M.insert s total

    total :: Text
    total = writeSum [(v, 1) | v <- sort scoredAfter]

    scored :: ScoringSet -> [Text]
    scored set = filter (counted (ssNormalization set) (ssWeighting set)) (map srVariable (rowsOf set))

    variables :: ScoringSet -> [Text]
    variables set = M.keys (ssVariables set) <> M.keys (ssComputed set)

-- | A label no other row of the set carries: the history names rows by their label.
labelIsFree :: ScoringSet -> Maybe Text -> Text -> Either Text ()
labelIsFree set self label =
    when (any (\r -> srLabel r == label && Just (srVariable r) /= self) (rowsOf set)) $
        Left ("the scoring set '" <> ssName set <> "' already has a row labelled '" <> label <> "'")

findRow :: ScoringSet -> Text -> Either Text ScoringRow
findRow set variable = case filter ((== variable) . srVariable) (rowsOf set) of
    row : _ -> Right row
    [] -> Left ("the scoring set '" <> ssName set <> "' has no row '" <> variable <> "'; its rows are " <> listed (map srLabel (rowsOf set)))

findSet :: MethodCollection -> Text -> Either Text ScoringSet
findSet collection name = case filter ((== name) . ssName) (mcScoringSets collection) of
    [set] -> Right set
    [] -> Left ("there is no scoring set named '" <> name <> "'; the collection has " <> listed (map ssName (mcScoringSets collection)))
    _ -> Left ("several scoring sets are named '" <> name <> "'")

-- | Every name a set holds, in lower case, as a formula reads them.
taken :: ScoringSet -> S.Set Text
taken set = S.fromList (map T.toLower (M.keys (ssVariables set) <> M.keys (ssComputed set)))

identifiers :: Text -> [Text]
identifiers = map T.toLower . Expr.collectIdentifiers Expr.Arithmetic

nonBlank :: Text -> Text -> Either Text Text
nonBlank refusal raw
    | T.null (T.strip raw) = Left refusal
    | otherwise = Right (T.strip raw)

listed :: [Text] -> Text
listed [] = "none"
listed names = T.intercalate ", " ["'" <> n <> "'" | n <- names]

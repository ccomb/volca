{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The shape of a scoring set, read and written the same way whatever made
it: a SimaPro file's translation, the configuration, or a journal.
-}
module Method.Scoring (
    TextEntry (..),
    NumberEntry (..),
    ScoringChange (..),
    applyChange,
    diffSets,
    revertChange,
    writeSum,
    textEntryWord,
    numberEntryWord,
    ScoringKey (..),
    keyOf,
    ScoringGesture (..),
    invertGesture,
    checkSet,
    ScoringRow (..),
    RowTerms (..),
    rowsOf,
    sumOfRows,
    singleScoreName,
    shortNames,
    freshName,
    linearTerms,
    counted,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Control.Applicative ((<|>))
import Data.List (mapAccumL, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified Expr
import Method.Types (ScoringSet (..), computeFormulaScores)

-- | The entries of a scoring set that hold text, each keyed by a variable or, for 'ScoreOf', a score.
data TextEntry
    = -- | the impact category a simple variable reads
      CategoryOf
    | -- | the formula of a computed variable
      FormulaOf
    | LabelOf
    | -- | the unit of a computed variable
      VariableUnitOf
    | -- | the formula of a score
      ScoreOf
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The entries of a scoring set that hold a number, keyed by a variable.
data NumberEntry = NormalizationOf | WeightOf
    deriving (Eq, Ord, Show, Enum, Bounded)

{- | One entry of a scoring set changed, with what it held before and what it
holds after; 'Nothing' is an entry absent, so setting and removing are the
same change and undoing one exchanges its two values.
-}
data ScoringChange
    = SetText TextEntry Text (Maybe Text) (Maybe Text)
    | SetNumber NumberEntry Text (Maybe Double) (Maybe Double)
    | RenameSet Text Text
    | SetUnitOfSet Text Text
    | SetDisplayMultiplier (Maybe Double) (Maybe Double)
    deriving (Eq, Show)

{- | Apply one change, provided the set holds what the change says it held
before: a line is replayed on the set it was written against, or refused.
-}
applyChange :: ScoringChange -> ScoringSet -> Either Text ScoringSet
applyChange change set = case change of
    SetText entry key before after -> textEntry entry (changed (textEntryWord entry) quoted key before after)
    SetNumber entry key before after -> numberEntry entry (changed (numberEntryWord entry) tshow key before after)
    RenameSet before after
        | ssName set == before -> Right set{ssName = after}
        | otherwise -> Left (T.concat ["The scoring set is named '", ssName set, "', not '", before, "' as recorded."])
    SetUnitOfSet before after
        | ssUnit set == before -> Right set{ssUnit = after}
        | otherwise -> Left (T.concat ["The scoring set '", ssName set, "' has the unit '", ssUnit set, "', not '", before, "' as recorded."])
    SetDisplayMultiplier before after
        | ssDisplayMultiplier set == before -> Right set{ssDisplayMultiplier = after}
        | otherwise -> Left (mismatch "display multiplier" "" (maybe "none" tshow (ssDisplayMultiplier set)) (maybe "none" tshow before))
  where
    textEntry :: TextEntry -> (M.Map Text Text -> Either Text (M.Map Text Text)) -> Either Text ScoringSet
    textEntry CategoryOf f = (\m -> set{ssVariables = m}) <$> f (ssVariables set)
    textEntry FormulaOf f = (\m -> set{ssComputed = m}) <$> f (ssComputed set)
    textEntry LabelOf f = (\m -> set{ssLabels = m}) <$> f (ssLabels set)
    textEntry VariableUnitOf f = (\m -> set{ssUnits = m}) <$> f (ssUnits set)
    textEntry ScoreOf f = (\m -> set{ssScores = m}) <$> f (ssScores set)

    numberEntry :: NumberEntry -> (M.Map Text Double -> Either Text (M.Map Text Double)) -> Either Text ScoringSet
    numberEntry NormalizationOf f = (\m -> set{ssNormalization = m}) <$> f (ssNormalization set)
    numberEntry WeightOf f = (\m -> set{ssWeighting = m}) <$> f (ssWeighting set)

    changed :: (Eq v) => Text -> (v -> Text) -> Text -> Maybe v -> Maybe v -> M.Map Text v -> Either Text (M.Map Text v)
    changed word shown key before after entries
        | M.lookup key entries == before = Right (M.alter (const after) key entries)
        | otherwise = Left (mismatch word key (maybe "none" shown (M.lookup key entries)) (maybe "none" shown before))

    mismatch :: Text -> Text -> Text -> Text -> Text
    mismatch word key found recorded =
        T.concat ["The scoring set '", ssName set, "' has the ", word, " ", found, forKey, ", not ", recorded, " as recorded."]
      where
        forKey :: Text
        forKey = if T.null key then "" else " for '" <> key <> "'"

    quoted :: Text -> Text
    quoted t = "'" <> t <> "'"

-- | Which entry of a set a change touches, without its values: what a line reaches.
data ScoringKey
    = TextKey TextEntry Text
    | NumberKey NumberEntry Text
    | -- | the name, the unit or the display multiplier, which every entry is read under
      WholeSet
    deriving (Eq, Ord, Show)

keyOf :: ScoringChange -> ScoringKey
keyOf (SetText entry key _ _) = TextKey entry key
keyOf (SetNumber entry key _ _) = NumberKey entry key
keyOf (RenameSet _ _) = WholeSet
keyOf (SetUnitOfSet _ _) = WholeSet
keyOf (SetDisplayMultiplier _ _) = WholeSet

-- | What one line did to a set, as its history says it in a sentence.
data ScoringGesture
    = -- | a row, by its label
      AddedRow Text
    | -- | a row, by its label after the line
      ChangedRow Text
    | RemovedRow Text
    | RenamedSet Text Text
    | SetUnitTo Text Text
    | SetMultiplierTo (Maybe Double) (Maybe Double)
    | -- | the formula of an existing variable, by its label or its name
      WroteFormula Text
    | AddedScore Text
    | ChangedScore Text
    | RemovedScore Text
    deriving (Eq, Show)

-- | What the line undoing a gesture did.
invertGesture :: ScoringGesture -> ScoringGesture
invertGesture (AddedRow label) = RemovedRow label
invertGesture (ChangedRow label) = ChangedRow label
invertGesture (RemovedRow label) = AddedRow label
invertGesture (RenamedSet before after) = RenamedSet after before
invertGesture (SetUnitTo before after) = SetUnitTo after before
invertGesture (SetMultiplierTo before after) = SetMultiplierTo after before
invertGesture (WroteFormula name) = WroteFormula name
invertGesture (AddedScore name) = RemovedScore name
invertGesture (ChangedScore name) = ChangedScore name
invertGesture (RemovedScore name) = AddedScore name

{- | Whether a set still scores: every formula is read with the categories the
set names at one, so a name the set does not have, a formula that cannot be
read and two computed variables that read each other are refused, by what the
computation itself says. Two variables a formula cannot tell apart are refused
too, since one would hide the other. Nothing else: an entry naming no variable
is never read, and a set that loads today must stay editable once copied.
-}
checkSet :: ScoringSet -> Either Text ()
checkSet set = do
    maybe (Right ()) Left (M.foldr (\names found -> found <|> clash names) Nothing byLower)
    either (Left . T.pack) (const (Right ())) (computeFormulaScores set probe)
  where
    probe :: M.Map Text Double
    probe = M.fromList [(category, 1) | category <- M.elems (ssVariables set)]

    byLower :: M.Map Text [Text]
    byLower = M.fromListWith (<>) [(T.toLower v, [v]) | v <- M.keys (ssVariables set) <> M.keys (ssComputed set)]

    clash :: [Text] -> Maybe Text
    clash names = case sort names of
        first : second : _ ->
            Just (T.concat ["The scoring set '", ssName set, "' names two variables '", first, "' and '", second, "', which a formula cannot tell apart."])
        _ -> Nothing

-- | The score a set read from a SimaPro file gives: the sum SimaPro computes.
singleScoreName :: Text
singleScoreName = "Single score"

-- | One row of a set's guided grouping: a computed variable, or a simple one with a weight.
data ScoringRow = ScoringRow
    { srVariable :: Text
    , srLabel :: Text
    -- ^ its label, or the category of a simple variable, or its name
    , srUnit :: Maybe Text
    , srTerms :: RowTerms
    , srNormalization :: Maybe Double
    , srWeight :: Maybe Double
    }
    deriving (Eq, Show)

data RowTerms
    = -- | categories, each times its coefficient
      Grouped [(Text, Double)]
    | -- | a formula that is no weighted sum of categories
      Written Text
    deriving (Eq, Show)

{- | A set as the rows of a guided grouping: every computed variable, weighted
or not, then every simple variable that has a weight, which is a category no
damage groups.
-}
rowsOf :: ScoringSet -> [ScoringRow]
rowsOf set =
    [row v (M.findWithDefault v v (ssLabels set)) (terms formula) | (v, formula) <- M.toList (ssComputed set)]
        <> [ row v (M.findWithDefault category v (ssLabels set)) (Grouped [(category, 1)])
           | (v, category) <- M.toList (ssVariables set)
           , M.member v (ssWeighting set)
           ]
  where
    row :: Text -> Text -> RowTerms -> ScoringRow
    row v label t = ScoringRow v label (M.lookup v (ssUnits set)) t (M.lookup v (ssNormalization set)) (M.lookup v (ssWeighting set))

    terms :: Text -> RowTerms
    terms formula =
        maybe
            (Written formula)
            (\ts -> Grouped [(M.findWithDefault p p (ssVariables set), coef) | (p, coef) <- ts])
            (linearTerms (M.keys (ssVariables set)) formula)

{- | Whether a score is the sum of the rows, each once, of those that enter a
single score ('counted'), in whatever order: the score a new row joins.
-}
sumOfRows :: ScoringSet -> Text -> Bool
sumOfRows set score = case (scored, M.lookup score (ssScores set)) of
    (_ : _, Just formula) -> fmap M.fromList (linearTerms (map srVariable (rowsOf set)) formula) == Just (M.fromList [(v, 1) | v <- scored])
    _ -> False
  where
    scored :: [Text]
    scored = filter (counted (ssNormalization set) (ssWeighting set)) (map srVariable (rowsOf set))

{- | The changes that turn one set into another, one per entry that differs:
how a gesture computed as the set it should leave becomes a journal line. The
entries come first and the name last, so every change reads the set under the
name the line gives it.
-}
diffSets :: ScoringSet -> ScoringSet -> [ScoringChange]
diffSets old new =
    texts CategoryOf ssVariables
        <> texts FormulaOf ssComputed
        <> texts LabelOf ssLabels
        <> texts VariableUnitOf ssUnits
        <> texts ScoreOf ssScores
        <> numbers NormalizationOf ssNormalization
        <> numbers WeightOf ssWeighting
        <> [SetDisplayMultiplier (ssDisplayMultiplier old) (ssDisplayMultiplier new) | ssDisplayMultiplier old /= ssDisplayMultiplier new]
        <> [SetUnitOfSet (ssUnit old) (ssUnit new) | ssUnit old /= ssUnit new]
        <> [RenameSet (ssName old) (ssName new) | ssName old /= ssName new]
  where
    texts :: TextEntry -> (ScoringSet -> M.Map Text Text) -> [ScoringChange]
    texts entry field = [SetText entry k b a | (k, b, a) <- differing (field old) (field new)]

    numbers :: NumberEntry -> (ScoringSet -> M.Map Text Double) -> [ScoringChange]
    numbers entry field = [SetNumber entry k b a | (k, b, a) <- differing (field old) (field new)]

    differing :: (Eq v) => M.Map Text v -> M.Map Text v -> [(Text, Maybe v, Maybe v)]
    differing a b = [(k, M.lookup k a, M.lookup k b) | k <- S.toList (M.keysSet a <> M.keysSet b), M.lookup k a /= M.lookup k b]

{- | The change that takes an entry back to what a change found there, from
what the set holds there now: the undo of a line reads the set as it is, so a
category renamed since does not stop it.
-}
revertChange :: ScoringSet -> ScoringChange -> ScoringChange
revertChange set = \case
    SetText entry key before _ -> SetText entry key (M.lookup key (textField entry)) before
    SetNumber entry key before _ -> SetNumber entry key (M.lookup key (numberField entry)) before
    RenameSet before _ -> RenameSet (ssName set) before
    SetUnitOfSet before _ -> SetUnitOfSet (ssUnit set) before
    SetDisplayMultiplier before _ -> SetDisplayMultiplier (ssDisplayMultiplier set) before
  where
    textField :: TextEntry -> M.Map Text Text
    textField CategoryOf = ssVariables set
    textField FormulaOf = ssComputed set
    textField LabelOf = ssLabels set
    textField VariableUnitOf = ssUnits set
    textField ScoreOf = ssScores set

    numberField :: NumberEntry -> M.Map Text Double
    numberField NormalizationOf = ssNormalization set
    numberField WeightOf = ssWeighting set

{- | A sum of variables times coefficients, as a formula: a coefficient of one
is not written, and a sum of nothing is zero.
-}
writeSum :: [(Text, Double)] -> Text
writeSum [] = "0"
writeSum terms = T.intercalate " + " [if coef == 1 then v else tshow coef <> " * " <> v | (v, coef) <- terms]

-- | How an entry is named in a sentence.
textEntryWord :: TextEntry -> Text
textEntryWord CategoryOf = "category"
textEntryWord FormulaOf = "formula"
textEntryWord LabelOf = "label"
textEntryWord VariableUnitOf = "unit"
textEntryWord ScoreOf = "score formula"

numberEntryWord :: NumberEntry -> Text
numberEntryWord NormalizationOf = "normalization"
numberEntryWord WeightOf = "weight"

{- | Formula identifiers for display names, made once and then stored as they
are: a later renaming of the category does not change them. Lower case only,
since a formula reads its names without regard to case.
-}
shortNames :: [Text] -> [Text]
shortNames = snd . mapAccumL (\taken name -> let free = freshName taken name in (S.insert free taken, free)) S.empty

{- | The identifier for one display name, avoiding the names already taken,
which are given in lower case.
-}
freshName :: S.Set Text -> Text -> Text
freshName taken name = firstOf [c | c <- base : [base <> "_" <> tshow n | n <- [2 :: Int ..]], not (S.member c taken)]
  where
    base :: Text
    base =
        let joined = T.intercalate "_" (filter (not . T.null) (T.split (not . isAsciiAlnum) (T.toLower name)))
         in case T.uncons joined of
                Nothing -> "v"
                Just (c, _) | isDigit c -> "v_" <> joined
                Just _ -> joined

    -- The first of an infinite list: total, unlike 'head'.
    firstOf :: [Text] -> Text
    firstOf = foldr const ""

    isAsciiAlnum :: Char -> Bool
    isAsciiAlnum c = isAsciiLower c || isDigit c

{- | Whether a variable enters SimaPro's single score: it has a weight, and a
normalization as well unless the set normalizes nothing, which is how SimaPro
reads a set with normalization switched off.
-}
counted :: M.Map Text Double -> M.Map Text Double -> Text -> Bool
counted norm weight v = M.member v weight && (M.null norm || M.member v norm)

{- | The terms of a formula that is a sum of the given variables times
constant coefficients, in the order the formula names them; 'Nothing' for
any other formula. Read the way 'scoreWeights' reads a score: one variable
at one and the others at zero gives its coefficient, and the formula must
give zero at zero and the weighted sum at another point.
-}
linearTerms :: [Text] -> Text -> Maybe [(Text, Double)]
linearTerms allowed formula = do
    named <- traverse (`M.lookup` byLower) (nubOrd (map T.toLower (Expr.collectIdentifiers Expr.Arithmetic formula)))
    let zeros = M.fromList [(v, 0) | v <- allowed]
        at env = either (const Nothing) Just (Expr.evaluate Expr.Arithmetic env formula)
        probe = M.fromList (zip allowed [2 :: Double ..])
    atZero <- at zeros
    coefs <- traverse (\v -> at (M.insert v 1 zeros)) named
    atProbe <- at probe
    let expected = sum (zipWith (*) coefs (map (\v -> M.findWithDefault 0 v probe) named))
        tolerance = 1e-9 * (abs atProbe + abs expected)
    if atZero == 0 && abs (atProbe - expected) <= tolerance
        then Just (zip named coefs)
        else Nothing
  where
    -- A formula reads its names without regard to case.
    byLower :: M.Map Text Text
    byLower = M.fromList [(T.toLower v, v) | v <- allowed]

tshow :: (Show a) => a -> Text
tshow = T.pack . show

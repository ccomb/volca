{-# LANGUAGE OverloadedStrings #-}

{- | The shape of a scoring set, read and written the same way whatever made
it: a SimaPro file's translation, the configuration, or a journal.
-}
module Method.Scoring (
    TextEntry (..),
    NumberEntry (..),
    ScoringChange (..),
    applyChange,
    shortNames,
    freshName,
    linearTerms,
    counted,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified Expr
import Method.Types (ScoringSet (..))

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
    SetText entry key before after -> textEntry entry (changed (textWord entry) quoted key before after)
    SetNumber entry key before after -> numberEntry entry (changed (numberWord entry) tshow key before after)
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

-- | How an entry is named in a sentence.
textWord :: TextEntry -> Text
textWord CategoryOf = "category"
textWord FormulaOf = "formula"
textWord LabelOf = "label"
textWord VariableUnitOf = "unit"
textWord ScoreOf = "score formula"

numberWord :: NumberEntry -> Text
numberWord NormalizationOf = "normalization"
numberWord WeightOf = "weight"

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

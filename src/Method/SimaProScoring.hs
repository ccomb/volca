{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- | A SimaPro method file states its single score in its own terms: damage
categories that group impact categories, and normalization-weighting sets
that multiply then weigh each damage. The engine keeps one representation of
a single score, the 'ScoringSet', so a file is read into scoring sets once,
at load, and written back from them at export.
-}
module Method.SimaProScoring (
    DamageCategory (..),
    NormWeightSet (..),
    shortNames,
    singleScoreName,
    damageOnlySetName,
    translateScoring,
    toSimaProBlocks,
) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Expr
import Method.Types (Method (..), ScoringSet (..), ScoringSetOrigin (..))

{- | Damage category: groups impact subcategories into a parent category.
E.g., "Ecotoxicity, freshwater" groups "...part 1", "...part 2", etc.
Each impact maps with a factor (usually 1.0).
-}
data DamageCategory = DamageCategory
    { dcName :: !Text
    -- ^ Damage category name
    , dcUnit :: !Text
    -- ^ Unit (e.g., "CTUe")
    , dcImpacts :: ![(Text, Double)]
    -- ^ [(subcategory name, aggregation factor)]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Normalization and weighting factor set, as SimaPro writes it: the
normalization multiplies a damage, the weight then multiplies the result.
-}
data NormWeightSet = NormWeightSet
    { nwName :: !Text
    -- ^ Set name
    , nwNormalization :: !(M.Map Text Double)
    -- ^ Damage category → normalization factor (multiplier)
    , nwWeighting :: !(M.Map Text Double)
    -- ^ Damage category → weight
    }
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

{- | Formula identifiers for display names, made once and then stored as they
are: a later renaming of the category does not change them. Lower case only,
since a formula reads its names without regard to case.
-}
shortNames :: [Text] -> [Text]
shortNames = go S.empty
  where
    go :: S.Set Text -> [Text] -> [Text]
    go _ [] = []
    go taken (name : rest) =
        let base = slug name
            free = firstOf [c | c <- base : [base <> "_" <> tshow n | n <- [2 :: Int ..]], not (S.member c taken)]
         in free : go (S.insert free taken) rest

    -- The first of an infinite list: total, unlike 'head'.
    firstOf :: [Text] -> Text
    firstOf = foldr const ""

    slug :: Text -> Text
    slug name =
        let joined = T.intercalate "_" (filter (not . T.null) (T.split (not . isAsciiAlnum) (T.toLower name)))
         in case T.uncons joined of
                Nothing -> "v"
                Just (c, _) | isDigit c -> "v_" <> joined
                Just _ -> joined

    isAsciiAlnum :: Char -> Bool
    isAsciiAlnum c = isAsciiLower c || isDigit c

-- | The score a set read from a SimaPro file gives: the sum SimaPro computes.
singleScoreName :: Text
singleScoreName = "Single score"

-- | The set a file with damage categories and no normalization-weighting set gives.
damageOnlySetName :: Text
damageOnlySetName = "Damage assessment"

{- | Read a SimaPro file's damage categories and normalization-weighting sets
as scoring sets, one per normalization-weighting set, so the single score has
one representation whatever file it came from. Works on the merged collection,
since a damage may group categories another file of the collection declares.
Returns the sets and what the load should say about what it could not read.
-}
translateScoring :: [Method] -> [DamageCategory] -> [NormWeightSet] -> ([ScoringSet], [Text])
translateScoring methods damages nwSets
    | null kept && null nwSets = ([], ghostWarnings)
    | otherwise = (map toSet setsToRead, ghostWarnings <> twiceWarnings <> strayWarnings)
  where
    categories :: [Text]
    categories = nubOrd (map methodName methods)

    known :: S.Set Text
    known = S.fromList categories

    kept, ghosts :: [DamageCategory]
    (kept, ghosts) = partition (all ((`S.member` known) . fst) . dcImpacts) damages

    names :: [Text]
    names = shortNames (categories <> map dcName kept)

    catVar :: M.Map Text Text
    catVar = M.fromList (zip categories names)

    damageVars :: [(Text, DamageCategory)]
    damageVars = zip (drop (length categories) names) kept

    grouped :: S.Set Text
    grouped = S.fromList [c | dc <- kept, (c, _) <- dcImpacts dc]

    -- What a normalization-weighting set names: a damage, or a category no damage groups.
    weighable :: [(Text, Text)]
    weighable =
        [(dcName dc, v) | (v, dc) <- damageVars]
            <> [(c, v) | c <- categories, not (S.member c grouped), Just v <- [M.lookup c catVar]]

    setsToRead :: [NormWeightSet]
    setsToRead
        | null nwSets = [NormWeightSet damageOnlySetName M.empty M.empty]
        | otherwise = nwSets

    toSet :: NormWeightSet -> ScoringSet
    toSet nw =
        let norm = M.fromList [(v, 1 / n) | (key, v) <- weighable, Just n <- [M.lookup key (nwNormalization nw)]]
            weight = M.fromList [(v, w) | (key, v) <- weighable, Just w <- [M.lookup key (nwWeighting nw)]]
            -- Sorted by name, not by file order: an export writes the damages
            -- in name order, and reading it back must give the same formula.
            scored = sort [v | (_, v) <- weighable, M.member v norm, M.member v weight]
         in ScoringSet
                { ssName = nwName nw
                , ssUnit = "Pt"
                , ssVariables = M.fromList [(v, c) | (c, v) <- M.toList catVar]
                , ssComputed = M.fromList [(v, grouping dc) | (v, dc) <- damageVars]
                , ssLabels = M.fromList [(v, dcName dc) | (v, dc) <- damageVars]
                , ssNormalization = norm
                , ssWeighting = weight
                , ssScores = if null scored then M.empty else M.singleton singleScoreName (T.intercalate " + " scored)
                , ssDisplayMultiplier = Nothing
                , ssUnits = M.fromList [(v, dcUnit dc) | (v, dc) <- damageVars]
                , ssOrigin = ReadFromSimaProFile
                }

    grouping :: DamageCategory -> Text
    grouping dc = T.intercalate " + " [term coef c | (c, coef) <- dcImpacts dc]

    term :: Double -> Text -> Text
    term coef c =
        let v = M.findWithDefault c c catVar
         in if coef == 1 then v else tshow coef <> " * " <> v

    ghostWarnings, twiceWarnings, strayWarnings :: [Text]
    ghostWarnings =
        [ "Damage category '" <> dcName dc <> "' groups '" <> c <> "', which no impact category of this collection carries; it is left out of the scoring sets."
        | dc <- ghosts
        , (c, _) <- take 1 (filter (not . (`S.member` known) . fst) (dcImpacts dc))
        ]
    twiceWarnings =
        [ "Impact category '" <> c <> "' is grouped by two damage categories (" <> T.intercalate ", " owners <> "); both read it, and the fields kept until 0.16.0 stay empty for it."
        | c <- categories
        , let owners = [dcName dc | dc <- kept, any ((== c) . fst) (dcImpacts dc)]
        , length owners > 1
        ]
    strayWarnings =
        [ "Normalization-weighting set '" <> nwName nw <> "' names '" <> key <> "', which is neither a damage category nor an ungrouped impact category; it is not read."
        | nw <- nwSets
        , key <- nubOrd (M.keys (nwNormalization nw) <> M.keys (nwWeighting nw))
        , key `notElem` map fst weighable
        ]

{- | Write scoring sets back as a SimaPro file's damage categories and
normalization-weighting sets, for the sets the format can hold: a grouping of
impact categories times coefficients, and one score summing the damages that
have both a normalization and a weight. A file has one set of damage
categories, so a set grouping otherwise than the first one written is left
out too. Every set left out is named, with why, in the warnings.
-}
toSimaProBlocks :: [ScoringSet] -> ([DamageCategory], [NormWeightSet], [Text])
toSimaProBlocks = go Nothing [] []
  where
    go :: Maybe (Text, [DamageCategory]) -> [NormWeightSet] -> [Text] -> [ScoringSet] -> ([DamageCategory], [NormWeightSet], [Text])
    go first nws warnings [] = (maybe [] snd first, reverse nws, reverse warnings)
    go first nws warnings (set : rest) = case (blocksOf set, first) of
        (Left reason, _) -> go first nws (refusal set reason : warnings) rest
        (Right (ds, nw), Nothing) -> go (Just (ssName set, ds)) (maybe nws (: nws) nw) warnings rest
        (Right (ds, nw), Just (firstName, firstDs))
            | ds == firstDs -> go first (maybe nws (: nws) nw) warnings rest
            | otherwise ->
                go first nws (refusal set ("a SimaPro file has one set of damage categories, and this set groups the impact categories otherwise than '" <> firstName <> "'.") : warnings) rest

    refusal :: ScoringSet -> Text -> Text
    refusal set reason = "Scoring set '" <> ssName set <> "' is not exported: " <> reason

{- | One set as damage categories and, when it weighs anything, a
normalization-weighting set; or why SimaPro cannot hold it.
-}
blocksOf :: ScoringSet -> Either Text ([DamageCategory], Maybe NormWeightSet)
blocksOf set = do
    unless (maybe True (== 1) (ssDisplayMultiplier set)) (Left "SimaPro has no place for a display multiplier.")
    groupings <- M.traverseWithKey grouping (ssComputed set)
    let grouped = S.fromList [p | terms <- M.elems groupings, (p, _) <- terms]
        weighable = M.keys (ssComputed set) <> filter (not . (`S.member` grouped)) (M.keys (ssVariables set))
        weighed = [v | v <- weighable, M.member v (ssNormalization set), M.member v (ssWeighting set)]
    unless (all (`elem` weighable) (M.keys (ssNormalization set) <> M.keys (ssWeighting set))) $
        Left "SimaPro weighs damage categories and ungrouped impact categories only."
    unless (scoresAreTheSum weighable weighed) $
        Left "SimaPro writes one score, the sum of the damages that have both a normalization and a weight."
    pure (M.elems (M.mapWithKey damage groupings), weighing)
  where
    grouping :: Text -> Text -> Either Text [(Text, Double)]
    grouping var formula =
        maybe (Left ("damage '" <> labelOf var <> "' is not a sum of impact categories times coefficients.")) Right $
            linearTerms (M.keys (ssVariables set)) formula

    damage :: Text -> [(Text, Double)] -> DamageCategory
    damage var terms =
        DamageCategory
            (labelOf var)
            (M.findWithDefault "" var (ssUnits set))
            [(category, coef) | (p, coef) <- terms, Just category <- [M.lookup p (ssVariables set)]]

    scoresAreTheSum :: [Text] -> [Text] -> Bool
    scoresAreTheSum weighable weighed = case M.elems (ssScores set) of
        [] -> True
        [formula] -> maybe False ((== M.fromList [(v, 1) | v <- weighed]) . M.fromList) (linearTerms weighable formula)
        _ -> False

    weighing :: Maybe NormWeightSet
    weighing
        | M.null (ssNormalization set) && M.null (ssWeighting set) = Nothing
        | otherwise =
            Just
                ( NormWeightSet
                    (ssName set)
                    (M.fromList [(keyOf v, 1 / n) | (v, n) <- M.toList (ssNormalization set)])
                    (M.fromList [(keyOf v, w) | (v, w) <- M.toList (ssWeighting set)])
                )

    -- A damage is named by its label, an ungrouped category by the category.
    keyOf :: Text -> Text
    keyOf var
        | M.member var (ssComputed set) = labelOf var
        | otherwise = M.findWithDefault var var (ssVariables set)

    labelOf :: Text -> Text
    labelOf var = M.findWithDefault var var (ssLabels set)

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

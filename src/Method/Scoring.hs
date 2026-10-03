{-# LANGUAGE OverloadedStrings #-}

{- | The shape of a scoring set, read and written the same way whatever made
it: a SimaPro file's translation, the configuration, or a journal.
-}
module Method.Scoring (
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

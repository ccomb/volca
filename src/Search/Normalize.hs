{- | Text normalization for search: accent-stripping, lowercase, and
tokenization. Shared by the BM25 index builder and the query path so
the same transformation applies to both sides.
-}
module Search.Normalize (
    normalize,
    tokenize,
    caseInsensitiveInfixOf,
) where

import Data.Char (GeneralCategory (NonSpacingMark), generalCategory, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN

normalize :: Text -> Text
normalize =
    T.map replacePunct
        . T.filter (not . isCombining)
        . TN.normalize TN.NFD
        . T.toLower
  where
    isCombining c = generalCategory c == NonSpacingMark
    replacePunct c = if isAlphaNum c then c else ' '

tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.words . normalize

{- | Case-insensitive substring test using Unicode case folding.
@needle `caseInsensitiveInfixOf` haystack@ mirrors 'T.isInfixOf' semantics.
-}
caseInsensitiveInfixOf :: Text -> Text -> Bool
caseInsensitiveInfixOf needle haystack =
    T.toCaseFold needle `T.isInfixOf` T.toCaseFold haystack

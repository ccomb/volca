{-# LANGUAGE OverloadedStrings #-}

{- | Correctly-rounded parsing of decimal amounts.

This is the read side of the amount round-trip whose write side is
'EcoSpold.Common.showFFloatTrim'. Keeping the reader in one place lets every
importer and every export round-trip guard share the exact same parse, so the
guards mirror what import will actually do.
-}
module Amount (readAmount) where

import qualified Data.Attoparsec.Text as A
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

{- | Parse a decimal amount to a 'Double' with correct round-to-nearest
rounding – the exact inverse of 'EcoSpold.Common.showFFloatTrim'.

'Data.Text.Read.double' is /not/ correctly rounded: it can be off by up to one
ULP on perfectly ordinary magnitudes (e.g. @"0.0000010897906999999999"@ parses
to @1.0897907e-6@, a different 'Double'). On import that silently corrupts
amounts; on the writers' round-trip guards it rejects values the format can
represent.

We tokenize with attoparsec's fast 'A.scientific', which yields an /exact/
'Data.Scientific.Scientific' (an arbitrary-precision decimal), then convert with
@fromRational . toRational@. 'toRational' is exact and 'fromRational' is
correctly rounded by the Report, so every finite 'Double' round-trips –
subnormals included. (We deliberately avoid 'Data.Scientific.toRealFloat', whose
underflow/overflow short-circuits are version-dependent and can lose a subnormal
to @0@.) An out-of-range literal like @"1e400"@ converts to 'Infinity' and is
rejected here, alongside @NaN@, since a non-finite value is never a valid LCA
amount and must surface rather than slip in silently.

Accepts the forms 'Data.Text.Read.double' does – surrounding whitespace, a
leading sign, scientific notation, and a bare leading or trailing decimal point
(@".5"@, @"1."@) – but rejects trailing garbage (@A.endOfInput@).
-}
readAmount :: Text -> Maybe Double
readAmount t = case A.parseOnly (A.scientific <* A.endOfInput) (normalize t) of
    Right s -> finite (fromRational (toRational s))
    Left _ -> Nothing
  where
    -- attoparsec's 'scientific' handles a leading sign, a trailing dot and an
    -- exponent itself; it only needs a digit before the point, so pad a bare
    -- leading dot (".5" / "-.5"). The '+' is stripped so that padding stays a
    -- two-case match.
    finite d
        | isNaN d || isInfinite d = Nothing
        | otherwise = Just d
    normalize = padLeadingDot . dropLeadingPlus . T.strip
    dropLeadingPlus s = fromMaybe s (T.stripPrefix "+" s)
    padLeadingDot s = case T.uncons s of
        Just ('.', _) -> "0" <> s
        Just ('-', r) | "." `T.isPrefixOf` r -> "-0" <> r
        _ -> s

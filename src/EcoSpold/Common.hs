{-# LANGUAGE OverloadedStrings #-}

-- | Common utilities shared between EcoSpold1 and EcoSpold2 parsers
module EcoSpold.Common (
    bsToText,
    decodeXmlEntities,
    decodeXmlEntitiesFull,
    numericRefChar,
    bsToIntMaybe,
    isElement,
    distributeFiles,
    nonEmptyText,
    docSection,
    joinParts,
    showFFloatTrim,

    -- * What one dataset yields
    ParsedDataset (..),
) where

import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Numeric (showFFloat)
import Types (Activity, BiosphereFlow, DocSection (..), TechnosphereFlow, Unit, WasteFlow)

{- | One dataset as a reader read it: the activity, the flows and units it
names, and whatever the reader has to say about the reading.

'pdDatasetNumber' is EcoSpold 1's: that format numbers its datasets, and an
exchange points at the numbered dataset that supplies it on its own row, where
EcoSpold 2 addresses a supplier by UUID. A reader with nothing to say there
leaves it empty.
-}
data ParsedDataset = ParsedDataset
    { pdActivity :: !Activity
    , pdTechFlows :: ![TechnosphereFlow]
    , pdBioFlows :: ![BiosphereFlow]
    , pdWasteFlows :: ![WasteFlow]
    , pdUnits :: ![Unit]
    , pdDatasetNumber :: !Int
    , pdWarnings :: ![Text]
    -- ^ What the reader could not make sense of, for the caller to report.
    }

-- | ByteString to Text conversion with UTF-8 decoding and XML entity decoding
bsToText :: BS.ByteString -> Text
bsToText = decodeXmlEntities . TE.decodeUtf8

{- | Decode the common XML entities Xeno (a fast SAX parser) leaves intact: the
five named entities plus the line-feed/carriage-return numeric refs that appear
inside EcoSpold attribute values (e.g. @generalComment="text&#10;"@).

Deliberately limited to those two numeric refs. This runs on the whole 'bsToText'
read path, so decoding arbitrary @&#NNN;@ here would collapse a double-encoded
literal @&amp;#NNN;@ to a control character instead of round-tripping it to the
literal @&#NNN;@. Full numeric decoding lives in 'decodeXmlEntitiesFull', applied
only where the text is afterwards split on @;@ (ILCD synonyms).
-}

-- @&amp;@ is resolved LAST (leftmost in the composition runs last), the exact
-- inverse of the writers escaping @&@ FIRST. Resolving it first would turn an
-- escaped literal @"&lt;"@ (written as @"&amp;lt;"@) back into @"<"@ instead of
-- @"&lt;"@ - a silent round-trip corruption for entity-like text.
decodeXmlEntities :: Text -> Text
decodeXmlEntities =
    T.replace "&amp;" "&"
        . T.replace "&lt;" "<"
        . T.replace "&gt;" ">"
        . T.replace "&quot;" "\""
        . T.replace "&apos;" "'"
        . T.replace "&#10;" "\n"
        . T.replace "&#13;" "\r"

{- | Fully decode XML entities, including arbitrary numeric character references,
iterating to a fixed point. The ILCD flow data double-encodes entities
(@&amp;#039;@, @&amp;lt;@): one 'decodeXmlEntities' pass exposes the inner
@&#039;@ / @&lt;@, 'decodeNumericRefs' (composed in here, NOT on the general read
path) resolves @&#039;@ to its character, and repeating until stable finishes the
named half. Use this only on free text afterwards split on @;@ (ILCD synonyms),
where a surviving entity's own @;@ would be taken for a separator - not on the
general read path, where it would collapse an escaped literal that
'decodeXmlEntities' deliberately preserves.
-}
decodeXmlEntitiesFull :: Text -> Text
decodeXmlEntitiesFull = go
  where
    go t =
        let t' = decodeNumericRefs (decodeXmlEntities t)
         in if t' == t then t else go t'

{- | Decode every XML numeric character reference in a text - decimal @&#NNN;@
and hex @&#xHH;@ - to its character, via 'numericRefChar'. A malformed or
out-of-range reference is left verbatim rather than crashing.
-}
decodeNumericRefs :: Text -> Text
decodeNumericRefs t =
    case T.breakOn "&#" t of
        (before, rest)
            | T.null rest -> before
            | otherwise ->
                let (body, semi) = T.span (/= ';') (T.drop 2 rest)
                 in case (numericRefChar body, T.null semi) of
                        (Just c, False) ->
                            before <> T.singleton c <> decodeNumericRefs (T.drop 1 semi)
                        _ ->
                            before <> "&#" <> decodeNumericRefs (T.drop 2 rest)

{- | Decode the body of a single XML numeric character reference - the text
between @&#@ and @;@ - to its character. Decimal by default, hexadecimal when
prefixed @x@/@X@. 'Nothing' on a malformed or out-of-range value (no partial
'chr' on bad input); 'Integer' parsing avoids overflow on absurdly long digit
runs. Shared with "BrightwayExcel.Parser".
-}
numericRefChar :: Text -> Maybe Char
numericRefChar body = case T.uncons body of
    Just (x, hexits)
        | x == 'x' || x == 'X' -> readRef TR.hexadecimal hexits
    _ -> readRef TR.decimal body
  where
    readRef :: TR.Reader Integer -> Text -> Maybe Char
    readRef reader digits = case reader digits of
        Right (n, leftover)
            | T.null leftover, n >= 0, n <= 0x10FFFF -> Just (chr (fromInteger n))
        _ -> Nothing

{- | ByteString to Int conversion that returns Nothing on parse failure.
Use for attribute values that are user-controlled or optional and where
crashing the parser on malformed input is not acceptable.
-}
bsToIntMaybe :: BS.ByteString -> Maybe Int
bsToIntMaybe bs = case TR.decimal (bsToText bs) of
    Right (val, _) -> Just val
    Left _ -> Nothing

{- | Check if element name matches (with or without namespace prefix)
Handles both "tagName" and "prefix:tagName" forms
-}
isElement :: BS.ByteString -> BS.ByteString -> Bool
isElement tagName expected =
    tagName == expected || BS.isSuffixOf (":" `BS.append` expected) tagName

-- | Drop empty / whitespace-only text. Single normalisation point.
nonEmptyText :: Text -> Maybe Text
nonEmptyText t =
    let s = T.strip t
     in if T.null s then Nothing else Just s

{- | One documentation section, or none at all when the dataset left the field
blank. Both EcoSpold parsers assemble their sections through this, so "a rubric
the source did not fill is not a rubric" is decided once.

A field an exporter filled with its own placeholder for absence counts as
blank: openLCA writes the literal @\<null\>@ where it has nothing, 3794 of the
11947 datasets of the BAFU export among them, and reporting that to a reader as
what the dataset says about its geography would be worse than saying nothing.
Only the placeholder alone is read that way - "none" and "not known" are
statements a person wrote, and they are reported as written.
-}
docSection :: Text -> Text -> [DocSection]
docSection label = maybe [] (pure . DocSection label) . nonEmptyText . dropNullMarker
  where
    dropNullMarker t = if T.strip t == "<null>" then "" else t

{- | Join the pieces a format spreads one section over, dropping the blanks:
a publisher with no place of publication reads as the publisher alone, not as
a dangling separator.
-}
joinParts :: Text -> [Text] -> Text
joinParts sep = T.intercalate sep . mapMaybe nonEmptyText

-- | Distribute a list evenly across N buckets (for parallel workers)
distributeFiles :: Int -> [a] -> [[a]]
distributeFiles n xs =
    let len = length xs
        baseSize = len `div` n
        remainder = len `mod` n
        sizes = replicate remainder (baseSize + 1) ++ replicate (n - remainder) baseSize
     in go sizes xs
  where
    go [] _ = []
    go _ [] = []
    go (s : ss) ys = let (h, t) = splitAt s ys in h : go ss t

{- | Render a 'Double' in fixed-point notation (never scientific) with trailing
zeros trimmed but at least one fractional digit kept. This is the canonical
amount format for the EcoSpold/ILCD/Brightway writers and the exact inverse of
'Amount.readAmount': through that correctly-rounded reader every finite 'Double' -
subnormals included - re-parses to the same value. Fixed-point is also the
form these exchange formats and their external readers expect. The writers'
export guards reject any amount that does not re-parse, which now leaves only the
non-finite @Infinity@/@NaN@.
-}
showFFloatTrim :: Double -> String
showFFloatTrim d =
    case break (== '.') (showFFloat Nothing d "") of
        (intPart, '.' : fracPart) ->
            let trimmed = reverse (dropWhile (== '0') (reverse fracPart))
             in intPart <> "." <> (if null trimmed then "0" else trimmed)
        (intPart, _) -> intPart <> ".0"

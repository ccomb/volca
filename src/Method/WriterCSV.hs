{-# LANGUAGE OverloadedStrings #-}

{- | Writer for the columnar CSV method format - the exact inverse of
"Method.ParserCSV". One file, one column per impact category, one row per
distinct (substance, compartment, CAS, flow unit): the spreadsheet view of
a method collection.

Deterministic: rows are sorted by their key, values render through
'formatAmount', and nothing volatile (timestamp, version) is emitted, so
the same collection always serializes to the same bytes.

Projections onto the format's conventions (matching "Method.WriterSimaPro"
where the two formats share a limitation):

* a regionalized CF is written as a name-suffixed substance (@Water, FR@);
* the compartment cell is the @top/sub/qualifier@ path the parser reads
  back exactly, so subcompartment distinctions survive;
* flow direction is not a column - the parser re-derives it from the
  compartment (resource and land media → input). A CF whose direction
  disagrees is reported in the warnings;
* duplicate factors for one key in one category (they exist in real method
  packages) become extra rows, never a silently dropped or merged value.

Not representable, reported as warnings and omitted: method descriptions,
mixed methodologies (the @# methodology@ comment holds one), damage
categories, normalization/weighting sets, and formula scoring sets.
-}
module Method.WriterCSV (
    serializeColumnarMethodCSV,
    checkColumnarExportable,
) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Method.CSV (joinRow)
import Method.ParserCSV (knownTops)
import Method.Types
import SimaPro.Writer (formatAmount)

delim :: Char
delim = ';'

keyLabels :: [Text]
keyLabels = ["substance", "compartment", "cas", "unit"]

{- | Serialize a method collection to columnar CSV bytes, paired with the
projection warnings. 'Left' when the format cannot represent the collection
without silent corruption on re-import ('checkColumnarExportable').
-}
serializeColumnarMethodCSV :: MethodCollection -> Either Text (BS.ByteString, [Text])
serializeColumnarMethodCSV mc = do
    checkColumnarExportable mc
    let ms = mcMethods mc
        headerRow field = joinRow delim (map (const "") keyLabels <> map (T.strip . field) ms)
        header =
            [ headerRow methodCategory
            , headerRow methodName
            , headerRow methodUnit
            , joinRow delim (keyLabels <> map (const "") ms)
            ]
        methodologyLine = maybe [] (\m -> ["# methodology: " <> T.strip m]) (sharedMethodology ms)
        allLines = methodologyLine <> header <> tableRows ms
        warnings = directionWarnings ms <> lossWarnings mc
    pure (TE.encodeUtf8 (T.unlines allLines), warnings)

-- | Key of a data row: what the parser reads back from the key columns.
type RowKey = (Text, Text, Text, Text)

{- | Every cell is emitted stripped: the parser strips what it reads, so
surrounding whitespace (it occurs in real source data, e.g. a CAS written
@\" 5595-10-8\"@) could never survive a round-trip - it is identifier noise,
not information.
-}
rowKey :: MethodCF -> RowKey
rowKey cf =
    ( T.strip (mcfFlowName cf <> maybe "" (", " <>) (mcfConsumerLocation cf))
    , compartmentCell (mcfCompartment cf)
    , maybe "" T.strip (mcfCAS cf)
    , T.strip (mcfUnit cf)
    )

-- | The @top/sub/qualifier@ path, segments stripped, trailing empties dropped.
compartmentCell :: Maybe Compartment -> Text
compartmentCell Nothing = ""
compartmentCell (Just (Compartment top sub qualifier)) =
    T.intercalate "/" (shorten (map T.strip [top, sub, qualifier]))
  where
    shorten = foldr (\s acc -> if T.null s && null acc then [] else s : acc) []

{- | Factorize the factors of all methods into sorted rows. A key repeated
inside one category keeps every occurrence: the widest column decides how
many physical rows the key spans, and each occurrence lands on its own row
in factor order.
-}
tableRows :: [Method] -> [Text]
tableRows ms =
    [ joinRow delim ([name, comp, cas, unit] <> [render i (M.lookup col table) | col <- columns])
    | ((name, comp, cas, unit), table) <- M.toAscList grouped
    , i <- [0 .. depth table - 1]
    ]
  where
    columns = zipWith const [0 :: Int ..] ms
    grouped =
        M.fromListWith
            (M.unionWith (flip (<>)))
            [ (rowKey cf, M.singleton col [mcfValue cf])
            | (col, m) <- zip [0 ..] ms
            , cf <- methodFactors m
            ]
    depth = maximum . (1 :) . map length . M.elems
    render i cells = case drop i (fromMaybe [] cells) of
        v : _ -> formatAmount v
        [] -> ""

{- | The one methodology the @# methodology@ comment can carry: the value all
impact categories agree on, if any.
-}
sharedMethodology :: [Method] -> Maybe Text
sharedMethodology ms = case S.toList (S.fromList (map methodMethodology ms)) of
    [Just m] -> Just m
    _ -> Nothing

{- | The parser derives direction from the compartment cell (resource and
land media → input, everything else → output). Report the factors whose
recorded direction would come back different - bounded to a count and a few
examples, a real method can disagree tens of thousands of times.
-}
directionWarnings :: [Method] -> [Text]
directionWarnings ms = case lost of
    [] -> []
    _ ->
        [ T.pack (show (length lost))
            <> " factors have a flow direction the compartment does not imply; the re-imported direction follows the compartment (e.g. "
            <> T.intercalate "; " examples
            <> ")"
        ]
  where
    lost = [cf | m <- ms, cf <- methodFactors m, mcfDirection cf /= impliedDirection cf]
    examples = take 5 (S.toAscList (S.fromList (map describe lost)))
    describe cf = mcfFlowName cf <> " (" <> compartmentCell (mcfCompartment cf) <> ")"
    impliedDirection cf = case mcfCompartment cf of
        Just (Compartment t _ _)
            | t == "natural resource" || "land " `T.isPrefixOf` t -> Input
        _ -> Output

-- | One warning per collection-level feature the format has no room for.
lossWarnings :: MethodCollection -> [Text]
lossWarnings mc =
    mapMaybe
        (\(count, what) -> if count == 0 then Nothing else Just (T.pack (show count) <> " " <> what))
        [ (length descriptions, "impact category descriptions are not representable in columnar CSV")
        , (lostMethodologies, "distinct stated methodologies; the single '# methodology' comment is omitted (it must be shared by every impact category)")
        , (length (mcDamageCategories mc), "damage categories are not representable in columnar CSV")
        , (length (mcNormWeightSets mc), "normalization/weighting sets are not representable in columnar CSV")
        , (length (mcScoringSets mc), "formula scoring sets are not representable in columnar CSV")
        ]
  where
    descriptions = mapMaybe methodDescription (mcMethods mc)
    -- Count only the methodologies actually stated: an absent one is not a
    -- distinct methodology, but it does block the shared comment - so any
    -- stated methodology is lost whenever 'sharedMethodology' finds none.
    lostMethodologies = case sharedMethodology (mcMethods mc) of
        Just _ -> 0
        Nothing -> S.size (S.fromList (mapMaybe methodMethodology (mcMethods mc)))

{- | Reject a collection the columnar format cannot represent without silent
corruption on re-import: no impact categories, a blank category name (its
column would be silently skipped), a non-finite value, a line break inside a
field (the parser splits on physical lines before CSV parsing, so quoting
cannot save it), or a @/@ inside a compartment segment (it would shift the
compartment path).
-}
checkColumnarExportable :: MethodCollection -> Either Text ()
checkColumnarExportable mc
    | null (mcMethods mc) = Left "method collection has no impact categories"
    | otherwise = mapM_ checkMethod (mcMethods mc)
  where
    checkMethod m = do
        checkName (methodName m)
        mapM_ (noLineBreak "impact category name") [methodName m]
        mapM_ (noLineBreak "impact category group") [methodCategory m]
        mapM_ (noLineBreak "impact category unit") [methodUnit m]
        mapM_ (noLineBreak "methodology") (methodMethodology m)
        mapM_ (checkCF (methodName m)) (methodFactors m)
    checkName name
        | T.null (T.strip name) = Left "impact category has a blank name"
        | otherwise = Right ()
    checkCF cat cf = do
        finite ("characterization factor for '" <> mcfFlowName cf <> "' in '" <> cat <> "'") (mcfValue cf)
        mapM_ (noLineBreak "flow name") [mcfFlowName cf]
        mapM_ (noLineBreak "flow unit") [mcfUnit cf]
        mapM_ (noLineBreak "CAS number") (mcfCAS cf)
        mapM_ (noLineBreak "consumer location") (mcfConsumerLocation cf)
        mapM_ checkCompartment (mcfCompartment cf)
    checkCompartment (Compartment top sub qualifier) = do
        checkTop top
        mapM_
            ( \segment -> do
                noLineBreak "compartment" segment
                noSlash segment
            )
            [top, sub, qualifier]
    -- The parser only reads a compartment path back when its top segment is
    -- one it knows; anything else would silently come back compartment-less.
    checkTop top
        | top `elem` knownTops = Right ()
        | otherwise =
            Left
                ( "compartment '"
                    <> top
                    <> "' is outside the ones columnar CSV can name ("
                    <> T.intercalate ", " knownTops
                    <> ")"
                )
    noSlash segment
        | T.any (== '/') segment =
            Left ("compartment segment contains '/' (the path separator): " <> T.take 60 segment)
        | otherwise = Right ()
    noLineBreak label t
        | T.any (\c -> c == '\n' || c == '\r') t =
            Left ("field contains a line break (" <> label <> "): " <> T.take 60 t)
        | otherwise = Right ()
    finite label v
        | isNaN v || isInfinite v = Left ("non-finite " <> label)
        | otherwise = Right ()

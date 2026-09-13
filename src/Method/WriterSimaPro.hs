{-# LANGUAGE OverloadedStrings #-}

{- | SimaPro method CSV writer – the inverse of "Method.ParserSimaPro".

Serializes an in-memory 'MethodCollection' to a SimaPro @{methods}@ CSV
export: one file-level method whose impact categories, damage categories and
normalization-weighting sets are the collection's. The output is deterministic
(no timestamp, 'M.toAscList' for the NW maps, CRLF, the same pinned header as
"SimaPro.Writer") so a write→parse→write cycle is stable, and a
@parse → write → parse@ round-trip of a SimaPro-origin collection reproduces
the collection exactly (flow and method UUIDs are name-derived on both sides).

Collections imported from other formats (ILCD, openLCA, tabular CSV) are
projected onto SimaPro's conventions rather than dropped:

  * a regionalized CF (@'mcfConsumerLocation' = Just loc@) becomes a
    name-suffixed substance row (@\"Water, FR\"@) – the representation the
    SimaPro-adapted method distributions themselves use;
  * a compartment qualifier folds into the subcompartment column
    (@\"groundwater, long-term\"@), SimaPro's own encoding of long-term;
  * the land media (@land occupation@, @land transformation@) file under
    SimaPro's @Raw@ compartment, where every SimaPro method distribution
    keeps its occupation and transformation flows – the from/to semantics
    (and hence the flow's direction) live in the flow name there, so these
    rows are exempt from the direction check below;
  * the CAS number is zero-padded back to SimaPro's 6-digit first segment.

What the format cannot carry travels as an explicit warning, never silently:
a CF with no compartment (emitted with empty compartment columns), a CF whose
direction contradicts its compartment (SimaPro derives direction from the
compartment column alone), an NW set with no factors, and formula scoring
sets (config-side, not part of the format). Anything that would corrupt the
file structure or re-import wrongly (a non-finite value, a line break inside
a field, a file-level name that collides with a section marker) is rejected
outright by 'checkMethodExportable'.
-}
module Method.WriterSimaPro (
    serializeSimaProMethodCSV,
    checkMethodExportable,
) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Method.Types (
    Compartment (..),
    DamageCategory (..),
    FlowDirection (..),
    Method (..),
    MethodCF (..),
    MethodCollection (..),
    NormWeightSet (..),
 )
import SimaPro.Writer (WriterConfig, escapeField, formatAmount, headerLines)

{- | Serialize a 'MethodCollection' to SimaPro method CSV bytes (UTF-8, CRLF),
paired with the projection warnings. The file-level method name is the
methodology shared by every impact category when there is one, otherwise the
supplied collection name. Fails on a collection the format cannot represent
without corruption ('checkMethodExportable').
-}
serializeSimaProMethodCSV :: WriterConfig -> Text -> MethodCollection -> Either Text (BS.ByteString, [Text])
serializeSimaProMethodCSV cfg collectionName mc = do
    checkMethodExportable mc
    let fileName = fileLevelName collectionName mc
    checkFileLevelName fileName
    let (catBlocks, issues) = unzip (map categoryBlock (mcMethods mc))
        (nwBlocks, nwWarnings) = unzip (map nwBlock (mcNormWeightSets mc))
        allLines =
            header cfg
                ++ metaBlock fileName mc
                ++ concat catBlocks
                ++ concatMap damageBlock (mcDamageCategories mc)
                ++ concat nwBlocks
                ++ ["End"]
        warnings = issueWarnings (concat issues) ++ concat nwWarnings ++ scoringSetWarning mc
    pure (TE.encodeUtf8 (T.intercalate crlf allLines <> crlf), warnings)

-- ============================================================================
-- Exportability guard
-- ============================================================================

{- | Reject a collection the SimaPro method format cannot represent without
silent corruption on re-import: no impact categories at all, a non-finite
value (its literal would re-import as a parse failure or a wrong number), or
a line break inside a field (the parser splits on physical lines before CSV
parsing, so quoting cannot save it – same rule as the process writer).
-}
checkMethodExportable :: MethodCollection -> Either Text ()
checkMethodExportable mc
    | null (mcMethods mc) = Left "method collection has no impact categories"
    | otherwise = do
        mapM_ checkMethod (mcMethods mc)
        mapM_ checkDamage (mcDamageCategories mc)
        mapM_ checkNW (mcNormWeightSets mc)
  where
    checkMethod m = do
        mapM_ (noLineBreak "impact category name") [methodName m]
        mapM_ (noLineBreak "impact category unit") [methodUnit m]
        mapM_ (checkCF (methodName m)) (methodFactors m)
    checkCF cat cf = do
        finite ("characterization factor for '" <> mcfFlowName cf <> "' in '" <> cat <> "'") (mcfValue cf)
        mapM_ (noLineBreak "flow name") [mcfFlowName cf]
        mapM_ (noLineBreak "flow unit") [mcfUnit cf]
        mapM_ (noLineBreak "CAS number") (mcfCAS cf)
        mapM_ (noLineBreak "consumer location") (mcfConsumerLocation cf)
        mapM_ (\(Compartment a b c) -> mapM_ (noLineBreak "compartment") [a, b, c]) (mcfCompartment cf)
    checkDamage dc = do
        mapM_ (noLineBreak "damage category name") [dcName dc]
        mapM_ (noLineBreak "damage category unit") [dcUnit dc]
        mapM_
            ( \(n, v) -> do
                noLineBreak "damage impact name" n
                finite ("damage factor for '" <> n <> "' in '" <> dcName dc <> "'") v
            )
            (dcImpacts dc)
    checkNW nw = do
        checkNWName (nwName nw)
        mapM_ (checkNamed ("normalization factor (" <> nwName nw <> ")")) (M.toAscList (nwNormalization nw))
        mapM_ (checkNamed ("weighting factor (" <> nwName nw <> ")")) (M.toAscList (nwWeighting nw))
    checkNamed label (n, v) = noLineBreak "impact category name" n *> finite (label <> " for '" <> n <> "'") v

{- | The NW-set name is written verbatim on its own line, and the parser takes
the first non-blank line after the marker as the name – a blank name would
promote the following section keyword (@Normalization@) to the set's name and
silently drop that section's factors on re-import.
-}
checkNWName :: Text -> Either Text ()
checkNWName name
    | T.null (T.strip name) = Left "normalization-weighting set has a blank name"
    | otherwise = noLineBreak "normalization-weighting set name" name

noLineBreak :: Text -> Text -> Either Text ()
noLineBreak label t
    | T.any (\c -> c == '\n' || c == '\r') t =
        Left ("field contains a line break (" <> label <> "): " <> T.take 60 t)
    | otherwise = Right ()

finite :: Text -> Double -> Either Text ()
finite label v
    | isNaN v || isInfinite v = Left ("non-finite " <> label)
    | otherwise = Right ()

{- | The file-level @Name@ line is read back verbatim as one physical line, and
the parser scans method-level metadata for section markers – a name that /is/
a marker would derail the state machine, so it is rejected rather than
written.
-}
checkFileLevelName :: Text -> Either Text ()
checkFileLevelName name = do
    noLineBreak "method name" name
    if name `elem` ["Impact category", "Damage category"]
        || "Normalization-Weighting set" `T.isPrefixOf` name
        || "Normalisation-Weighting set" `T.isPrefixOf` name
        then Left ("method name collides with a SimaPro section marker: " <> name)
        else Right ()

-- ============================================================================
-- Blocks
-- ============================================================================

crlf :: Text
crlf = "\r\n"

-- | Join fields with the semicolon delimiter (each escaped).
spRow :: [Text] -> Text
spRow = T.intercalate ";" . map escapeField

-- | The pinned header block with the @{methods}@ file-type line.
header :: WriterConfig -> [Text]
header cfg = case headerLines cfg of
    (banner : rest) -> banner : "{methods}" : rest
    [] -> ["{methods}"]

{- | File-level name: the methodology every impact category agrees on when
there is exactly one, otherwise the collection's own name. Preserves the
original SimaPro @Name@ across an upload→export cycle without inventing one
for mixed or ILCD-origin collections.
-}
fileLevelName :: Text -> MethodCollection -> Text
fileLevelName fallback mc =
    case S.toList (S.fromList (map methodMethodology (mcMethods mc))) of
        [Just m] -> m
        _ -> fallback

{- | Method-level metadata: the @Name@ the parser reads back as the
methodology, plus the Use-flags SimaPro expects, reflecting what the
collection actually carries. Key and value each sit on their own line.
-}
metaBlock :: Text -> MethodCollection -> [Text]
metaBlock fileName mc =
    ["Method", "", "Name", fileName, ""]
        ++ ["Use Damage Assessment", yesNo (not (null (mcDamageCategories mc))), ""]
        ++ ["Use Normalization", yesNo hasNorm, ""]
        ++ ["Use Weighting", yesNo hasWeight, ""]
        ++ (if hasWeight then ["Weighting unit", "Pt", ""] else [])
  where
    yesNo b = if b then "Yes" else "No"
    hasNorm = not (all (M.null . nwNormalization) (mcNormWeightSets mc))
    hasWeight = not (all (M.null . nwWeighting) (mcNormWeightSets mc))

-- | One @Impact category@ block, plus the per-CF representation issues.
categoryBlock :: Method -> ([Text], [CFIssue])
categoryBlock m =
    ( ["Impact category", spRow [methodName m, methodUnit m], "", "Substances"]
        ++ cfLines
        ++ [""]
    , concat issues
    )
  where
    (cfLines, issues) = unzip (map cfRow (methodFactors m))

{- | What a substance row cannot carry faithfully. Collected per CF, then
summarized into a bounded number of warnings ('issueWarnings').
-}
data CFIssue
    = -- | No compartment: emitted with empty compartment columns.
      NoCompartment !Text
    | {- | Direction contradicts the compartment column, which is what SimaPro
      derives direction from – a re-import flips it.
      -}
      DirectionLost !Text

{- | One substance row:
@compartment;subcompartment;name;cas;value;unit@. A regionalized CF gets the
SimaPro name suffix; the CAS is padded back to SimaPro's 6-digit first
segment.
-}
cfRow :: MethodCF -> (Text, [CFIssue])
cfRow cf = (line, noComp ++ dirLost)
  where
    (compCol, subCol) = compartmentColumns (mcfCompartment cf)
    name = mcfFlowName cf <> maybe "" (", " <>) (mcfConsumerLocation cf)
    line =
        spRow
            [ compCol
            , subCol
            , name
            , maybe "" padCAS (mcfCAS cf)
            , formatAmount (mcfValue cf)
            , mcfUnit cf
            ]
    noComp = case mcfCompartment cf of
        Nothing -> [NoCompartment name]
        Just _ -> []
    impliedDirection =
        let lc = T.toLower compCol
         in if lc == "resources" || "raw" `T.isPrefixOf` lc then Input else Output
    -- Land rows are exempt: filing them under Raw is the format's own
    -- convention (see the module header), not a representation loss.
    landProjected = case mcfCompartment cf of
        Just (Compartment medium _ _) -> isLandMedium medium
        Nothing -> False
    dirLost = [DirectionLost name | not landProjected, impliedDirection /= mcfDirection cf]

{- | Project a compartment onto SimaPro's two columns: the known media get
their SimaPro spelling, an unknown medium passes through verbatim, a
qualifier folds into the subcompartment (SimaPro's own long-term encoding),
and an empty subcompartment becomes @(unspecified)@.
-}
compartmentColumns :: Maybe Compartment -> (Text, Text)
compartmentColumns Nothing = ("", "")
compartmentColumns (Just (Compartment medium sub qualifier)) = (compCol, subCol)
  where
    compCol = case T.toLower medium of
        "air" -> "Air"
        "water" -> "Water"
        "soil" -> "Soil"
        "natural resource" -> "Raw"
        _ | isLandMedium medium -> "Raw"
        _ -> medium
    subWithQualifier
        | T.null qualifier = sub
        | T.null sub = qualifier
        | otherwise = sub <> ", " <> qualifier
    subCol = if T.null subWithQualifier then "(unspecified)" else subWithQualifier

-- | The ILCD land media, which SimaPro files under its @Raw@ compartment.
isLandMedium :: Text -> Bool
isLandMedium medium = T.toLower medium `elem` ["land occupation", "land transformation"]

{- | Pad a normalized CAS ("124-38-9") back to SimaPro's 6-digit first segment
("000124-38-9"). A value without dashes is not CAS-shaped and passes through.
-}
padCAS :: Text -> Text
padCAS cas = case T.splitOn "-" cas of
    (first : rest@(_ : _)) -> T.intercalate "-" (T.justifyRight 6 '0' first : rest)
    _ -> cas

-- | One @Damage category@ block.
damageBlock :: DamageCategory -> [Text]
damageBlock dc =
    ["Damage category", spRow [dcName dc, dcUnit dc], "", "Impact categories"]
        ++ [spRow [n, formatAmount v] | (n, v) <- dcImpacts dc]
        ++ [""]

{- | One @Normalization-Weighting set@ block. A set with no factors at all is
skipped with a warning: the parser drops such a set on re-import, so writing
it would only fake fidelity.
-}
nwBlock :: NormWeightSet -> ([Text], [Text])
nwBlock nw
    | M.null (nwNormalization nw) && M.null (nwWeighting nw) =
        ([], ["normalization-weighting set '" <> nwName nw <> "' has no factors; skipped"])
    | otherwise =
        ( ["Normalization-Weighting set", nwName nw, ""]
            ++ section "Normalization" (nwNormalization nw)
            ++ section "Weighting" (nwWeighting nw)
        , []
        )
  where
    section title m
        | M.null m = []
        | otherwise = title : [spRow [n, formatAmount v] | (n, v) <- M.toAscList m] ++ [""]

-- ============================================================================
-- Warnings
-- ============================================================================

-- | Summarize the per-CF issues into one bounded warning per kind.
issueWarnings :: [CFIssue] -> [Text]
issueWarnings issues =
    summarize
        "CF(s) without compartment, emitted with empty compartment columns"
        [n | NoCompartment n <- issues]
        ++ summarize
            "CF(s) whose direction the compartment column cannot express; a re-import derives the opposite direction"
            [n | DirectionLost n <- issues]
  where
    summarize label names =
        let distinct = S.toAscList (S.fromList names)
         in [ label
                <> ": "
                <> T.pack (show (length distinct))
                <> " – e.g. "
                <> T.intercalate ", " (take 5 distinct)
            | not (null distinct)
            ]

-- | Formula scoring sets are configuration, not part of the SimaPro format.
scoringSetWarning :: MethodCollection -> [Text]
scoringSetWarning mc =
    [ "formula scoring sets are not part of the SimaPro method CSV format; not exported"
    | not (null (mcScoringSets mc))
    ]

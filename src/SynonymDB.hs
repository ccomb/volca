{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synonym Database

Maps flow names to synonym group IDs for flow matching
across different nomenclatures (ILCD, ecoinvent, SimaPro).

Loaded at runtime from CSV files (pairs of synonym names).
-}
module SynonymDB (
    -- * Building
    buildFromCSV,
    parseRegistryCSV,
    RegistryRow (..),
    buildFromPairs,
    buildFromEdges,
    buildFromNormalizedEdges,
    fromClassMaps,
    excludeOverFrequentSynonyms,
    excludeJunkSynonyms,
    isJunkSynonymName,
    starEdges,
    loadFromCSVFileWithCache,

    -- * Lookup
    lookupSynonymGroup,
    getSynonyms,
    normalizeName,
    normalizeNameKeepUnit,
    mergeSynonymDBs,
    synonymCount,
    oversizedClasses,
    reopenedBridges,

    -- * Directional views
    inputView,
    outputView,

    -- * Unit suffixes
    unitSuffixes,
    uncoveredUnitSuffixes,

    -- * Re-exports
    SynonymDB (..),
    BridgeDirection (..),
    SynEdge (..),
    SynViews (..),
    emptySynonymDB,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Csv (FromRecord (..), HasHeader (..), Parser, decode, (.!))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Store (decodeEx, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import System.Directory (doesFileExist, getModificationTime)

import SubstanceRegistry (equivalenceClasses)
import SynonymDB.Types (
    BridgeDirection (..),
    SynEdge (..),
    SynViews (..),
    SynonymDB (..),
    emptySynonymDB,
 )

{- | One CSV row of the curated synonym registry: two @SameAs@ names, an
optional direction, and optional curation metadata. Accepted arities:

@
name1,name2                     -- bidirectional bridge
name1,name2,direction           -- input | output | empty (= both)
name1,name2,direction,cas       -- + CAS number of the bridged substance
name1,name2,direction,cas,note  -- + free-form curation note
@

An invalid direction token is a parse error (surfaced, never silently
coerced), as is any other arity. @cas@ and @note@ document WHY a bridge is
legitimate: they are audited by the offline registry lint (RegistryLintSpec),
never consulted by the matcher.
-}
data RegistryRow = RegistryRow
    { rrEdge :: !SynEdge
    , rrCas :: !(Maybe Text)
    , rrNote :: !(Maybe Text)
    }

instance FromRecord RegistryRow where
    parseRecord v
        | n >= 2 && n <= 5 = RegistryRow <$> edge <*> meta 3 <*> meta 4
        | otherwise =
            fail $ "expected 2-5 columns (name1,name2[,direction[,cas[,note]]]), got " <> show n
      where
        n = V.length v
        edge = SynEdge <$> v .! 0 <*> v .! 1 <*> direction
        direction
            | n >= 3 = v .! 2 >>= parseDir
            | otherwise = pure BridgeBoth
        meta i
            | n > i = blankToNothing <$> v .! i
            | otherwise = pure Nothing
        blankToNothing t = let s = T.strip t in if T.null s then Nothing else Just s
        parseDir :: Text -> Parser BridgeDirection
        parseDir t = case T.toLower (T.strip t) of
            "" -> pure BridgeBoth
            "input" -> pure BridgeInput
            "output" -> pure BridgeOutput
            other -> fail $ "invalid direction " <> show other <> " (expected input|output|empty)"

{- | Parse the registry CSV into rows, keeping curation metadata. Columns:
@name1,name2[,direction[,cas[,note]]]@ (see 'RegistryRow').
-}
parseRegistryCSV :: BL.ByteString -> Either String [RegistryRow]
parseRegistryCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> err
        Right rows -> Right (V.toList rows)

{- | Build a SynonymDB from CSV content. Each row declares two names as
@SameAs@ (see 'buildFromPairs'/'buildFromEdges'); the optional third column
restricts the bridge to one flow direction. Curation metadata is dropped –
it constrains what the registry may assert, not how matching behaves.
-}
buildFromCSV :: BL.ByteString -> Either String SynonymDB
buildFromCSV = fmap (buildFromEdges . map rrEdge) . parseRegistryCSV

{- | Build a SynonymDB from untyped @SameAs@ name pairs (all 'BridgeBoth').
The direction-agnostic entry point kept for callers that do not carry direction
(auto-extraction, JSON import); delegates to 'buildFromEdges'.
-}
buildFromPairs :: [(Text, Text)] -> SynonymDB
buildFromPairs raws = buildFromEdges [SynEdge a b BridgeBoth | (a, b) <- raws]

{- | Build a SynonymDB from directed @SameAs@ edges.

Names are normalized, then grouped into equivalence classes by transitive
closure (connected components) – the "set of sets" of the canonical flow
registry. A↔B and B↔C therefore land A, B and C in one class. The top-level
tables are the UNION closure (all directions), so direction-agnostic consumers
see today's behavior. When any edge is directional, two extra views are
materialized ('SynViews'): the input view closes @both ∪ input@, the output view
@both ∪ output@ – the matching layer picks one by the CF's direction.

Closure is taken honestly, with no silent degree cap; an implausibly large class
surfaces through 'oversizedClasses' (the loader warns) rather than being silently
dropped.
-}
buildFromEdges :: [SynEdge] -> SynonymDB
buildFromEdges = buildFromNormalizedEdges . normalizeEdges

{- | Build from edges whose endpoints already carry 'normalizeName''s output –
the invariant every built DB's 'synEdges' satisfies. 'mergeSynonymDBs'
re-closes through HERE, not 'buildFromEdges':
'normalizeName' is not idempotent (a suffix exposed by punctuation removal –
@"Zinc in ground,"@ → @"zinc in ground"@ → @"zinc"@ – strips only on a second
pass), so re-normalizing stored edges would key the rebuilt tables away from
the single-pass normalization every lookup applies.
-}
buildFromNormalizedEdges :: [SynEdge] -> SynonymDB
buildFromNormalizedEdges es =
    let normd = demoteDuplicates es
        base = buildTables normd
        views
            | all ((== BridgeBoth) . seDir) normd = AllBoth
            | otherwise =
                DirectedViews
                    (viewTables (edgesFor BridgeInput normd))
                    (viewTables (edgesFor BridgeOutput normd))
     in base{synViews = views}
  where
    edgesFor dir = filter (\e -> seDir e == BridgeBoth || seDir e == dir)
    -- Views are terminal: nothing re-closes them (merging reads the TOP-level
    -- 'synEdges'), so a view's own edge list is dead weight in memory and in
    -- the serialized cache – store the lookup tables only.
    viewTables = clearEdges . buildTables
    clearEdges t = t{synEdges = []}

{- | Normalize an edge's endpoints, dropping empty/self edges (as
'buildFromPairs' did). Direction is preserved.
-}
normalizeEdges :: [SynEdge] -> [SynEdge]
normalizeEdges es =
    [ SynEdge n1 n2 (seDir e)
    | e <- es
    , let n1 = normalizeName (seA e)
    , let n2 = normalizeName (seB e)
    , not (T.null n1)
    , not (T.null n2)
    , n1 /= n2
    ]

-- | Normalize both ends of each pair, dropping empty names and self-pairs.
normalizePairs :: [(Text, Text)] -> [(Text, Text)]
normalizePairs rs =
    [ (n1, n2)
    | (raw1, raw2) <- rs
    , let n1 = normalizeName raw1
    , let n2 = normalizeName raw2
    , not (T.null n1)
    , not (T.null n2)
    , n1 /= n2
    ]

{- | Drop the 'BridgeBoth' copy of an unordered pair that also has a directional
edge, so an untyped duplicate of that exact pair (e.g. a merged auto-extracted
@freshwater = water…@ row) cannot silently reopen a curated one-way bridge in
the other view. The guard is pair-local only: an untyped transitive chain
between the same endpoints (@a=x@, @x=b@) still re-links them in the closed
view – 'reopenedBridges' detects that residue so the loader can surface it.
-}
demoteDuplicates :: [SynEdge] -> [SynEdge]
demoteDuplicates es =
    [e | e <- es, not (seDir e == BridgeBoth && S.member (key e) directedPairs)]
  where
    key e = if seA e <= seB e then (seA e, seB e) else (seB e, seA e)
    directedPairs = S.fromList [key e | e <- es, seDir e /= BridgeBoth]

{- | Number name classes into the bidirectional lookup tables, closed from the
given edges. The result's 'synViews' is 'AllBoth' – 'buildFromEdges' attaches
directional views when needed (a view is itself an 'AllBoth' table).
-}
buildTables :: [SynEdge] -> SynonymDB
buildTables edges =
    let classes = equivalenceClasses [(seA e, seB e) | e <- edges]
        numbered = zip [0 ..] classes
        nameToId = M.fromList [(name, gid) | (gid, members) <- numbered, name <- members]
        idToNames = M.fromList numbered
     in SynonymDB nameToId idToNames edges AllBoth

{- | Wrap externally-numbered class tables (the synonyms-compiler's JSON import
and capped group builder) into a SynonymDB, reconstructing untyped star edges so
the relation stays re-closable. The one place those construction sites share, so
a new 'SynonymDB' field lands here instead of in every tool.
-}
fromClassMaps :: M.Map Text Int -> M.Map Int [Text] -> SynonymDB
fromClassMaps nameToId idToNames =
    SynonymDB
        { synNameToId = nameToId
        , synIdToNames = idToNames
        , synEdges = [SynEdge a b BridgeBoth | (a, b) <- starEdges (M.elems idToNames)]
        , synViews = AllBoth
        }

{- | Star edges for a set of name classes: connect each class's members to its
first member. Their transitive closure is exactly the classes – enough to
re-close the relation. 'buildFromPairs' overrides this with the original pairs,
for a faithful induced-subgraph restriction: a star centred on a node that the
restriction later drops would lose links the original topology preserves.
-}
starEdges :: [[Text]] -> [(Text, Text)]
starEdges classes = [(m0, m) | (m0 : ms) <- classes, m <- ms]

{- | The synonym view to use for INPUT (resource) CFs: the closure of the
bidirectional and input-only bridges. Identical to the union tables when no
directional edge exists.
-}
inputView :: SynonymDB -> SynonymDB
inputView db = case synViews db of
    AllBoth -> db
    DirectedViews i _ -> i

-- | The synonym view to use for OUTPUT (emission) CFs (see 'inputView').
outputView :: SynonymDB -> SynonymDB
outputView db = case synViews db of
    AllBoth -> db
    DirectedViews _ o -> o

{- | Load a SynonymDB from a CSV file, using a binary cache for speed.
  On first load: parse CSV → build SynonymDB → save .cache.zst
  On subsequent loads: load .cache.zst directly (if newer than CSV)
-}
loadFromCSVFileWithCache :: FilePath -> IO (Either String SynonymDB)
loadFromCSVFileWithCache csvPath = do
    let cachePath = csvPath ++ ".cache.zst"
    cached <- loadCache cachePath csvPath
    case cached of
        Just db -> return (Right db)
        Nothing -> do
            exists <- doesFileExist csvPath
            if not exists
                then return (Left ("File not found: " ++ csvPath))
                else do
                    csvData <- BL.readFile csvPath
                    case buildFromCSV csvData of
                        Left err -> return (Left err)
                        Right db -> do
                            saveCache cachePath db
                            return (Right db)
  where
    loadCache cachePath srcPath = do
        exists <- doesFileExist cachePath
        if not exists
            then return Nothing
            else
                catch
                    ( do
                        cacheTime <- getModificationTime cachePath
                        srcTime <- getModificationTime srcPath
                        if cacheTime < srcTime
                            then return Nothing
                            else do
                                compressed <- BS.readFile cachePath
                                case Zstd.decompress compressed of
                                    Zstd.Decompress raw -> do
                                        -- Version-tagged payload: a cache written by
                                        -- an older schema (different 'SynonymDB' Store
                                        -- shape) fails the version check or the decode,
                                        -- so it is reparsed rather than trusted.
                                        let (ver, db) = decodeEx raw
                                        if ver /= synonymDBCacheVersion
                                            then return Nothing
                                            else do
                                                result <- evaluate (force (db :: SynonymDB))
                                                return (Just result)
                                    _ -> return Nothing
                    )
                    (\(_ :: SomeException) -> return Nothing)
    saveCache cachePath db =
        catch
            (BS.writeFile cachePath (Zstd.compress 1 (encode (synonymDBCacheVersion, db))))
            (\(_ :: SomeException) -> return ())

{- | Schema version of the serialized 'SynonymDB' cache. Bump when the 'Store'
shape of 'SynonymDB' changes so stale @.cache.zst@ files are reparsed instead of
mis-decoded. (v2: directional edges + views.)
-}
synonymDBCacheVersion :: Word32
synonymDBCacheVersion = 2

{- | Merge multiple SynonymDBs into one, re-closing across them: if one source
declares A=B and another B=C, the merged DB groups {A,B,C}. The sources' own
@synEdges@ are concatenated and the whole edge set is closed again (directional
views rebuilt, 'demoteDuplicates' applied across sources), so the merged DB
carries the union of the original directed edges, not a lossy star reconstruction.
Stored edges are already normalized, hence 'buildFromNormalizedEdges'.
-}
mergeSynonymDBs :: [SynonymDB] -> SynonymDB
mergeSynonymDBs [] = emptySynonymDB
mergeSynonymDBs [db] = db
mergeSynonymDBs dbs = buildFromNormalizedEdges (concatMap synEdges dbs)

-- | Number of synonym names in the database.
synonymCount :: SynonymDB -> Int
synonymCount = M.size . synNameToId

{- | Synonym classes with more than @maxSize@ members, computed straight from
the raw pairs. Transitive closure has no degree cap (a hub no longer silently
truncates at 50), so an implausibly large class – a junk hub that fused
unrelated substances through one bad pair – must be surfaced rather than
silently polluting the synonym fan-out. The loader warns on whatever this
returns; an empty result means the closure stayed plausible.
-}
oversizedClasses :: Int -> [(Text, Text)] -> [[Text]]
oversizedClasses maxSize = filter ((> maxSize) . length) . equivalenceClasses . normalizePairs

{- | Directed edges whose one-way constraint is void: their endpoints are also
connected in the OPPOSITE direction's view. 'demoteDuplicates' removes only the
exact untyped duplicate of a directed pair; an untyped transitive chain
(@a=x@, @x=b@) or a contradictory opposite-direction row re-links the endpoints
anyway, silently widening the bridge back to both directions. That may be
intended (two one-way assertions do compose) but is more likely curation drift
in a merged source, so the loader surfaces whatever this returns rather than
letting a direction restriction quietly stop working.
-}
reopenedBridges :: SynonymDB -> [SynEdge]
reopenedBridges db = filter voided (synEdges db)
  where
    voided e = case seDir e of
        BridgeBoth -> False
        BridgeInput -> linkedIn (outputView db) e
        BridgeOutput -> linkedIn (inputView db) e
    -- Endpoints are already normalized ('normalizeEdges'), so probe the tables
    -- directly – 'lookupSynonymGroup' would re-normalize, and 'normalizeName'
    -- is not idempotent.
    linkedIn v e =
        ((==) <$> M.lookup (seA e) (synNameToId v) <*> M.lookup (seB e) (synNameToId v))
            == Just True

{- | Drop synonym pairs whose synonym (the second element) is carried by more
than @maxFlows@ distinct base names (the first element). An over-frequent
"synonym" is a classification label or stop-word – e.g. @"organic"@ (carried by
thousands of flows), @"inorganic"@, @"petroleum product"@ – not a true synonym,
which is ~1:1 with a substance and binds a handful of names at most.

Counting is directional and on normalized names, so a real flow that merely HAS
many synonyms (high out-degree, e.g. @"acetaminophen"@ with its trade names) is
never touched – only a name that ACTS as a synonym for many distinct flows is.
Returns the kept pairs and the excluded tokens with their flow counts
(descending), so the caller can surface the exclusion list, not drop it silently.
-}
excludeOverFrequentSynonyms :: Int -> [(Text, Text)] -> ([(Text, Text)], [(Text, Int)])
excludeOverFrequentSynonyms maxFlows pairs = (kept, excluded)
  where
    normed = [(p, normalizeName base, normalizeName syn) | p@(base, syn) <- pairs]
    flowsPerSynonym = M.fromListWith S.union [(ns, S.singleton nb) | (_, nb, ns) <- normed]
    overFrequent = M.filter (> maxFlows) (M.map S.size flowsPerSynonym)
    kept = [p | (p, _, ns) <- normed, not (ns `M.member` overFrequent)]
    excluded = sortOn (negate . snd) (M.toList overFrequent)

{- | Is this name an obvious non-synonym – a REACH/ILCD dossier placeholder
(@"not available"@, @"unknown"@, @"active matter"@, an ECHA id stub), or a bare
database identifier (a PubChem CID, an @"ENT 27164"@ registry number) – rather
than a substance name? These survive 'excludeOverFrequentSynonyms' (each is
carried by few flows) yet act as cut-vertices that fuse unrelated substances
through long chains, so they are dropped by string shape instead of frequency.
(@"ENT 27164"@ and @"ENT 27,164"@ both normalize to @"ent 27164"@, fusing carbon
tetrachloride and carbofuran through one shared dossier number.)

Deliberately conservative – it matches only forms that no real substance name
takes. A name carrying letters survives, so @"mixture"@ and digit-heavy names are
kept (@"toluenediisocyanate (mixture)"@, @"pcb-1254"@, @"carbon 14"@); only an
all-digit token or a known registry prefix followed by digits is dropped. The
@echa-@ check is anchored so it cannot fire on the @echa@ inside a word (e.g.
French @"huile de chauffage"@).
-}
isJunkSynonymName :: Text -> Bool
isJunkSynonymName name =
    n `elem` exactStops
        || any (`T.isInfixOf` n) infixStops
        || "unknown" `T.isPrefixOf` n
        || "echa-" `T.isPrefixOf` n
        || "echa_" `T.isPrefixOf` n
        || isRegistryId n
  where
    n = normalizeName name
    exactStops = ["none", "no data", "not applicable", "not assigned", "not specified"]
    infixStops =
        [ "available"
        , "confidential"
        , "active matter"
        , "activematter"
        , "active substance"
        , "activesubstance"
        , "active ingredient"
        , "activeingredient"
        ]
    -- A bare numeric identifier (a PubChem CID) or a known registry prefix
    -- followed by digits (USDA @"ent 27164"@, @"cipac 12"@) is a database id, not
    -- a name. A token carrying letters is never matched, so @"pcb-1254"@ and
    -- @"carbon 14"@ survive.
    isRegistryId t =
        (not (T.null t) && T.all isDigit t)
            || case T.words t of
                [pfx, num] -> pfx `elem` registryPrefixes && not (T.null num) && T.all isDigit num
                _ -> False
    registryPrefixes = ["ent", "cipac"]

{- | Drop synonym pairs touching a junk placeholder name ('isJunkSynonymName').
Returns the kept pairs and the distinct (normalized) junk tokens dropped, so the
caller can surface them rather than discard them silently.
-}
excludeJunkSynonyms :: [(Text, Text)] -> ([(Text, Text)], [Text])
excludeJunkSynonyms pairs = (kept, excluded)
  where
    kept = [p | p@(a, b) <- pairs, not (isJunkSynonymName a), not (isJunkSynonymName b)]
    excluded =
        S.toList . S.fromList $
            [normalizeName x | (a, b) <- pairs, x <- [a, b], isJunkSynonymName x]

{- | Normalize a name for lookup in the synonym database

Normalization rules:
- Lowercase
- Strip leading/trailing whitespace
- Collapse multiple spaces to single space
- Strip ", in ground" suffix (ecoinvent resource naming)
- Strip a trailing SimaPro unit suffix ("/kg", "/m3", "/Sm3")
- Remove punctuation: commas, parentheses, quotes
-}
normalizeName :: Text -> Text
normalizeName = normalizeNameWith True

{- | 'normalizeName' minus the unit-suffix strip: @"Gas, natural\/m3"@ keeps its
@\/m3@. The strip lets a unit variant borrow its base resource's CF, but it also
collapses a method's own per-unit rows (@\/kg@ vs @\/m3@ – same substance,
different densities) onto one key – this variant is the lookup key when those
rows must stay apart.
-}
normalizeNameKeepUnit :: Text -> Text
normalizeNameKeepUnit = normalizeNameWith False

normalizeNameWith :: Bool -> Text -> Text
normalizeNameWith stripUnits name =
    let
        -- Lowercase and strip
        t1 = T.strip $ T.toLower name
        -- Collapse whitespace
        t2 = T.unwords $ T.words t1
        -- Strip ", in ground" suffix
        t3 = stripSuffix ", in ground" $ stripSuffix " in ground" t2
        -- Strip a trailing SimaPro unit suffix; see 'unitSuffixes'.
        t4 = if stripUnits then foldr stripSuffix t3 unitSuffixes else t3
        -- Remove punctuation. Inlined char predicate: the old version used
        -- @T.filter (`notElem` (",()'\"" :: String))@ which forces 'T.filter'
        -- to traverse a 5-cons-cell @[Char]@ list per input character. With
        -- 'normalizeName' fired ~837K times during a single LCIA warmup, this
        -- alone took 27% of the warmup CPU.
        t5 = T.filter notPunctChar t4
        -- Collapse whitespace again (from removed punctuation)
        t6 = T.unwords $ T.words t5
     in
        t6
  where
    notPunctChar :: Char -> Bool
    notPunctChar c = c /= ',' && c /= '(' && c /= ')' && c /= '\'' && c /= '"'
    stripSuffix :: Text -> Text -> Text
    stripSuffix suffix txt =
        if suffix `T.isSuffixOf` txt
            then T.dropEnd (T.length suffix) txt
            else txt

{- | Unit suffixes that 'normalizeName' strips. SimaPro bakes a flow's unit into
its name (e.g. @"Gas, natural/m3"@); dropping the suffix lets a unit variant
share the registry node – and thus the CF – of its base resource.

MUST be lowercase: 'normalizeName' lowercases before it strips, so an uppercase
entry would never fire. Extending this list is the fix when 'uncoveredUnitSuffixes'
warns that a loaded database carries an un-stripped @/unit@ suffix.
-}
unitSuffixes :: [Text]
unitSuffixes = ["/kg", "/m3", "/sm3"]

{- | Flow names that will silently miss CF matching because they carry a trailing
@"/unit"@ for a real unit 'unitSuffixes' does not strip. Grouped by the offending
unit (each value lists example flow names) so a load-time warning is actionable –
add @"/unit"@ to 'unitSuffixes'. Empty when coverage is complete.

The unit test is supplied by the caller (@UnitConversion.isKnownUnit cfg@), so this
module stays free of a unit-system dependency.
-}
uncoveredUnitSuffixes :: (Text -> Bool) -> [Text] -> M.Map Text [Text]
uncoveredUnitSuffixes isUnit names =
    M.fromListWith
        (<>)
        [ (seg, [name])
        | name <- names
        , let (prefix, seg) = T.breakOnEnd "/" name
        , not (T.null prefix)
        , isUnit seg
        , ("/" <> T.toLower seg) `notElem` unitSuffixes
        ]

-- | Look up the synonym group ID for a flow name
lookupSynonymGroup :: SynonymDB -> Text -> Maybe Int
lookupSynonymGroup db name =
    M.lookup (normalizeName name) (synNameToId db)

-- | Get all synonyms for a group ID
getSynonyms :: SynonymDB -> Int -> Maybe [Text]
getSynonyms db gid = M.lookup gid (synIdToNames db)

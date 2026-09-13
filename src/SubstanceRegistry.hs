{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Canonical flow registry - foundation.

This module owns the entity-resolution core of the canonical flow registry:
given undirected @SameAs@ assertions between flow identities, it computes the
equivalence classes (connected components / transitive closure) - the "set of
sets" that unifies a substance's many names across nomenclatures (ILCD,
ecoinvent, SimaPro, BAFU).

Identity is currently resolved by name pairs (see "SynonymDB"). The typed,
provenanced edge layer (Subsumes / ProxyFor split-ratios, and CAS/UUID anchor
nodes that fuse a substance's names across sources) builds on top of this
closure primitive - none of which changes the primitive itself.
-}
module SubstanceRegistry (
    -- * Closure primitive
    equivalenceClasses,

    -- * Typed-edge model (the registry's substance layer)
    CASNumber (..),
    FlowUUID (..),
    NormName (..),
    SourceId (..),
    SplitWeight (..),
    ConversionFactor (..),
    ClassId (..),
    SubstanceKey (..),
    Relation (..),
    SubstanceEdge (..),
    ClassResult (..),
    classesFromEdges,

    -- * CAS enrichment
    normalizeCAS,
    nonEmptyCAS,
    casKey,
    casBindings,
    casBindingsFromEdges,

    -- * On-disk format
    KeyNormalizers (..),
    parseSubstanceEdges,
) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (..), decode)
import qualified Data.Graph as G
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Data.UUID (UUID, fromText)
import qualified Data.Vector as V
import Text.Read (readMaybe)

{- | Connected components of the undirected graph whose edges are the given
@SameAs@ pairs. Each component is the transitive closure of the identities
reachable through those pairs: @A↔B@ and @B↔C@ ⟹ one class @{A,B,C}@.

Only identities that appear in at least one pair are returned; an isolated
identity has no class here. Order within and across classes is unspecified.

Computed via 'Data.Graph' connected components - linear in vertices + edges, so
it scales to the hundreds of thousands of pairs a method's auto-extracted
synonyms produce. (A hand-rolled persistent-'Map' union-find without path
compression did not: it went quadratic on long chains and stalled method load
for minutes.)
-}
equivalenceClasses :: forall a. (Ord a) => [(a, a)] -> [[a]]
equivalenceClasses pairs =
    map (map nodeOf . Tree.flatten) (G.components graph)
  where
    elems :: [a]
    elems = S.toList $ S.fromList $ concatMap (\(a, b) -> [a, b]) pairs

    adjacency :: Map a [a]
    adjacency = M.fromListWith (++) $ concatMap (\(a, b) -> [(a, [b]), (b, [a])]) pairs

    (graph, fromVertex, _) =
        G.graphFromEdges [(x, x, M.findWithDefault [] x adjacency) | x <- elems]

    nodeOf :: G.Vertex -> a
    nodeOf v = let (n, _, _) = fromVertex v in n

-- | A CAS registry number (e.g. @50-00-0@). A global substance anchor.
newtype CASNumber = CASNumber Text
    deriving (Eq, Ord, Show)

-- | An elementary-flow UUID. Globally unique, so also a global anchor.
newtype FlowUUID = FlowUUID UUID
    deriving (Eq, Ord, Show)

{- | A normalized flow name (the output of @SynonymDB.normalizeName@). A bare
name is only meaningful within the source that uses it, hence 'ByName' scopes it
by 'SourceId'.
-}
newtype NormName = NormName Text
    deriving (Eq, Ord, Show)

-- | A flow-definition source/version, e.g. @ecoinvent-3.11@, @ef-3.1-jrc@.
newtype SourceId = SourceId Text
    deriving (Eq, Ord, Show)

-- | A derived class identity (a representative member is chosen for display).
newtype ClassId = ClassId Int
    deriving (Eq, Ord, Show)

{- | How a source identifies a substance occurrence. CAS and UUID are /global/
anchors - the same value from any source is the same node, which is exactly what
makes the CAS bridge fall out of closure for free (two flows sharing a CAS share
the node, no edge needed). A bare name collides across sources, so 'ByName' is
scoped by its 'SourceId'.
-}
data SubstanceKey
    = ByCAS !CASNumber
    | ByUUID !FlowUUID
    | ByName !SourceId !NormName
    deriving (Eq, Ord, Show)

{- | A narrower flow's share of a broader one in a 'Subsumes' fan-out; the
siblings under one parent are expected to sum to ~1.
-}
newtype SplitWeight = SplitWeight Double
    deriving (Eq, Show)

-- | A unit/scale factor carried by a 'ProxyFor' stand-in.
newtype ConversionFactor = ConversionFactor Double
    deriving (Eq, Show)

{- | A typed relation between two substance anchors. Only 'SameAs' forms
equivalence classes; 'Subsumes' (broader ⊃ narrower, weighted) and 'ProxyFor'
(approximate, scaled) are a separate directional layer consumed at score time;
'DistinctFrom' is recorded negative evidence that blocks accidental closure.

The weight lives inside 'Subsumes' and the factor inside 'ProxyFor', so a
'SameAs' can never carry a scale - that would contradict "identical".
-}
data Relation
    = SameAs
    | Subsumes !SplitWeight
    | ProxyFor !ConversionFactor
    | DistinctFrom
    deriving (Eq, Show)

-- | One typed assertion relating two substance anchors.
data SubstanceEdge = SubstanceEdge
    { seFrom :: !SubstanceKey
    , seTo :: !SubstanceKey
    , seRelation :: !Relation
    }
    deriving (Eq, Show)

{- | Resolving a set of edges yields the @SameAs@-closure classes plus any
'DistinctFrom' the closure /violated/ (two keys declared distinct that some
@SameAs@ chain nonetheless merged). A contradicting edge set is a data bug to
fix, so conflicts are surfaced here, never silently resolved.
-}
data ClassResult = ClassResult
    { crClasses :: ![[SubstanceKey]]
    , crConflicts :: ![(SubstanceKey, SubstanceKey)]
    }
    deriving (Eq, Show)

{- | Resolve typed edges into substance classes. Classes are the transitive
closure of the @SameAs@ edges /only/; 'Subsumes' and 'ProxyFor' connect distinct
classes and are ignored here (a later score-time layer applies them). Each
'DistinctFrom' whose endpoints the @SameAs@ closure placed in one class is
reported in 'crConflicts'.
-}
classesFromEdges :: [SubstanceEdge] -> ClassResult
classesFromEdges edges = ClassResult classes conflicts
  where
    classes = equivalenceClasses [(seFrom e, seTo e) | e <- edges, seRelation e == SameAs]
    classOf = M.fromList [(k, i) | (i, ks) <- zip [0 :: Int ..] classes, k <- ks]
    conflicts =
        [ (a, b)
        | e <- edges
        , seRelation e == DistinctFrom
        , let a = seFrom e
        , let b = seTo e
        , Just ia <- [M.lookup a classOf]
        , Just ib <- [M.lookup b classOf]
        , ia == ib
        ]

{- | Name→CAS identities asserted by @SameAs@ edges that link a name anchor to
a CAS anchor (in either direction). CAS is a /global/ anchor, so the binding
applies to a flow of that name in any source - the edge's 'SourceId' is
provenance, not a gate (mirroring how the @ProxyFor@ fan-out matches names
globally). A name bound to two distinct CAS is a data conflict, returned in the
second component rather than silently resolved (the first wins, but the caller
is told).
-}
casBindingsFromEdges :: [SubstanceEdge] -> (Map NormName CASNumber, [(NormName, (CASNumber, CASNumber))])
casBindingsFromEdges edges = casBindings pairs
  where
    pairs =
        [ nc
        | e <- edges
        , seRelation e == SameAs
        , nc <- nameCasPair (seFrom e) (seTo e)
        ]
    nameCasPair (ByName _ n) (ByCAS c) = [(n, c)]
    nameCasPair (ByCAS c) (ByName _ n) = [(n, c)]
    nameCasPair _ _ = []

{- | Canonical spelling of a CAS registry number, so the same substance keys
identically whichever source stated it.

A CAS number is @registry-group-check@: a registry number of two to seven
digits, a two-digit group, and a single check digit. Only the registry number
is ever zero-padded - ecoinvent writes @001309-36-0@ where the canonical form
is @1309-36-0@ - so only its leading zeros come off. The other two segments are
fixed-width and keep theirs: formaldehyde is @50-00-0@, never @50-0-0@.

Anything that is not three dash-separated segments is passed through stripped;
this is a canonicalizer, not a validator.
-}
normalizeCAS :: Text -> Text
normalizeCAS cas = case T.splitOn "-" (T.strip cas) of
    [registry, group, check] ->
        let registry' = T.dropWhile (== '0') registry
         in (if T.null registry' then "0" else registry') <> "-" <> group <> "-" <> check
    _ -> T.strip cas

{- | 'normalizeCAS' for a field that may carry no CAS at all: 'Nothing' for an
empty string and for a placeholder made only of zeros and dashes, which several
sources write where they mean "unknown".
-}
nonEmptyCAS :: Text -> Maybe Text
nonEmptyCAS cas
    | T.null stripped = Nothing
    | T.all (\c -> c == '-' || c == '0') stripped = Nothing
    | otherwise = Just (normalizeCAS stripped)
  where
    stripped = T.strip cas

{- | The key a CAS anchors on, or 'Nothing' when the text states no CAS.

Both sides of the CAS bridge build their key through here, so a flow and a
method factor meet whichever source spelled the padding - which is the whole
point of canonicalizing, and does not happen if either side keys on the raw
string. A placeholder never becomes a key at all, so unrelated substances
cannot collide on one.
-}
casKey :: Text -> Maybe CASNumber
casKey = fmap CASNumber . nonEmptyCAS

{- | Fold name→CAS pairs into bindings under the registry's conflict rule: the
first binding of a name wins, and a later pair binding the same name to a
/different/ CAS comes back as a conflict for the caller to report - never
resolved silently.
-}
casBindings :: [(NormName, CASNumber)] -> (Map NormName CASNumber, [(NormName, (CASNumber, CASNumber))])
casBindings = foldl' (flip insert) (M.empty, [])
  where
    insert (n, c) (m, conflicts) =
        case M.lookup n m of
            Nothing -> (M.insert n c m, conflicts)
            Just c'
                | c' == c -> (m, conflicts)
                | otherwise -> (m, (n, (c', c)) : conflicts)

{- | The normalizers a CSV row's keys pass through, injected to keep this
module free of the synonym layer it would otherwise have to import. Only
'knName' still needs injecting for that reason; 'knCAS' is 'normalizeCAS',
which lives here, and is a parameter so a test can key edges by a stub without
building the real name normalizer.
-}
data KeyNormalizers = KeyNormalizers
    { knName :: Text -> NormName
    , knCAS :: Text -> CASNumber
    }

{- | Parse @substance_edges.csv@ into typed edges. Eight columns, with a header:

@
from_keytype,from_source,from_key,to_keytype,to_source,to_key,relation,scale
@

* @keytype@ ∈ @cas@ | @uuid@ | @name@. @cas@/@uuid@ are global anchors, so their
  @source@ column is an optional annotation (ignored); @name@ collides across
  sources, so its @source@ is required.
* @relation@ ∈ @sameas@ | @subsumes@ | @proxyfor@ | @distinctfrom@. @scale@ holds
  the 'Subsumes' split weight in @(0,1]@ or the non-zero 'ProxyFor' conversion
  factor, and must be empty for @sameas@/@distinctfrom@ - a scale on an identity
  would contradict it.

Keys pass through the injected 'KeyNormalizers' (names and CAS; injected to
avoid a module cycle). Every malformed row is surfaced as a @Left@ carrying its
line number - nothing is dropped.
-}
parseSubstanceEdges :: KeyNormalizers -> BL.ByteString -> Either Text [SubstanceEdge]
parseSubstanceEdges norms csvData =
    case decode HasHeader csvData of
        Left err -> Left (T.pack ("substance_edges.csv parse error: " <> err))
        Right rows ->
            traverse parseRow $
                zip [2 :: Int ..] (map V.toList (V.toList (rows :: V.Vector (V.Vector Text))))
  where
    parseRow (n, [fkt, fsrc, fkey, tkt, tsrc, tkey, rel, scale]) =
        SubstanceEdge
            <$> parseKey n fkt fsrc fkey
            <*> parseKey n tkt tsrc tkey
            <*> parseRelation n rel scale
    parseRow (n, fields) =
        Left (rowErr n ("expected 8 fields, got " <> T.pack (show (length fields))))

    parseKey n kt src key =
        case T.toLower (T.strip kt) of
            "cas" -> Right (ByCAS (knCAS norms (T.strip key)))
            "uuid" -> case fromText (T.strip key) of
                Just u -> Right (ByUUID (FlowUUID u))
                Nothing -> Left (rowErr n ("invalid UUID '" <> T.strip key <> "'"))
            "name"
                | T.null (T.strip src) -> Left (rowErr n "name key needs a source")
                | otherwise -> Right (ByName (SourceId (T.strip src)) (knName norms key))
            other -> Left (rowErr n ("unknown key type '" <> other <> "' (cas|uuid|name)"))

    parseRelation n rel scale =
        case (T.toLower (T.strip rel), T.strip scale) of
            ("sameas", "") -> Right SameAs
            ("sameas", _) -> Left (rowErr n "sameas takes no scale")
            ("distinctfrom", "") -> Right DistinctFrom
            ("distinctfrom", _) -> Left (rowErr n "distinctfrom takes no scale")
            ("subsumes", s) -> Subsumes . SplitWeight <$> parseWeight n s
            ("proxyfor", s) -> ProxyFor . ConversionFactor <$> parseFactor n s
            (other, _) -> Left (rowErr n ("unknown relation '" <> other <> "' (sameas|subsumes|proxyfor|distinctfrom)"))

    parseWeight n s = do
        w <- parseScale n "subsumes weight" s
        if w > 0 && w <= 1 then Right w else Left (rowErr n "subsumes weight must be in (0,1]")

    parseFactor n s = do
        f <- parseScale n "proxyfor factor" s
        if f /= 0 then Right f else Left (rowErr n "proxyfor factor must be non-zero")

    parseScale :: Int -> Text -> Text -> Either Text Double
    parseScale n what s
        | T.null s = Left (rowErr n (what <> " is required"))
        | otherwise = maybe (Left (rowErr n ("invalid " <> what <> " '" <> s <> "'"))) Right (readMaybe (T.unpack s))

    rowErr n msg = T.pack ("substance_edges.csv row " <> show n <> ": ") <> msg

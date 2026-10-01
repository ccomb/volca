{-# LANGUAGE OverloadedStrings #-}

{- | What changed between two method collections, factor by factor.

Both collections are values in memory, so a comparison is a pure function of
them and of the reference data that says which names are one substance.

* Factors pair in a cascade, each rung seeing only what the rungs before it
  left unpaired, and always at the same place: the same direction, the same
  compartment once its spelling is read through the compartment table, the
  same location. The rungs are, in order: the same flow identifier, which
  two releases of one package keep across a rename; the same name, a unit
  suffix kept, so the rows a method writes per kilogram and per cubic metre
  stay two; two names of one synonym class; the same CAS number, refused
  when the registry knows both names, since past the class rung two known
  names are two classes (one CAS covers fossil and biogenic methane, which
  the registry keeps apart); and, for pattern and exclusion rows, which
  select flows rather than name a substance, the same prefix. The name comes
  before the class because one category often writes several names of one
  class side by side (a flow and its fossil variant, a flow and its regional
  twin): on the class rung they would all answer one key and pair none,
  where each has its own name.
* The compartment is compared strictly: the table's fallback rows, which let
  a flow read a factor written for a broader place, are not followed, since
  they would make a precise subcompartment equal to an unspecified one. One
  reading is taken: a subcompartment written @unspecified@ is the whole
  medium, which is how another format writes it with an empty cell. So is
  another: an occupation or a transformation filed under the whole natural
  resource medium is in its land subcompartment. The direction is compared
  as written, except for a land factor, which is taken from nature however its
  file writes it.
* The location is the one a factor states, or else a code the geography
  table holds written at the end of its name, the way one format writes a
  regionalized factor (@Ammonia, FR@ is ammonia at @FR@).
* A key several factors answer to, on either side, pairs none of them.
* Categories pair first on the pairs the caller forces, then on the method
  name, then on the impact category, case and spacing aside. The impact unit
  does not decide, since the unit table does not know impact units. A name
  several categories answer to is listed as ambiguous, for the caller to
  settle with a forced pair.
* Two values are read per one unit before comparing: converted when both
  are flow units, per the reference unit of the flow unit's dimension when
  the other states anything else, as scoring reads it, and as written when
  neither is a flow unit; an empty unit, which one reader leaves when the
  file states none, counts as not a flow unit, and each row says which
  reading it took. A spelling the unit table cannot settle (two of its units
  differ from it only by case) is not read at all: the pair is listed as
  unconvertible. They are equal within a relative 1e-9.
-}
module Service.CompareMethods (
    CompareMethodsContext (..),
    ForcedPair (..),
    CollectionSide (..),
    CompareMethodsRefusal (..),
    parseForcedPair,
    refusalMessage,
    Scope (..),
    compareCollections,
    compareCategories,
    profileCollection,
    factorReading,
    limitMethodComparison,
) where

import Control.Monad (guard, mfilter)
import Data.Either (partitionEithers)
import Data.Foldable (toList, traverse_)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import API.Types (
    AmbiguousCategories (..),
    AmbiguousFactors (..),
    CategoryComparison (..),
    CategoryMatch (..),
    CategoryProfile (..),
    CategorySide (..),
    ChangedFactor (..),
    DuplicateFactors (..),
    FactorMatch (..),
    FactorReading (..),
    FactorSide (..),
    LocationSource (..),
    MediumCount (..),
    MethodCollectionComparison (..),
    MethodCollectionProfile (..),
    ReadLocation (..),
    UnconvertibleFactor (..),
    ValueReading (..),
 )
import Method.Mapping (isExclusionCF, isPatternCF, patternPrefix, viewFor)
import Method.Types (Compartment (..), CompartmentMap, FlowDirection (..), Location (..), Method (..), MethodCF (..), MethodCollection (..), normalizeCompartment)
import Service.Compare (Cascade (..), Rung (..), Sides (..), cascadeWith, close, pairOn, refusing)
import SubstanceRegistry (nonEmptyCAS)
import SynonymDB (SynonymDB, lookupSynonymGroup, normalizeNameKeepUnit)
import UnitConversion (UnitConfig, UnitReading (..), canonicalUnitFor, convertOntoFactorBasis, isKnownUnit, readUnit)

-- | The reference data a comparison reads names and units through.
data CompareMethodsContext = CompareMethodsContext
    { cmcSynonyms :: !SynonymDB
    , cmcCompartments :: !CompartmentMap
    , cmcUnits :: !UnitConfig
    , cmcLocations :: !(M.Map Location [Location])
    -- ^ The geography table: what tells a region written at the end of a name from the rest of the name.
    }

data CompartmentKey = CompartmentKey
    { ckMedium :: !Text
    , ckSub :: !Text
    , ckQualifier :: !Text
    }
    deriving (Eq, Ord)

data Place = Place
    { plDirection :: !FlowDirection
    , plCompartment :: !(Maybe CompartmentKey)
    , plLocation :: !(Maybe Text)
    }
    deriving (Eq, Ord)

data PatternKind = Pattern | Exclusion
    deriving (Eq, Ord)

data Substance
    = ByFlow !UUID
    | ByClass !Int
    | ByCAS !Text
    | ByName !Text
    | ByPrefix !PatternKind !Text
    deriving (Eq, Ord)

-- | The substance first: it tells most keys apart at once, where the place is shared by thousands.
data FactorKey = FactorKey !Substance !Place
    deriving (Eq, Ord)

-- | A pair the cascade made, and what comparing its values found.
data Judged = Judged
    { jMatch :: !FactorMatch
    , jPair :: !(Sides MethodCF)
    , jVerdict :: !Verdict
    }

-- | Two values read per one unit, and how they were read.
data Compared = Compared !ValueReading !(Sides Double)

-- | What comparing the two values of a paired factor found.
data Verdict
    = Same
    | Differs !ValueReading !(Maybe Double)
    | Unconvertible

-- | Two categories the caller pairs by name, base first.
data ForcedPair = ForcedPair
    { fpBase :: !Text
    , fpOther :: !Text
    }
    deriving (Eq, Show)

data CollectionSide = BaseCollection | OtherCollection
    deriving (Eq, Show)

-- | Why the forced pairs a caller gave cannot be taken.
data CompareMethodsRefusal
    = MalformedPair !Text
    | UnknownCategory !CollectionSide !Text
    | SeveralCategories !CollectionSide !Text
    | PairedTwice !CollectionSide !Text
    | NotPaired !Text
    | -- | A base category name several pairs start from, and the other category of each.
      SeveralPairs !Text ![Text]
    deriving (Eq, Show)

-- | Which pairs of categories a comparison compares.
data Scope
    = EveryCategory
    | -- | The one pair whose base category is so named, case and spacing aside: comparing a single pair costs a fraction of comparing them all.
      OneCategory !Text
    deriving (Eq, Show)

-- | A pair written @base=other@, each name trimmed.
parseForcedPair :: Text -> Either CompareMethodsRefusal ForcedPair
parseForcedPair written = case map T.strip (T.splitOn "=" written) of
    [b, o] | not (T.null b), not (T.null o) -> Right ForcedPair{fpBase = b, fpOther = o}
    _ -> Left (MalformedPair written)

refusalMessage :: CompareMethodsRefusal -> Text
refusalMessage r = case r of
    MalformedPair t -> "A pair is written base=other, one '=' between two names: " <> t
    UnknownCategory side t -> "No category of the " <> sideName side <> " collection is named " <> t
    SeveralCategories side t -> "Several categories of the " <> sideName side <> " collection are named " <> t <> "; this name cannot choose between them"
    PairedTwice side t -> "The category " <> t <> " of the " <> sideName side <> " collection is named in two pairs"
    NotPaired t -> "No pair of categories starts from a base category named " <> t
    SeveralPairs t others -> "Several pairs of categories start from a base category named " <> t <> ", against " <> T.intercalate ", " others <> "; this name cannot choose between them"
  where
    sideName :: CollectionSide -> Text
    sideName BaseCollection = "base"
    sideName OtherCollection = "other"

compareCollections :: CompareMethodsContext -> [ForcedPair] -> Scope -> Sides MethodCollection -> Either CompareMethodsRefusal MethodCollectionComparison
compareCollections ctx forced scope collections = do
    Taken{tkChosen = chosen, tkRest = rest} <- takeForced forced (fmap mcMethods collections)
    let paired = cascadeWith categoryRung [SameMethodName, SameImpactCategory] rest
    selected <- inScope scope ([(ForcedByCaller, pair) | pair <- chosen] ++ cPairs paired)
    pure
        MethodCollectionComparison
            { mccCategories = L.sortOn (Down . changes) (map (uncurry (compareCategories ctx)) selected)
            , mccUnpairedBase = map categorySide (baseSide (cUnpaired paired))
            , mccUnpairedOther = map categorySide (otherSide (cUnpaired paired))
            , mccAmbiguous =
                [ AmbiguousCategories
                    { acgMatch = m
                    , acgBase = map categorySide (NE.toList (baseSide cands))
                    , acgOther = map categorySide (NE.toList (otherSide cands))
                    }
                | (m, cands) <- cAmbiguous paired
                ]
            }
  where
    changes :: CategoryComparison -> Int
    changes c = ccpAddedCount c + ccpRemovedCount c + ccpChangedCount c

{- | What each category of a collection holds, read the way a comparison reads
it: the medium after the compartment table, the location stated or read at the
end of the name. Its duplicates are what comparing the category with itself
cannot pair, the keys several of its factors answer to.
-}
profileCollection :: CompareMethodsContext -> MethodCollection -> MethodCollectionProfile
profileCollection ctx = MethodCollectionProfile . map (profileCategory ctx) . mcMethods

profileCategory :: CompareMethodsContext -> Method -> CategoryProfile
profileCategory ctx m =
    CategoryProfile
        { cpfCategory = categorySide m
        , cpfMedia = [MediumCount medium n | (medium, n) <- M.toList (M.fromListWith (+) [(frMedium r, 1) | r <- readings])]
        , cpfLocatedCount = length located
        , cpfLocatedInNameCount = length (filter ((== InName) . rlFrom) located)
        , cpfLocationCount = S.size (S.fromList (map rlCode located))
        , cpfZeroCount = length (filter ((== 0) . mcfValue) cfs)
        , cpfPatternCount = length (filter (\cf -> isPatternCF cf || isExclusionCF cf) cfs)
        , cpfDuplicates = [DuplicateFactors{dfxMatch = afxMatch a, dfxFactors = afxBase a} | a <- ccpAmbiguous (compareCategories ctx SameMethodName (Sides m m))]
        }
  where
    cfs :: [MethodCF]
    cfs = methodFactors m
    readings :: [FactorReading]
    readings = map (factorReading (cmcCompartments ctx) (cmcLocations ctx)) cfs
    located :: [ReadLocation]
    located = mapMaybe frLocation readings

{- | A factor's medium and location, read as a comparison reads them, so a
profile, a comparison and a factor list never disagree on where a factor is.
-}
factorReading :: CompartmentMap -> M.Map Location [Location] -> MethodCF -> FactorReading
factorReading cmap locations cf =
    FactorReading
        { frMedium = ckMedium . compartmentKey cmap <$> mcfCompartment cf
        , frLocation = location
        }
  where
    Located _ location = locatedName locations cf

-- | The pairs a scope keeps, chosen before any is compared.
inScope :: Scope -> [(CategoryMatch, Sides Method)] -> Either CompareMethodsRefusal [(CategoryMatch, Sides Method)]
inScope EveryCategory pairs = Right pairs
inScope (OneCategory name) pairs = case filter ((== categoryKey name) . categoryKey . methodName . baseSide . snd) pairs of
    [pair] -> Right [pair]
    [] -> Left (NotPaired name)
    several@(_ : _ : _) -> Left (SeveralPairs name (map (methodName . otherSide . snd) several))

categoryRung :: CategoryMatch -> Sides [Method] -> Rung Method
categoryRung m = case m of
    -- Forced pairs are taken before the cascade runs, never by a rung.
    ForcedByCaller -> pairOn (const (Nothing :: Maybe Text))
    SameMethodName -> pairOn (Just . categoryKey . methodName)
    -- The ILCD reader writes "unknown" where the file states no impact category: that is no category to pair on.
    SameImpactCategory -> pairOn (fmap categoryKey . mfilter (/= "unknown") . Just . methodCategory)

-- | A category name without case and with its spacing collapsed.
categoryKey :: Text -> Text
categoryKey = T.toCaseFold . T.unwords . T.words

-- | The pairs the caller forced, and the categories left for the cascade.
data Taken = Taken
    { tkChosen :: ![Sides Method]
    , tkRest :: !(Sides [Method])
    }

-- | The forced pairs, taken out of both sides before the cascade sees them.
takeForced :: [ForcedPair] -> Sides [Method] -> Either CompareMethodsRefusal Taken
takeForced forced methods = do
    traverse_ (Left . PairedTwice BaseCollection) (repeated (map fpBase forced))
    traverse_ (Left . PairedTwice OtherCollection) (repeated (map fpOther forced))
    chosen <- traverse pick forced
    pure
        Taken
            { tkChosen = chosen
            , tkRest =
                Sides
                    { baseSide = unTaken (map fpBase forced) (baseSide methods)
                    , otherSide = unTaken (map fpOther forced) (otherSide methods)
                    }
            }
  where
    pick :: ForcedPair -> Either CompareMethodsRefusal (Sides Method)
    pick p = Sides <$> named BaseCollection (fpBase p) (baseSide methods) <*> named OtherCollection (fpOther p) (otherSide methods)
    named :: CollectionSide -> Text -> [Method] -> Either CompareMethodsRefusal Method
    named side name ms = case filter ((== categoryKey name) . categoryKey . methodName) ms of
        [m] -> Right m
        [] -> Left (UnknownCategory side name)
        (_ : _ : _) -> Left (SeveralCategories side name)
    -- 'pick' has made sure each forced name designates exactly one category, so
    -- dropping by name drops that one. Not by 'methodId': it is a UUID v5 of
    -- the name alone, which two categories of one name share.
    unTaken :: [Text] -> [Method] -> [Method]
    unTaken names = filter ((`notElem` map categoryKey names) . categoryKey . methodName)

-- | The first name, case and spacing aside, that a list holds twice, as written the second time.
repeated :: [Text] -> Maybe Text
repeated = go S.empty
  where
    go :: S.Set Text -> [Text] -> Maybe Text
    go _ [] = Nothing
    go seen (t : ts)
        | categoryKey t `S.member` seen = Just t
        | otherwise = go (S.insert (categoryKey t) seen) ts

-- | Keep the first @n@ factors of each list of each category. The counts still cover them all.
limitMethodComparison :: Int -> MethodCollectionComparison -> MethodCollectionComparison
limitMethodComparison n c = c{mccCategories = map limited (mccCategories c)}
  where
    limited :: CategoryComparison -> CategoryComparison
    limited p =
        p
            { ccpAdded = take n (ccpAdded p)
            , ccpRemoved = take n (ccpRemoved p)
            , ccpChanged = take n (ccpChanged p)
            , ccpAmbiguous = take n (ccpAmbiguous p)
            , ccpUnconvertible = take n (ccpUnconvertible p)
            }

compareCategories :: CompareMethodsContext -> CategoryMatch -> Sides Method -> CategoryComparison
compareCategories ctx match methods =
    CategoryComparison
        { ccpMatch = match
        , ccpBase = categorySide (baseSide methods)
        , ccpOther = categorySide (otherSide methods)
        , ccpAddedCount = length added
        , ccpRemovedCount = length removed
        , ccpChangedCount = length changed
        , ccpUnchangedCount = length [() | Judged{jVerdict = Same} <- judged]
        , ccpAmbiguousCount = length ambiguous
        , ccpUnconvertibleCount = length unconvertible
        , ccpLargestRatio = listToMaybe changed >>= cfxRatio
        , ccpAdded = added
        , ccpRemoved = removed
        , ccpChanged = changed
        , ccpAmbiguous = ambiguous
        , ccpUnconvertible = unconvertible
        }
  where
    paired :: Cascade FactorMatch Keyed
    paired = cascadeWith factorRung [minBound .. maxBound] (fmap (map (keyedFactor ctx) . methodFactors) methods)
    judged :: [Judged]
    judged =
        [ Judged{jMatch = m, jPair = pair, jVerdict = judge (cmcUnits ctx) pair}
        | (m, keyedPair) <- cPairs paired
        , let pair = fmap kFactor keyedPair
        ]
    changed :: [ChangedFactor]
    changed =
        L.sortOn
            (\c -> (Down (distance (cfxRatio c)), cfxBase c, cfxOther c))
            [ ChangedFactor
                { cfxMatch = m
                , cfxBase = factorSide (baseSide pair)
                , cfxOther = factorSide (otherSide pair)
                , cfxReading = reading
                , cfxRatio = ratio
                }
            | Judged{jMatch = m, jPair = pair, jVerdict = Differs reading ratio} <- judged
            ]
    unconvertible :: [UnconvertibleFactor]
    unconvertible =
        L.sortOn (\u -> (ufxBase u, ufxOther u)) $
            [ UnconvertibleFactor{ufxMatch = m, ufxBase = factorSide (baseSide pair), ufxOther = factorSide (otherSide pair)}
            | Judged{jMatch = m, jPair = pair, jVerdict = Unconvertible} <- judged
            ]
    ambiguous :: [AmbiguousFactors]
    ambiguous =
        L.sortOn afxBase $
            [ AmbiguousFactors{afxMatch = m, afxBase = sides (baseSide cands), afxOther = sides (otherSide cands)}
            | (m, cands) <- cAmbiguous paired
            ]
    added :: [FactorSide]
    added = sides (otherSide (cUnpaired paired))
    removed :: [FactorSide]
    removed = sides (baseSide (cUnpaired paired))
    sides :: (Foldable t) => t Keyed -> [FactorSide]
    -- Every list sorts on whole rows, so its order never depends on how the cascade keyed them.
    sides = L.sort . map (factorSide . kFactor) . toList

{- | How far a ratio is from no change. A sign flip, a factor that became
zero and a zero that became a value (no ratio) are the farthest of all, so a
factor that vanished and one that appeared both lead the list.
-}
distance :: Maybe Double -> Double
distance = maybe (1 / 0) far
  where
    far :: Double -> Double
    far r
        | r <= 0 = 1 / 0
        | otherwise = abs (log r)

{- | A factor with its key on each rung. The fields are lazy on purpose: a
key is worked out the first time a rung asks for it and never again, where a
rung keying the factor itself would redo the name and compartment reading on
every rung, and twice on each.
-}
data Keyed = Keyed
    { kFactor :: !MethodCF
    , kFlow :: Maybe FactorKey
    , kName :: Maybe FactorKey
    , kClass :: Maybe FactorKey
    , kCAS :: Maybe FactorKey
    , kPrefix :: Maybe FactorKey
    }

keyedFactor :: CompareMethodsContext -> MethodCF -> Keyed
keyedFactor ctx cf =
    Keyed
        { kFactor = cf
        , kFlow = ordinary (ByFlow <$> mfilter (/= UUID.nil) (Just (mcfFlowRef cf)))
        , kName = ordinary (Just (ByName (normalizeNameKeepUnit name)))
        , kClass = ordinary (ByClass <$> lookupSynonymGroup (viewFor (plDirection place) (cmcSynonyms ctx)) name)
        , kCAS = ordinary (ByCAS <$> (mcfCAS cf >>= nonEmptyCAS))
        , kPrefix = (`FactorKey` place) <$> byPrefix
        }
  where
    Located name location = locatedName (cmcLocations ctx) cf
    place :: Place
    place = placeOf (cmcCompartments ctx) (rlCode <$> location) cf
    -- Pattern and exclusion rows select flows rather than name a substance: only their prefix keys them.
    ordinary :: Maybe Substance -> Maybe FactorKey
    ordinary substance = guard (not (isPatternCF cf || isExclusionCF cf)) >> (`FactorKey` place) <$> substance
    byPrefix :: Maybe Substance
    byPrefix
        | isExclusionCF cf = Just (ByPrefix Exclusion (patternPrefix cf))
        | isPatternCF cf = Just (ByPrefix Pattern (patternPrefix cf))
        | otherwise = Nothing

factorRung :: FactorMatch -> Sides [Keyed] -> Rung Keyed
factorRung rung = case rung of
    SameFlowId -> pairOn kFlow
    SameName -> contestedByCAS . pairOn kName
    SameSynonymClass -> contestedByCAS . pairOn kClass
    SameCAS -> refusing bothKnown . pairOn kCAS
    SamePattern -> pairOn kPrefix
  where
    -- After the class rung, two names the registry knows at one place are two classes.
    bothKnown :: Sides Keyed -> Bool
    bothKnown (Sides b o) = isJust (kClass b) && isJust (kClass o)

{- | A pair a name made whose CAS numbers disagree, when either number names a
factor the rung left on the other side: paraquat written under the CAS of
paraquat dichloride, against a Paraquat and a Paraquat dichloride. The name
reads one pair and the CAS numbers another, so neither is taken, and the
group names every candidate. A disagreement no factor stands behind, one CAS
number written for another, keeps the pair its name made.
-}
contestedByCAS :: Rung Keyed -> Rung Keyed
contestedByCAS rung =
    rung
        { rPairs = kept
        , rAmbiguous = rAmbiguous rung ++ mapMaybe (traverse NE.nonEmpty) groups
        , rLeft = fmap (filter (\k -> not (any (holds k) groups))) (rLeft rung)
        }
  where
    kept :: [Sides Keyed]
    contested :: [Sides [Keyed]]
    (kept, contested) = partitionEithers (map contest (rPairs rung))
    contest :: Sides Keyed -> Either (Sides Keyed) (Sides [Keyed])
    contest pair@(Sides b o) = case (kCAS b, kCAS o) of
        (Just cb, Just co)
            | cb /= co
            , rivals <- Sides (holding co (baseSide (rLeft rung))) (holding cb (otherSide (rLeft rung)))
            , not (all null rivals) ->
                Right (Sides (b : baseSide rivals) (o : otherSide rivals))
        _ -> Left pair
    holding :: FactorKey -> [Keyed] -> [Keyed]
    holding cas = filter ((== Just cas) . kCAS)
    -- Two contested pairs of one place can claim one rival: they are one group.
    groups :: [Sides [Keyed]]
    groups = L.foldl' join [] contested
    join :: [Sides [Keyed]] -> Sides [Keyed] -> [Sides [Keyed]]
    join acc g = let (touching, rest) = L.partition (\h -> any (`holds` h) (concat g)) acc in rest ++ [L.foldl' union g touching]
    holds :: Keyed -> Sides [Keyed] -> Bool
    holds k = any (sameFactor k) . concat
    union :: Sides [Keyed] -> Sides [Keyed] -> Sides [Keyed]
    union (Sides b1 o1) (Sides b2 o2) = Sides (L.unionBy sameFactor b1 b2) (L.unionBy sameFactor o1 o2)
    sameFactor :: Keyed -> Keyed -> Bool
    sameFactor x y = kFactor x == kFactor y

{- | The substance a factor names and the location it is written for. One
format writes the region in a field, another at the end of the name
(@Ammonia, FR@): a factor with no location whose name ends in a code the
geography table holds is that substance at that code, a code with a comma of
its own included (@Water, Europe, Western@). A name that ends in no code
(@Methane, fossil@), or in two the table holds, stays whole.
-}
locatedName :: M.Map Location [Location] -> MethodCF -> Located
locatedName locations cf = case (mcfConsumerLocation cf, readings) of
    (Nothing, [reading]) -> reading
    (location, _) -> Located (mcfFlowName cf) ((`ReadLocation` InField) <$> location)
  where
    readings :: [Located]
    readings =
        [ Located substance (Just (ReadLocation code InName))
        | (substance, rest) <- T.breakOnAll ", " (mcfFlowName cf)
        , not (T.null substance)
        , let code = T.drop 2 rest
        , M.member (Location code) locations
        ]

-- | A substance's name and the location a factor is written for.
data Located = Located !Text !(Maybe ReadLocation)

placeOf :: CompartmentMap -> Maybe Text -> MethodCF -> Place
placeOf cmap location cf =
    Place
        { plDirection = direction
        , plCompartment = compartment
        , plLocation = location
        }
  where
    compartment :: Maybe CompartmentKey
    compartment = landRead cf . compartmentKey cmap <$> mcfCompartment cf
    -- One format writes a transformation to a land type as an output to land,
    -- another as an input from nature: a land flow has one direction.
    direction :: FlowDirection
    direction
        | any isLand compartment = Input
        | otherwise = mcfDirection cf
    isLand :: CompartmentKey -> Bool
    isLand key = ckMedium key == "natural resource" && ckSub key == "land"

{- | One format files a land flow in the land subcompartment of the natural
resource medium, another under the whole medium (@Occupation, forest,
extensive@ with no subcompartment). An occupation or a transformation can
only be of land, so that flow is read in the land subcompartment.
-}
landRead :: MethodCF -> CompartmentKey -> CompartmentKey
landRead cf key
    | ckMedium key == "natural resource"
    , T.null (ckSub key)
    , any (`T.isPrefixOf` T.toCaseFold (mcfFlowName cf)) ["occupation, ", "transformation, "] =
        key{ckSub = "land"}
    | otherwise = key

compartmentKey :: CompartmentMap -> Compartment -> CompartmentKey
compartmentKey cmap c =
    CompartmentKey{ckMedium = folded medium, ckSub = wholeMedium (folded sub), ckQualifier = folded qualifier}
  where
    medium :: Text
    sub :: Text
    qualifier :: Text
    Compartment medium sub qualifier = normalizeCompartment cmap c
    folded :: Text -> Text
    folded = T.toCaseFold . T.strip
    wholeMedium :: Text -> Text
    wholeMedium s
        | s `elem` ["unspecified", "(unspecified)"] = T.empty
        | otherwise = s

judge :: UnitConfig -> Sides MethodCF -> Verdict
judge cfg pair = maybe Unconvertible verdictOn (comparedValues cfg pair)
  where
    verdictOn :: Compared -> Verdict
    verdictOn (Compared reading (Sides b o))
        | close b o = Same
        | b == 0 = Differs reading Nothing
        | otherwise = Differs reading (Just (o / b))

{- | The two values read per one unit, and how. 'Nothing' when both are flow
units that do not convert, or when a unit is a spelling the table cannot
settle.
-}
comparedValues :: UnitConfig -> Sides MethodCF -> Maybe Compared
comparedValues cfg (Sides b o)
    | ub == uo = Just (Compared UnitsIdentical (Sides vb vo))
    | unsettled ub || unsettled uo = Nothing
    | known ub && known uo = (\f -> Compared ConvertedOntoBaseUnit (Sides vb (vo * f))) <$> perOne ub uo
    | known ub = (\f -> Compared ReadPerReferenceUnit (Sides (vb * f) vo)) <$> perReference ub
    | known uo = (\f -> Compared ReadPerReferenceUnit (Sides vb (vo * f))) <$> perReference uo
    | otherwise = Just (Compared ComparedAsWritten (Sides vb vo))
  where
    ub :: Text
    ub = mcfUnit b
    uo :: Text
    uo = mcfUnit o
    vb :: Double
    vb = mcfValue b
    vo :: Double
    vo = mcfValue o
    known :: Text -> Bool
    known = isKnownUnit cfg
    unsettled :: Text -> Bool
    unsettled u = case readUnit cfg u of
        ReadAmbiguous{} -> True
        ReadExact{} -> False
        ReadRespelt{} -> False
        ReadUnknown -> False
    -- How many @from@ one @to@ holds: a factor per @from@ times this is the factor per @to@.
    perOne :: Text -> Text -> Maybe Double
    perOne to from = convertOntoFactorBasis cfg to from 1
    perReference :: Text -> Maybe Double
    perReference u = canonicalUnitFor cfg u >>= \ref -> perOne ref u

categorySide :: Method -> CategorySide
categorySide m =
    CategorySide
        { csdName = methodName m
        , csdCategory = methodCategory m
        , csdUnit = methodUnit m
        , csdFactorCount = length (methodFactors m)
        }

factorSide :: MethodCF -> FactorSide
factorSide cf =
    FactorSide
        { facFlowName = mcfFlowName cf
        , facDirection = mcfDirection cf
        , facCompartment = maybe T.empty path (mcfCompartment cf)
        , facCas = mcfCAS cf
        , facLocation = mcfConsumerLocation cf
        , facUnit = mfilter (not . T.null) (Just (mcfUnit cf))
        , facValue = mcfValue cf
        }
  where
    path :: Compartment -> Text
    path (Compartment medium sub qualifier) = T.intercalate "/" (filter (not . T.null) [medium, sub, qualifier])

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Database where

import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Database.MatrixBuild
import Progress
import qualified Search.BM25.Types as BM25T
import qualified Search.Fuzzy as Fuzzy
import qualified Search.Normalize as Normalize
import Types

{- | Build complete database with pre-computed sparse matrices

SIGN CONVENTION:
- Technosphere triplets are stored as POSITIVE values (input coefficients per unit output)
- Matrix.hs negates these when constructing (I-A) system matrix for solving
- The biosphere matrix stores ALL flows as POSITIVE (emissions AND resource extractions)
- Resource extractions represent "outputs" from nature into the technosphere (positive like emissions)
- This follows Ecoinvent convention where B matrix contains positive values for all environmental flows

Matrix Construction:
- Accepts a Map with (UUID, UUID) keys and converts to Vector internally
- Builds sparse triplets for technosphere (A) and biosphere (B) matrices
- Normalizes exchanges by NET reference product amounts (gross output - internal consumption)
- SELF-LOOPS (internal consumption) are EXCLUDED from matrix triplets but affect normalization
- Example: Electricity market with 1.0 kWh output and 0.012 kWh internal loss
  * Normalization factor: 1.0 - 0.012 = 0.988 kWh (net output)
  * All inputs divided by 0.988, giving ~1.2% increase in coefficients
  * Self-loop NOT exported as matrix entry (matches Ecoinvent convention)
- Solver constructs (I-A) by adding identity and negating technosphere triplets
-}
buildDatabaseWithMatrices :: BuildInputs -> SimpleDatabase -> IO (Either Text Database)
buildDatabaseWithMatrices inputs SimpleDatabase{sdbActivities = activityMap, sdbTechFlows = techFlowDB, sdbBioFlows = bioFlowDB, sdbWasteFlows = wasteFlowDB, sdbUnits = unitDB} = do
    reportMatrixOperation "Building database with pre-computed sparse matrices"
    let !tables = buildInterningTables activityMap
        !supplierRefUnits = buildSupplierRefUnits unitDB (itActivities tables)
        !indexes = buildIndexesWithProcessIds (itActivities tables) (itProcessIdTable tables)
        activityCount = itActivityCount tables

    reportMatrixOperation ("Activity index built: " ++ show activityCount ++ " activities")
    reportMatrixOperation "Building technosphere matrix triplets"
    case buildTechTriples (biUnitConfig inputs) unitDB tables supplierRefUnits of
        Left err -> pure (Left err)
        Right (techTriples, techWarnings) -> do
            mapM_ (reportProgress Warning) techWarnings
            reportMatrixOperation ("Technosphere matrix: " ++ show (VU.length techTriples) ++ " non-zero entries")

            reportMatrixOperation "Building biosphere matrix triplets"
            let !bioFlowUUIDs = collectBioFlowOrder (itActivities tables)
                !bioTriples = buildBioTriples bioFlowUUIDs tables
                bioFlowCount = fromIntegral (V.length bioFlowUUIDs)

            reportMatrixOperation ("Biosphere matrix: " ++ show (VU.length bioTriples) ++ " non-zero entries")
            reportMatrixOperation "Database with matrices built successfully"
            reportMatrixOperation
                ( "Final matrix stats: "
                    ++ show (VU.length techTriples)
                    ++ " tech entries, "
                    ++ show (VU.length bioTriples)
                    ++ " bio entries"
                )

            reportMatrixOperation "Building product index"
            let !productIndex = buildProductIndex (itActivities tables) (itProcessIdTable tables) techFlowDB
            reportMatrixOperation ("Product index: " ++ show (M.size (piByUUID productIndex)) ++ " products indexed")

            pure $
                Right
                    Database
                        { dbProcessIdTable = itProcessIdTable tables
                        , dbProcessIdLookup = itProcessIdLookup tables
                        , dbActivityUUIDIndex = itActivityUUIDIndex tables
                        , dbProductIndex = productIndex
                        , dbActivities = itActivities tables
                        , dbTechFlows = techFlowDB
                        , dbBioFlows = bioFlowDB
                        , dbWasteFlows = wasteFlowDB
                        , dbUnits = unitDB
                        , dbIndexes = indexes
                        , dbTechnosphereTriples = techTriples
                        , dbBiosphereTriples = bioTriples
                        , dbActivityIndex = V.generate (fromIntegral activityCount) fromIntegral
                        , dbBiosphereOrder = bioFlowUUIDs
                        , dbActivityCount = activityCount
                        , dbBiosphereCount = bioFlowCount
                        , dbCrossDBLinks = []
                        , dbDependsOn = []
                        , dbLinkingStats = mempty
                        , dbBuiltWith = inputs
                        , dbSynonymDB = Nothing
                        , dbFlowsByName = M.empty
                        , dbFlowsByCAS = M.empty
                        , dbProductSearchIndex = M.empty
                        , dbBM25Index = Nothing
                        }

{- | Build activity-level indexes (name / location / flow / unit). Flow-side
taxonomy lives on activities or biosphere compartments and is queried via
the flow databases directly, so no separate flow index is built here.
-}
buildIndexesWithProcessIds :: V.Vector Activity -> V.Vector (UUID, UUID) -> Indexes
buildIndexesWithProcessIds activityVec processIdTable =
    let
        activityUUIDs = [actUUID | (actUUID, _) <- V.toList processIdTable]
        activities = V.toList activityVec
        activityPairs = zip activityUUIDs activities
        -- The process id table is in row order, so the row of each activity is
        -- its position in it.
        rows = zip [0 ..] activities

        nameIdx =
            M.fromListWith
                (++)
                [(T.toLower (activityName activity), [uuid]) | (uuid, activity) <- activityPairs]

        locationIdx =
            M.fromListWith
                (++)
                [(activityLocation activity, [uuid]) | (uuid, activity) <- activityPairs]

        flowIdx =
            M.fromListWith
                (++)
                [ (exchangeFlowId ex, [pid]) | (pid, activity) <- rows, ex <- exchanges activity
                ]

        unitIdx =
            M.fromListWith
                (++)
                [(activityUnit activity, [uuid]) | (uuid, activity) <- activityPairs]
     in
        Indexes
            { idxByName = nameIdx
            , idxByLocation = locationIdx
            , idxByFlow = flowIdx
            , idxByUnit = unitIdx
            }

{- | Build ProductIndex for product-based lookups
Used for: (1) SimaPro upstream link resolution, (2) future product search
Maps product flow UUIDs and names to the activities that produce them
-}
buildProductIndex :: V.Vector Activity -> V.Vector (UUID, UUID) -> TechFlowDB -> ProductIndex
buildProductIndex activities processIdTable techFlowDb =
    let
        -- Build list of (ProcessId, productUUID, productName, location) for each activity
        entries =
            [ (pid, prodUUID, prodName, actLoc)
            | (pid, (_, prodUUID)) <- zip [0 ..] (V.toList processIdTable)
            , let act = activities V.! fromIntegral pid
            , let actLoc = activityLocation act
            , Just flow <- [M.lookup prodUUID techFlowDb]
            , let prodName = T.toLower (tfName flow)
            ]
     in
        ProductIndex
            { -- One flow, every row producing it: several is the ordinary shape of a
              -- product made in more than one geography, and 'M.fromList' would
              -- have kept whichever row came last.
              piByUUID = M.fromListWith (flip (<>)) [(prodUUID, pid :| []) | (pid, prodUUID, _, _) <- entries]
            , piByName = M.fromListWith (++) [(name, [pid]) | (pid, _, name, _) <- entries]
            , piByLocation = M.fromListWith (++) [(loc, [pid]) | (pid, _, _, loc) <- entries, not (T.null loc)]
            }

-- | Multi-word AND match: all words must appear in at least one of the given text fields (substring).
allWordsMatch :: Text -> (Activity -> [Text]) -> Activity -> Bool
allWordsMatch query getFields a =
    let searchWords = filter (not . T.null) $ T.words (T.toLower query)
        fields = map T.toLower (getFields a)
     in all (\w -> any (T.isInfixOf w) fields) searchWords

-- | Resolve a set of indices to (ProcessId, Activity) pairs against the activity vector.
resolveActivityIds :: V.Vector Activity -> IS.IntSet -> [(ProcessId, Activity)]
resolveActivityIds actVec ids =
    [(fromIntegral i, actVec V.! i) | i <- IS.toList ids, i < V.length actVec]

{- | Name-only candidate lookup. Does NOT touch geo/product/classification.
Non-exact path routes through the BM25 vocabulary + fuzzy expansion so
typos and stems still retrieve activities. Exact path is a linear scan
for case-insensitive full-name equality.
-}
findActivityNameCandidates :: Database -> Maybe Text -> Bool -> [(ProcessId, Activity)]
findActivityNameCandidates db Nothing _ = allActivities (dbActivities db)
findActivityNameCandidates db (Just name) True = exactNameMatches (dbActivities db) name
findActivityNameCandidates db (Just name) False =
    case dbBM25Index db of
        Just idx -> resolveActivityIds (dbActivities db) (bm25DocsMatchingName idx name)
        Nothing -> fullScanNameMatches (dbActivities db) name

allActivities :: V.Vector Activity -> [(ProcessId, Activity)]
allActivities actVec =
    [(fromIntegral i, a) | (i, a) <- zip [(0 :: Int) ..] (V.toList actVec)]

exactNameMatches :: V.Vector Activity -> Text -> [(ProcessId, Activity)]
exactNameMatches actVec name =
    [pair | pair@(_, a) <- allActivities actVec, T.toCaseFold (activityName a) == nameFold]
  where
    nameFold = T.toCaseFold name

fullScanNameMatches :: V.Vector Activity -> Text -> [(ProcessId, Activity)]
fullScanNameMatches actVec name =
    [pair | pair@(_, a) <- allActivities actVec, allWordsMatch name (\a' -> [activityName a']) a]

{- | How much of an identifier a query is allowed to be. A caller asking for an
exact name match asked for equality, so it gets the blocks its query is the
whole identifier of and no others; the ordinary caller also gets the ones it is
only a part of, because nobody types an identifier whole.
-}
data IdentifierReach = WholeIdentifier | WholeOrFragment
    deriving (Eq, Show)

{- | The rows of the source blocks a query names by their identifier, the ones
it names outright before the ones it only names part of. Which formats publish
an identifier, and which have nothing to publish because they name a dataset by
the activity UUID itself, is 'activityNativeId'.

An identifier is a key, not a word, so this is equality and containment and
never the fuzzy matching a word gets: the codes of one database differ by a
digit, and a matcher tolerant of a digit would answer with the neighbours of
the dataset that was asked for.

Nobody types one whole. Measured on the SimaPro export of ecoinvent 3.9.1, all
21 456 identifiers are 23 characters of which the first 18 are the same on
every one of them, and the last five are already unique across the export; the
WFLDB and Ginko exports have the same shape. So a fragment is what a reader has
in hand, and 'shortestFragment' is where a fragment starts to name something.

A fragment taken from the part every identifier shares (@MTE0@, @0000@) names
them all, which is the honest answer to a fragment that distinguishes nothing.
It costs the order of a page and not the answer, because a caller puts these
rows before an ordinary search rather than instead of it.

Every product of a block carries the block's identifier, so the answer is the
whole block, which is what someone holding a source file open is looking for.
-}
activitiesIdentifiedBy :: IdentifierReach -> V.Vector Activity -> Text -> [(ProcessId, Activity)]
activitiesIdentifiedBy reach actVec query
    | T.null wanted = []
    | otherwise = case reach of
        WholeIdentifier -> named (== wanted)
        WholeOrFragment
            | T.length wanted < shortestFragment -> named (== wanted)
            | otherwise -> named (== wanted) ++ named containsWanted
  where
    wanted :: Text
    wanted = T.toCaseFold (T.strip query)

    containsWanted :: Text -> Bool
    containsWanted nativeId = wanted `T.isInfixOf` nativeId && nativeId /= wanted

    named :: (Text -> Bool) -> [(ProcessId, Activity)]
    named matches = [pair | pair@(_, a) <- allActivities actVec, maybe False matches (identifierOf a)]

    identifierOf :: Activity -> Maybe Text
    identifierOf a = (\(NativeProcessId nativeId) -> T.toCaseFold nativeId) <$> activityNativeId a

{- | How many characters a query needs before it is read as a fragment of an
identifier rather than as a word.

Measured on the two SimaPro exports 'activitiesIdentifiedBy' cites: below four
characters nothing is ever selective, a three-character fragment naming 41
datasets at the median and never a single one. At four, nine fragments in ten
name exactly one dataset on one export and eight in ten name at most five on
the other.
-}
shortestFragment :: Int
shortestFragment = 4

{- | Docs whose BM25 postings cover every query token (AND), allowing any
fuzzy expansion of a token to satisfy that token (OR within a token).
-}
bm25DocsMatchingName :: BM25T.BM25Index -> Text -> IS.IntSet
bm25DocsMatchingName idx name =
    intersectAll (map docsForGroup groups)
  where
    groups = Fuzzy.expandTokensGrouped idx (Normalize.tokenize name)
    docsForGroup g = IS.unions [docsForToken t | (t, _) <- g]
    docsForToken t = case M.lookup t (BM25T.bm25Postings idx) of
        Nothing -> IS.empty
        Just postings -> IS.fromList [docId | (docId, _) <- VU.toList postings]
    intersectAll [] = IS.empty
    intersectAll (x : xs) = foldl IS.intersection x xs

{- | Does a location answer a geography filter?

A location is a code from a controlled vocabulary rather than free text, and those
codes overlap as text: @SE@ sits inside @US-SERC@, @DE@ inside @NORDEL@, and @CH@
inside @RER w/o CH+DE@, a region defined by excluding Switzerland. Matching anywhere
in the string answers all three with places nobody asked for, and the answer carries
nothing that says so.

So a location answers when it is the geography asked for, or one written under it:
@US@ answers for @US-WECC@ and @Europe@ for @Europe without Switzerland@, which is
what keeps a filter typed one letter at a time useful, and leaves no way for a code
to match inside an unrelated one.
-}
locationAnswers :: Text -> Text -> Bool
locationAnswers wanted location =
    T.toCaseFold (T.strip wanted) `T.isPrefixOf` T.toCaseFold (T.strip location)

{- | Apply geo, product, and classification filters to a pre-built candidate list.
Does NOT touch the name query: callers (BM25 retrieval or name-candidate lookup)
produce the initial list.
-}
applyStructuredFilters ::
    Database ->
    -- | geo
    Maybe Text ->
    -- | product
    Maybe Text ->
    -- | classification filters
    [(Text, Text, Bool)] ->
    -- | exactMatch (geo and product filters become case-insensitive equality)
    Bool ->
    [(ProcessId, Activity)] ->
    [(ProcessId, Activity)]
applyStructuredFilters db geoParam productParam classFilters exactMatch candidates =
    let actVec = dbActivities db
        pidx = dbProductSearchIndex db

        -- geography
        geoFiltered = case geoParam of
            Nothing -> candidates
            Just geo
                | exactMatch ->
                    let geoFold = T.toCaseFold geo
                     in [(pid, a) | (pid, a) <- candidates, T.toCaseFold (activityLocation a) == geoFold]
                | otherwise ->
                    [(pid, a) | (pid, a) <- candidates, locationAnswers geo (activityLocation a)]

        -- exchangeIsReference covers both ReferenceProduct (output) and
        -- ReferenceInput (treatment-process input). Both are the activity's
        -- reference product for search purposes; excluding ReferenceInput
        -- here used to hide waste-treatment activities from product filters.
        getProductNames a' =
            [ tfName flow
            | ex <- exchanges a'
            , exchangeIsReference ex
            , Just flow <- [M.lookup (exchangeFlowId ex) (dbTechFlows db)]
            ]

        productFiltered = case productParam of
            Nothing -> geoFiltered
            Just prod
                | exactMatch ->
                    let prodFold = T.toCaseFold prod
                     in [(pid, a) | (pid, a) <- geoFiltered, any ((== prodFold) . T.toCaseFold) (getProductNames a)]
                | M.null pidx ->
                    [(pid, a) | (pid, a) <- geoFiltered, allWordsMatch prod getProductNames a]
                | otherwise ->
                    let searchWords = filter (not . T.null) $ T.words (T.toLower prod)
                        wordCandidates w = IS.unions [ids | (key, ids) <- M.toList pidx, T.isInfixOf w key]
                        candidateSet = case map wordCandidates searchWords of
                            [] -> IS.fromList (map (fromIntegral . fst) geoFiltered)
                            (first : rest) -> foldl IS.intersection first rest
                        geoSet = IS.fromList (map (fromIntegral . fst) geoFiltered)
                        hitSet = IS.intersection candidateSet geoSet
                        hitPairs = resolveActivityIds actVec hitSet
                        hitPids = IS.fromList (map (fromIntegral . fst) hitPairs)
                     in -- Preserve the original order of geoFiltered (BM25 score order when BM25-driven).
                        [ (pid, a)
                        | (pid, a) <- geoFiltered
                        , IS.member (fromIntegral pid) hitPids
                        , allWordsMatch prod getProductNames a
                        ]

        classFiltered =
            let groups = M.fromListWith (++) [(sys, [(val, isExact)]) | (sys, val, isExact) <- classFilters]
                matchOne v (q, isExact) =
                    if isExact
                        then T.toLower q == T.toLower v
                        else T.isInfixOf (T.toLower q) (T.toLower v)
                applyGroup acc (sys, pairs) =
                    [ (pid, a)
                    | (pid, a) <- acc
                    , case M.lookup sys (activityClassification a) of
                        Just v -> any (matchOne v) pairs
                        Nothing -> False
                    ]
             in foldl applyGroup productFiltered (M.toList groups)
     in classFiltered

{- | Search activities by multiple fields (name, geography, product, classification).
Non-BM25 path: name filter is substring AND-of-tokens on activity name only.
Returns (ProcessId, Activity) pairs so callers don't need to re-scan for ProcessId.
-}
findActivitiesByFields :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Text, Bool)] -> Bool -> [(ProcessId, Activity)]
findActivitiesByFields db nameParam geoParam productParam classFilters exactMatch =
    applyStructuredFilters
        db
        geoParam
        productParam
        classFilters
        exactMatch
        (findActivityNameCandidates db nameParam exactMatch)

{- | Search flows by synonym across the technosphere, biosphere, and waste
maps. Result tagged with the flow kind so consumers can render the
appropriate shape via the 'flowKind*' projections in "API.Types".
-}
findFlowsBySynonym :: Database -> Text -> [FlowKind]
findFlowsBySynonym db query =
    filter (flowMatchesQuery query) (allFlows (dbTechFlows db) (dbBioFlows db) (dbWasteFlows db))

-- | Every flow the three maps hold, each tagged with the kind it came from.
allFlows :: TechFlowDB -> BioFlowDB -> WasteFlowDB -> [FlowKind]
allFlows tech bio waste =
    map TechKind (M.elems tech)
        ++ map BioKind (M.elems bio)
        ++ map WasteKind (M.elems waste)

{- | The text a flow answers a search on: its name, then every synonym it
carries. Different words of one query may land in different fields, since a
chemical is often searched by its trade name and its compartment at once.
-}
flowSearchFields :: FlowKind -> [Text]
flowSearchFields flow =
    flowKindName flow : concatMap S.toList (M.elems (flowKindSynonyms flow))

{- | A flow matches when every word of the query appears in one of its
searchable fields, case-blind and in any order ('Normalize.matchesEveryWord'
holds the rule, so a filter elsewhere answers the same query the same way).

The width that word-by-word matching brings is paid for by
'flowNameRelevance', which puts the flow the user actually typed at the top.
-}
flowMatchesQuery :: Text -> FlowKind -> Bool
flowMatchesQuery query = Normalize.matchesEveryWord query . flowSearchFields

{- | What a name filter keeps of the candidates it was given, each described
by its searchable fields, its own name first ('flowSearchFields' already
orders them that way).

A filter is not a search, and differs on two points. A query naming no word
at all (blank, or punctuation only) filters nothing, where a search for
nothing finds nothing: an argument that names nothing must not empty the
answer. And of everything the query matched, only the closest tier is kept
('flowNameRelevance'): a search relegates a lookalike to a later page, a
filter has no later page, so keeping every tier would mix
@Carbon dioxide, fossil@ and @Carbon dioxide, non-fossil@ into one answer and
leave the caller to notice. Asking for the flow as it is written therefore
returns exactly it, and dropping its punctuation returns the tier that
carries all its words.
-}
filterByName :: Text -> (a -> [Text]) -> [a] -> [a]
filterByName query fields xs
    | null (Normalize.queryWords query) = xs
    | otherwise = [x | (x, tier) <- ranked, tier == closest]
  where
    matches = Normalize.matchesEveryWord query
    ranked = [(x, flowNameRelevance query (nameOf (fields x))) | x <- xs, matches (fields x)]
    closest = case map snd ranked of
        [] -> 0 -- nothing matched, so the comprehension above is empty too
        tier : tiers -> foldr min tier tiers
    nameOf fs = case fs of
        [] -> ""
        name : _ -> name

{- | How closely a flow's name answers the query, smallest first: it carries
the query as it was typed, or it carries every word, or it does neither and
was reached some other way (through a synonym, or with the words scattered
over name and synonyms).

Matching word by word widens what comes back a lot, and the result list is
otherwise alphabetical, so on a real database @oil, crude@ pushed the flow
named @Oil, crude@ hundreds of rows down a list an assistant only ever sees
the first page of. Ranking is what keeps the width from costing the answer.
-}
flowNameRelevance :: Text -> Text -> Int
flowNameRelevance query name
    | T.toCaseFold (T.strip query) `T.isInfixOf` folded = 0
    | all ((`T.isInfixOf` folded) . T.toCaseFold) (Normalize.queryWords query) = 1
    | otherwise = 2
  where
    folded = T.toCaseFold name

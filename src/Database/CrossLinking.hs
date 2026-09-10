{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- | Cross-Database Activity Linking

When loading databases that depend on other databases (e.g., Ginko 2025 depends on
Agribalyse 3.2), this module provides functions to resolve supplier references
by searching across all loaded databases.

The matching algorithm uses:
1. The supplier activity the source names, where it names one apart from the
   product (Brightway Excel does; SimaPro folds it into the product name and
   EcoSpold 2 gives a UUID)
2. Product name matching (exact → synonym)
3. Location matching with hierarchy fallback
4. Unit compatibility checking

A candidate must score above a threshold to be automatically linked. Several
candidates of one database at the same score answer nothing, so the link
carries an 'AmbiguousSupplier' warning saying how many tied.

Performance: Uses pre-built indexes for O(1) product name lookup instead of
O(n) linear scans.
-}
module Database.CrossLinking (
    -- * Types
    LinkingContext (..),
    CrossDBCandidate (..),
    CrossDBLinkResult (..),
    LinkWarning (..),
    LinkBlocker (..),
    GeographyPolicy (..),
    LocationKind (..),
    IndexedDatabase (..),
    SupplierEntry (..),
    SupplierQuery (..),

    -- * Supplier aliases (relink mapping)
    AliasKey (..),
    AliasTarget (..),
    AliasMap (..),
    emptyAliasMap,
    lookupAlias,

    -- * Configuration
    defaultLinkingThreshold,

    -- * Index Building
    buildIndexedDatabase,
    buildIndexedDatabaseFromDB,
    buildSupplierEntries,
    supplierLocations,

    -- * Main Functions
    findSupplierInIndexedDBs,
    findSupplierByActivityProduct,
    findWasteTreatmentAcrossDatabases,
    findWasteTreatmentByActivity,
    WasteTreatmentMatch (..),

    -- * Scoring Functions
    matchProductName,
    matchLocation,
    acceptableLocation,

    -- * Location Hierarchy
    isSubregionOf,
    locationHierarchy,

    -- * Compound Name Parsing
    extractBracketedLocation,

    -- * Text Normalization
    normalizeText,
    normalizeUnicode,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Char (isUpper)
import Data.Foldable (find)
import Data.List (maximumBy, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import qualified Data.Vector as V
import Method.Types (Location (..))
import SynonymDB (SynonymDB, lookupSynonymGroup, normalizeName)
import Types (
    Activity (..),
    Database (..),
    Exchange,
    GeographyPolicy (..),
    LinkBlocker (..),
    LocationKind (..),
    SimpleDatabase (..),
    TechnosphereFlow (..),
    WasteFlow (..),
    activityNormFactor,
    exchangeFlowId,
    exchangeIsReference,
    exchangeLocation,
    exchanges,
    getActivity,
 )
import qualified UnitConversion as UC

{- | Pre-indexed database for fast cross-DB supplier lookup
Built once when database is loaded, reused for all lookups
-}
data IndexedDatabase = IndexedDatabase
    { idbName :: !Text
    , idbByProductName :: !(M.Map Text [SupplierEntry]) -- Normalized product name → suppliers
    , idbBySynonymGroup :: !(M.Map Int [SupplierEntry]) -- Synonym group ID → suppliers
    , idbWasteTreatmentByFlowUUID :: !(M.Map UUID [SupplierEntry])
    {- ^ Waste flow UUID → activities whose reference product is that waste
    (treatment activities). Strict-matched only — no synonym, no scoring.
    -}
    , idbWasteTreatmentByCanonicalName :: !(M.Map Text [SupplierEntry])
    {- ^ normalizeText (wfName) → same set, for the name-based fallback when
    two databases use different UUIDs for the same canonical waste flow.
    -}
    , idbByActivityAndProductName :: !(M.Map (Text, Text) [SupplierEntry])
    {- ^ (normalized activity name, normalized product name) → suppliers. What a
    demand that names its supplier activity is matched on first, before the
    product name alone: in a released background database, @electricity, medium voltage@
    in GLO is the reference product of 27 different activities, and the product
    name alone cannot say which of them the source meant.
    -}
    , idbByActivityProduct :: !(M.Map (UUID, UUID) SupplierEntry)
    {- ^ (activityUUID, productUUID) → supplier. The exact-identity index for
    EcoSpold2↔EcoSpold2 linking: an input's @(activityLinkId, flowId)@ is the
    supplier's @(activityUUID, referenceProductUUID)@ key, so this resolves a
    partial import's background references with no name/location guessing. One
    supplier per key (location variants of the same activity coincide here).
    -}
    }

{- | Entry in the supplier index
Contains only the metadata needed for cross-DB linking (no Activity/Flow import)
-}
data SupplierEntry = SupplierEntry
    { seActivityUUID :: !UUID
    , seProductUUID :: !UUID
    , seLocation :: !Text
    , seUnit :: !Text
    , seProductName :: !Text -- Product name for display/debugging
    , seActivityName :: !Text
    {- ^ Name of the activity that produces it. What separates
    @market group for electricity, medium voltage@ from the 26 other
    activities of that database whose reference product is @electricity, medium voltage@ in GLO,
    for the one import format that names the supplier activity apart from its
    product.
    -}
    , seRefSign :: !Double
    {- ^ Sign of the supplier's reference production: @+1@ for a normal product
    output, @-1@ for a negative-output waste treatment (the EcoSpold2
    convention; ILCD treatments keep @+1@ via their positive 'ReferenceInput').
    A cross-DB waste link multiplies its coefficient by this so the dependency
    solve drives the treatment in its waste-removing direction regardless of
    which reference convention the supplier database uses.
    -}
    }

{- | What one demand asks a dependency for. Four fields, three of them 'Text':
as positional arguments a caller could transpose the product name, the location
and the unit and the compiler would say nothing.
-}
data SupplierQuery = SupplierQuery
    { sqProductName :: !Text
    -- ^ Product the consumer asks for
    , sqSupplierActivity :: !(Maybe Text)
    {- ^ Activity the source names as its supplier, where it names one apart
    from the product (a 'Types.ClaimByName'). Matched first. A name no
    dependency carries falls through to the product name, so a foreground
    written against another release still links; a name that is carried, but
    not where the demand needs it, is refused as any other demand would be
    rather than answered by a different activity that happens to sit closer.
    -}
    , sqLocation :: !Text
    -- ^ Location of the consumer
    , sqUnit :: !Text
    -- ^ Unit of the exchange
    }

-- | Context for cross-database linking (with pre-built indexes)
data LinkingContext = LinkingContext
    { lcIndexedDatabases :: ![IndexedDatabase]
    -- ^ Pre-indexed databases to search
    , lcSynonymDB :: !SynonymDB
    -- ^ For product name matching
    , lcUnitConfig :: !UC.UnitConfig
    -- ^ For unit compatibility
    , lcThreshold :: !Int
    {- ^ Minimum score to auto-link (default: 55). Acts as a sanity floor on
    name+location scoring; geographic acceptability is enforced separately
    by 'lcGeographyPolicy' via 'acceptableLocation'.
    -}
    , lcLocationHierarchy :: !(M.Map Location [Location])
    -- ^ Location hierarchy (code → parent codes)
    , lcGeographyPolicy :: !GeographyPolicy
    -- ^ How aggressively to widen geography when no exact match is found
    , lcSupplierAliases :: !AliasMap
    {- ^ Consumer-flow → designated-supplier aliases from a relink mapping
    (an input named after one background database, rewritten to another
    database's activity name). A matching row preempts the direct cascade:
    the curated designation is a stronger statement of intent than a
    generic name match, otherwise a row could be silently overridden by a
    coincidental direct hit. Keys match the raw (un-normalized) flow name
    plus the demand's effective location — see 'lookupAlias'.
    'emptyAliasMap' disables the feature (the common case).
    -}
    }

{- | Alias source key: consumer flow name plus optional consumer location.
A row with a location applies only to demands at that exact location code
(no hierarchy — the curator writes the code as it appears in the data); a
row without one applies at any location.
-}
data AliasKey = AliasKey
    { akName :: !Text
    , akLocation :: !(Maybe Text)
    }
    deriving (Show, Eq, Ord)

{- | Designated supplier of an alias row: product/activity name, optionally
pinned to an exact location code. A pinned location is honoured literally —
the matcher links there bypassing the geography policy and score threshold,
and reports 'AliasTargetMissing' when nothing supplies that name there.
Without a pinned location the geography policy chooses among the name's
candidates, as for any other demand.
-}
data AliasTarget = AliasTarget
    { atName :: !Text
    , atLocation :: !(Maybe Text)
    }
    deriving (Show, Eq)

{- | Consumer-flow → designated-supplier mapping. The empty map means no
mapping is in force, so a bare newtype keeps "no aliases" and "no matching
row" from needing two representations.
-}
newtype AliasMap = AliasMap (M.Map AliasKey AliasTarget)
    deriving (Show, Eq)

emptyAliasMap :: AliasMap
emptyAliasMap = AliasMap M.empty

{- | The alias row in force for a demand: an exact (name, location) row wins
over a name-only row; a demand without a location can only match name-only
rows.
-}
lookupAlias :: AliasMap -> Text -> Text -> Maybe AliasTarget
lookupAlias (AliasMap m) name loc =
    located <|> M.lookup (AliasKey name Nothing) m
  where
    located =
        if T.null loc
            then Nothing
            else M.lookup (AliasKey name (Just loc)) m

-- | A candidate supplier from another database
data CrossDBCandidate = CrossDBCandidate
    { cdbActivityUUID :: !UUID
    -- ^ Activity UUID in the other database
    , cdbProductUUID :: !UUID
    -- ^ Reference product UUID
    , cdbDatabaseName :: !Text
    -- ^ Name of the source database
    , cdbScore :: !Int
    -- ^ Match score (higher = better)
    , cdbLocation :: !Text
    -- ^ Location of the activity
    , cdbProductName :: !Text
    -- ^ Product name for display/debugging
    , cdbActivityName :: !Text
    -- ^ Name of the activity that produces it, for display and for ambiguity reports
    }

-- | Result of cross-database linking attempt
data CrossDBLinkResult
    = CrossDBLinked
        { cdlrActivityUUID :: !UUID
        , cdlrProductUUID :: !UUID
        , cdlrDatabaseName :: !Text
        , cdlrScore :: !Int
        , cdlrProductName :: !Text
        , cdlrLocation :: !Text
        , cdlrWarnings :: ![LinkWarning]
        , cdlrTiedDatabases :: ![Text]
        {- ^ Other databases whose best candidate ties the winner's score.
        Used to detect redundant dependencies at staging time.
        -}
        }
    | CrossDBNotLinked !LinkBlocker

-- | Non-blocking warning: link succeeded but with caveats
data LinkWarning
    = -- | requestedLoc, actualLoc, kind (e.g. FR → RER, ParentLoc)
      UpperLocationUsed !Text !Text !LocationKind
    | {- | Several activities of the /same/ database tie for the best score, so
      the demand does not say which of them it meant and the winner is the one
      the ranking happened to return: chosen activity name, number tied.
      'cdlrTiedDatabases' reports the same across databases; this is the tie
      inside one, which used to pass in silence.
      -}
      AmbiguousSupplier !Text !Int
    deriving (Show, Eq)

-- | LinkBlocker is defined in Types and re-exported here

{- | Default threshold for automatic linking
Requires at minimum: product name match (45-50) + some location match (10+)
-}
defaultLinkingThreshold :: Int
defaultLinkingThreshold = 55

{- | Normalize Unicode characters to ASCII equivalents for matching.
Handles soft hyphens, various dash types, and non-breaking spaces that
appear in SimaPro CSV exports but not in ecoinvent's ASCII names.
-}
normalizeUnicode :: Text -> Text
normalizeUnicode = T.map replaceChar
  where
    replaceChar '\x00AD' = '-' -- Soft hyphen → ASCII hyphen
    replaceChar '\x2010' = '-' -- Hyphen → ASCII hyphen
    replaceChar '\x2011' = '-' -- Non-breaking hyphen → ASCII hyphen
    replaceChar '\x2012' = '-' -- Figure dash → ASCII hyphen
    replaceChar '\x2013' = '-' -- En dash → ASCII hyphen
    replaceChar '\x2014' = '-' -- Em dash → ASCII hyphen
    replaceChar '\x00A0' = ' ' -- Non-breaking space → space
    replaceChar '\x202F' = ' ' -- Narrow no-break space → space
    replaceChar c = c

-- | Normalize text for matching: lowercase, strip whitespace, normalize Unicode
normalizeText :: Text -> Text
normalizeText = T.toLower . T.strip . normalizeUnicode

{- | Extract a location code from any bracket pattern in a name.
Tries {XX} first (standard LCA geography notation), then [XX] with
validation to avoid chemical notation like [thio] or metadata like [Dummy].
-}
extractBracketedLocation :: Text -> Text
extractBracketedLocation name =
    case extractFromBrackets '{' '}' name of
        Just loc | not (T.null loc) -> loc
        _ -> case extractFromBrackets '[' ']' name of
            Just loc | looksLikeGeo loc -> loc
            _ -> ""
  where
    -- Accept only short codes starting uppercase (e.g. "GLO", "FR", "RER", "RoW")
    looksLikeGeo t =
        T.length t >= 2
            && T.length t <= 3
            && isUpper (T.head t)
    extractFromBrackets :: Char -> Char -> Text -> Maybe Text
    extractFromBrackets open close txt =
        let (_, afterOpen) = T.breakOn (T.singleton open) txt
         in if T.null afterOpen
                then Nothing
                else
                    let inside = T.drop 1 afterOpen -- skip the open bracket
                        (content, afterClose) = T.breakOn (T.singleton close) inside
                     in if T.null afterClose
                            then Nothing
                            else Just (T.strip content)

{- | Build an indexed database for fast cross-DB lookups
This should be called once when a database is loaded
-}
buildIndexedDatabase :: Text -> SynonymDB -> SimpleDatabase -> IndexedDatabase
buildIndexedDatabase dbName synDB db =
    let entries = buildSupplierEntries db
        -- Index by normalized product name, and by nothing else: a prefix of
        -- a product name is a different product.
        byName =
            M.fromListWith
                (++)
                [ (normalizeText prodName, [entry])
                | (prodName, entry) <- entries
                , not (T.null (normalizeText prodName))
                ]
        -- Index by synonym group (for synonym matching)
        bySynonym =
            M.fromListWith
                (++)
                [ (groupId, [entry])
                | (prodName, entry) <- entries
                , Just groupId <- [lookupSynonymGroup synDB (normalizeName prodName)]
                ]
        wasteEntries = buildWasteTreatmentEntries db
        wasteByUUID = M.fromListWith (++) [(uuid, [entry]) | (uuid, _, entry) <- wasteEntries]
        wasteByName = M.fromListWith (++) [(normalizeText name, [entry]) | (_, name, entry) <- wasteEntries, not (T.null (normalizeText name))]
        byActProd = indexByActivityProduct entries
        byActAndProdName = indexByActivityAndProductName entries
     in IndexedDatabase
            { idbName = dbName
            , idbByProductName = byName
            , idbBySynonymGroup = bySynonym
            , idbWasteTreatmentByFlowUUID = wasteByUUID
            , idbWasteTreatmentByCanonicalName = wasteByName
            , idbByActivityAndProductName = byActAndProdName
            , idbByActivityProduct = byActProd
            }

{- | Index supplier entries by their @(activityUUID, productUUID)@ identity.
Each location variant is a distinct activity with its own activity UUID, so
variants do not collide here; a collision can only be a genuine duplicate of the
same activity+product, making last-wins harmless.
-}
indexByActivityProduct :: [(Text, SupplierEntry)] -> M.Map (UUID, UUID) SupplierEntry
indexByActivityProduct entries =
    M.fromList [((seActivityUUID e, seProductUUID e), e) | (_, e) <- entries]

{- | Index supplier entries by their @(activity name, product name)@ pair, both
normalized. Unlike the UUID index above the key does not determine the value —
one activity name can be borne by several location variants — so the entries
stay in a list and the geography policy chooses among them as it does for a
product-name match.
-}
indexByActivityAndProductName :: [(Text, SupplierEntry)] -> M.Map (Text, Text) [SupplierEntry]
indexByActivityAndProductName entries =
    M.fromListWith
        (++)
        [ ((normalizeText (seActivityName e), normalizeText prodName), [e])
        | (prodName, e) <- entries
        , not (T.null (normalizeText (seActivityName e)))
        , not (T.null (normalizeText prodName))
        ]

{- | Build supplier entries from a SimpleDatabase. Reference exchanges of
production processes are always technosphere outputs, so the supplier flow
lives in `sdbTechFlows`.
-}
buildSupplierEntries :: SimpleDatabase -> [(Text, SupplierEntry)]
buildSupplierEntries db =
    [ (tfName flow, productEntry act actUUID prodUUID loc (tfName flow))
    | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
    , ex <- exchanges act
    , exchangeIsReference ex
    , Just flow <- [M.lookup (exchangeFlowId ex) (sdbTechFlows db)]
    , loc <- supplierLocations act ex
    ]

{- | A supplier entry for an activity's product output. Shared by the two index
builders so the field order is stated once: four of the fields are 'Text' and a
positional constructor is one transposition away from a wrong answer.
-}
productEntry :: Activity -> UUID -> UUID -> Text -> Text -> SupplierEntry
productEntry act actUUID prodUUID loc prodName =
    SupplierEntry
        { seActivityUUID = actUUID
        , seProductUUID = prodUUID
        , seLocation = loc
        , seUnit = activityUnit act
        , seProductName = prodName
        , seActivityName = activityName act
        , seRefSign = 1.0
        }

{- | A supplier entry for a waste-treatment activity, whose reference product is
the waste flow it removes and whose sign says which direction that is.
-}
treatmentEntry :: Activity -> UUID -> UUID -> Text -> SupplierEntry
treatmentEntry act actUUID prodUUID wasteName =
    SupplierEntry
        { seActivityUUID = actUUID
        , seProductUUID = prodUUID
        , seLocation = activityLocation act
        , seUnit = activityUnit act
        , seProductName = wasteName
        , seActivityName = activityName act
        , seRefSign = signum (activityNormFactor act (actUUID, prodUUID))
        }

{- | Treatment-activity entries from a SimpleDatabase: an activity is a waste
treatment supplier iff its reference exchange's flow is in 'sdbWasteFlows'
(the dataset author declared a waste flow as the activity's reference
product). One tuple per (waste flow UUID, waste flow name, entry).
-}
buildWasteTreatmentEntries :: SimpleDatabase -> [(UUID, Text, SupplierEntry)]
buildWasteTreatmentEntries db =
    [ (wfId flow, wfName flow, treatmentEntry act actUUID prodUUID (wfName flow))
    | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
    , ex <- exchanges act
    , exchangeIsReference ex
    , Just flow <- [M.lookup (exchangeFlowId ex) (sdbWasteFlows db)]
    ]

{- | Build an indexed database from a full Database (used when loading from cache)
This is the preferred method as it works with cached databases
-}
buildIndexedDatabaseFromDB :: Text -> SynonymDB -> Database -> IndexedDatabase
buildIndexedDatabaseFromDB dbName synDB db =
    let entries = buildSupplierEntriesFromDB db
        -- Index by normalized product name, and by nothing else: a prefix of
        -- a product name is a different product.
        byName =
            M.fromListWith
                (++)
                [ (normalizeText prodName, [entry])
                | (prodName, entry) <- entries
                , not (T.null (normalizeText prodName))
                ]
        -- Index by synonym group (for synonym matching)
        bySynonym =
            M.fromListWith
                (++)
                [ (groupId, [entry])
                | (prodName, entry) <- entries
                , Just groupId <- [lookupSynonymGroup synDB (normalizeName prodName)]
                ]
        wasteEntries = buildWasteTreatmentEntriesFromDB db
        wasteByUUID = M.fromListWith (++) [(uuid, [entry]) | (uuid, _, entry) <- wasteEntries]
        wasteByName = M.fromListWith (++) [(normalizeText name, [entry]) | (_, name, entry) <- wasteEntries, not (T.null (normalizeText name))]
        byActProd = indexByActivityProduct entries
        byActAndProdName = indexByActivityAndProductName entries
     in IndexedDatabase
            { idbName = dbName
            , idbByProductName = byName
            , idbBySynonymGroup = bySynonym
            , idbWasteTreatmentByFlowUUID = wasteByUUID
            , idbWasteTreatmentByCanonicalName = wasteByName
            , idbByActivityAndProductName = byActAndProdName
            , idbByActivityProduct = byActProd
            }

{- | Build supplier entries from a full Database. Same invariant as
'buildSupplierEntries' above: reference exchanges are technosphere.
-}
buildSupplierEntriesFromDB :: Database -> [(Text, SupplierEntry)]
buildSupplierEntriesFromDB db =
    [ (tfName flow, productEntry act actUUID prodUUID loc (tfName flow))
    | (pid, (actUUID, prodUUID)) <- zip ([0 ..] :: [Int]) (V.toList (dbProcessIdTable db))
    , Just act <- [getActivity db (fromIntegral pid)]
    , ex <- exchanges act
    , exchangeIsReference ex
    , Just flow <- [M.lookup (exchangeFlowId ex) (dbTechFlows db)]
    , loc <- supplierLocations act ex
    ]

{- | Locations under which an activity should be indexed as a supplier.

Always includes 'activityLocation'. Adds the reference exchange's
'techLocation' when it is non-empty and distinct — this surfaces SimaPro
products whose Products row declares a wider geographic scope than the
enclosing Process name (typically WFLDB: process @ /CH, product @ /GLO).

Design note: the activity table stays at 'activityLocation' (honest about
data-collection provenance); only the cross-DB lookup gets the alias.
-}
supplierLocations :: Activity -> Exchange -> [Text]
supplierLocations act ex =
    let actLoc = activityLocation act
        exLoc = exchangeLocation ex
     in if not (T.null exLoc) && exLoc /= actLoc
            then [actLoc, exLoc]
            else [actLoc]

-- | Treatment-activity entries from a full Database. Mirrors 'buildWasteTreatmentEntries'.
buildWasteTreatmentEntriesFromDB :: Database -> [(UUID, Text, SupplierEntry)]
buildWasteTreatmentEntriesFromDB db =
    [ (wfId flow, wfName flow, treatmentEntry act actUUID prodUUID (wfName flow))
    | (pid, (actUUID, prodUUID)) <- zip ([0 ..] :: [Int]) (V.toList (dbProcessIdTable db))
    , Just act <- [getActivity db (fromIntegral pid)]
    , ex <- exchanges act
    , exchangeIsReference ex
    , Just flow <- [M.lookup (exchangeFlowId ex) (dbWasteFlows db)]
    ]

{- | Outcome of a strict cross-DB waste-treatment lookup.

The matcher is intentionally narrow: it succeeds only when the dataset
author has provided an explicit alignment (same flow UUID, or — as a
fallback — byte-exact normalized flow name) and exactly one database in
the pool offers a candidate. Two databases offering a match resolves to
'WasteAmbiguous', never to a first-wins auto-pick. There is no synonym
graph, no compound-name extraction, no location widening, and no scoring
threshold — those would cross from honoring explicit intent into
fabricating links.
-}
data WasteTreatmentMatch
    = -- | entry + source database name
      WasteMatched !SupplierEntry !Text
    | -- | databases that all offered a candidate
      WasteAmbiguous ![Text]
    | WasteNoMatch

{- | Strict cross-DB waste-treatment lookup. Honors author-provided alignment
only — see 'WasteTreatmentMatch' for the semantics.

Resolution order: flow UUID first, then byte-exact normalized name. Within
each tier, a match is accepted iff exactly one database in 'lcIndexedDatabases'
contains a candidate. Multiple within-DB candidates count as one match for
that database (treatment by region selection is the database author's
choice; if it ships per-region treatments, we don't pick).
-}
findWasteTreatmentAcrossDatabases ::
    LinkingContext ->
    -- | Orphan waste flow UUID
    UUID ->
    -- | Canonical waste flow name (for the name-based fallback)
    Text ->
    WasteTreatmentMatch
findWasteTreatmentAcrossDatabases LinkingContext{lcIndexedDatabases} flowUUID flowName =
    case lookupSingleDB (M.lookup flowUUID . idbWasteTreatmentByFlowUUID) of
        match@WasteMatched{} -> match
        WasteAmbiguous dbs -> WasteAmbiguous dbs
        WasteNoMatch ->
            let normalized = normalizeText flowName
             in if T.null normalized
                    then WasteNoMatch
                    else lookupSingleDB (M.lookup normalized . idbWasteTreatmentByCanonicalName)
  where
    lookupSingleDB :: (IndexedDatabase -> Maybe [SupplierEntry]) -> WasteTreatmentMatch
    lookupSingleDB lookupKey =
        let perDB =
                [ (idbName idb, entries)
                | idb <- lcIndexedDatabases
                , Just entries <- [lookupKey idb]
                , not (null entries)
                ]
         in case perDB of
                [(dbN, [entry])] -> WasteMatched entry dbN
                [_] -> WasteNoMatch -- single DB with multiple candidates: stay orphan
                [] -> WasteNoMatch
                manyDbs -> WasteAmbiguous (map fst manyDbs)

{- | The treatment a waste output's own link names, looked up in the loaded
databases. The link is the dataset author's own disambiguation, so the match is
strict on identity: same waste flow, same activity UUID, no name match, no
location widening. Two databases shipping that identity resolve to
'WasteAmbiguous' rather than a first-wins pick, as everywhere else here.

Waste treatments live in 'idbWasteTreatmentByFlowUUID' and nowhere else:
'idbByActivityProduct' is built from activities whose reference is a
technosphere product, which a treatment's never is.
-}
findWasteTreatmentByActivity ::
    LinkingContext ->
    -- | Activity the waste output links to
    UUID ->
    -- | Waste flow UUID
    UUID ->
    WasteTreatmentMatch
findWasteTreatmentByActivity LinkingContext{lcIndexedDatabases} actUUID flowUUID =
    case [ (idbName idb, entry)
         | idb <- lcIndexedDatabases
         , Just entries <- [M.lookup flowUUID (idbWasteTreatmentByFlowUUID idb)]
         , entry <- entries
         , seActivityUUID entry == actUUID
         ] of
        [(dbN, entry)] -> WasteMatched entry dbN
        [] -> WasteNoMatch
        matches -> WasteAmbiguous (nub (map fst matches))

{- | Find a supplier across all loaded databases (using pre-built indexes)
This is the fast O(1) lookup version
-}
findSupplierInIndexedDBs :: LinkingContext -> SupplierQuery -> CrossDBLinkResult
findSupplierInIndexedDBs LinkingContext{..} SupplierQuery{sqProductName = productName, sqSupplierActivity = supplierActivity, sqLocation = location, sqUnit = unit} =
    -- An alias row preempts the direct cascade: the curator's designation is
    -- a stronger statement of intent than a generic name match — otherwise a
    -- row answering "which supplier replaces this input?" could be silently
    -- overridden by a coincidental direct hit. A name with no row resolves
    -- exactly as it would without any mapping.
    case lookupAlias lcSupplierAliases productName effectiveLocation of
        Just target -> resolveDesignated target
        Nothing ->
            -- Priority-ordered match strategies; take the first non-empty
            -- result via 'firstNonEmpty':
            --   1. The (supplier activity, product) pair, when the source named
            --      the activity: the only key that separates the 27
            --      activities whose reference product is "electricity, medium
            --      voltage" in GLO.
            --   2. Exact product-name match across all indexed DBs.
            --   3. Synonym-group match if exact yielded nothing, the named
            --      activity preferred among what the group returns.
            resolveCandidates $
                firstNonEmpty
                    [ byActivityAndProduct
                    , concatMap (lookupExact (normalizeText productName)) lcIndexedDatabases
                    , preferNamedActivity $ case lookupSynonymGroup lcSynonymDB (normalizeName productName) of
                        Just groupId -> concatMap (lookupBySynonym groupId) lcIndexedDatabases
                        Nothing -> []
                    ]
  where
    byActivityAndProduct :: [(Text, SupplierEntry)]
    byActivityAndProduct =
        [ (idbName idb, entry)
        | activity <- maybe [] pure supplierActivity
        , idb <- lcIndexedDatabases
        , entry <-
            fromMaybe [] $
                M.lookup (normalizeText activity, normalizeText productName) (idbByActivityAndProductName idb)
        ]

    {- The synonym tier answers under a name the dependency spells otherwise, so
    the pair index could not have matched and the activity name is still
    unspent. Where one candidate of the group carries it, that is the demand
    answered exactly; where none does, the release renamed the activity as well
    as the product and the ranking judges, as it did before. -}
    preferNamedActivity :: [(Text, SupplierEntry)] -> [(Text, SupplierEntry)]
    preferNamedActivity candidates = case supplierActivity of
        Nothing -> candidates
        Just activity -> firstNonEmpty [filter (named activity) candidates, candidates]
      where
        named :: Text -> (Text, SupplierEntry) -> Bool
        named activity (_, entry) = normalizeText activity == normalizeText (seActivityName entry)

    -- Effective location: if raw location is empty, try extracting from compound name
    effectiveLocation =
        if T.null location
            then extractBracketedLocation productName
            else location

    -- Resolve an alias row's designated target by its name, then either the
    -- normal geography-policy pipeline (no pinned location) or the literal
    -- designated link. A target name that matches nowhere is a loud
    -- curated-mapping error ('AliasTargetMissing'), never a silent
    -- 'NoNameMatch'.
    resolveDesignated (AliasTarget targetName mTargetLoc) =
        case tryName targetName of
            [] -> CrossDBNotLinked (AliasTargetMissing targetName Nothing)
            candidates -> case mTargetLoc of
                Nothing -> resolveCandidates candidates
                Just targetLoc -> designatedAt targetName targetLoc candidates

    -- The curator designated (name, location): link there directly. Unit
    -- compatibility still applies; the geography policy and score threshold
    -- do not (and no 'UpperLocationUsed' warning — the widening is
    -- deliberate). A designated location nothing supplies is a loud
    -- curated-mapping error, never a silent fallback to the generic cascade.
    -- The stored score still rates the supplier against the consumer's own
    -- location, so a pinned cross-location link can carry a score below the
    -- threshold — expected, since the designation overrode the ranking.
    designatedAt targetName targetLoc candidates =
        case filter ((== targetLoc) . seLocation . snd) candidates of
            [] -> CrossDBNotLinked (AliasTargetMissing targetName (Just targetLoc))
            atLoc@((_, firstSe) : _) ->
                case filter (\(_, se) -> unitsAreCompatible lcUnitConfig unit (seUnit se)) atLoc of
                    [] -> CrossDBNotLinked UnitIncompatible{uiQueryUnit = unit, uiSupplierUnit = seUnit firstSe}
                    compatible@(_ : _) ->
                        let scored = map (scoreEntry effectiveLocation) compatible
                            !best = maximumBy (comparing cdbScore) scored
                         in mkLinked best (sameDatabaseTies best scored) (tiedDatabases best scored)

    -- Pipeline once the candidate set is fixed: unit filter, geography
    -- policy, then rank the survivors against the score threshold.
    resolveCandidates allCandidates =
        case allCandidates of
            [] -> CrossDBNotLinked NoNameMatch
            ((_, firstSe) : _) ->
                -- Check unit compatibility first
                let unitCompatible = filter (\(_, se) -> unitsAreCompatible lcUnitConfig unit (seUnit se)) allCandidates
                 in if null unitCompatible
                        then
                            -- All candidates failed unit check — report the first supplier's unit
                            CrossDBNotLinked UnitIncompatible{uiQueryUnit = unit, uiSupplierUnit = seUnit firstSe}
                        else
                            -- Filter candidates geographically via policy, then rank survivors
                            let accepted = mapMaybe (classifyEntry effectiveLocation) unitCompatible
                             in case accepted of
                                    [] ->
                                        -- Candidates existed but every one was rejected.
                                        -- Distinguish "no plausible location" (narrowing only / hierarchy miss)
                                        -- from "policy rejected an otherwise valid match".
                                        case rejectionReason effectiveLocation unitCompatible of
                                            Just (bestLoc, bestKind) ->
                                                CrossDBNotLinked
                                                    LocationRejectedByPolicy
                                                        { lrRequested = effectiveLocation
                                                        , lrBestCandidate = bestLoc
                                                        , lrBestKind = bestKind
                                                        }
                                            Nothing ->
                                                CrossDBNotLinked (LocationUnavailable effectiveLocation)
                                    _ ->
                                        let scored = map (first (scoreEntry effectiveLocation)) accepted
                                            !(bestCand, bestKind) = maximumBy (comparing (cdbScore . fst)) scored
                                         in if cdbScore bestCand >= lcThreshold
                                                then
                                                    mkLinked
                                                        bestCand
                                                        ( [ UpperLocationUsed effectiveLocation (cdbLocation bestCand) bestKind
                                                          | not (T.null location || bestKind == ExactLoc)
                                                          ]
                                                            ++ sameDatabaseTies bestCand (map fst scored)
                                                        )
                                                        (tiedDatabases bestCand (map fst scored))
                                                else CrossDBNotLinked (LocationUnavailable effectiveLocation)

    {- Distinct suppliers of the winner's /own/ database that tie its score.
    Which of them wins is whichever the ranking returned, so the demand has not
    been answered, only closed: say so instead of letting it pass. Cheap because
    a tie is rare — the list is walked only to count the winners. -}
    sameDatabaseTies :: CrossDBCandidate -> [CrossDBCandidate] -> [LinkWarning]
    sameDatabaseTies winner scored =
        [ AmbiguousSupplier (cdbActivityName winner) tied
        | let tied =
                length . nub $
                    [ (cdbActivityUUID c, cdbProductUUID c)
                    | c <- scored
                    , cdbScore c == cdbScore winner
                    , cdbDatabaseName c == cdbDatabaseName winner
                    ]
        , tied > 1
        ]

    -- Other databases whose surviving best candidate ties the winner's
    -- score. Dedup by DB name to ignore intra-DB ties.
    tiedDatabases winner scored =
        M.keys $
            M.fromList
                [ (cdbDatabaseName c, ())
                | c <- scored
                , cdbScore c == cdbScore winner
                , cdbDatabaseName c /= cdbDatabaseName winner
                ]

    mkLinked best warnings tied =
        CrossDBLinked
            { cdlrActivityUUID = cdbActivityUUID best
            , cdlrProductUUID = cdbProductUUID best
            , cdlrDatabaseName = cdbDatabaseName best
            , cdlrScore = cdbScore best
            , cdlrProductName = cdbProductName best
            , cdlrLocation = cdbLocation best
            , cdlrWarnings = warnings
            , cdlrTiedDatabases = tied
            }
    lookupExact :: Text -> IndexedDatabase -> [(Text, SupplierEntry)]
    lookupExact name idb =
        [(idbName idb, entry) | entry <- fromMaybe [] (M.lookup name (idbByProductName idb))]

    lookupBySynonym :: Int -> IndexedDatabase -> [(Text, SupplierEntry)]
    lookupBySynonym groupId idb =
        [(idbName idb, entry) | entry <- fromMaybe [] (M.lookup groupId (idbBySynonymGroup idb))]

    -- \| First non-empty list in a priority order. This is the @First@ monoid
    --    on @Maybe [a]@ (lift each list into @Maybe@ via 'nonEmpty', combine with
    --    @<|>@, drop back), collapsed to a single helper because that's the
    --    exact shape every match-strategy cascade in this module wants.
    --
    firstNonEmpty :: [[a]] -> [a]
    firstNonEmpty = fromMaybe [] . find (not . null)

    -- Run the (exact, then synonym) sub-cascade on a single candidate name.
    tryName :: Text -> [(Text, SupplierEntry)]
    tryName p =
        let normalized = normalizeText p
            byExact = concatMap (lookupExact normalized) lcIndexedDatabases
            bySynonym = case lookupSynonymGroup lcSynonymDB (normalizeName p) of
                Just groupId -> concatMap (lookupBySynonym groupId) lcIndexedDatabases
                Nothing -> []
         in firstNonEmpty [byExact, bySynonym]

    classifyEntry :: Text -> (Text, SupplierEntry) -> Maybe ((Text, SupplierEntry), LocationKind)
    classifyEntry queryLoc entry@(_, SupplierEntry{seLocation}) =
        case acceptableLocation lcGeographyPolicy lcLocationHierarchy (Location queryLoc) (Location seLocation) of
            Just kind -> Just (entry, kind)
            Nothing -> Nothing

    -- For the rejected-by-policy case, find the best candidate that *would*
    -- have been accepted under 'GeoGlobal' but is rejected here, so we can
    -- report it to the user. Returns Nothing if even GeoGlobal would reject
    -- (e.g. narrowing only) — caller falls back to LocationUnavailable.
    rejectionReason :: Text -> [(Text, SupplierEntry)] -> Maybe (Text, LocationKind)
    rejectionReason queryLoc candidates =
        let permissive =
                mapMaybe
                    ( \(_, SupplierEntry{seLocation}) ->
                        (seLocation,)
                            <$> acceptableLocation GeoGlobal lcLocationHierarchy (Location queryLoc) (Location seLocation)
                    )
                    candidates
            kindOrder ExactLoc = 4 :: Int
            kindOrder ParentLoc = 3
            kindOrder GlobalLoc = 2
            kindOrder UnrelatedLoc = 1
         in case permissive of
                [] -> Nothing
                _ -> Just $ maximumBy (comparing (kindOrder . snd)) permissive

    scoreEntry :: Text -> (Text, SupplierEntry) -> CrossDBCandidate
    scoreEntry queryLoc (dbName, SupplierEntry{..}) =
        let locScore = matchLocation lcLocationHierarchy (Location queryLoc) (Location seLocation)
            nameScore = 50
            !totalScore = nameScore + locScore
         in CrossDBCandidate
                { cdbActivityUUID = seActivityUUID
                , cdbProductUUID = seProductUUID
                , cdbDatabaseName = dbName
                , cdbScore = totalScore
                , cdbLocation = seLocation
                , cdbProductName = seProductName
                , cdbActivityName = seActivityName
                }

{- | Resolve a supplier by exact @(activityUUID, productUUID)@ identity across
the indexed databases. An EcoSpold2 input's @(activityLinkId, flowId)@ is
exactly the supplier's @(activityUUID, referenceProductUUID)@ key, so this is
how a partial import resolves its background references with no name/location
guessing — the dataset author's own disambiguation, honoured verbatim.

Returns every database that ships the identical activity+product, in the order
of the indexed-database list. Callers take the head as the supplier and the
remaining database names as tied alternatives (for minimal-dependency
pre-selection). Empty when no loaded dependency provides this exact identity —
the cross-version case, where the caller falls back to attribute matching.
-}
findSupplierByActivityProduct :: [IndexedDatabase] -> UUID -> UUID -> [(SupplierEntry, Text)]
findSupplierByActivityProduct idbs actUUID prodUUID =
    [ (entry, idbName idb)
    | idb <- idbs
    , Just entry <- [M.lookup (actUUID, prodUUID) (idbByActivityProduct idb)]
    ]

{- | Match product names (simplified - just for scoring display)
Actual matching is done via index lookup
-}
matchProductName :: SynonymDB -> Text -> Text -> Int
matchProductName synDB query candidate
    | normalizeText query == normalizeText candidate = 50 -- Exact match
    | areSynonyms synDB query candidate = 45 -- Synonym match
    | otherwise = 0 -- No match

-- | Check if two names are synonyms using the SynonymDB
areSynonyms :: SynonymDB -> Text -> Text -> Bool
areSynonyms synDB name1 name2 =
    case ( lookupSynonymGroup synDB (normalizeName name1)
         , lookupSynonymGroup synDB (normalizeName name2)
         ) of
        (Just g1, Just g2) -> g1 == g2
        _ -> False

{- | Match locations with hierarchy fallback

Returns:
  30 = Exact match
  20 = Subregion match (e.g., FR ⊂ Europe)
  10 = Global fallback (GLO or RoW)
   5 = Different but not blocking
-}
matchLocation :: M.Map Location [Location] -> Location -> Location -> Int
matchLocation hier queryLoc candidateLoc
    | queryLoc == candidateLoc = 30 -- Exact
    | isSubregionOf hier queryLoc candidateLoc = 20 -- Widening (FR→GLO, FR→RER)
    | candidateLoc `elem` placelessLocations = 10 -- Global fallback
    | isSubregionOf hier candidateLoc queryLoc = 0 -- Narrowing (GLO→FR) — blocked
    | otherwise = 5 -- Unrelated

{- | Classify a candidate location relative to the requested one. Pure
description, ignoring any policy. Used by 'acceptableLocation' and to label
fallback warnings.

'GlobalLoc' takes precedence over 'ParentLoc' even when GLO/RoW appears in
the requested location's parent chain (every country has GLO/RoW listed in
'locationHierarchy'), because semantically a global fallback is a stronger
caveat than an honest geographic widening.
-}
describeLocation :: M.Map Location [Location] -> Location -> Location -> LocationKind
describeLocation hier queryLoc candidateLoc
    | queryLoc == candidateLoc = ExactLoc
    | candidateLoc `elem` placelessLocations = GlobalLoc
    | isSubregionOf hier queryLoc candidateLoc = ParentLoc
    | otherwise = UnrelatedLoc

{- | Decide whether a candidate location is acceptable for the given policy.

Narrowing (candidate strictly more specific than requested) is always
rejected: it would invent precision the source dataset does not have.
-}
acceptableLocation ::
    GeographyPolicy ->
    M.Map Location [Location] ->
    -- | requested location
    Location ->
    -- | candidate location
    Location ->
    Maybe LocationKind
acceptableLocation policy hier queryLoc candidateLoc
    | isNarrowing = Nothing
    | otherwise = case (describeLocation hier queryLoc candidateLoc, policy) of
        (ExactLoc, _) -> Just ExactLoc
        (ParentLoc, GeoExact) -> Nothing
        (ParentLoc, _) -> Just ParentLoc
        (GlobalLoc, GeoGlobal) -> Just GlobalLoc
        (GlobalLoc, _) -> Nothing
        (UnrelatedLoc, GeoGlobal) -> Just UnrelatedLoc
        (UnrelatedLoc, _) -> Nothing
  where
    -- candidate is more specific than query, but not when candidate is a
    -- placeless code (GLO/RoW/Unspecified) — those are wider, not narrower
    isNarrowing =
        queryLoc /= candidateLoc
            && candidateLoc `notElem` placelessLocations
            && isSubregionOf hier candidateLoc queryLoc

-- | Check if one location is a subregion of another
isSubregionOf :: M.Map Location [Location] -> Location -> Location -> Bool
isSubregionOf hier child parent =
    case M.lookup child hier of
        Just parents -> parent `elem` parents
        Nothing -> False

{- | Placeless codes: wider than any region — a valid global fallback,
never a narrowing target.
-}
placelessLocations :: [Location]
placelessLocations = map Location ["GLO", "RoW", "Unspecified"]

{- | Location hierarchy for common LCA regions
Maps a location code to its parent regions

The floor, not the table: a configuration that names no geographies file gets
this. Every shipped configuration points at @data/geographies.csv@, which is
where a location belongs — it covers the whole ecoinvent vocabulary, this
covers the handful of regions needed to start up without it.
-}
locationHierarchy :: M.Map Location [Location]
locationHierarchy = M.mapKeysMonotonic Location (M.map (map Location) rawLocationHierarchy)

-- | The hierarchy table, kept as literal 'Text' for readability.
rawLocationHierarchy :: M.Map Text [Text]
rawLocationHierarchy =
    M.fromList
        [ -- European countries → regional/continental groupings
          ("FR", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("DE", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("IT", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("ES", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("GB", ["Europe without Switzerland", "Europe without Austria", "Europe", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("UK", ["Europe without Switzerland", "Europe without Austria", "Europe", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("PL", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("NL", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("BE", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("AT", ["Europe without Switzerland", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("CH", ["Europe without Austria", "Europe", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("SE", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
        , ("NO", ["Europe without Switzerland", "Europe without Austria", "Europe", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
        , ("DK", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
        , ("FI", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
        , ("PT", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("GR", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("IE", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("CZ", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("RO", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("HU", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("SK", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("BG", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("HR", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("SI", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("LT", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("LV", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("EE", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("LU", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
        , ("MT", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "GLO", "RoW"])
        , ("CY", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "GLO", "RoW"])
        , -- Regional groupings → larger regions
          ("EU", ["Europe without Switzerland", "Europe without Austria", "Europe", "RER", "GLO", "RoW"])
        , ("EU-27", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "GLO", "RoW"])
        , ("EU-28", ["Europe without Switzerland", "Europe without Austria", "Europe", "EU", "RER", "GLO", "RoW"])
        , ("RER", ["Europe", "GLO", "RoW"])
        , ("ENTSO-E", ["Europe", "RER", "GLO", "RoW"])
        , ("NORDEL", ["Europe", "ENTSO-E", "RER", "GLO", "RoW"])
        , ("UCTE", ["Europe", "ENTSO-E", "RER", "GLO", "RoW"])
        , ("Europe without Switzerland", ["Europe", "RER", "GLO", "RoW"])
        , ("Europe without Austria", ["Europe", "RER", "GLO", "RoW"])
        , ("Europe", ["GLO", "RoW"])
        , -- North American countries
          ("US", ["North America", "NAFTA", "GLO", "RoW"])
        , ("CA", ["Canada without Quebec", "North America", "NAFTA", "GLO", "RoW"])
        , ("MX", ["North America", "Latin America", "NAFTA", "GLO", "RoW"])
        , ("NAFTA", ["North America", "GLO", "RoW"])
        , ("Canada without Quebec", ["North America", "CA", "NAFTA", "GLO", "RoW"])
        , ("North America", ["GLO", "RoW"])
        , ("RNA", ["North America", "GLO", "RoW"])
        , -- Asian countries
          ("CN", ["Asia", "GLO", "RoW"])
        , ("JP", ["Asia", "GLO", "RoW"])
        , ("KR", ["Asia", "GLO", "RoW"])
        , ("IN", ["Asia", "GLO", "RoW"])
        , ("IN-Southern grid", ["IN", "Asia", "GLO", "RoW"])
        , ("IN-North-eastern grid", ["IN", "Asia", "GLO", "RoW"])
        , ("IN-Eastern grid", ["IN", "Asia", "GLO", "RoW"])
        , ("IN-Northern grid", ["IN", "Asia", "GLO", "RoW"])
        , ("IN-Western grid", ["IN", "Asia", "GLO", "RoW"])
        , ("TW", ["Asia", "GLO", "RoW"])
        , ("ID", ["Asia", "GLO", "RoW"])
        , ("TH", ["Asia", "GLO", "RoW"])
        , ("MY", ["Asia", "GLO", "RoW"])
        , ("VN", ["Asia", "GLO", "RoW"])
        , ("PH", ["Asia", "GLO", "RoW"])
        , ("SG", ["Asia", "GLO", "RoW"])
        , ("Asia", ["GLO", "RoW"])
        , ("RAS", ["Asia", "GLO", "RoW"])
        , -- Latin American countries
          ("BR", ["Latin America", "South America", "GLO", "RoW"])
        , ("AR", ["Latin America", "South America", "GLO", "RoW"])
        , ("CL", ["Latin America", "South America", "GLO", "RoW"])
        , ("CO", ["Latin America", "South America", "GLO", "RoW"])
        , ("PE", ["Latin America", "South America", "GLO", "RoW"])
        , ("Latin America", ["GLO", "RoW"])
        , ("South America", ["Latin America", "GLO", "RoW"])
        , ("RLA", ["Latin America", "GLO", "RoW"])
        , -- African countries/regions
          ("ZA", ["Africa", "GLO", "RoW"])
        , ("EG", ["Africa", "Middle East", "GLO", "RoW"])
        , ("NG", ["Africa", "GLO", "RoW"])
        , ("MA", ["Africa", "GLO", "RoW"])
        , ("Africa", ["GLO", "RoW"])
        , ("RAF", ["Africa", "GLO", "RoW"])
        , -- Oceania
          ("AU", ["Oceania", "GLO", "RoW"])
        , ("NZ", ["Oceania", "GLO", "RoW"])
        , ("Oceania", ["GLO", "RoW"])
        , -- Middle East
          ("SA", ["Middle East", "Asia", "GLO", "RoW"])
        , ("AE", ["Middle East", "Asia", "GLO", "RoW"])
        , ("IL", ["Middle East", "GLO", "RoW"])
        , ("TR", ["Middle East", "Europe", "GLO", "RoW"])
        , ("Middle East", ["GLO", "RoW"])
        , ("RME", ["Middle East", "GLO", "RoW"])
        , -- Global and fallback
          ("GLO", ["RoW"])
        , ("RoW", [])
        ]

{- | Check if two units are compatible for linking.

Same rule as 'Database.Loader.linkUnitsCompatible' and the matrix builder's
own: the same unit needs no conversion, otherwise the dimensions must let one
happen. 'UC.unitKey' is what decides sameness, so two spellings the table does
not know are the same only written the same way.
-}
unitsAreCompatible :: UC.UnitConfig -> Text -> Text -> Bool
unitsAreCompatible cfg unit1 unit2
    | UC.unitKey cfg unit1 == UC.unitKey cfg unit2 = True
    | otherwise = UC.unitsCompatible cfg unit1 unit2 -- Dimensionally compatible

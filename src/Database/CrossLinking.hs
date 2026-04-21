{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Cross-Database Activity Linking

When loading databases that depend on other databases (e.g., Ginko 2025 depends on
Agribalyse 3.2), this module provides functions to resolve supplier references
by searching across all loaded databases.

The matching algorithm uses:
1. Product name matching (exact → synonym)
2. Location matching with hierarchy fallback
3. Unit compatibility checking

A candidate must score above a threshold to be automatically linked.

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
    IndexedDatabase (..),
    SupplierEntry (..),

    -- * Configuration
    defaultLinkingThreshold,

    -- * Index Building
    buildIndexedDatabase,
    buildIndexedDatabaseFromDB,

    -- * Main Functions
    findSupplierAcrossDatabases,
    findSupplierInIndexedDBs,

    -- * Scoring Functions
    matchProductName,
    matchLocation,

    -- * Location Hierarchy
    isSubregionOf,
    locationHierarchy,

    -- * Compound Name Parsing
    extractProductPrefixes,
    extractBracketedLocation,
    stripTrailingDBTag,
    stripTrailingLocationSuffix,

    -- * Text Normalization
    normalizeText,
    normalizeUnicode,
) where

import Data.Char (isAlpha, isUpper)
import Data.List (maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import qualified Data.Vector as V
import SynonymDB (SynonymDB, lookupSynonymGroup, normalizeName)
import Types (Activity (..), Database (..), Exchange (..), Flow (..), LinkBlocker (..), SimpleDatabase (..), getActivity)
import qualified UnitConversion as UC

{- | Pre-indexed database for fast cross-DB supplier lookup
Built once when database is loaded, reused for all lookups
-}
data IndexedDatabase = IndexedDatabase
    { idbName :: !Text
    , idbByProductName :: !(M.Map Text [SupplierEntry]) -- Normalized product name → suppliers
    , idbBySynonymGroup :: !(M.Map Int [SupplierEntry]) -- Synonym group ID → suppliers
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
    -- ^ Minimum score to auto-link (default: 60)
    , lcLocationHierarchy :: !(M.Map Text [Text])
    -- ^ Location hierarchy (code → parent codes)
    }

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
    }

-- | Result of cross-database linking attempt
data CrossDBLinkResult
    = -- | Success: actUUID, prodUUID, dbName, score, productName, location, warnings
      CrossDBLinked !UUID !UUID !Text !Int !Text !Text ![LinkWarning]
    | -- | Failed: reason for failure
      CrossDBNotLinked !LinkBlocker

-- | Non-blocking warning: link succeeded but with caveats
data LinkWarning
    = -- | requestedLoc, actualLoc (e.g. FR → RER)
      UpperLocationUsed !Text !Text
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

{- | Separators that may appear between product name and additional info
in SimaPro compound process names (e.g. "product//[GLO] activity name").
Ordered by priority.
-}
compoundSeparators :: [Text]
compoundSeparators = ["//", " {", " [", " |"]

{- | Strip a trailing database tag from a product name.
Matches patterns like "(WFLDB)", "(AGRIBALYSE)", "(SALCA)" — a parenthesized
suffix where the content is all uppercase letters.
Returns Just strippedName if a tag was found, Nothing otherwise.
-}
stripTrailingDBTag :: Text -> Maybe Text
stripTrailingDBTag name
    | T.null name = Nothing
    | T.last name /= ')' = Nothing
    | otherwise =
        let withoutClose = T.init name -- drop trailing ')'
            (before, tag) = T.breakOnEnd "(" withoutClose
         in if T.null before
                then Nothing -- no opening paren found
                else
                    let prefix = T.init before -- drop the '(' at end of 'before'
                        stripped = T.strip prefix
                     in if not (T.null tag)
                            && T.all (\c -> isUpper c || c == '-') tag
                            && T.any isAlpha tag -- at least one letter
                            && not (T.null stripped)
                            then Just stripped
                            else Nothing

{- | Strip a trailing @\/LOCATION MARKER@ suffix from a product name.
WFLDB names use the pattern @ProductName (WFLDB)\/CA U@ where the suffix
encodes location (2-3 uppercase letters) and unit type (1 uppercase letter).
-}
stripTrailingLocationSuffix :: Text -> Maybe Text
stripTrailingLocationSuffix name =
    case T.breakOnEnd "/" name of
        ("", _) -> Nothing
        (beforeSlash, afterSlash) ->
            case T.words afterSlash of
                [loc, marker]
                    | T.length loc >= 2
                    , T.length loc <= 3
                    , T.all isUpper loc
                    , T.length marker == 1
                    , T.all isUpper marker ->
                        Just (T.stripEnd (T.init beforeSlash))
                _ -> Nothing

{- | Extract product name prefixes from a compound name.
Tries splitting at each separator and returns candidate prefixes (stripped).
Also tries stripping a trailing database tag like "(WFLDB)" and a trailing
location suffix like "/CA U" (WFLDB convention).
Returns empty list if no separator is found and no tag detected.
-}
extractProductPrefixes :: Text -> [Text]
extractProductPrefixes name =
    let separatorPrefixes =
            [ T.strip prefix
            | sep <- compoundSeparators
            , let (prefix, rest) = T.breakOn sep name
            , not (T.null rest) -- separator was found
            , not (T.null prefix) -- non-empty prefix
            ]
        tagStripped = case stripTrailingDBTag name of
            Just stripped -> [stripped]
            Nothing -> []
        locationSuffixStripped = case stripTrailingLocationSuffix name of
            Just stripped ->
                stripped : case stripTrailingDBTag stripped of
                    Just alsoTagStripped -> [alsoTagStripped]
                    Nothing -> []
            Nothing -> []
     in separatorPrefixes ++ tagStripped ++ locationSuffixStripped

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
        -- Index by normalized product name + extracted prefixes
        byName =
            M.fromListWith
                (++)
                [ (normalizeText name, [entry])
                | (prodName, entry) <- entries
                , name <- prodName : extractProductPrefixes prodName
                , not (T.null (normalizeText name))
                ]
        -- Index by synonym group (for synonym matching)
        bySynonym =
            M.fromListWith
                (++)
                [ (groupId, [entry])
                | (prodName, entry) <- entries
                , Just groupId <- [lookupSynonymGroup synDB (normalizeName prodName)]
                ]
     in IndexedDatabase
            { idbName = dbName
            , idbByProductName = byName
            , idbBySynonymGroup = bySynonym
            }

-- | Build supplier entries from a SimpleDatabase
buildSupplierEntries :: SimpleDatabase -> [(Text, SupplierEntry)]
buildSupplierEntries db =
    [ (flowName flow, SupplierEntry actUUID prodUUID (activityLocation act) (activityUnit act) (flowName flow))
    | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
    , ex <- exchanges act
    , isReferenceExchange ex
    , Just flow <- [M.lookup (getExchangeFlowId ex) (sdbFlows db)]
    ]
  where
    isReferenceExchange :: Exchange -> Bool
    isReferenceExchange (TechnosphereExchange _ _ _ _ isRef _ _ _) = isRef
    isReferenceExchange _ = False

    getExchangeFlowId :: Exchange -> UUID
    getExchangeFlowId (TechnosphereExchange fid _ _ _ _ _ _ _) = fid
    getExchangeFlowId (BiosphereExchange fid _ _ _ _) = fid

{- | Build an indexed database from a full Database (used when loading from cache)
This is the preferred method as it works with cached databases
-}
buildIndexedDatabaseFromDB :: Text -> SynonymDB -> Database -> IndexedDatabase
buildIndexedDatabaseFromDB dbName synDB db =
    let entries = buildSupplierEntriesFromDB db
        -- Index by normalized product name + extracted prefixes
        byName =
            M.fromListWith
                (++)
                [ (normalizeText name, [entry])
                | (prodName, entry) <- entries
                , name <- prodName : extractProductPrefixes prodName
                , not (T.null (normalizeText name))
                ]
        -- Index by synonym group (for synonym matching)
        bySynonym =
            M.fromListWith
                (++)
                [ (groupId, [entry])
                | (prodName, entry) <- entries
                , Just groupId <- [lookupSynonymGroup synDB (normalizeName prodName)]
                ]
     in IndexedDatabase
            { idbName = dbName
            , idbByProductName = byName
            , idbBySynonymGroup = bySynonym
            }

-- | Build supplier entries from a full Database
buildSupplierEntriesFromDB :: Database -> [(Text, SupplierEntry)]
buildSupplierEntriesFromDB db =
    [ (flowName flow, SupplierEntry actUUID prodUUID (activityLocation act) (activityUnit act) (flowName flow))
    | (pid, (actUUID, prodUUID)) <- zip ([0 ..] :: [Int]) (V.toList (dbProcessIdTable db))
    , Just act <- [getActivity db (fromIntegral pid)]
    , ex <- exchanges act
    , isReferenceExchange ex
    , Just flow <- [M.lookup (getExchangeFlowId ex) (dbFlows db)]
    ]
  where
    isReferenceExchange :: Exchange -> Bool
    isReferenceExchange (TechnosphereExchange _ _ _ _ isRef _ _ _) = isRef
    isReferenceExchange _ = False

    getExchangeFlowId :: Exchange -> UUID
    getExchangeFlowId (TechnosphereExchange fid _ _ _ _ _ _ _) = fid
    getExchangeFlowId (BiosphereExchange fid _ _ _ _) = fid

{- | Find a supplier across all loaded databases (using pre-built indexes)
This is the fast O(1) lookup version
-}
findSupplierInIndexedDBs ::
    LinkingContext ->
    -- | Product name to find
    Text ->
    -- | Location of the consumer
    Text ->
    -- | Unit of the exchange
    Text ->
    CrossDBLinkResult
findSupplierInIndexedDBs LinkingContext{..} productName location unit =
    let normalizedName = normalizeText productName
        -- Try exact match first (O(1) lookup)
        exactCandidates = concatMap (lookupExact normalizedName) lcIndexedDatabases
        -- Try synonym match if no exact match
        synonymCandidates =
            if null exactCandidates
                then case lookupSynonymGroup lcSynonymDB (normalizeName productName) of
                    Just groupId -> concatMap (lookupBySynonym groupId) lcIndexedDatabases
                    Nothing -> []
                else []
        -- Fallback: try prefix-based splitting for compound names (e.g. SimaPro)
        prefixCandidates =
            if null exactCandidates && null synonymCandidates
                then tryPrefixes (extractProductPrefixes productName)
                else []
        allCandidates = exactCandidates ++ synonymCandidates ++ prefixCandidates
        -- Effective location: if raw location is empty, try extracting from compound name
        effectiveLocation =
            if T.null location
                then extractBracketedLocation productName
                else location
     in if null allCandidates
            then CrossDBNotLinked NoNameMatch
            else
                -- Check unit compatibility first
                let unitCompatible = filter (\(_, se) -> unitsAreCompatible lcUnitConfig unit (seUnit se)) allCandidates
                 in if null unitCompatible
                        then
                            -- All candidates failed unit check — report the first supplier's unit
                            let (_, firstSe) = head allCandidates
                             in CrossDBNotLinked (UnitIncompatible unit (seUnit firstSe))
                        else
                            -- Score by effective location
                            let scoredCandidates = map (scoreEntry effectiveLocation) unitCompatible
                                !best = maximumBy (comparing cdbScore) scoredCandidates
                             in if cdbScore best >= lcThreshold
                                    then
                                        -- Build warnings (only when original location was explicit)
                                        let warnings = if T.null location then [] else buildWarnings effectiveLocation (cdbLocation best)
                                         in CrossDBLinked
                                                (cdbActivityUUID best)
                                                (cdbProductUUID best)
                                                (cdbDatabaseName best)
                                                (cdbScore best)
                                                (cdbProductName best)
                                                (cdbLocation best)
                                                warnings
                                    else CrossDBNotLinked (LocationUnavailable effectiveLocation)
  where
    lookupExact :: Text -> IndexedDatabase -> [(Text, SupplierEntry)]
    lookupExact name idb =
        [(idbName idb, entry) | entry <- fromMaybe [] (M.lookup name (idbByProductName idb))]

    lookupBySynonym :: Int -> IndexedDatabase -> [(Text, SupplierEntry)]
    lookupBySynonym groupId idb =
        [(idbName idb, entry) | entry <- fromMaybe [] (M.lookup groupId (idbBySynonymGroup idb))]

    -- Try each prefix from compound name splitting, return first match
    tryPrefixes :: [Text] -> [(Text, SupplierEntry)]
    tryPrefixes [] = []
    tryPrefixes (p : ps) =
        let normalized = normalizeText p
            candidates = concatMap (lookupExact normalized) lcIndexedDatabases
         in if null candidates
                then -- Also try synonym match for this prefix
                    case lookupSynonymGroup lcSynonymDB (normalizeName p) of
                        Just groupId ->
                            let synCandidates = concatMap (lookupBySynonym groupId) lcIndexedDatabases
                             in if null synCandidates then tryPrefixes ps else synCandidates
                        Nothing -> tryPrefixes ps
                else candidates

    scoreEntry :: Text -> (Text, SupplierEntry) -> CrossDBCandidate
    scoreEntry queryLoc (dbName, SupplierEntry{..}) =
        let locScore = matchLocation lcLocationHierarchy queryLoc seLocation
            nameScore = 50
            !totalScore = nameScore + locScore
         in CrossDBCandidate
                { cdbActivityUUID = seActivityUUID
                , cdbProductUUID = seProductUUID
                , cdbDatabaseName = dbName
                , cdbScore = totalScore
                , cdbLocation = seLocation
                , cdbProductName = seProductName
                }

    buildWarnings :: Text -> Text -> [LinkWarning]
    buildWarnings queryLoc actualLoc
        | queryLoc == actualLoc = []
        | otherwise = [UpperLocationUsed queryLoc actualLoc]

-- | Legacy function for backward compatibility (slower, builds indexes on the fly)
findSupplierAcrossDatabases ::
    LinkingContext ->
    -- | Product name to find
    Text ->
    -- | Location of the consumer
    Text ->
    -- | Unit of the exchange
    Text ->
    CrossDBLinkResult
findSupplierAcrossDatabases ctx productName location unit =
    -- Just delegate to the indexed version
    findSupplierInIndexedDBs ctx productName location unit

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
matchLocation :: M.Map Text [Text] -> Text -> Text -> Int
matchLocation hier queryLoc candidateLoc
    | queryLoc == candidateLoc = 30 -- Exact
    | isSubregionOf hier queryLoc candidateLoc = 20 -- Widening (FR→GLO, FR→RER)
    | candidateLoc `elem` ["GLO", "RoW", "Unspecified"] = 10 -- Global fallback
    | isSubregionOf hier candidateLoc queryLoc = 0 -- Narrowing (GLO→FR) — blocked
    | otherwise = 5 -- Unrelated

-- | Check if one location is a subregion of another
isSubregionOf :: M.Map Text [Text] -> Text -> Text -> Bool
isSubregionOf hier child parent =
    case M.lookup child hier of
        Just parents -> parent `elem` parents
        Nothing -> False

{- | Location hierarchy for common LCA regions
Maps a location code to its parent regions
-}
locationHierarchy :: M.Map Text [Text]
locationHierarchy =
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

-- | Check if two units are compatible for linking
unitsAreCompatible :: UC.UnitConfig -> Text -> Text -> Bool
unitsAreCompatible cfg unit1 unit2
    | normalizeText unit1 == normalizeText unit2 = True -- Same unit (after normalization)
    | otherwise = UC.unitsCompatible cfg unit1 unit2 -- Dimensionally compatible

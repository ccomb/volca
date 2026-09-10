{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Dimensional unit conversion system.

Units are defined by:
1. A dimension vector (exponents for each base dimension)
2. A conversion factor to SI base units

Unit definitions are loaded at runtime from CSV files.
-}
module UnitConversion (
    -- * Types
    Dimension,
    UnitDef (..),
    UnitConfig (ucDimensionOrder, ucUnits, ucOriginalKeys, ucCanonical),
    mkUnitConfig,
    UnitDeclaration (..),

    -- * Loading
    defaultUnitConfig,
    buildFromCSV,
    addDeclaredUnits,
    mergeUnitConfigs,
    unitCount,

    -- * Operations
    normalizeUnit,
    isKnownUnit,
    unitsCompatible,
    convertUnit,
    lookupUnitDef,
    canonicalUnitFor,
    normalizeToCanonical,

    -- * Backward compatibility
    convertExchangeAmount,

    -- * Dimension parsing
    parseDimension,

    -- * Warnings
    UnknownUnitTracker,
    newUnknownUnitTracker,
    warnIfUnknownUnit,
    getUnknownUnits,
) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (..), decode)
import Data.Either (partitionEithers)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Store (Store)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Progress (ProgressLevel (Info), reportProgress)

{- | Dimension as exponent vector.
Order: [mass, length, time, energy, area, volume, count, currency]
-}
type Dimension = [Int]

-- | Unit definition: dimension + factor to convert to SI base units.
data UnitDef = UnitDef
    { udDimension :: !Dimension
    , udFactor :: !Double
    }
    deriving (Eq, Show, Generic)

instance NFData UnitDef
instance Store UnitDef

-- | Unit configuration loaded from CSV.
data UnitConfig = UnitConfig
    { ucDimensionOrder :: ![Text] -- ["mass", "length", "time", ...]
    , ucUnits :: !(M.Map Text UnitDef) -- normalized (lowercase, trimmed) keys
    , ucOriginalKeys :: !(M.Map Text Text) -- normalized -> original (for error messages)
    , ucCanonical :: !(M.Map Text Text) -- normalized -> reference unit of its dimension
    }
    deriving (Eq, Show, Generic)

instance NFData UnitConfig
instance Store UnitConfig

-- | Default dimension order.
defaultDimensionOrder :: [Text]
defaultDimensionOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]

-- | Normalize unit string for lookup (lowercase, trim whitespace).
normalizeUnit :: Text -> Text
normalizeUnit = T.toLower . T.strip

-- | Check if a unit is known in the config.
isKnownUnit :: UnitConfig -> Text -> Bool
isKnownUnit cfg u = M.member (normalizeUnit u) (ucUnits cfg)

-- | Look up a unit definition.
lookupUnitDef :: UnitConfig -> Text -> Maybe UnitDef
lookupUnitDef cfg u = M.lookup (normalizeUnit u) (ucUnits cfg)

-- | Check if two units are dimensionally compatible.
unitsCompatible :: UnitConfig -> Text -> Text -> Bool
unitsCompatible cfg u1 u2 =
    case (lookupUnitDef cfg u1, lookupUnitDef cfg u2) of
        (Just d1, Just d2) -> udDimension d1 == udDimension d2
        _ -> False

{- | Convert amount from one unit to another.
Returns Nothing if units are incompatible or unknown.
-}
convertUnit :: UnitConfig -> Text -> Text -> Double -> Maybe Double
convertUnit cfg fromUnit toUnit amount = do
    UnitDef dimFrom factorFrom <- lookupUnitDef cfg fromUnit
    UnitDef dimTo factorTo <- lookupUnitDef cfg toUnit
    if dimFrom == dimTo && factorTo /= 0
        then Just (amount * factorFrom / factorTo)
        else Nothing

{- | Canonical (reference) unit name for the dimension of a given unit.

The reference unit is the one whose factor is 1.0 in @units.csv@, normally the
SI base, but a dimension may instead pick the unit its characterization factors
are authored in. Radioactivity uses @kBq@ (not the SI @Bq@) because EF/ILCD
ionising-radiation CFs are defined per kBq, and energy uses @MJ@ (not the SI
joule) for the same reason. 'convertForCharacterization' normalizes a flow to
this reference before applying a result-expression CF, so the choice decides
what such a factor is read against.

A unit spelled two ways carries the same factor, so a dimension usually offers
several names at 1.0 ("mj" and "megajoule", "kg" and "kilogram"). The shortest
one wins, which is the symbol rather than the word: it is what a database
writes and what a practitioner reads, and it keeps the name a reference product
is recorded under from moving when a new spelling is added to the table.

Returns 'Nothing' if the input unit is unknown or its dimension defines no
reference unit.
-}
canonicalUnitFor :: UnitConfig -> Text -> Maybe Text
canonicalUnitFor cfg = flip M.lookup (ucCanonical cfg) . normalizeUnit

{- | Convert an amount to the canonical base unit of its dimension.
Returns '(canonicalUnitName, convertedAmount)'. 'Nothing' if the input unit is
unknown or its dimension has no base unit defined — callers must surface this
as a load-time failure (no silent fallback).
-}
normalizeToCanonical :: UnitConfig -> Text -> Double -> Maybe (Text, Double)
normalizeToCanonical cfg unitText amount = do
    canonical <- canonicalUnitFor cfg unitText
    converted <- convertUnit cfg unitText canonical amount
    Just (canonical, converted)

-- | Parse a dimension expression like "mass*length/time" into an exponent vector.
parseDimension :: [Text] -> Text -> Either Text Dimension
parseDimension dimOrder expr
    | T.null (T.strip expr) = Left "Empty dimension expression"
    | otherwise = do
        let baseVec = replicate (length dimOrder) 0
            (numExpr, denExpr) = case T.breakOn "/" expr of
                (num, rest)
                    | T.null rest -> (num, "")
                    | otherwise -> (num, T.drop 1 rest)
            numParts = filter (not . T.null) $ map T.strip $ T.splitOn "*" numExpr
            denParts =
                if T.null denExpr
                    then []
                    else
                        concatMap
                            (filter (not . T.null) . map T.strip . T.splitOn "*")
                            (T.splitOn "/" denExpr)
        vec1 <- foldlM (addExp dimOrder 1) baseVec numParts
        foldlM (addExp dimOrder (-1)) vec1 denParts
  where
    addExp :: [Text] -> Int -> Dimension -> Text -> Either Text Dimension
    addExp order delta vec dimName =
        case elemIndex dimName order of
            Just idx -> Right $ modifyAt idx (+ delta) vec
            Nothing ->
                Left $
                    "Unknown dimension: "
                        <> dimName
                        <> " (valid: "
                        <> T.intercalate ", " order
                        <> ")"

    modifyAt :: Int -> (Int -> Int) -> [Int] -> [Int]
    modifyAt idx f = zipWith (\i x -> if i == idx then f x else x) [0 ..]

    foldlM :: (b -> a -> Either e b) -> b -> [a] -> Either e b
    foldlM _ acc [] = Right acc
    foldlM f acc (x : xs) = case f acc x of
        Left err -> Left err
        Right acc' -> foldlM f acc' xs

-- | Build a UnitConfig from CSV content (three columns: name, dimension, factor).
buildFromCSV :: BL.ByteString -> Either Text UnitConfig
buildFromCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> T.pack err
        Right rows ->
            let dimOrder = defaultDimensionOrder
             in buildFromRows dimOrder (V.toList (rows :: V.Vector (Text, Text, Double)))
  where
    buildFromRows dimOrder rows = do
        pairs <- mapM (parseRow dimOrder) rows
        let units = M.fromList pairs
        Right (mkUnitConfig dimOrder units (M.mapWithKey const units))

    parseRow dimOrder (name, dimExpr, factor) = do
        dim <- parseDimension dimOrder dimExpr
        Right (normalizeUnit name, UnitDef dim factor)

{- | One row of the unit table a database file carries for itself: what the
unit is called, the unit it is expressed in, and how many of that it makes.

A file stating that @ha a@ is 10 000 @m2a@, or @tn.sh@ 907.18474 @kg@, is
saying what no table shipped with an engine can know about the spellings that
file uses.
-}
data UnitDeclaration = UnitDeclaration
    { declName :: !Text
    , declRelativeTo :: !Text
    , declHowMany :: !Double
    }
    deriving (Eq, Show)

{- | Lay a file's own unit table over the one the engine ships.

The shipped table wins wherever it has a row: its constants are exact where a
published list often rounds them, and a file that disagrees is reported rather
than followed. What the file adds is the spellings no shipped table can
enumerate, @tn.sh@ or @cm2a@ or @gal*@, sized against a unit already known. A
declaration is placed once the unit it is given in is known, whether from the
shipped table or from a declaration placed before it, so the pass repeats until
it places nothing new.

Two declarations this lookup cannot tell apart are the case that matters. One
published list states both @Mg@ and @mg@, and both @MBq@ and @mBq@: a lookup
that folds their case has one key for the pair, so placing them would let a
megagram overwrite the milligram and every milligram in the file arrive as a
tonne. Neither is placed and the pair is named, which leaves the reading where
it already was and says so.
-}
addDeclaredUnits :: UnitConfig -> [UnitDeclaration] -> (UnitConfig, [Text])
addDeclaredUnits cfg decls = (mkUnitConfig (ucDimensionOrder cfg) placed (M.mapWithKey const placed), notes)
  where
    placed :: M.Map Text UnitDef
    placed = settle (ucUnits cfg) added

    -- One declaration per key, dropping what the shipped table already answers
    -- for and what the file itself spells two ways for two sizes.
    added :: [UnitDeclaration]
    added =
        [ d
        | (key, d : rest) <- M.toList byKey
        , not (M.member key (ucUnits cfg))
        , all (agrees d) rest
        ]

    byKey :: M.Map Text [UnitDeclaration]
    byKey = M.fromListWith (<>) [(normalizeUnit (declName d), [d]) | d <- decls]

    agrees :: UnitDeclaration -> UnitDeclaration -> Bool
    agrees a b =
        normalizeUnit (declRelativeTo a) == normalizeUnit (declRelativeTo b)
            && abs (declHowMany a - declHowMany b) <= abs (declHowMany a) * 1.0e-9

    settle :: M.Map Text UnitDef -> [UnitDeclaration] -> M.Map Text UnitDef
    settle known pending
        | null done = known
        | otherwise = settle (M.union (M.fromList done) known) waiting
      where
        (waiting, done) = partitionEithers (map (place known) pending)

    place :: M.Map Text UnitDef -> UnitDeclaration -> Either UnitDeclaration (Text, UnitDef)
    place known d = maybe (Left d) (Right . (,) (normalizeUnit (declName d))) (against known d)

    against :: M.Map Text UnitDef -> UnitDeclaration -> Maybe UnitDef
    against known d = do
        UnitDef dim factor <- M.lookup (normalizeUnit (declRelativeTo d)) known
        pure (UnitDef dim (declHowMany d * factor))

    notes :: [Text]
    notes = say collapsed foldedTogether ++ say unplaceable statedButUnknown ++ say disagreeing sizedDifferently

    say :: Text -> [Text] -> [Text]
    say _ [] = []
    say what these = [what <> T.intercalate ", " these]

    collapsed, unplaceable, disagreeing :: Text
    collapsed = "the file tells apart units this lookup does not, so it reads neither and an amount in either keeps whatever the shipped table says: "
    unplaceable = "the file states a unit given in one this engine does not know, so it stays unknown: "
    disagreeing = "the file sizes a unit differently from the shipped table, which is the one used: "

    foldedTogether :: [Text]
    foldedTogether = [T.intercalate " beside " (map declName ds) | ds <- M.elems byKey, not (allAgree ds)]

    allAgree :: [UnitDeclaration] -> Bool
    allAgree [] = True
    allAgree (d : rest) = all (agrees d) rest

    statedButUnknown :: [Text]
    statedButUnknown =
        [ declName d <> " in " <> declRelativeTo d
        | d <- added
        , not (M.member (normalizeUnit (declName d)) placed)
        ]

    sizedDifferently :: [Text]
    sizedDifferently =
        [ declName d
        | ds <- M.elems byKey
        , allAgree ds
        , d <- take 1 ds
        , Just shipped <- [M.lookup (normalizeUnit (declName d)) (ucUnits cfg)]
        , Just mine <- [against (ucUnits cfg) d]
        , abs (udFactor mine - udFactor shipped) > abs (udFactor shipped) * 1.0e-9
        ]

{- | Merge multiple UnitConfigs (later entries override earlier ones).
| Merge unit configs. Later entries override earlier ones.
-}
mergeUnitConfigs :: [UnitConfig] -> UnitConfig
mergeUnitConfigs [] = defaultUnitConfig
mergeUnitConfigs cfgs@(first : _) =
    mkUnitConfig
        (ucDimensionOrder first)
        (M.unions (reverse $ map ucUnits cfgs))
        (M.unions (reverse $ map ucOriginalKeys cfgs))

-- | Number of unit definitions.
unitCount :: UnitConfig -> Int
unitCount = M.size . ucUnits

-- | Minimal bootstrap unit config (kg, m, s, item) for when no CSV is loaded.
defaultUnitConfig :: UnitConfig
defaultUnitConfig =
    mkUnitConfig
        defaultDimensionOrder
        ( M.fromList
            [ ("kg", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0)
            , ("m", UnitDef [0, 1, 0, 0, 0, 0, 0, 0] 1.0)
            , ("s", UnitDef [0, 0, 1, 0, 0, 0, 0, 0] 1.0)
            , ("item", UnitDef [0, 0, 0, 0, 0, 0, 1, 0] 1.0)
            ]
        )
        (M.fromList [("kg", "kg"), ("m", "m"), ("s", "s"), ("item", "item")])

{- | Assemble a config, indexing the reference unit of every dimension once.

'canonicalUnitFor' is asked once per row of every file read, and answering it
by scanning the table is the whole table walked per row. The answer depends
only on the table, so it is computed here.
-}
mkUnitConfig :: [Text] -> M.Map Text UnitDef -> M.Map Text Text -> UnitConfig
mkUnitConfig dimOrder units originalKeys =
    UnitConfig
        { ucDimensionOrder = dimOrder
        , ucUnits = units
        , ucOriginalKeys = originalKeys
        , ucCanonical = M.mapMaybe (\(UnitDef dim _) -> snd <$> M.lookup dim references) units
        }
  where
    references =
        M.fromListWith
            shorter
            [ (udDimension def, (normKey, M.findWithDefault normKey normKey originalKeys))
            | (normKey, def) <- M.toList units
            , udFactor def == 1.0
            ]
    shorter a b = if sortKey a <= sortKey b then a else b
    sortKey (normKey, _) = (T.length normKey, normKey)

-- | Tracker for unknown units encountered during parsing.
data UnknownUnitTracker = UnknownUnitTracker
    { uutSeen :: !(IORef (S.Set Text))
    , uutConfig :: !UnitConfig
    }

-- | Create a new tracker.
newUnknownUnitTracker :: UnitConfig -> IO UnknownUnitTracker
newUnknownUnitTracker cfg = do
    seen <- newIORef S.empty
    return UnknownUnitTracker{uutSeen = seen, uutConfig = cfg}

-- | Warn if a unit is unknown (deduplicated).
warnIfUnknownUnit :: UnknownUnitTracker -> Text -> IO ()
warnIfUnknownUnit tracker unit = do
    let normalized = normalizeUnit unit
    unless (isKnownUnit (uutConfig tracker) unit || T.null normalized) $ do
        seen <- readIORef (uutSeen tracker)
        unless (S.member normalized seen) $ do
            modifyIORef' (uutSeen tracker) (S.insert normalized)
            reportProgress Info $
                "[WARNING] Unknown unit: \""
                    <> T.unpack unit
                    <> "\" - add to [[units]] CSV"

-- | Get all unknown units encountered so far.
getUnknownUnits :: UnknownUnitTracker -> IO (S.Set Text)
getUnknownUnits tracker = readIORef (uutSeen tracker)

{- | Convert an amount from one unit to another.
Returns the original amount if conversion fails.
-}
convertExchangeAmount :: UnitConfig -> Text -> Text -> Double -> Double
convertExchangeAmount cfg fromUnit toUnit amount =
    fromMaybe amount (convertUnit cfg fromUnit toUnit amount)

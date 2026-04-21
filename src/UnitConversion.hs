{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    UnitConfig (..),

    -- * Loading
    defaultUnitConfig,
    buildFromCSV,
    mergeUnitConfigs,
    unitCount,

    -- * Operations
    normalizeUnit,
    isKnownUnit,
    unitsCompatible,
    convertUnit,
    lookupUnitDef,

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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

-- | Unit configuration loaded from CSV.
data UnitConfig = UnitConfig
    { ucDimensionOrder :: ![Text] -- ["mass", "length", "time", ...]
    , ucUnits :: !(M.Map Text UnitDef) -- normalized (lowercase, trimmed) keys
    , ucOriginalKeys :: !(M.Map Text Text) -- normalized -> original (for error messages)
    }
    deriving (Show, Generic)

instance NFData UnitConfig

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
    modifyAt idx f xs = zipWith (\i x -> if i == idx then f x else x) [0 ..] xs

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
        Right
            UnitConfig
                { ucDimensionOrder = dimOrder
                , ucUnits = units
                , ucOriginalKeys = M.mapWithKey (\k _ -> k) units
                }

    parseRow dimOrder (name, dimExpr, factor) = do
        dim <- parseDimension dimOrder dimExpr
        Right (normalizeUnit name, UnitDef dim factor)

{- | Merge multiple UnitConfigs (later entries override earlier ones).
| Merge unit configs. Later entries override earlier ones.
-}
mergeUnitConfigs :: [UnitConfig] -> UnitConfig
mergeUnitConfigs [] = defaultUnitConfig
mergeUnitConfigs cfgs =
    UnitConfig
        { ucDimensionOrder = ucDimensionOrder (head cfgs)
        , ucUnits = M.unions (reverse $ map ucUnits cfgs)
        , ucOriginalKeys = M.unions (reverse $ map ucOriginalKeys cfgs)
        }

-- | Number of unit definitions.
unitCount :: UnitConfig -> Int
unitCount = M.size . ucUnits

-- | Minimal bootstrap unit config (kg, m, s, item) for when no CSV is loaded.
defaultUnitConfig :: UnitConfig
defaultUnitConfig =
    UnitConfig
        { ucDimensionOrder = defaultDimensionOrder
        , ucUnits =
            M.fromList
                [ ("kg", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0)
                , ("m", UnitDef [0, 1, 0, 0, 0, 0, 0, 0] 1.0)
                , ("s", UnitDef [0, 0, 1, 0, 0, 0, 0, 0] 1.0)
                , ("item", UnitDef [0, 0, 0, 0, 0, 0, 1, 0] 1.0)
                ]
        , ucOriginalKeys = M.fromList [("kg", "kg"), ("m", "m"), ("s", "s"), ("item", "item")]
        }

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
    case convertUnit cfg fromUnit toUnit amount of
        Just converted -> converted
        Nothing -> amount

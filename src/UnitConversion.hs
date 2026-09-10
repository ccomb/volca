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
    UnitConfig (ucDimensionOrder, ucUnits, ucByFold, ucCanonical),
    UnitReading (..),
    mkUnitConfig,

    -- * Loading
    defaultUnitConfig,
    buildFromCSV,
    mergeUnitConfigs,
    unitCount,

    -- * Operations
    foldedUnit,
    readUnit,
    tableSpelling,
    unitKey,
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
    UnitVerdict (..),
    judgeUnits,
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
import Data.Maybe (fromMaybe, isJust)
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

{- | Unit configuration loaded from CSV.

Keys are the spelling the table itself writes, trimmed and nothing else, because
case is what separates a millijoule from a megajoule. 'ucByFold' is the index
that answers a source spelling the table does not hold exactly: it maps a
case-folded key to every spelling that folds onto it, which is what lets
'readUnit' tell one candidate from several.
-}
data UnitConfig = UnitConfig
    { ucDimensionOrder :: ![Text] -- ["mass", "length", "time", ...]
    , ucUnits :: !(M.Map Text UnitDef) -- the table's own spelling -> its definition
    , ucByFold :: !(M.Map Text [(Text, UnitDef)]) -- case-folded key -> the spellings under it
    , ucCanonical :: !(M.Map Text Text) -- the table's own spelling -> reference unit of its dimension
    }
    deriving (Eq, Show, Generic)

instance NFData UnitConfig
instance Store UnitConfig

-- | Default dimension order.
defaultDimensionOrder :: [Text]
defaultDimensionOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]

{- | The case-blind key a spelling is filed under.

This is not a lookup key: two units may share it and mean a factor of a billion
apart. It is only how 'readUnit' gathers the candidates a spelling could mean.
-}
foldedUnit :: Text -> Text
foldedUnit = T.toCaseFold . T.strip

{- | What the table says about a spelling a source wrote.

The three answers the loader acts on are: the table spells it this way
('ReadExact'), one spelling in the table differs from it only by case, so the
reading is settled and the difference is worth saying ('ReadRespelt'), or
several do and nothing in the table decides between them ('ReadAmbiguous').
'ReadUnknown' is the fourth and simplest: nothing in the table resembles it.
-}
data UnitReading
    = ReadExact !UnitDef
    | ReadRespelt !Text !UnitDef
    | -- | at least two spellings, hence two fields before the rest
      ReadAmbiguous !Text !Text ![Text]
    | ReadUnknown
    deriving (Eq, Show)

-- | Read a source's spelling against the table.
readUnit :: UnitConfig -> Text -> UnitReading
readUnit cfg written = case M.lookup asked (ucUnits cfg) of
    Just def -> ReadExact def
    Nothing -> case M.findWithDefault [] (foldedUnit asked) (ucByFold cfg) of
        [] -> ReadUnknown
        [(spelling, def)] -> ReadRespelt spelling def
        ((a, _) : (b, _) : rest) -> ReadAmbiguous a b (map fst rest)
  where
    asked :: Text
    asked = T.strip written

{- | The table's own spelling of what a source wrote, when the table settles it.

'Nothing' where 'readUnit' refuses, so everything keyed on a unit name is keyed
on one spelling rather than on however a source happened to write it.
-}
tableSpelling :: UnitConfig -> Text -> Maybe Text
tableSpelling cfg written = case readUnit cfg written of
    ReadExact _ -> Just (T.strip written)
    ReadRespelt spelling _ -> Just spelling
    ReadAmbiguous{} -> Nothing
    ReadUnknown -> Nothing

-- | Whether the table settles this spelling. An ambiguous one is not known.
isKnownUnit :: UnitConfig -> Text -> Bool
isKnownUnit cfg = isJust . lookupUnitDef cfg

{- | The one spelling two source spellings must share to name the same unit.

The table's own spelling where it settles the reading, and the source's own,
trimmed, where it does not. Two spellings the table knows are the same unit
only if they lead to the same row, so @kwh@ and @kWh@ do and @kg@ and
@kilogram@ do not, exactly as before; two it does not know are the same only
written the same way, because comparing those case-blind is what would let a
millijoule pass for a megajoule.
-}
unitKey :: UnitConfig -> Text -> Text
unitKey cfg written = fromMaybe (T.strip written) (tableSpelling cfg written)

{- | The definition a spelling resolves to, or 'Nothing' when the table refuses.

Conversion keeps its 'Maybe' shape: a spelling the table settles converts, one
it cannot converts nowhere. Which of the two refusals happened, and the
respelling worth reporting, are 'readUnit' questions, and the load-time gate in
"Database.Loader" is where they are answered to the user.
-}
lookupUnitDef :: UnitConfig -> Text -> Maybe UnitDef
lookupUnitDef cfg written = case readUnit cfg written of
    ReadExact def -> Just def
    ReadRespelt _ def -> Just def
    ReadAmbiguous{} -> Nothing
    ReadUnknown -> Nothing

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
canonicalUnitFor cfg written = tableSpelling cfg written >>= flip M.lookup (ucCanonical cfg)

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
    buildFromRows :: [Text] -> [(Text, Text, Double)] -> Either Text UnitConfig
    buildFromRows dimOrder rows = do
        pairs <- mapM (parseRow dimOrder) rows
        mkUnitConfig dimOrder <$> withoutRepeats pairs

    parseRow :: [Text] -> (Text, Text, Double) -> Either Text (Text, UnitDef)
    parseRow dimOrder (name, dimExpr, factor) = do
        dim <- parseDimension dimOrder dimExpr
        Right (T.strip name, UnitDef dim factor)

    {- A table that spells one unit twice has two factors for it and no way to
    say which is meant, and 'M.fromList' would keep the last in silence. -}
    withoutRepeats :: [(Text, UnitDef)] -> Either Text (M.Map Text UnitDef)
    withoutRepeats pairs = case M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(k, 1) | (k, _) <- pairs])) of
        [] -> Right (M.fromList pairs)
        repeats -> Left $ "unit spelled more than once: " <> T.intercalate ", " repeats

{- | Merge multiple UnitConfigs (later entries override earlier ones).
| Merge unit configs. Later entries override earlier ones.
-}
mergeUnitConfigs :: [UnitConfig] -> UnitConfig
mergeUnitConfigs [] = defaultUnitConfig
mergeUnitConfigs cfgs@(first : _) =
    mkUnitConfig
        (ucDimensionOrder first)
        (M.unions (reverse $ map ucUnits cfgs))

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

{- | Assemble a config, indexing once what every read of a unit would rescan.

'canonicalUnitFor' is asked once per row of every file read, and answering it
by scanning the table is the whole table walked per row. 'readUnit' would
likewise fold the whole table to gather the spellings a source's could mean.
Both answers depend only on the table, so both are computed here.
-}
mkUnitConfig :: [Text] -> M.Map Text UnitDef -> UnitConfig
mkUnitConfig dimOrder units =
    UnitConfig
        { ucDimensionOrder = dimOrder
        , ucUnits = units
        , -- sorted, so the candidates a refusal names come out in the same order every time
          ucByFold = M.fromListWith (<>) [(foldedUnit spelling, [(spelling, def)]) | (spelling, def) <- M.toDescList units]
        , ucCanonical = M.mapMaybe (\(UnitDef dim _) -> M.lookup dim references) units
        }
  where
    references :: M.Map Dimension Text
    references =
        M.fromListWith
            shorter
            [ (udDimension def, spelling)
            | (spelling, def) <- M.toList units
            , udFactor def == 1.0
            ]
    -- The symbol rather than the word, per 'canonicalUnitFor'.
    shorter :: Text -> Text -> Text
    shorter a b = if (T.length a, a) <= (T.length b, b) then a else b

{- | What the table says about the units one database declares.

Pure, because the answer depends on two tables and nothing else; naming it to
the user is the caller's half. The four lists are every answer worth reporting,
an exact spelling nothing else lands on being the silent one.
-}
data UnitVerdict = UnitVerdict
    { uvRespelt :: ![(Text, Text)]
    -- ^ as the database writes it, as the table does
    , uvAmbiguous :: ![(Text, [Text])]
    -- ^ as the database writes it, the table spellings it could equally mean
    , uvCollapsed :: ![(Text, [Text])]
    -- ^ one table spelling, and the several the database distinguishes under it
    , uvUnknown :: ![Text]
    -- ^ nothing in the table resembles it
    }
    deriving (Eq, Show)

{- | Read every unit a database declares against the table, keeping each answer.

The collapse is the one answer no single reading can give. A database writing
both @Mg@ and @mg@ against a table holding only @mg@ has each of them read as
that one row, and each reading on its own looks settled; it is the pair that
says the source tells apart two units the table does not, and reading either is
then a guess worth a factor. So it is judged over the declared set, not one
name at a time.
-}
judgeUnits :: UnitConfig -> [Text] -> UnitVerdict
judgeUnits cfg written =
    UnitVerdict
        { uvRespelt = [(u, spelling) | (u, ReadRespelt spelling _) <- readings, not (collapsedRow spelling)]
        , uvAmbiguous = [(u, a : b : rest) | (u, ReadAmbiguous a b rest) <- readings]
        , uvCollapsed = M.toList collapsed
        , uvUnknown = [u | (u, ReadUnknown) <- readings]
        }
  where
    readings :: [(Text, UnitReading)]
    readings = [(u, readUnit cfg u) | u <- S.toList (S.fromList written), not (T.null (T.strip u))]

    -- The table spelling each settled reading landed on, and what landed there.
    landed :: M.Map Text (S.Set Text)
    landed =
        M.fromListWith
            S.union
            [ (row, S.singleton (T.strip u))
            | (u, reading) <- readings
            , row <- case reading of
                ReadExact _ -> [T.strip u]
                ReadRespelt spelling _ -> [spelling]
                ReadAmbiguous{} -> []
                ReadUnknown -> []
            ]

    collapsed :: M.Map Text [Text]
    collapsed = M.map S.toList (M.filter ((> 1) . S.size) landed)

    collapsedRow :: Text -> Bool
    collapsedRow = flip M.member collapsed

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
    let folded = foldedUnit unit
    unless (isKnownUnit (uutConfig tracker) unit || T.null folded) $ do
        seen <- readIORef (uutSeen tracker)
        unless (S.member folded seen) $ do
            modifyIORef' (uutSeen tracker) (S.insert folded)
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

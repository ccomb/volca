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
    UnitDeclaration (..),

    -- * Loading
    defaultDimensionOrder,
    defaultUnitConfig,
    buildFromCSV,
    addDeclaredUnits,
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
    convertOntoFactorBasis,
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
import Data.Either (partitionEithers)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Indexing (uniqueIndex)
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
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
Order: [mass, length, time, energy, count, currency, result]

Area and volume are not among them: they are powers of length, and a table
free to write both spellings is a table where one quantity has two vectors
that never convert. 'dimensionShorthands' is where the two are spent.

@result@ is the mirror of that argument and the reason 'resultDimension'
exists: two things that must never convert into one another cannot share a
vector, because the vector is the whole of what decides.
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
defaultDimensionOrder = ["mass", "length", "time", "energy", "count", "currency", resultDimension]

{- | The slot that separates a result expression from the quantity it is
expressed per.

@m3-world equivalents@ is what a water scarcity indicator states its result in:
a cubic metre of water weighted by how scarce water is where it was taken. It
is not a volume, and an exchange may not be stated in it, but a factor written
in it is written per cubic metre. Writing it @result*volume@ keeps both facts:
nothing converts it into a litre, and 'convertOntoFactorBasis' still reads it
as a factor per cubic metre.

A unit of the quantity itself leaves this slot at zero, which is every other
row of the table.
-}
resultDimension :: Text
resultDimension = "result"

{- | The dimension names that stand for a power of a base dimension.

A square metre is a length squared and a cubic metre a length cubed, so area
and volume are spellings rather than dimensions: with a slot of their own, a
density written @mass/volume@ and one written @mass/length/length/length@ are
two vectors for one quantity, and nothing converts between them or can notice
that it should. Both spellings stay writable and both land on the same vector.

Energy keeps a slot of its own rather than becoming
@mass*length*length/time/time@, because the unit its characterization factors
are authored in is @MJ@ and merging it would make a joule convert into a
newton metre and so into a torque.
-}
dimensionShorthands :: [(Text, (Text, Int))]
dimensionShorthands = [("area", ("length", 2)), ("volume", ("length", 3))]

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
convertUnit = convertRead id

{- | Convert an amount onto the basis a characterization factor is written per.

'convertUnit' with one difference: a result expression is read as the quantity
its result is expressed per, so a factor in @m3-world equivalents@ is reached
by a flow in litres and not by one in kilograms. Characterization is the only
question that wants that reading. Everywhere else a result expression is a
thing no exchange may be stated in and nothing may link against, which is what
'convertUnit' and 'unitsCompatible' answer.
-}
convertOntoFactorBasis :: UnitConfig -> Text -> Text -> Double -> Maybe Double
convertOntoFactorBasis cfg = convertRead (onFactorBasis (ucDimensionOrder cfg)) cfg

{- | 'convertUnit' and 'convertOntoFactorBasis', which differ only in how each
reads a dimension before comparing two of them.
-}
convertRead :: (Dimension -> Dimension) -> UnitConfig -> Text -> Text -> Double -> Maybe Double
convertRead readDim cfg fromUnit toUnit amount = do
    UnitDef dimFrom factorFrom <- lookupUnitDef cfg fromUnit
    UnitDef dimTo factorTo <- lookupUnitDef cfg toUnit
    if readDim dimFrom == readDim dimTo && factorTo /= 0
        then Just (amount * factorFrom / factorTo)
        else Nothing

{- | A dimension with its 'resultDimension' slot spent: what a factor written
in that unit is written per.

Read by the slot's index, not by walking the two lists together: an order
shorter than the vectors would otherwise drop the slots past its end, and every
dimension would compare equal to every other.
-}
onFactorBasis :: [Text] -> Dimension -> Dimension
onFactorBasis dimOrder = maybe id zeroAt (elemIndex resultDimension dimOrder)
  where
    zeroAt :: Int -> Dimension -> Dimension
    zeroAt idx = zipWith (\slot e -> if slot == idx then 0 else e) [0 :: Int ..]

{- | Canonical (reference) unit name for the dimension of a given unit.

The reference unit is the one whose factor is 1.0 in @units.csv@, normally the
SI base, but a dimension may instead pick the unit its characterization factors
are authored in. Radioactivity uses @kBq@ (not the SI @Bq@) because EF/ILCD
ionising-radiation CFs are defined per kBq, and energy uses @MJ@ (not the SI
joule) for the same reason.

A composed dimension has no factors of its own and picks for the other half of
the same argument: the unit its inventory data is written in. @m2a@, @m3a@,
@kgy@, @tkm@ and @pkm@ are what a source states a land occupation, a water
volume over time or a transport service in, and the SI product of the parts
(@m2s@, @kgm@) is written by nobody. 'convertForCharacterization' normalizes a flow to
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
unknown or its dimension has no base unit defined - callers must surface this
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
        case elemIndex base order of
            Just idx -> Right $ modifyAt idx (+ (delta * power)) vec
            Nothing ->
                Left $
                    "Unknown dimension: "
                        <> dimName
                        <> " (valid: "
                        <> T.intercalate ", " (order <> map fst dimensionShorthands)
                        <> ")"
      where
        -- The base dimension this name is written against, and its power.
        base :: Text
        power :: Int
        (base, power) = fromMaybe (dimName, 1) (lookup dimName dimensionShorthands)

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
    withoutRepeats pairs = case uniqueIndex pairs of
        Right table -> Right table
        Left repeats -> Left $ "unit spelled more than once: " <> T.intercalate ", " (NE.toList repeats)

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

A file that spells one unit twice for two sizes has said nothing this can
read, so neither is placed and the pair is named. A published list stating both
@Mg@ and @mg@ is not that case: those are two spellings, they are read as
written, and the table holds both.
-}
addDeclaredUnits :: UnitConfig -> [UnitDeclaration] -> (UnitConfig, [Text])
addDeclaredUnits cfg decls = (laid{ucCanonical = canonicals}, notes)
  where
    laid :: UnitConfig
    laid = mkUnitConfig (ucDimensionOrder cfg) placed

    placed :: M.Map Text UnitDef
    placed = settle (ucUnits cfg) added

    {- Which unit a dimension is recorded in stays the shipped table's to
    decide. 'mkUnitConfig' elects the shortest name at factor 1.0, and a file
    stating @Kl@ as one cubic metre would win that election on spelling alone:
    every volume in the file would then be recorded as @kl@, a name no other
    database, method or matrix knows, and a cross-database link out of it would
    fail to convert. -}
    canonicals :: M.Map Text Text
    canonicals = M.mapMaybe (flip M.lookup shippedReference . udDimension) placed

    shippedReference :: M.Map Dimension Text
    shippedReference =
        M.fromList
            [ (udDimension def, reference)
            | (spelling, def) <- M.toList (ucUnits cfg)
            , Just reference <- [M.lookup spelling (ucCanonical cfg)]
            ]

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
    byKey = M.fromListWith (<>) [(T.strip (declName d), [d]) | d <- decls]

    agrees :: UnitDeclaration -> UnitDeclaration -> Bool
    agrees a b =
        T.strip (declRelativeTo a) == T.strip (declRelativeTo b)
            && abs (declHowMany a - declHowMany b) <= abs (declHowMany a) * 1.0e-9

    settle :: M.Map Text UnitDef -> [UnitDeclaration] -> M.Map Text UnitDef
    settle known pending
        | null done = known
        | otherwise = settle (M.union (M.fromList done) known) waiting
      where
        (waiting, done) = partitionEithers (map (place known) pending)

    place :: M.Map Text UnitDef -> UnitDeclaration -> Either UnitDeclaration (Text, UnitDef)
    place known d = maybe (Left d) (Right . (,) (T.strip (declName d))) (against known d)

    against :: M.Map Text UnitDef -> UnitDeclaration -> Maybe UnitDef
    against known d = do
        UnitDef dim factor <- M.lookup (T.strip (declRelativeTo d)) known
        pure (UnitDef dim (declHowMany d * factor))

    notes :: [Text]
    notes = say collapsed saidTwice ++ say unplaceable statedButUnknown ++ say disagreeing sizedDifferently

    say :: Text -> [Text] -> [Text]
    say _ [] = []
    say what these = [what <> T.intercalate ", " these]

    collapsed, unplaceable, disagreeing :: Text
    collapsed = "the file spells one unit twice for two sizes, so it reads neither and an amount in it keeps whatever the shipped table says: "
    unplaceable = "the file states a unit given in one this engine does not know, so it stays unknown: "
    disagreeing = "the file sizes a unit differently from the shipped table, which is the one used: "

    saidTwice :: [Text]
    saidTwice = [T.intercalate " and " (map declName ds) | ds <- M.elems byKey, not (allAgree ds)]

    allAgree :: [UnitDeclaration] -> Bool
    allAgree [] = True
    allAgree (d : rest) = all (agrees d) rest

    statedButUnknown :: [Text]
    statedButUnknown =
        [ declName d <> " in " <> declRelativeTo d
        | d <- added
        , not (M.member (T.strip (declName d)) placed)
        ]

    sizedDifferently :: [Text]
    sizedDifferently =
        [ declName d
        | ds <- M.elems byKey
        , allAgree ds
        , d <- take 1 ds
        , Just shipped <- [M.lookup (T.strip (declName d)) (ucUnits cfg)]
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

-- | Number of unit definitions.
unitCount :: UnitConfig -> Int
unitCount = M.size . ucUnits

{- | Minimal bootstrap unit config (kg, m, s, item) for when no CSV is loaded.

The four vectors are read off 'defaultDimensionOrder' rather than written out.
A vector written by hand is a copy of the slot list with no author: it keeps
parsing the day a slot is added or removed, and the kilogram here quietly stops
being the kilogram the table parses, which 'mergeUnitConfigs' would then hold
both of. "UnitConversionSpec" pins the four against 'parseDimension'.
-}
defaultUnitConfig :: UnitConfig
defaultUnitConfig =
    mkUnitConfig
        defaultDimensionOrder
        ( M.fromList
            [ (name, UnitDef (onlySlot dim) 1.0)
            | (name, dim) <- [("kg", "mass"), ("m", "length"), ("s", "time"), ("item", "count")]
            ]
        )
  where
    -- 1 in the slot this base dimension occupies, 0 in every other.
    onlySlot :: Text -> Dimension
    onlySlot dim = [if slot == dim then 1 else 0 | slot <- defaultDimensionOrder]

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

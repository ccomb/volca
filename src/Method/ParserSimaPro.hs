{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for SimaPro method CSV exports.

SimaPro can export LCIA methods as CSV files with file type @{methods}@.
Each file contains one method with multiple impact categories, each listing
characterization factors as substance rows, followed by damage categories,
normalization factors, and weighting factors.
-}
module Method.ParserSimaPro (
    parseSimaProMethodCSV,
    parseSimaProMethodCSVBytes,
    isSimaProMethodCSV,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID.V5 as UUID5

import Method.Types
import SubstanceRegistry (nonEmptyCAS)

import SimaPro.Parser (
    SimaProConfig (..),
    decodeBS,
    defaultConfig,
    ensureUtf8,
    generateFlowUUID,
    normalizeSimaProCompartment,
    parseAmount,
    simaproNamespace,
    splitCSV,
 )

-- ============================================================================
-- Public API
-- ============================================================================

-- | Parse a SimaPro method CSV file from disk.
parseSimaProMethodCSV :: FilePath -> IO (Either String MethodCollection)
parseSimaProMethodCSV path = parseSimaProMethodCSVBytes <$> BS.readFile path

-- | Pure parser for SimaPro method CSV bytes.
parseSimaProMethodCSVBytes :: BS.ByteString -> Either String MethodCollection
parseSimaProMethodCSVBytes raw =
    let !utf8 = ensureUtf8 raw
        lns = BS8.lines utf8
        cfg = parseConfig lns
        methodologyName = parseMethodName cfg lns
        result = foldl' (step cfg) (initState methodologyName) lns
     in Right (finalize result)

{- | Detect whether bytes are a SimaPro method CSV export.
Checks for the {SimaPro...} header on line 1 and a method-type marker on
line 2.  SimaPro localises the file-type keyword: English "{methods}",
French "{méthodes}", German/Dutch "{methoden}", Italian "{metodi}",
Spanish "{métodos}" - all start with the ASCII prefix "{m" or "{M}".
Database exports use "{processes}", "{products}", etc., which do not.
-}
isSimaProMethodCSV :: BS.ByteString -> Bool
isSimaProMethodCSV bs =
    BS8.isPrefixOf "{SimaPro" bs && isMethodTypeLine bs
  where
    isMethodTypeLine bytes =
        case drop 1 (BS8.lines (BS.take 300 bytes)) of
            (l : _) -> BS8.isPrefixOf "{m" l || BS8.isPrefixOf "{M" l
            [] -> False

-- ============================================================================
-- Parser State
-- ============================================================================

{- | In-progress accumulators. Each carries exactly the data its stage needs,
so the parser can never hold (say) lingering CFs while reading an NW set.
-}
data CatAccum = CatAccum !Text !Text ![MethodCF]

data DamageAccum = DamageAccum !Text !Text ![(Text, Double)]
data NWAccum = NWAccum !Text !(M.Map Text Double) !(M.Map Text Double)

{- | The single source of truth for "where we are and what we're collecting".
Folds the old @Phase@ enum together with the per-section accumulators: an
impossible state (a phase disagreeing with its accumulator) is unrepresentable.
-}
data Stage
    = Header -- reading {key: value} header lines
    | MethodMeta -- skipping method-level metadata until a section marker
    | BetweenSections -- finished a block; expecting the next marker or End
    | NeedCatLine -- expecting "Name;Unit" after "Impact category"
    | NeedSubstances !Text !Text -- have cat name+unit; expecting "Substances"
    | ReadingCFs !CatAccum -- reading substance/CF rows
    | NeedDcLine -- expecting "Name;Unit" after "Damage category"
    | NeedDcImpacts !Text !Text -- expecting "Impact categories" marker
    | ReadingDcImpacts !DamageAccum -- reading impact rows of a damage category
    | NeedNWName -- expecting the NW-set name line
    | NeedNWSection !NWAccum -- expecting "Normalization"/"Weighting"/next marker
    | ReadingNorm !NWAccum -- reading normalization rows
    | ReadingWeight !NWAccum -- reading weighting rows

data ParseState = ParseState
    { psStage :: !Stage
    , psMethodology :: !Text -- constant; the method-level "Name"
    , psMethods :: ![Method] -- completed methods (reversed)
    , psDamageCats :: ![DamageCategory] -- completed damage categories (reversed)
    , psNWsets :: ![NormWeightSet] -- completed NW sets (reversed)
    }

initState :: Text -> ParseState
initState methodology = ParseState Header methodology [] [] []

-- ============================================================================
-- State Machine
-- ============================================================================

step :: SimaProConfig -> ParseState -> BS.ByteString -> ParseState
step cfg st line = case psStage st of
    Header
        | BS8.isPrefixOf "{" line -> st
        | otherwise -> st{psStage = MethodMeta}
    MethodMeta
        | Just m <- detectMarker stripped -> st{psStage = stageFor m}
        | otherwise -> st
    BetweenSections
        | Just m <- detectMarker stripped -> st{psStage = stageFor m}
        | otherwise -> st
    NeedCatLine
        | isBlank line -> st
        | otherwise ->
            let (name, unit) = parseNameUnit cfg line
             in st{psStage = NeedSubstances name unit}
    NeedSubstances name unit
        | isBlank line -> st
        | stripped == "Substances" -> st{psStage = ReadingCFs (CatAccum name unit [])}
        | otherwise -> st
    ReadingCFs acc
        | isBlank line -> finishCat acc st{psStage = BetweenSections}
        | stripped == "End" -> finishCat acc st{psStage = BetweenSections}
        | Just m <- detectMarker stripped -> finishCat acc st{psStage = stageFor m}
        | Just cf <- parseCFRow cfg line -> st{psStage = ReadingCFs (consCF cf acc)}
        | otherwise -> st
    NeedDcLine
        | isBlank line -> st
        | otherwise ->
            let (name, unit) = parseNameUnit cfg line
             in st{psStage = NeedDcImpacts name unit}
    NeedDcImpacts name unit
        | isBlank line -> st
        | stripped == "Impact categories" -> st{psStage = ReadingDcImpacts (DamageAccum name unit [])}
        | otherwise -> st
    ReadingDcImpacts acc
        | isBlank line -> finishDamage acc st{psStage = BetweenSections}
        | stripped == "End" -> finishDamage acc st{psStage = BetweenSections}
        | Just m <- detectMarker stripped -> finishDamage acc st{psStage = stageFor m}
        | Just nv <- parseNameValue cfg line -> st{psStage = ReadingDcImpacts (consImpact nv acc)}
        | otherwise -> st
    NeedNWName
        | isBlank line -> st
        | otherwise -> st{psStage = NeedNWSection (NWAccum (decodeBS stripped) M.empty M.empty)}
    NeedNWSection acc
        | isBlank line -> st
        | stripped == "Normalization" -> st{psStage = ReadingNorm acc}
        | stripped == "Weighting" -> st{psStage = ReadingWeight acc}
        | stripped == "End" -> finishNW acc st{psStage = BetweenSections}
        | Just m <- detectMarker stripped -> finishNW acc st{psStage = stageFor m}
        | otherwise -> st
    ReadingNorm acc
        | isBlank line -> st{psStage = NeedNWSection acc}
        | stripped == "Weighting" -> st{psStage = ReadingWeight acc}
        | stripped == "End" -> finishNW acc st{psStage = BetweenSections}
        | Just (n, v) <- parseNameValue cfg line -> st{psStage = ReadingNorm (insertNorm n v acc)}
        | otherwise -> st
    ReadingWeight acc
        | isBlank line -> st{psStage = NeedNWSection acc}
        | stripped == "End" -> finishNW acc st{psStage = BetweenSections}
        | Just m <- detectMarker stripped -> finishNW acc st{psStage = stageFor m}
        | Just (n, v) <- parseNameValue cfg line -> st{psStage = ReadingWeight (insertWeight n v acc)}
        | otherwise -> st
  where
    stripped = BS8.strip line

{- | Append the completed category. A header with zero CF rows still emits an
empty 'Method': a category can be declared before its factors are added, and
silently dropping it would hide that from downstream.
Shared by 'step' (mid-stream, on blanks/markers/End) and 'finalize' (at EOF).
-}
finishCat :: CatAccum -> ParseState -> ParseState
finishCat (CatAccum name unit factors) st =
    st{psMethods = buildMethod (psMethodology st) name unit (reverse factors) : psMethods st}

{- | Append the completed damage category, including one with zero impact rows
(same rationale as 'finishCat').
-}
finishDamage :: DamageAccum -> ParseState -> ParseState
finishDamage (DamageAccum name unit impacts) st =
    st{psDamageCats = DamageCategory name unit (reverse impacts) : psDamageCats st}

finishNW :: NWAccum -> ParseState -> ParseState
finishNW (NWAccum name norm weight) st
    | M.null norm && M.null weight = st
    | otherwise = st{psNWsets = NormWeightSet name norm weight : psNWsets st}

{- | Flush whatever block is in progress at end of input, then read out the
accumulated collections in source order.
-}
finalize :: ParseState -> MethodCollection
finalize st =
    let s = finishCurrent st
     in MethodCollection
            (reverse (psMethods s))
            (reverse (psDamageCats s))
            (reverse (psNWsets s))
            []

finishCurrent :: ParseState -> ParseState
finishCurrent st = case psStage st of
    ReadingCFs acc -> finishCat acc st
    ReadingDcImpacts acc -> finishDamage acc st
    ReadingNorm acc -> finishNW acc st
    ReadingWeight acc -> finishNW acc st
    NeedNWSection acc -> finishNW acc st
    -- Stages with no in-progress block to flush. Enumerated (not wildcarded)
    -- so a future accumulator-carrying stage can't silently skip its EOF flush.
    Header -> st
    MethodMeta -> st
    BetweenSections -> st
    NeedCatLine -> st
    NeedSubstances{} -> st
    NeedDcLine -> st
    NeedDcImpacts{} -> st
    NeedNWName -> st

consCF :: MethodCF -> CatAccum -> CatAccum
consCF cf (CatAccum name unit factors) = CatAccum name unit (cf : factors)

consImpact :: (Text, Double) -> DamageAccum -> DamageAccum
consImpact i (DamageAccum name unit impacts) = DamageAccum name unit (i : impacts)

insertNorm :: Text -> Double -> NWAccum -> NWAccum
insertNorm n v (NWAccum name norm weight) = NWAccum name (M.insert n v norm) weight

insertWeight :: Text -> Double -> NWAccum -> NWAccum
insertWeight n v (NWAccum name norm weight) = NWAccum name norm (M.insert n v weight)

buildMethod :: Text -> Text -> Text -> [MethodCF] -> Method
buildMethod methodology name unit factors =
    Method
        { methodId =
            UUID5.generateNamed
                simaproNamespace
                (BS.unpack $ TE.encodeUtf8 $ "method:" <> name)
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = unit
        , methodCategory = name
        , methodMethodology = Just methodology
        , methodFactors = factors
        }

-- ============================================================================
-- Line parsers
-- ============================================================================

{- | A line that begins a new section. "End" is deliberately not a marker - it
only closes the current block - so the reading stages handle it inline.
-}
data Marker = MImpactCat | MDamageCat | MNWSet

detectMarker :: BS.ByteString -> Maybe Marker
detectMarker s
    | s == "Impact category" = Just MImpactCat
    | s == "Damage category" = Just MDamageCat
    | isNWsetMarker s = Just MNWSet
    | otherwise = Nothing

stageFor :: Marker -> Stage
stageFor MImpactCat = NeedCatLine
stageFor MDamageCat = NeedDcLine
stageFor MNWSet = NeedNWName

{- | Parse one substance/CF row into a 'MethodCF', or 'Nothing' if the row is
too short to be a factor line.
-}
parseCFRow :: SimaProConfig -> BS.ByteString -> Maybe MethodCF
parseCFRow cfg line =
    case splitCSV (spDelimiter cfg) line of
        (comp : sub : name : cas : cfVal : cfUnit : _) ->
            let !rawName = decodeBS (BS8.strip name)
                -- Keep the full suffixed name so the CF's 'mcfFlowRef' UUID
                -- matches the suffixed biosphere flow UUID parsed by
                -- 'SimaPro.Parser.bioRowToExchange'. SimaPro CSV CFs are
                -- name-regionalized: each region is a distinct elementary flow
                -- carrying its own CF (as Brightway matches them), NOT
                -- consumer-location regionalized. So the CF resolves by full
                -- name/UUID and 'mcfConsumerLocation' stays 'Nothing'. Setting
                -- it routed region-tagged flows into the consumer-location
                -- dispatch, which resolves C[f, loc(activity)] and - the flow's
                -- region living in its name, not the activity's location -
                -- fell back to the region-less default (AWARE water 42.95 for
                -- every region instead of e.g. CH's 1.34, a ~54x over-count).
                !cfUnitT = decodeBS (BS8.strip cfUnit)
                -- UUID hashed via the shared 'generateFlowUUID' +
                -- 'normalizeSimaProCompartment' so the CF side and
                -- 'SimaPro.Parser.bioRowToExchange' produce the same UUID for
                -- the same flow. The unit stays out of the hash on both sides:
                -- a factor written per kg answers an inventory row written per
                -- g, and 'convertForCharacterization' puts the quantity on the
                -- factor's basis. 'mcfUnit' keeps the unit the method states.
                !flowRef =
                    generateFlowUUID
                        rawName
                        (normalizeSimaProCompartment (decodeBS comp) (decodeBS sub))
                !cf =
                    MethodCF
                        { mcfFlowRef = flowRef
                        , mcfFlowName = rawName
                        , mcfDirection = direction comp
                        , mcfValue = parseAmount (spDecimal cfg) (BS8.strip cfVal)
                        , mcfCompartment = mkCompartment comp sub
                        , mcfCAS = nonEmptyCAS (decodeBS (BS8.strip cas))
                        , mcfUnit = cfUnitT
                        , mcfConsumerLocation = Nothing
                        }
             in Just cf
        _ -> Nothing

{- | Parse a two-column @name;value@ row (damage impacts, normalization,
weighting), or 'Nothing' if the row lacks both columns.
-}
parseNameValue :: SimaProConfig -> BS.ByteString -> Maybe (Text, Double)
parseNameValue cfg line =
    case splitCSV (spDelimiter cfg) line of
        (name : val : _) ->
            Just (decodeBS (BS8.strip name), parseAmount (spDecimal cfg) (BS8.strip val))
        _ -> Nothing

-- | Parse a @Name;Unit@ header line. Total: a missing unit yields "".
parseNameUnit :: SimaProConfig -> BS.ByteString -> (Text, Text)
parseNameUnit cfg line =
    case splitCSV (spDelimiter cfg) line of
        (name : unit : _) -> (decodeBS (BS8.strip name), decodeBS (BS8.strip unit))
        [name] -> (decodeBS (BS8.strip name), "")
        [] -> ("", "")

-- ============================================================================
-- Helpers
-- ============================================================================

isNWsetMarker :: BS.ByteString -> Bool
isNWsetMarker s =
    BS8.isPrefixOf "Normalization-Weighting set" s
        || BS8.isPrefixOf "Normalisation-Weighting set" s

parseConfig :: [BS.ByteString] -> SimaProConfig
parseConfig = foldl' go defaultConfig
  where
    go cfg line
        | "{CSV separator: Semicolon}" `BS.isInfixOf` line = cfg{spDelimiter = ';'}
        | "{CSV separator: Comma}" `BS.isInfixOf` line = cfg{spDelimiter = ','}
        | "{CSV separator: Tab}" `BS.isInfixOf` line = cfg{spDelimiter = '\t'}
        | "{Decimal separator: .}" `BS.isInfixOf` line = cfg{spDecimal = '.'}
        | "{Decimal separator: ,}" `BS.isInfixOf` line = cfg{spDecimal = ','}
        | otherwise = cfg

parseMethodName :: SimaProConfig -> [BS.ByteString] -> Text
parseMethodName _cfg = go False
  where
    go _ [] = "SimaPro Method"
    go True (l : _) = decodeBS (BS8.strip l)
    go False (l : ls)
        | BS8.strip l == "Name" = go True ls
        | otherwise = go False ls

isBlank :: BS.ByteString -> Bool
isBlank = BS.null . BS8.strip

direction :: BS.ByteString -> FlowDirection
direction comp
    | lc == "raw" || lc == "resources" || "raw" `BS8.isPrefixOf` lc = Input
    | otherwise = Output
  where
    lc = BS8.map toLower (BS8.strip comp)

mkCompartment :: BS.ByteString -> BS.ByteString -> Maybe Compartment
mkCompartment comp sub =
    let medium = case BS8.map toLower (BS8.strip comp) of
            "air" -> "air"
            "water" -> "water"
            "soil" -> "soil"
            "raw" -> "natural resource"
            "resources" -> "natural resource"
            c -> decodeBS c
        subcomp =
            let s = decodeBS (BS8.strip sub)
             in if s == "(unspecified)" then "" else s
     in -- An empty compartment column must yield no compartment at all: a
        -- 'Compartment "" sub ""' would wrongly constrain flow matching and
        -- leak an empty path onto the API.
        if T.null medium
            then Nothing
            else Just (Compartment medium subcomp "")

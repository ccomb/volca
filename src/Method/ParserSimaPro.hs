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
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID5

import Method.Types
import SimaPro.Parser (
    SimaProConfig (..),
    decodeBS,
    defaultConfig,
    ensureUtf8,
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
        methodName' = parseMethodName cfg lns
        result = foldl' (step cfg methodName') initState lns
     in Right (finalize result)

{- | Detect whether bytes are a SimaPro method CSV export.
Checks for the {SimaPro...} header on line 1 and a method-type marker on
line 2.  SimaPro localises the file-type keyword: English "{methods}",
French "{méthodes}", German/Dutch "{methoden}", Italian "{metodi}",
Spanish "{métodos}" — all start with the ASCII prefix "{m" or "{M}".
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

data ParseState = ParseState
    { psPhase :: !Phase
    , psCatName :: !Text -- current impact category name
    , psCatUnit :: !Text -- current impact category unit
    , psFactors :: ![MethodCF] -- CFs accumulated (reversed) for current category
    , psMethods :: ![Method] -- completed methods (reversed)
    -- Damage categories
    , psDamageCats :: ![DamageCategory] -- completed damage categories (reversed)
    , psDcName :: !Text -- current damage category name
    , psDcUnit :: !Text -- current damage category unit
    , psDcImpacts :: ![(Text, Double)] -- current damage category impacts (reversed)
    -- Normalization/Weighting
    , psNWsets :: ![NormWeightSet] -- completed NW sets (reversed)
    , psNWname :: !Text -- current NW set name
    , psNormMap :: !(M.Map Text Double) -- current normalization factors
    , psWeightMap :: !(M.Map Text Double) -- current weighting factors
    }

data Phase
    = PhHeader -- reading {key: value} header lines
    | PhMethodMeta -- reading method-level metadata (Name, Version, etc.)
    | PhExpectCategory -- expecting "Impact category" or "Damage category" etc.
    | PhExpectCatLine -- expecting the "Name;Unit" line after "Impact category"
    | PhExpectSubst -- expecting blank or "Substances" marker
    | PhReadingCFs -- reading substance/CF rows
    -- Damage categories
    | PhExpectDcLine -- expecting "Name;Unit" after "Damage category"
    | PhExpectDcImpacts -- expecting "Impact categories" marker
    | PhReadingDcImpacts -- reading impact category rows in damage category
    -- Normalization/Weighting
    | PhExpectNWname -- expecting NW set name line
    | PhExpectNWsection -- expecting "Normalization" or "Weighting" or next section
    | PhReadingNorm -- reading normalization rows
    | PhReadingWeight -- reading weighting rows

initState :: ParseState
initState = ParseState PhHeader "" "" [] [] [] "" "" [] [] "" M.empty M.empty

-- ============================================================================
-- State Machine
-- ============================================================================

step :: SimaProConfig -> Text -> ParseState -> BS.ByteString -> ParseState
step cfg methodologyName st line = case psPhase st of
    PhHeader
        | BS8.isPrefixOf "{" line -> st
        | otherwise -> st{psPhase = PhMethodMeta}
    PhMethodMeta
        | stripped == "Impact category" -> st{psPhase = PhExpectCatLine}
        | stripped == "Damage category" -> st{psPhase = PhExpectDcLine}
        | isNWsetMarker stripped -> st{psPhase = PhExpectNWname}
        | otherwise -> st
    PhExpectCategory
        | stripped == "Impact category" -> st{psPhase = PhExpectCatLine}
        | stripped == "Damage category" -> st{psPhase = PhExpectDcLine}
        | isNWsetMarker stripped -> st{psPhase = PhExpectNWname}
        | stripped == "End" -> st
        | otherwise -> st
    PhExpectCatLine
        | isBlank line -> st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
                catName = decodeBS (BS8.strip (head' fields))
                catUnit =
                    if length fields > 1
                        then decodeBS (BS8.strip (fields !! 1))
                        else ""
             in st
                    { psPhase = PhExpectSubst
                    , psCatName = catName
                    , psCatUnit = catUnit
                    , psFactors = []
                    }
    PhExpectSubst
        | isBlank line -> st
        | stripped == "Substances" -> st{psPhase = PhReadingCFs}
        | otherwise -> st
    PhReadingCFs
        | isBlank line -> finishCategory st
        | stripped == "Impact category" ->
            (finishCategory st){psPhase = PhExpectCatLine}
        | stripped == "Damage category" ->
            (finishCategory st){psPhase = PhExpectDcLine}
        | isNWsetMarker stripped ->
            (finishCategory st){psPhase = PhExpectNWname}
        | stripped == "End" -> finishCategory st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
             in case fields of
                    (comp : sub : name : cas : cfVal : cfUnit : _) ->
                        let !cf =
                                MethodCF
                                    { mcfFlowRef = makeFlowUUID name comp sub
                                    , mcfFlowName = decodeBS (BS8.strip name)
                                    , mcfDirection = direction comp
                                    , mcfValue = parseAmount (spDecimal cfg) (BS8.strip cfVal)
                                    , mcfCompartment = mkCompartment comp sub
                                    , mcfCAS = normalizeCAS (decodeBS (BS8.strip cas))
                                    , mcfUnit = decodeBS (BS8.strip cfUnit)
                                    }
                         in st{psFactors = cf : psFactors st}
                    _ -> st
    -- Damage category parsing
    PhExpectDcLine
        | isBlank line -> st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
                dcn = decodeBS (BS8.strip (head' fields))
                dcu = if length fields > 1 then decodeBS (BS8.strip (fields !! 1)) else ""
             in st{psPhase = PhExpectDcImpacts, psDcName = dcn, psDcUnit = dcu, psDcImpacts = []}
    PhExpectDcImpacts
        | isBlank line -> st
        | stripped == "Impact categories" -> st{psPhase = PhReadingDcImpacts}
        | otherwise -> st
    PhReadingDcImpacts
        | isBlank line -> finishDamageCategory st
        | stripped == "Damage category" ->
            (finishDamageCategory st){psPhase = PhExpectDcLine}
        | isNWsetMarker stripped ->
            (finishDamageCategory st){psPhase = PhExpectNWname}
        | stripped == "End" -> finishDamageCategory st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
             in case fields of
                    (name : val : _) ->
                        let n = decodeBS (BS8.strip name)
                            v = parseAmount (spDecimal cfg) (BS8.strip val)
                         in st{psDcImpacts = (n, v) : psDcImpacts st}
                    _ -> st
    -- Normalization/Weighting parsing
    PhExpectNWname
        | isBlank line -> st
        | otherwise -> st{psPhase = PhExpectNWsection, psNWname = decodeBS (BS8.strip line)}
    PhExpectNWsection
        | isBlank line -> st
        | stripped == "Normalization" -> st{psPhase = PhReadingNorm}
        | stripped == "Weighting" -> st{psPhase = PhReadingWeight}
        | stripped == "Damage category" ->
            (finishNWset st){psPhase = PhExpectDcLine}
        | isNWsetMarker stripped ->
            (finishNWset st){psPhase = PhExpectNWname}
        | stripped == "End" -> finishNWset st
        | otherwise -> st
    PhReadingNorm
        | isBlank line -> st{psPhase = PhExpectNWsection}
        | stripped == "Weighting" -> st{psPhase = PhReadingWeight}
        | stripped == "End" -> finishNWset st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
             in case fields of
                    (name : val : _) ->
                        let n = decodeBS (BS8.strip name)
                            v = parseAmount (spDecimal cfg) (BS8.strip val)
                         in st{psNormMap = M.insert n v (psNormMap st)}
                    _ -> st
    PhReadingWeight
        | isBlank line -> st{psPhase = PhExpectNWsection}
        | isNWsetMarker stripped ->
            (finishNWset st){psPhase = PhExpectNWname}
        | stripped == "End" -> finishNWset st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
             in case fields of
                    (name : val : _) ->
                        let n = decodeBS (BS8.strip name)
                            v = parseAmount (spDecimal cfg) (BS8.strip val)
                         in st{psWeightMap = M.insert n v (psWeightMap st)}
                    _ -> st
  where
    stripped = BS8.strip line

    finishCategory s =
        let !m =
                Method
                    { methodId =
                        UUID5.generateNamed
                            simaproNamespace
                            (BS.unpack $ TE.encodeUtf8 $ "method:" <> psCatName s)
                    , methodName = psCatName s
                    , methodDescription = Nothing
                    , methodUnit = psCatUnit s
                    , methodCategory = psCatName s
                    , methodMethodology = Just methodologyName
                    , methodFactors = reverse (psFactors s)
                    }
         in s{psPhase = PhExpectCategory, psFactors = [], psMethods = m : psMethods s}

    finishDamageCategory s =
        let !dc = DamageCategory (psDcName s) (psDcUnit s) (reverse (psDcImpacts s))
         in s{psPhase = PhExpectCategory, psDcImpacts = [], psDamageCats = dc : psDamageCats s}

    finishNWset s
        | M.null (psNormMap s) && M.null (psWeightMap s) = s{psPhase = PhExpectCategory}
        | otherwise =
            let !nw = NormWeightSet (psNWname s) (psNormMap s) (psWeightMap s)
             in s
                    { psPhase = PhExpectCategory
                    , psNormMap = M.empty
                    , psWeightMap = M.empty
                    , psNWname = ""
                    , psNWsets = nw : psNWsets s
                    }

finalize :: ParseState -> MethodCollection
finalize st =
    let methods = case psPhase st of
            PhReadingCFs
                | not (null (psFactors st)) ->
                    let !m =
                            Method
                                { methodId =
                                    UUID5.generateNamed
                                        simaproNamespace
                                        (BS.unpack $ TE.encodeUtf8 $ "method:" <> psCatName st)
                                , methodName = psCatName st
                                , methodDescription = Nothing
                                , methodUnit = psCatUnit st
                                , methodCategory = psCatName st
                                , methodMethodology = Nothing
                                , methodFactors = reverse (psFactors st)
                                }
                     in reverse (m : psMethods st)
            _ -> reverse (psMethods st)
        -- Flush any pending NW set
        nwSets = case psPhase st of
            PhReadingWeight
                | not (M.null (psWeightMap st)) ->
                    let !nw = NormWeightSet (psNWname st) (psNormMap st) (psWeightMap st)
                     in reverse (nw : psNWsets st)
            PhReadingNorm
                | not (M.null (psNormMap st)) ->
                    let !nw = NormWeightSet (psNWname st) (psNormMap st) (psWeightMap st)
                     in reverse (nw : psNWsets st)
            _ -> reverse (psNWsets st)
     in MethodCollection methods (reverse (psDamageCats st)) nwSets []

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

head' :: [a] -> a
head' (x : _) = x
head' [] = error "Method.ParserSimaPro: unexpected empty field list"

makeFlowUUID :: BS.ByteString -> BS.ByteString -> BS.ByteString -> UUID
makeFlowUUID name comp sub =
    let compartment = decodeBS (BS8.strip comp) <> "/" <> decodeBS (BS8.strip sub)
     in UUID5.generateNamed
            simaproNamespace
            (BS.unpack $ TE.encodeUtf8 $ "flow:" <> decodeBS (BS8.strip name) <> ":" <> compartment <> ":" <> "kg")

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
     in Just (Compartment medium subcomp "")

normalizeCAS :: Text -> Maybe Text
normalizeCAS cas
    | T.null cas = Nothing
    | otherwise =
        let segments = T.splitOn "-" cas
            stripped = map (T.dropWhile (== '0')) segments
            fixed = map (\s -> if T.null s then "0" else s) stripped
            result = T.intercalate "-" fixed
         in if T.all (\c -> c == '-' || c == '0') cas
                then Nothing
                else Just result

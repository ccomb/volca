{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | SimaPro CSV Parser for volca
Parses SimaPro CSV exports (like Agribalyse) into volca data structures
-}
module SimaPro.Parser (
    parseSimaProCSV,
    SimaProConfig (..),
    ProcessBlock (..),
    ProductRow (..),
    TechExchangeRow (..),
    BioExchangeRow (..),
    GlobalParams (..),
    emptyProcessBlock,
    fallbackAmounts,
    dropAmbiguousNativeIds,
    generateActivityUUID,
    generateFlowUUID,
    generateUnitUUID,
    canonicalRow,
    unitDeclarations,
    normalizeSimaProCompartment,
    indexFlows,
    extractLocation,
    Located (..),
    NameReading (..),

    -- * Shared utilities (used by Method.ParserSimaPro)
    defaultConfig,
    simaproNamespace,
    ensureUtf8,
    splitCSV,
    parseAmount,
    parseProductRow,
    parseTechRow,
    parseBioRow,
    parsePedigreePrefix,
    isMetadataKey,
    decodeBS,
) where

import Amount (readAmount)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isUpper, toLower)
import qualified Data.Csv as Csv
import Data.List (dropWhileEnd, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import qualified Expr
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import Progress (ProgressLevel (..), reportProgress)
import SubstanceRegistry (CASNumber (..), NormName (..), casBindings, normalizeCAS)
import SynonymDB (normalizeName)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types
import qualified UnitConversion

-- ============================================================================
-- Configuration Types
-- ============================================================================

-- | SimaPro file configuration extracted from header
data SimaProConfig = SimaProConfig
    { spVersion :: !Text -- SimaPro version (e.g. "9.6.0.1")
    , spFileType :: !Text -- "processes", "methods", "product stages"
    , spDelimiter :: !Char -- CSV delimiter (';', ',', '\t')
    , spDecimal :: !Char -- Decimal separator (',' or '.')
    , spDateFormat :: !Text -- Date format string
    }
    deriving (Show, Eq, Generic)

instance NFData SimaProConfig

-- | Default configuration (semicolon delimiter, comma decimal)
defaultConfig :: SimaProConfig
defaultConfig =
    SimaProConfig
        { spVersion = ""
        , spFileType = "processes"
        , spDelimiter = ';'
        , spDecimal = ','
        , spDateFormat = "dd/MM/yyyy"
        }

-- ============================================================================
-- Intermediate Row Types
-- ============================================================================

-- | Product row (reference output)
data ProductRow = ProductRow
    { prName :: !Text
    , prUnit :: !Text
    , prAmount :: !Double
    , prAmountRaw :: !Text
    , prAllocation :: !Double
    , prAllocRaw :: !Text
    , prWasteType :: !Text
    , prCategory :: !Text
    , prComment :: !Text
    }
    deriving (Show, Eq, Generic)

instance NFData ProductRow

-- | Technosphere exchange row (inputs from other processes)
data TechExchangeRow = TechExchangeRow
    { terName :: !Text
    , terUnit :: !Text
    , terAmount :: !Double
    , terAmountRaw :: !Text
    , terUncertainty :: !Text
    , terComment :: !Text
    }
    deriving (Show, Eq, Generic)

instance NFData TechExchangeRow

-- | Biosphere exchange row (emissions/resources)
data BioExchangeRow = BioExchangeRow
    { berName :: !Text
    , berCompartment :: !Text
    , berUnit :: !Text
    , berAmount :: !Double
    , berAmountRaw :: !Text
    , berUncertainty :: !Text
    , berComment :: !Text
    }
    deriving (Show, Eq, Generic)

instance NFData BioExchangeRow

-- ============================================================================
-- Process Block Accumulator
-- ============================================================================

-- | Accumulated data for a single process block
data ProcessBlock = ProcessBlock
    { pbIdentifier :: !Text
    , pbName :: !Text
    , pbCategoryType :: !Text
    , pbType :: !Text -- "Unit process" or "System"
    , pbLocation :: !Text
    , pbStatus :: !Text
    , pbTimePeriod :: !Text
    , pbTechnology :: !Text
    , pbRecord :: !Text
    , pbComment :: !Text
    , pbProducts :: ![ProductRow]
    , pbAvoidedProducts :: ![ProductRow]
    , pbMaterials :: ![TechExchangeRow]
    , pbElectricity :: ![TechExchangeRow]
    , pbWasteToTreatment :: ![TechExchangeRow]
    , pbResources :: ![BioExchangeRow]
    , pbEmissionsAir :: ![BioExchangeRow]
    , pbEmissionsWater :: ![BioExchangeRow]
    , pbEmissionsSoil :: ![BioExchangeRow]
    , pbFinalWaste :: ![BioExchangeRow]
    , pbInputParams :: ![(Text, Text)] -- name -> raw value
    , pbCalcParams :: ![(Text, Text)] -- name -> expression
    }
    deriving (Show, Eq, Generic)

instance NFData ProcessBlock

-- | Empty process block
emptyProcessBlock :: ProcessBlock
emptyProcessBlock =
    ProcessBlock
        { pbIdentifier = ""
        , pbName = ""
        , pbCategoryType = ""
        , pbType = ""
        , pbLocation = ""
        , pbStatus = ""
        , pbTimePeriod = ""
        , pbTechnology = ""
        , pbRecord = ""
        , pbComment = ""
        , pbProducts = []
        , pbAvoidedProducts = []
        , pbMaterials = []
        , pbElectricity = []
        , pbWasteToTreatment = []
        , pbResources = []
        , pbEmissionsAir = []
        , pbEmissionsWater = []
        , pbEmissionsSoil = []
        , pbFinalWaste = []
        , pbInputParams = []
        , pbCalcParams = []
        }

-- ============================================================================
-- Parser State Machine
-- ============================================================================

{- | A section of the process block being read. Each one owns one list of
'ProcessBlock', and 'addRowToBlock' is the only thing that reads it.
-}
data BlockSection
    = SecProducts
    | SecAvoidedProducts
    | SecMaterials
    | SecElectricity
    | SecWasteToTreatment
    | SecResources
    | SecEmissionsAir
    | SecEmissionsWater
    | SecEmissionsSoil
    | SecFinalWaste
    | SecInputParams
    | SecCalcParams
    deriving (Show, Eq)

{- | A section of the file rather than of a block: the four parameter tables in
scope for every process it declares, and the trailing @name;unit;cas;comment@
substance registry. Each one owns one field of 'ParseAcc'.
-}
data FileSection
    = SecDbInputParams
    | SecDbCalcParams
    | SecProjInputParams
    | SecProjCalcParams
    | SecSubstanceRegistry
    deriving (Show, Eq)

{- | What a recognised section header opens.

The split is what keeps the two functions that route a row from wildcarding
over each other's half: a block section can only reach 'addRowToBlock', a file
section can only reach the accumulator, and neither match has anything left to
cover. 'SecIgnored' is a header read on purpose and dropped; it still opens a
section, so the rows under it are swallowed rather than read as metadata.
-}
data SectionType
    = InBlock BlockSection
    | FileLevel FileSection
    | SecIgnored
    deriving (Show, Eq)

-- | Parser state
data ParseState
    = InHeader
    | InProcessMeta !BS.ByteString -- Current metadata key being read
    | InSection !SectionType
    | BetweenBlocks
    deriving (Show, Eq)

-- | Parse state accumulator
data ParseAcc = ParseAcc
    { paConfig :: !SimaProConfig
    , paState :: !ParseState
    , paInProcess :: !Bool -- inside a Process…End pair (vs the file trailer)
    , paCurrentBlock :: !ProcessBlock
    , paBlocks :: ![ProcessBlock]
    , paLineNum :: !Int
    , paDbInputParams :: ![(Text, Text)]
    , paDbCalcParams :: ![(Text, Text)]
    , paProjInputParams :: ![(Text, Text)]
    , paProjCalcParams :: ![(Text, Text)]
    , paSubstanceCAS :: ![(Text, Text)] -- (name, CAS) from the trailer registry
    }

-- ============================================================================
-- Global parameter bundle
-- ============================================================================

{- | Parameters declared outside any single process block — database- and
project-level Input/Calculated params — threaded into every block's evaluation
environment. The 'Monoid' instance merges the params each parallel worker
collected from its own chunk.
-}
data GlobalParams = GlobalParams
    { gpDbInput :: ![(Text, Text)]
    , gpDbCalc :: ![(Text, Text)]
    , gpProjInput :: ![(Text, Text)]
    , gpProjCalc :: ![(Text, Text)]
    }
    deriving (Show, Eq, Generic)

instance NFData GlobalParams

instance Semigroup GlobalParams where
    GlobalParams a1 b1 c1 d1 <> GlobalParams a2 b2 c2 d2 =
        GlobalParams (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid GlobalParams where
    mempty = GlobalParams [] [] [] []

-- | Output of parsing one contiguous chunk of lines.
data WorkerResult = WorkerResult
    { wrBlocks :: ![ProcessBlock]
    , wrParams :: !GlobalParams
    , wrSubstanceCAS :: ![(Text, Text)]
    }
    deriving (Show, Eq, Generic)

instance NFData WorkerResult

-- ============================================================================
-- Header Parsing
-- ============================================================================

-- | Parse a header line like "{key: value}" or "{value}"
parseHeaderLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseHeaderLine line
    | BS8.isPrefixOf "{" line && BS8.isSuffixOf "}" line =
        let content = BS.dropEnd 1 (BS.drop 1 line)
         in case BS8.breakSubstring ": " content of
                (key, rest) | not (BS.null rest) -> Just (BS8.strip key, BS8.strip (BS.drop 2 rest))
                _ -> Just (BS8.strip content, "")
    | otherwise = Nothing

-- | Update config from header line (takes ByteString, stores Text)
updateConfigFromHeader :: SimaProConfig -> BS.ByteString -> BS.ByteString -> SimaProConfig
updateConfigFromHeader cfg key value = case BS8.map toLower key of
    k | "simapro" `BS8.isPrefixOf` k -> cfg{spVersion = decodeBS key}
    "processes" -> cfg{spFileType = "processes"}
    "methods" -> cfg{spFileType = "methods"}
    "product stages" -> cfg{spFileType = "product stages"}
    "csv separator" -> cfg{spDelimiter = parseDelimiter value}
    "decimal separator" -> cfg{spDecimal = maybe ',' fst (BS8.uncons value)}
    "short date format" -> cfg{spDateFormat = localDecodeBS value}
    _ -> cfg
  where
    localDecodeBS = TE.decodeUtf8With TEE.lenientDecode
    parseDelimiter v
        | v == "Semicolon" = ';'
        | v == "Comma" = ','
        | v == "Tab" = '\t'
        | otherwise = ';'

-- ============================================================================
-- Section Detection
-- ============================================================================

-- | Detect section type from line (ByteString)
detectSection :: BS.ByteString -> Maybe SectionType
detectSection line = case BS8.strip line of
    "Products" -> Just (InBlock SecProducts)
    "Waste treatment" -> Just (InBlock SecProducts)
    "Avoided products" -> Just (InBlock SecAvoidedProducts)
    "Materials/fuels" -> Just (InBlock SecMaterials)
    "Electricity/heat" -> Just (InBlock SecElectricity)
    "Waste to treatment" -> Just (InBlock SecWasteToTreatment)
    "Resources" -> Just (InBlock SecResources)
    "Emissions to air" -> Just (InBlock SecEmissionsAir)
    "Emissions to water" -> Just (InBlock SecEmissionsWater)
    "Emissions to soil" -> Just (InBlock SecEmissionsSoil)
    "Final waste flows" -> Just (InBlock SecFinalWaste)
    "Input parameters" -> Just (InBlock SecInputParams)
    "Calculated parameters" -> Just (InBlock SecCalcParams)
    "Database Input parameters" -> Just (FileLevel SecDbInputParams)
    "Database Calculated parameters" -> Just (FileLevel SecDbCalcParams)
    "Project Input parameters" -> Just (FileLevel SecProjInputParams)
    "Project Calculated parameters" -> Just (FileLevel SecProjCalcParams)
    "Non material emissions" -> Just SecIgnored
    "Social issues" -> Just SecIgnored
    "Economic issues" -> Just SecIgnored
    _ -> Nothing

{- | Classify a section header, resolving the two names a SimaPro file reuses
for both a process section and a trailing substance-registry block. Inside a
@Process@…@End@ pair the process meaning wins; in the file trailer (no open
process) @Emissions to soil@ — and the registry-only @Raw materials@ /
@Airborne emissions@ / @Waterborne emissions@ — introduce the substance
registry, a @name;unit;cas;comment@ list of every substance with its CAS.
The trailer's @Final waste flows@ block collides too but is deliberately left
to its process-section reading: every one of its rows leaves the CAS column
blank, so it has nothing to contribute to the registry (and its productless
block is discarded as before).
-}
classifyHeader :: Bool -> BS.ByteString -> Maybe SectionType
classifyHeader inProcess line
    | not inProcess, BS8.strip line `elem` registryHeaders = Just (FileLevel SecSubstanceRegistry)
    | otherwise = detectSection line
  where
    registryHeaders =
        ["Raw materials", "Airborne emissions", "Waterborne emissions", "Emissions to soil"]

-- | Known metadata keys in process block (ByteString)
isMetadataKey :: BS.ByteString -> Bool
isMetadataKey key =
    key
        `elem` [ "Category type"
               , "Process identifier"
               , "Type"
               , "Process name"
               , "Status"
               , "Time period"
               , "Geography"
               , "Technology"
               , "Representativeness"
               , "Multiple output allocation"
               , "Substitution allocation"
               , "Cut off rules"
               , "Capital goods"
               , "Boundary with nature"
               , "Infrastructure"
               , "Date"
               , "Record"
               , "Generator"
               , "External documents"
               , "Literature references"
               , "Collection method"
               , "Data treatment"
               , "Verification"
               , "Comment"
               , "Allocation rules"
               , "System description"
               , "PlatformId"
               ]

-- ============================================================================
-- Row Parsing (ByteString based, decode to Text only when storing)
-- ============================================================================

-- | Decode ByteString to Text (lenient UTF-8)
decodeBS :: BS.ByteString -> Text
decodeBS = TE.decodeUtf8With TEE.lenientDecode
{-# INLINE decodeBS #-}

{- | Read a plain decimal cell, with the file's decimal separator. 0 when the
cell holds anything else.

The whole cell has to be the number. Reading it up to the first character that
is not part of one used to turn @0,45+0,247+,067@ into 0.45 — a number of the
right order of magnitude, wrong by a third, indistinguishable from a real one
downstream. An amount cell that is not a literal is an expression, and
'resolveAmount' evaluates it; one that is neither is surfaced by
'fallbackAmounts' rather than approximated here.
-}
parseAmount :: Char -> BS.ByteString -> Double
parseAmount decimalSep bs
    | BS.null bs = 0.0
    | otherwise = fromMaybe 0.0 (readAmount (decodeBS normalized))
  where
    normalized
        | decimalSep == ',' = BS8.map (\c -> if c == ',' then '.' else c) bs
        | otherwise = bs

-- | Split a CSV line by delimiter, respecting RFC 4180 quoted fields.
splitCSV :: Char -> BS.ByteString -> [BS.ByteString]
splitCSV delim bs =
    -- 'BS8.lines' leaves the CR of a CRLF terminator on the line. A lone
    -- trailing CR makes cassava's incremental parser wait for the LF of a
    -- CRLF and fail with "not enough input" at end of line (observably, its
    -- success even varies with the optimization level), silently degrading
    -- every CRLF row to the naive split below — which tears quoted fields
    -- apart. Strip it before parsing: it is line-terminator residue, never
    -- field data.
    let clean = BS8.dropWhileEnd (== '\r') bs
        opts =
            Csv.defaultDecodeOptions
                { Csv.decDelimiter = fromIntegral (fromEnum delim)
                }
     in case Csv.decodeWith opts Csv.NoHeader (BL.fromStrict clean) of
            Right rows | Just (row, _) <- V.uncons rows -> V.toList row
            _ -> BS8.split delim clean -- fallback to naive on parse error

-- | Parse a parameter row: name;value_or_expression;...
parseParamRow :: SimaProConfig -> BS.ByteString -> Maybe (Text, Text)
parseParamRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
     in case fields of
            (name : value : _) ->
                let n = decodeBS (BS8.strip name)
                    v = Expr.normalizeExpr (spDecimal cfg) (decodeBS (BS8.strip value))
                 in if T.null n then Nothing else Just (n, v)
            _ -> Nothing

{- | Parse a reference product line.
SimaPro CSV format:
  0. name
  1. unit
  2. value or formula
  3. allocation
  4. waste type
  5. category (separated by \)
  6. comment
However, some waste treatment product rows omit the allocation field (field 3),
producing only 6 fields. We detect this by checking whether field 3 looks numeric
(allocation is a percentage like "100" or "33.5"). If it doesn't, we treat it as
waste_type and shift the remaining fields accordingly.
Without this detection, the comment (which often contains \x7f-separated EcoSpold
property metadata) ends up being parsed as the category.
-}
parseProductRow :: SimaProConfig -> BS.ByteString -> Maybe ProductRow
parseProductRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
        norm = Expr.normalizeExpr (spDecimal cfg) . decodeBS . BS8.strip
     in case fields of
            -- 7+ fields with allocation (number or formula): name;unit;amount;alloc;waste;cat;comment
            (name : unit : amount : alloc : waste : cat : rest)
                | isAllocationField cfg alloc ->
                    Just
                        ProductRow
                            { prName = decodeBS (BS8.strip name)
                            , prUnit = decodeBS (BS8.strip unit)
                            , prAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                            , prAmountRaw = norm amount
                            , prAllocation = parseAmount (spDecimal cfg) (BS8.strip alloc)
                            , prAllocRaw = norm alloc
                            , prWasteType = decodeBS (BS8.strip waste)
                            , prCategory = decodeBS (BS8.strip cat)
                            , prComment = joinComment rest
                            }
            -- 6+ fields without allocation: name;unit;amount;waste;cat;comment
            (name : unit : amount : waste : cat : rest) ->
                Just
                    ProductRow
                        { prName = decodeBS (BS8.strip name)
                        , prUnit = decodeBS (BS8.strip unit)
                        , prAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                        , prAmountRaw = norm amount
                        , prAllocation = 100
                        , prAllocRaw = "100"
                        , prWasteType = decodeBS (BS8.strip waste)
                        , prCategory = decodeBS (BS8.strip cat)
                        , prComment = joinComment rest
                        }
            _ -> Nothing

{- | Check if a ByteString field is a valid expression (number, variable, or formula).
Uses the Megaparsec expression parser syntactically — accepts any identifier without
needing parameter values. Waste type descriptions ("All waste types") fail to parse.
-}
isAllocationField :: SimaProConfig -> BS.ByteString -> Bool
isAllocationField cfg bs = Expr.isExpression (spDecimal cfg) (decodeBS (BS8.strip bs))

-- | Parse a technosphere exchange row (ByteString input, Text output)

{- | Re-join the trailing CSV columns that hold the free-text comment.
A comment may itself contain ';' (un-escaped in SimaPro exports), so
we have to glue the tail back together. Empty padding columns on
either side get dropped so a blank-comment row produces "" rather
than ";;;" and a comment in the last filled column doesn't carry
leading ";"s from skipped intermediate columns.
-}
joinComment :: [BS.ByteString] -> Text
joinComment = decodeBS . BS8.intercalate ";" . dropWhile BS.null . dropWhileEnd BS.null

parseTechRow :: SimaProConfig -> BS.ByteString -> Maybe TechExchangeRow
parseTechRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
        norm = Expr.normalizeExpr (spDecimal cfg) . decodeBS . BS8.strip
     in case fields of
            (name : unit : amount : unc : _ : _ : _ : rest) ->
                Just
                    TechExchangeRow
                        { terName = decodeBS (BS8.strip name)
                        , terUnit = decodeBS (BS8.strip unit)
                        , terAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                        , terAmountRaw = norm amount
                        , terUncertainty = decodeBS (BS8.strip unc)
                        , terComment = joinComment rest
                        }
            (name : unit : amount : rest) ->
                Just
                    TechExchangeRow
                        { terName = decodeBS (BS8.strip name)
                        , terUnit = decodeBS (BS8.strip unit)
                        , terAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                        , terAmountRaw = norm amount
                        , terUncertainty = ""
                        , terComment = joinComment rest
                        }
            _ -> Nothing

-- | Parse a biosphere exchange row (ByteString input, Text output)
parseBioRow :: SimaProConfig -> BS.ByteString -> Maybe BioExchangeRow
parseBioRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
        norm = Expr.normalizeExpr (spDecimal cfg) . decodeBS . BS8.strip
     in case fields of
            (name : compartment : unit : amount : unc : _ : _ : _ : rest) ->
                Just
                    BioExchangeRow
                        { berName = decodeBS (BS8.strip name)
                        , berCompartment = decodeBS (BS8.strip compartment)
                        , berUnit = decodeBS (BS8.strip unit)
                        , berAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                        , berAmountRaw = norm amount
                        , berUncertainty = decodeBS (BS8.strip unc)
                        , berComment = joinComment rest
                        }
            (name : compartment : unit : amount : rest) ->
                Just
                    BioExchangeRow
                        { berName = decodeBS (BS8.strip name)
                        , berCompartment = decodeBS (BS8.strip compartment)
                        , berUnit = decodeBS (BS8.strip unit)
                        , berAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                        , berAmountRaw = norm amount
                        , berUncertainty = ""
                        , berComment = joinComment rest
                        }
            _ -> Nothing

{- | Parse one row of a trailing substance-registry block
(@name;unit;cas;comment@), returning the @(name, CAS)@ pair when the CAS column
is populated. The registry lists every elementary substance the file uses with
its CAS; waste rows and a few substances leave the CAS column blank and are
skipped (a name→CAS binding needs a CAS).
-}
parseSubstanceRow :: SimaProConfig -> BS.ByteString -> Maybe (Text, Text)
parseSubstanceRow cfg line =
    case splitCSV (spDelimiter cfg) line of
        (name : _unit : cas : _) ->
            let n = decodeBS (BS8.strip name)
                c = decodeBS (BS8.strip cas)
             in if T.null n || T.null c then Nothing else Just (n, c)
        _ -> Nothing

-- ============================================================================
-- State Machine Processing (ByteString based)
-- ============================================================================

-- | Process a single line (ByteString)
processLine :: ParseAcc -> BS.ByteString -> ParseAcc
processLine acc@ParseAcc{..} line
    -- Empty line handling
    | BS.null (BS8.strip line) = case paState of
        InProcessMeta _ -> acc{paState = BetweenBlocks}
        InSection _ -> acc{paState = BetweenBlocks}
        _ -> acc
    -- Header lines
    | Just (key, value) <- parseHeaderLine line
    , paState == InHeader =
        acc{paConfig = updateConfigFromHeader paConfig key value}
    -- Process block start
    | BS8.strip line == "Process" =
        acc
            { paState = BetweenBlocks
            , paInProcess = True
            , paCurrentBlock = emptyProcessBlock
            }
    -- End of block
    | BS8.strip line == "End" =
        let block = paCurrentBlock
            -- A block is valid if it has at least one product (process name not required)
            isValid = not (null (pbProducts block))
         in acc
                { paState = BetweenBlocks
                , paInProcess = False
                , paBlocks = if isValid then block : paBlocks else paBlocks
                , paCurrentBlock = emptyProcessBlock
                }
    -- Section detection (trailer registry blocks resolve against paInProcess)
    | Just sec <- classifyHeader paInProcess line =
        acc{paState = InSection sec}
    -- In a section, parse row (file-level sections to ParseAcc, block sections to the block)
    | InSection sec <- paState
    , not (BS.null (BS8.strip line)) =
        case sec of
            FileLevel f -> addFileRow paConfig f line acc
            InBlock b -> acc{paCurrentBlock = addRowToBlock paConfig b line paCurrentBlock}
            SecIgnored -> acc
    -- Metadata key-value pairs
    | paState == BetweenBlocks || isMetadataKey (BS8.strip line) =
        if isMetadataKey (BS8.strip line)
            then acc{paState = InProcessMeta (BS8.strip line)}
            else case paState of
                InProcessMeta key ->
                    acc
                        { paCurrentBlock = setMetadata key line paCurrentBlock
                        , paState = BetweenBlocks
                        }
                _ -> acc
    -- Value for metadata key
    | InProcessMeta key <- paState =
        acc
            { paCurrentBlock = setMetadata key line paCurrentBlock
            , paState = BetweenBlocks
            }
    | otherwise = acc{paLineNum = paLineNum + 1}

{- | Add a row of a file-level section to the accumulator field it belongs to.
The four parameter tables are in scope for every process the file declares;
the registry is the trailing substance list every name draws its CAS from.
-}
addFileRow :: SimaProConfig -> FileSection -> BS.ByteString -> ParseAcc -> ParseAcc
addFileRow cfg sec line acc = case sec of
    SecDbInputParams -> withParam $ \p -> acc{paDbInputParams = p : paDbInputParams acc}
    SecDbCalcParams -> withParam $ \p -> acc{paDbCalcParams = p : paDbCalcParams acc}
    SecProjInputParams -> withParam $ \p -> acc{paProjInputParams = p : paProjInputParams acc}
    SecProjCalcParams -> withParam $ \p -> acc{paProjCalcParams = p : paProjCalcParams acc}
    SecSubstanceRegistry ->
        maybe acc (\nc -> acc{paSubstanceCAS = nc : paSubstanceCAS acc}) (parseSubstanceRow cfg line)
  where
    withParam :: ((Text, Text) -> ParseAcc) -> ParseAcc
    withParam k = maybe acc k (parseParamRow cfg line)

-- | Add a row to the appropriate list in the block (ByteString)
addRowToBlock :: SimaProConfig -> BlockSection -> BS.ByteString -> ProcessBlock -> ProcessBlock
addRowToBlock cfg sec line block = case sec of
    SecProducts -> case parseProductRow cfg line of
        Just row -> block{pbProducts = row : pbProducts block}
        Nothing -> block
    SecAvoidedProducts -> case parseProductRow cfg line of
        Just row -> block{pbAvoidedProducts = row : pbAvoidedProducts block}
        Nothing -> block
    SecMaterials -> case parseTechRow cfg line of
        Just row -> block{pbMaterials = row : pbMaterials block}
        Nothing -> block
    SecElectricity -> case parseTechRow cfg line of
        Just row -> block{pbElectricity = row : pbElectricity block}
        Nothing -> block
    SecWasteToTreatment -> case parseTechRow cfg line of
        Just row -> block{pbWasteToTreatment = row : pbWasteToTreatment block}
        Nothing -> block
    SecResources -> case parseBioRow cfg line of
        Just row -> block{pbResources = row : pbResources block}
        Nothing -> block
    SecEmissionsAir -> case parseBioRow cfg line of
        Just row -> block{pbEmissionsAir = row : pbEmissionsAir block}
        Nothing -> block
    SecEmissionsWater -> case parseBioRow cfg line of
        Just row -> block{pbEmissionsWater = row : pbEmissionsWater block}
        Nothing -> block
    SecEmissionsSoil -> case parseBioRow cfg line of
        Just row -> block{pbEmissionsSoil = row : pbEmissionsSoil block}
        Nothing -> block
    SecFinalWaste -> case parseBioRow cfg line of
        Just row -> block{pbFinalWaste = row : pbFinalWaste block}
        Nothing -> block
    SecInputParams -> case parseParamRow cfg line of
        Just p -> block{pbInputParams = p : pbInputParams block}
        Nothing -> block
    SecCalcParams -> case parseParamRow cfg line of
        Just p -> block{pbCalcParams = p : pbCalcParams block}
        Nothing -> block

-- | Set metadata field in block (ByteString key, decode value to Text)
setMetadata :: BS.ByteString -> BS.ByteString -> ProcessBlock -> ProcessBlock
setMetadata key value block = case key of
    "Category type" -> block{pbCategoryType = decodeBS (BS8.strip value)}
    "Process identifier" -> block{pbIdentifier = decodeBS (BS8.strip value)}
    "Type" -> block{pbType = decodeBS (BS8.strip value)}
    "Process name" -> block{pbName = decodeBS (BS8.strip value)}
    "Status" -> block{pbStatus = decodeBS (BS8.strip value)}
    "Time period" -> block{pbTimePeriod = decodeBS (BS8.strip value)}
    "Geography" -> block{pbLocation = decodeBS (BS8.strip value)}
    "Technology" -> block{pbTechnology = decodeBS (BS8.strip value)}
    "Record" -> block{pbRecord = decodeBS (BS8.strip value)}
    "Comment" -> block{pbComment = decodeBS (BS8.strip value)}
    _ -> block

-- ============================================================================
-- UUID Generation
-- ============================================================================

-- | Namespace for SimaPro UUIDs
simaproNamespace :: UUID
simaproNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "simapro.pre.nl")

{- | Generate deterministic activity UUID from an Activity.

A SimaPro block publishes its own identifier on the "Process identifier" line,
and that is what names the activity when it is there. It is what the producer
says the dataset is, it survives a re-export of the same version, and it
survives the producer changing the spelling or the case of a name.

Without it the identifier falls back to the name and the location, folded in
case for the same reason the flow name is: two exports of one database write
one product two ways. 'dropAmbiguousNativeIds' has already taken away any
identifier that named more than one process, so this stays a total function of
the activity.
-}
generateActivityUUID :: Activity -> UUID
generateActivityUUID act =
    UUID5.generateNamed simaproNamespace . BS.unpack . TE.encodeUtf8 $ case activityNativeId act of
        Just (NativeProcessId nativeId) -> "process:" <> nativeId
        Nothing -> "activity:" <> T.toCaseFold (activityName act) <> "@" <> activityLocation act

{- | Take a native identifier away from the activities when it names more than
one process.

Every coproduct of one block shares the block's identifier, which is the point:
they are one process with several outputs. Two /different/ blocks sharing one
is a naming mistake on the producer's side, and the identifier then names
neither: those activities fall back to their name and location. The file still
loads, and the identifiers dropped are returned so the caller can name them.
-}
dropAmbiguousNativeIds :: [Activity] -> ([Activity], [Text])
dropAmbiguousNativeIds activities =
    (map forget activities, S.toList ambiguous)
  where
    named =
        M.fromListWith
            S.union
            [ (nativeId, S.singleton (activityName act, activityLocation act))
            | act <- activities
            , Just (NativeProcessId nativeId) <- [activityNativeId act]
            ]
    ambiguous = M.keysSet (M.filter ((> 1) . S.size) named)
    forget act = case activityNativeId act of
        Just (NativeProcessId nativeId) | nativeId `S.member` ambiguous -> act{activityNativeId = Nothing}
        _ -> act

{- | Generate deterministic flow UUID from name and compartment.

The unit is deliberately absent. It is a property of the row, not of the flow:
the same substance written in g by one block and in kg by another is one flow,
and 'canonicalRow' has already brought both rows to the reference unit of the
dimension. Keeping the unit in the key also made the identifier depend on the
engine's own unit table, so renaming a reference unit moved identifiers no data
had touched.

The name is folded in case for the same reason: two exports of one database
disagree on the case of a product name, and a flow is not two flows because a
producer capitalised it differently.
-}
generateFlowUUID :: Text -> Text -> UUID
generateFlowUUID name compartment =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "flow:" <> T.toCaseFold name <> ":" <> compartment)

{- | Canonical (compartment, subcompartment) string for SimaPro flow UUIDs.

The inventory parser ('bioRowToExchange') and the method CF parser
('Method.ParserSimaPro') both feed flow UUIDs through 'generateFlowUUID'.
They MUST agree on the compartment string or a CF will never match its
inventory flow by UUID — the lookup falls through to the slower name-based
cascade, and any CF whose canonical entry is keyed on a different medium is
silently dropped.

Normalization rules:

* lower-case + trim both inputs
* put the medium through 'parseMedium', so the SimaPro method CSVs' \"Raw\"
  section header and the inventory side's own word reach one spelling. A
  medium the reader cannot place is kept as written: this is a hash input, and
  refusing it here would leave the flow with no identity at all
* collapse the SimaPro placeholder sub @(unspecified)@ to empty so a CF
  carrying an explicit @(unspecified)@ subcompartment lands on the same
  UUID as an inventory row whose sub column is blank
* join non-empty parts with @\/@; emit just the medium when sub is empty
-}
normalizeSimaProCompartment :: Text -> Text -> Text
normalizeSimaProCompartment comp sub =
    let lcComp = either id mediumText (parseMedium comp)
        lcSub = T.toLower (T.strip sub)
        normSub
            | T.null lcSub || lcSub == "(unspecified)" = T.empty
            | otherwise = lcSub
     in if T.null normSub then lcComp else lcComp <> "/" <> normSub

-- | Generate deterministic unit UUID from name
generateUnitUUID :: Text -> UUID
generateUnitUUID unitName =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "unit:" <> unitName)

-- ============================================================================
-- Conversion to volca Types
-- ============================================================================

{- | How a location was read out of a SimaPro name. (Not to be confused with
'Types.LocationSource', which says whether an activity's location was declared
at all — every reading here is an inferred one by that measure.)

A tag is a location the producer wrote down. A slash suffix is a guess made by
cutting the name, and names end in a slash for reasons that have nothing to do
with geography — in "Already packed - PP/PE", PE is a plastic, not Peru. So the
ordering here says a tag beats a suffix, wherever each of the two sits.
-}
data NameReading
    = Tagged
    | SlashSuffix
    deriving (Eq, Ord, Show)

-- | A location read out of a name, with the name the reading leaves behind.
data Located = Located
    { locatedName :: Text
    {- ^ The name without the location: unchanged for a tag, which is only
    informational, shortened for a suffix, which is part of the name.
    -}
    , locatedLocation :: Text
    , locatedSource :: NameReading
    }
    deriving (Eq, Show)

{- | Extract location from SimaPro-style names
Handles three forms:
  * Curly-brace tag (ecoinvent 3.10+): "Name {FR}| market for ..."
  * Embedded bracket tag (ecoinvent 3.9.1 SimaPro export): "name//[FR] ..."
  * WFLDB-style "/XX" suffix: "Ammonium nitrate .../CN".
The first two preserve the full name (the tag is informational); the WFLDB
form strips at the slash because the geo code is a true suffix.

Nothing when the name states no location — callers keep the name they passed in
rather than a shortened one.
-}
extractLocation :: Text -> Maybe Located
extractLocation name =
    case T.breakOn "{" name of
        (_, rest) | not (T.null rest) ->
            case T.breakOn "}" (T.drop 1 rest) of
                (loc, afterBrace)
                    | not (T.null afterBrace) ->
                        let cleanLoc = T.strip loc
                         in if T.length cleanLoc >= 2
                                then Just (Located (T.strip name) cleanLoc Tagged)
                                else Nothing
                _ -> Nothing
        _ -> case extractBracketLocation name of
            Just loc -> Just (Located (T.strip name) loc Tagged)
            Nothing -> case extractSlashLocation name of
                Just (cleanName, loc) -> Just (Located cleanName loc SlashSuffix)
                Nothing -> Nothing
  where
    -- Match "//[XX]" anywhere in the string (older SimaPro exports of
    -- ecoinvent embed the geo code mid-name, so we keep the full name).
    extractBracketLocation n = case T.breakOn "//[" n of
        (_, rest) | not (T.null rest) ->
            case T.breakOn "]" (T.drop 3 rest) of
                (loc, afterBracket)
                    | not (T.null afterBracket) ->
                        let cleanLoc = T.strip loc
                         in if T.length cleanLoc >= 2 then Just cleanLoc else Nothing
                _ -> Nothing
        _ -> Nothing

    -- Extract location from WFLDB-style slash suffixes like:
    --   "Product (WFLDB)/CN U"       → ("Product (WFLDB)", "CN")
    --   "Product/ha/GLO/I U"         → ("Product/ha", "GLO")
    -- Scans rightward through slash-separated segments for the first geo code.
    extractSlashLocation = go
      where
        go t = case T.breakOnEnd "/" t of
            ("", _) -> Nothing
            (before, suffix) -> case T.words (T.strip suffix) of
                (loc : _)
                    | isGeoCode loc ->
                        Just (T.strip (T.dropWhileEnd (== '/') before), loc)
                _ -> go (T.dropWhileEnd (== '/') before)
    isGeoCode t = T.length t >= 2 && maybe False (isUpper . fst) (T.uncons t)

-- | Resolve a parameterized amount: try expression evaluation, fall back to numeric parse
resolveAmount :: M.Map Text Double -> Text -> Double -> Double
resolveAmount env raw fallback
    | T.null raw = fallback
    | otherwise = fromMaybe fallback (resolveExpr env raw)

-- | A number, or an expression over the parameter environment. Nothing: neither.
resolveExpr :: M.Map Text Double -> Text -> Maybe Double
resolveExpr env raw = readAmount raw <|> either (const Nothing) Just (Expr.evaluate env raw)

{- | Every raw amount in a block that 'resolveAmount' will replace with its
lenient fallback: not a number, and not an expression the block's parameter
environment can evaluate. Reported as warnings at the IO edge, like the CAS
conflicts, so the conversion itself stays pure and a wrong amount never
passes without a word.
-}
fallbackAmounts :: GlobalParams -> ProcessBlock -> [(Text, Text, Double)]
fallbackAmounts gp pb@ProcessBlock{..} =
    [ (blockName, raw, fallback)
    | (raw, fallback) <- rawAmounts
    , not (T.null raw)
    , isNothing (resolveExpr env raw)
    ]
  where
    -- Forced only when a raw fails 'readAmount', so a block of plain numbers
    -- never builds its environment a second time.
    env = fst (blockParamEnv gp pb)
    blockName = case nonEmptyText (T.strip pbName) of
        Just n -> n
        Nothing -> maybe "unnamed process" prName (listToMaybe pbProducts)
    rawAmounts =
        concatMap (\p -> [(prAmountRaw p, prAmount p), (prAllocRaw p, prAllocation p)]) pbProducts
            ++ map (\p -> (prAmountRaw p, prAmount p)) pbAvoidedProducts
            ++ map (\r -> (terAmountRaw r, terAmount r)) (pbMaterials ++ pbElectricity ++ pbWasteToTreatment)
            ++ map (\r -> (berAmountRaw r, berAmount r)) (pbResources ++ pbEmissionsAir ++ pbEmissionsWater ++ pbEmissionsSoil ++ pbFinalWaste)

{- | Build the resolved parameter environment and the raw-expression map from
ordered parameter groups. Input groups are resolved with a single pass each;
calc groups iterate to a fixed point (to resolve forward references where a
param depends on one defined later in the CSV). Groups are ordered low→high
precedence: database, project, process.
-}
buildParamEnv :: [[(Text, Text)]] -> [[(Text, Text)]] -> (M.Map Text Double, M.Map Text Text)
buildParamEnv inputGroups calcGroups =
    ( foldl' evalToFixpoint (foldl' (foldl' evalParam) M.empty inputGroups) calcGroups
    , M.fromList (concat (inputGroups ++ calcGroups))
    )
  where
    evalParam acc (name, rawVal) =
        either (const acc) (\v -> M.insert name v acc) (Expr.evaluate acc rawVal)
    evalToFixpoint acc params =
        let acc' = foldl' evalParam acc params
         in if M.size acc' == M.size acc then acc' else evalToFixpoint acc' params

-- | The parameter environment a block's amounts are evaluated in.
blockParamEnv :: GlobalParams -> ProcessBlock -> (M.Map Text Double, M.Map Text Text)
blockParamEnv GlobalParams{..} ProcessBlock{..} =
    buildParamEnv
        (reverse <$> [gpDbInput, gpProjInput, pbInputParams])
        (reverse <$> [gpDbCalc, gpProjCalc, pbCalcParams])

{- | Convert ProcessBlock to list of Activities (one per product)
This matches EcoSpold behavior where multi-product processes create multiple activities
Global params (db + project level) are passed in and combined with process-level params.
-}
processBlockToActivity ::
    UnitConversion.UnitConfig ->
    GlobalParams ->
    ProcessBlock ->
    Maybe (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit])
processBlockToActivity unitCfg gp pb@ProcessBlock{..} =
    case productsInFileOrder of
        [] -> Nothing
        reference : coproducts -> Just (block reference coproducts)
  where
    -- pbProducts is accumulated by prepending; restore file order so the
    -- first row is the reference (main) product of the block.
    productsInFileOrder = reverse pbProducts

    (env, exprMap) = blockParamEnv gp pb

    processReading = extractLocation pbName

    -- The Geography field, when the producer filled it in. It outranks anything
    -- read out of a name, being the one place meant to hold a location — a name
    -- reading is recorded as such below, for the quality report.
    statedLocation = mfilter ((/= "unspecified") . T.toLower) (nonEmptyText pbLocation)

    {- Trimmed Process name (without curly-brace location tag). Empty when the
    SimaPro "Process name" field is empty (typical for mono-product blocks
    where only the Product line carries the human-readable name).

    A name only loses its tail when that tail is what named the place. When the
    reference product states a tag, the tail was never a location and the whole
    name stays: "… - PP/PE | No preparation" keeps its packaging and its
    preparation step instead of ending at the PP. -}
    processNameTrimmed
        | (locatedSource <$> processReading) == Just SlashSuffix
        , (locatedSource <$> referenceReading) == Just Tagged =
            T.strip pbName
        | otherwise = T.strip (maybe pbName locatedName processReading)

    referenceReading = case productsInFileOrder of
        (p : _) -> extractLocation (prName p)
        [] -> Nothing

    -- Convert each section's rows to (exchange, flow, unit) triples in one pass.
    -- 'Final waste flows' are elementary: nothing treats them, so nothing
    -- produces them, and a method characterizes them under the medium the
    -- section is named after. Waste sent to treatment is 'pbWasteToTreatment',
    -- which goes to the technosphere with the rest.
    (avoidedExs, avoidedFlows, avoidedUnits) =
        unzip3 (productToExchange unitCfg env AvoidedProduct <$> pbAvoidedProducts)
    (techExs, techFlows, techUnits) =
        unzip3 (techRowToExchange unitCfg env <$> (pbMaterials ++ pbElectricity ++ pbWasteToTreatment))
    (bioExs, bioFlows, bioUnits) =
        unzip3 $
            (bioRowToExchange unitCfg env True NaturalResource <$> pbResources)
                ++ (bioRowToExchange unitCfg env False Air <$> pbEmissionsAir)
                ++ (bioRowToExchange unitCfg env False Water <$> pbEmissionsWater)
                ++ (bioRowToExchange unitCfg env False Soil <$> pbEmissionsSoil)
                ++ (bioRowToExchange unitCfg env False Waste <$> pbFinalWaste)

    -- Exchanges/flows/units the block's products share. They are written once,
    -- unscaled: "Database.Allocation" splits the block into one process per
    -- product and scales them by each product's declared share.
    sharedExchanges = avoidedExs ++ techExs ++ bioExs
    sharedTechFlows = avoidedFlows ++ techFlows
    sharedBioFlows = bioFlows
    -- The format has no waste axis: its two waste sections are elementary
    -- ('Final waste flows') and technosphere ('Waste to treatment').
    sharedWasteFlows = []
    sharedUnitNames =
        S.toList . S.fromList $ unitName <$> (avoidedUnits ++ techUnits ++ bioUnits)

    descriptionLines = maybeToList (nonEmptyText pbComment)
    nativeType = SimaProProcessType <$> nonEmptyText pbType

    -- Block identity, and what 'generateActivityUUID' mints the activity from
    -- when the block publishes one. That is what keeps two blocks apart where
    -- their name would not: a SimaPro "Process name" is truncated to 80
    -- characters and reused verbatim across unrelated blocks, so blocks with no
    -- identifier fall back to name and location and can still collide there.
    nativeId = NativeProcessId <$> nonEmptyText pbIdentifier

    block :: ProductRow -> [ProductRow] -> (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit])
    block reference coproducts =
        let (referenceExchange, referenceFlow, referenceUnit) = productToExchange unitCfg env ReferenceProduct reference
            (coproductExs, coproductFlows, coproductUnits) = unzip3 (productToExchange unitCfg env Coproduct <$> coproducts)
            -- Activity name = Process name when present, otherwise the reference
            -- product's name (with its location). Never a coproduct's own: the
            -- block is one activity, and every process split from it keeps
            -- its name, so they share one activityUUID.
            -- The readings that may name the place are the ones behind the name
            -- we settled on; the reference product's comes last, so a tie goes
            -- to the Process name, which is what named the block.
            -- Neither a Process name nor a named reference row: the first
            -- named product names the block, so a malformed export does not
            -- blank it and collide with every other blank block at its place.
            fallbackName =
                fromMaybe "" . listToMaybe $
                    [ maybe n locatedName (extractLocation n)
                    | p <- reference : coproducts
                    , let n = T.strip (prName p)
                    , not (T.null n)
                    ]
            (effectiveActivityName, readings)
                | not (T.null processNameTrimmed) = (processNameTrimmed, [processReading, referenceReading])
                | otherwise = (fallbackName, [referenceReading])
            -- Best-founded reading wins; sortOn is stable, so a tie goes to the
            -- Process name.
            readLocation =
                foldMap locatedLocation
                    . listToMaybe
                    . sortOn locatedSource
                    $ catMaybes readings
            effectiveLoc = fromMaybe readLocation statedLocation
            effectiveLocSource
                | isJust statedLocation = LocationDeclared
                | T.null effectiveLoc = LocationUnspecified
                | otherwise = LocationInferredFromName
            activity =
                Activity
                    { activityName = effectiveActivityName
                    , activityDescription = descriptionLines
                    , activityDocumentation = [] -- SimaPro states its provenance too; not read yet
                    , activitySynonyms = M.empty
                    , -- The reference row's category names the block; each product
                      -- row's own travels on its exchange ('techClassification')
                      -- and takes over on the process split for that product.
                      activityClassification =
                        M.fromList $
                            filter
                                (not . T.null . snd)
                                [ ("Category type", pbCategoryType)
                                , ("Category", prCategory reference)
                                ]
                    , activityLocation = effectiveLoc
                    , activityLocationSource = effectiveLocSource
                    , activityUnit = unitName referenceUnit
                    , exchanges = referenceExchange : coproductExs ++ sharedExchanges
                    , activityParams = env
                    , activityParamExprs = exprMap
                    , activityNativeType = nativeType
                    , activityNativeId = nativeId
                    , activityFormulaCheck = Nothing
                    }
            allUnits =
                map
                    (\name -> Unit (generateUnitUUID name) name name "")
                    (S.toList . S.fromList $ map unitName (referenceUnit : coproductUnits) ++ sharedUnitNames)
         in (activity, referenceFlow : coproductFlows ++ sharedTechFlows, sharedBioFlows, sharedWasteFlows, allUnits)

-- | True when the raw allocation cell is a plain decimal literal (no formula).
isNumericFormula :: Text -> Bool
isNumericFormula = isJust . readAmount

{- | The reference unit of a row's dimension, and the row's amount in it.

Every row is recorded in the reference unit of its dimension (kg for a mass,
mj for an energy, m3 for a volume), so one flow carries one unit and a matrix
row never sums two of them. A unit the table does not know is left as written:
an import stays tolerant, and the matrix builder in "Database" surfaces the
unknown unit with its own message.
-}
canonicalRow :: UnitConversion.UnitConfig -> Text -> Double -> (Text, Double)
canonicalRow unitCfg unit amount =
    fromMaybe (unit, amount) (UnitConversion.normalizeToCanonical unitCfg unit amount)

{- | Convert a product row to exchange, flow, and unit in one pass, under the
role its section gives it: @Products@ rows are the reference and its
coproducts, @Avoided products@ rows are substitutions.

The declared amount is converted to the reference unit of its dimension by
'canonicalRow', reference product and coproduct alike. For the reference that
is what makes 'activityNormFactor' and the matrix column read per 1 base unit:
a reference declared as "1 ton" would otherwise produce impacts 1000x too
large. For a coproduct it is what lets the row carry the same identifier as
the input that consumes it elsewhere.
-}
productToExchange :: UnitConversion.UnitConfig -> M.Map Text Double -> TechRole -> ProductRow -> (Exchange, TechnosphereFlow, Unit)
productToExchange unitCfg env role ProductRow{..} =
    let reading = extractLocation prName
        cleanName = maybe prName locatedName reading
        prodRowLoc = foldMap locatedLocation reading
        rawAmount = resolveAmount env prAmountRaw prAmount
        (effUnitName, amount) = canonicalRow unitCfg prUnit rawAmount
        flowUUID = generateFlowUUID cleanName ""
        unitUUID = generateUnitUUID effUnitName
        (pedigree, cleanedComment) = parsePedigreePrefix prComment
        exchange =
            TechnosphereExchange
                { techFlowId = flowUUID
                , techAmount = amount
                , techUnitId = unitUUID
                , techRole = role
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , -- Preserve the location encoded on the Products row (e.g.
                  -- WFLDB writes "Product (WFLDB)/m2/GLO U" while the
                  -- enclosing Process name may be "/CH"). The activity's
                  -- own location is set independently in makeActivity; this
                  -- field lets the cross-DB supplier index expose the
                  -- product under its declared geographic scope as well.
                  techLocation = prodRowLoc
                , techComment = cleanedComment
                , techPedigree = pedigree
                , techShare = share
                , techClassification = M.fromList [("Category", prCategory) | not (T.null prCategory)]
                , techProperties = noProperties
                }
        -- A product row states its share of the block; an avoided product row
        -- has the same columns but names a substitution, not a share of anything.
        share = case role of
            ReferenceProduct -> Just declared
            Coproduct -> Just declared
            AvoidedProduct -> Nothing
            ReferenceInput -> Nothing
            Input -> Nothing
        declared =
            DeclaredShare
                { dsPercent = resolveAmount env prAllocRaw prAllocation
                , dsFormula = mfilter (not . isNumericFormula) (nonEmptyText prAllocRaw)
                }
        flow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = cleanName
                , tfUnitId = unitUUID
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
        unit = Unit{unitId = unitUUID, unitName = effUnitName, unitSymbol = effUnitName, unitComment = ""}
     in (exchange, flow, unit)

{- | Drop empty / whitespace-only text, decoding SimaPro's in-cell line
breaks: a multi-line comment is exported on one physical line with @\\x7f@
(DEL) separating the lines. Single normalisation point for comment cleanup.
-}
nonEmptyText :: Text -> Maybe Text
nonEmptyText t =
    let s = T.strip (T.replace "\x7f" "\n" t)
     in if T.null s then Nothing else Just s

{- | Split SimaPro's trailing comment column into the pedigree matrix (when
present at the start) and the cleaned free-text comment.

SimaPro encodes pedigree as a `(r,c,t,g,f)` quintuple with each value in
1..5. It is conventionally followed by a separator (`,` or `;`) and then
the user-authored comment. Examples:

* `"(3,3,2,1,2),"`                          → pedigree only, no comment
* `"(3,3,2,1,2),. Water must be added"`     → pedigree + comment
* `"Free comment with no pedigree"`         → no pedigree, comment as-is

Out-of-range or malformed digits return `(Nothing, Just raw)` so we never
silently drop data we cannot interpret.
-}
parsePedigreePrefix :: Text -> (Maybe Pedigree, Maybe Text)
parsePedigreePrefix raw =
    let trimmed = T.stripStart raw
     in case T.stripPrefix "(" trimmed of
            Nothing -> (Nothing, nonEmptyText trimmed)
            Just rest -> case T.breakOn ")" rest of
                (_, "") -> (Nothing, nonEmptyText trimmed)
                (inside, afterClose) ->
                    let digits = map T.strip (T.splitOn "," inside)
                        leftover = T.drop 1 afterClose -- drop ')'
                     in case traverse readDigit digits of
                            Just [r, c, t, g, f]
                                | Just ped <- mkPedigree r c t g f ->
                                    (Just ped, nonEmptyText (stripCommentSeparators leftover))
                            _ -> (Nothing, nonEmptyText trimmed)
  where
    readDigit txt = case T.unpack (T.strip txt) of
        s | all (`elem` ("0123456789" :: String)) s -> readMaybe s
        _ -> Nothing
    -- After the closing paren SimaPro emits e.g. ",", ", ", ",. ", ";" before
    -- the comment proper. Strip leading separators and whitespace.
    stripCommentSeparators = T.dropWhile (`elem` (",;. \t" :: String))

{- | Convert a technosphere row to its exchange, flow and unit.

A row whose amount is zero is kept like any other. Zero is a modelling
statement: an input the author disabled, or a parameter that evaluates to zero
in this scenario, and the file says so on purpose. Dropping it lost that
statement without a word, made the reader disagree with the writer, and left
the biosphere side (which never dropped a zero) reading a different rule from
the same file. Nothing downstream is disturbed by the coefficient itself: both
matrix builders skip a zero entry on their own.
-}
techRowToExchange :: UnitConversion.UnitConfig -> M.Map Text Double -> TechExchangeRow -> (Exchange, TechnosphereFlow, Unit)
techRowToExchange unitCfg env TechExchangeRow{..} =
    let reading = extractLocation terName
        cleanName = maybe terName locatedName reading
        location = foldMap locatedLocation reading
        (effUnitName, resolvedAmount) = canonicalRow unitCfg terUnit (resolveAmount env terAmountRaw terAmount)
        flowUUID = generateFlowUUID cleanName ""
        unitUUID = generateUnitUUID effUnitName
        (pedigree, cleanedComment) = parsePedigreePrefix terComment
        exchange =
            TechnosphereExchange
                { techFlowId = flowUUID
                , techAmount = resolvedAmount
                , techUnitId = unitUUID
                , techRole = Input
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , techLocation = location
                , techComment = cleanedComment
                , techPedigree = pedigree
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
        flow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = cleanName
                , tfUnitId = unitUUID
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
        unit = Unit{unitId = unitUUID, unitName = effUnitName, unitSymbol = effUnitName, unitComment = ""}
     in (exchange, flow, unit)

{- | Convert biosphere row to exchange, flow, and unit in one pass
The compartment parameter is the medium the section names; berCompartment is
the row-level sub-compartment ("high. pop.", "river", etc. or empty).

'Waste' is @Final waste flows@: waste that leaves the system with no treatment
modelled for it, hence elementary. The flow's UUID is hashed from the medium's
canonical spelling, and the method reader hashes the same one, so the two sides
of a CF match meet on it.
-}
bioRowToExchange :: UnitConversion.UnitConfig -> M.Map Text Double -> Bool -> Medium -> BioExchangeRow -> (Exchange, BiosphereFlow, Unit)
bioRowToExchange unitCfg env isInput compartment BioExchangeRow{..} =
    let
        -- Keep SimaPro's per-region flow variants (`Nitrogen dioxide, FR`,
        -- `Water, FR`, …) as distinct elementary flows. EF 3.1 (and any
        -- SimaPro-style method) characterises them via suffix-keyed CFs of
        -- matching name, so collapsing them to a canonical name breaks
        -- per-region characterisation (Water use net cancellation, regional
        -- AWaRe CFs). Universal-CF matching still works through the
        -- (name, medium) fallback cascade by virtue of synonym fan-out.
        cleanName = berName
        -- UUID input goes through the shared normalizer so this side and
        -- 'Method.ParserSimaPro' agree on the hash, regardless of sub-
        -- compartment case or the SimaPro CF placeholder '(unspecified)'
        -- (which inventory rows leave blank in the same medium).
        (effUnitName, amount) = canonicalRow unitCfg berUnit (resolveAmount env berAmountRaw berAmount)
        flowUUID = generateFlowUUID cleanName (normalizeSimaProCompartment (mediumText compartment) berCompartment)
        unitUUID = generateUnitUUID effUnitName
        subcomp = if T.null berCompartment then Nothing else Just berCompartment
        (pedigree, cleanedComment) = parsePedigreePrefix berComment
        exchange =
            BiosphereExchange
                { bioFlowId = flowUUID
                , bioAmount = amount
                , bioUnitId = unitUUID
                , bioDirection = if isInput then Resource else Emission
                , bioLocation = ""
                , bioComment = cleanedComment
                , bioPedigree = pedigree
                }
        flow =
            BiosphereFlow
                { bfId = flowUUID
                , bfName = cleanName
                , bfUnitId = unitUUID
                , bfSynonyms = M.empty
                , bfCAS = Nothing
                , bfSubstanceId = Nothing
                , bfCompartment = Just (Compartment compartment subcomp)
                }
        unit = Unit{unitId = unitUUID, unitName = effUnitName, unitSymbol = effUnitName, unitComment = ""}
     in
        (exchange, flow, unit)

-- ============================================================================
-- Encoding Conversion
-- ============================================================================

{- | Ensure ByteString is proper UTF-8, converting from Windows-1252 if needed.
SimaPro CSV files use Windows-1252 encoding (no encoding header in the format).
Handles three cases:
  1. Already valid UTF-8 with no C1 controls -> pass through
  2. Valid UTF-8 but contains C1 controls (bad prior conversion) -> fix C1 chars
  3. Raw Windows-1252 bytes (not valid UTF-8) -> full Win-1252 decode
-}
ensureUtf8 :: BS.ByteString -> BS.ByteString
ensureUtf8 bs = case TE.decodeUtf8' bs of
    Right text
        | T.any isC1Control text -> TE.encodeUtf8 (fixWindows1252Controls text)
        | otherwise -> bs -- Already proper UTF-8, no changes needed
    Left _ -> TE.encodeUtf8 (decodeWindows1252 bs)
  where
    isC1Control c = c >= '\x0080' && c <= '\x009F'

{- | Decode raw Windows-1252 bytes to Text.
decodeLatin1 maps each byte to the same-valued Unicode codepoint,
then fixWindows1252Controls corrects the 0x80-0x9F range.
-}
decodeWindows1252 :: BS.ByteString -> Text
decodeWindows1252 = fixWindows1252Controls . TE.decodeLatin1

{- | Map C1 control characters (U+0080-U+009F) to their Windows-1252 equivalents.
These control characters never appear in real text; when present, they're
always Win-1252 bytes that were incorrectly mapped to Unicode codepoints.
-}
fixWindows1252Controls :: Text -> Text
fixWindows1252Controls = T.map fixChar
  where
    fixChar '\x0080' = '\x20AC' -- Euro sign
    fixChar '\x0082' = '\x201A' -- Single low-9 quotation mark
    fixChar '\x0083' = '\x0192' -- Latin small f with hook
    fixChar '\x0084' = '\x201E' -- Double low-9 quotation mark
    fixChar '\x0085' = '\x2026' -- Horizontal ellipsis
    fixChar '\x0086' = '\x2020' -- Dagger
    fixChar '\x0087' = '\x2021' -- Double dagger
    fixChar '\x0088' = '\x02C6' -- Modifier letter circumflex
    fixChar '\x0089' = '\x2030' -- Per mille sign
    fixChar '\x008A' = '\x0160' -- Latin capital S with caron
    fixChar '\x008B' = '\x2039' -- Single left-pointing angle quote
    fixChar '\x008C' = '\x0152' -- Latin capital ligature OE
    fixChar '\x008E' = '\x017D' -- Latin capital Z with caron
    fixChar '\x0091' = '\x2018' -- Left single quotation mark
    fixChar '\x0092' = '\x2019' -- Right single quotation mark
    fixChar '\x0093' = '\x201C' -- Left double quotation mark
    fixChar '\x0094' = '\x201D' -- Right double quotation mark
    fixChar '\x0095' = '\x2022' -- Bullet
    fixChar '\x0096' = '\x2013' -- En dash
    fixChar '\x0097' = '\x2014' -- Em dash
    fixChar '\x0098' = '\x02DC' -- Small tilde
    fixChar '\x0099' = '\x2122' -- Trade mark sign
    fixChar '\x009A' = '\x0161' -- Latin small s with caron
    fixChar '\x009B' = '\x203A' -- Single right-pointing angle quote
    fixChar '\x009C' = '\x0153' -- Latin small ligature oe
    fixChar '\x009E' = '\x017E' -- Latin small z with caron
    fixChar '\x009F' = '\x0178' -- Latin capital Y with diaeresis
    fixChar c = c

-- ============================================================================
-- Main Parser
-- ============================================================================

-- ============================================================================
-- Parallel Parsing Helpers
-- ============================================================================

{- | Extract SimaProConfig from header lines (lines starting with '{').
Stops at the first non-header, non-empty line.
-}
extractConfig :: [BS.ByteString] -> SimaProConfig
extractConfig = foldl' step defaultConfig . takeWhile isHeaderOrEmpty
  where
    isHeaderOrEmpty l = let s = BS8.strip l in BS.null s || BS8.isPrefixOf "{" s
    step cfg line = case parseHeaderLine line of
        Just (key, value) -> updateConfigFromHeader cfg key value
        Nothing -> cfg

{- | Split lines into N contiguous chunks at End boundaries.
Each chunk contains roughly totalEnds/N complete blocks.
-}
splitForWorkers :: Int -> [BS.ByteString] -> [[BS.ByteString]]
splitForWorkers numWorkers allLines
    | numWorkers <= 1 = [allLines]
    | totalEnds == 0 = [allLines]
    | otherwise = chopAtEnds endsPerChunk 0 [] allLines
  where
    isEnd l = BS8.strip l == "End"
    totalEnds = foldl' (\acc l -> if isEnd l then acc + 1 else acc) (0 :: Int) allLines
    endsPerChunk = max 1 ((totalEnds + numWorkers - 1) `div` numWorkers)

    chopAtEnds _ _ acc [] = [reverse acc | not (null acc)]
    chopAtEnds target endCount acc (l : ls)
        | isEnd l
        , endCount + 1 >= target =
            reverse (l : acc) : chopAtEnds target 0 [] ls
        | isEnd l =
            chopAtEnds target (endCount + 1) (l : acc) ls
        | otherwise =
            chopAtEnds target endCount (l : acc) ls

-- | Parse a contiguous range of lines into ProcessBlocks + global params.
parseWorkerLines :: SimaProConfig -> [BS.ByteString] -> WorkerResult
parseWorkerLines cfg ls =
    let initAcc =
            ParseAcc
                { paConfig = cfg
                , paState = BetweenBlocks
                , paInProcess = False
                , paCurrentBlock = emptyProcessBlock
                , paBlocks = []
                , paLineNum = 0
                , paDbInputParams = []
                , paDbCalcParams = []
                , paProjInputParams = []
                , paProjCalcParams = []
                , paSubstanceCAS = []
                }
        finalAcc = foldl' processLine initAcc ls
     in WorkerResult
            { wrBlocks = reverse (paBlocks finalAcc)
            , wrParams =
                GlobalParams
                    { gpDbInput = paDbInputParams finalAcc
                    , gpDbCalc = paDbCalcParams finalAcc
                    , gpProjInput = paProjInputParams finalAcc
                    , gpProjCalc = paProjCalcParams finalAcc
                    }
            , -- Restore file order (rows accumulate reversed): downstream the
              -- first binding of a name wins, and "first" must mean the file's.
              wrSubstanceCAS = reverse (paSubstanceCAS finalAcc)
            }

{- | Fill empty biosphere-flow CAS from the @(name, CAS)@ pairs a SimaPro
export lists in its trailing substance registry. Holes only — reuses
'fillBioFlowCAS', so a CAS the flow already carries is never overwritten. Name
and CAS are canonicalized the same way the runtime registry bridge is
('normalizeName' / 'normalizeCAS'), so a filled CAS keys the same as one the
method side resolved. A registry binding one name to two different CAS follows
the runtime registry's rule — the first wins — and the conflicts come back for
the caller to report. A no-op when the file had no registry.
-}
fillCASFromRegistry :: [(Text, Text)] -> BioFlowDB -> (BioFlowDB, [(NormName, (CASNumber, CASNumber))])
fillCASFromRegistry substanceCAS db = (fillBioFlowCAS bindings db, conflicts)
  where
    (bindings, conflicts) =
        casBindings
            [ (NormName (normalizeName nm), CASNumber (normalizeCAS cas))
            | (nm, cas) <- substanceCAS
            ]

-- ============================================================================
-- Main Entry Point
-- ============================================================================

{- | Parse a SimaPro CSV file
Handles Windows-1252/Latin-1 encoding common in SimaPro exports.

Reference-product amounts are normalized to the canonical base unit of their
dimension (e.g. 1 t → 1000 kg) during parsing, so downstream matrix
construction yields per-base-unit columns.
-}

{- | Index flows by their identifier, refusing a pair that disagrees on the unit.

Two rows of the same name and compartment land on one entry. When their units
convert into each other the conversion at ingest has already brought both to
the reference unit of the dimension, so they agree. When no conversion relates
them -- an energy against a mass -- nothing can make them one flow, and
'M.fromList' would silently keep whichever row came last. Refuse the file
instead, naming the flow and both units.
-}
indexFlows :: M.Map UUID.UUID Text -> (a -> (UUID.UUID, UUID.UUID, Text)) -> [a] -> Either Text (M.Map UUID.UUID a)
indexFlows unitNames identity = foldM add M.empty
  where
    add acc flow =
        let (flowId, unitRef, name) = identity flow
         in case identity <$> M.lookup flowId acc of
                Just (_, seen, _)
                    | seen /= unitRef ->
                        Left $
                            "flow '"
                                <> name
                                <> "' is written in two units that no conversion relates ('"
                                <> nameOf seen
                                <> "' and '"
                                <> nameOf unitRef
                                <> "'), so they cannot be one flow"
                _ -> Right (M.insert flowId flow acc)
    nameOf u = M.findWithDefault (UUID.toText u) u unitNames

{- | The units a SimaPro file declares for itself, in its @Units@ block: one
row of @name;quantity;conversion;reference unit@ between a line reading
@Units@ and the next @End@. The quantity is the file's own word for the
dimension and is not read: the dimension comes from the reference unit, which
is the one row of its quantity that states itself.
-}
unitDeclarations :: SimaProConfig -> [BS.ByteString] -> [UnitConversion.UnitDeclaration]
unitDeclarations cfg = mapMaybe row . takeWhile (/= "End") . drop 1 . dropWhile (/= "Units")
  where
    row :: BS.ByteString -> Maybe UnitConversion.UnitDeclaration
    row line = case splitCSV (spDelimiter cfg) line of
        (name : _quantity : howMany : reference : _)
            | not (BS.null name)
            , not (BS.null reference) ->
                UnitConversion.UnitDeclaration (decodeBS name) (decodeBS reference)
                    <$> readAmount (Expr.normalizeExpr (spDecimal cfg) (decodeBS howMany))
        _ -> Nothing

parseSimaProCSV :: UnitConversion.UnitConfig -> FilePath -> IO (Either Text ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB))
parseSimaProCSV unitCfg path = do
    reportProgress Info $ "Loading SimaPro CSV file: " ++ path
    startTime <- getCurrentTime

    -- Read as ByteString and convert from Windows-1252 to proper UTF-8.
    rawContent <- BS.readFile path
    let !utf8Content = ensureUtf8 rawContent
        lines' = map stripCR (BS8.lines utf8Content)

    -- Extract config from header (fast, sequential, ~5 lines)
    let cfg = extractConfig lines'

    -- A file carries its own unit table, and it is the one its amounts were
    -- written against: a spelling the shipped table never had, or a size it has
    -- differently, is settled here rather than guessed at row by row.
    let (fileUnits, unitNotes) = UnitConversion.addDeclaredUnits unitCfg (unitDeclarations cfg lines')
    forM_ unitNotes (reportProgress Warning . T.unpack)

    -- Split lines into N contiguous chunks at End boundaries
    numWorkers <- getNumCapabilities
    let workerChunks = splitForWorkers numWorkers lines'

    reportProgress Info $ printf "Parsing with %d parallel workers" numWorkers

    -- Parse chunks in parallel — each worker folds its contiguous range
    results <- mapConcurrently (evaluate . force . parseWorkerLines cfg) workerChunks
    let allBlocks = concatMap wrBlocks results
        globalParams = foldMap wrParams results
        substanceCAS = concatMap wrSubstanceCAS results

    -- Convert all blocks to activities (one per block; a block without a
    -- product row is no activity) - PARALLEL
    converted <- catMaybes <$> mapConcurrently (evaluate . force . processBlockToActivity fileUnits globalParams) allBlocks

    -- Surface every amount the conversion replaced with its lenient fallback:
    -- a number that silently shrinks is worse than a warned one.
    fallbacks <- concat <$> mapConcurrently (evaluate . force . fallbackAmounts globalParams) allBlocks
    forM_ fallbacks $ \(name, raw, fallback) ->
        reportProgress Warning $
            printf
                "amount '%s' in '%s' is neither a number nor a resolvable expression; using %g"
                (T.unpack raw)
                (T.unpack name)
                fallback
    let (activities, ambiguousIds) = dropAmbiguousNativeIds (map (\(a, _, _, _, _) -> a) converted)
    forM_ ambiguousIds $ \nativeId ->
        reportProgress Warning $
            printf
                "process identifier '%s' names more than one process; those blocks are identified by name and location instead"
                (T.unpack nativeId)
    let
        allTechFlows = concatMap (\(_, tf, _, _, _) -> tf) converted
        allBioFlows = concatMap (\(_, _, bf, _, _) -> bf) converted
        allWasteFlows = concatMap (\(_, _, _, wf, _) -> wf) converted
        allUnits = concatMap (\(_, _, _, _, u) -> u) converted

    -- Build deduplicated maps — UUID disjointness across kinds is guaranteed
    -- by construction: a technosphere flow hashes with an empty compartment, an
    -- elementary one with the compartment of the section it came from.
    let unitDB = M.fromList [(unitId u, u) | u <- allUnits]
        unitNames = M.map unitName unitDB
        indexed = do
            techFlowDB <- indexFlows unitNames (\f -> (tfId f, tfUnitId f, tfName f)) allTechFlows
            -- Fill empty flow CAS from the file's own substance registry (the
            -- trailing name;unit;cas blocks) so the native CAS bridge fires on
            -- a SimaPro export, which otherwise carries no per-flow CAS at all.
            bioIndexed <- indexFlows unitNames (\f -> (bfId f, bfUnitId f, bfName f)) allBioFlows
            wasteFlowDB <- indexFlows unitNames (\f -> (wfId f, wfUnitId f, wfName f)) allWasteFlows
            pure (techFlowDB, fillCASFromRegistry substanceCAS bioIndexed, wasteFlowDB)

    case indexed of
        Left err -> pure (Left err)
        Right (techFlowDB, (bioFlowDB, casConflicts), wasteFlowDB) -> do
            forM_ casConflicts $ \(NormName n, (CASNumber kept, CASNumber ignored)) ->
                reportProgress Warning $
                    printf
                        "substance registry binds '%s' to two CAS (%s kept, %s ignored)"
                        (T.unpack n)
                        (T.unpack kept)
                        (T.unpack ignored)

            -- Force evaluation before returning
            let !numActivities = length activities
            let !numTechFlows = M.size techFlowDB
            let !numBioFlows = M.size bioFlowDB
            let !numBioFlowsCAS = length [() | f <- M.elems bioFlowDB, maybe False (not . T.null) (bfCAS f)]
            let !numWasteFlows = M.size wasteFlowDB
            let !numUnits = M.size unitDB

            endTime <- getCurrentTime
            let duration = realToFrac (diffUTCTime endTime startTime) :: Double
            reportProgress Info $ printf "SimaPro parsing completed in %.2fs:" duration
            reportProgress Info $ printf "  Activities: %d processes" numActivities
            reportProgress Info $ printf "  Technosphere flows: %d unique" numTechFlows
            reportProgress Info $ printf "  Biosphere flows: %d unique (%d carry a CAS)" numBioFlows numBioFlowsCAS
            reportProgress Info $ printf "  Waste flows: %d unique" numWasteFlows
            reportProgress Info $ printf "  Units: %d unique" numUnits

            return (Right (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB))
  where
    -- Strip Windows \r from ByteString (fast, often no-op)
    stripCR :: BS.ByteString -> BS.ByteString
    stripCR bs = fromMaybe bs (BS.stripSuffix "\r" bs)

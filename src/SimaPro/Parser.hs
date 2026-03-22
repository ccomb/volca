{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | SimaPro CSV Parser for volca
-- Parses SimaPro CSV exports (like Agribalyse) into volca data structures
module SimaPro.Parser
    ( parseSimaProCSV
    , SimaProConfig(..)
    , ProcessBlock(..)
    , ProductRow(..)
    , TechExchangeRow(..)
    , BioExchangeRow(..)
    , generateActivityUUID
    , generateFlowUUID
    , generateUnitUUID
      -- * Shared utilities (used by Method.ParserSimaPro)
    , defaultConfig
    , simaproNamespace
    , ensureUtf8
    , splitCSV
    , parseAmount
    , decodeBS
    ) where

import Types
import qualified SimaPro.Expr as Expr
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Char (isUpper, toLower)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import Progress (reportProgress, ProgressLevel(..))
import Text.Printf (printf)

-- ============================================================================
-- Configuration Types
-- ============================================================================

-- | SimaPro file configuration extracted from header
data SimaProConfig = SimaProConfig
    { spVersion :: !Text           -- SimaPro version (e.g. "9.6.0.1")
    , spFileType :: !Text          -- "processes", "methods", "product stages"
    , spDelimiter :: !Char         -- CSV delimiter (';', ',', '\t')
    , spDecimal :: !Char           -- Decimal separator (',' or '.')
    , spDateFormat :: !Text        -- Date format string
    }
    deriving (Show, Eq, Generic)
instance NFData SimaProConfig

-- | Default configuration (semicolon delimiter, comma decimal)
defaultConfig :: SimaProConfig
defaultConfig = SimaProConfig
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
    , pbType :: !Text             -- "Unit process" or "System"
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
    , pbInputParams :: ![(Text, Text)]   -- name -> raw value
    , pbCalcParams  :: ![(Text, Text)]   -- name -> expression
    }
    deriving (Show, Eq, Generic)
instance NFData ProcessBlock

-- | Empty process block
emptyProcessBlock :: ProcessBlock
emptyProcessBlock = ProcessBlock
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

-- | Section types within a process block
data SectionType
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
    | SecDbInputParams
    | SecDbCalcParams
    | SecProjInputParams
    | SecProjCalcParams
    | SecNone
    deriving (Show, Eq)

-- | Parser state
data ParseState
    = InHeader
    | InProcessMeta !BS.ByteString  -- Current metadata key being read
    | InSection !SectionType
    | BetweenBlocks
    deriving (Show, Eq)

-- | Parse state accumulator
data ParseAcc = ParseAcc
    { paConfig :: !SimaProConfig
    , paState :: !ParseState
    , paCurrentBlock :: !ProcessBlock
    , paBlocks :: ![ProcessBlock]
    , paLineNum :: !Int
    , paDbInputParams   :: ![(Text, Text)]
    , paDbCalcParams    :: ![(Text, Text)]
    , paProjInputParams :: ![(Text, Text)]
    , paProjCalcParams  :: ![(Text, Text)]
    }

-- ============================================================================
-- Header Parsing
-- ============================================================================

-- | Parse a header line like "{key: value}" or "{value}"
parseHeaderLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseHeaderLine line
    | BS8.isPrefixOf "{" line && BS8.isSuffixOf "}" line =
        let content = BS8.init (BS8.tail line)
        in case BS8.breakSubstring ": " content of
            (key, rest) | not (BS.null rest) -> Just (BS8.strip key, BS8.strip (BS.drop 2 rest))
            _ -> Just (BS8.strip content, "")
    | otherwise = Nothing

-- | Update config from header line (takes ByteString, stores Text)
updateConfigFromHeader :: SimaProConfig -> BS.ByteString -> BS.ByteString -> SimaProConfig
updateConfigFromHeader cfg key value = case BS8.map toLower key of
    k | "simapro" `BS8.isPrefixOf` k -> cfg { spVersion = decodeBS key }
    "processes" -> cfg { spFileType = "processes" }
    "methods" -> cfg { spFileType = "methods" }
    "product stages" -> cfg { spFileType = "product stages" }
    "csv separator" -> cfg { spDelimiter = parseDelimiter value }
    "decimal separator" -> cfg { spDecimal = if BS.null value then ',' else BS8.head value }
    "short date format" -> cfg { spDateFormat = localDecodeBS value }
    _ -> cfg
  where
    localDecodeBS = TE.decodeUtf8With TEE.lenientDecode
    parseDelimiter v
        | v == "Semicolon" = ';'
        | v == "Comma"     = ','
        | v == "Tab"       = '\t'
        | otherwise        = ';'

-- ============================================================================
-- Section Detection
-- ============================================================================

-- | Detect section type from line (ByteString)
detectSection :: BS.ByteString -> Maybe SectionType
detectSection line = case BS8.strip line of
    "Products" -> Just SecProducts
    "Waste treatment" -> Just SecProducts
    "Avoided products" -> Just SecAvoidedProducts
    "Materials/fuels" -> Just SecMaterials
    "Electricity/heat" -> Just SecElectricity
    "Waste to treatment" -> Just SecWasteToTreatment
    "Resources" -> Just SecResources
    "Emissions to air" -> Just SecEmissionsAir
    "Emissions to water" -> Just SecEmissionsWater
    "Emissions to soil" -> Just SecEmissionsSoil
    "Final waste flows" -> Just SecFinalWaste
    "Input parameters" -> Just SecInputParams
    "Calculated parameters" -> Just SecCalcParams
    "Database Input parameters" -> Just SecDbInputParams
    "Database Calculated parameters" -> Just SecDbCalcParams
    "Project Input parameters" -> Just SecProjInputParams
    "Project Calculated parameters" -> Just SecProjCalcParams
    "Non material emissions" -> Just SecNone  -- Ignore
    "Social issues" -> Just SecNone
    "Economic issues" -> Just SecNone
    _ -> Nothing

-- | Known metadata keys in process block (ByteString)
isMetadataKey :: BS.ByteString -> Bool
isMetadataKey key = key `elem`
    [ "Category type", "Process identifier", "Type", "Process name"
    , "Status", "Time period", "Geography", "Technology"
    , "Representativeness", "Multiple output allocation", "Substitution allocation"
    , "Cut off rules", "Capital goods", "Boundary with nature"
    , "Infrastructure", "Date", "Record", "Generator"
    , "External documents", "Literature references", "Collection method"
    , "Data treatment", "Verification", "Comment", "Allocation rules"
    , "System description", "PlatformId"
    ]

-- ============================================================================
-- Row Parsing (ByteString based, decode to Text only when storing)
-- ============================================================================

-- | Decode ByteString to Text (lenient UTF-8)
decodeBS :: BS.ByteString -> Text
decodeBS = TE.decodeUtf8With TEE.lenientDecode
{-# INLINE decodeBS #-}

-- | Parse amount with configurable decimal separator (ByteString)
parseAmount :: Char -> BS.ByteString -> Double
parseAmount decimalSep bs
    | BS.null bs = 0.0
    | otherwise =
        let normalized = if decimalSep == ','
                         then BS8.map (\c -> if c == ',' then '.' else c) bs
                         else bs
        in case reads (BS8.unpack normalized) of
            [(val, "")] -> val
            [(val, _)] -> val  -- Allow trailing characters
            _ -> 0.0

-- | Split a CSV line by delimiter, respecting RFC 4180 quoted fields.
splitCSV :: Char -> BS.ByteString -> [BS.ByteString]
splitCSV delim bs =
    let opts = Csv.defaultDecodeOptions
            { Csv.decDelimiter = fromIntegral (fromEnum delim) }
    in case Csv.decodeWith opts Csv.NoHeader (BL.fromStrict bs) of
        Right rows | not (V.null rows) -> V.toList (V.head rows)
        _ -> BS8.split delim bs  -- fallback to naive on parse error

-- | Parse a parameter row: name;value_or_expression;...
parseParamRow :: SimaProConfig -> BS.ByteString -> Maybe (Text, Text)
parseParamRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
    in case fields of
        (name:value:_) ->
            let n = decodeBS (BS8.strip name)
                v = Expr.normalizeExpr (spDecimal cfg) (decodeBS (BS8.strip value))
            in if T.null n then Nothing else Just (n, v)
        _ -> Nothing

-- | Parse a reference product line.
-- SimaPro CSV format:
--   0. name
--   1. unit
--   2. value or formula
--   3. allocation
--   4. waste type
--   5. category (separated by \)
--   6. comment
-- However, some waste treatment product rows omit the allocation field (field 3),
-- producing only 6 fields. We detect this by checking whether field 3 looks numeric
-- (allocation is a percentage like "100" or "33.5"). If it doesn't, we treat it as
-- waste_type and shift the remaining fields accordingly.
-- Without this detection, the comment (which often contains \x7f-separated EcoSpold
-- property metadata) ends up being parsed as the category.
parseProductRow :: SimaProConfig -> BS.ByteString -> Maybe ProductRow
parseProductRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
        norm = Expr.normalizeExpr (spDecimal cfg) . decodeBS . BS8.strip
    in case fields of
        -- 7+ fields with allocation (number or formula): name;unit;amount;alloc;waste;cat;comment
        (name:unit:amount:alloc:waste:cat:rest)
            | isAllocationField cfg alloc -> Just ProductRow
                { prName = decodeBS (BS8.strip name)
                , prUnit = decodeBS (BS8.strip unit)
                , prAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
                , prAmountRaw = norm amount
                , prAllocation = parseAmount (spDecimal cfg) (BS8.strip alloc)
                , prAllocRaw = norm alloc
                , prWasteType = decodeBS (BS8.strip waste)
                , prCategory = decodeBS (BS8.strip cat)
                , prComment = decodeBS (BS8.intercalate ";" rest)
                }
        -- 6+ fields without allocation: name;unit;amount;waste;cat;comment
        (name:unit:amount:waste:cat:rest) -> Just ProductRow
            { prName = decodeBS (BS8.strip name)
            , prUnit = decodeBS (BS8.strip unit)
            , prAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
            , prAmountRaw = norm amount
            , prAllocation = 100
            , prAllocRaw = "100"
            , prWasteType = decodeBS (BS8.strip waste)
            , prCategory = decodeBS (BS8.strip cat)
            , prComment = decodeBS (BS8.intercalate ";" rest)
            }
        _ -> Nothing


-- | Check if a ByteString field is a valid expression (number, variable, or formula).
-- Uses the Megaparsec expression parser syntactically — accepts any identifier without
-- needing parameter values. Waste type descriptions ("All waste types") fail to parse.
isAllocationField :: SimaProConfig -> BS.ByteString -> Bool
isAllocationField cfg bs = Expr.isExpression (spDecimal cfg) (decodeBS (BS8.strip bs))

-- | Parse a technosphere exchange row (ByteString input, Text output)
parseTechRow :: SimaProConfig -> BS.ByteString -> Maybe TechExchangeRow
parseTechRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
        norm = Expr.normalizeExpr (spDecimal cfg) . decodeBS . BS8.strip
    in case fields of
        (name:unit:amount:unc:_:_:_:rest) -> Just TechExchangeRow
            { terName = decodeBS (BS8.strip name)
            , terUnit = decodeBS (BS8.strip unit)
            , terAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
            , terAmountRaw = norm amount
            , terUncertainty = decodeBS (BS8.strip unc)
            , terComment = decodeBS (BS8.intercalate ";" rest)
            }
        (name:unit:amount:rest) -> Just TechExchangeRow
            { terName = decodeBS (BS8.strip name)
            , terUnit = decodeBS (BS8.strip unit)
            , terAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
            , terAmountRaw = norm amount
            , terUncertainty = ""
            , terComment = decodeBS (BS8.intercalate ";" rest)
            }
        _ -> Nothing

-- | Parse a biosphere exchange row (ByteString input, Text output)
parseBioRow :: SimaProConfig -> BS.ByteString -> Maybe BioExchangeRow
parseBioRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
        norm = Expr.normalizeExpr (spDecimal cfg) . decodeBS . BS8.strip
    in case fields of
        (name:compartment:unit:amount:unc:_:_:_:rest) -> Just BioExchangeRow
            { berName = decodeBS (BS8.strip name)
            , berCompartment = decodeBS (BS8.strip compartment)
            , berUnit = decodeBS (BS8.strip unit)
            , berAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
            , berAmountRaw = norm amount
            , berUncertainty = decodeBS (BS8.strip unc)
            , berComment = decodeBS (BS8.intercalate ";" rest)
            }
        (name:compartment:unit:amount:rest) -> Just BioExchangeRow
            { berName = decodeBS (BS8.strip name)
            , berCompartment = decodeBS (BS8.strip compartment)
            , berUnit = decodeBS (BS8.strip unit)
            , berAmount = parseAmount (spDecimal cfg) (BS8.strip amount)
            , berAmountRaw = norm amount
            , berUncertainty = ""
            , berComment = decodeBS (BS8.intercalate ";" rest)
            }
        _ -> Nothing

-- ============================================================================
-- State Machine Processing (ByteString based)
-- ============================================================================

-- | Process a single line (ByteString)
processLine :: ParseAcc -> BS.ByteString -> ParseAcc
processLine acc@ParseAcc{..} line
    -- Empty line handling
    | BS.null (BS8.strip line) = case paState of
        InProcessMeta _ -> acc { paState = BetweenBlocks }
        InSection _ -> acc { paState = BetweenBlocks }
        _ -> acc

    -- Header lines
    | Just (key, value) <- parseHeaderLine line, paState == InHeader =
        acc { paConfig = updateConfigFromHeader paConfig key value }

    -- Process block start
    | BS8.strip line == "Process" =
        acc { paState = BetweenBlocks
            , paCurrentBlock = emptyProcessBlock
            }

    -- End of block
    | BS8.strip line == "End" =
        let block = paCurrentBlock
            -- A block is valid if it has at least one product (process name not required)
            isValid = not (null (pbProducts block))
        in acc { paState = BetweenBlocks
               , paBlocks = if isValid then block : paBlocks else paBlocks
               , paCurrentBlock = emptyProcessBlock
               }

    -- Section detection
    | Just sec <- detectSection line =
        acc { paState = InSection sec }

    -- In a section, parse row (route db/project params to ParseAcc, process params to block)
    | InSection sec <- paState, not (BS.null (BS8.strip line)) =
        case sec of
            SecDbInputParams -> case parseParamRow paConfig line of
                Just p  -> acc { paDbInputParams = p : paDbInputParams }
                Nothing -> acc
            SecDbCalcParams -> case parseParamRow paConfig line of
                Just p  -> acc { paDbCalcParams = p : paDbCalcParams }
                Nothing -> acc
            SecProjInputParams -> case parseParamRow paConfig line of
                Just p  -> acc { paProjInputParams = p : paProjInputParams }
                Nothing -> acc
            SecProjCalcParams -> case parseParamRow paConfig line of
                Just p  -> acc { paProjCalcParams = p : paProjCalcParams }
                Nothing -> acc
            _ -> acc { paCurrentBlock = addRowToBlock paConfig sec line paCurrentBlock }

    -- Metadata key-value pairs
    | paState == BetweenBlocks || isMetadataKey (BS8.strip line) =
        if isMetadataKey (BS8.strip line)
        then acc { paState = InProcessMeta (BS8.strip line) }
        else case paState of
            InProcessMeta key -> acc { paCurrentBlock = setMetadata key line paCurrentBlock
                                     , paState = BetweenBlocks
                                     }
            _ -> acc

    -- Value for metadata key
    | InProcessMeta key <- paState =
        acc { paCurrentBlock = setMetadata key line paCurrentBlock
            , paState = BetweenBlocks
            }

    | otherwise = acc { paLineNum = paLineNum + 1 }

-- | Add a row to the appropriate list in the block (ByteString)
addRowToBlock :: SimaProConfig -> SectionType -> BS.ByteString -> ProcessBlock -> ProcessBlock
addRowToBlock cfg sec line block = case sec of
    SecProducts -> case parseProductRow cfg line of
        Just row -> block { pbProducts = row : pbProducts block }
        Nothing -> block
    SecAvoidedProducts -> case parseProductRow cfg line of
        Just row -> block { pbAvoidedProducts = row : pbAvoidedProducts block }
        Nothing -> block
    SecMaterials -> case parseTechRow cfg line of
        Just row -> block { pbMaterials = row : pbMaterials block }
        Nothing -> block
    SecElectricity -> case parseTechRow cfg line of
        Just row -> block { pbElectricity = row : pbElectricity block }
        Nothing -> block
    SecWasteToTreatment -> case parseTechRow cfg line of
        Just row -> block { pbWasteToTreatment = row : pbWasteToTreatment block }
        Nothing -> block
    SecResources -> case parseBioRow cfg line of
        Just row -> block { pbResources = row : pbResources block }
        Nothing -> block
    SecEmissionsAir -> case parseBioRow cfg line of
        Just row -> block { pbEmissionsAir = row : pbEmissionsAir block }
        Nothing -> block
    SecEmissionsWater -> case parseBioRow cfg line of
        Just row -> block { pbEmissionsWater = row : pbEmissionsWater block }
        Nothing -> block
    SecEmissionsSoil -> case parseBioRow cfg line of
        Just row -> block { pbEmissionsSoil = row : pbEmissionsSoil block }
        Nothing -> block
    SecFinalWaste -> case parseBioRow cfg line of
        Just row -> block { pbFinalWaste = row : pbFinalWaste block }
        Nothing -> block
    SecInputParams -> case parseParamRow cfg line of
        Just p  -> block { pbInputParams = p : pbInputParams block }
        Nothing -> block
    SecCalcParams -> case parseParamRow cfg line of
        Just p  -> block { pbCalcParams = p : pbCalcParams block }
        Nothing -> block
    _ -> block

-- | Set metadata field in block (ByteString key, decode value to Text)
setMetadata :: BS.ByteString -> BS.ByteString -> ProcessBlock -> ProcessBlock
setMetadata key value block = case key of
    "Category type" -> block { pbCategoryType = decodeBS (BS8.strip value) }
    "Process identifier" -> block { pbIdentifier = decodeBS (BS8.strip value) }
    "Type" -> block { pbType = decodeBS (BS8.strip value) }
    "Process name" -> block { pbName = decodeBS (BS8.strip value) }
    "Status" -> block { pbStatus = decodeBS (BS8.strip value) }
    "Time period" -> block { pbTimePeriod = decodeBS (BS8.strip value) }
    "Geography" -> block { pbLocation = decodeBS (BS8.strip value) }
    "Technology" -> block { pbTechnology = decodeBS (BS8.strip value) }
    "Record" -> block { pbRecord = decodeBS (BS8.strip value) }
    "Comment" -> block { pbComment = decodeBS (BS8.strip value) }
    _ -> block

-- ============================================================================
-- UUID Generation
-- ============================================================================

-- | Namespace for SimaPro UUIDs
simaproNamespace :: UUID
simaproNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "simapro.pre.nl")

-- | Generate deterministic activity UUID from Activity (uses name + location)
generateActivityUUID :: Activity -> UUID
generateActivityUUID act =
    UUID5.generateNamed simaproNamespace
        (BS.unpack $ TE.encodeUtf8 $ "activity:" <> activityName act <> "@" <> activityLocation act)

-- | Generate deterministic flow UUID from name, compartment, unit
generateFlowUUID :: Text -> Text -> Text -> UUID
generateFlowUUID name compartment unit =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "flow:" <> name <> ":" <> compartment <> ":" <> unit)

-- | Generate deterministic unit UUID from name
generateUnitUUID :: Text -> UUID
generateUnitUUID unitName =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "unit:" <> unitName)

-- ============================================================================
-- Conversion to volca Types
-- ============================================================================

-- | Extract location from SimaPro-style names
-- Handles: "Name {FR}", "Name {Europe without Switzerland}| market for...", etc.
-- Also handles WFLDB-style "/XX" suffix (e.g. "Ammonium nitrate .../CN").
-- In SimaPro format, curly braces always denote geography.
extractLocation :: Text -> (Text, Text)
extractLocation name =
    case T.breakOn "{" name of
        (_, rest) | not (T.null rest) ->
            case T.breakOn "}" (T.drop 1 rest) of
                (loc, afterBrace) | not (T.null afterBrace) ->
                    let cleanLoc = T.strip loc
                    in if T.length cleanLoc >= 2
                       then (T.strip name, cleanLoc)
                       else (name, "")
                _ -> (name, "")
        _ -> -- No {XX} found — try WFLDB-style /XX suffix
            case extractSlashLocation name of
                Just (cleanName, loc) -> (cleanName, loc)
                Nothing               -> (name, "")
  where
    -- Extract location from WFLDB-style slash suffixes like:
    --   "Product (WFLDB)/CN U"       → ("Product (WFLDB)", "CN")
    --   "Product/ha/GLO/I U"         → ("Product/ha", "GLO")
    -- Scans rightward through slash-separated segments for the first geo code.
    extractSlashLocation n = go n
      where
        go t = case T.breakOnEnd "/" t of
            ("", _) -> Nothing
            (before, suffix) -> case T.words (T.strip suffix) of
                (loc:_) | isGeoCode loc ->
                    Just (T.strip (T.dropWhileEnd (== '/') before), loc)
                _ -> go (T.dropWhileEnd (== '/') before)
    isGeoCode t = T.length t >= 2 && isUpper (T.head t)

-- | Resolve a parameterized amount: try expression evaluation, fall back to numeric parse
resolveAmount :: M.Map Text Double -> Text -> Double -> Double
resolveAmount env raw fallback
    | T.null raw = fallback
    | otherwise = case Expr.evaluate env raw of
        Right v -> v
        Left _  -> fallback

-- | Convert ProcessBlock to list of Activities (one per product)
-- This matches EcoSpold behavior where multi-product processes create multiple activities
-- Global params (db + project level) are passed in and combined with process-level params.
processBlockToActivity :: ([(Text, Text)], [(Text, Text)], [(Text, Text)], [(Text, Text)])
                       -> ProcessBlock -> [(Activity, [Flow], [Unit])]
processBlockToActivity (dbInputPs, dbCalcPs, projInputPs, projCalcPs) ProcessBlock{..} =
    let -- Build parameter environment: input params first, then calculated params
        evalParam acc (name, rawVal) = case Expr.evaluate acc rawVal of
            Right v -> M.insert name v acc
            Left _  -> acc
        env0 = foldl' evalParam M.empty (reverse dbInputPs)
        env1 = foldl' evalParam env0 (reverse projInputPs)
        env2 = foldl' evalParam env1 (reverse pbInputParams)
        env3 = foldl' evalParam env2 (reverse dbCalcPs)
        env4 = foldl' evalParam env3 (reverse projCalcPs)
        env  = foldl' evalParam env4 (reverse pbCalcParams)

        -- Raw expression map for re-evaluation
        allExprs = reverse dbInputPs ++ reverse projInputPs ++ reverse pbInputParams
                ++ reverse dbCalcPs ++ reverse projCalcPs ++ reverse pbCalcParams
        exprMap = M.fromList allExprs

        -- Extract location from process name if not specified
        (_, locFromName) = extractLocation pbName
        location = if T.null pbLocation || T.toLower pbLocation == "unspecified" then locFromName else pbLocation

        -- Convert avoided products (shared across all activities)
        avoidedExchanges = map (productToExchange env False) pbAvoidedProducts

        -- Convert technosphere inputs (shared across all activities)
        techExchanges = concatMap (techRowToExchange env True) (pbMaterials ++ pbElectricity)
        wasteExchanges = concatMap (techRowToExchange env True) pbWasteToTreatment

        -- Convert biosphere exchanges (shared across all activities)
        resourceExchanges = map (bioRowToExchange env True "resource") pbResources
        airExchanges = map (bioRowToExchange env False "air") pbEmissionsAir
        waterExchanges = map (bioRowToExchange env False "water") pbEmissionsWater
        soilExchanges = map (bioRowToExchange env False "soil") pbEmissionsSoil
        wasteFlowExchanges = map (bioRowToExchange env False "waste") pbFinalWaste

        sharedExchanges = avoidedExchanges ++
                          techExchanges ++ wasteExchanges ++
                          resourceExchanges ++ airExchanges ++ waterExchanges ++
                          soilExchanges ++ wasteFlowExchanges

        -- Collect all flows and units (shared)
        allFlows = collectFlows (pbProducts ++ pbAvoidedProducts)
                                (pbMaterials ++ pbElectricity ++ pbWasteToTreatment)
                                (pbResources ++ pbEmissionsAir ++ pbEmissionsWater ++
                                 pbEmissionsSoil ++ pbFinalWaste)
        allUnits = collectUnits (pbProducts ++ pbAvoidedProducts)
                                (pbMaterials ++ pbElectricity ++ pbWasteToTreatment)
                                (pbResources ++ pbEmissionsAir ++ pbEmissionsWater ++
                                 pbEmissionsSoil ++ pbFinalWaste)

        -- Create one activity per product
        makeActivity :: ProductRow -> (Activity, [Flow], [Unit])
        makeActivity prod =
            let productExchange = productToExchange env True prod
                allocFraction = resolveAmount env (prAllocRaw prod) (prAllocation prod) / 100.0
                (cleanProductName, locFromProduct) = extractLocation (prName prod)
                effectiveLoc = if T.null location then locFromProduct else location
                activity = Activity
                    { activityName = cleanProductName
                    , activityDescription = if T.null pbComment then [] else [pbComment]
                    , activitySynonyms = M.empty
                    , activityClassification = M.fromList $ filter (not . T.null . snd)
                        [ ("Category type", pbCategoryType)
                        , ("Category", prCategory prod)
                        ]
                    , activityLocation = effectiveLoc
                    , activityUnit = prUnit prod
                    , exchanges = productExchange : map (scaleExchange allocFraction) sharedExchanges
                    , activityParams = env
                    , activityParamExprs = exprMap
                    }
            in (activity, allFlows, allUnits)

    in map makeActivity pbProducts

-- | Scale an exchange amount by a factor (for allocation)
scaleExchange :: Double -> Exchange -> Exchange
scaleExchange factor ex@TechnosphereExchange{} = ex { techAmount = techAmount ex * factor }
scaleExchange factor ex@BiosphereExchange{} = ex { bioAmount = bioAmount ex * factor }

-- | Convert product row to exchange
productToExchange :: M.Map Text Double -> Bool -> ProductRow -> Exchange
productToExchange env isRef ProductRow{..} =
    let cleanName = fst (extractLocation prName)
        flowUUID = generateFlowUUID cleanName "" prUnit
        unitUUID = generateUnitUUID prUnit
        amount = resolveAmount env prAmountRaw prAmount
    in TechnosphereExchange
        { techFlowId = flowUUID
        , techAmount = amount
        , techUnitId = unitUUID
        , techIsInput = False  -- Products are outputs
        , techIsReference = isRef
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = ""
        }

-- | Convert technosphere row to exchange
techRowToExchange :: M.Map Text Double -> Bool -> TechExchangeRow -> [Exchange]
techRowToExchange env isInput TechExchangeRow{..}
    | resolvedAmount == 0 = []
    | otherwise =
        let (cleanName, location) = extractLocation terName
            flowUUID = generateFlowUUID cleanName "" terUnit
            unitUUID = generateUnitUUID terUnit
        in [TechnosphereExchange
            { techFlowId = flowUUID
            , techAmount = abs resolvedAmount
            , techUnitId = unitUUID
            , techIsInput = isInput
            , techIsReference = False
            , techActivityLinkId = UUID.nil
            , techProcessLinkId = Nothing
            , techLocation = location
            }]
  where
    resolvedAmount = resolveAmount env terAmountRaw terAmount

-- | Convert biosphere row to exchange
bioRowToExchange :: M.Map Text Double -> Bool -> Text -> BioExchangeRow -> Exchange
bioRowToExchange env isInput _compartment BioExchangeRow{..} =
    let flowUUID = generateFlowUUID berName berCompartment berUnit
        unitUUID = generateUnitUUID berUnit
        amount = resolveAmount env berAmountRaw berAmount
    in BiosphereExchange
        { bioFlowId = flowUUID
        , bioAmount = amount
        , bioUnitId = unitUUID
        , bioIsInput = isInput
        , bioLocation = ""
        }

-- | Collect flows from all exchange types
collectFlows :: [ProductRow] -> [TechExchangeRow] -> [BioExchangeRow] -> [Flow]
collectFlows products techs bios =
    let productFlows = map productToFlow products
        techFlows = map techToFlow techs
        bioFlows = map bioToFlow bios
    in productFlows ++ techFlows ++ bioFlows
  where
    productToFlow ProductRow{..} =
        let cleanName = fst (extractLocation prName)
        in Flow
        { flowId = generateFlowUUID cleanName "" prUnit
        , flowName = cleanName
        , flowCategory = prCategory
        , flowSubcompartment = Nothing
        , flowUnitId = generateUnitUUID prUnit
        , flowType = Technosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }
    techToFlow TechExchangeRow{..} =
        let cleanName = fst (extractLocation terName)
        in Flow
        { flowId = generateFlowUUID cleanName "" terUnit
        , flowName = cleanName
        , flowCategory = ""
        , flowSubcompartment = Nothing
        , flowUnitId = generateUnitUUID terUnit
        , flowType = Technosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }
    bioToFlow BioExchangeRow{..} = Flow
        { flowId = generateFlowUUID berName berCompartment berUnit
        , flowName = berName
        , flowCategory = berCompartment
        , flowSubcompartment = Nothing  -- SimaPro uses combined compartment string
        , flowUnitId = generateUnitUUID berUnit
        , flowType = Biosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }

-- | Collect units from all exchange types
collectUnits :: [ProductRow] -> [TechExchangeRow] -> [BioExchangeRow] -> [Unit]
collectUnits products techs bios =
    let allUnits = map prUnit products ++ map terUnit techs ++ map berUnit bios
        uniqueUnits = S.toList $ S.fromList allUnits
    in map toUnit uniqueUnits
  where
    toUnit name = Unit
        { unitId = generateUnitUUID name
        , unitName = name
        , unitSymbol = name
        , unitComment = ""
        }

-- ============================================================================
-- Encoding Conversion
-- ============================================================================

-- | Ensure ByteString is proper UTF-8, converting from Windows-1252 if needed.
-- SimaPro CSV files use Windows-1252 encoding (no encoding header in the format).
-- Handles three cases:
--   1. Already valid UTF-8 with no C1 controls -> pass through
--   2. Valid UTF-8 but contains C1 controls (bad prior conversion) -> fix C1 chars
--   3. Raw Windows-1252 bytes (not valid UTF-8) -> full Win-1252 decode
ensureUtf8 :: BS.ByteString -> BS.ByteString
ensureUtf8 bs = case TE.decodeUtf8' bs of
    Right text
        | T.any isC1Control text -> TE.encodeUtf8 (fixWindows1252Controls text)
        | otherwise -> bs  -- Already proper UTF-8, no changes needed
    Left _ -> TE.encodeUtf8 (decodeWindows1252 bs)
  where
    isC1Control c = c >= '\x0080' && c <= '\x009F'

-- | Decode raw Windows-1252 bytes to Text.
-- decodeLatin1 maps each byte to the same-valued Unicode codepoint,
-- then fixWindows1252Controls corrects the 0x80-0x9F range.
decodeWindows1252 :: BS.ByteString -> Text
decodeWindows1252 = fixWindows1252Controls . TE.decodeLatin1

-- | Map C1 control characters (U+0080-U+009F) to their Windows-1252 equivalents.
-- These control characters never appear in real text; when present, they're
-- always Win-1252 bytes that were incorrectly mapped to Unicode codepoints.
fixWindows1252Controls :: Text -> Text
fixWindows1252Controls = T.map fixChar
  where
    fixChar '\x0080' = '\x20AC'  -- Euro sign
    fixChar '\x0082' = '\x201A'  -- Single low-9 quotation mark
    fixChar '\x0083' = '\x0192'  -- Latin small f with hook
    fixChar '\x0084' = '\x201E'  -- Double low-9 quotation mark
    fixChar '\x0085' = '\x2026'  -- Horizontal ellipsis
    fixChar '\x0086' = '\x2020'  -- Dagger
    fixChar '\x0087' = '\x2021'  -- Double dagger
    fixChar '\x0088' = '\x02C6'  -- Modifier letter circumflex
    fixChar '\x0089' = '\x2030'  -- Per mille sign
    fixChar '\x008A' = '\x0160'  -- Latin capital S with caron
    fixChar '\x008B' = '\x2039'  -- Single left-pointing angle quote
    fixChar '\x008C' = '\x0152'  -- Latin capital ligature OE
    fixChar '\x008E' = '\x017D'  -- Latin capital Z with caron
    fixChar '\x0091' = '\x2018'  -- Left single quotation mark
    fixChar '\x0092' = '\x2019'  -- Right single quotation mark
    fixChar '\x0093' = '\x201C'  -- Left double quotation mark
    fixChar '\x0094' = '\x201D'  -- Right double quotation mark
    fixChar '\x0095' = '\x2022'  -- Bullet
    fixChar '\x0096' = '\x2013'  -- En dash
    fixChar '\x0097' = '\x2014'  -- Em dash
    fixChar '\x0098' = '\x02DC'  -- Small tilde
    fixChar '\x0099' = '\x2122'  -- Trade mark sign
    fixChar '\x009A' = '\x0161'  -- Latin small s with caron
    fixChar '\x009B' = '\x203A'  -- Single right-pointing angle quote
    fixChar '\x009C' = '\x0153'  -- Latin small ligature oe
    fixChar '\x009E' = '\x017E'  -- Latin small z with caron
    fixChar '\x009F' = '\x0178'  -- Latin capital Y with diaeresis
    fixChar c = c

-- ============================================================================
-- Main Parser
-- ============================================================================

-- ============================================================================
-- Parallel Parsing Helpers
-- ============================================================================

-- | Extract SimaProConfig from header lines (lines starting with '{').
-- Stops at the first non-header, non-empty line.
extractConfig :: [BS.ByteString] -> SimaProConfig
extractConfig = foldl' step defaultConfig . takeWhile isHeaderOrEmpty
  where
    isHeaderOrEmpty l = let s = BS8.strip l in BS.null s || BS8.isPrefixOf "{" s
    step cfg line = case parseHeaderLine line of
        Just (key, value) -> updateConfigFromHeader cfg key value
        Nothing           -> cfg

-- | Split lines into End-delimited blocks.
-- Each returned chunk includes all lines up to and including its "End" line.
splitEndBlocks :: [BS.ByteString] -> [[BS.ByteString]]
splitEndBlocks [] = []
splitEndBlocks ls =
    let (block, rest) = break (\l -> BS8.strip l == "End") ls
    in case rest of
        []        -> []   -- no End found, discard trailing lines
        (e:rest') -> (block ++ [e]) : splitEndBlocks rest'

-- | Parse a list of End-delimited line chunks into ProcessBlocks + global params.
parseChunks :: SimaProConfig -> [[BS.ByteString]] -> ([ProcessBlock], [(Text,Text)], [(Text,Text)], [(Text,Text)], [(Text,Text)])
parseChunks cfg chunks =
    let initAcc = ParseAcc
            { paConfig = cfg, paState = BetweenBlocks
            , paCurrentBlock = emptyProcessBlock, paBlocks = []
            , paLineNum = 0
            , paDbInputParams = [], paDbCalcParams = []
            , paProjInputParams = [], paProjCalcParams = []
            }
        finalAcc = foldl' processLine initAcc (concat chunks)
    in ( reverse (paBlocks finalAcc)
       , paDbInputParams finalAcc, paDbCalcParams finalAcc
       , paProjInputParams finalAcc, paProjCalcParams finalAcc )

-- | Distribute items evenly among N workers.
distributeItems :: Int -> [a] -> [[a]]
distributeItems n xs =
    let len = length xs
        baseSize = len `div` n
        remainder = len `mod` n
        sizes = replicate remainder (baseSize + 1) ++ replicate (n - remainder) baseSize
    in go sizes xs
  where
    go [] _ = []
    go _ [] = []
    go (s:ss) items = take s items : go ss (drop s items)

-- ============================================================================
-- Main Entry Point
-- ============================================================================

-- | Parse a SimaPro CSV file
-- Handles Windows-1252/Latin-1 encoding common in SimaPro exports
parseSimaProCSV :: FilePath -> IO ([Activity], FlowDB, UnitDB)
parseSimaProCSV path = do
    reportProgress Info $ "Loading SimaPro CSV file: " ++ path
    startTime <- getCurrentTime

    -- Read as ByteString and convert from Windows-1252 to proper UTF-8.
    rawContent <- BS.readFile path
    let !utf8Content = ensureUtf8 rawContent
        lines' = map stripCR (BS8.lines utf8Content)

    -- Extract config from header (fast, sequential, ~5 lines)
    let cfg = extractConfig lines'

    -- Split all lines into End-delimited blocks and distribute to workers
    let endBlocks = splitEndBlocks lines'
    numWorkers <- getNumCapabilities
    let workerChunks = distributeItems numWorkers endBlocks

    reportProgress Info $ printf "Parsing %d End-blocks with %d parallel workers" (length endBlocks) numWorkers

    -- Parse blocks in parallel — each worker processes its share
    results <- mapConcurrently (evaluate . force . parseChunks cfg) workerChunks
    let allBlocks    = concatMap (\(b,_,_,_,_) -> b) results
        globalParams = ( concatMap (\(_,a,_,_,_) -> a) results
                       , concatMap (\(_,_,b,_,_) -> b) results
                       , concatMap (\(_,_,_,c,_) -> c) results
                       , concatMap (\(_,_,_,_,d) -> d) results )

    -- Convert all blocks to activities (one activity per product) - PARALLEL
    converted <- concat <$> mapConcurrently (evaluate . force . processBlockToActivity globalParams) allBlocks
    let activities = map (\(a,_,_) -> a) converted
        allFlows = concatMap (\(_,f,_) -> f) converted
        allUnits = concatMap (\(_,_,u) -> u) converted

    -- Build deduplicated maps
    let flowDB = M.fromList [(flowId f, f) | f <- allFlows]
        unitDB = M.fromList [(unitId u, u) | u <- allUnits]

    -- Force evaluation before returning
    let !numActivities = length activities
    let !numFlows = M.size flowDB
    let !numUnits = M.size unitDB

    endTime <- getCurrentTime
    let duration = realToFrac (diffUTCTime endTime startTime) :: Double
    reportProgress Info $ printf "SimaPro parsing completed in %.2fs:" duration
    reportProgress Info $ printf "  Activities: %d processes" numActivities
    reportProgress Info $ printf "  Flows: %d unique" numFlows
    reportProgress Info $ printf "  Units: %d unique" numUnits

    return (activities, flowDB, unitDB)
  where
    -- Strip Windows \r from ByteString (fast, often no-op)
    stripCR :: BS.ByteString -> BS.ByteString
    stripCR bs
        | BS.null bs = bs
        | BS8.last bs == '\r' = BS8.init bs
        | otherwise = bs


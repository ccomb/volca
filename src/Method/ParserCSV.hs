{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Parser for CSV-based LCIA method files.
--
-- A single CSV file encodes multiple impact categories as columns:
--
-- > # methodology: CML-IA aug 2016
-- > ;;global warming (GWP100);acidification;...
-- > ;;kg CO2 eq.;kg SO2 eq.;...
-- > substance;compartment;;;...
-- > Carbon dioxide;air;1.0;;...
--
-- One CSV file → multiple 'Method' values (one per column).
module Method.ParserCSV
    ( parseMethodCSV
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID5
import Data.Word (Word8)

import Method.Types

-- | Parse a CSV method file, producing one 'Method' per impact category column.
parseMethodCSV :: FilePath -> IO (Either String [Method])
parseMethodCSV path = do
    bytes <- BS.readFile path
    return $ parseMethodBytes bytes

parseMethodBytes :: BS.ByteString -> Either String [Method]
parseMethodBytes bytes =
    let allLines  = BC.lines bytes
        -- Extract metadata from # comment lines
        (comments, dataLines) = span (\l -> BC.isPrefixOf "#" l || BS.null l) allLines
        methodology = extractMethodology comments
        -- Need at least 3 rows: categories, units, column-labels
        -- (data rows may be empty for a valid but useless file)
    in case dataLines of
        (catRow : unitRow : _labelRow : rows) ->
            let !delim = detectDelimiter catRow
                cats  = drop 2 $ splitRow delim catRow
                units = drop 2 $ splitRow delim unitRow
                -- Build one Method per non-empty category column
            in Right [ mkMethod methodology catName unit colIdx rows delim
                     | (colIdx, catName, unit) <- zip3 [0..] cats units
                     , not (T.null catName)
                     ]
        _ -> Left "CSV method file needs at least 3 header rows (categories, units, column labels)"

-- | Build a single Method from one column of the CSV.
mkMethod :: Maybe Text -> Text -> Text -> Int -> [BS.ByteString] -> Word8 -> Method
mkMethod methodology catName unit colIdx rows delim =
    let !ns       = csvMethodNamespace
        !methodId = UUID5.generateNamed ns (bsKey $ "method:" <> catName)
        !factors  = [ MethodCF
                        { mcfFlowRef   = UUID5.generateNamed ns (bsKey $ sub <> "::" <> comp)
                        , mcfFlowName  = sub
                        , mcfDirection = directionFromCompartment comp
                        , mcfValue     = v
                        }
                    | row <- rows
                    , let cells = splitRow delim row
                    , length cells > colIdx + 2
                    , let !sub  = T.strip (cells !! 0)
                          !comp = T.strip (cells !! 1)
                          !raw  = T.strip (cells !! (colIdx + 2))
                    , not (T.null sub)
                    , not (T.null raw)
                    , Just v <- [parseDouble raw]
                    ]
    in Method
        { methodId          = methodId
        , methodName        = catName
        , methodDescription = Nothing
        , methodUnit        = if T.null unit then "unknown" else unit
        , methodCategory    = catName
        , methodMethodology = methodology
        , methodFactors     = factors
        }

-- | Deterministic namespace for CSV-derived UUIDs.
csvMethodNamespace :: UUID
csvMethodNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "fplca:csv-method")

-- | Direction: "resources" → Input, everything else → Output.
directionFromCompartment :: Text -> FlowDirection
directionFromCompartment comp
    | T.toCaseFold comp == "resources" = Input
    | otherwise                        = Output

-- | Auto-detect delimiter: semicolon if the line contains ';', else comma.
detectDelimiter :: BS.ByteString -> Word8
detectDelimiter line
    | BC.elem ';' line = 0x3B  -- ';'
    | otherwise        = 0x2C  -- ','

-- | Split a row on the delimiter, decoding to Text.
splitRow :: Word8 -> BS.ByteString -> [Text]
splitRow delim = map (TE.decodeUtf8With TEE.lenientDecode) . BS.split delim

-- | Extract methodology from "# methodology: ..." comment.
extractMethodology :: [BS.ByteString] -> Maybe Text
extractMethodology = go
  where
    go [] = Nothing
    go (l:ls)
        | BC.isPrefixOf "# methodology:" l =
            Just . T.strip . TE.decodeUtf8With TEE.lenientDecode $ BS.drop 15 l
        | otherwise = go ls

-- | Strict double parse, Nothing on failure.
parseDouble :: Text -> Maybe Double
parseDouble t = case TR.double t of
    Right (v, _) -> Just v
    Left _       -> Nothing

-- | Convert Text to bytes for UUID5 key generation.
bsKey :: Text -> [Word8]
bsKey = BS.unpack . TE.encodeUtf8

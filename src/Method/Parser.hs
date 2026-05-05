{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for ILCD LCIA Method XML files using Xeno SAX parser.

Parses the Environmental Footprint LCIA method format to extract
characterization factors for impact assessment.
-}
module Method.Parser (
    parseMethodFile,
    parseMethodBytes,
    parseMethodFileWithFlows,
    parseMethodBytesWithFlows,
    extractFlowName,
    extractCompartmentFromDesc,
) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Xeno.SAX as X

import EcoSpold.Common (bsToText, isElement)
import Method.FlowResolver (ILCDFlowInfo (..))
import Method.Types

-- | Parse an ILCD LCIA Method XML file
parseMethodFile :: FilePath -> IO (Either String Method)
parseMethodFile path = parseMethodFileWithFlows M.empty path

-- | Parse with ILCD flow info enrichment
parseMethodFileWithFlows :: M.Map UUID ILCDFlowInfo -> FilePath -> IO (Either String Method)
parseMethodFileWithFlows flowInfo path = do
    bytes <- BS.readFile path
    return $ parseMethodBytesWithFlows flowInfo bytes

-- | Parse ILCD LCIA Method XML from ByteString
parseMethodBytes :: BS.ByteString -> Either String Method
parseMethodBytes = parseMethodBytesWithFlows M.empty

-- | Parse with ILCD flow info enrichment
parseMethodBytesWithFlows :: M.Map UUID ILCDFlowInfo -> BS.ByteString -> Either String Method
parseMethodBytesWithFlows flowInfo bytes =
    case X.fold openTag attribute endOpen textHandler closeTag cdataHandler initialState bytes of
        Left err -> Left $ "XML parse error: " ++ show err
        Right state -> buildMethod flowInfo state

-- | Parser state accumulator
data ParseState = ParseState
    { psMethodId :: !Text
    , psMethodName :: !Text
    , psDescription :: !Text
    , psUnit :: !Text
    , psCategory :: !Text
    , psMethodology :: !Text
    , psFactors :: ![MethodCF]
    , -- Current factor being parsed
      psCurrentFlowId :: !Text
    , psCurrentFlowURI :: !Text
    , psCurrentFlowName :: !Text
    , psCurrentDirection :: !Text
    , psCurrentValue :: !Text
    , -- Context tracking
      psPath :: ![BS.ByteString] -- Stack of element names
    , psInFactor :: !Bool
    , psInRefQuantity :: !Bool -- Inside referenceQuantity element
    , psTextAccum :: ![BS.ByteString] -- Accumulated text content
    }

initialState :: ParseState
initialState =
    ParseState
        { psMethodId = ""
        , psMethodName = ""
        , psDescription = ""
        , psUnit = ""
        , psCategory = ""
        , psMethodology = ""
        , psFactors = []
        , psCurrentFlowId = ""
        , psCurrentFlowURI = ""
        , psCurrentFlowName = ""
        , psCurrentDirection = ""
        , psCurrentValue = ""
        , psPath = []
        , psInFactor = False
        , psInRefQuantity = False
        , psTextAccum = []
        }

-- | Handle opening tags - update path and context
openTag :: ParseState -> BS.ByteString -> ParseState
openTag state tagName =
    let newPath = tagName : psPath state
        newInFactor = psInFactor state || isElement tagName "factor"
        newInRefQuantity = psInRefQuantity state || isElement tagName "referenceQuantity"
     in state
            { psPath = newPath
            , psInFactor = newInFactor
            , psInRefQuantity = newInRefQuantity
            , psTextAccum = []
            }

-- | Handle attributes
attribute :: ParseState -> BS.ByteString -> BS.ByteString -> ParseState
attribute state attrName attrValue
    -- Capture flow UUID from referenceToFlowDataSet
    | psInFactor state && attrName == "refObjectId" && inFlowRef =
        state{psCurrentFlowId = bsToText attrValue}
    -- Capture URI as secondary UUID source
    | psInFactor state && attrName == "uri" && inFlowRef =
        state{psCurrentFlowURI = bsToText attrValue}
    | otherwise = state
  where
    inFlowRef = case psPath state of
        (x : _) -> isElement x "referenceToFlowDataSet"
        _ -> False

-- | Handle end of opening tag (after all attributes processed)
endOpen :: ParseState -> BS.ByteString -> ParseState
endOpen state _tagName = state

-- | Handle text content
textHandler :: ParseState -> BS.ByteString -> ParseState
textHandler state text =
    state{psTextAccum = text : psTextAccum state}

-- | Handle CDATA sections (treat same as text)
cdataHandler :: ParseState -> BS.ByteString -> ParseState
cdataHandler = textHandler

-- | Handle closing tags
closeTag :: ParseState -> BS.ByteString -> ParseState
closeTag state tagName
    -- End of factor element - build the CF (enrichment happens in buildMethod)
    | isElement tagName "factor" && psInFactor state =
        let flowUUID = resolveFlowUUID (psCurrentFlowId state) (psCurrentFlowURI state)
            cf =
                MethodCF
                    { mcfFlowRef = flowUUID
                    , mcfFlowName = extractFlowName (psCurrentFlowName state)
                    , mcfDirection = parseDirection (psCurrentDirection state)
                    , mcfValue = parseDoubleSafe (psCurrentValue state)
                    , mcfCompartment = extractCompartmentFromDesc (psCurrentFlowName state)
                    , mcfCAS = Nothing -- enriched in buildMethod from flow XMLs
                    , mcfUnit = psUnit state
                    , mcfConsumerLocation = Nothing
                    }
         in state
                { psPath = dropPath
                , psInFactor = False
                , psFactors = cf : psFactors state
                , psCurrentFlowId = ""
                , psCurrentFlowURI = ""
                , psCurrentFlowName = ""
                , psCurrentDirection = ""
                , psCurrentValue = ""
                , psTextAccum = []
                }
    -- UUID element - capture method UUID (only the first one, not flow UUIDs)
    | isElement tagName "UUID" && T.null (psMethodId state) && not (psInFactor state) =
        state
            { psPath = dropPath
            , psMethodId = accumulatedText
            , psTextAccum = []
            }
    -- Method name (only first one at top level)
    | isElement tagName "name" && T.null (psMethodName state) && not (psInFactor state) =
        state
            { psPath = dropPath
            , psMethodName = accumulatedText
            , psTextAccum = []
            }
    -- General comment (description)
    | isElement tagName "generalComment" =
        state
            { psPath = dropPath
            , psDescription = accumulatedText
            , psTextAccum = []
            }
    -- Impact category
    | isElement tagName "impactCategory" =
        state
            { psPath = dropPath
            , psCategory = accumulatedText
            , psTextAccum = []
            }
    -- Methodology
    | isElement tagName "methodology" =
        state
            { psPath = dropPath
            , psMethodology = accumulatedText
            , psTextAccum = []
            }
    -- Short description - could be unit or flow name depending on context
    | isElement tagName "shortDescription" =
        if psInFactor state
            then
                state
                    { psPath = dropPath
                    , psCurrentFlowName = accumulatedText
                    , psTextAccum = []
                    }
            -- Only capture unit when inside referenceQuantity element
            else
                if psInRefQuantity state && T.null (psUnit state)
                    then
                        state
                            { psPath = dropPath
                            , psUnit = accumulatedText
                            , psTextAccum = []
                            }
                    else state{psPath = dropPath, psTextAccum = []}
    -- End of referenceQuantity element
    | isElement tagName "referenceQuantity" =
        state
            { psPath = dropPath
            , psInRefQuantity = False
            , psTextAccum = []
            }
    -- Exchange direction (Input/Output)
    | isElement tagName "exchangeDirection" =
        state
            { psPath = dropPath
            , psCurrentDirection = accumulatedText
            , psTextAccum = []
            }
    -- Mean value (CF value)
    | isElement tagName "meanValue" =
        state
            { psPath = dropPath
            , psCurrentValue = accumulatedText
            , psTextAccum = []
            }
    -- Default: just pop the path
    | otherwise =
        state{psPath = dropPath, psTextAccum = []}
  where
    dropPath = case psPath state of
        (_ : rest) -> rest
        [] -> []
    accumulatedText = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)

-- | Build the final Method from parsed state, enriching CFs with flow XML data
buildMethod :: M.Map UUID ILCDFlowInfo -> ParseState -> Either String Method
buildMethod flowInfo state = do
    when (T.null (psMethodId state)) $
        Left "Missing method UUID"
    when (T.null (psMethodName state)) $
        Left "Missing method name"

    methodUUID <- case UUID.fromText (psMethodId state) of
        Just u -> Right u
        Nothing -> Left $ "Invalid method UUID: " ++ T.unpack (psMethodId state)

    let factors = reverse (psFactors state)
        enrichedFactors = map (enrichCF flowInfo) factors

    Right $
        Method
            { methodId = methodUUID
            , methodName = psMethodName state
            , methodDescription =
                if T.null (psDescription state)
                    then Nothing
                    else Just (psDescription state)
            , methodUnit = if T.null (psUnit state) then "unknown" else psUnit state
            , methodCategory = if T.null (psCategory state) then "unknown" else psCategory state
            , methodMethodology =
                if T.null (psMethodology state)
                    then Nothing
                    else Just (psMethodology state)
            , methodFactors = enrichedFactors
            }

-- | Enrich a MethodCF with data from ILCD flow XMLs
enrichCF :: M.Map UUID ILCDFlowInfo -> MethodCF -> MethodCF
enrichCF flowInfo cf = case M.lookup (mcfFlowRef cf) flowInfo of
    Nothing -> cf -- no flow XML found, keep fallback data from shortDescription
    Just info ->
        cf
            { mcfFlowName = ilcdBaseName info -- proper baseName replaces extracted name
            , mcfCompartment = firstJust (ilcdCompartment info) (mcfCompartment cf)
            , mcfCAS = ilcdCAS info
            }
  where
    firstJust (Just a) _ = Just a
    firstJust Nothing b = b

-- | Resolve flow UUID: prefer refObjectId, fall back to extracting UUID from URI path
resolveFlowUUID :: Text -> Text -> UUID
resolveFlowUUID refId uri
    | not (T.null refId), Just u <- UUID.fromText refId = u
    | not (T.null uri) = extractUUIDFromURI uri
    | otherwise = UUID.nil

-- | Extract UUID from a URI like "../flows/08a91e70-3ddc-11dd-a2a8-0050c2490048.xml"
extractUUIDFromURI :: Text -> UUID
extractUUIDFromURI uri =
    let filename = lastItem $ T.splitOn "/" uri
        stem = fromMaybe filename $ T.stripSuffix ".xml" filename
     in fromMaybe UUID.nil (UUID.fromText stem)
  where
    lastItem [] = ""
    lastItem xs = Prelude.last xs

-- | Parse Double from text, return 0 on failure
parseDoubleSafe :: Text -> Double
parseDoubleSafe txt = case TR.double txt of
    Right (val, _) -> val
    Left _ -> 0.0

-- | Parse flow direction from text
parseDirection :: Text -> FlowDirection
parseDirection txt
    | "Input" `T.isPrefixOf` txt = Input
    | "input" `T.isPrefixOf` txt = Input
    | otherwise = Output

{- | Extract flow name from shortDescription
Format: "methane (fossil) (Mass, kg, Emissions to ...)"
The flow name may contain parentheses (e.g. "methane (fossil)"),
so we find the "(Mass" / "(Volume" marker as the boundary.
-}
extractFlowName :: Text -> Text
extractFlowName txt =
    let markers = ["(Mass", "(Volume", "(Area", "(Energy", "(Length", "(Number of items"]
        findMarker [] = Nothing
        findMarker (m : ms) = case T.breakOn m txt of
            (before, rest) | not (T.null rest) -> Just before
            _ -> findMarker ms
     in T.strip $ case findMarker markers of
            Just before -> before
            Nothing -> txt -- no marker found: use full text

{- | Extract compartment info from shortDescription as fallback
Format: "... Emissions to air, non-urban ..."
-}
extractCompartmentFromDesc :: Text -> Maybe Compartment
extractCompartmentFromDesc txt
    | "Emissions to air" `T.isInfixOf` txt =
        Just (Compartment "air" (extractSubFromDesc "Emissions to air" txt) "")
    | "Emissions to water" `T.isInfixOf` txt =
        Just (Compartment "water" (extractSubFromDesc "Emissions to water" txt) "")
    | "Emissions to soil" `T.isInfixOf` txt =
        Just (Compartment "soil" (extractSubFromDesc "Emissions to soil" txt) "")
    | "Resources" `T.isInfixOf` txt =
        Just (Compartment "natural resource" "" "")
    | otherwise = Nothing
  where
    extractSubFromDesc prefix t =
        let (_, after) = T.breakOn prefix t
            rest = T.drop (T.length prefix) after
            -- Rest might be ", non-urban air or from high stacks)"
            cleaned = T.dropWhile (\c -> c == ',' || c == ' ') rest
            sub = T.takeWhile (/= ')') cleaned
         in T.strip sub

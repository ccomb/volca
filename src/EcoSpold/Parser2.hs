{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile, normalizeCAS) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (bsToDouble, bsToInt, bsToText, isElement)
import Progress (ProgressLevel (..), reportProgress)
import System.FilePath (takeBaseName)
import Types
import qualified Xeno.SAX as X

{- | Normalize CAS number by stripping leading zeros from first segment.
Ecoinvent zero-pads: "001309-36-0" → "1309-36-0". ILCD uses canonical form.
-}
normalizeCAS :: Text -> Text
normalizeCAS cas = case T.splitOn "-" cas of
    [a, b, c] ->
        let a' = T.dropWhile (== '0') a
         in (if T.null a' then "0" else a') <> "-" <> b <> "-" <> c
    _ -> T.strip cas

{- | Namespace UUID for generating deterministic UUIDs from invalid text
Using UUID v5 (SHA1-based) with a custom namespace for test data compatibility
-}
testDataNamespace :: UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "acvengine.test")

{- | Helper to safely parse UUID from Text, generating deterministic UUID for invalid formats
This ensures test data with invalid UUIDs like "productX-uuid" get unique UUIDs
Returns (UUID, Maybe warning) to avoid unsafePerformIO in pure context
-}
parseUUID :: Text -> (UUID, Maybe String)
parseUUID txt = case UUID.fromText txt of
    Just uuid -> (uuid, Nothing)
    Nothing ->
        -- Generate deterministic UUID from the text using UUID v5
        -- This prevents deduplication issues where all invalid UUIDs would map to nil
        let generatedUUID = UUID5.generateNamed testDataNamespace (BS.unpack $ TE.encodeUtf8 txt)
            -- Only warn for non-empty invalid UUIDs (empty is expected for optional fields)
            warning =
                if T.null txt
                    then Nothing
                    else Just $ "Invalid UUID format: " ++ T.unpack txt ++ " - generated UUID: " ++ show generatedUUID
         in (generatedUUID, warning)

{- | Parse ProcessId from filename (no Database needed here)
Expects format: activity_uuid_product_uuid.spold
-}
parseProcessId :: Text -> Maybe ProcessId
parseProcessId filename = case T.splitOn "_" filename of
    [_, _]
        | not (T.null filename) ->
            -- During parsing we don't have ProcessId yet, just return a placeholder
            -- The actual ProcessId will be assigned during database construction
            Just 0 -- Temporary ProcessId, will be replaced during DB construction
    _ -> Nothing

-- ============================================================================
-- Xeno SAX Parser Implementation (8-15x faster than xml-conduit)
-- ============================================================================

-- | Element context tracker - what element are we currently parsing?
data ElementContext
    = InActivityName
    | InGeographyShortname
    | InIntermediateExchange !IntermediateData
    | InElementaryExchange !ElementaryData
    | InGeneralCommentText !Int -- Track index
    | Other
    deriving (Eq)

-- | Intermediate exchange accumulator
data IntermediateData = IntermediateData
    { idFlowId :: !Text
    , idAmount :: !Double
    , idUnitId :: !Text
    , idFlowName :: !Text
    , idUnitName :: !Text
    , idInputGroup :: !Text
    , idOutputGroup :: !Text
    , idActivityLinkId :: !Text
    , idSynonyms :: !(M.Map Text (S.Set Text))
    }
    deriving (Eq)

-- | Elementary exchange accumulator
data ElementaryData = ElementaryData
    { edFlowId :: !Text
    , edAmount :: !Double
    , edUnitId :: !Text
    , edFlowName :: !Text
    , edUnitName :: !Text
    , edInputGroup :: !Text
    , edOutputGroup :: !Text
    , edCompartments :: ![Text]
    , edSubcompartments :: ![Text]
    , edSynonyms :: !(M.Map Text (S.Set Text))
    , edCAS :: !(Maybe Text)
    }
    deriving (Eq)

-- | Parsing state accumulator for SAX parsing
data ParseState = ParseState
    { psActivityName :: !(Maybe Text)
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![Exchange]
    , psFlows :: ![Flow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString] -- Element path stack
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString] -- Accumulated text content
    , psPendingInputGroup :: !Text -- Pending inputGroup value from child element
    , psPendingOutputGroup :: !Text -- Pending outputGroup value from child element
    , psWarnings :: ![String] -- Accumulated warnings (emitted in IO after fold)
    , psClassifications :: !(M.Map Text Text) -- Classification system -> value
    , psPendingClassSystem :: !Text -- Current classification system name
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState =
    ParseState
        { psActivityName = Nothing
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psFlows = []
        , psUnits = []
        , psPath = []
        , psContext = Other
        , psTextAccum = []
        , psPendingInputGroup = ""
        , psPendingOutputGroup = ""
        , psWarnings = []
        , psClassifications = M.empty
        , psPendingClassSystem = ""
        }

-- | Xeno SAX parser implementation
parseWithXeno :: BS.ByteString -> ProcessId -> Either String ((Activity, [Flow], [Unit]), [String])
parseWithXeno xmlContent processId =
    case X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent of
        Left err -> Left (show err)
        Right finalState -> case buildResult finalState processId of
            Left err -> Left err
            Right result -> Right (result, reverse $ psWarnings finalState)
  where
    -- Open tag handler - update path and context
    openTag state tagName =
        let newPath = tagName : psPath state
            cleanState =
                if isElement tagName "intermediateExchange" || isElement tagName "elementaryExchange"
                    then state{psPendingInputGroup = "", psPendingOutputGroup = ""}
                    else state
            newContext
                | isElement tagName "activityName" = InActivityName
                | isElement tagName "shortname" && any (isElement "geography") (psPath cleanState) = InGeographyShortname
                | isElement tagName "intermediateExchange" =
                    InIntermediateExchange (IntermediateData "" 0.0 "" "" "" "" "" "" M.empty)
                | isElement tagName "elementaryExchange" =
                    InElementaryExchange (ElementaryData "" 0.0 "" "" "" "" "" [] [] M.empty Nothing)
                | isElement tagName "text" && any (isElement "generalComment") (psPath cleanState) = InGeneralCommentText 0
                -- Classification elements: don't switch context. Handled via psTextAccum + psPendingClassSystem.
                -- Switching context here would destroy InIntermediateExchange when classifications appear inside exchanges.
                -- DON'T switch context for child elements (synonym, compartment, etc) - keep parent exchange context
                | otherwise = psContext cleanState
         in cleanState{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler - extract attributes
    attribute state name value =
        let isInsideProperty = case psPath state of
                [] -> False
                (current : _) -> isElement current "property"
         in case psContext state of
                InIntermediateExchange idata ->
                    let updated
                            | isElement name "intermediateExchangeId" = idata{idFlowId = bsToText value}
                            | isElement name "amount" && not isInsideProperty = idata{idAmount = bsToDouble value}
                            | isElement name "unitId" && not isInsideProperty = idata{idUnitId = bsToText value}
                            | isElement name "inputGroup" = idata{idInputGroup = bsToText value}
                            | isElement name "outputGroup" = idata{idOutputGroup = bsToText value}
                            | isElement name "activityLinkId" = idata{idActivityLinkId = bsToText value}
                            | otherwise = idata
                     in state{psContext = InIntermediateExchange updated}
                InElementaryExchange edata ->
                    let updated
                            | isElement name "elementaryExchangeId" = edata{edFlowId = bsToText value}
                            | isElement name "amount" && not isInsideProperty = edata{edAmount = bsToDouble value}
                            | isElement name "unitId" && not isInsideProperty = edata{edUnitId = bsToText value}
                            | isElement name "inputGroup" = edata{edInputGroup = bsToText value}
                            | isElement name "outputGroup" = edata{edOutputGroup = bsToText value}
                            | isElement name "casNumber" = edata{edCAS = Just (normalizeCAS (bsToText value))}
                            | otherwise = edata
                     in state{psContext = InElementaryExchange updated}
                InGeneralCommentText _ ->
                    let idx = if isElement name "index" then bsToInt value else 0
                     in state{psContext = InGeneralCommentText idx}
                _ -> state

    -- End of opening tag - no action needed for SAX
    endOpen state _tagName = state

    -- Text content handler - accumulate text
    text state content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content -- Trim spaces
         in if BS.null trimmed
                then state
                else state{psTextAccum = trimmed : psTextAccum state}

    -- Close tag handler - finalize elements
    closeTag state tagName
        | isElement tagName "activityName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
             in state{psActivityName = Just txt, psContext = Other, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "shortname" && psContext state == InGeographyShortname =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
             in state{psLocation = Just txt, psContext = Other, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "intermediateExchange" =
            case psContext state of
                InIntermediateExchange idata ->
                    -- Use pending group values if attribute values are empty
                    let finalInputGroup = if T.null (idInputGroup idata) then psPendingInputGroup state else idInputGroup idata
                        finalOutputGroup = if T.null (idOutputGroup idata) then psPendingOutputGroup state else idOutputGroup idata
                        isInput = not $ T.null finalInputGroup
                        isOutput = T.null finalInputGroup
                        -- Reference flow identification:
                        -- Reference products are identified ONLY by outputGroup="0"
                        -- This works for both normal production (positive amount) and waste treatment (negative amount)
                        -- Negative inputs (like wastewater discharge) should NOT be considered reference products
                        -- outputGroup valid values: 0=reference product, 1-3=byproducts, 4=allocated byproduct, 5=recyclable
                        isReferenceProduct = isOutput && finalOutputGroup == "0"
                        -- Parse UUIDs and collect warnings
                        (flowUUID, flowWarn) = parseUUID (idFlowId idata)
                        (unitUUID, unitWarn) = parseUUID (idUnitId idata)
                        (linkUUID, linkWarn) =
                            if T.null (idActivityLinkId idata)
                                then (UUID.nil, Nothing)
                                else parseUUID (idActivityLinkId idata)
                        uuidWarnings = [w | Just w <- [flowWarn, unitWarn, linkWarn]]
                        exchange =
                            TechnosphereExchange
                                flowUUID
                                (idAmount idata)
                                unitUUID
                                isInput
                                isReferenceProduct
                                linkUUID
                                Nothing
                                "" -- EcoSpold2: no per-exchange location
                        flow =
                            Flow
                                flowUUID
                                (if T.null (idFlowName idata) then idFlowId idata else idFlowName idata)
                                "technosphere"
                                Nothing -- subcompartment
                                unitUUID
                                Technosphere
                                (idSynonyms idata)
                                Nothing -- CAS
                                Nothing -- substanceId
                        unitNameWarning =
                            if T.null (idUnitName idata)
                                then
                                    [ "[WARNING] Missing unit name for intermediate exchange with flow ID: "
                                        ++ T.unpack (idFlowId idata)
                                        ++ " - using 'UNKNOWN_UNIT' placeholder"
                                    ]
                                else []
                        unit =
                            Unit
                                unitUUID
                                (if T.null (idUnitName idata) then "UNKNOWN_UNIT" else idUnitName idata)
                                (if T.null (idUnitName idata) then "?" else idUnitName idata)
                                ""
                        -- Set reference unit if this is the reference product
                        newRefUnit =
                            if isReferenceProduct && not (T.null (idUnitName idata))
                                then Just (idUnitName idata)
                                else psRefUnit state
                     in state
                            { psExchanges = exchange : psExchanges state
                            , psFlows = flow : psFlows state
                            , psUnits = unit : psUnits state
                            , psContext = Other
                            , psPath = tail (psPath state)
                            , psTextAccum = []
                            , psPendingInputGroup = ""
                            , psPendingOutputGroup = ""
                            , psRefUnit = newRefUnit
                            , psWarnings = uuidWarnings ++ unitNameWarning ++ psWarnings state
                            }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "elementaryExchange" =
            case psContext state of
                InElementaryExchange edata ->
                    -- Use pending group values if attribute values are empty
                    let finalInputGroup = if T.null (edInputGroup edata) then psPendingInputGroup state else edInputGroup edata
                        finalOutputGroup = if T.null (edOutputGroup edata) then psPendingOutputGroup state else edOutputGroup edata
                        category = case (edCompartments edata, edSubcompartments edata) of
                            ([], []) -> "unspecified"
                            (comp : _, []) -> comp
                            ([], sub : _) -> sub
                            (comp : _, sub : _) -> comp <> "/" <> sub
                        -- Determine if exchange is input (resource extraction)
                        -- Primary: use inputGroup/outputGroup if present
                        -- Fallback: use compartment heuristic (natural resource = input, others = output)
                        isInput =
                            if not (T.null finalInputGroup)
                                then True -- Has explicit inputGroup
                                else
                                    if not (T.null finalOutputGroup)
                                        then False -- Has explicit outputGroup
                                        else -- Fallback to compartment-based heuristic
                                            case edCompartments edata of
                                                (comp : _) | T.toLower comp == "natural resource" -> True
                                                _ -> False -- Default to output (emissions)
                                                -- Parse UUIDs and collect warnings
                        (flowUUID, flowWarn) = parseUUID (edFlowId edata)
                        (unitUUID, unitWarn) = parseUUID (edUnitId edata)
                        uuidWarnings = [w | Just w <- [flowWarn, unitWarn]]
                        exchange =
                            BiosphereExchange
                                flowUUID
                                (edAmount edata)
                                unitUUID
                                isInput
                                "" -- EcoSpold2: no per-exchange location
                                -- Get subcompartment from the list (first entry if any)
                        subcompartment = case edSubcompartments edata of
                            (s : _) | not (T.null s) -> Just s
                            _ -> Nothing
                        flow =
                            Flow
                                flowUUID
                                (if T.null (edFlowName edata) then edFlowId edata else edFlowName edata)
                                category
                                subcompartment
                                unitUUID
                                Biosphere
                                (edSynonyms edata)
                                (edCAS edata)
                                Nothing -- substanceId - to be filled later
                        unitNameWarning =
                            if T.null (edUnitName edata)
                                then
                                    [ "[WARNING] Missing unit name for elementary exchange with flow ID: "
                                        ++ T.unpack (edFlowId edata)
                                        ++ " - using 'UNKNOWN_UNIT' placeholder"
                                    ]
                                else []
                        unit =
                            Unit
                                unitUUID
                                (if T.null (edUnitName edata) then "UNKNOWN_UNIT" else edUnitName edata)
                                (if T.null (edUnitName edata) then "?" else edUnitName edata)
                                ""
                     in state
                            { psExchanges = exchange : psExchanges state
                            , psFlows = flow : psFlows state
                            , psUnits = unit : psUnits state
                            , psContext = Other
                            , psPath = tail (psPath state)
                            , psTextAccum = []
                            , psPendingInputGroup = ""
                            , psPendingOutputGroup = ""
                            , psWarnings = uuidWarnings ++ unitNameWarning ++ psWarnings state
                            }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "text" =
            case psContext state of
                InGeneralCommentText _idx ->
                    let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                     in -- Store as (index, text) pair for later sorting
                        if T.null txt
                            then state{psContext = Other, psTextAccum = []}
                            else state{psDescription = txt : psDescription state, psContext = Other, psTextAccum = []}
                _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "name" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                isInsideProperty = case psPath state of
                    (_ : parent : _) -> isElement parent "property"
                    _ -> False
             in case psContext state of
                    InIntermediateExchange idata
                        | not isInsideProperty ->
                            state{psContext = InIntermediateExchange idata{idFlowName = txt}, psPath = tail (psPath state), psTextAccum = []}
                    InElementaryExchange edata
                        | not isInsideProperty ->
                            state{psContext = InElementaryExchange edata{edFlowName = txt}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "unitName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                isInsideProperty = case psPath state of
                    (_ : parent : _) -> isElement parent "property"
                    _ -> False
             in case psContext state of
                    InIntermediateExchange idata
                        | not isInsideProperty ->
                            state{psContext = InIntermediateExchange idata{idUnitName = txt}, psPath = tail (psPath state), psTextAccum = []}
                    InElementaryExchange edata
                        | not isInsideProperty ->
                            state{psContext = InElementaryExchange edata{edUnitName = txt}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "synonym" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InIntermediateExchange idata
                        | not (T.null txt) ->
                            let syns = M.insertWith S.union "en" (S.singleton txt) (idSynonyms idata)
                             in state{psContext = InIntermediateExchange idata{idSynonyms = syns}, psPath = tail (psPath state), psTextAccum = []}
                    InElementaryExchange edata
                        | not (T.null txt) ->
                            let syns = M.insertWith S.union "en" (S.singleton txt) (edSynonyms edata)
                             in state{psContext = InElementaryExchange edata{edSynonyms = syns}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "inputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in -- DON'T change psContext - preserve the parent exchange context
                state{psPendingInputGroup = txt, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "outputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in -- DON'T change psContext - preserve the parent exchange context
                state{psPendingOutputGroup = txt, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "compartment" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InElementaryExchange edata
                        | not (T.null txt) ->
                            state{psContext = InElementaryExchange edata{edCompartments = txt : edCompartments edata}, psPath = tail (psPath state), psTextAccum = []}
                    _ ->
                        state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "subcompartment" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InElementaryExchange edata
                        | not (T.null txt) ->
                            state{psContext = InElementaryExchange edata{edSubcompartments = txt : edSubcompartments edata}, psPath = tail (psPath state), psTextAccum = []}
                    _ ->
                        state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "classificationSystem" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in state{psPendingClassSystem = txt, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "classificationValue" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
                sys = psPendingClassSystem state
             in state
                    { psClassifications =
                        if T.null sys || T.null txt
                            then psClassifications state
                            else M.insert sys txt (psClassifications state)
                    , psPath = tail (psPath state)
                    , psTextAccum = []
                    }
        | otherwise =
            state{psPath = if null (psPath state) then [] else tail (psPath state)}

    -- CDATA handler - treat as text
    cdata state content = text state content

    -- Build final result from parse state
    buildResult :: ParseState -> ProcessId -> Either String (Activity, [Flow], [Unit])
    buildResult st _pid =
        let name = case psActivityName st of
                Just n -> n
                Nothing -> "Unknown Activity"
            location = case psLocation st of
                Just loc -> loc
                Nothing -> "GLO"
            description = reverse (psDescription st) -- Reverse to get correct order
            refUnit = case psRefUnit st of
                Just u -> u
                Nothing -> "UNKNOWN_UNIT"
            -- Apply cutoff strategy to exchanges
            activity = Activity name description M.empty (psClassifications st) location refUnit (reverse $ psExchanges st) M.empty M.empty
            flows = reverse (psFlows st)
            units = reverse (psUnits st)
         in case applyCutoffStrategy activity of
                Right act -> Right (act, flows, units)
                Left err -> Left err

-- | Parse EcoSpold file using Xeno SAX parser
streamParseActivityAndFlowsFromFile :: FilePath -> IO (Either String (Activity, [Flow], [Unit]))
streamParseActivityAndFlowsFromFile path = do
    !xmlContent <- BS.readFile path
    let filenameBase = T.pack $ takeBaseName path
    case EcoSpold.Parser2.parseProcessId filenameBase of
        Nothing -> return $ Left $ "Invalid filename format for ProcessId: " ++ path
        Just pid -> case parseWithXeno xmlContent pid of
            Left err -> return $ Left err
            Right (result, warnings) -> do
                mapM_ (reportProgress Warning) warnings
                return $ Right result

{- | Apply cut-off strategy
1. Remove zero-amount production exchanges (co-products)
2. Assign single non-zero product as reference product
3. Ensure single-output process structure
4. VALIDATION: Fail if no reference product can be established
-}
applyCutoffStrategy :: Activity -> Either String Activity
applyCutoffStrategy activity =
    let filteredExchanges = removeZeroAmountCoproducts (exchanges activity)
        updatedActivity = activity{exchanges = filteredExchanges}
        finalActivity =
            if hasReferenceProduct updatedActivity
                then updatedActivity
                else assignSingleProductAsReference updatedActivity
     in if hasReferenceProduct finalActivity
            then Right finalActivity
            else Left $ "Activity has no reference product: " ++ T.unpack (activityName activity)

-- | Check if activity has any reference product
hasReferenceProduct :: Activity -> Bool
hasReferenceProduct activity = any exchangeIsReference (exchanges activity)

-- | Remove production exchanges with zero amounts
removeZeroAmountCoproducts :: [Exchange] -> [Exchange]
removeZeroAmountCoproducts exs = filter keepExchange exs
  where
    keepExchange (TechnosphereExchange _ _ _ False True _ _ _) = True
    keepExchange (TechnosphereExchange _ amount _ False False _ _ _) = amount /= 0.0
    keepExchange (TechnosphereExchange _ _ _ True _ _ _ _) = True
    keepExchange (BiosphereExchange _ _ _ _ _) = True

-- | Assign single product as reference product
assignSingleProductAsReference :: Activity -> Activity
assignSingleProductAsReference activity =
    let productionExchanges = [ex | ex <- exchanges activity, isProductionExchange ex]
        nonZeroProduction = [ex | ex <- productionExchanges, exchangeAmount ex /= 0.0]
     in case nonZeroProduction of
            [singleProduct] ->
                -- Update the single product to be reference product
                let updatedExchanges = map (updateReferenceProduct singleProduct) (exchanges activity)
                 in activity{exchanges = updatedExchanges}
            [] -> activity -- No production exchanges, leave as-is
            _ -> activity -- Multiple production exchanges, leave as-is (shouldn't happen after cutoff)

-- | Check if exchange is production exchange (output, non-reference)
isProductionExchange :: Exchange -> Bool
isProductionExchange (TechnosphereExchange _ _ _ False _ _ _ _) = True -- Output technosphere = production
isProductionExchange _ = False

-- | Update reference product flag for the specified exchange
updateReferenceProduct :: Exchange -> Exchange -> Exchange
updateReferenceProduct target current
    | exchangeFlowId target == exchangeFlowId current = markAsReference current
    | otherwise = unmarkAsReference current

-- | Mark exchange as reference product
markAsReference :: Exchange -> Exchange
markAsReference (TechnosphereExchange fid amt uid isInp _ linkId procLink loc) =
    TechnosphereExchange fid amt uid isInp True linkId procLink loc
markAsReference ex = ex -- No change for biosphere exchanges

-- | Unmark exchange as reference product
unmarkAsReference :: Exchange -> Exchange
unmarkAsReference (TechnosphereExchange fid amt uid isInp _ linkId procLink loc) =
    TechnosphereExchange fid amt uid isInp False linkId procLink loc
unmarkAsReference ex = ex -- No change for biosphere exchanges

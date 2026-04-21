{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | EcoSpold1 SAX Parser for Ecoinvent 2.x formats
This parser handles the older EcoSpold1 XML format (.XML files)
used in Ecoinvent versions 2.x (e.g., 2.2)
-}
module EcoSpold.Parser1 (
    streamParseActivityAndFlowsFromFile1,
    streamParseAllDatasetsFromFile1,

    -- * XML parsing (exported for testing)
    parseWithXeno,
    parseAllWithXeno,

    -- * Pure helpers (exported for testing)
    generateFlowUUID,
    generateUnitUUID,
    applyCutoffStrategy,
    hasReferenceProduct,
    removeZeroAmountCoproducts,
    assignSingleProductAsReference,
    isProductionExchange,
) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (bsToDouble, bsToInt, bsToText, isElement)
import Progress (ProgressLevel (..), reportProgress)
import Types
import qualified Xeno.SAX as X

{- | Namespace UUID for generating deterministic UUIDs from EcoSpold1 numeric IDs
Using UUID v5 (SHA1-based) with a custom namespace
-}
ecospold1Namespace :: UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "ecospold1.ecoinvent.org")

{- | Generate deterministic UUID from dataset number and exchange number
This ensures consistent UUIDs across multiple parses
-}
generateFlowUUID :: Int -> Int -> Text -> Text -> UUID
generateFlowUUID datasetNumber exchangeNumber flowName category =
    let key =
            T.pack (show datasetNumber)
                <> ":"
                <> T.pack (show exchangeNumber)
                <> ":"
                <> flowName
                <> ":"
                <> category
     in UUID5.generateNamed ecospold1Namespace (BS.unpack $ TE.encodeUtf8 key)

-- | Generate deterministic UUID for unit from unit name
generateUnitUUID :: Text -> UUID
generateUnitUUID unitName =
    UUID5.generateNamed ecospold1Namespace (BS.unpack $ TE.encodeUtf8 $ "unit:" <> unitName)

-- ============================================================================
-- Xeno SAX Parser Implementation for EcoSpold1
-- ============================================================================

-- | Element context tracker
data ElementContext
    = InReferenceFunction
    | InGeography
    | InExchange !ExchangeData
    | InInputGroup !ExchangeData -- Keep parent exchange data
    | InOutputGroup !ExchangeData -- Keep parent exchange data
    | Other
    deriving (Eq)

{- | Exchange accumulator for EcoSpold1 format
All data comes from attributes on the <exchange> element
-}
data ExchangeData = ExchangeData
    { exNumber :: !Int -- Exchange number (numeric ID)
    , exName :: !Text -- Flow name
    , exCategory :: !Text -- Category
    , exSubCategory :: !Text -- Subcategory
    , exLocation :: !Text -- Location (for technosphere)
    , exUnit :: !Text -- Unit name
    , exMeanValue :: !Double -- Amount
    , exInputGroup :: !Text -- Input group (1-4 = technosphere input, 4 = resource)
    , exOutputGroup :: !Text -- Output group (0 = reference, 1-3 = byproduct, 4 = emission)
    , exCASNumber :: !Text -- CAS number (optional)
    , exFormula :: !Text -- Chemical formula (optional)
    , exInfrastructure :: !Bool -- Infrastructure process flag
    }
    deriving (Eq)

-- | Initial exchange data
emptyExchangeData :: ExchangeData
emptyExchangeData = ExchangeData 0 "" "" "" "" "" 0.0 "" "" "" "" False

-- | Parsing state accumulator
data ParseState = ParseState
    { psDatasetNumber :: !Int
    , psActivityName :: !(Maybe Text)
    , psActivityCategory :: !Text
    , psActivitySubCategory :: !Text
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![Exchange]
    , psFlows :: ![Flow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString]
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString]
    , psSupplierLinks :: !(M.Map UUID Int) -- flowId → supplier dataset number (technosphere inputs)
    , psCompletedActivities :: ![Either String (Activity, [Flow], [Unit], Int, M.Map UUID Int)]
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState =
    ParseState
        { psDatasetNumber = 0
        , psActivityName = Nothing
        , psActivityCategory = ""
        , psActivitySubCategory = ""
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psFlows = []
        , psUnits = []
        , psPath = []
        , psContext = Other
        , psTextAccum = []
        , psSupplierLinks = M.empty
        , psCompletedActivities = []
        }

-- | Xeno SAX parser for EcoSpold1
parseWithXeno :: BS.ByteString -> Either String (Activity, [Flow], [Unit], Int, M.Map UUID Int)
parseWithXeno xmlContent =
    case X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent of
        Left err -> Left (show err)
        Right finalState ->
            case psCompletedActivities finalState of
                (result : _) -> result
                [] -> buildResult finalState
  where
    -- Open tag handler
    openTag state tagName =
        let newPath = tagName : psPath state
            newContext
                | isElement tagName "referenceFunction" = InReferenceFunction
                | isElement tagName "geography" = InGeography
                | isElement tagName "exchange" = InExchange emptyExchangeData
                | isElement tagName "inputGroup" =
                    case psContext state of
                        InExchange edata -> InInputGroup edata -- Preserve exchange data
                        _ -> psContext state
                | isElement tagName "outputGroup" =
                    case psContext state of
                        InExchange edata -> InOutputGroup edata -- Preserve exchange data
                        _ -> psContext state
                | otherwise = psContext state
         in state{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler
    attribute state name value =
        case psContext state of
            InReferenceFunction ->
                let st =
                        state
                            { psActivityName =
                                if isElement name "name"
                                    then Just (bsToText value)
                                    else psActivityName state
                            , psRefUnit =
                                if isElement name "unit"
                                    then Just (bsToText value)
                                    else psRefUnit state
                            , psActivityCategory =
                                if isElement name "category"
                                    then bsToText value
                                    else psActivityCategory state
                            , psActivitySubCategory =
                                if isElement name "subCategory"
                                    then bsToText value
                                    else psActivitySubCategory state
                            , psDescription =
                                if isElement name "generalComment" && not (BS.null value)
                                    then bsToText value : psDescription state
                                    else psDescription state
                            }
                 in st
            InGeography ->
                if isElement name "location"
                    then state{psLocation = Just (bsToText value)}
                    else state
            InExchange edata ->
                let updated =
                        edata
                            { exNumber = if isElement name "number" then bsToInt value else exNumber edata
                            , exName = if isElement name "name" then bsToText value else exName edata
                            , exCategory = if isElement name "category" then bsToText value else exCategory edata
                            , exSubCategory = if isElement name "subCategory" then bsToText value else exSubCategory edata
                            , exLocation = if isElement name "location" then bsToText value else exLocation edata
                            , exUnit = if isElement name "unit" then bsToText value else exUnit edata
                            , exMeanValue = if isElement name "meanValue" then bsToDouble value else exMeanValue edata
                            , exCASNumber = if isElement name "CASNumber" then bsToText value else exCASNumber edata
                            , exFormula = if isElement name "formula" then bsToText value else exFormula edata
                            , exInfrastructure =
                                if isElement name "infrastructureProcess"
                                    then bsToText value == "true"
                                    else exInfrastructure edata
                            }
                 in state{psContext = InExchange updated}
            _ ->
                -- Handle dataset number at top level
                if isElement name "number" && any (isElement "dataset") (psPath state)
                    then state{psDatasetNumber = bsToInt value}
                    else state

    -- End of opening tag
    endOpen state _tagName = state

    -- Text content handler
    text state content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed
                then state
                else state{psTextAccum = trimmed : psTextAccum state}

    -- Close tag handler
    closeTag state tagName
        | isElement tagName "inputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InInputGroup edata ->
                        -- Restore parent exchange context with updated inputGroup
                        state{psContext = InExchange edata{exInputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    InExchange edata ->
                        state{psContext = InExchange edata{exInputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "outputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InOutputGroup edata ->
                        -- Restore parent exchange context with updated outputGroup
                        state{psContext = InExchange edata{exOutputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    InExchange edata ->
                        state{psContext = InExchange edata{exOutputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "exchange" =
            case psContext state of
                InExchange edata ->
                    let (exchange, flow, unit) = buildExchange (psDatasetNumber state) (psLocation state) edata
                        !supplierLinks = case exchange of
                            TechnosphereExchange _ _ _ True _ _ _ _
                                | exNumber edata /= 0 ->
                                    M.insert (exchangeFlowId exchange) (exNumber edata) (psSupplierLinks state)
                            _ -> psSupplierLinks state
                     in state
                            { psExchanges = exchange : psExchanges state
                            , psFlows = flow : psFlows state
                            , psUnits = unit : psUnits state
                            , psSupplierLinks = supplierLinks
                            , psContext = Other
                            , psPath = tail (psPath state)
                            , psTextAccum = []
                            }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "referenceFunction" =
            state{psContext = Other, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "geography" =
            state{psContext = Other, psPath = tail (psPath state), psTextAccum = []}
        -- Handle dataset close tag: accumulate completed activity for multi-dataset files
        | isElement tagName "dataset" =
            let !result = buildResult state
                -- Reset dataset-specific fields for next dataset
                -- Preserve: psPath (after popping current element), psCompletedActivities
                resetState =
                    state
                        { psCompletedActivities = result : psCompletedActivities state
                        , psDatasetNumber = 0
                        , psActivityName = Nothing
                        , psActivityCategory = ""
                        , psActivitySubCategory = ""
                        , psLocation = Nothing
                        , psRefUnit = Nothing
                        , psDescription = []
                        , psExchanges = []
                        , psFlows = []
                        , psUnits = []
                        , psContext = Other
                        , psTextAccum = []
                        , psSupplierLinks = M.empty
                        }
             in resetState{psPath = tail (psPath state)}
        | otherwise =
            state{psPath = if null (psPath state) then [] else tail (psPath state)}

    -- CDATA handler
    cdata state content = text state content

    -- Build exchange, flow, and unit from exchange data
    -- activityLoc is the activity's location for fallback
    buildExchange :: Int -> Maybe Text -> ExchangeData -> (Exchange, Flow, Unit)
    buildExchange datasetNum activityLoc edata =
        let flowId = generateFlowUUID datasetNum (exNumber edata) (exName edata) (exCategory edata)
            unitId = generateUnitUUID (exUnit edata)

            -- Determine flow type from input/output groups
            -- EcoSpold1 groups:
            -- Input: 1-3 = technosphere, 4 = resource (biosphere)
            -- Output: 0 = reference product, 1-3 = byproduct/co-product, 4 = emission (biosphere)
            inputGroup = exInputGroup edata
            outputGroup = exOutputGroup edata

            isBiosphere = inputGroup == "4" || outputGroup == "4"
            isInput = not (T.null inputGroup)
            isReferenceProduct = outputGroup == "0"

            -- Build category string
            category =
                if T.null (exSubCategory edata)
                    then exCategory edata
                    else exCategory edata <> "/" <> exSubCategory edata

            flowType = if isBiosphere then Biosphere else Technosphere

            -- Exchange location: use exchange's own location
            -- For technosphere: leave empty if not specified, so Loader can use name-only lookup
            -- For biosphere: fall back to activity location (biosphere flows don't need supplier linking)
            exchangeLocation =
                if T.null (exLocation edata)
                    then
                        if isBiosphere
                            then case activityLoc of
                                Just loc -> loc
                                Nothing -> ""
                            else "" -- Technosphere: leave empty for name-only lookup in Loader
                    else exLocation edata

            -- Set activityLinkId to nil - will be resolved later in Loader using
            -- (flowName, exchangeLocation) lookup against supplier activities
            exchange =
                if isBiosphere
                    then BiosphereExchange flowId (exMeanValue edata) unitId (inputGroup == "4") exchangeLocation
                    else
                        TechnosphereExchange
                            flowId
                            (exMeanValue edata)
                            unitId
                            isInput
                            isReferenceProduct
                            UUID.nil -- Will be resolved in Loader.fixEcoSpold1ActivityLinks
                            Nothing
                            exchangeLocation

            cas = if T.null (exCASNumber edata) then Nothing else Just (exCASNumber edata)
            flow = Flow flowId (exName edata) category Nothing unitId flowType M.empty cas Nothing

            unit = Unit unitId (exUnit edata) (exUnit edata) ""
         in (exchange, flow, unit)

    -- Build final result
    buildResult :: ParseState -> Either String (Activity, [Flow], [Unit], Int, M.Map UUID Int)
    buildResult st =
        let name = case psActivityName st of
                Just n -> n
                Nothing -> "Unknown Activity"
            location = case psLocation st of
                Just loc -> loc
                Nothing -> "GLO"
            refUnit = case psRefUnit st of
                Just u -> u
                Nothing -> "UNKNOWN_UNIT"
            description = reverse (psDescription st)
            classifications =
                M.fromList $
                    filter
                        (not . T.null . snd)
                        [("Category", psActivityCategory st), ("SubCategory", psActivitySubCategory st)]
            activity = Activity name description M.empty classifications location refUnit (reverse $ psExchanges st) M.empty M.empty
            flows = reverse (psFlows st)
            units = reverse (psUnits st)
         in case applyCutoffStrategy activity of
                Right act -> Right (act, flows, units, psDatasetNumber st, psSupplierLinks st)
                Left err -> Left err

-- | Parse EcoSpold1 file using Xeno SAX parser
streamParseActivityAndFlowsFromFile1 :: FilePath -> IO (Either String (Activity, [Flow], [Unit], Int, M.Map UUID Int))
streamParseActivityAndFlowsFromFile1 path = do
    !xmlContent <- BS.readFile path
    return (parseWithXeno xmlContent)

-- | Apply cut-off strategy (same logic as EcoSpold2)
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
hasReferenceProduct act = any exchangeIsReference (exchanges act)

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
assignSingleProductAsReference act =
    let productionExchanges = [ex | ex <- exchanges act, isProductionExchange ex]
        nonZeroProduction = [ex | ex <- productionExchanges, exchangeAmount ex /= 0.0]
     in case nonZeroProduction of
            [singleProduct] ->
                let updatedExchanges = map (updateReferenceProduct singleProduct) (exchanges act)
                 in act{exchanges = updatedExchanges}
            _ -> act

-- | Check if exchange is production exchange
isProductionExchange :: Exchange -> Bool
isProductionExchange (TechnosphereExchange _ _ _ False _ _ _ _) = True
isProductionExchange _ = False

-- | Update reference product flag
updateReferenceProduct :: Exchange -> Exchange -> Exchange
updateReferenceProduct target current
    | exchangeFlowId target == exchangeFlowId current = markAsReference current
    | otherwise = unmarkAsReference current

-- | Mark exchange as reference product
markAsReference :: Exchange -> Exchange
markAsReference (TechnosphereExchange fid amt uid isInp _ linkId procLink loc) =
    TechnosphereExchange fid amt uid isInp True linkId procLink loc
markAsReference ex = ex

-- | Unmark exchange as reference product
unmarkAsReference :: Exchange -> Exchange
unmarkAsReference (TechnosphereExchange fid amt uid isInp _ linkId procLink loc) =
    TechnosphereExchange fid amt uid isInp False linkId procLink loc
unmarkAsReference ex = ex

-- ============================================================================
-- Multi-dataset file support
-- ============================================================================

{- | Parse ALL datasets from an EcoSpold1 file (multi-dataset support)
Returns the accumulated completed activities from psCompletedActivities
Outer Either = XML parse failure; inner Either = per-activity failure
-}
parseAllWithXeno :: BS.ByteString -> Either String [Either String (Activity, [Flow], [Unit], Int, M.Map UUID Int)]
parseAllWithXeno xmlContent =
    case X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent of
        Left err -> Left (show err)
        Right finalState -> Right (reverse $ psCompletedActivities finalState)
  where
    -- Open tag handler
    openTag state tagName =
        let newPath = tagName : psPath state
            newContext
                | isElement tagName "referenceFunction" = InReferenceFunction
                | isElement tagName "geography" = InGeography
                | isElement tagName "exchange" = InExchange emptyExchangeData
                | isElement tagName "inputGroup" =
                    case psContext state of
                        InExchange edata -> InInputGroup edata
                        _ -> psContext state
                | isElement tagName "outputGroup" =
                    case psContext state of
                        InExchange edata -> InOutputGroup edata
                        _ -> psContext state
                | otherwise = psContext state
         in state{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler
    attribute state name value =
        case psContext state of
            InReferenceFunction ->
                let st =
                        state
                            { psActivityName =
                                if isElement name "name"
                                    then Just (bsToText value)
                                    else psActivityName state
                            , psRefUnit =
                                if isElement name "unit"
                                    then Just (bsToText value)
                                    else psRefUnit state
                            , psActivityCategory =
                                if isElement name "category"
                                    then bsToText value
                                    else psActivityCategory state
                            , psActivitySubCategory =
                                if isElement name "subCategory"
                                    then bsToText value
                                    else psActivitySubCategory state
                            , psDescription =
                                if isElement name "generalComment" && not (BS.null value)
                                    then bsToText value : psDescription state
                                    else psDescription state
                            }
                 in st
            InGeography ->
                if isElement name "location"
                    then state{psLocation = Just (bsToText value)}
                    else state
            InExchange edata ->
                let updated =
                        edata
                            { exNumber = if isElement name "number" then bsToInt value else exNumber edata
                            , exName = if isElement name "name" then bsToText value else exName edata
                            , exCategory = if isElement name "category" then bsToText value else exCategory edata
                            , exSubCategory = if isElement name "subCategory" then bsToText value else exSubCategory edata
                            , exLocation = if isElement name "location" then bsToText value else exLocation edata
                            , exUnit = if isElement name "unit" then bsToText value else exUnit edata
                            , exMeanValue = if isElement name "meanValue" then bsToDouble value else exMeanValue edata
                            , exCASNumber = if isElement name "CASNumber" then bsToText value else exCASNumber edata
                            , exFormula = if isElement name "formula" then bsToText value else exFormula edata
                            , exInfrastructure =
                                if isElement name "infrastructureProcess"
                                    then bsToText value == "true"
                                    else exInfrastructure edata
                            }
                 in state{psContext = InExchange updated}
            _ ->
                -- Handle dataset number at top level
                if isElement name "number" && any (isElement "dataset") (psPath state)
                    then state{psDatasetNumber = bsToInt value}
                    else state

    -- End of opening tag
    endOpen state _tagName = state

    -- Text content handler
    text state content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed
                then state
                else state{psTextAccum = trimmed : psTextAccum state}

    -- Close tag handler
    closeTag state tagName
        | isElement tagName "inputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InInputGroup edata ->
                        state{psContext = InExchange edata{exInputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    InExchange edata ->
                        state{psContext = InExchange edata{exInputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "outputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InOutputGroup edata ->
                        state{psContext = InExchange edata{exOutputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    InExchange edata ->
                        state{psContext = InExchange edata{exOutputGroup = txt}, psPath = tail (psPath state), psTextAccum = []}
                    _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "exchange" =
            case psContext state of
                InExchange edata ->
                    let (exchange, flow, unit) = buildExchangeForAll (psDatasetNumber state) (psLocation state) edata
                        !supplierLinks = case exchange of
                            TechnosphereExchange _ _ _ True _ _ _ _
                                | exNumber edata /= 0 ->
                                    M.insert (exchangeFlowId exchange) (exNumber edata) (psSupplierLinks state)
                            _ -> psSupplierLinks state
                     in state
                            { psExchanges = exchange : psExchanges state
                            , psFlows = flow : psFlows state
                            , psUnits = unit : psUnits state
                            , psSupplierLinks = supplierLinks
                            , psContext = Other
                            , psPath = tail (psPath state)
                            , psTextAccum = []
                            }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "referenceFunction" =
            state{psContext = Other, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "geography" =
            state{psContext = Other, psPath = tail (psPath state), psTextAccum = []}
        -- Handle dataset close tag: accumulate completed activity
        | isElement tagName "dataset" =
            let !result = buildResultForAll state
                resetState =
                    state
                        { psCompletedActivities = result : psCompletedActivities state
                        , psDatasetNumber = 0
                        , psActivityName = Nothing
                        , psActivityCategory = ""
                        , psActivitySubCategory = ""
                        , psLocation = Nothing
                        , psRefUnit = Nothing
                        , psDescription = []
                        , psExchanges = []
                        , psFlows = []
                        , psUnits = []
                        , psContext = Other
                        , psTextAccum = []
                        , psSupplierLinks = M.empty
                        }
             in resetState{psPath = tail (psPath state)}
        | otherwise =
            state{psPath = if null (psPath state) then [] else tail (psPath state)}

    -- CDATA handler
    cdata state content = text state content

    -- Build exchange, flow, and unit from exchange data (same logic as parseWithXeno)
    buildExchangeForAll :: Int -> Maybe Text -> ExchangeData -> (Exchange, Flow, Unit)
    buildExchangeForAll datasetNum activityLoc edata =
        let flowId = generateFlowUUID datasetNum (exNumber edata) (exName edata) (exCategory edata)
            unitId = generateUnitUUID (exUnit edata)
            inputGroup = exInputGroup edata
            outputGroup = exOutputGroup edata
            isBiosphere = inputGroup == "4" || outputGroup == "4"
            isInput = not (T.null inputGroup)
            isReferenceProduct = outputGroup == "0"
            category =
                if T.null (exSubCategory edata)
                    then exCategory edata
                    else exCategory edata <> "/" <> exSubCategory edata
            flowType = if isBiosphere then Biosphere else Technosphere
            exchangeLocation =
                if T.null (exLocation edata)
                    then
                        if isBiosphere
                            then case activityLoc of
                                Just loc -> loc
                                Nothing -> ""
                            else ""
                    else exLocation edata
            exchange =
                if isBiosphere
                    then BiosphereExchange flowId (exMeanValue edata) unitId (inputGroup == "4") exchangeLocation
                    else
                        TechnosphereExchange
                            flowId
                            (exMeanValue edata)
                            unitId
                            isInput
                            isReferenceProduct
                            UUID.nil
                            Nothing
                            exchangeLocation
            cas = if T.null (exCASNumber edata) then Nothing else Just (exCASNumber edata)
            flow = Flow flowId (exName edata) category Nothing unitId flowType M.empty cas Nothing
            unit = Unit unitId (exUnit edata) (exUnit edata) ""
         in (exchange, flow, unit)

    -- Build final result for a single dataset
    buildResultForAll :: ParseState -> Either String (Activity, [Flow], [Unit], Int, M.Map UUID Int)
    buildResultForAll st =
        let name = case psActivityName st of
                Just n -> n
                Nothing -> "Unknown Activity"
            location = case psLocation st of
                Just loc -> loc
                Nothing -> "GLO"
            refUnit = case psRefUnit st of
                Just u -> u
                Nothing -> "UNKNOWN_UNIT"
            description = reverse (psDescription st)
            classifications =
                M.fromList $
                    filter
                        (not . T.null . snd)
                        [("Category", psActivityCategory st), ("SubCategory", psActivitySubCategory st)]
            activity = Activity name description M.empty classifications location refUnit (reverse $ psExchanges st) M.empty M.empty
            flows = reverse (psFlows st)
            units = reverse (psUnits st)
         in case applyCutoffStrategy activity of
                Right act -> Right (act, flows, units, psDatasetNumber st, psSupplierLinks st)
                Left err -> Left err

{- | Parse ALL datasets from a single EcoSpold1 file
Used for multi-dataset files where <ecoSpold> contains multiple <dataset> elements
Skips activities that fail (e.g. no reference product) and logs warnings
-}
streamParseAllDatasetsFromFile1 :: FilePath -> IO [(Activity, [Flow], [Unit], Int, M.Map UUID Int)]
streamParseAllDatasetsFromFile1 path = do
    !xmlContent <- BS.readFile path
    case parseAllWithXeno xmlContent of
        Right results -> do
            forM_ [e | Left e <- results] $ \e ->
                reportProgress Warning $ "Skipping dataset in " ++ path ++ ": " ++ e
            return [r | Right r <- results]
        Left err -> do
            reportProgress Warning $ "Failed to parse " ++ path ++ ": " ++ err
            return []

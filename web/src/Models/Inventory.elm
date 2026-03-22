module Models.Inventory exposing
    ( InventoryExport
    , InventoryMetadata
    , InventoryFlowDetail
    , InventoryStatistics
    , Flow
    , ActivitySummary
    , inventoryExportDecoder
    , inventoryMetadataDecoder
    , inventoryFlowDetailDecoder
    , inventoryStatisticsDecoder
    , flowDecoder
    , activitySummaryDecoder
    )

import Json.Decode as Decode exposing (Decoder, field, int, string, float, bool, list)
import Dict exposing (Dict)

-- Main inventory export structure
type alias InventoryExport =
    { ieMetadata : InventoryMetadata
    , ieFlows : List InventoryFlowDetail
    , ieStatistics : InventoryStatistics
    }

-- Metadata about the inventory calculation
type alias InventoryMetadata =
    { imRootActivity : ActivitySummary
    , imTotalFlows : Int
    , imEmissionFlows : Int
    , imResourceFlows : Int
    }

-- Individual flow detail in the inventory
type alias InventoryFlowDetail =
    { ifdFlow : Flow
    , ifdQuantity : Float
    , ifdUnitName : String
    , ifdIsEmission : Bool
    , ifdCategory : String
    }

-- Statistics about the inventory
type alias InventoryStatistics =
    { isTotalQuantity : Float
    , isEmissionQuantity : Float
    , isResourceQuantity : Float
    }

-- Flow information
type alias Flow =
    { flowId : String
    , flowName : String
    , flowCategory : String
    , flowSubcompartment : Maybe String
    , flowType : String
    , flowUnitId : String
    , flowSynonyms : Dict String (List String)
    }

-- Activity summary (reuse from Activity model or define here)
type alias ActivitySummary =
    { prsId : String
    , prsName : String
    , prsLocation : String
    }

-- JSON decoders
inventoryExportDecoder : Decoder InventoryExport
inventoryExportDecoder =
    Decode.map3 InventoryExport
        (field "ieMetadata" inventoryMetadataDecoder)
        (field "ieFlows" (list inventoryFlowDetailDecoder))
        (field "ieStatistics" inventoryStatisticsDecoder)

inventoryMetadataDecoder : Decoder InventoryMetadata
inventoryMetadataDecoder =
    Decode.map4 InventoryMetadata
        (field "imRootActivity" activitySummaryDecoder)
        (field "imTotalFlows" int)
        (field "imEmissionFlows" int)
        (field "imResourceFlows" int)

inventoryFlowDetailDecoder : Decoder InventoryFlowDetail
inventoryFlowDetailDecoder =
    Decode.map5 InventoryFlowDetail
        (field "ifdFlow" flowDecoder)
        (field "ifdQuantity" float)
        (field "ifdUnitName" string)
        (field "ifdIsEmission" bool)
        (field "ifdCategory" string)

inventoryStatisticsDecoder : Decoder InventoryStatistics
inventoryStatisticsDecoder =
    Decode.map3 InventoryStatistics
        (field "isTotalQuantity" float)
        (field "isEmissionQuantity" float)
        (field "isResourceQuantity" float)

flowDecoder : Decoder Flow
flowDecoder =
    Decode.map7 Flow
        (field "flowId" string)
        (field "flowName" string)
        (field "flowCategory" string)
        (Decode.maybe (field "flowSubcompartment" string))
        (field "flowType" string)
        (field "flowUnitId" string)
        (field "flowSynonyms" (Decode.dict (list string)))

activitySummaryDecoder : Decoder ActivitySummary
activitySummaryDecoder =
    Decode.map3 ActivitySummary
        (field "prsId" string)
        (field "prsName" string)
        (field "prsLocation" string)
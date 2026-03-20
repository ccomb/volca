module Models.SupplyChain exposing
    ( SupplyChainResponse
    , SupplyChainEntry
    , SupplyChainEdge
    , ActivitySummary
    , supplyChainResponseDecoder
    , supplyChainEntryDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias ActivitySummary =
    { processId : String
    , name : String
    , location : String
    , product : String
    , productAmount : Float
    , productUnit : String
    }


type alias SupplyChainEntry =
    { processId : String
    , name : String
    , location : String
    , quantity : Float
    , unit : String
    , scalingFactor : Float
    }


type alias SupplyChainEdge =
    { from : String
    , to : String
    , amount : Float
    }


type alias SupplyChainResponse =
    { root : ActivitySummary
    , totalActivities : Int
    , filteredActivities : Int
    , supplyChain : List SupplyChainEntry
    , edges : List SupplyChainEdge
    }


supplyChainResponseDecoder : Decoder SupplyChainResponse
supplyChainResponseDecoder =
    Decode.succeed SupplyChainResponse
        |> required "scrRoot" activitySummaryDecoder
        |> required "scrTotalActivities" Decode.int
        |> required "scrFilteredActivities" Decode.int
        |> required "scrSupplyChain" (Decode.list supplyChainEntryDecoder)
        |> required "scrEdges" (Decode.list supplyChainEdgeDecoder)


activitySummaryDecoder : Decoder ActivitySummary
activitySummaryDecoder =
    Decode.succeed ActivitySummary
        |> required "prsId" Decode.string
        |> required "prsName" Decode.string
        |> required "prsLocation" Decode.string
        |> required "prsProduct" Decode.string
        |> required "prsProductAmount" Decode.float
        |> required "prsProductUnit" Decode.string


supplyChainEntryDecoder : Decoder SupplyChainEntry
supplyChainEntryDecoder =
    Decode.succeed SupplyChainEntry
        |> required "sceProcessId" Decode.string
        |> required "sceName" Decode.string
        |> required "sceLocation" Decode.string
        |> required "sceQuantity" Decode.float
        |> required "sceUnit" Decode.string
        |> required "sceScalingFactor" Decode.float


supplyChainEdgeDecoder : Decoder SupplyChainEdge
supplyChainEdgeDecoder =
    Decode.succeed SupplyChainEdge
        |> required "sceEdgeFrom" Decode.string
        |> required "sceEdgeTo" Decode.string
        |> required "sceEdgeAmount" Decode.float

module Models.SupplyChain exposing
    ( SupplyChainEntry
    , SupplyChainEdge
    , SupplyChainResponse
    , supplyChainResponseDecoder
    , supplyChainEntryDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Models.Activity exposing (ActivitySummary, activitySummaryDecoder)


type alias SupplyChainEntry =
    { processId : String
    , name : String
    , location : String
    , quantity : Float
    , unit : String
    , scalingFactor : Float
    , classifications : Dict String String
    , depth : Int
    , upstreamCount : Int
    }


type alias SupplyChainEdge =
    { from : String
    , to : String
    , amount : Float
    }


type alias SupplyChainResponse =
    { totalActivities : Int
    , filteredActivities : Int
    , supplyChain : List SupplyChainEntry
    , root : ActivitySummary
    , edges : List SupplyChainEdge
    }


supplyChainResponseDecoder : Decoder SupplyChainResponse
supplyChainResponseDecoder =
    Decode.succeed SupplyChainResponse
        |> required "scrTotalActivities" Decode.int
        |> required "scrFilteredActivities" Decode.int
        |> required "scrSupplyChain" (Decode.list supplyChainEntryDecoder)
        |> required "scrRoot" activitySummaryDecoder
        |> required "scrEdges" (Decode.list supplyChainEdgeDecoder)


supplyChainEntryDecoder : Decoder SupplyChainEntry
supplyChainEntryDecoder =
    Decode.succeed SupplyChainEntry
        |> required "sceProcessId" Decode.string
        |> required "sceName" Decode.string
        |> required "sceLocation" Decode.string
        |> required "sceQuantity" Decode.float
        |> required "sceUnit" Decode.string
        |> required "sceScalingFactor" Decode.float
        |> required "sceClassifications" (Decode.dict Decode.string)
        |> required "sceDepth" Decode.int
        |> required "sceUpstreamCount" Decode.int


supplyChainEdgeDecoder : Decoder SupplyChainEdge
supplyChainEdgeDecoder =
    Decode.succeed SupplyChainEdge
        |> required "sceEdgeFrom" Decode.string
        |> required "sceEdgeTo" Decode.string
        |> required "sceEdgeAmount" Decode.float

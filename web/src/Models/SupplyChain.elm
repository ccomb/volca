module Models.SupplyChain exposing
    ( SupplyChainEntry
    , SupplyChainEdge
    , SupplyChainResponse
    , supplyChainResponseDecoder
    , supplyChainEntryDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
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
        |> optional "scrEdges" (Decode.list supplyChainEdgeDecoder) []


supplyChainEntryDecoder : Decoder SupplyChainEntry
supplyChainEntryDecoder =
    Decode.succeed SupplyChainEntry
        |> required "sceProcessId" Decode.string
        |> required "sceName" Decode.string
        |> required "sceLocation" Decode.string
        |> required "sceQuantity" Decode.float
        |> required "sceUnit" Decode.string
        |> required "sceScalingFactor" Decode.float
        |> optional "sceClassifications" (Decode.dict Decode.string) Dict.empty
        |> optional "sceDepth" Decode.int 0
        |> optional "sceUpstreamCount" Decode.int 0


supplyChainEdgeDecoder : Decoder SupplyChainEdge
supplyChainEdgeDecoder =
    Decode.succeed SupplyChainEdge
        |> required "sceEdgeFrom" Decode.string
        |> required "sceEdgeTo" Decode.string
        |> required "sceEdgeAmount" Decode.float

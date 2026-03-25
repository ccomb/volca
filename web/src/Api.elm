module Api exposing (SupplyChainParams, computeLCIABatch, createVariant, defaultSupplyChainParams, loadActivityInfo, loadActivityTree, loadConsumers, loadFlowActivities, loadFlowMapping, loadMethodCollections, loadMethodMapping, loadSupplyChain, searchFlows)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Activity exposing (ActivityInfo, ActivitySummary, ActivityTree, SearchResults, activityInfoDecoder, activitySummaryDecoder, activityTreeDecoder, searchResultsDecoder)
import Models.Flow exposing (FlowSearchResult, flowSearchResultDecoder)
import Models.LCIA exposing (FlowCFMapping, LCIABatchResult, MappingStatus, flowCFMappingDecoder, lciaBatchResultDecoder, mappingStatusDecoder)
import Models.Method exposing (MethodCollectionList, methodCollectionListDecoder)
import Models.SupplyChain exposing (SupplyChainResponse, supplyChainResponseDecoder)
import Models.Variant exposing (VariantResponse, variantResponseDecoder)
import Url.Builder


loadActivityInfo : (Result Http.Error ActivityInfo -> msg) -> String -> String -> Cmd msg
loadActivityInfo toMsg dbName activityId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId
        , expect = Http.expectJson toMsg activityInfoDecoder
        }


loadActivityTree : (Result Http.Error ActivityTree -> msg) -> String -> String -> Cmd msg
loadActivityTree toMsg dbName activityId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/tree"
        , expect = Http.expectJson toMsg activityTreeDecoder
        }


loadMethodCollections : (Result Http.Error MethodCollectionList -> msg) -> Cmd msg
loadMethodCollections toMsg =
    Http.get
        { url = "/api/v1/method-collections"
        , expect = Http.expectJson toMsg methodCollectionListDecoder
        }


computeLCIABatch : (Result Http.Error LCIABatchResult -> msg) -> String -> String -> String -> Cmd msg
computeLCIABatch toMsg dbName processId collection =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ processId ++ "/lcia-batch/" ++ collection
        , expect = Http.expectJson toMsg lciaBatchResultDecoder
        }


loadMethodMapping : (Result Http.Error MappingStatus -> msg) -> String -> String -> Cmd msg
loadMethodMapping toMsg methodId dbName =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/method/" ++ methodId ++ "/mapping"
        , expect = Http.expectJson toMsg mappingStatusDecoder
        }


loadFlowMapping : (Result Http.Error FlowCFMapping -> msg) -> String -> String -> Cmd msg
loadFlowMapping toMsg dbName methodId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/method/" ++ methodId ++ "/flow-mapping"
        , expect = Http.expectJson toMsg flowCFMappingDecoder
        }


type alias SupplyChainParams =
    { name : String
    , location : String
    , classification : String
    , maxDepth : String
    , minQuantity : String
    , limit : Int
    , offset : Int
    , sort : String
    , order : String
    }


defaultSupplyChainParams : SupplyChainParams
defaultSupplyChainParams =
    { name = ""
    , location = ""
    , classification = ""
    , maxDepth = ""
    , minQuantity = ""
    , limit = 50
    , offset = 0
    , sort = "depth"
    , order = "asc"
    }


loadSupplyChain : (Result Http.Error SupplyChainResponse -> msg) -> String -> String -> SupplyChainParams -> Cmd msg
loadSupplyChain toMsg dbName activityId params =
    let
        optParam key value =
            if String.isEmpty value then
                ""

            else
                "&" ++ key ++ "=" ++ value

        queryString =
            "?limit=" ++ String.fromInt params.limit
                ++ "&offset=" ++ String.fromInt params.offset
                ++ optParam "name" params.name
                ++ optParam "location" params.location
                ++ optParam "classification" params.classification
                ++ optParam "max-depth" params.maxDepth
                ++ optParam "min-quantity" params.minQuantity
                ++ optParam "sort" params.sort
                ++ optParam "order" params.order
    in
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/supply-chain" ++ queryString
        , expect = Http.expectJson toMsg supplyChainResponseDecoder
        }


loadConsumers : (Result Http.Error (List ActivitySummary) -> msg) -> String -> String -> Maybe String -> Cmd msg
loadConsumers toMsg dbName activityId nameFilter =
    let
        nameParam =
            case nameFilter of
                Just n ->
                    "&name=" ++ n

                Nothing ->
                    ""
    in
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/consumers?limit=200" ++ nameParam
        , expect = Http.expectJson toMsg (Decode.list activitySummaryDecoder)
        }


searchFlows : (Result Http.Error (SearchResults FlowSearchResult) -> msg) -> String -> String -> Int -> Int -> Cmd msg
searchFlows toMsg dbName query limit offset =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "flows" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "q" query)
                    , Just (Url.Builder.int "limit" limit)
                    , Just (Url.Builder.int "offset" offset)
                    ]
                )
        , expect = Http.expectJson toMsg (searchResultsDecoder flowSearchResultDecoder)
        }


loadFlowActivities : (Result Http.Error (List ActivitySummary) -> msg) -> String -> String -> Cmd msg
loadFlowActivities toMsg dbName flowId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/flow/" ++ flowId ++ "/activities"
        , expect = Http.expectJson toMsg (Decode.list activitySummaryDecoder)
        }


createVariant : (Result Http.Error VariantResponse -> msg) -> String -> String -> Encode.Value -> Cmd msg
createVariant toMsg dbName activityId body =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/variant"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg variantResponseDecoder
        }

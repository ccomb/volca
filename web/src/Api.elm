module Api exposing (computeLCIABatch, loadActivityInfo, loadActivityTree, loadFlowMapping, loadMethodCollections, loadMethodMapping, loadSupplyChain)

import Http
import Models.Activity exposing (ActivityInfo, ActivityTree, activityInfoDecoder, activityTreeDecoder)
import Models.LCIA exposing (FlowCFMapping, LCIABatchResult, MappingStatus, flowCFMappingDecoder, lciaBatchResultDecoder, mappingStatusDecoder)
import Models.Method exposing (MethodCollectionList, methodCollectionListDecoder)
import Models.SupplyChain exposing (SupplyChainResponse, supplyChainResponseDecoder)


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


loadSupplyChain : (Result Http.Error SupplyChainResponse -> msg) -> String -> String -> Cmd msg
loadSupplyChain toMsg dbName activityId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/supply-chain?limit=1000"
        , expect = Http.expectJson toMsg supplyChainResponseDecoder
        }

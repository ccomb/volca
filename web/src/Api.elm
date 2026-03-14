module Api exposing (computeLCIABatch, loadActivityInfo, loadActivityTree, loadFlowMapping, loadMethodCollections, loadMethodMapping)

import Http
import Models.Activity exposing (ActivityInfo, ActivityTree, activityInfoDecoder, activityTreeDecoder)
import Models.LCIA exposing (FlowCFMapping, LCIAResult, MappingStatus, flowCFMappingDecoder, lciaBatchDecoder, mappingStatusDecoder)
import Models.Method exposing (MethodCollectionList, methodCollectionListDecoder)


loadActivityInfo : (Result Http.Error ActivityInfo -> msg) -> String -> String -> Cmd msg
loadActivityInfo toMsg dbName activityId =
    Http.get
        { url = "/api/v1/database/" ++ dbName ++ "/activity/" ++ activityId
        , expect = Http.expectJson toMsg activityInfoDecoder
        }


loadActivityTree : (Result Http.Error ActivityTree -> msg) -> String -> String -> Cmd msg
loadActivityTree toMsg dbName activityId =
    Http.get
        { url = "/api/v1/database/" ++ dbName ++ "/activity/" ++ activityId ++ "/tree"
        , expect = Http.expectJson toMsg activityTreeDecoder
        }


loadMethodCollections : (Result Http.Error MethodCollectionList -> msg) -> Cmd msg
loadMethodCollections toMsg =
    Http.get
        { url = "/api/v1/method-collections"
        , expect = Http.expectJson toMsg methodCollectionListDecoder
        }


computeLCIABatch : (Result Http.Error (List LCIAResult) -> msg) -> String -> String -> String -> Cmd msg
computeLCIABatch toMsg dbName processId collection =
    Http.get
        { url = "/api/v1/database/" ++ dbName ++ "/activity/" ++ processId ++ "/lcia-batch/" ++ collection
        , expect = Http.expectJson toMsg lciaBatchDecoder
        }


loadMethodMapping : (Result Http.Error MappingStatus -> msg) -> String -> String -> Cmd msg
loadMethodMapping toMsg methodId dbName =
    Http.get
        { url = "/api/v1/database/" ++ dbName ++ "/method/" ++ methodId ++ "/mapping"
        , expect = Http.expectJson toMsg mappingStatusDecoder
        }


loadFlowMapping : (Result Http.Error FlowCFMapping -> msg) -> String -> String -> Cmd msg
loadFlowMapping toMsg dbName methodId =
    Http.get
        { url = "/api/v1/database/" ++ dbName ++ "/method/" ++ methodId ++ "/flow-mapping"
        , expect = Http.expectJson toMsg flowCFMappingDecoder
        }

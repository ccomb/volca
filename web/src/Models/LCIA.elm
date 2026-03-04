module Models.LCIA exposing
    ( FlowCFEntry
    , FlowCFMapping
    , LCIAResult
    , MappingStatus
    , MethodSummary
    , UnmappedFlow
    , flowCFMappingDecoder
    , lciaBatchDecoder
    , lciaResultDecoder
    , mappingStatusDecoder
    , methodSummaryDecoder
    , methodsListDecoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


{-| Summary of an LCIA method (for listing)
-}
type alias MethodSummary =
    { msmId : String
    , msmName : String
    , msmCategory : String
    , msmUnit : String
    , msmFactorCount : Int
    , msmCollection : String
    }


{-| LCIA computation result
-}
type alias LCIAResult =
    { lrMethodId : String
    , lrMethodName : String
    , lrCategory : String
    , lrScore : Float
    , lrUnit : String
    , lrMappedFlows : Int
    , lrUnmappedFlows : Int
    , lrUnmappedNames : List String
    }


{-| Flow mapping status for a method
-}
type alias MappingStatus =
    { mstMethodId : String
    , mstMethodName : String
    , mstTotalFactors : Int
    , mstMappedByUUID : Int
    , mstMappedByName : Int
    , mstMappedBySynonym : Int
    , mstUnmapped : Int
    , mstCoverage : Float
    , mstDbBiosphereCount : Int
    , mstUniqueDbFlowsMatched : Int
    , mstUnmappedFlows : List UnmappedFlow
    }


{-| Details about an unmapped flow
-}
type alias UnmappedFlow =
    { ufaFlowRef : String
    , ufaFlowName : String
    , ufaDirection : String
    }


{-| DB-flow-centric mapping: all biosphere flows with their CF assignments
-}
type alias FlowCFMapping =
    { fcmMethodName : String
    , fcmMethodUnit : String
    , fcmTotalFlows : Int
    , fcmMatchedFlows : Int
    , fcmFlows : List FlowCFEntry
    }


{-| A single DB biosphere flow with its CF assignment (if any)
-}
type alias FlowCFEntry =
    { fceFlowId : String
    , fceFlowName : String
    , fceFlowCategory : String
    , fceCfValue : Maybe Float
    , fceCfFlowName : Maybe String
    , fceMatchStrategy : Maybe String
    }



-- Decoders


methodSummaryDecoder : Decoder MethodSummary
methodSummaryDecoder =
    D.succeed MethodSummary
        |> required "msmId" D.string
        |> required "msmName" D.string
        |> required "msmCategory" D.string
        |> required "msmUnit" D.string
        |> required "msmFactorCount" D.int
        |> required "msmCollection" D.string


methodsListDecoder : Decoder (List MethodSummary)
methodsListDecoder =
    D.list methodSummaryDecoder


lciaResultDecoder : Decoder LCIAResult
lciaResultDecoder =
    D.succeed LCIAResult
        |> required "lrMethodId" D.string
        |> required "lrMethodName" D.string
        |> required "lrCategory" D.string
        |> required "lrScore" D.float
        |> required "lrUnit" D.string
        |> required "lrMappedFlows" D.int
        |> required "lrUnmappedFlows" D.int
        |> optional "lrUnmappedNames" (D.list D.string) []


lciaBatchDecoder : Decoder (List LCIAResult)
lciaBatchDecoder =
    D.list lciaResultDecoder


unmappedFlowDecoder : Decoder UnmappedFlow
unmappedFlowDecoder =
    D.succeed UnmappedFlow
        |> required "ufaFlowRef" D.string
        |> required "ufaFlowName" D.string
        |> required "ufaDirection" D.string


mappingStatusDecoder : Decoder MappingStatus
mappingStatusDecoder =
    D.succeed MappingStatus
        |> required "mstMethodId" D.string
        |> required "mstMethodName" D.string
        |> required "mstTotalFactors" D.int
        |> required "mstMappedByUUID" D.int
        |> required "mstMappedByName" D.int
        |> required "mstMappedBySynonym" D.int
        |> required "mstUnmapped" D.int
        |> required "mstCoverage" D.float
        |> required "mstDbBiosphereCount" D.int
        |> required "mstUniqueDbFlowsMatched" D.int
        |> required "mstUnmappedFlows" (D.list unmappedFlowDecoder)


flowCFEntryDecoder : Decoder FlowCFEntry
flowCFEntryDecoder =
    D.succeed FlowCFEntry
        |> required "fceFlowId" D.string
        |> required "fceFlowName" D.string
        |> required "fceFlowCategory" D.string
        |> optional "fceCfValue" (D.nullable D.float) Nothing
        |> optional "fceCfFlowName" (D.nullable D.string) Nothing
        |> optional "fceMatchStrategy" (D.nullable D.string) Nothing


flowCFMappingDecoder : Decoder FlowCFMapping
flowCFMappingDecoder =
    D.succeed FlowCFMapping
        |> required "fcmMethodName" D.string
        |> required "fcmMethodUnit" D.string
        |> required "fcmTotalFlows" D.int
        |> required "fcmMatchedFlows" D.int
        |> required "fcmFlows" (D.list flowCFEntryDecoder)

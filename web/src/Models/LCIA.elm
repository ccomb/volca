module Models.LCIA exposing
    ( FlowCFEntry
    , FlowCFMapping
    , FlowContributionEntry
    , LCIABatchResult
    , LCIAResult
    , MappingStatus
    , MethodSummary
    , UnmappedFlow
    , flowCFMappingDecoder
    , lciaBatchResultDecoder
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


{-| A single flow's contribution to an LCIA score
-}
type alias FlowContributionEntry =
    { fcoFlowName : String
    , fcoContribution : Float
    , fcoSharePct : Float
    }


{-| LCIA computation result
-}
type alias LCIAResult =
    { lrMethodId : String
    , lrMethodName : String
    , lrCategory : String
    , lrDamageCategory : String
    , lrScore : Float
    , lrUnit : String
    , lrNormalizedScore : Maybe Float
    , lrWeightedScore : Maybe Float
    , lrMappedFlows : Int
    , lrFunctionalUnit : String
    , lrTopContributors : List FlowContributionEntry
    }


{-| Batch LCIA result with optional single score
-}
type alias LCIABatchResult =
    { lbrResults : List LCIAResult
    , lbrSingleScore : Maybe Float
    , lbrSingleScoreUnit : Maybe String
    , lbrNormWeightSetName : Maybe String
    , lbrAvailableNWsets : List String
    }


{-| Flow mapping status for a method
-}
type alias MappingStatus =
    { mstMethodId : String
    , mstMethodName : String
    , mstTotalFactors : Int
    , mstMappedByUUID : Int
    , mstMappedByCAS : Int
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


flowContributionEntryDecoder : Decoder FlowContributionEntry
flowContributionEntryDecoder =
    D.succeed FlowContributionEntry
        |> required "fcoFlowName" D.string
        |> required "fcoContribution" D.float
        |> required "fcoSharePct" D.float


lciaResultDecoder : Decoder LCIAResult
lciaResultDecoder =
    D.succeed LCIAResult
        |> required "lrMethodId" D.string
        |> required "lrMethodName" D.string
        |> required "lrCategory" D.string
        |> optional "lrDamageCategory" D.string ""
        |> required "lrScore" D.float
        |> required "lrUnit" D.string
        |> optional "lrNormalizedScore" (D.nullable D.float) Nothing
        |> optional "lrWeightedScore" (D.nullable D.float) Nothing
        |> required "lrMappedFlows" D.int
        |> optional "lrFunctionalUnit" D.string ""
        |> optional "lrTopContributors" (D.list flowContributionEntryDecoder) []


lciaBatchResultDecoder : Decoder LCIABatchResult
lciaBatchResultDecoder =
    D.succeed LCIABatchResult
        |> required "lbrResults" (D.list lciaResultDecoder)
        |> optional "lbrSingleScore" (D.nullable D.float) Nothing
        |> optional "lbrSingleScoreUnit" (D.nullable D.string) Nothing
        |> optional "lbrNormWeightSetName" (D.nullable D.string) Nothing
        |> optional "lbrAvailableNWsets" (D.list D.string) []


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
        |> required "mstMappedByCAS" D.int
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

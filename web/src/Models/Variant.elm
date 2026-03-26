module Models.Variant exposing
    ( SubstitutionResult
    , VariantResponse
    , encodeVariantRequest
    , variantResponseDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Models.SupplyChain exposing (SupplyChainEntry, supplyChainEntryDecoder)


type alias SubstitutionResult =
    { from : String
    , to : String
    , coefficient : Float
    }


type alias VariantResponse =
    { originalProcessId : String
    , substitutions : List SubstitutionResult
    , supplyChain : List SupplyChainEntry
    , totalActivities : Int
    }


substitutionResultDecoder : Decoder SubstitutionResult
substitutionResultDecoder =
    Decode.succeed SubstitutionResult
        |> required "sbrFrom" Decode.string
        |> required "sbrTo" Decode.string
        |> required "sbrCoefficient" Decode.float


variantResponseDecoder : Decoder VariantResponse
variantResponseDecoder =
    Decode.succeed VariantResponse
        |> required "varOriginalProcessId" Decode.string
        |> required "varSubstitutions" (Decode.list substitutionResultDecoder)
        |> required "varSupplyChain" (Decode.list supplyChainEntryDecoder)
        |> required "varTotalActivities" Decode.int


encodeVariantRequest : List { from : String, to : String, consumer : String } -> Encode.Value
encodeVariantRequest substitutions =
    Encode.object
        [ ( "srSubstitutions"
          , Encode.list
                (\sub ->
                    Encode.object
                        [ ( "subFrom", Encode.string sub.from )
                        , ( "subTo", Encode.string sub.to )
                        , ( "subConsumer", Encode.string sub.consumer )
                        ]
                )
                substitutions
          )
        ]

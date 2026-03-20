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


encodeVariantRequest : List ( String, String ) -> Encode.Value
encodeVariantRequest substitutions =
    Encode.object
        [ ( "vrSubstitutions"
          , Encode.list
                (\( from, to ) ->
                    Encode.object
                        [ ( "subFrom", Encode.string from )
                        , ( "subTo", Encode.string to )
                        ]
                )
                substitutions
          )
        ]

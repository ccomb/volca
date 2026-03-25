module Models.Flow exposing (FlowSearchResult, flowSearchResultDecoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias FlowSearchResult =
    { id : String
    , name : String
    , category : String
    , unitName : String
    , synonyms : Dict String (List String)
    }


flowSearchResultDecoder : Decoder FlowSearchResult
flowSearchResultDecoder =
    Decode.succeed FlowSearchResult
        |> required "fsrId" Decode.string
        |> required "fsrName" Decode.string
        |> required "fsrCategory" Decode.string
        |> required "fsrUnitName" Decode.string
        |> required "fsrSynonyms" (Decode.dict (Decode.list Decode.string))

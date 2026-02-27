module Models.Database exposing
    ( ActivateResponse
    , DataPathCandidate
    , DatabaseList
    , DatabaseLoadStatus(..)
    , DatabaseSetupInfo
    , DatabaseStatus
    , DepLoadResult(..)
    , DependencySuggestion
    , LoadDatabaseResponse(..)
    , LocationFallback
    , MissingSupplier
    , UploadResponse
    , activateResponseDecoder
    , databaseListDecoder
    , databaseSetupInfoDecoder
    , databaseStatusDecoder
    , depLoadResultDecoder
    , loadDatabaseResponseDecoder
    , uploadResponseDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


{-| Load status: unloaded, partially linked, or fully loaded
-}
type DatabaseLoadStatus
    = Unloaded
    | PartiallyLinked
    | DbLoaded


{-| Database status from the API
-}
type alias DatabaseStatus =
    { name : String
    , displayName : String
    , description : Maybe String
    , loadAtStartup : Bool
    , status : DatabaseLoadStatus
    , isUploaded : Bool
    , path : String
    , format : Maybe String
    }


{-| List of databases
-}
type alias DatabaseList =
    { databases : List DatabaseStatus
    }


{-| Response from activating a database
-}
type alias ActivateResponse =
    { success : Bool
    , message : String
    , database : Maybe DatabaseStatus
    }


{-| JSON decoder for DatabaseLoadStatus
-}
databaseLoadStatusDecoder : Decoder DatabaseLoadStatus
databaseLoadStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "loaded" ->
                        Decode.succeed DbLoaded

                    "partially_linked" ->
                        Decode.succeed PartiallyLinked

                    "unloaded" ->
                        Decode.succeed Unloaded

                    _ ->
                        Decode.fail ("Unknown DatabaseLoadStatus: " ++ s)
            )


{-| JSON decoder for DatabaseStatus
-}
databaseStatusDecoder : Decoder DatabaseStatus
databaseStatusDecoder =
    Decode.succeed DatabaseStatus
        |> required "dsaName" Decode.string
        |> required "dsaDisplayName" Decode.string
        |> optional "dsaDescription" (Decode.nullable Decode.string) Nothing
        |> required "dsaLoadAtStartup" Decode.bool
        |> required "dsaStatus" databaseLoadStatusDecoder
        |> required "dsaIsUploaded" Decode.bool
        |> required "dsaPath" Decode.string
        |> optional "dsaFormat" (Decode.nullable Decode.string) Nothing


{-| JSON decoder for DatabaseList
-}
databaseListDecoder : Decoder DatabaseList
databaseListDecoder =
    Decode.succeed DatabaseList
        |> required "dlrDatabases" (Decode.list databaseStatusDecoder)


{-| JSON decoder for ActivateResponse
-}
activateResponseDecoder : Decoder ActivateResponse
activateResponseDecoder =
    Decode.succeed ActivateResponse
        |> required "arSuccess" Decode.bool
        |> required "arMessage" Decode.string
        |> optional "arDatabase" (Decode.nullable databaseStatusDecoder) Nothing


{-| Result of auto-loading a single dependency
-}
type DepLoadResult
    = DepLoaded String
    | DepLoadFailed String String


{-| Response for the load database endpoint
-}
type LoadDatabaseResponse
    = LoadFailed String
    | LoadSucceeded DatabaseStatus (List DepLoadResult)


{-| JSON decoder for DepLoadResult
-}
depLoadResultDecoder : Decoder DepLoadResult
depLoadResultDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "DepLoaded" ->
                        Decode.map DepLoaded (Decode.field "dlrName" Decode.string)

                    "DepLoadFailed" ->
                        Decode.map2 DepLoadFailed
                            (Decode.field "dlfName" Decode.string)
                            (Decode.field "dlfError" Decode.string)

                    _ ->
                        Decode.fail ("Unknown DepLoadResult tag: " ++ tag)
            )


{-| JSON decoder for LoadDatabaseResponse
-}
loadDatabaseResponseDecoder : Decoder LoadDatabaseResponse
loadDatabaseResponseDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "LoadFailed" ->
                        Decode.map LoadFailed (Decode.field "ldrError" Decode.string)

                    "LoadSucceeded" ->
                        Decode.map2 LoadSucceeded
                            (Decode.field "ldrDatabase" databaseStatusDecoder)
                            (Decode.field "ldrDeps" (Decode.list depLoadResultDecoder))

                    _ ->
                        Decode.fail ("Unknown LoadDatabaseResponse tag: " ++ tag)
            )


{-| Response from uploading a database
-}
type alias UploadResponse =
    { success : Bool
    , message : String
    , slug : Maybe String
    , format : Maybe String
    }


{-| JSON decoder for UploadResponse
-}
uploadResponseDecoder : Decoder UploadResponse
uploadResponseDecoder =
    Decode.succeed UploadResponse
        |> required "uprSuccess" Decode.bool
        |> required "uprMessage" Decode.string
        |> optional "uprSlug" (Decode.nullable Decode.string) Nothing
        |> optional "uprFormat" (Decode.nullable Decode.string) Nothing


{-| Information about a missing supplier product
-}
type alias MissingSupplier =
    { productName : String
    , count : Int
    , location : Maybe String
    , reason : String
    , detail : Maybe String
    }


{-| Suggestion for a dependency database
-}
type alias DependencySuggestion =
    { databaseName : String
    , displayName : String
    , matchCount : Int
    }


{-| Setup info for a database (for the setup page)
-}
type alias LocationFallback =
    { product : String
    , requested : String
    , actual : String
    }


{-| Candidate data path within an uploaded database
-}
type alias DataPathCandidate =
    { path : String
    , format : String
    , fileCount : Int
    }


type alias DatabaseSetupInfo =
    { name : String
    , displayName : String
    , activityCount : Int
    , inputCount : Int
    , completeness : Float
    , internalLinks : Int
    , crossDBLinks : Int
    , unresolvedLinks : Int
    , missingSuppliers : List MissingSupplier
    , selectedDependencies : List String
    , suggestions : List DependencySuggestion
    , isReady : Bool
    , unknownUnits : List String
    , locationFallbacks : List LocationFallback
    , dataPath : String
    , availablePaths : List DataPathCandidate
    , isLoaded : Bool
    }


{-| JSON decoder for MissingSupplier
-}
missingSupplierDecoder : Decoder MissingSupplier
missingSupplierDecoder =
    Decode.succeed MissingSupplier
        |> required "productName" Decode.string
        |> required "count" Decode.int
        |> optional "location" (Decode.nullable Decode.string) Nothing
        |> optional "reason" Decode.string "no_name_match"
        |> optional "detail" (Decode.nullable Decode.string) Nothing


{-| JSON decoder for DependencySuggestion
-}
dependencySuggestionDecoder : Decoder DependencySuggestion
dependencySuggestionDecoder =
    Decode.succeed DependencySuggestion
        |> required "databaseName" Decode.string
        |> required "displayName" Decode.string
        |> required "matchCount" Decode.int


{-| JSON decoder for DatabaseSetupInfo
-}
databaseSetupInfoDecoder : Decoder DatabaseSetupInfo
databaseSetupInfoDecoder =
    Decode.succeed DatabaseSetupInfo
        |> required "name" Decode.string
        |> required "displayName" Decode.string
        |> required "activityCount" Decode.int
        |> required "inputCount" Decode.int
        |> required "completeness" Decode.float
        |> required "internalLinks" Decode.int
        |> required "crossDBLinks" Decode.int
        |> required "unresolvedLinks" Decode.int
        |> required "missingSuppliers" (Decode.list missingSupplierDecoder)
        |> required "selectedDependencies" (Decode.list Decode.string)
        |> required "suggestions" (Decode.list dependencySuggestionDecoder)
        |> required "isReady" Decode.bool
        |> optional "unknownUnits" (Decode.list Decode.string) []
        |> optional "locationFallbacks" (Decode.list locationFallbackDecoder) []
        |> optional "dataPath" Decode.string ""
        |> optional "availablePaths" (Decode.list dataPathCandidateDecoder) []
        |> optional "isLoaded" Decode.bool False


{-| JSON decoder for DataPathCandidate
-}
dataPathCandidateDecoder : Decoder DataPathCandidate
dataPathCandidateDecoder =
    Decode.succeed DataPathCandidate
        |> required "path" Decode.string
        |> required "format" Decode.string
        |> required "fileCount" Decode.int


{-| JSON decoder for LocationFallback
-}
locationFallbackDecoder : Decoder LocationFallback
locationFallbackDecoder =
    Decode.succeed LocationFallback
        |> required "product" Decode.string
        |> required "requested" Decode.string
        |> required "actual" Decode.string

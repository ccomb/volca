port module Shared exposing
    ( Model
    , Msg(..)
    , AuthState(..)
    , ConsoleState(..)
    , RemoteData(..)
    , init
    , update
    , subscriptions
    , isDatabaseLoaded
    , isDatabaseLoading
    , getDatabaseDisplayName
    , httpErrorToString
    , viewLoadDatabasePrompt
    )

import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Models.Activity exposing (ActivityInfo, ActivityTree)
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), LoadDatabaseResponse(..), databaseListDecoder, loadDatabaseResponseDecoder)
import Models.Graph exposing (GraphData)
import Models.Inventory exposing (InventoryExport)
import Route exposing (Route(..))
import Set exposing (Set)


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failed String


type AuthState
    = AuthChecking
    | NeedsAuth { code : String, error : Maybe String }
    | Authenticated


type ConsoleState
    = Closed
    | Expanded


type alias Model =
    { key : Nav.Key
    , currentRoute : Route
    , lastActivitiesRoute : Maybe Route.ActivitiesFlags
    , databases : RemoteData DatabaseList
    , version : String
    , gitHash : String
    , buildTarget : String
    , consoleState : ConsoleState
    , menuOpen : Bool
    , cachedTrees : Dict String ActivityTree
    , cachedActivityInfo : Dict String ActivityInfo
    , cachedInventories : Dict String InventoryExport
    , cachedGraphs : Dict String GraphData
    , loadingDatabases : Set String
    , activeOperations : Int
    , loadProgressLines : List String
    , loadError : Maybe String
    , authState : AuthState
    , activityStack : List ( String, String )
    }


port onBackendProgress : (List String -> msg) -> Sub msg


type Msg
    = RouteChanged Route
    | NavigateTo Route
    | LoadDatabases
    | DatabasesLoaded (Result Http.Error DatabaseList)
    | LoadDatabase String
    | LoadDatabaseResult String (Result Http.Error LoadDatabaseResponse)
    | ToggleConsole
    | CloseConsole
    | CacheTree String ActivityTree
    | CacheActivityInfo String ActivityInfo
    | CacheInventory String InventoryExport
    | CacheGraph String GraphData
    | ToggleMenu
    | CloseMenu
    | PushActivity String String
    | NavigateBackToParent
    | BackendProgressLines (List String)
    | ClearProgress
    | VersionLoaded (Result Http.Error { version : String, gitHash : String, buildTarget : String })
    | UpdateAuthCode String
    | SubmitAuthCode
    | AuthResult (Result Http.Error ())
    | NoOp


init : () -> Nav.Key -> ( Model, Cmd Msg )
init () key =
    ( { key = key
      , currentRoute = RootRoute
      , lastActivitiesRoute = Nothing
      , databases = Loading
      , version = ""
      , gitHash = ""
      , buildTarget = ""
      , consoleState = Closed
      , menuOpen = False
      , cachedTrees = Dict.empty
      , cachedActivityInfo = Dict.empty
      , cachedInventories = Dict.empty
      , cachedGraphs = Dict.empty
      , loadingDatabases = Set.empty
      , activeOperations = 0
      , loadProgressLines = []
      , loadError = Nothing
      , authState = AuthChecking
      , activityStack = []
      }
    , Cmd.batch [ loadDatabases, loadVersion ]
    )


loadVersion : Cmd Msg
loadVersion =
    Http.get
        { url = "/api/v1/version"
        , expect =
            Http.expectJson VersionLoaded
                (Json.Decode.map3 (\v g b -> { version = v, gitHash = g, buildTarget = b })
                    (Json.Decode.field "version" Json.Decode.string)
                    (Json.Decode.field "gitHash" Json.Decode.string)
                    (Json.Decode.field "buildTarget" Json.Decode.string)
                )
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo route ->
            ( model
            , Nav.pushUrl model.key (Route.routeToUrl route)
            )

        RouteChanged route ->
            ( { model
                | currentRoute = route
                , menuOpen = False
                , lastActivitiesRoute =
                    case Route.matchActivities route of
                        Just flags ->
                            Just flags

                        Nothing ->
                            model.lastActivitiesRoute
              }
            , Cmd.none
            )

        VersionLoaded (Ok info) ->
            ( { model | version = info.version, gitHash = info.gitHash, buildTarget = info.buildTarget }, Cmd.none )

        VersionLoaded (Err _) ->
            ( model, Cmd.none )

        LoadDatabases ->
            case model.databases of
                Loaded _ ->
                    ( model, loadDatabases )

                _ ->
                    ( { model | databases = Loading }, loadDatabases )

        DatabasesLoaded (Ok dbList) ->
            let
                isInitialLoad =
                    case model.databases of
                        Loading ->
                            True

                        _ ->
                            False

                loadingNow =
                    Set.filter
                        (\name -> not (List.any (\db -> db.name == name && db.status /= Unloaded) dbList.databases))
                        model.loadingDatabases

                -- Trigger page re-init on initial load or when a database just finished loading
                shouldReloadPage =
                    isInitialLoad || Set.size loadingNow < Set.size model.loadingDatabases
            in
            ( { model
                | databases = Loaded dbList
                , authState = Authenticated
                , loadingDatabases = loadingNow
                , loadProgressLines =
                    if Set.size loadingNow < Set.size model.loadingDatabases then
                        []

                    else
                        model.loadProgressLines
              }
            , if shouldReloadPage then
                Nav.replaceUrl model.key (Route.routeToUrl model.currentRoute)

              else
                Cmd.none
            )

        DatabasesLoaded (Err error) ->
            case error of
                Http.BadStatus 401 ->
                    ( { model | authState = NeedsAuth { code = "", error = Nothing }, loadingDatabases = Set.empty, loadProgressLines = [] }
                    , Cmd.none
                    )

                _ ->
                    ( { model | databases = Failed (httpErrorToString error), loadingDatabases = Set.empty, loadProgressLines = [] }
                    , Cmd.none
                    )

        LoadDatabase dbName ->
            ( { model
                | loadingDatabases = Set.insert dbName model.loadingDatabases
                , loadProgressLines = []
                , loadError = Nothing
              }
            , loadDatabaseCmd dbName
            )

        LoadDatabaseResult dbName (Ok response) ->
            case response of
                LoadSucceeded _ _ ->
                    -- Keep dbName in loadingDatabases — DatabasesLoaded will clear it
                    -- once the refreshed list confirms it's loaded
                    ( { model
                        | cachedTrees = Dict.empty
                        , cachedActivityInfo = Dict.empty
                        , cachedInventories = Dict.empty
                        , cachedGraphs = Dict.empty
                      }
                    , loadDatabases
                    )

                LoadFailed err ->
                    ( { model
                        | loadingDatabases = Set.remove dbName model.loadingDatabases
                        , loadProgressLines = []
                        , loadError = Just err
                      }
                    , Cmd.none
                    )

        LoadDatabaseResult dbName (Err err) ->
            ( { model
                | loadingDatabases = Set.remove dbName model.loadingDatabases
                , loadProgressLines = []
                , loadError = Just (httpErrorToString err)
              }
            , Cmd.none
            )

        ToggleConsole ->
            ( { model
                | consoleState =
                    case model.consoleState of
                        Expanded ->
                            Closed

                        _ ->
                            Expanded
              }
            , Cmd.none
            )

        CloseConsole ->
            ( { model | consoleState = Closed }, Cmd.none )

        BackendProgressLines lines ->
            if List.isEmpty lines then
                ( model, Cmd.none )

            else
                let
                    startingCount =
                        List.length (List.filter (String.contains "[STARTING]") lines)

                    okCount =
                        List.length (List.filter (String.contains "[OK]") lines)

                    newOps =
                        max 0 (model.activeOperations + startingCount - okCount)
                in
                ( { model
                    | loadProgressLines = model.loadProgressLines ++ lines
                    , activeOperations = newOps
                  }
                , Cmd.none
                )

        ClearProgress ->
            ( model, Cmd.none )

        CacheTree activityId tree ->
            ( { model | cachedTrees = Dict.insert activityId tree model.cachedTrees }
            , Cmd.none
            )

        CacheActivityInfo activityId info ->
            ( { model | cachedActivityInfo = Dict.insert activityId info model.cachedActivityInfo }
            , Cmd.none
            )

        CacheInventory activityId inventory ->
            ( { model | cachedInventories = Dict.insert activityId inventory model.cachedInventories }
            , Cmd.none
            )

        CacheGraph activityId graphData ->
            ( { model | cachedGraphs = Dict.insert activityId graphData model.cachedGraphs }
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        CloseMenu ->
            ( { model | menuOpen = False }, Cmd.none )

        PushActivity dbName activityId ->
            ( { model | activityStack = ( dbName, activityId ) :: model.activityStack }
            , Cmd.none
            )

        NavigateBackToParent ->
            case model.activityStack of
                ( dbName, activityId ) :: rest ->
                    ( { model | activityStack = rest }
                    , Nav.pushUrl model.key (Route.routeToUrl (Route.withActivity dbName activityId model.currentRoute))
                    )

                [] ->
                    let
                        activitiesRoute =
                            case model.lastActivitiesRoute of
                                Just flags ->
                                    ActivitiesRoute flags

                                Nothing ->
                                    case Route.routeToDatabase model.currentRoute of
                                        Just db ->
                                            ActivitiesRoute { db = db, name = Nothing, limit = Just 20 }

                                        Nothing ->
                                            DatabasesRoute
                    in
                    ( model, Nav.pushUrl model.key (Route.routeToUrl activitiesRoute) )

        UpdateAuthCode code ->
            case model.authState of
                NeedsAuth state ->
                    ( { model | authState = NeedsAuth { state | code = code } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitAuthCode ->
            case model.authState of
                NeedsAuth state ->
                    ( model, postAuthCode state.code )

                _ ->
                    ( model, Cmd.none )

        AuthResult (Ok _) ->
            -- Cookie is now set, reload databases
            ( { model | authState = AuthChecking, databases = Loading }
            , loadDatabases
            )

        AuthResult (Err _) ->
            case model.authState of
                NeedsAuth state ->
                    ( { model | authState = NeedsAuth { state | error = Just "Invalid code" } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onBackendProgress BackendProgressLines
        , case model.consoleState of
            Expanded ->
                Browser.Events.onKeyDown
                    (Json.Decode.field "key" Json.Decode.string
                        |> Json.Decode.andThen
                            (\key ->
                                if key == "Escape" then
                                    Json.Decode.succeed CloseConsole

                                else
                                    Json.Decode.fail ""
                            )
                    )

            Closed ->
                Sub.none
        ]


{-| Check if a database is fully loaded (resolved all links)
-}
isDatabaseLoaded : Model -> String -> Bool
isDatabaseLoaded model dbName =
    case model.databases of
        Loaded dbList ->
            List.any (\db -> db.name == dbName && db.status == DbLoaded) dbList.databases

        _ ->
            False


{-| Get the display name for a database, falling back to the slug
-}
getDatabaseDisplayName : Model -> String -> String
getDatabaseDisplayName model dbName =
    case model.databases of
        Loaded dbList ->
            List.filter (\db -> db.name == dbName) dbList.databases
                |> List.head
                |> Maybe.map .displayName
                |> Maybe.withDefault dbName

        _ ->
            dbName


isDatabaseLoading : Model -> String -> Bool
isDatabaseLoading model dbName =
    Set.member dbName model.loadingDatabases


viewLoadDatabasePrompt : Model -> String -> msg -> Html msg
viewLoadDatabasePrompt model dbName loadMsg =
    if String.isEmpty dbName then
        Html.div [ Html.Attributes.class "notification is-warning", Html.Attributes.style "margin" "2rem" ]
            [ Html.p [] [ Html.text "No database selected." ]
            , Html.a
                [ Html.Attributes.href (Route.routeToUrl DatabasesRoute)
                , Html.Attributes.class "button is-primary"
                , Html.Attributes.style "margin-top" "1rem"
                ]
                [ Html.text "Go to Databases" ]
            ]

    else
        let
            loading =
                isDatabaseLoading model dbName

            displayName =
                getDatabaseDisplayName model dbName
        in
        Html.div [ Html.Attributes.class "notification is-warning", Html.Attributes.style "margin" "2rem" ]
            [ Html.p [] [ Html.text ("Database '" ++ displayName ++ "' is not loaded.") ]
            , Html.button
                [ Html.Attributes.class
                    ("button is-primary"
                        ++ (if loading then
                                " is-loading"

                            else
                                ""
                           )
                    )
                , Html.Attributes.style "margin-top" "1rem"
                , Html.Attributes.disabled loading
                , Html.Events.onClick loadMsg
                ]
                [ Html.text
                    (if loading then
                        "Loading..."

                     else
                        "Load " ++ displayName
                    )
                ]
            ]



-- HTTP Helpers


loadDatabases : Cmd Msg
loadDatabases =
    Http.get
        { url = "/api/v1/db"
        , expect = Http.expectJson DatabasesLoaded databaseListDecoder
        }


loadDatabaseCmd : String -> Cmd Msg
loadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/load"
        , body = Http.emptyBody
        , expect = Http.expectJson (LoadDatabaseResult dbName) loadDatabaseResponseDecoder
        }


postAuthCode : String -> Cmd Msg
postAuthCode code =
    Http.post
        { url = "/api/v1/auth"
        , body = Http.jsonBody (Json.Encode.object [ ( "code", Json.Encode.string code ) ])
        , expect = Http.expectWhatever AuthResult
        }



httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body

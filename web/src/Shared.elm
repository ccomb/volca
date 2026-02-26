module Shared exposing
    ( Model
    , Msg(..)
    , AuthState(..)
    , RemoteData(..)
    , ConsoleVisibility(..)
    , init
    , update
    , subscriptions
    , isDatabaseLoaded
    , isDatabaseLoading
    , getDatabaseDisplayName
    , httpErrorToString
    , viewLoadDatabasePrompt
    )

import Browser.Dom
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
import Models.Database exposing (DatabaseList, LoadDatabaseResponse(..), databaseListDecoder, loadDatabaseResponseDecoder)
import Models.Graph exposing (GraphData)
import Models.Inventory exposing (InventoryExport)
import Route exposing (Route(..))
import Set exposing (Set)
import Task
import Time


type RemoteData a
    = NotAsked
    | Loading
    | Loaded a
    | Failed String


type AuthState
    = AuthChecking
    | NeedsAuth { code : String, error : Maybe String }
    | Authenticated


type alias ConsoleModel =
    { logs : List String
    , nextIndex : Int
    , visibility : ConsoleVisibility
    }


type ConsoleVisibility
    = Hidden
    | Visible { atBottom : Bool }


type alias Model =
    { key : Nav.Key
    , currentRoute : Route
    , lastActivitiesRoute : Maybe Route.ActivitiesFlags
    , databases : RemoteData DatabaseList
    , version : String
    , console : ConsoleModel
    , menuOpen : Bool
    , cachedTrees : Dict String ActivityTree
    , cachedActivityInfo : Dict String ActivityInfo
    , cachedInventories : Dict String InventoryExport
    , cachedGraphs : Dict String GraphData
    , loadingDatabases : Set String
    , authState : AuthState
    }


type Msg
    = RouteChanged Route
    | NavigateTo Route
    | LoadDatabases
    | DatabasesLoaded (Result Http.Error DatabaseList)
    | LoadDatabase String
    | LoadDatabaseResult String (Result Http.Error LoadDatabaseResponse)
    | ToggleConsole
    | CloseConsole
    | PollConsoleLogs Time.Posix
    | ConsoleLogsLoaded (Result Http.Error { lines : List String, nextIndex : Int })
    | ConsoleScrolled Bool
    | CacheTree String ActivityTree
    | CacheActivityInfo String ActivityInfo
    | CacheInventory String InventoryExport
    | CacheGraph String GraphData
    | ToggleMenu
    | CloseMenu
    | UpdateAuthCode String
    | SubmitAuthCode
    | AuthResult (Result Http.Error ())
    | NoOp


init : { version : String } -> Nav.Key -> ( Model, Cmd Msg )
init flags key =
    ( { key = key
      , currentRoute = RootRoute
      , lastActivitiesRoute = Nothing
      , databases = Loading
      , version = flags.version
      , console =
            { logs = []
            , nextIndex = 0
            , visibility = Hidden
            }
      , menuOpen = False
      , cachedTrees = Dict.empty
      , cachedActivityInfo = Dict.empty
      , cachedInventories = Dict.empty
      , cachedGraphs = Dict.empty
      , loadingDatabases = Set.empty
      , authState = AuthChecking
      }
    , loadDatabases
    )


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

                -- Databases that are now confirmed loaded — remove from loading set
                stillLoading =
                    Set.filter
                        (\name -> not (List.any (\db -> db.name == name && db.loaded) dbList.databases))
                        model.loadingDatabases

                -- Trigger page re-init on initial load or when a database just finished loading
                shouldReloadPage =
                    isInitialLoad || Set.size stillLoading < Set.size model.loadingDatabases
            in
            ( { model
                | databases = Loaded dbList
                , authState = Authenticated
                , loadingDatabases = stillLoading
              }
            , if shouldReloadPage then
                Nav.replaceUrl model.key (Route.routeToUrl model.currentRoute)

              else
                Cmd.none
            )

        DatabasesLoaded (Err error) ->
            case error of
                Http.BadStatus 401 ->
                    ( { model | authState = NeedsAuth { code = "", error = Nothing }, loadingDatabases = Set.empty }
                    , Cmd.none
                    )

                _ ->
                    ( { model | databases = Failed (httpErrorToString error), loadingDatabases = Set.empty }
                    , Cmd.none
                    )

        LoadDatabase dbName ->
            ( { model | loadingDatabases = Set.insert dbName model.loadingDatabases }
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

                LoadFailed _ ->
                    ( { model | loadingDatabases = Set.remove dbName model.loadingDatabases }
                    , Cmd.none
                    )

        LoadDatabaseResult dbName (Err _) ->
            ( { model | loadingDatabases = Set.remove dbName model.loadingDatabases }
            , Cmd.none
            )

        ToggleConsole ->
            let
                newConsole =
                    model.console

                isVisible =
                    case newConsole.visibility of
                        Visible _ ->
                            True

                        Hidden ->
                            False
            in
            if isVisible then
                ( { model | console = { newConsole | visibility = Hidden } }
                , Cmd.none
                )

            else
                ( { model | console = { newConsole | visibility = Visible { atBottom = True } } }
                , Cmd.batch [ loadConsoleLogs newConsole.nextIndex, scrollConsoleToBottom ]
                )

        CloseConsole ->
            let
                console =
                    model.console
            in
            ( { model | console = { console | visibility = Hidden } }
            , Cmd.none
            )

        PollConsoleLogs _ ->
            ( model, loadConsoleLogs model.console.nextIndex )

        ConsoleLogsLoaded (Ok result) ->
            let
                console =
                    model.console

                isAtBottom =
                    case console.visibility of
                        Visible visibleState ->
                            visibleState.atBottom

                        Hidden ->
                            True
            in
            ( { model
                | console =
                    { console
                        | logs = console.logs ++ result.lines
                        , nextIndex = result.nextIndex
                    }
              }
            , if isAtBottom && not (List.isEmpty result.lines) then
                scrollConsoleToBottom

              else
                Cmd.none
            )

        ConsoleLogsLoaded (Err _) ->
            ( model, Cmd.none )

        ConsoleScrolled atBottom ->
            let
                console =
                    model.console
            in
            ( { model
                | console =
                    { console
                        | visibility = Visible { atBottom = atBottom }
                    }
              }
            , Cmd.none
            )

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
    case model.console.visibility of
        Visible _ ->
            Sub.batch
                [ Time.every 2000 PollConsoleLogs
                , Browser.Events.onKeyDown
                    (Json.Decode.field "key" Json.Decode.string
                        |> Json.Decode.andThen
                            (\key ->
                                if key == "Escape" then
                                    Json.Decode.succeed CloseConsole

                                else
                                    Json.Decode.fail "not Escape"
                            )
                    )
                ]

        Hidden ->
            Sub.none


{-| Check if a database is currently loaded
-}
isDatabaseLoaded : Model -> String -> Bool
isDatabaseLoaded model dbName =
    case model.databases of
        Loaded dbList ->
            List.any (\db -> db.name == dbName && db.loaded) dbList.databases

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
        { url = "/api/v1/databases"
        , expect = Http.expectJson DatabasesLoaded databaseListDecoder
        }


loadDatabaseCmd : String -> Cmd Msg
loadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/load"
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


loadConsoleLogs : Int -> Cmd Msg
loadConsoleLogs since =
    Http.get
        { url = "/api/v1/logs?since=" ++ String.fromInt since
        , expect =
            Http.expectJson ConsoleLogsLoaded
                (Json.Decode.map2 (\lines nextIndex -> { lines = lines, nextIndex = nextIndex })
                    (Json.Decode.field "lines" (Json.Decode.list Json.Decode.string))
                    (Json.Decode.field "nextIndex" Json.Decode.int)
                )
        }


scrollConsoleToBottom : Cmd Msg
scrollConsoleToBottom =
    Browser.Dom.getViewportOf "console-log-container"
        |> Task.andThen (\info -> Browser.Dom.setViewportOf "console-log-container" 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


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

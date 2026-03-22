module Pages.DatabaseSetup exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Json.Decode
import Json.Encode
import Models.Database exposing (ActivateResponse, DatabaseSetupInfo, activateResponseDecoder, databaseSetupInfoDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.DatabaseSetupView as DatabaseSetupView


type alias Model =
    { dbName : String
    , setupInfo : RemoteData DatabaseSetupInfo
    }


type Msg
    = SetupInfoLoaded (Result String DatabaseSetupInfo)
    | DatabaseSetupViewMsg DatabaseSetupView.Msg
    | DependencyActionResult (Result Http.Error DatabaseSetupInfo)
    | FinalizeResult (Result Http.Error ActivateResponse)
    | NewFlags String


page : Shared.Model -> Spa.Page.Page String Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> String -> ( Model, Effect Shared.Msg Msg )
init shared dbName =
    ( { dbName = dbName
      , setupInfo = Loading
      }
    , Effect.fromCmd (loadSetupInfo dbName)
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        SetupInfoLoaded (Ok info) ->
            ( { model | setupInfo = Loaded info }
            , Effect.none
            )

        SetupInfoLoaded (Err error) ->
            ( { model | setupInfo = Failed error }
            , Effect.none
            )

        DatabaseSetupViewMsg viewMsg ->
            case viewMsg of
                DatabaseSetupView.AddDependency depName ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (addDependency model.dbName depName)
                    )

                DatabaseSetupView.RemoveDependency depName ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (removeDependency model.dbName depName)
                    )

                DatabaseSetupView.SetDataPath path ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (setDataPath model.dbName path)
                    )

                DatabaseSetupView.FinalizeDatabase ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (finalizeDatabase model.dbName)
                    )

                DatabaseSetupView.GoBack ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.DatabasesRoute))
                    )

                DatabaseSetupView.GoToActivities ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivitiesRoute { db = model.dbName, name = Nothing, limit = Just 20 })))
                    )

                DatabaseSetupView.GoToMapping ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.DatabaseMappingRoute model.dbName Nothing)))
                    )

        DependencyActionResult (Ok info) ->
            ( { model | setupInfo = Loaded info }
            , Effect.none
            )

        DependencyActionResult (Err error) ->
            ( { model | setupInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        FinalizeResult (Ok response) ->
            if response.success then
                ( model
                , Effect.batch
                    [ Effect.fromShared Shared.LoadDatabases
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.DatabasesRoute))
                    ]
                )

            else
                ( { model | setupInfo = Failed response.message }
                , Effect.none
                )

        FinalizeResult (Err error) ->
            ( { model | setupInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        NewFlags flags ->
            init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        ( maybeSetupInfo, error ) =
            case model.setupInfo of
                Loaded info ->
                    ( Just info, Nothing )

                Failed err ->
                    ( Nothing, Just err )

                _ ->
                    ( Nothing, Nothing )
    in
    let
        displayName =
            Shared.getDatabaseDisplayName shared model.dbName
    in
    { title = "Database Setup"
    , body =
        Html.map DatabaseSetupViewMsg
            (DatabaseSetupView.viewDatabaseSetupPage
                displayName
                maybeSetupInfo
                error
                shared.loadProgressLines
            )
    }



-- HTTP


loadSetupInfo : String -> Cmd Msg
loadSetupInfo dbName =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/setup"
        , expect = expectJsonWithBody SetupInfoLoaded databaseSetupInfoDecoder
        }


expectJsonWithBody : (Result String a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
expectJsonWithBody toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Result.mapError Json.Decode.errorToString (Json.Decode.decodeString decoder body)

                Http.BadStatus_ _ body ->
                    Err body

                Http.Timeout_ ->
                    Err "Request timed out"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)


addDependency : String -> String -> Cmd Msg
addDependency dbName depName =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/add-dependency/" ++ depName
        , body = Http.emptyBody
        , expect = Http.expectJson DependencyActionResult databaseSetupInfoDecoder
        }


removeDependency : String -> String -> Cmd Msg
removeDependency dbName depName =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/remove-dependency/" ++ depName
        , body = Http.emptyBody
        , expect = Http.expectJson DependencyActionResult databaseSetupInfoDecoder
        }


setDataPath : String -> String -> Cmd Msg
setDataPath dbName path =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/set-data-path"
        , body = Http.jsonBody (Json.Encode.object [ ( "path", Json.Encode.string path ) ])
        , expect = Http.expectJson DependencyActionResult databaseSetupInfoDecoder
        }


finalizeDatabase : String -> Cmd Msg
finalizeDatabase dbName =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/finalize"
        , body = Http.emptyBody
        , expect = Http.expectJson FinalizeResult activateResponseDecoder
        }



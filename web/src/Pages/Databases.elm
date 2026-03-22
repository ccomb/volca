module Pages.Databases exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Models.Database exposing (ActivateResponse, activateResponseDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.DatabasesView as DatabasesView


type alias Model =
    { pendingAction : Maybe PendingAction
    , actionError : Maybe String
    , confirmingDelete : Maybe String
    }


type PendingAction
    = UnloadingDb String
    | DeletingDb String


type Msg
    = DatabasesViewMsg DatabasesView.Msg
    | ActionResult (Result Http.Error ActivateResponse)


page : Shared.Model -> Spa.Page.Page () Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> () -> ( Model, Effect Shared.Msg Msg )
init shared _ =
    ( { pendingAction = Nothing
      , actionError = Nothing
      , confirmingDelete = Nothing
      }
    , Effect.fromShared Shared.LoadDatabases
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        DatabasesViewMsg dbMsg ->
            case dbMsg of
                DatabasesView.NavigateToDatabase dbName ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivitiesRoute { db = dbName, name = Nothing, limit = Just 20 })))
                    )

                DatabasesView.LoadDatabase dbName ->
                    ( { model | actionError = Nothing }
                    , Effect.fromShared (Shared.LoadDatabase dbName)
                    )

                DatabasesView.UnloadDatabase dbName ->
                    ( { model | pendingAction = Just (UnloadingDb dbName), actionError = Nothing }
                    , Effect.fromCmd (unloadDatabaseCmd dbName)
                    )

                DatabasesView.ConfirmDeleteDatabase dbName ->
                    ( { model | confirmingDelete = Just dbName }
                    , Effect.none
                    )

                DatabasesView.CancelDeleteDatabase ->
                    ( { model | confirmingDelete = Nothing }
                    , Effect.none
                    )

                DatabasesView.DeleteDatabase dbName ->
                    ( { model | pendingAction = Just (DeletingDb dbName), actionError = Nothing }
                    , Effect.fromCmd (deleteDatabaseCmd dbName)
                    )

                DatabasesView.SetupDatabase dbName ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.DatabaseSetupRoute dbName)))
                    )

        ActionResult (Ok response) ->
            if response.success then
                ( { model | pendingAction = Nothing, actionError = Nothing }
                , Effect.fromShared Shared.LoadDatabases
                )

            else
                ( { model | pendingAction = Nothing, actionError = Just response.message }
                , Effect.none
                )

        ActionResult (Err err) ->
            ( { model | pendingAction = Nothing, actionError = Just (Shared.httpErrorToString err) }
            , Effect.none
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        maybeDatabases =
            case shared.databases of
                Loaded dbList ->
                    Just dbList

                _ ->
                    Nothing

        error =
            case ( model.actionError, shared.loadError, shared.databases ) of
                ( Just err, _, _ ) ->
                    Just err

                ( _, Just err, _ ) ->
                    Just err

                ( _, _, Failed err ) ->
                    Just err

                _ ->
                    Nothing

        unloadingDb =
            case model.pendingAction of
                Just (UnloadingDb name) ->
                    Just name

                _ ->
                    Nothing

        deletingDb =
            case model.pendingAction of
                Just (DeletingDb name) ->
                    Just name

                _ ->
                    Nothing
    in
    { title = "Databases"
    , body =
        Html.map DatabasesViewMsg
            (DatabasesView.viewDatabasesPage
                maybeDatabases
                error
                model.confirmingDelete
                shared.loadingDatabases
                unloadingDb
                deletingDb
                shared.loadProgressLines
            )
    }



-- HTTP commands


unloadDatabaseCmd : String -> Cmd Msg
unloadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/db/" ++ dbName ++ "/unload"
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        }


deleteDatabaseCmd : String -> Cmd Msg
deleteDatabaseCmd dbName =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/v1/db/" ++ dbName
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

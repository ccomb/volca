module Pages.Methods exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Models.Database exposing (ActivateResponse, activateResponseDecoder)
import Models.Method exposing (MethodCollectionList, methodCollectionListDecoder)
import Route
import Set exposing (Set)
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.MethodsView as MethodsView


type alias Model =
    { methods : RemoteData MethodCollectionList
    , loadingMethods : Set String
    , pendingAction : Maybe PendingAction
    , confirmingDelete : Maybe String
    , actionError : Maybe String
    }


type PendingAction
    = UnloadingMethod String
    | DeletingMethod String


type Msg
    = MethodsViewMsg MethodsView.Msg
    | MethodsLoaded (Result Http.Error MethodCollectionList)
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
init _ _ =
    ( { methods = Loading
      , loadingMethods = Set.empty
      , pendingAction = Nothing
      , confirmingDelete = Nothing
      , actionError = Nothing
      }
    , Effect.fromCmd fetchMethods
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        MethodsLoaded (Ok methodList) ->
            ( { model
                | methods = Loaded methodList
                , loadingMethods =
                    -- Clear loading state for methods that are now loaded
                    Set.filter
                        (\name ->
                            not
                                (List.any
                                    (\m -> m.name == name && m.status /= Models.Database.Unloaded)
                                    methodList.methods
                                )
                        )
                        model.loadingMethods
              }
            , Effect.none
            )

        MethodsLoaded (Err err) ->
            ( { model | methods = Failed (Shared.httpErrorToString err) }
            , Effect.none
            )

        MethodsViewMsg viewMsg ->
            case viewMsg of
                MethodsView.LoadMethod name ->
                    ( { model | loadingMethods = Set.insert name model.loadingMethods, actionError = Nothing }
                    , Effect.fromCmd (loadMethodCmd name)
                    )

                MethodsView.UnloadMethod name ->
                    ( { model | pendingAction = Just (UnloadingMethod name), actionError = Nothing }
                    , Effect.fromCmd (unloadMethodCmd name)
                    )

                MethodsView.ConfirmDeleteMethod name ->
                    ( { model | confirmingDelete = Just name }, Effect.none )

                MethodsView.CancelDeleteMethod ->
                    ( { model | confirmingDelete = Nothing }, Effect.none )

                MethodsView.DeleteMethod name ->
                    ( { model | pendingAction = Just (DeletingMethod name), actionError = Nothing }
                    , Effect.fromCmd (deleteMethodCmd name)
                    )

                MethodsView.NavigateToMethod name ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.MethodDetailRoute name Nothing)))
                    )

        ActionResult (Ok response) ->
            if response.success then
                ( { model | loadingMethods = Set.empty, pendingAction = Nothing, confirmingDelete = Nothing, actionError = Nothing }
                , Effect.fromCmd fetchMethods
                )

            else
                ( { model | loadingMethods = Set.empty, pendingAction = Nothing, confirmingDelete = Nothing, actionError = Just response.message }
                , Effect.fromCmd fetchMethods
                )

        ActionResult (Err err) ->
            ( { model | loadingMethods = Set.empty, pendingAction = Nothing, confirmingDelete = Nothing, actionError = Just (Shared.httpErrorToString err) }
            , Effect.fromCmd fetchMethods
            )


view : Shared.Model -> Model -> View Msg
view _ model =
    let
        maybeMethods =
            case model.methods of
                Loaded list ->
                    Just list

                _ ->
                    Nothing

        error =
            case ( model.actionError, model.methods ) of
                ( Just err, _ ) ->
                    Just err

                ( _, Failed err ) ->
                    Just err

                _ ->
                    Nothing

        unloadingMethod =
            case model.pendingAction of
                Just (UnloadingMethod name) ->
                    Just name

                _ ->
                    Nothing

        deletingMethod =
            case model.pendingAction of
                Just (DeletingMethod name) ->
                    Just name

                _ ->
                    Nothing
    in
    { title = "Methods"
    , body =
        Html.map MethodsViewMsg
            (MethodsView.viewMethodsPage
                maybeMethods
                error
                model.confirmingDelete
                model.loadingMethods
                unloadingMethod
                deletingMethod
            )
    }



-- HTTP


fetchMethods : Cmd Msg
fetchMethods =
    Http.get
        { url = "/api/v1/method-collections"
        , expect = Http.expectJson MethodsLoaded methodCollectionListDecoder
        }


loadMethodCmd : String -> Cmd Msg
loadMethodCmd name =
    Http.post
        { url = "/api/v1/method-collections/" ++ name ++ "/load"
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        }


unloadMethodCmd : String -> Cmd Msg
unloadMethodCmd name =
    Http.post
        { url = "/api/v1/method-collections/" ++ name ++ "/unload"
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        }


deleteMethodCmd : String -> Cmd Msg
deleteMethodCmd name =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/v1/method-collections/" ++ name
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

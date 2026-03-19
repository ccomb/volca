module Pages.SupplyChainGraph exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Models.Activity exposing (ActivityTree)
import Models.SupplyChain exposing (SupplyChainResponse, supplyChainResponseDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.SupplyChainGraphView as SCGraphView
import Api


type alias Model =
    { activityId : String
    , dbName : String
    , state : PageState
    }


type PageState
    = Loading
    | Ready SCGraphView.Model
    | Failed String


type Msg
    = DataLoaded (Result Http.Error SupplyChainResponse)
    | TreeLoaded (Result Http.Error ActivityTree)
    | GraphViewMsg SCGraphView.Msg
    | RequestLoadDatabase
    | NewFlags ( String, String )


page : Shared.Model -> Spa.Page.Page ( String, String ) Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = subscriptions
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> ( String, String ) -> ( Model, Effect Shared.Msg Msg )
init shared ( db, activityId ) =
    if not (Shared.isDatabaseLoaded shared db) then
        ( { activityId = activityId, dbName = db, state = Loading }
        , Effect.none
        )

    else
        ( { activityId = activityId, dbName = db, state = Loading }
        , Effect.batch
            [ Effect.fromCmd (loadSupplyChain db activityId)
            , case Dict.get activityId shared.cachedTrees of
                Nothing ->
                    Effect.fromCmd (Api.loadActivityTree TreeLoaded db activityId)

                Just _ ->
                    Effect.none
            ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update _ msg model =
    case msg of
        DataLoaded (Ok response) ->
            ( { model | state = Ready (SCGraphView.init response) }
            , Effect.none
            )

        DataLoaded (Err error) ->
            ( { model | state = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        TreeLoaded (Ok tree) ->
            ( model
            , Effect.fromShared (Shared.CacheTree model.activityId tree)
            )

        TreeLoaded (Err _) ->
            ( model, Effect.none )

        GraphViewMsg graphMsg ->
            case model.state of
                Ready graphModel ->
                    ( { model | state = Ready (SCGraphView.update graphMsg graphModel) }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags _ ->
            -- Page will be re-initialized by elm-spa framework
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Ready graphModel ->
            Sub.map GraphViewMsg (SCGraphView.subscriptions graphModel)

        _ ->
            Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "Supply Chain Graph"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        { title = "Supply Chain Graph"
        , body =
            div [ class "graph-page" ]
                [ viewNavbar shared model
                , case model.state of
                    Loading ->
                        div [ class "has-text-centered" ]
                            [ div [ class "is-size-3" ] [ text "Loading supply chain..." ]
                            , progress [ class "progress is-primary", attribute "max" "100" ] []
                            ]

                    Failed error ->
                        div [ class "notification is-danger" ]
                            [ strong [] [ text "Error: " ]
                            , text error
                            ]

                    Ready graphModel ->
                        Html.map GraphViewMsg (SCGraphView.view graphModel)
                ]
        }


viewNavbar : Shared.Model -> Model -> Html Msg
viewNavbar shared model =
    let
        activityName =
            Dict.get model.activityId shared.cachedTrees
                |> Maybe.andThen (\tree -> Dict.get model.activityId tree.nodes)
                |> Maybe.map .name
    in
    nav [ class "navbar is-light", style "height" "52px" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ h1 [ class "title is-4" ] [ text "Supply Chain Graph" ] ]
            ]
        , div [ class "navbar-menu is-active" ]
            [ div [ class "navbar-end" ]
                (case activityName of
                    Just name ->
                        [ div [ class "navbar-item" ] [ span [ class "title is-4" ] [ text name ] ] ]

                    Nothing ->
                        []
                )
            ]
        ]


loadSupplyChain : String -> String -> Cmd Msg
loadSupplyChain dbName activityId =
    Http.get
        { url = "/api/v1/database/" ++ dbName ++ "/activity/" ++ activityId ++ "/supply-chain?limit=50"
        , expect = Http.expectJson DataLoaded supplyChainResponseDecoder
        }

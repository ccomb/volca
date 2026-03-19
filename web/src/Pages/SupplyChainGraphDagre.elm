module Pages.SupplyChainGraphDagre exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.SupplyChain exposing (SupplyChainResponse, supplyChainResponseDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.SupplyChainGraphViewDagre as DagreView


type alias Model =
    { activityId : String
    , dbName : String
    , state : PageState
    , cutoffInput : String
    }


type PageState
    = Loading
    | Ready DagreView.Model
    | Failed String


type Msg
    = DataLoaded (Result Http.Error SupplyChainResponse)
    | DagreMsg DagreView.Msg
    | UpdateCutoff String
    | Reload
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
    let
        cutoff =
            "1.0"

        model =
            { activityId = activityId
            , dbName = db
            , state = Loading
            , cutoffInput = cutoff
            }
    in
    if not (Shared.isDatabaseLoaded shared db) then
        ( model, Effect.none )

    else
        ( model
        , Effect.fromCmd (loadSupplyChain db activityId 1.0)
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        DataLoaded (Ok response) ->
            ( { model | state = Ready (DagreView.init response) }
            , Effect.none
            )

        DataLoaded (Err error) ->
            ( { model | state = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        DagreMsg dagreMsg ->
            case model.state of
                Ready dagreModel ->
                    ( { model | state = Ready (DagreView.update dagreMsg dagreModel) }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        UpdateCutoff str ->
            ( { model | cutoffInput = str }, Effect.none )

        Reload ->
            let
                cutoff =
                    String.toFloat model.cutoffInput |> Maybe.withDefault 1.0
            in
            ( { model | state = Loading }
            , Effect.fromCmd (loadSupplyChain model.dbName model.activityId cutoff)
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            init shared flags


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Ready dagreModel ->
            Sub.map DagreMsg (DagreView.subscriptions dagreModel)

        _ ->
            Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "SC Dagre"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        { title = "SC Dagre"
        , body =
            div [ class "graph-page" ]
                [ viewNavbar
                , viewControls model
                , viewBody model
                ]
        }


viewNavbar : Html Msg
viewNavbar =
    nav [ class "navbar is-light", style "height" "52px" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ h1 [ class "title is-4" ] [ text "Supply Chain (Dagre Layout)" ] ]
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ style "height" "90px" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ Html.form
                        [ class "field has-addons"
                        , onSubmit Reload
                        ]
                        [ div [ class "control" ]
                            [ label [ class "label" ] [ text "Min quantity" ]
                            , input
                                [ class "input"
                                , type_ "text"
                                , value model.cutoffInput
                                , onInput UpdateCutoff
                                , placeholder "e.g., 0.001"
                                ]
                                []
                            ]
                        , div [ class "control" ]
                            [ button
                                [ class "button is-primary"
                                , type_ "submit"
                                , style "margin-top" "1.5rem"
                                ]
                                [ text "Reload" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.state of
        Loading ->
            div [ class "has-text-centered" ]
                [ div [ class "is-size-3" ] [ text "Loading supply chain..." ]
                , progress [ class "progress is-primary", attribute "max" "100" ] []
                ]

        Failed error ->
            div [ class "notification is-danger" ]
                [ button [ class "delete", onClick Reload ] []
                , strong [] [ text "Error: " ]
                , text error
                ]

        Ready dagreModel ->
            Html.map DagreMsg (DagreView.view dagreModel)


loadSupplyChain : String -> String -> Float -> Cmd Msg
loadSupplyChain dbName activityId minQuantity =
    Http.get
        { url =
            "/api/v1/database/"
                ++ dbName
                ++ "/activity/"
                ++ activityId
                ++ "/supply-chain?min-quantity="
                ++ String.fromFloat minQuantity
        , expect = Http.expectJson DataLoaded supplyChainResponseDecoder
        }

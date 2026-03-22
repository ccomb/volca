module Pages.Graph exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Api
import Models.Activity exposing (ActivityTree)
import Models.Graph exposing (GraphData, graphDataDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.GraphView as GraphView


type alias Model =
    { activityId : String
    , dbName : String
    , graphState : GraphState
    , cutoffInput : String
    }


type GraphState
    = GraphLoading
    | GraphReady GraphView.Model
    | GraphFailed String


type Msg
    = GraphLoaded (Result Http.Error GraphData)
    | TreeLoaded (Result Http.Error ActivityTree)
    | GraphViewMsg GraphView.Msg
    | UpdateCutoff String
    | ReloadGraph
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
        ( { activityId = activityId
          , dbName = db
          , graphState = GraphLoading
          , cutoffInput = "1.0"
          }
        , Effect.none
        )

    else
        let
            cached =
                Dict.get activityId shared.cachedGraphs
        in
        ( { activityId = activityId
          , dbName = db
          , graphState =
                case cached of
                    Just graphData ->
                        GraphReady (GraphView.init graphData)

                    Nothing ->
                        GraphLoading
          , cutoffInput = "1.0"
          }
        , case cached of
            Just _ ->
                Effect.none

            Nothing ->
                let
                    graphCmd =
                        Effect.fromCmd (loadGraphData db activityId 1.0)

                    treeCmd =
                        case Dict.get activityId shared.cachedTrees of
                            Nothing ->
                                Effect.fromCmd (Api.loadActivityTree TreeLoaded db activityId)

                            Just _ ->
                                Effect.none
                in
                Effect.batch [ graphCmd, treeCmd ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        GraphLoaded (Ok graphData) ->
            ( { model | graphState = GraphReady (GraphView.init graphData) }
            , Effect.fromShared (Shared.CacheGraph model.activityId graphData)
            )

        GraphLoaded (Err error) ->
            ( { model | graphState = GraphFailed (Shared.httpErrorToString error) }
            , Effect.none
            )

        TreeLoaded (Ok tree) ->
            ( model
            , Effect.fromShared (Shared.CacheTree model.activityId tree)
            )

        TreeLoaded (Err _) ->
            ( model, Effect.none )

        GraphViewMsg graphMsg ->
            case model.graphState of
                GraphReady graphModel ->
                    ( { model | graphState = GraphReady (GraphView.update graphMsg graphModel) }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        UpdateCutoff cutoffStr ->
            ( { model | cutoffInput = cutoffStr }, Effect.none )

        ReloadGraph ->
            let
                cutoff =
                    String.toFloat model.cutoffInput |> Maybe.withDefault 1.0
            in
            ( { model | graphState = GraphLoading }
            , Effect.fromCmd (loadGraphData model.dbName model.activityId cutoff)
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            init shared flags


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.graphState of
        GraphReady graphModel ->
            Sub.map GraphViewMsg (GraphView.subscriptions graphModel)

        _ ->
            Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "Graph"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        viewLoaded shared model


viewLoaded : Shared.Model -> Model -> View Msg
viewLoaded shared model =
    let
        activityInfo =
            Dict.get model.activityId shared.cachedGraphs
                |> Maybe.andThen
                    (\graphData ->
                        List.filter (\node -> node.processId == model.activityId) graphData.nodes
                            |> List.head
                            |> Maybe.map (\node -> ( node.label, node.location ))
                    )
                |> (\maybeInfo ->
                        case maybeInfo of
                            Just info ->
                                Just info

                            Nothing ->
                                Dict.get model.activityId shared.cachedTrees
                                    |> Maybe.andThen (\tree -> Dict.get model.activityId tree.nodes)
                                    |> Maybe.map (\node -> ( node.name, node.location ))
                   )
    in
    { title = "Graph"
    , body =
        div [ class "graph-page" ]
            [ viewPageNavbar "Activity Network Graph" activityInfo
            , div [ style "height" "90px" ]
                [ div []
                    [ div [ class "level" ]
                        [ div [ class "level-left" ]
                            [ div [ class "level-item" ]
                                [ Html.form
                                    [ class "field has-addons"
                                    , onSubmit ReloadGraph
                                    ]
                                    [ div [ class "control" ]
                                        [ label [ class "label" ] [ text "Cutoff (%)" ]
                                        , input
                                            [ class "input"
                                            , type_ "text"
                                            , value model.cutoffInput
                                            , onInput UpdateCutoff
                                            , placeholder "e.g., 0.1, 1.0, 5.0"
                                            ]
                                            []
                                        ]
                                    , div [ class "control" ]
                                        [ button
                                            [ class "button is-primary"
                                            , type_ "submit"
                                            , style "margin-top" "1.5rem"
                                            ]
                                            [ text "Reload Graph" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div []
                [ div []
                    [ case model.graphState of
                        GraphLoading ->
                            div [ class "has-text-centered" ]
                                [ div [ class "is-size-3" ] [ text "Loading graph..." ]
                                , progress [ class "progress is-primary", attribute "max" "100" ] []
                                ]

                        GraphFailed error ->
                            div [ class "notification is-danger" ]
                                [ button [ class "delete", onClick ReloadGraph ] []
                                , strong [] [ text "Error: " ]
                                , text error
                                ]

                        GraphReady graphModel ->
                            Html.map GraphViewMsg (GraphView.view model.activityId graphModel)
                    ]
                ]
            ]
    }


viewPageNavbar : String -> Maybe ( String, String ) -> Html Msg
viewPageNavbar title maybeActivity =
    nav [ class "navbar is-light", style "height" "52px" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ h1 [ class "title is-4" ] [ text title ] ]
            ]
        , div [ class "navbar-menu is-active" ]
            [ div [ class "navbar-end" ]
                (case maybeActivity of
                    Just ( name, location ) ->
                        [ div [ class "navbar-item" ] [ span [ class "title is-4" ] [ text name ] ]
                        , div [ class "navbar-item" ] [ span [ class "subtitle is-6" ] [ text location ] ]
                        ]

                    Nothing ->
                        [ div [ class "navbar-item" ] [ span [ class "subtitle is-6" ] [ text "Loading..." ] ] ]
                )
            ]
        ]


loadGraphData : String -> String -> Float -> Cmd Msg
loadGraphData dbName activityId cutoff =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/graph?cutoff=" ++ String.fromFloat cutoff
        , expect = Http.expectJson GraphLoaded graphDataDecoder
        }

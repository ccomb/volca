module Pages.Tree exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Api
import Models.Activity exposing (ActivityTree)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.TreeView as TreeView


type alias Model =
    { activityId : String
    , dbName : String
    , tree : TreeState
    }


type TreeState
    = TreeLoading
    | TreeReady
        { treeData : ActivityTree
        , viewModel : TreeView.Model
        }
    | TreeFailed String


type Msg
    = TreeLoaded (Result Http.Error ActivityTree)
    | TreeViewMsg TreeView.Msg
    | NavigateToParent
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
          , tree = TreeLoading
          }
        , Effect.none
        )

    else
        let
            cached =
                Dict.get activityId shared.cachedTrees
        in
        ( { activityId = activityId
          , dbName = db
          , tree =
                case cached of
                    Just treeData ->
                        TreeReady
                            { treeData = treeData
                            , viewModel = TreeView.init treeData
                            }

                    Nothing ->
                        TreeLoading
          }
        , case cached of
            Just _ ->
                Effect.none

            Nothing ->
                Effect.fromCmd (Api.loadActivityTree TreeLoaded db activityId)
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        TreeLoaded (Ok tree) ->
            ( { model
                | tree =
                    TreeReady
                        { treeData = tree
                        , viewModel = TreeView.init tree
                        }
              }
            , Effect.fromShared (Shared.CacheTree model.activityId tree)
            )

        TreeLoaded (Err error) ->
            ( { model | tree = TreeFailed (Shared.httpErrorToString error) }
            , Effect.none
            )

        TreeViewMsg treeMsg ->
            case model.tree of
                TreeReady ready ->
                    let
                        updatedViewModel =
                            TreeView.update treeMsg ready.viewModel
                    in
                    case treeMsg of
                        TreeView.NodeDoubleClick nodeId ->
                            case Dict.get nodeId ready.viewModel.idMapping of
                                Just processId ->
                                    if processId /= model.activityId then
                                        ( { model | tree = TreeReady { ready | viewModel = updatedViewModel } }
                                        , Effect.batch
                                            [ Effect.fromShared (Shared.PushActivity model.dbName model.activityId)
                                            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Tree model.dbName processId)))
                                            ]
                                        )

                                    else
                                        ( { model | tree = TreeReady { ready | viewModel = updatedViewModel } }
                                        , Effect.none
                                        )

                                Nothing ->
                                    ( { model | tree = TreeReady { ready | viewModel = updatedViewModel } }
                                    , Effect.none
                                    )

                        _ ->
                            ( { model | tree = TreeReady { ready | viewModel = updatedViewModel } }
                            , Effect.none
                            )

                _ ->
                    ( model, Effect.none )

        NavigateToParent ->
            case model.tree of
                TreeReady ready ->
                    case Dict.get model.activityId ready.treeData.nodes of
                        Just currentNode ->
                            case currentNode.parentId of
                                Just parentId ->
                                    ( model
                                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Tree model.dbName parentId)))
                                    )

                                Nothing ->
                                    ( model
                                    , Effect.fromCmd (Nav.back shared.key 1)
                                    )

                        Nothing ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            init shared flags


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tree of
        TreeReady ready ->
            Sub.map TreeViewMsg (TreeView.subscriptions ready.viewModel)

        _ ->
            Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Tree"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        let
            activityInfo =
                case model.tree of
                    TreeReady ready ->
                        Dict.get model.activityId ready.treeData.nodes
                            |> Maybe.map (\node -> ( node.name, node.location ))

                    _ ->
                        Nothing
        in
        div [ class "tree-page" ]
            [ viewPageNavbar "Activity Tree Navigator" activityInfo
            , div []
                [ div []
                    [ case model.tree of
                        TreeLoading ->
                            div [ class "has-text-centered" ]
                                [ div [ class "is-size-3" ] [ text "Loading..." ]
                                , progress [ class "progress is-primary", attribute "max" "100" ] []
                                ]

                        TreeFailed error ->
                            div [ class "notification is-danger" ]
                                [ button [ class "delete", onClick (TreeLoaded (Err Http.NetworkError)) ] []
                                , strong [] [ text "Error: " ]
                                , text error
                                ]

                        TreeReady ready ->
                            let
                                canGoUp =
                                    case Dict.get model.activityId ready.treeData.nodes of
                                        Just node ->
                                            node.parentId /= Nothing

                                        Nothing ->
                                            False
                            in
                            div []
                                [ div [ class "level", style "height" "70px" ]
                                    [ div [ class "level-left" ]
                                        [ div [ class "level-item" ]
                                            [ button
                                                [ class "button is-primary"
                                                , onClick NavigateToParent
                                                , disabled (not canGoUp)
                                                ]
                                                [ span [ class "icon" ] [ Html.i [ class "fas fa-arrow-up" ] [] ]
                                                , span [] [ text "Parent Activity" ]
                                                ]
                                            ]
                                        ]
                                    , div [ class "level-right" ]
                                        [ div [ class "level-item" ]
                                            [ text ("Total nodes: " ++ String.fromInt ready.treeData.tree.totalNodes) ]
                                        , div [ class "level-item" ]
                                            [ text ("Max depth: " ++ String.fromInt ready.treeData.tree.maxDepth) ]
                                        , div [ class "level-item" ]
                                            [ text ("Expandable: " ++ String.fromInt ready.treeData.tree.expandableNodes) ]
                                        ]
                                    ]
                                , Html.map TreeViewMsg (TreeView.view model.activityId ready.viewModel)
                                ]
                    ]
                ]
            ]


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

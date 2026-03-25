module Pages.FlowSearch exposing (Model, Msg, page)

import Api
import Browser.Dom
import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.Activity exposing (ActivitySummary, SearchResults)
import Models.Flow exposing (FlowSearchResult)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import Task
import Url.Builder
import View exposing (View)


type SearchState
    = NotSearched
    | Searching
    | Results (SearchResults FlowSearchResult)
    | LoadingMore (SearchResults FlowSearchResult)
    | SearchFailed String


type alias Model =
    { dbName : String
    , query : String
    , results : SearchState
    , selectedFlow : Maybe FlowSearchResult
    , flowActivities : RemoteData (List ActivitySummary)
    }


type Msg
    = UpdateQuery String
    | SearchResultsLoaded (Result Http.Error (SearchResults FlowSearchResult))
    | MoreResultsLoaded (Result Http.Error (SearchResults FlowSearchResult))
    | SelectFlow FlowSearchResult
    | DeselectFlow
    | FlowActivitiesLoaded (Result Http.Error (List ActivitySummary))
    | LoadMore
    | NavigateToActivity String
    | RequestLoadDatabase
    | NewFlags Route.FlowSearchFlags
    | NoOp


page : Shared.Model -> Spa.Page.Page Route.FlowSearchFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> Route.FlowSearchFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        query =
            Maybe.withDefault "" flags.q

        dbLoaded =
            Shared.isDatabaseLoaded shared flags.db

        shouldSearch =
            not (String.isEmpty query) && dbLoaded
    in
    ( { dbName = flags.db
      , query = query
      , results =
            if shouldSearch then
                Searching

            else
                NotSearched
      , selectedFlow = Nothing
      , flowActivities = NotAsked
      }
    , Effect.batch
        [ if shouldSearch then
            Effect.fromCmd (Api.searchFlows SearchResultsLoaded flags.db query 20 0)

          else
            Effect.none
        , Effect.fromCmd (Browser.Dom.focus "flow-search" |> Task.attempt (\_ -> NoOp))
        ]
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        UpdateQuery query ->
            let
                hasQuery =
                    not (String.isEmpty query)

                newRoute =
                    Route.FlowSearchRoute
                        { db = model.dbName
                        , q =
                            if hasQuery then
                                Just query

                            else
                                Nothing
                        , limit = Just 20
                        , offset = Nothing
                        }
            in
            ( { model
                | query = query
                , results =
                    if hasQuery then
                        Searching

                    else
                        NotSearched
                , selectedFlow = Nothing
                , flowActivities = NotAsked
              }
            , Effect.batch
                [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                , if hasQuery && Shared.isDatabaseLoaded shared model.dbName then
                    Effect.fromCmd (Api.searchFlows SearchResultsLoaded model.dbName query 20 0)

                  else
                    Effect.none
                ]
            )

        SearchResultsLoaded (Ok results) ->
            ( { model | results = Results results }, Effect.none )

        SearchResultsLoaded (Err error) ->
            ( { model | results = SearchFailed (Shared.httpErrorToString error) }, Effect.none )

        MoreResultsLoaded (Ok newResults) ->
            case model.results of
                LoadingMore existing ->
                    ( { model
                        | results =
                            Results
                                { existing
                                    | results = existing.results ++ newResults.results
                                    , offset = newResults.offset
                                    , hasMore = newResults.hasMore
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        MoreResultsLoaded (Err _) ->
            case model.results of
                LoadingMore existing ->
                    ( { model | results = Results existing }, Effect.none )

                _ ->
                    ( model, Effect.none )

        SelectFlow flow ->
            ( { model | selectedFlow = Just flow, flowActivities = Loading }
            , Effect.fromCmd (Api.loadFlowActivities FlowActivitiesLoaded model.dbName flow.id)
            )

        DeselectFlow ->
            ( { model | selectedFlow = Nothing, flowActivities = NotAsked }, Effect.none )

        FlowActivitiesLoaded (Ok activities) ->
            ( { model | flowActivities = Loaded activities }, Effect.none )

        FlowActivitiesLoaded (Err error) ->
            ( { model | flowActivities = Failed (Shared.httpErrorToString error) }, Effect.none )

        LoadMore ->
            case model.results of
                Results results ->
                    let
                        newOffset =
                            results.offset + results.limit
                    in
                    ( { model | results = LoadingMore results }
                    , Effect.fromCmd (Api.searchFlows MoreResultsLoaded model.dbName model.query results.limit newOffset)
                    )

                _ ->
                    ( model, Effect.none )

        NavigateToActivity processId ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Upstream model.dbName processId)))
            )

        RequestLoadDatabase ->
            ( model, Effect.fromShared (Shared.LoadDatabase model.dbName) )

        NewFlags flags ->
            if Maybe.withDefault "" flags.q == model.query then
                ( model, Effect.none )

            else
                init shared flags

        NoOp ->
            ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Flow Search"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ h2 [ class "title is-4" ] [ text "Flow Search" ]
            , div [ class "field" ]
                [ div [ class "control has-icons-left" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Search flows by name..."
                        , value model.query
                        , onInput UpdateQuery
                        , id "flow-search"
                        ]
                        []
                    , span [ class "icon is-small is-left" ]
                        [ i [ class "fas fa-search" ] [] ]
                    ]
                ]
            , viewResults model
            , viewFlowDetail model
            ]


viewResults : Model -> Html Msg
viewResults model =
    case model.results of
        NotSearched ->
            p [ class "has-text-grey" ] [ text "Enter a flow name to search." ]

        Searching ->
            div [ class "has-text-centered" ]
                [ progress [ class "progress is-primary is-small", attribute "max" "100" ] [] ]

        SearchFailed err ->
            div [ class "notification is-danger" ]
                [ strong [] [ text "Error: " ], text err ]

        Results results ->
            viewResultsTable model results False

        LoadingMore results ->
            viewResultsTable model results True


viewResultsTable : Model -> SearchResults FlowSearchResult -> Bool -> Html Msg
viewResultsTable model results loadingMore =
    div []
        [ p [ class "has-text-grey", style "margin-bottom" "0.5rem" ]
            [ text (String.fromInt results.totalCount ++ " flows found") ]
        , table [ class "table is-fullwidth is-hoverable is-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Category" ]
                    , th [] [ text "Unit" ]
                    , th [] [ text "Synonyms" ]
                    ]
                ]
            , tbody []
                (List.map (viewFlowRow model.selectedFlow) results.results)
            ]
        , if results.hasMore then
            div [ class "has-text-centered", style "margin-top" "1rem" ]
                [ button
                    [ class
                        ("button is-primary is-outlined"
                            ++ (if loadingMore then
                                    " is-loading"

                                else
                                    ""
                               )
                        )
                    , onClick LoadMore
                    , disabled loadingMore
                    ]
                    [ text "Load more" ]
                ]

          else
            text ""
        ]


viewFlowRow : Maybe FlowSearchResult -> FlowSearchResult -> Html Msg
viewFlowRow selected flow =
    let
        isSelected =
            Maybe.map .id selected == Just flow.id

        synonymText =
            Dict.values flow.synonyms
                |> List.concat
                |> List.take 3
                |> String.join ", "
    in
    tr
        [ onClick (SelectFlow flow)
        , style "cursor" "pointer"
        , classList [ ( "is-selected", isSelected ) ]
        ]
        [ td [] [ text flow.name ]
        , td [] [ text flow.category ]
        , td [] [ text flow.unitName ]
        , td [ class "has-text-grey", style "font-size" "0.9em" ]
            [ text synonymText ]
        ]


viewFlowDetail : Model -> Html Msg
viewFlowDetail model =
    case model.selectedFlow of
        Nothing ->
            text ""

        Just flow ->
            div [ class "box", style "margin-top" "1rem" ]
                [ div [ class "level" ]
                    [ div [ class "level-left" ]
                        [ h3 [ class "title is-5", style "margin-bottom" "0" ]
                            [ text ("Activities using: " ++ flow.name) ]
                        ]
                    , div [ class "level-right" ]
                        [ button [ class "delete", onClick DeselectFlow ] [] ]
                    ]
                , case model.flowActivities of
                    Loading ->
                        progress [ class "progress is-primary is-small", attribute "max" "100" ] []

                    Failed err ->
                        div [ class "notification is-danger is-light" ]
                            [ text err ]

                    Loaded activities ->
                        if List.isEmpty activities then
                            p [ class "has-text-grey" ] [ text "No activities use this flow." ]

                        else
                            table [ class "table is-fullwidth is-hoverable is-narrow" ]
                                [ thead []
                                    [ tr []
                                        [ th [] [ text "Activity" ]
                                        , th [] [ text "Location" ]
                                        , th [] [ text "Product" ]
                                        ]
                                    ]
                                , tbody []
                                    (List.map viewActivityRow activities)
                                ]

                    NotAsked ->
                        text ""
                ]


viewActivityRow : ActivitySummary -> Html Msg
viewActivityRow activity =
    tr [ onClick (NavigateToActivity activity.id), style "cursor" "pointer" ]
        [ td [] [ text activity.name ]
        , td [] [ text activity.location ]
        , td [] [ text activity.product ]
        ]

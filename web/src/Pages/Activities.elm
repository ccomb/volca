module Pages.Activities exposing (Model, Msg, page)

import Browser.Dom
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Json.Decode as Decode
import Models.Activity exposing (ActivitySummary, ClassificationSystem, SearchResults, activitySummaryDecoder, classificationSystemDecoder, searchResultsDecoder)
import Models.Database exposing (DatabaseList)
import Route exposing (ActivityTab(..), Route(..))
import Shared exposing (RemoteData(..))
import Process
import Spa.Page
import Task
import Url.Builder
import View exposing (View)
import Views.ActivitiesView as ActivitiesView
import Views.DatabasesView as DatabasesView


type alias Model =
    { searchQuery : String
    , productQuery : String
    , dbName : String
    , results : SearchState
    , classificationSystems : Maybe (List ClassificationSystem)
    , activeFilters : List ( String, String )
    , pendingSystem : Maybe String
    , pendingValue : String
    , debounceCounter : Int
    }


type SearchState
    = NotSearched
    | Searching
    | Results (SearchResults ActivitySummary)
    | LoadingMore (SearchResults ActivitySummary)
    | SearchFailed String


type Msg
    = ActivitiesViewMsg ActivitiesView.Msg
    | SearchResultsLoaded (Result Http.Error (SearchResults ActivitySummary))
    | MoreResultsLoaded (Result Http.Error (SearchResults ActivitySummary))
    | ClassificationsLoaded (Result Http.Error (List ClassificationSystem))
    | NewFlags Route.ActivitiesFlags
    | ScrollDone
    | DebounceTick Int
    | RequestLoadDatabase


page : Shared.Model -> Spa.Page.Page Route.ActivitiesFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> Route.ActivitiesFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        searchQuery =
            Maybe.withDefault "" flags.name

        productQuery =
            Maybe.withDefault "" flags.product

        dbLoaded =
            Shared.isDatabaseLoaded shared flags.db
    in
    ( { searchQuery = searchQuery
      , productQuery = productQuery
      , dbName = flags.db
      , results = if dbLoaded then Searching else NotSearched
      , classificationSystems = Nothing
      , activeFilters = flags.classifications
      , pendingSystem = Nothing
      , pendingValue = ""
      , debounceCounter = 0
      }
    , Effect.batch
        [ if dbLoaded then
            Effect.batch
                [ Effect.fromCmd (searchActivities flags.db searchQuery productQuery flags.classifications)
                , Effect.fromCmd (fetchClassifications flags.db)
                ]

          else
            Effect.none
        , Effect.fromCmd (Browser.Dom.focus "activity-search" |> Task.attempt (\_ -> ScrollDone))
        ]
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivitiesViewMsg viewMsg ->
            case viewMsg of
                ActivitiesView.UpdateSearchQuery query ->
                    let
                        newCounter =
                            model.debounceCounter + 1
                    in
                    ( { model | searchQuery = query, debounceCounter = newCounter }
                    , Effect.fromCmd (Process.sleep 100 |> Task.perform (\_ -> DebounceTick newCounter))
                    )

                ActivitiesView.UpdateProductQuery query ->
                    let
                        newCounter =
                            model.debounceCounter + 1
                    in
                    ( { model | productQuery = query, debounceCounter = newCounter }
                    , Effect.fromCmd (Process.sleep 100 |> Task.perform (\_ -> DebounceTick newCounter))
                    )

                ActivitiesView.SelectActivity activityId ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivityRoute Upstream model.dbName activityId)))
                    )

                ActivitiesView.LoadMore ->
                    case model.results of
                        Results results ->
                            let
                                newOffset =
                                    results.offset + results.limit
                            in
                            ( { model | results = LoadingMore results }
                            , Effect.fromCmd (searchActivitiesWithOffset model.dbName model.searchQuery model.productQuery model.activeFilters newOffset results.limit)
                            )

                        _ ->
                            ( model, Effect.none )

                ActivitiesView.SelectDatabase dbName ->
                    let
                        queryName =
                            if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery

                        queryProduct =
                            if String.isEmpty model.productQuery then Nothing else Just model.productQuery
                    in
                    ( { model
                        | dbName = dbName
                        , results = Searching
                        , classificationSystems = Nothing
                        , activeFilters = []
                        , pendingSystem = Nothing
                        , pendingValue = ""
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivitiesRoute { db = dbName, name = queryName, product = queryProduct, limit = Just 20, classifications = [] })))
                        , Effect.fromCmd (searchActivities dbName model.searchQuery model.productQuery [])
                        , Effect.fromCmd (fetchClassifications dbName)
                        ]
                    )

                ActivitiesView.SelectClassificationSystem system ->
                    let
                        newModel =
                            { model | pendingSystem = system, pendingValue = "" }
                    in
                    ( { newModel | results = Searching }
                    , if Shared.isDatabaseLoaded shared model.dbName then
                        Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery (currentFilters newModel))
                      else
                        Effect.none
                    )

                ActivitiesView.UpdatePendingValue val ->
                    let
                        newCounter =
                            model.debounceCounter + 1
                    in
                    ( { model | pendingValue = val, debounceCounter = newCounter }
                    , Effect.fromCmd (Process.sleep 100 |> Task.perform (\_ -> DebounceTick newCounter))
                    )

                ActivitiesView.CommitWithValue sys val ->
                    let
                        newFilters =
                            model.activeFilters ++ [ ( sys, val ) ]

                        newRoute =
                            ActivitiesRoute
                                { db = model.dbName
                                , name = if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery
                                , product = if String.isEmpty model.productQuery then Nothing else Just model.productQuery
                                , limit = Just 20
                                , classifications = newFilters
                                }
                    in
                    ( { model | activeFilters = newFilters, pendingValue = "", results = Searching }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery newFilters)
                        ]
                    )

                ActivitiesView.CommitFilter ->
                    case model.pendingSystem of
                        Nothing ->
                            ( model, Effect.none )

                        Just sys ->
                            if String.isEmpty model.pendingValue then
                                ( model, Effect.none )

                            else
                                let
                                    newFilters =
                                        model.activeFilters ++ [ ( sys, model.pendingValue ) ]

                                    newRoute =
                                        ActivitiesRoute
                                            { db = model.dbName
                                            , name = if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery
                                            , product = if String.isEmpty model.productQuery then Nothing else Just model.productQuery
                                            , limit = Just 20
                                            , classifications = newFilters
                                            }
                                in
                                ( { model | activeFilters = newFilters, pendingValue = "", results = Searching }
                                , Effect.batch
                                    [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                                    , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery newFilters)
                                    ]
                                )

                ActivitiesView.RemoveFilter sys ->
                    let
                        newFilters =
                            List.filter (\( s, _ ) -> s /= sys) model.activeFilters

                        newModel =
                            { model | activeFilters = newFilters }

                        urlFilters =
                            currentFilters newModel

                        newRoute =
                            ActivitiesRoute
                                { db = model.dbName
                                , name = if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery
                                , product = if String.isEmpty model.productQuery then Nothing else Just model.productQuery
                                , limit = Just 20
                                , classifications = urlFilters
                                }

                        hasQuery =
                            not (List.isEmpty urlFilters) || not (String.isEmpty model.searchQuery) || not (String.isEmpty model.productQuery)
                    in
                    ( { newModel
                        | results =
                            if hasQuery then
                                Searching

                            else
                                NotSearched
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , if hasQuery then
                            Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery urlFilters)

                          else
                            Effect.none
                        ]
                    )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        SearchResultsLoaded (Ok results) ->
            ( { model | results = Results results }
            , Effect.none
            )

        SearchResultsLoaded (Err error) ->
            ( { model | results = SearchFailed (Shared.httpErrorToString error) }
            , Effect.none
            )

        MoreResultsLoaded (Ok newResults) ->
            case model.results of
                LoadingMore existingResults ->
                    ( { model
                        | results =
                            Results
                                { existingResults
                                    | results = existingResults.results ++ newResults.results
                                    , offset = newResults.offset
                                    , hasMore = newResults.hasMore
                                    , totalCount = newResults.totalCount
                                }
                      }
                    , Effect.fromCmd (scrollToBottom "main-content")
                    )

                _ ->
                    ( model, Effect.none )

        MoreResultsLoaded (Err error) ->
            case model.results of
                LoadingMore existingResults ->
                    ( { model | results = Results existingResults }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        ClassificationsLoaded (Ok systems) ->
            ( { model | classificationSystems = Just systems }
            , Effect.none
            )

        ClassificationsLoaded (Err _) ->
            ( model, Effect.none )

        ScrollDone ->
            ( model, Effect.none )

        DebounceTick counter ->
            if counter /= model.debounceCounter || not (Shared.isDatabaseLoaded shared model.dbName) then
                ( model, Effect.none )

            else
                let
                    queryName =
                        if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery

                    queryProduct =
                        if String.isEmpty model.productQuery then Nothing else Just model.productQuery

                    newRoute =
                        ActivitiesRoute { db = model.dbName, name = queryName, product = queryProduct, limit = Just 20, classifications = currentFilters model }
                in
                ( { model | results = Searching }
                , Effect.batch
                    [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                    , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery (currentFilters model))
                    ]
                )

        NewFlags flags ->
            let
                newQuery =
                    Maybe.withDefault "" flags.name

                newProduct =
                    Maybe.withDefault "" flags.product

                dbNowLoaded =
                    Shared.isDatabaseLoaded shared flags.db

                -- Re-init if DB just became available while we were waiting
                needsRetry =
                    model.results == NotSearched && dbNowLoaded

                classChanged =
                    flags.classifications /= model.activeFilters
            in
            if newQuery == model.searchQuery && newProduct == model.productQuery && not needsRetry && not classChanged then
                ( model
                , if dbNowLoaded && model.classificationSystems == Nothing then
                    Effect.fromCmd (fetchClassifications flags.db)
                  else
                    Effect.none
                )

            else if newQuery == model.searchQuery && newProduct == model.productQuery && not needsRetry then
                if flags.classifications == currentFilters model then
                    -- URL was updated by our own DebounceTick — ignore to preserve pending state
                    ( model, Effect.none )

                else
                    -- External navigation changed classifications — update committed filters
                    ( { model | activeFilters = flags.classifications, pendingSystem = Nothing, pendingValue = "" }
                    , Effect.none
                    )

            else
                init shared flags


updateQuery : Shared.Model -> Model -> ( Model, Effect Shared.Msg Msg )
updateQuery shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        ( model, Effect.none )

    else
        let
            queryName =
                if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery

            queryProduct =
                if String.isEmpty model.productQuery then Nothing else Just model.productQuery

            newRoute =
                ActivitiesRoute { db = model.dbName, name = queryName, product = queryProduct, limit = Just 20, classifications = currentFilters model }
        in
        ( { model | results = Searching }
        , Effect.batch
            [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
            , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery (currentFilters model))
            ]
        )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        searchResults =
            case model.results of
                Results r ->
                    Just r

                LoadingMore r ->
                    Just r

                _ ->
                    Nothing

        searchLoading =
            case model.results of
                Searching ->
                    True

                _ ->
                    False

        loadingMore =
            case model.results of
                LoadingMore _ ->
                    True

                _ ->
                    False

        error =
            case model.results of
                SearchFailed err ->
                    Just err

                _ ->
                    Nothing

        maybeDatabaseList =
            case shared.databases of
                Loaded dbList ->
                    Just dbList

                _ ->
                    Nothing
    in
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "Activities"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        { title = "Activities"
        , body =
            Html.map ActivitiesViewMsg
                (ActivitiesView.viewActivitiesPage
                    model.dbName
                    model.searchQuery
                    model.productQuery
                    searchResults
                    searchLoading
                    loadingMore
                    error
                    maybeDatabaseList
                    model.classificationSystems
                    model.activeFilters
                    model.pendingSystem
                    model.pendingValue
                )
        }



-- Active + pending classification filters (pending only included when value is non-empty)
currentFilters : Model -> List ( String, String )
currentFilters model =
    case model.pendingSystem of
        Nothing ->
            model.activeFilters

        Just sys ->
            if String.isEmpty model.pendingValue then
                model.activeFilters

            else
                model.activeFilters ++ [ ( sys, model.pendingValue ) ]


-- HTTP commands


searchActivities : String -> String -> String -> List ( String, String ) -> Cmd Msg
searchActivities dbName query productQuery classFilters =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "name" query)
                    , if String.isEmpty productQuery then Nothing else Just (Url.Builder.string "product" productQuery)
                    , Just (Url.Builder.int "limit" 20)
                    ]
                    ++ List.concatMap
                        (\( sys, val ) ->
                            [ Url.Builder.string "classification" sys
                            , Url.Builder.string "classification-value" val
                            ]
                        )
                        classFilters
                )
        , expect = Http.expectJson SearchResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }


fetchClassifications : String -> Cmd Msg
fetchClassifications dbName =
    Http.get
        { url = Url.Builder.absolute [ "api", "v1", "db", dbName, "classifications" ] []
        , expect = Http.expectJson ClassificationsLoaded (Decode.list classificationSystemDecoder)
        }


scrollToBottom : String -> Cmd Msg
scrollToBottom containerId =
    Browser.Dom.getViewportOf containerId
        |> Task.andThen (\info -> Browser.Dom.setViewportOf containerId 0 info.scene.height)
        |> Task.attempt (\_ -> ScrollDone)


searchActivitiesWithOffset : String -> String -> String -> List ( String, String ) -> Int -> Int -> Cmd Msg
searchActivitiesWithOffset dbName query productQuery classFilters offset limit =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "name" query)
                    , if String.isEmpty productQuery then Nothing else Just (Url.Builder.string "product" productQuery)
                    , Just (Url.Builder.int "limit" limit)
                    , Just (Url.Builder.int "offset" offset)
                    ]
                    ++ List.concatMap
                        (\( sys, val ) ->
                            [ Url.Builder.string "classification" sys
                            , Url.Builder.string "classification-value" val
                            ]
                        )
                        classFilters
                )
        , expect = Http.expectJson MoreResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }

module Pages.Activities exposing (Model, Msg, page)

import Browser.Dom
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Models.Activity exposing (ActivitySummary, SearchResults, activitySummaryDecoder, searchResultsDecoder)
import Models.Database exposing (DatabaseList)
import Route exposing (ActivityTab(..), Route(..))
import Shared exposing (RemoteData(..))
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
    | NewFlags Route.ActivitiesFlags
    | ScrollDone
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

        hasQuery =
            not (String.isEmpty searchQuery) || not (String.isEmpty productQuery)

        shouldSearch =
            hasQuery && dbLoaded
    in
    ( { searchQuery = searchQuery
      , productQuery = productQuery
      , dbName = flags.db
      , results =
            if shouldSearch then
                Searching

            else
                NotSearched
      }
    , Effect.batch
        [ if shouldSearch then
            Effect.fromCmd (searchActivities flags.db searchQuery productQuery)

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
                    updateQuery shared { model | searchQuery = query }

                ActivitiesView.UpdateProductQuery query ->
                    updateQuery shared { model | productQuery = query }

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
                            , Effect.fromCmd (searchActivitiesWithOffset model.dbName model.searchQuery model.productQuery newOffset results.limit)
                            )

                        _ ->
                            ( model, Effect.none )

                ActivitiesView.SelectDatabase dbName ->
                    let
                        queryName =
                            if String.isEmpty model.searchQuery then
                                Nothing

                            else
                                Just model.searchQuery

                        queryProduct =
                            if String.isEmpty model.productQuery then
                                Nothing

                            else
                                Just model.productQuery

                        hasQuery =
                            not (String.isEmpty model.searchQuery) || not (String.isEmpty model.productQuery)
                    in
                    ( { model
                        | dbName = dbName
                        , results =
                            if hasQuery then
                                Searching

                            else
                                NotSearched
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivitiesRoute { db = dbName, name = queryName, product = queryProduct, limit = Just 20 })))
                        , if hasQuery then
                            Effect.fromCmd (searchActivities dbName model.searchQuery model.productQuery)

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

        ScrollDone ->
            ( model, Effect.none )

        NewFlags flags ->
            let
                newQuery =
                    Maybe.withDefault "" flags.name

                newProduct =
                    Maybe.withDefault "" flags.product

                dbNowLoaded =
                    Shared.isDatabaseLoaded shared flags.db

                hasQuery =
                    not (String.isEmpty newQuery) || not (String.isEmpty newProduct)

                -- Re-init if DB just became available with a pending query
                needsRetry =
                    model.results == NotSearched && hasQuery && dbNowLoaded
            in
            if newQuery == model.searchQuery && newProduct == model.productQuery && not needsRetry then
                ( model, Effect.fromCmd (Browser.Dom.focus "activity-search" |> Task.attempt (\_ -> ScrollDone)) )

            else
                init shared flags


updateQuery : Shared.Model -> Model -> ( Model, Effect Shared.Msg Msg )
updateQuery shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        ( model, Effect.none )

    else
        let
            queryName =
                if String.isEmpty model.searchQuery then
                    Nothing

                else
                    Just model.searchQuery

            queryProduct =
                if String.isEmpty model.productQuery then
                    Nothing

                else
                    Just model.productQuery

            hasQuery =
                queryName /= Nothing || queryProduct /= Nothing

            newRoute =
                ActivitiesRoute { db = model.dbName, name = queryName, product = queryProduct, limit = Just 20 }

            cmds =
                if not hasQuery then
                    Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))

                else
                    Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery)
                        ]
        in
        ( { model
            | results =
                if not hasQuery then
                    NotSearched

                else
                    case model.results of
                        Results _ ->
                            model.results

                        _ ->
                            Searching
          }
        , cmds
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
                )
        }



-- HTTP commands


searchActivities : String -> String -> String -> Cmd Msg
searchActivities dbName query productQuery =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "database", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "name" query)
                    , if String.isEmpty productQuery then Nothing else Just (Url.Builder.string "product" productQuery)
                    , Just (Url.Builder.int "limit" 20)
                    ]
                )
        , expect = Http.expectJson SearchResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }


scrollToBottom : String -> Cmd Msg
scrollToBottom containerId =
    Browser.Dom.getViewportOf containerId
        |> Task.andThen (\info -> Browser.Dom.setViewportOf containerId 0 info.scene.height)
        |> Task.attempt (\_ -> ScrollDone)


searchActivitiesWithOffset : String -> String -> String -> Int -> Int -> Cmd Msg
searchActivitiesWithOffset dbName query productQuery offset limit =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "database", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "name" query)
                    , if String.isEmpty productQuery then Nothing else Just (Url.Builder.string "product" productQuery)
                    , Just (Url.Builder.int "limit" limit)
                    , Just (Url.Builder.int "offset" offset)
                    ]
                )
        , expect = Http.expectJson MoreResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }

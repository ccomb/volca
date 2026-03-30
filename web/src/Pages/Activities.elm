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
    , selectedSystem : Maybe String
    , selectedValue : Maybe String
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

        hasTextQuery =
            not (String.isEmpty searchQuery)

        hasProductQuery =
            not (String.isEmpty productQuery)

        hasClassFilter =
            flags.classificationValue /= Nothing

        shouldSearch =
            (hasTextQuery || hasProductQuery || hasClassFilter) && dbLoaded
    in
    ( { searchQuery = searchQuery
      , productQuery = productQuery
      , dbName = flags.db
      , results =
            if shouldSearch then
                Searching

            else
                NotSearched
      , classificationSystems = Nothing
      , selectedSystem = flags.classification
      , selectedValue = flags.classificationValue
      , debounceCounter = 0
      }
    , Effect.batch
        [ if shouldSearch then
            Effect.fromCmd (searchActivities flags.db searchQuery productQuery flags.classification flags.classificationValue)

          else
            Effect.none
        , if dbLoaded then
            Effect.fromCmd (fetchClassifications flags.db)

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
                            , Effect.fromCmd (searchActivitiesWithOffset model.dbName model.searchQuery model.productQuery model.selectedSystem model.selectedValue newOffset results.limit)
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

                        shouldSearch =
                            not (String.isEmpty model.searchQuery) || not (String.isEmpty model.productQuery) || model.selectedValue /= Nothing
                    in
                    ( { model
                        | dbName = dbName
                        , results =
                            if shouldSearch then
                                Searching

                            else
                                NotSearched
                        , classificationSystems = Nothing
                        , selectedSystem = Nothing
                        , selectedValue = Nothing
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivitiesRoute { db = dbName, name = queryName, product = queryProduct, limit = Just 20, classification = Nothing, classificationValue = Nothing })))
                        , if shouldSearch then
                            Effect.fromCmd (searchActivities dbName model.searchQuery model.productQuery Nothing Nothing)

                          else
                            Effect.none
                        , Effect.fromCmd (fetchClassifications dbName)
                        ]
                    )

                ActivitiesView.SelectClassificationSystem system ->
                    let
                        newRoute =
                            ActivitiesRoute
                                { db = model.dbName
                                , name = if String.isEmpty model.searchQuery then Nothing else Just model.searchQuery
                                , product = if String.isEmpty model.productQuery then Nothing else Just model.productQuery
                                , limit = Just 20
                                , classification = system
                                , classificationValue = Nothing
                                }
                    in
                    ( { model | selectedSystem = system, selectedValue = Nothing }
                    , Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                    )

                ActivitiesView.SelectClassificationValue value ->
                    let
                        queryProduct =
                            if String.isEmpty model.productQuery then
                                Nothing

                            else
                                Just model.productQuery

                        newRoute =
                            ActivitiesRoute
                                { db = model.dbName
                                , name =
                                    if String.isEmpty model.searchQuery then
                                        Nothing

                                    else
                                        Just model.searchQuery
                                , product = queryProduct
                                , limit = Just 20
                                , classification = model.selectedSystem
                                , classificationValue = value
                                }

                        hasQuery =
                            value /= Nothing || not (String.isEmpty model.searchQuery) || not (String.isEmpty model.productQuery)
                    in
                    ( { model
                        | selectedValue = value
                        , results =
                            if hasQuery then
                                Searching

                            else
                                NotSearched
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , if hasQuery then
                            Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery model.selectedSystem value)

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
                        if String.isEmpty model.searchQuery then
                            Nothing

                        else
                            Just model.searchQuery

                    queryProduct =
                        if String.isEmpty model.productQuery then
                            Nothing

                        else
                            Just model.productQuery

                    newRoute =
                        ActivitiesRoute { db = model.dbName, name = queryName, product = queryProduct, limit = Just 20, classification = model.selectedSystem, classificationValue = model.selectedValue }

                    hasQuery =
                        not (String.isEmpty model.searchQuery) || not (String.isEmpty model.productQuery) || model.selectedValue /= Nothing
                in
                ( { model
                    | results =
                        if not hasQuery then
                            NotSearched

                        else
                            Searching
                  }
                , if not hasQuery then
                    Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))

                  else
                    Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery model.selectedSystem model.selectedValue)
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

                hasQuery =
                    not (String.isEmpty newQuery) || not (String.isEmpty newProduct)

                -- Re-init if DB just became available with a pending query
                needsRetry =
                    model.results == NotSearched && hasQuery && dbNowLoaded

                classChanged =
                    flags.classification /= model.selectedSystem || flags.classificationValue /= model.selectedValue
            in
            if newQuery == model.searchQuery && newProduct == model.productQuery && not needsRetry && not classChanged then
                ( model, Effect.none )

            else if newQuery == model.searchQuery && newProduct == model.productQuery && not needsRetry then
                -- Only classification changed — update state without stealing focus
                ( { model | selectedSystem = flags.classification, selectedValue = flags.classificationValue }
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
                queryName /= Nothing || queryProduct /= Nothing || model.selectedValue /= Nothing

            newRoute =
                ActivitiesRoute { db = model.dbName, name = queryName, product = queryProduct, limit = Just 20, classification = model.selectedSystem, classificationValue = model.selectedValue }

            cmds =
                if not hasQuery then
                    Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))

                else
                    Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , Effect.fromCmd (searchActivities model.dbName model.searchQuery model.productQuery model.selectedSystem model.selectedValue)
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
                    model.classificationSystems
                    model.selectedSystem
                    model.selectedValue
                )
        }



-- HTTP commands


searchActivities : String -> String -> String -> Maybe String -> Maybe String -> Cmd Msg
searchActivities dbName query productQuery classification classificationValue =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "name" query)
                    , if String.isEmpty productQuery then Nothing else Just (Url.Builder.string "product" productQuery)
                    , Just (Url.Builder.int "limit" 20)
                    , Maybe.map (Url.Builder.string "classification") classification
                    , Maybe.map (Url.Builder.string "classification-value") classificationValue
                    ]
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


searchActivitiesWithOffset : String -> String -> String -> Maybe String -> Maybe String -> Int -> Int -> Cmd Msg
searchActivitiesWithOffset dbName query productQuery classification classificationValue offset limit =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then Nothing else Just (Url.Builder.string "name" query)
                    , if String.isEmpty productQuery then Nothing else Just (Url.Builder.string "product" productQuery)
                    , Just (Url.Builder.int "limit" limit)
                    , Just (Url.Builder.int "offset" offset)
                    , Maybe.map (Url.Builder.string "classification") classification
                    , Maybe.map (Url.Builder.string "classification-value") classificationValue
                    ]
                )
        , expect = Http.expectJson MoreResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }

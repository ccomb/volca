module Pages.Composition exposing (Model, Msg, page)

import Api exposing (SupplyChainParams)
import Browser.Dom
import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Models.Activity exposing (ActivityInfo, ClassificationSystem, classificationSystemDecoder)
import Process
import Task
import Models.SupplyChain exposing (SupplyChainEntry, SupplyChainResponse)
import Route exposing (Route(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import Url.Builder
import Utils.Format as Format
import View exposing (View)
import Views.ActivityHeader


type alias Model =
    { activityId : String
    , dbName : String
    , activityInfo : RemoteData ActivityInfo
    , supplyChain : RemoteData SupplyChainResponse
    , params : SupplyChainParams
    , sortColumn : SortColumn
    , sortAscending : Bool
    , loadingMore : Bool
    , debounceCounter : Int
    , classificationSystems : Maybe (List ClassificationSystem)
    , selectedSystem : Maybe String
    }


type SortColumn
    = ByName
    | ByLocation
    | ByAmount
    | ByUnit
    | ByDepth
    | ByConsumers


type Msg
    = ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | SupplyChainLoaded (Result Http.Error SupplyChainResponse)
    | SetNameFilter String
    | SetLocationFilter String
    | SelectClassificationSystem (Maybe String)
    | SelectClassificationValue (Maybe String)
    | ClassificationsLoaded (Result Http.Error (List ClassificationSystem))
    | SetMaxDepth String
    | SetMinQuantity String
    | DebounceTick Int
    | LoadMore
    | SortBy SortColumn
    | NavigateToActivity String
    | NavigateBack
    | RequestLoadDatabase
    | NewFlags Route.CompositionFlags
    | ScrollDone


page : Shared.Model -> Spa.Page.Page Route.CompositionFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> Route.CompositionFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        db =
            flags.db

        activityId =
            flags.processId

        params =
            { name = Maybe.withDefault "" flags.name
            , location = Maybe.withDefault "" flags.location
            , classification = Maybe.withDefault "" flags.classification
            , classificationValue = Maybe.withDefault "" flags.classificationValue
            , maxDepth = Maybe.withDefault "" flags.maxDepth
            , minQuantity = Maybe.withDefault "" flags.minQuantity
            , limit = Api.defaultSupplyChainParams.limit
            , offset = 0
            , sort = Maybe.withDefault "depth" flags.sort
            , order = Maybe.withDefault "asc" flags.order
            , includeEdges = False
            }

        model =
            { activityId = activityId
            , dbName = db
            , activityInfo = NotAsked
            , supplyChain = NotAsked
            , params = params
            , sortColumn = parseSortColumn (Maybe.withDefault "depth" flags.sort)
            , sortAscending = Maybe.withDefault "asc" flags.order /= "desc"
            , loadingMore = False
            , debounceCounter = 0
            , classificationSystems = Nothing
            , selectedSystem = flags.classification
            }
    in
    if not (Shared.isDatabaseLoaded shared db) then
        ( model, Effect.none )

    else
        let
            cached =
                Dict.get activityId shared.cachedActivityInfo
        in
        ( { model
            | activityInfo =
                case cached of
                    Just info ->
                        Loaded info

                    Nothing ->
                        Loading
            , supplyChain = Loading
          }
        , Effect.batch
            [ case cached of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (Api.loadActivityInfo ActivityInfoLoaded db activityId)
            , Effect.fromCmd (Api.loadSupplyChain SupplyChainLoaded db activityId params)
            , Effect.fromCmd (fetchClassifications db)
            ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivityInfoLoaded (Ok info) ->
            ( { model | activityInfo = Loaded info }
            , Effect.fromShared (Shared.CacheActivityInfo model.activityId info)
            )

        ActivityInfoLoaded (Err error) ->
            ( { model | activityInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        SupplyChainLoaded (Ok data) ->
            let
                merged =
                    if model.loadingMore then
                        case model.supplyChain of
                            Loaded prev ->
                                { data | supplyChain = prev.supplyChain ++ data.supplyChain }

                            _ ->
                                data

                    else
                        data
            in
            ( { model | supplyChain = Loaded merged, loadingMore = False }
            , if model.loadingMore then
                Effect.fromCmd (scrollToBottom "main-content")

              else
                Effect.none
            )

        SupplyChainLoaded (Err error) ->
            ( { model | supplyChain = Failed (Shared.httpErrorToString error), loadingMore = False }
            , Effect.none
            )

        SetNameFilter val ->
            updateFilter shared model (\p -> { p | name = val })

        SetLocationFilter val ->
            updateFilter shared model (\p -> { p | location = val })

        SelectClassificationSystem system ->
            updateFilter shared { model | selectedSystem = system } (\p -> { p | classification = Maybe.withDefault "" system, classificationValue = "" })

        SelectClassificationValue maybeVal ->
            let
                val =
                    Maybe.withDefault "" maybeVal
            in
            updateFilter shared model (\p -> { p | classificationValue = val })

        ClassificationsLoaded (Ok systems) ->
            ( { model | classificationSystems = Just systems }, Effect.none )

        ClassificationsLoaded (Err _) ->
            ( model, Effect.none )

        SetMaxDepth val ->
            updateFilter shared model (\p -> { p | maxDepth = val })

        SetMinQuantity val ->
            updateFilter shared model (\p -> { p | minQuantity = val })

        DebounceTick counter ->
            if counter == model.debounceCounter then
                let
                    newRoute =
                        CompositionRoute
                            { db = model.dbName
                            , processId = model.activityId
                            , name = toMaybe model.params.name
                            , location = toMaybe model.params.location
                            , classification = toMaybe model.params.classification
                            , classificationValue = toMaybe model.params.classificationValue
                            , maxDepth = toMaybe model.params.maxDepth
                            , minQuantity = toMaybe model.params.minQuantity
                            , sort = Just (sortColumnToString model.sortColumn)
                            , order = Just (if model.sortAscending then "asc" else "desc")
                            }
                in
                ( model
                , Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                )

            else
                ( model, Effect.none )

        LoadMore ->
            let
                params =
                    model.params

                newOffset =
                    params.offset + params.limit
            in
            ( { model | loadingMore = True, params = { params | offset = newOffset } }
            , Effect.fromCmd (Api.loadSupplyChain SupplyChainLoaded model.dbName model.activityId { params | offset = newOffset })
            )

        SortBy col ->
            let
                newAsc =
                    if model.sortColumn == col then
                        not model.sortAscending

                    else
                        True

                newRoute =
                    CompositionRoute
                        { db = model.dbName
                        , processId = model.activityId
                        , name = toMaybe model.params.name
                        , location = toMaybe model.params.location
                        , classification = toMaybe model.params.classification
                        , classificationValue = toMaybe model.params.classificationValue
                        , maxDepth = toMaybe model.params.maxDepth
                        , minQuantity = toMaybe model.params.minQuantity
                        , sort = Just (sortColumnToString col)
                        , order = Just (if newAsc then "asc" else "desc")
                        }
            in
            ( model
            , Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
            )

        NavigateToActivity processId ->
            let
                ( dbName, actualProcessId ) =
                    case String.split "::" processId of
                        [ db, pid ] ->
                            ( db, pid )

                        _ ->
                            ( model.dbName, processId )
            in
            ( model
            , Effect.batch
                [ Effect.fromShared (Shared.PushActivity model.dbName model.activityId)
                , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Upstream dbName actualProcessId)))
                ]
            )

        NavigateBack ->
            ( model
            , Effect.fromShared Shared.NavigateBackToParent
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            init shared flags

        ScrollDone ->
            ( model, Effect.none )


fetchClassifications : String -> Cmd Msg
fetchClassifications dbName =
    Http.get
        { url = Url.Builder.absolute [ "api", "v1", "db", dbName, "classifications" ] []
        , expect = Http.expectJson ClassificationsLoaded (Decode.list classificationSystemDecoder)
        }


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Composition"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ case model.activityInfo of
                Loading ->
                    viewLoading

                Failed err ->
                    viewError err

                Loaded activityInfo ->
                    div []
                        [ Views.ActivityHeader.viewActivityHeader activityInfo "Composition" NavigateBack
                        , viewFilters model
                        , viewSupplyChain model
                        ]

                NotAsked ->
                    text ""
            ]


viewLoading : Html msg
viewLoading =
    div [ class "has-text-centered" ]
        [ div [ class "is-size-3" ] [ text "Loading..." ]
        , progress [ class "progress is-primary", attribute "max" "100" ] []
        ]


viewError : String -> Html msg
viewError err =
    div [ class "notification is-danger" ]
        [ strong [] [ text "Error: " ]
        , text err
        ]


viewFilters : Model -> Html Msg
viewFilters model =
    div [ style "margin" "0.75rem 0", style "padding" "0 1.25rem", style "display" "flex", style "gap" "0.5rem", style "flex-wrap" "wrap", style "align-items" "flex-end" ]
        ([ viewFilterInput "Name" "Search activities..." model.params.name SetNameFilter
         , viewFilterInput "Location" "e.g. FR, DE..." model.params.location SetLocationFilter
         ]
         ++ viewClassificationFilter model
         ++ [ viewFilterInput "Max depth" "e.g. 3" model.params.maxDepth SetMaxDepth
            , viewFilterInput "Min quantity" "e.g. 0.001" model.params.minQuantity SetMinQuantity
            ]
        )


viewClassificationFilter : Model -> List (Html Msg)
viewClassificationFilter model =
    case model.classificationSystems of
        Nothing ->
            [ viewFilterInput "Classification" "Search classifications..." model.params.classificationValue (\v -> SelectClassificationValue (if String.isEmpty v then Nothing else Just v)) ]

        Just [] ->
            []

        Just systems ->
            let
                systemDropdown =
                    div [ style "display" "flex", style "flex-direction" "column" ]
                        [ label [ class "label is-small", style "margin-bottom" "0.25rem" ] [ text "Classification" ]
                        , div [ class "select is-small" ]
                            [ select
                                [ onInput
                                    (\val ->
                                        if val == "" then
                                            SelectClassificationSystem Nothing
                                        else
                                            SelectClassificationSystem (Just val)
                                    )
                                ]
                                (option [ value "", selected (model.selectedSystem == Nothing) ] [ text "All systems..." ]
                                    :: List.map
                                        (\sys ->
                                            option
                                                [ value sys.name
                                                , selected (model.selectedSystem == Just sys.name)
                                                ]
                                                [ text (sys.name ++ " (" ++ String.fromInt sys.activityCount ++ ")") ]
                                        )
                                        systems
                                )
                            ]
                        ]

                currentValues =
                    case model.selectedSystem of
                        Nothing ->
                            []

                        Just sys ->
                            systems
                                |> List.filter (\s -> s.name == sys)
                                |> List.head
                                |> Maybe.map .values
                                |> Maybe.withDefault []

                filteredValues =
                    if String.isEmpty model.params.classificationValue then
                        []
                    else
                        List.filter (\v -> String.contains (String.toLower model.params.classificationValue) (String.toLower v)) currentValues

                suggestionsDropdown =
                    if List.isEmpty filteredValues then
                        text ""
                    else
                        div
                            [ style "position" "absolute"
                            , style "z-index" "10"
                            , style "background" "white"
                            , style "border" "1px solid #dbdbdb"
                            , style "border-radius" "4px"
                            , style "max-height" "200px"
                            , style "overflow-y" "auto"
                            , style "width" "100%"
                            , style "top" "100%"
                            , style "left" "0"
                            ]
                            (List.map
                                (\v ->
                                    div
                                        [ class "dropdown-item"
                                        , style "cursor" "pointer"
                                        , onClick (SelectClassificationValue (Just v))
                                        ]
                                        [ text v ]
                                )
                                filteredValues
                            )

                valueInput =
                    div [ style "display" "flex", style "flex-direction" "column" ]
                        [ label [ class "label is-small", style "margin-bottom" "0.25rem" ] [ text "Value" ]
                        , div [ style "position" "relative" ]
                            [ input
                                [ class "input is-small"
                                , type_ "text"
                                , placeholder "Filter by value..."
                                , value model.params.classificationValue
                                , onInput (\v -> SelectClassificationValue (if String.isEmpty v then Nothing else Just v))
                                , style "width" "180px"
                                ]
                                []
                            , suggestionsDropdown
                            ]
                        ]
            in
            [ systemDropdown, valueInput ]


viewFilterInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewFilterInput labelText placeholderText val toMsg =
    div [ style "display" "flex", style "flex-direction" "column" ]
        [ label [ class "label is-small", style "margin-bottom" "0.25rem" ] [ text labelText ]
        , input
            [ class "input is-small"
            , type_ "text"
            , placeholder placeholderText
            , value val
            , onInput toMsg
            , style "width" "150px"
            ]
            []
        ]



viewSupplyChain : Model -> Html Msg
viewSupplyChain model =
    case model.supplyChain of
        Loading ->
            viewLoading

        Failed err ->
            viewError err

        Loaded data ->
            let
                totalLoaded =
                    List.length data.supplyChain
            in
            div [ style "margin-top" "0.5rem" ]
                [ div [ style "display" "flex", style "justify-content" "space-between", style "align-items" "center", style "margin-bottom" "0.5rem", style "padding" "0 1.25rem" ]
                    [ div [ style "color" "#666", style "font-size" "0.9rem" ]
                        [ text
                            ("Showing "
                                ++ String.fromInt totalLoaded
                                ++ " of "
                                ++ String.fromInt data.filteredActivities
                                ++ " activities"
                                ++ (if data.filteredActivities /= data.totalActivities then
                                        " (filtered from " ++ String.fromInt data.totalActivities ++ ")"

                                    else
                                        ""
                                   )
                            )
                        ]
                    , div [ style "color" "#888", style "font-size" "0.85rem" ]
                        [ text ("per " ++ Format.formatScientific data.root.productAmount ++ " " ++ data.root.productUnit ++ " of " ++ data.root.product) ]
                    ]
                , viewTable model data.supplyChain
                , if totalLoaded < data.filteredActivities then
                    div [ class "has-text-centered", style "padding" "1rem 0" ]
                        [ button
                            [ class
                                (if model.loadingMore then
                                    "button is-primary is-loading"

                                 else
                                    "button is-primary"
                                )
                            , onClick LoadMore
                            , disabled model.loadingMore
                            ]
                            [ text "Load more" ]
                        ]

                  else
                    text ""
                ]

        NotAsked ->
            text ""


viewTable : Model -> List SupplyChainEntry -> Html Msg
viewTable model entries =
    table [ class "table is-striped is-hoverable is-fullwidth", style "font-size" "0.9rem" ]
        [ thead []
            [ tr []
                [ viewSortHeader model ByName "Activity" (style "width" "45%")
                , viewSortHeader model ByLocation "Location" (style "width" "8%")
                , viewSortHeader model ByAmount "Amount" (style "text-align" "right")
                , th [ style "width" "5%" ] [ text "Unit" ]
                , viewSortHeader model ByDepth "Depth" (style "text-align" "right")
                , viewSortHeader model ByConsumers "Consumers" (style "text-align" "right")
                ]
            ]
        , tbody [] (List.map viewEntry entries)
        ]


viewSortHeader : Model -> SortColumn -> String -> Html.Attribute Msg -> Html Msg
viewSortHeader model col label_ extraStyle =
    let
        arrow =
            if model.sortColumn == col then
                if model.sortAscending then
                    " \u{25B2}"

                else
                    " \u{25BC}"

            else
                ""
    in
    th [ onClick (SortBy col), style "cursor" "pointer", extraStyle ]
        [ text (label_ ++ arrow) ]


viewEntry : SupplyChainEntry -> Html Msg
viewEntry entry =
    tr [ style "cursor" "pointer", onClick (NavigateToActivity entry.processId) ]
        [ td []
            [ span [ style "color" "#2366d1" ] [ text entry.name ] ]
        , td [ style "color" "#888" ] [ text entry.location ]
        , td [ style "text-align" "right", style "font-family" "monospace" ]
            [ text (Format.formatScientific entry.quantity) ]
        , td [ style "color" "#888" ] [ text entry.unit ]
        , td [ style "text-align" "right" ] [ text (String.fromInt entry.depth) ]
        , td [ style "text-align" "right" ] [ text (String.fromInt entry.upstreamCount) ]
        ]


updateFilter : Shared.Model -> Model -> (Api.SupplyChainParams -> Api.SupplyChainParams) -> ( Model, Effect Shared.Msg Msg )
updateFilter _ model updateParams =
    let
        newParams =
            updateParams model.params

        newCounter =
            model.debounceCounter + 1
    in
    ( { model | params = newParams, debounceCounter = newCounter }
    , Effect.fromCmd (Process.sleep 100 |> Task.perform (\_ -> DebounceTick newCounter))
    )


toMaybe : String -> Maybe String
toMaybe s =
    if String.isEmpty s then
        Nothing

    else
        Just s


sortColumnToString : SortColumn -> String
sortColumnToString col =
    case col of
        ByName -> "name"
        ByLocation -> "location"
        ByAmount -> "amount"
        ByUnit -> "unit"
        ByDepth -> "depth"
        ByConsumers -> "consumers"


parseSortColumn : String -> SortColumn
parseSortColumn s =
    case s of
        "name" -> ByName
        "location" -> ByLocation
        "amount" -> ByAmount
        "unit" -> ByUnit
        "consumers" -> ByConsumers
        _ -> ByDepth


scrollToBottom : String -> Cmd Msg
scrollToBottom containerId =
    Browser.Dom.getViewportOf containerId
        |> Task.andThen (\info -> Browser.Dom.setViewportOf containerId 0 info.scene.height)
        |> Task.attempt (\_ -> ScrollDone)

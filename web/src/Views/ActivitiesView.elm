module Views.ActivitiesView exposing (viewActivitiesPage, Msg(..))

import Html exposing (Html, button, div, input, option, select, table, tbody, text, th, thead, tr, h2, p, span)
import Html.Attributes exposing (class, disabled, id, list, placeholder, selected, style, type_, value)
import Html.Events exposing (onInput, onClick, on)
import Json.Decode as Decode
import Models.Activity exposing (ActivitySummary, ClassificationSystem, SearchResults)
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)
import Views.ActivityRow as ActivityRow


type Msg
    = UpdateSearchQuery String
    | UpdateProductQuery String
    | SelectActivity String
    | LoadMore
    | SelectDatabase String
    | SelectClassificationSystem (Maybe String)
    | UpdatePendingValue String
    | CommitFilter
    | RemoveFilter Int


viewActivitiesPage : String -> String -> String -> Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Maybe String -> Maybe DatabaseList -> Maybe (List ClassificationSystem) -> List ( String, String ) -> Maybe String -> String -> Html Msg
viewActivitiesPage currentDbName searchQuery productQuery searchResults searchLoading loadingMore error maybeDatabaseList classificationSystems activeFilters pendingSystem pendingValue =
    div [ class "activities-page" ]
        [ div [ class "box" ]
            [ h2 [ class "title is-3" ] [ text "Search Activities" ]
            , p [ class "subtitle" ] [ text "Find activities by name and view their environmental inventory" ]
            , viewFiltersRow maybeDatabaseList currentDbName classificationSystems activeFilters pendingSystem pendingValue
            , viewSearchInputs searchQuery productQuery searchLoading
            , case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text ("Error: " ++ err) ]

                Nothing ->
                    text ""
            ]
        , viewSearchResults searchResults searchLoading loadingMore
        ]


viewSearchInputs : String -> String -> Bool -> Html Msg
viewSearchInputs query productQuery isLoading =
    div [ class "field is-grouped", style "margin-bottom" "0.75rem" ]
        [ div [ class "control has-icons-left is-expanded", class (if isLoading then "is-loading" else "") ]
            [ input
                [ id "activity-search"
                , class "input"
                , type_ "text"
                , placeholder "Search activities by name..."
                , value query
                , onInput UpdateSearchQuery
                ]
                []
            , span [ class "icon is-left" ]
                [ Html.i [ class "fas fa-search" ] []
                ]
            ]
        , div [ class "control has-icons-left is-expanded" ]
            [ input
                [ id "product-search"
                , class "input"
                , type_ "text"
                , placeholder "Filter by product..."
                , value productQuery
                , onInput UpdateProductQuery
                ]
                []
            , span [ class "icon is-left" ]
                [ Html.i [ class "fas fa-cube" ] []
                ]
            ]
        ]


viewFiltersRow : Maybe DatabaseList -> String -> Maybe (List ClassificationSystem) -> List ( String, String ) -> Maybe String -> String -> Html Msg
viewFiltersRow maybeDatabaseList currentDbName maybeSystems activeFilters pendingSystem pendingValue =
    let
        dbDropdown =
            case maybeDatabaseList of
                Nothing ->
                    []

                Just dbList ->
                    let
                        loadedDatabases =
                            List.filter (\db -> db.status == DbLoaded) dbList.databases
                    in
                    [ div [ class "control" ]
                        [ div [ class "select" ]
                            [ select
                                [ onInput SelectDatabase ]
                                (List.map (viewDatabaseOption currentDbName) loadedDatabases)
                            ]
                        ]
                    ]

        chips =
            List.indexedMap
                (\i ( sys, val ) ->
                    div [ class "control" ]
                        [ div [ class "tags has-addons" ]
                            [ span [ class "tag is-info is-large" ] [ text (sys ++ ": " ++ val) ]
                            , span
                                [ class "tag is-delete is-large"
                                , style "cursor" "pointer"
                                , onClick (RemoveFilter i)
                                ]
                                []
                            ]
                        ]
                )
                activeFilters

        classDropdown =
            case maybeSystems of
                Nothing ->
                    []

                Just systems ->
                    if List.isEmpty systems then
                        []

                    else
                        [ div [ class "control" ]
                            [ div [ class "select" ]
                                [ select
                                    [ onInput
                                        (\val ->
                                            if val == "" then
                                                SelectClassificationSystem Nothing

                                            else
                                                SelectClassificationSystem (Just val)
                                        )
                                    ]
                                    (option [ value "", selected (pendingSystem == Nothing) ] [ text "Classification..." ]
                                        :: List.map
                                            (\sys ->
                                                option
                                                    [ value sys.name
                                                    , selected (pendingSystem == Just sys.name)
                                                    ]
                                                    [ text (sys.name ++ " (" ++ String.fromInt sys.activityCount ++ ")") ]
                                            )
                                            systems
                                    )
                                ]
                            ]
                        ]

        valueInput =
            case ( maybeSystems, pendingSystem ) of
                ( Just _, Just sys ) ->
                    let
                        currentValues =
                            maybeSystems
                                |> Maybe.withDefault []
                                |> List.filter (\s -> s.name == sys)
                                |> List.head
                                |> Maybe.map .values
                                |> Maybe.withDefault []

                        datalistId =
                            "classification-values"
                    in
                    [ div [ class "control is-expanded" ]
                        [ input
                            [ class "input"
                            , type_ "text"
                            , placeholder "Filter by classification value..."
                            , value pendingValue
                            , list datalistId
                            , onInput UpdatePendingValue
                            , on "keydown" (Decode.andThen (\key -> if key == "Enter" then Decode.succeed CommitFilter else Decode.fail "not enter") (Decode.field "key" Decode.string))
                            ]
                            []
                        , Html.node "datalist"
                            [ id datalistId ]
                            (List.map (\v -> option [ value v ] []) currentValues)
                        ]
                    ]

                _ ->
                    []

        addButton =
            case pendingSystem of
                Just _ ->
                    if String.isEmpty pendingValue then
                        []

                    else
                        [ div [ class "control" ]
                            [ button
                                [ class "button is-info"
                                , onClick CommitFilter
                                ]
                                [ span [ class "icon" ] [ Html.i [ class "fas fa-plus" ] [] ]
                                , span [] [ text "Add" ]
                                ]
                            ]
                        ]

                Nothing ->
                    []

        allControls =
            dbDropdown ++ chips ++ classDropdown ++ valueInput ++ addButton
    in
    if List.isEmpty allControls then
        text ""

    else
        div [ class "field is-grouped is-grouped-multiline", style "margin-bottom" "0.5rem" ]
            allControls


viewSearchResults : Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Html Msg
viewSearchResults maybeResults isLoading loadingMore =
    case ( isLoading, maybeResults ) of
        ( True, Nothing ) ->
            div [ class "has-text-centered" ]
                [ div [ class "is-size-5 has-text-grey" ] [ text "Searching..." ]
                ]

        ( _, Nothing ) ->
            text ""

        ( _, Just results ) ->
            if List.isEmpty results.results then
                div [ class "has-text-centered" ]
                    [ div [ class "is-size-5 has-text-grey" ] [ text "No activities found" ]
                    ]
            else
                div []
                    [ div [ style "padding" "0.5rem 0" ]
                        [ span [ class "tag is-info is-light" ]
                            [ text (String.fromInt (List.length results.results) ++ " / " ++ String.fromInt results.totalCount ++ " activities")
                            ]
                        ]
                    , table [ class "table is-striped is-hoverable is-fullwidth" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "Activity Name" ]
                                , th [ class "has-text-right" ] [ text "Amount" ]
                                , th [] [ text "Unit" ]
                                , th [] [ text "Product" ]
                                , th [] [ text "Location" ]
                                ]
                            ]
                        , tbody []
                            (List.map viewActivityRow results.results)
                        ]
                    , if results.hasMore then
                        div [ class "has-text-centered", style "padding" "1rem 0" ]
                            [ button
                                [ class (if loadingMore then "button is-primary is-loading" else "button is-primary")
                                , onClick LoadMore
                                , disabled loadingMore
                                ]
                                [ text "Load more" ]
                            ]
                      else
                        text ""
                    ]


viewActivityRow : ActivitySummary -> Html Msg
viewActivityRow activity =
    ActivityRow.viewActivityRow
        { id = Just activity.id
        , name = activity.name
        , amount = activity.productAmount
        , unit = activity.productUnit
        , product = activity.product
        , location = activity.location
        , onNavigate = SelectActivity
        }


viewDatabaseOption : String -> DatabaseStatus -> Html Msg
viewDatabaseOption currentDbName db =
    option
        [ value db.name
        , selected (db.name == currentDbName)
        ]
        [ text db.displayName ]

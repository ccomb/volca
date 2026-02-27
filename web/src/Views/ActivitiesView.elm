module Views.ActivitiesView exposing (viewActivitiesPage, Msg(..))

import Html exposing (Html, button, div, input, option, select, table, tbody, text, th, thead, tr, h2, p, span)
import Html.Attributes exposing (class, disabled, id, placeholder, selected, style, type_, value)
import Html.Events exposing (onInput, onClick)
import Models.Activity exposing (ActivitySummary, SearchResults)
import Models.Database exposing (DatabaseList, DatabaseStatus)
import Views.ActivityRow as ActivityRow


type Msg
    = UpdateSearchQuery String
    | SelectActivity String
    | LoadMore
    | SelectDatabase String


viewActivitiesPage : String -> String -> Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Maybe String -> Maybe DatabaseList -> Html Msg
viewActivitiesPage currentDbName searchQuery searchResults searchLoading loadingMore error maybeDatabaseList =
    div [ class "activities-page" ]
        [ div [ class "box" ]
            [ h2 [ class "title is-3" ] [ text "Search Activities" ]
            , p [ class "subtitle" ] [ text "Find activities by name and view their environmental inventory" ]
            , viewSearchBar maybeDatabaseList currentDbName searchQuery searchLoading
            , case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text ("Error: " ++ err) ]

                Nothing ->
                    text ""
            ]
        , viewSearchResults searchResults searchLoading loadingMore
        ]


viewSearchBar : Maybe DatabaseList -> String -> String -> Bool -> Html Msg
viewSearchBar maybeDatabaseList currentDbName query isLoading =
    div [ class "field is-grouped is-grouped-multiline", style "margin-bottom" "1.5rem" ]
        [ -- Database selector on the left
          case maybeDatabaseList of
            Nothing ->
                text ""

            Just dbList ->
                let
                    loadedDatabases =
                        List.filter .loaded dbList.databases
                in
                div [ class "control" ]
                    [ div [ class "select is-large" ]
                        [ select
                            [ onInput SelectDatabase ]
                            (List.map (viewDatabaseOption currentDbName) loadedDatabases)
                        ]
                    ]
        , -- Search input on the right (expanded)
          div [ class "control has-icons-left is-expanded", class (if isLoading then "is-loading" else "") ]
            [ input
                [ id "activity-search"
                , class "input is-large"
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
        ]


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
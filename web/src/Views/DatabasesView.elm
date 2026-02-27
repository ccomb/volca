module Views.DatabasesView exposing (Msg(..), viewDatabasesPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)


type Msg
    = NavigateToDatabase String
    | LoadDatabase String
    | UnloadDatabase String
    | DeleteDatabase String
    | ConfirmDeleteDatabase String
    | CancelDeleteDatabase
    | SetupDatabase String


viewDatabasesPage : Maybe DatabaseList -> Bool -> Maybe String -> Maybe String -> Html Msg
viewDatabasesPage maybeDatabases loading error confirmingDelete =
    div [ class "databases-page" ]
        [ div [ class "box" ]
            [ div [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ h2 [ class "title is-3", style "margin-bottom" "0" ] [ text "Databases" ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ a [ href "/databases/upload", class "button is-primary" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-plus" ] [] ]
                            , span [] [ text "Add database" ]
                            ]
                        ]
                    ]
                ]
            , p [ class "subtitle" ] [ text "Manage your LCA databases" ]
            , case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text err ]

                Nothing ->
                    text ""
            ]
        , case ( loading, maybeDatabases ) of
            ( True, _ ) ->
                div [ class "has-text-centered", style "padding" "2rem" ]
                    [ div [ class "is-size-4" ] [ text "Loading databases..." ]
                    , progress [ class "progress is-primary", attribute "max" "100" ] []
                    ]

            ( False, Just dbList ) ->
                viewDatabasesList dbList confirmingDelete

            ( False, Nothing ) ->
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No database information available" ]
        ]


viewDatabasesList : DatabaseList -> Maybe String -> Html Msg
viewDatabasesList dbList confirmingDelete =
    let
        dbCount =
            List.length dbList.databases
    in
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt dbCount ++ " databases") ]
            ]
        , table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ thead []
                [ tr []
                    [ th [ style "width" "50px" ] [ text "" ]
                    , th [ style "width" "200px" ] [ text "Actions" ]
                    , th [] [ text "Name" ]
                    , th [] [ text "Description" ]
                    , th [ style "width" "100px" ] [ text "Source" ]
                    , th [ style "width" "120px" ] [ text "Format" ]
                    ]
                ]
            , tbody []
                (List.map (viewDatabaseRow confirmingDelete) dbList.databases)
            ]
        ]


viewDatabaseRow : Maybe String -> DatabaseStatus -> Html Msg
viewDatabaseRow confirmingDelete db =
    let
        isInMemory =
            db.status /= Unloaded

        canUnload =
            isInMemory

        canDelete =
            db.isUploaded && not isInMemory

        statusIndicator =
            case db.status of
                DbLoaded ->
                    span [ class "has-text-success", style "font-size" "1.5rem" ] [ text "●" ]

                PartiallyLinked ->
                    span [ class "has-text-warning", style "font-size" "1.5rem" ] [ text "●" ]

                Unloaded ->
                    span [ class "has-text-grey-lighter", style "font-size" "1.5rem" ] [ text "○" ]

        rowAttrs =
            case db.status of
                DbLoaded ->
                    [ class "is-clickable"
                    , style "cursor" "pointer"
                    , onClick (NavigateToDatabase db.name)
                    ]

                _ ->
                    []
    in
    tr rowAttrs
        [ td [ style "text-align" "center", style "vertical-align" "middle" ]
            [ statusIndicator ]
        , td []
            [ viewActionButtons db canUnload canDelete (confirmingDelete == Just db.name) ]
        , td []
            [ text db.displayName ]
        , td [ class "has-text-grey" ]
            [ text (db.description |> Maybe.withDefault "") ]
        , td [ class "has-text-grey" ]
            [ if db.isUploaded then
                text "Uploaded"

              else
                text "Preinstalled"
            ]
        , td [ class "has-text-grey" ]
            [ text (db.format |> Maybe.withDefault "") ]
        ]


viewActionButtons : DatabaseStatus -> Bool -> Bool -> Bool -> Html Msg
viewActionButtons db canUnload canDelete isConfirming =
    div [ class "buttons are-small" ]
        [ case db.status of
            Unloaded ->
                button
                    [ class "button is-primary is-small"
                    , stopPropagationOn "click"
                        (Decode.succeed
                            ( if db.isUploaded then
                                SetupDatabase db.name

                              else
                                LoadDatabase db.name
                            , True
                            )
                        )
                    ]
                    [ span [ class "icon is-small" ] [ i [ class "fas fa-folder-open" ] [] ]
                    , span [] [ text "Open" ]
                    ]

            PartiallyLinked ->
                span [ class "buttons are-small", style "display" "inline-flex" ]
                    [ button
                        [ class "button is-info is-small"
                        , stopPropagationOn "click" (Decode.succeed ( SetupDatabase db.name, True ))
                        ]
                        [ span [ class "icon is-small" ] [ i [ class "fas fa-info-circle" ] [] ]
                        , span [] [ text "Info" ]
                        ]
                    , button
                        [ class "button is-warning is-small"
                        , stopPropagationOn "click" (Decode.succeed ( UnloadDatabase db.name, True ))
                        ]
                        [ span [ class "icon is-small" ] [ i [ class "fas fa-times" ] [] ]
                        , span [] [ text "Close" ]
                        ]
                    ]

            DbLoaded ->
                span [ class "buttons are-small", style "display" "inline-flex" ]
                    [ button
                        [ class "button is-info is-small is-outlined"
                        , stopPropagationOn "click" (Decode.succeed ( SetupDatabase db.name, True ))
                        ]
                        [ span [ class "icon is-small" ] [ i [ class "fas fa-info-circle" ] [] ]
                        , span [] [ text "Info" ]
                        ]
                    , button
                        [ class "button is-warning is-small"
                        , stopPropagationOn "click" (Decode.succeed ( UnloadDatabase db.name, True ))
                        ]
                        [ span [ class "icon is-small" ] [ i [ class "fas fa-times" ] [] ]
                        , span [] [ text "Close" ]
                        ]
                    ]
        , if canDelete then
            if isConfirming then
                span [ class "buttons are-small", style "display" "inline-flex" ]
                    [ span [ class "has-text-danger", style "font-weight" "bold", style "margin-right" "0.25rem", style "line-height" "2em" ] [ text "Delete?" ]
                    , button
                        [ class "button is-danger is-small"
                        , stopPropagationOn "click" (Decode.succeed ( DeleteDatabase db.name, True ))
                        ]
                        [ text "Yes" ]
                    , button
                        [ class "button is-light is-small"
                        , stopPropagationOn "click" (Decode.succeed ( CancelDeleteDatabase, True ))
                        ]
                        [ text "No" ]
                    ]

            else
                button
                    [ class "button is-danger is-small is-outlined"
                    , stopPropagationOn "click" (Decode.succeed ( ConfirmDeleteDatabase db.name, True ))
                    ]
                    [ span [ class "icon is-small" ] [ i [ class "fas fa-trash" ] [] ]
                    ]

          else
            text ""
        ]

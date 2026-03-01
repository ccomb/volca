module Views.DatabasesView exposing (Msg(..), viewDatabasesPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)
import Set exposing (Set)


type Msg
    = NavigateToDatabase String
    | LoadDatabase String
    | UnloadDatabase String
    | DeleteDatabase String
    | ConfirmDeleteDatabase String
    | CancelDeleteDatabase
    | SetupDatabase String


viewDatabasesPage : Maybe DatabaseList -> Maybe String -> Maybe String -> Set String -> Maybe String -> Maybe String -> List String -> Html Msg
viewDatabasesPage maybeDatabases error confirmingDelete loadingDbs unloadingDb deletingDb progressLines =
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
        , case maybeDatabases of
            Just dbList ->
                viewDatabasesList dbList confirmingDelete loadingDbs unloadingDb deletingDb progressLines

            Nothing ->
                div [ class "has-text-centered", style "padding" "2rem" ]
                    [ span [ class "icon is-large has-text-primary" ]
                        [ i [ class "fas fa-spinner fa-spin fa-2x" ] [] ]
                    ]
        ]


viewDatabasesList : DatabaseList -> Maybe String -> Set String -> Maybe String -> Maybe String -> List String -> Html Msg
viewDatabasesList dbList confirmingDelete loadingDbs unloadingDb deletingDb progressLines =
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
                (List.concatMap (viewDatabaseRowWithProgress confirmingDelete loadingDbs unloadingDb deletingDb progressLines) dbList.databases)
            ]
        ]


viewDatabaseRowWithProgress : Maybe String -> Set String -> Maybe String -> Maybe String -> List String -> DatabaseStatus -> List (Html Msg)
viewDatabaseRowWithProgress confirmingDelete loadingDbs unloadingDb deletingDb progressLines db =
    let
        isLoading =
            Set.member db.name loadingDbs

        lastLines =
            List.reverse progressLines |> List.take 3 |> List.reverse
    in
    viewDatabaseRow confirmingDelete loadingDbs unloadingDb deletingDb db
        :: (if isLoading && not (List.isEmpty lastLines) then
                [ tr []
                    [ td [ attribute "colspan" "6", style "padding" "0.25rem 1rem 0.75rem", style "border-top" "none" ]
                        [ div
                            [ style "font-family" "'Consolas', 'Monaco', monospace"
                            , style "font-size" "0.75rem"
                            , style "color" "#7a7a7a"
                            , style "line-height" "1.5"
                            ]
                            (List.map (\line -> div [] [ text line ]) lastLines)
                        ]
                    ]
                ]

            else
                []
           )


viewDatabaseRow : Maybe String -> Set String -> Maybe String -> Maybe String -> DatabaseStatus -> Html Msg
viewDatabaseRow confirmingDelete loadingDbs unloadingDb deletingDb db =
    let
        isLoading =
            Set.member db.name loadingDbs

        isUnloading =
            unloadingDb == Just db.name

        isDeleting =
            deletingDb == Just db.name

        canDelete =
            db.isUploaded && db.status == Unloaded

        statusIndicator =
            if isLoading then
                span [ class "has-text-primary", style "font-size" "1.5rem" ]
                    [ i [ class "fas fa-spinner fa-spin" ] [] ]

            else
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
            [ if isLoading then
                span [ class "has-text-grey is-size-7" ] [ text "Loading..." ]

              else
                viewActionButtons db canDelete (confirmingDelete == Just db.name) isUnloading isDeleting
            ]
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


viewActionButtons : DatabaseStatus -> Bool -> Bool -> Bool -> Bool -> Html Msg
viewActionButtons db canDelete isConfirming isUnloading isDeleting =
    let
        closeButton =
            button
                [ class
                    ("button is-warning is-small"
                        ++ (if isUnloading then
                                " is-loading"

                            else
                                ""
                           )
                    )
                , Html.Attributes.disabled isUnloading
                , stopPropagationOn "click" (Decode.succeed ( UnloadDatabase db.name, True ))
                ]
                [ span [ class "icon is-small" ] [ i [ class "fas fa-times" ] [] ]
                , span [] [ text "Close" ]
                ]
    in
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
                    , closeButton
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
                    , closeButton
                    ]
        , if canDelete then
            if isConfirming then
                span [ class "buttons are-small", style "display" "inline-flex" ]
                    [ span [ class "has-text-danger", style "font-weight" "bold", style "margin-right" "0.25rem", style "line-height" "2em" ] [ text "Delete?" ]
                    , button
                        [ class
                            ("button is-danger is-small"
                                ++ (if isDeleting then
                                        " is-loading"

                                    else
                                        ""
                                   )
                            )
                        , Html.Attributes.disabled isDeleting
                        , stopPropagationOn "click" (Decode.succeed ( DeleteDatabase db.name, True ))
                        ]
                        [ text "Yes" ]
                    , button
                        [ class "button is-light is-small"
                        , Html.Attributes.disabled isDeleting
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



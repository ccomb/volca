module Views.DatabaseSetupView exposing (Msg(..), viewDatabaseSetupPage)

{-| Database Setup Page

This page allows users to configure cross-database dependencies before
finalizing a database for use. It shows:

  - Supply chain completeness progress bar
  - Missing suppliers list
  - Dependency selection with match counts
  - "Ready for Analysis" button

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.Database exposing (DataPathCandidate, DatabaseSetupInfo, DependencySuggestion, LocationFallback, MissingSupplier)


type Msg
    = AddDependency String
    | RemoveDependency String
    | SetDataPath String
    | FinalizeDatabase
    | GoBack


viewDatabaseSetupPage : Maybe DatabaseSetupInfo -> Bool -> Maybe String -> Html Msg
viewDatabaseSetupPage maybeSetupInfo loading error =
    div [ class "database-setup-page" ]
        [ viewHeader maybeSetupInfo
        , case error of
            Just err ->
                div [ class "notification is-danger", style "margin" "1rem" ]
                    [ text err ]

            Nothing ->
                text ""
        , case ( loading, maybeSetupInfo, error ) of
            ( True, _, _ ) ->
                viewLoading

            ( False, Just setupInfo, _ ) ->
                viewSetupContent setupInfo

            ( False, Nothing, Just _ ) ->
                text ""

            ( False, Nothing, Nothing ) ->
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No setup information available" ]
        ]


viewHeader : Maybe DatabaseSetupInfo -> Html Msg
viewHeader maybeSetupInfo =
    div [ class "box" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ button [ class "button is-text", onClick GoBack ]
                        [ span [ class "icon" ] [ i [ class "fas fa-arrow-left" ] [] ]
                        ]
                    ]
                , div [ class "level-item" ]
                    [ h2 [ class "title is-3", style "margin-bottom" "0" ]
                        [ text
                            (maybeSetupInfo
                                |> Maybe.map .displayName
                                |> Maybe.withDefault "Database Setup"
                            )
                        ]
                    ]
                ]
            ]
        , p [ class "subtitle" ] [ text "Configure supply chain dependencies" ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "has-text-centered", style "padding" "2rem" ]
        [ div [ class "is-size-4" ] [ text "Loading setup information..." ]
        , progress [ class "progress is-primary", attribute "max" "100" ] []
        ]


viewSetupContent : DatabaseSetupInfo -> Html Msg
viewSetupContent setupInfo =
    div [ style "padding" "1rem" ]
        [ div [ class "columns" ]
            [ div [ class "column is-8" ]
                [ viewDataSourceCard setupInfo
                , viewCompletenessCard setupInfo
                , viewUnknownUnitsCard setupInfo
                , viewLocationFallbacksCard setupInfo
                , viewMissingSuppliersCard setupInfo
                ]
            , div [ class "column is-4" ]
                [ viewDependenciesCard setupInfo
                , viewFinalizeCard setupInfo
                ]
            ]
        ]


viewDataSourceCard : DatabaseSetupInfo -> Html Msg
viewDataSourceCard setupInfo =
    if List.length setupInfo.availablePaths <= 1 then
        text ""

    else
        div [ class "card", style "margin-bottom" "1rem" ]
            [ div [ class "card-content" ]
                [ h3 [ class "title is-5" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-folder-open" ] [] ]
                        , span [] [ text "Data Source" ]
                        ]
                    ]
                , div [ class "content" ]
                    [ p [ class "has-text-grey is-size-7", style "margin-bottom" "0.5rem" ]
                        [ text "Multiple data sources found in this upload:" ]
                    , div [] (List.map (viewDataPathOption setupInfo.dataPath) setupInfo.availablePaths)
                    ]
                ]
            ]


viewDataPathOption : String -> DataPathCandidate -> Html Msg
viewDataPathOption currentPath candidate =
    let
        isSelected =
            String.endsWith candidate.path currentPath
                || String.endsWith candidate.path (String.replace "\\" "/" currentPath)
    in
    div
        [ class
            (if isSelected then
                "box has-background-primary-light"

             else
                "box is-clickable"
            )
        , style "padding" "0.75rem"
        , style "margin-bottom" "0.5rem"
        , style "cursor"
            (if isSelected then
                "default"

             else
                "pointer"
            )
        , if isSelected then
            class ""

          else
            onClick (SetDataPath candidate.path)
        ]
        [ div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ if isSelected then
                        span [ class "icon has-text-primary" ] [ i [ class "fas fa-check-circle" ] [] ]

                      else
                        span [ class "icon has-text-grey-light" ] [ i [ class "far fa-circle" ] [] ]
                    , span [ class "has-text-weight-semibold", style "margin-left" "0.25rem" ]
                        [ text candidate.path ]
                    ]
                ]
            , div [ class "level-right" ]
                [ div [ class "level-item" ]
                    [ span [ class "tag is-info is-light", style "margin-right" "0.5rem" ]
                        [ text candidate.format ]
                    , span [ class "tag is-light" ]
                        [ text (String.fromInt candidate.fileCount ++ " files") ]
                    ]
                ]
            ]
        ]


viewCompletenessCard : DatabaseSetupInfo -> Html Msg
viewCompletenessCard setupInfo =
    let
        completenessDisplay =
            let
                whole =
                    floor setupInfo.completeness

                decimal =
                    round ((setupInfo.completeness - toFloat whole) * 10)

                ( adjustedWhole, adjustedDecimal ) =
                    if decimal >= 10 then
                        ( whole + 1, 0 )

                    else
                        ( whole, decimal )
            in
            String.fromInt adjustedWhole ++ "," ++ String.fromInt adjustedDecimal ++ "%"

        progressValue =
            floor setupInfo.completeness

        progressColor =
            if setupInfo.unresolvedLinks == 0 then
                "is-success"

            else if setupInfo.completeness >= 50 then
                "is-warning"

            else
                "is-danger"
    in
    div [ class "card", style "margin-bottom" "1rem" ]
        [ div [ class "card-content" ]
            [ h3 [ class "title is-5" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-link" ] [] ]
                    , span [] [ text "Supply Chain Completeness" ]
                    ]
                ]
            , div [ class "content" ]
                [ div [ class "level", style "margin-bottom" "0.5rem" ]
                    [ div [ class "level-left" ]
                        [ span [ class "is-size-3 has-text-weight-bold" ]
                            [ text completenessDisplay ]
                        ]
                    , div [ class "level-right" ]
                        [ span [ class "has-text-grey" ]
                            [ text
                                (String.fromInt (setupInfo.internalLinks + setupInfo.crossDBLinks)
                                    ++ " / "
                                    ++ String.fromInt setupInfo.inputCount
                                    ++ " inputs resolved"
                                )
                            ]
                        ]
                    ]
                , progress
                    [ class ("progress " ++ progressColor)
                    , attribute "max" "100"
                    , attribute "value" (String.fromInt progressValue)
                    ]
                    []
                , div [ class "tags", style "margin-top" "0.5rem" ]
                    [ span [ class "tag is-info is-light" ]
                        [ text (String.fromInt setupInfo.activityCount ++ " activities") ]
                    , span [ class "tag is-success is-light" ]
                        [ text (String.fromInt setupInfo.internalLinks ++ " internal") ]
                    , span [ class "tag is-primary is-light" ]
                        [ text (String.fromInt setupInfo.crossDBLinks ++ " cross-DB") ]
                    , if setupInfo.unresolvedLinks > 0 then
                        span [ class "tag is-warning is-light" ]
                            [ text (String.fromInt setupInfo.unresolvedLinks ++ " unresolved") ]

                      else
                        text ""
                    ]
                ]
            ]
        ]


viewMissingSuppliersCard : DatabaseSetupInfo -> Html Msg
viewMissingSuppliersCard setupInfo =
    div [ class "card", style "margin-bottom" "1rem" ]
        [ div [ class "card-content" ]
            [ h3 [ class "title is-5" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-exclamation-triangle" ] [] ]
                    , span [] [ text "Missing Suppliers" ]
                    ]
                ]
            , if List.isEmpty setupInfo.missingSuppliers then
                div [ class "notification is-success is-light" ]
                    [ text "All suppliers resolved!" ]

              else
                div [ class "content" ]
                    [ p [ class "has-text-grey" ]
                        [ text "These products need suppliers from other databases:" ]
                    , table [ class "table is-striped is-fullwidth is-narrow" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "Product" ]
                                , th [ style "width" "180px" ] [ text "Reason" ]
                                , th [ style "width" "100px" ] [ text "Needed by" ]
                                ]
                            ]
                        , tbody []
                            (List.map viewMissingSupplierRow setupInfo.missingSuppliers)
                        ]
                    ]
            ]
        ]


viewMissingSupplierRow : MissingSupplier -> Html Msg
viewMissingSupplierRow supplier =
    tr []
        [ td []
            [ text supplier.productName
            , case supplier.location of
                Just loc ->
                    span [ class "has-text-grey is-size-7", style "margin-left" "0.5rem" ]
                        [ text ("(" ++ loc ++ ")") ]

                Nothing ->
                    text ""
            ]
        , td [] [ viewReasonTag supplier.reason supplier.detail ]
        , td [ class "has-text-right" ]
            [ span [ class "tag is-light" ]
                [ text (String.fromInt supplier.count ++ " activities") ]
            ]
        ]


viewReasonTag : String -> Maybe String -> Html Msg
viewReasonTag reason detail =
    case reason of
        "unit_incompatible" ->
            span [ class "tag is-danger is-light" ]
                [ text ("Unit: " ++ Maybe.withDefault "" detail) ]

        "location_unavailable" ->
            span [ class "tag is-warning is-light" ]
                [ text ("Location: " ++ Maybe.withDefault "" detail) ]

        "no_name_match" ->
            span [ class "tag is-light" ] [ text "Not found" ]

        _ ->
            span [ class "tag is-light" ] [ text reason ]


viewUnknownUnitsCard : DatabaseSetupInfo -> Html Msg
viewUnknownUnitsCard setupInfo =
    if List.isEmpty setupInfo.unknownUnits then
        text ""

    else
        div [ class "card", style "margin-bottom" "1rem" ]
            [ div [ class "card-content" ]
                [ h3 [ class "title is-5" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon has-text-warning" ] [ i [ class "fas fa-question-circle" ] [] ]
                        , span [] [ text "Unknown Units" ]
                        ]
                    ]
                , div [ class "notification is-warning is-light" ]
                    [ p [] [ text "These units are not recognized and may prevent cross-database linking:" ]
                    , div [ class "tags", style "margin-top" "0.5rem" ]
                        (List.map (\u -> span [ class "tag is-warning" ] [ text u ]) setupInfo.unknownUnits)
                    , p [ class "is-size-7 has-text-grey", style "margin-top" "0.5rem" ]
                        [ text "Add them to [units.aliases] in fplca.toml" ]
                    ]
                ]
            ]


viewLocationFallbacksCard : DatabaseSetupInfo -> Html Msg
viewLocationFallbacksCard setupInfo =
    if List.isEmpty setupInfo.locationFallbacks then
        text ""

    else
        div [ class "card", style "margin-bottom" "1rem" ]
            [ div [ class "card-content" ]
                [ h3 [ class "title is-5" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon has-text-info" ] [ i [ class "fas fa-map-marker-alt" ] [] ]
                        , span [] [ text "Location Fallbacks" ]
                        ]
                    ]
                , div [ class "notification is-info is-light" ]
                    [ p [] [ text "These products were linked using a wider geography than requested:" ]
                    , table [ class "table is-striped is-fullwidth is-narrow", style "margin-top" "0.5rem" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "Product" ]
                                , th [ style "width" "200px" ] [ text "Location" ]
                                ]
                            ]
                        , tbody []
                            (List.map viewLocationFallbackRow setupInfo.locationFallbacks)
                        ]
                    ]
                ]
            ]


viewLocationFallbackRow : LocationFallback -> Html Msg
viewLocationFallbackRow fb =
    tr []
        [ td [] [ text fb.product ]
        , td []
            [ span [ class "tag is-light" ] [ text fb.requested ]
            , span [ class "icon is-small has-text-grey", style "margin" "0 0.25rem" ]
                [ i [ class "fas fa-arrow-right" ] [] ]
            , span [ class "tag is-info is-light" ] [ text fb.actual ]
            ]
        ]


viewDependenciesCard : DatabaseSetupInfo -> Html Msg
viewDependenciesCard setupInfo =
    div [ class "card", style "margin-bottom" "1rem" ]
        [ div [ class "card-content" ]
            [ h3 [ class "title is-5" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-database" ] [] ]
                    , span [] [ text "Dependencies" ]
                    ]
                ]
            , div [ class "content" ]
                [ if List.isEmpty setupInfo.selectedDependencies then
                    p [ class "has-text-grey" ] [ text "No dependencies selected" ]

                  else
                    div []
                        [ p [ class "has-text-grey is-size-7", style "margin-bottom" "0.5rem" ]
                            [ text "Using data from:" ]
                        , div [ class "tags" ]
                            (List.map viewSelectedDependency setupInfo.selectedDependencies)
                        ]
                , if List.isEmpty setupInfo.suggestions && List.isEmpty setupInfo.selectedDependencies then
                    p [ class "has-text-grey is-size-7", style "margin-top" "0.5rem" ]
                        [ text "Load a database first, then add it here as a dependency." ]

                  else if List.isEmpty setupInfo.suggestions then
                    text ""

                  else
                    div [ style "margin-top" "1rem" ]
                        [ p [ class "has-text-grey is-size-7", style "margin-bottom" "0.5rem" ]
                            [ text "Available databases:" ]
                        , div []
                            (List.map viewDependencySuggestion setupInfo.suggestions)
                        ]
                ]
            ]
        ]


viewSelectedDependency : String -> Html Msg
viewSelectedDependency depName =
    span [ class "tag is-success is-medium" ]
        [ text depName
        , button
            [ class "delete is-small"
            , onClick (RemoveDependency depName)
            ]
            []
        ]


viewDependencySuggestion : DependencySuggestion -> Html Msg
viewDependencySuggestion suggestion =
    div
        [ class "box is-clickable"
        , style "padding" "0.75rem"
        , style "margin-bottom" "0.5rem"
        , style "cursor" "pointer"
        , onClick (AddDependency suggestion.databaseName)
        ]
        [ div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ span [ class "icon has-text-primary" ]
                        [ i [ class "fas fa-plus-circle" ] [] ]
                    , span [ class "has-text-weight-semibold" ]
                        [ text suggestion.displayName ]
                    ]
                ]
            , div [ class "level-right" ]
                [ div [ class "level-item" ]
                    [ span [ class "tag is-info is-light" ]
                        [ text (String.fromInt suggestion.matchCount ++ " products") ]
                    ]
                ]
            ]
        ]


viewFinalizeCard : DatabaseSetupInfo -> Html Msg
viewFinalizeCard setupInfo =
    div [ class "card" ]
        [ div [ class "card-content" ]
            [ h3 [ class "title is-5" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-check-circle" ] [] ]
                    , span [] [ text "Finalize" ]
                    ]
                ]
            , div [ class "content" ]
                [ if setupInfo.isReady then
                    div []
                        [ p [ class "has-text-success" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-check" ] [] ]
                            , text " Database is ready for analysis"
                            ]
                        , button
                            [ class "button is-primary is-fullwidth", style "margin-top" "1rem"
                            , onClick FinalizeDatabase
                            ]
                            [ span [ class "icon" ] [ i [ class "fas fa-rocket" ] [] ]
                            , span [] [ text "Ready for Analysis" ]
                            ]
                        ]

                  else if setupInfo.activityCount == 0 then
                    div []
                        [ p [ class "has-text-danger" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-exclamation-circle" ] [] ]
                            , text " No activities found. The data file may be corrupted or in an unsupported format."
                            ]
                        , button
                            [ class "button is-fullwidth is-outlined", style "margin-top" "1rem"
                            , attribute "disabled" "disabled"
                            ]
                            [ span [ class "icon" ] [ i [ class "fas fa-ban" ] [] ]
                            , span [] [ text "Cannot finalize (0 activities)" ]
                            ]
                        ]

                  else
                    div []
                        [ p [ class "has-text-warning" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-exclamation-triangle" ] [] ]
                            , text " Add dependencies to resolve all inputs before finalizing"
                            ]
                        , button
                            [ class "button is-fullwidth is-outlined", style "margin-top" "1rem"
                            , attribute "disabled" "disabled"
                            ]
                            [ span [ class "icon" ] [ i [ class "fas fa-lock" ] [] ]
                            , span [] [ text ("" ++ String.fromInt setupInfo.unresolvedLinks ++ " unresolved inputs") ]
                            ]
                        ]
                ]
            ]
        ]

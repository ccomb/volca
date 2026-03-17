module Views.DatabaseSetupView exposing (Msg(..), viewDatabaseSetupPage)

{-| Database Setup Page

This page allows users to configure cross-database dependencies before
finalizing a database for use. It shows:

  - Supply chain completeness progress bar
  - Missing suppliers list
  - Dependency selection with match counts
  - Link to method mapping analyzer
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
    | GoToActivities
    | GoToMapping


viewDatabaseSetupPage : String -> Maybe DatabaseSetupInfo -> Maybe String -> List String -> Html Msg
viewDatabaseSetupPage dbName maybeSetupInfo error progressLines =
    div [ class "database-setup-page" ]
        [ viewHeader dbName maybeSetupInfo
        , case error of
            Just err ->
                div [ class "notification is-danger", style "margin" "1rem" ]
                    [ text err ]

            Nothing ->
                text ""
        , case maybeSetupInfo of
            Just setupInfo ->
                viewSetupContent setupInfo

            Nothing ->
                if error == Nothing then
                    let
                        lastLines =
                            List.reverse progressLines |> List.take 3 |> List.reverse
                    in
                    div [ class "has-text-centered", style "padding" "3rem" ]
                        [ span [ class "icon is-large has-text-primary" ]
                            [ i [ class "fas fa-spinner fa-spin fa-2x" ] [] ]
                        , p [ class "has-text-grey", style "margin-top" "1rem" ]
                            [ text "Loading database information..." ]
                        , if not (List.isEmpty lastLines) then
                            div
                                [ style "margin-top" "1rem"
                                , style "font-family" "'Consolas', 'Monaco', monospace"
                                , style "font-size" "0.75rem"
                                , style "color" "#7a7a7a"
                                , style "line-height" "1.5"
                                , style "text-align" "left"
                                , style "max-width" "600px"
                                , style "margin-left" "auto"
                                , style "margin-right" "auto"
                                ]
                                (List.map (\line -> div [] [ text line ]) lastLines)

                          else
                            text ""
                        ]

                else
                    text ""
        ]


viewHeader : String -> Maybe DatabaseSetupInfo -> Html Msg
viewHeader dbName maybeSetupInfo =
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
                                |> Maybe.withDefault dbName
                            )
                        ]
                    ]
                ]
            ]
        , p [ class "subtitle" ]
            [ text
                (maybeSetupInfo
                    |> Maybe.map
                        (\info ->
                            if info.isLoaded then
                                "Database information"

                            else
                                "Configure supply chain dependencies"
                        )
                    |> Maybe.withDefault "Configure supply chain dependencies"
                )
            ]
        ]


viewSetupContent : DatabaseSetupInfo -> Html Msg
viewSetupContent setupInfo =
    div [ style "padding" "1rem" ]
        [ div [ class "columns" ]
            [ div [ class "column is-8" ]
                [ if not setupInfo.isLoaded then
                    viewDataSourceCard setupInfo

                  else
                    text ""
                , viewCompletenessCard setupInfo
                , viewUnknownUnitsCard setupInfo
                , viewLocationFallbacksCard setupInfo
                , viewMissingSuppliersCard setupInfo
                , viewMappingCard
                ]
            , div [ class "column is-4" ]
                [ viewActionCard setupInfo
                , viewDependenciesCard setupInfo
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
                    floor ((setupInfo.completeness - toFloat whole) * 10)
            in
            String.fromInt whole ++ "," ++ String.fromInt decimal ++ "%"

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
    if List.isEmpty setupInfo.missingSuppliers then
        text ""

    else
        div [ class "card", style "margin-bottom" "1rem" ]
            [ div [ class "card-content" ]
                [ h3 [ class "title is-5" ]
                    [ span [ class "icon-text" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-exclamation-triangle" ] [] ]
                        , span [] [ text "Missing Suppliers" ]
                        ]
                    ]
                , div [ class "content" ]
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
                    p [ class "has-text-grey" ] [ text "No dependencies" ]

                  else
                    div []
                        [ p [ class "has-text-grey is-size-7", style "margin-bottom" "0.5rem" ]
                            [ text "Using data from:" ]
                        , div [ class "tags" ]
                            (List.map
                                (if setupInfo.isLoaded then
                                    viewSelectedDependency

                                 else
                                    viewSelectedDependencyEditable
                                )
                                setupInfo.selectedDependencies
                            )
                        ]
                , if setupInfo.isLoaded then
                    text ""

                  else if List.isEmpty setupInfo.suggestions && List.isEmpty setupInfo.selectedDependencies then
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
        [ text depName ]


viewSelectedDependencyEditable : String -> Html Msg
viewSelectedDependencyEditable depName =
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


viewActionCard : DatabaseSetupInfo -> Html Msg
viewActionCard setupInfo =
    div [ class "card", style "margin-bottom" "1rem" ]
        [ div [ class "card-content" ]
            [ if setupInfo.isLoaded then
                div []
                    [ p [ class "has-text-success" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-check" ] [] ]
                        , text " Database is loaded"
                        ]
                    , button
                        [ class "button is-primary is-fullwidth", style "margin-top" "1rem"
                        , onClick GoToActivities
                        ]
                        [ span [ class "icon" ] [ i [ class "fas fa-search" ] [] ]
                        , span [] [ text "Search Activities" ]
                        ]
                    ]

              else if setupInfo.isReady then
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
                        , text " No activities found."
                        ]
                    ]

              else
                div []
                    [ p [ class "has-text-warning" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-exclamation-triangle" ] [] ]
                        , text (" " ++ String.fromInt setupInfo.unresolvedLinks ++ " unresolved inputs")
                        ]
                    ]
            ]
        ]


viewMappingCard : Html Msg
viewMappingCard =
    div [ class "card", style "margin-bottom" "1rem" ]
        [ div [ class "card-content" ]
            [ h3 [ class "title is-5" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-flask" ] [] ]
                    , span [] [ text "Method Mapping" ]
                    ]
                ]
            , p [ class "has-text-grey", style "margin-bottom" "0.75rem" ]
                [ text "Check how well biosphere flows match characterization factors." ]
            , button
                [ class "button is-outlined is-fullwidth"
                , onClick GoToMapping
                ]
                [ span [ class "icon" ] [ i [ class "fas fa-microscope" ] [] ]
                , span [] [ text "Analyse mapping" ]
                ]
            ]
        ]

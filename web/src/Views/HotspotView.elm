module Views.HotspotView exposing (MethodPickerConfig, viewFlowHotspotResult, viewMethodPicker, viewProcessHotspotResult)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.LCIA exposing (FlowContributionEntry, FlowHotspotResult, MethodSummary, ProcessContribution, ProcessHotspotResult)
import Models.Method exposing (MethodCollectionStatus)
import Shared exposing (RemoteData(..))
import Utils.Format as Format


type alias MethodPickerConfig msg =
    { collections : RemoteData (List MethodCollectionStatus)
    , selectedCollection : Maybe String
    , methods : RemoteData (List MethodSummary)
    , selectedMethod : Maybe String
    , onSelectCollection : String -> msg
    , onSelectMethod : String -> msg
    }


viewMethodPicker : MethodPickerConfig msg -> Html msg
viewMethodPicker config =
    div [ style "padding" "0 1rem" ]
        [ viewCollectionDropdown config
        , viewMethodDropdown config
        ]


viewCollectionDropdown : MethodPickerConfig msg -> Html msg
viewCollectionDropdown config =
    case config.collections of
        Loading ->
            div [ class "has-text-centered", style "padding" "1rem" ]
                [ progress [ class "progress is-primary", attribute "max" "100" ] [] ]

        Failed err ->
            div [ class "notification is-danger" ]
                [ strong [] [ text "Error: " ], text err ]

        NotAsked ->
            text ""

        Loaded collections ->
            if List.isEmpty collections then
                div [ class "notification is-warning", style "margin" "0 0 0.5rem 0" ]
                    [ text "No method collections loaded. Go to Methods to load one." ]

            else
                div [ class "field has-addons", style "margin-bottom" "0.5rem" ]
                    [ div [ class "control" ]
                        [ span [ class "button is-static" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-flask" ] [] ]
                            , span [] [ text "Collection" ]
                            ]
                        ]
                    , div [ class "control is-expanded" ]
                        [ div [ class "select is-fullwidth" ]
                            [ select [ onInput config.onSelectCollection ]
                                (option [ value "" ] [ text "-- Select a method collection --" ]
                                    :: List.map
                                        (\c ->
                                            option
                                                [ value c.name
                                                , selected (config.selectedCollection == Just c.name)
                                                ]
                                                [ text (c.displayName ++ " (" ++ String.fromInt c.methodCount ++ " methods)") ]
                                        )
                                        collections
                                )
                            ]
                        ]
                    ]


viewMethodDropdown : MethodPickerConfig msg -> Html msg
viewMethodDropdown config =
    case config.selectedCollection of
        Nothing ->
            text ""

        Just _ ->
            case config.methods of
                NotAsked ->
                    text ""

                Loading ->
                    div [ class "has-text-centered", style "padding" "0.5rem" ]
                        [ progress [ class "progress is-primary is-small", attribute "max" "100" ] [] ]

                Failed _ ->
                    text ""

                Loaded methods ->
                    if List.isEmpty methods then
                        div [ class "notification is-warning is-light", style "margin" "0 0 0.5rem 0" ]
                            [ text "No methods in this collection." ]

                    else
                        let
                            sorted =
                                List.sortBy .msmCategory methods
                        in
                        div [ class "field has-addons", style "margin-bottom" "0.5rem" ]
                            [ div [ class "control" ]
                                [ span [ class "button is-static" ]
                                    [ span [ class "icon" ] [ i [ class "fas fa-chart-bar" ] [] ]
                                    , span [] [ text "Method" ]
                                    ]
                                ]
                            , div [ class "control is-expanded" ]
                                [ div [ class "select is-fullwidth" ]
                                    [ select [ onInput config.onSelectMethod ]
                                        (option [ value "" ] [ text "-- Select a method --" ]
                                            :: List.map
                                                (\m ->
                                                    option
                                                        [ value m.msmId
                                                        , selected (config.selectedMethod == Just m.msmId)
                                                        ]
                                                        [ text (m.msmCategory ++ " [" ++ m.msmUnit ++ "]") ]
                                                )
                                                sorted
                                        )
                                    ]
                                ]
                            ]


viewFlowHotspotResult : RemoteData FlowHotspotResult -> Html msg
viewFlowHotspotResult rd =
    case rd of
        NotAsked ->
            text ""

        Loading ->
            div [ class "has-text-centered", style "padding" "2rem" ]
                [ div [ class "is-size-5" ] [ text "Computing flow hotspots..." ]
                , progress [ class "progress is-primary", attribute "max" "100", style "margin-top" "1rem" ] []
                ]

        Failed err ->
            div [ class "notification is-danger", style "margin" "1rem" ]
                [ strong [] [ text "Error: " ], text err ]

        Loaded result ->
            if List.isEmpty result.fhrTopFlows then
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No flow contributions found." ]

            else
                let
                    maxContrib =
                        result.fhrTopFlows
                            |> List.map (\e -> abs e.fcoContribution)
                            |> List.maximum
                            |> Maybe.withDefault 1.0
                in
                div []
                    [ div [ class "table-container", style "padding" "0 1rem" ]
                        [ table [ class "table is-fullwidth is-hoverable" ]
                            [ thead []
                                [ tr []
                                    [ th [ style "width" "45%" ] [ text "Flow" ]
                                    , th [ style "width" "15%", style "text-align" "right" ] [ text "Contribution" ]
                                    , th [ style "width" "10%" ] [ text "Unit" ]
                                    , th [ style "width" "5%", style "text-align" "right" ] [ text "%" ]
                                    , th [ style "width" "25%" ] [ text "Relative" ]
                                    ]
                                ]
                            , tbody []
                                (List.map (viewFlowRow result.fhrUnit maxContrib) result.fhrTopFlows)
                            ]
                        ]
                    , viewTotalScore result.fhrTotalScore result.fhrUnit
                    ]


viewFlowRow : String -> Float -> FlowContributionEntry -> Html msg
viewFlowRow unit maxContrib entry =
    let
        pct =
            if maxContrib > 0 then
                abs entry.fcoContribution / maxContrib * 100

            else
                0
    in
    tr []
        [ td [] [ text entry.fcoFlowName ]
        , td [ style "text-align" "right", style "font-family" "monospace" ]
            [ text (Format.formatScientific entry.fcoContribution) ]
        , td [ class "has-text-grey" ] [ text unit ]
        , td [ style "text-align" "right" ] [ text (String.fromInt (round entry.fcoSharePct) ++ "%") ]
        , td [] [ viewRelativeBar pct ]
        ]


viewProcessHotspotResult : (String -> msg) -> RemoteData ProcessHotspotResult -> Html msg
viewProcessHotspotResult onNavigate rd =
    case rd of
        NotAsked ->
            text ""

        Loading ->
            div [ class "has-text-centered", style "padding" "2rem" ]
                [ div [ class "is-size-5" ] [ text "Computing process hotspots..." ]
                , progress [ class "progress is-primary", attribute "max" "100", style "margin-top" "1rem" ] []
                ]

        Failed err ->
            div [ class "notification is-danger", style "margin" "1rem" ]
                [ strong [] [ text "Error: " ], text err ]

        Loaded result ->
            if List.isEmpty result.phrProcesses then
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No process contributions found." ]

            else
                let
                    maxContrib =
                        result.phrProcesses
                            |> List.map (\p -> abs p.pcContribution)
                            |> List.maximum
                            |> Maybe.withDefault 1.0
                in
                div []
                    [ div [ class "table-container", style "padding" "0 1rem" ]
                        [ table [ class "table is-fullwidth is-hoverable" ]
                            [ thead []
                                [ tr []
                                    [ th [ style "width" "28%" ] [ text "Activity" ]
                                    , th [ style "width" "18%" ] [ text "Product" ]
                                    , th [ style "width" "7%" ] [ text "Location" ]
                                    , th [ style "width" "15%", style "text-align" "right" ] [ text "Contribution" ]
                                    , th [ style "width" "5%", style "text-align" "right" ] [ text "%" ]
                                    , th [ style "width" "22%" ] [ text "Relative" ]
                                    ]
                                ]
                            , tbody []
                                (List.map (viewProcessRow result.phrUnit maxContrib onNavigate) result.phrProcesses)
                            ]
                        ]
                    , viewTotalScore result.phrTotalScore result.phrUnit
                    ]


viewProcessRow : String -> Float -> (String -> msg) -> ProcessContribution -> Html msg
viewProcessRow unit maxContrib onNavigate proc =
    let
        pct =
            if maxContrib > 0 then
                abs proc.pcContribution / maxContrib * 100

            else
                0
    in
    tr []
        [ td []
            [ button
                [ class "button is-ghost is-small"
                , style "font-weight" "500"
                , style "text-align" "left"
                , style "white-space" "normal"
                , style "height" "auto"
                , style "padding" "0"
                , onClick (onNavigate proc.pcProcessId)
                ]
                [ text proc.pcActivityName ]
            ]
        , td [ class "has-text-grey" ] [ text proc.pcProductName ]
        , td [] [ text proc.pcLocation ]
        , td [ style "text-align" "right", style "font-family" "monospace" ]
            [ text (Format.formatScientific proc.pcContribution) ]
        , td [ style "text-align" "right" ] [ text (String.fromInt (round proc.pcSharePct) ++ "%") ]
        , td [] [ viewRelativeBar pct ]
        ]


viewTotalScore : Float -> String -> Html msg
viewTotalScore score unit =
    div
        [ style "padding" "0.75rem 1rem"
        , style "margin" "0 1rem"
        , style "background" "#f5f5f5"
        , style "border-radius" "4px"
        , style "display" "flex"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        ]
        [ span [ class "has-text-weight-bold" ] [ text "Total score" ]
        , span [ style "font-family" "monospace", style "font-size" "1.2rem" ]
            [ text (Format.formatScientific score ++ " " ++ unit) ]
        ]


viewRelativeBar : Float -> Html msg
viewRelativeBar pct =
    div
        [ style "background" "#f0f0f0"
        , style "border-radius" "3px"
        , style "height" "16px"
        , style "width" "100%"
        , style "overflow" "hidden"
        ]
        [ div
            [ style "background" "#3273dc"
            , style "height" "100%"
            , style "width" (String.fromFloat pct ++ "%")
            , style "border-radius" "3px"
            , style "transition" "width 0.3s ease"
            ]
            []
        ]

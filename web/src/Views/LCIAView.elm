module Views.LCIAView exposing (viewLCIAPage, viewPageNavbar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.LCIA exposing (LCIAResult)
import Models.Method exposing (MethodCollectionStatus)
import Shared exposing (RemoteData(..))
import Utils.Format as Format


type alias ViewConfig msg =
    { collections : RemoteData (List MethodCollectionStatus)
    , selectedCollection : Maybe String
    , results : RemoteData (List LCIAResult)
    , expandedRow : Maybe String
    , activityInfo : Maybe ( String, String )
    , onSelectCollection : String -> msg
    , onToggleRow : String -> msg
    }


viewLCIAPage : ViewConfig msg -> Html msg
viewLCIAPage config =
    div [ class "lcia-page" ]
        [ viewPageNavbar "Impact Assessment (LCIA)" config.activityInfo
        , viewCollectionPicker config
        , viewResults config
        ]


viewCollectionPicker : ViewConfig msg -> Html msg
viewCollectionPicker config =
    case config.collections of
        Loading ->
            div [ class "has-text-centered", style "padding" "2rem" ]
                [ progress [ class "progress is-primary", attribute "max" "100" ] [] ]

        Failed err ->
            div [ class "notification is-danger" ]
                [ strong [] [ text "Error: " ], text err ]

        NotAsked ->
            text ""

        Loaded collections ->
            if List.isEmpty collections then
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No method collections loaded. Go to Methods to load one." ]

            else
                div [ style "padding" "0 1rem" ]
                    [ div [ class "field has-addons", style "margin-bottom" "0.5rem" ]
                        [ div [ class "control" ]
                            [ span [ class "button is-static" ]
                                [ span [ class "icon" ] [ i [ class "fas fa-flask" ] [] ]
                                , span [] [ text "Method" ]
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
                                                    [ text (c.displayName ++ " (" ++ String.fromInt c.methodCount ++ " categories)") ]
                                            )
                                            collections
                                    )
                                ]
                            ]
                        ]
                    ]


viewResults : ViewConfig msg -> Html msg
viewResults config =
    case config.results of
        NotAsked ->
            text ""

        Loading ->
            div [ class "has-text-centered", style "padding" "2rem" ]
                [ div [ class "is-size-5" ] [ text "Computing impacts..." ]
                , progress [ class "progress is-primary", attribute "max" "100", style "margin-top" "1rem" ] []
                ]

        Failed err ->
            div [ class "notification is-danger", style "margin" "1rem" ]
                [ strong [] [ text "Error: " ], text err ]

        Loaded results ->
            if List.isEmpty results then
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No impact categories in this collection." ]

            else
                viewResultsTable config.onToggleRow config.expandedRow results


viewResultsTable : (String -> msg) -> Maybe String -> List LCIAResult -> Html msg
viewResultsTable onToggleRow expandedRow results =
    let
        maxScore =
            results
                |> List.map (\r -> abs r.lrScore)
                |> List.maximum
                |> Maybe.withDefault 1.0
    in
    div [ class "table-container", style "padding" "0 1rem" ]
        [ table [ class "table is-fullwidth is-hoverable" ]
            [ thead []
                [ tr []
                    [ th [ style "width" "30%" ] [ text "Category" ]
                    , th [ style "width" "15%", style "text-align" "right" ] [ text "Score" ]
                    , th [ style "width" "15%" ] [ text "Unit" ]
                    , th [ style "width" "15%", style "text-align" "center" ] [ text "Mapping" ]
                    , th [ style "width" "25%" ] [ text "Relative" ]
                    ]
                ]
            , tbody []
                (List.concatMap (viewResultRow onToggleRow expandedRow maxScore) results)
            ]
        ]


viewResultRow : (String -> msg) -> Maybe String -> Float -> LCIAResult -> List (Html msg)
viewResultRow onToggleRow expandedRow maxScore result =
    let
        isExpanded =
            expandedRow == Just result.lrMethodId

        pct =
            if maxScore > 0 then
                abs result.lrScore / maxScore * 100

            else
                0

        total =
            result.lrMappedFlows + result.lrUnmappedFlows

        mappingPct =
            if total > 0 then
                toFloat result.lrMappedFlows / toFloat total * 100

            else
                0
    in
    [ tr
        ([ style "cursor"
            (if result.lrUnmappedFlows > 0 then
                "pointer"

             else
                "default"
            )
         ]
            ++ (if result.lrUnmappedFlows > 0 then
                    [ onClick (onToggleRow result.lrMethodId) ]

                else
                    []
               )
        )
        [ td [ style "font-weight" "500" ] [ text result.lrMethodName ]
        , td [ style "text-align" "right", style "font-family" "monospace" ]
            [ text (Format.formatScientific result.lrScore) ]
        , td [ class "has-text-grey" ] [ text result.lrUnit ]
        , td [ style "text-align" "center" ]
            [ viewMappingBadge result.lrMappedFlows result.lrUnmappedFlows mappingPct ]
        , td [] [ viewRelativeBar pct ]
        ]
    ]
        ++ (if isExpanded && not (List.isEmpty result.lrUnmappedNames) then
                [ viewExpandedRow result ]

            else
                []
           )


viewMappingBadge : Int -> Int -> Float -> Html msg
viewMappingBadge mapped unmapped pct =
    span []
        [ span [ class "tag is-success is-light", style "font-size" "0.75rem" ]
            [ text (String.fromInt mapped) ]
        , if unmapped > 0 then
            span
                [ class "tag is-warning is-light"
                , style "font-size" "0.75rem"
                , style "margin-left" "0.25rem"
                , title ("Mapping coverage: " ++ String.fromInt (round pct) ++ "%")
                ]
                [ text (String.fromInt unmapped ++ " unmapped") ]

          else
            text ""
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


viewExpandedRow : LCIAResult -> Html msg
viewExpandedRow result =
    tr []
        [ td [ colspan 5, style "padding" "0.5rem 1rem", style "background" "#fafafa" ]
            [ div [ style "max-height" "200px", style "overflow-y" "auto" ]
                [ p [ class "has-text-weight-semibold is-size-7", style "margin-bottom" "0.5rem" ]
                    [ text ("Unmapped flows (" ++ String.fromInt result.lrUnmappedFlows ++ " total, showing first " ++ String.fromInt (List.length result.lrUnmappedNames) ++ "):") ]
                , div [ class "tags" ]
                    (List.map
                        (\name ->
                            span [ class "tag is-light", style "font-size" "0.7rem" ] [ text name ]
                        )
                        result.lrUnmappedNames
                    )
                ]
            ]
        ]


{-| Shared navbar component for all pages
-}
viewPageNavbar : String -> Maybe ( String, String ) -> Html msg
viewPageNavbar pageTitle maybeActivity =
    nav [ class "navbar is-light" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ h1 [ class "title is-4" ] [ text pageTitle ]
                ]
            ]
        , div [ class "navbar-menu is-active" ]
            [ div [ class "navbar-end" ]
                (case maybeActivity of
                    Just ( name, location ) ->
                        [ div [ class "navbar-item" ]
                            [ span [ class "title is-4" ] [ text name ]
                            ]
                        , div [ class "navbar-item" ]
                            [ span [ class "subtitle is-6" ] [ text location ]
                            ]
                        ]

                    Nothing ->
                        [ div [ class "navbar-item" ]
                            [ span [ class "subtitle is-6" ] [ text "Loading..." ]
                            ]
                        ]
                )
            ]
        ]

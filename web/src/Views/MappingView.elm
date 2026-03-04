module Views.MappingView exposing (ViewConfig, viewMappingOverview)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.LCIA exposing (MappingStatus, MethodSummary)


type alias ViewConfig msg =
    { methods : List MethodSummary
    , mappings : Dict String MappingStatus
    , loading : Bool
    , onClickMethod : String -> msg
    }


viewMappingOverview : ViewConfig msg -> Html msg
viewMappingOverview config =
    let
        loadedMappings =
            Dict.values config.mappings
    in
    div []
        [ viewSummaryBar loadedMappings config.loading
        , viewMethodTable config
        ]


viewSummaryBar : List MappingStatus -> Bool -> Html msg
viewSummaryBar mappings loading =
    let
        totalCFs =
            List.sum (List.map .mstTotalFactors mappings)

        matchedCFs =
            totalCFs - List.sum (List.map .mstUnmapped mappings)

        -- Use first mapping's dbBiosphereCount (same for all methods in same DB)
        dbBioCount =
            List.head mappings
                |> Maybe.map .mstDbBiosphereCount
                |> Maybe.withDefault 0

        dbFlowsCovered =
            List.sum (List.map .mstUniqueDbFlowsMatched mappings)

        pctCFs =
            if totalCFs > 0 then
                toFloat matchedCFs / toFloat totalCFs * 100 |> round

            else
                0

        pctDb =
            if dbBioCount > 0 then
                toFloat dbFlowsCovered / toFloat dbBioCount * 100 |> round

            else
                0
    in
    if List.isEmpty mappings then
        if loading then
            div [ class "notification is-info is-light", style "margin" "0.5rem 0" ]
                [ text "Loading mapping data..." ]

        else
            text ""

    else
        div [ style "display" "flex", style "gap" "1.5rem", style "margin" "0.5rem 0", style "flex-wrap" "wrap" ]
            [ div [ class "notification is-light", style "padding" "0.5rem 1rem", style "margin" "0", style "flex" "1" ]
                [ p [ class "heading", style "margin-bottom" "0.25rem" ] [ text "DB Biosphere" ]
                , p [ class "is-size-5" ]
                    [ strong [] [ text (String.fromInt dbFlowsCovered) ]
                    , text (" / " ++ String.fromInt dbBioCount ++ " flows (" ++ String.fromInt pctDb ++ "%)")
                    ]
                ]
            , div [ class "notification is-light", style "padding" "0.5rem 1rem", style "margin" "0", style "flex" "1" ]
                [ p [ class "heading", style "margin-bottom" "0.25rem" ] [ text "Method CFs" ]
                , p [ class "is-size-5" ]
                    [ strong [] [ text (String.fromInt matchedCFs) ]
                    , text (" / " ++ String.fromInt totalCFs ++ " matched (" ++ String.fromInt pctCFs ++ "%)")
                    ]
                ]
            ]


viewMethodTable : ViewConfig msg -> Html msg
viewMethodTable config =
    table [ class "table is-fullwidth is-hoverable" ]
        [ thead []
            [ tr []
                [ th [ style "width" "30%" ] [ text "Method Name" ]
                , th [ style "width" "15%", style "text-align" "right" ] [ text "CFs Matched" ]
                , th [ style "width" "10%", style "text-align" "right" ] [ text "DB Flows" ]
                , th [ style "width" "30%" ] [ text "Strategy" ]
                ]
            ]
        , tbody []
            (List.map (viewMethodRow config) config.methods)
        ]


viewMethodRow : ViewConfig msg -> MethodSummary -> Html msg
viewMethodRow config method =
    case Dict.get method.msmId config.mappings of
        Just mapping ->
            tr [ style "cursor" "pointer", onClick (config.onClickMethod method.msmId) ]
                [ td [ style "font-weight" "500" ] [ text method.msmName ]
                , td [ style "text-align" "right", style "font-family" "monospace" ]
                    [ text (String.fromInt (mapping.mstTotalFactors - mapping.mstUnmapped) ++ " / " ++ String.fromInt mapping.mstTotalFactors) ]
                , td [ style "text-align" "right", style "font-family" "monospace" ]
                    [ text (String.fromInt mapping.mstUniqueDbFlowsMatched) ]
                , td [] [ viewStrategyBadges mapping ]
                ]

        Nothing ->
            tr []
                [ td [ style "font-weight" "500" ] [ text method.msmName ]
                , td [ style "text-align" "center", colspan 3 ]
                    [ if config.loading then
                        span [ class "icon has-text-grey-light" ]
                            [ i [ class "fas fa-spinner fa-spin" ] [] ]

                      else
                        span [ class "has-text-grey-light" ] [ text "—" ]
                    ]
                ]


viewStrategyBadges : MappingStatus -> Html msg
viewStrategyBadges mapping =
    span [ style "display" "flex", style "gap" "0.25rem", style "flex-wrap" "wrap" ]
        (List.filterMap identity
            [ if mapping.mstMappedByUUID > 0 then
                Just (span [ class "tag is-info is-light", style "font-size" "0.75rem" ] [ text (String.fromInt mapping.mstMappedByUUID ++ " uuid") ])

              else
                Nothing
            , if mapping.mstMappedByName > 0 then
                Just (span [ class "tag is-success is-light", style "font-size" "0.75rem" ] [ text (String.fromInt mapping.mstMappedByName ++ " name") ])

              else
                Nothing
            , if mapping.mstMappedBySynonym > 0 then
                Just (span [ class "tag is-warning is-light", style "font-size" "0.75rem" ] [ text (String.fromInt mapping.mstMappedBySynonym ++ " synonym") ])

              else
                Nothing
            , if mapping.mstUnmapped > 0 then
                Just (span [ class "tag is-danger is-light", style "font-size" "0.75rem" ] [ text (String.fromInt mapping.mstUnmapped ++ " unmapped") ])

              else
                Nothing
            ]
        )

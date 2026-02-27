module Views.InventoryView exposing (Msg(..), viewInventoryTables)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Inventory exposing (InventoryFlowDetail)
import Utils.Format as Format


type Msg
    = UpdateResourcesSearch String
    | UpdateEmissionsSearch String


viewInventoryTables : String -> String -> Int -> Int -> List InventoryFlowDetail -> Html Msg
viewInventoryTables resourcesSearch emissionsSearch resourcesCount emissionsCount flows =
    let
        resources =
            flows |> List.filter (not << .ifdIsEmission)

        emissions =
            flows |> List.filter .ifdIsEmission

        filteredResources =
            filterFlows resourcesSearch resources

        filteredEmissions =
            filterFlows emissionsSearch emissions
    in
    div [ class "columns" ]
        [ div [ class "column" ]
            [ viewTableWithTitle ("Natural Resources (" ++ String.fromInt resourcesCount ++ ")") "fas fa-leaf" "has-text-success" resourcesSearch UpdateResourcesSearch filteredResources False
            ]
        , div [ class "column" ]
            [ viewTableWithTitle ("Emissions (" ++ String.fromInt emissionsCount ++ ")") "fas fa-cloud" "has-text-warning" emissionsSearch UpdateEmissionsSearch filteredEmissions True
            ]
        ]


viewTableWithTitle : String -> String -> String -> String -> (String -> Msg) -> List InventoryFlowDetail -> Bool -> Html Msg
viewTableWithTitle title iconClass iconColor searchQuery onSearch flows _ =
    div []
        [ h3 [ class "title is-5", style "margin-bottom" "0.5rem" ]
            [ span [ class ("icon " ++ iconColor), style "margin-right" "0.5rem" ]
                [ i [ class iconClass ] []
                ]
            , text title
            ]
        , table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ thead []
                [ tr []
                    [ th [ class "has-text-right" ] [ text "Amount" ]
                    , th [] [ text "Unit" ]
                    , th [] [ text "Flow" ]
                    , th [] [ text "Compartment" ]
                    ]
                , tr []
                    [ th [] []
                    , th [] []
                    , th [ style "padding" "0.25rem 0.5rem" ]
                        [ div [ class "control has-icons-left" ]
                            [ input
                                [ class "input is-small"
                                , type_ "text"
                                , placeholder "Search..."
                                , value searchQuery
                                , onInput onSearch
                                ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-search" ] []
                                ]
                            ]
                        ]
                    , th [] []
                    ]
                ]
            , tbody []
                (List.map viewInventoryRow flows)
            ]
        ]


viewInventoryRow : InventoryFlowDetail -> Html msg
viewInventoryRow flowDetail =
    tr []
        [ td [ class "has-text-right" ] [ text (Format.formatScientific flowDetail.ifdQuantity) ]
        , td [] [ text flowDetail.ifdUnitName ]
        , td [] [ text flowDetail.ifdFlow.flowName ]
        , td [] [ text flowDetail.ifdFlow.flowCategory ]
        ]


filterFlows : String -> List InventoryFlowDetail -> List InventoryFlowDetail
filterFlows searchQuery flows =
    if String.isEmpty (String.trim searchQuery) then
        flows

    else
        let
            searchWords =
                searchQuery
                    |> String.toLower
                    |> String.words
                    |> List.filter (\w -> not (String.isEmpty w))

            matchesAllWords flow =
                let
                    searchableText =
                        String.toLower
                            (flow.ifdFlow.flowName
                                ++ " "
                                ++ flow.ifdFlow.flowCategory
                                ++ " "
                                ++ flow.ifdUnitName
                            )
                in
                List.all (\word -> String.contains word searchableText) searchWords
        in
        List.filter matchesAllWords flows

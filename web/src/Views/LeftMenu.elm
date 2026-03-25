module Views.LeftMenu exposing (Msg(..), mapMsg, viewLeftMenu)

import Char
import Html exposing (Html, a, button, div, i, img, nav, node, p, span, text)
import Html.Attributes exposing (attribute, class, classList, href, src, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Route exposing (ActivePage(..), ActivityTab(..))


type Msg
    = NavigateTo ActivePage
    | ToggleConsole
    | CloseConsole


mapMsg : { onNavigate : ActivePage -> msg, onToggleConsole : msg, onCloseConsole : msg } -> Msg -> msg
mapMsg handlers msg =
    case msg of
        NavigateTo page ->
            handlers.onNavigate page

        ToggleConsole ->
            handlers.onToggleConsole

        CloseConsole ->
            handlers.onCloseConsole


viewLeftMenu : ActivePage -> String -> Maybe String -> Maybe String -> String -> String -> Bool -> Bool -> Bool -> Html Msg
viewLeftMenu currentPage currentActivityId currentDatabaseName currentActivityName version gitHash showConsole menuOpen isHosted =
    nav [ classList [ ( "left-menu", True ), ( "is-active", menuOpen ) ], style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
        [ -- Top section (scrollable content)
          div [ style "flex" "1", style "overflow-y" "auto" ]
            [ div
                [ class "database-header"
                , style "padding" "1.25rem 1rem"
                , style "font-size" "1.4rem"
                , style "font-weight" "bold"
                , style "text-align" "center"
                , style "cursor" "pointer"
                , onClick (NavigateTo HomeActive)
                ]
                [ case currentDatabaseName of
                    Just dbName ->
                        span [ style "overflow" "hidden", style "text-overflow" "ellipsis", style "white-space" "nowrap", style "display" "block" ] [ text dbName ]

                    Nothing ->
                        img [ src "/static/volca.svg", style "height" "2rem" ] []
                ]
            , -- DATABASES section
              div [ class "menu-items" ]
                [ menuLabel "Manage"
                , menuItem currentPage DatabasesActive "fas fa-database" "Databases" False
                , menuItem currentPage MethodsActive "fas fa-flask" "Methods" False
                , menuItem currentPage FlowSynonymsActive "fas fa-exchange-alt" "Synonyms" False
                , menuItem currentPage CompartmentMappingsActive "fas fa-layer-group" "Compartments" False
                , menuItem currentPage UnitsActive "fas fa-ruler" "Units" False
                ]
            , -- SEARCH section
              div [ class "menu-items" ]
                [ menuLabel "Search"
                , menuItem currentPage ActivitiesActive "fas fa-search" "Activities" False
                , menuItem currentPage FlowSearchActive "fas fa-atom" "Flows" False
                ]
            , -- Activity section (only show if an activity is selected)
              -- White background to look like vertical tabs connected to main content
              case currentActivityName of
                Just _ ->
                    div
                        [ class "menu-items explore-section"
                        , style "background-color" "white"
                        , style "margin" "0.5rem 0 0.5rem 0.5rem"
                        , style "padding" "0.5rem 0"
                        , style "border-radius" "6px 0 0 6px"
                        ]
                        [ menuLabel "Explore"
                        , menuItem currentPage (ActivityActive Upstream) "fas fa-arrow-up" "Upstream activities" False
                        , menuItem currentPage (ActivityActive Emissions) "fas fa-cloud" "Direct emissions" False
                        , menuItem currentPage (ActivityActive Resources) "fas fa-leaf" "Natural resources" False
                        , menuItem currentPage (ActivityActive Products) "fas fa-box" "Outgoing products" False
                        , menuItem currentPage (ActivityActive Inventory) "fas fa-list-ul" "Inventory" False
                        , menuItem currentPage (ActivityActive SupplyChainTable) "fas fa-table" "Supply Chain" False
                        , node "details"
                            (class "lab-section"
                                :: (if isLabTab currentPage then
                                        [ attribute "open" "" ]

                                    else
                                        []
                                   )
                            )
                            [ node "summary"
                                [ class "menu-label has-text-grey-light"
                                , style "padding" "0.5rem 1rem"
                                , style "margin-top" "0.5rem"
                                , style "font-size" "0.75rem"
                                , style "text-transform" "uppercase"
                                , style "cursor" "pointer"
                                ]
                                [ text "Lab" ]
                            , menuItem currentPage (ActivityActive LCIA) "fas fa-chart-bar" "Impacts" True
                            , menuItem currentPage (ActivityActive Tree) "fas fa-project-diagram" "Tree" True
                            , menuItem currentPage (ActivityActive Graph) "fas fa-network-wired" "Graph" True
                            , menuItem currentPage (ActivityActive SupplyChainGraph) "fas fa-sitemap" "Supply Chain Graph" True
                            , menuItem currentPage (ActivityActive SupplyChainGraphDagre) "fas fa-project-diagram" "SC Dagre" True
                            , menuItem currentPage (ActivityActive Variant) "fas fa-exchange-alt" "Variant" True
                            , menuItem currentPage (ActivityActive Consumers) "fas fa-arrow-down" "Consumers" True
                            , menuItem currentPage CompositionActive "fas fa-cubes" "Composition" True
                            ]
                        ]

                Nothing ->
                    text ""
            ]
        , -- Footer (fixed)
          div
            [ class "menu-footer"
            , style "flex-shrink" "0"
            , style "padding" "0.5rem 1rem"
            , style "color" "#888"
            , style "font-size" "0.8rem"
            , style "text-align" "center"
            , style "border-top" "1px solid #ddd"
            ]
            [ button
                [ style "background" "none"
                , style "border" "none"
                , style "cursor" "pointer"
                , style "color" (if showConsole then "#3273dc" else "#888")
                , style "font-size" "0.8rem"
                , style "font-family" "inherit"
                , style "padding" "0.25rem 0"
                , style "margin-bottom" "0.5rem"
                , style "display" "inline-flex"
                , style "align-items" "center"
                , style "gap" "0.4rem"
                , stopPropagationOn "click" (Json.Decode.succeed ( ToggleConsole, True ))
                ]
                [ i [ class "fas fa-terminal", style "font-size" "0.7rem" ] []
                , span [] [ text "Console output" ]
                ]
            , if isHosted then
                Html.a
                    [ Html.Attributes.href "/account"
                    , style "display" "inline-flex"
                    , style "align-items" "center"
                    , style "gap" "0.4rem"
                    , style "color" "#888"
                    , style "font-size" "0.8rem"
                    , style "text-decoration" "none"
                    , style "padding" "0.25rem 0"
                    , style "margin-bottom" "0.5rem"
                    ]
                    [ i [ class "fas fa-cog", style "font-size" "0.7rem" ] []
                    , span [] [ text "Manage instance" ]
                    ]
              else
                text ""
            , div [ style "border-top" "1px solid #ddd", style "margin" "0.5rem 0" ] []
            , div []
                [ img [ src "/static/volca.svg", style "height" "1.2rem" ] [] ]
            , div
                [ style "font-size" "0.8em"
                , style "margin-top" "0.25rem"
                , Html.Attributes.title (if String.isEmpty gitHash then "" else gitHash)
                ]
                [ text (formatVersion version) ]
            ]
        ]


formatVersion : String -> String
formatVersion version =
    -- Check if version looks like a semantic version (starts with digit or 'v' followed by digit)
    case String.uncons version of
        Just ( first, _ ) ->
            if Char.isDigit first then
                "version " ++ version

            else if first == 'v' then
                -- Remove 'v' prefix for display
                "version " ++ String.dropLeft 1 version

            else
                -- Commit hash - just show as-is
                version

        Nothing ->
            version


isLabTab : ActivePage -> Bool
isLabTab page =
    case page of
        ActivityActive tab ->
            List.member tab [ LCIA, Tree, Graph, SupplyChainGraph, SupplyChainGraphDagre, Variant, Consumers ]

        CompositionActive ->
            True

        _ ->
            False


menuLabel : String -> Html Msg
menuLabel label =
    p [ class "menu-label has-text-grey-light", style "padding" "0.5rem 1rem", style "margin-top" "0.5rem", style "font-size" "0.75rem", style "text-transform" "uppercase" ]
        [ text label ]


menuItem : ActivePage -> ActivePage -> String -> String -> Bool -> Html Msg
menuItem currentPage targetPage iconClass label isLab =
    button
        [ classList
            [ ( "menu-item", True )
            , ( "is-active", currentPage == targetPage )
            , ( "is-lab", isLab )
            ]
        , onClick (NavigateTo targetPage)
        ]
        [ span [ class "icon" ]
            [ i [ class iconClass ] []
            ]
        , span [ class "menu-label" ] [ text label ]
        ]

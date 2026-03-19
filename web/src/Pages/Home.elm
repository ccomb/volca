module Pages.Home exposing (Model, Msg, page)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)
import Route exposing (Route(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    ()


page : Shared.Model -> Spa.Page.Page () Shared.Msg (View Msg) () ()
page shared =
    Spa.Page.static (view shared)


view : Shared.Model -> View Msg
view shared =
    { title = "Home"
    , body =
        div [ style "padding" "2rem", style "max-width" "1200px", style "margin" "0 auto" ]
            [ viewBranding
            , div [ class "columns is-multiline" ]
                [ tile viewQuickStart
                , tile viewActivityViews
                , tile viewLabViews
                , tile (viewDatabases shared.databases)
                ]
            ]
    }


tileStyle : Attribute msg
tileStyle =
    style "background-color" "#f5f5f5"


tile : Html msg -> Html msg
tile content =
    div [ class "column is-3-desktop is-6-tablet" ]
        [ div [ tileStyle, style "border-radius" "8px", style "padding" "1.25rem", style "height" "100%" ]
            [ content ]
        ]


viewBranding : Html msg
viewBranding =
    div [ style "text-align" "center", style "margin-bottom" "1.5rem", style "padding-top" "0.5rem" ]
        [ div [ style "margin-bottom" "0.5rem" ]
            [ img [ src "/static/volca.svg", style "height" "3rem" ] [] ]
        , p [ class "subtitle is-5 has-text-grey" ] [ text "Life Cycle Assessment Engine" ]
        ]



-- Databases tile


viewDatabases : RemoteData DatabaseList -> Html msg
viewDatabases databases =
    div []
        [ tileTitle "fas fa-database" "Databases"
        , case databases of
            Loading ->
                p [ class "has-text-grey" ] [ text "Loading..." ]

            Failed err ->
                p [ class "has-text-danger" ] [ text err ]

            NotAsked ->
                text ""

            Loaded dbList ->
                if List.isEmpty dbList.databases then
                    div []
                        [ p [ class "has-text-grey", style "margin-bottom" "0.5rem" ] [ text "No databases configured." ]
                        , a [ href "/databases/upload", class "is-size-7" ] [ text "Upload a database" ]
                        ]

                else
                    div []
                        (List.map viewDbLink dbList.databases
                            ++ [ div [ style "margin-top" "0.5rem" ]
                                    [ a [ href "/databases", class "is-size-7 has-text-grey" ] [ text "Manage databases" ] ]
                               ]
                        )
        ]


viewDbLink : DatabaseStatus -> Html msg
viewDbLink db =
    let
        ( dotClass, content ) =
            case db.status of
                DbLoaded ->
                    ( "has-text-success"
                    , a
                        [ href (Route.routeToUrl (ActivitiesRoute { db = db.name, name = Nothing, product = Nothing, limit = Just 20 }))
                        , style "font-weight" "500"
                        ]
                        [ text db.displayName ]
                    )

                PartiallyLinked ->
                    ( "has-text-warning"
                    , span [ class "has-text-grey" ] [ text db.displayName ]
                    )

                Unloaded ->
                    ( "has-text-grey-lighter"
                    , span [ class "has-text-grey" ] [ text db.displayName ]
                    )
    in
    div [ style "margin-bottom" "0.3rem", style "display" "flex", style "align-items" "center", style "gap" "0.4rem" ]
        [ span [ style "font-size" "0.7rem", class dotClass ] [ text "\u{25CF}" ]
        , content
        ]



-- Quick start tile


viewQuickStart : Html msg
viewQuickStart =
    div [ class "content", style "margin-bottom" "0" ]
        [ tileTitle "fas fa-rocket" "Quick start"
        , ol [ style "margin-top" "0.5rem", style "margin-left" "1.25rem" ]
            [ li []
                [ strong [] [ text "Load a database" ]
                , text " \u{2014} open "
                , strong [] [ text "Databases" ]
                , text " and click "
                , strong [] [ text "Open" ]
                , text "."
                ]
            , li []
                [ strong [] [ text "Search" ]
                , text " \u{2014} go to "
                , strong [] [ text "Activities" ]
                , text ", type a name (e.g. \u{201C}electricity\u{201D})."
                ]
            , li []
                [ strong [] [ text "Explore" ]
                , text " \u{2014} click a result; the left menu expands with detailed views."
                ]
            ]
        ]



-- Activity views tile


viewActivityViews : Html msg
viewActivityViews =
    div []
        [ tileTitle "fas fa-search" "Activity views"
        , viewFeatureList
            [ ( "fas fa-arrow-up", "Upstream", "Direct inputs this activity consumes. Click to navigate." )
            , ( "fas fa-cloud", "Emissions", "Substances emitted to air, water or soil by this activity." )
            , ( "fas fa-leaf", "Resources", "Resources taken from nature (land, water, minerals)." )
            , ( "fas fa-box", "Products", "Products and co-products that this activity produces." )
            , ( "fas fa-list-ul", "Inventory", "Full life-cycle inventory across the entire supply chain." )
            ]
        ]



-- Lab views tile


viewLabViews : Html msg
viewLabViews =
    div []
        [ tileTitle "fas fa-flask" "Lab"
        , p [ class "has-text-grey", style "margin-bottom" "0.5rem", style "font-size" "0.85rem" ]
            [ text "Experimental tools for deeper analysis:" ]
        , viewFeatureList
            [ ( "fas fa-chart-bar", "Impacts", "LCIA results using a characterization method." )
            , ( "fas fa-project-diagram", "Tree", "Recursive supply-chain dependency hierarchy." )
            , ( "fas fa-network-wired", "Graph", "Force-directed graph of the supply chain." )
            ]
        ]



-- Shared helpers


tileTitle : String -> String -> Html msg
tileTitle iconClass title =
    h3 [ class "title is-5", style "margin-bottom" "0.75rem", style "display" "flex", style "align-items" "center", style "gap" "0.5rem" ]
        [ span [ class "icon" ] [ i [ class iconClass ] [] ]
        , text title
        ]


viewFeatureList : List ( String, String, String ) -> Html msg
viewFeatureList =
    div [] << List.map viewFeatureItem


viewFeatureItem : ( String, String, String ) -> Html msg
viewFeatureItem ( iconClass, name, description ) =
    div [ style "display" "flex", style "align-items" "flex-start", style "margin-bottom" "0.4rem", style "font-size" "0.9rem" ]
        [ span [ class "icon is-small has-text-grey", style "margin-right" "0.4rem", style "margin-top" "0.1rem" ]
            [ i [ class iconClass, style "font-size" "0.75rem" ] [] ]
        , div []
            [ strong [] [ text name ]
            , text (" \u{2014} " ++ description)
            ]
        ]

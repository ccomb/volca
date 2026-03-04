module Pages.FlowMapping exposing (Model, Msg, page)

import Api
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.LCIA exposing (FlowCFEntry, FlowCFMapping)
import Route exposing (FlowMappingFlags)
import Shared exposing (RemoteData(..))
import Spa.Page
import Utils.Format as Format
import View exposing (View)


type Filter
    = All
    | Matched
    | Unmatched


type alias Model =
    { methodId : String
    , dbName : Maybe String
    , mapping : RemoteData FlowCFMapping
    , filter : Filter
    , searchQuery : String
    }


type Msg
    = MappingLoaded (Result Http.Error FlowCFMapping)
    | SetFilter Filter
    | SetSearch String
    | GoBack


page : Shared.Model -> Spa.Page.Page FlowMappingFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> FlowMappingFlags -> ( Model, Effect Shared.Msg Msg )
init _ flags =
    ( { methodId = flags.methodId
      , dbName = flags.db
      , mapping = Loading
      , filter = All
      , searchQuery = ""
      }
    , case flags.db of
        Just dbName ->
            Effect.fromCmd (Api.loadFlowMapping MappingLoaded dbName flags.methodId)

        Nothing ->
            Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        MappingLoaded (Ok mapping) ->
            ( { model | mapping = Loaded mapping }, Effect.none )

        MappingLoaded (Err err) ->
            ( { model | mapping = Failed (Shared.httpErrorToString err) }, Effect.none )

        SetFilter f ->
            ( { model | filter = f }, Effect.none )

        SetSearch q ->
            ( { model | searchQuery = q }, Effect.none )

        GoBack ->
            ( model
            , Effect.fromCmd (Nav.back shared.key 1)
            )


view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Flow Mapping"
    , body =
        div [ class "databases-page" ]
            [ case model.mapping of
                Loading ->
                    div [ class "has-text-centered", style "padding" "3rem" ]
                        [ div [ class "is-size-5" ] [ text "Loading flow mapping..." ]
                        , progress [ class "progress is-primary", attribute "max" "100", style "margin-top" "1rem" ] []
                        ]

                Failed err ->
                    div [ class "notification is-danger", style "margin" "1rem" ]
                        [ strong [] [ text "Error: " ], text err ]

                Loaded mapping ->
                    viewLoaded model mapping

                NotAsked ->
                    div [ class "notification is-warning", style "margin" "1rem" ]
                        [ text "No database selected." ]
            ]
    }


viewLoaded : Model -> FlowCFMapping -> Html Msg
viewLoaded model mapping =
    let
        filtered =
            filterFlows model.filter model.searchQuery mapping.fcmFlows

        pct =
            if mapping.fcmTotalFlows > 0 then
                toFloat mapping.fcmMatchedFlows / toFloat mapping.fcmTotalFlows * 100 |> round

            else
                0
    in
    div []
        [ -- Header
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
                            [ text mapping.fcmMethodName ]
                        ]
                    ]
                ]
            , p [ class "is-size-5", style "margin-top" "0.5rem" ]
                [ strong [] [ text (String.fromInt mapping.fcmMatchedFlows) ]
                , text (" / " ++ String.fromInt mapping.fcmTotalFlows ++ " biosphere flows have a CF (" ++ String.fromInt pct ++ "%)")
                ]
            ]
        , -- Filter controls
          div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "center", style "margin-bottom" "0.75rem", style "flex-wrap" "wrap" ]
            [ viewFilterButton "All" All model.filter (List.length mapping.fcmFlows)
            , viewFilterButton "Matched" Matched model.filter mapping.fcmMatchedFlows
            , viewFilterButton "Unmatched" Unmatched model.filter (mapping.fcmTotalFlows - mapping.fcmMatchedFlows)
            , div [ class "control", style "margin-left" "auto" ]
                [ input
                    [ class "input is-small"
                    , type_ "text"
                    , placeholder "Search flows..."
                    , value model.searchQuery
                    , onInput SetSearch
                    , style "width" "250px"
                    ]
                    []
                ]
            ]
        , -- Table
          div [ class "table-container" ]
            [ table [ class "table is-fullwidth is-hoverable is-narrow" ]
                [ thead []
                    [ tr []
                        [ th [ style "width" "25%" ] [ text "DB Flow" ]
                        , th [ style "width" "15%" ] [ text "Category" ]
                        , th [ style "width" "20%" ] [ text "CF Flow Name" ]
                        , th [ style "width" "12%", style "text-align" "right" ] [ text "CF Value" ]
                        , th [ style "width" "8%" ] [ text "Unit" ]
                        , th [ style "width" "8%" ] [ text "Match" ]
                        ]
                    ]
                , tbody []
                    (List.map (viewFlowRow mapping.fcmMethodUnit) filtered)
                ]
            , if List.isEmpty filtered then
                div [ class "has-text-centered has-text-grey", style "padding" "2rem" ]
                    [ text "No flows match the current filter." ]

              else
                div [ class "has-text-grey-light", style "padding" "0.5rem", style "font-size" "0.85rem" ]
                    [ text ("Showing " ++ String.fromInt (List.length filtered) ++ " flows") ]
            ]
        ]


viewFilterButton : String -> Filter -> Filter -> Int -> Html Msg
viewFilterButton label filter currentFilter count =
    button
        [ class
            (if filter == currentFilter then
                "button is-small is-primary"

             else
                "button is-small"
            )
        , onClick (SetFilter filter)
        ]
        [ text (label ++ " (" ++ String.fromInt count ++ ")") ]


viewFlowRow : String -> FlowCFEntry -> Html Msg
viewFlowRow unit entry =
    tr
        [ style "color"
            (if entry.fceCfValue == Nothing then
                "#999"

             else
                "inherit"
            )
        ]
        [ td [ style "font-weight" "500" ] [ text entry.fceFlowName ]
        , td [ class "has-text-grey", style "font-size" "0.85rem" ] [ text entry.fceFlowCategory ]
        , td [ style "font-size" "0.85rem" ]
            [ case entry.fceCfFlowName of
                Just name ->
                    text name

                Nothing ->
                    span [ class "has-text-grey-light" ] [ text "—" ]
            ]
        , td [ style "text-align" "right", style "font-family" "monospace" ]
            [ case entry.fceCfValue of
                Just v ->
                    text (Format.formatScientific v)

                Nothing ->
                    span [ class "has-text-grey-light" ] [ text "—" ]
            ]
        , td [ class "has-text-grey", style "font-size" "0.85rem" ]
            [ case entry.fceCfValue of
                Just _ ->
                    text unit

                Nothing ->
                    text ""
            ]
        , td []
            [ case entry.fceMatchStrategy of
                Just strategy ->
                    viewStrategyTag strategy

                Nothing ->
                    text ""
            ]
        ]


viewStrategyTag : String -> Html msg
viewStrategyTag strategy =
    let
        tagClass =
            case strategy of
                "uuid" ->
                    "tag is-info is-light"

                "name" ->
                    "tag is-success is-light"

                "synonym" ->
                    "tag is-warning is-light"

                _ ->
                    "tag is-light"
    in
    span [ class tagClass, style "font-size" "0.75rem" ] [ text strategy ]


filterFlows : Filter -> String -> List FlowCFEntry -> List FlowCFEntry
filterFlows filter query flows =
    let
        byFilter =
            case filter of
                All ->
                    flows

                Matched ->
                    List.filter (\e -> e.fceCfValue /= Nothing) flows

                Unmatched ->
                    List.filter (\e -> e.fceCfValue == Nothing) flows

        lowerQuery =
            String.toLower query
    in
    if String.isEmpty query then
        byFilter

    else
        List.filter (\e -> String.contains lowerQuery (String.toLower e.fceFlowName)) byFilter

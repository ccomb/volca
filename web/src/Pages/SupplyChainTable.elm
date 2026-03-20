module Pages.SupplyChainTable exposing (Model, Msg, page)

import Api
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.SupplyChain exposing (SupplyChainEntry, SupplyChainResponse)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)


type SortColumn
    = ByName
    | ByLocation
    | ByQuantity
    | ByUnit
    | ByScalingFactor


type SortDir
    = Asc
    | Desc


type alias Model =
    { activityId : String
    , dbName : String
    , data : RemoteData SupplyChainResponse
    , nameFilter : String
    , sortColumn : SortColumn
    , sortDir : SortDir
    }


type Msg
    = DataLoaded (Result Http.Error SupplyChainResponse)
    | UpdateFilter String
    | SetSort SortColumn
    | NavigateToActivity String
    | RequestLoadDatabase
    | NewFlags ( String, String )


page : Shared.Model -> Spa.Page.Page ( String, String ) Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> ( String, String ) -> ( Model, Effect Shared.Msg Msg )
init shared ( db, activityId ) =
    ( { activityId = activityId
      , dbName = db
      , data = Loading
      , nameFilter = ""
      , sortColumn = ByQuantity
      , sortDir = Desc
      }
    , if Shared.isDatabaseLoaded shared db then
        Effect.fromCmd (Api.loadSupplyChain DataLoaded db activityId)

      else
        Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        DataLoaded (Ok response) ->
            ( { model | data = Loaded response }, Effect.none )

        DataLoaded (Err error) ->
            ( { model | data = Failed (Shared.httpErrorToString error) }, Effect.none )

        UpdateFilter text ->
            ( { model | nameFilter = text }, Effect.none )

        SetSort col ->
            ( { model
                | sortColumn = col
                , sortDir =
                    if model.sortColumn == col then
                        toggleDir model.sortDir

                    else
                        Asc
              }
            , Effect.none
            )

        NavigateToActivity processId ->
            let
                ( dbName, actualProcessId ) =
                    case String.split "::" processId of
                        [ db, pid ] ->
                            ( db, pid )

                        _ ->
                            ( model.dbName, processId )
            in
            ( model
            , Effect.batch
                [ Effect.fromShared (Shared.PushActivity model.dbName model.activityId)
                , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Upstream dbName actualProcessId)))
                ]
            )

        RequestLoadDatabase ->
            ( model, Effect.fromShared (Shared.LoadDatabase model.dbName) )

        NewFlags flags ->
            init shared flags


toggleDir : SortDir -> SortDir
toggleDir dir =
    case dir of
        Asc ->
            Desc

        Desc ->
            Asc


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Supply Chain Table"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ case model.data of
                Loading ->
                    div [ class "has-text-centered" ]
                        [ div [ class "is-size-3" ] [ text "Loading supply chain..." ]
                        , progress [ class "progress is-primary", attribute "max" "100" ] []
                        ]

                Failed err ->
                    div [ class "notification is-danger" ]
                        [ strong [] [ text "Error: " ], text err ]

                Loaded response ->
                    viewTable model response

                NotAsked ->
                    text ""
            ]


viewTable : Model -> SupplyChainResponse -> Html Msg
viewTable model response =
    let
        filtered =
            if String.isEmpty model.nameFilter then
                response.supplyChain

            else
                let
                    lower =
                        String.toLower model.nameFilter
                in
                List.filter (\e -> String.contains lower (String.toLower e.name)) response.supplyChain

        sorted =
            sortEntries model.sortColumn model.sortDir filtered
    in
    div []
        [ div [ class "level", style "margin-bottom" "1rem" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ h2 [ class "title is-5", style "margin-bottom" "0" ]
                        [ text ("Supply Chain (" ++ String.fromInt (List.length filtered) ++ " of " ++ String.fromInt response.totalActivities ++ " activities)") ]
                    ]
                ]
            , div [ class "level-right" ]
                [ div [ class "level-item" ]
                    [ div [ class "field" ]
                        [ div [ class "control has-icons-left" ]
                            [ input
                                [ class "input is-small"
                                , type_ "text"
                                , placeholder "Filter by name..."
                                , value model.nameFilter
                                , onInput UpdateFilter
                                ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-filter" ] [] ]
                            ]
                        ]
                    ]
                ]
            ]
        , table [ class "table is-fullwidth is-hoverable is-striped" ]
            [ thead []
                [ tr []
                    [ sortableHeader model ByName "Name"
                    , sortableHeader model ByLocation "Location"
                    , sortableHeader model ByQuantity "Quantity"
                    , sortableHeader model ByUnit "Unit"
                    , sortableHeader model ByScalingFactor "Scaling Factor"
                    ]
                ]
            , tbody []
                (List.map viewRow sorted)
            ]
        ]


sortableHeader : Model -> SortColumn -> String -> Html Msg
sortableHeader model col label =
    let
        icon =
            if model.sortColumn == col then
                case model.sortDir of
                    Asc ->
                        " \u{25B2}"

                    Desc ->
                        " \u{25BC}"

            else
                ""
    in
    th [ onClick (SetSort col), style "cursor" "pointer", style "user-select" "none" ]
        [ text (label ++ icon) ]


viewRow : SupplyChainEntry -> Html Msg
viewRow entry =
    tr [ onClick (NavigateToActivity entry.processId), style "cursor" "pointer" ]
        [ td [] [ text entry.name ]
        , td [] [ text entry.location ]
        , td [ class "has-text-right" ] [ text (formatFloat entry.quantity) ]
        , td [] [ text entry.unit ]
        , td [ class "has-text-right" ] [ text (formatScientific entry.scalingFactor) ]
        ]


sortEntries : SortColumn -> SortDir -> List SupplyChainEntry -> List SupplyChainEntry
sortEntries col dir entries =
    let
        comparator =
            case col of
                ByName ->
                    List.sortBy .name

                ByLocation ->
                    List.sortBy .location

                ByQuantity ->
                    List.sortBy .quantity

                ByUnit ->
                    List.sortBy .unit

                ByScalingFactor ->
                    List.sortBy .scalingFactor

        sorted =
            comparator entries
    in
    case dir of
        Asc ->
            sorted

        Desc ->
            List.reverse sorted


formatFloat : Float -> String
formatFloat f =
    let
        absF =
            abs f
    in
    if absF == 0 then
        "0"

    else if absF >= 0.01 && absF < 1.0e6 then
        let
            rounded =
                toFloat (round (f * 10000)) / 10000
        in
        String.fromFloat rounded

    else
        formatScientific f


formatScientific : Float -> String
formatScientific f =
    if f == 0 then
        "0"

    else
        let
            absF =
                abs f

            e =
                logBase 10 absF |> floor

            mantissa =
                f / toFloat (10 ^ e)

            roundedM =
                toFloat (round (mantissa * 1000)) / 1000
        in
        String.fromFloat roundedM ++ "e" ++ String.fromInt e

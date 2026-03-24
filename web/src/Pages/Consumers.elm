module Pages.Consumers exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Api
import Http
import Json.Decode
import Models.Activity exposing (ActivityInfo, ActivitySummary)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import Utils.Format as Format
import View exposing (View)
import Views.ActivityHeader


type alias Model =
    { activityId : String
    , dbName : String
    , activityInfo : RemoteData ActivityInfo
    , consumers : RemoteData (List ActivitySummary)
    , nameFilter : String
    }


type Msg
    = ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | ConsumersLoaded (Result Http.Error (List ActivitySummary))
    | SetNameFilter String
    | Search
    | NavigateToActivity String
    | NavigateBack
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
    if not (Shared.isDatabaseLoaded shared db) then
        ( { activityId = activityId
          , dbName = db
          , activityInfo = NotAsked
          , consumers = NotAsked
          , nameFilter = ""
          }
        , Effect.none
        )

    else
        let
            cached =
                Dict.get activityId shared.cachedActivityInfo
        in
        ( { activityId = activityId
          , dbName = db
          , activityInfo =
                case cached of
                    Just info ->
                        Loaded info

                    Nothing ->
                        Loading
          , consumers = Loading
          , nameFilter = ""
          }
        , Effect.batch
            [ case cached of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (Api.loadActivityInfo ActivityInfoLoaded db activityId)
            , Effect.fromCmd (Api.loadConsumers ConsumersLoaded db activityId Nothing)
            ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivityInfoLoaded (Ok info) ->
            ( { model | activityInfo = Loaded info }
            , Effect.fromShared (Shared.CacheActivityInfo model.activityId info)
            )

        ActivityInfoLoaded (Err error) ->
            ( { model | activityInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        ConsumersLoaded (Ok consumers) ->
            ( { model | consumers = Loaded consumers }, Effect.none )

        ConsumersLoaded (Err error) ->
            ( { model | consumers = Failed (Shared.httpErrorToString error) }, Effect.none )

        SetNameFilter name ->
            ( { model | nameFilter = name }, Effect.none )

        Search ->
            let
                filter =
                    if String.isEmpty model.nameFilter then
                        Nothing

                    else
                        Just model.nameFilter
            in
            ( { model | consumers = Loading }
            , Effect.fromCmd (Api.loadConsumers ConsumersLoaded model.dbName model.activityId filter)
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
                , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Consumers dbName actualProcessId)))
                ]
            )

        NavigateBack ->
            ( model, Effect.fromShared Shared.NavigateBackToParent )

        RequestLoadDatabase ->
            ( model, Effect.fromShared (Shared.LoadDatabase model.dbName) )

        NewFlags flags ->
            init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Consumers"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ case model.activityInfo of
                Loaded activityInfo ->
                    div []
                        [ Views.ActivityHeader.viewActivityHeader activityInfo "Consumers" NavigateBack
                        , viewSearchBar model
                        , viewConsumersList model
                        ]

                Loading ->
                    div [ class "has-text-centered", style "padding" "2rem" ]
                        [ progress [ class "progress is-primary", attribute "max" "100" ] [] ]

                Failed err ->
                    div [ class "notification is-danger" ] [ text err ]

                NotAsked ->
                    text ""
            ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div [ class "field has-addons", style "margin" "1rem" ]
        [ div [ class "control is-expanded" ]
            [ input
                [ class "input"
                , type_ "text"
                , placeholder "Filter by name..."
                , value model.nameFilter
                , onInput SetNameFilter
                , onEnter Search
                ]
                []
            ]
        , div [ class "control" ]
            [ button [ class "button is-primary", onClick Search ] [ text "Search" ] ]
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown"
        (Html.Events.keyCode
            |> Json.Decode.andThen
                (\code ->
                    if code == 13 then
                        Json.Decode.succeed msg

                    else
                        Json.Decode.fail ""
                )
        )


viewConsumersList : Model -> Html Msg
viewConsumersList model =
    case model.consumers of
        Loading ->
            div [ class "has-text-centered", style "padding" "1rem" ]
                [ progress [ class "progress is-small is-primary", attribute "max" "100" ] [] ]

        Failed err ->
            div [ class "notification is-warning", style "margin" "1rem" ] [ text err ]

        Loaded consumers ->
            if List.isEmpty consumers then
                div [ style "padding" "1rem", class "has-text-grey" ]
                    [ text "No downstream consumers found." ]

            else
                div [ style "margin" "0 1rem" ]
                    [ p [ class "has-text-grey", style "margin-bottom" "0.5rem" ]
                        [ text (String.fromInt (List.length consumers) ++ " consumers") ]
                    , table [ class "table is-fullwidth is-hoverable is-narrow" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "Name" ]
                                , th [] [ text "Location" ]
                                , th [] [ text "Product" ]
                                , th [ class "has-text-right" ] [ text "Amount" ]
                                , th [] [ text "Unit" ]
                                ]
                            ]
                        , tbody [] (List.map viewConsumerRow consumers)
                        ]
                    ]

        NotAsked ->
            text ""


viewConsumerRow : ActivitySummary -> Html Msg
viewConsumerRow consumer =
    tr [ style "cursor" "pointer", onClick (NavigateToActivity consumer.id) ]
        [ td [] [ a [ href "#" ] [ text consumer.name ] ]
        , td [] [ text consumer.location ]
        , td [] [ text consumer.product ]
        , td [ class "has-text-right" ] [ text (Format.formatScientific consumer.productAmount) ]
        , td [] [ text consumer.productUnit ]
        ]

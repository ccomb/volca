module Pages.Emissions exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Api
import Models.Activity exposing (ActivityInfo, ExchangeType(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.ActivityHeader
import Views.DetailsView as DetailsView


type alias Model =
    { activityId : String
    , dbName : String
    , activityInfo : RemoteData ActivityInfo
    }


type Msg
    = ActivityInfoLoaded (Result Http.Error ActivityInfo)
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
          }
        , case cached of
            Just _ ->
                Effect.none

            Nothing ->
                Effect.fromCmd (Api.loadActivityInfo ActivityInfoLoaded db activityId)
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

        NavigateBack ->
            ( model
            , Effect.fromShared Shared.NavigateBackToParent
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Direct Emissions"
    , body =
        if not (Shared.isDatabaseLoaded shared model.dbName) then
            Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

        else
            div [ class "details-page-container" ]
                [ case model.activityInfo of
                    Loading ->
                        div [ class "has-text-centered" ]
                            [ div [ class "is-size-3" ] [ text "Loading..." ]
                            , progress [ class "progress is-primary", attribute "max" "100" ] []
                            ]

                    Failed err ->
                        div [ class "notification is-danger" ]
                            [ strong [] [ text "Error: " ]
                            , text err
                            ]

                    Loaded activityInfo ->
                        let
                            exchanges =
                                List.filter (\ex -> ex.exchangeType == BiosphereEmissionType) activityInfo.exchanges
                        in
                        div [ style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
                            [ div [ style "flex-shrink" "0" ]
                                [ Views.ActivityHeader.viewActivityHeader activityInfo "Direct Emissions" NavigateBack
                                ]
                            , div [ style "flex" "1", style "display" "flex", style "flex-direction" "column", style "min-height" "0" ]
                                [ DetailsView.viewEmissionsExchanges exchanges
                                ]
                            ]

                    NotAsked ->
                        text ""
                ]
    }

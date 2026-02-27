module Pages.Inventory exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Api
import Models.Activity exposing (ActivityInfo)
import Models.Inventory exposing (InventoryExport, inventoryExportDecoder)
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.ActivityHeader
import Views.InventoryView as InventoryView


type alias Model =
    { activityId : String
    , dbName : String
    , activityInfo : RemoteData ActivityInfo
    , inventory : RemoteData InventoryExport
    , resourcesSearch : String
    , emissionsSearch : String
    }


type Msg
    = ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | InventoryLoaded (Result Http.Error InventoryExport)
    | InventoryViewMsg InventoryView.Msg
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
          , inventory = NotAsked
          , resourcesSearch = ""
          , emissionsSearch = ""
          }
        , Effect.none
        )

    else
        let
            cachedInfo =
                Dict.get activityId shared.cachedActivityInfo

            cachedInventory =
                Dict.get activityId shared.cachedInventories
        in
        ( { activityId = activityId
          , dbName = db
          , activityInfo =
                case cachedInfo of
                    Just info ->
                        Loaded info

                    Nothing ->
                        Loading
          , inventory =
                case cachedInventory of
                    Just inv ->
                        Loaded inv

                    Nothing ->
                        Loading
          , resourcesSearch = ""
          , emissionsSearch = ""
          }
        , Effect.batch
            [ case cachedInfo of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (Api.loadActivityInfo ActivityInfoLoaded db activityId)
            , case cachedInventory of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (loadInventory db activityId)
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

        InventoryLoaded (Ok inventory) ->
            ( { model | inventory = Loaded inventory }
            , Effect.fromShared (Shared.CacheInventory model.activityId inventory)
            )

        InventoryLoaded (Err error) ->
            ( { model | inventory = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        InventoryViewMsg viewMsg ->
            case viewMsg of
                InventoryView.UpdateResourcesSearch query ->
                    ( { model | resourcesSearch = query }, Effect.none )

                InventoryView.UpdateEmissionsSearch query ->
                    ( { model | emissionsSearch = query }, Effect.none )

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
    { title = "Inventory"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ case ( model.activityInfo, model.inventory ) of
                ( Loading, _ ) ->
                    div [ class "has-text-centered" ]
                        [ div [ class "is-size-3" ] [ text "Loading..." ]
                        , progress [ class "progress is-primary", attribute "max" "100" ] []
                        ]

                ( Failed err, _ ) ->
                    div [ class "notification is-danger" ]
                        [ strong [] [ text "Error: " ], text err ]

                ( _, Loading ) ->
                    div [ class "has-text-centered" ]
                        [ div [ class "is-size-3" ] [ text "Loading inventory..." ]
                        , progress [ class "progress is-primary", attribute "max" "100" ] []
                        ]

                ( _, Failed err ) ->
                    div [ class "notification is-danger" ]
                        [ strong [] [ text "Error: " ], text err ]

                ( Loaded activityInfo, Loaded inventory ) ->
                    let
                        resourcesCount =
                            List.length (List.filter (not << .ifdIsEmission) inventory.ieFlows)

                        emissionsCount =
                            List.length (List.filter .ifdIsEmission inventory.ieFlows)
                    in
                    div []
                        [ Views.ActivityHeader.viewActivityHeader activityInfo "Life Cycle Inventory (LCI)" NavigateBack
                        , div [ style "padding" "0.5rem" ]
                            [ Html.map InventoryViewMsg
                                (InventoryView.viewInventoryTables
                                    model.resourcesSearch
                                    model.emissionsSearch
                                    resourcesCount
                                    emissionsCount
                                    inventory.ieFlows
                                )
                            ]
                        ]

                _ ->
                    text ""
            ]




-- HTTP


loadInventory : String -> String -> Cmd Msg
loadInventory dbName activityId =
    Http.get
        { url = "/api/v1/db/" ++ dbName ++ "/activity/" ++ activityId ++ "/inventory"
        , expect = Http.expectJson InventoryLoaded inventoryExportDecoder
        }

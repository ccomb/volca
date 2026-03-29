module Pages.FlowHotspot exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Api
import Models.Activity exposing (ActivityInfo)
import Models.Database exposing (DatabaseLoadStatus(..))
import Models.LCIA exposing (FlowHotspotResult, MethodSummary)
import Models.Method exposing (MethodCollectionList, MethodCollectionStatus)
import Route exposing (HotspotFlags, Route(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.HotspotView as HotspotView
import Views.LCIAView as LCIAView


type alias Model =
    { dbName : String
    , activityId : String
    , collections : RemoteData (List MethodCollectionStatus)
    , selectedCollection : Maybe String
    , allMethods : List MethodSummary
    , collectionMethods : RemoteData (List MethodSummary)
    , selectedMethod : Maybe String
    , result : RemoteData FlowHotspotResult
    }


type Msg
    = CollectionsLoaded (Result Http.Error MethodCollectionList)
    | ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | MethodsLoaded (Result Http.Error (List MethodSummary))
    | SelectCollection String
    | SelectMethod String
    | ResultLoaded (Result Http.Error FlowHotspotResult)
    | RequestLoadDatabase
    | NewFlags HotspotFlags


page : Shared.Model -> Spa.Page.Page HotspotFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> HotspotFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        model =
            { dbName = flags.db
            , activityId = flags.processId
            , collections = Loading
            , selectedCollection = flags.collection
            , allMethods = []
            , collectionMethods = NotAsked
            , selectedMethod = flags.method
            , result = NotAsked
            }
    in
    if not (Shared.isDatabaseLoaded shared flags.db) then
        ( { model | collections = NotAsked }, Effect.none )

    else
        ( model
        , Effect.batch
            [ Effect.fromCmd (Api.loadMethodCollections CollectionsLoaded)
            , case Dict.get flags.processId shared.cachedActivityInfo of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (Api.loadActivityInfo ActivityInfoLoaded flags.db flags.processId)
            ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        CollectionsLoaded (Ok collectionList) ->
            let
                loaded =
                    List.filter (\c -> c.status == DbLoaded) collectionList.methods

                autoSelect =
                    case model.selectedCollection of
                        Just _ ->
                            model.selectedCollection

                        Nothing ->
                            case loaded of
                                [ single ] ->
                                    Just single.name

                                _ ->
                                    Nothing

                newModel =
                    { model | collections = Loaded loaded, selectedCollection = autoSelect }
            in
            case autoSelect of
                Just _ ->
                    ( { newModel | collectionMethods = Loading }
                    , Effect.fromCmd (Api.loadMethods MethodsLoaded)
                    )

                Nothing ->
                    ( newModel, Effect.none )

        CollectionsLoaded (Err err) ->
            ( { model | collections = Failed (Shared.httpErrorToString err) }, Effect.none )

        ActivityInfoLoaded (Ok info) ->
            ( model, Effect.fromShared (Shared.CacheActivityInfo model.activityId info) )

        ActivityInfoLoaded (Err _) ->
            ( model, Effect.none )

        MethodsLoaded (Ok methods) ->
            let
                collMethods =
                    case model.selectedCollection of
                        Nothing ->
                            []

                        Just col ->
                            List.filter (\m -> m.msmCollection == col) methods

                newModel =
                    { model | allMethods = methods, collectionMethods = Loaded collMethods }
            in
            case ( model.selectedMethod, List.any (\m -> Just m.msmId == model.selectedMethod) collMethods ) of
                ( Just mid, True ) ->
                    ( { newModel | result = Loading }
                    , Effect.fromCmd (Api.loadFlowHotspot ResultLoaded model.dbName model.activityId (Maybe.withDefault "" model.selectedCollection) mid 20)
                    )

                _ ->
                    ( newModel, Effect.none )

        MethodsLoaded (Err err) ->
            ( { model | collectionMethods = Failed (Shared.httpErrorToString err) }, Effect.none )

        SelectCollection name ->
            if name == "" then
                ( { model | selectedCollection = Nothing, collectionMethods = NotAsked, selectedMethod = Nothing, result = NotAsked }
                , Effect.fromShared (Shared.NavigateTo (FlowHotspotRoute model.dbName model.activityId Nothing Nothing))
                )

            else
                let
                    collMethods =
                        List.filter (\m -> m.msmCollection == name) model.allMethods

                    newModel =
                        { model
                            | selectedCollection = Just name
                            , selectedMethod = Nothing
                            , result = NotAsked
                            , collectionMethods =
                                if List.isEmpty model.allMethods then
                                    Loading

                                else
                                    Loaded collMethods
                        }
                in
                ( newModel
                , Effect.batch
                    [ Effect.fromShared (Shared.NavigateTo (FlowHotspotRoute model.dbName model.activityId (Just name) Nothing))
                    , if List.isEmpty model.allMethods then
                        Effect.fromCmd (Api.loadMethods MethodsLoaded)

                      else
                        Effect.none
                    ]
                )

        SelectMethod methodId ->
            if methodId == "" then
                ( { model | selectedMethod = Nothing, result = NotAsked }
                , Effect.fromShared (Shared.NavigateTo (FlowHotspotRoute model.dbName model.activityId model.selectedCollection Nothing))
                )

            else
                ( { model | selectedMethod = Just methodId, result = Loading }
                , Effect.batch
                    [ Effect.fromCmd (Api.loadFlowHotspot ResultLoaded model.dbName model.activityId (Maybe.withDefault "" model.selectedCollection) methodId 20)
                    , Effect.fromShared (Shared.NavigateTo (FlowHotspotRoute model.dbName model.activityId model.selectedCollection (Just methodId)))
                    ]
                )

        ResultLoaded (Ok result) ->
            ( { model | result = Loaded result }, Effect.none )

        ResultLoaded (Err err) ->
            ( { model | result = Failed (Shared.httpErrorToString err) }, Effect.none )

        RequestLoadDatabase ->
            ( model, Effect.fromShared (Shared.LoadDatabase model.dbName) )

        NewFlags flags ->
            case model.collections of
                NotAsked ->
                    init shared flags

                _ ->
                    if flags.db == model.dbName && flags.processId == model.activityId then
                        ( model, Effect.none )

                    else
                        init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "Flow Hotspot"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        let
            activityInfo =
                Dict.get model.activityId shared.cachedActivityInfo
                    |> Maybe.map (\info -> ( info.name, info.location ))
        in
        { title = "Flow Hotspot"
        , body =
            div [ class "lcia-page" ]
                [ LCIAView.viewPageNavbar "Flow Hotspot" activityInfo
                , HotspotView.viewMethodPicker
                    { collections = model.collections
                    , selectedCollection = model.selectedCollection
                    , methods = model.collectionMethods
                    , selectedMethod = model.selectedMethod
                    , onSelectCollection = SelectCollection
                    , onSelectMethod = SelectMethod
                    }
                , HotspotView.viewFlowHotspotResult model.result
                ]
        }

module Pages.LCIA exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Api
import Models.Activity exposing (ActivityInfo)
import Models.Database exposing (DatabaseLoadStatus(..))
import Models.LCIA exposing (LCIABatchResult, LCIAResult)
import Models.Method exposing (MethodCollectionList, MethodCollectionStatus)
import Route exposing (LCIAFlags, Route(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.LCIAView as LCIAView


type alias Model =
    { dbName : String
    , activityId : String
    , collections : RemoteData (List MethodCollectionStatus)
    , selectedCollection : Maybe String
    , batchResult : RemoteData LCIABatchResult
    , expandedRow : Maybe String -- methodId of expanded row
    , viewMode : LCIAView.ViewMode
    }


type Msg
    = CollectionsLoaded (Result Http.Error MethodCollectionList)
    | ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | SelectCollection String
    | BatchResultsLoaded (Result Http.Error LCIABatchResult)
    | ToggleRow String
    | SetViewMode LCIAView.ViewMode
    | RequestLoadDatabase
    | NewFlags LCIAFlags


page : Shared.Model -> Spa.Page.Page LCIAFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> LCIAFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        model =
            { dbName = flags.db
            , activityId = flags.processId
            , collections = Loading
            , selectedCollection = flags.method
            , batchResult = NotAsked
            , expandedRow = Nothing
            , viewMode = LCIAView.Raw
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
                    { model
                        | collections = Loaded loaded
                        , selectedCollection = autoSelect
                    }
            in
            case autoSelect of
                Just col ->
                    ( { newModel | batchResult = Loading }
                    , Effect.fromCmd (Api.computeLCIABatch BatchResultsLoaded model.dbName model.activityId col)
                    )

                Nothing ->
                    ( newModel, Effect.none )

        CollectionsLoaded (Err error) ->
            ( { model | collections = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        ActivityInfoLoaded (Ok info) ->
            ( model
            , Effect.fromShared (Shared.CacheActivityInfo model.activityId info)
            )

        ActivityInfoLoaded (Err _) ->
            ( model, Effect.none )

        SelectCollection name ->
            if name == "" then
                ( { model | selectedCollection = Nothing, batchResult = NotAsked }
                , Effect.fromShared (Shared.NavigateTo (LCIARoute model.dbName model.activityId Nothing))
                )

            else
                ( { model | selectedCollection = Just name, batchResult = Loading, expandedRow = Nothing }
                , Effect.batch
                    [ Effect.fromCmd (Api.computeLCIABatch BatchResultsLoaded model.dbName model.activityId name)
                    , Effect.fromShared (Shared.NavigateTo (LCIARoute model.dbName model.activityId (Just name)))
                    ]
                )

        BatchResultsLoaded (Ok result) ->
            ( { model | batchResult = Loaded result }, Effect.none )

        BatchResultsLoaded (Err error) ->
            ( { model | batchResult = Failed (Shared.httpErrorToString error) }, Effect.none )

        ToggleRow methodId ->
            ( { model
                | expandedRow =
                    if model.expandedRow == Just methodId then
                        Nothing

                    else
                        Just methodId
              }
            , Effect.none
            )

        SetViewMode mode ->
            ( { model | viewMode = mode }, Effect.none )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            if flags.db == model.dbName && flags.processId == model.activityId && flags.method == model.selectedCollection then
                ( model, Effect.none )

            else
                init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "LCIA"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        viewLoaded shared model


viewLoaded : Shared.Model -> Model -> View Msg
viewLoaded shared model =
    let
        activityInfo =
            Dict.get model.activityId shared.cachedActivityInfo
                |> Maybe.map (\info -> ( info.name, info.location ))
    in
    { title = "LCIA"
    , body =
        LCIAView.viewLCIAPage
            { collections = model.collections
            , selectedCollection = model.selectedCollection
            , batchResult = model.batchResult
            , expandedRow = model.expandedRow
            , activityInfo = activityInfo
            , viewMode = model.viewMode
            , onSelectCollection = SelectCollection
            , onToggleRow = ToggleRow
            , onSetViewMode = SetViewMode
            }
    }

module Pages.DatabaseDetail exposing (Model, Msg, page)

import Api
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.Database exposing (DatabaseLoadStatus(..), DatabaseStatus)
import Models.LCIA exposing (MappingStatus, MethodSummary, methodsListDecoder)
import Models.Method exposing (MethodCollectionList, MethodCollectionStatus)
import Route exposing (DatabaseMappingFlags)
import Shared exposing (RemoteData(..))
import Spa.Page
import Url
import View exposing (View)
import Views.MappingView as MappingView


type alias Model =
    { dbName : String
    , selectedCollection : Maybe String
    , collections : RemoteData (List MethodCollectionStatus)
    , methods : RemoteData (List MethodSummary)
    , mappings : Dict String MappingStatus
    , mappingLoading : Bool
    }


type Msg
    = CollectionsLoaded (Result Http.Error MethodCollectionList)
    | MethodsLoaded (Result Http.Error (List MethodSummary))
    | SelectCollection String
    | MappingLoaded String (Result Http.Error MappingStatus)
    | ClickMethod String
    | GoBack
    | GoToActivities


page : Shared.Model -> Spa.Page.Page DatabaseMappingFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> DatabaseMappingFlags -> ( Model, Effect Shared.Msg Msg )
init _ flags =
    let
        dbName =
            Url.percentDecode flags.dbName |> Maybe.withDefault flags.dbName
    in
    ( { dbName = dbName
      , selectedCollection = flags.method
      , collections = Loading
      , methods =
            case flags.method of
                Just _ ->
                    Loading

                Nothing ->
                    NotAsked
      , mappings = Dict.empty
      , mappingLoading = False
      }
    , Effect.batch
        [ Effect.fromCmd (Api.loadMethodCollections CollectionsLoaded)
        , case flags.method of
            Just _ ->
                Effect.fromCmd fetchMethods

            Nothing ->
                Effect.none
        ]
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        CollectionsLoaded (Ok collectionList) ->
            let
                loaded =
                    List.filter (\c -> c.status == DbLoaded) collectionList.methods
            in
            ( { model | collections = Loaded loaded }, Effect.none )

        CollectionsLoaded (Err err) ->
            ( { model | collections = Failed (Shared.httpErrorToString err) }, Effect.none )

        MethodsLoaded (Ok allMethods) ->
            let
                filtered =
                    case model.selectedCollection of
                        Just coll ->
                            List.filter (\m -> m.msmCollection == coll) allMethods

                        Nothing ->
                            []

                newModel =
                    { model | methods = Loaded filtered, mappingLoading = True }
            in
            ( newModel
            , Effect.batch (List.map (\m -> Effect.fromCmd (Api.loadMethodMapping (MappingLoaded m.msmId) m.msmId model.dbName)) filtered)
            )

        MethodsLoaded (Err err) ->
            ( { model | methods = Failed (Shared.httpErrorToString err) }, Effect.none )

        SelectCollection name ->
            if name == "" then
                ( { model | selectedCollection = Nothing, methods = NotAsked, mappings = Dict.empty, mappingLoading = False }
                , Effect.fromShared (Shared.NavigateTo (Route.DatabaseMappingRoute model.dbName Nothing))
                )

            else
                ( { model | selectedCollection = Just name, methods = Loading, mappings = Dict.empty, mappingLoading = False }
                , Effect.batch
                    [ Effect.fromShared (Shared.NavigateTo (Route.DatabaseMappingRoute model.dbName (Just name)))
                    , Effect.fromCmd fetchMethods
                    ]
                )

        MappingLoaded methodId (Ok mapping) ->
            let
                newMappings =
                    Dict.insert methodId mapping model.mappings

                allLoaded =
                    case model.methods of
                        Loaded ms ->
                            Dict.size newMappings >= List.length ms

                        _ ->
                            False
            in
            ( { model | mappings = newMappings, mappingLoading = not allLoaded }, Effect.none )

        MappingLoaded _ (Err _) ->
            ( { model | mappingLoading = False }, Effect.none )

        ClickMethod methodId ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.DatabaseMappingRoute model.dbName (Just methodId))))
            )

        GoBack ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.DatabaseSetupRoute model.dbName)))
            )

        GoToActivities ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivitiesRoute { db = model.dbName, name = Nothing, limit = Just 20 })))
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        maybeDbInfo =
            case shared.databases of
                Loaded dbList ->
                    List.filter (\db -> db.name == model.dbName) dbList.databases
                        |> List.head

                _ ->
                    Nothing
    in
    { title = "Mapping: " ++ model.dbName
    , body =
        div [ class "databases-page" ]
            [ div [ class "box" ]
                [ div [ class "level" ]
                    [ div [ class "level-left" ]
                        [ div [ class "level-item" ]
                            [ button [ class "button is-text", onClick GoBack ]
                                [ span [ class "icon" ] [ i [ class "fas fa-arrow-left" ] [] ]
                                ]
                            ]
                        , div [ class "level-item" ]
                            [ h2 [ class "title is-3", style "margin-bottom" "0" ]
                                [ text (Maybe.map .displayName maybeDbInfo |> Maybe.withDefault model.dbName) ]
                            ]
                        ]
                    , div [ class "level-right" ]
                        [ div [ class "level-item" ]
                            [ button [ class "button is-primary is-outlined", onClick GoToActivities ]
                                [ span [ class "icon" ] [ i [ class "fas fa-search" ] [] ]
                                , span [] [ text "Search activities" ]
                                ]
                            ]
                        ]
                    ]
                , viewDbInfo maybeDbInfo
                , viewCollectionPicker model
                ]
            , case model.methods of
                Loading ->
                    div [ class "has-text-centered", style "padding" "2rem" ]
                        [ span [ class "icon is-large has-text-primary" ]
                            [ i [ class "fas fa-spinner fa-spin fa-2x" ] [] ]
                        ]

                Failed err ->
                    div [ class "notification is-danger" ] [ text err ]

                Loaded methods ->
                    MappingView.viewMappingOverview
                        { methods = methods
                        , mappings = model.mappings
                        , loading = model.mappingLoading
                        , onClickMethod = ClickMethod
                        }

                NotAsked ->
                    text ""
            ]
    }


viewDbInfo : Maybe DatabaseStatus -> Html msg
viewDbInfo maybeDb =
    case maybeDb of
        Just db ->
            div [ style "display" "flex", style "gap" "1rem", style "margin-top" "0.5rem" ]
                [ span [ class "tag is-info is-light" ]
                    [ text (String.fromInt db.activityCount ++ " activities") ]
                , case db.format of
                    Just fmt ->
                        span [ class "tag is-light" ] [ text fmt ]

                    Nothing ->
                        text ""
                ]

        Nothing ->
            text ""


viewCollectionPicker : Model -> Html Msg
viewCollectionPicker model =
    case model.collections of
        Loaded collections ->
            if List.isEmpty collections then
                p [ class "has-text-grey", style "margin-top" "0.5rem" ]
                    [ text "No method collections loaded." ]

            else
                div [ class "field has-addons", style "margin-top" "0.75rem" ]
                    [ div [ class "control" ]
                        [ span [ class "button is-static" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-flask" ] [] ]
                            , span [] [ text "Check mapping" ]
                            ]
                        ]
                    , div [ class "control is-expanded" ]
                        [ div [ class "select is-fullwidth" ]
                            [ select [ onInput SelectCollection ]
                                (option [ value "" ] [ text "-- Select a method collection --" ]
                                    :: List.map
                                        (\c ->
                                            option
                                                [ value c.name
                                                , selected (model.selectedCollection == Just c.name)
                                                ]
                                                [ text (c.displayName ++ " (" ++ String.fromInt c.methodCount ++ " categories)") ]
                                        )
                                        collections
                                )
                            ]
                        ]
                    ]

        Loading ->
            div [ class "has-text-centered", style "padding" "1rem" ]
                [ span [ class "icon has-text-grey-light" ] [ i [ class "fas fa-spinner fa-spin" ] [] ] ]

        _ ->
            text ""



-- HTTP


fetchMethods : Cmd Msg
fetchMethods =
    Http.get
        { url = "/api/v1/methods"
        , expect = Http.expectJson MethodsLoaded methodsListDecoder
        }

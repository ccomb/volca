module Pages.MethodDetail exposing (Model, Msg, page)

import Api
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.Database exposing (DatabaseLoadStatus(..))
import Models.LCIA exposing (MappingStatus, MethodSummary, methodsListDecoder)
import Route exposing (MethodDetailFlags)
import Shared exposing (RemoteData(..))
import Spa.Page
import Url
import View exposing (View)
import Views.MappingView as MappingView


type alias Model =
    { collectionName : String
    , selectedDb : Maybe String
    , methods : RemoteData (List MethodSummary)
    , mappings : Dict String MappingStatus
    , mappingLoading : Bool
    }


type Msg
    = MethodsLoaded (Result Http.Error (List MethodSummary))
    | SelectDatabase String
    | MappingLoaded String (Result Http.Error MappingStatus)
    | ClickMethod String
    | GoBack


page : Shared.Model -> Spa.Page.Page MethodDetailFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> MethodDetailFlags -> ( Model, Effect Shared.Msg Msg )
init _ flags =
    let
        collectionName =
            Url.percentDecode flags.collection |> Maybe.withDefault flags.collection
    in
    ( { collectionName = collectionName
      , selectedDb = flags.db
      , methods = Loading
      , mappings = Dict.empty
      , mappingLoading = False
      }
    , Effect.fromCmd fetchMethods
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        MethodsLoaded (Ok allMethods) ->
            let
                filtered =
                    List.filter (\m -> m.msmCollection == model.collectionName) allMethods

                newModel =
                    { model | methods = Loaded filtered }
            in
            case model.selectedDb of
                Just dbName ->
                    ( { newModel | mappingLoading = True }
                    , Effect.batch (List.map (\m -> Effect.fromCmd (Api.loadMethodMapping (MappingLoaded m.msmId) m.msmId dbName)) filtered)
                    )

                Nothing ->
                    ( newModel, Effect.none )

        MethodsLoaded (Err err) ->
            ( { model | methods = Shared.Failed (Shared.httpErrorToString err) }
            , Effect.none
            )

        SelectDatabase name ->
            if name == "" then
                ( { model | selectedDb = Nothing, mappings = Dict.empty, mappingLoading = False }
                , Effect.fromShared (Shared.NavigateTo (Route.MethodDetailRoute model.collectionName Nothing))
                )

            else
                let
                    methods =
                        case model.methods of
                            Loaded ms ->
                                ms

                            _ ->
                                []
                in
                ( { model | selectedDb = Just name, mappings = Dict.empty, mappingLoading = True }
                , Effect.batch
                    (Effect.fromShared (Shared.NavigateTo (Route.MethodDetailRoute model.collectionName (Just name)))
                        :: List.map (\m -> Effect.fromCmd (Api.loadMethodMapping (MappingLoaded m.msmId) m.msmId name)) methods
                    )
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
            ( { model | mappings = newMappings, mappingLoading = not allLoaded }
            , Effect.none
            )

        MappingLoaded _ (Err _) ->
            ( { model | mappingLoading = False }, Effect.none )

        ClickMethod methodId ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.FlowMappingRoute methodId model.selectedDb)))
            )

        GoBack ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.MethodsRoute))
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Method: " ++ model.collectionName
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
                                [ text model.collectionName ]
                            ]
                        ]
                    ]
                , viewDbPicker shared model
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
                    if model.selectedDb /= Nothing then
                        MappingView.viewMappingOverview
                            { methods = methods
                            , mappings = model.mappings
                            , loading = model.mappingLoading
                            , onClickMethod = ClickMethod
                            }

                    else
                        viewMethodsTable methods

                NotAsked ->
                    text ""
            ]
    }


viewDbPicker : Shared.Model -> Model -> Html Msg
viewDbPicker shared model =
    case shared.databases of
        Loaded dbList ->
            let
                loadedDbs =
                    List.filter (\db -> db.status == DbLoaded) dbList.databases
            in
            if List.isEmpty loadedDbs then
                p [ class "subtitle", style "margin-top" "0.5rem" ]
                    [ text "No databases loaded." ]

            else
                div [ class "field has-addons", style "margin-top" "0.75rem" ]
                    [ div [ class "control" ]
                        [ span [ class "button is-static" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-database" ] [] ]
                            , span [] [ text "Check mapping" ]
                            ]
                        ]
                    , div [ class "control is-expanded" ]
                        [ div [ class "select is-fullwidth" ]
                            [ select [ onInput SelectDatabase ]
                                (option [ value "" ] [ text "-- Select a database --" ]
                                    :: List.map
                                        (\db ->
                                            option
                                                [ value db.name
                                                , selected (model.selectedDb == Just db.name)
                                                ]
                                                [ text db.displayName ]
                                        )
                                        loadedDbs
                                )
                            ]
                        ]
                    ]

        _ ->
            text ""


viewMethodsTable : List MethodSummary -> Html Msg
viewMethodsTable methods =
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt (List.length methods) ++ " impact categories") ]
            ]
        , table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Impact Category" ]
                    , th [] [ text "Unit" ]
                    , th [ style "text-align" "right" ] [ text "Characterization Factors" ]
                    ]
                ]
            , tbody []
                (List.map viewMethodRow methods)
            ]
        ]


viewMethodRow : MethodSummary -> Html Msg
viewMethodRow method =
    tr []
        [ td [] [ text method.msmName ]
        , td [ class "has-text-grey" ] [ text method.msmCategory ]
        , td [ class "has-text-grey" ] [ text method.msmUnit ]
        , td [ style "text-align" "right" ] [ text (String.fromInt method.msmFactorCount) ]
        ]



-- HTTP


fetchMethods : Cmd Msg
fetchMethods =
    Http.get
        { url = "/api/v1/methods"
        , expect = Http.expectJson MethodsLoaded methodsListDecoder
        }

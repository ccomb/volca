module Pages.Variant exposing (Model, Msg, page)

import Api
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.Activity exposing (ActivitySummary, SearchResults, activitySummaryDecoder, searchResultsDecoder)
import Models.SupplyChain exposing (SupplyChainEntry, SupplyChainResponse)
import Models.Variant exposing (encodeVariantRequest)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import Url.Builder
import View exposing (View)


type alias Model =
    { activityId : String
    , dbName : String
    , supplyChain : RemoteData SupplyChainResponse
    , substitutions : List { from : String, to : String, consumer : String }
    , swappingEntry : Maybe SupplyChainEntry
    , searchQuery : String
    , searchResults : RemoteData (SearchResults ActivitySummary)
    , result : RemoteData SupplyChainResponse
    }


type Msg
    = SupplyChainLoaded (Result Http.Error SupplyChainResponse)
    | StartSwap SupplyChainEntry
    | CancelSwap
    | UpdateSearch String
    | SearchLoaded (Result Http.Error (SearchResults ActivitySummary))
    | SelectReplacement String
    | RemoveSubstitution String
    | SubmitVariant
    | VariantLoaded (Result Http.Error SupplyChainResponse)
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
      , supplyChain = Loading
      , substitutions = []
      , swappingEntry = Nothing
      , searchQuery = ""
      , searchResults = NotAsked
      , result = NotAsked
      }
    , if Shared.isDatabaseLoaded shared db then
        Effect.fromCmd (Api.loadSupplyChain SupplyChainLoaded db activityId Api.defaultSupplyChainParams)

      else
        Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        SupplyChainLoaded (Ok response) ->
            ( { model | supplyChain = Loaded response }, Effect.none )

        SupplyChainLoaded (Err error) ->
            ( { model | supplyChain = Failed (Shared.httpErrorToString error) }, Effect.none )

        StartSwap entry ->
            ( { model | swappingEntry = Just entry, searchQuery = "", searchResults = NotAsked }, Effect.none )

        CancelSwap ->
            ( { model | swappingEntry = Nothing, searchQuery = "", searchResults = NotAsked }, Effect.none )

        UpdateSearch query ->
            if String.isEmpty query then
                ( { model | searchQuery = query, searchResults = NotAsked }, Effect.none )

            else
                ( { model | searchQuery = query, searchResults = Loading }
                , Effect.fromCmd (searchActivities model.dbName query)
                )

        SearchLoaded (Ok results) ->
            ( { model | searchResults = Loaded results }, Effect.none )

        SearchLoaded (Err error) ->
            ( { model | searchResults = Failed (Shared.httpErrorToString error) }, Effect.none )

        SelectReplacement toId ->
            case model.swappingEntry of
                Just entry ->
                    let
                        consumer =
                            case model.supplyChain of
                                Loaded response ->
                                    response.edges
                                        |> List.filter (\e -> e.from == entry.processId)
                                        |> List.head
                                        |> Maybe.map .to
                                        |> Maybe.withDefault model.activityId

                                _ ->
                                    model.activityId
                    in
                    ( { model
                        | substitutions = { from = entry.processId, to = toId, consumer = consumer } :: List.filter (\s -> s.from /= entry.processId) model.substitutions
                        , swappingEntry = Nothing
                        , searchQuery = ""
                        , searchResults = NotAsked
                      }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )

        RemoveSubstitution fromId ->
            ( { model | substitutions = List.filter (\s -> s.from /= fromId) model.substitutions }, Effect.none )

        SubmitVariant ->
            if List.isEmpty model.substitutions then
                ( model, Effect.none )

            else
                ( { model | result = Loading }
                , Effect.fromCmd (Api.createVariant VariantLoaded model.dbName model.activityId (encodeVariantRequest model.substitutions))
                )

        VariantLoaded (Ok response) ->
            ( { model | result = Loaded response }, Effect.none )

        VariantLoaded (Err error) ->
            ( { model | result = Failed (Shared.httpErrorToString error) }, Effect.none )

        RequestLoadDatabase ->
            ( model, Effect.fromShared (Shared.LoadDatabase model.dbName) )

        NewFlags flags ->
            init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Variant Analysis"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ h2 [ class "title is-4" ] [ text "Variant Analysis" ]
            , p [ class "has-text-grey", style "margin-bottom" "1rem" ]
                [ text "Swap suppliers in the supply chain to see how quantities change." ]
            , case model.supplyChain of
                Loading ->
                    div [ class "has-text-centered" ]
                        [ div [ class "is-size-5" ] [ text "Loading supply chain..." ]
                        , progress [ class "progress is-primary", attribute "max" "100" ] []
                        ]

                Failed err ->
                    div [ class "notification is-danger" ]
                        [ strong [] [ text "Error: " ], text err ]

                Loaded response ->
                    div []
                        [ viewSubstitutions model
                        , viewSubmitButton model
                        , viewResult model response
                        , viewSupplyChainWithSwap model response
                        , viewSwapModal model
                        ]

                NotAsked ->
                    text ""
            ]


viewSubstitutions : Model -> Html Msg
viewSubstitutions model =
    if List.isEmpty model.substitutions then
        text ""

    else
        div [ class "box", style "margin-bottom" "1rem" ]
            [ h3 [ class "title is-6" ] [ text "Planned Substitutions" ]
            , div [ class "tags" ]
                (List.map
                    (\sub ->
                        span [ class "tag is-info is-medium" ]
                            [ text (truncate 20 sub.from ++ " \u{2192} " ++ truncate 20 sub.to)
                            , button [ class "delete is-small", onClick (RemoveSubstitution sub.from) ] []
                            ]
                    )
                    model.substitutions
                )
            ]


viewSupplyChainWithSwap : Model -> SupplyChainResponse -> Html Msg
viewSupplyChainWithSwap model response =
    let
        hasSubstitution entry =
            List.any (\s -> s.from == entry.processId) model.substitutions
    in
    table [ class "table is-fullwidth is-hoverable is-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Location" ]
                , th [ class "has-text-right" ] [ text "Quantity" ]
                , th [] [ text "Unit" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody []
            (List.map
                (\entry ->
                    tr
                        [ classList [ ( "has-background-info-light", hasSubstitution entry ) ] ]
                        [ td [] [ text entry.name ]
                        , td [] [ text entry.location ]
                        , td [ class "has-text-right" ] [ text (String.fromFloat (toFloat (round (entry.quantity * 10000)) / 10000)) ]
                        , td [] [ text entry.unit ]
                        , td []
                            [ button
                                [ class "button is-small is-outlined"
                                , onClick (StartSwap entry)
                                ]
                                [ span [ class "icon is-small" ] [ i [ class "fas fa-exchange-alt" ] [] ]
                                , span [] [ text "Swap" ]
                                ]
                            ]
                        ]
                )
                (List.take 100 response.supplyChain)
            )
        ]


viewSwapModal : Model -> Html Msg
viewSwapModal model =
    case model.swappingEntry of
        Nothing ->
            text ""

        Just entry ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background", onClick CancelSwap ] []
                , div [ class "modal-card" ]
                    [ Html.header [ class "modal-card-head" ]
                        [ p [ class "modal-card-title" ] [ text ("Replace: " ++ entry.name) ]
                        , button [ class "delete", onClick CancelSwap ] []
                        ]
                    , section [ class "modal-card-body" ]
                        [ div [ class "field" ]
                            [ div [ class "control has-icons-left" ]
                                [ input
                                    [ class "input"
                                    , type_ "text"
                                    , placeholder "Search replacement activity..."
                                    , value model.searchQuery
                                    , onInput UpdateSearch
                                    , Html.Attributes.autofocus True
                                    ]
                                    []
                                , span [ class "icon is-small is-left" ]
                                    [ i [ class "fas fa-search" ] [] ]
                                ]
                            ]
                        , case model.searchResults of
                            Loading ->
                                progress [ class "progress is-primary is-small", attribute "max" "100" ] []

                            Loaded results ->
                                if List.isEmpty results.results then
                                    p [ class "has-text-grey" ] [ text "No results found." ]

                                else
                                    div [ style "max-height" "300px", style "overflow-y" "auto" ]
                                        [ table [ class "table is-fullwidth is-hoverable is-narrow" ]
                                            [ tbody []
                                                (List.map
                                                    (\a ->
                                                        tr [ onClick (SelectReplacement a.id), style "cursor" "pointer" ]
                                                            [ td [] [ text a.name ]
                                                            , td [] [ text a.location ]
                                                            , td [] [ text a.product ]
                                                            ]
                                                    )
                                                    results.results
                                                )
                                            ]
                                        ]

                            Failed err ->
                                div [ class "notification is-danger is-light" ] [ text err ]

                            NotAsked ->
                                p [ class "has-text-grey" ] [ text "Type to search for a replacement activity." ]
                        ]
                    ]
                ]


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    if List.isEmpty model.substitutions then
        text ""

    else
        div [ class "has-text-centered", style "margin" "1rem 0" ]
            [ button
                [ class
                    ("button is-primary is-medium"
                        ++ (if model.result == Loading then
                                " is-loading"

                            else
                                ""
                           )
                    )
                , onClick SubmitVariant
                , disabled (model.result == Loading)
                ]
                [ span [ class "icon" ] [ i [ class "fas fa-play" ] [] ]
                , span [] [ text "Compute Variant" ]
                ]
            ]


viewResult : Model -> SupplyChainResponse -> Html Msg
viewResult model original =
    case model.result of
        NotAsked ->
            text ""

        Loading ->
            text ""

        Failed err ->
            div [ class "notification is-danger" ]
                [ strong [] [ text "Error: " ], text err ]

        Loaded response ->
            div [ class "box" ]
                [ h3 [ class "title is-5" ] [ text "Variant Result" ]
                , p [ class "has-text-grey", style "margin-bottom" "0.5rem" ]
                    [ text ("Total activities: " ++ String.fromInt response.totalActivities) ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ h4 [ class "title is-6" ] [ text "Original" ]
                        , viewCompactTable original.supplyChain
                        ]
                    , div [ class "column" ]
                        [ h4 [ class "title is-6" ] [ text "Variant" ]
                        , viewCompactTable response.supplyChain
                        ]
                    ]
                ]


viewCompactTable : List SupplyChainEntry -> Html Msg
viewCompactTable entries =
    table [ class "table is-fullwidth is-narrow is-size-7" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [ class "has-text-right" ] [ text "Quantity" ]
                , th [] [ text "Unit" ]
                ]
            ]
        , tbody []
            (List.map
                (\e ->
                    tr []
                        [ td [] [ text (truncate 30 e.name) ]
                        , td [ class "has-text-right" ] [ text (String.fromFloat (toFloat (round (e.quantity * 10000)) / 10000)) ]
                        , td [] [ text e.unit ]
                        ]
                )
                (List.take 50 entries)
            )
        ]


truncate : Int -> String -> String
truncate n s =
    if String.length s > n then
        String.left n s ++ "..."

    else
        s


searchActivities : String -> String -> Cmd Msg
searchActivities dbName query =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                [ Url.Builder.string "name" query
                , Url.Builder.int "limit" 10
                ]
        , expect = Http.expectJson SearchLoaded (searchResultsDecoder activitySummaryDecoder)
        }

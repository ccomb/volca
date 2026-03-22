module Pages.Upload exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Json.Encode
import Models.Database exposing (UploadResponse, uploadResponseDecoder)
import Route
import Shared
import Spa.Page
import View exposing (View)
import Views.UploadView as UploadView


type alias Model =
    { uploadView : UploadView.Model
    , uploadState : UploadState
    }


type UploadState
    = Idle
    | Uploading
    | Succeeded String
    | UploadFailed String


type Msg
    = UploadViewMsg UploadView.Msg
    | UploadResult (Result Http.Error UploadResponse)


page : Shared.Model -> Spa.Page.Page () Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> () -> ( Model, Effect Shared.Msg Msg )
init _ _ =
    ( { uploadView = UploadView.init
      , uploadState = Idle
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        UploadViewMsg viewMsg ->
            case viewMsg of
                UploadView.UploadDatabase ->
                    case model.uploadView.fileContent of
                        Just content ->
                            let
                                body =
                                    Json.Encode.object
                                        [ ( "urName", Json.Encode.string model.uploadView.name )
                                        , ( "urDescription"
                                          , if String.isEmpty model.uploadView.description then
                                                Json.Encode.null

                                            else
                                                Json.Encode.string model.uploadView.description
                                          )
                                        , ( "urFileData", Json.Encode.string content )
                                        ]
                            in
                            let
                                updatedView =
                                    model.uploadView
                            in
                            ( { model
                                | uploadState = Uploading
                                , uploadView = { updatedView | uploading = True }
                              }
                            , Effect.fromCmd (uploadDatabase body)
                            )

                        Nothing ->
                            ( model, Effect.none )

                UploadView.CancelUpload ->
                    ( { model | uploadView = UploadView.init, uploadState = Idle }
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.DatabasesRoute))
                    )

                _ ->
                    let
                        ( newUploadView, cmd ) =
                            UploadView.update viewMsg model.uploadView
                    in
                    ( { model | uploadView = newUploadView }
                    , Effect.fromCmd (Cmd.map UploadViewMsg cmd)
                    )

        UploadResult (Ok response) ->
            if response.success then
                let
                    updatedView =
                        { name = ""
                        , description = ""
                        , selectedFile = Nothing
                        , fileContent = Nothing
                        , uploading = False
                        , error = Nothing
                        , success = Just response.message
                        }
                in
                ( { model | uploadView = updatedView, uploadState = Succeeded response.message }
                , Effect.batch
                    [ Effect.fromShared Shared.LoadDatabases
                    , case response.slug of
                        Just slug ->
                            Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.DatabaseSetupRoute slug)))

                        Nothing ->
                            Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.DatabasesRoute))
                    ]
                )

            else
                let
                    updatedView =
                        model.uploadView

                    viewWithError =
                        { updatedView | uploading = False, error = Just response.message }
                in
                ( { model | uploadView = viewWithError, uploadState = UploadFailed response.message }
                , Effect.none
                )

        UploadResult (Err err) ->
            let
                errorMsg =
                    Shared.httpErrorToString err

                updatedView =
                    model.uploadView

                viewWithError =
                    { updatedView | uploading = False, error = Just errorMsg }
            in
            ( { model | uploadView = viewWithError, uploadState = UploadFailed errorMsg }
            , Effect.none
            )


view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Upload Database"
    , body =
        Html.map UploadViewMsg (UploadView.view UploadView.databaseConfig model.uploadView)
    }



-- HTTP


uploadDatabase : Json.Encode.Value -> Cmd Msg
uploadDatabase body =
    Http.post
        { url = "/api/v1/db/upload"
        , body = Http.jsonBody body
        , expect = Http.expectJson UploadResult uploadResponseDecoder
        }

module Views.MethodsView exposing (Msg(..), viewMethodsPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Database exposing (DatabaseLoadStatus(..))
import Models.Method exposing (MethodCollectionList, MethodCollectionStatus)
import Set exposing (Set)
import Views.ListActions as ListActions


type Msg
    = LoadMethod String
    | UnloadMethod String
    | ConfirmDeleteMethod String
    | CancelDeleteMethod
    | DeleteMethod String
    | NavigateToMethod String


viewMethodsPage : Maybe MethodCollectionList -> Maybe String -> Maybe String -> Set String -> Maybe String -> Maybe String -> Html Msg
viewMethodsPage maybeMethods error confirmingDelete loadingMethods unloadingMethod deletingMethod =
    div [ class "databases-page" ]
        [ ListActions.viewPageHeader
            { title = "Methods", subtitle = "Manage your LCIA characterization methods", addHref = "/methods/upload", addLabel = "Add method" }
            error
        , case maybeMethods of
            Just methodList ->
                viewMethodsList methodList confirmingDelete loadingMethods unloadingMethod deletingMethod

            Nothing ->
                ListActions.viewLoadingSpinner
        ]


viewMethodsList : MethodCollectionList -> Maybe String -> Set String -> Maybe String -> Maybe String -> Html Msg
viewMethodsList methodList confirmingDelete loadingMethods unloadingMethod deletingMethod =
    let
        methodCount =
            List.length methodList.methods
    in
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt methodCount ++ " method collection(s)") ]
            ]
        , if methodCount == 0 then
            div [ class "notification is-info is-light", style "margin-top" "1rem" ]
                [ text "No method collections configured. Add "
                , code [] [ text "[[methods]]" ]
                , text " entries to your volca.toml, or upload a method package."
                ]

          else
            table [ class "table is-striped is-hoverable is-fullwidth" ]
                [ ListActions.viewTableHeader "Impact categories"
                , tbody []
                    (List.map (viewMethodRow confirmingDelete loadingMethods unloadingMethod deletingMethod) methodList.methods)
                ]
        ]


viewMethodRow : Maybe String -> Set String -> Maybe String -> Maybe String -> MethodCollectionStatus -> Html Msg
viewMethodRow confirmingDelete loadingMethods unloadingMethod deletingMethod mc =
    let
        isLoading =
            Set.member mc.name loadingMethods

        isUnloading =
            unloadingMethod == Just mc.name

        isDeleting =
            deletingMethod == Just mc.name

        actions =
            div [ class "buttons are-small" ]
                (case mc.status of
                    Unloaded ->
                        [ ListActions.viewOpenButton
                            { onOpen = LoadMethod mc.name
                            , isLoading = isLoading
                            }
                        ]
                            ++ (if mc.isUploaded then
                                    [ ListActions.viewDeleteConfirmation
                                        { onConfirm = ConfirmDeleteMethod mc.name
                                        , onDelete = DeleteMethod mc.name
                                        , onCancel = CancelDeleteMethod
                                        , isConfirming = confirmingDelete == Just mc.name
                                        , isDeleting = isDeleting
                                        }
                                    ]

                                else
                                    []
                               )

                    _ ->
                        [ ListActions.viewCloseButton
                            { onClose = UnloadMethod mc.name, isClosing = isUnloading }
                        ]
                )
    in
    ListActions.viewRow
        { status = mc.status
        , actions = actions
        , displayName = mc.displayName
        , description = mc.description |> Maybe.withDefault ""
        , count = mc.methodCount
        , isUploaded = mc.isUploaded
        , isAuto = False
        , format = mc.format |> Maybe.withDefault ""
        , onNavigate =
            case mc.status of
                DbLoaded ->
                    Just (NavigateToMethod mc.name)

                _ ->
                    Nothing
        }

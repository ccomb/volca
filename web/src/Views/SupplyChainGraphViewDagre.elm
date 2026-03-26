module Views.SupplyChainGraphViewDagre exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dagre
import Dagre.Attributes as DA
import Dict exposing (Dict)
import Graph exposing (Graph, Node, Edge)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Models.SupplyChain exposing (SupplyChainEdge, SupplyChainEntry, SupplyChainResponse)
import Svg exposing (Svg, defs, g, line, marker, polygon, rect, svg, text_)
import Svg.Attributes as SvgA
import Svg.Events
import Utils.Format as Format


-- MODEL


type alias Model =
    { response : SupplyChainResponse
    , layout : Dagre.GraphLayout
    , graph : Graph NodeLabel ()
    , idMap : Dict String Int
    , reverseIdMap : Dict Int String
    , hoveredNode : Maybe Int
    , viewBoxX : Float
    , viewBoxY : Float
    , viewBoxWidth : Float
    , viewBoxHeight : Float
    , isPanning : Maybe ( Float, Float )
    }


type alias NodeLabel =
    { name : String
    , location : String
    , quantity : Float
    , unit : String
    , isRoot : Bool
    }


type Msg
    = NodeHover (Maybe Int)
    | ZoomIn
    | ZoomOut
    | AutoFit
    | StartPan ( Float, Float )
    | MovePan ( Float, Float )
    | EndPan
    | WheelZoom Float


svgWidth : Float
svgWidth =
    1400


svgHeight : Float
svgHeight =
    900


nodeBoxWidth : Float
nodeBoxWidth =
    180


nodeBoxHeight : Float
nodeBoxHeight =
    54



-- INIT


init : SupplyChainResponse -> Model
init response =
    let
        edges =
            response.edges

        -- Assign integer IDs: root gets 0, supply chain entries get 1..n
        rootId =
            0

        entryIds =
            response.supplyChain
                |> List.indexedMap (\i entry -> ( entry.processId, i + 1 ))

        idMap =
            ( response.root.id, rootId ) :: entryIds |> Dict.fromList

        reverseIdMap =
            Dict.toList idMap
                |> List.map (\( k, v ) -> ( v, k ))
                |> Dict.fromList

        -- Build graph nodes
        rootNode =
            Graph.Node rootId
                { name = response.root.name
                , location = response.root.location
                , quantity = response.root.productAmount
                , unit = response.root.productUnit
                , isRoot = True
                }

        entryNodes =
            response.supplyChain
                |> List.filterMap
                    (\entry ->
                        Dict.get entry.processId idMap
                            |> Maybe.map
                                (\nid ->
                                    Graph.Node nid
                                        { name = entry.name
                                        , location = entry.location
                                        , quantity = entry.quantity
                                        , unit = entry.unit
                                        , isRoot = False
                                        }
                                )
                    )

        allNodes =
            rootNode :: entryNodes

        -- Build graph edges
        graphEdges =
            edges
                |> List.filterMap
                    (\edge ->
                        Maybe.map2
                            (\fromId toId ->
                                Graph.Edge toId fromId ()
                            )
                            (Dict.get edge.from idMap)
                            (Dict.get edge.to idMap)
                    )

        graph =
            Graph.fromNodesAndEdges allNodes graphEdges

        -- Compute layout with dagre
        nodeCount =
            List.length allNodes

        widthDict =
            allNodes
                |> List.map (\n -> ( n.id, nodeBoxWidth ))
                |> Dict.fromList

        heightDict =
            allNodes
                |> List.map (\n -> ( n.id, nodeBoxHeight ))
                |> Dict.fromList

        dagreAttrs =
            [ DA.rankDir DA.TB
            , DA.widthDict widthDict
            , DA.heightDict heightDict
            , DA.nodeSep 40
            , DA.rankSep 80
            , DA.marginX 30
            , DA.marginY 30
            ]

        layout =
            Dagre.runLayout dagreAttrs graph

        -- Set initial viewBox to fit the layout
        fitBox =
            fitToLayout layout
    in
    { response = response
    , layout = layout
    , graph = graph
    , idMap = idMap
    , reverseIdMap = reverseIdMap
    , hoveredNode = Nothing
    , viewBoxX = fitBox.x
    , viewBoxY = fitBox.y
    , viewBoxWidth = fitBox.width
    , viewBoxHeight = fitBox.height
    , isPanning = Nothing
    }


fitToLayout : Dagre.GraphLayout -> { x : Float, y : Float, width : Float, height : Float }
fitToLayout layout =
    let
        margin =
            50

        rawW =
            layout.width + 2 * margin

        rawH =
            layout.height + 2 * margin

        -- Ensure viewBox isn't flatter than ~16:10
        minH =
            rawW * 0.625

        h =
            max rawH minH

        yOffset =
            (h - rawH) / 2
    in
    { x = -margin
    , y = -margin - yOffset
    , width = rawW
    , height = h
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NodeHover maybeId ->
            { model | hoveredNode = maybeId }

        ZoomIn ->
            zoomBy 0.8 model

        ZoomOut ->
            zoomBy 1.25 model

        AutoFit ->
            let
                fitBox =
                    fitToLayout model.layout
            in
            { model
                | viewBoxX = fitBox.x
                , viewBoxY = fitBox.y
                , viewBoxWidth = fitBox.width
                , viewBoxHeight = fitBox.height
            }

        StartPan mousePos ->
            { model | isPanning = Just mousePos }

        MovePan mousePos ->
            case model.isPanning of
                Just startPos ->
                    let
                        dx =
                            (Tuple.first startPos - Tuple.first mousePos) * (model.viewBoxWidth / svgWidth)

                        dy =
                            (Tuple.second startPos - Tuple.second mousePos) * (model.viewBoxHeight / svgHeight)
                    in
                    { model
                        | viewBoxX = model.viewBoxX + dx
                        , viewBoxY = model.viewBoxY + dy
                        , isPanning = Just mousePos
                    }

                Nothing ->
                    model

        EndPan ->
            { model | isPanning = Nothing }

        WheelZoom delta ->
            let
                factor =
                    if delta < 0 then
                        0.9

                    else
                        1.1
            in
            zoomBy factor model


zoomBy : Float -> Model -> Model
zoomBy factor model =
    let
        newWidth =
            model.viewBoxWidth * factor

        newHeight =
            model.viewBoxHeight * factor

        cx =
            model.viewBoxX + model.viewBoxWidth / 2

        cy =
            model.viewBoxY + model.viewBoxHeight / 2
    in
    { model
        | viewBoxX = cx - newWidth / 2
        , viewBoxY = cy - newHeight / 2
        , viewBoxWidth = newWidth
        , viewBoxHeight = newHeight
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.isPanning of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map MovePan decodeMousePosition)
                , Browser.Events.onMouseUp (Decode.succeed EndPan)
                ]

        Nothing ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "height" "calc(100vh - 142px)"
        , Html.Attributes.style "overflow" "hidden"
        , Html.Events.custom "wheel"
            (Decode.map
                (\delta ->
                    { message = WheelZoom delta
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
                (Decode.field "deltaY" Decode.float)
            )
        ]
        [ svg
            [ SvgA.width "100%"
            , SvgA.height "100%"
            , SvgA.viewBox
                (String.fromFloat model.viewBoxX
                    ++ " "
                    ++ String.fromFloat model.viewBoxY
                    ++ " "
                    ++ String.fromFloat model.viewBoxWidth
                    ++ " "
                    ++ String.fromFloat model.viewBoxHeight
                )
            , SvgA.style "border: 1px solid #ccc; background-color: white; display: block; cursor: grab;"
            , Svg.Events.custom "mousedown"
                (Decode.map
                    (\mousePos ->
                        { message = StartPan mousePos
                        , stopPropagation = False
                        , preventDefault = True
                        }
                    )
                    decodeMousePosition
                )
            ]
            [ defs []
                [ marker
                    [ SvgA.id "arrowhead-dagre"
                    , SvgA.markerWidth "10"
                    , SvgA.markerHeight "7"
                    , SvgA.refX "10"
                    , SvgA.refY "3.5"
                    , SvgA.orient "auto"
                    ]
                    [ polygon
                        [ SvgA.points "0 0, 10 3.5, 0 7"
                        , SvgA.fill "#999"
                        ]
                        []
                    ]
                ]
            , drawEdges model
            , drawNodes model
            ]
        , viewTooltip model
        , viewZoomControls model
        ]


drawEdges : Model -> Svg Msg
drawEdges model =
    let
        edges =
            model.response.edges

        maxAmount =
            edges
                |> List.map .amount
                |> List.maximum
                |> Maybe.withDefault 1

        drawEdge edge =
            case ( Dict.get edge.from model.idMap, Dict.get edge.to model.idMap ) of
                ( Just fromId, Just toId ) ->
                    case ( Dict.get fromId model.layout.coordDict, Dict.get toId model.layout.coordDict ) of
                        ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                            let
                                -- Edge stroke width proportional to amount
                                strokeW =
                                    1 + (edge.amount / maxAmount) * 4
                            in
                            line
                                [ SvgA.x1 (String.fromFloat x1)
                                , SvgA.y1 (String.fromFloat (y1 + nodeBoxHeight / 2))
                                , SvgA.x2 (String.fromFloat x2)
                                , SvgA.y2 (String.fromFloat (y2 - nodeBoxHeight / 2))
                                , SvgA.stroke "#999"
                                , SvgA.strokeWidth (String.fromFloat strokeW)
                                , SvgA.markerEnd "url(#arrowhead-dagre)"
                                , SvgA.style "pointer-events: none;"
                                ]
                                []

                        _ ->
                            g [] []

                _ ->
                    g [] []
    in
    g [] (List.map drawEdge edges)


drawNodes : Model -> Svg Msg
drawNodes model =
    let
        allNodes =
            Graph.nodes model.graph

        drawNode : Node NodeLabel -> Svg Msg
        drawNode node =
            case Dict.get node.id model.layout.coordDict of
                Just ( cx, cy ) ->
                    let
                        label =
                            node.label

                        isHovered =
                            model.hoveredNode == Just node.id

                        borderColor =
                            if label.isRoot then
                                "#48c774"

                            else if isHovered then
                                "#ff9800"

                            else
                                "#3273dc"

                        bgColor =
                            if label.isRoot then
                                "#f0fff4"

                            else if isHovered then
                                "#fff3e0"

                            else
                                "#f5f8ff"

                        rectX =
                            cx - nodeBoxWidth / 2

                        rectY =
                            cy - nodeBoxHeight / 2

                        truncatedName =
                            truncate 22 label.name
                    in
                    g
                        [ Svg.Events.onMouseOver (NodeHover (Just node.id))
                        , Svg.Events.onMouseOut (NodeHover Nothing)
                        , SvgA.style "cursor: pointer;"
                        ]
                        [ rect
                            [ SvgA.x (String.fromFloat rectX)
                            , SvgA.y (String.fromFloat rectY)
                            , SvgA.width (String.fromFloat nodeBoxWidth)
                            , SvgA.height (String.fromFloat nodeBoxHeight)
                            , SvgA.fill bgColor
                            , SvgA.stroke borderColor
                            , SvgA.strokeWidth
                                (if label.isRoot then
                                    "3"

                                 else
                                    "1.5"
                                )
                            , SvgA.rx "4"
                            ]
                            []
                        , text_
                            [ SvgA.x (String.fromFloat cx)
                            , SvgA.y (String.fromFloat (cy - 10))
                            , SvgA.textAnchor "middle"
                            , SvgA.dominantBaseline "middle"
                            , SvgA.fontSize "11"
                            , SvgA.style "font-weight: bold;"
                            , SvgA.fill "#333"
                            , SvgA.style "pointer-events: none;"
                            ]
                            [ Svg.text truncatedName ]
                        , text_
                            [ SvgA.x (String.fromFloat cx)
                            , SvgA.y (String.fromFloat (cy + 4))
                            , SvgA.textAnchor "middle"
                            , SvgA.dominantBaseline "middle"
                            , SvgA.fontSize "9"
                            , SvgA.fill "#666"
                            , SvgA.style "pointer-events: none;"
                            ]
                            [ Svg.text label.location ]
                        , text_
                            [ SvgA.x (String.fromFloat cx)
                            , SvgA.y (String.fromFloat (cy + 17))
                            , SvgA.textAnchor "middle"
                            , SvgA.dominantBaseline "middle"
                            , SvgA.fontSize "9"
                            , SvgA.fill "#888"
                            , SvgA.style "pointer-events: none;"
                            ]
                            [ Svg.text (Format.formatScientific label.quantity ++ " " ++ label.unit) ]
                        ]

                Nothing ->
                    g [] []
    in
    g [] (List.map drawNode allNodes)


viewTooltip : Model -> Html Msg
viewTooltip model =
    case model.hoveredNode of
        Nothing ->
            text ""

        Just nodeId ->
            case
                Graph.nodes model.graph
                    |> List.filter (\n -> n.id == nodeId)
                    |> List.head
            of
                Nothing ->
                    text ""

                Just node ->
                    let
                        label =
                            node.label

                        processId =
                            Dict.get nodeId model.reverseIdMap
                                |> Maybe.withDefault ""
                    in
                    div
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "top" "10px"
                        , Html.Attributes.style "right" "10px"
                        , Html.Attributes.style "background" "white"
                        , Html.Attributes.style "padding" "10px"
                        , Html.Attributes.style "border" "1px solid #ccc"
                        , Html.Attributes.style "border-radius" "4px"
                        , Html.Attributes.style "box-shadow" "0 2px 8px rgba(0,0,0,0.1)"
                        , Html.Attributes.style "max-width" "300px"
                        , Html.Attributes.style "z-index" "10"
                        ]
                        [ Html.h4 [ Html.Attributes.style "margin" "0 0 0.5rem 0" ]
                            [ text label.name ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ Html.strong [] [ text "Location: " ]
                            , text label.location
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ Html.strong [] [ text "Quantity: " ]
                            , text (Format.formatScientific label.quantity ++ " " ++ label.unit)
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0" ]
                            [ Html.strong [] [ text "Process ID: " ]
                            , text processId
                            ]
                        ]


viewZoomControls : Model -> Html Msg
viewZoomControls model =
    div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "bottom" "20px"
        , Html.Attributes.style "right" "20px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "background" "white"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "box-shadow" "0 2px 8px rgba(0,0,0,0.15)"
        ]
        [ Html.button
            [ Html.Attributes.class "button is-small"
            , Html.Events.onClick ZoomIn
            , Html.Attributes.title "Zoom In"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "+" ]
        , Html.button
            [ Html.Attributes.class "button is-small"
            , Html.Events.onClick ZoomOut
            , Html.Attributes.title "Zoom Out"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "\u{2212}" ]
        , Html.button
            [ Html.Attributes.class "button is-small"
            , Html.Events.onClick AutoFit
            , Html.Attributes.title "Fit to View"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "\u{22A1}" ]
        , div
            [ Html.Attributes.style "font-size" "10px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "color" "#666"
            ]
            [ Html.text (String.fromInt (round ((svgWidth / model.viewBoxWidth) * 100)) ++ "%") ]
        ]



-- HELPERS


truncate : Int -> String -> String
truncate maxLen str =
    if String.length str <= maxLen then
        str

    else
        String.left (maxLen - 3) str ++ "..."


decodeMousePosition : Decode.Decoder ( Float, Float )
decodeMousePosition =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)

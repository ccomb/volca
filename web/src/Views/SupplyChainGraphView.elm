module Views.SupplyChainGraphView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Models.SupplyChain exposing (SupplyChainEdge, SupplyChainEntry, SupplyChainResponse)
import Svg exposing (Svg, defs, g, line, marker, path, polygon, rect, svg, text_)
import Svg.Attributes as SA
import Svg.Events
import Utils.Format as Format


-- MODEL


type alias Model =
    { nodes : List LayoutNode
    , edges : List LayoutEdge
    , rootId : String
    , viewBox : ViewBox
    , isPanning : Maybe ( Float, Float )
    , hoveredNode : Maybe String
    }


type alias ViewBox =
    { x : Float, y : Float, w : Float, h : Float }


type alias LayoutNode =
    { id : String
    , label : String
    , location : String
    , quantity : Float
    , unit : String
    , x : Float
    , y : Float
    , layer : Int
    }


type alias LayoutEdge =
    { from : String
    , to : String
    , amount : Float
    }



-- LAYOUT CONSTANTS


nodeWidth : Float
nodeWidth =
    220


nodeHeight : Float
nodeHeight =
    50


layerSpacing : Float
layerSpacing =
    120


nodeSpacing : Float
nodeSpacing =
    40


svgW : Float
svgW =
    1400


svgH : Float
svgH =
    900



-- INIT


init : SupplyChainResponse -> Model
init response =
    let
        -- Build adjacency: edge.to -> [edge.from] (suppliers feed into consumers)
        -- Root is at layer 0, its suppliers at layer 1, etc.
        adjacency =
            response.edges
                |> List.foldl
                    (\e acc ->
                        Dict.update e.to
                            (\maybe ->
                                Just (e.from :: Maybe.withDefault [] maybe)
                            )
                            acc
                    )
                    Dict.empty

        rootId =
            response.root.processId

        -- BFS to assign layers (root = 0, suppliers = deeper)
        layers =
            bfsLayers rootId adjacency

        -- Group nodes by layer
        allEntries =
            { processId = rootId
            , name = response.root.name
            , location = response.root.location
            , quantity = response.root.productAmount
            , unit = response.root.productUnit
            , scalingFactor = 1.0
            }
                :: response.supplyChain

        entryDict =
            allEntries
                |> List.map (\e -> ( e.processId, e ))
                |> Dict.fromList

        -- Barycenter ordering within layers
        layerGroups =
            groupByLayer layers

        orderedGroups =
            barycenterSort layerGroups response.edges

        -- Position nodes
        nodes =
            orderedGroups
                |> List.concatMap
                    (\( layer, ids ) ->
                        let
                            count =
                                List.length ids

                            totalWidth =
                                toFloat count * nodeWidth + toFloat (count - 1) * nodeSpacing

                            startX =
                                svgW / 2 - totalWidth / 2
                        in
                        List.indexedMap
                            (\i pid ->
                                let
                                    entry =
                                        Dict.get pid entryDict
                                in
                                { id = pid
                                , label = entry |> Maybe.map .name |> Maybe.withDefault pid
                                , location = entry |> Maybe.map .location |> Maybe.withDefault ""
                                , quantity = entry |> Maybe.map .quantity |> Maybe.withDefault 0
                                , unit = entry |> Maybe.map .unit |> Maybe.withDefault ""
                                , x = startX + toFloat i * (nodeWidth + nodeSpacing) + nodeWidth / 2
                                , y = 60 + toFloat layer * (nodeHeight + layerSpacing) + nodeHeight / 2
                                , layer = layer
                                }
                            )
                            ids
                    )

        edges =
            response.edges
                |> List.map (\e -> { from = e.from, to = e.to, amount = e.amount })

        -- Auto-fit
        fitBox =
            autoFit nodes
    in
    { nodes = nodes
    , edges = edges
    , rootId = rootId
    , viewBox = fitBox
    , isPanning = Nothing
    , hoveredNode = Nothing
    }



-- BFS LAYER ASSIGNMENT


bfsLayers : String -> Dict String (List String) -> Dict String Int
bfsLayers root adjacency =
    bfsStep [ root ] (Dict.singleton root 0) adjacency


bfsStep : List String -> Dict String Int -> Dict String (List String) -> Dict String Int
bfsStep queue visited adjacency =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                currentLayer =
                    Dict.get current visited |> Maybe.withDefault 0

                neighbors =
                    Dict.get current adjacency |> Maybe.withDefault []

                newNeighbors =
                    List.filter (\n -> not (Dict.member n visited)) neighbors

                newVisited =
                    List.foldl (\n acc -> Dict.insert n (currentLayer + 1) acc) visited newNeighbors
            in
            bfsStep (rest ++ newNeighbors) newVisited adjacency



-- GROUP BY LAYER


groupByLayer : Dict String Int -> List ( Int, List String )
groupByLayer layers =
    layers
        |> Dict.toList
        |> List.foldl
            (\( pid, layer ) acc ->
                Dict.update layer
                    (\maybe -> Just (pid :: Maybe.withDefault [] maybe))
                    acc
            )
            Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.first



-- BARYCENTER SORTING (one pass)


barycenterSort : List ( Int, List String ) -> List SupplyChainEdge -> List ( Int, List String )
barycenterSort layerGroups edges =
    let
        -- Build position index from previous layer
        sortLayer prevPositions ( layer, ids ) =
            let
                posDict =
                    prevPositions

                barycenter pid =
                    let
                        -- Find connected nodes in the previous layer
                        connectedPositions =
                            edges
                                |> List.filterMap
                                    (\e ->
                                        if e.from == pid then
                                            Dict.get e.to posDict

                                        else if e.to == pid then
                                            Dict.get e.from posDict

                                        else
                                            Nothing
                                    )
                    in
                    case connectedPositions of
                        [] ->
                            999999

                        positions ->
                            List.sum positions / toFloat (List.length positions)

                sorted =
                    ids |> List.sortBy barycenter

                newPositions =
                    sorted
                        |> List.indexedMap (\i pid_ -> ( pid_, toFloat i ))
                        |> Dict.fromList
            in
            ( ( layer, sorted ), Dict.union newPositions posDict )
    in
    layerGroups
        |> List.foldl
            (\group ( acc, posDict ) ->
                let
                    ( sorted, newPosDict ) =
                        sortLayer posDict group
                in
                ( acc ++ [ sorted ], newPosDict )
            )
            ( [], Dict.empty )
        |> Tuple.first



-- AUTO-FIT


autoFit : List LayoutNode -> ViewBox
autoFit nodes =
    let
        margin =
            80

        xs =
            List.map .x nodes

        ys =
            List.map .y nodes

        minX =
            (List.minimum xs |> Maybe.withDefault 0) - nodeWidth / 2 - margin

        maxX =
            (List.maximum xs |> Maybe.withDefault svgW) + nodeWidth / 2 + margin

        minY =
            (List.minimum ys |> Maybe.withDefault 0) - nodeHeight / 2 - margin

        maxY =
            (List.maximum ys |> Maybe.withDefault svgH) + nodeHeight / 2 + margin
        rawW =
            maxX - minX

        rawH =
            maxY - minY

        -- Ensure viewBox isn't flatter than ~16:10
        minH =
            rawW * 0.625

        h =
            max rawH minH

        yOffset =
            (h - rawH) / 2
    in
    { x = minX, y = minY - yOffset, w = rawW, h = h }



-- MSG


type Msg
    = ZoomIn
    | ZoomOut
    | AutoFitView
    | StartPan ( Float, Float )
    | MovePan ( Float, Float )
    | EndPan
    | WheelZoom Float
    | NodeHover (Maybe String)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ZoomIn ->
            zoomBy 0.8 model

        ZoomOut ->
            zoomBy 1.25 model

        AutoFitView ->
            { model | viewBox = autoFit model.nodes }

        StartPan pos ->
            { model | isPanning = Just pos }

        MovePan pos ->
            case model.isPanning of
                Just ( sx, sy ) ->
                    let
                        vb =
                            model.viewBox

                        dx =
                            (sx - Tuple.first pos) * (vb.w / svgW)

                        dy =
                            (sy - Tuple.second pos) * (vb.h / svgH)
                    in
                    { model
                        | viewBox = { vb | x = vb.x + dx, y = vb.y + dy }
                        , isPanning = Just pos
                    }

                Nothing ->
                    model

        EndPan ->
            { model | isPanning = Nothing }

        WheelZoom delta ->
            zoomBy
                (if delta < 0 then
                    0.9

                 else
                    1.1
                )
                model

        NodeHover id ->
            { model | hoveredNode = id }


zoomBy : Float -> Model -> Model
zoomBy factor model =
    let
        vb =
            model.viewBox

        cx =
            vb.x + vb.w / 2

        cy =
            vb.y + vb.h / 2

        nw =
            vb.w * factor

        nh =
            vb.h * factor
    in
    { model | viewBox = { x = cx - nw / 2, y = cy - nh / 2, w = nw, h = nh } }



-- VIEW


view : Model -> Html Msg
view model =
    let
        vb =
            model.viewBox
    in
    div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "height" "calc(100vh - 142px)"
        , Html.Attributes.style "overflow" "hidden"
        , Html.Events.custom "wheel"
            (Decode.map
                (\d -> { message = WheelZoom d, stopPropagation = True, preventDefault = True })
                (Decode.field "deltaY" Decode.float)
            )
        ]
        [ svg
            [ SA.width "100%"
            , SA.height "100%"
            , SA.viewBox
                (String.join " "
                    [ String.fromFloat vb.x
                    , String.fromFloat vb.y
                    , String.fromFloat vb.w
                    , String.fromFloat vb.h
                    ]
                )
            , SA.style "border: 1px solid #ccc; background: white; cursor: grab;"
            , Svg.Events.custom "mousedown"
                (Decode.map
                    (\pos -> { message = StartPan pos, stopPropagation = False, preventDefault = True })
                    decodeMousePos
                )
            ]
            [ defs [] [ arrowMarker ]
            , drawEdges model
            , drawNodes model
            ]
        , viewTooltip model
        , viewControls model
        ]


arrowMarker : Svg msg
arrowMarker =
    marker
        [ SA.id "arrow"
        , SA.markerWidth "8"
        , SA.markerHeight "6"
        , SA.refX "8"
        , SA.refY "3"
        , SA.orient "auto"
        ]
        [ polygon [ SA.points "0 0, 8 3, 0 6", SA.fill "#888" ] [] ]


drawEdges : Model -> Svg Msg
drawEdges model =
    let
        nodePos =
            model.nodes
                |> List.map (\n -> ( n.id, ( n.x, n.y ) ))
                |> Dict.fromList

        maxAmount =
            model.edges
                |> List.map (.amount >> abs)
                |> List.maximum
                |> Maybe.withDefault 1

        drawEdge edge =
            case ( Dict.get edge.from nodePos, Dict.get edge.to nodePos ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    let
                        -- Edge goes from supplier (below) to consumer (above)
                        -- Start from top of supplier node, end at bottom of consumer node
                        sy =
                            y1 - nodeHeight / 2

                        ey =
                            y2 + nodeHeight / 2

                        -- Quadratic bezier with control point at midY
                        midY =
                            (sy + ey) / 2

                        thickness =
                            1 + (abs edge.amount / maxAmount) * 8
                    in
                    path
                        [ SA.d
                            ("M " ++ String.fromFloat x1 ++ " " ++ String.fromFloat sy
                                ++ " Q " ++ String.fromFloat x1 ++ " " ++ String.fromFloat midY
                                ++ " " ++ String.fromFloat ((x1 + x2) / 2) ++ " " ++ String.fromFloat midY
                                ++ " Q " ++ String.fromFloat x2 ++ " " ++ String.fromFloat midY
                                ++ " " ++ String.fromFloat x2 ++ " " ++ String.fromFloat ey
                            )
                        , SA.fill "none"
                        , SA.stroke "#c0c0c0"
                        , SA.strokeWidth (String.fromFloat thickness)
                        , SA.markerEnd "url(#arrow)"
                        , SA.style "pointer-events: none;"
                        ]
                        []

                _ ->
                    g [] []
    in
    g [] (List.map drawEdge model.edges)


drawNodes : Model -> Svg Msg
drawNodes model =
    g [] (List.map (drawNode model) model.nodes)


drawNode : Model -> LayoutNode -> Svg Msg
drawNode model node =
    let
        isRoot =
            node.id == model.rootId

        isHovered =
            model.hoveredNode == Just node.id

        fillColor =
            if isRoot then
                "#e8f5e9"

            else if isHovered then
                "#fff3e0"

            else
                "#fafafa"

        borderColor =
            if isRoot then
                "#2e7d32"

            else if isHovered then
                "#ef6c00"

            else
                "#9e9e9e"

        rx =
            node.x - nodeWidth / 2

        ry =
            node.y - nodeHeight / 2

        truncLabel =
            if String.length node.label > 28 then
                String.left 25 node.label ++ "..."

            else
                node.label
    in
    g
        [ Svg.Events.onMouseOver (NodeHover (Just node.id))
        , Svg.Events.onMouseOut (NodeHover Nothing)
        , SA.style "cursor: pointer;"
        ]
        [ rect
            [ SA.x (String.fromFloat rx)
            , SA.y (String.fromFloat ry)
            , SA.width (String.fromFloat nodeWidth)
            , SA.height (String.fromFloat nodeHeight)
            , SA.fill fillColor
            , SA.stroke borderColor
            , SA.strokeWidth
                (if isRoot then
                    "2.5"

                 else
                    "1.5"
                )
            , SA.rx "4"
            ]
            []
        , text_
            [ SA.x (String.fromFloat node.x)
            , SA.y (String.fromFloat (node.y - 8))
            , SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            , SA.fontSize "11"
            , SA.fill "#333"
            , SA.style "pointer-events: none;"
            ]
            [ Svg.text truncLabel ]
        , text_
            [ SA.x (String.fromFloat node.x)
            , SA.y (String.fromFloat (node.y + 10))
            , SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            , SA.fontSize "9"
            , SA.fill "#777"
            , SA.style "pointer-events: none;"
            ]
            [ Svg.text (node.location ++ " | " ++ Format.formatScientific node.quantity ++ " " ++ node.unit) ]
        ]


viewTooltip : Model -> Html Msg
viewTooltip model =
    case model.hoveredNode of
        Nothing ->
            text ""

        Just id ->
            case List.filter (\n -> n.id == id) model.nodes |> List.head of
                Nothing ->
                    text ""

                Just node ->
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
                        [ Html.h4 [ Html.Attributes.style "margin" "0 0 0.5rem 0" ] [ text node.label ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ Html.strong [] [ text "Location: " ], text node.location ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ Html.strong [] [ text "Quantity: " ]
                            , text (Format.formatScientific node.quantity ++ " " ++ node.unit)
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0" ]
                            [ Html.strong [] [ text "Process: " ], text node.id ]
                        ]


viewControls : Model -> Html Msg
viewControls model =
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
            , Html.Events.onClick AutoFitView
            , Html.Attributes.title "Fit to View"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "\u{229E}" ]
        , div
            [ Html.Attributes.style "font-size" "10px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "color" "#666"
            ]
            [ Html.text (String.fromInt (round (svgW / model.viewBox.w * 100)) ++ "%") ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.isPanning of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map MovePan decodeMousePos)
                , Browser.Events.onMouseUp (Decode.succeed EndPan)
                ]

        Nothing ->
            Sub.none



-- HELPERS


decodeMousePos : Decode.Decoder ( Float, Float )
decodeMousePos =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)

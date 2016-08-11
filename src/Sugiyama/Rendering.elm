module Sugiyama.Rendering exposing (..)

import Sugiyama.Domain exposing (..)
import List
import List.Extra as List
import Dict exposing (Dict)


type alias RenderGroup a =
    { min : Float, max : Float, items : List (Node a) }


type alias RenderableGraph a =
    { width : Int
    , height : Int
    , vertices :
        List
            { value : a
            , x : Float
            , y : Float
            }
    , edges :
        List (List ( Float, Float ))
    }


asRenderedGraph : LayeredGraph a -> RenderableGraph a
asRenderedGraph input =
    let
        height =
            List.length input.layers

        nodeLayerIndex =
            input.layers
                |> List.indexedMap (\index layer -> (List.map (\node -> ( node.id, index )) layer))
                |> List.concat
                |> Dict.fromList

        nodeIdToY nodeId =
            nodeLayerIndex
                |> Dict.get nodeId
                |> Maybe.map (\index -> (1.0 / toFloat (height - 1)) * toFloat index)
                |> Maybe.withDefault 0.0

        allNodes =
            List.concat input.layers

        nodeIndex =
            allNodes
                |> List.map (\n -> ( n.id, n ))
                |> Dict.fromList

        dummyGroups =
            List.filter isToDummyEdge input.edges
                |> List.map (asDummyGroup input.edges)
                |> List.sortBy (List.length)

        renderGroups =
            asRenderGroups input

        -- Foldr because we want to start with the longest dummy groups
        ( nodePositions, newRenderGroups ) =
            List.foldr handleDummyGroup ( Dict.empty, renderGroups ) dummyGroups

        positions =
            newRenderGroups
                |> List.concatMap (\renderGroup -> List.indexedMap (\index node -> ( node, positionForIndex renderGroup index )) renderGroup.items)
                |> List.foldl (\( node, offset ) -> Dict.insert node.id ( node, offset )) nodePositions
                |> Dict.toList
                |> List.map snd
                |> List.map (\( node, x ) -> ( node, x, nodeIdToY node.id ))

        nodes =
            positions
                |> List.filterMap
                    (\( node, x, y ) ->
                        case node.value of
                            Val n ->
                                Just { value = n, x = x, y = y }

                            Dummy ->
                                Nothing
                    )

        normalEdges =
            input.edges
                |> List.filter (\( x, y ) -> not (isDummy x) && not (isDummy y))
                |> List.map (\( x, y ) -> [ x, y ])

        dummyEdges =
            input.edges
                |> List.filter isToDummyEdge
                |> List.map (edgePath input.edges)

        edges =
            normalEdges
                ++ dummyEdges
                |> List.map (edgePathToPositionPath positions)
    in
        { width = List.map List.length input.layers |> List.maximum |> Maybe.withDefault 0
        , height = height
        , vertices = nodes
        , edges = edges
        }


edgePathToPositionPath : List ( Node a, Float, Float ) -> List (Node a) -> List ( Float, Float )
edgePathToPositionPath positions input =
    input
        |> List.filterMap
            (\node ->
                List.filter (\( n, _, _ ) -> n == node) positions
                    |> List.head
                    |> Maybe.map (\( _, x, y ) -> ( x, y ))
            )


handleDummyGroup : List (Node a) -> ( Dict String ( Node a, Float ), List (RenderGroup a) ) -> ( Dict String ( Node a, Float ), List (RenderGroup a) )
handleDummyGroup dummyGroup ( answer, renderGroups ) =
    let
        linked =
            linkNodesToRenderGroups dummyGroup renderGroups

        ( leftV, leftItemCount ) =
            getLeftOffsetData renderGroups dummyGroup linked

        ( rightV, rightItemCount ) =
            getRightOffsetData renderGroups dummyGroup linked

        splitAt =
            (rightV + leftV)
                / 2.0

        answer' =
            List.foldl (\node -> Dict.insert node.id ( node, splitAt )) answer dummyGroup

        fixedRenderGroups =
            fixRenderGroups dummyGroup splitAt renderGroups linked
    in
        ( answer', fixedRenderGroups )


fixRenderGroups : List (Node a) -> Float -> List (RenderGroup a) -> List ( Node a, RenderGroup a ) -> List (RenderGroup a)
fixRenderGroups nodes splitAt renderGroups linked =
    let
        targetRenderGroups =
            List.map snd linked

        untouchedGroups =
            List.filter (flip List.member targetRenderGroups >> not) renderGroups

        newTargetRenderGroups =
            List.concatMap (fixRenderGroup splitAt) linked
    in
        untouchedGroups ++ newTargetRenderGroups


fixRenderGroup : Float -> ( Node a, RenderGroup a ) -> List (RenderGroup a)
fixRenderGroup splitAt ( node, renderGroup ) =
    case List.elemIndex node renderGroup.items of
        Just index ->
            if List.length renderGroup.items == 1 then
                []
            else if index == 0 then
                [ { renderGroup | min = splitAt, items = List.drop 1 renderGroup.items } ]
            else if index == List.length renderGroup.items - 1 then
                [ { renderGroup | max = splitAt, items = List.take index renderGroup.items } ]
            else
                [ { renderGroup | max = splitAt, items = List.take index renderGroup.items }
                , { renderGroup | min = splitAt, items = List.drop (index + 1) renderGroup.items }
                ]

        Nothing ->
            Debug.log "WARN - Sugiyame.Rendering - fixRenderGroupForNothingShouldNeverHappen"
                [ renderGroup ]


linkNodesToRenderGroups : List (Node a) -> List (RenderGroup a) -> List ( Node a, RenderGroup a )
linkNodesToRenderGroups nodes renderGroups =
    List.filterMap
        (\x ->
            List.filter (\group -> List.member x group.items) renderGroups
                |> List.head
                |> Maybe.map ((,) x)
        )
        nodes


getRightOffsetData : List (RenderGroup a) -> List (Node a) -> List ( Node a, RenderGroup a ) -> ( Float, Int )
getRightOffsetData renderGroups dummies linked =
    let
        positions =
            List.filterMap
                (\( n, renderGroup ) ->
                    List.elemIndex n renderGroup.items
                        |> Maybe.map (\index -> ( (List.length renderGroup.items - 1) - index, (positionForIndex renderGroup (index)) ))
                )
                linked

        maxLeft =
            positions
                |> List.map fst
                |> List.maximum
                |> Maybe.withDefault 0

        minOffset =
            positions
                |> List.filter (fst >> (==) maxLeft)
                |> List.map snd
                |> List.sort
                |> List.last
                |> Maybe.withDefault 0
    in
        ( minOffset, maxLeft )


getLeftOffsetData : List (RenderGroup a) -> List (Node a) -> List ( Node a, RenderGroup a ) -> ( Float, Int )
getLeftOffsetData renderGroups dummies linked =
    let
        positions =
            List.filterMap
                (\( n, renderGroup ) ->
                    List.elemIndex n renderGroup.items
                        |> Maybe.map (\index -> ( index, (positionForIndex renderGroup index) ))
                )
                linked

        maxLeft =
            positions
                |> List.map fst
                |> List.maximum
                |> Maybe.withDefault 0

        minOffset =
            positions
                |> List.filter (fst >> (==) maxLeft)
                |> List.map snd
                |> List.sort
                |> List.head
                |> Maybe.withDefault 0
    in
        ( minOffset, maxLeft )


positionForIndex : RenderGroup a -> Int -> Float
positionForIndex renderGroup index =
    let
        span =
            (renderGroup.max - renderGroup.min)

        divider =
            toFloat (List.length renderGroup.items + 1)

        parts =
            toFloat (index + 1)
    in
        if List.length renderGroup.items == 1 then
            span / 2.0 + renderGroup.min
        else
            (span / divider * parts) + renderGroup.min


asRenderGroups : LayeredGraph a -> List (RenderGroup a)
asRenderGroups input =
    let
        ( x, y ) =
            baseValues input
    in
        List.map (\n -> { min = x, max = y, items = n }) input.layers


baseValues : LayeredGraph a -> ( Float, Float )
baseValues input =
    let
        longest =
            input.layers
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 0
    in
        if longest <= 1 then
            ( 0.0, 1.0 )
        else
            let
                step =
                    1.0 / (toFloat (longest - 1))
            in
                ( 0.0 - step, 1.0 + step )


edgePath : List (Edge a) -> Edge a -> List (Node a)
edgePath edges ( from, to ) =
    if isDummy to then
        let
            rest =
                edges
                    |> List.filter (\( f, t ) -> f == to)
                    |> List.head
                    |> Maybe.map (edgePath edges)
                    |> Maybe.withDefault [ to ]
        in
            from :: rest
    else
        [ from, to ]


asDummyGroup : List (Edge a) -> Edge a -> List (Node a)
asDummyGroup edges ( _, to ) =
    if isDummy to then
        let
            rest =
                edges
                    |> List.filter (\( f, t ) -> f == to)
                    |> List.head
                    |> Maybe.map (asDummyGroup edges)
                    |> Maybe.withDefault []
        in
            to :: rest
    else
        []


isToDummyEdge : Edge a -> Bool
isToDummyEdge ( from, to ) =
    not (isDummy from) && isDummy to


isDummy : Node a -> Bool
isDummy node =
    node.value == Dummy

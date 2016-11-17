module Sugiyama.Dummies exposing (addDummyVertices)

import Sugiyama.Domain exposing (..)
import List.Extra as List
import Dict exposing (Dict)
import String


addDummyVertices : LayeredGraph a -> LayeredGraph a
addDummyVertices input =
    let
        indexedLayers =
            indexLayers input.layers

        expandedEdges =
            List.map (handleEdge indexedLayers) input.edges

        newLayerVertices =
            List.concatMap Tuple.first expandedEdges

        newEdges =
            List.concatMap Tuple.second expandedEdges

        newLayers =
            List.foldl addLayerVertice input.layers newLayerVertices
    in
        { layers = newLayers
        , edges = newEdges
        , mapping = input.mapping
        }


handleEdge : Dict String Int -> ( Node, Node ) -> ( List LayerNode, List Edge )
handleEdge nodeLayerIndex ( from, to ) =
    Maybe.map2 (\fIndex tIndex -> expandOverLayers ( fIndex, from ) ( tIndex, to ))
        (Dict.get from nodeLayerIndex)
        (Dict.get to nodeLayerIndex)
        |> Maybe.withDefault ( [], [] )


expandOverLayers : LayerNode -> LayerNode -> ( List LayerNode, List Edge )
expandOverLayers (( fIndex, from ) as fromNode) (( tIndex, to ) as toNode) =
    if fIndex >= tIndex - 1 then
        ( [ fromNode, toNode ], [ ( from, to ) ] )
    else
        let
            nextIndex =
                fIndex + 1

            nextId =
                case String.toInt from of
                    Ok _ ->
                        from ++ "_" ++ to ++ "_x"

                    Err _ ->
                        from ++ "x"

            next =
                ( nextIndex
                , nextId
                )

            ( restNodes, restEdges ) =
                expandOverLayers next toNode
        in
            ( fromNode :: restNodes, ( from, Tuple.second next ) :: restEdges )


addVertexToLayer : Node -> List Node -> List Node
addVertexToLayer node list =
    if List.isEmpty (List.filter ((==) node) list) then
        node :: list
    else
        list


addLayerVertice : LayerNode -> List (List Node) -> List (List Node)
addLayerVertice ( index, node ) layers =
    List.updateAt index
        (addVertexToLayer node)
        layers
        |> Maybe.withDefault layers


indexLayer : Int -> List Node -> List ( Node, Int )
indexLayer index layer =
    List.map (flip (,) index) layer


indexLayers : List (List Node) -> Dict Node Int
indexLayers layers =
    List.indexedMap indexLayer layers
        |> List.concat
        |> Dict.fromList

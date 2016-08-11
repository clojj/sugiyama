module Sugiyama.Dummies exposing (addDummyVertices)

import Sugiyama.Domain exposing (..)
import List.Extra as List
import Dict exposing (Dict)


addDummyVertices : LayeredGraph a -> LayeredGraph a
addDummyVertices input =
    let
        indexedLayers =
            indexLayers .id input.layers

        expandedEdges =
            List.map (handleEdge indexedLayers) input.edges

        newLayerVertices =
            List.concatMap fst expandedEdges

        newEdges =
            List.concatMap snd expandedEdges

        newLayers =
            List.foldl addLayerVertice input.layers newLayerVertices
    in
        { layers = newLayers
        , edges = newEdges
        }


handleEdge : Dict String Int -> ( Node a, Node a ) -> ( List (LayerNode a), List (Edge a) )
handleEdge nodeLayerIndex ( from, to ) =
    Maybe.map2 (\fIndex tIndex -> expandOverLayers ( fIndex, from ) ( tIndex, to ))
        (Dict.get from.id nodeLayerIndex)
        (Dict.get to.id nodeLayerIndex)
        |> Maybe.withDefault ( [], [] )


expandOverLayers : LayerNode a -> LayerNode a -> ( List (LayerNode a), List (Edge a) )
expandOverLayers (( fIndex, from ) as fromNode) (( tIndex, to ) as toNode) =
    if fIndex >= tIndex - 1 then
        ( [ fromNode, toNode ], [ ( from, to ) ] )
    else
        let
            nextIndex =
                fIndex + 1

            nextId =
                case from.value of
                    Dummy ->
                        from.id ++ "x"

                    Val _ ->
                        from.id ++ "_" ++ to.id ++ "_x"

            next =
                ( nextIndex
                , { id = nextId
                  , value = Dummy
                  }
                )

            ( restNodes, restEdges ) =
                expandOverLayers next toNode
        in
            ( fromNode :: restNodes, ( from, snd next ) :: restEdges )


addVertexToLayer : Node a -> List (Node a) -> List (Node a)
addVertexToLayer node list =
    if List.isEmpty (List.filter (.id >> (==) node.id) list) then
        node :: list
    else
        list


addLayerVertice : LayerNode a -> List (List (Node a)) -> List (List (Node a))
addLayerVertice ( index, node ) layers =
    List.updateAt index
        (addVertexToLayer node)
        layers
        |> Maybe.withDefault layers


indexLayer : (a -> comparable) -> Int -> List a -> List ( comparable, Int )
indexLayer f index layer =
    List.map (\elem -> ( f elem, index )) layer


indexLayers : (a -> comparable) -> List (List a) -> Dict comparable Int
indexLayers f layers =
    List.indexedMap (indexLayer f) layers
        |> List.concat
        |> Dict.fromList

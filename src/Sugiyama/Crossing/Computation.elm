module Sugiyama.Crossing.Computation exposing (..)

import Sugiyama.Domain exposing (Node, Layer, LayeredGraph)
import Sugiyama.Utils exposing (orderedPairs)
import List.Extra as List
import Dict exposing (Dict)


crossingsForLayeredGraph : LayeredGraph a -> Int
crossingsForLayeredGraph input =
    let
        countForTwoLayers x y =
            computeCrossings x y input.edges
    in
        List.map2 countForTwoLayers input.layers (List.drop 1 input.layers)
            |> List.sum


computeCrossingsPairs : Layer -> Layer -> List ( Node, Node ) -> Dict ( String, String ) Int
computeCrossingsPairs inLayer outLayer edges =
    outLayer
        |> List.concatMap (\x -> outLayer `List.andThen` (\y -> [ ( x, y ) ]))
        |> List.filter (uncurry (/=))
        |> List.map (\( x, y ) -> ( ( x, y ), crossingsForItems inLayer edges x y ))
        |> Dict.fromList


computeCrossings : Layer -> Layer -> List ( Node, Node ) -> Int
computeCrossings aNodes bNodes links =
    let
        bPairs =
            orderedPairs bNodes
    in
        bPairs
            |> List.map (\( l, r ) -> crossingsForItems aNodes links l r)
            |> List.sum


crossingsForItems : Layer -> List ( Node, Node ) -> Node -> Node -> Int
crossingsForItems aNodes links left right =
    let
        rightSources =
            getSourceIndexes aNodes links right

        leftSources =
            getSourceIndexes aNodes links left
    in
        leftSources
            |> List.map (\li -> List.takeWhile ((>) li) rightSources |> List.length)
            |> List.sum


getSourceIndexes : Layer -> List ( Node, Node ) -> Node -> List Int
getSourceIndexes aNodes links target =
    links
        |> List.filter (snd >> (==) target)
        |> List.map fst
        |> List.filterMap (flip List.elemIndex aNodes)

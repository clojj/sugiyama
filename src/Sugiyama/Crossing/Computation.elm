module Sugiyama.Crossing.Computation exposing (..)

import Sugiyama.Domain exposing (Node, Layer, LayeredGraph)
import Sugiyama.Utils exposing (orderedPairs)
import Sugiyama.Cache as Cache exposing (Cache)
import List.Extra as List
import Dict exposing (Dict)


crossingsForLayeredGraph : ( LayeredGraph a, Cache a ) -> ( Int, Cache a )
crossingsForLayeredGraph ( input, cache ) =
    let
        countForTwoLayers ( x, y ) =
            computeCrossings x y input.edges

        foldCount pair ( count, c ) =
            case Cache.hasLayerPairCrossing c pair of
                Just n ->
                    ( count + n, c )

                Nothing ->
                    let
                        result =
                            countForTwoLayers pair

                        cache_ =
                            Cache.cacheLayerPairCrossings c pair result
                    in
                        ( count + result, cache_ )
    in
        List.map2 (,) input.layers (List.drop 1 input.layers)
            |> List.foldl foldCount ( 0, cache )


computeCrossingsPairs : Layer -> Layer -> List ( Node, Node ) -> Dict ( String, String ) Int
computeCrossingsPairs inLayer outLayer edges =
    outLayer
        |> List.concatMap (\x -> outLayer |> List.andThen (\y -> [ ( x, y ) ]))
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
        |> List.filter (Tuple.second >> (==) target)
        |> List.map Tuple.first
        |> List.filterMap (flip List.elemIndex aNodes)

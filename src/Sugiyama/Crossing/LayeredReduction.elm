module Sugiyama.Crossing.LayeredReduction exposing (..)

import List.Extra as List
import List
import Sugiyama.Domain exposing (..)
import Sugiyama.Cache as C exposing (Cache)
import Sugiyama.Utils exposing (orderedPairs)
import Sugiyama.Crossing.Computation as Computation
import Dict exposing (Dict)


optimizeCrossing : (LayeredGraph a, Cache a) -> (LayeredGraph a, Cache a)
optimizeCrossing (input, cache) =
    optimizeCrossing' cache input


optimizeCrossing' : Cache a -> LayeredGraph a -> (LayeredGraph a, Cache a)
optimizeCrossing' cache input =
    let
        before =
            Computation.crossingsForLayeredGraph input

        ( optimized, newCache ) =
            findBestLayers cache input

        after =
            Computation.crossingsForLayeredGraph optimized
    in
        if after == 0 then
            (optimized, newCache)
        else if after < before then
            optimizeCrossing' newCache optimized
        else
            (input, newCache)

findBestLayers : Cache a -> LayeredGraph a -> ( LayeredGraph a, Cache a )
findBestLayers cache input =
    let
        edges =
            input.edges

        invertedEdges =
            edges
                |> List.map (\( x, y ) -> ( y, x ))

        layers =
            input.layers
                |> List.indexedMap (,)

        ( resultLayersToLeft, newCache ) =
            List.foldl (handleLayer edges) ( [], cache ) layers

        ( resultLayersToRight, newCache_ ) =
            List.foldr (handleLayer invertedEdges) ( [], newCache ) resultLayersToLeft

        newLayers =
            resultLayersToRight |> List.map snd |> List.reverse
    in
        ( { input | layers = newLayers }, newCache_ )


handleLayer : List ( Node a, Node a ) -> ( Int, Layer a ) -> ( List ( Int, Layer a ), Cache a ) -> ( List ( Int, Layer a ), Cache a )
handleLayer edges ( layerId, next ) ( result, cache ) =
    case List.last result of
        Nothing ->
            ( [ ( layerId, next ) ], cache )

        Just ( lId, last ) ->
            case (C.loadFromCache last next cache) of
                Just hit ->
                    ( result ++ [ ( layerId, hit ) ]
                    , cache
                    )

                Nothing ->
                    let
                        computedLayer =
                            reduceTo cache ( last, ( layerId, next ), edges )

                        newCache =
                            C.addToCache last next computedLayer cache
                    in
                        ( result ++ [ ( layerId, computedLayer ) ]
                        , newCache
                        )


reduceTo : Cache a -> ( Layer a, ( Int, Layer a ), List ( Node a, Node a ) ) -> Layer a
reduceTo cache ( aNodes, ( layerId, bNodes ), edges ) =
    if Computation.computeCrossings aNodes bNodes edges == 0 then
        bNodes
    else
        cache
            |> C.cachedPermutations
            |> Dict.get layerId
            |> Maybe.map (\permutation -> findOptimalPermutation permutation aNodes bNodes edges)
            |> Maybe.withDefault bNodes


findOptimalPermutation : LayerPermutation a -> Layer a -> Layer a -> List ( Node a, Node a ) -> Layer a
findOptimalPermutation permutations aNodes bNodes edges =
    let
        bNodePairCrossings : Dict ( String, String ) Int
        bNodePairCrossings =
            Computation.computeCrossingsPairs aNodes bNodes edges

        crossingsForPairs : List ( Node a, Node a ) -> Int
        crossingsForPairs a =
            a
                |> List.map (\( s, t ) -> ( s.id, t.id ))
                |> List.filterMap (flip Dict.get bNodePairCrossings)
                |> List.sum

    in
        permutations
            |> List.map (\x -> ( x |> orderedPairs |> crossingsForPairs, x ))
            |> List.sortBy fst
            |> List.head
            |> Maybe.map snd
            |> Maybe.withDefault bNodes

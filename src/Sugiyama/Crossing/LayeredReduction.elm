module Sugiyama.Crossing.LayeredReduction exposing (..)

import List.Extra as List
import List
import Sugiyama.Domain exposing (..)
import Sugiyama.Cache as C exposing (Cache)
import Sugiyama.Utils exposing (orderedPairs)
import Sugiyama.Crossing.Computation as Computation
import Dict exposing (Dict)


type alias LayerPermutationDict a =
    Dict Int (LayerPermutation a)


type alias LayerPermutation a =
    List (Layer a)


optimizeCrossing : LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    optimizeCrossing' C.newCache (layerPermutationsForGraph input) input


optimizeCrossing' : Cache a -> LayerPermutationDict a -> LayeredGraph a -> LayeredGraph a
optimizeCrossing' cache permutations input =
    let
        before =
            Computation.crossingsForLayeredGraph input

        ( optimized, newCache ) =
            findBestLayers cache permutations input

        after =
            Computation.crossingsForLayeredGraph optimized
    in
        if after == 0 then
            optimized
        else if after < before then
            optimizeCrossing' newCache permutations optimized
        else
            input


layerPermutationsForGraph : LayeredGraph a -> LayerPermutationDict a
layerPermutationsForGraph input =
    input
        |> .layers
        |> List.map List.permutations
        |> List.indexedMap (,)
        |> Dict.fromList


findBestLayers : Cache a -> LayerPermutationDict a -> LayeredGraph a -> ( LayeredGraph a, Cache a )
findBestLayers cache permutations input =
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
            List.foldl (handleLayer edges permutations) ( [], cache ) layers

        ( resultLayersToRight, newCache_ ) =
            List.foldr (handleLayer invertedEdges permutations) ( [], newCache ) resultLayersToLeft

        newLayers =
            resultLayersToRight |> List.map snd |> List.reverse
    in
        ( { input | layers = newLayers }, newCache_ )


handleLayer : List ( Node a, Node a ) -> LayerPermutationDict a -> ( Int, Layer a ) -> ( List ( Int, Layer a ), Cache a ) -> ( List ( Int, Layer a ), Cache a )
handleLayer edges permutationsDict ( layerId, next ) ( result, cache ) =
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
                            reduceTo permutationsDict ( last, ( layerId, next ), edges )

                        newCache =
                            C.addToCache last next computedLayer cache
                    in
                        ( result ++ [ ( layerId, computedLayer ) ]
                        , newCache
                        )


reduceTo : LayerPermutationDict a -> ( Layer a, ( Int, Layer a ), List ( Node a, Node a ) ) -> Layer a
reduceTo permutations ( aNodes, ( layerId, bNodes ), edges ) =
    permutations
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

        bNodesCrossing =
            bNodes |> orderedPairs |> crossingsForPairs

    in
        if bNodesCrossing == 0 then
            bNodes
        else
            permutations
                |> List.map (\x -> ( x |> orderedPairs |> crossingsForPairs, x ))
                |> List.sortBy fst
                |> List.head
                |> Maybe.map snd
                |> Maybe.withDefault bNodes

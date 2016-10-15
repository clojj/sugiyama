module Sugiyama.CrossingReduction exposing (..)

import List.Extra as List
import List
import Sugiyama.Domain exposing (..)
import Sugiyama.Cache as C exposing (Cache)
import Sugiyama.Utils exposing (orderedPairs)
import Sugiyama.CrossingComputation as Crossing
import Dict exposing (Dict)


type alias LayerPermutationDict a =
    Dict Int (LayerPermutation a)

type alias LayerPermutation a = List (Layer a)


optimizeCrossing : LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    optimizeCrossing' C.newCache (layerPermutationsForGraph input) input


optimizeCrossing' : Cache a -> LayerPermutationDict a -> LayeredGraph a -> LayeredGraph a
optimizeCrossing' cache permutations input =
    let
        before =
            numberOfCrossings input
                |> Debug.log "Before"

        ( optimized, newCache ) =
            findBestLayers cache permutations input

        after =
            numberOfCrossings optimized
                |> Debug.log "After"
    in
        if Debug.log "Yes?" <| after < before then
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
                    let
                        _ =
                            Debug.log "Hit" "!"
                    in
                        ( result ++ [ ( layerId, hit ) ]
                        , cache
                        )

                Nothing ->
                    let
                        (computedLayer, newCache) =
                            reduceTo newCache permutationsDict ( last, (layerId, next ), edges )

                        newCache_ =
                            C.addToCache last next computedLayer cache

                    in
                        ( result ++ [ ( layerId, computedLayer ) ]
                        , newCache_
                        )


numberOfCrossings : LayeredGraph a -> Int
numberOfCrossings input =
    let
        countForTwoLayers x y =
            Crossing.computeCrossings x y input.edges
    in
        List.map2 countForTwoLayers input.layers (List.drop 1 input.layers)
            |> List.sum



-- Per 2 layers


reduceTo : Cache a -> LayerPermutationDict a -> ( Layer a, ( Int, Layer a ), List ( Node a, Node a ) ) -> (Layer a, Cache a)
reduceTo cache permutations ( aNodes, ( layerId, bNodes ), edges ) =
    let
        permutation =
            Dict.get layerId permutations

        default = (bNodes, cache)
    in
        case permutation of
            Nothing ->
                default

            Just bNodesList ->
                findOptimalPermution2 cache bNodesList aNodes bNodes edges

findOptimalPermution2: Cache a -> LayerPermutation a -> Layer a -> Layer a -> List ( Node a, Node a )  -> (Layer a, Cache a)
findOptimalPermution2 cache permutations aNodes bNodes edges =
    let
        x = Crossing.computeCrossingsPairs aNodes bNodes edges

        x' : Dict (String, String) Int
        x' = Dict.fromList <| List.map (\(a,b,c) -> ((a,b),c)) <| x

        z : List ( Node a,Node a) -> Int
        z a =
            a
            |> List.map (\(s,t) -> (s.id, t.id))
            |> List.filterMap (\k -> Dict.get k x')
            |> List.sum

        y = permutations
            |> List.map (\x -> (x |> orderedPairs |> z, x))
            |> List.sortBy fst
            |> List.head
            |> Maybe.map snd
            |> Maybe.withDefault bNodes
    in
        (y, cache)

module Sugiyama.CrossingReduction exposing (..)

import List.Extra as List
import List
import Sugiyama.Domain exposing (..)


optimizeCrossing : LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    let

        before =
            numberOfCrossings input

        optimized =
            findBestLayers input

        after =
            numberOfCrossings optimized
    in
        if Debug.log "Yes?" <| after < before then
            optimizeCrossing optimized
        else
            input


findBestLayers : LayeredGraph a -> LayeredGraph a
findBestLayers input =
    let
        edges =
            input.edges
        invertedEdges =
            edges
            |> List.map (\(x,y) -> (y,x))

        layers =
            input.layers

        resultLayersToLeft =
            List.foldl (handleLayer edges) [] layers

        resultLayersToRight =
          List.foldr (handleLayer invertedEdges) [] resultLayersToLeft
          |> List.reverse

    in
        {input | layers = resultLayersToRight}


handleLayer : List ( Node a, Node a ) -> List (Node a) -> List (List (Node a)) -> List (List (Node a))
handleLayer edges next result =
    case List.last result of
        Nothing ->
            [ next ]

        Just last ->
            result ++ [ reduceTo ( last, next, edges ) ]


numberOfCrossings : LayeredGraph a -> Int
numberOfCrossings input =
    let
        countForTwoLayers x y =
            computeCrossings x y input.edges
    in
        List.map2 countForTwoLayers input.layers (List.drop 1 input.layers)
            |> List.sum



-- Per 2 layers


reduceTo : ( List a, List b, List ( a, b ) ) -> List b
reduceTo ( aNodes, bNodes, edges ) =
    let

        bNodesList =
            List.permutations bNodes
            |> Debug.log "Reduce to!"
    in
        bNodesList
            |> List.map (\n -> ( computeCrossings aNodes n edges, n ))
            |> List.minimumBy fst
            |> Maybe.map snd
            |> Maybe.withDefault bNodes


computeCrossings : List a -> List b -> List ( a, b ) -> Int
computeCrossings aNodes bNodes links =
    let
        bPairs =
            orderedPairs bNodes
    in
        bPairs
            |> List.map (\( l, r ) -> crossingsForItems aNodes links l r)
            |> List.sum


crossingsForItems : List a -> List ( a, b ) -> b -> b -> Int
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


getSourceIndexes : List a -> List ( a, b ) -> b -> List Int
getSourceIndexes aNodes links target =
    links
        |> List.filter (snd >> (==) target)
        |> List.map fst
        |> List.filterMap (flip List.elemIndex aNodes)


orderedPairs : List b -> List ( b, b )
orderedPairs input =
    case input of
        x :: rest ->
            List.map ((,) x) rest ++ orderedPairs rest

        [] ->
            []

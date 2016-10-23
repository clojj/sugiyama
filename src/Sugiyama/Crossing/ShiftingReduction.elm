module Sugiyama.Crossing.ShiftingReduction exposing (..)

import Sugiyama.Crossing.Computation as Computation
import Sugiyama.Domain exposing (LayeredGraph, Node, Layer)
import Sugiyama.Cache as Cache exposing (Cache)
import List.Extra as List


optimizeCrossing : ( LayeredGraph a, Cache a ) -> ( LayeredGraph a, Cache a )
optimizeCrossing ( input, cache ) =
    let
        (currentCrossings, cache1) =
            Computation.crossingsForLayeredGraph (input, cache)
    in
        if currentCrossings == 0 then
            ( input, cache )
        else
            let
                pathVariationList =
                    List.map (variationsForPath input)
                        (Cache.pathsToHandle cache1)

                (cache2, result ) = improvementForPathVariationList cache1 currentCrossings pathVariationList
            in
                ( Maybe.withDefault input result
                , cache2
                )



improvementForPathVariationList : Cache a -> Int -> List (List (LayeredGraph a)) -> (Cache a, Maybe (LayeredGraph a))
improvementForPathVariationList cache n graphsList =
    case graphsList of
        [] ->
            (cache, Nothing)

        graphList :: xs ->
            let
                (cache', result) = improvementForPathVariations cache n graphList
            in
              case result of
                Just n ->
                    (cache', Just n)

                Nothing ->
                    improvementForPathVariationList cache' n xs


improvementForPathVariations : Cache a -> Int -> List (LayeredGraph a) -> (Cache a,  Maybe (LayeredGraph a))
improvementForPathVariations cache n graphs =
    case graphs of
        [] ->
            (cache, Nothing)

        graph :: xs ->
            let
                (count, cache') = Computation.crossingsForLayeredGraph (graph, cache)
            in
                if  count  < n then
                    (cache', Just graph)
                else
                    improvementForPathVariations cache' n xs



variationsForPath : LayeredGraph a -> List String -> List (LayeredGraph a)
variationsForPath input ids =
    List.interweave
        (shiftRightVariations input ids)
        (shiftLeftVariations input ids)


shiftLeftVariations : LayeredGraph a -> List String -> List (LayeredGraph a)
shiftLeftVariations input ids =
    let
        firstIds =
            input.layers |> List.filterMap List.head

        allFirst =
            List.all (flip List.member firstIds) ids
    in
        if allFirst then
            []
        else
            let
                newInput =
                    { input | layers = List.map (pushIdLeftInLayer ids) input.layers }
            in
                newInput :: shiftLeftVariations newInput ids


shiftRightVariations : LayeredGraph a -> List String -> List (LayeredGraph a)
shiftRightVariations input ids =
    let
        lastIds =
            input.layers |> List.filterMap List.last

        allLast =
            List.all (flip List.member lastIds) ids
    in
        if allLast then
            []
        else
            let
                newInput =
                    { input | layers = List.map (pushIdRightInLayer ids) input.layers }
            in
                newInput :: shiftRightVariations newInput ids


pushIdLeftInLayer : List String -> Layer-> Layer
pushIdLeftInLayer ids layer =
    case layer of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: xs ->
            if List.member y ids then
                y :: x :: xs
            else
                x :: pushIdLeftInLayer ids (y :: xs)


pushIdRightInLayer : List String -> Layer -> Layer
pushIdRightInLayer ids layer =
    case layer of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: xs ->
            if List.member x ids then
                y :: x :: xs
            else
                x :: pushIdRightInLayer ids (y :: xs)

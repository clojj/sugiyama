module Sugiyama.Crossing.ShiftingReduction exposing (..)

import Sugiyama.Crossing.Computation as Computation

import Sugiyama.Domain exposing (LayeredGraph, Node, Layer)
import Sugiyama.Cache as Cache exposing (Cache)
import List.Extra as List
import Dict

optimizeCrossing : (LayeredGraph a, Cache a) -> (LayeredGraph a, Cache a)
optimizeCrossing (input, cache) =
    let
        currentCrossings = Computation.crossingsForLayeredGraph input
    in
        if currentCrossings == 0 then
            (input, cache)
        else
            let
                outgoingForId n =
                    input.edges
                    |> List.filter (fst >> .id >> (==) n)
                    |> List.map (\(x,y) -> (x.id, y.id))

                idIncomingDict = input.layers
                    |> List.concatMap (List.map .id)
                    |> List.map (\x -> (x, outgoingForId x))
                    |> List.filter (snd >> List.length >> (>=) 1)
                    |> Dict.fromList

                pathVariationList = List.map (variationsForPath input) (Cache.pathsToHandle cache)
            in
                (improvementForPathVariationList currentCrossings pathVariationList
                    |> Maybe.withDefault input
                , cache)

improvementForPathVariationList : Int -> List (List (LayeredGraph a)) -> Maybe (LayeredGraph a)
improvementForPathVariationList n graphsList =
    case graphsList of
        [] -> Nothing
        graphList :: xs ->
            case improvementForPathVariations n graphList of
                Just n ->
                    Just n
                Nothing ->
                    improvementForPathVariationList n xs

improvementForPathVariations : Int -> List (LayeredGraph a) -> Maybe (LayeredGraph a)
improvementForPathVariations n graphs =
    case graphs of
        [] -> Nothing
        graph :: xs ->
            if Computation.crossingsForLayeredGraph graph < n then
                Just graph
            else
                improvementForPathVariations n xs

variationsForPath : LayeredGraph a -> List String -> List (LayeredGraph a)
variationsForPath input ids =
    List.interweave
        (shiftRightVariations input ids)
        (shiftLeftVariations input ids)

shiftLeftVariations : LayeredGraph a -> List String -> List (LayeredGraph a)
shiftLeftVariations input ids =
    let
        firstIds = input.layers |> List.filterMap List.head |> List.map .id

        allFirst = List.all (flip List.member firstIds) ids
    in
        if allFirst then
            []
        else
            let
                newInput = { input | layers = List.map (pushIdLeftInLayer ids) input.layers }
            in
                newInput :: shiftLeftVariations newInput ids


shiftRightVariations : LayeredGraph a -> List String -> List (LayeredGraph a)
shiftRightVariations input ids =
    let
        lastIds = input.layers |> List.filterMap List.last |> List.map .id

        allLast = List.all (flip List.member lastIds) ids
    in
        if allLast then
            []
        else
            let
                newInput = { input | layers = List.map (pushIdRightInLayer ids) input.layers }
            in
                newInput :: shiftRightVariations newInput ids

pushIdLeftInLayer : List String -> Layer a -> Layer a
pushIdLeftInLayer ids layer =
    case layer of
        [] ->
            []
        [x] ->
            [x]

        x :: y :: xs ->
            if List.member y.id ids then
                y :: x :: xs
            else
                x :: pushIdLeftInLayer ids (y :: xs)

pushIdRightInLayer : List String -> Layer a -> Layer a
pushIdRightInLayer ids layer =
    case layer of
        [] ->
            []
        [x] ->
            [x]

        x :: y :: xs ->
            if List.member x.id ids then
                y :: x :: xs
            else
                x :: pushIdRightInLayer ids (y :: xs)

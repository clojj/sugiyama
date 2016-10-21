module Sugiyama.Crossing.ShiftingReduction exposing (..)

import Sugiyama.Crossing.Computation as Computation

import Sugiyama.Domain exposing (LayeredGraph, Node, Layer)
import Dict exposing (Dict)
import List.Extra as List
optimizeCrossing : LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    let
        currentCrossings = Computation.crossingsForLayeredGraph input
    in
        if currentCrossings == 0 then
            input
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

                pathsToHandle = createPaths idIncomingDict |> List.sortBy List.length

                pathVariationList = List.map (variationsForPath input) pathsToHandle


            in
                improvementForPathVariationList currentCrossings pathVariationList
                |> Maybe.withDefault input

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
    --- TODO Also do left
    shiftRightVariations input ids

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


createPaths : Dict String (List (String, String)) -> List (List String)
createPaths input =
    let
        keys = Dict.keys input

        pathForKey key =
            Dict.get key input
            |> Maybe.map (\list ->
                case list of
                    [] -> []
                    [(x,y)] -> (x,y) :: pathForKey y
                    _ -> [])
            |> Maybe.withDefault []

    in
        -- TODO: Do something about subpath of

            List.map pathForKey keys
            |> List.filter (List.length >> (<=) 2)
            |> List.filterMap (\p ->
                Maybe.map (\h -> (fst h) :: (List.map snd p) ) (List.head p))
            |> Debug.log "Paths"

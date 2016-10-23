module Sugiyama.ShiftablePaths exposing (shiftablePaths)

import Sugiyama.Domain exposing (..)
import Dict exposing (Dict)

shiftablePaths : LayeredGraph a -> List (List String)
shiftablePaths input =
    let
        outgoingForId n =
            input.edges
            |> List.filter (fst >> (==) n)

        idIncomingDict = input.layers
            |> List.concat
            |> List.map (\x -> (x, outgoingForId x))
            |> List.filter (snd >> List.length >> (>=) 1)
            |> Dict.fromList

    in
      createPaths idIncomingDict |> List.sortBy List.length


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

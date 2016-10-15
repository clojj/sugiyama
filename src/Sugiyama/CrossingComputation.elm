module Sugiyama.CrossingComputation exposing (..)

import Sugiyama.Domain exposing (Node, Layer)
import Sugiyama.Utils exposing (orderedPairs)
import List.Extra as List

computeCrossingsPairs : Layer a -> Layer b -> List ( Node a, Node b ) -> List (String, String, Int)
computeCrossingsPairs inLayer outLayer edges =
    let
        pairs = outLayer
                |> List.concatMap (\x -> outLayer `List.andThen` (\y -> [(x, y)]))
                |> List.filter (\(x,y) -> x.id /= y.id)
                |> List.map (\(x,y) -> (x.id,y.id, crossingsForItems inLayer edges x y))
    in
        pairs


computeCrossings : Layer a -> Layer b -> List ( Node a, Node b ) -> Int
computeCrossings aNodes bNodes links =
    let
        bPairs =
            orderedPairs bNodes
    in
        bPairs
            |> List.map (\( l, r ) -> crossingsForItems aNodes links l r)
            |> List.sum


crossingsForItems : Layer a -> List ( Node a, Node b ) -> Node b -> Node b -> Int
crossingsForItems aNodes links left right =
    let
        _ = Debug.log "CrossingsForItem" "!"
        rightSources =
            getSourceIndexes aNodes links right

        leftSources =
            getSourceIndexes aNodes links left
    in
        leftSources
            |> List.map (\li -> List.takeWhile ((>) li) rightSources |> List.length)
            |> List.sum

getSourceIndexes : Layer a -> List ( Node a, Node b ) -> Node b -> List Int
getSourceIndexes aNodes links target =
    links
        |> List.filter (snd >> (==) target)
        |> List.map fst
        |> List.filterMap (flip List.elemIndex aNodes)

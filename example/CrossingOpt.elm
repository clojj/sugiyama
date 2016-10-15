module CrossingOpt exposing (..)

import Html exposing (..)
import Sugiyama.Domain exposing (..)
import Sugiyama.CrossingComputation as CrossingComputation exposing (..)
import Sugiyama.Cache as Cache
import Sugiyama.CrossingReduction as CrosRed
import List.Extra as List

node : Int -> Node Int
node x = { id = toString x, value = Val x}

layer1 : Layer Int
layer1=  [1..3] |> List.map node

layer2 : Layer Int
layer2 = [4..8] |> List.map node

edges : List (Node Int, Node Int)
edges = [(node 1, node 7), (node 1, node 4), (node 3, node 5),(node 2, node 6), (node 4,node 6)]

main : Html a
main = main1

main1 : Html a
main1 =
    let
        m = List.permutations layer2
    in
        text <| toString <| CrosRed.findOptimalPermution2 Cache.newCache m layer1 layer2 edges

-- main2 : Html a
-- main2 =
--     let
--         m = List.permutations layer2
--
--         x = CrosRed.findOptimalPermution Cache.newCache m layer1 layer2 edges
--             -- |> List.map (flip (CrossingComputation.computeCrossings layer1) edges)
--             -- |> List.minimum
--
--         _ = Debug.log "Naieve" "!"
--     in
--     div []
--         [ p [] [text "Layer 1 ", text <|toString layer1]
--         , p [] [text "Layer 2 ", text <|toString layer2]
--         , p [] [text "Edges ", text <|toString edges]
--         , p [] [text "Crossings: ", text <| toString x]
--         ]

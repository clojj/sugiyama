port module PerformanceTest exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)
import DemoGraphs
import Sugiyama.Crossing.Reduction as Reduction
import Sugiyama.Crossing.Computation as Computation
import Sugiyama.Dummies as Dummies
import Sugiyama.Cache as Cache
import Expect
import Sugiyama

main : Program Value
main =
    run emit <|
        test "Just speed for graph 2" <|
            \() ->
                DemoGraphs.graph2
                 |> uncurry Sugiyama.asGraph
                 |> Sugiyama.layeredGraph
                 |> Dummies.addDummyVertices
                 |> Reduction.optimizeCrossing
                 |> (\l -> (l, Cache.newCache l))
                 |> Computation.crossingsForLayeredGraph
                 |> fst
                 |> flip Expect.equal 1



port emit : ( String, Value ) -> Cmd msg

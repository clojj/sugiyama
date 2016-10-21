module Tests exposing (..)

import Test exposing (..)
import Expect
import Sugiyama
import DemoGraphs
import Sugiyama.Crossing.LayeredReduction as LayeredReduction
import Sugiyama.Crossing.Reduction as Reduction
import Sugiyama.Crossing.Computation as Computation

all : Test
all =
    describe "A Test Suite"
        [ graph4ShouldHaveNoCrossings
        ]

graph4ShouldHaveNoCrossings : Test
graph4ShouldHaveNoCrossings =
    test "Graph 4 should have no crossings" <|
        \ () ->
            --TODO With Random edges
            DemoGraphs.graph4
             |> uncurry Sugiyama.asGraph
             |> Sugiyama.layeredGraph
             |> Reduction.optimizeCrossing
             |> Computation.crossingsForLayeredGraph
             |> flip Expect.equal 0

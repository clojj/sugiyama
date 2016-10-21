module Sugiyama.Crossing.Reduction exposing (..)

import Sugiyama.Domain exposing (..)
import Sugiyama.Crossing.LayeredReduction as LayeredReduction
import Sugiyama.Crossing.ShiftingReduction as ShiftingReduction

optimizeCrossing : LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    let
        newInput = LayeredReduction.optimizeCrossing input
            |> ShiftingReduction.optimizeCrossing
    in
        newInput

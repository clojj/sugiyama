module Sugiyama.Crossing.Reduction exposing (..)

import Sugiyama.Domain exposing (..)
import Sugiyama.Crossing.LayeredReduction as LayeredReduction
import Sugiyama.Crossing.ShiftingReduction as ShiftingReduction
import Sugiyama.Cache as Cache exposing (Cache)

optimizeCrossing :  LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    let
        _ = Debug.log "Optimize crossings" "!"
        cache =
            Cache.newCache input
    in
        optimizeCrossing' (input, cache ) |> fst

optimizeCrossing' : (LayeredGraph a, Cache a) -> (LayeredGraph a, Cache a)
optimizeCrossing' input =
    let
        result =
            input
            |> LayeredReduction.optimizeCrossing
            |> ShiftingReduction.optimizeCrossing

    in
        if fst result == fst input then
            result
        else
            optimizeCrossing' result

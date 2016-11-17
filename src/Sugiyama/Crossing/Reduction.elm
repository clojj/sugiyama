module Sugiyama.Crossing.Reduction exposing (..)

import Sugiyama.Domain exposing (..)
import Sugiyama.Crossing.LayeredReduction as LayeredReduction
import Sugiyama.Crossing.ShiftingReduction as ShiftingReduction
import Sugiyama.Cache as Cache exposing (Cache)


optimizeCrossing : LayeredGraph a -> LayeredGraph a
optimizeCrossing input =
    let
        _ =
            Debug.log "Optimize crossings" "!"

        cache =
            Cache.newCache input
    in
        optimizeCrossing_ ( input, cache ) |> Tuple.first


optimizeCrossing_ : ( LayeredGraph a, Cache a ) -> ( LayeredGraph a, Cache a )
optimizeCrossing_ input =
    let
        result =
            input
                |> LayeredReduction.optimizeCrossing
                |> ShiftingReduction.optimizeCrossing
    in
        if Tuple.first result == Tuple.first input then
            result
        else
            optimizeCrossing_ result

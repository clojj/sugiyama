module Sugiyama.Utils exposing (orderedPairs)


orderedPairs : List b -> List ( b, b )
orderedPairs input =
    case input of
        x :: rest ->
            List.map ((,) x) rest ++ orderedPairs rest

        [] ->
            []

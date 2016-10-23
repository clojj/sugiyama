module Sugiyama.Utils exposing (orderedPairs, isNotDummy, isDummy)

import Sugiyama.Domain exposing (Node)
import String

orderedPairs : List b -> List ( b, b )
orderedPairs input =
    case input of
        x :: rest ->
            List.map ((,) x) rest ++ orderedPairs rest

        [] ->
            []


isNotDummy : Node ->  Bool
isNotDummy =
    not << isDummy


isDummy : Node -> Bool
isDummy node =
    case String.toInt node of
        Ok _ -> False
        Err _ -> True

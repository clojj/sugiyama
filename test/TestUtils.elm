module TestUtils exposing (..)


import Sugiyama.Domain exposing (..)


intNode : Int -> Node Int
intNode n =
    { id = toString n, value = Val n }

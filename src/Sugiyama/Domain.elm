module Sugiyama.Domain exposing (..)

import Dict exposing (Dict)

type alias Graph a =
    { vertices : List (Node a)
    , edges : List (Edge a)
    , mapping : Dict String a
    }

type alias Layer a = List (Node a)


type alias LayerPermutationDict a =
    Dict Int (LayerPermutation a)


type alias LayerPermutation a =
    List (Layer a)


type alias LayeredGraph a =
    { layers : List (Layer a)
    , edges : List (Edge a)
    }


type NodeValue a
    = Dummy
    | Val a


type alias Node a =
    { id : String
    , value : NodeValue a
    }


type alias Edge a =
    ( Node a, Node a )


type alias LayerNode a =
    ( Int, Node a )

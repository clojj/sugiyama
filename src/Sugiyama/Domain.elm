module Sugiyama.Domain exposing (..)


type alias Graph a =
    { vertices : List (Node a)
    , edges : List (Edge a)
    }


type alias LayeredGraph a =
    { layers : List (List (Node a))
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

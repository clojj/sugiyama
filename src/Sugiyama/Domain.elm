module Sugiyama.Domain exposing (..)

import Dict exposing (Dict)

type alias Graph a =
    { vertices : List Node
    , edges : List Edge
    , mapping : Dict String a
    }

type alias Layer = List Node


type alias RenderGroup =
    { min : Float, max : Float, items : List Node }


type alias RenderableGraph a =
    { width : Int
    , height : Int
    , vertices :
        List
            { key : String 
            , x : Float
            , y : Float
            }
    , edges :
        List (List ( Float, Float ))
    , mapping : Dict String a
    }

type alias LayerPermutationDict =
    Dict Int LayerPermutation


type alias LayerPermutation =
    List Layer


type alias LayeredGraph a =
    { layers : List Layer
    , edges : List Edge
    , mapping : Dict String a
    }

type alias Node = String


type alias Edge =
    ( Node, Node )


type alias LayerNode =
    ( Int, Node )

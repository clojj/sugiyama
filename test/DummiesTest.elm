module DummiesTest exposing (..)

import ElmTest exposing (..)
import TestUtils exposing (..)
import Sugiyama.Dummies exposing (..)
import Sugiyama.Domain exposing (..)


inputGraph : LayeredGraph Int
inputGraph =
    { layers =
        [ [ intNode 1, intNode 2 ]
        , [ intNode 3 ]
        , [ intNode 4 ]
        , [ intNode 5 ]
        ]
    , edges =
        [ ( intNode 1, intNode 3 )
        , ( intNode 2, intNode 4 )
        , ( intNode 1, intNode 5 )
        , ( intNode 3, intNode 4 )
        , ( intNode 4, intNode 5 )
        ]
    }


testAddLayerNodes : Test
testAddLayerNodes =
    test "Adds layer nodes"
        (assertEqual
            (addDummyVertices inputGraph).layers
            [ [ intNode 1, intNode 2 ]
            , [ { id = "1_5_x", value = Dummy }, { id = "2_4_x", value = Dummy }, intNode 3 ]
            , [ { id = "1_5_xx", value = Dummy }, intNode 4 ]
            , [ intNode 5 ]
            ]
        )


testAddEdges : Test
testAddEdges =
    test "Adds layer nodes"
        (assertEqual
            ((addDummyVertices inputGraph).edges |> List.sortBy (\( from, to ) -> from.id))
            [ ( intNode 1, intNode 3 )
            , ( intNode 1, { id = "1_5_x", value = Dummy } )
            , ( { id = "1_5_x", value = Dummy }, { id = "1_5_xx", value = Dummy } )
            , ( { id = "1_5_xx", value = Dummy }, intNode 5 )
            , ( intNode 2, { id = "2_4_x", value = Dummy } )
            , ( { id = "2_4_x", value = Dummy }, intNode 4 )
            , ( intNode 3, intNode 4 )
            , ( intNode 4, intNode 5 )
            ]
        )


dummiesSuite : Test
dummiesSuite =
    suite "Dummies Suite"
        [ testAddLayerNodes
        , testAddEdges
        ]

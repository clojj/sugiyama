module CrossingReductionTest exposing (..)

import Sugiyama.CrossingReduction exposing (..)
import ElmTest exposing (..)
import Sugiyama.Domain exposing (..)
import TestUtils exposing (..)

orderedPairsSuite : Test
orderedPairsSuite =
    suite "Orderd Pairs"
        [ test "Empty list" (assertEqual (orderedPairs []) [])
        , test "Single element " (assertEqual (orderedPairs [ 1 ]) [])
        , test "Two elements" (assertEqual (orderedPairs [ 1, 2 ]) [ ( 1, 2 ) ])
        , test "Three element"
            (assertEqual (orderedPairs [ 1, 2, 3 ])
                [ ( 1, 2 )
                , ( 1, 3 )
                , ( 2, 3 )
                ]
            )
        ]


computeCrossingsSuite : Test
computeCrossingsSuite =
    suite "Compute Crossing"
        [ test "Empty List" (assertEqual (computeCrossings [] [] []) 0)
        , test "Single link" (assertEqual (computeCrossings [ 11 ] [ 21 ] [ ( 11, 22 ) ]) 0)
        , test "Parallel links" (assertEqual (computeCrossings [ 11, 12 ] [ 21, 22 ] [ ( 11, 21 ), ( 12, 22 ) ]) 0)
        , test "Crossing links" (assertEqual (computeCrossings [ 11, 12 ] [ 21, 22 ] [ ( 11, 22 ), ( 12, 21 ) ]) 1)
        ]


crossingForItemsSuite : Test
crossingForItemsSuite =
    suite "Crossing For Items"
        [ test "Parallel" (assertEqual (crossingsForItems [ 11, 12 ] [ ( 11, 21 ), ( 12, 22 ) ] 21 22) 0)
        , test "Crossed" (assertEqual (crossingsForItems [ 11, 12 ] [ ( 11, 21 ), ( 12, 22 ) ] 22 21) 1)
        ]


reduceToSuite : Test
reduceToSuite =
    suite "Recude To"
        [ test "Straighten out"
            (assertEqual
                (reduceTo
                    ( [ 1, 2, 3 ]
                    , [ 11, 12, 13 ]
                    , [ ( 1, 12 ), ( 2, 13 ), ( 3, 11 ) ]
                    )
                )
                [ 12, 13, 11 ]
            )
        , test "Best possible"
            (assertEqual
                (reduceTo
                    ( [ 1, 2 ]
                    , [ 11, 12, 13, 14 ]
                    , [ ( 1, 11 ), ( 2, 11 ), ( 1, 14 ), ( 2, 14 ), ( 2, 13 ), ( 1, 12 ) ]
                    )
                )
                [ 12, 11, 14, 13 ]
            )
        ]

numberOfCrossingsSuite : Test
numberOfCrossingsSuite =
    suite "Number of crossings"
        [ test "Case 1"
            (assertEqual
                (numberOfCrossings
                    { layers =
                        [ [ intNode1, intNode2 ]
                        , [ intNode3, intNode4, intNode5, intNode6 ]
                        , [ intNode7, intNode8]
                        , [ intNode9]
                        ]
                    , edges =
                        [ ( intNode1, intNode4 )
                        , ( intNode1, intNode5 )
                        , ( intNode1, intNode6 )
                        , (intNode2, intNode3)
                        , (intNode3, intNode8)
                        , (intNode4, intNode8)
                        , (intNode6, intNode8)
                        , (intNode5, intNode7)
                        , (intNode7, intNode9)
                        , (intNode8, intNode9)
                        ]
                    }
                )
                5
            )
        ]


allSuites : Test
allSuites =
    suite "All Tests"
        [ orderedPairsSuite
        , reduceToSuite
        , computeCrossingsSuite
        , crossingForItemsSuite
        , numberOfCrossingsSuite
        ]


main : Program Never
main =
    runSuiteHtml allSuites

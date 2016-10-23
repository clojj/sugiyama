module DemoGraphs exposing (..)


type alias DemoGraph a =  ( List a, List ( a, a ) )

graph6 : DemoGraph String
graph6 =
    ( ["A","B","C"]
    , [("A","B"),("B","C")]
    )

graph1 : DemoGraph Int
graph1 =
    ( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    , [ ( 1, 4 )
      , ( 2, 4 )
      , ( 3, 6 )
      , ( 4, 5 )
      , ( 4, 6 )
      , ( 5, 7 )
      , ( 6, 8 )
      , ( 6, 9 )
      , ( 1, 9 )
      , ( 1, 6 )
      , ( 3, 7 )
      , ( 1, 8 )
      , ( 4, 8 )
      , ( 1, 2 )
      , ( 1, 3 )
      ]
    )


graph2 : DemoGraph Int
graph2 =
    ( [0..47]
    , [ ( 0, 1 )
      , ( 0, 4 )
      , ( 0, 5 )
      , ( 1, 2 )
      , ( 2, 3 )
      , ( 3, 6 )
      , ( 3, 7 )
      , ( 3, 13 )
      , ( 4, 6 )
      , ( 5, 36 )
      , ( 6, 9 )
      , ( 7, 8 )
      , ( 8, 14 )
      , ( 9, 11 )
      , ( 9, 14 )
      , ( 10, 12 )
      , ( 11, 17 )
      , ( 12, 18 )
      , ( 13, 17 )
      , ( 14, 15 )
      , ( 15, 16 )
      , ( 16, 17 )
      , ( 18, 19 )
      , ( 18, 22 )
      , ( 18, 23 )
      , ( 19, 20 )
      , ( 20, 21 )
      , ( 21, 24 )
      , ( 21, 25 )
      , ( 21, 31 )
      , ( 22, 24 )
      , ( 23, 28 )
      , ( 24, 27 )
      , ( 25, 26 )
      , ( 26, 32 )
      , ( 27, 28 )
      , ( 27, 29 )
      , ( 27, 32 )
      , ( 28, 30 )
      , ( 29, 35 )
      , ( 30, 35 )
      , ( 31, 35 )
      , ( 32, 33 )
      , ( 33, 34 )
      , ( 34, 17 )
      , ( 36, 37 )
      , ( 36, 38 )
      , ( 37, 39 )
      , ( 37, 40 )
      , ( 37, 41 )
      , ( 38, 42 )
      , ( 38, 43 )
      , ( 39, 47 )
      , ( 40, 47 )
      , ( 41, 44 )
      , ( 42, 45 )
      , ( 43, 46 )
      , ( 44, 46 )
      , ( 45, 46 )
      , ( 46, 47 )
      , ( 47, 10 )
      ]
    )

graph3 : DemoGraph Int
graph3 =
    let
        (nodes, edges) = graph2
        newNodes =  nodes
        |> List.filter ((<=) 17)
        |> List.filter ((>=) 35)
        |> List.filter ((/=) 18)
        |> List.filter ((/=) 19)
        |> List.filter ((/=) 20)
    in
        ( newNodes
        , edges |> List.filter (\(x,y) -> List.member x newNodes && List.member y newNodes)
        )

graph4 : DemoGraph Int
graph4 =
    let
        nodes = [1,2, 3,4,5, 6,7
            ]

        edges =
            [ (1,4) , (2,3), (2,5), (3,6), (4,7), (5,6), (5,7) ]
    in (nodes, edges)

graph5 : DemoGraph Int
graph5 =
    let nodes = [1..12]
        edges = [(1,2),(1,3)
                ,(2,4),(2,5),(2,8),(3,6),(3,7)
                ,(4,12),(5,12),(6,11),(7,9),(8,10)
                ,(9,11),(10,11)
                ,(11,12)
                ]
    in
        (nodes, edges)

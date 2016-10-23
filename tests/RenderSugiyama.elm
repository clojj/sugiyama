module RenderSugiyama exposing (..)

import Html
import Sugiyama
import Svg
import Svg.Attributes exposing (cx, cy, r)
import String
import DemoGraphs
import Dict

height : Float
height =
    600.0


width : Float
width =
    600.0


padding : Float
padding =
    50.0


radius : Float
radius =
    20.0


main : Html.Html a
main =
    let
        ( nodes, edges ) =
            DemoGraphs.graph6

        optimized =
            Sugiyama.sugiyama nodes edges
    in
        case optimized of
            Err err ->
                Html.text err

            Ok result ->
                let
                    circles =
                        result.vertices
                            |> List.filterMap (\n ->
                                case Dict.get n.key result.mapping of
                                    Just v ->
                                        Just <| asCircle (n.x) n.y v
                                    Nothing ->
                                        Nothing
                            )
                            |> List.concat

                    lines =
                        result.edges
                            |> List.map (List.map toRealCoordinate)
                            |> List.map fixPathOriginAndDestination
                            |> List.concatMap asPath

                    markers =
                        [ Svg.defs [] [ arrowHead ] ]
                in
                    Svg.svg
                        [ Svg.Attributes.width (toString width)
                        , Svg.Attributes.height (toString height)
                        , Svg.Attributes.fill "blue"
                        ]
                        (markers ++ lines ++ circles)


arrowHead : Svg.Svg msg
arrowHead =
    Svg.marker
        [ Svg.Attributes.id "arrowHead"
        , Svg.Attributes.markerWidth "13"
        , Svg.Attributes.markerHeight "13"
        , Svg.Attributes.refX "2"
        , Svg.Attributes.refY "6"
        , Svg.Attributes.orient "auto"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M2,2 L2,11 L10,6 L2,2"
            , Svg.Attributes.fill "black"
            ]
            []
        ]


fixPathOriginAndDestination : List ( Float, Float ) -> List ( Float, Float )
fixPathOriginAndDestination input =
    let
        _ =
            Debug.log "Foo"

        len =
            List.length input

        newFirst =
            List.head input
                |> Maybe.map (\( x, y ) -> ( x, y + radius ))
                |> Maybe.map (\x -> [ x ])
                |> Maybe.withDefault []

        newLast =
            List.drop ((List.length input) - 1) input
                |> List.map (\( x, y ) -> ( x, y - radius - 8 ))

        mid =
            List.drop 1 input |> List.take (len - 2)
    in
        newFirst ++ mid ++ newLast


asPath : List ( Float, Float ) -> List (Svg.Svg msg)
asPath points =
    let
        pairs =
            List.map2 (,) points (List.drop 1 points)

        init =
            List.take (List.length pairs - 1) pairs
                |> List.map (asCurve False)

        last =
            List.drop (List.length pairs - 1) pairs
                |> List.map (asCurve True)
    in
        init ++ last


asCurve : Bool -> ( ( Float, Float ), ( Float, Float ) ) -> Svg.Svg msg
asCurve hasMarker ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        mid =
            (y2 - y1) / 2.0 + y1
    in
        Svg.path
            [ [ "M"
              , toString x1
              , toString y1
              , "C"
              , toString x1
              , toString mid
              , toString x2
              , toString mid
              , toString x2
              , toString y2
              ]
                |> String.join " "
                |> Svg.Attributes.d
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.fill "transparent"
            , Svg.Attributes.markerEnd
                (if hasMarker then
                    "url(#arrowHead)"
                 else
                    ""
                )
            ]
            []


toRealCoordinate : ( Float, Float ) -> ( Float, Float )
toRealCoordinate ( x, y ) =
    ( asX x, asY y )


asCircle : Float -> Float -> a -> List (Svg.Svg msg)
asCircle x y n =
    [ Svg.circle
        [ cx <| toString (asX x)
        , cy <| toString (asY y)
        , r (toString radius)
        ]
        []
    , Svg.text'
        [ Svg.Attributes.x <| toString (asX x)
        , Svg.Attributes.y <| toString (asY y)
        , Svg.Attributes.fill "white"
        ]
        [ Svg.text <| toString n ]
    ]


asX : Float -> Float
asX x =
    (width - 2 * padding) * x + padding


asY : Float -> Float
asY y =
    (height - 2 * padding) * y + padding

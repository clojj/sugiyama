module Sugiyama exposing (sugiyama,asGraph,layeredGraph)

import Dict exposing (Dict)
import Sugiyama.Crossing.Reduction as Reduction
import Sugiyama.Domain exposing (..)
import Sugiyama.Rendering as Rendering
import Sugiyama.Dummies as Dummies
import Sugiyama.Utils exposing (isNotDummy)
import Result


sugiyama : List a -> List (a, a) -> Result String (RenderableGraph a)
sugiyama vertices edges =
    let
      graph = asGraph vertices edges

      layered = layeredGraph graph

      layeredValues =
        layered.layers
        |> List.concat
        |> List.filter isNotDummy

      isAcyclic = List.all (\n -> List.member n layeredValues) (Dict.keys layered.mapping)
    in
      if not isAcyclic then
        Result.Err "Not an acyclic graph"
      else
        layered
          |> Dummies.addDummyVertices
          |> Reduction.optimizeCrossing
          |> Rendering.asRenderedGraph
          |> Result.Ok

-- Build Graph



{-| TODO REWRITE ARGUMENTS
-}
asGraph : List a -> List ( a, a ) -> Graph a
asGraph nodes edges =
    let
        mappingPairs = nodes |> List.indexedMap (\n x -> (toString n, x))

        mapping =
            Dict.fromList mappingPairs

        getStringForValue v =
            mappingPairs
            |> List.filter (snd >> (==) v)
            |> List.head
            |> Maybe.map fst

        -- realVerticesIndex =
        --     vertexIndex vertices

        realNodes =
            nodes |> List.filterMap getStringForValue

        realEdges =
            edges
            |> List.filterMap (\(x,y) ->
                 Maybe.map2 (,) (getStringForValue x) (getStringForValue y)
                 )
    in
        { vertices = realNodes
        , edges = realEdges
        , mapping = mapping
        }

--
-- vertexIndex : List a -> Dict Int Node
-- vertexIndex vertices =
--     vertices
--         |> List.indexedMap (\i v -> ( i, { id = toString (i + 1), value = Val v } ))
--         |> Dict.fromList


-- asRealEdge : List a -> Dict Int Node -> ( a, a ) -> Maybe ( Node a, Node a )
-- asRealEdge list index ( from, to ) =
--     let
--         indexFrom =
--             List.elemIndex from list
--
--         indexTo =
--             List.elemIndex to list
--
--         nodeFrom =
--             indexFrom `Maybe.andThen` (\x -> Dict.get x index)
--
--         nodeTo =
--             indexTo `Maybe.andThen` (\x -> Dict.get x index)
--     in
--         case ( nodeFrom, nodeTo ) of
--             ( Just f, Just t ) ->
--                 Just ( f, t )
--
--             _ ->
--                 Nothing
--


-- Graph to a layered graph


layeredGraph : Graph a -> LayeredGraph a
layeredGraph graph =
    let
        layers =
            layeredModulesInner [] graph.edges graph.vertices []

        layersIndex =
            layers
                |> List.indexedMap (\index layer -> List.map (\node -> ( node, index )) layer)
                |> List.concat
                |> Dict.fromList
    in
        { layers = List.reverse layers
        , edges = graph.edges
        , mapping = graph.mapping
        }


layeredModulesInner : List String -> List Edge -> List Node -> List (List Node) -> List (List Node)
layeredModulesInner resolved allEdges allModules answer =
    let
        nextGroup' =
            nextGroup resolved allEdges allModules
    in
        if List.isEmpty nextGroup' then
            answer
        else
            let
                nextGroupIds' =
                    nextGroup'

                newResolved =
                    nextGroupIds' ++ resolved

                newAllModules =
                    List.filter (not << flip List.member nextGroupIds') allModules
            in
                layeredModulesInner newResolved allEdges newAllModules (nextGroup' :: answer)


nextGroup : List String -> List Edge -> List Node -> List Node
nextGroup resolved allEdges restVertices =
    let
        isResolved vertex =
            let
                deps =
                    allDeps allEdges vertex
            in
                List.all (\x -> List.member x resolved) deps
    in
        List.filter isResolved restVertices


allDeps : List Edge -> Node -> List String
allDeps edges vertex =
    List.filter (\( x, y ) -> y == vertex) edges
        |> List.map fst

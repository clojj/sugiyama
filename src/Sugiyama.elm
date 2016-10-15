module Sugiyama exposing (sugiyama)

import Dict exposing (Dict)
import List.Extra as List
import Sugiyama.CrossingReduction as CrossingReduction
import Sugiyama.Domain exposing (..)
import Sugiyama.Rendering as Rendering
import Sugiyama.Dummies as Dummies
import Result

type alias RenderableGraph a =
    Rendering.RenderableGraph a


sugiyama : List a -> List ( a, a ) -> Result String (RenderableGraph a)
sugiyama vertices edges =
  -- TODO Check cycles...
    let
      graph = asGraph vertices edges
          -- |> Debug.log "Graph"

      layered = layeredGraph graph
          -- |> Debug.log "Layered"

      layeredValues =
        layered.layers
        |> List.concat
        |> List.filterMap (\x ->
           case x.value of
            Val n -> Just n
            Dummy -> Nothing
        )


      isAcyclic = List.all (\n -> List.member n layeredValues) vertices
        -- |> Debug.log "isAcyclic"
    in
      if not isAcyclic then
        Result.Err "Not an acyclic graph"
      else
        layered
          |> Dummies.addDummyVertices
        --   |> Debug.log "Dummies"
          |> CrossingReduction.optimizeCrossing
        --   |> Debug.log "Optimized"
          |> Rendering.asRenderedGraph
        --   |> Debug.log "As rendered graph"
          |> Result.Ok


-- Build Graph


asGraph : List a -> List ( a, a ) -> Graph a
asGraph vertices edges =
    let
        realVerticesIndex =
            vertexIndex vertices

        realVertices =
            Dict.values realVerticesIndex

        realEdges =
            List.filterMap (asRealEdge vertices realVerticesIndex) edges
    in
        { vertices = realVertices
        , edges = realEdges
        }


vertexIndex : List a -> Dict Int (Node a)
vertexIndex vertices =
    vertices
        |> List.indexedMap (\i v -> ( i, { id = toString (i + 1), value = Val v } ))
        |> Dict.fromList


asRealEdge : List a -> Dict Int (Node a) -> ( a, a ) -> Maybe ( Node a, Node a )
asRealEdge list index ( from, to ) =
    let
        indexFrom =
            List.elemIndex from list

        indexTo =
            List.elemIndex to list

        nodeFrom =
            indexFrom `Maybe.andThen` (\x -> Dict.get x index)

        nodeTo =
            indexTo `Maybe.andThen` (\x -> Dict.get x index)
    in
        case ( nodeFrom, nodeTo ) of
            ( Just f, Just t ) ->
                Just ( f, t )

            _ ->
                Nothing



-- Graph to a layered graph


layeredGraph : Graph a -> LayeredGraph a
layeredGraph graph =
    let
        layers =
            layeredModulesInner [] graph.edges graph.vertices []

        layersIndex =
            layers
                |> List.indexedMap (\index layer -> List.map (\node -> ( node.id, index )) layer)
                |> List.concat
                |> Dict.fromList
    in
        { layers = List.reverse layers, edges = graph.edges }


layeredModulesInner : List String -> List (Edge a) -> List (Node a) -> List (List (Node a)) -> List (List (Node a))
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
                    List.map .id nextGroup'

                newResolved =
                    nextGroupIds' ++ resolved

                newAllModules =
                    List.filter (not << flip List.member nextGroupIds' << .id) allModules
            in
                layeredModulesInner newResolved allEdges newAllModules (nextGroup' :: answer)


nextGroup : List String -> List (Edge a) -> List (Node a) -> List (Node a)
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


allDeps : List (Edge a) -> Node a -> List String
allDeps edges vertex =
    List.filter (\( x, y ) -> y.id == vertex.id) edges
        |> List.map fst
        |> List.map .id

module Sugiyama.Cache exposing (Cache, newCache, cachedPermutations, pathsToHandle, loadFromCache, addToCache, cacheLayerPairCrossings, hasLayerPairCrossing)

import Dict exposing (Dict)
import Sugiyama.Domain exposing (..)
import List.Extra as List
import Sugiyama.ShiftablePaths exposing (shiftablePaths)
import String


type Cache a
    = Cache
        { optimalLayers : Dict String Layer
        , layerPermutationDict : LayerPermutationDict
        , pathsToHandle : List (List String)
        , layerPairCrossings : Dict String Int
        }


newCache : LayeredGraph a -> Cache a
newCache input =
    let
        layerPermutationDict =
            layerPermutationsForGraph input

        -- layerPermutationDict = Dict.empty
    in
        Cache
            { optimalLayers = Dict.empty
            , layerPermutationDict = layerPermutationDict
            , pathsToHandle = shiftablePaths input
            , layerPairCrossings = Dict.empty
            }


hasLayerPairCrossing : Cache a -> ( Layer, Layer ) -> Maybe Int
hasLayerPairCrossing (Cache c) ( x, y ) =
    let
        key =
            List.append x y |> String.join "|"
    in
        Dict.get key c.layerPairCrossings


cacheLayerPairCrossings : Cache a -> ( Layer, Layer ) -> Int -> Cache a
cacheLayerPairCrossings (Cache c) ( x, y ) n =
    let
        key =
            List.append x y |> String.join "|"
    in
        Cache { c | layerPairCrossings = Dict.insert key n c.layerPairCrossings }


cachedPermutations : Cache a -> LayerPermutationDict
cachedPermutations (Cache x) =
    x.layerPermutationDict


pathsToHandle : Cache a -> List (List String)
pathsToHandle (Cache x) =
    x.pathsToHandle


layerPermutationsForGraph : LayeredGraph a -> LayerPermutationDict
layerPermutationsForGraph input =
    input
        |> .layers
        |> List.map List.permutations
        |> List.indexedMap (,)
        |> Dict.fromList


loadFromCache : Layer -> Layer -> Cache a -> Maybe Layer
loadFromCache from to (Cache cache) =
    Dict.get (optimalLayerKey from to) cache.optimalLayers


addToCache : Layer -> Layer -> Layer -> Cache a -> Cache a
addToCache from toOld toNew (Cache cache) =
    Cache { cache | optimalLayers = Dict.insert (optimalLayerKey from toOld) toNew cache.optimalLayers }


optimalLayerKey : Layer -> Layer -> String
optimalLayerKey from to =
    toString ( from, to |> List.sort )

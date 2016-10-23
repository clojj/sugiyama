module Sugiyama.Cache exposing (Cache, newCache, cachedPermutations, pathsToHandle, loadFromCache, addToCache)

import Dict exposing (Dict)
import Sugiyama.Domain exposing (..)
import List.Extra as List
import Sugiyama.ShiftablePaths exposing (shiftablePaths)
type Cache a
    = Cache
        { optimalLayers : Dict String Layer
        , layerPermutationDict : LayerPermutationDict
        ,pathsToHandle : List (List String)
        }


newCache : LayeredGraph a -> Cache a
newCache input =
    let
        layerPermutationDict = layerPermutationsForGraph input


    in
      Cache { optimalLayers = Dict.empty
          , layerPermutationDict = layerPermutationDict
          , pathsToHandle = shiftablePaths input
      }

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

module Sugiyama.Cache exposing (Cache, newCache, cachedPermutations, pathsToHandle, loadFromCache, addToCache)

import Dict exposing (Dict)
import Sugiyama.Domain exposing (..)
import List.Extra as List
import Sugiyama.ShiftablePaths exposing (shiftablePaths)
type Cache a
    = Cache
        { optimalLayers : Dict String (Layer a)
        , layerPermutationDict : LayerPermutationDict a
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

cachedPermutations : Cache a -> LayerPermutationDict a
cachedPermutations (Cache x) =
    x.layerPermutationDict

pathsToHandle : Cache a -> List (List String)
pathsToHandle (Cache x) =
    x.pathsToHandle

layerPermutationsForGraph : LayeredGraph a -> LayerPermutationDict a
layerPermutationsForGraph input =
    input
        |> .layers
        |> List.map List.permutations
        |> List.indexedMap (,)
        |> Dict.fromList


loadFromCache : Layer a -> Layer a -> Cache a -> Maybe (Layer a)
loadFromCache from to (Cache cache) =
    Dict.get (optimalLayerKey from to) cache.optimalLayers


addToCache : Layer a -> Layer a -> Layer a -> Cache a -> Cache a
addToCache from toOld toNew (Cache cache) =
    Cache { cache | optimalLayers = Dict.insert (optimalLayerKey from toOld) toNew cache.optimalLayers }


optimalLayerKey : Layer a -> Layer a -> String
optimalLayerKey from to =
    toString ( from |> List.map .id, to |> List.map .id |> List.sort )

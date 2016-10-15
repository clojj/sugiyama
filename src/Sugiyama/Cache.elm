module Sugiyama.Cache exposing (Cache, newCache, loadFromCache, addToCache)

import Dict exposing (Dict)
import Sugiyama.Domain exposing (..)


type Cache a
    = Cache
        { optimalLayers : Dict String (Layer a)
        }


newCache : Cache a
newCache =
    Cache { optimalLayers = Dict.empty }


loadFromCache : Layer a -> Layer a -> Cache a -> Maybe (Layer a)
loadFromCache from to (Cache cache) =
    Dict.get (optimalLayerKey from to) cache.optimalLayers


addToCache : Layer a -> Layer a -> Layer a -> Cache a -> Cache a
addToCache from toOld toNew (Cache cache) =
    Cache { cache | optimalLayers = Dict.insert (optimalLayerKey from toOld) toNew cache.optimalLayers }


optimalLayerKey : Layer a -> Layer a -> String
optimalLayerKey from to =
    toString ( from |> List.map .id, to |> List.map .id |> List.sort )



module Data.LinSearch where

import qualified Data.Vector as V
import qualified Data.List   as L

import Linear

import Data.Function

nearestNeighbor vs q = V.minimumBy (compare `on` qd q) vs

nearestNeighbors vs q = L.sortBy (compare `on` qd q) . V.toList $ vs

pointsAround vs r q = L.sortBy (compare `on` qd q) . V.toList . V.filter ((< (r^2)) . qd q) $ vs


--------------------------------------------------
-- | This module is only used in
-- | kdtree tests and benchmarks
--------------------------------------------------
module Data.LinSearch where




import qualified Data.Vector.Generic as V
import qualified Data.List           as L

import Linear

import Data.Function

nearestNeighbor :: (Ord b, Metric f, Num b, V.Vector v (f b))
                => f b -> v (f b) -> f b
nearestNeighbor q = V.minimumBy (compare `on` qd q)

nearestNeighbors :: (Ord b, Metric f, Num b, V.Vector v (f b))
                 => f b -> v (f b) -> [f b]
nearestNeighbors q vs = L.sortBy (compare `on` qd q) . V.toList $ vs

pointsAround :: (Ord b, Metric f, Num b, V.Vector v (f b))
             => b -> f b -> v (f b) -> [f b]
pointsAround r q vs = L.sortBy (compare `on` qd q) . V.toList . V.filter ((< r*r) . qd q) $ vs


--------------------------------------------------
-- | This module is only used in
-- | kdtree tests and benchmarks
--------------------------------------------------
module Data.LinSearch where




import qualified Data.Vector as V
import qualified Data.List   as L

import Linear

import Data.Function

nearestNeighbor :: (Ord b, Metric f, Num b) => V.Vector (f b) -> f b -> f b
nearestNeighbor vs q = V.minimumBy (compare `on` qd q) vs

nearestNeighbors :: (Ord b, Metric f, Num b) => V.Vector (f b) -> f b -> [f b]
nearestNeighbors vs q = L.sortBy (compare `on` qd q) . V.toList $ vs

pointsAround :: (Ord b, Metric f, Num b) => V.Vector (f b) -> b -> f b -> [f b]
pointsAround vs r q = L.sortBy (compare `on` qd q) . V.toList . V.filter ((< (r^(2::Int))) . qd q) $ vs


--------------------------------------------------
-- | This module is only used in
-- | kdtree tests and benchmarks
--------------------------------------------------
module Data.KDTree.Internal.LinSearch where




import qualified Data.Vector.Generic as V
import qualified Data.List           as L

import qualified Data.KDTree         as KD

import Linear

import Data.Function

--------------------------------------------------------------------------------

nearestNeighbor :: (Ord b, Metric f, Num b, V.Vector v (f b))
                => f b -> v (f b) -> f b
nearestNeighbor q = V.minimumBy (compare `on` qd q)

nearestNeighbors :: (Ord b, Metric f, Num b, V.Vector v (f b))
                 => f b -> v (f b) -> [f b]
nearestNeighbors q vs = L.sortBy (compare `on` qd q) . V.toList $ vs

pointsAround :: (Ord b, Metric f, Num b, V.Vector v (f b))
             => b -> f b -> v (f b) -> [f b]
pointsAround r q vs = L.sortBy (compare `on` qd q) . V.toList . V.filter ((< r*r) . qd q) $ vs


--------------------------------------------------------------------------------

partition :: (KD.KDCompare b, V.Vector v b)
          => KD.Dim b -> Ordering -> b -> v b -> (v b, v b)
partition dim ord q = V.partition ((== ord) . flip (KD.dimCompare dim) q)

select :: (KD.KDCompare b, V.Vector v b)
       => KD.Dim b -> Ordering -> b -> v b -> v b
select dim ord q = fst . partition dim ord q

delete :: (KD.KDCompare b, V.Vector v b)
       => KD.Dim b -> Ordering -> b -> v b -> v b
delete dim ord q = snd . partition dim ord q

update :: (KD.KDCompare b, V.Vector v b)
       => KD.Dim b -> Ordering -> b -> (b -> b) -> v b -> v b
update dim ord q f = V.map (\v -> if KD.dimCompare dim v q == ord then f v else v)

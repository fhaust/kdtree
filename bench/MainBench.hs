{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

import Criterion.Main

import qualified Data.KDTree                    as KD
import qualified Data.KDTree.Internal.Common    as KD
import qualified Data.KDTree.Internal.LinSearch as Lin

import qualified Data.Vector.Storable as V
import qualified Data.List as L

import Control.DeepSeq

import System.Random

import Linear

main :: IO ()
main = do

  -- create some samples
  gen <- getStdGen
  let vs = force $ V.fromListN 50000 $ randoms gen :: KD.VV3D

  -- get a query point that exists in the set
  --let q = vs V.! (V.length vs `quot` 2)
  let q  = V3 0 0 0
      q2 = V3 0.9 0.9 0.9


  -- create kdtree from dataset
  let bs       = 64
  let kd       = force . KD.kdtree 64 $ vs
  let nrRadius = 0.1

  let dim = KD.V3X
  let ord = GT


  -- run benchmarks
  defaultMain

     -- search the nearest neighbor
     [ bgroup "nn"
       [ bench  "linear_nn"   $ nf (Lin.nearestNeighbor q) vs
       , bench  "kdtree_nn"   $ nf (KD.nearestNeighbor q) kd
       ]
     , bgroup "nn5"
       [ bench  "linear_nn5"  $ nf (L.take 5 . Lin.nearestNeighbors q) vs
       , bench  "kdtree_nn5"  $ nf (L.take 5 . KD.nearestNeighbors q)  kd
       ]
     , bgroup "nr"
       [ bench "linear_nr"  $ nf (Lin.pointsAround nrRadius q) vs
       , bench "kdtree_nr"  $ nf (KD.pointsAround nrRadius q) kd
       ]
     , bgroup "create"
       [ bench "kdtree" $ nf (KD.kdtree 64) vs
       ]
     , bgroup "partition"
       [ bench "linear" $ nf (V.partition ((== ord) . KD.dimCompare dim q2)) vs
       , bench "kdtree" $ nf (KD.partition dim ord q2) kd
       ]
     , bgroup "select"
       [ bench "linear" $ nf (V.filter ((== ord) . KD.dimCompare dim q2)) vs
       , bench "kdtree" $ nf (KD.select dim ord q2) kd
       ]
     , bgroup "delete"
       [ bench "linear" $ nf (V.filter (not . (== ord) . KD.dimCompare dim q2)) vs
       , bench "kdtree" $ nf (KD.delete dim ord q2) kd
       ]
     , bgroup "merge"
       [ bench "kdtree" $ nf (KD.merge bs kd) kd
       ]
     , bgroup "update"
       [ bench "linear" $ nf (Lin.update dim ord q2 (+0.1)) vs
       , bench "kdtree" $ nf (KD.update bs dim ord q2 (+0.1)) kd
       ]
     ]


-- {-# SPECIALIZE KD.nearestNeighbor  :: V3 Double -> KD.KDTree V.Vector V3D -> [V3D] #-}
-- {-# SPECIALIZE KD.nearestNeighbors :: V3 Double -> KD.KDTree V.Vector V3D -> [V3D] #-}
-- {-# SPECIALIZE KD.pointsAround     :: Double -> V3 Double -> KD.KDTree V.Vector V3D -> [V3D] #-}


instance NFData a => NFData (V3 a) where
    rnf (V3 x y z) = x `seq` y `seq` z `seq` ()

instance Random (V3 Double) where
    randomR (V3 hx hy hz, V3 lx ly lz) g0 = (V3 x y z, g3)
      where (x,g1) = randomR (hx,lx) g0
            (y,g2) = randomR (hy,ly) g1
            (z,g3) = randomR (hz,lz) g2
    random g0 = (V3 x y z, g3)
      where (x,g1) = random g0
            (y,g2) = random g1
            (z,g3) = random g2

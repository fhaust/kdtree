{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

import Criterion.Main

import qualified Data.LinSearch as Lin
import qualified Data.KDTree as KD

import qualified Data.Vector.Storable as V
import qualified Data.List as L

import Control.DeepSeq

import System.Random

import Linear

main :: IO ()
main = do

  -- create some samples
  gen <- getStdGen
  let vs = force $ V.fromListN 50000 $ randoms gen :: V.Vector (V3 Double)

  -- get a query point that exists in the set
  --let q = vs V.! (V.length vs `quot` 2)
  let q = V3 0 0 0


  -- create kdtree from dataset
  let kd  = force . KD.kdtree 64 $ vs
  let nrRadius = 0.1


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
     --, bgroup "full_nn"
     --  [ bench "linear_full_nn"  $ nf (Lin.nearestNeighbor vs) q
     --  , bench "kdtree_full_nn"  $ nf (KD.nearestNeighbor q . KD.kdtree 64 8) vs
     --  ]
     --, bgroup "full_nn5"
     --  [ bench "linear_full_nn5"  $ nf (L.take 5 . flip Lin.nearestNeighbors q) vs
     --  , bench "kdtree_full_nn5"  $ nf (L.take 5 . KD.nearestNeighbors q . KD.kdtree 64 8) vs
     --  ]
     --, bgroup "full_nr"
     --  [ bench "linear_full_nr" $ nf (uncurry2 Lin.pointsAround) (vs,1,q)
     --  , bench "kdtree_full_nr" $ nf (KD.pointsAround 1 q . KD.kdtree 64 8) vs
     --  ]
     ]

-- specialize all calls so ghc can remove the implicit type class
-- dictionary passing ...

{-# SPECIALIZE KD.nearestNeighbor
                  :: V3 Double -> KD.KDTree V.Vector (V3 Double) -> [V3 Double] #-}
{-# SPECIALIZE KD.nearestNeighbors
                  :: V3 Double -> KD.KDTree V.Vector (V3 Double) -> [V3 Double] #-}
{-# SPECIALIZE KD.pointsAround
                  :: Double -> V3 Double -> KD.KDTree V.Vector (V3 Double) -> [V3 Double] #-}

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

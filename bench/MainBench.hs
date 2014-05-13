{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

import Criterion.Main

import qualified Data.LinSearch as Lin
import qualified Data.KDTree as KD
import qualified Data.KDTreeF as KDF

import qualified Data.Vector as V
import qualified Data.List as L

import Data.Functor.Foldable

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
  let kd  = force . KD.kdtree 64 8 $ vs
  let kdf = force . KDF.kdtree 64 8 $ vs


  -- run benchmarks
  defaultMain

     -- search the nearest neighbor
     [ bgroup "nn"
       [ bench  "linear_nn"  $ nf (Lin.nearestNeighbor vs) q
       , bench  "kdtree_nn"  $ nf (KD.nearestNeighbor kd) q
       ]
     , bgroup "nn5"
       [ bench  "linear_nn5"  $ nf (L.take 5 . Lin.nearestNeighbors vs) q
       , bench  "kdtree_nn5"  $ nf (L.take 5 . KD.nearestNeighbors kd) q
       , bench  "kdtreef_nn5" $ nf (L.take 5 . KDF.nearestNeighbors kdf) q
       ]
     , bgroup "nr"
       [ bench "linear_nr" $ nf (Lin.pointsAround vs 1) q
       , bench "kdtree_nr" $ nf (KD.pointsAround kd 1) q
       ]
     , bgroup "build tree"
       [ bench "kdtree_build" $ nf (KD.kdtree 64 8) vs
       , bench "kdtreef_build" $ nf (KDF.kdtree 64 8) vs
       ]
     , bgroup "build and collapse tree"
       [ bench "kdtree_bandc" $ nf buildAndCollapse (q,vs)
       , bench "kdtreef_bandc" $ nf KDF.buildAndCollapseF (q,vs)
       ]
     ]

buildAndCollapse (q,vs) = KD.nearestNeighbor (KD.kdtree 64 8 vs) q

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

instance NFData a => NFData (Fix (KDF.KDTreeF V.Vector a)) where
    rnf (Fix (KDF.LeafF vs))      = rnf vs `seq` ()
    rnf (Fix (KDF.NodeF _ _ l r)) = rnf l `seq` rnf r `seq` ()

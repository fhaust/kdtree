{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Data.KDTreeF where


import qualified Data.Vector.Generic  as G
import qualified Data.Vector          as V
import qualified Data.List            as L

import Data.Function

import Linear

import Control.DeepSeq

import Data.Functor.Foldable

type Distance = Double
type Depth    = Int
type MaxDepth = Int
type MaxBucket = Int

data KDTreeF v a f = NodeF Depth a f f
                   | LeafF (v a)

  deriving (Show, Read, Eq, Functor)

--------------------------------------------------

class KDCompare a where
    kdCompare :: Int -> a -> a -> Ordering
    kdDistance :: Int -> a -> a -> Double
    realDistance :: a -> a -> Double


instance Real a => KDCompare (V3 a) where
    kdCompare d = V.fromList [compare `on` getX
                             ,compare `on` getY
                             ,compare `on` getZ
                             ] V.! (d `mod` 3)
    kdDistance d = V.fromList [(-) `on` (realToFrac . getX)
                              ,(-) `on` (realToFrac . getY)
                              ,(-) `on` (realToFrac . getZ)
                              ] V.! (d `mod` 3)
    realDistance a b = realToFrac $ qd a b
    {-# INLINE kdCompare #-}
    {-# INLINE kdDistance #-}
    {-# INLINE realDistance #-}

getX, getY, getZ :: V3 a -> a
getX (V3 x _ _) = x
getY (V3 _ y _) = y
getZ (V3 _ _ z) = z

--------------------------------------------------


kdtree :: (KDCompare a, G.Vector v a)
       => MaxBucket -> MaxDepth -> v a -> Fix (KDTreeF v a)
kdtree mb md vs = ana (kdtreeF mb md) (vs,0)

kdtreeF :: (KDCompare a, G.Vector v a)
        => MaxBucket -> MaxDepth -> (v a,Int) -> KDTreeF v a (v a,Int)
kdtreeF maxBucket maxDepth = go
  where go (vs,d) | d >= maxDepth || G.length vs <= maxBucket = LeafF (G.convert vs)
                  | otherwise = do

                      let (l,r) = splitHalfBy (kdCompare d) vs

                      -- create node
                      NodeF d (G.head r) (l,d+1) (r,d+1)


splitHalfBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> (v a, v a)
splitHalfBy f vs = G.splitAt (G.length vs `quot` 2)
                 . G.fromListN (G.length vs)
                 . L.sortBy f
                 $ G.toList vs

buildAndCollapseF :: (G.Vector v a, KDCompare a) => (a, v a) -> a
buildAndCollapseF (q,vs) = head $ hylo (nearestNeighborsF q (take 1)) (kdtreeF 64 8) (vs,0)


--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (KDCompare a, G.Vector v a)
                 => Fix (KDTreeF v a) -> a -> [a]
nearestNeighbors kdf q = cata (nearestNeighborsF q id) kdf

nearestNeighborsF :: (KDCompare a, G.Vector v a) => a -> ([a] -> [a]) -> KDTreeF v a [a] -> [a]
nearestNeighborsF q f (LeafF vs)      = f $ L.sortBy (compare `on` realDistance q) . G.toList $ vs
nearestNeighborsF q f (NodeF d n l r) = f $ uncurry (mergeBuckets q x) (if x<0 then (l,r) else (r,l))
    where x = kdDistance d n q
          {-# INLINE x #-}

{-# INLINE nearestNeighborsF #-}


-- | recursively merge the two children
-- | the second line makes sure that points in the
-- | 'safe' region are preferred
mergeBuckets :: KDCompare a => a -> Double -> [a] -> [a] -> [a]
mergeBuckets q d = go
  where go []     bs     = bs
        go (a:as) bs     | dqd a < d     = a : go as bs
        go as     []     = as
        go (a:as) (b:bs) | dqd a < dqd b = a : go as (b:bs)
                         | otherwise     = b : go (a:as) bs

        dqd = realDistance q
        {-# INLINE dqd #-}
        {-# INLINE go #-}

{-# INLINE mergeBuckets #-}


--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree
--nearestNeighbor :: (G.Vector v a, a ~ V3 Double) => KDTree v a -> V3 Double -> a
--nearestNeighbor = nearestNeighborBy id

--nearestNeighborByF :: (KDCompare c, G.Vector v c) => c -> KDTreeF v c [c] -> c
--nearestNeighborByF q = head . nearestNeighborsByF q


--------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
--pointsAround :: (G.Vector v a, a~V3 Double) => KDTree v a -> Double -> V3 Double -> [a]
--pointsAround = pointsAroundBy id

--pointsAroundBy :: (G.Vector v a, a ~ V3 Double)
--               => (a -> V3 Double) -> KDTree v a -> Double -> V3 Double -> [a]
--pointsAroundBy f t r q = takeWhile (\p -> qd q (f p) < (r*r)) . nearestNeighborsBy f t $ q

--------------------------------------------------

--------------------------------------------------

instance (NFData (v a), NFData a, NFData f) => NFData (KDTreeF v a f) where
  rnf (LeafF   vs)    = rnf vs `seq` ()
  rnf (NodeF _ _ l r) = rnf l `seq` rnf r `seq` ()


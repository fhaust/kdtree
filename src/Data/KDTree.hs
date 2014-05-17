{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.KDTree where


import qualified Data.Vector.Generic  as G
import qualified Data.List            as L

import Data.Function

import Linear

import Control.DeepSeq
import Control.Applicative

import Data.Functor.Foldable

import Data.KDTree.Common

--------------------------------------------------

-- | define a kd tree
--   planes are seperated by point + normal
data KDTree v a = Node (V3 Double) (V3 Double) (KDTree v a) (KDTree v a)
                | Leaf (v a)
  deriving (Show, Read, Eq)


-- | define the fix point variant of KDTree
data KDTreeF v a f = NodeF (V3 Double) (V3 Double) f f | LeafF (v a)
  deriving (Show, Read, Eq, Functor)


-- implement Base, Foldable and Unfoldable for KDTree
type instance Base (KDTree v a) = KDTreeF v a

instance Foldable (KDTree v a) where
  project (Leaf a)       = LeafF a
  project (Node p n l r) = NodeF p n l r

instance Unfoldable (KDTree v a) where
  embed (LeafF a)       = Leaf a
  embed (NodeF p n l r) = Node p n l r

---

instance (NFData (v a), NFData a) => NFData (KDTree v a) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node _ _ l r) = rnf l `seq` rnf r `seq` ()

--------------------------------------------------

newtype MinBucket = MinBucket Int
  deriving (Eq,Ord,Show,Read,Num)

newtype MaxDepth = MaxDepth Int
  deriving (Eq,Ord,Show,Read,Num)

--------------------------------------------------

kdtree :: (G.Vector v a, a ~ V3 Double) => MinBucket -> MaxDepth -> v a -> KDTree v a
kdtree mb md vs = ana (kdtreeF mb) (md,vs)

kdtreeF :: (G.Vector v a, a ~ V3 Double)
          => MinBucket -> (MaxDepth,v a) -> KDTreeF v a (MaxDepth,v a)
kdtreeF (MinBucket b) = go
  where go (MaxDepth d,fs) | d < 1 || G.length fs <= b = LeafF (G.convert fs)
                           | otherwise = NodeF p n (MaxDepth $ d-1,l) (MaxDepth $ d-1,r)
                               where n     = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)
                                     p     = mean fs
                                     (l,r) = G.unstablePartition (\x -> distPlanePoint p n x < 0) fs


{-# INLINE kdtreeF #-}

--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (G.Vector v a, a~V3 Double) => V3 Double -> KDTree v (V3 Double) -> [V3 Double]
nearestNeighbors q = cata (nearestNeighborsF q)

nearestNeighborsF :: (G.Vector v a, a ~ V3 Double) => a -> KDTreeF v a [a] -> [a]
nearestNeighborsF q (LeafF vs)      = L.sortBy (compare `on` qd q) . G.toList $ vs
nearestNeighborsF q (NodeF p n l r) = if d < 0 then go l r else go r l

  where d   = distPlanePoint p n q
        go  = mergeBuckets id d q

        {-# INLINE go #-}
        {-# INLINE d #-}

{-# INLINE nearestNeighborsF #-}

--------------------------------------------------

-- | get the nearest neighbor of point q
nearestNeighbor :: (G.Vector v a, a ~ V3 Double) => V3 Double -> KDTree v a -> [a]
nearestNeighbor q = cata (nearestNeighborF q)

nearestNeighborF :: (G.Vector v a, a ~ V3 Double) => a -> KDTreeF v a [a] -> [a]
nearestNeighborF q = take 1 <$> nearestNeighborsF q

{-# INLINE nearestNeighborF #-}

----------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: (G.Vector v a, a~V3 Double) => Double -> V3 Double -> KDTree v a -> [a]
pointsAround r q = cata (pointsAroundBy r q)

pointsAroundBy :: (G.Vector v a, a ~ V3 Double)
               => Double -> V3 Double -> KDTreeF v a [a] -> [a]
pointsAroundBy r q = takeWhile (\p -> qd q p < (r*r)) <$> nearestNeighborsF q

{-# INLINE pointsAroundBy #-}
--------------------------------------------------

{-# INLINE distPlanePoint #-}
distPlanePoint :: (Metric f, Num a) => f a -> f a -> f a -> a
distPlanePoint p n x = n `dot` (x ^-^ p)

mean :: (G.Vector v a, Fractional a) => v a -> a
mean = uncurry (/) . G.foldl' (\(!s,!l) b -> (s+b,l+1)) (0,0)

stddev
  :: (Floating b, Fractional (f b), Functor f, G.Vector v (f b)) =>
     f b -> v (f b) -> f b
stddev m vs = fmap sqrt . (/ (n-1)) . G.sum . G.map (\x -> (x - m)^(2::Int)) $ vs
  where n = fromIntegral . G.length $ vs


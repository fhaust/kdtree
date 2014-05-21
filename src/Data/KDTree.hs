{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
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

import Data.Functor.Foldable

--------------------------------------------------

class KDCompare a where
  dimDistance  :: Depth -> a -> a -> Double
  realDistance :: a -> a -> Double

instance (Floating a, Real a) => KDCompare (V3 a) where
  dimDistance d (V3 ax ay az) (V3 bx by bz) = case d `mod` 3 of
                                                0 -> realToFrac $ ax - bx
                                                1 -> realToFrac $ ay - by
                                                2 -> realToFrac $ az - bz
                                                _ -> error "this shouldn't happen"
  realDistance a b = realToFrac $ distance a b

--------------------------------------------------

-- | define a kd tree
--   planes are seperated by point + normal
data KDTree v a = Node Depth a (KDTree v a) (KDTree v a)
                | Leaf (v a)
  deriving (Show, Read, Eq)


-- | define the fix point variant of KDTree
data KDTreeF v a f = NodeF Depth a f f | LeafF (v a)
  deriving (Show, Read, Eq, Functor)


-- implement Base, Foldable and Unfoldable for KDTree
type instance Base (KDTree v a) = KDTreeF v a

instance Foldable (KDTree v a) where
  project (Leaf a)       = LeafF a
  project (Node d p l r) = NodeF d p l r

instance Unfoldable (KDTree v a) where
  embed (LeafF a)       = Leaf a
  embed (NodeF d p l r) = Node d p l r

---

instance (NFData (v a), NFData a) => NFData (KDTree v a) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node _ _ l r) = rnf l `seq` rnf r `seq` ()

--------------------------------------------------

newtype BucketSize = BucketSize {unMinBucket :: Int}
  deriving (Eq,Ord,Show,Read,Num)

newtype Depth = Depth {unDepth :: Int}
  deriving (Eq,Ord,Show,Read,Num,Real,Enum,Integral)

--------------------------------------------------

empty :: (G.Vector v a) => KDTree v a
empty = Leaf G.empty

kdtree :: (G.Vector v a, a ~ V3 Double) => BucketSize -> v a -> KDTree v a
kdtree mb vs = ana (kdtreeF mb) (0,vs)

kdtreeF :: (KDCompare a, G.Vector v a)
          => BucketSize -> (Depth,v a) -> KDTreeF v a (Depth,v a)
kdtreeF (BucketSize mb) = go
  where go (d,fs) | G.length fs <= mb = LeafF (G.convert fs)
                  | otherwise         = NodeF d (G.head r) (d+1,l) (d+1,r)
                    where (l,r) = splitBuckets d fs

{-# INLINE kdtreeF #-}

splitBuckets :: (KDCompare a, G.Vector v a)
             => Depth -> v a -> (v a, v a)
splitBuckets dim vs = G.splitAt (G.length vs `quot` 2)
                    . G.fromListN (G.length vs)
                    . L.sortBy (compare `on` dimDistance dim (G.head vs))
                    $ G.toList vs

{-# INLINE splitBuckets #-}

--------------------------------------------------


-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (KDCompare a, G.Vector v a) => a -> KDTree v a -> [a]
nearestNeighbors q = cata (nearestNeighborsF q)

nearestNeighborsF :: (KDCompare a, G.Vector v a) => a -> KDTreeF v a [a] -> [a]
nearestNeighborsF q (LeafF vs)      = L.sortBy (compare `on` realDistance q) . G.toList $ vs
nearestNeighborsF q (NodeF d p l r) = if x < 0 then go l r else go r l

  where x   = dimDistance d p q
        go  = mergeBuckets x q

        {-# INLINE go #-}
        {-# INLINE x  #-}

{-# INLINE nearestNeighborsF #-}

-- recursively merge the two children
-- the second line makes sure that points in the
-- 'safe' region are prefered
mergeBuckets :: (KDCompare a) => Double -> a -> [a] -> [a] -> [a]
mergeBuckets d q = go
  where rdq = realDistance q
        go []     bs                     = bs
        go (a:as) bs     | rdq a < d     = a : go as bs
        go as     []                     = as
        go (a:as) (b:bs) | rdq a < rdq b = a : go as (b:bs)
                         | otherwise     = b : go (a:as) bs

{-# INLINE mergeBuckets #-}

--------------------------------------------------

-- | get the nearest neighbor of point q
nearestNeighbor :: (KDCompare a, G.Vector v a) => a -> KDTree v a -> [a]
nearestNeighbor q = take 1 . nearestNeighbors q

----------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: (KDCompare a, G.Vector v a) => Double -> a -> KDTree v a -> [a]
pointsAround r q = takeWhile (\p -> realDistance q p < abs r) . nearestNeighbors q

--------------------------------------------------


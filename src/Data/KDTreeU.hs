{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.KDTreeU where


import qualified Data.Vector.Generic  as G
--import qualified Data.Vector.Storable as V
import qualified Data.List            as L

import Data.Function
import Data.Functor.Foldable

import Linear

import Control.Applicative

import Data.KDTree.Common

import Data.KDTree (KDTree(..), mean, distPlanePoint)
import Data.KDTreeF2 (KDTreeF(..))


--------------------------------------------------


-- wrapping stuff

type instance Base (KDTree v a) = KDTreeF v a

instance Foldable (KDTree v a) where
  project (Leaf a)       = LeafF a
  project (Node p n l r) = NodeF p n l r

instance Unfoldable (KDTree v a) where
  embed (LeafF a)       = Leaf a
  embed (NodeF p n l r) = Node p n l r


--------------------------------------------------
-- FIXME just testing

fullNN :: (G.Vector v (V3 Double))
         => Int -> Int -> V3 Double -> v (V3 Double) -> [V3 Double]
fullNN mb md q vs = hylo (nearestNeighborF q) (kdtreeF mb) (md,vs)

fullNNS :: (G.Vector v (V3 Double))
         => Int -> Int -> Int -> V3 Double -> v (V3 Double) -> [V3 Double]
fullNNS n mb md q vs = hylo (take n <$> nearestNeighborsF q) (kdtreeF mb) (md,vs)

fullNNR :: (G.Vector v (V3 Double))
        => Double -> Int -> Int -> V3 Double -> v (V3 Double) -> [V3 Double]
fullNNR r mb md q vs = hylo (pointsAroundF r q) (kdtreeF mb) (md,vs)
--------------------------------------------------

kdtree :: (G.Vector v a, a ~ V3 Double) => Int -> Int -> v a -> KDTree v a
kdtree mb md vs = ana (kdtreeF mb) (md,vs)

kdtreeF :: (G.Vector v a, a ~ V3 Double) => Int -> (Int,v a) -> KDTreeF v a (Int,v a)
kdtreeF = kdtreeBy id

kdtreeBy :: (G.Vector v a, a ~ V3 Double)
         => (a -> V3 Double) -> Int -> (Int,v a) -> KDTreeF v a (Int,v a)
kdtreeBy f b = go
  where go (d,fs) | d < 1 || G.length fs <= b = LeafF (G.convert fs)
                  | otherwise = NodeF p n (d-1,l) (d-1,r)
                      where n     = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)
                            p     = mean . G.map f $ fs
                            (l,r) = G.unstablePartition (\x -> distPlanePoint p n (f x) < 0) fs

{-# INLINE kdtree #-}
{-# INLINE kdtreeF #-}
{-# INLINE kdtreeBy #-}


--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (G.Vector v a, a ~ V3 Double)
                 => V3 Double -> KDTree v a -> [V3 Double]
nearestNeighbors q = cata (nearestNeighborsF q)


nearestNeighborsF :: (G.Vector v a, a~V3 Double)
                  => V3 Double -> KDTreeF v (V3 Double) [V3 Double] -> [V3 Double]
nearestNeighborsF = nearestNeighborsBy id

nearestNeighborsBy :: (G.Vector v a) => (a -> V3 Double) -> V3 Double -> KDTreeF v a [a] -> [a]
nearestNeighborsBy f q (LeafF vs)      = L.sortBy (compare `on` (qd q . f)) . G.toList $ vs
nearestNeighborsBy f q (NodeF p n l r) = if d < 0 then go l r else go r l

  where d   = distPlanePoint p n q
        go  = mergeBuckets f d q

        {-# INLINE go #-}
        {-# INLINE d #-}

        ---- recursively merge the two children
        ---- the second line makes sure that points in the
        ---- 'safe' region are prefered
        --go []     bs     = bs
        --go (a:as) bs     | qdq a < (d*d) = a : go as bs
        --go as     []     = as
        --go (a:as) (b:bs) | qdq a < qdq b      = a : go as (b:bs)
        --                 | otherwise          = b : go (a:as) bs

        ---- quadratic distance to query point
        --qdq = qd q . f

{-# INLINE nearestNeighborsBy #-}
{-# INLINE nearestNeighbors #-}
{-# INLINE nearestNeighborsF #-}

--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree

nearestNeighbor :: (G.Vector v a, a ~ V3 Double) => V3 Double -> KDTree v a -> [a]
nearestNeighbor q = cata (nearestNeighborF q)


nearestNeighborF :: (G.Vector v a, a ~ V3 Double) => V3 Double -> KDTreeF v a [a] -> [a]
nearestNeighborF = nearestNeighborBy id

nearestNeighborBy :: (G.Vector v a) => (a -> V3 Double) -> V3 Double -> KDTreeF v a [a] -> [a]
nearestNeighborBy f q = fmap (take 1) (nearestNeighborsBy f q)


{-# INLINE nearestNeighborF #-}
{-# INLINE nearestNeighbor #-}
{-# INLINE nearestNeighborBy #-}

--------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'

pointsAround :: (G.Vector v a, a ~ V3 Double)
               => Double -> V3 Double -> KDTree v a -> [a]
pointsAround r q = cata (pointsAroundF r q)

pointsAroundF :: (G.Vector v a, a~V3 Double) => Double -> V3 Double -> KDTreeF v a [a] -> [a]
pointsAroundF = pointsAroundBy id

pointsAroundBy :: (G.Vector v a, a ~ V3 Double)
               => (a -> V3 Double) -> Double -> V3 Double -> KDTreeF v a [a] -> [a]
pointsAroundBy f r q = fmap (takeWhile (\p -> qd q (f p) < (r*r))) (nearestNeighborsBy f q)

{-# INLINE pointsAroundBy #-}
{-# INLINE pointsAroundF #-}
{-# INLINE pointsAround #-}

--------------------------------------------------


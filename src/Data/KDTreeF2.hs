{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.KDTreeF2 where


import qualified Data.Vector.Generic  as G
--import qualified Data.Vector.Storable as V
import qualified Data.List            as L

import Data.Function
import Data.Functor.Foldable

import Linear

import Control.DeepSeq
import Control.Applicative

import Data.KDTree.Common

type Distance = Double


data KDTreeF v a f = Node { _point  :: !(V3 Double)
                          , _normal :: !(V3 Double)
                          , _left   :: f
                          , _right  :: f
                          }
                   | Leaf { _bucket :: v a }

  deriving (Show, Read, Eq, Functor)

type KDTree v a = Fix (KDTreeF v a)


--------------------------------------------------
-- FIXME just testing

fullNN :: (G.Vector v (V3 Double))
         => Int -> Int -> v (V3 Double) -> V3 Double -> [V3 Double]
fullNN mb md vs q = hylo (nearestNeighborF q) (kdtreeF mb) (md,vs)

fullNNS :: (G.Vector v (V3 Double))
         => Int -> Int -> Int -> v (V3 Double) -> V3 Double -> [V3 Double]
fullNNS n mb md vs q = hylo (take n <$> nearestNeighborsF q) (kdtreeF mb) (md,vs)

fullNNR :: (G.Vector v (V3 Double))
        => Double -> Int -> Int -> v (V3 Double) -> V3 Double -> [V3 Double]
fullNNR r mb md vs q = hylo (pointsAroundF r q) (kdtreeF mb) (md,vs)
--------------------------------------------------

kdtree :: (G.Vector v a, a ~ V3 Double) => Int -> Int -> v a -> KDTree v a
kdtree mb md vs = ana (kdtreeF mb) (md,vs)

kdtreeF :: (G.Vector v a, a ~ V3 Double) => Int -> (Int,v a) -> KDTreeF v a (Int,v a)
kdtreeF = kdtreeBy id
{-# INLINE kdtreeF #-}

kdtreeBy :: (G.Vector v a, a ~ V3 Double)
         => (a -> V3 Double) -> Int -> (Int,v a) -> KDTreeF v a (Int,v a)
kdtreeBy f b = go
  where go (d,fs) | d < 1 || G.length fs <= b = Leaf (G.convert fs)
                  | otherwise = Node p n (d-1,l) (d-1,r)
                      where n     = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)
                            p     = mean . G.map f $ fs
                            (l,r) = G.unstablePartition (\x -> distPlanePoint p n (f x) < 0) fs
{-# INLINE kdtreeBy #-}

--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (G.Vector v a, a ~ V3 Double)
                 => V3 Double -> KDTree v a -> [V3 Double]
nearestNeighbors q = cata (nearestNeighborsF q)
{-# INLINE nearestNeighbors #-}


nearestNeighborsF :: (G.Vector v a, a~V3 Double)
                  => V3 Double -> KDTreeF v (V3 Double) [V3 Double] -> [V3 Double]
nearestNeighborsF = nearestNeighborsBy id
{-# INLINE nearestNeighborsF #-}

nearestNeighborsBy :: (G.Vector v a) => (a -> V3 Double) -> V3 Double -> KDTreeF v a [a] -> [a]
nearestNeighborsBy f q (Leaf vs)      = L.sortBy (compare `on` (qd q . f)) . G.toList $ vs
nearestNeighborsBy f q (Node p n l r) = if d < 0 then go l r else go r l

  where d   = distPlanePoint p n q
        go  = mergeBuckets f d q

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

--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree

nearestNeighbor :: (G.Vector v a, a ~ V3 Double) => V3 Double -> KDTree v a -> [a]
nearestNeighbor q = cata (nearestNeighborF q)
{-# INLINE nearestNeighbor #-}


nearestNeighborF :: (G.Vector v a, a ~ V3 Double) => V3 Double -> KDTreeF v a [a] -> [a]
nearestNeighborF = nearestNeighborBy id
{-# INLINE nearestNeighborF #-}

nearestNeighborBy :: (G.Vector v a) => (a -> V3 Double) -> V3 Double -> KDTreeF v a [a] -> [a]
nearestNeighborBy f q = fmap (take 1) (nearestNeighborsBy f q)
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


--------------------------------------------------

instance (NFData (v a), NFData a, NFData f) => NFData (KDTreeF v a f) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node _ _ l r) = rnf l `seq` rnf r `seq` ()

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.KDTree where


import qualified Data.Vector.Generic  as G
--import qualified Data.Vector.Storable as V
import qualified Data.List            as L

import Data.Function

import Linear

import Control.DeepSeq

import Data.KDTree.Common

type Distance = Double

data KDTree v a = Node { _point  :: !(V3 Double)
                       , _normal :: !(V3 Double)
                       , _left   :: KDTree v a
                       , _right  :: KDTree v a
                       }
                | Leaf { _bucket :: v a }

  deriving (Show, Read, Eq)

--------------------------------------------------

kdtree :: (G.Vector v a, a ~ V3 Double) => Int -> Int -> v a -> KDTree v a
kdtree = kdtreeBy id

kdtreeBy :: (G.Vector v a, a ~ V3 Double) => (a -> V3 Double) -> Int -> Int -> v a -> KDTree v a
kdtreeBy f b d = go
  where go fs | d < 1 || G.length fs <= b = Leaf (G.convert fs)
              | otherwise = Node p n (kdtreeBy f (d-1) b l) (kdtreeBy f (d-1) b r)
                  where n     = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)
                        p     = mean . G.map f $ fs
                        (l,r) = G.unstablePartition (\x -> distPlanePoint p n (f x) < 0) fs

                        -- -- could be faster?
                        --(l,r) = splitHalfBy (compare `on` \x -> distPlanePoint 0 n (f x)) fs
                        --p     = G.head r


{-# INLINE kdtree #-}
{-# INLINE kdtreeBy #-}


--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (G.Vector v a, a~V3 Double) => V3 Double -> KDTree v (V3 Double) -> [V3 Double]
nearestNeighbors = nearestNeighborsBy id

nearestNeighborsBy :: (G.Vector v a) => (a -> V3 Double) -> V3 Double -> KDTree v a -> [a]
nearestNeighborsBy f q (Leaf vs)      = L.sortBy (compare `on` (qd q . f)) . G.toList $ vs
nearestNeighborsBy f q (Node p n l r) = if d < 0 then go nnl nnr else go nnr nnl

  where d   = distPlanePoint p n q

        nnl = nearestNeighborsBy f q l
        nnr = nearestNeighborsBy f q r

        go = mergeBuckets f d q

{-# INLINE nearestNeighbors #-}
{-# INLINE nearestNeighborsBy #-}

--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree
nearestNeighbor :: (G.Vector v a, a ~ V3 Double) => V3 Double -> KDTree v a -> a
nearestNeighbor = nearestNeighborBy id

nearestNeighborBy :: (G.Vector v c) => (c -> V3 Double) -> V3 Double -> KDTree v c -> c
nearestNeighborBy f q = head . nearestNeighborsBy f q

{-# INLINE nearestNeighbor #-}
{-# INLINE nearestNeighborBy #-}

--------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: (G.Vector v a, a~V3 Double) => Double -> V3 Double -> KDTree v a -> [a]
pointsAround = pointsAroundBy id

pointsAroundBy :: (G.Vector v a, a ~ V3 Double)
               => (a -> V3 Double) -> Double -> V3 Double -> KDTree v a -> [a]
pointsAroundBy f r q t = takeWhile (\p -> qd q (f p) < (r*r)) . nearestNeighborsBy f q $ t

{-# INLINE pointsAround #-}
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


--------------------------------------------------

instance (NFData (v a), NFData a) => NFData (KDTree v a) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node _ _ l r) = rnf l `seq` rnf r `seq` ()

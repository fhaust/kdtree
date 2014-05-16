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

                        --(l,r) = splitHalfBy (compare `on` \x -> distPlanePoint 0 n (f x)) fs
                        --p     = G.head r

--splitHalfBy :: (G.Vector v a) => (a -> a -> Ordering) -> v a -> (v a, v a)
--splitHalfBy f vs = G.splitAt (G.length vs `quot` 2)
--                 . G.fromListN (G.length vs)
--                 . L.sortBy f
--                 $ G.toList vs



--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (G.Vector v a, a~V3 Double) => KDTree v (V3 Double) -> V3 Double -> [V3 Double]
nearestNeighbors = nearestNeighborsBy id

--nearestNeighborsBy :: (V.Storable a) => (a -> V3 Double) -> KDTree a -> V3 Double -> [a]
nearestNeighborsBy :: (G.Vector v a) => (a -> V3 Double) -> KDTree v a -> V3 Double -> [a]
nearestNeighborsBy f (Leaf vs)      q = L.sortBy (compare `on` (qd q . f)) . G.toList $ vs
nearestNeighborsBy f (Node p n l r) q = if d < 0 then go nnl nnr else go nnr nnl

  where d   = distPlanePoint p n q

        nnl = nearestNeighborsBy f l q
        nnr = nearestNeighborsBy f r q

        go = mergeBuckets f d q

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

--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree
nearestNeighbor :: (G.Vector v a, a ~ V3 Double) => KDTree v a -> V3 Double -> a
nearestNeighbor = nearestNeighborBy id

nearestNeighborBy :: (G.Vector v c) => (c -> V3 Double) -> KDTree v c -> V3 Double -> c
nearestNeighborBy f t = head . nearestNeighborsBy f t

--------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: (G.Vector v a, a~V3 Double) => KDTree v a -> Double -> V3 Double -> [a]
pointsAround = pointsAroundBy id

pointsAroundBy :: (G.Vector v a, a ~ V3 Double)
               => (a -> V3 Double) -> KDTree v a -> Double -> V3 Double -> [a]
pointsAroundBy f t r q = takeWhile (\p -> qd q (f p) < (r*r)) . nearestNeighborsBy f t $ q

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

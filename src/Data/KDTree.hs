{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE StandaloneDeriving #-}

module Data.KDTree where



import qualified Data.Vector.Generic  as G
import qualified Data.List            as L


import Data.Function

import Linear

import Control.DeepSeq

import Data.Functor.Foldable

--------------------------------------------------

class KDCompare a where
  data Dim a :: *

  kSucc :: Dim a -> Dim a
  kFirst :: Dim a

  dimDistance  :: Dim a -> a -> a -> Double
  realDistance :: a -> a -> Double
  dimCompare   :: Dim a -> a -> a -> Ordering


--------------------------------------------------

instance (Real a, Floating a) => KDCompare (V3 a) where

  data Dim (V3 a) = V3X | V3Y | V3Z deriving (Show,Read,Eq,Enum)

  kSucc k = case k of V3X -> V3Y; V3Y -> V3Z; V3Z -> V3X
  kFirst = V3X

  dimDistance k (V3 qx qy qz) (V3 x y z) = realToFrac $ case k of
                                                V3X -> x - qx
                                                V3Y -> y - qy
                                                V3Z -> z - qz
  realDistance a b = realToFrac $ distance a b

  dimCompare k (V3 qx qy qz) (V3 x y z) = case k of
                                                V3X -> compare x qx
                                                V3Y -> compare y qy
                                                V3Z -> compare z qz



  {-# INLINABLE kSucc #-}
  {-# INLINABLE kFirst #-}
  {-# INLINABLE dimDistance #-}
  {-# INLINABLE realDistance #-}
  {-# INLINABLE dimCompare #-}

--------------------------------------------------


-- | define a kd tree
--   planes are seperated by point + normal
data KDTree v a = Node (Dim a) a (KDTree v a) (KDTree v a)
                | Leaf (Dim a) (v a)

deriving instance (Show (v a), Show (Dim a), Show a) => Show (KDTree v a)
deriving instance (Read (v a), Read (Dim a), Read a) => Read (KDTree v a)
deriving instance (Eq (v a), Eq (Dim a), Eq a) => Eq (KDTree v a)

-- | define the fix point variant of KDTree
data KDTreeF v a f = NodeF (Dim a) a f f
                   | LeafF (Dim a) (v a)
  deriving (Functor)


-- implement Base, Foldable and Unfoldable for KDTree
type instance Base (KDTree v a) = KDTreeF v a

instance Foldable (KDTree v a) where
  project (Leaf d a)     = LeafF d a
  project (Node d p l r) = NodeF d p l r

instance Unfoldable (KDTree v a) where
  embed (LeafF d a)     = Leaf d a
  embed (NodeF d p l r) = Node d p l r

---

instance (NFData (v a), NFData a) => NFData (KDTree v a) where
  rnf (Leaf _ vs)    = rnf vs
  rnf (Node _ _ l r) = rnf l `seq` rnf r `seq` ()

--------------------------------------------------

newtype BucketSize = BucketSize {unMB :: Int}
  deriving (Eq,Ord,Show,Read,Num)

--------------------------------------------------

empty :: (KDCompare a, G.Vector v a) => KDTree v a
empty = Leaf kFirst G.empty

singleton :: (KDCompare a, G.Vector v a) => a -> KDTree v a
singleton x = Leaf kFirst (G.singleton x)

toVec :: (G.Vector v a) => KDTree v a -> v a
toVec = cata toVecF

toVecF :: (G.Vector v a) => KDTreeF v a (v a) -> (v a)
toVecF (LeafF _ xs)    = xs
toVecF (NodeF _ _ l r) = l G.++ r


toList :: (G.Vector v a) => KDTree v a -> [a]
toList = cata toListF

toListF :: (G.Vector v a) => KDTreeF v a [a] -> [a]
toListF (LeafF _ xs)    = G.toList xs
toListF (NodeF _ _ l r) = l ++ r

--------------------------------------------------

--------------------------------------------------

kdtree :: (KDCompare a, G.Vector v a) => BucketSize -> v a -> KDTree v a
kdtree mb vs = ana (kdtreeF mb) (kFirst,vs)

{-# INLINABLE kdtree #-}

kdtreeF :: (KDCompare a, G.Vector v a)
          => BucketSize -> (Dim a,v a) -> KDTreeF v a (Dim a,v a)
kdtreeF (BucketSize mb) = go
  where go (k,fs) | G.length fs <= mb = LeafF k fs
                  | otherwise         = NodeF k (G.head r) (kSucc k,l) (kSucc k,r)
                    where (l,r) = splitBuckets k fs

{-# INLINABLE kdtreeF #-}

splitBuckets :: (KDCompare a, G.Vector v a)
             => Dim a -> v a -> (v a, v a)
splitBuckets dim vs = G.splitAt (G.length vs `quot` 2)
                    . G.fromListN (G.length vs)
                    . L.sortBy (compare `on` dimDistance dim (G.head vs))
                    $ G.toList vs

{-# INLINABLE splitBuckets #-}

--------------------------------------------------


-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (KDCompare a, G.Vector v a) => a -> KDTree v a -> [a]
nearestNeighbors q = cata (nearestNeighborsF q)

{-# INLINABLE nearestNeighbors #-}

nearestNeighborsF :: (KDCompare a, G.Vector v a) => a -> KDTreeF v a [a] -> [a]
nearestNeighborsF q (LeafF _ vs)    = L.sortBy (compare `on` realDistance q) . G.toList $ vs
nearestNeighborsF q (NodeF d p l r) = if x < 0 then go l r else go r l

  where x   = dimDistance d p q
        go  = mergeBuckets x q

{-# INLINABLE nearestNeighborsF #-}

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

{-# INLINABLE mergeBuckets #-}

--------------------------------------------------

-- | get the nearest neighbor of point q
nearestNeighbor :: (KDCompare a, G.Vector v a) => a -> KDTree v a -> [a]
nearestNeighbor q = take 1 . nearestNeighbors q

{-# INLINABLE nearestNeighbor #-}

----------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: (KDCompare a, G.Vector v a) => Double -> a -> KDTree v a -> [a]
pointsAround r q = takeWhile (\p -> realDistance q p < abs r) . nearestNeighbors q

{-# INLINABLE pointsAround #-}
--------------------------------------------------


partition :: (KDCompare a, G.Vector v a, Eq (Dim a))
          => Dim a -> Ordering -> a -> KDTree v a -> (KDTree v a, KDTree v a)
partition dim ord q = go
  where go (Leaf d vs) = (Leaf d valid, Leaf d invalid)
                           where predicate       = (== ord) . dimCompare dim q
                                 (valid,invalid) = G.unstablePartition predicate vs
        go (Node d p l r) | dim /= d  = (Node d p lval rval, Node d p linv rinv)
                          | otherwise = case dimCompare dim q p of
                                          GT | ord == GT -> ( Node d p lval r
                                                            , Node d p linv empty
                                                            )
                                          LT | ord == LT -> ( Node d p l rval
                                                            , Node d p empty rinv
                                                            )
                                          _              -> ( Node d p lval rval
                                                            , Node d p linv rinv
                                                            )
          where (lval,linv) = go l
                (rval,rinv) = go r

{-# INLINABLE partition #-}

select :: (KDCompare a, G.Vector v a, Eq (Dim a))
       => Dim a -> Ordering -> a -> KDTree v a -> KDTree v a
select dim ord q = fst . partition dim ord q

{-# INLINABLE select #-}

delete :: (KDCompare a, G.Vector v a, Eq (Dim a))
       => Dim a -> Ordering -> a -> KDTree v a -> KDTree v a
delete dim ord q = snd . partition dim ord q

{-# INLINABLE delete #-}



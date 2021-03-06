{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE StandaloneDeriving #-}

module Data.KDTree where



import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.List                    as L


import Data.Function

import Control.DeepSeq
import Control.Arrow

import Data.Functor.Foldable

--------------------------------------------------

class KDCompare a where
  data Dim a :: *

  kSucc :: Dim a -> Dim a
  kFirst :: Dim a

  dimDistance :: Dim a -> a -> a -> Double
  realSqDist  :: a -> a -> Double
  dimCompare  :: Dim a -> a -> a -> Ordering


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

toVecF :: (G.Vector v a) => KDTreeF v a (v a) -> v a
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

kdtree' :: (KDCompare a, G.Vector v a) => Dim a -> BucketSize -> v a -> KDTree v a
kdtree' kf mb vs = ana (kdtreeF mb) (kf,vs)

{-# INLINABLE kdtree #-}
{-# INLINABLE kdtree' #-}

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
                    . vecSortBy (dimCompare dim)
                    $ vs

{-# INLINABLE splitBuckets #-}

--------------------------------------------------

verify :: (KDCompare a, G.Vector v a) => KDTree v a -> Bool
verify (Node d p l r) = leftValid && rightValid
  where leftSmaller  = G.all (\x -> x `comp` p == LT) $ toVec l
        rightSmaller = G.all (\x -> x `comp` p /= LT) $ toVec r
        leftValid    = verify l && leftSmaller
        rightValid   = verify r && rightSmaller
        comp = dimCompare d
verify (Leaf _ _ ) = True

--------------------------------------------------


-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: (KDCompare a, G.Vector v a) => a -> KDTree v a -> [a]
nearestNeighbors q = cata (nearestNeighborsF q)

{-# INLINABLE nearestNeighbors #-}

nearestNeighborsF :: (KDCompare a, G.Vector v a) => a -> KDTreeF v a [a] -> [a]
nearestNeighborsF q (LeafF _ vs)    = L.sortBy (compare `on` realSqDist q) . G.toList $ vs
nearestNeighborsF q (NodeF d p l r) = if x < 0 then go l r else go r l

  where x   = dimDistance d q p
        go  = mergeBuckets x q

{-# INLINABLE nearestNeighborsF #-}

-- recursively merge the two children
-- the second line makes sure that points in the
-- 'safe' region are prefered
mergeBuckets :: (KDCompare a) => Double -> a -> [a] -> [a] -> [a]
mergeBuckets d q = go
  where rdq = realSqDist q
        go []     bs                     = bs
        go (a:as) bs     | rdq a < d*d   = a : go as bs
        go as     []                     = as
        go (a:as) (b:bs) | rdq a < rdq b = a : go as (b:bs)
                         | otherwise     = b : go (a:as) bs

{-# INLINABLE mergeBuckets #-}

--------------------------------------------------

-- | get the nearest neighbor of point q
nearestNeighbor :: (KDCompare a, G.Vector v a) => a -> KDTree v a -> a
nearestNeighbor q = head . nearestNeighbors q

{-# INLINABLE nearestNeighbor #-}

----------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: (KDCompare a, G.Vector v a) => Double -> a -> KDTree v a -> [a]
pointsAround r q = takeWhile (\p -> realSqDist q p < r*r) . nearestNeighbors q

{-# INLINABLE pointsAround #-}
--------------------------------------------------


partition :: (KDCompare a, G.Vector v a, Eq (Dim a))
          => Dim a -> Ordering -> a -> KDTree v a -> (KDTree v a, KDTree v a)
partition dim ord q = go
  where go (Leaf d vs) = (Leaf d valid, Leaf d invalid)
                           where predicate       = (== ord) . flip (dimCompare dim) q
                                 (valid,invalid) = G.unstablePartition predicate vs
        go (Node d p l r) | dim /= d  = (Node d p lval rval, Node d p linv rinv)
                          | otherwise = case ord of
                                          GT -> case dimCompare dim q p of
                                                 LT -> ( Node d p lval r
                                                       , Node d p linv empty
                                                       )
                                                 GT -> ( Node d p empty rval
                                                       , Node d p l     rinv
                                                       )
                                                 EQ -> ( Node d p empty r
                                                       , Node d p l     empty
                                                       )
                                          LT -> case dimCompare dim q p of
                                                 LT -> ( Node d p lval empty
                                                       , Node d p linv r
                                                       )
                                                 GT -> ( Node d p l     rval
                                                       , Node d p empty rinv
                                                       )
                                                 EQ -> ( Node d p l     empty
                                                       , Node d p empty r
                                                       )
                                          EQ -> case dimCompare dim q p of
                                                 LT -> ( Node d p lval empty
                                                       , Node d p linv r
                                                       )
                                                 _  -> ( Node d p empty rval
                                                       , Node d p l     rinv
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



--------------------------------------------------------------------------------

merge :: (Eq (Dim a), G.Vector v a, KDCompare a)
      => BucketSize -> KDTree v a -> KDTree v a -> KDTree v a
merge (BucketSize bs) = go
  where go (Node d p l r) kd = Node d p (go l l') (go r r')
          where (l',r') = partition d LT p kd

        go (Leaf ad avs) (Leaf _ bvs) | G.length avs + G.length bvs < bs = Leaf ad (avs G.++ bvs)
                                      | otherwise = kdtree' ad (BucketSize bs) (avs G.++ bvs)

        go kd (Node d p l r) = Node d p (go l l') (go r r')
          where (l',r') = partition d LT p kd

{-# INLINABLE merge #-}

--------------------------------------------------------------------------------

update :: (KDCompare a, G.Vector v a, Eq (Dim a))
       => BucketSize -> Dim a -> Ordering -> a -> (a -> a) -> KDTree v a -> KDTree v a
update bs dim ord q f = uncurry (merge bs)
                      . first (kdtree bs . G.map f . toVec)
                      . partition dim ord q

{-# INLINABLE update #-}

--------------------------------------------------------------------------------
-- mostly util stuff here

pretty :: (G.Vector v a, Show (v a), Show (Dim a), Show a) => KDTree v a -> String
pretty = go 0
  where go d (Leaf p xs)    = replicate (2*d) ' '
                              ++ "Leaf " ++ show p ++ " "
                              ++ G.foldl' (\acc x -> acc
                                                     ++ replicate (2*d + 2) ' '
                                                     ++ show x ++ "\n") "\n" xs
        go d (Node k p l r) = replicate (2*d) ' '
                              ++ "Node " ++ show k ++ " " ++ show p ++ "\n"
                              ++ go (d+1) l
                              ++ go (d+1) r

vecSortBy :: G.Vector v a => I.Comparison a -> v a -> v a
vecSortBy f = G.modify (I.sortBy f)

{-# INLINABLE vecSortBy #-}

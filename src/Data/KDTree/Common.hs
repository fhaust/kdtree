
module Data.KDTree.Common where

import Linear


-- recursively merge the two children
-- the second line makes sure that points in the
-- 'safe' region are prefered
mergeBuckets :: (Num b, Ord b, Metric f) => (a -> f b) -> b -> f b -> [a] -> [a] -> [a]
mergeBuckets f d q = go
  where qdq = qd q . f
        go []     bs     = bs
        go (a:as) bs     | qdq a < (d*d) = a : go as bs
        go as     []     = as
        go (a:as) (b:bs) | qdq a < qdq b      = a : go as (b:bs)
                         | otherwise          = b : go (a:as) bs

{-# INLINE mergeBuckets #-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import qualified Data.Vector.Storable as V
import qualified Data.List as L

import Control.Applicative
import Control.Arrow

import qualified Data.KDTree                    as KD
import qualified Data.KDTree.Internal.Common    as KD
import qualified Data.KDTree.Internal.LinSearch as LS

import Linear

--------------------------------------------------
-- Arbitrary instances

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance (V.Storable a, Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList . getNonEmpty <$> arbitrary
  shrink v = [V.init v, V.tail v]

instance Arbitrary KD.BucketSize where
  arbitrary = KD.BucketSize . getPositive <$> arbitrary

instance Arbitrary (KD.Dim KD.V3D) where
  arbitrary = elements [KD.V3X, KD.V3Y, KD.V3Z]

--------------------------------------------------
-- tree building properties
prop_verify :: (KD.BucketSize, KD.VV3D) -> Bool
prop_verify (b,vs) = KD.verify . KD.kdtree b $ vs

--------------------------------------------------
-- nearest neighbor properties


prop_nns_leaf :: (V3 Double,KD.VV3D) -> Bool
prop_nns_leaf (p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors p . KD.kdtree 1000000 $ vs
        linSearch  = LS.nearestNeighbors p vs

prop_nns_node :: (V3 Double,KD.VV3D) -> Bool
prop_nns_node (p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors p . KD.kdtree 1 $ vs
        linSearch  = LS.nearestNeighbors p vs

--------------------------------------------------

prop_nns :: (KD.BucketSize,V3 Double,KD.VV3D) -> Bool
prop_nns (b,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors p . KD.kdtree b $ vs
        linSearch  = LS.nearestNeighbors p vs

prop_nn :: (KD.BucketSize,V3 Double,KD.VV3D) -> Bool
prop_nn (b,p,vs) = head treeSearch == linSearch
  where treeSearch = KD.nearestNeighbor p . KD.kdtree b $ vs
        linSearch  = LS.nearestNeighbor p vs



prop_nr :: (KD.BucketSize,V3 Double,Double,KD.VV3D) -> Bool
prop_nr (b,p,r,vs) = treeSearch == linSearch
  where treeSearch = KD.pointsAround r p . KD.kdtree b $ vs
        linSearch  = LS.pointsAround r p vs

--------------------------------------------------

prop_partition :: (KD.Dim KD.V3D, Ordering, KD.BucketSize, KD.V3D, KD.VV3D) -> Bool
prop_partition (dim,ord,bs,q,vs) = (L.null $ tsA L.\\ lsA) && (L.null $ tsB L.\\ lsB)
  where (tsA, tsB) = both KD.toList
                   . KD.partition dim ord q
                   $ KD.kdtree bs vs
        (lsA, lsB) = both V.toList . LS.partition dim ord q $ vs
        both f = f *** f

prop_partition_leaf :: (KD.Dim KD.V3D, Ordering, KD.V3D, KD.VV3D) -> Bool
prop_partition_leaf (dim,ord,q,vs) = prop_partition (dim,ord,1000000,q,vs)

prop_partition_node :: (KD.Dim KD.V3D, Ordering, KD.V3D, KD.VV3D) -> Bool
prop_partition_node (dim,ord,q,vs) = prop_partition (dim,ord,1,q,vs)

--------------------------------------------------

prop_select :: (KD.Dim KD.V3D, Ordering, KD.BucketSize, KD.V3D, KD.VV3D) -> Bool
prop_select (dim,ord,bs,q,vs) = L.null $ treeSearch L.\\ linSearch
  where treeSearch = KD.toList
                   . KD.select dim ord q
                   $ KD.kdtree bs vs
        linSearch  = V.toList . LS.select dim ord q $ vs

prop_delete :: (KD.Dim KD.V3D, Ordering, KD.BucketSize, KD.V3D, KD.VV3D) -> Bool
prop_delete (dim,ord,bs,q,vs) = L.null $ treeSearch L.\\ linSearch
  where treeSearch = KD.toList
                   . KD.delete dim ord q
                   $ KD.kdtree bs vs
        linSearch  = V.toList . LS.delete dim ord q $ vs


--------------------------------------------------



--------------------------------------------------
-- main function / test generator

main :: IO ()
main = $defaultMainGenerator

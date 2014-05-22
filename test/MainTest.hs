
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import qualified Data.Vector as V

import Control.Applicative

import qualified Data.KDTree    as KD
import qualified Data.LinSearch as LS

import Linear

--------------------------------------------------
-- Arbitrary instances

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList . getNonEmpty <$> arbitrary

instance Arbitrary KD.BucketSize where
  arbitrary = KD.BucketSize . getPositive <$> arbitrary

--------------------------------------------------
-- properties


prop_nn :: (KD.BucketSize,V3 Double,V.Vector (V3 Double)) -> Bool
prop_nn (b,p,vs) = head treeSearch == linSearch
  where treeSearch = KD.nearestNeighbor p . KD.kdtree b $ vs
        linSearch  = LS.nearestNeighbor p vs

prop_nns :: (KD.BucketSize,V3 Double,V.Vector (V3 Double)) -> Bool
prop_nns (b,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors p . KD.kdtree b $ vs
        linSearch  = LS.nearestNeighbors p vs


prop_nr :: (KD.BucketSize,V3 Double,Double,V.Vector (V3 Double)) -> Bool
prop_nr (b,p,r,vs) = treeSearch == linSearch
  where treeSearch = KD.pointsAround r p . KD.kdtree b $ vs
        linSearch  = LS.pointsAround r p vs

--------------------------------------------------
-- main function / test generator

main :: IO ()
main = $defaultMainGenerator

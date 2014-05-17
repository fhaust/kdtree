
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

instance Arbitrary KD.MinBucket where
instance Arbitrary KD.MaxDepth where

--------------------------------------------------
-- properties


prop_nn :: (NonNegative Int,NonNegative Int,V3 Double,V.Vector (V3 Double)) -> Bool
prop_nn (NonNegative b,NonNegative d,p,vs) = head treeSearch == linSearch
  where treeSearch = KD.nearestNeighbor p . KD.kdtree (KD.MinBucket b) (KD.MaxDepth d) $ vs
        linSearch  = LS.nearestNeighbor vs p

prop_nns :: (NonNegative Int,NonNegative Int,V3 Double,V.Vector (V3 Double)) -> Bool
prop_nns (NonNegative b,NonNegative d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors p . KD.kdtree (KD.MinBucket b) (KD.MaxDepth d) $ vs
        linSearch  = LS.nearestNeighbors vs p


prop_nr :: (NonNegative Int,NonNegative Int,V3 Double,Double,V.Vector (V3 Double)) -> Bool
prop_nr (NonNegative b,NonNegative d,p,r,vs) = treeSearch == linSearch
  where treeSearch = KD.pointsAround r p . KD.kdtree (KD.MinBucket b) (KD.MaxDepth d) $ vs
        linSearch  = LS.pointsAround vs r p

--------------------------------------------------
-- main function / test generator

main :: IO ()
main = $defaultMainGenerator

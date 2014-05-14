
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import qualified Data.Vector as V

import Control.Applicative

import qualified Data.KDTree    as KD
import qualified Data.KDTreeF2   as KDF
import qualified Data.LinSearch as LS

import Data.Functor.Foldable

import Linear

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList . getNonEmpty <$> arbitrary

type MinBucket = NonNegative Int
type MaxDepth  = NonNegative Int



prop_nn :: (MinBucket,MaxDepth,V3 Double,V.Vector (V3 Double)) -> Bool
prop_nn (NonNegative b,NonNegative d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbor (KD.kdtree d b vs) p
        linSearch  = LS.nearestNeighbor vs p

prop_f_nn :: (MinBucket,MaxDepth,V3 Double,V.Vector (V3 Double)) -> Bool
prop_f_nn (NonNegative b,NonNegative d,p,vs) = head treeSearch == linSearch
  where treeSearch = KDF.nearestNeighbor p . KDF.kdtree b d $ vs
        linSearch  = LS.nearestNeighbor vs p



prop_nns :: (MinBucket,MaxDepth,V3 Double,V.Vector (V3 Double)) -> Bool
prop_nns (NonNegative b,NonNegative d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors (KD.kdtree d b vs) p
        linSearch  = LS.nearestNeighbors vs p

prop_f_nns :: (MinBucket,MaxDepth,V3 Double,V.Vector (V3 Double)) -> Bool
prop_f_nns (NonNegative b,NonNegative d,p,vs) = treeSearch == linSearch
  where treeSearch = KDF.nearestNeighbors p . KDF.kdtree b d $ vs
        linSearch  = LS.nearestNeighbors vs p




prop_nr :: (MinBucket,MaxDepth,V3 Double,Double,V.Vector (V3 Double)) -> Bool
prop_nr (NonNegative b,NonNegative d,p,r,vs) = treeSearch == linSearch
  where treeSearch = KD.pointsAround (KD.kdtree d b vs) r p
        linSearch  = LS.pointsAround vs r p

prop_f_nr :: (MinBucket,MaxDepth,V3 Double,Double,V.Vector (V3 Double)) -> Bool
prop_f_nr (NonNegative b,NonNegative d,p,r,vs) = treeSearch == linSearch
  where treeSearch = KDF.pointsAround r p . KDF.kdtree b d $ vs
        linSearch  = LS.pointsAround vs r p




main :: IO ()
main = $defaultMainGenerator

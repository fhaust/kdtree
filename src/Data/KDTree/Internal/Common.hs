{-# LANGUAGE TypeFamilies #-}

module Data.KDTree.Internal.Common where

import qualified Data.Vector.Storable as V

import Linear

import Data.KDTree



--------------------------------------------------------------------------------
-- Some type aliases

type V3D = V3 Double
type VV3D = V.Vector V3D

--------------------------------------------------------------------------------
-- KDCompare instance for V3s

instance (Real a, Floating a) => KDCompare (V3 a) where

  data Dim (V3 a) = V3X | V3Y | V3Z deriving (Show,Read,Eq,Enum)

  kSucc k = case k of V3X -> V3Y; V3Y -> V3Z; V3Z -> V3X
  kFirst = V3X

  dimDistance k (V3 ax ay az) (V3 bx by bz) = realToFrac $ case k of
                                                V3X -> ax - bx
                                                V3Y -> ay - by
                                                V3Z -> az - bz
  realSqDist a b = realToFrac $ qd a b

  dimCompare k (V3 ax ay az) (V3 bx by bz) = case k of
                                                V3X -> compare ax bx
                                                V3Y -> compare ay by
                                                V3Z -> compare az bz



  {-# INLINABLE kSucc #-}
  {-# INLINABLE kFirst #-}
  {-# INLINABLE dimDistance #-}
  {-# INLINABLE realSqDist #-}
  {-# INLINABLE dimCompare #-}

--------------------------------------------------------------------------------

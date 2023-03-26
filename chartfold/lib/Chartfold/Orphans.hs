{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports orphan instances for third-party libraries.
module Chartfold.Orphans () where

import Data.AdditiveGroup (AdditiveGroup(..))
import Data.AffineSpace (AffineSpace(..))
import Data.Colour qualified as Co
import Data.Colour.SRGB qualified as Co
import Data.Time qualified as Time

-- | Orphan.
instance AdditiveGroup Time.NominalDiffTime where
  zeroV = 0
  {-# INLINE zeroV #-}
  negateV = negate
  {-# INLINE negateV #-}
  (^+^) = (+)
  {-# INLINE (^+^) #-}
  (^-^) = (-)
  {-# INLINE (^-^) #-}

-- | Orphan.
instance AffineSpace Time.UTCTime where
  type Diff Time.UTCTime = Time.NominalDiffTime
  (.-.) = Time.diffUTCTime
  {-# INLINE (.-.) #-}
  (.+^) = flip Time.addUTCTime
  {-# INLINE (.+^) #-}

-- | Orphan.
instance (Ord a, Floating a) => Ord (Co.AlphaColour a) where
  {-# INLINE compare #-}
  compare x y = let Co.RGB rx gx bx = Co.toSRGB (Co.over x Co.black)
                    Co.RGB ry gy by = Co.toSRGB (Co.over y Co.black)
                in compare (rx, gx, bx, Co.alphaChannel x)
                           (ry, gy, by, Co.alphaChannel y)


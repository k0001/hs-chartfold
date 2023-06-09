{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports orphan instances for third-party libraries.
module Chartfold.Backend.Chart.Orphans () where

import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time

import Graphics.Rendering.Chart.Axis.Floating ()
import Graphics.Rendering.Chart.Axis.Time qualified as C
import Graphics.Rendering.Chart.Axis.Types qualified as C
import Graphics.Rendering.Chart.Backend.Types qualified as C
import Graphics.Rendering.Chart.Plot.Types qualified as C

import Chartfold.Constraint (Entails)

--------------------------------------------------------------------------------

-- | Orphan.
deriving newtype instance Ord C.FillStyle

-- | Orphan.
deriving stock instance Ord C.LineStyle

-- | Orphan. Defers all work to the 'Double' instance.
instance C.PlotValue Rational where
  {-# INLINE toValue #-}
  toValue = fromRational
  {-# INLINE fromValue#-}
  fromValue = toRational
  {-# INLINE autoAxis #-}
  autoAxis = g . C.autoAxis . fmap fromRational
    where g :: C.AxisData Double -> C.AxisData Rational
          g = \ad -> ad
            { C._axis_viewport = \r x -> C._axis_viewport ad r (fromRational x)
            , C._axis_tropweiv = \r d -> toRational (C._axis_tropweiv ad r d)
            , C._axis_ticks = first toRational <$> C._axis_ticks ad
            , C._axis_labels = fmap (first toRational) <$> C._axis_labels ad
            , C._axis_grid = toRational <$> C._axis_grid ad
            }

-- | Orphan.

instance C.TimeValue Time.NominalDiffTime where
  utctimeFromTV = Time.posixSecondsToUTCTime
  {-# INLINE utctimeFromTV #-}
  tvFromUTCTime = Time.utcTimeToPOSIXSeconds
  {-# INLINE tvFromUTCTime #-}
  doubleFromTimeValue = realToFrac
  {-# INLINE doubleFromTimeValue #-}
  timeValueFromDouble = fromRational . toRational
  {-# INLINE timeValueFromDouble #-}

-- | Orphan.
instance C.PlotValue Time.NominalDiffTime where
  toValue = C.doubleFromTimeValue
  {-# INLINE toValue #-}
  fromValue = C.timeValueFromDouble
  {-# INLINE fromValue#-}
  autoAxis = C.autoTimeValueAxis -- NOTE: This is really slow. It converts to UTCTime internally.
  {-# INLINE autoAxis #-}

-- | Orphan.
instance Semigroup (C.Plot x y) where
  (<>) = C.joinPlot
  {-# INLINE (<>) #-}

-- | Orphan.
instance Monoid (C.Plot x y) where
  mempty = C.Plot { C._plot_render = \_ -> pure ()
                  , C._plot_legend = []
                  , C._plot_all_points = ([], [])
                  }

-- | Orphan.
instance (C.PlotValue a) => Entails c (C.PlotValue a)



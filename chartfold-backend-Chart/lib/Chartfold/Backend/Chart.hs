{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Chartfold.Backend.Chart
  ( C
  , plotXCharts
  , plotXChart
  , plotChart
  , plotLine
  , plotHLine
  , plotVLine
  , plotFill
  , plotCandle
  ) where

import Control.Lens (set, mapped, _1)
import Data.Constraint
import Data.Default.Class (Default(..))
import Data.AffineSpace (AffineSpace(..))
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Rendering.Chart.Axis.Types as G
import qualified Graphics.Rendering.Chart.Backend.Types as G
import qualified Graphics.Rendering.Chart.Geometry as G
import qualified Graphics.Rendering.Chart.Layout as G
import qualified Graphics.Rendering.Chart.Legend as G
import qualified Graphics.Rendering.Chart.Plot.Candle as G
import qualified Graphics.Rendering.Chart.Plot.Lines as G
import qualified Graphics.Rendering.Chart.Plot.FillBetween as G
import qualified Graphics.Rendering.Chart.Plot.Types as G

import qualified Chartfold.Candle as Candle
import qualified Chartfold.Chart as Chart
import qualified Chartfold.Fill as Fill
import qualified Chartfold.HLine as HLine
import qualified Chartfold.Line as Line
import qualified Chartfold.VLine as VLine
import qualified Chartfold.XChart as XChart
import qualified Chartfold.XCharts as XCharts
import Chartfold.Constraint (Entails(..), Entails1)

import Chartfold.Backend.Chart.Orphans ()

--------------------------------------------------------------------------------

-- | As a convenience, if you plan to use 'plotXCharts' or 'plotChart',
-- you can use 'C' as the @c@ parameter of @'XCharts.XCharts' x c@
-- or @'XChart.XChart' x c@.
--
-- Otherwise, define your own class, but be sure to create the corresponding
-- 'Entails' instances.
class (Typeable y, Eq y, Show y, G.PlotValue y) => C y
instance (Typeable y, Eq y, Show y, G.PlotValue y) => C y

-- instance C y => Entails (C y) (Typeable y)
-- instance C y => Entails (C y) (Eq y)
-- instance C y => Entails (C y) (Show y)
-- instance C y => Entails (C y) (G.PlotValue y)

--------------------------------------------------------------------------------

plotXCharts
  :: forall c x
  .  ( G.PlotValue x
     , AffineSpace x
     , Fractional (Diff x)
     , Entails1 c G.PlotValue )
  => XCharts.XCharts x c
  -> G.StackedLayouts x
plotXCharts a = G.StackedLayouts
  { G._slayouts_layouts = plotXChart <$> IntMap.elems a.charts
  , G._slayouts_compress_legend = False
  }

plotXChart
  :: forall c x
  .  ( G.PlotValue x
     , AffineSpace x
     , Fractional (Diff x)
     , Entails1 c G.PlotValue )
  => XChart.XChart x c
  -> G.StackedLayout x
plotXChart xc = XChart.withXChart xc $ \(c :: Chart.Chart x y) ->
                withDict (entails :: c y :- G.PlotValue y) $
                G.StackedLayout (plotChart c)

plotChart
  :: (G.PlotValue x, G.PlotValue y, AffineSpace x, Fractional (Diff x))
  => Chart.Chart x y -> G.Layout x y
plotChart a = def
  { G._layout_title = T.unpack a.config.title
  , G._layout_legend = Just $ def
      { G._legend_position    = G.LegendRight
      , G._legend_orientation = G.LORows 1
      }
  , G._layout_plots = mconcat
      [ plotCandle <$> IntMap.elems a.candles
      , plotVLine  <$> IntMap.elems a.vLines
      , plotFill   <$> IntMap.elems a.fills
      , plotLine   <$> IntMap.elems a.lines
      , plotHLine  <$> IntMap.elems a.hLines
      ]
  }

plotLine :: forall x y. Line.Line x y -> G.Plot x y
plotLine a =
  let title = T.unpack a.config.title
      m1 :: Map Line.Style [(x, y)]
      m1 = Map.foldrWithKey (\x (ls, y) -> Map.insertWith mappend ls [(x, y)])
                            mempty a.info
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (ls, xys) <- Map.toAscList m1
       pure $ G.toPlot $ def
         { G._plot_lines_title  = title
         , G._plot_lines_style  = fromLineStyle ls
         , G._plot_lines_values = [xys]
         }

plotHLine :: forall x y. HLine.HLine x y -> G.Plot x y
plotHLine a = case a.info of
  Nothing -> mempty
  Just (ls, y) -> G.toPlot $ def
    { G._plot_lines_title = T.unpack a.config.title
    , G._plot_lines_style = fromHLineStyle ls
    , G._plot_lines_limit_values =
        [[ (G.LMin, G.LValue y), (G.LMax, G.LValue y) ]]
    }

plotVLine :: forall x y. VLine.VLine x y -> G.Plot x y
plotVLine a =
  let title = T.unpack a.config.title
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (ls, sx) <- Map.toAscList a.info
       pure $ G.toPlot $ def
         { G._plot_lines_title        = title
         , G._plot_lines_style        = fromVLineStyle ls
         , G._plot_lines_limit_values =
              fmap (\x -> [(G.LValue x, G.LMin), (G.LValue x, G.LMax)])
                   (Set.toAscList sx)
         }

plotFill :: forall x y. Fill.Fill x y -> G.Plot x y
plotFill a =
  let title = T.unpack a.config.title
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (s, m) <- Map.toAscList a.info
       pure $ G.toPlot $ def
         { G._plot_fillbetween_title  = title
         , G._plot_fillbetween_style  = fromFillStyle s
         , G._plot_fillbetween_values = do
             (x, sys) <- Map.toAscList m
             ys <- Set.toList sys
             pure (x, ys)
         }

plotCandle :: forall x y
           .  (AffineSpace x, Fractional (Diff x))
           => Candle.Candle x y -> G.Plot x y
plotCandle a =
  let title = T.unpack a.config.title
      m1 :: Map Candle.Style [G.Candle x y]
      m1 = Map.foldrWithKey (\(s, e) (sty, o, h, l, c) ->
                                Map.insertWith mappend sty [f s e o h l c])
                            mempty a.info
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (sty, cs) <- Map.toAscList m1
       let (ls, fs) = fromCandleStyle sty
       pure $ G.toPlot $ def
         { G._plot_candle_title           = title
         , G._plot_candle_fill            = True
         , G._plot_candle_line_style      = ls
         , G._plot_candle_rise_fill_style = fs
         , G._plot_candle_fall_fill_style = fs
         , G._plot_candle_values          = cs
         , G._plot_candle_centre          = 0 -- See note [candle_mid].
         }
  where
    f :: x -> x -> y -> y -> y -> y -> G.Candle x y
    f s e o h l c =
       let !x = s .+^ ((e .-. s) / 2)
       in G.Candle { G.candle_x     = x
                   , G.candle_open  = o
                   , G.candle_high  = h
                   , G.candle_low   = l
                   , G.candle_close = c
                   , G.candle_mid   = c -- Note [candle_mid]: Dummy value. Not
                                        -- used during rendering if
                                        -- G._plot_candle_centre is set to 0.
                   }

--------------------------------------------------------------------------------


fromLineStyle :: Line.Style -> G.LineStyle
fromLineStyle a = def
  { G._line_color  = a.color
  , G._line_width  = a.width
  , G._line_dashes = a.dashes
  , G._line_cap    = fromLineStyleCap a.cap
  , G._line_join   = fromLineStyleJoin a.join
  }

fromLineStyleCap :: Line.StyleCap -> G.LineCap
fromLineStyleCap = \case
  Line.StyleCapButt   -> G.LineCapButt
  Line.StyleCapRound  -> G.LineCapRound
  Line.StyleCapSquare -> G.LineCapSquare

fromLineStyleJoin :: Line.StyleJoin -> G.LineJoin
fromLineStyleJoin = \case
  Line.StyleJoinMiter -> G.LineJoinMiter
  Line.StyleJoinRound -> G.LineJoinRound
  Line.StyleJoinBevel -> G.LineJoinBevel

fromHLineStyle :: HLine.Style -> G.LineStyle
fromHLineStyle a = def
  { G._line_color  = a.color
  , G._line_width  = a.width
  , G._line_dashes = a.dashes
  }

fromVLineStyle :: VLine.Style -> G.LineStyle
fromVLineStyle a = def
  { G._line_color  = a.color
  , G._line_width  = a.width
  , G._line_dashes = a.dashes
  }

fromFillStyle :: Fill.Style -> G.FillStyle
fromFillStyle a = def
  { G._fill_color = a.color
  }

fromCandleStyle :: Candle.Style -> (G.LineStyle, G.FillStyle)
fromCandleStyle a =
  ( def { G._line_color  = a.lineColor
        , G._line_width  = a.lineWidth
        , G._line_dashes = a.lineDashes
        }
  , def { G._fill_color  = a.fillColor
        }
  )

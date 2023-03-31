{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Chartfold.Backend.Chart
  ( plotXCharts
  , plotXChart
  , plotChart
  , plotLine
  , plotHLine
  , plotVLine
  , plotFill
  , plotCandle
  ) where

import Control.Lens (set, mapped, _1)
import Control.Parallel.Strategies
import Data.AffineSpace (AffineSpace(..))
import Data.Constraint
import Data.Default.Class (Default(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Graphics.Rendering.Chart.Axis.Types qualified as G
import Graphics.Rendering.Chart.Backend.Types qualified as G
import Graphics.Rendering.Chart.Geometry qualified as G
import Graphics.Rendering.Chart.Layout qualified as G
import Graphics.Rendering.Chart.Legend qualified as G
import Graphics.Rendering.Chart.Plot.Candle qualified as G
import Graphics.Rendering.Chart.Plot.FillBetween qualified as G
import Graphics.Rendering.Chart.Plot.Lines qualified as G
import Graphics.Rendering.Chart.Plot.Types qualified as G

import Chartfold.Candle (Candle)
import Chartfold.Candle qualified as Candle
import Chartfold.Chart (Chart)
import Chartfold.Constraint (Entails(..), Entails1)
import Chartfold.Extra (Interval, OHLC)
import Chartfold.Fill (Fill)
import Chartfold.Fill qualified as Fill
import Chartfold.HLine (HLine)
import Chartfold.HLine qualified as HLine
import Chartfold.Line (Line)
import Chartfold.Line qualified as Line
import Chartfold.VLine (VLine)
import Chartfold.VLine qualified as VLine
import Chartfold.XChart (XChart, pattern XChart)
import Chartfold.XCharts (XCharts)
import Chartfold.XCharts qualified as XCharts

import Chartfold.Backend.Chart.Orphans ()

--------------------------------------------------------------------------------

plotXCharts
  :: forall s x c
  .  ( G.PlotValue x
     , AffineSpace x
     , Fractional (Diff x)
     , Entails1 c G.PlotValue )
  => XCharts s x c
  -> G.StackedLayouts x
plotXCharts a = G.StackedLayouts
  { G._slayouts_layouts = runEval $ XCharts.traverse (rpar . plotXChart) a
  , G._slayouts_compress_legend = False
  }

plotXChart
  :: forall s x c
  .  ( G.PlotValue x
     , AffineSpace x
     , Fractional (Diff x)
     , Entails1 c G.PlotValue )
  => XChart s x c
  -> G.StackedLayout x
plotXChart (XChart (c :: Chart s' x y)) =
  withDict (entails :: c y :- G.PlotValue y) $
  G.StackedLayout (plotChart c)

plotChart
  :: forall s x y
  .  ( G.PlotValue x
     , G.PlotValue y
     , AffineSpace x
     , Fractional (Diff x) )
  => Chart s x y
  -> G.Layout x y
plotChart a = def
  { G._layout_title = T.unpack a.config.title
  , G._layout_legend = Just $ def
      { G._legend_position    = G.LegendRight
      , G._legend_orientation = G.LORows 1
      }
  , G._layout_plots = mconcat
      [ map plotCandle a.candle `using` parListSuffixChunks 3
      , map plotVLine  a.vline  `using` parListSuffixChunks 3
      , map plotFill   a.fill   `using` parListSuffixChunks 3
      , map plotLine   a.line   `using` parListSuffixChunks 3
      , map plotHLine  a.hline  `using` parListSuffixChunks 3
      ]
  }

plotLine :: forall x y. AffineSpace x => Line x y -> G.Plot x y
plotLine a =
  let title = T.unpack a.config.title
      m1 :: Map Line.Style [(x, y)]
      m1 = Map.foldrWithKey (\x (ls, y) ->
                                let x' = x .+^ a.config.xoff
                                in  Map.insertWith mappend ls [(x', y)])
                            mempty a.points
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (ls, xys) <- Map.toAscList m1
       pure $ G.toPlot $ def
         { G._plot_lines_title  = title
         , G._plot_lines_style  = fromLineStyle ls
         , G._plot_lines_values = [xys]
         }

plotHLine :: forall x y. HLine y -> G.Plot x y
plotHLine a =
  let title = T.unpack a.config.title
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (ls, sy) <- Map.toAscList (backMap a.points)
       pure $ G.toPlot $ def
         { G._plot_lines_title = T.unpack a.config.title
         , G._plot_lines_style = fromHLineStyle ls
         , G._plot_lines_limit_values = do
             y <- toList sy
             pure [ (G.LMin, G.LValue y)
                  , (G.LMax, G.LValue y) ]
         }

plotVLine :: forall x y. AffineSpace x => VLine x -> G.Plot x y
plotVLine a =
  let title = T.unpack a.config.title
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (ls, sx) <- Map.toAscList (backMap a.points)
       pure $ G.toPlot $ def
         { G._plot_lines_title        = title
         , G._plot_lines_style        = fromVLineStyle ls
         , G._plot_lines_limit_values = do
              x <- toList sx
              let x' = x .+^ a.config.xoff
              pure [ (G.LValue x', G.LMin)
                   , (G.LValue x', G.LMax) ]
         }

plotFill :: forall x y. AffineSpace x => Fill x y -> G.Plot x y
plotFill a =
  let title = T.unpack a.config.title
  in set (G.plot_legend . mapped . _1) title $ mconcat $ do
       (s, m) <- Map.toAscList a.points
       pure $ G.toPlot $ def
         { G._plot_fillbetween_title  = title
         , G._plot_fillbetween_style  = fromFillStyle s
         , G._plot_fillbetween_values = do
             (x, sys) <- Map.toAscList m
             ys <- toList sys
             let x' = x .+^ a.config.xoff
             pure (x', (ys.start, ys.end))
         }

plotCandle :: forall x y
           .  (AffineSpace x, Fractional (Diff x))
           => Candle x y -> G.Plot x y
plotCandle a =
  let title = T.unpack a.config.title
      m1 :: Map Candle.Style [G.Candle x y]
      m1 = Map.foldrWithKey (\se (sty, ohlc') ->
                                Map.insertWith mappend sty [f se ohlc'])
                            mempty a.points
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
    f :: Interval x -> OHLC y -> G.Candle x y
    f se ohlc' =
       let !x = se.start .+^ ((se.end .-. se.start) / 2)
       in G.Candle { G.candle_x     = x .+^ a.config.xoff
                   , G.candle_open  = ohlc'.open
                   , G.candle_high  = ohlc'.high
                   , G.candle_low   = ohlc'.low
                   , G.candle_close = ohlc'.close
                   , G.candle_mid   = ohlc'.close
                     -- Note [candle_mid]: Dummy value. Not used during
                     -- rendering if G._plot_candle_centre is set to 0.
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
        , G._line_dashes = a.lineDashes }
  , def { G._fill_color  = a.fillColor }
  )

-- | Let the values in a 'Map' become its keys, and its keys become its values.
-- Each @[k]@ is in descending order.  There are no repeated @k@s in the output.
backMap :: (Ord v) => Map k v -> Map v [k]
backMap = Map.foldlWithKey (\m k v -> Map.insertWith mappend v [k] m) Map.empty

-- | Divides the list into chunks of the specified length. The first chunk
-- is evaluated sequentially, the rest of the chunks are evaluated in parallel.
parListSuffixChunks :: Int -> Strategy [a]
parListSuffixChunks n as = do
  let (pre0, pos0) = splitAt (n - 1) as
  pos1 <- case pos0 of
            [] -> pure []
            _  -> parListChunk (n - 1) rseq pos0
  pre1 <- evalList rseq pre0
  pure (pre1 <> pos1)


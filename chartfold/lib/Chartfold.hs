module Chartfold
 ( runy
 ) where

import Data.AffineSpace (AffineSpace(..))
import qualified Data.Colour as Co
import qualified Data.Colour.Names as Co
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq)

import Chartfold.Core (Id(..), Element(..))
import qualified Chartfold.Candle as Candle
import qualified Chartfold.Chart as Chart
import qualified Chartfold.Fill as Fill
import qualified Chartfold.HLine as HLine
import qualified Chartfold.Line as Line
import qualified Chartfold.VLine as VLine
import qualified Chartfold.XChart as XChart
import qualified Chartfold.XCharts as XCharts
import Chartfold.Backend.Charts (C, plotXChart)
import Tra.Constraint (Class1(..))

import qualified Graphics.Rendering.Chart.Gtk as GGtk
import qualified Graphics.Rendering.Chart.Layout as G
import qualified Graphics.Rendering.Chart.Renderable as G


runy :: IO ()
runy = case xchart1 of
  Left  e -> error (show e)
  Right (xc :: XChart.XChart Double C) -> do
    let sls = G.StackedLayouts [plotXChart xc] False
    GGtk.renderableToWindow (G.renderStackedLayouts sls) 1000 1000

--------------------------------------------------------------------------------

xchart1Config :: (Num (Diff x), c Double) => Config (XChart.XChart x c)
xchart1Config = XChart.Config (chart1Config :: Config (Chart.Chart _ Double))

xchart1
  :: (Num x, Num (Diff x), Enum (Diff x), Enum x, AffineSpace x, Ord x,
      c Double)
  => Either (Err (XChart.XChart x c)) (XChart.XChart x c)
xchart1 = bimap XChart.ErrXChart_Chart XChart.XChart
                (chart1 :: Either _ (Chart.Chart _ Double))

xchart1Updates
  :: (Num x, Num (Diff x), Enum (Diff x), c Double)
  => [Seq (Update (XChart.XChart x c))]
xchart1Updates = fmap (fmap XChart.Update)
                      (chart1Updates :: [Seq (Update (Chart.Chart _ Double))])

chart1Config :: Num (Diff x) => Config (Chart.Chart x y)
chart1Config = Chart.Config
  { title = "MyChart"
  , lines = IntMap.fromList [(line1Id.un, line1Config)]
  , hLines = IntMap.fromList [(hLine1Id.un, hLine1Config)]
  , vLines = IntMap.fromList [(vLine1Id.un, vLine1Config)]
  , fills = IntMap.fromList [(fill1Id.un, fill1Config)]
  , candles = IntMap.fromList [(candle1Id.un, candle1Config)]
  }

chart1
  :: (Num x, Num y, Enum y, Num (Diff x), Enum (Diff x), Fractional y,
      Enum x, AffineSpace x, Ord x, Ord y)
  => Either (Err (Chart.Chart x y)) (Chart.Chart x y)
chart1 = foldlM (\acc (x, us) -> update x us acc)
                (element chart1Config)
                (zip [1..] chart1Updates)

chart1Updates
  :: (Num x, Num y, Enum y, Num (Diff x), Enum (Diff x), Fractional y, Ord y)
  => [Seq (Update (Chart.Chart x y))]
chart1Updates = zipWith5 (\a b c d e -> fromList [a, b, c, d, e])
  (Chart.UpdateChartFill fill1Id <$> fill1Updates)
  (Chart.UpdateChartLine line1Id <$> line1Updates)
  (Chart.UpdateChartHLine hLine1Id <$> hLine1Updates)
  (Chart.UpdateChartVLine vLine1Id <$> vLine1Updates)
  (Chart.UpdateChartCandle candle1Id <$> candle1Updates)

fill1Id :: Id (Fill.Fill x y)
fill1Id = Id 8

fill1Config :: Config (Fill.Fill x y)
fill1Config = Fill.Config "MyFill"

fill1Updates
  :: (Num x, Num y, Enum y, Num (Diff x), Enum (Diff x))
  => [Update (Fill.Fill x y)]
fill1Updates = do
  xo <- [1 .. 5]
  y0 <- [4 .. 8]
  y1 <- [9 .. 13]
  co <- [Co.blue, Co.red, Co.green]
  pure $ Fill.Update
    { style = Fill.style { Fill.color = Co.withOpacity co 0.6 }
    , x = xo
    , y = (y0, y1)
    }

line1Id :: Id (Line.Line x y)
line1Id = Id 2

line1Config :: Num (Diff x) => Config (Line.Line x y)
line1Config = Line.Config "MyLine" 0

line1Updates
  :: (Num x, Num y, Enum y, Num (Diff x), Enum (Diff x), Fractional y)
  => [Update (Line.Line x y)]
line1Updates = do
  y <- [1 .. 10] <> [9 .. 2] <> [5 .. 50]
  co <- [Co.blue, Co.green, Co.red]
  pure $ Line.Update
          { style = Line.style { Line.color = Co.opaque co }
          , y     = y * (if co == Co.blue then 1 else 0.9)
          }

hLine1Id :: Id (HLine.HLine x y)
hLine1Id = Id 1

hLine1Config :: Config (HLine.HLine x y)
hLine1Config = HLine.Config "MyHLine"

hLine1Updates :: Num y => [Update (HLine.HLine x y)]
hLine1Updates = do
  yy <- [Nothing, Just 30, Just 20, Nothing, Just 40] <> repeat (Just 10)
  pure $ HLine.Update
    { style = HLine.style
    , y     = yy
    }

vLine1Id :: Id (VLine.VLine x y)
vLine1Id = Id 38

vLine1Config :: Config (VLine.VLine x y)
vLine1Config = VLine.Config "MyVLine"

vLine1Updates :: Num (Diff x) => [Update (VLine.VLine x y)]
vLine1Updates = repeat $ VLine.Update
  { style = VLine.style
  , x     = 0
  }


candle1Id :: Id (Candle.Candle x y)
candle1Id = Id 9

candle1Config :: Config (Candle.Candle x y)
candle1Config = Candle.Config "MyCandle"

candle1Updates :: (Num (Diff x), Num y, Ord y) => [Update (Candle.Candle x y)]
candle1Updates = cycle $ do
  o <- [1, 2, 3, 4, 5]
  h <- [1, 2, 3, 4, 5]
  l <- [1, 2, 3, 4, 5]
  c <- [1, 2, 3, 4, 5]
  guard (l <= min o c && max o c <= h)
  pure $ Candle.Update
    { style = if o < c
        then Candle.style { Candle.lineColor = Co.opaque Co.teal }
        else Candle.style { Candle.lineColor = Co.opaque Co.red  }
    , start = -1
    , end   = 0
    , open  = o
    , high  = h
    , low   = l
    , close = c
    }

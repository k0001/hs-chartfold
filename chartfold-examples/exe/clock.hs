module Main (main) where

import Control.Concurrent.STM.TMVar
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Backend.Types qualified as G
import Graphics.Rendering.Chart.Backend.Cairo qualified as GCairo
import Graphics.Rendering.Chart.Layout qualified as G
import Graphics.Rendering.Chart.Renderable as G
import Graphics.UI.Gtk.Gdk.Events qualified as GdkE
import Graphics.UI.Gtk qualified as Gtk

import Chartfold.Backend.Chart (plotChart)
import Chartfold.Chart (Chart)
import Chartfold.Chart qualified as Chart
import Chartfold.Line qualified as Line
import Chartfold.HLine qualified as HLine

--------------------------------------------------------------------------------

main :: IO ()
main = withClockChart $ \cc -> do
    chartTV :: TVar (Chart s Time.POSIXTime Rational) <-
      newTVarIO $! cc.initial
    glayoutTV :: TVar (G.Layout Time.POSIXTime Rational) <-
      newTVarIO $! plotChart cc.initial
    let tickx :: Time.UTCTime -> STM ()
        tickx x = do
          chart <- readTVar chartTV
          let chart' = Chart.update (cc.tickx x) chart
          writeTVar chartTV $! chart'
          writeTVar glayoutTV $! plotChart chart'
        ticky :: Rational -> STM ()
        ticky y = do
          chart <- readTVar chartTV
          let chart' = Chart.update (cc.ticky y) chart
          writeTVar chartTV $! chart'
          writeTVar glayoutTV $! plotChart chart'
    pickTMV :: TMVar (G.PickFn (G.LayoutPick Time.POSIXTime Rational Rational))
      <- newEmptyTMVarIO

    void $ Gtk.initGUI
    window <- Gtk.windowNew
    canvas <- Gtk.drawingAreaNew
    Gtk.set window [Gtk.containerChild Gtk.:= canvas]
    Gtk.widgetSetSizeRequest window 800 600
    void $ Gtk.onDestroy window Gtk.mainQuit
    void $ Gtk.onExpose canvas $ \_ -> do
      glayout <- readTVarIO glayoutTV
      updateCanvas (G.layoutToRenderable glayout) canvas pickTMV
    void $ Gtk.onButtonPress canvas $ \case
      GdkE.Button{GdkE.eventX=ex, GdkE.eventY=ey} -> do
        atomically $ do
          pick <- readTMVar pickTMV
          case pick (G.Point ex ey) of
            Just (G.LayoutPick_PlotArea _cx cy _) -> ticky cy
            _ -> pure ()
        Gtk.widgetQueueDraw window
        pure True
      _ -> pure False

    void $ forkIO $ forever $ do
      t <- Time.getCurrentTime
      atomically $ tickx t
      Gtk.widgetQueueDraw window
      threadDelay 10_000

    Gtk.widgetShowAll window
    Gtk.mainGUI

updateCanvas :: G.Renderable a -> Gtk.DrawingArea -> TMVar (G.PickFn a) -> IO Bool
updateCanvas chart canvas pickTMV = do
  win <- Gtk.widgetGetDrawWindow canvas
  (width, height) <- Gtk.widgetGetSize canvas
  let sz = (fromIntegral width,fromIntegral height)
      env = GCairo.defaultEnv G.bitmapAlignmentFns
  pick <- Gtk.renderWithDrawable win $ GCairo.runBackend env (render chart sz)
  atomically $ tryTakeTMVar pickTMV >> putTMVar pickTMV pick
  return True

data ClockChart s = ClockChart
  { tickx   :: Time.UTCTime -> Chart.Update s Time.POSIXTime Rational
  , ticky   :: Rational -> Chart.Update s Time.POSIXTime Rational
  , initial :: Chart.Chart s Time.POSIXTime Rational
  }

withClockChart :: (forall s. ClockChart s -> a) -> a
withClockChart f =
  let uline :: forall x y. x -> y -> Line.Update x y
      uline = Line.set Line.styleDefault
  in Chart.configure "Clock" $ do
       fh <- Chart.line $ Line.configDefault "Hour"
       fm <- Chart.line $ Line.configDefault "Minute"
       fs <- Chart.line $ Line.configDefault "Second"
       fy <- Chart.hline $ HLine.configDefault "Mouse Y"
       pure $ \c -> f $ ClockChart
         { initial = Chart.initial c
         , ticky = fy . HLine.set HLine.styleDefault
         , tickx = \t ->
           let x = Time.utcTimeToPOSIXSeconds t
               tod = Time.timeToTimeOfDay (Time.utctDayTime t)
           in fh (uline x (toInteger (Time.todHour tod) % 23)) <>
              fm (uline x (toInteger (Time.todMin tod) % 59)) <>
              fs (uline x (min 60 (truncate (Time.todSec tod)) % 60))
         }


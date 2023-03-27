module Main (main) where

import Control.Concurrent.STM.TMVar
import Data.Sequence (Seq)
import Data.Time qualified as Time
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Backend.Types qualified as G
import Graphics.Rendering.Chart.Backend.Cairo qualified as GCairo
import Graphics.Rendering.Chart.Layout qualified as G
import Graphics.Rendering.Chart.Renderable as G
import Graphics.UI.Gtk.Gdk.Events qualified as GdkE
import Graphics.UI.Gtk qualified as Gtk
import MTLPrelude

import Chartfold.Backend.Chart (plotChart)
import Chartfold.Chart (Chart)
import Chartfold.Chart qualified as Chart
import Chartfold.Line (Config(..))
import Chartfold.Line qualified as Line
import Chartfold.HLine (Config(..))
import Chartfold.HLine qualified as HLine

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let (uMouseY, uTime, chart0) = initialChart
    chartTV :: TVar (Chart Time.UTCTime Rational) <- newTVarIO $! chart0
    glayoutTV :: TVar (G.Layout Time.UTCTime Rational) <-
      newTVarIO $! plotChart chart0
    let tickTime :: Time.UTCTime -> STM ()
        tickTime x = do
          chart <- readTVar chartTV
          chart' <- either throwSTM pure $ Chart.update x (uTime x) chart
          writeTVar chartTV $! chart'
          writeTVar glayoutTV $! plotChart chart'
        tickMouseY :: Time.UTCTime -> Rational -> STM ()
        tickMouseY x y = do
          chart <- readTVar chartTV
          chart' <- either throwSTM pure $ Chart.update x (uMouseY y) chart
          writeTVar chartTV $! chart'
          writeTVar glayoutTV $! plotChart chart'
    pickTMV :: TMVar (G.PickFn (G.LayoutPick Time.UTCTime Rational Rational))
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
        t <- Time.getCurrentTime
        atomically $ do
          pick <- readTMVar pickTMV
          case pick (G.Point ex ey) of
            Just (G.LayoutPick_PlotArea _cx cy _) -> tickMouseY t cy
            _ -> error "unexpected"
        Gtk.widgetQueueDraw window
        pure True
      _ -> pure False

    void $ forkIO $ forever $ do
      t <- Time.getCurrentTime
      atomically $ tickTime t
      Gtk.widgetQueueDraw window
      threadDelay 1_000_000

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

initialChart
  :: ( Rational -> Seq (Chart.Update Time.UTCTime Rational)
     , Time.UTCTime -> Seq (Chart.Update Time.UTCTime Rational)
     , Chart Time.UTCTime Rational )
initialChart = case runState m (Chart.configDefault "time") of
                 ((g, h), c) -> (g, h, Chart.initial c)
  where
    m = do fh <- Chart.line $ Line.Config {title="hour", x=0}
           fm <- Chart.line $ Line.Config {title="minute", x=0}
           fs <- Chart.line $ Line.Config {title="second", x=0}
           fx <- Chart.hline $ HLine.Config {title="mousex"}
           let u = \y -> Line.Update {style=Line.styleDefault, y=y}
               g = \y -> fx [HLine.Update {style=HLine.styleDefault, y=Just y}]
               h = \x -> let tod = Time.timeToTimeOfDay (Time.utctDayTime x)
                         in fh [u (toInteger (Time.todHour tod) % 23)] <>
                            fm [u (toInteger (Time.todMin tod) % 59)] <>
                            fs [u (min 60 (truncate (Time.todSec tod)) % 60)]
           pure (g, h)



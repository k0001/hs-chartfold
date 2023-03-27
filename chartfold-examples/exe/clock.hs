module Main (main) where

import Diagrams.Prelude
import Graphics.Blank (blankCanvas)
import Data.Time qualified as Time
import Data.Sequence (Seq)
import Diagrams.Backend.Canvas
import Graphics.Blank qualified as GB
import Graphics.Rendering.Chart.Renderable as G
import Graphics.Rendering.Chart.Backend.Types as G
import Graphics.Rendering.Chart.Backend.Diagrams qualified as GD
import MTLPrelude

import Chartfold.Backend.Chart (plotChart)
import Chartfold.Chart qualified as Chart
import Chartfold.Line (Config(..))
import Chartfold.Line qualified as Line

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Go to http://127.0.0.1:3000"
  blankCanvas 3000 mainLoop

mainLoop :: GB.DeviceContext -> IO ()
mainLoop ctx = do
    denv <- GD.defaultEnv G.vectorAlignmentFns 800 600
    go denv chart0
  where
    (u, chart0) = fmap Chart.initial mkChartConfig
    go denv chart = do
      t <- Time.getCurrentTime
      chart1 <- either throwIO pure $ Chart.update t (u t) chart
      let glayout = plotChart chart1
          (dia, _fpick) = GD.runBackendR denv (G.toRenderable glayout)
      GB.send ctx $ renderDia Canvas (CanvasOptions (mkWidth 1200)) dia
      threadDelay 20000 -- 0.2 seconds
      go denv chart1

mkChartConfig
  :: ( Time.UTCTime -> Seq (Chart.Update Time.UTCTime Rational)
     , Chart.Config Time.UTCTime )
mkChartConfig = flip runState (Chart.configDefault "time") $ do
  fh <- Chart.line $ Line.Config {title="hour", x=0}
  fm <- Chart.line $ Line.Config {title="minute", x=0}
  fs <- Chart.line $ Line.Config {title="second", x=0}
  let u = \y -> Line.Update {style=Line.styleDefault, y=y}
  pure $ \t ->
    let tod = Time.timeToTimeOfDay (Time.utctDayTime t)
    in  fh [u (toInteger (Time.todHour tod) % 23)] <>
        fm [u (toInteger (Time.todMin tod) % 59)] <>
        fs [u (min 60 (truncate (Time.todSec tod)) % 60)]


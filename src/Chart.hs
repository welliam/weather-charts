{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Chart (render) where

import           Control.Lens                           (Identity, Lens', (.~))
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Base64                 as Base64
import qualified Data.Colour                            as Colour
import qualified Data.Colour.Names                      as Colour
import           Data.Default.Class                     (def)
import           Data.Monoid                            ((<>))
import qualified Data.Time                              as Time
import qualified Data.UUID                              as UUID
import qualified Graphics.Rendering.Chart               as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as CairoChart
import qualified System.IO                              as IO
import qualified System.Random                          as Random

import qualified Types

type Layout = Chart.LayoutLR XValue YValue YValue
type Axis = Chart.LayoutAxis YValue
type ChartLines = Chart.PlotLines XValue YValue
type XValue = Time.UTCTime
type YValue = Int

getWeather :: XValue -> [(XValue, YValue, YValue)]
getWeather currentTime
  = [(Time.addUTCTime (-90) currentTime, 0, 60)
    ,(Time.addUTCTime (-60) currentTime, 20, 40)
    ,(Time.addUTCTime (-30) currentTime, 40, 120)
    ,(currentTime, 60, 0)]

options :: CairoChart.FileOptions
options = CairoChart.FileOptions (800,600) CairoChart.SVG

weatherLineOf :: (Types.Weather -> Int) -> [Types.Weather] -> [(XValue, YValue)]
weatherLineOf f weather = [(Types.created w, f w) | w <- weather]

chart :: [Types.Weather] -> Chart.Renderable ()
chart weather = Chart.toRenderable (getLayout (0, 100) weather1 weather2)
  where weather1
          = plot "Temperature" Colour.red (weatherLineOf Types.temperature weather)
        weather2
          = plot "Humidity" Colour.blue (weatherLineOf Types.humidity weather)

formatAxis :: (YValue, YValue) -> Lens' Layout Axis -> Layout -> Layout
formatAxis (min, max) axis layout =
  axis . Chart.laxis_override .~ Chart.axisGridHide
  $ axis . Chart.laxis_generate .~ Chart.autoAxis . (min:) . (max:)
  $ layout

getLayout :: (YValue, YValue) -> ChartLines-> ChartLines-> Layout
getLayout range leftPlot rightPlot =
  formatAxis' Chart.layoutlr_left_axis
  $ formatAxis' Chart.layoutlr_right_axis
  $ Chart.layoutlr_plots .~ [Left (Chart.toPlot leftPlot), Right (Chart.toPlot rightPlot)]
  $ Chart.layoutlr_grid_last .~ False
  $ def
  where formatAxis' = formatAxis range

plot :: String -> Colour.Colour Double -> [(XValue, YValue)] -> ChartLines
plot title colour values =
  Chart.plot_lines_style . Chart.line_color .~ Colour.opaque colour
  $ Chart.plot_lines_values .~ [values]
  $ Chart.plot_lines_title .~ title
  $ def

getTmpPath :: UUID.UUID -> IO.FilePath
getTmpPath uuid = "/tmp/chart-" <> UUID.toString uuid <> ".svg"

inlineSVG :: BS.ByteString -> BS.ByteString
inlineSVG svgBS
  = "<img src=\"data:image/svg+xml;base64,"
    <> Base64.encode svgBS
    <> "\" />"

render :: [Types.Weather] -> IO BS.ByteString
render weather = do
  tmpPath <- getTmpPath <$> Random.randomIO
  _ <- CairoChart.renderableToFile options tmpPath (chart weather)
  inlineSVG <$> BS.readFile tmpPath

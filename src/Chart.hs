{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Chart (render) where

import           Control.Lens                           (Lens', (.~))
import qualified Data.ByteString                        as BS
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

type XValue = Time.LocalTime

type YValue = Int

options :: CairoChart.FileOptions
options = CairoChart.FileOptions (800,600) CairoChart.PNG

weatherLineOf :: Time.TimeZone -> (Types.Weather -> Int) -> [Types.Weather] -> [(XValue, YValue)]
weatherLineOf tz f weather = [(Time.utcToLocalTime tz (Types.created w), f w) | w <- weather]

chart :: Time.TimeZone -> [Types.Weather] -> Chart.Renderable ()
chart tz weather = Chart.toRenderable (getLayout (0, 100) weather1 weather2)
  where weather1 = plot "Temperature" Colour.red (weatherLineOf tz Types.temperature weather)
        weather2 = plot "Humidity" Colour.blue (weatherLineOf tz Types.humidity weather)

formatAxis :: (YValue, YValue) -> Lens' Layout Axis -> Layout -> Layout
formatAxis (low, high) axis layout =
  axis . Chart.laxis_override .~ Chart.axisGridHide
  $ axis . Chart.laxis_generate .~ Chart.autoAxis . (low:) . (high:)
  $ layout

getLayout :: (YValue, YValue) -> ChartLines -> ChartLines -> Layout
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

getTmpPath :: String -> UUID.UUID -> IO.FilePath
getTmpPath ext uuid = "/tmp/chart-" <> UUID.toString uuid <> ext

render :: [Types.Weather] -> IO BS.ByteString
render weather = do
  timezone <- Time.getCurrentTimeZone
  tmpPath <- getTmpPath ".svg" <$> Random.randomIO
  _ <- CairoChart.renderableToFile options tmpPath (chart timezone weather)
  BS.readFile tmpPath

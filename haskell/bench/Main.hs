module Main where

import           Control.Arrow                         (first, (&&&))

import           Criterion
import           Criterion.IO                          (readRecords)
import           Criterion.Main                        (defaultConfig,
                                                        defaultMainWith)
import           Criterion.Types
import           Statistics.Resampling.Bootstrap       (estPoint)

import qualified Data.ByteString.Lazy                  as BL
import           Data.Csv                              as Csv
import           Data.List                             (isPrefixOf, sortBy, zip4)
import           Data.Ord                              (comparing)
import qualified Data.Vector.Unboxed                   as V

import           Test.QuickCheck

import           Graphics.Gnuplot.Advanced
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.Option         as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.PNG         as PNG

import           Quickselect

range :: [Int]
range = [0, 1000 .. 50000]

testData :: [(Int, IO (V.Vector Int))]
testData = map f range
  where f n = (n, generate $ (V.fromList <$> vectorOf n arbitrary))

rawOut = "../data/kth-element-haskell.raw"
csvOut = "../data/kth-element-haskell.csv"
pngOut = "../data/kth-element-haskell.png"

config :: Config
config = defaultConfig { resamples = 3, rawDataFile = Just rawOut }

main :: IO ()
main = do
  defaultMainWith config [
    bgroup "main" [
        bgroup "find" $ map (run quickselect) testData
        , bgroup "cheat" $ map (run sortselect) testData
        ]
    ]
  (Right records) <- readRecords rawOut
  let records' = map (\(Analysed r) -> r) records
  let find' = extractOutcomes (filterReports "main/find/" records')
      cheat' = extractOutcomes (filterReports "main/cheat/" records')
      results = zip3 find' cheat' range
  BL.writeFile csvOut (encode results)
  plot (PNG.cons pngOut) resultPlot
  return ()
  where run f (n, xs) = env xs $ \ ~(xs') -> bench (show n) $ nf (f n) xs'
        filterReports name = filter ((name `isPrefixOf`) . reportName)
        extractOutcomes = map (estPoint . anMean . reportAnalysis)

resultPlot :: Frame.T (Graph2D.T Int Double)
resultPlot =
  let lineSpec title =
        Graph2D.lineSpec $
        LineSpec.title title $
        LineSpec.deflt
      lineSpec1 = lineSpec "find"
      lineSpec2 = lineSpec "cheat"
      frameOpts =
        Opts.xLabel "input size" $
        Opts.yLabel "execution time (seconds)" $
        Opts.title "Performance of Kth Element" $
        Opts.add (Opt.custom "datafile separator" "") ["\",\""] $
        Opts.deflt
      path1 = fmap lineSpec1 $ Plot2D.pathFromFile Graph2D.lines csvOut 3 1
      path2 = fmap lineSpec2 $ Plot2D.pathFromFile Graph2D.lines csvOut 3 2
  in Frame.cons frameOpts $ mconcat [path1, path2]

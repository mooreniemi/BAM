import Quickselect._
import scala.util.Random
import java.io.{File, PrintWriter}
import com.panayotis.gnuplot.JavaPlot
import com.panayotis.gnuplot.dataset.ArrayDataSet
import com.panayotis.gnuplot.plot.DataSetPlot
import com.panayotis.gnuplot.style.{PlotStyle, Style}
import org.scalameter._

object Main {
  def bench = config(
    Key.exec.benchRuns -> 3
  ) withWarmer {
    new Warmer.Default
  } withMeasurer(new Measurer.IgnoringGC, Aggregator.median[Double])

  val inputRange = 0 to 50000 by 100
  lazy val genInput: Seq[(Int, Seq[Int])] =
    for {
      n <- inputRange
      k = n / 10
      xs = Seq.fill(n) { Random.nextInt }
    } yield (k, xs)

  def getTimesFor(selector: (Int, Array[Int]) => Option[Int]): Seq[Double] =
    for {
      (k, xs) <- genInput
      _ = if (k % 100 == 0) { print(".") }
      xs0 = xs.toArray
    } yield (bench measure { selector(k, xs0) }).value / 1000.0

  def writeCSV(filename: String, data: Seq[Seq[Double]]): Unit = {
    val writer = new PrintWriter(new File(filename))
    for (row <- data) {
      val line = row.map(_.toString).mkString(",")
      writer.println(line)
    }
    writer.close()
  }

  def doPlot(results: Seq[Seq[Double]], filename: String): Unit = {
    val p = new JavaPlot
    p.setTitle("Performance of Kth Element")
    p.getAxis("x").setLabel("input size")
    p.getAxis("y").setLabel("execution time (seconds)")

    // val Seq(timesFind, timesFindSimple, timesSort) = results
    val Seq(timesFind, timesSort) = results
    for ((name, times) <- Seq( // ("quickselectSimple", timesFindSimple),
                              ("quickselect", timesFind),
                              ("sort", timesSort)
                              )) {
      val ts: Array[Array[Double]] =
        Array[Array[Double]](inputRange.toArray.map(_.toDouble), times.toArray).transpose
      val plot = new DataSetPlot(new ArrayDataSet(ts))
      plot.setTitle(name)
      plot.setPlotStyle(new PlotStyle(Style.LINES))
      p.addPlot(plot)
    }
    p.plot()
  }

  def main(args: Array[String]): Unit = {
    val timesFind = getTimesFor(quickselect)
    // val timesFindSimple = getTimesFor(quickselectSimple)
    val timesSort = getTimesFor(sortselect)
    // val allTimes = Seq(timesFind, timesFindSimple, timesSort)
    val allTimes = Seq(timesFind, timesSort)
    val output = allTimes :+ inputRange.map(_.toDouble)
    writeCSV("../data/kth-element-scala.csv", output)
    doPlot(allTimes, "../data/kth-element-scala.png")
  }
}

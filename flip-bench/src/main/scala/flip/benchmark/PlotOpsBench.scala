package flip.benchmark

import java.util.concurrent.TimeUnit

import flip.{range, _}
import flip.plot.{CountPlot, DensityPlot, Plot}
import flip.range.RangeP
import flip.plot.syntax._
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class PlotOpsBench { self =>

  @Param(Array("20", "2000"))
  var recordsNo: Int = _

  var records: List[(RangeP, Double)] = _

  var countplot: CountPlot = _

  var densityplot: DensityPlot = _

  @Setup
  def setupSketch(): Unit = {
    val records = (0.0 until recordsNo.toDouble by 1.0).toList.map(p => (RangeP.point(p), math.cos(p)))
    val countplot = CountPlot.disjoint(records)
    val densityplot = DensityPlot.disjoint(records)

    self.records = records
    self.countplot = countplot
    self.densityplot = densityplot
  }

  @Benchmark
  def interpolation: Double = {
    countplot.interpolation(recordsNo / 2)
  }

  @Benchmark
  def disjoint: CountPlot = {
    CountPlot.disjoint(records)
  }

  @Benchmark
  def planarizeRecords: List[(RangeP, List[Double])] = {
    CountPlot.planarizeRecords(records)
  }

  @Benchmark
  def merge: DensityPlot = {
    (1.0, densityplot) ++ (1.0, densityplot)
  }

  @Benchmark
  def inverseNormalizeCumulative: DensityPlot = {
    densityplot.inverseNormalizeCumulative
  }

}

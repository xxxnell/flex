package flip.benchmark

import java.util.concurrent.TimeUnit

import flip._
import flip.plot.{CountPlot, Plot}
import flip.range.RangeP
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class PlotOpsBench { self =>

  @Param(Array("200", "2000"))
  var recordsNo: Int = _

  var records: List[(RangeP, Double)] = _

  var plot: Plot = _

  @Setup
  def setupSketch(): Unit = {
    val records = (0.0 until recordsNo.toDouble by 1.0).toList.map(p => (RangeP.point(p), math.cos(p)))
    val plot = CountPlot.disjoint(records)

    self.records = records
    self.plot = plot
  }

  @Benchmark
  def interpolation: Double = {
    plot.interpolation(recordsNo / 2)
  }

  @Benchmark
  def disjoint: CountPlot = {
    CountPlot.disjoint(records)
  }

}

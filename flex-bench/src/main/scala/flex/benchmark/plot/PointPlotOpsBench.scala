package flex.benchmark.plot

import java.util.concurrent.TimeUnit

import flex._
import flex.plot.syntax._
import flex.plot.{ CountPlot, DensityPlot, PointPlot }
import flex.range.RangeP
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class PointPlotOpsBench { self =>

  @Param(Array("20", "200", "2000"))
  var recordsNo: Int = _

  var recordsP: Array[(Double, Double)] = _

  var recordsR: List[(RangeP, Double)] = _

  var pointPlot: PointPlot = _

  @Setup
  def setup(): Unit = {
    val recordsP = (0.0 until recordsNo.toDouble by 1.0).toArray.map(p => (p, math.cos(p)))
    val recordsR = recordsP.toList.map(xy => (RangeP(xy._1), xy._2))

    self.recordsP = recordsP
    self.recordsR = recordsR
    self.pointPlot = PointPlot.apply(recordsP)
  }

  @Benchmark
  def construct: PointPlot = {
    val pointPlot = PointPlot.apply(recordsP)
    pointPlot.index
    pointPlot
  }

  @Benchmark
  def interpolation: Double =
    pointPlot.interpolation(recordsNo / 2)

  @Benchmark
  def add(): PointPlot =
    (1.0, pointPlot) :+ (1.0, pointPlot)

  @Benchmark
  def inverse: PointPlot =
    pointPlot.inverse

  @Benchmark
  def normalizedCumulative: PointPlot =
    pointPlot.normalizedCumulative

  @Benchmark
  def inverseNormalizedCumulative: PointPlot =
    pointPlot.inverseNormalizedCumulative

}

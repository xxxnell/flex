package flip.benchmark.plot

import java.util.concurrent.TimeUnit

import flip._
import flip.plot.syntax._
import flip.plot.{CountPlot, DensityPlot, PointPlot}
import flip.range.RangeP
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class RangePlotOpsBench { self =>

  @Param(Array("20", "200", "2000"))
  var recordsNo: Int = _

  var recordsP: Array[(Double, Double)] = _

  var recordsR: List[(RangeP, Double)] = _

  var countPlot: CountPlot = _

  var densityPlot: DensityPlot = _

  @Setup
  def setup(): Unit = {
    val recordsP = (0.0 until recordsNo.toDouble by 1.0).toArray.map(p => (p, math.cos(p)))
    val recordsR = recordsP.toList.map(xy => (RangeP(xy._1), xy._2))

    self.recordsP = recordsP
    self.recordsR = recordsR
    self.countPlot = CountPlot.disjoint(recordsR)
    self.densityPlot = DensityPlot.disjoint(recordsR)
  }

  @Benchmark
  def interpolationOfCountplot: Double = {
    countPlot.interpolation(recordsNo / 2)
  }

  @Benchmark
  def add(): DensityPlot = {
    (1.0, densityPlot) :+ (1.0, densityPlot)
  }

  @Benchmark
  def inverse: DensityPlot = {
    densityPlot.inverse
  }

  @Benchmark
  def normalizedCumulative: DensityPlot = {
    densityPlot.normalizedCumulative
  }

  @Benchmark
  def inverseNormalizedCumulative: DensityPlot = {
    densityPlot.inverseNormalizedCumulative
  }

  @Benchmark
  def disjointOfCountplot: CountPlot = {
    CountPlot.disjoint(recordsR)
  }

  @Benchmark
  def planarizeRecordsOfCountplot: List[(RangeP, List[Double])] = {
    CountPlot.planarizeRecords(recordsR)
  }

}

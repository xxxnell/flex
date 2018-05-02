package flip.benchmark

import java.util.concurrent.TimeUnit

import flip._
import flip.plot.syntax._
import flip.plot.{CountPlot, DensityPlot, PointPlot}
import flip.range.RangeP
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class PlotOpsBench { self =>

  @Param(Array("20", "200", "2000"))
  var recordsNo: Int = _

  var recordsP: Array[(Double, Double)] = _

  var recordsR: List[(RangeP, Double)] = _

  var countPlot: CountPlot = _

  var densityPlot: DensityPlot = _

  var pointPlot: PointPlot = _

  @Setup
  def setup(): Unit = {
    val recordsP = (0.0 until recordsNo.toDouble by 1.0).toArray.map(p => (p, math.cos(p)))
    val recordsR = recordsP.toList.map(xy => (RangeP(xy._1), xy._2))

    self.recordsP = recordsP
    self.recordsR = recordsR
    self.countPlot = CountPlot.disjoint(recordsR)
    self.densityPlot = DensityPlot.disjoint(recordsR)
    self.pointPlot = PointPlot.apply(recordsP)
  }

  @Benchmark
  def constructOfPointPlot: PointPlot = {
    val pointPlot = PointPlot.apply(recordsP)
    pointPlot.index
    pointPlot
  }

  @Benchmark
  def interpolationOfCountplot: Double = {
    countPlot.interpolation(recordsNo / 2)
  }

  @Benchmark
  def interpolationOfPointPlot: Double = {
    pointPlot.interpolation(recordsNo / 2)
  }

  @Benchmark
  def addOfDensityPlot(): DensityPlot = {
    (1.0, densityPlot) :+ (1.0, densityPlot)
  }

  @Benchmark
  def addOfPointPlot(): PointPlot = {
    (1.0, pointPlot) :+ (1.0, pointPlot)
  }

  @Benchmark
  def inverseOfDensityPlot: DensityPlot = {
    densityPlot.inverse
  }

  @Benchmark
  def inverseOfPointPlot: PointPlot = {
    pointPlot.inverse
  }

  @Benchmark
  def normalizedCumulativeOfDensityPlot: DensityPlot = {
    densityPlot.normalizedCumulative
  }

  @Benchmark
  def normalizedCumulativeOfPointPlot: PointPlot = {
    pointPlot.normalizedCumulative
  }

  @Benchmark
  def inverseNormalizedCumulativeOfDensityPlot: DensityPlot = {
    densityPlot.inverseNormalizedCumulative
  }

  @Benchmark
  def inverseNormalizedCumulativeOfPointPlot: PointPlot = {
    pointPlot.inverseNormalizedCumulative
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

package flip.benchmark.sketch

import java.util.concurrent.TimeUnit

import flip.cmap.Cmap
import flip.implicits._
import flip.measure.Measure
import flip.measure.syntax.{-∞, ∞}
import flip.pdf.{AdaptiveSketch, Count}
import flip.pdf.sampling.IcdfSampling
import flip.pdf.update.EqUpdate
import flip.plot.DensityPlot
import flip.range.RangeM
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class EqUpdateBench { self =>

  // parameters

  @Param(Array("0", "50"))
  var bufferSize: Int = _

  @Param(Array("3"))
  var cmapNo: Int = _

  @Param(Array("20", "2000"))
  var cmapSize: Int = _

  @Param(Array("1"))
  var counterNo: Int = _

  // variables

  implicit var conf: SketchConf = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = Int.MaxValue,
      thresholdPeriod = Int.MaxValue,
      bufferSize = bufferSize,
      cmapSize = cmapSize,
      cmapNo = cmapNo,
      counterNo = counterNo
    )
    val (_, samples) = NumericDist.normal(0.0, 1).samples(bufferSize + 1)
    val sketch0 = Sketch.empty[Double]

    (0 until cmapNo).foreach { _ =>
      self.sketch = sketch0.narrowUpdate(samples: _*).rearrange
    }
    self.sketch = sketch0.narrowUpdate(samples: _*)
    self.conf = conf
  }

  object EqUpdateAlgorithm {

    // params

    lazy val mixingRatio: Double = sketch.conf.mixingRatio

    lazy val window: Double = sketch.conf.dataKernelWindow

    lazy val measure: Measure[Double] = sketch.measure

    lazy val icdfSampling: (Double => Double) => List[RangeM[Double]] =
      IcdfSampling.samplingF(measure, sketch.conf.cmap)

    lazy val ps: List[(Double, Count)] = sketch.asInstanceOf[AdaptiveSketch[Double]].buffer.dataset.toList

    // steps

    lazy val samplingC: DensityPlot = sampling

    def sampling: DensityPlot = sketch.sampling

    lazy val mergeC: DensityPlot = merge

    def merge: DensityPlot = {
      if (ps.nonEmpty) {
        val c1 = 1 / (mixingRatio + 1)
        val c2 = mixingRatio / (mixingRatio + 1)
        (c1, sampling) ++ (c2, DensityPlot.squareKernel(ps, window))
      } else sampling
    }

    lazy val icdfPlotC: DensityPlot = icdfPlot

    def icdfPlot: DensityPlot = {
      mergeC.inverseNormalizeCumulative
    }

    val rangesC: List[RangeM[Double]] = ranges

    def ranges: List[RangeM[Double]] = {
      def icdf =
        (d: Double) =>
          if (d <= 0) measure.from(-∞) else if (d >= 1) measure.from(∞) else measure.from(icdfPlotC.interpolation(d))
      icdfSampling(icdf)
    }

    val dividerC: List[Double] = divider

    def divider: List[Double] =
      rangesC
        .map(rangeM => rangeM.primitivize.start)
        .drop(1)

  }

  @Benchmark
  def updateCmapForSketch(): Cmap = {
    val ps = sketch.asInstanceOf[AdaptiveSketch[Double]].buffer.dataset.toList
    EqUpdate.updateCmapForSketch(sketch, ps)
  }

  @Benchmark
  def sampling: DensityPlot = EqUpdateAlgorithm.sampling

  @Benchmark
  def merge: DensityPlot = EqUpdateAlgorithm.merge

  @Benchmark
  def icdfPlot: DensityPlot = EqUpdateAlgorithm.icdfPlot

  @Benchmark
  def ranges: List[RangeM[Double]] = EqUpdateAlgorithm.ranges

  @Benchmark
  def divider: List[Double] = EqUpdateAlgorithm.divider

}

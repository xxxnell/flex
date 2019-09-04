package flex.benchmark

import java.util.concurrent.TimeUnit

import flex.implicits._
import flex.pdf.Histogram
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
class SketchFeaturedOpsBench { self =>

  // parameters

  @Param(Array("20"))
  var cmapSizeL: Int = _

  @Param(Array("3"))
  var cmapNoL: Int = _

  @Param(Array("50"))
  var bufferSizeL: Int = _

  @Param(Array("1.0"))
  var decayFactorL: Double = _

  @Param(Array("0.2"))
  var rebuildThresholdL: Double = _

  // variables

  implicit var conf: SketchConf = _

  var sketch: Sketch[Double] = _

  var cdf: PointPlot = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      cmapSize = cmapSizeL,
      cmapNo = cmapNoL,
      cmapStart = Some(-20),
      cmapEnd = Some(20),
      bufferSize = bufferSizeL,
      thresholdPeriod = bufferSizeL,
      decayFactor = decayFactorL,
      rebuildThreshold = rebuildThresholdL)
    val (_, samples) = NumericDist.normal(0.0, 1).samples(bufferSizeL)
    val sketch0 = Sketch.empty[Double]

    self.conf = conf
    self.sketch = sketch0.updateInOrder(samples).rebuild
    self.cdf = sketch.cdfSampling
  }

//  @Benchmark
//  def narrowUpdate: Sketch[Double] = {
//    sketch.narrowUpdate(1)
//  }
//
//  @Benchmark
//  def deepUpdate: (Sketch[Double], Option[Histogram[Double]]) = {
//    sketch.deepUpdate(1.0 to 10.0 by 1.0: _*)
//  }

  @Benchmark
  def probability: Double =
    sketch.probability(1, 2)

//  @Benchmark
//  def probabilityCached: Double = {
//    cdf.interpolation(2) - cdf.interpolation(1)
//  }

}

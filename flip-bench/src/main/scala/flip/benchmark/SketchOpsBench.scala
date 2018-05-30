package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip.implicits._
import flip.cmap.Cmap
import flip.hcounter.HCounter
import flip.pdf.Histogram

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SketchOpsBench { self =>

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
      startThreshold = 50,
      thresholdPeriod = 100,
      cmapSize = cmapSize,
      cmapNo = cmapNo,
      cmapStart = Some(-10d),
      cmapEnd = Some(10d),
      counterNo = counterNo
    )
    val (_, samples) = NumericDist.normal(0.0, 1).samples(bufferSize)
    val sketch0 = Sketch.empty[Double]

    self.conf = conf
    self.sketch = sketch0.narrowUpdate(samples: _*)
  }

  @Benchmark
  def construct: Sketch[Double] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50,
      thresholdPeriod = 100,
      bufferSize = bufferSize,
      cmapSize = cmapSize,
      cmapNo = cmapNo,
      cmapStart = Some(-10d),
      cmapEnd = Some(10d),
      counterNo = counterNo
    )

    Sketch.empty[Double]
  }

  @Benchmark
  def sampling: PointPlot = {
    sketch.pdfSampling
  }

  @Benchmark
  def narrowUpdate: Sketch[Double] = {
    sketch.narrowUpdate(1)
  }

  @Benchmark
  def deepUpdate: (Sketch[Double], Option[Histogram[Double]]) = {
    sketch.deepUpdate(1.0 to 10.0 by 1.0: _*)
  }

  @Benchmark
  def flatMap: Sketch[Double] = {
    sketch.flatMap(a => Dist.delta(a))
  }

  @Benchmark
  def rebuild: Sketch[Double] = {
    sketch.rebuild
  }

  @Benchmark
  def probability: Double = {
    sketch.probability(1, 2)
  }

  @Benchmark
  def count: Double = {
    sketch.count(1, 2)
  }

  @Benchmark
  def median: Double = {
    sketch.median
  }

  @Benchmark
  def sample: (Sketch[Double], Double) = {
    sketch.sample
  }

  @Benchmark
  def fastPdf: Double = {
    sketch.pdf(1)
  }

}

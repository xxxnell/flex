package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip._
import flip.cmap.Cmap
import flip.hcounter.HCounter

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SketchOpsBench { self =>

  // parameters

  @Param(Array("0", "30"))
  var queueSize: Int = _

  @Param(Array("2", "20"))
  var cmapNo: Int = _

  @Param(Array("20", "200"))
  var cmapSize: Int = _

  @Param(Array("2"))
  var counterNo: Int = _

  @Param(Array("100000"))
  var counterSize: Int = _

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
      counterSize = counterSize,
      counterNo = counterNo
    )
    val (_, samples) = NumericDist.normal(0.0, 1).samples(queueSize)
    val sketch0 = Sketch.empty[Double]

    self.conf = conf
    self.sketch = sketch0.narrowUpdate(samples: _*)
  }

  @Benchmark
  def construct: Sketch[Double] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50,
      thresholdPeriod = 100,
      queueSize = queueSize,
      cmapSize = cmapSize,
      cmapNo = cmapNo,
      cmapStart = Some(-10d),
      cmapEnd = Some(10d),
      counterSize = counterSize,
      counterNo = counterNo
    )

    Sketch.empty[Double]
  }

  @Benchmark
  def sampling: DensityPlot = {
    sketch.sampling
  }

  @Benchmark
  def narrowUpdate: Sketch[Double] = {
    sketch.narrowUpdate(1)
  }

  @Benchmark
  def deepUpdate: (Sketch[Double], Option[(Cmap, HCounter)]) = {
    sketch.deepUpdate(1.0 to 10.0 by 1.0: _*)
  }

  @Benchmark
  def flatMap: Sketch[Double] = {
    sketch.flatMap(a => Dist.delta(a))
  }

  @Benchmark
  def rearrange: Sketch[Double] = {
    sketch.rearrange
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

}

package flip.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import flip._
import flip.cmap.Cmap
import flip.hcounter.HCounter

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SketchOpsBench {

  @Param(Array("5", "20"))
  var cmapNo: Int = _

  @Param(Array("200", "2000"))
  var cmapSize: Int = _

  @Param(Array("2", "10"))
  var counterNo: Int = _

  @Param(Array("40", "1000"))
  var counterSize: Int = _

  var sketch: Sketch[Double] = _

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch = Sketch.empty[Double]
  }

  @Benchmark
  def construct: Sketch[Double] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    Sketch.empty[Double]
  }

  @Benchmark
  def sampling: Option[DensityPlot] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch.sampling
  }

  @Benchmark
  def narrowUpdate: Option[Sketch[Double]] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch.narrowUpdate(1)
  }

  @Benchmark
  def deepUpdate: Option[(Sketch[Double], Option[(Cmap, HCounter)])] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch.deepUpdate(1.0 to 10.0 by 1.0: _*)
  }

  @Benchmark
  def flatMap: Sketch[Double] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch.flatMap(a => Dist.delta(a))
  }

  @Benchmark
  def rearrange: Option[Sketch[Double]] = {
    implicit val conf: SketchConf = SketchConf(
      startThreshold = 50, thresholdPeriod = 100,
      cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = Some(-10d), cmapEnd = Some(10d),
      counterSize = counterSize, counterNo = counterNo
    )

    sketch.rearrange
  }

}

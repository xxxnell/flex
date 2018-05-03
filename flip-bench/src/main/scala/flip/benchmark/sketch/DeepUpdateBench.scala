package flip.benchmark.sketch

import java.util.concurrent.TimeUnit

import flip.implicits._
import flip.pdf.{Count, Structure}
import flip.{NumericDist, SketchConf}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class DeepUpdateBench { self =>

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

  @Benchmark
  def deepUpdate: (Sketch[Count], Option[Structure]) = {
    sketch.deepUpdate()
  }

  @Benchmark
  def rearrange: Sketch[Count] = {
    sketch.rearrange
  }

}

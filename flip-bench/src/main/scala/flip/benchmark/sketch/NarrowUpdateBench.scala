package flip.benchmark.sketch

import java.util.concurrent.TimeUnit

import flip.implicits._
import flip.pdf.{AdaPerSketch, AdaptiveSketch, Buffer, Count}
import flip.pdf.Buffer.syntax._
import flip.{NumericDist, Sketch, SketchConf}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class NarrowUpdateBench { self =>

  // parameters

  @Param(Array("5000"))
  var bufferSize: Int = _

  @Param(Array("3"))
  var cmapNo: Int = _

  @Param(Array("20"))
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

    self.conf = conf
    self.sketch = sketch0.narrowUpdate(samples: _*)
  }

  @Benchmark
  def narrowUpdate: Sketch[Double] = {
    sketch.narrowUpdate(1.0)
  }

  @Benchmark
  def primNarrowUpdateForStr: Sketch[Double] = {
    Sketch.primNarrowUpdateForStr(sketch, (1.0, 1.0) :: Nil)
  }

  @Benchmark
  def append: (AdaptiveSketch[Double], List[(Double, Count)]) = {
    AdaptiveSketch.append(sketch.asInstanceOf[AdaptiveSketch[Double]], (1.0, 1.0) :: Nil)
  }

  @Benchmark
  def modifyBuffer: AdaptiveSketch[Count] = {
    AdaptiveSketch.modifyBuffer(
      sketch.asInstanceOf[AdaptiveSketch[Double]],
      (buffer0: Buffer[Double]) => buffer0 :+ (2.0, 1.0))
  }

}

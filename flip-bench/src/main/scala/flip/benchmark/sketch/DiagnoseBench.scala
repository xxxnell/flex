package flip.benchmark.sketch

import java.util.concurrent.TimeUnit

import flip.conf.pdf.CustomAdaSelSketchConf
import flip.implicits._
import flip.pdf.{AdaptiveSketch, SelectiveSketch}
import flip.plot.PointPlot
import flip.pdf.Buffer.syntax._
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class DiagnoseBench { self =>

  // parameters

  @Param(Array("50"))
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
    implicit val conf: SketchConf = CustomAdaSelSketchConf(
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
      self.sketch = sketch0.narrowUpdate(samples: _*).rebuild
    }
    self.sketch = sketch0.narrowUpdate(samples: _*)
    self.conf = conf
  }

  @Benchmark
  def diagnose: Boolean = {
    SelectiveSketch.diagnose(sketch.asInstanceOf[SelectiveSketch[Double]])
  }

  @Benchmark
  def bufferCdf: PointPlot = {
    val measure = sketch.measure
    val bufferPrims =
      sketch.asInstanceOf[AdaptiveSketch[Double]].buffer.toList.map { case (a, count) => (measure.to(a), count) }
    PointPlot.normalizedCumulative(bufferPrims)
  }

  @Benchmark
  def samplingCdf: PointPlot = sketch.sampling.normalizedCumulative

  def cdfKld: Double = ???

}

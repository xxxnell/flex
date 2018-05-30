package flip.benchmark.sketch

import java.util.concurrent.TimeUnit

import flip.conf.pdf.CustomAdaSelSketchConf
import flip.implicits._
import flip.measure.Measure
import flip.pdf.{AdaptiveSketch, Count, Histogram, Prim, SelectiveSketch}
import flip.plot.PointPlot
import flip.pdf.Buffer.syntax._
import flip.pdf.diagnose.{CDFDiagnose, KLDDiagnose, KSDiagnose}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class DiagnoseBench { self =>

  // parameters

  @Param(Array("50", "5000"))
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

  lazy val measure: Measure[Count] = sketch.measure

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
  def bufferToList: List[(Prim, Count)] = {
    sketch
      .asInstanceOf[AdaptiveSketch[Double]]
      .buffer
      .toList
      .map { case (a, count) => (measure.to(a), count) }
      .sortBy(_._1)
  }

  lazy val _bufferToList: List[(Prim, Count)] = bufferToList

  @Benchmark
  def bufferCdf: PointPlot = {
    PointPlot.unsafeNormalizedCumulative(_bufferToList)
  }

  lazy val _bufferCdf: PointPlot = bufferCdf

  @Benchmark
  def youngStructure: Histogram[Double] = {
    sketch.structures.head.scanUpdate(_bufferToList)
  }

  lazy val _youngStructure: Histogram[Double] = youngStructure

  @Benchmark
  def youngSamplingCdf: PointPlot = {
    _youngStructure.cdfSampling
  }

  lazy val _youngSamplingCdf: PointPlot = youngSamplingCdf

  @Benchmark
  def ksDiagnose: Boolean = KSDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, 0.1)

  @Benchmark
  def kldDiagnose: Boolean = KLDDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, 0.1)

}

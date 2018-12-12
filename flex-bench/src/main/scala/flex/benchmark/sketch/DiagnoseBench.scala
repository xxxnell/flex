package flex.benchmark.sketch

import java.util.concurrent.TimeUnit

import flex.conf.pdf.CustomAdaSelSketchConf
import flex.implicits._
import flex.measure.Measure
import flex.pdf.{AdaptiveSketch, Count, Histogram, Prim, SelectiveSketch}
import flex.plot.PointPlot
import flex.pdf.Buffer.syntax._
import flex.pdf.diagnose.{CDFDiagnose, EDDiagnose, KLDDiagnose, KSDiagnose}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class DiagnoseBench { self =>

  // parameters

  @Param(Array("50", "5000"))
  var bufferSizeL: Int = _

  @Param(Array("3"))
  var cmapNoL: Int = _

  @Param(Array("20"))
  var cmapSizeL: Int = _

  @Param(Array("1"))
  var counterNoL: Int = _

  // variables

  implicit var conf: SketchConf = _

  var sketch: Sketch[Double] = _

  lazy val measure: Measure[Count] = sketch.measure

  @Setup
  def setupSketch(): Unit = {
    implicit val conf: SketchConf = CustomAdaSelSketchConf(
      startThreshold = Int.MaxValue,
      thresholdPeriod = Int.MaxValue,
      bufferSize = bufferSizeL,
      cmapSize = cmapSizeL,
      cmapNo = cmapNoL,
      counterNo = counterNoL
    )
    val (_, samples) = NumericDist.normal(0.0, 1).samples(bufferSizeL + 1)
    val sketch0 = Sketch.empty[Double]

    (0 until cmapNoL).foreach { _ =>
      self.sketch = sketch0.narrowUpdate(samples: _*).rebuild
    }
    self.sketch = sketch0.narrowUpdate(samples: _*)
    self.conf = conf
  }

//  @Benchmark
  def diagnose: Boolean = {
    SelectiveSketch.diagnose(sketch.asInstanceOf[SelectiveSketch[Double]])
  }

//  @Benchmark
  def bufferToList: List[(Prim, Count)] = {
    sketch
      .asInstanceOf[AdaptiveSketch[Double]]
      .buffer
      .toList
      .map { case (a, count) => (measure.to(a), count) }
      .sortBy(_._1)
  }

  lazy val _bufferToList: List[(Prim, Count)] = bufferToList

//  @Benchmark
  def bufferCdf: PointPlot = {
    PointPlot.unsafeNormalizedCumulative(_bufferToList)
  }

  lazy val _bufferCdf: PointPlot = bufferCdf

//  @Benchmark
  def youngStructure: Histogram[Double] = {
    sketch.structures.head.scanUpdate(_bufferToList)
  }

  lazy val _youngStructure: Histogram[Double] = youngStructure

//  @Benchmark
  def youngSamplingCdf: PointPlot = {
    _youngStructure.cdfSampling
  }

  lazy val _youngSamplingCdf: PointPlot = youngSamplingCdf

//  @Benchmark
//  def ksDiagnose1: Boolean = KSDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, 1.0)
//
//  @Benchmark
//  def ksDiagnose2: Boolean = KSDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, 0.0)

//  @Benchmark
//  def kldDiagnose: Boolean = KLDDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, 0.0)

  @Benchmark
  def d2Diagnose1: Boolean = EDDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, 0.0)

  @Benchmark
  def d2Diagnose2: Boolean = EDDiagnose.diagnose(_bufferCdf, _youngSamplingCdf, Double.MaxValue)

}

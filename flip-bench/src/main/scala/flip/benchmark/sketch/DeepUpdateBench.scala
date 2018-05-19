package flip.benchmark.sketch

import java.util.concurrent.TimeUnit

import flip.cmap.Cmap
import flip.hcounter.HCounter
import flip.implicits._
import flip.pdf.update.{EqUpdate, EqualSpaceSmoothingPs, SmoothingPs}
import flip.pdf.{AdaptiveSketch, Count, Prim, Sketch, Structure}
import flip.pdf.Buffer.syntax._
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
  def setup(): Unit = {
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
      self.sketch = sketch0.narrowUpdate(samples: _*).rebuild
    }
    self.sketch = sketch0.narrowUpdate(samples: _*)
    self.conf = conf
  }

  object DeepUpdateAlgorithm {

    lazy val ps: List[(Count, Count)] = sketch.asInstanceOf[AdaptiveSketch[Double]].buffer.toList

    lazy val cmapC: Cmap = cmap

    def cmap: Cmap = EqUpdate.updateCmapForSketch(sketch, ps)

    def seed: Int = ((Sketch.sum(sketch) + ps.headOption.map(_._1).getOrElse(-1d)) * 1000).toInt

    lazy val emptyCounterC: HCounter = emptyCounter

    def emptyCounter: HCounter = Sketch.counter(sketch.conf, seed)

    def strs: (List[(Cmap, HCounter)], List[(Cmap, HCounter)]) =
      ((cmapC, emptyCounterC) :: sketch.structures).toList.splitAt(sketch.conf.cmap.no)

    def updatePs(): Sketch[Count] =
      if (ps.nonEmpty) {
        Sketch.primSmoothingNarrowUpdateForStr(sketch, ps)
      } else sketch

  }

//  @Benchmark
//  def deepUpdate: (Sketch[Count], Option[Structure]) = {
//    sketch.deepUpdate()
//  }

//  @Benchmark
//  def primDeepUpdate: (Sketch[Count], Option[(Cmap, HCounter)]) = {
//    Sketch.primDeepUpdate(sketch, sketch.asInstanceOf[AdaptiveSketch[Double]].buffer.toList)
//  }

  @Benchmark
  def rebuild: Sketch[Count] = {
    sketch.rebuild
  }

//  @Benchmark
//  def cmap: Cmap = {
//    DeepUpdateAlgorithm.cmap
//  }
//
//  @Benchmark
//  def seed: Int = {
//    DeepUpdateAlgorithm.seed
//  }
//
//  @Benchmark
//  def emptyCounter: HCounter = {
//    DeepUpdateAlgorithm.emptyCounter
//  }
//  @Benchmark
//  def strs: (List[(Cmap, HCounter)], List[(Cmap, HCounter)]) = {
//    DeepUpdateAlgorithm.strs
//  }
//
//  @Benchmark
//  def smoothingPs: Dist[Prim] = {
//    DeepUpdateAlgorithm.smoothingPs
//  }
//
//  @Benchmark
//  def updatePs(): Sketch[Count] = {
//    DeepUpdateAlgorithm.updatePs()
//  }

}

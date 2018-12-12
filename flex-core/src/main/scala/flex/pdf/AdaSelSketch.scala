package flex.pdf

import flex.conf.pdf.AdaSelSketchConf
import flex.measure.Measure
import flex.pdf.Buffer.syntax._
import flex.pdf.diagnose.{CDFDiagnose, KLDDiagnose, KSDiagnose}
import flex.plot.PointPlot
import flex.rand.IRng
import flex.pdf.syntax._

import scala.language.higherKinds

trait AdaSelSketch[A] extends AdaptiveSketch[A] with SelectiveSketch[A] {

  def conf: AdaSelSketchConf

}

trait AdaSelSketchOps[S[_] <: AdaSelSketch[_]] extends AdaptiveSketchOps[S] with SelectiveSketchOps[S] {

  private val tester: CDFDiagnose = KSDiagnose

  def diagnose[A](sketch: S[A]): Boolean = {
    val threshold = sketch.conf.rebuildThreshold
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    val buffer = sketch.buffer.asInstanceOf[Buffer[A]]
    val bufferPrims = buffer.toList.map { case (a, count) => (measure.to(a), count) }.sortBy(_._1)
    val bufferCdf = PointPlot.unsafeNormalizedCumulative(bufferPrims)
    val youngStr1 = sketch.structures.head.scanUpdate(bufferPrims)
    val strCdf = youngStr1.cdfSampling

    tester.diagnose(bufferCdf, strCdf, threshold)
  }

}

object AdaSelSketch extends AdaSelSketchOps[AdaSelSketch] {

  private case class AdaSelSketchImpl[A](measure: Measure[A],
                                         rng: IRng,
                                         conf: AdaSelSketchConf,
                                         structures: Structures,
                                         buffer: Buffer[A],
                                         thresholds: Stream[Count],
                                         count: Count)
      extends AdaSelSketch[A]

  def bare[A](measure: Measure[A],
              rng: IRng,
              conf: AdaSelSketchConf,
              structures: Structures,
              buffer: Buffer[A],
              thresholds: Stream[Count],
              count: Count): AdaSelSketch[A] =
    AdaSelSketchImpl(measure, rng, conf, structures, buffer, thresholds, count)

  def empty[A](implicit measure: Measure[A], conf: AdaSelSketchConf): AdaSelSketch[A] =
    bare(
      measure,
      IRng(-1),
      conf,
      structures(conf),
      Buffer.empty[A],
      periodicThresholds(conf.startThreshold, conf.thresholdPeriod),
      0)

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: AdaSelSketchConf): AdaSelSketch[A] = {
    val emptySketch = bare(
      measure,
      IRng(-1),
      conf,
      concatStructures(as, measure, conf),
      Buffer.empty[A],
      periodicThresholds(conf.startThreshold, conf.thresholdPeriod),
      0)

    narrowUpdate(emptySketch, as)
  }

  def modifyRng[A](sketch: AdaSelSketch[A], f: IRng => IRng): AdaSelSketch[A] =
    bare(sketch.measure, f(sketch.rng), sketch.conf, sketch.structures, sketch.buffer, sketch.thresholds, sketch.count)

  def modifyStructures[A](sketch: AdaSelSketch[A], f: Structures => Structures): AdaSelSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, f(sketch.structures), sketch.buffer, sketch.thresholds, sketch.count)

  def modifyBuffer[A](sketch: AdaSelSketch[A], f: Buffer[A] => Buffer[A]): AdaSelSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, f(sketch.buffer), sketch.thresholds, sketch.count)

  def modifyThresholds[A](sketch: AdaSelSketch[A], f: Stream[Count] => Stream[Count]): AdaSelSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, sketch.buffer, f(sketch.thresholds), sketch.count)

  def modifyCount[A](sketch: AdaSelSketch[A], f: Count => Count): AdaSelSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, sketch.buffer, sketch.thresholds, f(sketch.count))

}

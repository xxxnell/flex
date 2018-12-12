package flex.pdf

import cats.data.NonEmptyList
import cats.implicits._
import flex.cmap.Cmap
import flex.conf.pdf._
import flex.hcounter.HCounter
import flex.measure.Measure
import flex.pdf.sampling.IcdfSampling
import flex.pdf.update.EqUpdate
import flex.plot.{DensityPlot, PointPlot}
import flex.rand.IRng
import flex.range.{RangeM, RangeP}

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.util.Try

/**
  * Sketch is a probabilistic summarization data structure that quantizes and
  * stores data streams. And It is a nonparametric density estimation algorithm.
  * However, unlike other quantization algorithms such as histogram, Sketch
  * picks quantization points adaptively for incoming data. Thus, Sketch does
  * not have distortion without requiring a prior knowledge of the incoming
  * dataset.
  */
trait Sketch[A] extends DataBinningDist[A] {

  /**
    * Internal structure list of Sketch. Order: young -> old
    * */
  def structures: Structures

  def conf: SketchConf

}

trait SketchPropOps[S[_] <: Sketch[_]] extends DataBinningDistOps[S] with SketchPropLaws[S] {

  // update ops

  def modifyStructures[A](sketch: S[A], f: Structures => Structures): S[A]

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): S[A]

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)]): (S[A], Option[Histogram[Double]])

}

trait SketchPropLaws[S[_] <: Sketch[_]] { self: SketchPropOps[S] =>

  def modifyEffStructure[A](sketch: S[A], f: Histogram[Double] => Histogram[Double]): S[A] = modifyStructures(
    sketch,
    strs => {
      val cmapNo = sketch.conf.cmap.no
      val effNo = if (cmapNo > 1) cmapNo - 1 else cmapNo
      var i = 0
      strs.map { hist =>
        if (i < effNo) {
          i += 1
          f(hist)
        } else hist
      }
    }
  )

  def flatDensity: Double = (1 / Cmap.max) * (1 / (1 - Cmap.min / Cmap.max))

  def probability[A](sketch: S[A], start: A, end: A): Double = {
    val count = self.count(sketch, start, end)
    val sum = self.sum(sketch)
    val measure = sketch.measure.asInstanceOf[Measure[A]]

    if (sum > 0) count / sum else flatDensity * RangeM.bare(start, end, measure).roughLength
  }

  def rebuild[A](sketch: S[A]): S[A] = deepUpdate(sketch, Nil)._1

  def rangePdfSampling[A](sketch: S[A]): DensityPlot = {
    rangePdfSamplingForRanges(sketch, samplingPoints(sketch))
  }

  def samplingPoints[A](sketch: S[A]): List[RangeM[A]] = {
    val cmap = youngCmap(sketch)
    val rangePs = cmap.bins
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    rangePs.map(rangeP => rangeP.modifyMeasure(measure))
  }

  def rangePdfSamplingForRanges[A](sketch: S[A], ranges: List[RangeM[A]]): DensityPlot = {
    val rangeProbs = ranges.map(range => (range, probability(sketch, range.start, range.end)))
    val rangeDensities = rangeProbs
      .map { case (rangeM, prob) => (RangeP.forRangeM(rangeM), Try(prob / rangeM.roughLength).toOption) }
      .flatMap { case (range, densityO) => densityO.map(density => (range, density)) }
    DensityPlot.disjoint(rangeDensities)
  }

  def cdfSampling[A](sketch: S[A]): PointPlot = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    val ranges = sketch.structures.head.cmap.binsArr
    val records = new ArrayBuffer[(Double, Double)]
    var i = 1

    val head = ranges.apply(0).modifyMeasure(measure)
    val last = ranges.apply(ranges.length - 1).modifyMeasure(measure)
    val sum = self.sum(sketch) - count(sketch, head.start, head.end) - count(sketch, last.start, last.end)
    var cum = 0.0
    records.append((head.primitivize.end, cum))
    while (i < ranges.length - 1) {
      val rangeP = ranges.apply(i)
      val rangeM = rangeP.modifyMeasure(measure)
      cum += count(sketch, rangeM.start, rangeM.end)
      if (sum != 0) records.append((rangeP.end, cum / sum))
      i += 1
    }

    PointPlot.unsafe(records.toArray)
  }

//  def fastPdf[A](sketch: S[A], a: A): Double = {
//    val cmap = youngCmap(sketch)
//    val p = sketch.measure.asInstanceOf[Measure[A]].to(a)
//    val idx = cmap(p)
//    val rangePs = cmap.range(idx - 1) :: cmap.range(idx) :: cmap.range(idx + 1) :: Nil
//    val rangeMs = rangePs.map(rangeP => rangeP.modifyMeasure(sketch.measure.asInstanceOf[Measure[A]]))
//    val sampling = rangeSamplingForRanges(sketch, rangeMs)
//
//    sampling.interpolation(p)
//  }

  override def pdf[A](dist: S[A], a: A): Count = interpolationPdf(dist, a) // fastPdf(dist, a)

  def median[A](sketch: S[A]): A = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    measure.from(icdfSampling(sketch).interpolation(0.5))
  }

  def cmapNo(sketch: S[_]): Int = sketch.structures.size.toInt

  def cmapSize(sketch: S[_]): Int =
    sketch.structures.head.cmap.size

  def counterNo(sketch: S[_]): Int =
    sketch.structures.head.counter.depth

  def counterSize(sketch: S[_]): Int =
    sketch.structures.head.counter.width

  def youngCmap(sketch: S[_]): Cmap =
    sketch.structures.head.cmap

  def domain[A](sketch: S[A]): RangeM[A] = {
    val youngCmap = self.youngCmap(sketch)
    val head = youngCmap.headRange.start
    val last = youngCmap.lastRange.end
    val measure = sketch.measure.asInstanceOf[Measure[A]]

    RangeM(measure.from(head), measure.from(last))(measure)
  }

  // construct

  def counter(conf: SketchConf, seed: Int): HCounter =
    if (conf.cmap.size > conf.counter.size) HCounter(conf.counter, -1)
    else HCounter.emptyUncompressed(conf.cmap.size)

  def structures(conf: SketchConf): Structures =
    NonEmptyList.of(Histogram.empty(flex.doubleMeasure, conf))

  def concatStructures[A](as: List[(A, Count)], measure: Measure[A], conf: SketchConf): Structures = {
    val ps = as.map { case (a, c) => (measure.to(a), c) }
    val cmap = EqUpdate.updateCmap(
      PointPlot.empty,
      ps,
      1000,
      conf.dataKernelWindow,
      IcdfSampling.samplingF(measure, conf.cmap),
      measure
    )

    NonEmptyList.of(Histogram.forCmap(cmap)(flex.doubleMeasure, conf))
  }

}

object Sketch extends SketchPrimPropOps[Sketch] { self =>

  /**
    * @param measure  measure of Sketch
    * */
  def empty[A](implicit measure: Measure[A], conf: SketchConf): Sketch[A] = conf match {
    case conf: PeriodicSketchConf => PeriodicSketch.empty(measure, conf)
    case _ => SimpleSketch.empty(measure, conf)
  }

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: SketchConf): Sketch[A] = conf match {
    case conf: PeriodicSketchConf => PeriodicSketch.concat(as)(measure, conf)
    case _ => SimpleSketch.concat(as)(measure, conf)
  }

  // mapping ops

  def modifyStructures[A](sketch: Sketch[A], f: Structures => Structures): Sketch[A] = sketch match {
    case sketch: RecurSketch[_] => RecurSketch.modifyStructures(sketch, f)
    case sketch: AdaptiveSketch[_] => AdaptiveSketch.modifyStructures(sketch, f)
    case _ => SimpleSketch.modifyStructures(sketch, f)
  }

  def modifyRng[A](sketch: Sketch[A], f: IRng => IRng): Sketch[A] = sketch match {
    case sketch: RecurSketch[_] => RecurSketch.modifyRng(sketch, f)
    case sketch: AdaptiveSketch[_] => AdaptiveSketch.modifyRng(sketch, f)
    case _ => SimpleSketch.modifyRng(sketch, f)

  }

  // syntatic sugars

  def update[A](sketch: Sketch[A], as: List[(A, Count)]): Sketch[A] = sketch match {
    case (sketch: RecurSketch[A]) => RecurSketch.update(sketch, as)
    case (sketch: AdaptiveSketch[A]) => AdaptiveSketch.update(sketch, as)
    case (sketch: SimpleSketch[A]) => SimpleSketch.update(sketch, as)
    case _ => narrowUpdate(sketch, as)
  }

  // overrides

  override def count[A](sketch: Sketch[A], start: A, end: A): Count = sketch match {
    case (sketch: AdaptiveSketch[A]) => AdaptiveSketch.count(sketch, start, end)
    case _ => super.count(sketch, start, end)
  }

  override def sum(sketch: Sketch[_]): Count = sketch match {
    case (sketch: AdaptiveSketch[_]) => AdaptiveSketch.sum(sketch)
    case _ => super.sum(sketch)
  }

  override def narrowUpdate[A](sketch: Sketch[A], as: List[(A, Count)]): Sketch[A] = sketch match {
    case (sketch: AdaptiveSketch[A]) => AdaptiveSketch.narrowUpdate(sketch, as)
    case _ => super.narrowUpdate(sketch, as)
  }

  override def rebuild[A](sketch: Sketch[A]): Sketch[A] = sketch match {
    case (sketch: AdaptiveSketch[A]) => AdaptiveSketch.rebuild(sketch)
    case _ => super.rebuild(sketch)
  }

}

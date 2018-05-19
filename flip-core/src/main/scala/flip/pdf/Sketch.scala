package flip.pdf

import cats.data.NonEmptyList
import cats.implicits._
import flip._
import flip.cmap.Cmap
import flip.conf.{PeriodicSketchConf, SketchConf}
import flip.hcounter.HCounter
import flip.measure.Measure
import flip.pdf.sampling.IcdfSampling
import flip.pdf.update.EqUpdate
import flip.plot.{DensityPlot, PointPlot}
import flip.rand.IRng
import flip.range.{RangeM, RangeP}

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

  // read ops

  /**
    * Get the number of effective elements be memorized.
    * */
  def count[A](sketch: S[A], from: A, to: A): Count

  def sum(sketch: S[_]): Count

  // update ops

  def modifyStructures[A](sketch: S[A], f: Structures => Structures): S[A]

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): S[A]

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)]): (S[A], Option[Structure])

  //  def clear(sketch: S): S

}

trait SketchPropLaws[S[_] <: Sketch[_]] { self: SketchPropOps[S] =>

  def modifyStructure[A](sketch: S[A], i: Int, f: Structure => Structure): S[A] = {
    modifyStructures(sketch, strs => {
      val _strs = strs.toList
      NonEmptyList.fromListUnsafe(_strs.updated(i, f(_strs.apply(i))))
    })
  }

  def flatDensity: Double = (1 / Cmap.max) * (1 / (1 - Cmap.min / Cmap.max))

  def probability[A](sketch: S[A], start: A, end: A): Double = {
    val count = self.count(sketch, start, end)
    val sum = self.sum(sketch)
    val measure = sketch.measure.asInstanceOf[Measure[A]]

    if (sum > 0) count / sum else flatDensity * RangeM.bare(start, end, measure).roughLength
  }

  def rebuild[A](sketch: S[A]): S[A] = deepUpdate(sketch, Nil)._1

  def sampling[A](sketch: S[A]): PointPlot = pointSampling(sketch)

  def rangeSampling[A](sketch: S[A]): DensityPlot = {
    rangeSamplingForRanges(sketch, samplingPoints(sketch))
  }

  def samplingPoints[A](sketch: S[A]): List[RangeM[A]] = {
    val cmap = youngCmap(sketch)
    val rangePs = cmap.bin
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    rangePs.map(rangeP => rangeP.modifyMeasure(measure))
  }

  def rangeSamplingForRanges[A](sketch: S[A], ranges: List[RangeM[A]]): DensityPlot = {
    val rangeProbs = ranges.map(range => (range, probability(sketch, range.start, range.end)))
    val rangeDensities = rangeProbs
      .map { case (rangeM, prob) => (RangeP.forRangeM(rangeM), Try(prob / rangeM.roughLength).toOption) }
      .flatMap { case (range, densityO) => densityO.map(density => (range, density)) }
    DensityPlot.disjoint(rangeDensities)
  }

  def pointSampling[A](sketch: S[A]): PointPlot = {
    val RATIO = 100.0 // todo why 100.0?
    val CUTOFF = 1E300 // todo CUTOFF is hacky approach

    val cmap = youngCmap(sketch)
    val rangePs = cmap.bin.filter(range => math.abs(range.start) < CUTOFF && math.abs(range.end) < CUTOFF).toArray
    val measure = sketch.measure.asInstanceOf[Measure[A]]

    var i = 0
    val records = new ArrayBuffer[(Double, Double)]
    while (i < rangePs.length) {
      lazy val rangeP0 = rangePs.apply(i - 1)
      lazy val rangeP1 = rangePs.apply(i)
      lazy val rangeM1 = rangeP1.modifyMeasure(measure)
      lazy val l1 = rangeP1.cutoffLength
      lazy val l0 = rangeP0.cutoffLength
      lazy val widthRatio = l1 / l0
      lazy val (_, prod0) = records.apply(records.length - 1)
      lazy val prod1O: Option[Double] =
        if (l1 != 0) Some(probability(sketch, rangeM1.start, rangeM1.end) / l1) else None

      prod1O.foreach(prod1 => {
        if (records.nonEmpty && widthRatio < (1 / RATIO) && l0 > 0) {
          val _prob = prod0 + (prod1 - prod0) * (l1 / l0)
          records.append((rangeP1.start, _prob))
        } else if (records.nonEmpty && widthRatio > RATIO && l1 > 0) {
          val _prob = prod1 + (prod0 - prod1) * (l0 / l1)
          records.append((rangeP1.start, _prob))
        }
        records.append((rangeP1.cutoffMiddle, prod1))
      })
      i += 1
    }

    PointPlot.safe(records.toArray)
  }

  def fastPdf[A](sketch: S[A], a: A): Double = {
    val cmap = youngCmap(sketch)
    val p = sketch.measure.asInstanceOf[Measure[A]].to(a)
    val idx = cmap(p)
    val rangePs = cmap.range(idx - 1) :: cmap.range(idx) :: cmap.range(idx + 1) :: Nil
    val rangeMs = rangePs.map(rangeP => rangeP.modifyMeasure(sketch.measure.asInstanceOf[Measure[A]]))
    val sampling = rangeSamplingForRanges(sketch, rangeMs)

    sampling.interpolation(p)
  }

  override def pdf[A](dist: S[A], a: A): Count = fastPdf(dist, a)

  def median[A](sketch: S[A]): A = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]

    measure.from(icdfPlot(sketch).interpolation(0.5))
  }

  def cmapNo(sketch: S[_]): Int = sketch.structures.size.toInt

  def cmapSize(sketch: S[_]): Int =
    sketch.structures.head._1.size

  def counterNo(sketch: S[_]): Int =
    sketch.structures.head._2.depth

  def counterSize(sketch: S[_]): Int =
    sketch.structures.head._2.width

  def youngCmap(sketch: S[_]): Cmap =
    sketch.structures.head._1

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
    NonEmptyList.of((Cmap(conf.cmap), counter(conf, -1)))

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

    NonEmptyList.of((cmap, counter(conf, -1)))
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

  def concat[A](ps: List[(A, Count)])(implicit measure: Measure[A], conf: SketchConf): Sketch[A] = conf match {
    case conf: PeriodicSketchConf => PeriodicSketch.concat(ps)(measure, conf)
    case _ => SimpleSketch.concat(ps)(measure, conf)
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

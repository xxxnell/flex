package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.{AdaptiveSketchConf, PeriodicSketchConf, SketchConf}
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot

import scala.language.higherKinds
import scala.util.Try
import cats.implicits._
import sketch.scope.range.RangeP

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] extends DataBinningDist[A] {

  /**
    * Internal structure list of Sketch. Order: young -> old
    * */
  def structures: Structures

}

trait SketchPropOps[S[_]<:Sketch[_], C<:SketchConf]
  extends DataBinningDistOps[S, C]
    with SketchPropLaws[S, C] {

  // create ops

  def sample[A](dist: S[A]): (S[A], A)

  // read ops

  /**
    * Get the number of effective elements be memorized.
    * */
  def count[A](sketch: S[A], from: A, to: A): Option[Count]

  def sum(sketch: S[_]): Count

  // update ops

  def modifyStructure[A](sketch: S[A], f: Structures => Option[Structures]): Option[S[A]]

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]]

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[(S[A], Option[Structure])]

  //  def clear(sketch: S): S

}

trait SketchPropLaws[S[_]<:Sketch[_], C<:SketchConf] { self: SketchPropOps[S, C] =>

  def probability[A](sketch: S[A], start: A, end: A): Option[Double] = for {
    count <- count(sketch, start, end)
    sum = self.sum(sketch)
  } yield count / sum

  def sampling[A](sketch: S[A]): Option[DensityPlot] = for {
    cmapHcounter <- sketch.structures.lastOption
    (cmap, _) = cmapHcounter
    rangePs = cmap.bin
    rangeMs = rangePs.map(rangeP => rangeP.modifyMeasure(sketch.measure.asInstanceOf[Measure[A]]))
    rangeProbs <- rangeMs.traverse(rangeM => probability(sketch, rangeM.start, rangeM.end).map(prob => (rangeM, prob)))
    rangeDensities = rangeProbs
      .map { case (rangeM, prob) => (RangeP.forRangeM(rangeM), Try(prob / rangeM.length).toOption) }
      .flatMap { case (range, densityO) => densityO.map(density => (range, density.toDouble)) }
  } yield DensityPlot.disjoint(rangeDensities)

  def rearrange[A](sketch: S[A], conf: C): Option[S[A]] = deepUpdate(sketch, Nil, conf).map(_._1)

  def cmapNo(sketch: S[_]): Int = sketch.structures.size

  def cmapSize(sketch: S[_]): Int = (for {
    structure <- sketch.structures.headOption
    (cmap, _) = structure
  } yield cmap.size).getOrElse(0)

  def counterNo(sketch: S[_]): Int = (for {
    structure <- sketch.structures.headOption
    (_, hcounter) = structure
  } yield hcounter.depth).getOrElse(0)

  def counterSize(sketch: S[_]): Int = (for {
    structure <- sketch.structures.headOption
    (_, hcounter) = structure
  } yield hcounter.width).getOrElse(0)

  def youngCmap(sketch: S[_]): Option[Cmap] = for {
    structure <- sketch.structures.headOption
    cmap = structure._1
  } yield cmap

  def conf2Structures(conf: C): Structures =
    List((Cmap(conf.cmap), HCounter(conf.counter, -1)))

}

object Sketch extends SketchPrimPropOps[Sketch, SketchConf] { self =>

  def apply[A](measure: Measure[A], structure: Structures): Sketch[A] =
    SimpleSketch(measure, structure)

  /**
    * @param measure  measure of Sketch
    * */
  def empty[A](implicit measure: Measure[A], conf: SketchConf): Sketch[A] = conf match {
    case conf: PeriodicSketchConf => PeriodicSketch.empty(measure, conf)
    case _ => SimpleSketch.empty(measure, conf)
  }

  // mapping ops

  def modifyStructure[A](sketch: Sketch[A], f: Structures => Option[Structures]): Option[Sketch[A]] = sketch match {
    case sketch: RecurSketch[_] => RecurSketch.modifyStructure(sketch, f)
    case sketch: AdaptiveSketch[_] => AdaptiveSketch.modifyStructure(sketch, f)
    case _ => SimpleSketch.modifyStructure(sketch, f)
  }

  // syntatic sugars

  def update[A](sketch: Sketch[A], as: List[(A, Count)], conf: SketchConf): Option[Sketch[A]] = (sketch, conf) match {
    case (sketch: RecurSketch[A], _) => RecurSketch.update(sketch, as, conf)
    case (sketch: AdaptiveSketch[A], conf: AdaptiveSketchConf) => AdaptiveSketch.update(sketch, as, conf)
    case (sketch: SimpleSketch[A], _) => SimpleSketch.update(sketch, as, conf)
    case _ => narrowUpdate(sketch, as, conf)
  }

  def pdf[A](sketch: Sketch[A], a: A): Option[Count] = sketch match {
    case sketch: AdaptiveSketch[A] => AdaptiveSketch.fastPdf(sketch, a)
    case _ => super.fastPdf(sketch, a)
  }

  // overrides

  override def probability[A](sketch: Sketch[A], start: A, end: A): Option[Count] = sketch match {
    case sketch: AdaptiveSketch[A] => AdaptiveSketch.probability(sketch, start, end)
    case _ => super.probability(sketch, start, end)
  }

  override def sampling[A](sketch: Sketch[A]): Option[DensityPlot] = sketch match {
    case sketch: AdaptiveSketch[A] => AdaptiveSketch.sampling(sketch)
    case _ => super.sampling(sketch)
  }

  override def count[A](sketch: Sketch[A], start: A, end: A): Option[Count] = sketch match {
    case sketch: AdaptiveSketch[A] => AdaptiveSketch.count(sketch, start, end)
    case _ => super.count(sketch, start, end)
  }

  override def sum(sketch: Sketch[_]): Count = sketch match {
    case sketch: AdaptiveSketch[_] => AdaptiveSketch.sum(sketch)
    case _ => super.sum(sketch)
  }


  override def narrowUpdate[A](sketch: Sketch[A],
                               as: List[(A, Count)],
                               conf: SketchConf): Option[Sketch[A]] = (sketch, conf) match {
    case (sketch: AdaptiveSketch[A], conf: AdaptiveSketchConf) => AdaptiveSketch.narrowUpdate(sketch, as, conf)
    case _ => super.narrowUpdate(sketch, as, conf)
  }

  override def rearrange[A](sketch: Sketch[A], conf: SketchConf): Option[Sketch[A]] = (sketch, conf) match {
    case (sketch: AdaptiveSketch[A], conf: AdaptiveSketchConf) => AdaptiveSketch.rearrange(sketch, conf)
    case _ => super.rearrange(sketch, conf)
  }

}
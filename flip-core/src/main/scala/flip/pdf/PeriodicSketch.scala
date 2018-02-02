package flip.pdf

import flip.cmap.Cmap
import flip.conf.{AdaPerSketchConf, PeriodicSketchConf, SketchConf}
import flip.hcounter.HCounter
import flip.measure.Measure

import scala.language.higherKinds

trait PeriodicSketch[A] extends RecurSketch[A] {

  def conf: PeriodicSketchConf

}

trait PeriodicSketchOps[S[_]<:PeriodicSketch[_]] extends RecurSketchOps[S] {

  def thresholds(start: Double, period: Double): Stream[Count] = Stream.from(0).map(i => start + period * i)

}

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch] {

  case class PeriodicSketchImpl[A](measure: Measure[A],
                                   conf: PeriodicSketchConf,
                                   structures: List[(Cmap, HCounter)],
                                   thresholds: Stream[Count],
                                   count: Count)
    extends PeriodicSketch[A]

  def bare[A](measure: Measure[A],
              conf: PeriodicSketchConf,
              structures: List[(Cmap, HCounter)],
              thresholds: Stream[Count],
              count: Count): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, conf, structures, thresholds, count)

  def empty[A](implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = conf match {
    case conf: AdaPerSketchConf => AdaPerSketch.empty(measure, conf)
    case _ => bare(measure, conf, structures(conf), thresholds(conf.startThreshold, conf.thresholdPeriod), 0)
  }

  def concat[A](as: List[(A, Count)])
               (implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = conf match {
    case conf: AdaPerSketchConf => AdaPerSketch.concat(as)(measure, conf)
    case _ => narrowUpdate(bare(
      measure, conf, concatStructures(as, measure, conf),
      thresholds(conf.startThreshold, conf.thresholdPeriod), 0
    ), as).get
  }

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
    case _ => f(sketch.structures).map(utdStrs =>
      bare(sketch.measure, sketch.conf, utdStrs, sketch.thresholds, sketch.count)
    )
  }

  def modifyThresholds[A](sketch: PeriodicSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyThresholds(sketch, f)
    case _ => f(sketch.thresholds)
      .map(utdThrs => bare(sketch.measure, sketch.conf, sketch.structures, utdThrs, sketch.count))
  }

  def modifyCount[A](sketch: PeriodicSketch[A], f: Count => Count): PeriodicSketch[A] = sketch match {
    case adaper: AdaPerSketch[A] => AdaPerSketch.modifyCount(adaper, f)
    case _ => bare(sketch.measure, sketch.conf, sketch.structures, sketch.thresholds, f(sketch.count))
  }

  override def update[A](sketch: PeriodicSketch[A],
                         as: List[(A, Count)]): Option[PeriodicSketch[A]] = sketch match {
    case (sketch: AdaPerSketch[A]) => AdaPerSketch.update(sketch, as)
    case _ => super.update(sketch, as)
  }

}
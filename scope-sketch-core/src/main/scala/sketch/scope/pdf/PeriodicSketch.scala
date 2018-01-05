package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.{AdaPerSketchConf, PeriodicSketchConf, SketchConf}
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

import scala.language.higherKinds

trait PeriodicSketch[A] extends RecurSketch[A] {

}

trait PeriodicSketchOps[S[_]<:PeriodicSketch[_], C<:PeriodicSketchConf] extends RecurSketchOps[S, C] {

  def thresholds(start: Double, period: Double): Stream[Count] = Stream.from(0).map(i => start + period * i)

}

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch, PeriodicSketchConf] {

  case class PeriodicSketchImpl[A](measure: Measure[A],
                                   structures: List[(Cmap, HCounter)],
                                   thresholds: Stream[Count],
                                   count: Count)
    extends PeriodicSketch[A]

  def bare[A](measure: Measure[A],
              structures: List[(Cmap, HCounter)],
              thresholds: Stream[Count],
              count: Count): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, structures, thresholds, count)

  def empty[A](implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = conf match {
    case conf: AdaPerSketchConf => AdaPerSketch.empty[A](measure, conf)
    case _ => bare(measure, conf2Structures(conf), thresholds(conf.startThreshold, conf.thresholdPeriod), 0)
  }

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
    case _ => f(sketch.structures).map(utdStrs =>
      bare(sketch.measure, utdStrs, sketch.thresholds, sketch.count)
    )
  }

  def modifyThresholds[A](sketch: PeriodicSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyThresholds(sketch, f)
    case _ => f(sketch.thresholds).map(utdThrs => bare(sketch.measure, sketch.structures, utdThrs, sketch.count))
  }

  def modifyCount[A](sketch: PeriodicSketch[A], f: Count => Count): PeriodicSketch[A] = sketch match {
    case adaper: AdaPerSketch[A] => AdaPerSketch.modifyCount(adaper, f)
    case _ => bare(sketch.measure, sketch.structures, sketch.thresholds, f(sketch.count))
  }

  override def update[A](sketch: PeriodicSketch[A],
                         as: List[(A, Count)],
                         conf: PeriodicSketchConf): Option[PeriodicSketch[A]] = (sketch, conf) match {
    case (sketch: AdaPerSketch[A], conf: AdaPerSketchConf) => AdaPerSketch.update(sketch, as, conf)
    case _ => super.update(sketch, as, conf)
  }

}
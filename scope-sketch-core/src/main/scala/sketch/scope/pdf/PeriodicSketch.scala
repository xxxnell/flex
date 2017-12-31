package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.{AdaPerSketchConf, PeriodicSketchConf, SketchConf}
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait PeriodicSketch[A] extends RecurSketch[A] {

  val start: Double

  val period: Double

  val thresholds: Stream[Double] = Stream.from(0).map(i => start + period * i)

}

trait PeriodicSketchOps[S[_]<:PeriodicSketch[_], C<:PeriodicSketchConf] extends RecurSketchOps[S, C]

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch, PeriodicSketchConf] {

  case class PeriodicSketchImpl1[A](measure: Measure[A],
                                    structures: List[(Cmap, HCounter)],
                                    start: Double,
                                    period: Double)
    extends PeriodicSketch[A]

  case class PeriodicSketchImpl2[A](measure: Measure[A],
                                    structures: List[(Cmap, HCounter)],
                                    start: Double,
                                    period: Double,
                                    override val thresholds: Stream[Count])
    extends PeriodicSketch[A]

  def bare[A](measure: Measure[A],
              structures: List[(Cmap, HCounter)],
              start: Double,
              period: Double): PeriodicSketch[A] = PeriodicSketchImpl1(measure, structures, start, period)

  def empty[A](implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = conf match {
    case conf: AdaPerSketchConf => AdaPerSketch.empty[A](measure, conf)
    case _ => bare(measure, conf2Structures(conf), conf.startThreshold, conf.thresholdPeriod)
  }

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
    case _ => f(sketch.structures).map(utdStrs =>
      PeriodicSketchImpl1(sketch.measure, utdStrs, sketch.start, sketch.period)
    )
  }

  def modifyThresholds[A](sketch: PeriodicSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyThresholds(sketch, f)
    case _ => f(sketch.thresholds).map(utdThrs =>
      PeriodicSketchImpl2(sketch.measure, sketch.structures,
        sketch.start, sketch.period, utdThrs)
    )
  }

  def sample[A](sketch: PeriodicSketch[A]): (PeriodicSketch[A], A) = ???

  override def update[A](sketch: PeriodicSketch[A],
                         as: List[(A, Count)],
                         conf: PeriodicSketchConf): Option[PeriodicSketch[A]] = (sketch, conf) match {
    case (sketch: AdaPerSketch[A], conf: AdaPerSketchConf) => AdaPerSketch.update(sketch, as, conf)
    case _ => super.update(sketch, as, conf)
  }

}
package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.PeriodicSketchConf
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

trait PeroidocSketchOps[S[_]<:PeriodicSketch[_], C<:PeriodicSketchConf] extends RecurSketchOps[S, C]

object PeriodicSketch extends PeroidocSketchOps[PeriodicSketch, PeriodicSketchConf] {

  case class PeriodicSketchImpl[A](measure: Measure[A],
                                   structures: List[(Cmap, HCounter)],
                                   start: Double,
                                   period: Double)
    extends PeriodicSketch[A]

//  val defaultPeriod = 100

  def bare[A](measure: Measure[A],
              structures: List[(Cmap, HCounter)],
              start: Double,
              period: Double): PeriodicSketch[A] = PeriodicSketchImpl(measure, structures, start, period)

  def empty[A](implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = {
    bare(measure, conf2Structures(conf), conf.startThreshold, conf.thresholdPeriod)
  }

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] = sketch match {
    case _ => ???
  }

  def modifyThresholds[A](sketch: PeriodicSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] = sketch match {
    case _ => ???
  }

  def sample[A](sketch: PeriodicSketch[A]): (PeriodicSketch[A], A) = sketch match {
    case _ => ???
  }

}
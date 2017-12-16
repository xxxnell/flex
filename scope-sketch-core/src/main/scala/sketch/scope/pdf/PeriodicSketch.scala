package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.SketchConf
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
trait PeriodicSketch[A] extends RecurSketch[A] {

  val period: Double

  val thresholds: Stream[Double] = Stream.from(1).map(i => period * i)

}

object PeriodicSketch extends RecurSketchOps[PeriodicSketch] {

  case class PeriodicSketchImpl[A](measure: Measure[A],
                                   structures: List[(Cmap, HCounter)],
                                   conf: SketchConf,
                                   period: Double)
    extends PeriodicSketch[A]

  val defaultPeriod = 100

  def bare[A](measure: Measure[A],
             structures: List[(Cmap, HCounter)],
             conf: SketchConf,
             period: Double): PeriodicSketch[A] = PeriodicSketchImpl(measure, structures, conf, period)

  def empty[A](implicit measure: Measure[A], conf: SketchConf): PeriodicSketch[A] = emptyForPeriod(defaultPeriod)

  def emptyForPeriod[A](period: Double)(implicit measure: Measure[A], conf: SketchConf): PeriodicSketch[A] = {
    val structure = (1 to conf.cmap.no).toList
      .map(_ => (Cmap(conf.cmap), HCounter(conf.counter)))
    bare(measure, structure, conf, period)
  }

  def modifyThresholds[A](sketch: PeriodicSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] = ???

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] = ???

  def sample[A](dist: PeriodicSketch[A]): (PeriodicSketch[A], A) = ???

}
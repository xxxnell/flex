package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.SketchConf
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
trait PeriodicSketch[A] extends RecurSketch[A] {

  val start: Double

  val period: Double

  val thresholds: Stream[Double] = Stream.from(0).map(i => start + period * i)

}

object PeriodicSketch {

  case class PeriodicSketchImpl[A](measure: Measure[A],
                                   structures: List[(Cmap, HCounter)],
                                   conf: SketchConf,
                                   start: Double,
                                   period: Double)
    extends PeriodicSketch[A]

  val defaultPeriod = 100

  def bare[A](measure: Measure[A],
              structures: List[(Cmap, HCounter)],
              conf: SketchConf,
              start: Double,
              period: Double): PeriodicSketch[A] = PeriodicSketchImpl(measure, structures, conf, start, period)

  def empty[A](implicit measure: Measure[A], conf: SketchConf): PeriodicSketch[A] =
    emptyForPeriod(defaultPeriod, defaultPeriod)

  def emptyForPeriod[A](start: Double, period: Double)
                       (implicit measure: Measure[A], conf: SketchConf): PeriodicSketch[A] = {
    val structure = (1 to conf.cmap.no).toList
      .map(i => (Cmap(conf.cmap), HCounter(conf.counter, ~i)))
    bare(measure, structure, conf, start, period)
  }

}
package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.AdaPerSketchConf
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

import scala.language.higherKinds

/**
  * Adaptive and Periodic Sketch.
  * */
trait AdaPerSketch[A]
  extends AdaptiveSketch[A]
    with PeriodicSketch[A]

trait AdaPerSketchOps[S[_]<:AdaPerSketch[_], C<:AdaPerSketchConf]
  extends AdaptiveSketchOps[S, C]
    with PeriodicSketchOps[S, C] {

}

object AdaPerSketch extends AdaPerSketchOps[AdaPerSketch, AdaPerSketchConf] {

  private case class AdaPerSketchImpl1[A](measure: Measure[A],
                                          structures: List[(Cmap, HCounter)],
                                          queue: List[(A, Count)],
                                          start: Double,
                                          period: Double)
    extends AdaPerSketch[A]

  private case class AdaPerSketchImpl2[A](measure: Measure[A],
                                          structures: List[(Cmap, HCounter)],
                                          queue: List[(A, Count)],
                                          start: Double,
                                          period: Double,
                                          override val thresholds: Stream[Count])
    extends AdaPerSketch[A]

  def empty[A](implicit measure: Measure[A], conf: AdaPerSketchConf): AdaPerSketch[A] =
    AdaPerSketchImpl1(measure, conf2Structures(conf),
      List.empty[(A, Count)], conf.startThreshold, conf.thresholdPeriod)

  def modifyQueue[A](sketch: AdaPerSketch[A],
                     f: List[(A, Count)] => List[(A, Count)]): AdaPerSketch[A] =
    AdaPerSketchImpl1(sketch.measure, sketch.structures, f(sketch.queue), sketch.start, sketch.period)

  def modifyStructure[A](sketch: AdaPerSketch[A],
                         f: Structures => Option[Structures]): Option[AdaPerSketch[A]] = f(sketch.structures)
    .map(utdStr => AdaPerSketchImpl1(sketch.measure, utdStr, sketch.queue, sketch.start, sketch.period))

  def modifyThresholds[A](sketch: AdaPerSketch[A],
                          f: Stream[Count] => Option[Stream[Count]]): Option[AdaPerSketch[A]] = f(sketch.thresholds)
    .map(thr =>
      AdaPerSketchImpl2(sketch.measure, sketch.structures, sketch.queue, sketch.start, sketch.period, thr)
    )

}

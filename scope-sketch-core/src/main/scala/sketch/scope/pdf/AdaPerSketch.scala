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

  private case class AdaPerSketchImpl[A](measure: Measure[A],
                                         structures: List[(Cmap, HCounter)],
                                         queue: List[(A, Count)],
                                         thresholds: Stream[Count],
                                         count: Count)
    extends AdaPerSketch[A]

  def empty[A](implicit measure: Measure[A], conf: AdaPerSketchConf): AdaPerSketch[A] =
    AdaPerSketchImpl(measure, conf2Structures(conf),
      List.empty[(A, Count)], thresholds(conf.startThreshold, conf.thresholdPeriod), 0)

  def modifyQueue[A](sketch: AdaPerSketch[A],
                     f: List[(A, Count)] => List[(A, Count)]): AdaPerSketch[A] =
    AdaPerSketchImpl(sketch.measure, sketch.structures, f(sketch.queue), sketch.thresholds, sketch.count)

  def modifyStructure[A](sketch: AdaPerSketch[A],
                         f: Structures => Option[Structures]): Option[AdaPerSketch[A]] = f(sketch.structures)
    .map(utdStr => AdaPerSketchImpl(sketch.measure, utdStr, sketch.queue, sketch.thresholds, sketch.count))

  def modifyThresholds[A](sketch: AdaPerSketch[A],
                          f: Stream[Count] => Option[Stream[Count]]): Option[AdaPerSketch[A]] = f(sketch.thresholds)
    .map(thr => AdaPerSketchImpl(sketch.measure, sketch.structures, sketch.queue, thr, sketch.count))

  def modifyCount[A](sketch: AdaPerSketch[A], f: Count => Count): AdaPerSketch[A] =
    AdaPerSketchImpl(sketch.measure, sketch.structures, sketch.queue, sketch.thresholds, f(sketch.count))

}

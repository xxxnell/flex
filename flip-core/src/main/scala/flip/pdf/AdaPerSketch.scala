package flip.pdf

import flip.cmap.Cmap
import flip.conf.{AdaPerSketchConf, PeriodicSketchConf}
import flip.hcounter.HCounter
import flip.measure.Measure

import scala.language.higherKinds

/**
  * Adaptive and Periodic Sketch.
  * */
trait AdaPerSketch[A]
  extends AdaptiveSketch[A]
    with PeriodicSketch[A] {

  def conf: AdaPerSketchConf

}

trait AdaPerSketchOps[S[_]<:AdaPerSketch[_]]
  extends AdaptiveSketchOps[S]
    with PeriodicSketchOps[S] {

}

object AdaPerSketch extends AdaPerSketchOps[AdaPerSketch] {

  private case class AdaPerSketchImpl[A](measure: Measure[A],
                                         conf: AdaPerSketchConf,
                                         structures: List[(Cmap, HCounter)],
                                         queue: List[(A, Count)],
                                         thresholds: Stream[Count],
                                         count: Count)
    extends AdaPerSketch[A]

  def bare[A](measure: Measure[A],
              conf: AdaPerSketchConf,
              structures: List[(Cmap, HCounter)],
              queue: List[(A, Count)],
              thresholds: Stream[Count],
              count: Count): AdaPerSketch[A] =
    AdaPerSketchImpl(measure, conf, structures, queue, thresholds, count)

  def empty[A](implicit measure: Measure[A], conf: AdaPerSketchConf): AdaPerSketch[A] =
    bare(measure, conf, structures(conf),
      List.empty[(A, Count)], thresholds(conf.startThreshold, conf.thresholdPeriod), 0)

  def concat[A](as: List[(A, Count)])
               (implicit measure: Measure[A], conf: AdaPerSketchConf): AdaPerSketch[A] = {
    val emptySketch = bare(measure, conf, concatStructures(as, measure, conf),
      List.empty[(A, Count)], thresholds(conf.startThreshold, conf.thresholdPeriod), 0)

    narrowUpdate(emptySketch, as).get
  }

  def modifyQueue[A](sketch: AdaPerSketch[A],
                     f: List[(A, Count)] => List[(A, Count)]): AdaPerSketch[A] =
    AdaPerSketchImpl(sketch.measure, sketch.conf, sketch.structures, f(sketch.queue), sketch.thresholds, sketch.count)

  def modifyStructure[A](sketch: AdaPerSketch[A],
                         f: Structures => Option[Structures]): Option[AdaPerSketch[A]] = f(sketch.structures)
    .map(utdStr => AdaPerSketchImpl(sketch.measure, sketch.conf, utdStr, sketch.queue, sketch.thresholds, sketch.count))

  def modifyThresholds[A](sketch: AdaPerSketch[A],
                          f: Stream[Count] => Option[Stream[Count]]): Option[AdaPerSketch[A]] = f(sketch.thresholds)
    .map(thr => AdaPerSketchImpl(sketch.measure, sketch.conf, sketch.structures, sketch.queue, thr, sketch.count))

  def modifyCount[A](sketch: AdaPerSketch[A], f: Count => Count): AdaPerSketch[A] =
    AdaPerSketchImpl(sketch.measure, sketch.conf, sketch.structures, sketch.queue, sketch.thresholds, f(sketch.count))

}

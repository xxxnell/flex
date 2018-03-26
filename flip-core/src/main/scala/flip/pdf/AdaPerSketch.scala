package flip.pdf

import flip.conf.AdaPerSketchConf
import flip.measure.Measure
import flip.rand.IRng

import scala.language.higherKinds

/**
  * AdaPerSketch, or Adaptive and Periodic Sketch is both AdaptiveSketch and
  * PeriodicSketch.
  * */
trait AdaPerSketch[A] extends AdaptiveSketch[A] with PeriodicSketch[A] {

  def conf: AdaPerSketchConf

}

trait AdaPerSketchOps[S[_] <: AdaPerSketch[_]] extends AdaptiveSketchOps[S] with PeriodicSketchOps[S] {}

object AdaPerSketch extends AdaPerSketchOps[AdaPerSketch] {

  private case class AdaPerSketchImpl[A](measure: Measure[A],
                                         rng: IRng,
                                         conf: AdaPerSketchConf,
                                         structures: Structures,
                                         queue: List[(A, Count)],
                                         thresholds: Stream[Count],
                                         count: Count)
      extends AdaPerSketch[A]

  def bare[A](measure: Measure[A],
              rng: IRng,
              conf: AdaPerSketchConf,
              structures: Structures,
              queue: List[(A, Count)],
              thresholds: Stream[Count],
              count: Count): AdaPerSketch[A] =
    AdaPerSketchImpl(measure, rng, conf, structures, queue, thresholds, count)

  def empty[A](implicit measure: Measure[A], conf: AdaPerSketchConf): AdaPerSketch[A] =
    bare(
      measure,
      IRng(-1),
      conf,
      structures(conf),
      List.empty[(A, Count)],
      thresholds(conf.startThreshold, conf.thresholdPeriod),
      0)

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: AdaPerSketchConf): AdaPerSketch[A] = {
    val emptySketch = bare(
      measure,
      IRng(-1),
      conf,
      concatStructures(as, measure, conf),
      List.empty[(A, Count)],
      thresholds(conf.startThreshold, conf.thresholdPeriod),
      0)

    narrowUpdate(emptySketch, as)
  }

  def modifyRng[A](sketch: AdaPerSketch[A], f: IRng => IRng): AdaPerSketch[A] =
    bare(sketch.measure, f(sketch.rng), sketch.conf, sketch.structures, sketch.queue, sketch.thresholds, sketch.count)

  def modifyStructure[A](sketch: AdaPerSketch[A], f: Structures => Structures): AdaPerSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, f(sketch.structures), sketch.queue, sketch.thresholds, sketch.count)

  def modifyQueue[A](sketch: AdaPerSketch[A], f: List[(A, Count)] => List[(A, Count)]): AdaPerSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, f(sketch.queue), sketch.thresholds, sketch.count)

  def modifyThresholds[A](sketch: AdaPerSketch[A], f: Stream[Count] => Stream[Count]): AdaPerSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, sketch.queue, f(sketch.thresholds), sketch.count)

  def modifyCount[A](sketch: AdaPerSketch[A], f: Count => Count): AdaPerSketch[A] =
    bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, sketch.queue, sketch.thresholds, f(sketch.count))

}

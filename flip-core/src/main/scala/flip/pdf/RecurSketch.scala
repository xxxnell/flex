package flip.pdf

import flip.conf.SketchConf
import flip.measure.Measure
import flip.rand.IRng

import scala.language.higherKinds

/**
  * RecurSketch, or Recurrent Sketch has a stream of thresholds. RecurSketch
  * consumes the thresholds and rebuild itself when its counter for
  * number of data exceeds the threshold.
  * */
trait RecurSketch[A] extends Sketch[A] {

  def count: Count

  def thresholds: Stream[Double]

}

trait RecurSketchOps[S[_] <: RecurSketch[_]] extends SketchPrimPropOps[S] with RecurSketchLaws[S] { self =>

  def modifyThresholds[A](sketch: S[A], f: Stream[Double] => Stream[Double]): S[A]

  def modifyCount[A](sketch: S[A], f: Count => Count): S[A]

}

trait RecurSketchLaws[S[_] <: RecurSketch[_]] { self: RecurSketchOps[S] =>

  def dropThreshold[A](sketch: S[A]): S[A] = modifyThresholds(sketch, thresholds => thresholds.drop(1))

  def update[A](sketch: S[A], as: List[(A, Count)]): S[A] = {
    val nextThreshold = sketch.thresholds.headOption.getOrElse(Double.PositiveInfinity)
    val utdSketch1 = narrowUpdate[A](sketch, as)
    val utdSketch2 = modifyCount(utdSketch1, count => count + as.map(_._2).sum)
    val utdSketch3 = if (nextThreshold <= utdSketch2.count) dropThreshold(rebuild(utdSketch2)) else utdSketch2
    utdSketch3
  }

}

object RecurSketch extends RecurSketchOps[RecurSketch] {

  private case class RecurSketchImpl[A](measure: Measure[A],
                                        rng: IRng,
                                        conf: SketchConf,
                                        structures: Structures,
                                        thresholds: Stream[Double],
                                        count: Count)
      extends RecurSketch[A]

  def bare[A](measure: Measure[A],
              rng: IRng,
              conf: SketchConf,
              structure: Structures,
              thresholds: Stream[Double],
              count: Count): RecurSketch[A] =
    RecurSketchImpl(measure, rng, conf, structure, thresholds, count)

  def modifyRng[A](sketch: RecurSketch[A], f: IRng => IRng): RecurSketch[A] =
    sketch match {
      case periodic: PeriodicSketch[A] => PeriodicSketch.modifyRng(periodic, f)
      case _ => bare(sketch.measure, f(sketch.rng), sketch.conf, sketch.structures, sketch.thresholds, sketch.count)
    }

  def modifyStructures[A](sketch: RecurSketch[A], f: Structures => Structures): RecurSketch[A] =
    sketch match {
      case periodic: PeriodicSketch[A] => PeriodicSketch.modifyStructures(periodic, f)
      case _ => bare(sketch.measure, sketch.rng, sketch.conf, f(sketch.structures), sketch.thresholds, sketch.count)
    }

  def modifyThresholds[A](sketch: RecurSketch[A], f: Stream[Double] => Stream[Double]): RecurSketch[A] =
    sketch match {
      case periodic: PeriodicSketch[A] => PeriodicSketch.modifyThresholds(periodic, f)
      case _ => bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, f(sketch.thresholds), sketch.count)
    }

  def modifyCount[A](sketch: RecurSketch[A], f: Count => Count): RecurSketch[A] = sketch match {
    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyCount(periodic, f)
    case _ => bare(sketch.measure, sketch.rng, sketch.conf, sketch.structures, sketch.thresholds, f(sketch.count))
  }

  // overrides

  override def update[A](sketch: RecurSketch[A], as: List[(A, Count)]): RecurSketch[A] = sketch match {
    case (sketch: PeriodicSketch[A]) => PeriodicSketch.update(sketch, as)
    case _ => super.update(sketch, as)
  }

}

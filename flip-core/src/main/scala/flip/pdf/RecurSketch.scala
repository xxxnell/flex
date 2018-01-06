package flip.pdf

import flip.conf._
import flip.measure.Measure

import scala.language.higherKinds

trait RecurSketch[A] extends Sketch[A] {

  def count: Count

  def thresholds: Stream[Double]

}

trait RecurSketchOps[S[_]<:RecurSketch[_], C<:SketchConf]
  extends SketchPrimPropOps[S, C]
    with RecurSketchLaws[S, C] { self =>

  def modifyThresholds[A](sketch: S[A], f: Stream[Double] => Option[Stream[Double]]): Option[S[A]]

  def modifyCount[A](sketch: S[A], f: Count => Count): S[A]

}

trait RecurSketchLaws[S[_]<:RecurSketch[_], C<:SketchConf] { self: RecurSketchOps[S, C] =>

  def dropThreshold[A](sketch: S[A]): Option[S[A]] = modifyThresholds(sketch, thresholds => Some(thresholds.drop(1)))

  def update[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]] = for {
    nextThreshold <- sketch.thresholds.headOption
    utdSketch1 <- narrowUpdate[A](sketch, as, conf)
    utdSketch2 = modifyCount(utdSketch1, count => count + as.map(_._2).sum)
    utdSketch3 <- if(nextThreshold <= utdSketch2.count) for {
      rearranged <- rearrange(utdSketch2, conf)
      dropped <- dropThreshold(rearranged)
    } yield dropped else Some(utdSketch2)
  } yield utdSketch3

}

object RecurSketch extends RecurSketchOps[RecurSketch, SketchConf] {

  private case class RecurSketchImpl[A](measure: Measure[A],
                                        structures: Structures,
                                        thresholds: Stream[Double],
                                        count: Count)
    extends RecurSketch[A]

  def apply[A](measure: Measure[A],
               structure: Structures,
               thresholds: Stream[Double],
               count: Count): RecurSketch[A] =
    bare(measure, structure, thresholds, count)

  def bare[A](measure: Measure[A],
              structure: Structures,
              thresholds: Stream[Double], count: Count): RecurSketch[A] =
    RecurSketchImpl(measure, structure, thresholds, count)

  def modifyStructure[A](sketch: RecurSketch[A],
                         f: Structures => Option[Structures]): Option[RecurSketch[A]] = sketch match {
    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyStructure(periodic, f)
    case _ => f(sketch.structures).map(structure => bare(sketch.measure, structure, sketch.thresholds, sketch.count))
  }

  def modifyThresholds[A](sketch: RecurSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[RecurSketch[A]] = sketch match {
    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyThresholds(periodic, f)
    case _ => f(sketch.thresholds).map(threshold => bare(sketch.measure, sketch.structures, threshold, sketch.count))
  }

  def modifyCount[A](sketch: RecurSketch[A], f: Count => Count): RecurSketch[A] = sketch match {
    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyCount(periodic, f)
    case _ => bare(sketch.measure, sketch.structures, sketch.thresholds, f(sketch.count))
  }

  override def update[A](sketch: RecurSketch[A],
                         as: List[(A, Count)],
                         conf: SketchConf): Option[RecurSketch[A]] = (sketch, conf) match {
    case (sketch: PeriodicSketch[A], conf: PeriodicSketchConf) => PeriodicSketch.update(sketch, as, conf)
    case _ => super.update(sketch, as, conf)
  }

}

package sketch.scope.pdf

import sketch.scope.conf._
import sketch.scope.measure.Measure

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  *
  *
  */
trait RecurSketch[A] extends Sketch[A] {

  def thresholds: Stream[Double]

}

trait RecurSketchOps[S[_]<:RecurSketch[_], C<:SketchConf]
  extends SketchPrimPropOps[S, C]
    with RecurSketchLaws[S, C] { self =>

  def modifyThresholds[A](sketch: S[A], f: Stream[Double] => Option[Stream[Double]]): Option[S[A]]

}

trait RecurSketchLaws[S[_]<:RecurSketch[_], C<:SketchConf] { self: RecurSketchOps[S, C] =>

  def dropThreshold[A](sketch: S[A]): Option[S[A]] = modifyThresholds(sketch, thresholds => Some(thresholds.drop(1)))

  def update[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]] = for {
    nextThreshold <- sketch.thresholds.headOption
    utdSketch1 <- narrowUpdate[A](sketch, as)
    sum = self.sum(utdSketch1)
    utdSketch2 <- if(nextThreshold <= sum) for {
      rearrangedSketch <- rearrange(utdSketch1, conf)
      droppedSketch <- dropThreshold(rearrangedSketch)
    } yield droppedSketch else Some(utdSketch1)
  } yield utdSketch2

}

object RecurSketch extends RecurSketchOps[RecurSketch, SketchConf] {

  private case class RecurSketchImpl[A](measure: Measure[A],
                                        structures: Structures,
                                        thresholds: Stream[Double])
    extends RecurSketch[A]

  def apply[A](measure: Measure[A],
               structure: Structures,
               thresholds: Stream[Double]): RecurSketch[A] =
    bare(measure, structure, thresholds)

  def bare[A](measure: Measure[A],
              structure: Structures,
              thresholds: Stream[Double]): RecurSketch[A] =
    RecurSketchImpl(measure, structure, thresholds)

  def modifyStructure[A](sketch: RecurSketch[A],
                         f: Structures => Option[Structures]): Option[RecurSketch[A]] = sketch match {
//    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyStructure(periodic, f)
    case _ => f(sketch.structures).map(structure => bare(sketch.measure, structure, sketch.thresholds))
  }

  def modifyThresholds[A](sketch: RecurSketch[A],
                          f: Stream[Double] => Option[Stream[Double]]): Option[RecurSketch[A]] = sketch match {
//    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyThresholds(periodic, f)
    case _ => f(sketch.thresholds).map(threshold => bare(sketch.measure, sketch.structures, threshold))
  }

  def sample[A](sketch: RecurSketch[A]): (RecurSketch[A], A) = sketch match {
//    case periodic: PeriodicSketch[A] => PeriodicSketch.sample(periodic)
    case _ => ???
  }

}

package sketch.scope.pdf

import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  *
  *
  */
trait PeriodicSketch[A] extends Sketch[A] {

  def periods: Stream[Double]

}

trait PeriodicSketchOps[S[_]<:PeriodicSketch[_]] extends SketchPrimPropOps[S] with PeriodicSketchLaws[S] { self =>

  def modifyPeriods[A](sketch: S[A], f: Stream[Double] => Option[Stream[Double]]): Option[S[A]]

}

trait PeriodicSketchLaws[S[_]<:PeriodicSketch[_]] { self: PeriodicSketchOps[S] =>

  def dropPeriod[A](sketch: S[A]): Option[S[A]] = modifyPeriods(sketch, periods => Some(periods.drop(1)))

  def update[A](sketch: S[A], as: List[(A, Count)]): Option[S[A]] = for {
    nextPeriod <- sketch.periods.headOption
    utdSketch1 <- narrowUpdate[A](sketch, as)
    sum = self.sum(utdSketch1)
    utdSketch2 <- if(nextPeriod < sum) for {
      rearrangedSketch <- rearrange(utdSketch1)
      droppedSketch <- dropPeriod(rearrangedSketch)
    } yield droppedSketch else Some(utdSketch1)
  } yield utdSketch2

}

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch] {

  private case class PeriodicSketchImpl[A](measure: Measure[A],
                                           structures: Structures,
                                           periods: Stream[Double])
    extends PeriodicSketch[A]

  def apply[A](measure: Measure[A],
               structure: Structures,
               periods: Stream[Double]): PeriodicSketch[A] =
    bare(measure, structure, periods)

  def bare[A](measure: Measure[A],
              structure: Structures,
              periods: Stream[Double]): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, structure, periods)

  def empty[A](measure: Measure[A], caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    cont(measure, caDepth, caSize, coDepth, coSize)

  def cont[A](measure: Measure[A], caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] =
    f(sketch.structures).map(structure => bare(sketch.measure, structure, sketch.periods))

  def modifyPeriods[A](sketch: PeriodicSketch[A],
                       f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] =
    f(sketch.periods).map(period => bare(sketch.measure, sketch.structures, period))

  def sample[A](sketch: PeriodicSketch[A]): (PeriodicSketch[A], A) = ???

}

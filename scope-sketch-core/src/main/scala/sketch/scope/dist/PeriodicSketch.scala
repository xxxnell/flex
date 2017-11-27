package sketch.scope.dist

import scala.reflect.runtime.universe._
import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter

import scala.collection.immutable.NumericRange
import scala.util.Try
import cats._
import cats.data._
import cats.implicits._
import sketch.scope.hmap.HDim

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

}

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch] {

  private case class PeriodicSketchImpl[A](measure: A => Double,
                                           structures: Structures,
                                           periods: Stream[Double])
    extends PeriodicSketch[A]

  def apply[A](measure: A => Double,
               structure: Structures,
               periods: Stream[Double]): PeriodicSketch[A] =
    bare(measure, structure, periods)

  def bare[A](measure: A => Double,
              structure: Structures,
              periods: Stream[Double]): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, structure, periods)

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    cont(measure, caDepth, caSize, coDepth, coSize)

  def cont[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structures => Option[Structures]): Option[PeriodicSketch[A]] =
    f(sketch.structures).map(structure => bare(sketch.measure, structure, sketch.periods))

  def modifyPeriods[A](sketch: PeriodicSketch[A],
                       f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] =
    f(sketch.periods).map(period => bare(sketch.measure, sketch.structures, period))

}

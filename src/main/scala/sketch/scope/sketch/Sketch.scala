package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.counter.CDim
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.Hmap

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] {

  def measure: A => Double

  def structure: List[(Cmap, HCounter)]

}

trait SketchOps[S[_]<:Sketch[_]] extends SketchLaws[S] {

  def primitiveUpdate[A](sketch: S[A], p: Double): Option[S[A]]

  def primitiveCount[A](sketch: S[A], pFrom: Double, pTo: Double): Double

  /**
    * Total number of elements be memorized.
    * @return
    * */
  def sum[A](sketch: S[A]): Double

  /**
    * Clear all memorization.
    * @return
    * */
  def clear[A](sketch: S[A]): S[A]

  def archiveFirst[_](sketch: S[_]): S[_]

}

trait SketchLaws[S[_]<:Sketch[_]] { self: SketchOps[S] =>

  /**
    * Update the element to be memorized.
    * @return
    * */
  def update[A](sketch: Sketch[A], a: A): Option[Sketch[A]] = ???
//    primitiveUpdate(sketch, sketch.measure(a))

  /**
    * Get the number of elements be memorized.
    * @return
    * */
  def count[A](sketch: S[A], from: A, to: A): Double = ???
//    primitiveCount(sketch, sketch.measure(from), sketch.measure(to))

  /**
    * @return
    * */
  def probability[A](sketch: S[A], from: A, to: A): Double =
    (BigDecimal(count(sketch, from, to)) / BigDecimal(sum(sketch))).toDouble

  /**
    * @return
    * */
  def pdf[A](sketch: S[A], a: A): Double = ???

  /**
    * @return
    * */
  def cdf[A](sketch: S[A], a: A): Double = ???

  def plot(sketch: S[_]): List[(NumericRange[Double], Double)] = ???

}

trait SketchSyntax {

  implicit class SketchSyntaxImpl[A](sketch: Sketch[A]) {
    def update(a: A): Option[Sketch[A]] = Sketch.update(sketch, a)
    def count(from: A, to: A): Double = Sketch.count(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
    def clear: Sketch[A] = Sketch.clear(sketch)
    def probability(from: A, to: A): Double = Sketch.probability(sketch, from, to)
  }

}

object Sketch extends SketchOps[Sketch] {

//  private case class SketchImpl[A](measure: A => Double, structure: List[(Cmap, HCounter)]) extends Sketch[A]

  /**
    * @return
    */
  def empty[A](measure: A => Double, cmapSize: Int, depth: Int, cdimSize: Int): Sketch[A] = ???
//    SketchImpl(measure, (Cmap.uniform(cmapSize), HCounter.empty(depth, cdimSize)) :: Nil)

  def primitiveUpdate[A](sketch: Sketch[A], p: Double): Option[Sketch[A]] = ???

  def primitiveCount[A](sketch: Sketch[A], pFrom: Double, pTo: Double): Double = ???

  def sum[A](sketch: Sketch[A]): Double = ???

  def clear[A](sketch: Sketch[A]): Sketch[A] = ???

  def archiveFirst[_](sketch: Sketch[_]): Sketch[_] = ???

}
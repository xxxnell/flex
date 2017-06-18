package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hmap.Hmap

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] {

  def measure: A => Double

//  def structure: List[()]

}

trait SketchOps extends SketchLaws {

  /**
    * Update the element to be memorized.
    * @return
    * */
  def update[A](sketch: Sketch[A], a: A): Sketch[A]

  /**
    * Get the number of elements be memorized.
    * @return
    * */
  def count[A](sketch: Sketch[A], from: A, to: A): Int

  /**
    * Total number of elements be memorized.
    * @return
    * */
  def size[A](sketch: Sketch[A]): Int

  /**
    * Clear all memorization.
    * @return
    * */
  def clear[A](sketch: Sketch[A]): Sketch[A]

}

trait SketchLaws { self: SketchOps =>

  /**
    * @return
    * */
  def probability[A](sketch: Sketch[A], from: A, to: A): Double =
    (BigDecimal(count(sketch, from, to)) / BigDecimal(size(sketch))).toDouble

  /**
    * @return
    * */
  def pdf[A](sketch: Sketch[A], a: A): Double = ???

  /**
    * @return
    * */
  def cdf[A](sketch: Sketch[A], a: A): Double = ???

}

trait SketchSyntax {

  implicit class SketchSyntaxImpl[A](sketch: Sketch[A]) {
    def update(a: A): Sketch[A] = Sketch.update(sketch, a)
    def count(from: A, to: A): Int = Sketch.count(sketch, from, to)
    def size: Int = Sketch.size(sketch)
    def clear: Sketch[A] = Sketch.clear(sketch)
    def probability(from: A, to: A): Double = Sketch.probability(sketch, from, to)
  }

}

object Sketch extends SketchOps {

  private case class SketchImpl[A](measure: A => Double) extends Sketch[A]

  // Construction

  def apply[A](measure: A => Double): Sketch[A] = ???

  /**
    * @return
    */
  def empty[A](cmap: Cmap, hmap: Hmap): Sketch[A] = ???

  // Ops

  def update[A](sketch: Sketch[A], a: A): Sketch[A] = ???

  def count[A](sketch: Sketch[A], from: A, to: A): Int = ???

  def size[A](sketch: Sketch[A]): Int = ???

  def clear[A](sketch: Sketch[A]): Sketch[A] = ???

}
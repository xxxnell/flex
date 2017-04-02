package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hmap.Hmap

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A]

trait SketchOps extends SketchLaws {

  /**
    * Update the element to be memorized.
    * @return
    * */
  def update[A](sketch: Sketch[A])(a: A): Sketch[A]

  /**
    * Get the number of elements be memorized.
    * @return
    * */
  def count[A](sketch: Sketch[A])(from: A, to: A): BigInt

  /**
    * Total number of elements be memorized.
    * @return
    * */
  def size[A](sketch: Sketch[A]): BigInt

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
  def probability[A](sketch: Sketch[A])(from: A, to: A): BigDecimal =
    BigDecimal(count(sketch)(from, to)) / BigDecimal(size(sketch))

  /**
    * @return
    * */
  def pdf[A](sketch: Sketch[A])(a: A): BigDecimal = ???

  /**
    * @return
    * */
  def cdf[A](sketch: Sketch[A])(a: A): BigDecimal = ???

}

trait SketchSyntax {

  implicit class SketchSyntaxImpl[A](sketch: Sketch[A]) {
    def update(a: A): Sketch[A] = Sketch.update(sketch)(a)
    def count(from: A, to: A): BigInt = Sketch.count(sketch)(from, to)
    def size: BigInt = Sketch.size(sketch)
    def clear: Sketch[A] = Sketch.clear(sketch)
    def probability(from: A, to: A): BigDecimal = Sketch.probability(sketch)(from, to)
  }

}

object Sketch extends SketchOps {

  // Construction

  def apply[A]: Sketch[A] = ???

  /**
    * @return
    */
  def empty[A](cmap: Cmap[A], hmap: Hmap): Sketch[A] = ???

  // Ops

  def update[A](sketch: Sketch[A])(a: A): Sketch[A] = ???

  def count[A](sketch: Sketch[A])(from: A, to: A): BigInt = ???

  def size[A](sketch: Sketch[A]): BigInt = ???

  def clear[A](sketch: Sketch[A]): Sketch[A] = ???

}
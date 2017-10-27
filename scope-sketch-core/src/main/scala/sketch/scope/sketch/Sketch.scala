package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import cats.implicits._
import scala.reflect.runtime.universe._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] {

  def measure: A => Prim

  def structure: List[(Cmap, HCounter)]

}

trait SketchOps[S[_]<:Sketch[_]] extends SketchLaws[S] {

  /**
    * Update a primitive value <code>p</code> without rearrange process.
    * */
  def simpleUpdate[A](sketch: S[A], p: Prim): Option[Sketch[A]] = for {
    utdStr <- sketch.structure.traverse { case (cmap, hcounter) =>
      hcounter.update(cmap.apply(p), 1).map(hcounter => (cmap, hcounter))
    }
  } yield Sketch(sketch.measure.asInstanceOf[A => Prim], utdStr)

  /**
    * Update a primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primUpdate[A](sketch: S[A], p: Prim): Option[S[A]]

  def primCount(sketch: S[_], pFrom: Prim, pTo: Prim): Option[Double]

  /**
    * Total number of elements be memorized.
    * */
  def sum(sketch: S[_]): Double

  /**
    * Clear all memorization.
    * */
//  def clear(sketch: S): S

  def rearrange[A](sketch: S[A]): Option[S[A]]

}

trait SketchLaws[S[_]<:Sketch[_]] { self: SketchOps[S] =>

  /**
    * Update the element to be memorized.
    * */
  def update[A](sketch: S[A], a: A): Option[S[A]] = {
    primUpdate(sketch, sketch.measure.asInstanceOf[A => Double](a))
  }

  /**
    * Get the number of elements be memorized.
    * */
  def count[A](sketch: S[A], from: A, to: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[A => Double]
    primCount(sketch, measure(from), measure(to))
  }

  /***/
  def probability[A](sketch: S[A], from: A, to: A): Option[Double] = for {
    count <- count(sketch, from, to)
    sum = self.sum(sketch)
  } yield (BigDecimal(count) / BigDecimal(sum)).toDouble

//  def pdf(sketch: S, a: Double): Option[Double] = ???

//  def cdf(sketch: S, a: Double): Option[Double] = ???

  def plot(sketch: S[_]): Option[List[(Range, Double)]] = {
    for {
      cmapHcounter <- sketch.structure.lastOption
      (cmap, _) = cmapHcounter
      ranges = cmap.bin
      counts <- ranges.traverse(range => primCount(sketch, range.start, range.end))
    } yield ranges.zip(counts)
  }

  def densityPlot(sketch: S[_]): Option[List[(Range, Double)]] = {
    val sum = self.sum(sketch)
    plot(sketch).map(plot =>
      plot.map { case (range, count) => (range, count / (sum * (range.end - range.start))) }
    )
  }

}

trait SketchSyntax {

  implicit class SketchSyntaxImpl[A](sketch: Sketch[A]) {
    def update(a: A): Option[Sketch[A]] = Sketch.update(sketch, a)
    def count(from: A, to: A): Option[Double] = Sketch.count(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
//    def clear: Sketch = Sketch.clear(sketch)
    def probability(from: A, to: A): Option[Double] = Sketch.probability(sketch, from, to)
    def rearrange: Option[Sketch[A]] = Sketch.rearrange(sketch)
  }

}

object Sketch extends SketchOps[Sketch] {

  private case class SketchImpl[A](measure: A => Prim,
                                   structure: List[(Cmap, HCounter)])
    extends Sketch[A]

  def apply[A](measure: A => Prim, structure: List[(Cmap, HCounter)]): Sketch[A] = bare(measure, structure)

  def bare[A](measure: A => Prim, structure: List[(Cmap, HCounter)]): Sketch[A] = SketchImpl(measure, structure)

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): Sketch[A] =
    PeriodicSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def primUpdate[A](sketch: Sketch[A], p: Double): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.primUpdate(sketch, p)
  }

  def primCount(sketch: Sketch[_], pFrom: Double, pTo: Double): Option[Double] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.primCount(sketch, pFrom, pTo)
  }

  def sum(sketch: Sketch[_]): Double = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.sum(sketch)
  }

//  def clear(sketch: Sketch): Sketch = sketch match {
//    case sketch: PeriodicSketch => PeriodicSketch.clear(sketch)
//  }

  def rearrange[A](sketch: Sketch[A]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.rearrange(sketch)
  }

}
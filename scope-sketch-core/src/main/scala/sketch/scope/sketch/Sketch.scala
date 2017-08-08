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

  def measure: A => Double

  def structure: List[(Cmap, HCounter)]

}

trait SketchOps[S[_]<:Sketch[_]] extends SketchLaws[S] {

  def primitiveUpdate[A](sketch: S[A], p: Double): Option[S[A]]

  def primitiveCount(sketch: S[_], pFrom: Double, pTo: Double): Option[Double]

  /**
    * Total number of elements be memorized.
    * @return
    * */
  def sum(sketch: S[_]): Double

  /**
    * Clear all memorization.
    * @return
    * */
//  def clear(sketch: S): S

  def rearrange[A](sketch: S[A]): Option[S[A]]

}

trait SketchLaws[S[_]<:Sketch[_]] { self: SketchOps[S] =>

  /**
    * Update the element to be memorized.
    * @return
    * */
  def update[A](sketch: S[A], a: A): Option[S[A]] = {
    primitiveUpdate(sketch, sketch.measure.asInstanceOf[A => Double](a))
  }

  /**
    * Get the number of elements be memorized.
    * @return
    * */
  def count[A](sketch: S[A], from: A, to: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[A => Double]
    primitiveCount(sketch, measure(from), measure(to))
  }

  /**
    * @return
    * */
  def probability[A](sketch: S[A], from: A, to: A): Option[Double] = for {
    count <- count(sketch, from, to)
    sum = self.sum(sketch)
  } yield (BigDecimal(count) / BigDecimal(sum)).toDouble

  /**
    * @return
    * */
//  def pdf(sketch: S, a: Double): Option[Double] = ???

  /**
    * @return
    * */
//  def cdf(sketch: S, a: Double): Option[Double] = ???

  def plot(sketch: S[_]): Option[List[(Range, Double)]] = {
    for {
      cmapHcounter <- sketch.structure.lastOption
      (cmap, _) = cmapHcounter
      ranges = cmap.bin
      counts <- ranges.traverse(range => primitiveCount(sketch, range.start, range.end))
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

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): Sketch[A] =
    PeriodicSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def primitiveUpdate[A](sketch: Sketch[A], p: Double): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.primitiveUpdate(sketch, p)
  }

  def primitiveCount(sketch: Sketch[_], pFrom: Double, pTo: Double): Option[Double] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.primitiveCount(sketch, pFrom, pTo)
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